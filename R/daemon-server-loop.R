## TODO: remove the closed connection
runDaemon <- function(daemonName, 
                      interruptable = TRUE, 
                      detach = FALSE, 
                      logFile = NULL,
                      threshold= c("INFO", "WARN", "ERROR", "DEBUG", "TRACE"),
                      cleanup = TRUE){
    threshold <- match.arg(threshold)
    enableLog(logFile = logFile, threshold = threshold)
    
    ## Start the server connection
    startServerConnection(daemonName = daemonName)
    if(cleanup)
        on.exit(quitDaemon(), add = TRUE)
    
    if(detach){
        detachConsole()
    }
    
    ## The daemon loop
    resetTimer("server", "loop")
    .suspendInterruptsIfRequired({
        continue <- TRUE
        while(continue){
            handleExceptions({
                continue <- daemonLoop()
            },
            warningPrefix = "Unclassified warning",
            errorPrefix = "Unclassified error")
        }
    }, 
    interruptable = interruptable)
}

daemonLoop <- function(){
    ## Limit the frequency of the loop
    waitUntilTimeout(
        class = "main", 
        name = "daemon", 
        timeout = serverData$mainLoopInterval)
    
    ## Accept new connections
    handleExceptions({
        acceptConnections()
    },
    warningPrefix = "Uncached warning in acceptConnections",
    errorPrefix = "Uncached error in acceptConnections")
    
    
    ## run the existing tasks
    handleExceptions({
        runTasks()
    },
    warningPrefix = "Uncached warning in runTasks",
    errorPrefix = "Uncached error in runTasks")
    
    ## process incoming requests
    
    handleExceptions({
        processRequest()
    },
    warningPrefix = "Uncached warning in processRequest",
    errorPrefix = "Uncached error in processRequest")
    
    ## Truncate the log if needed
    handleExceptions({
        truncateLog()
    },
    warningPrefix = "Uncached warning in truncateLog",
    errorPrefix = "Uncached error in truncateLog")
    
    handleExceptions({
        cleanupInvalidConnection()
    },
    warningPrefix = "Uncached warning in cleanupInvalidConnection",
    errorPrefix = "Uncached error in cleanupInvalidConnection")
    
    ## Check if the daemon is timeout
    handleExceptions({
        timeout <- isLoopTimeout()
    },
    warningPrefix = "Uncached warning in isLoopTimeout",
    errorPrefix = "Uncached error in isLoopTimeout")
    
    if(timeout){
        flog.info("No task is running, quit daemon")
        return(FALSE)
    }
    flog.trace("Client Number: %d", length(serverData$connections))
    TRUE
}


enableSink <- function(){
    logFile <- serverData$logFile
    if(!is.null(logFile)&&nzchar(logFile)){
        if(file.exists(logFile))
            unlink(logFile)
        con <- file(logFile, open = "wt", blocking = FALSE)
        sink(con, append = FALSE)
        sink(con, append = FALSE, type = "message")
    }
}

disableSink <- function(){
    sink(type = "message")
    sink()
}

enableLog <- function(logFile, threshold){
    futile.logger::flog.threshold(get(threshold))
    
    ## sink the output
    if(!is.null(logFile)&&nzchar(logFile)){
        serverData$logFile <- logFile
        enableSink()
    }
    flog.info("Daemon PID: %d", Sys.getpid())
}

truncateLog <- function(){
    timeout <- isTimeout("truncate", "log", serverData$logTruncationInterval)
    if(!timeout)
        return()
    logFile <- serverData$logFile
    if(is.null(logFile)||!nzchar(logFile))
        return()
    if(!file.exists(logFile))
        return()
    
    logs <- readLines(con = logFile)
    n <- serverData$logMaxLineNum
    ## We do not want to do this every time
    ## so just make it tolerate more lines
    if(length(logs) > n * 2){
        disableSink()
        logs <- paste0(paste0(tail(logs, n = n), collapse = "\n"), "\n")
        enableSink()
        cat(logs)
    }
}

startServerConnection <- function(daemonName){
    ## Check if the daemon already exists
    if(daemonExists(daemonName)){
        if(getDaemonPid(daemonName) != Sys.getpid()){
            flog.error("The daemon '%s' already exists!", daemonName)
            stop(call. = FALSE)
        }
    }
    ## Try to start the daemon server
    serverData$port <- findPort()
    ## Run and check if this daemon gets the permission to continue
    serverData$serverConn <- serverSocket(serverData$port)
    stopifnot(!is.null(serverData$serverConn))
    
    setDaemonPort(daemonName, serverData$port)
    setDaemonPid(daemonName, Sys.getpid())
    Sys.sleep(1)
    if(getDaemonPort(daemonName) != serverData$port){
        close(serverData$serverConn)
        serverData$serverConn <- NULL
        return()
    }
    serverData$daemonName <- daemonName
    serverData$isServer <- TRUE
    setDaemonConnection(daemonName, TRUE)
}

quitDaemon <- function(){
    close(serverData$serverConn)
    serverData$serverConn <- NULL
    daemonName <- serverData$daemonName
    setDaemonPort(daemonName, NA_integer_)
    setDaemonPid(daemonName, NA_integer_)
    serverData$isServer <- FALSE
}

acceptConnections <- function(){
    incomingConnection <- getDaemonConnection(lastRegisteredDaemon())
    if(!incomingConnection){
        return()
    }
    setDaemonConnection(lastRegisteredDaemon(), FALSE)
    flog.debug("Receive the connection signal, processing")
    
    while(TRUE){
        con <- tryCatch(
            suppressWarnings(
                socketAccept(serverData$serverConn, 
                             open = "r+", 
                             timeout = 1)
            ),
            error = function(e) NULL)
        if(is.null(con)) break
        
        flog.debug("A connection has been created, waiting for the handshake")
        request <- waitData(con, timeout = 10)
        pid <- as.character(request$pid)
        if(isOneTimeConnection(request)){
            flog.debug("Receive an one-time request from pid %s", pid)
            request <- request$data
            processIndividualRequest(request = request, pid = pid, con = con)
            if(isOpen(con))
                close(con)
            next
        }
        if(!isHandshake(request)){
            flog.warn("Handshake failed! Closing the connection")
            close(con)
            next
        }
        
        oldCon <- serverData$connections[[pid]]
        if(!is.null(oldCon)){
            close(oldCon)
        }
        serverData$connections[[pid]] <- con
        flog.info("A connection from the pid %s is established", pid)
    }
}

runTasks <- function(){
    taskIds <- names(serverData$tasks)
    for(taskId in taskIds){
        ## Check whether the task need to be executed
        taskInterval <- serverData$taskIntervals[[taskId]]
        if(is.null(taskInterval))
            taskInterval <- 1L
        timeout <- isTimeout(class = "runTask", 
                             name = taskId, 
                             timeout = taskInterval)
        if(!timeout)
            next
        
        ## Run the task
        serverData$currentTaskId <- taskId
        errorPrefix <- sprintf(
            "Error in evaluating the task with the id %s", 
            taskId)
        warningPrefix <- sprintf(
            "Warning in evaluating the task with the id %s", 
            taskId)
        
        handleExceptions(
            eval(expr = serverData$tasks[[taskId]], 
                 envir  = serverData$taskData[[taskId]]),
            errorPrefix = errorPrefix,
            warningPrefix = warningPrefix)
    }
}

processRequest <- function(){
    pids <- names(serverData$connections)
    for(pid in pids){
        con <- serverData$connections[[pid]]
        requests <- readData(con)
        for(request in requests){
            errorPrefix <- sprintf(
                "Error in processing the request from the pid %s", 
                pid)
            warningPrefix <- sprintf(
                "Warning in processing the request from the pid %s", 
                pid)
            handleExceptions(
                processIndividualRequest(request),
                errorPrefix = errorPrefix,
                warningPrefix = warningPrefix)
        }
    }
}

processIndividualRequest <- function(request, pid = NULL, con = NULL){
    taskId <- request$taskId
    data <- request$data
    if(is.null(pid))
        pid <- request$pid
    pid <- as.character(pid)
    if(is.null(con) && length(pid)!=0)
        con <- serverData$connections[[pid]]
    
    if(isSetTaskRequest(request)){
        exports <- request$exports
        interval <- request$interval
        server.setTask(expr = data, 
                       taskId = taskId,
                       exports = exports,
                       interval = interval)
        return()
    }
    
    ## TODO:: switch
    if(isEval(request)){
        if(is.null(con)){
            stop("The connection to the pid `",pid,"` does not exist")
        }
        result <- server.eval(expr = data, taskId = taskId)
        server.response(con, result)
        return()
    }
    
    
    if(isGetTaskRequest(request)){
        if(is.null(con)){
            stop("The connection to the pid `",pid,"` does not exist")
        }
        
        result <- server.getTask(taskId = taskId)
        server.response(con, result)
        return()
    }
    
    if(isExportRequest(request)){
        server.export(taskId = taskId, objects = data)
        return()
    }
    
    if(isDeleteTaskRequest(request)){
        server.deleteTask(taskId)
        return(TRUE)
    }
    
    if(isCopyTask(request)){
        sourceId <- data
        server.copyTask(sourceId = sourceId, targetId = taskId)
        return()
    }
    
    if(isClose(request)){
        if(!is.null(con)){
            close(con)
        }
        serverData$connections[[pid]] <- NULL
        for(taskId in as.character(data)){
            serverData$tasks[[taskId]] <- NULL
            serverData$taskData[[taskId]] <- NULL
            flog.info("Delete the task %s", taskId)
        }
        flog.info("The connection to pid %s has been closed", pid)
        return()
    }
    
    stop("Unknown task type: ", request$type)
}


isLoopTimeout <- function(){
    timeout <- isTimeout("server", "loop", serverData$noTaskTimeout)
    if(length(serverData$tasks) == 0){
        timeout
    }else{
        resetTimer("server", "loop")
        FALSE
    }
}


cleanupInvalidConnection <- function(){
    timeout <- isTimeout("server", "closeConnection", serverData$ConnectionCleanupInterval)
    if(!timeout)
        return()
    pids <- names(serverData$connections)
    exists <- isProcessAlive(pids)
    idx <- which(!exists)
    for(i in idx){
        pid <- pids[i]
        flog.info("The connection from the pid %s is closed", pid)
        close(serverData$connections[[pid]])
        serverData$connections[[pid]] <- NULL
    }
}
