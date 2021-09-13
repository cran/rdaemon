#' The daemon functions
#' 
#' The functions for managing the daemon in the background.
#' The daemon is uniquely identified by a daemon name.
#' If the user do not specify the daemon name, each R process
#' will create its own daemon using a unique daemon name by default.
#' However, it is also possible to let multiple R sessions connect 
#' with the same daemon if they use a common daemon name.
#' 
#' @name rdaemon-methods
#' @rdname rdaemon-methods
#' @seealso \code{\link{isProcessAlive}}, \code{\link{interruptProcess}}
#' @examples 
#' ## Check the default daemon name and task ID
#' ## These will be used in the subsequent calls
#' ## in this example
#' lastRegisteredDaemon()
#' lastRegisteredTaskId()
#' 
#' ## Register a daemon using the default daemon name
#' registerDaemon()
#' daemonLogs()
#' 
#' ## Check if the daemon is running
#' daemonExists()
#' 
#' ## Set a task run by the daemon
#' daemonSetTask(message("Hello from daemon"))
#' 
#' ## See the daemon log for the output
#' Sys.sleep(1)
#' daemonLogs()
#' 
#' ## Get the task expression from the daemon
#' daemonGetTask()
#' 
#' ## Export an object to the task in the daemon
#' daemonExport(i = 1)
#' 
#' ## Evaluate an expression in the daemon
#' daemonEval(i)
#' 
#' ## Close the connection to the daemon and remove the task
#' ## set by this process
#' ## The daemon will silently quit after a few minutes 
#' ## if no other tasks exist.
#' deregisterDaemon()
#' daemonExists()
#' 
#' ## explicitly kill the daemon if you are impatient
#' killDaemon()
#' daemonExists()
NULL


#' @details 
#' `lastRegisteredDaemon`: Get the name of the last registered daemon,
#' or a unique daemon name for the current process if no daemon 
#' is registered.
#' 
#' @returns 
#' `lastRegisteredDaemon`: character(1)
#' @rdname rdaemon-methods
#' @export
lastRegisteredDaemon <- function(){
    if(serverData$isServer){
        serverData$daemonName
    }else{
        clientData$lastRegisteredDaemon
    }
}

#' @details 
#' `lastRegisteredTaskId`: Get the last task ID set via `daemonSetTask`,
#' or a unique task ID for the current process if no task 
#' is set.
#' 
#' @returns 
#' `lastRegisteredTaskId`: character(1)
#' @rdname rdaemon-methods
#' @export
lastRegisteredTaskId <- function(){
    if(serverData$isServer){
        serverData$currentTaskId
    }else{
        clientData$lastRegisteredTaskId
    }
}


#' @param daemonName character(1), the name of the daemon. If not provided,
#' the last registered daemon or the default daemon name will be used. Note
#' that there are length limit on the number of characters in the name and the limit
#' varys with the OS. For portability, we recommend using a shorter name.
#' @param logFile character(1), the path to the log file. If the file
#' does not exist, it will be created. Otherwise, the file will be overwritten.
#' You can also use `daemonLogs()` to obtain the log.
#' @param  threshold, character(1), the verbose level of the daemon log.
#' The value must be one of "INFO", "WARN", "ERROR", "DEBUG", or "TRACE".
#' 
#' @details 
#' `registerDaemon`: create a new daemon or connect with an existing daemon
#' identified by the daemon name. This function cannot be called in
#' the daemon.
#' 
#' @returns 
#' `registerDaemon`: invisible()
#' @rdname rdaemon-methods
#' @export
registerDaemon <- function(daemonName = lastRegisteredDaemon(),
                           logFile = NULL,
                           threshold = c("INFO", "WARN", "ERROR", "DEBUG", "TRACE")){
    threshold <- match.arg(threshold)
    daemonName <- truncateLongName(daemonName)
    checkDaemonArgs(daemonName = daemonName)
    stopifnot(!serverData$isServer)
    
    client.registerDaemon(
        daemonName = daemonName, 
        logFile = logFile,
        threshold = threshold)
    invisible()
}

#' @param deleteTask logical(1), whether to delete the task set by
#' the current R session.
#' 
#' @details 
#' `deregisterDaemon`: 
#' disconnect a daemon and (optionally) remove all tasks in the daemon
#' which are set by the current process. 
#' The daemon might keep running in the background
#' until no task is scheduled or is killed by the other process.
#' This function cannot be called in the daemon.
#' 
#' @returns 
#' `deregisterDaemon`: invisible()
#' @rdname rdaemon-methods
#' @export
deregisterDaemon <- function(
    daemonName = lastRegisteredDaemon(), 
    deleteTask = TRUE){
    checkDaemonArgs(daemonName = daemonName)
    stopifnot(!serverData$isServer)
    client.deregisterDaemon(daemonName = daemonName, 
                            deleteTask = deleteTask)
    invisible()
}

#' @details 
#' `killDaemon`: Kill a daemon. 
#' 
#' @returns 
#' `killDaemon`: invisible()
#' @rdname rdaemon-methods
#' @export
killDaemon <- function(daemonName = lastRegisteredDaemon()){
    checkDaemonArgs(daemonName = daemonName)
    if(serverData$isServer && identical(serverData$daemonName, daemonName)){
        quit(save = "no")
    }else{
        client.killDaemon(daemonName = daemonName)
    }
    invisible()
}

#' @details 
#' `daemonExists`: Whether the daemon exists.
#' 
#' @returns 
#' `daemonExists`: logical(1)
#' @rdname rdaemon-methods
#' @export
daemonExists <- function(daemonName = lastRegisteredDaemon()){
    checkDaemonArgs(daemonName = daemonName)
    if(serverData$isServer && identical(serverData$daemonName, daemonName))
        return(TRUE)
    
    daemonPid <- getDaemonPid(daemonName)
    daemonPort <- getDaemonPort(daemonName)
    if(!is.na(daemonPid)&&
       !is.na(daemonPort)&&
       isProcessAlive(daemonPid)&&
       portOccupied(daemonPort)){
        TRUE
    }else{
        FALSE
    }
}

#' @param expr expression(), an R expression that can be run by the daemon.
#' @param taskId character(1), the ID of the task. If not specified, the 
#' task ID from the previous `daemonSetTask` call will be used. If `daemonSetTask`
#' is never called, the default task ID will be provided.
#' @param expr.char character(1), An R expression in the character format.
#' @param interval numeric(1), the time to wait in seconds between 
#' two executions of the same task.
#' @param exports list, the variables that will be exported to the task
#' environment.
#' 
#' @details 
#' `daemonSetTask`: set the task expression that 
#' will be evaluated by the daemon. Calling the function without `expr` and
#' `expr.char` arguments will remove the task specified by `taskId`.
#' 
#' @returns 
#' `daemonSetTask`: logical(1)
#' @rdname rdaemon-methods
#' @export
daemonSetTask <- function(expr = NULL, 
                          daemonName = lastRegisteredDaemon(),
                          taskId = lastRegisteredTaskId(),
                          expr.char = NULL,
                          interval = 1L,
                          exports = list()){
    checkDaemonArgs(daemonName = daemonName, taskId = taskId)
    if(length(exports)>0){
        stopifnot(length(names(exports))>0)
        stopifnot(all(nzchar(names(exports))))
    }
    
    if(missing(expr.char)){
        expr <- substitute(expr)
    }else{
        expr.char <- gsub("\r", "", expr.char)
        expr <- parse(text = expr.char)
    }
    
    if(serverData$isServer){
        stopifnot(identical(serverData$daemonName, daemonName))
        server.setTask(expr = expr, 
                       taskId = taskId, 
                       exports = exports,
                       interval = interval)
    }else{
        client.setTask(daemonName = daemonName, 
                       taskId = taskId,
                       expr = expr,
                       exports = exports,
                       interval = interval)
    }
    invisible()
}

#' @param script character(1), the path to the script.
#' 
#' @details 
#' `daemonSetTaskScript`: set the task expression that will be 
#' evaluated by the daemon using a script file. 
#' Note that the script file will only be read once and further changes
#' in the script file after calling this function would not affect the 
#' task expression.
#' This function will ultimately call `daemonSetTask` to set the task, 
#' so it also affects the value returned by `lastRegisteredDaemon`.
#' 
#' 
#' @returns 
#' `daemonSetTaskScript`: invisible()
#' @rdname rdaemon-methods
#' @export
daemonSetTaskScript <- function(script, 
                                daemonName = lastRegisteredDaemon(), 
                                taskId = lastRegisteredTaskId(),
                                interval = 1,
                                exports = list()){
    checkDaemonArgs(daemonName = daemonName, taskId = taskId)
    stopifnot(!serverData$isServer)
    script <- readChar(script, file.info(script)$size)
    daemonSetTask(daemonName = daemonName, 
                  taskId = taskId, 
                  expr.char = script,
                  interval = interval,
                  exports = exports)
}


#' @details 
#' `daemonSetTaskInterval`: set the time to wait in seconds
#' between two executions of the same task. Because the daemon
#' schedule all tasks sequentially, it is possible to have a 
#' long-running task jamming the execution of all the other tasks.
#' Therefore, the interval should be considered as the minimal 
#' waiting time.
#' 
#' @returns 
#' `daemonSetTaskInterval`: invisible()
#' @rdname rdaemon-methods
#' @export
daemonSetTaskInterval <- function(interval, 
                                  daemonName = lastRegisteredDaemon(), 
                                  taskId = lastRegisteredTaskId()){
    checkDaemonArgs(daemonName = daemonName, taskId = taskId)
    expr <- paste0("rdaemon:::server.setTaskInterval('",taskId,"',", interval,")")
    daemonEval(expr.char = expr, daemonName = daemonName, taskId = taskId)
    invisible()
}

#' @details 
#' `daemonGetTaskInterval`: get the time to wait in seconds
#' between two executions of the same task.
#' 
#' @returns 
#' `daemonGetTaskInterval`: numeric(1) or NULL
#' @rdname rdaemon-methods 
#' @export
daemonGetTaskInterval <- function(interval, 
                                  daemonName = lastRegisteredDaemon(), 
                                  taskId = lastRegisteredTaskId()){
    checkDaemonArgs(daemonName = daemonName, taskId = taskId)
    expr <- paste0("rdaemon:::serverData$taskIntervals[['",taskId,"']]")
    daemonEval(expr.char = expr, daemonName = daemonName, taskId = taskId)
}

#' @details 
#' `daemonEval`: Evaluate an expression in the daemon.
#' 
#' @returns 
#' `daemonEval`: The result of the expression
#' @rdname rdaemon-methods 
#' @export
daemonEval <- function(expr,
                       daemonName = lastRegisteredDaemon(), 
                       taskId = lastRegisteredTaskId(),
                       expr.char = NULL){
    checkDaemonArgs(daemonName = daemonName, taskId = taskId)
    stopifnot(xor(missing(expr), missing(expr.char)))
    if(missing(expr.char)){
        expr <- substitute(expr)
    }else{
        expr.char <- gsub("\r", "", expr.char)
        expr <- parse(text = expr.char)
    }
    
    response <-if(serverData$isServer){
        stopifnot(identical(serverData$daemonName, daemonName))
        server.eval(expr = expr, taskId = taskId)
    }else{
        client.eval(daemonName = daemonName, 
                    taskId = taskId,
                    expr = expr)
    }
    if(inherits(response, "simpleError")){
        stop(response)
    }else{
        response
    }
}

#' @details 
#' `daemonGetTask`: get the task expression from the daemon
#' 
#' @returns 
#' `daemonGetTask`: An expression
#' @rdname rdaemon-methods 
#' @export
daemonGetTask <- function(daemonName = lastRegisteredDaemon(), 
                          taskId = lastRegisteredTaskId()){
    checkDaemonArgs(daemonName = daemonName, taskId = taskId)
    if(serverData$isServer){
        stopifnot(identical(serverData$daemonName, daemonName))
        server.getTask(taskId = taskId)
    }else{
        client.getTask(daemonName = daemonName, taskId = taskId)
    }
}

#' @param ... Named arguments that will be exported.
#' @details 
#' `daemonExport`: export variables to a task environment in the daemon. 
#' Each variable in `...` should be named.
#' 
#' @returns 
#' `daemonExport`: invisible()
#' @rdname rdaemon-methods 
#' @export
daemonExport <- function(..., 
                         daemonName = lastRegisteredDaemon(), 
                         taskId = lastRegisteredTaskId()){
    checkDaemonArgs(daemonName = daemonName, taskId = taskId)
    objects <- list(...)
    stopifnot(length(names(objects))>0)
    stopifnot(all(nzchar(names(objects))))
    
    if(serverData$isServer){
        stopifnot(identical(serverData$daemonName, daemonName))
        server.export(objects = objects, taskId = taskId)
    }else{
        client.export(daemonName = daemonName, 
                      taskId = taskId, 
                      objects = objects)
    }
    invisible()
}

#' @param prettyPrint logical(1), whether to print out the log in a pretty format
#' 
#' @details 
#' `daemonLogs`: Get the log of the daemon.
#' 
#' @returns 
#' `daemonLogs`: character(n)
#' @rdname rdaemon-methods 
#' @export
daemonLogs <- function(daemonName = lastRegisteredDaemon(), prettyPrint = TRUE){
    checkDaemonArgs(daemonName = daemonName)
    logPath <- daemonEval(daemonName = daemonName, 
                          expr.char = "rdaemon:::serverData$logFile")
    if(is.null(logPath)||!nzchar(logPath))
        return(character())
    resetTimer("client", "logFile")
    while(!file.exists(logPath) && !isTimeout("client", "logFile", 5)){
        Sys.sleep(0.1)
    }
    stopifnot(file.exists(logPath))
    x <- readLines(logPath)
    if(prettyPrint){
       cat(paste0(x,collapse = "\n"))
       cat("\n")
       invisible(x)
    }else{
        x
    }
}

#' @param sourceId character(1), the source task ID.
#' @param targetId character(1), the target task ID.
#' @details 
#' `daemonCopyTask`: Copy the task `sourceId` to `targetId`. 
#' The task expression and data will be copied. If the task `targetId` is
#' not empty, the existing content will be overwritten.
#' 
#' @returns 
#' `daemonCopyTask`: invisible()
#' @rdname rdaemon-methods 
#' @export
daemonCopyTask <- function(sourceId, 
                           targetId = lastRegisteredTaskId(), 
                           daemonName = lastRegisteredDaemon()){
    checkDaemonArgs(daemonName = daemonName)
    stopifnot(isScalerChar(sourceId))
    stopifnot(isScalerChar(targetId))
    
    if(serverData$isServer){
        stopifnot(identical(serverData$daemonName, daemonName))
        server.copyTask(sourceId = sourceId, targetId = targetId)
    }else{
        client.copyTask(daemonName = daemonName, 
                        sourceId = sourceId, 
                        targetId = targetId)
    }
    invisible()
}

