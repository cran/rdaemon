serverData <- new.env(parent = emptyenv())
serverData$daemonName <- NULL
serverData$serverConn <- NULL
serverData$port <- NULL
## the element name is the client pid
serverData$connections <- list()
## element's name is the task id
## connections: The server to client connection
## tasks: The client task
## taskData: The data used by the client task
serverData$tasks <- list()
serverData$taskData <- list()
serverData$taskIntervals <- list()
## timeout: Time to wait before quit if no task is running
## isServer: Whether this is a daemon server
## taskPid: The pid corresponds to the currently processed task
serverData$noTaskTimeout <- 2*60
serverData$logTruncationInterval <- 60
serverData$ConnectionCleanupInterval <- 10
serverData$isServer <- FALSE
serverData$currentTaskId <- NULL
serverData$logFile <- NULL
serverData$logMaxLineNum <- 10000
serverData$mainLoopInterval <- 0.1


server.setTask <- function(taskId, 
                           expr,
                           exports,
                           interval){
    taskId <- as.character(taskId)
    serverData$tasks[[taskId]] <- expr
    server.export(taskId = taskId, objects = exports)
    server.setTaskInterval(taskId = taskId, interval = interval)
}

server.eval<- function(taskId, expr){
    taskId <- as.character(taskId)
    if(is.null(serverData$taskData[[taskId]]))
        serverData$taskData[[taskId]] <- new.env(parent = globalenv())
    ## TODO: warning
    tryCatch(
        eval(expr = expr, 
             envir  = serverData$taskData[[taskId]]),
        error = function(e) e
    )
}

server.getTask <- function(taskId){
    taskId <- as.character(taskId)
    serverData$tasks[[taskId]]
}

server.export <- function(taskId, objects){
    taskId <- as.character(taskId)
    if(is.null(serverData$taskData[[taskId]])){
        serverData$taskData[[taskId]] <- new.env(parent = globalenv())
    }
    for(i in names(objects)){
        serverData$taskData[[taskId]][[i]] <- objects[[i]]
    }
}


server.copyTask <- function(sourceId, targetId){
    sourceId <- as.character(sourceId)
    targetId <- as.character(targetId)
    sourceEnv <- serverData$taskData[[sourceId]]
    serverData$taskData[[targetId]] <- as.environment(as.list(sourceEnv, all.names=TRUE))
    parent.env(serverData$taskData[[targetId]]) <- globalenv()
    serverData$tasks[[targetId]] <- serverData$tasks[[sourceId]]
}

server.deleteTask <- function(taskId){
    taskId <- as.character(taskId)
    for(i in taskId){
        serverData$taskData[[i]] <- NULL
        serverData$tasks[[i]] <- NULL
    }
}

server.response <- function(con, x){
    response <- list(data = x)
    writeData(con, response)
}


server.setTaskInterval <- function(taskId, interval){
    serverData$taskIntervals[[as.character(taskId)]] <- interval
}
