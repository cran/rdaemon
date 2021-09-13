request.setTask <- function(taskId, 
                            expr,
                            exports,
                            interval){
    list(
        taskId = taskId,
        type = "setTask",
        data = expr,
        exports = exports,
        interval = interval
    )
}

request.eval <- function(taskId, expr){
    list(
        pid = Sys.getpid(),
        taskId = taskId,
        type = "eval",
        data = expr
    )
}

request.getTask <- function(taskId){
    list(
        pid = Sys.getpid(),
        taskId = taskId,
        type = "getTask"
    )
}

request.export <- function(taskId, objects){
    list(
        taskId = taskId,
        type = "export",
        data = objects
    )
}

request.deleteTask <- function(taskId){
    list(
        taskId = taskId,
        type = "deleteTask"
    )
}

request.copyTask <- function(sourceId, targetId){
    list(
        taskId = targetId,
        type = "copyTask",
        data = sourceId
    )
}

request.handshake <- function(){
    list(
        pid = Sys.getpid(),
        type = "handshake"
    )
}

request.oneTimeConnection <- function(request){
    list(
        pid = Sys.getpid(),
        type = "oneTimeConnection",
        data = request
    )
}

request.close <- function(taskIds){
    list(
        pid = Sys.getpid(),
        type = "close",
        data = taskIds
    )
}



isSetTaskRequest <- function(msg){
    msg$type == "setTask"
}

isEval <- function(msg){
    msg$type == "eval"
}

isGetTaskRequest <- function(msg){
    msg$type == "getTask"
}

isExportRequest <- function(msg){
    msg$type == "export"
}

isDeleteTaskRequest <- function(msg){
    msg$type == "deleteTask"
}

isCopyTask <- function(msg){
    msg$type == "copyTask"
}

isClose <- function(msg){
    msg$type == "close"
}


isHandshake <- function(msg){
    msg$type == "handshake"
}

isOneTimeConnection <- function(msg){
    msg$type == "oneTimeConnection"
}

