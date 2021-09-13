truncateLongName <- function(name, warn = TRUE){
    maxLen <- getNameMaxLen() - nchar(daemonPortName(""))
    if(nchar(name) > maxLen){
            name <- substr(name, 0, maxLen)
            if(warn)
                warning("The daemon name exceeds the name length limit ", 
                        "and will be truncated to '", name, "'")
    }
    name
}

getSharedMemoryName <- function(name){
    if(getOS() == "windows"){
        paste0("Local\\rd_",name)
    }else{
        paste0("/rd_",name)
    }
}

daemonPortName <- function(name){
    getSharedMemoryName(paste0(name, "_port"))
}

daemonPidName <- function(name){
    getSharedMemoryName(paste0(name, "_pid"))
}

daemonConnectionName <- function(name){
    getSharedMemoryName(paste0(name, "_con"))
}

getDaemonPort <- function(name){
    getGlobalVariable(daemonPortName(name))
}

getDaemonPid <- function(name){
    getGlobalVariable(daemonPidName(name))
}

getDaemonConnection <- function(name){
    con <- getGlobalVariable(daemonConnectionName(name))
    if(is.na(con))
        FALSE
    else
        as.logical(con)
}

setDaemonPort <- function(name, port){
    setGlobalVariable(daemonPortName(name), port)
}

setDaemonPid <- function(name, pid){
    setGlobalVariable(daemonPidName(name), pid)
}

setDaemonConnection <- function(name, hasConnection){
    setGlobalVariable(daemonConnectionName(name), as.integer(hasConnection))
}