## Borrowed from 
## https://rdrr.io/github/quarto-dev/quarto-r/src/R/daemon.R
## with minor changes
findPort <- function() {
  for (i in 1:20) {
    # determine the port (exclude those considered unsafe by Chrome)
    while(TRUE) {
      port <- 3000 + sample(5000, 1)
      if (!port %in% c(3659, 4045, 6000, 6665:6669,6697))
        break
    }
    # see if it's valid
    if (!portOccupied(port)) {
      return(port)
    }
  }
  NULL
}

portOccupied <- function(port) {
  tryCatch({
    suppressWarnings(con <- serverSocket(port))
    close(con)
    FALSE
  }, error = function(e) TRUE)
}


writeData <- function(con, data){
    writeLines(
        base64enc::base64encode(serialize(data, NULL)), 
        con)
}

readData <- function(con, n = -1){
    data <- readLines(con, n = n)
    if(length(data)){
        lapply(data, function(x)unserialize(base64enc::base64decode(x)))
    }else{
        NULL
    }
}

waitData <- function(con, timeout = 10){
    startTime <- Sys.time()
    data <- NULL
    while(.difftime(Sys.time(), startTime) < timeout){
        data <- readData(con, n = 1)[[1]]
        if(!is.null(data))
            break
    }
    data
}

flushData <- function(con){
    readLines(con)
}
