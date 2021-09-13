readTxt <- function(file){
    readChar(file, file.info(file)$size)
}

.difftime <- function(t1, t2){
    difftime(t1, t2, units = "secs")
}

truncateTxt <- function(x, n){
  for(i in seq_along(x))
    if(nchar(x[i]) > n)
      x[i] <- paste0(substring(x[i], 1, n), "...")
  x
}

.warning <- function(prefix){
    function(e){
        flog.warn(paste0(prefix, ": %s"), conditionMessage(e))
        tryInvokeRestart("muffleWarning")
    }
}

.error <- function(prefix){
    function(e){
        flog.error(paste0(prefix, ": %s"), conditionMessage(e))
    }
}

.suspendInterruptsIfRequired <- function(expr, interruptable){
    if(interruptable){
        expr
    }else{
        suspendInterrupts(expr)
    }
}

handleExceptions <- function(expr, warningPrefix, errorPrefix){
  tryCatch(
    {
      withCallingHandlers(
        expr,
        warning = .warning(warningPrefix),
        error = function(e) {
          flog.error(paste0(errorPrefix, ": %s"), conditionMessage(e))
          
          stack <- sys.calls()
          stack <- truncateTxt(sapply(stack, deparse), 50)
          stack <- stack[!grepl("[t|T]ryCatch", stack)]
          stack <- stack[!grepl("withCallingHandlers", stack, fixed = TRUE)]
          stack <- stack[!grepl(".handleSimpleError", stack, fixed = TRUE)]
          stack <- stack[!grepl("simpleError", stack, fixed = TRUE)]
          stack <- stack[!grepl("handleExceptions", stack, fixed = TRUE)]
          idx <- which(grepl("runTasks", stack, fixed = TRUE))
          if(length(idx)){
            stack <- stack[-seq_len(max(idx))]
          }
          flog.error("Calling stack: ")
          for(i in stack){
            flog.error("  %s", i)
          }
        }
      )
    },
    error = function(e) NULL
  )
}

isScalerChar <- function(x){
    length(x) == 1 && is.character(x)
}

checkDaemonArgs <- function(daemonName = NULL, taskId = NULL){
    if(!is.null(daemonName)){
        stopifnot(isScalerChar(daemonName))
    }
    if(!is.null(taskId)){
        stopifnot(isScalerChar(taskId))
    }
}


getOS <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}
