#' Utility functions for the daemon
#' 
#' Utility functions for the daemon
#' 
#' @param pids integer(n), the process IDs
#' 
#' @details 
#' `interruptProcess`: send SIGINT signal to the other process. 
#' The implementation for Windows is tricky and therefore 
#' it is only recommended to run this function in the daemon.
#' 
#' @returns 
#' `interruptProcess`: invisible()
#' @examples 
#' ## interrupt a process by the PID
#' \dontrun{
#' interruptProcess(pids = 1234L)
#' }
#' @rdname daemon-utils
#' @export
interruptProcess <- function(pids){
    if(length(pids)!=1){
        lapply(pids, interruptProcess)
        return(invisible())
    }
    pid <- as.integer(pids)
    if(Sys.info()[['sysname']]=="Windows"){
        send_SIGINT(pid)
    }else{
        tools::pskill(pid, tools::SIGINT)
    }
    invisible()
}


#' @details 
#' `isProcessAlive`: check whether a process is running
#' @returns 
#' `isProcessAlive`: logical(1)
#' @examples 
#' isProcessAlive(1234L)
#' 
#' @rdname daemon-utils
#' @export
isProcessAlive <- function(pids){
    vapply(as.integer(pids), isProcessRunning, logical(1))
}