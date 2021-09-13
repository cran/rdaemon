#define STRICT_R_HEADERS
#include "Rcpp.h"

#ifdef _WIN32
#include <windows.h>
#else
#include <signal.h>
#include <string.h>
#include <errno.h>
#endif

// [[Rcpp::export]]
bool isProcessRunning(long long unsigned pid)
{
    bool exist = false;
    #ifdef _WIN32
    HANDLE process = OpenProcess(SYNCHRONIZE, FALSE, pid);
    if(process == NULL){
        DWORD errorMessageID = ::GetLastError();
        if(errorMessageID == ERROR_ACCESS_DENIED){
            return true;
        }else{
            return false;
        }
    }
    DWORD ret = WaitForSingleObject(process, 0);
    CloseHandle(process);
    exist = (ret == WAIT_TIMEOUT);
    #else
    int res = kill(pid, 0);
    if(res == 0){
        exist = true;
    }else{
    if(res == -1){
        if(errno == ESRCH)
            exist = false;
        else if(errno == EPERM)
            exist = true;
        else
            Rcpp::stop("Fail to check the process status! Error: %s", strerror(errno));
    }else{
        Rcpp::stop("Unknown return value %d in kill().", res);
    }
    }
    #endif
    return exist;
}
