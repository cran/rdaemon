#define STRICT_R_HEADERS
#include "Rcpp.h"
#include <string>

#ifdef _WIN32
#include <windows.h>
#else
#include <unistd.h>
#include <string.h>
#include <errno.h>
#endif

// [[Rcpp::export]]
void detachConsole()
{
#ifdef _WIN32
    FreeConsole();
#else
     if ( getppid() == 1 ) {
         Rprintf("The current process is the group leader!\n");
         //Rf_error("Cannot detach group leader!");
     }
    int sid = setsid();  
    if (sid < 0)    
    {  
        Rcpp::stop("Fail to detach! Error: %s", strerror(errno));
    }  
#endif
}

