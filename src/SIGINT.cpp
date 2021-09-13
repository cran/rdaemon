#ifdef _WIN32
#include <windows.h>

// From https://stackoverflow.com/questions/813086/can-i-send-a-ctrl-c-sigint-to-an-application-on-windows
// with a minor change
// [[Rcpp::export]]
bool send_SIGINT(long long unsigned pid)
{
    DWORD dwProcessId = pid;
    DWORD dwCtrlEvent = CTRL_C_EVENT;
    bool success = false;
    DWORD thisConsoleId = GetCurrentProcessId();
    // Leave current console if it exists
    // (otherwise AttachConsole will return ERROR_ACCESS_DENIED)
    bool consoleDetached = (FreeConsole() != FALSE);

    if (AttachConsole(dwProcessId) != FALSE)
    {
        // Add a fake Ctrl-C handler for avoid instant kill is this console
        // WARNING: do not revert it or current program will be also killed
        SetConsoleCtrlHandler(nullptr, true);
        success = (GenerateConsoleCtrlEvent(dwCtrlEvent, 0) != FALSE);
        FreeConsole();
    }

    if (consoleDetached)
    {
        // Create a new console if previous was deleted by OS
        if (AttachConsole(thisConsoleId) == FALSE)
        {
            int errorCode = GetLastError();
            if (errorCode == 31) // 31=ERROR_GEN_FAILURE
            {
                AllocConsole();
            }
        }
    }
    return success;
}
#else
bool send_SIGINT(long long unsigned pid){
    return true;
}
#endif
