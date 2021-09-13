#include <string>
#include <map>
#define STRICT_R_HEADERS
#include "Rcpp.h"

#ifdef _WIN32
#include <windows.h>
#else
#include <sys/mman.h>
#include <sys/stat.h>        /* For mode constants */
#include <fcntl.h>           /* For O_* constants */
#include <unistd.h>          /* For close file descriptor */
#include <errno.h>
#include <string.h>
#endif


// [[Rcpp::export]]
unsigned int getNameMaxLen(){
    #ifdef __APPLE__
    #ifdef SHM_NAME_MAX
    return SHM_NAME_MAX - 1;
    #else
    return 32 - 1;
    #endif
    #endif
    #ifdef unix
    return NAME_MAX - 1;
    #endif
    #ifdef _WIN32
        return MAX_PATH;
    #endif
    return 1024;
}


#ifdef _WIN32
std::map<std::string, HANDLE> handleMap;
std::map<std::string, int *> mappedMemoryMap;

//From: https://stackoverflow.com/questions/1387064/how-to-get-the-error-message-from-the-error-code-returned-by-getlasterror
//Returns the last Win32 error, in string format. Returns an empty string if there is no error.
std::string GetLastErrorAsString()
{
    //Get the error message ID, if any.
    DWORD errorMessageID = ::GetLastError();
    if(errorMessageID == 0) {
        return std::string(); //No error message has been recorded
    }
    
    LPSTR messageBuffer = nullptr;

    //Ask Win32 to give us the string version of that message ID.
    //The parameters we pass in, tell Win32 to create the buffer that holds the message for us (because we don't yet know how long the message string will be).
    size_t size = FormatMessageA(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                                 NULL, errorMessageID, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPSTR)&messageBuffer, 0, NULL);
    
    //Copy the error message into a std::string.
    std::string message(messageBuffer, size);
    
    //Free the Win32's string's buffer.
    LocalFree(messageBuffer);
            
    return message;
}

// [[Rcpp::export]]
bool existsGlobalVariable(std::string name)
{
    HANDLE hMapFile = CreateFileMappingA(
        INVALID_HANDLE_VALUE, // use paging file
        NULL,                 // default security
        PAGE_READONLY,        // read access
        0,                    // maximum object size (high-order DWORD)
        1,                    // maximum object size (low-order DWORD)
        name.c_str());
    bool exist = (GetLastError() == ERROR_ALREADY_EXISTS);
    CloseHandle(hMapFile);
    return exist;
}

void createGlobalVariable(std::string  name, int size)
{
    HANDLE hMapFile;
    int *pBuf;

    if (handleMap.find(name) != handleMap.end())
    {
        return;
    }

    hMapFile = CreateFileMappingA(
        INVALID_HANDLE_VALUE, // use paging file
        NULL,                 // default security
        PAGE_READWRITE,       // read/write access
        0,                    // maximum object size (high-order DWORD)
        size,                 // maximum object size (low-order DWORD)
        name.c_str());        // name of mapping object

    if (hMapFile == NULL)
    {
        Rcpp::stop("Fail to create file mapping! Error: %s", GetLastErrorAsString().c_str());
        return;
    }
    pBuf = (int *)MapViewOfFile(hMapFile,            // handle to map object
                                      FILE_MAP_ALL_ACCESS, // read/write permission
                                      0,
                                      0,
                                      size);

    if (pBuf == NULL)
    {
        Rcpp::stop("Fail to map view of file! Error: %s", GetLastErrorAsString().c_str());
        return;
    }

    handleMap.emplace(name, hMapFile);
    mappedMemoryMap.emplace(name, pBuf);
}

// [[Rcpp::export]]
void setGlobalVariable(std::string  name, int value)
{
    if (handleMap.find(name) == handleMap.end())
    {
        createGlobalVariable(name, sizeof(int));
    }
    int *ptr = mappedMemoryMap.at(name);
    *ptr = value;
}

// [[Rcpp::export]]
int getGlobalVariable(std::string  name)
{
    if (handleMap.find(name) == handleMap.end())
    {
        if(!existsGlobalVariable(name))
            return NA_INTEGER;
        else
            createGlobalVariable(name, sizeof(int));
    }
    return *mappedMemoryMap.at(name);
}

// [[Rcpp::export]]
void unsetGlobalVariable(std::string  name)
{
    if (handleMap.find(name) != handleMap.end())
    {
        UnmapViewOfFile(mappedMemoryMap.at(name));
        CloseHandle(handleMap.at(name));
    }
}

#else

bool existsGlobalVariable(std::string  name)
{
    int fd = shm_open(name.c_str(), O_RDONLY, S_IRUSR|S_IWUSR);

    bool exist = (fd != -1);
    if(exist)
        close(fd);
    return exist;
}

void setGlobalVariable(std::string  name, int value)
{
    size_t size = sizeof(int);
    bool exists = existsGlobalVariable(name);
    int fd = shm_open(name.c_str(), O_CREAT|O_RDWR, S_IRUSR|S_IWUSR);
    if(fd == -1){
        Rcpp::stop("Fail to create the shared memory file! Error: %s", strerror(errno));
        return;
    }
    if(!exists){
        int success = ftruncate(fd, size);
        if(success == -1){
            close(fd);
            Rcpp::stop("Fail to truncate the shared memory file! Error: %s", strerror(errno));
            return;
        }
    }
    int *ptr = (int *)mmap(NULL, size,
                                          PROT_READ|PROT_WRITE,
                                          MAP_SHARED, fd, 0);
    close(fd);
    if(ptr == (void*) -1){
        Rcpp::stop("Fail to perform the memory mapping!! Error: %s", strerror(errno));
        return;
    }
    *ptr = value;
    munmap(ptr, size);
}

int getGlobalVariable(std::string  name)
{
    size_t size = sizeof(int);
    int fd = shm_open(name.c_str(), O_RDONLY, S_IRUSR|S_IWUSR);
    if(fd == -1){
        return NA_INTEGER;
    }
    int *ptr = (int *)mmap(NULL, size,
                                          PROT_READ,
                                          MAP_SHARED, fd, 0);
    
    close(fd);
    if(ptr != (void*) -1){
        int value = *ptr;
        munmap(ptr, size);
        return value;
    }else{
        return NA_INTEGER;
    }
}

void unsetGlobalVariable(std::string  name)
{
    shm_unlink(name.c_str());
}

#endif
