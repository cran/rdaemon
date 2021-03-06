% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/daemon-export.R
\name{rdaemon-methods}
\alias{rdaemon-methods}
\alias{lastRegisteredDaemon}
\alias{lastRegisteredTaskId}
\alias{registerDaemon}
\alias{deregisterDaemon}
\alias{killDaemon}
\alias{daemonExists}
\alias{daemonSetTask}
\alias{daemonSetTaskScript}
\alias{daemonSetTaskInterval}
\alias{daemonGetTaskInterval}
\alias{daemonEval}
\alias{daemonGetTask}
\alias{daemonExport}
\alias{daemonLogs}
\alias{daemonCopyTask}
\title{The daemon functions}
\usage{
lastRegisteredDaemon()

lastRegisteredTaskId()

registerDaemon(
  daemonName = lastRegisteredDaemon(),
  logFile = NULL,
  threshold = c("INFO", "WARN", "ERROR", "DEBUG", "TRACE")
)

deregisterDaemon(daemonName = lastRegisteredDaemon(), deleteTask = TRUE)

killDaemon(daemonName = lastRegisteredDaemon())

daemonExists(daemonName = lastRegisteredDaemon())

daemonSetTask(
  expr = NULL,
  daemonName = lastRegisteredDaemon(),
  taskId = lastRegisteredTaskId(),
  expr.char = NULL,
  interval = 1L,
  exports = list()
)

daemonSetTaskScript(
  script,
  daemonName = lastRegisteredDaemon(),
  taskId = lastRegisteredTaskId(),
  interval = 1,
  exports = list()
)

daemonSetTaskInterval(
  interval,
  daemonName = lastRegisteredDaemon(),
  taskId = lastRegisteredTaskId()
)

daemonGetTaskInterval(
  interval,
  daemonName = lastRegisteredDaemon(),
  taskId = lastRegisteredTaskId()
)

daemonEval(
  expr,
  daemonName = lastRegisteredDaemon(),
  taskId = lastRegisteredTaskId(),
  expr.char = NULL
)

daemonGetTask(
  daemonName = lastRegisteredDaemon(),
  taskId = lastRegisteredTaskId()
)

daemonExport(
  ...,
  daemonName = lastRegisteredDaemon(),
  taskId = lastRegisteredTaskId()
)

daemonLogs(daemonName = lastRegisteredDaemon(), prettyPrint = TRUE)

daemonCopyTask(
  sourceId,
  targetId = lastRegisteredTaskId(),
  daemonName = lastRegisteredDaemon()
)
}
\arguments{
\item{daemonName}{character(1), the name of the daemon. If not provided,
the last registered daemon or the default daemon name will be used. Note
that there are length limit on the number of characters in the name and the limit
varys with the OS. For portability, we recommend using a shorter name.}

\item{logFile}{character(1), the path to the log file. If the file
does not exist, it will be created. Otherwise, the file will be overwritten.
You can also use `daemonLogs()` to obtain the log.}

\item{threshold, }{character(1), the verbose level of the daemon log.
The value must be one of "INFO", "WARN", "ERROR", "DEBUG", or "TRACE".}

\item{deleteTask}{logical(1), whether to delete the task set by
the current R session.}

\item{expr}{expression(), an R expression that can be run by the daemon.}

\item{taskId}{character(1), the ID of the task. If not specified, the 
task ID from the previous `daemonSetTask` call will be used. If `daemonSetTask`
is never called, the default task ID will be provided.}

\item{expr.char}{character(1), An R expression in the character format.}

\item{interval}{numeric(1), the time to wait in seconds between 
two executions of the same task.}

\item{exports}{list, the variables that will be exported to the task
environment.}

\item{script}{character(1), the path to the script.}

\item{...}{Named arguments that will be exported.}

\item{prettyPrint}{logical(1), whether to print out the log in a pretty format}

\item{sourceId}{character(1), the source task ID.}

\item{targetId}{character(1), the target task ID.}
}
\value{
`lastRegisteredDaemon`: character(1)

`lastRegisteredTaskId`: character(1)

`registerDaemon`: invisible()

`deregisterDaemon`: invisible()

`killDaemon`: invisible()

`daemonExists`: logical(1)

`daemonSetTask`: logical(1)

`daemonSetTaskScript`: invisible()

`daemonSetTaskInterval`: invisible()

`daemonGetTaskInterval`: numeric(1) or NULL

`daemonEval`: The result of the expression

`daemonGetTask`: An expression

`daemonExport`: invisible()

`daemonLogs`: character(n)

`daemonCopyTask`: invisible()
}
\description{
The functions for managing the daemon in the background.
The daemon is uniquely identified by a daemon name.
If the user do not specify the daemon name, each R process
will create its own daemon using a unique daemon name by default.
However, it is also possible to let multiple R sessions connect 
with the same daemon if they use a common daemon name.
}
\details{
`lastRegisteredDaemon`: Get the name of the last registered daemon,
or a unique daemon name for the current process if no daemon 
is registered.

`lastRegisteredTaskId`: Get the last task ID set via `daemonSetTask`,
or a unique task ID for the current process if no task 
is set.

`registerDaemon`: create a new daemon or connect with an existing daemon
identified by the daemon name. This function cannot be called in
the daemon.

`deregisterDaemon`: 
disconnect a daemon and (optionally) remove all tasks in the daemon
which are set by the current process. 
The daemon might keep running in the background
until no task is scheduled or is killed by the other process.
This function cannot be called in the daemon.

`killDaemon`: Kill a daemon.

`daemonExists`: Whether the daemon exists.

`daemonSetTask`: set the task expression that 
will be evaluated by the daemon. Calling the function without `expr` and
`expr.char` arguments will remove the task specified by `taskId`.

`daemonSetTaskScript`: set the task expression that will be 
evaluated by the daemon using a script file. 
Note that the script file will only be read once and further changes
in the script file after calling this function would not affect the 
task expression.
This function will ultimately call `daemonSetTask` to set the task, 
so it also affects the value returned by `lastRegisteredDaemon`.

`daemonSetTaskInterval`: set the time to wait in seconds
between two executions of the same task. Because the daemon
schedule all tasks sequentially, it is possible to have a 
long-running task jamming the execution of all the other tasks.
Therefore, the interval should be considered as the minimal 
waiting time.

`daemonGetTaskInterval`: get the time to wait in seconds
between two executions of the same task.

`daemonEval`: Evaluate an expression in the daemon.

`daemonGetTask`: get the task expression from the daemon

`daemonExport`: export variables to a task environment in the daemon. 
Each variable in `...` should be named.

`daemonLogs`: Get the log of the daemon.

`daemonCopyTask`: Copy the task `sourceId` to `targetId`. 
The task expression and data will be copied. If the task `targetId` is
not empty, the existing content will be overwritten.
}
\examples{
## Check the default daemon name and task ID
## These will be used in the subsequent calls
## in this example
lastRegisteredDaemon()
lastRegisteredTaskId()

## Register a daemon using the default daemon name
registerDaemon()
daemonLogs()

## Check if the daemon is running
daemonExists()

## Set a task run by the daemon
daemonSetTask(message("Hello from daemon"))

## See the daemon log for the output
Sys.sleep(1)
daemonLogs()

## Get the task expression from the daemon
daemonGetTask()

## Export an object to the task in the daemon
daemonExport(i = 1)

## Evaluate an expression in the daemon
daemonEval(i)

## Close the connection to the daemon and remove the task
## set by this process
## The daemon will silently quit after a few minutes 
## if no other tasks exist.
deregisterDaemon()
daemonExists()

## explicitly kill the daemon if you are impatient
killDaemon()
daemonExists()
}
\seealso{
\code{\link{isProcessAlive}}, \code{\link{interruptProcess}}
}
