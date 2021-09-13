## Client
test_that("Client set task",{
    taskId <- as.character(Sys.getpid())
    expr <- expression(5)
    exports <- list(a = 1)
    interval <- 10
    request <- request.setTask(taskId, 
                               expr = expr,
                               exports = exports,
                               interval = interval)
    processIndividualRequest(request)
    expect_equal(serverData$tasks[[taskId]], expr)
    expect_equal(serverData$taskData[[taskId]]$a, exports$a)
    expect_equal(serverData$taskIntervals[[taskId]], interval)
}
)

test_that("Client get task",{
    con <- file(tempfile(), open= 'w+')
    taskId <- Sys.getpid()
    request <- request.getTask(taskId)
    processIndividualRequest(request, con = con)
    seek(con, 0)
    response <- readData(con)[[1]]$data
    expect_equal(response, expression(5))
    close(con)
}
)


test_that("Client eval",{
    con <- file(tempfile(), open= 'w+')
    taskId <- Sys.getpid()
    expr <- expression(5)
    request <- request.eval(taskId, expr)
    processIndividualRequest(request, con = con)
    seek(con, 0)
    response <- readData(con)[[1]]$data
    expect_equal(response, eval(expr))
    close(con)
}
)

test_that("Client export",{
    taskId <- Sys.getpid()
    objects <- list(a = 1, b = 2)
    request <- request.export(taskId, objects)
    processIndividualRequest(request)
    
    taskEnv <- serverData$taskData[[as.character(taskId)]]
    expect_true(setequal(objects,as.list(taskEnv)))
}
)

test_that("Client copy task",{
    sourceId <- Sys.getpid()
    targetId <- Sys.getpid() + 1
    request <- request.copyTask(sourceId, targetId)
    processIndividualRequest(request)
    
    expr <- expression(5)
    objects <- list(a = 1, b = 2)
    expect_equal(expr, serverData$tasks[[as.character(targetId)]])
    
    taskEnv1 <- serverData$taskData[[as.character(sourceId)]]
    taskEnv2 <- serverData$taskData[[as.character(targetId)]]
    expect_true(setequal(objects, as.list(taskEnv2)))
    expect_false(identical(taskEnv1, taskEnv2))
}
)


test_that("Client delete task",{
    taskId <- Sys.getpid() + 1
    request <- request.deleteTask(taskId)
    processIndividualRequest(request)
    expect_equal(serverData$tasks[[as.character(taskId)]], NULL)
    expect_equal(serverData$taskData[[as.character(taskId)]], NULL)
}
)


test_that("Client close connection",{
    futile.logger::flog.threshold(WARN)
    taskId <- Sys.getpid()
    serverData$connections[[as.character(taskId)]] <- file(tempfile(), open= 'w+')
    request <- request.close(taskId)
    processIndividualRequest(request)
    expect_equal(serverData$connections[[as.character(taskId)]], NULL)
    expect_equal(serverData$tasks[[as.character(taskId)]], NULL)
    expect_equal(serverData$taskData[[as.character(taskId)]], NULL)
}
)


## server
test_that("Server set task",{
    taskId <- as.character(Sys.getpid())
    expr <- expression(5)
    exports <- list(a = 2)
    interval <- 5
    server.setTask(taskId, 
                   expr = expr,
                   exports = exports,
                   interval = interval)
    expect_equal(serverData$tasks[[taskId]], expr)
    expect_equal(serverData$taskData[[taskId]]$a, exports$a)
    expect_equal(serverData$taskIntervals[[taskId]], interval)
}
)

test_that("Server eval",{
    taskId <- Sys.getpid()
    expr <- expression(5)
    result <- server.eval(taskId = taskId, expr = expr)
    expect_equal(result, eval(expr))
}
)

test_that("Server get task",{
    taskId <- Sys.getpid()
    result <- server.getTask(taskId = taskId)
    expr <- expression(5)
    expect_equal(result, expr)
}
)


test_that("Server export",{
    taskId <- Sys.getpid()
    objects <- list(a = 1, b = 2)
    server.export(taskId = taskId, objects = objects)
    
    taskEnv <- serverData$taskData[[as.character(taskId)]]
    expect_true(setequal(objects,as.list(taskEnv)))
}
)

test_that("Server copy task",{
    sourceId <- Sys.getpid()
    targetId <- Sys.getpid() + 1
    server.copyTask(sourceId, targetId)
    
    expr <- expression(5)
    objects <- list(a = 1, b = 2)
    expect_equal(expr, serverData$tasks[[as.character(targetId)]])
    
    taskEnv1 <- serverData$taskData[[as.character(sourceId)]]
    taskEnv2 <- serverData$taskData[[as.character(targetId)]]
    expect_true(setequal(objects, as.list(taskEnv2)))
    expect_false(identical(taskEnv1, taskEnv2))
}
)


test_that("Server delete task",{
    taskId <- Sys.getpid() + 1
    server.deleteTask(taskId)
    expect_equal(serverData$tasks[[as.character(taskId)]], NULL)
    expect_equal(serverData$taskData[[as.character(taskId)]], NULL)
}
)
