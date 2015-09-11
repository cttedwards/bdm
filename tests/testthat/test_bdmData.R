
context("bdmData object")

test_that('initialize bdmData', {
    dat <- bdmData(index = runif(10), harvest = 1:10)
    expect_is(dat, "bdmData")
})

test_that('sigmao(dat) initialise', {
    # check dimensions
    dat <- bdmData(index = runif(10), harvest = 1:10)
    expect_equal(dim(sigmao(dat)), c(10, 1))
    dat <- bdmData(index = matrix(runif(20), 10, 2), harvest = 1:10)
    expect_equal(dim(sigmao(dat)), c(10, 2))
    # check values
    sigmao.in <- matrix(runif(20), 10, 2)
    dat <- bdmData(index = matrix(runif(20), 10, 2), harvest = 1:10, sigmao = sigmao.in)
    expect_equal(dim(sigmao(dat)), c(10, 2))
    expect_equal(sigmao(dat)[,1], sigmao.in[,1])
    expect_equal(sigmao(dat)[,2], sigmao.in[,2])
    sigmao.in <- runif(2)
    dat <- bdmData(index = matrix(runif(20), 10, 2), harvest = 1:10, sigmao = sigmao.in)
    expect_equal(dim(sigmao(dat)), c(10, 2))
    expect_equal(sigmao(dat)[,1], rep(sigmao.in[1],10))
    expect_equal(sigmao(dat)[,2], rep(sigmao.in[2],10))
    sigmao.in <- runif(1)
    dat <- bdmData(index = runif(10), harvest = 1:10, sigmao = sigmao.in)
    expect_equal(dim(sigmao(dat)), c(10, 1))
    expect_equal(sigmao(dat)[,1], rep(sigmao.in,10))
})

test_that('sigmao(dat) assignment', {
    # assign matrix for >1 index
    dat <- bdmData(index = matrix(runif(20), 10, 2), harvest = 1:10)
    sigmao.in <- matrix(runif(20), 10, 2)
    sigmao(dat) <- sigmao.in
    expect_equal(dim(sigmao(dat)), c(10, 2))
    expect_equal(sigmao(dat)[,1], sigmao.in[,1])
    expect_equal(sigmao(dat)[,2], sigmao.in[,2])
    # assign numeric for >1 index
    dat <- bdmData(index = matrix(runif(20), 10, 2), harvest = 1:10)
    sigmao.in <- runif(2)
    sigmao(dat) <- sigmao.in
    expect_equal(dim(sigmao(dat)), c(10, 2))
    expect_equal(sigmao(dat), matrix(sigmao.in, 10, 2, byrow = TRUE))
    sigmao.in <- runif(1)
    sigmao(dat) <- sigmao.in
    expect_equal(dim(sigmao(dat)), c(10, 2))
    expect_equal(sigmao(dat), matrix(sigmao.in, 10, 2))
    # assign numeric for 1 index
    dat <- bdmData(index = runif(10), harvest = 1:10)
    sigmao.in <- runif(1)
    sigmao(dat) <- sigmao.in
    expect_equal(dim(sigmao(dat)), c(10, 1))
    expect_equal(sigmao(dat), matrix(sigmao.in, 10, 1))
})

test_that('shape(dat) assignment', {
    dat <- bdmData(index = runif(10), harvest = 1:10)
    shape.in   <- runif(1, 0.1, 0.9)
    shape(dat) <- shape.in
    expect_less_than(abs(shape(dat) - shape.in), .Machine$double.eps^0.25)
    n <- shape(dat, 'n')
    expect_less_than(abs((1/n)^(1/(n-1)) - shape.in), .Machine$double.eps^0.25)
})

test_that('plot bdmData', {
    # load some data
    data(albio)
    dat <- bdmData(harvest = albio$catch, index = albio$cpue, time = rownames(albio))
    # plots
    gg <- plot(dat)
    expect_is(gg, "ggplot")
})



