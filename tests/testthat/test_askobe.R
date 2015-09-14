
context("as.kobe")

test_that("as.kobe", {
    # get some data
    data(haknz)
    dat <- bdmData(harvest = haknz$landings, index = haknz$cpue)
     
    # initialize and fit default model
    mdl <- bdm()
    capture.output(mdl <- compiler(mdl))
    capture.output(mdl <- sampler(mdl, dat, chains = 1))

    # constant harvest rate projection scenario
    mdl.project <- project(mdl, harvest = 0.10, time = 10, harvest_rate = TRUE)
    
    # check output class
    expect_is(as.kobe(mdl), "data.frame")
    expect_is(as.kobe(mdl, mdl.project), "data.frame")
})


