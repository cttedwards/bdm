
context("project")

test_that("project", {
    # get some data
    data(haknz)
    dat <- bdmData(harvest = haknz$landings, index = haknz$cpue)
     
    # initialize and fit default model
    mdl <- bdm()
    capture.output(mdl <- compiler(mdl))
    capture.output(mdl <- sampler(mdl, dat, run = 'example_run'))

    # constant harvest rate projection scenario
    mdl.project <- project(mdl, harvest = 0.10, time = 10, harvest_rate = TRUE)
    
    expect_is(mdl.project, "list")
    expect_equal(mdl@run, mdl.project$run)
    expect_equal(mdl.project$harvest_rate[4000, 48, 1], 0.1)
    
    # constant harvest projection scenario
    mdl.project <- project(mdl, harvest = 1000, time = 10, harvest_rate = FALSE)
    
    expect_is(mdl.project, "list")
    expect_equal(mdl@run, mdl.project$run)
    expect_equal(mdl.project$harvest[4000, 48, 1], 1000)
})


