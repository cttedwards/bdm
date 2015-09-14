
context("plots")

test_that("diagnostic plots", {
    # load some data
    data(albio)
    dat <- bdmData(harvest = albio$catch, index = albio$cpue, time = rownames(albio))
    # initialise, compile and run
    mdl <- bdm()
    capture.output(mdl <- compiler(mdl))
    capture.output(mdl <- sampler(mdl, dat))
    # plots
    gg <- traceplot(mdl)
    expect_is(gg, "ggplot")
    gg <- histplot(mdl)
    expect_is(gg, "ggplot")
    gg <- cumsumplot(mdl)
    expect_is(gg, "ggplot")
    gg <- dynplot(mdl)
    expect_is(gg, "ggplot")
    gg <- idxplot(mdl)
    expect_is(gg, "ggplot")
})


