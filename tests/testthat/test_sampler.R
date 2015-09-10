
context("sampler")

test_that("rstan sampler", {
    bdm_model <- "parameters {real y;} model {y ~ normal(0,1);}"
    mdl <- bdm(model.code = bdm_model)
    mdl <- compiler(mdl)
    mdl <- sampler(mdl, iter = 100)
    expect_true(length(mdl@trace) > 0)
    expect_equal(mdl@nsamples, 200)
})


