
context("compiler")

test_that("rstan compilation", {
    # initialize code directly
    expect_error(bdm(model.code = "parameters {real y;} model {y ~ normal(0,1);}"))
    # must provide a model name
    mdl <- bdm(model.code = "parameters {real y;} model {y ~ normal(0,1);}", model.name = "toy_model")
    capture.output(mdl <- compiler(mdl))
    expect_is(mdl, "bdm")
    expect_equal(mdl@model_name, "toy_model")
    expect_is(mdl@dso, "cxxdso")
    expect_true(mdl@dso@dso_saved)
    # initialize code indirectly
    # model name generated automatically
    toy_model <- "parameters {real y;} model {y ~ normal(0,1);}"
    mdl <- bdm(model.code = toy_model)
    capture.output(mdl <- compiler(mdl))
    expect_is(mdl, "bdm")
    expect_equal(mdl@model_name, "toy_model")
    expect_is(mdl@dso, "cxxdso")
    expect_true(mdl@dso@dso_saved)
    # default initialization
    mdl <- bdm()
    capture.output(mdl <- compiler(mdl))
    expect_is(mdl, "bdm")
    expect_equal(mdl@model_name, "default")
    expect_is(mdl@dso, "cxxdso")
    expect_true(mdl@dso@dso_saved)
})