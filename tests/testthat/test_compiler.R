
context("compiler")

test_that("rstan compilation", {
    # initialize code directly
    expect_error(toy(model.code = "parameters {real y;} model {y ~ normal(0,1);}"))
    # must provide a model name
    mdl <- toy(model.code = "parameters {real y;} model {y ~ normal(0,1);}", model.name = "toy_model")
    mdl <- compiler(mdl)
    expect_is(mdl, "toy")
    expect_is(mdl@dso, "cxxdso")
    expect_true(mdl@dso@dso_saved)
    # initialize code indirectly
    # model name generated automatically
    toy_model <- "parameters {real y;} model {y ~ normal(0,1);}"
    mdl <- toy(model.code = toy_model)
    mdl <- compiler(mdl)
    expect_is(mdl, "toy")
    expect_is(mdl@dso, "cxxdso")
    expect_true(mdl@dso@dso_saved)
    # default initialization
    mdl <- toy()
    mdl <- compiler(mdl)
    expect_is(mdl, "toy")
    expect_is(mdl@dso, "cxxdso")
    expect_true(mdl@dso@dso_saved)
})