
context("update prior")

test_that("update", {
    mdl <- bdm()
    mdl <- updatePrior(mdl, list(par = 'r', meanlog = -1.1, sdlog = 0.1))
    mdl <- updatePrior(mdl, list(par = 'logK', min = 1, max = 100))
    expect_equal(getr(mdl)[['E[log(r)]']], -1.1)
    expect_equal(getr(mdl)[['SD[log(r)]']], 0.1)
    expect_equal(getlogK(mdl)[['min[logK]']], 1)
    expect_equal(getlogK(mdl)[['max[logK]']], 100)
})


