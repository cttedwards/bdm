

# roxygenize
devtools::document()

# examples
devtools::run_examples(fresh = TRUE)

# run tests
devtools::test()

# generate vignettes
devtools::build_vignettes()

# final check
# (temporarily remove test folder to avoid repetition)
devtools::check(vignettes = FALSE)

# build and install to R library
system("Rcmd.exe INSTALL --no-multiarch --with-keep.source ../bdm")

# build local binary
devtools::build(binary = TRUE)



