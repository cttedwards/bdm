

# Get Version and date
VERSION <- scan('DESCRIPTION',what = character(),skip = 3,nlines = 1)[2]
DATE    <- Sys.Date()
TIME    <- Sys.time()

# increment development version
VERSION <- paste(substr(VERSION, start = 1, stop = 6), as.numeric(substr(VERSION, start = 7, stop = 10)) + 1, sep = '')

# update DESCRIPTION
DESCRIPTION    <- readLines('DESCRIPTION')
DESCRIPTION[4] <- paste('Version:', VERSION)
DESCRIPTION[5] <- paste('Date:', DATE)
writeLines(DESCRIPTION, 'DESCRIPTION')
rm(DESCRIPTION)

# Write .onAttach
filename <- "R/zzz.R"
cat(".onAttach <- function(libname, pkgname)\n", file = filename)
cat("{\n", file = filename, append = TRUE)
cat(paste("    packageStartupMessage(\"bdm beta version ", VERSION, " (", TIME, ")\")\n", sep = ""), file = filename, append = TRUE)
cat("}\n", file = filename, append = TRUE)
rm(filename)

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



