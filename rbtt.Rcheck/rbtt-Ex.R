pkgname <- "rbtt"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "rbtt-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('rbtt')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("rbtt")
### * rbtt

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rbtt
### Title: Perform robust bootstrapped t-tests
### Aliases: rbtt

### ** Examples

x=rbinom(50,1,0.5)*rlnorm(50,0,1)
y=rbinom(150,1,0.3)*rlnorm(150,2,1)

rbtt(x, y, n.boot=999)

# Use 9999 bootstrap resamples on 2 cores
rbtt(x, y, n.boot=9999, n.cores=2)

# Use methods 1 or 2 individually
rbtt(x, y, n.boot = 999, method = 1)
rbtt(x, y, n.boot = 999, method = 2)

# Use a confidence level of 0.99
rbtt(x, y, n.boot = 999, conf.level = 0.99)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rbtt", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
