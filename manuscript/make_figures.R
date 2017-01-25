to_pdf <- function(expr, filename, ..., verbose=TRUE) {
  if(!file.exists(dirname(filename)))
    dir.create(dirname(filename), recursive=TRUE)
  if ( verbose )
    cat(sprintf("Creating %s\n", filename))
  pdf(filename, ...)
  on.exit(dev.off())
  eval.parent(substitute(expr))
}


source("functions and packages/startscripts.R")


fn <- function(...)file.path("manuscript",...)

to_pdf(source("master_scripts/allometryplotting.R"), fn("Figure1.pdf"), width=6, height=7.8)
to_pdf(source("master_scripts/airvars_plotting.R"), fn("Figure2.pdf"), width=6, height=8)
to_pdf(source("master_scripts/allocation_plotting.R"), fn("Figure3.pdf"), width=6, height=8)
to_pdf(source("master_scripts/Amax_TNC_N_plotting2.R"), fn("Figure4.pdf"), width=8, height=6)
to_pdf(source("master_scripts/cuefig.R"), fn("Figure4.pdf"), width=6, height=6)

