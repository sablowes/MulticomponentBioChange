############################################################################
### set directories and load packages
############################################################################

# Set user dependent working directories

user <- Sys.info()["user"]
path2wd <- switch(user,
                  "sb25gaqy" = "~/Dropbox/1current/multidimensionalChangeMS/",
                  'jc155893' = '~/Dropbox/1current/multidimensionalChangeMS/multiComponentChange/'
)

setwd(path2wd)
   
############################################################################
### helper functions
############################################################################

### helper function to try things out
is.error <- function(x) inherits(x, "try-error")

############################################################################
### Load all needed libraries
############################################################################
# TODO: check if all these are required...
needed_libs <- c("brms",
                 'vegan',
                 "cowplot",
                 "devtools",
                 "iNEXT", # for coverage standardized richness
                 "ggridges",
                 "gridGraphics",
                 #"magick",
                 #"maps",
                 "mobr", # calculation of biodiversity indices
					       "mobsim", # for random community simulation
                 "tidyverse",
                 "stringr",
					        "viridisLite"
)

usePackage <- function(p) {
   if (!is.element(p, installed.packages()[,1])) {   
      if(p == "mobr") {install_github('MoBiodiv/mobr')}  
      install.packages(p, dep = TRUE)
   }
   require(p, character.only = TRUE)
}

sapply(needed_libs, usePackage)
rm(usePackage)

### document system properties
# session_info() 

# gg_theme 
theme_set(theme_minimal())
