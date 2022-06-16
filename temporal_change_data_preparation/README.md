BioTIMEx
--------

### Description

This research compendium regroups scripts used to download, re-structure
and aggregate data sets to constitute a large meta-analysis of
communities in experimental setups sampled several times. The code found
here was originally versionned using git and stored on github &lt;&gt;,
as a standalone project. It was then merged with the analysis project in

and was eventually submitted to Zenodo &lt;&gt;. This code accompanies
the article: 

**Local biodiversity change reflects interactions among changing abundance, evenness and richness**

*by Shane A. Blowes, Gergana N. Daskalova, Maria Dornelas, Thore Engel, Nicholas J. Gotelli, Anne Magurran, Inês S. Martins, Brian McGill, Daniel J. McGlinn, Alban Sagouis, Hideyasu Shimadzu, Sarah R. Supp, and Jonathan M. Chase*


### Reproducibility and R environment

To ensure that the working environment (R version and package versions)
are documented and isolated, the package renv
(<a href="https://rstudio.github.io/renv/index.html" class="uri">https://rstudio.github.io/renv/index.html</a>)
was used. To download data and reproduce the data manipulation, please run the code in
the master script in the [temporal_change_data_preparation folder](www.github.com/sablowes/MulticomponentBioChange/temporal_change_data_preparation).  
The `renv::run()` function used runs the code in an isolated session with the
correct paths and package versions.

### Methods

Data sets were originally searched for among LTER data sets and suitable
open access data stored on EPI were selected
(<a href="https://portal.edirepository.org/nis/home.jsp" class="uri">https://portal.edirepository.org/nis/home.jsp</a>).

Suitable data sets were individually downloaded from R. Scripts managing
these downloads are grouped inside `temporal_change_data_preparation/R/data download/`. These scripts
follow EDI process of data checking and formatting. You should run all
these scripts at once by running the corresponding command here or from the master script.  

All downloaded data sets are saved in separate folders named following
the convention `author_year`.  

In a second step, each data set is re-structured or wrangled to fit a
common format before analysis. The scripts turning the original
heterogeneously structured data sets into comparable tables are in the
`./R/data wrangling/` folder. You should run all these scripts at once by
running the corresponding command in the master script.  

Finally, all restructured tables are aggregated together in a final
table by the `.R/3.0_merging_long-format_tables.r` script. The structure
of the end-product table is a long format with each row recording the
composition of a community in one place at a given time. Format is
described in `./data/template long format.txt` and variables are
defined.

### Analyses

Further analyses were carried at on R too by Shane Blowes and
collaborators in [the MulticomponentBioChange repository.](www.github.com/sablowes/MulticomponentBioChange)

