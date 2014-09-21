## About  

This is an R package to obtain data from the EMBL-EBI Proteomics Repository Identifications Database (PRIDE). It uses its RESTful Web Service for that purpose.  

Currently, the following domain entities are supported:  

* Projects as S4 objects, including methods to get them from PRIDE by accession and `as.data.frame`  
* A data frame with Projects (in ways of deprecation in favor of S4 class based methods)  
* A data frame with Proteins, in the context of a given project (deprecating as previous)  

### Installation  

First, we need to install `devtools`:  

    install.packages(devtools)
    library(devtools)
    
Then we just call  

    install_github(username="jadianes", repo="prider")

### Future Works  

Some things to be done, sooner than later:  

- Check mandatory parameters
- Convert `projects.df.R` and `proteins.df.R` into OO interfaces as `project.summary.R`

