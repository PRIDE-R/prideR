## About
This is an R package to obtain data from the EMBL-EBI Proteomics Repository Identifications Database (PRIDE). It uses its RESTful Web Service for that purpose.

### Installation

First, we need to install *devtools*:  

    install.packages(devtools)
    library(devtools)
    
Then we just call  

    install_github(username="jadianes", repo="prider")

### Future Works
Some things to be done, sooner than later:  

- Check mandatory parameters
- Wrap the data frame and some statistics into a Class (probably S4?)

