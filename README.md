[![Build Status](https://travis-ci.org/PRIDE-R/prideR.svg)](https://travis-ci.org/PRIDE-R/prideR)
prideR
======

This is an R package to obtain data from the EMBL-EBI Proteomics Repository Identifications Database ([PRIDE Archive](http://www.ebi.ac.uk/pride/archive/) and [PRIDE Cluster](http://wwwdev.ebi.ac.uk/pride/cluster)). It uses its RESTful Web Services at [PRIDE Archive WS](http://www.ebi.ac.uk/pride/ws/archive/) and [PRIDE Cluster WS](http://wwwdev.ebi.ac.uk/pride/ws/archive) for that purpose.  

Currently, the following domain entities are supported:  

* Projects as S4 objects, including methods to get them from PRIDE by accession and `as.data.frame`  
* Assays as S4 objects, including methods to get them from PRIDE by accession and `as.data.frame`  
* Files as S4 objects, including methods to get them from PRIDE by project and assay accession and `as.data.frame`  
* Protein identifications associated with a project, as S4 objects, including methods to get them from PRIDE by project accession and `as.data.frame`  
* PSM identifications associated with a project, as S4 objects, including methods to get them from PRIDE by project accession and `as.data.frame`  
* PRIDE Cluster ClusterSummary, as S4 objects and as `as.data.frame`.  

### Installation  

First, we need to install `devtools`:  

    install.packages("devtools")
    library(devtools)
   
Then we just call  

    install_github("PRIDE-R/prideR")
    library(prideR)

### Examples  

#### PRIDE Archive  

Get project `PXD000001` summary:  

    get.ProjectSummary("PXD000001")

Search for at most 20 projects by term `blood`. The results are returned as a `list` of `ProjectSummary` objects:  

    search.list.ProjectSummary("blood",20)

Get them as a `data.frame`:  

    list.to.data.frame(search.list.ProjectSummary("blood",20))

Get the first 50 Proteins for project `PXD000001` as a list of `ProteinDetail` objects:  

    protein.list(list.ProteinDetailList("PXD000001", 0, 50))

Or as a `data.frame`:  

    as.data.frame(list.ProteinDetailList("PXD000001",0, 50))

Plot some counts:  

    plot(list.ProteinDetailList("PXD000001",0, 50))

Get 5 PSMs for project `PXD000001` as a list of `PsmDetail` objects:  

    get.list.PsmDetail("PXD000001", 5)

There are also count methods for each of the PRIDE Archive entitites.  

#### PRIDE Cluster  

Get page 0 with a size of 20 clusters for peptide sequence *ELISN*:  

    search.ClusterSearchResults("elisn", 0, 20)

As a data frame:  

    as.data.frame(search.ClusterSearchResults("elisn", 1, 20))

Plot results:

    plotresults(search.ClusterSearchResults("elisn", 0, 20))

### Future Works  

Some things to be done, sooner than later:  

- Check mandatory parameters  
- Deal with `SpectrumDetail` entities when available  

### How to cite

* Vizcaíno, J. A., Côté, R. G., Csordas, A., Dianes, J. A., Fabregat, A., Foster, J. M., ... & Hermjakob, H. (2013). *The PRoteomics IDEntifications (PRIDE) database and associated tools: status in 2013*. Nucleic acids research, 41(D1), D1063-D1069. [HERE](http://www.nature.com/nbt/journal/v32/n3/full/nbt.2839.html)  

