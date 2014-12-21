if (!isGeneric("id")) {
    setGeneric("id", function(object) standardGeneric("id"))
}

if (!isGeneric("id<-")) {
    setGeneric("id<-", function(object, value) standardGeneric("id<-"))
}

if (!isGeneric("average.precursor.mz")) {
    setGeneric("average.precursor.mz", function(object) standardGeneric("average.precursor.mz"))
}

if (!isGeneric("average.precursor.mz<-")) {
    setGeneric("average.precursor.mz<-", function(object, value) standardGeneric("average.precursor.mz<-"))
}

if (!isGeneric("average.precursor.charge")) {
    setGeneric("average.precursor.charge", function(object) standardGeneric("average.precursor.charge"))
}

if (!isGeneric("average.precursor.charge<-")) {
    setGeneric("average.precursor.charge<-", function(object, value) standardGeneric("average.precursor.charge<-"))
}

if (!isGeneric("num.spectra")) {
    setGeneric("num.spectra", function(object) standardGeneric("num.spectra"))
}

if (!isGeneric("num.spectra<-")) {
    setGeneric("num.spectra<-", function(object, value) standardGeneric("num.spectra<-"))
}

if (!isGeneric("max.ratio")) {
    setGeneric("max.ratio", function(object) standardGeneric("max.ratio"))
}

if (!isGeneric("max.ratio<-")) {
    setGeneric("max.ratio<-", function(object, value) standardGeneric("max.ratio<-"))
}

if (!isGeneric("peptide.sequence")) {
    setGeneric("peptide.sequence", function(object) standardGeneric("peptide.sequence"))
}

if (!isGeneric("peptide.sequence<-")) {
    setGeneric("peptide.sequence<-", function(object, value) standardGeneric("peptide.sequence<-"))
}

if (!isGeneric("protein.accession")) {
    setGeneric("protein.accession", function(object) standardGeneric("protein.accession"))
}

if (!isGeneric("protein.accession<-")) {
    setGeneric("protein.accession<-", function(object, value) standardGeneric("protein.accession<-"))
}

if (!isGeneric("cluster.quality")) {
    setGeneric("cluster.quality", function(object) standardGeneric("cluster.quality"))
}

if (!isGeneric("cluster.quality<-")) {
    setGeneric("cluster.quality<-", function(object, value) standardGeneric("cluster.quality<-"))
}







