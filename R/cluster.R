pride_cluster_url <- "http://wwwdev.ebi.ac.uk/pride/ws/cluster"

MISSING_VALUE <- "Not available"

#' ClusterSearchResults represents a PRIDE Cluster project summary result set
#'
#' @importFrom rjson fromJSON
#' @export 
#' @exportClass ClusterSearchResults
setClass(
    "ClusterSearchResults", 
    representation(
        query = "character",
        precursor = "numeric",
        peaks = "character",
        results = "list", 
        total.results = "numeric",
        page.number = "numeric",
        page.size = "numeric"
    ),
    prototype(
        query = "",
        precursor = 0.0,
        peaks = "",
        results = list(), 
        total.results = 0,
        page.number = 0,
        page.size = 10
    )
)

setMethod("show",
          signature = "ClusterSearchResults",
          definition = function(object) {
            cat("An ", class(object), sep="")
            cat(" representing a search for Clusters with \n", sep="")
            cat("    Query: ", object@query, "\n", sep="")
            cat("    Total results: ", object@total.results, "\n", sep="")
            cat("    Page number: ", object@page.number, " \n", sep="")
            cat("    Page size: ", object@page.size, " clusters in page\n", sep="")
            invisible(NULL)
          }
)

#' Returns a cluster search results list
#' 
#' @param object a ClusterSearchResults results
#' @return the results
#' @author Jose A. Dianes
#' @export
setMethod("results", "ClusterSearchResults", function(object) object@results)

#' Returns the total number of clusters for a given search results
#' 
#' @param object a ClusterSearchResults total results
#' @return the total number of results
#' @author Jose A. Dianes
#' @export
setMethod("total.results", "ClusterSearchResults", function(object) object@total.results)

#' Returns the page number for a given search results
#' 
#' @param object a ClusterSearchResults page number
#' @return the page number 
#' @author Jose A. Dianes
#' @export
setMethod("page.number", "ClusterSearchResults", function(object) object@page.number)

#' Returns the page size for a given search results
#' 
#' @param object a ClusterSearchResults page size
#' @return the page size 
#' @author Jose A. Dianes
#' @export
setMethod("page.size", "ClusterSearchResults", function(object) object@page.size)



#' Cluster represents a PRIDE Cluster project summary
#'
#' @importFrom rjson fromJSON
#' @export 
#' @exportClass Cluster
setClass(
    "Cluster", 
    representation(
        id = "character", 
        average.precursor.mz = "numeric",
        average.precursor.charge = "numeric",
        num.spectra = "numeric",
        max.ratio = "numeric",
        peptide.sequence = "character",
        protein.accession = "character",
        cluster.quality = "character"
    ),
    prototype(
        id = MISSING_VALUE, 
        average.precursor.mz = 0.0,
        average.precursor.charge = 0.0,
        num.spectra = 0,
        max.ratio = 0.0,
        peptide.sequence = MISSING_VALUE,
        protein.accession = MISSING_VALUE,
        cluster.quality = MISSING_VALUE
    )
)

setMethod("show",
          signature = "Cluster",
          definition = function(object) {
              cat("An ", class(object), sep="")
              cat(" representing the peptide ", object@peptide.sequence, "\n", sep="")
              cat("    Cluster Quality: ", object@cluster.quality, "\n", sep="")
              cat("    Number of Spectra: ", object@num.spectra, " spectra\n", sep="")
              cat("    Max. Ratio: ", object@max.ratio, "\n", sep="")
              cat("    Precursor MZ: ", object@average.precursor.mz, "\n", sep="")
              cat("    Precursor Charge: ", object@average.precursor.charge, "\n", sep="")
              cat("    Peptide Sequence: ", object@protein.accession, "\n", sep="")
              cat("    Id: ", object@id, "\n", sep="")
              invisible(NULL)
          }
)

#' Returns a cluster id
#' 
#' @param object a Cluster
#' @return the id
#' @author Jose A. Dianes
#' @export
setMethod("id", "Cluster", function(object) object@id)

#' Replaces a cluster id
#' 
#' @param object a Cluster
#' @param value the id
#' @author Jose A. Dianes
#' @export
setMethod("id<-", "Cluster",
          function(object, value) {
              object@id <- value
              if (validObject(object))
                  return(object)
          }
)

#' Returns a cluster average.precursor.mz
#' 
#' @param object a Cluster
#' @return the average.precursor.mz
#' @author Jose A. Dianes
#' @export
setMethod("average.precursor.mz", "Cluster", function(object) object@average.precursor.mz)

#' Replaces a cluster average.precursor.mz
#' 
#' @param object a Cluster
#' @param value the average.precursor.mz
#' @author Jose A. Dianes
#' @export
setMethod("average.precursor.mz<-", "Cluster",
          function(object, value) {
              object@average.precursor.mz <- value
              if (validObject(object))
                  return(object)
          }
)

#' Returns a cluster average.precursor.charge
#' 
#' @param object a Cluster
#' @return the average.precursor.charge
#' @author Jose A. Dianes
#' @export
setMethod("average.precursor.charge", "Cluster", function(object) object@average.precursor.charge)

#' Replaces a cluster average.precursor.charge
#' 
#' @param object a Cluster
#' @param value the average.precursor.charge
#' @author Jose A. Dianes
#' @export
setMethod("average.precursor.charge<-", "Cluster",
          function(object, value) {
              object@average.precursor.charge <- value
              if (validObject(object))
                  return(object)
          }
)

#' Returns a cluster num.spectra
#' 
#' @param object a Cluster
#' @return the num.spectra
#' @author Jose A. Dianes
#' @export
setMethod("num.spectra", "Cluster", function(object) object@num.spectra)

#' Replaces a cluster num.spectra
#' 
#' @param object a Cluster
#' @param value the num.spectra
#' @author Jose A. Dianes
#' @export
setMethod("num.spectra<-", "Cluster",
          function(object, value) {
              object@num.spectra <- value
              if (validObject(object))
                  return(object)
          }
)

#' Returns a cluster max.ratio
#' 
#' @param object a Cluster
#' @return the max.ratio
#' @author Jose A. Dianes
#' @export
setMethod("max.ratio", "Cluster", function(object) object@max.ratio)

#' Replaces a cluster max.ratio
#' 
#' @param object a Cluster
#' @param value the max.ratio
#' @author Jose A. Dianes
#' @export
setMethod("max.ratio<-", "Cluster",
          function(object, value) {
              object@max.ratio <- value
              if (validObject(object))
                  return(object)
          }
)

#' Returns a cluster peptide.sequence
#' 
#' @param object a Cluster
#' @return the peptide.sequence
#' @author Jose A. Dianes
#' @export
setMethod("peptide.sequence", "Cluster", function(object) object@peptide.sequence)

#' Replaces a cluster peptide.sequence
#' 
#' @param object a Cluster
#' @param value the peptide.sequence
#' @author Jose A. Dianes
#' @export
setMethod("peptide.sequence<-", "Cluster",
          function(object, value) {
              object@peptide.sequence <- value
              if (validObject(object))
                  return(object)
          }
)

#' Returns a cluster protein.accession
#' 
#' @param object a Cluster
#' @return the protein.accession
#' @author Jose A. Dianes
#' @export
setMethod("protein.accession", "Cluster", function(object) object@protein.accession)

#' Replaces a cluster protein.accession
#' 
#' @param object a Cluster
#' @param value the protein.accession
#' @author Jose A. Dianes
#' @export
setMethod("protein.accession<-", "Cluster",
          function(object, value) {
              object@protein.accession <- value
              if (validObject(object))
                  return(object)
          }
)

#' Returns a cluster cluster.quality
#' 
#' @param object a Cluster
#' @return the cluster.quality
#' @author Jose A. Dianes
#' @export
setMethod("cluster.quality", "Cluster", function(object) object@cluster.quality)

#' Replaces a cluster cluster.quality
#' 
#' @param object a Cluster
#' @param value the cluster.quality
#' @author Jose A. Dianes
#' @export
setMethod("cluster.quality<-", "Cluster",
          function(object, value) {
              object@cluster.quality <- value
              if (validObject(object))
                  return(object)
          }
)

#' Returns a data frame from Cluster inputs
#'
#' @param x The cluster summaries
#' @param row.names optional row names
#' @param optional optional
#' @return The cluster summaries as a data frame
#' @author Jose A. Dianes
#' @details TODO
#' @export
as.data.frame.Cluster <-
    function(x, row.names=NULL, optional=FALSE, ...)
    {
        # set row names if provided
        if (is.null(row.names))
            row.names <- x@id
        # create the data frame just with the id column
        value <- list(x@id)
        attr(value, "row.names") <- row.names
        class(value) <- "data.frame"
        names(value) <- c("accession")
        # add the rest of the columns
        value$average.precursor.mz <- x@average.precursor.mz
        value$average.precursor.charge <- x@average.precursor.charge
        value$num.spectra <- x@num.spectra
        value$max.ratio <- x@max.ratio
        value$peptide.sequence <- x@peptide.sequence
        value$protein.accession <- x@protein.accession
        value$cluster.quality <- x@cluster.quality
        
        return(value)
    }

format.Cluster <- function(x, ...) paste0(x@id, ", ", x@peptide.sequence)
