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
        clusters = "list", 
        num.results = "numeric",
        page.number = "numeric",
        page.size = "numeric"
    ),
    prototype(
        clusters = list(), 
        num.results = 0,
        page.number = 0,
        page.size = 10
    )
)

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

#' Returns a Cluster instance from a JSON string representation
#'
#' @param json_str The JSON object
#' @param file the name of a file to read the json_str from; this can also be a URL. Only one of json_str or file must be supplied.
#' @param method use the C implementation, or the older slower (and one day to be depricated) R implementation
#' @param unexpected.escape changed handling of unexpected escaped characters. Handling value should be one of "error", "skip", or "keep"; on unexpected characters issue an error, skip the character, or keep the character
#' @return The Cluster instance
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
#' @export
from.json.Cluster <- function(json.object) {
    res <- new("Cluster",
               id = as.character(json.object$id),
               average.precursor.mz = json.object$averagePrecursorMz,
               average.precursor.charge = json.object$averagePrecursorCharge,
               num.spectra = json.object$numberOfSpectra,
               max.ratio = json.object$maxRatio,
               peptide.sequence = json.object$peptideSequence,
               protein.accession = json.object$proteinAccession,
               cluster.quality = json.object$clusterQuality
    )
    
    return (res)
}

#' Returns a PRIDE Cluster cluster
#'
#' @param id The cluster id
#' @return The cluster as object
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
#' @export
get.Cluster <- function(id) {
    from.json.Cluster(fromJSON(file=paste0(pride_cluster_url, "/cluster/", id), method="C"))
}

#' Returns a list of PRIDE Cluster cluster summaries
#'
#' @param page the page number
#' @param size maximum number of results per page
#' @return The list of Cluster objects
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
#' @export
get.list.Cluster <- function(page=1, size=10) {
    json.list <- fromJSON(file=paste0(pride_cluster_url, "/cluster/search", "?page=", page, "&size=", size), method="C")$results   
    cluster.list <- lapply(json.list, function(x) { from.json.Cluster(x)})
    return(cluster.list)
}

#' Returns a series of PRIDE Cluster clusters
#' to satisify a given query. This is actually a 
#' query filtered version of get.list.Cluster
#'
#' @param q The query terms
#' @param page the page number
#' @param size maximum number of results per page
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
#' @export
search.list.Cluster <- function(q="", page=1,size=10) {
    json.list <- fromJSON(file=paste0(pride_cluster_url, "/cluster/search", "?page=", page, "&size=", size,"&q=", q), method="C")$results
    cluster.list <- lapply(json.list, function(x) { from.json.Cluster(x)})
    return(cluster.list)
}



