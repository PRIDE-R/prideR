pride_cluster_url <- "http://wwwdev.ebi.ac.uk/pride/ws/cluster"

MISSING_VALUE <- "Not available"

#' ClusterSummary represents a PRIDE Cluster project summary
#'
#' @importFrom rjson fromJSON
#' @export 
#' @exportClass ClusterSummary
setClass(
    "ClusterSummary", 
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
          signature = "ClusterSummary",
          definition = function(object) {
              cat("An object of class ", class(object), "\n", sep="")
              cat(" with ", object@num.spectra, " spectra representing the peptide with sequence ", object@peptide.sequence, "\n", sep="")
              cat("    Id: ", object@id, "\n", sep="")
              invisible(NULL)
          }
)

if (!isGeneric("id")) {
    setGeneric("id", function(object) standardGeneric("id"))
}
#' Returns a cluster id
#' 
#' @param object a ClusterSummary
#' @return the id
#' @author Jose A. Dianes
#' @export
setMethod("id", "ClusterSummary", function(object) object@id)

if (!isGeneric("id<-")) {
    setGeneric("id<-", function(object, value) standardGeneric("id<-"))
}
#' Replaces a cluster id
#' 
#' @param object a ClusterSummary
#' @param value the id
#' @author Jose A. Dianes
#' @export
setMethod("id<-", "ClusterSummary",
          function(object, value) {
              object@id <- value
              if (validObject(object))
                  return(object)
          }
)




#' Returns a data frame from ClusterSummary inputs
#'
#' @param x The cluster summaries
#' @param row.names optional row names
#' @param optional optional
#' @return The cluster summaries as a data frame
#' @author Jose A. Dianes
#' @details TODO
#' @export
as.data.frame.ClusterSummary <-
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

format.ClusterSummary <- function(x, ...) paste0(x@id, ", ", x@peptide.sequence)

#' Returns a ClusterSummary instance from a JSON string representation
#'
#' @param json_str The JSON object
#' @param file the name of a file to read the json_str from; this can also be a URL. Only one of json_str or file must be supplied.
#' @param method use the C implementation, or the older slower (and one day to be depricated) R implementation
#' @param unexpected.escape changed handling of unexpected escaped characters. Handling value should be one of "error", "skip", or "keep"; on unexpected characters issue an error, skip the character, or keep the character
#' @return The ClusterSummary instance
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
#' @export
from.json.ClusterSummary <- function(json.object) {
    res <- new("ClusterSummary",
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

#' Returns a PRIDE Cluster cluster summary
#'
#' @param id The cluster id
#' @return The cluster as object
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
#' @export
get.ClusterSummary <- function(id) {
    from.json.ClusterSummary(fromJSON(file=paste0(pride_cluster_url, "/clusterSummary/", id), method="C"))
}

#' Returns a list of PRIDE Cluster cluster summaries
#'
#' @param page the page number
#' @param size maximum number of results per page
#' @return The list of ClusterSummary objects
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
#' @export
get.list.ClusterSummary <- function(page=1, size=10) {
    json.list <- fromJSON(file=paste0(pride_cluster_url, "/clusterSummary/search", "?page=", page, "&size=", size), method="C")$results   
    cluster.list <- lapply(json.list, function(x) { from.json.ClusterSummary(x)})
    return(cluster.list)
}

#' Returns a series of PRIDE Cluster clusters
#' to satisify a given query. This is actually a 
#' query filtered version of get.list.ClusterSummary
#'
#' @param q The query terms
#' @param page the page number
#' @param size maximum number of results per page
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
#' @export
search.list.ClusterSummary <- function(q="", page=1,size=10) {
    json.list <- fromJSON(file=paste0(pride_cluster_url, "/clusterSummary/search", "?page=", page, "&size=", size,"&q=", q), method="C")$results
    cluster.list <- lapply(json.list, function(x) { from.json.ClusterSummary(x)})
    return(cluster.list)
}



