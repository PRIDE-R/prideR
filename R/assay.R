pride_archive_url <- "http://www.ebi.ac.uk/pride/ws/archive"
pride_archive_url_dev <- "http://wwwdev.ebi.ac.uk/pride/ws/archive"

MISSING_VALUE <- "Not available"

#' AssayDetail represents a PRIDE Archive assay
#'
#' @importFrom rjson fromJSON
#' @export 
#' @exportClass AssayDetail
setClass(
    "AssayDetail", 
    representation(
        assay.accession = "character", 
        project.accession = "character",
        protein.count = "numeric",
        peptide.count = "numeric",
        unique.peptide.count = "numeric",
        identified.spectrum.count = "numeric",
        total.spectrum.count = "numeric"
    ),
    prototype(
        assay.accession = MISSING_VALUE, 
        project.accession = MISSING_VALUE,
        protein.count = 0,
        peptide.count = 0,
        unique.peptide.count = 0,
        identified.spectrum.count = 0,
        total.spectrum.count = 0
    )
)

setMethod("show",
          signature = "AssayDetail",
          definition = function(object) {
              cat("An object of class ", class(object), "\n", sep="")
              cat("    Assay accession: ", object@assay.accession, "\n", sep="")
              cat("    Project accession: ", object@project.accession, "\n", sep="")
              cat("    Protein #: ", object@protein.count, "\n", sep="")
              cat("    Peptide #: ", object@peptide.count, "\n", sep="")
              cat("    Unique Peptide # ", object@unique.peptide.count, "\n", sep="")
              cat("    Identified Spectrum #: ", object@identified.spectrum.count, "\n", sep="")
              cat("    Total Spectrum #: ", object@total.spectrum.count, "\n", sep="")
              invisible(NULL)
          }
)

if (!isGeneric("assay.accession")) {
    setGeneric("assay.accession", function(object) standardGeneric("assay.accession"))
}
#' Returns an assay accession
#' 
#' @param object a AssayDetail
#' @return the accession
#' @author Jose A. Dianes
#' @export
setMethod("assay.accession", "AssayDetail", function(object) object@assay.accession)

if (!isGeneric("assay.accession<-")) {
    setGeneric("assay.accession<-", function(object, value) standardGeneric("assay.accession<-"))
}
#' Replaces an assay accession
#' 
#' @param object a AssayDetail
#' @param value the accession
#' @author Jose A. Dianes
#' @export
setMethod("assay.accession<-", "AssayDetail",
          function(object, value) {
              object@assay.accession <- value
              if (validObject(object))
                  return(object)
          }
)

if (!isGeneric("project.accession")) {
    setGeneric("project.accession", function(object) standardGeneric("project.accession"))
}
#' Returns an assay project.assay
#' 
#' @param object a AssayDetail
#' @return the project accession
#' @author Jose A. Dianes
#' @export
setMethod("project.accession", "AssayDetail", function(object) object@project.accession)

if (!isGeneric("project.accession<-")) {
    setGeneric("project.accession<-", function(object, value) standardGeneric("project.accession<-"))
}
#' Replaces an assay project accession
#' 
#' @param object a AssayDetail
#' @param value the project.accession
#' @author Jose A. Dianes
#' @export
setMethod("project.accession<-", "AssayDetail",
          function(object, value) {
              object@project.accession <- value
              if (validObject(object))
                  return(object)
          }
)



#' Returns a data frame from AssayDetail inputs
#'
#' @param x The assay details
#' @param row.names optional row names
#' @return The assay details as a data frame
#' @author Jose A. Dianes
#' @details TODO
#' @export
as.data.frame.AssayDetail <-
    function(x, row.names=NULL, optional=FALSE, ...)
    {
        # set row names if provided
        if (is.null(row.names))
            row.names <- x@assay.accession
        # create the data frame just with the accession column
        value <- list(x@assay.accession)
        attr(value, "row.names") <- row.names
        class(value) <- "data.frame"
        names(value) <- c("assay.accession")
        # add the rest of the columns
        value$project.accession <- x@project.accession
        value$protein.count <- x@protein.count
        value$peptide.count <- x@peptide.count
        value$unique.peptide.count <- x@unique.peptide.count
        value$identified.spectrum.count <- x@identified.spectrum.count
        value$total.spectrum.count <- x@total.spectrum.count
        return(value)
    }

format.AssayDetail <- function(x, ...) paste0(x@assay.accession, ", ", x@project.accession)

#' Returns a AssayDetail instance from a JSON string representation
#'
#' @param json_str The JSON object
#' @param file the name of a file to read the json_str from; this can also be a URL. Only one of json_str or file must be supplied.
#' @param method use the C implementation, or the older slower (and one day to be depricated) R implementation
#' @param unexpected.escape changed handling of unexpected escaped characters. Handling value should be one of "error", "skip", or "keep"; on unexpected characters issue an error, skip the character, or keep the character
#' @return The AssayDetail instance
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
#' @export
from.json.AssayDetail <- function(json.object) {
    res <- new("AssayDetail",
               assay.accession = json.object$assayAccession,
               project.accession = json.object$projectAccession,
               protein.count = json.object$proteinCount,
               peptide.count = json.object$peptideCount,
               unique.peptide.count = json.object$uniquePeptideCount,
               identified.spectrum.count = json.object$identifiedSpectrumCount,
               total.spectrum.count = json.object$totalSpectrumCount
    )
    
    return (res)
}

#' Returns a PRIDE Archive assay
#'
#' @param accession The assay accession
#' @return The assay as object of AssayDetail
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
#' @export
get.AssayDetail <- function(assay.accession) {
    from.json.AssayDetail(fromJSON(file=paste0(pride_archive_url, "/assay/", assay.accession), method="C"))
}

#' Returns a list of PRIDE Archive assays associated to a project
#'
#' @param project.accession the project accession
#' @return The list of AssayDetail objects
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
#' @export
get.list.AssayDetail <- function(project.accession) {
    json.list <- fromJSON(file=paste0(pride_archive_url, "/assay/list/project/", project.accession), method="C")
    assay.list <- lapply(json.list[[1]], function(x) { from.json.AssayDetail(x)})
    return(assay.list)
}

#' Returns the number of assays associated with a project
#'
#' @return The count of assays
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
#' @export
count.AssayDetail <- function(project.accession) {
    assayCount <- fromJSON(file=paste0(pride_archive_url, "/assay/count/project/", project.accession), method="C")
    assayCount                          
}

