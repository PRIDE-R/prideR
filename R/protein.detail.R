pride_archive_url <- "http://www.ebi.ac.uk/pride/ws/archive"
pride_archive_url_dev <- "http://wwwdev.ebi.ac.uk/pride/ws/archive"


#' ProteinDetail represents a PRIDE Archive protein identification
#'
#' @importFrom rjson fromJSON
#' @export 
#' @exportClass ProteinDetail
setClass(
  "ProteinDetail", 
  representation(
    accession = "character", 
    projectAccession = "character", 
    assayAccession = "character", 
    synonyms = "vector", 
    description = "character", 
    sequence = "character"
  ),
  prototype(
    accession = "Not available", 
    projectAccession = "Not available", 
    assayAccession = "Not available", 
    synonyms = c("Not available"), 
    description = "Not available", 
    sequence = "Not available"
  )
)

#' Returns a data frame from ProteinDetail inputs
#'
#' @param x The protein detail inputs
#' @param row.names optional row names
#' @return The protein identification details as a data frame
#' @author Jose A. Dianes
#' @details TODO
#' @export
as.data.frame.ProteinDetail <-
  function(x, row.names=NULL, optional=FALSE, ...)
  {
    # set row names if provided
    if (is.null(row.names))
      row.names <- x@accession
    # create the data frame just with the accession column
    value <- list(x@accession)
    attr(value, "row.names") <- row.names
    class(value) <- "data.frame"
    names(value) <- c("accession")
    # add the rest of the columns
    value$project.accession <- x@projectAccession
    value$assay.accession <- x@assayAccession
    value$synonyms <- paste(x@synonyms, collapse=" || ")
    value$description <- x@description
    value$sequence <- x@sequence
    
    return(value)
  }

#' Returns a ProteinDetail instance from a JSON string representation
#'
#' @param json_str The JSON object
#' @param file the name of a file to read the json_str from; this can also be a URL. Only one of json_str or file must be supplied.
#' @param method use the C implementation, or the older slower (and one day to be depricated) R implementation
#' @param unexpected.escape changed handling of unexpected escaped characters. Handling value should be one of "error", "skip", or "keep"; on unexpected characters issue an error, skip the character, or keep the character
#' @return The ProteinDetail instance
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
#' @export
from.json.ProteinDetail <- function(json.object) {
  
  res <- new("ProteinDetail",
             accession = json.object$accession,
             projectAccession = json.object$projectAccession,
             assayAccession = json.object$assayAccession,
             synonyms = ifelse(json.object$synonyms==NULL, c("Not available"), json.object$synonyms),
             description = as.character(json.object$description),
             sequence =  as.character(json.object$sequence)
  )
  
  return (res)
}

#' Returns a list of PRIDE Archive PSMs associated with a given project
#'
#' @param accession The project accession
#' @return The list of ProteinDetail objects
#' @author Jose A. Dianes
#' @details TODO
#' @export
get.list.ProteinDetail <- function(project.accession, count=1) {
  json.list <- fromJSON(file=paste0(pride_archive_url_dev, "/protein/list/project/", project.accession, "?show=", count), method="C")
  details.list <- lapply(json.list, function(x) { from.json.ProteinDetail(x[[1]])})
  
  return(details.list)
}

format.ProteinDetail <- function(x, ...) paste0(x@accession, ", ", x@assayAccession)
