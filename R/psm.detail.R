pride_archive_url <- "http://www.ebi.ac.uk/pride/ws/archive"
pride_archive_url_dev <- "http://wwwdev.ebi.ac.uk/pride/ws/archive"
MISSING_VALUE <- "Not available"

#' PsmDetail represents a PRIDE Archive peptide-spectrum matching
#'
#' @importFrom rjson fromJSON
#' @export 
#' @exportClass PsmDetail
setClass(
  "PsmDetail", 
  representation(
    id = "character",
    project.accession = "character", 
    assay.accession = "character", 
    protein.accession = "character",
    start.position = "numeric", 
    end.position = "numeric", 
    modifications = "vector", 
    search.engines = "vector", 
    search.engines.scores = "vector", 
    retention.time = "numeric", 
    charge = "numeric",
    calculated.mz = "numeric", 
    experimental.mz = "numeric", 
    pre.aa = "character",
    post.aa = "character",
    spectrum.id = "character",
    reported.id = "character",
    sequence = "character"
  ),
  prototype(
    id = "Not Available",
    project.accession = "Not Available", 
    assay.accession = "Not Available", 
    protein.accession = "Not Available",
    start.position = 0, 
    end.position = 0, 
    modifications = c("Not Available"), 
    search.engines = c("Not Available"), 
    search.engines.scores = c("Not Available"), 
    retention.time = 0.0, 
    charge = 0,
    calculated.mz = 0.0, 
    experimental.mz = 0.0, 
    pre.aa = "Not Available",
    post.aa = "Not Available",
    spectrum.id = "Not Available",
    reported.id = "Not Available",
    sequence = "Not Available"
  )
)

#' Returns a data frame from PsmDetail inputs
#'
#' @param x The PSM detail inputs
#' @param row.names optional row names
#' @return The PSM details as a data frame
#' @author Jose A. Dianes
#' @details TODO
#' @export
as.data.frame.PsmDetail <-
  function(x, row.names=NULL, optional=FALSE, ...)
  {
    # set row names if provided
    if (is.null(row.names))
      row.names <- x@id
    # create the data frame just with the id column
    value <- list(x@id)
    attr(value, "row.names") <- row.names
    class(value) <- "data.frame"
    names(value) <- c("id")
    # add the rest of the columns
    value$project.accession <- x@project.accession
    value$assay.accession <- x@assay.accession
    value$protein.accession <- x@protein.accession
    value$start.position <- x@start.position
    value$end.position <- x@end.position
    value$modifications <- paste(x@modifications, collapse=" || ")
    value$search.engines <- paste(x@search.engines, collapse=" || ")
    value$search.engines.scores <- paste(x@search.engines.scores, collapse=" || ")
    value$retention.time <- x@retention.time
    value$charge <- x@charge
    value$calculated.mz <- x@calculated.mz
    value$experimental.mz <- x@experimental.mz
    value$pre.aa <- x@pre.aa
    value$post.aa <- x@post.aa
    value$spectrum.id <- x@spectrum.id
    value$reported.id <- x@reported.id
    value$sequence <- x@sequence
    
    return(value)
  }

format.PsmDetail <- function(x, ...) paste0(x@id, ", ", x@assayAccession)

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
from.json.PsmDetail <- function(json.object) {
    
    res <- new("PsmDetail",
               id = json.object$id,
               project.accession = json.object$projectAccession,
               assay.accession = json.object$assay.accession,
               start.position = json.object$start.position, 
               end.position = json.object$end.position, 
               modifications = ifelse(is.null(json.object$modifications), c(MISSING_VALUE), json.object$modifications),
               search.engines = ifelse(is.null(json.object$searchEngines), c(MISSING_VALUE), json.object$searchEngines),
               search.engines.scores = ifelse(is.null(json.object$searchEnginesScores), c(MISSING_VALUE), json.object$searchEnginesScores),
               retention.time = json.object$retentionTime,
               charge = json.object$charge,
               calculated.mz = json.object$calculatedMz,
               experimental.mz = json.object$experimentalMz,
               pre.aa = json.object$preAA,
               post.aa = json.object$postAA,
               spectrum.id = json.object$spectrumId,
               reported.id = json.object$reportedId,
               sequence = json.object$sequence
    )
    
    return (res)
}

#' Returns a list of PRIDE Archive PSMs associated with a given project
#'
#' @param accession The project accession
#' @param count The maximum PSMs to return from the project (deault is 1)
#' @return The list of PSM objects
#' @author Jose A. Dianes
#' @details TODO
#' @export
get.list.PsmDetail <- function(project.accession, count=100) {
    json.list <- fromJSON(file=paste0(pride_archive_url_dev, "/peptide/list/project/", project.accession, "?show=", count), method="C")
    details.list <- lapply(json.list[[1]], function(x) { from.json.PsmDetail(x)})
    return(details.list)
}

#' Returns the number of PSM for a particual public project
#'
#' @param project.accession The project accession to count PSM from
#' @return The count of PSM
#' @author Jose A. Dianes
#' @details TODO
#' @export
#' @importFrom rjson fromJSON
count.PsmDetail <- function(project.accession) {    
    psm.count <- fromJSON(file=URLencode(paste0(pride_archive_url_dev, "/peptide/count/project/", project.accession)), method="C")
    psm.count                          
}



