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

#' Returns a PsmDetail instance from a JSON string representation
#'
#' @param json_str The JSON object
#' @param file the name of a file to read the json_str from; this can also be a URL. Only one of json_str or file must be supplied.
#' @param method use the C implementation, or the older slower (and one day to be depricated) R implementation
#' @param unexpected.escape changed handling of unexpected escaped characters. Handling value should be one of "error", "skip", or "keep"; on unexpected characters issue an error, skip the character, or keep the character
#' @return The PsmDetail instance
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
#' @export
fromJSON.PsmDetail <- function(json.str, file, method = "C") {
  json.list <- fromJSON(json.str, file, method)
  
  res <- new("PsmDetail",
             id = json.list$id,
             project.accession = json.list$projectAccession,
             assay.accession = json.list$assayAccession,
             protein.accession = json.list$proteinAccession,
             start.position = json.list$startPosition,
             end.position = json.list$endPosition,
             modifications = json.list$modifications,             
             search.engines = json.list$searchEngines,
             search.engines.scores = json.list$searchEnginesScores,
             retention.time = json.list$retentionTime,
             charge = json.list$charge,
             calculated.mz = json.list$calculatedMZ,
             experimental.mz = json.list$experimentalMZ,
             pre.aa = json.list$preAA,
             post.aa = x@post.aa,
             spectrum.id = json.list$spectrumID,
             reported.id = json.list$reportedID,            
             sequence = json.list$sequence
  )
  
  return (res)
}




