#' ProjectSummary represents a PRIDE Archive project dataset
#'
#' @importFrom rjson fromJSON
#' @export 
#' @exportClass ProjectSummary
setClass(
    "ProjectSummary", 
    representation(
        accession = "character", 
        title = "character",
        projectDescription = "character",
        publicationDate = "POSIXct",
        numAssays = "numeric",
        species = "vector",
        tissues = "vector",
        ptmNames = "vector",
        instrumentNames = "vector",
        projectTags = "vector",
        submissionType = "character"
    ),
    prototype(
        accession = "Not available", 
        title = "Not available",
        projectDescription = "Not available",
        publicationDate = Sys.time(),
        numAssays = 0,
        species = c("Not available"),
        tissues = c("Not available"),
        ptmNames = c("Not available"),
        instrumentNames = c("Not available"),
        projectTags = c("Not available"),
        submissionType = "Not available"
    )
)

#' Returns a ProjectSummary instance from a JSON string representation
#'
#' @param json_str The JSON object
#' @param x The project summaries
#' @param row.names optional row names
#' @return The project summaries as a data frame
#' @author Jose A. Dianes
#' @details TODO
#' @export
as.data.frame.ProjectSummary <-
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
    value$title <- x@title
    value$project.description <- x@projectDescription
    value$publication.date <- x@publicationDate
    value$num.assays <- x@numAssays
    value$species <- paste(x@species, collapse=" || ")
    value$tissues <- paste(x@tissues, collapse=" || ")
    value$ptm.names <- paste(x@ptmNames, collapse=" || ")
    value$instrument.names <- paste(x@instrumentNames, collapse=" || ")
    value$project.tags <- paste(x@projectTags, collapse=" || ")
    value$submissionType <- x@submissionType
    
    return(value)
}

format.ProjectSummary <- function(x, ...) paste0(x@accession, ", ", x@title)

#' Returns a ProjectSummary instance from a JSON string representation
#'
#' @param json_str The JSON object
#' @param json_str the name of a file to read the json_str from; this can also be a URL. Only one of json_str or file must be supplied.
#' @param method use the C implementation, or the older slower (and one day to be depricated) R implementation
#' @param unexpected.escape changed handling of unexpected escaped characters. Handling value should be one of "error", "skip", or "keep"; on unexpected characters issue an error, skip the character, or keep the character
#' @return The ProjectSummary instance
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
fromJSON.ProjectSummary <- function(json_str, file, method = "C") {
    json.list <- fromJSON(json_str, file, method)
    
    res <- new("ProjectSummary",
               accession = json.list$accession,
               title = json.list$title,
               projectDescription = json.list$projectDescription,
               publicationDate = as.POSIXct(json.list$publicationDate),
               numAssays = json.list$numAssays,
               species = json.list$species,
               tissues = json.list$tissues,
               ptmNames = json.list$ptmNames,
               instrumentNames = json.list$instrumentNames,
               projectTags = json.list$projectTags,
               submissionType = json.list$submissionType
               )
    
    return (res)
}

#' Returns a PRIDE Archive project
#'
#' @param accession The project accession
#' @return The project in a data frame
#' @author Jose A. Dianes
#' @details TODO
#' @export
#' @importFrom rjson fromJSON
project <- function(accession) {
    fromJSON.ProjectSummary(file=paste0(pride_archive_url, "/project/", accession), method="C")
}
