pride_archive_url <- "http://www.ebi.ac.uk/pride/ws/archive"
pride_archive_url_dev <- "http://wwwdev.ebi.ac.uk/pride/ws/archive"

MISSING_VALUE <- "Not available"

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
        accession = MISSING_VALUE, 
        title = MISSING_VALUE,
        projectDescription = MISSING_VALUE,
        publicationDate = Sys.time(),
        numAssays = 0,
        species = c(MISSING_VALUE),
        tissues = c(MISSING_VALUE),
        ptmNames = c(MISSING_VALUE),
        instrumentNames = c(MISSING_VALUE),
        projectTags = c(MISSING_VALUE),
        submissionType = MISSING_VALUE
    )
)

#' Returns a data frame from ProjectSummary inputs
#'
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
#' @param file the name of a file to read the json_str from; this can also be a URL. Only one of json_str or file must be supplied.
#' @param method use the C implementation, or the older slower (and one day to be depricated) R implementation
#' @param unexpected.escape changed handling of unexpected escaped characters. Handling value should be one of "error", "skip", or "keep"; on unexpected characters issue an error, skip the character, or keep the character
#' @return The ProjectSummary instance
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
#' @export
from.json.ProjectSummary <- function(json.object) {
    res <- new("ProjectSummary",
               accession = json.object$accession,
               title = json.object$title,
               projectDescription = ifelse(is.null(json.object$projectDescription), MISSING_VALUE, json.object$projectDescription),
               publicationDate = as.POSIXct(json.object$publicationDate),
               numAssays = json.object$numAssays,
               species = json.object$species,
               tissues = json.object$tissues,
               ptmNames = json.object$ptmNames,
               instrumentNames = json.object$instrumentNames,
               projectTags = json.object$projectTags,
               submissionType = json.object$submissionType
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
get.ProjectSummary <- function(accession) {
    fromJSON.ProjectSummary(file=paste0(pride_archive_url, "/project/", accession), method="C")
}

#' Returns a list of PRIDE Archive project summaries
#'
#' @param count the maximum number of projects
#' @return The list of ProjectSummary objects
#' @author Jose A. Dianes
#' @details TODO
#' @export
get.list.ProjectSummary <- function(count=1) {
    json.list <- fromJSON(file=paste0(pride_archive_url, "/project/list", "?show=", count), method="C")
    project.list <- lapply(json.list[[1]], function(x) { from.json.ProjectSummary(x)})
    return(project.list)
}

#' Returns a series of PRIDE Archive projects
#' to satisify a given query. This is actually a 
#' query filtered version of project_list
#'
#' @param q The query terms
#' @param count The maximum number of search results
#' @return The search results in a data frame
#' @author Jose A. Dianes
#' @details TODO
#' @export
#' @importFrom rjson fromJSON
search.list.ProjectSummary <- function(q,count) {
    json.list <- fromJSON(file=paste0(pride_archive_url, "/project/list", "?show=", count,"&q=", q), method="C")
    project.list <- lapply(json.list[[1]], function(x) { from.json.ProjectSummary(x)})
    return(project.list)
}

#' Returns the number of public projects in PRIDE Archive
#'
#' @return The count of projects
#' @author Jose A. Dianes
#' @details TODO
#' @export
#' @importFrom rjson fromJSON
count.ProjectSummary <- function() {
    projectCount <- fromJSON(file=paste0(pride_archive_url, "/project/count"), method="C")
    projectCount                          
}

