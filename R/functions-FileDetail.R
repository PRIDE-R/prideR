pride_archive_url <- "http://www.ebi.ac.uk/pride/ws/archive"
pride_archive_url_dev <- "http://wwwdev.ebi.ac.uk/pride/ws/archive"

#' Returns a data frame from FileDetail inputs
#'
#' @param x The file details
#' @param row.names optional row names
#' @return The file details as a data frame
#' @author Jose A. Dianes
#' @details TODO
#' @export
as.data.frame.FileDetail <-
    function(x, row.names=NULL, optional=FALSE, ...)
    {
        # set row names if provided
        if (is.null(row.names))
            row.names <- x@file.name
        # create the data frame just with the accession column
        value <- list(x@file.name)
        attr(value, "row.names") <- row.names
        class(value) <- "data.frame"
        names(value) <- c("file.name")
        # add the rest of the columns
        value$assay.accession <- x@assay.accession
        value$project.accession <- x@project.accession
        value$file.type <- x@file.type
        value$file.source <- x@file.source
        value$file.size <- x@file.size
        value$download.link <- x@download.link
        return(value)
    }

format.FileDetail <- function(x, ...) paste0(x@file.name, ", ", x@project.accession)

#' Returns a FileDetail instance from a JSON string representation
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
from.json.FileDetail <- function(json.object) {
    res <- new("FileDetail",
               assay.accession = json.object$assayAccession,
               project.accession = json.object$projectAccession,
               file.name = json.object$fileName,
               file.type = json.object$fileType,
               file.source = json.object$fileSource,
               file.size = json.object$fileSize,
               download.link = json.object$downloadLink
    )
    
    return (res)
}

#' Returns a list of PRIDE Archive files associated with a project
#'
#' @param project.accession the project accession
#' @return The list of FileDetail objects
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
#' @export
get.list.project.FileDetail <- function(project.accession) {
    json.list <- fromJSON(file=paste0(pride_archive_url, "/file/list/project/", project.accession), method="C")
    file.list <- lapply(json.list[[1]], function(x) { from.json.FileDetail(x)})
    return(file.list)
}

#' Returns the number of files associated with a project
#'
#' @param project.accession the project accession
#' @return The count of files
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
#' @export
count.project.FileDetail <- function(project.accession) {
    fileCount <- fromJSON(file=paste0(pride_archive_url, "/file/count/project/", project.accession), method="C")
    fileCount                          
}

#' Returns a list of PRIDE Archive files associated with an assay
#'
#' @param assay.accession the assay accession
#' @return The list of FileDetail objects
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
#' @export
get.list.assay.FileDetail <- function(assay.accession) {
    json.list <- fromJSON(file=paste0(pride_archive_url, "/file/list/assay/", assay.accession), method="C")
    file.list <- lapply(json.list[[1]], function(x) { from.json.FileDetail(x)})
    return(file.list)
}

#' Returns the number of files associated with an assay
#'
#' @param assay.accession the assay accession
#' @return The count of files
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
#' @export
count.assay.FileDetail <- function(assay.project) {
    fileCount <- fromJSON(file=paste0(pride_archive_url, "/file/count/assay/", assay.accession), method="C")
    fileCount                          
}
