pride_archive_url <- "http://www.ebi.ac.uk/pride/ws/archive"
pride_archive_url_dev <- "http://wwwdev.ebi.ac.uk/pride/ws/archive"

MISSING_VALUE <- "Not available"

#' FileDetail represents a PRIDE Archive file
#'
#' @importFrom rjson fromJSON
#' @export 
#' @exportClass FileDetail
setClass(
    "FileDetail", 
    slots = c(
              assay.accession = "character", 
              project.accession = "character",
              file.name = "character",
              file.type = "character",
              file.source = "character",
              file.size = "numeric",
              download.link = "character"
            ),
    prototype = list(
                    assay.accession = MISSING_VALUE, 
                    project.accession = MISSING_VALUE,
                    file.name = MISSING_VALUE,
                    file.type = MISSING_VALUE,
                    file.source = MISSING_VALUE,
                    file.size = 0,
                    download.link = MISSING_VALUE
                )
)

setMethod("show",
          signature = "FileDetail",
          definition = function(object) {
              cat("An object of class ", class(object), "\n", sep="")
              cat("    Assay accession: ", object@assay.accession, "\n", sep="")
              cat("    Project accession: ", object@project.accession, "\n", sep="")
              cat("    File name: ", object@file.name, "\n", sep="")
              cat("    File type: ", object@file.type, "\n", sep="")
              cat("    File source: ", object@file.source, "\n", sep="")
              cat("    File size: ", object@file.size, "\n", sep="")
              cat("    Download link: ", object@download.link, "\n", sep="")
              invisible(NULL)
          }
)

if (!isGeneric("assay.accession")) {
    setGeneric("assay.accession", function(object) standardGeneric("assay.accession"))
}
#' Returns a file assay accession
#' 
#' @param object a FileDetail
#' @return the assay accession
#' @author Jose A. Dianes
#' @export
setMethod("assay.accession", "FileDetail", function(object) object@assay.accession)

if (!isGeneric("assay.accession<-")) {
    setGeneric("assay.accession<-", function(object, value) standardGeneric("assay.accession<-"))
}
#' Replaces a file assay accession
#' 
#' @param object a FileDetail
#' @param value the assay accession
#' @author Jose A. Dianes
#' @export
setMethod("assay.accession<-", "FileDetail",
          function(object, value) {
              object@assay.accession <- value
              if (validObject(object))
                  return(object)
          }
)

if (!isGeneric("project.accession")) {
    setGeneric("project.accession", function(object) standardGeneric("project.accession"))
}
#' Returns a file project.assay
#' 
#' @param object a FileDetail
#' @return the project accession
#' @author Jose A. Dianes
#' @export
setMethod("project.accession", "FileDetail", function(object) object@project.accession)

if (!isGeneric("project.accession<-")) {
    setGeneric("project.accession<-", function(object, value) standardGeneric("project.accession<-"))
}
#' Replaces a file project accession
#' 
#' @param object a FileDetail
#' @param value the project.accession
#' @author Jose A. Dianes
#' @export
setMethod("project.accession<-", "FileDetail",
          function(object, value) {
              object@project.accession <- value
              if (validObject(object))
                  return(object)
          }
)

if (!isGeneric("file.name")) {
    setGeneric("file.name", function(object) standardGeneric("file.name"))
}
#' Returns a file name
#' 
#' @param object a FileDetail
#' @return the file.name
#' @author Jose A. Dianes
#' @export
setMethod("file.name", "FileDetail", function(object) object@file.name)

if (!isGeneric("file.name<-")) {
    setGeneric("file.name<-", function(object, value) standardGeneric("file.name<-"))
}
#' Replaces a file name
#' 
#' @param object a FileDetail
#' @param value the file.name
#' @author Jose A. Dianes
#' @export
setMethod("file.name<-", "FileDetail",
          function(object, value) {
              object@file.name <- value
              if (validObject(object))
                  return(object)
          }
)

if (!isGeneric("file.type")) {
    setGeneric("file.type", function(object) standardGeneric("file.type"))
}
#' Returns a file type
#' 
#' @param object a FileDetail
#' @return the file.type
#' @author Jose A. Dianes
#' @export
setMethod("file.type", "FileDetail", function(object) object@file.type)

if (!isGeneric("file.type<-")) {
    setGeneric("file.type<-", function(object, value) standardGeneric("file.type<-"))
}
#' Replaces a file type
#' 
#' @param object a FileDetail
#' @param value the file.type
#' @author Jose A. Dianes
#' @export
setMethod("file.type<-", "FileDetail",
          function(object, value) {
              object@file.type <- value
              if (validObject(object))
                  return(object)
          }
)

if (!isGeneric("file.source")) {
    setGeneric("file.source", function(object) standardGeneric("file.source"))
}
#' Returns a file source
#' 
#' @param object a FileDetail
#' @return the file.source
#' @author Jose A. Dianes
#' @export
setMethod("file.source", "FileDetail", function(object) object@file.source)

if (!isGeneric("file.source<-")) {
    setGeneric("file.source<-", function(object, value) standardGeneric("file.source<-"))
}
#' Replaces a file source
#' 
#' @param object a FileDetail
#' @param value the file.source
#' @author Jose A. Dianes
#' @export
setMethod("file.source<-", "FileDetail",
          function(object, value) {
              object@file.source <- value
              if (validObject(object))
                  return(object)
          }
)

if (!isGeneric("file.size")) {
    setGeneric("file.size", function(object) standardGeneric("file.size"))
}
#' Returns a file size
#' 
#' @param object a FileDetail
#' @return the file.size
#' @author Jose A. Dianes
#' @export
setMethod("file.size", "FileDetail", function(object) object@file.size)

if (!isGeneric("file.size<-")) {
    setGeneric("file.size<-", function(object, value) standardGeneric("file.size<-"))
}
#' Replaces a file size
#' 
#' @param object a FileDetail
#' @param value the file.size
#' @author Jose A. Dianes
#' @export
setMethod("file.size<-", "FileDetail",
          function(object, value) {
              object@file.size <- value
              if (validObject(object))
                  return(object)
          }
)

if (!isGeneric("download.link")) {
    setGeneric("download.link", function(object) standardGeneric("download.link"))
}
#' Returns a file download link
#' 
#' @param object a FileDetail
#' @return the download.link
#' @author Jose A. Dianes
#' @export
setMethod("download.link", "FileDetail", function(object) object@download.link)

if (!isGeneric("download.link<-")) {
    setGeneric("download.link<-", function(object, value) standardGeneric("download.link<-"))
}
#' Replaces a file download.link
#' 
#' @param object a FileDetail
#' @param value the download.link
#' @author Jose A. Dianes
#' @export
setMethod("download.link<-", "FileDetail",
          function(object, value) {
              object@download.link <- value
              if (validObject(object))
                  return(object)
          }
)

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
