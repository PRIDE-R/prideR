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
                ),
    
    validity = function(object) {
      # check assay.accession
      if (!is.character(object@assay.accession) || nchar(object@assay.accession) == 0 || is.na(object@assay.accession))
        return("'assay.accession' must be a single valid string")
      
      # check project.accession
      if (!is.character(object@project.accession) || nchar(object@project.accession) == 0 || is.na(object@project.accession))
        return("'project.accession' must be a single valid string")
      
      # check file.name
      if (!is.character(object@file.name) || nchar(object@file.name) == 0 || is.na(object@file.name))
        return("'file.name' must be a single valid string")
      
      # check file.source
      if (!is.character(object@file.source) || nchar(object@file.source) == 0 || is.na(object@file.source))
        return("'file.source' must be a single valid string")
      
      # check file.size
      if (!is.numeric(object@file.size) || object@file.size < 0 ||is.na(object@file.size))
        return("'file.size' must be a none negative number")
      
      # check download.link
      if (!is.character(object@download.link) || nchar(object@download.link) == 0 || is.na(object@download.link))
        return("'download.link' must be a single valid string")
      
      return(TRUE)
    }
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

#' Returns a file assay accession
#' 
#' @param object a FileDetail
#' @return the assay accession
#' @author Jose A. Dianes
#' @export
setMethod("assay.accession", "FileDetail", function(object) object@assay.accession)

#' Replaces a file assay accession
#' 
#' @param object a FileDetail
#' @param value the assay accession
#' @author Jose A. Dianes
#' @export
setReplaceMethod("assay.accession", "FileDetail",
          function(object, value) {
              object@assay.accession <- value
              validObject(object)
              return(object)
          }
)

#' Returns a file project.assay
#' 
#' @param object a FileDetail
#' @return the project accession
#' @author Jose A. Dianes
#' @export
setMethod("project.accession", "FileDetail", function(object) object@project.accession)

#' Replaces a file project accession
#' 
#' @param object a FileDetail
#' @param value the project.accession
#' @author Jose A. Dianes
#' @export
setReplaceMethod("project.accession", "FileDetail",
          function(object, value) {
              object@project.accession <- value
              if (validObject(object))
                  return(object)
          }
)

#' Returns a file name
#' 
#' @param object a FileDetail
#' @return the file.name
#' @author Jose A. Dianes
#' @export
setMethod("file.name", "FileDetail", function(object) object@file.name)

#' Replaces a file name
#' 
#' @param object a FileDetail
#' @param value the file.name
#' @author Jose A. Dianes
#' @export
setReplaceMethod("file.name", "FileDetail",
          function(object, value) {
              object@file.name <- value
              if (validObject(object))
                  return(object)
          }
)

#' Returns a file type
#' 
#' @param object a FileDetail
#' @return the file.type
#' @author Jose A. Dianes
#' @export
setMethod("file.type", "FileDetail", function(object) object@file.type)

#' Replaces a file type
#' 
#' @param object a FileDetail
#' @param value the file.type
#' @author Jose A. Dianes
#' @export
setReplaceMethod("file.type", "FileDetail",
          function(object, value) {
              object@file.type <- value
              if (validObject(object))
                  return(object)
          }
)

#' Returns a file source
#' 
#' @param object a FileDetail
#' @return the file.source
#' @author Jose A. Dianes
#' @export
setMethod("file.source", "FileDetail", function(object) object@file.source)

#' Replaces a file source
#' 
#' @param object a FileDetail
#' @param value the file.source
#' @author Jose A. Dianes
#' @export
setReplaceMethod("file.source", "FileDetail",
          function(object, value) {
              object@file.source <- value
              if (validObject(object))
                  return(object)
          }
)

#' Returns a file size
#' 
#' @param object a FileDetail
#' @return the file.size
#' @author Jose A. Dianes
#' @export
setMethod("file.size", "FileDetail", function(object) object@file.size)

#' Replaces a file size
#' 
#' @param object a FileDetail
#' @param value the file.size
#' @author Jose A. Dianes
#' @export
setReplaceMethod("file.size", "FileDetail",
          function(object, value) {
              object@file.size <- value
              if (validObject(object))
                  return(object)
          }
)

#' Returns a file download link
#' 
#' @param object a FileDetail
#' @return the download.link
#' @author Jose A. Dianes
#' @export
setMethod("download.link", "FileDetail", function(object) object@download.link)

#' Replaces a file download.link
#' 
#' @param object a FileDetail
#' @param value the download.link
#' @author Jose A. Dianes
#' @export
setReplaceMethod("download.link", "FileDetail",
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
