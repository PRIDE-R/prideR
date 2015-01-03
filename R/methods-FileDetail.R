#' Constructor for FileDetail
#' 
#' @param assay.accession assay accession
#' @param project.accession project accession
#' @param file.name the name of the file
#' @param file.type the type of the file. e.g. RAW, PEAK, RESULT and etc
#' @param file.source the source of the file. e.g. SUBMITTED or GENERATED
#' @param file.size the size of the file
#' @param download.link URL for downloading the file
#' @export
FileDetail <- function(assay.accession, project.accession, file.name, file.type, file.source, file.size, download.link) {
  new("FileDetail", assay.accession = assay.accession, 
      project.accession = project.accession, 
      file.name = file.name, 
      file.type = file.type, 
      file.source = file.source, 
      file.size = file.size, 
      download.link = download.link)
}

#' print out the details of the FileDetail on screen
#' 
#' @param object a FileDetail object
#' @author Jose A. Dianes
#' @export
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
                   if (validObject(object)) {
                     return(object)   
                   }
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
                   if (validObject(object)) {
                     return(object)   
                   }
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
                   if (validObject(object)) {
                     return(object)   
                   }
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
                   if (validObject(object)) {
                     return(object)   
                   }
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
                   if (validObject(object)) {
                     return(object)   
                   }
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
                   if (validObject(object)) {
                     return(object)   
                   }
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
                   if (validObject(object)) {
                     return(object)   
                   }
                 }
)