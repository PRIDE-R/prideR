pride_archive_url <- "http://www.ebi.ac.uk/pride/ws/archive"
pride_archive_url_dev <- "http://wwwdev.ebi.ac.uk/pride/ws/archive"
MISSING_VALUE <- "Not available"

#' ProteinDetail represents a PRIDE Archive protein identification
#'
#' @importFrom rjson fromJSON
#' @export 
#' @exportClass ProteinDetail
setClass(
    "ProteinDetail", 
    representation(
        accession = "character", 
        project.accession = "character", 
        assay.accession = "character", 
        synonyms = "vector", 
        description = "character", 
        sequence = "character"
    ),
    prototype(
        accession = "Not available", 
        project.accession = "Not available", 
        assay.accession = "Not available", 
        synonyms = c("Not available"), 
        description = "Not available", 
        sequence = "Not available"
    )
)

setMethod("show",
          signature = "ProteinDetail",
          definition = function(object) {
              cat("An object of class ", class(object), " with\n", sep="")
              cat("    Accession: ", object@accession, "\n", sep="")
              cat("    Project accession: ", object@project.accession, "\n", sep="")
              cat("    Assay accession: ", object@assay.accession, "\n", sep="")
              cat("    Synonyms: ", object@synonyms, "\n", sep=" ")
              cat("    Description: ", object@description, "\n", sep=" ")
              cat("    Sequence: ", object@sequence, "\n", sep=" ")
              invisible(NULL)
          }
)

if (!isGeneric("accession")) {
    setGeneric("accession", function(object) standardGeneric("accession"))
}
#' Returns a protein accession
#' 
#' @param object a ProjectDetail
#' @return the accession
#' @author Jose A. Dianes
#' @export
setMethod("accession", "ProteinDetail", function(object) object@accession)

if (!isGeneric("accession<-")) {
    setGeneric("accession<-", function(object, value) standardGeneric("accession<-"))
}
#' Replaces a protein accession
#' 
#' @param object a ProteinDetail
#' @param value the accession
#' @author Jose A. Dianes
#' @export
setMethod("accession<-", "ProteinDetail",
          function(object, value) {
              object@accession <- value
              if (validObject(object))
                  return(object)
          }
)

if (!isGeneric("project.accession")) {
    setGeneric("project.accession", function(object) standardGeneric("project.accession"))
}
#' Returns a protein project accession
#' 
#' @param object a ProjectDetail
#' @return the project accession
#' @author Jose A. Dianes
#' @export
setMethod("project.accession", "ProteinDetail", function(object) object@project.accession)

if (!isGeneric("project.accession<-")) {
    setGeneric("project.accession<-", function(object, value) standardGeneric("project.accession<-"))
}
#' Replaces a protein project accession
#' 
#' @param object a ProteinDetail
#' @param value the project accession
#' @author Jose A. Dianes
#' @export
setMethod("project.accession<-", "ProteinDetail",
          function(object, value) {
              object@project.accession <- value
              if (validObject(object))
                  return(object)
          }
)

if (!isGeneric("assay.accession")) {
    setGeneric("assay.accession", function(object) standardGeneric("assay.accession"))
}
#' Returns a protein assay accession
#' 
#' @param object a ProjectDetail
#' @return the assay accession
#' @author Jose A. Dianes
#' @export
setMethod("assay.accession", "ProteinDetail", function(object) object@assay.accession)

if (!isGeneric("assay.accession<-")) {
    setGeneric("assay.accession<-", function(object, value) standardGeneric("assay.accession<-"))
}
#' Replaces a protein assay accession
#' 
#' @param object a ProteinDetail
#' @param value the assay accession
#' @author Jose A. Dianes
#' @export
setMethod("assay.accession<-", "ProteinDetail",
          function(object, value) {
              object@assay.accession <- value
              if (validObject(object))
                  return(object)
          }
)

if (!isGeneric("synonyms")) {
    setGeneric("synonyms", function(object) standardGeneric("synonyms"))
}
#' Returns a protein synonyms
#' 
#' @param object a ProjectDetail
#' @return the synonyms
#' @author Jose A. Dianes
#' @export
setMethod("synonyms", "ProteinDetail", function(object) object@synonyms)

if (!isGeneric("synonyms<-")) {
    setGeneric("synonyms<-", function(object, value) standardGeneric("synonyms<-"))
}
#' Replaces a protein synonyms
#' 
#' @param object a ProteinDetail
#' @param value the synonyms
#' @author Jose A. Dianes
#' @export
setMethod("synonyms<-", "ProteinDetail",
          function(object, value) {
              object@synonyms <- value
              if (validObject(object))
                  return(object)
          }
)

if (!isGeneric("description")) {
    setGeneric("description", function(object) standardGeneric("description"))
}
#' Returns a protein description
#' 
#' @param object a ProjectDetail
#' @return the description
#' @author Jose A. Dianes
#' @export
setMethod("description", "ProteinDetail", function(object) object@description)

if (!isGeneric("description<-")) {
    setGeneric("description<-", function(object, value) standardGeneric("description<-"))
}
#' Replaces a protein description
#' 
#' @param object a ProteinDetail
#' @param value the description
#' @author Jose A. Dianes
#' @export
setMethod("description<-", "ProteinDetail",
          function(object, value) {
              object@description <- value
              if (validObject(object))
                  return(object)
          }
)

if (!isGeneric("sequence")) {
    setGeneric("sequence", function(object) standardGeneric("sequence"))
}
#' Returns a protein sequence
#' 
#' @param object a ProjectDetail
#' @return the sequence
#' @author Jose A. Dianes
#' @export
setMethod("sequence", "ProteinDetail", function(object) object@sequence)

if (!isGeneric("sequence<-")) {
    setGeneric("sequence<-", function(object, value) standardGeneric("sequence<-"))
}
#' Replaces a protein sequence
#' 
#' @param object a ProteinDetail
#' @param value the sequence
#' @author Jose A. Dianes
#' @export
setMethod("sequence<-", "ProteinDetail",
          function(object, value) {
              object@sequence <- value
              if (validObject(object))
                  return(object)
          }
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
        value$project.accession <- x@project.accession
        value$assay.accession <- x@assay.accession
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
               project.accession = json.object$projectAccession,
               assay.accession = json.object$assayAccession,
               synonyms = ifelse(is.null(json.object$synonyms), c(MISSING_VALUE), json.object$synonyms),
               description = ifelse(is.null(json.object$description), MISSING_VALUE, json.object$description),
               sequence = ifelse(is.null(json.object$sequence), MISSING_VALUE, json.object$sequence)
    )
    
    return (res)
}

#' Returns a list of PRIDE Archive PSMs associated with a given project
#'
#' @param accession The project accession
#' @param count The maximum proteins to return from the project (deault is 1)
#' @return The list of ProteinDetail objects
#' @author Jose A. Dianes
#' @details TODO
#' @export
get.list.ProteinDetail <- function(project.accession, count=100) {
    json.list <- fromJSON(file=paste0(pride_archive_url, "/protein/list/project/", project.accession, "?show=", count), method="C")
    details.list <- lapply(json.list[[1]], function(x) { from.json.ProteinDetail(x)})
    return(details.list)
}

#' Returns the number of proteins for a particual public project
#'
#' @param project.accession The project accession to count proteins from
#' @return The count of proteins
#' @author Jose A. Dianes
#' @details TODO
#' @export
#' @importFrom rjson fromJSON
count.ProteinDetail <- function(project.accession) {    
    protein.count <- fromJSON(file=URLencode(paste0(pride_archive_url, "/protein/count/project/", project.accession)), method="C")
    protein.count                          
}

format.ProteinDetail <- function(x, ...) paste0(x@accession, ", ", x@assayAccession)


