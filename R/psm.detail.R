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

setMethod("show",
          signature = "PsmDetail",
          definition = function(object) {
              cat("An object of class ", class(object), " with\n", sep="")
              cat("    ID: ", object@id, "\n", sep="")
              cat("    Project accession: ", object@project.accession, "\n", sep="")
              cat("    Assay accession: ", object@assay.accession, "\n", sep="")
              cat("    Protein accession: ", object@protein.accession, "\n", sep="")
              cat("    Sequence: ", object@sequence, "\n", sep=" ")
              invisible(NULL)
          }
)

if (!isGeneric("id")) {
    setGeneric("id", function(object) standardGeneric("id"))
}
#' Returns a PSM id
#' 
#' @param object a PsmDetail
#' @return the ID
#' @author Jose A. Dianes
#' @export
setMethod("id", "PsmDetail", function(object) object@id)

if (!isGeneric("id<-")) {
    setGeneric("id<-", function(object, value) standardGeneric("id<-"))
}
#' Replaces a PSM is
#' 
#' @param object a ProteinDetail
#' @param value the id
#' @author Jose A. Dianes
#' @export
setMethod("id<-", "PsmDetail",
          function(object, value) {
              object@id <- value
              if (validObject(object))
                  return(object)
          }
)

if (!isGeneric("project.accession")) {
    setGeneric("project.accession", function(object) standardGeneric("project.accession"))
}
#' Returns a PSM project.accession
#' 
#' @param object a PsmDetail
#' @return the project.accession
#' @author Jose A. Dianes
#' @export
setMethod("project.accession", "PsmDetail", function(object) object@project.accession)

if (!isGeneric("project.accession<-")) {
    setGeneric("project.accession<-", function(object, value) standardGeneric("project.accession<-"))
}
#' Replaces a PSM project.accession
#' 
#' @param object a ProteinDetail
#' @param value the project.accession
#' @author Jose A. Dianes
#' @export
setMethod("project.accession<-", "PsmDetail",
          function(object, value) {
              object@project.accession <- value
              if (validObject(object))
                  return(object)
          }
)

if (!isGeneric("assay.accession")) {
    setGeneric("assay.accession", function(object) standardGeneric("assay.accession"))
}
#' Returns a PSM assay.accession
#' 
#' @param object a PsmDetail
#' @return the assay.accession
#' @author Jose A. Dianes
#' @export
setMethod("assay.accession", "PsmDetail", function(object) object@assay.accession)

if (!isGeneric("assay.accession<-")) {
    setGeneric("assay.accession<-", function(object, value) standardGeneric("assay.accession<-"))
}
#' Replaces a PSM assay.accession
#' 
#' @param object a ProteinDetail
#' @param value the assay.accession
#' @author Jose A. Dianes
#' @export
setMethod("assay.accession<-", "PsmDetail",
          function(object, value) {
              object@assay.accession <- value
              if (validObject(object))
                  return(object)
          }
)

if (!isGeneric("protein.accession")) {
    setGeneric("protein.accession", function(object) standardGeneric("protein.accession"))
}
#' Returns a PSM protein.accession
#' 
#' @param object a PsmDetail
#' @return the protein.accession
#' @author Jose A. Dianes
#' @export
setMethod("protein.accession", "PsmDetail", function(object) object@protein.accession)

if (!isGeneric("protein.accession<-")) {
    setGeneric("protein.accession<-", function(object, value) standardGeneric("protein.accession<-"))
}
#' Replaces a PSM assay.accession
#' 
#' @param object a ProteinDetail
#' @param value the protein.accession
#' @author Jose A. Dianes
#' @export
setMethod("protein.accession<-", "PsmDetail",
          function(object, value) {
              object@protein.accession <- value
              if (validObject(object))
                  return(object)
          }
)

if (!isGeneric("start.postion")) {
    setGeneric("start.postion", function(object) standardGeneric("start.postion"))
}
#' Returns a PSM start.postion
#' 
#' @param object a PsmDetail
#' @return the start.postion
#' @author Jose A. Dianes
#' @export
setMethod("start.postion", "PsmDetail", function(object) object@start.postion)

if (!isGeneric("start.postion<-")) {
    setGeneric("start.postion<-", function(object, value) standardGeneric("start.postion<-"))
}
#' Replaces a PSM start.postion
#' 
#' @param object a ProteinDetail
#' @param value the start.postion
#' @author Jose A. Dianes
#' @export
setMethod("start.postion<-", "PsmDetail",
          function(object, value) {
              object@start.postion <- value
              if (validObject(object))
                  return(object)
          }
)

if (!isGeneric("end.position")) {
    setGeneric("end.position", function(object) standardGeneric("end.position"))
}
#' Returns a PSM end.position
#' 
#' @param object a PsmDetail
#' @return the end.position
#' @author Jose A. Dianes
#' @export
setMethod("end.position", "PsmDetail", function(object) object@end.position)

if (!isGeneric("end.position<-")) {
    setGeneric("end.position<-", function(object, value) standardGeneric("end.position<-"))
}
#' Replaces a PSM end.position
#' 
#' @param object a ProteinDetail
#' @param value the end.position
#' @author Jose A. Dianes
#' @export
setMethod("end.position<-", "PsmDetail",
          function(object, value) {
              object@end.position <- value
              if (validObject(object))
                  return(object)
          }
)

if (!isGeneric("modifications")) {
    setGeneric("modifications", function(object) standardGeneric("modifications"))
}
#' Returns a PSM modifications
#' 
#' @param object a PsmDetail
#' @return the modifications
#' @author Jose A. Dianes
#' @export
setMethod("modifications", "PsmDetail", function(object) object@modifications)

if (!isGeneric("modifications<-")) {
    setGeneric("modifications<-", function(object, value) standardGeneric("modifications<-"))
}
#' Replaces a PSM modifications
#' 
#' @param object a ProteinDetail
#' @param value the id
#' @author Jose A. Dianes
#' @export
setMethod("modifications<-", "PsmDetail",
          function(object, value) {
              object@modifications <- value
              if (validObject(object))
                  return(object)
          }
)

if (!isGeneric("search.engines")) {
    setGeneric("search.engines", function(object) standardGeneric("search.engines"))
}
#' Returns a PSM search.engines
#' 
#' @param object a PsmDetail
#' @return the search.engines
#' @author Jose A. Dianes
#' @export
setMethod("search.engines", "PsmDetail", function(object) object@search.engines)

if (!isGeneric("search.engines<-")) {
    setGeneric("search.engines<-", function(object, value) standardGeneric("search.engines<-"))
}
#' Replaces a PSM search.engines
#' 
#' @param object a ProteinDetail
#' @param value the search.engines
#' @author Jose A. Dianes
#' @export
setMethod("search.engines<-", "PsmDetail",
          function(object, value) {
              object@search.engines <- value
              if (validObject(object))
                  return(object)
          }
)

if (!isGeneric("search.engines.scores")) {
    setGeneric("search.engines.scores", function(object) standardGeneric("search.engines.scores"))
}
#' Returns a PSM search.engines.scores
#' 
#' @param object a PsmDetail
#' @return the search.engines.scores
#' @author Jose A. Dianes
#' @export
setMethod("search.engines.scores", "PsmDetail", function(object) object@search.engines.scores)

if (!isGeneric("search.engines.scores<-")) {
    setGeneric("search.engines.scores<-", function(object, value) standardGeneric("search.engines.scores<-"))
}
#' Replaces a PSM search.engines.scores
#' 
#' @param object a ProteinDetail
#' @param value the search.engines.scores
#' @author Jose A. Dianes
#' @export
setMethod("search.engines.scores<-", "PsmDetail",
          function(object, value) {
              object@search.engines.scores <- value
              if (validObject(object))
                  return(object)
          }
)

if (!isGeneric("retention.time")) {
    setGeneric("retention.time", function(object) standardGeneric("retention.time"))
}
#' Returns a PSM retention.time
#' 
#' @param object a PsmDetail
#' @return the retention.time
#' @author Jose A. Dianes
#' @export
setMethod("retention.time", "PsmDetail", function(object) object@retention.time)

if (!isGeneric("retention.time<-")) {
    setGeneric("retention.time<-", function(object, value) standardGeneric("retention.time<-"))
}
#' Replaces a PSM retention.time
#' 
#' @param object a ProteinDetail
#' @param value the retention.time
#' @author Jose A. Dianes
#' @export
setMethod("retention.time<-", "PsmDetail",
          function(object, value) {
              object@retention.time <- value
              if (validObject(object))
                  return(object)
          }
)

if (!isGeneric("charge")) {
    setGeneric("charge", function(object) standardGeneric("charge"))
}
#' Returns a PSM charge
#' 
#' @param object a PsmDetail
#' @return the charge
#' @author Jose A. Dianes
#' @export
setMethod("charge", "PsmDetail", function(object) object@charge)

if (!isGeneric("charge<-")) {
    setGeneric("charge<-", function(object, value) standardGeneric("charge<-"))
}
#' Replaces a PSM charge
#' 
#' @param object a ProteinDetail
#' @param value the charge
#' @author Jose A. Dianes
#' @export
setMethod("charge<-", "PsmDetail",
          function(object, value) {
              object@charge <- value
              if (validObject(object))
                  return(object)
          }
)

if (!isGeneric("calculated.mz")) {
    setGeneric("calculated.mz", function(object) standardGeneric("calculated.mz"))
}
#' Returns a PSM calculated.mz
#' 
#' @param object a PsmDetail
#' @return the calculated.mz
#' @author Jose A. Dianes
#' @export
setMethod("calculated.mz", "PsmDetail", function(object) object@calculated.mz)

if (!isGeneric("calculated.mz<-")) {
    setGeneric("calculated.mz<-", function(object, value) standardGeneric("calculated.mz<-"))
}
#' Replaces a PSM calculated.mz
#' 
#' @param object a ProteinDetail
#' @param value the calculated.mz
#' @author Jose A. Dianes
#' @export
setMethod("calculated.mz<-", "PsmDetail",
          function(object, value) {
              object@calculated.mz <- value
              if (validObject(object))
                  return(object)
          }
)

if (!isGeneric("experimental.mz")) {
    setGeneric("experimental.mz", function(object) standardGeneric("experimental.mz"))
}
#' Returns a PSM experimental.mz
#' 
#' @param object a PsmDetail
#' @return the experimental.mz
#' @author Jose A. Dianes
#' @export
setMethod("experimental.mz", "PsmDetail", function(object) object@experimental.mz)

if (!isGeneric("experimental.mz<-")) {
    setGeneric("experimental.mz<-", function(object, value) standardGeneric("experimental.mz<-"))
}
#' Replaces a PSM experimental.mz
#' 
#' @param object a ProteinDetail
#' @param value the experimental.mz
#' @author Jose A. Dianes
#' @export
setMethod("experimental.mz<-", "PsmDetail",
          function(object, value) {
              object@experimental.mz <- value
              if (validObject(object))
                  return(object)
          }
)

if (!isGeneric("pre.aa")) {
    setGeneric("pre.aa", function(object) standardGeneric("pre.aa"))
}
#' Returns a PSM pre.aa
#' 
#' @param object a PsmDetail
#' @return the pre.aa
#' @author Jose A. Dianes
#' @export
setMethod("pre.aa", "PsmDetail", function(object) object@pre.aa)

if (!isGeneric("pre.aa<-")) {
    setGeneric("pre.aa<-", function(object, value) standardGeneric("pre.aa<-"))
}
#' Replaces a PSM pre.aa
#' 
#' @param object a ProteinDetail
#' @param value the pre.aa
#' @author Jose A. Dianes
#' @export
setMethod("pre.aa<-", "PsmDetail",
          function(object, value) {
              object@pre.aa <- value
              if (validObject(object))
                  return(object)
          }
)

if (!isGeneric("post.aa")) {
    setGeneric("post.aa", function(object) standardGeneric("post.aa"))
}
#' Returns a PSM post.aa
#' 
#' @param object a PsmDetail
#' @return the post.aa
#' @author Jose A. Dianes
#' @export
setMethod("post.aa", "PsmDetail", function(object) object@post.aa)

if (!isGeneric("post.aa<-")) {
    setGeneric("post.aa<-", function(object, value) standardGeneric("post.aa<-"))
}
#' Replaces a PSM post.aa
#' 
#' @param object a ProteinDetail
#' @param value the post.aa
#' @author Jose A. Dianes
#' @export
setMethod("post.aa<-", "PsmDetail",
          function(object, value) {
              object@post.aa <- value
              if (validObject(object))
                  return(object)
          }
)

if (!isGeneric("spectrum.id")) {
    setGeneric("spectrum.id", function(object) standardGeneric("spectrum.id"))
}
#' Returns a PSM spectrum.id
#' 
#' @param object a PsmDetail
#' @return the spectrum.id
#' @author Jose A. Dianes
#' @export
setMethod("spectrum.id", "PsmDetail", function(object) object@spectrum.id)

if (!isGeneric("spectrum.id<-")) {
    setGeneric("spectrum.id<-", function(object, value) standardGeneric("spectrum.id<-"))
}
#' Replaces a PSM spectrum.id
#' 
#' @param object a ProteinDetail
#' @param value the spectrum.id
#' @author Jose A. Dianes
#' @export
setMethod("spectrum.id<-", "PsmDetail",
          function(object, value) {
              object@spectrum.id <- value
              if (validObject(object))
                  return(object)
          }
)

if (!isGeneric("reported.id")) {
    setGeneric("reported.id", function(object) standardGeneric("reported.id"))
}
#' Returns a PSM reported.id
#' 
#' @param object a PsmDetail
#' @return the reported.id
#' @author Jose A. Dianes
#' @export
setMethod("reported.id", "PsmDetail", function(object) object@reported.id)

if (!isGeneric("reported.id<-")) {
    setGeneric("reported.id<-", function(object, value) standardGeneric("reported.id<-"))
}
#' Replaces a PSM reported.id
#' 
#' @param object a ProteinDetail
#' @param value the reported.id
#' @author Jose A. Dianes
#' @export
setMethod("reported.id<-", "PsmDetail",
          function(object, value) {
              object@reported.id <- value
              if (validObject(object))
                  return(object)
          }
)

if (!isGeneric("sequence")) {
    setGeneric("sequence", function(object) standardGeneric("sequence"))
}
#' Returns a PSM sequence
#' 
#' @param object a PsmDetail
#' @return the sequence
#' @author Jose A. Dianes
#' @export
setMethod("sequence", "PsmDetail", function(object) object@sequence)

if (!isGeneric("sequence<-")) {
    setGeneric("sequence<-", function(object, value) standardGeneric("sequence<-"))
}
#' Replaces a PSM sequence
#' 
#' @param object a ProteinDetail
#' @param value the sequence
#' @author Jose A. Dianes
#' @export
setMethod("sequence<-", "PsmDetail",
          function(object, value) {
              object@sequence <- value
              if (validObject(object))
                  return(object)
          }
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
               assay.accession = json.object$assayAccession,
               protein.accession = json.object$proteinAccession,
               start.position = ifelse(is.null(json.object$startPosition), -1, json.object$startPosition),
               end.position = ifelse(is.null(json.object$endPosition), -1, json.object$endPosition),
               modifications = ifelse(is.null(json.object$modifications), c(MISSING_VALUE), json.object$modifications),
               search.engines = ifelse(is.null(json.object$searchEngines), c(MISSING_VALUE), json.object$searchEngines),
               search.engines.scores = ifelse(is.null(json.object$searchEnginesScores), c(MISSING_VALUE), json.object$searchEnginesScores),
               retention.time = ifelse(is.null(json.object$retentionTime), 0.0, json.object$retentionTime),
               charge = ifelse(is.null(json.object$charge), 0.0, json.object$charge),
               calculated.mz = ifelse(is.null(json.object$calculatedMz), 0.0, json.object$calculatedMz),
               experimental.mz = ifelse(is.null(json.object$experimentalMz), 0.0, json.object$experimentalMz),
               pre.aa = ifelse(is.null(json.object$preAA), MISSING_VALUE, json.object$preAA),
               post.aa = ifelse(is.null(json.object$postAA), MISSING_VALUE, json.object$postAA),
               spectrum.id = ifelse(is.null(json.object$spectrumID), 0.0, json.object$spectrumID),
               reported.id = ifelse(is.null(json.object$reportedID), 0.0, json.object$reportedID),
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



