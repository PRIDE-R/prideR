pride_archive_url <- "http://www.ebi.ac.uk/pride/ws/archive"
pride_archive_url_dev <- "http://wwwdev.ebi.ac.uk/pride/ws/archive"

MISSING_VALUE <- "Not available"

#' AssayDetail represents a PRIDE Archive assay
#'
#' @importFrom rjson fromJSON
#' @export 
#' @exportClass AssayDetail
setClass(
    "AssayDetail", 
    representation(
        assay.accession = "character", 
        project.accession = "character",
        protein.count = "numeric",
        peptide.count = "numeric",
        unique.peptide.count = "numeric",
        identified.spectrum.count = "numeric",
        total.spectrum.count = "numeric",
        diseases = "vector",
        species = "vector",
        softwares = "vector",
        instrument.names = "vector",
        sample.details = "vector",
        ptm.names = "vector",
        quant.methods = "vector",
        contacts = "vector",
        experimental.factor = "character",
        chromatogram = "logical",
        ms2.annotation = "logical",
        keywords = "character",
        short.label = "character",
        title = "character"
    ),
    prototype(
        assay.accession = MISSING_VALUE, 
        project.accession = MISSING_VALUE,
        protein.count = 0,
        peptide.count = 0,
        unique.peptide.count = 0,
        identified.spectrum.count = 0,
        total.spectrum.count = 0,
        diseases = c(MISSING_VALUE),
        species = c(MISSING_VALUE),
        softwares = c(MISSING_VALUE),
        instrument.names = c(MISSING_VALUE),
        sample.details = c(MISSING_VALUE),
        ptm.names = c(MISSING_VALUE),
        quant.methods = c(MISSING_VALUE),
        contacts = c(MISSING_VALUE),
        experimental.factor = MISSING_VALUE,
        chromatogram = FALSE,
        ms2.annotation = FALSE,
        keywords = MISSING_VALUE,
        short.label = MISSING_VALUE,
        title = MISSING_VALUE
    )
)

setMethod("show",
          signature = "AssayDetail",
          definition = function(object) {
              cat("An object of class ", class(object), "\n", sep="")
              cat("    Title: ", object@title, "\n", sep="")
              cat("    Assay accession: ", object@assay.accession, "\n", sep="")
              cat("    Project accession: ", object@project.accession, "\n", sep="")
              cat("    Protein #: ", object@protein.count, "\n", sep="")
              cat("    Peptide #: ", object@peptide.count, "\n", sep="")
              cat("    Unique Peptide # ", object@unique.peptide.count, "\n", sep="")
              cat("    Identified Spectrum #: ", object@identified.spectrum.count, "\n", sep="")
              cat("    Total Spectrum #: ", object@total.spectrum.count, "\n", sep="")
              cat("    Diseases: ", object@diseases, "\n", sep=" ")
              cat("    Species: ", object@species, "\n", sep=" ")
              cat("    Softwares: ", object@softwares, "\n", sep=" ")
              cat("    Instrument names: ", object@instrument.names, "\n", sep=" ")
              cat("    Sample details: ", object@sample.details, "\n", sep=" ")
              cat("    PTM names: ", object@ptm.names, "\n", sep=" ")
              cat("    Quant methods: ", object@quant.methods, "\n", sep=" ")
              cat("    Contacts: ", object@contacts[[1]]@email, "\n", sep=" ")
              cat("    Experimental factor: ", object@experimental.factor, "\n", sep=" ")
              cat("    Chromatogram: ", object@chromatogram, "\n", sep=" ")
              cat("    MS2 annotation: ", object@ms2.annotation, "\n", sep=" ")
              cat("    Short label: ", object@short.label, "\n", sep=" ")
              invisible(NULL)
          }
)

if (!isGeneric("assay.accession")) {
    setGeneric("assay.accession", function(object) standardGeneric("assay.accession"))
}
#' Returns an assay accession
#' 
#' @param object a AssayDetail
#' @return the accession
#' @author Jose A. Dianes
#' @export
setMethod("assay.accession", "AssayDetail", function(object) object@assay.accession)

if (!isGeneric("assay.accession<-")) {
    setGeneric("assay.accession<-", function(object, value) standardGeneric("assay.accession<-"))
}
#' Replaces an assay accession
#' 
#' @param object a AssayDetail
#' @param value the accession
#' @author Jose A. Dianes
#' @export
setMethod("assay.accession<-", "AssayDetail",
          function(object, value) {
              object@assay.accession <- value
              if (validObject(object))
                  return(object)
          }
)

if (!isGeneric("project.accession")) {
    setGeneric("project.accession", function(object) standardGeneric("project.accession"))
}
#' Returns an assay project.assay
#' 
#' @param object a AssayDetail
#' @return the project accession
#' @author Jose A. Dianes
#' @export
setMethod("project.accession", "AssayDetail", function(object) object@project.accession)

if (!isGeneric("project.accession<-")) {
    setGeneric("project.accession<-", function(object, value) standardGeneric("project.accession<-"))
}
#' Replaces an assay project accession
#' 
#' @param object a AssayDetail
#' @param value the project.accession
#' @author Jose A. Dianes
#' @export
setMethod("project.accession<-", "AssayDetail",
          function(object, value) {
              object@project.accession <- value
              if (validObject(object))
                  return(object)
          }
)

if (!isGeneric("protein.count")) {
    setGeneric("protein.count", function(object) standardGeneric("protein.count"))
}
#' Returns an assay protein.count
#' 
#' @param object a AssayDetail
#' @return the protein.count
#' @author Jose A. Dianes
#' @export
setMethod("protein.count", "AssayDetail", function(object) object@protein.count)

if (!isGeneric("protein.count<-")) {
    setGeneric("protein.count<-", function(object, value) standardGeneric("protein.count<-"))
}
#' Replaces an assay protein.count
#' 
#' @param object a AssayDetail
#' @param value the protein.count
#' @author Jose A. Dianes
#' @export
setMethod("protein.count<-", "AssayDetail",
          function(object, value) {
              object@protein.count <- value
              if (validObject(object))
                  return(object)
          }
)

if (!isGeneric("peptide.count")) {
    setGeneric("peptide.count", function(object) standardGeneric("peptide.count"))
}
#' Returns an assay peptide.count
#' 
#' @param object a AssayDetail
#' @return the peptide.count
#' @author Jose A. Dianes
#' @export
setMethod("peptide.count", "AssayDetail", function(object) object@peptide.count)

if (!isGeneric("peptide.count<-")) {
    setGeneric("peptide.count<-", function(object, value) standardGeneric("peptide.count<-"))
}
#' Replaces an assay peptide.count
#' 
#' @param object a AssayDetail
#' @param value the peptide.count
#' @author Jose A. Dianes
#' @export
setMethod("peptide.count<-", "AssayDetail",
          function(object, value) {
              object@peptide.count <- value
              if (validObject(object))
                  return(object)
          }
)

if (!isGeneric("unique.peptide.count")) {
    setGeneric("unique.peptide.count", function(object) standardGeneric("unique.peptide.count"))
}
#' Returns an assay unique.peptide.count
#' 
#' @param object a AssayDetail
#' @return the unique.peptide.count
#' @author Jose A. Dianes
#' @export
setMethod("unique.peptide.count", "AssayDetail", function(object) object@unique.peptide.count)

if (!isGeneric("unique.peptide.count<-")) {
    setGeneric("unique.peptide.count<-", function(object, value) standardGeneric("unique.peptide.count<-"))
}
#' Replaces an assay unique.peptide.count
#' 
#' @param object a AssayDetail
#' @param value the unique.peptide.count
#' @author Jose A. Dianes
#' @export
setMethod("unique.peptide.count<-", "AssayDetail",
          function(object, value) {
              object@unique.peptide.count <- value
              if (validObject(object))
                  return(object)
          }
)

if (!isGeneric("identified.spectrum.count")) {
    setGeneric("identified.spectrum.count", function(object) standardGeneric("identified.spectrum.count"))
}
#' Returns an assay identified.spectrum.count
#' 
#' @param object a AssayDetail
#' @return the identified.spectrum.count
#' @author Jose A. Dianes
#' @export
setMethod("identified.spectrum.count", "AssayDetail", function(object) object@identified.spectrum.count)

if (!isGeneric("identified.spectrum.count<-")) {
    setGeneric("identified.spectrum.count<-", function(object, value) standardGeneric("identified.spectrum.count<-"))
}
#' Replaces an assay identified.spectrum.count
#' 
#' @param object a AssayDetail
#' @param value the identified.spectrum.count
#' @author Jose A. Dianes
#' @export
setMethod("identified.spectrum.count<-", "AssayDetail",
          function(object, value) {
              object@identified.spectrum.count <- value
              if (validObject(object))
                  return(object)
          }
)

if (!isGeneric("total.spectrum.count")) {
    setGeneric("total.spectrum.count", function(object) standardGeneric("total.spectrum.count"))
}
#' Returns an assay total.spectrum.count
#' 
#' @param object a AssayDetail
#' @return the total.spectrum.count
#' @author Jose A. Dianes
#' @export
setMethod("total.spectrum.count", "AssayDetail", function(object) object@total.spectrum.count)

if (!isGeneric("total.spectrum.count<-")) {
    setGeneric("total.spectrum.count<-", function(object, value) standardGeneric("total.spectrum.count<-"))
}
#' Replaces an assay total.spectrum.count
#' 
#' @param object a AssayDetail
#' @param value the total.spectrum.count
#' @author Jose A. Dianes
#' @export
setMethod("total.spectrum.count<-", "AssayDetail",
          function(object, value) {
              object@total.spectrum.count <- value
              if (validObject(object))
                  return(object)
          }
)

#' Returns a data frame from AssayDetail inputs
#'
#' @param x The assay details
#' @param row.names optional row names
#' @return The assay details as a data frame
#' @author Jose A. Dianes
#' @details TODO
#' @export
as.data.frame.AssayDetail <-
    function(x, row.names=NULL, optional=FALSE, ...)
    {
        # set row names if provided
        if (is.null(row.names))
            row.names <- x@assay.accession
        # create the data frame just with the accession column
        value <- list(x@assay.accession)
        attr(value, "row.names") <- row.names
        class(value) <- "data.frame"
        names(value) <- c("assay.accession")
        # add the rest of the columns
        value$project.accession <- x@project.accession
        value$protein.count <- x@protein.count
        value$peptide.count <- x@peptide.count
        value$unique.peptide.count <- x@unique.peptide.count
        value$identified.spectrum.count <- x@identified.spectrum.count
        value$total.spectrum.count <- x@total.spectrum.count
        return(value)
    }

format.AssayDetail <- function(x, ...) paste0(x@assay.accession, ", ", x@project.accession)

#' Returns a AssayDetail instance from a JSON string representation
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
from.json.AssayDetail <- function(json.object) {
    res <- new("AssayDetail",
               assay.accession = json.object$assayAccession,
               project.accession = json.object$projectAccession,
               protein.count = json.object$proteinCount,
               peptide.count = json.object$peptideCount,
               unique.peptide.count = json.object$uniquePeptideCount,
               identified.spectrum.count = json.object$identifiedSpectrumCount,
               total.spectrum.count = json.object$totalSpectrumCount,
               diseases = ifelse(is.null(json.object$diseases)||(length(json.object$diseases)==0), c(MISSING_VALUE), json.object$diseases),
               species = ifelse(is.null(json.object$species)||(length(json.object$species)==0), c(MISSING_VALUE), json.object$species),
               softwares = ifelse(is.null(json.object$softwares)||(length(json.object$softwares)==0), c(MISSING_VALUE), json.object$softwares),
               instrument.names = ifelse(is.null(json.object$instrumentNames)||(length(json.object$instrumentNames)==0), c(MISSING_VALUE), json.object$instrumentNames),
               sample.details = ifelse(is.null(json.object$sampleDetails)||(length(json.object$sampleDetails)==0), c(MISSING_VALUE), json.object$sampleDetails),
               ptm.names = ifelse(is.null(json.object$ptmNames)||(length(json.object$ptmNames)==0), c(MISSING_VALUE), json.object$ptmNames),
               quant.methods = ifelse(is.null(json.object$quantMethods)||(length(json.object$quantMethods)==0), c(MISSING_VALUE), json.object$quantMethods),
               contacts = ifelse(is.null(json.object$contacts)||(length(json.object$contacts)==0), c(MISSING_VALUE), lapply(json.object$contacts, function (x) {from.json.ContactDetail(x)})),
               experimental.factor = json.object$experimentalFactor,
               chromatogram = json.object$chromatogram,
               ms2.annotation = json.object$ms2Annotation,
               keywords = ifelse(is.null(json.object$keywords), MISSING_VALUE, json.object$keywords),
               short.label = json.object$shortLabel,
               title = json.object$title
    )
    
    return (res)
}

#' Returns a PRIDE Archive assay
#'
#' @param accession The assay accession
#' @return The assay as object of AssayDetail
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
#' @export
get.AssayDetail <- function(assay.accession) {
    from.json.AssayDetail(fromJSON(file=paste0(pride_archive_url, "/assay/", assay.accession), method="C"))
}

#' Returns a list of PRIDE Archive assays associated to a project
#'
#' @param project.accession the project accession
#' @return The list of AssayDetail objects
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
#' @export
get.list.AssayDetail <- function(project.accession) {
    json.list <- fromJSON(file=paste0(pride_archive_url, "/assay/list/project/", project.accession), method="C")
    assay.list <- lapply(json.list[[1]], function(x) { from.json.AssayDetail(x)})
    return(assay.list)
}

#' Returns the number of assays associated with a project
#'
#'@param project.accession the project accession
#' @return The count of assays
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
#' @export
count.AssayDetail <- function(project.accession) {
    assayCount <- fromJSON(file=paste0(pride_archive_url, "/assay/count/project/", project.accession), method="C")
    assayCount                          
}

