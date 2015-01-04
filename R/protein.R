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
    
    slots = c(
              protein.accession = "character", 
              project.accession = "character", 
              assay.accession = "character", 
              protein.synonyms = "character", 
              protein.description = "character", 
              protein.sequence = "character"
            ),
    
    prototype = list(
                  protein.synonyms = MISSING_VALUE, 
                  protein.description = MISSING_VALUE, 
                  protein.sequence = MISSING_VALUE
                ),
    
    validity = function(object) {
      # check protein.accession
      if (!is.character(object@protein.accession) || nchar(object@protein.accession) == 0 || is.na(object@protein.accession))
        return("'protein.accession' must be a single valid string")
      
      # check project.accession
      if (!is.character(object@project.accession) || nchar(object@project.accession) == 0 || is.na(object@project.accession))
        return("'project.accession' must be a single valid string")
      
      # check assay.accession
      if (!is.character(object@assay.accession) || nchar(object@assay.accession) == 0 || is.na(object@assay.accession))
        return("'assay.accession' must be a single valid string")
      
      # check protein.synonyms
      if (!is.character(object@protein.synonyms) || 0 %in% nchar(object@protein.synonyms) || is.na(object@protein.synonyms))
        return("'proetin.synonyms' must be a one or multiple valid strings")
      
      # check protein.description
      if (!is.character(object@protein.description) || nchar(object@protein.description) == 0 || is.na(object@protein.description))
        return("'protein.description' must be a single valid string")
      
      # check protein.sequence
      if (!is.character(object@protein.sequence) || nchar(object@protein.sequence) == 0 || is.na(object@protein.sequence))
        return("'protein.sequence' must be a single valid string")
    }
)

#' Constructor for ProteinDetail
#' 
#' @param protein.accession the protein accession
#' @param project.accession the project accession where the protein belongs to
#' @param assay.accession the assay accession where the protein belongs to
#' @param protein.synonyms the synonyms of the protein
#' @param protein.description the description of the protein
#' @param protein.sequence the protein sequence
#' @export
ProteinDetail <- function(protein.accession, 
                          project.accession, 
                          assay.accession, 
                          protein.synonyms, 
                          protein.description, 
                          protein.sequence) {
  new("ProteinDetail",
      protein.accession = protein.accession,
      project.accession = project.accession,
      assay.accession = assay.accession,
      protein.synonyms = protein.synonyms,
      protein.description = protein.description,
      protein.sequence = protein.sequence
      )    
}

#' Show the content of a ProteinDetail
#' 
#' @param object ProteinDetail object
#' @export
setMethod("show",
          signature = "ProteinDetail",
          definition = function(object) {
              cat("An object of class ", class(object), " with\n", sep="")
              cat("    Protein accession: ", object@protein.accession, "\n", sep="")
              cat("    Project accession: ", object@project.accession, "\n", sep="")
              cat("    Assay accession: ", object@assay.accession, "\n", sep="")
              cat("    Synonyms: ", object@protein.synonyms, "\n", sep=" ")
              cat("    Description: ", object@protein.description, "\n", sep=" ")
              cat("    Sequence: ", object@protein.sequence, "\n", sep=" ")
              invisible(NULL)
          }
)

#' Returns a protein accession
#' 
#' @param object a ProjectDetail
#' @return the accession
#' @author Jose A. Dianes
#' @export
setMethod("protein.accession", "ProteinDetail", function(object) object@protein.accession)

#' Replaces a protein accession
#' 
#' @param object a ProteinDetail
#' @param value the accession
#' @author Jose A. Dianes
#' @export
setReplaceMethod("protein.accession", "ProteinDetail",
          function(object, value) {
              object@protein.accession <- value
              if (validObject(object))
                  return(object)
          }
)

#' Returns a protein project accession
#' 
#' @param object a ProjectDetail
#' @return the project accession
#' @author Jose A. Dianes
#' @export
setMethod("project.accession", "ProteinDetail", function(object) object@project.accession)

#' Replaces a protein project accession
#' 
#' @param object a ProteinDetail
#' @param value the project accession
#' @author Jose A. Dianes
#' @export
setReplaceMethod("project.accession", "ProteinDetail",
          function(object, value) {
              object@project.accession <- value
              if (validObject(object))
                  return(object)
          }
)

#' Returns a protein assay accession
#' 
#' @param object a ProjectDetail
#' @return the assay accession
#' @author Jose A. Dianes
#' @export
setMethod("assay.accession", "ProteinDetail", function(object) object@assay.accession)

#' Replaces a protein assay accession
#' 
#' @param object a ProteinDetail
#' @param value the assay accession
#' @author Jose A. Dianes
#' @export
setReplaceMethod("assay.accession", "ProteinDetail",
          function(object, value) {
              object@assay.accession <- value
              if (validObject(object))
                  return(object)
          }
)

#' Returns a protein synonyms
#' 
#' @param object a ProjectDetail
#' @return the synonyms
#' @author Jose A. Dianes
#' @export
setMethod("protein.synonyms", "ProteinDetail", function(object) object@protein.synonyms)

#' Replaces a protein synonyms
#' 
#' @param object a ProteinDetail
#' @param value the synonyms
#' @author Jose A. Dianes
#' @export
setReplaceMethod("protein.synonyms", "ProteinDetail",
          function(object, value) {
              object@protein.synonyms <- value
              if (validObject(object))
                  return(object)
          }
)

#' Returns a protein description
#' 
#' @param object a ProjectDetail
#' @return the description
#' @author Jose A. Dianes
#' @export
setMethod("protein.description", "ProteinDetail", function(object) object@protein.description)

#' Replaces a protein description
#' 
#' @param object a ProteinDetail
#' @param value the description
#' @author Jose A. Dianes
#' @export
setReplaceMethod("protein.description", "ProteinDetail",
          function(object, value) {
              object@protein.description <- value
              if (validObject(object))
                  return(object)
          }
)

#' Returns a protein sequence
#' 
#' @param object a ProjectDetail
#' @return the sequence
#' @author Jose A. Dianes
#' @export
setMethod("protein.sequence", "ProteinDetail", function(object) object@protein.sequence)

#' Replaces a protein sequence
#' 
#' @param object a ProteinDetail
#' @param value the sequence
#' @author Jose A. Dianes
#' @export
setReplaceMethod("protein.sequence", "ProteinDetail",
          function(object, value) {
              object@protein.sequence <- value
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
            row.names <- x@protein.accession
        # create the data frame just with the accession column
        value <- list(x@protein.accession)
        attr(value, "row.names") <- row.names
        class(value) <- "data.frame"
        names(value) <- c("protein.accession")
        # add the rest of the columns
        value$project.accession <- x@project.accession
        value$assay.accession <- x@assay.accession
        value$protein.synonyms <- paste(x@protein.synonyms, collapse=" || ")
        value$protein.description <- x@protein.description
        value$protein.sequence <- x@protein.sequence
        
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
               protein.accession = json.object$accession,
               project.accession = json.object$projectAccession,
               assay.accession = json.object$assayAccession,
               protein.synonyms = ifelse(is.null(json.object$synonyms), c(MISSING_VALUE), json.object$synonyms),
               protein.description = ifelse(is.null(json.object$description), MISSING_VALUE, json.object$description),
               protein.sequence = ifelse(is.null(json.object$sequence), MISSING_VALUE, json.object$sequence)
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
#' @importFrom rjson fromJSON
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
#' @importFrom rjson fromJSON
#' @export
count.ProteinDetail <- function(project.accession) {    
    protein.count <- fromJSON(file=URLencode(paste0(pride_archive_url, "/protein/count/project/", project.accession)), method="C")
    protein.count                          
}

format.ProteinDetail <- function(x, ...) paste0(x@accession, ", ", x@assayAccession)


