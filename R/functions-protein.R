pride_archive_url <- "http://www.ebi.ac.uk/pride/ws/archive"
pride_archive_url_dev <- "http://wwwdev.ebi.ac.uk/pride/ws/archive"

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
             protein.synonyms = ifelse(is.null(json.object$synonyms) || (length(json.object$synonyms)==0), MISSING_VALUE, json.object$synonyms),
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
list.ProteinDetailList <- function(project.accession, page.number=0, page.size=100) {
  json.list <- fromJSON(file=paste0(pride_archive_url, "/protein/list/project/", project.accession, "?show=", page.size, "&page=", page.number), method="C")
  protein.list <- lapply(json.list[[1]], function(x) { from.json.ProteinDetail(x)})
  protein.detail.list <- new("ProteinDetailList", 
                             project.accession=project.accession, 
                             protein.list=protein.list, 
                             page.number=page.number, 
                             page.size=page.size)
  return(protein.detail.list)
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
