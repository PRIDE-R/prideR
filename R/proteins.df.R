
pride_archive_url <- "http://www.ebi.ac.uk/pride/ws/archive"
pride_archive_url_dev <- "http://wwwdev.ebi.ac.uk/pride/ws/archive"


#' Returns the number of proteins for a particual public project
#'
#' @param project.accession The project accession to count proteins from
#' @return The count of proteins
#' @author Jose A. Dianes
#' @details TODO
#' @export
#' @importFrom rjson fromJSON
protein_count <- function(project.accession) {    
  protein.count <- fromJSON(file=URLencode(paste0(pride_archive_url_dev, "/protein/count/project/", project.accession)), method="C")
  protein.count                          
}

#' Retrieves the list of proteins for a given PRIDE projects
#'
#' @param project.accession The project accession to list proteins from
#' @param count The maximum number of proteins to retrieve in the list
#' @return The list of proteins in a data frame
#' @author Jose A. Dianes
#' @details TODO
#' @export
#' @importFrom rjson fromJSON
protein_list <- function(project.accession, count=10) {
  pride.json <- fromJSON(file=URLencode(paste0(pride_archive_url_dev, "/protein/list/project/", project.accession, "?show=", count)), method="C")
  proteins.df <- fromJsonListToDataFrame(pride.json$list)
  proteins.df
}

