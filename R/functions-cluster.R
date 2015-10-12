pride_cluster_url <- "http://www.ebi.ac.uk/pride/ws/cluster"

#' Returns a Cluster instance from a JSON string representation
#'
#' @param json_str The JSON object
#' @param file the name of a file to read the json_str from; this can also be a URL. Only one of json_str or file must be supplied.
#' @param method use the C implementation, or the older slower (and one day to be depricated) R implementation
#' @param unexpected.escape changed handling of unexpected escaped characters. Handling value should be one of "error", "skip", or "keep"; on unexpected characters issue an error, skip the character, or keep the character
#' @return The Cluster instance
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
#' @export
from.json.Cluster <- function(json.object) {
  res <- new("Cluster",
             id = as.character(json.object$id),
             average.precursor.mz = json.object$averagePrecursorMz,
             average.precursor.charge = json.object$averagePrecursorCharge,
             num.spectra = json.object$numberOfSpectra,
             max.ratio = json.object$maxRatio,
             peptide.sequence = json.object$sequence,
             protein.accession = json.object$proteinAccession,
             cluster.quality = json.object$clusterQuality
  )
  
  return (res)
}

#' Returns a ClusterSearchResults instance from a JSON string representation
#'
#' @param json_str The JSON object
#' @param file the name of a file to read the json_str from; this can also be a URL. Only one of json_str or file must be supplied.
#' @param method use the C implementation, or the older slower (and one day to be depricated) R implementation
#' @param unexpected.escape changed handling of unexpected escaped characters. Handling value should be one of "error", "skip", or "keep"; on unexpected characters issue an error, skip the character, or keep the character
#' @return The ClusterSearchResults instance
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
#' @export
from.json.ClusterSearchResults <- function(json.object) {
  res <- new("ClusterSearchResults",
             results = lapply(json.object$results, function(x) { from.json.Cluster(x)}),
             total.results = json.object$totalResults,
             page.number = json.object$pageNumber,
             page.size = json.object$pageSize
  )
  
  return (res)
}

#' Returns a PRIDE Cluster cluster
#'
#' @param id The cluster id
#' @return The cluster as object
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
#' @export
get.Cluster <- function(id) {
  from.json.Cluster(fromJSON(file=paste0(pride_cluster_url, "/cluster/", id), method="C"))
}

#' Returns a ClusterSearchResults instance to satisify a given peptide query. 
#'
#' @param q The query terms
#' @param page the page number
#' @param size maximum number of results per page
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
#' @export
search.ClusterSearchResults <- function(q="", page=0,size=10) {
  json.object <- fromJSON(file=paste0(pride_cluster_url, "/cluster/list", "?page=", page, "&size=", size,"&q=", q), method="C")
  results <- from.json.ClusterSearchResults(json.object)
  results@query <- q
  return(results)
}

#' Returns a ClusterSearchResults instance to satisify a given spectral query. 
#'
#' @param precursor The input precursor MZ
#' @param peaks The input spectrum peak list
#' @param page the page number
#' @param size maximum number of results per page
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
#' @export
spectral.search.ClusterSearchResults <- function(precursor,peaks,page=0,size=10) {
  peaks_escape <- gsub(" ", "%20", peaks)
  json.object <- fromJSON(file=paste0(pride_cluster_url, "/cluster/nearest", "?precursor=", precursor, "&peaks=", peaks_escape, "&page=", page, "&size=", size), method="C")
  results <- from.json.ClusterSearchResults(json.object)
  results@precursor <- precursor
  results@peaks <- peaks
  return(results)
}

