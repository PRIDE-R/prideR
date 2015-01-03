MISSING_VALUE <- "Not available"

#' FileDetail represents a PRIDE Archive file
#'
#' @importFrom rjson fromJSON
#' @import methods
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