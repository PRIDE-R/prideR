pride_archive_url <- "http://www.ebi.ac.uk/pride/ws/archive"
pride_archive_url_dev <- "http://wwwdev.ebi.ac.uk/pride/ws/archive"

MISSING_VALUE <- "Not available"

#' ContactDetail represents a PRIDE Archive assay
#'
#' @importFrom rjson fromJSON
#' @export 
#' @exportClass ContactDetail
setClass(
    "ContactDetail", 
    representation(
        email = "character"
    ),
    prototype(
        email = MISSING_VALUE
    )
)


#' Returns a ContactDetail instance from a JSON string representation
#'
#' @param json_str The JSON object
#' @param file the name of a file to read the json_str from; this can also be a URL. Only one of json_str or file must be supplied.
#' @param method use the C implementation, or the older slower (and one day to be depricated) R implementation
#' @param unexpected.escape changed handling of unexpected escaped characters. Handling value should be one of "error", "skip", or "keep"; on unexpected characters issue an error, skip the character, or keep the character
#' @return The ContactDetail instance
#' @author Jose A. Dianes
#' @details TODO
#' @importFrom rjson fromJSON
#' @export
from.json.ContactDetail <- function(json.object) {
    res <- new("ContactDetail",
               email = json.object$email
    )
    
    return (res)
}