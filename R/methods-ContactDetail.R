#' Constructor for ContactDetail
#' 
#' @param title the title of the contact
#' @param first.name the first name of the contact
#' @param last.name the last name of the contact
#' @param affiliation the affiliation of the contact
#' @export
ContactDetail <- function(title, first.name, last.name, email, affiliation) {
  new("ContactDetail", 
      title = title, 
      first.name = first.name, 
      last.name = last.name, 
      email = email, 
      affiliation = affiliation)
}

#' print out the details of the ContactDetail on screen
#' 
#' @param object a ContactDetail object
#' @export
setMethod("show",
          signature = "ContactDetail",
          definition = function(object) {
            cat("An object of class ", class(object), "\n", sep="")
            cat("    Title: ", object@title, "\n", sep="")
            cat("    First name: ", object@first.name, "\n", sep="")
            cat("    Last name: ", object@last.name, "\n", sep="")
            cat("    Email: ", object@email, "\n", sep="")
            cat("    Affiliation: ", object@affiliation, "\n", sep="")
            invisible(NULL)
          }
)

#' Returns the title of a contact
#' 
#' @param object a ContactDetail
#' @return the title
#' @export
setMethod("title", "ContactDetail", function(object) object@title)

#' Replaces the title of a contact
#' 
#' @param object a ContactDetail
#' @param value the new title
#' @export
setReplaceMethod("title", "ContactDetail",
                 function(object, value) {
                   object@title <- value
                   if (validObject(object)) {
                     return(object)   
                   }
                 }
)

#' Returns the first name of a contact
#' 
#' @param object a ContactDetail
#' @return the first name
#' @export
setMethod("first.name", "ContactDetail", function(object) object@first.name)

#' Replaces the first name of a contact
#' 
#' @param object a ContactDetail
#' @param value the new first name
#' @export
setReplaceMethod("first.name", "ContactDetail",
                 function(object, value) {
                   object@first.name <- value
                   if (validObject(object)) {
                     return(object)   
                   }
                 }
)

#' Returns the last name of a contact
#' 
#' @param object a ContactDetail
#' @return the last name
#' @export
setMethod("last.name", "ContactDetail", function(object) object@last.name)

#' Replaces the last name of a contact
#' 
#' @param object a ContactDetail
#' @param value the new last name
#' @export
setReplaceMethod("last.name", "ContactDetail",
                 function(object, value) {
                   object@last.name <- value
                   if (validObject(object)) {
                     return(object)   
                   }
                 }
)

#' Returns the email of a contact
#' 
#' @param object a ContactDetail
#' @return the email
#' @export
setMethod("email", "ContactDetail", function(object) object@email)

#' Replaces the email of a contact
#' 
#' @param object a ContactDetail
#' @param value the new email
#' @export
setReplaceMethod("email", "ContactDetail",
                 function(object, value) {
                   object@email <- value
                   if (validObject(object)) {
                     return(object)   
                   }
                 }
)

#' Returns the affiliation of a contact
#' 
#' @param object a ContactDetail
#' @return the affiliation
#' @export
setMethod("affiliation", "ContactDetail", function(object) object@affiliation)

#' Replaces the affiliation of a contact
#' 
#' @param object a ContactDetail
#' @param value the new affiliation
#' @export
setReplaceMethod("affiliation", "ContactDetail",
                 function(object, value) {
                   object@affiliation <- value
                   if (validObject(object)) {
                     return(object)   
                   }
                 }
)