
MISSING_VALUE <- "Not available"

#' ProteinDetailList represents a PRIDE Archive project proteins collection
#'
#' @importFrom rjson fromJSON
#' @export 
#' @exportClass ProteinDetailList
setClass(
  "ProteinDetailList", 
  representation(
    project.accession = "character",
    protein.list = "list",
    page.number = "numeric",
    page.size = "numeric"
  ),
  prototype(
    project.accession = "",
    protein.list = list(), 
    page.number = 0,
    page.size = 10
  )
)

setMethod("show",
          signature = "ProteinDetailList",
          definition = function(object) {
            cat("A ", class(object), sep="")
            cat(" representing the list of proteins for project ", object@project.accession, " with \n", sep="")
            cat("    Proteins in page: ", length(object@protein.list), "\n", sep="")
            cat("    Page number: ", object@page.number, " \n", sep="")
            cat("    Page size: ", object@page.size, " protein details in page\n", sep="")
            invisible(NULL)
          }
)

#' Plot function
#' 
#' @param object a ProteinDetailList instance
#' @author Jose A. Dianes
#' @export
setMethod("plot",
          "ProteinDetailList",
          function(x,y,...) {
            # Save present graphic params
            opar <- par(no.readonly=TRUE)
            
            object <- x
            results.df <- as.data.frame(object)

            # plot protein accession mappings counts
            mapped.proteins.df <- results.df[results.df$protein.synonyms!=MISSING_VALUE,]
            if (nrow(mapped.proteins.df)>0) {
              protein.counts.mapped <- sort(table(mapped.proteins.df$protein.synonyms),decreasing=TRUE)[1:min(10,nrow(mapped.proteins.df))]
              
              layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE)) 
              barplot(protein.counts.mapped, 
                      main=paste("Top Proteins in ", project.accession(object), " with known ID mappings (UniProt, ENSEMBL)", sep=""), 
                      ylab="Times reported",
                      cex.names = 0.75,
                      cex.lab   = 0.75,
                      space=1.5)
            }
            
            # plot protein accession counts
            protein.counts <- sort(table(results.df$protein.accession),decreasing=TRUE)[1:10]
            barplot(protein.counts, 
                    main=paste("Top Proteins in ", project.accession(object), " as submitted", sep=""), 
                    ylab="Times reported",
                    cex.names = 0.75,
                    cex.lab   = 0.75,
                    space=1.5)
            
            # Restore previous graphics params
            par(opar)
          }
)

#' Returns a project accession for a protein list page
#' 
#' @param object a ProteinDetailList instance
#' @return the project accession
#' @author Jose A. Dianes
#' @export
setMethod("project.accession", "ProteinDetailList", function(object) object@project.accession)

#' Returns a project protein list page as a list
#' 
#' @param object a ProteinDetailList instance
#' @return the protein list
#' @author Jose A. Dianes
#' @export
setMethod("protein.list", "ProteinDetailList", function(object) object@protein.list)

#' Returns the page number for a given protein details list
#' 
#' @param object a ProteinDetailList instance
#' @return the page number 
#' @author Jose A. Dianes
#' @export
setMethod("page.number", "ProteinDetailList", function(object) object@page.number)

#' Returns the page size for a given protein details list
#' 
#' @param object a ProteinDetailList instance
#' @return the page size 
#' @author Jose A. Dianes
#' @export
setMethod("page.size", "ProteinDetailList", function(object) object@page.size)

#' Returns a data frame from a ProteinDetailList instance
#'
#' @param x The ProteinDetailList instance
#' @param row.names optional row names
#' @param optional optional
#' @return The page of protein details as a data frame
#' @author Jose A. Dianes
#' @details TODO
#' @export
setMethod("as.data.frame", "ProteinDetailList",
          function(object, row.names=NULL, optional=FALSE, ...)
          {
            value <- list.to.data.frame(object@protein.list)
            
            return(value)
          }
)



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
        return("'protein.synonyms' must be a one or multiple valid strings")
      
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
setMethod("as.data.frame", "ProteinDetail",
    function(object, row.names=NULL, optional=FALSE, ...)
    {
        # set row names if provided
        if (is.null(row.names))
            row.names <- object@protein.accession
        # create the data frame just with the accession column
        value <- list(object@protein.accession)
        attr(value, "row.names") <- row.names
        class(value) <- "data.frame"
        names(value) <- c("protein.accession")
        # add the rest of the columns
        value$project.accession <- object@project.accession
        value$assay.accession <- object@assay.accession
        value$protein.synonyms <- paste(object@protein.synonyms, collapse=" || ")
        value$protein.description <- object@protein.description
        value$protein.sequence <- object@protein.sequence
        
        return(value)
    }
)

format.ProteinDetail <- function(x, ...) paste0(x@accession, ", ", x@assayAccession)
