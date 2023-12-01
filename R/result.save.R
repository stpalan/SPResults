#' result.save
#'
#' Saves a result (a number, text, vector, list, table, plot, etc.) with proper documentation.
#' @param data the result data to be saved
#' @param id unique ID of this result. If not specified, a unique Id is randomly generated and returned.
#' @param resultvar result variable to save to
#' @param label designation of this result in the research article (e.g., 'Table 2')
#' @param type type of result, i.e., 'Regression', 'Pairwise t-Test', etc.
#' @param description longer description of the result, e.g., 'Test for a treatment effect in the pooled data.'
#' @param id.n the number of random ID characters.
#' @param use_lowercase a logical value indicating whether lower case letters 
#' should be used for the ID
#' @param use_uppercase a logical value indicating whether upper case letters 
#' should be used for the ID
#' @param use_numeric a logical value indicating whether numeric digits' should
#' be used for the ID
#' @keywords results, documentation
#' @export
#' @examples
#' result.save(table(x),
#'             label = "Table 1",
#'             type = "Summary table",
#'             description = "Table listing the experimental conditions.")


#######################################
# Add dump of current package versions (for each result)!
#######################################

result.save <- function(data = NULL,
                        id = NA,
                        resultvar = "Result",
                        label = NA,
                        type = NA,
                        description = NA,
                        id.n = 4,
                        use_lowercase = T,
                        use_uppercase = F,
                        use_numeric = T
                        ) {

  
  
  # Checks ------------------------------------------------------------------

  if (is.null(data)) stop("you need to provide data to save")



  # Packages ----------------------------------------------------------------
  
  require(SPResults)
  
  
  
  # Code --------------------------------------------------------------------
  
  # Check if user provided an ID
  id.set <- !is.na(id)
  
  # Check if results object exists, otherwise create it
  if (exists(resultvar, envir = .GlobalEnv)) {
    r.orig <- get(resultvar, envir = .GlobalEnv)
  } else {
    r.orig <- list()
    r.orig[["Summary"]]<-list()
    r.orig[["Summary"]][["lastUpdate"]] <- Sys.time()
    r.orig[["Summary"]][["sessionInfo"]] <- sessionInfo()
  }
  
  # Generates ID if none was handed over
  if (is.na(id)) {
    id <- result.new.id(id.n = id.n, resultvar = r.orig, use_lowercase = use_lowercase, use_uppercase = use_uppercase, use_numeric = use_numeric)
  }
  
  # Prepares temporary object for current result
  r <- list()
  r$id <- id
  r$label <- label
  r$type <- type
  r$description <- description
  r$data <- data
  r$timestamp <- Sys.time()
  if(version_info_verbose){r$sessionInfo <- sessionInfo()}
  
  # Saves result to results object
  r.orig[[id]] <- r
  r.orig[["Summary"]][["LastUpdate"]]<-Sys.time()
  
  # Checks package version information, warns in case of changes and updates
  if(!identical(r.orig[["Summary"]][["sessionInfo"]],sessionInfo())){
    warning("Detected change in loaded packages since the last result was added.")
    r.orig[["Summary"]][["sessionInfo"]]<-sessionInfo()
  }
  
  # Assigns updated results object back to global environment
  assign(resultvar, r.orig, envir = .GlobalEnv)

  # Prints ID if ID was not set by user
  if (!id.set) {
    print(paste("The ID is:"))
    return(id)
  }
}