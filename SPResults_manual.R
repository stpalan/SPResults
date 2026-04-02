result.new.id <- # Create a new, random ID that is unique among all IDs in the results object
  
  # Example:
  #' Create a new, random, 4-character ID that is unique among all IDs in the
  #' 'Result' results object:
  #' 
  #' result.new.id()
  #' result.new.id(id.n = 4, resultvar = "Result")
  #' 
  #' 
  #' # Creates a new, random, 6-character ID that uses lower case and upper case
  #' letter but no digits and is unique among all IDs in the 'main' results
  #' object:
  #' 
  #' result.new.id(6, resultvar = "main", use_uppercase = T, use_numeric = F)
  
  function(id.n = 4, # The number of random ID characters.
           resultvar = "Result", # a character vector containing the name of the result variable to check for ID uniqueness
           use_lowercase = T, # a logical value indicating whether lower case letters should be used for the ID
           use_uppercase = F, # a logical value indicating whether upper case letters should be used for the ID
           use_numeric = T # logical value indicating whether numeric digits' should be used for the ID
  ){
    
    # Checks ------------------------------------------------------------------
    
    if(use_lowercase ==F & use_uppercase == F & use_numeric == F) stop("you need to allow at least one type of symbol (lowercase, uppercase, numeric)")
    
    # Defines vectors of legal symbols to use to avoid symbols that may look alike in some fonts
    legal.lowercase <- c("a","b","c","d","e","f","g","h","i","j","k","m","n","p","q","r","s","t","u","v","w","x","y","z")
    legal.uppercase <- c("A","B","C","D","E","F","G","H","J","K","L","M","N","P","Q","R","S","T","U","V","W","X","Y","Z")
    legal.numeric <- 2:9
    legal.all <- c(if (use_lowercase) {legal.lowercase} else {NULL},
                   if (use_uppercase) {legal.uppercase} else {NULL},
                   if (use_numeric) {legal.numeric} else {NULL})
    
    # Draws new random id of length id.n
    repeat{
      
      # Draws random id
      id <- paste0(sample(legal.lowercase, 1), paste(sample(legal.all, size = id.n - 1, replace = T), collapse = "", sep = ""))
      
      # Leaves loop only if id is unique
      if (!id %in% names(resultvar)) break
    }
    
    return(id)
  }



result.save <- # Saves a result (a number, text, vector, list, table, plot, etc.) with proper documentation.
  
  # Example:
  #' result.save(table(x),
  #'             label = "Table 1",
  #'             type = "Summary table",
  #'             description = "Table listing the experimental conditions.")

  function(data = NULL, #the result data to be saved
                        id = NA, #unique ID of this result. If not specified, a unique Id is randomly generated and returned.
                        resultvar = "Result", #result variable to save to
                        label = NA, # designation of this result in the research article (e.g., 'Table 2')
                        type = NA, # type of result, i.e., 'Regression', 'Pairwise t-Test', etc.
                        description = NA, # longer description of the result, e.g., 'Test for a treatment effect in the pooled data.'
                        version_info_verbose = F, # logical value indicating whether to save the full sessionInfo for each result (TRUE) or only in the summary of the results object (FALSE)
                        id.n = 4, # the number of random ID characters.
                        use_lowercase = T, # a logical value indicating whether lower case letters should be used for the ID
                        use_uppercase = F, # a logical value indicating whether upper case letters should be used for the ID
                        use_numeric = T # logical value indicating whether numeric digits' should be used for the ID
                        ) {

  
  
  # Checks ------------------------------------------------------------------

  if (is.null(data)) stop("you need to provide data to save")



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