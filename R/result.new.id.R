#' result.new.id
#'
#' Draws a new, unique, random result ID.
#' @param id.n The number of random ID characters.
#' @param resultvar a character vector containing the name of the result
#' variable to check for ID uniqueness
#' @param use_lowercase a logical value indicating whether lower case letters 
#' should be used for the ID
#' @param use_uppercase a logical value indicating whether upper case letters 
#' should be used for the ID
#' @param use_numeric a logical value indicating whether numeric digits' should
#' be used for the ID
#' @keywords results, unique ID
#' @export
#' @examples
#' # Create a new, random, 4-character ID that is unique among all IDs in the
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


result.new.id <- function(id.n = 4, resultvar = "Result", use_lowercase = T, use_uppercase = F, use_numeric = T){
  
  # Checks ------------------------------------------------------------------
  
  # if(use_lowercase | use_uppercase | use_numeric == F) stop("you need to allow at least one type of symbol (lowercase, uppercase, numeric)")
  
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
