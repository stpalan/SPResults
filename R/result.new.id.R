#' result.new.id
#'
#' Draws a new, unique, random result ID.
#' @param n number of random ID characters
#' @param resultvar result variable to check for ID uniqueness
#' @keywords results, unique ID
#' @export
#' @examples
#' result.new.id()
#' result.new.id(n = 4, resultvar = "Result")
#' Creates a new, random, 4-character ID that is unique among all IDs in the 'Result' results object
#' result.new.id(6, resultvar = "main", use_uppercase = T, use_numeric = F)
#' Creates a new, random, 6-character ID that is unique among all IDs in the 'main' results object

result.new.id<-function(n=4,resultvar,use_lowercase=T,use_uppercase=F,use_numeric=T){
  
  # Checks ------------------------------------------------------------------
  
  # if(use_lowercase | use_uppercase | use_numeric == F) stop("you need to allow at least one type of symbol (lowercase, uppercase, numeric)")
  
  # Defines vectors of legal symbols to use to avoid symbols that may look alike in some fonts
  legal.lowercase<-c("a","b","c","d","e","f","g","h","i","j","k","m","n","p","q","r","s","t","u","v","w","x","y","z")
  legal.uppercase<-c("A","B","C","D","E","F","G","H","J","K","L","M","N","P","Q","R","S","T","U","V","W","X","Y","Z")
  legal.numeric<-2:9
  legal.all<-c(if(use_lowercase){legal.lowercase}else{NULL},if(use_uppercase){legal.uppercase}else{NULL},if(use_numeric){legal.numeric}else{NULL})
  
  # Draws new random id of length n
  repeat{
    
    # Draws random id
    id<-paste0(sample(legal.lowercase,1),paste(sample(legal.all,size=n-1,replace=T),collapse="", sep=""))
    
    # Leaves loop only if id is unique
    if(!id%in%names(resultvar)) break
  }
  
  return(id)
}
