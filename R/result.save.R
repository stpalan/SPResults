#' result.save
#'
#' Saves a result (a number, text, vector, list, table, plot, etc.) with proper documentation.
#' @param data the result data to be saved
#' @param id unique ID of this result. If not specified, a unique Id is randomly generated and returned.
#' @param resultvar result variable to save to
#' @param label designation of this result in the research article (e.g., 'Table 2')
#' @param type type of result, i.e., 'Regression', 'Pairwise t-Test', etc.
#' @param description longer description of the result, e.g., 'Test for a treatment effect in the pooled data.'
#' @keywords results, documentation
#' @export
#' @examples
#' Instead of writing, for example:
#' A[A$a==B$a & A$b==B$b & A$c==B$c & A$d==B$d,]
#' one can write
#' A[SPSubset(A,B,c("a","b","c","d")),]

# Testdata
data <- c(1,2,3,4,4,5)
id <- "ashg"
label <- "Table 2"
type <- "Regression"
description <- "A table consisting of three regression models exploring..."

#######################################
# Add dump of current package versions (for each result)!
#######################################

result.save <- function(data=NULL,
                        id=NA,
                        resultvar="Result",
                        label=NA,
                        type=NA,
                        description=NA,
                        id.n=4
                        ) {


  # Checks ------------------------------------------------------------------

  # if(!is.numeric(precision)) stop("precision must be numeric")
  # if(!is.numeric(conf.levels)) stop("conf.levels must be numeric")
  # if(!is.numeric(ylim)) stop("ylim must be numeric")
  # if (precision<=0 & precision <1) stop("precision must be positive and less than 1") # Checks precision is in allowed range



  # Packages ----------------------------------------------------------------

  # require(ggplot2)
  # require(tidyverse)




  # Functions ---------------------------------------------------------------

  result.new.id<-function(resultvar,n=4){

    # Defines vector of legal symbols to use
    legal.lowercase<-c("a","b","c","d","e","f","g","h","j","k","m","n","p","q","r","s","t","u","v","w","x","y","z")
    # legal.uppercase<-c("A","B","C","D","E","F","G","H","J","K","L","M","N","P","Q","R","S","T","U","V","W","X","Y","Z")
    legal.numeric<-2:9

    # Draws new random id of length n
    repeat{

      # Draws random id
      id<-paste0(sample(legal.lowercase,1),paste(sample(c(legal.lowercase,legal.numeric),size=n-1,replace=T),collapse="", sep=""))

      # Leaves loop only if id is unique
      if(!id%in%names(resultvar)) break
    }

    return(id)
  }



  # Code --------------------------------------------------------------------

  id.set=!is.na(id)

  if(exists(resultvar,envir=.GlobalEnv)){
    r.orig<-get(resultvar, envir=.GlobalEnv)
  } else {
    r.orig<-list()
  }

  if(is.na(id)) {
    id<-result.new.id(r.orig,n=id.n)
  }

  r<-list()
  r$id<-id
  r$label<-label
  r$type<-type
  r$description<-description
  r$data<-data

  r.orig[[id]]<-r

  assign(resultvar,r.orig,envir=.GlobalEnv)

  # Prints id if id was not set by user
  if(!id.set) {
    print(paste("The ID is:"))#,id))
    return(id)

  }
}
