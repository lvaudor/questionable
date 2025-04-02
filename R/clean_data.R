#' Tests whether vector is free text or not
#'
#' @param x the vector
#' @param max_distinct the maximum number of distinct values to consider a column as free text. Defaults to 20.
#' @export
is_free_text=function(x,max_distinct=20){
  if(!is.character(x)){
    return(FALSE)
  }
  if(length(unique(x))>max_distinct){
    return(TRUE)
  }
  return(FALSE)
}

#' Transforms a vector for ease of use in a random forest
#'
#' @param x the vector to optimize for running a random forest
#' @param replace_NA_with the character string used to replace NA as category in categorical responses. Defaults to "Not provided"
#' @export
optimize_for_rf=function(x, replace_NA_with="Not provided"){
  if(is.numeric(x)){return(x)}
  if(is.character(x)){
    x=case_when(is.na(x)~replace_NA_with,
                TRUE~x)
    x=as.factor(x)
  }
  result=x %>%
    stringr::str_extract("^\\d*") %>%
    as.numeric()
  ind=which(is.na(result))
  #if there are too many NAs, we keep the column as is
  if(length(ind)>0.5*length(x)){
    return(x)
  }
  #if there are few NAs, we replace them with random values drawn from observed distribution
  result[ind]=sample(na.omit(result),length(ind))
  return(result)
}
