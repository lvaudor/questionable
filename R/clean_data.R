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

#' Replace values improperly provided as a given string indicating missing value with NA
#'
#' @param x the vector
#' @param possible_strings the strings provided in raw table which could possibly indicate a NA value
#' @export
clean_improper_NA=function(x, possible_strings=c("N/A",
                                                 "[Nn]on [Rr]enseigné",
                                                 "[Mm]anquant",
                                                 "[Uu]nknown",
                                                 "[Nn]ot provided",
                                                 "[Mm]issing")){
  for(i in 1:length(possible_strings)){
    x[x==possible_strings[i]]=NA
  }
  #if an improper NA has made the column falsely recognized as character, convert it to numeric
  if(all(is.na(as.numeric(x)))){return(x)}else{return(as.numeric(x))}
  return(x)
}

#' Replace values improperly provided as a given string indicating missing value with NA
#'
#' @param x the vector
#' @param acceptable_proportion the proportion of missing values above which the vector is considered as mainly missing
#' @export
is_mainly_missing=function(x,acceptable_proportion=0.33){
  proportion=length(which(is.na(x)))/length(x)
  result=proportion>acceptable_proportion
  return(result)
}

#' Transforms a vector for ease of use in a random forest
#'
#' @param x the vector to optimize for running a random forest
#' @param replace_NA_with the character string used to replace NA as category in categorical responses if not replaced by random values drawn from the observed distribution. Defaults to "Not provided"
#' @param unpredictable_NA_proportion=0.2 the proportion of NAs that can be replaced by random values drawn from the observed distribution. Defaults to 0.2.
#' @export
optimize_for_rf=function(x, replace_NA_with="Not provided", unpredictable_NA_proportion=0.2){
  # If x is numeric it is returned as is
  if(is.numeric(x)){return(x)}
  # If it is character, then it could be ordinal "0_not at all","1_a bit",..."5_very much" for instance
  if(is.character(x)){
    result=x %>%
      stringr::str_extract("^\\d*") %>%
      as.numeric()
    # If it is not ordinal, result above will probably be all NA
    if(length(table(result))<2){
      result=x
    }
  }
  # Regarding NA values:
  ind=which(is.na(result))
  # If there are so many NAs that observed distribution is dubious and NAs cannot be filled randomly,
  # we consider that a NA is a category in itself
  if(length(ind)>unpredictable_NA_proportion*length(result) & is.character(result)){
    result=case_when(is.na(result)~replace_NA_with,
                     TRUE~result)
    return(result)
  }
  #If there are few NAs, we replace them with random values drawn from observed distribution
  result[ind]=sample(na.omit(result),length(ind))
  return(result)
}
