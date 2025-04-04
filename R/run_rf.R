#' Tests whether vector is free text or not
#'
#' @param x the vector to optimize for running a random forest
#' @param replace_NA_with the character string used to replace NA as category in categorical responses. Defaults to "Not provided"
#' @export
#' @examples
#' # example code
#' data(icecream)
#' run_rf(icecream %>%
#'           select(-id) %>%
#'           mutate_all(optimize_for_rf),
#'           reponse="sorbet_citron")
run_rf=function(data,reponse=colnames(data)[1],plot=FALSE,clean_name=FALSE){
  data=na.omit(data)
  datarf=dplyr::select(data,-dplyr::one_of(reponse))
  reponserf=dplyr::select(data,dplyr::one_of(reponse)) %>% pull()
  myrf=randomForest::randomForest(datarf,reponserf,ntree=1000)
  impDF=data.frame(variable=rownames(myrf$importance),
                   importance=myrf$importance[,1],
                   stringsAsFactors=FALSE)
  impDF$variable=factor(impDF$variable,levels=impDF$variable[order(impDF$importance)])
  impDF$importance=impDF$importance/sum(impDF$importance)*100
  impDF=impDF %>% dplyr::arrange(desc(importance))
  if(plot==TRUE){
    p=ggplot2::ggplot(impDF %>% head(20),
                      ggplot2::aes(x=variable, y=importance))+
      ggplot2::geom_point()+
      ggplot2::coord_flip()+
      ggplot2::ggtitle(stringr::str_c("Prédiction de \n"))+
      xlab("Variables explicatives")+
      ggplot2::theme(plot.title=element_text(hjust=1))
    plot(p)
  }
  if("err.rate" %in% names(myrf)){
    perf=1-myrf$err.rate[1000,1]
  }
  if("rsq" %in% names(myrf)){
    perf=myrf$rsq[1000]
  }
  return(list(datarf=as.data.frame(datarf),rf=myrf, impDF=impDF, performance=perf))
}
