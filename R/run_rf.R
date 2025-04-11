#' Runs a random forest model
#'
#' @param data the data (optimized for running a rf)
#' @param response the name of the response variable
#' @export
#' @examples
#' # example code
#' data(icecream)
#' run_rf(icecream %>%
#'           select(-id) %>%
#'           mutate_all(optimize_for_rf),
#'           response="sorbet_citron")
run_rf=function(data,response=colnames(data)[1],plot=FALSE,clean_name=FALSE){
  data=na.omit(data)
  datarf=dplyr::select(data,-dplyr::one_of(response))
  responserf=data %>%
    dplyr::select(dplyr::one_of(response)) %>%
    dplyr::pull()
  if(is.character(responserf)){responserf=as.factor(responserf)}
  myrf=randomForest::randomForest(datarf,responserf,ntree=10000)
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
      ggplot2::ggtitle(paste0("Prediction of \n",response))+
      ggplot2::xlab("Predictors")+
      ggplot2::theme(plot.title=ggplot2::element_text(hjust=1))
    plot(p)
  }
  if("err.rate" %in% names(myrf)){
    perf=1-myrf$err.rate[1000,1]
  }
  if("rsq" %in% names(myrf)){
    perf=myrf$rsq[1000]
  }
  return(list(rf=myrf,
              response=response,
              datarf=as.data.frame(datarf),
              impDF=impDF, performance=perf))
}
