#' Runs a random forest model
#'
#' @param data the data (optimized for running a rf)
#' @param response the name of the response variable
#' @param plot whether or not to plot the importance of predictors. Defaults to FALSE
#' @export
#' @examples
#' # example code
#' data(icecream)
#' run_rf(icecream %>%
#'           dplyr::select(-id) %>%
#'           dplyr::mutate_all(optimize_for_rf),
#'           response="sorbet_citron",
#'           plot=TRUE)
run_rf=function(data,
                response=colnames(data)[1],
                plot=FALSE,
                min_importance=1){
  data=na.omit(data)
  datarf=dplyr::select(data,-dplyr::one_of(response))
  responserf=data %>%
    dplyr::select(dplyr::one_of(response)) %>%
    dplyr::pull()
  if(is.character(responserf)){responserf=as.factor(responserf)}
  myrf=randomForest::randomForest(datarf,responserf,ntree=1000)
  impDF=data.frame(variable=rownames(myrf$importance),
                   importance=myrf$importance[,1],
                   stringsAsFactors=FALSE)
  impDF$variable=factor(impDF$variable,levels=impDF$variable[order(impDF$importance)])
  impDF$importance=impDF$importance/sum(impDF$importance)*100
  impDF=impDF %>% dplyr::arrange(desc(importance))
  if(plot==TRUE){
    impDF_keep=impDF %>%
      dplyr::filter(importance>=min_importance)
    if(nrow(impDF_keep)==0){
      impDF_keep=impDF %>%
        dplyr::slice(1:5)
      warning(paste0("All predictors have an importance <",
                     min_importance,
                     ". The top 5 predictors are plotted instead.")
    }
    p=ggplot2::ggplot(impDF_keep,
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
