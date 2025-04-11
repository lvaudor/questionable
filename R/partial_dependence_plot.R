#' Plots the partial dependence of a random forest model response to a predictor (based on the training dataset)
#'
#' @param rf the result of a run_rf() function
#' @param predictor the name (passed as a string) of the predictor considered
#' @param probabilities if TRUE, the probabilities of the (categorical) response variable are plotted instead of the predicted values. Defaults to FALSE.
#' @return a partial dependence plot
#' @export
#' @examples
#' # example code
#' data(icecream)
#' lemon_rf=run_rf(icecream %>%
#'           dplyr::select(-id) %>%
#'           dplyr::mutate_all(optimize_for_rf),
#'           response="sorbet_citron")
#' partial_dependence_plot(lemon_rf,predictor="age")
#' partial_dependence_plot(lemon_rf,predictor="creme_glacee_vanille")
partial_dependence_plot=function(rf,predictor,probabilities=FALSE){
  response=rf$rf$predicted
  if(is.factor(response)){
    tib=tibble::tibble()
    for (i in 1:length(levels(response))){
      result=do.call(randomForest::partialPlot,
                     list(x=rf$rf,
                          pred.data=rf$datarf,
                          x.var=predictor,
                          which.class=levels(response)[i],
                          plot=FALSE))
      tib_i=tibble::tibble(level=rep(levels(response)[i],length(result$x)),
                           x=result$x,
                           y=result$y)
      tib=bind_rows(tib,tib_i)
      if(probabilities==TRUE){
        softmax <- function(z) {
          exp_z <- exp(z - max(z))  # soustraction de max(z) pour la stabilité numérique
          return(exp_z / sum(exp_z))
        }
        tib=tib %>%
          dplyr::group_by(x) %>%
          dplyr::mutate(y=softmax(y)) %>%
          dplyr::ungroup()
        ggplot2::ggplot(tib,
                          ggplot2::aes(x=x,y=y,fill=level))+
          ggplot2::geom_col()
      }
    }
    p=ggplot2::ggplot(tib,
                      ggplot2::aes(x=x,y=y,color=level))+
      ggplot2::geom_smooth()
    return(p)
  }
  result=do.call(randomForest::partialPlot,
                 list(x=rf$rf,
                      pred.data=rf$datarf,
                      x.var=predictor,
                      which.class="Oui"))
  tib=tibble::tibble(x=result$x,y=result$y)
  p=biplot(tib,"x","y")+
    ggplot2::xlab(predictor)+
    ggplot2::ylab(rf$reponse)
  return(p)
}
