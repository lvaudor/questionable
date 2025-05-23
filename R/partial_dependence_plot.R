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
#' choc_rf=run_rf(icecream %>%
#'           dplyr::select(-id) %>%
#'           dplyr::mutate_all(optimize_for_rf),
#'           response="creme_glacee_chocolat")
#' partial_dependence_plot(choc_rf,predictor="age")
#' partial_dependence_plot(choc_rf,predictor="genre")
#' genre_rf=run_rf(icecream %>%
#'           dplyr::select(-id) %>%
#'           dplyr::mutate_all(optimize_for_rf),
#'           response="genre")
#' partial_dependence_plot(genre_rf,predictor="creme_glacee_vanille", probabilities=TRUE)
#' partial_dependence_plot(genre_rf,predictor="regular_eating", probabilities=TRUE)
#' iris_sepal_length_rf=run_rf(iris, response="Sepal.Length")
#' partial_dependence_plot(iris_sepal_length_rf,predictor="Species", probabilities=TRUE)
#' iris_species_rf=run_rf(iris, response="Species")
#' partial_dependence_plot(iris_species_rf,predictor="Petal.Width", probabilities=TRUE)


partial_dependence_plot=function(rf,predictor,which.class="Oui",probabilities=FALSE){
  response=rf$rf$predicted
  quantiR=!is.factor(response)
  quantiP=!is.character(rf$datarf[[predictor]]) & !is.factor(rf$datarf[[predictor]])
  ##################################################################################
  if(!quantiR & quantiP){
    #print("qualiR-quantiP")
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
            tib=dplyr::bind_rows(tib,tib_i)
          }
            if(probabilities==TRUE){
              softmax <- function(z) {
                exp_z <- exp(z - max(z))  # soustraction de max(z) pour la stabilité numérique
                return(exp_z / sum(exp_z))
              }
              tib=tib %>%
                dplyr::group_by(x) %>%
                dplyr::mutate(y=softmax(y)) %>%
                dplyr::ungroup()
              p=ggplot2::ggplot(tib,
                                ggplot2::aes(x=x,y=y,fill=level))+
                ggplot2::geom_col()
            }else{
              p=ggplot2::ggplot(tib,
                                ggplot2::aes(x=x,y=y,color=level))+
                ggplot2::geom_smooth()
            }

    }# end qualiR-quantiP
    ############################################################################
    if(!quantiR & !quantiP){
      #print("qualiR-qualiP")
      tib=tibble::tibble()
      for (i in 1:length(levels(response))){
        result=do.call(pdp::partial,
                       list(object=rf$rf,
                            pred.var=predictor,
                            train=rf$datarf,
                            which.class=levels(response)[i]))
        colnames(result)=c("x","y")
        tib_i=tibble::tibble(level=rep(levels(response)[i],length(result$x)),
                             x=result$x,
                             y=result$y)
        tib=dplyr::bind_rows(tib,tib_i)
      }
      if(probabilities==TRUE){
        softmax <- function(z) {
          exp_z <- exp(z - max(z))  # soustraction de max(z) pour la stabilité numérique
          return(exp_z / sum(exp_z))
        }
        tib=tib %>%
          dplyr::group_by(x) %>%
          dplyr::mutate(y=softmax(y)) %>%
          dplyr::ungroup()
        p=ggplot2::ggplot(tib,
                          ggplot2::aes(x=x,y=y,fill=level))+
          ggplot2::geom_col()
      }else{
        p=ggplot2::ggplot(tib,
                          ggplot2::aes(x=x,y=y,color=level))+
          ggplot2::geom_point()
      }
  } # end qualiR-qualiP
  ##############################################################################
  if(quantiR & quantiP){
          #print("quantiR-quantiP")
          p=ggplot2::ggplot(tib,
                            ggplot2::aes(x=x,y=y,color=level))+
            ggplot2::geom_smooth()+
            ggplot2::xlab(predictor)+
            ggplot2::ylab(rf$response)
          result=do.call(randomForest::partialPlot,
                         list(x=rf$rf,
                              pred.data=rf$datarf,
                              x.var=predictor,
                              which.class=which.class))
          tib=tibble::tibble(x=result$x,y=result$y)
          p=biplot(tib,"x","y")
      }
  # end quantiR-quantiP
  ##############################################################################
  if(quantiR & !quantiP){

    #print("quantiR-qualiP")
    result=pdp::partial(rf$rf,
                        pred.var = predictor,
                        train=rf$datarf
                        )
    colnames(result)=c("x","y")
    p=ggplot2::ggplot(data=result,
                      ggplot2::aes(x=x,y=y, col=x))+
      ggplot2::geom_point()
  } # end quantiR-quantiP
  p=p+
    ggplot2::xlab(predictor)+
    ggplot2::ylab(rf$response)
  return(p)
}
