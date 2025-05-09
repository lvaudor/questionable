#
# #On définit une fonction `IDQ_to_tQ()` qui permet de **transformer les identifiants de questions en textes simplifiés**.
#
#
#
# IDQ_to_tQ=function(IDQ){
#   tQ=rep(NA,length(IDQ))
#   for (i in 1:length(IDQ)){
#     ind=which(varnames$nom==IDQ[i])
#     tQ[i]=varnames$description[ind][1]
#   }
#   return(tQ)
# }
#' Assesses the types of variables (quantitative or qualitative) for a bivariate analysis
#' @param dataset the dataset containing the variables
#' @param x the name of first variable
#' @param y the name of second variable
#' @returns the type of bivariate analysis (either quanti-quanti, quali-quanti or quali-quali)
#' @export
#' @examples
#' # example code
#' data(icecream)
#' biplot(icecream,"creme_glacee_vanille","creme_glacee_chocolat")
#' biplot(icecream,"age","creme_glacee_chocolat")
#' biplot(icecream,"age","taille")
bivar_type=function(dataset,x,y){
  modex="quanti"
  modey="quanti"
  if(mode(as.vector(dataset[[x]]))=="character"){modex="quali"}
  if(mode(as.vector(dataset[[y]]))=="character"){modey="quali"}
  type=paste0(modex,"-",modey)
  if(type=="quanti-quali"){
    z=y;y=x;x=z
    type="quali-quanti"
  }
  return(type)
}
#' Performs a plot crossing two variables, adapting to the kind of variable (qualitative or quantitative)
#' @param dataset the dataset containing the variables
#' @param x the name of first variable
#' @param y the name of second variable
#' @param in case X and Y are quantitative, the method of regression (defaults to "lm" for a linear regression, can be set to NULL for loess or GAM regressions)
#' @param add_pval defaults to TRUE, if TRUE, the p-value of the test is added to the title of the plot
#' @export
#' @examples
#' # example code
#' data(icecream)
#' biplot(icecream,"creme_glacee_vanille","creme_glacee_chocolat")
#' biplot(icecream,"creme_glacee_chocolat","age")
#' biplot(icecream,"age","taille")
#' biplot(icecream,"age","taille",method=NULL)
biplot=function(dataset,x,y, method="lm", add_pval=TRUE){
  type=bivar_type(dataset,x,y)
  dat=dataset %>%
    dplyr::select(all_of(c(x,y)))
  colnames(dat)=c("x","y")
  # quali vs quanti
  if(type=="quali-quanti"){
    datg= dat %>%
      dplyr::group_by(x) %>%
      dplyr::summarise(y=mean(y, na.rm=T))
    p=ggplot2::ggplot(dat,ggplot2::aes(x=x,y=y))+
      ggplot2::geom_boxplot(fill="yellow")+
      ggplot2::geom_point(data=datg,ggplot2::aes(x=x,y=y))+
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90))
  }
  # quali vs quali


  if(type=="quali-quali"){
    p=ggplot2::ggplot(dat,ggplot2::aes(x=x,fill=y))+
      ggplot2::geom_bar(position="fill")+
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90))
  }
  # quanti vs quanti
  if(type=="quanti-quanti"){
    p=ggplot2::ggplot(dat,ggplot2::aes(x=x,y=y))+
      ggplot2::geom_point(alpha=0.5,color="red")+
      ggplot2::geom_smooth(method=method)
    mean_number_of_individuals_by_location=dat %>%
      dplyr::group_by(x,y) %>%
      dplyr::summarise(n=dplyr::n(),.groups="drop") %>%
      dplyr::pull(n) %>%
      mean()
    if(mean_number_of_individuals_by_location>5){
     p=p+
       ggplot2::geom_jitter(alpha=0.5,
                            position=ggplot2::position_jitter(width=0.1,height=0.1))
    }
    }
  p=p+ggplot2::xlab(x)
  p=p+ggplot2::ylab(y)
  if(add_pval & !is.null(method)){
    pval=test_pval(dat,"x","y")
    print(pval)
    pvalstar=test_pvalstar(pval)
    p=p+ggplot2::ggtitle(paste0("Effect of ",x, " on ", y,": ",pvalstar))
  }
  return(p)
}
