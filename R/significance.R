#' Tests whether vector is free text or not
#'
#' @param x the vector to optimize for running a random forest
#' @param replace_NA_with the character string used to replace NA as category in categorical responses. Defaults to "Not provided"
#' @export
#' @examples
#' # example code
#' data(icecream)
#' test_pval(icecream,"age","creme_glacee_rhumraisins")
test_pval=function(dataset,x,y){
  modex="quanti"
  modey="quanti"
  x=dataset[[x]]
  y=dataset[[y]]
  if(mode(x)=="character"){modex="quali"}
  if(mode(y)=="character"){modey="quali"}
  type=paste0(modex,modey)
  if(type=="quantiquali"){
    z=y;y=x;x=z
    type="qualiquanti"
  }
  # quali vs quanti
  if(type=="qualiquanti"){
    pvalue=anova(lm(y~x+0))[1,5]
  }
  # quali vs quali
  if(type=="qualiquali"){
    pvalue=chisq.test(table(x,y))$p.value
  }
  # quanti vs quanti
  if(type=="quantiquanti"){
    pvalue=anova(lm(y~x))[1,5]
  }
  return(pvalue)
}

#' Replace p-value with stars (***:<0.001, **:<0.01, *:<0.05, .:<0.1, not significant:>0.1)
#' @param pvalue the p-value to replace with stars
#' @export
#' @examples
#' # example code
#' data(icecream)
#' pvalue=test_pval(icecream,"age","creme_glacee_rhumraisins")
#' test_pvalstar(pvalue)
test_pvalstar=function(pvalue){
  pvaluestar=cut(pvalue,c(0,0.001,0.01,0.05,0.1,1), include.lowest=TRUE)
  message("***:<0.001, **:<0.01, *:<0.05, .:<0.1, not significant:>0.1")
  levels(pvaluestar)=c("***","**","*",".","not significant")
  pvaluestar=as.vector(pvaluestar)
  return(pvaluestar)
}

#' Show all two-by-two test results for a dataset
#' @param dataset the dataset with all variables to test two-by-two
#' @param var_reorder whether to reorder variables so as to have the ones with the most significant two-by-two relationships to other variables on top. Defaults to FALSE.
#' @return a plot with all relationships two
#' @export
#' @examples
#' # example code
#' data(icecream)
#' pval_graph(icecream %>% dplyr::select(-comment,-id))
pval_graph=function(dataset, var_reorder=FALSE){
  vars=colnames(dataset)
  names=stringr::str_match(vars,"([[:alpha:]]*)(\\d*)(.*)")
  names=names[order(names[,2],as.numeric(names[,3]),names[,4]),]
  vars=names[,1]

  datpval=data.frame(expand.grid(vars,vars,
                                 stringsAsFactors=FALSE),
                     pval=NA)

  for (i in 1:nrow(datpval)){
    namex=datpval$Var1[i]
    namey=datpval$Var2[i]
    if(namex!=namey){
      datpval$pval[i]=test_pval(dataset,namex,namey)
    }
  }
  datpval$pvalcat=cut(datpval$pval, c(0,0.001,0.01,0.05,0.1,1),include.lowest=TRUE)
  if(var_reorder==TRUE){
    varorder=datpval %>%
      dplyr::mutate(pval=case_when(is.na(pval)~0,
                            TRUE~pval)) %>%
      dplyr::group_by(Var1) %>%
      dplyr::summarise(spval=sum(pval), .groups="drop") %>%
      dplyr::arrange(spval) %>%
      dplyr::pull(Var1)
  }else(varorder=unique(datpval$Var1))
  p=ggplot2::ggplot(datpval,
                    ggplot2::aes(x=forcats::fct_relevel(Var1,varorder),
                       y=forcats::fct_relevel(Var2,rev(varorder)),
                       fill=pvalcat))+
    ggplot2::geom_tile(alpha=0.5)+
    ggplot2::scale_fill_manual(values =c("[0,0.001]"="dodgerblue4",
                                "(0.001,0.01]"="dodgerblue3",
                                "(0.01,0.05]"="dodgerblue2",
                                "(0.05,0.1]"="dodgerblue1",
                                "(0.1,1]"="orange"))+
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=45, hjust=1))+
    ggplot2::xlab("")+ggplot2::ylab("")
  return(p)
}

#
# datpval
#
# namesvar=unique(datpval$Var1)
#
#
#   pull(pval) %>%
#   matrix(nrow=sqrt(nrow(datpval))) %>%
#   magrittr::set_rownames(namesvar) %>%
#   magrittr::set_colnames(namesvar)
# varclust=hclust(as.dist(truc))
# p=ggplot(datpval,aes(x=forcats::fct_relevel(Var1, varorder),
#                      y=forcats::fct_relevel(Var2, varorder),
#                      fill=pvalcat))+geom_tile()+
#   scale_fill_manual(values=c(paste0("dodgerblue",c("",1:3)),"orange"))+
#   theme(axis.text.x=element_text(angle=45,hjust=1))
# plot(p)
#
