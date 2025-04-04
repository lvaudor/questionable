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

test_pvalstar=function(x,y){
  pvalue=test_pval(x,y)
  pvaluestar=cut(pvalue,c(0,0.001,0.01,0.05,0.1,1))
  levels(pvaluestar)=c("***","**","*",".","x")
  pvaluestar=as.vector(pvaluestar)
  return(pvaluestar)
}


pval_graph=function(data){
  vars=colnames(data)
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
      datpval$pval[i]=test_pval(data[[namex]],data[[namey]])
    }
  }
  datpval$pvalcat=cut(datpval$pval, c(0,0.001,0.01,0.05,0.1,1),include.lowest=TRUE)
  datpval$Var1=factor(datpval$Var1,levels=vars)
  datpval$Var2=factor(datpval$Var2,levels=vars)
  p=ggplot(datpval,aes(x=Var1,y=Var2, fill=pvalcat))+geom_tile()+
    scale_fill_manual(values=c(paste0("dodgerblue",c("",1:3)),"orange"))+
    theme(axis.text.x=element_text(angle=-90))
  plot(p)
}




