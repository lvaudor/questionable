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

#' Performs a plot crossing two variables, adapting to the kind of variable (qualitative or quantitative)
#'
#' @param x the name of first variable
#' @param y the name of second variable
#' @export
#' @examples
#' # example code
#' data(icecream)
#' biplot(icecream,"creme_glacee_vanille","creme_glacee_chocolat")
#' biplot(icecream,"age","creme_glacee_chocolat")
#' biplot(icecream,"age","taille")
biplot=function(dataset,x,y){
  modex="quanti"
  modey="quanti"
  if(mode(as.vector(dataset[[x]]))=="character"){modex="quali"}
  if(mode(as.vector(dataset[[y]]))=="character"){modey="quali"}
  type=paste0(modex,modey)
  if(type=="quantiquali"){
    z=y;y=x;x=z
    type="qualiquanti"
  }
  dat=select(dataset,all_of(c(x,y)))
  colnames(dat)=c("x","y")
  # quali vs quanti
  if(type=="qualiquanti"){
    datg= dat %>% group_by(x) %>% summarise(y=mean(y, na.rm=T))
    p=ggplot(dat,aes(x=x,y=y))+
      geom_boxplot(fill="yellow")+
      geom_point(data=datg,aes(x=x,y=y))+
      theme(axis.text.x=element_text(angle=90))
  }
  # quali vs quali
  if(type=="qualiquali"){
    p=ggplot(dat,aes(x=x,fill=y))+
      geom_bar(position="fill")+
      theme(axis.text.x=element_text(angle=90))
  }
  # quanti vs quanti
  if(type=="quantiquanti"){
    p=ggplot(dat,aes(x=x,y=y))+geom_point(alpha=0.5,color="red")+geom_smooth()
    mean_number_of_individuals_by_location=dat %>%
      group_by(x,y) %>%
      summarise(n=n(),.groups="drop") %>%
      pull(n) %>%
      mean()
    if(mean_number_of_individuals_by_location>5){
     p=p+geom_jitter(alpha=0.5,position=position_jitter(width=0.1,height=0.1))
    }
    }
  p=p+xlab(x)
  p=p+ylab(y)
  return(p)
}
