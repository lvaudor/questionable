library(tidyverse)
n=150
values=c("0_pas_du_tout","1_un peu","2_moyennement","3_beaucoup","4_énormément")

sample_with_proportions=function(size,values,proportions){
  proportions=proportions/sum(proportions)
  result=sample(values,size=size,replace=TRUE, prob=proportions)
  return(result)
}

icecream=tibble::tibble(
  id=1:n,
  age=round(runif(n,3,80)),
  genre=sample(c(rep("homme",42),rep("femme",49),rep("non binaire",8)),size=n,replace=TRUE)) %>%
  mutate(taille=case_when(genre=="homme" & age>18~ rnorm(n(),170,5),
                          genre=="femme" & age>18~ rnorm(n(),164,5),
                          age<=18~90+(age-3)*5+rnorm(n(),5))) %>%
  mutate(sorbet_fraise=case_when(genre=="homme"~sample_with_proportions(n,values=values,proportions=c(30,20,30,20,10)),
                                 genre=="femme"~sample_with_proportions(n,values=values,proportions=c(20,20,20,20,20)),
                                 genre=="non binaire"~sample_with_proportions(n,values=values,proportions=c(20,20,20,20,20))),
         sorbet_framboise=case_when(genre=="homme"~sample_with_proportions(n,values=values,proportions=c(20,20,30,20,20)),
                                    genre=="femme"~sample_with_proportions(n,values=values,proportions=c(20,20,20,20,20)),
                                    genre=="non binaire"~sample_with_proportions(n,values=values,proportions=c(20,20,20,20,20))),
         sorbet_passion=case_when(genre=="homme"~sample_with_proportions(n,values=values,proportions=c(10,20,30,20,20)),
                                  genre=="femme"~sample_with_proportions(n,values=values,proportions=c(20,20,20,20,20)),
                                  genre=="non binaire"~sample_with_proportions(n,values=values,proportions=c(20,20,20,20,20))),
         sorbet_citron=case_when(genre=="homme"~sample_with_proportions(n,values=values,proportions=c(10,20,30,20,20)),
                                 genre=="femme"~sample_with_proportions(n,values=values,proportions=c(20,20,20,40,20)),
                                 genre=="non binaire"~sample_with_proportions(n,values=values,proportions=c(20,20,20,30,20))),
         creme_glacee_vanille=case_when(genre=="homme"~sample_with_proportions(n,values=values,proportions=c(10,20,20,20,34)),
                                        genre=="femme"~sample_with_proportions(n,values=values,proportions=c(20,20,20,20,20)),
                                        genre=="non binaire"~sample_with_proportions(n,values=values,proportions=c(20,20,20,20,20))),
         creme_glacee_chocolat=case_when(genre=="homme"~sample_with_proportions(n,values=values,proportions=c(20,20,20,20,10)),
                                         genre=="femme"~sample_with_proportions(n,values=values,proportions=c(20,20,20,20,30)),
                                         genre=="non binaire"~sample_with_proportions(n,values=values,proportions=c(20,20,20,30,20))
         ),
         creme_glacee_rhumraisins=case_when(age>50~sample_with_proportions(n,values=values,proportions=c(10,20,20,40,40)),
                                            TRUE~sample_with_proportions(n,values=values,proportions=c(30,20,20,20,20))),
         creme_glacee_caramel=case_when(genre=="homme"~sample_with_proportions(n,values=values,proportions=c(30,20,20,20,20)),
                                        genre=="femme"~sample_with_proportions(n,values=values,proportions=c(20,20,30,30,50)),
                                        genre=="non binaire"~sample_with_proportions(n,values=values,proportions=c(20,20,20,20,20)))
  )
usethis::use_data(icecream, overwrite=TRUE)

