library(tidyverse)
n=1500
values=c("0_pas_du_tout","1_un peu","2_moyennement","3_beaucoup","4_énormément")

#sample size values with proportions
swp=function(size,values,proportions){
  proportions=proportions/sum(proportions)
  result=sample(values,size=size,replace=TRUE, prob=proportions)
  return(result)
}
randomly_introduce_values=function(x,value,proportion=0.1){
  n=round(proportion*length(x))
  ind=sample(1:length(x),n)
  x[ind]=value
  return(x)
}

icecream=tibble::tibble(
  id=1:n,
  age=round(runif(n,3,80)),
  genre=sample(c(rep("homme",42),rep("femme",49),rep("non binaire",8)),size=n,replace=TRUE)) %>%
  mutate(taille=case_when(genre=="homme" & age>18~ rnorm(n(),170,5),
                          genre=="femme" & age>18~ rnorm(n(),164,5),
                          age<=18~90+(age-3)*5+rnorm(n(),5))) %>%
  mutate(regular_eating=case_when(age<20 & genre!="femme"~swp(n,c("Oui","Non"),c(90,10)),
                                  age>20 & genre!="femme"~swp(n,c("Oui","Non"),c(60,40)),
                                  age<20 & genre=="homme"~swp(n,c("Oui","Non"),c(90,10)),
                                  age>20 & genre=="homme"~swp(n,c("Oui","Non"),c(30,70)),
                                  TRUE~swp(n,c("Oui","Non"),c(30,70)))) %>%
  mutate(sorbet_fraise=case_when(genre=="homme"~swp(n,values,c(30,20,30,20,10)),
                                 genre=="femme"~swp(n,values,c(20,20,20,20,20)),
                                 genre=="non binaire"~swp(n,values,c(20,20,20,20,20))),
         sorbet_framboise=case_when(genre=="homme"~swp(n,values,c(20,20,30,20,20)),
                                    genre=="femme"~swp(n,values,c(20,20,20,20,20)),
                                    genre=="non binaire"~swp(n,values,c(20,20,20,20,20))),
         sorbet_passion=case_when(age<20~swp(n,values,c(50,30,10,10,0)),
                                  age>20~swp(n,values,c(20,30,20,20,20))),
         sorbet_citron=swp(n,values,c(40,20,20,20,10)),
         creme_glacee_vanille=swp(n,values,c(10,20,20,30,30)),
         creme_glacee_chocolat=swp(n,values,c(10,20,20,30,30)),
         creme_glacee_rhumraisins=case_when(age>50~swp(n,values,c(10,20,20,40,40)),
                                            TRUE~swp(n,values,c(30,20,20,20,20))),
         creme_glacee_caramel=case_when(genre=="homme"~swp(n,values,c(30,20,20,20,20)),
                                        genre=="femme"~swp(n,values,c(20,20,30,30,50)),
                                        genre=="non binaire"~swp(n,values,c(20,20,20,20,20)))
  ) %>%
  mutate(comment=rep(NA,n)) %>%
  mutate(regular_eating=randomly_introduce_values(regular_eating,value="Non renseigné",proportion=0.4)) %>%
  mutate(across(c(age,genre,taille),~randomly_introduce_values(.x,value=NA,proportion=0.1))) %>%
  mutate(across(starts_with("sorbet"),~randomly_introduce_values(.x,value="Non renseigné",proportion=0.1)))%>%
  mutate(across(starts_with("creme_glacee"),~randomly_introduce_values(.x,value="Non renseigné",proportion=0.2))) %>%
  mutate(creme_glacee_caramel=randomly_introduce_values(creme_glacee_caramel,value="Non renseigné",proportion=0.5))

icecream$comment[33]="Moi j'adore les glaces mais c'est difficile de trouver de bons artisans de nos jours."
icecream$comment[105]="Je ne sais pas si je vais pouvoir répondre à toutes les questions, je suis un peu fatigué aujourd'hui."
icecream$comment[56]="J'essaie de réduire ma consommation suite au reportage de France 2 sur les glaces artisanales. Je ne sais pas si je vais y arriver, c'est tellement bon !"
icecream$comment[28]="C'est quoi le but de votre enquête? c'est à ça que servent nos impôts j'y crois pas!"
icecream$comment[10]="Pouet pouet camionnette!"
icecream$comment[94]="Les meilleurs parfums ne sont pas cités: stracciatella, passion, noisette..."
icecream$comment[97]="J'essaie de trouver des Nutriscore pas trop nases mais en glaces c'est la galère."
icecream$comment[65]="L'offre de parfums en vegan c'est vraiment pas ça. Je suis déçu."
icecream$comment[62]="Est-ce qu'on pourra recevoir les résultats de l'enquête? Je vous laisse mon mail: xxxx.xxxx@gmail.com"
icecream$comment[61]="RAS"
icecream$comment[34]="RAS"
icecream$comment[56]="Bonne continuation, c'est un travail très important que vous réalisez!"



usethis::use_data(icecream, overwrite=TRUE)

