
bd<- read.csv('bankdata_csv_all.csv')
bd$age <- cut(bd$age, breaks = c(0,10,20,30,40,50,60,Inf),labels=c("child","teens","twenties","thirties","forties","fifties","old"))

str(bd)

bd = subset(bd, select = -c(id) )

min_income <- min(bd$income)
max_income <- max(bd$income)
bins = 3 
width=(max_income - min_income)/bins;
bd$income = cut(bd$income, breaks=seq(min_income, max_income, width),labels = c('low','medium','high'))
bd$income


bd$children=factor(bd$children)


bd$married=dplyr::recode(bd$married, YES="married=YES", NO="married=NO")
bd$car=dplyr::recode(bd$car, YES="car=YES", NO="car=NO")
bd$save_act=dplyr::recode(bd$save_act, YES="save_act=YES", NO="save_act=NO")
bd$current_act=dplyr::recode(bd$current_act, YES="current_act=YES", NO="current_act=NO")
bd$mortgage=dplyr::recode(bd$mortgage, YES="mortgage=YES", NO="mortgage=NO")
bd$pep=dplyr::recode(bd$pep, YES="pep=YES", NO="pep=NO")


library(arules)
library(arulesViz)

myRules = apriori(bd, parameter =list(supp = 0.001, conf = 0.9, maxlen = 3))
options(digits=2)
inspect(myRules)

myRules<-sort(myRules, by="confidence", decreasing=TRUE)



myRules<-sort(myRules, by=c("confidence","count"), decreasing=TRUE)
inspect(myRules)




myRules_Filtered_No = apriori(bd, parameter =list(supp = 0.001, conf = 0.9, maxlen = 4),
                           appearance = list(default="lhs",rhs="pep=pep=NO"))



myRules_Filtered_No<-sort(myRules_Filtered_No, by=c("confidence","count"), decreasing=TRUE)
inspect(myRules_Filtered_No)




myRules_Filtered_Yes = apriori(bd, parameter =list(supp = 0.001, conf = 0.9, maxlen = 4),
                           appearance = list(default="lhs",rhs="pep=pep=YES"))

myRules_Filtered_Yes<-sort(myRules_Filtered_Yes, by=c("confidence","count"), decreasing=TRUE)
inspect(myRules_Filtered_Yes)



nrow(bd)

table(bd$pep)



