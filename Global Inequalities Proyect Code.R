#SOURCES:

#GINI data is extracted from CONEVAL. Estimates based on the 2020 MCS-ENIGH and the 2020 Population and Housing Census sample, INEGI.
#Obesity data is extracted from INEGI. Calculations using small-area estimation techniques, based on ENSANUT, the 2015 Intercensal Survey, and administrative records.
#Information of social backwardness index is extracted from CONEVAL.

##Libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggeffects)

##charge and clean data from "a_peq_prev_2018.xlsx"
obesity<-read_xlsx("C:/Users/ibris/OneDrive/Escritorio/CURSOS/R studio/a_peq_prev_2018.xlsx")%>%
  select("Identificador único del municipio","Entidad federativa","Municipio o delegación","Estimador","Porcentaje de población de 20 años y más con obesidad.")%>%
  pivot_wider(names_from = "Estimador",values_from ="Porcentaje de población de 20 años y más con obesidad.")

##filter the results with a high Coefficient of Variation (more than 30%)
obesity<-filter(obesity,obesity[8]<30)


obesity<-obesity%>%select("Identificador único del municipio","Entidad federativa","Municipio o delegación","Valor")%>%mutate(Municipality_ID=`Identificador único del municipio`,State=`Entidad federativa`,Municipality=`Municipio o delegación`,Obesity_percentage=`Valor`)%>%
  select(State,Municipality_ID,Municipality,Obesity_percentage)

#charge data from "CoefGini, RazonIng segun municipios, Mexico 2010-2020.xlsx"
GINI<-read_excel("C:/Users/ibris/OneDrive/Escritorio/CURSOS/R studio/CoefGini, RazonIng segun municipios, Mexico 2010-2020.xlsx")%>%
  mutate(Municipality_ID=`Clave de municipio`,Municipality=`Municipio`,GINI=as.numeric(`Coeficiente de Gini`))%>%
  select(Municipality_ID,GINI)

#Join data
DF<-inner_join(GINI,obesity,by="Municipality_ID")%>%select(State,Municipality, Municipality_ID,GINI,Obesity_percentage)

#Charge social backwardness index dataset
SBI<-read_xlsx("C:/Users/ibris/OneDrive/Escritorio/CURSOS/R studio/Calculo_IRS_2020_R/temp/rezago_mun.xlsx")%>%mutate(Municipality_ID=clave_mun)
#Join Data again

DF2<-inner_join(DF,SBI,by="Municipality_ID")%>%select(-clave_ent,-nom_ent,-clave_mun,-nom_mun)

#Some graphs

ggplot(DF2,aes(irs,Obesity_percentage))+geom_point()+geom_smooth()+
  labs(x = "SBI",y = "Obesity Percentage",title = "Social Backwardness Index and Obesity")

ggplot(DF2,aes(GINI,irs))+geom_point()+geom_smooth()+
  labs(x = "GINI",y = "SBI",title = "Inequality and Social Backwardness Index")

ggplot(DF2,aes(GINI,Obesity_percentage))+geom_point()+geom_smooth()+
  labs(x = "GINI",y = "Obesity Percentage",title = "Inequality and Obesity")

#Statistical Model

model <- lm(Obesity_percentage ~ GINI + i_sdsalud + i_edbasinc + pobtot,data = DF2)
summary(model)


#Graph of the model

plot(ggpredict(model, terms = "GINI"))