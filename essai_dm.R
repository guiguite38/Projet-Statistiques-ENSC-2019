#----------------------------------------------------------------
#================================================================
#===================== Etude statistique ========================
#========================= ENSC 2019 ============================
#=== Par Leatitia Calice, Guillaume Grosse et Céline Lemoigne ===
#================================================================
#----------------------------------------------------------------


#------------------------------------------------------------------------------
#========== Préparation de l'environnement de travail et des données ==========
#------------------------------------------------------------------------------

path<-("./activation2020.Rdata")
essai<-readRDS(path)
head(essai)
dim(essai)


# Objet de l'étude !
frontal_1L<-essai$Prod_G_Frontal_Inf_Tri_1_L

# variables explicatives potentielles
sexe<-essai$Sexe
preference<-essai$Preference_Manuelle
sujet<-essai$Sujet
age<-essai$Age
volume<-essai$Volume_Cerebral
index<-essai$Index_Lateralisation_Hemispherique
frontal_1R<-essai$Prod_G_Frontal_Inf_Tri_1_R
angular_2R<-essai$Prod_G_Angular_2_R
occi_1R<-essai$Prod_G_Occipital_Lat_1_R
rolandic_1R<-essai$Prod_G_Rolandic_Oper_1_R
hippo_1R<-essai$Prod_G_Hippocampus_1_R
tempo_4R<-essai$Prod_S_Sup_Temporal_4_R
angular_2L<-essai$Prod_G_Angular_2_L
occi_1L<-essai$Prod_G_Occipital_Lat_1_L
rolan_1L<-essai$Prod_G_Rolandic_Oper_1_L
hippo_1L<-essai$Prod_G_Hippocampus_1_L
tempo_4L<-essai$Prod_S_Sup_Temporal_4_L



donnees<-c(sexe,preference,sujet,age, volume, index,frontal_1R,angular_2R,
         occi_1R,rolandic_1R,hippo_1R,tempo_4R,frontal_1L,angular_2L,
         occi_1L,rolan_1L,hippo_1L,tempo_4L)

#tableau de donnees
matYX <- data.frame(frontal_1L,sexe,preference,sujet,age,volume,index,frontal_1R,
                  angular_2R,occi_1R,rolandic_1R,hippo_1R,tempo_4R,angular_2L,
                  occi_1L,rolan_1L,hippo_1L,tempo_4L)
pairs(matYX)





#---------------------------------------------
#========== Statistique descriptive ==========
#---------------------------------------------
summary(matYX)

#!!!!!!!!!!!!!!!!
#!! A APPROFONDIR
#!!!!!!!!!!!!!!!!

boxplot(age, main="Etude des participants par age")
# 50% des sujets dans 22-28 ans


#-------------------------
#========== ACP ==========
#-------------------------

# variables décorrélées : index, angular_2R, angular_2L, age, preference, volume



#--------------------------------------------------
#========== Observation des corrélations ==========
#--------------------------------------------------


#frame1 ----------------------------------------------------------------------
frame1 <- data.frame(frontal_1L,angular_2L,occi_1L,rolan_1L,hippo_1L,tempo_4L)
head(frame1)
summary(frame1)
plot(frame1)

# Les variables frontal_1L et tempo_4L   semblent avoir une bonne corrélation (7/10)
# Les variables frontal_1L et angular_2L semblent avoir une faible corrélation (3/10)
# Les variables frontal_1L et hippo_1L   semblent avoir une faible corrélation (3/10)


#frame2 ----------------------------------------------------------------------
frame2 <- data.frame(frontal_1L,frontal_1R,angular_2R,occi_1R,rolandic_1R,hippo_1R,tempo_4R)
head(frame2)
summary(frame2)
plot(frame2)

# Les variables frontal_1L et frontal_1R  semblent avoir une forte corrélation (9/10)
# Les variables frontal_1L et rolandic_1R  semblent avoir une moyenne corrélation (5/10)
# Les variables frontal_1L et occi_1R     semblent avoir une faible corrélation (3/10)
# Les variables frontal_1L et hippo_1R    semblent avoir une faible corrélation (3/10)


#frame3 ----------------------------------------------------------------------
frame3 <- data.frame(frontal_1L,sexe,preference,sujet,age, volume, index)
head(frame3)
summary(frame3)
plot(frame3)

# Les variables frontal_1L et index  semblent avoir une moyenne corrélation (4/10)



#---------------------------------------------------------------------
#========== Régression Linéaire Multiple - Toutes Variables ==========
#---------------------------------------------------------------------

#on a juste retiré sujet puisque aucun intérét
res0 <- lm(frontal_1L~sexe+preference+age+volume+index+frontal_1R+
           angular_2R+occi_1R+rolandic_1R+hippo_1R+tempo_4R+angular_2L+
           occi_1L+rolan_1L+hippo_1L+tempo_4L,
          data=matYX)

summary(res0)



#-----------------------------------------------------------------------------------
#========== Régression Linéaire Multiple - Avec sélection des Données ACP ==========
#-----------------------------------------------------------------------------------

res <- lm(frontal_1L~frontal_1R+occi_1R+rolandic_1R+hippo_1R+
          tempo_4R+occi_1L+rolan_1L+hippo_1L+tempo_4L+sexe,
          data=matYX)

summary(res)
# Multiple R-squared:  0.4669,	Adjusted R-squared:  0.4444 


#---------------------------------------
#========== Etude des résidus ==========
#---------------------------------------

plot(res$fitted,res$residuals)
#pas de structure dans les résidus

shapiro.test(res$residuals)
# W = 0.98213, p-value = 0.003199
# normalité des résidus

residus.stud<-rstudent(res)
plot(residus.stud,ylim=c(-3.5,3.5))
abline(h=c(-2,0,2),lty=c(2,1,2))
#résidus suivent loi de student



#-----------------------------------------------------
#========== Approche critére AIC descendant ==========
#-----------------------------------------------------

#--------------------- Etape 1 ----------------------

drop1(res)
#occi_1L a la valeur d'AIC la plus faible

res2 <- lm(frontal_1L~frontal_1R+occi_1R+rolandic_1R+hippo_1R+
             tempo_4R+rolan_1L+hippo_1L+tempo_4L+sexe,
           data=matYX)

summary(res2)
# Multiple R-squared:  0.4627,	Adjusted R-squared:  0.4425 


#--------------------- Etape 2 ----------------------

drop1(res2)
#la ligne <none> a la valeur AIC la plus faible donc on ne peut pas retirer plus de variables



#----------------------------------------------------
#========== Approche critére AIC ascendant ==========
#----------------------------------------------------


# on choisit les variables les plus significatives du modéle AIC
res3<-lm(frontal_1L~1,data=matYX)
summary(res3)


#--------------------- Etape 1 ----------------------

add1(res3,~sexe+preference+age+volume+index+angular_2R+occi_1R+
       hippo_1R+angular_2L+occi_1L+rolan_1L+hippo_1L+tempo_4L+
       frontal_1R+rolandic_1R+tempo_4R)
# On ajoute la variable avec l'AIC le plus faible
# frontal_1R   1   15.6648 35.584 -480.44

res4<-lm(frontal_1L~frontal_1R,data=matYX)
summary(res4)
# Multiple R-squared:  0.3057,	Adjusted R-squared:  0.3029 


#--------------------- Etape 2 ----------------------

add1(res4,~sexe+preference+age+volume+index+angular_2R+occi_1R+
       hippo_1R+angular_2L+occi_1L+rolan_1L+hippo_1L+tempo_4L+
       frontal_1R+rolandic_1R+tempo_4R)
# angular_2L avec AIC faible
# angular_2L   1    2.9789 32.605 -500.21

res5<-lm(frontal_1L~frontal_1R+angular_2L,data=matYX)
summary(res5)
# Multiple R-squared:  0.3638,	Adjusted R-squared:  0.3586 
# + 6%


#--------------------- Etape 3 ----------------------

add1(res5,~sexe+preference+age+volume+index+angular_2R+occi_1R+
       hippo_1R+angular_2L+occi_1L+rolan_1L+hippo_1L+tempo_4L+
       frontal_1R+rolandic_1R+tempo_4R)
# Ajout tempo_4L
# tempo_4L     1    3.7732 28.832 -528.84

res6<-lm(frontal_1L~frontal_1R+angular_2L+tempo_4L,data=matYX)
summary(res6)
# Multiple R-squared:  0.4374,	Adjusted R-squared:  0.4305 
# + 7%


#--------------------- Etape 4 ----------------------

add1(res6,~sexe+preference+age+volume+index+angular_2R+occi_1R+
       hippo_1R+angular_2L+occi_1L+rolan_1L+hippo_1L+tempo_4L+
       frontal_1R+rolandic_1R+tempo_4R)
# Ajout index
# index        1   1.50254 27.329 -540.16
# !!!!!!!!!!!!!
# On pourrait aussi étudier séparément les gauchers et les droitiers
# !!!!!!!!!!!!!

res7<-lm(frontal_1L~frontal_1R+angular_2L+tempo_4L+index,data=matYX)
summary(res7)
# Multiple R-squared:  0.4667,	Adjusted R-squared:  0.458 
# + 3%
# ON PREND LE PARTI DE S'ARRETER ICI, L'AJOUT DE VARIABLE N'INDUIT PLUS DE
# CHANGEMENT SIGNIFICATIF


#--------------------- Etape 5 ----------------------

add1(res7,~sexe+preference+age+volume+index+angular_2R+occi_1R+
       hippo_1R+angular_2L+occi_1L+rolan_1L+hippo_1L+tempo_4L+
       frontal_1R+rolandic_1R+tempo_4R)
# Ajout hippo_1L
# hippo_1L     1   0.79707 26.532 -545.53

res8<-lm(frontal_1L~frontal_1R+angular_2L+tempo_4L+index+hippo_1L,data=matYX)
summary(res8)
# Multiple R-squared:  0.4823,	Adjusted R-squared:  0.4716 
# + 2%


#--------------------- Etape 6 ----------------------

add1(res8,~sexe+preference+age+volume+index+angular_2R+occi_1R+
       hippo_1R+angular_2L+occi_1L+rolan_1L+hippo_1L+tempo_4L+
       frontal_1R+rolandic_1R+tempo_4R)
# Ajout hippo_1R
# hippo_1R     1   0.67964 25.853 -550.00

res9<-lm(frontal_1L~frontal_1R+angular_2L+tempo_4L+index+hippo_1L+hippo_1R,data=matYX)
summary(res9)
# Multiple R-squared:  0.4955,	Adjusted R-squared:  0.483 
# + 1%



#---------------------------------------------------
#========== Approche critére AIC stepwise ==========
#---------------------------------------------------


# on choisit les variables les plus significatives du modéle AIC
res3<-lm(frontal_1L~1,data=matYX)
summary(res3)


#--------------------- Etape 1 ----------------------

add1(res3,~sexe+preference+age+volume+index+angular_2R+occi_1R+
             hippo_1R+angular_2L+occi_1L+rolan_1L+hippo_1L+tempo_4L+
             frontal_1R+rolandic_1R+tempo_4R)
# On ajoute la variable avec l'AIC le plus faible
# frontal_1R   1   15.6648 35.584 -480.44

res4<-lm(frontal_1L~frontal_1R,data=matYX)
summary(res4)
# Multiple R-squared:  0.3057,	Adjusted R-squared:  0.3029
#----------------------------------------------------
#========== Séparation Homme/Femme ==================
#----------------------------------------------------
if(!require(tidyverse)) {
        install.packages(("tidyverse"))
        library(tidyverse)
}


dataHomme <- filter(essai,sexe == "H")
dataHomme

dataFemme <-filter(essai,sexe=="F")
dataFemme

#----------------------------------------------------
#========== Séparation Droitier/Gaucher =============
#----------------------------------------------------
dataDoitier <- filter(essai, preference =="R")
dataDoitier
dataGaucher <-filter(essai,preference =="G")
dataGaucher
