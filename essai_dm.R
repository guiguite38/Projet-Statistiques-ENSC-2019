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

summary(matYX)


#-------------------------
#========== ACP ==========
#-------------------------

# variables décorrélées : index, angular_2R, angular_2L, age, preference, volume



#---------------------------------------------
#========== Statistique descriptive ==========
#---------------------------------------------
#!! A APPROFONDIR

boxplot(age, main="Etude des participants par age")
# 50% des sujets dans 22-28 ans



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

res0 <- lm(frontal_1L~sexe+preference+age+volume+index+frontal_1R+
           angular_2R+occi_1R+rolandic_1R+hippo_1R+tempo_4R+angular_2L+
           occi_1L+rolan_1L+hippo_1L+tempo_4L,
          data=matYX)

summary(res0)



#--------------------------------------------------
#========== Régression Linéaire Multiple ==========
#--------------------------------------------------


res <- lm(frontal_1L~frontal_1R+occi_1R+rolandic_1R+hippo_1R+
          tempo_4R+occi_1L+rolan_1L+hippo_1L+tempo_4L+sexe,
          data=matYX)

summary(res)


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



#----------------------------------------------------
#========== Affinement du modèle - Etape 1 ==========
#----------------------------------------------------

drop1(res)
#occi_1L a la valeur d'AIC la plus faible

res2 <- lm(frontal_1L~frontal_1R+occi_1R+rolandic_1R+hippo_1R+
             tempo_4R+rolan_1L+hippo_1L+tempo_4L+sexe,
           data=matYX)

summary(res2)

#----------------------------------------------------
#========== Affinement du modèle - Etape 2 ==========
#----------------------------------------------------

drop1(res2)
