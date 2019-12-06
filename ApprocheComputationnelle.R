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

path <- ('activation2020.Rdata')
data <- readRDS(path)
head(data)
dim(data)


# Objet de l'étude !
frontal_1L<-data$Prod_G_Frontal_Inf_Tri_1_L

# variables explicatives potentielles
sexe<-data$Sexe
preference<-data$Preference_Manuelle
sujet<-data$Sujet
age<-data$Age
volume<-data$Volume_Cerebral
index<-data$Index_Lateralisation_Hemispherique
frontal_1R<-data$Prod_G_Frontal_Inf_Tri_1_R
angular_2R<-data$Prod_G_Angular_2_R
occi_1R<-data$Prod_G_Occipital_Lat_1_R
rolandic_1R<-data$Prod_G_Rolandic_Oper_1_R
hippo_1R<-data$Prod_G_Hippocampus_1_R
tempo_4R<-data$Prod_S_Sup_Temporal_4_R
angular_2L<-data$Prod_G_Angular_2_L
occi_1L<-data$Prod_G_Occipital_Lat_1_L
rolan_1L<-data$Prod_G_Rolandic_Oper_1_L
hippo_1L<-data$Prod_G_Hippocampus_1_L
tempo_4L<-data$Prod_S_Sup_Temporal_4_L

#tableau de donnees
matYX <- data.frame(frontal_1L,sexe,preference,age,volume,index,frontal_1R,
                    angular_2R,occi_1R,rolandic_1R,hippo_1R,tempo_4R,angular_2L,
                    occi_1L,rolan_1L,hippo_1L,tempo_4L)



#-----------------------------------------------
#========== Approche Computationnelle ==========
#-----------------------------------------------

MSE = matrix(0,nrow = 100, ncol= 17)

# Pour chaque variable
for(j in 1:17)
  # on fait 100 fois :
  #    permutation des valeurs au sein d'une colonne
  #    production d'un modèle
  #    calcul du MSE = moyenne des écarts²
  for (i in 1:100)
  {
    copiematYX = matYX
    if(j!=1)
    {
      copiematYX[,j]=sample(matYX[,j],replace=FALSE)
    }
    
    res <- lm(frontal_1L~sexe+preference+age+volume+index+frontal_1R+
                    angular_2R+occi_1R+rolandic_1R+hippo_1R+tempo_4R+angular_2L+
                    occi_1L+rolan_1L+hippo_1L+tempo_4L,
                  data=copiematYX)
    
    MSE[i,j] = 1/249*sum(res$residuals^2)
  }

boxplot(MSE)
value = mean(MSE[,1])+0.025*mean(MSE[,1])
abline(h=value,col="red")

# conclusion : on garde tous les boxplots au-dessus de mean(MSE(base)) * 1.025
# index frontal_1R angular_2L hippo_1L tempo_4L

