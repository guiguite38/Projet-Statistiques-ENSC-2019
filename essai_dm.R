#path<-("D:/2A/modelisation_stat/activation2020.Rdata") # Chemin Laetitia
path<-("activation2020.Rdata") # Chemin Céline
essai<-readRDS(path)
head(essai)
dim(essai)

#les donnees
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
frontal_1L<-essai$Prod_G_Frontal_Inf_Tri_1_L
angular_2L<-essai$Prod_G_Angular_2_L
occi_1L<-essai$Prod_G_Occipital_Lat_1_L
rolan_1L<-essai$Prod_G_Rolandic_Oper_1_L
hippo_1L<-essai$Prod_G_Hippocampus_1_L
tempo_4L<-essai$Prod_S_Sup_Temporal_4_L

# ---------------- ACP ---------------------------

#???install.packages(("PCAmixdata"))
#library(PCAmixdata)

donne<-c(sexe,preference,sujet,age, volume, index,frontal_1R,angular_2R,occi_1R,rolandic_1R,hippo_1R,tempo_4R,frontal_1L,angular_2L,occi_1L,rolan_1L,hippo_1L,tempo_4L)
plot(donne)