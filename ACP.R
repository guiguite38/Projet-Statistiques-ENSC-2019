path<-("activation2020.Rdata")
donnees<-readRDS(path)
head(donnees)
dim(donnees)
summary(donnees)

# ---------------- ACP ---------------------------

if(!require(PCAmixdata)) {
  install.packages(("PCAmixdata"))
  library(PCAmixdata)
}

donne<-c(age, volume, frontal_1R,angular_2R,occi_1R,rolandic_1R,hippo_1R,tempo_4R,frontal_1L,angular_2L,occi_1L,rolan_1L,hippo_1L,tempo_4L)
res.pca<-PCAmix(X.quali = donnees[,c("Preference_Manuelle","Sexe")],
               X.quanti = donnees[,c("Age","Volume_Cerebral","Index_Lateralisation_Hemispherique","Prod_G_Frontal_Inf_Tri_1_R","Prod_G_Angular_2_R","Prod_G_Occipital_Lat_1_R","Prod_G_Rolandic_Oper_1_R","Prod_G_Hippocampus_1_R","Prod_S_Sup_Temporal_4_R","Prod_G_Frontal_Inf_Tri_1_L","Prod_G_Angular_2_L","Prod_G_Occipital_Lat_1_L","Prod_G_Rolandic_Oper_1_L","Prod_G_Hippocampus_1_L","Prod_S_Sup_Temporal_4_L")])


#Choix du nombre d'axes à retenir
par(mfrow=c(1,1))
round(res.pca$eig,digit=2) #affiche les valeurs propres et les pourcentages de variances expliquées par chaque axe
#on prend les 6 premiers axes car après en dessous de 1

#infos sur les dimensions
barplot(res.pca$eig[,1],main="Eigenvalues",names.arg=1:nrow(res.pca$eig))
abline(h=1,col=2,lwd=2)
plot(res.pca)

#graphiques des individus
par(mfrow=c(1,2))
#plot(res,axes=c(1,2),choice="ind",label=FALSE)
plot(res.pca,axes=c(1,2),choice="cor") #cercle de corrélations
plot(res.pca,axes=c(1,2),choice="sqload")
res.pca$ind
round(res.pca$ind$cos2,digit=3)
res.pca$quanti
round(res.pca$quanti$cos2,digit=3)