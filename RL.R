### Données réelles 
#Jeu de données réelles : Ronfle


summary(ronfle)
library(MASS)




#Regression logistique
modele_complet <- glm(RONFLE ~ ., data = ronfle, family = binomial)
summary(modele_complet)
modele_complet

#Mod0
mod0<- lda(RONFLE ~ ., ronfle)
mod0
predict(mod0, ronfle)

TC0<-table(predict(mod0, ronfle)$class,ronfle[,6])
TC0
prec0=(TC0[1,1]+TC0[2,2])/sum(TC0)
prec0

#Mod0 avec Validation Croisée (Leave One Out)

mod0LOO<- lda(RONFLE ~ .,CV=T, ronfle)
prev0LOO<-mod0LOO$class

TC0LOO<-table(prev0LOO,ronfle[,6])
TC0LOO
prec0LOO=(TC0LOO[1,1]+TC0LOO[2,2])/sum(TC0LOO)
prec0LOO



#Mod0 avec Validation Croisée  (Croos Validation)

x<-sample(c(1:100), 80)
don0=ronfle[x,]
don1=ronfle[-x,]

mod0CV<- lda(RONFLE ~., don0)

#predict(mod0CV, don1)

TC0CV<-table(predict(mod0CV, don1)$class,don1[,6])
TC0CV
prec0CV=(TC0CV[1,1]+TC0CV[2,2])/sum(TC0VC)
prec0CV


# Nouvelles données

donquant <- matrix(c(42,58,35,67,55,94,70,63,169,185,180,166,0,4,6,3),ncol=4,byrow=F)
donquant
donqual <- matrix(c("F","H","H","F","N","O","O","N"),ncol=2,byrow=F)
donqual
n_donnees<- cbind.data.frame(donquant,donqual)
n_donnees

nomcol<-c("AGE","POIDS","TAILLE","ALCOOL","SEXE","TABAC")
colnames(n_donnees)<-nomcol

predict(mod0, n_donnees)

