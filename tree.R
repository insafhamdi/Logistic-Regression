library(rpart)
df<-read.table("C:/Users/hamdi/Downloads/ost program/ronfle.txt",header=TRUE)
summary(df)
summary(df$RONFLE)
arbre.full <- rpart(RONFLE ~ AGE+ALCOOL+SEXE+TABAC, data = df, method = "class")
arbre.full
#si on augmente minsplit,l'arbre max augmente, l'arbre devient plus profond.
# 6 splits ? ==> 7 feuilles
#arbre.full <- rpart(RONFLE ~ AGE+ALCOOL+SEXE+TABAC, data = df, method = "class", minsplit=5)
#xerror faible? ==> mieux est le résultat 
#minsplit nbre min d'indiv dans un noeuds 
#erreur min : somme de xerror et xstd 
#on voit l'importance de la variable de son ordre dans l'arbre textuelle, quand elle intervient ici , age, alcool, tabac 
#exple : si (TABAC =N) et (alcool >7.5) et ( 4.5<age<49.5) alors oui (car devant tabac=n on a 1 ce qui est oui)


#Impression d'un arbre sous forme graphique :
plot(arbre.full, uniform = TRUE, branch = 0.5, margin = 0.1)
text(arbre.full, all = FALSE, use.n = TRUE)

#Avec rpart.plot

library(rpart.plot)

binary.model <- rpart(RONFLE ~ AGE+ALCOOL+SEXE+TABAC, data = df, method = "class")
rpart.plot(binary.model)



#sortir l'arbre en fichier ps (postscript)
post(arbre.full)


#Prediction

pred=predict(arbre.full,newdata=don,type="class")
pred


#Pruning

##Elagage et param tre de complexit 

plotcp(arbre.full)
printcp(arbre.full)



# l'arbre le plus  lagu  (ligne 1 de la cptable) s'obtient pour cp 
# dans l'intervalle ]cp1; 1] ;
# le deuxi me (un peu moins  lagu ) (ligne 2 de la cptable) s'obtient 
# pour cp dans l'intervalle ]cp2; cp1] ;
# le troisi me (encore un peu moins  lagu ) (ligne 3 de la cptable) 
# s'obtient pour cp dans l'intervalle ]cp3; cp2] ;
# etc.

# Notez bien que la borne inf rieure de l'intervalle est exclus : 
# la valeur de cp = cp2 correspond   l'arbre 3, pas au 2.


#deux maniere d'elager l'arbre, soit simplement en faisant :


arbre.full.prune<-prune(arbre.full,cp=0.048)

#ou bien en utilisant le param tre de control :

don.cnt <- rpart.control (minsplit = 1,cp=0.048)
arbre.full.prune<-rpart(RONFLE ~ AGE+ALCOOL+SEXE+TABA, data = df, method = "class",control=don.cnt)