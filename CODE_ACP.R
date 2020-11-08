#Codage ACP pour un triplet (X,Q,D) 


ACP_dim2 <- function(X,Q,D){
  Si <- t(X)%*%D%*%X%*%Q  # Matrice inertie
  U <- matrix(data = eigen(Si)$vectors[,1:2], nrow = 1000)  # Les deux premiers vecteurs propres de Si 
  inerti <- sum(eigen(Si)$val[1:2])/sum(eigen(Si)$val)*100
  print(paste("L'inertie est de", inerti))
  A <- Q%*%U  # Facteurs principaux
  Comp_princ <- sapply(1:2, function(x) X %*% A[,x]) 
  s.label(Comp_princ, sub= "Graphique des Individus en gardant 2 axes", possub = "topleft", csub = 1.9)
  G <- sapply(1:2,function(x) (eigen(Si)$val[x]**(1/2))*A[,x])
  s.corcircle(G,sub= "Graphique des Individus en gardant deux axes", possub = "topleft", csub = 1.9) # GRAPH DES VARIABLES
  resultat <- data.frame(Comp_princ,G)
  return(resultat)
}


ACPdim3 <- function(X,Q,D){
  Si <- t(X)%*%D%*%X%*%Q  # Matrice inertie
  U <- matrix(data = eigen(Si)$vectors[,1:3], nrow = 1000)  # Les deux premiers vecteurs propres de Si 
  inerti <- sum(eigen(Si)$val[1:3])/sum(eigen(Si)$val)*100
  print(paste("L'inertie est de", inerti))
  A <- Q%*%U  # Facteurs principaux
  Comp_princ <- sapply(1:3, function(x) X %*% A[,x]) 
  G <- sapply(1:3,function(x) (eigen(Si)$val[x]**(1/2))*A[,x])
  scatterplot3d(Comp_princ[,1], 
                Comp_princ[,2],
                Comp_princ[,3],
                xlab= "Axe 1",
                ylab='Axe2',
                zlab='Axe3',
                main  = 'Representation des individu en dimension 3',
                color = "blue")
  resultat <- data.frame(Comp_princ,G)
  scatterplot3d(G[,1], 
                G[,2],
                G[,3],
                xlab= "Axe 1",
                ylab='Axe2',
                zlab='Axe3',
                main  = 'Representation des individu en dimension 3',
                color = "blue")
  return(resultat)
}