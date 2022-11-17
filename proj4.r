
 grad<- function(func, paras){
   #Function to be input as an expression, with paras input as a vector of strings
   ## which contain all parameters we want to partially differentiate
   #Function calculates and returns the gradient vector of the objective function, f.
   
   #begin with empty vector to store expressions for gradient vector
  g<- c()
  
  for (i in 1:length(paras)){
    #loop over ever element we want to differentiate
  #add the partial differential to the grad vector 
    g<-append(g,D(f,paras[i]))
  }
  #function returns the gradient vector
   return(g)
 }
 
 
 hess<- function(func, paras){
   #Function to be input as an expression, with paras input as a vector of strings
   ## which contain all parameters we want to partially differentiate
   #Function calculates and returns the gradient vector of the objective function, f.
   
   gradf<- grad(func, paras)
   #begin with empty vector to store expressions for gradient vector
   g<-c()
   
   for (i in 1:length(gradf)){
   for (j in paras){
     g<-append(g,D(gradf[[i]],j))
   }}
   
   
   #function returns the gradient vector
   return(g)
 }
 

