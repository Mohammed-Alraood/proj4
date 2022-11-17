
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
 
hessi<- function(func,theta){
  fhess<-hess(func,theta)

  
}

newt<-function(theta,func,grad,hess=NULL,...,tol=1e-8, fscale=1,maxit=100, max.half=20, eps=1e-6){
  hessi<- 1/d*matrix(c(hess[4], -hess[2], -hess[3], hess[1]))
  ngrad<- grad(theta[1],theta[2])
  mfunc<- -hessi %*% ngrad
  
  taylorT<- func(theta[1],theta[2]) + t(mfunc)%*%ngrad + 0.5* t(mfunc)%*%hess %*% mfunc
  
  #rl<- list(f,theta,iter,g,Hi)
  return(taylorT)
}