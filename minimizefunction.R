
# grad<- function(func, paras){
#   #Function to be input as an expression, with paras input as a vector of strings
#   ## which contain all parameters we want to partially differentiate
#   #Function calculates and returns the gradient vector of the objective function, f.
#   
#   #begin with empty vector to store expressions for gradient vector
#   g<- c()
#   
#   for (i in 1:length(paras)){
#     #loop over ever element we want to differentiate
#     #add the partial differential to the grad vector 
#     g<-append(g,D(func,paras[i]))
#   }
#   #function returns the gradient vector
#   return(g)
# }
# 
# 
# hess<- function(func, paras){
#   #Function to be input as an expression, with paras input as a vector of strings
#   ## which contain all parameters we want to partially differentiate
#   #Function calculates and returns the gradient vector of the objective function, f.
#   
#   gradf<- grad(func, paras)
#   #begin with empty vector to store expressions for gradient vector
#   g<-c()
#   
#   for (i in 1:length(gradf)){
#     for (j in paras){
#       g<-append(g,D(gradf[[i]],j))
#     }}
#   
#   g<-matrix(g,length(paras),length(paras))
#   #function returns the gradient vector
#   return(g)
# }
# 



newt<-function(theta,func,grad,hess=NULL,...,tol=1e-8, fscale=1,maxit=100, max.half=20, eps=1e-6){
  
  #evaluate function,grad and hess at theta
  nf<-func(theta)
  ng<-grad(theta)
  nh<-hess(theta)
  eigenvals<-eigen(nh)[[1]]
  
  while(any(eigenvals<=0)==TRUE){
    #if eigenvalues are negative, matrix is not positive definite
    #perturb hessian to arrive at a positive definite matrix
    nh<- nh + length(theta)*diag(length(theta))
    eigenvals<-eigen(nh)[[1]]
    
  }
  
  #check if we have reached convergence
  check<-abs(ng) <  tol*nf + fscale
  

   while(any(check==FALSE)){
     #minimizing function
    mf<- -chol2inv(chol(nh)) %*% ng
    #new theta values
    theta<- theta+mf
     
    
    nf<-func(theta)
    ng<-grad(theta)
    nh<-hb(theta)
    
    
    #check if we have reached convergence
    check<-abs(ng) <  tol*nf + fscale
     
   }
  
  return(theta)
  
  
  
  
  #if hessian matrix not provided, an approximation to Hessian is provided by finite differencing approximation
  #of the the gradient vector, finding the hessian matrix
  # if (is.null(hess)==TRUE) {
  #   He <- function (theta,...) {
  #     ##hessian of the function  
  #     alpha
  #     
  #   }}
  # 
  # #test the hessian by finite difference aprox
  # hees <- grad(theta,t, y) ##grad of grad at
  # eps <- le-8 ##fininte difference interval 
  # Hfd <- matrix (0,)  #finite diference Hessian
  # for (i in 1:length((theta))) {
  #   the1 <- theta
  #   the1[i] <- the1[i] + eps   ##compute resulting 
  #   hess1 <- grad (the1,t= , y=y) ##compute resulting 
  #   Hfd [i,] <- (hess1-hees)/eps  ##approximate second derives
  # }
  # x= theta
  # 
  # 
  # 
  # #while loop to find the min value of the objective fucntion
  # #counter to count the iteration
  # counter=1
  # #while loop to find the min value for the obj function
  # while (abs(func(x,...))>tol || counter <= maxit)  {
  #   
  #   #minimize the quadratic
  #   deltaa <- -(chol2inv(chol(hess(x,...)))) %*% grad(x,...)
  #   
  #   #test objective function values after each iteration using Taylor's theorem
  #   func(x,...)= func(x,...)+t(deltaa) %*%grad(x,...) + 0.5 * t(deltaa) %*% hess(x,...) %*% deltaa 
  #   
  # }
  # x
  # 
  # 
  # 
  # 
  # 
  # counter <- counter+1 #add number of iteration
  # 
  # rl<- list(f,theta,iter,g,Hi)
  # return(rl)
}










#given functions

rb <- function(th,k=2) {
  k*(th[2]-th[1]^2)^2 + (1-th[1])^2
}
gb <- function(th,k=2) {
  c(-2*(1-th[1])-k*4*th[1]*(th[2]-th[1]^2),k*2*(th[2]-th[1]^2))
}

hb <- function(th,k=2) {
  h <- matrix(0,2,2)
  h[1,1] <- 2-k*2*(2*(th[2]-th[1]^2) - 4*th[1]^2)
  h[2,2] <- 2*k
  h[1,2] <- h[2,1] <- -4*k*th[1]
  h
}