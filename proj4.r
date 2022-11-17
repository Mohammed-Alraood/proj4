
##Use func<- expression(5*theta + theta^2 - x^3)
 ##paras<- c("theta", "x")
 ## to see how it works, paras must be string form and 
#Function must be used as an expression

grad<- function(func, paras){
  g <- (1:length(paras))*0
  
  for(i in 1:length(paras)){
  g[i] <- expression(D(func,paras[i]))
  }
  
  return(g)
}
  



newt<-function(theta,func,grad,hess=NULL,...,tol=1e-8, fscale=1,maxit=100, max.half=20, eps=1e-6){
  
 
  
  #if hessian matrix not provided, an approximation to Hessian is provided by finite differencing approximation
  #of the the gradient vector, finding the hessian matrix
  if (hess ==0) {
  if (is.null(hess)==TRUE) {
    He <- function (theta,...) {
      
    }}

  #test the hessian by finite difference aprox
  hees <- grad(theta,...) ##grad of grad at
  Hfd <- matrix (0,2,2)  #finite diference Hessian
  for (i in 1:length((theta))) {
    the1 <- theta
    the1[i] <- the1[i] + eps   ##compute resulting 
    hess1 <- grad (the1,t= , y=y) ##compute resulting 
    Hfd [i,] <- (hess1-hees)/eps  ##approximate second derives
  }}
  x= theta
  
  
  
  #while loop to find the min value of the objective fucntion
    #counter to count the iteration
   counter=1
   #while loop to find the min value for the obj function
  while (abs(func(x,...))>tol || counter <= maxit)  {
    
    #minimize the quadratic
    deltaa <- -(chol2inv(chol(hess(x,...)))) %*% grad(x,...)
    
    #test objective function values after each iteration using Taylor's theorem
    obj_f= func(x,...)+t(deltaa) %*%grad(x,...) + 0.5 * t(deltaa) %*% hess(x,...) %*% deltaa 
    obj_f<- func(x,...)
  }
   x
    
    
  
    
    
    counter <- counter+1 #add number of iteration
    
    rl<- list(f,theta,iter,g,Hi)
    return(rl)
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
