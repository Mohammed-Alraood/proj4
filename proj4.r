
##Use func<- expression(5*theta + theta^2 - x^3)
 ##paras<- c("theta", "x")
 ## to see how it works, paras must be string form and 
#Function must be used as an expression

grad<- function(func, paras){
  g <- (1:length(paras))*0
  
  for(i in 1:length(paras)){
  g[i] <- expression(D(func,paras[i]))
  }
  
}
  


newt<-function(theta,func,grad,hess=NULL,...,tol=1e-8, fscale=1,maxit=100, max.half=20, eps=1e-6){
  
 
  
  
  rl<- list(f,theta,iter,g,Hi)
  return(rl)
}