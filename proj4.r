
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
  

#if hessian matrix not provided, an approximation to Hessian is provided by finite differencing approximation
#of the the gradient vector, finding the hessian matrix
if (hess ==0) {
  He <- function (theta, t,y) {
    ##hessian of the function  
    alpha
    
  }}

#test the hessian by finite difference aprox
hees <- grad(theta,t, y) ##grad of grad at
eps <- le-8 ##fininte difference interval 
Hfd <- matrix (0,)  #finite diference Hessian
for (i in 1:length((theta))) {
  the1 <- theta
  the1[i] <- the1[i] + eps   ##compute resulting 
  hess1 <- grad (the1,t= , y=y) ##compute resulting 
  Hfd [i,] <- (hess1-hees)/eps  ##approximate second derives
}

newt<-function(theta,func,grad,hess=NULL,...,tol=1e-8, fscale=1,maxit=100, max.half=20, eps=1e-6){
  
 
  x= theta
  
  
  
  #while loop to find the min value of the objective fucntion
    #counter to count the iteration
   counter=1
  
   #while loop to find the min value for the obj function
  while (abs(func(x,...))>tol || counter <= maxit)  {
    
    
    
  
    
    
    counter <- counter+1 #add number of iteration
  }
  
  
  
  
  rl<- list(f,theta,iter,g,Hi)
  return(rl)
}