fdhess<-function(theta,grad,hess){
  #function for finding finite difference of hessian if it is not provided
  #input theta values, gradient vector and hess provided or =NULL
  #returns the hessian
  #if hessian matrix not provided, an approximation to Hessian is provided by finite differencing approximation
  #of the the gradient vector, finding the hessian matrix
if (is.null(hess)==TRUE) {
  #test the hessian by finite difference aprox
  hees <- grad(theta,...) ##grad of grad
  Hfd <- matrix (0,length(theta),length(theta))  #finite diference Hessian
  for (i in 1:length((theta))) {
    the1 <- theta
    the1[i] <- the1[i] + eps   ##compute resulting 
    hess1 <- grad(the1,...) ##compute resulting 
    Hfd [i,] <- (hess1-hees)/eps  ##approximate second derives
    nh<-Hfd
  }
}else{
  nh<-hess(theta)
}
  return(nh)
}


newt<-function(theta,func,grad,hess=NULL,...,tol=1e-8, fscale=1,maxit=100, max.half=20, eps=1e-6){

  #call finite difference hessian function to determine hessian matrix, if hess defined or not
  nh<-fdhess(theta,grad,hess)
  
  #evaluate function and grad at theta
  nf<-func(theta)
  ng<-grad(theta)

  #find eigenvalues for postive definitiveness 
  eigenvals<-eigen(nh)[[1]]

  while(any(eigenvals<=0)==TRUE){
    #if eigenvalues are negative, matrix is not positive definite
    #perturb hessian to arrive at a positive definite matrix
    nh<- nh + length(theta)*diag(length(theta))
    eigenvals<-eigen(nh)[[1]]
  }
  
  
  #check if the objective or derivative is finite, if yes stop and display warning message stating it's not finite
  if ( is.finite(func(theta)== FALSE)|| is.finite(grad(theta)) ==FALSE){
    
    warning("The objective function or the derivative is not finite")
  }
  
  
  #check if we have reached convergence
  check<-abs(ng) <  tol*abs(nf) + fscale
  count=0
  while(any(check==FALSE) ){
    
    #Check if maximum iterations have been reached, if they have without convergence, exit method
    if(count > maxit){
      stop("max iterations reached: did not reach convergence")
    }
    
    count=count+1
    #minimizing function
    mf<- -chol2inv(chol(nh)) %*% ng

     trynew<- theta+mf
count2<-0
    while(nf< func(trynew)){
      if(count2>max.half){
        stop("Reaches maximum limit of step halving: did not reach convergence")
      }
      mf<-0.5*mf
      trynew<- theta+mf
    

    }


    theta<-theta+mf
    
    
    nf<-func(theta)
    ng<-grad(theta)
  #computing new hessian at min
    nh<-fdhess(theta,grad,hess)
    
    
    #check if we have reached convergence
    check<-abs(ng) <  tol*nf + fscale
    
  }
  #function evaluated at the minimum
  fmin<- nf
  #iterations taken to find minimum
  iter<-count
  #gradient evaluated minimum
  gmin<- ng
  #inverse evaluated at the minimum
  Hi<- -chol2inv(chol(nh))
  
  
  #IF hessian not finite, warning message
  if ( any(is.finite(nh)==FALSE)==TRUE){
    
    warning("The Hessian matrix is not finite at convergence")
  }
  rl<-list(fmin,theta,iter,gmin,Hi)
  
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