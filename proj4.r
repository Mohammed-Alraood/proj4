
##Use func<- expression(5*theta + theta^2 - x^3)
 ##paras<- c("theta", "x")
 ## to see how it works, paras must be string form and 
#Function must be used as an expression

# grad<- function(func, paras){
#   g <- (1:length(paras))*0
#   
#   for(i in 1:length(paras)){
#   g[i] <- expression(D(func,paras[i]))
#   }
#   
#   return(g)
# }
#   



newt<-function(theta,func,grad,hess=NULL,...,tol=1e-8, fscale=1,maxit=100, max.half=20, eps=1e-6){
  nh<-NULL
 
  
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
      }

      #evaluate function,grad and hess at theta
      nf<-func(theta)
      ng<-grad(theta)
      
      #if hessian is provided, evaluate it at theta
      if(is.null(nh)==TRUE){
        print(nh)
      nh<-hess(theta)}
      
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
        #new theta values
        theta<- theta+mf
        
        
        nf<-func(theta)
        ng<-grad(theta)
        nh<-hb(theta)
        
        
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
      if ( is.finite(hess(theta)== FALSE)){
        
        warning("The Hessian matrix is not finite at convergence")
      }
      rl<-list(fmin,theta,iter,gmin,Hi)
      
      return(rl)
      
      
  }
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
  #     
  #     
  # 
  # #check if the objective or derivative is finite, if yes stop and pup up a message stating it's not finite
  #   if ( is.finite(func(theta)== FALSE)|| is.finite(grad(theta)) ==FALSE){
  #     
  #     warning("The objective function or the derivative is not finite")
  #   }
  #     
  # #ch
  # 
  # #while loop to find the min value of the objective fucntion
  #   #counter to count the iteration
  #  counter=1
  # #while loop to check each element of gradient matrix 
  #  #seeing whether all elements of the gradient vector have absolute value less
  #  #than tol times the absolute value of the objective function plus fscale
  #  
  # while (abs(grad(theta))< tol * abs(func(theta)+ fscale)) {
  #   
  # 
  #  #while loop to find the min value for the obj function
  # while (abs(func(theta,...))>tol || counter <= maxit)  {
  #   
  #   #minimize the quadratic
  #   deltaa <- -(chol2inv(chol(hess(theta,...)))) %*% grad(theta,...)
  #   
  #   theta= theta - deltaa
  #   
  #   
  # }
  #  x
  #   
  #   
  # 
  #   
  #   
  #   counter <- counter+1 #add number of iteration
  #   
  #   rl<- list(f,theta,iter,g,Hi)
  #   return(rl)
  # }}}
  # 
  # 
  # 
  # 
  # 
  # 




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
