#Group #36
#Group Member: Chloe Bircham s2415006, Mohammed Alraood s2227255, Xinyan Chen s2304087
#https://github.com/Mohammed-Alraood/proj4.git
#contribution: 
#Chloe Bircham:Created minimizing function mf, apply to function, perturb if matrix not positive def
##check for convergence and set maxit, maxhalf conditions.
#Mohammed Alraood:Created the repo, approx Hessian by finite differencing, warning message, create newt function
#Xinyan Chen:Provide specific iterative steps for Newton's method, check the code and suggest changes
##and comments on the gradient comparison and warning parts.

fdhess<-function(theta,grad,hess,eps,...){
      #function for finding finite difference of hessian if it is not provided
      #input theta values, gradient vector and hess provided or =NULL
      #returns the hessian as an output
      #if hessian matrix not provided, an approximation to Hessian is provided by finite differencing approximation
      #of the the gradient vector, to find hessian matrix
    if (is.null(hess)==TRUE) {
      #test the hessian by finite difference aprox
      hees <- grad(theta,...) ##grad of gradient vector
      Hfd <- matrix (0,length(theta),length(theta))  #finite diference Hessian
      for (i in 1:length((theta))) {
        #loop to go through theta and find the second derivative
        the1 <- theta #assign the1 to theta
        the1[i] <- the1[i] + eps   ##compute resulting 
        hess1 <- grad(the1,...) ##compute resulting in gradient 
        Hfd [i,] <- (hess1-hees)/eps  ##approximate second derives
        nh<-Hfd #renam the Hfd (hessian)
      }
     }else{
      #if hess was provided, evaluate it at theta
      nh<-hess(theta)
    }
  #return the hessian
  return(nh)
  
}


newt<-function(theta,func,grad,hess=NULL,...,tol=1e-8, fscale=1,maxit=100, max.half=20, eps=1e-6){
  #newt function that optimized a given objective function by applying the newton method.
  #INPUT:
    #theta: vector of inital values
    #func: objective function to minimize
    #grad: the gradient function for the objective function
    #hess: the hessian matrix function for the objective function. if equal to null, newt computes
    #hess by finite differencing
    #the tolerance set for convergence
    #fscale: estimate for magnitude of function near the optimum
    #maxit: limit for number of iterations
    #max.half: max number of halving steps to increase objective function
    #eps: finite difference interval for calculating hessian if not provided

  #OUTPUT: list of
    #f: objective function at minimum value
    #theta: value of parameters at the minimum
    #iter: number of iterations needed to reach minimum
    #g: gradient vector at the minimum
    #Hi: the inverse of the hessian at minimum

    #call finite difference hessian function to determine hessian matrix, if hess defined or not
    nh<-fdhess(theta,grad,hess,eps,...)

    #evaluate function and grad at initial theta
    nf<-func(theta,...)
    ng<-grad(theta,...)

    #find eigenvalues for evaluating if postive definitive 
    eigenvals<-eigen(nh)[[1]]

    while(any(eigenvals<=0)==TRUE){
      #if eigenvalues are negative, matrix is not positive definite
      #perturb hessian to arrive at a positive definite matrix
      #perturb by addition of a multiple of the identity matrix
      nh<- nh + length(theta)*diag(length(theta))
      #reassign eigenvals to test if positive definite
      eigenvals<-eigen(nh)[[1]]
    }

    #check if the objective or derivative is finite, if yes stop and display warning message stating it's not finite
    if ( is.finite(func(theta)== FALSE)|| is.finite(grad(theta)) ==FALSE){

      warning("The objective function or the derivative is not finite")
    }


    #check if we have reached convergence, using parameters set by the function
    check<-abs(ng) <  tol*abs(nf) + fscale

    #set a counter=0 to count iterations
    count=0

    #checking if the absolute value of the function at the current value is less than consitions for convergence
      ##if it is not then:
    while(any(check==FALSE) ){
      #Check if maximum iterations have been reached, if they have without convergence, exit method
      if(count > maxit){
        stop("max iterations reached: did not reach convergence")
      }

      #add one to nuber of iterations
      count=count+1

      #minimizing function
      #negative inverse of the hessian multiplied with the gradient (both evaluated at current value)
      mf<- -chol2inv(chol(nh)) %*% ng

      #assigning a new value of theta as the current value plus the minimizing function
       trynew<- theta+mf
       #set another counter =0 to count number of halving steps performed
      count2<-0

      #check if the function evaluated at the new values have inproved the objective function
      #if they havent, then:
      while(nf< func(trynew)){

        #check we havent reached maximum halving steps, if we have, stop iterations, print error message
        if(count2>max.half){
          stop("Reaches maximum limit of step halving: did not reach convergence")
        }

        #half the minimizing function and add these to the theta values. reiterate the loop to 
        #check if these values minimize the function. Loop ends when appropriate values are found
        mf<-0.5*mf
        trynew<- theta+mf

      }

      #Assign the new values of theta as the previous steps values + new minimixing function
      theta<-theta+mf

      #evaluate function, gradient and hessian at the new theta
      nf<-func(theta,...)
      ng<-grad(theta,...)
      #computing new hessian at min
      nh<-fdhess(theta,grad,hess,eps,...)


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

      warning("The Hessian matrix is not finite at convergence") #warning message to show up
    }
    #list to be returned as an output of newt function
    rl<-list(f=fmin,theta=theta,iter=iter,g=gmin,Hi=Hi)

  return(rl)   
}
