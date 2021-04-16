ACLIME<-function(Data, delta = 2, rho = 2, cri = 0.000005){
  
  Z<-Data
  n<-dim(Z)[1]
  p<-dim(Z)[2]
  
  ####Some usefull function specific to the ADMM program####
  
  #Unit vector
  unit<-rep(1,p)
  
  #This function creates the matrix A (see p5 in [2])
  A<-function(j){
    SigmahatUp<-Sigmahat-tau*unit%*%t(diag(p)[,j])
    SigmahatDown<-Sigmahat+tau*unit%*%t(diag(p)[,j])
    stack<-matrix(nrow=2*p,ncol=p)
    stack[1:p,]<-SigmahatUp
    stack[(p+1):(2*p),]<-(-SigmahatDown)
    return(stack)
  }
  
  #This function creates the matrix C (see ibid)
  C<-function(j){
    stack2<-as.matrix(numeric(2*p))
    stack2[1:p]<-diag(p)[,j]
    stack2[(p+1):(2*p)]<-(-diag(p)[,j])
    return(as.matrix(stack2))
  }
  
  #This is the box function (see p8 in [2])
  box<-Vectorize(function(a,b,c){return((b+c)*((a-b)>c)+a*(abs(a-b)<=c)+(b-c)*((a-b)<(-c)))})
  
  #Soft tresholding operator
  Soft<-function(i,j){return(sign(i)*(abs(i)-(1/j))*(abs(i)>(1/j)))}
  
#################
####Algorithm####
#################

#Compute the sample covariance matrix
  Sigma<-1/(n-1)*t(Z)%*%Z
#Compute the regularized covariance matrix
  Sigmahat<-Sigma+diag(p)*(1/n)  
  
####Parameters that depends on the data
eta<-2*rho*norm(Sigma,type="2")^2    #Second parameter of the ADMM program, must satisfy some relashionship with rho (see p10 in [2])
tau<-delta*sqrt(log(p)/n) 

####Creating matrix that will be filled with the estimated####
omega<-matrix(nrow=p,ncol=p)
fakeinverse<-matrix(nrow=p,ncol=p)
finalinverse<-matrix(nrow=p,ncol=p)

####Step 1####
#Estimation is done collumn by collumn
q=1
while(q<=p){
  
  #Creating variables for the ADMM programm 
  Yt0<-as.matrix(rep(2,2*p))
  Yt1<-as.matrix(rep(3,2*p))
  rt0<-as.matrix(rep(4,2*p))
  rt1<-as.matrix(rep(3,2*p))
  bt0<-as.matrix(Sigma[,q],p)
  bt1<-as.matrix(rep(2,p))
  
  #Load A and C
  A1<-A(q)
  C1<-C(q)  
  Dista=1000
  while(Dista>cri){
    #a<-1
    #while(a<400){
    
    #Creating g and v based on the previous iteartion 
    g<-(rho/eta)*t(A1)%*%((2*Yt1)-Yt0)
    v<-bt0-g
    #updating b by applying the softtreasholding
    bt1<-Soft(v,(eta))
    Yt0<-Yt1
    #uptating r
    h<-C1-Yt0-A1%*%bt1
    
    rt1<-h*(h>0)
    #updating Y
    Yt1<-Yt0+A1%*%bt1+rt1-C1
    
    #updating distance
    Dista<-(bt0[q]-bt1[q])^2
    #message((bt0-bt1)^2)
    bt0<-bt1
    
  }
  #message(q)
  omega[,q]<-bt1
  q<-q+1
}

#Computing equation (4) in [2]

omegaTilde<-as.matrix(diag(omega)*(diag(Sigma)<=sqrt(n/log(p)))+sqrt(log(p)/n)*(diag(Sigma)>sqrt(n/log(p))))


####END Step1####

####Step 2####  
q=1
while (q<=p){
  
  #loading of lambda
  lambda<-tau*sqrt(omegaTilde[q])
  
  #loading the variable for the ADMM program
  
  Yt0<-as.matrix(rep(2,p))
  Yt1<-as.matrix(rep(3,p))
  bt0<-as.matrix(Sigma[,q])
  bt1<-as.matrix(rep(2,p))
  
  #solving for column q
  Dista<-1000
  while (Dista>cri) {
    #updating b
    g<-rho/eta*t(Sigmahat)%*%(2*Yt1-Yt0)
    v<-bt0-g
    #updating b by applying the softtreasholding
    bt1<-Soft(v,(eta))
    Yt0<-Yt1
    #updating z
    lambdat<-as.matrix(rep(lambda,p))
    zt1<-box(Sigmahat%*%bt1+Yt0,as.matrix(diag(p)[,q]),lambdat)
    #updating Y
    
    Yt1<-Yt0+Sigmahat%*%bt1-zt1
  
    #Compute the distance and update bt0 and bt1
    Dista<-sum((bt1-bt0)^2)
    bt0<-bt1
  }
  
  fakeinverse[,q]<-bt1
  q<-q+1
}


#symmetrisation of the fake inverse using (6) in [2])

for(i in 1:p){
  for(j in 1:p){
    finalinverse[i,j]<-fakeinverse[i,j]*(fakeinverse[i,j]<=fakeinverse[j,i])+fakeinverse[j,i]*(fakeinverse[i,j]>fakeinverse[j,i])
  }
}

return(finalinverse)}
