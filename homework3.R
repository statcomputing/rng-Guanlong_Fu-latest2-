# Homework 2
#Q2

#b)


# generate numbers follows mixture of gamma distribution

mixgamma <- function ( n, p, alpha, beta ) {
                                                        # if you only gen 1 interger, it will still follow the vector p
  k<- sample.int(length(p), n, replace= TRUE, prob = p) # it will sample the ith integer in length(p) according to the vector p
                                                         # n >= size to use replacement = FALSE
   l <- rgamma(n, shape = alpha[k], rate = 1/beta[k])   # rate = 1/beta
   #If there are m rate values, then each of the m random deviates 
   #will be chosen with the corresponding rate parameter. 
   
   return(l)
}

theta = 1 
p <- c( 2*gamma(theta)/(2*gamma(theta)+gamma(theta+0.5)), gamma(theta+0.5)/(2*gamma(theta)+gamma(theta+0.5)) )
alpha <- c ( theta, (theta+0.5))
beta  <- c( 1,1 )

x1 <- mixgamma(10000, p, alpha, beta)




# the true density function of g

mixgamma_d <- function (x,theta) {
g <- (2/(2*gamma(theta)+gamma(theta+0.5)))*x^(theta-1)*exp(-x)+(1/(2*gamma(theta)+gamma(theta+0.5)))*x^(theta-0.5)*exp(-x)

return(g)
}

# plot 
sample_d <- density(x1, n=10000)$y  # density from the sample

x_true <- seq(from= 0, to = 9.999, by = (10/10000))      # intervel 
true_d <- mixgamma_d(x_true,theta)
datafram1 <- data.frame(cbind(x_true,true_d))
datafram2 <- data.frame(cbind(x1,sample_d))
ggplot()+ geom_line(data=datafram1, aes(x=x_true, y=true_d))+
  geom_point(data=datafram2,aes(x=x1, sample_d), color='red')



              
#the two density almost coincide

#c)
# write a function to calculate the value of q(x)/g(x)

alfa <- function(x,theta) {
  
 alfa<- -(sqrt(4+x)*x^(theta-1)*exp(-x))/
  ((2/(2*gamma(theta)+gamma(theta+0.5)))*x^(theta-1)*exp(-x)+(1/(2*gamma(theta)+gamma(theta+0.5)))*x^(theta-0.5)*exp(-x))
  
 return(alfa)

 }
# find the optimal value of alfa
a <- -nlminb(0.2,alfa, theta = 1)$objective

# rejection sampling  


i <- 1
n2=1e4
x2 <- rep(0, n2)  # generate a empty vector of x2 for the final sample
while(i<= n2){

  u <- runif(1, min = 0, max = 1)      #initiate u
  x4 <- mixgamma(1, p, alpha, beta)    #initiate x4, which is a intermediate variable and changing within 'while' loop
  xr <- x4                             # initiate xr, if cond sati, we get xr without any loop
  while(u>(-1/a)*(alfa(x4,theta))) {     # comparing criteria to determin whether to continue drawing
    x4 <- mixgamma(1, p, alpha, beta)  # sampling new  x
    u <- runif(1, min = 0, max = 1)    # sampling new U
    xr <- x4                           # update xr, if this x4 satis condition, loop stops and we get xr, o.w. we get new x4 and xr
    }
   x2[i] =xr                     # you have to assign xr to x2[i] here, rather in the inner 'while'
i=i+1
}

# plot
# define f function
 f <- function(x, theta){
   return(sqrt(4+x)*x^(theta-1)*exp(-x))
 }


sample_d2 <- density(x2, n=10000)$y  # density from the sample
x_true2 <- seq(from= 0, to = 9.999, by = (10/10000))      # ture x2 intervel 
true_d2 <- f(x_true2,theta)
datafram3 <- data.frame(cbind(x_true2,true_d2))
datafram4 <- data.frame(cbind(x2,sample_d2))
ggplot()+ geom_line(data=datafram3, aes(x=x_true2, y=true_d2))+
  geom_line(data=datafram4,aes(x=x2, sample_d2), color='green')

plot(x_true2,true_d2)
par(new=TRUE)
plot(x2,sample_d2,axes=FALSE, col='green')

# density of x2 is cut off at around 0.5, why is this?

#Q3

#a)
# wright a function to sample from mixture beta


mixbeta <- function(x,n,a,b,lambda){


u2 <- runif(n, min = 0, max = 1)  # sample u2
v <- runif(n, min = 0, max = 1)   # sample v

    for (i in 1:length(u2)) {
      if (v[i]> lambda) {
        x[i]=u2[i]^(1/a)
     } else {
       x[i]=1-u2[i]^(1/b)
     }
    }
 return(x)
}


n3  <- 1e2
x3 <- rep(0,n3)
lambda <- 1/2
a <- 2
b <- 3

x_g<-mixbeta(x3,n3,a,b,lambda)

# write a function to calculate q(x)/g(x)
  
alpha2 <- function(x,a,b) {
  
return(  -(x^(a-1)/(1+x^2)+sqrt(2+x^2)*(1-x)^(b-1)) / ((1-lambda)*a*dbeta(x, a, 1)+lambda*b*dbeta(x,1, b))   )  
  
}
  
-alpha2(0.5,2,3)
# calculate the value of alpha
a2 <- -nlminb(0.3,alpha2, a=2, b=3)$objective   #sensitive the the chose of lambda

# rejection sampling

i <- 1
n6=1e4            # length of final sample
x6 <- rep(0, n6)  # for the final sample from f(x)
x8 <- rep(0, 1)  # for the sample from g(x)
while(i<= n6){
  
  u <- runif(1, min = 0, max = 1)      #initiate u
  x7 <- mixbeta(x8, 1, a,b, lambda)    #initiate x4, which is a intermediate variable and changing within 'while' loop
  xr2 <- x7  # initiate xr2, if cond sati, we get xr2 without any loop
  while(u>(-1/a2)*alpha2(x7,a,b)) {     # comparing criteria to determin whether to continue drawing
    x7 <- mixbeta(x8, 1, a, b, lambda)  # sampling new  x
    u <- runif(1, min = 0, max = 1)    # sampling new U
    xr2 <- x7                          # update xr2 at the end of each loop, if cond sat, we get xr2 and loop stops
    }
  x6[i] =xr2                     # you have to assign xr to x2[i] here, rather in the inner 'while'
  i=i+1
}

plot(density(x6, n=10000)$y)

#Q3
#b)
# generate a function calculate q1(x)/g1(x)

 ak1 <- function (x,a,b){
   return(  -(x^(a-1)/(1+x^2)) / a*dbeta(x, a, b)  )                                                                        
 }

 # generate a function calculate q2(x)/g2(x)
 ak2 <- function (x,a,b){
   return(  -sqrt(2+x^2)*(1-x)^(b-1) / b*dbeta(x, a, b)    )                                                                        
 }
 # calculate the value of ak1 and ak2 and assign the value to p proportionally
 a1 <- c( 2, 1)  #specify parameter values
 b1 <- c( 1, 3)
 
 ak11 <- -nlminb(0.3,ak1, a=2,b=1)$objective
 ak22 <- -nlminb(0.3,ak2, a=1,b=3)$objective
 a6 <- c(ak11,ak22)
 p3 <- c( ak11/(ak11+ak22), ak22/(ak11+ak22))

 
 # rejection sampling from qk(x)/gl(x)
 i <- 1
 n89=1e4
 x89 <- rep(0, n89)  # generate a empty vector of x2 for the final sample
 while(i<= n89){
   k3<- sample.int(length(p3), 1, replace= TRUE, prob = p3)  # initiate k3
   xgk <- rbeta(1, a1[k3],b1[k3])   # initiate xgk
   u6 <- runif(1, min = 0, max = 1) # initiate U
   xrr <- xgk  # initiate xrr, if cond sati, we get xrr before any loop
   # initiating: q(k)/g(k) using xk, for k =1 or 2 
   if (k3==1) {
     qgk <- ak1(xgk,a1[1],b1[1])
   } else {
     qgk <- ak2(xgk,a1[2],b1[2])
   }
   while(u6>(-1/a6[k3])*qgk) { # to evaluate the condition use the vars from the last round
     k3<- sample.int(length(p3), 1, replace= TRUE, prob = p3) # sample one sample k from 1,2 with prob= p31, p32 
     xgk <- rbeta(1, a1[k3],b1[k3])   # sample one xk from gk
     u6 <- runif(1, min = 0, max = 1)  # sample U
     xrr <- xgk  # update xrr, if cretia satisfied, loop stops, o.w. loop cont
     if (k3==1) {                     # calculate qk/gk whithin loop( actually part of the condition evaluation)
       qgk <- ak1(xgk,a1[1],b1[1])
     } else {
       qgk <- ak2(xgk,a1[2],b1[2])
     }
     
    }
   x89[i] =xrr                     # you have to assign xrr to x89[i] here, rather in the inner 'while'
   i=i+1
 
 }
 density(x89, n=10000)$y

