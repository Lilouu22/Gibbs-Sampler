#Devoir 5 : Lisa Pamela Valente et Émilie Zhang
xi = c(2.4,3.8,0.7,8.8,5.1)
sumx = sum(x)
#on pose les paramètres
it = 200000
bi= 10000 #temps de chauffe
theta = 1 #paramètre de moyenne
mu = 1
tau = 1  #associé à variancce
sigma = sqrt(2)
sigma2 =2
theta.vec=c()

for(i in 1:it){
   theta=rnorm(1,(sum(x)+tau*sigma2*mu)/(5+tau*sigma2),sigma/sqrt(5+tau*sigma2))
   tau = rgamma(1,5/2,0.5*((theta-mu)^2+1))
   mu = rnorm(1,theta,sqrt(1/tau))
   theta.vesc=c(theta.vec, theta)
   #theta
   #tau
   #mu
}
prob = sum(theta.vec[-(1:bi)]<=3)/(it-bi)  #autour de 0.0327

#on peut faire un histogramme

