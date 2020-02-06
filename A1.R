library(jrvFinance)
library(lubridate)
#import data
bonds <- read.csv("A1.csv")
#verify the data
bonds<-as.data.frame(bonds)
rownames(bonds) <- seq(length=nrow(bonds))
bonds[,3]<-as.Date(bonds[,3])
bonds[,5]<-as.Date(bonds[,5], "%d/%m/%y")
bonds$coupon <-as.numeric(sub("%","",bonds$coupon))
bonds$price <- as.numeric(bonds$price)
#set vector to store ytm
ytm = numeric(100)
#calculate ytm 
for (i in 1:100)
{ytm[i] <- bond.yield(bonds$date[i],bonds$maturity[i],bonds$coupon[i]/100,price=bonds$price[i],convention = "ACT/ACT")}
bonds$ytm <- ytm
View(bonds)
#initialize date vector
x<-seq(as.Date("2020/1/02"), as.Date("2020/1/15"), "days")
x<-c(x[1:2],x[5:9],x[12:14])
x1 <- rep(x[1],10)
X <- data.frame(x1) 
for( i in c(2:10)){
  v = rep(x[i],10)
  X <- cbind(X,v)
}
for( j in c(1:10)){
  for (i in c(1:10)) {
    month(X[i,j])<-month(X[i,j])+6*i
  }
}
colnames(X)<-x
#Use bootstrapping to calculate the spot curve
#r stores spot rates, r[i,j] is the approximate spot rates for i-th date we observed +0.5i*year
r <- matrix(0, nrow = 10, ncol = 10)
r <- as.data.frame(r)
colnames(r) <- x
row.names(r) <- c("0.5 year","1 year","1.5 year", "2 year","2.5 year","3 year","3.5 year", "4 year",
          "4.5 year","5 year")
#calculate the first yield(0.5 year yield)
for(i in 1:10)
{ index = 10*(i-1)+1
  r[1,i] = -2 * log(bonds$price[index]/((bonds$coupon[index]/2)+100))}
#calculate the following yields
for(j in 2:10){
  for( i in 1:10){
    index <- 10 *(i-1) +  j
    k = numeric(j-1)
    for( m in 1:j-1){k[m]<--m*r[m,i]}
    r[j,i] = -(log((bonds$price[index] - sum(0.5 * bonds$coupon[index]*(exp(k))))/(0.5*bonds$coupon[index]+100)))/(0.5*j)
  }
}
view(r)
R<-as.data.frame(unlist(r))
colnames(R)<-c("yield")
rownames(R) <- seq(length=nrow(R))
bonds<- cbind(bonds,R)

write.csv(r,"spot-rates.csv")
write.csv(bonds,"bonds.csv")

#foward rates
#set up the data frame that will store the foward rates
f<-rep(1,4)
f<-data.frame(f,f,f,f,f,f,f,f,f,f)
row.names(f) <- c("f12","f13","f14","f15")
#calculate foward rates
for(i in 1:10){
for(j in 1:4){
  j2 = 2*j + 2
      f[j,i] =(((1+r[j2,i])^(j2))/((1+r[2,i])^(2))-1)
}}
colnames(f)<- x
view(f)
write.csv(f,"foward-rate.csv")
#plotting the curve
#set colors
plot_colours <- c("blue", "red", "forestgreen", "yellow", rgb(0.3,0.3,.3),
                  rgb(0.6,0.3,0), rgb(.9,0,0), 
                  rgb(0.3,0.6,0), rgb(0.3,0,.6), rgb(0,0.3,.6))
i1 <- seq(1,10)

#plot the yield curve
png(file="Yield Curve.png", width = 960, height = 480)
plot(bonds$maturity[i1],100*bonds$ytm[i1],col=plot_colours[1],type="l",ylab = "ytm(%)",xlab="Date(year)",ylim=c(1.53,1.83))
for(i in 2:10)
{ i1 <- seq((i-1)*10+1,10*i)
  lines(bonds$maturity[i1],100*bonds$ytm[i1],col=plot_colours[i])}
legend("topright", 1.2, paste("Yield Curve for ", 
                              bonds$date[1:10],
                              sep=""), lty=c(1,1), 
       lwd=c(2,2),cex=.8, bty = "n", col=plot_colours)
dev.off()

#plot the spot curve
png(file="Spot curve.png", width = 960, height = 480)
plot(X[[1]],r[[1]]*100,col=plot_colours[1],type="l",ylab = "spot rate(%)",xlab="Date(year)",ylim=c(1.45,2.2))
for(i in 2:10)
{lines(X[[i]],r[[i]]*100,col=plot_colours[i])}
legend("topright", 1.2, paste("Spot Curve for ", 
                              bonds$date[1:10],
                              sep=""), lty=c(1,1), 
       lwd=c(2,2),cex=.8, bty = "n", col=plot_colours)
dev.off()

#forward curve
png(file="Foward curve.png", width = 960, height = 480)
plot(f[[1]]*100,col=plot_colours[1],type="l",ylab = "foward rate(%)",xlab="year")
for(i in 2:10)
{lines(f[[i]]*100,col=plot_colours[i])}
legend("topright", 1.2, paste("Foward Curve for ", 
                              bonds$date[1:10],
                              sep=""), lty=c(1,1), 
       lwd=c(2,2),cex=.8, bty = "n", col=plot_colours)
dev.off()
#Covariance matrices
#matrix to store the daily log returns
log_return<-matrix(nrow=9,ncol=5)
#Calculate log-returns of yields for the time series, ith year, jth date
for(i in 1:5){
  for(j in 1:9){
    index = i * 2
    m = j+1
    log_return[j,i] = log(r[index,m]/r[index,j])
  }
}
#show the daily log returns of yield table
colnames(log_return)<-c("X1","X2","X3","X4","X5")
log_return <- as.data.frame(log_return)
view(log_return)
write.csv(log_return,"log_return.csv")
#covariance table for the daily log returns of yield table
C1 <- as.data.frame(cov(log_return))
View(C1)
write.csv(C1,"Covariance for log return.csv")
#covariance table for the 1-year foward rate
#arrange f to be in 4 variables timeseries 
tf <- as.data.frame(as.numeric(f[1,]))
row.names(tf)<-x
colnames(tf) <- "f12"
tf$f13 <- as.numeric(f[2,])
tf$f14 <- as.numeric(f[3,])
tf$f15 <- as.numeric(f[4,])
view(tf)
write.csv(tf,"time series of foward rates.csv")
#the covariance table for the timeseries of foward rates
C2 <- cov(tf)
view(C2)
write.csv(C2,"Covariance for foward rates.csv")

#Get the eigenvalues and eigenvectors
#store the vectors
eg1v <- as.data.frame(eigen(C1)$vectors)
eg2v <- as.data.frame(eigen(C2)$vectors)
view(eg1v)
view(eg2v)
write.csv(eg1v,"eigenvectors1.csv")
write.csv(eg1v,"eigenvectors2.csv")
#show the eigenvalues
eigen(C1)$values
eigen(C2)$values
