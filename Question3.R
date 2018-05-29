#Quant 2 Problem set 1 - Alessandro Vecchiato
setwd("/Users/Ale/Dropbox/NYU/QUANT II/PS")
library(ggplot2)
library(foreign)
data <- read.dta(file.choose())
data$X
#Part A - Histograms of the variables
#X
hist(data$X,main = paste("Histogram of X"), xlab="X")
#Y
hist(data$Y,main = paste("Histogram of Y"), xlab="Y")

#Part B - Simulations of convergence of the means
#Average of X
set.seed(1234)
z10<-matrix(,10,1000)
for (i in 1:1000) {
  z10[1:10,i]<-sample(data$X, 10)}
meanX10<-vector(,1000)
for (i in 1:1000){
meanX10[i]<-mean(z10[1:10,i])}
hist(meanX10, main="Histagram of X for n=10", xlab="X")

set.seed(1234)
z50<-matrix(,50,1000)
for (i in 1:1000) {
  z50[1:50,i]<-sample(data$X, 50)}
meanX50<-vector(,1000)
for (i in 1:1000){
  meanX50[i]<-mean(z50[1:50,i])}
hist(meanX50, main="Histagram of X for n=50", xlab="X")

set.seed(1234)
z250<-matrix(,250,1000)
for (i in 1:1000) {
  z250[1:250,i]<-sample(data$X, 250)}
meanX250<-vector(,1000)
for (i in 1:1000){
  meanX250[i]<-mean(z250[1:250,i])}
hist(meanX250, main="Histagram of X for n=250", xlab="X")

set.seed(1234)
z500<-matrix(,500,1000)
for (i in 1:1000) {
  z500[1:500,i]<-sample(data$X, 500)}
meanX500<-vector(,1000)
for (i in 1:1000){
  meanX500[i]<-mean(z500[1:250,i])}
hist(meanX500, main="Histagram of X for n=500", xlab="X")

#Average of Y
set.seed(1234)
z10<-matrix(,10,1000)
for (i in 1:1000) {
  z10[1:10,i]<-sample(data$Y, 10)}
meanX10<-vector(,1000)
for (i in 1:1000){
  meanY10[i]<-mean(z10[1:10,i])}
hist(meanY10, main="Histagram of Y for n=10", xlab="Y")

set.seed(1234)
z50<-matrix(,50,1000)
for (i in 1:1000) {
  z50[1:50,i]<-sample(data$Y, 50)}
meanY50<-vector(,1000)
for (i in 1:1000){
  meanY50[i]<-mean(z50[1:50,i])}
hist(meanY50, main="Histagram of Y for n=50", xlab="Y")

set.seed(1234)
z250<-matrix(,250,1000)
for (i in 1:1000) {
  z250[1:250,i]<-sample(data$Y, 250)}
meanY250<-vector(,1000)
for (i in 1:1000){
  meanY250[i]<-mean(z250[1:250,i])}
hist(meanY250, main="Histagram of Y for n=250", xlab="Y")


#Part C - Simulation of randomized experiment
#To develop the simulation I generate a function that has as input the sample size and as output the distribution of ATE. 
#Specifically, it computes the ATE following the expression provided, and generate independent graphs of the histogram of
#the ATE with a random normal distribution centered at the sample moments. 
rho_sim <- function(n) {
  A <- matrix(NA, nrow=1000, ncol=1)
  set.seed(12345)
  for (i in 1:1000) {
    dat_i <- data[sample(nrow(data),n),]
    A[i] <- mean(dat_i[1:(n/2),]$X) - mean(dat_i[(n/2+1):n,]$Y)
  }
  A <- as.data.frame(A)
  colnames(A)=c("Treatment")
  graph1 <- ggplot(A, aes(Treatment)) + 
    geom_histogram(fill="blue", colour="black", aes(y=..density..)) +
    stat_function(fun=dnorm, args=list(mean=mean(A$Treatment), sd=sd(A$Treatment)))
  return(graph1)
}
#Simulation for N=20
rho_sim(20)
#Simulation for N=100
rho_sim(100)
#Simulation for N=500
rho_sim(500)
#Simulation for N=1000
rho_sim(1000)