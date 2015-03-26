###############################################
### Script to Compare Various Distributions ###
###############################################
set.seed(1989)
source("JohnsonGenerate.r")
source("Omega.r")
source("multiplot.r")
library('ggplot2')
library('reshape')
N = 10000


#first two moments set constant
MU = 3
SIGMA = 5
#try the params mu = 2, std = 1.6, skew = +/- 0.398, kurt = 3.84
#try normal(3,2.76) and mu = 3, std = 2.76, skew = 0, kurt = 9.6
#normal from johnson
z1 <- JohnsonGenerate(MU,SIGMA,0,3,N)
#excel kurtosis
z2 <- JohnsonGenerate(MU,SIGMA,0,10,N)
#some skewness
z3a <- JohnsonGenerate(MU,SIGMA,1,8,N)
z3b <- JohnsonGenerate(MU,SIGMA,-2,8,N)

#plot densities of the distributions
densities = data.frame(z1,z2,z3a,z3b)
colnames(densities) <- c("Skew 0, Kurt 3", "Skew 0, Kurt 10", "Skew 1, Kurt 8", "Skew -2, Kurt 8")
densities = melt(densities, measure = c("Skew 0, Kurt 3", "Skew 0, Kurt 10", "Skew 1, Kurt 8", "Skew -2, Kurt 8"))
colnames(densities) <- c("variable","returns")
pdf("plots/Densities.pdf", width=10)
qplot(returns, data=densities, geom="density", fill = variable, alpha = I(0.2), xlim = c(-10,17)) + labs(fill='Densities with mu = 3 and sigma = 5')
dev.off()

#plot DVA of 1000$ THIS NEEDS WORK IT IS BLOWING UP
initWealth = 1000
M = 50
paths = 30
z1Paths = matrix(nrow = M+1, ncol = paths)
z2Paths = matrix(nrow = M+1, ncol = paths)
z3aPaths = matrix(nrow = M+1, ncol = paths)
z3bPaths = matrix(nrow = M+1, ncol = paths)
z4Paths = matrix(nrow = M+1, ncol = paths)
z5Paths = matrix(nrow = M+1, ncol = paths)

z1Paths[1,] = z2Paths[1,] = z3aPaths[1,] = z3bPaths[1,] = z4Paths[1,] = z5Paths[1,] = initWealth
for (i in 1:paths) {
    z1Paths[-1,i] = initWealth*cumprod((JohnsonGenerate(3,5,0,3,M) + 100)/100)
    z2Paths[-1,i] = initWealth*cumprod((JohnsonGenerate(3,5,0,10,M) + 100)/100)
    z3aPaths[-1,i] = initWealth*cumprod((JohnsonGenerate(3,5,1,8,M) + 100)/100)
    z3bPaths[-1,i] = initWealth*cumprod((JohnsonGenerate(3,5,-2,8,M) + 100)/100)
    z4Paths[-1,i] = initWealth*cumprod((JohnsonGenerate(3,5,-4,20,M) + 100)/100)
    z5Paths[-1,i] = initWealth*cumprod((JohnsonGenerate(3,5,0,10,M) + 100)/100)
}
time = 1:nrow(z1Paths)
z1Paths = data.frame(time,z1Paths)
z1Paths = melt(z1Paths, id = "time")#, measure = c("X1", "X2", "X3", "X4", "X5","X6","X7","X8","X9","X10"))
z2Paths = data.frame(time,z2Paths)
z2Paths = melt(z2Paths, id = "time")#, measure = c("X1", "X2", "X3", "X4", "X5","X6","X7","X8","X9","X10"))
z3aPaths = data.frame(time,z3aPaths)
z3aPaths = melt(z3aPaths, id = "time")#, measure = c("X1", "X2", "X3", "X4", "X5","X6","X7","X8","X9","X10"))
z3bPaths = data.frame(time,z3bPaths)
z3bPaths = melt(z3bPaths, id = "time")#, measure = c("X1", "X2", "X3", "X4", "X5","X6","X7","X8","X9","X10"))
z4Paths = data.frame(time,z4Paths)
z4Paths = melt(z4Paths, id = "time")
z5Paths = data.frame(time,z5Paths)
z5Paths = melt(z5Paths, id = "time")

#set ylim so it is the max of all paths
p1 <- qplot(time, value, data = z1Paths, geom = "line", colour = variable, ylim = c(0,8000)) + theme(legend.position = "none") + ggtitle("Skew = 0, Kurtosis = 3") 
p2 <- qplot(time, value, data = z2Paths, geom = "line", colour = variable, ylim = c(0,8000)) + theme(legend.position = "none") + ggtitle("Skew = 0, Kurtosis = 10")
p3 <- qplot(time, value, data = z3aPaths, geom = "line", colour = variable, ylim = c(0,8000)) + theme(legend.position = "none") + ggtitle("Skew = 1, Kurtosis = 8")
p4 <- qplot(time, value, data = z3bPaths, geom = "line", colour = variable, ylim = c(0,8000)) + theme(legend.position = "none") + ggtitle("Skew = -2, Kurtosis = 8")
p5 <- qplot(time, value, data = z4Paths, geom = "line", colour = variable, ylim = c(0,8000)) + theme(legend.position = "none") + ggtitle("Skew = -4, Kurtosis = 20")
p6 <- qplot(time, value, data = z5Paths, geom = "line", colour = variable, ylim = c(0,8000)) + theme(legend.position = "none") + ggtitle("Skew = 0, Kurtosis = 10")
pdf("plots/DVApaths.pdf")
multiplot(p1,p2,p3,p4, cols=2)
dev.off()

pdf("plots/DVApaths2.pdf",width=15)
multiplot(p5,p6, cols=2)
dev.off()

#investigate omegas for the above distributions
L = seq(-2,6,0.5)
OmegaZ1 = Omega(z1,L)
OmegaZ2 = Omega(z2,L)
OmegaZ3a = Omega(z3a,L)
OmegaZ3b = Omega(z3b,L)
sharpe = cbind(L, 3/5)

omegas = data.frame(OmegaZ1,OmegaZ2[,2],OmegaZ3a[,2],OmegaZ3b[,2],sharpe[,2])
colnames(omegas) <- c("L","Skew 0, Kurt 3","Skew 0, Kurt 10","Skew 1, Kurt 3","Skew -2, Kurt 8","Sharpe")
omegas = melt(omegas, measure = c("Skew 0, Kurt 3","Skew 0, Kurt 10","Skew 1, Kurt 3","Skew -2, Kurt 8","Sharpe"))
colnames(omegas) <- c("L","variable","Omega")
pdf("plots/Preferences.pdf", width=10)
qplot(x=L, y=Omega, data=omegas, geom="line", colour = variable) + labs(colour="Johnson's w/ mean = 3, sigma = 5")
dev.off()

pdf("plots/PreferencesZoom.pdf", width=10)
qplot(x=L, y=Omega, data=omegas, geom="line", colour = variable) + labs(colour="Johnson's w/ mean = 3, sigma = 5") + xlim(2.5,6) + ylim(0,1.5)
dev.off()


#pdf("Preferences.pdf")
#ggplot(omegas, aes(L)) +
#geom_line(aes(y = Omega, colour = "Skew 0, Kurt 3")) +
#geom_line(aes(y = OmegaZ2, colour = "Skew 0, Kurt 10")) +
#geom_line(aes(y = OmegaZ3a, colour = "Skew 1, Kurt 3")) +
#geom_line(aes(y = OmegaZ3b, colour = "Skew -2, Kurt 8")) +
#geom_line(aes(y = sharpe, colour = "sharpe ratio")) +
#scale_colour_hue("3rd and 4th Moments")
#dev.off()

#analysis of empirical data
load('giltData.dat')
giltsDf = data.frame(gilt,gilt.rand.normal,gilt.rand.bimodal)
colnames(giltsDf) <- c("Empirical Gilts", "Normal", "Bimodal Normal")
giltDensities = melt(giltsDf, measure = c("Empirical Gilts", "Normal", "Bimodal Normal"))
colnames(giltDensities) <- c("variable","returns")
pdf("plots/EmpiricalDensities.pdf",width=10)
qplot(returns, data=giltDensities, geom="density", fill = variable, alpha = I(0.2)) + labs(fill='Densities with mu = 2.37 and variance = 2.12')
dev.off()

L = seq(-1,3,0.1)
sharpe = cbind(L, 3/5)
OmegaGilt = Omega(gilt, L)
OmegaNorm = Omega(gilt.rand.normal, L)
OmegaBimodal = Omega(gilt.rand.bimodal, L)

omegasEmpirical = data.frame(OmegaGilt, OmegaNorm[,2], OmegaBimodal[,2])
colnames(omegasEmpirical) <- c("L", "Empirical Gilts", "Normal", "Bimodal Normal")
omegasEmpirical = melt(omegasEmpirical, measure = c("Empirical Gilts", "Normal", "Bimodal Normal"))
colnames(omegasEmpirical) <- c("L","variable","Omega")
pdf("plots/EmpiricalOmegas.pdf", width=10)
qplot(x=L, y=Omega, data=omegasEmpirical, geom="line", colour = variable, ylim= c(0,500)) + labs(colour='Omegas for Densities')
dev.off()

