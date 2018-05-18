############################################################################
#######           Laplace  Function - not to be used in this code   ########
#Prepared it for the noise injection - privacy preserved version of K-Means#
############################################################################
rlaplace = function(n,mu,sigma){
  U = runif(n,0,1)
  sign = ifelse(rbinom(n,1,.5)>.5,1,-1)     
  y = mu + sign*sigma/sqrt(2)*log(1-U)  
  y
}
epsilon = 10/6
lap <- rlaplace(1,0,((2*25)/epsilon)*((2*25)/epsilon))
#######################################################################
#############           Distance Function               ###############
#######################################################################
euclid <- function(points1, points2) {
  distanceMatrix <- matrix(NA, nrow=dim(points1)[1], ncol=dim(points2)[1])
  for(i in 1:nrow(points2)) {
    distanceMatrix[,i] <- sqrt(rowSums(t(t(points1)-points2[i,])^2))
  }
  distanceMatrix
}
#######################################################################
#################           My K-Means!               #################
#######################################################################
K_means <- function(x, centers, distFun, nItter) {
  clusterHistory <- vector(nItter, mode="list")
  centerHistory <- vector(nItter, mode="list")
  epsilon = 10/6
  lap <- rlaplace(1,0,((2*25)/epsilon)*((2*25)/epsilon))
  for(i in 1:nItter) {
    distsToCenters <- distFun(x, centers)
    clusters <- apply(distsToCenters, 1, which.min)
    centers <- apply(x, 2, tapply, clusters, mean)
    #xx = sapply(x, as.numeric)
    #cc = sapply(clusters, as.numeric)
    #sse <- apply(xx, 2, tapply, clusters, sum( (xx - mean )^2 ))
    permcenter = sapply(centers, as.numeric)
    perm_center <- matrix(permcenter, ncol=2, nrow=3)
    clusterHistory[[i]] <- clusters
    centerHistory[[i]] <- centers
    #sseHistory[[i]] <- sse
  }
  list(clusters=clusterHistory, centers=centerHistory)#, sse = sseHistory[[i]] <- sse )
}
#######################################################################
#################            Data From Dataset        #################
#######################################################################
data = as.matrix(read.csv("dataset.csv"))
test=data # A data.frame
ktest=as.matrix(test) # Turn into a matrix
centers <- ktest[sample(nrow(ktest), 3),]
res <- K_means(ktest, centers, euclid, 15)
#######################################################################
###############               PLOT to PDF             #################
#######################################################################
somePDFPath = "C:\\Users\\BRG_user\\Desktop\\MyProjects\\Privacy\\plot.pdf"
pdf(file=somePDFPath) 
lap = 0.15
for (i in seq(1,15))   
{   
  perm = as.vector(sapply(res$clusters[i], as.numeric))
  permcenter = sapply(res$centers[i], as.numeric)
  #lap_permcenter= permcenter +  lap
  perm_center <- matrix(permcenter, ncol=2, nrow=3)
  plot(data[,1], data[,2],col=perm+1)
  #perm_center[1,1] = perm_center[1,1] + lap
  #perm_center[1,2] = perm_center[1,2] + lap
  #perm_center[2,1] = perm_center[2,1] + lap
  #perm_center[2,2] = perm_center[2,2] + lap
  #perm_center[3,1] = perm_center[3,1] + lap
  #perm_center[3,2] = perm_center[3,2] + lap
  
  points(perm_center, pch=16)
} 
dev.off()
#######################################################################
#################              PLOT                   #################
#######################################################################
#perm = as.vector(sapply(res$clusters[1], as.numeric))
#plot(x, y,col=perm)
#points(res$center, pch=16)
#######################################################################
#################           Random Data               #################
#######################################################################
#y=rnorm(5,1.65)
#x=rnorm(5,1.15)
#data=cbind(x,y)
#test=data # A data.frame
#ktest=as.matrix(test) # Turn into a matrix
#centers <- ktest[sample(nrow(ktest), 3),] 
#res <- K_means(ktest, centers, euclid, 3)
