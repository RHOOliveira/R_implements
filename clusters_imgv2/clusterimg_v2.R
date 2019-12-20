#==================================================================================
library(jpeg)
library(ggplot2)
#library(ggrepel)


img <- readJPEG("tenis.jpg") # Read the image
img
#dimension
iDm <- dim(img)
iDm
# RGB
RGB <- data.frame(
  x = rep(1:iDm[2], each = iDm[1]),
  y = rep(iDm[1]:1, iDm[2]),
  R = as.vector(img[,,1]),
  G = as.vector(img[,,2]),
  B = as.vector(img[,,3])
)

aux1 <- RGB[, c("R", "G", "B")]

kClusters <- 3
kMeans <- kmeans(RGB[, c("R", "G", "B")], centers = kClusters, nstart = 20)
legenda <- rgb(kMeans$centers)

#principio de reciclagem 
kColours <- rgb(kMeans$centers[kMeans$cluster,])

vec2mat.c <- matrix(kMeans$cluster, ncol=iDm[2])


# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# =============================================================

posit.leg <- function(ii, iDm, vec2mat.c){
  
  indices.cluster1<-which(vec2mat.c==ii, arr.ind = TRUE )
  
  
  moda1 <- getmode(indices.cluster1[,2])   #pos x [,2] y (y inicia do topo da img )
  moda1y <- getmode(indices.cluster1[,1])
  
  posy <- iDm[1]-moda1y
  
  # if((moda1 < 10)||(posy<10)){}
  if(moda1<10){
    moda1 <- moda1 + 50
  }
  
  if(posy<10){
    posy <- posy + 50
  }
  
  if(moda1 > iDm[2]-25){
    moda1 <- moda1 - 50
  }
  
  if(posy>iDm[1]-25){
    posy <- posy - 50
  }
  
  
  
  return(c(moda1,posy))
}



xy <- c()
for(i in 1:kClusters){
  
  xy1 <- posit.leg(i,iDm,vec2mat.c)
  xy <- c(xy,xy1) 
}
xy


#library(ggplot2)
#plot.new()
leg.char <- as.character(legenda)

plotar <- ggplot(data = RGB, aes(x = x, y = y)) + 
  geom_point(colour = kColours) +
  labs(title = paste("k-Means :", kClusters, "Clusters")) 
for(i in 1:kClusters){
  plotar <- plotar + annotate(geom="label", x=xy[1+2*(i-1)], y=xy[2+2*(i-1)], label=leg.char[i],
                              color="white", fill=leg.char[i])
  #plotar <- geom_label(aes(x=xy[1+2*(i-1)],  y=xy[2+2*(i-1)], label = leg.char[i]) , fill = leg.char[i])
}

plotar


file <- paste('img','com',kClusters,"clusters2.jpeg", sep = '_')
#ggsave(file, scale = 1,width = 19.87, height = 19.05, units = c('cm','cm'))
#ggsave(file)
