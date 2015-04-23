PCA<- function(k, M){
	j<-1
	shift <-matrix(0, dim(M)[1], dim(M)[2])
	while(j<dim(M)[2]+1){
		ave <- mean(M[,j])
		t <- 1
		while(t<dim(M)[1]+1){
			ave->shift[t, j]
			t<-t+1
		}
		j <- j+1
	}
	
	centered <- M-shift
	
	eigs <- eigen(centered%*%t(centered), symmetric = TRUE)
	i <- 1
	projection <- matrix(0, dim(centered)[1], dim(centered)[2])
	while(i<k+1){
		vector <- as.matrix(eigs$vectors[,i])
		vector <- vector/(norm(vector))
		projection <- projection + eigs$values[i]*(vector %*% t(vector))
		i <- i+1
	}
	
	approx <-(1/(dim(M)[2]))*projection%*%centered + shift
	image(approx, axes=FALSE, col=grey(seq(0, 1, length=256)))
}