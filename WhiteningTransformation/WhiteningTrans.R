# Implementation of the Whitening transformation
# Set parameters: dimension -> d, number of samples -> N, center -> M
d <- 2
N <- 10000
M <- rbind(1,2)
Sigma <- matrix(c(4,4,4,9),nrow=2,ncol=2)

# Calculate eigenvectors and eigenvalues
e <- eigen(Sigma,symmetric = TRUE)
values <- e$values
vectors <- e$vectors

Phi <- vectors
Lambda <- matrix(c(values[1],0,0,values[2]),nrow=2,ncol=2)

# Check that Sigma_Y is identity
Lambda_sqrt_inv <- solve(sqrt(Lambda))
Sigma_Y <- Lambda_sqrt_inv %*% t(Phi) %*% Sigma %*% Phi %*% Lambda_sqrt_inv

Y<-replicate(2, rnorm(N,0,1))
X <- Phi %*% sqrt(Lambda) %*% t(Y)

X1 <- X[1,1:N] + M[1]
X2 <- X[2,1:N] + M[2]
plot(X1,X2)

# Whitened distribution
Yw = Lambda_sqrt_inv %*% t(Phi) %*% X

Y1 <- Yw[1,1:N] + M[1]
Y2 <- Yw[2,1:N] + M[2]
points(Y1,Y2,col="green")

points(M[1],M[2],col="red",lwd=2)
legend(-5,10,legend=c("Original", "Whitened","Center"),
       col=c("black", "green","red"), lty=1)


