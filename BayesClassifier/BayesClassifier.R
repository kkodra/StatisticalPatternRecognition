# Bayes classifier design: Quadratic Discriminant Analysis (QDA) example
# Linear Dicriminant Analysis (LDA) if covariances are the same i.e. set Sigma1 = Sigma2

# Set dimension and number of samples
d <- 2
N <- 10000
M1 <- rbind(0,0)
M2 <- rbind(0,0)

# Define covariance matrices
Sigma1 <- matrix(c(1,0.5,0.5,1),nrow=d,ncol=d)
Sigma2 <- matrix(c(1,-0.5,-0.5,1),nrow=d,ncol=d)

# Prior probabilites
P1 <- 0.5
P2 <- 0.5

###### 1. Generate N samples from each class #####
# Calculate eigenvectors and eigenvalues
e1 <- eigen(Sigma1,symmetric = TRUE)
values1 <- e1$values
vectors1 <- e1$vectors

e2<-eigen(Sigma2,symmetric = TRUE)
values2 <- e2$values
vectors2 <- e2$vectors

Phi1 <- vectors1
Phi2 <- vectors2

Lambda1 <- matrix(c(values1[1],0,0,values1[2]),nrowd=d,ncol=d)
Lambda2 <- matrix(c(values2[1],0,0,values2[2]),nrow=d,ncol=d)

Y1 <- replicate(2, rnorm(N,0,1))
Y2 <- replicate(2, rnorm(N,0,1))

X1 <- Phi1 %*% sqrt(Lambda1) %*% t(Y1)
X2 <- Phi2 %*% sqrt(Lambda2) %*% t(Y2)

# Plot the samples
X1_1 <- X1[1,1:N] + M1[1]
X1_2 <- X1[2,1:N] + M1[2]

X2_1 <- X2[1,1:N] + M2[1]
X2_2 <- X2[2,1:N] + M2[2]

plot(X1_1,X1_2, col="red",xlab="X1",ylab="X2")
points(X2_1,X2_2, col="blue")
abline(v=c(0,20), col="black")

legend(-3,3,legend=c("Sample 1", "Sample 2"),
       col=c("red","blue"), lty=1)

###### 2. Design Bayes classified for minimum error #####
# Refer to (3.11) in the book
M1_rep <- matrix(0,d,N)
M2_rep <- matrix(0,d,N)

h1 <- 0.5*t(X1-M1_rep) %*% solve(Sigma1) %*% (X1-M1_rep) - 0.5*t(X1-M2_rep) %*% solve(Sigma2) %*% (X1-M2_rep) + 0.5*log(det(Sigma1)/det(Sigma2))
h2 <- 0.5*t(X2-M2_rep) %*% solve(Sigma1) %*% (X2-M1_rep) - 0.5*t(X2-M1_rep) %*% solve(Sigma2) %*% (X2-M1_rep) + 0.5*log(det(Sigma1)/det(Sigma2))

# Get elements of interest (Diagonal values)
h1 <- diag(h1)
h2 <- diag(h2)

priorCondition <- log(0.5/0.5)

# Compare first X vector
class_X1 <- h1[h1 < priorCondition]
percClass1 <- length(class_X1)/N

class1_mis <- (N-length(class_X1))/N*100
sprintf("Percentage in class1: %.2f",percClass1*100)
sprintf("Percentage misclassified in class1: %.2f",class1_mis)

# Compare second X vector
class_X2 <- h2[h2 > priorCondition]
percClass2 <- length(class_X2)/N

class2_mis <- (N-length(class_X2))/N*100
sprintf("Percentage in class2: %.2f",percClass2*100)
sprintf("Percentage misclassified in class1: %.2f",class2_mis)

title(main=bquote(paste("Class 1 misclassification: ",.(class1_mis),"%  | Class 2 misclassification: ",.(class2_mis),"%")))
