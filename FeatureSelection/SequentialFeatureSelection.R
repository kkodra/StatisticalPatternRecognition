# Sequential feature selection
# Work in progress ...


# Class 1 mean
f_C1_m <- 1; 

# Class 2 mean
f_C2_m <- -1; 

f1_std <- sqrt(10)
f2_std <- sqrt(5)
f3_std <- sqrt(3.33)
f4_std <- sqrt(1.43)

# Generate a sequence of points
x <- seq(-30,30,by=0.01)

dist1_C1 <- dnorm(x,f_C1_m,f1_std)
dist2_C1 <- dnorm(x,f_C1_m,f2_std)
dist3_C1 <- dnorm(x,f_C1_m,f3_std)
dist4_C1 <- dnorm(x,f_C1_m,f4_std)

dist1_C2 <- dnorm(x,f_C2_m,f1_std)
dist2_C2 <- dnorm(x,f_C2_m,f2_std)
dist3_C2 <- dnorm(x,f_C2_m,f3_std)
dist4_C2 <- dnorm(x,f_C2_m,f4_std)

dev.new()
plot(x,dist1_C1,col='green',ylim=range(0,0.5),ylab='Probability Density',title(expression(paste(mu, ' = 1',mu, ' = -1'))))
points(x,dist1_C2,col='blue')

dev.new()
plot(x,dist2_C1,col='green',ylim=range(0,0.5),ylab='Probability Density',title(expression(paste(mu, ' = -1'))))
points(x,dist2_C2,col='blue')
