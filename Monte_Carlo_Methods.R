### 1.5 Correct assignments of letters to envelopes at random ###
m <- 100000		# number of trials
n <- 10		# number of envelopes/letters
x <- numeric(m)		# creates an empty vector of length m
for (i in 1:m){
	perm <- sample(1:n, n)
	x[i] <- sum(1:n==perm)
}
cutp <- (-1:n) + 0.5
hist(x, breaks=cutp, prob=T, ylim=c(0,0.4), col="palegreen3")


### 1.6 - Probability of being dealt a 5-card hand containing no aces ###
choose(48,5)/choose(52,5)
# alternatively
dhyper(0,4,48,5)

# via simulation #
m <- 100000		# number of simulations
deck <- 1:52	# where numbers 48-52 are aces
draw <- numeric(m)
numAces <- numeric(m)
for (i in 1:m){
	draw <- sample(deck, 5)
	numAces[i] <- sum(draw >= 49)	
}
mean(numAces==0)
table(numAces)/m


### 1.7 Martian birthdays ###
n <- 1:60		# vector of number of people in sample
p <- numeric(length(n))		# introduce empty vector of length n
for (i in n){
	q <- prod(1-(0:(i-1))/669)		# prob(no two have same birthday)
	p[i] <- 1 - q		# prob(at least two share the same birthday)
}
plot(n,p, type="l", xlab="Number of Martians", ylab="Probability of Two Matching Birthdays")
min(n[p>0.5])


### 1.8 nonuniform birthday question ###
m <- 100000
n <- 25		
x <- numeric(m)
p <- c(rep( 96,61), rep( 98,89), rep( 99,62), rep(100,61), rep(104,62), rep(106,30), 25)
for (i in 1:m){
	b <- sample(1:366, n, replace=T, prob=p)		# prob(no two have same birthday)
	x[i] <- n - length(unique(b))		# prob(at least two share the same birthday)
}
cutp <- (0:(max(x)+1)) - 0.5
hist(x, breaks=cutp, prob=T, ylim=c(0,0.5), col="aquamarine2")
plot(n,p, type="l")
min(n[p>0.5])


### Linear congruential pseudorandom number ###
# initialize
a <- 1093; b <- 18257; d <- 86436; s <- 7
m <- 1000; r <- numeric(m); r[1] <- s

# generate
for (i in 1:(m-1)){
	r[i+1] <- (a*r[i] + b) %% d
}
u <- (r+0.5)/d

# display
par(mfrow=c(1,2), pty="s")
	hist(u, breaks=10, col="wheat")
		abline(h=m/10, lty="dashed")
	u1 <- u[1:(m-1)]; u2 <- u[2:m]
	plot(u1,u2,pch=19, cex=0.5)
par(mfrow=c(1,1), pty="m")


### 3.1 Riemann sum approximation of an integral ###

m <- 10000; a <- 0; b <- 1		# define constants
w <- (b - a)/m		# w is the width of each triangle
g <- seq(a + w/2, b - w/2, length=m)		# sequence of 'grid points'
const <- 1/sqrt(2*pi)		# constant of the density function
h <- const * exp(-g^2/2)		# vector of m heights (of the triangles)
sum(w*h)		# total area


### Monte Carlo approximation of an integral ###
set.seed(12)
m <- 50000		# number of random points
a <- 0; b <- 1		# interval endpoints
w <- (b - a)/m		# width of triangles
u <- a + (b - a)*runif(m)		# vector of m random points
h <- dnorm(u)		# heights of density above random points
sum(w*h)


### Acceptance-Rejection approximation of an integral ###
set.seed(12)
m <- 50000
u <- runif(m, 0, 1)
h <- runif(m, 0, 0.4)
frac.acc <- mean(h < dnorm(u))
0.4*frac.acc 


### Random sample approximation of an intergral ###
set.seed(1212)
m <- 50000
a <- ; b <- 1
z <- rnorm(m)
mean(z > a & z <= b)


### Law of Large Numbers and Bernoulli trials ###
set.seed(1212)
m <- 10000
n <- 1:m
h <- rbinom(m, 1, 0.5)
y <- cumsum(h)/n
plot(n,y,type="l", ylim=c(0,1), xlab="Number of Trials", ylab="Probability")
abline(h=0.52, lty="dashed", col="red")
abline(h=0.48, lty="dashed", col="red")


### Wait time for bank tellers ###
set.seed(1212)
m <- 100000; lam1 <- 1/5; lam2 <- 1/4
x1 <- rexp(m, lam1); x2 <- rexp(m, lam2)
v <- pmin(x1, x2)
hist(v, prob=T, ylab="P{waiting time greater than x}")
xx <- seq(0, 15, 0.01); lines(xx, dexp(xx, 9/20))
mean(v > 5)


### Time to execute consecutive tasks ###
set.seed(1212)
m <- 100000; lam1 <- 1/5; lam2 <- 1/4
x1 <- rexp(m, lam1); x2 <- rexp(m, lam2); t <- x1 + x2
mean(t > 5); hist(t, prob=T)
# yy <- seq(0, 15, 0.01); lines(yy, dexp(yy))


### Stringing computer CPU for longenvity ###
set.seed(1212)
m <- 100000; n <- 5; lam <- 1/4
x <- rexp(m*n, lam)
DTA <- matrix(x, nrow=m)
w <- apply(DTA, 1, max)
hist(w, main="Lifetime of Stringed CPU", xlab="Years")
summary(w)

