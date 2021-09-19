##Ecuaciones de primer orden
y0 <- 8
a <- 4
p <- .6
t <- c(0:20)
## Solución matematica
yt <- (a/(1-p))+p^t*(y0-(a/(1-p)))
plot(t,yt, type = "l", xlab="t", ylab="yr", ylim = c(summary(yt)[1]-.5,summary(yt)[6]+.5), xlim = c(0,20))
abline(h=a/(1-p), col="blue", lwd=2, lty=2)

A <- c(y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20)
t <- c(0:20)
D <- cbind(t,A)

plot(D, type = "l", xlab="t", ylab="yr", ylim = c(summary(A)[1]-.5,summary(A)[6]+.5), xlim = c(0,20))

## Solución recursiva

ed1 <- function(alpha, phi, n, y0) {
  t <- 0:n
  y <- vector(mode="double", length = n+1)
  y[1] <- y0
  for (i in 2:(n+1)) y[i] <- alpha + phi*y[i-1]
  plot(t,y, type = "l", main = "Ecuaciones en diferencias, orden 1", ylim = c(summary(y)[1]-.5,summary(y)[6]+.5), xlim = c(0,20))
  abline(h=alpha/(1-phi), col="blue", lwd=2, lty=2)
}

##############################################################################

##Ecuaciones de segundo orden

a <- 10
p1 <- 0
p2 <- -.4

ypt <- a/(1-p1-p2)
d <- (p1^2)+(4*p2)
d

y0 <- 12
y1 <- 7

## Si d>0
l1 <- (p1/2)+(1/2)*(d)^(1/2)
l2 <- (p1/2)-(1/2)*(d)^(1/2)
c(l1, l2)
##solución general
#yt <- ypt+(A1*(l1)^t)+(A2*(l2)^t)

#Resolvemos para t=0
## y0 <- ypt+A1+A2
#Resolvemos para t=1
## y1 <- ypt+(A1*l1)+(A2*l2)
## Usaremos A1 de t=0
# A1 <- y0-ypt-A2
## Sustituiremos en t=1
# y1 <- ypt+((y0-ypt-A2)*l1)+(A2*l2)
## Despejamos A2
A2 <- (-ypt-(l1*(y0-ypt))+y1)/(l2-l1)
A1 <- y0-ypt-A2
c(A1, A2)
## Ya podemos usar la solucion general
#yt <- ypt+(A1*(l1)^t)+(A2*(l2)^t)

A <- c(y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20)
t <- c(0:20)
D <- cbind(t,A)

plot(D, type = "l", xlab="t", ylab="yr", ylim = c(summary(A)[1]-.5,summary(A)[6]+.5), xlim = c(0,20))

## Solución matematica
yt <- ypt+(A1*(l1)^t)+(A2*(l2)^t)
plot(t,yt, type = "l", xlab="t", ylab="yr", ylim = c(summary(yt)[1]-.5,summary(yt)[6]+.5), xlim = c(0,20))

##############################################################################

a <- 4
p1 <- .8
p2 <- -.16

ypt <- a/(1-p1-p2)
d <- (p1^2)+(4*p2)

y0 <- 6
y1 <- 9


## Si d=0
l <- p1/2

## Solución general
# yt <- ypt+(A1*(l)^t)+(A2*t*(l)^t)
## Resolvemos para t=0
# y0 <- ypt+A1
A1 <- y0-ypt
## Resolvemos para t=1
# y1 <- ypt+(A1*(l))+(A2*(l))
A2 <- (y1-ypt-(A1*l))/l

## Ahora podemos usar la solución general
# yt <- ypt+(A1*(l)^t)+(A2*t*(l)^t)


## Solución matematica
t <- c(0:20)
yt <- ypt+(A1*(l)^t)+(A2*t*(l)^t)
plot(t,yt, type = "l", xlab="t", ylab="yr", ylim = c(summary(yt)[1]-.5,summary(yt)[6]+.5), xlim = c(0,20))

##############################################################################

## Si d<0

a <- 10
p1 <- 0
p2 <- -.4

ypt <- a/(1-p1-p2)
d <- (p1^2)+(4*p2)
d

y0 <- 5
y1 <- 8

l1 <- complex(real=(p1/2), imaginary=((1/2)*(-d)^(1/2)))
l2 <- complex(real=(p1/2), imaginary=-((1/2)*(-d)^(1/2)))
c(l1,l2)

a1 <- p1/2
b1 <- as.complex((1/2)*(-d)^(1/2))
b1 <- 0.6324555

R <- (a1^2+b1^2)^(1/2)

costh <- a1/R
senth <- b1/R

th <- acos(costh)
th <- asin(senth)


l12 <- R*complex(length.out = 1, real = costh, imaginary = senth)
l22 <- R*complex(length.out=1, real = costh, imaginary = -senth)

## En solución general
# l1t <- (R*complex(real = costh, imaginary = senth))^t
# l2t <- (R*complex(real = costh, imaginary = -senth))^t
# yt <- ypt+(A1*R^t*l12)+(A2*R^t*l22)
# yt <- ypt+R^t(A3*cos(th*t)+A4*sin(th*t))

## Si t = 0
# y0 <- ypt+R^0(A3*cos(th*0)+A4*sin(th*0))
## Si t = 1
# y1 <- ypt+R^1(A3*cos(th*1)+A4*sin(th*1))
## Despejamos A3 de t=0
A3 <- (((y0-ypt)/(R^0))-sin(th*0))/(cos(th*0))

## Sustituimos en t=1 y despejamos A4
A4 <- (((y1-ypt)/(R^1))-(A3*cos(th)))/sin(th)
c(A3, A4)
## Ahora podemos usar la solución general
# yt <- ypt+R^t(A3*cos(th*t)+A4*sin(th*t))

## Solución matematica
t <- c(0:20)
yt <- ypt+R^t*(A3*cos(th*t)+A4*sin(th*t))

plot(t,yt, type = "l", xlab="t", ylab="y(t)", ylim = c(summary(yt)[1]-.5,summary(yt)[6]+.5), xlim = c(0,20))
abline(h=a/(1-p1-p2), col="blue", lwd=2, lty=2)  
## Solución recursiva

ed2 <- function(alpha, phi1, phi2, y0, y1, n){
  t <- 0:(n+1)
  y <- vector(mode = "double", length=(n+2))
  y[1] <- y0
  y[2] <- y1
  for (i in 3:(n+2)) y[i] <- alpha+phi1*y[i-1]+phi2*y[i-2]
  plot(t,y, type = "l", main = "Ecuaciones en diferencias, orden 2", ylim = c(summary(y)[1]-.5,summary(y)[6]+.5), xlim = c(0,20))
  abline(h=alpha/(1-phi1-phi2), col="blue", lwd=2, lty=2)  
}





