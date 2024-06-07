x<-sample(-100:100, size=100)
x<--100:100
y<- -1 * x[x%%2 != 0]
y


catX <- x[x <= -30] <- 0
catX <- x[x > -30 & x<= 3 ] <- 1
catX <- x[x > 3 ] <- 2
catX

y <- x[catX ==1 | catX ==2]

z <- x[catX ==1 | catX ==2 & x%%2 != 0]
