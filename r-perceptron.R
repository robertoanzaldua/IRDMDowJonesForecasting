# sigmoid function 
# input: x an array
# output: y = 1/ (1 + exp(-x))
sigmoid <- function(x) {
  return(1 / (1 + exp(-x)))
}

# threshold binary function 
# input: x an array
# output: y = ifelse(x > 0, 1, 0)
thresh.bin <- function(x) {
  return((x > 0) * 1.)
}

# function compute error
# input: 
#	model: list(weights, bias)
#	x: a n*d matrix in which each row is a datum point
#	y: a n*1 matrix in which each row is the target of x[i,]
# output: classification error
compute.error <- function(model, x, y) {
  n.cases <- dim(x)[1]
  err	<- sum((y - thresh.bin(x %*% model$weights + model$bias))^2)/n.cases
  return(err)
}

compute.accuracy <- function(model, x, y) {
  n.cases <- dim(x)[1]
  err	<- sum(abs(y - thresh.bin(x %*% model$weights + model$bias)))/n.cases
  return(1-err)
}

# online-learning perceptron
# input:
#	x: a n*d matrix in which each row is a datum point
#	y: a n*1 matrix in which each row is the target of x[i,]
#	maxit: the number of iterations for training
#	learn.rate: the learning rate for training
#	animation: TRUE for plotting examples and hyperplan (ONLY when d == 2)
#	ani.step: the graph is shown each ani.step
#	ani.delay: the delay time (sec) between two plots
# output: list(weights, bias, errors)
perceptron <- function(x, y, maxit = 100, learn.rate = 0.1,
                       stepbystep = FALSE, 
                       animation = FALSE, ani.step = 10, ani.delay = 0.5) {
  in.dim	<- dim(x)[2]
  n.cases	<- dim(x)[1]
  weights	<- runif(in.dim, min=-0, max=0)
  bias	<- runif(1, min=-0, max=0)
  errors	<- rep(0, maxit)
  act.function <- thresh.bin
 
  # 
  #cat('------ init -----\n')
  #cat('weights ', weights, '\n');
  #cat('bias ', bias, '\n');
  #cat('learning rate: ', learn.rate, '\n\n');
  
  for (i in 1:maxit) {
    # randomly pick an example and then predict its target
    id		<- floor(runif(1,min=1,max=n.cases+1))
    if (stepbystep == T) {
      id <- (i-1) %% n.cases + 1
      cat('pick x[',id,'] = ',x[id,],'\n')
      readline()
    }
    
    feat	<- x[id,]
    targ	<- y[id,]
    pred	<- act.function(sum(weights*feat)+bias)
    
    # update weights and bias
    weights <- weights + learn.rate * (targ-pred) * feat
    bias	<- bias + learn.rate * (targ-pred)
    
    if (stepbystep == T) {
      cat('target ', targ, '\n');
      cat('prediction ', pred, '\n');
      cat('weights ', weights, '\n');
      cat('bias ', bias, '\n\n\n');
    }
    
    # compute error
    errors[i] <- compute.error(list(weights = weights, bias = bias), x, y)
    
    # plot
    if (in.dim == 2 && animation == TRUE && i %% ani.step == 0) {
      d1.min <- min(x[,1]) - 2; d1.max <- max(x[,1]) + 2
      d2.min <- min(x[,2]) - 2; d2.max <- max(x[,2]) + 2
      
      plot(x[y==0,1], x[y==0,2], col='blue', 
           xlim = c(d1.min,d1.max), ylim = c(d2.min,d2.max),
           xlab = colnames(x)[1], ylab = colnames(x)[2])
      points(x[y==1,1], x[y==1,2], col='red')
      points(feat[1], feat[2], col='black')
      
      d1 <- seq(d1.min, d1.max, by=0.1)
      d2 <- (-bias - d1*weights[1]) / weights[2];
      lines(d1, d2)
      Sys.sleep(ani.delay)
    }
  }
  
  return(list(weights = weights, bias = bias, errors = errors))
}

# split data
# input: 
#	x: a n*d matrix in which each row is a datum point
#	y: a n*1 matrix in which each row is the target of x[i,]
#	ratio: n_train / n_test
# output: list(x.train, y.train, x.test, y.test)
split.data <- function(x, y, ratio) {
  ncases <- dim(x)[1]
  ntrain <- round(ncases * ratio / (ratio+1))
  list(
    x.train = x[1:ntrain,],
    y.train = as.matrix(y[1:ntrain,]),
    x.test = x[(ntrain+1):ncases,],
    y.test = as.matrix(y[(ntrain+1):ncases,]))
}

