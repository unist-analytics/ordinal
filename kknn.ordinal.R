## this fucntion is the base for akknn_type.
## this function represents awknn method, which is modification of knn method for the imbalanced data set
## and case of ordinal classification.

options(stringsAsFactors = FALSE)
# to stop to handle strings as factors.

kknn.ordinal<-function (formula = formula(train), train, test, na.action = na.omit(), param=0.5,
          k = 7, distance = 2, kernel = "optimal", ykernel = NULL, 
          scale = TRUE, contrasts = c(unordered = "contr.dummy", ordered = "contr.ordinal")) {
  if (is.null(ykernel)) 
    ykernel = 0
  weight.y = function(l = 1, diff = 0) {
    k = diff + 1
    result = matrix(0, l, l)
    # to make a 1x1 matrix composed of 0       
    diag(result) = k
    # diag function finds the entry of the diagonal of matrix, which returns 0 in this case.
    # in the result, the entry is chaged from 0 to 1.        
    for (i in 1:(k - 1))
    # i in 1:0          
    {
      for (j in 1:(l - i)) {
        result[j, j + i] = k - i
        result[j + i, j] = k - i
      }
    }
    result
  }
  kernel <- match.arg(kernel, c("rectangular", "triangular", 
                                "epanechnikov", "biweight", "triweight", "cos", "inv", 
                                "gaussian", "rank", "optimal"), FALSE)
  # to find the same function for kerenl parameter.                   
  ca <- match.call()
  response = NULL
  old.contrasts <- getOption("contrasts")
  options(contrasts = contrasts)
  formula = as.formula(formula)
  mf <- model.frame(formula, data = train)
  mt <- attr(mf, "terms")
  mt2 <- delete.response(mt)
  cl <- model.response(mf)
  d <- sum(attr(mt, "order"))
  if (is.ordered(cl)) {
    response <- "ordinal"
    lev <- levels(cl)
  }
  if (is.numeric(cl)) 
    response <- "continuous"
  if (is.factor(cl) & !is.ordered(cl)) {
    response <- "nominal"
    lev <- levels(cl)
  }
  # to sort the response variable's type; ordinal, continuos, nominal.        
  if (distance <= 0) 
    stop("distance must >0")
  if (k <= 0) 
    stop("k must >0")
  learn <- model.matrix(mt, mf)
  valid <- model.matrix(mt2, test)
  m <- dim(learn)[1]
  p <- dim(valid)[1]
  q <- dim(learn)[2]
  ind <- attributes(learn)$assign
  d.sd <- numeric(length(ind)) + 1
  we <- numeric(length(ind)) + 1
  d.sd = apply(learn, 2, stats::var)
  for (i in unique(ind)) {
    d.sd[ind == i] = sqrt(mean(d.sd[ind == i]))
    we[ind == i] = 1/sum(ind == i)
  }
  we[d.sd == 0] = 0
  d.sd[d.sd == 0] = 1
  if (scale) {
    learn <- sweep(learn, 2L, d.sd, "/", check.margin = FALSE)
    valid <- sweep(valid, 2L, d.sd, "/", check.margin = FALSE)
  }
  ord = order(we * apply(learn, 2, sd), decreasing = TRUE)
  we = we[ord]
  learn = learn[, ord, drop = FALSE]
  valid = valid[, ord, drop = FALSE]
  Euclid <- FALSE
  if (distance == 2) 
    Euclid <- TRUE
  if (Euclid) 
    dmtmp <- .C("dmEuclid", as.double(learn), as.double(valid), 
                as.integer(m), as.integer(p), as.integer(q), dm = double((k + 
                                                                            1L) * p), cl = integer((k + 1L) * p), k = as.integer(k + 
                                                                                                                                   1), as.double(distance), as.double(we), dup = FALSE, 
                PACKAGE = "kknn")
  else dmtmp <- .C("dm", as.double(learn), as.double(valid), 
                   as.integer(m), as.integer(p), as.integer(q), dm = double((k + 
                                                                               1L) * p), cl = integer((k + 1L) * p), k = as.integer(k + 
                                                                                                                                      1), as.double(distance), as.double(we), dup = FALSE, 
                   PACKAGE = "kknn")
  D <- matrix(dmtmp$dm, nrow = p, ncol = k + 1)
  C <- matrix(dmtmp$cl, nrow = p, ncol = k + 1)
  maxdist <- D[, k + 1]
  maxdist[maxdist < 1e-06] <- 1e-06
  D <- D[, 1:k]
  C <- C[, 1:k] + 1
  CL <- matrix(cl[C], nrow = p, ncol = k)
  if (response != "continuous") {
    l <- length(lev)
    weightClass <- matrix(0, p, l)
  }
  if (response == "continuous") {
    weightClass <- NULL
  }
  W <- D/maxdist
  W <- pmin(W, 1 - (1e-06))
  W <- pmax(W, 1e-06)
  if (kernel == "rank") 
    W <- (k + 1) - t(apply(as.matrix(D), 1, rank))
  if (kernel == "inv") 
    W <- 1/W
  if (kernel == "rectangular") 
    W <- matrix(1, nrow = p, ncol = k)
  if (kernel == "triangular") 
    W <- 1 - W
  if (kernel == "epanechnikov") 
    W <- 0.75 * (1 - W^2)
  if (kernel == "biweight") 
    W <- dbeta((W + 1)/2, 3, 3)
  if (kernel == "triweight") 
    W <- dbeta((W + 1)/2, 4, 4)
  if (kernel == "cos") 
    W <- cos(W * pi/2)
  if (kernel == "triweights") 
    W <- 1
  if (kernel == "gaussian") {
    alpha = 1/(2 * (k + 1))
    qua = abs(qnorm(alpha))
    W = W * qua
    W = dnorm(W, sd = 1)
  }
  if (kernel == "optimal") {
    W = rep(optKernel(k, d = d), each = p)
  }
  # according to type of kernel, the equation for transforming the distance to weight is different.          
  W <- matrix(W, p, k)
  if (response != "continuous") {
    for (i in 1:l) {
      weightClass[, i] <- rowSums(W * (CL == lev[i]))
    }
    weightClass <- weightClass/rowSums(weightClass)
    colnames(weightClass) <- lev
  }
  if (response == "ordinal") {
    blub = length(lev)
    weightClass = weightClass %*% weight.y(blub, ykernel)
    weightClass <- weightClass/rowSums(weightClass)
    weightClass <- t(apply(weightClass, 1, cumsum))
    colnames(weightClass) <- lev
    fit <- numeric(p)
    for (i in 1:p) fit[i] <- min((1:l)[weightClass[i, ] >= param])
    fit <- ordered(fit, levels = 1:l, labels = lev)
  }

  if (response == "continuous") 
    fit <- rowSums(W * CL)/pmax(rowSums(W), 1e-06)
  options(contrasts = old.contrasts)
  result <- list(fitted.values = fit, CL = CL, W = W, D = D, 
                 C = C, prob = weightClass, response = response, distance = distance, 
                 call = ca, terms = mt)
  class(result) = "kknn"
  result
}





# postscript("../figs/trend.eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
# plot(dat$Month,dat$V2,xaxt = "n",xlab="",ylab="Accuracy",pch=19,cex.lab=1.5)
# axis.Date(side = 1, mon,at = labDates, format = "%b %Y",las = 2)
# dev.off()

# postscript("../figs/numdata.eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
# plot(dat$Month,dat$V3,xaxt = "n",xlab="",ylab="Number of Data",pch=19,cex.lab=1.5)
# axis.Date(side = 1, mon,at = labDates, format = "%b %Y",las = 2)
# dev.off()


# alpha=read.csv("alpha.csv",header=F)

# postscript("../figs/alpha.eps",width = 8.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
# plot(alpha$V1,alpha$V2/100,xaxt = "n",xlab=substitute(alpha),ylab="Accuracy",pch=19,type="b",cex.lab=1.5)
# axis(side = 1,alpha$V1 ,at = alpha$V1,cex.axis=1.5)
# dev.off()


# alphas<-c(0.32,0.42,0.31,0.54,0.51,0.43,0.50,0.54,0.39,0.40,0.40,0.43,0.40,0.43,0.61,0.40,0.51,0.36,0.57,0.41,0.43,0.49,0.39,0.42,0.41,0.30,0.61,0.51,0.56,0.42,0.40,0.43,
# 0.32,0.35,0.50,0.31,0.46,0.55,0.46,0.47,0.40,0.56,0.40,0.48,0.59,0.66,0.49,0.32,0.40,0.49,0.64,0.42,0.41,0.46,0.47,0.37,0.65,0.33,0.37,0.54,0.57,0.51,0.42,0.54,
# 0.39,0.41,0.39,0.67,0.51,0.46,0.47,0.28,0.47,0.39,0.37,0.54,0.40,0.36,0.41,0.50,0.41,0.39,0.42,0.38,0.64,0.43,0.41,0.50,0.49,0.41,0.41,0.34,0.43,0.59,0.50,0.53,
# 0.45,0.32,0.53,0.52,0.33,0.41,0.42,0.42,0.44,0.47,0.39,0.54,0.45,0.55,0.54,0.49,0.42,0.50,0.69,0.49,0.47,0.41,0.42,0.50,0.43,0.57,0.38,0.35,0.52,0.43,0.48,0.46,
# 0.53,0.36,0.40,0.56,0.38,0.53,0.47,0.52,0.40,0.43,0.43,0.57,0.49,0.49,0.40,0.68,0.49,0.62,0.56,0.40,0.48,0.39,0.44,0.50,0.57,0.43,0.54,0.57,0.66,0.52,0.49,0.40,
# 0.41,0.50,0.27,0.57,0.52,0.55,0.50,0.42,0.39,0.53,0.28,0.42,0.64,0.59,0.62,0.30,0.40,0.35,0.49,0.57,0.53,0.51,0.61,0.33,0.47,0.55,0.38,0.39,0.60,0.40,0.47,0.50,
# 0.46,0.64,0.43,0.33,0.40,0.65,0.44,0.57)

# postscript("../figs/optalpha.eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
# hist(alphas,breaks =seq(0.2,0.8,0.05),xlab=substitute(alpha),main="",cex.lab=1.5)
# dev.off()

# accu05<-c(78.19549,78.57143,77.06767,83.08271,81.95489,81.20301,82.33083,81.57895,83.08271,80.45113,79.69925,77.06767,79.32331,82.33083,81.95489,81.95489,81.20301,79.69925,
# 81.95489,83.83459,82.70677,81.95489,80.82707,79.69925,75.56391,78.94737,79.69925,81.95489,80.07519,78.94737,81.95489,82.33083,77.81955,79.32331,81.57895,75.18797,
# 79.69925,83.45865,78.19549,80.82707,80.82707,81.20301,80.07519,81.95489,78.94737,78.94737,78.57143,77.44361,83.08271,81.95489,80.82707,81.20301,75.93985,82.70677,
# 79.32331,78.19549,75.93985,80.82707,76.69173,78.19549,81.20301,77.06767,78.94737,75.93985,78.57143,80.07519,81.57895,82.70677,82.70677,81.20301,80.07519,83.08271,
# 81.20301,78.19549,81.95489,84.58647,80.07519,82.70677,82.70677,81.20301,76.69173,78.57143,82.33083,77.44361,81.57895,81.95489,79.32331,84.21053,81.20301,80.45113,
# 80.07519,80.82707,82.33083,74.06015,81.95489,79.69925,84.21053,83.08271,83.83459,75.93985,81.57895,77.81955,76.31579,75.93985,78.94737,83.45865,75.93985,76.69173,
# 80.45113,83.83459,78.94737,81.57895,80.82707,84.21053,77.06767,81.57895,81.57895,84.21053,77.06767,80.82707,81.20301,84.58647,79.32331,80.07519,81.20301,80.07519,
# 75.93985,75.18797,80.45113,79.69925,81.57895,77.81955,78.57143,79.69925,79.69925,80.82707,78.19549,84.58647,81.95489,81.95489,80.45113,79.69925,79.69925,78.57143,
# 78.94737,80.07519,83.45865,79.69925,82.70677,79.69925,77.44361,84.58647,80.45113,80.82707,75.56391,81.95489,80.07519,80.07519,81.20301,78.19549,82.70677,83.08271,
# 80.45113,81.57895,84.21053,76.31579,80.82707,82.70677,81.57895,82.33083,73.68421,82.33083,81.20301,78.57143,82.33083,79.69925,78.94737,73.68421,76.31579,81.20301,
# 80.82707,80.07519,78.57143,83.45865,82.70677,79.32331,80.07519,83.83459,79.69925,81.57895,80.45113,83.45865,81.95489,77.81955,80.45113,79.69925,78.94737,78.19549,
# 81.57895,82.33083)

# accopt<-c(80.07519,79.32331,82.33083,83.83459,81.95489,81.57895,82.33083,82.33083,84.21053,82.70677,80.82707,77.81955,80.07519,83.45865,82.33083,83.45865,81.95489,80.82707,
          # 82.33083,85.33835,84.21053,81.95489,84.21053,80.45113,80.82707,80.07519,81.57895,81.95489,80.82707,82.33083,82.33083,83.08271,81.20301,80.07519,81.57895,76.31579,
          # 80.45113,84.96241,78.94737,80.82707,82.33083,82.33083,82.33083,83.08271,80.82707,80.45113,78.94737,80.07519,83.08271,82.33083,83.08271,82.33083,78.57143,83.83459,
          # 79.69925,80.45113,77.06767,81.95489,77.44361,79.69925,81.95489,77.06767,80.45113,75.93985,80.07519,80.45113,82.70677,83.45865,83.45865,82.33083,80.45113,84.58647,
          # 81.95489,79.69925,82.70677,85.33835,82.33083,84.21053,83.08271,81.20301,78.94737,80.07519,84.21053,81.20301,84.58647,82.33083,82.33083,84.21053,81.20301,82.33083,
          # 82.70677,82.70677,83.08271,75.93985,81.95489,80.45113,84.58647,85.33835,83.83459,76.69173,82.33083,82.33083,80.07519,77.44361,83.08271,83.45865,78.57143,76.69173,
          # 80.45113,84.96241,79.32331,81.57895,82.70677,84.21053,77.81955,81.57895,82.33083,84.58647,80.82707,80.82707,81.95489,85.33835,79.69925,81.57895,81.95489,81.20301,
          # 76.31579,75.56391,81.57895,81.20301,83.83459,79.32331,79.69925,80.07519,79.69925,80.82707,79.69925,84.96241,83.08271,82.33083,80.45113,82.33083,81.20301,80.07519,
          # 79.32331,80.82707,83.45865,81.20301,83.08271,83.08271,78.19549,84.58647,80.82707,81.95489,78.19549,83.08271,80.82707,81.20301,81.20301,79.32331,84.21053,83.08271,
          # 83.45865,82.33083,84.58647,78.19549,80.82707,83.45865,83.45865,82.33083,80.07519,85.71429,81.95489,78.57143,83.83459,81.57895,81.95489,78.19549,76.31579,81.95489,
          # 81.57895,80.07519,80.07519,85.71429,83.83459,79.69925,81.57895,83.83459,80.45113,83.45865,80.82707,83.45865,83.08271,78.57143,80.45113,80.82707,80.82707,80.07519,
          # 81.95489,83.83459)

# acc<-cbind(wkNN=accu05,"Adaptive wkNN"=accopt)

# postscript("../figs/boxplot.eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
# boxplot(as.data.frame(acc),ylab="Accuracy",cex.lab=1.5)
# dev.off()

