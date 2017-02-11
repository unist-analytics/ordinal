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
 else if (is.numeric(cl)) 
    response <- "continuous"
 else if (is.factor(cl) & !is.ordered(cl)) {
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
                as.integer(m), as.integer(p), as.integer(q), dm = double((k + 1L) * p), cl = integer((k + 1L) * p)
                , k = as.integer(k + 1), as.double(distance), as.double(we), dup = FALSE, PACKAGE = "kknn")
  else dmtmp <- .C("dm", as.double(learn), as.double(valid), as.integer(m), as.integer(p), as.integer(q), dm = double((k + 1L) * p)
                 ,cl = integer((k + 1L) * p), k = as.integer(k + 1), as.double(distance), as.double(we), dup = FALSE, 
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
 else if (response == "continuous") {
    fit <- rowSums(W * CL)/pmax(rowSums(W), 1e-06)
  options(contrasts = old.contrasts)
  result <- list(fitted.values = fit, CL = CL, W = W, D = D, 
                 C = C, prob = weightClass, response = response, distance = distance, 
                 call = ca, terms = mt)
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

  
  class(result) = "kknn"
  result
}






