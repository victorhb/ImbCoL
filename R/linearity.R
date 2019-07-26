#' Measures of linearity
#'
#' Classification task. The linearity measures try to quantify if it is possible
#' to separate the classes by a hyperplane. The underlying assumption is that a 
#' linearly separable problem can be considered simpler than a problem requiring
#' a non-linear decision boundary.
#'
#' @family complexity-measures
#' @param x A data.frame contained only the input attributes.
#' @param y A factor response vector with one label for each row/component of x.
#' @param measures A list of measures names or \code{"all"} to include all them.
#' @param formula A formula to define the class column.
#' @param data A data.frame dataset contained the input attributes and class.
#' @param ... Not used.
#' @details
#'  The following measures are allowed for this method:
#'  \describe{
#'    \item{"L1"}{Sum of the error distance by linear programming (L1) computes 
#'      the sum of the distances of incorrectly classified examples to a linear 
#'      boundary used in their classification.}
#'    \item{"L2"}{Error rate of linear classifier (L2) computes the error rate 
#'      of the linear SVM classifier induced from dataset.}
#'    \item{"L3"}{Non-linearity of a linear classifier (L3) creates a new 
#'      dataset randomly interpolating pairs of training examples of the same 
#'      class and then induce a linear SVM on the original data and measure 
#'      the error rate in the new data points.}
#'    \item{"LX_partial"}{X \in [1,2,3]. It is the decomposed version of the 
#'      correponding LX function. Instead of giving a single complexity value 
#'      for the dataset, it returns one complexity value per class.}
#'  }
#' @return A list named by the requested linearity measure.
#'
#' @examples
#' ## Extract all linearity measures
#' data(iris)
#' linearity.class(Species ~ ., iris)
#' @export
linearity.class <- function(...) {
  UseMethod("linearity.class")
}

#' @rdname linearity.class
#' @export
linearity.class.default <- function(x, y, measures="all", ...) {

  if(!is.data.frame(x)) {
    stop("data argument must be a data.frame")
  }

  if(is.data.frame(y)) {
    y <- y[, 1]
  }

  y <- as.factor(y)

  if(min(table(y)) < 2) {
    stop("number of examples in the minority class should be >= 2")
  }

  if(nrow(x) != length(y)) {
    stop("x and y must have same number of rows")
  }

  if(measures[1] == "all") {
    measures <- ls.linearity.class()
  }

  measures <- match.arg(measures, ls.linearity.class(), TRUE)
  colnames(x) <- make.names(colnames(x))

  data <- data.frame(x, class=y)
  data_ova <- ova(data)
  data <- ovo(data)

  model <- lapply(data, smo)
  model_ova <- lapply(data_ova, smo)
  
  sapply(measures, function(f) {
    if(regexpr("_partial",f) > -1){
      eval(call(paste("c", f, sep="."), model=model_ova, data=data_ova))
    }else{
      eval(call(paste("c", f, sep="."), model=model, data=data))
    }
  })
}

#' @rdname linearity.class
#' @export
linearity.class.formula <- function(formula, data, measures="all", ...) {

  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  linearity.class.default(modFrame[, -1, drop=FALSE], modFrame[, 1, drop=FALSE],
    measures, ...)
}

ls.linearity.class <- function() {
  c("L1_partial","L2_partial","L3_partial")
}

smo <- function(data) {
  e1071::svm(class ~ ., data, scale=TRUE, kernel="linear")
}

c.L1 <- function(model, data) {

  aux <- mapply(function(m, d) {
    prd <- stats::predict(m, d, decision.values=TRUE)
    err <- rownames(d[prd != d$class,])
    dst <- attr(prd, "decision.values")[err,]
    sum(abs(dst))/nrow(d)
  }, m=model, d=data)

  aux <- mean(aux)
  
  aux <- 1 / (aux + 1)
  aux <- 1 - aux
  
  return(aux)
}

error <- function(pred, class) {
  1 - sum(diag(table(class, pred)))/sum(table(class, pred))
}

c.L2 <- function(model, data) {

  aux <- mapply(function(m, d) {
    prd <- stats::predict(m, d)
    error(prd, d$class)
  }, m=model, d=data)

  return(mean(aux))
}

c.L3 <- function(model, data) {

  aux <- mapply(function(m, d) {
    tmp <- c.generate(d, nrow(d))
    prd <- stats::predict(m, tmp)
    error(prd, tmp$class)
  }, m=model, d=data)

  return(mean(aux))
}

ova <- function(data) {
  
  l = levels(data$class)
  tmp <- lapply(1:length(l), FUN = function(c){
    a = branch_partial(data,l[c])
    b = branch_partial(data,l[-c])
    a <- cbind(a,class = l[c])
    b <- cbind(b,class = paste("no",l[c], sep = "_"))
    aux = rbind(a,b)
    aux$class <- as.factor(aux$class)
    return(aux)
  })
  names(tmp) <- levels(data$class)
  return(tmp)
}

c.L1_partial <- function(model, data){ #recieves as entry data_ova and model_ova
  aux <- sapply(names(data),function(c) {
    data_interest = data[[c]][which(data[[c]][,"class"] == c),]
    prd <- stats::predict(model[[c]], data_interest, decision.values=TRUE)
    err <- rownames(data_interest[prd != data_interest$class,])
    dst <- attr(prd, "decision.values")[err,]
    #sum(abs(dst))/(nrow(data_interest)*hyperretangle(data_interest))
    sum(abs(dst))/(nrow(data_interest))
  })
  
  aux <- 1 / (aux + 1)
  aux <- 1 - aux
  
  names(aux) <- names(data)
  
  return(aux)
}

c.L2_partial <- function(model, data) {
  
  aux <- sapply(names(data), FUN = function(c){
    data_interest = data[[c]][which(data[[c]][,"class"] == c),]
    pred <- stats::predict(model[[c]], data_interest)
    error(pred, data_interest$class)
  })
  
  names(aux) = names(data)
  
  return(aux)
}

c.L3_partial <- function(model, data) {
  
  aux <- sapply(names(data), FUN = function(c) {
    tmp <- c.generate(data[[c]][which(data[[c]][,"class"] == c),], n = nrow(data[[c]]))
    pred <- stats::predict(model[[c]], tmp)
    error(pred, tmp$class)
  })
  names(aux) <- names(data)
  return(aux)
}
