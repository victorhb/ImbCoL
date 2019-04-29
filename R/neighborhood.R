#' Measures of neighborhood
#'
#' Classification task. The Neighborhood measures analyze the neighborhoods of 
#' the data items and try to capture class overlapping and the shape of the 
#' decision boundary. They work over a distance matrix storing the distances 
#' between all pairs of data points in the dataset.
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
#'    \item{"N1"}{Fraction of borderline points (N1) computes the percentage of 
#'      vertexes incident to edges connecting examples of opposite classes in 
#'      a Minimum Spanning Tree (MST).}
#'    \item{"N2"}{Ratio of intra/extra class nearest neighbor distance (N2)  
#'      computes the ratio of two sums: intra-class and inter-class. The former 
#'      corresponds to the sum of the distances between each example and its 
#'      closest neighbor from the same class. The later is the sum of the 
#'      distances between each example and its closest neighbor from another 
#'      class (nearest enemy).}
#'    \item{"N3"}{Error rate of the nearest neighbor (N3) classifier corresponds
#'      to the error rate of a one Nearest Neighbor (1NN) classifier, estimated 
#'      using a leave-one-out procedure in dataset.}
#'    \item{"N4"}{Non-linearity of the nearest neighbor classifier (N4) creates 
#'      a new dataset randomly interpolating pairs of training examples of the 
#'      same class and then induce a the 1NN classifier on the original data and
#'      measure the error rate in the new data points.}
#'    \item{"T1"}{Fraction of hyperspheres covering data (T1) builds 
#'      hyperspheres centered at each one of the training examples, which have 
#'      their radios growth until the hypersphere reaches an example of another 
#'      class. Afterwards, smaller hyperspheres contained in larger hyperspheres 
#'      are eliminated. T1 is finally defined as the ratio between the number of 
#'      the remaining hyperspheres and the total number of examples in the 
#'      dataset.}
#'    \item{"LSC"}{Local Set Average Cardinality (LSC) is based on Local Set 
#'      (LS) and defined as the set of points from the dataset whose distance of
#'      each example is smaller than the distance from the exemples of the 
#'      different class. LSC is the average of the LS.}
#'    \item{"X_partial"}{X \in [N1,N2,N3,N4,T1]. It is the decomposed version of the 
#'      correponding X function. Instead of giving a single complexity value 
#'      for the dataset, it returns one complexity value per class.}
#'  }
#' @return A list named by the requested neighborhood measure.
#'
#' @examples
#' ## Extract all neighborhood measures
#' data(iris)
#' neighborhood(Species ~ ., iris)
#' @export
neighborhood <- function(...) {
  UseMethod("neighborhood")
}

#' @rdname neighborhood
#' @export
neighborhood.default <- function(x, y, measures="all", ...) {
  
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
    measures <- ls.neighborhood()
  }
  
  measures <- match.arg(measures, ls.neighborhood(), TRUE)
  colnames(x) <- make.names(colnames(x))
  
  data <- data.frame(x, class=y)
  dst <- dist(x)
  
  sapply(measures, function(f) {
    eval(call(paste("c", f, sep="."), dst=dst, data=data))
  })
}

#' @rdname neighborhood
#' @export
neighborhood.formula <- function(formula, data, measures="all", ...) {
  
  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }
  
  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }
  
  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL
  
  neighborhood.default(modFrame[, -1, drop=FALSE], modFrame[, 1, drop=FALSE],
                       measures, ...)
}

ls.neighborhood <- function() {
  c("N1","N2", "N3", "N4", "T1", "N1_partial","N2_partial", "N3_partial", "N4_partial", "T1_partial")
}

c.N1 <- function(dst, data) {
  links = ape::mst(as.dist(dst))
  aux = sapply(rownames(links), FUN = function(x) sum(data[x,"class"] != data[names(which(links[x,] == 1)),"class"]) > 0 )
  return(sum(aux)/nrow(data))
}

intra <- function(dst, data, i) {
  tmp <- rownames(data[data$class == data[i,]$class,])
  aux <- min(dst[i, setdiff(tmp, i)])
  return(aux)
}

inter <- function(dst, data, i) {
  tmp <- rownames(data[data$class != data[i,]$class,])
  aux <- sort(dst[i, tmp])[1]
  return(aux)
}

c.N2 <- function(dst, data) {
  
  aux <- sapply(rownames(data), function(i) {
    c(intra(dst, data, i), inter(dst, data, i))
  })
  
  aux <- sum(aux[1,])/sum(aux[2,])
  return(aux)
}

knn <- function(data, dst, k) {
  apply(dst, 1, function(i) {
    tmp <- names(sort(i)[k])
    data[tmp,]$class
  })
}

c.N3 <- function(dst, data) {
  aux <- knn(data, dst, 2) != data$class
  return(mean(aux))
}

c.N4 <- function(dst, data) {
  
  tran <- rbind(data, c.generate(data, nrow(data)))
  test <- utils::tail(tran, nrow(data))
  
  dst <- dist(tran[,-ncol(tran), drop=FALSE])
  dst <- dst[rownames(test), rownames(data)]
  
  aux <- knn(data, dst, 1) != test$class
  return(mean(aux))
}

radios <- function(dst, data, i) {
  
  di <- inter(dst, data, i)
  j <- names(di)
  dj <- inter(dst, data, j)
  k <- names(dj)
  
  if(i == k) {
    return(di/2)
  } else {
    tmp <- radios(dst, data, j)
    return(di - tmp)
  }
}

hyperspher <- function(dst, data) {
  
  aux <- sapply(rownames(data), function(i) {
    as.numeric(radios(dst, data, i))
  })
  
  return(aux)
}

translate <- function(dst, r) {
  
  aux <- t(sapply(rownames(dst), function(i) {
    dst[i,] < r[i]
  }))
  
  return(aux)
}

adherence <- function(adh, data) {
  
  h <- n <- c()
  
  repeat{
    
    aux <- which.max(rowSums(adh))
    tmp <- names(which(adh[aux,]))
    dif <- setdiff(rownames(adh), c(tmp, names(aux)))
    adh <- adh[dif, dif, drop=FALSE]
    
    if(all(dim(adh) != 0)) {
      h <- c(h, length(tmp))
    } else {
      h <- c(h, 1)
    }
    
    n <- c(n, names(aux))
    
    if(all(dim(adh)) == 0)
      break
  }
  
  names(h) <- n
  return(h)
}

c.T1 <- function(dst, data) {
  r <- hyperspher(dst, data)
  aux <- adherence(translate(dst, r), data)
  aux <- length(aux)/nrow(data)
  return(aux)
}

c.LSC <- function(dst, data) {
  
  r <- sapply(rownames(data), function(i) {
    as.numeric(inter(dst, data, i))
  })
  
  aux <- sum(translate(dst, r))/(nrow(dst)^2)
  return(aux)
}

c.N1_partial <- function(dst, data) {
  
  links = ape::mst(as.dist(dst))
  classes <- unique(data$class)
  ret = mapply(classes, FUN=function(c){
    aux = sapply(rownames(data[data$class == c,]), FUN = function(x) sum(data[x,"class"] != data[names(which(links[x,] == 1)),"class"]) > 0 )
    return(sum(aux)/sum(data$class == c))
  })
  names(ret) = classes
  return(ret)
}

c.N2_partial <- function(dst, data) {
  
  ret = mapply(levels(data$class), FUN = function(c){
    aux <- sapply(rownames(data[which(data$class == c),]), 
      function(i) {
        a <- intra(dst, data, i)
        r <- inter(dst, data, i)
        return(c(a,r))
      })
    
    aux = sum(aux[1,])/sum(aux[2,])
  })
  names(ret) = levels(data$class)
  return(ret)
}

partial.1nn <- function(data, dst, i) {
  tmp <- names(sort(dst[i,])[2])
  aux <- data[tmp,]$class
  names(aux) <- tmp
  return(aux) 
}

c.N3_partial <- function(dst, data) {

  
  ret = mapply(levels(data$class), FUN = function(c){
    aux <- unlist(
      lapply(rownames(data[which(data$class == c),]), 
             function(i) {
               partial.1nn(dst = dst, data = data, i = i) != data[i,]$class
             })
    )
    
    return(mean(aux))
  })
  
  names(ret) = levels(data$class)
  ret
}

c.N4_partial <- function(dst, data) {
  
  ret = mapply(levels(data$class), FUN = function(c){
    aux <- rbind(data, c.generate(data[which(data$class == c),], n = nrow(data)))
    vet <- setdiff(rownames(aux), rownames(data))
    dst <- dist(aux[,-ncol(aux), drop=FALSE])
    
    aux <- unlist(
      lapply(vet, function(i) {
        idx <- which.min(dst[i, rownames(data)])
        data[names(idx),]$class != aux[i,]$class
      })
    )
    
    return(mean(aux))
  })
  
  names(ret) <- levels(data$class)
  ret
}

c.T1_partial <- function(dst, data) {
  r <- hyperspher(dst, data)
  aux <- adherence(translate(dst, r), data)
  ret = mapply(levels(data$class), FUN= function(c){
    length(which(data[names(aux),"class"] == c))/sum(data[,"class"]==c)
  })
  names(ret) = levels(data$class)
  return(ret)
}
