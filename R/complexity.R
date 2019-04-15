#' Extract the complexity measures from datasets
#'
#' This function is responsable to extract the complexity measures from the 
#' classes of the classification tasks. For such, they take into account the 
#' overlap between classes imposed by feature values, the separability and 
#' distribution of the data points. 
#'
#' @param x A data.frame contained only the input attributes.
#' @param y A response vector with one value for each row/component of x.
#' @param groups A list of complexity measures groups or \code{"all"} to include
#'  all of them.
#' @param formula A formula to define the output column.
#' @param data A data.frame dataset contained the input and output attributes.
#' @param ... Not used.
#' @details
#'  The following groups are allowed for this method:
#'  \describe{
#'    \item{"overlapping"}{The feature overlapping measures characterize how 
#'      informative the available features are to separate the classes See 
#'      \link{overlapping} for more details.}
#'    \item{"neighborhood"}{Neighborhood measures characterize the presence and 
#'      density of same or different classes in local neighborhoods. See 
#'      \link{neighborhood} for more details.}
#'    \item{"linearity"}{Linearity measures try to quantify whether the classes 
#'      can be linearly separated. See \link{linearity.class} or 
#'      \link{linearity.regr} for more details.}
#'  }
#' @return A numeric vector named by the requested complexity measures.
#'
#' @references
#' 
#'  Victor H. Barella and Luis P. Garcia and Marcilio P. de Souto and Ana C. Lorena
#'     and Andre de Carvalho (2018). Data Complexity Measures for Imbalanced
#'     Classification Tasks. In 2018 International Joint Conference on Neural Networks
#'     (IJCNN) (pp. 1-8). IEEE.
#' 
#'  Tin K Ho and Mitra Basu. (2002). Complexity measures of supervised 
#'    classification problems. IEEE Transactions on Pattern Analysis and Machine
#'    Intelligence, 24, 3, 289--300.
#'
#'  Albert Orriols-Puig, Nuria Macia and Tin K Ho. (2010). Documentation for 
#'    the data complexity library in C++. Technical Report. La Salle - 
#'    Universitat Ramon Llull.
#'
#'  Ana C Lorena and Aron I Maciel and Pericles B C Miranda and Ivan G Costa and
#'    Ricardo B C Prudencio. (2018). Data complexity meta-features for 
#'    regression problems. Machine Learning, 107, 1, 209--246.
#'
#' @examples
#' ## Extract all complexity measures for classification task
#' data(iris)
#' complexity(Species ~ ., iris, type="class")
#' @export
complexity <- function(...) {
  UseMethod("complexity")
}

#' @rdname complexity
#' @export
complexity.default <- function(x, y, groups="all", ...) {

  if(!is.data.frame(x)) {
    stop("data argument must be a data.frame")
  }

  if(is.data.frame(y)) {
    y <- y[, 1]
  }

  if(type == "class") {
    if(min(table(y)) < 2) {
      stop("number of examples in the minority class should be >= 2")
    }
  } 

  if(nrow(x) != length(y)) {
    stop("x and y must have same number of rows")
  }

  if(groups[1] == "all") {
    groups <- ls.complexity(type)
  }

  groups <- match.arg(groups, ls.complexity(type), TRUE)
  colnames(x) <- make.names(colnames(x))

  unlist(
    sapply(groups, function(group) {
      do.call(group, list(x=x, y=y, ...))
    }, simplify=FALSE)
  )
}

#' @rdname complexity
#' @export
complexity.formula <- function(formula, data, groups="all", ...) {

  if(!inherits(formula, "formula")) {
    stop("method is only for formula datas")
  }

  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame")
  }

  modFrame <- stats::model.frame(formula, data)
  attr(modFrame, "terms") <- NULL

  complexity.default(modFrame[, -1, drop=FALSE], modFrame[, 1, drop=FALSE],
    groups, ...)
}

ls.complexity <- function() {
      c("overlapping", "neighborhood", "linearity.class")
}
