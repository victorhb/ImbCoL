# ImbCoL
R package for data complexity measures for imbalanced classification tasks. These measures were adapted from Ho and Basu [1] and published on Barella et. al [2]. Mainly, this package provides a decomposition by class of the original measures which showed to be powerful describing imbalanced classification tasks [2]. The implementation is based on the package ECoL (https://github.com/lpfgarcia/ECoL) [3].

## Measures

The decomposed data complexity measures can be grouped in: (1) feature overlapping measures, (2) neighborhood measures, (3) linearity measures. They are listed below:

**Measures of overlapping** 

* F1: Fisher's discriminant ratio
* F2: Overlapping of the per-class bounding boxes
* F3: Maximum individual feature efficiency
* F4: Cllective feature efficiency

**Measures of neighborhood information** 

* N1: Fraction of points lying on the class boundary
* N2: Average intra/inter class nearest neighbor distances
* N3: Leave-one-out error rate of the 1-nearest neighbor algorithm
* N4: Nonlinearity of the one-nearest neighbor classifier
* T1: Fraction of maximum covering spheres on data

**Measures of linearity** 

* L1: Distance of erroneous instances to a linear classifier
* L2: Training error of a linear classifier
* L3: Nonlinearity of a linear classifier

## Installation

This package is not available on CRAN but it can be installed with devtools.

```r
if (!require("devtools")) {
    install.packages("devtools")
}
devtools::install_github("victorhb/ImbCoL")
library("ImbCoL")
```
## Example of use

The simplest way to compute the complexity measures are using the `complexity` method. It is possible to use a `formula` as parameter or a `data.frame`. To extract a specific measure, use the function related with the group. A simple example is given next:

```r
## Extract all complexity measures available
ImbCoL::complexity(Species ~ ., iris)

## Extract all complexity measures using data frame
ImbCoL::complexity(iris[,1:4], iris[,5])

## Extract the overlapping measures
ImbCoL::overlapping(Species ~ ., iris)

## Extract the decomposed N3 measure using neighborhood function
ImbCoL::neighborhood(Species ~ ., iris, measures="N3_partial")
```

## Developer notes

The implementation of ImbCoL is based on the implementation of ECoL. We suggest using the namespace `ImbCoL::` when using both packages to avoid conflict.

To cite `ImbCoL` in publications use: 

* Barella, V. H., Garcia, L. P., de Souto, M. P., Lorena, A. C., & De Carvalho, A. (2018, July). Data Complexity Measures for Imbalanced Classification Tasks. In 2018 International Joint Conference on Neural Networks (IJCNN) (pp. 1-8). IEEE.

To submit bugs and feature requests, report at [project issues](https://github.com/victorhb/ImbCoL/issues).

## References

[1] Ho, T., and Basu, M. (2002). Complexity measures of supervised classification problems. IEEE Transactions on Pattern Analysis and Machine Intelligence, 24(3):289-300.

[2] Barella, V. H., Garcia, L. P., de Souto, M. P., Lorena, A. C., and De Carvalho, A. (2018, July). Data Complexity Measures for Imbalanced Classification Tasks. In 2018 International Joint Conference on Neural Networks (IJCNN) (pp. 1-8). IEEE.

[3] Garcia, L., Lorena, A., Lehmann, and J.  ECoL:   Complexity   Measures   for   Classification   Problems,   https://CRAN.R-project.org/package=ECoL, 2018

[4] R Core Team (2019). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.
