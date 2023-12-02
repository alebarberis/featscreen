#'@include 0-utility-functions.R 1-log-functions.R
NULL

# Generic ----------------------------------------------------------------------

#'Stat Function Doc
#'
#'@description This function computes a statistical measure from each row of
#'the input data and the output variable.
#'
#'@param x \code{matrix} or \code{data.frame}.
#'@param g a vector or factor object giving the group for the corresponding
#'elements of \code{x}.
#'@param y numeric vector of data values. Must have the same length as \code{ncol(x)}.
#'@param alternative character string or vector of length \code{nrow(x)}.
#'The alternative hypothesis for each row of \code{x}.
#'Values must be one of \code{"two.sided"} (default), \code{"greater"} or \code{"less"}.
#'@param conf.level numerical value or numeric vector of length \code{nrow(x)}.
#'The confidence levels of the intervals.
#'All values must be in the range of \eqn{[0:1]} or \code{NA}.
#'@param null numerical value or numeric vector of length \code{nrow(x)}.
#'The true values of the difference in means between the two groups of
#'observations for each row.
#'
#'@return A list containing two elements:
#'
#'\describe{
#'   \item{statistic}{A numeric vector, the values of the test statistic}
#'   \item{significance}{A numeric vector, the p-values of the selected test}
#'}
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
#'
#'@name genericRowStat
NULL

# Numerical output variable ----------------------------------------------------

#'Row Correlations
#'
#'@description This function tests for association between each row vector in
#'\code{x} and \code{y}, using one of Pearson's product moment coefficient,
#'Kendall's tau or Spearman's rho.
#'
#'See the **Details** section below for further information.
#'
#'@inheritParams genericRowStat
#'@param method character string indicating how to measure the association.
#'Available options are:
#'\describe{
#'   \item{\code{"pearson"}}{Pearson's product moment coefficient}
#'   \item{\code{"kendall"}}{Kendall's tau}
#'   \item{\code{"spearman"}}{Spearman's rho}
#'}
#'@param ... further arguments to \code{\link[stats]{cor.test}}
#'
#'@inherit genericRowStat return
#'
#'@details See the following functions for each specific implementation:
#'
#'\describe{
#' \item{\code{"pearson" }}{\code{\link{rowPearsonCor}}}
#' \item{\code{"kendall" }}{\code{\link{rowKendallCor}}}
#' \item{\code{"spearman"}}{\code{\link{rowSpearmanCor}}}
#'}
#'
#'@inherit genericRowStat author
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Data
#'x = matrix(rnorm(100 * 20), 100, 20)
#'y = rnorm(20)
#'
#'#Compute
#'rowCor(x = x, y = y)
#'
#'@export
rowCor <- function(
    x,
    y,
    alternative = c("two.sided", "greater", "less"),
    method = c("pearson", "kendall", "spearman"),
    conf.level = 0.95,
    ...
){
  alternative = match.arg(alternative);
  method      = match.arg(method);

  cor = switch(
    method,
    "pearson"  =  rowPearsonCor(x = x, y = y, alternative = alternative, conf.level = conf.level),
    "kendall"  =  rowKendallCor(x = x, y = y, alternative = alternative, conf.level = conf.level),
    "spearman" = rowSpearmanCor(x = x, y = y, alternative = alternative, conf.level = conf.level)
  )
  return(cor)
}

#'Pearson's Correlation
#'
#'@description This function computes the *Pearson's correlation* for each row
#'vector in \code{x}.
#'See the **Details** section below for further information.
#'
#'@inheritParams genericRowStat
#'@inheritParams matrixTests::row_cor_pearson
#'
#'@inherit genericRowStat return
#'
#'@details It is a wrapper to \code{\link[matrixTests]{row_cor_pearson}}
#'function.
#'
#'
#'@inherit genericRowStat author
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Data
#'x = matrix(rnorm(100 * 20), 100, 20)
#'y = rnorm(20)
#'
#'#Compute
#'rowPearsonCor(x = x, y = y)
#'
#'@export
rowPearsonCor <- function(
    x,
    y,
    alternative = c("two.sided", "greater", "less"),
    conf.level = 0.95
){
  alternative = match.arg(alternative);

  r = matrixTests::row_cor_pearson(
    x = x,
    y = y,
    alternative=alternative,
    conf.level = conf.level
  );

  r = list(statistic = r$statistic, significance = r$pvalue)

  # Describe
  attr(x = r, which = "comment") = getStatisticalMethodName(id = "pearson")

  return(r);
}

#'Spearman's Correlation
#'
#'@description This function computes the *Spearman's correlation* for each row
#'vector in \code{x}.
#'See the **Details** section below for further information.
#'
#'
#'@param alternative character string indicating the alternative hypothesis for
#'each row of \code{x}.
#'It must be one of \code{"two.sided"} (default), \code{"greater"} or \code{"less"}.
#'
#'@inheritParams genericRowStat
#'@inheritParams stats::cor.test
#'@param ... further arguments to \code{\link[stats]{cor.test}}
#'
#'@inherit genericRowStat return
#'
#'@details It is a wrapper to \code{\link[stats]{cor.test}}
#'function.
#'
#'@inherit genericRowStat author
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Data
#'x = matrix(rnorm(100 * 20), 100, 20)
#'y = rnorm(20)
#'
#'#Compute
#'rowSpearmanCor(x = x, y = y)
#'
#'@export
rowSpearmanCor <- function(
    x,
    y,
    alternative = c("two.sided", "greater", "less"),
    conf.level = 0.95,
    ...
  ){
  alternative = match.arg(alternative)

  r = apply(
    X = x,
    MARGIN = 1,
    FUN = stats::cor.test,
    y = y,
    alternative = alternative,
    method = "spearman",
    conf.level = conf.level,
    ...
  )

  out = list()
  out$statistic = unlist(lapply(X = r, FUN = '[[', 'statistic'))
  names(out$statistic) = NULL
  out$significance = unlist(lapply(X = r, FUN = '[[', 'p.value'))
  names(out$significance) = NULL

  # Describe
  attr(x = out, which = "comment") = getStatisticalMethodName(id = "spearman")

  return(out)
}

#'Kendall's Correlation
#'
#'@description This function computes the *Kendall's correlation* for each row
#'vector in \code{x}.
#'See the **Details** section below for further information.
#'
#'@param alternative character string indicating the alternative hypothesis for
#'each row of \code{x}.
#'It must be one of \code{"two.sided"} (default), \code{"greater"} or \code{"less"}.
#'
#'@inheritParams genericRowStat
#'@inheritParams stats::cor.test
#'@param ... further arguments to \code{\link[stats]{cor.test}}
#'
#'@inherit genericRowStat return
#'
#'@details It is a wrapper to \code{\link[stats]{cor.test}}
#'function.
#'
#'@inherit genericRowStat author
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Data
#'x = matrix(rnorm(100 * 20), 100, 20)
#'y = rnorm(20)
#'
#'#Compute
#'rowKendallCor(x = x, y = y)
#'
#'@export
rowKendallCor <- function(
    x,
    y,
    alternative = c("two.sided", "greater", "less"),
    conf.level = 0.95,
    ...
){
  alternative = match.arg(alternative)

  r = apply(
    X = x,
    MARGIN = 1,
    FUN = stats::cor.test,
    y = y,
    alternative = alternative,
    method = "kendall",
    conf.level = conf.level,
    ...
  )

  out = list()
  out$statistic = unlist(lapply(X = r, FUN = '[[', 'statistic'))
  names(out$statistic) = NULL
  out$significance = unlist(lapply(X = r, FUN = '[[', 'p.value'))
  names(out$significance) = NULL

  # Describe
  attr(x = out, which = "comment") = getStatisticalMethodName(id = "kendall")

  return(out)
}

# Categorical output variable --------------------------------------------------

## Dichotomous variables -------------------------------------------------------

### Parametric Tests -----------------------------------------------------------

#' Two-sample t-Test
#'
#'@description Computes the two-sample t-Test for each feature.
#'
#'@inheritParams genericRowStat
#'
#'@param paired logical, whether to compute a paired t-test.
#'@param var character string indicating whether to treat the two variances as
#'being unequal (default) or equal. If \code{var = "unequal"}, the Welch
#'approximation to the degrees of freedom is used.
#'
#'@inherit genericRowStat return
#'
#'@description It internally calls functions from the\code{matrixTests}
#'package for the various tests.
#'
#'@inherit genericRowStat author
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Data
#'x = matrix(rnorm(100 * 20), 100, 20)
#'g = c(rep(0,10),rep(1,10))
#'
#'#Compute two-sample t-test
#'rowTwoSampleT(x = x, g = g, var = "equal")
#'
#'#Compute two-sample t-test with Welch approximation
#'rowTwoSampleT(x = x, g = g)
#'
#'#Compute two-sample paired t-test
#'rowTwoSampleT(x = x, g = g, paired = TRUE)
#'
#'@export
rowTwoSampleT <- function(
    x,
    g,
    null = 0,
    alternative = c("two.sided", "greater", "less"),
    conf.level = 0.95,
    var = c("unequal", "equal"),
    paired = FALSE
){
  # Setup
  alternative = match.arg(alternative);
  var = match.arg(var)
  stopifnot("'g' must be a vector or factor giving group membership. Please, check your input.\n" =
              (!is.null(g) && (is.factor(g) || is.vector(g)) && (length(g) == ncol(x))))

  #Convert to factor
  g = as.factor(g);
  #Get levels
  lvls = levels(g)
  #Get the indices of the 2 groups
  index.g1 = which(g == lvls[1])
  index.g2 = which(g == lvls[2])

  if(isTRUE(paired)){
    r = matrixTests::row_t_paired(
      x = x[, index.g1,drop=F],
      y = x[, index.g2,drop=F],
      alternative = alternative,
      conf.level = conf.level,
      null = null
    )
    text = getStatisticalMethodName(id = "t.test.paired")
  } else {
    r = switch(
      var,
      "unequal" = matrixTests::row_t_welch(
        x = x[,index.g1,drop=F],
        y = x[,index.g2,drop=F],
        alternative = alternative,
        conf.level = conf.level,
        null = null
      ),
      "equal" = matrixTests::row_t_equalvar(
        x = x[, index.g1,drop=F],
        y = x[, index.g2,drop=F],
        alternative = alternative,
        conf.level = conf.level,
        null = null
      )
    )

    text = switch(
      var,
      "unequal" = getStatisticalMethodName(id = "t.test.unequal"),
      "equal" = getStatisticalMethodName(id = "t.test.equal")
    )
  }

  r = list(statistic = r$statistic, significance = r$pvalue)

  # Describe
  attr(x = r, which = "comment") = text

  return(r);
}

#'Student's t-Test
#'
#'@description Computes the two-sample Student's pooled t-test for each feature.
#'
#'See the **Details** section below for further information.
#'
#'@inheritParams rowTwoSampleT
#'
#'@inherit genericRowStat return
#'
#'@details It is a wrapper to \code{\link[matrixTests]{row_t_equalvar}}
#'function.
#'
#'@inherit genericRowStat author
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Data
#'x = matrix(rnorm(100 * 20), 100, 20)
#'g = sample(c(0,1), 20, replace = TRUE)
#'
#'#Compute
#'rowEqualVarT(x = x, g = g)
#'
#'@export
rowEqualVarT = function(
    x,
    g,
    null = 0,
    alternative = c("two.sided", "greater", "less"),
    conf.level = 0.95
){
  alternative = match.arg(alternative)

  #Get the indices of the 2 groups
  g = as.factor(g);
  lvls = levels(g)
  index.g1 = which(g == lvls[1]);
  index.g2 = which(g == lvls[2]);

  #Results
  r = matrixTests::row_t_equalvar(
    x = x[, index.g1,drop=F],
    y = x[, index.g2,drop=F],
    alternative = alternative,
    conf.level = conf.level,
    null = null
  )

  r = list(statistic = r$statistic, significance = r$pvalue)
  # Describe
  attr(x = r, which = "comment") = getStatisticalMethodName(id = "t.test.equal")
  return(r);
}


#'Paired t-Test
#'
#'@description Computes the paired two-sample Student's t-test for each feature.
#'
#'See the **Details** section below for further information.
#'
#'@inheritParams rowTwoSampleT
#'
#'@inherit genericRowStat return
#'
#'@details It is a wrapper to \code{\link[matrixTests]{row_t_paired}}
#'function.
#'
#'@inherit genericRowStat author
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Data
#'x = matrix(rnorm(100 * 20), 100, 20)
#'g = c(rep(0,10),rep(1,10))
#'
#'#Compute
#'rowPairedT(x = x, g = g)
#'
#'@export
rowPairedT <- function(
    x,
    g,
    null = 0,
    alternative = c("two.sided", "greater", "less"),
    conf.level = 0.95
){
  alternative = match.arg(alternative)

  #Get the indices of the 2 groups
  g = as.factor(g);
  lvls = levels(g)
  index.g1 = which(g == lvls[1]);
  index.g2 = which(g == lvls[2]);

  #Results
  r = matrixTests::row_t_paired(
    x = x[, index.g1,drop=F],
    y = x[, index.g2,drop=F],
    alternative = alternative,
    conf.level = conf.level,
    null = null
  )
  r = list(statistic = r$statistic, significance = r$pvalue)
  # Describe
  attr(x = r, which = "comment") = getStatisticalMethodName(id = "t.test.paired")
  return(r);
}

#'Welch's t-Test
#'
#'@description Computes the two-sample t-test with the Welch modification to the
#'degrees of freedom for each feature.
#'
#'See the **Details** section below for further information.
#'
#'@inheritParams rowTwoSampleT
#'
#'@inherit genericRowStat return
#'
#'@details It is a wrapper to \code{\link[matrixTests]{row_t_welch}}
#'function.
#'
#'@inherit genericRowStat author
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Data
#'x = cbind(
#'  matrix(rnorm(n = 100 * 10, sd = 1), 100, 10),
#'  matrix(rnorm(n = 100 * 10, sd = 3), 100, 10)
#')
#'g = c(rep(0,10),rep(1,10))
#'
#'#Compute
#'rowUnequalVarT(x = x, g = g)
#'
#'@export
rowUnequalVarT <- function(
    x,
    g,
    null = 0,
    alternative = c("two.sided", "greater", "less"),
    conf.level = 0.95
){
  alternative = match.arg(alternative);

  #Convert to factor
  g = as.factor(g);
  #Get levels
  lvls = levels(g)
  #Get the indices of the 2 groups
  index.g1 = which(g == lvls[1])
  index.g2 = which(g == lvls[2])
  #Compute
  r = matrixTests::row_t_welch(
    x = x[,index.g1,drop=F],
    y = x[,index.g2,drop=F],
    alternative = alternative,
    conf.level = conf.level,
    null = null
  );
  r = list(statistic = r$statistic, significance = r$pvalue)
  # Describe
  attr(x = r, which = "comment") = getStatisticalMethodName(id = "t.test.unequal")
  return(r);
}

### Non-Parametric Tests -------------------------------------------------------


#'Two-Sample Wilcoxon Rank Sum and Signed Rank Tests
#'
#'@description Computes the two-sample Wilcoxon test for each feature.
#'
#'See the **Details** section below for further information.
#'
#'@inheritParams genericRowStat
#'@param paired logical, whether to compute a paired Wilcoxon test.
#'@inheritParams matrixTests::row_wilcoxon_paired
#'
#'@inherit genericRowStat return
#'
#'@details The function internally calls \code{\link[matrixTests]{row_wilcoxon_paired}}
#'for the paired test, and \code{\link[matrixTests]{row_wilcoxon_twosample}} for
#'the unpaired test.
#'
#'@inherit genericRowStat author
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Data
#'x = matrix(rnorm(100 * 20), 100, 20)
#'g = c(rep(0,10),rep(1,10))
#'
#'#Compute
#'rowTwoSampleWilcoxonT(x = x, g = g)
#'
#'#Compute paired
#'rowTwoSampleWilcoxonT(x = x, g = g, paired = TRUE)
#'
#'@export
rowTwoSampleWilcoxonT <- function(
    x,
    g,
    alternative = c("two.sided", "greater", "less"),
    paired = FALSE,
    null = 0,
    exact = NA,
    correct = TRUE
){
  alternative = match.arg(alternative);
  stopifnot("'g' must be a vector or factor giving group membership. Please, check your input.\n" =
              (!is.null(g) && (is.factor(g) || is.vector(g)) && (length(g) == ncol(x))))

  #Convert to factor
  g = as.factor(g);
  #Get levels
  lvls = levels(g)
  #Get the indices of the 2 groups
  index.g1 = which(g == lvls[1])
  index.g2 = which(g == lvls[2])

  if(isTRUE(paired)){
    r = matrixTests::row_wilcoxon_paired(
      x = x[, index.g1,drop=F],
      y = x[, index.g2,drop=F],
      alternative = alternative,
      null        = null,
      exact       = exact,
      correct     = correct
    )
    text = getStatisticalMethodName(id = "w.test.paired")
  } else {
    r = matrixTests::row_wilcoxon_twosample(
      x = x[, index.g1,drop=F],
      y = x[, index.g2,drop=F],
      alternative = alternative,
      null        = null,
      exact       = exact,
      correct     = correct
    )
    text = getStatisticalMethodName(id = "w.test.ranksum")
  }

  r = list(statistic = r$statistic, significance = r$pvalue)

  # Describe
  attr(x = r, which = "comment") = text

  return(r);
}


#'Wilcoxon Rank-Sum Test
#'
#'@description Computes the two-sample Wilcoxon test for each feature.
#'
#'See the **Details** section below for further information.
#'
#'@inheritParams rowTwoSampleWilcoxonT
#'
#'@inherit genericRowStat return
#'
#'@details It is a wrapper to \code{\link[matrixTests]{row_wilcoxon_twosample}}
#'function.
#'
#'@inherit genericRowStat author
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Data
#'x = matrix(rnorm(100 * 20), 100, 20)
#'g = sample(c(0,1), 20, replace = TRUE)
#'
#'#Compute
#'rowWilcoxonT(x = x, g = g)
#'
#'@export
rowWilcoxonT = function(
    x,
    g,
    alternative = c("two.sided", "greater", "less"),
    null = 0,
    exact = NA,
    correct = TRUE
  ){
  alternative = match.arg(alternative)

  #Get the indices of the 2 groups
  g = as.factor(g);
  lvls = levels(g)
  index.g1 = which(g == lvls[1]);
  index.g2 = which(g == lvls[2]);

  #Results
  r = matrixTests::row_wilcoxon_twosample(
    x = x[, index.g1,drop=F],
    y = x[, index.g2,drop=F],
    alternative = alternative,
    null        = null,
    exact       = exact,
    correct     = correct
  )

  r = list(statistic = r$statistic, significance = r$pvalue)
  # Describe
  attr(x = r, which = "comment") = getStatisticalMethodName(id = "w.test.ranksum")
  return(r);
}


#'Wilcoxon Signed-Rank Test
#'
#'@description Computes the two-sample Wilcoxon signed-rank test for each feature.
#'
#'See the **Details** section below for further information.
#'
#'@inheritParams rowTwoSampleWilcoxonT
#'
#'@inherit genericRowStat return
#'
#'@details It is a wrapper to \code{\link[matrixTests]{row_wilcoxon_paired}}
#'function.
#'
#'@inherit genericRowStat author
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Data
#'x = matrix(rnorm(100 * 20), 100, 20)
#'g = c(rep(0,10),rep(1,10))
#'
#'#Compute
#'rowPairedWilcoxonT(x = x, g = g)
#'
#'@export
rowPairedWilcoxonT = function(
    x,
    g,
    alternative = c("two.sided", "greater", "less"),
    null = 0,
    exact = NA,
    correct = TRUE
){
  alternative = match.arg(alternative)

  #Get the indices of the 2 groups
  g = as.factor(g);
  lvls = levels(g)
  index.g1 = which(g == lvls[1]);
  index.g2 = which(g == lvls[2]);

  #Results
  r = matrixTests::row_wilcoxon_paired(
    x = x[, index.g1,drop=F],
    y = x[, index.g2,drop=F],
    alternative = alternative,
    null        = null,
    exact       = exact,
    correct     = correct
  )

  r = list(statistic = r$statistic, significance = r$pvalue)
  # Describe
  attr(x = r, which = "comment") = getStatisticalMethodName(id = "w.test.paired")
  return(r);
}


## Polytomous variables --------------------------------------------------------

### Parametric Tests -----------------------------------------------------------

#'Fit an Analysis of Variance Model
#'
#'@description Computes the one-way analysis of variance test for each feature.
#'
#'See the **Details** section below for further information.
#'
#'@inheritParams matrixTests::row_oneway_equalvar
#'@param var character string, indicating whether the groups have \code{"equal"}
#'or \code{"unequal"} (default) variances
#'
#'@inherit genericRowStat return
#'
#'@details It is a wrapper to \code{\link[matrixTests]{row_oneway_equalvar}}
#'function.
#'
#'@inherit genericRowStat author
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Data
#'x = matrix(rnorm(100 * 20), 100, 20)
#'g = sample(c(0,1), 20, replace = TRUE)
#'
#'#Compute one-way analysis of variance
#'rowOneWayAnova(x = x, g = g, var = "equal")
#'
#'#Compute one-way analysis of variance with Welch correction
#'rowOneWayAnova(x = x, g = g)
#'
#'@export
rowOneWayAnova = function(x, g, var = c("unequal", "equal")){

  stopifnot("'g' must be a vector or factor giving group membership. Please, check your input.\n" =
              (!is.null(g) && (is.factor(g) || is.vector(g)) && (length(g) == ncol(x))))
  var = match.arg(var)

  r = switch(
    var,
    "equal"   = matrixTests::row_oneway_equalvar(x=x,g=g),
    "unequal" = matrixTests::row_oneway_welch(x=x,g=g)
  );
  r = list(statistic = r$statistic, significance = r$pvalue)
  # Describe
  attr(x = r, which = "comment") = switch(
    var,
    "equal"   = getStatisticalMethodName(id = "anova.equal"),
    "unequal" = getStatisticalMethodName(id = "anova.unequal")
  )

  return(r);
}


#'Fit an Analysis of Variance Model
#'
#'@description Computes the one-way analysis of variance test for each feature.
#'
#'See the **Details** section below for further information.
#'
#'@inheritParams matrixTests::row_oneway_equalvar
#'
#'@inherit genericRowStat return
#'
#'@details It is a wrapper to \code{\link[matrixTests]{row_oneway_equalvar}}
#'function.
#'
#'@inherit genericRowStat author
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Data
#'x = matrix(rnorm(100 * 20), 100, 20)
#'g = sample(c(0,1), 20, replace = TRUE)
#'
#'#Compute
#'rowEqualVarOneWayAnova(x = x, g = g)
#'
#'@export
rowEqualVarOneWayAnova = function(x, g){
  r = matrixTests::row_oneway_equalvar(
    x = x,
    g = g
  );
  r = list(statistic = r$statistic, significance = r$pvalue)
  # Describe
  attr(x = r, which = "comment") = getStatisticalMethodName(id = "anova.equal")
  return(r);
}


#'Fit an Analysis of Variance Model
#'
#'@description Computes the one-way analysis of variance test with Welch correction
#'for each feature.
#'
#'See the **Details** section below for further information.
#'
#'@inheritParams matrixTests::row_oneway_welch
#'
#'@inherit genericRowStat return
#'
#'@details It is a wrapper to \code{\link[matrixTests]{row_oneway_welch}}
#'function.
#'
#'@inherit genericRowStat author
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Data
#'x = matrix(rnorm(100 * 30), 100, 30)
#'g = sample(c(0,1,3), 30, replace = TRUE)
#'
#'#Compute
#'rowUnequalVarOneWayAnova(x = x, g = g)
#'
#'@export
rowUnequalVarOneWayAnova = function(x, g){
  r = matrixTests::row_oneway_welch(
    x = x,
    g = g
  );
  r = list(statistic = r$statistic, significance = r$pvalue)
  # Describe
  attr(x = r, which = "comment") = getStatisticalMethodName(id = "anova.unequal")
  return(r);
}

### Non-Parametric Tests -------------------------------------------------------

#'Kruskal-Wallis Rank Sum Test
#'
#'@description Computes the Kruskal-Wallis rank sum test for each feature.
#'
#'See the **Details** section below for further information.
#'
#'@inheritParams genericRowStat
#'
#'@inherit genericRowStat return
#'
#'@details It is a wrapper to \code{\link[matrixTests]{row_kruskalwallis}}
#'function.
#'
#'@inherit genericRowStat author
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Data
#'x = matrix(rnorm(100 * 20), 100, 20)
#'g = sample(c(0,1), 20, replace = TRUE)
#'
#'#Compute
#'rowKruskalWallis(x = x, g = g)
#'
#'@export
rowKruskalWallis <- function(x, g){
  stopifnot("'g' must be a vector or factor giving group membership. Please, check your input.\n" =
              (!is.null(g) && (is.factor(g) || is.vector(g)) && (length(g) == ncol(x))))
  r = matrixTests::row_kruskalwallis(
    x = x,
    g = g
  );
  r = list(statistic = r$statistic, significance = r$pvalue)
  # Describe
  attr(x = r, which = "comment") = getStatisticalMethodName(id = "kruskal.wallis")
  return(r);
}


# Categorical input and output variables --------------------------------------
## Polytomous variables --------------------------------------------------------
### Non-Parametric Tests -------------------------------------------------------

#'Pearson's Chi-squared Test of Independence
#'
#'
#'@description Computes the Pearson's Chi-squared test of independence for each
#'row vector in \code{x}.
#'
#'See the **Details** section below for further information.
#'
#'@inheritParams genericRowStat
#'@inheritParams stats::chisq.test
#'
#'@inherit genericRowStat return
#'
#'@details It is a wrapper to \code{\link[stats]{chisq.test}} function.
#'
#'@inherit genericRowStat author
#'
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Data
#'x = rbind(
#'   matrix(sample(c("mut", "wt"),30,TRUE), 1, 30),
#'   matrix(sample(c("m", "f")   ,30,TRUE), 1, 30)
#')
#'g = sample(c("a","b","c"), 30, replace = TRUE)
#'
#'#Compute
#'rowPearsonChiSq(x = x, g = g, simulate.p.value = TRUE)
#'
#'@export
rowPearsonChiSq <- function(
    x,
    g,
    correct = TRUE,
    simulate.p.value = FALSE,
    B = 2000
  ){

  stopifnot("'g' must be a vector or factor giving group membership. Please, check your input.\n" =
              (!is.null(g) && (is.factor(g) || is.vector(g)) && (length(g) == ncol(x))))

  r = apply(
    X = x,
    MARGIN = 1,
    FUN = stats::chisq.test,
    y = g,
    correct = correct,
    simulate.p.value = simulate.p.value,
    B = B
  )

  out = list()
  out$statistic = unlist(lapply(X = r, FUN = '[[', 'statistic'))
  names(out$statistic) = NULL
  out$significance = unlist(lapply(X = r, FUN = '[[', 'p.value'))
  names(out$significance) = NULL

  # Describe
  attr(x = out, which = "comment") = getStatisticalMethodName(id = "chisq.test")

  return(out)
}


# Time-to-event output variable ------------------------------------------------

#'Fit Univariate Proportional Hazards Regression Model
#'
#'@description Fits a Cox proportional hazards regression model.
#'
#'@param x independent variable
#'@param y \code{data.frame} with two columns, \code{time} and \code{status}
#'@param ... further arguments to \code{\link[survival]{coxph}}
#'
#'@details It is a wrapper to \code{\link[survival]{coxph}}
#'function.
#'
#'@inherit genericRowStat author
#'
#'
#'@examples
#'\dontrun{
#'#Seed
#'set.seed(1010)
#'
#'#Data
#'x = c(0,2,1,1,1,0,0)
#'y = data.frame(
#'   time = c(4,3,1,1,2,2,3),
#'   status = c(1,1,1,0,1,1,0)
#')
#'
#'#Compute
#'coxPH(x = x, y = y)
#'}
#'@keywords internal
coxPH <- function(x, y, ...){
  #Fits a Cox proportional hazards regression model
  r = survival::coxph(
    formula = survival::Surv(time, status) ~ x,
    data = cbind(y, data.frame(x=x,stringsAsFactors = F)),
    ...
  )
  #Produces a summary of a fitted coxph model
  r = summary(r);
  #Get the Hazard Ratio and the coefficient p-value
  r = list(
    statistic = r[['coefficients']][,'exp(coef)'],
    significance = r[['coefficients']][,'Pr(>|z|)']
  );
  return(r)
}

#'Fit Univariate Proportional Hazards Regression Model
#'
#'@description Fits a Cox proportional hazards regression model for each
#'row vector in \code{x}.
#'
#'@param x independent variable
#'@param y \code{data.frame} with two columns, \code{time} and \code{status}
#'@param ... further arguments to \code{\link[survival]{coxph}}
#'
#'@inherit genericRowStat return
#'
#'@details It is a wrapper to \code{\link[survival]{coxph}}
#'function.
#'
#'@inherit genericRowStat author
#'
#'
#'@examples
#'#Seed
#'set.seed(1010)
#'
#'#Data
#'x = matrix(rnorm(100 * 7), 100, 7)
#'y = data.frame(
#'   time = c(4,3,1,1,2,2,3),
#'   status = c(1,1,1,0,1,1,0)
#')
#'
#'#Compute
#'rowCoxPH(x = x, y = y)
#'
#'@export
rowCoxPH <- function(
    x,
    y,
    ...
  ){

  r = apply(
    X = x,
    MARGIN = 1,
    FUN = coxPH,
    y = y,
    ...
  )
  r = list(
    statistic = unlist(lapply(X = r, FUN = '[[', 'statistic')),
    significance = unlist(lapply(X = r, FUN = '[[', 'significance'))
  )
  # Describe
  attr(x = r, which = "comment") = getStatisticalMethodName(id = "coxph")

  return(r)
}
