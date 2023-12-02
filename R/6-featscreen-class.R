#'@include 1-log-functions.R
NULL

# Featscreen Class -------------------------------------------------------------

#'Constructor of the `featscreen` Class
#'
#'@description This is the low-level constructor of the \code{featscreen} class
#'representing a set of features resulting from a screening procedure.
#'
#'@param method character string, the id of the used screening method.
#'@param multi (optional) character string, the id of the used multi-response aggregation method.
#'@param selection character string, the id of the used selection method.
#'@param summary character string, a textual summary of the screening.
#'@param n integer, the dimension of the feature space.
#'@param features string vector, the names of the features constituting the feature space.
#'@param keep integer or logical vector, the features to keep.
#'@param ranks (optional) integer vector, the ordering indices.
#'
#'@return An object of class `featscreen`.
#'
#'@details An object of class `featscreen` is a named list with eight elements:
#'\describe{
#' \item{`method`}{the id of the used screening method}
#' \item{`multi`}{the id of the used multi-response aggregation method}
#' \item{`selection`}{the id of the used selection method}
#' \item{`summary`}{a textual summary of the screening}
#' \item{`n`}{the dimension of the feature space}
#' \item{`features`}{the feature names}
#' \item{`keep`}{the features to keep}
#' \item{`ranks`}{the feature ranks}
#'}
#'
#'Functions to facilitate access to the data stored in a `resampling` object are
#'available:
#'
#'* `?getScreeningMethodId`: returns the screening method id
#'* `?getMultiresponseAggregationMethodId`: returns the multi-response aggregation method id
#'* `?getSelectionMethodId`: returns the selection method id
#'* `?getSummary`: returns the textual summary of the screening
#'* `?getFeatureDimensionality`: returns the dimension of the feature space
#'* `?getFeatureNames`: returns the feature names
#'* `?getScreenedFeatures`: returns the features to keep
#'* `?getFeatureRanks`: returns the feature ranks
#'
#'Other useful functions include:
#'
#'* \code{\link[=print.featscreen]{print}}: print a summary of the `featscreen` object
#'
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
newFeatscreen <- function(
    method    = character(),
    multi     = character(),
    selection = character(),
    summary   = character(),
    n         = integer(),
    features  = character(),
    keep      = integer(),
    ranks     = integer()
) {

  stopifnot(is.character(method))
  stopifnot(is.character(multi))
  stopifnot(is.character(selection))
  stopifnot(is.character(summary))
  stopifnot(is.integer(n))
  stopifnot(is.character(features))
  stopifnot(is.logical(keep) || is.vector(keep))
  stopifnot(is.integer(ranks))

  # 'keep' can be
  # 1) a logical vector of length n
  # 2) an integer vector of length <= n (the ranked features to keep)
  stopifnot(is.logical(keep) || is.integer(keep))

  structure(
    .Data = list(
      method    = method,
      multi     = multi,
      selection = selection,
      summary   = summary,
      n         = n,
      features  = features,
      keep      = keep,
      ranks     = ranks
    ),
    class = "featscreen"
  )
}

#'Validator of `featscreen` Objects
#'
#'@description This is the validator of the `featscreen` objects.
#'
#'@param x a \code{\link{featscreen}} object.
#'
#'@inherit newFeatscreen return
#'
#'@inherit newFeatscreen author
#'
#'@examples \dontrun{
#'#No error is raised
#'validateFeatscreen(newFeatscreen())
#'
#'#An error is raised
#'validateFeatscreen(
#' newFeatscreen(
#'  method = 'cor.test',
#'  n = 3,
#'  keep = c(1, 3)
#' )
#')
#'}
#'@keywords internal
validateFeatscreen <- function(x){
  #Check the dimension of the feature space
  if(isTRUE(length(x$n)==1)){
    if(isFALSE(x$n > 0)){
      stop("'n' must be a positive integer value.", call. = F)
    }
  } else if(isTRUE(length(x$n)>1)){
    stop("'n' must be of length 1.", call. = F)
  }

  #Check indices of screened elements
  if(isTRUE(length(x$keep) > 0)){
    if(isFALSE( all(!is.na(x$keep)) & (all(is.logical(x$keep) && length(x$keep)==x$n) ||
                                     all(is.integer(x$keep) & (x$keep>0 & x$keep<=x$n))) )){
      stop("'keep' must be a logical or integer vector containing non-missing,",
            "positive less than or equal to 'n' values.",
           call. = F)
    }
  }
  #return
  return(x)
}


#'Constructor of the `featscreen` class
#'
#'@description This is the constructor of the `featscreen` class.
#'
#'@inheritParams newFeatscreen
#'
#'@inherit newFeatscreen return
#'
#'@inherit newFeatscreen details
#'
#'@inherit newFeatscreen author
#'
#'@examples
#'#default
#'featscreen()
#'
#'#featscreen object
#'featscreen(
#'  method = 'cor.test',
#'  selection = 'cutoff',
#'  summary = '3 out of 5 features selected by a cutoff.',
#'  n = 5,
#'  keep = c(TRUE,FALSE,TRUE,FALSE,TRUE),
#'  features = paste0("f", seq_len(5)),
#'  ranks = c(1,5,3,4,2)
#')
#'
#'@export
featscreen <- function(
    method  = c(
      "cor.test",
      "t.test",
      "w.test",
      "anova",
      "kruskal.wallis",
      "chisq.test",
      "coxph",
      "moderated.t",
      "moderated.F",
      "sam.test",
      "missing.value",
      "above.median",
      "above.minimum",
      "median",
      "variability"
    ),
    multi = character(),
    selection = c(
      "cutoff",
      "rank",
      "percentile",
      "fpr",
      "fdr"
    ),
    summary  = character(),
    n        = integer(),
    features = character(),
    keep     = vector(),
    ranks    = integer()
){
  #coerce input
  n = as.integer(n)
  ranks = as.integer(ranks)
  if(isFALSE(is.logical(keep))){keep = as.integer(keep)}

  #check input
  method = match.arg(method)
  selection = match.arg(selection)

  #create object
  o = newFeatscreen(
    method    = method,
    multi     = multi,
    selection = selection,
    summary   = summary,
    n         = n,
    features  = features,
    keep      = keep,
    ranks     = ranks
  )

  #validate
  o = validateFeatscreen(o)

  #return
  return(o)
}

# Getters ----------------------------------------------------------------------

#'Get the screening method id
#'
#'@description Get the screening method id from a [`featscreen`] object.
#'
#'@param x an object of class [`featscreen`]
#'
#'@return A character string, the screening method id.
#'
#'@author Alessandro Barberis
#'
#'@export
getScreeningMethodId <- function(x){
  #return
  return(x$method)
}

#'Get the multi-response aggregation method id
#'
#'@description Get the multi-response aggregation method id from a [`featscreen`] object.
#'
#'@param x an object of class [`featscreen`].
#'
#'@return A character string, the multi-response aggregation method id.
#'
#'@author Alessandro Barberis
#'
#'@export
getMultiresponseAggregationMethodId <- function(x){
  #return
  return(x$multi)
}

#'Get the selection method id
#'
#'@description Get the selection method id from a [`featscreen`] object.
#'
#'@param x an object of class [`featscreen`].
#'
#'@return A character string, the selection method id.
#'
#'@author Alessandro Barberis
#'
#'@export
getSelectionMethodId <- function(x){
  #return
  return(x$selection)
}

#'Get the screening summary
#'
#'@description Get the summary from a [`featscreen`] object.
#'
#'@param x an object of class [`featscreen`].
#'
#'@return A character string, the textual summary of the screening process.
#'
#'@author Alessandro Barberis
#'
#'@export
getSummary <- function(x){
  #return
  return(x$summary)
}

#'Get the dimension of the feature space
#'
#'@description Get the feature space dimension from a [`featscreen`] object.
#'
#'@param x an object of class [`featscreen`].
#'
#'@return An integer value, the number of screened features.
#'
#'@author Alessandro Barberis
#'
#'@export
getFeatureDimensionality <- function(x){
  return(x$n)
}

#'Get the screened features
#'
#'@description Get the screened features from a [`featscreen`] object.
#'
#'@param x an object of class [`featscreen`].
#'
#'@return A logical or integer vector indicating the features to keep.
#'If an integer vector of length equal to the dimension of the feature space is
#'returned, it is assumed to contain the indices of the ranked features.
#'
#'@author Alessandro Barberis
#'
#'@export
getScreenedFeatures <- function(x){
  return(x$keep)
}

#'Get the feature names
#'
#'@description Get the feature names from a [`featscreen`] object.
#'
#'@param x an object of class [`featscreen`].
#'
#'@return A character vector indicating the feature names.
#'
#'@author Alessandro Barberis
#'
#'@export
getFeatureNames <- function(x){
  return(x$features)
}

#'Get the feature ranks
#'
#'@description Get the feature ranks from a [`featscreen`] object.
#'
#'@param x an object of class [`featscreen`].
#'
#'@return An integer vector indicating the ordering feature indices.
#'
#'@author Alessandro Barberis
#'
#'@export
getFeatureRanks <- function(x){
  return(x$ranks)
}

# Methods ----------------------------------------------------------------------

#'Check a `featscreen` object
#'
#'@description Function to check if an object is a [`featscreen`] object.
#'
#'@param x an object of class [`featscreen`].
#'
#'@return Returns `TRUE` if its argument is a valid [`featscreen`] object.
#'
#'@author Alessandro Barberis
#'
#'@examples
#'#valid object
#'is.featscreen(x = featscreen())
#'
#'#invalid object
#'is.featscreen(x = 3)
#'
#'@export
is.featscreen <- function(x){
  bool = TRUE
  #check structure
  if(isFALSE(is.list(x) & identical(class(x), "featscreen") & length(x)==8 &
             all(names(x) %in% names(newFeatscreen())) )){
    return(FALSE)
  } else {
    bool = tryCatch(
      expr = {
        x = validateFeatscreen(x)
        return(TRUE)
      },
      error = function(e){return(FALSE)}
    )
  }
  return(bool)
}

#'Print a `featscreen` object
#'
#'@description Print a summary of the [`featscreen`] object.
#'
#'@param x an object of class [`featscreen`].
#'@param show.top logical indicating whether to show \code{top} ranked features.
#'@param top integer, the number of top ranked features to show.
#'@param show.names logical indicating whether to show indices or names.
#'@param ... additional print arguments
#'
#'@return Silently return \code{x}.
#'
#'@examples
#'#featscreen object
#'x = featscreen(
#'  method = 'cor.test',
#'  selection = 'cutoff',
#'  summary = '3 out of 5 features selected by a cutoff.',
#'  n = 5,
#'  keep = c(TRUE,FALSE,TRUE,FALSE,TRUE),
#'  features = paste0("f", seq_len(5)),
#'  ranks = c(1,5,3,4,2)
#')
#'
#'#print
#'print(x)
#'
#'@export print.featscreen
#'@export
print.featscreen <- function(x, show.top = TRUE, top = 5L, show.names = TRUE, ...){
  #check input
  stopifnot(is.numeric(top))
  stopifnot(is.logical(show.top))
  stopifnot(is.logical(show.names))

  #coerce
  top = as.integer(top)

  #get data
  ##screening method
  screeningMethodId = getScreeningMethodId(x)
  ##dimension of feature space
  featureDim = getFeatureDimensionality(x)
  ##screened features
  screenedFeatures = getScreenedFeatures(x)
  ##number of selected features
  numScreenedFeatures = if(is.logical(screenedFeatures)){
    sum(screenedFeatures)
  } else {
    length(screenedFeatures)
  }
  ##feature names
  featNames = getFeatureNames(x)
  ##feature ranks
  featRanks = getFeatureRanks(x)

  # Update
  n = min(top, length(featRanks))


  # Get text summary
  textSummary = getSummary(x)
  if(isTRUE(length(textSummary)>0)){
    cat("\n")
    #summary
    cat(
      strwrap(
        x = getSummary(x)
      ),
      sep = "\n"
    )
    cat("\n")
  }

  if(isTRUE(show.top && length(featRanks)>0)){
    topFeats = featRanks[seq_len(n)]
    if(isTRUE(show.names && (length(featNames)>0))){
      topFeats = featNames[topFeats]
    }
    cat(
      paste(
        "Top", n, "ranked features:",
        paste(topFeats, collapse = ", ")
      ),
      sep = "\n"
    )
    cat("\n")
  }

  #return
  invisible(x)
}
