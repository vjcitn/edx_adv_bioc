#' produce post-filter Tasks for mlr3
#' @param taskid character(1) arbitrary label
#' @param se SummarizedExperiment instance
#' @param classvar character(1) optional name of colData element to use for classification 
#' @param nfeat numeric(1) number of features to retain
#' @param tmaker a Task as defined in mlr3
#' @param filtfun a function that computes a Filter score in the mlr3filters framework, defaults
#' to the "variance" scoring method
#' @param \dots passed to `tmaker$new()` for the filtered task production
#' @export
se_to_filtered_task = function(taskid, se, classvar, nfeat=100, tmaker=mlr3::TaskClassif, 
   filtfun=function(x) flt("variance")$calculate(x, nfeat=nfeat), ... ) { 
 mat = t(assay(se))
 colnames(mat) = make.names(colnames(mat))
 if (!missing(classvar)) {
  dfr = data.frame(mat, tmpclass=factor(se[[classvar]]))
  names(dfr)[ncol(dfr)] = classvar
  tmptask = tmaker$new("tmp", dfr, target=classvar)
  } else {
  dfr = data.frame(mat)
  tmptask = tmaker$new("tmp", dfr)
 }
 filt = filtfun(tmptask) # gives scores and features
 kp = as.data.frame(as.data.table(filt))$feature[1:nfeat]
 if (!missing(classvar)) {
  dfr2 = data.frame(mat[,kp], class=factor(se[[classvar]]))
  names(dfr2)[ncol(dfr2)] = classvar
  return(tmaker$new(taskid, dfr2, target=classvar, ...))
  }
 dfr2 = data.frame(mat[,kp])
 tmaker$new(taskid, dfr2, ...)
}
