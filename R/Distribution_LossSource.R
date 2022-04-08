#'Loss source distribution information
#'@description Indicates the distribution of sources of loss: aggregate, random or regular.
#'@usage Distribution_LossSource(DataLoss)
#'@param DataLoss It is an matrix object containing data from loss sources.
#'@author Germano Leao Demolin-Leite (Instituto de Ciencias Agrarias da UFMG) \cr
#' Alcinei Mistico Azevedo (Instituto de Ciencias Agrarias da UFMG)
#'@return Return distribution of sources of loss: aggregate, random or regular.
#'@seealso  \code{\link{EffectivenessOfSolution}} ,  \code{\link{NonAttentionLevel}}  ,  \code{\link{LossSource}}
#'@importFrom stats lm
#'@export
#'@examples
#' library(ImportanceIndice)
#'data("DataLossSource")
#'data("DataSolutionSource")
#'
#'Distribution_LossSource(DataLossSource)
#'Distribution_SolutionSource(DataSolutionSource)

#'

Distribution_LossSource=function(DataLoss){
  verbose=FALSE

  D=DataLoss
  n=colSums(D)
pv=NULL
Class.=NULL
for(i in 1:ncol(D)){
  chisq=suppressWarnings(chisq.test(D[,i]))
  pv=c(pv,chisq$p.value)
  pv2=100*(1-chisq$p.value)
  class="Random"
  if(pv2<2.5){
    class="Aggregated"
  }

  if(pv2>97.5){
    class="Regular"
  }

  Class.=c(Class.,class)
  }



  Var=apply(D,2,FUN = "var")
  Mean=apply(D,2,mean)
  p.Value=pv
  Res2=data.frame(Var=Var,Mean=Mean,p.Value=p.Value,Aggregation=Class.)

  return(Res2=Res2)
}
