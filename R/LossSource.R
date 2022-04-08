#'Obtaining indices associated with sources of loss
#'@description These functions allow to calculate the total n of the L.S. (n),
#' R.P., ks, c, ds, n.I.I., Sum.n.I.I., and percentage of I.I. (P.I.I.) by each L.S..\cr
#'Equations:
#'n=total n per sample \cr
#'k.s.= R.P./n \cr
#'c = SUM of occurrence of L.S. on the samples, where, absence = 0 or presence = 1.\cr
#'ds = 1 - P of the chi-square test of L.S. on the samples.\cr
#'n.I.I.=ks x c x ds \cr
#'Sum.n.I.I. = sum of all n.I.I.\cr
#'Percentage of I.I. (P.I.I.)=(n.I.I. of each L.S./sum of all n.I.I.)*100

#'@usage LossSource(DataLoss,DataProd,verbose)
#'@param DataLoss It is an matrix object containing data from loss sources.
#'@param DataProd Matrix with a column containing the production data.
#'@param verbose Logical value (TRUE/FALSE). TRUE displays the results of the analysis.

#'@author Germano Leao Demolin-Leite (Instituto de Ciencias Agrarias da UFMG) \cr
#' Alcinei Mistico Azevedo (Instituto de Ciencias Agrarias da UFMG)
#'@return The function returns several indices associated with the loss source.
#'@seealso  \code{\link{EffectivenessOfSolution}} ,  \code{\link{NonAttentionLevel}}
#'@importFrom stats lm
#'@export
#'@examples
#\dontrun{
#' library(ImportanceIndice)
#' data("DataLossSource")
#' data("DataSolutionSource")
#' data("DataProduction")
#' data("DataNumberSamples")
#'
#' Distribution_LossSource(DataLossSource)
#' Distribution_SolutionSource(DataSolutionSource)
#'
#' #################################################
#' ###################################################
#'
#'
#' LS=LossSource(DataLoss = DataLossSource,DataProd = DataProduction,verbose = TRUE)
#' LS
#'
#' LP=LossProduction(Data=DataLossSource,Prod = DataProduction,
#'                   Evaluation=DataNumberSamples,
#'                   SegurityMargen=0.75,MaximumToleranceOfLossFruits=1)
#' LP
#'
#' ES=EffectivenessOfSolution(DataLossSource=DataLossSource,
#'                            DataSolutionSource=DataSolutionSource,Production=DataProduction)
#' ES
# }
#'

LossSource=function(DataLoss,DataProd,verbose){
  Prod=DataProd
  D=DataLoss
  n=colSums(D)
  RP=R.P(D,Prod,verbose=FALSE)
  KS=RP/n
  c=colSums(D>0)
  ds=NULL
  chisqq=Class.=NULL
  for(i in 1:ncol(D)){
    chisq=suppressWarnings(chisq.test(D[,i]))
    chisqq=c(chisqq,chisq$p.value)
#    if(verbose==TRUE){
#      cat(green("################################################  \n"))
 #     cat(colnames(D)[i],"\n")
  #    cat(green("################################################  \n"))
   #   print(chisq)
    #  ds=c(ds,chisq$p.value)
     # cat("________________________________________________  \n")

    #  print(paste("Chosen:" ,R.P2(D = D[,i],Prod = Prod,verbose=TRUE)))

  #}
  }

  ds=sapply(1:ncol(D), function (i) 1-suppressWarnings(chisq.test(D[,i])$p.value))
  NII=KS*c*ds
  PII=100*NII/sum(NII)


 RP0=R.Pescolha(D,Prod)


  Res1= data.frame(
    n=colSums(D),
    R.P.=RP0,
    ks=RP0/n,
    c=colSums(D>0),
    ds=sapply(1:ncol(D), function (i) 1-suppressWarnings(chisq.test(D[,i])$p.value)),
    n.I.I.=KS*c*ds,
    Sum.n.I.I.=sum(KS*c*ds),
    `P.I.I.`=100*NII/sum(NII))


  Var=apply(D,2,var)
  Mean=apply(D,2,mean)
  p.Value=chisqq


  Class.=NULL
  for(i in 1:length(p.Value)){
    p.V=p.Value[i]
  pv2=100*(p.V)
  class="Random"
  if(pv2<2.5){
    class="Aggregated"
  }

  if(pv2>97.5){
    class="Regular"
  }

  Class.=c(Class.,class)
  }



  Res2=data.frame(Var=Var,Mean=Mean,p.Value=p.Value,Aggregation=Class.)

  return(list(Res1=round(Res1,6),Res2=Res2))
}
