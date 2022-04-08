#'Obtaining indices associated with loss of production.
#'@description Allows calculating loss of production per
#'loss source (L.P.L.S.) and its total, maximum estimated production (M.E.P.),
#' percentage of loss of production per loss source (Percentage_L.P.L.S.=P.L.P.L.S.)
#' and its total, n_per_sample, and attention level (A.L.). \cr
#' Equations: \cr
#'   *L.P.L.S. = total n of the L.S. x R.P. of the L.S. Where R.P. is R2 x (1 - P)
#'    when it is of the first degree, or R.P. = ((R2 x (1 - P))x(B2/B1) when it
#'    is of the second degree. Where, R2 = determination coefficient and
#'    P = significance of ANOVA, B1 = regression coefficient, and
#'    B2 = regression coefficient (variable2), of the simple regression equation of the L.S.\cr
#'    *M.E.P. = Total production (P.) + SUM L.P.L.S.1 + ....L.P.L.S.n. \cr
#'    *Percentage_L.P.L.S. = (L.P.L.S./M.E.P.) x 100. \cr
#'    * n_per_sample is n per sample \cr
#'    *A.L. = (n of the L.S. per sample x 0.75)/Percentage_L.P.L.S.. \cr
#'    Where, n of the L.S. per sample = n/(number of trees/evaluation frequency/years/number of plant parts evaluated).
#'     In this case, the number of trees = 20; evaluation frequency = 12 months per year for leaves, trunks, and branches,
#'      two months for bunches of flowers per year, and three months for bunches of fruits per year; years = three;
#'      and the number of plant parts evaluated = 12 leaves, 12 bunches of flowers and/or fruits,
#'      and one trunk per tree/evaluation. And, 0.75 = 1 percent of loss fruits x 0.75 (safety margin).

#'
#'@usage LossProduction(DataLossSource,Prod,Evaluation,SegurityMargen=0.75,
#'MaximumToleranceOfLossFruits=1,verbose=TRUE)
#'@param DataLossSource It is an matrix object containing data from loss sources.
#'@param Prod Matrix with a column containing the production data.
#'@param Evaluation Matrix containing three lines with the number of evaluations performed on each individual,
#'the number of months evaluated and the number of evaluations performed per month.
#'Must have a column for each source of loss.
#'@param SegurityMargen  Segurity margen (default=0.75)
#'@param MaximumToleranceOfLossFruits Maximum tolerance in percentage  (default=1)
#'@param verbose Logical value (TRUE/FALSE). TRUE displays the results of the

#'@author Germano Leao Demolin-Leite (Instituto de Ciencias Agrarias da UFMG) \cr
#' Alcinei Mistico Azevedo (Instituto de Ciencias Agrarias da UFMG)
#'@return The function returns several indices associated with the production loss.
#'@seealso  \code{\link{EffectivenessOfSolution}} ,  \code{\link{NonAttentionLevel}}  ,  \code{\link{LossSource}}
#'@importFrom stats lm
#'@export
#'
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

LossProduction=function(DataLossSource,Prod,Evaluation,SegurityMargen=0.75,MaximumToleranceOfLossFruits=1,verbose=TRUE){
  Data=DataLossSource

  D=Data
  Avaliacoes=Evaluation
  Res=LossSource(DataLoss = D,DataProd = Prod,verbose=verbose)$Res1

  id=Res$P.I.I.>0

  LPLS=Res[id,2]*Res[id,1]
  MEP=sum(LPLS)+sum(Prod)
  PLPLS=100*(LPLS)/MEP
  Avaliacoes2=Avaliacoes[,-1]
  nBYsample=colSums(D)[id]/(nrow(D)*unlist(Avaliacoes2[1,id])*unlist(Avaliacoes2[2,id])*unlist(Avaliacoes2[3,id]))

  LC=(nBYsample*SegurityMargen*MaximumToleranceOfLossFruits)/PLPLS


  Res1=cbind(L.P.L.S.=LPLS,M.E.P.=MEP,P.L.P.L.S.=PLPLS,n_per_sample=nBYsample,A.L=LC)
Res2=colSums(Res1[,c(1,3)])
list(Res1=Res1,Res2=Res2)

}
