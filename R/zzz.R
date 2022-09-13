###########################################################################
###################################
R.P=function(D,Prod,verbose=FALSE){
  rp=NULL
 # D=matrix(D,nrow=nrow(Prod))
  for(i in 1:ncol(D)){





    B2=100

    xx=unlist(strsplit(colnames(D)[i],"_"))[1]
    positivo=FALSE
    if((xx=="X")|(xx=="X")){positivo=TRUE}



    x=D[,i]
    Prod=as.matrix(Prod)
    m=lm(Prod~x)
    m2=lm(Prod~x+I(x^2))

    ANOVA=anova(m)
    ANOVA2=anova(m2)


    ms1=summary(m)
    ms2=summary(m2)

    if(verbose==TRUE){
      cat(green("################################################ \n"))
      cat(green(colnames(D)[i],"\n"))
      cat(green("################################################ \n"))


      print(ms1)
      cat("___________________________________________________  \n")
      print(ms2)
    }

    pvf1=1-pf(ms1$fstatistic[1],1,nrow(D)-2)
    pvf2=1-pf(ms2$fstatistic[1],2,nrow(D)-3)

    if(nrow(coefficients(ms2))==2){pvf2=1}

    hipo=sum(pvf1<=0.05,pvf2<=0.05)
    util=hipo>0


    if(util==TRUE){
      if(hipo==1){
        if(pvf1<=0.05){
          ANOVA=anova(m)
          p=pvf1
          R2=cor(predict(m),Prod)^2
          B1=coefficients(m)[1]
          B2=coefficients(m)[2]
          res=abs(R2*(1-p))

          if(verbose==TRUE){
            cat("___________________________________________________  \n")
            print(paste("Chosen: ","linear"))
            }
        }

        if(pvf2<=0.05){
          ANOVA=anova(m2)
          p=pvf2
          R2=cor(predict(m2),Prod)^2
          B1=coefficients(m2)[2]
          B2=coefficients(m2)[3]
          res=abs(R2*(1-p)*(B2/B1))
          if(verbose==TRUE){
            cat("___________________________________________________  \n")
            print(paste("Chosen: ","quadratic"))
          }

           }

      }
      if(hipo==2){
        pv1= ms1$coefficients[,4]
        pv2= ms2$coefficients[,4]
        if(pv2[length(pv2)]<=0.05){
          ANOVA=anova(m2)
          p=pvf2
          R2=cor(predict(m2),Prod)^2
          B1=coefficients(m2)[2]
          B2=coefficients(m2)[3]
          res=abs(R2*(1-p)*(B2/B1))

          if((B2>0)&(B1<0)){
            ANOVA=anova(m)
            p=pvf1
            R2=cor(predict(m),Prod)^2
            B1=coefficients(m)[1]
            B2=coefficients(m)[2]

            res=abs(R2*(1-p))
          }
          if(verbose==TRUE){
            cat("___________________________________________________  \n")
            print(paste("Chosen: ","quadratic"))
          }

        }
        if(pv2[length(pv2)]>0.05){
          ANOVA=anova(m)
          p=pvf1
          R2=cor(predict(m),Prod)^2
          B1=coefficients(m)[1]
          B2=coefficients(m)[2]
          res=abs(R2*(1-p))
          if(verbose==TRUE){
            cat("___________________________________________________  \n")
            print(paste("Chosen: ","linear"))
          }
          }

      }

    }




    if(is.na(B2)){B2=100}
    if((B2>0)&(positivo==FALSE)){util=FALSE}
    if(util==FALSE){res=0}
    rp=c(rp,res)
  }

  return(rp)
}

###########################################################################
###################################
R.P2=function(D,Prod,verbose=TRUE){
  rp=NULL
  # D=matrix(D,nrow=nrow(Prod))






    B2=100

   # xx=unlist(strsplit(names(D),"_"))[1]
    positivo=FALSE
  #  if((xx=="X")|(xx=="X")){positivo=TRUE}



    x=c(D)
    Prod=as.matrix(Prod)
    m=lm(Prod~x)
    m2=lm(Prod~x+I(x^2))

    ANOVA=anova(m)
    ANOVA2=anova(m2)


    ms1=summary(m)
    ms2=summary(m2)


      print(ms1)
      cat("___________________________________________________  \n")
      print(ms2)


    pvf1=1-pf(ms1$fstatistic[1],1,nrow(D)-2)
    pvf2=1-pf(ms2$fstatistic[1],2,nrow(D)-3)

    if(nrow(coefficients(ms2))==2){pvf2=1}

    hipo=sum(pvf1<=0.05,pvf2<=0.05)
    util=hipo>0


    if(util==TRUE){
      if(hipo==1){
        if(pvf1<=0.05){
          ANOVA=anova(m)
          p=pvf1
          R2=cor(predict(m),Prod)^2
          B1=coefficients(m)[1]
          B2=coefficients(m)[2]
          res=abs(R2*(1-p))


            cat("___________________________________________________  \n")
            print(paste("Chosen: ","linear"))
Esco="linear"
        }

        if(pvf2<=0.05){
          ANOVA=anova(m2)
          p=pvf2
          R2=cor(predict(m2),Prod)^2
          B1=coefficients(m2)[2]
          B2=coefficients(m2)[3]
          res=abs(R2*(1-p)*(B2/B1))

            cat("___________________________________________________  \n")
            print(paste("Chosen: ","quadratic"))
            Esco="quadratic"

        }

      }
      if(hipo==2){
        pv1= ms1$coefficients[,4]
        pv2= ms2$coefficients[,4]
        if(pv2[length(pv2)]<=0.05){
          ANOVA=anova(m2)
          p=pvf2
          R2=cor(predict(m2),Prod)^2
          B1=coefficients(m2)[2]
          B2=coefficients(m2)[3]
          res=abs(R2*(1-p)*(B2/B1))
          Esco="quadratic"
          if((B2>0)&(B1<0)){
            ANOVA=anova(m)
            p=pvf1
            R2=cor(predict(m),Prod)^2
            B1=coefficients(m)[1]
            B2=coefficients(m)[2]

            res=abs(R2*(1-p))
            cat("___________________________________________________  \n")
            print(paste("Chosen: ","linear"))
            Esco="linear"
          }



        }
        if(pv2[length(pv2)]>0.05){
          ANOVA=anova(m)
          p=pvf1
          R2=cor(predict(m),Prod)^2
          B1=coefficients(m)[1]
          B2=coefficients(m)[2]
          res=abs(R2*(1-p))

            cat("___________________________________________________  \n")
            print(paste("Chosen: ","linear"))
            Esco="linear"
        }

      }

    }




    if(is.na(B2)){B2=100}
    if((B2>0)&(positivo==FALSE)){util=FALSE}
    if(util==FALSE){res=0}
    rp=c(rp,res)
    return(Esco)
  }

###########################################################################
###################################
R.Pescolha=function(D,Prod,verbose=FALSE){
  rp=NULL
  # D=matrix(D,nrow=nrow(Prod))
  for(i in 1:ncol(D)){

    B2=100

    xx=unlist(strsplit(colnames(D)[i],"_"))[1]
    positivo=FALSE
    if((xx=="X")|(xx=="X")){positivo=TRUE}



    x=D[,i]
    Prod=as.matrix(Prod)
    m=lm(Prod~x)
    m2=lm(Prod~x+I(x^2))

    ANOVA=anova(m)
    ANOVA2=anova(m2)


    ms1=summary(m)
    ms2=summary(m2)




    pvf1=1-pf(ms1$fstatistic[1],1,nrow(D)-2)
    pvf2=1-pf(ms2$fstatistic[1],2,nrow(D)-3)

    if(nrow(coefficients(ms2))==2){pvf2=1}

    hipo=sum(pvf1<=0.05,pvf2<=0.05)
    util=hipo>0


    if(util==TRUE){
      if(hipo==1){
        if(pvf1<=0.05){
          ANOVA=anova(m)
          p=pvf1
          R2=cor(predict(m),Prod)^2
          B1=coefficients(m)[1]
          B2=coefficients(m)[2]
          res=abs(R2*(1-p))

if(B2>0){
  cat(green("################################################ \n"))
  cat(green(colnames(D)[i],"\n"))
  cat(green("################################################ \n"))

  print(ms1)
  util=readline(prompt = "Is this interaction useful? (y,n) ")
}

        }

        if(pvf2<=0.05){
          ANOVA=anova(m2)
          p=pvf2
          R2=cor(predict(m2),Prod)^2
          B1=coefficients(m2)[2]
          B2=coefficients(m2)[3]
          res=abs(R2*(1-p)*(B2/B1))


        }

      }
      if(hipo==2){

        pv1= ms1$coefficients[,4]
        pv2= ms2$coefficients[,4]
        if(pv2[length(pv2)]<=0.05){
          ANOVA=anova(m2)
          p=pvf2
          R2=cor(predict(m2),Prod)^2
          B1=coefficients(m2)[2]
          B2=coefficients(m2)[3]
          res=abs(R2*(1-p)*(B2/B1))



          if((B2>0)&(B1<0)){
            ANOVA=anova(m)
            p=pvf1
            R2=cor(predict(m),Prod)^2
            B1=coefficients(m)[1]
            B2=coefficients(m)[2]
            if(B2>0){
              cat(green("################################################ \n"))
              cat(green(colnames(D)[i],"\n"))
              cat(green("################################################ \n"))

              print(ms1)
              util=readline(prompt = "Is this interaction useful? (y,n) ")
            }
            res=abs(R2*(1-p))

          }


        }
        if(pv2[length(pv2)]>0.05){
          ANOVA=anova(m)
          p=pvf1
          R2=cor(predict(m),Prod)^2
          B1=coefficients(m)[1]
          B2=coefficients(m)[2]
          res=abs(R2*(1-p))
          if(B2>0){
            cat(green("################################################ \n"))
            cat(green(colnames(D)[i],"\n"))
            cat(green("################################################ \n"))

            print(ms1)
            util=readline(prompt = "Is this interaction useful? (y,n) ")
          }

        }

      }

    }




    if(is.na(B2)){B2=100}



    if(util=="n"){res=0}
    #if((B2>0)&(util=="y")){util=FALSE}
   # if(util=="n"){res=0}

    rp=c(rp,res)


    }

  return(rp)
}


###########################################################################
###################################
# EffectivenessOfSolution2=function(DataLossSource,DataSolutionSource,Production){
#   verbose=FALSE
#   D1=matrix(unlist(DataLossSource),ncol=1)
#   D2=matrix(unlist(DataSolutionSource),ncol=1)
#   Prod=Production
#
#
#   LS=LossSource2(D1,Prod,verbose = F)
#   #id=LS$Res1[,8]>0
#
#   D1b=D1
#
#
#   es=NULL
#   D=D2
#
#       x=D
#       Prod=as.matrix(D1b)
#       m=lm(Prod~x)
#       m2=lm(Prod~x+I(x^2))
#
#       ANOVA=anova(m)
#       ANOVA2=anova(m2)
#
#
#       ms1=summary(m)
#       ms2=summary(m2)
#
#       if(verbose==TRUE){
#       #  cat(green("################################################ \n"))
#       #  cat(green(colnames(D)[i]," VS ",colnames(D)[j],"\n"))
#       #  cat(green("################################################ \n"))
#         print(ms1)
#         cat(green("_______________________________________________ \n"))
#         print(ms2)
#
#
#
#
#
#       }
#
#       pvf1=1-pf(ms1$fstatistic[1],1,nrow(D)-2)
#       pvf2=1-pf(ms2$fstatistic[1],2,nrow(D)-3)
#
#       if(nrow(coefficients(ms2))==2){pvf2=1}
#
#       hipo=sum(pvf1<=0.05,pvf2<=0.05)
#       util=hipo>0
#
#
#       if(util==TRUE){
#         if(hipo==1){
#           if(pvf1<=0.05){
#             ANOVA=anova(m)
#             p=pvf1
#             R2=cor(predict(m),Prod)^2
#             B1=coefficients(m)[1]
#             B2=coefficients(m)[2]
#             res=abs(R2*(1-p))
#             if(B2>0){res=-res}
#             if(verbose==TRUE){
#               cat("___________________________________________________  \n")
#               print(paste("Chosen: ","linear"))
#             }
#           }
#
#           if(pvf2<=0.05){
#             ANOVA=anova(m2)
#             p=pvf2
#             R2=cor(predict(m2),Prod)^2
#             B1=coefficients(m2)[2]
#             B2=coefficients(m2)[3]
#             res=abs(R2*(1-p)*(B2/B1))
#             if(verbose==TRUE){
#               cat("___________________________________________________  \n")
#               print(paste("Chosen: ","quadratic"))
#             }
#           }
#
#         }
#         if(hipo==2){
#           pv1= ms1$coefficients[,4]
#           pv2= ms2$coefficients[,4]
#           if(pv2[length(pv2)]<=0.05){
#             ANOVA=anova(m2)
#             p=pvf2
#             R2=cor(predict(m2),Prod)^2
#             B1=coefficients(m2)[2]
#             B2=coefficients(m2)[3]
#             res=abs(R2*(1-p)*(B2/B1))
#             param=FALSE
#             if((B2>0)&(B1<0)){
#               ANOVA=anova(m)
#               p=pvf1
#               R2=cor(predict(m),Prod)^2
#               B1=coefficients(m)[1]
#               B2=coefficients(m)[2]
#
#               res=abs(R2*(1-p))
#               if(B2>0){res=-res}
#               if(verbose==TRUE){
#                 cat("___________________________________________________  \n")
#                 print(paste("Chosen: ","linear"))
#               }
#               param=TRUE
#             }
#
#             if((verbose==TRUE)&param==FALSE){
#               cat("___________________________________________________  \n")
#               print(paste("Chosen: ","linear"))
#             }
#           }
#           if(pv2[length(pv2)]>0.05){
#             ANOVA=anova(m)
#             p=pvf1
#             R2=cor(predict(m),Prod)^2
#             B1=coefficients(m)[1]
#             B2=coefficients(m)[2]
#             res=abs(R2*(1-p))
#             if(B2>0){res=-res}
#
#             if(verbose==TRUE){
#               cat("___________________________________________________  \n")
#               print(paste("Chosen: ","linear"))
#             }
#
#           }
#
#         }
#
#       }
#
#
#
#
#       if(is.na(B2)){B2=100}
#       #if((B2>0)&(positivo==FALSE)){util=FALSE}
#       if(util==FALSE){res=0}
#
#       res=c(colnames(D2)[i],colnames(D1b)[j],round(res,6))
#
#       es=rbind(es,res)
#
#     }

#######################################################
###################

LossSource2=function(DataLoss,DataProd,verbose=TRUE){
  Prod=DataProd
  D=DataLoss
  n=sum(D)
  RP=R.P2(D,Prod,verbose=FALSE)
  KS=RP/n
  c=colSums(D>0)
  ds=NULL
  chisqq=NULL
  for(i in 1:ncol(D)){
    chisq=suppressWarnings(chisq.test(D[,i]))
    chisqq=c(chisqq,chisq$p.value)
    if(verbose==TRUE){
      cat(green("################################################  \n"))
      cat(colnames(D)[i],"\n")
      cat(green("################################################  \n"))
      print(chisq)
      ds=c(ds,chisq$p.value)
      cat("________________________________________________  \n")

      #  print(paste("Chosen:" ,R.P2(D = D[,i],Prod = Prod,verbose=TRUE)))

    }}

  ds=sapply(1:ncol(D), function (i) 1-suppressWarnings(chisq.test(D[,i])$p.value))
  NII=KS*c*ds
  PII=100*NII/sum(NII)

  if(verbose==TRUE){
    R.P(D,Prod,verbose = T)
  }




  Res1= data.frame(
    n=colSums(D),
    R.P.=R.P(D,Prod,verbose = F),
    ks=RP/n,
    c=colSums(D>0),
    ds=sapply(1:ncol(D), function (i) 1-suppressWarnings(chisq.test(D[,i])$p.value)),
    n.I.I.=KS*c*ds,
    Sum.n.I.I.=sum(KS*c*ds),
    `P.I.I.`=100*NII/sum(NII))


  Var=apply(D,2,var)
  Mean=apply(D,2,mean)
  p.Value=chisqq
  Res2=data.frame(Var=Var,Mean=Mean,p.Value=p.Value)

  return(list(Res1=round(Res1,6),Res2=Res2))
}


