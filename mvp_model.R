###### Script created to import, transform and analyze brazilian stock market prices ########
###### Optimization through the method of Minimum Portfolio Variance ########
###### MaTheus Dias Ignacio S., Economist and Data Scientist ######

#### Load required packages ######

library(fPortfolio)



###############################################################################
############# Import data #####################################################

setwd() ### Set the working directory where the files will be dowloaded

Quotes <- read.csv("cot.csv", sep=";", dec=".", header=TRUE)

ret <- 100*log(Quotes[2:nrow(Quotes),2:26]/Quotes[1:(nrow(Quotes)-1),2:26])


r <- as.timeSeries(ret)


#### CDI #################################### 

CDI <- (1+Quotes[,28]/100)^(1/252)

Retcdi1  <-  prod(CDI[494:513])-1
Retcdi2  <-  prod(CDI[513:532])-1
Retcdi3  <-  prod(CDI[532:554])-1 
Retcdi4  <-  prod(CDI[554:574])-1
Retcdi5  <-  prod(CDI[574:595])-1
Retcdi6  <-  prod(CDI[595:617])-1
Retcdi7  <-  prod(CDI[617:638])-1
Retcdi8  <-  prod(CDI[638:661])-1
Retcdi9  <-  prod(CDI[661:682])-1
Retcdi10 <-  prod(CDI[682:702])-1
Retcdi11 <-  prod(CDI[702:722])-1
Retcdi12 <-  prod(CDI[722:743])-1

Retcdi13 <-  prod(CDI[743:764])-1
Retcdi14 <-  prod(CDI[764:782])-1
Retcdi15 <-  prod(CDI[782:805])-1
Retcdi16 <-  prod(CDI[805:823])-1
Retcdi17 <-  prod(CDI[823:845])-1
Retcdi18 <-  prod(CDI[845:866])-1
Retcdi19 <-  prod(CDI[866:887])-1
Retcdi20 <-  prod(CDI[887:910])-1
Retcdi21 <-  prod(CDI[910:930])-1
Retcdi22 <-  prod(CDI[930:951])-1
Retcdi23 <-  prod(CDI[951:970])-1
Retcdi24 <-  prod(CDI[970:989])-1

Retcdi2016 <- c(Retcdi1, Retcdi2, Retcdi3, Retcdi4, Retcdi5, Retcdi6, Retcdi7, Retcdi8, Retcdi9, Retcdi10, Retcdi11, Retcdi12)
Retcdi2017 <- c(Retcdi13, Retcdi14, Retcdi15, Retcdi16, Retcdi17, Retcdi18, Retcdi19, Retcdi20, Retcdi21, Retcdi22, Retcdi23, Retcdi24)

RetAcumCDI2016 <- (1+Retcdi1)*(1+Retcdi2)*(1+Retcdi3)*(1+Retcdi4)*(1+Retcdi5)*(1+Retcdi6)*(1+Retcdi7)*(1+Retcdi8)*(1+Retcdi9)*(1+Retcdi10)*(1+Retcdi11)*(1+Retcdi12)

RetAcumCDI2017 <- (1+Retcdi13)*(1+Retcdi14)*(1+Retcdi15)*(1+Retcdi16)*(1+Retcdi17)*(1+Retcdi18)*(1+Retcdi19)*(1+Retcdi20)*(1+Retcdi21)*(1+Retcdi22)*(1+Retcdi23)*(1+Retcdi24)

MeanRetcdi2017<-mean(Retcdi2017)
MeanRetcdi2017
###############################################################################

##### Minimum Portfolio Variance ###############################################

globminSpec <- portfolioSpec()

globminPortfolio1 <- minvariancePortfolio(data=r[493:742,], spec=globminSpec , constraints = "LongOnly")
Wmvp1 <- getWeights(globminPortfolio1)

globminPortfolio2 <- minvariancePortfolio(data=r[512:763,], spec=globminSpec , constraints = "LongOnly")
Wmvp2 <- getWeights(globminPortfolio2)

globminPortfolio3 <- minvariancePortfolio(data=r[531:781,], spec=globminSpec , constraints = "LongOnly")
Wmvp3 <- getWeights(globminPortfolio3)

globminPortfolio4 <- minvariancePortfolio(data=r[553:804,], spec=globminSpec , constraints = "LongOnly")
Wmvp4 <- getWeights(globminPortfolio4)

globminPortfolio5 <- minvariancePortfolio(data=r[573:822,], spec=globminSpec , constraints = "LongOnly")
Wmvp5 <- getWeights(globminPortfolio5)

globminPortfolio6 <- minvariancePortfolio(data=r[594:844,], spec=globminSpec , constraints = "LongOnly")
Wmvp6 <- getWeights(globminPortfolio6)

globminPortfolio7 <- minvariancePortfolio(data=r[616:865,], spec=globminSpec , constraints = "LongOnly")
Wmvp7 <- getWeights(globminPortfolio7)

globminPortfolio8 <- minvariancePortfolio(data=r[637:886,], spec=globminSpec , constraints = "LongOnly")
Wmvp8 <- getWeights(globminPortfolio8)

globminPortfolio9 <- minvariancePortfolio(data=r[660:909,], spec=globminSpec , constraints = "LongOnly")
Wmvp9 <- getWeights(globminPortfolio9)

globminPortfolio10 <- minvariancePortfolio(data=r[681:929,], spec=globminSpec , constraints = "LongOnly") 
Wmvp10 <- getWeights(globminPortfolio10)                                                                   

globminPortfolio11 <- minvariancePortfolio(data=r[701:950,], spec=globminSpec , constraints = "LongOnly")  
Wmvp11 <- getWeights(globminPortfolio11)                                                                   
                 
globminPortfolio12 <- minvariancePortfolio(data=r[721:969,], spec=globminSpec , constraints = "LongOnly") 
Wmvp12 <- getWeights(globminPortfolio12)                                                                   
 
W <- rbind(c(Wmvp1),c(Wmvp2),c(Wmvp3),c(Wmvp4),c(Wmvp5),c(Wmvp6),c(Wmvp7),c(Wmvp8),c(Wmvp9),c(Wmvp10),c(Wmvp11),c(Wmvp12))


####

RetMvp1 <- Wmvp1%*%t((Quotes[764,2:26]-Quotes[743,2:26])/Quotes[743,2:26])
RetMvp2 <- Wmvp2%*%t((Quotes[782,2:26]-Quotes[764,2:26])/Quotes[764,2:26])
RetMvp3 <- Wmvp3%*%t((Quotes[805,2:26]-Quotes[782,2:26])/Quotes[782,2:26])
RetMvp4 <- Wmvp4%*%t((Quotes[823,2:26]-Quotes[805,2:26])/Quotes[805,2:26])
RetMvp5 <- Wmvp5%*%t((Quotes[845,2:26]-Quotes[823,2:26])/Quotes[823,2:26])
RetMvp6 <- Wmvp6%*%t((Quotes[866,2:26]-Quotes[845,2:26])/Quotes[845,2:26])
RetMvp7 <- Wmvp7%*%t((Quotes[887,2:26]-Quotes[866,2:26])/Quotes[866,2:26])
RetMvp8 <- Wmvp8%*%t((Quotes[910,2:26]-Quotes[887,2:26])/Quotes[887,2:26])
RetMvp9 <- Wmvp9%*%t((Quotes[930,2:26]-Quotes[910,2:26])/Quotes[910,2:26])
RetMvp10 <- Wmvp10%*%t((Quotes[951,2:26]-Quotes[930,2:26])/Quotes[930,2:26])
RetMvp11 <- Wmvp11%*%t((Quotes[970,2:26]-Quotes[951,2:26])/Quotes[951,2:26])
RetMvp12 <- Wmvp12%*%t((Quotes[989,2:26]-Quotes[970,2:26])/Quotes[970,2:26])

   

###Monthly Return
RetMensalMvp<-rbind(RetMvp1,RetMvp2, RetMvp3, RetMvp4, RetMvp5, RetMvp6, RetMvp7, RetMvp8, RetMvp9, RetMvp10, RetMvp11, RetMvp12)

#Cumulated Return
RetMvpacum<-c((1+RetMvp1)*(1+RetMvp2)*(1+RetMvp3)*(1+RetMvp4)*(1+RetMvp5)*(1+RetMvp6)*(1+RetMvp7)*(1+RetMvp8)*(1+RetMvp9)*(1+RetMvp10)*(1+RetMvp11)*(1+RetMvp12))

### Average of Monthly Return###
MeanRetMensalMvp<-mean(RetMensalMvp)
MeanRetMensalMvp
RetMensalMvp
RetMvpacum

##Monthly Variance##
SigmaMvp<- sqrt(var(RetMensalMvp))
SigmaMvp
### Sharpe Index ###

SharperatioMvp<- (MeanRetMensalMvp - MeanRetcdi2017)/SigmaMvp
SharperatioMvp


###############################################################################




# Function Calculate Expected Return and Varcov Multifactorial Model
#
#
################################################################################

VarcovFactor <- function(R,spec=NULL){
  
  R <- as.matrix(R)
  
  SigmaF <- cov(Factors)
  
  fit <- lm(R ~ Factors )
  
  
  list(mu = matrix(colMeans(R)),
       Sigma = t(fit$coeff[2:4,])%*%SigmaF%*%fit$coeff[2:4,] + 
         diag( diag( cov(fit$resid ) ), ncol(fit$resid ), ncol(fit$resid ))
  )
  
}

RM <- 100*log(Quotes[2:nrow(Quotes),27]/Quotes[1:(nrow(Quotes)-1),27])


Rm <- RM
VarcovOneFactor(r)



globminSpecFactor <- portfolioSpec()

setEstimator(globminSpecFactor) <- "VarcovOneFactor"



Rm <- RM[493:742]
globminPortfoliof1 <- minvariancePortfolio(data = r[493:742,], spec = globminSpecFactor , constraints= "LongOnly")
Wmvpf1 <- getWeights(globminPortfoliof1)

Rm <- RM[512:763]
globminPortfoliof2 <- minvariancePortfolio(data = r[512:763,], spec = globminSpecFactor , constraints= "LongOnly")
Wmvpf2 <- getWeights(globminPortfoliof2)


Rm <- RM[531:781]
globminPortfoliof3 <- minvariancePortfolio(data = r[531:781,], spec = globminSpecFactor , constraints= "LongOnly")
Wmvpf3 <- getWeights(globminPortfoliof3)

Rm <- RM[553:804]
globminPortfoliof4 <- minvariancePortfolio(data = r[553:804,], spec = globminSpecFactor , constraints= "LongOnly")
Wmvpf4 <- getWeights(globminPortfoliof4)

Rm <- RM[573:822]
globminPortfoliof5 <- minvariancePortfolio(data = r[573:822,], spec = globminSpecFactor , constraints= "LongOnly")
Wmvpf5 <- getWeights(globminPortfoliof5)

Rm <- RM[594:844]
globminPortfoliof6 <- minvariancePortfolio(data = r[594:844,], spec = globminSpecFactor , constraints= "LongOnly")
Wmvpf6 <- getWeights(globminPortfoliof6)


Rm <- RM[616:865]
globminPortfoliof7 <- minvariancePortfolio(data = r[616:865,], spec = globminSpecFactor , constraints= "LongOnly")
Wmvpf7 <- getWeights(globminPortfoliof7)


Rm <- RM[637:886]
globminPortfoliof8 <- minvariancePortfolio(data = r[637:886,], spec = globminSpecFactor , constraints= "LongOnly")
Wmvpf8 <- getWeights(globminPortfoliof8)


Rm <- RM[660:909]
globminPortfoliof9 <- minvariancePortfolio(data = r[660:909,], spec = globminSpecFactor , constraints= "LongOnly")
Wmvpf9 <- getWeights(globminPortfoliof9)


Rm <- RM[681:929]
globminPortfoliof10 <- minvariancePortfolio(data = r[681:929,], spec = globminSpecFactor , constraints= "LongOnly")
Wmvpf10 <- getWeights(globminPortfoliof10)


Rm <- RM[701:950]
globminPortfoliof11 <- minvariancePortfolio(data = r[701:950,], spec = globminSpecFactor , constraints= "LongOnly")  
Wmvpf11 <- getWeights(globminPortfoliof11)


Rm <- RM[721:969]                                                                                                  
globminPortfoliof12 <- minvariancePortfolio(data = r[721:969,], spec = globminSpecFactor , constraints= "LongOnly") 
Wmvpf12 <- getWeights(globminPortfoliof12)                                                                         



Wf <- rbind(c(Wmvpf1),c(Wmvpf2),c(Wmvpf3),c(Wmvpf4),c(Wmvpf5),c(Wmvpf6),c(Wmvpf7),c(Wmvpf8),c(Wmvpf9),c(Wmvpf10),c(Wmvpf11),c(Wmvpf12))



####

RetMvpf1 <- Wmvpf1%*%t((Quotes[764,2:26]-Quotes[743,2:26])/Quotes[743,2:26])
RetMvpf2 <- Wmvpf2%*%t((Quotes[782,2:26]-Quotes[764,2:26])/Quotes[764,2:26])
RetMvpf3 <- Wmvpf3%*%t((Quotes[805,2:26]-Quotes[782,2:26])/Quotes[782,2:26])
RetMvpf4 <- Wmvpf4%*%t((Quotes[823,2:26]-Quotes[805,2:26])/Quotes[805,2:26])
RetMvpf5 <- Wmvpf5%*%t((Quotes[845,2:26]-Quotes[823,2:26])/Quotes[823,2:26])
RetMvpf6 <- Wmvpf6%*%t((Quotes[866,2:26]-Quotes[845,2:26])/Quotes[845,2:26])
RetMvpf7 <- Wmvpf7%*%t((Quotes[887,2:26]-Quotes[866,2:26])/Quotes[866,2:26])
RetMvpf8 <- Wmvpf8%*%t((Quotes[910,2:26]-Quotes[887,2:26])/Quotes[887,2:26])
RetMvpf9 <- Wmvpf9%*%t((Quotes[930,2:26]-Quotes[910,2:26])/Quotes[910,2:26])
RetMvpf10 <- Wmvpf10%*%t((Quotes[951,2:26]-Quotes[930,2:26])/Quotes[930,2:26])
RetMvpf11 <- Wmvpf11%*%t((Quotes[970,2:26]-Quotes[951,2:26])/Quotes[951,2:26])
RetMvpf12 <- Wmvpf12%*%t((Quotes[989,2:26]-Quotes[970,2:26])/Quotes[970,2:26])

Wf


###Rendimento Mensal Unifatorial
RetMensalf<-c(RetMvpf1,RetMvpf2, RetMvpf3, RetMvpf4, RetMvpf5, RetMvpf6, RetMvpf7, RetMvpf8, RetMvpf9, RetMvpf10, RetMvpf11, RetMvpf12)

####Rendimento Acumulado Unifatorial
RetMvpacumf<-c((1+RetMvpf1)*(1+RetMvpf2)*(1+RetMvpf3)*(1+RetMvpf4)*(1+RetMvpf5)*(1+RetMvpf6)*(1+RetMvpf7)*(1+RetMvpf8)*(1+RetMvpf9)*(1+RetMvpf10)*(1+RetMvpf11)*(1+RetMvpf12))

###Retorno M?dio Unifatorial###

MeanRetMensalf<-mean(RetMensalf)

###Desvio-padr?o mensal unifatorial###
Sigmaf<-sqrt(var(RetMensalf))


####?ndice de Sharpe do Modelo unifatorial####
Sharperatiof<- (MeanRetMensalf - MeanRetcdi2017)/Sigmaf



MeanRetMensalf
RetMensalf
RetMvpacumf
Sigmaf
Sharperatiof






#(3)# Portfolio MVP para Multifatorial.

source("FunctionFactor.txt")

Rm <- 100*log(Quotes[2:nrow(Quotes),27]/Quotes[1:(nrow(Quotes)-1),27])
smb <- as.matrix(Quotes$SMB[-1]) # Tem que retirar a primeira linha para ficar com o mesmo nzmero de linhas de Rm. Como extrai os fatores prontos
hml <- as.matrix(Quotes$HML[-1]) # tem o valor do fator na primeira observagco. Para Rm nco tem, perde a primeira observagco ao calcular retorno.


mF <- cbind(Rm,smb,hml)

Factors <- mF
VarcovFactor(r)



globminSpecFactors <- portfolioSpec() # Declare object fPortfolio 
setEstimator(globminSpecFactors) <- "VarcovFactor" 


### Factors calc
Factors <- mF[493:742,]
globminPortfolioMF1 <- minvariancePortfolio(data = r[493:742,], spec = globminSpecFactors , constraints= "LongOnly")
WmvpMF1 <- getWeights(globminPortfolioMF1)

Factors <- mF[512:763,]
globminPortfolioMF2 <- minvariancePortfolio(data = r[512:763,], spec = globminSpecFactors , constraints= "LongOnly")
WmvpMF2 <- getWeights(globminPortfolioMF2)

Factors <- mF[531:781,]
globminPortfolioMF3 <- minvariancePortfolio(data = r[531:781,], spec = globminSpecFactors , constraints= "LongOnly")
WmvpMF3 <- getWeights(globminPortfolioMF3)

Factors <- mF[553:804,]
globminPortfolioMF4 <- minvariancePortfolio(data = r[553:804,], spec = globminSpecFactors , constraints= "LongOnly")
WmvpMF4 <- getWeights(globminPortfolioMF4)

Factors <- mF[573:822,]
globminPortfolioMF5 <- minvariancePortfolio(data = r[573:822,], spec = globminSpecFactors , constraints= "LongOnly")
WmvpMF5 <- getWeights(globminPortfolioMF5)

Factors <- mF[594:844,]
globminPortfolioMF6 <- minvariancePortfolio(data = r[594:844,], spec = globminSpecFactors , constraints= "LongOnly")
WmvpMF6 <- getWeights(globminPortfolioMF6)

Factors <- mF[616:865,]
globminPortfolioMF7 <- minvariancePortfolio(data = r[616:865,], spec = globminSpecFactors , constraints= "LongOnly")
WmvpMF7 <- getWeights(globminPortfolioMF7)

Factors <- mF[637:886,]
globminPortfolioMF8 <- minvariancePortfolio(data = r[637:886,], spec = globminSpecFactors , constraints= "LongOnly")
WmvpMF8 <- getWeights(globminPortfolioMF8)

Factors <- mF[660:909,]
globminPortfolioMF9 <- minvariancePortfolio(data = r[660:909,], spec = globminSpecFactors, constraints= "LongOnly")
WmvpMF9 <- getWeights(globminPortfolioMF9)

Factors <- mF[681:929,]
globminPortfolioMF10 <- minvariancePortfolio(data = r[681:929,], spec = globminSpecFactors , constraints= "LongOnly")
WmvpMF10 <- getWeights(globminPortfolioMF10)

Factors <- mF[701:950,]
globminPortfolioMF11 <- minvariancePortfolio(data = r[701:950,], spec = globminSpecFactors , constraints= "LongOnly")   #>>> Mesmo erro que do modelo unifatorial 
WmvpMF11 <- getWeights(globminPortfolioMF11)

Factors <- mF[721:969,]
globminPortfolioMF12 <- minvariancePortfolio(data = r[721:969,], spec = globminSpecFactors , constraints= "LongOnly")
WmvpMF12 <- getWeights(globminPortfolioMF12)


WMF<-rbind(c(WmvpMF1),c(WmvpMF2),c(WmvpMF3),c(WmvpMF4),c(WmvpMF5),c(WmvpMF6),c(WmvpMF7),c(WmvpMF8),c(WmvpMF9),c(WmvpMF10),c(WmvpMF11),c(WmvpMF12))
WMF


### Monthly Returns ####
RetMvpMF1 <- WmvpMF1%*%t((Quotes[764,2:26]-Quotes[743,2:26])/Quotes[743,2:26])
RetMvpMF2 <- WmvpMF2%*%t((Quotes[782,2:26]-Quotes[764,2:26])/Quotes[764,2:26])
RetMvpMF3 <- WmvpMF3%*%t((Quotes[805,2:26]-Quotes[782,2:26])/Quotes[782,2:26])
RetMvpMF4 <- WmvpMF4%*%t((Quotes[823,2:26]-Quotes[805,2:26])/Quotes[805,2:26])
RetMvpMF5 <- WmvpMF5%*%t((Quotes[845,2:26]-Quotes[823,2:26])/Quotes[823,2:26])
RetMvpMF6 <- WmvpMF6%*%t((Quotes[866,2:26]-Quotes[845,2:26])/Quotes[845,2:26])
RetMvpMF7 <- WmvpMF7%*%t((Quotes[887,2:26]-Quotes[866,2:26])/Quotes[866,2:26])
RetMvpMF8 <- WmvpMF8%*%t((Quotes[910,2:26]-Quotes[887,2:26])/Quotes[887,2:26])
RetMvpMF9 <- WmvpMF9%*%t((Quotes[930,2:26]-Quotes[910,2:26])/Quotes[910,2:26])
RetMvpMF10 <- WmvpMF10%*%t((Quotes[951,2:26]-Quotes[930,2:26])/Quotes[930,2:26])
RetMvpMF11 <- WmvpMF11%*%t((Quotes[970,2:26]-Quotes[951,2:26])/Quotes[951,2:26])
RetMvpMF12 <- WmvpMF12%*%t((Quotes[989,2:26]-Quotes[970,2:26])/Quotes[970,2:26])


### Rendimento Mensal Multifatorial ####
RetMensalMF<-rbind(RetMvpMF1,RetMvpMF2, RetMvpMF3, RetMvpMF4, RetMvpMF5, RetMvpMF6, RetMvpMF7, RetMvpMF8, RetMvpMF9, RetMvpMF10, RetMvpMF11, RetMvpMF12)

#### Rendimento Acumulado MultiFatorial ####
RetMvpacumF<-c((1+RetMvpMF1)*(1+RetMvpMF2)*(1+RetMvpMF3)*(1+RetMvpMF4)*(1+RetMvpMF5)*(1+RetMvpMF6)*(1+RetMvpMF7)*(1+RetMvpMF8)*(1+RetMvpMF9)*(1+RetMvpMF10)*(1+RetMvpMF11)*(1+RetMvpMF12))

MeanRetMensalMF<-mean(RetMensalMF)

### Desvio-Padr?o Retorno Mensal Multifatorial ###
SigmaMF<-sqrt(var(RetMensalMF))

#####?ndice de Sharpe###
SharperatioMF<- (MeanRetMensalMF - MeanRetcdi2017)/SigmaMF

RetMensalMF
RetMvpacumF
MeanRetMensalMF
SigmaMF
SharperatioMF

###Retornos do Ibovespa####

RetIbov1<- (Quotes[764,27]-Quotes[743,27])/(Quotes[743,27])
RetIbov2<- (Quotes[782,27]-Quotes[764,27])/(Quotes[764,27])
RetIbov3<- (Quotes[805,27]-Quotes[782,27])/(Quotes[782,27])
RetIbov4<- (Quotes[823,27]-Quotes[805,27])/(Quotes[805,27])
RetIbov5<- (Quotes[845,27]-Quotes[823,27])/(Quotes[823,27])
RetIbov6<- (Quotes[866,27]-Quotes[845,27])/(Quotes[845,27])
RetIbov7<- (Quotes[887,27]-Quotes[866,27])/(Quotes[866,27])
RetIbov8<- (Quotes[910,27]-Quotes[887,27])/(Quotes[887,27])
RetIbov9<- (Quotes[930,27]-Quotes[910,27])/(Quotes[910,27])
RetIbov10<-(Quotes[951,27]-Quotes[930,27])/(Quotes[930,27])
RetIbov11<-(Quotes[970,27]-Quotes[951,27])/(Quotes[951,27])
RetIbov12<-(Quotes[989,27]-Quotes[970,27])/(Quotes[970,27])


###Rendimento Mensal Ibovespa
RetMensalIbov<-c(RetIbov1,RetIbov2, RetIbov3, RetIbov4, RetIbov5, RetIbov6, RetIbov7, RetIbov8, RetIbov9, RetIbov10, RetIbov11, RetIbov12)

####Rendimento Acumulado Ibovespa
RetacumIbov<-c((1+RetIbov1)*(1+RetIbov2)*(1+RetIbov3)*(1+RetIbov4)*(1+RetIbov5)*(1+RetIbov6)*(1+RetIbov7)*(1+RetIbov8)*(1+RetIbov9)*(1+RetIbov10)*(1+RetIbov11)*(1+RetIbov12))

MeanRetMensalIbov<-mean(RetMensalIbov)
SigmaIbov<-sqrt(var(RetMensalIbov))

RetMensalIbov
RetacumIbov
MeanRetMensalIbov
SigmaIbov









