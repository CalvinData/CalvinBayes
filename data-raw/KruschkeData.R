
library(readr)
library(devtools)
# foo <-
#   function(x) {
#     name <- gsub(".csv", "", x)
#     name <- gsub("Data", "", name)
#     invisible(
#       cat(
#         paste(
#           name, ' <- read_csv("', x , '")\n',
#           'use_data(', name , ', overwrite = TRUE)\n\n',
#           collapse = "", sep = "")
#       )
#     )
#   }
#
# lapply(dir(), foo)

AnovaShrinkage <- read_csv("AnovaShrinkageData.csv")
use_data(AnovaShrinkage, overwrite = TRUE)

BattingAverage <- read_csv("BattingAverage.csv")
use_data(BattingAverage, overwrite = TRUE)

BugsRats <- read_csv("BugsRatsData.csv")
use_data(BugsRats, overwrite = TRUE)

CondLogistReg1 <- read_csv("CondLogistRegData1.csv")
use_data(CondLogistReg1, overwrite = TRUE)

CondLogistReg2 <- read_csv("CondLogistRegData2.csv")
use_data(CondLogistReg2, overwrite = TRUE)

CrimeDrink <- read_csv("CrimeDrink.csv")
use_data(CrimeDrink, overwrite = TRUE)

FourByFourCount <- read_csv("FourByFourCount.csv")
use_data(FourByFourCount, overwrite = TRUE)

FruitflyReduced <- read_csv("FruitflyDataReduced.csv")
use_data(FruitflyReduced, overwrite = TRUE)

Guber1999<- read_csv("Guber1999data.csv")
use_data(Guber1999, overwrite = TRUE)

HairEyeColor <- read_csv("HairEyeColor.csv")
use_data(HairEyeColor, overwrite = TRUE)

HappinessAssetsDebt <- read_csv("HappinessAssetsDebt.csv")
use_data(HappinessAssetsDebt, overwrite = TRUE)

HGN <- read_csv("HGN.csv")
use_data(HGN, overwrite = TRUE)

HierLinRegress <- read_csv("HierLinRegressData.csv")
use_data(HierLinRegress, overwrite = TRUE)

HtWt110 <- read_csv("HtWtData110.csv")
use_data(HtWt110, overwrite = TRUE)

HtWt30 <- read_csv("HtWtData30.csv")
use_data(HtWt30, overwrite = TRUE)

HtWt300 <- read_csv("HtWtData300.csv")
use_data(HtWt300, overwrite = TRUE)

IncomeFamilySize <- read_csv("IncomeFamszState.csv")
use_data(IncomeFamilySize, overwrite = TRUE)

IncomeFamilySize <- read_csv("IncomeFamszState3yr.csv")
use_data(IncomeFamilySize, overwrite = TRUE)

ItemResponseTheory <- read_csv("ItemResponseTheoryData.csv")
use_data(ItemResponseTheory, overwrite = TRUE)

MetaAnalysisBetaBlocker <- read_csv("MetaAnalysisBetaBlocker.csv")
use_data(MetaAnalysisBetaBlocker, overwrite = TRUE)

Movies <- read_csv("Movies.csv")
use_data(Movies, overwrite = TRUE)

MultLinRegrPlotCorr <- read_csv("MultLinRegrPlotCorr.csv")
use_data(MultLinRegrPlotCorr, overwrite = TRUE)

MultLinRegrPlotUnif <- read_csv("MultLinRegrPlotUnif.csv")
use_data(MultLinRegrPlotUnif, overwrite = TRUE)

NonhomogVar <- read_csv("NonhomogVarData.csv")
use_data(NonhomogVar, overwrite = TRUE)

OrdinalProbit1grp1 <- read_csv("OrdinalProbitData-1grp-1.csv")
use_data(OrdinalProbit1grp1, overwrite = TRUE)

OrdinalProbit1grp2 <- read_csv("OrdinalProbitData-1grp-2.csv")
use_data(OrdinalProbit1grp2, overwrite = TRUE)

OrdinalProbitLinReg2 <- read_csv("OrdinalProbitData-LinReg-2.csv")
use_data(OrdinalProbitLinReg2, overwrite = TRUE)

OrdinalProbitMovies <- read_csv("OrdinalProbitData-Movies.csv")
use_data(OrdinalProbitMovies, overwrite = TRUE)

OrdinalProbit1 <- read_csv("OrdinalProbitData1.csv")
use_data(OrdinalProbit1, overwrite = TRUE)

RatLives <- read_csv("RatLives.csv")
use_data(RatLives, overwrite = TRUE)

Salary <- read_csv("Salary.csv")
use_data(Salary, overwrite = TRUE)

Seaweed <- read_csv("SeaweedData.csv")
use_data(Seaweed, overwrite = TRUE)

ShohatOphirKAMH2012dataReduced <- read_csv("ShohatOphirKAMH2012dataReduced.csv")
use_data(ShohatOphirKAMH2012dataReduced, overwrite = TRUE)

SoftmaxReg1 <- read_csv("SoftmaxRegData1.csv")
use_data(SoftmaxReg1, overwrite = TRUE)

SoftmaxReg2 <- read_csv("SoftmaxRegData2.csv")
use_data(SoftmaxReg2, overwrite = TRUE)

SplitPlotAgri <- read_csv("SplitPlotAgriData.csv")
use_data(SplitPlotAgri, overwrite = TRUE)

StormTressoldiDiRisio2010data <- read_csv("StormTressoldiDiRisio2010data.csv")
use_data(StormTressoldiDiRisio2010data, overwrite = TRUE)

TherapeuticTouch <- read_csv("TherapeuticTouchData.csv")
use_data(TherapeuticTouch, overwrite = TRUE)

TwoGroupIQ <- read_csv("TwoGroupIQ.csv")
use_data(TwoGroupIQ, overwrite = TRUE)

z15N50 <- read_csv("z15N50.csv")
use_data(z15N50, overwrite = TRUE)

z6N8z2N7 <- read_csv("z6N8z2N7.csv")
use_data(z6N8z2N7, overwrite = TRUE)

