
agex = 61
log(agex)

xyz = 5^exp(2)


setCVDRisk = function(ageVector, totalCholVector, HDLVector, bpsystVector, smokingStatus, diabetesStatus){

f1 = function(ageVector, totalCholVector, HDLVector, bpsystVector, smokingStatus, diabetesStatus){ # African american / treated for hypertension
  2.468*log(ageVector) + 0.302*log(totalCholVector) + (-0.307)*log(HDLCholVector) + 1.916*log(bpsystVector) + 0.549*smokingStatus + 0.645*diabetesStatus
  }

f2 = function(ageVector, totalCholVector, HDLVector, bpsystVector, smokingStatus, diabetesStatus){ # African american / not treated for hypertension
  2.468*log(ageVector) + 0.302*log(totalCholVector) + (-0.307)*log(HDLCholVector) + 1.809*log(bpsystVector) + 0.549*smokingStatus + 0.645*diabetesStatus 
}

f3 = function(ageVector, totalCholVector, HDLVector, bpsystVector, smokingStatus, diabetesStatus){ # White (incl. latinx + others) / treated for hypertension
  12.344*log(ageVector) + 11.853*log(totalCholVector) + (-2.664)*(log(ageVector)*log(totalCholVector)) + (-7.99)*log(HDLCholVector) + 
    1.769*(log(ageVector)*log(HDLCholVector)) + 1.797*log(bpsystVector) + 7.837*smokingStatus + (-1.795)*(log(ageVector)*smokingStatus) + 0.658*diabetesStatus 
}

f4 = function(ageVector, totalCholVector, HDLVector, bpsystVector, smokingStatus, diabetesStatus){ # White (incl. latinx + others / not treated for hypertension
  12.344*log(age) + 11.853*log(totalChol) + (-2.664)*(log(age)*log(totalChol)) + (-7.99)*log(HDLChol) + 
    1.769*(log(age)*log(HDLChol)) + 1.764*log(bpsyst) + 7.837*smoking + (-1.795)*(log(age)*smoking) + 0.658*diabetes 
}

CVDRisk = rep(NA, length(raceVector))
if (length(CVDRisk[racevector==1 & hypertensionTreatStatus==1])>0) {
CVDRisk[racevector==1 & hypertensionTreatStatus==1] = mapply(f1, ageVector[racevector==1 & hypertensionTreatStatus==1], totalCholVector[racevector==1 & hypertensionTreatStatus==1], 
                                                          HDLVector[racevector==1 & hypertensionTreatStatus==1], bpsystVector[racevector==1 & hypertensionTreatStatus==1], 
                                                          smokingStatus[racevector==1 & hypertensionTreatStatus==1], diabetesStatus[racevector==1 & hypertensionTreatStatus==1])
}
if (length(CVDRisk[racevector==1 & hypertensionTreatStatus==0])>0) {
  CVDRisk[racevector==1 & hypertensionTreatStatus==0] = mapply(f2, ageVector[racevector==1 & hypertensionTreatStatus==0], totalCholVector[racevector==1 & hypertensionTreatStatus==0], 
                                                               HDLVector[racevector==1 & hypertensionTreatStatus==0], bpsystVector[racevector==1 & hypertensionTreatStatus==0], 
                                                               smokingStatus[racevector==1 & hypertensionTreatStatus==0], diabetesStatus[racevector==1 & hypertensionTreatStatus==0])
}
if (length(CVDRisk[racevector>1 & hypertensionTreatStatus==1])>0) { # White / treated for hypertension
  CVDRisk[racevector>1 & hypertensionTreatStatus==1] = mapply(f3, ageVector[racevector>1 & hypertensionTreatStatus==1], totalCholVector[racevector>1 & hypertensionTreatStatus==1], 
                                                               HDLVector[racevector>1 & hypertensionTreatStatus==1], bpsystVector[racevector>1 & hypertensionTreatStatus==1], 
                                                               smokingStatus[racevector>1 & hypertensionTreatStatus==1], diabetesStatus[racevector>1 & hypertensionTreatStatus==1])
}
if (length(CVDRisk[racevector>1 & hypertensionTreatStatus==0])>0) { # White / not treated for hypertension
  CVDRisk[racevector>1 & hypertensionTreatStatus==0] = mapply(f4, ageVector[racevector>1 & hypertensionTreatStatus==0], totalCholVector[racevector>1 & hypertensionTreatStatus==0], 
                                                              HDLVector[racevector>1 & hypertensionTreatStatus==0], bpsystVector[racevector>1 & hypertensionTreatStatus==0], 
                                                              smokingStatus[racevector>1 & hypertensionTreatStatus==0], diabetesStatus[racevector>1 & hypertensionTreatStatus==0])
}
}


CVDRisk = setCVDRisk(age, totalChol, HDLChol, bpsyst, smoking, diabetes)



# African american / treated for hypertension
SumBlackTreated = 2.468*log(age) + 0.302*log(totalChol) + (-0.307)*log(HDLChol) + 1.916*log(bpsyst) + 0.549*smoking + 0.645*diabetes 

# African american / not treated for hypertension
SumBlackUntreated = 2.468*log(age) + 0.302*log(totalChol) + (-0.307)*log(HDLChol) + 1.809*log(bpsyst) + 0.549*smoking + 0.645*diabetes 

# White / treated for hypertension
SumWhiteTreated = 12.344*log(age) + 11.853*log(totalChol) + (-2.664)*(log(age)*log(totalChol)) + (-7.99)*log(HDLChol) + 
  1.769*(log(age)*log(HDLChol)) + 1.797*log(bpsyst) + 7.837*smoking + (-1.795)*(log(age)*smoking) + 0.658*diabetes 

# White / not treated for hypertension
SumWhiteUntreated = 12.344*log(age) + 11.853*log(totalChol) + (-2.664)*(log(age)*log(totalChol)) + (-7.99)*log(HDLChol) + 
  1.769*(log(age)*log(HDLChol)) + 1.764*log(bpsyst) + 7.837*smoking + (-1.795)*(log(age)*smoking) + 0.658*diabetes 

  
  
  totalChol
HDLChol
bpsyst 
hypertensionTreat

cvdRiskParamBlack = -29.18
SumBlackTreated = -28.95

cvdRiskParamWhite

Survival10yrBlack = 0.9665

Survival10yrWhite



cvdRiskBlackTreated = 1 - Survival10yrBlack^exp(SumBlackTreated-cvdRiskParamBlack)
cvdRiskBlackUntreated = 1 - Survival10yrBlack^exp(SumBlackTreated-cvdRiskParamBlack)

cvdRiskWhiteTreated = 1 - Survival10yrWhitek^exp(SumWhiteTreated-cvdRiskParamWhite)
cvdRiskWhiteUntreated = 1 - Survival10yrWhite^exp(SumWhiteTreated-cvdRiskParamWhite)




#---------------------- Test replace in vector

check = rbinom(n=20, size=1, prob=0.5)
value = rep(5,20)
add = rep(7,20)

xxx=data.frame(check,value)

xxx$value[check==1] = xxx$value+5
xxx$value[check==1] = xxx$value[check==1]+5

aux3 = xxx[xxx$check==1,]
add2 = rep(7,dim(aux3)[1])
xxx$value[check==1] = xxx$value[check==1]+add2

#--------------------- Test: working with subsets of dataframes

n=20
check = rbinom(n=20, size=1, prob=0.5)
value = rep(5,20)
ages = sample (1:10, n, replace = TRUE)
xyz = data.frame(check, value, ages) 
  
# Subset of xyz dataframe
xy = xyz[xyz$check==1,]

groups = rep(1, dim(xy)[1]) # THIS ONE IS THE CORRECT VERSION!
groups[xy$ages>5] = 2
groups[xy$ages>8] = 3

groups2 = rep(1, dim(xy)[1])
groups2[xyz$ages>5] = 2
groups2[xyz$ages>8] = 3

xyz$value[xyz$check==1] = xyz$value[xyz$check==1] + groups # OK this works

xx = xyz[xyz$check==2,]

test = rep(NA, dim(xyz)[1])

value2= "3"
switch(value2,
       "1" = {test = rep(0, dim(xyz)[1])},
       "2" = {test = 2},
       "3" = {test = 3+xyz$value}
       )


xyz$value[xyz$check==1] = xyz$value[xyz$check==1] + 2




