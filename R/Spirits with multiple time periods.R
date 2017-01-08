######## I. Set up the correct working directory and load the "xlsx" package #############

## set working directory, this can also be done by clicking: Session -> Set working directory -> Choose Directory
setwd("C:/HJH/Spirit")
### Install package xlsx, if needed
#install.packages('xlsx')



### load package "xlsx"

library('xlsx')
library('foreign')



###### II. Read in all the data files into R  ########



########### read in the raw data
##comma is replaced with semicolon
##'#NULL!' is treated as NA
#Raw = read.csv('spiritsQ2-B.csv', header = TRUE, sep = ',') ##disable quoting
Raw = read.spss('spirits_ex_timepts.sav', to.data.frame = TRUE)

save(Raw, file = 'Raw1.RData')  ## you only have to run this line for once
load('Raw1.RData')  ##once 'Raw.RData' is saved, just load it instead of reading from the .csv file (faster)

### split raw data by time, and remove it to clean up memory space
Raw1 = Raw[Raw$time==1,]
Raw2 = Raw[Raw$time==2,]
rm(Raw)

####questionnaire
E5b = read.xlsx('questionnaireB.xlsx', 1, header = FALSE)
B4 = read.xlsx('questionnaireB.xlsx', 2, header = FALSE)
B5 = read.xlsx('questionnaireB.xlsx', 3, header = FALSE)
B6 = read.xlsx('questionnaireB.xlsx', 4, header = FALSE)
Brand_List = read.xlsx('brand_list.xlsx', 1, header = TRUE, stringsAsFactors = FALSE)

##########read in the template
tmplt = read.xlsx2('templateB.xlsx', 1, header = TRUE, stringsAsFactors = FALSE)


###### III. Build up a function that can fill data into the template ######

###### functions
#### We design a few functions here to help us fill in the template
#### run this part, and R will build up those functions so you can call them in the future
spirit = function(brand, code, bvgtype, tmplt, Raw, names.Raw, measure)
  ## bvgtype: beverage type
  ## tmplt: template
  ## measure: tmplt$Measures
{
  ##benchmark
  vbcode = names.Raw[grep('E7aBrandUse_r.*[0-9]$', names.Raw, perl = TRUE)]
  if(sum(vbcode %in% names.Raw)==0)
  {}
  else
  {
    TMP = sapply(vbcode, function(x) {tmp = table(factor(Raw[,names.Raw==x], levels = 1:7));
    sum(7:1 * tmp)/sum(tmp)})
    Weights = sapply(vbcode, function(x) {sum(table(Raw[,names.Raw==x]))})
    
    benchmark1 = round(weighted.mean(TMP, Weights), digits = 1)  ##Brand Loyalty among brands consumed mean (benchmark)
    
    ####Awareness - Unaided
    crrt_row = which(measure == 'Awareness - Unaided')  ##current row
    tmplt[crrt_row, 5] = nrow(Raw)  ##total_count
    tmplt[crrt_row, 4] = length(grep(brand, Raw$E1aOverallUATOM, perl = TRUE, ignore.case = TRUE))
    tmplt[crrt_row, 3] = pc_mn(tmplt[crrt_row, 4], tmplt[crrt_row, 5])
    tmplt[crrt_row, 2] = bsln_q1(tmplt[crrt_row, 3])
  }
  
  
  ####Awareness - Unaided by beverage type
  crrt_row = which(measure == 'Awareness - Unaided by beverage type')  ##current row
  vbcode = paste0('E1bUATOM_r', switch(bvgtype, 'Bourbon' = 3, 'Scotch' = 3, 'Whiskey' = 3, 'Brandy' = 4, 'Cognac' = 5,
                                       'Cordials' = 8, 'Liqueur' = 8, 'Gin' = 9, 'Rum' = 14, 
                                       'Tequila' = 15, 'Vodka' = 16),
                  '_c1')
  if(sum(vbcode %in% names.Raw)==0)
  {}
  else
  {
    tmplt[crrt_row, 5] = sum(grepl('[A-z]', Raw$E1bUATOM_r3_c1,  perl = TRUE))
    tmplt[crrt_row, 4] = length(grep(brand, Raw[,which(names.Raw==vbcode)], perl = TRUE, ignore.case = TRUE)) ##??
    tmplt[crrt_row, 3] = pc_mn(tmplt[crrt_row, 4], tmplt[crrt_row, 5])
    tmplt[crrt_row, 2] = bsln_q1(tmplt[crrt_row, 3])
  }
  
  
  ####Aided Awareness Within Spirit Type
  ## - Among Past 6 Month Spirit Type Consumers
  vbcode = paste0('E2aBrandAware_', code)
  crrt_row = which(measure == ' - Among Past 6 Month Spirit Type Consumers') ##current row
  if(sum(vbcode %in% names.Raw)==0)
  {}
  else
  {
    tmplt[crrt_row, 5] = sum(is.na(Raw[, names.Raw==vbcode])==FALSE)
    tmplt[crrt_row, 4] = sum(Raw[, names.Raw==vbcode]=='1', na.rm = TRUE)
    tmplt[crrt_row, 3] = pc_mn(tmplt[crrt_row, 4], tmplt[crrt_row, 5])
    tmplt[crrt_row, 2] = bsln_q1(tmplt[crrt_row, 3])
  }
  
  
  
  ####Consideration  - among brands aware of
  vbcode = paste0('E3aConsider_r', code)
  ##___% Current User (Past Month) - Would Consider Again (E3aConsider_rXX = 7)
  crrt_row = which(measure == '___% Current User (Past Month) - Would Consider Again')
  if(sum(vbcode %in% names.Raw)==0)
  {}
  else
  {
    tmplt[crrt_row, 4] = sum(Raw[, names.Raw==vbcode]==7, na.rm = TRUE)
    ##___% Lapsed User (Over 1 Months Ago) - Would Consider Again (E3aConsider_rXX = 5)
    tmplt[crrt_row+1, 4] = sum(Raw[, names.Raw==vbcode]==5, na.rm = TRUE)
    ##___% Non User - Would Consider Consuming (E3aConsider_rXX = 3)
    tmplt[crrt_row+2, 4] = sum(Raw[, names.Raw==vbcode]==3, na.rm = TRUE)
    ##___% Current User (Past Month) - Would Not Consider Again (E3aConsider_rXX = 6)
    tmplt[crrt_row+3, 4] = sum(Raw[, names.Raw==vbcode]==6, na.rm = TRUE)
    ##___% Lapsed User (Over 1 Months Ago) - Would Not Consider Again (E3aConsider_rXX = 4)
    tmplt[crrt_row+4, 4] = sum(Raw[, names.Raw==vbcode]==4, na.rm = TRUE)
    ##___% Non User - Would Not Consider Consuming (E3aConsider_rXX = 2)
    tmplt[crrt_row+5, 4] = sum(Raw[, names.Raw==vbcode]==2, na.rm = TRUE)
    ##added by May 8/31/15 new category ___% ___% Never drank and no opinion about drinking in the future (E3aConsider_rXX = 1)
    tmplt[crrt_row+6, 4] = sum(Raw[, names.Raw==vbcode]==1, na.rm = TRUE)
    
    TMP = sum(as.numeric(tmplt[crrt_row+c(0:6), 4])) #revised to 6
    
    for(i in 0:6) #revised to 6
    {
      tmplt[crrt_row+i, 3] = pc_mn(tmplt[crrt_row+i, 4], TMP)
      tmplt[crrt_row+i, 2] = bsln_q1(tmplt[crrt_row+i, 3])
    }
    
    ##total: Consideration  - among brands aware of
    crrt_row = which(measure == 'Consideration  - among brands aware of')
    tmplt[crrt_row+1, 5] = TMP #don't change
    
    ####Penetration among spirit type consumed more than 1 time a month
    crrt_row = which(measure == 'Penetration among spirit type consumed more than 1 time a month')
    vbcode = paste0('S8aTimes_c', switch(bvgtype, 'Bourbon' = 3, 'Scotch' = 3, 'Whiskey' = 3, 'Brandy' = 4, 'Cognac' = 5,
                                         'Cordials' = 8, 'Liqueur' = 8, 'Gin' = 9, 'Rum' = 14, 
                                         'Tequila' = 15, 'Vodka' = 16))
    if(sum(vbcode %in% names.Raw)==0)
    {}
    else
    {  
      ##need 'E3aConsider_r'
      tmplt[crrt_row, 5] = sum(Raw[, names.Raw==vbcode] <= 10, na.rm = TRUE)
      tmplt[crrt_row, 4] = sum(as.numeric(tmplt[which(measure=='___% Current User (Past Month) - Would Consider Again'), 4]),
                               as.numeric(tmplt[which(measure=='___% Current User (Past Month) - Would Not Consider Again'), 4]))
      tmplt[crrt_row, 3] = pc_mn(tmplt[crrt_row, 4], tmplt[crrt_row, 5])
      tmplt[crrt_row, 2] = bsln_q1(tmplt[crrt_row, 3])
    }
  }
  
  
  
  
  ####Purchased among brands aware of
  crrt_row = which(measure=='Purchased among brands aware of')
  vbcode = paste0('E3aConsider_r', code)
  if(sum(vbcode %in% names.Raw)==0)
  {}
  else
  {
    TMP = sum(is.na(Raw[, names.Raw==vbcode])==FALSE)
    tmplt[crrt_row+1, 5] = TMP
    ##___Past Month
    crrt_row = which(measure=='___Past Month')
    tmplt[crrt_row, 4] = tmplt[which(measure == 'Penetration among spirit type consumed more than 1 time a month') ,4]
    tmplt[crrt_row, 3] = pc_mn(tmplt[crrt_row, 4], TMP)
    tmplt[crrt_row, 2] = bsln_q1(tmplt[crrt_row, 3])
    ##___Lapsed
    crrt_row = which(measure=='___Lapsed')
    tmplt[crrt_row, 4] = sum(as.numeric(tmplt[which(measure=='___% Lapsed User (Over 1 Months Ago) - Would Consider Again'), 4]),
                             as.numeric(tmplt[which(measure=='___% Lapsed User (Over 1 Months Ago) - Would Not Consider Again'), 4]))
    tmplt[crrt_row, 3] = pc_mn(tmplt[crrt_row, 4], TMP)
    tmplt[crrt_row, 2] = bsln_q1(tmplt[crrt_row, 3])
  }
  
  
  #####Satisfaction among brands consumed
  vbcode = paste0('E6aCustSat_r', code)
  ##___% Extremely Satisfied (9-10)  (E6aCustSat_rXX=10,11)
  crrt_row = which(measure=='___% Extremely Satisfied (9-10)')
  if(sum(vbcode %in% names.Raw)==0)
  {}
  else
  {
    tmplt[crrt_row, 4] = sum(Raw[,names.Raw==vbcode]==10, Raw[,names.Raw==vbcode]==11, na.rm = TRUE)
    ##___% Mostly Satisfied (6-8) (E6aCustSat_rXX=7-9)
    tmplt[crrt_row+1, 4] = sum(Raw[,names.Raw==vbcode] %in% 7:9, na.rm = TRUE)
    ##___% Neutral (5) (E6aCustSat_rXX=6)
    tmplt[crrt_row+2, 4] = sum(Raw[,names.Raw==vbcode]==6, na.rm = TRUE)
    ##___% Dissatisfied (0-4) (E6aCustSat_rXX=1-5)
    tmplt[crrt_row+3, 4] = sum(Raw[,names.Raw==vbcode] %in% 1:5, na.rm = TRUE)
    TMP = sum(as.numeric(tmplt[crrt_row+0:3, 4]))
    for(i in 0:3)
    {
      tmplt[crrt_row+i, 3] = pc_mn(tmplt[crrt_row+i, 4], TMP)
      tmplt[crrt_row+i, 2] = bsln_q1(tmplt[crrt_row+i, 3])
    }
    
    ##total: Satisfaction among brands consumed
    crrt_row = which(measure=='Satisfaction among brands consumed')
    tmplt[crrt_row+1, 5] = TMP
  }
  
  
  ####Brand Loyalty among brands consumed mean (benchmark)
  vbcode = paste0('E7aBrandUse_r', code)
  crrt_row = which(measure=='Brand Loyalty among brands consumed mean (benchmark)')
  if(sum(vbcode %in% names.Raw)==0)
  {}
  else
  {
    TMP = table(factor(Raw[, names.Raw==vbcode], levels = 1:7))
    tmplt[crrt_row, 3] = round(sum(7:1 * TMP)/sum(TMP), digits = 1)##reverse
    tmplt[crrt_row, 4] = sum(TMP)   ###???
    tmplt[crrt_row, 2] = paste0(tmplt[crrt_row, 3], '(', benchmark1, ')')
    tmplt[crrt_row+1, 5] = tmplt[which(measure=='Satisfaction among brands consumed')+1,5]
    ####___Top 2 box (%)
    crrt_row = which(measure=='___Top 2 box (%)')
    tmplt[crrt_row, 4] = sum(Raw[, names.Raw==vbcode] %in% 1:2, na.rm = TRUE)
    tmplt[crrt_row, 3] = pc_mn(tmplt[crrt_row, 4],
                               tmplt[which(measure=='Satisfaction among brands consumed')+1,5])
    tmplt[crrt_row, 2] = bsln_q1(tmplt[crrt_row, 3])
  }
  
  ####Advocacy among brands consumed mean (benchmark)
  
  ####___Recommend brand spontaneously (E7cBrandUse_rXX=1)
  vbcode = paste0('E7cBrandUse_r', code)
  crrt_row = which(measure=='___Recommend brand spontaneously')
  if(sum(vbcode %in% names.Raw)==0)
  {}
  else
  {
    
    tmplt[crrt_row, 4] = sum(Raw[,names.Raw==vbcode]==1, na.rm = TRUE)
    ##___Recommend brand after being asked (E7cBrandUse_rXX=2)
    tmplt[crrt_row+1, 4] = sum(Raw[,names.Raw==vbcode]==2, na.rm = TRUE)
    ##___Haven't made a recommendation for or against (E7cBrandUse_rXX=3)
    tmplt[crrt_row+2, 4] = sum(Raw[,names.Raw==vbcode]==3, na.rm = TRUE)
    ##___Recommend NOT consuming brand after being asked (E7cBrandUse_rXX=4)
    tmplt[crrt_row+3, 4] = sum(Raw[,names.Raw==vbcode]==4, na.rm = TRUE)
    ##___Recommend NOT consuming brand spontaneously (E7cBrandUse_rXX=5)
    tmplt[crrt_row+4, 4] = sum(Raw[,names.Raw==vbcode]==5, na.rm = TRUE)
    TMP = sum(as.numeric(tmplt[crrt_row+0:4, 4]))
    for(i in 0:4)
    {
      tmplt[crrt_row+i, 3] = pc_mn(tmplt[crrt_row+i, 4], TMP)
      tmplt[crrt_row+i, 2] = bsln_q1(tmplt[crrt_row+i, 3])
    }
    
    tmplt[crrt_row-1, 5] = TMP
  }
  
  
  ####Top 5 Sources of Communication Awareness- past 3 months (E5bInfo_r1_cXX =1)
  crrt_row = which(measure=='Top 5 Sources of Communication Awareness- past 3 months')
  vbcode = sapply(1:nrow(E5b), function(x) {paste0('E5bInfo_r', x, '_c', code)})  #changed to 24
  if(sum(vbcode %in% names.Raw)==0)
  {}
  else
  {
    counts = sapply(vbcode, function(x) {if(length(Raw[,names.Raw==x]))
      sum(Raw[,names.Raw==x], na.rm = TRUE)
      else
        0
    })
    orders = order(counts, decreasing = TRUE)
    tmplt[crrt_row, 5] = sum(Raw[, names.Raw==vbcode[1]] %in% 0:1, na.rm = TRUE)
    for(i in 1:5)
    {
      tmplt[crrt_row+i, 4] = counts[orders[i]]
      tmplt[crrt_row+i, 3] = pc_mn(tmplt[crrt_row+i, 4], tmplt[crrt_row, 5])
      tmplt[crrt_row+i, 2] = paste0(E5b[orders[i],2], ', ', tmplt[crrt_row+i,3], '%')
    }
  }
  
  
  ####Top 10 Brand Perceptions (B4_r1_XX--B4_r17_XX  & B5_r1_XX--B5_r24_XX)
  B4B5 = rbind(B4, B5)
  crrt_row = which(measure=='Top 10 Brand Perceptions')
  vbcodeB4 = sapply(1:nrow(B4), function(x) {paste0('B4_r', x, '_', code)})
  vbcodeB5 = sapply(1:nrow(B5), function(x) {paste0('B5_r', x, '_',code)})
  vbcode = c(vbcodeB4, vbcodeB5)
  if(sum(vbcode %in% names.Raw)==0)
  {}
  else
  {
    avg = sapply(vbcode, function(x) {mean(Raw[, names.Raw==x], na.rm = TRUE)})
    orders = order(avg, decreasing = TRUE)
    for(i in 1:10)
    {
      tmplt[crrt_row+i, 4] = sum(is.na(Raw[,names.Raw==vbcode[orders[i]]])==FALSE)
      tmplt[crrt_row+i, 3] = round(avg[orders[i]], digits = 1)
      tmplt[crrt_row+i, 2] = paste0(B4B5[orders[i],2], ', ', tmplt[crrt_row+i, 3])
    }
  }
  
  ####Top 10 Brand Imagery Attributes (B6_1_XX--B6_25_XX)
  crrt_row = which(measure=='Top 10 Brand Imagery Attributes')
  vbcode = sapply(1:nrow(B6), function(x) {paste0('B6_', x, '_', code)}) #revised to 26
  if(sum(vbcode %in% names.Raw)==0)
  {}
  else
  {
    avg = sapply(vbcode, function(x) {mean(Raw[,names.Raw==x], na.rm = TRUE)})
    orders = order(avg, decreasing = TRUE)
    for(i in 1:10)
    {
      tmplt[crrt_row+i, 4] = sum(is.na(Raw[,names.Raw==vbcode[orders[i]]])==FALSE)
      tmplt[crrt_row+i, 3] = round(avg[orders[i]], digits = 1)
      tmplt[crrt_row+i, 2] = paste0(B6[orders[i],2], ', ', tmplt[crrt_row+i, 3])
    }
  }
  
  
  ####Demographics among monthly consumers (E3aConsider_rXX==6, 7 ???)
  crrt_row = which(measure=='Demographics among monthly consumers')
  vbcode = paste0('E3aConsider_r', code)
  if(sum(vbcode %in% names.Raw)==0)
  {
    return(tmplt)
    break
  }
  else
  {
    ##reguser is computed from 'E3aConsider_rXX', , it is indespensible to fill in the rest of the sheet
    reguser = Raw[,names.Raw==vbcode] %in% 6:7  
    TMP = sum(reguser, na.rm = TRUE)
    tmplt[crrt_row+1, 5] = TMP
  }
  
  
  ##Age 21-34 (S2age)
  crrt_row = which(measure=='Age 21-34')
  reguser_age = Raw$S2Age[reguser]
  tmplt[crrt_row, 4] = sum(reguser_age>=21 & reguser_age<=34)
  ##___ 35-50
  tmplt[crrt_row+1, 4] = sum(reguser_age>=35 & reguser_age<=50)
  ##___ 51-69
  tmplt[crrt_row+2, 4] = sum(reguser_age>=51 & reguser_age<=69)
  ##___ 70+
  tmplt[crrt_row+3, 4] = sum(reguser_age>=70)
  
  for(i in 0:3)
  {
    tmplt[crrt_row+i, 3] = pc_mn(tmplt[crrt_row+i, 4], TMP)
    tmplt[crrt_row+i, 2] = bsln_q1(tmplt[crrt_row+i, 3])
  }
  
  ####Race White NH
  crrt_row = which(measure=='Race White NH')
  vbcode = sapply(1:7, function(x) {paste0('D9Ethnic_', x)})
  if(sum(vbcode %in% names.Raw)==0)
  {}
  else
  {
    mixed = rowSums(Raw[, names.Raw %in% vbcode], na.rm = TRUE)[reguser]
    other = rowSums(Raw[, names.Raw %in% vbcode[5:7]], na.rm = TRUE)[reguser]
    tmplt[crrt_row, 4] = sum(Raw[reguser, names.Raw==vbcode[1]]==1 & mixed<2, na.rm = TRUE)
    ##___Hispanic
    tmplt[crrt_row+1, 4] = sum(Raw[reguser, names.Raw==vbcode[2]]==1 & mixed<2, na.rm = TRUE)
    ##___Black NH
    tmplt[crrt_row+2, 4] = sum(Raw[reguser, names.Raw==vbcode[3]]==1 & mixed<2, na.rm = TRUE)
    ##___Asian NH
    tmplt[crrt_row+3, 4] = sum(Raw[reguser, names.Raw==vbcode[4]]==1 & mixed<2, na.rm = TRUE)
    ##___Other NH
    tmplt[crrt_row+4, 4] = sum(other==1 & mixed<2, na.rm = TRUE)
    ##___Mixed
    tmplt[crrt_row+5, 4] = sum(mixed>=2, na.rm = TRUE)
    
    for(i in 0:5)
    {
      tmplt[crrt_row+i, 3] = pc_mn(tmplt[crrt_row+i, 4], TMP)
      tmplt[crrt_row+i, 2] = bsln_q1(tmplt[crrt_row+i, 3])
    }
  }
  
  
  #### Married
  crrt_row = which(measure=='Married')
  tmplt[crrt_row, 4] = sum(Raw$D1Marital[reguser]==2, na.rm = TRUE)
  tmplt[crrt_row, 3] = pc_mn(tmplt[crrt_row, 4], TMP)
  tmplt[crrt_row, 2] = bsln_q1(tmplt[crrt_row, 3])
  
  ####Income <$30,000
  crrt_row = which(measure=='Income <$30,000')
  tmplt[crrt_row, 4] = sum(Raw$D8Income[reguser]==1, na.rm = TRUE)
  tmplt[crrt_row+1, 4] = sum(Raw$D8Income[reguser]==2, na.rm = TRUE)
  tmplt[crrt_row+2, 4] = sum(Raw$D8Income[reguser]==3|Raw$D8Income[reguser]==4, na.rm = TRUE)
  tmplt[crrt_row+3, 4] = sum(Raw$D8Income[reguser]==5|Raw$D8Income[reguser]==6|Raw$D8Income[reguser]==7,
                             na.rm = TRUE)
  for(i in 0:3)
  {
    tmplt[crrt_row+i, 3] = pc_mn(tmplt[crrt_row+i, 4], TMP)
    tmplt[crrt_row+i, 2] = bsln_q1(tmplt[crrt_row+i, 3])
  }
  
  ####Education Some HS-GED/HS
  crrt_row = which(measure=='Education Some HS-GED/HS')
  tmplt[crrt_row, 4] = sum(Raw$D6Ed[reguser]==1|Raw$D6Ed[reguser]==2, na.rm = TRUE)
  tmplt[crrt_row+1, 4] = sum(Raw$D6Ed[reguser]==3|Raw$D6Ed[reguser]==4, na.rm = TRUE)
  tmplt[crrt_row+2, 4] = sum(Raw$D6Ed[reguser]==5, na.rm = TRUE)
  tmplt[crrt_row+3, 4] = sum(Raw$D6Ed[reguser]==6|Raw$D6Ed[reguser]==7|Raw$D6Ed[reguser]==8,
                             na.rm = TRUE)
  for(i in 0:3)
  {
    tmplt[crrt_row+i, 3] = pc_mn(tmplt[crrt_row+i, 4], TMP)
    tmplt[crrt_row+i, 2] = bsln_q1(tmplt[crrt_row+i, 3])
  }
  
  ####Gender Male
  crrt_row = which(measure=='Gender Male')
  tmplt[crrt_row, 4] = sum(Raw$S1Gender[reguser]==1, na.rm = TRUE)
  tmplt[crrt_row+1, 4] = sum(Raw$S1Gender[reguser]==2, na.rm = TRUE)
  for(i in 0:1)
  {
    tmplt[crrt_row+i, 3] = pc_mn(tmplt[crrt_row+i, 4], TMP)
    tmplt[crrt_row+i, 2] = bsln_q1(tmplt[crrt_row+i, 3])
  }
  
  ####Household size (mean)
  crrt_row = which(measure == 'Household size (mean)')
  tmplt[crrt_row, 4] = TMP
  tmplt[crrt_row, 3] = round(mean(Raw$D2HHSize[reguser], na.rm = TRUE), digits = 1)
  tmplt[crrt_row, 2] = tmplt[crrt_row, 3]
  
  ####Total Spirits Volume among monthly consumers
  crrt_row = which(measure == 'Total Spirits Volume among monthly consumers')
  brandlist = c('BSW', 'Brandy', 'Cognac', 'CordLiq', 'Gin', 'Rum', 'Tequila', 'Vodka')
  vbcode = sapply(brandlist, function(x) {paste0('V2Occ', x, '_r', 1:17, '_c1')})
  if(sum(vbcode %in% names.Raw)==0)
  {}
  else
  {
    TMP = apply(Raw[,names.Raw %in% vbcode], 1, function(x) {sum(x, na.rm = TRUE)})
    tmplt[crrt_row, 5] = sum(TMP>0 & TMP<75)
    tmplt[crrt_row, 4] = sum(TMP>0 & TMP<75 & reguser)  ##???
    tmplt[crrt_row, 3] = pc_mn(sum(TMP[TMP>0 & TMP<75 & reguser]), sum(TMP[TMP>0 & TMP<75]))
    tmplt[crrt_row, 2] = bsln_q1(tmplt[crrt_row, 3])
  }
  
  return(tmplt)
}


## This two functions are nested in spirit
pc_mn = function(d4, d5)
  ##nested in spirit
{
  round(as.numeric(d4)/as.numeric(d5) * 100, digits = 1)
}

bsln_q1 = function(d3)
  ##nested in spirit
{
  paste0(d3, '%')
}


####  IV. Run the function on each brand and output the final scorecard that we want  ####
##output for multiple time period
initial1 = spirit(brand = Brand_List$Brand[1], code = Brand_List$Code[1], bvgtype = Brand_List$Type[1],
                 Raw = Raw1, tmplt = tmplt, names.Raw = names(Raw1), measure = tmplt$Measures)
initial2 = spirit(brand = Brand_List$Brand[1], code = Brand_List$Code[1], bvgtype = Brand_List$Type[1],
                 Raw = Raw2, tmplt = tmplt, names.Raw = names(Raw2), measure = tmplt$Measures)
initial = cbind(initial1[,1:2], initial2[,2])
names(initial) = c('Measures', 'BASELINE_Q1', 'BASELINE_Q2')
write.xlsx(initial, 'MultipleB.xlsx', sheetName = paste0(Brand_List$Brand[1], '_', Brand_List$Type[1]),
           row.names = FALSE)

options(warn = 1)
options(java.parameters = "-Xmx4000m")
for(i in 2:nrow(Brand_List))
{
  sht1 = tryCatch(spirit(brand = Brand_List$Brand[i], code = Brand_List$Code[i], bvgtype = Brand_List$Type[i],
                        Raw = Raw1, tmplt = tmplt, names.Raw = names(Raw1), measure = tmplt$Measures),
                 warnings = function(w) {print(paste0('W', i))},
                 error = function(e) {print("i")})
  sht2 = tryCatch(spirit(brand = Brand_List$Brand[i], code = Brand_List$Code[i], bvgtype = Brand_List$Type[i],
                         Raw = Raw2, tmplt = tmplt, names.Raw = names(Raw2), measure = tmplt$Measures),
                  warnings = function(w) {print(paste0('W', i))},
                  error = function(e) {print("i")})
  if(is.data.frame(sht1) & is.data.frame(sht2))
  {
    sht = cbind(sht1[,1:2], sht2[,2])
    names(sht) = c('Measures', 'BASELINE_Q1', 'BASELINE_Q2')
  }
  else
    sht = i
  
  write.xlsx(sht, 'MultipleB.xlsx', sheetName = paste0(Brand_List$Brand[i], '_', Brand_List$Type[i]),
             row.names = FALSE, append = TRUE)
}


############################################# Old stuff ######################### 
###### Final output  ######
### In this part, we use the functions built in the previous section to fill in one template for each brand
### and append them together into one .xlsx file
initial = spirit(brand = Brand_List$Brand[1], code = Brand_List$Code[1], bvgtype = Brand_List$Type[1],
                 Raw = Raw1, tmplt = tmplt, names.Raw = names(Raw1), measure = tmplt$Measures)
write.xlsx(initial, 'MultipleB.xlsx', sheetName = paste0(Brand_List$Brand[1], '_', Brand_List$Type[1]),
           row.names = FALSE)

options(warn = 1)
options(java.parameters = "-Xmx4000m")
for(i in 2:nrow(Brand_List))
{
  sht = tryCatch(spirit(brand = Brand_List$Brand[i], code = Brand_List$Code[i], bvgtype = Brand_List$Type[i],
                        Raw = Raw1, tmplt = tmplt, names.Raw = names(Raw1), measure = tmplt$Measures),
                 warnings = function(w) {print(paste0('W', i))},
                 error = function(e) {print("Hehe")})
  
  write.xlsx(sht, 'MultipleB.xlsx', sheetName = paste0(Brand_List$Brand[i], '_', Brand_List$Type[i]),
             row.names = FALSE, append = TRUE)
}

### for Raw2
initial = spirit(brand = Brand_List$Brand[1], code = Brand_List$Code[1], bvgtype = Brand_List$Type[1],
                 Raw = Raw2, tmplt = tmplt, names.Raw = names(Raw2), measure = tmplt$Measures)
write.xlsx(initial, 'MultipleB.xlsx', sheetName = paste0(Brand_List$Brand[1], '_', Brand_List$Type[1]),
           row.names = FALSE)

options(warn = 1)
options(java.parameters = "-Xmx4000m")
for(i in 2:nrow(Brand_List))
{
  sht = tryCatch(spirit(brand = Brand_List$Brand[i], code = Brand_List$Code[i], bvgtype = Brand_List$Type[i],
                        Raw = Raw2, tmplt = tmplt, names.Raw = names(Raw2), measure = tmplt$Measures),
                 warnings = function(w) {print(paste0('W', i))},
                 error = function(e) {print("Hehe")})
  
  write.xlsx(sht, 'MultipleB.xlsx', sheetName = paste0(Brand_List$Brand[i], '_', Brand_List$Type[i]),
             row.names = FALSE, append = TRUE)
}






