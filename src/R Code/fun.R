#Functions for Actuarial Science Research Project
#Conor O'Sullivan
#26 July 2017

df <- function(vector){
  #Returns a data frame using a vector of "Coicop" numbers 
  #The "Coicop" numbers define the products included in "Expenditure"
  
  #Coicop codes for defining explanatory variables
  workIncome. = c(50100000) 
  otherIncome. = c(50331000,50332000,50333000,50333100,50333200,50333300,50333400,50333500,50400000,
                   50519000,50519100,50600000,50510500)
  inKindIncome. = c(51200000, 51300000,51400000,51520000, 51530000,51730000,51740000,51770000,51780000,
                    51801000,51802000, 51803000,51804000,51805000,51806000,51807000,51808000,51809000,
                    51810000,51811000,51812000,51813000,51814000,51815000,51816000,51817000,51818000,
                    51910000,51920000,51930000,51940000,51950000,51960000,51971000,51972000,
                    51990000)
  employerCont. = c(51973000,52422000)
  
  x1 = df.(vector)
  names(x1)[2] = 'expenditure'
  
  x2 = df.(workIncome.)
  names(x2)[2] = 'workIncome'
  #x2
  x2$num <- NULL
  
  x3 = df.(otherIncome.)
  names(x3)[2] = 'otherIncome'
 
  x3$num <- NULL
  
  x4 = df.(inKindIncome.)
  names(x4)[2] = 'inKindIncome'
  x4$num <- NULL
  
  x5 = df.(employerCont.)
  names(x5)[2] = 'employerCont'
  x5$num <- NULL
  Exp1 <- data.frame(ID = dataHInfo$UQNO,
                    incomeDecile = dataHInfo$IncomeDecile,income = dataHInfo$Income, province = dataHInfo$Province, 
                     settlementType = dataHInfo$Settlement_type, hSize = dataHInfo$Hsize, 
                     genderOfHead = dataHInfo$GenderOfHead, popGrpOfHead = dataHInfo$PopGrpOfHead, 
                     personsSupported =dataHInfo$Q111PERSSUPPORTED,weight = dataHInfo$Full_calwgt)
  
  data = merge(x1,x2, by = "ID")
  data = merge(data,x3, by = "ID")
  data = merge(data,x4,by = "ID")
  data = merge(data,x5,by = "ID")
  data = merge(data, Exp1,by = "ID")
  data$ID <- NULL
  row = data.frame(row = 1:nrow(data))
  bin = non_zero <- ifelse(data$expenditure > 0, 1, 0)
  data = cbind(row,bin,data)
  return(data)
}
df. <- function(vector){
  #this function helps the function "df"
  d1 <- dataTotal[dataTotal$Coicop %in% vector,]
  d2 <- data.frame(ID = d1$UQNO, col = d1$Valueannualized)
  
  d3 <- ddply(d2,.(ID),summarize,col = sum(col), num = length(ID))
  ID = dataHInfo$UQNO
  ID2 = ID[!dataHInfo$UQNO %in% d3$ID]
  d4 <- data.frame(ID = ID2, col = 0, num = 1)
  
  d5 <- rbind(d3, d4)
  
  
  
  return(d5)
}

check <- function(df, vector){
  #Performs checks on the data frame created by the function "df"
  v1 = nrow(df) - nrow(dataHInfo)
  v2= sum(dataTotal$Coicop %in%vector) - sum(df$num[df$expenditure>0])
  
  d1 = dataTotal[dataTotal$Coicop %in%vector,]
  v3 = sum(df$expenditure) - sum(d1$Valueannualized)
  
  a1 = (d_all$workIncome) + (d_all$otherIncome)
  a2 = (as.numeric(dataHInfo$Income))
  v4 =sum(a1-a2)
  
  v5 = nrow(df[df$expenditure>0,]) - sum(df$bin)
  print(v1)
  print(v2)
  print(v3)
  print(v4)
  print(v5)
  return(v1 == 0 & v2 == 0 & v3 == 0 & abs(v4)<500 & v5==0)
}

decile<- function(vector)
{
  #Creates decile factor varaible from a continuous variable 
  #note all 0 values are lumped into a single level
  vector. = vector[vector>0]
 # quantile(vector.,seq(0,1,0.1),names = TRUE)
  quant = quantile(vector.,seq(0,1,0.1),names = FALSE)
  quant[1] = 0.1
  quant = c(-0.1,quant)
  print(quant)
  return(cut(vector,quant,labels = 0:10))
}

