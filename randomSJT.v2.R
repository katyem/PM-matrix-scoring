# Create a response set for a rank-order sjt that has four scenarios and four response options/solutions per scenario. 
# scoring is in the SANDBOX excel file
packages  <- c('tidyverse', 'readxl', 'writexl', 'GGally', 'MASS', 'stringr')

for(p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

# load scoring table
getwd( ) 
setwd("D:/R stuff/PM matrix scoring")
scoringData <- read_excel("SANDBOX SJA data.v1.xlsx") 


# create a data set
thisN = 1000

x1 <- round(rnorm(n = thisN, mean = 77, sd = 11))  # arbitrary but trying to get 92 as max
max(x1)
min(x1)
adj <- max(x1)-92
if(adj<min(x1)) {
  x1 <- x1-adj
  print('yay')
} else {
  x1<- ifelse(x1>mean(x1),x1-adj,x1)
  print('uh oh')
}

max(x1)
min(x1)

## CREDIT: https://stats.stackexchange.com/questions/15011/generate-a-random-variable-with-a-defined-correlation-to-an-existing-variables
n     <- thisN                # length of vector
rho   <- 0.32                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
#x1    <- rnorm(n, 1, 1)        # fixed given data
x2    <- rnorm(n, 30, 5)      # new random data
X     <- cbind(x1, x2)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1

x <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(x1, x)                                    # check correlation = rho

x <- round(x*100)
x <- x+50
# --------------------------------

Xdata <- data.frame(x1,x)
names(Xdata) <- c('score', 'perfScore')
cor(Xdata)
head(Xdata)

data <- data.frame(Xdata$score)
colnames(data) <- c('score')


head(data)
################

checkScore <- function(s_score) {
  if (s_score > 23) {
    s_score <- 23
    }
  return(s_score)
}

i = 1; j =1; x = 1
# names: s1,s2,s3,s4 
#define names of 4 empty columns
for (x in 1:4) {  
    item1 <- paste('s', x, sep = '', collapse=NULL)
    new_cols <- c(item1)
    data[ , new_cols] <- NA
}

head(data)
# loop through scores and assign item responses
# names: s11,s12,s13,s14; s21,s22,s23,s24; s31... 
#define names of 16 empty columns
for (x in 1:4) {  
  for (j in 1:4) {
    item1 <- paste('s', x, j, sep = '', collapse=NULL)
    new_cols <- c(item1)
    data[ , new_cols] <- NA
  }  
}
head(data)
# split scores by 4 (items each worth max of 23 points -> 92 = highest score)
for (i in 1:nrow(data)) {
  theScore <- data$score[i]
  if (theScore > 91) {
    data$s1[i] <- 23
    data$s2[i] <- 23
    data$s3[i] <- 23
    data$s4[i] <- 23
    pattern <- c(1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4)
    for (j in 1:16) {
      data[i,j+5] <- pattern[j]
    }
  } else {
    itemScore <- checkScore(round(rnorm(1, theScore,7)/4))
    data$s1[i] <- itemScore # == data[i,2]
    s_row <- 24 - itemScore
    pattern <- as.numeric(scoringData[s_row,c(1,2,3,4)])
    for (j in 1:4){
      data[i,j+5] <- pattern[j]
    }
    temp <- theScore - data$s1[i]
    
    itemScore <- checkScore(round(rnorm(1, temp, 6)/3))
    data$s2[i] <- itemScore # == data[i,3]
    s_row <- 24 - itemScore
    pattern <- as.numeric(scoringData[s_row,c(1,2,3,4)])
    for (j in 1:4){
      data[i,j+9] <- pattern[j]
    }
    temp <- theScore - data$s1[i] - data$s2[i]
    
    itemScore <- checkScore(round(rnorm(1, temp, 5)/2))
    data$s3[i] <- itemScore # == data[i,4]
    s_row <- 24 - itemScore
    pattern <- as.numeric(scoringData[s_row,c(1,2,3,4)])
    for (j in 1:4){
      data[i,j+13] <- pattern[j]
    }    
    
    itemScore <- checkScore(round(temp-data$s3[i])) 
    data$s4[i] <- itemScore # == data[i,5]
    s_row <- 24 - itemScore
    pattern <- as.numeric(scoringData[s_row,c(1,2,3,4)])
    for (j in 1:4){
      data[i,j+17] <- pattern[j]
    }
  }
 
    
}

data$new_score <- data$s1+data$s2+data$s3+data$s4
head(data)
mean(data$new_score)


## Create performance scores
# custom function to implement min max scaling

###

data$perfScore <- Xdata$perfScore
###

head(data)
adj <- max(data$perfScore)-50
data$perfScore <- data$perfScore-adj
df <- data[ , c("new_score", "perfScore")]

head(df)
ggpairs(df)


## Save data with file name that includes date/time
x = str_replace_all(Sys.time(), "[^[:alnum:]]", "")    # Delete non-alphanumeric
x <- paste('sjt_data.', x,'.xlsx', sep = '' )

write_xlsx(as.data.frame(data), x)

           