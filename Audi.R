library(dplyr)
library(lattice)
library(ggplot2)
library(caret)

setwd("F:\\Kernels")
df <- read.csv("Audi.csv")

dmy <- dummyVars(" ~ .", data = df)
dat_transformed <- data.frame(predict(dmy, newdata = df))

RR = lm(dat_transformed$price~dat_transformed$model..A1+dat_transformed$model..A2+dat_transformed$model..A3+dat_transformed$model..A4+
          dat_transformed$model..A5+dat_transformed$model..A6+dat_transformed$model..A7+dat_transformed$model..A8+dat_transformed$model..Q2+
          dat_transformed$model..Q3+dat_transformed$model..Q5+dat_transformed$model..Q7+dat_transformed$model..Q8+dat_transformed$model..R8+
          dat_transformed$model..RS3+dat_transformed$model..RS4+dat_transformed$model..RS5+dat_transformed$model..RS6+dat_transformed$model..RS7+
          dat_transformed$model..S3+dat_transformed$model..S4+dat_transformed$model..S5+dat_transformed$model..S8+dat_transformed$model..SQ5+
          dat_transformed$model..SQ7+dat_transformed$model..TT+
          dat_transformed$year+
          dat_transformed$transmission.Automatic+dat_transformed$transmission.Manual+dat_transformed$transmission.Semi.Auto+
          dat_transformed$mileage+
          dat_transformed$fuelType.Diesel+dat_transformed$fuelType.Hybrid+dat_transformed$fuelType.Petrol+
          dat_transformed$tax+
          dat_transformed$mpg+
          dat_transformed$engineSize)

RR 
summary(RR)
