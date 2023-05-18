#######################
# Autorzy
# Zuzanna Grzechnik, 101252 
# Olaf Puchalski, 82073 
#######################

rm(list=ls())

#######################
# wczytanie bibliotek #
#######################
library("ggplot2")
library("car")
library("lmtest")
library("Metrics")
library("tidyverse")
library("neuralnet")
library("nnet")
library("keras")
library("tensorflow")

#######################
#   wczytanie danych  #
#######################
# https://www.kaggle.com/datasets/fedesoriano/stroke-prediction-dataset
dane <- read.csv("healthcare_dataset.csv")

# Będziemy chcieli obliczyć prawdopodobieństwo zawału dla pacjenta o danych parametrach

#######################
#   edycja danych  #
#######################
dane$bmi <- as.numeric(dane$bmi)
colSums(is.na(dane)) #widzimy, że pojawiły się NA dla bmi, wyrzućmy je
dane <- na.omit(dane)
dane <- dane %>% mutate_if(is.character, as.factor) #zmiana char na factory
dane <- dane %>% mutate_if(is.integer, as.numeric)
dane$stroke <- as.factor(dane$stroke)

summary(dane)

#Jak widzimy, mamy mniej niż 5% przypadków dla zawału, więc nasza próba jest niezbilansowana

library(fastDummies)
dane <- dummy_cols(dane, select_columns = c("gender", "ever_married", "work_type", "Residence_type", "smoking_status"))
dane <- subset(dane, select = -c(gender, ever_married, work_type, Residence_type, smoking_status))

set.seed(245)
liczba_wierszy <- floor(0.80 * nrow(dane)) 
numerki_treningowe <- sample(c(1:nrow(dane)), liczba_wierszy)
train_data <- dane[numerki_treningowe,]
test_data <- dane[-numerki_treningowe,]

#######################
#   budowa modelu  #
#######################

 
myform <- as.formula(paste0('stroke ~ ', 
                            paste(names(train_data[!names(train_data) %in% 'label']),
                                  collapse = ' + ')))

model1 <- nnet(stroke~., data = train_data, size = 4)
model2 <-  neuralnet(stroke ~ .,
  data=train_data,
  hidden=c(4,2),
  linear.output = FALSE
)

net <- neuralnet(stroke ~ ., train_data, hidden=0)
