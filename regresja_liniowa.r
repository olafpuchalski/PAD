rm(list=ls())

#######################
# wczytanie bibliotek #
#######################
library("ggplot2")
library("car")
library("lmtest")
library("Metrics")

#######################
#   wczytanie danych  #
#######################
# https://www.kaggle.com/datasets/kumarajarshi/life-expectancy-who
dane <- read.csv("Life Expectancy Data.csv")

####################################
# Generowanie liczb pseudolosowych #
####################################

# generator liczb pseudo-losowych z rozkładu N(0,1) (r - random)
rnorm(6, 0, 1)

# generator liczb pseudo-losowych z rozkładu jednostajnego na przedziale [0; 10]
runif(n = 5, min = 0, max = 10)
runif(n = 5, min = 0, max = 10)
runif(n = 5, min = 0, max = 10)

# ziarno generatora (inicjalizacja punktu startowego)
set.seed(123)
# od tego samego punktu startowego, generowane są te same sekwencje liczb
runif(n = 5, min = 0, max = 10)
set.seed(123)
runif(n = 5, min = 0, max = 10)

################################################
# losowy podział na zbiór treningowy i testowy #
################################################
set.seed(2023)
rozmiar <- floor(0.8 * nrow(dane))
indexes <- sample(c(1:nrow(dane)), size = rozmiar, replace = FALSE)
treningowy <- dane[indexes,]
testowy <- dane[-indexes,]

##########################
#   wizualizacja danych  #
##########################

# statystyki opisowe
summary(dane)

# histogram oczekiwanej długości życia na świecie
ggplot(dane, aes(x=Life.expectancy)) + geom_histogram()

# oczekiwana długości życia w Polsce na przestrzeni lat
ggplot(dane[dane$Country == "Poland",], aes(x=Year, y=Life.expectancy)) + geom_line()

# oczekiwana długości życia w Polsce vs BMI
ggplot(dane[dane$Country == "Poland",], aes(x=BMI, y=Life.expectancy)) + geom_point()

# oczekiwana długości życia w Indiach vs BMI
ggplot(dane[dane$Country == "India",], aes(x=BMI, y=Life.expectancy)) + geom_point()

# oczekiwana długości życia vs BMI
ggplot(dane, aes(x=BMI, y=Life.expectancy)) + geom_point()

# histogram oczekiwanej długości życia per rok
ggplot(dane, aes(x=Life.expectancy)) + geom_histogram() + facet_wrap(. ~ Year)

# oczekiwana długość życia względem statusu kraju
ggplot(dane ,aes(x=Status, y=Life.expectancy, fill=Status)) + 
  geom_boxplot() +
  ggtitle("Oczekiwana długość życia per statusu kraju")

# zapisanie wykresu do pliku
ggsave(filename = "dlugosc_zycia.jpg")

# zapisanie danych o Polsce do pliku 
write.csv(x = dane[dane$Country == "Poland",], file = "dane_Polska.csv", row.names = FALSE)
write.csv2(x = dane[dane$Country == "Poland",], file = "dane_Polska_2.csv", row.names = FALSE)

###########################################################
#         modelowanie - korelacja liniowa Pearsona        #
###########################################################

cor.test(dane$Life.expectancy, dane$BMI)
cor.test(dane$Life.expectancy[dane$Country == "Poland"], dane$BMI[dane$Country == "Poland"])
# usuniecie wartości odstającej
dane_Polska <- dane[dane$Country == "Poland",]
dane_Polska <- dane_Polska[dane_Polska$BMI > 10,]
cor.test(dane_Polska$Life.expectancy, dane_Polska$BMI)

###########################################################
#         modelowanie - regresja liniowa (Indie)          #
###########################################################

# Klasyczna Metoda Najmniejszych Kwadratów
dane_Indie <- dane[dane$Country == "India",]
fit <- lm(data = dane_Indie, formula = Life.expectancy ~ BMI)

# interpretacja: parametry modelu, istotność statystyczna, R2
summary(fit)

# jakość dopasowania dla poszczególnych obserwacji
plot(fit, which = 1)
# wartości odstające
plot(fit, which = 4)

# przykład testu RESET
x <- c(1:30)
y1 <- 1 + x + x^2 + rnorm(30)
y2 <- 1 + x + rnorm(30)
resettest(y1 ~ x , power=2, type="fitted") # zła specyfikacja postaci funkcyjnej modelu
resettest(y2 ~ x , power=2, type="fitted") 

# test RESET
resettest(fit, power=2:3, type= "fitted")

# dodanie nowych zmiennych
fit2 <- lm(data = dane_Indie, formula = Life.expectancy ~ BMI + I(BMI^2) + I(BMI^3))
summary(fit2)
resettest(fit2, power=2:3, type= "fitted")

# transformacja zmiennych
fit2 <- lm(data = dane_Indie, formula = Life.expectancy ~ I(log(BMI)))
summary(fit2)
resettest(fit2, power=2:3, type= "fitted")

###########################################################
#         modelowanie - regresja liniowa (świat)          #
###########################################################

# Klasyczna Metoda Najmniejszych Kwadratów
fit3 <- lm(data = treningowy, formula = Life.expectancy ~ Year + Alcohol + BMI + Schooling)
summary(fit3)

# test RESET
resettest(fit3, power=2:3, type= "fitted")

# VIF (czynnik inflacji wariancji, ang. Variance Inflation Factor)
vif(fit3) # stopień współliniowości zmiennych objaśniających w dopuszczalny zakresie (< 10)

# prognoza
testowy2 <- testowy[, c("Life.expectancy", "Year", "Alcohol", "BMI", "Schooling")]
summary(testowy2)
testowy2 <- testowy2[complete.cases(testowy2),]
summary(testowy2)

prognoza <- predict(object = fit3, newdata = testowy2)
rmse(testowy2$Life.expectancy, prognoza)
mape(testowy2$Life.expectancy, prognoza)

##############################################################
#         modelowanie - regresja liniowa (bez kraju)         #
##############################################################

# rozbudowany model (zamienne bez kraju)
fit4 <- lm(data = treningowy, formula = Life.expectancy ~ . - Country)
summary(fit4)
vif(fit4)
resettest(fit4, power=2:3, type= "fitted")

testowy2 <- testowy[complete.cases(testowy),]
prognoza <- predict(object = fit4, newdata = testowy2)
rmse(testowy2$Life.expectancy, prognoza)
mape(testowy2$Life.expectancy, prognoza)

##############################################################
#         modelowanie - regresja liniowa (kraj)              #
##############################################################

fit5 <- lm(data = treningowy, formula = Life.expectancy ~ Country)
summary(fit5)