#Data source: https://archive.ics.uci.edu/dataset/53/iris
# Wczytanie bibliotek
#install.packages("mvnormtest")
library(mvnormtest)

# Wczytanie danych z pliku txt
data <- read.csv("Data/iris.txt", header = TRUE, sep = ",")

setosa <- data[1:50, ]
versicolor <- data[51:100, ]
virginica <- data[101:150, ]

columns <- c("sepal.length", "sepal.width", "petal.length", "petal.width")

setosa_result <- mvnormtest::mshapiro.test(t(setosa[, columns]))
print("Gatunek setosa:")
print(setosa_result)

versicolor_result <- mvnormtest::mshapiro.test(t(versicolor[, columns]))
print("Gatunek versicolor:")
print(versicolor_result)

virginica_result <- mvnormtest::mshapiro.test(t(virginica[, columns]))
print("Gatunek virginica:")
print(virginica_result)
