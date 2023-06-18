#Data source: https://archive.ics.uci.edu/dataset/53/iris

#install.packages("mvnormtest")
library(mvnormtest)

data <- read.csv("Data/iris.txt", header = TRUE, sep = ",")

setosa <- data[1:50, ]
versicolor <- data[51:100, ]
virginica <- data[101:150, ]

columns <- c("sepal.length", "sepal.width", "petal.length", "petal.width")

setosa_result <- mvnormtest::mshapiro.test(t(setosa[, columns]))
print("Class setosa:")
print(setosa_result)

versicolor_result <- mvnormtest::mshapiro.test(t(versicolor[, columns]))
print("Class versicolor:")
print(versicolor_result)

virginica_result <- mvnormtest::mshapiro.test(t(virginica[, columns]))
print("Class virginica:")
print(virginica_result)
