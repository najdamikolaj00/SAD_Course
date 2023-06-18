# install.packages("dplyr")
# install.packages("car")
library(dplyr)
library(car)

data <- read.csv("Data/gdp_per_capita.csv", header = TRUE)
print(head(data))

shapiro_result <- shapiro.test(data$gdpPercap)
print(shapiro_result)

continent_data <- split(data$gdpPercap, data$continent)

results <- lapply(continent_data, function(x) {
  ks.test(x, "pnorm", mean = mean(x), sd = sd(x))
})

for (i in 1:length(results)) {
  cat("Kontynent:", names(results[i]), "\n")
  print(results[[i]])
  cat("\n")
}

levene_result <- leveneTest(gdpPercap ~ continent, data = data)
print(levene_result)

kruskal_result <- kruskal.test(gdpPercap ~ continent, data = data)
print(kruskal_result)