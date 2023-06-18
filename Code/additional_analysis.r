#install.packages("mvnormtest")
#install.packages("car")

library(mvnormtest)
library(car)
library(ggplot2)

data <- read.csv("Data/iris.txt", header = TRUE, sep = ",")
columns <- c("sepal.length", "petal.length")


mshapiro_result <- mvnormtest::mshapiro.test(t(data[, columns]))
print(mshapiro_result)

levene_result <- leveneTest(sepal.length ~ species, data = data)
print("Homogeniczność, sepal lenght:")
print(levene_result)

levene_result <- leveneTest(petal.length ~ species, data = data)
print("Homogeniczność, petal lenght:")
print(levene_result)

data$log_sepal_length <- log(data$sepal.length)
data$log_petal_length <- log(data$petal.length)

setosa_chart <- ggplot(data[data$species == "setosa", ], aes(x = sepal.length, y = petal.length)) +
  geom_point(color = "black", size = 3) +
  labs(title = "Grupa: Setosa", x = "Sepal Length", y = "Petal Length") +
  theme(plot.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5))

print(setosa_chart)
#ggsave("setosa_chart.pdf", plot = setosa_chart, device = "pdf") # nolint


versicolor_chart <- ggplot(data[data$species == "versicolor", ], aes(x = sepal.length, y = petal.length)) +
  geom_point(color = "black", size = 3) +
  labs(title = "Grupa: Versicolor", x = "Sepal Length", y = "Petal Length") +
  theme(plot.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5))

print(versicolor_chart)
#ggsave("versicolor_chart.pdf", plot = versicolor_chart, device = "pdf") # nolint

virginica_chart <- ggplot(data[data$species == "virginica", ], aes(x = sepal.length, y = petal.length)) +
  geom_point(color = "black", size = 3) +
  labs(title = "Grupa: Virginica", x = "Sepal Length", y = "Petal Length") +
  theme(plot.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5))

print(virginica_chart)
#ggsave("virginica_chart.pdf", plot = virginica_chart, device = "pdf") # nolint

levene_result <- leveneTest(log_sepal_length ~ species, data = data)
print("Homogeniczność, logarytm sepal length:")
print(levene_result)

levene_result <- leveneTest(log_petal_length ~ species, data = data)
print("Homogeniczność, logarytm petal length:")
print(levene_result)

# Test MANOVA
manova_result <- manova(cbind(log_sepal_length, log_petal_length) ~ species, data = data)
print(manova_result)