# Wczytanie bibliotek i instalacja bibliotek
# install.packages("dplyr")
# install.packages("car")
library(dplyr)
library(car)

data <- read.csv("Data/gdp_per_capita.csv", header = TRUE)
print(head(data))

# Sprawdzenie założeń - normalność rozkładu
# shapiro_result <- shapiro.test(data$gdpPercap)
# print(shapiro_result) # Brak rozkładu normalnego

# Podział danych według kontynentów
continent_data <- split(data$gdpPercap, data$continent)

# Przeprowadzenie testu Kołmogorowa-Smirnowa dla każdego kontynentu
results <- lapply(continent_data, function(x) {
  ks.test(x, "pnorm", mean = mean(x), sd = sd(x))
})

# Wyświetlenie wyników
for (i in 1:length(results)) {
  cat("Kontynent:", names(results[i]), "\n")
  print(results[[i]])
  cat("\n")
}

# Sprawdzenie homogeniczności wariancji
levene_result <- leveneTest(gdpPercap ~ continent, data = data)
print(levene_result) # Brak homogeniczności 

kruskal_result <- kruskal.test(gdpPercap ~ continent, data = data)
print(kruskal_result)