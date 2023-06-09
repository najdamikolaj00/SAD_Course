# Wczytanie pakietów
# install.packages("readxl")
library(readxl)

# Wczytanie danych z pliku
data <- read_excel("Data/energy.xlsx")

print(colnames(data))

# Usunięcie ostatniego wiersza/podsumowującego
data <- data[-nrow(data), ]

energy <- data$'Billions of Kilowatt-Hours'

# Obliczenie statystyk
mean_value <- mean(energy)
variance <- var(energy)
std_dev <- sd(energy)
median_value <- median(energy)
q1 <- quantile(energy, 0.25)
q3 <- quantile(energy, 0.75)
min_value <- min(energy)
max_value <- max(energy)
iqr <- IQR(energy)
skewness <- sum((energy - mean_value)^3) / (length(energy) * std_dev^3)
kurtosis <- sum((energy - mean_value)^4) / (length(energy) * std_dev^4) - 3

# Wyświetlenie wyników
cat("Średnia z próby:", mean_value, "\n")
cat("Wariancja z próby:", variance, "\n")
cat("Odchylenie standardowe z próby:", std_dev, "\n")
cat("Mediana:", median_value, "\n")
cat("Pierwszy kwartyl:", q1, "\n")
cat("Trzeci kwartyl:", q3, "\n")
cat("Minimum:", min_value, "\n")
cat("Maximum:", max_value, "\n")
cat("Rozstęp międzykwartylowy:", iqr, "\n")
cat("Skośność:", skewness, "\n")
cat("Kurtoza:", kurtosis, "\n")

# Wczytanie pakietu do rysowania wykresów
# install.packages("ggplot2")
library(ggplot2)

binwidth <- IQR(energy) / 4

# Histogram
histogram <- ggplot(data, aes(x = Country)) +
  geom_bar(aes(y = energy), stat = "identity", fill = "steelblue", color = "white") +
  labs(x = "Kraje", y = "Zużycie energii netto (mld kWh)") +
  ggtitle("Zużycie energii netto w Azji (2003)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))


# Wyświetlanie wykresu

# print(histogram)
# ggsave("histogram_energia.pdf", plot = histogram, device = "pdf")


#Pudełko z wąsami
boxplot <- ggplot(data, aes(y = energy)) +
  geom_boxplot(fill = "steelblue", color = "black", width = 0.5, lwd = 1.5, lty = 1) +
  labs(y = "Zużycie energii netto (mld kWh)") +
  ggtitle("Pudełko z wąsami - Zużycie energii netto w Azji (2003)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(),
        plot.title = element_text(hjust = 0.5))

print(boxplot)
ggsave("boxplot_energia.pdf", plot = boxplot, device = "pdf")