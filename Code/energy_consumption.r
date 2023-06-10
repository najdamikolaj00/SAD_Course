# Wczytanie pakietów
#if(!require(tseries)){install.packages("tseries", "readxl", "ggplot2")} # nolint
library(readxl)
library(ggplot2)
library(tseries)

# Wczytanie danych z pliku
data <- read_excel("Data/energy.xlsx")

print(colnames(data))

# Usunięcie ostatniego wiersza/podsumowującego
data <- data[-nrow(data), ]

energy <- data$"Billions of Kilowatt-Hours"

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


binwidth <- IQR(energy) / 4

# Histogram

# histogram <- ggplot(data, aes(x = Country)) +
#   geom_bar(aes(y = energy), stat = "identity", fill = "steelblue", color = "white") + # nolint
#   labs(x = "Kraje", y = "Zużycie energii netto (mld kWh)") +
#   ggtitle("Zużycie energii netto w Azji (2003)") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         plot.title = element_text(hjust = 0.5))

# print(histogram) # nolint
# ggsave("histogram_energia.pdf", plot = histogram, device = "pdf") # nolint


#Pudełko z wąsami

# boxplot <- ggplot(data, aes(y = energy)) +
#   geom_boxplot(fill = "steelblue", color = "black", width = 0.5, lwd = 1.5, lty = 1) + # nolint
#   labs(y = "Zużycie energii netto (mld kWh)") +
#   ggtitle("Pudełko z wąsami - Zużycie energii netto w Azji (2003)") +
#   theme_minimal() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), # nolint
#         axis.line = element_line(), # nolint
#         plot.title = element_text(hjust = 0.5))

# print(boxplot) # nolint
# ggsave("boxplot_energia.pdf", plot = boxplot, device = "pdf") # nolint


# Testy normalności
# shapiro_result <- shapiro.test(energy)
# print(shapiro_result)

# ks_result <- ks.test(energy, "pnorm")
# print(ks_result)

# jarque_result <- jarque.bera.test(energy)
# print(jarque_result)


# Przekształcenie logarytmiczne
transformed_log <- log(energy)
print(transformed_log)

histogram <- ggplot(data, aes(x = Country)) +
  geom_bar(aes(y = transformed_log), stat = "identity", fill = "steelblue", color = "white") + # nolint
  labs(x = "Kraje", y = "Zużycie energii netto (mld kWh)") +
  ggtitle("Zużycie energii netto w Azji (2003)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

#print(histogram) # nolint
ggsave("histogram_energia_log.pdf", plot = histogram, device = "pdf") # nolint

boxplot <- ggplot(data, aes(y = transformed_log)) +
  geom_boxplot(fill = "steelblue", color = "black", width = 0.5, lwd = 1.5, lty = 1) + # nolint
  labs(y = "Zużycie energii netto (mld kWh)") +
  ggtitle("Pudełko z wąsami - Zużycie energii netto w Azji (2003)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), # nolint
        axis.line = element_line(), # nolint
        plot.title = element_text(hjust = 0.5))

print(boxplot) # nolint
ggsave("boxplot_energia_log.pdf", plot = boxplot, device = "pdf") # nolint

# Weryfikacja normalności dla przekształcenia logarytmicznego
shapiro_result <- shapiro.test(transformed_log)
jarque_result <- jarque.bera.test(transformed_log)
ks_result <- ks.test(transformed_log, "pnorm")

print("Przekształcenie logarytmiczne:")
print(shapiro_result)
print(jarque_result)
print(ks_result)

# Przekształcenie pierwiastka kwadratowego
transformed_sqrt <- sqrt(energy)

histogram <- ggplot(data, aes(x = Country)) +
  geom_bar(aes(y = transformed_sqrt), stat = "identity", fill = "steelblue", color = "white") + # nolint
  labs(x = "Kraje", y = "Zużycie energii netto (mld kWh)") +
  ggtitle("Zużycie energii netto w Azji (2003)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

#print(histogram) # nolint
ggsave("histogram_energia_sqrt.pdf", plot = histogram, device = "pdf") # nolint

boxplot <- ggplot(data, aes(y = transformed_sqrt)) +
  geom_boxplot(fill = "steelblue", color = "black", width = 0.5, lwd = 1.5, lty = 1) + # nolint
  labs(y = "Zużycie energii netto (mld kWh)") +
  ggtitle("Pudełko z wąsami - Zużycie energii netto w Azji (2003)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), # nolint
        axis.line = element_line(), # nolint
        plot.title = element_text(hjust = 0.5))

print(boxplot) # nolint
ggsave("boxplot_energia_sqrt.pdf", plot = boxplot, device = "pdf") # nolint

# Weryfikacja normalności dla przekształcenia pierwiastka kwadratowego
shapiro_result <- shapiro.test(transformed_sqrt)
jarque_result <- jarque.bera.test(transformed_sqrt)
ks_result <- ks.test(transformed_sqrt, "pnorm")

print("Przekształcenie pierwiastka kwadratowego:")
print(shapiro_result)
print(jarque_result)
print(ks_result)

# Przekształcenie pierwiastka sześciennego
transformed_cbrt <- energy^(1/3)

histogram <- ggplot(data, aes(x = Country)) +
  geom_bar(aes(y = transformed_cbrt), stat = "identity", fill = "steelblue", color = "white") + # nolint
  labs(x = "Kraje", y = "Zużycie energii netto (mld kWh)") +
  ggtitle("Zużycie energii netto w Azji (2003)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

#print(histogram) # nolint
ggsave("histogram_energia_cbrt.pdf", plot = histogram, device = "pdf") # nolint

boxplot <- ggplot(data, aes(y = transformed_cbrt)) +
  geom_boxplot(fill = "steelblue", color = "black", width = 0.5, lwd = 1.5, lty = 1) + # nolint
  labs(y = "Zużycie energii netto (mld kWh)") +
  ggtitle("Pudełko z wąsami - Zużycie energii netto w Azji (2003)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), # nolint
        axis.line = element_line(), # nolint
        plot.title = element_text(hjust = 0.5))

print(boxplot) # nolint
ggsave("boxplot_energia_cbrt.pdf", plot = boxplot, device = "pdf") # nolint

# Weryfikacja normalności dla przekształcenia pierwiastka sześciennego
shapiro_result <- shapiro.test(transformed_cbrt)
jarque_result <- jarque.bera.test(transformed_cbrt)
ks_result <- ks.test(transformed_cbrt, "pnorm")

print("Przekształcenie pierwiastka sześciennego:")
print(shapiro_result)
print(jarque_result)
print(ks_result)
