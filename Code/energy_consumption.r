# install.packages("tseries")
# install.packages("readxl")
# install.packages("ggplot2")

library(readxl)
library(ggplot2)
library(tseries)

data <- read_excel("Data/energy.xlsx")

print(colnames(data))
data <- data[-nrow(data), ]

energy <- data$"Billions of Kilowatt-Hours"

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

cat("Mean value:", mean_value, "\n")
cat("Variance:", variance, "\n")
cat("Standard devation:", std_dev, "\n")
cat("Median:", median_value, "\n")
cat("Q1:", q1, "\n")
cat("Q3:", q3, "\n")
cat("Minimum:", min_value, "\n")
cat("Maximum:", max_value, "\n")
cat("IQR:", iqr, "\n")
cat("Skewness:", skewness, "\n")
cat("Kurtosis:", kurtosis, "\n")


binwidth <- IQR(energy) / 4

histogram <- ggplot(data, aes(x = Country)) +
  geom_bar(aes(y = energy), stat = "identity", fill = "steelblue", color = "white") + 
  labs(x = "Kraje", y = "Zużycie energii netto (mld kWh)") +
  ggtitle("Zużycie energii netto w Azji (2003)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

print(histogram)
#ggsave("histogram_energia.pdf", plot = histogram, device = "pdf")

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
#ggsave("boxplot_energia.pdf", plot = boxplot, device = "pdf") 


shapiro_result <- shapiro.test(energy)
print(shapiro_result)

ks_result <- ks.test(energy, "pnorm")
print(ks_result)

jarque_result <- jarque.bera.test(energy)
print(jarque_result)


transformed_log <- log(energy)
print(transformed_log)

histogram <- ggplot(data, aes(x = Country)) +
  geom_bar(aes(y = transformed_log), stat = "identity", fill = "steelblue", color = "white") +  
  labs(x = "Kraje", y = "Zużycie energii netto (mld kWh)") +
  ggtitle("Zużycie energii netto w Azji (2003)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

#print(histogram)  
#ggsave("histogram_energia_log.pdf", plot = histogram, device = "pdf")  

boxplot <- ggplot(data, aes(y = transformed_log)) +
  geom_boxplot(fill = "steelblue", color = "black", width = 0.5, lwd = 1.5, lty = 1) +  
  labs(y = "Zużycie energii netto (mld kWh)") +
  ggtitle("Pudełko z wąsami - Zużycie energii netto w Azji (2003)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),  
        axis.line = element_line(),  
        plot.title = element_text(hjust = 0.5))

print(boxplot)  
#ggsave("boxplot_energia_log.pdf", plot = boxplot, device = "pdf")  

shapiro_result <- shapiro.test(transformed_log)
jarque_result <- jarque.bera.test(transformed_log)
ks_result <- ks.test(transformed_log, "pnorm")

print("Przekształcenie logarytmiczne:")
print(shapiro_result)
print(jarque_result)
print(ks_result)

transformed_sqrt <- sqrt(energy)

histogram <- ggplot(data, aes(x = Country)) +
  geom_bar(aes(y = transformed_sqrt), stat = "identity", fill = "steelblue", color = "white") +  
  labs(x = "Kraje", y = "Zużycie energii netto (mld kWh)") +
  ggtitle("Zużycie energii netto w Azji (2003)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

print(histogram)  
#ggsave("histogram_energia_sqrt.pdf", plot = histogram, device = "pdf")  

boxplot <- ggplot(data, aes(y = transformed_sqrt)) +
  geom_boxplot(fill = "steelblue", color = "black", width = 0.5, lwd = 1.5, lty = 1) +  
  labs(y = "Zużycie energii netto (mld kWh)") +
  ggtitle("Pudełko z wąsami - Zużycie energii netto w Azji (2003)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),  
        axis.line = element_line(),  
        plot.title = element_text(hjust = 0.5))

print(boxplot)  
#ggsave("boxplot_energia_sqrt.pdf", plot = boxplot, device = "pdf")  

shapiro_result <- shapiro.test(transformed_sqrt)
jarque_result <- jarque.bera.test(transformed_sqrt)
ks_result <- ks.test(transformed_sqrt, "pnorm")

print("Przekształcenie pierwiastka kwadratowego:")
print(shapiro_result)
print(jarque_result)
print(ks_result)

transformed_cbrt <- energy^(1/3)

histogram <- ggplot(data, aes(x = Country)) +
  geom_bar(aes(y = transformed_cbrt), stat = "identity", fill = "steelblue", color = "white") +  
  labs(x = "Kraje", y = "Zużycie energii netto (mld kWh)") +
  ggtitle("Zużycie energii netto w Azji (2003)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

print(histogram)  
#ggsave("histogram_energia_cbrt.pdf", plot = histogram, device = "pdf")  

boxplot <- ggplot(data, aes(y = transformed_cbrt)) +
  geom_boxplot(fill = "steelblue", color = "black", width = 0.5, lwd = 1.5, lty = 1) +  
  labs(y = "Zużycie energii netto (mld kWh)") +
  ggtitle("Pudełko z wąsami - Zużycie energii netto w Azji (2003)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),  
        axis.line = element_line(),  
        plot.title = element_text(hjust = 0.5))

print(boxplot)  
#ggsave("boxplot_energia_cbrt.pdf", plot = boxplot, device = "pdf")  

shapiro_result <- shapiro.test(transformed_cbrt)
jarque_result <- jarque.bera.test(transformed_cbrt)
ks_result <- ks.test(transformed_cbrt, "pnorm")

print("Przekształcenie pierwiastka sześciennego:")
print(shapiro_result)
print(jarque_result)
print(ks_result)
