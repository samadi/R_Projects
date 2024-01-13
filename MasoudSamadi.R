#import datasets
un_countries <- read.csv("UNDeveloped-country-csv.csv")
de_countries <- read.csv("Developed-country-csv.csv")

# first row and last rows of dataset
head(un_countries)
tail(de_countries)

# seperate columns after column 3
columns_to_convert_un <- 3:ncol(un_countries)   
columns_to_convert_de <- 3:ncol(de_countries)  

# Convert columns after column 3 to numeric
un_countries[, columns_to_convert_un] <- apply(un_countries[, columns_to_convert_un], 2, as.numeric)
de_countries[, columns_to_convert_de] <- apply(de_countries[, columns_to_convert_de], 2, as.numeric)


#check for missing values
missing_un_countries <- colnames(un_countries)[colSums(is.na(un_countries)) > 0]
print(missing_un_countries)

missing_de_countries <- colnames(de_countries)[colSums(is.na(de_countries)) > 0]
print(missing_de_countries)

# difine numeric columns
numeric_columns_un <- sapply(un_countries, is.numeric)
numeric_columns_de <- sapply(de_countries, is.numeric)
# Fill  missing values
un_countries[, numeric_columns_un] <- lapply(un_countries[, numeric_columns_un, drop = FALSE], function(x) {
  if (any(is.na(x))) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
  }
  return(x)
})

de_countries[, numeric_columns_de] <- lapply(de_countries[, numeric_columns_de, drop = FALSE], function(x) {
  if (any(is.na(x))) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
  }
  return(x)
})



z_scores_un <- scale(un_countries$GDP_usd)
outliers <- abs(z_scores_un) > 3

print(outliers)


q1 <- quantile(un_countries$GDP_usd, 0.25)
q3 <- quantile(un_countries$GDP_usd, 0.75)
iqr <- q3 - q1
threshold <- 1.5
outliers <- un_countries$GDP_usd < (q1 - threshold * iqr) | un_countries$GDP_usd > (q3 + threshold * iqr)
print(outliers)


numeric_columns_un <- sapply(un_countries, is.numeric)

# box plot
par(mfrow = c(2, ceiling(sum(numeric_columns_un)/2))) 
for (col in names(un_countries)[numeric_columns_un]) {
  boxplot(un_countries[[col]], main = col, col = "skyblue", border = "black", horizontal = TRUE)
}
par(mfrow = c(1, 1))

# transforming outliers

numeric_columns_un <- sapply(un_countries[, 4:ncol(un_countries)], is.numeric)
un_countries_transformed <- un_countries
un_countries_transformed[, numeric_columns_un] <- lapply(un_countries_transformed[, numeric_columns_un], function(col) {
  if (all(is.numeric(col) & col >= 0 & is.finite(col))) {
    sqrt(col)
  } else {
    col  
  }
})


numeric_columns_de <- sapply(de_countries[, 4:ncol(de_countries)], is.numeric)
de_countries_transformed <- de_countries
de_countries_transformed[, numeric_columns_de] <- lapply(de_countries_transformed[, numeric_columns_de], function(col) {
  if (all(is.numeric(col) & col >= 0 & is.finite(col))) {
    sqrt(col)
  } else {
    col  
  }
})


#to see summary of dataset

summary(de_countries_transformed)

# Create the plot to compare GDP in 2020



library(tidyverse)


library(dplyr)

un_countries_2020 <- un_countries %>% 
  dplyr::filter(Year == 2020) %>%
  dplyr::mutate(Source = "UN Countries") %>%
  dplyr::select(Country_Name, GDP_per_capita_usd, Source)

de_countries_2020 <- de_countries %>%
  dplyr::filter(Year == 2020) %>% 
  mutate(Source = "DE Countries") %>%
  select(Country_Name, GDP_per_capita_usd, Source)

combined_df <- dplyr::bind_rows(un_countries_2020, de_countries_2020) %>%
  arrange(GDP_per_capita_usd)

library(ggplot2)

ggplot(combined_df, aes(x = Country_Name, y = GDP_per_capita_usd, color = Source)) +
  geom_point() +
  labs(
    x = "Country",
    y = "GDP Per Capita USD",
    title = "Country GDP Per Capita in 2020",
    color = "Source"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



afghanistan <- un_countries %>%
  filter(Country_Name == "Afghanistan")

# Plot Access_to_electricity over time
ggplot(data = afghanistan, aes(x = Year, y = Access_to_electricity)) + 
  geom_line() +
  labs(title = "Electricity Access in Afghanistan 2011-2020",
       x = "Year",
       y = "Access to Electricity (%)")






# Filter to only Afghanistan and Sudan
two_countries <- un_countries %>%
  filter(Country_Name %in% c("Afghanistan", "Sudan"))

# Plot line chart for each country  
ggplot(data = two_countries, aes(x = Year, y = Access_to_electricity, color = Country_Name)) +
  geom_line() +
  labs(
    title = "Electricity Access in Afghanistan & Sudan",
    x = "Year",
    y = "Access to Electricity (%)"
  )








angola <- un_countries %>% 
  filter(Country_Name == "Angola")

germany <- de_countries %>% 
  filter(Country_Name == "Germany")

countries <- bind_rows(angola, germany)

ggplot(data = countries, aes(x = Year, y = Inflation, color=Country_Name)) +
  geom_line() + 
  labs(
    title = "Inflation in Angola and Germany",
    x = "Year",
    y = "Inflation (%)"
  )


un_avg <- un_countries %>%
  group_by(Year) %>% 
  summarize(avg_life_exp = mean(Life_expectancy_birth_total)) %>%
  mutate(Source = "UN Countries")

de_avg <- de_countries %>% 
  group_by(Year) %>%
  summarize(avg_life_exp = mean(Life_expectancy_birth_total)) %>%
  mutate(Source = "DE Countries")

combined_avg <- bind_rows(un_avg, de_avg)

ggplot(combined_avg, aes(Year, avg_life_exp, color = Source)) +
  geom_line() +
  scale_color_manual(values = c("UN Countries"="blue", "DE Countries"="red")) + 
  labs(title = "Avg Life Expectancy Over Time",  
       x = "Year",
       y = "Avg Life Expectancy")




un_avg <- un_countries %>%
  group_by(Year) %>%
  summarize(avg_gdp_grow = mean(GDP_grow)) %>%
  mutate(Source = "UN Countries")

de_avg <- de_countries %>% 
  group_by(Year) %>%
  summarize(avg_gdp_grow = mean(GDP_grow)) %>%
  mutate(Source = "DE Countries")

# Combine the datasets
combined_avg <- bind_rows(un_avg, de_avg)

# Plot averages over time with a legend
ggplot(combined_avg, aes(x = Year, y = avg_gdp_grow, color = Source)) +
  geom_line() +
  labs(title = "Average GDP Growth over Time",
       x = "Year",
       y = "Average GDP Growth",
       color = "Dataset") +
  scale_color_manual(values=c("UN Countries"="blue", "DE Countries"="red"))




library(scales)

ggplot(data = un_countries, aes(x = Year, y = GDP_usd)) +
  geom_line() +
  facet_wrap(~Country_Name) +
  scale_y_continuous(labels = label_number(scale = 0.000001))


ggplot(data = de_countries %>% filter(Year == 2020),
       aes(x = Country_Name, y = GDP_usd)) +
  scale_y_continuous(labels = label_number(scale = 0.000001))+
  geom_col()




data = bind_rows(
  de_countries %>% mutate(Source = "DE Countries"), 
  un_countries %>% mutate(Source = "UN Countries")
)

# Scatter plot
ggplot(data, aes(Year, Mortality_rate_infant, color = Source)) +
  geom_point() +
  scale_color_manual(values = c("DE Countries" = "green", "UN Countries" = "red"))


plot(un_countries$Access_to_electricity,un_countries$Life_expectancy_birth_total)






library(dplyr)
library(e1071)
calculate_stats <- function(data, column) {
  column_data <- data[[column]]
  stats <- tibble(
    Feature = column,
    Mean = format(mean(column_data, na.rm = TRUE), nsmall = 2),
    Median = format(median(column_data, na.rm = TRUE), nsmall = 2),
    SD = format(sd(column_data, na.rm = TRUE), nsmall = 2),
    Min = format(min(column_data, na.rm = TRUE), nsmall = 2),
    Max = format(max(column_data, na.rm = TRUE), nsmall = 2),
    Skewness = skewness(column_data, na.rm = TRUE),
    Kurtosis = kurtosis(column_data, na.rm = TRUE))
  return(stats)
}
stats_df_un <- do.call(rbind, lapply(cols, calculate_stats, data = un_countries)) 
print(stats_df_un)
stats_df_de <- do.call(rbind, lapply(cols, calculate_stats, data = de_countries))
print(stats_df_de)


# Correlation


library(tidyverse)
library(ggcorrplot)
corr_matrix_un <- cor(select_if(un_countries, is.numeric)) 
print(round(corr_matrix_un, 2))
corr_matrix_de <- cor(select_if(de_countries, is.numeric)) 
print(round(corr_matrix_de, 2))

ggcorrplot(corr_matrix_un, 
           hc.order = TRUE,  
           type = "lower",
           lab = TRUE,
           lab_size = 1,  # Adjust the label size here
           colors = c("red", "white", "blue"),
           title = "Correlation Matrix of Variables undeveloped countries")


un_countries <- un_countries %>% 
  mutate(Access_electricity_norm = as.numeric(scale(Access_to_electricity)))

un_countries <- un_countries %>% 
  mutate(Life_expectancy_norm = as.numeric(scale(Life_expectancy_birth_total)))

# Check histograms
hist(un_countries$Access_electricity_norm)
hist(un_countries$Life_expectancy_norm)


cor(un_countries$Access_electricity_norm, un_countries$Life_expectancy_norm, method = "pearson")



cor_result <- cor(un_countries$Access_electricity_norm, un_countries$Life_expectancy_norm, method = "pearson")
print(paste("The Pearson correlation coefficient is:", cor_result))


ggplot(un_countries, aes(x = Access_electricity_norm, y = Life_expectancy_norm)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title="Correlation between Access to Electricity and Life Expectancy",
       x = "Access to Electricity",
       y = "Life Expectancy")




# Spearman correlation 
spearman_corr <- cor(de_countries$Trade_per_GDP, de_countries$GDP_per_capita_usd, method = "spearman")

print(paste0("The Spearman correlation coefficient between Trade_per_GDP and GDP_per_capita_usd is: ", round(spearman_corr, 3)))

# Scatterplot
ggplot(de_countries, aes(x = Trade_per_GDP, y = GDP_per_capita_usd)) +
  geom_point() +
  labs(title = "Relationship between Trade_per_GDP and GDP_per_capita_usd",
       x = "Trade_per_GDP",  
       y = "GDP_per_capita_usd")

library(dplyr)
library(ggplot2)
# Hypothesis 1: GDP per capita vs Life Expectancy
data_hypothesis1 <- na.omit(de_countries[, c("GDP_per_capita_usd", "Life_expectancy_birth_total")])
cor_test_h1 <- cor.test(data_hypothesis1$GDP_per_capita_usd, 
                        data_hypothesis1$Life_expectancy_birth_total,
                        method = "pearson")
print(cor_test_h1)


# Hypothesis 2: Access to electricity vs mortality

data_for_analysis <- un_countries %>%
  select(Access_to_electricity, Mortality_rate_infant) %>%
  na.omit()

data_for_analysis$Mortality_rate_infant <- as.numeric(as.character(data_for_analysis$Mortality_rate_infant))

# Pearson Correlation Test
correlation_test <- cor.test(data_for_analysis$Access_to_electricity, 
                             data_for_analysis$Mortality_rate_infant,
                             method = "pearson")

print(correlation_test)

# Hypothesis 3: Relationship between inflation and deposit interest rate

data_hyp3 <- de_countries %>%  
  select(Inflation, Deposit_interest_rate) %>% 
  na.omit()
data_hyp3$Inflation <- as.numeric(as.character(data_hyp3$Inflation))
corr_test_hyp3 <- cor.test(data_hyp3$Inflation, 
                           data_hyp3$Deposit_interest_rate,
                           method="pearson")

print("Hypothesis 3 Correlation Results")
print(corr_test_hyp3)



# T-test
library(dplyr)

data_2011 <- subset(de_countries, Year == 2011)
data_2020 <- subset(de_countries, Year == 2020)
t_test_result <- t.test(data_2011$GDP_grow, data_2020$GDP_grow)
print(t_test_result)


# one-Way ANOVA
anova_result <- aov(Life_expectancy_birth_total ~ Year, data = un_countries)
summary(anova_result)




library(tidyverse) 

data <- de_countries %>% 
  select(Mortality_rate_infant, Life_expectancy_birth_total)

# Scatterplot 
ggplot(data, aes(x = Mortality_rate_infant, y = Life_expectancy_birth_total)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", se = FALSE) +   
  ggtitle("Mortality rate vs Life Expectancy") +
  xlab("mortality rate") + 
  ylab("Life Expectancy (Years)")

#linear regression
model <- lm(Mortality_rate_infant ~ Life_expectancy_birth_total, data = un_countries)
summary(model)


# multiple regression
multi_model <- lm(Inflation ~ GDP_grow + Trade_per_GDP + Unemployment_total, data = de_countries)
summary(multi_model)









# time series
switzerland_data <- de_countries %>% filter(Country_Name == "Switzerland")
library(ggplot2)
ggplot(switzerland_data, aes(x = Year)) +
  geom_line(aes(y = GDP_usd), color = "blue") +
  ggtitle("GDP Over Time in Switzerland") +
  ylab("GDP (USD)") +
  theme_minimal()


ggplot(switzerland_data, aes(x = Year)) +
  geom_line(aes(y = Unemployment_female, color = "Female"), size = 1.5) +
  geom_line(aes(y = Unemployment_male, color = "Male"), size = 1.5) +
  geom_line(aes(y = Unemployment_total, color = "Total"), size = 1.5) +
  ggtitle("unemployment between 2011 to 2020 in Switzerland") +
  ylab("total unemployment") +
  theme_minimal() +
  scale_color_manual(values = c("Female" = "red", "Male" = "green", "Total" = "blue")) +
  labs(color = "Gender")


ggplot(switzerland_data, aes(x = Year)) +
  geom_line(aes(y = Life_expectancy_birth_total), color = "blue") +
  ggtitle("life expectancy between 2011 to 2020 in Switzerland") +
  ylab("life Expectancy ") +
  theme_minimal()


de_countries$Year <- as.numeric(as.character(de_countries$Year), na.rm = TRUE)
de_countries_ts <- ts(de_countries$GDP_usd, start = min(de_countries$Year), end = max(de_countries$Year), frequency = 1)
plot.ts(de_countries_ts, main = " GDP plot time Series  ", ylab = "GDP (USD)")
un_countries$Year <- as.numeric(un_countries$Year)  
ts_un_countries <- ts(un_countries[, -1], start = 2011, end = 2020, frequency = 1)
plot.ts(ts_un_countries[, "GDP_usd"], main = "GDP (USD) time series plot  ", ylab = "GDP (USD)", col = "blue")
plot.ts(ts_un_countries[, "GDP_per_capita_usd"], main = "GDP Per Capita time Series Plot  ", ylab = "GDP Per Capita (USD)", col = "green")
plot.ts(ts_un_countries[, "Inflation"], main = "inflation time series plot - ", ylab = "Inflation", col = "red")
plot.ts(ts_un_countries[, "Unemployment_total"], main = "total unemployment time series Plot - ", ylab = "Unemployment Rate", col = "purple")


un_countries$Year <- as.numeric(un_countries$Year)  
ts_data <- ts(un_countries[, -1], start = 2011, end = 2020, frequency = 1)
un_countries_forecasts <- HoltWinters(ts_data, beta = TRUE, gamma = FALSE)
un_countries_forecasts

gdp_series <- ts(un_countries$GDP_usd, start = c(2011), frequency = 1)
gdp_forecasts <- HoltWinters(gdp_series, beta = TRUE, gamma = FALSE)
print(gdp_forecasts)
library(forecast)
plot(forecast(gdp_forecasts, h = 5)) 


residuals <- residuals(gdp_forecasts)
hist(residuals, main = "histogram of forecast errors", xlab = "forecast error", col = "lightblue", border = "black")

time_series_data <- ts(de_countries$GDP_per_capita_usd, start=c(2011), frequency=1)
library(tseries)
adf.test(time_series_data)
acf(time_series_data, lag.max=20)
acf(time_series_data, lag.max=20, plot=FALSE)

fit <- auto.arima(time_series_data)
summary(fit)
forecast_length <- 5
future_forecast <- forecast(fit, h=forecast_length)
plot(future_forecast)







































