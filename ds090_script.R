#install.packages("tidyverse")
library(tidyverse)

# Load dataset and keep original names
CO2 <- read.csv("tidy_format_co2_emission_dataset.csv", check.names = FALSE)

# Rename to simple names
colnames(CO2) <- c("country", "year", "CO2")

# Convert CO2 to numeric
CO2$CO2 <- as.numeric(CO2$CO2)

#remove rows with null CO2
CO2 <- CO2 %>% drop_na(CO2)

# Filter two years
CO2_1990 <- CO2 %>%
  filter(year == 1990) %>%
  select(country, CO2_1990 = CO2)

CO2_2021 <- CO2 %>%
  filter(year == 2021) %>%
  select(country, CO2_2021 = CO2)

# Merge & compute differences
paired <- inner_join(CO2_1990, CO2_2021, by = "country") %>%
  mutate(diff = CO2_2021 - CO2_1990)

# BOXPLOT


# HISTOGRAM
png("hist_diff_CO2.png")
hist(paired$diff, breaks=20,
     main="Histogram of CO2 Emission Differences (2021 - 1990)",
     xlab="Change in CO2 Emission (metric tonnes)",
     ylab="Frequency",
     col = "lightblue")
dev.off()

#  Add normal curve
png("hist_diff_normal_curve.png")
hist(paired$diff, freq = FALSE, breaks = 20,
     main = "Histogram with Normal Curve",
     xlab = "Change in CO2 Emission (metric tonnes)")
curve(dnorm(x, mean = mean(paired$diff), sd = sd(paired$diff)),
      col = "red", lwd = 2, add = TRUE)
dev.off()