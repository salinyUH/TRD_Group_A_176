
# Load dataset
df <- read.csv("tidy_format_co2_emission_dataset.csv")

# Check the structure
str(df)

#print few rows
head(df)

#rename CO2Emission Rate..mt.
names(df)[3] <- "CO2"

#print column names
colnames(df)

# Convert CO₂ emissions column into numeric (important!)
df$CO2 <- as.numeric(df$CO2)

#Data set col Min,max,quartile,Mean,median,mode
summary(df)

# Count how many countries exist in each year
table(df$Year)

# Filter the two years we want
df1990 <- df[df$Year == 1990, ]
df2021 <- df[df$Year == 2021, ]

# Show how many rows we have (to justify sample size)
nrow(df1990)
nrow(df2021)

# Create two groups (1990 vs 2021)

df1990$Group <- "1990"
df2021$Group <- "2021"
combined <- rbind(df1990, df2021)


# Histogram → helps justify use of t-test vs nonparametric test
hist(combined$CO2,
     breaks = 30,
     main = "Distribution of CO₂ Emissions",
     xlab = "CO₂ Emission Rate (mt)",
     col = "lightblue")


# Boxplot → visual comparison of the two years

boxplot(CO2 ~ Group,
        data = combined,
        col = c("skyblue", "orange"),
        main = "CO₂ Emissions in 1990 vs 2021",
        xlab = "Year",
        ylab = "CO₂ Emission Rate (mt)")


# First, try the t-test
t.test(CO2 ~ Group, data = combined)

# If histogram shows data is NOT normal, use non-parametric test:
wilcox.test(CO2 ~ Group, data = combined)



