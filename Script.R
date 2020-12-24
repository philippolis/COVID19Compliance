# Loading the libraries ----
library(dplyr)
library(ggplot2)
library(scales)
library(lme4)
library(broom.mixed)
library(plotly)
library(reshape2)

# Data Wrangling -----
## Adding the Policy Data -----
### Read in the Data
policy_data <- read.csv("PolicyData.csv")

### convert Date column to date format
policy_data$Date = as.Date(as.character(policy_data$Date), format = "%Y%m%d")

### create dataframe with only national level data
policy_data_countries <- policy_data %>% filter(Jurisdiction == "NAT_TOTAL")

### create dataframe with only national level of Germany, France, UK, Spain, Italy
policy_data_Europe <- policy_data_countries %>% filter(CountryCode == "DEU" |
                                                         CountryCode == "ITA" |
                                                         CountryCode == "FRA" |
                                                         CountryCode == "ESP" |
                                                         CountryCode == "GBR" |
                                                         CountryCode == "NLD" |
                                                         CountryCode == "BEL" |
                                                         CountryCode == "POL" |
                                                         CountryCode == "SWE" |
                                                         CountryCode == "AUT" |
                                                         CountryCode == "IRL" |
                                                         CountryCode == "DNK")


## Adding the Mobility Data ----
### Reading in the data
mobility_data <- read.csv("MobilityData.csv")

### Add a column with an average of the change in sectors, excluding residential and parks

mobility_data <- mobility_data %>% mutate(avg_change = rowMeans(.[,c(6, 7, 9, 10)]))

### force recognition of the date format
mobility_data$date = as.Date(mobility_data$date, format = "%Y-%m-%d")

### Filter so that only country-level data remains
mobility_data_countries <- mobility_data %>% filter(sub.region.1 == "Total" & sub.region.2 == "Total")

### Create mobility data frame for Europe
mobility_data_Europe <- mobility_data_countries %>% filter(country == "Germany" |
                                                             country == "Italy" |
                                                             country == "France" |
                                                             country == "United Kingdom" |
                                                             country == "Spain" |
                                                             country == "Netherlands" |
                                                             country == "Belgium" |
                                                             country == "Poland" |
                                                             country == "Sweden" |
                                                             country == "Austria" |
                                                             country == "Denmark" |
                                                             country == "Ireland")

## Merging the Data into policyMobility_data_Europe ----
### renaming the date columns to match
names(mobility_data_Europe)[names(mobility_data_Europe)=="date"] <- "Date"
names(mobility_data_Europe)[names(mobility_data_Europe)=="country"] <- "CountryName"
names(mobility_data_Europe)[names(mobility_data_Europe)=="avg_change"] <- "MobilityIndex"

### Only selecting the columns that are interesting
mobility_data_Europe_small <- mobility_data_Europe %>% select(Date, CountryName, MobilityIndex)
policy_data_Europe_small <- policy_data_Europe %>% select(Date, CountryName, StringencyIndex)

### Merging policy and mobility data on date
policyMobility_data_Europe = mobility_data_Europe_small %>% 
  inner_join(policy_data_Europe_small, by = c("Date", "CountryName"))


## Adding a column with Compliance-index ----

policyMobility_data_Europe$ComplianceIndex <- 100 - (policyMobility_data_Europe$StringencyIndex + policyMobility_data_Europe$MobilityIndex)

### Rescaling the Compliance-index to assume values between 0 and 100

policyMobility_data_Europe$ComplianceIndex <- rescale(policyMobility_data_Europe$ComplianceIndex, to = c(0, 100))


# Explorative Data Visualization ----
## Create a Boxplot of Mobility by Country ----
ggplot(data = policyMobility_data_Europe, aes(x = CountryName, y = MobilityIndex)) +
  geom_boxplot()

## Create a Boxplot of Compliance by Country ----
ggplot(data = policyMobility_data_Europe, aes(x = CountryName, y = ComplianceIndex)) +
  geom_boxplot() +
  xlab("Country") +
  ylab("Policy Compliance Index") +
  ggtitle("Policy Compliance in Europe") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 

## Create a faceted plot with Stringency and Mobility for European Countries ----

ggplot(data = policyMobility_data_Europe) +
  geom_line(aes(x = Date, y = StringencyIndex), color = "black", linetype = "longdash", size = 0.5) +
  geom_smooth(aes(x = Date, y = -1*MobilityIndex), method = "loess", span = 1/10, 
              se = FALSE, size = 0.5, color = "black", linetype = "solid") +
  facet_wrap(~CountryName) +
  xlab("Month") +
  scale_x_date(breaks = date_breaks("3 months"),
               labels = date_format("%b\n%Y")) +
  scale_y_reverse(name = "Policy Stringency Index (dashed line)",
                  sec.axis = sec_axis(~.*-1, name = "Mobility %-change vs Baseline (solid line)")) +
  theme(
    axis.title.x = element_text(vjust = 0.5),
    axis.title.y = element_text(color = "black", size=12, vjust = 3),
    axis.title.y.right = element_text(color = "black", size=12, vjust = 3)
  ) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  ggtitle("Development of Policy Stringency and Mobility Reduction in Europe")

## Create a faceted plot with Compliance for European Countries ----
ggplot(policyMobility_data_Europe) +
  geom_smooth(aes(x = Date, y = ComplianceIndex), method = "loess", se = FALSE, span = 1/10, 
              color = "black", linetype = "solid", size = 0.1) +
  geom_smooth(aes(x = Date, y = ComplianceIndex), method = "lm", se = FALSE, 
              color = "black", linetype = "solid", size = 1) + 
  facet_wrap(~CountryName) +
  xlab("Month") +
  scale_x_date(breaks = date_breaks("3 months"),
               labels = date_format("%b\n%Y")) +
  scale_y_continuous(name = "Policy Compliance Index") +
  theme(
    axis.title.y = element_text(color = "black", size=12, vjust = 3),
    axis.title.x = element_text(size=12, vjust = 0.5)
    ) +
  ggtitle("Policy Compliance in Europe Across Time")

# Modelling ----

## Non-hierarchical model ----
lm(data = policyMobility_data_Europe, MobilityIndex ~ StringencyIndex + CountryName + Date)

summary(lm(data = policyMobility_data_Europe, MobilityIndex ~ StringencyIndex + CountryName + Date))

### Visualising different predicted intercepts and different slopes
ggplot(data = augment(lm(data = policyMobility_data_Europe, 
                         MobilityIndex ~ Date + CountryName + StringencyIndex)),
       aes(x = Date, y = MobilityIndex, color = CountryName))  +
  geom_smooth(aes(y = .fitted), method = "lm", se = FALSE)

ggplot(data = augment(lm(data = policyMobility_data_Europe, 
                         MobilityIndex ~ Date + CountryName + StringencyIndex)))  +
  geom_line(aes(x = Date, y = .fitted, color = CountryName))


### Visualization of the non-hierarchical model ----

#### Fit model

model_out <- lm(data = policyMobility_data_Europe, MobilityIndex ~ StringencyIndex + Date)

#### predict over sensible grid of values
unique_stringency <- unique(policyMobility_data_Europe$StringencyIndex)
unique_date <- unique(policyMobility_data_Europe$Date)
grid <- with(policyMobility_data_Europe, expand.grid(unique_stringency, unique_date))
d <- setNames(data.frame(grid), c("StringencyIndex", "Date"))
vals <- predict(model_out, newdata = d)

#### form matrix and give to plotly
model_out <- matrix(vals, nrow = length(unique(d$StringencyIndex)), ncol = length(unique(d$Date)))

model_out_plot <- plot_ly() %>% 
  add_surface(x = ~unique_date, y = ~unique_stringency, z = ~model_out, colorscale = list(c(0, 1), c("green", "yellow"))) %>% 
  add_markers(data = policyMobility_data_Europe, z = ~MobilityIndex, x = ~Date, y = ~StringencyIndex, color = ~CountryName, opacity = 0.6, size = 0.1) %>%
  layout(scene = list(xaxis = list(title = 'Date'),
                      yaxis = list(title = 'Stringency Index'),
                      zaxis = list(title = 'Mobility Index')))

hide_colorbar(model_out_plot)

#### Visualising the linear model in 3D ----

##### Scatterplot for Europe
plot_ly(data = policyMobility_data_Europe, z = ~MobilityIndex, x = ~Date, y = ~StringencyIndex, color = ~CountryName, opacity = 0.6) %>%
  add_markers(size=0.1)

##### Scatterplot for Austria
plot_ly(data = subset(policyMobility_data_Europe, CountryName == "Austria"), z = ~MobilityIndex, x = ~Date, y = ~StringencyIndex, opacity = 0.6) %>%
  add_markers(size=0.1)

##### Line plot for Europe
plot_ly(policyMobility_data_Europe, x = ~Date, y = ~StringencyIndex, z = ~MobilityIndex, color = ~CountryName, type = 'scatter3d', mode = 'lines',
               opacity = 0.6, line = list(width = 3, reverscale = FALSE))

## How does the mobility change over time in Europe? - I do understand this model ----
lmer_out <- lmer(data = policyMobility_data_Europe, MobilityIndex ~ StringencyIndex + Date + (Date | CountryName))

### Looking at the results
summary(lmer(data = policyMobility_data_Europe, MobilityIndex ~ StringencyIndex + Date + (Date | CountryName)))

### extract out the fixed effect for date
fixef(lmer(data = policyMobility_data_Europe, MobilityIndex ~ StringencyIndex + Date + (Date | CountryName)))

### extract out the random effect for country
ranef(lmer(data = policyMobility_data_Europe, MobilityIndex ~ StringencyIndex + Date + (Date | CountryName)))

## How does the Compliance change over time in Europe? This model has not enough independent variables ----

lmer_out <- lmer(data = policyMobility_data_Europe, ComplianceIndex ~ Date + (Date | CountryName))

### Looking at the results
summary(lmer(data = policyMobility_data_Europe, ComplianceIndex ~ Date + (Date | CountryName)))

### extract out the fixed effect for date
fixef(lmer(data = policyMobility_data_Europe, ComplianceIndex ~ Date + (Date | CountryName)))

### extract out the random effect for country
ranef(lmer(data = policyMobility_data_Europe, ComplianceIndex ~ Date + (Date | CountryName)))


# Communicating the Results ----

## Plot of Compliance per Country

### Extract out the fixed-effect slope for Date
Date_slope <- fixef(lmer_out)['Date']

### Extract out the random-effect slopes for Country
Country_slope <- ranef(lmer_out)$CountryName

### Create a new column for the slope
Country_slope$slope <- Country_slope$Date + Date_slope

### Use the row names to create a county name column
Country_slope$country <- rownames(Country_slope)

### Create an ordered county-level factor based upon slope values
Country_slope$country_plot <- factor(Country_slope$country, 
                                   levels = Country_slope$country[order(Country_slope$slope)])

### Now plot the results using ggplot2
ggplot(data = Country_slope, aes(x = country_plot, y = slope)) + 
  geom_point() +
  coord_flip() + 
  theme_bw() + 
  ylab("Compliance")  +
  xlab("Country")

