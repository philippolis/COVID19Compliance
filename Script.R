# Loading the libraries ----
library(dplyr)
library(ggplot2)
library(scales)
library(lme4)
library(broom.mixed)
library(plotly)
library(reshape2)
library(utils)
library(texreg)
library(stargazer)
library(extrafont)
library(lmtest)
loadfonts()

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

### Only selecting the columns that are interesting
policy_data_Europe_small <- policy_data_Europe %>% select(Date, CountryName, StringencyIndex)


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

### renaming the date columns to match
names(mobility_data_Europe)[names(mobility_data_Europe)=="date"] <- "Date"
names(mobility_data_Europe)[names(mobility_data_Europe)=="country"] <- "CountryName"
names(mobility_data_Europe)[names(mobility_data_Europe)=="avg_change"] <- "MobilityIndex"

### Only selecting relevant data
mobility_data_Europe_small <- mobility_data_Europe %>% select(Date, CountryName, MobilityIndex)


## Adding the Cases Data ----

###read the Dataset sheet in
cases_data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

### renaming the date and countries columns to match
names(cases_data)[names(cases_data)=="dateRep"] <- "Date"
names(cases_data)[names(cases_data)=="countriesAndTerritories"] <- "CountryName"

### Forcing Date format
cases_data$Date = as.Date(as.character(cases_data$Date), format = "%d/%m/%Y")

### create dataframe with only national level of Germany, France, UK, Spain, Italy
cases_data_Europe <- cases_data %>% filter(CountryName == "Germany" |
                                             CountryName == "Italy" |
                                             CountryName == "France" |
                                             CountryName == "Spain" |
                                             CountryName == "United_Kingdom" |
                                             CountryName == "Netherlands" |
                                             CountryName == "Belgium" |
                                             CountryName == "Poland" |
                                             CountryName == "Sweden" |
                                             CountryName == "Ireland" |
                                             CountryName == "Austria" |
                                             CountryName == "Denmark")

### Changing United_Kingdom to United Kingdom
rename <- c(United_Kingdom="United Kingdom", Germany="Germany", Italy="Italy",
            France="France", Spain="Spain", Netherlands="Netherlands", Belgium="Belgium",
            Poland="Poland", Sweden="Sweden", Ireland="Ireland", Austria="Austria",
            Denmark="Denmark")

cases_data_Europe$CountryName <- as.character(rename[cases_data_Europe$CountryName])

### Only selecting the columns that are interesting
cases_data_Europe_small <- cases_data_Europe %>% select(Date, CountryName, notification_rate_per_100000_population_14.days)

## Adding the Trends Data ----
trends_data <- read.csv("GoogleTrends.csv", sep = ";")

### Forcing correct date format
trends_data$Date = as.Date(trends_data$Date, format = "%d.%m.%Y")+1

## Renaming the Sum column to Publicity
names(trends_data)[names(trends_data)=="Sum"] <- "Publicity"

## Only selecting the columns that are interesting
trends_data <- trends_data %>% select(Date, CountryName, Publicity)

## Merging policy mobility trends and cases data on date and country ----

data = policy_data_Europe_small %>% 
  full_join(mobility_data_Europe_small, by = c("Date", "CountryName")) %>%
  full_join(cases_data_Europe_small, by = c("Date", "CountryName")) %>%
  full_join(trends_data, by = c("Date", "CountryName"))

## Adding a column with Compliance-index ----

data$ComplianceIndex <- 100 - (data$StringencyIndex + data$MobilityIndex)

### Rescaling the Compliance-index to assume values between 0 and 100

data$ComplianceIndex <- rescale(data$ComplianceIndex, to = c(0, 100))


# Explorative Data Visualization ----


## Correlation table
pairs(subset(data, select = c(Date, Publicity, ComplianceIndex, notification_rate_per_100000_population_14.days)))

## Create a Boxplot of Compliance by Country ----
ggplot(data = data, aes(x = CountryName, y = ComplianceIndex)) +
  geom_boxplot() +
  xlab("Country") +
  ylab("Policy Compliance Index") +
  ggtitle("Policy Compliance in Europe") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        text=element_text(family="CMU Serif"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "grey", linetype = "solid"))

## Create a faceted plot with Stringency and Mobility for European Countries ----

ggplot(data = data) +
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
    axis.title.y.right = element_text(color = "black", size=12, vjust = 3),
    text=element_text(family="CMU Serif"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(colour = "grey", linetype = "dotted"),
    strip.background = element_rect(fill="white")
  ) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  ggtitle("Development of Policy Stringency and Mobility Reduction in Europe")

## Create a faceted plot with Compliance for European Countries ----
ggplot(data) +
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
    axis.title.x = element_text(size=12, vjust = 0.5),
    text=element_text(family="CMU Serif"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(colour = "grey", linetype = "dotted"),
    strip.background = element_rect(fill="white")
  ) +
  ggtitle("Policy Compliance in Europe Across Time")

# Modelling ----

## Non-hierarchical model ----

lm_out <- lm(data = data, ComplianceIndex ~ Date + Publicity + Date + notification_rate_per_100000_population_14.days)

summary(lm_out)

### Creating nice regression table output ----
lm1 <- lm(data = data, ComplianceIndex ~ Date + Publicity + notification_rate_per_100000_population_14.days + CountryName)

#### Modify htmlreg arguments in order to improve table output
#### Then insert the texreg statement in Rmd
table <- texreg::htmlreg(lm1, single.row = TRUE, 
#                         custom.header = list("Model Results" = 1),
#                         model.names = c("My name 1", "My name 2"), 
                         custom.coef.names = c("Intercept", "Date", "Publicity",
                                               "Cases/100,000/14 Days",
                                               "Belgium",
                                               "Denmark",
                                               "France",
                                               "Germany",
                                               "Ireland",
                                               "Italy",
                                               "Netherlands",
                                               "Poland",
                                               "Spain",
                                               "Sweden",
                                               "United Kingdom"),
                          bold = 0.05,
                          center = TRUE,
                          caption = "Regression Table"
                         )

tempDir <- tempfile()
dir.create(tempDir)
htmlFile <- file.path(tempDir, "test.html")
writeLines(table, htmlFile)
rstudioapi::viewer(htmlFile)

### Visualising different predicted intercepts and different slopes
ggplot(data = augment(lm(data = data, 
                         ComplianceIndex ~ Date + CountryName + Publicity + notification_rate_per_100000_population_14.days)),
       aes(x = Date, y = ComplianceIndex, color = CountryName))  +
  geom_smooth(aes(y = .fitted), method = "lm", se = FALSE)

### Visualization of the non-hierarchical model ----

### Visualising different predicted intercepts and different slopes
ggplot(data = augment(lm(data = data, 
                         ComplianceIndex ~ Date + CountryName + Publicity + notification_rate_per_100000_population_14.days)),
       aes(x = Date, y = ComplianceIndex, color = CountryName))  +
  geom_smooth(aes(y = .fitted), method = "lm", se = FALSE) +
  xlab("Month") +
  scale_x_date(breaks = date_breaks("3 months"),
               labels = date_format("%b\n%Y")) +
  scale_y_continuous(name = "Policy Compliance Index") +
  theme(
    axis.title.y = element_text(color = "black", size=12, vjust = 3),
    axis.title.x = element_text(size=12, vjust = 0.5),
    text=element_text(family="CMU Serif"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(colour = "grey", linetype = "dotted")
  ) +
  ggtitle("Model Predictions for Policy Compliance in Europe Across Time")

#### Fit model

model_out <- lm(data = data, MobilityIndex ~ StringencyIndex + Date)

#### predict over sensible grid of values
unique_stringency <- unique(data$StringencyIndex)
unique_date <- unique(data$Date)
grid <- with(data, expand.grid(unique_stringency, unique_date))
d <- setNames(data.frame(grid), c("StringencyIndex", "Date"))
vals <- predict(model_out, newdata = d)

#### form matrix and give to plotly
model_out <- matrix(vals, nrow = length(unique(d$StringencyIndex)), ncol = length(unique(d$Date)))

model_out_plot <- plot_ly() %>% 
  add_surface(x = ~unique_date, y = ~unique_stringency, z = ~model_out, colorscale = list(c(0, 1), c("green", "yellow"))) %>% 
  add_markers(data = data, z = ~MobilityIndex, x = ~Date, y = ~StringencyIndex, color = ~CountryName, opacity = 0.6, size = 0.1) %>%
  layout(scene = list(xaxis = list(title = 'Date'),
                      yaxis = list(title = 'Stringency Index'),
                      zaxis = list(title = 'Mobility Index')))

hide_colorbar(model_out_plot)

#### Visualising the linear model in 3D ----

##### Scatterplot for Europe
plot_ly(data = data, z = ~MobilityIndex, x = ~Date, y = ~StringencyIndex, color = ~CountryName, opacity = 0.6) %>%
  add_markers(size=0.1)

##### Scatterplot for Austria
plot_ly(data = subset(data, CountryName == "Austria"), z = ~MobilityIndex, x = ~Date, y = ~StringencyIndex, opacity = 0.6) %>%
  add_markers(size=0.1)

##### Line plot for Europe
plot_ly(data, x = ~Date, y = ~StringencyIndex, z = ~MobilityIndex, color = ~CountryName, type = 'scatter3d', mode = 'lines',
               opacity = 0.6, line = list(width = 3, reverscale = FALSE))

## How does the Compliance change over time in Europe? This model has not enough independent variables ----

lmer_out <- lmer(data = data, ComplianceIndex ~ Date + Publicity + notification_rate_per_100000_population_14.days +  (Date | CountryName))

### Looking at the results
summary(lmer_out)

### Getting p-values

2*pt(11.505, 520, lower.tail = F)

2*pt(0.435, 520, lower.tail = F)

2*pt(1.873, 520, lower.tail = F)

### extract out the fixed effect for date
fixef(lmer_out)

### extract out the random effect for country
ranef(lmer_out)

### Formatting the regression table
#### Modify htmlreg arguments in order to improve table output
#### Then insert the texreg statement in Rmd
table <- texreg::htmlreg(lmer_out, single.row = TRUE, 
                        #                         custom.header = list("Model Results" = 1),
                        #                         model.names = c("My name 1", "My name 2"), 
                        custom.coef.names = c("Intercept", "Date", "Publicity",
                                              "Cases/100,000/14 Days"),
                        #"Belgium",
                        #"Denmark",
                        #"France",
                        #"Germany",
                        #"Ireland",
                        #"Italy",
                        #"Netherlands",
                        #"Poland",
                        #"Spain",
                        #"Sweden",
                        #"United Kingdom"),
                        bold = 0.05,
                        center = TRUE,
                        caption = "Regression Table 2"
)

tempDir <- tempfile()
dir.create(tempDir)
htmlFile <- file.path(tempDir, "test.html")
writeLines(table, htmlFile)
rstudioapi::viewer(table)

## Likelihood ratio test

lrtest(lm_out, lmer_out)

## Warning in modelUpdate(objects[[i - 1]], objects[[i]]): original model was of
## class "lmerMod", updated model is of class "lm"
## Likelihood ratio test
## 
## Model 1: LabVote19 ~ LabVote17 + Leave16Vote + (1 | region)
## Model 2: LabVote19 ~ LabVote17 + Leave16Vote + region - 1
##   #Df  LogLik Df  Chisq Pr(>Chisq)    
## 1   5 -1738.8                         
## 2  14 -1713.8  9 50.053  1.053e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

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

