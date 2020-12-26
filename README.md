# Abstract
The COVID-19-Pandemic is an ongoing challenge for policymakers. Especially, because overcoming it is so dependent on the policy-compliance of each and every individual. However, how is the state of this policy-compliance in Europe? Does it differ across different countries and how does it evolve over time? This analysis tests both a non- and a hierarchical model. It finds that, interestingly, levels of compliance with mobility-reducing COVID-19-policies differ across selected European countries and that policy-compliance expresses an alarming, negative trend.

# Data
This analysis combines data from the [Google COVID-19 Community Mobility Reports](https://www.google.com/covid19/mobility) and data from the [Oxford Policy Tracker](https://www.bsg.ox.ac.uk/research/research-projects/coronavirus-government-response-tracker). The Google Mobility Report charts “movement trends over time by geography, across different categories.” For the sake of this analysis the movement trends of the categories retail and recreation, supermarkets and pharmacies, public transport and workplaces were included and averaged without weighting. The Stringency Index was obtained from the Oxford Policy Tracker and records information on “the strictness of ‘lockdown style’ policies that primarily restrict people’s behaviour.” (Hale et al.2020)

For this analysis, policy-compliance is approximated as the difference between the level of policy-stringency and the observed reduction in mobility.

Compliance = 100 - (Policy Stringency Index - %-change in Mobility)

The predictions of policy-compliance are additionally corrected for [COVID-19-cases](https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data/vbim-akqf/), as tracked by Centers for Disease Control and Prevention Case Surveillance Task Force, and public attention on the topic, as approximated by Google search queries with the terms [“covid” & “corona”](https://trends.google.com/trends/explore?q=covid,corona) on a per country basis.

Countries included in the analysis were chosen based on their membership to the EU (including the United
Kingdom) and their GDP.
