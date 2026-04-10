# ----------------
# setup
# ----------------
# Template of R script to answer problem set
# Group number: 
# Group composition: Madelief van Weerdenburg, Nicollo Zambello, Jakub Przewoski 

### 0. Load packages ###
# install.packages("haven")        # For reading Stata/SPSS/SAS files
# install.packages("dplyr")        # For data manipulation
# install.packages("plm")          # For panel data analysis
# install.packages("fixest")       # For fixed effects estimation
# install.packages("ggplot2")      # For data visualization
# install.packages("tidyr")        # For data tidying
# install.packages("bacondecomp")  # For Bacon decomposition
# install.packages("did")          # For difference-in-differences analysis
# install.packages("modelsummary") # For regression tables
# install.packages("TwoWayFEWeights") # For TWFE weights analysis


library(haven)
library(dplyr)
library(plm)
library(fixest)
library(ggplot2)
library(tidyr)
library(bacondecomp)
# library(did)        # Uncomment if using Callaway & Sant’Anna's estimator
library(modelsummary) # For regression tables
library(TwoWayFEWeights) # If implementing TWFE weights

#excercise 1
# -------- a --------
#Given that divorce rates are
#an average computed in each state and the variable stpop provides the population in
#each of these states, which is the weight you should use when reporting the evolution
#of divorce rates or a regression of divorce rates on unilateral divorce laws to 
#match the analysis in Wolfers (2006)?

#Wolfers uses the metric of number of divorces per 1,000 people per each US state 
#for the div_rate variable. This creates a general metric with which you can properly 
#compare between states, but when one wants to report the evolution of divorce rates 
#or to run a regression of divorce rates on unilateral divorce laws, it is necessary to 
#give each state the proper weight in order to not over account for smaller states and 
#under account for larger states. (not having a bias toward smaller states).
#The weight type we use for this is Analytic Weights (aweight / cellsize) as it 
#corrects for cell means with different group sizes (stpop). Furthermore, it corrects
#for the heteroskedasticity in the standard deviations, which would naturally
#occur due to to different group sizes (here, the state populations).



# -------- b --------
df <- pset_2

df <- df %>%
  mutate(year = as.numeric(year))%>%
  arrange(st, year, div_rate, stpop)%>%
  mutate(
    reformed = lfdivlaw >= 1968 & lfdivlaw <= 1988,
    group = ifelse(reformed, "Reform states", "Control states")
  )

df_collapsed <- df %>%
  group_by(year, group) %>%
  summarise(
    avg_div_rate = weighted.mean(div_rate, stpop, na.rm = TRUE),
    .groups = "drop"
  )



pdata <- pdata.frame(df, index = c("st", "year"))


df_diff <- df_collapsed %>%
  pivot_wider(names_from = group, values_from = avg_div_rate) %>%
  mutate(Difference = `Reform states` - `Control states`) %>%
  pivot_longer(cols = c("Reform states", "Control states", "Difference"),
               names_to = "group", values_to = "avg_div_rate")


ggplot(df_diff, aes(x = year, y = avg_div_rate, color = group, linetype = group, 
                    linewidth = group)) + geom_line() +
  scale_color_manual(values = c(
    "Reform states" = "black",
    "Control states" = "grey50",
    "Difference" = "black"
  )) +
  scale_linetype_manual(values = c(
    "Reform states" = "solid",
    "Control states" = "solid",
    "Difference" = "dashed"
  )) +
  scale_linewidth_manual(values = c(
    "Reform states" = 1,
    "Control states" = 1,
    "Difference" = 0.5
  )) +
  geom_vline(xintercept = 1969, linetype = "solid", linewidth = 0.4) +
  geom_vline(xintercept = 1977, linetype = "solid", linewidth = 0.4) +
  annotate("segment", x = 1968, xend = 1988, y = 0.15, yend = 0.15) +
  annotate("text", x = 1983, y = 0.35, 
           label = "Friedberg's sample", size = 3) +
  scale_x_continuous(breaks = seq(1956, 1998, by = 2)) +
  scale_y_continuous(limits = c(0, 7.5), breaks = 0:7) +
  labs(
    x = "Year",
    y = "Divorce rate: 
    Divorces per 1,000 persons per year",
    color = NULL, linetype = NULL, linewidth = NULL,
    caption = "FIGURE 1. AVERAGE DIVORCE RATE: REFORM STATES AND CONTROLS"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    legend.position = "top",
    legend.key.width = unit(1.5, "cm"),
    plot.caption = element_text(hjust = 0.5, face = "bold", size = 9)
  )