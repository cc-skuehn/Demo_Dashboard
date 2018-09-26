
############
## Libraries
############

# Plotting
library(ggplot2)
library(flexdashboard)
library(shiny)
library(plotly)
#library(scales)

# Forecasting, Causal Inference
library(prophet)
library(CausalImpact)

# Markdown
library(knitr)
library(markdown)

# Color palettes - in case
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# The same palette with black first
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# Read data

colmodes <- c('character','Date',rep('numeric',4)) 
data_berlin <- read.csv('ProcessedData/data_berlin.csv', colClasses = colmodes) # as.is = T ignores Date
data_hamburg <- read.csv('ProcessedData/data_hamburg.csv', colClasses = colmodes)
data_munich <- read.csv('ProcessedData/data_munich.csv', colClasses = colmodes)

# Reference Values

# Small helper function, calculates avg temperatures per month
calculate_reference <- function(df){
  # Expects data frame with following structure:
  # STATION_NAME | DATUM | cm_05 | cm_10 | cm_20 | cm_50
  
  # Add month information
  df$month <- months(df$DATUM)
  all_months <- unique(df$month)
  monthly_reference <- df[1:12,3:7]
  for (i in 1:12) {
    monthly_reference[i,1:4] <- colMeans(df[df$month==all_months[i],3:6])
    monthly_reference[i,5] <- all_months[i]
  }
  # return value
  monthly_reference
}

