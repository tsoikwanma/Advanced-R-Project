# Advanced Programming in R [2400-DS1APR] Final Project by:
# I Putu Agastya Harta Pratama - 472876 - i.pratama@student.uw.edu.pl
# Tsoi Kwan Ma - 476914 - t.ma@student.uw.edu.pl



options(shiny.maxRequestSize = 70 * 1024^2) 

# Package installation, if not available
packages <- c("shiny", "bslib", "here", "Rcpp", "ggplot2", "DT", "tidyverse", 
              "shinythemes", "sf", "rnaturalearth", "rnaturalearthdata", 
              "leaflet", "RColorBrewer", "plotly")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

library(shiny)
library(bslib) # https://shiny.posit.co/r/articles/build/themes/
library(here)
library(Rcpp)
library(ggplot2)
library(DT)
library(tidyverse)
library(shinythemes) # https://bootswatch.com/simplex/
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(leaflet)
library(RColorBrewer)
library(plotly)

emission_data <- read.csv(here("2010 - 2022.csv"))
emission_data <- emission_data[, c(9, 15:16, 18)]
colnames(emission_data) <- c("sector", "country", "year", "emission")

# function for sums a vector of emissions
cppFunction('
double total_emission(NumericVector emission) {
  double total = 0;
  for(int i = 0; i < emission.size(); i++) {
    total += emission[i];
  }
  return total;
}
')

# Function for calculating difference in sums emission per year
cppFunction('
NumericVector emission_change(NumericVector emissions_start, NumericVector emissions_end) {
  int n = emissions_start.size();
  NumericVector changes(n);
  for (int i = 0; i < n; ++i) {
    changes[i] = emissions_end[i] - emissions_start[i];
  }
  return changes;
}
')

# Geospatial configuration
world_sf   <- ne_countries(scale = "medium", returnclass = "sf")
eu_members <- c(
  "Austria",        "Belgium",      "Bulgaria",
  "Croatia",        "Cyprus",       "Czechia",
  "Denmark",        "Estonia",      "Finland",
  "France",         "Germany",      "Greece",
  "Hungary",        "Ireland",      "Italy",
  "Latvia",         "Lithuania",    "Luxembourg",
  "Malta",          "Netherlands",  "Poland",
  "Portugal",       "Romania",      "Slovakia",
  "Slovenia",       "Spain",        "Sweden",
  # extra neighbors
  "United Kingdom", "Norway",       "Switzerland",
  "Iceland"
)

eu_sf <- world_sf %>% filter(admin %in% eu_members)

# Launch app
source("ui.R")
source("server.R")

shinyApp(ui, server)
