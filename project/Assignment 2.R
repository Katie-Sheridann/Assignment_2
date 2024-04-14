install.packages("tidyverse")
install.packages("plotly")

library(tidyverse)
library(plotly)

# Importing Data
unicef_metadata <- read_csv("unicef_metadata.csv")
unicef_indicator_1 <- read_csv("unicef_indicator_1 - unicef_indicator_1.csv")
unicef_indicator_2 <- read_csv("unicef_indicator_2 - unicef_indicator_2.csv")
Deaths_caused_by_diarrhoea <- read_csv("Deaths caused by diarrhoea.csv")
Transmission_rate_2021_2024 <- read_csv("Transmission rate 2021-2024.csv")
Deaths_under_5_2024 <- read_csv("^1 deaths under 5-2024 (3).csv")


# Filter Data
unicef_indicator_2_2021 <- filter(unicef_indicator_2, time_period == 2021)
unicef_indicator_1_2021 <- filter(unicef_indicator_1, time_period == 2021)
Death_under5_total <- filter(Deaths_under_5_2024, Sex == "Total")




# Joining Data
date_join_1<- full_join(Death_under5_total, Deaths_caused_by_diarrhoea, by = c("Geographic area" = "Country"))



# Map Visualization 1
options(scipen = 999)

map_world <- map_data("world")

map_data_join_1 <- full_join(map_world, unicef_indicator_2_2021, by = c("region" = "country"))

ggplot(map_data_join_1) +
  aes(x = long, y = lat, group = group, fill = obs_value) +
  geom_polygon() +
  scale_fill_gradient(low = "pink", high = "darkred", na.value = "grey", limits=c(1, 3023000)) +
  labs(
    title = " Number of pregnant women presenting at antenatal clinics (ANC) who were HIV positive in 2021 (World)",
    subtitle = "Countries in grey have no data due to a mismatch with their names",
    caption = "Source:Unicef",
    x = "Longitude",
    y = "Latitude",
    fill = "No. of + HIV tests"
  ) +
    theme_classic() +
  theme(text = element_text(size = 10, family = "serif"))


# Map Visualization 2
map_data_join_2 <- full_join(map_world, unicef_indicator_1_2021, by = c("region" = "country"))

ggplot(map_data_join_2) +
  aes(x = long, y = lat, group = group, fill = obs_value) +
  geom_polygon() +
  scale_fill_gradient(low = "pink", high = "darkred", na.value = "grey", limits=c(0,100 )) +
  labs(
    title = " Proportion of population using improved sanitation facilities in 2021 (World)",
    subtitle = "Countries in grey have no data due to a mismatch with their names",
    caption = "Source:Unicef",
    x = "Longitude",
    y = "Latitude",
    fill = "Prop.of Population"
  ) +
  theme_classic()+
  theme(text = element_text(size = 10, family = "serif"))


# Scatter Plot
ggplot(date_join_1) +
  aes(x = OBS_VALUE, y = Indicator.y, colour = ...3 ) +
  geom_point() +
  scale_color_brewer(palette = "Reds") +
  geom_smooth(method = "lm", colour = "black") +
  theme_classic() +
  theme(text = element_text(size = 10, family = "serif")) +
  labs(
  title = " % Deaths caused by Diarrhoea and Overall mortality rate in Children under-Five in 2021 (World)",
  caption = "Source: Unicef",
  x = "Under-five mortality rate",
  y = " % Deaths Caused by Diarrhoea",
  colour = "Continents"
  )
  
  

# Bar Chart
colnames(Transmission_rate_2021_2024) <- c("Country", "Indicator", "Sex","year", "OBS_VALUE", "Unit multiplier","Unit of measure", "Observation Status")

Transmission_subset<-subset(Transmission_rate_2021_2024, Country %in% c("Angola", "Burkina Faso", "Cameroon", "Chad", "CÃ´te d'Ivoire", "Ivory Coast", "Congo, the Democratic Republic of the", "Ethiopia", "Ghana", "Indonesia", "Iran, Islamic Republic of", "Kenya", "Malawi", "Mexico", "Mozambique", "South Africa", "Uganda", "Tanzania, United Republic of", "Uzbekistan", "Viet Nam", "Vietnam", "Zambia"))
Transmission_graph <-ggplot(Transmission_subset) +
  aes(reorder(Country, OBS_VALUE), OBS_VALUE, fill = Country,) +
  geom_col() +
  guides(fill = "none") +
  scale_x_discrete(label=abbreviate) +
  theme_classic() +
  theme(text = element_text(size = 10, family = "serif"),
        axis.text.x = element_blank()
  ) +
  labs(
    x = "Country",
    y = " Mother-to-child HIV transmission rate",
    colour = "Country"
  ) +
  facet_wrap(~ year) +
  scale_fill_manual(values = c("#feede2","#fee4d2","#fed5b9", "#ffcba9", "#ffc299", "#ffb888", "#ffa970", "#ff9b57", "#ff8c3f","#ff7d26", "#ff6a06","#f46200", "#e45c00", "#d45500", "#c34f00", "#b34800", "#a34100","#923b00", "#823400","#722e00", "#612700", "#800000","#730000","#660000","#5a0000","#4d0000","#400000","#330000", "#260000", "#1a0000", "#0d0000"))
  
ggplotly(Transmission_graph)%>%
  layout(title = list(text = paste0( "Mother-to-child HIV transmission rate in 2021 vs. 2022",                
                                     "<br>",
                                     "<sup>",
                                     "This is a sample taken from the list of countries with the highest number of pregnant women who tested positive for HIV in 2021","</sup>")))
# Timeseries Visulization 
colnames(unicef_metadata) <- c("Country", "alpha_2_code", "alpha_3_code" ,"numeric_code" ,"iso3c", "year" , "Population", "GDP_Per_Capita" , "GNI", "Inflation", "LifeExp", "Military")
  
Metadata_timeseries <- unicef_metadata %>%
  ggplot() +
  aes(year, LifeExp, group = Country, colour = Country) +
  geom_line() +
  guides(colour = "none") +
  labs(
    title = "The Evolution of the average Life Expectancy at birth from 1960-2022 (World)",
    subtitle = "Certain rows removed due to missing values",
    caption = "Source: Unicef",
    x = "Year",
    y = " Life Expectancy at birth",
    colour = "Country"
  ) +
  theme_classic() +
  theme(text = element_text(size = 10, family = "serif"))
  
  ggplotly(Metadata_timeseries)

  
  
# Timeseries Visualization 2
  Life_Expectancy_subset <-subset(unicef_metadata, Country %in% c("Angola", "Burkina Faso", "Cameroon", "Chad", "CÃ´te d'Ivoire", "Ivory Coast", "Congo, the Democratic Republic of the", "Ethiopia", "Ghana", "Indonesia", "Iran, Islamic Republic of", "Kenya", "Malawi", "Mexico", "Mozambique", "South Africa", "Uganda", "Tanzania, United Republic of", "Uzbekistan", "Viet Nam", "Vietnam", "Zambia"))
  
  Metadata_timeseries_subset <- Life_Expectancy_subset %>%
    ggplot() +
    aes(year, LifeExp, group = Country, colour = Country) +
    geom_line() +
    guides(colour = "none") +
    theme_classic() +
    theme(text = element_text(size = 10, family = "serif")) +
    labs(
      x = "Year",
      y = " Life Expectancy at birth",
      colour = "Country"
    ) +
    scale_color_manual(values = c("#ffb888", "#ffa970", "#ff9b57", "#ff8c3f", "#ff6a06","#f46200", "#e45c00", "#d45500", "#c34f00", "#b34800", "#a34100","#923b00", "#823400","#722e00", "#612700", "#800000","#730000","#5a0000","#4d0000","#400000","#330000", "#260000"))+
    theme_classic() +
    theme(text = element_text(size = 10, family = "serif"))
  
  ggplotly(Metadata_timeseries_subset) %>%
  layout(title = list(text = paste0( "The Analysation of the average life Expectancy at birth from 1960-2022 from a sample of countries.",
                                    "<br>",
                                    "<sup>",
                                    "This a sample of twenty countries with the highest number of pregnant women who tested positive for HIV in 2021.","</sup>")))
  
# Timeseries Visualization 3
  Metadata_timeseries_2 <- unicef_metadata %>%
    ggplot() +
    aes(year, Population, group = Country, colour = Country) +
    geom_line() +
    guides(colour = "none") +
    labs(
      title = "Population of each Country from 1960-2022(World)",
      subtitle = "Certain rows removed due to missing values",
      caption = "Source: Unicef",
      x = "Year",
      y = " Population per Country",
      colour = "Country"
    ) +
    theme_classic() +
    theme(text = element_text(size = 10, family = "serif"))
  
  ggplotly(Metadata_timeseries_2)
  
  

