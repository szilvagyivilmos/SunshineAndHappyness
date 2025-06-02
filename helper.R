library(ggplot2)
library(tidyverse)
library(dplyr)
library(corrplot)
library(RColorBrewer)
library(maps)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)

setwd('/Users/mac/BME/Onlab/SunshineandHappyness')

happyness<-read.csv("./happyness_detailed.csv")
#happyness<-read.csv("./happiest-countries-in-the-world-2025.csv")
sunshine<-read.csv("./complete_sunshine_hours.csv")
gas<-read.csv("./GasEmissions.csv")
temp<-read.csv("./temp.csv")
loc<-read.csv("./map/atlagcoord.csv")


temp_national <- temp %>% filter(Level == "National") %>% select(Country, Continent, ISO_Code,X2022)  %>% drop_na()

h<-happyness[happyness$Year=="2024",]


                     
s<-sunshine %>% select(-Ref., -vteClimate.of.the.United.States.1,-vteClimate.of.the.United.States)   %>% drop_na()
s<- s %>% group_by(Country) %>% summarise_all(mean)

all <- inner_join(s, h, by = "Country")
all <- inner_join(all, loc, by = "Country")
all <- inner_join(all, gas, by = "Country")
all <- inner_join(all, temp_national, by = "Country")




all<- all %>% rename(
    score = HappinessScore,
    temp=X2022,
    Continent = Continent.y,
    sun_yearly = Year.x,
    lat = Latitude..average.,
    long = Longitude..average., ) %>% select(-City)


cor(all$score, all$gdp, use = "complete.obs")                


all <- all %>%
  mutate(sun_diff = abs(sun_yearly / lat))

numeric_data <- select_if(all%>%filter(Continent=="Africa"), is.numeric)
cor_matrix <- cor(numeric_data, use = "complete.obs")
print(cor_matrix)


numeric_data <- numeric_data[, sapply(numeric_data, sd, na.rm = TRUE) != 0]
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
cor_matrix.plot <- corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", col = brewer.pal(n = 8, name = "RdYlBu"))



correlations <- sapply(numeric_data, function(x) cor(numeric_data$score, x, use = "pairwise.complete.obs"))
correlations <- correlations[names(correlations) != "score"]
cor_df <- data.frame( Variable = names(correlations), Correlation = correlations)

ggplot(cor_df, aes(x = reorder(Variable, Correlation), y = Correlation)) +
  geom_bar(stat = "identity", fill = "skyblue") + 
  coord_flip() +
  labs(title = "Correlation with Ladder.score", x = "Variable", y = "Correlation") +
  theme_minimal()




corr_matrix <- cor(numeric_data, use = "pairwise.complete.obs")

# Basic correlogram with circles
corrplot(corr_matrix, method = "circle")

# For a plot similar to your image (with colored circles and variable names on both axes):
# You can customize the color and order as well

corrplot(
  corr_matrix,
  method = "circle",           # or "color", "number", etc.
  type = "upper",              # show only upper triangle
  order = "hclust",            # hierarchical clustering order
  col = brewer.pal(n = 8, name = "RdYlBu") # nice color palette
)
corr_matrix %<%  select(score)





correlations <- sapply(numeric_data, function(x) cor(numeric_data$score, x, use = "pairwise.complete.obs"))
correlations <- correlations[names(correlations) != "score"]
cor_df <- data.frame(Variable = names(correlations), Correlation = correlations)

# Sort by absolute correlation, descending
cor_df <- cor_df[order(cor_df$Correlation), ]

print(cor_df)


numeric_cols <- names(select_if(all, is.numeric))
numeric_cols <- setdiff(numeric_cols, "score")

# Calculate correlations for each continent
cor_list <- lapply(split(all, all$Continent), function(df) {
  sapply(numeric_cols, function(col) cor(df$score, df[[col]], use = "pairwise.complete.obs"))
})

# Combine into a dataframe
cor_df <- bind_rows(lapply(names(cor_list), function(cont) {
  data.frame(
    Continent = cont,
    Variable = names(cor_list[[cont]]),
    Correlation = as.numeric(cor_list[[cont]])
  )
}), .id = NULL)

cor_df <- cor_df %>% filter(Continent==America   )

# Plot: bars colored by continent
ggplot(cor_df, aes(x = reorder(Variable, Correlation), y = Correlation, fill = Continent)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Correlation with Ladder.score by Continent", x = "Variable", y = "Correlation") +
  theme_minimal()



# Filter for America and Europe
filtered <- all %>% filter(Continent %in% c("America", "Europe"))

# Select only month columns and score
month_cols <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
numeric_months <- filtered %>% select(all_of(month_cols), score, Continent)

# Calculate correlations for each continent
cor_list <- lapply(split(numeric_months, numeric_months$Continent), function(df) {
  sapply(month_cols, function(col) cor(df$score, df[[col]], use = "pairwise.complete.obs"))
})

# Combine into a dataframe
cor_df <- bind_rows(lapply(names(cor_list), function(cont) {
  data.frame(
    Continent = cont,
    Month = names(cor_list[[cont]]),
    Correlation = as.numeric(cor_list[[cont]])
  )
}), .id = NULL)

# Plot: bars colored by continent, months on y-axis
ggplot(cor_df, aes(x = reorder(Month, Correlation), y = Correlation, fill = Continent)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Correlation of Monthly Sunshine with Ladder.score (America & Europe)", 
       x = "Month", y = "Correlation") +
  theme_minimal()

# Define the correct month order
month_levels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# If your cor_df has a column named 'Variable' with month names:
cor_df$Variable <- factor(cor_df$Variable, levels = month_levels)

# Now plot
ggplot(cor_df, aes(x = Variable, y = Correlation)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Correlation with Ladder.score", x = "Month", y = "Correlation") +
  theme_minimal()




# 1. Calculate sun_diff
all <- all %>%
  mutate(sun_diff = sun_yearly - lat)

# 2. Aggregate by country (if needed)
country_data <- all %>%
  group_by(Country) %>%
  summarise(sun_diff = mean(sun_diff, na.rm = TRUE))

# 3. Get world map data
world <- map_data("world")

# 4. Merge map data with your country data
map_df <- world %>%
  left_join(country_data, by = c("region" = "Country"))

# 5. Plot: fill country territory by sun_diff
ggplot(map_df, aes(long, lat, group = group, fill = sun_diff)) +
  geom_polygon(color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90", name = "Sun Diff") +
  labs(title = "World Map: Difference Between Sunshine Hours and Latitude",
       x = "", y = "") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())






# 1. Get world map with ISO codes
world <- ne_countries(scale = "medium", returnclass = "sf")

# 2. Prepare your data (make sure ISO_Code is character)
country_data <- all %>%
  group_by(ISO_Code.x) %>%
  summarise(sun_diff = mean(gdp, na.rm = TRUE))

# 3. Join by ISO code
world_data <- left_join(world, country_data, by = c("iso_a3" = "ISO_Code.x"))

# 4. Plot
ggplot(world_data) +
  geom_sf(aes(fill = sun_diff), color = "white", size = 0.1) +
  scale_fill_viridis(option = "plasma", na.value = "grey90", name = "Sun Diff") +
  labs(title = "World Map: Difference Between Sunshine Hours and Latitude",
       x = "", y = "") +
  theme_minimal()

