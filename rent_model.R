library(tidyverse)
library(brms)
library(sf)

# Load PRMS data
df <- read_csv("prms_mean_rent.csv")
# Convert rooms to ordered factor to work with brms
df$rooms <- factor(df$rooms, levels = c("Room","Studio","One","Two","Three","Four"), ordered = TRUE)
df$lad_code <- as_factor(df$lad_code)

# Fit model with random intercepts by local authority, a global monotonic slope by number of rooms, and a random slope for each local authority
res <- brm(log(mean) ~ 1 + mo(rooms) + (1+mo(rooms)|lad_code),
           data = df,
           iter = 2000,
           chains = 4,
           cores = 4,
           threads = threading(3),
           backend = "cmdstanr")

# Examine model fit
print(res)

# Generate predictions for each local authority/room combo
newdata <- expand_grid(lad_code = factor(levels(df$lad_code), levels = levels(df$lad_code)),
                       rooms = factor(levels(df$rooms), levels = levels(df$rooms), ordered = TRUE))
newdata <- bind_cols(newdata,predict(res, newdata))

# Load shapefile
boundaries <- read_sf("boundaries/LAD_DEC_2020_UK_BGC.shp")

# Crate rent map plot with predictions
boundaries %>%
  filter(substr(LAD20CD,1,1)=="E") %>%
  left_join(newdata, by = join_by("LAD20CD"=="lad_code")) %>%
  ggplot(aes(fill = Estimate)) +
  geom_sf() +
  coord_sf(datum = NA) +
  scale_fill_gradientn(colors = c(scales::muted("blue"),"white",scales::muted("red")),
                       breaks = log(c(400,650,1100,1800,3000,5000)),
                       labels = function(x) paste("£",
                                                  format(
                                                    round(exp(x), 0),
                                                    big.mark=","
                                                    ),
                                                  sep="")) +
  facet_wrap(~rooms) +
  labs(title = "Average private rent in England by local authority and number of bedrooms",
       subtitle = "Private rental market summary statistics",
       caption = "@jwhandley17")
ggsave("rent_map.png",width=8,height=6)
