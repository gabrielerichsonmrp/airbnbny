source('dependencies.R')
# load all packages
#lapply(required_packages, require, character.only = TRUE)

#THEME-------------------------
theme_algoritma <- theme(legend.key = element_rect(fill="black"),
                         legend.background = element_rect(color="#4a4a4a", fill="#263238"),
                         legend.text = element_text(size=8, color="#4a4a4a"),
                         plot.title = element_text(size=10, color="#4a4a4a"),
                         plot.subtitle = element_text(size=6, color="#4a4a4a"),
                         panel.background = element_rect(fill="#dddddd"),
                         panel.border = element_rect(fill=NA),
                         panel.grid.minor.x = element_blank(),
                         panel.grid.major.x = element_blank(),
                         panel.grid.major.y = element_line(color="darkgrey", linetype=2),
                         panel.grid.minor.y = element_blank(),
                         plot.background = element_rect(fill="#e6e6e6"),
                         text = element_text(color="#4a4a4a"),
                         axis.text = element_text(size=8, color="#4a4a4a")
                         )


# DATA -------------------------

airbnb <- read.csv("data/AB_NYC_2019.csv")

airbnb <- airbnb %>% 
  mutate(
    id = as.integer(id),
    name = as.character(name),
    host_id = as.integer(host_id),
    host_name = as.character(host_name),
    neighbourhood_group = as.factor(neighbourhood_group),
    neighbourhood_group <- droplevels(neighbourhood_group),
    neighbourhood <- as.character(neighbourhood),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    room_type = gsub("/apt","",room_type),
    room_type = as.factor(room_type),
    room_type <- droplevels(room_type),
    price = as.numeric(price,2),
    min_nights = as.integer(minimum_nights),
    reviews = as.integer(number_of_reviews),
    last_review = ymd(last_review),
    reviews_per_month = as.numeric(reviews_per_month,2),
    host_listings_count = as.integer(calculated_host_listings_count),
    avail_365 = as.integer(availability_365)
  ) %>%
  filter(price>0) %>% 
  select(id,name,host_id,host_name,neighbourhood_group,neighbourhood,latitude,longitude,room_type,
         price,min_nights,reviews,last_review,reviews_per_month,host_listings_count,avail_365) %>% 
  distinct()


airbnb <- airbnb %>%    
  mutate(
    price_per_night = round(as.numeric((price/min_nights)),1)
  ) %>% 
  filter(price_per_night>0)

airbnb.neighbourhood_group <- c("All",levels(airbnb$neighbourhood_group))


airbnb.neighbourhood <- airbnb %>% 
  select(neighbourhood_group,neighbourhood) %>% 
  distinct()

min.date <- airbnb %>% 
  arrange(last_review) %>% 
  head(1)


max.date <- airbnb %>% 
  arrange(desc(last_review)) %>% 
  head(1)


