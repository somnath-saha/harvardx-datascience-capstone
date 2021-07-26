# House Sales in King County, USA
# This dataset contains house sale prices for King County, which includes Seattle. 
# It includes homes sold between May 2014 and May 2015.
# 

library(tidyverse)

housedata <- read.csv("cyo-housesale-project/kc_house_data.csv", as.is = TRUE)


# edx %>% group_by(movieId) %>%
#    summarise(ave_rating = sum(rating)/n()) %>%
#    ggplot(aes(ave_rating)) +
#    geom_histogram(bins=30, color = I("white"), fill = "pink") +
#    labs(x = "Average rating", y = "Number of movies", caption = caption_text) + plot_theme

plot_theme <- theme(plot.caption = element_text(size = 7, face = "italic"), 
                    axis.title = element_text(size = 11))
caption_text <- "PH125.9x | Source: Kaggle KC House Data | Somnath Saha"

housedata %>% ggplot(aes(price)) +
   geom_histogram(binwidth = 100000, color = I("blue"), fill = "skyblue") +
   scale_y_continuous(breaks = seq(0, 4500, 500)) +
   scale_x_continuous(breaks = seq(0, 8000000, 500000), labels = sprintf("%sM", seq(0, 8, 0.5))) +
   labs(x = "Price Range", y = "Number of Houses", caption = caption_text) + plot_theme

housedata %>% filter(price < 1000000) %>% ggplot(aes(price)) +
   geom_histogram(binwidth = 100000, color = I("white"), fill = "lightblue") +
   scale_y_continuous(breaks = seq(0, 4500, 500)) +
   scale_x_continuous(breaks = seq(0, 1500000, 100000), labels = sprintf("%sK", seq(0, 1500, 100))) +
   labs(x = "Price Range", y = "Number of Houses", caption = caption_text) + plot_theme


boxplot(price ~ bedrooms, data = housedata, 
         xlab = "No of Bedrooms",
         ylab = "Price of House", 
         col = "maroon",
         ylim = c(0, 8000000),
         #names = sprintf("%sM", seq(0, 8, 0.5)),
         main = "Bedrooms vs Price of House"
         )

housedata %>% group_by(gr = cut(price/1000000, breaks= seq(0, 10, by = 1)) ) %>% 
               summarise(n = n()) %>%
               arrange(desc(as.numeric(n)))

housedata %>% group_by(gr = cut(price/1000, breaks= seq(0, 20000, by = 100)) ) %>% 
               summarise(n = n()) %>%
               arrange(desc(as.numeric(n)))

housedata %>% qplot(price, )



Reference :
   https://www.tutorialspoint.com/r/r_boxplots.htm










