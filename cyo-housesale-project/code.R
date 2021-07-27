# House Sales in King County, USA
# This dataset contains house sale prices for King County, which includes Seattle. 
# It includes homes sold between May 2014 and May 2015.
# 

library(tidyverse)

housedata <- read.csv("cyo-housesale-project/kc_house_data.csv", as.is = TRUE)

plot_theme <- theme(plot.caption = element_text(size = 7, face = "italic"), 
                    axis.title = element_text(size = 11))
caption_text <- "PH125.9x | Source: Kaggle KC House Data | Somnath Saha"

# Distribution of all the different price of houses (Upto 8M)
housedata %>% ggplot(aes(price)) +
   geom_histogram(binwidth = 100000, color = I("blue"), fill = "skyblue") +
   scale_y_continuous(breaks = seq(0, 4500, 500)) +
   scale_x_continuous(breaks = seq(0, 8000000, 500000), labels = sprintf("%sM", seq(0, 8, 0.5))) +
   labs(x = "Price Range ($)", y = "Number of Houses", caption = caption_text) + plot_theme

# Distribution of the house prices within 1.5M 
housedata %>% filter(price < 1000000) %>% ggplot(aes(price)) +
   geom_histogram(binwidth = 100000, color = I("white"), fill = "lightblue") +
   scale_y_continuous(breaks = seq(0, 4500, 500)) +
   scale_x_continuous(breaks = seq(0, 1500000, 100000), labels = sprintf("%sK", seq(0, 1500, 100))) +
   labs(x = "Price Range ($)", y = "Number of Houses", caption = caption_text) + plot_theme

# Distribution of all sqft_living values of houses
housedata %>% filter(sqft_living < 10000) %>% ggplot(aes(sqft_living)) +
   geom_histogram(binwidth = 1000, color = I("blue"), fill = "skyblue") +
   #scale_y_continuous(breaks = seq(0, 4500, 500)) +
   scale_x_continuous(breaks = seq(0, 10000, 1000), labels = seq(0, 10000, 1000)) +
   labs(x = "Living Room Size", y = "Count", caption = caption_text) + plot_theme

# Distribution of all sqft_lot values of houses
housedata %>% filter(sqft_lot < 100000) %>% 
   ggplot(aes(sqft_lot)) +
   geom_histogram(binwidth = 10000, color = I("blue"), fill = "skyblue") +
   scale_y_continuous(breaks = seq(0, 20000, 1000)) +
   scale_x_continuous(breaks = seq(0, 100000, 5000), labels = sprintf("%s", seq(0, 10, 0.5))) +
   labs(x = "sqft_lot (In 1000 sqft)", y = "Count", caption = caption_text) + plot_theme

# Distribution of number of houses based on the following column values - 
# 'floors', 'waterfront', 'view', 'condition', 'grade', 'yr_built', 'yr_renovated', 'zipcode'
grp_col_names <- c('floors', 'waterfront', 'view', 'condition', 'grade')

lapply(grp_col_names, function(c) {
   res <- housedata %>% group_by(.dots = c) %>%  
      summarise('Count' = n(), Percentage = n() * 100 / nrow(housedata)) %>%
      arrange(desc(Percentage)) %>% top_n(10)
   res %>% ggplot(aes_string(x=c, y='Count')) + 
      geom_bar(stat = "identity", color = I("white"), fill = "skyblue") +
      geom_text(aes_string(label='Count'), position=position_dodge(width=0.5), vjust=-0.3) +
      labs(x = c, y = "Count", caption = caption_text) + plot_theme
})

grp_col_names_yr <- c('yr_built', 'yr_renovated', 'zipcode')
lapply(grp_col_names_yr, function(c) {
   res <- housedata %>% group_by(.dots = c) %>%  
      summarise('Count' = n(), Percentage = n() * 100 / nrow(housedata)) %>%
      arrange(desc(Percentage)) %>% top_n(10)
   res
})
   
# Influence of number of bedrooms on the price of house 
housedata %>% filter(bedrooms < 15) %>% ggplot(aes(x=bedrooms, y=price, group=bedrooms)) +
   geom_boxplot() +
   scale_x_continuous(breaks = seq(0, 15, 1)) +
   scale_y_continuous(breaks = seq(0, 8000000, 1000000), labels = sprintf("%sM", seq(0, 8, 1))) +
   labs(x = "No of Bedrooms", y = "Price of House ($)", caption = caption_text) + plot_theme

# Influence of number of bathrooms on the price of house 
housedata %>% ggplot(aes(x=bathrooms, y=price, group=bathrooms)) +
   geom_boxplot() +
   scale_x_continuous(breaks = seq(0, 15, 1)) +
   scale_y_continuous(breaks = seq(0, 8000000, 1000000), labels = sprintf("%sM", seq(0, 8, 1))) +
   labs(x = "No of Bathrooms", y = "Price of House ($)", caption = caption_text) + plot_theme



# Influence of number of sqft_living on the price of house 
housedata %>% filter(sqft_living < 10000) %>% ggplot(aes(x=sqft_living, y=price, group=sqft_living)) +
   geom_boxplot() +
   #scale_x_continuous(breaks = seq(0, 15, 1)) +
   scale_y_continuous(breaks = seq(0, 8000000, 1000000), labels = sprintf("%sM", seq(0, 8, 1))) +
   labs(x = "sqft_living", y = "Price of House ($)", caption = caption_text) + plot_theme

# Influence of number of sqft_lot on the price of house 
housedata %>% filter(bedrooms < 15) %>% ggplot(aes(x=sqft_lot, y=price, group=sqft_lot)) +
   geom_boxplot() +
   scale_x_continuous(breaks = seq(0, 15, 1)) +
   scale_y_continuous(breaks = seq(0, 8000000, 1000000), labels = sprintf("%sM", seq(0, 8, 1))) +
   labs(x = "No of Bedrooms", y = "Price of House ($)", caption = caption_text) + plot_theme
# boxplot(price ~ bedrooms, data = housedata, 
#          xlab = "No of Bedrooms",
#          ylab = "Price of House", 
#          col = "maroon",
#          #ylim = sprintf("%sM", seq(0, 8, 0.5)),
#          #names = sprintf("%sM", seq(0, 8, 0.5)),
#          main = "Bedrooms vs Price of House"
#          )

housedata %>% group_by(gr = cut(price/1000000, breaks= seq(0, 10, by = 1)) ) %>% 
               summarise(n = n()) %>%
               arrange(desc(as.numeric(n)))

housedata %>% group_by(gr = cut(price/1000, breaks= seq(0, 20000, by = 100)) ) %>% 
               summarise(n = n()) %>%
               arrange(desc(as.numeric(n)))

housedata %>% qplot(price, )



Reference :
   https://www.tutorialspoint.com/r/r_boxplots.htm










