# Make a function to create output multiple pdf reports from analysis
library(ggplot2movies)
library(magrittr)
library(magick)
library(tidyverse)
# install.packages("magick")
data("movies")

movies %>% glimpse()

# Make a function that creates a chart for each year and saves as png file

save_images <- function(df, filename) {
  
  temp_chart <- df %>%
    mutate(Movie_Genre = reorder(Movie_Genre,n)) %>%
    ggplot(aes(x = Movie_Genre, y = n, fill = Movie_Genre)) +
    geom_col() +
    coord_flip() +
    theme(legend.position = "none") +
    ggtitle(paste0(filename, " Movie Genres"))
  
  ggsave(filename = paste0(filename,".png"), path = "www/", plot = temp_chart, width = 4, 
         height = 3, units = "in")
  
}

# Create a plot for a single year
movies %>%
  pivot_longer(cols = Action:Short, names_to = "Movie_Genre") %>% 
  filter(value == 1) %>% 
  count(year, Movie_Genre) %>% 
  drop_na() %>%
  nest(-year) %$%
  walk2(data, year, save_images)
 

# Create an animated GIF from the output images
# 
## list file names and read in
imgs <- list.files("www/", full.names = TRUE)
img_list <- lapply(imgs, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 1)

## view animated image
img_animated

## save to disk
image_write(image = img_animated,
            path = "output/movie_genre_trends.gif")
