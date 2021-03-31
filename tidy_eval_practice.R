# Tidy Eval practice and explanation

# How to create a generic modeling workflow that leverages tidy eval

library(datasets)
# data("stackloss")
library(modeldata)
library(tidymodels)
data("concrete")
# library(GGally)
# data("mtcars")
data("mpg")

concrete %>% glimpse()

GGally::ggpairs()

concrete %>% 
  ggpairs()

concrete %>% 
  ggplot(aes(x = compressive_strength, y = fine_aggregate, color = water)) +
  geom_point() +
  title(paste0("Relationship between ",compressive_strength, " and ", fine_aggregate))
  labels(x = "compressive_strength", y = "fine_aggregate")

  library(tidymodels)
  data("concrete")
  
  # Simple selection function
  averages <- function(data, vars) {
    # vars_quo <- enquo(vars) # Method 1
    data %>%
      # select(!!vars_quo) %>% # Method 1
      select({{vars}}) %>% # Method 2
      map_dbl(mean, na.rm = TRUE)
  }
  
  # Both of the following calls on the function yield the same values
  # The quoting and unquoting is handled correctly
  averages(concrete, "superplasticizer")
  
  averages(concrete, superplasticizer)
  
  # Plot the histogram function
  # Where the column variable is used as both part of the dataframe 
  # and as environment character variable
  
  plot_histogram <- function(data_tbl, column){
    # Convert quote to a string for use in labels
    # column <- (is.character(column), sym(column), column)
    # quo_column <- enquo(column)  # Method 1
    q <- rlang::enexpr(column)
    col_string <- rlang::as_string(q)
    
    col_string <- str_to_title(str_replace(col_string, pattern = "_", " "))
    col_mean <- data_tbl %>% summarise(mean({{column}})) %>% pull() # Method 2
    # col_mean <- data_tbl %>% summarise(mean(!!quo_column)) %>% pull() # Method 1
      # Notation for inclusion in dplyr pipes: mutate(!!col_name := mean({{column}}))
    
    # How to flexibly handle if user supplies a string while the function takes a quote?
    # ...
    # ...
    
    return(
      # Using {{column}} notation to capture the quote and then unquote it
      # ggplot(data_tbl, aes(x = !!quo_column)) +  # Method 1
      ggplot(data_tbl, aes(x = {{column}})) +  # Method 2
      geom_histogram(bins = 25)  +
      geom_vline(xintercept = col_mean, linetype = "dotted", color = "white") +
      labs(x = col_string,
           y = "Number") +
      labs(title = paste0("Density Plot for ",col_string))
    )
    }
  
  plot_histogram(concrete, compressive_strength)  # Fails if using character string for column argument
  
# Using tidy evaluation with ggplot
# Using mpg dataset reading code from https://www.tidyverse.org/blog/2018/07/ggplot2-tidy-evaluation/
# You can now use quasiquotation in aes(), facet_wrap(), and facet_grid(). 
# For aes(), quasiquotation (!!, !!!, :=) replaces aes_() and aes_string()
mpg %>% glimpse()
library(hrbrthemes)
# install.packages("hrbrthemes")
# hrbrthemes::import_roboto_condensed()
# library(kableExtra)

mpg %>% glimpse()
 # Prepare data
mpg_by_manufacturer <- mpg %>% 
  group_by(manufacturer) %>%
  summarise(avg_city = mean(cty)) %>%
  arrange(desc(avg_city)) %>%
  mutate(rank = row_number()) %>%
  filter(!is.na(avg_city)) %>%
  mutate(manufacturer = reorder(manufacturer,avg_city)) %>%
  
  # Plot
  ggplot(aes(x=manufacturer, y=avg_city, fill = manufacturer) ) +
  geom_bar(stat="identity") +
  coord_flip() +
  theme_ipsum() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none"
  ) +
  xlab("") +
  ylab("Miles per gallon by Manufacturer")

# Create stacked bar plot
plot_stacked_bar_plot <- function(data, category, summarise_by) {
  
  # Add flexibility to specify function %in% c("mean", "median", "std dev")
  # Select increasing or decreasing
  data %>% 
    group_by({{category}}) %>%
    summarise(statistic = mean({{summarise_by}})) %>%
    arrange(desc(statistic)) %>%
    filter(!is.na(statistic)) %>%
    mutate(category = reorder({{category}},statistic)) %>%
    
    # Plot
    ggplot(aes(x=category, y=statistic, fill = {{category}}) ) +
    geom_bar(stat="identity") +
    coord_flip() +
    theme_ipsum() +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position="none"
    ) +
    xlab("") +
    ylab("Statistic By Category")
  
}

plot_stacked_bar_plot(mpg, category = class, summarise_by = cty)
  
# Create a treemap function ----
library(treemap)
# install.packages("treemap")

# Plot treemap - base function
{treemap(mpg_by_manufacturer,
        
        # data
        index="manufacturer",
        vSize="avg_city",
        type="index",
        
        # Main
        title="",
        palette="Dark2",
        
        # Borders:
        border.col=c("black"),             
        border.lwds=1,                         
        
        # Labels
        fontsize.labels=0.5,
        fontcolor.labels="white",
        fontface.labels=1,            
        bg.labels=c("transparent"),              
        align.labels=c("left", "top"),                                  
        overlap.labels=0.5,
        inflate.labels=T                        # If true, labels are bigger when rectangle is bigger.
        
        
)}

plot_treemap <- function(data, obs, vars) {
  q <- rlang::enexpr(obs)
  obs_string <- rlang::as_string(q)
  p <- rlang::enexpr(vars)
  var_string <- rlang::as_string(p)
  
  # Plot treemap
  treemap(data,
          
          # data
          index= obs_string,
          vSize= var_string,
          type="index",
          
          # Main
          title="",
          palette="Dark2",
          
          # Borders:
          border.col=c("black"),             
          border.lwds=1,                         
          
          # Labels
          fontsize.labels=0.5,
          fontcolor.labels="white",
          fontface.labels=1,            
          bg.labels=c("transparent"),              
          align.labels=c("left", "top"),                                  
          overlap.labels=0.5,
          inflate.labels=T                        # If true, labels are bigger when rectangle is bigger.
          
          
  )
}

plot_treemap(mpg_by_manufacturer, manufacturer, avg_city)
  
  concrete %>%
    summarise(mean(compressive_strength)) %>% pull()
  
  var <- "fine_aggregate"
  q <- rlang::enexpr(var)
  col_string <- rlang::as_string(q)
  col_string <- str_to_title(str_replace(col_string, pattern = "_", " "))
  rlang::qq_show(labs(title = paste0("Density Plot for ",col_string)))
  rlang::qq_show(enexpr(column))
  column <- quote(column)
  
  select(concrete, var)
  
  concrete %>% 
    ggplot(aes(x = compressive_strength)) +
    geom_histogram(bins = 25, aes(y = ..density..)) +
    labs(title = paste0("Density Plot for ",compressive_strength))
  
  ?ggplot
  