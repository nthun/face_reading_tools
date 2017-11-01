# FUNCTION: Calculates binding rectangle coordinates that are missing in the dataset retrieved from the google cloud face api. 
# INPUT: a tibble, that has variables id, xmin, xmax, ymin, and ymax
# OUTPUT: a data frame 
# EXAMPLE: plot_emotions_google("image/Ekman_faces.jpg")


# Correct binding rectangle coordinates by calculating the average area (only applicable when faces are of the same size)data without missing 
# The logic is this: calculate are for all 
# Note that since we may have a max or min value missing, but hopefully not both, and we are using the other coordinate to get the value, we have to caluculate one value twice.

library(dplyr)
correct_coord <- function(df){
    df %>% 
        mutate(xlength = xmax - xmin,
               ylength = ymax - ymin,
               area = xlength * ylength,
               area = ifelse(is.na(area), mean(area, na.rm = TRUE), area), # Imput area with average
               xlength = ifelse(is.na(xlength), (area/ylength) %>% round(), xlength),
               ylength = ifelse(is.na(ylength), (area/ylength) %>% round(), ylength)) %>% 
        transmute(
            id,
            xmax = ifelse(is.na(xmax), (xlength - xmin) %>% round(), xmax),
            xmin = ifelse(is.na(xmin), (xlength - xmax) %>% round() %>% pmax(1), xmin),
            xmax = ifelse(is.na(xmax), (xlength - xmin) %>% round(), xmax), 
            ymax = ifelse(is.na(ymax), (ylength - min) %>% round(), ymax),
            ymin = ifelse(is.na(ymin), (ylength - ymax) %>% round() %>% pmax(1), ymin),
            ymax = ifelse(is.na(ymax), (ylength - min) %>% round(), ymax)
        )
}