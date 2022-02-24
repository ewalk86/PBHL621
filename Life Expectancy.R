
library(readr) #readr is part of the tidyverse

#setwd("C:\\Users\\cindy.leary\\Desktop")
setwd("~/Desktop/R/2022")
le_data<-read_csv("testlifeexpectency.csv")

##Five functions to get to know your data
head(le_data)
tail(le_data)
names(le_data)
str(le_data)
View(le_data)

summary(le_data$LE)

le_data %>%
  


### Life expectancy for White men in California
#Make a scatter plot of the life expectancy for White men in California
#over time.

#Since the dataset contains 39 states across two genders and two races, first use
#a function to subset the data to contain only White men in California.

library(dplyr)
wm_cali <- le_data %>% filter(state == "California", 
                              sex == "Male", 
                              race == "white")


#This is equivalent....

wm_cali <- le_data %>% filter(state == "California" & sex == "Male" & race == "white")

### Set up the ggplot canvas

library(ggplot2)
ggplot(data = wm_cali, aes(x = year, y = LE))

### Second step to building a `ggplot()`: tell `ggplot` how to plot the data
ggplot(data = wm_cali, aes(x = year, y = LE)) + geom_point()


### `labs()` to add a title, a caption, and modify x and y axes titles
ggplot(data = wm_cali, aes(x = year, y = LE)) + geom_point() +
  labs(title = "Life expectancy in White men in California, 1969-2013",
       y = "Life expectancy", 
       x = "Year", 
       caption = "Data from Riddell et al. (2018)")

### `col` controls the color of geom_point()
ggplot(data = wm_cali, aes(x = year, y = LE)) + geom_point(col = "blue") +
  labs(title = "Life expectancy in White men in California, 1969-2013",
       y = "Life expectancy", 
       x = "Year", 
       caption = "Data from Riddell et al. (2018)")

### `size` controls the size of geom_point()
ggplot(data = wm_cali, aes(x = year, y = LE)) + geom_point(col = "blue", size = 4) +
  labs(title = "Life expectancy in White men in California, 1969-2013",
       y = "Life expectancy", 
       x = "Year", 
       caption = "Data from Riddell et al. (2018)")



### Line plot rather than scatter plot
ggplot(data = wm_cali, aes(x = year, y = LE)) + 
    geom_line(col = "blue") +
  labs(title = "Life expectancy in White men in California, 1969-2013",
       y = "Life expectancy", 
       x = "Year", 
       caption = "Data from Riddell et al. (2018)")

### Life expectancy for White and Black men in California
wbm_cali <- le_data %>% filter(state == "California",
                               sex == "Male")

ggplot(data = wbm_cali, aes(x = year, y = LE)) + geom_line(aes(col = race)) +
  labs(title = "Life expectancy in Black and White men in California, 1969-2013",
       y = "Life expectancy", 
       x = "Year", 
       caption = "Data from Riddell et al. (2018)")



### Always use the aes() function to link a plot feature to a variable in your data frame

#The operative word is *link*. Whenever you want to link something about how 
#the plot looks to a variable in the data frame, you need to *link* these items
#inside the `aes()` function:

### What if we also wanted to look at women?
cali_data <- le_data %>% filter(state == "California")
ggplot(data = cali_data, aes(x = year, y = LE)) + geom_line(aes(col = race, lty=sex)) +
  labs(title = "Life expectancy in California, 1969-2013",
       y = "Life expectancy", 
       x = "Year", 
       caption = "Data from Riddell et al. (2018)")

#increase line width
ggplot(data = cali_data, aes(x = year, y = LE)) + geom_line(aes(col = race, lty=sex),size=1) +
  labs(title = "Life expectancy in California, 1969-2013",
       y = "Life expectancy", 
       x = "Year", 
       caption = "Data from Riddell et al. (2018)")

### Use `facet_wrap()` to make separate plots for a specified variable
ggplot(data = cali_data, aes(x = year, y = LE)) + 
  geom_line(aes(col = race),size=1) +
  labs(title = "Life expectancy in California, 1969-2013",
       y = "Life expectancy", 
       x = "Year", 
       caption = "Data from Riddell et al. (2018)") +
  facet_wrap(~ sex) 


### Remove gridlines and Use `facet_wrap()` to make separate 
# plots for a specified variable
ggplot(data = cali_data, aes(x = year, y = LE)) + 
  geom_line(aes(col = race),size=1) +
  labs(title = "Life expectancy in California, 1969-2013",
       y = "Life expectancy", 
       x = "Year", 
       caption = "Data from Riddell et al. (2018)") +
  facet_wrap(~ sex) + theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

### ### Compare two states

#How do we update the `filter` to include data from California and New York?
  
updated_data <- le_data %>% filter(state %in% c("California", "New York"))
ggplot(data = updated_data, aes(x = year, y = LE)) + 
  geom_line(aes(col = race, lty = sex),size=1) +
  labs(title = "Life expectancy in California and New York, 1969-2013",
       y = "Life expectancy", 
       x = "Year", 
       caption = "Data from Riddell et al. (2018)") +
  facet_grid(state ~ sex)


### Question: What is the difference between `facet_wrap()` and 
# `facet_grid()`?
### So far.......
#  - `geom_point()` to make scatter plots
#  - `geom_line()` to make line plots
#  - `col = "blue"`, `size = 2`, `lty = 2`, to change color, size and line type of the `geom`
#  - `aes(col = race)` to *link* color to race
#  - `aes(lty = sex)` to *link* line type to sex
#  - `facet_wrap(~ var1)` to make separate plots for different levels of one variable
#  - `facet_grid(var1 ~ var2)` to make separate plots for combinations of levels of two variables

### What if we wanted to make a histogram...
# of life expectancy of White men in 2013?
# Before you code, try and visualize what the histogram will show
#- What is on the x axis? 
# - What is on the y axis? 
### Update the `filter`
  
wm_data <- le_data %>% filter(year == 2013, sex == "Male", race == "white")
ggplot(dat = wm_data, aes(x = LE)) + geom_histogram()

#change binwidth, outline color, and fill color
#Also change yaxis label
ggplot(dat = wm_data, aes(x = LE)) + 
  geom_histogram(binwidth = 1, col = "white", fill = "forest green")+
ylab("Frequency")


### Apply some of our new skills

data_2013 <- le_data %>% filter(year == 2013)

ggplot(dat = data_2013, aes(x = LE)) + 
  geom_histogram(binwidth = 1, col = "white", aes(fill = sex)) + 
  facet_grid(race ~ sex)

#####################################################################
### Recap: What functions did we learn?
#1. `ggplot()`
#- `geom_scatter()`
#- `geom_line()`
#- `geom_histogram()`
#- `aes()` to link aesthetics to variables in our data frame
#- `facet_wrap(~ var1)`, `facet_grid(var1 ~ var2)`
#- `labs(title = "Main", y = "y axis", x = "x axis", caption = "below plot")`

### Recap: What arguments were useful?

#2. `ggplot()`
#- `col`
#- `size`
#- `lty`


### We only skimmed the surface!

#- You now have a sense of how `ggplot` works, but you might be itching to learn more.
#- [Kieran Healy's data visualization book](https://www.amazon.com/gp/product/0691181624/ref=as_li_tl?ie=UTF8&tag=kieranhealysw-20&camp=1789&creative=9325&linkCode=as2&creativeASIN=0691181624&linkId=16d53b3cc1ec3bc3aac60b27c29b92e8)
#    - [RStudio ggplot2 cheatsheet](https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf)

### Where to ask ggplot2 questions

#- [The RStudio community page](https://community.rstudio.com/)
#- [Stack Overflow](https://stackoverflow.com/)
#- On Twitter using the #rstats hashtag