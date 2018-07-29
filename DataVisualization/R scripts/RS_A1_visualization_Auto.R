# Graded Assignment 1
# Topic     : Data Visualization in R
# Solution  : Rohan shetty (Jig4010)

# Step 2    : Data Visualization

# Part (d)  : Data Visualization for Sector "Automotive"

# Loading the cleaned dataset prepared during Step 1
BrandsData <- read.csv("brands_cleaned_data.csv",sep = ",", header = TRUE, stringsAsFactors = FALSE)
View(BrandsData)

#Loading dplyr Library
library(dplyr)

#Filter by Automotive Industry
BrandsData%>%filter(Industry=="Automotive")->AutoData
head(AutoData)

#Counting the number of records for Automotive Industry
AutoData%>%nrow()


#Loading ggplot2 library
library(ggplot2)

#Setting the Theme
theme_set(theme_bw())

#Base plot
## Assigned X and Y axis parameters
## Adjusted position of plot title
## Formatted the major grid lines
## Setting Titles for the plot, X and Y Axis 
g <- ggplot(data = AutoData, aes(x = Company.Advertising, y = Brand.Revenue)) + 
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_line(colour="gray")) +
  geom_point(aes(col=Brand, size=Brand.Value)) + 
  labs(title = "Automotive",y="Brand Revenue in Billions of $",
       x="Company Advertising in Billions of $")

#Modifying the Legend. 
##Turning off legend for colour using scale colour function
## Changed the name and breaks of legend for size using scale size function
## Formatted the Legend key by adding rectangle element
## Formatted the size of legend title and text
## Transformed the scale of x and y axis using scale continuous function.
gg <- g + scale_color_discrete(name="Brand",guide = FALSE) + 
  scale_size_continuous(name = "Brand Value $ (Billions)", breaks = c(6.2,20.0,37.8)) +
  theme(legend.key = element_rect(fill=NULL,colour = "black"), 
        legend.text= element_text(size=7),legend.title = element_text(size=8)) +
  scale_x_continuous(breaks = seq(0.0,10.0,0.1)) +  scale_y_continuous(breaks = seq(0,200,10))

#Adding Labels
## Using Brand as label,colour and adjusted its position
gAuto <- gg + geom_text(aes(label=Brand,col=Brand),hjust=0.5,vjust=2)

#plot graph
plot(gAuto)

