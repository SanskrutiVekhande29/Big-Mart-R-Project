setwd(r'(C:\Users\Sanskruti\Downloads)')
df<-read.csv('Train.csv')
install.packages("ggplot2")
install.packages('tidyverse')
install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
install.packages("corrplot") #corrplot function will display a color-coded correlation plot of the selected variables.
install.packages("ggcorrplot")#ggcorrplot is used to create a ggplot-based correlation plot. 
install.packages("cowplot") #Arrange plots using cowplot
library(ggcorrplot)
library(ggplot2)
library(corrplot)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(cowplot)

View(df)
head(df)
str(df)#identifies dataframe

# 1.-----------------------Dimensions,summary,structure ---------------------
#dimensions
dim_df<-dim(df)
dim_df
#summary
summary(df)
#columns ban
col<-colnames(df)
col

#Distinct categories from each category columns
unique(df$Item_Fat_Content)
unique(df$Item_Type)
unique(df$Outlet_Establishment_Year)
unique(df$Outlet_Size)

# -------------------------subset-------------------------

lf<-df[df$Item_Fat_Content=='Low Fat',c('Item_Weight','Item_MRP')]
View(lf)

#to change LF to Low Fat from and reg to Regular unique(df$Item_Fat_Content)
df[df$Item_Fat_Content=='LF' | df$Item_Fat_Content=='low fat',]<-'Low Fat'
df[df$Item_Fat_Content=='reg',]<-'Regular'
unique(df$Item_Fat_Content)

#----------------------Null Values Treatment--------------------

sum(is.na(df))#sum of null values
colSums(is.na(df))#to check which column is the null value

df$Item_Weight<-ifelse((df$Item_Weight=='Low Fat' | df$Item_Weight=='Regular'),0,df$Item_Weight)
df$Item_Weight<-as.numeric(df$Item_Weight)
df$Item_Weight<-replace(df$Item_Weight,is.na(df$Item_Weight),mean(df$Item_Weight,na.rm = T))

View(df[df$Item_Weight==0,])
#the dataframe where we don't want any value 0 
df<-(df[!df$Item_Weight==0,])
df$Item_Outlet_Sales<-as.numeric(df$Item_Outlet_Sales)
df$Outlet_Establishment_Year<-as.numeric(df$Outlet_Establishment_Year)
df$Item_MRP<-as.numeric(df$Item_MRP)
df$Item_Visibility<as.numeric(df$Item_Visibility)
summary(df)
View(df)
#unique(df$Item_Weight)
colSums(is.na(df))

table(df$Outlet_Size)
#maximum value fill
df$Outlet_Size<-ifelse(df$Outlet_Size=='','Medium',df$Outlet_Size)
#after running the above code run the table code

#View(df[df$Outlet_Size=='',])


#maximum value fill

table(df$Item_Fat_Content)
View(table(df$Item_Type))

df_IT<-data.frame(table(df$Item_Type))
#pipe operator #glimpse show the structue
df %>% select(2:4) %>% glimpse()

View(df_IT)
df_IT$Freq

#-------------------------------------Graphs------------------------------------------

#pie chart of item type
p1<-ggplot(df_IT,aes(x='',y=Freq,fill=Var1))+
  geom_col()+
  geom_text(aes(label=paste(round(Freq*100/sum(Freq)),'%')),position = position_stack(vjust = 0.5))+
  coord_polar(theta = 'y')+
  labs(title='percentage of Item_type')+
  theme_void()

p1


#histogram of Outlet Establishment year
p2<-ggplot(df, aes(x = Outlet_Establishment_Year)) +
  geom_histogram(binwidth = 1, fill = "pink", color = "black") +
  labs(title = "Distribution of Outlet Establishment year", x = "Outlet Establishment year ", y = "Frequency")

p2


#bar plot of Item type
p3<-ggplot(df, aes(x = Item_Type)) +
  geom_bar(fill = "lightcoral") +
  labs(title = "Number of Items by Type", x = "Item Type", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p3


#Boxplot of Item Outlet Sales:
p4<-ggplot(df, aes(y = Item_Outlet_Sales)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +
  labs(title = "Distribution of Item Outlet Sales", y = "Item Outlet Sales")
p4


#Correlation 
p5<- cor(df[, c("Outlet_Establishment_Year", "Item_Visibility", "Item_MRP","Item_Outlet_Sales")])
corrplot(p5, method = "color")


#scatter plot of  Item Weight & Item MRP
p6<-ggplot(df, aes(x = Item_Weight, y = Item_MRP)) +
  geom_point(color = "red") +
  labs(title = "Scatterplot of Item Weight vs. Item MRP", x = "Item_Weight", y = "Item MRP")
p6


#Bar plot of Item Type by Outlet Size:
p7<-ggplot(df, aes(x = Item_Type, fill = Outlet_Size)) +
  geom_bar(position = "dodge") +
  labs(title = "Item Type Distribution by Outlet Size", x = "Item Type", y = "Count")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p7


#line plot of Outlet Sales Over Years
p8<-ggplot(df, aes(x = Outlet_Establishment_Year, y = Item_Outlet_Sales, group = 1)) +
  geom_line(color = "black") +
  labs(title = "Item Outlet Sales Over Years", x = "Outlet Establishment Year", y = "Item Outlet Sales")
p8


#------------------Arranging graphs------------------------

# Calculate the correlation matrix
cor_matrix <- cor(df[, c("Outlet_Establishment_Year", "Item_Visibility", "Item_MRP", "Item_Outlet_Sales")])

# Create a ggcorrplot with the default method
p5 <- ggcorrplot(cor_matrix)

# Optionally, add color manually if desired
p5 <- p5 + theme(legend.position = "none")  # Remove color legend
p5 <- p5 + scale_fill_gradient(low = "white", high = "red")  # Add your desired color scale

# Arrange plots using cowplot
library(cowplot)
plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, nrow = 2, ncol = 4)

