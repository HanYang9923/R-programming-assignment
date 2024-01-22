
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(stats)
library(lmtest)
library(Hmisc)

##  IMPORT DATA
HR=read.csv("C:\\Users\\LuiHanYang99\\Documents\\Assignment\\Assignment Y2 Sem1\\PFDA\\House_Rent_Dataset.csv",header=TRUE,sep=",")

##   DATA PREPROCESSING
# Rename Column
names(HR) =c("Date","BHK","Rent_Price","House_Size","Floor","Type_of_Area","Location"
             ,"City","Furnishing_Status","Tenants","Number_of_Bathroom","Point_of_Contact")

# Reformat column "Floor"
HR = separate(HR,col=Floor,into=c("Floor_Number","TotalNum_Floor"),sep="out of")
HR <- na.omit(HR)

HR$Floor_Number <- replace(HR$Floor_Number, HR$Floor_Number == "Ground ",0)
HR$Floor_Number <- replace(HR$Floor_Number, HR$Floor_Number == "Upper Basement ",-1)
HR$Floor_Number <- replace(HR$Floor_Number,HR$Floor_Number=="Lower Basement ",-2)

# apply suitable type of class to columns
sapply(HR, class) 
HR=type.convert(HR, as.is=TRUE)
HR[c("House_Size","Rent_Price","Floor_Number","TotalNum_Floor")]=
  lapply(HR[c("House_Size","Rent_Price","Floor_Number","TotalNum_Floor")], as.numeric)
sapply(HR, class) 

##   DATA CLEANING
### Remove Outliers
# Detect the outliers of house rents using z-scores
HR$z_scores_ofRentPrice <- scale(HR$Rent_Price)
outlier_threshold <- 3
HR_filtered <- HR[abs(HR$z_scores_ofRentPrice) <= outlier_threshold,]

ggplot(data= HR_filtered,aes(y= Rent_Price))+geom_boxplot(color="blue")+
  ggtitle("Boxplot of Rental Price")+
  theme(plot.title = element_text(hjust = 0.5))

# Filter the potential outliers of house size
HR_filtered <- HR_filtered[HR_filtered$House_Size < 7000,]
HR_filtered <- HR_filtered[HR_filtered$House_Size < 5000,]

ggplot(data= HR_filtered,aes(y= House_Size))+geom_boxplot()+
  ggtitle("Boxplot of House Size")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels = scales::number_format(scale = 1))

# Check for missing values
sum(is.na(HR_filtered))
# Delete rows with missing values
HR_filtered=na.omit(HR_filtered)
sum(is.na(HR_filtered))

# Check for duplicate rows
sum(duplicated(HR_filtered))
# exclude duplicates
HR_filtered=unique(HR_filtered)
sum(duplicated(HR_filtered))

## DATA EXPLORATION
names(HR_filtered)
str(HR_filtered)
View(head(HR_filtered))
View(tail(HR_filtered))
colSums(is.na(HR_filtered)) 
summary(HR_filtered)
round(cor(HR_filtered[c('BHK', 'Rent_Price', 'House_Size', 'Floor_Number','Number_of_Bathroom')]), 2)

# Determine high rental price
mean_HR=mean(HR_filtered$Rent_Price)
mean_HR <- as.integer(mean_HR)
mean_HR #29129
high_HR_filtered=HR_filtered[HR_filtered$Rent_Price > mean_HR, ]

#Determine low rental price
low_HR_filtered=HR_filtered[HR_filtered$Rent_Price < mean_HR, ]

#correlation matrix between columns
round(cor(high_HR_filtered[c('BHK', 'Rent_Price', 'House_Size', 'Number_of_Bathroom')]), 2)
#The correlation matrix shows how the variables are related to each other in terms of their linear relationships.



# Hypothesis: There are above 50% of the high rental price being affected by type of area, house size, furnishing status and
# tenants preferred.

# Objective 4: To find the relationship between the house rent and tenants preferred.

# dependent variable: Rent_Price


# Question 4.1: How do the types of tenants affect the high rental prices?

# Analysis 4.1.1: Find and indicate the relationship between high rental prices and tenants preferred.

  # Find total number of tenants in the range of high rental prices
total_highRent <- nrow(high_HR_filtered) # =1295

  # Proportion of each type of tenants
  # Bachelors 
b_inFilteredHighRent <- nrow(high_HR_filtered[high_HR_filtered$Tenants=="Bachelors",]) # =302
percentage_b <- (b_inFilteredHighRent/total_highRent)*100 # =23.3205%

  # Bachelors/Family
bf_inFilteredHighRent <- nrow(high_HR_filtered[high_HR_filtered$Tenants=="Bachelors/Family",]) # =793
percentage_bf <- (bf_inFilteredHighRent/total_highRent)*100 # =61.2355%

  # Family
f_inFilteredHighRent <- nrow(high_HR_filtered[high_HR_filtered$Tenants=="Family",]) # =200
percentage_f <- (f_inFilteredHighRent/total_highRent)*100 # =15.4440%

  # generate data frame with variable “percentage” and “tenants”
df_pOFt <- data.frame(
  percentage = c(percentage_b,percentage_bf,percentage_f),
  tenants = c("Bachelors","Bachelors/Family","Family")
)

  # pie chart
ggplot(df_pOFt, aes(x="",y=percentage,fill=tenants))+
  geom_bar(stat = "identity",width = 1)+
  coord_polar("y",start = 0)+
  theme_void()+
  ggtitle("Pie chart of the proportion of tenants in range of high rental price")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label = paste0(round(percentage,1),"%")), position = position_stack(vjust = 0.5))

  # Find the mean, first quantile, third quantile and interquartile range of the house rents with different tenants
summary(high_HR_filtered$Rent_Price[high_HR_filtered$Tenants=="Bachelors/Family"])
summary(high_HR_filtered$Rent_Price[high_HR_filtered$Tenants=="Bachelors"])
summary(high_HR_filtered$Rent_Price[high_HR_filtered$Tenants=="Family"])

  # Plot violin plot
ggplot(data = high_HR_filtered, aes(x =Tenants, y =Rent_Price,color=Tenants)) +
  geom_violin() +
  geom_boxplot(width=0.1)+
  stat_summary(fun.y=mean, geom="point", shape=23, size=2,color="black")+
  labs(title = "Violin Plot of Distribution of High Rental Price", x = "Tenants", y = "Rent_Price") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels = scales::number_format(scale = 1))

# Analysis 4.1.2: Find the potential percentage of high rental prices being affected by the categories of tenants.

  # ANOVA
anova_HR_Tenants <- aov(high_HR_filtered$Rent_Price ~ high_HR_filtered$Tenants, high_HR_filtered = high_HR_filtered)
summary(anova_HR_Tenants)

  # Tukey HSD test
tukey_HR_Tenants <- TukeyHSD(anova_HR_Tenants)
print(tukey_HR_Tenants)

  # Linear regression- R-squared
model_highHR_tenants<- lm(high_HR_filtered$Rent_Price ~ high_HR_filtered$Tenants, high_HR_filtered = high_HR_filtered)

  # Print the summary of the regression model
summary(model_highHR_tenants)


  # Calculate R-squared value
r_squared_highHR_tenants<- summary(model_highHR_tenants)$r.squared

  # Calculate potential percentage affected by tenants' preferences
percentage_affected_tenants<- paste0(round(r_squared_highHR_tenants * 100,2),"%")
percentage_affected_tenants


# Question 4.2: If other factors are added into the analysis, do they have an impact on the high rental prices based on the types of tenants preferred?

# Analysis 4.2.1: Find the impact of cities on the high rental prices.

unique(HR$City) # 6 different cities

ggplot(data= high_HR_filtered,aes(y=Rent_Price,x=City,color=Tenants))+geom_boxplot()+
  ggtitle("Boxplot of High Rental Prices against City by Tenants")+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~Tenants)+
  scale_y_continuous(labels = scales::number_format(scale = 1))

  # Create a temporary data frame to store necessary data
portion_CityByTenants <- high_HR_filtered %>%
  group_by(City, Tenants) %>%
  summarise(Count = n()) %>%
  group_by(City)

  # Create the bar plot
ggplot(portion_CityByTenants, aes(x=Tenants,y=Count,fill=Tenants))+
  geom_bar(stat = "identity",width = 0.5)+
  facet_wrap(~City)+
  labs(title="Bar plot of Number of Tenants Preferred in Different Cities",x="Tenant Categories", y="Tenant Count")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label = Count), vjust = -0.5)

  #Performing chi-square test for city and tenant preferred
tenants_City <- table(high_HR_filtered$Tenants, high_HR_filtered$City)
tenants_City

chi_square_tenants_City <- chisq.test(tenants_City)
chi_square_tenants_City

  # ANOVA
anova_HR_tenantsByCity <- aov(high_HR_filtered$Rent_Price ~ high_HR_filtered$Tenants+ 
                                high_HR_filtered$City, high_HR_filtered = high_HR_filtered)
summary(anova_HR_tenantsByCity)

  # Linear regression- R-squared
model_highHR_tenantsByCity<- lm(high_HR_filtered$Rent_Price ~ high_HR_filtered$City+ 
                                  high_HR_filtered$Tenants, high_HR_filtered = high_HR_filtered)

  # Print the summary of the regression model
summary(model_highHR_tenantsByCity)

  # Calculate R-squared value
r_squared_highHR_tenantsByCity<- summary(model_highHR_tenantsByCity)$r.squared

  # Calculate potential percentage affected by tenants' preferences
percentage_affected_tenantsByCity<- paste0(round(r_squared_highHR_tenantsByCity * 100,2),"%")
percentage_affected_tenantsByCity 


# Analysis 4.2.2: Find the potential percentage of high rental prices being affected by the cities, floor number and tenants preferred.

  # By using boxplot, shorten the range and filter out the potential outliers of floor number.
ggplot(high_HR_filtered,aes(y=Floor_Number))+geom_boxplot()
high_HR_filtered <- high_HR_filtered[high_HR_filtered$Floor_Number < 50,]

  # Plot the boxplot to identify the relationship
ggplot(high_HR_filtered, aes(x = Floor_Number, y = Rent_Price, color = Tenants)) +
  geom_jitter() +
  facet_wrap(~ City) +
  ggtitle("Scatterplot of Relationship Between Rental Price, Tenants, Floor Number and City") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(labels = scales::number_format(scale = 1))+
  labs(x = "Floor Number", y = "House Rent")


  # Linear regression- R-squared
model_highHR_tenants_City_Floor<- lm(high_HR_filtered$Rent_Price ~ high_HR_filtered$City+ high_HR_filtered$Tenants+ 
                                       high_HR_filtered$Floor_Number, high_HR_filtered = high_HR_filtered)

  # Print the summary of the regression model
summary(model_highHR_tenants_City_Floor)

  # Calculate R-squared value
r_squared_highHR_tenants_City_Floor<- summary(model_highHR_tenants_City_Floor)$r.squared

  # Calculate potential percentage affected by tenants' preferences
percentage_affected_tenants_City_Floor<- paste0(round(r_squared_highHR_tenants_City_Floor * 100,2),"%")
percentage_affected_tenants_City_Floor

# Analysis 4.2.3: Identify the relationship between city, tenants, house rent and house size.

ggplot(high_HR_filtered, aes(x = House_Size, y = Rent_Price, color = Tenants)) +
  geom_jitter() +
  facet_wrap(~ City) +
  ggtitle("Scatterplot of Relationship Between Rental Price, Tenants, Floor Number and City") +
  theme(plot.title = element_text(hjust = 0.5))+
  stat_smooth(method = lm)+
  scale_y_continuous(labels = scales::number_format(scale = 1))+
  labs(x = "House Size", y = "House Rent")


  # Linear regression- R-squared
model_highHR_tenants_City_Size<- lm(high_HR_filtered$Rent_Price ~ high_HR_filtered$City+ high_HR_filtered$Tenants+ 
                                      high_HR_filtered$House_Size, high_HR_filtered = high_HR_filtered)

  # Print the summary of the regression model
summary(model_highHR_tenants_City_Size)

  # Calculate R-squared value
r_squared_highHR_tenants_City_Size<- summary(model_highHR_tenants_City_Size)$r.squared

  # Calculate potential percentage affected by tenants' preferences
percentage_affected_tenants_City_Size<- paste0(round(r_squared_highHR_tenants_City_Size * 100,2),"%")
percentage_affected_tenants_City_Size


# Analysis 4.2.4: Identify the effect of the number of bathrooms on the house rents.

  # In the range of high rental prices, find the houses with many bathrooms but house rents are lower than 200000.
nrow(high_HR_filtered[(high_HR_filtered$Number_of_Bathroom > 5) & (high_HR_filtered$Rent_Price < 200000),])

  # Plot the scatter plot
ggplot(high_HR_filtered, aes(x=Number_of_Bathroom, y=Rent_Price,color=Tenants))+
  geom_point(aes(shape = factor(Tenants), colour = factor(Tenants)))+
  ggtitle("Boxplot of High Rental Prices Vs Number of Bathroom based on Tenants")+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~Tenants)+
  scale_y_continuous(labels = scales::number_format(scale = 1))

  # Find the mean of house rents based on number of bathrooms
mean_rentsbyNumOfBathroom <- high_HR_filtered %>%
  group_by(Tenants, Number_of_Bathroom) %>%
  summarise(mean_rent = mean(Rent_Price))

ggplot(mean_rentsbyNumOfBathroom, aes(x=Number_of_Bathroom, y=mean_rent,fill = Tenants))+
  geom_bar(stat="identity")+
  labs(title = "Bar Chart of Hgih Rental Prices Vs Number of Bathroom based on Tenants",
       x = "Number of Bathroom" ,
       y = "Mean House Rents")+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~Tenants)+
  scale_y_continuous(labels = scales::number_format(scale = 1))

  # Change data type to operate linear regression analysis
high_HR_filtered$Number_of_Bathroom <- as.character(high_HR_filtered$Number_of_Bathroom)

  # Linear regression- R-squared
model_highHR_tenants_Bathroom<- lm(high_HR_filtered$Rent_Price ~ high_HR_filtered$Tenants+ 
                                     high_HR_filtered$Number_of_Bathroom, high_HR_filtered = high_HR_filtered)

  # Print the summary of the regression model
summary(model_highHR_tenants_Bathroom)

  # Calculate R-squared value
r_squared_highHR_tenants_Bathroom<- summary(model_highHR_tenants_Bathroom)$r.squared

  # Calculate potential percentage affected by tenants' preferences
percentage_affected_tenants_Bathroom<- paste0(round(r_squared_highHR_tenants_Bathroom * 100,2),"%")
percentage_affected_tenants_Bathroom

  # Change back the data type
high_HR_filtered$Number_of_Bathroom <- as.integer(high_HR_filtered$Number_of_Bathroom)

    # Extra: Rent_Price vs (Tenants, Number_of_Bathroom, BHK)
    # Change data type of BHK to categorical value temporarily
  high_HR_filtered$BHK <- as.character(high_HR_filtered$BHK)
  
  ggplot(high_HR_filtered, aes(x = Number_of_Bathroom, y = Rent_Price, color = BHK)) +
    geom_jitter() +
    facet_wrap(~ Tenants) +
    ggtitle("Scatterplot of Relationship Between Rental Price, Tenants, BHK and Number of Bathrooms") +
    theme(plot.title = element_text(hjust = 0.5))+
    scale_y_continuous(labels = scales::number_format(scale = 1))+
    labs(x = "Number of Bathrooms", y = "House Rent")
  
  
    # Change data type to operate linear regression analysis
  high_HR_filtered$Number_of_Bathroom <- as.character(high_HR_filtered$Number_of_Bathroom)
  
    # Linear regression- R-squared
  model_highHR_tenants_Bathroom_BHK<- lm(high_HR_filtered$Rent_Price ~ high_HR_filtered$Tenants+ high_HR_filtered$Number_of_Bathroom+ 
                                           high_HR_filtered$BHK, high_HR_filtered = high_HR_filtered)
  
    # Print the summary of the regression model
  summary(model_highHR_tenants_Bathroom_BHK)
  
    # Calculate R-squared value
  r_squared_highHR_tenants_Bathroom_BHK<- summary(model_highHR_tenants_Bathroom_BHK)$r.squared
  
    # Calculate potential percentage affected by tenants' preferences
  percentage_affected_tenants_Bathroom_BHK<- paste0(round(r_squared_highHR_tenants_Bathroom_BHK * 100,2),"%")
  percentage_affected_tenants_Bathroom_BHK
  
    # Change back the data type
  high_HR_filtered$BHK <- as.integer(high_HR_filtered$BHK)
  high_HR_filtered$Number_of_Bathroom <- as.integer(high_HR_filtered$Number_of_Bathroom)


# Analysis 4.2.5: Find the relationship of house rents against point of contact by house rents through the boxplots.

unique(HR$Point_of_Contact) # 3 types
  
num_of_contactBuilder <- nrow(HR[HR$Point_of_Contact == "Contact Builder",])
num_of_contactBuilder # =1
  
ggplot(data=high_HR_filtered,aes(y=Rent_Price,x=Point_of_Contact,color=Tenants))+
    geom_boxplot()+
    ggtitle("High Rental Prices Vs Point of Contact based on Types of Tenants")+
    theme(plot.title = element_text(hjust = 0.5))+
    facet_wrap(~Tenants)+
    scale_y_continuous(labels = scales::number_format(scale = 1))
  # High rental prices: Contact Agent > Contact Owner, Contact Builder = 0

  # ANOVA
anova_HR_tenants_POC <- aov(high_HR_filtered$Rent_Price ~ high_HR_filtered$Tenants+ 
                              high_HR_filtered$Point_of_Contact, high_HR_filtered = high_HR_filtered)
summary(anova_HR_tenants_POC)

  # Tukey HSD test
tukey_HR_tenants_POC <- TukeyHSD(anova_HR_tenants_POC)
print(tukey_HR_tenants_POC)

  # Linear regression- R-squared
model_highHR_tenants_POC<- lm(high_HR_filtered$Rent_Price ~ high_HR_filtered$Tenants+ high_HR_filtered$Point_of_Contact 
                              , high_HR_filtered = high_HR_filtered)

  # Print the summary of the regression model
summary(model_highHR_tenants_POC)

  # Calculate R-squared value
r_squared_highHR_tenants_POC<- summary(model_highHR_tenants_POC)$r.squared

  # Calculate potential percentage affected by tenants' preferences
percentage_affected_tenants_POC<- paste0(round(r_squared_highHR_tenants_POC * 100,2),"%")
percentage_affected_tenants_POC

    # Extra: Rent_Price vs (Point_of_Contact, Tenants, City)
  ggplot(high_HR_filtered, aes(x = Point_of_Contact, y = Rent_Price, color = Tenants)) +
    geom_jitter() +
    facet_wrap(~ City) +
    ggtitle("Scatterplot of Relationship Between Rental Price, Tenants, Point of Contact and Cities") +
    theme(plot.title = element_text(hjust = 0.5))+
    scale_y_continuous(labels = scales::number_format(scale = 1))+
    labs(x = "Point of Coantact", y = "House Rent")

    # Extra: Rent_Price vs (Point_of_Contact, Tenants, Furnishing_Status)
  ggplot(high_HR_filtered, aes(x = Point_of_Contact, y = Rent_Price, color = Tenants)) +
    geom_jitter() +
    facet_wrap(~ Furnishing_Status) +
    ggtitle("Scatterplot of Relationship Between Rental Price, Tenants, Point of Contact and Furnishing Status") +
    theme(plot.title = element_text(hjust = 0.5))+
    scale_y_continuous(labels = scales::number_format(scale = 1))+
    labs(x = "Point of Contact", y = "House Rent")

# Analysis 4.2.6: Find out which variables can affect more than 50% of high rental prices when combined.

  # Change data type to categorical variable
high_HR_filtered$Number_of_Bathroom <- as.character(high_HR_filtered$Number_of_Bathroom)

  # Linear regression- R-squared
model_highHR_50percent <- lm(high_HR_filtered$Rent_Price ~ high_HR_filtered$House_Size+ high_HR_filtered$Tenants+
                              high_HR_filtered$Type_of_Area+ high_HR_filtered$City+ 
                              high_HR_filtered$Number_of_Bathroom+ high_HR_filtered$Furnishing_Status+ 
                              high_HR_filtered$Point_of_Contact, high_HR_filtered = high_HR_filtered)

  # Print the summary of the regression model
summary(model_highHR_50percent)

  # Calculate R-squared value
r_squared_highHR_50percent <- summary(model_highHR_50percent)$r.squared

  # Calculate potential percentage affected by tenants' preferences
percentage_above_50percent <- paste0(round(r_squared_highHR_50percent * 100,2),"%")
percentage_above_50percent

  # Change back data type
high_HR_filtered$Number_of_Bathroom <- as.integer(high_HR_filtered$Number_of_Bathroom)




# Conclusion:
  # Linear regression- R-squared
  model_highHR_Area_Size_Furnishing_Tenants<- lm(high_HR_filtered$Rent_Price ~ high_HR_filtered$House_Size+ high_HR_filtered$Tenants+
                                                   high_HR_filtered$Type_of_Area+ high_HR_filtered$Furnishing_Status, high_HR_filtered = high_HR_filtered)
  
  # Print the summary of the regression model
  summary(model_highHR_Area_Size_Furnishing_Tenants)
  
  # Calculate R-squared value
  r_squared_highHR_Area_Size_Furnishing_Tenants<- summary(model_highHR_Area_Size_Furnishing_Tenants)$r.squared
  
  # Calculate potential percentage affected by tenants' preferences
  percentage_affected_Area_Size_Furnishing_Tenants<- paste0(round(r_squared_highHR_Area_Size_Furnishing_Tenants * 100,2),"%")
  percentage_affected_Area_Size_Furnishing_Tenants
  