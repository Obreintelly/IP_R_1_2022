# 1. Statement of the Problem
#A Kenyan entrepreneur has created an online cryptography course and would want to advertise it on her blog. 
#She currently targets audiences originating from various countries. 
#In the past, she ran ads to advertise a related course on the same 
#blog and collected data in the process. 
#She would now like to employ your services as a Data Science Consultant 
#to help her identify which individuals are most likely to click on her ads.


#Metric for Success



#Experimental Design
#1. Data Cleaning
#2. Data Exploration
#3. Recommendations & Conclusions

#Downloading the relevant Packages

#install.packages("Hmisc")

#install.packages("ggthemes")

#install.packages("moments")

#install.packages("corrplot")

#install.packages("DataExplorer")



#Loading the relevant libraries

library(data.table)

library(tidyverse)

library(ggplot2)

library(Hmisc)

library(ggthemes)

library(moments)

library(corrplot)

library(DataExplorer)

#Loading the Dataset

advert <- fread('http://bit.ly/IPAdvertisingData')

#Data Exploration

#Checking the first 6 rows
head(advert)

#Checking the last 6 rows

tail(advert)

#Data Structure
str(advert)

#Dimension of Dataset
dim(advert)

#We have 1000 rows and 10 columns in the dataset

#Checking the Data Types of the columns
sapply(advert, class)

#3. Data Cleaning

# Standardize column names by using upper case and replacing the 
#spaces with underscores using gsub() function

names(advert) <- gsub(" ","_", names(advert))

# lower the case of the column names using toupper() function
names(advert) <- toupper(names(advert))

# Confirming the changes
colnames(advert)

#Checking for Missing Data in columns using the colSums & is.na

colSums(is.na(advert))

#There are no missing entries in the dataset

#Checking for Duplicates in the Dataset
anyDuplicated((advert))

#There are no duplicated records in the Dataset

#Renaming the Columns to make them precise
names(advert)[1] <- "BROWSE_TIME"

names(advert)[4] <- "NET_USAGE"

names(advert)[10] <- "CLICKS"

names(advert)[5]  <- "TOPIC"

names(advert)[3]  <- "INCOME"
names(advert)[7]  <- 'GENDER'

#Preview Dataset
head(advert, 3)

#Checking for Unique Values in the Gender Column to ensure 
#alignment with expectations

distinct(select(advert, GENDER ))

#Gender column consists of expected values 0 & 1

#Checking for unique values in the Number of Clicks per Ad
distinct(select(advert, CLICKS))

#Clicks column has expected values of 0 for NO and 1 for Yes

#Gender and Clicks are erroneously classed as integers
#They are categorical features. Therefore we convert them
#to factors

advert$GENDER <- factor(advert$GENDER)

advert$CLICKS <- factor(advert$CLICKS)

#Checking Structure of Data
str(advert)


#Outlier Detection
#Checking for Outliers in the Income Column

advert %>%
  ggplot(aes(x= 1, y=INCOME)) +
  geom_boxplot(fill = "grey", color= 'blue') +
  ggtitle("Outlier Detection in the Income Column") + 
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 20))

#We have about 8 outliers in the dataset that represent actual 
#income levels of individuals. We will not drop them from the 
#dataset as they are actual datapoints.
#Checking for Outliers in the Age Column
advert %>%
  ggplot(aes(x= 1, y=AGE)) +
  geom_boxplot(fill= 'blue') +
  ggtitle("Outlier Detection in the Age Column") + 
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 20))

# plot a boxplot to check for outliers in the 'Net_Usage' column
boxplot(advert$NET_USAGE, main="Boxplot for Internet Usage", 
        xlab = "Daily Internet Usage", ylab = "Value", boxwex=0.4, cex.main=2,
        cex.lab=1.5, cex.axis=1.2)

# display number of outlier values in the column
outlier_NetUSage <- boxplot.stats(advert$NET_USAGE)$out
mtext(paste("Outliers: ", paste(length(outlier_NetUSage), collapse=", ")), 
      cex=1)

#With the exception of the Individual Income Level which had circa eight 
#outliers on the higher side, the rest of the columns had no outliers.Given that
#the outlier values are valid data points, we make the decision to retain them 
#in the dataset.

#Leveraging power of Regular Expressions to check for non-charnumeric values
sum(grepl(':', advert))

#There are no non-charnumeric values


#FEATURE ENGINEERING

#Additional Feature Engineering to get the Gender factors to easily comprehensible
#types

# replace the ones and zeros in 'gender' column with 'male' and 'female' using 
#the ifelse() function

advert$GENDER <- ifelse(advert$GENDER == 1,"Male", "Female")

advert$CLICKS <- ifelse(advert$CLICKS == 1, "Yes", "No")


#Grouping Countries by Continent

AFRICA <- advert %>%
  mutate(AFRICA = COUNTRY %in% c("Lesotho", "Mozambique", "Namibia", "Cape Verde", 
                                 "Comoros", "Ethiopia", "Mali", "Djibouti", "Sudan", 
                                 "Cameroon","Egypt", "Burundi", "Ghana", "Tunisia"))

EUROPE <- advert %>% 
  mutate(EUROPE = COUNTRY %in% c("Slovakia (Slovak Republic)", "Andorra",
                                 "Denmark", "Slovenia", "Romania", "Isle of Man",
                                 "Greece", "Monaco", "Russian Federation", "Spain",
                                 "Bosnia and Herzegovina", "Norway", "Iceland",
                                 "Italy", "San Marino"))
ASIA <- advert %>% 
  mutate(ASIA = COUNTRY %in% c("Armenia", "Kiribati", "Marshall Islands", 
                               "India", "Nepal", "Vanuatu", "Macao", "Tuvalu" ,
                               "Tokelau" , "Korea", 
                               "British Indian Ocean Territory (Chagos Archipelago)", 
                               "Australia", "Myanmar","Nauru"))
AMERICA <- advert %>% 
  mutate(AMERICA = COUNTRY %in% c("South Georgia and the South Sandwich Islands",
                                  "Uruguay", "Cayman Islands", "United States Virgin Islands",
                                  "Aruba", "Peru", "British Virgin Islands", 
                                  "Bouvet Island (Bouvetoya)" , "Barbados", "Grenada" ))
MID_EAST <- advert %>%
  mutate(MID_EAST = COUNTRY %in% c("Syrian Arab Republic","Yemen", "Afghanistan",
                                   "Palestinian Territory" , "Qatar"  ))

#Creating Region Column in Our Dataset
advert <- mutate (advert, REGION = ifelse(COUNTRY %in% c("Congo", "Uganda", "Sierra Leone", "Angola", "Benin", "Central African Republic", "Equatorial Guinea", "Reunion", "Seychelles","Libyan Arab Jamahiriya", "Eritrea", "Tanzania", "Burkina Faso","Zimbabwe", "Swaziland", "Kenya", "Chad", "Gabon", "Mauritania", "Sao Tome and Principe", "Cote d'Ivoire","Western Sahara", "Gambia", "Algeria", "Guinea", "Morocco", "Zambia", "Guinea-Bissau","Rwanda", "Togo", "South Africa","Somalia","Madagascar","Liberia","Senegal","Malawi","Niger","Lesotho", "Mozambique", "Mayotte","Namibia", "Cape Verde", "Comoros", "Ethiopia", "Mali", "Djibouti", "Sudan", "Cameroon", "Egypt", "Burundi", "Ghana", "Tunisia"),"AFRICA",
                                          ifelse(COUNTRY %in% c("Saint Barthelemy", "Germany", "Pitcairn Islands", "Bermuda", "United Kingdom", "Hungary", "Netherlands", "Liechtenstein", "Gibraltar", "Portugal", "Georgia", "Sweden", "Ukraine", "Faroe Islands", "Switzerland", "Greenland", "Martinique", "Guernsey", "Albania", "Holy See (Vatican City State)", "Finland", "Croatia", "French Southern Territories", "Serbia", "Austria", "Estonia","Belgium", "Turkey", "Moldova", "French Polynesia","Latvia","Falkland Islands (Malvinas)", "Jersey", "Macedonia","Netherlands Antilles","Svalbard & Jan Mayen Islands","Belarus","Malta", "Ireland","Lithuania", "Czech Republic", "Luxembourg","Cyprus","Montenegro","Saint Pierre and Miquelon","France", "Bulgaria", "Poland","Slovakia (Slovak Republic)", "Andorra", "Denmark", "Slovenia", "Romania", "Isle of Man", "Greece", "Monaco", "Russian Federation", "Spain", "Bosnia and Herzegovina", "Norway", "Iceland", "Italy", "San Marino"), "EUROPE",
                                                 ifelse(COUNTRY %in% c("Saint Martin", "Panama", "Guam", "Argentina", "Costa Rica", "Belize", "Trinidad and Tobago", "Montserrat", "Nicaragua", "Turks and Caicos Islands", "Saint Lucia", "Saint Helena","Dominica", "Cuba", "Saint Vincent and the Grenadines", "Antigua and Barbuda","Saint Kitts and Nevis","Anguilla", "United States Minor Outlying Islands","United States of America", "Suriname", "Chile", "French Guiana", "Guadeloupe", "Puerto Rico", "American Samoa", "Jamaica", "Honduras","Bolivia", "El Salvador","Venezuela","Paraguay","Dominican Republic","Bahamas", "Guyana","Ecuador","Haiti", "Colombia","Canada","Northern Mariana Islands","Mexico","Brazil","Guatemala","South Georgia and the South Sandwich Islands", "Uruguay", "Cayman Islands", "United States Virgin Islands", "Aruba", "Peru", "British Virgin Islands", "Bouvet Island (Bouvetoya)" , "Barbados", "Grenada"), "AMERICA",
                                                        ifelse(COUNTRY %in% c("Niue", "Mauritius", "Fiji", "Vietnam", "Kazakhstan", "Bhutan", "Christmas Island", "Azerbaijan",  "Cook Islands", "Antarctica (the territory South of 60 deg S)", "Papua New Guinea", "China", "Thailand", "Indonesia", "Bangladesh", "Samoa","New Caledonia","Taiwan", "Wallis and Futuna","New Zealand", "Sri Lanka", "Micronesia", "Tajikistan","Kyrgyz Republic","Pakistan", "Malaysia","Timor-Leste","Heard Island and McDonald Islands","Lao People's Democratic Republic","Singapore","Maldives", "Philippines", "Cambodia", "Norfolk Island","Japan","Hong Kong", "Palau","Tonga","Brunei Darussalam","Uzbekistan","Mongolia","Armenia", "Kiribati", "Marshall Islands", "India", "Nepal", "Vanuatu", "Macao", "Tuvalu" , "Tokelau" , "Korea", "British Indian Ocean Territory (Chagos Archipelago)", "Australia", "Myanmar","Nauru"), "ASIA",
                                                               ifelse(COUNTRY %in% c("Kuwait", "Jordan","Turkmenistan", "Israel", "Saudi Arabia", "Iran","United Arab Emirates","Bahrain","Syrian Arab Republic","Yemen", "Afghanistan","Palestinian Territory","Qatar", "Lebanon"), "MID_EAST","OTHER_REGION"))))))


#Subsetting the Other Region Sub-classification to ensure we have all the countries
#in the Region Column
OTHER <- subset(advert, advert$REGION == "OTHER_REGION")

OTHER

#Previewing the dataset
tail(advert)

#We will Split Date and Time from Timestamp in order to carry out further analysis
advert$DATE <- as.Date(advert$TIMESTAMP)
advert$TIME <- format(as.POSIXct(advert$TIMESTAMP), format = "%H:%M:%S")

#Extracting time from the date/time stamp

advert <- advert %>% separate(TIME, c("HOUR", "MINUTE", "SECONDS"))

#Apportioning the Hour Column into features that can be analyzed
advert$HOUR = ifelse(advert$HOUR >= "00" & advert$HOUR <= "06", "Wee Hours",
                     ifelse(advert$HOUR >= "07" & advert$HOUR <= "12", "Morning Hours",
                            ifelse(advert$HOUR >= "13" & advert$HOUR <= "18", 
                                   "Afternoon Hours", "Night")))


#Previewing the dataset
head(advert)

#Dropping Columns we don't need for analysis

advert <- select(advert, -c(TOPIC, CITY, TIMESTAMP, MINUTE, DATE, SECONDS))


numeric <- select(advert, c(BROWSE_TIME, AGE, INCOME, NET_USAGE) )

non.numeric <- select(advert, c(GENDER, COUNTRY, CLICKS, REGION, HOUR))

#EXPLORATORY DATA ANALYSIS

#UNIVARIATE ANALYSIS


#Measures of Central Tendency
#Summary of the numeric values using the function summary
summary(numeric)

#The average Browse time was 65, average age of users 36 years, average region income 
#being 55000 and the average network usage 180.

#The maximum time spent online was 91.43 while the least was 32.60
#The oldest person online was age 61 whilst the youngest was only 19

#The highest area income was around 79000 whilst the least was around 14000

#The highest internet usage per day was 270 whilst the least was 105


#Description of the entire Dataset using the Describe function

describe(advert)

# Pie-chart displaying the distribution of the countries in the Dataset

region_perc <- advert %>%
  filter(REGION != "NA") %>%
  group_by(REGION) %>%
  count() %>%
  ungroup() %>%
  arrange(desc(REGION)) %>%
  mutate( percentage = round(n/sum(n), 1)*100, lab.pos = cumsum(percentage)- 0.5 * percentage)
ggplot(region_perc, aes(x = "", y= percentage, fill = REGION)) +
  geom_bar(stat = "identity")+
  coord_polar("y", start = 200) +
  geom_text(aes(y = lab.pos, label = paste(percentage,"%", sep = "")), col = "black") +
  theme_void() + scale_fill_brewer(palette = "Set1") + labs(title= "Distribution of Countries in 2016 Dataset") + 
  theme(plot.title = element_text(hjust = 0.4, size = 20))
#Europe was the most represented region in the dataset whilst the Mid_East was
#the least represented


#Display of the most active hours
hour_perc <- advert %>%
  filter(HOUR != "NA") %>%
  group_by(HOUR) %>%
  count() %>%
  ungroup() %>%
  arrange(desc(HOUR)) %>%
  mutate( percentage = round(n/sum(n), 1)*100, lab.pos = cumsum(percentage)- 0.5 * percentage)
ggplot(hour_perc, aes(x = "", y= percentage, fill = HOUR)) +
  geom_bar(stat = "identity")+
  coord_polar("y", start = 200) +
  geom_text(aes(y = lab.pos, label = paste(percentage,"%", sep = "")), col = "black") +
  theme_void() + scale_fill_brewer(palette = "PRGn") + labs(title= "Distribution of Activity by Hour in the 2016 Data") + 
  theme(plot.title = element_text(hjust = 0.4, size = 20))


#Most browsing activity took place in the wee Hours of the night and the morning
#hours


#Display of whether an advert was clicked or not

click_perc <- advert %>%
  filter(CLICKS != "NA") %>%
  group_by(CLICKS) %>%
  count() %>%
  ungroup() %>%
  arrange(desc(CLICKS)) %>%
  mutate( percentage = round(n/sum(n), 1)*100, lab.pos = cumsum(percentage)- 0.5 * percentage)
ggplot(click_perc, aes(x = "", y= percentage, fill = CLICKS)) +
  geom_bar(stat = "identity")+
  coord_polar("y", start = 200) +
  geom_text(aes(y = lab.pos, label = paste(percentage,"%", sep = "")), col = "black") +
  theme_void() + scale_fill_brewer(palette = "Set1") + labs(title= "Distribution of Site Clicks in 2016") + 
  theme(plot.title = element_text(hjust = 0.4, size = 20))

# There was no split on whether an advert was clicked or not. There was always 
#a 50% chance that a user would click on an advert


#Plotting Pie Chart for Gender Distribution

#Filtering the gender df 
pie_gender <- advert %>%
  filter(GENDER != "NA") %>%
  group_by(GENDER) %>%
  count() %>%
  ungroup() %>%
  arrange(desc(GENDER)) %>%
  mutate( percentage = round(n/sum(n), 2)*100, lab.pos = cumsum(percentage)- 0.5 * percentage)
ggplot(pie_gender, aes(x = "", y= percentage, fill = GENDER)) +
  geom_bar(stat = "identity")+
  coord_polar("y", start = 200) +
  geom_text(aes(y = lab.pos, label = paste(percentage,"%", sep = "")), col = "black") +
  theme_void() + scale_fill_brewer(palette = "Spectral") + labs(title= "Gender Distribution in 2016") + 
  theme(plot.title = element_text(hjust = 0.4, size = 20))

#It appears that more women in the dataset browsed on the internet

#Density Plot Distribution of the Age Column
ggplot(advert, aes(x= AGE)) + 
  geom_density(bw = 5) + 
  ggtitle("Density Plot of Age") + 
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 20))
#The individuals in the dataset were between ages 19 and 61 with the median age
#being around 35 years.

# Histogram for Daily time spent on site

                                
ggplot(advert, aes(x = `NET_USAGE` )) +
  geom_histogram(fill = "cornflowerblue", 
                 color = "white",bins = 20) + 
  theme_gdocs() +                                 
  labs(title="NET_USAGE",
       x = "USAGE", y = "Frequency")


# Skewness and kurtosis of Daily Browsing
cat('The skewness and kurtosis of daily browsing', '\n')
cat("Skewness: ", skewness(advert$BROWSE_TIME), '\n')
cat("Kurtosis: ", kurtosis(advert$BROWSE_TIME), '\n')
cat("Variance: ", var(advert$BROWSE_TIME),  '\n')
cat("Standard Deviation: ", sd(advert$BROWSE_TIME),  '\n')

#Skewness, variance, standard deviation and Kurtosis of Income

cat('The skewness and kurtosis of Area Income', '\n')
cat("Skewness: ", skewness(advert$INCOME), '\n')
cat("Kurtosis: ", kurtosis(advert$INCOME), '\n')
cat("Variance: ", var(advert$INCOME),  '\n')
cat("Standard Deviation: ", sd(advert$INCOME),  '\n')

#Skewness and Kurtosis of Age

cat('The skewness and kurtosis of Age', '\n')
cat("Skewness: ", skewness(advert$AGE), '\n')
cat("Kurtosis: ", kurtosis(advert$AGE), '\n')
cat("Variance: ", var(advert$AGE),  '\n')
cat("Standard Deviation: ", sd(advert$AGE),  '\n')

#The values are fairly symmetrical, very slightly skewed to the right and platykurtic


#Bivariate Analysis

#Correlation Plot

options(repr.plot.width = 18, repr.plot.height = 18)

plot_correlation(advert, type = 'c',cor_args = list( 'use' = 'complete.obs'))



#Using Gaceted Histograms, we investigate the distribution of Age along
#Gender Lines
ggplot(advert, aes(x= AGE)) +
  geom_histogram(bins = 30, color = "blue") + 
  facet_wrap(~GENDER) +
  ggtitle("Faceted Histogram of Age Distribution by Gender") +  theme(axis.text = element_text(size=18),
                                                                      axis.title = element_text(size = 18),
                                                                      plot.title = element_text(hjust = 0.5, size = 20))


#Distibution of Income along Click Lens
ggplot(advert, aes(x= INCOME)) +
  geom_histogram(bins = 30, color = "purple") + 
  facet_wrap(~CLICKS) +
  ggtitle("Faceted Histogram of Income across Clicks") + 
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 20))


#Scatterplot of Age VS Income

#print(b1 + geom_point())
b1 <- ggplot(advert, aes(x=INCOME, y=AGE))

b2 <- b1 + geom_point(aes(color=AGE), size=5) + scale_color_gradient(low='blue', high = 'red')
print(b2 + ggtitle("Scatterplot of Age Vs Income in 2016") +  theme(axis.text = element_text(size=18),
                                                                    axis.title = element_text(size = 18),
                                                                    plot.title = element_text(hjust = 0.5, size = 20)))


#Highest income levels registered by people under the age of 40 but greater than 20.



#Scatterplot of Age Vs Daily time on the Internet
b2 <- ggplot(advert, aes(x=BROWSE_TIME, y=AGE))

b3 <- b2 + geom_point(aes(color=AGE), size=5) + scale_color_gradient(low='green', high = 'red')
print(b3 + ggtitle("Scatterplot of Age Vs Browse Time in 2016") +  theme(axis.text = element_text(size=18),
                                                                         axis.title = element_text(size = 18),
                                                                         plot.title = element_text(hjust = 0.5, size = 20)))

#Individuals between ages 25 and 45 spend the most amount of time online.

# Creating a side-by-side barchart of Gender by Clicks
ggplot(advert, aes(x = GENDER, fill = CLICKS)) + 
  geom_bar(position = "dodge") +
  ggtitle("Side-Barchart of Clicks by Gender") +
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, size = 20))

#More males clicked sites than females.

# Creating a side-by-side barchart of Clicks by Hour
ggplot(advert, aes(x = CLICKS, fill = HOUR)) + 
  geom_bar(position= "dodge") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Barchart of Clicks by Hours") +  theme(axis.text = element_text(size=18),
                                                  axis.title = element_text(size = 18),
                                                  plot.title = element_text(hjust = 0.5, size = 20))

#There are more clicks in the Wee Hours of the Night than Morning, Night and afternoon. 
#The least number of clicks were registered at night.
#Still, the wee hours also registered the highest number of no clicks. 
#Night hours offered the least number of zero activities.


#Bar chart showing how the regions compared by the Hour
ggplot(advert, aes(x = REGION, fill = HOUR)) + 
  geom_bar(position= "dodge") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Barchart of Region by Hours") +  theme(axis.text = element_text(size=18),
                                                  axis.title = element_text(size = 18),
                                                  plot.title = element_text(hjust = 0.5, size = 20))

#In the African region, the afternoon and wee hours were the most active
#In the European region, morning hours were the most active whilst the Night was
#quieter
#Generally, the wee hours were the busiest in the regions. Only the Mid_East had
#Night time as the busiest
#Comparatively, it was less busy in the Mid_East at any point in time than in any
#other region

# Creating a side-by-side barchart of Region by gender
ggplot(advert, aes(x = REGION, fill = GENDER)) + 
  geom_bar(position= "dodge") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Barchart of Region according to Gender") +  theme(axis.text = element_text(size=18),
                                                             axis.title = element_text(size = 18),
                                                             plot.title = element_text(hjust = 0.5, size = 20))

#With the exception of America's region, women were the majority across all other regions.

#Europe was highly represented compared to other regions while the middle East 
#was the least represented.

#Plot proportion of Clicks, conditional on Region
ggplot(advert, aes(x = REGION, fill = CLICKS)) + 
  geom_bar(position = "fill") + coord_flip() +
  ylab("proportion") +
  ggtitle("Proportional Barchart of Clicks by Region") +  theme(axis.text = element_text(size=18),
                                                                axis.title = element_text(size = 18),
                                                                plot.title = element_text(hjust = 0.5, size = 20))

#Of all the regions, Asia had the most clicks.

#The Middle East, Europe, Africa and the America's had pretty much the same proportion of Clicks.

#The Regions aforementioned oscillated around 50% clicks and no clicks

ggplot(advert, aes(x = INCOME, y=AGE, color = REGION)) + 
  geom_point(size = 4, shape=1) +
  geom_smooth(aes(group=1), method= 'lm', formula = y~log(x), se=F, color ='red') +
  ggtitle("Trend of Age Vs Income by Region") +  theme(axis.text = element_text(size=18),
                                                       axis.title = element_text(size = 18),
                                                       plot.title = element_text(hjust = 0.5, size = 20))

#Although the trend line suggests a drop in age as the income increases, 
#we cannot tell with certainty whether there is any relationships with the fall by region. 
#However, we can see a huge concentration of Europe, Asia and America around age 35 and income levels of 60,000.

#Trend of Age Vs Browse Time by Region
ggplot(advert, aes(x = BROWSE_TIME, y=AGE, color = REGION)) + 
  geom_point(size = 4, shape=1) +
  geom_smooth(aes(group=1), method= 'lm', formula = y~log(x), se=F, color ='red') +
  ggtitle("Trend of Age Vs Browse Time by Region") +  theme(axis.text = element_text(size=18),
                                                            axis.title = element_text(size = 18),
                                                            plot.title = element_text(hjust = 0.5, size = 20))
#Generally, the Browse_time tends to increase as the age reduces. We still see great concentration in Africa, Asia and Europe.


#Trend of Browse TIme vs Net Usage by Region
ggplot(advert, aes(x = NET_USAGE, y=BROWSE_TIME, color = REGION)) + 
  geom_point(size = 4, shape=1) +
  geom_smooth(aes(group=1), method= 'lm', formula = y~log(x), se=F, color ='red') +
  ggtitle("Trend of Browse Time Vs Net Usage by Region") +  theme(axis.text = element_text(size=18),
                                                                  axis.title = element_text(size = 18),
                                                                  plot.title = element_text(hjust = 0.5, size = 20))

#Net usage increases as the Browse time increases.



#FOLLOW UP QUESTIONS

#Reflecting on whether we have achieved the objectives we set out

#1. Did we have the right data? Yes, we did

#2. Do we need other Data top answer our question? Yes, it would go along way in 
#explaining and validating certain observations in the current dataset e.g
#why they is a 50% chance of CLicking or not clicking an add & a fair representation
#of countries in the Mid_East


#3. Did we have the right Question? Yes, we did. 


#COnclusions & Recommendations

#In conclusion, women are the least likely to click on a link. 
#Perhaps focus should be placed on items or topics likely to get women interested in clicking a link.

#Men are most likely to click a link. We recommend that the be targeted the most. 
#A lot of traffic be directed to men.

#Clearly the afternoons are the worst possible times to advertise online. 
#It appears the wee hours of the night are the best times to advertise Crypto topics.

#Asia is clearly a key focus area as most of the clicks were registered there
#