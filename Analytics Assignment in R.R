library(dplyr)
library(stats)
marketing_campaign_data <- read.csv("C:\\Users\\amylo\\marketing_campaign.csv", sep="\t",
                                    stringsAsFactors = FALSE)

# Gives summary of the dataset
summary(marketing_campaign_data)
str(marketing_campaign_data)

# Check for Null/Missing Values
sapply(marketing_campaign_data, function(x) sum(is.na(x)))

# Dropping Null/Missing values from the dataset
marketing_campaign_data <- na.omit(marketing_campaign_data)

# Bringing down the categories under the Marital Status
marketing_campaign_data$Marital_Status[marketing_campaign_data$Marital_Status == 'Together'] <- 'Married'
marketing_campaign_data$Marital_Status[marketing_campaign_data$Marital_Status == 'Divorced'] <- 'Single'
marketing_campaign_data$Marital_Status[marketing_campaign_data$Marital_Status == 'Absurd'] <- 'Single'
marketing_campaign_data$Marital_Status[marketing_campaign_data$Marital_Status == 'Widow'] <- 'Single'
marketing_campaign_data$Marital_Status[marketing_campaign_data$Marital_Status == 'YOLO'] <- 'Single'
marketing_campaign_data$Marital_Status[marketing_campaign_data$Marital_Status == 'Alone'] <- 'Single'


# Bringing down the categories under the Education
marketing_campaign_data$Education[marketing_campaign_data$Education == 'Basic'] <- 'Undergraduate'
marketing_campaign_data$Education[marketing_campaign_data$Education == '2n Cycle'] <- 'Undergraduate'
marketing_campaign_data$Education[marketing_campaign_data$Education == 'Graduation'] <- 'Postgraduate'
marketing_campaign_data$Education[marketing_campaign_data$Education == 'Master'] <- 'Postgraduate'
marketing_campaign_data$Education[marketing_campaign_data$Education == 'PhD'] <- 'Postgraduate'

table(marketing_campaign_data$Education)
table(marketing_campaign_data$Marital_Status)

# Generating new scaled variable called Age
marketing_campaign_data$Age = 2014 - marketing_campaign_data$Year_Birth

# Generating new scaled variable called Total_Spending
marketing_campaign_data$Total_Spending = marketing_campaign_data$MntWines + marketing_campaign_data$MntFruits + 
  marketing_campaign_data$MntMeatProducts + marketing_campaign_data$MntFishProducts + marketing_campaign_data$MntGoldProds + 
  marketing_campaign_data$MntSweetProducts

# Generating new scaled variable called Total_Children
marketing_campaign_data$Total_Children = marketing_campaign_data$Teenhome + marketing_campaign_data$Kidhome

# Generating new categorical variable called Has_Child which means whether the customer has a child or not
marketing_campaign_data$Has_Child = ifelse(marketing_campaign_data$Total_Children > 0,"Yes", "No")

# Finding number of outliers in each scaled variable
Outlier_Income <- boxplot(marketing_campaign_data$Income, plot=FALSE)$out
Outlier_Recency <- boxplot(marketing_campaign_data$Recency, plot=FALSE)$out
Outlier_MntWines <- boxplot(marketing_campaign_data$MntWines, plot=FALSE)$out
Outlier_MntFruits <- boxplot(marketing_campaign_data$MntFruits, plot=FALSE)$out
Outlier_MntMeanProducts <- boxplot(marketing_campaign_data$MntMeatProducts, plot=FALSE)$out
Outlier_MntFinshProducts <- boxplot(marketing_campaign_data$MntFishProducts, plot=FALSE)$out
Outlier_MntSweetProducts <- boxplot(marketing_campaign_data$MntSweetProducts, plot=FALSE)$out
Outlier_NumDealsPurchases <- boxplot(marketing_campaign_data$NumDealsPurchases, plot=FALSE)$out
Outlier_NumWebPurchases <- boxplot(marketing_campaign_data$NumWebPurchases, plot=FALSE)$out
Outlier_NumCatalogPurchases <- boxplot(marketing_campaign_data$NumCatalogPurchases, plot=FALSE)$out
Outlier_NumWebVisitsMonth <- boxplot(marketing_campaign_data$NumWebVisitsMonth, plot=FALSE)$out
Outlier_Age <- boxplot(marketing_campaign_data$Age, plot=FALSE)$out
Outlier_TotalSpending <- boxplot(marketing_campaign_data$Total_Spending, plot=FALSE)$out

# Dropping significant outliers from the dataset
clean_data <- subset(marketing_campaign_data, !(Income %in% Outlier_Income))
clean_data <- subset(clean_data, !(Age %in% Outlier_Age))
clean_data <- subset(clean_data, !(Total_Spending %in% Outlier_TotalSpending))

# Exporting clean data for further statistical analysis
write.csv(clean_data, "C:\\Users\\amylo\\marketing_clean_data.csv")
