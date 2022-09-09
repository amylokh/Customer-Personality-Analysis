library(dplyr)
library(stats)
library(pastecs)
library(Hmisc)
library(psych)
marketing_campaign_data <- read.csv("C:\\Users\\amylo\\marketing_campaign.csv", sep="\t",
                                    stringsAsFactors = FALSE)

# Gives summary of the dataset
summary(marketing_campaign_data)
str(marketing_campaign_data)

#Gives kurtosis & skewness of the databset 
describe(marketing_campaign_data)

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

# Finding outliers in each scaled variable
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

# Drawing density graph & distribution
ggplot(clean_data, aes(x=Income)) + geom_density()
ggplot(clean_data, aes(x=Total_Spending)) + geom_density()
ggplot(clean_data, aes(x=Total_Children)) + geom_density()
ggplot(clean_data, aes(x=Age)) + geom_density()
ggplot(clean_data, aes(x=NumWebVisitsMonth)) + geom_density()

# Conducting Chi-Square test
# Problem 1
# Null Hypotheses - There is no association/relation between the Education of a customer 
# and whether the customer accepts the offer in first campaign or not
# Alternate Hypotheses - There is some association/relation between the Education of a customer 
# and whether the customer accepts the offer in first campaign or not
contingency_table <- table(clean_data$Education, clean_data$AcceptedCmp1)
chisq.test(contingency_table)

# As the p-value is greater than the level of significance - 5%, we do not reject the Null Hypotheses
# Hence, we can conclude that there is weak or no association between the education of a customer and 
# whether the customer accepts the offer in first campaign or not

# Problem 2
# Null Hypotheses - There is no association/relation between the marital status of a customer 
# and whether the customer accepts the offer in first campaign or not
# Alternate Hypotheses - There is some association/relation between the marital status of a customer 
# and whether the customer accepts the offer in first campaign or not
contingency_table2 <- table(clean_data$Marital_Status, clean_data$AcceptedCmp1)
chisq.test(contingency_table2)

# As the p-value is greater than the level of significance - 5%, we do not reject the Null Hypotheses
# Hence, we can conclude that there is weak or no association between the marital status of a customer and 
# whether the customer accepts the offer in first campaign or not

# Problem 3
# Null Hypotheses - There is no association/relation between whether the customer has a child or not 
# and whether the customer accepts the offer in first campaign or not
# Alternate Hypotheses - There is some association/relation between whether the customer has a child or not 
# and whether the customer accepts the offer in first campaign or not
contingency_table3 <- table(clean_data$Has_Child, clean_data$AcceptedCmp1)
chisq.test(contingency_table3)

# As the p-value is significantly lesser than the level of significance - 5%, we reject the Null Hypotheses
# Hence, we can conclude that there is some association between the customer has a child or not and 
# whether the customer accepts the offer in first campaign or not


# Exporting clean data for further statistical analysis
write.csv(clean_data, "C:\\Users\\amylo\\marketing_clean_data.csv")

marketing_campaign_data <- read.csv("E:\\SCMHRD-Docs\\Assignments\\Statistics\\marketing_clean_data.csv")

# Conducting 2 sample t test

#-------------------------------------------------------------------------------------------------------------------------
# Problem 1
# Null hypothesis: mean(Total spending of Grad Students) = mean(Total spending of Post Grad Students)
# Alternate hypothesis: mean(Total spending of Grad Students) ≠ mean(Total spending of Post Grad Students)

undergrad_student_spending <- subset( marketing_campaign_data, Education == "Undergraduate", select = c(Total_Spending))
postgrad_student_spending <- subset( marketing_campaign_data, Education == "Postgraduate", select = c(Total_Spending))

ratio <- var(undergrad_student_spending)/var(postgrad_student_spending)
# Ratio = 0.7149963. Hence we can assume equal variance for the 2 sample t test

t.test(undergrad_student_spending, postgrad_student_spending, var.equal = TRUE)

# As the p-value is significantly lesser than the level of significance - 5%, we reject the Null Hypotheses
# Hence, we can conclude that the mean(Total spending of Grad Students) ≠ mean(Total spending of Post Grad Students)

#-------------------------------------------------------------------------------------------------------------------------
# Problem 2
# Null hypothesis: mean(Total spending Has Child) = mean(Total spending No child)
# Alternate hypothesis: mean(Total spending Has Child) ≠ mean(Total spending No child)

parent_spending <- subset( marketing_campaign_data, Has_Child == "Yes", select = c(Total_Spending))
non_parent_spending <- subset( marketing_campaign_data, Has_Child == "No", select = c(Total_Spending))

ratio <- var(parent_spending)/var(non_parent_spending)
# Ratio = 0.7149963. Hence we can assume equal variance for the 2 sample t test

t.test(parent_spending, non_parent_spending, var.equal = TRUE)

# As the p-value is significantly lesser than the level of significance - 5%, we reject the Null Hypotheses
# Hence, we can conclude that the mean(Total spending Has Child) ≠ mean(Total spending No child)

#--------------------------------------------------------------------------------------------------------------------------
# Problem 3
# Null hypothesis: mean(Total Spending Single) = mean(Total Spending Married)
# Alternate hypothesis: mean(Total Spending Single) ≠ mean(Total Spending Married)

married_spending <- subset( marketing_campaign_data, Marital_Status == "Married", select = c(Total_Spending))
single_spending <- subset( marketing_campaign_data, Marital_Status == "Single", select = c(Total_Spending))

ratio <- var(married_spending)/var(single_spending)
# Ratio = 0.7149963. Hence we can assume equal variance for the 2 sample t test

t.test(married_spending, single_spending, var.equal = TRUE)

# As the p-value is significantly lesser than the level of significance - 5%, we reject the Null Hypotheses
# Hence, we can conclude that the mean(Total Spending Single) ≠ mean(Total Spending Married)

#--------------------------------------------------------------------------------------------------------------------------


#ANOVA TEST
#-------------------------------------------------------------------------------------------------------------------------
# Problem 1
# Null hypothesis:mean(Total Spending having 0 children) = mean(Total Spending having 1 children) = mean(Total Spending having 2 children) = mean(Total Spending having 3 children)
# Alternate hypothesis: At least one of them are unequal

one.way <- aov(Total_Spending ~ Total_Children, data = marketing_campaign_data)
summary(one.way)

#                 Df    Sum Sq   Mean Sq   F value Pr(>F)    
#Total_Children    1  196230133 196230133     731  <2e-16 ***
#Residuals      2200  590597423    268453
# Standard F value = 3.84568895 Calculated F value = 731
# Since Calculated F value > Standard F Value 
# We reject the null hypothesis.
#-------------------------------------------------------------------------------------------------------------------------
# Problem 2
# Null hypothesis:mean(Total Spending for Undergraduates) = mean(Total Spending for Postgraduates)
# Alternate hypothesis: At least one of them are unequal

one.way <- aov(Total_Spending ~ Education, data = marketing_campaign_data)
summary(one.way)

#              Df    Sum Sq  Mean Sq F value   Pr(>F)    
#Education      1  10734764 10734764   30.43 3.87e-08 ***
#Residuals    2200 776092792   352769 
# Standard F value = 3.84568895 Calculated F value = 30.43
# Since Calculated F value > Standard F Value 
# We reject the null hypothesis.
#-------------------------------------------------------------------------------------------------------------------------
# Problem 3
# Null hypothesis:mean(Total Spending for Parents) = mean(Total Spending for Non Parents)
# Alternate hypothesis: At least one of them are unequal

one.way <- aov(Total_Spending ~ Has_Child, data = marketing_campaign_data)
summary(one.way)

#              Df    Sum Sq   Mean Sq F value Pr(>F)    
#Has_Child      1 213458411 213458411     819 <2e-16 ***
#Residuals   2200 573369145    260622
# Standard F value = 3.84568895 Calculated F value = 819
# Since Calculated F value > Standard F Value 
# We reject the null hypothesis.
#-------------------------------------------------------------------------------------------------------------------------


# Correlation
View(marketing_clean_data_csv)

# Problem 1
# Null Hypothesis - Income is not correlated with the total spending of a customer.
# Alternate Hypothesis - Income is correlated with the total spending of a customer.
cor.test(marketing_clean_data_csv$Income,marketing_clean_data_csv$Total_Spending)

# Problem 2
# Null Hypothesis - Income is not correlated with the Number of visits per month of a customer
# Alternate Hypothesis - Income is correlated with the number of visits per month of a customer
cor.test(marketing_clean_data_csv$Income,marketing_clean_data_csv$NumWebVisitsMonth)

# Problem 3
# Null Hypothesis - The number of store purchases by a customer is not correlated with the total spending of a customer
# Alternative Hypothesis - The number of store purchases by a customer is correlated with the total spending of a customer
cor.test(marketing_clean_data_csv$NumStorePurchases,marketing_clean_data_csv$Total_Spending)


