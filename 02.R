#EXPLORATORY DATA ANALYSIS OF CREDIT SCORE DATA
#Question:Perform data cleaning and EDA on a dataset of choice. Explore the relationship between variables and identify patterns and trends.
#DATA DESCRIPTION
#The data is 'Credit Profile (Two-wheeler loan) Dataset'. This dataset provides a comprehensive overview of potential loan applicants' profiles in India, specifically tailored for the Indian demographic. It encapsulates a range of features, from basic demographics to financial details, that can be instrumental in assessing the creditworthiness of an individual.
#DATASET
library(readxl)
credit_data <- read_excel("D:/OneDrive/Documents/edu/prodigy int/2/credit_data.xlsx")
df<-data.frame(credit_data)
head(df,2)
dim(df)
#This shows our data has 279856 rows and  15 columns.

#DATA CLEANING
# Checking for missing values
missing_values <- colSums(is.na(df))
print(missing_values)
#We observe there are no missing values in our data.
# Removing duplicate rows
df <- unique(df)
# Converting data types   
str(df)
# since all the variables have the correct data type, we don't need to convert any.

#EXPLORATORY DATA ANALYSIS (EDA):
summary(df)
table(df$Gender)
table(df$Existing.Customer)
table(df$State)
table(df$City)
table(df$Employment.Profile)
table(df$Occupation)

#GRAPHICAL ANALYSIS
#HISTOGRAMS
h1<-hist(df$Income,main="INCOME OF LOAN APPLICANTS", xlab="Income (in rupee)", col="pink", ylim=c(0,20000))
text(h1$mids,h1$counts,labels=h1$counts,adj=c(0.5,-0.5))
#we observe the distribution of income among loan applicants is right-skewed. Majority of people have income less than 1L
h2<-hist(df$Loan.Amount,main="LOAN AMOUNT OF APPLICANTS", xlab="Amount (in rupee)", col="purple", ylim=c(0,70000))
text(h2$mids,h2$counts,labels=h2$counts,adj=c(0.5,-0.5))
#we observe majority of applicants apply for loan of about 1.5L
h3<-hist(df$Loan.Tenure,main="LOAN TENURE OF APPLICANTS", xlab="Months", col="maroon", ylim=c(0,25000))
text(h3$mids,h3$counts,labels=h3$counts,adj=c(0.5,-0.5))
#We observe the distribution of loan tenure is right- skewed. Majority of applicants want a tenure shorter than 100 momths (~= 8yrs)
h4<-hist(df$Profile.Score,main="PROFILE SCORE OF APPLICANTS", xlab="Score", col="violet", ylim=c(0,60000))
text(h4$mids,h4$counts,labels=h4$counts,adj=c(0.5,-0.5))
#We observe that the distribution of profile scores is left-skewed, with majority of applicants having a score of 100

# Bar chart 
b1<-barplot(table(df$Number.of.Existing.Loans),xlab="NUMBER OF EXISTING LOANS", ylab="NUMBER OF APPLICANTS",main="NUMBER OF EXISITNG LOANS OF APPLICANTS", col=rainbow(length(table(df$Number.of.Existing.Loans))),las=2,ylim=c(0,20000))
text(b1, table(df$Number.of.Existing.Loans), round(table(df$Number.of.Existing.Loans), 1),cex=1,pos=3) 
#least number of applicants had 10 existing loans.
b2<-barplot(table(df$City),xlab="CITY", ylab="NUMBER OF APPLICANTS",main="NUMBER OF APPLICANTS IN CITIES", col=rainbow(length(table(df$City))),las=2,ylim=c(0,20000))
text(b2, table(df$City), round(table(df$City), 1),cex=1,pos=3) 
#New Delhi, Hyderabad and Kolkata had the maximum number of loan applicants.
b3<-barplot(table(df$Occupation),xlab="OCCUPATION", ylab="NUMBER OF APPLICANTS",main="NUMBER OF APPLICANTS OF DIFFERENT OCCUPATIONS", col=rainbow(length(table(df$Occupation))),las=2,ylim=c(0,20000))
text(b3, table(df$Occupation), round(table(df$Occupation), 1),cex=1,pos=3) 
#Bankers, Civil servants,Software engineers and Teachers have the most number of loan applicants.

#Piechart
table1<-table(df$Gender) 
piepercent1<- round(100 * table1 / sum(table1), 2) 
pie1<-pie(table1,piepercent1, main="PROPORTION OF LOAN APPLICANTS BY DIFFERENT GENDERS", col=rainbow(length(table1)), clockwise=TRUE) 
legend("bottomright", c("Female","Male","Others"), cex = 0.75, fill = rainbow(length(table1)))
#Proportion of female and male applicants is almost equal (~=46%)
table2<-table(df$State) 
piepercent2<- round(100 * table2 / sum(table2), 2) 
pie2<-pie(table2,piepercent2, main="PROPORTION OF LOAN APPLICANTS BY DIFFERENT STATES", col=rainbow(11), clockwise=TRUE) 
legend("bottomright", c("Delhi","Gujarat","Karnataka","Kerala","Maharashtra","Rajasthan","Tamil Nadu","Telangana","UP","West Bengal"), cex = 0.75, fill = rainbow(11))
#Proportion of applicants is almost same for all states

#Boxplot
p1<-boxplot(df$Age~df$Employment.Profile, data=df, ylab="Age",xlab="Employment profile", col=c("lightcoral","blue","greenyellow","deeppink3","pink"))
#Median age for loan applicants is highest for salaried applicants (46 yrs). 
p2<-boxplot(df$Credit.Score~df$Employment.Profile, data=df, ylab="Credit Score",xlab="Employment profile", col=c("lightcoral","blue","greenyellow","deeppink3","pink"))
#Salaried people have a slightly higher median credit score compared to other employment profiles. median credit scores for freelancers, students and unemployed people is almost the same

#Making a df of only numerical variables to check for correlation
df1 <- subset(df, select = -c(2, 9,10,11,13,15))
# Correlation matrix
library(Hmisc)
rcorr(as.matrix(df1))
#Age and Income have a strong positive correlation (0.62)
#Credit.Score and Income also have a positive correlation (0.22).
#LTV.Ratio and Profile.Score have a moderate negative correlation (-0.54).
#Credit.Score and Number.of.Existing.Loans have a very strong positive correlation (0.99)
#Credit.History.Length and all other variables have zero correlation, suggesting no linear relationship.









