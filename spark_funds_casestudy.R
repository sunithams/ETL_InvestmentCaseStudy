#####################################################################################
  #LOADING ALL THE PACKAGES REQUIRED FOR EXECUTION
#######################################################################################

require (tidyr)
require(stringr)
require(dplyr)
library(countrycode)

########################################################################################

# Importing all the external files required for execution

#Importing the companies.txt file and rounds.csv and the english speaking countries file

companies <-read.csv("companies.txt",sep="\t")
rounds2 <- read.csv("rounds2.csv")
eng_countries <- read.csv("english_countries.txt",sep ="\t",stringsAsFactors = FALSE)
#Import the mapping.csv
mapping <- read.csv("mapping.csv",stringsAsFactors = FALSE)



#perf_func <- function(){ 

# Had put this function to evaluate the performance of the script, after importing
#all the libraries and data sets.


# Our analysis begins here

##########################################################################################################

#Checkpoints - Part 1
# Table 1.1: Understand the Data Set 

####################################################################

# How many unique companies are present in rounds2?
# Answer=66368

unique_companies_rounds2 <- unique(tolower(rounds2$company_permalink))
length(unique_companies_rounds2)  #66368

#####################################################################

# How many unique companies are present in the companies file?
# Answer=66368

unique_companies_companies <- unique(tolower(companies$permalink))
length(unique_companies_companies) #66368

####################################################################
#In the companies data frame, which column can be used as the unique key for each company? Write the name of the column.
#Answer: Permalink

####################################################################

#Are there any companies in the rounds2 file which are not present in companies? Answer yes or no: Y/N
#Answer : No

setdiff(tolower(rounds2$company_permalink),tolower(companies$permalink)) #To show the difference between 2 columns
#character(0)

####################################################################

#Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame. Name the merged frame master_frame.
#How many observations are present in master_frame?

# Answer:  There are 114949 Observations in master frame


companies$permalink = as.character(tolower(companies$permalink))    #converting to lower case for case sensitivity
rounds2$company_permalink = as.character(tolower(rounds2$company_permalink))   #converting to lower case for case sensitivity
master_frame <- inner_join(rounds2,companies,by=c("company_permalink"="permalink"),string_As_factors=FALSE)  #Doing innerjoingjoin which is under the dplyr package



#####################################################################################################################################
##Checkpoint 2: Data Cleaning 2

###########################################################################################################################################

#Table 2.1: NA Values Treatment
####################################################################################

#How many NA values are present in the column raised_amount_usd ?
#Answer: 19990

sum_NA_raised_usd <- sum(is.na(master_frame$raised_amount_usd))
sum_NA_raised_usd
#19990


####################################################################################
# Replace all the NA values from the raised_amount_usd column of the master frame


master_frame[which(is.na(master_frame$raised_amount_usd)),"raised_amount_usd"] <- -1

#####################################################################################
######################################################################################################################################

#Checkpoint 3: Funding Type Analysis

####################################################################################################################################################################################################################

#Calculate the average investment amount for each of the four funding types 
#(venture, angel, seed, and private equity) and report the answers in Table 3.1

##########################################################################################
#Step: To get the average funding for each one of the investment types
#Using the groupby function under the dplyr package and summarizing by the mean by each funding type


investment_summary <- filter(master_frame,funding_round_type %in% c("angel","seed","private_equity","venture"),raised_amount_usd != -1) %>% group_by(funding_round_type) %>% summarise(average_funding_amount=mean(raised_amount_usd))
investment_summary

#funding_round_type        average_funding_amount

#1              angel               958694.5
#2     private_equity             73308593.0
#3               seed               719818.0
#4            venture             11748949.1

##############################################################################################

#Considering that Spark Funds wants to invest between 5 to 15 million USD per  investment round, which investment type is the most suitable for them?
# Answer: Venture Type


#################################################################################################################################################################################################################################################

#Checkpoints - Part 2

########################################################################################################################################################################################################################################

#This is the second goal of analysis - country analysis.

######################################################################################


#Spark Funds wants to see the top nine countries which have received the highest total funding 
#(across ALL sectors for the chosen investment type)
#########################################################################################

master_frame1 <- master_frame #Duplicating the master frame

#Mapping codes to names
master_frame1$country_names <- countrycode(master_frame1$country_code, "iso3c", "country.name")

#################################################################################################
#Getting the countries with engish speaking ones

country_vect <- c(eng_countries$Country)  #creating a vector of all english speaking countries,dataframe created from imported txt file
#This is useful for plotting the top 9 countries in Tableau

master_frame1$"english_speaking_countries" <- grepl(paste(country_vect, collapse = "*|"), master_frame1$country_names)

################################################################################################

#Getting the top 9 countries by sum of investments

top9 <- group_by(master_frame1,country_code)  %>% filter(funding_round_type %in% "venture",country_code != "",raised_amount_usd != -1) %>%  summarise(total_investment = sum(raised_amount_usd)) %>% arrange(desc(total_investment)) %>% slice(1:9)

#appending country names

top9$country_names <- countrycode(top9$country_code,"iso3c", "country.name")

#Determining english speaking or not for Top9 countries

top9$"english_speaking_countries" <- grepl(paste(country_vect, collapse = "*|"), top9$country_names)

####################################################################################

#Identify the top three English-speaking countries in the data frame top9.

top3 <- filter(top9,english_speaking_countries ==TRUE) %>% slice(1:3)


#############################################################################################################################################################################################

#Checkpoint 5: Sector Analysis 1

###############################################################################################################################################################################################
#Extract the primary sector of each category list from the category_list column



master_frame1 <- separate(master_frame1,category_list,into=c("primary_sector"),sep ="\\|") #Using the seperate function from tidyr package


########################################################################################
#Use the mapping file 'mapping.csv' to map each primary sector to one of the eight main sectors 
#(Note that 'Others' is also considered one of the main sectors)
########################################################################################

#converting from wide to long format

mapping_long <- gather(mapping,main_Sector,val,2:10)
mapping_long <- mapping_long[!(mapping_long$val==0),]
mapping_long[,"val"]= NULL
mapping_long$category_list <- str_replace_all(mapping_long$category_list,pattern="0","na")  #Replacing the '0' in the string to 'na'

########################################################################################

# Merging the mapping_long dataframe which has the main sectors to the main frame
 
mapping_long$category_list=tolower(mapping_long$category_list)   #handling case sensitivity
master_frame1$primary_sector=tolower(as.character(master_frame1$primary_sector))

merge_frame2 <- full_join(master_frame1,mapping_long,by=c("primary_sector"="category_list"))


#############################################################################################
#Checkpoint 6: Sector Analysis 2
############################################################################################

#Filtering for the country code USA,venture type="venture",funding betweeb 5M to 15M

############################################################################################

# The total number (or count) of investments for each main sector in a separate column
# The total amount invested in each main sector in a separate column
# Computing and adding columns Number of investments and Total investment by main sector

#############Creating a function to give 3 dataframes#####################################


top3   # Contains the Country code and name for top 3 english speaking countries

sector_analysis <- function(x){
  
     countrywise_sector_df <-  filter(merge_frame2,country_code %in% x,funding_round_type %in% "venture",between(raised_amount_usd,5000000,15000000),main_Sector!="Blanks",!is.na(main_Sector)) %>% 
                group_by(main_Sector) %>% mutate(no_of_investments=n(),total_investment=sum(raised_amount_usd))
    return (countrywise_sector_df)
  
  
}

#Getting the data frame for First country in top 3

top3$country_code <- as.character(top3$country_code)
D1 <- sector_analysis((top3[1,1])) #Countrycode =USA
D2 <- sector_analysis((top3[2,1]))  #countrycode =UK
D3 <- sector_analysis((top3[3,1]))  #countrycode =INDIA

##############################################################################################

#Table 6.1 : Sector-wise Investment Analysis

##############################################################################################  


#writing a function to compute the answers for all the 10 questions in Table 6.1

investment_analysis <- function(x) {
  options(scipen = 999) #Disabling the Scientific notation In R to enable printing the number in non exponent form
  
  sector_numbers <- group_by(x,main_Sector) %>% summarise(no_of_investments=n(),total_investment=sum(total_investment)) #Rolling up by main sector and aggregating by sum of inv and count of Inv
  
  total_num_inv <- sum(sector_numbers$no_of_investments) #Total number of investments across main sectors
  total_sum_inv <- sum(sector_numbers$total_investment)  #Total sum of investment across all main sectors
  
  
  D4 <- group_by(x,main_Sector) %>% summarise(n=n()) %>% arrange(desc(n)) %>% slice(1:3) #grouping  mainsector and getting the top3 investments by number 
  mainsectors <- unlist(D4[1])
  no_of_inv <- unlist(D4[2])
  
  company1 <- filter(x,main_Sector %in% mainsectors[1] ) %>% group_by(name)   %>% summarise(n=sum(raised_amount_usd)) %>% arrange(desc(n)) %>% slice(1:1) 
  top_company1 <- unlist(company1[1])   #getting the company wise data for the Top sector and amount of funding raised
  
  company2  <-  filter(x,main_Sector %in% mainsectors[2] ) %>%  group_by(name)  %>%  summarise(n=sum(raised_amount_usd))  %>% arrange(desc(n)) %>% slice(1:1) 
  top_company2 <- unlist(company2[1])    # #getting the company wise data for the second  Top sector and amount of funding raised
  
  table_6_1_dataframe <- data.frame(total_num_inv,total_sum_inv,mainsectors[1],mainsectors[2],mainsectors[3],no_of_inv[1],no_of_inv[2],no_of_inv[3],top_company1[1],top_company2[1])

  return(table_6_1_dataframe)   #returning a data frame which has columns mapped to 10 questions being asked in the table 6.1
  
}


D1a <- investment_analysis(D1) # Passing the D1 dataframe as object to the function
D2a <- investment_analysis(D2) # Passing the D2 dataframe as object to the function
D3a <- investment_analysis(D3)  # Passing the D3 dataframe as object to the function
investment_summary_dataframe <- bind_rows(D1a,D2a,D3a)  #Binding all the data frame row wise to get one composite dataframe

#The columns have the 10 questions , rows have the 3 countries

#Defining the row names
row.names(investment_summary_dataframe)[1] <- "Country1(USA)"
row.names(investment_summary_dataframe)[2] <- "Country2(Great Britain and Ireland)"
row.names(investment_summary_dataframe)[3] <- "Country3(India)"


#Performing the transpose on investment_summary_dataframe to have a layout as Table 6.1 : Sector-wise Investment Analysis

investment_summary_dataframe <- t(investment_summary_dataframe)
View(investment_summary_dataframe) 


# Our analysis end here 



######################################################################################################################################################

#Script performance

#} Closing the function (for performance testing)

#system.time(perf_func())   Takes 2.37s

#user  system elapsed 
#2.37    0.05    2.55 

############################################################################################################