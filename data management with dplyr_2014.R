#####
#####
#####ESM 567 Worksheet #1.  Data Management using dplyr
#####

install.packages(c('dplyer','ggplot2','reshape','dplr')) #install new packages to your computer
library(dplyr)

##import the data and convert them into a tbl_df data frame for dplyr
dta <- tbl_df(read.csv("wemap.csv")) #import from working directory after creating project.
dta

dta <- tbl_df(read.csv("C:\\Users\\bwyp\\Desktop\\wemap.csv", stringsAsFactors = FALSE)) #make sure to change the file direcotry to where your data are stored
dta  #the data frame with tbl_df format only shows the top ten rows of a large dataset

## use 'filter' to extract a subset of the data (by rows)
dta_2003 <- filter(dta, YEAR == "2003") #select all data collected in 2003
dta_2003
PL_2003 <-filter(dta, YEAR == "2003" & ECO3=="PL")  #select all data collected in 2003 in the Plain ecoregion
PL_2003

## use 'select' to extract a subset of the data (by columns)
nutrient<-select(dta,NH4, NO3, NTL)  #select 3 nitrogen variables

## use 'mutate' to create a new variable (column)
DIN<-mutate(nutrient, DIN=NH4+NO3)   #sum NH4 and NO3 to a new variable called DIN

## use 'summarise' to summary the data
N_summary<-summarise(DIN,DIN_mean=mean(DIN, na.rm=T))  # calculate mean of DIN

##use 'group_by' to group the data
by_ECO3 <- group_by(dta,ECO3) #group the data by 'ECO3' 
by_ECO3
ECO3_summary <- summarise(by_ECO3, 
                         pH_mean = mean(PH, na.rm = TRUE),
                         ANC_mean = mean(ANC, na.rm = TRUE)
)  
ECO3_summary

by_ECO3_YEAR <- group_by(dta,ECO3,YEAR) #group the data by 'ECO3'
ECO3_YEAR_summary <- summarise(by_ECO3_YEAR,                 #summarize PH and ANC by ECO3
                          pH_mean = mean(PH, na.rm = TRUE),
                          ANC_mean = mean(ANC, na.rm = TRUE)
)
ECO3_YEAR_summary

##let's try to use pipe opertator (%>%) to form a chain of R functions 

dd.1 <-dta%>%          #select a dataset called 'dta'
  group_by(ECO3) %>%    #group the data by ECO3
  filter(AG_TOT>60)%>%       #select all rows with % AG>60%
  filter(!is.na(AG_TOT))%>%      #remove all missing values
  summarise(Ag_Max=max(AG_TOT),   #summarize maximum, minimum, and the number of sites with AG>60%
            Ag_Min=min(AG_TOT),
            Number_Site=n()) 
dd.1

## Your turn to learn more about the data 


