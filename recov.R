# Installing Libraries                                                          ----
# library(FinancialMath)
library(plyr)
library(dplyr)
library(tidyr)
library(corrplot)
library(ggplot2)
library(lubridate)
library(splitstackshape) # for stratified sampling
library(dummies)
library(corrplot)

emi <- function( p , r , n )
{
round(p*(r/12)*(1+r/12)^n/((1+r/12)^n - 1))
}

# above emi calculated with
# p = Loan_Amount - Advance_EMI_Amount , r = Finance_IRR / 100 , n = Net_Tenure

roi <- function( p , emi , n)
{
print((emi*n - p)/( p * n / 12))
}

# Loading Data and Identifing important variables                               ----

w   <- read.csv("C:\\Users\\kawaljeet\\Desktop\\D\\Credit_Scoring_wahile\\Quantafic_20181225\\static_data.csv")

company_columns    <- c("DealId" , "Asset_Cost" , "On_Road_Price" , "Loan_Amount" , "Finance_IRR" , "EMI" ,
                        "Advance_EMI_Amount" , "Advance_EMI_Period"  , "Gross_Tenure" , "Net_Tenure" ,
                        "Total_Processing_Fees" , "Dealer_Commission" , "Promo_Scheme_Id" , "Salesman_Id" ,
                        "Salesman_Branch" , "Credit_Scheme" , "Login_Location_Latitude" , "Login_Location_Longitude" ,
                        "Login_Location")

customer_columns   <- c("DealId" , "Repayment_Mode" , "Repayment_Bank" , "Repayment_Bank_Branch" , "Applicant_Profile" ,
                        "Applicant_DOB" , "Applicant_Gender" , "Applicant_Mobile" , "Applicant_Office_Phone" ,
                        "Applicant_Resi_Area" , "Applicant_Resi_Tahasil" , "Applicant_Resi_City" ,
                        "Applicant_Resi_State" , "Applicant_Resi_Pin" , "Applicant_Office_Area" ,
                        "Applicant_Office_Tahasil" , "Applicant_Office_City" , "Applicant_Office_State" ,
                        "Applicant_Office_Pin" , "Gave_Permanent_Address" , "Has_Own_House" , "Gave_Own_House_Proof" ,
                        "CIBIL_Score")

cocustomer_columns <- c("DealId" , "CoApplicant_Constitution" , "CoApplicant_DOB" , "CoApplicant_Gender" ,
                        "CoApplicant_Location_Area" , "CoApplicant_Location_Tahasil" , "CoApplicant_Location_City" ,
                        "CoApplicant_Location_State" ,"CoApplicant_Location_Pin")

Vehicle_columns    <- c("DealId" , "Manufacture", "Model" , "Dealer", "Branch" , "Make" , "Asset_Cost")

w_comp <- w %>%
            select(company_columns) %>%
            as.data.frame()

# the calculated emi matches almost exactly with the emi on the data, the only data
# that does not match is :

cbind(c = emi( (w_comp$Loan_Amount - w_comp$Advance_EMI_Amount) ,
                w_comp$Finance_IRR/100 ,
                w_comp$Net_Tenure) ,
                w_comp %>%
                select(EMI)) %>%
                mutate(d = Mod(c-EMI)) %>%
                cbind(w_comp %>%
                select(-EMI)) %>%
                filter(d!=0) %>%
                View

w_comp$DealId          <- as.factor(w_comp$DealId)
w_comp$Salesman_Id     <- as.factor(w_comp$Salesman_Id)
w_comp$Promo_Scheme_Id <- as.factor(w_comp$Promo_Scheme_Id)

# Dealer Commission plot
w_comp %>%
    ggplot(aes(x = Dealer_Commission)) +
    geom_density()

# Viewing different Salesman from different braches and the number of deals     ----
w_comp %>%
  select(Salesman_Branch , Salesman_Id) %>%
  group_by(Salesman_Branch) %>%
  unique() %>%
  arrange(Salesman_Branch) %>%
  summarise(n()) %>%
  View

w_comp %>%
  group_by(Salesman_Branch , Salesman_Id) %>%
  arrange(Salesman_Branch) %>%
  summarise(n()) %>%
  View

# Loading DPD data                                                              ----
q        <- read.csv("C:\\Users\\kawaljeet\\Desktop\\D\\Credit_Scoring_wahile\\Quantafic_20181225\\dpd_data.csv")

q$DealId <- as.factor(q$DealId)

q %>%
  group_by(DealId) %>%
  summarise(n() , mean(dpd) , median(dpd) , max(dpd)) %>%
  arrange(DealId) %>%
  as.data.frame() %>%
  cbind(w_comp %>%
  select(DealId , Gross_Tenure , Net_Tenure) %>%
  arrange(DealId) %>%
  select(-DealId) %>%
  as.data.frame()) %>%
  View

# Number of Entries in DPD does not match with the net tenure in static data    ----
q %>%
  group_by(DealId) %>%
  summarise(NoofMonths = n() ,
  last_ins_Date = max(as.Date(MONTH))) %>%
  as.data.frame() %>%
  join(w_comp %>%
  select(Gross_Tenure , Net_Tenure , DealId) ,
  by = "DealId") %>%
  View()

# Some problem in the dpd data where the revovery stops abrupt
# ie dates and the month dont match with the static data
# See the data below : Below data only contain the customers having
# dpd months less than the tenure

q %>%
  group_by(DealId) %>%
  summarise(NoofMonths = n() ,
            last_ins_date  = max(as.Date(MONTH)) ,
            first_ins_date = min(as.Date(MONTH)) )  %>%
            as.data.frame() %>%
  join(w_comp %>%
  select(Gross_Tenure , Net_Tenure , DealId) ,
  by = "DealId") %>%
  mutate(x = NoofMonths - Net_Tenure) %>%
  join(q %>%
          group_by(DealId) %>%
          summarise(sum(dpd)) ,
       by = "DealId") %>%
  arrange(last_ins_date) %>%
  filter(x < 0 ) %>%
  select(-x) %>%
  View

# The transaction Data                                                          ----

e   <- read.csv("C:\\Users\\kawaljeet\\Desktop\\D\\Credit_Scoring_wahile\\Quantafic_20181225\\transaction__repayment_schedule.csv")

e %>%
  group_by(DealId) %>%
  summarise(emi_month = max(EMI_NO) ,
            amt_paid = sum(EMI_Amt) ,
            pric_comp = sum(Principal_Component) ,
            last_ins_date = max(as.Date(Billing_Date))) %>%
  as.data.frame() %>%
  join(w %>%
        select(DealId , Net_Tenure ,  Loan_Amount) %>%
        as.data.frame() ,
        by = "DealId") %>%
View

# this data is planned by our company, not important for our model

# find the number of projects that are completed                                ----
q %>%
  mutate(MONTH = as.Date(MONTH)) %>%
  group_by(DealId) %>%
  summarise(max_month = max(MONTH) ,
            sum_dpd = sum(dpd) ,
            no_of_months = n()) %>%
  as.data.frame() %>%
  mutate(Month = month(max_month)) %>%
  mutate(Year = year(max_month)) %>%
  filter(!(Month == 11 & Year == 2018)) %>%
  arrange(desc(max_month)) %>%
  join(w_comp %>%
          select(Net_Tenure ,
                Loan_Amount ,
                DealId) ,
  by = "DealId") %>%
  View

# Also observe the above candidates on dpd data to categorise them into good or bad
# some candid leave abruptly, do look them up
# there is a file named completed_projects(made by me), study it carefully

t0 <- read.csv("C:\\Users\\kawaljeet\\Desktop\\D\\Credit_Scoring_wahile\\Quantafic_20181225\\transaction__bounce_data_fy1516.csv")
t1 <- read.csv("C:\\Users\\kawaljeet\\Desktop\\D\\Credit_Scoring_wahile\\Quantafic_20181225\\transaction__bounce_data_fy1617.csv")
t2 <- read.csv("C:\\Users\\kawaljeet\\Desktop\\D\\Credit_Scoring_wahile\\Quantafic_20181225\\transaction__bounce_data_fy1718.csv")
t3 <- read.csv("C:\\Users\\kawaljeet\\Desktop\\D\\Credit_Scoring_wahile\\Quantafic_20181225\\transaction__bounce_data_fy1819.csv")

rbind(t0 , t1 , t2 , t3) %>%
  mutate(Presentation_Date = as.Date(Presentation_Date)) %>%
  group_by(DealId) %>%
  summarise(min_date = min(Presentation_Date) ,
            max_date = max(Presentation_Date) ) %>%
            as.data.frame() %>%
  join(rbind(t0 , t1 , t2 , t3 ) %>%
            filter(Clearing_Status == "Cleared") %>%
            group_by(DealId) %>%
            summarise(payment = sum(Presenation_Amount)) ,
            by = "DealId") %>%
  View

completed_projects <- w %>% 
                        mutate(Authorization_Date = as.Date(Authorization_Date)) %>% 
                        filter(Authorization_Date + 30*Net_Tenure < dmy("31-12-2018")) %>% 
                        select(DealId)
                        
good_candidates   <-   rbind(t0 , t1 , t2 , t3) %>% 
                            filter(DealId %in% completed_projects$DealId) %>% 
                            spread(Clearing_Status , Presenation_Amount , fill = 0) %>% 
                            group_by(DealId) %>% 
                            summarise(Sum_cleared = sum(Cleared)) %>% 
                            join(w %>% 
                                   select(DealId , EMI , Net_Tenure) ,
                                 by = "DealId") %>% 
                            filter(Sum_cleared == EMI*Net_Tenure) %>% 
                            select(DealId)
 
                           




# ----
# The best candidates with 0 sum_dpd and no_of_months = net_tenure, there are only 15 such candidates

p <- q %>%
      mutate(MONTH = as.Date(MONTH)) %>%
      group_by(DealId) %>%
      summarise(max_month = max(MONTH) ,
                sum_dpd = sum(dpd) ,
                no_of_months = n()) %>%
      as.data.frame() %>%
      mutate(Month = month(max_month)) %>%
      mutate(Year = year(max_month)) %>%
      filter(!(Month == 11 & Year == 2018)) %>%
      arrange(desc(max_month)) %>%
      join(w_comp %>%
              select(Net_Tenure ,
                      Loan_Amount ,
                      DealId) ,
           by = "DealId") %>%
      filter(no_of_months == Net_Tenure) %>%
      filter(sum_dpd == 0) %>%
      select(DealId) %>%
      as.vector()

w %>%
  filter(DealId %in%
          p$DealId) %>%
  View
# See Credit_Scheme




# Model 1 : Logistic Regression                                                 ----

set.seed(3.14)

w1 <- w %>% 
        select(DealId , Asset_Cost , Loan_Amount , Finance_IRR , EMI , Advance_EMI_Amount , 
               Advance_EMI_Period , Net_Tenure , Total_Processing_Fees , Dealer_Commission_Perc , 
               Manufacture , Dealer , Branch , Credit_Scheme , CIBIL_Score , Repayment_Mode , 
               Applicant_Profile , Applicant_DOB , Applicant_Gender , Applicant_Resi_City , 
               Applicant_Resi_State , Gave_Permanent_Address , Has_Own_House , Gave_Own_House_Proof) %>% 
        filter(DealId %in% completed_projects$DealId)

w1 <- w1 %>% 
        select(-c(Dealer , Applicant_Resi_City))

w1 <- w1 %>% 
         mutate(Applicant_Profile = as.factor(ifelse(as.character(Applicant_Profile) == "" , 
                                              "None" , 
                                              as.character(Applicant_Profile)))) %>% 
         mutate(Applicant_Gender = as.factor(ifelse(as.character(Applicant_Gender) == "" , 
                                             "Male" , 
                                             as.character(Applicant_Gender)))) %>% 
         mutate(Age = sapply(strsplit(as.character(Applicant_DOB) , 
                                      split = "-") ,
                             function(x) 
                               {2019 - as.numeric(x[1])} )) %>% 
        select(-Applicant_DOB) %>% 
        mutate(Age = ifelse(is.na(Age)  , median(Age , na.omit(Age)) , Age))

w1 <- w1 %>% 
        mutate(cand_score = as.logical(ifelse(DealId %in% good_candidates$DealId , T , F )))

w1_train <- w1 %>% 
              select(-DealId) %>% 
              dummy.data.frame() %>% 
              stratified(group = "cand_score" , size = .7 , bothSets = T )

w1_test  <- w1_train$SAMP2 %>% 
                as.data.frame()

w1_train <- w1_train$SAMP1  %>%   
                as.data.frame()

G <- w1_train %>% 
      glm(formula = cand_score ~ . , 
          family = "binomial")
              
# rep <- rep("False" , nrow(w1_train))

rep <- G %>% 
          predict(type = "response")> .5

t = table(rep , w1_train$cand_score)
t

accuracy = function(x)
{
  print("accuracy")  
  print((x[1,1] + x[2,2]) / (x[1,1] + x[1,2] + x[2,1] + x[2,2]))
  print("sensitivity")
  print( x[2,2] / ( x[2,1] + x[2,2]))
  print("specificity")
  print(x[1,1] / ( x[1,1] + x[1,2]))
  
}

accuracy(t)

# View the number of levels in each variable        

# rbind(w %>% 
#         colnames() , 
#       lapply(1:ncol(w) , 
#              function(x) length(levels(w[ , x])))) %>%
#   as.matrix() %>% 
#   t() %>% 
#   matrix(nrow = ncol(w) , ncol = 2 , dimnames = list(NULL , c("Name" , "Levels"))) %>% 
#   as.data.frame() %>% 
#   View



