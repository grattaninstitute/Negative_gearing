# Superannuation earnings charts
# The cost of changes to superannuation earnings taxation is done in a seperate spreadsheet

# Here I am using SIH2011-12 to examine incomes for those over 60 and their super earnings + tax effect
# Will also have a look at this by wealth


# ======================================================================================================== #
# Super model for Budget Repair report ============================================================================ #
# ======================================================================================================== #

# This script is for analyzing current policy settings and reform options for the tax treatment of super earnings. We gather super balances, incomes etc. from the SIH 11-12 person-level dataset, inflate everything forward to 2015-16, and use these to project super earnings on those balances. We then apply current policy setting for super earnings, generate benchmark tax treatments where: (a) all taxpayers pay 15% tax on super earnings regardless of age; and (b) super earnings are incorporated into taxable income and taxed at personal marginal tax rates (this is how Treasury's tax expenditures are done). We then consider the various reform options, generate costings and charts that are usd ion the super chapter of the Budget Repair report

# However, as always more can be done to refine this analysis. The below to do list reports various refinements and whether they have been done.

# NEXT STEPS - 

# (1) We should inflate all our variables forward to 2015-16, including super balances- DONE for inflation, but we could grow income forward by nominal wages (eg. use the ABS wage price index for this)
# (2) Check our estimate for super earnings - standard benchmark used in Treasury and the OECD appears to be 7 per cent after fees (but before tax)
# (3) Adjust tax rates for super earnings to account for (a) 10% CGT for super; and (b) dividend imputation. Estimates from Mercer suggest an effective tax rate of around 8-9 per cent is reasonable
# (4) Generate a more nuanced taxable income variable (i.e. better than SIH total income) to use when estimate value of super earnings tax concession against personal income tax benchmark
# (5) Should we be substracting any super income streams from our total income (including super earnings)? If we're including super earnings in income, then we shouldnt be including drawdowns on super as income at the same time to avoid double counting
# (6) Better define those in the drawdown phase as those aged over 60 drawing an income stream from a super fund - DONE 
# (7) Introducing a different earnings rate in the accumulation and drawdown phases - DONE
# (8) Introducing a different effective tax rate on earnings between the accumulation and drawdown phases - SETUP BUT DIFFERENT EARNINGS RATE NOT ESTIMATED SO BOTH STILL THE SAME FOR NOW
# (9) Write in Medicare Levy function, differentiated by Age (DONE) and family status (NOT DONE)
# (10) Write in SAPTO for singles and couples (DONE) and allow SAPTO entitlements to be transferred among couples (NOT DONE)
# (11) Write in LITO function (DONE)
# (12) Account for behavioural change as retirees shift funds out of super to minimise tax by making the most of their TF threshold, SAPTO and LITO entitlements (DONE)
# (12) Better map ABS total incomes to ATO taxable incomes. One approach would be to estimate our estimated tax on individuals, and compare this to the ABS' imputed estimated. If all else fails we might have to use HILDA instead if the data there is more reliable / detailed, especially on the tax estimates.

# ======================================================================================================== #
# Loading required packages ============================================================================ #
# ======================================================================================================== #

# The survey of income and housing variable list can be found at the ABS website
# http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/6541.0.30.0012011-12?OpenDocument
# Basic cross tabs are also here and can be used for QC

rm(list=ls())

# Load packages
library(foreign) # for reading .dta files
library(dplyr) # for data manipulation
library(survey) # For survey weighted cross tabs
library(Hmisc) # for variable manipulation
library(car)
library(gam)
library(lattice)
library(ggplot2)

# ======================================================================================================== 
# Pulling in the data
# ======================================================================================================== #

# I know we should we using relative file paths but this will work for now

person.df <- read.dta("../SIH/sih11bp.dta")

# List of variables we need for the analysis
key.vars <- c("HHPOS", # Position in the household (publication definition)
              "AGEBC", # Age of person
              "SEXP", # Sex
              "LFSCP", # LFS
              "FAMTYPE",
              "ABSHID",
              "VSUPGCP", 
              "VSUPNCP",
              "SIHPSWT",
              "ISSSCP",
              "IWSUCP",
              "IAGECP",
              "INCTSCP8",
              "ITAXCP8",
              "ITGCBCP8",
              "ISUPERCP", 
              "INSSCP", # Current weekly benefit from employer provided superannuation (non salary sacrifice)
              "ISSSCP", # Current weekly employee income salary sacrificed for superannuation
              "I40SUP") # Personal irregular receipts from superannuation payments over last 2 years

# Renaming the variables for ease of use
names(key.vars) <- c("HH.pos", 
                     "Age", 
                     "Sex", 
                     "LFS",
                     "Fam.type",
                     "HHID",                    
                     "Gov.super",
                     "Non.gov.super",
                     "Weights",
                     "Sacrified.income.super",
                     "Employee.income.weekly",
                     "Age.pension",
                     "Total.income",
                     "Tax",
                     "Tot.income.gov",
                     "Super.income", 
                     "Super.cont.non.ss", 
                     "Super.cont.ss", 
                     "Super.lump.sum")

# Subset the data frame keeping on the key variables
person.dfkv <- person.df[,key.vars]
colnames(person.dfkv) <- names(key.vars)

# Create personID
person.dfkv$PID <- 1:nrow(person.dfkv)

# Create survey object
summary(person.dfkv$Gov.super)
summary(person.dfkv$Non.gov.super)

# Number of people in the sample with some super
aaa <- person.dfkv$Gov.super + person.dfkv$Non.gov.super
length(aaa[aaa >0])

# Create total super balance variable
person.dfkv$Total.super <- person.dfkv$Gov.super + person.dfkv$Non.gov.super

# Create own disposable income series (total less estimated taxes)
person.dfkv$Income.disp <- person.dfkv$Total.income - person.dfkv$Tax

# We also create our own definition of taxable income, which is total income less super income streams which are not taxed under the PIT
# For now we just take off super income streams which are tax free. 

person.dfkv$Taxable.income <- person.dfkv$Total.income - person.dfkv$Super.income 

person.dfkv$Taxable.income <- ifelse(person.dfkv$Taxable.income < 0, 0, person.dfkv$Taxable.income)

summary(person.dfkv$Total.income)
summary(person.dfkv$Taxable.income)

# In time we can look to extend this to better reconcile our definition of taxable income with that used by the ATO to determine actual tax liability. 

# The factors we would need to think through would be:

# (a) Investment income that is concessionally taxed - dividend imputatio, CGT discoubt, trusts
# (b) Deductibles - maybe we could just go with a standard deductible of $300, as proposed by Henry, and assume that takes us down to the right point. 
# (c) Alternatively we could just cut down our taxable income variable in proportion to the aggregate difference in taxable income and total income in ATO Taxation Statistics Individuals Detailed Tables - which was 95% in 2011-12 and 2012-13

# ----------------------------- Count people with super 

# People with some super
x <- sum((person.dfkv$Total.super > 0)*person.dfkv$Weights)
# People with no super
y <- sum((person.dfkv$Total.super <= 0)*person.dfkv$Weights)
(x+y)/10000000

# ======================================================================================================== #
# Set up price changes and tax rates         ===========
# ======================================================================================================== #

# Some basic calculations to adjust for inflation 

# We want to cost this for the 2015-16 budget 
# For now I'll just inflate at the inflation rate - in the future it may be better to inflate  variables by 10 year averages of inflation and wages, dependiong on the variable

# This also maintains the real value of super balances (and therefore also super earnings on those balances) over the years to 2015-16. More realistically super balances will compound on average since more $$$ are being added to the system each year in contributions and investment earnings, than are being taken out via drawdowns. Therefore, in this sense, our costings are relatively conservative as they are based off smaller super balances.

# Assume 2.5 per cent inflation rate

# Prices are now in 2015-16 terms since that's the budget we're looking at
years = 4
inflation.f <- 1.025^years

# List of variables that do not need to be adjusted
no.inflation <- c("HH.pos","Age","Sex","LFS","HHID","Weights", "PID", "Fam.type")

# Inflation adjusting the data frame (inflating all not selected as no inflation variables)
person.dfkvi <- cbind(person.dfkv[,no.inflation], (person.dfkv[,!names(person.dfkv) %in% no.inflation])*inflation.f)

#===================================================================================
# Investigating super balances

# We have total super balances, before inflating, of $1.14 trillion

sum(person.dfkv$Total.super * person.dfkv$Weights) / 10^9

# We have total super balances, after inflating, of $1.26 trillion

sum(person.dfkvi$Total.super * person.dfkvi$Weights) / 10^9

# By comparison, APRA reports total funds under management of $2 trillion as of end March 2015
# http://www.apra.gov.au/Super/Publications/Documents/1505-QSP-March2015.pdf

# Number of people with balances of over $1 million in 2011-12 is 110,000, or 0.6% of individuals, with total funds of $181 billion

person.dfkv %>% group_by(Total.super<10^6) %>% 
  summarise(No.individuals = sum(Weights), 
            Total.funds = sum(Total.super * Weights) / 10^9) %>% 
  ungroup %>%
  mutate(Prop.individuals = No.individuals / sum(No.individuals) * 100)

# Inflating forward, we estimate that 130,000 would now have balances of more than $1 million, or 0.7% of individuals, with total funds of $220 billion

person.dfkvi %>% group_by(Total.super<10^6) %>% 
  summarise(No.individuals = sum(Weights), 
            Total.funds = sum(Total.super * Weights) / 10^9)  %>% 
  ungroup %>%
  mutate(Prop.individuals = No.individuals / sum(No.individuals) * 100)

# Number of people with balances over $2.5 million is 13,480, with total funds of $50 billion

person.dfkv %>% group_by(Total.super < 2500000) %>% 
  summarise(No.individuals = sum(Weights), 
            Total.funds = sum(Total.super * Weights) / 10^9) %>% 
  ungroup %>%
  mutate(Prop.individuals = No.individuals / sum(No.individuals) * 100)

# Inflating forward, we estimate that around 16,000 would now have balances of more than $2.5 million, with total balances of $61.6 billion

person.dfkvi %>% group_by(Total.super < 2500000) %>% 
  summarise(No.individuals = sum(Weights), 
            Total.funds = sum(Total.super * Weights) / 10^9) %>% 
  ungroup %>%
  mutate(Prop.individuals = No.individuals / sum(No.individuals) * 100)

#===================================================================================
# Adding in extra tax policy variables

# We add these in afterwards as they are from 2014-15, so we dont want to inflate them up from 2012-13

# We map a new variable for individuals based on whether they are (a) indiv; or (b) couple

person.dfkvi$Fam.type.numeric <- as.numeric(person.dfkvi$Fam.type)
person.dfkvi$Single <- ifelse(person.dfkvi$Fam.type.numeric > 9 | person.dfkvi$Fam.type.numeric < 2, 1, 0)

# ======
# Medicare Levy

# We set the Medicare Levy phase-in thresholds for individuals, retirees and families (we prob wont use this last one as it's more complicated to implement, but feasible in SIH as we know how many kids they have)

# Need to check what year this are from, to make sure we're indexing them in the right way. At present we're indexing them as if they are the thresholds in 2012-13.

# Individuals
person.dfkvi$ML.lower <- rep(20896*1.025,length(person.dfkvi$Age))
person.dfkvi$ML.upper <- rep(26120*1.025,length(person.dfkvi$Age))
person.dfkvi$ML.lower.senior <- rep(33044*1.025,length(person.dfkvi$Age))
person.dfkvi$ML.upper.senior <- rep(41305*1.025,length(person.dfkvi$Age))

# Families - these thresholds are for family taxable income for the couple
# person.dfkvi$ML.lower.cpl <- rep(20896,length(person.dfkvi$Age))
# person.dfkvi$ML.upper.cpl <- rep(44076,length(person.dfkvi$Age))
# person.dfkvi$ML.lower.cpl.senior <- rep(33044,length(person.dfkvi$Age))
# person.dfkvi$ML.upper.cpl.senior <- rep(57500,length(person.dfkvi$Age))
# person.dfkvi$ML.per.child <- rep(3238,length(person.dfkvi$Age))

# ======
# SAPTO

# We add in the income thresholds and maximum offsets for SAPTO - we only use the individual thresholds for now. These are the figures for 2014-15. I dont think these thresholds are indexed - they've stayed the same over 2012-13, 2013-14 and 2014-15 - so we use the same ones in 2015-16

person.dfkvi$SAPTO.lower.indiv <- rep(32279, length(person.dfkvi$Age))
person.dfkvi$SAPTO.upper.indiv <- rep(50119, length(person.dfkvi$Age))
person.dfkvi$SAPTO.max.indiv <- rep(2230, length(person.dfkvi$Age)) 
person.dfkvi$SAPTO.lower.cpl <- rep(57948, length(person.dfkvi$Age))
person.dfkvi$SAPTO.upper.cpl <- rep(83580, length(person.dfkvi$Age))
person.dfkvi$SAPTO.max.cpl <- rep(1602, length(person.dfkvi$Age)) 

# ======
# LITO - income thresholds are unindexed

person.dfkvi$LITO.lower <- rep(37000, length(person.dfkvi$Age))
person.dfkvi$LITO.upper <- rep(66666, length(person.dfkvi$Age))
person.dfkvi$LITO.max <- rep(445, length(person.dfkvi$Age))

# ==================================================================================
# Subsetting for those aged over 60 in the drawdown phase 

# Where Age >= 22 we know they are over 60
# Where Super.income > 0, we know they are in the drawdown phase

# We start by defining a 'drawdown' variable, where we need Age as a numeric variable to filter on it 
person.dfkvi$Age.numeric <- as.numeric(person.dfkvi$Age)
person.dfkvi$Super.ddown <- ifelse(person.dfkvi$Age.numeric < 21, 0, ifelse(person.dfkvi$Super.income > 0, 1, 0))

# Now we see how many of those that are over 60, and have a super balance, are in the drawdown phase

person.dfkvi %>% filter(Age.numeric>21) %>% 
  group_by(Super.ddown) %>% 
  summarise(total.super = sum((Total.super * Weights) / 10^9)) 

# So roughly 70 per cent of the super assets of over 60s are in the drawdown phase. So it's important that we distinguish between super assets of over 60s that are in the drawdown phase, and those that are not.

# Are those over 60s that are not in the draw down phase earning higher incomes? 

person.dfkvi %>% filter(Age.numeric>21) %>% 
  group_by(Super.ddown) %>% 
  summarise(ave.income = weighted.mean(x = Total.income, w = Weights)) 

# So those over 60s in the drawdown phase have higher income than those that are not drawing down on thier super. This could just be their super drawdown boosting their incomes

person.dfkvi$Total.income.ex.super <- person.dfkvi$Total.income - person.dfkvi$Super.income

person.dfkvi %>% filter(Age.numeric>21) %>% 
  group_by(Super.ddown) %>% 
  summarise(ave.income = weighted.mean(x = Total.income.ex.super, w = Weights)) 

# So excluding incomes from super drawdown, those drawing on super have lower incomes. This may be because they get less pension income due to means tests. Anyway it doesnt really matter that much.  

# What do super drawdown behaviours look like across different ages?

ddown.df <- person.dfkvi %>% group_by(Age, Super.ddown) %>% 
  summarise(total.super = sum((Total.super * Weights) / 10^9)) 

# So we see some 59 year olds drawing down on super. From the age of 63 onwards most retirees are drawing down on their super balances.

# Now we generate out drawdown subset which we use for our earnings analysis

person.dfkvi.ddown <- person.dfkvi %>% filter(Age.numeric>21, Super.ddown == 1)  

# We also check out the super balances of those in the drawdown phase - total of $335 billion

sum(person.dfkvi.ddown$Total.super * person.dfkvi.ddown$Weights) / 10^9

# Number of people with balances of over $1 million in 2011-12 is 67,000, or 6.8% of those in the drawdown phase, with total funds of $113 billion

person.dfkvi.ddown %>% group_by(Total.super<1000000) %>% 
  summarise(No.individuals = sum(Weights), 
            Total.funds = sum(Total.super * Weights) / 10^9)  %>% 
  ungroup %>%
  mutate(Prop.individuals = No.individuals / sum(No.individuals) * 100)

# Number of people with balances over $2.5 million is 7,125, or 0.7 per cent of those in the drawdown phase, with total funds of $28 billion

person.dfkvi.ddown %>% group_by(Total.super < 2500000) %>% 
  summarise(No.individuals = sum(Weights), 
            Total.funds = sum(Total.super * Weights) / 10^9) %>% 
  ungroup %>%
  mutate(Prop.individuals = No.individuals / sum(No.individuals) * 100)

# =======================================================================================================
# Estimate super earnings        
# ======================================================================================================== #

# --------------
# Step 1 is to calculate earnings
# Set super rate of return

# The rate of return on super account balances will vary between the accumulation and drawdown phases. Retirees in the drawdown phase tend to be more conservative in their investment decisions, with a greater weighting towards safer, lower yielding assets such as bank deposits

# The 10-year average post-tax return for all super funds in the APRA Superannuation Bulletin is 6 per cent. This figure is net of fees and taxes on earnings, and therefore also includes any franking credits on investments in Australian equities or trusts. (http://www.apra.gov.au/Super/Publications/Documents/Revised%202013%20Annual%20Superannuation%20Bulletin%2005-02-14.pdf)

# With an effective tax rate on super earnings bounded by the tax rate on capital gains (10%) and the tax rate on other investment income for the super fund (15%), it's reasonable to suggest a pre-tax rate of return on super fund assets of 7% in the accumulation phase 

# In the drawdown phase we use a pre-tax rate of return of 5%. The 5% return in the drawdown phase comes from PBO advice to Senator David L on a similar costing that has been published onthr DLP's facebook page (no kidding...) - https://www.facebook.com/LDP.australia/posts/10152975190247672

Super.return.rate.acc <- 0.07 # 7 per cent return in the accumulation phase
Super.return.rate.ddown <- 0.05 # 5 per cent return in the drawdown phase

# So super earnings will depend upon whether super account holders are in the accumulation or drawdown phases

person.dfkvi$Super.earnings <- ifelse(person.dfkvi$Super.ddown == 1, Super.return.rate.ddown*person.dfkvi$Total.super, Super.return.rate.acc*person.dfkvi$Total.super)
summary(person.dfkvi$Super.earnings)

sum(person.dfkvi$Total.super * person.dfkvi$Weights) / 10^9 # We estimate total super balances of $1.26 trillion
sum(person.dfkvi$Super.earnings * person.dfkvi$Weights) / 10^9 # With annual super earnings of $81 billion

# By comparison, APRA Supperannuation Bulletin Statistics report total funds under management of $1.19 trillion as at June 2011, rising to $1.34 billion as at June 2012 and to $2 trillion . Funds reported investment earnings of $106 billion in 2010-11, and $24 billion in 2011-12, or an average of $65 billion across the 2 years. So our numbers probably stack up okay.
hist(person.dfkvi$Super.earnings, breaks = 200, xlim = c(0,50000))

# Need to redo these for over 60
person.dfkvi$cuts1000.earnings <- cut2(person.dfkvi$Super.earnings, cuts = seq(0,max(person.dfkvi$Super.earnings), 1000))
plot(person.dfkvi$cuts1000.earnings, xlim = c(0,50))

# =======================================================================================================
# Effective tax rate on super earnings 

# Here we apply an effective tax rate on super earnings, where those earnings are net of fees. Since franking credits are included in super fund income, these franking credits are taxed in the fund. Therefore the effective tax rate on super earnings should be bounded by the tax rate on capital gains (10%) and the tax rate on other investment income for the super fund (15%).

# For now, we assume a 50:50 split in super fund earnings between capital gains and other investment earnings. This leads to an effective tax rate on super earnings of 12.5%. We will structure the analysis to allow different effective tax rates on super earnings between the accumulation and drawdown phases, as those in the drawdown phase tend to invest in less risky, income earning assets with fewer or no capital gains (i.e. bank deposits)

# Set assumed tax rate - we use 8% currently but depends on the answers to the above
tax.rate.acc <- 0.125 # For the accumulation phase
tax.rate.ddown <- 0.14 # For the drawdown phase

person.dfkvi$super.earnings.tax <- ifelse(person.dfkvi$Super.ddown == 1, person.dfkvi$Super.earnings*tax.rate.ddown, person.dfkvi$Super.earnings*tax.rate.acc) 

# ==================================================================================
# Subsetting those over 60 for earnings analysis - exploratory really - we dont use this in the costings or the dist analysis

person.dfkvi.60 <- person.dfkvi[person.dfkvi$Age %in% levels(person.dfkvi$Age)[22:length(levels(person.dfkvi$Age))],]

# Summary stats

# 60+ year olds with super
x <- sum((person.dfkvi.60$Total.super > 0)*person.dfkvi.60$Weights)
y <- sum((person.dfkvi.60$Total.super <= 0)*person.dfkvi.60$Weights)
x/(x+y) # 43 per cent of those over 60 have some super

# Proportion with super by age group
person.dfkvi.60 %>% group_by(Age) %>% summarise(number.super = sum((Total.super > 0)*Weights), 
                                               Persons =sum(Weights),
                                               Prop.w.super = number.super/Persons,
                                               Ave.super = mean(Total.super))

# Those with super - what are their balances
person.dfkvi.60 %>% filter(Total.super > 0) %>% group_by(Age) %>% summarise(Ave.super = mean(Total.super))

# Based on their income quintile. This is unweighted - need to weight it. 
person.dfkvi.60$Disp.income.quintile <- cut2(person.dfkvi.60$Income.disp, g = 5)

# What are these people on massive negative incomes?
person.dfkvi.60 %>% group_by(Disp.income.quintile) %>% summarise(Ave.super = mean(Total.super))

ggplot(data = person.dfkvi.60, aes(x = Total.super, y = Income.disp, colour = Age)) + geom_point()

mod1 <- lm(data = person.dfkvi.60, Total.super ~ Income.disp + as.factor(Age))
summary(mod1)

person.dfkvi.60$Super.pred <- predict(mod1)
x <- person.dfkvi.60 %>% group_by(Disp.income.quintile) %>% summarise(Ave.super = mean(Super.pred))
plot(x)

# Cut by $1000k increments of super earnings
table.60.earnings <- person.dfkvi.60 %>% group_by(cuts1000.earnings) %>% summarise(number.persons = sum(Weights),
                                                                                  tax.collected = sum(super.earnings.tax*Weights))


income.table.60 <- person.dfkvi.60 %>% group_by(Age, Disp.income.quintile) %>% summarise(number.super = sum((Total.super > 0)*Weights), 
                                                                                        Persons =sum(Weights),
                                                                                        Prop.w.super = number.super/Persons,
                                                                                        Super.bal.total = sum(Total.super*Weights),
                                                                                        Wages = sum(Employee.income.weekly*Weights),
                                                                                        Age.pension = sum(Age.pension*Weights),
                                                                                        Super.income = sum(Super.income*Weights),
                                                                                        Total.income = sum(Total.income*Weights),
                                                                                        Income.tax = sum(Tax*Weights),
                                                                                        number.super = sum((Total.super > 0)*Weights))

# ==================================================================================
# Establishing a no tax concession counterfactual for super earnings

# Now we establish the counterfactual where super fund earnings are taxed at individuals' marginal tax rates. This personal income tax benchmark will allow us to establish the value of the super earnings tax concession 

# First we need to annualise total income, so that we can pass a PIT function over it to work out how much tax they would pay

person.dfkvi$Total.income.annual <- person.dfkvi$Total.income * 52
person.dfkvi$Taxable.income.annual <- person.dfkvi$Taxable.income * 52

# We create a new variable that adds in an estimate of the earnings from superannuation fund balances, which are not captured in the SIH 11-12. What the SIH includes on super is the value of any benefits paid  from a superannuation fund, such as a pension or a lump sum withdrawals.

# We also also assuming that all withdrawals from super (pension or lump sum) are not included in estimates of total income. 

person.dfkvi$Total.income.annual.s <- person.dfkvi$Total.income.annual + person.dfkvi$Super.earnings
person.dfkvi$Taxable.income.annual.s <- person.dfkvi$Taxable.income.annual + person.dfkvi$Super.earnings

# Source: SIH Questionairre: Module 3.12 (Super), http://www.ausstats.abs.gov.au/Ausstats/subscriber.nsf/0/7EAA6F8CC80D5855CA257BD60010E895/$File/sih%202011-12%20paper%20questionnaire.pdf

# Now we write a quick tax function - we include the Medicare Levy, SAPTO and LITO

tax.function <- function(income){
  
  tax <- ifelse(income<18200, 0, 
                ifelse(income<37000, (income-18200)*0.19, 
                       ifelse(income<80000, 3572 + (income-37000)*0.325,
                              ifelse(income<180000, 17547 + (income-80000)*0.37, 54547 + 0.47*(income-180000)))))
  tax
}

# We also write a function for the Medicare Levy

ML.function <- function(income, 
                        ML.lower, 
                        ML.upper, 
                        ML.lower.senior, 
                        ML.upper.senior,
                        Age.numeric){

  Medicare.levy <- ifelse(Age.numeric < 27,ifelse(income < ML.lower, 0, 
                                                  ifelse(income < ML.upper, (income - ML.lower) * 0.1, 0.02 * income)), 
                          ifelse(income<ML.lower.senior, 0, 
                                 ifelse(income < ML.upper.senior, (income - ML.lower.senior) * 0.1, 0.02 * income)))
  
  Medicare.levy  

}

# And a function for SAPTO entitlement
# Here we distingush between the entitlements for individuals and couples, as well as by age. We don't however, currently allow couples to share their SAPTO entitlements between them (as can happen) and so we will underestimate the degree to which SAPTO reduces personal income tax paid at the aggregate. If we have time we'll add this in.

SAPTO.function <- function(income, 
                           Age.numeric,
                           Single,
                           SAPTO.lower.indiv, 
                           SAPTO.upper.indiv, 
                           SAPTO.max.indiv, 
                           SAPTO.lower.cpl, 
                           SAPTO.upper.cpl, 
                           SAPTO.max.cpl) {
  
  SAPTO <- ifelse(Age.numeric < 27, 
                  0, 
                  ifelse(Single == 1, 
                         ifelse(income < SAPTO.lower.indiv, 
                                SAPTO.max.indiv,
                                ifelse(income < SAPTO.upper.indiv, 
                                       (SAPTO.upper.indiv - income) * 0.125, 
                                       0)), 
                         ifelse(income < SAPTO.lower.cpl, 
                                SAPTO.max.cpl,
                                ifelse(income < SAPTO.upper.cpl, 
                                       (SAPTO.upper.cpl - income) * 0.125, 
                                       0))))
                                  
  SAPTO
}

# And a function for LITO entitlement

LITO.function <- function(income, LITO.lower, LITO.upper, LITO.max) {
  
  LITO <- ifelse(income < LITO.lower, LITO.max, 
                 ifelse(income < LITO.upper, (LITO.upper - income) * 0.015, 0))
}

# We also define an 'unused' tax free threshold function, which we will pick up later when we consider behavioural change in response to the reintroduction of taxes on super earnings in the drawdown phase 

TF.unused.function <- function(income) {
  
  TF.unused <- ifelse(income < 0, 18200, 
                      ifelse(income<18200, (18200 - income), 0))
}


# And we estimate each person's tax liability

person.dfkvi$Tax.estimate <- tax.function(person.dfkvi$Taxable.income.annual) 

sum(person.dfkvi$Tax.estimate * person.dfkvi$Weights)/10^9 # $199.8 billion

# We alo estimate their Medicare Levy liability - this uses taxable income for the income test 

person.dfkvi$Medicare.levy <- ML.function(person.dfkvi$Taxable.income.annual,
                                          person.dfkvi$ML.lower, 
                                          person.dfkvi$ML.upper, 
                                          person.dfkvi$ML.lower.senior, 
                                          person.dfkvi$ML.upper.senior, 
                                          person.dfkvi$Age.numeric)

person.dfkvi$Tax.estimate <- person.dfkvi$Tax.estimate + person.dfkvi$Medicare.levy

sum(person.dfkvi$Medicare.levy * person.dfkvi$Weights)/10^9 # ML raises $17.4 billion

sum(person.dfkvi$Tax.estimate * person.dfkvi$Weights)/10^9 # $199.8 billion once ML included

# Neither SAPTO or LITO are refundable tax offsets, so we only apply them to reduce tax if the individual actually has a tax liability up to this point

# LITO

person.dfkvi$LITO.entitlement <- LITO.function(person.dfkvi$Taxable.income.annual, 
                                               person.dfkvi$LITO.lower, 
                                               person.dfkvi$LITO.upper, 
                                               person.dfkvi$LITO.max)

person.dfkvi$Tax.estimate <- ifelse(person.dfkvi$Tax.estimate < person.dfkvi$LITO.entitlement, 0,
                                    person.dfkvi$Tax.estimate - person.dfkvi$LITO.entitlement)

sum(person.dfkvi$Tax.estimate * person.dfkvi$Weights)/10^9 # $200.8 billion

# SAPTO 

# We need to distinguish for individuals based on age here, and family status (single vs couple) - DONE
# SAPTO eligibility uses an income definition known as 'rebate income' which is the sum of (a) Taxable Income; (b) Adjusted fringe Benefits; (c) Total net investment loss (financial net investment loss and rental property loss); and (d) reportable super contributions
# For now, we're just going to use taxable income since we dont know most of these things. As a result, we're going to be more generous in providing SAPTO entitlements to individuals than the ATO will be

person.dfkvi$SAPTO.entitlement <- SAPTO.function(person.dfkvi$Taxable.income.annual,
                                                 person.dfkvi$Age.numeric,
                                                 person.dfkvi$Single,
                                                 person.dfkvi$SAPTO.lower.indiv, 
                                                 person.dfkvi$SAPTO.upper.indiv, 
                                                 person.dfkvi$SAPTO.max.indiv, 
                                                 person.dfkvi$SAPTO.lower.cpl, 
                                                 person.dfkvi$SAPTO.upper.cpl, 
                                                 person.dfkvi$SAPTO.max.cpl)

person.dfkvi$Tax.estimate <- ifelse(person.dfkvi$Tax.estimate < person.dfkvi$SAPTO.entitlement, 0, 
                                    person.dfkvi$Tax.estimate - person.dfkvi$SAPTO.entitlement)

sum(person.dfkvi$Tax.estimate * person.dfkvi$Weights)/10^9 # $199.8 billion

# Now we have our estimated tax entitlement, accounting for the Medicare Levy (individuals only), and SAPTO and LITO entitlements
# We compare this against the ABS-derived tax estimate in the SIH 11-12, which used the PIT tax scales for that year, including a 1.5% Medicare Levy

person.dfkvi$Tax.annual <- person.dfkvi$Tax * 52
(sum(person.dfkvi$Tax.estimate * person.dfkvi$Weights) - sum(person.dfkvi$Tax.annual * person.dfkvi$Weights)) / 10 ^ 9 # 52.8 billion difference
sum(person.dfkvi$Tax.annual * person.dfkvi$Weights) / 10^9 # ABS estimates PIT of $147 billion for 2011-12
sum(person.dfkvi$Tax.estimate * person.dfkvi$Weights) / 10^9 # We estimate PIT of $199.8 billion for 2015-16. 

# There is clearly a difference in the taxable income base we use. ATO Revenue Statistics reports total personal income tax revenues of $149.8 billion in 2011-12, but our figures are based on different tax settings, and are inflated ot 2015-16. The CW Budget projects personal income tax receipts of $176.6 billion in 2015-16, which is what we should really be aiming for. So our taxable income is still too high   

# We also generate an estimate of the tax collected if super earnings were taxed as part of regular income 

person.dfkvi$Tax.estimate.s <- tax.function(person.dfkvi$Taxable.income.annual.s) 

sum(person.dfkvi$Tax.estimate.s * person.dfkvi$Weights)/10^9 # $226.4 billion

# We alo estimate their Medicare Levy liability

person.dfkvi$Medicare.levy <- ML.function(person.dfkvi$Taxable.income.annual.s,
                                          person.dfkvi$ML.lower, 
                                          person.dfkvi$ML.upper, 
                                          person.dfkvi$ML.lower.senior, 
                                          person.dfkvi$ML.upper.senior, 
                                          person.dfkvi$Age.numeric)

person.dfkvi$Tax.estimate.s <- person.dfkvi$Tax.estimate.s + person.dfkvi$Medicare.levy

sum(person.dfkvi$Medicare.levy * person.dfkvi$Weights)/10^9 # ML raises $17.4 billion

sum(person.dfkvi$Tax.estimate.s * person.dfkvi$Weights)/10^9 # $243.8 billion once ML included

# Neither SAPTO or LITO are refundable tax offsets, so we only apply them to reduce tax if the individual actually has a tax liability up to this point

# LITO

person.dfkvi$LITO.entitlement <- LITO.function(person.dfkvi$Taxable.income.annual.s, 
                                               person.dfkvi$LITO.lower, 
                                               person.dfkvi$LITO.upper, 
                                               person.dfkvi$LITO.max)

person.dfkvi$Tax.estimate.s <- ifelse(person.dfkvi$Tax.estimate.s < person.dfkvi$LITO.entitlement, 0,
                                    person.dfkvi$Tax.estimate.s - person.dfkvi$LITO.entitlement)

sum(person.dfkvi$Tax.estimate.s * person.dfkvi$Weights)/10^9 # $243.8 billion

# SAPTO

person.dfkvi$SAPTO.entitlement <- SAPTO.function(person.dfkvi$Taxable.income.annual.s,
                                                 person.dfkvi$Age.numeric,
                                                 person.dfkvi$Single,
                                                 person.dfkvi$SAPTO.lower.indiv, 
                                                 person.dfkvi$SAPTO.upper.indiv, 
                                                 person.dfkvi$SAPTO.max.indiv, 
                                                 person.dfkvi$SAPTO.lower.cpl, 
                                                 person.dfkvi$SAPTO.upper.cpl, 
                                                 person.dfkvi$SAPTO.max.cpl)

person.dfkvi$Tax.estimate.s <- ifelse(person.dfkvi$Tax.estimate.s < person.dfkvi$SAPTO.entitlement, 0, 
                                    person.dfkvi$Tax.estimate.s - person.dfkvi$SAPTO.entitlement)

sum(person.dfkvi$Tax.estimate.s * person.dfkvi$Weights) / 10^9 # Taxing super earnings at PIT marginal rates, we estimate total PIT of $243.8 billion



# ==================================================================================
# Establishing a base case for current taxation arrangements for super

# Currently earnings on super balances for those aged under 60 are taxed at 15 per cent (or 10 per cent in the case of capital gains)
# For those aged over 60, and drawing the minimum (4% annually) from their super account, super earnings are tax free

# So we establish a base case treatment of super earnings taxation under the current arrangements

# First we convert Age into a numeric varible upon which we can filter for different tax treatments of super by Age
person.dfkvi$Age.numeric <- as.factor(person.dfkvi$Age)
person.dfkvi$Age.numeric <- as.numeric(person.dfkvi$Age.numeric)

# We now assign super earnings tax liability, excluding those that are aged over 60 and in the drawdown phase. 

person.dfkvi$super.earnings.tax.current <- ifelse(person.dfkvi$Super.ddown == 1, 0, person.dfkvi$Super.earnings * tax.rate.acc)

sum(person.dfkvi$super.earnings.tax.current * person.dfkvi$Weights) / 10^9

# So we estimate that current super taxes on earnings raise $7.9 billion annually

# Total taxes on super funds averaged $7.3 billion between 2004-05 and 2013-14, and were $7.9 billion in 2011-12. However this also includes taxes paid on concessional contributions. 

# Source: http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/5506.02013-14?OpenDocument

# So the current value of the tax concession for each taxpayer, relative to a personal income tax benchmark, is:

person.dfkvi$Super.concession <- person.dfkvi$Tax.estimate.s - person.dfkvi$Tax.estimate - person.dfkvi$super.earnings.tax.current

# And the total value of the tax concession for super earnings in 2015-16 is:

sum(person.dfkvi$Super.concession * person.dfkvi$Weights) / 10^9 # $18.7 billion

# By comparison, the 2015-16 Budget forecasts a super earnings tax concession tax expenditure of $16.15 billion (http://www.budget.gov.au/2015-16/content/bp1/download/Budget_Paper_No_1.pdf)

# ==================================================================================
# Establishing the value of tax free super for over 60s, compared to a 15% super earnings tax on everyone

# We do this compared to a benchmark of 15% tax on super earnings (for everyone, including those aged 60+)

person.dfkvi$Super.concession.over60s <- (person.dfkvi$Super.earnings * tax.rate.ddown) - person.dfkvi$super.earnings.tax.current 

sum(person.dfkvi$Super.concession.over60s * person.dfkvi$Weights) / 10^9 # Concession is worth $3.4 billion, as per costing below

# ==================================================================================
# Exploring reform options - no behaviour change

# For now we investigate these reform options without behavioural change. We'll come back to the question of behavioural change later, as it's important

# In this section we consider 3 reform options:

# (a) Reintroduce a 15% tax rate on super for over 60s (Grattan Proposal)
# (b) Reintroduce a 15% tax rate on super for over 60s - with an $18200 tax free threshold
# (c) Apply a 30% super earning tax rate for under 60s - with a $75000 tax free threshold (ALP policy)

# ==================================================================================
# (a) Reintroduce a 15% tax rate on super for over 60s

person.dfkvi$super.earnings.tax.opt1 <- ifelse(person.dfkvi$Age.numeric < 27, 
                                               person.dfkvi$Super.earnings * tax.rate.acc, 
                                               person.dfkvi$Super.earnings * tax.rate.ddown)

# Total super earnings tax raised - $11.1 billion

sum(person.dfkvi$super.earnings.tax.opt1 * person.dfkvi$Weights) / 10^9
  
# Size of super earnings tax expenditure post reform is $17.9 billion

person.dfkvi$Super.concession.opt1 <- person.dfkvi$Tax.estimate.s - person.dfkvi$Tax.estimate - person.dfkvi$super.earnings.tax.opt1

sum(person.dfkvi$Super.concession.opt1 * person.dfkvi$Weights) / 10^9

# Budget saving from reintroducing the 15% tax on super earnings for over 60s - $3.2 billion

sum((person.dfkvi$super.earnings.tax.opt1 - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9

# ==================================================================================
# (b) 20000  - tax free threshold for over 60s in super drawdown phase

person.dfkvi$super.earnings.tax.20000tf <- ifelse(person.dfkvi$Super.ddown == 1,
                                                  ifelse(person.dfkvi$Super.earnings - 20000 >= 0,
                                                         (person.dfkvi$Super.earnings-20000) *tax.rate.ddown, 0), 
                                                  person.dfkvi$Super.earnings * tax.rate.acc)

# Value of the tax concession, relative to a personal income tax benchmark, becomes:

person.dfkvi$Super.concession.20000tf <- person.dfkvi$Tax.estimate.s - person.dfkvi$Tax.estimate - person.dfkvi$super.earnings.tax.20000tf

sum(person.dfkvi$Super.concession.20000tf * person.dfkvi$Weights) / 10^9 # Total concession is $17.2 billion

# The value of the tax concession, compared to a world where everyone pays 15% tax on super earniings, is:

person.dfkvi$Super.concession.20000tf.over60s <- person.dfkvi$super.earnings.tax.opt1 - person.dfkvi$super.earnings.tax.20000tf

sum(person.dfkvi$Super.concession.20000tf.over60s * person.dfkvi$Weights) / 10^9 

# How much does it save  - $1 billion

sum((person.dfkvi$super.earnings.tax.20000tf - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9 

# How much do we sacrifice by including a tax free threshold for over 60s of 20000, compared to no TF threshold - $2.2 billion. 

sum((person.dfkvi$super.earnings.tax.opt1 - person.dfkvi$super.earnings.tax.20000tf) * person.dfkvi$Weights) / 10^9 

# So we lose well more than half of the potential revenue gain by including a TF thresholf of 20000

# We'll just check this against the share of total earnings that would be captured by the TF threshold

person.dfkvi %>% filter(Super.ddown == 1) %>% 
  group_by(Super.earnings<20000) %>%  
  summarise(earnings.total = sum(Super.earnings * Weights)) %>%
  ungroup %>% mutate(earnings.prop = earnings.total/sum(earnings.total))

# So 28 per cent of super earnings come from ppl that have less than 20k in earnings. 

# However with a TF threshold we also lose a whole bunch of tax $$ from those earning more than 20k would have tax free earnings on the first 20k they earn also

person.dfkvi$Super.earnings.under.20kTF <- ifelse(person.dfkvi$Super.ddown == 1, 
                                                  ifelse(person.dfkvi$Super.earnings < 20000,person.dfkvi$Super.earnings, 20000), 
                                                  0)

person.dfkvi %>% filter(Super.ddown == 1) %>%
  group_by(Super.earnings < 20000) %>%
  summarise(no.individuals = sum(Weights), 
            earnings.total.under20kTF = sum(Super.earnings.under.20kTF * Weights), 
            earnings.total = sum(Super.earnings * Weights)) %>%
  ungroup %>% mutate(earnings.prop = earnings.total.under20kTF / sum(earnings.total))

# So 60% of earnings in the draw down phase would fall under the TF threshold, 28% from those earning less than 20k from super in ddown phase, and 32% from those earning more than 20k annually from super in the ddown phase. 

# An alternative policy would be to put together a rebate on super earnings, like LITO or SAPTO, that is withdrawn for higher super earnings. However the taper rate could have the effect of worsening EMTRs to long run savings via super for middle-upper income earners, and would add more complexity.     

# ==================================================================================
# (c) a tax free threshold of 75,000 for under 60s, tax free super earnings for over 60s in ddown phase, consistent with proposed ALP policy

person.dfkvi$super.earnings.tax.ALP <- ifelse(person.dfkvi$Super.ddown == 1, ifelse(person.dfkvi$Super.earnings >= 75000,((person.dfkvi$Super.earnings-75000) * tax.rate.ddown), 0), person.dfkvi$Super.earnings * tax.rate.acc)

# Value of the tax concession becomes:

person.dfkvi$Super.concession.ALP <- person.dfkvi$Tax.estimate.s - person.dfkvi$Tax.estimate - person.dfkvi$super.earnings.tax.ALP

sum(person.dfkvi$Super.concession.ALP * person.dfkvi$Weights) / 10^9 # Total concession is $18.5 billion

# How much does it save compared to the current tax concessions on super earnings - $0.2 billion

sum((person.dfkvi$super.earnings.tax.ALP - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9 

# This estimate is much lower than the PBO's costing of a policy for David L, in a request to the PBO which sought to replicate the ALP's proposed (but unpublished) policy. That costing estimated a revenue impact of $600 million in 2018-19.  

# ==================================================================================
# Exploring reform options - with behavioural change

# Now we take a look at the reform options accounying for behavioural change with respect to whether ppl choose to withdraw their super savings to make use of Tax free thresholds outside of the super system.

# In this section we consider 3 reform options:

# (a) Reintroduce a 15% tax rate on super for over 60s (Grattan Proposal)
# (b) Reintroduce a 15% tax rate on super for over 60s - with an $18200 tax free threshold
# (c) Apply a 30% super earning tax rate for under 60s - with a $75000 tax free threshold (ALP policy)

# ==================================================================================
# (a) Reintroduce a 15% tax rate on super for over 60s

person.dfkvi$super.earnings.tax.opt1 <- ifelse(person.dfkvi$Super.ddown == 1, 
                                               person.dfkvi$Super.earnings * tax.rate.acc, 
                                               person.dfkvi$Super.earnings * tax.rate.ddown)

# However, those in the drawdown phase that now face tax on super earnings from the first $ have a strong incentive to withdraw funds from super in order to make the most of the tax free threshold that exists outside of super. We assume that individuals respond to the tax change by minimising their tax by withdrawing super earnings up to the point that they maximise their tax-free threshold and LITO and SAPTO entitlements outside of super. 

# Incidentally, this modelling approach also satisfies alternative approaches to administering any tax free threshold, or rebate on super earnings, where super earnings are included in taxable income on individuals' tax returns, allowing retirees to make the most of any unused TF threshold, SAPTO and LITO entitlements, and any tax paid on super earnings can be paid out of individuals' disposable incomes, or financed out of their super account balances each year.

# So we write in our behavioural change - ppl affected withdraw what super earnings they can from super to make use of TF thresholds outside of super

# We need to write a variable for the effective tax free income threshold for each individuals, based on the TF threshold and their eligibility for SAPTO and LITO

# Working out what tax bracket they are in, including the Medicare Levy for those above 37k - we dont use this yet but could come in handy

tax.brackets <- c(-200000,18200, 37000, 80000, 180000)
person.dfkvi$marginal.tax.rate <- cut2(person.dfkvi$Taxable.income.annual.s, cuts = tax.brackets)
levels(person.dfkvi$marginal.tax.rate) <- c(0,0,0.19,0.345,0.39,0.47)
person.dfkvi$TF.threshold <- rep(18200, length(person.dfkvi$Age))

# The unused taxfree threshold, in terms of tax paid, is the some of any SAPTO and LITO entitlements of an individual, plus any unused portion of the 18200 TF threshold, multiplied by the tax rate on earnings in the drawdown phase

person.dfkvi$Excess.TF.income.threshold <- ifelse(person.dfkvi$Super.ddown == 1, 
                                                  TF.unused.function(person.dfkvi$Taxable.income.annual) * tax.rate.ddown + 
                                                    (person.dfkvi$SAPTO.entitlement + 
                                                       person.dfkvi$LITO.entitlement), 0) 

# So now we know how much tax ppl can avoid by shifting their earnings out of super to make the most of any remaining TF threshold and SAPTO / LITO entitlements outside of the super system

# Super earnings tax collected after behaviour change

person.dfkvi$super.earnings.tax.opt1.behav <- ifelse(person.dfkvi$super.earnings.tax.opt1 < person.dfkvi$Excess.TF.income.threshold, 0, person.dfkvi$super.earnings.tax.opt1 - person.dfkvi$Excess.TF.income.threshold)

# We write a variable of the tax foregone by behavioural change

person.dfkvi$super.earnings.tax.opt1.foregone <- person.dfkvi$super.earnings.tax.opt1 - person.dfkvi$super.earnings.tax.opt1.behav

sum(person.dfkvi$super.earnings.tax.opt1.foregone * person.dfkvi$Weights)/10^9

# So only lose $1 billion via behavioural change

# We compare the two super tax reform outcomes to make sure the function has worked properly

beffect.opt1.df <- data_frame(Total.income.s = person.dfkvi$Total.income.annual.s,
                               Super.earnings.untaxed = person.dfkvi$Super.earnings,
                               Super.earnings.tax.opt1 = person.dfkvi$super.earnings.tax.opt1,
                               Opt1.no.behav = person.dfkvi$super.earnings.tax.opt1,
                               Opt1.w.behav = person.dfkvi$super.earnings.tax.opt1.behav,
                               Opt1.b.change = person.dfkvi$super.earnings.tax.opt1.foregone,
                               TF.outside = person.dfkvi$Excess.TF.income.threshold,
                               Super.ddown = person.dfkvi$Super.ddown) %>% filter(Super.ddown == 1) 
View(beffect.opt1.df)

# What proportion of those in the drawdown phase reduce their tax liability through behavioural change?

Behav.effect.opt1.df <- person.dfkvi %>% filter(Super.ddown ==1) %>% 
  group_by(super.earnings.tax.opt1 == super.earnings.tax.opt1.behav) %>% 
  summarise (No.individuals = sum(Weights),
             Total.earnings = sum(Super.earnings * Weights),
             Total.extra.tax.no.behav = sum((super.earnings.tax.opt1 - super.earnings.tax.current) * Weights),
             Total.super.tax.fgone = sum(super.earnings.tax.opt1.foregone * Weights)) %>%
  ungroup %>%
  mutate(Prop.individuals = No.individuals / sum(No.individuals), 
         Prop.earnings = Total.earnings / sum(Total.earnings), 
         Prop.earnings.tax.foregone = Total.super.tax.fgone / sum(Total.extra.tax.no.behav))

View(Behav.effect.opt1.df)


# So 70% of those affected could reduce their tax liability by shifting $$ from super, and we lose 25% of the potential extra super earnings tax

# Total super earnings tax raised - $10 billion

sum(person.dfkvi$super.earnings.tax.opt1.behav * person.dfkvi$Weights) / 10^9

# Size of super earnings tax expenditure post reform is $18.4 billion

person.dfkvi$Super.concession.opt1.behav <- person.dfkvi$Tax.estimate.s - person.dfkvi$Tax.estimate - person.dfkvi$super.earnings.tax.opt1.behav

sum(person.dfkvi$Super.concession.opt1.behav * person.dfkvi$Weights) / 10^9

# Budget saving from reintroducing the 15% tax on super earnings for over 60s - $2.1 billion

sum((person.dfkvi$super.earnings.tax.opt1.behav - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9

# What does behavioural change cost us? $1 billion

sum((person.dfkvi$super.earnings.tax.opt1 - person.dfkvi$super.earnings.tax.opt1.behav) * person.dfkvi$Weights) / 10^9

# ==================================================================================
# (b) 20000  - tax free threshold for over 60s in super drawdown phase

person.dfkvi$super.earnings.tax.20000tf <- ifelse(person.dfkvi$Super.ddown == 1,
                                                  ifelse(person.dfkvi$Super.earnings - 20000 >= 0,
                                                         (person.dfkvi$Super.earnings-20000) *tax.rate.ddown, 0), 
                                                  person.dfkvi$Super.earnings * tax.rate.acc)

# Now with behavioural change, we only collect the tax on those that can't withdraw some of their asets from super to make the most of SAPTO, LITO and unused TF thresholds outside of the super system

person.dfkvi$super.earnings.tax.20000tf.behav <- ifelse(person.dfkvi$super.earnings.tax.20000tf < person.dfkvi$Excess.TF.income.threshold, 0, 
                                                        person.dfkvi$super.earnings.tax.20000tf - person.dfkvi$Excess.TF.income.threshold)


# We write a variable of the tax foregone by behavioural change

person.dfkvi$super.earnings.tax.20000tf.foregone <- person.dfkvi$super.earnings.tax.20000tf - person.dfkvi$super.earnings.tax.20000tf.behav

sum(person.dfkvi$super.earnings.tax.20000tf.foregone * person.dfkvi$Weights)/10^9

# So only lose $0.3 billion via behavioural change. Most of those that would choose to shift money out of super are those that would benefit from TF earnings inside of super with a 20k super earnings TF threshold, or an equivalent rebate

# We compare the two super tax reform outcomes to make sure the function has worked properly

beffect.20kTF.df <- data_frame(Total.income.s = person.dfkvi$Total.income.annual.s,
                               Super.earnings.untaxed = person.dfkvi$Super.earnings,
                               Super.earnings.tax.opt1 = person.dfkvi$super.earnings.tax.opt1,
                               Opt1.no.behav = person.dfkvi$super.earnings.tax.opt1,
                               Opt1.w.behav = person.dfkvi$super.earnings.tax.opt1.behav,
                               Opt1.b.change = person.dfkvi$super.earnings.tax.opt1.foregone,
                               Opt2.20kTF.no.behav = person.dfkvi$super.earnings.tax.20000tf,
                               Opt2.20kTF.w.behav = person.dfkvi$super.earnings.tax.20000tf.behav,
                               Opt2.20kTF.b.change = person.dfkvi$super.earnings.tax.20000tf.foregone,
                               TF.outside = person.dfkvi$Excess.TF.income.threshold,
                               Super.ddown = person.dfkvi$Super.ddown) %>% filter(Super.ddown == 1) 
View(beffect.20kTF.df)

# What proportion of those in the drawdown phase reduce their tax liability through behavioural change?

Behav.effect.20kTF.df <- person.dfkvi %>% filter(Super.ddown ==1) %>% 
  group_by(super.earnings.tax.20000tf == super.earnings.tax.20000tf.behav) %>% 
  summarise (No.individuals = sum(Weights),
             Total.earnings = sum(Super.earnings * Weights),
             Total.extra.tax.no.behav = sum((super.earnings.tax.20000tf - super.earnings.tax.current) * Weights),
             Total.extra.tax.behav = sum((super.earnings.tax.20000tf.behav - super.earnings.tax.current) * Weights),
             Total.super.tax.fgone = sum(super.earnings.tax.20000tf.foregone * Weights)) %>%
  ungroup %>%
  mutate(Prop.individuals = No.individuals / sum(No.individuals), 
         Prop.earnings = Total.earnings / sum(Total.earnings), 
         Prop.earnings.tax.foregone = Total.super.tax.fgone / sum(Total.extra.tax.no.behav))

View(Behav.effect.20kTF.df)

# So 10 percent of those in the drawdown phase have an incentive to change behaviour when we re-introduce the super earnings tax with the 20k TF threshold, and revenue leakage from behaviour change is only equal to 6 per cent of the taxable super earnings in the drawdown phase. This is because most of those that would have had an incentive to withdraw $$$ from super to make the most of unused tax free income entitlements in the PIT system no longer have a reason to, as their earnings are exampt within super. In other words, there is a strong correlation between having a low super balance, and having unused TF income entitlements in the PIT system.    

# Value of the tax concession, relative to a personal income tax benchmark, becomes:

person.dfkvi$Super.concession.20000tf.behav <- person.dfkvi$Tax.estimate.s - person.dfkvi$Tax.estimate - person.dfkvi$super.earnings.tax.20000tf.behav

sum(person.dfkvi$Tax.estimate.s * person.dfkvi$Weights)/10^9
sum(person.dfkvi$Tax.estimate * person.dfkvi$Weights)/10^9
sum(person.dfkvi$super.earnings.tax.20000tf.behav * person.dfkvi$Weights)/10^9

sum(person.dfkvi$Super.concession.20000tf.behav * person.dfkvi$Weights) / 10^9 # Total concession is $17.8 billion

# How much does it save - $0.7 billion

sum((person.dfkvi$super.earnings.tax.20000tf.behav - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9 

# How much do we sacrifice by including a tax free threshold for over 60s of 20000, compared to no TF threshold - $1.4 billion. 

sum((person.dfkvi$super.earnings.tax.opt1.behav - person.dfkvi$super.earnings.tax.20000tf.behav) * person.dfkvi$Weights) / 10^9 

# So we lose roughly half the revenue with a TF thresold, but less than before we account for behavioural change. This is because most of those that can avoid the tax via by taking $$ out of super and benefit from TF thresholds under PIT are all helped by making the first 20k of super earnings tax free.

# Incidentally the revenue leakage due to behavioural change under this proposal is just $60 million

sum((person.dfkvi$super.earnings.tax.20000tf.foregone * person.dfkvi$Weights)) / 10^9

# ==================================================================================
# (c) a tax free threshold of 75,000 for over 60s, consistent with proposed ALP policy

person.dfkvi$super.earnings.tax.ALP <- ifelse(person.dfkvi$Super.ddown == 1, ifelse(person.dfkvi$Super.earnings >= 75000,((person.dfkvi$Super.earnings-75000) * tax.rate.ddown), 0), person.dfkvi$Super.earnings * tax.rate.acc)

# Now with behavioural change, we only collect the tax on those that can't withdraw some of their asets from super to make the most of SAPTO, LITO and unused TF thresholds outside of the super system

person.dfkvi$super.earnings.tax.ALP.behav <- ifelse(person.dfkvi$super.earnings.tax.ALP < person.dfkvi$Excess.TF.income.threshold, 0, 
                                                        person.dfkvi$super.earnings.tax.ALP - person.dfkvi$Excess.TF.income.threshold)

# We write a variable of the tax foregone by behavioural change

person.dfkvi$super.earnings.tax.ALP.foregone <- person.dfkvi$super.earnings.tax.ALP - person.dfkvi$super.earnings.tax.ALP.behav

sum(person.dfkvi$super.earnings.tax.ALP.foregone * person.dfkvi$Weights)/10^9

#  There is no revenue leakage via behavioural change. Basically no one with super earnings of more than 75k has unused TF threshold, SAPTO or LITO entitlements in the PIT system

# We compare the two super tax reform outcomes to make sure the function has worked properly

beffect.ALP.df <- data_frame(Total.income.s = person.dfkvi$Total.income.annual.s,
                               Super.earnings.untaxed = person.dfkvi$Super.earnings,
                               Super.earnings.tax.opt1 = person.dfkvi$super.earnings.tax.opt1,
                               Opt1.no.behav = person.dfkvi$super.earnings.tax.opt1,
                               Opt1.w.behav = person.dfkvi$super.earnings.tax.opt1.behav,
                               Opt1.b.change = person.dfkvi$super.earnings.tax.opt1.foregone,
                               Opt2.ALP.no.behav = person.dfkvi$super.earnings.tax.ALP,
                               Opt2.ALP.w.behav = person.dfkvi$super.earnings.tax.ALP.behav,
                               Opt2.ALP.b.change = person.dfkvi$super.earnings.tax.ALP.foregone,
                               TF.outside = person.dfkvi$Excess.TF.income.threshold,
                               Super.ddown = person.dfkvi$Super.ddown) %>% filter(Super.ddown == 1) %>% filter(Super.earnings.untaxed > 75000)
View(beffect.ALP.df)

# What proportion of those in the drawdown phase reduce their tax liability through behavioural change?

Behav.effect.ALP.df <- person.dfkvi %>% filter(Super.ddown ==1) %>% 
  group_by(super.earnings.tax.ALP == super.earnings.tax.ALP.behav) %>% 
  summarise (No.individuals = sum(Weights),
             Total.earnings = sum(Super.earnings * Weights),
             Total.extra.tax.no.behav = sum((super.earnings.tax.ALP - super.earnings.tax.current) * Weights),
             Total.extra.tax.behav = sum((super.earnings.tax.ALP.behav - super.earnings.tax.current) * Weights),
             Total.super.tax.fgone = sum(super.earnings.tax.ALP.foregone * Weights)) %>%
  ungroup %>%
  mutate(Prop.individuals = No.individuals / sum(No.individuals), 
         Prop.earnings = Total.earnings / sum(Total.earnings), 
         Prop.earnings.tax.foregone = Total.super.tax.fgone / sum(Total.extra.tax.no.behav))

View(Behav.effect.ALP.df)


Ddown.stats <- person.dfkvi %>% filter(Age.numeric > 26) %>% group_by(Super.ddown == 1) %>%
  summarise(no.individuals = sum(Weights), 
            mean.income = weighted.mean(Total.income.annual, Weights), 
            mean.income.ex.super.pymts = weighted.mean(Total.income.ex.super, Weights)) %>% ungroup %>%
  mutate(prop.individuals = no.individuals / sum(no.individuals)) 
  
# Value of the tax concession becomes:

person.dfkvi$Super.concession.ALP <- person.dfkvi$Tax.estimate.s - person.dfkvi$Tax.estimate - person.dfkvi$super.earnings.tax.ALP.behav

sum(person.dfkvi$Super.concession.ALP * person.dfkvi$Weights) / 10^9 # Total concession is $20.8 billion

# How much does it save compared to the current tax concessions on super earnings - $0.18 billion

sum((person.dfkvi$super.earnings.tax.ALP.behav - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9 

# This estimate is much lower than the PBO's costing of a policy for David L, in a request to the PBO which sought to replicate the ALP's proposed (but unpublished) policy. That costing estimated a revenue impact of $600 million in 2018-19. 

# ==================================================================================
# Distributional analysis: super earnings and tax concessions by total income (both with and without super earnings)
# ================================================================================== #

# Here we establish weighted quintiles for annual total income (with and without super income)

# First lets make sure our income variables are spot on

summary(person.dfkvi$Total.income) # There are people with large negative incomes

# The 6523.0 - Household Income and Income Distribution, Australia, 2011-12  has the following to say on the matter:

# Some households report extremely low and even negative income in the survey, which places them well below the safety net of income support provided by government pensions and allowances. Households may under report their incomes in the survey at all income levels, including low income households. However, households can correctly report low levels of income if they incur losses in their unincorporated business or have negative returns from their other investments.

# When creating equivalised income deciles the ABS converts negative disposable incomes to zero

# http://www.abs.gov.au/AUSSTATS/abs@.nsf/Lookup/6523.0Explanatory%20Notes12011-12?OpenDocument

# Therefore we first re-write total income such that it cant be negative

person.dfkvi$Total.income.annual <- ifelse(person.dfkvi$Total.income.annual < 0,0, person.dfkvi$Total.income.annual)

person.dfkvi$Total.income.annual.s <- ifelse(person.dfkvi$Total.income.annual.s < 0, 0, person.dfkvi$Total.income.annual.s)

# Then we define a survey object (you need the survey pkg for this)

SIHP.svy <- svydesign(id=~PID, weights= ~Weights, fpc=NULL, data = person.dfkvi)

# Then we use this to define survey weighted quintiles and deciles

# Deciles for total income (excluding super) and total income (including super)
SIHP.tincome.10 <- svyquantile(~Total.income.annual, SIHP.svy,c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1), ci = FALSE)
SIHP.tincome.s.10 <- svyquantile(~Total.income.annual.s, SIHP.svy,c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1), ci = FALSE)

# So now we use cut to break up the dataset along the deciles specified in the survey object

# Total income (excluding super) 
person.dfkvi$tincome.decile <- cut(person.dfkvi$Total.income.annual, breaks = SIHP.tincome.10) %>% as.numeric()
person.dfkvi$tincome.decile.range <- cut(person.dfkvi$Total.income.annual, breaks = SIHP.tincome.10) 

# Total income (including super)
person.dfkvi$tincome.s.decile <- cut(person.dfkvi$Total.income.annual.s, breaks = SIHP.tincome.s.10) %>% as.numeric()
person.dfkvi$tincome.s.decile.range <- cut(person.dfkvi$Total.income.annual.s, breaks = SIHP.tincome.s.10)

# ==================================================================================
# Final outputs for super chapter in budget repair report - figures, charts and tables
# ==================================================================================

# ==================================================================================
# Our final costings for the reform options, without behavioural change, which are cited in the chapter, are:

# (a) Budget saving from reintroducing the 15% tax on super earnings for over 60s - $3.2 billion
sum((person.dfkvi$super.earnings.tax.opt1 - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9

# (b) 20000  - tax free threshold for over 60s in super drawdown phase - $1 billion
sum((person.dfkvi$super.earnings.tax.20000tf - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9 

# (c) a tax free threshold of 75,000 for under 60s, tax free super earnings for over 60s, consistent with proposed ALP policy - $0.2 billion
sum((person.dfkvi$super.earnings.tax.ALP - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9 
# Note: we might not end up using this last one in the chapter

# ==================================================================================
# Our final costings for the reform options, with behavioural change, which are cited in the chapter, are:

# (a) Budget saving from reintroducing the 15% tax on super earnings for over 60s - $2.1 billion
sum((person.dfkvi$super.earnings.tax.opt1.behav - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9

# (b) 20000  - tax free threshold for over 60s in super drawdown phase - $0.7 billion
sum((person.dfkvi$super.earnings.tax.20000tf.behav - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9 

# (c) a tax free threshold of 75,000 for under 60s, tax free super earnings for over 60s, consistent with proposed ALP policy - $0.2 billion
sum((person.dfkvi$super.earnings.tax.ALP - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9 
# Note: we might not end up using this last one in the chapter

#==================================================================================
# Investigating non-concessional super contributions 


#==================================================================================
# Our final charts for the chapter are:
#================================================================================== #

# Chart 1 - Untaxed super earnings and value of tax free super earnings for over 60s by total income (excluding super earnings) decile of over 60s
Chart1.df <- person.dfkvi %>% filter(Age.numeric >=22) %>% 
  filter(!is.na(tincome.decile)) %>% group_by(tincome.decile.range) %>% 
  summarise(Mean.super.earnings =  sum(Super.earnings * Weights) / sum(Weights), 
            Mean.tax.concession = sum(Super.concession.over60s * Weights) / sum(Weights), 
            Super.share.earnings = sum((Super.earnings / Total.income.annual.s) * Weights) / sum(Weights) * 100)

# We see that when we exclude super earnings from the calculation of our income deciles then we end up with lots of ppl with significant super earnings (but few other income streams) in the bottom income decile. This is because super earnings are not included in total income in the SIH as the earnings compound in the fund, rather than becoming available for consumption. Therefore we use Chart 2 instead, which includes super earnings in total income when calculating our total income deciles.

# Chart 2 - Total income (including super earnings) of over 60s
Chart2.df <- person.dfkvi %>% filter(Age.numeric >=22) %>% 
  filter(!is.na(tincome.decile)) %>%
  group_by(tincome.s.decile.range) %>% 
  summarise(Mean.super.earnings =  sum(Super.earnings * Weights) / sum(Weights), 
            Mean.tax.concession = sum(Super.concession.over60s * Weights) / sum(Weights))

write.table(Chart2.df , sep = "\t", file = clip <- pipe("pbcopy", "w"))
close(clip)0

# Chart 3 - Tax concession by income decile with a TF threshold, and under current arrangements
Chart3.df <- person.dfkvi %>% filter(Age.numeric >=22) %>% 
  filter(!is.na(tincome.decile)) %>%
  group_by(tincome.s.decile.range) %>% 
  summarise(Mean.super.earnings =  sum(Super.earnings * Weights) / sum(Weights),
            Mean.tax.concession.current = sum(Super.concession.over60s * Weights) / sum(Weights), 
            Mean.tax.concession.20000tf = sum(Super.concession.20000tf.over60s * Weights) / sum(Weights))

# Chart 4 - Value of super earnings tax concessions under current arrangements and reform proposals compared to personal income tax benchmark

Chart4.df <- person.dfkvi %>% filter(Age.numeric >=22) %>% 
  filter(!is.na(tincome.decile)) %>%
  group_by(tincome.s.decile.range) %>% 
  summarise(Mean.super.earnings =  sum(Super.earnings * Weights) / sum(Weights),
            Mean.tax.concession.current = sum(Super.concession * Weights) / sum(Weights), 
            Mean.tax.concession.opt1 = sum(Super.concession.opt1 * Weights) / sum(Weights), 
            Mean.tax.concession.20000tf = sum(Super.concession.20000tf * Weights) / sum(Weights), 
            Mean.tax.concession.ALP = sum(Super.concession.ALP * Weights) / sum(Weights)) 

# Chart 5 - Summary costing of super earnings tax concession reforms - without behavioural change

Chart5.df <- data_frame(Reform.15percent.tax.on.everyone = sum((person.dfkvi$super.earnings.tax.opt1 - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9, 
                        Reform.15percent.tax.20000tf = sum((person.dfkvi$super.earnings.tax.20000tf - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9, 
                        Reform.15percent.tax.75000tf = sum((person.dfkvi$super.earnings.tax.ALP - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9)

# Chart 6 - Summary costing of earnings tax concession reforms - comparing pre- and post behavioural change

Chart6.df <- data_frame(Reform.15percent.tax.on.everyone = sum((person.dfkvi$super.earnings.tax.opt1 - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9, 
                        Reform.15percent.tax.20000tf = sum((person.dfkvi$super.earnings.tax.20000tf - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9, 
                        Reform.15percent.tax.75000tf = sum((person.dfkvi$super.earnings.tax.ALP - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9,
                        Reform.15percent.tax.on.everyone.bchange = sum((person.dfkvi$super.earnings.tax.opt1.behav - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9, 
                        Reform.15percent.tax.20000tf.bchange = sum((person.dfkvi$super.earnings.tax.20000tf.behav - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9, 
                        Reform.15percent.tax.75000tf.bchange = sum((person.dfkvi$super.earnings.tax.ALP.behav - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9)

# (a) Budget saving from reintroducing the 15% tax on super earnings for over 60s - $3.2 billion
sum((person.dfkvi$super.earnings.tax.opt1 - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9

# (b) 20000  - tax free threshold for over 60s in super drawdown phase - $1 billion
sum((person.dfkvi$super.earnings.tax.20000tf - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9 

# (c) a tax free threshold of 75,000 for under 60s, tax free super earnings for over 60s, consistent with proposed ALP policy - $0.2 billion
sum((person.dfkvi$super.earnings.tax.ALP - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9 

# Chart 6 - Summary costing of ASFA proposal - limit concessions to those earning more than 


# ==================================================================================
# BT / ASFA proposal a tax free threshold of 150,000 for over 60s in the drawdown phase
# ================================================================================== #

# We dont bother with behavioural change when ew're talking about a TF threshold of 150k

person.dfkvi$super.earnings.tax.BT <- ifelse(person.dfkvi$Super.ddown == 1, ifelse(person.dfkvi$Super.earnings >= 150000,((person.dfkvi$Super.earnings-150000) * tax.rate.ddown), 0), person.dfkvi$Super.earnings * tax.rate.acc)

# Value of the tax concession becomes:

person.dfkvi$Super.concession.BT <- person.dfkvi$Tax.estimate.s - person.dfkvi$Tax.estimate - person.dfkvi$super.earnings.tax.BT

sum(person.dfkvi$Super.concession.BT * person.dfkvi$Weights) / 10^9 # Total concession is $18.6 billion

# How much does it save compared to the current tax concessions on super earnings - $112 million in 2015-16

sum((person.dfkvi$super.earnings.tax.BT - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9 

#==================================================================================
# Stats quoted in super chapter

# IF WE NEED TO CALCULATE ANY STATS THAT ARE CITED IN THE CHAPTER TEXT - DO THEM HERE

# JD media request

data.df <- person.dfkvi %>% group_by(tincome.decile) %>% summarise(Total.income = sum(Total.income * Weights))

data2.df <- person.dfkvi %>% group_by(tincome.decile) %>% summarise(Total.income = sum(Total.income * Weights))

#==================================================================================
# Cutting room floor
#==================================================================================

# Cam's old table on super earnings tax concessions, which we no longer use

income.table.60 <- person.dfkvi.60 %>% group_by(Age, Disp.income.quintile) %>% summarise(number.super = sum((Total.super > 0)*Weights), 
                                                                                         Persons =sum(Weights),
                                                                                         Prop.w.super = number.super/Persons,
                                                                                         Super.bal.total = sum(Total.super*Weights),
                                                                                         Wages = sum(Employee.income.weekly*Weights),
                                                                                         Age.pension = sum(Age.pension*Weights),
                                                                                         Super.income = sum(Super.income*Weights),
                                                                                         Total.income = sum(Total.income*Weights),
                                                                                         Income.tax = sum(Tax*Weights),
                                                                                         number.super = sum((Total.super > 0)*Weights))

# -------------------------------------------------------------------------------------------------------------------------


