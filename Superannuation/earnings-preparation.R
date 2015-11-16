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