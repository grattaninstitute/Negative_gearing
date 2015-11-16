

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


# Create total super balance variable
person.dfkv$Total.super <- person.dfkv$Gov.super + person.dfkv$Non.gov.super

# Create own disposable income series (total less estimated taxes)
person.dfkv$Income.disp <- person.dfkv$Total.income - person.dfkv$Tax

# We also create our own definition of taxable income, which is total income less super income streams which are not taxed under the PIT
# For now we just take off super income streams which are tax free. 

person.dfkv$Taxable.income <- person.dfkv$Total.income - person.dfkv$Super.income 
person.dfkv$Total.income <- person.dfkv$Total.income - person.dfkv$Super.income 
person.dfkv$Total.income.inc.wdls <- person.dfkv$Total.income + person.dfkv$Super.income
person.dfkv$Total.income.annual <- person.dfkv$Total.income * 52

person.dfkv$Taxable.income <- ifelse(person.dfkv$Taxable.income < 0, 0, person.dfkv$Taxable.income)
person.dfkv$Taxable.income.annual  <- person.dfkv$Taxable.income * 52

# In time we can look to extend this to better reconcile our definition of taxable income with that used by the ATO to determine actual tax liability. 

# The factors we would need to think through would be:

# (a) Investment income that is concessionally taxed - dividend imputatio, CGT discoubt, trusts
# (b) Deductibles - maybe we could just go with a standard deductible of $300, as proposed by Henry, and assume that takes us down to the right point. 
# (c) Alternatively we could just cut down our taxable income variable in proportion to the aggregate difference in taxable income and total income in ATO Taxation Statistics Individuals Detailed Tables - which was 95% in 2011-12 and 2012-13

# ----------------------------- Count people with super 

# ======================================================================================================== #
# Set up price changes and tax rates         ===========
# ======================================================================================================== #

# Some basic calculations to adjust for inflation 

# We want to cost this for the 2015-16 budget 
# For now I'll just inflate at the inflation rate - in the future it may be better to inflate  variables by 10 year averages of inflation and wages, depending on the variable

# We dont inflate super balances at this stage since we adjust them separately below. This is because (a) balances compound over time at a rate faster than inflation - ore $$$ are being added to the system each year in contributions and investment earnings, than are being taken out via drawdowns; and (b) the survey data is likely to underreport aggregate super balances 

# Assume 2.5 per cent inflation rate

# Prices are now in 2015-16 terms since that's the budget we're looking at
years = 4
inflation.f <- 1.025^years

# List of variables that do not need to be adjusted
no.inflation <- c("HH.pos","Age","Sex","LFS","HHID","Weights", "PID", "Fam.type", "Total.super", "Gov.super", "Non.gov.super")

# Inflation adjusting the data frame (inflating all not selected as no inflation variables)
person.dfkvi <- cbind(person.dfkv[,no.inflation], (person.dfkv[,!names(person.dfkv) %in% no.inflation])*inflation.f)

# By comparison, APRA reports total funds under management of $2 trillion as of end June 2015
# http://www.apra.gov.au/Super/Publications/Documents/1508-QSP-June2015.pdf

# So we inflate forward our super account balances by a factor of 2/1.14. This way our weighted survey sample has total account balances equal to the aggregate account balances nationally, and we assume that the distribution of super balances across our survey sample holds across the whole population 

# Let x be our growth factor
x <- 2/1.14

# Inflating super balances by x
person.dfkvi$Gov.super <- person.dfkvi$Gov.super * x
person.dfkvi$Non.gov.super <- person.dfkvi$Non.gov.super * x
person.dfkvi$Total.super <- person.dfkvi$Total.super * x

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
person.dfkvi$Super.ddown <- ifelse(person.dfkvi$Age.numeric < 22, 0, ifelse(person.dfkvi$Super.income > 0, 1, 0))

