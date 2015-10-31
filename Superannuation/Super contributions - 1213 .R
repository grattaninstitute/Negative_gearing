# Budget repair report
# Superannuation contribution concessions 

# Major problem here is the lack of behavioural change - likely to actually save less.

# Housekeeping ------------------------------------------------------------

# Load required packages
require(ggplot2)
require(dplyr)
require(car)
require(Hmisc)
require(reshape2)
library(tidyr)

# I know we should we using relative file paths but this will work for now
setwd("..")

# Read in data - select key variables -------------------------------------

# Read in the tax stats 2012-13 sample file
taxstats13.df <- readr::read_csv("2013 ATO.csv")

# Select the variables we care about
variables <- c("Ind",
               "Gender",
               "age_range",
               "Occ_code",
               "Sw_amt",
               "Aust_govt_pnsn_allw_amt", # Government pensions or allowances amount
               "Taxable_Income", # Taxable income (total income less total deductions and allowable prior year losses)
               "Taxed_othr_pnsn_amt", # Australian annuity or superannuation income stream - taxed
               "Untaxed_othr_pnsn_amt", # Australian annuity or superannuation income stream - untaxed
               "Non_emp_spr_amt", # Non-employer sponsored superannuation contribution deductions
               "Rptbl_Empr_spr_cont_amt", # Reportable employer superannuation contributions - i.e. salary sacrificed super contributions beyond the SG
               "Spouse_adjusted_taxable_inc", # Spouse adjusted taxable income
               "Tot_inc_amt", # Total income
               "Net_fincl_invstmt_lss_amt") # Net financial investment loss

variables.names <- c("ID",
                     "Sex",
                     "Age",
                     "Occ",
                     "Wages",
                     "Gov_pension",
                     "Taxable_income",
                     "Other_pension_taxed",
                     "Other_pension_untaxed",
                     "Non_emp_spr_deductions",
                     "Rpt_emp_spr_deductions",
                     "Spouse_adjusted",
                     "Total.income")

# Subset the data according the variables we care about
taxstats13.df <- taxstats13.df[, variables]
names(taxstats13.df) <- variables.names

# ================================================================================================================================
# Create tax bases and generate necessary superannuation variables


# Set contribution tax rate (Superannuation Guarantee)
cont.rate <- 0.15

# Calculate compulsory SG contributions (assumption that these are based on wages only)
taxstats13.df$Super.SG <- taxstats13.df$Wages*0.095

# For the 2012-13 tax year SG were only 9 per cent
taxstats13.df$Super.SG.1213 <- taxstats13.df$Wages*0.09

# Check amount of SG contributions
sum(taxstats13.df$Super.SG)*50/10^9
sum(taxstats13.df$Super.SG)*50/10^9*(9/9.5)
# We estimate $53.5 billion in SG contributions in 2012-13, or $50.7 billion at a 9 per cent super guarantee rate.

# Concessionally taxed voluntary super contributions in taxstats

# Write voluntary contributions variable also

taxstats13.df$Super.voluntary <- taxstats13.df$Rpt_emp_spr_deductions + taxstats13.df$Non_emp_spr_deductions
sum(taxstats13.df$Super.voluntary)*50/10^9

# Tax stats includes reportable superannuation contributions. These are superannuation contributions, beyond those made by employers under the super guarantee, which are reported to the ATO as part of income tests for various taxes and payments, such as the Medicare levy surcharge, and the senior and pension tax offsets.

# These reportable super contributions consist of:

# (a) Reportable employer super contributions employers make on behalf of taxpayers: this includes salary sacrificed contributions made out of pre tax income, or where a taxpayer negotiates for a part of exta employer super contributions as part of a salary package. It doesnt include SG contributions, or extra employer contributions negotiated as part of a collective agreement (i.e. APS employees recieve 12 percent)

# For more info see: https://www.ato.gov.au/Individuals/Super/In-detail/Contributions/Guide-for-employees-and-self-employed---reportable-superannuation-contributions/?page=4#Reportable_employer_super_contributions

# (b) Personal deductable contributions made by taxpayers: You can claim an income tax deduction for personal contributions you make to a super fund if you meet certain eligibility criteria (i.e. if recieve bulk of income from self employment, so dont benefit from SG super contributions).

# For more info see: https://www.ato.gov.au/Individuals/Super/In-detail/Contributions/Guide-for-employees-and-self-employed---reportable-superannuation-contributions/?page=3#Personal_deductible_contributions
# And for eligibility criteria for deduction on these types of contributions: https://www.ato.gov.au/Individuals/Tax-Return/2014/Supplementary-tax-return/Deduction-questions-D11-D15/D12---Personal-superannuation-contributions/

# It's unclear whether reportable super contributions, as described above, account for all concessionally taxed voluntary super contributions made by individuals. It may depend on what you deem to be 'voluntary contributions'. For example, higher than SG contribution rates for APS employees are negotiated collectively, not individually, so can be argued that they dont count for voluntary super contributions for our purposes. What we're really interested are those concessionally taxed contributions that individuals make of their own accord.

# Its clear that tax stats wont capture non concessional voluntary contributions where they are made out of post tax income. These are only reported to the ATO by super funds separately, so that the ATO can administer the non-concessional contributions cap of $180k per year.

# Total super contributions (voluntary + SG)
taxstats13.df$Super.total <- taxstats13.df$Rpt_emp_spr_deductions + taxstats13.df$Super.SG + taxstats13.df$Non_emp_spr_deductions
(sum(taxstats13.df$Super.total)*50)/10^9 # So we have $66 billion in super contributions in 2012-13 at 9.5% SG rate

taxstats13.df$Super.total.1213 <- taxstats13.df$Rpt_emp_spr_deductions + taxstats13.df$Super.SG.1213 + taxstats13.df$Non_emp_spr_deductions
(sum(taxstats13.df$Super.total.1213)*50)/10^9 # So we have about $63 billion at the 9% SG rate

# By comparison the APRA Annual Superannuation Bulletin (Table 7) shows total superannuation contributions of $115 billion, of which $78 billion in employer contributions, $37 billion in member contributions, and $1 billion in 'other' contributions'.

# Currently, super annuation is not included in a persons taxable income (salary sacrificied it becomes a deducion)
# However, Henry recommends including it as income and thus we need to create an alternative taxable income measure including super

# The second is taxable income plus any SG and salary sacrificed super contribtuions (for Henry reco and others)
taxstats13.df$Taxable_income_s <- taxstats13.df$Taxable_income + taxstats13.df$Super.total

# Alterations and new variables -------------------------------------------

## Need to create a 50 or over variable for differential super contribution caps
taxstats13.df$Over.50 <- ifelse(taxstats13.df$Age <= 4, 1,0)
taxstats13.df$Over.50 <- as.factor(taxstats13.df$Over.50)

# Recoding Age variables for clear labelling
# But first we add an age variable that we can still filter on

taxstats13.df$Age.numeric <- taxstats13.df$Age

taxstats13.df$Age <- recode(taxstats13.df$Age, "0 = '70 and over';
                            1 = '65 to 69';
                            2 = '60 to 64';
                            3 = '55 to 59';
                            4 = '50 to 54';
                            5 = '45 to 49';
                            6 = '40 to 44';
                            7 = '35 to 39';
                            8 = '30 to 34';
                            9 = '25 to 29';
                            10 = '20 to 24';
                            11 = 'under 20'")

# We also create some more aggregated age groupings

taxstats13.df$Age.group <- ifelse(taxstats13.df$Age == "under 20", "Under 30", 
                                  ifelse(taxstats13.df$Age == "20 to 24", "Under 30",
                                         ifelse(taxstats13.df$Age == "25 to 29", "Under 30",
                                                ifelse(taxstats13.df$Age == "30 to 34", "30 to 49",
                                                       ifelse(taxstats13.df$Age == "35 to 39", "30 to 49",
                                                              ifelse(taxstats13.df$Age == "40 to 44", "30 to 49",
                                                                     ifelse(taxstats13.df$Age == "45 to 49", "30 to 49",
                                                                            ifelse(taxstats13.df$Age == "50 to 54", "50 to 54",
                                                                                   ifelse(taxstats13.df$Age == "55 to 59", "55 to 59",
                                                                                          ifelse(taxstats13.df$Age == "60 to 64", "60 to 64",
                                                                                                 ifelse(taxstats13.df$Age == "65 to 69", "65 to 69","70 and over")))))))))))


# Ensuring at the age is interpreted as a factor
taxstats13.df$Age <- as.factor(taxstats13.df$Age)

# Need to build in our lower and upper income thresholds for the medicare levy. We dont just write these into the tax function below because they are indexed to inflation.
# Source: https://www.ato.gov.au/Individuals/Medicare-levy/Reduction-for-people-on-low-incomes/Income-thresholds-for-Medicare-levy-reduction/

taxstats13.df$ML.lower <- rep(20542,length(taxstats13.df$Age))
taxstats13.df$ML.upper <- rep(24167,length(taxstats13.df$Age))


# Set up price changes and tax rates --------------------------------------

# Some basic calculations to adjust for wage inflation - this way bracket creep is already factored in

# We want to cost this for the 2015-16 budget 
# For now I'll just inflate at the inflation rate - in the future it may be better to inflate the variables by 10 year averages of inflation and wages, dependiong on the variable

# Assume 2.5 per cent inflation rate

# Prices are now in 2015-16 terms since that's the budget we're looking at
inflation.f <- 1.025^3 

# List of variables that do not need to be adjusted
no.inflation <- c("ID","Sex","Age","Occ","Over.50","Age.numeric", "Age.group")

# Inflation adjusted data frame (inflating all not selected as no inflation variables)
taxstats13.dfi <- cbind(taxstats13.df[,no.inflation], (taxstats13.df[,!names(taxstats13.df) %in% no.inflation])*inflation.f)

# Load an income tax function ---------------------------------------------
# First need to load in a tax function (These are the 2014-15 tax rates - were the same in 2013-14 and 2012-13). We use the 2% medicare levy surcharge as thats the policy from 2014-15 onwards
# See - https://www.ato.gov.au/rates/individual-income-tax-rates/
# Also - https://www.ato.gov.au/Individuals/Medicare-levy/
# Note - we include the medicare levy as it applies to individuals and assume no one is eligible for the seniors and pensioners tax offset, in which case they would face a higher threshold before the ML kicks in

tax.function <- function(income, ML.lower, ML.upper){
  tax <- ifelse(income<18200, 0, 
                ifelse(income<37000, (income-18200)*0.19, 
                       ifelse(income<80000, 3572 + (income-37000)*0.325,
                              ifelse(income<180000, 17547 + (income-80000)*0.39, 54547 + 0.45*(income-180000)))))
  medicare.levy <- ifelse(income<ML.lower,0, 
                          ifelse(income < ML.upper, (income - ML.lower)*.1, 0.02*income))
  
  tax + medicare.levy
}

# ---- Updating for division 293

# Application of Division 293 ---------------------------------------------

# Division 293 applies to those with taxable incomes of over 300k
# It effectively taxes the contributions of those on high incomes at a rate of 30 per cent instead of 15.
# www.dfrdb.gov.au/your-scheme/division-293-tax/ for info

# The defition of incomes for Division 293 is not just taxable incomes (i.e. total income less total deductions and allowable prior year losses). Instead the 300k threshold is applied to 'income for surcharge purposes', which is the basis for assessing eligibility for the Medicare Levy Surcharge. See: https://www.ato.gov.au/Individuals/Income-and-deductions/Income-tests/#Income_for_surcharge_purposes

# The definition of income for Division 293 tax is the same as when assessing your liability to pay the Medicare levy surcharge (MLS):

# (a) taxable income (including the net amount on which family trust distribution tax has been paid)
# (b) reportable fringe benefits amount, as reported on the payment summary
# (c) total net investment loss (includes both net financial investment loss and net rental property loss)
# (d) reportable super contributions (includes both reportable employer super contributions and deductible personal super contributions; and
# (e) if you had a spouse, your spouse's share of the net income of a trust on which the trustee must pay tax.

# If it's the concessional contributions that push someone's 'income for surcharge purposes' over 300k, then the tax would apply only to those concessional contributions that take their 'income for surcharge purposes' above 300k. For example, someone with income excluding their concessional contributions of $285,000, and concessional contributions of $20,000 (taking their total income to $305,000), would have the reduced tax concession only apply to $5000 of their contributions.

# For now, we're just going to take taxable income and add in concessional super contributions. The difference between this definition of income, and that actually used in the Division 293 tax, this difference is likely to be fairly small.

# So now we apply the concessional rate to the lesser of concessional contributions or taxable income + contributions of 300k. 

# Step 1 is to calculate the income assessable under division 293
taxstats13.dfi$div293.incass <- ifelse((taxstats13.dfi$Taxable_income + taxstats13.dfi$Super.total) > 300000, taxstats13.dfi$Taxable_income + taxstats13.dfi$Super.total, 0)

# Work out the amount to be subject to the extra 15 per cent tax (Really this is about taxing anything in excess of 300k, to remove any jumps in MTR)
taxstats13.dfi$div293.taxbase <- ifelse(taxstats13.dfi$div293.incass > 0, apply(cbind(max((taxstats13.dfi$Taxable_income + taxstats13.dfi$Super.total - 300000),0),taxstats13.dfi$Super.total), 1, min),0)
sum(taxstats13.dfi$div293.taxbase)*50/10^9

# Analysis (QC check) of 293 tax base ----------------------------------------------
# Quick review of this tax base - looks ok compared to figures in Swan PR on announcement (http://ministers.treasury.gov.au/DisplayDocs.aspx?doc=pressreleases/2013/020.htm&pageID=&min=brs&Year=&DocType=0)

taxstats13.dfi %>% filter(div293.taxbase > 0) %>% summarise(share.div293 = length(div293.taxbase) * 50) # It affects 147,500 taxpayers (PR said 128,000)
table(taxstats13.dfi$div293.taxbase > 0)/length(taxstats13.dfi$div293.taxbase) # Just over 1 percent pay the Div 293 tax
summary(taxstats13.dfi[taxstats13.dfi$div293.taxbase > 0,]$div293.taxbase) # The most anyone would pay in Div 293 tax is $2.2 million
taxstats13.dfi %>% filter(div293.taxbase > 0) %>% summarise(taxtake.div293 = (sum(div293.taxbase) *0.15 * 50)/10^9) # We say it raises $860 million, 2012-13 Budget says it will raise $200 million. We might have to revisit our crude definition of 'income for surcharge purposes' here, and our treatment of incomes around the 300K threshold

hist(taxstats13.dfi[taxstats13.dfi$div293.taxbase > 0,]$div293.taxbase) # But a histogram shows most of the revenue is raised from taxpayers that arent on right on the 300k threshold

# Calculating current and full tax  ---------------------------------------

taxstats13.dfi$Tax.current <- tax.function(taxstats13.dfi$Taxable_income,taxstats13.dfi$ML.lower,taxstats13.dfi$ML.upper) + cont.rate*(taxstats13.dfi$Super.total + taxstats13.dfi$div293.taxbase)
(sum(taxstats13.dfi$Tax.current)*50)/10^9 # We estimate personal income tax returns from taxable income at $189.9 billion, including taxes on superannuation contributions. By comparison, 2014-15 Budget has personal income tax receipts at 169 billion (http://www.budget.gov.au/2014-15/content/bp1/download/BP1_combined.pdf).

# Tax payable when super contributions are included in the tax base
# Important to note that the no concession case treats super income as "in pocket" it is no longer deductable.
taxstats13.dfi$Tax.no.conc <- tax.function(taxstats13.dfi$Taxable_income_s,taxstats13.dfi$ML.lower,taxstats13.dfi$ML.upper)
(sum(taxstats13.dfi$Tax.no.conc)*50)/10^9 
# So with no super contributions tax concessions, we estimate that the CW would raise $205.7 billion in personal income tax from taxable income (including that contributed to super funds via SG and voluntary contributions)

# Tax expenditure estimate ------------------------------------------------

# The difference between these two is the current value of total contribution concessions 
# (multiply by 50 to get 100 per cent sample)
# This number is a good test of model - should be close to TES
sum(taxstats13.dfi$Tax.no.conc - taxstats13.dfi$Tax.current)*50/10^9 # Our TES estimate is $15.8 billion
# So we estimate value of tax expenditures at $14.5 for contributions concessions. Bo comparison, 2014-15 TES for super contributions is forecast to be $17.8 billion in 2014-15 and $19.15 billion in 2015-16.
# http://www.budget.gov.au/2014-15/content/bp1/download/BP1_combined.pdf
# http://www.treasury.gov.au/~/media/Treasury/Publications%20and%20Media/Publications/2013/TES/downloads/PDF/TES_2013_Consolidated.ashx

# Who is worse off without LISC --------------------------------------------------

# This shows who is worse off without the LISC as their SG contributions are taxed at 15%, while their labour income is untaxed up to the tax free threshold of 18200.

# Negatives should be interpreted as those made worse off under the current system
diff <- taxstats13.dfi$Tax.no.conc - taxstats13.dfi$Tax.current
# Shows those worse off up to total taxable income
plot(taxstats13.dfi$Taxable_income_s, diff, xlim = c(0,20000), ylim = c(-3000,1000))
# Proportion worse off is about 11 per cent of the population. Substantial!
table(diff < 0)/length(diff)

# ==========================================================================================================================================
# Analysis of the options
# ========================================================================================================================

# Grattan scenarios 

# 1. Contribution concession capped 10k (assuming full marginal rate there after)

# Set the new contribution cap
concession.cap <- 10000

# Create a variable for the concessional contribution. We assume no one contributes more than 10k under the SG
taxstats13.dfi$conc.cont <- ifelse(taxstats13.dfi$Super.total > concession.cap, concession.cap, taxstats13.dfi$Super.total)
# Tax paid on super contributions
taxstats13.dfi$conc.cont.tax <-  taxstats13.dfi$conc.cont*0.15
sum(taxstats13.dfi$conc.cont.tax)*50/10^9 # So $8 billion paid in taxes on concessional contributions

# We also have to work out how much Div 293 tax should be collected, since this tax will still exist alongside the 10k contributions cap. For example, those earning over 300k will still pay a 30 percernt tax on income that ends up in super contributions, compared to 15 percent for everyone else, for their contributions up to the 10k cap.

# Step 1 is to shift any income that was previously contributed to super above the 10k cap back into taxable income so it can be taxed at the full marginal rate. This includes both SG and voluntary contributions

taxstats13.dfi$Taxable_income_s_10kcap <- taxstats13.dfi$Taxable_income_s - taxstats13.dfi$conc.cont
sum(taxstats13.dfi$Taxable_income_s_10kcap)*50/10^9 - (sum(taxstats13.dfi$Taxable_income_s)*50/10^9 - sum(taxstats13.dfi$conc.cont)*50/10^9) # Should be zero

# Step 2 is to calculate the income assessable under division 293 - this is now our new taxable income + capped contributions
taxstats13.dfi$div293.incass.10kcap <- ifelse((taxstats13.dfi$Taxable_income_s_10kcap + taxstats13.dfi$conc.cont) > 300000, taxstats13.dfi$Taxable_income_s_10kcap + taxstats13.dfi$conc.cont, 0)
sum(taxstats13.dfi$div293.incass.10kcap)*50/10^9 # This will still be the same as when concessional contributions are uncapped

# Work out the amount to be subject to the extra 15 per cent tax - this is going to be the super contributions of those earning more than 300k up to the 10k cap. Those contributions that now fall outside the cap are treated as taxable income and are subject to the personal MTR
taxstats13.dfi$div293.taxbase.10kcap <- ifelse(taxstats13.dfi$div293.incass.10kcap > 0, apply(cbind(max((taxstats13.dfi$Taxable_income_s_10kcap + taxstats13.dfi$conc.cont - 300000),0),taxstats13.dfi$conc.cont), 1, min),0)
sum(taxstats13.dfi$div293.taxbase.10kcap)*50/10^9 

# We write the Div 293 tax payable by each eligible tax payer to our main data frame
taxstats13.dfi <- taxstats13.dfi %>% mutate(taxtake.div293.10kcap = div293.taxbase.10kcap *0.15) 

taxstats13.dfi %>% summarise(taxtake.div293.10kcap = (sum(taxtake.div293.10kcap)* 50)/10^9)# So with the 10k cap we only raise $0.21 billion from Div 293 tax, rather than $0.86 billion when concessional contributions are untaxed. This makes sense because those contributions beyond the 10k cap are now being treated as taxable income, and taxed at MTRs. Since those on 300k plus are taxed at 47%, this will raise more revene than what is foregone on Div 293 tax. 

# Total tax paid under this option. This is tax levied at marginal personal tax rates for all taxable income, including super contributions that no longer fall below the 10k cap
taxstats13.dfi$tax_payable.option1 <- tax.function(taxstats13.dfi$Taxable_income + taxstats13.dfi$Super.total - taxstats13.dfi$conc.cont,taxstats13.dfi$ML.lower,taxstats13.dfi$ML.upper) + taxstats13.dfi$conc.cont.tax + taxstats13.dfi$taxtake.div293.10kcap
sum(taxstats13.dfi$tax_payable.option1)*50/10^9 # So 194.7 billion paid in income tax, including taxes on super contributions
sum(taxstats13.dfi$conc.cont.tax)*50/10^9
sum(taxstats13.dfi$taxtake.div293.10kcap)*50/10^9

# What is the size of this tax concession (this is kind of like the new super contributions tax expenditure)
sum(taxstats13.dfi$Tax.no.conc - taxstats13.dfi$tax_payable.option1)*50/10^9 # So value of contributions tax concession falls to $11 billion
# Implies a saving of around $4.8 billion (lower if people find a way to lower tax)

# Check - $4.8 billion
sum(taxstats13.dfi$tax_payable.option1 - taxstats13.dfi$Tax.current)*50/10^9

# -------------------------------------------------------------------------------------------------------------------------
# Henry - 15 per cent discount up to the cap (assume current cap stays in place)

# There are two models we want to consider here
# 1. Low income earners recieve negative tax (ie gov super top up)
# 2. Low income earners pay zero tax on contributions

# First - the basic model which does not take account of caps (effectively assuming 50 25 as were in place in 2011-12)
# This will no longer be applicable but will do for a very early first cut.

# Here I am calculate the individuals marginal tax rate based on income inclusive of super - the 15 per cent discount will
# then be applied to any income contributed to super. The discount is effectively a refund after all else is calculated.

# We already have the total tax liability ("taxstats13.dfi$Tax.no.conc")

# Step 1. Is to find the marginal rate for each individual
# I'm going to do this by storing the marginal tax rates, and using the function to cut the taxable income measure into factors
# The marginal tax rates include the impact of the Medicare Levy of 2 percent of incomes above $25000 or so for most taxpayers. As a proxy will just apply them to those on incomes above $37,000

tax.brackets <- c(0,18200, 37000, 80000, 180000)
taxstats13.dfi$marginal.tax.rate <- cut2(taxstats13.dfi$Taxable_income_s, cuts = tax.brackets)
levels(taxstats13.dfi$marginal.tax.rate) <- c(0,0.19,0.345,0.39,0.47) # Applying marginal rates, including the medicare levy
taxstats13.dfi$marginal.tax.rate <- as.numeric(levels(taxstats13.dfi$marginal.tax.rate))[taxstats13.dfi$marginal.tax.rate] # BC: I dont follow syntax here from Cam but it clearly works

# Now we just need to workout the total tax payable 

# Contribution discount
cont.tax.discount <- 0.20

# Option 1 - negative marginal rate on super contribution for low income households (this is the same as provided a super tax credit for low income earners, similar to the LISC)
# Tax on super contribution
taxstats13.dfi$Cont.tax.henry.1 <- (taxstats13.dfi$marginal.tax.rate-cont.tax.discount)*taxstats13.dfi$Super.total
# Checking has positives (tax) and negatives (top ups)
summary(taxstats13.dfi$Cont.tax.henry.1)
hist(taxstats13.dfi$Cont.tax.henry.1)
sum(taxstats13.dfi$Cont.tax.henry.1)*50/10^9 # So Henry proposal raises $16.6 billion in taxes on super contributions, based on concessional caps of 25k and 50k

# Option 2 is the same but with no top ups
taxstats13.dfi$Cont.tax.henry.2 <- ifelse(taxstats13.dfi$Cont.tax.henry.1 < 0,0, taxstats13.dfi$Cont.tax.henry.1)
sum(taxstats13.dfi$Cont.tax.henry.2)*50/10^9 # This raises $16.8 billion in tax on super contributions. So the difference between the Henry proposal with and without the low income super tax credit is only $0.2 billion

# -----------------------------------------------------------------------------------------------------------------------
# Total tax under Henry options

# This shortcut is not ideal - but I think the calculation - a discount from the marginal rate - is as Henry intended. 
# We assume that Div 293 tax is abolished under the Henry option
taxstats13.dfi$Tax.total.henry.1 <- taxstats13.dfi$Tax.no.conc - (taxstats13.dfi$marginal.tax.rate*taxstats13.dfi$Super.total) + taxstats13.dfi$Cont.tax.henry.1 
taxstats13.dfi$Tax.total.henry.2 <- taxstats13.dfi$Tax.no.conc - (taxstats13.dfi$marginal.tax.rate*taxstats13.dfi$Super.total) + taxstats13.dfi$Cont.tax.henry.2

# Neither option saves much - providing top ups to those on low incomes really costs nothing in the scheme of things.
sum(taxstats13.dfi$Tax.total.henry.1 - taxstats13.dfi$Tax.current)*50/10^9 # This raises $4.6 billion with a 15pp discount
sum(taxstats13.dfi$Tax.total.henry.2 - taxstats13.dfi$Tax.current)*50/10^9 # This raises $4.8 billion with a 15 pp discount

# =======================================================================================================================
# Incorporating the 2015-16 concessional caps into the analysis
# https://www.ato.gov.au/Rates/Key-superannuation-rates-and-thresholds/?page=3&page21=#Concessional_contributions_cap

# Will assume that the concessional general cap is $30k and that the over 50 concessional cap is $35k


# -----------------------------------------------------------------------------------------------------------------------
# First step is to see how many people have gone over the concessional caps in the past.

# 2011-12 Anaylsis

# The 2011-12 caps were $25k and $50k
taxstats13.df$Super.SG <- taxstats13.df$Wages*0.09
taxstats13.df$Super.total <- taxstats13.df$Rpt_emp_spr_deductions + taxstats13.df$Super.SG + taxstats13.df$Non_emp_spr_deductions

# Proportion of under 50s contributing more than 50k 
# NOW NEED TO WORK OUT WHY THIS IS NOT WORKING FOR US - BUT WE DONT USE IT IN THE CHAPTER
#under.50.df <- taxstats13.df %>% filter(Over.50 == 0) 
#over.50.df <- taxstats13.df %>% filter(Over.50 == 1)

#hist(under.50.df$Super.total)
#hist(over.50.df$Super.total)

# Proportion of under 50s contributing more than 50k 
#taxstats13.df %>% filter(Over.50 == 0) %>% summarise(prop.over.cap = sum(Super.total > 25000)/length(Super.total))
# One percent of young people
#taxstats13.df %>% filter(Over.50 == 1) %>% summarise(prop.over.cap = sum(Super.total > 50000)/length(Super.total))
# 1.5 per cent of the oldies

# 2015-16 Costing

# Need to look at the number over the 30k and the 35k cap for oldies. 
# The younger people are more interesting here - the over 50 will of course be very wrong

# Proportion of under 50s contributing more than 50k 
under.50.df <- taxstats13.dfi %>% filter(Over.50 == 0) 
over.50.df <- taxstats13.dfi %>% filter(Over.50 == 1)

hist(under.50.df$Super.total)
hist(over.50.df$Super.total)

# Less than 1 per cent are likely to be contributing over the 30k cap now.  
taxstats13.dfi %>% filter(Over.50 == 0) %>% summarise(prop.over.cap = sum(Super.total > 30000)/length(Super.total))
# Even with the policy change - only 1 per cent contribute over $35!
taxstats13.dfi %>% filter(Over.50 == 1) %>% summarise(prop.over.cap = sum(Super.total > 35000)/length(Super.total))

# ==================================================================================================================#
# We use this in Figure 4 in the chapter showing the distribution of (taxable income + super)

# Chart: Income distributions
# 1. Taxable income 
# 2. Taxable income + super contributions (this is figure 4 in the super chapter)

# 1. Create custom breaks - minimum to maximum by $2000

income.breaks <- seq(from = min(taxstats13.dfi$Taxable_income), to = max(taxstats13.dfi$Taxable_income), by = 2000)
x <- table(cut2(taxstats13.dfi$Taxable_income, cuts = income.breaks))
y <- table(cut2(taxstats13.dfi$Taxable_income_s, cuts = income.breaks))

# 2. Write the output to csv files
write.csv(x, "Taxable income.csv")
write.csv(y, "Taxable income and super.csv")

# 3. Multiply each row by 50 to get the full distribution of (income + super) used in Fig 4 in the chapter

# Chart: Contributions by gender ------------------------------------------


age_sex_cont <- taxstats13.dfi %>% group_by(Sex, Age) %>% summarise(med.super.total = median(Super.total), 
                                                                    ave.super.total = mean(Super.total))

#--------------------------------------------------------------------------------------------------------------------
# Incorporating the caps into the analysis (mainly for Henry but also need a baseline for our own analysis)
# Because so people go over the caps I'm going to assume that the maximum amount of super anyone will contribute 
# is set by the cap. I'll complete the analysis above using the capped data frame.

# Final costings - no one contributes more than concessional cap ----------

x <- taxstats13.dfi %>% filter(Over.50 == 0) %>% mutate(Super.capped = ifelse(Super.total > 30000, 30000, Super.total))
y <- taxstats13.dfi %>% filter(Over.50 == 1) %>% mutate(Super.capped = ifelse(Super.total > 35000, 35000, Super.total))
taxstats13.dfic <- rbind(x,y)
rm(x,y)

hist(taxstats13.dfic$Super.capped) # The spikes at 30k and 35k are where super contributions have been capped, simulating the current policy

# Next - we need to establish an estimate of the current system.
# Here I will assume that all super.capped contributions were income tax deductions.

taxstats13.dfic$Taxable_income.caps <- taxstats13.dfic$Taxable_income_s - taxstats13.dfic$Super.capped

# Income tax paid on 2015-16 base case. This doesnt include 15% tax on concessional super contributions or Div 293 tax
taxstats13.dfic$Income.tax.caps <- tax.function(taxstats13.dfic$Taxable_income.caps,taxstats13.dfi$ML.lower,taxstats13.dfi$ML.upper)
sum(taxstats13.dfic$Income.tax.caps)*50/10^9 # $179.8 billion

# We need to work out how much Div 293 tax would be paid now, assuming no one puts more than 30k / 35k into their super. If we dont do this we ignore the fact that some super contributions would be taxed at 30 percent, rather than 15 percent

# Step 1 is to define the income included in the income test for Div 293 tax, now that we have the 30k / 35k concessional contribution caps. 

taxstats13.dfic$Taxable_income_s_30k35kcap <- taxstats13.dfic$Taxable_income.caps + taxstats13.dfic$Super.capped
sum(taxstats13.dfic$Taxable_income_s_30k35kcap)*50/10^9 - (sum(taxstats13.dfic$Taxable_income.caps)*50/10^9 + sum(taxstats13.dfic$Super.capped)*50/10^9) # Should be zero

# Step 2 is to calculate the income assessable under division 293 - this is now our new taxable income + capped contributions
taxstats13.dfic$div293.incass.caps <- ifelse((taxstats13.dfic$Taxable_income_s_30k35kcap + taxstats13.dfic$Super.capped) > 300000, taxstats13.dfic$Taxable_income_s_30k35kcap + taxstats13.dfic$Super.capped, 0)
sum(taxstats13.dfic$div293.incass.10kcap)*50/10^9 # 

# Work out the amount to be subject to the extra 15 per cent tax - this is going to be the super contributions of those earning more than 300k up to the 30k and 35k caps. Like the 10k cap, those contributions that now fall outside the cap are treated as taxable income and are subject to the personal MTR
taxstats13.dfic$div293.taxbase.caps <- ifelse(taxstats13.dfic$div293.incass.caps > 0, 
                                              apply(cbind(max((taxstats13.dfic$Taxable_income_s_30k35kcap + taxstats13.dfic$Super.capped - 300000),0),
                                                          taxstats13.dfic$Super.capped), 1, min),
                                              0)


sum(taxstats13.dfic$div293.taxbase.caps)*50/10^9 # So tax base is $4.7 billion in concessional contributions that will be subject to Div 293 tax

# We write the Div 293 tax paytable by each eligible tax payer to our main data frame
taxstats13.dfic <- taxstats13.dfic %>% mutate(taxtake.div293.caps = div293.taxbase.caps *0.15) 

taxstats13.dfic %>% summarise(taxtake.div293.caps = (sum(taxtake.div293.caps)* 50)/10^9)# So with the 30k/35k caps we only raise $0.71 billion from Div 293 tax, rather than $0.86 billion when concessional contributions are untaxed. 

################################

# Tax on super contributions in the base case - including Div 293 tax
taxstats13.dfic$Super.cont.tax.caps <- taxstats13.dfic$Super.capped*0.15 + taxstats13.dfic$taxtake.div293.caps
sum(taxstats13.dfic$Super.cont.tax.caps)*50/10^9 # So total tax on concessional super contributions of $11.2 billion
sum(taxstats13.dfic$Super.capped*0.15)*50/10^9 # 15 percent tax rate on contributions raises $10.5 billion
sum(taxstats13.dfic$taxtake.div293.caps)*50/10^9 # Div 293 tax raises 0.71 billion

# Total tax 2015-16 base case - which is tax collected on personal taxable income + Tax collected on super contributions up to the 30k and 35k caps
taxstats13.dfic$Total.tax.16 <- taxstats13.dfic$Super.cont.tax.caps + taxstats13.dfic$Income.tax.caps
sum(taxstats13.dfic$Total.tax.16)*50/10^9 # So total tax of $191billion
sum(taxstats13.dfic$Income.tax.caps)*50/10^9 # Personal income tax take of $179.8 billion


# ========================================================================================================
# Here I putting together the final charts needed for the super contributions section

# Chart - concessional contributions by Men and Women over 50+ by type by taxable income --------

# First - what we are reporting here is not the tax base of the estimates but some price adjusted summary stats

Chart1.df <- taxstats13.dfi %>% filter(Over.50 == 1)

# Create taxable income bins for over 50s (so income for over 50s)
Chart1.df$Tax.inc.bin <- cut2(Chart1.df$Taxable_income, g = 10)

Chart1.table <- Chart1.df %>% group_by(Tax.inc.bin) %>% summarise(SG = mean(Super.SG),
                                                                  Employ.vol = mean(Rpt_emp_spr_deductions),
                                                                  Nonemply.vol = mean(Non_emp_spr_deductions))
write.csv(Chart1.table, "chart1.csv")

write.table(Chart1.table , sep = "\t", file = clip <- pipe("pbcopy", "w"))
close(clip)


# Chart2 - Women over 50 making voluntary concessional contributions over 10k and taxable --------
Chart2.df <- Chart1.df
Chart2.table  <- Chart2.df %>% group_by(Sex, Tax.inc.bin) %>% summarise(prop.over.10k = sum(Super.total > 10000)/length(Super.total))

# Chart2.df %>% group_by(Sex) %>% summarise(x = mean(Taxable_income))

write.csv(Chart2.table, "chart2.csv")

# Chart 3 - average tax concession by income - all age groups
# Here the estimates reported are not adjusted for behavioural change - this is done so that the taxable income deciles are consistent.

taxstats13.dfi$Tax.exp.current <- taxstats13.dfi$Tax.no.conc - taxstats13.dfi$Tax.current
taxstats13.dfi$Tax.exp.opt1 <- taxstats13.dfi$Tax.no.conc - taxstats13.dfi$tax_payable.option1

Chart3.df <- taxstats13.dfi

Chart3.df$Tax.inc.bin <- cut2(Chart3.df$Taxable_income, g = 10)

Chart3.table <- Chart3.df %>% group_by(Tax.inc.bin) %>% summarise(Current.exp = mean(Tax.exp.current), Opt1.ex = mean(Tax.exp.opt1))



write.csv(Chart3.table, "chart3.csv")


# For the AFR op-ed on 22 April we also state that 'the top 20 per cent of income earners recieve almost 60 per cent of these [super contributions] concessions. This is calculated using the data from Chart 3 as follows:

AFR.table <- Chart3.df %>% group_by(Tax.inc.bin) %>%  
  summarise(taxpayers = length(ID) * 50, 
            (Current.exp.sum = sum(Tax.exp.current) * 50) / 10^9)  # value of contribution tax concession in billions

AFR.table2 <- Chart3.df %>%  
  summarise(taxpayers = length(ID) * 50, 
            (Current.exp.sum = sum(Tax.exp.current) * 50) / 10^9)

# So we have 12.7 million tax payers, recieving $15.8 billion in contributions tax concessions. (This was $13.97 billion when AFR went to print but the change to the income tax function to include the Medicare levy has increased it)

# Chart 4 - concessional voluntary concessional super contributions by age and income

# Note: we use the uninflated tax stats data frame here as otherwise we'll get strange results of voluntary contributions exceeding the concessional contributions cap for 2012-13

# We cut concessional voluntary super contributions by age and size of voluntary contributions

# Writes our cut points, except the upper bound of 200k (the largest annual concessional voluntary contribution is around $196k - still not sure how this is possible)
vol.df <- seq(from = 0,to = 35000, by = 5000) 


# Generate our data frame for those contributing more than 10k voluntarily each year

# We write a new Age variable that captures all those aged 30 to 50. Can't get the labelling right but we can sort that out in Excel

# Chart4.df.10kplus <- taxstats13.dfi  %>% 
#  mutate(Age.numeric = as.numeric(Age)) %>%
#  mutate(Age.broad = ifelse (Age.numeric < 7 & Age.numeric > 2, "30 to 50", Age))

#Chart4.df.10kplus <- Chart4.df.10kplus  %>%
#mutate(Taxable.income.decile = as.numeric(cut2(Taxable_income_s, g = 10)),
#       Taxable.income.decile.range = cut2(Taxable_income_s, g = 10), 
#       Voluntary.cont = cut2(Super.voluntary, cuts = c(vol.df, 200000)), 
#       Voluntary.cont.range = as.numeric(cut2(Super.voluntary, cuts = c(vol.df, 200000)))) %>%
#  group_by(Age.broad,Taxable.income.decile.range) %>% summarise(percentage = sum(Super.voluntary>10000)/n()*100) 

# # And we chart it
# Chart4.df.10kplus %>% ggplot(aes(x = Age, fill = Taxable.income.decile.range, y = percentage)) + geom_bar(stat = "identity", position = "dodge") + labs(title = "Share of cohort voluntarily contributing more than $10k annually to superannuation", x = "Age") 
# 
# Chart4.df.10kplus %>% ggplot(aes(x = Taxable.income.decile.range, fill = Age, y = percentage)) + geom_bar(stat = "identity", position = "dodge") + labs(title = "Share of cohort voluntarily contributing more than $10k annually to superannuation",                                                                                                                            x = "Taxable Income (including super contributions)") 
# 
# dev.copy2pdf(file = "Chart4.df.10kplus.pdf", width = 15, height = 7) 
# 
# # And produce the summary table for chart in Excel
# 
# Chart4.df.10kplus <- Chart4.df.10kplus %>% spread(Age, percentage)
# 
# # Generate our data frame for those contributing more than 2k voluntarily each year
# Chart4.df.2kplus <- taxstats13.df %>% 
#   mutate(Taxable.income.decile = as.numeric(cut2(Taxable_income_s, g = 10)),
#          Taxable.income.decile.range = cut2(Taxable_income_s, g = 10), 
#          Voluntary.cont = cut2(Super.voluntary, cuts = c(vol.df, 200000)), 
#          Voluntary.cont.range = as.numeric(cut2(Super.voluntary, cuts = c(vol.df, 200000)))) %>%
#   group_by(Age,Taxable.income.decile.range) %>% summarise(percentage = sum(Super.voluntary>2000)/n()*100) 
# 
# # And we chart it
# Chart4.df.2kplus %>% ggplot(aes(x = Age, fill = Taxable.income.decile.range, y = percentage)) + geom_bar(stat = "identity", position = "dodge") + labs(title = "Share of cohort voluntarily contributing more than $2k annually to superannuation",                                                                                                                            x = "Age") 
# 
# Chart4.df.2kplus %>% ggplot(aes(x = Taxable.income.decile.range, fill = Age, y = percentage)) + geom_bar(stat = "identity", position = "dodge") + labs(title = "Share of cohort voluntarily contributing more than $2k annually to superannuation",                                                                                                                            x = "(including super contributions)") 
# 
# 
# dev.copy2pdf(file = "Chart4.df.2kplus.pdf", width = 15, height = 7) 
# 
# # And produce the summary table for chart in Excel
# 
# Chart4.df.2kplus <- Chart4.df.2kplus %>% spread(Age, percentage)

# We also subset the dataframe to just those that are over the age of 50

# Generate our data frame for those contributing more than 10k voluntarily each year
#Chart4.df.10kplus.over50 <- taxstats13.df  %>% filter (Age.numeric < 5) %>%
#  mutate(Taxable.income.decile = as.numeric(cut2(Taxable_income_s, g = 10)),
#         Taxable.income.decile.range = cut2(Taxable_income_s, g = 10), 
#         Voluntary.cont = cut2(Super.voluntary, cuts = c(vol.df, 200000)), 
#         Voluntary.cont.range = as.numeric(cut2(Super.voluntary, cuts = c(vol.df, 200000)))) %>%
#  group_by(Age,Taxable.income.decile.range) %>% summarise(percentage = sum(Super.voluntary>10000)/n()*100) 

# And we chart it
#Chart4.df.10kplus.over50 %>% ggplot(aes(x = Age, fill = Taxable.income.decile.range, y = percentage)) + geom_bar(stat = "identity", position = "dodge") + labs(title = "Share of cohort voluntarily contributing more than $10k annually to superannuation", x = "Age") 

#dev.copy2pdf(file = "Chart4.df.10kplus.pdf", width = 15, height = 7) 

# And produce the summary table for chart in Excel

#Chart4.df.10kplus.over50 <- Chart4.df.10kplus.over50 %>% spread(Age, percentage)

# Generate our data frame for those contributing more than 2k voluntarily each year
#Chart4.df.2kplus.over50 <- taxstats13.df %>% filter (Age.numeric < 5) %>%
#  mutate(Taxable.income.decile = as.numeric(cut2(Taxable_income_s, g = 10)),
#         Taxable.income.decile.range = cut2(Taxable_income_s, g = 10), 
#         Voluntary.cont = cut2(Super.voluntary, cuts = c(vol.df, 200000)), 
#         Voluntary.cont.range = as.numeric(cut2(Super.voluntary, cuts = c(vol.df, 200000)))) %>%
#  group_by(Age,Taxable.income.decile.range) %>% summarise(percentage = sum(Super.voluntary>2000)/n()*100) 

# And we chart it
#Chart4.df.2kplus.over50 %>% ggplot(aes(x = Age, fill = Taxable.income.decile.range, y = percentage)) + geom_bar(stat = "identity", position = "dodge") + labs(title = "Share of cohort voluntarily contributing more than $2k annually to superannuation",                                                                                                                            x = "Age") 

#Chart4.df.2kplus.over50 %>% ggplot(aes(x = Taxable.income.decile.range, fill = Age, y = percentage)) + geom_bar(stat = "identity", position = "dodge") + labs(title = "Share of cohort voluntarily contributing more than $2k annually to superannuation",                                                                                                                            x = "(including super contributions)") 


#dev.copy2pdf(file = "Chart4.df.2kplus.over50.pdf", width = 15, height = 7) 

# And produce the summary table for chart in Excel

#Chart4.df.2kplus.over50 <- Chart4.df.2kplus.over50 %>% spread(Age, percentage)

# Chart 5 - concessional voluntary super contributions by age and gender

# Both genders
Chart5.df <- taxstats13.df  %>%
  group_by(Age) %>% summarise(Voluntary.cont = mean(Super.voluntary))
  
# Male only

Chart5.df.male <- taxstats13.df  %>% filter(Sex == 0) %>%
  group_by(Age) %>% summarise(Voluntary.cont = mean(Super.voluntary))

# Female only

Chart5.df.female <- taxstats13.df  %>% filter(Sex == 1) %>%
  group_by(Age) %>% summarise(Voluntary.cont = mean(Super.voluntary))

# Now we combine the two into a single dataframe for charting to Excel

Chart5.df <- data_frame(Age = Chart5.df.male$Age, 
                        Male = Chart5.df.male$Voluntary.cont, 
                        Female = Chart5.df.female$Voluntary.cont)

# Chart 6 - Histogram of those making concessional voluntary super contributions by size of contribution and income decile

# First we create the voluntary concesisonal contribution buckets
cont.breaks <- seq(from = min(taxstats13.df$Super.voluntary), to = max(taxstats13.df$Super.voluntary), by = 1000)

# We allocate taxpayers into buckets based on size of concessional voluntary contributions and income decile, filtering out zero voluntary contributions


Chart6.df <- taxstats13.df %>% filter(!Super.voluntary == 0, Super.voluntary < 100000) %>%
  mutate(Vol.cont.intervals = cut2(Super.voluntary, cuts = cont.breaks),
         Taxable.income.decile = cut2(Taxable_income_s, cuts = c(0,50000,75000,100000,150000,200000,max(Taxable_income_s)))) %>%
  group_by(Vol.cont.intervals,Taxable.income.decile) %>%
  summarise(Count.thousands = n() * 50 / 1000)

# Chart6.df %>% ggplot(aes(x = Vol.cont.intervals, y = Count.thousands, fill = Taxable.income.decile)) + geom_bar(stat = "identity", position = "stack") + scale_x_discrete(breaks = seq(from = 0, to = 50000, by = 5000), labels = seq(from = 0, to = 50000, by = 5000))

# Chart6.df %>% ggplot(aes(x = Vol.cont.intervals, y = Count.thousands, fill = Age)) + geom_bar(stat = "identity", position = "stack") + scale_x_discrete(breaks = seq(from = 0, to = 50000, by = 5000), labels = seq(from = 0, to = 50000, by = 5000))

# And produce the summary table for chart in Excel

Chart6.df <- Chart6.df %>% spread(Vol.cont.intervals, Count.thousands)

# Chart 7: Histogram of those making voluntary super contributions by size of contribution and age

Chart7.df <- taxstats13.df %>% filter(!Super.voluntary == 0, Super.voluntary < 100000) %>%
  mutate(Vol.cont.intervals = cut2(Super.voluntary, cuts = cont.breaks),
         Taxable.income.decile = cut2(Taxable_income_s, cuts = c(0,50000,75000,100000,150000,200000,max(Taxable_income_s)))) %>%
  group_by(Vol.cont.intervals,Age) %>%
  summarise(Count.thousands = n() * 50 / 1000)

Chart7.df %>% ggplot(aes(x = Vol.cont.intervals, y = Count.thousands, fill = Age)) + geom_bar(stat = "identity", position = "stack") + scale_x_discrete(breaks = seq(from = 0, to = 50000, by = 5000), labels = seq(from = 0, to = 50000, by = 5000))

Chart7.df <- Chart7.df %>% spread(Vol.cont.intervals, Count.thousands)

# Chart 8: share of taxpayers contributing more than 10k in total, and in voluntary contributions


Share.10k.total <- taxstats13.df %>% summarise(share.over.10k = sum(Super.total > 10000)/n()*100)

Share.10k.voluntary <- taxstats13.df %>% summarise(share.over.10k = sum(Super.voluntary > 16000)/n()*100)

# Chart 9: Number of taxpayers making concessional contributions of more than $10k

# Women over 50 making voluntary concessional contributions over 10k and taxable --------
Chart9.df <- Chart3.df 
Chart9.table  <- Chart9.df %>% group_by(Sex, Tax.inc.bin) %>% 
  summarise(prop.over.10k = sum(Super.total > 10000)*50, 
            no.taxpayers = length(Super.total > 10000) * 50)

write.table(Chart9.table , sep = "\t", file = clip <- pipe("pbcopy", "w"))
close(clip)


# Chart 10: Share of voluntary concessional contributions over $10k coming from each age group

Chart10.df <- taxstats13.df  %>%
  group_by(Age) %>% filter(Super.voluntary > 10000) %>%
  summarise(Voluntary.cont = sum(Super.voluntary)*50, 
            No.individuals = length((Super.voluntary)*50))

write.table(Chart10.df , sep = "\t", file = clip <- pipe("pbcopy", "w"))
close(clip)

# Chart 11: Share of voluntary concessional contributions over $0k coming from each age group

Chart11.df <- taxstats13.df  %>%
  group_by(Age) %>% 
  summarise(Voluntary.cont = sum(Super.voluntary)*50, 
            No.individuals = length((Super.voluntary)*50))

write.table(Chart11.df , sep = "\t", file = clip <- pipe("pbcopy", "w"))
close(clip)

# Chart 12: Average concessional contributions by age

Chart12.df <- taxstats13.df %>% 
  group_by(Age.group) %>% 
  summarise(Compulsory.mean = mean(Super.SG),
            Salary.sacrificed.mean = mean(Non_emp_spr_deductions),
            Personal.concessional.mean = mean(Rpt_emp_spr_deductions), 
            Salary.sacrificed.no = length(Non_emp_spr_deductions) * 50, 
            Compulsory.no = length(Super.SG) * 50,
            Personal.concessional.no = length(Rpt_emp_spr_deductions) * 50) 

# Chart 13: Average concessional contributions by age, of only those that are making contributions

# We filter the dataset for each variable separately and then munge then together

Compulsory.df <- taxstats13.df %>% 
  filter(Super.SG > 0) %>%
  group_by(Age.group) %>%
  summarise(Compulsory.mean = mean(Super.SG),
            Compulsory.no = length(Super.SG) * 50)

Personal.df <- taxstats13.df %>% 
  filter(Non_emp_spr_deductions > 0) %>%
  group_by(Age.group) %>%
  summarise(Salary.sacrificed.mean = mean(Non_emp_spr_deductions),
            Salary.sacrificed.no = length(Non_emp_spr_deductions) * 50)

Salary.sacrificed.df <- taxstats13.df %>% 
  filter(Rpt_emp_spr_deductions > 0) %>%
  group_by(Age.group) %>%
  summarise(Personal.mean = mean(Rpt_emp_spr_deductions),
            Personal.no = length(Rpt_emp_spr_deductions) * 50)

Chart13.df <- full_join(Compulsory.df, Personal.df, by = "Age.group")
Chart13.df <- full_join(Chart13.df,Salary.sacrificed.df, by = "Age.group")            

write.table(Chart13.df , sep = "\t", file = clip <- pipe("pbcopy", "w"))
close(clip)

# Chart 14: Average concessional contributions by age, of only those that are making some form of concessional contributions

Chart14.df <- taxstats13.df %>% 
  filter(Super.total.1213 > 0) %>%
  group_by(Age.group) %>%
  summarise(Super.totalmean = mean(Super.total.1213),
            Super.total.no = length(Super.total.1213) * 50)   

write.table(Chart14.df , sep = "\t", file = clip <- pipe("pbcopy", "w"))
close(clip)

sum(taxstats13.df$Super.total.1213) * 50 / 10^9
sum(taxstats13.df$Super.total) * 50 / 10^9

All.taxpayers.df <- taxstats13.df %>% 
  group_by(Age.group) %>%
  summarise(Taxpayers.no = length(Super.total) * 50)  

write.table(All.taxpayers.df , sep = "\t", file = clip <- pipe("pbcopy", "w"))
close(clip)



# Chart 15: Number of ppl making over $10k in concessional  - split be voluntary alone, or including total concessional contributions from each decile and gender

Chart15.df <- Chart3.df %>% group_by(Sex, Tax.inc.bin) %>% 
  summarise(voluntary.over.10k = sum(Super.voluntary > 10000)*50,
            no.taxpayers = length(Super.voluntary > 10000) * 50)

write.table(Chart15.df, sep = "\t", file = clip <- pipe("pbcopy", "w"))
close(clip)

# Chart 16: Number of ppl making over $10k in voluntary concessional contributions from each decile and gender

Chart16.df <- Chart3.df %>% group_by(Sex, Tax.inc.bin) %>% 
  summarise(voluntary.over.10k = sum(Super.voluntary > 10000)*50,
            total.over.10k = sum(Super.total > 10000)*50,
            additional.total = total.over.10k - voluntary.over.10k,
            no.taxpayers = length(Super.voluntary > 10000) * 50)

write.table(Chart16.df, sep = "\t", file = clip <- pipe("pbcopy", "w"))
close(clip)

# Chart 17: Average super contributions by gender and age actual 2012-13 figures (not inflated)

Chart17.df <- taxstats13.df %>% 
  group_by(Age.group, Sex) %>% 
  summarise(SG = mean(Super.SG.1213),
            Employ.vol = mean(Rpt_emp_spr_deductions),
            Nonemply.vol = mean(Non_emp_spr_deductions), 
            Mean.taxable.income = mean(Taxable_income_s))

Chart17.df.men <- taxstats13.df %>% filter(Sex == 0) %>%
  group_by(Age.group, Sex) %>% 
  summarise(SG = mean(Super.SG.1213),
            Employ.vol = mean(Rpt_emp_spr_deductions),
            Nonemply.vol = mean(Non_emp_spr_deductions), 
            Mean.taxable.income = mean(Taxable_income_s))

write.table(Chart17.df.men, sep = "\t", file = clip <- pipe("pbcopy", "w"))
close(clip)

Chart17.df.women <- taxstats13.df %>% filter(Sex == 1) %>% 
  filter(!is.na(Taxable_income)) %>%
  filter(!is.na(Super.SG.1213)) %>%
  group_by(Age.group, Sex) %>% 
  summarise(SG = mean(Super.SG.1213),
            Employ.vol = mean(Rpt_emp_spr_deductions),
            Nonemply.vol = mean(Non_emp_spr_deductions), 
            Mean.taxable.income = mean(Taxable_income_s), 
            Mean.prop.cont = weighted.mean(Super.total.1213 / Taxable_income))

write.table(Chart17.df.women, sep = "\t", file = clip <- pipe("pbcopy", "w"))
close(clip)


# Chart 18: Number of ppl making over concessional contributions of more than a certain amount, by gender 

taxstats13.df$Pretax.cont.range <- ifelse(taxstats13.df$Super.total < 5000, "Less than 5,000",
                                            ifelse(taxstats13.df$Super.total < 10000, "Less than 10,000",
                                                   ifelse(taxstats13.df$Super.total < 15000, "Less than 15,000",
                                                          ifelse(taxstats13.df$Super.total < 20000, "Less than 20,000",
                                                                 ifelse(taxstats13.df$Super.total < 25000, "Less than 25,000",
                                                                        ifelse(taxstats13.df$Super.total < 30000, "Less than 30,000", "More than 30,000"))))))


taxstats13.dfi$Pretax.cont.range <- ifelse(taxstats13.dfi$Super.total < 5000, "Less than 5,000",
                                          ifelse(taxstats13.dfi$Super.total < 10000, "Less than 10,000",
                                                 ifelse(taxstats13.dfi$Super.total < 15000, "Less than 15,000",
                                                        ifelse(taxstats13.dfi$Super.total < 20000, "Less than 20,000",
                                                               ifelse(taxstats13.dfi$Super.total < 25000, "Less than 25,000",
                                                                      ifelse(taxstats13.dfi$Super.total < 30000, "Less than 30,000", "More than 30,000"))))))

Chart18.dfi <- taxstats13.dfi %>% 
  group_by(Sex,Pretax.cont.range) %>% 
  summarise(no.taxpayers = length(Super.total) * 50)

View(Chart18.dfi)

write.table(Chart18.dfi, sep = "\t", file = clip <- pipe("pbcopy", "w"))
close(clip)

Chart19.df <- taxstats13.df %>% filter(Super.voluntary > 15000) %>%
  group_by(Sex) %>% 
  summarise(no.taxpayers = length(Super.voluntary) * 50)


# Stats in super report

# Number of taxpayers that would be affected by the $11k concessional contribution

ChartX.df <- Chart3.df 
ChartX.table  <- ChartX.df %>% group_by(Sex, Tax.inc.bin) %>% 
  summarise(prop.over.11k = sum(Super.total > 11000)*50, 
            no.taxpayers = length(Super.total > 11000) * 50)

write.table(ChartX.table , sep = "\t", file = clip <- pipe("pbcopy", "w"))
close(clip)

# Number of taxpayers that would be affected by lowering the thresold for Div 293 tax to $115,000

ChartY.df <- Chart3.df 
ChartY.table  <- ChartY.df %>% filter (Super.total > 0) %>% 
  group_by(Tax.inc.bin) %>% 
  summarise(prop.over.115k = sum(Taxable_income_s > 115000)*50, 
            no.taxpayers = length(Taxable_income_s) * 50)

write.table(ChartY.table , sep = "\t", file = clip <- pipe("pbcopy", "w"))
close(clip)
            

# =======================================================================================================================
# Simulating ALP policy proposal on taxing super contributions

# The ALP has released a proposal to reduce the tax concession on super contributions for those earnings more than $250,000. The announcement is available here: http://www.chrisbowen.net/media-centre/allNews.do?newsId=6978

# The ALP proposes reduce the tax concession on super contributions for those earnings more than $250,000 by lowering the threshold at which Div 293 tax kicks in to $250,000, from incomes of $300,000 at present

# First we're going to simulate the ALP's proposal, and then we're going to run some different scenarios with lower thresholds, and see how these scenarios compare to our own proposal to simply cap concessional contributions at $10k for everyone (and some other permutations thereof)

# Since we're operating in a world where the 30k and 35k concessional caps exist, will use the .dfic dataframe for our analysis

#---------

taxstats13.dfic$Taxable_income.caps <- taxstats13.dfic$Taxable_income_s - taxstats13.dfic$Super.capped

# Income tax paid on 2015-16 base case. This doesnt include Div 293 tax
taxstats13.dfic$Income.tax.caps <- tax.function(taxstats13.dfic$Taxable_income.caps, taxstats13.dfi$ML.lower,taxstats13.dfi$ML.upper)

# We need to work out how much Div 293 tax would be paid now, assuming no one puts more than 30k / 35k into their super. If we dont do this we ignore the fact that some super contributions would be taxed at 30 percent, rather than 15 percent

# Step 1 is to define the income included in the income test for Div 293 tax, now that we have the 30k / 35k concessional contribution caps. 

taxstats13.dfic$Taxable_income_s_30k35kcap <- taxstats13.dfic$Taxable_income.caps + taxstats13.dfic$Super.capped
sum(taxstats13.dfic$Taxable_income_s_30k35kcap)*50/10^9 - (sum(taxstats13.dfic$Taxable_income.caps)*50/10^9 + sum(taxstats13.dfic$Super.capped)*50/10^9) # Should be zero

# Step 2 is to calculate the income assessable under division 293 - this is now our new taxable income + capped contributions
taxstats13.dfic$div293.incass.caps <- ifelse((taxstats13.dfic$Taxable_income_s_30k35kcap + taxstats13.dfic$Super.capped) > 300000, taxstats13.dfic$Taxable_income_s_30k35kcap + taxstats13.dfic$Super.capped, 0)
sum(taxstats13.dfic$div293.incass.10kcap)*50/10^9 # 

# Work out the amount to be subject to the extra 15 per cent tax - this is going to be the super contributions of those earning more than 300k up to the 30k and 35k caps. Like the 10k cap, those contributions that now fall outside the cap are treated as taxable income and are subject to the personal MTR
taxstats13.dfic$div293.taxbase.caps <- ifelse(taxstats13.dfic$div293.incass.caps > 0, apply(cbind(max((taxstats13.dfic$Taxable_income_s_30k35kcap + taxstats13.dfic$Super.capped - 300000),0),taxstats13.dfic$Super.capped), 1, min),0)
sum(taxstats13.dfic$div293.taxbase.caps)*50/10^9 # So tax base is $4.2 billion in concessional contributions that will be subject to Div 293 tax

# We write the Div 293 tax paytable by each eligible tax payer to our main data frame
taxstats13.dfic <- taxstats13.dfic %>% mutate(taxtake.div293.caps = div293.taxbase.caps *0.15) 

taxstats13.dfic %>% summarise(taxtake.div293.caps = (sum(taxtake.div293.caps)* 50)/10^9)# So with the 30k/35k caps we only raise $0.63 billion from Div 293 tax, rather than $0.86 billion when concessional contributions are uncapped

################################

# Tax on super contributions in the base case - including Div 293 tax
taxstats13.dfic$Super.cont.tax.caps <- taxstats13.dfic$Super.capped*0.15 + taxstats13.dfic$taxtake.div293.caps
sum(taxstats13.dfic$Super.cont.tax.caps)*50/10^9 # So total tax on concessional super contributions of $11 billion
sum(taxstats13.dfic$Super.capped*0.15)*50/10^9 # 15 percent tax rate on contributions raises $10.4 billion
sum(taxstats13.dfic$taxtake.div293.caps)*50/10^9 # Div 293 tax raises 0.63 billion

# Total tax 2015-16 base case - which is tax collected on personal taxable income + Tax collected on super contributions up to the 30k and 35k caps
taxstats13.dfic$Total.tax.16 <- taxstats13.dfic$Super.cont.tax.caps + taxstats13.dfic$Income.tax.caps
sum(taxstats13.dfic$Total.tax.16)*50/10^9 # So total tax of $197.2 billion


#-------

# Step 1 is to establish the income assessable under the ALP policy (250k), as well as at thresholds of $200k, $150k, $125k, $115k and $100k

# Current policy with Div 293 tax - to establish a baseline with a contributions cap where concessional contributions are capped at 35k (over 50s) / 30k (under 50s) respectively.

# Step 2 is to calculate the income assessable under division 293 - this is now our new taxable income + capped contributions

taxstats13.dfic$Div293.incass.caps <- ifelse((taxstats13.dfic$Taxable_income_s_30k35kcap + taxstats13.dfic$Super.capped) > 300000, taxstats13.dfic$Taxable_income_s_30k35kcap + taxstats13.dfic$Super.capped, 0)
sum(taxstats13.dfic$Div293.incass.caps)*50/10^9

# ALP Proposal - $250k threshold
taxstats13.dfic$thold.250k.incass.caps <- ifelse((taxstats13.dfic$Taxable_income_s_30k35kcap + taxstats13.dfic$Super.capped) > 250000, taxstats13.dfic$Taxable_income_s_30k35kcap + taxstats13.dfic$Super.capped, 0)
sum(taxstats13.dfic$thold.250k.incass.caps)*50/10^9

# 1st alternative threshold, of $200k threshold
taxstats13.dfic$thold.200k.incass.caps <- ifelse((taxstats13.dfic$Taxable_income_s_30k35kcap + taxstats13.dfic$Super.capped) > 200000, taxstats13.dfic$Taxable_income_s_30k35kcap + taxstats13.dfic$Super.capped, 0)
sum(taxstats13.dfic$thold.200k.incass.caps)*50/10^9

# 2st alternative threshold, of $150k threshold
taxstats13.dfic$thold.150k.incass.caps <- ifelse((taxstats13.dfic$Taxable_income_s_30k35kcap + taxstats13.dfic$Super.capped) > 150000, taxstats13.dfic$Taxable_income_s_30k35kcap + taxstats13.dfic$Super.capped, 0)
sum(taxstats13.dfic$thold.150k.incass.caps)*50/10^9

# 3rd alternative threshold, of $125k threshold, which we'll call thold.125k
taxstats13.dfic$thold.125k.incass.caps <- ifelse((taxstats13.dfic$Taxable_income_s_30k35kcap + taxstats13.dfic$Super.capped) > 125000, taxstats13.dfic$Taxable_income_s_30k35kcap + taxstats13.dfic$Super.capped, 0)
sum(taxstats13.dfic$thold.125k.incass.caps)*50/10^9


# 4th alternative threshold, of $115k threshold, which we'll call thold.115k
taxstats13.dfic$thold.115k.incass.caps <- ifelse((taxstats13.dfic$Taxable_income_s_30k35kcap + taxstats13.dfic$Super.capped) > 115000, taxstats13.dfic$Taxable_income_s_30k35kcap + taxstats13.dfic$Super.capped, 0)
sum(taxstats13.dfic$thold.115k.incass.caps)*50/10^9

# 5th alternative threshold, of $100k threshold, which we'll call thold.100k
taxstats13.dfic$thold.100k.incass.caps <- ifelse((taxstats13.dfic$Taxable_income_s_30k35kcap + taxstats13.dfic$Super.capped) > 100000, taxstats13.dfic$Taxable_income_s_30k35kcap + taxstats13.dfic$Super.capped, 0)
sum(taxstats13.dfic$thold.100k.incass.caps)*50/10^9

# So the size of the assessable tax base for the extra 15% contributions tax rises as the threshold falls - as we'd expect 

# Now we work out the amount to be subject to the extra 15 per cent tax at each threshold

# Current policy - 300k threshold
taxstats13.dfic$Div293.taxbase <- ifelse(taxstats13.dfic$Div293.incass.caps > 0, apply(cbind(max((taxstats13.dfic$Taxable_income_s_30k35kcap + taxstats13.dfic$Super.capped - 300000),0),taxstats13.dfic$Super.capped), 1, min),0)
sum(taxstats13.dfic$Div293.taxbase)*50/10^9
table(taxstats13.dfic$div293.taxbase > 0)/length(taxstats13.dfic$div293.taxbase) # 1 percent affected
Div293.taxtake <- taxstats13.dfic %>% filter(div293.taxbase > 0) %>% summarise(taxtake.div293 = (sum(div293.taxbase) *0.15 * 50)/10^9) # Raises $720 million 
hist(taxstats13.dfic[taxstats13.dfic$div293.taxbase > 0,]$div293.taxbase)

# ALP policy - 250k threshold
taxstats13.dfic$thold.250k.taxbase <- ifelse(taxstats13.dfic$thold.250k.incass.caps > 0, apply(cbind(max((taxstats13.dfic$Taxable_income_s_30k35kcap + taxstats13.dfic$Super.capped - 250000),0),taxstats13.dfic$Super.capped), 1, min),0)
sum(taxstats13.dfic$thold.250k.taxbase)*50/10^9
table(taxstats13.dfic$thold.250k.taxbase > 0)/length(taxstats13.dfic$thold.250k.taxbase) # 2 percent affected
thold.250k.taxtake <- taxstats13.dfic %>% filter(thold.250k.taxbase > 0) %>% summarise(taxtake.thold.250k = (sum(thold.250k.taxbase) *0.15 * 50)/10^9) # Raises $0.95 billion
hist(taxstats13.dfic[taxstats13.dfic$thold.250k.taxbase > 0,]$thold.250k.taxbase)

# Alternative 1 - 200k threshold
taxstats13.dfic$thold.200k.taxbase <- ifelse(taxstats13.dfic$thold.200k.incass.caps > 0, apply(cbind(max((taxstats13.dfic$Taxable_income_s_30k35kcap + taxstats13.dfic$Super.capped - 200000),0),taxstats13.dfic$Super.capped), 1, min),0)
sum(taxstats13.dfic$thold.200k.taxbase)*50/10^9
table(taxstats13.dfic$thold.200k.taxbase > 0)/length(taxstats13.dfic$thold.200k.taxbase) # 3.7 percent affected
thold.200k.taxtake <- taxstats13.dfic %>% filter(thold.200k.taxbase > 0) %>% summarise(taxtake.thold.200k = (sum(thold.200k.taxbase) *0.15 * 50)/10^9) # Raises $1.56 billion
hist(taxstats13.dfic[taxstats13.dfic$thold.200k.taxbase > 0,]$thold.200k.taxbase)

# Alternative 2 - 150k threshold
taxstats13.dfic$thold.150k.taxbase <- ifelse(taxstats13.dfic$thold.150k.incass.caps > 0, apply(cbind(max((taxstats13.dfic$Taxable_income_s_30k35kcap + taxstats13.dfic$Super.capped - 150000),0),taxstats13.dfic$Super.capped), 1, min),0)
sum(taxstats13.dfic$thold.150k.taxbase)*50/10^9
table(taxstats13.dfic$thold.150k.taxbase > 0)/length(taxstats13.dfic$thold.150k.taxbase) # 7.7 percent affected
thold.150k.taxtake <- taxstats13.dfic %>% filter(thold.150k.taxbase > 0) %>% summarise(taxtake.thold.150k = (sum(thold.150k.taxbase) *0.15 * 50)/10^9) # Raises $2.8 billion
hist(taxstats13.dfic[taxstats13.dfic$thold.150k.taxbase > 0,]$thold.150k.taxbase)

# Alternative 3 - 125k threshold
taxstats13.dfic$thold.125k.taxbase <- ifelse(taxstats13.dfic$thold.125k.incass.caps > 0, apply(cbind(max((taxstats13.dfic$Taxable_income_s_30k35kcap + taxstats13.dfic$Super.capped - 125000),0),taxstats13.dfic$Super.capped), 1, min),0)
sum(taxstats13.dfic$thold.125k.taxbase)*50/10^9
table(taxstats13.dfic$thold.125k.taxbase > 0)/length(taxstats13.dfic$thold.125k.taxbase) # 12 percent affected
thold.125k.taxtake <- taxstats13.dfic %>% filter(thold.125k.taxbase > 0) %>% summarise(taxtake.thold.125k = (sum(thold.125k.taxbase) *0.15 * 50)/10^9) # Raises $3.9 billion
hist(taxstats13.dfic[taxstats13.dfic$thold.125k.taxbase > 0,]$thold.125k.taxbase)

# Alternative 4 - 115k threshold
taxstats13.dfic$thold.115k.taxbase <- ifelse(taxstats13.dfic$thold.115k.incass.caps > 0, apply(cbind(max((taxstats13.dfic$Taxable_income_s_30k35kcap + taxstats13.dfic$Super.capped - 115000),0),taxstats13.dfic$Super.capped), 1, min),0)
sum(taxstats13.dfic$thold.115k.taxbase)*50/10^9
table(taxstats13.dfic$thold.115k.taxbase > 0)/length(taxstats13.dfic$thold.115k.taxbase) # 12 percent affected
thold.115k.taxtake <- taxstats13.dfic %>% filter(thold.115k.taxbase > 0) %>% summarise(taxtake.thold.115k = (sum(thold.115k.taxbase) *0.15 * 50)/10^9) # Raises $4.5 billion
hist(taxstats13.dfic[taxstats13.dfic$thold.115k.taxbase > 0,]$thold.115k.taxbase)

# Alternative 5 - 100k threshold
taxstats13.dfic$thold.100k.taxbase <- ifelse(taxstats13.dfic$thold.100k.incass.caps > 0, apply(cbind(max((taxstats13.dfic$Taxable_income_s_30k35kcap + taxstats13.dfic$Super.capped - 100000),0),taxstats13.dfic$Super.capped), 1, min),0)
sum(taxstats13.dfic$thold.100k.taxbase)*50/10^9
table(taxstats13.dfic$thold.100k.taxbase > 0)/length(taxstats13.dfic$thold.100k.taxbase) # 19.5 percent of taxpayers affected
thold.100k.taxtake <- taxstats13.dfic %>% filter(thold.100k.taxbase > 0) %>% summarise(taxtake.thold.100k = (sum(thold.100k.taxbase) *0.15 * 50)/10^9) # Raises $5.4 billion
hist(taxstats13.dfic[taxstats13.dfic$thold.100k.taxbase > 0,]$thold.100k.taxbase)

# Now we pull all these options together into a couple of summary tables and charts: (1) Showing revenue gain under each proposal compared to current policy today

# We're doing this compared to current policy, so Div 293 option is our baseline case. We compare the revenue raised by the other options against current policy 

Thold.revenue <- taxstats13.dfic %>%  
  summarise(thold.100k = (sum(thold.100k.taxbase) - sum(div293.taxbase)) * 0.15 * 50/10^9,
            thold.115k = (sum(thold.115k.taxbase) - sum(div293.taxbase)) * 0.15 * 50/10^9,
            thold.125k = (sum(thold.125k.taxbase) - sum(div293.taxbase)) * 0.15 * 50/10^9,
            thold.150k = (sum(thold.150k.taxbase) - sum(div293.taxbase)) * 0.15 * 50/10^9, 
            thold.200k = (sum(thold.200k.taxbase) - sum(div293.taxbase)) * 0.15 * 50/10^9, 
            thold.250k = (sum(thold.250k.taxbase) - sum(div293.taxbase)) * 0.15 * 50/10^9)

            
# Charting extra revenue raised from dropping thresholds vs. current policy

#Thold.revenue %>% melt() %>% ggplot(aes(x = variable, y = value)) +
#  geom_bar(stat = "identity") + 
#  labs(title = "Additional annual tax revenue from lowering thold on 30% cont tax ($ billion)", 
#       x = "Income threshold for 30 percent contributions tax", y = "Total revenue ($ billion)") + theme_hugh() 

# Generates pdf of chart for printing
# dev.copy2pdf(file = "Thold.revenue.pdf", width = 15, height = 7)  

# =======================================================================================================================
# Simulating 10k, 11k, 15k, 20k, and 25k contributions caps, compared to the current arrangements

# We also want to see how much we would raise from reducing the caps on concessional contributions to between 10k and 25k. At present the caps are 30k for under 50s and 35k for over 50s. Like our Grattan proposal above, we'll just apply the same cap to everyone regardless of age

# We also keep the Division 293 threshold at 300k as per current policy.

# So we use the same baseline from earlier, using the existing concessional contribution caps of 30k (under 50s) and 35k (over 50s)
# As before, we again we will assume that all super.capped contributions were income tax deductions.

taxstats13.dfic$Taxable_income.caps <- taxstats13.dfic$Taxable_income_s - taxstats13.dfic$Super.capped

# Income tax paid on 2015-16 base case
taxstats13.dfic$Income.tax.caps <- tax.function(taxstats13.dfic$Taxable_income.caps, taxstats13.dfi$ML.lower,taxstats13.dfi$ML.upper)

# Tax on super contributions in the base case
taxstats13.dfic$Super.cont.tax.caps <- taxstats13.dfic$Super.capped*0.15

# Total tax 2015-16 base case - which is tax collected on personal taxable income + Tax collected on super contributions up to the 30k and 35k caps
taxstats13.dfic$Total.tax.16 <- taxstats13.dfic$Super.cont.tax.caps + taxstats13.dfic$Income.tax.caps

# Now we establish our alternative concessional caps of 11k, 15k, 20k, 25k and 30k. Recall that we already have 10k at the Grattan proposal from earlier. We'll do these one at a time, starting with the 15k cap

# ========================================================================================================================

# 10k cap scenario

# Set the new contribution cap
cap.10k <- 10000

# Create a variable for the concessional contribution. We assume no one contributes more than 10k under the SG
taxstats13.dfi$conc.cont.cap.10k <- ifelse(taxstats13.dfi$Super.total > cap.10k, cap.10k, taxstats13.dfi$Super.total)
# Tax paid on super contributions
taxstats13.dfi$conc.cont.tax.cap.10k  <-  taxstats13.dfi$conc.cont.cap.10k*0.15
sum(taxstats13.dfi$conc.cont.tax.cap.10k)*50/10^9 # So $8 billion paid in taxes on concessional contributions with a 10k cap

# We also have to work out how much Div 293 tax should be collected under the 10k cap. Will be more than under a 10k cap as more concessional contributions (but with less tax on taxable income at full MTR)

# Step 1 is to shift any income that was previously contributed to super above the 10k cap back into taxable income so it can be taxed at the full marginal rate. This includes both SG and voluntary contributions

taxstats13.dfi$Taxable_income_s_10kcap <- taxstats13.dfi$Taxable_income_s - taxstats13.dfi$conc.cont.cap.10k
sum(taxstats13.dfi$Taxable_income_s_10kcap)*50/10^9 - (sum(taxstats13.dfi$Taxable_income_s)*50/10^9 - sum(taxstats13.dfi$conc.cont.cap.10k)*50/10^9) # Should be zero

# Step 2 is to calculate the income assessable under division 293 - this is now our new taxable income + capped contributions
taxstats13.dfi$div293.incass.10kcap <- ifelse((taxstats13.dfi$Taxable_income_s_10kcap + taxstats13.dfi$conc.cont.cap.10k) > 300000, taxstats13.dfi$Taxable_income_s_10kcap + taxstats13.dfi$conc.cont.cap.10k, 0)
sum(taxstats13.dfi$div293.incass.10kcap)*50/10^9 # This will still be the same as when concessional contributions are uncapped (i.e. $87.4 billion)

# Work out the amount to be subject to the extra 15 per cent tax - this is going to be the super contributions of those earning more than 300k up to the 10k cap. Those contributions that now fall outside the cap are treated as taxable income and are subject to the personal MTR
taxstats13.dfi$div293.taxbase.10kcap <- ifelse(taxstats13.dfi$div293.incass.10kcap > 0, apply(cbind(max((taxstats13.dfi$Taxable_income_s_10kcap + taxstats13.dfi$conc.cont.cap.10k - 300000),0),taxstats13.dfi$conc.cont.cap.10k), 1, min),0)
sum(taxstats13.dfi$div293.taxbase.10kcap)*50/10^9 # $1.4 billion

# We write the Div 293 tax payable by each eligible tax payer to our main data frame
taxstats13.dfi <- taxstats13.dfi %>% mutate(taxtake.div293.10kcap = div293.taxbase.10kcap *0.15) 

taxstats13.dfi %>% summarise(taxtake.div293.10kcap = (sum(taxtake.div293.10kcap)* 50)/10^9)# So with the 10k cap we raise $0.21 billion from Div 293 tax, rather than $0.86 billion when concessional contributions are uncapped

# Total tax paid under this option. This is tax levied at marginal personal tax rates for all taxable income, including super contributions that no longer fall below the 10k cap
taxstats13.dfi$tax_payable.cap.10k <- tax.function(taxstats13.dfi$Taxable_income + taxstats13.dfi$Super.total - taxstats13.dfi$conc.cont.cap.10k,taxstats13.dfi$ML.lower,taxstats13.dfi$ML.upper) + taxstats13.dfi$conc.cont.tax.cap.10k + taxstats13.dfi$taxtake.div293.10kcap
sum(taxstats13.dfi$tax_payable.cap.10k )*50/10^9 # So 192 billion paid in income tax, including taxes on super contributions
sum(taxstats13.dfi$conc.cont.tax.cap.10k)*50/10^9 # $ 8.1 billion tax on concessional super contributions
sum(taxstats13.dfi$taxtake.div293.10kcap)*50/10^9 # Raises $0.21 billion in Div 293 tax

# Final costing for 10k cap
#---------------------------
# We compare the revenue raised with a 10k concessional contributions cap against the world where the 30k and 35k concessional caps at in place

sum(taxstats13.dfi$tax_payable.cap.10k - taxstats13.dfic$Total.tax.16)*50/10^9
# Conclusion - it raises $3.9 billion compared to current policy settings

# ========================================================================================================================


# 11k cap scenario

# Set the new contribution cap
cap.11k <- 11000

# Create a variable for the concessional contribution. We assume no one contributes more than 10k under the SG
taxstats13.dfi$conc.cont.cap.11k <- ifelse(taxstats13.dfi$Super.total > cap.11k, cap.11k, taxstats13.dfi$Super.total)
# Tax paid on super contributions
taxstats13.dfi$conc.cont.tax.cap.11k  <-  taxstats13.dfi$conc.cont.cap.11k*0.15
sum(taxstats13.dfi$conc.cont.tax.cap.11k)*50/10^9 # So $8.5 billion paid in taxes on concessional contributions with a 11k cap

# We also have to work out how much Div 293 tax should be collected under the 11k cap. Will be more than under a 10k cap as more concessional contributions (but with less tax on taxable income at full MTR)

# Step 1 is to shift any income that was previously contributed to super above the 10k cap back into taxable income so it can be taxed at the full marginal rate. This includes both SG and voluntary contributions

taxstats13.dfi$Taxable_income_s_11kcap <- taxstats13.dfi$Taxable_income_s - taxstats13.dfi$conc.cont.cap.11k
sum(taxstats13.dfi$Taxable_income_s_11kcap)*50/10^9 - (sum(taxstats13.dfi$Taxable_income_s)*50/10^9 - sum(taxstats13.dfi$conc.cont.cap.11k)*50/10^9) # Should be zero

# Step 2 is to calculate the income assessable under division 293 - this is now our new taxable income + capped contributions
taxstats13.dfi$div293.incass.11kcap <- ifelse((taxstats13.dfi$Taxable_income_s_11kcap + taxstats13.dfi$conc.cont.cap.11k) > 300000, taxstats13.dfi$Taxable_income_s_11kcap + taxstats13.dfi$conc.cont.cap.11k, 0)
sum(taxstats13.dfi$div293.incass.11kcap)*50/10^9 # This will still be the same as when concessional contributions are uncapped (i.e. $85.5 billion)

# Work out the amount to be subject to the extra 15 per cent tax - this is going to be the super contributions of those earning more than 300k up to the 11k cap. Those contributions that now fall outside the cap are treated as taxable income and are subject to the personal MTR
taxstats13.dfi$div293.taxbase.11kcap <- ifelse(taxstats13.dfi$div293.incass.11kcap > 0, apply(cbind(max((taxstats13.dfi$Taxable_income_s_11kcap + taxstats13.dfi$conc.cont.cap.11k - 300000),0),taxstats13.dfi$conc.cont.cap.11k), 1, min),0)
sum(taxstats13.dfi$div293.taxbase.11kcap)*50/10^9 # $1.4 billion

# We write the Div 293 tax payable by each eligible tax payer to our main data frame
taxstats13.dfi <- taxstats13.dfi %>% mutate(taxtake.div293.11kcap = div293.taxbase.11kcap *0.15) 

taxstats13.dfi %>% summarise(taxtake.div293.11kcap = (sum(taxtake.div293.11kcap)* 50)/10^9)# So with the 11k cap we raise $0.21 billion from Div 293 tax, rather than $0.21 billion with a 10k cap, or $0.86 billion when concessional contributions are uncapped

# Total tax paid under this option. This is tax levied at marginal personal tax rates for all taxable income, including super contributions that no longer fall below the 11k cap
taxstats13.dfi$tax_payable.cap.11k <- tax.function(taxstats13.dfi$Taxable_income + taxstats13.dfi$Super.total - taxstats13.dfi$conc.cont.cap.11k,taxstats13.dfi$ML.lower,taxstats13.dfi$ML.upper) + taxstats13.dfi$conc.cont.tax.cap.11k + taxstats13.dfi$taxtake.div293.11kcap
sum(taxstats13.dfi$tax_payable.cap.11k )*50/10^9 # So 200 billion paid in income tax, including taxes on super contributions
sum(taxstats13.dfi$conc.cont.tax.cap.11k)*50/10^9 # $ 8.5 billion tax on concessional super contributions
sum(taxstats13.dfi$taxtake.div293.11kcap)*50/10^9 # Raises $0.21 billion in Div 293 tax

# Final costing for 11k cap
#---------------------------
# We compare the revenue raised with a 11k concessional contributions cap against the world where the 30k and 35k concessional caps at in place

sum(taxstats13.dfi$tax_payable.cap.11k - taxstats13.dfic$Total.tax.16)*50/10^9
# Conclusion - it raises $3.5 billion compared to current policy settings

# ========================================================================================================================

# 15k cap scenario

# Set the new contribution cap
cap.15k <- 15000

# Create a variable for the concessional contribution. We assume no one contributes more than 10k under the SG
taxstats13.dfi$conc.cont.cap.15k <- ifelse(taxstats13.dfi$Super.total > cap.15k, cap.15k, taxstats13.dfi$Super.total)
# Tax paid on super contributions
taxstats13.dfi$conc.cont.tax.cap.15k  <-  taxstats13.dfi$conc.cont.cap.15k*0.15
sum(taxstats13.dfi$conc.cont.tax.cap.15k)*50/10^9 # So $9 billion paid in taxes on concessional contributions with a 15k cap

# We also have to work out how much Div 293 tax should be collected under the 15k cap. Will be more than under a 10k cap as more concessional contributions (but with less tax on taxable income at full MTR)

# Step 1 is to shift any income that was previously contributed to super above the 10k cap back into taxable income so it can be taxed at the full marginal rate. This includes both SG and voluntary contributions

taxstats13.dfi$Taxable_income_s_15kcap <- taxstats13.dfi$Taxable_income_s - taxstats13.dfi$conc.cont.cap.15k
sum(taxstats13.dfi$Taxable_income_s_15kcap)*50/10^9 - (sum(taxstats13.dfi$Taxable_income_s)*50/10^9 - sum(taxstats13.dfi$conc.cont.cap.15k)*50/10^9) # Should be zero

# Step 2 is to calculate the income assessable under division 293 - this is now our new taxable income + capped contributions
taxstats13.dfi$div293.incass.15kcap <- ifelse((taxstats13.dfi$Taxable_income_s_15kcap + taxstats13.dfi$conc.cont.cap.15k) > 300000, taxstats13.dfi$Taxable_income_s_15kcap + taxstats13.dfi$conc.cont.cap.15k, 0)
sum(taxstats13.dfi$div293.incass.15kcap)*50/10^9 # This will still be the same as when concessional contributions are uncapped (i.e. $87.4 billion)

# Work out the amount to be subject to the extra 15 per cent tax - this is going to be the super contributions of those earning more than 300k up to the 15k cap. Those contributions that now fall outside the cap are treated as taxable income and are subject to the personal MTR
taxstats13.dfi$div293.taxbase.15kcap <- ifelse(taxstats13.dfi$div293.incass.15kcap > 0, apply(cbind(max((taxstats13.dfi$Taxable_income_s_15kcap + taxstats13.dfi$conc.cont.cap.15k - 300000),0),taxstats13.dfi$conc.cont.cap.15k), 1, min),0)
sum(taxstats13.dfi$div293.taxbase.15kcap)*50/10^9 # $2.1 billion

# We write the Div 293 tax payable by each eligible tax payer to our main data frame
taxstats13.dfi <- taxstats13.dfi %>% mutate(taxtake.div293.15kcap = div293.taxbase.15kcap *0.15) 

taxstats13.dfi %>% summarise(taxtake.div293.15kcap = (sum(taxtake.div293.15kcap)* 50)/10^9)# So with the 15k cap we raise $0.31 billion from Div 293 tax, rather than $0.21 billion with a 10k cap, or $0.86 billion when concessional contributions are uncapped

# Total tax paid under this option. This is tax levied at marginal personal tax rates for all taxable income, including super contributions that no longer fall below the 15k cap
taxstats13.dfi$tax_payable.cap.15k <- tax.function(taxstats13.dfi$Taxable_income + taxstats13.dfi$Super.total - taxstats13.dfi$conc.cont.cap.15k,taxstats13.dfi$ML.lower,taxstats13.dfi$ML.upper) + taxstats13.dfi$conc.cont.tax.cap.15k + taxstats13.dfi$taxtake.div293.15kcap
sum(taxstats13.dfi$tax_payable.cap.15k )*50/10^9 # So 192.7 billion paid in income tax, including taxes on super contributions
sum(taxstats13.dfi$conc.cont.tax.cap.15k)*50/10^9 # $ 9 billion tax on concessional super contributions
sum(taxstats13.dfi$taxtake.div293.15kcap)*50/10^9 # Raises $0.31 billion in Div 293 tax

# Final costing for 15k cap
#---------------------------
# We compare the revenue raised with a 15k concessional contributions cap against the world where the 30k and 35k concessional caps at in place

sum(taxstats13.dfi$tax_payable.cap.15k - taxstats13.dfic$Total.tax.16)*50/10^9
# Conclusion - it raises $2.8 billion compared to current policy settings

# ========================================================================================================================

# 20k cap scenario

# Set the new contribution cap
cap.20k <- 20000

# Create a variable for the concessional contribution. We assume no one contributes more than 10k under the SG
taxstats13.dfi$conc.cont.cap.20k <- ifelse(taxstats13.dfi$Super.total > cap.20k, cap.20k, taxstats13.dfi$Super.total)
# Tax paid on super contributions
taxstats13.dfi$conc.cont.tax.cap.20k  <-  taxstats13.dfi$conc.cont.cap.20k*0.15
sum(taxstats13.dfi$conc.cont.tax.cap.20k)*50/10^9 # So $9.6 billion paid in taxes on concessional contributions with a 20k cap

# We also have to work out how much Div 293 tax should be collected under the 20k cap. Will be more than under a 10k cap as more concessional contributions (but with less tax on taxable income at full MTR)

# Step 1 is to shift any income that was previously contributed to super above the 10k cap back into taxable income so it can be taxed at the full marginal rate. This includes both SG and voluntary contributions

taxstats13.dfi$Taxable_income_s_20kcap <- taxstats13.dfi$Taxable_income_s - taxstats13.dfi$conc.cont.cap.20k
sum(taxstats13.dfi$Taxable_income_s_20kcap)*50/10^9 - (sum(taxstats13.dfi$Taxable_income_s)*50/10^9 - sum(taxstats13.dfi$conc.cont.cap.20k)*50/10^9) # Should be zero

# Step 2 is to calculate the income assessable under division 293 - this is now our new taxable income + capped contributions
taxstats13.dfi$div293.incass.20kcap <- ifelse((taxstats13.dfi$Taxable_income_s_20kcap + taxstats13.dfi$conc.cont.cap.20k) > 300000, taxstats13.dfi$Taxable_income_s_20kcap + taxstats13.dfi$conc.cont.cap.20k, 0)
sum(taxstats13.dfi$div293.incass.20kcap)*50/10^9 # This will still be the same as when concessional contributions are uncapped (i.e. $87.4 billion)

# Work out the amount to be subject to the extra 15 per cent tax - this is going to be the super contributions of those earning more than 300k up to the 20k cap. Those contributions that now fall outside the cap are treated as taxable income and are subject to the personal MTR
taxstats13.dfi$div293.taxbase.20kcap <- ifelse(taxstats13.dfi$div293.incass.20kcap > 0, apply(cbind(max((taxstats13.dfi$Taxable_income_s_20kcap + taxstats13.dfi$conc.cont.cap.20k - 300000),0),taxstats13.dfi$conc.cont.cap.20k), 1, min),0)
sum(taxstats13.dfi$div293.taxbase.20kcap)*50/10^9 # $2.7 billion

# We write the Div 293 tax payable by each eligible tax payer to our main data frame
taxstats13.dfi <- taxstats13.dfi %>% mutate(taxtake.div293.20kcap = div293.taxbase.20kcap *0.15) 

taxstats13.dfi %>% summarise(taxtake.div293.20kcap = (sum(taxtake.div293.20kcap)* 50)/10^9)# So with the 20k cap we raise $0.41 billion from Div 293 tax, rather than $0.21 billion with a 10k cap, or $0.86 billion when concessional contributions are uncapped

# Total tax paid under this option. This is tax levied at marginal personal tax rates for all taxable income, including super contributions that no longer fall below the 20k cap
taxstats13.dfi$tax_payable.cap.20k <- tax.function(taxstats13.dfi$Taxable_income + taxstats13.dfi$Super.total - taxstats13.dfi$conc.cont.cap.20k,taxstats13.dfi$ML.lower,taxstats13.dfi$ML.upper) + taxstats13.dfi$conc.cont.tax.cap.20k + taxstats13.dfi$taxtake.div293.20kcap
sum(taxstats13.dfi$tax_payable.cap.20k )*50/10^9 # So 192.3 billion paid in income tax, including taxes on super contributions
sum(taxstats13.dfi$conc.cont.tax.cap.20k)*50/10^9 # $ 9.6 billion tax on concessional super contributions
sum(taxstats13.dfi$taxtake.div293.20kcap)*50/10^9 # Raises $0.41 billion in Div 293 tax

# Final costing for 20k cap
#---------------------------
# We compare the revenue raised with a 20k concessional contributions cap against the world where the 30k and 35k concessional caps at in place

sum(taxstats13.dfi$tax_payable.cap.20k - taxstats13.dfic$Total.tax.16)*50/10^9
# Conclusion - it raises $2 billion compared to current policy settings

# ========================================================================================================================

# 25k cap scenario

# Set the new contribution cap
cap.25k <- 25000

# Create a variable for the concessional contribution. We assume no one contributes more than 10k under the SG
taxstats13.dfi$conc.cont.cap.25k <- ifelse(taxstats13.dfi$Super.total > cap.25k, cap.25k, taxstats13.dfi$Super.total)
# Tax paid on super contributions
taxstats13.dfi$conc.cont.tax.cap.25k  <-  taxstats13.dfi$conc.cont.cap.25k*0.15
sum(taxstats13.dfi$conc.cont.tax.cap.25k)*50/10^9 # So $10 billion paid in taxes on concessional contributions with a 25k cap

# We also have to work out how much Div 293 tax should be collected under the 25k cap. Will be more than under a 10k cap as more concessional contributions (but with less tax on taxable income at full MTR)

# Step 1 is to shift any income that was previously contributed to super above the 10k cap back into taxable income so it can be taxed at the full marginal rate. This includes both SG and voluntary contributions

taxstats13.dfi$Taxable_income_s_25kcap <- taxstats13.dfi$Taxable_income_s - taxstats13.dfi$conc.cont.cap.25k
sum(taxstats13.dfi$Taxable_income_s_25kcap)*50/10^9 - (sum(taxstats13.dfi$Taxable_income_s)*50/10^9 - sum(taxstats13.dfi$conc.cont.cap.25k)*50/10^9) # Should be zero

# Step 2 is to calculate the income assessable under division 293 - this is now our new taxable income + capped contributions
taxstats13.dfi$div293.incass.25kcap <- ifelse((taxstats13.dfi$Taxable_income_s_25kcap + taxstats13.dfi$conc.cont.cap.25k) > 300000, taxstats13.dfi$Taxable_income_s_25kcap + taxstats13.dfi$conc.cont.cap.25k, 0)
sum(taxstats13.dfi$div293.incass.25kcap)*50/10^9 # This will still be the same as when concessional contributions are uncapped (i.e. $87.4 billion)

# Work out the amount to be subject to the extra 15 per cent tax - this is going to be the super contributions of those earning more than 300k up to the 25k cap. Those contributions that now fall outside the cap are treated as taxable income and are subject to the personal MTR
taxstats13.dfi$div293.taxbase.25kcap <- ifelse(taxstats13.dfi$div293.incass.25kcap > 0, apply(cbind(max((taxstats13.dfi$Taxable_income_s_25kcap + taxstats13.dfi$conc.cont.cap.25k - 300000),0),taxstats13.dfi$conc.cont.cap.25k), 1, min),0)
sum(taxstats13.dfi$div293.taxbase.25kcap)*50/10^9 # $3.4 billion

# We write the Div 293 tax payable by each eligible tax payer to our main data frame
taxstats13.dfi <- taxstats13.dfi %>% mutate(taxtake.div293.25kcap = div293.taxbase.25kcap *0.15) 

taxstats13.dfi %>% summarise(taxtake.div293.25kcap = (sum(taxtake.div293.25kcap)* 50)/10^9)# So with the 25k cap we raise $0.5 billion from Div 293 tax, rather than $0.21 billion with a 10k cap, or $0.86 billion when concessional contributions are uncapped

# Total tax paid under this option. This is tax levied at marginal personal tax rates for all taxable income, including super contributions that no longer fall below the 25k cap
taxstats13.dfi$tax_payable.cap.25k <- tax.function(taxstats13.dfi$Taxable_income + taxstats13.dfi$Super.total - taxstats13.dfi$conc.cont.cap.25k,taxstats13.dfi$ML.lower,taxstats13.dfi$ML.upper) + taxstats13.dfi$conc.cont.tax.cap.25k + taxstats13.dfi$taxtake.div293.25kcap
sum(taxstats13.dfi$tax_payable.cap.25k )*50/10^9 # So $191.2 billion paid in income tax, including taxes on super contributions
sum(taxstats13.dfi$conc.cont.tax.cap.25k)*50/10^9 # $10 billion tax on concessional super contributions
sum(taxstats13.dfi$taxtake.div293.25kcap)*50/10^9 # Raises $0.5 billion in Div 293 tax

# Final costing for 25k cap
#---------------------------
# We compare the revenue raised with a 25k concessional contributions cap against the world where the 30k and 35k concessional caps at in place

sum(taxstats13.dfi$tax_payable.cap.25k - taxstats13.dfic$Total.tax.16)*50/10^9
# Conclusion - it raises $1.3 billion compared to current policy setting

# ========================================================================================================================

# 30k cap scenario

# Set the new contribution cap
cap.30k <- 30000

# Create a variable for the concessional contribution. We assume no one contributes more than 10k under the SG
taxstats13.dfi$conc.cont.cap.30k <- ifelse(taxstats13.dfi$Super.total > cap.30k, cap.30k, taxstats13.dfi$Super.total)
# Tax paid on super contributions
taxstats13.dfi$conc.cont.tax.cap.30k  <-  taxstats13.dfi$conc.cont.cap.30k*0.15
sum(taxstats13.dfi$conc.cont.tax.cap.30k)*50/10^9 # So $10.3 billion paid in taxes on concessional contributions with a 30k cap

# We also have to work out how much Div 293 tax should be collected under the 30k cap. Will be more than under a 10k cap as more concessional contributions (but with less tax on taxable income at full MTR)

# Step 1 is to shift any income that was previously contributed to super above the 10k cap back into taxable income so it can be taxed at the full marginal rate. This includes both SG and voluntary contributions

taxstats13.dfi$Taxable_income_s_30kcap <- taxstats13.dfi$Taxable_income_s - taxstats13.dfi$conc.cont.cap.30k
sum(taxstats13.dfi$Taxable_income_s_30kcap)*50/10^9 - (sum(taxstats13.dfi$Taxable_income_s)*50/10^9 - sum(taxstats13.dfi$conc.cont.cap.30k)*50/10^9) # Should be zero

# Step 2 is to calculate the income assessable under division 293 - this is now our new taxable income + capped contributions
taxstats13.dfi$div293.incass.30kcap <- ifelse((taxstats13.dfi$Taxable_income_s_30kcap + taxstats13.dfi$conc.cont.cap.30k) > 300000, taxstats13.dfi$Taxable_income_s_30kcap + taxstats13.dfi$conc.cont.cap.30k, 0)
sum(taxstats13.dfi$div293.incass.30kcap)*50/10^9 # This will still be the same as when concessional contributions are uncapped (i.e. $87.4 billion)

# Work out the amount to be subject to the extra 15 per cent tax - this is going to be the super contributions of those earning more than 300k up to the 30k cap. Those contributions that now fall outside the cap are treated as taxable income and are subject to the personal MTR
taxstats13.dfi$div293.taxbase.30kcap <- ifelse(taxstats13.dfi$div293.incass.30kcap > 0, apply(cbind(max((taxstats13.dfi$Taxable_income_s_30kcap + taxstats13.dfi$conc.cont.cap.30k - 300000),0),taxstats13.dfi$conc.cont.cap.30k), 1, min),0)
sum(taxstats13.dfi$div293.taxbase.30kcap)*50/10^9 # $3.9 billion

# We write the Div 293 tax payable by each eligible tax payer to our main data frame
taxstats13.dfi <- taxstats13.dfi %>% mutate(taxtake.div293.30kcap = div293.taxbase.30kcap *0.15) 

taxstats13.dfi %>% summarise(taxtake.div293.30kcap = (sum(taxtake.div293.30kcap)* 50)/10^9)# So with the 30k cap we raise $0.6 billion from Div 293 tax, rather than $0.21 billion with a 10k cap, or $0.86 billion when concessional contributions are uncapped

# Total tax paid under this option. This is tax levied at marginal personal tax rates for all taxable income, including super contributions that no longer fall below the 30k cap
taxstats13.dfi$tax_payable.cap.30k <- tax.function(taxstats13.dfi$Taxable_income + taxstats13.dfi$Super.total - taxstats13.dfi$conc.cont.cap.30k,taxstats13.dfi$ML.lower,taxstats13.dfi$ML.upper) + taxstats13.dfi$conc.cont.tax.cap.30k + taxstats13.dfi$taxtake.div293.30kcap
sum(taxstats13.dfi$tax_payable.cap.30k )*50/10^9 # So $190.8 billion paid in income tax, including taxes on super contributions
sum(taxstats13.dfi$conc.cont.tax.cap.30k)*50/10^9 # $10.3 billion tax on concessional super contributions
sum(taxstats13.dfi$taxtake.div293.30kcap)*50/10^9 # Raises $0.6 billion in Div 293 tax

# Final costing for 30k cap
#---------------------------
# We compare the revenue raised with a flat 30k concessional contributions cap for all ages against the world where the 30k and 35k concessional caps at in place

sum(taxstats13.dfi$tax_payable.cap.30k - taxstats13.dfic$Total.tax.16)*50/10^9
# Conclusion - it raises $0.9 billion compared to current policy setting. 
# Another way to think of this is that the cost of allowing a 35k cap for over 50s is 0.9 billion only

# ========================================================================================================================

# Pulling our 10k, 11k, 15k, 20k, and 25k concessional contributions cap costings together in a table and charting them 

# First we write our cap costings to the capped data frame so that we can use dplyr

taxstats13.dfic$tax_payable.cap.10k <- taxstats13.dfi$tax_payable.cap.10k
taxstats13.dfic$tax_payable.cap.11k <- taxstats13.dfi$tax_payable.cap.11k
taxstats13.dfic$tax_payable.cap.15k <- taxstats13.dfi$tax_payable.cap.15k
taxstats13.dfic$tax_payable.cap.20k <- taxstats13.dfi$tax_payable.cap.20k
taxstats13.dfic$tax_payable.cap.25k <- taxstats13.dfi$tax_payable.cap.25k
taxstats13.dfic$tax_payable.cap.30k <- taxstats13.dfi$tax_payable.cap.30k

Cap.revenue <- taxstats13.dfic %>%  
  summarise(cap.10k = (sum(tax_payable.cap.10k) - sum(Total.tax.16)) * 50/10^9,
            cap.11k = (sum(tax_payable.cap.11k) - sum(Total.tax.16)) * 50/10^9,
            cap.15k = (sum(tax_payable.cap.15k) - sum(Total.tax.16)) * 50/10^9,
            cap.20k = (sum(tax_payable.cap.20k) - sum(Total.tax.16)) * 50/10^9,
            cap.25k = (sum(tax_payable.cap.25k) - sum(Total.tax.16)) * 50/10^9,
            cap.30k = (sum(tax_payable.cap.30k) - sum(Total.tax.16)) * 50/10^9)

# Charting extra revenue raised from dropping thresholds vs. current policy

#Cap.revenue %>% melt() %>% ggplot(aes(x = variable, y = value)) +
#  geom_bar(stat = "identity") + 
#  labs(title = "Additional annual tax revenue from tightening the concessional cap ($ billion)", 
#       x = "Cap on concessional contributions", y = "Total revenue ($ billion)") + theme_hugh() + 
#  scale_y_continuous(limits = c(0,5),expand = c(0,0)) 


# Generates pdf of chart for printing

# dev.copy2pdf(file = "Cap.revenue.pdf", width = 15, height = 7)  

# ========================================================================================================================
# Examining permutation of the Henry proposal to tax concessional contributions at marginal tax rates less some discount

# =======================================================================================================================
# Part 1: Providing tax credit for low income earners
#====================================================

# We will first run this simulation providing a tax credit (or co-contribution) on super concessions for those below the tax free threshold
# We assume that Div 293 tax is abolished under the Henry option


# Here we consider six discounts to individuals marginal tax rates, which are: 5 percent, 10 percent, 15 percent, 20 percent; 25 percent and 30 percent

Henry.cont.discount <- c(.05,.1,.15,.2,.25,.3)

# We assume the current concessional caps stay in place - so 30k for under 50s, and 35k for over 50s. Hence we use the taxstats13.dfic dataset

# Here I am calculate the individuals marginal tax rate based on income inclusive of super - the Henry discount will then be applied to any income contributed to super. The discount is effectively a refund after all else is calculated.

# We already have the total tax liability ("taxstats13.dfi$Tax.no.conc")

# Step 1. Is to find the marginal rate for each individual
# I'm going to do this by storing the marginal tax rates, and using the function to cut the taxable income measure into factors
# The marginal tax rates include the impact of the Medicare Levy of 2 percent of incomes above $25000 or so for most taxpayers. As a proxy will just apply them to those on incomes above $37,000

tax.brackets <- c(0,18200, 37000, 80000, 180000)
taxstats13.dfic$marginal.tax.rate <- cut2(taxstats13.dfic$Taxable_income_s, cuts = tax.brackets)
levels(taxstats13.dfic$marginal.tax.rate) <- c(0,0.19,0.345,0.39,0.47) # Applying marginal rates, including the medicare levy
taxstats13.dfic$marginal.tax.rate <- as.numeric(levels(taxstats13.dfic$marginal.tax.rate))[taxstats13.dfic$marginal.tax.rate] # BC: I dont follow syntax here from Cam but it clearly works

##### Henry option 1 - 5 percent discount #####

# Contribution discount
cont.tax.discount <- 0.05

# Tax on super contribution
taxstats13.dfic$Cont.tax.henry.5 <- (taxstats13.dfic$marginal.tax.rate-cont.tax.discount)*taxstats13.dfic$Super.total
# Checking has positives (tax) and negatives (top ups)
summary(taxstats13.dfic$Cont.tax.henry.5)
hist(taxstats13.dfic$Cont.tax.henry.5)
sum(taxstats13.dfic$Cont.tax.henry.5)*50/10^9 # So Henry proposal raises $24.1 billion in taxes on super contributions, based on concessional caps of 30k and 35k

# Total tax under Henry with 5 pp discount

taxstats13.dfic$Tax.total.henry.5 <- taxstats13.dfic$Tax.no.conc - (taxstats13.dfic$marginal.tax.rate*taxstats13.dfic$Super.total) + taxstats13.dfic$Cont.tax.henry.5 

# Savings compared to current arrangements
sum(taxstats13.dfic$Tax.total.henry.5 - taxstats13.dfic$Total.tax.16)*50/10^9 # This raises $11.7 billion with a 5pp discount

#### Henry option 2 - 10 percent discount #####

# Contribution discount
cont.tax.discount <- 0.10

# Tax on super contribution
taxstats13.dfic$Cont.tax.henry.10 <- (taxstats13.dfic$marginal.tax.rate-cont.tax.discount)*taxstats13.dfic$Super.total
# Checking has positives (tax) and negatives (top ups)
summary(taxstats13.dfic$Cont.tax.henry.10)
hist(taxstats13.dfic$Cont.tax.henry.10)
sum(taxstats13.dfic$Cont.tax.henry.10)*50/10^9 # So Henry proposal raises $20.4 billion in taxes on super contributions, based on concessional caps of 30k and 35k

# Total tax under Henry with 10 pp discount
taxstats13.dfic$Tax.total.henry.10 <- taxstats13.dfic$Tax.no.conc - (taxstats13.dfic$marginal.tax.rate*taxstats13.dfic$Super.total) + taxstats13.dfic$Cont.tax.henry.10 

# Savings compared to current arrangements
sum(taxstats13.dfic$Tax.total.henry.10 - taxstats13.dfic$Tax.current)*50/10^9 # This raises $8.3 billion with a 10pp discount

#### Henry option 3 - 15 percent discount #####

# Contribution discount
cont.tax.discount <- 0.15

# Tax on super contribution
taxstats13.dfic$Cont.tax.henry.15 <- (taxstats13.dfic$marginal.tax.rate-cont.tax.discount)*taxstats13.dfic$Super.total
# Checking has positives (tax) and negatives (top ups)
summary(taxstats13.dfic$Cont.tax.henry.15)
hist(taxstats13.dfic$Cont.tax.henry.15)
sum(taxstats13.dfic$Cont.tax.henry.15)*50/10^9 # So Henry proposal raises $16.6 billion in taxes on super contributions, based on concessional caps of 30k and 35k

# Total tax under Henry with 15 pp discount
taxstats13.dfic$Tax.total.henry.15 <- taxstats13.dfic$Tax.no.conc - (taxstats13.dfic$marginal.tax.rate*taxstats13.dfic$Super.total) + taxstats13.dfic$Cont.tax.henry.15 

# Savings compared to current arrangements
sum(taxstats13.dfic$Tax.total.henry.15 - taxstats13.dfic$Tax.current)*50/10^9 # This raises $4.6 billion with a 15pp discount

#### Henry option 4 - 20 percent discount #####

# Contribution discount
cont.tax.discount <- 0.20

# Tax on super contribution
taxstats13.dfic$Cont.tax.henry.20 <- (taxstats13.dfic$marginal.tax.rate-cont.tax.discount)*taxstats13.dfic$Super.total
# Checking has positives (tax) and negatives (top ups)
summary(taxstats13.dfic$Cont.tax.henry.20)
hist(taxstats13.dfic$Cont.tax.henry.20)
sum(taxstats13.dfic$Cont.tax.henry.20)*50/10^9 # So Henry proposal raises $12.9 billion in taxes on super contributions, based on concessional caps of 30k and 35k

# Total tax under Henry with 20 pp discount
taxstats13.dfic$Tax.total.henry.20 <- taxstats13.dfic$Tax.no.conc - (taxstats13.dfic$marginal.tax.rate*taxstats13.dfic$Super.total) + taxstats13.dfic$Cont.tax.henry.20 

# Savings compared to current arrangements
sum(taxstats13.dfic$Tax.total.henry.20 - taxstats13.dfic$Tax.current)*50/10^9 # This saves $0.86 billion with a 20pp discount

#### Henry option 5 - 25 percent discount #####

# Contribution discount
cont.tax.discount <- 0.25

# Tax on super contribution
taxstats13.dfic$Cont.tax.henry.25 <- (taxstats13.dfic$marginal.tax.rate-cont.tax.discount)*taxstats13.dfic$Super.total
# Checking has positives (tax) and negatives (top ups)
summary(taxstats13.dfic$Cont.tax.henry.25)
hist(taxstats13.dfic$Cont.tax.henry.25)
sum(taxstats13.dfic$Cont.tax.henry.25)*50/10^9 # So Henry proposal raises $9.2 billion in taxes on super contributions, based on concessional caps of 30k and 35k

# Total tax under Henry with 25 pp discount 
taxstats13.dfic$Tax.total.henry.25 <- taxstats13.dfic$Tax.no.conc - (taxstats13.dfic$marginal.tax.rate*taxstats13.dfic$Super.total) + taxstats13.dfic$Cont.tax.henry.25 

# Savings compared to current arrangements
sum(taxstats13.dfic$Tax.total.henry.25 - taxstats13.dfic$Tax.current)*50/10^9 # This costs $2.9 billion billion with a 25pp discount

#### Henry option 6 - 30 percent discount #####

# Contribution discount
cont.tax.discount <- 0.30

# Tax on super contribution
taxstats13.dfic$Cont.tax.henry.30 <- (taxstats13.dfic$marginal.tax.rate-cont.tax.discount)*taxstats13.dfic$Super.total
# Checking has positives (tax) and negatives (top ups)
summary(taxstats13.dfic$Cont.tax.henry.30)
hist(taxstats13.dfic$Cont.tax.henry.30)
sum(taxstats13.dfic$Cont.tax.henry.30)*50/10^9 # So Henry proposal raises $5.4 billion in taxes on super contributions, based on concessional caps of 30k and 35k

# Total tax under Henry with 30 pp discount
taxstats13.dfic$Tax.total.henry.30 <- taxstats13.dfic$Tax.no.conc - (taxstats13.dfic$marginal.tax.rate*taxstats13.dfic$Super.total) + taxstats13.dfic$Cont.tax.henry.30 

# Savings compared to current arrangements
sum(taxstats13.dfic$Tax.total.henry.30 - taxstats13.dfic$Tax.current)*50/10^9 # This costs $6.6 billion billion with a 30pp discount

# Part 2: Providing tax credit for low income earners
#====================================================

# Now we re-run these simulations without providing a tax credit to low income earners, where the size of the Henry discount is greater than their effective marginal tax rate

#### Henry option 1 - 5 percent discount, no top up #####

taxstats13.dfic$Cont.tax.henry.5.nocredit <- ifelse(taxstats13.dfic$Cont.tax.henry.5 < 0,0, taxstats13.dfic$Cont.tax.henry.5)

# Total tax under Henry with 5 pp discount

taxstats13.dfic$Tax.total.henry.5.nocredit <- taxstats13.dfic$Tax.no.conc - (taxstats13.dfic$marginal.tax.rate*taxstats13.dfic$Super.total) + taxstats13.dfic$Cont.tax.henry.5.nocredit 

# Savings compared to current arrangements
sum(taxstats13.dfic$Tax.total.henry.5.nocredit - taxstats13.dfic$Total.tax.16)*50/10^9 # This raises $10.5 billion with a 5pp discount
sum(taxstats13.dfic$Tax.total.henry.5.nocredit - taxstats13.dfic$Tax.total.henry.5)*50/10^9 # Cost of credit is only $50 million with a 5pp discount

#### Henry option 2 - 10 percent discount, no top up #####

taxstats13.dfic$Cont.tax.henry.10.nocredit<- ifelse(taxstats13.dfic$Cont.tax.henry.10 < 0,0, taxstats13.dfic$Cont.tax.henry.10)

# Total tax under Henry with 10 pp discount

taxstats13.dfic$Tax.total.henry.10.nocredit<- taxstats13.dfic$Tax.no.conc - (taxstats13.dfic$marginal.tax.rate*taxstats13.dfic$Super.total) + taxstats13.dfic$Cont.tax.henry.10.nocredit

# Savings compared to current arrangements
sum(taxstats13.dfic$Tax.total.henry.10.nocredit- taxstats13.dfic$Total.tax.16)*50/10^9 # This raises $8.1 billion with a 10pp discount
sum(taxstats13.dfic$Tax.total.henry.10.nocredit- taxstats13.dfic$Tax.total.henry.10)*50/10^9 # Cost of credit is $0.1 billion with a 10pp discount


#### Henry option 3 - 15 percent discount, no top up #####

taxstats13.dfic$Cont.tax.henry.15.nocredit<- ifelse(taxstats13.dfic$Cont.tax.henry.15 < 0,0, taxstats13.dfic$Cont.tax.henry.15)

# Total tax under Henry with 15 pp discount

taxstats13.dfic$Tax.total.henry.15.nocredit<- taxstats13.dfic$Tax.no.conc - (taxstats13.dfic$marginal.tax.rate*taxstats13.dfic$Super.total) + taxstats13.dfic$Cont.tax.henry.15.nocredit

# Savings compared to current arrangements
sum(taxstats13.dfic$Tax.total.henry.15.nocredit- taxstats13.dfic$Total.tax.16)*50/10^9 # This raises $4.4 billion with a 15pp discount
sum(taxstats13.dfic$Tax.total.henry.15.nocredit- taxstats13.dfic$Tax.total.henry.15)*50/10^9 # Cost of credit is $0.16 billion with a 15pp discount

#### Henry option 4 - 20 percent discount, no top up #####

taxstats13.dfic$Cont.tax.henry.20.nocredit<- ifelse(taxstats13.dfic$Cont.tax.henry.20 < 0,0, taxstats13.dfic$Cont.tax.henry.20)

# Total tax under Henry with 20 pp discount

taxstats13.dfic$Tax.total.henry.20.nocredit<- taxstats13.dfic$Tax.no.conc - (taxstats13.dfic$marginal.tax.rate*taxstats13.dfic$Super.total) + taxstats13.dfic$Cont.tax.henry.20.nocredit

# Savings compared to current arrangements
sum(taxstats13.dfic$Tax.total.henry.20.nocredit- taxstats13.dfic$Total.tax.16)*50/10^9 # This saves $0.76 billion with a 20pp discount
sum(taxstats13.dfic$Tax.total.henry.20.nocredit- taxstats13.dfic$Tax.total.henry.20)*50/10^9 # Cost of credit is $0.26 billion with a 20pp discount

#### Henry option 5 - 25 percent discount, no top up #####

taxstats13.dfic$Cont.tax.henry.25.nocredit<- ifelse(taxstats13.dfic$Cont.tax.henry.25 < 0,0, taxstats13.dfic$Cont.tax.henry.25)

# Total tax under Henry with 25 pp discount

taxstats13.dfic$Tax.total.henry.25.nocredit<- taxstats13.dfic$Tax.no.conc - (taxstats13.dfic$marginal.tax.rate*taxstats13.dfic$Super.total) + taxstats13.dfic$Cont.tax.henry.25.nocredit

# Savings compared to current arrangements
sum(taxstats13.dfic$Tax.total.henry.25.nocredit- taxstats13.dfic$Total.tax.16)*50/10^9 # This costs $2.7 billion with a 25pp discount
sum(taxstats13.dfic$Tax.total.henry.25.nocredit- taxstats13.dfic$Tax.total.henry.25)*50/10^9 # Cost of credit is $0.54 billion with a 25pp discount

#### Henry option 5 - 30 percent discount, no top up #####

taxstats13.dfic$Cont.tax.henry.30.nocredit<- ifelse(taxstats13.dfic$Cont.tax.henry.30 < 0,0, taxstats13.dfic$Cont.tax.henry.30)

# Total tax under Henry with 30 pp discount

taxstats13.dfic$Tax.total.henry.30.nocredit<- taxstats13.dfic$Tax.no.conc - (taxstats13.dfic$marginal.tax.rate*taxstats13.dfic$Super.total) + taxstats13.dfic$Cont.tax.henry.30.nocredit

# Savings compared to current arrangements
sum(taxstats13.dfic$Tax.total.henry.30.nocredit- taxstats13.dfic$Total.tax.16)*50/10^9 # This costs $6.2 billion with a 30pp discount
sum(taxstats13.dfic$Tax.total.henry.30.nocredit- taxstats13.dfic$Tax.total.henry.30)*50/10^9 # Cost of credit is $0.82 billion with a 30pp discount

# ========================================================================================================================

# Now pulling together our Henry concerssional contribution discounts into one table

# First we write our cap costings to the capped data frame so that we can use dplyr

# (1) With a tax credit for Low Income
Henry.discount.revenue <- taxstats13.dfic %>%  
  summarise("Discount.5" = (sum(Tax.total.henry.5) - sum(Total.tax.16)) * 50/10^9,
            "Discount.10" = (sum(Tax.total.henry.10) - sum(Total.tax.16)) * 50/10^9,
            "Discount.15" = (sum(Tax.total.henry.15) - sum(Total.tax.16)) * 50/10^9,
            "Discount.20" = (sum(Tax.total.henry.20) - sum(Total.tax.16)) * 50/10^9,
            "Discount.25" = (sum(Tax.total.henry.25) - sum(Total.tax.16)) * 50/10^9,
            "Discount.30" = (sum(Tax.total.henry.30) - sum(Total.tax.16)) * 50/10^9)

# (2) With no tax credit for Low Income
Henry.discount.revenue.nc <- taxstats13.dfic %>%  
  summarise("Discount.5" = (sum(Tax.total.henry.5.nocredit) - sum(Total.tax.16)) * 50/10^9,
            "Discount.10" = (sum(Tax.total.henry.10.nocredit) - sum(Total.tax.16)) * 50/10^9,
            "Discount.15" = (sum(Tax.total.henry.15.nocredit) - sum(Total.tax.16)) * 50/10^9,
            "Discount.20" = (sum(Tax.total.henry.20.nocredit) - sum(Total.tax.16)) * 50/10^9,
            "Discount.25" = (sum(Tax.total.henry.25.nocredit) - sum(Total.tax.16)) * 50/10^9,
            "Discount.30" = (sum(Tax.total.henry.30.nocredit) - sum(Total.tax.16)) * 50/10^9)

# Charting extra revenue raised from dropping thresholds vs. current policy

#Henry.discount.revenue %>% melt() %>% ggplot(aes(x = variable, y = value)) +
#  geom_bar(stat = "identity", position = "dodge") + 
#  labs(title = "Additional annual tax revenue from Henry proposal on concessional contributions ($ billion)", 
#       x = "Discount to marginal tax rate (percentage points)", y = "Total revenue ($ billion)") + theme_hugh() + 
#  scale_y_continuous(limits = c(-10,15),expand = c(0,0))

#dev.copy2pdf(file = "Henry.discount.revenue.pdf", width = 15, height = 7)  

#Henry.discount.revenue.nc %>% melt() %>% ggplot(aes(x = variable, y = value)) +
#  geom_bar(stat = "identity", position = "dodge") + 
#  labs(title = "Additional annual tax revenue from Henry proposal on conc. contributions ($ billion), with no tax #credit ", 
#       x = "Discount to marginal tax rate (percentage points)", y = "Total revenue ($ billion)") + theme_hugh() +
#  scale_y_continuous(limits = c(-10,15),expand = c(0,0)) 

# dev.copy2pdf(file = "Henry.discount.revenue.nocredit.pdf", width = 15, height = 7)  

# ========================================================================================================================

# FINAL STEP: Pulling the costings from all three reform proposals together into one chart

# Final.costings.df
Final.costings.df <- data_frame(Cap.11k = Cap.revenue$cap.10k, 
                                Henry.15pp.discount = Henry.discount.revenue$Discount.15, 
                                Henry.20pp.discount = Henry.discount.revenue$Discount.20, 
                                Thold.115k = Thold.revenue$thold.125k)

# FACT CHECK: taxable income percentiles 2012-13


taxstats13.df <- taxstats13.df %>% mutate(percentile = as.numeric(cut2(taxstats13.df$Taxable_income, g = 100)))
percentiles.df <- taxstats13.df %>% group_by(percentile) %>% summarise(Average_Taxable_income = mean(Taxable_income))

# FACT CHECK: taxable income percentiles 2015-16

taxstats13.dfi <- taxstats13.dfi %>% mutate(percentile = as.numeric(cut2(taxstats13.dfi$Taxable_income, g = 100)))
percentiles.dfi <- taxstats13.df %>% group_by(percentile) %>% summarise(Average_Taxable_income = mean(Taxable_income))

# FACT CHECK: proportion of contributions made by those aged over 60, of those making contributions of more than 10k

test.df <- taxstats13.df %>% filter(Super.total.1213 > 10000) %>% 
  group_by(Age.numeric < 3) %>%
  summarise(total.cont = sum(Super.total) * 50 / 10^9)

5471932156 / 22537031652

# 24 per cent of concessional contributions that would breach a $10k annual cap are made by those aged 60 and over

test2.df <- taxstats13.df %>% filter(Super.total.1213 > 10000) %>% 
  group_by(Age) %>%
  summarise(total.cont = sum(Super.total) * 50 / 10^9)

test3.df <- taxstats13.df %>%  
  group_by(Age) %>%
  summarise(total.cont = sum(Super.total) * 50 / 10^9)

test4.df <- taxstats13.df %>% 
  group_by(Age.numeric < 3) %>%
  summarise(total.cont = sum(Super.total) * 50 / 10^9)

7.84 / 55

# In comparison, those aged 60 and over only account for 14 per cent of all concessional super contributions

# Input into chart on breakdown of contributions by tax bracket

# Chart2.df - salary sacrificed contributions by income

# We calculate the total salary sacrificed contributions by taxpayers in each tax bracket, average salary sacrificed contributions (of all those in tax bracket), average salary sacrificed contributions (of those in tax bracket that make such contributions),  and no. making contributions by tax bracket

# We combine this with data in Taxation Statistics 2012-13, Individuals Table 16, to separate out compulsory and voluntary employer contributions. This is all done in the Excel sheet 'DAT - Reconciling super contributions datasets'

sum(taxstats13.df$Rpt_emp_spr_deductions * 50) / 10^9

# We have $9.3 billion in reportable employer super contributions in 2012-13. This compares to $9.488 billion in the aggregate ATO Taxation Statistics 2012-13. So its pretty close.

taxstats13.df$marginal.tax.rate <- taxstats13.dfi$marginal.tax.rate

Sacrificed.df <- taxstats13.df %>% group_by(marginal.tax.rate) %>% 
  summarise(total.sacrificed = sum(Rpt_emp_spr_deductions * 50),
          average.sacrificed.all = sum(Rpt_emp_spr_deductions) / length(ID),
          average.sacrificed.those.that.do = sum(Rpt_emp_spr_deductions) / sum(Rpt_emp_spr_deductions > 0),
          no.sacrificing = sum(Rpt_emp_spr_deductions > 0) *50, 
          no.taxpayers = length(ID) *50
          )
  

# Input into chart on breakdown of contributions by tax bracket

# Chart3.df - salary sacrificed contributions by income

# We calculate the total salary sacrificed contributions by taxpayers in each age bracket, average salary sacrificed contributions (of all those in tax bracket), average salary sacrificed contributions (of those in tax bracket that make such contributions),  and no. making contributions by age bracket

# We combine this with data released on an ad hoc basis by the ATO on contributios by type by age bracket. This is all done in the Excel sheet 'DAT - Reconciling super contributions datasets'

sum(taxstats13.df$Rpt_emp_spr_deductions * 50) / 10^9

# We have $9.3 billion in reportable employer super contributions in 2012-13. This compares to $9.488 billion in the aggregate ATO Taxation Statistics 2012-13. So its pretty close.

Sacrificed.Age.df <- taxstats13.df %>% group_by(Age) %>% 
  summarise(total.sacrificed = sum(Rpt_emp_spr_deductions * 50),
            average.sacrificed.all = sum(Rpt_emp_spr_deductions) / length(ID),
            average.sacrificed.those.that.do = sum(Rpt_emp_spr_deductions) / sum(Rpt_emp_spr_deductions > 0),
            no.sacrificing = sum(Rpt_emp_spr_deductions > 0) *50, 
            no.taxpayers = length(ID) *50
  )

# Chart4.df - salary sacrificed contributions by income and age

# We calculate the total salary sacrificed contributions by taxpayers in each tax bracket and age group, average salary sacrificed contributions (of all those in tax bracket), average salary sacrificed contributions (of those in tax bracket that make such contributions),  and no. making contributions by age and tax bracket

# We combine this with data released on an ad hoc basis by the ATO on contributios by type by age bracket and taxable income bracket. This is all done in the Excel sheet 'DAT - Reconciling super contributions datasets'

taxstats13.df$marginal.tax.rate <- taxstats13.dfi$marginal.tax.rate

sum(taxstats13.df$Rpt_emp_spr_deductions * 50) / 10^9

# We have $9.3 billion in reportable employer super contributions in 2012-13. This compares to $9.488 billion in the aggregate ATO Taxation Statistics 2012-13. So its pretty close.

Sacrificed.Age.TI.df <- taxstats13.df %>% group_by(marginal.tax.rate,Age) %>% 
  summarise(total.sacrificed = sum(Rpt_emp_spr_deductions * 50),
            average.sacrificed.all = sum(Rpt_emp_spr_deductions) / length(ID),
            average.sacrificed.those.that.do = sum(Rpt_emp_spr_deductions) / sum(Rpt_emp_spr_deductions > 0),
            no.sacrificing = sum(Rpt_emp_spr_deductions > 0) *50, 
            no.taxpayers = length(ID) *50
  )

# ========================================================================================================================
# Costing the retention of the Low Income Superannuation Contribution
# ======================================================================================================================== #

# The LISC is set to be abolished from the 2017-18 financial year onwards. 

# We include a costing on the basis that the LISC is retained beyond that year.

# For details of the LISC see: https://www.ato.gov.au/Individuals/Super/In-detail/Growing/Low-income-super-contribution/

# We write a new variable for the concessional contributions tax paid, where that tax is refunded for those earning less than $37k (including concessional super contributions). 

# The actual income test used for the LISC is adjusted taxable income, which includes concessional super contributions, pensions and tax-free superannuation income streams (non-assessable, non exempt income). Our income test will exclude the last of these since these are not included on income tax returns. 

taxstats13.dfi$conc.cont.tax.keep.LISC <- ifelse(taxstats13.dfi$Taxable_income_s < 37000, 0, taxstats13.dfi$conc.cont.tax)

(sum(taxstats13.dfi$conc.cont.tax * 50) - sum(taxstats13.dfi$conc.cont.tax.keep.LISC * 50)) / 10^9

# So the LISC costs $0.81 billion per year
                                                 
sum(taxstats13.dfi$conc.cont.tax * 50) / 10^9

# Inflating it forward to 2017-18 would be $0.85 billion
0.81 * 1.05



# Create a variable for the concessional contribution. We assume no one contributes more than 10k under the SG
taxstats13.dfi$conc.cont <- ifelse(taxstats13.dfi$Super.total > concession.cap, concession.cap, taxstats13.dfi$Super.total)
# Tax paid on super contributions
taxstats13.dfi$conc.cont.tax <-  taxstats13.dfi$conc.cont*0.15
sum(taxstats13.dfi$conc.cont.tax)*50/10^9 # So $8 billion paid in taxes on concessional contributions

x <- taxstats13.dfi %>% filter(Over.50 == 0) %>% mutate(Super.capped = ifelse(Super.total > 30000, 30000, Super.total))
y <- taxstats13.dfi %>% filter(Over.50 == 1) %>% mutate(Super.capped = ifelse(Super.total > 35000, 35000, Super.total))
taxstats13.dfic <- rbind(x,y)
rm(x,y)

hist(taxstats13.dfic$Super.capped) # The spikes at 30k and 35k are where super contributions have been capped, simulating the current policy

# Next - we need to establish an estimate of the current system.
# Here I will assume that all super.capped contributions were income tax deductions.

taxstats13.dfic$Taxable_income.caps <- taxstats13.dfic$Taxable_income_s - taxstats13.dfic$Super.capped

# Income tax paid on 2015-16 base case. This doesnt include 15% tax on concessional super contributions or Div 293 tax
taxstats13.dfic$Income.tax.caps <- tax.function(taxstats13.dfic$Taxable_income.caps,taxstats13.dfi$ML.lower,taxstats13.dfi$ML.upper)
sum(taxstats13.dfic$Income.tax.caps)*50/10^9 # $179.8 billion

# We need to work out how much Div 293 tax would be paid now, assuming no one puts more than 30k / 35k into their super. If we dont do this we ignore the fact that some super contributions would be taxed at 30 percent, rather than 15 percent

# Step 1 is to define the income included in the income test for Div 293 tax, now that we have the 30k / 35k concessional contribution caps. 

taxstats13.dfic$Taxable_income_s_30k35kcap <- taxstats13.dfic$Taxable_income.caps + taxstats13.dfic$Super.capped
sum(taxstats13.dfic$Taxable_income_s_30k35kcap)*50/10^9 - (sum(taxstats13.dfic$Taxable_income.caps)*50/10^9 + sum(taxstats13.dfic$Super.capped)*50/10^9) # Should be zero

# Step 2 is to calculate the income assessable under division 293 - this is now our new taxable income + capped contributions
taxstats13.dfic$div293.incass.caps <- ifelse((taxstats13.dfic$Taxable_income_s_30k35kcap + taxstats13.dfic$Super.capped) > 300000, taxstats13.dfic$Taxable_income_s_30k35kcap + taxstats13.dfic$Super.capped, 0)
sum(taxstats13.dfic$div293.incass.10kcap)*50/10^9 # 

# Work out the amount to be subject to the extra 15 per cent tax - this is going to be the super contributions of those earning more than 300k up to the 30k and 35k caps. Like the 10k cap, those contributions that now fall outside the cap are treated as taxable income and are subject to the personal MTR
taxstats13.dfic$div293.taxbase.caps <- ifelse(taxstats13.dfic$div293.incass.caps > 0, apply(cbind(max((taxstats13.dfic$Taxable_income_s_30k35kcap + taxstats13.dfic$Super.capped - 300000),0),taxstats13.dfic$Super.capped), 1, min),0)
sum(taxstats13.dfic$div293.taxbase.caps)*50/10^9 # So tax base is $4.7 billion in concessional contributions that will be subject to Div 293 tax

# We write the Div 293 tax paytable by each eligible tax payer to our main data frame
taxstats13.dfic <- taxstats13.dfic %>% mutate(taxtake.div293.caps = div293.taxbase.caps *0.15) 

taxstats13.dfic %>% summarise(taxtake.div293.caps = (sum(taxtake.div293.caps)* 50)/10^9)# So with the 30k/35k caps we only raise $0.71 billion from Div 293 tax, rather than $0.86 billion when concessional contributions are untaxed. 

################################

# Tax on super contributions in the base case - including Div 293 tax
taxstats13.dfic$Super.cont.tax.caps <- taxstats13.dfic$Super.capped*0.15 + taxstats13.dfic$taxtake.div293.caps
sum(taxstats13.dfic$Super.cont.tax.caps)*50/10^9 # So total tax on concessional super contributions of $11.2 billion
sum(taxstats13.dfic$Super.capped*0.15)*50/10^9 # 15 percent tax rate on contributions raises $10.5 billion
sum(taxstats13.dfic$taxtake.div293.caps)*50/10^9 # Div 293 tax raises 0.71 billion

# Chart data for pre-tax voluntary contribution marimekko

marimekko.df <- taxstats13.dfi %>% 
  mutate(Taxable.income.decile = as.numeric(cut2(Taxable_income_s, g = 10))) %>%
  group_by(Taxable.income.decile, Age.group) %>%
  summarise(total.voluntary.contributions = sum(Super.voluntary) * 50, 
            total.taxpayers = length(Super.voluntary) * 50, 
            average.voluntary.contributions = mean(Super.voluntary))

View(marimekko.df)

write.table(marimekko.df , sep = "\t", file = clip <- pipe("pbcopy", "w"))
close(clip)


 