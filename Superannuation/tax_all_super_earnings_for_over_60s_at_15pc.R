library(Hmisc)


source("earnings-preparation.R")

TF.unused.function <- function(income) {
  TF.unused <- ifelse(income < 0, 18200, 
                      ifelse(income < 18200, (18200 - income), 0))
}

tax_all_super_earnings_over60_at <- function(rate = 0.15, 
                                             tax.rate.accumulation.phase = 0.125, 
                                             tax.rate.drawdown.phase = 0.14, 
                                             super.rate.of.return.in.accumulation = 0.07, 
                                             super.rate.of.return.in.drawdown = 0.05){
  
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
return(sum((person.dfkvi$super.earnings.tax.opt1.behav - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights))
}