
source("earnings-preparation.R")

tax_all_super_earnings_over60_at <- function(rate = 0.15, 
                                             tax.rate.accumulation.phase = 0.125, 
                                             tax.rate.drawdown.phase = 0.14, 
                                             super.rate.of.return.in.accumulation = 0.07, 
                                             super.rate.of.return.in.drawdown = 0.05){
  
  # So super earnings will depend upon whether super account holders are in the accumulation or drawdown phases
  
  person.dfkvi$Super.earnings <- ifelse(person.dfkvi$Super.ddown == 1, 
                                        super.rate.of.return.in.drawdown*person.dfkvi$Total.super, 
                                        super.rate.of.return.in.accumulation*person.dfkvi$Total.super)
  
  person.dfkvi$Total.income.annual.s <- 
    person.dfkvi$Total.income.annual + person.dfkvi$Super.earnings
  
  person.dfkvi$Taxable.income.annual.s <- 
    person.dfkvi$Taxable.income.annual + person.dfkvi$Super.earnings
  
# (a) Reintroduce a 15% tax rate on super for over 60s

# First we set our earnings tax rate, dependig on whether they're in the drowdown phase or not (to reflect fact that ppl make more conservative investment decisions in retirement that yield fewer capital gains, hence higher effective tax rate in the ddown phase)

person.dfkvi$super.earnings.tax.opt1 <- ifelse(person.dfkvi$Super.ddown == 1, 
                                               person.dfkvi$Super.earnings * tax.rate.accumulation.phase, 
                                               person.dfkvi$Super.earnings * tax.rate.drawdown.phase)

# However, those in the drawdown phase that now face tax on super earnings from the first $ have a strong incentive to withdraw funds from super in order to make the most of the tax free threshold that exists outside of super. We assume that individuals respond to the tax change by minimising their tax by withdrawing super earnings up to the point that they maximise their tax-free threshold and LITO and SAPTO entitlements outside of super. 

# Incidentally, this modelling approach also satisfies alternative approaches to administering any tax free threshold, or rebate on super earnings, where super earnings are included in taxable income on individuals' tax returns, allowing retirees to make the most of any unused TF threshold, SAPTO and LITO entitlements, and any tax paid on super earnings can be paid out of individuals' disposable incomes, or financed out of their super account balances each year.

# So we write in our behavioural change - ppl affected withdraw what super earnings they can from super to make use of TF thresholds outside of super

# We need to write a variable for the effective tax free income threshold for each individuals, based on the TF threshold and their eligibility for SAPTO and LITO

# Working out what tax bracket they are in, including the Medicare Levy for those above 37k - we dont use this yet but could come in handy

tax.brackets <- c(-200000,18200, 37000, 80000, 180000)
person.dfkvi$marginal.tax.rate <- cut2(person.dfkvi$Taxable.income.annual.s, cuts = tax.brackets)
levels(person.dfkvi$marginal.tax.rate) <- c(0,0,0.19,0.345,0.39,0.47)
person.dfkvi$TF.threshold <- rep(18200, length(person.dfkvi$Age))

# The unused taxfree threshold, in terms of tax paid, is the sum of any SAPTO and LITO entitlements of an individual, plus any unused portion of the 18200 TF threshold multiplied by the tax rate on earnings in the drawdown phase

# This is the unused tax free entitlements in terms of tax that can be avoided, not income

# THIS IS WRONG - we're including the SAPTO and LITO entitlements here even when they are being used

# We need to write a different function instead that only accounts for SAPTO and LITO entitlement WHEN they are actually unused. It works in the simulation below


person.dfkvi %<>%
  mutate(Excess.TF.income.threshold = ifelse(Super.ddown == 1, 
                                             ifelse(Taxable.income.annual < 18200,
                                                    TF.unused.function(Taxable.income.annual) * tax.rate.ddown + SAPTO.entitlement + LITO.entitlement,
                                                    ifelse(Tax.estimate + Medicare.levy - SAPTO.entitlement - LITO.entitlement < 0,
                                                           SAPTO.entitlement + LITO.entitlement - Tax.estimate - Medicare.levy,
                                                           0)),
                                             0))

# # View(person.dfkvi)

# We check what's the max income of someone that has tax credits left. It should only be circa $33k

ddown.max.income.df <- person.dfkvi %>% filter(Excess.TF.income.threshold > 0)

summary(ddown.max.income.df$Taxable.income.annual)

# Ok so the max income of someone with  is $33,160, which is pretty spot on with the maximum effective tax free threshold for an individual of Age Pension age

summary(ddown.max.income.df$Total.income.annual.s)

# But there are people with very high super earnings that still qualify for tax credits from SAPTO and LITO that they can use to reduce their taxes on super earnings

# How many unused tax credits are there among those in the ddown phase?

sum(person.dfkvi$Excess.TF.income.threshold * person.dfkvi$Weights) / 10^9

# $1.85 billion in unused tax credits among those in the drawdown phase. That may be a little low? Lets check against other characteristics of this population group, as it is a pretty small group overall.

ddown.group.df <- person.dfkvi %>% filter(Super.ddown == 1) %>%
  summarise(Total.income = sum(Taxable.income.annual * Weights) / 10^9,
            Average.income = weighted.mean(x = Taxable.income.annual, w = Weights), 
            Total.tax = sum(Tax.estimate * Weights) / 10^9, 
            Average.tax = weighted.mean(x = Tax.estimate, w = Weights),
            Total.unused.entitlements = sum(Excess.TF.income.threshold * Weights), 
            Average.unused.entitlements = weighted.mean(x = Excess.TF.income.threshold, w = Weights),
            No.individuals = sum(Weights))

2700 * 10^6 / 10^9 # The maximum possible tax credits from LITO and SPTO that could be expected from this group would be around $2.7 billion. Given that the average income is $26,000, unused entitlements that average $1800 seems reasonable, leading to a total unused entitlement from this group of $1.8 billion. This means that the maximum revenue lost through behavioural change is going to be $1.8 million from any reforms to super earnings taxes for over 60s.

# So now we know how much tax ppl can avoid by shifting their earnings out of super to make the most of any remaining TF threshold and SAPTO / LITO entitlements outside of the super system

# Super earnings tax collected after behaviour change

person.dfkvi$super.earnings.tax.opt1.behav <- ifelse(person.dfkvi$super.earnings.tax.opt1 < person.dfkvi$Excess.TF.income.threshold, 0, person.dfkvi$super.earnings.tax.opt1 - person.dfkvi$Excess.TF.income.threshold)

# We write a variable of the tax foregone by behavioural change

person.dfkvi$super.earnings.tax.opt1.foregone <- person.dfkvi$super.earnings.tax.opt1 - person.dfkvi$super.earnings.tax.opt1.behav

sum(person.dfkvi$super.earnings.tax.opt1.foregone * person.dfkvi$Weights) / 10^9

# So only lose $0.9 billion via behavioural change

# We compare the two super tax reform outcomes to make sure the function has worked properly

beffect.opt1.df <- data_frame(Total.income.s = person.dfkvi$Total.income.annual.s,
                              Total.income = person.dfkvi$Total.income.annual,
                              Super.earnings.untaxed = person.dfkvi$Super.earnings,
                              Super.earnings.tax.opt1 = person.dfkvi$super.earnings.tax.opt1,
                              Opt1.no.behav = person.dfkvi$super.earnings.tax.opt1,
                              Opt1.w.behav = person.dfkvi$super.earnings.tax.opt1.behav,
                              Opt1.b.change = person.dfkvi$super.earnings.tax.opt1.foregone,
                              TF.outside = person.dfkvi$Excess.TF.income.threshold,
                              Super.ddown = person.dfkvi$Super.ddown) %>% filter(Super.ddown == 1)

# # View(beffect.opt1.df)

cross.check <- beffect.opt1.df %>% filter (Super.ddown == 1) %>% ggplot(aes(x = Total.income, y = TF.outside))+ geom_point()

summary(cross.check$Age.numeric)

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

# # View(Behav.effect.opt1.df)


# So 70% of those affected could reduce their tax liability by shifting $$ from super, and we lose 30% of the potential extra super earnings tax

# Total super earnings tax raised - $10 billion

sum(person.dfkvi$super.earnings.tax.opt1.behav * person.dfkvi$Weights) / 10^9

# Size of super earnings tax expenditure post reform is $18.4 billion

person.dfkvi$Super.concession.opt1.behav <- person.dfkvi$Tax.estimate.s - person.dfkvi$Tax.estimate - person.dfkvi$super.earnings.tax.opt1.behav

sum(person.dfkvi$Super.concession.opt1.behav * person.dfkvi$Weights) / 10^9

# Budget saving from reintroducing the 15% tax on super earnings for over 60s - $3.9 billion

return(sum((person.dfkvi$super.earnings.tax.opt1.behav - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights))
}