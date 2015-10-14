#Codebook for RCT assignment

drugtreatment <- read.csv("~/Downloads/drugtreatment.csv")
View(drugtreatment)
summary(drugtreatment) #descriptives

count(drugtreatment$race)
count(drugtreatment$treat)
count(drugtreatment$dfree)
count(drugtreatment$ivhx)
count(drugtreatment$site)

attach(drugtreatment)
sitebytreat <- table(site,treat) # A will be rows, B will be columns 
sitebytreat # print table

prop.table(sitebytreat, 2) #percentages in columns

# independent 2-group t-test
t.test(dfree~treat)

prop.table(sitebytreat, 1) # row percentages

#balancetest
balancetest <- lm(treat ~ age + beck + ivhx2 + ivhx3 + ndrugtx + race + site, data=drugtreatment)
summary(balancetest) # show results
coefficients(balancetest) # model coefficients
anova(balancetest) # anova table
balancetestsite <- lm(site ~ age + beck + ivhx2 + ivhx3 + ndrugtx + race + treat, data=drugtreatment)
summary(balancetestsite)

dfreetest <- lm(dfree ~ treat, data=drugtreatment)
summary(dfreetest) # show results

dfreetest2 <- lm(dfree ~ treat + age + beck + ivhx2 + ivhx3 + ndrugtx + race + site, data=drugtreatment)
summary(dfreetest) # show results

site2 <- subset(drugtreatment, site=="1", select=age:treatndrugtx)
testsite1 <- lm(dfree ~ treat + age + beck + ivhx2 + ivhx3 + ndrugtx + race, data=site2)
coefficients(testsite1)


site1 <- subset(drugtreatment, site=="0", select=age:treatndrugtx)
testsite2 <- lm(dfree ~ treat + age + beck + ivhx2 + ivhx3 + ndrugtx + race, data=site1)
summary(testsite2)

treatsite <- lm(dfree ~ age + beck + ivhx2 + ivhx3 + ndrugtx + race + +site + treatsite, data=drugtreatment)
summary(treatsite) # show results

subgroup <- lm(dfree ~ age + beck + ivhx2 + ivhx3 + ndrugtx + race + site + treatbeck + treatrace + treatage + treatndrugtx, data=drugtreatment)
summary(subgroup) # show results


logistic <- glm(dfree ~ treat + age + beck + ivhx2 + ivhx3 + ndrugtx + race + site, data=drugtreatment, family=binomial())
summary(logistic)
exp(coef(logistic))


subgrouprace <- lm(dfree ~ age + beck + ivhx2 + ivhx3 + ndrugtx + race + site + racesite, data=drugtreatment)

ftable(race, site, dfree)
ftable(race, site, treat, dfree)


