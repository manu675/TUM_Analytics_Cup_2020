library(tidyverse) 
library(descr)
library(ggplot2)
setwd("~/Dropbox/TUM/Master/2_Semester/BA/analytics_cup")
payments = read_delim("payments.csv", delim = ",")
payments = payments[payments$Total_Amount_of_Payment_USDollars<5000000,]
physicians = read_delim("physicians.csv", delim = ",")
# remove Allopathic & Osteopathic Physicians|
physicians$Primary_Specialty = gsub("Allopathic & Osteopathic Physicians\\|", "", physicians$Primary_Specialty)
#  remove \|.*
physicians$Primary_Specialty = gsub("\\|.*", "", physicians$Primary_Specialty)
#unique(physicians$Primary_Specialty)
#length(unique(physicians$Primary_Specialty))


valid_physicians = physicians[physicians$set =="train",]
#unique(valid_physicians$Primary_Specialty)
#length(unique(valid_physicians$Primary_Specialty))
set.seed(8675309)
train_physicians = valid_physicians[sample(nrow(valid_physicians), 4000), ]
#length(unique(train_physicians$Primary_Specialty))
valid_physicians = valid_physicians[ !(valid_physicians$id %in% train_physicians$id), ]

test_physicians = physicians[physicians$set =="test",]
test_physicians

# Ownership_Indicator: No -> 0 | Yes -> 1
payments <- payments %>%
  mutate(Ownership_Indicator = ifelse(Ownership_Indicator == "No",0,1))

# Group payments by Physician_ID and aggregate max(Ownership_Indicator)
ownership_interest = payments %>% group_by(Physician_ID = Physician_ID ) %>% summarise(
  ownership_interest = max(Ownership_Indicator)
)
ownership_interest
#physician_1 = payments[payments$Physician_ID =="1",]
#physician_1$Ownership_Indicator
#physician_6 = payments[payments$Physician_ID =="6",]
#physician_6$Ownership_Indicator

# Group payments by Physician_ID and aggregate sum(Total_Amount_of_Payment_USDollars)
total_payments = payments %>% group_by(Physician_ID = Physician_ID ) %>% summarise(
  total_payments = sum(Total_Amount_of_Payment_USDollars)
)
total_payments
#physician_1 = payments[payments$Physician_ID =="1",]
#physician_1$Total_Amount_of_Payment_USDollars
#sum(physician_1$Total_Amount_of_Payment_USDollars)
#physician_6 = payments[payments$Physician_ID =="6",]
#physician_6$Total_Amount_of_Payment_USDollars
#sum(physician_6$Total_Amount_of_Payment_USDollars)

# Group payments by Physician_ID and aggregate n()
number_of_payments = payments %>% group_by(Physician_ID = Physician_ID ) %>% summarise(
  number_of_payments = n()
)
number_of_payments

#names(payments)
#unique(payments$Nature_of_Payment_or_Transfer_of_Value)
# Group payments by Physician_ID and aggregate top_Nature_of_Payment_or_Transfer_of_Value
top_nature = payments %>% group_by(Physician_ID = Physician_ID, nature=Nature_of_Payment_or_Transfer_of_Value) %>% summarise(
  total_of_nature = sum(Total_Amount_of_Payment_USDollars)
)
top_nature
top_nature = top_nature %>% group_by(Physician_ID) %>% top_n(1, total_of_nature)
top_nature

# Group by month and Physician_ID and aggregate number of payments
(payments_per_month = payments %>% group_by(year = gsub("../../", "", Date), month = gsub("/../....", "", Date),Physician_ID = Physician_ID) %>% 
    summarise(total = sum(Total_Amount_of_Payment_USDollars),number_of_payments = n()))
(payments_per_month = payments_per_month[payments_per_month$year>2013,])
#p1_payments_per_month = payments_per_month[payments_per_month$Physician_ID == "301",]
#p1_payments_per_month
#physician_1 = payments[payments$Physician_ID =="301",]
#physician_1
#physician_1

#(payments_per_month[payments_per_month$Physician_ID == "301",])

(payments_per_year = payments_per_month %>% group_by(year = year,Physician_ID = Physician_ID) %>% summarise(total = sum(total),number_of_payments = sum(number_of_payments)))
#(payments_per_year[payments_per_year$Physician_ID == "4000",])
#barplot(height=payments_per_month$number_of_payments,names=paste(payments_per_month$year,'|',payments_per_month$month, "|",payments_per_month$Physician_ID))
(payment_range = payments_per_year %>% group_by(Physician_ID = Physician_ID) %>% summarise(range = max(total) - min(total)))

(number_of_payment_range = payments_per_year %>% group_by(Physician_ID = Physician_ID) %>% summarise(number_of_payment_range = max(number_of_payments) - min(number_of_payments)))

(max_of_payment_count = payments_per_year %>% group_by(Physician_ID = Physician_ID) %>% summarise(max_of_payment_count = max(number_of_payments)))

(min_of_payment_count = payments_per_year %>% group_by(Physician_ID = Physician_ID) %>% summarise(min_of_payment_count = min(number_of_payments)))

# group by Company_ID
total_company_payments = payments %>% group_by(Physician_ID = Physician_ID,Company_ID = Company_ID ) %>% summarise(
  total_payments = sum(Total_Amount_of_Payment_USDollars), number_of_payments = n(), avg = total_payments/number_of_payments
)
total_company_payments
total_company_payments[total_company_payments$Physician_ID == "1751",]

# top_company 
top_company = total_company_payments %>% top_n(1,total_payments) %>% top_n(1, Company_ID) %>% select(Physician_ID,Company_ID)
top_company$Company_ID <- sapply(top_company$Company_ID, as.character)
length(top_company$Company_ID)

# group by Physician_ID
total_companys = total_company_payments %>% group_by(Physician_ID = Physician_ID) %>% summarise(
  companys = n(), sd=sd(avg)
)
total_companys
summary(total_companys$sd)

# group by Related_Product_Indicator
Related_Product_Indicator = payments %>% group_by(Physician_ID = Physician_ID, 
                                                  rpi= Related_Product_Indicator)  %>% summarise(
  rpi_count = n()
)
Related_Product_Indicator = Related_Product_Indicator %>% group_by(Physician_ID) %>% top_n(1, rpi_count) %>% top_n(1, rpi)
Related_Product_Indicator

# Form_of_Payment_or_Transfer_of_Value
Form_of_Payment_or_Transfer_of_Value = payments %>% group_by(Physician_ID = Physician_ID, 
                                                  form_of_payment= Form_of_Payment_or_Transfer_of_Value)  %>% summarise(
                                                    fop_count = n()
                                                  )
unique(Form_of_Payment_or_Transfer_of_Value$form_of_payment)
Physician_ID=unique(payments$Physician_ID)
fops = tibble(Physician_ID)
fops
(cash = Form_of_Payment_or_Transfer_of_Value[Form_of_Payment_or_Transfer_of_Value$form_of_payment =="Cash or cash equivalent",])
fops = merge(x = fops, y = cash, by = "Physician_ID", all.x = TRUE)
colnames(fops)[3] <- "cash"
fops = fops %>% select(Physician_ID,cash)
fops[is.na(fops)] <- 0
fops

(services = Form_of_Payment_or_Transfer_of_Value[Form_of_Payment_or_Transfer_of_Value$form_of_payment =="In-kind items and services",])
(fops = merge(x = fops, y = services, by = "Physician_ID", all.x = TRUE))
colnames(fops)[4] <- "services"
fops = fops %>% select(Physician_ID,cash,services)
fops[is.na(fops)] <- 0
fops

(stock = Form_of_Payment_or_Transfer_of_Value[Form_of_Payment_or_Transfer_of_Value$form_of_payment =="Stock",])
(fops = merge(x = fops, y = stock, by = "Physician_ID", all.x = TRUE))
colnames(fops)[5] <- "stock"
fops = fops %>% select(Physician_ID,cash,services,stock)
fops[is.na(fops)] <- 0
fops

(stock_opt = Form_of_Payment_or_Transfer_of_Value[Form_of_Payment_or_Transfer_of_Value$form_of_payment =="Stock option",])
(fops = merge(x = fops, y = stock_opt, by = "Physician_ID", all.x = TRUE))
colnames(fops)[6] <- "stock_opt"
fops = fops %>% select(Physician_ID,cash,services,stock,stock_opt)
fops[is.na(fops)] <- 0
fops

(ownership = Form_of_Payment_or_Transfer_of_Value[Form_of_Payment_or_Transfer_of_Value$form_of_payment =="Any other ownership interest",])
(fops = merge(x = fops, y = ownership, by = "Physician_ID", all.x = TRUE))
colnames(fops)[7] <- "ownership"
fops = fops %>% select(Physician_ID,cash,services,stock,stock_opt,ownership)
fops[is.na(fops)] <- 0
fops

(dividend = Form_of_Payment_or_Transfer_of_Value[Form_of_Payment_or_Transfer_of_Value$form_of_payment =="Dividend, profit or other return on investment",])
(fops = merge(x = fops, y = dividend, by = "Physician_ID", all.x = TRUE))
colnames(fops)[8] <- "dividend"
fops = fops %>% select(Physician_ID,cash,services,stock,stock_opt,ownership,dividend)
fops[is.na(fops)] <- 0
fops

(stock_or_other = Form_of_Payment_or_Transfer_of_Value[Form_of_Payment_or_Transfer_of_Value$form_of_payment =="Stock, stock option, or any other ownership interest",])
(fops = merge(x = fops, y = stock_or_other, by = "Physician_ID", all.x = TRUE))
colnames(fops)[9] <- "stock_or_other"
fops = fops %>% select(Physician_ID,cash,services,stock,stock_opt,ownership,dividend,stock_or_other)
fops[is.na(fops)] <- 0
fops

Form_of_Payment_or_Transfer_of_Value
Form_of_Payment_or_Transfer_of_Value = Form_of_Payment_or_Transfer_of_Value %>% group_by(Physician_ID) %>% top_n(1, fop_count) %>% top_n(1, form_of_payment)
Form_of_Payment_or_Transfer_of_Value

# select id,State,Primary_Specialty
train_physicians = train_physicians %>% select(id,State,Primary_Specialty)
valid_physicians = valid_physicians %>% select(id,State,Primary_Specialty)
test_physicians = test_physicians %>% select(id,State,Primary_Specialty)

# rename id -> Physician_ID
colnames(train_physicians)[1] <- "Physician_ID"
train_physicians

colnames(valid_physicians)[1] <- "Physician_ID"
valid_physicians

colnames(test_physicians)[1] <- "Physician_ID"
test_physicians

# merge ownership_interest with train_physicians
(train_physicians = merge(train_physicians, ownership_interest, by = "Physician_ID"))
(valid_physicians = merge(valid_physicians, ownership_interest, by = "Physician_ID"))

# merge ownership_interest with total_payments
(train_physicians = merge(train_physicians, total_payments, by = "Physician_ID"))
(valid_physicians = merge(valid_physicians, total_payments, by = "Physician_ID"))
(test_physicians = merge(test_physicians, total_payments, by = "Physician_ID"))

# merge ownership_interest with number_of_payments
(train_physicians = merge(train_physicians, number_of_payments, by = "Physician_ID"))
(valid_physicians = merge(valid_physicians, number_of_payments, by = "Physician_ID"))
(test_physicians = merge(test_physicians, number_of_payments, by = "Physician_ID"))

# merge ownership_interest with top_nature
(train_physicians = merge(train_physicians, top_nature, by = "Physician_ID"))
(valid_physicians = merge(valid_physicians, top_nature, by = "Physician_ID"))
(test_physicians = merge(test_physicians, top_nature, by = "Physician_ID"))

# merge ownership_interest with payment_range
(train_physicians = merge(train_physicians, payment_range, by = "Physician_ID"))
(valid_physicians = merge(valid_physicians, payment_range, by = "Physician_ID"))
(test_physicians = merge(test_physicians, payment_range, by = "Physician_ID"))

# merge ownership_interest with number_of_payment_range
(train_physicians = merge(train_physicians, number_of_payment_range, by = "Physician_ID"))
(valid_physicians = merge(valid_physicians, number_of_payment_range, by = "Physician_ID"))
(test_physicians = merge(test_physicians, number_of_payment_range, by = "Physician_ID"))

# merge ownership_interest with total_companys
(train_physicians = merge(train_physicians, total_companys, by = "Physician_ID"))
(valid_physicians = merge(valid_physicians, total_companys, by = "Physician_ID"))
(test_physicians = merge(test_physicians, total_companys, by = "Physician_ID"))

# merge ownership_interest with Related_Product_Indicator
(train_physicians = merge(train_physicians, Related_Product_Indicator, by = "Physician_ID"))
(valid_physicians = merge(valid_physicians, Related_Product_Indicator, by = "Physician_ID"))
(test_physicians = merge(test_physicians, Related_Product_Indicator, by = "Physician_ID"))

# merge ownership_interest with Form_of_Payment_or_Transfer_of_Value
(train_physicians = merge(train_physicians, Form_of_Payment_or_Transfer_of_Value, by = "Physician_ID"))
(valid_physicians = merge(valid_physicians, Form_of_Payment_or_Transfer_of_Value, by = "Physician_ID"))
(test_physicians = merge(test_physicians, Form_of_Payment_or_Transfer_of_Value, by = "Physician_ID"))

# merge ownership_interest with Form_of_Payment_or_Transfer_of_Value
(train_physicians = merge(train_physicians, fops, by = "Physician_ID"))
(valid_physicians = merge(valid_physicians, fops, by = "Physician_ID"))
(test_physicians = merge(test_physicians, fops, by = "Physician_ID"))

# merge ownership_interest with top_company
(train_physicians = merge(train_physicians, top_company, by = "Physician_ID"))
(valid_physicians = merge(valid_physicians, top_company, by = "Physician_ID"))
(test_physicians = merge(test_physicians, top_company, by = "Physician_ID"))




# merge ownership_interest with max_of_payment_count
#(train_physicians = merge(train_physicians, max_of_payment_count, by = "Physician_ID"))
#(valid_physicians = merge(valid_physicians, max_of_payment_count, by = "Physician_ID"))
#(test_physicians = merge(test_physicians, max_of_payment_count, by = "Physician_ID"))

# merge ownership_interest with min_of_payment_count
#(train_physicians = merge(train_physicians, min_of_payment_count, by = "Physician_ID"))
#(valid_physicians = merge(valid_physicians, min_of_payment_count, by = "Physician_ID"))
#(test_physicians = merge(test_physicians, min_of_payment_count, by = "Physician_ID"))


sum(is.na(train_physicians))
train_physicians = na.omit(train_physicians)
train_physicians

sum(is.na(valid_physicians))
valid_physicians = na.omit(valid_physicians)
valid_physicians

sum(is.na(test_physicians))
test_physicians = na.omit(test_physicians)
test_physicians


#write_csv(train_physicians, "train_physicians.csv")
#write_csv(test_physicians, "test_physicians.csv")




Farben = c("lightgreen","coral")

# 0 -> No | 1 -> Yes
ownership_interest.f = factor(train_physicians$ownership_interest)
levels(ownership_interest.f) = c("No","Yes")
ownership_interest.f

# ownership_interest per state
crosstab(ownership_interest.f,train_physicians$State,prop.c=T, col = Farben)

# ownership_interest per Primary_Specialty
crosstab(ownership_interest.f,train_physicians$Primary_Specialty,prop.c=T, col = Farben)

# ownership_interest per top_nature
crosstab(ownership_interest.f,train_physicians$nature,prop.c=T, col = Farben)
sort(unique(train_physicians$nature))

# ownership_interest per rpi
crosstab(ownership_interest.f,train_physicians$rpi,prop.c=T, col = Farben)


# ownership_interest per rpi
crosstab(ownership_interest.f,train_physicians$form_of_payment,prop.c=T, col = Farben)

# ownership_interest per top_company
crosstab(ownership_interest.f,train_physicians$Company_ID,prop.c=T, col = Farben)

# ownership_interest ~ total_of_nature
ggplot(data=train_physicians, aes(y=log(total_of_nature),x=ownership_interest.f))+ geom_violin() + labs(title=" ",y="total_of_nature", x = " ")

ggplot(train_physicians, aes(log(total_of_nature), fill=ownership_interest.f)) + geom_density(alpha=.6) +
  scale_fill_manual(values = c('lightgreen','coral'))

# ownership_interest ~ total_payments
ggplot(data=train_physicians, aes(y=log(total_payments),x=ownership_interest.f))+ geom_violin() + labs(title=" ",y="total_payments", x = " ")

ggplot(train_physicians, aes(log(total_payments), fill=ownership_interest.f)) + geom_density(alpha=.6) +
  scale_fill_manual(values = c('lightgreen','coral'))

# ownership_interest ~ number_of_payments
ggplot(data=train_physicians, aes(y=number_of_payments,x=ownership_interest.f))+ geom_violin() + labs(title=" ",y="number_of_payments", x = " ")

ggplot(train_physicians, aes(log(number_of_payments), fill=ownership_interest.f)) + geom_density(alpha=.6) +
  scale_fill_manual(values = c('lightgreen','coral'))

# ownership_interest ~ payment_range
ggplot(data=train_physicians, aes(y=log(range),x=ownership_interest.f))+ geom_violin() + labs(title=" ",y="range", x = " ")

ggplot(train_physicians, aes(log(range), fill=ownership_interest.f)) + geom_density(alpha=.6) +
  scale_fill_manual(values = c('lightgreen','coral'))

# ownership_interest ~ number_of_payment_range
ggplot(data=train_physicians, aes(y=number_of_payment_range,x=ownership_interest.f))+ geom_violin() + labs(title=" ",y="range", x = " ")

ggplot(train_physicians, aes(log(number_of_payment_range), fill=ownership_interest.f)) + geom_density(alpha=.6) +
  scale_fill_manual(values = c('lightgreen','coral'))

# ownership_interest ~ companys
ggplot(data=train_physicians, aes(y=companys,x=ownership_interest.f))+ geom_violin() + labs(title=" ",y="total_companys", x = " ")

ggplot(train_physicians, aes(companys, fill=ownership_interest.f)) + geom_density(alpha=.6) +
  scale_fill_manual(values = c('lightgreen','coral'))

# ownership_interest ~ sd
ggplot(data=train_physicians, aes(y=log(sd),x=ownership_interest.f))+ geom_violin() + labs(title=" ",y="sd", x = " ")

ggplot(train_physicians, aes(log(sd), fill=ownership_interest.f)) + geom_density(alpha=.6) +
  scale_fill_manual(values = c('lightgreen','coral'))

# ownership_interest ~ rpi_count
ggplot(data=train_physicians, aes(y=rpi_count,x=ownership_interest.f))+ geom_violin() + labs(title=" ",y="total_companys", x = " ")

ggplot(train_physicians, aes(log(rpi_count), fill=ownership_interest.f)) + geom_density(alpha=.6) +
  scale_fill_manual(values = c('lightgreen','coral'))

# ownership_interest ~ fop_count
ggplot(data=train_physicians, aes(y=fop_count,x=ownership_interest.f))+ geom_violin() + labs(title=" ",y="total_companys", x = " ")

ggplot(train_physicians, aes(log(fop_count), fill=ownership_interest.f)) + geom_density(alpha=.6) +
  scale_fill_manual(values = c('lightgreen','coral'))

# ownership_interest ~ cash
ggplot(data=train_physicians, aes(y=log(cash),x=ownership_interest.f))+ geom_violin() + labs(title=" ",y="cash", x = " ")

ggplot(train_physicians, aes(log(cash), fill=ownership_interest.f)) + geom_density(alpha=.6) +
  scale_fill_manual(values = c('lightgreen','coral'))

# ownership_interest ~ services
ggplot(data=train_physicians, aes(y=log(services),x=ownership_interest.f))+ geom_violin() + labs(title=" ",y="services", x = " ")

ggplot(train_physicians, aes(log(services), fill=ownership_interest.f)) + geom_density(alpha=.6) +
  scale_fill_manual(values = c('lightgreen','coral'))

# ownership_interest ~ stock
ggplot(data=train_physicians, aes(y=stock,x=ownership_interest.f))+ geom_violin() + labs(title=" ",y="stock", x = " ")

ggplot(train_physicians, aes(stock, fill=ownership_interest.f)) + geom_density(alpha=.6) +
  scale_fill_manual(values = c('lightgreen','coral'))

# ownership_interest ~ stock_opt
ggplot(data=train_physicians, aes(y=stock_opt,x=ownership_interest.f))+ geom_violin() + labs(title=" ",y="stock_opt", x = " ")

ggplot(train_physicians, aes(stock_opt, fill=ownership_interest.f)) + geom_density(alpha=.6) +
  scale_fill_manual(values = c('lightgreen','coral'))

# ownership_interest ~ ownership
ggplot(data=train_physicians, aes(y=ownership,x=ownership_interest.f))+ geom_violin() + labs(title=" ",y="ownership", x = " ")

ggplot(train_physicians, aes(ownership, fill=ownership_interest.f)) + geom_density(alpha=.6) +
  scale_fill_manual(values = c('lightgreen','coral'))

# ownership_interest ~ dividend
ggplot(data=train_physicians, aes(y=dividend,x=ownership_interest.f))+ geom_violin() + labs(title=" ",y="dividend", x = " ")

ggplot(train_physicians, aes(dividend, fill=ownership_interest.f)) + geom_density(alpha=.6) +
  scale_fill_manual(values = c('lightgreen','coral'))

# ownership_interest ~ stock_or_other
ggplot(data=train_physicians, aes(y=stock_or_other,x=ownership_interest.f))+ geom_violin() + labs(title=" ",y="stock_or_other", x = " ")

ggplot(train_physicians, aes(stock_or_other, fill=ownership_interest.f)) + geom_density(alpha=.6) +
  scale_fill_manual(values = c('lightgreen','coral'))



# ownership_interest ~ max_of_payment_count
#ggplot(data=train_physicians, aes(y=max_of_payment_count,x=ownership_interest.f))+ geom_violin() + labs(title=" ",y="range", x = " ")

#ggplot(train_physicians, aes(log(max_of_payment_count), fill=ownership_interest.f)) + geom_density(alpha=.6) +
 # scale_fill_manual(values = c('lightgreen','coral'))

# ownership_interest ~ min_of_payment_count
#ggplot(data=train_physicians, aes(y=min_of_payment_count,x=ownership_interest.f))+ geom_violin() + labs(title=" ",y="range", x = " ")

#ggplot(train_physicians, aes(log(min_of_payment_count), fill=ownership_interest.f)) + geom_density(alpha=.6) +
 # scale_fill_manual(values = c('lightgreen','coral'))


ps = train_physicians$Primary_Specialty
oi = train_physicians$ownership_interest
nature = train_physicians$nature
total_payments = log(train_physicians$total_payments)
number_of_payments = log(train_physicians$number_of_payments)
range = log(train_physicians$range)
payment_range = log(train_physicians$number_of_payment_range)
companys = train_physicians$companys
company_sd = log(train_physicians$sd)
rpi = train_physicians$rpi
rpi_count = log(train_physicians$rpi_count)
fop = train_physicians$form_of_payment
fop_count = train_physicians$fop_count
cash = train_physicians$cash
services = train_physicians$services
stock = train_physicians$stock   
stock_opt = train_physicians$stock_opt
dividend = train_physicians$dividend
stock_or_other = train_physicians$stock_or_other
top_company = train_physicians$Company_ID



log.reg = glm(oi  ~ ps + nature + total_payments + number_of_payments + range + 
                payment_range  + companys + company_sd + rpi_count + rpi + fop +
                fop_count + cash + services+ stock + stock_opt + dividend +
                stock_or_other + top_company,
           family=binomial(link = "logit"))

summary(log.reg)

MCFad = 1 - (log.reg$deviance/log.reg$null.deviance)

MCFad	

ps = valid_physicians$Primary_Specialty
nature = valid_physicians$nature
total_payments = log(valid_physicians$total_payments)
number_of_payments = log(valid_physicians$number_of_payments)
range = log(valid_physicians$range)
payment_range = log(valid_physicians$number_of_payment_range)
companys = valid_physicians$companys
company_sd = log(valid_physicians$sd)
rpi = valid_physicians$rpi
rpi_count = log(valid_physicians$rpi_count)
fop = valid_physicians$form_of_payment
fop_count = valid_physicians$fop_count
cash = valid_physicians$cash
services = valid_physicians$services
stock = valid_physicians$stock   
stock_opt = valid_physicians$stock_opt
dividend = valid_physicians$dividend
stock_or_other = valid_physicians$stock_or_other
top_company = valid_physicians$Company_ID



myinstances = data.frame(ps,nature,total_payments,number_of_payments,range,
                         payment_range,companys,company_sd,rpi_count,fop,
                         fop_count,cash, services, stock, stock_opt, dividend,
                         stock_or_other,top_company)

sum(is.na(myinstances))
#log.reg$xlevels[["top_company"]]
#top_company
#log.reg$xlevels[["top_company"]] <- union(log.reg$xlevels[["top_company"]], top_company)

#myinstances$ownership = predict(log.reg , newdata=myinstances, type="response")
#names(myinstances)

?predict
preds = predict(log.reg, newdata=myinstances, type='response',allow.new.levels=TRUE)
length(preds)

cm = table(true=valid_physicians$ownership_interest, prediction=round(preds))
addmargins(cm)


