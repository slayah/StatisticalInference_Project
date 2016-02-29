data("ToothGrowth")

str(ToothGrowth)

head(ToothGrowth)

summary(ToothGrowth)


plot(dose~len,data = ToothGrowth,pch=19)

plot(as.numeric(supp)~len,data = ToothGrowth,pch=19)

boxplot(len~supp,data = ToothGrowth,col="red")

boxplot(len~dose,data = ToothGrowth,col="pink")

hist(as.numeric(ToothGrowth$len),col = "blue",breaks=15)

t.test(len ~ supp, ToothGrowth[ToothGrowth$dose == .5, ])

t.test(len ~ supp, ToothGrowth[ToothGrowth$dose == 1, ])

t.test(len ~ supp, ToothGrowth[ToothGrowth$dose == 2, ])










