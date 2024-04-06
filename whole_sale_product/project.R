library(readr)
data <- read_csv("Downloads/Wholesale_customers_data.csv")

library(Hmisc)
describe(data) # no missing data
# all data distributed to region 3 throught out channel 1 and 2

data$channel1 <- as.integer(data$Channel == 1)
data$channel2 <- as.integer(data$Channel == 2)
sum(data$channel1) # channel1 : 298. more distributed to through channel1. 
sum(data$channel2) # channel2 : 142
# but how much of them ?

# TOTAL EACH PRODUCT
sum(data$Fresh) # Fresh product : 5280131
sum(data$Milk) # Milk : 2550357
sum(data$Grocery) # Grocery : 3498562
sum(data$Frozen) # Frozen product : 1351650
sum(data$Detergents_Paper) # Detergen paper : 1267857
sum(data$Delicassen) # Delicassen :  670943
# Fresh product is the most distributed items
# Delicassen is the least distributed product 
# Fresh product, grocery, milk, frozen product, detergen paper, delicassen


# MILK
total_milk <- sum(data$Milk) # 2550357 total milk distributed
total_milk1 = subset(data, channel1 == 1)
total_milk2 = subset(data, channel2 == 1)
milk1 <- sum(total_milk1$Milk) # distributed milk to channel 1 with less amount : 1028614
milk2 <- sum(total_milk2$Milk) # distributed milk to channel 2 with more amount : 1521743
mean(total_milk1$Milk) # 3451.7
mean(total_milk2$Milk) # 10716.5
t.test(total_milk1$Milk, total_milk2$Milk, alternative = "two.sided", conf.level = 0.95)
# channel 2 get more milk than channel 1
# negative t-value means the mean of total_milk1 less than the mean of total_milk2
# p-value is 7.513e-15 very small and close to zero ~0 . strong evidence against the null hypothesis
# the null-hypothesis : the true difference in means between the two samples is equal to zero
# since the null-hypothesis very small, we reject the null hypothesis in favor of alternative hypothesis
# confidence interval : since the interval doesn't contain 0, it indicates that the diff.
# in means is statistically significant

library(ggplot2)
# display histogram from Milk product and the median
ggplot(data, aes(x=Milk)) + geom_histogram(color="blue", fill="lightblue", bins=20) + 
            geom_vline(xintercept = median(data$Milk)) +
            labs(x="Milk", y="Total", title="Total Distributed Milk")

# display the Grocery between Channel 1 and 2
ggplot(data, aes(x=Grocery, fill=Channel)) + 
    geom_histogram(bins=20, color="black") + 
    labs(x="Grocery", y="Total", title="Distributed Grocery by Channel") +
    facet_wrap(~Channel)

# comparing the fresh and frozen product with the different channel
ggplot(data, aes(x=Fresh, y=Frozen, color=Channel)) + 
    geom_point() +
    labs(x="Fresh", y="Frozen", title="Distributed Fresh vs. Frozen Product")

# comparing the fresh and delicassen product with the different of channel
ggplot(data, aes(x=Fresh, y=Delicassen, color=Channel)) + 
      geom_point() +
      labs(x="Fresh", y="Frozen", title="Fresh vs. Frozen") +
      geom_smooth(method="lm") +
      facet_wrap(~Channel)
      

# comparing Milk product in two different Channel
# Product below mostly distributed to channel1
ggplot(data, aes(x=Fresh, y=Channel, color=Channel)) + geom_point() 
ggplot(data, aes(x=Frozen, y=Channel, color=Channel)) + geom_point()
ggplot(data, aes(x=Delicassen, y=Channel, color=Channel)) + geom_point()

# Products below mostly distributed to channel2
ggplot(data, aes(x=Milk, y=Channel, color=Channel)) + geom_point()
ggplot(data, aes(x=Grocery, y=Channel, color=Channel)) + geom_point()
ggplot(data, aes(x=Detergents_Paper, y=Channel, color=Channel)) + geom_point()
sum(data$Grocery)

