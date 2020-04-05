library(tidyverse)
populationSize = 5000
prevalence = 0.0148
truePositive = 50
falseNegative = 24
falsePositive = 443
trueNegative = 4483

pts = runif(populationSize,0, 100)
n = round(populationSize*prevalence)
n1 = round(truePositive) # truePositive
n2 = round(falseNegative) #n - n1 # falseNegative
n3 = round(falsePositive) # falsePositive
n4 = round(trueNegative) # populationSize - n1 - n2 -n3 #trueNegative
positivePredictiveValue = truePositive / (truePositive + falsePositive)
negativePredictiveValue = trueNegative / (trueNegative + falseNegative)

status1 = ifelse(1:n %in% sample(1:n,n1),"True Positive","False Negative")
status2 = ifelse((n+1):populationSize %in% sample((n+1):populationSize,n4),"True Negative","False Positive")
status = c(status1, status2)
df = data.frame(index = 1:populationSize, pts = pts, status = status)
ggplot(df, aes_string(x="index", y="pts", color="status")) + geom_point() +
  annotate("rect", xmin = 0, xmax = n, ymin = 0, ymax = 100, alpha = .2) + theme(legend.position = "bottom") +
  scale_color_manual(values=c("#333333","#F8766D","#619CFF","#00BA38"))


df %>% group_by(status) %>% summarise(n_distinct(index))



