##########################################
library(readr)
read.csv("../data/lab_sodra.csv")
head(lab_sodra)
library(dplyr)
Filtras = filter(lab_sodra,ecoActCode==471900)
head(Filtras)
####################################
#1 uzduotis#
hist(Filtras$avgWage, xlab = "Average Wage", ylab= "Frequency", breaks =20, col = "steelblue", main = "Average Wage")
#################################################
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)
#2 uzduotis#
Top5 = Filtras %>%
  group_by(name)%>%
  slice_max(avgWage, n=1)%>%
  ungroup()%>%
  top_n(avgWage, n=5)

Top51 <- Filtras %>%
  filter(name %in% Top5$name)


ggplot(Top51, aes(x=month, y=avgWage, color=name)) +
  geom_line() +
  scale_color_manual(values = c("red", "blue", "green", "purple", "gold")) +
  labs(x = "Month", y = "Average Wage", title = "Average Wage by Month for Top 5 Companies") +
  theme_minimal() +
  theme(legend.position = "right", 
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14))

###################################################
#3 uzduotis#

Top51 %>%
  group_by(name)%>%
  slice_max(numInsured,n=1)%>%
  top_n(numInsured, n=5)

ggplot(Top51, aes(x=reorder(name, -numInsured), y=numInsured))+geom_col(fill = "steelblue") +
  theme_minimal()+
  labs(title = "Number of insured employees", x="Company", Y="Count")


######################################################################

