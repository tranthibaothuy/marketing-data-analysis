datasubset <- read.csv("datasubset.csv")
datasubset <- datasubset[,2:10]

# Calculate the frequency and contingency of categorical variables 

# Functions:
# table() _ Create an N-way contingency table from N categorical variables(factor)
# xtabs() _ Create an N-way contingency table on a formula and a matrix or data frame
# prop.table(table, margins)_ Expresses table entries as fractions of the marginal table defined by the margins
# margin.table(table, margins)_Computers the sum of table entries for a marginal table defined by the margin
# addmargins(table, margins)_ Puts summary margins (sum by default) on a table
# ftable(table)_ Create a compact "flat" contingency table

library(vcd)
my_contigency_table <- xtabs(~y+housing+loan, data=datasubset)
print(my_contigency_table)

margin.table(my_contigency_table, 1)

prop.table(my_contigency_table, 1)

addmargins(prop.table(my_contigency_table, 1),1)

addmargins(prop.table(my_contigency_table, 1),2)

ftable(addmargins(prop.table(my_contigency_table, c(1,2)),3))*100


# Two-way table using CrossTable
install.packages("gmodels")
library(gmodels)
CrossTable(datasubset$y, datasubset$loan)

# Chi-square test of Independence to a two-way table

mytable <- xtabs(~y+housing, data=datasubset)
chisq.test(mytable)


# Fisher's Exact Test
fisher.test(mytable)

# Cochran-Mantel-Haenszel Test
mytable <- xtabs(~y+housing+default, data=datasubset)
mantelhaen.test(mytable)

# Measures of Association
mytable <- xtabs(~y+loan, data=datasubset)
assocstats(mytable)

#-------------------------------------