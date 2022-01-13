# TESTE DE QUI QUADRADO PARA K PROPORÇÕES

# To compare k ( > 2) proportions there is a test based on the normal 
# approximation. It consists of the calculation of a weighted sum of 
# squared deviations between the observed proportions in each group and 
# the overall proportion for all groups. The test statistic has an 
# approximate c2 distribution with k −1 degrees of freedom.

# To use prop.test on a table with multiple categories or groups, we need 
# to convert it to a vector of "successes" and a vector of "trials", one 
# for each group. In the data, each group has multiple levels of 
# success and insuccess, so we need to total the number of cases and 
# controls for each group.

# Base de Dados
n=c("AVE"=24,"CT"=10)
p1=c("AVE"=4,"CT"=1)
p1=c(4,1)
p2=c(6,0)
p3=c(20,2)
p4=c(6,1)
p5=c(5,1)
p6=c(14,0)
p7=c(6,1)
p9=c(11,0)
p10=c(20,0)

# After this it is easy to perform the test:

prop.test(p1,n)
prop.test(p2,n)
prop.test(p3,n)
prop.test(p4,n)
prop.test(p5,n)
prop.test(p6,n)
prop.test(p7,n)
prop.test(p9,n)
prop.test(p10,n)
# H0: The proportion of cases is the same in each age group: 
# p1 = p2 = p3 = p4 = p5 = p6
# Ha: The proportion of cases is not the same in each age group: 
# at least one pi is different from the others


# You can test for a linear trend in the proportions using prop.trend.test.
# The null hypothesis is that there is no trend in the proportions; 
# the alternative is that there is a linear increase/decrease in the 
# proportion as you go up/down in categories. Note: you would only want 
# to perform this test if your categorical variable was an ordinal 
# variable. You would not do this, for, say, political party affiliation 
# or eye color.

prop.trend.test(p1,n)

# H0: There is no linear trend in the proportion of cases across age groups
# Ha: There is a linear trend in the proportion of cases across age groups