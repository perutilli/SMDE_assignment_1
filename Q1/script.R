# import the dataframe
df = read.csv("decathlon.csv", stringsAsFactors = TRUE)
summary(df)

# Point a
boxplot(X100m ~ Competition, data=df)

# Point b
X100m_cat <- cut(df[, 'X100m'], breaks = c(-Inf,11,Inf), labels = c("Low","High"))
df[['X100m_cat']] = X100m_cat

# perform chi squared test
chisq.test(df[, 'Competition'], X100m_cat)
# given the very low p value they are indipendent

