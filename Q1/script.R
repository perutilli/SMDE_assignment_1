# import the dataframe
df = read.csv("decathlon.csv", stringsAsFactors = TRUE)
df$Rank = as.factor(df$Rank)
summary(df)

# Point a
boxplot(X100m ~ Competition, data=df)

# Point b
X100m_cat_test = sapply(df[, 'X100m'], (function(x) if(x < 11) "Low" else "High"))
df[['X100m_cat']] = sapply(X100m_cat_test, as.factor)

# perform chi squared test
chisq.test(df[, 'Competition'], X100m_cat)
# given the very low p value they are indipendent

# Point c

hist(df[, "X100m"]) # prob normal
shapiro.test(df$X100m)
hist(df[, "Long.jump"]) # normal
shapiro.test(df$Long.jump)
hist(df[, "Shot.put"]) # normal
shapiro.test(df$Shot.put)
hist(df[, "High.jump"]) # not normal
shapiro.test(df$High.jump)
hist(df[, "X400m"]) # not normal
shapiro.test(df$X400m)
hist(df[, "X110m.hurdle"]) # not normal
shapiro.test(df$X110m.hurdle)
hist(df[, "Discus"]) # maybe normal
shapiro.test(df$Discus) # not normal
hist(df[, "Pole.vault"]) # not normal
shapiro.test(df$Pole.vault)
hist(df[, "Javeline"]) # not normal
shapiro.test(df$Javeline)
hist(df[, "X1500m"]) # not normal
shapiro.test(df$X1500m)
hist(df[, "Points"]) # not normal
shapiro.test(df$Points)
