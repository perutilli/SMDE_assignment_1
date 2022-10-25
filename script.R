# QUESTION 1

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
shapiro.test(df$X100m) # normal
hist(df[, "Long.jump"]) # normal
shapiro.test(df$Long.jump) # normal
hist(df[, "Shot.put"]) # normal
shapiro.test(df$Shot.put) # normal
hist(df[, "High.jump"]) # not normal
shapiro.test(df$High.jump) # not normal
hist(df[, "X400m"]) # normal
shapiro.test(df$X400m) # normal
hist(df[, "X110m.hurdle"]) # not normal
shapiro.test(df$X110m.hurdle) # not normal
hist(df[, "Discus"]) # maybe normal
shapiro.test(df$Discus) # normal
hist(df[, "Pole.vault"]) # not normal
shapiro.test(df$Pole.vault)
hist(df[, "Javeline"]) # maybe normal
shapiro.test(df$Javeline) # normal
hist(df[, "X1500m"]) # not normal
shapiro.test(df$X1500m)
hist(df[, "Points"]) # normal
shapiro.test(df$Points) # normal

# Point d

d1 = rnorm(50, mean = 0, sd = 2)
d2 = rnorm(50, mean = 0, sd = 1)
d3 = rnorm(50, mean = 1, sd = 2)

t.test(d1, d2)
t.test(d1, d3)
t.test(d3, d2)

# Point e

t.test(df$X100m, df$X400m)


# QUESTION 2

d1 = rnorm(100, mean = 10, sd = 5)
d2 = rnorm(100, mean = 40, sd = 5)
d3 = rnorm(100, mean = 10, sd = 5)

hist(d1)
hist(d2)
hist(d3)




