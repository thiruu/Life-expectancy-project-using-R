file.choose()
DMproj <- read.csv("C:\\Users\\Roshan\\Desktop\\MSM\\Winter '16\\Data Mining\\Project\\my proj.csv", header = TRUE)
str(DMproj)

# To check high correlation
DMproj.cor <- subset(DMproj, select = -c(1, 2))
cor(DMproj.cor)
str(DMproj.cor)
economic.var.names <- c(1:20)
pairs(DMproj.cor[,economic.var.names])
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = pmax(1, cex.cor * r))
}
pairs(DMproj.cor[,economic.var.names], lower.panel = panel.cor)

# Regression analysis

proj.lm <- lm(DMproj$Life.Exp ~ DMproj$Region + DMproj$Access.to.water + DMproj$Access.to.sanitation... + DMproj$Alcohol.consumption.Ls.person.year. + DMproj$Tobacco.use.15.... + DMproj$Tobacco.use.15.....1 + DMproj$Physicians.Density..per.10k. + DMproj$Nursing.desnity..per.10k. + DMproj$Beds..per.10k. + DMproj$Expenditure.on.health....of.GDP. + DMproj$Per.capita.expenditure.on.health.... + DMproj$Total.Population...000s. + DMproj$Median.age + DMproj$Over.60.... + DMproj$Annual.growth.rate.... + DMproj$Living.in.urban.... + DMproj$Total.fertility.rate..per.woman. + DMproj$Adolescent.fertility.rate..per.1000.women. + DMproj$Literacy.rate.... + DMproj$National.income.per.capita....)
summary(proj.lm)

# Regression with all the variables except State
proj.lm1 <- lm(Life.Exp ~ . - State, data = DMproj)
summary(proj.lm1)

# Removing the most insignificant from proj.lm1 - DMproj$Tobacco.use.15.....1
proj.lm2 <- lm(Life.Exp ~ . - State - Tobacco.use.15.....1, data = DMproj)
summary(proj.lm2)

# Removing the most insignificant from proj.lm2 - Literacy.rate.... 
proj.lm3 <- lm(Life.Exp ~ . - State - Tobacco.use.15.....1 - Literacy.rate...., data = DMproj)
summary(proj.lm3)

# Removing the most insignificant from proj.lm3 - Total.Population...000s.
proj.lm4 <- lm(Life.Exp ~ . - State - Tobacco.use.15.....1 - Literacy.rate.... - Total.Population...000s., data = DMproj)
summary(proj.lm4)

# Removing the most insignificant from proj.lm3 - Total.Population...000s.
proj.lm4 <- lm(Life.Exp ~ . - State - Tobacco.use.15.....1 - Literacy.rate.... - Total.Population...000s., data = DMproj)
summary(proj.lm4)

# Removing the most insignificant from proj.lm4 - National.income.per.capita....
proj.lm5 <- lm(Life.Exp ~ . - State - Tobacco.use.15.....1 - Literacy.rate.... - Total.Population...000s. - National.income.per.capita...., data = DMproj)
summary(proj.lm5)

# Removing the most insignificant from proj.lm5 - Beds..per.10k.
proj.lm6 <- lm(Life.Exp ~ . - State - Tobacco.use.15.....1 - Literacy.rate.... - Total.Population...000s. - National.income.per.capita.... - DMproj$Beds..per.10k., data = DMproj)
summary(proj.lm6)

# Removing the most insignificant from proj.lm6 - Beds..per.10k.
proj.lm7 <- lm(Life.Exp ~ . - State - Tobacco.use.15.....1 - Literacy.rate.... - Total.Population...000s. - National.income.per.capita.... - Beds..per.10k., data = DMproj)
summary(proj.lm7)

# Removing the most insignificant from proj.lm7 - Expenditure.on.health....of.GDP.
proj.lm8 <- lm(Life.Exp ~ . - State - Tobacco.use.15.....1 - Literacy.rate.... - Total.Population...000s. - National.income.per.capita.... - Beds..per.10k. - Expenditure.on.health....of.GDP., data = DMproj)
summary(proj.lm8)

# Removing the most insignificant from proj.lm8 - Nursing.desnity..per.10k. 
proj.lm9 <- lm(Life.Exp ~ . - State - Tobacco.use.15.....1 - Literacy.rate.... - Total.Population...000s. - National.income.per.capita.... - Beds..per.10k. - Expenditure.on.health....of.GDP. - Nursing.desnity..per.10k., data = DMproj)
summary(proj.lm9)

# Removing the most insignificant from proj.lm9 - Access.to.water 
proj.lm10 <- lm(Life.Exp ~ . - State - Tobacco.use.15.....1 - Literacy.rate.... - Total.Population...000s. - National.income.per.capita.... - Beds..per.10k. - Expenditure.on.health....of.GDP. - Nursing.desnity..per.10k. - Access.to.water, data = DMproj)
summary(proj.lm10)

# Removing the most insignificant from proj.lm10 - Median.age
proj.lm11 <- lm(Life.Exp ~ . - State - Tobacco.use.15.....1 - Literacy.rate.... - Total.Population...000s. - National.income.per.capita.... - Beds..per.10k. - Expenditure.on.health....of.GDP. - Nursing.desnity..per.10k. - Access.to.water - Median.age, data = DMproj)
summary(proj.lm11)

# Removing the most insignificant from proj.lm11 - Tobacco.use.15.... 
proj.lm12 <- lm(Life.Exp ~ . - State - Tobacco.use.15.....1 - Literacy.rate.... - Total.Population...000s. - National.income.per.capita.... - Beds..per.10k. - Expenditure.on.health....of.GDP. - Nursing.desnity..per.10k. - Access.to.water - Median.age - Tobacco.use.15...., data = DMproj)
summary(proj.lm12)

# Removing the most insignificant from proj.lm12 - Physicians.Density..per.10k. 
proj.lm13 <- lm(Life.Exp ~ . - State - Tobacco.use.15.....1 - Literacy.rate.... - Total.Population...000s. - National.income.per.capita.... - Beds..per.10k. - Expenditure.on.health....of.GDP. - Nursing.desnity..per.10k. - Access.to.water - Median.age - Tobacco.use.15.... - Physicians.Density..per.10k., data = DMproj)
summary(proj.lm13)

# Removing the most insignificant from proj.lm13 - Access.to.sanitation... 
proj.lm14 <- lm(Life.Exp ~ . - State - Tobacco.use.15.....1 - Literacy.rate.... - Total.Population...000s. - National.income.per.capita.... - Beds..per.10k. - Expenditure.on.health....of.GDP. - Nursing.desnity..per.10k. - Access.to.water - Median.age - Tobacco.use.15.... - Physicians.Density..per.10k. - Access.to.sanitation..., data = DMproj)
summary(proj.lm14)

# Removing the most insignificant from proj.lm14 - Latitude 
proj.lm15 <- lm(Life.Exp ~ . - State - Tobacco.use.15.....1 - Literacy.rate.... - Total.Population...000s. - National.income.per.capita.... - Beds..per.10k. - Expenditure.on.health....of.GDP. - Nursing.desnity..per.10k. - Access.to.water - Median.age - Tobacco.use.15.... - Physicians.Density..per.10k. - Access.to.sanitation... - Latitude, data = DMproj)
summary(proj.lm15)

# Removing the most insignificant from proj.lm15 - Alcohol.consumption.Ls.person.year. 
proj.lm16 <- lm(Life.Exp ~ . - State - Tobacco.use.15.....1 - Literacy.rate.... - Total.Population...000s. - National.income.per.capita.... - Beds..per.10k. - Expenditure.on.health....of.GDP. - Nursing.desnity..per.10k. - Access.to.water - Median.age - Tobacco.use.15.... - Physicians.Density..per.10k. - Access.to.sanitation... - Latitude - Alcohol.consumption.Ls.person.year., data = DMproj)
summary(proj.lm16)

# After excluding all insignificant variables, reading the new file with just the significant variables.
postreg <- read.csv("C:\\Users\\Roshan\\Desktop\\MSM\\Winter '16\\Data Mining\\Project\\proj_postreg.csv", header = TRUE)
str(postreg)

# Cluster analysis
postreg$State <- NULL
postreg$Region <- NULL

# Determine number of cluster
wss <- (nrow(postreg)-1)*sum(apply(postreg,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(postreg, centers = i)$withinss)

plot(1:20, wss, type = "b", xlab = "Number of clusters", ylab = "Within groups SS")

# kmeans
kmeans.proj <- kmeans(postreg, 4)
kmeans.proj


# Question 1 analysis
proj.lm17 <- lm(DMproj$Life.Exp ~ DMproj$Region * DMproj$Annual.growth.rate.... )
summary(proj.lm17)

anova(update(proj.lm16, . ~ . - Annual.growth.rate....), update(proj.lm16, . ~ .), test = "Chisq")

library(cluster)
clusplot(postreg, kmeans.proj$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

table.with.clusters <- cbind(postreg, kmeans.proj$cluster)
write.csv(table.with.clusters, file = "proj cluster")
