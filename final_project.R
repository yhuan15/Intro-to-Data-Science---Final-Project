
setwd("./R-studio")      # Relative path
getwd()

library(dplyr)
library(tidyverse)
library(ggplot2)
library(cluster)
library(factoextra)
library(scatterplot3d)
library(ISLR)

# Read in files
happy19 <- read.csv("./data/2019.csv") 
# Exploratory Data Analysis
sum(is.na(happy19)) # Check for missing values
# [1] 0 
# No missing values
str(happy19)
# 'data.frame':	156 obs. of  9 variables:
#   $ Overall.rank                : int  1 2 3 4 5 6 7 8 9 10 ...
# $ Country.or.region           : chr  "Finland" "Denmark" "Norway" "Iceland" ...
# $ Score                       : num  7.77 7.6 7.55 7.49 7.49 ...
# $ GDP.per.capita              : num  1.34 1.38 1.49 1.38 1.4 ...
# $ Social.support              : num  1.59 1.57 1.58 1.62 1.52 ...
# $ Healthy.life.expectancy     : num  0.986 0.996 1.028 1.026 0.999 ...
# $ Freedom.to.make.life.choices: num  0.596 0.592 0.603 0.591 0.557 0.572 0.574 0.585 0.584 0.532 ...
# $ Generosity                  : num  0.153 0.252 0.271 0.354 0.322 0.263 0.267 0.33 0.285 0.244 ...
# $ Perceptions.of.corruption   : num  0.393 0.41 0.341 0.118 0.298 0.343 0.373 0.38 0.308 0.226 ...
summary(happy19)
# Overall.rank    Country.or.region      Score       GDP.per.capita  
# Min.   :  1.00   Length:156         Min.   :2.853   Min.   :0.0000  
# 1st Qu.: 39.75   Class :character   1st Qu.:4.545   1st Qu.:0.6028  
# Median : 78.50   Mode  :character   Median :5.380   Median :0.9600  
# Mean   : 78.50                      Mean   :5.407   Mean   :0.9051  
# 3rd Qu.:117.25                      3rd Qu.:6.184   3rd Qu.:1.2325  
# Max.   :156.00                      Max.   :7.769   Max.   :1.6840  
# Social.support  Healthy.life.expectancy Freedom.to.make.life.choices
# Min.   :0.000   Min.   :0.0000          Min.   :0.0000              
# 1st Qu.:1.056   1st Qu.:0.5477          1st Qu.:0.3080              
# Median :1.272   Median :0.7890          Median :0.4170              
# Mean   :1.209   Mean   :0.7252          Mean   :0.3926              
# 3rd Qu.:1.452   3rd Qu.:0.8818          3rd Qu.:0.5072              
# Max.   :1.624   Max.   :1.1410          Max.   :0.6310 
# Generosity     Perceptions.of.corruption
# Min.   :0.0000   Min.   :0.0000           
# 1st Qu.:0.1087   1st Qu.:0.0470           
# Median :0.1775   Median :0.0855           
# Mean   :0.1848   Mean   :0.1106           
# 3rd Qu.:0.2482   3rd Qu.:0.1412           
# Max.   :0.5660   Max.   :0.4530  

# Data Cleansing and Manipulation
# change the column names to make it easier for data manipulation
names(happy19) <- c("Happiness_Rank","Country","Happiness_Score","GDP_per_Capita","Social_Support",
                  "Healthy_Life_Expectancy","Freedom","Generosity","Government_Corruption")
# Creating two other columns: Year and Continent
happy19$Year <- 2019
happy19$Year <- as.factor(happy19$Year)
happy19$Continent <- NA
happy19$Continent[which(happy19$Country %in% c("Israel", "United Arab Emirates", "Singapore", "Thailand", "Taiwan Province of China",
                                                             "Qatar", "Saudi Arabia", "Kuwait", "Bahrain", "Malaysia", "Uzbekistan", "Japan",
                                                             "South Korea", "Turkmenistan", "Kazakhstan", "Turkey", "Hong Kong S.A.R., China", "Philippines",
                                                             "Jordan", "China", "Pakistan", "Indonesia", "Azerbaijan", "Lebanon", "Vietnam",
                                                             "Tajikistan", "Bhutan", "Kyrgyzstan", "Nepal", "Mongolia", "Palestinian Territories",
                                                             "Iran", "Bangladesh", "Myanmar", "Iraq", "Sri Lanka", "Armenia", "India", "Georgia",
                                                             "Cambodia", "Afghanistan", "Yemen", "Syria"))] <- "Asia"
happy19$Continent[which(happy19$Country %in% c("Norway", "Denmark", "Iceland", "Switzerland", "Finland",
                                                             "Netherlands", "Sweden", "Austria", "Ireland", "Germany",
                                                             "Belgium", "Luxembourg", "United Kingdom", "Czech Republic",
                                                             "Malta", "France", "Spain", "Slovakia", "Poland", "Italy",
                                                             "Russia", "Lithuania", "Latvia", "Moldova", "Romania",
                                                             "Slovenia", "North Cyprus", "Cyprus", "Estonia", "Belarus",
                                                             "Serbia", "Hungary", "Croatia", "Kosovo", "Montenegro",
                                                             "Greece", "Portugal", "Bosnia and Herzegovina", "Macedonia",
                                                             "Bulgaria", "Albania", "Ukraine"))] <- "Europe"
happy19$Continent[which(happy19$Country %in% c("Canada", "Costa Rica", "United States", "Mexico",  
                                                             "Panama","Trinidad and Tobago", "El Salvador", "Belize", "Guatemala",
                                                             "Jamaica", "Nicaragua", "Dominican Republic", "Honduras",
                                                             "Haiti"))] <- "North America"
happy19$Continent[which(happy19$Country %in% c("Chile", "Brazil", "Argentina", "Uruguay",
                                                             "Colombia", "Ecuador", "Bolivia", "Peru",
                                                             "Paraguay", "Venezuela"))] <- "South America"
happy19$Continent[which(happy19$Country %in% c("New Zealand", "Australia"))] <- "Australia"
happy19$Continent[which(is.na(happy19$Continent))] <- "Africa"
class(happy19$Continent)
# [1] "character"
# coerce the new variable to a factor 
happy19$Continent <- factor(happy19$Continent)
class(happy19$Continent)
# [1] "factor"

# select variables necessary to achieve the desired goals
happiness <- happy19 %>% 
  select(3:9)
head(happiness)
# Explore the data: select the features which are highly dependent on the response.
cor(happiness, method="pearson")
pairs(happiness)   # a plot matrix, consisting of scatterplots for each variable-combination 
X0 <- chisq.test(happy19$Happiness_Score, happy19$Generosity)
X0
# 	Pearson's Chi-squared test

# data:  happy19$Happiness_Score and happy19$Generosity
# X-squared = 18096, df = 18018, p-value = 0.3395
X <- chisq.test(happy19$Happiness_Score, happy19$Government_Corruption)
X
# Pearson's Chi-squared test
#
# data:  happy19$Happiness_Score and happy19$Government_Corruption
# X-squared = 17355, df = 17248, p-value = 0.2815
# Comment: There does not exist a positive relationship between happiness score and Government_Corruption because P-value < 0.05
# Comment: Most variables correlate highly/moderately with Happiness Score since adding the variables together provides the Happiness Score.
# Comment: However, no significant correlations between generosity/government corruption and happiness score.

# Comment: look at the internal relationships in a little more detail. I chose a couple of variables that interest me: GDP per capita, healthy life expectancy, and social support.
lm1 <- lm(Healthy_Life_Expectancy ~ GDP_per_Capita, data = happy19) 
summary(lm1)
with(happy19,plot(Healthy_Life_Expectancy ~ GDP_per_Capita, pch=19,
                  col="blue", cex=0.5))
# Draw the regression line through the distribution
abline(lm1,col="red",lwd=3)
# Comment: There is a positive relationship between life expectancy and Economy (GDP per capita). Probably it is because better economic condition generally results in longer life expectancy because of the more advanced medical services.
# Comment: Further explore if the economic growth would increase the rate of social support
lm2 <- lm(Social_Support ~ GDP_per_Capita, data = happy19) 
summary(lm2)
with(happy19,plot(Social_Support ~ GDP_per_Capita, pch=19,
                     col="blue", cex=0.5))
abline(lm2,col="red",lwd=3)
# Comment: with a better economic condition/GDP, there will e higher rate of social support.

# Compare different continents regarding individual variable
# select the variables necessary to achieve my goals
con <- happy19 %>%
  select(Continent, Happiness_Score, GDP_per_Capita, Social_Support, Healthy_Life_Expectancy, Freedom, Generosity,Government_Corruption) %>%
  group_by(Continent) %>%
  summarise_if(is.numeric, function(x) mean(x)) %>%
  arrange(desc(Happiness_Score))
con
# A tibble: 6 x 8
# Continent Happiness_Score GDP_per_Capita Social_Support Healthy_Life_Ex… Freedom
# <fct>               <dbl>          <dbl>          <dbl>            <dbl>   <dbl>
# 1 Australia            7.27          1.34           1.55             1.03    0.571
# 2 Europe               6.29          1.23           1.43             0.925   0.409
# 3 North Am…            6.15          0.929          1.32             0.825   0.455
# 4 South Am…            5.92          0.983          1.38             0.834   0.434
# 5 Asia                 5.21          0.951          1.19             0.748   0.403
# 6 Africa               4.52          0.578          0.975            0.494   0.341
# … with 2 more variables: Generosity <dbl>, Government_Corruption <dbl>
install.packages("reshape2")
library(reshape2)
# reshape.melt data so I can facet wrap based on the variables.
conmelt <- melt(con)
# Plot each variable in bar graphs
ggplot(conmelt, aes(x = Continent, y = value, fill = Continent)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~variable) + theme(axis.text.x = element_blank()) +
  ggtitle('Average Scores per Continent') +
  xlab('') + ylab('')
# showing average score of every variable at a glance in different continents
# Comment: Australia leading in these categories has the highest average happiness score.

# Comparing different continents regarding their happiness score 
# Count countries in each continent
table(happy19$Continent)
#        Africa          Asia     Australia        Europe North America 
#           51            41             2            40            12 
#   South America 
#           10 
# compute percentages of observations represented by each continent
prop.table(table(happy19$Continent))
#        Africa          Asia     Australia        Europe North America 
#    0.32692308    0.26282051    0.01282051    0.25641026    0.07692308 
#  South America 
#    0.06410256 
# visualize the number of countries in each continent by using ggplot(barplot).
ggplot(happy19, aes(Continent, fill = Continent)) +
  geom_bar(stat = "count", show.legend = F, color = 'grey') + 
  scale_fill_brewer(palette = "Blues") + coord_flip() +
  labs(x = "", fill = "Continent", y = "Country Counts", 
       title = "Number of Countries per Continent") + 
  theme_light(base_size = 20)

library(sqldf)
# calculate the average and median happiness score grouped by Continent
sqldf("select Continent, avg(Happiness_Score) from happy19 group by Continent") 
#       Continent     avg(Happiness_Score)
# 1        Africa             4.518216
# 2          Asia             5.214341
# 3     Australia             7.267500
# 4        Europe             6.293350
# 5 North America             6.151583
# 6 South America             5.920200
sqldf("select Continent, median(Happiness_Score) from happy19 group by Continent")
# Continent median(Happiness_Score)
# 1        Africa                  4.5090
# 2          Asia                  5.2080
# 3     Australia                  7.2675
# 4        Europe                  6.1655
# 5 North America                  6.2870
# 6 South America                  6.0570

# Visualize median Happiness Score for each continent
r <- happy19 %>%
  select(Continent, `Happiness_Score`) %>%
  group_by(Continent) %>%
  summarise(n = n(),
            mn = mean(`Happiness_Score`),
            md = median(`Happiness_Score`),
            std = sd(`Happiness_Score`))

ggplot(r, aes(x = fct_reorder(Continent, md), y = md, fill = mn)) +
  geom_bar(stat = "Identity", show.legend = F) +
  scale_fill_gradient(low = "purple", high = "pink") +
  labs(x = "", y = "Median Happiness Score", fill = "Happiness_Score",
       title = "Median Happiness Score per Continent") +
  theme_light(base_size = 16) + coord_flip()
# Use boxplot to see the happiness score distribution in different countries and countinents
with(happy19, boxplot(Happiness_Score ~ as.factor(Continent),
                      xlab = "Continent",
                      ylab = "Happiness Score", col=c("lightblue","firebrick","orange", "darkorchid", "lightcoral", "olivedrab")))
# Comment: Australia has the least range and Asia has the widest range of scores. 
sub_au <- with(happy19, 
                 subset(Happiness_Score, Continent == "Australia" ))
str(sub_au)
# num [1:2] 7.31 7.23
# Comment: Australia has the highest average and median happiness score, followed by Europe and North American.
# Comment: Africa has the lowest average and median happiness score. 
# Comment: From the boxplot, we can see the range of happiness score for different continents.
# Comment:there are just two countries in Australia listed in this data file

# Comment: I want to explore more about the relationship between happiness score and Generosity or Corruption 
# I am using linear regression, grouped by each continent.
ggplot(happy19, aes(x = Generosity,y = Happiness_Score, 
                    color = Continent)) + 
  facet_grid(~Continent)+geom_point() + 
  geom_smooth(method = "lm") +
  labs(x="Generosity",y = "Happiness Score")
# Comment: based on the plots, the relationships between happiness and generosity are quite different for each continent. 

ggplot(happy19, aes(x = Government_Corruption,y = Happiness_Score, 
                    color = Continent)) + 
  facet_grid(~Continent)+geom_point() + 
  geom_smooth(method = "lm") +
  labs(x="Government Corruption",y = "Happiness Score")
# Comment: As we can see from the plots, there is a positive correlation between corruption and happiness for most points.
# Comment: However, it is the opposite in Africa. So government corruption is not a good predictor here. 

# Extract & Compare Highest 5 and Worst 5 Countries
High <- happy19 %>% head(5) %>%
  select(Happiness_Rank, Country, Happiness_Score, Continent)
High
#   Happiness_Rank     Country Happiness_Score Continent
# 1              1     Finland           7.769    Europe
# 2              2     Denmark           7.600    Europe
# 3              3      Norway           7.554    Europe
# 4              4     Iceland           7.494    Europe
# 5              5 Netherlands           7.488    Europe
Worst <- happy19 %>% tail(5) %>%
  select(Happiness_Rank, Country, Happiness_Score, Continent)
Worst
#       Happiness_Rank                Country Happiness_Score Continent
# 152            152                   Rwanda           3.334    Africa
# 153            153                 Tanzania           3.231    Africa
# 154            154              Afghanistan           3.203      Asia
# 155            155 Central African Republic           3.083    Africa
# 156            156              South Sudan           2.853    Africa
# Comment: the happiest five countries according to ranked scores are all European countries.
# Comment: the five countries listed with the lowest happiness scores are located in Africa and Asia.

# More Data visulization
# Look at the relationships between happiness score and the other variables through 3D plots.
library(scatterplot3d)
scatterplot3d(happy19$GDP_per_Capita, 
                             happy19$Social_Support, 
                             happy19$Happiness_Score, 
              highlight.3d=TRUE, 
              col.axis="blue", 
              grid=TRUE,
              col.grid="lightblue",
              main="Global Happiness Data", 
              pch=19, 
              xlab="Economy", 
              ylab="Social Support", 
              zlab="Happiness Score")
# Comment: The better economy and social support lead to higher happiness score.
scatterplot3d(happy19$Healthy_Life_Expectancy, 
              happy19$Freedom, 
              happy19$Happiness_Score, 
              highlight.3d=TRUE, 
              col.axis="blue", 
              grid=TRUE,
              col.grid="lightblue",
              main="Global Happiness Data", 
              pch=19, 
              xlab="Health", 
              ylab="Freedom", 
              zlab="Happiness Score")
# Comment: The higher the life expectancy and freedom, the higher the happiness score will be.
scatterplot3d(happy19$Generosity, 
              happy19$Government_Corruption, 
              happy19$Happiness_Score, 
              highlight.3d=TRUE, 
              col.axis="blue", 
              grid=TRUE,
              col.grid="lightblue",
              main="Global Happiness Data", 
              pch=19, 
              xlab="Generosity", 
              ylab="Government Corruption", 
              zlab="Happiness Score")
# Comment: From the 3D scatterplot, lower generosity leads to higher happiness score.
# Comment: There is no significant relationship between government corruption and happiness level.

# Prediction: Logistic Regression
# Transform Happiness Score to Low and High.
# Countries with a score higher than the average Happiness Score are in 1/'High' level, otherwise are in 0/'Low' level.
Happy_Level <- ifelse(happy19$Happiness_Score > mean(happy19$Happiness_Score), yes = 1, no = 0)
Happy <- cbind(happy19, Happy_Level)
# Generate the logistic regression model.
glm01 <-  glm(Happy_Level ~ GDP_per_Capita + Social_Support + Healthy_Life_Expectancy + Freedom + Generosity + Government_Corruption,
              data = Happy,
              family = binomial)
summary(glm01)
# Call:
#   glm(formula = Happy_Level ~ GDP_per_Capita + Social_Support + 
#        Healthy_Life_Expectancy + Freedom + Generosity + Government_Corruption, 
#       family = binomial, data = Happy)

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.1362  -0.2008  -0.0017   0.3253   3.3001
# Coefficients:
#                            Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)              -18.557      3.634  -5.106 3.28e-07 ***
#   GDP_per_Capita             1.311      1.644   0.798  0.42508    
#   Social_Support             5.812      2.022   2.874  0.00405 ** 
#   Healthy_Life_Expectancy    9.066      3.058   2.964  0.00303 ** 
#   Freedom                    8.106      2.603   3.114  0.00185 ** 
#   Generosity                -2.915      3.471  -0.840  0.40112    
#   Government_Corruption      3.393      5.627   0.603  0.54647    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# (Dispersion parameter for binomial family taken to be 1)

# Null deviance: 216.236  on 155  degrees of freedom
# Residual deviance:  78.512  on 149  degrees of freedom
# AIC: 92.512

# Number of Fisher Scoring iterations: 7

# Comment: Only three Variabels (Social Support, Healthy Life Expectancy and Freedom) are significant because the p-values < 0.05.

# Test the reduced logistic regression model
glm02 <-  glm(Happy_Level ~ Social_Support + Healthy_Life_Expectancy + Freedom,
              data = Happy,
              family = binomial)
summary(glm02)

# Call:
# glm(formula = Happy_Level ~ Social_Support + Healthy_Life_Expectancy + 
#       Freedom, family = binomial, data = Happy)

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.2563  -0.1955  -0.0017   0.3579   3.3203  

# Coefficients:
#                             Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)              -19.107      3.527  -5.418 6.03e-08 ***
#   Social_Support             6.262      1.873   3.343 0.000828 ***
#   Healthy_Life_Expectancy   10.691      2.737   3.905 9.41e-05 ***
#   Freedom                    7.448      2.292   3.250 0.001153 ** 
#  ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)

# Null deviance: 216.24  on 155  degrees of freedom
# Residual deviance:  80.78  on 152  degrees of freedom
# AIC: 88.78

# Number of Fisher Scoring iterations: 7

# Comment: All predictors look good in the reduced model at significance level = 0.05.

# Split the data into a training set and test set
n <- nrow(Happy)  
ntrain <- round(n*0.6)  # 60% for training set
set.seed(314)    
tindex <- sample(n, ntrain)   
train_happy <- Happy[tindex,]   # Create training set
test_happy <- Happy[-tindex,]   # Create test set
# Perform logistic regression on the training set in order to predict happiness level
glm_train <-  glm(Happy_Level ~ Social_Support + Healthy_Life_Expectancy + Freedom,
              data = train_happy,
              family = binomial)
summary(glm_train)
# Call:
# glm(formula = Happy_Level ~ Social_Support + Healthy_Life_Expectancy + 
#      Freedom, family = binomial, data = train_happy)

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.8111  -0.2667  -0.0035   0.3104   3.2763  

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)              -18.805      4.402  -4.272 1.94e-05 ***
#   Social_Support             4.371      2.165   2.019  0.04347 *  
#   Healthy_Life_Expectancy   11.932      3.692   3.232  0.00123 ** 
#   Freedom                   10.182      3.416   2.981  0.00287 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)

# Null deviance: 129.928  on 93  degrees of freedom
# Residual deviance:  44.969  on 90  degrees of freedom
# AIC: 52.969

# Number of Fisher Scoring iterations: 7

# Comment: All predictors are significant(p value < 0.05) in the training model.

# Find the test error of the model obtained
glm.predict <-  predict(glm_train, test_happy, type = "response")
glm_pred <- rep(0, length(glm.predict))
glm_pred[glm.predict > 0.5] <- 1
mean(test_happy$Happy_Level != glm_pred)
# [1] 0.1451613
# Comment: the test error of the model obtained is about 0.145

# Characteristics (ROC) curve from predicted happiness level
calc_ROC <- function(probabilities, known_truth, model.name=NULL)
{
  outcome <- as.numeric(factor(known_truth))-1
  pos <- sum(outcome) # total known positives
  neg <- sum(1-outcome) # total known negatives
  pos_probs <- outcome*probabilities # probabilities for known positives
  neg_probs <- (1-outcome)*probabilities # probabilities for known negatives
  true_pos <- sapply(probabilities,
                     function(x) sum(pos_probs>=x)/pos) # true pos. rate
  false_pos <- sapply(probabilities,
                      function(x) sum(neg_probs>=x)/neg)
  if (is.null(model.name))
    result <- data.frame(true_pos, false_pos)
  else
    result <- data.frame(true_pos, false_pos, model.name)
  result %>% arrange(false_pos, true_pos)
}
ROC <- calc_ROC(probabilities=glm02$fitted.values,
                 known_truth=Happy$Happy_Level,
                 model.name="combined")
ggplot(data=NULL, aes(x=false_pos, y=true_pos)) +
  geom_line(data=ROC, aes(color=model.name))
# Calculate the area under the ROC curve (AUC)
ROC %>% mutate(delta=false_pos-lag(false_pos)) %>% 
  summarize(AUC=sum(delta*true_pos, na.rm=T))
#         AUC
# 1 0.9597238
# Comment: the model is accurate in predicting the happiness level of each country because AUC score is close to 1,

# Classification - Random Forests
library(randomForest)
set.seed(314)
rf <- randomForest(Happy_Level~GDP_per_Capita + Social_Support + Healthy_Life_Expectancy + Freedom + Generosity + Government_Corruption, data=happy19, ntree=500, 
                   mtry=2, importance=TRUE)
rf
# Call:
# randomForest(formula = Happy_Level ~ GDP_per_Capita + Social_Support +      Healthy_Life_Expectancy + Freedom + Generosity + Government_Corruption,      data = happy19, ntree = 500, mtry = 2, importance = TRUE) 
# Type of random forest: regression
# Number of trees: 500
# No. of variables tried at each split: 2

# Mean of squared residuals: 0.08705173
# % Var explained: 65.17
importance(rf)
#                          %IncMSE   IncNodePurity
# GDP_per_Capita          22.35766542      8.255450
# Social_Support          31.61821734     12.142725
# Healthy_Life_Expectancy 22.42581751      8.559051
# Freedom                 13.62976829      4.447662
# Generosity               3.08276920      1.672873
# Government_Corruption   -0.06117613      1.872708

# Comment: The higher value indicates that the variable is more important than others.
# Comment: Social support is the most significant factor and Government Corruption is the least important one.
varImpPlot(rf)
# Comment: varImpPlot() provides a dot chart of variable importance asmeasured by Random Forest
# Comment: We see that Social support and healthy life expectancy are most important predcitors. 




