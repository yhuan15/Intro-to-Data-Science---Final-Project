# Intro-to-Data-Science---Final-Project
I have been studying Data Science for a semester and I am doing this project to try out the principles of data science using R as discussed in my class. I hope to become more productive with my new data science skills by working on something practical and meaningful.  
Author: Yiming Huang
Date: 13 Dec 2020

# Introduction: 
Perhaps one of the most significant purposes of our life is to be happy. What actually makes up happy? This question has plagued philosophers for years and most people fail in their quest for happiness because it is too complicated. Fortunately, researchers have made questionnaires worldwide, called “The World Happiness Report”, to further explore the answer to this question through scientific process. The science of happiness describes personal and national variations in happiness. The report continues to gain global recognition as governments and organizations can use the results to further the policymaking process and assess the progress of countries.
The dataset that I have chosen for this project is their “2019 World Happiness Report”. It ranks 156 countries by their happiness score based on six measurements – economic production (GDP per capita), social support, healthy life expectancy, freedom, generosity, and government corruption. I used the R language and my main focus is to find out the impact of each factor on happiness. As a result, we can focus on improving these factors to accomplish a higher level of happiness. 

# Goals of the project / Research Questions:
1.	To discover which factors are more important for us to live a happier life. 
2.	I intend to find out if there is any association between the continent and its average happiness score. For example, people who live in European countries are always thought to be happier. I hope to examine if this is misinformation. 
3.	I hope to find out any similarities and differences among the countries experiencing the highest or lowest happiness level. 

# Content: 
I have chosen the World Happiness Report dataset (2019) on Kaggle. In this dataset, we can find happiness rank and score among 156 countries globally based on six factors. The higher the happiness rank, the higher the happiness score. From the shape, we can see that the dataset includes 156 rows and 9 columns.

# Definition of the 9 variables:
•	Happiness Rank: Rank of the country based on the Happiness Score.
•	Country: Name of the country.
•	Happiness Score: A metric measured by asking the sampled people the question: "How would you rate your happiness on a scale of 0 to 10 where 10 is the happiest."
•	GDP per capita: This indicates economic prosperity of each country.  
•	Social support:  This indicates socio-emotional development of the citizens.
•	Healthy life expectancy: This indicates physical health of the citizens.
•	Freedom to make life choices: The national average of binary responses to the GWP question “Are you satisfied or dissatisfied with your freedom to choose what you do with your life?”
•	Generosity: The shared public values of generosity.
•	Perceptions of corruption: This indicates social trust, including confidence in the honesty of government and business. 


# Data Cleansing and Manipulation
I observed the structure of the variables using the str() function. The data is quite neat, but I have changed the column names and add new variables “Year” and “Continent” to make it look better and easier for data analysis. The year is 2019. Asia, Africa, North America, South America, Europe, and Australia are our six continents in this dataset. I changed the type of the two variables to factors. The final structure of my dataset has 156 observations and 11 variables. Happiness rank is an integer, Country is a character variable, and the remaining variables are numeric.

# Data Visualization
I generated some visualizations to further explore the data provided. In the scatterplots, I found that GDP, Social Support, Healthy Life Expectancy, and Freedom are correlated with the Happiness score. GDP tends to have the biggest impact on happiness. I find the correlation of Government Corruption the most interesting here. From Pearson's Chi-squared test, the two variables are independent. When the corruption score is high, however, on the plot it appears to have a positive trend. Government Corruption seems to be a positive indicator on a threshold. Besides, GDP is a significant predictor for Social Support and Healthy Life Expectancy. 

Given this, I further work on the six continents to compare and discover whether there are different trends for them regarding the happiness score. From histograms and box plots, Australia has the highest average and median happiness score, followed by Europe and North American. Africa and Asia have lower average and median happiness score. Moreover, the scatterplots colored by Continent show that there is a strong positive correlation between government corruption score and happiness score for most continents except for those in Africa. It suggests that generosity does not have a big impact on happiness.

I also extract and compare the top five and bottom five countries of the dataset. The happiest five countries according to ranked scores are all European countries. The five countries listed with the lowest happiness scores are located in Africa and Asia.


# Prediction
I used Logistic Regression and Random Forest to make predictions based on the transformed dataset. By transforming Happiness Score to a binary variable, countries with a score higher than the average Happiness Score are in the High level, otherwise are in a Low level. Social Support, Healthy Life Expectancy and Freedom are found to be significant at predicting the happiness level of each country. To my surprise GDP per capita is not a good factor at predicting happiness level. While taking a closer look at the scatterplot of GDP and happiness score, economy seems to have a positive effect on happiness below a certain score of GDP. Therefore, even though economic prosperity (GDP) tends to be the most responsible variable in determining happiness score, it no longer has a significant impact on happiness level while it is above a certain threshold.


# Conclusion
After analyzing the dataset of the Global Happiness Report in 2019, I am able to find out the impact of the different factors in determining the Happiness Score. Now we know that GDP, Social Support, and Healthy Life Expectancy contribute the most in evaluating the happiness level of each country. Interestingly, economic prosperity (GDP) has a significant impact on happiness level only when it is above a certain threshold. It looks like that money makes you happy up to a certain level, however having good health and social support is always important for us to be happier. As economy, physical health, and mental health are significant factors to live happier, governments should value both economic and socio-emotional developments to achieve national progress. 

In addition, I explore the research question deeper by classifying the top 5 and bottom 5 countries in the dataset according to happiness ranking. Countries located in Europe. like Finland, Denmark, Norway, and Iceland, for their citizens, life is decent and happy. Regarding the least happy countries located in Africa and Asia, most people are unhappy perhaps because their most basic needs are far from being satisfied. This thus suggests that countries in the same continent tend to have similar happiness levels and are perhaps affected by similar factors. 
Through this analysis, I am able to answer most of my questions and prove/disprove my hypothesis. Individuals should focus on both physical health and human socio-emotional development. More broadly, if governments and organizations can focus on improving these aspects, it will lead us to achieve a higher level of happiness on personal and global levels. 


# References
World Happiness Report dataset. ‘Sustainable Development Solutions Network’ (2019). [Online] Available at: https://www.kaggle.com/unsdsn/world-happiness
