A machine learning project to predict the resale price of HDB flats based on their geographical, temporal, and structural properties was done as part of an introductory data science course, DSE1101 Data Science for Economics. 

# Project Approach
1. Exploratory Data Analysis: Relationships between key variables, such as flat type, storey range, remaining lease and resale price, were explored using boxplots and scatter plots to identify potential correlations and collinearity. Manual feature selection was done to shrink the large number of parameters.

2. Model Development:
 - Linear Regression (MLR): Multiple models were built, including models using the full data set, data set with reduced parameters and their log-transformed versions, and evaluated using RMSE.
 - Decision Trees: Decision tree models were developed using rpart and tree libraries, with pruning and cross-validation to improve accuracy.
 - Principal Component Regression (PCR): Dimensionality reduction was applied, and models with varying numbers of components were created to optimize performance.

3. Model Evaluation: All models were assessed on the test dataset using RMSE to compare their predictive accuracy.
