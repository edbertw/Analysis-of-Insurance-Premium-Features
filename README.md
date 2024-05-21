Overview ->
This project aims to thoroughly answer the data science question : What is the order of relative importance of variables when determining Health insurance premium costs? using R

I will give a deep analysis with the help of univariate analysis and visualizations as well as Random Forest regression methods, with the purpose of developing a firm and accurate solution to the problem.

Dataset ->
The dataset used in this project is insurance.csv, which consists of several attributes possessed by policyholders. These include age, sex, body-mass-index, number of children, smoking status and insurance charges.

Data Preprocessing ->
1. Null checks
2. Data Encoding (Converting categorical variables to numerical form)
3. Standardizing numerical data

Methodologies ->
1. Data Visualizations to show univariate and bivariate relationships
2. Correlation Analysis
3. Importance Feature of Random Forest Library
4. Adding new predicted column, consisting of predicted charges by RandomForest trained on entire dataset.

Model Efficiency / Accuracy ->
1. %Var explained = 85.28%
2. R-squared = 0.853
3. Mean-squared residuals = 21566862

Results ->
The results have unanimously shown that the smoker column has the most significance on premium charges, with a relative importance score of 364.07, followed by bmi, age, children, region and sex, at last place with a relative importance score of -6.12 .

