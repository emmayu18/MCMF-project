My CHI. My Future. Modeling
================

# Overview

With the MCMF program data, I created two predictive models: NLP machine
learning category classification model and time series program count
forecasting model.

Click on the following link to learn more about the data cleaning
process and EDA:

<a href="https://github.com/emmayu18/MCMF-project/blob/main/eda/EDA.md"
target="_blank">Link to EDA</a>

# Text Classification

Each program in the dataset has a description and I wanted to see if I
could use NLP modeling to predict the category (Academics, Community
Service, Leisure and Arts, or Professional Skill Building) of a program
based on its description. Although I performed the data wrangling in R,
I decided to do all of the modeling in Python through a Jupyter notebook
file because it was significantly faster at training the models. A
successful category classification model could reduce the data input
work of program planners and perhaps classify the programs more
accurately. <br>

<a
href="https://github.com/emmayu18/MCMF-project/blob/main/model/text_classification.ipynb"
target="_blank">Link to Jupyter notebook</a>

## Models Tested

- Logistic regression
- Multinomial Naive-Bayes
- Random forest
- Support vector machine (SVM)
- K-nearest neighbors
- Stacked model of the previous 5

## Techniques

The following are techniques I implemented in my modeling to improve
performance: <br>

- Text pre-processing
  - Text cleaning
  - Stop word removal
  - Stemming
  - Frequent token removal
  - TF-IDF vectorization
- Grid search via stratified k-fold cross validation
- Model stacking

The following techniques resulted in a decrease in cross validation
score: <br>

- Word2Vec
- Dimensionality reduction (SVD because the tokenized data is a sparse
  matrix)
- Random oversampling, random undersampling, and a combination of both
- Balanced class weights for logistic regression, random forest, and SVM

## Results

I used 4 methods to test my model:<br><br>

#### 1. Testing Data

I first used the testing set from the original data split. The following
table contains the resulting metrics:

<table class="table table-condensed">
<thead>
<tr>
<th style="text-align:left;">
Metric
</th>
<th style="text-align:center;">
Value
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Accuracy
</td>
<td style="text-align:center;">
0.885
</td>
</tr>
<tr>
<td style="text-align:left;">
F1-score
</td>
<td style="text-align:center;">
0.733
</td>
</tr>
<tr>
<td style="text-align:left;">
Precision
</td>
<td style="text-align:center;">
0.811
</td>
</tr>
<tr>
<td style="text-align:left;">
Recall
</td>
<td style="text-align:center;">
0.680
</td>
</tr>
<tr>
<td style="text-align:left;">
ROC AUC
</td>
<td style="text-align:center;">
0.945
</td>
</tr>
<tr>
<td style="text-align:left;">
Hamming Loss
</td>
<td style="text-align:center;">
0.115
</td>
</tr>
</tbody>
</table>

All of the performance metrics are in the acceptable to good range,
indicating that the model is sufficient at categorizing the MCMF
programs through their descriptions. Recall is the lowest, most likely
due to the imbalance in the data. <br><br>

#### 2. Updated Data (2024)

The original data used to train the model was downloaded in April 2023.
I obtained an updated dataset in August 2024 with new observations. The
following table contains the performance metrics for the new dataset:

<table class="table table-condensed">
<thead>
<tr>
<th style="text-align:left;">
Metric
</th>
<th style="text-align:center;">
Value
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Accuracy
</td>
<td style="text-align:center;">
0.848
</td>
</tr>
<tr>
<td style="text-align:left;">
F1-score
</td>
<td style="text-align:center;">
0.639
</td>
</tr>
<tr>
<td style="text-align:left;">
Precision
</td>
<td style="text-align:center;">
0.739
</td>
</tr>
<tr>
<td style="text-align:left;">
Recall
</td>
<td style="text-align:center;">
0.595
</td>
</tr>
<tr>
<td style="text-align:left;">
ROC AUC
</td>
<td style="text-align:center;">
0.901
</td>
</tr>
<tr>
<td style="text-align:left;">
Hamming Loss
</td>
<td style="text-align:center;">
0.152
</td>
</tr>
</tbody>
</table>

The performance is slightly worse than what we saw with the testing set
but it is pretty similar. In this case, the f1-score and recall are in
slightly concerning range, indicating that the model is most likely
conservative in its prediction. <br><br>

#### 3. Multi-Category Data

Before modeling, I removed observations that were labeled with multiple
categories from the dataset. For this testing method, I ran these
observations through the model to see if the predicted category was one
of the multiple categories. **Out of 1086 programs, 994 were correctly
classified (91.5%)**. <br><br>

#### 4. Manually-Made Data

I hand wrote descriptions of imaginary afterschool programs to see if
the model can correctly predict the category. The `True` column shows
the category I labeled each program as and the `Predicted` column shows
the category classified by the model.

<table class="table table-condensed">
<thead>
<tr>
<th style="text-align:left;">
True
</th>
<th style="text-align:left;">
Predicted
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
<span style="color: #333333">Professional Skill Building</span>
</td>
<td style="text-align:left;">
<span style="color: #333333">Professional Skill Building</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
<span style="color: #333333">Leisure and Arts </span>
</td>
<td style="text-align:left;">
<span style="color: #333333">Leisure and Arts </span>
</td>
</tr>
<tr>
<td style="text-align:left;">
<span style="color: #cf0000">Community Service </span>
</td>
<td style="text-align:left;">
<span style="color: #cf0000">Leisure and Arts </span>
</td>
</tr>
<tr>
<td style="text-align:left;">
<span style="color: #333333">Academics </span>
</td>
<td style="text-align:left;">
<span style="color: #333333">Academics </span>
</td>
</tr>
<tr>
<td style="text-align:left;">
<span style="color: #333333">Professional Skill Building</span>
</td>
<td style="text-align:left;">
<span style="color: #333333">Professional Skill Building</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
<span style="color: #333333">Leisure and Arts </span>
</td>
<td style="text-align:left;">
<span style="color: #333333">Leisure and Arts </span>
</td>
</tr>
<tr>
<td style="text-align:left;">
<span style="color: #cf0000">Community Service </span>
</td>
<td style="text-align:left;">
<span style="color: #cf0000">Leisure and Arts </span>
</td>
</tr>
<tr>
<td style="text-align:left;">
<span style="color: #333333">Academics </span>
</td>
<td style="text-align:left;">
<span style="color: #333333">Academics </span>
</td>
</tr>
</tbody>
</table>

It looks like the model is good at predicting the categories on
completely new data but has trouble classifying Community Service
opportunities. This could be because of the imbalance in the training
dataset. Community Service was the smallest category in the training
data while Leisure and Arts was the largest.

## Next Steps

In order to improve performance, I could implement text augmentation
(synonym replacement, random insertion/deletion, text paraphrasing,
etc.). I could also experiment with a hybrid of random
over/undersampling and class weighting. I have tried both techniques
separately but saw a decrease in performance (most likely due to
overfitting), but I think an appropriate combination of those methods
could possibly improve performance. Increasing the number of TF-IDF
features have shown to increase performance and if I had the computing
power, that is another strategy I would experiment with. However, I
would have to be careful not to overfit the training data. <br><br>

# Time Series Analysis

In the original data, each program had a start date and end date, which
allowed a time series analysis on the program count. I wanted to create
a forecasting model to predict MCMF program count. After looking at the
time series data and seeing moderate volatility, I also trained a
volatility model. <br>

<a
href="https://github.com/emmayu18/MCMF-project/blob/main/model/growth_forecasting.R"
target="_blank">Link to R script</a>

## Data

The following grid shows time series plots for each variable in the
dataset. Since most of the variables follow a similar pattern as the
total count time series, I decided to focus my analysis on the overall
data.

<img src="plots/eda5.png" style="width:65.0%" />

The next figure shows the time series plot of the MCMF program count. I
used a 60-40 ratio to split the training and testing sets.

<img src="plots/training_testing.png" style="width:65.0%" />

The data consisted of daily program count from 1/1/2020 to 3/15/2023.
The training/testing cutoff was at 12/2/2021. The plot shows a complex
time series with significant dips that seem to display seasonality. The
dips are found approximately every 2-5 months, during student holiday
seasons (spring break, start of summer break, end of summer break,
winter holidays). There is also a general trend of count growth from
2020 to the later quarter of 2021. The program count peaks at slightly
over 2000 programs and then displays a decreasing trend. The data also
seems to have a moderate amount of volatility even without the dips.

## Models Tested

The following models were tested for program count forecasting: <br>

- Autoregressive Integrated Moving Average (ARIMA) model
- Seasonal Autoregressive Integrated Moving Average (SARIMA) model
- Autoregressive Fractionally Integrated Moving Average (ARFIMA) model

The following model was tested for volatility modeling/forecasting: <br>

- Generalized Autoregressive Conditional Heteroscedastic (GARCH) model

<br> I ultimately decided to use ARFIMA to model the time series data
because although the data passed the Augmented Dickey-Fuller test and
was proven to be stationary, the ACF plot did not display a significant
cutoff. This could be indicating that the data is long-range dependent
and ARFIMA is the best-suited model for this type.

<img src="plots/acf_pacf.png" style="width:80.0%" />

## Results

#### Performance Metric

The following table shows the resulting performance metric values from
comparing the forecasted data to the test data:

<table class="table table-condensed">
<thead>
<tr>
<th style="text-align:left;">
Metrics
</th>
<th style="text-align:center;">
Values
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Mean Absolute Error (MAE)
</td>
<td style="text-align:center;">
343.33621
</td>
</tr>
<tr>
<td style="text-align:left;">
Root Mean Squared Error (RMSE)
</td>
<td style="text-align:center;">
402.34916
</td>
</tr>
<tr>
<td style="text-align:left;">
Mean Percentage Error (MPE)
</td>
<td style="text-align:center;">
-32.84141
</td>
</tr>
<tr>
<td style="text-align:left;">
Mean Average Percentage Error (MAPE)
</td>
<td style="text-align:center;">
60.98613
</td>
</tr>
</tbody>
</table>

The performance metrics indicate that the model is not completely good
at forecasting program count. However, this is expected from running a
complex time series data through a model that is used to simpler data.
<br><br>

#### ARFIMA Forecast

The following plots show the forecasted data from the ARFIMA model
compared to the testing data:

<img src="plots/ARFIMA_forecast.png" style="width:45.0%" />
<img src="plots/ARFIMA_forecast_test.png" style="width:45.0%" />

Although the performance metrics were not good, we can see from the
plots that the forecast follows the general decreasing trend of the
testing data. <br><br>

#### ARFIMA-GARCH Forecast

The following plots show the ARFIMA forecast along with the volatility
forecast made by the GARCH model:

<img src="plots/ARFIMA_GARCH_forecast.png" style="width:45.0%" />
<img src="plots/ARFIMA_GARCH_forecast_test.png" style="width:45.0%" />
<br><br>

#### GARCH Variance Bounds

I also plotted the conditional variance bounds against the training
data:

<img src="plots/GARCH_estimate.png" style="width:65.0%" />

## Next Steps

The time series data for MCMF programs was very complex, with extreme
fluctuations that isn’t captured by simple seasonal differencing,
contributing to the inadequate performance. Despite the limitations
imposed by the nature of the data, the ARFIMA model was able to accurate
forecast the general trend of the program count. In order to create a
model that is able to capture the complexity of this data, I could try
training a deep learning model such as LSTM. I could also take a
completely opposite approach and try a simpler forecasting method such
as exponential smoothing.
