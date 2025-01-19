# Text Clustering Analysis in a Portfolio Choice
The repository contains the data and R script with the solution to the "Text Clustering Analysis in a Portfolio Choice" problem I was given during the course "Applied Textual Data Analysis" at NHH Norwegian School of Economics.

This assignment also evaluates different representations of the text in the n-gram space (uni- versus bi-grams) and term limits based on Log-Likelihood.
The assignment was solved in a group of 3 people (Julia Lorenc, Giuseppe Pio Lando & Claudia dal Prà).

## Table of Contents
* [Project Overview](#project-overview)
* [Data](#data)
* [Analysis](#analysis)
* [Dependencies](#dependencies)

## Project Overview
It is the 31.12.2013 and you are a portfolio manager constructing a portfolio for 2014. You can allocate 1 million NOK across the 500 companies available to you. You are convinced that oil companies will perform very well in 2014. Unfortunately, regulation prevents you to invest a single NOK in this sector, in your data defined as **industry.fama.french.49 == 30 Oil** in the `raw.data` data frame. Luckily, from Hoberg G. and Phillips G. (2016) you know that industry assignment is not perfect and that there might be firms which business is actually in the oil sector while not classified as such.

You develop the following strategy using insights from textual analysis: You identify a subset of firms that – based on their business description in their annual report – sound like oil firms even though they are not classified as such. You invest an equal amount in each of those firms.

## Data
Data used to perform the analysis and solve the problem are in `Data` folder in the current repository.

## Analysis
The procedure for the analysis and algorithm evaluation are the following:
1. Data cleaning: remove punctuation, remove numbers, remove stopwords, make lower case, only consider terms with 3 to 20 letters, and delete excess white spaces
2. Transforming the data into bigrams
3. Making a document term matrix only including tokens (bi-grams) that appear in more than 5 but less than 100 documents
4. Identifying firms in the Oil sector and firms not in the Oil sector
5. Computing the Log-Likelihood of being in the subset of oil firms and the remaining for each token (bi-gram) in the corpus
6. Finding 5 non-oil peer firms using cosine similarity for each oil firm
7. Computing the average return for the oil portfolio and the peer group for the year 2014
8. Evaluating the performance of your algorithm - computing the Root Mean Squared Error between both portfolios

## Dependencies
The project requires the following R packages:
* tm
* tidytext
* dplyr
* stringr
* tokenizers
* stopwords
* proxy
* tidyverse
