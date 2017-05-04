# SpeedDatingLogRegression
## Love in the Fastlane – Predicting Success in Speed Dating using Logistic Regression and R
Conducted statistical analysis and logistic regression modelling to predict match success in speed dating events

This was submitted as a project for my Statistical Methods and Computation class in my MS Business Analytics program. The original title is “Love in the Fastlane: Success in Speed Dating”. My other teammate is Ruoxuan Gong. This project was done for educational purposes only. Click the photos to enlarge.

## Problem Statement
The purpose of this study is to determine if speed dating outcomes can be predicted, and if yes, what are the most important factors that would help speed dating participants successfully match with each other

## Methodology
### Data Cleaning and Pre-processing
- 8,369 rows containing speed dating round and participant information in one table
- Table was separated into two tables: round-specific data (round condition and relative data) and participant data (demographics and interests)
- Has a big amount of double-counted rows:
    - Wave ID 1 with Participant A and Partner D
    - Wave ID 1 with Participant D and Partner A (double-count and removed)
- After removal of double-counts, nrow = 4,184.
- Columns with a lot of missing data were removed.
- Domain knowledge used to produce initial set of variables
- Rows with missing data were removed; new nrow = 3,377
- All Participants are women and all partners are men

### Hypothesis
It is possible to predict, to a certain level of confidence, the outcome of a speed dating round(match or no match) by analyzing and taking into consideration the different factors during the round itself.

### Variable Selection
- Dependent Variable: Match (1/0)
- Independent Variables: Round-specific data, difference in preference ratings (attractiveness, sincerity, intelligence, fun(funny?), ambitious, and shared hobbies), ratings for and by the participant

### Tool & Model Selection
- Tool: R
- Model: Logistic Regression

# Read the analysis at https://dataontherocks.wordpress.com/2017/05/04/love-in-the-fastlane-predicting-success-in-speed-dating-using-logistic-regression-and-r/
