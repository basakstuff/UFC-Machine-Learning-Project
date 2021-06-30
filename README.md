# UFC-Machine-Learning-Project
### CME 4403 Introduction to Machine Learning Course Term Project

This project aims to find out which side will win the fight. This prediction was made with traditional machine learning algorithms.

# Dataset

Decision of selecting the dataset depended on mostly on the sample size of the dataset, understandability, and usability. At the end decision was done on the [dataset](https://www.kaggle.com/rajeevw/ufcdata) UFC’s fight records between 1993-20192.

The dataset `data.csv` has 145 columns and 5144 rows. Columns include fighters’ names, referee, data of the fight, location and more. One column includes two fighters named as traditionally blue corner (B_) and red corner (R_), and there are the same columns for both corners. For readability purposes this differentiation will not be included in exploration, one column that has blue and red corner will be marked with an asterisk (*).

Pearson correlation is used when constructing the correlation matrix. The visualization can be viewed below.

| Coloumns  | Coloumns |Coloumns  | Coloumns|
| ------------- | ------------- |------------- | ------------- |
| Fighter*  | Stance* | Height* | Reach* | 
| Weight* | Weight_class  | Referee | Date |
| Location | Winner |Title_bout  | Total_time_fought.sec* |
| Total_rounds_fought*| Total_title_bouts | No_of_rounds* |Current_lose_streak*  |
| Current_win_streak* |Longest_win_streak* | Losses* | Draw* |
|  Wins* | Win_by_KO.TKO* | Win_by_Submission* | Win_by_decision* |

![00000](https://user-images.githubusercontent.com/25417307/120241057-afff5400-c26a-11eb-908b-f1174f25f3df.png)

# Data Preprocessing

Performed tasks:
- Feature Selection
- Replacing empty string with NA
- Distinguishing numeric & symbolic fields
- Removing constant columns (due to no variation in them)
- Formating data to 3 Decimal Points

# Models

## k-Nearest Neighbor

I evaluated all values in the range of 1-100, each running for 30 times; the average accuracy percentage of each value was then plotted, as in the graph below. In fact, the experiment was administered twice, initially on the original data set, and further when PCA was applied. The confusion matrix for Pre-PCA application and post-PCA application is also available in the appendix section. The graph below, also, clearly illustrates the KNN average accuracy for each “k” value. Hence, it is derivable that the accuracy of the KNN algorithm is with peak accuracy value of k = 54.

![dadas](https://user-images.githubusercontent.com/25417307/120241634-d4a7fb80-c26b-11eb-8806-2746ef1c285d.png)

![knn](https://user-images.githubusercontent.com/25417307/120241512-97436e00-c26b-11eb-88b2-3779baeadac7.png)

## Support Vector Machines (SVM)s

SVM is another nonlinear modelling approach to solve classification problems. SVM uses a technique named as kernel trick to transform data and find optimal boundaries. We are using the `E1071` R library to perform SVM classification. In our model Radial Basis Functions was used as the  kernel technique. The confusion matrix for SVM implementation with reduced dimensions and the original dataset is displayed below.

![dadas](https://user-images.githubusercontent.com/25417307/120241672-edb0ac80-c26b-11eb-9327-830ca1d63360.png)

## Naïve-Bayes (NB)

Bayesian prediction is very good with predicting categorical targets. The reason it is called naive is, I estimate that the features are independent from each other. My target feature is about finding which corner is won, which is a categorical feature.

![02](https://user-images.githubusercontent.com/25417307/120241774-2486c280-c26c-11eb-94b0-979dda4cbf7d.png)


## Random Forests (RF)

To train the model in my training dataset, I used the `randomForest()` function in the randomForest R library. I did not normalize the data this time, unlike previous approaches, because random forests do not require data to be rescaled or transformed. In both PCA (principal component analysis) areas and without PCA fields, I trained the model.

![03](https://user-images.githubusercontent.com/25417307/120241913-67489a80-c26c-11eb-9360-e1b9fca7ea3e.png)

# Result

As a conclusion, I have used an already prepared dataset related to a mixed martial art and preprocessed it for machine learning models that I have chosen to be used. These models were kNN, naïve Bayes, support vector machines, and random forests. In the training random sampling and cross validation methods were used as well as feature extraction method such as principal component analysis. The results of experiment can be seen from table The best predicting model is support vector machines with accuracy value of 59.63%, followed by kNN with 58.96% and worst performing is random forests with 56.04%. Interestingly, using principal component analysis did not help with increasing the accuracy of model excluding naïve Bayes model.


| Models | Accuracy | Accuracy with PCA |
| --- | --- | --- |
| KNN | 58.96 | 56.84 |
| Naive Bayes | 56.84 | 58.17 |
| SVM | 59.63 | 56.97 |
| Random Forest | 57.1 | 56.04 |

# Author 

<p>
  <a href="https://www.linkedin.com/in/basaker" rel="nofollow noreferrer">
    <img src="https://i.stack.imgur.com/gVE0j.png" alt="linkedin"> LinkedIn
  </a> &nbsp; 
