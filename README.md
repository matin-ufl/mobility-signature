# Repository for article (_Actigraphy features for predicting mobility disability in older adults_)

All the codes for our data analysis published under _Actigraphy features for predicting mobility disability in older adults_ is available.
***
The packages in this repository contain functions (_f0x_ files) and some scripts (_s0x_ files) to test them. Therefore, if you wish to use methods for data preprocessing and analysis, load the functions.
The packages can be summarized as follows:

* **LIFE toolbox:** Contains all the functions used to construct features mentioned in the paper. See _demo\_feature\_construction.R_ to see how features are calculated for one participant data.
* **Dataset Aggregation:** Merging avaliable data for our participant from different sources.
 * Provided by data collection centers, which are mostly traditional easy-to-access features; i.e., steps per day, minutes of having 1000+ activity count, ...
 * Primary accelerometer features, which contain most of the features in the dataset (obtained by running feature construction module, mentioned above).
* **Dataset examination:**  Checking correlation, VIF, correlation bar plot to the target variable,...
* **Feature Selection and Evaluation:**
 * Applying different feature selection methods (SFS, LASSO, Ridge Regression, ...)
 * Identifying important features (importance factor)
 * Results of different subsets

***
If you need further assistant, contact me at matin@cise.ufl.edu
