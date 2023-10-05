# Disaster Relief Project

## Metadata:
Disaster Relief Project for the University of Virginia's DS6021 course.

Included in the repository are the final PDF file of the report as well as the R code in an RMD file.

The rest of this README.md contains an overview of the project, data, and my findings. Please refer to the .PDF and .RMD files for analysis and code, respectively.

## Synopsis:
The **``Disaster Relief Project``** is the final project for UVA's DS6021 Statistical Learning course wherein students are tasked with using various classification methods to solve a real historical data-mining problem — locating displaced persons living in makeshift shelters following the destruction of the earthquake in Haiti in 2010.

Following the 2010 earthquake, rescue workers needed to get supplies to the displaced persons; a challenging goal given the earthquake destroyed communications infrastructure, made roads impassable, etc.

As part of the rescue effort, a team from the Rochester Institute of Technology collected high resolution geo-referenced imagery. It was known that the people whose homes had been destroyed by the earthquake were creating temporary shelters using blue tarps, and these blue tarps would be good indicators of the displaced persons' locations. The problem was that there was no way for aid workers to search the thousands of images in time to find the tarps and communicate the locations back to the rescue workers. Data-mining algorithms, which could search the images far faster and more thoroughly then humanly possible, served as a solution. The goal was to find an algorithm that could effectively search the images to locate displaced persons and communicate those locations to rescue workers.[^1]

[^1]: UVA School of Data Science, DS6030: Statistical Learning.

The project tasks students with testing various algorithms they learned during the course on the imagery data collected during the relief efforts to determine which method they would use to as accurately as possible, and in as timely a manner as possible, locate as many of the displaced persons identified in the imagery data.

Students use 10-fold cross-validation to tune and evaluate the performance of 7 models:

* Logistic Regression
* LDA (Linear Discriminant Analysis)
* QDA (Quadratic Discriminant Analysis)
* KNN (K-nearest neighbor)
* Penalized Logistic Regression (Elastic Net penalty)
* Random Forest
* Support Vector Machine

## The Data:
We train the models using a dataset of 63,241 observations and test them using a holdout set of 2,004,177 observations.

The training data contains 63,241 observations with four columns:

-   **`Class`**: Categorical variable for pixel classification, including Blue Tarp, Rooftop, Soil, Various Non-Tarp, and Vegetation.

-   **`Red`**: Numeric variable for the pixel's Red color quantity.

-   **`Green`**: Numeric variable for the pixel's Green color quantity.

-   **`Blue`**: Numeric variable for the pixel's Blue color quantity.

Several steps were necessary to wrangle the data for modeling. First, we created a factor type variable, **`Tarp`**, to differentiate observations identified as Blue Tarps (e.g., **`Tarp=Yes`**) from those identified as Rooftop, Soil, Various Non-Tarp, or Vegetation (e.g., **`Tarp=No`**).

The holdout set required more wrangling to get it into a format comparable with the training data. Those providing the holdout set shared several text files that we cleaned and converted to CSV files before combining the separate files into one.

## Conclusions:
#### Best Performing Algorithm:

##### Cross-Validation:

The SVM with an RBF kernel performed best on the training data in the CV. While the RF had the highest AUC, the SVM had the second-highest AUC but lowest FPR and highest Balanced Accuracy. That said, the SVM also had the longest train duration, at 56.69 seconds, which isn't all that long but is roughly five times the duration of the Logistic, LDA, and QDA models. We were also careful in our hyperparameter tuning for the SVM; the recommended $\gamma$ value may have actually forced the SVM to perform even better in the CV, but we knew this would likely result in poor performance on the holdout set.

##### Holdout:

The PLR model performed best on the holdout set; it has the second-lowest FPR and Balanced Accuracy, the highest AUC, Kappa, and F1 score, and it's the fastest to deploy. The traditional Logistic and LDA models are close contenders. In fact, we believe could choose either of these three models as performing best on the holdout set, but PLR strikes a reasonable balance between the performance metrics we consider. That being said, interpretability and communicating model performance is incredibly important, so we could just as easily choose the more simple Logistic or LDA models. 

#### Discussing Best Performing Algorithm:

The optimal model differs for the CV (SVM) and the holdout (Logistic). On the surface, this may seem incompatible, but it is, however, reconcilable; the performance metrics for all models don't fluctuate too much between the CV and holdout testing except for the FPR for QDA, KNN, and RF and the test duration for KNN. For QDA, KNN, and RF, we believe the models overfit the training data. For KNN, it's possible that $k=23$ is too high and results a less flexible, nearly linear decision boundary. It's also possible the $k=23$ is too low, resulting in a classifier with high variance and an overly flexible decision boundary. For the RF, as mentioned in section 8.4, it's possible the bootstrapped samples contain few (if any) observations of the minority class, which may partially explain the difference in performance. The performance differences could also have more to do with differences between the training and holdout data.

#### Recommended Model:

All models perform relatively well on the holdout set, but a few stand out over the others. We recommend the PLR model as the best option for detecting the presence of blue tarps. Tested against the holdout set, the PLR model has the highest AUC, Kappa, and F1 score, the second-highest Balanced Accuracy, the second-lowest FPR, and the fastest test time. The Logistic model is a close second, followed by LDA and SVM. The Logistic and PLR models are similar, but the PLR model may have performed slightly better because of the combination of L1 (Lasso) and L2 (Ridge) penalties encouraging sparse coefficient estimates that *might* then minimize the effects of multicollinearity, whereas the traditional Logistic model may overfit the training data in the presence of multicollinearity.

#### Relevance of Performance Metrics:

We emphasized the importance of AUC, Balanced Accuracy, Kappa, F1, and FPR throughout this analysis. While training the models and selecting thresholds, we focused a lot of attention on the FPR. This may have resulted in biased threshold selection; that is, selecting the threshold with more weight in our consideration given to FPR may result in the models performing poorly on unseen data. This is part of the reason we chose to also include the other performance metrics, but it made choosing the threshold a bit of a trade off as the FPR was the only performance metric to fluctuate much between thresholds. Given the degree of class imbalance in both datasets, two of our decisions regarding performance metrics seem appropriate and relevant, those are (1) the use AUC instead of Accuracy as the metric choice for caret's *train* function and (2) consideration of Balanced Accuracy, Kappa, and F1 scores as the main performance metrics (alongside FPR in the larger project context of disaster relief).

#### Limitations and Considerations for Future Modeling:

The author's lack of subject matter expertise is a significant limitation of the methods used here. In that light, these models could serve as preliminary algorithms for further refining, specifically in terms of which performance metrics to consider in model evaluation and parameter tuning; this could be achieved via collaboration with data scientists and non-data workers from involved government entities and non-governmental organizations, including but not limited to the Red Cross, the United Nations, and other humanitarian aid and disaster relief organizations.

Another limitation in the model development and parameter optimization presented here is that the models are trained on a limited dataset. The models are accurate, but we would be remiss not to advise further model training on new data from different locations. It may even be the case that one model performs better on data from urban areas and another better on data from rural environments.

On the data-level, the RGB color scheme is not the most useful for such important use cases. Given more time, we would liked to have researched deeper into feature engineering and transforming the RGB values into alternative color models that can decouple intensity (HSL, for example) and chromaticity (CIELUV, for example).

On the algorithmic-level, various other approaches to adapt the learning algorithms beyond what we leverage in this report may result in improved model performance; for example, perhaps using the Synthetic Minority Over-sampling Technique (SMOTE) with the RF may improve that algorithms performance.

Each algorithm used in this project also has several underlying assumptions and limitations; we list these in the subsections "Assumptions and Limitations."

#### Real-World Application:

A wealth of subject matter expertise and related contextual information went untapped for this project. The dataset provided does not tell us where the data were captured; for example, if the data is from photographs taken over Port-au-Prince, then running the predictive models may not be all that useful given that many displaced persons in Port-au-Prince were sheltering under tarps provided by the Red Cross, so their location is assumed to be already known.[^2]

[^2]: American Red Cross, "Haiti Earthquake Relief: One-Year Report," (2011), available at <https://www.redcross.org/content/dam/redcross/atg/PDF_s/HaitiEarthquake_OneYearReport.pdf>.

There's much to consider beyond strictly model development in the context of this project. We could find the optimal model for classifying pixels values and identifying blue tarps, but that knowledge is only useful if communicated in such a way that the aid providers on the ground can quickly act on it. It's one thing to say, "There's a blue tarp here," and another to quickly and accurately identify a blue tarp's location, know the terrain features around it, and navigate to it to provide the necessary relief. A vast chasm exists between the model output and the relief practitioners on the ground, but with communication, iteration, and practice, perhaps the next disaster can apply lessons learned and reduce loss of life and suffering for displaced persons.
