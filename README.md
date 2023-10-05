# Disaster Relief Project

## Metadata:
Disaster Relief Project for the University of Virginia's DS6021 course.

Included are the final PDF file of the report as well as the R code in an RMD file.

## Synopsis:
The **``Disaster Relief Project``** is the final project for UVA's DS6021 Statistical Learning course wherein students are tasked with using various classification methods to solve a real historical data-mining problem — locating displaced persons living in makeshift shelters following the destruction of the earthquake in Haiti in 2010.

Following the 2010 earthquake, rescue workers needed to get supplies to the displaced persons; a challenging goal given the earthquake destroyed communications infrastructure, made roads impassable, etc.

As part of the rescue effort, a team from the Rochester Institute of Technology collected high resolution geo-referenced imagery. It was known that the people whose homes had been destroyed by the earthquake were creating temporary shelters using blue tarps, and these blue tarps would be good indicators of the displaced persons' locations. The problem was that there was no way for aid workers to search the thousands of images in time to find the tarps and communicate the locations back to the rescue workers. Data-mining algorithms, which could search the images far faster and more thoroughly then humanly possible, served as a solution. The goal was to find an algorithm that could effectively search the images to locate displaced persons and communicate those locations to rescue workers.^[UVA DS6030 Disaster Relief Project].

The project tasks students with testing various algorithms they learned during the course on the imagery data collected during the relief efforts to determine which method they would use to as accurately as possible, and in as timely a manner as possible, locate as many of the displaced persons identified in the imagery data.

Students use 10-fold cross-validation to tune and evaluate the performance of 7 models:

* Logistic Regression
* LDA (Linear Discriminant Analysis)
* QDA (Quadratic Discriminant Analysis)
* KNN (K-nearest neighbor)
* Penalized Logistic Regression (Elastic Net penalty)
* Random Forest
* Support Vector Machine
