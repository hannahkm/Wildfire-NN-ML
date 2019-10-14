# Wildfire-NN-ML
An extension of Wildfire-Analysis (github.com/hk21042/Wildfire-Analysis). Using maching learning to identify the relationship between climate variables and how fire weather can be predicted. Done through the JHU APL ASPIRE program (Summer 2019).

## The Problem
In the previous portion of this study, I found that the Fire Weather Index (FWI) showed significant inconsistencies with fire weather as predicted by climate variables. It was able to predict inactive seasons, when less fires occurred, but was less capable in predicting active seasons. A new index would be needed to accurately predict, and thus prevent, potentially disastrous wildfires in California. In this study, several methods of machine learning were used to identify patterns or other relationships between climate and fire variables such as BUI (build-up index), ISI (initial spread index), and FRP (fire radiative power). The methods used, as listed below, were: support vector machines, support vector regressions, DBSCAN, convolutional neural networks, extreme learning machines, and recurrent neural networks.  

## Data
- Moderate Resolution Imaging Spectroradiometer (MODIS): provided the active fire products to define active and inactive summers
- California Department of Forestry and Fire Protection (CalFire): continuous record of acres burned by wildfires since the 1930s. Data from the period between 1997 and 2016 were used
- Giovanni (https://giovanni.gsfc.nasa.gov/giovanni/): an interface for NASA’s Modern-Era Retrospective Analysis for Research and Applications, version 2 (MERRA-2) for obtaining weather variables
- Global Fire Weather Database (GFWED): provided data for the Fire Weather Index (FWI)

## Methods
1. Support Vector Machine/Support Vector Regression (SVM/SVR): draws lines between points to divide the dataset into categories. SVM deals with categorical variables, SVR uses continuous variables
2. DBSCAN: clustering algorithm that groups points that are within a certain distance of each other. DOES account for noise
3. Convolutional Neural Networks (CNN): a deep neural network used most often in image processing - it identifies larger patterns using smaller patterns
4. Extreme Learning Machine (ELM): a feedforward neural network meant to be much faster and more accurate than SVM/SVR and other networks that use backpropogation
5. Recurrent Neural Networks (RNN): an artificial neural network that contains loops, which means it can retain information and patterns unlike other neural networks
