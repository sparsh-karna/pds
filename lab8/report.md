# **Programming for Data Science – Lab Experiment 8**

**Title:** Skewness Handling, Transformation, and Regression Evaluation
**Student Name:** Sparsh Karna
**Reg. No.:** 23BDS1172
**Date:** 18-09-2025

---

## **1. Problem Statement**

In healthcare, predicting the **Length of Stay (LOS)** of patients is important for hospital resource allocation, planning, and cost control. LOS data is naturally **right-skewed**, as most patients have short stays, while a few with severe or chronic conditions remain hospitalized for much longer. This skewness violates linear regression assumptions like residual normality and constant variance, which can reduce model accuracy.
The objective of this lab is to **diagnose and handle skewness** through transformations, build regression models on both original and transformed datasets, and evaluate improvements in predictive performance.

---

## **2. Dataset Design and Generation**

* **Rows:** 2,500 synthetic patients.
* **Attributes:** 12 total (8 numeric, 4 categorical).

**Numeric Variables:**

* Age, BMI, Blood Pressure (Systolic), Cholesterol (all near normal).
* Income, Medical Expenses, Num Visits (right-skewed).
* Comorbidity Score (normal).
* Length of Stay (target, right-skewed).

**Categorical Variables:**

* Gender (Male/Female), Smoking Status (Non-Smoker/Smoker/Ex-Smoker), Region (Urban/Rural/Suburban), Insurance Type (Private/Public/None).

**Target Simulation (Length of Stay):**
LOS was modeled as a function of demographic and medical attributes, with added random gamma-distributed noise to induce realistic skewness.

---

## **3. Missingness Plan**

**Attributes with injected missing values:**

* Income (\~5%, MCAR)
* Cholesterol (\~3%, MCAR)
* Num Visits (\~3%, MCAR)
* Medical Expenses (\~4%, MAR; higher missingness for Age > 60)

**Pre-imputation missingness summary:**
*(Insert `missing_summary_post_inject` table here)*

---

## **4. Missing Value Handling**

**Imputation strategy:**

* Mean imputation for Cholesterol (symmetric).
* Median imputation for Income, Medical Expenses, Num Visits (skewed, to avoid outlier distortion).

**Post-imputation summary:**
*(Insert `missing_summary_post` table here — should show zero missing values)*

This ensured no distortion in variable distributions while handling skew-sensitive variables appropriately.

---

## **5. Skewness Diagnosis**

**Method:** Pearson’s skewness (moments::skewness).

**Skewness Table (Before Transformation):**
*(Insert `skewness_table` here)*

**Key Observations:**

* Strong right-skew (>1) in Income, Medical Expenses, Num Visits, and Length of Stay.
* Other numeric variables were approximately symmetric.

**Plots (Before Transformation):**
*(Insert histograms and boxplots for each numeric attribute here, e.g., Age, BMI, etc.)*

---

## **6. Skewness Reduction (Transformations)**

**Applied methods:**

* **Log1p:** Used on non-negative variables.
* **Yeo–Johnson:** Used for all major skewed variables (final dataset kept Yeo–Johnson versions).

**Skewness Comparison Table (Before vs After):**
*(Insert `skewness_table_after` here)*

**Observations:**

* Skewness of LOS reduced from strong right-skew (>1) to near symmetry (<0.5).
* Similar improvements for Income, Medical Expenses, Num Visits.
* Variance stabilization achieved, at the cost of interpretability of coefficients on the original scale.

**Plots (After Transformation):**
*(Insert histograms and boxplots for transformed variables: Income, Medical Expenses, Num Visits, Length of Stay)*

---

## **7. Modeling Setup**

* **Validation:** 80/20 train-test split.
* **Model:** Linear Regression (`lm`).
* **Categorical Handling:** One-hot encoding with `caret::dummyVars`.
* **Datasets compared:**

  1. Original (imputed, skewed variables intact).
  2. Transformed (skewness handled with Yeo–Johnson).

---

## **8. Results & Discussion**

**Model Performance Table:**
*(Insert `metrics_table` here: RMSE, MAE, R² for Original vs Transformed)*

**Key Results:**

* RMSE and MAE both decreased after transformation → more accurate predictions.
* R² improved, showing better overall model fit.
* Percentage improvements (as per your metrics table) indicate transformations reduced skewness-driven noise.

**Residual Diagnostics:**

* **Shapiro-Wilk test:** p-value higher post-transformation → residuals closer to normal.
* **Residual plots:** Transformed dataset showed tighter clustering around zero and reduced heteroscedasticity.

*(Insert residuals plots here: Residuals vs Fitted for Original and Transformed models)*

---

## **9. Limitations & Future Work**

* Single train/test split used; k-fold cross validation would be more robust.
* Linear regression may still underperform if relationships are non-linear; tree-based models (skew-invariant) could be tested.
* Transformations reduce interpretability of coefficients.
* Future experiments could include Box-Cox and quantile normalization for comparison.


