# Model Card: Blue Tarp Pixel Classifier

## Model Details

- **Model type:** Penalized Logistic Regression (Elastic Net)
- **Framework:** R `caret` + `glmnet`
- **Input:** Three integer features — Red, Green, Blue (0–255 per channel)
- **Output:** Probability that a pixel belongs to a blue tarp; binary label at threshold 0.6
- **Hyperparameters:** Tuned via 10-fold stratified CV (alpha and lambda selected by AUC)
- **Version:** 1.0
- **License:** MIT

## Intended Use

- **Primary use:** Identify blue tarps (emergency shelters for displaced persons) in satellite imagery of disaster zones to assist humanitarian response teams in locating affected populations.
- **Intended users:** Aid organizations, disaster response teams, and remote sensing analysts triaging satellite imagery after natural disasters.
- **Out-of-scope uses:** This model should not be used as the sole basis for resource allocation decisions. It is a screening tool — predictions require human review before directing ground teams.

## Training Data

- **Source:** Satellite imagery of Port-au-Prince, Haiti, collected after the January 2010 earthquake by Rochester Institute of Technology.
- **Size:** 63,241 pixel observations.
- **Features:** RGB color channels extracted at the pixel level.
- **Class distribution:** 3.2% positive (Blue Tarp) / 96.8% negative — severe class imbalance.
- **Labeling:** Ground truth derived from expert-annotated image regions. The `Class` column includes Blue Tarp, Rooftop, Soil, Various Non-Tarp, and Vegetation. All non-tarp classes are collapsed to a single negative label.

## Evaluation Data

- **Source:** Same satellite collection campaign as training data, held out during model development.
- **Size:** ~2 million pixel observations.
- **Class distribution:** 0.73% positive — even more imbalanced than training data.

## Performance

All metrics measured on the holdout set at threshold = 0.6:

| Metric | Value |
|--------|:-----:|
| AUROC | 99.97% |
| Balanced Accuracy | 98.43% |
| Kappa | 87.13% |
| F1 | 87.23% |
| False Positive Rate | 0.19% |
| Deploy Time | 1.10 s |

### Performance Relative to Other Models

The PLR was selected over six alternatives (Logistic Regression, LDA, QDA, KNN, Random Forest, SVM). The SVM achieved the best cross-validation performance but the PLR generalized better to the holdout set, likely because the Elastic Net penalty mitigates multicollinearity among the RGB features (pairwise correlations >= 0.94).

## Limitations

- **Single geography:** Trained and evaluated exclusively on imagery from Port-au-Prince, Haiti. Performance on other disaster sites, climates, or satellite sensors is unknown.
- **RGB only:** Uses raw RGB values without color space transformations (e.g., HSL, CIELUV). Blue tarps may be confused with other blue objects (pools, painted roofs) in different contexts.
- **Pixel-level only:** Does not incorporate spatial context, object boundaries, or texture. A cluster of tarp-classified pixels still needs to be aggregated into actionable locations.
- **Temporal sensitivity:** Tarp color fades over time due to sun exposure. Model performance may degrade on imagery captured weeks or months after initial deployment of tarps.
- **Resolution dependence:** Performance depends on the spatial resolution of the input imagery matching the training data. Resampled or compressed imagery may produce different RGB distributions.

## Ethical Considerations

- **False negatives have human cost.** A missed tarp means aid workers may not locate a displaced household. The 0.6 threshold was chosen to balance FPR against recall — lowering it would catch more tarps but increase false alarms.
- **False positives waste scarce resources.** In a disaster response, sending teams to non-tarp locations diverts effort from actual need. The model's 0.19% FPR is low but non-zero across millions of pixels.
- **Not a substitute for ground truth.** Model output should be treated as a prioritization signal, not a definitive assessment. Human review and ground verification remain essential.
- **Consent and privacy.** Satellite imagery can reveal the locations of vulnerable populations. Model outputs and underlying imagery should be handled with appropriate data governance, shared only with authorized response organizations.
- **Equity in coverage.** The model was trained on imagery from a single urban area. Rural or peri-urban disaster zones may have different spectral characteristics, and relying on this model without validation could create coverage gaps that disproportionately affect those populations.

## Recommendations

1. Validate on imagery from at least one additional disaster site before operational deployment.
2. Explore color space transformations (HSL, CIELUV) to improve robustness to lighting variation.
3. Pair pixel-level predictions with spatial clustering to produce location-level alerts.
4. Establish a human-in-the-loop review process for all model-flagged regions before dispatching ground teams.
