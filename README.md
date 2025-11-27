# LCMNL-Mixed-MNL-segmentation-R
Discrete choice modelling of a DCE dataset in R using Apollo, with segmentation via Multinomial Logit, Latent Class, and Mixed Logit models.

This project implements discrete choice models on a stated-preference dataset from a Discrete Choice Experiment (DCE), using the Apollo package in R. The code estimates a baseline Multinomial Logit (MNL) model, then introduces Latent Class MNL for segmenting respondents into preference classes, and a Mixed Logit (MixL) specification to model continuous heterogeneity in tastes.

The workflow follows Apollo’s building blocks (inputs, likelihood, estimation, output): loading and structuring the DCE data, specifying class-specific and random coefficients, setting up simulation draws for the Mixed Logit, and exporting model output for interpretation. The project is a compact example of how to use Apollo for segmentation and preference heterogeneity analysis in DCE data.
