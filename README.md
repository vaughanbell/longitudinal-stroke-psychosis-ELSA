Analysis code and results for the study

# Longitudinal associations of stroke and psychosis: a population-based study of older adults

<p align="center">
	<a href="https://en.wikipedia.org/wiki/R_(programming_language)"><img
		alt="R Programming Language"
		src="https://img.shields.io/badge/Language-R-%232268BB.svg"></a>
	<a href="https://en.wikipedia.org/wiki/Project_Jupyter#Jupyter_Notebook"><img
		alt="Jupyter Notebook"
		src="https://img.shields.io/badge/Jupyter-Notebook-68B7EB"></a>
	<a href="https://opensource.org/licenses/MIT"><img
		alt="MIT License"
		src="https://img.shields.io/badge/license-MIT-blue.svg"></a>
</p>

Code written by Alvin Richards-Belle with review and modifications by Vaughan Bell

Publication status: currently awaiting [pre-print](https://not_online_yet).

This archive contains the R code for the analysis reported in the above study. The code is presented as a base R script and [Jupyter notebooks](https://jupyter-notebook-beginner-guide.readthedocs.io/en/latest/what_is_jupyter.html) which are documents that combine both code and the output in a form that can be viewed online, but also re-run and the results reproduced when accompanied by the original datasets.

This repository contains:

1.  [Stroke Psychosis ELSA Pre-processing.R](https://github.com/vaughanbell/longitudinal-stroke-psychosis-ELSA/blob/main/Stroke_Psychosis_ELSA_Pre-processing.R) - a pre-processing script that recodes and merges ELSA data files into a single .rda file for analysis by Jupyter notebooks
2.  [Richards-Belle_et_al_Descriptives_Tables.ipynb](https://github.com/vaughanbell/longitudinal-stroke-psychosis-ELSA/blob/main/Richards-Belle_et_al_Descriptives_Tables.ipynb) - A Jupyter notebook that calculates the demographic and descriptive statistics reported in the study
3.  [Richards-Belle_et_al_LogisticRegressions.ipynb](https://github.com/vaughanbell/longitudinal-stroke-psychosis-ELSA/blob/main/Richards-Belle_et_al_LogisticRegressions.ipynb) - A Jupyter notebook that reports the logistic regression analyses 
4.  [Richards-Belle_et_al_SurvivalAnalyses.ipynb](https://github.com/vaughanbell/stroke-psychosis-bidirectional-trajectory/blob/main/Richards-Belle_et_al_SurvivalAnalyses.ipynb) - A Jupyter notebook that reports the survival analyses

### Dataset

This study used data from the [English Longitudinal Study of Ageing](https://www.elsa-project.ac.uk/) (ELSA).

ELSA is a "panel study of a representative cohort of men and women living in England aged ≥50 years." that is "multidisciplinary in orientation, involving the collection of economic, social, psychological, cognitive, health, biological and genetic data. The study commenced in 2002, and the sample has been followed up every 2 years" [(Steptoe et al, 2013)](https://doi.org/10.1093/ije/dys168)

The data can be download from the [UK Data Service](https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=5050).

### Platform and package versions

To aid reproducibility, the platform (hardware, operating system), R language version, and package versions used to generate the results are automatically generated at runtime and reported at the end of the Jupyter notebooks.
