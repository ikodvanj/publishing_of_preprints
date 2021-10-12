Accompanying git repository with code and data used in the article "Publishing of COVID-19 Preprints in Peer-reviewed Journals, Preprinting Trends, Public Discussion and Quality Issues" by Kodvanj I, Homolak J, Virag D, Trkulja V (2020 bioRxiv, https://doi.org/10.1101/2020.11.23.394577v3)

Repo contains:

  * analysis.R - R code used for the analysis
  * sample.RDS - list of randomly sampled articles used to check the submission and acceptance dates of published articles (contains DOIs, and submission/acceptance dates)
  * data.rds - primary data analyzed in the publication. Each row represents one preprint for which the following is known: title, source (bioRxiv/medRxiv), topic (COVID-19 and non-COVID-19), the publication date of first and last version, number of versions and Altmetric data, and number of posts on bioRxiv/medRxiv page. Published preprints have DOI of a published version and submission and acceptance date and the length of peer-review, 
  * usage.rds - monthly statistics about the number of abstract views, full-text views, pdf downloads, and their cumulative number.
  * pubmed_retracted - matrix containing info about the number of COVID-19 and NON-COVID-19 articles that were and were not retracted

 
Analysis.R dependencies:

  * stringr
  * dplyr
  * lubridate
  * ggplot2
  * ggpubr
  * stringdist
  * devEMF
  * ggpubr
  * survival
  * ggsurvplot
  * ggsci
  * logbin
  * survminer


Used R version: 4.0.2 -- "Taking Off Again"