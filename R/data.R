#' @title Yeast protein localisations.
#'
#' @description A dataset containing the subcellular protein localisations along with
#' several amino acid sequence based metrics used to make a classification
#' of the localisation. `yeast` contains the full dataset while `yeast_classes` is
#' simply `class` collumn from the `yeast` dataset as a vector, for convenience
#' in the practical excercise.
#'
#' @format A data frame with 1484 rows and 10 variables:
#' \describe{
#'   \item{seq}{Accession number for the SWISS-PROT database}
#'   \item{mcg}{McGeoch's method for signal sequence recognition}
#'   \item{gvh}{von Heijne's method for signal sequence recognition}
#'   \item{alm}{Score of the ALOM membrane spanning region prediction program}
#'   \item{mit}{Score of discriminant analysis of the amino acid content of
#'              the N-terminal region (20 residues long) of mitochondrial and
#'              non-mitochondrial proteins}
#'   \item{erl}{Presence of "HDEL" substring (thought to act as a signal for
#'              retention in the endoplasmic reticulum lumen). Binary attribute}
#'   \item{pox}{Peroxisomal targeting signal in the C-terminus}
#'   \item{vac}{Score of discriminant analysis of the amino acid content of
#'              vacuolar and extracellular proteins}
#'   \item{nuc}{Score of discriminant analysis of nuclear localization signals
#'              of nuclear and non-nuclear proteins}
#'   \item{class}{Experimentally observed subcellular localisations.
#'
#'                Class Distribution. The class is the localization site.
#'
#'                CYT (cytosolic or cytoskeletal)                    463
#'
#'                NUC (nuclear)                                      429
#'
#'                MIT (mitochondrial)                                244
#'
#'                ME3 (membrane protein, no N-terminal signal)       163
#'
#'                ME2 (membrane protein, uncleaved signal)            51
#'
#'                ME1 (membrane protein, cleaved signal)              44
#'
#'                EXC (extracellular)                                 37
#'
#'                VAC (vacuolar)                                      30
#'
#'                POX (peroxisomal)                                   20
#'
#'                ERL (endoplasmic reticulum lumen)                    5}
#'   ...
#' }
#' @source Paul Horton & Kenta Nakai, ["A Probablistic Classification System for Predicting the Cellular Localization Sites of Proteins"](https://www.aaai.org/Papers/ISMB/1996/ISMB96-012.pdf), Intelligent Systems in Molecular Biology, 109-115. St. Louis, USA 1996.
"yeast"

#' Classes for the yeast data
#'
#' This is a subset of the \link{yeast} data set. See the \link{yeast} help page for further info
"yeast_classes"

#' Measurements of sequence recognition from yeast proteins with different subcellular localisations
#'
#' This is a subset of the \link{yeast} data set. See the\link{yeast} help page for further info
"two_classes"

#' @title Infectious disease outbreaks in the USA, data from the CDC
#'
#' @description This dataset provides data on foodborne disease outbreaks reported to CDC from 1998 
#' through 2015. Data fields include year, state (outbreaks occurring in more than one state are listed 
#' as "multistate"), location where the food was prepared, reported food vehicle and contaminated 
#' ingredient, etiology (the pathogen, toxin, or chemical that caused the illnesses), status (whether 
#' the etiology was confirmed or suspected), total illnesses, hospitalizations, and fatalities. 
#' In many outbreak investigations, a specific food vehicle is not identified; for these outbreaks, 
#' the food vehicle variable is blank.
#' 
#' @source https://www.kaggle.com/cdc/foodborne-diseases
"outbreaks"
