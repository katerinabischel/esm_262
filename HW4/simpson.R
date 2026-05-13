# ---------------------------------------------------------------
# simpson_index
# ---------------------------------------------------------------
# Computes Simpson's Biodiversity Index (D) for a sample of
# species observations.
#
# Simpson's Index measures the probability that two individuals
# randomly selected from a sample will belong to the same species.
# Values range from 0 to 1, with LOWER values associated with
# LOWER diversity under the convention used in this assignment
# (D itself; note some texts report 1 - D as the "diversity" form).
#
# Inputs
#   species : a vector of species observations - one entry per
#             individual. Can be character, numeric, or factor.
#             Example: c("oak","oak","pine","maple","oak","pine")
#   method  : which formula to use
#               "discrete"     -> D = sum( n_i * (n_i - 1) ) / ( N * (N - 1) )
#               "proportional" -> D = sum( (n_i / N)^2 )
#             Default is "discrete".
#
# Output
#   A single numeric value between 0 and 1 - Simpson's Index (D)
#
# Errors
#   Stops with an informative message if fewer than 2 individuals
#   are provided, if NA values are present, or if an unknown
#   method is requested.
#
# Author: Katerina Bischel
# Date:   May 2026
# ---------------------------------------------------------------

simpson_index = function(species, method = "discrete") {

  # ----- input validation ---------------------------------------
  if (missing(species)) {
    stop("must supply a vector of species observations")
  }

  if (any(is.na(species))) {
    stop("species vector contains NA values - please remove or impute them")
  }

  if (!(method %in% c("discrete", "proportional"))) {
    stop("method must be either 'discrete' or 'proportional'")
  }

  # ----- count individuals per species (hint: use factors) ------
  species_factor = as.factor(species)
  counts         = as.numeric(table(species_factor))
  N              = sum(counts)

  if (N < 2) {
    stop("need at least 2 individuals to compute Simpson's Index")
  }

  # ----- compute Simpson's Index --------------------------------
  if (method == "discrete") {
    D = sum(counts * (counts - 1)) / (N * (N - 1))
  } else {
    D = sum((counts / N)^2)
  }

  return(D)
}
