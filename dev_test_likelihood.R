# dev_test_likelihood.R
# Test data for .befCalculateLikelihoodNew, .befCalculateLikelihoodOld,
# and .befUpdateBioLike (standalone, no JASP needed)
#
# Scenario
#   Batches 1-3 already processed; 3 known species: Pigeon, Duck, Cat
#   Batch 4 arrives: contains Pigeon & Duck (seen) + Deer (unseen, first ever)

source("R/LSBiodiversityestimationfull.R")

# ── 0. Shared constants ────────────────────────────────────────────────────────

abundanceS    <- 1:10
abundanceSMax <- 10

# ── 1. abundanceState (state after 3 batches) ──────────────────────────────────
# 3 known species: Pigeon (18 obs), Duck (14 obs), Cat (7 obs)
# currentK = 3  →  S < 3 columns are pruned

alpha_matrix <- matrix(NA_real_, nrow = abundanceSMax, ncol = length(abundanceS))
for (col_idx in seq_along(abundanceS))
  alpha_matrix[1:abundanceS[col_idx], col_idx] <- 1

rownames(alpha_matrix) <- c("Pigeon", "Duck", "Cat", paste0("unknown_", 4:abundanceSMax))
colnames(alpha_matrix) <- paste0("S=", abundanceS)   # "S=1" … "S=10"

# Add cumulative New-individual counts from batches 1-3
cumulative_new <- c(Pigeon = 18, Duck = 14, Cat = 7)
for (spe in names(cumulative_new))
  alpha_matrix[spe, ] <- alpha_matrix[spe, ] + cumulative_new[spe]

# Prune S < currentK = 3
currentK     <- 3
keep         <- abundanceS >= currentK
alpha_matrix <- alpha_matrix[, keep, drop = FALSE]
s_values     <- abundanceS[keep]

abundanceState <- list(
  alphaMatrix  = alpha_matrix,
  speciesNames = rownames(alpha_matrix),
  s_values     = s_values,
  currentK     = currentK
)

cat("=== abundanceState$alphaMatrix ===\n")
print(abundanceState$alphaMatrix)

# ── 2. bioLikeState (state after 3 batches) ────────────────────────────────────

S_values <- 1:10
like_df  <- data.frame(S = S_values, stringsAsFactors = FALSE)
like_df[["batch_1"]]    <- c(rep(-Inf, 2), -8.2, -9.1, -9.8, -10.3, -10.9, -11.4, -11.9, -12.3)
like_df[["batch_2"]]    <- c(rep(-Inf, 2), -7.5, -8.4, -9.0,  -9.6, -10.1, -10.6, -11.0, -11.5)
like_df[["batch_3"]]    <- c(rep(-Inf, 2), -6.9, -7.8, -8.3,  -8.8,  -9.3,  -9.7, -10.2, -10.6)
like_df[["cumulative"]] <- rowSums(like_df[, c("batch_1","batch_2","batch_3")], na.rm = FALSE)

bioLikeState <- list(
  likeDF       = like_df,
  batch_count  = 3,
  seen_species = c("Pigeon", "Duck", "Cat")
)

cat("\n=== bioLikeState$likeDF ===\n")
print(bioLikeState$likeDF)
cat("\nbioLikeState$seen_species:", bioLikeState$seen_species, "\n")

# ── 3. batch_data (batch 4) ────────────────────────────────────────────────────
# Deer = species-level unseen (first time ever)
# Pigeon, Duck = seen species (but some individuals are new IDs)

batch_data <- data.frame(
  species = c("Pigeon", "Pigeon", "Pigeon", "Duck", "Duck", "Deer", "Deer"),
  id      = c(201, 202, 15,  203, 8,  204, 205),
  status  = c("New","New","Old", "New","Old", "New","New"),
  stringsAsFactors = FALSE
)

cat("\n=== batch_data (batch 4) ===\n")
print(batch_data)

# ── 4. Derived inputs (mirrors .befUpdateBioLike logic) ────────────────────────

new_animals        <- batch_data$species[batch_data$status == "New"]
new_animals_counts <- table(new_animals)
new_animals_names  <- names(new_animals_counts)
seen_species       <- bioLikeState$seen_species
unseen_species     <- setdiff(new_animals_names, seen_species)   # "Deer"

cat("\n=== new_animals_counts ===\n"); print(new_animals_counts)
cat("unseen_species:", unseen_species, "\n")

# After subtracting the first sighting of each unseen species
remaining_counts <- new_animals_counts
for (name in unseen_species)
  remaining_counts[name] <- remaining_counts[name] - 1
remaining_counts <- remaining_counts[remaining_counts > 0]

cat("remaining_counts (after removing first sighting of unseen):\n")
print(remaining_counts)

# ── 5. Test .befCalculateLikelihoodNew ─────────────────────────────────────────

cat("\n=== Test .befCalculateLikelihoodNew ===\n")
result_new <- .befCalculateLikelihoodNew(abundanceState, bioLikeState, unseen_species)
cat("likelihood (lik_new):\n"); print(result_new$likelihood)
cat("updated alphaMatrix rows for Deer:\n")
print(result_new$abundanceState$alphaMatrix["Deer", ])

# ── 6. Test .befCalculateLikelihoodOld ─────────────────────────────────────────

cat("\n=== Test .befCalculateLikelihoodOld ===\n")
result_old <- .befCalculateLikelihoodOld(result_new$abundanceState, remaining_counts)
cat("likelihood (lik_old):\n"); print(result_old$likelihood)

# ── 7. Combined batch likelihood ───────────────────────────────────────────────

batch_log_like <- result_new$likelihood + result_old$likelihood
cat("\n=== Combined batch_log_like (lik_new + lik_old) ===\n")
print(round(batch_log_like, 4))

# ── 8. Edge case: unseen_species is empty ──────────────────────────────────────

cat("\n=== Edge case: unseen_species = character(0) ===\n")
result_empty <- .befCalculateLikelihoodNew(abundanceState, bioLikeState, character(0))
cat("likelihood (should be all 0):\n"); print(result_empty$likelihood)

# ── 9. Edge case: all new animals are first sightings (sum remaining == 0) ──────

cat("\n=== Edge case: only one Deer (remaining_counts all zero) ===\n")
counts_one_deer <- table(c("Deer"))
result_one <- .befCalculateLikelihoodNew(abundanceState, bioLikeState, "Deer")
remaining_zero <- table(c("Deer"))[c("Deer")] - 1   # = 0
cat("remaining after subtract:", remaining_zero, "\n")
cat("sum == 0:", sum(remaining_zero) == 0, "→ should early-return in UpdateBioLike\n")
