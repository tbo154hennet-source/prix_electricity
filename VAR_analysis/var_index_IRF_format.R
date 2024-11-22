# Generalized function to process lambda matrices
process_lambda_matrices <- function(lambda_array) {
  lapply(seq(dim(lambda_array)[3]), function(i) {
    slice <- lambda_array[,,i]  # Extract the i-th slice
    slice_df <- 0.01 * slice + 1  # Transform the slice
    apply(slice_df, 2, cumprod)  # Compute cumulative product
  }) |> simplify2array()  # Combine into a 3D array
}

# Function to calculate quantiles and store in a 4D array
calculate_quantiles <- function(lambda_indices, quantile_probs, irhor, n) {
  strong_instruments <- array(NA, dim = c(2, irhor, n, 2))
  dimnames(strong_instruments) <- list(
    c("lower", "upper"), NULL, NULL, c("lambda_01", "lambda_02")
  )
  
  for (lambda_idx in 1:2) {
    lambda_name <- paste0("lambda_0", lambda_idx, "_index")
    quantiles <- apply(lambda_indices[[lambda_name]], c(1:2), function(x) quantile(x, probs = quantile_probs))
    strong_instruments[,,,lambda_idx] <- array(quantiles, dim = c(irhor, n, 2))
  }
  
  strong_instruments
}

# Apply the processing for both MBB and GK matrices
lambda_names <- c("lambda_01_index", "lambda_02_index")

for (lambda_idx in 1:2) {
  lambda_name <- paste0("ncvg_lambda_0", lambda_idx)
  sample_matrices_MBB[[lambda_names[lambda_idx]]] <- process_lambda_matrices(sample_matrices_MBB[[lambda_name]])
  sample_matrices_GK[[lambda_names[lambda_idx]]] <- process_lambda_matrices(sample_matrices_GK[[lambda_name]])
}

# Define dimensions and quantile probabilities
irhor <- dim(sample_matrices_MBB[["lambda_01_index"]])[1]
n <- dim(sample_matrices_MBB[["lambda_01_index"]])[2]
quantile_probs <- c(0.05, 0.95)

# Calculate quantiles for MBB and GK matrices
strong_instruments_lambda_index_MBB <- calculate_quantiles(sample_matrices_MBB, quantile_probs, irhor, n)
strong_instruments_lambda_index_GK <- calculate_quantiles(sample_matrices_GK, quantile_probs, irhor, n)

#strong_instruments_lambda_index_MBB["lower",,,"lambda_01"]
#dim(strong_instruments_lambda_index_MBB)
#dim(strong_instruments_lambda_index_GK)

# Permute dimensions for final arrays
strong_instruments_lambda_index_MBB <- aperm(strong_instruments_lambda_index_MBB, perm = c(2, 3, 1, 4))
strong_instruments_lambda_index_GK <- aperm(strong_instruments_lambda_index_GK, perm = c(2, 3, 1, 4))

# Print dimensions for verification
cat("Dimensions of strong_instruments_lambda_MBB:", dim(strong_instruments_lambda_MBB), "\n")
cat("Dimensions of strong_instruments_lambda_GK:", dim(strong_instruments_lambda_GK), "\n")
