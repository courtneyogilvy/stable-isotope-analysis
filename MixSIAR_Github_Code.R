

library(MixSIAR)


# 1. Load data ------------------------------------------------------------

# Load consumer data
# Replace the system.file call with the path to your file
mix.filename <- read.csv("filename_here.csv")

mix <- load_mix_data(filename= "filename_here.csv",
                     iso_names=c("d13C","d15N"),
                     factors=NULL,
                     fac_random=NULL,
                     fac_nested=NULL,
                     cont_effects=NULL)

#Load source data

# Replace the system.file call with the path to your file
source.filename <- read.csv("source_filename.csv")

source <- load_source_data(filename="source_filename.csv",
                           source_factors=NULL,
                           conc_dep=FALSE,
                           data_type="means",
                           mix)

# Load discrimination data

# Replace the system.file call with the path to your file
discr.filename <- read.csv("discrimination_filename.csv")
discr <- load_discr_data(filename="driscrimination_filename.csv", mix)



# 2. Check data are loaded correctly --------------------------------------

# Make an isospace plot
plot_data(filename="isospace_plot", plot_save_pdf=FALSE, plot_save_png=TRUE, mix,source,discr)



# 3. Plot uninformative prior -----------------------------------

# Plot uninformative prior
plot_prior(alpha.prior=1, source, filename = "orior_filename", plot_save_png = TRUE)


# 4. Select model parameters ----------------------------------------------


# Define model structure and write JAGS model file
model_filename <- "model_output_filename"   # Name of the JAGS model file
resid_err <- TRUE
process_err <- TRUE
write_JAGS_model(model_filename, resid_err, process_err, mix, source)


# 5. Run model ------------------------------------------------------------

# Run the JAGS model
#jags.uninf <- run_model(run="test",mix,source,discr,model_filename)
jags.uninf <- run_model(run="normal",mix,source,discr,model_filename)

# Process diagnostics, summary stats, and posterior plots
output_JAGS(jags.uninf, mix, source)
