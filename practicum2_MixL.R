################################################################################
# 
# Erasmus School of Economics
# Erasmus Choice Modelling Centre
#
# Course: New Research Methods in Data Analytics
# Course code: FEB 13069
# Academic year: 2025/2026
#
# Practicum 2: Choice model specification and estimation (MixMNL model)
# Topic: How to code a basic Mixed MNL model in R using Apollo?
#
# Instruction: To make the practicum assignments, you need to complete the code
#              below at the indicated lines which states "USER ACTION".
#
# Note: In case you have not installed R, Rstudio, and Apollo yet, please
#       follow the installation guide provided on Canvas.
#
# Any questions?
#
# Option 1: Search in the Apollo manual
#   - http://www.apollochoicemodelling.com/files/manual/Apollo.pdf
#
# Option 2: Search or ask your question on the Apollo forum
#   - http://www.apollochoicemodelling.com/forum/
#
# Option 3: Ask your teacher!
#
# v0.1 (Nov,2025)
# Created by:
#       Luis Pilli (l.e.pilli@eshpm.eur.nl)
#
##############################################################################

### Step 1: Clear memory
rm(list = ls())

### Step 2: Set working directory for R initialization
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Step 3: Load Apollo library
library(apollo)

### Step 4: Initialise Apollo code
apollo_initialise()

### Step 5: Set core controls
###     ATTENTION: Your inputs must be enclosed in quotes like "this"
apollo_control = list(
  # USER ACTION: Specify model name
  ##    Note: Change the model name for every model that you run
  modelName       = ,
  # USER ACTION: Provide model description
  ##    Note: Change the model description to reflect the current model
  modelDescr      = ,
  # USER ACTION: Specify the column with the respondent id
  indivID         = ,
  # USER ACTION: Set logical variable to activate estimation of random parameters
  mixing          = ,
  # Define number of cores used during estimation (used to speed up estimation time)
  nCores          = 5,
  # USER ACTION: Set path to the folder on your PC where the model results will be stored
  ##    Note: Use the "outputs" folder that was created by the pre-processing syntax
  outputDirectory = 
)

### Step 6: Load data
# Set path to directory on your PC where the dataset is stored
path_data = paste0("data",sep=.Platform$file.sep,"dataset.csv")
# Load dataset into global environment
database = read.csv(path_data, header=TRUE)

### Step 7: Initialise all parameters that needs to be estimated in your Mixed Logit model
# USER ACTION: Define the (1) mu parameters that estimate the sample mean, and
#                         (2) sigma parameters that estimate the sample distribution
#              Please, complete the list with the parameters that are missing.
#              Provide names for each parameter following by assigning a
#              starting value.
apollo_beta=c(mu_asc_out         = 0,
              sigma_asc_out      = 0)

### Step 8:
##  USER ACTION: Complete the list with parameters (as initialised above) that
##               should be kept fixed during estimation (in quotes); if none, keep empty
apollo_fixed = c()

### Step 9: Set parameters for generating draws
# USER ACTION: Define the number of one random variable for each sigma in apollo_beta
# Use the command line interNormDraws

apollo_draws = list(
  interDrawsType = "mlhs",
  interNDraws    = 200,
  interUnifDraws = c(),
  interNormDraws = c(inter_1,...),
  intraDrawsType = "mlhs",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

### Step 10: Create random parameters
# USER ACTION: Write every random coefficient function
# If necessary check the lecture slides
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["asc_out"]] = mu_asc_out + sigma_asc_out * inter_1
  randcoeff[["b_eff"]] = 
  randcoeff[["b_fneg"]] = 
  randcoeff[["b_freq_2yr"]] = 
  randcoeff[["b_freq_3yr"]] = 
  randcoeff[["b_wdiag_2wks"]] = 
  randcoeff[["b_wdiag_3wks"]] = 
  randcoeff[["b_wfup_4wks"]] = 
  randcoeff[["b_wfup_8wks"]] = 
  
  return(randcoeff)
}

### Step 11: Checkpoint for model inputs
apollo_inputs = apollo_validateInputs()

### Step 12: Define model and likelihood function
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
    
  ### Attach dataset inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  ### Create list of choice probabilities P
  P = list()
  
  ### List of utility functions: these must use the same names as in mnl_settings (see below), order is irrelevant
  V = list()
  
  # USER ACTION: Define utility function for alternative 1
  # Code "effectiveness" and "risk false negative" attributes as numerical variables
  V[["ALT1"]]  = 

  # USER ACTION: Define utility function for alternative 2
  # Code "effectiveness" and "risk false negative" attributes as numerical variables
  V[["ALT2"]]  = 
  
  # USER ACTION: Utility function for alternative 3 (i.e., opt-out)
  V[["ALT3"]] = 
  
  ### Define settings for MNL model component
  mnl_settings = list(
    # USER ACTION: Attach utility function to the choice alternative in your dataset
    alternatives  = c(ALT1=, ALT2=, ALT3=),
    # USER ACTION: Define which alternatives are "available" in each choice task
    #              In our study, all alternatives are "available"
    avail         = 1,
    # USER ACTION: Specify the column containing the chosen alternative
    choiceVar     = ,
    # USER ACTION: Attach list of utility functions
    utilities     = 
  )
  
  ### Compute choice probabilities using MNL model
  #### functionality="estimate" as the parameters will be updated for estimating the MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observations for same individual
  ### (i.e., considering the panel structure of the data)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ## Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

### Step 13: Model estimation
model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

### Step 14: Print model output with two-sided p-values
#### Note: if one-sided p-values are needed, set "printPVal=1" (p-values are not reported if set to "0")
modelOutput_setting=list(printPVal=2)
apollo_modelOutput(model, modelOutput_setting)

### Save model output with two-sided p-values
apollo_saveOutput(model, modelOutput_setting)

### Step 15: Estimate individual coefficients conditional on choice sequence
conditionals = apollo_conditionals(model,
                                   apollo_probabilities,
                                   apollo_inputs)

# Set path to directory on your PC where the conditionals will be stored
path_cond = paste0(apollo_control$outputDirectory,sep=.Platform$file.sep,"conditionals.RDS")
### Save conditionals
saveRDS(conditionals, file = path_cond)
