################################################################################
# 
# Erasmus School of Economics
# Erasmus Choice Modelling Centre
#
# Course: New Research Methods in Data Analytics
# Course code: FEB 13069
# Academic year: 2025/2026
#
# Practicum 2: Choice model specification and estimation (Latent Class Model)
# Topic: How to code and interpret a Latent Class model in R using Apollo?
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
################################################################################

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
  modelName = "LCMNL-6c",
  # USER ACTION: Provide model description
  ##    Note: Change the model description to reflect the current model
  modelDescr = "Latent Class MNL with 6 possible characteristics",
  # USER ACTION: Specify the column with the respondent id
  indivID = 1,
  # Define number of cores used during estimation (used to speed up estimation time)
  nCores = 5,
  # Define seed used for any random number generation
  seed = 100,
  # USER ACTION: Set path to the folder on your PC where the model results will be stored
  ##    Note: Use the "outputs" folder that was created by the pre-processing syntax
  outputDirectory = paste(getwd(),'outputs',"practicum_lcmodel_3class_covar",sep=.Platform$file.sep)
)

### Step 6: Load data
# Set path to the folder on your PC where the dataset is stored
path_data = paste(getwd(),'New Data set',"New Data set.xlsx",sep=.Platform$file.sep)
# Load dataset into global environment
database = read.csv(path_data, header=TRUE)
# Excel
library(readxl)
New_Data_set <- read_excel("New Data set.xlsx")

### Step 7: Initialise all parameters that needs to be estimated in your MNL model
# USER ACTION: Define the (1) class-specific and (2) class membership parameters
#              followed by assigning a starting value. The class-specific
#              alternative specific constant for the opt-out option and the
#              constants for the class membership models are already defined.
#              Please, complete the list with the parameters that are missing.
#              Provide names for each parameter following by assigning a
#              starting value.

# Prior estimates (from pilot project)
price_p <- -0.6
mode_p <- 0.3
packaging_p <- 0.25
time_p <- -0.5

apollo_beta=c(# Class 1
              asc_out_1      = 0,
              price_1 = price_p
              mode_1 = mode_p
              packaging_1 = packaging_p
              time_1 = time_p
              
              # Class 2
              asc_out_2      = 0,
              price_2 = price_p
              mode_2 = mode_p
              packaging_2 = packaging_p
              time_2 = time_p

              # Class membership - class 1
              delta_1        = 0,

              # Class membership - class 2
              delta_2        = 0.1,


### Step 8
##  USER ACTION: Complete the list with parameters (as initialised above) that
##               should be kept fixed during estimation (in quotes)
apollo_fixed = c("delta_2")

### Step 9: Define class membership model
apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  ## USER ACTION: Complete the empty lists by specifying the missing class-specific parameters
  ##              which are needed for the class-specific utility functions
  lcpars[["asc_out"]] = list(asc_out_1, asc_out_2)
  lcpars[["b_price"]] = list(price_1, price_2)
  lcpars[["b_mode"]] = list(mode_1, mode_2)
  lcpars[["b_packaging"]] = list(packaging_1, packaging_2)
  lcpars[["b_time"]] = list(time_1, time_2)
  
  ## List of class-membership functions:
  ##  These must use the same names as in classAlloc_settings (see below), order is irrelevant
  V=list()
  # USER ACTION: Define class-membership function for class 1
  V[["class_1"]] = delta_1

  # USER ACTION: Define class-membership functions for class 2
  V[["class_2"]] = delta_2

  # USER ACTION: Define class-membership functions for class 3
  # V[["class_3"]] = delta_3
  
  ## Define settings for class-membership model
  classAlloc_settings = list(
    # USER ACTION: Attach class-membership functions to the respective classes
    classes      = c(class_1=1, class_2=1),
    # USER ACTION: Define which classes are "available" in our study, all classes are "available"
    avail        = ,
    # USER ACTION: Attach list of class-membership functions
    utilities    = c(V[["class_1"]], V[["class_2"]])
  )
  
  lcpars[["pi_values"]] = apollo_classAlloc(classAlloc_settings)
  return(lcpars)
}

### Step 10: Checkpoint for model inputs
apollo_inputs = apollo_validateInputs()

### Step 11: Define model and likelihood function
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
    
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  ### Create list of choice probabilities P
  P = list()

  ### Define settings for MNL model component that are generic across classes
  mnl_settings = list(
    # USER ACTION: Attach utility functions to the alternatives in your dataset
    alternatives  = c(ALT1=, ALT2=),
    # USER ACTION: Define which alternatives are "available" in each choice task; in our study, all alternatives are "available"
    avail         = list(ALT1=, ALT2=),
    # USER ACTION: Specify the column containing the chosen alternative; beware, no dummies are used (!)
    choiceVar     = 
  )

  ### List of utility functions for each latent class: these must use the same names as in mnl_settings (see above), order is irrelevant
  # USER ACTION: Set number of latent classes you are estimating in the model
  ##       Note: You can call class-specific parameters by NAME_PARAM[[s]]; see example in ALT3 for the class-specific
  ##             alternative specific constant
  for(s in 1:2) #'REPLACE THIS WITH THE NUMBER OF CLASSES YOU WANT TO ESTIMATE'
    V=list()
    # USER ACTION: Define utility function for alternative 1 for class "s"
    V[["ALT1"]] = asc_out[[1]] + 

    # USER ACTION: Define utility function for alternative 2 for class "s"
    V[["ALT2"]] = 

    # USER ACTION: Define utility function for alternative 3 for class "s"
    V[["ALT3"]] = asc_out[[s]]

    mnl_settings$utilities = V
    mnl_settings$componentName = paste0("Class_",s)

    ### Compute within-class choice probabilities using MNL model
    P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    
    ### Take product across observations for same individual (i.e., considering the panel structure of the data)
    P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
  }

  ### Compute latent class model probabilities
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

### Step 12: Searching for starting value (recommended to ensure model convergence!)
apollo_beta = apollo_searchStart(apollo_beta,
                                 apollo_fixed,
                                 apollo_probabilities,
                                 apollo_inputs,
                                 searchStart_settings=list(nCandidates=2))

### Step 13: Model estimation
model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

### Step 14: Print model output with two-sided p-values
### Note: if one-sided p-values are needed, set "printPVal=1" (p-values are not reported if set to "0")
modelOutput_setting=list(printPVal=2)
apollo_modelOutput(model, modelOutput_setting)

### Save model output with two-sided p-values
apollo_saveOutput(model, modelOutput_setting)