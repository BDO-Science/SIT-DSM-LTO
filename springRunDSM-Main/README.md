# CVPIA LCM_LTO Application
A wrapper for running WINTER RUN CHINOOK SALMON SCIENCE INTEGRATION TEAM MODEL and SPRING RUN CHINOOK SALMON SCIENCE INTEGRATION TEAM MODEL as part of the Long-term Operation of the CVP.
**November 2023**

## Primary Authors:                                                     
 Alex Jensen
 U.S. Bureau of Reclamation, Bay-Delta Office, Science Division
 ajensen@usbr.gov
 
 Lisa Elliott
 U.S. Bureau of Reclamation, Bay-Delta Office, Science Division
 lelliott@usbr.gov


Based on the SCIENCE INTEGRATION TEAM MODELS originally produced by:
James T. Peterson                                                    
U.S. Geological Survey, Oregon Cooperative Fish and Wildlife         
Research Unit, Oregon State University                               
Corvallis, Oregon 97331-3803, jt.peterson@oregonstate.edu            
                                                                     
Adam Duarte                                                          
Oregon Cooperative Fish and Wildlife Research Unit,                  
Oregon State University,                                             
Corvallis, Oregon 97331-3803, adam.duarte@oregonstate.edu

## Disclaimers:
The following disclaimer and license are associated with the original SIT DSM model code: Although this code has been processed successfully on a computer system at the U.S. Geological Survey (USGS), no warranty expressed or implied is made regarding the display or utility of the code for other purposes, nor on all computer systems, nor shall the act of distribution constitute any such warranty. The USGS or the U.S. Government shall not be held liable for improper or incorrect use of the  code  described and/or contained herein. License: [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/)
IP-117068 

We modified the code with the purpose of comparing alternative operations as part of the LTO Reinitiation of Consultation for the CVP. The changes to the code mean that the revised models, resulting output, and any summary does not represent views of CVPIA SIT.Although this code has been processed successfully on a computer system at the U.S. Bureau of Reclamation, no warranty expressed or implied is made regarding the display or utility of the code for other purposes, nor on all computer systems, nor shall the act of distribution constitute any such warranty. The U.S. Bureau of Reclamation or the U.S. Government shall not be held liable for improper or incorrect use of the  code  described and/or contained herein.

## Overview of repository
Each run of interest (spring-run and winter-run) has its own folder within the repository (./springRunDSM-Main and ./winterRunDSM-Main/) that contains the original SIT DSM code and additional scripts to use new inputs to run the model. 
In addition, each run has a dedicated folder housing the original code to calibrate that run's model and additional wrapper scripts to re-calibrate the model based on new inputs (./springRunDSM-Main_Calibration/ and ./winterRunDSM-Main_Calibration/). 
Finally, there are folders that can be used to house the new alternatives' raw data (in .dss formats) for flow (./cvpiaFlow) and temperature (./cvpiaTemperature), along with scripts for converting the raw data into the inputs needed for the SIT DSMs.    

## How to run the models

	## To set up and compare model inputs for both the winter-run and spring-run models (only need to conduct this step once)

**Save** the desired raw data from CalSim 3 or HEC-5Q (.dss format) to the appropriate folder (./cvpiaFlow or ./cvpiaTemperature)
	-*Note* .dss files are not provided due to file quantity and size limitations

**Run** ./cvpiaFlow/Scripts/DSS workflow_cvpiaFlow_clean_CalSim 3.R or ./cvpiaFlow/Scripts/DSS workflow_cvpiaFlow_clean.R in order to access CalSim 3 or CalSim II files, respectively, and create flow data objects used as inputs for the SIT DSMs.
	-Scripts are located in ./cvpiaFlow/Scripts
	-New SIT DSM inputs are saved to the ./cvpiaFlow/Final Data Objects/ folder
	-These scripts will NOT run without obtaining the corresponding .dss files for each alternative
	-Additionally, these scripts require users to have installed the HEC-DSSVUE program and update relavant directories in the R script

**Run (OPTIONAL)** ./cvpiaFlow/Scripts/Input Plotting Script_Alternatives.R to plot and compare flow inputs from the originalDSM (not correcting total diversions in Upper Sac) and the NAA alternative. 
	-Resulting figures are saved in the ./cvpiaFlow/Plots/OG vs CalSim 3 NAA/ folder

**Run** ./cvpiaTemperature/Scripts/DSS workflow_cvpiaTemperature_CalSim 3.R to R script used to access HEC-5Q DSS outputs for specified alternatives and create new temperature data objects utilized in SIT DSMs. 
	-This script will NOT run without obtaining the corresponding .dss files for each alternative
	-New SIT DSM inputs are saved to the ./cvpiaTemperature/Final Data Objects/ folder
	-Additionally, this script requires users to have installed the HEC-DSSVUE program and update relavant directories in the R script

**Run (OPTIONAL)** ./cvpiaTemperature/Scripts/Temperature Input Plotting Script_Alternatives.R to plot and compare temperature inputs from the originalDSM and the NAA alternative. 
	-Resulting figures are saved in the ./cvpiaTemperature/Plots/originalDSM vs LTO NAA/ folder.

	## To run the winter- or spring-run DSMs for each alternative:
		## Winter-run-specific code and results are provided in ./winterRunDSM-Main/ ("Run"=winterRunDSM-Main)
		## Spring-run-specific code and results are provided in ./springRunDSM-Main/ ("Run"=springRunDSM-Main)

**Start** a new R session within the 'CVPIA LCM_LTO Application' R project, 
	**Open** ./"Run"/Scripts/Wrapper script.R
	**Update** the alternative name as "alt" in the ./"Run"/Scripts/Wrapper script.R file, line 89/90
	**Run** script. Model output is stored in ./"Run"/Output/ subfolder. 
**Note** for running ./"Run"/Scripts/Wrapper script.R:
    # *Be sure to start a new R session before running models to prevent clashes between packages!
    # *Users will need to install the cvpiaData package from the FlowWest GitHub, in order to load base model inputs
      # e.g., remotes::install_github("FlowWest/CVPIAdata")
    # *We also recommend users install other dependent data packages, for documentation purposes
      # e.g., remotes::install_github("FlowWest/cvpiaHabitat")
      # remotes::install_github("FlowWest/cvpiaFlow")
      #remotes::install_github("FlowWest/cvpiaTemperature")
  # Changes to the run_scenarios() function
    #run_scenarios() function (renamed life_cycle_model()) is located in its own script (located in 'functions' sub-folder) and is called in this script
    # The new input, 'alt', tells the model to use the original or updated inputs and needs to be specified before running the script. 
	# If the 'alt' is new, it will need to be added to the list of possible options in the ./"Run"/functions/Load Model Inputs.R script.
      # There are slight differences in the data structure between the original and updated inputs.
    # Additional changes to this function are described in the corresponding script
  # Changes to 'Deterministic Runs'
    # Removed the for loop, so it only calls one scenario at a time (scenario 0, or no habitat addition)
    # Allows specification of the CalSim alternative, or 'alt'
      # Requires a valid name is provided, or an error is thrown
      # If a new alternative is named, it pulls updated inputs from the './cvpiaFlow/Final Data Objects/' sub-folder
    # Imports seed number of spawners, to start simulation at yr2=6
    # Output from life_cycle_model() is saved as an .rds file in the './"Run"/Output/' sub-folder
  # Changes to 'Stochastic Runs'
    # Created new custom data objects for storing results across iterations
    # Allows specification of the CalSim alternative, or 'alt'
      # Requires a valid name is provided, or an error is thrown
      # If a new alternative is named, it pulls updated inputs from the './"Run"/cvpiaFlow/Final Data Objects/' and ./"Run"/cvpiaTemperature/Final Data Objects/ sub-folders
    # Imports seed number of spawners, to start simulation at yr2=6
    # Output from life_cycle_model() is saved as an .rds file in the 'Output' sub-folder

	###To visualize new inputs and model outputs:

# To visualize new inputs: 
**Run** ./cvpiaFlow/Input Plotting Script_Alternatives.R and **change** the "alts" list (line 43) to include any alternatives of interest for comparison and **set** the name to use for comparison as "comp" (line 44). 
	**Specify** in lines 47-50 which flow variables (flow_vars) you want to visualize. 
	The resulting plots will be saved to ./cvpiaFlow/Plots/"comp"/, with "comp" referring to the name set in line 44. 
	*Note* The script is currently only set up to compare two alternatives.
**Run** ./cvpiaTemperature/Temperature Input Plotting Script_Alternatives.R and **change** the "alts" list (line 44) to include any alternatives of interest for comparison and **set** the name to use for comparison as "comp" (line 46). 
	**Specify** in lines 55 which variables (temp_vars) you want to visualize. 
	The resulting plots will be saved to ./cvpiaTemperature/Plots/"comp"/, with "comp" referring to the name set in line 46. 
	*Note* The script is currently only set up to compare two alternatives.

# To visualize and compare new outputs across alternatives
	# Winter-run
**Run** ./winterRunDSM-Main/Scripts/Plotting_9.14.23.R. 
	**Set up** model type (model_type, deterministic or stochastic; line 16/17)
	**Set up** set of alternatives for comparison (out.name, EIS, BA, or BA Chapter; lines 20-22) 
	Output plots will be posted to ./winterRunDSM-Main/Output/Figures/out.name_model_type_variable name, where 'variable name' is the variable being plotted.
	*Note* The color palette used in plotting is currently the ggplot default, and may be updated in the future to standardize alternative-specific colors across alternatives.
	# Spring-run
**Run** ./springRunDSM-Main/Scripts/Plotting_10.11.23.R. 
	**Set up** model type (model_type, deterministic or stochastic; line 18/19)
	**Set up** set of alternatives for comparison (out.name, EIS, BA, or BA Chapter; lines 22-24) 
	Output plots will be posted to ./springRunDSM-Main/Output/Figures/out.name_model_type_variable name, where 'variable name' is the variable being plotted.
	*Note* The color palette used in plotting is currently the ggplot default, and may be updated in the future to standardize alternative-specific colors across alternatives.

#To generate tabular summaries of new outputs
	# Winter-run
**Run** ./winterRunDSM-Main/Scripts/Tabling_9.14.23.R. 
	Output tables are saved ./winterRunDSM-Main/Output/Tables in .csv format. 
	*Note* The script creates separate tables for EIS and BA.
**Run** ./winterRunDSM-Main/Scripts/Generating All Summary Tables in Word_BA.rmd
	Formatted tables in Word for the BA will be generated in ./winterRunDSM-Main/Scripts/
	*Note* Users will need to update the reference_docx directory in line 6 to point to TableTemplate.docx provided in the base folder
**Run** ./winterRunDSM-Main/Scripts/Generating All Summary Tables in Word_EIS.rmd
	Formatted tables in Word for the EIS will be generated in ./winterRunDSM-Main/Scripts/
	*Note* Users will need to update the reference_docx directory in line 6 to point to TableTemplate.docx provided in the base folder
	# Spring-Run
**Run** ./springRunDSM-Main/Scripts/Tabling_10.12.23.R. 
	Output tables are saved ./springRunDSM-Main/Output/Tables in .csv format. 
	*Note* The script creates separate tables for EIS and BA.
**Run** ./springRunDSM-Main/Scripts/Creating tables in Word BA.rmd
	Formatted tables in Word for the BA will be generated in ./springRunDSM-Main/Scripts/
	*Note* Users will need to update the reference_docx directory in line 6 to point to TableTemplate.docx provided in the base folder
**Run** ./springRunDSM-Main/Scripts/Creating tables in Word EIS.rmd
	Formatted tables in Word for the EIS will be generated in ./springRunDSM-Main/Scripts/
	*Note* Users will need to update the reference_docx directory in line 6 to point to TableTemplate.docx provided in the base folder

# To visualize and compare new outputs relevant to Southern Resident Killer Whale analyses across alternatives
	# Winter-run
**Run** ./winterRunDSM-Main/Scripts/SRKW Plotting+Tabling_9.27.23.R
	Output plots and tables are saved in ./winterRunDSM-Main/Output/Figures/ and ./winterRunDSM-Main/Output/Tables/, respectively
	The script generates summaries for all alternatives (EXP1, EXP3, NAA, Alt1-4)
	# Spring-run
**Run** ./springRunDSM-Main/Scripts/SRKW Plotting+Tabling_9.27.23.R
	Output plots and tables are saved in ./springRunDSM-Main/Output/Figures/ and ./springRunDSM-Main/Output/Tables/, respectively
	The script generates summaries for all alternatives (EXP1, EXP3, NAA, Alt1-4)



##Model Calibration
Model recalibration was needed to address several changes in operations, updated hydrological modeling, and several corrections to the original code that influenced model calibration. 
Files for calibration can be found in folders ./winterRunDSM-Main_Calibration/ and ./springRunDSM-Main_Calibration/. Further details are available upon request.
