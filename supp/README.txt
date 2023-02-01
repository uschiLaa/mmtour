This folder contains all data files, source scripts and example scripts to reproduce the results from "New and simplified manual controls for projection and slice tours, with application to exploring classification boundaries in high dimensions". 

- The data/ folder contains three .csv files with the data input used both by the R example scripts and the Mathematica notebooks
- The mathematica/ folder contains the Mathematica source code in src/ and the example notebooks using the penguins data in notebooks/. All paths within the notebooks are relative and work within this folder structure
- The R/ folder contains the R source code, the code to reproduce the example from the raw data in example/ and the scripts to reproduce results from the main paper and appendix in paper/. We use .Rproj files together with the `here` package such that all paths will work within the provided folder structure.

GETTING STARTED:

- To run the mathematica examples open mathematica/notebooks/penguins_*.nb and enable the dynamic content. Run through the notebook to load the data and manual tour views, and use controls on the left of the interactive panel to change the views.
- To regenerate the example data use the code provided in R/example/penguins.R.
- To reproduce results figures from the paper run the code in R/paper/paper.R. 

