# automl

experiment.R is the interface to the project. It uses build_script.R to source all files and install required packages.

Run 

  Rscript experiment.R -e -d 'dataset_name.csv' -p 'project_name' 

to experiment with a dataset (which must be contained in workspace/datasets_repo). A directory project_'project_name' will be created under workspace. Your R working directory must be automl.


