automl is a command-line tool for the automization of machine learning experiments. It is written in R. Its aim is to offer automated solutions for each step of a machine learning pipeline by employing a variety of R packages. Meta-learning has been applied for the prediction of optimal model hyper-parameters and heuristics have been embedded in order to insert expert-like behavior to preprocessing and evaluation techniques. We are currently dealing with binary classification problems, for which we produce an optimized ensemble using the forward-model selection technique. The interface of our program aims at offering a highly customizable data scientist with an easily understandable and reusable output. To this end, a report describing the conducted experiment is created upon completion.

experiment.R is the interface to the project. Your R working directory must be automl to run it. Script build_script.R is used to source all files and install all required packages.

Run 

  Rscript experiment.R -e -d 'dataset_name.csv' -p 'project_name' 

to experiment with a dataset (which must be placed under workspace/datasets_repo). A directory project_'project_name' will be created under workspace, containing the experiment's output.

Available datasets for testing our program can be found under https://drive.google.com/open?id=0B5kUY54Gc5R1TkJUQWJ1V3Uzcnc. Please unzip this folder under workspace.


