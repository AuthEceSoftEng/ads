# automl

automl is a command-line tool for the automization of machine learning experiments. It is written in R. Its aim is to offer automated solutions for each step of a machine learning pipeline by employing a variety of R packages. Meta-learning has been applied for the prediction of optimal model hyper-parameters and heuristics have been embedded in order to insert expert-like behavior to preprocessing and evaluation techniques. We are currently dealing with binary classification problems, for which we produce an optimized ensemble using the forward-model selection technique. The interface of our program aims at offering a highly customizable data scientist with an easily understandable and reusable output. To this end, a report describing the conducted experiment is created upon completion.

## Getting started
Please follow these instructions in order to install and test our software.

### Prerequisites
This is a project developed for linux-gnu using R version 3.3. All package dependencies are automatically installed under a project-specific [lib](lib) directory.

### Installing
Clone our project's branch refactor, which contains the software components of our tool, [build_script.R](build_script.R) for downloading all necessary packages and building all our source files and [experiment.R](experiment.R), a script that defines its interface. Available datasets for testing our program can be found under https://drive.google.com/open?id=0B5kUY54Gc5R1TkJUQWJ1V3Uzcnc. Please unzip this folder under [workspace](workspace).

## How to run

Our tool supports a variety of command-line options, which you can find in [experiment.R](experiment.R).

The main functionality of our tool is to perform a machine learning experiment in order to provide an optimized ensemble of prediction models for a binary classification dataset, the format of which must coincide with the example datasets under [datasets_repo](workspace/datasets_repo). There are different options:

* type of testing (kfold, holdout, loocv) and ratio of instances used for training
* size of ensemble (defaults to 50)
* machine learning algorithms used (choose to deactivate among svm, knn, nnet, rpart)
* how to deal with unknown values
* how to deal with infinite values

### Example commands
`Rscript experiment.R -e --dataset crx.csv `

performs an experiment with default settings and creates a directory project\_crx under [workspace](workspace), which contains all information produced during the experiment. This includes all trained models, an [experiment\_info.Rdata](workspace/project_crx/experiment_info.Rdata) file containing a nested list with all valuable information produced during the experiment and an [automl_report.html](workspace/project_crx/automl_report.html) file, a report on the performed experiment.  

`Rscript experiment.R -e --dataset crx.csv --project my_project`

will create a project\_my\_project under [workspace](workspace). Please use this option when you already have a project named project_crx.

`Rscript experiment.R -e --dataset crx.csv --project my_project --testing_technique holdout --testing_technique_ratio 0.8`

will use the holdout technique, with 80% instances used for training and 20%  for testing the final ensemble.

`Rscript experiment.R -e --dataset crx.csv --project my_project --testing_technique holdout --testing_technique_ratio 0.9`

will perform 10-fold cross-validation on the final ensemble. Note that this is the default setting.

`Rscript experiment.R -e --dataset crx.csv --project my_project --dis_svm`

will exclude Support Vector Machines from being used in the ensemble.

`Rscript experiment.R -e --dataset crx.csv --project my_project --unknown_action replace --unknown_replace 0`

instructs to replace all NAs with zero during preprocessing.

Parallel execution of testing folds is supported upon request:

`Rscript experiment.R -e --dataset crx.csv --testing_technique holdout --testing_technique_ratio 0.9 --cores 2`

will perform parallely 10-fold cross-validation using 2 cores. Note that the number of cores you request must be available on your machine.


After the completion of an experiment one can use the produced ensemble in order to predict the class of a dataset. (Note that the features of the dataset should coincide with the features of the dataset used to perform the experiment. Also, if a feature named Class is included, it will be ignored.)

`Rscript experiment.R --predict --project crx --dataset crx.csv`

will predict the class of instances in file crx.csv and produce a file [predictions.csv](workspace/project_crx/predictions.csv) under specified project. Note that in order for this to be possible, a completed experiment must have been performed, equivalently an [project_crx/experiment_info.Rdata](workspace/project_crx/experiment_info.Rdata) must exist.


Finally, one can choose to compare the performance of different algorithms on different datasets.

`Rscript experiment.R --compare --project compare --benchmark_file thesis_benchmarks.csv`

will create a [project_compare](workspace/project_compare) containing a nested list of information in [comparison_info.Rdata](workspace/project_compare/comparison_info.Rdata) plus a [compare_report.html](workspace/project_compare/compare_report.html), describing the comparison performed. One can use his own benchmarks by placing them under [workspace/benchmarks](workspace/benchmarks).


