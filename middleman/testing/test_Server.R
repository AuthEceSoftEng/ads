setwd("../..")

test_that("Test createProject",{
  server <- Server$new()
  server$createProject(dataset_name = "test_dataset.csv", project_name = "test_project" , workspace_dir ="workspace")
  str(list.files("workspace"))
  expect_true( "project_test_project" %in% list.files("workspace"))
    server$createProject(dataset_name = "test_dataset.csv", project_name = NULL, workspace_dir ="workspace")
    expect_true( "project_test_dataset" %in% list.files("workspace"))
})

test_that("Test performExperiment",{
  # not implemented yet
    server <- Server$new(directories_ = list( Workspace = "workspace", Project = "workspace/project_test_project"))
    server$performExperiment(dataset_name = "baboon_mating.csv")
})

test_that("Test saveExperimentInfo",{
 # tested through trial
})

test_that("Test gatherExperimentInfo",{
 # tested through trial
})

test_that("Test compareAlgorithms",{

})

test_that("Test trainScientist",{
  # not implemented yet
})


