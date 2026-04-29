home_dir <- fs::path_home()
test_drive_dir <- fs::path_expand("~/test_drive")
fs::dir_create(path=test_drive_dir)
test_project_path <- "BRANCHES/dr_test/test_project/"

unlink(paste(test_drive_dir,"/",test_project_path,sep=""))
unlink(paste(test_drive_dir,"/","BRANCHES/dr_test/",sep=""))

full_path <- paste0(test_drive_dir,"/",test_project_path)



test_that("test project creation",{
  create_project(path=full_path,
                 project_name="Test Project", pi="Dr. Test",analyst ="Test Name",
                 data_location ="BRANCHES/dr_test/test_project",
                 git_location="CIDA/Test_test_project" )
  expect_equal(file.exists(paste0(full_path,"/Admin/ReadMe.md")), TRUE)
  expect_equal(file.exists(paste0(full_path,"/Background/ReadMe.md")), TRUE)
  expect_equal(file.exists(paste0(full_path,"/Code/ReadMe.md")), TRUE)
  expect_equal(file.exists(paste0(full_path,"/DataProcessed/ReadMe.md")), TRUE)
  expect_equal(file.exists(paste0(full_path,"/DataRaw/ReadMe.md")), TRUE)
  expect_equal(file.exists(paste0(full_path,"/Dissemination/ReadMe.md")), TRUE)
  expect_equal(file.exists(paste0(full_path,"/ReadMe.md")), TRUE)
  expect_equal(file.exists(paste0(full_path,"/Reports/ReadMe.md")), TRUE)
  expect_equal(dir.exists(paste0(full_path,"/.ProjData")), TRUE)
  expect_equal(file.exists(paste0(full_path,"/.ProjData/Data.dcf")), TRUE)
  expect_equal(file.exists(paste0(full_path,"/test_project.Rproj")), TRUE)
  expect_equal(file.exists(paste0(full_path,"/.Rprofile")), TRUE)
})

test_that("test open_project",{
  open_project(remote_project_folder=full_path)
  expect_equal(as.character(options("cida_tools.remote_current_project_path")), full_path)
})

test_that("test project meta data",{
  options(cida_tools.remote_current_project_path= "")

  # Process the DCF file using your function
  w <- capture_warnings(github <- get_project_github())
  expect_match(w,".ProjData directory not found in project.",all=FALSE)
  expect_match(w,".ProjData/Data.dcf File does not exist.",all=FALSE)
  expect_equal(github, "")

  w <- capture_warnings(proj_name <- get_project_name())
  expect_match(w,".ProjData directory not found in project.",all=FALSE)
  expect_match(w,".ProjData/Data.dcf File does not exist.",all=FALSE)
  expect_equal(proj_name, "")
  w <- capture_warnings(pi <- get_project_pi())
  expect_match(w,".ProjData directory not found in project.",all=FALSE)
  expect_match(w,".ProjData/Data.dcf File does not exist.",all=FALSE)
  expect_equal(pi, "")
  w <- capture_warnings(analyst <- get_project_analyst())
  expect_match(w,".ProjData directory not found in project.",all=FALSE)
  expect_match(w,".ProjData/Data.dcf File does not exist.",all=FALSE)
  expect_equal(analyst,"")
  w <- capture_warnings(location <- get_project_location())
  expect_match(w,".ProjData directory not found in project.",all=FALSE)
  expect_match(w,".ProjData/Data.dcf File does not exist.",all=FALSE)
  expect_equal(location,fs::path(""))

  open_project(remote_project_folder=full_path)
  # Process the DCF file using your function
  github <- get_project_github()
  proj_name <- get_project_name()
  pi <- get_project_pi()
  analyst <- get_project_analyst()
  location <- get_project_location()
  # Expectations
  expect_equal(github, "CIDA/Test_test_project")
  expect_equal(proj_name, "Test Project")
  expect_equal(pi, "Dr. Test")
  expect_equal(analyst,"Test Name")
  expect_equal(location,fs::path("BRANCHES/dr_test/test_project/"))

  options(cida_tools.remote_current_project_path= "")

  setwd(full_path)
  rm(github, proj_name, pi, analyst, location)
  # Process the DCF file using your function
  github <- get_project_github()
  proj_name <- get_project_name()
  pi <- get_project_pi()
  analyst <- get_project_analyst()
  location <- get_project_location()
  # Expectations
  expect_equal(github, "CIDA/Test_test_project")
  expect_equal(proj_name, "Test Project")
  expect_equal(pi, "Dr. Test")
  expect_equal(analyst,"Test Name")
  expect_equal(location,fs::path("BRANCHES/dr_test/test_project/"))

})

test_that("get project drive path",{
  open_project(remote_project_folder=full_path)
  drive_path <- get_project_drive_path()
  expect_equal(drive_path,test_drive_dir)

  options(cida_tools.remote_current_project_path= "")
  errMsg <- tryCatch({get_project_drive_path()}, error=function(x) {geterrmessage()})
  expect_match(errMsg, "Nothing found at (.*?) ensure drive is mounted and you have entered your password to access the drive \\(and are logged into the VPN if needed\\.\\) If still experiencing issues try set_project_data_path\\(\\) or  set_global_default_path\\(\\)")
  #expect_equal(errMsg, "Nothing found at /Volumes/dept || SPH || SPH-CIDA || BRANCHES Please ensure drive is mounted and you have entered your password to access the drive (and are logged into the VPN if needed.) If still experiencing issues try set_project_data_path() or  set_global_default_path()")
  # Since the above call to get_project_drive_path() will error out, drive_path retains the old value from the first call. Commenting out for now to prevent the check.
  #expect_equal(drive_path,"")
})

unlink(paste(test_drive_dir,"/",test_project_path,sep=""))
