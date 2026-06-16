home_dir <- fs::path_home()
test_drive_dir <- fs::path_expand("~/test_drive/")
fs::dir_create(path=test_drive_dir)
test_project_path <- "BRANCHES/dr_test/test_project"
full_project_dir <- paste(test_drive_dir,test_project_path,"/.ProjData/",sep="")
fs::dir_create(path= full_project_dir)

unlink(paste(full_project_dir,"Data.dcf",sep=""))

test_that("create a test project dcf file and test getter functions", {
  setwd(paste(test_drive_dir,test_project_path,sep=""))
  dcf_content <- list( ProjectName="Test Project 5", PI="Dr. Test",analyst="Analyst Name",datalocation="BRANCHES/dr_test/test_project",gitlocation="CIDATools/Test_test_project")

   w <- capture_warnings(save_project_data(dcf_content))
   expect_match(w,".ProjData/Data.dcf File does not exist.", perl=TRUE ,all=FALSE)
   #expect_warning(save_project_data(dcf_content),".ProjData/Data.dcf file not found in project.")

   # Process the DCF file using your function
   github <- get_project_github()
   proj_name <- get_project_name()
   pi <- get_project_pi()
   analyst <- get_project_analyst()
   location <- get_project_location()
   # Expectations
   expect_equal(github, "CIDATools/Test_test_project")
   expect_equal(proj_name, "Test Project 5")
   expect_equal(pi, "Dr. Test")
   expect_equal(analyst,"Analyst Name")
   expect_equal(location,fs::path("BRANCHES/dr_test/test_project/"))

  })

  test_that("create a test project dcf file and test setter/getter functions", {
    set_project_analyst("Analyst 2")
    set_project_name("Project 1")
    set_project_pi("Dr. Test2")
    set_project_location("BRANCHES/dr_test/test_project2/")
    set_project_github("CIDATools/Test2_test_project2")

    expect_equal(get_project_analyst(),"Analyst 2")
    expect_equal(get_project_name(),"Project 1")
    expect_equal(get_project_pi(),"Dr. Test2")
    expect_equal(get_project_location(),fs::path("BRANCHES/dr_test/test_project2/"))
    expect_equal(get_project_github(),"CIDATools/Test2_test_project2")

  })

test_that("test setting/getting full project path",{
  test_full_path <- fs::path_abs(fs::path_join(c(test_drive_dir,test_project_path)))

  w <- capture_warnings(before_set<-get_full_project_path())
  expect_match(w,"default_full_path_to_project not found in project data.",all=FALSE)
  expect_equal(before_set,"")

  msg <- set_full_project_path(path=test_full_path)
  expect_match(msg,paste("The project default full path has been changed to ",test_full_path,sep=""), all=FALSE)



})

unlink(paste(full_project_dir,"Data.dcf",sep=""))
