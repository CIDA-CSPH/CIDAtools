# BEGIN CIDATools Added
# The below lines will load the CIDAtools package, and open the CIDA project in this directory. To disable automatic project opening, comment the below lines.
# FROM HERE
message("CIDAtools automatic project opening is enabled. To disable, edit the .Rprofile in your project directory.")
library(CIDATools)
CIDATools::open_project(local_path="{{config.project_path}}")
# END CIDATools Added
