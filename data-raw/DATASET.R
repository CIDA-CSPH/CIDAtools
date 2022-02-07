# imports example murre data from Movebank
# user must provide Movebank credentials to successfully download

murres <- opp_download_data(248994009)

usethis::use_data(murres, overwrite = T)

