# imports example murre data from Movebank

murres <- track2KBA::move2KBA(movebankID = 	248994009,
                              user = rstudioapi::askForPassword(prompt = 'Movebank username:'),
                              password = rstudioapi::askForPassword(prompt = 'Movebank password:'),
                              filename = NULL)

usethis::use_data(murres, overwrite = T)

