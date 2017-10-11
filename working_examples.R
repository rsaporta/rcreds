if (FALSE) 
{
  devtools::install_local("~/Development/rpkgs/rcreds", depend=FALSE)
  # options(rcreds.folder = "~/rcreds2/rcreds_testing")
  # options(rcreds.key.folder = "~/rcreds2/rcreds_keys_testing")
}

library(rcreds)

# createKey                
# readCredentialsFromFile  
# readKeyFromFile          
# saveKey                  
# writeCredentialsToFile



username <- "Cosmo"
password <- "Too Many Secrets"


writeCredentialsToFile(username = username, password = password)



get_default_rcreds_key_file()
get_default_rcreds_file()



?dir.create


&&&&&&&&&&& RECA LEFT OFF HERE
key_object <- createKey

retreg <- writeCredentialsToFile(user = "sample_user", password = "what")
creds <- readCredentialsFromFile()

ret <- writeDBCredentialsToFile()
params <- readDBCredentialsFromFile()
catnn(names(params))
