if (FALSE) 
{
  devtools::install_local("~/Development/rpkgs/rcreds", depend=FALSE)
  # options(rcreds.folder = "~/rcreds2/rcreds_testing")
  # options(rcreds.key.folder = "~/rcreds2/rcreds_keys_testing")
}

library(rcreds)

# createKey                
# read_credentials_from_file  
# readKeyFromFile          
# saveKey                  
# write_credentials_to_file



username <- "Cosmo"
password <- "Too Many Secrets"

write_credentials_to_file(username = username, password = password)



get_default_rcreds_key_file()
get_default_rcreds_file()



?dir.create


&&&&&&&&&&& RECA LEFT OFF HERE
key_object <- createKey

retreg <- write_credentials_to_file(user = "sample_user", password = "what")
creds <- read_credentials_from_file()

ret <- write_db_credentials_to_file()
params <- read_db_credentials_from_file()
cat(names(params), "", sep="\n")


requireNamespace("rcreds")
rcreds::read_db_credentials_from_file(, key ... )

