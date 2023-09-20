#### Curbcut Montreal sync script ##############################################

# On launch ---------------------------------------------------------------

install_github("https://github.com/Curbcut/cc.data")
install_github("https://github.com/Curbcut/curbcut")
install_github("https://github.com/Curbcut/cc.buildr")
cc.data::bucket_get_folder("data", "curbcut.montreal.data")
cc.data::bucket_get_folder("dev/data", "curbcut.montreal.dev.data")


# On exit -----------------------------------------------------------------

cc.data::bucket_write_folder("data", "curbcut.montreal.data")
cc.data::bucket_write_folder("dev/data", "curbcut.montreal.dev.data")
