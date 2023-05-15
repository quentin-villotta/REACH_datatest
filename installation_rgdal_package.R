install.packages("sf", type = "source", configure.args = 
                 '--with-gdal-lib=/opt/homebrew/opt/gdal/lib/ 
                  --with-sqlite3-lib=/opt/homebrew/opt/sqlite/lib/ 
                  --with-gdal-config=/opt/homebrew/bin/gdal-config
                  --with-proj-share=/opt/homebrew/opt/proj/share/ 
                  --with-proj-lib=/opt/homebrew/opt/proj/lib 
                  --with-proj-include=/opt/homebrew/opt/proj/include/
                  --with-geos-config=/opt/homebrew/opt/geos/bin/geos-config')



# install.packages("rgdal", type = "source", configure.args = 
#                   '--with-gdal-config=/opt/homebrew/bin/gdal-config
#                   --with-proj-lib=/opt/homebrew/opt/proj/lib 
#                   --with-proj-include=/opt/homebrew/opt/proj/include/')