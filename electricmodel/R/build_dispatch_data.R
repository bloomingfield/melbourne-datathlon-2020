

read.nested.zip.csvs = function(zip.file, temp.i) {
  tempdir = paste0('data/aemo/dispatch/temp', temp.i)
  unzip(zip.file, exdir = tempdir)
  temp.files = list.files(tempdir, full.names = T)
  for (temp in temp.files) {
    unzip(temp, exdir = tempdir)
  }
  
  read.files = list.files(tempdir, pattern = '*.CSV', full.names = T) %>% 
                  map_df(~read_csv(., skip=1))
  unlink(tempdir, recursive = T, force=T)
  unlink(tempdir)
  browser()
  
}



process.dispatch.data = function() {
  read.files = list.files('data/aemo/dispatch', full.names = T)  
  
  read.nested.zip.csvs(read.files[1], 1)
  
}