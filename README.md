# When Science Strikes Back - Has a scientific background helped leaders against COVID-19?

# Replication steps
- You must have installed R (version 4.4.5), Rstudio or Visual Studio, and RTools (4.5) (https://cran.r-project.org/bin/windows/Rtools/rtools45/rtools.html)
- Download this folder clicking on the green button "<>Code", Download zip and unzip OR Clone repository using Github Desktop
- If using Visual Studio open "code/00_main_code.R"
- If using Rstudio open "replication_package_science.Rproj" then open "code/00_main_code.R"
- Run command line renv::restore() (Only needed for the first time to install packages)
  - Press "1" to activate the local repository
  - R will probably reboot, run renv::restores again()
  - Select "Y" to install required packages
  - (if received error run renv::install("jsonlite", type = "binary"))
- Run all the script
- It may take up to 20 min to finish it all
- If you only want to run a specific script, you still need to run the main script first, commenting out the source lines that run the other scripts
