# When Science Strikes Back - Has a scientific background helped leaders against COVID-19?

# Replication steps
- You must have installed R (version 4.4.5), Rstudio or Visual Studio, and RTools (4.5) (https://cran.r-project.org/bin/windows/Rtools/rtools45/rtools.html)
- Download this folder clicking on the green button "<>Code", Download zip and unzip OR Clone repository using Github Desktop
- If using Rstudio open "replication_package_science.Rproj" then open "code/00_main_code.R"
  - Press Source button (Ctrl + Shift + S)
  - Press "1" to activate the local repository
  - R will probably reboot, Press Source button again
  - Select "Y" to install required packages
  - All the code will run and reproduce figure and tables under 5 minutes
 
- If using Visual Studio open "code/00_main_code.R"
  - Click on the Explorer icon on the left (Ctrl+Shift+E)
  - Click on the "Open Folder" blue button
  - Select the "replication_package_science-main" (where all the folders are) and click on "Select folder"
  - Open "code/00_main_code.R" again and click on "Run Source" to open the Terminal
  - Click on "Run Source" again
  - Press "1" to activate the local repository
  - R will probably reboot and run automatically, just wait around 5 minutes.
  - If you received an error run renv::install("jsonlite", type = "binary") and try it again
