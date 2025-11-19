# Leading like Scientists: The Effect of STEM Background on CEO Performance under Uncertainty

## Replication Instructions

### Requirements
Before running the replication package, ensure you have installed:

- **R (version 4.4.5)**
- **RStudio** *or* **Visual Studio Code**
- **RTools 4.5**  
  <https://cran.r-project.org/bin/windows/Rtools/rtools45/rtools.html>

---

## 1. Download the replication package

Choose one of the following:

- Click the green **<> Code** button → **Download ZIP** → unzip the folder  
**OR**
- Clone the repository using **GitHub Desktop**

---

## 2.A Running the replication in RStudio

1. Open the project file: **`replication_package_science.Rproj`**
2. Open the main script: **`code/00_main_code.R`**
3. Click **Source** (or press **Ctrl + Shift + S**)
4. When prompted:
   - Press **"1"** to activate the local repository  
   - R will likely restart  
5. After R restarts, click **Source** again
6. Select **"Y"** to install required packages
7. The script will execute automatically and reproduce all figures and tables in **under 5 minutes**.

---

## 2.B Running the replication in Visual Studio Code

1. Open **`code/00_main_code.R`**
2. Click the **Explorer** icon on the left (Ctrl + Shift + E)
3. Click **"Open Folder"** and select the folder **`replication_package_science-main`**
4. Open **`code/00_main_code.R`** again
5. Click **Run Source** to open the R Terminal
6. Click **Run Source** once more to start the script
7. When prompted:
   - Press **"1"** to activate the local repository  
   - R will reboot and continue running automatically  
8. Wait approximately **5 minutes** for all outputs to be reproduced
9. **If you encounter an error while installing the packages**, try it again after running:
   ```r
   renv::install("jsonlite", type = "binary")
