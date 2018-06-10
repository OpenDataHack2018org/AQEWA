# AQEWA#Installation of Rstudio
If not already installed on your computer, install RStudio

#Installing packages
Make sure all the required packages are installed before running the code.
To install missing packages in R, type in the console:
install.packages(“nameofpackage”)

#Downloading the Application folder
Download the folder named “Application” with all the files in it from Github

#Creating the folder Forecast
Create a folder “Forecast” in the folder “Application”. This must be exactly the name “Forecast” and nothing else.

#Download the forecast files from the CAMS website:
1.	Go to http://www.regional.atmosphere.copernicus.eu/index.php?category=data_access&subensemble=reanalysis_products
2.	In the DATA DOWNLOAD section on the right, click on “ONLINE DATA” section
3.	Select a model: ENSEMBLE
4.	Select type of data: FORECAST
5.	Select a specie ONLY AMONGT THOSE ONES: CO, O3, PM25, PM10, SO2, NO2
6.	Select type of level: From surface up to 5000m
7.	Select time step package: From -24H to -1H
8.	Select a reference time
9.	Select format: NetCDF
10.	Click on: “In order to download CAMS data products, the CAMS data license available here must be accepted”

#Filling the Forecast folder with forecast files
Copy the files you downloaded from CAMS and place them in the folder “Forecast”

#Launch the application
Open the ui.R file with RSutdio and click on the button “Run App” from RStudio
