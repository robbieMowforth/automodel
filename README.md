# automodel
Hello, welcome to the README! Made by Robbie Mowforth

# DOWNLOADING UNDERSTANDING SOCIETY DATA

Since the Understanding Society data files are so large, a manaual download of then is going to be required.

To access the Wave 2 and 3 survey data, please go to the following link: https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=6614
To access the Nurse Visit data, please go to the following link: https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=7251
You are going to need to be able to access this data with a given license, the University of Essex has a liscense for all members
Once you have access, you need to download the TAB versions of the data (.tab files)

Once downloaded, follow this prodcedure:

1. Copy the files "b_indresp", "b_income", "c_indresp" and "c_income" from the data download from the link: https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=6614

2. Navigate into the directory "rData"

3. Paste these files in the directory called "selected" (In the direcroty there should be a text file named "FILES GO HERE")

4. Copy the files "xindresp_ns" and "xlabblood_ns" from the data download from the link: https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=7251

5. Navigate into the directory "rData"

6. Paste these files in the directory called "selected" (In the direcroty there should be a text file named "FILES GO HERE")

Once completed, you should have all of the data to complete a full run of the code

# RUNNING THE CODE

To run the code correctly, please do the following:

1. open up the "runningexample.Rproj" file in R Studio (can be done by clicking on the file)

2. Navigate to the "Files" tab in the mid-left on R-Studio

3. Navigate into the file "rCode" directory (if this directory can't be seen, try going up a level in the files tab by clicking the two dots (..) at the top of the navigator)

4. Click of the R script "presentation.R"

In this script you can see a demostration of how to install package and run it

# VIEWING AUTOMODEL FUNCTION

To view the code behind the autoModel fucntion.

1. Navigate to the main directory (as set by runningexample.Rproj)

2. Navigate to the "automodel" directory

3. Navigate to the "R" directory

4. Click of the R script "automodel.R" 

You can then view the code used to madke the autoModel function (and the Roxygen2 skeleton used to generate the documentation)
