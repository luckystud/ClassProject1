# Steps to Generate Tidy Data or What Does the run_analysis.R script do?
## First Operation: Create a data frame from "test" and "train" files and merge
### Files
* test/X_test.txt and train/X_train.txt 
   * Contains the actual data.  
   * Each line has 561 numbers
   * X_test.txt has 2947 lines
   * X_train.txt has 7352 lines
* features.txt
   * Length of file is 561 lines
   * SWEET...Each line must correspond to a number per line in X_test.txt and X_train.txt
   * Each line must be a header for a column of data
* test/subject_test.txt and train/subject_train.txt
   * Length of files are 2947 lines and 7352 lines respectively.
   * SWEET...Each file corresponds to the number of lines in the test data
   * These are the corresponding volunteers for each line of data, anonymously assigned a numeric ID
   * There are 30 unique IDs in this file, implying 30 volunteers contributed to this data.
* test/y_test.txt and train/y_train.txt
   * Length of files are 2947 lines and 7352 lines respectively.
   * SWEET...Each file corresponds to the number of lines in the test data
   * These are the corresponding actions each volunteer performed
   * There are 6 unique IDs in this file, implying 6 action being measured on
* activity_lables.txt
   * Length of file is 6 lines.
   * These must correspond to the number of actions in the y_*.txt files.
### Operations
* For the "test and "train" data
   * Read the data file (test/X_test.txt and train/X_train.txt) into a data frame
   * Read in the features.txt file and assign the contents as headers to the data frame
   * Read in the actions file (test/y_test.txt and train/y_train.txt)
   * Read in the action description file (activity_labels.txt)
   * Create a new vector mapping the descriptive action to the action ID
   * Read in the subject file (test/subject_test.txt and train/subject_train.txt)
   * Merge the two new columns to the data frame
* After retrieving the "test" and "train" data, merge them together.  
* This satisfies Step 1, Step 3 and Step 4 of the Class Project
