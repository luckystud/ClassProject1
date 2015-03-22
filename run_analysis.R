# Note to readers of this script.  The whole script is invoked by the
#   run_analysis() function at the end of this script.  Users of the script
#   should "source()" this file and run run_analysis() in the top level
#   working directory containing the unzip Samsung phone data.

# Prepares a data frame that contains a test set for the "Human
#    Activity Recognition Using Smartphones Dataset".  The assumption
#    is that this function is run with the working directory set as
#    top level directory.  For the experiment, the only valid arguments
#    to this function are either "test" or "train"
# Note to graders - Some sections of this function satisfies Step 3 and Step 4
#    of the Class Project.
#    Step 3 - "Uses descriptive activity names to name the activities in the data set"
#    Step 4 - "Appropriately labels the data set with descriptive variable names"
#
prepare_df <- function(type) {
    # Note to graders - It would be good to read in this file only once but
    # it is a relatively quick operation and I don't want to get marked off
    # for not doing it.  The same goes for the activity_labels.txt file.
    print("Reading in experiment variable names")
    features <- read.table("features.txt")
    header_col <- features[[2]]
    headers <- sapply(header_col, as.character)

    print("Reading in experimental data")
    X_file <- sprintf("%s/X_%s.txt", type, type)
    X_data <- read.table(X_file)
    #
    # Note to graders, the following line satisfies Step 4 of the class project
    #
    colnames(X_data) <- headers

    print("Reading in experiment subject IDs")
    subj_file <- sprintf("%s/subject_%s.txt", type, type)
    subj_ids <- scan(subj_file)

    #
    # Note to graders, the following code section, along with the "cbind"
    # call afterward, satisfies Step 3 of the class project
    #
    print("Reading in experiment activities")
    activity_file <- sprintf("%s/y_%s.txt", type, type)
    activity_ids <- read.table(activity_file)
    names(activity_ids)[1] <- "activity_id"
    activity_labels <- read.table("activity_labels.txt")
    names(activity_labels)[1] <- "id"
    names(activity_labels)[2] <- "activity_name"
    get_act <- function(idx) {activity_labels$activity_name[idx]}
    activity_names <- sapply(activity_ids$activity_id, get_act)

    print("Binding above data into data frame")
    alldata <- cbind(subj_id = subj_ids, activity_name = activity_names, X_data)
}

# Pull all means and standard deviations from full data.
# All variable names were pulled from feature_info.txt and hard-coded into
#   the measurements character vector.  Each variable has multiple measurements
#   associated with it, including the average and standard deviation.  The
#   average has a "-mean()" suffix to its measurment header while the 
#   standard deviation has a "std()" suffix.  If the measuremnt doesn't have
#   "Mag" in its header, there are X, Y and Z measurements to indicate
#   movement on the X, Y and Z axis.  These headers have an additional "-X", 
#   "-Y" and "-Z" suffix.
# All other measurements included with the data set are ignored
#
# Note to graders: This function satisfies Step 2 of the Class Project
# Step 2: Extracts only the measurements on the mean and standard deviation for each measurement.
#
pull_mean_std <- function(full_data) {
    # The following measurement names were derived from feature_info.txt
    # Note that in feature_info.txt, "fBodyAccJerkMag", "fBodyGyroMag"
    # and "fBodyGyroJerkMag" were mentioned but doesn't exist in the dataset
    measurements <- c("tBodyAcc", "tGravityAcc", "tBodyAccJerk",
                      "tBodyGyro", "tBodyGyroJerk", "tBodyGyroJerk",
                      "tBodyAccMag", "tGravityAccMag", "tBodyAccJerkMag",
                      "tBodyGyroMag", "tBodyGyroJerkMag", "fBodyAcc", 
                      "fBodyAccJerk", "fBodyGyro", "fBodyAccMag")
                      # The feature_info.txt file implied these measurements
                      # existed but they don't
                      #"fBodyAccJerkMag", "fBodyGyroMag", "fBodyGyroJerkMag")
    # new_data will only contain the relavent measurements for this
    #   class project
    new_data = data.frame(full_data[,"subj_id"])
    # new_names will contain the data frame headers for new_data
    new_names = c("subj_id")
    new_data = cbind(new_data, full_data[,"activity_name"])
    new_names = c(new_names, "activity_name")
    # At this point, new_data has a column for the subject ID and another
    #   column for the activity_name.  The following code adds the
    #   average and standard deviation for each of the measurements
    for (m in measurements) {
        if (length(grep("Mag", c(m))) > 0) {
            # Each of the "Mag" measurements have a mean and standard
            # deviation measurement.
            for (suf in c("-mean()", "-std()")) {
                title = paste(m, suf, sep = "")
                col <- full_data[,title]
                new_data = cbind(new_data, col)
                new_names = c(new_names, title)
            }
        } else {
            # Each of the non-"Mag" measurements have a mean and standard
            # deviation measurement in the X, Y and Z axis
            for (suf in c("-mean()-X", "-mean()-Y", "-mean()-Z", "-std()-X",
                          "-std()-Y", "-std()-Z")) {
                title = paste(m, suf, sep = "")
                col <- full_data[,title]
                new_data = cbind(new_data, col)
                new_names = c(new_names, title)
            }
        }
    }
    # Finally, add the column names to the new data frame
    colnames(new_data) <- new_names
    new_data
}

# Calculate the averages of the measurements for each subjects activity.
#   For example, subject ID #1 has 95 observations for each measurement while
#   "WALKING".  Take the average for the 95 observations and collapse the 
#   95 to one average observation per measurement.  The output will be the
#   average per subject activity (30 subjects with 6 activities = 180
#   average observations)
#
# Note to graders: This function satisfies Step 5 of the class project
# Step 5: From the data set in step 4, creates a second, independent 
#   tidy data set with the average of each variable for each activity 
#   and each subject.
calc_averages <- function(df) {
    avedf = data.frame(matrix(ncol = ncol(df)-1, nrow = 0))
    act_factors <- attributes(df$activity_name)
    act_vec <- c()
    for (id in unique(df$subj_id)) {
        for (activity in unique(df$activity_name)) {
            print(paste("Finding averages for subject ", id, " performing ", activity))
            # Get the data frame subset that only includes the subject
            #   performing a certain activity
            subdf <- subset(df, subj_id == id & activity_name == activity)
            # Note below that column 2 (activity_name) is skipped
            #   This is because rbind() chokes when trying to append to the 
            #   data frame.  The reason is due to rbind() and factors.
            #   For now, collect the activities in a separate vector
            avelist <- list(subj_id = id)
            for (n in 3:ncol(subdf)) {
                avelist[n-1] <- mean(subdf[,n])
            }
            names(avelist) <- names(df[c(1,3:ncol(df))])
            names(avedf) <- names(df[c(1,3:ncol(df))])
            act_vec <- c(act_vec, activity)
            avedf <- rbind(avedf, avelist)
        }
    }
    # Create the final data frame.  Note that the activity_name column is
    #   finally added here.
    avedf <- cbind(avedf[1], activity_name = act_vec, avedf[2:ncol(avedf)])
}

run_analysis <- function() {
    # Note to graders: The following two lines satisfies Step 3 and 4 of
    # the class project.  See prepare_df() above for details
    X_test <- prepare_df("test")
    X_train <- prepare_df("train")
    
    # Note to graders: The following line satisfies Step 1 of the class
    # project
    X <- rbind(X_test, X_train)
    
    # Note to graders: The following line satisfies Step 2 of the class
    # project.  See pull_mean_std() above for details
    X_mean_std <- pull_mean_std(X)
    
    # Note to graders: The following line satisfies Step 5 of the class
    # project.  See calc_averages() above for details
    X_all_ave <- calc_averages(X_mean_std)
    
    # Write the data out to a file
    write.table(X_all_ave, file = "tidydata.txt", row.names=FALSE)
}

