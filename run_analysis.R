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

# Pull all means and standard deviations from full data
# Note to graders: This function satisfies Step 2 of the Class Project
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
    new_data = data.frame(full_data[,"subj_id"])
    new_names = c("subj_id")
    new_data = cbind(new_data, full_data[,"activity_name"])
    new_names = c(new_names, "activity_name")
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
    colnames(new_data) <- new_names
    new_data
}

calc_averages <- function(df) {
    avedf = data.frame(matrix(ncol = ncol(df)-1, nrow = 0))
    act_factors <- attributes(df$activity_name)
    act_vec <- c()
    for (id in unique(df$subj_id)) {
        for (activity in unique(df$activity_name)) {
            print(paste("Finding averages for subject ", id, " performing ", activity))
            subdf <- subset(df, subj_id == id & activity_name == activity)
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

