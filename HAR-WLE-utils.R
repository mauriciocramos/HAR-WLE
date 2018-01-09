# Clean-up training and testing sets AT THE PARENT ENVIROMENT!

WLEdataCleanup <- function() {
    require(dplyr)
    missedTrainVars <- names(which(sapply(training, function(x) any(is.na(x)))))
    missedTestVars <- names(which(sapply(testing, function(x) any(is.na(x)))))
    missedVars <- unique(c(missedTrainVars, missedTestVars))
    training <<- select(training, -one_of(missedVars), -matches("window|timestamp"), -X)
    testing <<- select(testing, -one_of(missedVars), -matches("window|timestamp"), -X)
    levels(training$classe) <<- c("A)correctly", "B)throwing elbows", "C)lifting halfway", "D)lowering halfway", "E)throwing hips")
}
