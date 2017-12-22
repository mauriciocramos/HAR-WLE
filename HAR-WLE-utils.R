
# Caculate proporpotion of NA or "" or "#DIV/0!" values for given vectors
missProportion <- function(x) mean(is.na(x) | (is.character(x) & x %in% c("", "#DIV/0!")))


# Clean-up training and testing sets AT THE PARENT ENVIROMENT!
WLEdataCleanup <- function() {
    badVars <- 
        unique(c(names(which(sapply(training,
                                    function(x) { mean(is.na(x) | (is.character(x) & x %in% c("", "#DIV/0!"))) } ) > 0)),
                 names(which(sapply(testing, 
                                    function(x) { mean(is.na(x) | (is.character(x) & x %in% c("", "#DIV/0!"))) } ) > 0))))
    training <<- select(training, -one_of(badVars))
    testing <<- select(testing, -one_of(badVars))
    training$new_window <<- NULL
    training$num_window <<- NULL
    training$cvtd_timestamp <<- NULL
    testing$new_window <<- NULL
    testing$num_window <<- NULL
    testing$cvtd_timestamp <<- NULL
    classes <- c("A:correctly", "B:throwing elbows", "C:lifting halfway", "D:lowering halfway", "E:throwing hips")
    training <<- training %>% mutate(classe = factor(classe, labels = classes), user_name = factor(user_name))
}
