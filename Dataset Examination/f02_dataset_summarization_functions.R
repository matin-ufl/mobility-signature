
# Selected participants (after outlier removal - Final Step) should be included for summarization
chosen.ones <- function(keyVariables.df, selected.pids) {
     result <- keyVariables.df
     notSelected.idx <- rep(F, nrow(keyVariables.df))
     for (i in 1:nrow(keyVariables.df)) {
          if(length(which(keyVariables.df$accpid[i] == selected.pids)) == 0) {
               notSelected.idx[i] <- T
          }
     }
     a <- which(notSelected.idx)
     result[-c(a), ]
}