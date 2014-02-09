test <- data.frame(YEAR=c(1,3,5,10), TO=c(3,5,10,15),MPOP=c(3,4,2, 6))

test2 <- test[rep(1:nrow(test),(test$TO - test$YEAR)),]
test2$YEAR <- (test2$YEAR[1]:(test2$TO[nrow(test2)] - 1))
test2$TO <- test2$YEAR + 1