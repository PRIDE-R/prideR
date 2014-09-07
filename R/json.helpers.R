
fromJsonListToDataFrame <- function(jsonList) {
    cleanJsonList <- lapply(jsonList, function(x) {
        x[sapply(x, is.null)] <- NA # clean null values
        x[sapply(x,length)==0] <- NA # clean empty lists
        x[sapply(x,length)>1] <- sapply(x[sapply(x,length)>1], function(y) {paste(y,collapse=" || ")}) # clean multivalued lists
        unlist(x)
    })
    dataFrame <- as.data.frame(do.call("rbind", cleanJsonList))
}

fromJsonToDataFrame <- function(jsonItem) {
    jsonItem[sapply(jsonItem, is.null)] <- NA # clean null values
    jsonItem[sapply(jsonItem,length)==0] <- NA # clean empty lists
    jsonItem[sapply(jsonItem,length)>1] <- sapply(jsonItem[sapply(jsonItem,length)>1], function(y) {paste(y,collapse=" || ")}) # clean multivalued lists
    dataFrame <- as.data.frame(jsonItem)
}