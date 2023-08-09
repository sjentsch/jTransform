combine <- function(..., prefix = "", sep = "_",sep0=".") {
  paste0(paste0(prefix,sep0), levels(interaction(..., sep = sep)))
}

file_ext<-function (x) 
{
  pos <- regexpr("\\.([[:alnum:]]+)$", x)
  ifelse(pos > -1L, substring(x, pos + 1L), "")
}
`ladd<-`<-function(x,value) {
  x[[length(x)+1]]<-value
  return(x)
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

notmissing<-function(vars,data) {
  
  m<-lapply(vars, function(x) which(is.na(data[[x]])))
  miss <-unique(unlist(m))
  sel  <-setdiff(seq_len(nrow(data)),miss)
  data<-data[sel,]
  m<-lapply(vars, function(x) which(is.nan(data[[x]])))
  miss <-unique(unlist(m))
  sel  <-setdiff(seq_len(nrow(data)),miss)
  data<-data[sel,]
  data
  
}


notallmissing<-function(data) {
  
  sel <- apply(data, 1, function(x){all(is.na(x))})
  data[!sel,]
  
}