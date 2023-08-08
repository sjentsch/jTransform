jtSortClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtSortClass",
    inherit = jtSortBase,
    private = list(
        .names=NULL,
        .tables=list(),
        .notrun=TRUE,
        .init=function() {
          
          jinfo("MODULE: jtSort init phase started")

          if (!is.something(self$options$varSrt)) {
                       self$results$help$setContent(HELP_jtsort[[1]])
                       return()
            
          }
          else {
               if (!self$options$btnOut)
                       self$results$help$setContent(HELP_all[[1]])

            
          }          

          private$.notrun=FALSE
          private$.inspect()
          atable<-SmartTable$new(self$results$info)
          atable$initSource<-private$.infotable
          private$.tables[["info"]]<-atable
          atable<-SmartTable$new(self$results$features)
          atable$initSource<-private$.features
          private$.tables[["features"]]<-atable
          atable<-SmartTable$new(self$results$showdata)
          atable$expandOnInit<-TRUE
          atable$expandFrom<-2 
          .names<-private$.names
          tab<-as.data.frame(matrix(".",ncol = length(.names),nrow = 1))
          names(tab)<-.names
          atable$initSource<-tab
          private$.tables[["showdata"]]<-atable
          
          
          lapply(private$.tables,function(x) x$initTable())          
          jinfo("MODULE: init ended")
          
        },
        .postInit = function() {
          if ( ! is.null(self$results$showdata$state)) {
            atable <- private$.tables[["showdata"]]
            atable$initSource <- private$.showdata
            atable$initTable()
          }
          
        },
        

        .run = function() {
    
          if (private$.notrun)    return()
          
     
          private$.tables[["info"]]$runSource<-private$.infotable
          private$.tables[["features"]]$runSource<-private$.features
          private$.tables[["showdata"]]$runSource<-private$.showdata
          
          self$results$showdata$deleteRows()
          
          lapply(private$.tables,function(x) x$runTable())

          if (is.something(self$options$varIncl) && is.something(self$options$varExcl)) {
            msg<-"<h1>Warning</h1><div>Both included and excluded variables are defined. Only the included variables are considered.</div>"
            self$results$help$setContent(msg)
          }
          
          if (self$options$btnOut)
              private$.create()
        },
        .create=function() {
          jinfo("MODULE: creating new dataset")
          .data<-subset(self$data, select=private$.names)
          crrArg <- list(dtaInp = .data, fleOut = NULL, varSrt = paste0(gsub("descend", "-", gsub("ascend", "",
                                                            sapply(self$options$ordSrt, "[[", "order"))), sapply(self$options$ordSrt, "[[", "var")))
          do.call(jmvReadWrite::sort_omv, crrArg[-2])
                  
        },
        .inspect=function() {
          .allnames<-names(self$data)
          if (is.something(self$options$varExcl))
            .allnames<-setdiff(.allnames,self$options$varExcl)
          if (is.something(self$options$varIncl))
            .allnames<-self$options$varIncl
          private$.names<-unique(c(self$options$varSrt,.allnames))
          
        },
        .infotable=function() {
          
          list(
            list(text="# Index Variables",var=length(self$options$varSrt)),
            list(text="# Included Variables",var=length(private$.names))
            )
        },
        .features=function() {
  
             self$options$ordSrt
        },
        .showdata=function() {
            data <- self$results$showdata$state
            if (is.null(data)) {
                           .data<-subset(self$data, select=private$.names)
                           crrArg <- list(dtaInp = .data, fleOut = NULL, varSrt = paste0(gsub("descend", "-", gsub("ascend", "",                                                                                                      sapply(self$options$ordSrt, "[[", "order"))), sapply(self$options$ordSrt, "[[", "var")))
                           .data<-do.call(jmvReadWrite::sort_omv, crrArg)
                            data <- showdata(self,.data)
                            self$results$showdata$setState(data)
            }
           data
         }


        
    )
)
