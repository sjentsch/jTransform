transposeClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "transposeClass",
    inherit = transposeBase,
    private = list(

      .names=NULL,
      .tables=list(),
      .notrun=TRUE,
      .init=function() {
        
        jinfo("MODULE: Transpose init phase started")
        
        if (!is.something(self$options$varTarget)) {
          self$results$help$setContent(HELP_transpose[[1]])
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
        atable<-SmartTable$new(self$results$showdata)
        atable$expandOnInit<-TRUE
        atable$expandOnRun<-TRUE
        atable$expandFrom<-3
        .names<-private$.names
        tab<-data.frame(ID = .names)
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
        private$.tables[["showdata"]]$runSource<-private$.showdata
        

        lapply(private$.tables,function(x) x$runTable())
        
        private$.create()
      },
      .create=function() {
        
        if (self$options$btnOut) {
          jinfo("MODULE: creating new dataset")
           crrArg <- list(dtaInp = self$data,  varOth = self$options$varTarget,
                         varNme = ifelse(is.null(self$options$varNames), "", self$options$varNames))
          do.call(jmvReadWrite::transpose_omv, crrArg)
        }
        
      },
      .inspect=function() {
        private$.names<-self$options$varTarget
      },
      .infotable=function() {
        if (is.something(self$options$varNames))
           .colnames<-self$options$varNames
        else 
          .colnames<-paste("Generated: V_1, V_2,...")
        
        list( 
          list(text="# new columns",var=dim(self$data)[1]),
          list(text="# new rows",var=length(private$.names)),
          list(text="New Names",var=.colnames)
          
          )
      },
      .showdata=function() {
        data <- self$results$showdata$state
        if (is.null(data)) {
          crrArg <- list(dtaInp = self$data, fleOut = NULL, varOth = self$options$varTarget,
                         varNme = ifelse(is.null(self$options$varNames), "", self$options$varNames))
          .data<-do.call(jmvReadWrite::transpose_omv, crrArg)
          data <- showdata(self,.data)
          self$results$showdata$setState(data)
        }
        data
      },
      
      
      
      
      
      
      
        .ciao = function() {

            # check whether all required variables are present
            if (length(self$options$varOth) > 1) {
                # assemble the arguments for transpose_omv
                crrArg <- list(dtaInp = self$data, fleOut = NULL, varOth = self$options$varOth,
                               varNme = ifelse(is.null(self$options$varNme), "", self$options$varNme))

                # if CREATE was pressed (btnOut == TRUE), open a new jamovi session with the data
                if (self$options$btnOut) {
                    do.call(jmvReadWrite::transpose_omv, crrArg[-2])
                    self$results$txtPvw$setContent(self$results$txtPvw)
                # if not, create a preview of the data (crtPvw in utils.R)
                } else {
                    self$results$txtPvw$setContent(crtPvw(do.call(jmvReadWrite::transpose_omv, crrArg)))
                }
            } else {
                self$results$txtPvw$setContent("")
            }

        }

    )
)
