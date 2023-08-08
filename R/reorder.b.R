reorderClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "reorderClass",
    inherit = reorderBase,
    private = list(
      .names=NULL,
      .tables=list(),
      .notrun=TRUE,
      .init=function() {
        
        jinfo("MODULE: Reorder init phase started")
        
        if (!is.something(self$options$varOrd)) {
          self$results$help$setContent(HELP_reorder[[1]])
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
        atable$expandFrom<-2 
        .names<-private$.names
        if (length(.names)>10) .names<-.names[1:10]
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
        private$.tables[["showdata"]]$runSource<-private$.showdata
        
        self$results$showdata$deleteRows()
        
        lapply(private$.tables,function(x) x$runTable())
        
        private$.create()
      },
      .create=function() {
        if (self$options$btnOut) {
          
        jinfo("MODULE: creating new dataset")
        .data<-subset(self$data, select=private$.names)
        opendata(.data)
        }
        
      },
      
        # return()
        #     # check whether all required variables are present
        #     if (length(self$options$varOrd) > 1) {
        #         # assemble the arguments for arrange_cols_omv
        #         crrArg <- list(dtaInp = self$data, fleOut = NULL, varOrd = self$options$varOrd)
        # 
        #         # if CREATE was pressed (btnOut == TRUE), open a new jamovi session with the data
        #         if (self$options$btnOut) {
        #             do.call(jmvReadWrite::arrange_cols_omv, crrArg[-2])
        #             self$results$txtPvw$setContent(self$results$txtPvw)
        #         # if not, create a preview of the data (crtPvw in utils.R)
        #         } else {
        #             self$results$txtPvw$setContent(crtPvw(do.call(jmvReadWrite::arrange_cols_omv, crrArg)))
        #         }
        #     } else {
        #         self$results$txtPvw$setContent("")
        #     }
        # 
        # },
      .inspect=function() {
        .allnames<-names(self$data)
        .names<-self$options$varOrd
        if (self$options$add=="all")
            .names<-unique(c(.names,.allnames))
        private$.names<-.names
      },
      .infotable=function() {
        
        list(
          list(text="# of variables",var=length(private$.names))
        )
      },
      .showdata=function() {
        data <- self$results$showdata$state
        if (is.null(data)) {
          data<-subset(self$data, select=private$.names)
          data<-showdata(self,data)
          self$results$showdata$setState(data)
        }
        data
      }
      
      
      

    )
)
