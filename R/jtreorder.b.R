jtreorderClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtreorderClass",
    inherit = jtreorderBase,
    private = list(
      .names=NULL,
      .tables=list(),
      .notrun=TRUE,
      .init=function() {
        
        jinfo("MODULE: Reorder init phase started")
        
        ### if no variable is defined in the UI, we give a feedback and skip init
        if (!is.something(self$options$varOrd)) {
          self$results$help$setContent(HELP_reorder[[1]])
          return()
          
        }
        else {
          ### if CREATE was not clicked, we give a hint about it
          
          if (!self$options$btnOut)
            self$results$help$setContent(HELP_all[[1]])
          
          
        }          
        
        private$.notrun=FALSE
        ### inspect the data to retrieve some info about the variables
        private$.inspect()
        ### define the table to be filled
        atable<-SmartTable$new(self$results$info)
        ## give the table some data to init it
        atable$initSource<-private$.infotable
        ## add the table to a list
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
        
        ### now we init the tables
        lapply(private$.tables,function(x) x$initTable())          
        jinfo("MODULE: init ended")
        
      },
      ## this post init is needed to avoid accordion (shrinking and widening)
      ## of tables associated with data
      .postInit = function() {
        if ( ! is.null(self$results$showdata$state)) {
          atable <- private$.tables[["showdata"]]
          atable$initSource <- private$.showdata
          atable$initTable()
        }
        
      },
      
      .run = function() {

        if (private$.notrun)    return()
        
        ## give the tables a function to fill them 
        private$.tables[["info"]]$runSource<-private$.infotable
        private$.tables[["showdata"]]$runSource<-private$.showdata
        
        # this was suggested by jonathon, but I do not remember why
        self$results$showdata$deleteRows()
        
        ## fill the tables
        lapply(private$.tables,function(x) x$runTable())
        ### check if a new datasets is needed
        private$.create()
      },
      .create=function() {
        if (self$options$btnOut) {
          
        jinfo("MODULE: creating new dataset")
        .data<-subset(self$data, select=private$.names)
        ## opendata runs jmvReadWrite:::jmvOpn()
        opendata(.data)
        }
        
      },
      
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
        ## this is necessary to avoid table accordion
        data <- self$results$showdata$state
        if (is.null(data)) {
          data<-subset(self$data, select=private$.names)
          ## showdata reduce the data preview in cases of large files
          data<-showdata(self,data)
          self$results$showdata$setState(data)
        }
        data
      }
      
      

    )
)
