jtmergeClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "jtmergeClass",
    inherit = jtmergeBase,

    private = list(
      .names=NULL,
      .exnames=NULL,
      .exdata=NULL,
      .commons=NULL,
      .mergedata=NULL,
      .tables=list(),
      .notrun=FALSE,
      .init=function() {
        
        jinfo("MODULE: merge init phase started")

        if (!self$options$btnOut)
            self$results$help$setContent(HELP_all[[1]])
        
        if (!is.something(self$options$fleInp)) {
          self$results$help$setContent(HELP_merge[[1]])
          private$.notrun=TRUE
          return()
          
        }
        if (!is.something(self$options$varBy)) {
          self$results$help$setContent(HELP_merge[[2]])
          private$.notrun=TRUE
        }
        
        private$.inspect()
        
        if (length(private$.commons)==0) {
          private$.notrun=TRUE
          self$results$help$setContent("<h1>Warning</h1>
                                       <div><p>No common variables in the datasets, merging it is not possible.</p>
                                       </div>")
          
        }
        
        atable<-SmartTable$new(self$results$features)
        atable$initSource<-lapply(private$.commons,function(x) list(var=x))
        private$.tables[["features"]]<-atable
        atable<-SmartTable$new(self$results$showdata)
        atable$expandOnInit<-TRUE
        atable$expandOnRun<-TRUE
        atable$expandFrom<-2
        .names<-self$options$varBy
        if (length(.names)>10) .names<-.names[1:10]
        tab<-as.data.frame(matrix(".",ncol = length(.names),nrow = 1))
        names(tab)<-.names
        atable$initSource<-tab
        private$.tables[["showdata"]]<-atable
        
        atable<-SmartTable$new(self$results$info)
        atable$initSource<-private$.infotable
        private$.tables[["info"]]<-atable
        
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
        
        
        jinfo("MODULE: jtmerge run phase")
        
        private$.tables[["features"]]$runSource<-private$.features
        private$.tables[["showdata"]]$runSource<-private$.showdata
        private$.tables[["info"]]$runSource<-private$.infotable
        
        self$results$showdata$deleteRows()
        
        lapply(private$.tables,function(x) x$runTable())
        
        private$.create()
      },
      .create=function() {
        
        if (private$.notrun)
            return()
        
        if (self$options$btnOut) {
          jinfo("MODULE: creating new dataset")
          opendata(private$.mergedata)
        }

      },
      .inspect=function() {
        
        private$.names<-names(self$data)
        filename<-self$options$fleInp
        private$.exdata<-notallmissing(jmvReadWrite:::read_all(filename))
        private$.exnames<-names(private$.exdata)
        private$.commons<-intersect(private$.names,private$.exnames)
      },
      .infotable=function() {
        jinfo("MODULE: filling infotable")

        tab<-list(
          list(text="Variables in this dataset",var=length(private$.names)),
          list(text="Variables in external dataset",var=length(private$.exnames)),
          list(text="Common Variables",var=length(private$.commons)),
          list(text="Matching Variables",var=length(self$options$varBy)),
          list(text="Variables in Merged Dataset"),
          list(text="Cases in Dataset"),
          list(text="Cases in External Dataset"),
          list(text="Cases in Merged Dataset")
        )
          if (dim(self$data)[1]>0) {
            .data<-notallmissing(self$data)
            tab[[5]]$var<-dim(private$.mergedata)[2]
            tab[[6]]$var<-dim(.data)[1]
            tab[[7]]$var<-dim(private$.exdata)[1]
            tab[[8]]$var<-dim(private$.mergedata)[1]
            mark(tail(private$.mergedata))
          }
        tab
      },
      .features=function() {

        att1<-jmvReadWrite:::jmvAtt(self$data)
        att2<-jmvReadWrite:::jmvAtt(private$.exdata)

        lapply(private$.commons, function(x) {
            list(var=x,
                 lab1=attr(att1[[x]],"measureType"),
                 lab2=attr(att2[[x]],"measureType")
                  )
        })

      },
      .merge=function() {
        
        if (private$.notrun)    return()
        
         .data<-notmissing(self$options$varBy,self$data)
         .exdata<-private$.exdata
         .commons<-setdiff(private$.commons,self$options$varBy)
         if (self$options$common=="left")
           .exdata<-.exdata[,!(names(.exdata) %in% .commons) ]
         if (self$options$common=="right")
           .data<-.data[,!(names(.data) %in% .commons) ]
          opts<-list(x=.data,y=.exdata,by=self$options$varBy)
          switch (self$options$type,
            outer = opts$all<-TRUE,
            inner = opts$all<-FALSE,
            left  = opts$all.x<-TRUE,
            right = opts$all.y<-TRUE
          )
          private$.mergedata<-do.call(merge,opts)

      },
      .showdata=function() {
        
        if (private$.notrun)    return()
        private$.merge()
        data <- self$results$showdata$state
        if (is.null(data)) {
          data<-showdata(self,private$.mergedata)
          self$results$showdata$setState(data)
        }
        data
      }
    )
)
