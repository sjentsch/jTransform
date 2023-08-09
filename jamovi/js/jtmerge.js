'use strict';

module.exports = {


    update: function(ui) {
        console.log("merge is update");
        this.getColumnNames = () => {
            return this.requestData('columns', {  })
                .then((data) => {
                    return data.columns.map(col => col.name);
                }).then((names) => {
                    // exclude filters
                    let index = 0;
                    for (;index < names.length; index++) {
                        let name = names[index];
                        if (/^Filter [1-9][0-9]*$/.exec(name) ||
                            /^F[1-9][0-9]* \([1-9][0-9]*\)$/.exec(name))
                                continue; // a filter
                        else
                            break; // not a filter
                    }
                    return names.slice(index);
                });
        };

        this.getColumnNames().then((columns) => {
            ui.varAll.setValue(columns);
        });
      
    },
    loaded: function(ui) {
      
        console.log("merge is loading");
        let $contents = ui.btnOpenFile.$el;

        $contents.after(`<input id="formOpenFile", type="file" style="display:none" accept=".omv,.csv,.sav,.xpt,.sas7bdat,.dta,.jasp" style="display: none;" />`);
        $contents.on("click", (f) =>  { 
                      $("#formOpenFile").click()
        });
        $("#formOpenFile").on("change", (f) => {
          ui.fleInp.setValue(f.target.files[0].path);
        })
        console.log(ui.fleInp.value());
//        ui.fleInp.setValue("");
        
        this.getColumnNames = () => {
            return this.requestData('columns', {  })
                .then((data) => {
                    return data.columns.map(col => col.name);
                }).then((names) => {
                    // exclude filters
                    let index = 0;
                    for (;index < names.length; index++) {
                        let name = names[index];
                        if (/^Filter [1-9][0-9]*$/.exec(name) ||
                            /^F[1-9][0-9]* \([1-9][0-9]*\)$/.exec(name))
                                continue; // a filter
                        else
                            break; // not a filter
                    }
                    return names.slice(index);
                });
        };

        this.getColumnNames().then((columns) => {
            ui.varAll.setValue(columns);
        });

    },

    dataChanged: function(ui, event) {
        if (event.dataType !== 'columns')
            return;
        this.getColumnNames().then((columns) => {
            let old = ui.varAll.value();
            if (old.toString() !== columns.toString()) {
                ui.varAll.setValue(columns);
            }
        });
    }


};
