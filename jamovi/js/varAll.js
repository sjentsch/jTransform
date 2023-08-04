'use strict';

module.exports = {

    loaded(ui) {

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

    remoteDataChanged(ui, event) {
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
