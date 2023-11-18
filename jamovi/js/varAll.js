'use strict';

module.exports = {

    loaded(ui) {

        this.getColumnNames = () => {
            return this.requestData('columns', {
                }).then((data) => {
                    return data.columns.map(col => col.name);
                }).then((names) => {
                    // exclude filters
                    let index = 0;
                    while (/^Filter [1-9][0-9]*$/.exec(names[index]) ||
                           /^F[1-9][0-9]* \([1-9][0-9]*\)$/.exec(names[index])) {
                        index++
                    }
                    return names.slice(index);
                });
        };

        this.getColumnNames().then((columns) => {
            ui.varAll.setValue(columns);
        });

    },

    update(ui) {

        this.getColumnNames().then((columns) => {
            if (ui.varAll.value().toString() !== columns.toString()) {
                ui.varAll.setValue(columns);
            }
        });

    },

    dataChanged(ui, event) {
  
        if (event.dataType !== 'columns')
            return;
        this.getColumnNames().then((columns) => {
            if (ui.varAll.value().toString() !== columns.toString()) {
                ui.varAll.setValue(columns);
            }
        });

    }

};
