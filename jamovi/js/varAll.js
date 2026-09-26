// varAll.js
'use strict';
const same = (a, b) => a.length === b.length && a.every((v, i) => v === b[i]);

module.exports = {
    view_loaded: function(ui, event) {

        this.getColumnNames = () => {
            return this.requestData('columns', {})
                .then((data) => {
                    return data.columns.map(col => col.name);
                })
                .then((names) => {
                    // Exclude filters
                    let index = 0;
                    while (
                        /^Filter [1-9][0-9]*$/.exec(names[index]) ||
                        /^F[1-9][0-9]* \([1-9][0-9]*\)$/.exec(names[index])
                    ) {
                        index++;
                    }
                    return names.slice(index);
                });
        };

        // Populate the UI element with column names
        this.getColumnNames().then((columns) => {
            ui.varAll.setValue(columns);
        });
    },

    // Called to update the UI when data changes
    update: function(ui) {
        this.getColumnNames().then((columns) => {
            if (!same(ui.varAll.value(), columns)) {
                ui.varAll.setValue();
            }
        });
    },

    // Called when data changes remotely, e.g., when the dataset is modified
    dataChanged: function(ui, event) {
        if (event.dataType !== 'columns') return;

        this.getColumnNames().then((columns) => {
            if (!same(ui.varAll.value(), columns)) {
                ui.varAll.setValue(columns);
            }
        });
    }
};
