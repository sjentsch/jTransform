// varAll.js
'use strict';
require('./css');
const { enableLoggingIndicator, TooltipManager, DOMUtils } = require('./ModuleUtils');

module.exports = {
    /**
     * This function is called when the view is loaded in the UI.
     * Adds the LOGGING ACTIVE indicator to the interface.
     */
    view_loaded: function(ui, event) {

        // 1. Enable the LOGGING ACTIVE indicator using the utility function
        enableLoggingIndicator({ ui });

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

    /**
     * Called to update the UI when data changes.
     */
    update: function(ui) {
        this.getColumnNames().then((columns) => {
            if (!_.isEqual(ui.varAll.value(), columns)) {
                ui.varAll.setValue(columns);
            }
        });
    },

    /**
     * Called when data changes remotely, e.g., when the dataset is modified.
     */
    dataChanged: function(ui, event) {
        if (event.dataType !== 'columns') return;

        this.getColumnNames().then((columns) => {
            if (!_.isEqual(ui.varAll.value(), columns)) {
                ui.varAll.setValue(columns);
            }
        });
    }
};
