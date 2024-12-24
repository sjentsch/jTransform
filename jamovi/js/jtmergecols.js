'use strict';
require('./css');
const { TooltipManager, DOMUtils } = require('./ModuleUtils');

module.exports = {

    loaded: function(ui) {

        let $btnchs = ui.fleChs.$el;
        $btnchs.append(`<label>
                        <input type="button" style="display: none;"/>
                        <span id="butsf-file" class="button-style" style="font-size: 1.3em;">Browse...</span>
                        </label>`);

        // Tooltip for buttons with control to not exit the display area
        TooltipManager.createTooltip($btnchs.find('#butsf-file')[0], 'Click to select a files to merge', 'left');

        // Custom file dialog trigger
        document.getElementById('butsf-file').addEventListener('click', () => {
            DOMUtils.createCustomFileDialog(ui, (filePaths) => {
                ui.fleInp.setValue(filePaths);
            });
        });

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
            if (!_.isEqual(ui.varAll.value(), columns)) {
                ui.varAll.setValue(columns);
            }
        });
    },

    dataChanged(ui, event) {
        if (event.dataType !== 'columns') return;

        this.getColumnNames().then((columns) => {
            if (!_.isEqual(ui.varAll.value(), columns)) {
                ui.varAll.setValue(columns);
            }
        });
    }
};
