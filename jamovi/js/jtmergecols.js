// jtmergecols.js
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

        // 2. Add "Browse..." button for file selection
        let $btnchs = ui.fleChs.$el;
        $btnchs.append(`
            <label>
                <input type="button" style="display: none;"/>
                <span id="butsf-file" class="button-style" style="font-size: 1.3em;">Browse...</span>
            </label>
        `);

        // 3. Tooltip for the "Browse..." button
        TooltipManager.createTooltip(
            $btnchs.find('#butsf-file')[0],
            'Click to select a file to merge',
            'left'
        );

        // 4. Custom file dialog trigger
        document.getElementById('butsf-file').addEventListener('click', () => {
            DOMUtils.createCustomFileDialog(ui, (filePaths) => {
                ui.fleInp.setValue(filePaths);
            });
        });
    
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
    }
};

