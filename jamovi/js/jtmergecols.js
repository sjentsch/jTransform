// jtmergecols.js
'use strict';
const same = (a, b) => a.length === b.length && a.every((v, i) => v === b[i]);

module.exports = {
    view_loaded: function(ui, event) {
        // 1. Add "Browse..." button for file selection
        let $fleChs = ui.fleChs.$el;
        $fleChs.append(`
            <label>
                <input type="button" style="display: none;"/>
                <span id="btnChs" class="button-style" style="font-size: 1.3em;">Browse...</span>
            </label>
        `);

        // 2. Tooltip for the "Browse..." button
        TooltipManager.createTooltip(
            $fleChs.find('#btnChs')[0],
            'Click to select a file to merge',
            'left'
        );

        // 3. Custom file dialog trigger
        document.getElementById('tglChs').addEventListener('click', () => {
            console.log("Click on Browse")
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
