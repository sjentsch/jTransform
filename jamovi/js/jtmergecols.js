// jtmergecols.js
'use strict';
require('./css');
const { TooltipManager, KeyboardShortcuts, DOMUtils, ToastManager } = require('./ModuleUtils');

module.exports = {

    view_loaded: function(ui, event) {

        // 1. Create a container that holds both LOGGING ACTIVE label and future toast.
        const loggingContainer = DOMUtils.createDivWithClass('logging-container');

        // Insert this container after the <h1>
        const header = document.querySelector('.silky-options-header');
        const titleContainer = header.querySelector('h1');
        DOMUtils.insertAfter(titleContainer, loggingContainer);

        // 2. Create the "LOGGING ACTIVE" label
        const loggingMessage = DOMUtils.createDivWithClass('logging-active hidden');
        loggingMessage.id = 'logging-message';
        loggingMessage.innerHTML = `<span>LOGGING ACTIVE</span>`;
        loggingContainer.appendChild(loggingMessage);

        // 3. Show/hide the label
        function toggleLoggingMessage(isVisible) {
            if (isVisible) {
                DOMUtils.addClass(loggingMessage, 'visible');
                DOMUtils.removeClass(loggingMessage, 'hidden');
            } else {
                DOMUtils.addClass(loggingMessage, 'hidden');
                DOMUtils.removeClass(loggingMessage, 'visible');
            }
        }

        // 4. Tooltip for the label
        TooltipManager.createTooltip(
            loggingMessage,
            'Press CTRL+ALT+L to disable logging',
            'center'
        );

        // 5. Shortcuts
        KeyboardShortcuts.addShortcut(['ctrl', 'shift', 'l'], () => {
            console.log('Ctrl+Shift+L pressed. Enabling logging.');
            ui.jlog.setValue(true);

            // Show a toast that EXACTLY covers the "LOGGING ACTIVE" label
            // for 2 seconds, then disappears
            ToastManager.showToast('LOGGING ENABLED', 2000, loggingContainer);

            // Leave the label visible after the toast goes away
            toggleLoggingMessage(true);
        });

        KeyboardShortcuts.addShortcut(['ctrl', 'alt', 'l'], () => {
            console.log('Ctrl+Alt+L pressed. Disabling logging.');
            ui.jlog.setValue(false);

            // Show a toast that covers the label
            ToastManager.showToast('LOGGING DISABLED', 2000, loggingContainer);

            // After the toast is gone, also hide the label
            toggleLoggingMessage(false);
        });

        // 6. Creating the 'Browse...' button for select files
        let $btnchs = ui.fleChs.$el;
        $btnchs.append(`
            <label>
                <input type="button" style="display: none;"/>
                <span id="butsf-file" class="button-style" style="font-size: 1.3em;">Browse...</span>
            </label>
        `);

        // 7. Tooltip for the "Browse..." button
        TooltipManager.createTooltip(
            $btnchs.find('#butsf-file')[0],
            'Click to select a file to merge',
            'left'
        );

        // 8. Custom file dialog trigger
        document.getElementById('butsf-file').addEventListener('click', () => {
            DOMUtils.createCustomFileDialog(ui, (filePaths) => {
                ui.fleInp.setValue(filePaths);
            });
        });

        // 9. Example: load column names
        this.getColumnNames = () => {
            return this.requestData('columns', {})
                .then((data) => data.columns.map(col => col.name))
                .then((names) => {
                    // exclude filters
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

        this.getColumnNames().then((columns) => {
            ui.varAll.setValue(columns);
        });
    },

    view_updated(ui, event) {
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

