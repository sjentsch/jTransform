// ModuleUtils.js
'use strict';

/**
 * Tooltip Management Utility
 */
const TooltipManager = {
    /**
     * Creates a simple tooltip for the given element, showing "text"
     * when hovering on "element". The "position" parameter can be 'left',
     * 'right', or 'center'.
     */
    createTooltip(element, text, position = 'center') {
        const tooltip = document.createElement('div');
        tooltip.className = 'custom-tooltip';
        tooltip.textContent = text;
        document.body.appendChild(tooltip);

        element.addEventListener('mouseover', (event) => {
            tooltip.style.display = 'block';

            // Dynamic positioning
            const tooltipWidth = tooltip.offsetWidth;
            const tooltipHeight = tooltip.offsetHeight;
            const { pageX, pageY } = event;

            switch (position) {
                case 'left':
                    tooltip.style.left = `${pageX - tooltipWidth - 10}px`;
                    break;
                case 'right':
                    tooltip.style.left = `${pageX + 10}px`;
                    break;
                case 'center':
                default:
                    tooltip.style.left = `${pageX - tooltipWidth / 2}px`;
            }
            tooltip.style.top = `${pageY + 10}px`;
        });

        element.addEventListener('mousemove', (event) => {
            const tooltipWidth = tooltip.offsetWidth;
            const { pageX } = event;

            if (position === 'center') {
                tooltip.style.left = `${pageX - tooltipWidth / 2}px`;
            }
        });

        element.addEventListener('mouseout', () => {
            tooltip.style.display = 'none';
        });
    }
};

/**
 * Keyboard Shortcut Management Utility
 */
const KeyboardShortcuts = {
    /**
     * Adds a shortcut listener. "keys" is an array of key parts
     * (e.g. ['ctrl', 'shift', 'l']). The "callback" is triggered
     * when all keys in "keys" are pressed simultaneously.
     */
    addShortcut(keys, callback) {
        document.addEventListener('keydown', (event) => {
            // Normalize keys to lowercase
            const keyCombo = keys.map((key) => key.toLowerCase());
            const isMatch = keyCombo.every((key) => {
                if (key === 'ctrl') return event.ctrlKey;
                if (key === 'shift') return event.shiftKey;
                if (key === 'alt') return event.altKey;
                return event.key.toLowerCase() === key;
            });

            if (isMatch) {
                event.preventDefault();
                callback(event);
            }
        }, false);
    }
};

/**
 * DOM Manipulation Utility
 */
const DOMUtils = {
    /**
     * Inserts "newNode" into the DOM immediately after "referenceNode".
     */
    insertAfter(referenceNode, newNode) {
        referenceNode.parentNode.insertBefore(newNode, referenceNode.nextSibling);
    },

    /**
     * Creates a <div> with the specified class name.
     */
    createDivWithClass(className) {
        const div = document.createElement('div');
        div.className = className;
        return div;
    },

    /**
     * Adds a CSS class to the given element.
     */
    addClass(element, className) {
        element.classList.add(className);
    },

    /**
     * Removes a CSS class from the given element.
     */
    removeClass(element, className) {
        element.classList.remove(className);
    },

    /**
     * Creates a custom file dialog overlay with drag-and-drop and
     * "Add Files" button, calling "callback" with the selected file paths.
     */
    createCustomFileDialog(ui, callback) {
        const overlay = DOMUtils.createDivWithClass('file-dialog-overlay');
        const dialog = DOMUtils.createDivWithClass('file-dialog');

        const title = DOMUtils.createDivWithClass('file-dialog-title');
        const iconContainer = DOMUtils.createDivWithClass('file-dialog-title-icon');
        iconContainer.innerHTML = '&#x1F4C2'; // folder icon

        const titleText = DOMUtils.createDivWithClass('file-dialog-title-text');
        titleText.innerHTML = 'Select Files';

        title.appendChild(iconContainer);
        title.appendChild(titleText);

        const fileList = DOMUtils.createDivWithClass('file-dialog-list');
        const instructions = document.createElement('p');
        instructions.textContent = 'Drag and drop files here or click Add Files to browse.';
        fileList.appendChild(instructions);

        const fileInput = document.createElement('input');
        fileInput.type = 'file';
        fileInput.multiple = true;
        fileInput.accept = '.omv,.csv,.sav,.xpt,.sas7bdat,.dta,.jasp';
        fileInput.style.display = 'none';

        const buttonContainer = DOMUtils.createDivWithClass('file-dialog-button-container');

        const addButton = DOMUtils.createDivWithClass('button-style');
        addButton.textContent = 'Add Files';
        addButton.addEventListener('click', () => fileInput.click());

        const confirmButton = DOMUtils.createDivWithClass('button-style');
        confirmButton.textContent = 'Confirm';
        confirmButton.disabled = true;

        const cancelButton = DOMUtils.createDivWithClass('button-style');
        cancelButton.textContent = 'Cancel';

        const files = [];

        // file selection
        fileInput.addEventListener('change', (event) => {
            Array.from(event.target.files).forEach(file => {
                files.push(file);
                const fileItem = DOMUtils.createDivWithClass('file-item');
                fileItem.textContent = file.path;
                fileList.appendChild(fileItem);
            });
            confirmButton.disabled = false;
        });

        // drag & drop
        fileList.addEventListener('dragover', (event) => {
            event.preventDefault();
            fileList.classList.add('dragover');
        });

        fileList.addEventListener('dragleave', () => {
            fileList.classList.remove('dragover');
        });

        fileList.addEventListener('drop', (event) => {
            event.preventDefault();
            fileList.classList.remove('dragover');
            Array.from(event.dataTransfer.files).forEach(file => {
                files.push(file);
                const fileItem = DOMUtils.createDivWithClass('file-item');
                fileItem.textContent = file.path;
                fileList.appendChild(fileItem);
            });
            confirmButton.disabled = false;
        });

        confirmButton.addEventListener('click', () => {
            const filePaths = files.map(file => file.path).join('; ');
            callback(filePaths);
            document.body.removeChild(overlay);
        });

        cancelButton.addEventListener('click', () => {
            document.body.removeChild(overlay);
        });

        buttonContainer.appendChild(addButton);
        buttonContainer.appendChild(confirmButton);
        buttonContainer.appendChild(cancelButton);

        dialog.appendChild(title);
        dialog.appendChild(fileList);
        dialog.appendChild(buttonContainer);
        overlay.appendChild(dialog);

        // extra styling to give some rounding
        overlay.style.borderRadius = '15px';
        dialog.style.borderTopLeftRadius = '50px';
        dialog.style.borderTopRightRadius = '15px';
        dialog.style.borderBottomLeftRadius = '15px';
        dialog.style.borderBottomRightRadius = '15px';

        document.body.appendChild(overlay);
    }
};

/**
 * Toast Management Utility
 */
const ToastManager = {

    /**
     * Shows a temporary toast. By default it creates a new <div class="logging-toast">
     * and appends it to the container (if provided) or to document.body.
     *
     * After 'duration' ms, the toast fades out and is removed from DOM.
     *
     * @param {string} message - The toast text
     * @param {number} duration - Milliseconds to show the toast
     * @param {HTMLElement} container - (Optional) where to place the toast
     */
    showToast(message, duration = 3000, container) {
        const toast = document.createElement('div');
        toast.classList.add('logging-toast');
        toast.textContent = message;

        const parent = container || document.body;
        parent.appendChild(toast);

        // Fade-in
        requestAnimationFrame(() => {
            toast.classList.add('show');
        });

        // Fade-out after "duration" ms
        setTimeout(() => {
            toast.classList.remove('show');
            toast.addEventListener('transitionend', () => {
                toast.remove();
            }, { once: true });
        }, duration);
    }
};

// Export all utilities
module.exports = {
    TooltipManager,
    KeyboardShortcuts,
    DOMUtils,
    ToastManager
};
