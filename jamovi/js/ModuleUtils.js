'use strict';

/**
 * Tooltip Management Utility
 */
const TooltipManager = {
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
 * DOM Manipulation Utility
 */
const DOMUtils = {
    insertAfter(referenceNode, newNode) {
        referenceNode.parentNode.insertBefore(newNode, referenceNode.nextSibling);
    },

    createDivWithClass(className) {
        const div = document.createElement('div');
        div.className = className;
        return div;
    },

    addClass(element, className) {
        element.classList.add(className);
    },

    removeClass(element, className) {
        element.classList.remove(className);
    },

    createCustomFileDialog(ui, callback) {
        const overlay = DOMUtils.createDivWithClass('file-dialog-overlay');
        const dialog = DOMUtils.createDivWithClass('file-dialog');

        const title = DOMUtils.createDivWithClass('file-dialog-title');
        const iconContainer = DOMUtils.createDivWithClass('file-dialog-title-icon');
        iconContainer.innerHTML = '&#x1F4C2';

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

        fileInput.addEventListener('change', (event) => {
            Array.from(event.target.files).forEach(file => {
                files.push(file);
                const fileItem = DOMUtils.createDivWithClass('file-item');
                fileItem.textContent = file.path;
                fileList.appendChild(fileItem);
            });
            confirmButton.disabled = false;
        });

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

        overlay.style.borderRadius = '15px';
        dialog.style.borderTopLeftRadius = '50px';
        dialog.style.borderTopRightRadius = '15px';
        dialog.style.borderBottomLeftRadius = '15px';
        dialog.style.borderBottomRightRadius = '15px';

        document.body.appendChild(overlay);
    }
};

// We export all utilities
module.exports = {
    TooltipManager,
    DOMUtils
};
