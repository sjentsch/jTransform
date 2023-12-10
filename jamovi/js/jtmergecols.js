'use strict';

module.exports = {

    fleChs_creating: function(ui) {
        console.log("fleChs_creating")
        if (navigator.userAgent.toLowerCase().indexOf(' electron/') > -1 && navigator.platform === 'Win32') {
            console.log("Windows")
        } else {
            console.log("Linux, Mac or Cloud")
            let $contents = ui.fleChs.$el;
            $contents.append(`<label><input type="file" multiple accept=".omv,.csv,.sav,.xpt,.sas7bdat,.dta,.jasp" style="display: none;" /><span style="font-size: 1.2em;">Browse...</span></label>`);
            $contents.on("change", (f) => { var crrTxt = ""; for (const crrFle of f.target.files) { crrTxt += `${crrFle.path}; `; }; ui.fleInp.setValue(crrTxt.slice(0, -2)); });
        }
    }

};
