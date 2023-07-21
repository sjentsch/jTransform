const events = {

    rstBtn: function(ui) {
        ui.blnOut.setValue(false);
    },

    crtBtn: function(ui) {
        // I am using a (bit of a) trick here: typically, blnOut would be false (it is created with false and whenever
        // an UI element is changed, it is reset to false, however, if it is true after an error, the function under
        // click toggles it (i.e., first resets it to false and afterwards sets it to true again; this ensures that
        // run() is called (if the value isn't changed, it isn't)
        let $contents = ui.btnOut.$el;
        $contents.append(`<input type="submit" value="CREATE" style="font-size: 1.2em; font-weight: bold">`);
        $contents.on("click", () => { ui.blnOut.setValue(!ui.blnOut.value()); });
        ui.blnOut.setValue(false);
    },

    rstFlC: function(ui) {
        ui.blnOut.setValue(false);
        ui.fleInp.setValue("");
    },

    crtFlC: function(ui) {
        let $contents = ui.fleChs.$el;
        $contents.append(`<label><input type="file" multiple accept=".omv,.csv,.sav,.xpt,.sas7bdat,.dta,.jasp" style="display: none;" /><span style="font-size: 1.2em;">Browse...</span></label>`);
        $contents.on("change", (f) =>  { var crrTxt = ""; for (const crrFle of f.target.files) { crrTxt += `${crrFle.name}; `; }; ui.fleInp.setValue(crrTxt.slice(0, -2)); });
        ui.blnOut.setValue(false);
        ui.fleInp.setValue("");
    }

};

module.exports = events;
