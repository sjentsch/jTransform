const events = {

    rstBtn: function(ui) {
        ui.blnOut.setValue(false);
    },

    crtBtn: function(ui) {
        let $contents = ui.btnOut.$el;
        $contents.append(`<input type="submit" value="WRITE" style="font-size: 1.2em; font-weight: bold">`);
        $contents.on("click", () => { ui.blnOut.setValue(true); });
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
