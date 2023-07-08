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
        ui.fleInp.setValue("");
    },

    crtFlC: function(ui) {
        let $contents = ui.fleInp.$el;
        $contents.append(`<input type="file" accept=".omv,.csv,.sav,.xpt,.sas7bdat,.dta,.jasp" multiple/>`);
        $contents.on("click", () =>  { ui.fleInp.setValue(""); });
        ui.fleInp.setValue("");
    }

};

module.exports = events;
