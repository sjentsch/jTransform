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
        let $contents = ui.fleChs.$el;
        $contents.append(`<input type="file" if = "" accept=".omv,.csv,.sav,.xpt,.sas7bdat,.dta,.jasp" multiple/>`);
        $contents.on("change", function(e) =>  { if (e.target.files[0]) { ui.fleInp.setValue(e.target.files.name); } });
        ui.fleInp.setValue("");
    }

};

module.exports = events;
