'use strict';

module.exports = {

    srcCst_creating(ui) {
        let $contents = ui.srcCst.$el;
        $contents.append(`<input type="search" name="srcCst" size=50><br>`);
        $contents.on("input", (f) => { setTimeout(() => { ui.srcTrm.setValue(`${f.target.value}`); }, 800); });
        ui.srcTrm.setValue("")
    }

};
