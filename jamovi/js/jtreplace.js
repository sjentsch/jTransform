'use strict';

module.exports = {

    rplCst_creating(ui) {
        let $contents = ui.rplCst.$el;
        $contents.append(`<textarea name="rplCst" rows="4" cols="50" style="resize: none;"></textarea><br>`);
        $contents.on("input", (f) => { setTimeout(() => { ui.rplTrm.setValue(`${f.target.value}`); }, 800); });
        ui.rplTrm.setValue("")
    }

};
