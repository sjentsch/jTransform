'use strict';

module.exports = {

    rplCst_creating(ui) {
        let $contents = ui.rplCst.$el;
        $contents.append(`<textarea name="rplCst" class="text" rows="4" cols="50" style="resize: none;"></textarea><br>`);
        $contents.on("input", (f) => { if (this.crrPgr) { clearTimeout(this.crrPgr); }; this.crrPgr = setTimeout(() => { ui.rplTrm.setValue(`${f.target.value}`); this.crrPgr = null; }, 800); });
        ui.rplTrm.setValue("");
//      ui.txtPvw.setValue("");
    }

};
