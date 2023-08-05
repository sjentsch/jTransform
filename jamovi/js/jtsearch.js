'use strict';

module.exports = {

    srcCst_creating(ui) {
        let $contents = ui.srcCst.$el;
        $contents.append(`<input type="search" name="srcCst" class="text" size=50><br>`);
        $contents.on("input", (f) => { if (this.crrPgr) { clearTimeout(this.crrPgr); }; this.crrPgr = setTimeout(() => { ui.srcTrm.setValue(`${f.target.value}`); this.crrPgr = null; }, 800); });
        ui.srcTrm.setValue("");
//      ui.srcRes.setValue("");
    }

};
