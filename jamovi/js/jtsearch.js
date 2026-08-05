'use strict';

module.exports = {

    srcCst_creating(ui) {
        let $contents = ui.srcCst.$el;
        $contents.append(`<input type="search" name="srcCst" class="text" size=40><br>`);
        $contents.find('input').val(ui.srcTrm.value() || '');
        $contents.on("input", (f) => {
            if (this.crrPgr) clearTimeout(this.crrPgr);
            this.crrPgr = setTimeout(() => { ui.srcTrm.setValue(`${f.target.value}`); this.crrPgr = null; }, 800);
        });
    }

};
