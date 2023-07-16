'use strict';

module.exports = {

    varSrt_changed: function(ui) {
        var varVal = utils.clone(ui.varSrt.value(), []);
        var ordVal = utils.clone(ui.ordSrt.value(), []);
        var ordUpd = [];

        for (let i = 0; i < varVal.length; i++) {
            let ordFnd = false;
            for (let j = 0; j < ordVal.length; j++) {
                if (ordVal[j].var === varVal[i]) {
                    ordFnd = true;
                    ordUpd.push(ordVal[j]);
                    break;
                }
            }
            if (! ordFnd)
                ordUpd.push({ var: varVal[i], order: "ascend" });
        }

        if (ordVal === null ? 0 : ordVal.length !== ordUpd.length || JSON.stringify(ordVal) !== JSON.stringify(ordUpd))
            ui.ordSrt.setValue(ordUpd);
    }
};
