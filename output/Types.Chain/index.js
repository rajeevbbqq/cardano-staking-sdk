// Generated by purs version 0.14.5
"use strict";
var Data_Eq = require("../Data.Eq/index.js");
var Data_Generic_Rep = require("../Data.Generic.Rep/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_Show_Generic = require("../Data.Show.Generic/index.js");
var Serialization_Address = require("../Serialization.Address/index.js");
var BlockHeaderHash = function (x) {
    return x;
};
var ChainTip = function (x) {
    return x;
};
var TipAtGenesis = (function () {
    function TipAtGenesis() {

    };
    TipAtGenesis.value = new TipAtGenesis();
    return TipAtGenesis;
})();
var Tip = (function () {
    function Tip(value0) {
        this.value0 = value0;
    };
    Tip.create = function (value0) {
        return new Tip(value0);
    };
    return Tip;
})();
var newtypeChainTip_ = {
    Coercible0: function () {
        return undefined;
    }
};
var newtypeBlockHeaderHash_ = {
    Coercible0: function () {
        return undefined;
    }
};
var genericTip_ = {
    to: function (x) {
        if (x instanceof Data_Generic_Rep.Inl) {
            return TipAtGenesis.value;
        };
        if (x instanceof Data_Generic_Rep.Inr) {
            return new Tip(x.value0);
        };
        throw new Error("Failed pattern match at Types.Chain (line 18, column 1 - line 18, column 30): " + [ x.constructor.name ]);
    },
    from: function (x) {
        if (x instanceof TipAtGenesis) {
            return new Data_Generic_Rep.Inl(Data_Generic_Rep.NoArguments.value);
        };
        if (x instanceof Tip) {
            return new Data_Generic_Rep.Inr(x.value0);
        };
        throw new Error("Failed pattern match at Types.Chain (line 18, column 1 - line 18, column 30): " + [ x.constructor.name ]);
    }
};
var genericChainTip_ = {
    to: function (x) {
        return x;
    },
    from: function (x) {
        return x;
    }
};
var genericBlockHeaderHash_ = {
    to: function (x) {
        return x;
    },
    from: function (x) {
        return x;
    }
};
var showBlockHeaderHash = {
    show: Data_Show_Generic.genericShow(genericBlockHeaderHash_)(Data_Show_Generic.genericShowConstructor(Data_Show_Generic.genericShowArgsArgument(Data_Show.showString))({
        reflectSymbol: function () {
            return "BlockHeaderHash";
        }
    }))
};
var showChainTip = {
    show: Data_Show_Generic.genericShow(genericChainTip_)(Data_Show_Generic.genericShowConstructor(Data_Show_Generic.genericShowArgsArgument(Data_Show.showRecord()(Data_Show.showRecordFieldsCons({
        reflectSymbol: function () {
            return "blockHeaderHash";
        }
    })(Data_Show.showRecordFieldsCons({
        reflectSymbol: function () {
            return "slot";
        }
    })(Data_Show.showRecordFieldsNil)(Serialization_Address.showSlot))(showBlockHeaderHash))))({
        reflectSymbol: function () {
            return "ChainTip";
        }
    }))
};
var showTip = {
    show: Data_Show_Generic.genericShow(genericTip_)(Data_Show_Generic.genericShowSum(Data_Show_Generic.genericShowConstructor(Data_Show_Generic.genericShowArgsNoArguments)({
        reflectSymbol: function () {
            return "TipAtGenesis";
        }
    }))(Data_Show_Generic.genericShowConstructor(Data_Show_Generic.genericShowArgsArgument(showChainTip))({
        reflectSymbol: function () {
            return "Tip";
        }
    })))
};
var eqBlockHeaderHash = Data_Eq.eqString;
var eqChainTip = Data_Eq.eqRec()(Data_Eq.eqRowCons(Data_Eq.eqRowCons(Data_Eq.eqRowNil)()({
    reflectSymbol: function () {
        return "slot";
    }
})(Serialization_Address.eqSlot))()({
    reflectSymbol: function () {
        return "blockHeaderHash";
    }
})(eqBlockHeaderHash));
var eqTip = {
    eq: function (x) {
        return function (y) {
            if (x instanceof TipAtGenesis && y instanceof TipAtGenesis) {
                return true;
            };
            if (x instanceof Tip && y instanceof Tip) {
                return Data_Eq.eq(eqChainTip)(x.value0)(y.value0);
            };
            return false;
        };
    }
};
module.exports = {
    TipAtGenesis: TipAtGenesis,
    Tip: Tip,
    ChainTip: ChainTip,
    BlockHeaderHash: BlockHeaderHash,
    genericTip_: genericTip_,
    eqTip: eqTip,
    showTip: showTip,
    newtypeChainTip_: newtypeChainTip_,
    genericChainTip_: genericChainTip_,
    eqChainTip: eqChainTip,
    showChainTip: showChainTip,
    genericBlockHeaderHash_: genericBlockHeaderHash_,
    newtypeBlockHeaderHash_: newtypeBlockHeaderHash_,
    eqBlockHeaderHash: eqBlockHeaderHash,
    showBlockHeaderHash: showBlockHeaderHash
};
