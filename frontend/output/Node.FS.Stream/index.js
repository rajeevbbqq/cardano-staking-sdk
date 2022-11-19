// Generated by purs version 0.14.5
"use strict";
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Nullable = require("../Data.Nullable/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var Node_FS = require("../Node.FS/index.js");
var Node_FS_Internal = require("../Node.FS.Internal/index.js");
var Node_FS_Perms = require("../Node.FS.Perms/index.js");
var readWrite = (function () {
    var rw = Data_Semiring.add(Node_FS_Perms.semiringPerm)(Node_FS_Perms.read)(Node_FS_Perms.write);
    return Node_FS_Perms.mkPerms(rw)(rw)(rw);
})();
var $$null = Data_Nullable.toNullable(Data_Maybe.Nothing.value);
var nonnull = function ($4) {
    return Data_Nullable.toNullable(Data_Maybe.Just.create($4));
};
var fs = Node_FS_Internal.unsafeRequireFS;
var fdCreateWriteStreamWith = function (opts) {
    return function (fd) {
        return Node_FS_Internal.mkEffect(function (v) {
            return fs.createWriteStream($$null, {
                fd: fd,
                mode: Node_FS_Perms.permsToInt(opts.perms),
                flags: Node_FS.fileFlagsToNode(opts.flags)
            });
        });
    };
};
var fdCreateReadStreamWith = function (opts) {
    return function (fd) {
        return Node_FS_Internal.mkEffect(function (v) {
            return fs.createReadStream($$null, {
                fd: fd,
                mode: Node_FS_Perms.permsToInt(opts.perms),
                flags: Node_FS.fileFlagsToNode(opts.flags),
                autoClose: opts.autoClose
            });
        });
    };
};
var defaultWriteStreamOptions = {
    flags: Node_FS.W.value,
    perms: readWrite
};
var fdCreateWriteStream = fdCreateWriteStreamWith(defaultWriteStreamOptions);
var defaultReadStreamOptions = {
    flags: Node_FS.R.value,
    perms: readWrite,
    autoClose: true
};
var fdCreateReadStream = fdCreateReadStreamWith(defaultReadStreamOptions);
var createWriteStreamWith = function (opts) {
    return function (file) {
        return Node_FS_Internal.mkEffect(function (v) {
            return fs.createWriteStream(nonnull(file), {
                mode: Node_FS_Perms.permsToInt(opts.perms),
                flags: Node_FS.fileFlagsToNode(opts.flags)
            });
        });
    };
};
var createWriteStream = createWriteStreamWith(defaultWriteStreamOptions);
var createReadStreamWith = function (opts) {
    return function (file) {
        return Node_FS_Internal.mkEffect(function (v) {
            return fs.createReadStream(nonnull(file), {
                mode: Node_FS_Perms.permsToInt(opts.perms),
                flags: Node_FS.fileFlagsToNode(opts.flags),
                autoClose: opts.autoClose
            });
        });
    };
};
var createReadStream = createReadStreamWith(defaultReadStreamOptions);
module.exports = {
    createWriteStream: createWriteStream,
    fdCreateWriteStream: fdCreateWriteStream,
    defaultWriteStreamOptions: defaultWriteStreamOptions,
    createWriteStreamWith: createWriteStreamWith,
    fdCreateWriteStreamWith: fdCreateWriteStreamWith,
    createReadStream: createReadStream,
    fdCreateReadStream: fdCreateReadStream,
    defaultReadStreamOptions: defaultReadStreamOptions,
    createReadStreamWith: createReadStreamWith,
    fdCreateReadStreamWith: fdCreateReadStreamWith
};
