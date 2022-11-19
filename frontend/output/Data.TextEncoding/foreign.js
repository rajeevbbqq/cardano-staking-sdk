'use strict'

function importTextEncoding() {
  try {
    return require('util').TextEncoder
  } catch (e) {
    console.log(
      'You appear to be trying to use the purescript-text-encoding in a node based ' +
        'environment that does not support the TextEncoding api introduced in node version 8.3.0. \n' +
        'It is recommended to update your node environment beyond that to support the necessary api.' +
        ' if this is not sufficient, please feel free to contact the maintainer of this library via its github here:\n' +
        'https://github.com/AlexaDeWit/purescript-text-encoding'
    )
    throw new Error('TextEncoder could not be imported from node environment')
  }
}
// `TextEncoder` is not available in `node`, use polyfill in that case
var TextEncoder =
  (typeof window === 'object' && window.TextEncoder) ||
  (typeof require === 'function' && importTextEncoding())

exports._encode = function(utfLabel, str) {
  var encoder = new TextEncoder(utfLabel)

  return encoder.encode(str)
}
