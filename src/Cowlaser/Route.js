'use first';

exports.extractFirstPathComponent = function(uri) {
  var match = /^\/?(.*?)([/?].*)?$/.exec(uri);
  return {first: match[1], rest: match[2] || ''};
};

exports.isRoot = function(uri) {
  return /^\/?(\?.*)?$/.test(uri);
};
