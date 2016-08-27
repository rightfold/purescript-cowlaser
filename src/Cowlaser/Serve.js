'use strict';

var hasOwnProperty = Object.prototype.hasOwnProperty;

exports.makeHeaders = function(array) {
  var map = {};
  for (var i = 0, n = array.length; i < n; ++i) {
    var pair = array[i];
    var name = pair.name.toUpperCase();
    var value = pair.value;
    if (!hasOwnProperty.call(map, name)) {
      map[name] = [value];
    } else {
      map[name].push(value);
    }
  }
  return map;
};
