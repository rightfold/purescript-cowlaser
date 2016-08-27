'use strict';

exports.compareCI = function(a) {
  return function(b) {
    return a.toUpperCase() === b.toUpperCase();
  };
};
