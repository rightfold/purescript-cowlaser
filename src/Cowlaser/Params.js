'use strict';

var url = require('url');

exports._query = function(key) {
  return function(uri) {
    var query = url.parse(uri, true).query;
    var values = query[key];
    if (values === undefined) {
      return [];
    } else if (typeof values === 'string') {
      return [values];
    } else {
      return values;
    }
  };
};
