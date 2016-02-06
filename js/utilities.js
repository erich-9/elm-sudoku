'use strict';


Array.prototype.initialize = function(n, f)
{
  return Array.apply(null, new Array(n)).map(function(x, i) { return f(i); });
};
