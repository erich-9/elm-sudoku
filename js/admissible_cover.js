'use strict';


function AdmissibleCoverProblem(size, subsets, maxCoverSize)
{
  var R = subsets.length;
  var C = size;

  var rows = subsets;
  var fbds = [].initialize(R, function() { return []; });
  var cols = [].initialize(C, function() { return []; });

  rows.forEach(function(row, r) {
    row.forEach(function(c) {
      cols[c].push(r);
    });
  });

  var i, _coverSize = 0;

  if (maxCoverSize === undefined || maxCoverSize > R) {
    maxCoverSize = R;
  }

  var xr = [].initialize(R, function() { return 0; });
  var xc = [].initialize(C, function(c) { return cols[c].length; });

  var cc = [].initialize(maxCoverSize, function() { return -1; });
  var ccr = [].initialize(maxCoverSize + 1, function() { return -1; });


  this.forceExactness = function()
  {
    rows.forEach(function(row, r) {
      row.forEach(function(c) {
        cols[c].forEach(function(r_) {
          _addForbidRule(r, r_);
        });
      });
    });
  };


  this.addForbidRule = function(r, r_)
  {
    _addForbidRule(r, r_);
  };


  this.markAsForbidden = function(r)
  {
    _increaseCollisionCount(r);
  };


  this.startIterationOverChosenSubsets = function()
  {
    i = 0;
  };


  this.nextChosenSubset = function(it)
  {
    return i === _coverSize ? -1 : cols[cc[i]][ccr[i++]];
  };


  this.traverseAdmissibleCovers = function(handler, maxTraversals)
  {
    var K = _coverSize;

    for (var t = 1; t <= maxTraversals; ++t) {

      while (_coverSize >= K && _coverSize < maxCoverSize) {

        var c, min = R + 1;
        for (var c_ = 0; min > 1 && c_ < C; ++c_) {
          if (xc[c_] < min) {
            c = c_, min = xc[c_];
          }
        }

        if (min > 0 && min <= R) {

          var ir = ccr[_coverSize] + 1, colsc_l = cols[c].length;
          while (ir < colsc_l && xr[cols[c][ir]] > 0) {
            ++ir;
          }

          if (ir < colsc_l) {
            _addToCover(cols[c][ir], c, ir);
            continue;
          }
        }

        _removeLastFromCover(K);
      }

      if (_coverSize === maxCoverSize) {
        handler(this);
        _removeLastFromCover(K);
      } else {
        break;
      }
    }
  };


  var _addForbidRule = function(r, r_)
  {
    fbds[r].push(r_);
  };


  var _addToCover = function(r, c, ir)
  {
    rows[r].forEach(_markAsCovered);
    fbds[r].forEach(_increaseCollisionCount);

    cc[_coverSize] = c, ccr[_coverSize] = ir;
    ++_coverSize;
  };


  var _removeLastFromCover = function(K)
  {
    cc[_coverSize] = -1, ccr[_coverSize] = -1;
    --_coverSize;

    if (_coverSize < K) {
      return;
    }

    var r = cols[cc[_coverSize]][ccr[_coverSize]];

    rows[r].forEach(_markAsUncovered);
    fbds[r].forEach(_decreaseCollisionCount);
  };


  var _increaseCollisionCount = function(r)
  {
    if (xr[r]++ > 0) {
      return;
    }

    rows[r].forEach(function(c) { --xc[c]; });
  };


  var _decreaseCollisionCount = function(r)
  {
    if (--xr[r] > 0) {
      return;
    }

    rows[r].forEach(function(c) { ++xc[c]; });
  };


  var _markAsCovered = function(c)
  {
    xc[c] += R + 1;
  };


  var _markAsUncovered = function(c)
  {
    xc[c] -= R + 1;
  };

}
