'use strict';


function Sudoku(n, str)
{
  var n2 = n * n;
  var n3 = n * n2;
  var n4 = n * n3;

  var board = [].initialize(n4, function(p) {
    var v;
    if (str !== undefined && !isNaN(v = parseInt(str.charAt(p), 10))) {
      return [v - 1];
    }
    return [].initialize(n2, function(v) { return v; });
  });


  var pos2row = function(p) { return ~~(p / n2); };
  var pos2col = function(p) { return p % n2; };
  var pos2box = function(p) { return ~~(p / n3) * n + ~~((p % n2) / n); };


  this.set = function(x, p)
  {
    board.splice(p, 1, typeof x === 'number' ? [x] : x);
  };


  this.toString = function()
  {
    var res = '';

    board.forEach(function(pos) {
      res += typeof pos.length === 1 ? pos[0] + 1 : '.';
    });

    return res;
  };


  this.serialize = function()
  {
    return { n: n, board: board };
  };


  this.toAdmissibleCoverProblem = function()
  {
    var rows = [];

    for (var p = 0; p < n4; ++p) {
      for (var v = 0; v < n2; ++v) {
        var row = [];

        row.push(0 * n4 + p);
        row.push(1 * n4 + pos2row(p) * n2 + v);
        row.push(2 * n4 + pos2col(p) * n2 + v);
        row.push(3 * n4 + pos2box(p) * n2 + v);

        rows.push(row);
      }
    }

    var P = new AdmissibleCoverProblem(n4 + 3 * n2 * n2, rows, n4);

    P.forceExactness();

    board.forEach(function(pos, p) {
      var pos_l = pos.length, i = 0;
      for (var v = 0; v < n2; ++v) {
        if (i < pos_l && pos[i] === v) {
          ++i;
        } else {
          P.markAsForbidden(p * n2 + v);
        }
      }
    });

    return P;
  };


  this.printSolution = function(P)
  {
    var res = [];

    P.startIterationOverChosenSubsets();
    for (var r; (r = P.nextChosenSubset()) >= 0; ) {
      res[~~(r / n2)] = r % n2 + 1;
    }

    document.write(res.join('') + '<br />');
  };


  this.appendSerializedSolution = function(list)
  {
    return function(P)
    {
      var res = [];

      P.startIterationOverChosenSubsets();
      for (var r; (r = P.nextChosenSubset()) >= 0; ) {
        res[~~(r / n2)] = [r % n2];
      }

      list.push({ n: n, board: res });
    };
  };

}
