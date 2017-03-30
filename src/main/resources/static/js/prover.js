(function() {
  var proverApp = angular.module('proverApp', ['ngRoute']);

  proverApp.config(['$routeProvider', function ($routeProvider) {
    $routeProvider
      .when("/", {
        template: "<books></books>"
      })
      .when("/:bookKey", {
        template: "<book></book>"
      })
      .when("/:bookKey/:chapterKey", {
        template: "<chapter></chapter>"
      })
  }]);

  proverApp.component('books', {
    templateUrl: 'template/books.html',
    controller: ['$scope', '$http', function ($scope, $http) {
      $scope.books = null;
      $http.get('/books').then(function (response) {
        $scope.$parent.$parent.breadcrumbs = [];
        $scope.books = response.data;
      });
    }]
  });

  proverApp.component('book', {
    templateUrl: 'template/book.html',
    controller: ['$scope', '$http', '$routeParams', function ($scope, $http, $routeParams) {
      $scope.book = null;
      $http.get('/books').then(function (response) {
        var books = response.data;
        $scope.book = _.find(books, ['key', $routeParams.bookKey]);
        $scope.$parent.$parent.breadcrumbs = [
          {
            text: 'Books',
            link: '#/'
          },
          {
            text: $scope.book.title
          }
        ];
      });
    }]
  });

  proverApp.component('chapter', {
    templateUrl: 'template/chapter.html',
    controller: ['$scope', '$http', '$routeParams', function ($scope, $http, $routeParams) {
      $scope.chapter = null;
      $http.get('/books').then(function (response) {
        var books = response.data;
        var book = _.find(books, ['key', $routeParams.bookKey]);
        $scope.chapter = _.find(book.chapters, ["key", $routeParams.chapterKey]);
        $scope.$parent.$parent.breadcrumbs = [
          {
            text: 'Books',
            link: '#/'
          },
          {
            text: book.title,
            link: '#/' + book.key
          },
          {
            text: $scope.chapter.title
          }
        ];
      });
    }]
  });

  proverApp.controller('NavController', function ($scope) {
    $scope.breadcrumbs = [];
  });

  function formatPremise(premise) {
    if (premise.antecedent) {
      return premise.antecedent + " ⊢ " + premise.consequent;
    } else {
      return premise.statement;
    }
  }

  function joinWordList(list) {
    if (list.length > 1) {
      return list.slice(0, list.length - 1).join(", ") +
        " and " +
        list[list.length - 1];
    } else {
      return list[0];
    }
  }

  proverApp.component('theorem', {
    templateUrl: 'template/theorem.html',
    bindings: {
      theorem: '<'
    },
    controller: ['$scope', function ($scope) {
      var theorem = $scope.$ctrl.theorem;
      $scope.joinWordList = joinWordList;
      $scope.proofRows = [];
      $scope.showSteps = false;
      $scope._ = _;

      $scope.premises = _.map(theorem.premises, formatPremise);
      if ($scope.premises.length) {
        $scope.premiseText = joinWordList($scope.premises);
      }

      $scope.highlightRow = function(rowData, event) {
        if (!rowData.assertion) {
          return;
        }
        var tableRow = $(event.target).closest("tr");
        var proofContainer = tableRow.closest(".theoremProof");
        var allTableRows = proofContainer.find("tr");
        var rowIndex = allTableRows.index(tableRow);
        var premises = proofContainer.find(".premise");

        var previousRows = $scope.proofRows.slice(0, rowIndex);
        var referrableRows = _.filter(previousRows, function(row) {
          return row.indentLevel <= rowData.indentLevel;
        });
        if (rowData.assumption) {
          referrableRows.push(rowData);
        }

        function highlightPremise(reference, subreference) {
          if (reference < premises.length) {
            premises.eq(reference).addClass("highlightPremise");
          } else {
            var referredRow = referrableRows[reference - premises.length];
            var referredRowIndex = _.indexOf($scope.proofRows, referredRow);
            var referredTableRow = allTableRows.eq(referredRowIndex);
            var referredAssumption = referredTableRow.find(".assumption");
            var referredAssertion = referredTableRow.find(".assertion");
            if (referredAssumption.length) {
              referredAssumption.addClass("highlightPremise");
            } else {
              referredAssertion.addClass("highlightPremise");
            }
            if (subreference != null) {
              var followingRows = _.drop($scope.proofRows, referredRowIndex + 1);
              var nestedRows = _.takeWhile(followingRows, function (row) {
                return row.indentLevel > referredRow.indentLevel;
              });
              var childRows = _.filter(nestedRows, function (row) {
                return row.indentLevel == referredRow.indentLevel + 1;
              });
              if (referredRow.assertion) {
                childRows.unshift(referredRow);
              }
              var childRow = childRows[subreference];
              var childRowIndex = _.indexOf($scope.proofRows, childRow);
              var childTableRow = allTableRows.eq(childRowIndex);
              childTableRow.find(".assertion").addClass("highlightPremise");
            }
          }
        }

        _.forEach(rowData.assertion.references, function(reference) {
          if (reference.index != null) {
            highlightPremise(reference.index);
          } else {
            highlightPremise(reference.antecedentIndex, reference.consequentIndex);
          }
        });
        tableRow.find(".assertion").addClass("highlightConclusion");
      };

      $scope.removeHighlight = function(rowData, event) {
        $(event.target).closest(".theoremProof").find(".highlightPremise").removeClass("highlightPremise");
        $(event.target).closest(".theoremProof").find(".highlightConclusion").removeClass("highlightConclusion");
      };

      function addAssumption(assumption, steps, indentLevel) {
        if (steps.length == 1 && steps[0].provenStatement) {
          $scope.proofRows.push({
            prefix: 'Then',
            assumption: assumption,
            assertion: steps[0],
            indentLevel: indentLevel
          });
        } else {
          $scope.proofRows.push({
            prefix: 'Assume',
            assumption: assumption,
            indentLevel: indentLevel
          });
          _.forEach(steps, function (step) {
            addStep(step, indentLevel + 1)
          });
        }
      }

      function addAssertion(assertion, indentLevel) {
        $scope.proofRows.push({
          prefix: 'Then',
          assertion: assertion,
          indentLevel: indentLevel
        });
      }

      function addStep(step, indentLevel) {
        if (step.assumption) {
          addAssumption(step.assumption, step.steps, indentLevel);
        } else {
          addAssertion(step, indentLevel);
        }
      }

      _.forEach(theorem.proof.steps, function (step) {
        addStep(step, 0)
      });
    }]
  });

  proverApp.component('axiom', {
    templateUrl: 'template/axiom.html',
    bindings: {
      axiom: '<'
    },
    controller: ['$scope', function ($scope) {
      var axiom = $scope.$ctrl.axiom;
      $scope.joinWordList = joinWordList;
      $scope.premises = _.map(axiom.premises, formatPremise);
      if ($scope.premises.length) {
        $scope.premiseText = joinWordList($scope.premises);
      }
    }]
  });
})();

