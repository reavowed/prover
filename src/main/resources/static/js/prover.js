(function() {
  var proverApp = angular.module('proverApp', ['ngRoute']);

  proverApp.config(['$locationProvider', function($locationProvider) {
    $locationProvider.hashPrefix('');
  }]);

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
      .when("/:bookKey/:chapterKey/:inferenceKey", {
        template: "<inference></inference>"
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
      $http.get('/books/' + $routeParams.bookKey).then(function (response) {
        $scope.book = response.data;
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
      $scope.joinWordList = joinWordList;
      $http.get('/books/' + $routeParams.bookKey + '/' + $routeParams.chapterKey).then(function (response) {
        $scope.chapter = response.data;
        $scope.$parent.$parent.breadcrumbs = [
          {
            text: 'Books',
            link: '#/'
          },
          {
            text: $scope.chapter.bookTitle,
            link: '#/' + $scope.chapter.bookKey
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

  function joinWordList(list, word) {
    word = word || "and";
    if (list.length > 1) {
      return list.slice(0, list.length - 1).join(", ") +
        " " + word + " " +
        list[list.length - 1];
    } else {
      return list[0];
    }
  }

  proverApp.component('inference', {
    templateUrl: 'template/inference.html',
    controller: ['$scope', '$http', '$routeParams', function ($scope, $http, $routeParams) {
      $scope.inference = null;
      $http.get('/books/' + $routeParams.bookKey + '/' + $routeParams.chapterKey + '/' + $routeParams.inferenceKey).then(function (response) {
        $scope.inference = response.data.inference;
        $scope.bookUsages = response.data.bookUsages;
        $scope.joinWordList = joinWordList;
        $scope.proofRows = [];
        $scope.showSteps = false;
        $scope._ = _;

        $scope.$parent.$parent.breadcrumbs = [
          {
            text: 'Books',
            link: '#/'
          },
          {
            text: $scope.inference.bookTitle,
            link: '#/' + $scope.inference.bookKey
          },
          {
            text: $scope.inference.chapterTitle,
            link: '#/' + $scope.inference.bookKey + '/' + $scope.inference.chapterKey
          },
          {
            text: $scope.inference.name
          }
        ];

        $scope.premises = _.map($scope.inference.premises, formatPremise);
        if ($scope.premises.length) {
          $scope.premiseText = joinWordList($scope.premises);
        }
        if ($scope.inference.proof) {
          _.forEach($scope.inference.proof.steps, function (step, index) {
            addStep(step, $scope.inference.premises.length + index, 0, 0)
          });
        }

        $scope.highlightRow = function (rowData, event) {
          if (!rowData.references) {
            return;
          }
          var tableRow = $(event.target).closest("tr");
          var proofContainer = tableRow.closest(".theoremProof");
          var allTableRows = proofContainer.find("tr");
          var rowIndex = allTableRows.index(tableRow);
          var premises = proofContainer.closest(".inference").find(".premise");

          var referrableRows = $scope.proofRows.slice(0, rowIndex);
          if (rowData.assumption) {
            referrableRows.push(rowData);
          }

          function markElement(element, htmlToHighlight) {
            htmlToHighlight = htmlToHighlight || element.html();
            element.mark(htmlToHighlight, {element: "span", className: "highlightPremise", separateWordSearch: false});
          }

          function highlightPremise(reference, subreference, htmlToHighlight) {
            if (reference < premises.length) {
              markElement(premises.eq(reference), htmlToHighlight);
            } else {
              var referredRow = _.findLast(referrableRows, function (row) {
                return row.reference === reference;
              });
              if (referredRow == null || (referredRow === rowData && !rowData.assertion)) return;
              var referredRowIndex = _.indexOf($scope.proofRows, referredRow);
              var referredTableRow = allTableRows.eq(referredRowIndex);
              var referredAssumption = referredTableRow.find(".assumption");
              var referredAssertion = referredTableRow.find(".assertion");
              if (referredAssumption.length) {
                markElement(referredAssumption, subreference != null ? null : htmlToHighlight);
              } else {
                markElement(referredAssertion, htmlToHighlight);
              }
              if (subreference != null) {
                var followingRows = _.drop($scope.proofRows, referredRowIndex + 1);
                var nestedRows = _.takeWhile(followingRows, function (row) {
                  return row.conceptualIndentLevel > referredRow.conceptualIndentLevel;
                });
                var childRows = _.filter(nestedRows, function (row) {
                  return row.conceptualIndentLevel === referredRow.conceptualIndentLevel + 1;
                });
                var childRow = _.findLast(childRows, function (row) {
                  return row.reference === subreference;
                });
                if (childRow == null) {
                  if (referredRow.assertion && subreference === reference + 1) {
                    childRow = referredRow
                  } else {
                    return
                  }
                }
                var childRowIndex = _.indexOf($scope.proofRows, childRow);
                var childTableRow = allTableRows.eq(childRowIndex);
                markElement(childTableRow.find(".assertion"), htmlToHighlight);
              }
            }
          }

          _.forEach(rowData.references, function (reference) {
            if (reference.index != null) {
              highlightPremise(reference.index, null, reference.html);
            } else {
              highlightPremise(reference.antecedentIndex, reference.consequentIndex, null);
            }
          });
          tableRow.find(".assertion").addClass("highlightConclusion");
        };

        $scope.removeHighlight = function (rowData, event) {
          $(event.target).closest(".theoremProof").find(".assumption").unmark();
          $(event.target).closest(".theoremProof").find(".assertion").unmark();
          $(event.target).closest(".theoremProof").find(".highlightConclusion").removeClass("highlightConclusion");
          $(".premise").unmark();
        };

        $scope.popoverRow = function (rowData, event) {
          if (rowData.conditions) {
            var rowElement = $(event.target).closest('.proofRowStatement');
            var html = "";
            if (rowData.conditions.arbitraryVariables.length) {
              html += "<div>Arbitrary variables: " + joinWordList(rowData.conditions.arbitraryVariables) + "</div>"
            }
            if (rowData.conditions.distinctVariables.length) {
              var text = joinWordList(_.map(rowData.conditions.distinctVariables, function (condition) {
                return "(" + condition[0] + ", " + condition[1] + ")";
              }));
              html += "<div> Distinct variables: " + text + "</div>"
            }
            if (!html.length) {
              html = "No conditions.";
            }
            rowElement.popover({content: html, html: true, placement: 'bottom', container: 'body'}).popover('show');
          }
        };

        function addAssumption(assumption, steps, reference, visibleIndentLevel, conceptualIndentLevel) {
          if (steps.length == 1 && steps[0].provenStatement) {
            $scope.proofRows.push({
              prefix: 'Then',
              assumption: assumption,
              assertion: steps[0].provenStatement.statement,
              references: steps[0].references,
              conditions: steps[0].provenStatement.conditions,
              reference: reference,
              inferenceName: steps[0].inference.name,
              inferenceLink : getLink(steps[0].inference),
              visibleIndentLevel: visibleIndentLevel,
              conceptualIndentLevel: conceptualIndentLevel
            });
          } else {
            $scope.proofRows.push({
              prefix: 'Assume',
              assumption: assumption,
              reference: reference,
              visibleIndentLevel: visibleIndentLevel,
              conceptualIndentLevel: conceptualIndentLevel,
            });
            _.forEach(steps, function (step, index) {
              addStep(step, reference + index + 1, visibleIndentLevel + 1, conceptualIndentLevel + 1)
            });
          }
        }

        function addNaming(variable, assumptionStep, assertionStep, reference, visibleIndentLevel, conceptualIndentLevel, override) {
          $scope.proofRows.push({
            prefix: 'Let ' + variable + ' be such that',
            assumption: assumptionStep.assumption,
            inferenceName: assertionStep.inference.name,
            inferenceLink : getLink(assertionStep.inference),
            references: assertionStep.references,
            reference: reference,
            visibleIndentLevel: visibleIndentLevel,
            conceptualIndentLevel: conceptualIndentLevel
          });
          _.forEach(assumptionStep.steps, function (step, index) {
            var localOverride = index === assumptionStep.steps.length - 1 ?
              (override || {
                conditions: assertionStep.provenStatement.conditions,
                level: conceptualIndentLevel,
                reference: reference
              }) :
              null;
            addStep(step, reference + index + 1, visibleIndentLevel, conceptualIndentLevel + 1, localOverride)
          });
        }

        function addAssertion(assertionStep, reference, visibleIndentLevel, conceptualIndentLevel, override) {
          $scope.proofRows.push({
            prefix: 'Then',
            assertion: assertionStep.provenStatement.statement,
            references: assertionStep.references,
            conditions: override ? override.conditions : assertionStep.provenStatement.conditions,
            inferenceName: assertionStep.inference.name,
            inferenceLink : getLink(assertionStep.inference),
            reference: override ? override.reference : reference,
            visibleIndentLevel: visibleIndentLevel,
            conceptualIndentLevel: override ? override.level : conceptualIndentLevel
          });
        }

        function getLink(inference) {
          return inference.key ?
            '#/' + inference.bookKey + '/' + inference.chapterKey + '/' + inference.key :
            null
        }

        function addStep(step, reference, visibleIndentLevel, conceptualIndentLevel, override) {
          if (step.assumption) {
            addAssumption(step.assumption, step.steps, reference, visibleIndentLevel, conceptualIndentLevel, override);
          } else if (step.variable) {
            addNaming(step.variable, step.assumptionStep, step.assertionStep, reference, visibleIndentLevel, conceptualIndentLevel, override)
          } else {
            addAssertion(step, reference, visibleIndentLevel, conceptualIndentLevel, override);
          }
        }

      });
    }]
  });

  proverApp.component('chapterInference', {
    templateUrl: 'template/chapterInference.html',
    bindings: {
      inference: '<'
    },
    controller: ['$scope', function ($scope) {
      this.$onInit = function() {
        $scope.joinWordList = joinWordList;
        $scope.premises = _.map($scope.$ctrl.inference.premises, formatPremise);
        if ($scope.premises.length) {
          $scope.premiseText = joinWordList($scope.premises);
        }
      };
    }]
  });

  proverApp.component('arbitraryVariables', {
    templateUrl: 'template/arbitraryVariables.html',
    bindings: {
      arbitraryVariables: '<'
    },
    controller: ['$scope', function ($scope) {
      this.$onInit = function() {
        $scope.text = joinWordList($scope.$ctrl.arbitraryVariables);
      }
    }]
  });
  proverApp.component('distinctVariables', {
    templateUrl: 'template/distinctVariables.html',
    bindings: {
      distinctVariables: '<'
    },
    controller: ['$scope', function ($scope) {
      this.$onInit = function() {
        $scope.text = joinWordList(_.map($scope.$ctrl.distinctVariables, function (condition) {
          return "(" + condition[0] + ", " + condition[1] + ")";
        }));
      }
    }]
  });
})();

