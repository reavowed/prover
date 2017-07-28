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
    controller: ['$scope', '$http', '$routeParams', '$sce', function ($scope, $http, $routeParams, $sce) {
      $scope.inference = null;
      $http.get('/books/' + $routeParams.bookKey + '/' + $routeParams.chapterKey + '/' + $routeParams.inferenceKey).then(function (response) {
        $scope.inference = response.data.inference;
        $scope.bookUsages = response.data.bookUsages;
        $scope.previous = response.data.previous;
        if ($scope.previous) $scope.previous.link = getInferenceLink($scope.previous);
        $scope.next = response.data.next;
        if ($scope.next) $scope.next.link = getInferenceLink($scope.next);
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

        $scope.premises = _.map(_.map($scope.inference.premises, formatPremise), $sce.trustAsHtml);
        if ($scope.premises.length) {
          $scope.premiseText = joinWordList($scope.premises);
        }
        $scope.conclusion = $sce.trustAsHtml($scope.inference.conclusion);

        if ($scope.inference.proof) {
          _.forEach($scope.inference.proof.steps, function (step, index) {
            addStep(step, $scope.inference.premises.length + index, 0, 0)
          });
          _.forEach($scope.proofRows, function (row) {
            if (row.prefix) {
              row.prefix = $sce.trustAsHtml(row.prefix);
            }
            if (row.assumption) {
              row.assumption = $sce.trustAsHtml(row.assumption);
            }
            if (row.assertion) {
              row.assertion = $sce.trustAsHtml(row.assertion);
            }
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
            var textToHighlight = $('<div>' + htmlToHighlight + '</div>').text();
            element.mark(textToHighlight, {element: "span", className: "highlightPremise", separateWordSearch: false, acrossElements: true});
          }

          function highlightPremise(index, subindex, htmlToHighlight) {
            if (index < premises.length) {
              markElement(premises.eq(index), htmlToHighlight);
            } else {
              var referredRow = _.findLast(referrableRows, function (row) {
                return row.reference === index;
              });
              if (referredRow == null || (referredRow === rowData && !rowData.assertion)) return;
              var referredRowIndex = _.indexOf($scope.proofRows, referredRow);
              var referredTableRow = allTableRows.eq(referredRowIndex);
              var referredAssumption = referredTableRow.find(".assumption");
              var referredAssertion = referredTableRow.find(".assertion");
              if (referredAssumption.length) {
                markElement(referredAssumption, subindex != null ? null : htmlToHighlight);
              } else {
                markElement(referredAssertion, htmlToHighlight);
              }
              if (subindex != null) {
                var followingRows = _.drop($scope.proofRows, referredRowIndex + 1);
                var nestedRows = _.takeWhile(followingRows, function (row) {
                  return row.conceptualIndentLevel > referredRow.conceptualIndentLevel;
                });
                var childRows = _.filter(nestedRows, function (row) {
                  return row.conceptualIndentLevel === referredRow.conceptualIndentLevel + 1;
                });
                var childRow = _.findLast(childRows, function (row) {
                  return row.reference === subindex;
                });
                if (childRow == null) {
                  if (referredRow.assertion && subindex === index + 1) {
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

          function highlightReferences(references) {
            _.forEach(references, function (reference) {
              highlightReference(reference);
            });
          }

          function highlightReference(reference, html) {
            if (reference.referenceType === "direct") {
              highlightPremise(reference.index, null, html);
            } else if (reference.referenceType === "deduced") {
              highlightPremise(reference.antecedentIndex, reference.consequentIndex, html);
            } else if (reference.referenceType === "simplification") {
              highlightReference(reference.reference, html || reference.statement);
            } else if (reference.referenceType === "elided" || reference.referenceType === "expanded") {
              highlightReferences(reference.references);
            } else {
              throw "Unrecognised reference type " + reference.referenceType;
            }
          }

          highlightReferences(rowData.references);

          tableRow.find(".assertion").addClass("highlightConclusion");
        };

        $scope.removeHighlight = function (rowData, event) {
          $(event.target).closest(".theoremProof").find(".assumption").unmark();
          $(event.target).closest(".theoremProof").find(".assertion").unmark();
          $(event.target).closest(".theoremProof").find(".highlightConclusion").removeClass("highlightConclusion");
          $(".premise").unmark();
        };

        function addAssumption(assumption, steps, reference, visibleIndentLevel, conceptualIndentLevel) {
          $scope.proofRows.push({
            prefix: 'Assume',
            assumption: assumption,
            reference: reference,
            visibleIndentLevel: visibleIndentLevel,
            conceptualIndentLevel: conceptualIndentLevel
          });
          _.forEach(steps, function (step, index) {
            addStep(step, reference + index + 1, visibleIndentLevel + 1, conceptualIndentLevel + 1)
          });
        }

        function addNaming(variable, assumptionStep, assertionStep, reference, visibleIndentLevel, conceptualIndentLevel, override) {
          $scope.proofRows.push({
            prefix: 'Let ' + variable + ' be such that',
            assumption: assumptionStep.assumption,
            inference: getInference(assertionStep),
            references: getReferences(assertionStep),
            reference: reference,
            visibleIndentLevel: visibleIndentLevel,
            conceptualIndentLevel: conceptualIndentLevel
          });
          _.forEach(assumptionStep.steps, function (step, index) {
            var localOverride = index === assumptionStep.steps.length - 1 ?
              (override || {
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
            assertion: assertionStep.statement,
            references: getReferences(assertionStep),
            inference: getInference(assertionStep),
            reference: override ? override.reference : reference,
            visibleIndentLevel: visibleIndentLevel,
            conceptualIndentLevel: override ? override.level : conceptualIndentLevel
          });
        }

        function addRearrangement(rearrangementStep, reference, visibleIndentLevel, conceptualIndentLevel, override) {
          $scope.proofRows.push({
            prefix: 'Then',
            assertion: rearrangementStep.provenStatement,
            references: getReferences(rearrangementStep),
            inference: { name: "Rearrangement" },
            reference: override ? override.reference : reference,
            visibleIndentLevel: visibleIndentLevel,
            conceptualIndentLevel: override ? override.level : conceptualIndentLevel
          });
        }

        function getInference(step) {
          var elidedReference = _.find(step.references, function(r) { return r.referenceType === 'elided'; });
          var inference = elidedReference ? elidedReference.inference : step.inference;
          return {
            name: inference.name,
            link: getInferenceLink(inference)
          }
        }

        function getInferenceLink(inference) {
          return inference.key ? '#/' + inference.bookKey + '/' + inference.chapterKey + '/' + inference.key : null;
        }

        function getReferences(step) {
          var baseReferences = step.references || (step.reference ? [step.reference] : []);
          var innerReferences = _.flatMap(step.references, getReferences);
          return baseReferences.concat(innerReferences);
        }

        function addStep(step, reference, visibleIndentLevel, conceptualIndentLevel, override) {
          if (step.type === "assumption") {
            addAssumption(step.assumption, step.steps, reference, visibleIndentLevel, conceptualIndentLevel, override);
          } else if (step.type === "assertion") {
            addAssertion(step, reference, visibleIndentLevel, conceptualIndentLevel, override);
          } else if (step.type === "naming") {
            addNaming(step.variable, step.assumptionStep, step.assertionStep, reference, visibleIndentLevel, conceptualIndentLevel, override);
          } else if (step.type === "rearrange") {
            addRearrangement(step, reference, visibleIndentLevel, conceptualIndentLevel, override);
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
    controller: ['$scope', '$sce', function ($scope, $sce) {
      this.$onInit = function() {
        $scope.joinWordList = joinWordList;
        if ($scope.$ctrl.inference.premises.length) {
          var formattedPremises = _.map($scope.$ctrl.inference.premises, formatPremise);
          $scope.premiseText = $sce.trustAsHtml(joinWordList(formattedPremises));
        }
        $scope.conclusion = $sce.trustAsHtml($scope.$ctrl.inference.conclusion);
      };
    }]
  });

  proverApp.component('chapterDefinition', {
    templateUrl: 'template/chapterDefinition.html',
    bindings: {
      definition: '<'
    },
    controller: ['$scope', '$sce', function ($scope, $sce) {
      this.$onInit = function() {
        var definition = $scope.$ctrl.definition;
        if ($scope.premises && $scope.premises.length) {
          $scope.premiseText = $sce.trustAsHtml(joinWordList($scope.premises));
        }
        $scope.defaultValue = $sce.trustAsHtml(definition.defaultValue);
        $scope.definingStatement = $sce.trustAsHtml(definition.definingStatement);
      };
    }]
  });
})();

