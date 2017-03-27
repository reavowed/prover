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

  function joinPremises(formattedPremises) {
    if (formattedPremises.length > 1) {
      return formattedPremises.slice(0, formattedPremises.length - 1).join(", ") +
        " and " +
        formattedPremises[formattedPremises.length - 1];
    } else {
      return formattedPremises[0];
    }
  }

  proverApp.component('theorem', {
    templateUrl: 'template/theorem.html',
    bindings: {
      theorem: '<'
    },
    controller: ['$scope', function ($scope) {
      var theorem = $scope.$ctrl.theorem;
      $scope.proofRows = [];
      $scope.showSteps = false;
      $scope._ = _;

      $scope.premises = _.map(theorem.premises, formatPremise);
      if ($scope.premises.length) {
        $scope.premiseText = joinPremises($scope.premises);
      }

      function addAssumption(assumption, steps, indentLevel) {
        if (steps.length == 1 && steps[0].provenStatement) {
          $scope.proofRows.push({
            prefix: 'Then',
            statement: assumption + " ⊢ " + steps[0].provenStatement.statement,
            inferenceName: steps[0].inference.name,
            indentLevel: indentLevel
          });
        } else {
          $scope.proofRows.push({
            prefix: 'Assume',
            statement: assumption,
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
          statement: assertion.provenStatement.statement,
          inferenceName: assertion.inference.name,
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
      $scope.premises = _.map(axiom.premises, formatPremise);
      if ($scope.premises.length) {
        $scope.premiseText = joinPremises($scope.premises);
      }
    }]
  });
})();
