var proverApp = angular.module('proverApp', ['ngRoute']);

proverApp.config(['$routeProvider', function($routeProvider) {
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
  controller: ['$scope', '$http', function($scope, $http) {
    $scope.books = null;
    $http.get('/books').then(function(response) {
      $scope.$parent.$parent.breadcrumbs = [];
      $scope.books = response.data;
    });
  }]
});

proverApp.component('book', {
  templateUrl: 'template/book.html',
  controller: ['$scope', '$http', '$routeParams', function($scope, $http, $routeParams) {
    $scope.book = null;
    $http.get('/books').then(function(response) {
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
  controller: ['$scope', '$http', '$routeParams', function($scope, $http, $routeParams) {
    $scope.chapter = null;
    $http.get('/books').then(function(response) {
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

proverApp.controller('NavController', function($scope) {
  $scope.breadcrumbs = [];
});

proverApp.component('theorem', {
  templateUrl: 'template/theorem.html',
  bindings: {
    theorem: '<'
  },
  controller: ['$scope', function($scope) {
    $scope.proofRows = [];
    $scope._ = _;
    function addRow(prefix, statement, deductionId, reference, indentLevel) {
      $scope.proofRows.push({
        prefix: prefix,
        statement: statement,
        deductionId: deductionId,
        reference: reference,
        indentLevel: indentLevel
      });
    }
    function addFantasy(fantasy, outerReference, indentLevel) {
      addRow('Assume', fantasy.assumption, "assumption", outerReference + "f.a", indentLevel);
      _.forEach(fantasy.steps, function(step, index) { addStep(step, outerReference + "f.", index, indentLevel + 1) });

    }
    function addStep(step, outerReference, index, indentLevel) {
      if (step.fantasy) {
        addFantasy(step.fantasy, outerReference, indentLevel);
      }
      addRow(step.fantasy ? 'So' : 'Then', step.statement, step.deductionId, outerReference + (index + 1), indentLevel);
    }
    _.forEach($scope.$ctrl.theorem.steps, function(step, index) { addStep(step, "", index, 0) });
  }]
});
