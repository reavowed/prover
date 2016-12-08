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

proverApp.component('step', {
  templateUrl: 'template/step.html',
  bindings: {
    step: '<'
  }
});
