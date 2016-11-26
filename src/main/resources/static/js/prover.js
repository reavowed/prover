var proverApp = angular.module('proverApp', ['ngRoute']);

proverApp.config(['$routeProvider', function($routeProvider) {
  $routeProvider
    .when("/", {
      template: "<book></book>"
    })
}]);

proverApp.component('book', {
  templateUrl: 'template/book.html',
  controller: ['$scope', '$http', function($scope, $http) {
    $scope.books = [];
    $scope.newBookTitle = "";
    $scope.chosenTheorem = null;
    $http.get('/book').then(function(response) {
      $scope.book = response.data;
    });
    $scope.setChosenTheorem = function(chosenTheorem) {
      $scope.chosenTheorem = chosenTheorem;
    }
  }]
});

proverApp.component('theorem', {
  templateUrl: 'template/theorem.html',
  bindings: {
    theorem: '='
  }
});

proverApp.component('step', {
  templateUrl: 'template/step.html',
  bindings: {
    step: '<'
  }
});
