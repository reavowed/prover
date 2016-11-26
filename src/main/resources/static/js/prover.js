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
    $http.get('/book').then(function(response) {
      $scope.book = response.data;
    });
  }]
});