var destringify = function(arr){

    for (var i=1; i<arr.length; i++){

        for (var j=1; j<arr[i].length; j++){

            arr[i][j] = parseFloat(arr[i][j],10);

            console.log(arr[i][j]);
        }
  }

  return arr;
}
console.log(destringify([["smth","smthh","smthhh","smthhhh"], ["smth","2000","3000","4000"],["smth","2555","2444","3666"]]));