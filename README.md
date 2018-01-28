<!DOCTYPE html>
<html lang="en-us">
  <head>
    <meta charSet="utf-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <title>Elm clone of 2048</title>
    <meta name="description" content="Elm implementation of game 2048"/>
    <meta name="author" content="Asparuh Kostadinov"/>
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.3/jquery.min.js"></script>
  </head>
  <body>
    <div id="main"></div>
    <div class="game-info_foot">
      <p class="game-explanation">
        <strong class="important">How to play:</strong> Use your <strong>arrow keys</strong> to move the tiles. When two tiles with the same number touch, they <strong>merge into one!</strong>
      </p>
      <hr>
      <p>
        This is a clone of <a href="http://gabrielecirulli.com" target="_blank">Gabriele Cirulli's</a> game <a href="https://gabrielecirulli.github.io/2048/">2048</a>
      </p>
    </div>
    <script src="main.js"></script>
    <script>
      var node = document.getElementById('main');
      var app = Elm.Main.embed(node);

      var ar=new Array(37,38,39,40);

      $(document).keydown(function(e) {
           var key = e.which;
            if($.inArray(key,ar) > -1) {
                e.preventDefault();
                return false;
            }
            return true;
      });
  </script>
  </body>
</html>
