# Twothousandfortyeight

This is a primitive console clone of the popular JavaScript game "2048" by Gabriele
Cirulli (http://gabrielecirulli.github.io/2048/).

The keys needed to navigate are the classic WASD keys: W up, S down, A left, D right.

The game works best from ghci because it has set the terminal to uncooked mode, i.e. the
user doesn't need to press enter after having pressed a key. However, by using
Haskeline or similar libraries, the binary can act this way as well.

There is also a very primitive solving helper which can be reached by using the 'h' key.
It simply does that move that has the highest average value (sum of tiles divided by
tiles). It's not really good.

Twothousandfortyeight recognizes if the user has won or lost a game.

![Screenshot of a lost game in Twothousandfortyeight](http://i.imgur.com/m9FH3wY.png)

