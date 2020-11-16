# Constraint Game

In this repository I am experimenting with the development of high level
declarative GUI applications using a combination of `gi-gtk-declarative` and
`diagrams`.

Diagrams can be rendered to Cairo with `diagrams-cairo` and that can be
rendered to a `gi-gtk` widget [as described here][1]. That rendering function
can then be run inside the draw handler of a custom widget defined in
`gi-gtk-declarative`.

Eventually, I would like to make the DrawingArea widget a standard
`gi-gtk-declarative` widget. And I would also strongly support a good gi-cairo
implementation that actually supports rendering [as discussed in this issue
thread][2]. I think [cohomology's work on gi-cairo-render][3] is a good start,
but I'm not sure if its production ready. I would also like to research a way to
create automatic bindings for C libraries without special gobject introspection
metadata.

I believe this combination of diagrams and gi-gtk-declarative can be turned into
a [gloss][4] replacement that is a leap forward in functionality and that could
actually be used for real graphical applications.

## Sudoku

To show off this approach to designing GUI applications I have chosen the sudoku
game. The secondary goal of this project is to implement a sudoku GUI
application for which you can write your own solving techniques. The challenge
is to find rules that do not use backtracking. I'm planning to implement many of
[the techniques that are described on the sudopedia][5].

Currently, you can place numbers by clicking in the boxes and you can change the
number you place by pressing the desired digit on your keyboard. To switch to 
an erasing mode you can press the zero key.

[1]: https://github.com/haskell-gi/haskell-gi/wiki/Using-Cairo-with-haskell-gi-generated-bindings
[2]: https://github.com/haskell-gi/haskell-gi/issues/148
[3]: https://github.com/cohomology/gi-cairo-render
[4]: http://gloss.ouroborus.net/
[5]: http://sudopedia.enjoysudoku.com/Solving_Technique.html
