module Day21.Input exposing (rawInput)


rawInput : String
rawInput =
    """../.. => ###/###/.##
#./.. => ..#/###/##.
##/.. => ..#/##./##.
.#/#. => #../.#./.##
##/#. => #.#/###/.#.
##/## => ##./.../.#.
.../.../... => ...#/.#../#.#./##.#
#../.../... => .#.#/.#../####/###.
.#./.../... => #.##/#.##/.###/##.#
##./.../... => ..##/#.##/.##./..##
#.#/.../... => .#.#/#.#./#..#/...#
###/.../... => #.../.##./.#../.###
.#./#../... => ##.#/...#/##.#/.##.
##./#../... => #.#./###./...#/#.##
..#/#../... => ..##/.###/..../.##.
#.#/#../... => ...#/#..#/#.#./#.#.
.##/#../... => ...#/#.##/..##/.###
###/#../... => .##./..##/##../##.#
.../.#./... => ####/.##./##.#/####
#../.#./... => ..../.##./#..#/##.#
.#./.#./... => ..../#.##/#.../..#.
##./.#./... => .###/.#.#/...#/....
#.#/.#./... => ..##/.#../.###/#.##
###/.#./... => ..../..##/##.#/###.
.#./##./... => .###/.#.#/#..#/#.#.
##./##./... => #..#/#..#/#.##/.##.
..#/##./... => #.##/...#/..#./.##.
#.#/##./... => ..##/#.../..../...#
.##/##./... => ##.#/...#/..##/#..#
###/##./... => ..##/..#./.###/..##
.../#.#/... => .###/..##/.#.#/..##
#../#.#/... => ..##/...#/##../..#.
.#./#.#/... => ..##/##.#/#..#/###.
##./#.#/... => #.../####/..#./#...
#.#/#.#/... => ..../##.#/.##./#..#
###/#.#/... => ..##/#.#./.#.#/.#..
.../###/... => ..##/.#../.#.#/#..#
#../###/... => #.#./.#../.##./....
.#./###/... => ##.#/...#/###./#.##
##./###/... => ..../#.../.###/#.#.
#.#/###/... => ####/..../...#/....
###/###/... => ##.#/##../#.##/#...
..#/.../#.. => ##.#/..#./#.##/..#.
#.#/.../#.. => .#../...#/..#./.##.
.##/.../#.. => ...#/#.../#..#/#..#
###/.../#.. => .###/##../.##./.#..
.##/#../#.. => ..##/#.##/.#.#/...#
###/#../#.. => ...#/.###/..../#..#
..#/.#./#.. => #..#/..../..#./..##
#.#/.#./#.. => #..#/..../#.#./.###
.##/.#./#.. => ..../.##./..##/.#.#
###/.#./#.. => ##.#/###./##.#/..##
.##/##./#.. => #.#./..../###./####
###/##./#.. => #..#/#.##/#.##/#...
#../..#/#.. => ##../#..#/#.../###.
.#./..#/#.. => #.#./.#.#/..../.#.#
##./..#/#.. => #.#./#.../#.#./#..#
#.#/..#/#.. => ..##/.#.#/.#../.###
.##/..#/#.. => ##.#/..##/..../.###
###/..#/#.. => ..#./.##./...#/.#.#
#../#.#/#.. => #.../.#../#.#./##..
.#./#.#/#.. => ..../..../##../#...
##./#.#/#.. => ..#./..../#.../..#.
..#/#.#/#.. => #.#./.#.#/.#../#.##
#.#/#.#/#.. => ...#/##.#/.##./#...
.##/#.#/#.. => ..#./...#/.##./#...
###/#.#/#.. => ..##/#..#/..../..##
#../.##/#.. => ##.#/##.#/#.##/.#.#
.#./.##/#.. => ..##/##../#.#./####
##./.##/#.. => #.#./..../..##/#.##
#.#/.##/#.. => ..#./###./##.#/##.#
.##/.##/#.. => #..#/...#/..##/....
###/.##/#.. => ..##/##../##.#/#.##
#../###/#.. => ####/###./.###/....
.#./###/#.. => ...#/.##./...#/#.##
##./###/#.. => ...#/...#/##.#/.##.
..#/###/#.. => ..##/.##./#.#./...#
#.#/###/#.. => .###/.##./.###/.#.#
.##/###/#.. => ##../.#../#.#./##.#
###/###/#.. => ..../..../.###/##..
.#./#.#/.#. => ##.#/##.#/..##/.##.
##./#.#/.#. => .#../#.##/#.##/#.#.
#.#/#.#/.#. => ..##/#.#./#.../..##
###/#.#/.#. => ##.#/.#.#/##.#/.###
.#./###/.#. => #.#./..#./..##/.##.
##./###/.#. => ...#/#.##/###./#.##
#.#/###/.#. => ...#/.###/#.#./#.#.
###/###/.#. => .#.#/#..#/####/#...
#.#/..#/##. => #.##/#.#./##../####
###/..#/##. => ##.#/...#/..../####
.##/#.#/##. => #.../#..#/..##/....
###/#.#/##. => ##../###./...#/####
#.#/.##/##. => ##.#/..##/..../#...
###/.##/##. => ..#./####/..../#...
.##/###/##. => ..##/#.##/..#./####
###/###/##. => #.##/...#/..../..#.
#.#/.../#.# => ..#./#.##/#..#/#.#.
###/.../#.# => ..#./###./..##/#...
###/#../#.# => .###/#..#/##../.#..
#.#/.#./#.# => ###./##.#/.#../#..#
###/.#./#.# => ##.#/###./#.../...#
###/##./#.# => ####/##../#.../....
#.#/#.#/#.# => ..#./..##/..#./...#
###/#.#/#.# => ...#/##.#/##.#/#.##
#.#/###/#.# => ..#./####/.#../##.#
###/###/#.# => ..../.#.#/..../...#
###/#.#/### => #.#./..##/##.#/....
###/###/### => ..#./#.##/####/###."""