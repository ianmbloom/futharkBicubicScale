import "hoas"
import "shuffle"
import "bicubic"

let fromTuple (i:i64,j:i64):[2]i64 = [i,j]

entry shuffler (seed:i64)
               (h:i64)
               (w:i64):
               [h][w][2]i64 =
    let rng = mkRng seed
    let (_rng0, shuffled) = shuffleField_2d h w rng
    in  map_2d fromTuple shuffled

entry bicubicInterpolationImage [h][w][n] (newSizeY:i64) (newSizeX:i64)(input:[h][w][n]f32):[newSizeY][newSizeX][n]f32 =
      bicubicInterpolationN newSizeY newSizeX input
