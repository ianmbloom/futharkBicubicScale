module Scalar = f32

let cubicPolate (v:[4]Scalar.t) (frac:Scalar.t) =
    let a = (v[3]-v[2])-(v[0]-v[1])
    let b = (v[0]-v[1])-a
    let c = v[2]-v[0]
    let d = v[1]
    in  d + frac * (c + frac * (b + frac * a))

-- Helper functions for bicubicInterpolation function:

let clip_f32 (input:Scalar.t) (lowerbound:Scalar.t) (upperbound:Scalar.t):Scalar.t =
    f32.min (f32.max input lowerbound) upperbound

let bicubicPolate (ndata:[16]Scalar.t) (fracy:Scalar.t) (fracx:Scalar.t) =
    let ns = unflatten 4 4 ndata
    let xs = tabulate 4 (\i -> cubicPolate ns[i] fracx)
    in  clip_f32 (cubicPolate xs fracy) 0 1

let clip (input:i64) (lowerbound:i64) (upperbound:i64):i64 =
    i64.min (i64.max input lowerbound) upperbound

let bicubicInterpolation [h][w] (newSizeY:i64) (newSizeX:i64) (input:[h][w]Scalar.t):[newSizeY][newSizeX]Scalar.t =
    let ratiox = Scalar.i64 w / Scalar.i64 newSizeX
    let ratioy = Scalar.i64 h / Scalar.i64 newSizeY
    let ndata =
        tabulate_3d newSizeY newSizeX 16 (\y x i ->
            let yMappingToOrigin = Scalar.i64 y * ratioy
            let xMappingToOrigin = Scalar.i64 x * ratiox
            let yMappingToOriginFloor = Scalar.floor yMappingToOrigin
            let xMappingToOriginFloor = Scalar.floor xMappingToOrigin
            let ndatay = i / 4
            let ndatax = i % 4
            in  input[ clip (i64.f32 yMappingToOriginFloor + (ndatay-1)) 0 (h-1)
                     , clip (i64.f32 xMappingToOriginFloor + (ndatax-1)) 0 (w-1)
                     ]
        )
    in  tabulate_2d newSizeY newSizeX (\y x ->
            let yMappingToOrigin = Scalar.i64 y * ratioy
            let xMappingToOrigin = Scalar.i64 x * ratiox
            let yMappingToOriginFloor = Scalar.floor yMappingToOrigin
            let xMappingToOriginFloor = Scalar.floor xMappingToOrigin
            let yMappingToOriginFrac  = yMappingToOrigin - yMappingToOriginFloor
            let xMappingToOriginFrac  = xMappingToOrigin - xMappingToOriginFloor
            in  bicubicPolate ndata[y,x] yMappingToOriginFrac xMappingToOriginFrac
        )

let layer 't [h][w][n] (i:i64) (input:[h][w][n]t):[h][w]t =
    tabulate_2d h w (\row col -> input[row,col,i])

let bicubicInterpolationN [h][w][n] (newSizeY:i64) (newSizeX:i64) (input:[h][w][n]Scalar.t):[newSizeY][newSizeX][n]Scalar.t =
    let layers = tabulate n (\i -> bicubicInterpolation newSizeY newSizeX (layer i input))
    in  tabulate_3d newSizeY newSizeX n (\row col i -> layers[i,row,col])

let subdivideImageN [h][w][n] (input:[h][w][n]Scalar.t):[][][n]Scalar.t =
    bicubicInterpolationN (h*2) (w*2) input
