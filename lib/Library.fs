module OpenSCAD.Fs.Lib 

open System
open System.Text
open System.IO
open FSharpx.Collections

[<Measure>]
type deg

[<Measure>]
type rad

let degToRad (degree : float<deg>) : float<rad> = degree * Math.PI / 180.0<deg/rad>
let radToDeg (radians : float<rad>) : float<deg> = radians * 180.0<deg/rad> / Math.PI
let sin rad = Math.Sin (rad / 1.0<rad>)
let cos rad = Math.Cos (rad / 1.0<rad>)

type Cube = {
    Vector : PersistentVector<float>
    Centered : bool
}

type Sphere = {
    Radius : float
    FA : float
    FS : float
    FN : float
}

type Cylinder = {
    Height : float
    BottomRadius : float
    TopRadius : float
    Centered : bool
    FA : float
    FS : float
    FN : float
}

type Polyhedron = {
    Points : PersistentVector<float> list
    Faces : float list list
    Convexity : int
}

type Object =
    | Cube of Cube
    | Sphere of Sphere
    | Cylinder of Cylinder
    | Polyhedron of Polyhedron

type Translate = {
    Vector : PersistentVector<float>
    Children : Element list
}

and Scale = {
    ScaleFactor : PersistentVector<float>
    Children : Element list
}

and Resize = {
    NewSize : PersistentVector<float>
    Auto : bool
    Children : Element list
}

and Rotate = {
    Degrees : float<deg>
    Axis : PersistentVector<float>
    Children : Element list
}

and Mirror = {
    Axis : PersistentVector<float>
    Children : Element list
}

and Minkowski = {
    Children : Element list
}

and Hull = {
    Children : Element list
}

and Color = {
    Red : float
    Green : float
    Blue : float
    Alpha : float
    Children : Element list
}

and Operator =
    | Translate of Translate
    | Scale of Scale
    | Resize of Resize
    | Rotate of Rotate
    | Mirror of Mirror
    | Minkowski of Minkowski
    | Hull of Hull
    | Color of Color

and Union = {
    Children : Element list
}

and Difference = {
    Children : Element list
}

and Intersection = {
    Children : Element list
}

and Combinator =
    | Union of Union
    | Difference of Difference
    | Intersection of Intersection

and Projection = {
    Cut : bool
    Children : Element list
}

and Linear = {
    Height : float
    Center : bool
    Convexity : int
    Twist : float
    Slices : int
    Scale : float
    FN : float
    Children : Element list
}

and Extrusion =
    | Linear of Linear
    | Rotational

and Element =
    | Object of Object
    | Operator of Operator
    | Combinator of Combinator
    | Projection of Projection
    | Extrusion of Extrusion

type Program = {
    Elements : Element list
}

let extractChildren children =
    match children with
    | [x] -> x
    | x -> x |> List.collect id

module Cube =
    let create() = 
        {Cube.Vector = PersistentVector.ofSeq [1.0; 1.0; 1.0]; Centered = false}

    let resizeByFloat size cube =
        {cube with Cube.Vector = PersistentVector.ofSeq [size; size; size]}

    let resizeByVector vector cube = 
        {cube with Cube.Vector = PersistentVector.ofSeq vector} 

    let center cube =
        {cube with Cube.Centered = true} 

    let toSingleObject = Cube >> Object

    let toObject cube  = cube |> Cube |> Object |> List.singleton

module Sphere =
    let create() =
        {Radius = 1.0; FA = 12.0; FS = 2.0; FN = 0.0} 

    let resize(radius : float) sphere = 
        {sphere with Radius = radius} 

    let fragmentAngle(degree : float) sphere = 
        {sphere with Sphere.FA = degree}

    let fragmentSize(size : float) sphere = 
        {sphere with Sphere.FS = size}

    let resolution(resolution : float) sphere = 
        {sphere with Sphere.FN = resolution}

    let toObject sphere = sphere |> Sphere |> Object |> List.singleton

module Cylinder =
    let create() =
        {Height = 1.0; BottomRadius = 1.0; TopRadius = 1.0; Centered = false; FA = 12.0; FS = 2.0; FN = 0.0}

    let center cylinder =
        {cylinder with Cylinder.Centered = true} 

    let resize(height : float) cylinder =
        {cylinder with Cylinder.Height = height} 

    let radius(radius : float) cylinder =
        {cylinder with BottomRadius = radius; TopRadius = radius} 

    let bottomRadius(radius : float) cylinder =
        {cylinder with BottomRadius = radius} 

    let topRadius(radius : float) cylinder =
        {cylinder with TopRadius = radius} 

    let fragmentAngle(degree : float) cylinder =
        {cylinder with Cylinder.FA = degree} 

    let fragmentSize(size : float) cylinder =
        {cylinder with Cylinder.FS = size}

    let fragmentNumber(number : float) cylinder =
        {cylinder with Cylinder.FN = number} 

    let toSingleObject = Cylinder >> Object 

    let toObject cylinder = cylinder |> Cylinder |> Object |> List.singleton

module Polyhedron =
    let create() =
        {Points = []; Faces = []; Convexity = 1} 

    let addPoint point polyhedron = 
        {polyhedron with Points = polyhedron.Points |> List.append [point]} 

    let addFace face polyhedron = 
        {polyhedron with Faces = polyhedron.Faces |> List.append [face]} 

    let toObject = Polyhedron >> Object |> List.singleton

module Operator = 
    let translate vector children = 
        [{Vector = PersistentVector.ofSeq vector; Children = children} |> Translate |> Operator]

    let scale vector children =
        [{ScaleFactor = PersistentVector.ofSeq vector; Children = children} |> Scale |> Operator]

    let resize vector auto children =
        [{NewSize = PersistentVector.ofSeq vector; Auto = auto; Children = children} |> Resize |> Operator]

    let rotate degrees axis children =
        [{Degrees = degrees; Axis = PersistentVector.ofSeq axis; Children = children} |> Rotate |> Operator]

    let mirror axis children =
        [{Axis = PersistentVector.ofSeq axis; Children = children} |> Mirror |> Operator]

    let minkowski children =
        [{ Minkowski.Children = children} |> Minkowski |> Operator]

    let hull children =
        [{ Hull.Children = children} |> Hull |> Operator]

    let color red green blue alpha children =
        [{Red = red; Green = green; Blue = blue; Alpha = alpha; Children = children} |> Color |> Operator]

module Combinator =
    let union children =
        [{Union.Children = children} |> Union |> Combinator]

    let difference children =
        [{Difference.Children = children} |> Difference |> Combinator]

    let intersection children =
        [{Intersection.Children = children} |> Intersection |> Combinator]

module Projection =
    let project children =
        [{Projection.Children = children; Cut = false} |> Projection] 

    let projectCut children =
        [{Projection.Children = children; Cut = true} |> Projection] 

module Extrusion =
    let linear height twist convexity children =
        [{Height = height; Center = true; Convexity = convexity; Twist = twist; Slices = 20; Scale = 1.0; FN = 16.0; Children = children} |> Linear |> Extrusion]

let print writer elements =

    let rec printInner writer tabcount elem =
        let tabs = String(' ', tabcount)
        match elem with
        | [] -> ()
        | h :: t ->
            match h with
            | Object o ->
                match o with
                | Cube c -> 
                    let v = c.Vector
                    fprintfn writer "%scube([%.2f, %.2f, %.2f], center = %b);" tabs v.[0] v.[1] v.[2] c.Centered
                | Sphere s ->
                    fprintfn writer "%ssphere($fn = %.2f, $fa = %.2f, $fs = %.2f, r = %.2f);" tabs s.FN s.FA s.FS s.Radius
                | Cylinder c ->
                    fprintfn writer "%scylinder($fn = %.2f, $fa = %.2f, $fs = %.2f, h = %.2f, r1 = %.2f, r2 = %.2f, center = %b);" tabs c.FN c.FA c.FS c.Height c.BottomRadius c.TopRadius c.Centered
                | Polyhedron p ->
                    fprintfn writer "%spolyhedron(points = %A, faces = %A, convexity = %i);" tabs p.Points p.Faces p.Convexity

            | Operator o ->
                match o with
                | Translate t -> 
                    let v = t.Vector
                    fprintfn writer "%stranslate([%.2f, %.2f, %.2f]) {" tabs v.[0] v.[1] v.[2]
                    printInner writer (tabcount + 1) t.Children
                    fprintfn writer "%s}" tabs
                | Scale s ->
                    let v = s.ScaleFactor
                    fprintfn writer "%sscale([%.2f, %.2f, %.2f]) {" tabs v.[0] v.[1] v.[2]
                    printInner writer (tabcount + 1) s.Children
                    fprintfn writer "%s}" tabs
                | Resize r ->
                    let v = r.NewSize
                    fprintfn writer "%sresize([%.2f, %.2f, %.2f], auto = %b) {" tabs v.[0] v.[1] v.[2] r.Auto
                    printInner writer (tabcount + 1) r.Children
                    fprintfn writer "%s}" tabs
                | Rotate r ->
                    let v = r.Axis 
                    fprintfn writer "%srotate(a = %.2f, v = [%.2f, %.2f, %.2f]) {" tabs r.Degrees v.[0] v.[1] v.[2]
                    printInner writer (tabcount + 1) r.Children
                    fprintfn writer "%s}" tabs
                | Mirror m ->
                    let v = m.Axis
                    fprintfn writer "%smirror([%.2f, %.2f, %.2f]) {" tabs v.[0] v.[1] v.[2]
                    printInner writer (tabcount + 1) m.Children
                    fprintfn writer "%s}" tabs
                | Color c ->
                    fprintfn writer "%scolor([%.2f, %.2f, %.2f, %.2f]) {" tabs c.Red c.Green c.Blue c.Alpha
                    printInner writer (tabcount + 1) c.Children
                    fprintfn writer "%s}" tabs
                | Minkowski m ->
                    fprintfn writer "%sminkowski() {" tabs 
                    printInner writer (tabcount + 1) m.Children
                    fprintfn writer "%s}" tabs
                | Hull h ->
                    fprintfn writer "%shull() {" tabs 
                    printInner writer (tabcount + 1) h.Children
                    fprintfn writer "%s}" tabs


            | Combinator c ->
                match c with
                | Union u ->
                    fprintfn writer "%sunion() {" tabs
                    printInner writer (tabcount + 1) u.Children
                    fprintfn writer "%s}" tabs
                | Difference d ->
                    fprintfn writer "%sdifference() {" tabs 
                    printInner writer (tabcount + 1) d.Children
                    fprintfn writer "%s}" tabs
                | Intersection i ->                
                    fprintfn writer "%sintersection() {" tabs 
                    printInner writer (tabcount + 1) i.Children
                    fprintfn writer "%s}" tabs

            | Projection p ->
                fprintfn writer "%sprojection(cut = %b) {" tabs p.Cut
                printInner writer (tabcount + 1) p.Children
                fprintfn writer "%s}" tabs

            | Extrusion e ->
                match e with
                | Rotational -> ()
                | Linear l ->
                    fprintfn writer "%slinear_extrude(height = %f, center = %b, convexity = %d, twist = %f, slices = %d, scale = %f, $fn = %f) {" tabs l.Height l.Center l.Convexity l.Twist l.Slices l.Scale l.FN
                    printInner writer (tabcount + 1) l.Children
                    fprintfn writer "%s}" tabs

                
            |> ignore                

            printInner writer tabcount t

    printInner writer 0 elements


