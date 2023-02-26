use "lib:openlibm"
use @sqrt[F64](x: F64)

trait Nominal[A: (Comparable[A] #read & Stringable)]
    fun data(): Array[A]
    fun box eq(that: box->Data[A]): Bool val =>
        let arr1 = data()
        let arr2 = that.data()
        if arr1.size() != arr2.size() then return false end
        for d in arr1.pairs() do
            try
                if d._2 != arr2(d._1)? then
                    return false
                end
            else
                return false
            end
        end
        true
    fun box ne(that: box->Data[A]): Bool val =>
        let arr1 = data()
        let arr2 = that.data()
        if arr1.size() != arr2.size() then return true end
        for d in arr1.pairs() do
            try
                if d._2 != arr2(d._1)? then
                    return true
                end
            else
                return true
            end
        end
        false

trait Ordinal[A: (Comparable[A] #read & Stringable)]
    fun data(): Array[A]
    fun box eq(that: box->Data[A]): Bool val =>
        let arr1 = data()
        let arr2 = that.data()
        if arr1.size() != arr2.size() then return false end
        for d in arr1.pairs() do
            try
                if d._2 != arr2(d._1)? then
                    return false
                end
            else
                return false
            end
        end
        true
    fun box ne(that: box->Data[A]): Bool val =>
        let arr1 = data()
        let arr2 = that.data()
        if arr1.size() != arr2.size() then return true end
        for d in arr1.pairs() do
            try
                if d._2 != arr2(d._1)? then
                    return true
                end
            else
                return true
            end
        end
        false

trait Continuous[A: (Comparable[A] #read & Stringable)]
    fun data(): Array[A] 
    fun box eq(that: box->Data[A]): Bool val =>
        let arr1 = data()
        let arr2 = that.data()
        if arr1.size() != arr2.size() then return false end
        for d in arr1.pairs() do
            try
                if d._2 != arr2(d._1)? then
                    return false
                end
            else
                return false
            end
        end
        true
    fun box ne(that: box->Data[A]): Bool val =>
        let arr1 = data()
        let arr2 = that.data()
        if arr1.size() != arr2.size() then return true end
        for d in arr1.pairs() do
            try
                if d._2 != arr2(d._1)? then
                    return true
                end
            else
                return true
            end
        end
        false

trait Discrete[A: (Comparable[A] #read & Stringable)]
    fun data(): Array[A]
    fun box eq(that: box->Data[A]): Bool val =>
        let arr1 = data()
        let arr2 = that.data()
        if arr1.size() != arr2.size() then return false end
        for d in arr1.pairs() do
            try
                if d._2 != arr2(d._1)? then
                    return false
                end
            else
                return false
            end
        end
        true
    fun box ne(that: box->Data[A]): Bool val =>
        let arr1 = data()
        let arr2 = that.data()
        if arr1.size() != arr2.size() then return true end
        for d in arr1.pairs() do
            try
                if d._2 != arr2(d._1)? then
                    return true
                end
            else
                return true
            end
        end
        false

type Data[A: (Comparable[A] #read & Stringable)] is (Nominal[A] | Ordinal[A] | Continuous[A] | Discrete[A])

trait Space[A: (Comparable[A] #read & Stringable), B: Data[A]]
    fun inner(x: B, y: B): A?
    fun distance(x: B, y: B): A?
    fun zero(): B
    fun mean(data: Array[B]): B?

trait Vector[A: (Real[A] & Float & Comparable[A] #read & Stringable)] is Continuous[A]
    fun data(): Array[A]
    fun box string(): String iso^

interface Euclidean[A: (Real[A] & Float), B: Vector[A] ref] is Space[A, B]
    fun inner(x: B, y: B): A?
    fun distance(x: B, y: B): A?
    fun zero(): B
    fun mean(data: Array[B]): B?

primitive IsEuclidean[A: (Real[A] & Float), B: Vector[A] ref]
    fun check(e: Euclidean[A, B] val): Bool => 
        // let zero = e.zero()
        // if (e.distance(zero, zero))
        true

class Vec3[A: (Real[A] & Float)] is Vector[A]
    let x: A
    let y: A
    let z: A
    new create(x': A, y': A, z': A) =>
        x = x'
        y = y'
        z = z'
    fun data(): Array[A] => [x; y; z]
    fun box string(): String iso^ =>
        var s: String = "< "
        for d in data().values() do
            s = s + d.string() + " "
        end
        s + ">"

primitive R3 is Space[F64 val, Vec3[F64 val] ref]
    fun inner(r1: Vec3[F64], r2: Vec3[F64]): F64? =>
        let arr1 = r1.data()
        let arr2 = r2.data()
        let x: F64 = (arr1(0)? * arr2(0)?) 
        let y: F64 = (arr1(1)? * arr2(1)?)
        let z: F64 = (arr1(2)? * arr2(2)?)
        x + y + z

    fun distance(r1: Vec3[F64], r2: Vec3[F64]): F64? => 
        let arr1 = r1.data()
        let arr2 = r2.data()
        let x: F64 = (arr1(0)? - arr2(0)?) 
        let y: F64 = (arr1(1)? - arr2(1)?)
        let z: F64 = (arr1(2)? - arr2(2)?) 
        @sqrt(((x*x) + (y*y) + (z*z)))

    fun zero(): Vec3[F64] =>
        Vec3[F64](0,0,0)

    fun mean(data: Array[Vec3[F64]]): Vec3[F64]? =>
        var sum = zero()
        for d in data.values() do
            let arr1 = sum.data()
            let arr2 = d.data()
            sum = Vec3[F64](
                (arr1(0)? + arr2(0)?),
                (arr1(1)? + arr2(1)?),
                (arr1(2)? + arr2(2)?))
        end
        let arr1 = sum.data()
        Vec3[F64](
            (arr1(0)? / data.size().f64()),
            (arr1(1)? / data.size().f64()),
            (arr1(1)? / data.size().f64()))
    
class Vec2[A: (Real[A] & Float)] is Vector[A]
    let x: A
    let y: A
    new create(x': A, y': A) =>
        x = x'
        y = y'
    fun data(): Array[A] => [x; y]
    fun box string(): String iso^ =>
        var s: String = "< "
        for d in data().values() do
            s = s + d.string() + " "
        end
        s + ">"

primitive R2 is Space[F64 val, Vec2[F64 val] ref]
    fun inner(r1: Vec2[F64], r2: Vec2[F64]): F64? =>
        let arr1 = r1.data()
        let arr2 = r2.data()
        let x: F64 = (arr1(0)? * arr2(0)?) 
        let y: F64 = (arr1(1)? * arr2(1)?)
        x + y

    fun distance(r1: Vec2[F64], r2: Vec2[F64]): F64? => 
        let arr1 = r1.data()
        let arr2 = r2.data()
        let x: F64 = (arr1(0)? - arr2(0)?) 
        let y: F64 = (arr1(1)? - arr2(1)?)
        @sqrt(((x*x) + (y*y)))

    fun zero(): Vec2[F64] =>
        Vec2[F64](0,0)

    fun mean(data: Array[Vec2[F64]]): Vec2[F64]? =>
        var sum = zero()
        for d in data.values() do
            let arr1 = sum.data()
            let arr2 = d.data()
            sum = Vec2[F64](
                (arr1(0)? + arr2(0)?),
                (arr1(1)? + arr2(1)?))
        end
        let arr1 = sum.data()
        Vec2[F64](
            (arr1(0)? / data.size().f64()),
            (arr1(1)? / data.size().f64()))
    

