use "random"
use "time"

actor Main
  new create(env: Env) =>
    var vec: Vec2[F64] = Vec2[F64](1, 2)
    var vec1: Vec2[F64] = Vec2[F64](0, 1)
    let count: USize = 10
    var features: Array[Vec2[F64]] = Array[Vec2[F64]](count)
    var i: USize = 0
    let rand = Rand(Time.now()._2.u64())
    while i < count do
      let v = Vec2[F64](
        (rand.real() %% 10) * 10,
        (rand.real() %% 10) * 10)
      features.push(v)
      env.out.print(v.string())
      i = i + 1 
    end

    let k: USize = 3
    let maxIter: USize = 100
    let space: R2 = R2
    env.out.print(IsEuclidean[F64, Vec2[F64]].check(space).string())
    try
      let results = KMeans[F64, Vec2[F64], R2](env, features, k, maxIter, space)?
      for arr in results.pairs() do
        env.out.print("Group " + arr._1.string())
        for r in arr._2.values() do 
          env.out.print(r.string())
        end
      end
    else
      env.out.print("Failed")
    end