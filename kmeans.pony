use "assert"
use "random"
use "time"

primitive KMeans[A: (Comparable[A] #read & Stringable), B: (Data[A] & Stringable), C: Space[A, B] val]
    fun apply(env: Env, features: Array[B], k: USize, maxIter: USize, space: C): Array[Array[B]]? =>
        Assert(features.size() >= k)?
        let rand = Rand(Time.now()._2.u64())
        let start = rand.usize() %% (features.size() - k).usize()
        var centroids: Array[B] = features.slice(start, start + k)
        var oldCentroids = Array[B](k)
        var iterations: USize = 0
        var res = Array[Array[B]](k)
        var j: USize = 0
        while j < k do
            res.push(Array[B](features.size() / k))
            j = j + 1
        end

        while (iterations < maxIter) and (not _sameCentroids(centroids, oldCentroids, k)?) do
            for r in res.values() do
                r.clear()
            end
            for feat in features.values() do
                var min: (USize, B) = (0, centroids(0)?)
                var mind: A = space.distance(min._2, feat)?
                var i: USize = 1
                while i < k do
                    let x: B = centroids(i)?
                    let dist: A = space.distance(x, feat)?
                    if dist < mind then 
                        min = (i, x)
                        mind = dist
                    end
                    i = i + 1
                end
                res(min._1)?.push(feat)
            end
            oldCentroids = centroids
            centroids = _calculateCentroids(res, k, space)?
            iterations = iterations + 1
        end
        res

    fun _calculateCentroids(labled: Array[Array[B]], k: USize, space: C): Array[B]? =>
        var centroids = Array[B](k)
        for set in labled.values() do
            centroids.push(space.mean(set)?)
        end
        centroids

    fun _sameCentroids(centroids: Array[B] ref, oldCentroids: Array[B] ref, k: USize): Bool? =>
        for c in oldCentroids.pairs() do
            if centroids(c._1)? != c._2 then
                return false
            end
        end
        centroids.size() == oldCentroids.size()