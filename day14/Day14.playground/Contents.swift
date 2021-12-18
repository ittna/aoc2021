import Foundation

guard let sampleUrl = Bundle.main.url(forResource: "input", withExtension: "txt"),
    let sample = try? String(contentsOf: sampleUrl) else {
    fatalError("no input")
}

let lines = sample.split(separator: "\n")
let template = String(lines[0])
let rules = Dictionary(uniqueKeysWithValues: lines[1...]
    .map { $0.components(separatedBy: " -> ") }
    .map { ($0[0], $0[1]) })

struct FunctionArgs<A1: Hashable, A2: Hashable, A3: Hashable> : Hashable {
    let a1: A1
    let a2: A2
    let a3: A3
}

func memoize<A1, A2, A3, V>(fn: @escaping (A1, A2, A3) -> V) -> (A1, A2, A3) -> V where A1: Hashable, A2: Hashable, A3: Hashable {
    var cache = [FunctionArgs<A1, A2, A3>: V]()
    return { a1, a2, a3 -> V in
        let args = FunctionArgs(a1: a1, a2: a2, a3: a3)
        if let value = cache[args] {
            return value
        }
        
        let value = fn(a1, a2, a3)
        cache[args] = value
        return value
    }
}

var memoCount: ((Int, String, String) -> Counter<String>) = { _, _, _ in Counter() }
func count(steps: Int, a: String, b: String) -> Counter<String> {
    if steps == 0 {
        return Counter(a)
    }
    else {
        guard let ab = rules[a + b] else {
            fatalError("no rule for \(a + b)")
        }
        return memoCount(steps - 1, a, ab)
        + memoCount(steps - 1, ab, b)
    }
}
memoCount = memoize(fn: count(steps:a:b:))

func solve(steps: Int, template: String) -> UInt64 {
    var result = template.pairwise()
        .map { (String($0.0), String($0.1)) }
        .map { memoCount(steps, $0.0, $0.1) }
        .reduce(Counter()) { $0 + $1 }
    result = result + Counter(String(template.last!))

    guard let max = result.max(), let min = result.min() else {
        fatalError("no max or min count in \(template)")
    }
    return max - min
}

solve(steps: 10, template: template)
solve(steps: 40, template: template)
