import Foundation

public struct Counter<Key> where Key: Hashable {
    let counters: [Key: UInt64]
    
    public init() {
        counters = [Key: UInt64]()
    }
    
    public init(_ key: Key) {
        counters = [key: 1]
    }
    
    init(counters: [Key: UInt64]) {
        self.counters = counters
    }
    
    public func min() -> UInt64? {
        return counters.values.min()
    }
    
    public func max() -> UInt64? {
        return counters.values.max()
    }
    
    public static func +(_ lhs: Counter, _ rhs: Counter) -> Counter {
        var counters = lhs.counters
        for (key, c2) in rhs.counters {
            if let c1 = counters[key] {
                counters[key] = c1 + c2
            } else {
                counters[key] = c2
            }
        }
        return Counter(counters: counters)
    }
}

extension Counter: CustomStringConvertible {
    public var description: String {
        return counters.description
    }
}
