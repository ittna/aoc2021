import Foundation

extension Sequence {
    
    public func pairwise() -> [(Iterator.Element, Iterator.Element)] {
        var pairs = [(Iterator.Element, Iterator.Element)]()
        var previous: Iterator.Element? = nil
        for item in self {
            if let p = previous {
                pairs.append((p, item))
            }
            previous = item
        }
        return pairs
    }
    
}
