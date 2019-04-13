use rand::distributions::Uniform;
use rand::prelude::*;

pub type Scalar = u8;

pub trait PerfectHash {
    #[inline]
    fn hash<K: AsRef<[u8]>>(&self, key: K) -> usize {
        let func = |sample: &[Scalar]| -> Scalar {
            let bytes = key.as_ref().iter().enumerate();
            let hash: usize = bytes
                .map(|(i, &b)| (sample[i % sample.len()] as usize) * b as usize)
                .sum();
            (hash % self.graph().len()) as Scalar
        };
        let (l, r) = (func(&self.left()) as usize, func(&self.right()) as usize);
        (self.graph()[l] as usize + self.graph()[r] as usize) % self.graph().len()
    }
    fn left(&self) -> &[Scalar];
    fn right(&self) -> &[Scalar];
    fn graph(&self) -> &[Scalar];
}

#[derive(Debug)]
pub struct Hash {
    left: Vec<Scalar>,
    right: Vec<Scalar>,
    graph: Vec<Scalar>,
}

impl PerfectHash for Hash {
    fn left(&self) -> &[Scalar] {
        &self.left
    }
    fn right(&self) -> &[Scalar] {
        &self.right
    }
    fn graph(&self) -> &[Scalar] {
        &self.graph
    }
}

impl Hash {
    pub fn new(left: Vec<Scalar>, right: Vec<Scalar>, graph: Vec<Scalar>) -> Self {
        assert_eq!(left.len(), right.len());
        Self { left, right, graph }
    }
}

pub fn generate_hash<K, I>(mut keys: I) -> Hash
where
    K: AsRef<[u8]>,
    I: ExactSizeIterator<Item = (usize, K)> + Clone,
{
    const TRIALS: Scalar = 5;

    let mut n = keys.clone().map(|(v, _)| v + 1).max().unwrap_or(1) as Scalar;
    let mut trial = 0;
    let (mut f1, mut f2, vals) = loop {
        if trial > 0 && trial % TRIALS == 0 {
            n = (n + 1).max((1.05 * f64::from(n)) as Scalar);
        }
        trial += 1;

        let (mut g, mut f1, mut f2) = (Graph::new(n), Hasher::new(n), Hasher::new(n));
        for (v, k) in keys.clone() {
            g.connect(f1.apply(&k) as Scalar, f2.apply(&k) as Scalar, v as Scalar);
        }

        if let Some(vals) = g.map() {
            break (f1, f2, vals);
        }
    };

    debug_assert!(keys.all(|(v, k)| (vals[f1.apply(&k) as usize] as usize
        + vals[(f2.apply(&k) as usize)] as usize)
        % usize::from(n)
        == v));

    Hash::new(f1.into_salt(), f2.into_salt(), vals)
}

struct Graph {
    vertices: Scalar,
    adjacent: Vec<Vec<(Scalar, Scalar)>>,
}

impl Graph {
    pub fn new(vertices: Scalar) -> Self {
        Self {
            vertices,
            adjacent: vec![vec![]; vertices as usize],
        }
    }

    pub fn connect(&mut self, left: Scalar, right: Scalar, value: Scalar) {
        self.adjacent[left as usize].push((right, value));
        self.adjacent[right as usize].push((left, value));
    }

    pub fn map(&self) -> Option<Vec<Scalar>> {
        let (mut vals, mut visited) = (
            vec![Scalar::max_value(); self.vertices as usize],
            vec![false; self.vertices as usize],
        );

        for root in 0..self.vertices {
            if visited[root as usize] {
                continue;
            }
            vals[root as usize] = 0;

            let mut next = vec![(None, root)];
            while let Some((parent, vertex)) = next.pop() {
                visited[vertex as usize] = true;

                let mut skip = true;
                for &(neighbor, value) in &self.adjacent[vertex as usize] {
                    if skip && Some(neighbor) == parent {
                        skip = false;
                        continue;
                    }

                    if visited[neighbor as usize] {
                        return None;
                    }

                    next.push((Some(vertex), neighbor));

                    let val = (self.vertices + value - vals[vertex as usize]) % self.vertices;
                    vals[neighbor as usize] = val;
                }
            }
        }

        debug_assert!(vals.iter().all(|&x| x < Scalar::max_value()));
        Some(vals)
    }
}

struct Hasher {
    n: Scalar,
    salt: Vec<u64>,
}

impl Hasher {
    pub fn new(n: Scalar) -> Self {
        Self { n, salt: vec![] }
    }

    pub fn apply<K: AsRef<[u8]>>(&mut self, key: K) -> u64 {
        let key = key.as_ref();
        if self.salt.len() < key.len() {
            let mut rng = thread_rng();
            self.salt.extend(
                Uniform::new(0, u64::from(self.n))
                    .sample_iter(&mut rng)
                    .take(key.len() - self.salt.len()),
            )
        };

        let salt = self
            .salt
            .iter()
            .zip(key)
            .map(|(&k, &v)| k * u64::from(v))
            .sum::<u64>();

        salt % u64::from(self.n)
    }

    pub fn into_salt(self) -> Vec<Scalar> {
        self.salt.into_iter().map(|b| b as Scalar).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn animals() {
        let animals = ["Dog", "Cat", "Mouse", "Camel", "Elephant"];

        let h = generate_hash(animals.iter().enumerate());
        assert!(animals
            .iter()
            .enumerate()
            .all(|(i, animal)| h.hash(animal) == i));
    }

    #[test]
    fn states() {
        let states = [
            "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN",
            "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV",
            "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN",
            "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY",
        ];

        let h = generate_hash(states.iter().enumerate());
        assert!(states.iter().enumerate().all(|(i, v)| h.hash(v) == i));
    }
}
