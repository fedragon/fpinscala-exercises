package fpinscala

package object chapter6 {
  type Rand[+A] = RNG => (A, RNG)
}
