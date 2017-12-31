const fs = require('fs')
const {sign, abs} = Math
let input = fs.readFileSync('./input.txt', 'utf8')
input = input
  .trim()
  .replace(/p=</gi, '[[')
  .replace(/>, (v|a)=</gi, '],[')
  .replace(/>/gi, ']]')
  .replace(/\n/gi, ',')
const arr = JSON.parse(`[${input}]`)

for (let i = 0; i < arr.length; i++) {
  arr[i].push(i)
}

const removeCollisions = x => {
  const collides = x.filter(i =>
    x.filter(j =>
      j[0][0] === i[0][0] &&
      j[0][1] === i[0][1] &&
      j[0][2] === i[0][2]
    ).length > 1
  )
  if (collides.length) {
    return x.filter(i => collides.indexOf(i) === -1)
  }
  return x
}

const removeFurthest = (left, esc) => {
  const absolute = left.map(x => [
    abs(x[0][0]) + abs(x[0][1]) + abs(x[0][2]),
    abs(x[1][0]) + abs(x[1][1]) + abs(x[1][2]),
    abs(x[2][0]) + abs(x[2][1]) + abs(x[2][2]),
    x
  ])
  const furthest = absolute.sort((a, b) => b[0] - a[0])[0]
  const fastest = absolute.sort((a, b) => b[1] - a[1])[0]
  const burstiest = absolute.sort((a, b) => b[2] - a[2])[0]
  if (furthest[1] === fastest[1] && furthest[2] === burstiest[2]) {
    left = left.filter(x => x !== furthest[3])
    esc = [...esc, furthest[3]]
  }
  return [left, esc]
}

const signMismatch = x => {
  for (let i = 0; i < 3; i++) {
    if (x[1][i] === 0 && x[2][i] === 0) {
      continue
    }
    const pSign = sign(x[0][i])
    const vSign = sign(x[1][i])
    if (x[2][i] === 0) {
      if (pSign !== vSign) {
        return true
      }
    } else {
      const aSign = sign(x[2][i])
      if (pSign !== vSign || vSign !== aSign) {
        return true
      }
    }
  }
  return false
}

const tick = x => {
  for (const i of x) {
    i[0][0] += (i[1][0] += i[2][0])
    i[0][1] += (i[1][1] += i[2][1])
    i[0][2] += (i[1][2] += i[2][2])
  }
}

let alive = [...arr]
let escaped = []
let checkFurthest = false
while (alive.length) {
  alive = removeCollisions(alive)

  if (!alive.length) {
    break
  }

  checkFurthest = !!checkFurthest || alive.filter(signMismatch).length === 0
  if (checkFurthest) {
    [alive, escaped] = removeFurthest(alive, escaped)
  }

  tick(arr)
}

const [[closest]] = arr.map(x => [
  x[3],
  abs(x[0][0]) + abs(x[0][1]) + abs(x[0][2])
]).sort((a, b) => a[1] - b[1])

console.log('part 1:', closest)
console.log('part 2:', escaped.length)
