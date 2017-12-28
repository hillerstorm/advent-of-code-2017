const run = (second = false) => {
  let i = second ? 106700 : 67
  const max = second ? 123700 : i
  const start = second ? 3 : 2
  let count = 0
  while (true) {
    if (second && i % 2 === 0) {
      count++
    } else {
      const maxX = second ? Math.sqrt(i) : i
      for (let x = start; x < maxX; x++) {
        if (second) {
          if (i % x++ === 0) {
            count++
            break
          }
        } else {
          count += i - start
        }
      }
    }
    if (i === max) {
      break
    }
    i += 17
  }
  return count
}

console.log('Part 1:', run())
console.log('Part 2:', run(true))
