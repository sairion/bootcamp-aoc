const map = `..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#`.split("\n")

let treeMetCount = 0
map.forEach((row, rowIndex) => {
  const x = (rowIndex * 3) % map[0].length
  const y = rowIndex

  if (y > 0 && row[x] === "#") {
    treeMetCount += 1
  }
  console.log(treeMetCount)
})
