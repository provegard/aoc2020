const fs = require("fs").promises







async function readInput() {
    const contents = await fs.readFile("input", { encoding: "utf-8"})
    return contents.split(/\r?\n/).map(Number)
}



async function p1() {
    const numbers = await readInput()
    for (let i = 0; i < numbers.length; i++)
        for (let j = i + 1; j < numbers.length; j++) {
                const a = numbers[i]
                const b = numbers[j]
                if (a + b === 2020) {
                    console.log(a * b)
                    return
                }
            }
}

async function p2() {
    const numbers = await readInput()
    for (let i = 0; i < numbers.length; i++)
        for (let j = i + 1; j < numbers.length; j++)
            for (let z = j + 1; z < numbers.length; z++) {
                const a = numbers[i]
                const b = numbers[j]
                const c = numbers[z]
                if (a + b + c === 2020) {
                    console.log(a * b * c)
                    return
                }
            }
}


p1().then(p2)