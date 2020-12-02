const fs = require("fs").promises

async function readInput() {
    const contents = await fs.readFile("input", { encoding: "utf-8"})
    return contents.split(/\r?\n/).map(Number)
}

function sum(tup) {
    return tup.reduce((s, i) => s + i, 0)
}

function mul(tup) {
    return tup.reduce((s, i) => s * i, 1)
}

function first(iter, predicate) {
    for (const item of iter) {
        if (predicate(item)) {
            return item
        }
    }
    throw new Error("No item")
}

function *genTuples(tup, list, startIdx, tupleLen) {
    if (tupleLen === 0) throw new Error("tupleLen === 0")
    else if (tupleLen === 1) {
        for (let i = startIdx, j = list.length; i < j; i++) yield [...tup, list[i]]
    }
    else {
        for (let i = startIdx, j = list.length; i < j; i++) {
            const t2 = [...tup, list[i]]
            for (const t of genTuples(t2, list, i + 1, tupleLen - 1)) {
                yield t
            }

        }
    }
}


function tuples(list, tupleLen) {
    return genTuples([], list, 0, tupleLen)
}

async function p12() {
    const numbers = await readInput()
    const tups = tuples(numbers, 2)
    const t = first(tups, tx => sum(tx) === 2020)
    console.log(mul(t))
}

async function p22() {
    const numbers = await readInput()
    const tups = tuples(numbers, 3)
    const t = first(tups, tx => sum(tx) === 2020)
    console.log(mul(t))
}

p12().then(p22)