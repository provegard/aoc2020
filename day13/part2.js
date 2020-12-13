
const fs = require("fs")

const inp = fs.readFileSync("input", { encoding: "utf-8" })
const l2 = inp.split(/\n/)[1]

const parts = l2.split(/,/).map((e, i) => [+e, i]).filter((tup) => !isNaN(tup[0]))
console.log(parts)

const buses = parts

let tick = Date.now()

let t = 0
let step = buses[0][0]
let changed = false
buses[0]._first = 0
while (true) {

    let all = true
    for (const tup of buses) {
        const b = tup[0]
        const diff = tup[1]
        const match = (t + diff) % b === 0
        all = all && match

        if (match && tup !== buses[0]) {
            if (!tup._first) {
                tup._first = t
                console.log(`Locked ${tup[0]} at t = ${t}`)
            }
        }
    }

    if (!changed && buses.every((tup) => typeof tup._first !== "undefined")) {
        const muls = buses.slice(1).map((tup) => buses[0][0] * tup[0])
        const max = Math.max.apply(null, muls)

        console.log("At time t = " + t)
        console.log("Max = " + max)

        step = max
        changed = true
    }

    if (all) {
        console.log("All done at t = " + t)
        break;
    }


    t += step

    if (Date.now() - tick > 10000) {
        console.log(`t = ${t}`)
        tick = Date.now()
    }
}



/*

t = 267421693679530
t = 268280168610968
t = 269151316554301
t = 269991875386661
t = 271050324497597
t = 272149672049412
t = 273378019823801


t = 505126221866428
t = 506313517854289
t = 507534786211441
t = 508751676044574
t = 509974510287014
t = 511240337326547
t = 512504764530280

t = 595171366442979
t = 596294871299086
t = 597466596461359
t = 598640414751150
t = 599748745058070
All done at t = 600691418730595

*/