const div = 20201227

function findLoopSize(rem) {
    let ls = 1
    let x = 1
    while (true) {
        x = (x * 7) % div
        if (x === rem) {
            return ls
        }
        ls++
    }
}

function transform(sub, ls) {
    let x = 1
    for (let i = 0; i < ls; i++) {
        x = (x * sub) % div
    }
    return x
}


const cardPubKey = 12092626
const doorPubKey = 4707356


const cardLoopSize = findLoopSize(cardPubKey)
const doorLoopSize = findLoopSize(doorPubKey)

let e1 = transform(doorPubKey, cardLoopSize)
let e2 = transform(cardPubKey, doorLoopSize)


// 18329280
console.log(e1)
console.log(e2)