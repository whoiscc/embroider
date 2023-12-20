class Tree {
    constructor(item, depth) {
        this.item = item;
        this.depth = depth;
        if (depth > 0) {
            this.left = new Tree(2 * item - 1, depth - 1);
            this.right = new Tree(2 * item, depth - 1);
        } else {
            this.left = this.right = null;
        }
    }

    check() {
        if (this.left !== null) {
            return this.item + this.left.check() - this.right.check();
        } else {
            return this.item;
        }
    }
}

const min_depth = 4;
const max_depth = 14;
const stretch_depth = max_depth + 1;

const start = Date.now() / 1000;

console.log('streitch tree of depth ' + stretch_depth);
console.log('check: ' + (new Tree(0, stretch_depth)).check());

const long_lived_tree = new Tree(0, max_depth);
let iterations = 2 ** max_depth;

let depth = min_depth;
while (depth < stretch_depth) {
    let check = 0;
    for (let i = 1; i <= iterations; i += 1) {
        check += (new Tree(i, depth)).check() + (new Tree(-i, depth)).check();
    }

    console.log('num tree: ' + iterations * 2);
    console.log('depth: ' + depth);
    console.log('check: ' + check);

    iterations /= 4;
    depth += 2;
}

console.log('long lived tree of depth:' + max_depth);
console.log('check: ' + long_lived_tree.check());
console.log(Date.now() / 1000 - start);