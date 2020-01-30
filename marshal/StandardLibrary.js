// Eml standard library
const and = a => b => a && b;
const mod = a => b => a % b;
const eq = a => b => a == b;
const show = num => num.toString();
const log = str => console.log(str);
const rangeTo = n => {
    let acc = [];
    for (let i = n; i > 0; i--)
        acc = [i, acc];
    return acc;
}
const toJSList = xs => {
    let acc = [];
    while (xs && xs[0]) {
        acc.push(xs[0]);
        xs = xs[1];
    }
    return acc;
}
const foldr = f => start => xs => {
    let list = toJSList(xs);
    list.reverse();
    let acc = start;
    for (const elem of list) {
        acc = f(elem)(acc);
    }
    return acc;
}

const interpolate = template => xs => {
    let list = toJSList(xs);
    for (const elem of list)
        template = template.replace(/\{\}/, elem);
    return template;
}
