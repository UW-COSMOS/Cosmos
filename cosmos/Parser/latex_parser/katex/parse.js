const katex = require('katex');
try {
    var input = process.argv[2].replace(/\[backslash]/g,'\\')
    input = input.replace(/\[space]/g,' ')
    var html = katex.renderToString(input, {
        throwOnError: true
    });
    console.log(html)
}
catch (e) {
    console.log('-1')
    throw e
}
