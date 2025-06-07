#!/usr/bin/env node

/* Usage:
 * node fixup_yarn_lock.js yarn.lock
 */

const fs = require('fs')
const readline = require('readline')

const urlToName = require('../lib/urlToName')

const yarnLockPath = process.argv[2]

const readFile = readline.createInterface({
  input: fs.createReadStream(yarnLockPath, { encoding: 'utf8' }),

  // Note: we use the crlfDelay option to recognize all instances of CR LF
  // ('\r\n') in input.txt as a single line break.
  crlfDelay: Infinity,

  terminal: false, // input and output should be treated like a TTY
})

const result = []

readFile
  .on('line', line => {
    // Adjust regex to also handled scoped and early escaped packages
    const arr = line.match(/^ {2}resolved "([^"]+)"$/)

    if (arr) {
      const [_, target] = arr;

      // Only rewrite if it's an actual URL (http or yarn-escaped)
      if (/^https?:/.test(target) || target.startsWith('https___')) {
        const rewritten = urlToName(target);
        console.error(`🔁 Rewriting: ${target} → ${rewritten}`);
        result.push(`  resolved "${rewritten}"`);
      }
      else {
        // Already a safe filename — do not touch
        result.push(line);
      }
    }
    else {
      result.push(line);
    }
  })
  .on('close', () => {
    fs.writeFile(yarnLockPath, result.join('\n'), 'utf8', err => {
      if (err) {
        console.error(
          'fixup_yarn_lock: fatal error when trying to write to yarn.lock',
          err,
        )
      }
    })
  })
