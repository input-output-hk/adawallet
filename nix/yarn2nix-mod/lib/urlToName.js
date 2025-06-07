const path = require('path')
const { URL } = require('url')

// String -> String

// @url examples:
// - https://registry.yarnpkg.com/acorn-es7-plugin/-/acorn-es7-plugin-1.1.7.tgz
// - https://registry.npmjs.org/acorn-es7-plugin/-/acorn-es7-plugin-1.1.7.tgz
// - git+https://github.com/srghma/node-shell-quote.git
// - git+https://1234user:1234pass@git.graphile.com/git/users/1234user/postgraphile-supporter.git
// - https://codeload.github.com/Gargron/emoji-mart/tar.gz/934f314fd8322276765066e8a2a6be5bac61b1cf

// Required for scoped and early escaped package support.
//
// This function won't produce perfect canonical escaped filenames that yarn
// would normally expect, but to do so would add substantially more complexity.
// As long as naming between yarn.lock, yarn.nix and offline cache tarball
// names are consistent, which this function will provide for, there is no
// problem.
//
// Examples of new url types now supported:
// - Scoped:
//   - https://registry.npmjs.org/@emurgo/cardano-serialization-lib-nodejs/-/cardano-serialization-lib-nodejs-13.1.0.tgz
// - Early escaped:
//   - https___registry.npmjs.org__is_arguments___is_arguments_1.0.4.tgz_3faf966c...
module.exports = function urlToName(urlStr) {
  // Normalize Yarn-escaped URLs
  const maybeUrl =
    urlStr.startsWith('https___') || urlStr.includes('__')
      ? urlStr.replace(/^https___/, 'https://').replace(/__/g, '/')
      : urlStr

  try {
    const url = new URL(maybeUrl)

    if (
      url.hostname.includes('registry.yarnpkg.com') ||
      url.hostname.includes('registry.npmjs.org')
    ) {
      // Yarn-style registry tarball: flatten the path only
      return url.pathname.slice(1).replace(/[@/%:-]/g, '_')
    }

    // For non-registry URLs, use the file name only
    return path.basename(url.pathname)
  }

  catch (_) {
    // Fallback for malformed or already-escaped inputs
    return urlStr.replace(/[@/%:-]/g, '_')
  }
}
