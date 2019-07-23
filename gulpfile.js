const $ = require('gulp');
const $changed = require('gulp-changed');
const $htmlmin = require('gulp-htmlmin');
const $plumber = require('gulp-plumber');

const del = require('del');
const server = require('browser-sync').create();

$.task('build', $.series($.parallel(pages)));
$.task('default', $.series('build', $.parallel(serve, watch)));

function clean() {
  return del(['build']);
}

function reload(done) {
  server.reload();
  done();
}

function watch() {
  $.watch(['build/*.html'], $.series(pages, reload));
}

function serve(done) {
  server.init({server: 'build'});
  done();
}

function pages() {
  // https://github.com/gulpjs/gulp/issues/267  
    return $.src('build/*.html',  {base: './'})
    .pipe($changed('build'))
    .pipe($plumber())
    .pipe($htmlmin({
      removeComments: true,
      collapseWhitespace: true,
      removeEmptyAttributes: true,
      minifyJS: true,
      minifyCSS: true}))
    .pipe($.dest('./'));
}

