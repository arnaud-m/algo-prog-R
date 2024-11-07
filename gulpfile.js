const $ = require('gulp');
const $changed = require('gulp-changed');
const $htmlmin = require('gulp-htmlmin');
const $plumber = require('gulp-plumber');

const del = require('del');
const server = require('browser-sync').create();

$.task('clean', clean);
$.task('publish', publish);
$.task('minimize', pages);
$.task('build', $.series('clean', 'publish', 'minimize'));
$.task('rebuild', $.series('publish', 'minimize'));
$.task('default', $.series('build', $.parallel(serve, watch)));

function clean() {
    // TODO Find home dir : https://stackoverflow.com/questions/34713622/accessing-home-directory-with-base-in-gulp
    return del(['build', '/home/nono/.org-timestamps/org-r-notes.cache', '/home/nono/.org-timestamps/org-r-static.cache'], {force:true});
}

var spawn = require('child_process').spawn;

function publish(cb) {
    //var cmd = spawn('cmd', ['arg1', 'agr2'], {stdio: 'inherit'});
    // var cmd = spawn('ls', [], {stdio: 'inherit'});
    // Emacs Workaround : conflict between whitespace-style and org-publish.
    var cmd = spawn('emacs', ['--eval', '(prog1 (setq whitespace-style \'(face))(org-publish-project "org-r")(kill-emacs))'], {stdio: 'inherit'});
    cmd.on('close', function (code) {
    console.log('emacs org-publish exited with code ' + code);
    cb(code);
    });
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
