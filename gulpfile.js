const gulp = require('gulp');
const plumber = require('gulp-plumber');

const { deleteAsync } = require('del');
const browserSync = require('browser-sync').create();

const through2 = require('through2');
const { minify } = require('html-minifier-terser');
const { spawn } = require('child_process');

/* =========================
   TASKS (Gulp 4 style)
========================= */

gulp.task('clean', clean);
gulp.task('publish', publish);
gulp.task('minimize', pages);

gulp.task('build', gulp.series('clean', 'publish', 'minimize'));
gulp.task('rebuild', gulp.series('publish', 'minimize'));

gulp.task('default', gulp.series('build', gulp.parallel(serve, watch)));

/* =========================
   CLEAN
========================= */

function clean() {
    return deleteAsync([
        'build',
        '/home/nono/.org-timestamps/org-r-notes.cache',
        '/home/nono/.org-timestamps/org-r-static.cache'
    ], { force: true });
}

/* =========================
   EMACS PUBLISH
========================= */

function publish(cb) {
    // Emacs Workaround : conflict between whitespace-style and org-publish.
  const cmd = spawn(
    'emacs',
    [
      '--eval',
      `(prog1
         (setq whitespace-style '(face))
         (org-publish-project "org-r")
         (kill-emacs))`
    ],
    { stdio: 'inherit' }
  );

  cmd.on('close', (code) => {
    console.log('emacs org-publish exited with code ' + code);
    cb(code);
  });
}

/* =========================
   HTML MINIFY STREAM
========================= */

function htmlMinify() {
  return through2.obj(function (file, _, cb) {
    if (file.isBuffer()) {
      minify(file.contents.toString(), {
        removeComments: true,
        collapseWhitespace: true,
        removeEmptyAttributes: true,
        minifyJS: true,
        minifyCSS: true
      })
        .then(result => {
          file.contents = Buffer.from(result);
          cb(null, file);
        })
        .catch(cb);
    } else {
      cb(null, file);
    }
  });
}

/* =========================
   PAGES
========================= */

function pages() {
  return gulp.src('build/**/*.html', { base: './' })
    .pipe(plumber())
    .pipe(htmlMinify())
    .pipe(gulp.dest('./'));
}

/* =========================
   WATCH + SERVER
========================= */

function reload(done) {
  browserSync.reload();
  done();
}

function watch() {
  gulp.watch('build/**/*.html', gulp.series(pages, reload));
}

function serve(done) {
  browserSync.init({
    server: 'build'
  });
  done();
}
