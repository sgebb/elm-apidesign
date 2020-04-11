var gulp = require('gulp');
var elm  = require('gulp-elm');
var plumber = require('gulp-plumber');
var del = require('del');
var browserSync = require('browser-sync');

// File paths
var paths = {
  dest: 'dist',
  src: 'src',
  elm: 'src/*.elm',
  static: 'src/*.{html,css,png,ico}'
};

// Clean dist folder

// Remove pre-existing content from output folders
var clean = function(done) {
    // Clean the dist folder
  	del.sync([
  		paths.dest
  	]);

	// Signal completion
	done();
};

// Make elm into js
var elmCompile = function(done) {
  return gulp.src(paths.elm, { optimize: true })
    .pipe(elm())
    .pipe(gulp.dest('dist/'));
};

// Move static assets to dist
var staticCompile = function(done) {
    return gulp.src(paths.static)
        .pipe(plumber())
        .pipe(gulp.dest('dist/'));
};

// Pack project
var elmbundle = function(done) {
  return gulp.src('src/**/Main.elm', { optimize: true })
    .pipe(elm.bundle('bundle.js'))
    .pipe(gulp.dest('dist/'));

	// Signal completion
	done();
};

// Watch for changes to the src directory
var startServer = function(done) {
	// Initialize BrowserSync
	browserSync.init({
		server: {
			baseDir: paths.dest
		}
	});

	// Signal completion
	done();
};

// Reload the browser when files change
var reloadBrowser = function(done) {
	browserSync.reload();

	// Signal completion
	done();
};

// Watch for changes
var watchSource = function(done) {
	gulp.watch(paths.src, gulp.series(exports.default, reloadBrowser));

	// Signal completion
	done();
};

var pack = function(done) {
    gulp.series(elmbundle, staticCompile)

	// Signal completion
	done();
};

/**
 * Export Tasks
 */

// Default task
// gulp
exports.default = gulp.series(
  clean,
	elmCompile,
  staticCompile
);

// Watch and reload
// gulp watch
exports.dev = gulp.series(
	exports.default,
	startServer,
	watchSource
);
