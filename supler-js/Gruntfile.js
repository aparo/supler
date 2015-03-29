'use strict';

module.exports = function (grunt) {
    grunt.loadNpmTasks('grunt-contrib-watch');
    grunt.loadNpmTasks('grunt-ts');
    grunt.loadNpmTasks('grunt-contrib-clean');
    grunt.loadNpmTasks('grunt-mocha');
    grunt.loadNpmTasks('grunt-contrib-copy');
    grunt.loadNpmTasks('grunt-contrib-uglify');

    // Configurable paths
    var config = {
        src: 'src',
        tests: 'tests',
        target: 'target'
    };

    grunt.initConfig({
        pkg: grunt.file.readJSON('package.json'),
        config: config,
        ts: {
            default: {
                src: ['<%= config.src %>/**/*.ts'],
                out: '<%= config.target %>/supler.js'
            }
        },
        watch: {
            ts: {
                files: [ '<%= config.src %>/**/*.ts', '<%= config.tests %>/**/*.js' ],
                tasks: [ 'test' ],
                options: {
                    atBegin: true,
                    livereload: true
                }
            }
        },
        clean: [ '<%= config.target %>' ],
        mocha: {
            test: {
                src: [ 'tests/runner.html'],
                options: {
                    run: true,
                    log: true,
                    logErrors: true
                }
            }
        },
        copy: {
            testforms: {
                expand: true,
                src: '../supler/target/scala-2.11/test-classes/*.js',
                dest: 'tests/generated/',
                flatten: true
            },
            suplerjs: {
                expand: true,
                src: 'target/supler*.js',
                dest: '../',
                flatten: true
            }
        },
        uglify: {
            suplerjs: {
                files: {
                    'target/supler.min.js': ['target/supler.js']
                }
            }
        }
    });

    grunt.registerTask('test', [ 'ts', 'copy:testforms', 'mocha', 'uglify:suplerjs', 'copy:suplerjs' ]);

    grunt.registerTask('dev', [ 'watch' ]);
};
