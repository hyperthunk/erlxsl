
0.2.0 / 2011-03-21
==================

  * Add async state field for providers
  * Change from 2 to 4 spaces indentation
  * Finally fix binary handling for large and small inputs
  * Update to my branch of rebar
  * Tidy up common_test suites; test larger driver I/O
  * Clarify intent of the small binary ct tests
  * Merge branch 'master' of github.com:hyperthunk/erlxsl
  * Work around for small/heap-allocated binaries. Closes #15.
  * Drop out changes to small binary handling due to excessive complexity; will use place holder ref binaries to minimise this instead
  * Pull out task submission; Warnings as errors
  * Update README to reflect support for R14
  * Upgrade to latest rebar
  * Fix failing inttests; Tidy up whitespace; Remove async transform test
  * Store clients as proper list; port controller tidy up
  * Appropriate error data is returned to client
  * Stop ignoring errors in worker processes
  * Fix buffer handling in read_ev
  * Merge branch 'master' of github.com:hyperthunk/erlxsl
  * Fix arch specific build options for OS X
  * Fix ct driver config on linux
  * Stop passing headers, use IOVec/ErlDrvBin properly
  * Simplify port driver/controller protocol
  * Use unsigned 32bit int for size and offset
  * Fix broken assert checking specs
  * fix build error in master
  * Revert "refactor ei_test plumbing"
  * refactor ei_test plumbing
  * check for type and unit allocation failures
  * test cases for ei integration
  * remove cspec folder; tidy up ei include
  * Test write_cmd_data macro
  * provide macro for writing iov command_data
  * remove over-specification from the API tests
  * remove erroneous comment from resize_result_buffer macro
  * fix test engine to use refactored macros
  * ensure buffer size before appending results
  * remove mistakenly added swap file
  * ignore swap files
  * refactor buffer management macros
  * tidy up (assign|write)_result buffer macros
  * tidy uses of ALLOC macro
  * test command free/init
  * test command initialisation
  * refactor assert handling
  * update changelog

0.1.0 / 2011-01-11
==================

  * test for init behaviour when malloc fails
  * add structure init tests
  * test DriverIOVec init function
  * add image loading spec; build only works for Darwin FTM
  * update the build so that cspec tests are bundled properly
  * add cspec build integration
  * add test harness
  * add a light sprinkling of comments to the code
  * factor includes to separate linked-in driver code from general purpose functions
  * rewrite for new data structures API; back in the green
  * rewrite using cleaner api (esp. data types)
  * refactor api data structures
  * rename erlxsl api header
  * switch tests to hamcrest
  * no more tabs
  * consolidate load_library; pass full path to dlopen (temporary fix)
  * rename port_server to port_controller
  * load (native) xslt engine/library dynamically
  * refactor port_server to use state record
  * fix marshall tests to actually run; turn on code coverage; fix ct using test config to specify .so file correctly when cover compiled modules in use
  * adding cover.spec
  * fix marshall tests
  * additional tests for marshalling
  * support application:stop/1 and teardown; control logging (on/off) via options; ct move to application start/stop on init/end per_suite
  * switch to stdint.h; return to sender (not port owner); fire requests to gen_server:call/2 and receive response;
  * pulling protocol back together; darwin working; pulling typedefs from APR; build changes
  * driver protocol fixing;
  * fix logging; add supervision tree; fix app startup; driver initialisation works; driver xsl protocol broken; default config needs fixing
  * embedding rebar; providing Makefile;
  * initialise driver synchronously; pack request data; basic port server
  * migrating port control process from the older prototype.
  * gradual refactoring
  * getting semblance of order - compile now works. :)
  * fleshing out the APIs (still no build system)
  * slowly bringing old code base back in
  * Creating Skeleton Application
  * adding rebar config
  * Initial Commit
