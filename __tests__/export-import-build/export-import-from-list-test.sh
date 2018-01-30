doTest () {
  initFixture symlinks-into-dep
  run esy build
  run esy export-dependencies

  find _export -type f > list.txt
  run cat list.txt

  run rm -rf ../esy/3_*/i/*

  run esy import-build --from ./list.txt

  run ls -1 ../esy/3/i/
}
