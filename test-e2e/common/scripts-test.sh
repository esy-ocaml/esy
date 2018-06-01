#!/bin/bash

doTest() {
  initFixture ./fixtures/scripts-workflow

  run esy build

  expectStdout "cmd1_result" esy cmd1
  expectStdout "cmd2_result" esy cmd2
  expectStdout "cmd_array_result" esy cmd3

  expectStdout "script_exec_result" esy x cmd4
  expectStdout "script_exec_result" esy exec_cmd4
  expectStdout "script_exec_result" esy exec_cmd4_cmd

  expectStdout "build_cmd1_result" esy b build_cmd1
  expectStdout "build_cmd2_result" esy b build_cmd2
  expectStdout "build_cmd3_result" esy build_cmd3
}
