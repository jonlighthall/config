#!/bin/bash -u

function test_this_line() {
    # print file line
    this_line
}

function test_that_line() {
    # print file line
    this_line "here"
    hello
    this_line "this is line 12"
    hello
    this_line
    hello
    this_line "this is line 16"
    this_line "there"

}

function test_lecho() {
    this_line "hello 0"
    lecho "hello 1"
    plecho "hello 2"

}
