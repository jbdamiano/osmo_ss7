#!/bin/sh -ex

rebar3 compile
rebar3 eunit
