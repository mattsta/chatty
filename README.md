chatty: riak-backed conversation storage and retrieval
======================================================

Status
------
`chatty` stores comments (or discussion threads)
in riak while maintaining topic/parent/child
hierarchies in redis (using `ghost`).

Usage
-----
See the tests for sample usage.

Building
--------
        rebar get-deps
        rebar compile

Testing
-------
The testing is fairly comprehensive, but it requires
a local redis and riak setup to work.  See the test
module for ideal ports to use or change them to fit
your setup.  Your target redis and riak instances
will be erased when testing starts.

        rebar eunit skip_deps=true suite=chatty

Next Steps
----------
Release full example application of a discussion
system.
