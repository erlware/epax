epax
====
Erlang Package Manager 1.0.0

[![Build Status](https://travis-ci.org/mangalaman93/epax.png?branch=master)](https://travis-ci.org/mangalaman93/epax)

Structure of index file
-----------------------
The index contains the information about the OTP Application in the following format:

    {Appname, LinkToRepo, RepoType, [Info]}

    Appname    :: atom(),
    LinkToRepo :: url(),
    RepoType   :: git | bzr | svn,
    Info       :: {publisher, Publisher :: string()}
                | {tags, [Tag :: string()]}
                | {branches, [Branch :: string()]}
                | {description, Description :: string()}


BUILDING
--------
To build epax and generate a standalone escript executable:

    $ make

This creates an executable `epax`.


SYNOPSIS
--------

    epax subcommand [options]



COMMANDS
--------

    init                Initialize the index, deletes old index or packages if any
    add    <repo_link>  Add new package into index (repo must follow OTP structure)
    list                List down all packages in the index in lexicographical order
    remove <appname>    Remove the package from index
    update              Update details of all packages in the index
    check               Try to fix broken packages if any, updates the index as well
    bundle <appname>    Figure out dependencies for the package and copies all non-standard packages
