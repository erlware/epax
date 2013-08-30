epax
====
Erlang Package Manager

[![Build Status](https://travis-ci.org/mangalaman93/epax.png?branch=feature_basic_git)](https://travis-ci.org/mangalaman93/epax)

Structure of index file
-----------------------
The index contains the information about the OTP App in the following format-

    {app_name(), link_to_repo(), [info()]}

    app_name()  :: atom()
    repo_link() :: url()
    info()      :: {publisher, publisher()::string()}
                 | {tags, [tag()]}
                 | {branches, [branch()]}
                 | {description, descrip()}
    descrip()   :: string(),
    branch()    :: string(),
    tag()       :: string(),
    revision()  :: integer().


BUILDING
--------
To build epax and generate a standalone escript executable:

    $ make

This creates the executable `epax`.


SYNOPSIS
--------

    epax subcommand [options]



COMMANDS
--------

    init                Initialize the index, deletes old index or packages if any
    add    <repo_link>  Add new package into index (repo must follow OTP structure)
    list                List down all packages in the index in lexicographical order
    remove <appname>    Remove the package from index
    update              Update information about all packages added into the index
    check               Try to fix broken packages if any, updates the index too
    bundle <appname>    Figure out the dependencies for the application and copies all valid packages into deps folder
