epax
====

Overview
--------

The remote repositories consist of two parts. A list of sources that
point to individual source control repositories that contain OTP
Applications and an index that contains information about the packages
and a url pointing to where those package items live.

There is a remote process that takes the list of sources and produces
packages from those sources. The process is as follows

* For each item in the sources list
* access the repository and pull it somewhere local
* Look for new tags that look like a version number in that repository
  (based against what is already in the index)
* For each new tag create a tarball of the OTP Application
* Push the tarball to an accessable source (S3 may be a good option here)
* update the index with information about the new app

Optionally (in the future)

Check each version of the OTP App and make sure that the tag has not
changed based on the commit id/ref in the index.

### Repo Layout

   <repo>
     |- index.cfg
     |- sources.cfg


Source
------

The `sources` file contains a list of source repositories that contain
OTP Applications. These sources are

### Source format

    {name(), repo()}
    | {name(), location(), repo()}

    name() :: atom()
    location() :: string() % Location relative to repo root on disk
    repo() :: {type(), source(), spec()}

    type() :: github | git | bzr | svn | bitbucket
    source() :: string() % varies depending on type()
    spec() :: {spec_id(), detail()}
    spec_id() :: branch | tag | ref
    detail() :: string() % "master" for the master branch, etc

#### Example

    {dialyxir,
      {github,"jeremyjh/dialyxir",
        {branch,"master"}}}

    {dialyxir,
      "/apps/dialyxir",
      {github,"jeremyjh/dialyxir",
        {branch,"master"}}}

Index
-----

The index contains the information about the OTP App in question.

    {name(), vsn(), [detail()]}.

    name() :: atom()
    vsn() :: string() % must be a semantic version
    detail() ::  {dependencies, [ec_semver:constraint()]}
               | {build_dependencies, [ec_semver:constraint()]}
               | {vcs_ref, string()}
               | {date_published, string()}
               | {author, string()}

#### Example

    {dialyxir, "0.0.1",
     [{dependencies, [{kernel, "0.0.1", gte},
                      stdlib]}
      {build_dependencies, [rebar]},
      {vcs_ref, "EAF0120BB"},
      {date_published, "01/02/2013"},
      {author, "ericbmerritt@gmail.com"}]}
