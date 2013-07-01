epax
====

Structure of index file
-----------------------

The index contains the information about the OTP App in the following format-

    {app_name(), link_to_repo(), details()}

    app_name() :: atom()
    link_to_repo() :: url()
    details() ::  {publisher, publisher()::string()}
                | {tags, [tag()]}
                | {branches, [branch()]}
                | {description, description()}
    description() :: string(),
    branch() :: string(),
    tag() :: string().
