-module(epax).
-export([install/0, add_app/1, remove_app/1, list_apps/0, update_index/0]).

% creates initial directories if not exists
install() ->
    % create the empty file storing the information about applications
    % these packages are added manually by the current user
    os:cmd('mkdir -p ~/.epax'),                             % creating .epax directory if not exists
    os:cmd('touch ~/.epax/index.cfg'),                      % creating index.cfg file
    Data = [],
    write_to_index_file(Data),                              % writing empty list to index.cfg
    os:cmd('mkdir -p ~/.epax/packages'),                    % creating packages directory if not exists
    ok.

% @todo handle other than git
% @todo handle different possible annotations of a link
add_app(Link) ->
    % adding apps (in the file index.cfg)
    % it will have following structure- [{app_name, link_to_repo, publisher, tags, branches}]
    {ok, [Existing_apps]} = file:consult(get_abs_path("index.cfg")),
    case app_exists(Link, Existing_apps) of
        {true, Reason} ->
            io:format("~p~n", [Reason]);
        false ->
            % @optimization => don't checkout the whole repository, download only when required
            Path = get_abs_path("packages/temp"),
            os:cmd(lists:concat(["git clone ", Link, " ", Path])),
            {Appname, Description, Author} = find_publisher(Path),
            Tags = collect_tags(Path),
            Branches = collect_branches(Path),
            Data = Existing_apps ++ [{Appname, Link, {description, Description},
                                                      {publisher, Author},
                                                      {tags, Tags},
                                                      {branches, Branches}}],
            write_to_index_file(Data),
            os:cmd(lists:concat(["mv ", get_abs_path("packages/temp "), get_abs_path("packages/"), Appname])),
            "package/app added successfully!"
    end.

% removes app from the index
remove_app(Appname) ->
    {ok, [Existing_apps]} = file:consult(get_abs_path("index.cfg")),
    New_apps = find_and_remove_key(Appname, Existing_apps),
    write_to_index_file(New_apps),
    run_in_dir(get_abs_path("/packages"), lists:concat(["rm -rf ", atom_to_list(Appname)])),
    ok.

% list all the application/packages indexed
list_apps() ->
    {ok, [Existing_apps]} = file:consult(get_abs_path("index.cfg")),
    lists:map(fun(App) ->
                  element(1, App)
              end,
              Existing_apps).

% updates the index
update_index() ->
    % do nothing as of now!
    none.

% check whether the application is already added into the index
app_exists(_Link, []) ->
    false;
app_exists(Link, [H|_]) when element(2, H) == Link ->
    {true, "the url is already added into the index!"};
app_exists(Link, [_|Rest]) ->
    app_exists(Link, Rest).

% get the absolute path of any file inside the `.epax` folder
get_abs_path(Location) ->
    % @todo error handling
    {ok, [[Home]]} = init:get_argument(home),               % getting the home directory location
    lists:concat([Home, '/.epax/', Location]).              % getting absolute path to index.cfg

% writes the data into index file
write_to_index_file(Data) ->
    file:write_file(get_abs_path("index.cfg"), io_lib:fwrite("~p.\n",[Data])).

% finding the name of the app.src file in the package
get_appfile_name([]) -> 
    % @todo raise error
    "";
get_appfile_name([File|Rest]) ->
    case re:run(File, ".*\.app\.src", [caseless]) of
        {match, _} -> File;
        _ -> get_appfile_name(Rest)
    end.

% finding a value to a key in a list of key-value pair (tuple)
find_key(_, []) ->
    % @todo raise error
    "";
find_key(Key, [{Key, Val}|_]) ->
    Val;
find_key(Key, [_|T]) ->
    find_key(Key, T).

% finds publisher, appname  and description for the application/package
find_publisher(Path) ->
    Src_folder = lists:concat([Path, "/src"]),              %abs location to src folder
    {ok, Files} = file:list_dir(Src_folder),
    App_file_loc = lists:concat([Src_folder, "/", get_appfile_name(Files)]),
    {ok, [Info]} = file:consult(App_file_loc),
    Appname = element(2, Info),
    Description = find_key(description, element(3, Info)),
    Author = find_key(author, element(3, Info)),
    {Appname, Description, Author}.

% runs a command from the given directory
run_in_dir(Path, Cmd) ->
    os:cmd(lists:concat(["cd ", Path, "&& ", Cmd])).

% get the branch names for a repo using regular expression
collect_branches(Path) ->
    Ret = run_in_dir(Path, "git branch --remote"),
    [_|List] = re:split(Ret, "\n"),
    lists:foldl(fun(Branch, Acc) ->
                    Full_branch = binary_to_list(Branch),
                    case Full_branch of
                        "" -> Acc;
                        _ -> {match,[{Loc, Len}]} = re:run(Full_branch, "origin/"),
                             [lists:nthtail(Loc+Len, Full_branch)|Acc]
                    end
                end,
                [],
                List).

% finds all the tags for a given repo path
collect_tags(Path) ->
    List_tags = run_in_dir(Path, "git tag"),
    lists:foldl(fun(T, Acc) ->
                    case Tag = binary_to_list(T) of
                        [] -> Acc;
                        _ -> [Tag|Acc]
                    end
                end,
                [],
                re:split(List_tags, "\n")).

% removes the key from the list of key-value pairs
find_and_remove_key(Appname, [H|T]) when element(1, H) == Appname ->
    T;
find_and_remove_key(Appname, [H|T]) ->
    [H|find_and_remove_key(Appname, T)].
