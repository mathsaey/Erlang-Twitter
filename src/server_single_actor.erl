%% This is a simple single-actor example implementation of the project.
%%
%% It will create an actor per client connection, and one actor to represent
%% all internal state (all users, their subscriptions, and their tweets).
%%
%% This implementation is provided with unit tests. However, these tests are
%% neither complete nor implementation independent. Thus, be careful when
%% reusing them.
-module(server_single_actor).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

%%
%% Exported Functions
%%
-export([initialize/0,
         register_user/0,
         subscribe/3,

         % internal actors
         data_actor/1,
         entry_actor/0]).

%%
%% API Functions
%%

% Start server.
initialize() ->
    register(data_actor, spawn_link(?MODULE, data_actor, [[]])),
    ok.

% Register a new user and return its id and the entry actor that is
% responsible for this user.
-spec register_user() -> {integer(), pid()}.
register_user() ->
    data_actor ! {self(), register_user},
    receive
        {registered_user, UserId, EntryPid} -> {UserId, EntryPid}
    end.

% Subscribe/follow another user.
-spec subscribe(pid(), integer(), integer()) -> ok.
subscribe(EntryPid, UserId, UserIdToSubscribeTo) ->
    EntryPid ! {self(), subscribe, UserId, UserIdToSubscribeTo},
    receive
        {EntryPid, subscribed, UserId, UserIdToSubscribeTo} -> ok
    end.

% The data actor works like a small database and encapsulates all state of this
% simple implementation.
data_actor(Data) ->
    receive
        {Sender, register_user} ->
            {NewData, NewUserId, NewUserActor} = add_new_user(Data),
            Sender ! {registered_user, NewUserId, NewUserActor},
            data_actor(NewData);
            
        {Sender, get_timeline, UserId, Page} ->
            Sender ! {self(), timeline, UserId, Page, timeline(Data, UserId, Page)},
            data_actor(Data);
            
        {Sender, get_tweets,   UserId, Page} ->
            Sender ! {self(), tweets,   UserId, Page, tweets(Data, UserId, Page)},
            data_actor(Data);
        
        {Sender, tweet,        UserId, Tweet} ->
            {NewData, Timestamp} = tweet(Data, UserId, Tweet),
            Sender ! {self(), tweet_accepted, UserId, Timestamp},
            data_actor(NewData);
        
        {Sender, subscribe,    UserId, UserIdToSubscribeTo} ->
            NewData = subscribe_to_user(Data, UserId, UserIdToSubscribeTo),
            Sender ! {self(), subscribed, UserId, UserIdToSubscribeTo},
            data_actor(NewData)
    end.

% This simple implementation of the entry actor just delegates to the data
% actor.
entry_actor() ->
    DataActor = whereis(data_actor),
    receive
        % RequestType ::= tweets | timeline
        {Sender, RequestType, UserId, PageOrTweetOrUserId} -> 
            DataActor ! {self(), RequestType, UserId, PageOrTweetOrUserId},
            
            receive
                {DataActor, ResponseType, UserId, Page, Result} ->
                    Sender ! {self(), ResponseType, UserId, Page, Result};
                {DataActor, tweet_accepted, UserId, Timestamp} ->
                    Sender ! {self(), tweet_accepted, UserId, Timestamp};
                {DataActor, subscribed, UserId, PageOrTweetOrUserId} ->
                    Sender ! {self(), subscribed, UserId, PageOrTweetOrUserId}
            end
    end,
    entry_actor().

%%
%% Internal Functions
%%
add_new_user(Data) ->
    NewUserId = length(Data),
    NewData = Data ++ [{user, NewUserId, [], sets:new()}],
    NewUserActor = spawn_link(?MODULE, entry_actor, []),
    {NewData, NewUserId, NewUserActor}.

timeline(Data, UserId, _Page) ->
    {user, UserId, Tweets, Subscriptions} = lists:nth(UserId + 1, Data),
    
    UnsortedTweetsForTimeLine =
        lists:foldl(fun(UserId2, AllTweets) ->
                        {_, _, SomeTweets, _} = lists:nth(UserId2 + 1, Data),
                        AllTweets ++ SomeTweets
                    end,
                    Tweets,
                    sets:to_list(Subscriptions)),
    SortedTweets = lists:reverse(lists:keysort(3, UnsortedTweetsForTimeLine)),
    lists:sublist(SortedTweets, 10).

tweets(Data, UserId, _Page) ->
    {user, UserId, Tweets, _Subscriptions} = lists:nth(UserId + 1, Data),
    Tweets.

tweet(Data, UserId, Tweet) ->
    {user, UserId, Tweets, Subscriptions} = lists:nth(UserId + 1, Data),
    Timestamp = erlang:now(),
    NewUser = {user, UserId, Tweets ++ [{tweet, UserId, Timestamp, Tweet}], Subscriptions},

    {UsersBefore, [_|UsersAfter]} = lists:split(UserId, Data),
    {lists:append([UsersBefore, [NewUser | UsersAfter]]), Timestamp}.

subscribe_to_user(Data, UserId, UserIdToSubscribeTo) ->
    {user, UserId, Tweets, Subscriptions} = lists:nth(UserId + 1, Data),
    NewUser = {user, UserId, Tweets, sets:add_element(UserIdToSubscribeTo, Subscriptions)},

    {UsersBefore, [_|UsersAfter]} = lists:split(UserId, Data),
    lists:append([UsersBefore, [NewUser | UsersAfter]]).
    

%%
%% Test Functions
%% 
%% These tests are for this specific implementation. They are a partial
%% definition of the semantics of the provided interface but also make certain
%% assumptions of its implementation. Thus, they need to be reused with care.
%%
initialization_test() ->
    catch unregister(data_actor),
    ?assertMatch(ok, initialize()).

register_user_test() ->
    initialization_test(),

    % We assume here that everything is sequential, and we have simple
    % incremental ids
    ?assertMatch({0, _Pid1}, register_user()),
    ?assertMatch({1, _Pid2}, register_user()),
    ?assertMatch({2, _Pid3}, register_user()),
    ?assertMatch({3, _Pid4}, register_user()).

init_for_test() ->
    catch unregister(data_actor),
    initialize(),
    {0, Pid1} = register_user(),
    {1, Pid2} = register_user(),
    {2, Pid3} = register_user(),
    {3, Pid4} = register_user(),
    [Pid1, Pid2, Pid3, Pid4].

timeline_test() ->
    Pids = init_for_test(),
    [Pid1, Pid2 | _ ] = Pids,

    ?assertMatch([], server:get_timeline(Pid1, 1, 0)),
    ?assertMatch([], server:get_timeline(Pid2, 2, 0)).

users_tweets_test() ->
    Pids = init_for_test(),
    [Pid1 | _ ] = Pids,

    ?assertMatch([], server:get_tweets(Pid1, 1, 0)),
    ?assertMatch([], server:get_tweets(Pid1, 2, 0)).

tweet_test() ->
    Pids = init_for_test(),
    [Pid1, Pid2 | _ ] = Pids,

    ?assertMatch([], server:get_timeline(Pid1, 1, 0)),
    ?assertMatch([], server:get_timeline(Pid2, 2, 0)),

    ?assertMatch({_MegaSecs, _Secs, _MicroSecs}, server:tweet(Pid1, 1, "Tweet no. 1")),

    ?assertMatch([{tweet, _, _, "Tweet no. 1"}], server:get_tweets(Pid1, 1, 0)),
    ?assertMatch([], server:get_tweets(Pid1, 2, 0)),

    ?assertMatch([{tweet, _, _, "Tweet no. 1"}], server:get_timeline(Pid1, 1, 0)), % own tweets included in timeline
    ?assertMatch([], server:get_timeline(Pid2, 2, 0)),

    Pids. % no subscription

subscription_test() ->
    [_Pid1, Pid2 | _ ] = tweet_test(),

    ?assertMatch(ok, subscribe(Pid2, 2, 1)),

    ?assertMatch([{tweet, _, _, "Tweet no. 1"}], server:get_timeline(Pid2, 2, 0)), % now there is a subscription

    ?assertMatch({_MegaSecs, _Secs, _MicroSecs}, server:tweet(Pid2, 2, "Tweet no. 2")),
    ?assertMatch([{tweet, _, _, "Tweet no. 2"},
                  {tweet, _, _, "Tweet no. 1"}],
                 server:get_timeline(Pid2, 2, 0)),
    done. 
