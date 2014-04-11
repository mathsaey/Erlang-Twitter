% logger.erl
% Mathijs Saey
% Multicore Programming

% This module defines the functions for logging data.

% The logger is a process responsible for receiving and displaying
% logging messages. It doesn't need to scale or be fast.
% It only needs to ensure other processes are not delayed by logging.

-module(logger).
-export([start/0, info/1, info/2, warn/1, warn/2, err/1, err/2]).

% Logger name
const_name() -> twitter_logger.

% Output file
const_file() -> "../twitter.log".

% Get the Pid of the logger.
get_logger() -> whereis(const_name()).

% -------- %
% External %
% -------- %

% Log a message.
log(Level, Message, Args) -> get_logger() ! {self(), Level, Message, Args}.

% Log a message with a severity level.
%
% Message
%	The message you want to log.
% Args (optional)
%	Arguments that will be added to the message.
%	The Message argument should be a format string
%	if arguments are provided.


info(Message) -> info(Message, []).
warn(Message) -> warn(Message, []).
err(Message) -> err(Message, []).
info(Message, Args) -> log("INFO", Message, Args), ok.
warn(Message, Args) -> log("WARN", Message, Args), ok.
err(Message, Args)   -> log("ERR!", Message, Args), ok.

% -------- %
% Internal %
% -------- %

% Start the logger.
start() -> register(const_name(), spawn(fun() -> log_loop() end)).

% Open the output file and start the actual logging.
log_loop() ->
	{_, File} = file:open(const_file(), [delayed_write, write]),
	log_loop(File).

% Main loop of the logger.
log_loop(File) ->
	receive
		{Pid, Level, Message, Args} ->
			Str = io_lib:format(Message, Args), 
			io:fwrite(File, "[~w][~s]: ~s~n", [Pid, Level, Str]),
			log_loop(File)
	end.