-module(lesson3_task3).
-export([split/2]).

split(Binary, Splitter) when is_binary(Binary), is_binary(Splitter) ->
    split(Binary, Splitter, <<>>, []).

split(<<Char, Rest/binary>>, Splitter, Acc, Words) ->
    SplitterSize = byte_size(Splitter),
    case <<Char, Rest/binary>> of
        <<Splitter:SplitterSize/binary, Rest1/binary>> -> 
            case Acc of
                <<>> -> split(Rest1, Splitter, <<>>, Words);
                _ -> split(Rest1, Splitter, <<>>, Words ++ [Acc])
            end;
        _ -> split(Rest, Splitter, <<Acc/binary, Char>>, Words)
    end;
split(<<>>, _Splitter, Acc, Words) ->
    case Acc of
        <<>> -> Words;
        _ -> Words ++ [Acc]
    end.