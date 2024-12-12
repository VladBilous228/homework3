-module(lesson3_task4).
-export([decode/2]).
-export([normalize_json/1]).
-define(COLON, <<":">>).

% Головна функція декодування
decode(Json, proplist) -> decode_json(Json, proplist);
decode(Json, map) -> decode_json(Json, map).

% Загальний парсер JSON
decode_json(<<>> = Json, _Type) -> {ok, Json};
decode_json(Json, proplist) -> decode_proplist(Json, []);
decode_json(Json, map) -> decode_map(Json, #{}).

% Парсер JSON у proplist
decode_proplist(<<"{", Rest/binary>>, Acc) ->
    decode_object(Rest, Acc, proplist);
decode_proplist(_, _) -> error("Invalid JSON structure").

% Парсер JSON у map
decode_map(<<"{", Rest/binary>>, Map) ->
    decode_object(Rest, Map, map);
decode_map(_, _) -> error("Invalid JSON structure").

% Парсер об'єктів JSON
decode_object(<<"\"", Rest/binary>>, Acc, proplist) ->
    {Key, RestAfterKey} = decode_string(Rest),
    {ok, RestAfterColon} = decode_colon(RestAfterKey),
    {Value, RestAfterValue} = decode_value(RestAfterColon),
    decode_object(RestAfterValue, [{Key, Value} | Acc], proplist);
decode_object(<<"\"", Rest/binary>>, Map, map) ->
    {Key, RestAfterKey} = decode_string(Rest),
    {ok, RestAfterColon} = decode_colon(RestAfterKey),
    {Value, RestAfterValue} = decode_value(RestAfterColon),
    decode_object(RestAfterValue, maps:put(Key, Value, Map), map);
decode_object(<<"}", Rest/binary>>, Acc, proplist) ->
    {lists:reverse(Acc), Rest};
decode_object(<<"}", Rest/binary>>, Map, map) ->
    {Map, Rest};
decode_object(_, _, _) -> error("Invalid JSON format").

% Декодуємо рядок
decode_string(Binary) ->
    decode_string(Binary, <<>>).

decode_string(<<"\"", Rest/binary>>, Acc) ->
    {Acc, Rest};
decode_string(<<Char, Rest/binary>>, Acc) ->
    decode_string(Rest, <<Acc/binary, Char>>).

% Перевіряємо двокрапку після ключа
decode_colon(<<":", Rest/binary>>) -> {ok, Rest};
decode_colon(_) -> error("Expected colon after key").

% Парсер значень
decode_value(<<"\"", Rest/binary>>) ->
    decode_string(Rest);
decode_value(<<"true", Rest/binary>>) ->
    {true, Rest};
decode_value(<<"false", Rest/binary>>) ->
    {false, Rest};
decode_value(<<"[", Rest/binary>>) ->
    decode_array(Rest, []);
decode_value(<<"{", Rest/binary>>) ->
    decode_object(Rest, [], proplist);
decode_value(Binary) ->
    decode_number_or_error(Binary).

% Парсер чисел або помилка
decode_number_or_error(Binary) ->
    case take_number(Binary, <<>>) of
        {Number, Rest} -> {list_to_integer(Number), Rest};
        error -> error("Invalid number format")
    end.

take_number(<<Char, Rest/binary>>, Acc) when Char >= $0, Char =< $9 ->
    take_number(Rest, <<Acc/binary, Char>>);
take_number(Binary, Acc) -> {Acc, Binary}.

% Парсер масивів
decode_array(<<"]", Rest/binary>>, Acc) ->
    {lists:reverse(Acc), Rest};
decode_array(Binary, Acc) ->
    {Value, RestAfterValue} = decode_value(Binary),
    decode_array(RestAfterValue, [Value | Acc]).

normalize_json(Text) ->
    % Видалення зайвих пробілів і переведення рядків
    TrimmedText = re:replace(Text, "\\s+", " ", [global, {return, list}]),

    % Додавання лапок навколо ключів
    NormalizedKeys = re:replace(TrimmedText, "(\\w+):", "\"\\1\":", [global, {return, list}]),

    % Додавання лапок навколо значень рядків
    NormalizedValues = re:replace(NormalizedKeys, ": ([a-zA-Z][^,}\\]]*)", ": \"\\1\"", [global, {return, list}]),

    % Видалення зайвих ком перед закриттям об'єктів і масивів
    FixedCommas = re:replace(NormalizedValues, ",[ \\n]*([}\\]])", "\\1", [global, {return, list}]),

    % Очищення та повернення валідного JSON
    FinalJson = "{" ++ FixedCommas ++ "}",
    FinalJson.
