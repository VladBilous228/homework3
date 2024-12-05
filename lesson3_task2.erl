-module(lesson3_task2).
-export([words/1]).

words(BinText) ->
    words(BinText, <<>>, []).

% Основна рекурсивна функція
words(<<>>, Acc, Words) -> 
    case Acc of
        <<>> -> lists:reverse(Words);  % Якщо немає накопиченого слова, повертаємо результат
        _ -> lists:reverse([Acc | Words]) % Додаємо останнє слово і повертаємо результат
    end;
words(<<32, Rest/binary>>, Acc, Words) -> 
    case Acc of
        <<>> -> 
            words(Rest, <<>>, Words); % Якщо попереднє слово порожнє, пропускаємо пробіл
        _ -> 
            words(Rest, <<>>, [Acc | Words]) % Додаємо слово до результату
    end;
words(<<Char, Rest/binary>>, Acc, Words) ->
    words(Rest, <<Acc/binary, Char>>, Words).  % Додаємо символ до поточного слова
