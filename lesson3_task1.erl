-module(lesson3_task1).
-export([first_word/1]).

first_word(BinText) ->
    first_word(BinText, <<>>, false).

% Рекурсивна функція для пошуку першого слова
first_word(<<>>, Acc, true) -> 
    Acc; % Якщо слово знайдено і досягли кінця рядка, повертаємо його
first_word(<<>>, _Acc, false) -> 
    <<>>; % Якщо не знайдено жодного слова
first_word(<<Char, Rest/binary>>, Acc, WordStarted) ->
    case is_word_char(Char) of
        true ->
            % Якщо символ є частиною слова, додаємо його до акумулятора
            first_word(Rest, <<Acc/binary, Char>>, true);
        false when WordStarted == true ->
            % Якщо слово закінчилося, повертаємо акумулятор
            Acc;
        false ->
            % Пропускаємо символи, які не є частиною слова
            first_word(Rest, Acc, false)
    end.

% Перевірка, чи символ є частиною слова
is_word_char(Char) when Char >= $a, Char =< $z; 
                         Char >= $A, Char =< $Z; 
                         Char >= $0, Char =< $9 ->
    true;
is_word_char(_) ->
    false.
