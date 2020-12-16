# `String :: [Char]`
- неэффективен по памяти (40 байт на символ)
- неэффективно из-за копирования
- неэффективно как и лист []

# `Text`
- T.pack :: String -> Text
- нормальная строка - указатель на память
- Для юникода внутри программы
- Хорошо для юникода utf16

# `ByteString`
Больше про бинарные данные

Два последних оптимальны как сишный код, асимптотика операций другая.
- Для передачи по сети

### `ByteString.Char8`
Аналогично, но символ 8 байт
Подходит для текста с символами 8 байт

Использовать: 
- для бинарных:
	`ByteString`
- text
	`ByteString.Char8`, `CompactString.ASCII/Latin1/...`
- unicode
	UTF-32: `String`
	UTF-16: `Text`, `CompactString.UTF16`
	UTF-8: `ByteString.UTF8`, `CompactString.UTF8`

NB: Unicode32 только [Char]
