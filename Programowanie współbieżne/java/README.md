# Warsztat

Celem zadania było przygotowanie współbieżnej symulacji dzielenia urządzeń w warsztacie, 
celem maksymalizacji wykorzystania warszatu, ale zapewniając jednocześnie, że każdy chetny
do korzystania z warsztatu z niego skorzysta (żywotność) oraz osoba pracująca przy danej
maszynie ma ją na wyłączność (bezpieczeństwo).


Program korzysta z mechanizmów współbieżnośc w javie, w szczególności z semaforów, mutexów
 i kolejek blokujących. __Interfejsy klas oraz program `demo` zostały dostarczony przez autora zadania.__


Symulacja wypisuje na wyjście linijki z przebiegiem symulacji, które można wkleić do
skryptu w pythonie. Wyświetla on graficzną interpretację napisaną w `pygame`, dzięki
czemu można łatwo ocenić spełnienei warunków zadania.