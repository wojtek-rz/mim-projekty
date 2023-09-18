# Egzekutor

Zadanie polegało na napisaniu aplikacji do równoległego uruchamiania programów i sprawdzania ostatniej linijki wypisanej przez programy.
Ponadto wypisuje w czasie rzeczywistym powiadomienia o zakończeniu programu i jego kodzie wyjścia.

Egzekutor jest napisany w języku c i korzysta z mechanizmów kontroli współbieżności z biblioteki `pthread` oraz poleceń systemowych i sygnałów.

W folderze `testing` znajdują się skrypty do testowania działania egzekutora.
