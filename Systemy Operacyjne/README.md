# [Systemy operacyjne](https://usosweb.uw.edu.pl/kontroler.php?_action=katalog2/przedmioty/pokazPrzedmiot&prz_kod=1000-214bSOB)

Zadania 1 oraz 2 polegały na napisaniu programu w asemblerze x86-64 (w NASM). Natomiast zadania 3, 4 i 5 polegały na modyfikacji kodu źródłowego prostego systemu operacyjnego [MINIX](https://www.minix3.org/). Do nakładania łatki służy polecenie `patch -p1 < patch_file.patch` w katalogu głównym na maszynie witrualnej MINIX.

Zadanie 3 polega na modyfikacji serwera procesów PM i dodaniu do niego obsługi płatności między procesami za pomocą nowego wywołania systemowego. W zadaniu 4 należało dodać nowy sposób szeregowania procesów przez zmiany w serwerze `sched` oraz w kernelu. Zadanie 5 polegało na dodaniu do serwera plików nowej funkcji dającej użytkownikowi plik na wyłączność.

# Punkty

|     ZADANIE     |      PLIK       |      PUNKTY       |
| :-------------: | :-------------: | :---------------: |
|    **_zadanie 1_**    | `inverse_permutation.asm` |  **5** / 5 _pkt_  |
|    **_zadanie 2_**    | `core.asm` |  **4.8** / 5 _pkt_  |
| **_zadanie 3_** | `transfer_money.patch` |  **5** / 5 _pkt_  |
| **_zadanie 4_** | `max_before_deadline.patch` | **3.8** / 5 _pkt_ |
| **_zadanie 5_** | `exclusive.patch` | **5** / 5 _pkt_ |