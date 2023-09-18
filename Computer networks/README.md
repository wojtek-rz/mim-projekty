# Radio internetowe

Radio internetowe z obsługą wielu stacji oraz wielu odbiorników działające w opraciu o adresy rozgłoszeniowe.\
Komunikacja odbywa się za pomocą protokołu UDP (dane audio) oraz TCP (sterowanie). \
Paczki audio są w następującym formacie:
```
 0                                63
+-----------------------------------+
|            Session id             |
+-----------------------------------+
|          First byte num           |
+-----------------------------------+
|          data octets ...          |
+---------------- ...
```

Do przykładów użycia potrzebne są programy `sox` oraz `pv` do odczytywania i strumieniowania muzyki.

## Budowanie
```
mkdir build
cd build
cmake ..
make
```

## Sender

```
Allowed options:
  --help                                produce help message
  -C [ --CTRL_PORT ] arg (=38709)       control port for UDP connection
  -R [ --RTIME ] arg (=250)             retransmission time
  -a [ --MCAST_ADDR ] arg (=239.10.11.12)
                                        multicast address for sending audio 
                                        data
  -P [ --DATA_PORT ] arg (=28709)       data port for UDP connection
  -p [ --PSIZE ] arg (=512)             number of audio data bytes in one 
                                        package
  -f [ --FSIZE ] arg (=131072)          sender buffer size
  -n [ --NAME ] arg (=Nienazwany Nadajnik)
                                        name of the radio station
```

Przykład użycia:
```
sox -S "melody.wav" -r 44100 -b 16 -e signed-integer -c 2 -t raw - | pv -q -L $((44100 * 4)) | ./sikradio-sender -a "239.10.11.12" -n "Shady Radio"
```

## Receiver
```
Allowed options:
  --help                                produce help message
  -C [ --CTRL_PORT ] arg (=38709)       control port for UDP connection
  -R [ --RTIME ] arg (=250)             retransmission time
  -d [ --DISCOVER_ADDR ] arg (=255.255.255.255)
                                        address for discovering radio stations
  -U [ --UI_PORT ] arg (=18709)         TCP port for text control interface
  -b [ --BSIZE ] arg (=65536)           size of buffer
  -n [ --NAME ] arg                     name of the radio station
```
Przykład użycia:
```
./sikradio-receiver | play -t raw -c 2 -r 44100 -b 16 -e signed-integer --buffer $((128)) -
```

## Text interface
To connect to receiver interface use telnet:
```
telnet localhost 18709
```
The interface looks like this:
```
------------------------------------------------------------------------

 SIK Radio

------------------------------------------------------------------------

 > Radio2

Shady Radio

Warsaw Radio

------------------------------------------------------------------------
```