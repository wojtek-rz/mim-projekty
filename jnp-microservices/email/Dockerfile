# Budujemy obraz na bazie obrazu golang
FROM golang:1.20

# Ustawiamy katalog roboczy wewnątrz kontenera
WORKDIR /app

# Kopiujemy pliki źródłowe projektu do katalogu roboczego
COPY ./code/* .

# Kompilujemy kod Go
RUN go build -o users_app
CMD ["./users_app"]