PROJECT = list_test

CC = gcc
CFLAGS += -std=c99 -pedantic -Wall -Wextra -Werror
LDFLAGS +=

SRC = $(PROJECT).c
OBJ = list.o

all: $(PROJECT)

debug: CFLAGS += -g
debug: $(PROJECT)

%.o: %.c %.h
	$(CC) $(CFLAGS) -c $<

$(PROJECT): $(OBJ)
	$(CC) $(SRC) $(OBJ) $(LDFLAGS) -o $(PROJECT)

clean:
	rm $(PROJECT) *.o

