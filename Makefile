TARGET = PassQ
CC     = gfortran
SRCS   = src/main.cpp

PassQ:
	$(CC) $(SRCS) -o $(TARGET) -Wall

clean:
	rm -fr $(TARGET)
