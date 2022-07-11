TARGET = PassQ
CC     = gfortran
SRCS   = src/main.f90

PassQ:
	$(CC) $(SRCS) -o $(TARGET) -Wall

clean:
	rm -fr $(TARGET)
