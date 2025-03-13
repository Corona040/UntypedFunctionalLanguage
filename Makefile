NAME = LC

SRC = LambdaCalculus.hs
OBJ = LambdaCalculus.o
HI =  LambdaCalculus.hi

all: $(NAME)

$(NAME): $(SRC)
	ghc $< -package mtl -package containers -o $(NAME)

clean:
	rm -rf $(OBJ)

fclean: clean
	rm -rf $(HI) $(NAME)

re: fclean all

.PHONY: all clean fclean re
