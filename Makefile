GHC=/usr/bin/ghc
NAME=nfa2dfa
FILE=Main
RM=rm
TEST=input.txt

all:
	$(GHC) $(FILE).hs -o $(NAME)

run: all
	./$(NAME)

clean:
	$(RM) $(FILE).hi
	$(RM) $(FILE).hs
	$(RM) $(FILE).o
	$(RM) $(NAME)

test:
	./$(NAME) < $(TEST)