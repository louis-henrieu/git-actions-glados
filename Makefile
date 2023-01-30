##
## EPITECH PROJECT, 2023
## glados
## File description:
## Makefile
##

PATH_BIN	:=	$(shell stack path --local-install-root)

RM		=		rm -rf

NAME	=		glados

all:			$(NAME)

$(NAME):
				stack build
				cp -f $(PATH_BIN)/bin/$(NAME)-exe ./$(NAME)

debug:
				stack build -v
				cp -f $(PATH_BIN)/bin/$(NAME)-exe ./$(NAME)

clean:
				stack clean

flcean:			clean
				$(RM) $(NAME)

re:				flcean all

tests_run:
#				stack test --coverage
				stack hpc report --all --destdir test/coverage

.PHONY: 		all $(NAME) clean fclean re tests_run