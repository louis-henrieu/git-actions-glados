##
## EPITECH PROJECT, 2023
## glados
## File description:
## Makefile
##

PATH_BIN	:=	$(shell stack path --local-install-root)

RM		=		rm -rf

FT_NAME	=		test_glados.sh

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
				$(RM) src/*.hi src/*.o

flcean:			clean
				$(RM) test/coverage
				$(RM) $(NAME)

re:				flcean all

tests_run:
				stack test --coverage
				stack hpc report --all --destdir test/coverage
#				microsoft-edge ./test/coverage/hpc_index.html

func_test:
				cp -f ./test/$(FT_NAME) ./
				chmod +x $(FT_NAME)
				./$(FT_NAME)
				$(RM) $(FT_NAME)

.PHONY: 		all $(NAME) clean fclean re tests_run func_test