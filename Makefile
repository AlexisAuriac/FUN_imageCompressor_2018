##
## EPITECH PROJECT, 2018
## imageCompressor
## File description:
## Makefile for imageCompressor.
##

NAME	=	imageCompressor

all:
	make $(NAME)

$(NAME):
	stack build
	cp `stack path --local-install-root`/bin/$(NAME) .

.PHONY:	all $(NAME)

