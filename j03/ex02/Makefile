############################### OCAML MAKEFILE ################################
###############################################################################
# Exec name

NAME = ft_Cipher

# srcs
SOURCES = *.ml

# Compiler
CAMLC = ocamlc
CAMLOPT = ocamlopt
CAMLDEP = ocamldep

#Lib
LIBS = $(WITHGRAPHICS)
WITHGRAPHICS = graphics.cma -cclib -lGraphics

#################################### RULES ####################################

# Dep
all: depend $(NAME)

# Link
$(NAME) : opt byt
	ln -s $(NAME).byt $(NAME)

# compile rules
opt: $(NAME).opt
byt: $(NAME).byt

# obj
OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(SOURCES:.ml=.cmx)

#################################### LINK #####################################

# name

$(NAME).byt:	$(OBJS)
		$(CAMLC) -o $(NAME).byt $(LIBS) $(OBJS)

$(NAME).opt:	$(OPTOBJS)
		$(CAMLOPT) -o $(NAME).opt $(LIBS:.cma=.cmxa) $(OPTOBJS)

################################# COMPILING ####################################

# erase suffixes

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

# srcs to objs

.ml.cmo:
	$(CAMLC) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) -c $<

################################## CLEANING ####################################

clean:
	rm -f *.cm[iox] *~ .*~ #*#
	rm -f $(NAME).o

fclean: clean
	rm -f $(NAME)
	rm -f $(NAME).opt
	rm -f $(NAME).byt

depend: .depend
	$(CAMLDEP) $(SOURCES) > .depend

re: fclean all

include .depend