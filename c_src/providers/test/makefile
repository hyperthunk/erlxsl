# ....
.SUFFIXES: .c .o

# Environment
MKDIR=mkdir
CP=cp
CC=gcc

# Object Directory
OBJECTDIR=../../../priv/obj

# Bin Directory
BINDIR=../../../priv/bin

OPT_TOP=/usr/local
ERL_TOP=${OTP_TOP}/lib/erlang

# C Compiler includes options
CINCLUDES=-I./../../includes -I${EXTERN_DIR}/include

DEBUG=-DDEBUG

# C Compiler Flags
CFLAGS=${DEBUG} -Wall ${CINCLUDES} -fPIC

# Linker flags
LDFLAGS=-shared

# Linker Libs options
LDLIBS= \
	-Wl,-L/usr/lib \
	-Wl,-rpath /usr/lib
	#-lexpat -lsablot -dynamic

# Provider source file
# NB: this is pretty limited. Maybe it needs a rethink!?
PROVIDER_IMPL=test_provider.c

PROVIDER_OUTPUT=xslprovider.so

all: clean build

build: compile link

compile: test_provider.c
	${CC} -c ${CFLAGS} -o ${OBJECTDIR}/xslprovider.o test_provider.c

link: test_provider.c
	${CC} -o ${BINDIR}/${PROVIDER_OUTPUT} -fPIC ${OBJECTDIR}/xslprovider.o ${LDFLAGS} ${LDLIBS}

clean:
	${RM} -r ${OBJECTDIR}/xslprovider.o
	${RM} -r ${BINDIR}/xslprovider.so
