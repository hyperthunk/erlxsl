# starting point makefile - probably needs a lot of improvement
# (e.g. ./configure support would be a start!).

# Environment
MKDIR=mkdir
CP=cp
CC=gcc

# Driver source code directory
DRIVERDIR=./driver

# Object files folder
OBJECTDIR=../priv/obj

# Bin folder
BINDIR=../priv/bin


# Implementation provider name
PROVIDER=test

# Object Files
OBJECTFILES= \
	${OBJECTDIR}/driver.o \
	${OBJECTDIR}/protocol.o

# Binary output file (NB: really need ./configure here, esp. as .dll is platform specific)
BINFILE=erlxsl.so

OUTPUT=${BINDIR}/${BINFILE}

OTP_TOP=/usr/local
ERL_TOP=${OTP_TOP}/lib/erlang
ERTS=${ERL_TOP}/erts-5.6.5

# C Compiler includes options
CINCLUDES=-I ${ERL_TOP}/usr/include -I./includes

DEBUG=-DDEBUG

# C Compiler Flags
CFLAGS=-Wall -fPIC ${DEBUG} ${CINCLUDES}

# removed -g from CFLAGS

# Linker flags
LDFLAGS=-unresolved-symbols=ignore-in-object-files -shared

# Linker Libs options
XLDLIBS= \
        -L${ERL_TOP}/usr/lib \
	-L${ERTS}/lib \
	-L${ERL_TOP}/lib \
	-L/media/neon/home/code/open-source/erlxsl/priv/bin \
	-Wl,-rpath ${ERL_TOP}/lib \
	-Wl,-rpath ${ERL_TOP}/usr/lib \
	-Wl,-rpath /media/neon/home/code/open-source/erlxsl/priv/bin

build: build-provider compile link

# include project implementation makefile
build-provider:
	cd providers/${PROVIDER} && ${MAKE} -f makefile all

compile: ${BINDIR}/xslprovider.so
	$(COMPILE.c) -o ${OBJECTDIR}/protocol.o ${DRIVERDIR}/protocol.c
	$(COMPILE.c) -o ${OBJECTDIR}/driver.o ${DRIVERDIR}/driver.c

link:
	${LINK.c} -shared -unresolved-symbols=ignore-in-object-files -o ${OUTPUT} ${OBJECTFILES} ${XLDLIBS} -dynamic

clean:
	cd ./providers/${PROVIDER} && ${MAKE} -f makefile clean
	${RM} -r ${BINDIR}/erlxsl.so
	${RM} -r ${OBJECTFILES}

