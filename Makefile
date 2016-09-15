#  Makefile.defs for CLIM 2.0

# where to dump the CLIM images
CLIM		= clim.dxl

SHARED_EXT = so

LISP	= alisp
DCL	= ~/dev/acl100express/alisp.dxl

# Set the case mode when building and dumping an image
# case_mode:
#	ansi -- traditional case-insensitive upper-case CL
#	modern -- case-sensitive lower-case CL

# This controls whether patches are loaded before dumping an image.
# They should be loaded except during development
# load_patches:
#	yes -- load them
#	no -- don't load
load_patches = yes

ifeq ($(load_patches),no)
STOP_PATCHES = '(pushnew :clim-dont-load-patches *features*)'
endif

CL	= $(LISP) -I $(DCL)
CLOPTS	= -qq -batch -backtrace-on-error -locale japan.euc

RM	= rm
CAT	= cat
ECHO	= echo
TAGS	= etags

SHELL	= sh

# Lisp optimization for compiling
SPEED	?= 3
SAFETY	?= 1
# This next should be set to 1 for distribution
DEBUG   ?= 1

# Training

TRAIN_TIMES=1
# view, file, print
PSVIEW=:file
HPGLVIEW=:file
TRAIN_COMPILE=t
TRAIN_PROFILEP=t
TRAIN_BM=t
FRAME_TESTS=t

ifneq ($(OS_NAME),windows)
CFLAGS	= -O -D_NO_PROTO -DSTRINGS_ALIGNED -DNO_REGEX -DNO_ISDIR \
	 	-DUSE_RE_COMP -DUSER_GETWD
endif

# Used for tags
ALL_SRC = *.lisp */*.lisp *.cl */*.cl *.c */*.c *.h */*.h

# These are the files that make up the source code product.
PRODUCT_SRC_FILES = sys/*.lisp utils/*.lisp  silica/*.lisp clim/*.lisp \
	demo/*.lisp test/test-suite.lisp


# This has to be kept consistent with xlib/xlib-funs.lisp
UNDEFS=misc/undefinedsymbols

# This has to be kept consistent with tk/xt-funs.lisp
XT_UNDEFS=misc/undefinedsymbols.xt

# This has to be kept consistent with tk/xm-funs.lisp
XM_UNDEFS=misc/undefinedsymbols.motif
# This has to be kept consistent with tk/xm-classes.lisp
XMC_UNDEFS=misc/undefinedsymbols.cmotif

# This has to be kept consistent with tk/ol-funs.lisp
OL_UNDEFS=misc/undefinedsymbols.olit
# This has to be kept consistent with tk/ol-classes.lisp
OLC_UNDEFS=misc/undefinedsymbols.colit

# This has to be kept consistent with wnn/jl-funs.lisp
WNN_UNDEFS=misc/undefinedsymbols.wnn

# These are the fasls and the .o that form the product

PRODUCT-GENERIC-FASLS = \
	climg.fasl climdemo.fasl clim-debug.fasl climps.fasl \
	climhpgl.fasl # climgg.fasl

PRODUCT-XM-FASLS = climxm.fasl clim-debugxm.fasl
PRODUCT-OL-FASLS = climol.fasl clim-debugol.fasl

PRODUCT-WNN-FASLS = climwnn.fasl clim-debugwnn.fasl

PRODUCT-GENERIC-OBJS= \
	stub-xt.o stub-x.o xtsupport.o xlibsupport.o

STATIC-XM-OBJS= stub-motif.o xmsupport.o
SHARED-XM-OBJS= climxm.$(SHARED_EXT)

STATIC-OL-OBJS= stub-olit.o olsupport.o
SHARED-OL-OBJS= climol.$(SHARED_EXT)

WNNLIB = libwnn.a
STATIC-WNN-OBJS=stub-wnn.o $(WNNLIB)

SYSTEM=		motif-clim

PRODUCT-FASLS=  $(PRODUCT-GENERIC-FASLS) $(PRODUCT-XM-FASLS)
PRODUCT-OBJS=	$(PRODUCT-GENERIC-OBJS) $(STATIC-XM-OBJS)

ICS-PRODUCT-FASLS= $(PRODUCT-WNN-FASLS)
ICS-PRODUCT-OBJS=  $(STATIC-WNN-OBJS)

Makefile=Makefile

ifeq ($(shell if test -d /usr/include/openmotif; then echo yes; fi),yes)
XINCLUDES = -I/usr/include/openmotif
XLIBDIR   = /usr/lib/openmotif
endif

# This is the old location, but include it here, just in case
ifeq ($(shell if test -d /usr/X11R6/include; then echo yes; fi),yes)
XINCLUDES = -I/usr/X11R6/include
XLIBDIR   = /usr/X11R6/lib
endif

XINCLUDES ?= /usr/include
XLIBDIR   ?= /usr/lib

TKLIB=-lXm -lXpm -lXext -lXp
XTLIB=-lXt
XLIB=-lX11

ifdef FI_USE_DMALLOC
THREADLIB = -lpthread -ldmallocth
CFLAGS = -I/usr/local/include
else
THREADLIB = -lpthread
endif

SET_LIBRARY_PATH = LD_RUN_PATH=$(XLIBDIR):/lib:/usr/lib; export LD_RUN_PATH

PRODUCT-OBJS= $(PRODUCT-GENERIC-OBJS) $(STATIC-XM-OBJS) $(SHARED-XM-OBJS)

#MOTIFXTRAS=-lgen

#PICFLAGS = -K pic
SHAREFLAGS =
MAKE_SHARED = ld -shared -L$(XLIBDIR)
STD_DEFINES = -DSVR4 -DSYSV
AR = ar cq

include Makefile.generic
