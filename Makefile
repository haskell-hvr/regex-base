TOP=..
include $(TOP)/mk/boilerplate.mk

SUBDIRS = 

ALL_DIRS = \
	Text/Regex \
	Text/Regex/Base

PACKAGE 	= regex-base
VERSION 	= 0.71
PACKAGE_DEPS 	= base

EXCLUDED_SRCS	= Setup.hs

SRC_HADDOCK_OPTS += -t "Haskell Hierarchical Libraries ($(PACKAGE) package)"

include $(TOP)/mk/target.mk
