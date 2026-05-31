/* config.h.  Generated from config.h.in by configure.  */
/* config.h.in.  Generated from configure.ac by autoheader.  */

/* Define if building universal (internal helper macro) */
/* #undef AC_APPLE_UNIVERSAL_BUILD */

/* Enable debugging mode */
#define DEBUG 1

/* Enable full F2003 features (gfortran 4.9) */
/* #undef F2003_FULL_FEATURES */

/* Define to dummy `main' function (if any) required to link to the Fortran
   libraries. */
/* #undef FC_DUMMY_MAIN */

/* Define if F77 and FC dummy `main' functions are identical. */
/* #undef FC_DUMMY_MAIN_EQ_F77 */

/* Define to a macro mangling the given C identifier (in lower and upper
   case), which must not contain underscores, for linking with Fortran. */
#define FC_FUNC(name,NAME) name ## _

/* As FC_FUNC, but for C identifiers containing underscores. */
#define FC_FUNC_(name,NAME) name ## _

/* Enable dballe */
#define HAVE_DBALLE 1

/* Define to 1 if you have the <dlfcn.h> header file. */
#define HAVE_DLFCN_H 1

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Enable gdal */
#define HAVE_LIBGDAL 1

/* Enable gribapi code */
#define HAVE_LIBGRIBAPI 1

/* Enable log4c */
#define HAVE_LIBLOG4C 1

/* Enable ncar graphics code */
#define HAVE_LIBNCARG 1

/* Enable netcdf code */
#define HAVE_LIBNETCDF 1

/* Enable ngmath code */
#define HAVE_LIBNGMATH 1

/* Define to 1 if you have the <log4c.h> header file. */
#define HAVE_LOG4C_H 1

/* Enable shapelib */
#define HAVE_SHAPELIB 1

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdio.h> header file. */
#define HAVE_STDIO_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define to the sub-directory where libtool stores uninstalled libraries. */
#define LT_OBJDIR ".libs/"

/* Name of package */
#define PACKAGE "libsim"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "dcesari@arpae.it"

/* Define to the full name of this package. */
#define PACKAGE_NAME "libsim"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "libsim 7.2.6"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "libsim"

/* Define to the home page for this package. */
#define PACKAGE_URL ""

/* Define to the version of this package. */
#define PACKAGE_VERSION "7.2.6"

/* macro for pasting two macros without blanks */
#define PASTE(a,b) a/**/b

/* 2nd half of macro for pasting two macros without blanks */
/* #undef POSTPASTE */

/* install prefix */
#define PREFIX "/scratch/dcesari/github/install"

/* Fortran KIND defining an INTEGER containing a C pointer */
#define SIZEOF_PTR_C int_ll

/* The size of `size_t', as computed by sizeof. */
#define SIZEOF_SIZE_T 8

/* The size of `void *', as computed by sizeof. */
#define SIZEOF_VOID_P 8

/* Define to 1 if all of the C90 standard headers exist (not just the ones
   required in a freestanding environment). This macro is provided for
   backward compatibility; new code need not use it. */
#define STDC_HEADERS 1

/* Enable vapor code */
/* #undef VAPOR */

/* Version number of package */
#define VERSION "7.2.6"

/* Define WORDS_BIGENDIAN to 1 if your processor stores words with the most
   significant byte first (like Motorola and SPARC, unlike Intel). */
#if defined AC_APPLE_UNIVERSAL_BUILD
# if defined __BIG_ENDIAN__
#  define WORDS_BIGENDIAN 1
# endif
#else
# ifndef WORDS_BIGENDIAN
/* #  undef WORDS_BIGENDIAN */
# endif
#endif
