#include <errno.h>
#include <string.h>
#include <iconv.h>

#if defined(__APPLE__)
#pragma for Ada overload size_t iconv (iconv_t cd, \
	char const **inbuf, size_t *inbytesleft, \
	char **outbuf, size_t *outbytesleft)
#pragma for Ada "errno.h" include "sys/errno.h"
#elif defined(__linux__)
#pragma for Ada overload size_t iconv (iconv_t __cd, \
	char const ** restrict __inbuf, size_t * restrict __inbytesleft, \
	char ** restrict __outbuf, size_t * restrict __outbytesleft)
#pragma for Ada "errno.h" include "bits/errno.h"
#pragma for Ada "errno.h" include "asm-generic/errno.h" /* EILSEQ */
#pragma for Ada "errno.h" include "asm-generic/errno-base.h" /* EINVAL */
#endif
