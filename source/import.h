#include <errno.h>
#include <string.h>
#include <iconv.h>
#if defined(__APPLE__)
#pragma for Ada overload size_t iconv (iconv_t cd, \
	const char **inbuf, size_t *inbytesleft, \
	char **outbuf, size_t *outbytesleft)
#pragma for Ada "errno.h" include "sys/errno.h"
#endif
