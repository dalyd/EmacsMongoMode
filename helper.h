#include <bson.h>
#include <mongoc.h>

#ifndef MONGO_FFI_HELPER
#define MONGO_FFI_HELPER

extern bool cursor_next_bson_wrap(mongoc_cursor_t *cursor, bson_t *doc);

#endif // MONGO_FFI_HELPER
