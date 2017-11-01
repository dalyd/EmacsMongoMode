#include "helper.h"

bool cursor_next_bson_wrap(mongoc_cursor_t *cursor, bson_t *doc){
   bson_error_t error;
   const bson_t *return_doc;
   if (mongoc_cursor_next (cursor, &return_doc))
       {
           bson_copy_to(return_doc, doc);
           return true;
   }
   return false;

}
