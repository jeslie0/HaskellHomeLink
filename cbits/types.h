#ifndef PIPEWIRE_TYPES_HS
#define PIPEWIRE_TYPES_HS

#include <spa/utils/dict.h>
#include <stdio.h>

// SpaDictItem

/* struct spa_dict_item* new_spa_dict_item(const char* key, const char* value); */

/* void free_spa_dict_item(struct spa_dict_item* ptr); */

/* const char* spa_dict_item_key(struct spa_dict_item* ptr); */

/* const char* spa_dict_item_value(struct spa_dict_item* ptr); */

/* // SpaDict */

/* struct spa_dict* new_spa_dict(uint32_t flags, uint32_t n_items, const struct spa_dict_item* items); */

/* void free_spa_dict(struct spa_dict* ptr); */


void check_spa_item(struct spa_dict_item* item)
{
    printf("key: %s, value: %s\n", item->key, item->value);
}


#endif
