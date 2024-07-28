#include "types.h"
#include <spa/utils/dict.h>
#include <stdlib.h>

// SpaDictItem

struct spa_dict_item* new_spa_dict_item(const char* key, const char* value)
{
    struct spa_dict_item* ptr = malloc(sizeof(struct spa_dict_item));
    ptr->key = key;
    ptr->value = value;
    return ptr;
}

void free_spa_dict_item(struct spa_dict_item* ptr)
{
    free(ptr);
}

const char* spa_dict_item_key(struct spa_dict_item* ptr)
{
    return ptr->key;
}

const char* spa_dict_item_value(struct spa_dict_item* ptr)
{
    return ptr->value;
}


// SpaDict

struct spa_dict* new_spa_dict(uint32_t flags, uint32_t n_items, const struct spa_dict_item* items)
{
    struct spa_dict* ptr = malloc(sizeof(struct spa_dict));
    ptr->flags = flags;
    ptr->n_items = n_items;
    ptr->items = items;
    return ptr;
}

void free_spa_dict(struct spa_dict* ptr)
{
    free(ptr);
}
