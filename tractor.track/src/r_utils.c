#include "r_utils.h"

#include <R.h>
#include <Rdefines.h>

SEXP get_list_element (SEXP list, const char *name)
{
    SEXP element = R_NilValue;
    SEXP names = getAttrib(list, R_NamesSymbol);
     
    for (R_len_t i = 0; i < length(list); i++)
    {
        if (strcmp(CHAR(STRING_ELT(names,i)), name) == 0)
        {
            element = VECTOR_ELT(list, i);
            break;
        }
    }
    
    return element;
}
