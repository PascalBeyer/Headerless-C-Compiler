
// repro of weird struct inclusion in 'oaidl.h'

typedef struct _wireVARIANT *wireVARIANT; // line 278

typedef struct _wireSAFEARR_VARIANT // line 300
{
    int Size;
    wireVARIANT *aVariant;
} 	SAFEARR_VARIANT;

typedef struct _wireSAFEARRAY_UNION{ // line 335
    int type;
    union __MIDL_IOleAutomationTypes_0001{
        SAFEARR_VARIANT VariantStr; 
    } u; 
} SAFEARRAYUNION;


struct _wireVARIANT{ // line 565
    int size;
    union{
        wireVARIANT *pvarVal;  // line 604
    } u;
};


// type_stack:
//    [0] = struct _wireSAFEARR_VARIANT
//    [1] = pointer to wireVariant
//    [2] = pointer to struct _wireVARIANT
//    [3] = struct _wireVARIANT
//    [4] = union <anonymous>
//    [5] = pointer to wireVARIANT
// next one would be 
//    [6] = pointer to struct _wireVARIANT
// but this one is already marked 'tempoary'


int main(){
   return 0; 
}
