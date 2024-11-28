// compile /Wdeclaration_differs_in_attribute
// check "'a': [0] Redeclaration differs in __declspec(dllexport) attribute."
// check "'b': [0] Redeclaration differs in __declspec(dllexport) attribute."
// check "'c': [0] Redeclaration differs in __declspec(selectany) attribute."
// check "'d': [0] Redeclaration differs in __declspec(selectany) attribute."
// check "'e': [0] Redeclaration differs in __declspec(dllimport) attribute."
// check "'f': [0] Redeclaration differs in __declspec(dllimport) attribute."

int a;
__declspec(dllexport) int a; // warning: Redeclaration differs int __declspec(dllexport) attribute.
__declspec(dllexport) int b;
int b;                       // warning: Redeclaration differs int __declspec(dllexport) attribute.
int c;
__declspec(selectany) int c; // warning: Redeclaration differs int __declspec(selectany) attribute.
__declspec(selectany) int d;
int d;                       // warning: Redeclaration differs int __declspec(selectany) attribute.
int e;
__declspec(dllimport) int e; // warning: Redeclaration differs int __declspec(dllimport) attribute.
__declspec(dllimport) int f;
int f;                       // warning: Redeclaration differs int __declspec(dllimport) attribute.

int main(){}
