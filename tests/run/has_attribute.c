// run

// __has_attribute has to work both directly and through macro aliases. glibc
// uses the latter in sys/cdefs.h as `#define __glibc_has_attribute(attr) __has_attribute (attr)`.

#define HAS_ATTRIBUTE(x) __has_attribute(x)
#define HAS_ATTRIBUTE_ALIAS(x) HAS_ATTRIBUTE(x)

#ifndef __has_attribute
#error __has_attribute is not defined
#endif

#if !__has_attribute(cleanup)
#error cleanup was not detected
#endif

#if !HAS_ATTRIBUTE(cleanup)
#error cleanup was not detected through a macro alias
#endif

#if !HAS_ATTRIBUTE_ALIAS(cleanup)
#error cleanup was not detected through a nested macro alias
#endif

#if __has_attribute(not_a_real_attribute)
#error an unknown attribute was detected
#endif

#if HAS_ATTRIBUTE(not_a_real_attribute)
#error an unknown attribute was detected through a macro alias
#endif

int main(void) {
    return 0;
}
