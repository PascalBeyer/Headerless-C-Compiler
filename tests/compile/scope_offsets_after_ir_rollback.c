// compile

/* The constant false condition in ROUND_DOWN discards the true branch after it
 * has been parsed. That branch contains a statement expression, so rolling back
 * only the IR arena used to leave the statement-expression scope end pointing
 * into the following code. The parser must roll back the scope offsets too.
 */

typedef long int64_t;

long g(long value)
{
    return value;
}

#define MAX_INTERNAL(a, b, _a, _b)                      \
    ({                                                  \
        typeof(1 ? (a) : (b)) _a = (a), _b = (b);       \
        _a > _b ? _a : _b;                              \
    })
#define MAX(a, b) MAX_INTERNAL((a), (b), _a, _b)

#define ROUND_DOWN(n, d) ((n) & -(0 ? (n) : (d)))
#define ROUND_UP(n, d) ROUND_DOWN((n) + (d) - 1, (d))

long f(int64_t total_bytes, int64_t offset, int64_t align, int64_t bytes)
{
    int64_t max_bytes;
    long ret = 0;

    max_bytes = ROUND_UP(MAX(0, total_bytes - offset), align);
    if (bytes <= max_bytes && bytes <= 12) {
        ret = g(bytes);
        goto out;
    }

    return 0;

out:
    return ret;
}

int main(void)
{
    return f(100, 0, 8, 8) == 8 ? 0 : 1;
}
