// run
// os linux

// System V passes a 16-byte struct in two GPRs. When the struct is a member
// of a pointer-based expression, the first eightbyte must not be loaded into
// the register that holds the base pointer. The old codegen allocated the
// destination register after emitting the load, so the second eightbyte used
// the loaded value as its base. The second call below is the one that hits it.

typedef long timediff_t;

struct curltime {
    long tv_sec;
    int tv_usec;
};

struct Curl_tree {
    struct Curl_tree *smaller;
    struct Curl_tree *larger;
    struct Curl_tree *samen;
    struct Curl_tree *samep;
    struct curltime key;
};

timediff_t Curl_timediff_us(struct curltime newer, struct curltime older)
{
    timediff_t diff = (timediff_t)newer.tv_sec - older.tv_sec;
    return diff * 1000000 + newer.tv_usec - older.tv_usec;
}

struct Curl_tree *Curl_splay(struct curltime i, struct Curl_tree *t)
{
    timediff_t comp = Curl_timediff_us(i, t->key);
    if(comp < 0) {
        if(!t->smaller)
            return 0;
        Curl_timediff_us(i, t->smaller->key);
    }

    return t;
}

static struct Curl_tree smaller = {0, 0, 0, 0, {20, 0}};
static struct Curl_tree node = {&smaller, 0, 0, 0, {10, 20}};

int main(void) {
    struct curltime zero = {0, 0};

    Curl_splay(zero, &node);

    return 0;
}
