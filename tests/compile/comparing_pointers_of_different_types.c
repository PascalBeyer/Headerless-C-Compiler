
void* my_memmove(void* dest, const void* src, unsigned __int64 count)
{
    char *dest_ = dest, *src_ = (char *)src;
    if ((char*)src + count > dest && src < dest) 
    {
        dest_ += (src_ += count - 1, count - 1);
        while (count--) *dest_-- = *src_--;
    }
    else while (count--) *dest_++ = *src_++;
    return dest;
}

int main(){
    
}
