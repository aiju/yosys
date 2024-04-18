#include <array>

template<size_t n>
using Signal = std::array<bool, n>;

template<size_t n, size_t m>
Signal<n> slice(Signal<m> const& a, size_t offset)
{
    Signal<n> ret;

    std::copy(a.begin() + offset, a.begin() + offset + n, ret.begin());
    return ret;
}

template<size_t n>
Signal<n> $const(int val)
{
    size_t i;
    Signal<n> ret;

    for(i = 0; i < n; i++)
        ret[i] = val & (1<<i);
    return ret;
}

template<size_t n>
bool as_bool(Signal<n> sig)
{
    for(int i = 0; i < n; i++)
        if(sig[i])
            return true;
    return false;
}

template<size_t n>
int as_int(Signal<n> sig)
{
    int ret = 0;
    for(int i = 0; i < n; i++)
        if(sig[i])
            ret |= 1<<i;
    return ret;
}

template<size_t n>
Signal<n> $mux(Signal<n> const& a, Signal<n> const &b, Signal<1> const &s)
{
    return s[0] ? b : a;
}

template<size_t n>
Signal<1> $not(Signal<n> const& a)
{
    return { !as_bool(a) };
}

template<size_t n>
Signal<1> $reduce_or(Signal<n> const& a)
{
    return { as_bool(a) };
}

template<size_t n>
Signal<1> $reduce_and(Signal<n> const& a)
{
    size_t i;
    for(i = 0; i < n; i++)
        if(!a[i])
            return { false };
    return { true };
}

template<size_t n>
Signal<1> $reduce_bool(Signal<n> const& a)
{
    return { as_bool(a) };
}

template<size_t n>
Signal<1> $logic_and(Signal<n> const& a, Signal<n> const& b)
{
    return { as_bool(a) && as_bool(b) };
}

template<size_t n>
Signal<1> $logic_or(Signal<n> const& a, Signal<n> const& b)
{
    return { as_bool(a) || as_bool(b) };
}

template<size_t n>
Signal<1> $logic_not(Signal<n> const& a)
{
    return { !as_bool(a) };
}

template<size_t n>
Signal<n> $add(Signal<n> const& a, Signal<n> const &b)
{
    Signal<n> ret;
    size_t i;
    int x = 0;
    for(i = 0; i < n; i++){
        x += (int)a[i] + (int)b[i];
        ret[i] = x & 1;
        x >>= 1;
    }
    return ret;
}
template<size_t n>
Signal<n> $sub(Signal<n> const& a, Signal<n> const &b)
{
    Signal<n> ret;
    int x = 1;
    for(size_t i = 0; i < n; i++){
        x += (int)a[i] + (int)!b[i];
        ret[i] = x & 1;
        x >>= 1;
    }
    return ret;
}

template<size_t n>
Signal<1> $uge(Signal<n> const& a, Signal<n> const &b)
{
    for(size_t i = n; i-- != 0; )
        if(a[i] != b[i])
            return { a[i] };
    return { true };
}

template<size_t n>
Signal<1> $ugt(Signal<n> const& a, Signal<n> const &b)
{
    for(size_t i = n; i-- != 0; )
        if(a[i] != b[i])
            return { a[i] };
    return { false };
}

template<size_t n>
Signal<1> $ge(Signal<n> const& a, Signal<n> const &b)
{
    if(a[n-1] != b[n-1])
        return { b[n-1] };
    return $uge(a, b);
}

template<size_t n>
Signal<1> $gt(Signal<n> const& a, Signal<n> const &b)
{
    if(a[n-1] != b[n-1])
        return { b[n-1] };
    return $ugt(a, b);
}

template<size_t n> Signal<1> $ule(Signal<n> const& a, Signal<n> const &b) { return $uge(b, a); }
template<size_t n> Signal<1> $ult(Signal<n> const& a, Signal<n> const &b) { return $ugt(b, a); }
template<size_t n> Signal<1> $le(Signal<n> const& a, Signal<n> const &b) { return $ge(b, a); }
template<size_t n> Signal<1> $lt(Signal<n> const& a, Signal<n> const &b) { return $gt(b, a); }

template<size_t n>
Signal<n> $and(Signal<n> const& a, Signal<n> const &b)
{
    Signal<n> ret;
    for(size_t i = 0; i < n; i++)
        ret[i] = a[i] && b[i];
    return ret;
}

template<size_t n>
Signal<n> $or(Signal<n> const& a, Signal<n> const &b)
{
    Signal<n> ret;
    for(size_t i = 0; i < n; i++)
        ret[i] = a[i] || b[i];
    return ret;
}

template<size_t n>
Signal<n> $xor(Signal<n> const& a, Signal<n> const &b)
{
    Signal<n> ret;
    for(size_t i = 0; i < n; i++)
        ret[i] = a[i] != b[i];
    return ret;
}

template<size_t n, size_t na, size_t nb>
Signal<n> $shl(Signal<na> const& a, Signal<nb> const &b)
{
    if(nb >= sizeof(int) * 8 - 1)
        for(size_t i = sizeof(int) * 8 - 1; i < nb; i++)
            assert(!b[i]);
    size_t amount = as_int(b);
    Signal<n> ret = $const<n>(0);
    if(amount < n){
        if(amount + na > n)
            std::copy(a.begin(), a.begin() + (n - amount), ret.begin() + amount);
        else
            std::copy(a.begin(), a.end(), ret.begin() + amount);
    }
    return ret;
}

template<size_t n>
Signal<1> $eq(Signal<n> const& a, Signal<n> const &b)
{
    for(size_t i = 0; i < n; i++)
        if(a[i] != b[i])
            return { false };
    return { true };
}

template<size_t n>
Signal<1> $ne(Signal<n> const& a, Signal<n> const &b)
{
    for(size_t i = 0; i < n; i++)
        if(a[i] != b[i])
            return { true };
    return { false };
}

template<size_t n, size_t ns>
Signal<n> $pmux(Signal<n> const& a, Signal<n*ns> const &b, Signal<ns> const &s)
{
    bool found;
    Signal<n> ret;

    found = false;
    ret = a;
    for(size_t i = 0; i < ns; i++){
        if(s[i]){
            if(found)
                return $const<n>(0);
            found = true;
            ret = slice<n>(b, n * i);
        }
    }
    return ret;
}

template<size_t n, typename... Ts>
Signal<n> concat(Signal<n> const& a)
{
    return a;
}

template<size_t n, typename... Ts>
auto concat(Signal<n> const& a, Ts... args)
{
    auto b = concat(args...);
    Signal<n + b.size()> ret;
    std::copy(b.begin(), b.end(), ret.begin() + a.size());
    std::copy(a.begin(), a.end(), ret.begin());
    return ret;
}
