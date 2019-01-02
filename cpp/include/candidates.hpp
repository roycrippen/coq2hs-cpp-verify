#include <vector>

using namespace std;

namespace cn {
    /// square a number
    int square(int a) {
        return a * a;
    }

    /// do a,b, c form a pythagorean triple
    bool is_pythagorean_triple(int a, int b, int c) {
        return square(a) + square(b) == square(c);
    }

    /// XOR Encryption
    string applyXorCipher(string const &cs, string const &key) {
        string res = cs;
        auto mod = key.size() / sizeof(char);
        for (int i = 0; i < cs.size(); i++) {
            auto c = key[i % mod];
            res[i] = cs[i] ^ c;
        }
        return res;
    }

    /// pretty string of a const vector<T>
    template<typename T>
    string show(const vector<T> &vs) {
        string res = "{ ";
        for (int i = 0; i < vs.size() - 1; i++) {
            res += to_string(vs[i]) + ", ";
        }
        res += to_string(vs[vs.size()]) + " }";
        return res;
    }

    /// pretty string of a vector<T>
    template<typename T>
    string show(vector<T> &vs) {
        const vector<T> vs_ = vs;
        return show(vs_);
    }

} // namespace cn