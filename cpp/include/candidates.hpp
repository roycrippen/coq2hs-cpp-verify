#include <vector>
#include <sstream>

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

    string show_utf8_hex(vector<uint8_t> xs) {
        stringstream ss;
        for (auto i = 0; i < xs.size() - 1; ++i) {
            ss << uppercase << hex << static_cast<int>(xs[i]) << " ";
        }
        ss << uppercase << hex << static_cast<int>(xs[xs.size() - 1]);
        auto s = ss.str();
        if (s == "0")
            s = "00";
        return s;
    }

    // Unicode codepoint encoding and decoding
    // port from https://rosettacode.org/wiki/UTF-8_encode_and_decode#Go
    // first byte of a 2-byte encoding starts 110 and carries 5 bits of data
    const uint8_t b2Lead = 0xC0;  // 1100 0000
    const uint8_t b2Mask = 0x1F;  // 0001 1111

    // first byte of a 3-byte encoding starts 1110 and carries 4 bits of data
    const uint8_t b3Lead = 0xE0;  // 1110 0000
    const uint8_t b3Mask = 0x0F;  // 0000 1111

    // first byte of a 4-byte encoding starts 11110 and carries 3 bits of data
    const uint8_t b4Lead = 0xF0;  // 1111 0000
    const uint8_t b4Mask = 0x07;  // 0000 0111

    // non-first bytes start 10 and carry 6 bits of data
    const uint8_t mbLead = 0x80;  // 1000 0000
    const uint8_t mbMask = 0x3F;  // 0011 1111


    vector<uint8_t> encodeCodepoint(const uint32_t i) {
        vector<uint8_t> res = {};

        if (i <= (1 << 7) - 1) { // max code point that encodes into a single byte
            res.push_back(static_cast<uint8_t>(i));
        } else if (i <= (1 << 11) - 1) {  // into two bytes
            res.push_back(b2Lead | static_cast<uint8_t>(i >> 6));
            res.push_back(mbLead | (static_cast<uint8_t>(i) & mbMask));
        } else if (i <= (1 << 16) - 1) {  // three
            res.push_back(b3Lead | static_cast<uint8_t>(i >> 12));
            res.push_back(mbLead | (static_cast<uint8_t>(i >> 6) & mbMask));
            res.push_back(mbLead | (static_cast<uint8_t>(i) & mbMask));
        } else {  // four
            res.push_back(b4Lead | static_cast<uint8_t>(i >> 18));
            res.push_back(mbLead | (static_cast<uint8_t>(i >> 12) & mbMask));
            res.push_back(mbLead | (static_cast<uint8_t>(i >> 6) & mbMask));
            res.push_back(mbLead | (static_cast<uint8_t>(i) & mbMask));
        }
        return res;
    }

    uint32_t encodeCodepoint(const vector<uint8_t> &bs) {
        if (bs.empty() || bs.size() > 4)
            return 0;

        auto bs0 = bs[0];
        if (bs0 < 0x80)
            return static_cast<uint32_t>(bs0);
        else if (bs0 < 0xE0)
            return static_cast<uint32_t>(bs0 & b2Mask) << 6 |
                   static_cast<uint32_t>(bs[1] & mbMask);
        else if (bs0 < 0xF0)
            return static_cast<uint32_t>(bs0 & b3Mask) << 12 |
                   static_cast<uint32_t>(bs[1] & mbMask) << 6 |
                   static_cast<uint32_t>(bs[2] & mbMask);
        else
            return static_cast<uint32_t>(bs0 & b4Mask) << 18 |
                   static_cast<uint32_t>(bs[1] & mbMask) << 12 |
                   static_cast<uint32_t>(bs[2] & mbMask) << 6 |
                   static_cast<uint32_t>(bs[3] & mbMask);
    }

} // namespace cn