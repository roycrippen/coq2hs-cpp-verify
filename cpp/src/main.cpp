#include <iostream>
#include "../include/candidates.hpp"

using namespace std;

int main(int argc, char *argv[]) {
    int a = 3;
    int b = 4;
    int c = 5;
    cout << "Is (" << a << ", " << b << ", " << c << ") a pythagorean triple: "
         << boolalpha << cn::is_pythagorean_triple(a, b, c) << "\n";
    c = 6;
    cout << "Is (" << a << ", " << b << ", " << c << ") a pythagorean triple: "
         << boolalpha << cn::is_pythagorean_triple(a, b, c) << "\n";

    auto cs = "This is the test string...";
    string key = "cipher key 123";
    cout << "key: '" << key << "'\n";
    cout << "input     string: '" << cs << "'\n";
    auto encrypted = cn::applyXorCipher(cs, key);
    const vector<uint8_t> vec(encrypted.begin(), encrypted.end());
    cout << "encrypted string: '" << cn::show(vec) << "'\n";
    auto decrypted = cn::applyXorCipher(encrypted, key);
    cout << "decrypted string: '" << decrypted << "'\n";
    auto orig_str = cn::applyXorCipher(cn::applyXorCipher(cs, key), key);
    cout << "applyXorCipher(applyXorCipher(cs, key), key): '" << orig_str << "'\n";

    const vector<uint32_t> codepoints = {0x0041, 0x00f6, 0x0416, 0x20ac, 0x1d11e, 0x0000};

    cout << "\n";
    printf("Unicode    UTF-8 encoding  Decoded  \n");
    printf("---------  --------------  ---------\n");
    for (auto codepoint : codepoints) {
        auto utf8 = cn::encodeCodepoint(codepoint);
        auto decoded = cn::decodeToCodepoint(utf8);
        printf("U+%-7.4x  %-14s  U+%-7.4x\n", codepoint, cn::show_utf8_hex(utf8).c_str(), decoded);
    }

    uint32_t i = 0x00f6;
    vector<uint8_t> es = cn::encodeCodepoint(i);
    cout << "\n i = " << i << ", es = " << cn::show_utf8_hex(es) << "\n";

    return 0;
}
