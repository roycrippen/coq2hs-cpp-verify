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

    return 0;
}
