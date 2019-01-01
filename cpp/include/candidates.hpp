#include <vector>

using namespace std;

namespace cn
{
int square(int a)
{
  return a * a;
}

bool is_pythagorean_triple(int a, int b, int c)
{
  return square(a) + square(b) == square(c);
}

// encode a string to a vector of ints
vector<int> encode(string &cs)
{
  vector<int> res = {};
  for (auto &c : cs)
  {
    res.push_back((int)c);
  }
  return res;
}

// decode an encoded vector of ints to a string
string decode(vector<int> &xs)
{
  string s = "";
  for (int x : xs)
  {
    s.push_back(x);
  }
  return s;
}

// pretty string of a vector if int
string show(const vector<int> &vs)
{
  string res = "{ ";
  for (int i = 0; i < vs.size(); i++)
  {
    res += to_string(vs[i]) + ", ";
  }
  res.pop_back();
  res.pop_back();
  res += " }";
  return res;
}

// pretty string of a vector if uint8_t
string show(const std::vector<uint8_t> &vs)
{
  string res = "{ ";
  for (int i = 0; i < vs.size(); i++)
  {
    res += to_string(vs[i]) + ", ";
  }
  res.pop_back();
  res.pop_back();
  res += " }";
  return res;
}

// XOR Encryption
string applyXorCipher(string const &cs, string const &key)
{
  string res = cs;
  auto mod = key.size() / sizeof(char);
  for (int i = 0; i < cs.size(); i++)
  {
    auto c = key[i % mod];
    res[i] = cs[i] ^ c;
  }
  return res;
}

} // namespace cn