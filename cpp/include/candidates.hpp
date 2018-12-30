#include <vector>

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
std::vector<int> encode(std::string &cs)
{
  std::vector<int> res = {};
  for (auto &c : cs)
  {
    res.push_back((int)c);
  }
  return res;
}

// decode an encoded vector of ints to a string
std::string decode(std::vector<int> &xs)
{
  std::string s = "";
  for (int x : xs)
  {
    s.push_back(x);
  }
  return s;
}

// pretty string of a vector if int
std::string show(const std::vector<int> &vs)
{
  std::string res = "{ ";
  for (int i = 0; i < vs.size(); i++)
  {
    res += std::to_string(vs[i]) + ", ";
  }
  res.pop_back();
  res.pop_back();
  res += " }";
  return res;
}

std::string show(const std::vector<std::vector<char>> &vs)
{
  std::string res = "{ ";
  for (int i = 0; i < vs.size(); i++)
  {
    std::string str(vs[i].begin(), vs[i].end());
    res += str + ", ";
  }
  res.pop_back();
  res.pop_back();
  res += " }";
  return res;
}

} // namespace cn