#include <stdio.h>
#include <stdlib.h>
#include <fmt/core.h>

using namespace std;
using fmt::print;

template <typename... Args>
static void die(fmt::format_string<Args...> FormatStr, Args &&... args)
{
  fmt::print(stderr, "{}", fmt::format(FormatStr, std::forward<Args>(args)...));
  exit(-1);
}

#define CASE(s) if (s == S_)

int main(int argc, char **argv)
{
  int argp = 1;

  while(argp < argc && argv[argp][0] == '-')
  {
    const string& S_ = argv[argp++];

    CASE("--help")
    {
      fmt::print("__PROJECT-NAME__ - \n\n"
                 "Syntax:\n"
                 "    __PROJECT-NAME__ [] ...\n"
                 "\n"
                 "Options:\n"
                 "    --foo x    Foo\n");
      exit(0);
    }
    die("Unknown option {}!", S_);
  }
  exit(0);
  return(0);
}
