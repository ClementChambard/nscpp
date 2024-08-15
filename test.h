#ifndef TEST_DOT_H_INCLUDED
#define TEST_DOT_H_INCLUDED

#define debugBreak() __builtin_trap()

namespace ns {
void report_assertion_failure(const char *, const char *, const char *, int);
} // namespace ns

#define NS_ASSERT_M(expr, message)                                             \
  {                                                                            \
    if (expr) {                                                                \
    } else {                                                                   \
      ::ns::report_assertion_failure(#expr, message, __FILE__, __LINE__);      \
      debugBreak();                                                            \
    }                                                                          \
  }

#endif // TEST_DOT_H_INCLUDED
