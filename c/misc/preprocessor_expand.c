#define CHECK_N(x, n, ...) n
#define CHECK(...) CHECK_N(__VA_ARGS__, 0,)
#define PROBE(x) x, 1,

#define IS_PAREN(x) CHECK(IS_PAREN_PROBE x)
#define IS_PAREN_PROBE(...) PROBE(~)

IS_PAREN_PROBE ()
// PROBE(~)
// ~, 1,

IS_PAREN(()) // Expands to 1
//CHECK(IS_PAREN_PROBE ())
//CHECK( PROBE(~) )
//CHECK( ~, 1, )
//CHECK_N( ~, 1, , 0, )
//1

IS_PAREN(xxx) // Expands to 0
//CHECK( IS_PAREN_PROBE xxx )
//CHECK_N( IS_PAREN_PROBE xxx, 0, )
//0

#define EMPTY()
#define DEFER(id) id EMPTY()
#define OBSTRUCT(...) __VA_ARGS__ DEFER(EMPTY)()
#define EXPAND(...) __VA_ARGS__

#define A() 123

A () // Expands to 123

DEFER(A)() // Expands to A () because it requires one more scan to fully expand
// A EMPTY()()
// A ()		// this is because preprocessor is not scanning backward once EMPTY() is processed

EXPAND(DEFER(A)()) // Expands to 123, because the EXPAND macro forces another scan
// EXPAND( A EMPTY()() )
// EXPAND( A () )
// EXPAND( 123 )
// 123

OBSTRUCT(DEFER(A)())
// OBSTRUCT( A EMPTY()() )
// OBSTRUCT( A () )
// OBSTRUCT( 123 )
// 123 DEFER(EMPTY)()
// 123 EMPTY EMPTY()()
// 123 EMPTY ()

DEFER(EMPTY)() .
// EMPTY EMPTY()() .
// EMPTY () .

#define COMPARE_foo(x) x
#define COMPARE_bar(x) x

#define PRIMITIVE_COMPARE(x, y) IS_PAREN \
( \
COMPARE_ ## x ( COMPARE_ ## y) (())  \
)

PRIMITIVE_COMPARE(foo, bar) // Expands to 1
// IS_PAREN( COMPARE_foo ( COMPARE_bar ) (()) )
// IS_PAREN( COMPARE_bar (()) )
// IS_PAREN( () )
// CHECK(IS_PAREN_PROBE ())
// CHECK(PROBE(~))
// CHECK_N(~, 1,)
// 1

PRIMITIVE_COMPARE(foo, foo) // Expands to 0
// IS_PAREN( COMPARE_foo ( COMPARE_foo ) (()) )
// IS_PAREN( COMPARE_foo (()) )		// COMPARE_foo (()) will not be expanded
// CHECK(IS_PAREN_PROBE COMPARE_foo (()))
// CHECK_N(IS_PAREN_PROBE COMPARE_foo (()), 0,)
// 0

PRIMITIVE_COMPARE(foo, unfoo) // Should expand to 1, but it expands to 0
// IS_PAREN( COMPARE_foo ( COMPARE_unfoo ) (()) )
// IS_PAREN( COMPARE_unfoo (()) )
// CHECK(IS_PAREN_PROBE COMPARE_unfoo (()))
// CHECK_N(IS_PAREN_PROBE COMPARE_unfoo (()), 0,)
// 0

#define CAT(a, ...) PRIMITIVE_CAT(a, __VA_ARGS__)
#define PRIMITIVE_CAT(a, ...) a ## __VA_ARGS__

#define IIF(c) PRIMITIVE_CAT(IIF_, c)
#define IIF_0(t, ...) __VA_ARGS__
#define IIF_1(t, ...) t

#define COMPL(b) PRIMITIVE_CAT(COMPL_, b)
#define COMPL_0 1
#define COMPL_1 0

#define BITAND(x) PRIMITIVE_CAT(BITAND_, x)
#define BITAND_0(y) 0
#define BITAND_1(y) y

#define NOT(x) CHECK(PRIMITIVE_CAT(NOT_, x))
#define NOT_0 PROBE(~)

#define BOOL(x) COMPL(NOT(x))
#define IF(c) IIF(BOOL(c))

#define EAT(...)
#define EXPAND(...) __VA_ARGS__
#define WHEN(c) IF(c)(EXPAND, EAT)

#define IS_COMPARABLE(x) IS_PAREN( CAT(COMPARE_, x) (()) )

#define NOT_EQUAL(x, y) \
IIF(BITAND(IS_COMPARABLE(x))(IS_COMPARABLE(y)) ) \
( \
   PRIMITIVE_COMPARE, \
   1 EAT \
)(x, y)

#define EQUAL(x, y) COMPL(NOT_EQUAL(x, y))

IS_COMPARABLE(foo) // 1
// IS_PAREN( CAT(COMPARE_, foo) (()) )
// IS_PAREN( COMPARE_foo (()) )
// IS_PAREN( () )
// 1

IS_COMPARABLE(bar) // 1
// IS_PAREN( CAT(COMPARE_, bar) (()) )
// IS_PAREN( COMPARE_bar (()) )
// IS_PAREN( () )
// 1

IS_COMPARABLE(unfoo) // 0
// IS_PAREN( CAT(COMPARE_, unfoo) (()) )
// IS_PAREN( COMPARE_unfoo (()) )
// 0

NOT_EQUAL(foo, bar) // 1
// IIF( BITAND(1)(IS_COMPARABLE(bar)) )( PRIMITIVE_COMPARE, 1 EAT )(foo, bar)
// IIF( IS_COMPARABLE(bar) )( PRIMITIVE_COMPARE, 1 EAT )(foo, bar)
// IIF( 1 )( PRIMITIVE_COMPARE, 1 EAT )(foo, bar)
// PRIMITIVE_COMPARE(foo, bar)
// 1

NOT_EQUAL(foo, foo) // 0
// IIF( BITAND(1)(IS_COMPARABLE(foo)) )( PRIMITIVE_COMPARE, 1 EAT )(foo, foo)
// IIF( IS_COMPARABLE(foo) )( PRIMITIVE_COMPARE, 1 EAT )(foo, foo)
// IIF( 1 )( PRIMITIVE_COMPARE, 1 EAT )(foo, foo)
// PRIMITIVE_COMPARE(foo, foo)
// 0

NOT_EQUAL(foo, unfoo). // 1 .
// IIF( BITAND(1)(IS_COMPARABLE(unfoo)) )( PRIMITIVE_COMPARE, 1 EAT )(foo, unfoo)
// IIF( IS_COMPARABLE(unfoo) )( PRIMITIVE_COMPARE, 1 EAT )(foo, unfoo)
// IIF( 0 )( PRIMITIVE_COMPARE, 1 EAT )(foo, unfoo)
// 1 EAT(foo, unfoo)
// 1 .

EQUAL(foo, bar) // 0
EQUAL(foo, foo) // 1
EQUAL(foo, unfoo) // 0

