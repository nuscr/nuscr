Printing global types in mpstk syntax.

https://github.com/alcestes/mpstk/blob/5a056cc2193c2c588c1c7ddf130176c3aacdf38e/examples/streaming.global
Streaming protocol in MPSTK looks like this:
μ(t)(
DP→K:d(bool) .
KP→K:k(bool) .
K→C:c(bool) .
DP→K:d(bool) .
KP→K:k(bool) .
K→C:c(bool) . t
)
  $ nuscr --show-global-type-mpstk Streaming Streaming.nuscr
  μ(t)(DP→K:d(bool) . KP→K:k(bool) . K→C:c(bool) . DP→K:d(bool) . KP→K:k(bool) . K→C:c(bool) . t)

https://github.com/alcestes/mpstk/blob/5a056cc2193c2c588c1c7ddf130176c3aacdf38e/examples/two-buyers.global
Two buyer protocol in MPSTK looks like this:
B1→S:s(string) .
S→B1:b1(int) .
S→B2:b2(int) .
B1→B2:bi2(int) .
B2→S:{
ok . B2→S:s(string) . S→B2:b2(string) . end,
quit . end
}
  $ nuscr --show-global-type-mpstk TwoBuyer TwoBuyer.nuscr
  B1→S:s(string) . S→B1:b1(int) . S→B2:b2(int) . B1→B2:bi2(int) . B2→S:{
  ok . B2→S:s(string) . S→B2:b2(string) . end,
  quit . end
  }
