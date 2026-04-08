# Pre-PR Checklist

Run the following in order:

```bash
dune build @fmt --auto-promote
dune build
dune runtest
```

If any step fails, fix the issue before opening the PR.
