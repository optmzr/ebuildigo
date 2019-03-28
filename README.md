# ebuildigo

Find and convert Go modules/dependencies to the Ego (Ebuild class for Portage)
format.

Test it with (running it inside this Git repository):
```
tail -n+2 test-deps.txt | stack run
```

And run it on a real Go project with:
```
go list -f "{{ .Path }} {{ .Version }}" -m all | tail -n+2 | ebuildigo
```
