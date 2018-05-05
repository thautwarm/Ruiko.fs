import re, timeit, time, gc
spec = [
	"\n",
	"keyword1",
	"keyword2",
	"keyword3",
	"[a-zA-Z_]{1}[a-zA-Z_0-9]*",
	"\s+"
	]
regex = list(map(re.compile, spec))

def test(text: str):
	pos = 0
	k = 0
	n = len(text)
	while pos < n:
		for each in regex:
			k += 1
			m = each.match(text, pos)
			if m:
				pos += len(m.group())
				break
		else:
			pos += 1
			continue
with open("text") as f:
	s = f.read()
	print(timeit.timeit('test(s)', number=100000, globals={'test': test, 's': s}) * 1000)





