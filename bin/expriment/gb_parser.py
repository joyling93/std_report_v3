
with open('../debug/gb/pHS-AVC-LY018.gb') as f:
	content = f.readlines()
	print(content)
i = 0
feature={}
for line in content:
	if not line.startswith(' '):
		feature[line.rstrip()] = i
	i+=1
print(feature)

for key in feature.keys():
	if key.startswith('FEATURES'):
		

for line in :
	print(line)