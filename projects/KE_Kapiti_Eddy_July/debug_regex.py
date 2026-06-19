import re
target = 'THETA'
p_pattern = re.compile(r'(<par\s+name\s*=\s*"' + re.escape(target) + r'"\s+value\s*=\s*")([^"]+)("(?:\s*/>|>\s*</par\s*>))')
text = '      <par name="THETA" value="09"/>'
m = p_pattern.search(text)
if m:
    print(f"Groups: {m.groups()}")
else:
    print("No match")
