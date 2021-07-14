map = static_library('map', 'map.c', dependencies: m_dep)
heap = static_library('heap', 'heap.c', dependencies: m_dep)
tree = static_library('tree', 'tree.c', dependencies: m_dep, link_with: [map, heap])

executable(
	'huffman', 'huffman.c',
	dependencies: m_dep,
	link_with: [map, tree]
)
