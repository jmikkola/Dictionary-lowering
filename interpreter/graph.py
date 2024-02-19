from collections import defaultdict


def topological_order(edge_list):
    '''
    [('a', 'b')] means that b depends on a, and a
    should come first in the output.
    '''

    children = defaultdict(set)
    incoming_counts = defaultdict(int)
    nodes = set()

    for a, b in edge_list:
        nodes.add(a)
        nodes.add(b)
        children[a].add(b)
        incoming_counts[b] += 1

    order = []
    stack = []

    # sorted() is just here to make the order stable for the sake of unit tests
    for node in sorted(nodes):
        if incoming_counts[node] == 0:
            stack.append(node)

    visited = set()
    while stack:
        node = stack.pop()
        visited.add(node)
        order.append(node)

        for child in sorted(children[node]):
            incoming_counts[child] -= 1
            if incoming_counts[child] == 0:
                stack.append(child)

    if len(order) < len(nodes):
        return False
    return order


def strongly_connected_components(graph):
    ''' This assumes the graph is a dict of lists of strings.'''
    # Use Tarjan's strongly connected components algorithm

    index = 0
    indexes = {}
    lowlinks = {}
    stack = []
    in_stack = set()
    components = []

    def strong_connect(node):
        nonlocal index
        indexes[node] = index
        lowlinks[node] = index
        index += 1

        stack.append(node)
        in_stack.add(node)

        for neighbor in graph.get(node, []):
            if neighbor not in indexes:
                strong_connect(neighbor)
                lowlinks[node] = min(lowlinks[node], lowlinks[neighbor])
            elif neighbor in in_stack:
                lowlinks[node] = min(lowlinks[node], indexes[neighbor])

        if lowlinks[node] == indexes[node]:
            component = []
            while True:
                neighbor = stack.pop()
                in_stack.remove(neighbor)
                component.append(neighbor)
                if neighbor == node:
                    break
            components.append(component)

    for node in graph:
        if node not in indexes:
            strong_connect(node)

    return components
