import matplotlib.pyplot as plt
import networkx as nx

import random as r
import sys

def random_sum_to(n, m):
    a = r.sample(range(1, n), m-1) + [0, n]
    list.sort(a)
    return [a[i+1] - a[i] for i in range(len(a) - 1)]

def spanning_tree(G):
    root = r.choice(list(G.nodes))

    T = nx.DiGraph()
    T.add_node(root)

    while True:
        fringe = [root]

        # DFS from the chosen root
        while fringe:
            node = fringe.pop()
            for n in G.successors(node):
                if n not in T.nodes():
                    T.add_node(n)
                    dic = G[node][n]
                    chosen_field = r.choice(list(dic.keys()))
                    field = G[node][n][chosen_field]['field']
                    T.add_edge(node, n, field=field, key=field, label=f"{field}")
                    fringe.append(n)

        # Go back if we cannot find a root
        if T.number_of_nodes() < G.number_of_nodes():
            next_root = False
            for p in G.predecessors(root):
                if p not in T.nodes():
                    T.add_node(p)
                    dic = G[p][root]
                    chosen_field = r.choice(list(dic.keys()))
                    field = G[p][root][chosen_field]['field']
                    T.add_edge(p, root, field=field, key=field, label=f"{field}")
                    next_root = p
                    break

            if next_root == False:
                return False, False

            root = next_root
        else:
            break

    if not nx.is_tree(T.to_undirected()):
        print("Spanning tree is not a tree")
        nx.draw(T)
        plt.show()
        nx.draw(G)
        plt.show()
        exit(1)

    return T, root

def label_edges(G):
    nG = nx.MultiDiGraph()

    from collections import defaultdict

    nb_edges = defaultdict(lambda : 0)

    for e in G.edges():
        if e[0] not in nG.nodes():
            nG.add_node(e[0])

        x = nb_edges[e[0]]
        nb_edges[e[0]] += 1
        nG.add_edge(e[0], e[1], field=x, key=x, label=f"{x}")

    return nG


def gen_tree(n):
    FAIL = (False, False, False)

    # Generate all possible combinations of edges
    out_degs = [r.randint(1, 3) for _ in range(n-1)]
    in_degs = random_sum_to(sum(out_degs), n)

    try:
        # G = nx.directed_havel_hakimi_graph(in_degs, out_degs)
        G = nx.directed_configuration_model(in_degs, out_degs)
    except:
        return FAIL

    if not nx.is_connected(G.to_undirected()):
        return FAIL

    
    nx.nx_pydot.write_dot(G, "graph2.dot")
    G = label_edges(G)
    T, root = spanning_tree(G)

    if T == False:
        #print("no spanning tree")
        return FAIL

    nx.nx_pydot.write_dot(G, "graph.dot")
    nx.nx_pydot.write_dot(T, "tree.dot")
    # Show with edges labeled
    # Draw the graph
    #pos = nx.spring_layout(G)  # Positions for all nodes
    #nx.draw(G, pos, with_labels=True)

    ## Draw edge labels
    #edge_labels = nx.get_edge_attributes(G, 'field')
    #nx.draw_networkx_edge_labels(G, pos, edge_labels=edge_labels)
    #plt.show()

    #pos = nx.spring_layout(T)  # Positions for all nodes
    #nx.draw(T, pos, with_labels=True)

    ## Draw edge labels
    #edge_labels = nx.get_edge_attributes(T, 'field')
    #nx.draw_networkx_edge_labels(T, pos, edge_labels=edge_labels)
    #plt.show()

    return G, T, root

def print_tree(T, root, buffer):

    fields = [99, 99, 99]
    for n in T.successors(root):
        fieldn = T[root][n]['field']
        fields[fieldn] = (n,)

    buffer.write(f"(##rib ;; {root}\n")
    for f in fields:
        if type(f) == tuple:
            print_tree(T, f[0], buffer)
        else:
            buffer.write(" ")
            buffer.write(str(f))
            buffer.write("\n")
    buffer.write(")\n")

def calculate_nodes(T, root):
    nodes = [0] * len(T.nodes())

    def dfs(node, path):
        nodes[node] = path

        for n in T.successors(node):
            field = T[node][n]['field']
            dfs(n, path + [field])

    dfs(root, [])

    return nodes

def node_as_string(node, var):
    if len(node) == 0:
        return var

    field = node[0]
    return node_as_string(node[1:], "(##field" + str(field) + " " + var + ")")

def write_graph_ribbit(G, T, root, buffer):

    buffer.write("(define root ")
    print_tree(T, root, buffer)
    buffer.write(")\n")

    buffer.write("(gc_check)")

    buffer.write("\n\n")

    nodes = calculate_nodes(T, root)

    for edge in G.edges(data=True):
        field = edge[2]['field']

        if T.has_edge(edge[0], edge[1]) and T[edge[0]][edge[1]]['field'] == field:
            continue

        src = node_as_string(nodes[edge[0]], "root")
        dst = node_as_string(nodes[edge[1]], "root")

        buffer.write("(##field" + str(field) + "-set! " + src + " " + dst + f");; {edge[0]} -{field}-> {edge[1]}\n")

    buffer.write("(gc_check)")
    buffer.write("\n\n")

    while len(G.nodes()) > 1:
        edge_to_remove = r.choice(list(G.edges(data=True)))
        field = edge_to_remove[2]['field']
        src = node_as_string(nodes[edge_to_remove[0]], "root")
        buffer.write(f"(##field" + str(field) + "-set! " + src + f" 99) ;; {edge_to_remove[0]} -X{field}X-> {edge_to_remove[1]}\n")
        buffer.write("(gc_check)\n")
        G.remove_edge(edge_to_remove[0], edge_to_remove[1], key=field)

        SP = nx.shortest_path(G, root)

        reachable_nodes = list(SP.keys())
        current_nodes = list(G.nodes())
        for n in current_nodes:
            if n not in reachable_nodes:
                G.remove_node(n)
            else:
                new_path = []
                for path_id in range(len(SP[n])-1):
                    edge = G[SP[n][path_id]][SP[n][path_id+1]]
                    field = list(edge.keys())[0]
                    new_path.append(field)
                nodes[n] = new_path


    buffer.write("\n\n")

def main():

    # Parse args
    n = int(sys.argv[1])

    if len(sys.argv) > 2:
        buffer = open(sys.argv[2], "w")
    else:
        buffer = sys.stdout

    # Write the graph to a file/standard output
    G, T, root = False, False, False
    while G == False:
        G, T, root = gen_tree(n)

    write_graph_ribbit(G, T, root, buffer)

main()









