
import networkx as nx
import numpy as np

class Network:

    def __init__(self, n):
        self.angle_dict = []
        self.label_dict = []
        self.edge_list = []
        self.node_list = []
        self.G = nx.Graph()
        self.n = n

    def __get_angle(self):
        angle = []
        for i, node in zip(range(self.n), self.node_list):
            theta = 2.0 * np.pi * i / self.n
            angle.append((np.cos(theta), np.sin(theta)))
            self.angle_dict[node] = theta


