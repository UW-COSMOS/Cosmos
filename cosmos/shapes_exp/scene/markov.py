import numpy as np

class MarkovSampler:
    NOT_INIT = -1

    def __init__(self, init_dist, transition_matrix):
        self.init_dist = init_dist.values.reshape(init_dist.shape[1])
        self.transition_matrix = transition_matrix
        self.names = list(init_dist.columns)
        self.curr = MarkovSampler.NOT_INIT

    def sample(self):

        if self.curr == MarkovSampler.NOT_INIT:
            next_state = np.random.choice(len(self.init_dist), p=self.init_dist)
        else:
            row_select = self.transition_matrix[self.transition_matrix["init"] == self.names[self.curr]]
            mat = row_select.values[0, 1:].reshape(len(self.names))
            mat = np.array(list(mat))
            next_state = np.random.choice(len(self.names), p=mat)
        self.curr = next_state
        name = self.names[next_state]
        return name
