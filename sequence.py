class Sequence:
    def __init__(self, id, sequence):
        self.id = id
        self.sequence = sequence

    def __str__(self):
        return f"Sequence ID: {self.id}, Length: {len(self.sequence)}"
