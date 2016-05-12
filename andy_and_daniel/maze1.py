import random


class Position(object):
    def __init__(self, y, x):
        self.y = y
        self.x = x

    def up(self):
        return Position(self.y - 1, self.x)

    def down(self):
        return Position(self.y + 1, self.x)

    def right(self):
        return Position(self.y, self.x + 1)

    def left(self):
        return Position(self.y, self.x - 1)

    def neighbours(self):
        return [self.up(), self.down(), self.left(), self.right()]

    def __str__(self):
        return '(x={}, y={})'.format(self.x, self.y)


class Cell(object):
    def __init__(self, upper, side):
        self.upper = upper
        self.side = side

        self.visited = False

        self.distance = None


class Maze(object):

    def __init__(self, n, rows):
        self.n = n
        self.rows = rows

    def __getitem__(self, position):
        return self.rows[position.y][position.x]

    def is_valid(self, position):
        return (0 <= position.x < self.n) and (0 <= position.y < self.n)

    def valid_neighbours(self, position):
        return [neighbour for neighbour in position.neighbours() if self.is_valid(neighbour)]

def remove_wall_between(maze, start, target):
    # Give a maze, a starting cell and a target cell, remove the
    # adjoining wall. If they are not neighbours, then error.

    # They are either vertical neighbours, or horizontal neighbours

    if abs(start.x - target.x) == 1:
        # They are horizontal neighbours
        right_position = sorted([start, target], key=lambda pos: pos.x)[1]
        right_cell = maze[right_position]
        # Remove that wall, and mark it as visited
        right_cell.side = False
    elif abs(start.y - target.y) == 1:
        # They are vertical neighbours
        bottom_position = sorted([start, target], key=lambda pos: pos.y)[1]
        bottom_cell = maze[bottom_position]
        # Remove that wall, and mark it as visited
        bottom_cell.upper = False
    else:
        raise ValueError('{} and {} are no neighbours'.format(start, target))
        
        


def list_of_cells(n):
    cells = []
    for i in range(n):
        cell = Cell(True, True)
        cells.append(cell)
    return cells


def all_visited(maze):
    for row in maze.rows:
        for cell in row:
            if not cell.visited:
                return False
    return True


def get_maze(n):
    rows = []
    for i in range(n):
        row = list_of_cells(n)
        rows.append(row)
    maze = Maze(n=n, rows=rows)

    # Have walls everywhere

    # TODO:
    # Pick starting position
    # Then loop until all visited:
    #   Pick a direction
    #   if target cell not visited, remove wall
    #   move there

    # position as (y, x)
    position = Position(random.randint(0, n-1), random.randint(0, n-1))

    while not all_visited(maze):

        # mark the current position as visited
        maze[position].visited = True

        # Move in a random direction
        legal_new_positions = maze.valid_neighbours(position)
        new_position = random.choice(legal_new_positions)

        # if we haven't visited the new position, tear down this wall.

        target_cell = maze[new_position]
        if not target_cell.visited:
            remove_wall_between(maze, position, new_position)

        position = new_position

    return maze



import pygame, sys
from pygame.locals import *


def draw_maze(maze, n):
    CELL_SIZE = 20

    # set up the window

    screen_size = (n*CELL_SIZE+1, n*CELL_SIZE+1)

    surface = pygame.display.set_mode(screen_size, 0, 32)

    

    BLACK = (0, 0, 0)
    WHITE = (255, 255, 255)

    # draw the white background onto the surface
    surface.fill(WHITE)

    for y_index, row in enumerate(maze.rows):
        for x_index, cell in enumerate(row):
            cell_top_left = (x_index * CELL_SIZE, y_index * CELL_SIZE)
            if cell.upper:
                end = (cell_top_left[0] + CELL_SIZE, cell_top_left[1])
                pygame.draw.line(
                    surface, BLACK, cell_top_left, end, 1)
            if cell.side:
                end = (cell_top_left[0], cell_top_left[1] + CELL_SIZE)
                pygame.draw.line(
                    surface, BLACK, cell_top_left, end, 1)

    maze_top_left = (0, 0)
    # Can just borrow the final values of y_index and x_index
    maze_bottom_right = ((y_index + 1) * CELL_SIZE, (x_index + 1) * CELL_SIZE)

    pygame.draw.rect(surface, BLACK, (maze_top_left, maze_bottom_right), 1)
    # draw the window onto the screen
    pygame.display.update()

    # run the game loop
    while True:
        for event in pygame.event.get():
            if event.type == QUIT:
                pygame.quit()
                sys.exit()



def main():
    n = 30
    maze = get_maze(n)
    draw_maze(maze, n)


if __name__ == '__main__':
    main()
