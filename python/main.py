from random import randint as rand
from random import seed
import time
from color import get_random_color as get_color
from svg_builder import create_svg
from svg_builder import get_triangles

COORD = (1080, 2316)

def save_file(data: str):
    with open("pattern.svg", 'w') as file:
        file.write(data)



figures = get_triangles(150, COORD) 

save_file(create_svg(COORD, figures))

