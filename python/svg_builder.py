from color import get_random_color as get_color
from random import randint as rand
from random import seed
import time

seed(time.time())

def get_random_coords(len: int, coord: tuple):
    if (len == 0):
        return []

    seed(time.time())
    coords = [(rand(0,coord[0]), rand(0, coord[1]))]
    lastCoord = coords[0]
    for n in range(len-1):
        coords.append((rand(0,coord[0]), rand(0, coord[1])))
    return coords

def get_triangles_from_coords(coords):
    svg_triangles = []
    for idx in range(len(coords) - 2):
        color = get_color()
        fst_dot = f"{coords[idx][0]},{coords[idx][1]}"
        snd_dot = f"{coords[idx+1][0]},{coords[idx+1][1]}"
        trd_dot = f"{coords[idx+2][0]},{coords[idx+2][1]}"
        polygon = f"<polygon points='{fst_dot} {snd_dot} {trd_dot}' style='fill:{color};stroke:black;stroke-width:1;opacity:0.1' />"
        svg_triangles.append(polygon)
    return svg_triangles

def get_triangles(ammount: int, coord: tuple):
    return get_triangles_from_coords(get_random_coords(ammount, coord))

def svg_begin(coord: tuple):
    return f"<svg width='{coord[0]}' height='{coord[1]}' xmlns='http://www.w3.org/2000/svg'>\n"

def create_svg(coord: tuple, figures):

    svg = svg_begin(coord)
    for figure in figures:
        svg += f'\t{figure}\n'

    svg += "</svg>"
    return svg
