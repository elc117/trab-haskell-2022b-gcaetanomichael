from svg_builder import create_svg
from svg_builder import get_triangles

RESOLUTION = (1080, 2316)

figures = get_triangles(150, RESOLUTION)
svg = create_svg(RESOLUTION, figures)

with open("pattern.svg", 'w') as file:
    file.write(svg)
