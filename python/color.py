from random import randint as rand
from random import seed
import time

seed(time.time())

def get_str_color(num):
    color = f"0{hex(num)[2:]}" if (num < 16) else f"{hex(num)[2:]}"
    return color

def get_random_color():
    red = get_str_color(rand(0,255))
    green = get_str_color(rand(0,255))
    blue = get_str_color(rand(0,255))
    return f"#{red}{green}{blue}"