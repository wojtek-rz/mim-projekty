# Plik z logiem umieścić w pliku "log.txt"
# Szczegóły formatu linijek są koło 100 linijki.
# Wszystkie stałe (szczególnie szerokości i wysokości) można zmieniać.
# Tak, wiem, że istnieją funkcje w pythonie.

import pygame
import copy

FILE_NAME = 'log.txt'

HEIGHT, WIDTH = 800, 1600
MARGIN = 70
TEXT_GAP = 20
BIG_FONT_SIZE = 30
SMALL_FONT_SIZE = 20
STATION_BOX_HEIGHT = 200
STATION_BOX_WIDTH = 150

INFO_TXT = 'USE ARROWS TO MOVE FORWARDS/BACKWARDS AND SPACE TO PLAY'

# =================== pygame init =======================
pygame.init()
screen = pygame.display.set_mode((WIDTH, HEIGHT))
clock = pygame.time.Clock()

test_surface = pygame.Surface((WIDTH, HEIGHT))
test_surface.fill('white')

small_font = pygame.freetype.SysFont('Sans', SMALL_FONT_SIZE)
big_font = pygame.freetype.SysFont('Sans', BIG_FONT_SIZE)

# ================== reading txt file ======================
actors_set = set()
stations_set = set()
with open(FILE_NAME) as f:
    log = f.read()

operations = []
for line in log.split('\n'):
    if ' tries to enter the workshop and occupy ' in line:
        actor, station = line.split(' tries to enter the workshop and occupy ')
        actors_set.add(actor)
        stations_set.add(station)

    if ' tries to switch its workplace to ' in line:
        actor, station = line.split(' tries to switch its workplace to ')
        actors_set.add(actor)
        stations_set.add(station)

    if ' leaves ' in line:
        actor, station = line.split(' leaves ')
        actors_set.add(actor)

    operations.append(line)

print(actors_set)
print(stations_set)

# ================== preparing station coordinates ======================
stations_coords = dict()
cumulated_x = 0
for station_name in sorted(stations_set):
    text_rect = small_font.get_rect(station_name)
    stations_coords[station_name] = pygame.Rect(
        MARGIN + cumulated_x,
        HEIGHT - MARGIN - STATION_BOX_HEIGHT,
        max(STATION_BOX_WIDTH, text_rect.width) + TEXT_GAP,
        STATION_BOX_HEIGHT)

    cumulated_x += max(STATION_BOX_WIDTH, text_rect.width) + TEXT_GAP * 2

# ================== preparing actors start coordinates ==================
actors_start_coords = dict()
cumulated_x = MARGIN
cumulated_y = MARGIN
for actor_name in actors_set:
    actors_start_coords[actor_name] = (cumulated_x, cumulated_y)
    text_rect = big_font.get_rect(actor_name)
    cumulated_x += text_rect.width + TEXT_GAP
    if cumulated_x > 500:
        cumulated_x = MARGIN
        cumulated_y += BIG_FONT_SIZE + 10

# =================== preparing states ==================
global_states = list()  # [(actors_states, workstation_lists, workstations_waiting_lists), ...]
actors_start_states = dict()
workstations_lists = dict()
workstations_waiting_lists = dict()
home_list = list()
for actor in actors_set:
    actors_start_states[actor] = ('HOME', '')
for workstation in stations_set:
    workstations_lists[workstation] = []
for workstation in stations_set:
    workstations_waiting_lists[workstation] = []
for actor in actors_set:
    home_list.append(actor)

global_states.append((actors_start_states, workstations_lists, workstations_waiting_lists, home_list))

for line in operations:
    print(line)
    new_states, new_workstations_lists, new_workstations_waiting_lists, new_home_list = copy.deepcopy(global_states[-1])
    if ' tries to enter the workshop and occupy ' in line:
        actor, station = line.split(' tries to enter the workshop and occupy ')
        new_states[actor] = ('HOME', "")
        new_workstations_waiting_lists[station].append(actor)
    elif ' now occupies ' in line:
        actor, station = line.split(' now occupies ')
        prev_workstation = new_states[actor][1]
        if actor in new_workstations_lists.get(prev_workstation, []):
            new_workstations_lists[prev_workstation].remove(actor)
        if actor in new_home_list:
            new_home_list.remove(actor)
        if actor in new_workstations_waiting_lists[station]:
            new_workstations_waiting_lists[station].remove(actor)

        new_workstations_lists[station].append(actor)
        new_states[actor] = ('OCCUPIES', station)
    elif ' starts using ' in line:
        actor, station = line.split(' starts using ')
        new_states[actor] = ('USES', station)
    elif ' stops using ' in line:
        actor, station = line.split(' stops using ')
        new_states[actor] = ('OCCUPIES', station)
    elif ' tries to switch its workplace to ' in line:
        actor, station = line.split(' tries to switch its workplace to ')
        new_workstations_waiting_lists[station].append(actor)
    elif ' leaves ' in line:
        actor, station = line.split(' leaves the workshop')
        prev_workstation = new_states[actor][1]
        if actor in new_workstations_lists[prev_workstation]:
            new_workstations_lists[prev_workstation].remove(actor)

        new_home_list.append(actor)
        new_states[actor] = ('HOME', '')
    else:
        print('error: unsupported operation')
        print('"', line, '"')
    global_states.append((new_states, new_workstations_lists, new_workstations_waiting_lists, new_home_list))

# ================= main loop =====================
time = 0
n = 0
run = True
PLAY = False
while run:
    clock.tick(30)
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            run = False
        if event.type == pygame.KEYDOWN:
            if event.key == pygame.K_RIGHT:
                n = (n + 1) % len(operations)
                print(n, 'states: ', global_states[n][0])
                print(n, 'occupying: ', global_states[n][1])
                print(n, 'waiting: ', global_states[n][2])
                print(n, 'home', global_states[n][3])

            if event.key == pygame.K_LEFT:
                n = (n - 1) % len(operations)
            if event.key == pygame.K_SPACE:
                PLAY = not PLAY

    if PLAY:
        time += 1
        if time > 30:
            print(global_states[n])
            (time, n) = (0, (n + 1) % len(operations))

    screen.blit(test_surface, (0, 0))

    actors_state, workstations_lists, workstations_waiting_lists, home_list = global_states[n]
    for actor in home_list:
        big_font.render_to(screen, actors_start_coords[actor], actor)

    for workstation, workstation_list in workstations_lists.items():
        ws_cords = stations_coords.get(workstation)
        render_coords = (ws_cords.left + 10, ws_cords.top + 10)
        for i, actor in enumerate(workstation_list):
            height_shift = i * (BIG_FONT_SIZE + 5)
            new_render_coords = (render_coords[0], render_coords[1] + height_shift)
            big_font.render_to(screen, new_render_coords, actor)
            if actors_state[actor][0] == 'USES':
                font_rect = big_font.get_rect(actor)
                pygame.draw.rect(screen, 'red', (
                new_render_coords[0] - 5, new_render_coords[1] - 5, font_rect.width + 10, font_rect.height + 10), 2)

    for workstation, ws_waiting_list in workstations_waiting_lists.items():
        ws_cords = stations_coords.get(workstation)
        render_coords = (ws_cords.left + 10, ws_cords.top - 50)
        for i, actor in enumerate(ws_waiting_list):
            height_shift = i * (BIG_FONT_SIZE + 5)
            new_render_coords = (render_coords[0], render_coords[1] - height_shift)
            big_font.render_to(screen, new_render_coords, actor, 'grey')

    for name, rect in stations_coords.items():
        pygame.draw.rect(screen, 'black', rect, 2)
        small_font.render_to(screen, (rect.left + TEXT_GAP / 2, rect.top + rect.height - 50), name)

    small_font.render_to(screen, (WIDTH - MARGIN - small_font.get_rect(INFO_TXT).width, MARGIN), INFO_TXT)

    instruction_txt = str(n) + ": " + operations[n - 1]
    small_font.render_to(screen, (WIDTH - MARGIN - small_font.get_rect(instruction_txt).width, MARGIN + 50),
                         instruction_txt, 'red')

    if PLAY:
        small_font.render_to(screen, (WIDTH - MARGIN - small_font.get_rect('PLAYING').width, MARGIN + 100), 'PLAYING')

    pygame.display.flip()

pygame.quit()
exit()
