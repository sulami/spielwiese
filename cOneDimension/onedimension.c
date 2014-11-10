#include <ncurses.h>
#include <stdlib.h>
#include <unistd.h>

#define SIZE 8
#define DAMAGE 25
#define SPAWN_CHANCE 5
#define SPEED 20
#define ENEMY_HP 100

#define RANDOM rand() % 100

struct enemy {
    unsigned int x;
    bool right;
    int hp;
    struct enemy *prev;
    struct enemy *next;
};

static int max_x, max_y, ch, x, size;
static struct enemy *enemies;

struct enemy *spawn_enemy(struct enemy *prev, bool right)
{
    struct enemy *e = malloc(sizeof(struct enemy));
    if (!e)
        return 0;

    e->x = right ? max_x : 1;
    e->right = right;
    e->hp = ENEMY_HP;
    e->prev = prev;
    e->next = NULL;

    return e;
}

void event_loop()
{
    struct enemy *e;

    ch = getch();
    if (ch) {
        switch(ch) {
        case 'z':
            x -= 1;
            break;
        case 'm':
            x += 1;
            break;
        case 'q':
            endwin();
            exit(0);
        }
    }

    getmaxyx(stdscr, max_y, max_x);
    clear();

    mvprintw(max_y / 2, x, "O");
    mvprintw(max_y / 2, x - size, "|");
    mvprintw(max_y / 2, x + size, "|");

    for (e = enemies; e; e = e->next) {
        if (abs(x - e->x) <= SIZE)
            e->hp -= DAMAGE;
        if (!e->hp) {
            if (e->prev) {
                e->prev->next = e->next;
                if (e->prev->next)
                    e->prev->next->prev = e->prev;
            } else {
                enemies = e->next;
                if (enemies)
                    enemies->prev = NULL;
            }
            free(e);
        }
    }

    for (e = enemies; e; e = e->next) {
        if (RANDOM <= SPEED)
            e->x = e->right ? e->x - 1 : e->x + 1;
        mvprintw(max_y / 2, e->x, "X");
    }

    if (RANDOM * 10 <= SPAWN_CHANCE) {
        if (enemies) {
            for (e = enemies; e->next; e = e->next);
            e->next = spawn_enemy(e, RANDOM > 50 ? false : true);
        } else {
            enemies = spawn_enemy(NULL, RANDOM > 50 ? false : true);
        }
    }

    refresh();
}

int main(int argc, char *argv[])
{
    initscr();
    raw();
    noecho();
    nodelay(stdscr, true);
    curs_set(FALSE);
    getmaxyx(stdscr, max_y, max_x);

    size = SIZE;
    x = max_x / 2;

    while(1) {
        event_loop();
        usleep(30000);
    }

    endwin();
    return 0;
}
