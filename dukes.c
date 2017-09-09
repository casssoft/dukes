#include <stdio.h>      /* printf, scanf, puts, NULL */
#include <stdlib.h>     /* srand, rand */

#define MAPX 1000
#define MAPY 1000

#include <string.h>
#include <unistd.h>
#include <sys/select.h>
#include <termios.h>
#include <sys/ioctl.h>
#include <time.h>       /* time */
#include <math.h>

#define TRUE 1
#define FALSE 0


struct termios orig_termios;

static char hasreset = 0;
void reset_terminal_mode()
{
    if (!hasreset) {
      tcsetattr(0, TCSANOW, &orig_termios);
      hasreset = 1;
    }
}

void set_conio_terminal_mode()
{
    struct termios new_termios;

    /* take two copies - one for now, one for later */
    tcgetattr(0, &orig_termios);
    memcpy(&new_termios, &orig_termios, sizeof(new_termios));

    /* register cleanup handler, and set the new terminal mode */
    atexit(reset_terminal_mode);
    cfmakeraw(&new_termios);
    new_termios.c_iflag &= ~IXOFF;
    new_termios.c_oflag |= OPOST;
    tcsetattr(0, TCSANOW, &new_termios);
}

int kbhit()
{
    struct timeval tv = { 0L, 0L };
    fd_set fds;
    FD_ZERO(&fds);
    FD_SET(0, &fds);
    return select(1, &fds, NULL, NULL, &tv);
}

int getch()
{
    int r;
    unsigned char c;
    if ((r = read(0, &c, sizeof(c))) < 0) {
        return r;
    } else {
        return c;
    }
}
#define PI 3.14159265
static unsigned int sWidth = 0;
static unsigned int sHeight = 0;

unsigned int spritesLength = 0;
char**sprites = NULL;

char* getSpriteFromFile(char*filename) {
    //from https://stackoverflow.com/questions/3747086/reading-the-whole-text-file-into-a-char-array-in-c
    FILE *fp;
    long lSize;
    char *buffer;

    fp = fopen (filename, "rb" );
    if( !fp ) perror(filename),exit(1);

    fseek( fp , 0L , SEEK_END);
    lSize = ftell( fp );
    rewind( fp );

    /* allocate memory for entire content */
    buffer = calloc( 1, lSize+1 );
    if( !buffer ) fclose(fp),fputs("memory alloc fails",stderr),exit(1);

    /* copy the file into the buffer */
    if( 1!=fread( buffer , lSize, 1 , fp) )
      fclose(fp),free(buffer),fputs("entire read fails",stderr),exit(1);
    fclose(fp);
    return buffer;
}

void loadSprites() {
    spritesLength = 3;
    sprites = (char**)malloc(sizeof(char*)*spritesLength);
    sprites[0] = getSpriteFromFile("sprites/dukeofvim");
    sprites[1] = getSpriteFromFile("sprites/king");
    sprites[2] = getSpriteFromFile("sprites/knight");
}

void drawSprite(int num) {
    printf("%s", sprites[num]);
}

void drawTextBox(const char*text) {
    int maxCol = 0;
    int i = 0;
    int curCol = 0;;
    while(text[i] != 0) {
        if (text[i] == '\n') {
            curCol = 0;
        } else {
            curCol += 1;
            if (maxCol < curCol) {
                maxCol = curCol;
            }
        }
        i += 1;
    }
    printf("\n __/\\");
    for (i = 0; i < maxCol + 2 - 3 - 1; ++i) printf("_");

    printf("\n|");
    for (i = 0; i < maxCol + 2; ++i) printf(" ");
    printf("|\n| ");
    i = 0;
    curCol = 0;
    while (text[i] != 0) {
        if (text[i] == '\n') {
            if (curCol < maxCol) {
                for (int j = 0; j < (maxCol - curCol); ++j) printf(" ");
            }
            printf(" |\n| ");
            curCol = 0;
        } else {
            curCol += 1;
            printf("%c", text[i]);
        }
        i += 1;
    }
    printf(" |\n|");
    for (i = 0; i < maxCol + 2; ++i) printf("_");
    printf("|\n");
}




#define INTRO 0
#define KNIGHT_INTRO 1
#define KING_INTRO 2
int state = INTRO;
int subscreen = 0;

const char *INTRO_DIALOG[] = {
"HEY IT'S YOU!\n"
"...\n"
"COME ON PRESS SPACE!", // 0

"MY NAME IS DUKELING\n"
"IM SURE YOU ALREADY KNOW THAT THOUGH", // 1

"OH YOU DONT?", // 2

"AHH I GUESS YOU ARE NEW HERE\n"
"I'LL INTRODUCE YOU TO EVERYONE", // 3

"DO YOU WANT ME TO INTRODUCE YOU TO\n"
"THE KNIGHT (1)? or THE KING (2)?", //4

"OK THEY ARE RIGHT OVER THERE\n"
"BYE BYE FOR NOW"}; //5


const char *KNIGHT_INTRO_D[] = {
"I AM THE HUMBLE KNIGHT\n"
"DO YOU WANT TO HEAR A RIDDLE?",
"JK JK JK NONE FOR NOW"};

const char *KING_INTRO_D[] = {
"LOL I AM THE KING" };



int curSprite = 0;
const char* curString = NULL;



void advanceState(char ch) {
  //switch (state) {
  //    case INTRO:
  //        drawSprite(0);
  //        drawTextBox(curString);
  //}

}

void draw() {
    printf("%c[%dJ", 0x1B, 2); // clear screen
    printf("%c[%d;%dH", 0x1B, 1, 1);
    drawSprite(curSprite);
    drawTextBox(curString);
}


int main() {
  curString = INTRO_DIALOG[0];
  loadSprites();
  struct winsize w;
  ioctl(STDOUT_FILENO, TIOCGWINSZ, &w);

  sWidth = w.ws_col;
  sHeight = w.ws_row;
  srand(time(NULL));

  set_conio_terminal_mode();
  printf("%c[?%dl", 0x1B, 25);
  printf("%c[%dJ", 0x1B, 2); // clear screen
  printf("%c[%d;%dH", 0x1B, 1, 1);
  char quit = FALSE;
  char hasLag = FALSE;
  
  char c;
  do {
      draw();
      c = getch();
      if (c == 'q') {
          break;
      }
      advanceState(c);
  } while (TRUE);
  printf("%c[%dJ", 0x1B, 2);
  printf("%c[?%dh", 0x1B, 25);
  printf("%c[%d;%dH", 0x1B, 1, 1);
  reset_terminal_mode();
  printf("Top Score: %d\n", 0);
  return 0;
}

