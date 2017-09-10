#include <stdlib.h>

#include <stdio.h>      /* printf, scanf, puts, NULL */
#include <stdlib.h>     /* srand, rand */

#include <string.h>
#include <string.h>
#include <unistd.h>
#include <sys/select.h>
#include <termios.h>
#include <sys/ioctl.h>
#include <time.h>       /* time */
#include <math.h>

struct termios orig_termios;

static char hasreset = 0;
void reset_terminal_mode(int a)
{
    if (!hasreset) {
      tcsetattr(0, TCSANOW, &orig_termios);
      hasreset = 1;
    }
}

void set_conio_terminal_mode(int b)
{
    struct termios new_termios;

    /* take two copies - one for now, one for later */
    tcgetattr(0, &orig_termios);
    memcpy(&new_termios, &orig_termios, sizeof(new_termios));

    /* register cleanup handler, and set the new terminal mode */
    //atexit(reset_terminal_mode);
    cfmakeraw(&new_termios);
    new_termios.c_iflag &= ~IXOFF;
    new_termios.c_oflag |= OPOST;
    tcsetattr(0, TCSANOW, &new_termios);
}
static unsigned int spritesLength = 0;
static char**sprites = NULL;

static char* getSpriteFromFile(char*filename) {
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

void loadSprites(int c) {
    spritesLength = 3;
    sprites = (char**)malloc(sizeof(char*)*spritesLength);
    sprites[0] = getSpriteFromFile("sprites/dukeofvim");
    sprites[1] = getSpriteFromFile("sprites/king");
    sprites[2] = getSpriteFromFile("sprites/knight");
}

static void drawSprite(int num) {
    printf("%s", sprites[num]);
}

static void drawTextBox(const char*text) {
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
    if (curCol < maxCol) {
        for (int j = 0; j < (maxCol - curCol); ++j) printf(" ");
    }
    printf(" |\n|");
    for (i = 0; i < maxCol + 2; ++i) printf("_");
    printf("|\n");
}

void drawScene(int num, const char* text) {
    printf("%c[%dJ", 0x1B, 2); // clear screen
    printf("%c[%d;%dH", 0x1B, 1, 1);
    drawSprite(num);
    drawTextBox(text);
}

