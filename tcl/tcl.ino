#include <SPI.h>
#include <TCL.h>

/*****************************************************************************
 * config
 ****************************************************************************/

const int LEDS = 25;
const int OVERHEAD = 1; // is 1 for strings up to 254 lights in length


/*****************************************************************************
 * RGB
 ****************************************************************************/

typedef struct {
    byte r;
    byte g;
    byte b;
} RGB;

const RGB blackRGB  = { 0x00, 0x00, 0x00 };
const RGB redRGB    = { 0xff, 0x00, 0x00 };
const RGB greenRGB  = { 0x00, 0xff, 0x00 };
const RGB blueRGB   = { 0x00, 0x00, 0xff };
const RGB purpleRGB = { 0xff, 0x00, 0xff };
const RGB whiteRGB  = { 0xff, 0xff, 0xff };
const RGB yellowRGB = { 0xff, 0x70, 0x00 };
const RGB orangeRGB = { 0xff, 0x20, 0x00 };
const RGB pinkRGB   = { 0xff, 0x00, 0x20 };

const int PRETTY_COLORS = 8;
const RGB prettyColors[PRETTY_COLORS] =
    { redRGB
    , greenRGB
    , blueRGB
    , purpleRGB
    , whiteRGB
    , yellowRGB
    , orangeRGB
    , pinkRGB
    };

void sendRGB(RGB rgb) {
    TCL.sendColor(rgb.r, rgb.g, rgb.b);
}

void blank() {
    TCL.sendEmptyFrame();
    for (int led = 0; led < LEDS; led++) {
        sendRGB(blackRGB);
    }
    TCL.sendEmptyFrame();
}

RGB readRGB() {
    RGB rgb;
    rgb.r = analogRead(TCL_POT3)>>2;
    rgb.b = analogRead(TCL_POT2)>>2;
    rgb.g = analogRead(TCL_POT4)>>2;

    return(rgb);
}

void sendRGBS (RGB rgb[]) {
    TCL.sendEmptyFrame();
    for (int led = 0; led < LEDS; led++) {
        sendRGB(rgb[led]);
    }
    TCL.sendEmptyFrame();
}

/*****************************************************************************
 * typedefs
 ****************************************************************************/

typedef RGB * strand;

/*****************************************************************************
 * unpack
*****************************************************************************/
void unpack(const unsigned char *data, RGB *rgb) {
    int i = 0;
    for (int l = 0; l < LEDS; l++) {
        i = l * 3;
        rgb[l].r = data[i];
        rgb[l].g = data[i+1];
        rgb[l].b = data[i+2];
    }
}

/*****************************************************************************
 * unstuff
 *****************************************************************************/

// unstuff data (Consistent Overhead Byte Stuffing,
// http://en.wikipedia.org/wiki/Consistent_Overhead_Byte_Stuffing)

void unstuff(const unsigned char *src, unsigned char *dst) {
    const unsigned char *end = src + (LEDS * 3) + OVERHEAD;
    while (src < end) {
        int i, code = *src++;
        for (i=1; i < code; i++)
            *dst++ = *src++;
        if (code < 0xFF)
            *dst++ = 0;
    }
}

/*****************************************************************************
 * setup
 ****************************************************************************/

void setup() {
    TCL.begin();
    TCL.setupDeveloperShield();
    Serial.begin(115200); // FIXME: baud rate seems a bit low
}

/*****************************************************************************
 * loop
 ****************************************************************************/

unsigned char buf[(LEDS*3)+1];
unsigned char unstuffed[(LEDS*3)+1];
unsigned char *ptr;
unsigned char *end = buf + (LEDS*3) + 1;
int state = 0;
RGB rgbs[LEDS];

void loop () {
    byte val;

    switch(state) {
    case 0: // waiting for start of frame
            while (Serial.available()) {
                val = Serial.read();
                if (val == 0)
                {
                    ptr = buf;
                    state = 1;
                }
            };
            break;
    case 1: // filling buffer
        while (Serial.available()) {
            val = Serial.read();
            if (!val) { // got a zero -- must have been misaligned
                ptr = buf;
            } else {
                *ptr++ = val;
                if (ptr >= end) {
                    state = 2;
                    break;
                }
            }
        }
        break;
    }

    if (state == 2) {
        state = 0;
        unstuff(buf, unstuffed);
        unpack(unstuffed, rgbs);
        sendRGBS(rgbs);
    }
}
