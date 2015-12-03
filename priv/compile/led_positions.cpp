#include "leds.h"

LED led_positions_array[] = {
    {8, 14, -104, 20, 123, 121},
    {10, -1, -108, 67, 123, -126},
    {28, -46, -113, 104, 117, -111},
    {41, 6, -99, 57, 111, 124},
    {29, 35, -100, 27, 115, 113},
    {64, 35, -119, 43, 107, 115},
    {70, 30, -118, 47, 105, 116},
    {82, 26, -114, 51, 101, 117},
    {67, 10, -118, 57, 106, 123},
    {66, -8, -105, 68, 104, -123},
    {53, 4, -109, 60, 108, 125},
    {40, -15, -93, 78, 110, -120},
    {40, -15, -121, 78, 114, -121},
    {32, -13, -91, 79, 113, -121},
    {41, -15, -92, 77, 110, -120},
    {32, -31, -84, 94, 112, -112},
    {44, -45, -106, 95, 111, -110},
    {47, -47, -89, 95, 107, -107},
    {35, -68, -87, 107, 111, -100},
    {28, -73, -84, 112, 114, -98},
    {22, -72, -105, 115, 118, -102},
    {7, -49, -85, 121, 123, -105},
    {24, -48, -90, 108, 116, -107},
    {34, -39, -77, 97, 110, -108},
    {32, -50, -63, 103, 108, -99},
    {13, -30, -65, 110, 118, -109},
    {5, -66, -68, 123, 124, -95},
    {-6, -43, -58, -121, -122, -101},
    {2, -29, -64, 124, 125, -109},
    {-14, -28, -73, -108, -119, -112},
    {-23, -62, -80, -112, -115, -100},
    {-35, -44, -70, -99, -108, -104},
    {-47, -49, -58, -96, -99, -98},
    {-58, -37, -67, -86, -98, -106},
    {-54, -26, -72, -81, -100, -112},
    {-40, -8, -63, -71, -104, -121},
    {-32, -23, -53, -88, -105, -110},
    {-39, -10, -41, -73, -96, -117},
    {-58, -21, -46, -77, -90, -109},
    {-60, -10, -38, -70, -86, -116},
    {-59, 9, -49, -57, -91, 119},
    {-43, 0, -58, -63, -101, 126},
    {-42, 12, -57, -52, -101, 118},
    {-22, 4, -59, -56, -112, 124},
    {-22, 18, -41, -35, -107, 110},
    {-36, 10, -41, -52, -97, 117},
    {-49, 43, -44, -34, -93, 95},
    {-45, 49, -53, -30, -98, 96},
    {-19, 45, -39, -16, -108, 92},
    {-6, 44, -53, -5, -122, 99},
    {7, 16, -105, 16, 124, 120},
    {-8, 3, -105, -48, -123, 125},
    {-7, 19, -99, -14, -124, 119},
    {-16, 16, -95, -31, -120, 120},
    {-37, 16, -89, -47, -111, 119},
    {-56, 29, -111, -44, -108, 116},
    {-72, 44, -107, -41, -103, 111},
    {-45, 41, -91, -33, -108, 109},
    {-45, 62, -90, -25, -108, 102},
    {-24, 52, -86, -17, -115, 105},
    {-16, 52, -93, -12, -120, 106},
    {-11, 40, -92, -10, -122, 110},
    {-2, 37, -100, -2, -126, 112},
    {11, 35, -101, 12, 122, 113},
    {1, 48, -102, 0, 126, 109},
    {8, 68, -109, 4, 124, 104},
    {9, 73, -101, 4, 123, 101},
    {26, 68, -91, 14, 115, 101},
    {43, 69, -125, 22, 113, 106},
    {24, 50, -103, 18, 117, 108},
    {43, 36, -114, 35, 112, 114},
    {30, 36, -89, 28, 113, 111},
    {19, 23, -89, 27, 118, 116},
    {16, 26, -75, 22, 118, 113},
    {34, 34, -65, 31, 107, 107},
    {25, 57, -67, 16, 112, 98},
    {36, 53, -59, 24, 104, 97},
    {62, 53, -65, 34, 96, 99},
    {64, 39, -73, 41, 97, 107},
    {63, 20, -62, 51, 94, 114},
    {44, 20, -60, 46, 101, 114},
    {44, 5, -64, 58, 102, 123},
    {33, -2, -72, 65, 109, -125},
    {34, -11, -63, 76, 106, -120},
    {58, -4, -63, 66, 96, -124},
    {67, -33, -68, 82, 95, -108},
    {54, -30, -76, 84, 102, -111},
    {32, -44, -62, 101, 107, -102},
    {27, -21, -66, 90, 111, -114},
    {15, -20, -45, 100, 114, -110},
    {39, -24, -48, 85, 99, -108},
    {46, -32, -40, 88, 92, -99},
    {33, -50, -47, 103, 102, -94},
    {23, -48, -55, 108, 111, -97},
    {10, -50, -39, 118, 116, -90},
    {13, -31, -54, 110, 117, -105},
    {-1, -21, -51, -125, -126, -111},
    {-12, -35, -41, -113, -115, -98},
    {-10, -50, -37, -118, -116, -89},
    {-28, -56, -39, -108, -101, -88},
    {-7, 1, -121, -57, -124, 126},
    {-7, -6, -111, -92, -124, -124},
    {0, -18, -94, 126, 126, -119},
    {10, -25, -93, 111, 122, -116},
    {1, -52, -108, 126, 126, -108},
    {7, -60, -89, 122, 123, -103},
    {-2, -67, -84, -125, -126, -99},
    {-24, -72, -82, -114, -115, -97},
    {-29, -63, -89, -109, -114, -102},
    {-24, -55, -110, -110, -118, -108},
    {-18, -34, -90, -107, -118, -112},
    {57, 19, -94, 50, 104, 118},
    {1, -6, -106, 120, 126, -124},
    {-46, -28, -80, -85, -105, -113},
    {-71, -36, -98, -82, -101, -112},
    {-75, -14, -80, -70, -96, -120},
    {-78, -11, -97, -69, -99, -122},
    {-82, 10, -104, -58, -100, 123},
    {-61, 7, -105, -58, -105, 124},
    {-56, 12, -101, -54, -106, 122},
    {-25, 3, -85, -58, -115, 125},
    {-36, -2, -75, -65, -108, -125},
    {-48, 6, -79, -58, -104, 123},
    {-58, 1, -60, -62, -95, 126},
    {-72, 18, -81, -53, -97, 118},
    {-52, 23, -56, -46, -96, 111},
    {-53, 36, -76, -39, -102, 109},
    {-25, 34, -63, -25, -111, 106},
    {-18, 27, -81, -23, -118, 114},
    {-24, 39, -84, -22, -115, 109},
    {-30, 40, -60, -26, -108, 103},
    {-22, 61, -69, -13, -114, 97},
    {-5, 72, -60, -2, -123, 91},
    {4, 71, -72, 2, 124, 95},
    {15, 50, -68, 11, 118, 101},
    {0, 41, -81, 0, 126, 108},
    {11, 21, -68, 19, 120, 114},
    {-2, 31, -53, -2, -125, 105},
    {10, 39, -59, 10, 120, 103},
    {1, 58, -50, 0, 126, 92},
    {13, 53, -39, 9, 114, 89},
    {24, 55, -35, 16, 102, 86},
    {37, 48, -49, 26, 100, 95},
    {48, 43, -55, 33, 97, 100},
    {28, 26, -59, 33, 109, 110},
    {37, 16, -45, 47, 99, 113},
    {55, 20, -56, 49, 95, 113},
    {61, 6, -37, 59, 85, 120},
    {53, -5, -56, 67, 96, -123},
    {36, -6, -39, 70, 96, -120},
    {1, 7, -27, 5, 125, 116},
    {12, 19, -22, 22, 106, 98},
    {16, 29, -5, 20, 75, 70},
    {18, 20, 6, 29, 50, 51},
    {8, 17, 17, 17, 17, 31},
    {23, 31, 18, 25, 36, 42},
    {32, 20, 18, 40, 42, 33},
    {34, 7, 22, 55, 40, 12},
    {35, -10, 19, 74, 43, -19},
    {22, -23, 14, 96, 40, -41},
    {7, -14, 17, 108, 15, -27},
    {3, -33, 22, 123, 5, -39},
    {-13, -32, 27, -111, -18, -35},
    {-23, -23, 27, -95, -28, -28},
    {-34, -16, 30, -81, -34, -19},
    {-32, -3, 16, -67, -44, -7},
    {-26, 9, 21, -50, -36, 16},
    {-24, 21, 22, -34, -33, 30},
    {-15, 32, 26, -17, -21, 35},
    {-3, 36, 29, -3, -4, 36},
    {10, 28, 27, 13, 14, 32},
    {20, 25, 38, 27, 19, 23},
    {28, 16, 50, 42, 20, 12},
    {37, 2, 43, 61, 28, 1},
    {27, -10, 38, 77, 24, -10},
    {17, -17, 42, 95, 15, -15},
    {3, -23, 37, 121, 3, -22},
    {-5, -21, 46, -117, -4, -17},
    {-20, -9, 42, -80, -17, -8},
    {-17, -1, 55, -65, -12, 0},
    {-15, 17, 59, -29, -10, 11},
    {-4, 22, 61, -7, -2, 13},
    {9, 17, 51, 19, 7, 13},
    {28, 14, 63, 44, 16, 8},
    {24, -2, 63, 66, 14, -1},
    {19, -13, 62, 87, 12, -8},
    {2, -24, 59, 123, 1, -15},
    {-8, -9, 54, -97, -5, -6},
    {-17, -6, 70, -77, -9, -3},
    {-5, 5, 60, -31, -3, 3},
    {-1, -1, 71, -95, 0, 0},
    {10, -1, 85, 67, 4, 0},
    {0, 3, 94, 0, 0, 1},
    {8, -3, 103, 78, 3, -1},
    {0, -10, 121, 126, 0, -3},
    {10, 0, 105, 63, 3, 0},
    {-2, -12, 111, -120, 0, -4},
    {11, 1, 118, 59, 3, 0},
    {-1, -6, 102, -120, 0, -2},
    {6, -3, 125, 82, 1, 0},
    {4, 14, -21, 11, 119, 103},
    {8, 21, -10, 14, 99, 81},
    {-7, 22, -17, -12, -111, 90},
    {0, 33, 3, 0, 0, 59},
    {-19, 22, -1, -28, -65, 65},
    {-28, 22, 5, -36, -56, 54},
    {-38, 18, 0, -45, -63, 63},
    {-42, 6, -5, -57, -68, 91},
    {-38, -12, -5, -75, -68, -79},
    {-37, -21, 6, -84, -57, -52},
    {-27, -29, -9, -96, -76, -75},
    {-22, -36, 0, -104, -63, -63},
    {-7, -40, 2, -120, -52, -61},
    {6, -42, -7, 121, 98, -70},
    {22, -34, -1, 103, 65, -64},
    {37, -25, -5, 87, 68, -71},
    {38, -12, -10, 75, 73, -91},
    {45, -2, -7, 65, 69, -115},
    {48, 18, -3, 48, 66, 70},
    {33, 29, -10, 34, 75, 76},
    {25, 35, -2, 25, 66, 65},
    {19, 38, -7, 18, 77, 70},
    {7, 45, -24, 6, 115, 83},
    {-8, 46, -22, -6, -112, 81},
    {-18, 38, -33, -17, -106, 92},
    {-31, 32, -35, -31, -97, 97},
    {-42, 23, -30, -43, -88, 100},
    {-52, 6, -19, -58, -77, 114},
    {-45, 0, -34, -63, -89, 126},
    {-47, -6, -24, -68, -82, -117},
    {-40, -19, -32, -81, -90, -105},
    {-48, -25, -16, -82, -76, -86},
    {-41, -36, -21, -92, -82, -84},
    {-22, -41, -13, -107, -85, -75},
    {-19, -39, -28, -108, -102, -88},
    {-8, -58, -22, -121, -112, -78},
    {8, -46, -27, 120, 115, -84},
    {16, -39, -29, 111, 106, -89},
    {31, -39, -25, 99, 90, -86},
    {39, -27, -14, 87, 77, -82},
    {57, -31, -20, 83, 77, -86},
    {58, -9, -25, 69, 79, -113},
    {51, 1, -32, 62, 86, 125},
    {58, 17, -28, 51, 81, 104},
    {48, 29, -18, 41, 78, 85},
    {36, 40, -21, 29, 84, 83},
    {19, 36, -16, 19, 91, 80},
    {5, 29, -32, 6, 120, 97},
    {5, 22, -45, 9, 122, 108},
    {27, 32, -53, 28, 107, 105},
};
const int8_t axis_min[3] = {-82, -73, -125};
const int8_t axis_max[3] = {82, 73, 125};

const LED *const get_led_positions(void) {
    return led_positions_array;
}

const LED *const leds = get_led_positions();

const int num_leds = sizeof(led_positions_array) / sizeof(led_positions_array[0]);
