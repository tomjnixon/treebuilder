#include "leds.h"

LED led_positions_array[] = {
    {-7, 8, -113, -29, -124, 124},
    {-11, 8, -120, -38, -123, 124},
    {-22, 15, -112, -39, -119, 121},
    {-29, 26, -110, -33, -116, 117},
    {-48, 25, -97, -44, -108, 116},
    {-45, 35, -104, -36, -110, 113},
    {-28, 35, -96, -27, -115, 112},
    {-48, 46, -90, -32, -107, 107},
    {-55, 53, -96, -32, -105, 106},
    {-61, 66, -88, -30, -102, 100},
    {-53, 66, -96, -27, -106, 102},
    {-39, 77, -87, -18, -109, 97},
    {-33, 71, -96, -17, -113, 101},
    {-25, 61, -88, -15, -115, 102},
    {-21, 51, -103, -15, -118, 108},
    {-8, 39, -107, -8, -123, 112},
    {-9, 25, -97, -13, -123, 116},
    {-19, 36, -78, -19, -117, 109},
    {-8, 49, -73, -6, -122, 103},
    {-25, 40, -74, -22, -113, 106},
    {-17, 62, -66, -10, -116, 96},
    {-24, 63, -78, -14, -114, 99},
    {-39, 69, -65, -20, -105, 94},
    {-21, 74, -69, -11, -115, 93},
    {-21, 81, -62, -10, -113, 89},
    {-9, 77, -66, -4, -121, 92},
    {3, 72, -72, 1, 125, 95},
    {3, 67, -69, 1, 125, 95},
    {5, 56, -72, 3, 124, 100},
    {2, 50, -65, 1, 125, 100},
    {2, 33, -75, 2, 125, 110},
    {9, 32, -55, 11, 120, 105},
    {-3, 48, -47, -2, -124, 94},
    {6, 53, -42, 4, 121, 90},
    {-7, 70, -44, -4, -120, 86},
    {4, 77, -45, 2, 123, 84},
    {15, 74, -45, 8, 114, 85},
    {24, 63, -46, 14, 107, 88},
    {22, 57, -45, 14, 108, 90},
    {21, 34, -45, 22, 109, 100},
    {11, 26, -49, 16, 118, 107},
    {16, 18, -39, 29, 111, 109},
    {31, 18, -25, 42, 90, 101},
    {43, 29, -27, 39, 86, 93},
    {43, 40, -21, 33, 81, 83},
    {32, 52, -26, 22, 91, 82},
    {18, 54, -22, 13, 99, 79},
    {9, 50, -31, 7, 115, 85},
    {14, 29, -24, 18, 105, 91},
    {11, 31, -24, 13, 109, 90},
    {24, 0, -4, 63, 70, 126},
    {31, -8, -2, 73, 66, -73},
    {23, -21, -8, 93, 77, -78},
    {20, -35, -2, 106, 67, -65},
    {-1, -34, -1, -125, -95, -64},
    {-10, -43, -9, -117, -93, -71},
    {-15, -23, -15, -103, -95, -86},
    {-34, -31, -13, -93, -78, -79},
    {-23, -13, -17, -84, -89, -100},
    {-41, -14, -19, -76, -81, -101},
    {-35, 2, -20, -61, -84, 122},
    {-53, 7, -12, -58, -72, 105},
    {-33, 16, -16, -45, -81, 95},
    {-40, 4, 2, -59, -61, 44},
    {-41, -2, 7, -65, -56, -11},
    {-22, -12, 1, -83, -61, -60},
    {-28, -22, 20, -90, -38, -33},
    {-18, -26, 7, -102, -48, -52},
    {-12, -32, 19, -112, -22, -41},
    {1, -23, 10, 125, 4, -46},
    {23, -29, 18, 99, 36, -41},
    {27, -13, 20, 81, 37, -23},
    {1, -9, 19, 122, 2, -17},
    {8, -21, 34, 112, 9, -22},
    {-2, -21, 21, -123, -3, -31},
    {-22, -22, 32, -95, -24, -24},
    {-22, -2, 26, -67, -28, -3},
    {-37, -14, 25, -78, -39, -20},
    {-29, 0, 18, -63, -41, 0},
    {-36, 0, 40, -63, -29, 0},
    {-33, 0, 45, -63, -25, 0},
    {-28, -9, 55, -76, -19, -6},
    {-23, -15, 56, -86, -15, -10},
    {-10, 0, 43, -63, -9, 0},
    {-9, -22, 49, -111, -7, -17},
    {-4, -24, 43, -120, -3, -20},
    {2, -8, 39, 117, 2, -8},
    {1, -4, 58, 117, 0, -2},
    {12, -13, 45, 96, 10, -11},
    {11, -6, 28, 83, 15, -8},
    {-5, -3, 36, -85, -5, -3},
    {5, 13, 31, 14, 6, 16},
    {3, 4, 16, 26, 7, 9},
    {14, -5, 15, 77, 30, -13},
    {15, 6, 6, 48, 48, 31},
    {38, 6, 9, 57, 54, 23},
    {30, 5, -11, 56, 77, 109},
    {30, 18, -5, 41, 70, 74},
    {23, 24, -9, 30, 78, 78},
    {10, 17, -19, 21, 107, 97},
    {-2, 36, -9, -2, -118, 73},
    {21, 22, 2, 30, 59, 59},
    {18, 26, 19, 24, 30, 37},
    {21, 26, 18, 27, 34, 39},
    {8, 10, 15, 27, 19, 23},
    {1, 9, 16, 4, 2, 20},
    {21, 7, 31, 50, 24, 8},
    {12, 10, 27, 35, 16, 14},
    {18, 2, 48, 59, 14, 1},
    {14, 16, 61, 29, 9, 10},
    {9, 16, 69, 20, 5, 9},
    {2, 4, 67, 18, 1, 2},
    {0, -4, 79, 126, 0, -2},
    {-1, 0, 98, -63, 0, 0},
    {-2, 0, 96, -63, 0, 0},
    {-1, 4, 108, -9, 0, 1},
    {-7, 3, 102, -47, -2, 1},
    {-9, 3, 114, -50, -3, 1},
    {-3, 2, 118, -39, -1, 0},
    {5, 0, 113, 63, 1, 0},
    {3, 1, 103, 50, 1, 0},
    {5, 1, 98, 55, 2, 0},
    {12, 0, 115, 63, 4, 0},
    {-3, 0, 125, -63, 0, 0},
    {-15, 4, 113, -52, -5, 1},
    {-12, 2, 96, -56, -5, 0},
    {-4, 0, 88, -63, -1, 0},
    {-9, 6, 77, -39, -4, 3},
    {-6, -5, 63, -91, -3, -3},
    {-20, 3, 63, -57, -12, 1},
    {-4, 18, 64, -8, -2, 11},
    {-18, 18, 57, -31, -12, 12},
    {-12, 22, 46, -20, -10, 18},
    {-16, 24, 40, -23, -15, 21},
    {-6, 32, 43, -7, -5, 25},
    {6, 32, 36, 7, 6, 29},
    {16, 20, 27, 27, 21, 25},
    {-3, 29, 29, -4, -4, 31},
    {-10, 30, 23, -13, -16, 37},
    {-18, 20, 18, -29, -31, 33},
    {-26, 18, 40, -39, -23, 17},
    {-35, 13, 30, -49, -34, 16},
    {-36, 16, 11, -46, -51, 39},
    {-34, 36, 11, -30, -50, 51},
    {-35, 37, -2, -30, -65, 65},
    {-24, 45, -4, -19, -70, 67},
    {-13, 23, -6, -20, -81, 73},
    {-4, 25, -7, -6, -106, 74},
    {-8, 48, -3, -6, -78, 66},
    {-5, 36, -17, -5, -115, 81},
    {9, 3, -106, 50, 123, 125},
    {9, 4, -121, 46, 123, 125},
    {27, 13, -110, 45, 117, 122},
    {28, 18, -107, 40, 116, 120},
    {35, 11, -105, 51, 114, 122},
    {52, 14, -103, 52, 108, 121},
    {57, 24, -103, 47, 106, 117},
    {68, 29, -105, 47, 103, 116},
    {62, 14, -106, 54, 105, 121},
    {81, 30, -106, 49, 100, 115},
    {87, 25, -107, 52, 99, 117},
    {91, 7, -109, 60, 98, 124},
    {86, 7, -105, 60, 99, 124},
    {78, -4, -108, 65, 101, -125},
    {61, 0, -104, 63, 105, 126},
    {48, -11, -109, 72, 110, -122},
    {34, 0, -109, 63, 114, 126},
    {26, 5, -97, 55, 116, 124},
    {15, 2, -89, 58, 120, 126},
    {30, 7, -81, 54, 112, 123},
    {39, -18, -82, 81, 109, -118},
    {43, 10, -76, 54, 106, 121},
    {57, 10, -69, 56, 99, 121},
    {43, -7, -69, 70, 104, -122},
    {65, 1, -73, 62, 97, 126},
    {66, -7, -79, 67, 98, -123},
    {70, -22, -60, 75, 92, -112},
    {59, -21, -73, 77, 99, -115},
    {44, -29, -63, 87, 102, -109},
    {44, -15, -82, 76, 107, -119},
    {32, -25, -67, 90, 108, -112},
    {22, -8, -62, 77, 113, -121},
    {11, -17, -58, 103, 119, -115},
    {27, -8, -49, 75, 106, -120},
    {42, -23, -49, 83, 98, -109},
    {30, -25, -50, 91, 105, -108},
    {54, -25, -48, 81, 92, -107},
    {46, -41, -50, 92, 96, -99},
    {36, -48, -53, 100, 102, -97},
    {20, -51, -50, 111, 111, -94},
    {17, -38, -47, 109, 112, -99},
    {6, -18, -52, 114, 122, -113},
    {1, -14, -32, 124, 125, -110},
    {-4, -36, -30, -122, -121, -91},
    {0, -48, -26, 126, 126, -83},
    {11, -56, -23, 119, 108, -79},
    {21, -52, -23, 111, 97, -80},
    {24, -47, -35, 107, 102, -89},
    {31, -39, -29, 99, 93, -89},
    {13, -35, -25, 112, 107, -88},
    {0, -10, -100, 126, 126, -122},
    {10, -18, -116, 106, 123, -120},
    {13, -29, -103, 109, 121, -115},
    {26, -18, -107, 87, 117, -120},
    {45, -12, -104, 74, 110, -122},
    {50, -24, -98, 81, 107, -117},
    {28, -32, -98, 97, 115, -114},
    {49, -30, -93, 85, 107, -114},
    {36, -40, -95, 97, 112, -110},
    {59, -39, -92, 87, 103, -110},
    {60, -49, -86, 91, 102, -106},
    {48, -55, -99, 97, 108, -106},
    {40, -63, -90, 104, 110, -102},
    {27, -55, -97, 108, 115, -106},
    {18, -49, -97, 112, 119, -108},
    {14, -35, -96, 111, 121, -112},
    {7, -15, -104, 109, 124, -121},
    {8, -15, -86, 107, 123, -120},
    {11, -25, -79, 110, 121, -114},
    {15, -38, -78, 111, 119, -108},
    {28, -47, -69, 105, 111, -102},
    {17, -45, -75, 112, 117, -105},
    {18, -56, -81, 114, 118, -102},
    {31, -57, -82, 106, 112, -102},
    {24, -72, -80, 114, 115, -97},
    {16, -76, -79, 118, 118, -96},
    {-2, -68, -69, -125, -125, -95},
    {3, -55, -71, 124, 125, -100},
    {-13, -48, -67, -116, -119, -101},
    {-8, -29, -75, -116, -122, -112},
    {-2, -16, -66, -121, -125, -117},
    {-5, -36, -52, -121, -123, -102},
    {6, -48, -47, 121, 121, -94},
    {0, -64, -48, 126, 126, -89},
    {-13, -70, -47, -119, -116, -87},
    {-22, -61, -52, -112, -110, -92},
    {-32, -52, -51, -104, -104, -94},
    {-28, -43, -49, -103, -106, -97},
    {-20, -33, -46, -105, -110, -101},
    {-13, -28, -66, -109, -119, -110},
    {0, -16, -70, 126, 126, -117},
    {-9, -3, -74, -76, -122, -125},
    {-10, 5, -70, -44, -121, 124},
    {5, 4, -68, 36, 124, 124},
    {0, -3, -65, 126, 126, -125},
    {-3, -1, -73, -76, -125, -126},
    {-7, 13, -88, -19, -123, 121},
    {-7, -5, -75, -88, -123, -124},
    {-15, 6, -91, -48, -120, 124},
    {-5, 11, -107, -17, -125, 122},
    {16, 4, -111, 53, 121, 125},
    {9, 9, -124, 31, 124, 124},
    {5, 23, -125, 8, 125, 119},
    {3, 25, -110, 4, 125, 117},
    {19, 39, -112, 18, 120, 113},
    {31, 44, -94, 24, 114, 109},
    {31, 46, -107, 23, 115, 110},
    {43, 65, -106, 23, 111, 104},
    {43, 69, -101, 22, 110, 102},
    {37, 80, -98, 17, 112, 99},
    {30, 71, -99, 16, 115, 101},
    {12, 81, -95, 5, 121, 98},
    {10, 70, -99, 5, 122, 102},
    {22, 59, -107, 14, 118, 106},
    {8, 45, -102, 7, 123, 110},
    {-2, 61, -98, -1, -126, 104},
    {-2, 51, -93, -1, -126, 106},
    {3, 32, -105, 3, 125, 115},
    {8, 23, -101, 13, 123, 117},
    {0, 24, -87, 0, 126, 116},
    {25, 24, -87, 32, 115, 116},
    {37, 27, -77, 38, 108, 113},
    {21, 33, -85, 22, 117, 112},
    {14, 53, -76, 10, 119, 102},
    {27, 55, -70, 18, 112, 100},
    {31, 31, -71, 31, 110, 110},
    {39, 43, -64, 29, 104, 103},
    {47, 65, -66, 25, 102, 95},
    {55, 57, -57, 31, 96, 95},
    {61, 43, -59, 38, 94, 101},
    {46, 34, -71, 37, 103, 108},
    {40, 31, -59, 36, 102, 107},
    {28, 16, -70, 42, 111, 117},
    {20, 16, -65, 36, 114, 117},
    {31, 29, -55, 33, 106, 107},
    {41, 35, -49, 34, 98, 101},
    {50, 22, -45, 46, 93, 108},
    {56, 38, -47, 39, 91, 99},
    {60, 25, -44, 47, 89, 106},
    {65, 12, -44, 56, 87, 116},
    {47, 0, -48, 63, 95, 126},
    {40, -1, -46, 64, 98, -126},
    {20, 2, -47, 59, 110, 125},
    {25, 1, -31, 61, 99, 125},
    {28, -11, -25, 78, 92, -110},
    {48, -17, -26, 77, 83, -103},
    {53, -10, -21, 71, 78, -109},
    {60, 4, -27, 60, 80, 121},
    {53, 13, -29, 53, 83, 109},
    {37, 6, -26, 57, 88, 117},
    {-1, -11, -103, -123, -126, -122},
    {-13, 0, -119, -63, -122, 126},
    {-26, -6, -114, -72, -117, -124},
    {-41, -7, -100, -70, -111, -124},
    {-47, -29, -97, -85, -108, -115},
    {-61, -20, -96, -76, -104, -118},
    {-67, -21, -96, -75, -102, -118},
    {-55, -20, -96, -77, -105, -118},
    {-64, -20, -93, -75, -102, -118},
    {-75, -15, -89, -71, -98, -120},
    {-85, -30, -90, -77, -96, -114},
    {-81, -13, -98, -69, -99, -121},
    {-91, -15, -84, -70, -93, -119},
    {-79, -3, -98, -65, -99, -125},
    {-75, 6, -89, -60, -98, 124},
    {-61, -1, -102, -64, -105, -126},
    {-41, 7, -101, -56, -111, 124},
    {-31, 8, -99, -53, -114, 123},
    {-11, 16, -86, -24, -121, 119},
    {-41, 2, -84, -61, -108, 126},
    {-54, 20, -76, -49, -102, 116},
    {-48, 10, -72, -55, -103, 121},
    {-68, -1, -73, -64, -96, -126},
    {-54, 13, -69, -53, -100, 119},
    {-73, 3, -77, -61, -96, 125},
    {-79, 8, -65, -59, -91, 122},
    {-85, 21, -70, -53, -91, 115},
    {-68, 33, -66, -45, -94, 108},
    {-56, 32, -72, -42, -100, 110},
    {-49, 26, -65, -43, -100, 111},
    {-30, 27, -73, -33, -111, 112},
    {-23, 14, -73, -41, -114, 119},
    {-20, 25, -55, -27, -112, 109},
    {-29, 34, -49, -28, -105, 102},
    {-50, 21, -51, -47, -95, 111},
    {-59, 28, -54, -45, -93, 107},
    {-61, 43, -48, -38, -90, 97},
    {-43, 52, -50, -27, -98, 94},
    {-40, 55, -44, -25, -97, 90},
    {-32, 47, -43, -24, -101, 93},
    {-27, 40, -45, -24, -105, 97},
    {-12, 28, -48, -16, -117, 105},
    {-6, 29, -39, -8, -120, 101},
    {-5, 34, -27, -5, -119, 90},
    {1, 55, -24, 0, 125, 80},
    {-8, 61, -23, -5, -113, 78},
    {-22, 59, -23, -14, -96, 78},
    {-33, 51, -23, -23, -88, 80},
    {-16, 47, -24, -13, -103, 82},
    {-31, 38, -31, -27, -95, 91},
    {-7, -10, -107, -102, -124, -123},
    {-11, -23, -116, -108, -123, -119},
    {-9, -36, -109, -117, -123, -114},
    {-29, -43, -98, -103, -115, -110},
    {-5, -36, -101, -121, -124, -113},
    {1, -51, -101, 126, 126, -108},
    {-5, -67, -98, -123, -124, -102},
    {-11, -69, -94, -120, -122, -101},
    {-21, -57, -95, -112, -118, -105},
    {-14, -81, -101, -120, -121, -99},
    {-21, -62, -90, -113, -117, -102},
    {-41, -75, -97, -106, -110, -100},
    {-44, -65, -100, -102, -110, -103},
    {-43, -45, -98, -96, -110, -109},
    {-35, -40, -106, -97, -114, -112},
    {-21, -22, -108, -96, -119, -118},
    {-19, -9, -96, -81, -119, -123},
    {-26, 0, -94, -63, -116, 126},
    {-26, -26, -90, -95, -115, -115},
    {-36, -20, -70, -84, -107, -115},
    {-22, -25, -77, -97, -115, -114},
    {-28, -49, -78, -106, -113, -104},
    {-38, -54, -75, -102, -108, -101},
    {-39, -35, -72, -93, -106, -108},
    {-56, -51, -77, -93, -101, -103},
    {-66, -49, -65, -89, -94, -100},
    {-69, -32, -72, -81, -96, -110},
    {-62, -27, -69, -80, -97, -111},
    {-52, -15, -71, -74, -101, -118},
    {-40, -19, -69, -81, -105, -116},
    {-26, -1, -73, -65, -113, -126},
    {-33, -1, -53, -64, -104, -126},
    {-29, -13, -50, -80, -105, -116},
    {-43, -32, -52, -89, -99, -104},
    {-54, -31, -47, -84, -92, -103},
    {-69, -21, -46, -75, -87, -109},
    {-68, -15, -42, -72, -85, -113},
    {-62, 2, -50, -62, -90, 125},
    {-46, -12, -48, -73, -96, -117},
    {-42, 9, -42, -54, -95, 118},
    {-29, 3, -49, -59, -105, 124},
    {-22, 9, -29, -47, -100, 114},
    {-26, 21, -24, -36, -93, 97},
    {-47, 27, -22, -42, -81, 91},
    {-56, 22, -30, -48, -83, 101},
    {-62, 10, -24, -57, -78, 111},
    {-55, -5, -28, -67, -82, -119},
    {-41, 10, -22, -53, -83, 109},
    {-38, -5, -29, -68, -89, -120},
    {-26, -12, -32, -81, -99, -112},
};
const int8_t axis_min[3] = {-91, -81, -125};
const int8_t axis_max[3] = {91, 81, 125};

const LED *const get_led_positions(void) {
    return led_positions_array;
}

const LED *const leds = get_led_positions();
