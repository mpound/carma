#ifndef color_h
#define color_h
/*
 * Define single-color ramp functions.
 */
static float grey_l[]  = {0.0,1.0};
static float grey_c[]  = {0.0,1.0};
static float blank_c[] = {0.0,0.0};
/*
 * Define a rainbow color table.
 */
static float rain_l[] = {-0.5, 0.0, 0.17, 0.33, 0.50, 0.67, 0.83, 1.0, 1.7};
static float rain_r[] = { 0.0, 0.0,  0.0,  0.0,  0.6,  1.0,  1.0, 1.0, 1.0};
static float rain_g[] = { 0.0, 0.0,  0.0,  1.0,  1.0,  1.0,  0.6, 0.0, 1.0};
static float rain_b[] = { 0.0, 0.3,  0.8,  1.0,  0.3,  0.0,  0.0, 0.0, 1.0};

/*
 * Iraf "heat" color table.
 */
static float heat_l[] = {0.0, 0.2, 0.4, 0.6, 1.0};
static float heat_r[] = {0.0, 0.5, 1.0, 1.0, 1.0};
static float heat_g[] = {0.0, 0.0, 0.5, 1.0, 1.0};
static float heat_b[] = {0.0, 0.0, 0.0, 0.3, 1.0};

/*
 * Weird IRAF ramp color table.
 */
static float ramp_l[] = {0.0, 0.5, 0.5, 0.7, 0.7, 0.85, 0.85, 0.95, 0.95, 1.0};
static float ramp_r[] = {0.0, 1.0, 0.0, 0.0, 0.3,  0.8,  0.3,  1.0,  1.0, 1.0};
static float ramp_g[] = {0.0, 0.5, 0.4, 1.0, 0.0,  0.0,  0.2,  0.7,  1.0, 1.0};
static float ramp_b[] = {0.0, 0.0, 0.0, 0.0, 0.4,  1.0,  0.0,  0.0, 0.95, 1.0};

/*
 * AIPS tvfiddle discrete rainbow color table.
 */
static float aips_l[] = {0.0, 0.1, 0.1, 0.2, 0.2, 0.3, 0.3, 0.4, 0.4, 0.5,
			 0.5, 0.6, 0.6, 0.7, 0.7, 0.8, 0.8, 0.9, 0.9, 1.0};
static float aips_r[] = {0.0, 0.0, 0.3, 0.3, 0.5, 0.5, 0.0, 0.0, 0.0, 0.0,
			 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0};
static float aips_g[] = {0.0, 0.0, 0.3, 0.3, 0.0, 0.0, 0.0, 0.0, 0.8, 0.8,
			 0.6, 0.6, 1.0, 1.0, 1.0, 1.0, 0.8, 0.8, 0.0, 0.0};
static float aips_b[] = {0.0, 0.0, 0.3, 0.3, 0.7, 0.7, 0.7, 0.7, 0.9, 0.9,
			 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};

#ifdef A_SIZE
#undef A_SIZE
#endif
#define A_SIZE(lev) sizeof(lev)/sizeof(lev[0])

typedef struct {
  const char *name;
  int n;
  float *l;
  float *r;
  float *g;
  float *b;
} Cmap;

typedef enum {
C_AIPS,
C_BLUE,
C_GREEN,
C_GREY,
C_HEAT,
C_NONE,
C_RAIN,
C_RAMP,
C_RED
} Cnum;


static Cmap std_cmaps[] = {
  {"aips",    A_SIZE(aips_l),   aips_l,  aips_r,  aips_g,  aips_b},
  {"blue",    A_SIZE(grey_l),   grey_l, blank_c, blank_c,  grey_c},
  {"green",   A_SIZE(grey_l),   grey_l, blank_c,  grey_c, blank_c},
  {"grey",    A_SIZE(grey_l),   grey_l,  grey_c,  grey_c,  grey_c},
  {"heat",    A_SIZE(heat_l),   heat_l,  heat_r,  heat_g,  heat_b},
  {"none",                 0,        0,       0,       0,       0},
  {"rainbow", A_SIZE(rain_l),   rain_l,  rain_r,  rain_g,  rain_b},
  {"ramp",    A_SIZE(ramp_l),   ramp_l,  ramp_r,  ramp_g,  ramp_b},
  {"red",     A_SIZE(grey_l),   grey_l,  grey_c, blank_c, blank_c},
};
static int n_std_cmap = A_SIZE(std_cmaps);
/*
	  ctab->contra = 5.0 * kp.y / (kp.y < 0.0f ? mp->wya : -mp->wyb);
	  ctab->bright = 0.5 + 1.0 * (fabs(ctab->contra)+1.0) *
	    ((kp.x - mp->wxb)/(mp->wxa - mp->wxb) - 0.5);

*/

#endif
