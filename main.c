/*
 *  main.c
 *  Hihi
 *
 *  Created by Dan Knapp on 10/11/09.
 *  Copyright 2009 Dan Knapp. All rights reserved.
 *
 */

#include "Emerald-Frame.h"
#include <stdlib.h>
#include <string.h>


struct location {
    enum ground_type { ground, grass, water } ground_type;
    enum object_type { empty, heart, emerald, rock, tree,
		       arrow_right, arrow_left, arrow_up, arrow_down } object_type;
};

enum orientation {
    unrotated,
    rotated_right,
    rotated_180,
    rotated_left
};


void load_sounds();
void init_al();
void load_textures(EF_Drawable drawable);
void init_gl(EF_Drawable drawable);
void draw(EF_Drawable drawable, void *context);
void draw_tile(int x, int y, int tile, enum orientation orientation);
void frame(EF_Timer timer, void *context);

static uint64_t startup_time;
static GLuint texture_ids[10];
static int last_played_blip_at_frame;
static ALuint audio_buffer_ids[2];
static ALuint audio_source_ids[2];
static struct location level_data[20][20] =
    {
      // Y = 0
      { { water, empty }, { grass, empty }, { grass, empty }, { grass, empty },
	{ ground, empty }, { ground, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { ground, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { ground, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { ground, empty }, { ground, empty }, { ground, empty } },
      // Y = 1
      { { water, empty }, { grass, empty }, { grass, empty }, { grass, empty },
	{ grass, empty }, { ground, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { ground, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { ground, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { ground, empty }, { ground, empty }, { ground, empty } },
      // Y = 2
      { { water, empty }, { water, empty }, { grass, empty }, { grass, empty },
	{ grass, empty }, { grass, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { ground, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { ground, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { ground, empty }, { ground, empty }, { ground, empty } },
      // Y = 3
      { { water, empty }, { water, empty }, { grass, empty }, { grass, empty },
	{ grass, empty }, { grass, empty }, { grass, empty }, { ground, empty },
	{ ground, empty }, { ground, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { ground, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { ground, empty }, { ground, empty }, { ground, empty } },
      // Y = 4
      { { water, empty }, { water, empty }, { water, empty }, { grass, empty },
	{ grass, empty }, { grass, empty }, { grass, empty }, { grass, empty },
	{ ground, empty }, { ground, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { ground, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { ground, empty }, { ground, empty }, { ground, empty } },
      // Y = 5
      { { water, empty }, { water, empty }, { water, empty }, { grass, empty },
	{ grass, empty }, { grass, empty }, { grass, empty }, { grass, empty },
	{ grass, empty }, { ground, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { ground, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { ground, empty }, { ground, empty }, { ground, empty } },
      // Y = 6
      { { water, empty }, { water, empty }, { water, empty }, { ground, empty },
	{ grass, empty }, { grass, empty }, { grass, empty }, { grass, empty },
	{ grass, empty }, { grass, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { ground, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { ground, empty }, { ground, empty }, { ground, empty } },
      // Y = 7
      { { water, empty }, { water, empty }, { ground, empty }, { ground, empty },
	{ grass, empty }, { grass, empty }, { grass, empty }, { grass, empty },
	{ grass, empty }, { grass, empty }, { grass, empty }, { ground, empty },
	{ ground, empty }, { ground, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { ground, empty }, { ground, empty }, { ground, empty } },
      // Y = 8
      { { water, empty }, { water, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { grass, empty }, { grass, empty }, { grass, empty },
	{ grass, empty }, { grass, empty }, { grass, empty }, { grass, empty },
	{ ground, empty }, { ground, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { ground, empty }, { ground, empty }, { ground, empty } },
      // Y = 9
      { { water, empty }, { ground, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { grass, empty }, { grass, empty }, { grass, empty },
	{ grass, empty }, { grass, empty }, { grass, empty }, { grass, empty },
	{ grass, empty }, { ground, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { ground, empty }, { ground, empty }, { ground, empty } },
      // Y = 10
      { { water, empty }, { ground, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { ground, empty }, { grass, empty }, { grass, empty },
	{ grass, empty }, { grass, arrow_up }, { grass, empty }, { grass, empty },
	{ grass, empty }, { grass, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { ground, empty }, { ground, empty }, { ground, empty } },
      // Y = 11
      { { ground, empty }, { ground, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { ground, empty }, { grass, empty }, { grass, empty },
	{ grass, arrow_left }, { grass, empty }, { grass, arrow_right }, { grass, empty },
	{ grass, empty }, { grass, empty }, { grass, empty }, { ground, empty },
	{ ground, empty }, { ground, empty }, { ground, empty }, { ground, empty } },
      // Y = 12
      { { ground, empty }, { ground, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { ground, empty }, { ground, empty }, { grass, empty },
	{ grass, empty }, { grass, arrow_down }, { grass, empty }, { grass, empty },
	{ grass, empty }, { grass, empty }, { grass, empty }, { grass, empty },
	{ ground, empty }, { ground, empty }, { ground, empty }, { ground, empty } },
      // Y = 13
      { { ground, empty }, { ground, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { ground, empty }, { ground, empty }, { grass, empty },
	{ grass, empty }, { grass, empty }, { grass, empty }, { grass, empty },
	{ grass, empty }, { grass, empty }, { grass, empty }, { grass, empty },
	{ grass, empty }, { ground, empty }, { ground, empty }, { ground, empty } },
      // Y = 14
      { { ground, empty }, { ground, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { ground, empty }, { ground, empty }, { ground, empty },
	{ grass, empty }, { grass, empty }, { grass, empty }, { grass, empty },
	{ grass, empty }, { grass, empty }, { grass, empty }, { grass, empty },
	{ grass, empty }, { grass, empty }, { ground, empty }, { ground, empty } },
      // Y = 15
      { { ground, empty }, { ground, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { ground, empty }, { ground, empty }, { ground, empty },
	{ grass, empty }, { grass, empty }, { grass, empty }, { grass, empty },
	{ grass, empty }, { grass, empty }, { grass, empty }, { grass, empty },
	{ grass, empty }, { grass, empty }, { grass, empty }, { ground, empty } },
      // Y = 16
      { { ground, empty }, { ground, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { ground, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { grass, empty }, { grass, empty }, { grass, empty },
	{ grass, empty }, { grass, empty }, { grass, empty }, { grass, empty },
	{ grass, empty }, { grass, empty }, { grass, empty }, { grass, empty } },
      // Y = 17
      { { ground, empty }, { ground, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { ground, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { grass, empty }, { grass, empty }, { grass, empty },
	{ grass, empty }, { grass, empty }, { grass, empty }, { grass, empty },
	{ grass, empty }, { grass, empty }, { grass, empty }, { grass, empty } },
      // Y = 18
      { { ground, empty }, { ground, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { ground, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { ground, empty }, { grass, empty }, { grass, empty },
	{ grass, empty }, { grass, empty }, { grass, empty }, { grass, empty },
	{ grass, empty }, { grass, empty }, { grass, empty }, { grass, empty } },
      // Y = 19
      { { ground, empty }, { ground, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { ground, empty }, { ground, empty }, { ground, empty },
	{ ground, empty }, { ground, empty }, { grass, empty }, { grass, empty },
	{ grass, empty }, { grass, empty }, { grass, empty }, { grass, empty },
	{ grass, empty }, { grass, empty }, { grass, empty }, { grass, empty } }
    };
      

int main(int argc, char **argv) {
    ef_init((utf8 *) "Adventures of Hihi");
    
    ALCdevice *device = alcOpenDevice(NULL);
    ALCcontext *context = alcCreateContext(device, NULL);
    alcMakeContextCurrent(context);
    
    ef_video_set_double_buffer(True);
    ef_video_set_color_size(24);
    ef_video_set_alpha_size(8);
    ef_video_set_depth_size(8);
    ef_video_set_stencil_size(8);
    ef_video_set_accumulation_size(24);
    ef_video_set_samples(5);
    ef_video_set_multisample(True);
    EF_Drawable drawable = ef_video_new_drawable(640, 480, False, NULL);
    ef_drawable_set_draw_callback(drawable, draw, NULL);
    
    load_sounds();
    init_al();
    
    load_textures(drawable);
    init_gl(drawable);
    
    startup_time = ef_time_unix_epoch();
    
    last_played_blip_at_frame = -1;
    
    ef_time_new_repeating_timer(20, frame, (void *) drawable);
    
    ef_main();
}


void load_sounds() {
    alGenBuffers(2, audio_buffer_ids);
    
    utf8 *resource_path = ef_configuration_resource_directory();

    utf8 *test_sound_path;

    test_sound_path = malloc((strlen((char *) resource_path) + 128) * sizeof(utf8));
    strcpy((char *) test_sound_path, (char *) resource_path);
    strcat((char *) test_sound_path, "wing.mp3");
    ef_audio_load_sound_file(test_sound_path, audio_buffer_ids[0]);
    free(test_sound_path);
    
    test_sound_path = malloc((strlen((char *) resource_path) + 128) * sizeof(utf8));
    strcpy((char *) test_sound_path, (char *) resource_path);
    strcat((char *) test_sound_path, "blip.wav");
    ef_audio_load_sound_file(test_sound_path, audio_buffer_ids[1]);
    free(test_sound_path);
}


void init_al() {
    alGenSources(2, audio_source_ids);
    
    alSourceQueueBuffers(audio_source_ids[0], 1, &audio_buffer_ids[0]);
    alSourcei(audio_source_ids[0], AL_LOOPING, AL_TRUE);
    alSourcePlay(audio_source_ids[0]);
    
    alSourceQueueBuffers(audio_source_ids[1], 1, &audio_buffer_ids[1]);
}


void load_textures(EF_Drawable drawable) {
    ef_drawable_make_current(drawable);

    glEnable(GL_TEXTURE_2D);
    
    glGenTextures(10, texture_ids);
    
    utf8 *resource_path = ef_configuration_resource_directory();
    
    utf8 *test_texture_path;
    
    test_texture_path = malloc((strlen((char *) resource_path) + 128) * sizeof(utf8));
    strcpy((char *) test_texture_path, (char *) resource_path);
    strcat((char *) test_texture_path, "tile-ground.png");
    ef_video_load_texture_file(test_texture_path, texture_ids[0], False);
    free(test_texture_path);
    
    test_texture_path = malloc((strlen((char *) resource_path) + 128) * sizeof(utf8));
    strcpy((char *) test_texture_path, (char *) resource_path);
    strcat((char *) test_texture_path, "tile-grass.png");
    ef_video_load_texture_file(test_texture_path, texture_ids[1], False);
    free(test_texture_path);
    
    test_texture_path = malloc((strlen((char *) resource_path) + 128) * sizeof(utf8));
    strcpy((char *) test_texture_path, (char *) resource_path);
    strcat((char *) test_texture_path, "tile-water1.png");
    ef_video_load_texture_file(test_texture_path, texture_ids[2], False);
    free(test_texture_path);
    
    test_texture_path = malloc((strlen((char *) resource_path) + 128) * sizeof(utf8));
    strcpy((char *) test_texture_path, (char *) resource_path);
    strcat((char *) test_texture_path, "tile-water2.png");
    ef_video_load_texture_file(test_texture_path, texture_ids[3], False);
    free(test_texture_path);
    
    test_texture_path = malloc((strlen((char *) resource_path) + 128) * sizeof(utf8));
    strcpy((char *) test_texture_path, (char *) resource_path);
    strcat((char *) test_texture_path, "object-heart.png");
    ef_video_load_texture_file(test_texture_path, texture_ids[4], False);
    free(test_texture_path);
    
    test_texture_path = malloc((strlen((char *) resource_path) + 128) * sizeof(utf8));
    strcpy((char *) test_texture_path, (char *) resource_path);
    strcat((char *) test_texture_path, "object-emerald.png");
    ef_video_load_texture_file(test_texture_path, texture_ids[5], False);
    free(test_texture_path);
    
    test_texture_path = malloc((strlen((char *) resource_path) + 128) * sizeof(utf8));
    strcpy((char *) test_texture_path, (char *) resource_path);
    strcat((char *) test_texture_path, "object-rock.png");
    ef_video_load_texture_file(test_texture_path, texture_ids[6], False);
    free(test_texture_path);
    
    test_texture_path = malloc((strlen((char *) resource_path) + 128) * sizeof(utf8));
    strcpy((char *) test_texture_path, (char *) resource_path);
    strcat((char *) test_texture_path, "object-tree.png");
    ef_video_load_texture_file(test_texture_path, texture_ids[7], False);
    free(test_texture_path);
    
    test_texture_path = malloc((strlen((char *) resource_path) + 128) * sizeof(utf8));
    strcpy((char *) test_texture_path, (char *) resource_path);
    strcat((char *) test_texture_path, "object-arrow.png");
    ef_video_load_texture_file(test_texture_path, texture_ids[8], False);
    free(test_texture_path);
    
    test_texture_path = malloc((strlen((char *) resource_path) + 128) * sizeof(utf8));
    strcpy((char *) test_texture_path, (char *) resource_path);
    strcat((char *) test_texture_path, "character-hihi-down.png");
    ef_video_load_texture_file(test_texture_path, texture_ids[9], False);
    free(test_texture_path);

    for(int i = 0; i < 10; i++) {
	glBindTexture(GL_TEXTURE_2D, texture_ids[i]);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    }
}


void init_gl(EF_Drawable drawable) {
    ef_drawable_make_current(drawable);
    
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrtho(0, 640, 0, 480, -300, 300);
    
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();

    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
}


void draw(EF_Drawable drawable, void *context) {
    uint64_t current_time = ef_time_unix_epoch();
    uint64_t elapsed_frames = (current_time - startup_time) / 20;
    
    glClearColor(0.0f, 0.0f, 0.5f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    glColor4f(1.0f, 1.0f, 1.0f, 1.0f);
    glEnable(GL_BLEND);
    
    for(int y = 0; y < 20; y++) {
	for(int x = 0; x < 20; x++) {
	    struct location *location = &level_data[y][x];

	    switch(location->ground_type) {
	    case ground:
		draw_tile(x*2, y*2, 0, unrotated);
		break;
	    case grass:
		draw_tile(x*2, y*2, 1, unrotated);
		break;
	    case water:
		if((x + y*10 + elapsed_frames/20) % 2 == 0)
		    draw_tile(x*2, y*2, 2, unrotated);
		else
		    draw_tile(x*2, y*2, 3, unrotated);
		break;
	    }

	    switch(location->object_type) {
	    case empty:
		break;
	    case heart:
		draw_tile(x*2, y*2, 4, unrotated);
		break;
	    case emerald:
		draw_tile(x*2, y*2, 5, unrotated);
		break;
	    case rock:
		draw_tile(x*2, y*2, 6, unrotated);
		break;
	    case tree:
		draw_tile(x*2, y*2, 7, unrotated);
		break;
	    case arrow_right:
		draw_tile(x*2, y*2, 8, unrotated);
		break;
	    case arrow_left:
		draw_tile(x*2, y*2, 8, rotated_180);
		break;
	    case arrow_up:
		draw_tile(x*2, y*2, 8, rotated_left);
		break;
	    case arrow_down:
		draw_tile(x*2, y*2, 8, rotated_right);
		break;
	    }
	}
    }
    
    ef_drawable_swap_buffers(drawable);
}


void draw_tile(int x, int y, int tile, enum orientation orientation) {
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, texture_ids[tile]);
    
    int top = 480 - (y * 24);
    int left = x * 24;
    int bottom = top - 48;
    int right = left + 48;

    GLfloat textureMin = 0.0f;
    GLfloat textureMax = 24.0f / 32.0f;
    
    glBegin(GL_QUADS);
    switch(orientation) {
    case unrotated: glTexCoord2f(textureMin, textureMax); break;
    case rotated_right: glTexCoord2f(textureMax, textureMax); break;
    case rotated_180: glTexCoord2f(textureMax, textureMin); break;
    case rotated_left: glTexCoord2f(textureMin, textureMin); break;
    }
    glVertex2s(left, bottom);
    switch(orientation) {
    case unrotated: glTexCoord2f(textureMax, textureMax); break;
    case rotated_right: glTexCoord2f(textureMax, textureMin); break;
    case rotated_180: glTexCoord2f(textureMin, textureMin); break;
    case rotated_left: glTexCoord2f(textureMin, textureMax); break;
    }
    glVertex2s(right, bottom);
    switch(orientation) {
    case unrotated: glTexCoord2f(textureMax, textureMin); break;
    case rotated_right: glTexCoord2f(textureMin, textureMin); break;
    case rotated_180: glTexCoord2f(textureMin, textureMax); break;
    case rotated_left: glTexCoord2f(textureMax, textureMax); break;
    }
    glVertex2s(right, top);
    switch(orientation) {
    case unrotated: glTexCoord2f(textureMin, textureMin); break;
    case rotated_right: glTexCoord2f(textureMin, textureMax); break;
    case rotated_180: glTexCoord2f(textureMax, textureMax); break;
    case rotated_left: glTexCoord2f(textureMax, textureMin); break;
    }
    glVertex2s(left, top);
    glEnd();
}


void frame(EF_Timer timer, void *context) {
    EF_Drawable drawable = (EF_Drawable) context;
    ef_drawable_redraw(drawable);

    uint64_t current_time = ef_time_unix_epoch();
    uint64_t elapsed_frames = (current_time - startup_time) / 20;
    
    int next_blip_frame;
    if(last_played_blip_at_frame == -1)
	next_blip_frame = 0;
    else
	next_blip_frame = last_played_blip_at_frame + 100;
    
    if(elapsed_frames >= next_blip_frame) {
	alSourcePlay(audio_source_ids[1]);
	last_played_blip_at_frame = next_blip_frame;
    }
}
