
#include <math.h>
#include <GL/glut.h>
#include <string.h>
#include <stdlib.h>
#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/unixsupport.h>
#include <caml/signals.h>
#include <caml/config.h>

float angle=0.0,deltaAngle = 0.0,ratio;
float x=0.0f,y=1.75f,z=5.0f;
float lx=0.0f,ly=0.0f,lz=-1.0f;
int deltaMove = 0;
int font=(int)GLUT_BITMAP_TIMES_ROMAN_10;//GLUT_BITMAP_8_BY_13;


void changeSize(int w, int h)
	{

	// Prevent a divide by zero, when window is too short
	// (you cant make a window of zero width).
	if(h == 0)
		h = 1;

	ratio = 1.0f * w / h;
	// Reset the coordinate system before modifying
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	
	// Set the viewport to be the entire window
    glViewport(0, 0, w, h);

	// Set the clipping volume
	gluPerspective(45,ratio,0.1,1000);
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	gluLookAt(x, y, z, 
		      x + lx,y + ly,z + lz,
			  0.0f,1.0f,0.0f);


	}



void initScene() {

	glEnable(GL_DEPTH_TEST);

}



void orientMe(float ang) {


	lx = sin(ang);
	lz = -cos(ang);
	glLoadIdentity();
	gluLookAt(x, y, z, 
		      x + lx,y + ly,z + lz,
			  0.0f,1.0f,0.0f);
}


void moveMeFlat(int i) {
	x = x + i*(lx)*0.01;
	z = z + i*(lz)*0.01;
	glLoadIdentity();
	gluLookAt(x, y, z, 
		      x + lx,y + ly,z + lz,
			  0.0f,1.0f,0.0f);
}


void renderBitmapCharacher(float x, float y, float z, void *font,char *string)
{
  
  char *c;
  glRasterPos3f(x, y,z);
  for (c=string; *c != '\0'; c++) {
    glutBitmapCharacter(font, *c);
  }
}

void renderScene(void) {

	if (deltaMove)
		moveMeFlat(deltaMove);
	if (deltaAngle) {
		angle += deltaAngle;
		orientMe(angle);
	}
	
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glPushMatrix();

	renderBitmapCharacher(0,1.8,0,(void *)font,"3D Tech");
	renderBitmapCharacher(0,1.7,0,(void *)font,"GLUT Tutorial");

	glPopMatrix();

	glutSwapBuffers();
}





void pressKey(int key, int x, int y) {

	switch (key) {
		case GLUT_KEY_LEFT : deltaAngle = -0.01f;break;
		case GLUT_KEY_RIGHT : deltaAngle = 0.01f;break;
		case GLUT_KEY_UP : deltaMove = 1;break;
		case GLUT_KEY_DOWN : deltaMove = -1;break;
	}
}

void releaseKey(int key, int x, int y) {

	switch (key) {
		case GLUT_KEY_LEFT : 
		case GLUT_KEY_RIGHT : deltaAngle = 0.0f;break;
		case GLUT_KEY_UP : 
		case GLUT_KEY_DOWN : deltaMove = 0;break;
	}
}

void processMenuEvents(int option) {

		font = option;
}


void createMenus() {
	int menu = glutCreateMenu(processMenuEvents);

	glutAddMenuEntry("8 by 13",(int)GLUT_BITMAP_8_BY_13);
	glutAddMenuEntry("9 by 15",(int)GLUT_BITMAP_9_BY_15);
	glutAddMenuEntry("Times Roman 10",(int)GLUT_BITMAP_TIMES_ROMAN_10);
	glutAddMenuEntry("Times Roman 24",(int)GLUT_BITMAP_TIMES_ROMAN_24);
	glutAddMenuEntry("Helvetica 10",(int)GLUT_BITMAP_HELVETICA_10);
	glutAddMenuEntry("Helvetica 12",(int)GLUT_BITMAP_HELVETICA_12);
	glutAddMenuEntry("Helvetica 18",(int)GLUT_BITMAP_HELVETICA_18);
	glutAttachMenu(GLUT_RIGHT_BUTTON);
}

void processNormalKeys(unsigned char key, int x, int y) {

	if (key == 27) 
		exit(0);
}

// int main(int argc, char **argv)
// {
// 	glutInit(&argc, argv);
// 	glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA);
// 	glutInitWindowPosition(100,100);
// 	glutInitWindowSize(640,360);
// 	glutCreateWindow("SnowMen from Lighthouse 3D");

// 	initScene();

// 	glutKeyboardFunc(processNormalKeys);
// 	glutIgnoreKeyRepeat(1);
// 	glutSpecialFunc(pressKey);
// 	glutSpecialUpFunc(releaseKey);
// 	createMenus();
// 	glutDisplayFunc(renderScene);
// 	glutIdleFunc(renderScene);

// 	glutReshapeFunc(changeSize);

// 	glutMainLoop();

// 	return(0);
// }

value render_some_text (value unit)
{
  CAMLparam1(unit);
  changeSize (1680, 1030);
  renderScene();//  renderBitmapCharacher(0,1.8,0,(void *)font,"3D Tech");
  CAMLreturn (Val_unit);   
}
