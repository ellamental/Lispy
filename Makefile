lispy: lispy.c
	cc -I/usr/include/gc -lgc -lm -o lispy lispy.c
