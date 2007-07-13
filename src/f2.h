
#ifdef _MBCS
#pragma comment(lib, "Rdll.lib") 
#endif

#ifdef WIN32
	#define PRE_FUNC extern "C" __declspec(dllexport)
#else
	#define PRE_FUNC extern "C"
#endif //#ifdef WIN32


PRE_FUNC void f2 (int *dim, int *mat, int *fk, double *Fk, double *w) ;
