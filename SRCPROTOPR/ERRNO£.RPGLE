000100081202     D**************************************************************************
000200081202     D*
000300081202     D* Ritorna un puntatore alla variabile errno che contiene informazioni
000400081202     D* sul funzionamento delle funzioni UNIX.
000500081202     D* http://publib.boulder.ibm.com/infocenter/iseries/v5r4/topic/apis/unix14.htm
000600081202     D*
000700081202     D**************************************************************************
000800081202     D getErrno        PR              *   EXTPROC('__errno')
