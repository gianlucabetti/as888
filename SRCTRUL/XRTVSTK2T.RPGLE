000100030604     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200141002
000300161207
000400161207      // - Campi per procedura esterna
000500161207     d  pStack         s           1000
000600161207     d  pStartPoint    s              3  0
000700161207     d  pEntries       s              3  0
000800161207
000900161207
001000161207      // - Parametri per ricerca Programmi chiamanti
001100161207     d dsStack         ds
001200161207     d  skStack                      10    Dim(100)
001300161207
001400161207
001500141002       // - Pgm per ricerca Programmi chiamati
001600141002     d XRTVSTKR        pr                  extpgm('XRTVSTKR')
001700141002     d  pStack                     1000
001800141002     d  pStartPoint                   3  0
001900141002     d  pEntries                      3  0
002000141002
002100161207     c                   setoff                                       63
002200161207      *
002300141006      /free
002400141006         XRTVSTKR (pStack:pStartPoint:pEntries);
002500141006         dsStack = pStack;
002600161207         if %lookup('FNLR36R   ':skStack) > 0;
002700161207          *in63=*on;
002800161207         endif;
002900141006      /end-free
003000141008      *
003100161207     c                   seton                                        lr
003200161207
