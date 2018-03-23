000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300991027
000400090302
000500090302     D*------------------
000600090302     D* LINKING A DEFINIZIONI ESTERNE
000700090302     D*------------------
000800090302     D/COPY GAITRASRC/SRCPROTOPR,UBIFSCHKLK
000900090302     D/COPY GAITRASRC/SRCPROTOPI,UBIFSCHKLK
001000090302
001100021007
001200090302
001300991027     C                   EXSR      traduci
001400921023     C                   SETON                                        LR
001500991027
001600081007
001700081007
001800991027     C     traduci       BEGSR
001900991027     C*
002000090302     C                   exsr      existsLINK
002100991027     C*
002200910830     C                   ENDSR
002300090302
002400090302
002500090302
002600090302
002700090302     C     existsLINK    BEGSR
002800090302     C*
002900090302     C* valorizzazione proprietà
003000110429     C                   eval      pInFullIFS =
003100110429     C                                          '/QNTC/CEDINTRA/APPDATA/' +
003200110429     C                                          'POD/CLIENTI/0635587/PI/' +
003300110429     C                                          '1.TIF'
003400090302     C*
003500090302     C* Chiamata metodo Found
003600090302     C                   if        UBIFSCHKLK_Found(pInPathIFS
003700090302     C                                             :pInFileIFS
003800090302     C                                             :pInFullIFS
003900090302     C                                             :pOutResult) = 0
004000090302     C                   if        pOutResult = '*FOUND'
004100090302     C                   else
004200090302     C                   endif
004300090302     C                   endif
004400090302     C*
004500090302     C                   ENDSR
