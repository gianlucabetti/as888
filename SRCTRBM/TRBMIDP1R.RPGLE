000100050310      /TITLE Upload ins. manuale da Internet: lettura TIVPI00F
000200130905     H dftactgrp(*no) actgrp(*caller)
000300990907
000400130905     Ftimal12l  uf   e           k disk
000500130905     Ftimap01l  uf   e           k disk
000600990907
000700130918     C*-----------------------------------------------------------------------------
000800130918     C*
000900130918     C     *ENTRY        PLIST
001000130918     C                   parm                    chiudi            1
001100130918     C*
001200130918     C                   setoff                                       lr
001300130918     C*
001400130918     C                   if        chiudi = 'S'
001500130918     C                   seton                                        lr
001600130918     C                   endif
001700130905     C*
001800130905     C* Leggo tutta la vista logica
001900130905     C                   dow       not *inlr
002000130905     C                   read      timal12l                               lr
002100130905     C* Se presenti record da leggere
002200130905     C                   if        not *inlr
002300130905     C* Gestione ENDJOB.
002400130905     C                   shtdn                                        lr
002500130905     C                   if        not *inlr
002600130905     C*
002700130905     C* Reperisco l'ID processo con il carico totale più basso
002800130905     C* su cui attribuire il record il record corrente di TIMAL da bilanciare
002900130905     C     *loval        setll     timap01l
003000170329     C                   read      timap01l                               01
003100161114     C*
003200161114     C                   if        not %eof(timap01l)
003300161114     C*
003400161114     C* Se il progressivo più basso è > 999.000.000.000.000
003500161114     C* Azzero tutti i contatori di processo
003600161114     C                   if        mapCOUNT > 999000000000000
003700161114     C                   dow       not *inlr
003800161114     C                   eval      mapCOUNT = 1
003900161114     C                   update    timap000
004000161114     C                   read      timap01l                               lr
004100161114     C                   enddo
004200161114     C*
004300161114     C                   else
004400130905     C*
004500130905     C                   eval      malIDP = mapIDP
004600130905     C                   update    timal000
004700130905     C*
004800130905     C* Incremento con step 1 il contatore del processo con meno carico complessivo
004900130905     C                   eval      mapCOUNT = mapCOUNT + 1
005000130905     C                   update    timap000
005100130905     C                   endif
005200051108     C*
005300170329     C                   else
005400170329     C                   eval      malIDP = '1'
005500170329     C                   update    timal000
005600051108     C                   endif
005700051108     C                   endif
005800161114     C                   endif
005900010525     C*
006000010525     C                   enddo
006100130905     C*
