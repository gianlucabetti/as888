000100031205      *------------------------------------------------------------------------*
000200081124      * controllo se c'e' stesso potenziale per vidersi unificanti
000300031205      *------------------------------------------------------------------------*
000400071122     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000500031205      *------------------------------------------------------------------------*
000600081121     FWFDEid1l  uF   E           k DISK
000700081124     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000800031205      *------------------------------------------------------------------------*
000900031205      *   C A M P I   D I   L A V O R O
001000031205      *------------------------------------------------------------------------*
001100081121     d savrag          s                   like(deirag)
001200081216     d savunif         s                   like(deikscu)
001300081121     d savcpo          s                   like(deicpo)
001400081124     d savpiv          s                   like(deipiv)
001500081124     d savcdf          s                   like(deicdf)
001600081124     d totdiv          s              7  0
001700081124     d totugu          s              7  0
001800031205      *------------------------------------------------------------------------*
001900031205      *   D S   I N T E R N E / E S T E R N E
002000031205      *------------------------------------------------------------------------*
002100081119     d Kpjba         e ds
002200081119     d TIBS34ds      e ds
002300081119      // - Dati utente
002400081119     d §AzUte        e ds                  extname(AZUTE00F)
002500081119     d                                     dtaara
002600081119     d §DatiUte      e ds                  extname(dDatiUte)
002700081119     d                                     dtaara
002800071122      *------------------------------------------------------------------------*
002900071122      *   P R O T O T I P I
003000071122      *------------------------------------------------------------------------*
003100081119      /copy gaitrasrc/srcprotopr,tibs34r
003200081119      //---------------------------------------------------------------
003300081119      //?M A I N - L I N E
003400081119      //---------------------------------------------------------------
003500081119     c     *Entry        plist
003600081119     c                   parm                    KPJBA
003700081119
003800081119      /free
003900081119
004000081119         // Reperimento dati job
004100081119         exsr DatiJob;
004200080108
004300081121       clear savcpo     ;
004400081121       clear savunif    ;
004500081121       clear savrag     ;
004600081124       clear savpiv     ;
004700081124       clear savcdf     ;
004800081121
004900081121       setll  *loval  wfdeid1l  ;
005000081121       read    wfdeid1l ;
005100071217
005200081121 1     dow  not %eof(wfdeid1l) ;
005300081124
005400081124       clear deierr           ;
005500081121
005600081121
005700081121       if deicpo=savcpo  and savcpo>0;
005800081124       if (deipiv<>savpiv and deipiv<>*blanks and savpiv>*blanks)  or
005900081124          (deicdf<>savcdf and deicdf<>*blanks and savcdf>*blanks)   ;
006000081124         deierr='STESSO POTENZIALE, <> unificanti'     ;
006100081124         totdiv=totdiv+1  ;
006200081124       else              ;
006300081124         totugu=totugu+1  ;
006400081121       endif             ;
006500081121       endif             ;
006600081121
006700081128       except AGGdei     ;
006800081124
006900081121       savcpo =deicpo    ;
007000081216       savunif=deikscu   ;
007100081121       savrag =deirag    ;
007200081124       savpiv =deipiv    ;
007300081124       savcdf =deicdf    ;
007400081121
007500081121       READ wfdeid1l          ;
007600081121       enddo             ;
007700071218
007800081124       except totale     ;
007900081124
008000071121       *inlr=*on         ;
008100081119       //--------------------------------------------------------------
008200081119       //?Reperimento Dati del job (Utente/Operativi).
008300081119       //--------------------------------------------------------------
008400081119       BEGSR DatiJob;
008500081119
008600081119         in(E) §AzUte;
008700081119         if NOT %error;
008800081119           in(E) §DatiUte;
008900081119         endif;
009000081119         if %error or RSut = *blanks;
009100081119           clear TIBS34ds;
009200081119           tibs34r(tibs34ds);
009300081119           in §AzUte;
009400081119           in §DatiUte;
009500081119         endif;
009600081119
009700081119       ENDSR;
009800081124      /end-free
009900071130
010000081128     OWFDEID00  E            AGGDEI
010100081128     O                       DEIERR
010200081128     o
010300081124     OQSYSPRT   E            TOTALE           02
010400081124     O                                              'CONTROLLI STESSO POTENZIA'
010500081124     O                                              'LE diversi unificanti'
010600081124     OQSYSPRT   E            TOTALE      2
010700081124     O                                              'Totale stesso pot <> unif'
010800081124     O                       totdiv        2  +   2
010900081124     O                                        +   2 'Totale stesso pot <> unif'
011000081124     o                                        +   0 ' con stessa PIV/CDF'
011100081124     O                       totugu        2  +   2
