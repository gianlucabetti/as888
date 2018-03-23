000100031205      *------------------------------------------------------------------------*
000200081121      * controllo se c'e' stesso unificante per vidersi potenziali
000300031205      *------------------------------------------------------------------------*
000400071122     H Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000500031205      *------------------------------------------------------------------------*
000600081124     FWFDEi01l  uF   E           k DISK
000700081124     FWFDEi02l  uF   E           k DISK    rename(wfdei000:wfdei002)
000800081124     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000900031205      *------------------------------------------------------------------------*
001000031205      *   C A M P I   D I   L A V O R O
001100031205      *------------------------------------------------------------------------*
001200081124     d savpiv          s                   like(deipiv)
001300081124     d savcdf          s                   like(deicdf)
001400081124     d totdivp         s              7  0
001500081124     d totugup         s              7  0
001600081124     d totdivf         s              7  0
001700081124     d totuguf         s              7  0
001800081124     d totdive         s              7  0
001900081124     d xx              s              7  0
002000081124     d Indx            s              7  0
002100081124
002200081124     d savcpo          s             11  0 dim(10)
002300081124     d                 ds
002400081124     d  deicpo1
002500081124     d  deicpo2
002600081124     d  deicpo3
002700081124     d  deicpo4
002800081124     d  deicpo5
002900081124     d  deicpo6
003000081124     d  deicpo7
003100081124     d  deicpo8
003200081124     d  deicpo9
003300081124     d   cpo           s             11  0 dim(10)
003400031205      *------------------------------------------------------------------------*
003500031205      *   D S   I N T E R N E / E S T E R N E
003600031205      *------------------------------------------------------------------------*
003700081119     d Kpjba         e ds
003800081119     d TIBS34ds      e ds
003900081119      // - Dati utente
004000081119     d §AzUte        e ds                  extname(AZUTE00F)
004100081119     d                                     dtaara
004200081119     d §DatiUte      e ds                  extname(dDatiUte)
004300081119     d                                     dtaara
004400071122      *------------------------------------------------------------------------*
004500071122      *   P R O T O T I P I
004600071122      *------------------------------------------------------------------------*
004700081119      /copy gaitrasrc/srcprotopr,tibs34r
004800081119      //---------------------------------------------------------------
004900081119      //?M A I N - L I N E
005000081119      //---------------------------------------------------------------
005100081119     c     *Entry        plist
005200081119     c                   parm                    KPJBA
005300081119
005400081119      /free
005500081119
005600081119         // Reperimento dati job
005700081119         exsr DatiJob;
005800080108
005900081121       clear savcpo     ;
006000081124       clear savpiv     ;
006100081121
006200081124       setll  *loval  wfdei01l  ;
006300081124       read    wfdei01l ;
006400071217
006500081124 1     dow  not %eof(wfdei01l) ;
006600081124
006700081124       clear deierr           ;
006800081124       Indx=%scan('$$': deipiv)    ;
006900081128 2     if deipiv<>*blanks and Indx=0   and deicpo1>0       ;
007000081124       cpo(1)=deicpo1    ;
007100081124       cpo(2)=deicpo2    ;
007200081124       cpo(3)=deicpo3    ;
007300081124       cpo(4)=deicpo4    ;
007400081124       cpo(5)=deicpo5    ;
007500081124       cpo(6)=deicpo6    ;
007600081124       cpo(7)=deicpo7    ;
007700081124       cpo(8)=deicpo8    ;
007800081124       cpo(9)=deicpo9    ;
007900081121
008000081121
008100081124 3     if (deipiv=savpiv)  ;
008200081124       // Se stessa PIV verifico se c'e' anche almeno 1 pot uguale
008300081124         xx=1             ;
008400081124 4       dow cpo(xx)>0    ;
008500081124         Indx=%lookup(cpo(XX):savcpo)  ;
008600081124 5       if Indx>0        ;
008700081124           totugup=totugup+1  ;
008800081124           leave            ;
008900081124 5       endif           ;
009000081124         xx=xx+1    ;
009100081124 4       enddo      ;
009200081124
009300081124        // Tutti potenziali diversi  --> segnalo errore
009400081124 4     if Indx=0        ;
009500081124         deierr='Stessa PIV.UNIFICANTE <>potenziali'  ;
009600081124         totdivp=totdivp+1  ;
009700081124 4     endif             ;
009800081124 3     endif             ;
009900081121
010000081128       except AGGdei     ;
010100081124 2     endif             ;
010200081124
010300081124       savpiv =deipiv    ;
010400081124       savcpo =cpo       ;
010500081121
010600081124       READ wfdei01l          ;
010700081124 1     enddo             ;
010800071218
010900081124
011000081124       clear savcpo     ;
011100081124       clear savpiv     ;
011200081124       clear savcdf     ;
011300081124
011400081124       setll  *loval  wfdei02l  ;
011500081124       read    wfdei02l ;
011600081124
011700081124 1     dow  not %eof(wfdei02l) ;
011800081124
011900081124 2     if deicdf<>*blanks ;
012000081124       cpo(1)=deicpo1    ;
012100081124       cpo(2)=deicpo2    ;
012200081124       cpo(3)=deicpo3    ;
012300081124       cpo(4)=deicpo4    ;
012400081124       cpo(5)=deicpo5    ;
012500081124       cpo(6)=deicpo6    ;
012600081124       cpo(7)=deicpo7    ;
012700081124       cpo(8)=deicpo8    ;
012800081124       cpo(9)=deicpo9    ;
012900081124
013000081124
013100081124 3     if (deicdf=savcdf)  ;
013200081124       // Se stessa CDF verifico se c'e' anche almeno 1 pot uguale
013300081124         xx=1             ;
013400081124 4       dow cpo(xx)>0    ;
013500081124         Indx=%lookup(cpo(XX):savcpo)  ;
013600081124 5       if Indx>0        ;
013700081124           totuguf=totuguf+1  ;
013800081124           leave            ;
013900081124 5       endif           ;
014000081124         xx=xx+1    ;
014100081124 4       enddo      ;
014200081124
014300081124        // Tutti potenziali diversi  --> segnalo errore
014400081124 4     if Indx=0        ;
014500081124       if deierr<>'        '    ;
014600081124         deierr='Stessi PIV-CDF.UNIFICANTE<>potenz'  ;
014700081124         totdive=totdive+1  ;
014800081128         totdivp=totdivp-1  ;
014900081124         else  ;
015000081124         deierr='Stessa CDF.UNIFICANTE <>potenziali'  ;
015100081124         totdivf=totdivf+1  ;
015200081124         endif;
015300081124 4     endif             ;
015400081124 3     endif             ;
015500081124
015600081128       except AGGdei2    ;
015700081124 2     endif             ;
015800081124
015900081124       savcdf =deicdf    ;
016000081124       savcpo =cpo       ;
016100081124
016200081124       READ wfdei02l          ;
016300081124 1     enddo             ;
016400081124       except totale     ;
016500081124
016600071121       *inlr=*on         ;
016700081119       //--------------------------------------------------------------
016800081119       //?Reperimento Dati del job (Utente/Operativi).
016900081119       //--------------------------------------------------------------
017000081119       BEGSR DatiJob;
017100081119
017200081119         in(E) §AzUte;
017300081119         if NOT %error;
017400081119           in(E) §DatiUte;
017500081119         endif;
017600081119         if %error or RSut = *blanks;
017700081119           clear TIBS34ds;
017800081119           tibs34r(tibs34ds);
017900081119           in §AzUte;
018000081119           in §DatiUte;
018100081119         endif;
018200081119
018300081119       ENDSR;
018400081124      /end-free
018500071130
018600081128     OWFDEI000  E            AGGDEI
018700081128     O                       DEIERR
018800081128     o
018900081128     OWFDEI002  E            AGGDEI2
019000081128     O                       DEIERR
019100081128     o
019200081124     OQSYSPRT   E            TOTALE           02
019300081124     O                                              'CONTROLLI STESSO UNIFICAN'
019400081124     O                                              'TE diversi Potenziali'
019500081124     OQSYSPRT   E            TOTALE      2
019600081124     O                                              'Totale stessa PIV <> pot.'
019700081124     O                       totdivp       2  +   2
019800081124     O                                        +   2 'Totale stesso CDF <> pot.'
019900081124     O                       totdivf       2  +   2
020000081124     O                                        +   2 'Totale stessi PIVeCDF<>pot'
020100081124     O                       totdive       2  +   2
020200081124     OQSYSPRT   E            TOTALE      2
020300081124     O                                              'Totale stessa PIV =  pot.'
020400081124     O                       totugup       2  +   2
020500081124     O                                        +   2 'Totale stesso CDF =  pot.'
020600081124     O                       totuguf       2  +   2
