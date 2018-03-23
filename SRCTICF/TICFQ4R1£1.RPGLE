000100040119      *===============================================================*
000200160223      * TICFQ4R1   * Confronto Fatturazione Nuovo - Librerie
000300040119      *===============================================================*
000400040119
000500040119     h decedit('0,') datedit(*dmy/)
000600040119     h option(*nodebugio)
000700040119
000800040119      *---------------------------------------------------------------*
000900040119      *
001000140407     D msg             S             78    DIM(6) CTDATA PERRCD(1)
001100050614
001200050526     d kpjba         e ds
001300130123
001400050614     d param           ds
001500050614     D  pardrf                 1      8  0
001600140407     D  pardtc                 9     16  0
001700140407     D  pardal                17     24  0
001800140407     D  paral                 25     32  0
001900140407     d  parmlib               33     42
002000140407     d  parmlibb              43     52
002100130718
002200130718     d wData           ds
002300130718     D  Wgiorno                1      2  0
002400130718     D  Wmese                  3      4  0
002500130718     D  Wanno                  5      8  0
002600130123
002700050613     d Azuteds       e ds                  Extname(Azute00f)
002800050613     d dDatiute      e ds
002900050613     d Tibs34ds      e ds                  Inz
003000050614     D WLBDA8          DS                  INZ
003100050614     D  G08DAT                 1      8  0
003200050614     D  G08INV                 9     16  0
003300050614     D  G08ERR                17     17
003400050614     D  G08TGI                18     22  0
003500130130
003600130718     d w0140           s             14  0
003700130130     d Err_lib         s              1
003800130130     d Libreria        s             10
003900130130     d Testo           s             50
004000140523     d Wmesel          s              2
004100140523       // -?Campi Data
004200140523     d wData_ISO       s               d   datfmt(*iso)
004300140523     d wData_EUR       s               d   datfmt(*eur)
004400140523     d wDate           s              8s 0 inz
004500040119
004600040119      *
004700050613     c     *entry        plist
004800050613     c                   parm                    kpjba
004900130123      *
005000050613     c     *dtaara       define    §azute        azuteds
005100050613     c     *dtaara       define    §datiute      ddatiute
005200050613     c                   in(E)     *dtaara
005300050613     c                   If        %error  or RSUT = *blanks
005400050613     c                   CLEAR                   tibs34ds
005500050613     c                   CALL      'TIBS34R'
005600050613     c                   PARM                    tibs34ds
005700050613     c                   in        *dtaara
005800050613     c                   EndIf
005900130718      *
006000130718      * Reperisco data corrente
006100130718     c                   time                    w0140
006200130718     c                   move      w0140         wData
006300140407
006400140407     C                   move      W0140         G08dat
006500140407     C                   CALL      'XSRDA8'
006600140407     C                   PARM                    WLBDA8
006700140407     C                   Z-ADD     G08INV        dateu             8 0
006800160223
006900140526      * VERIFICO SE ESISTE GIÀ LA LIBRERIA DI SALVATAGGIO
007000140526      * se esiste pulisco i file all'interno
007100140526     c                   eval      wmesel = %subst(%editc(pardal:'X'): 5: 2)
007200140526      * verifico se esiste la libreria di salvataggio se esiste deleto i file presenti
007300160223     c                   eval      %subst(prmlibb:8:1) = 'M'
007400140528     c                   eval      Testo='Conf. Fatturazione Bolle ' +
007500140528     c                                    %editc(pardal: 'X') + '/' +
007600140528     c                                     %editc(paral: 'X')
007700140526     C                   call      'TICF89C'
007800140526     C                   parm      *blanks       Err_lib
007900140526     C                   parm                    Libreria
008000140526     c                   If        ERR_lib = 'E'
008100140526     c                   movel(p)  libreria      kpjbu
008200140528     c                   eval      %subst(kpjbu:11:50) = testo
008300140526     c                   call      'TICFQ8C'
008400140526     C                   parm                    Kpjba
008500140526     c                   else
008600130130      * se ok i controlli e F6 ci conferma devo creare la libreria di salvataggio
008700140407      * per il confronto Bolle da fatturare
008800130130     c                   movel(p)  libreria      kpjbu
008900130130     c                   eval      %subst(kpjbu:11:50) = testo
009000140526     c                   call      'TICFQ0C'
009100130130     C                   parm                    Kpjba
009200140526     c                   endif
009300130130
009400140407     c                   eval      parmlibb= libreria
009500140407     c                   call      'BCH09'
009600140407     c                   parm                    kpjba
009700140407     c                   call      'BCH10'
009800140407     c                   parm                    kpjba
009900050613     c     fine          tag
010000050613     c                   seton                                        lr
010100050614     c**********************************************************************
010200050614     C     ctrd01        begsr
010300050614     c                   setoff                                       4041
010400140407     c                   setoff                                       424344
010500050614     c                   clear                   pardrf
010600140407     c                   clear                   pardtc
010700140407     c                   clear                   pardal
010800140407     c                   clear                   paral
010900140407     c* controllo data di riferimento
011000121217     c* .....obbligatorietà
011100121217     c                   if        v1cdrf = *zeros
011200050614     c                   seton                                        902840
011300050614     c                   movel     msg(1)        v1cmsg
011400050614     c                   goto      endctr
011500050614     c                   endif
011600050614     c* .....correttezza
011700080714     c                   If        v1cdrf > *zeros
011800050614     c                   clear                   wlbda8
011900050614     C                   z-add     v1cdrf        G08dat
012000050614     C                   CALL      'XSRDA8'
012100050614     C                   PARM                    WLBDA8
012200050614IF  2C     G08ERR        IFEQ      '1'
012300050614     C                   seton                                        409028
012400050614     C                   MOVEL     MSG(2)        v1cmsg
012500050614     c                   goto      endctr
012600050614E   2C                   ENDIF
012700050614     C                   Z-ADD     G08DAT        V1cdrf
012800050614     C                   Z-ADD     G08INV        pardrf            8 0
012900080714     c                   endif
013000140407     c* controllo data tariffa corrente
013100140407     c* non .obbligatoria
013200140407     c* .....correttezza
013300140407     c                   If        v1cdtc > *zeros
013400140407     c                   clear                   wlbda8
013500140407     C                   z-add     v1cdtc        G08dat
013600140407     C                   CALL      'XSRDA8'
013700140407     C                   PARM                    WLBDA8
013800140407IF  2C     G08ERR        IFEQ      '1'
013900140407     C                   seton                                        429028
014000140407     C                   MOVEL     MSG(2)        v1cmsg
014100140407     c                   goto      endctr
014200140407E   2C                   ENDIF
014300140407     C                   Z-ADD     G08DAT        V1cdtc
014400140407     C                   Z-ADD     G08INV        pardtc            8 0
014500140407     c                   endif
014600050614     c*
014700140407     c* controllo periodo da elaborare
014800050614     c* .....obbligatorietà
014900140407     c     v1cdal        ifeq      *zeros
015000050614     c                   seton                                        902841
015100050614     c                   movel     msg(3)        v1cmsg
015200050614     c                   goto      endctr
015300050614     c                   endif
015400140407     c* .....obbligatorietà
015500140407     c     v1cal         ifeq      *zeros
015600140407     c                   seton                                        902843
015700140407     c                   movel     msg(3)        v1cmsg
015800140407     c                   goto      endctr
015900140407     c                   endif
016000050614     c* .....correttezza
016100140407     c                   If        v1cdal > *zeros
016200140407     c                   clear                   wlbda8
016300140407     C                   z-add     v1cdal        G08dat
016400140407     C                   CALL      'XSRDA8'
016500140407     C                   PARM                    WLBDA8
016600140407IF  2C     G08ERR        IFEQ      '1'
016700140407     C                   seton                                        419028
016800140407     C                   MOVEL     MSG(2)        v1cmsg
016900140407     c                   goto      endctr
017000140407E   2C                   ENDIF
017100140407     C                   Z-ADD     G08DAT        V1cdal
017200140407     C                   Z-ADD     G08INV        pardal            8 0
017300140407     c                   endif
017400140407     c* .....correttezza
017500140407     c                   If        v1cal > *zeros
017600140407     c                   clear                   wlbda8
017700140407     C                   z-add     v1cal         G08dat
017800140407     C                   CALL      'XSRDA8'
017900140407     C                   PARM                    WLBDA8
018000140407IF  2C     G08ERR        IFEQ      '1'
018100140407     C                   seton                                        439028
018200140407     C                   MOVEL     MSG(2)        v1cmsg
018300140407     c                   goto      endctr
018400140407E   2C                   ENDIF
018500140407     C                   Z-ADD     G08DAT        V1cal
018600140407     C                   Z-ADD     G08INV        paral             8 0
018700140407     c                   endif
018800140407      * verifico sia iniziale che finale maggiore di oggi
018900140407     c                   if        pardal > Dateu
019000140407     C                   seton                                        419028
019100140407     C                   MOVEL     MSG(4)        v1cmsg
019200140407     c                   goto      endctr
019300140407     C                   ENDIF
019400140407     c                   if        paral > Dateu
019500140407     C                   seton                                        439028
019600140407     C                   MOVEL     MSG(4)        v1cmsg
019700140407     c                   goto      endctr
019800140407     C                   ENDIF
019900140407      * verifico la congruenza tra le due date
020000140407     c                   if        pardal > paral
020100140407     C                   seton                                        439028
020200140407     C                   MOVEL     MSG(6)        v1cmsg
020300140407     c                   goto      endctr
020400140407     c                   Endif
020500050614     c     endctr        endsr
020600050614**
020700050614Data obbligatoria
020800050614Data errata
020900140407Periodo da elaborare obbligatorio
021000140407La data non può essere maggiore di oggi
021100130130ATTENZIONE è presente la libreria di salvataggio
021200140407La data dal deve essere minore di data al
