000100171113      *===============================================================*
000200180205      * TICFQ01R    * Confronto fatturazione presidio vendite
000300171113      *===============================================================*
000400171113
000500171113     h decedit('0,') datedit(*dmy/)
000600171113     h option(*nodebugio)
000700171113
000800171113      *---------------------------------------------------------------*
000900171113
001000171113     FTNTBE01L  if   e           k disk
001100180205     FticfQ01d  cf   e             workstn
001200171113      *
001300171113     D msg             S             78    DIM(6) CTDATA PERRCD(1)
001400171113
001500171113     d kpjba         e ds
001600171113
001700171113     d param           ds
001800171113     D  pardrf                 1      8  0
001900171113     D  pardtc                 9     16  0
002000171113     D  pardal                17     24  0
002100171113     D  paral                 25     32  0
002200171113     d  parmlib               33     42
002300171113     d  parmlibb              43     52
002400171113
002500171113     d wData           ds
002600171113     D  Wgiorno                1      2  0
002700171113     D  Wmese                  3      4  0
002800171113     D  Wanno                  5      8  0
002900171113
003000171113     d Azuteds       e ds                  Extname(Azute00f)
003100171113     d dDatiute      e ds
003200171113     d Tibs34ds      e ds                  Inz
003300171113     D WLBDA8          DS                  INZ
003400171113     D  G08DAT                 1      8  0
003500171113     D  G08INV                 9     16  0
003600171113     D  G08ERR                17     17
003700171113     D  G08TGI                18     22  0
003800171113
003900171113     d w008a           s              8a
004000171113     d w0140           s             14  0
004100171113     d Err_lib         s              1
004200171113     d Libreria        s             10
004300171113     d Testo           s             50
004400171113     d Wmesel          s              2
004500171113       // -?Campi Data
004600171113     d wData_ISO       s               d   datfmt(*iso)
004700171113     d wData_EUR       s               d   datfmt(*eur)
004800171113     d wDate           s              8s 0 inz
004900171113
005000171113      *
005100171113     c     *entry        plist
005200171113     c                   parm                    kpjba
005300171113      *
005400171113     c     *dtaara       define    §azute        azuteds
005500171113     c     *dtaara       define    §datiute      ddatiute
005600171113     c                   in(E)     *dtaara
005700171113     c                   If        %error  or RSUT = *blanks
005800171113     c                   CLEAR                   tibs34ds
005900171113     c                   CALL      'TIBS34R'
006000171113     c                   PARM                    tibs34ds
006100171113     c                   in        *dtaara
006200171113     c                   EndIf
006300171113      *
006400171113      * Reperisco data corrente
006500171113     c                   time                    w0140
006600171113     c                   move      w0140         wData
006700171113
006800171113     C                   move      W0140         G08dat
006900171113     C                   CALL      'XSRDA8'
007000171113     C                   PARM                    WLBDA8
007100171113     C                   Z-ADD     G08INV        dateu             8 0
007200171113     c*
007300171113     c                   clear                   v1cdrf
007400171113     c                   clear                   v1cdtc
007500171113     c                   clear                   v1cdal
007600171113     c                   clear                   v1cal
007700171113
007800171113       // imposto la data di riferimento che trovo sulla tabella IAC
007900180205         chain ('IAC':'TICFQ0R') TNTBE01L;
008000171113         IF  %found(TNTBE01L);
008100171113           w008a = %subst(TBEuni:1:8);
008200171113         ENDIF;
008300171113         IF  w008a <> *blanks and w008a > *zeros;
008400171113           V1Cdrf =%dec(w008a:8:0);
008500171113     c                   clear                   wlbda8
008600171113     c                   move      V1Cdrf        G08inv
008700171113     C                   movel     '3'           G08err
008800171113     C                   CALL      'XSRDA8'
008900171113     C                   PARM                    WLBDA8
009000171113     C                   Z-ADD     G08dat        V1Cdrf
009100171113         ENDIF;
009200171113
009300171113      * imposto periodo da elaborare inizio e fine mese precedente
009400171113     c                   eval      Wgiorno = 01
009500171113     C                   move      Wdata         G08dat
009600171113     C                   CALL      'XSRDA8'
009700171113     C                   PARM                    WLBDA8
009800171113     C                   Z-ADD     G08INV        wdate
009900171113      /free
010000171113         wData_ISO = %date(wDate:*iso);
010100171113         wData_EUR = wData_ISO - %months(1);
010200171113         wData_ISO = wData_EUR;
010300171113         V1cdal    = %dec(wData_EUR);
010400171113         wData_ISO = %date(wDate:*iso);
010500171113         wData_EUR = wData_ISO - %days(1);
010600171113         wData_ISO = wData_EUR;
010700171113         V1cal     = %dec(wData_EUR);
010800171113      /end-free
010900171113     c
011000171113     c* emissione video
011100171113     c     emiss         tag
011200171113     C                   exfmt     cfq0d01
011300171113     c                   setoff                                       9028
011400171113     C                   if        *in03
011500171113     C                   goto      fine
011600171113     c                   endif
011700171113      * controlli del video
011800171113     c                   exsr      ctrd01
011900171113     c                   if        *in06 = *off  or *in90 = *on
012000171113     c                   goto      emiss
012100171113     c                   endif
012200171113      * VERIFICO SE ESISTE GIÀ LA LIBRERIA DI SALVATAGGIO
012300171113      * se esiste pulisco i file all'interno
012400171113     c                   eval      wmesel = %subst(%editc(pardal:'X'): 5: 2)
012500171113      * verifico se esiste la libreria di salvataggio se esiste deleto i file presenti
012600180205     c                   eval      libreria = 'EDPCONA_' + wMESEL
012700171113     c                   eval      Testo='Conf. Fatturazione Bolle ' +
012800171113     c                                    %editc(pardal: 'X') + '/' +
012900171113     c                                     %editc(paral: 'X')
013000171113     C                   call      'TICF89C'
013100171113     C                   parm      *blanks       Err_lib
013200171113     C                   parm                    Libreria
013300171113     c                   If        ERR_lib = 'E'
013400171113     c                   movel(p)  libreria      kpjbu
013500171113     c                   eval      %subst(kpjbu:11:50) = testo
013600171113     c                   call      'TICFQ8C'
013700171113     C                   parm                    Kpjba
013800171113     c                   else
013900171113      * se ok i controlli e F6 ci conferma devo creare la libreria di salvataggio
014000171113      * per il confronto Bolle da fatturare
014100171113     c                   movel(p)  libreria      kpjbu
014200171113     c                   eval      %subst(kpjbu:11:50) = testo
014300171113     c                   call      'TICFQ0C'
014400171113     C                   parm                    Kpjba
014500171113     c                   endif
014600171113
014700180205     c                   eval      parmlib = 'EDPCONA_TA'
014800171113     c                   eval      parmlibb= libreria
014900180205     c                   eval      kcoaz = 'CFQ5'
015000171113     c                   movel(p)  param         kpjbu
015100171113     c                   call      'BCH09'
015200171113     c                   parm                    kpjba
015300171113     c                   call      'BCH10'
015400171113     c                   parm                    kpjba
015500171113     c     fine          tag
015600171113     c                   seton                                        lr
015700171113     c**********************************************************************
015800171113     C     ctrd01        begsr
015900171113     c                   setoff                                       4041
016000171113     c                   setoff                                       424344
016100171113     c                   clear                   pardrf
016200171113     c                   clear                   pardtc
016300171113     c                   clear                   pardal
016400171113     c                   clear                   paral
016500171113     c* controllo data di riferimento
016600171113     c* .....obbligatorietà
016700171113     c                   if        v1cdrf = *zeros
016800171113     c                   seton                                        902840
016900171113     c                   movel     msg(1)        v1cmsg
017000171113     c                   goto      endctr
017100171113     c                   endif
017200171113     c* .....correttezza
017300171113     c                   If        v1cdrf > *zeros
017400171113     c                   clear                   wlbda8
017500171113     C                   z-add     v1cdrf        G08dat
017600171113     C                   CALL      'XSRDA8'
017700171113     C                   PARM                    WLBDA8
017800171113IF  2C     G08ERR        IFEQ      '1'
017900171113     C                   seton                                        409028
018000171113     C                   MOVEL     MSG(2)        v1cmsg
018100171113     c                   goto      endctr
018200171113E   2C                   ENDIF
018300171113     C                   Z-ADD     G08DAT        V1cdrf
018400171113     C                   Z-ADD     G08INV        pardrf            8 0
018500171113     c                   endif
018600171113     c* controllo data tariffa corrente
018700171113     c* non .obbligatoria
018800171113     c* .....correttezza
018900171113     c                   If        v1cdtc > *zeros
019000171113     c                   clear                   wlbda8
019100171113     C                   z-add     v1cdtc        G08dat
019200171113     C                   CALL      'XSRDA8'
019300171113     C                   PARM                    WLBDA8
019400171113IF  2C     G08ERR        IFEQ      '1'
019500171113     C                   seton                                        429028
019600171113     C                   MOVEL     MSG(2)        v1cmsg
019700171113     c                   goto      endctr
019800171113E   2C                   ENDIF
019900171113     C                   Z-ADD     G08DAT        V1cdtc
020000171113     C                   Z-ADD     G08INV        pardtc            8 0
020100171113     c                   endif
020200171113     c*
020300171113     c* controllo periodo da elaborare
020400171113     c* .....obbligatorietà
020500171113     c     v1cdal        ifeq      *zeros
020600171113     c                   seton                                        902841
020700171113     c                   movel     msg(3)        v1cmsg
020800171113     c                   goto      endctr
020900171113     c                   endif
021000171113     c* .....obbligatorietà
021100171113     c     v1cal         ifeq      *zeros
021200171113     c                   seton                                        902843
021300171113     c                   movel     msg(3)        v1cmsg
021400171113     c                   goto      endctr
021500171113     c                   endif
021600171113     c* .....correttezza
021700171113     c                   If        v1cdal > *zeros
021800171113     c                   clear                   wlbda8
021900171113     C                   z-add     v1cdal        G08dat
022000171113     C                   CALL      'XSRDA8'
022100171113     C                   PARM                    WLBDA8
022200171113IF  2C     G08ERR        IFEQ      '1'
022300171113     C                   seton                                        419028
022400171113     C                   MOVEL     MSG(2)        v1cmsg
022500171113     c                   goto      endctr
022600171113E   2C                   ENDIF
022700171113     C                   Z-ADD     G08DAT        V1cdal
022800171113     C                   Z-ADD     G08INV        pardal            8 0
022900171113     c                   endif
023000171113     c* .....correttezza
023100171113     c                   If        v1cal > *zeros
023200171113     c                   clear                   wlbda8
023300171113     C                   z-add     v1cal         G08dat
023400171113     C                   CALL      'XSRDA8'
023500171113     C                   PARM                    WLBDA8
023600171113IF  2C     G08ERR        IFEQ      '1'
023700171113     C                   seton                                        439028
023800171113     C                   MOVEL     MSG(2)        v1cmsg
023900171113     c                   goto      endctr
024000171113E   2C                   ENDIF
024100171113     C                   Z-ADD     G08DAT        V1cal
024200171113     C                   Z-ADD     G08INV        paral             8 0
024300171113     c                   endif
024400171113      * verifico sia iniziale che finale maggiore di oggi
024500171113     c                   if        pardal > Dateu
024600171113     C                   seton                                        419028
024700171113     C                   MOVEL     MSG(4)        v1cmsg
024800171113     c                   goto      endctr
024900171113     C                   ENDIF
025000171113     c                   if        paral > Dateu
025100171113     C                   seton                                        439028
025200171113     C                   MOVEL     MSG(4)        v1cmsg
025300171113     c                   goto      endctr
025400171113     C                   ENDIF
025500171113      * verifico la congruenza tra le due date
025600171113     c                   if        pardal > paral
025700171113     C                   seton                                        439028
025800171113     C                   MOVEL     MSG(6)        v1cmsg
025900171113     c                   goto      endctr
026000171113     c                   Endif
026100171113     c     endctr        endsr
026200171113**
026300171113Data obbligatoria
026400171113Data errata
026500171113Periodo da elaborare obbligatorio
026600171113La data non può essere maggiore di oggi
026700171113ATTENZIONE è presente la libreria di salvataggio
026800171113La data dal deve essere minore di data al
