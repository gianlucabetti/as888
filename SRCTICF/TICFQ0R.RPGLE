000100040119      *===============================================================*
000200140523      * TICFQ0R    * Confronto fatturazione con tariffe in UNITRAGRU
000300040119      *===============================================================*
000400040119
000500040119     h decedit('0,') datedit(*dmy/)
000600040119     h option(*nodebugio)
000700040119
000800040119      *---------------------------------------------------------------*
000900040119
001000171113     FTNTBE01L  if   e           k disk
001100140407     FticfQ0d   cf   e             workstn
001200040119      *
001300140407     D msg             S             78    DIM(6) CTDATA PERRCD(1)
001400050614
001500050526     d kpjba         e ds
001600130123
001700050614     d param           ds
001800050614     D  pardrf                 1      8  0
001900140407     D  pardtc                 9     16  0
002000140407     D  pardal                17     24  0
002100140407     D  paral                 25     32  0
002200140407     d  parmlib               33     42
002300140407     d  parmlibb              43     52
002400130718
002500130718     d wData           ds
002600130718     D  Wgiorno                1      2  0
002700130718     D  Wmese                  3      4  0
002800130718     D  Wanno                  5      8  0
002900130123
003000050613     d Azuteds       e ds                  Extname(Azute00f)
003100050613     d dDatiute      e ds
003200050613     d Tibs34ds      e ds                  Inz
003300050614     D WLBDA8          DS                  INZ
003400050614     D  G08DAT                 1      8  0
003500050614     D  G08INV                 9     16  0
003600050614     D  G08ERR                17     17
003700050614     D  G08TGI                18     22  0
003800130130
003900171113     d w008a           s              8a
004000130718     d w0140           s             14  0
004100130130     d Err_lib         s              1
004200130130     d Libreria        s             10
004300130130     d Testo           s             50
004400140523     d Wmesel          s              2
004500140523       // -?Campi Data
004600140523     d wData_ISO       s               d   datfmt(*iso)
004700140523     d wData_EUR       s               d   datfmt(*eur)
004800140523     d wDate           s              8s 0 inz
004900040119
005000040119      *
005100050613     c     *entry        plist
005200050613     c                   parm                    kpjba
005300130123      *
005400050613     c     *dtaara       define    §azute        azuteds
005500050613     c     *dtaara       define    §datiute      ddatiute
005600050613     c                   in(E)     *dtaara
005700050613     c                   If        %error  or RSUT = *blanks
005800050613     c                   CLEAR                   tibs34ds
005900050613     c                   CALL      'TIBS34R'
006000050613     c                   PARM                    tibs34ds
006100050613     c                   in        *dtaara
006200050613     c                   EndIf
006300130718      *
006400130718      * Reperisco data corrente
006500130718     c                   time                    w0140
006600130718     c                   move      w0140         wData
006700140407
006800140407     C                   move      W0140         G08dat
006900140407     C                   CALL      'XSRDA8'
007000140407     C                   PARM                    WLBDA8
007100140407     C                   Z-ADD     G08INV        dateu             8 0
007200050614     c*
007300050614     c                   clear                   v1cdrf
007400140407     c                   clear                   v1cdtc
007500140407     c                   clear                   v1cdal
007600140407     c                   clear                   v1cal
007700171113       // imposto la data di riferimento che trovo sulla tabella IAC
007800171113         chain ('IAC':'TICFQ0R') TNTBE01L;
007900171113         IF  %found(TNTBE01L);
008000171113           w008a = %subst(TBEuni:1:8);
008100171113         ENDIF;
008200171113         IF  w008a <> *blanks and w008a > *zeros;
008300171113           V1Cdrf =%dec(w008a:8:0);
008301171113     c                   clear                   wlbda8
008302171113     c                   move      V1Cdrf        G08inv
008303171113     C                   movel     '3'           G08err
008304171113     C                   CALL      'XSRDA8'
008305171113     C                   PARM                    WLBDA8
008306171113     C                   Z-ADD     G08dat        V1Cdrf
008400171113         ENDIF;
008500140407      *
008600170104      * imposto come data riferimento il 31 / 12 dell'anno precedente
008700171113     c**!!!!!!!          eval      v1cdrf = (3112 * 10000)+(wanno -1)
008800170104      * se il lancio è gennaio vado indietro di 2 anni
008900171113     c**!!!!!!!          if        wmese = 01
009000171113     c**!!!!!!!          eval      v1cdrf = (3112 * 10000)+(wanno -2)
009100171113     c**!!!!!!!          endif
009200171113
009300140523      * imposto periodo da elaborare inizio e fine mese precedente
009400140523     c                   eval      Wgiorno = 01
009500140523     C                   move      Wdata         G08dat
009600140523     C                   CALL      'XSRDA8'
009700140523     C                   PARM                    WLBDA8
009800140523     C                   Z-ADD     G08INV        wdate
009900140523      /free
010000140523         wData_ISO = %date(wDate:*iso);
010100140523         wData_EUR = wData_ISO - %months(1);
010200140523         wData_ISO = wData_EUR;
010300140523         V1cdal    = %dec(wData_EUR);
010400140523         wData_ISO = %date(wDate:*iso);
010500140523         wData_EUR = wData_ISO - %days(1);
010600140523         wData_ISO = wData_EUR;
010700140523         V1cal     = %dec(wData_EUR);
010800140523      /end-free
010900130718     c
011000050614     c* emissione video
011100050613     c     emiss         tag
011200140407     C                   exfmt     cfq0d01
011300050614     c                   setoff                                       9028
011400050613     C                   if        *in03
011500050613     C                   goto      fine
011600050613     c                   endif
011700111220      * controlli del video
011800050614     c                   exsr      ctrd01
011900050614     c                   if        *in06 = *off  or *in90 = *on
012000050613     c                   goto      emiss
012100050613     c                   endif
012200140526      * VERIFICO SE ESISTE GIÀ LA LIBRERIA DI SALVATAGGIO
012300140526      * se esiste pulisco i file all'interno
012400140526     c                   eval      wmesel = %subst(%editc(pardal:'X'): 5: 2)
012500140526      * verifico se esiste la libreria di salvataggio se esiste deleto i file presenti
012600140526     c                   eval      libreria = 'EDPCONF_' + wMESEL
012700140528     c                   eval      Testo='Conf. Fatturazione Bolle ' +
012800140528     c                                    %editc(pardal: 'X') + '/' +
012900140528     c                                     %editc(paral: 'X')
013000140526     C                   call      'TICF89C'
013100140526     C                   parm      *blanks       Err_lib
013200140526     C                   parm                    Libreria
013300140526     c                   If        ERR_lib = 'E'
013400140526     c                   movel(p)  libreria      kpjbu
013500140528     c                   eval      %subst(kpjbu:11:50) = testo
013600140526     c                   call      'TICFQ8C'
013700140526     C                   parm                    Kpjba
013800140526     c                   else
013900130130      * se ok i controlli e F6 ci conferma devo creare la libreria di salvataggio
014000140407      * per il confronto Bolle da fatturare
014100130130     c                   movel(p)  libreria      kpjbu
014200130130     c                   eval      %subst(kpjbu:11:50) = testo
014300140526     c                   call      'TICFQ0C'
014400130130     C                   parm                    Kpjba
014500140526     c                   endif
014600130130
014700140523     c                   eval      parmlib = 'EDPCONF_TA'
014800140407     c                   eval      parmlibb= libreria
014900140407     c                   eval      kcoaz = 'CFQ9'
015000140407     c                   movel(p)  param         kpjbu
015100140407     c                   call      'BCH09'
015200140407     c                   parm                    kpjba
015300140407     c                   call      'BCH10'
015400140407     c                   parm                    kpjba
015500050613     c     fine          tag
015600050613     c                   seton                                        lr
015700050614     c**********************************************************************
015800050614     C     ctrd01        begsr
015900050614     c                   setoff                                       4041
016000140407     c                   setoff                                       424344
016100050614     c                   clear                   pardrf
016200140407     c                   clear                   pardtc
016300140407     c                   clear                   pardal
016400140407     c                   clear                   paral
016500140407     c* controllo data di riferimento
016600121217     c* .....obbligatorietà
016700121217     c                   if        v1cdrf = *zeros
016800050614     c                   seton                                        902840
016900050614     c                   movel     msg(1)        v1cmsg
017000050614     c                   goto      endctr
017100050614     c                   endif
017200050614     c* .....correttezza
017300080714     c                   If        v1cdrf > *zeros
017400050614     c                   clear                   wlbda8
017500050614     C                   z-add     v1cdrf        G08dat
017600050614     C                   CALL      'XSRDA8'
017700050614     C                   PARM                    WLBDA8
017800050614IF  2C     G08ERR        IFEQ      '1'
017900050614     C                   seton                                        409028
018000050614     C                   MOVEL     MSG(2)        v1cmsg
018100050614     c                   goto      endctr
018200050614E   2C                   ENDIF
018300050614     C                   Z-ADD     G08DAT        V1cdrf
018400050614     C                   Z-ADD     G08INV        pardrf            8 0
018500080714     c                   endif
018600140407     c* controllo data tariffa corrente
018700140407     c* non .obbligatoria
018800140407     c* .....correttezza
018900140407     c                   If        v1cdtc > *zeros
019000140407     c                   clear                   wlbda8
019100140407     C                   z-add     v1cdtc        G08dat
019200140407     C                   CALL      'XSRDA8'
019300140407     C                   PARM                    WLBDA8
019400140407IF  2C     G08ERR        IFEQ      '1'
019500140407     C                   seton                                        429028
019600140407     C                   MOVEL     MSG(2)        v1cmsg
019700140407     c                   goto      endctr
019800140407E   2C                   ENDIF
019900140407     C                   Z-ADD     G08DAT        V1cdtc
020000140407     C                   Z-ADD     G08INV        pardtc            8 0
020100140407     c                   endif
020200050614     c*
020300140407     c* controllo periodo da elaborare
020400050614     c* .....obbligatorietà
020500140407     c     v1cdal        ifeq      *zeros
020600050614     c                   seton                                        902841
020700050614     c                   movel     msg(3)        v1cmsg
020800050614     c                   goto      endctr
020900050614     c                   endif
021000140407     c* .....obbligatorietà
021100140407     c     v1cal         ifeq      *zeros
021200140407     c                   seton                                        902843
021300140407     c                   movel     msg(3)        v1cmsg
021400140407     c                   goto      endctr
021500140407     c                   endif
021600050614     c* .....correttezza
021700140407     c                   If        v1cdal > *zeros
021800140407     c                   clear                   wlbda8
021900140407     C                   z-add     v1cdal        G08dat
022000140407     C                   CALL      'XSRDA8'
022100140407     C                   PARM                    WLBDA8
022200140407IF  2C     G08ERR        IFEQ      '1'
022300140407     C                   seton                                        419028
022400140407     C                   MOVEL     MSG(2)        v1cmsg
022500140407     c                   goto      endctr
022600140407E   2C                   ENDIF
022700140407     C                   Z-ADD     G08DAT        V1cdal
022800140407     C                   Z-ADD     G08INV        pardal            8 0
022900140407     c                   endif
023000140407     c* .....correttezza
023100140407     c                   If        v1cal > *zeros
023200140407     c                   clear                   wlbda8
023300140407     C                   z-add     v1cal         G08dat
023400140407     C                   CALL      'XSRDA8'
023500140407     C                   PARM                    WLBDA8
023600140407IF  2C     G08ERR        IFEQ      '1'
023700140407     C                   seton                                        439028
023800140407     C                   MOVEL     MSG(2)        v1cmsg
023900140407     c                   goto      endctr
024000140407E   2C                   ENDIF
024100140407     C                   Z-ADD     G08DAT        V1cal
024200140407     C                   Z-ADD     G08INV        paral             8 0
024300140407     c                   endif
024400140407      * verifico sia iniziale che finale maggiore di oggi
024500140407     c                   if        pardal > Dateu
024600140407     C                   seton                                        419028
024700140407     C                   MOVEL     MSG(4)        v1cmsg
024800140407     c                   goto      endctr
024900140407     C                   ENDIF
025000140407     c                   if        paral > Dateu
025100140407     C                   seton                                        439028
025200140407     C                   MOVEL     MSG(4)        v1cmsg
025300140407     c                   goto      endctr
025400140407     C                   ENDIF
025500140407      * verifico la congruenza tra le due date
025600140407     c                   if        pardal > paral
025700140407     C                   seton                                        439028
025800140407     C                   MOVEL     MSG(6)        v1cmsg
025900140407     c                   goto      endctr
026000140407     c                   Endif
026100050614     c     endctr        endsr
026200050614**
026300050614Data obbligatoria
026400050614Data errata
026500140407Periodo da elaborare obbligatorio
026600140407La data non può essere maggiore di oggi
026700130130ATTENZIONE è presente la libreria di salvataggio
026800140407La data dal deve essere minore di data al
