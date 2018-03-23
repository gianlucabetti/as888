000100000906     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200000906     F**********************************************************************
000300000906     F** FNLSZ1R - Controllo/esplosione cod. "CHI SONO"
000400000906     F**********************************************************************
000500010402     D MSG             S             78    DIM(05) CTDATA PERRCD(1)
000600010402     D kpjba         E DS
000700000906     D fnlsz1ds      E DS
000800010402     D tibs02ds      E DS
000900000906     D ds1b          E DS
001000010402     D ddkc          E DS
001100000914     d                 ds
001200000914     d §11f                    1     11s 0
001300000914     d §11                     1     11s 0 DIM(11)
001400000906     D* RADICE + N.PROGRESSIVO DEL SEGNACOLLO BARTOLINI
001500000906     d sscbar1         ds            35
001600000914     d  radsbar1               1      8s 0
001700000914     d  prgsbar1               9     15s 0
001800000914     D  §C                     1     17s 0 dim(17)
001900000906     d sscbar2         ds            35
002000000914     d  radsbar2               1      8s 0
002100000914     d  prgsbar2               9     15s 0
002200000906     D* RADICE + N.PROGRESSIVO CHI SONO BARTOLINI
002300000906     d sscchib1        ds            35
002400000906     d  radchib1               1      2
002500000914     d  prgchib1               3     13s 0
002600000914     d  chkdig1               14     14s 0
002700000906     d sscchib2        ds            35
002800000906     d  radchib2               1      2
002900000914     d  prgchib2               3     13s 0
003000000914     d  chkdig2               14     14s 0
003100000912     d* DS per routine calcolo check digit segnacollo Bartolini (fls/lna...)
003200000912     D                 DS
003300000912     D  §CB                    1     18  0
003400000912     D                                     DIM(18)
003500000912     D  §CA                    1     18
003600000912     D                                     DIM(18)
003700000912     D                 DS
003800000912     D  §2                     1      2  0
003900000912     D                                     DIM(2)
004000000912     D  §2A                    1      2
004100000912     D                                     DIM(2)
004200000912     D*
004300000906     D digits          c                   '0123456789'
004400000906     D nn              s              2  0
004500000906     D wchi            s                   like(iz1ch1)
004600000906     D wchkd           s              1  0
004700010306     D ow1ncl          s             30  0
004800010403     D numchi          s              1  0
004900000906     F*----------------------------------------------------*
005000000906      * INDICATORI
005100000906     C*----------------------------------------------------*
005200000906     C*
005300000906     C     *ENTRY        PLIST
005400010402     c                   parm                    kpjba
005500000907     C                   PARM                    fnlsz1ds
005600000906     c                   PARM                    DS1B
005700000906    1c     iz1tla        ifne      'C'
005800000906     c* inizializzazione campi di output
005900000906     c                   clear                   oz1err
006000000906     c                   clear                   oz1msg
006100000907     c                   clear                   oz1ech
006200000906     c                   clear                   oz1ncl
006300000906     c                   clear                   oz1chi
006400000911     c                   clear                   oz1max
006500000906     c     iz1mod        ifeq      'C'
006600000907     c* controllo cod."CHI SONO"
006700001017     c* Se codice trattamento merce '7P' non faccio nessun controllo
006800001017     c* (DISK A con fnvat clienti euroexpress)
006900001017     c     iz1ctm        ifne      '7P'
007000010817     c* Eseguo comunque sempre i controlli standard.
007100010817     c* Se controlli standard non superati e il cliente è presente in tabell
007200010817     c* "DKC" eseguo i controlli specifici del cliente
007300010817     c                   exsr      chkstd
007400010817    4c     oz1err        ifne      *blanks
007500010402     c                   clear                   tibs02ds
007600010402     c                   movel     'C'           t02mod
007700010402     c                   movel     'DKC'         t02cod
007800010402     c                   movel     iz1ccm        t02ke1
007900010402     c                   call      'TIBS02R'
008000010402     c                   parm                    kpjba
008100010402     c                   parm                    TIBS02DS
008200010817    5c     t02err        ifeq      'E'
008300010402     c     t02err        oreq      'A'
008400010817   x5c                   else
008500010402     c                   movel     t02uni        ddkc
008600010817     c                   clear                   oz1err
008700010817     c                   clear                   oz1msg
008800010817     c                   clear                   oz1ech
008900010817     c                   clear                   oz1ncl
009000010817     c                   clear                   oz1chi
009100010817     c                   clear                   oz1max
009200010402     c                   exsr      chkcli
009300010817    5c                   endif
009400010817    4c                   endif
009500010817    3c                   endif
009600010817   x3c                   else
009700000906     c* determino cod. "CHI SONO" seguente a quello passato
009800000906     c                   exsr      segchi
009900010817    2c                   endif
010000000906    1c                   endif
010100000906    1c     iz1tla        ifeq      ' '
010200000906     c                   return
010300000906   x1c                   else
010400000906     c                   seton                                        lr
010500000906    1c                   endif
010600000906     C*-------------------------------------------------------------------------
010700000906     c* CHKSTD - Controlli standard cod.chi sono
010800000906     C*-------------------------------------------------------------------------
010900000906     c     CHKSTD        BEGSR
011000000906     c* SEGNACOLLO BARTOLINI:FLS LNA NRS NSC ZNC
011100000906     c* flag 12 = B --> diskB che diventa diskC                        NO
011200000906    1c     §1bf12        ifeq      'B'
011300000906     c* Devono essere 18 numeri
011400000906     c* N.B.: Questo controllo + quello che dalla 19 alla 35 pos.siano tutti
011500000906     c*       blank non sarebbe necessario in quanto i campi che lo
011600000906     c*       compongono sono numerici. Lo lascio lo stesso per congruità
011700000906     c*       con il calcolo del dal/al
011800000907     c                   movel     iz1ch1        wchi
011900000906     c     SOPRA         TAG
012000000906     c     digits        check     wchi          nn
012100000906    2c     nn            ifne      19
012200000907     c     wchi          ifeq      iz1ch1
012300000907     c                   movel     '1'           oz1ech
012400000907     c                   else
012500000907     c                   movel     '2'           oz1ech
012600000907     c                   endif
012700000907     c                   movel     '1'           oz1err
012800000906     c                   movel     msg(1)        oz1msg
012900000906     c                   goto      endstd
013000000906    2c                   endif
013100000906     c* dall 19 pos. fino alla fine devono essere tutti blank
013200000906     c     ' '           check     wchi:19       nn
013300000906    2c     nn            ifgt      *zeros
013400000907     c     wchi          ifeq      iz1ch1
013500000907     c                   movel     '1'           oz1ech
013600000907     c                   else
013700000907     c                   movel     '2'           oz1ech
013800000907     c                   endif
013900000906     c                   movel     '1'           oz1err
014000000906     c                   movel     msg(1)        oz1msg
014100000906     c                   goto      endstd
014200000906    2c                   endif
014300000906     c* se ricevuto anche il chi sono 2 ripeto i controlli
014400000906    2c     iz1ch2        ifgt      *zeros
014500000906    3c     iz1ch2        ifne      wchi
014600000907     c                   movel     iz1ch2        wchi
014700000906     c                   goto      sopra
014800000906    3c                   endif
014900000906     c* verifico se dal al
015000000906    3c     iz1ch1        ifne      iz1ch2
015100000906     c                   movel     iz1ch1        sscbar1
015200000906     c                   movel     iz1ch2        sscbar2
015300000906    4c     radsbar1      ifeq      radsbar2
015400010306     c                   eval      ow1ncl=%ABS(prgsbar2 - prgsbar1)
015500010306     c                   add       1             ow1ncl
015600010306     c* se numero colli calcolato maggiore del massimo imposto numero colli
015700010306     c* = max
015800010306     c                   if        ow1ncl > wnclmax
015900010306     c                   eval      oz1ncl = wnclmax
016000010306     c                   else
016100010306     c                   eval      oz1ncl = ow1ncl
016200010306     c                   endif
016300000911     c     prgsbar2      ifgt      prgsbar1
016400000911     c                   movel     '2'           oz1max
016500000911     c                   else
016600000911     c                   movel     '1'           oz1max
016700000911     c                   endif
016800000906     c                   goto      endstd
016900000906    4c                   endif
017000000906     c                   z-add     2             oz1ncl
017100000906     c                   goto      endstd
017200000906   x3c                   else
017300000906     c* dal = al --> 1 collo
017400000906     c                   z-add     1             oz1ncl
017500000906     c                   goto      endstd
017600000906    3c                   endif
017700000906   x2c                   else
017800000906     c                   z-add     1             oz1ncl
017900000906     c                   goto      endstd
018000000906    2c                   endif
018100000906   x1c                   else
018200010402     c* CHI SONO BARTOLINI: AA/CC+12 NUMERI
018300021118      * FM - FedEx Merce
018400021118      * FD - FedEx Documenti
018500000906     c                   eval      sscchib1=iz1ch1
018600000906    2c     radchib1      ifne      'AA'
018700010402    2c     radchib1      andne     'CC'
018800021118    2c     radchib1      andne     'FD'
018900021118    2c     radchib1      andne     'FM'
019000000906     c                   movel     '1'           oz1err
019100000907     c                   movel     '1'           oz1ech
019200000906     c                   movel     msg(1)        oz1msg
019300000906     c                   goto      endstd
019400000906    2c                   endif
019500000906     c* dalla posizione 3 alla posizione 14 devono essere tutti numeri
019600000906     c* e il resto (da 15 a 35) devono essere tutti blank
019700000906     c     digits        check     iz1ch1:3      nn
019800000906    2c     nn            ifne      15
019900000906     c                   movel     '1'           oz1err
020000000907     c                   movel     '1'           oz1ech
020100000906     c                   movel     msg(1)        oz1msg
020200000906     c                   goto      endstd
020300000906    2c                   endif
020400000906     c     ' '           check     iz1ch1:15     nn
020500000906    2c     nn            ifgt      *zeros
020600000906     c                   movel     '1'           oz1err
020700010402     c                   movel     '1'           oz1ech
020800000906     c                   movel     msg(1)        oz1msg
020900000906     c                   goto      endstd
021000000906    2c                   endif
021100000906     c* controllo il check digit
021200000914     c                   move      prgchib1      §11f
021300000906     c                   exsr      m1031
021400000906    2c     chkdig1       ifne      wchkd
021500000906     c                   movel     '1'           oz1err
021600000907     c                   movel     '1'           oz1ech
021700000906     c                   movel     msg(1)        oz1msg
021800000906     c                   goto      endstd
021900000906    2c                   endif
022000000906     c* RIPETO controlli se passato chi sono 2
022100000906    2c     iz1ch2        ifne      *blanks
022200000911     c     iz1ch2        andne     iz1ch1
022300000906     c                   eval      sscchib2=iz1ch2
022400000906    3c     radchib2      ifne      'AA'
022500010402    3c     radchib2      andne     'CC'
022600021118    3c     radchib2      andne     'FD'
022700021118    3c     radchib2      andne     'FM'
022800000906     c                   movel     '1'           oz1err
022900000907     c                   movel     '2'           oz1ech
023000000906     c                   movel     msg(1)        oz1msg
023100000906     c                   goto      endstd
023200000906    3c                   endif
023300000906     c* dalla posizione 3 alla posizione 14 devono essere tutti numeri
023400000906     c* e il resto (da 15 a 35) devono essere tutti blank
023500000906     c     digits        check     iz1ch2:3      nn
023600000906    3c     nn            ifne      15
023700000906     c                   movel     '1'           oz1err
023800000907     c                   movel     '2'           oz1ech
023900000906     c                   movel     msg(1)        oz1msg
024000000906     c                   goto      endstd
024100000906    3c                   endif
024200000906     c     ' '           check     iz1ch2:15     nn
024300000906    3c     nn            ifgt      *zeros
024400000906     c                   movel     '1'           oz1err
024500000907     c                   movel     '2'           oz1ech
024600000906     c                   movel     msg(1)        oz1msg
024700000906     c                   goto      endstd
024800000906    3c                   endif
024900000906     c* controllo il check digit
025000000914     c                   move      prgchib2      §11f
025100000906     c                   exsr      m1031
025200000906    3c     chkdig2       ifne      wchkd
025300000906     c                   movel     '1'           oz1err
025400000907     c                   movel     '2'           oz1ech
025500000906     c                   movel     msg(1)        oz1msg
025600000906     c                   goto      endstd
025700000906    3c                   endif
025800000906     c* verifico se dal al
025900000906     c     radchib1      ifeq      radchib2
026000010306     c                   eval      ow1ncl=%ABS(prgchib2-prgchib1)
026100010306     c                   add       1             ow1ncl
026200010306     c* se numero colli calcolato maggiore del massimo imposto numero colli
026300010306     c* = max
026400010306     c                   if        ow1ncl > wnclmax
026500010306     c                   eval      oz1ncl = wnclmax
026600010306     c                   else
026700010306     c                   eval      oz1ncl = ow1ncl
026800010306     c                   endif
026900000911     c     prgchib2      ifgt      prgchib1
027000000911     c                   movel     '2'           oz1max
027100000911     c                   else
027200000911     c                   movel     '1'           oz1max
027300000911     c                   endif
027400000906     c                   else
027500000906     c                   z-add     2             oz1ncl
027600000906     c                   goto      endstd
027700000906     c                   endif
027800000906   x2c                   else
027900000906     c                   z-add     1             oz1ncl
028000000906    2c                   endif
028100000906     c
028200000906    1c                   endif
028300000906     c     endstd        endsr
028400000906     C*
028500000906     c*
028600000906     C*
028700000906      ** CALCOLA CHECK DIGIT -----------------------------
028800000906     C     m1031         begsr
028900000906      *
029000000906     C                   move      *zeros        §TT2              3 0
029100000906     C                   clear                   w0030             3 0
029200000906     C                   clear                   w0010             1 0
029300000906      *
029400000906      * TOTALIZZO I VALORI, QUELLI NELLE POSIZIONI DISPARI PER 3
029500000906     C                   do        11            xx                2 0
029600000906      *
029700000906     C     xx            div       2             §ctr              3 0
029800000906     C                   mvr                     §RST              3 030
029900000906      *
030000000906     C     *IN30         IFEQ      '1'
030100000906     C     §11(XX)       MULT      3             §CTR
030200000906     C                   ELSE
030300000906     C                   Z-ADD     §11(XX)       §CTR
030400000906     C                   ENDIF
030500000906      *
030600000906     C                   ADD       §CTR          W0030
030700000906     C                   ENDDO
030800000906      *
030900000906     C     W0030         ADD       9             §TT2
031000000906     C                   MOVE      0             §TT2
031100000906     C     §TT2          SUB       W0030         wchkd
031200000906      *
031300000906     C                   ENDSR
031400000912     C*****************************************************
031500000912     C* CALCOLA CIFRA DI CONTROLLO SECONDO IL MODULO 10 IBM
031600000912     C*****************************************************
031700000912     C     M10           BEGSR
031800000912     C                   MOVE      *ZERO         §TT               3 0
031900000912     C                   MOVE      *ZERO         §TT2              3 0
032000000912     C                   MOVE      *ZERO         §1                1 0
032100000912     C                   Z-ADD     18            §S                2 0
032200000912     C                   DO        17            §N                2 0
032300000912     C                   SUB       1             §S
032400000912     C* INTANTO SPOSTO DA UNA SCHIERA AL CAMPO COD.BARRE
032500000912     C                   MOVE      §C(§N)        §CB(§N)
032600000912     C     §N            DIV       2             §CTR              3 0
032700000912     C                   MVR                     §RST              3 010
032800000912     C     *IN(10)       IFEQ      '1'
032900000912     C     §C(§S)        MULT      2             §N2               2 0
033000000912     C                   MOVE      §N2           §A2               2
033100000912     C                   MOVEA     §A2           §2A
033200000912     C                   XFOOT     §2            §CTR
033300000912     C                   ELSE
033400000912     C                   Z-ADD     §C(§S)        §CTR
033500000912     C                   END
033600000912     C                   ADD       §CTR          §TT
033700000912     C                   END
033800000912     C*
033900000912     C     §TT           ADD       9             §TT2
034000000912     C                   MOVE      0             §TT2
034100000912     C     §TT2          SUB       §TT           §1
034200000912     C*
034300000912     C                   MOVE      §1            §CB(18)
034400000912     C                   ENDSR
034500000906     C*-------------------------------------------------------------------------
034600000906     c* SEGCHI -  Routine di determinazione chi sono successivo
034700000906     C*-------------------------------------------------------------------------
034800000906     c     segCHI        BEGSR
034900000912     c* chi sono = struttura segnacolli bartolini
035000000906     c     §1bf12        ifeq      'B'
035100000906     c                   eval      sscbar1=iz1ch1
035200000906     c                   add       1             prgsbar1
035300000912     c                   exsr      m10
035400000912     c                   movea     §ca           oz1chi
035500000906     c                   else
035600010402     c* chi sono = chi sono bartolini AA/CC....
035700000906     c                   eval      sscchib1=iz1ch1
035800000906     c                   add       1             prgchib1
035900000914     c                   move      prgchib1      §11f
036000000911     c                   exsr      m1031
036100000911     c                   eval      chkdig1=wchkd
036200000906     c                   evaL      oz1chi=sscchib1
036300000906     c                   endif
036400000906     c                   ENDSr
036500010402     C*-------------------------------------------------------------------------
036600010402     c* CHKCLI - Controlli cod.chi sono per cliente
036700010402     C*-------------------------------------------------------------------------
036800010402     c     CHKCLI        BEGSR
036900010402     c* COD. CHI SONO 1
037000010403     c                   eval      wchi = iz1ch1
037100010403     c                   eval      numchi = 1
037200010403     c                   exsr      chkchi
037300010403     c* COD. CHI SONO 2 se presente
037400010403     c* in questo caso non si tratta mai di dal/al ed imposto fisso
037500010403     c* numero colli = 2
037600010403     c     iz1ch2        ifne      *blanks
037700010403     c                   eval      wchi = iz1ch2
037800010403     c                   eval      numchi = 2
037900010403     c                   exsr      chkchi
038000010403     c                   eval      oz1ncl = 2
038100010403     c                   else
038200010403     c* se ricevuto solo cod. chi sono 1 imposto numero colli = 1
038300010403     c                   eval      oz1ncl = 1
038400010403     c                   endif
038500010403     c                   endsr
038600010306     C*-------------------------------------------------------------------------
038700010403     C*-------------------------------------------------------------------------
038800010403     c* CHKCHI - Controlli cod.chi sono per cliente
038900010403     C*-------------------------------------------------------------------------
039000010403     c     CHKCHI        BEGSR
039100010403     c* Lunghezza Barcode
039200010403     c     ' '           checkr    wchi          §z                2 0
039300010403     c* errore se lunghezza barcode non rientra nel minimo/max imposta-
039400010403     c* in tabella
039500010403     c     §z            iflt      §dkcmn
039600010403     c     §z            orgt      §dkcmx
039700010403     c     numchi        ifeq      1
039800010403     c                   movel     '1'           oz1ech
039900010403     c                   else
040000010403     c                   movel     '2'           oz1ech
040100010403     c                   endif
040200010403     c                   movel     '2'           oz1err
040300010403     c                   movel     msg(2)        oz1msg
040400010403     c                   goto      endcli
040500010403     c                   endif
040600010403     c* Caratteri iniziali:
040700010403     c* errore se definiti caratteri iniziali e sono diversi da quelli
040800010403     c* passati
040900010403     c                   if        %subst(wchi:1:§dkcil) <>
041000010403     c                             %subst(§dkcin:1:§dkcil)
041100010403     c     numchi        ifeq      1
041200010403     c                   movel     '1'           oz1ech
041300010403     c                   else
041400010403     c                   movel     '2'           oz1ech
041500010403     c                   endif
041600010403     c                   movel     '3'           oz1err
041700010403     c                   movel     msg(3)        oz1msg
041800010403     c                   goto      endcli
041900010403     c                   endif
042000010403     c     endcli        ENDSR
042100010306     c* INZSR  -
042200010306     C*-------------------------------------------------------------------------
042300010306     c     *INZSR        BEGSR
042400010306     c     *like         define    oz1ncl        wnclmax
042500010306     c                   move      *all'9'       wnclmax
042600010306     c                   ENDSR
042700000906**
042800000906Codice chi sono errato
042900010402Lunghezza fuori dai limiti min/max stabiliti
043000010402Caratteri iniziali barcode cliente diversi da quellli stabiliti
