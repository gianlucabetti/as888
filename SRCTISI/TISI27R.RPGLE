000100971121     H*------------------------------------------------------------------------*
000200971121     H* AUTOMATISMO TEMPI DI TRAZIONE - RICHIESTA PARAMETRI                    *
000300971121     H*------------------------------------------------------------------------*
000400971121     H DECEDIT('0,') DATEDIT(*DMY.)
000500971121     F*------------------------------------------------------------------------*
000600971121     F* DATA BASE
000700971121     F*------------------------------------------------------------------------*
000800971121     FTISI27D   CF   E             WORKSTN INFDS(DEVDS1)
000900971121     F                                     SFILE(SI27DVM:MSGULT)
001000061205     FAZORG01L  IF   E           K DISK
001100971121     D*------------------------------------------------------------------------*
001200971121     D* SCHIERE
001300971121     D*------------------------------------------------------------------------*
001400971121     D*-------------------
001500971121     D* MESSAGGI, COMANDI E FUNZIONI
001600971121     D*-------------------
001700070117     D MSG             S             78    DIM(14) CTDATA PERRCD(1)             *MESSAGGI
001800971121     D CMD             S             80    DIM(5) CTDATA PERRCD(1)              *COMANDI
001900971121     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
002000971121     D*-------------------
002100971121     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002200971121     D*-------------------
002300971121     D FFU             S              6    DIM(31)                              *FUNZIONE
002400971121     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002500971121     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002600971121     D*-------------------
002700971121     D* TABELLA DESCRIZIONI A VIDEO
002800971121     D*-------------------
002900971121     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
003000971121     D*-------------------
003100971121     D* TABELLA TASTI FUNZIONALI PREMUTI
003200971121     D*-------------------
003300971121     D KEYS            S              1    DIM(40)
003400971121     D*------------------------------------------------------------------------*
003500971121     D* INPUT
003600971121     D*------------------------------------------------------------------------*
003700971121     D*---------------------
003800971121     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
003900971121     D*---------------------
004000971121     D DSFUN         E DS
004100971121     D  FUNRIG                 1     40
004200971121     D*---------------------
004300971121     D* DS PER GESTIONE TASTI FUNZIONALI
004400971121     D*---------------------
004500971121     D DSKEYS        E DS
004600971121     D  KEYRIG                 1     40
004700971121     D*---------------------
004800971121     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
004900971121     D*---------------------
005000971121     D                SDS
005100971121     D  USER                 254    263
005200971121     D*---------------------
005300971121     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005400971121     D*---------------------
005500971121     D DEVDS1          DS
005600971121     D  KEY                  369    369
005700971121     D  CURSOR               370    371
005800971121     D*---------------------
005900971121     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
006000971121     D*---------------------
006100971121     D DSMSG         E DS
006200971121     D*---------------------
006300020919     D* DS REPERIMENTO DATI UTENTE
006400971121     D*---------------------
006500061205     D fnlv55ds      E DS                                                       *Profili utente
006600061205     D TIBS34DS      E DS                                                       *Profili utente
006700020919     D DDATIUTE      E DS                                                       *Dati utente
006800020919     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006900051216     D WLBDA8          DS                  INZ
007000051216     D  G08DAT                 1      8  0
007100051216     D  G08INV                 9     16  0
007200051216     D  G08ERR                17     17
007300051216     D  G08TGI                18     22  0
007400971121     D*---------------------
007500971121     D* ARCHITETTURA
007600971121     D*---------------------
007700971121     D KPJBA         E DS
007800051216     d
007900061205     d pardde          s              8s 0   inz
008000061205     d partfp          s              3s 0   inz
008100061205     d partpcpy        s              3s 0   inz
008200070117     d partfa          s              3s 0   inz
008300070117     d partacpy        s              3s 0   inz
008400971121     C*------------------------------------------------------------------------*
008500971121     C* MAIN LINE
008600971121     C*------------------------------------------------------------------------*
008700971121     C*
008800971121     C* CICLO FINO A CHE:
008900971121DO  1C     $FINE         DOUEQ     '1'
009000971121     C*
009100971121     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
009200971121     C*
009300971121     C* CONTROLLA GESTIONE VIDEO
009400971121IF  2C     $GES          IFEQ      'V1'
009500971121     C                   EXSR      GESV1                                        *PARAMETRI
009600971121E   2C                   ENDIF
009700971121E   1C                   ENDDO
009800971121     C*
009900971121     C                   SETON                                        LR
010000971121     C*------------------------------------------------------------------------*
010100971121     C* GESV1 - GESTIONE VIDEO UNO
010200971121     C*------------------------------------------------------------------------*
010300971121     C     GESV1         BEGSR
010400971121     C*
010500971121IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
010600971121     C                   EXSR      CARV1                                        *CARICA VIDEO
010700971121E   1C                   ENDIF
010800971121     C*
010900971121IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
011000971121     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
011100971121     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
011200971121E   1C                   ENDIF
011300971121     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
011400971121     C*
011500971121     C                   EXFMT     SI27DV1                                      *PARAMETRI
011600971121     C*
011700971121     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
011800971121     C*
011900971121IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
012000971121SL  2C                   SELECT
012100971121     C     KEY           WHENEQ    ENTER
012200971121     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
012300971121     C     KEY           WHENEQ    F6
012400971121     C                   EXSR      F06GES                                       *CONFERMA
012500971121     C     KEY           WHENEQ    F22
012600971121     C                   EXSR      F22GES                                       *GESTIONE STAMPE
012700971121     C                   OTHER
012800971121     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
012900971121E   2C                   ENDSL
013000971121E   1C                   ENDIF
013100971121     C*
013200971121     C                   ENDSR
013300971121     C*------------------------------------------------------------------------*
013400971121     C* CARV1 - CARICA VIDEO UNO
013500971121     C*------------------------------------------------------------------------*
013600971121     C     CARV1         BEGSR
013700971121     C*
013800971121     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
013900971121     C*
014000971121     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
014100971121     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
014200971121     C*
014300971121     C                   WRITE     SI27DVT                                      *TESTATA
014400971121     C                   WRITE     SI27DVF                                      *FUNZIONI
014500971121     C*
014600971121     C                   ENDSR
014700971121     C*------------------------------------------------------------------------*
014800971121     C* ENTV1 - GESTIONE TASTO ENTER VIDEO UNO
014900971121     C*------------------------------------------------------------------------*
015000971121     C     ENTV1         BEGSR
015100971121     C*
015200971121     C* CONTROLLA I CAMPI IMMESSI
015300971121     C                   EXSR      DECODI
015400971121     C*
015500971121     C                   ENDSR
015600971121     C*------------------------------------------------------------------------*
015700971121     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA LANCIO
015800971121     C*------------------------------------------------------------------------*
015900971121     C     F06GES        BEGSR
016000971121     C*
016100971121     C* CONTROLLA I CAMPI IMMESSI
016200971121     C                   EXSR      DECODI
016300971121     C*
016400971121     C* LANCIA IL PROGRAMMA
016500070117     c* Se copia tempi di trazione sia come P che come A
016600070117     c*  faccio doppio lancio
016700971121IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORI
016800971121     C                   MOVEL     V1CTEM        PARTEM                         *TIPO TEMPO
016900971121     C                   MOVEL     V1CSIM        PARSIM                         *SIMULAZIONE
017000051216     C                   z-add     ddevid        PARdde                         *SIMULAZIONE
017100070117     c                   clear                   partfp
017200070117     c                   clear                   partpcpy
017300070117     c                   clear                   partfa
017400070117     c                   clear                   partacpy
017500070117     c
017600070117     c                   if        v1ctip='P' or v1ctip=' '
017700061205     C                   z-add     v1ctfp        PARtfp                         *SIMULAZIONE
017800061205     C                   z-add     v1ctpcpy      PARtpcpy                       *SIMULAZIONE
017900070117     c                   endif
018000070117     c                   if        v1ctip='A'
018100070117     C                   z-add     v1ctfp        PARtfa                         *SIMULAZIONE
018200070117     C                   z-add     v1ctpcpy      PARtacpy                       *SIMULAZIONE
018300070117     c                   endif
018400070117     c
018500971121     C                   CALL      'TISI28R'
018600971121     C                   PARM                    PARTEM            1
018700971121     C                   PARM                    PARSIM            1
018800051216     C                   PARM                    PARdde
018900061205     C                   PARM                    partfp
019000061205     C                   PARM                    PARtpcpy
019100070117     C                   PARM                    partfa
019200070117     C                   PARM                    PARtacpy
019300070117     c
019400070117     c                   if        v1ctip=' ' and v1ctfp>0
019500070117     c                   clear                   partfp
019600070117     c                   clear                   partpcpy
019700070117     c                   clear                   partfa
019800070117     c                   clear                   partacpy
019900070117     C                   z-add     v1ctfp        PARtfa                         *SIMULAZIONE
020000070117     C                   z-add     v1ctpcpy      PARtacpy                       *SIMULAZIONE
020100070117     c
020200070117     C                   CALL      'TISI28R'
020300070117     C                   PARM                    PARTEM            1
020400070117     C                   PARM                    PARSIM            1
020500070117     C                   PARM                    PARdde
020600070117     C                   PARM                    partfp
020700070117     C                   PARM                    PARtpcpy
020800070117     C                   PARM                    partfa
020900070117     C                   PARM                    PARtacpy
021000070117     c                   endif
021100070117     c
021200971121     C                   MOVEL     MSG(10)       $MSG
021300971121     C                   EXSR      CARMSG
021400971121E   1C                   ENDIF
021500971121     C*
021600971121     C                   ENDSR
021700971121     C*------------------------------------------------------------------------*
021800971121     C* F22GES - GESTIONE TASTO FUNZIONALE F22 ==> GESTIONE STAMPE
021900971121     C*------------------------------------------------------------------------*
022000971121     C     F22GES        BEGSR
022100971121     C*
022200971121     C                   Z-ADD     80            LENGH            15 5
022300971121     C                   CALL      'QCAEXEC'
022400971121     C                   PARM                    CMD(1)
022500971121     C                   PARM                    LENGH
022600971121     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
022700971121     C*
022800971121     C                   ENDSR
022900971121     C*------------------------------------------------------------------------*
023000971121     C* DECODI - CONTROLLA CAMPI E DECODIFICA
023100971121     C*------------------------------------------------------------------------*
023200971121     C     DECODI        BEGSR
023300971121     C*
023400971121     C* REIMPOSTA INDICATORI DI ERRORE
023500051216     C                   SETOFF                                       606162
023600061205     C                   SETOFF                                       6364
023700971121     C*--------------------
023800971121     C* TIPO TEMPO
023900971121     C*--------------------
024000971121IF  1C     $ERR          IFEQ      '0'
024100971121IF  2C     V1CTEM        IFEQ      'P'
024200971121     C     V1CTEM        OREQ      'T'
024300971121X   2C                   ELSE
024400971121     C                   SETON                                        60
024500971121     C                   MOVEL     '1'           $ERR
024600971121     C                   MOVEL     MSG(7)        $MSG
024700971121     C                   EXSR      CARMSG
024800971121E   2C                   ENDIF
024900971121E   1C                   ENDIF
025000971121     C*--------------------
025100971121     C* AGGIORNAMENTO TEMPI
025200971121     C*--------------------
025300971121IF  1C     $ERR          IFEQ      '0'
025400971121IF  2C     V1CSIM        IFEQ      'S'
025500971121     C     V1CSIM        OREQ      'N'
025600971121X   2C                   ELSE
025700971121     C                   SETON                                        61
025800971121     C                   MOVEL     '1'           $ERR
025900971121     C                   MOVEL     MSG(7)        $MSG
026000971121     C                   EXSR      CARMSG
026100971121E   2C                   ENDIF
026200971121E   1C                   ENDIF
026300051216     C*--------------------
026400051216     C* DATA DECORRENZA
026500051216     C*--------------------
026600051216     c     *like         define    v1cdde        ddevid
026700051216IF  1C     $ERR          IFEQ      '0'
026800051216IF  2C     V1Cdde        IFEQ      *zeros
026900051216     C                   SETON                                        62
027000051216     C                   MOVEL     '1'           $ERR
027100051216     C                   MOVEL     MSG(8)        $MSG
027200051216     C                   EXSR      CARMSG
027300051216X   2C                   ELSE
027400051216     C                   z-add     v1cdde        wda8
027500051216     C                   z-add     *zeros        da8w
027600051216     C                   exsr      chkda8
027700051216     C                   z-add     wda8          v1cdde
027800051216     C                   z-add     da8w          ddevid
027900051216IF  2C     g08err        IFEQ      '1'
028000061205     C                   SETON                                        63
028100051216     C                   MOVEL     '1'           $ERR
028200061205     C                   MOVEL     MSG(8)        $MSG
028300051216     C                   EXSR      CARMSG
028400051216E   2C                   ENDIF
028500051216E   1C                   ENDIF
028600051216E   1C                   ENDIF
028700061205     c*
028800061205     C*--------------------
028900061205     C* nuovo terminal di partenza
029000061205     C*--------------------
029100061205     c                   clear                   v1dtfp
029200061205     c* solo per tempi di trazione
029300061205IF  1C     $ERR          IFEQ      '0'
029400061205     c     v1ctfp        andgt     0
029500061205     c     v1ctem        andne     'T'
029600061205     C                   SETON                                        63
029700061205     C                   MOVEL     '1'           $ERR
029800061205     C                   MOVEL     MSG(12)       $MSG
029900061205     C                   EXSR      CARMSG
030000061205     c                   endif
030100061205     c
030200061205IF  1C     $ERR          IFEQ      '0'
030300061205     c     v1ctfp        andgt     0
030400061205     c     v1ctfp        chain     azorg01l
030500061205     c                   if        not %found(azorg01l)  or (orgfag<>'F'
030600061205     c                             and orgfag<>'A')
030700061205     C                   SETON                                        63
030800061205     C                   MOVEL     '1'           $ERR
030900070117     C                   MOVEL     MSG(14)       $MSG
031000061205     C                   EXSR      CARMSG
031100061205     c                   else
031200061205     c                   movel     orgdes        v1dtfp
031300061205E   2C                   ENDIF
031400061205E   2C                   ENDIF
031500061205     c
031600070117     c* Verifico se si tratta di un terminal di partenza o arrivo o
031700070117     c*  entrambi in base al tipo impostato
031800070117     c                   if        $err='0' and v1ctfp>0 and
031900070117     c                             (v1ctip='P' or v1ctip=' ')
032000070117IF  1C**   $ERR          IFEQ      '0'
032100070117     c**   v1ctfp        andgt     0
032200061205     c                   clear                   fnlv55ds
032300061205     c                   eval      d55tpt='P'
032400061205     c                   eval      d55lin=v1ctfp
032500061205     c                   eval      d55drf=ddevid
032600061205     c                   call      'FNLV55R'
032700061205     c                   parm                    fnlv55ds
032800061205     c                   if        d55err<>' ' or d55tfp<>d55lin
032900061205     C                   SETON                                        63
033000061205     C                   MOVEL     '1'           $ERR
033100061205     C                   MOVEL     MSG(9)        $MSG
033200061205     C                   EXSR      CARMSG
033300061205E   2C                   ENDIF
033400061205E   2C                   ENDIF
033500070117     c
033600070117     c                   if        $err='0' and v1ctfp>0 and
033700070117     c                             (v1ctip='A' or v1ctip=' ')
033800070117     c                   clear                   fnlv55ds
033900070117     c                   eval      d55tpt='A'
034000070117     c                   eval      d55lin=v1ctfp
034100070117     c                   eval      d55drf=ddevid
034200070117     c                   call      'FNLV55R'
034300070117     c                   parm                    fnlv55ds
034400070117     c                   if        d55err<>' ' or d55tfa<>d55lin
034500070117     C                   SETON                                        63
034600070117     C                   MOVEL     '1'           $ERR
034700070117     C                   MOVEL     MSG(13)       $MSG
034800070117     C                   EXSR      CARMSG
034900070117E   2C                   ENDIF
035000070117E   2C                   ENDIF
035100061205     C*--------------------
035200070117     C* copiandolo dal terminal
035300061205     C*--------------------
035400061205     c                   clear                   v1dtpcpy
035500061205IF  1C     $ERR          IFEQ      '0'
035600061205     c     v1ctfp        andeq     0
035700061205     c     v1ctpcpy      andgt     0
035800061205     C                   SETON                                        64
035900061205     C                   MOVEL     '1'           $ERR
036000061205     C                   MOVEL     MSG(11)       $MSG
036100061205     C                   EXSR      CARMSG
036200061205E   2C                   ENDIF
036300070117     c
036400070117IF  1C     $ERR          IFEQ      '0'
036500070117     c     v1ctfp        andeq     0
036600070117     c     v1ctip        andne     ' '
036700070117     C                   SETON                                        64
036800070117     C                   MOVEL     '1'           $ERR
036900070117     C                   MOVEL     MSG(11)       $MSG
037000070117     C                   EXSR      CARMSG
037100070117E   2C                   ENDIF
037200061205     c
037300061205IF  1C     $ERR          IFEQ      '0'
037400061205     c     v1ctfp        andgt     0
037500061205     c     v1ctpcpy      chain     azorg01l
037600061205     c                   if        not %found(azorg01l)  or (orgfag<>'F'
037700061205     c                             and orgfag<>'A')
037800061205     C                   SETON                                        64
037900061205     C                   MOVEL     '1'           $ERR
038000070117     C                   MOVEL     MSG(14)       $MSG
038100061205     C                   EXSR      CARMSG
038200061205     c                   else
038300061205     c                   movel     orgdes        v1dtpcpy
038400061205E   2C                   ENDIF
038500061205E   2C                   ENDIF
038600061205     c
038700070117IF  1C**   $ERR          IFEQ      '0'
038800070117     c**   v1ctpcpy      andgt     0
038900070117     c                   if        $err='0' and v1ctpcpy>0 and
039000070117     c                             (v1ctip='P' or v1ctip=' ')
039100061205     c                   clear                   fnlv55ds
039200061205     c                   eval      d55tpt='P'
039300061205     c                   eval      d55lin=v1ctpcpy
039400061205     c                   eval      d55drf=ddevid
039500061205     c                   call      'FNLV55R'
039600061205     c                   parm                    fnlv55ds
039700061205     c                   if        d55err<>' ' or d55tfp<>d55lin
039800061205     C                   SETON                                        64
039900061205     C                   MOVEL     '1'           $ERR
040000061205     C                   MOVEL     MSG(9)        $MSG
040100061205     C                   EXSR      CARMSG
040200061205E   2C                   ENDIF
040300061205E   2C                   ENDIF
040400070117     c                   if        $err='0' and v1ctpcpy>0 and
040500070117     c                             (v1ctip='A' or v1ctip=' ')
040600070117     c                   clear                   fnlv55ds
040700070117     c                   eval      d55tpt='A'
040800070117     c                   eval      d55lin=v1ctpcpy
040900070117     c                   eval      d55drf=ddevid
041000070117     c                   call      'FNLV55R'
041100070117     c                   parm                    fnlv55ds
041200070117     c                   if        d55err<>' ' or d55tfa<>d55lin
041300070117     C                   SETON                                        64
041400070117     C                   MOVEL     '1'           $ERR
041500070117     C                   MOVEL     MSG(13)       $MSG
041600070117     C                   EXSR      CARMSG
041700070117E   2C                   ENDIF
041800070117E   2C                   ENDIF
041900971121     C*
042000971121     C                   ENDSR
042100971121     C*------------------------------------------------------------------------*
042200971121     C* IMPVID - IMPOSTAZIONI VIDEO
042300971121     C*------------------------------------------------------------------------*
042400971121     C     IMPVID        BEGSR
042500971121     C*
042600971121     C                   MOVEL     'T'           V1CTEM                         *TIPO TEMPO
042700971121     C                   MOVEL     'S'           V1CSIM                         *SIMULAZIONE
042800051216     c                   time                    w0140            14 0
042900051216     c                   move      w0140         v1cdde
043000061205     c                   clear                   v1ctfp
043100061205     c                   clear                   v1ctpcpy
043200971121     C*
043300971121     C                   ENDSR
043400971121     C*------------------------------------------------------------------------*
043500971121     C* CARTAB - CARICA LE TABELLE
043600971121     C*------------------------------------------------------------------------*
043700971121     C     CARTAB        BEGSR
043800971121     C                   ENDSR
043900020919     C*--------------------------------------------------------------------------------------------*
044000020919     C* REPERISCE I DATI UTENTE
044100020919     C*--------------------------------------------------------------------------------------------*
044200020919     C     REPDATIUTE    BEGSR
044300020919     C*
044400020919     C* INIZIALIZZA VARIABILI DI WRK
044500020919     C                   CLEAR                   TIBS34DS
044600020919     C                   CLEAR                   AZUTEDS
044700020919     C                   CLEAR                   DDATIUTE
044800020919     C*
044900020919     C     *DTAARA       DEFINE    §azute        azuteds
045000020919     C     *DTAARA       DEFINE    §datiute      ddatiute
045100020919     C                   IN(E)     *DTAARA
045200020919if  1C                   IF        %Error
045300020919     c                   EVAL      I34Tla = 'L'
045400020919     C                   CALL      'TIBS34R'
045500020919     C                   PARM                    Tibs34Ds
045600020919     C                   IN        *DTAARA
045700020919e   1C                   ENDIF
045800020919     C*
045900020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
046000020919     C                   MOVEL(P)  RSUT          DSFIRS           20
046100020919     C*
046200020919     C                   ENDSR
046300051216     C*------------------------------------------------------------------------*
046400051216     C* CHKDA8 - CONTROLLA LA DATA (8)
046500051216     C*------------------------------------------------------------------------*
046600051216     C     chkda8        begsr
046700051216     C*
046800051216     C     *like         define    v1cdde        wda8
046900051216     C     *LIKE         define    v1cdde        da8w
047000051216     C*
047100051216     C                   z-add     wda8          G08DAT
047200051216     C                   z-add     *zeros        G08INV
047300051216     C                   movel     '0'           G08ERR
047400051216     C                   call      'XSRDA8'
047500051216     C                   parm                    WLBDA8
047600051216     C                   z-add     g08dat        WDA8
047700051216     C                   z-add     g08inv        DA8W
047800051216     C*
047900051216     C                   endsr
048000971121     C*------------------------------------------------------------------------*
048100971121     C* *INZSR - ROUTINE INIZIALE
048200971121     C*------------------------------------------------------------------------*
048300971121     C     *INZSR        BEGSR
048400971121     C*--------------------
048500971121     C* RICEVIMENTO PARAMETRI
048600971121     C*--------------------
048700971121     C     *ENTRY        PLIST
048800971121     C                   PARM                    KPJBA
048900971121     C*--------------------
049000971121     C* VARIABILI DI CONTROLLO
049100971121     C*--------------------
049200971121     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
049300971121     C***
049400971121     C* IMPOSTAZIONI VIDEO
049500971121     C***
049600971121     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
049700971121     C*
049800971121     C* ESEGUE OPERAZIONI INIZIALI STANDARD
049900971121     C                   EXSR      INZSTD
050000971121     C*
050100971121     C* CARICA LE TABELLE
050200971121     C                   EXSR      CARTAB
050300971121     C*
050400971121     C* IMPOSTAZIONI VIDEO
050500971121     C                   EXSR      IMPVID
050600971121     C*
050700971121     C* EMETTE VIDEATA
050800971121     C                   EXSR      CARV1
050900971121     C*
051000971121     C                   ENDSR
051100971121     C*------------------------------------------------------------------------*
051200971121     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
051300971121     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
051400971121     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
051500971121     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
051600971121     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
051700971121     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
051800971121     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
051900971121     C*------------------------------------------------------------------------*
052000971121     C* INZSTD - ROUTINE INIZIALE STANDARD
052100971121     C*------------------------------------------------------------------------*
052200971121     C     INZSTD        BEGSR
052300971121     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
052400971121     C                   CALL      'XKEYS'
052500971121     C                   PARM                    DSKEYS
052600971121     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
052700971121     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
052800971121     C     XMSGPA        PLIST
052900971121     C                   PARM                    DSMSMS
053000971121     C                   PARM                    DSMSPG
053100971121     C                   PARM                    DSMSKE
053200971121     C                   PARM                    DSMSER
053300971121     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
053400971121     C                   MOVEL     'TISI27R'     DSMSPG                         *NOME PGM CHIAMANTE
053500971121     C                   MOVEL     'TISI27R'     VMCQUE                         *CODA MESSAGGI DESTI
053600971121     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
053700971121     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
053800971121     C* INIZIALIZZO FLAG CONTROLLO PGM
053900971121     C                   MOVEL     '0'           $ERR              1            *ERRORE
054000971121     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
054100971121     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
054200971121     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
054300971121     C                   MOVEL     '0'           $FINE             1            *FINE PGM
054400971121     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
054500971121     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
054600971121     C* LANCIO ROUTINE PER VERIFICARE SE
054700971121     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
054800971121     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
054900020919     C                   EXSR      REPDATIUTE
055000971121     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
055100971121     C                   BITOFF    '01234567'    POSMI1            1
055200971121     C                   BITOFF    '01234567'    POSMI2            1
055300971121     C                   BITOFF    '01234567'    POSMA1            1
055400971121     C                   BITOFF    '01234567'    POSMA2            1
055500971121     C                   BITON     '34'          POSMI1            1            *18
055600971121     C                   BITON     '7'           POSMI2            1            *01
055700971121     C                   BITON     '34'          POSMA1            1            *18
055800971121     C                   BITON     '0'           POSMA2            1            *80
055900971121     C                   MOVE      POSMI2        POSMIN            2
056000971121     C                   MOVEL     POSMI1        POSMIN
056100971121     C                   MOVE      POSMA2        POSMAX            2
056200971121     C                   MOVEL     POSMA1        POSMAX
056300971121     C                   ENDSR
056400971121     C*------------------------------------------------------------------------*
056500971121     C* FUNSTD - LANCIA SUBROUTINE STANDARD
056600971121     C*------------------------------------------------------------------------*
056700971121     C     FUNSTD        BEGSR
056800971121     C                   SELECT
056900971121     C     KEY           WHENEQ    F3
057000971121     C                   EXSR      F03STD
057100971121     C     KEY           WHENEQ    F12
057200971121     C                   EXSR      F12STD
057300971121     C     KEY           WHENEQ    F24
057400971121     C                   EXSR      F24STD
057500971121     C                   OTHER
057600971121     C                   MOVEL     MSG(2)        $MSG
057700971121     C                   EXSR      CARMSG
057800971121     C                   ENDSL
057900971121     C                   ENDSR
058000971121     C*------------------------------------------------------------------------*
058100971121     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
058200971121     C*------------------------------------------------------------------------*
058300971121     C     F03STD        BEGSR
058400971121     C* IMPOSTO FLAG DI PREMUTO TASTO F03
058500971121     C                   MOVEL     '1'           $F03
058600971121     C* IMPOSTO FLAG DI FINE PROGRAMMA
058700971121     C                   MOVEL     '1'           $FINE
058800971121     C* PULISCE IL MESSAGGIO
058900971121     C                   ENDSR
059000971121     C*------------------------------------------------------------------------*
059100971121     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
059200971121     C*------------------------------------------------------------------------*
059300971121     C     F12STD        BEGSR
059400971121     C* IMPOSTO FLAG DI FINE PROGRAMMA
059500971121     C                   MOVEL     '1'           $FINE
059600971121     C                   ENDSR
059700971121     C*------------------------------------------------------------------------*
059800971121     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
059900971121     C*------------------------------------------------------------------------*
060000971121     C     F24STD        BEGSR
060100971121     C*
060200971121     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
060300971121     C                   EXSR      OUTFUN
060400971121     C*
060500971121     C                   ENDSR
060600971121     C*------------------------------------------------------------------------*
060700971121     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
060800971121     C*          CODA MESSAGGI DEL PGM
060900971121     C*------------------------------------------------------------------------*
061000971121     C     CARMSG        BEGSR
061100971121     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
061200971121     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
061300971121     C     MSGULT        IFEQ      0
061400971121     C                   MOVEL     '1'           *IN40
061500971121     C                   WRITE     SI27DVK
061600971121     C                   MOVEL     '0'           *IN40
061700971121     C                   ENDIF
061800971121     C* INVIA MESSAGGIO ALLA CODA DEL PGM
061900971121     C                   MOVEL     $MSG          DSMSMS
062000971121     C                   CALL      'XMSG'        XMSGPA
062100971121     C     DSMSER        IFEQ      0
062200971121     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
062300971121     C                   ENDIF
062400971121     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
062500971121     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
062600971121     C                   WRITE     SI27DVM                                      *SFL MESS
062700971121     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
062800971121     C                   MOVEL     *BLANKS       $MSG
062900971121     C                   ENDSR
063000971121     C*------------------------------------------------------------------------*
063100971121     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
063200971121     C*------------------------------------------------------------------------*
063300971121     C     OUTMSG        BEGSR
063400971121     C* VISUALIZZAZIONE MESSAGGIO
063500971121     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
063600971121     C                   WRITE     SI27DVK                                      *CONTR MESS
063700971121     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
063800971121     C                   Z-ADD     *ZEROS        MSGULT
063900971121     C                   ENDSR
064000971121     C*------------------------------------------------------------------------*
064100971121     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
064200971121     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
064300971121     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
064400971121     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
064500971121     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
064600971121     C*------------------------------------------------------------------------*
064700971121     C     CARFUN        BEGSR
064800971121     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
064900971121     C     1             DO        31            I                 5 0          --- 1 -->
065000971121     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
065100971121     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
065200971121     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
065300020919     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
065400971121     C     $TGES         IFEQ      'G'                                          *GESTIONE
065500971121     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
065600971121     C                   ELSE                                                   *INTERROG.
065700971121     C                   MOVEL     FUNASI        FAT(I)
065800971121     C                   ENDIF
065900971121     C                   ELSE                                                   <-- 2 --> *FILIALE
066000971121     C     $TGES         IFEQ      'G'                                          *GESTIONE
066100971121     C                   MOVEL     FUNAFG        FAT(I)
066200971121     C                   ELSE                                                   *INTERROG.
066300971121     C                   MOVEL     FUNAFI        FAT(I)
066400971121     C                   ENDIF
066500971121     C                   ENDIF                                                  <-- 2 ---
066600971121     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
066700971121     C                   MOVEL     USER          WEDP              3            *PROFILO
066800971121     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
066900971121     C                   MOVEL     'S'           FAT(I)                         *ABILITO
067000971121     C                   ELSE
067100971121     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
067200971121     C                   ENDIF
067300971121     C                   ENDIF
067400971121     C                   ENDDO                                                  <-- 1 ---
067500971121     C                   ENDSR
067600971121     C*------------------------------------------------------------------------*
067700971121     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
067800971121     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
067900971121     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
068000971121     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
068100971121     C*------------------------------------------------------------------------*
068200971121     C     OUTFUN        BEGSR
068300971121     C     1             DO        8             I                              *PULIZIA FUNZIONI A
068400971121     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
068500971121     C                   ENDDO
068600971121     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
068700971121     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
068800971121     C     1             DO        31            I
068900971121     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
069000971121     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
069100971121     C                   ADD       1             FUNNAT
069200971121     C                   ENDIF
069300971121     C                   ENDDO
069400971121     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
069500971121     C     FUNNAT        IFGT      8
069600971121     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
069700971121     C                   ENDIF
069800971121     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
069900971121     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
070000971121     C                   Z-ADD     8             FUNVIS            5 0
070100971121     C     FAT(23)       IFEQ      'S'
070200971121     C                   SUB       1             FUNVIS
070300971121     C                   ENDIF
070400971121     C     FAT(24)       IFEQ      'S'
070500971121     C                   SUB       1             FUNVIS
070600971121     C                   ENDIF
070700971121     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
070800971121     C                   Z-ADD     *ZEROS        W                 5 0
070900971121     C                   ADD       1             FUNULT
071000971121     C     FUNULT        IFGT      31
071100971121     C                   Z-ADD     1             FUNULT
071200971121     C                   ENDIF
071300971121     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
071400971121     C     FUNULT        DO        31            I                              --- 1 -->
071500971121     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
071600971121     C     W             IFLT      FUNVIS                                       --- 2 -->
071700971121     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
071800971121     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
071900971121     C     FDE(I)        ANDNE     *BLANKS
072000971121     C     I             IFEQ      23                                           *TOLGO F23
072100971121     C     I             OREQ      24                                           *E F24
072200971121     C                   ELSE
072300971121     C                   ADD       1             W
072400971121     C                   MOVEL     FDE(I)        FUV(W)
072500971121     C                   ENDIF
072600971121     C                   ENDIF                                                  <-- 3 ---
072700971121     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
072800971121     C                   Z-ADD     I             FUNULT
072900971121     C                   ENDIF                                                  <-- 2 ---
073000971121     C                   ENDDO                                                  <-- 1 ---
073100971121     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
073200971121     C                   MOVEL     FUV(1)        VFD001
073300971121     C                   MOVEL     FUV(2)        VFD002
073400971121     C                   MOVEL     FUV(3)        VFD003
073500971121     C                   MOVEL     FUV(4)        VFD004
073600971121     C                   MOVEL     FUV(5)        VFD005
073700971121     C                   MOVEL     FUV(6)        VFD006
073800971121     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
073900971121     C     FAT(24)       ANDEQ     'N'
074000971121     C                   MOVEL     FUV(7)        VFD007
074100971121     C                   MOVEL     FUV(8)        VFD008
074200971121     C                   ELSE                                                   <-- 1 -->
074300971121     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
074400971121     C     FAT(24)       ANDEQ     'S'
074500971121     C                   MOVEL     FDE(23)       VFD007
074600971121     C                   MOVEL     FDE(24)       VFD008
074700971121     C                   ELSE                                                   <-- 2 -->
074800971121     C                   MOVEL     FUV(7)        VFD007
074900971121     C     FAT(23)       IFEQ      'S'
075000971121     C                   MOVEL     FDE(23)       VFD008
075100971121     C                   ELSE
075200971121     C                   MOVEL     FDE(24)       VFD008
075300971121     C                   ENDIF
075400971121     C                   ENDIF                                                  <-- 2 ---
075500971121     C                   ENDIF                                                  <-- 1 ---
075600971121     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
075700971121     C                   ENDSR
075800971121     C*------------------------------------------------------------------------*
075900971121     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
076000971121     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
076100971121     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
076200971121     C*                   - ERRORE SE FUNZIONE NON ATTIVA
076300971121     C*------------------------------------------------------------------------*
076400971121     C     CONFUN        BEGSR
076500971121     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
076600971121     C     1             DO        31            I                              --- 1 -->
076700971121     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
076800971121     C* CONTROLLO SE IL TASTO E' ATTIVO
076900971121     C     FAT(I)        IFNE      'S'                                          --- 3 -->
077000971121     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
077100971121     C                   EXSR      CARMSG
077200971121     C                   MOVEL     '1'           $ERR
077300971121     C                   ENDIF                                                  <-- 3 ---
077400971121     C                   ENDIF                                                  <-- 2 ---
077500971121     C                   ENDDO                                                  <-- 1 ---
077600971121     C                   ENDSR
077700971121     O*------------------------------------------------------------------------*
077800971121** MSG - (MESSAGGI)
077900971121TISI27R- Tasto funzionale non valido.                                          1
078000971121TISI27R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
078100971121TISI27R- Opzione non valida.                                                   3
078200971121TISI27R- Opzione abilitata ma non gestita. Avvertire CED.                      4
078300971121TISI27R- Inizio elenco.                                                        5
078400971121TISI27R- Fine elenco.                                                          6
078500971121TISI27R- Immettere uno dei valori possibili                                    7
078600051216TISI27R- Data mancante o errata
078700070117TISI27R- P.O. indicato non è un terminal di partenza
078800971121TISI27R- Automatismo applicato ...                                             10
078900070117Immettere Terminal da cui copiare solo se immesso terminal x creazione tempi   11
079000070117Immettere Terminal solo per tempi di Trazione                                  12
079100070117TISI27R- P.O. indicato non è un terminal di arrivo                             13
079200070117TISI27R- P.O. indicato errato                                                  14
079300971121** CMD  (COMANDI)
079400971121WRKSPLF
0795009711212
0796009711213
0797009711214
0798009711215
079900971121** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
080000971121F1    -                   -    NN     NN
080100971121F2    -                   -    NN     NN
080200971121F3    -F3=Fine            -    SS     SS
080300971121F4    -                   -    NN     NN
080400971121F5    -                   -    NN     NN
080500971121F6    -F6=Conferma        -    SS     SS
080600971121F7    -                   -    NN     NN
080700971121F8    -                   -    NN     NN
080800971121F9    -                   -    NN     NN
080900971121F10   -                   -    NN     NN
081000971121F11   -                   -    NN     NN
081100971121F12   -F12=Ritorno        -    SS     SS
081200971121F13   -                   -    NN     NN
081300971121F14   -                   -    NN     NN
081400971121F15   -                   -    NN     NN
081500971121F16   -                   -    NN     NN
081600971121F17   -                   -    NN     NN
081700971121F18   -                   -    NN     NN
081800971121F19   -                   -    NN     NN
081900971121F20   -                   -    NN     NN
082000971121F21   -                   -    NN     NN
082100971121F22   -F22=Gestione stampe-    EE     EE
082200971121F23   -                   -    NN     NN
082300971121F24   -F24=Altri tasti    -    NN     NN
082400971121ENTER -                   -    SS     SS
082500971121ROLLUP-                   -    NN     NN
082600971121ROLLDN-                   -    NN     NN
082700971121HELP  -                   -    NN     NN
082800971121PRINT -                   -    NN     NN
082900971121BSPACE-                   -    NN     NN
083000971121CLEAR -                   -    NN     NN
