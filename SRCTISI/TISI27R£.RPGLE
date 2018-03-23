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
001700061205     D MSG             S             78    DIM(12) CTDATA PERRCD(1)             *MESSAGGI
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
008200971121     C*------------------------------------------------------------------------*
008300971121     C* MAIN LINE
008400971121     C*------------------------------------------------------------------------*
008500971121     C*
008600971121     C* CICLO FINO A CHE:
008700971121DO  1C     $FINE         DOUEQ     '1'
008800971121     C*
008900971121     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
009000971121     C*
009100971121     C* CONTROLLA GESTIONE VIDEO
009200971121IF  2C     $GES          IFEQ      'V1'
009300971121     C                   EXSR      GESV1                                        *PARAMETRI
009400971121E   2C                   ENDIF
009500971121E   1C                   ENDDO
009600971121     C*
009700971121     C                   SETON                                        LR
009800971121     C*------------------------------------------------------------------------*
009900971121     C* GESV1 - GESTIONE VIDEO UNO
010000971121     C*------------------------------------------------------------------------*
010100971121     C     GESV1         BEGSR
010200971121     C*
010300971121IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
010400971121     C                   EXSR      CARV1                                        *CARICA VIDEO
010500971121E   1C                   ENDIF
010600971121     C*
010700971121IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
010800971121     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
010900971121     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
011000971121E   1C                   ENDIF
011100971121     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
011200971121     C*
011300971121     C                   EXFMT     SI27DV1                                      *PARAMETRI
011400971121     C*
011500971121     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
011600971121     C*
011700971121IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
011800971121SL  2C                   SELECT
011900971121     C     KEY           WHENEQ    ENTER
012000971121     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
012100971121     C     KEY           WHENEQ    F6
012200971121     C                   EXSR      F06GES                                       *CONFERMA
012300971121     C     KEY           WHENEQ    F22
012400971121     C                   EXSR      F22GES                                       *GESTIONE STAMPE
012500971121     C                   OTHER
012600971121     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
012700971121E   2C                   ENDSL
012800971121E   1C                   ENDIF
012900971121     C*
013000971121     C                   ENDSR
013100971121     C*------------------------------------------------------------------------*
013200971121     C* CARV1 - CARICA VIDEO UNO
013300971121     C*------------------------------------------------------------------------*
013400971121     C     CARV1         BEGSR
013500971121     C*
013600971121     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
013700971121     C*
013800971121     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
013900971121     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
014000971121     C*
014100971121     C                   WRITE     SI27DVT                                      *TESTATA
014200971121     C                   WRITE     SI27DVF                                      *FUNZIONI
014300971121     C*
014400971121     C                   ENDSR
014500971121     C*------------------------------------------------------------------------*
014600971121     C* ENTV1 - GESTIONE TASTO ENTER VIDEO UNO
014700971121     C*------------------------------------------------------------------------*
014800971121     C     ENTV1         BEGSR
014900971121     C*
015000971121     C* CONTROLLA I CAMPI IMMESSI
015100971121     C                   EXSR      DECODI
015200971121     C*
015300971121     C                   ENDSR
015400971121     C*------------------------------------------------------------------------*
015500971121     C* F06GES - GESTIONE TASTO FUNZIONALE F06 ==> CONFERMA LANCIO
015600971121     C*------------------------------------------------------------------------*
015700971121     C     F06GES        BEGSR
015800971121     C*
015900971121     C* CONTROLLA I CAMPI IMMESSI
016000971121     C                   EXSR      DECODI
016100971121     C*
016200971121     C* LANCIA IL PROGRAMMA
016300971121IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORI
016400971121     C                   MOVEL     V1CTEM        PARTEM                         *TIPO TEMPO
016500971121     C                   MOVEL     V1CSIM        PARSIM                         *SIMULAZIONE
016600051216     C                   z-add     ddevid        PARdde                         *SIMULAZIONE
016700061205     C                   z-add     v1ctfp        PARtfp                         *SIMULAZIONE
016800061205     C                   z-add     v1ctpcpy      PARtpcpy                       *SIMULAZIONE
016900971121     C                   CALL      'TISI28R'
017000971121     C                   PARM                    PARTEM            1
017100971121     C                   PARM                    PARSIM            1
017200051216     C                   PARM                    PARdde
017300061205     C                   PARM                    partfp
017400061205     C                   PARM                    PARtpcpy
017500971121     C                   MOVEL     MSG(10)       $MSG
017600971121     C                   EXSR      CARMSG
017700971121E   1C                   ENDIF
017800971121     C*
017900971121     C                   ENDSR
018000971121     C*------------------------------------------------------------------------*
018100971121     C* F22GES - GESTIONE TASTO FUNZIONALE F22 ==> GESTIONE STAMPE
018200971121     C*------------------------------------------------------------------------*
018300971121     C     F22GES        BEGSR
018400971121     C*
018500971121     C                   Z-ADD     80            LENGH            15 5
018600971121     C                   CALL      'QCAEXEC'
018700971121     C                   PARM                    CMD(1)
018800971121     C                   PARM                    LENGH
018900971121     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
019000971121     C*
019100971121     C                   ENDSR
019200971121     C*------------------------------------------------------------------------*
019300971121     C* DECODI - CONTROLLA CAMPI E DECODIFICA
019400971121     C*------------------------------------------------------------------------*
019500971121     C     DECODI        BEGSR
019600971121     C*
019700971121     C* REIMPOSTA INDICATORI DI ERRORE
019800051216     C                   SETOFF                                       606162
019900061205     C                   SETOFF                                       6364
020000971121     C*--------------------
020100971121     C* TIPO TEMPO
020200971121     C*--------------------
020300971121IF  1C     $ERR          IFEQ      '0'
020400971121IF  2C     V1CTEM        IFEQ      'P'
020500971121     C     V1CTEM        OREQ      'T'
020600971121X   2C                   ELSE
020700971121     C                   SETON                                        60
020800971121     C                   MOVEL     '1'           $ERR
020900971121     C                   MOVEL     MSG(7)        $MSG
021000971121     C                   EXSR      CARMSG
021100971121E   2C                   ENDIF
021200971121E   1C                   ENDIF
021300971121     C*--------------------
021400971121     C* AGGIORNAMENTO TEMPI
021500971121     C*--------------------
021600971121IF  1C     $ERR          IFEQ      '0'
021700971121IF  2C     V1CSIM        IFEQ      'S'
021800971121     C     V1CSIM        OREQ      'N'
021900971121X   2C                   ELSE
022000971121     C                   SETON                                        61
022100971121     C                   MOVEL     '1'           $ERR
022200971121     C                   MOVEL     MSG(7)        $MSG
022300971121     C                   EXSR      CARMSG
022400971121E   2C                   ENDIF
022500971121E   1C                   ENDIF
022600051216     C*--------------------
022700051216     C* DATA DECORRENZA
022800051216     C*--------------------
022900051216     c     *like         define    v1cdde        ddevid
023000051216IF  1C     $ERR          IFEQ      '0'
023100051216IF  2C     V1Cdde        IFEQ      *zeros
023200051216     C                   SETON                                        62
023300051216     C                   MOVEL     '1'           $ERR
023400051216     C                   MOVEL     MSG(8)        $MSG
023500051216     C                   EXSR      CARMSG
023600051216X   2C                   ELSE
023700051216     C                   z-add     v1cdde        wda8
023800051216     C                   z-add     *zeros        da8w
023900051216     C                   exsr      chkda8
024000051216     C                   z-add     wda8          v1cdde
024100051216     C                   z-add     da8w          ddevid
024200051216IF  2C     g08err        IFEQ      '1'
024300061205     C                   SETON                                        63
024400051216     C                   MOVEL     '1'           $ERR
024500061205     C                   MOVEL     MSG(8)        $MSG
024600051216     C                   EXSR      CARMSG
024700051216E   2C                   ENDIF
024800051216E   1C                   ENDIF
024900051216E   1C                   ENDIF
025000061205     c*
025100061205     C*--------------------
025200061205     C* nuovo terminal di partenza
025300061205     C*--------------------
025400061205     c                   clear                   v1dtfp
025500061205     c* solo per tempi di trazione
025600061205IF  1C     $ERR          IFEQ      '0'
025700061205     c     v1ctfp        andgt     0
025800061205     c     v1ctem        andne     'T'
025900061205     C                   SETON                                        63
026000061205     C                   MOVEL     '1'           $ERR
026100061205     C                   MOVEL     MSG(12)       $MSG
026200061205     C                   EXSR      CARMSG
026300061205     c                   endif
026400061205     c
026500061205IF  1C     $ERR          IFEQ      '0'
026600061205     c     v1ctfp        andgt     0
026700061205     c     v1ctfp        chain     azorg01l
026800061205     c                   if        not %found(azorg01l)  or (orgfag<>'F'
026900061205     c                             and orgfag<>'A')
027000061205     C                   SETON                                        63
027100061205     C                   MOVEL     '1'           $ERR
027200061205     C                   MOVEL     MSG(9)        $MSG
027300061205     C                   EXSR      CARMSG
027400061205     c                   else
027500061205     c                   movel     orgdes        v1dtfp
027600061205E   2C                   ENDIF
027700061205E   2C                   ENDIF
027800061205     c
027900061205IF  1C     $ERR          IFEQ      '0'
028000061205     c     v1ctfp        andgt     0
028100061205     c                   clear                   fnlv55ds
028200061205     c                   eval      d55tpt='P'
028300061205     c                   eval      d55lin=v1ctfp
028400061205     c                   eval      d55drf=ddevid
028500061205     c                   call      'FNLV55R'
028600061205     c                   parm                    fnlv55ds
028700061205     c                   if        d55err<>' ' or d55tfp<>d55lin
028800061205     C                   SETON                                        63
028900061205     C                   MOVEL     '1'           $ERR
029000061205     C                   MOVEL     MSG(9)        $MSG
029100061205     C                   EXSR      CARMSG
029200061205E   2C                   ENDIF
029300061205E   2C                   ENDIF
029400061205     C*--------------------
029500061205     C* copiandolo dal terminal di partenza
029600061205     C*--------------------
029700061205     c                   clear                   v1dtpcpy
029800061205IF  1C     $ERR          IFEQ      '0'
029900061205     c     v1ctfp        andeq     0
030000061205     c     v1ctpcpy      andgt     0
030100061205     C                   SETON                                        64
030200061205     C                   MOVEL     '1'           $ERR
030300061205     C                   MOVEL     MSG(11)       $MSG
030400061205     C                   EXSR      CARMSG
030500061205E   2C                   ENDIF
030600061205     c
030700061205IF  1C     $ERR          IFEQ      '0'
030800061205     c     v1ctfp        andgt     0
030900061205     c     v1ctpcpy      chain     azorg01l
031000061205     c                   if        not %found(azorg01l)  or (orgfag<>'F'
031100061205     c                             and orgfag<>'A')
031200061205     C                   SETON                                        64
031300061205     C                   MOVEL     '1'           $ERR
031400061205     C                   MOVEL     MSG(9)        $MSG
031500061205     C                   EXSR      CARMSG
031600061205     c                   else
031700061205     c                   movel     orgdes        v1dtpcpy
031800061205E   2C                   ENDIF
031900061205E   2C                   ENDIF
032000061205     c
032100061205IF  1C     $ERR          IFEQ      '0'
032200061205     c     v1ctpcpy      andgt     0
032300061205     c                   clear                   fnlv55ds
032400061205     c                   eval      d55tpt='P'
032500061205     c                   eval      d55lin=v1ctpcpy
032600061205     c                   eval      d55drf=ddevid
032700061205     c                   call      'FNLV55R'
032800061205     c                   parm                    fnlv55ds
032900061205     c                   if        d55err<>' ' or d55tfp<>d55lin
033000061205     C                   SETON                                        64
033100061205     C                   MOVEL     '1'           $ERR
033200061205     C                   MOVEL     MSG(9)        $MSG
033300061205     C                   EXSR      CARMSG
033400061205E   2C                   ENDIF
033500061205E   2C                   ENDIF
033600971121     C*
033700971121     C                   ENDSR
033800971121     C*------------------------------------------------------------------------*
033900971121     C* IMPVID - IMPOSTAZIONI VIDEO
034000971121     C*------------------------------------------------------------------------*
034100971121     C     IMPVID        BEGSR
034200971121     C*
034300971121     C                   MOVEL     'T'           V1CTEM                         *TIPO TEMPO
034400971121     C                   MOVEL     'S'           V1CSIM                         *SIMULAZIONE
034500051216     c                   time                    w0140            14 0
034600051216     c                   move      w0140         v1cdde
034700061205     c                   clear                   v1ctfp
034800061205     c                   clear                   v1ctpcpy
034900971121     C*
035000971121     C                   ENDSR
035100971121     C*------------------------------------------------------------------------*
035200971121     C* CARTAB - CARICA LE TABELLE
035300971121     C*------------------------------------------------------------------------*
035400971121     C     CARTAB        BEGSR
035500971121     C                   ENDSR
035600020919     C*--------------------------------------------------------------------------------------------*
035700020919     C* REPERISCE I DATI UTENTE
035800020919     C*--------------------------------------------------------------------------------------------*
035900020919     C     REPDATIUTE    BEGSR
036000020919     C*
036100020919     C* INIZIALIZZA VARIABILI DI WRK
036200020919     C                   CLEAR                   TIBS34DS
036300020919     C                   CLEAR                   AZUTEDS
036400020919     C                   CLEAR                   DDATIUTE
036500020919     C*
036600020919     C     *DTAARA       DEFINE    §azute        azuteds
036700020919     C     *DTAARA       DEFINE    §datiute      ddatiute
036800020919     C                   IN(E)     *DTAARA
036900020919if  1C                   IF        %Error
037000020919     c                   EVAL      I34Tla = 'L'
037100020919     C                   CALL      'TIBS34R'
037200020919     C                   PARM                    Tibs34Ds
037300020919     C                   IN        *DTAARA
037400020919e   1C                   ENDIF
037500020919     C*
037600020919     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
037700020919     C                   MOVEL(P)  RSUT          DSFIRS           20
037800020919     C*
037900020919     C                   ENDSR
038000051216     C*------------------------------------------------------------------------*
038100051216     C* CHKDA8 - CONTROLLA LA DATA (8)
038200051216     C*------------------------------------------------------------------------*
038300051216     C     chkda8        begsr
038400051216     C*
038500051216     C     *like         define    v1cdde        wda8
038600051216     C     *LIKE         define    v1cdde        da8w
038700051216     C*
038800051216     C                   z-add     wda8          G08DAT
038900051216     C                   z-add     *zeros        G08INV
039000051216     C                   movel     '0'           G08ERR
039100051216     C                   call      'XSRDA8'
039200051216     C                   parm                    WLBDA8
039300051216     C                   z-add     g08dat        WDA8
039400051216     C                   z-add     g08inv        DA8W
039500051216     C*
039600051216     C                   endsr
039700971121     C*------------------------------------------------------------------------*
039800971121     C* *INZSR - ROUTINE INIZIALE
039900971121     C*------------------------------------------------------------------------*
040000971121     C     *INZSR        BEGSR
040100971121     C*--------------------
040200971121     C* RICEVIMENTO PARAMETRI
040300971121     C*--------------------
040400971121     C     *ENTRY        PLIST
040500971121     C                   PARM                    KPJBA
040600971121     C*--------------------
040700971121     C* VARIABILI DI CONTROLLO
040800971121     C*--------------------
040900971121     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
041000971121     C***
041100971121     C* IMPOSTAZIONI VIDEO
041200971121     C***
041300971121     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
041400971121     C*
041500971121     C* ESEGUE OPERAZIONI INIZIALI STANDARD
041600971121     C                   EXSR      INZSTD
041700971121     C*
041800971121     C* CARICA LE TABELLE
041900971121     C                   EXSR      CARTAB
042000971121     C*
042100971121     C* IMPOSTAZIONI VIDEO
042200971121     C                   EXSR      IMPVID
042300971121     C*
042400971121     C* EMETTE VIDEATA
042500971121     C                   EXSR      CARV1
042600971121     C*
042700971121     C                   ENDSR
042800971121     C*------------------------------------------------------------------------*
042900971121     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
043000971121     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
043100971121     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
043200971121     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
043300971121     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
043400971121     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
043500971121     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
043600971121     C*------------------------------------------------------------------------*
043700971121     C* INZSTD - ROUTINE INIZIALE STANDARD
043800971121     C*------------------------------------------------------------------------*
043900971121     C     INZSTD        BEGSR
044000971121     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
044100971121     C                   CALL      'XKEYS'
044200971121     C                   PARM                    DSKEYS
044300971121     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
044400971121     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
044500971121     C     XMSGPA        PLIST
044600971121     C                   PARM                    DSMSMS
044700971121     C                   PARM                    DSMSPG
044800971121     C                   PARM                    DSMSKE
044900971121     C                   PARM                    DSMSER
045000971121     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
045100971121     C                   MOVEL     'TISI27R'     DSMSPG                         *NOME PGM CHIAMANTE
045200971121     C                   MOVEL     'TISI27R'     VMCQUE                         *CODA MESSAGGI DESTI
045300971121     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
045400971121     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
045500971121     C* INIZIALIZZO FLAG CONTROLLO PGM
045600971121     C                   MOVEL     '0'           $ERR              1            *ERRORE
045700971121     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
045800971121     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
045900971121     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
046000971121     C                   MOVEL     '0'           $FINE             1            *FINE PGM
046100971121     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
046200971121     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
046300971121     C* LANCIO ROUTINE PER VERIFICARE SE
046400971121     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
046500971121     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
046600020919     C                   EXSR      REPDATIUTE
046700971121     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
046800971121     C                   BITOFF    '01234567'    POSMI1            1
046900971121     C                   BITOFF    '01234567'    POSMI2            1
047000971121     C                   BITOFF    '01234567'    POSMA1            1
047100971121     C                   BITOFF    '01234567'    POSMA2            1
047200971121     C                   BITON     '34'          POSMI1            1            *18
047300971121     C                   BITON     '7'           POSMI2            1            *01
047400971121     C                   BITON     '34'          POSMA1            1            *18
047500971121     C                   BITON     '0'           POSMA2            1            *80
047600971121     C                   MOVE      POSMI2        POSMIN            2
047700971121     C                   MOVEL     POSMI1        POSMIN
047800971121     C                   MOVE      POSMA2        POSMAX            2
047900971121     C                   MOVEL     POSMA1        POSMAX
048000971121     C                   ENDSR
048100971121     C*------------------------------------------------------------------------*
048200971121     C* FUNSTD - LANCIA SUBROUTINE STANDARD
048300971121     C*------------------------------------------------------------------------*
048400971121     C     FUNSTD        BEGSR
048500971121     C                   SELECT
048600971121     C     KEY           WHENEQ    F3
048700971121     C                   EXSR      F03STD
048800971121     C     KEY           WHENEQ    F12
048900971121     C                   EXSR      F12STD
049000971121     C     KEY           WHENEQ    F24
049100971121     C                   EXSR      F24STD
049200971121     C                   OTHER
049300971121     C                   MOVEL     MSG(2)        $MSG
049400971121     C                   EXSR      CARMSG
049500971121     C                   ENDSL
049600971121     C                   ENDSR
049700971121     C*------------------------------------------------------------------------*
049800971121     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
049900971121     C*------------------------------------------------------------------------*
050000971121     C     F03STD        BEGSR
050100971121     C* IMPOSTO FLAG DI PREMUTO TASTO F03
050200971121     C                   MOVEL     '1'           $F03
050300971121     C* IMPOSTO FLAG DI FINE PROGRAMMA
050400971121     C                   MOVEL     '1'           $FINE
050500971121     C* PULISCE IL MESSAGGIO
050600971121     C                   ENDSR
050700971121     C*------------------------------------------------------------------------*
050800971121     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
050900971121     C*------------------------------------------------------------------------*
051000971121     C     F12STD        BEGSR
051100971121     C* IMPOSTO FLAG DI FINE PROGRAMMA
051200971121     C                   MOVEL     '1'           $FINE
051300971121     C                   ENDSR
051400971121     C*------------------------------------------------------------------------*
051500971121     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
051600971121     C*------------------------------------------------------------------------*
051700971121     C     F24STD        BEGSR
051800971121     C*
051900971121     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
052000971121     C                   EXSR      OUTFUN
052100971121     C*
052200971121     C                   ENDSR
052300971121     C*------------------------------------------------------------------------*
052400971121     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
052500971121     C*          CODA MESSAGGI DEL PGM
052600971121     C*------------------------------------------------------------------------*
052700971121     C     CARMSG        BEGSR
052800971121     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
052900971121     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
053000971121     C     MSGULT        IFEQ      0
053100971121     C                   MOVEL     '1'           *IN40
053200971121     C                   WRITE     SI27DVK
053300971121     C                   MOVEL     '0'           *IN40
053400971121     C                   ENDIF
053500971121     C* INVIA MESSAGGIO ALLA CODA DEL PGM
053600971121     C                   MOVEL     $MSG          DSMSMS
053700971121     C                   CALL      'XMSG'        XMSGPA
053800971121     C     DSMSER        IFEQ      0
053900971121     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
054000971121     C                   ENDIF
054100971121     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
054200971121     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
054300971121     C                   WRITE     SI27DVM                                      *SFL MESS
054400971121     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
054500971121     C                   MOVEL     *BLANKS       $MSG
054600971121     C                   ENDSR
054700971121     C*------------------------------------------------------------------------*
054800971121     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
054900971121     C*------------------------------------------------------------------------*
055000971121     C     OUTMSG        BEGSR
055100971121     C* VISUALIZZAZIONE MESSAGGIO
055200971121     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
055300971121     C                   WRITE     SI27DVK                                      *CONTR MESS
055400971121     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
055500971121     C                   Z-ADD     *ZEROS        MSGULT
055600971121     C                   ENDSR
055700971121     C*------------------------------------------------------------------------*
055800971121     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
055900971121     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
056000971121     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
056100971121     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
056200971121     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
056300971121     C*------------------------------------------------------------------------*
056400971121     C     CARFUN        BEGSR
056500971121     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
056600971121     C     1             DO        31            I                 5 0          --- 1 -->
056700971121     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
056800971121     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
056900971121     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
057000020919     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
057100971121     C     $TGES         IFEQ      'G'                                          *GESTIONE
057200971121     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
057300971121     C                   ELSE                                                   *INTERROG.
057400971121     C                   MOVEL     FUNASI        FAT(I)
057500971121     C                   ENDIF
057600971121     C                   ELSE                                                   <-- 2 --> *FILIALE
057700971121     C     $TGES         IFEQ      'G'                                          *GESTIONE
057800971121     C                   MOVEL     FUNAFG        FAT(I)
057900971121     C                   ELSE                                                   *INTERROG.
058000971121     C                   MOVEL     FUNAFI        FAT(I)
058100971121     C                   ENDIF
058200971121     C                   ENDIF                                                  <-- 2 ---
058300971121     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
058400971121     C                   MOVEL     USER          WEDP              3            *PROFILO
058500971121     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
058600971121     C                   MOVEL     'S'           FAT(I)                         *ABILITO
058700971121     C                   ELSE
058800971121     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
058900971121     C                   ENDIF
059000971121     C                   ENDIF
059100971121     C                   ENDDO                                                  <-- 1 ---
059200971121     C                   ENDSR
059300971121     C*------------------------------------------------------------------------*
059400971121     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
059500971121     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
059600971121     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
059700971121     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
059800971121     C*------------------------------------------------------------------------*
059900971121     C     OUTFUN        BEGSR
060000971121     C     1             DO        8             I                              *PULIZIA FUNZIONI A
060100971121     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
060200971121     C                   ENDDO
060300971121     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
060400971121     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
060500971121     C     1             DO        31            I
060600971121     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
060700971121     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
060800971121     C                   ADD       1             FUNNAT
060900971121     C                   ENDIF
061000971121     C                   ENDDO
061100971121     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
061200971121     C     FUNNAT        IFGT      8
061300971121     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
061400971121     C                   ENDIF
061500971121     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
061600971121     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
061700971121     C                   Z-ADD     8             FUNVIS            5 0
061800971121     C     FAT(23)       IFEQ      'S'
061900971121     C                   SUB       1             FUNVIS
062000971121     C                   ENDIF
062100971121     C     FAT(24)       IFEQ      'S'
062200971121     C                   SUB       1             FUNVIS
062300971121     C                   ENDIF
062400971121     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
062500971121     C                   Z-ADD     *ZEROS        W                 5 0
062600971121     C                   ADD       1             FUNULT
062700971121     C     FUNULT        IFGT      31
062800971121     C                   Z-ADD     1             FUNULT
062900971121     C                   ENDIF
063000971121     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
063100971121     C     FUNULT        DO        31            I                              --- 1 -->
063200971121     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
063300971121     C     W             IFLT      FUNVIS                                       --- 2 -->
063400971121     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
063500971121     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
063600971121     C     FDE(I)        ANDNE     *BLANKS
063700971121     C     I             IFEQ      23                                           *TOLGO F23
063800971121     C     I             OREQ      24                                           *E F24
063900971121     C                   ELSE
064000971121     C                   ADD       1             W
064100971121     C                   MOVEL     FDE(I)        FUV(W)
064200971121     C                   ENDIF
064300971121     C                   ENDIF                                                  <-- 3 ---
064400971121     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
064500971121     C                   Z-ADD     I             FUNULT
064600971121     C                   ENDIF                                                  <-- 2 ---
064700971121     C                   ENDDO                                                  <-- 1 ---
064800971121     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
064900971121     C                   MOVEL     FUV(1)        VFD001
065000971121     C                   MOVEL     FUV(2)        VFD002
065100971121     C                   MOVEL     FUV(3)        VFD003
065200971121     C                   MOVEL     FUV(4)        VFD004
065300971121     C                   MOVEL     FUV(5)        VFD005
065400971121     C                   MOVEL     FUV(6)        VFD006
065500971121     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
065600971121     C     FAT(24)       ANDEQ     'N'
065700971121     C                   MOVEL     FUV(7)        VFD007
065800971121     C                   MOVEL     FUV(8)        VFD008
065900971121     C                   ELSE                                                   <-- 1 -->
066000971121     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
066100971121     C     FAT(24)       ANDEQ     'S'
066200971121     C                   MOVEL     FDE(23)       VFD007
066300971121     C                   MOVEL     FDE(24)       VFD008
066400971121     C                   ELSE                                                   <-- 2 -->
066500971121     C                   MOVEL     FUV(7)        VFD007
066600971121     C     FAT(23)       IFEQ      'S'
066700971121     C                   MOVEL     FDE(23)       VFD008
066800971121     C                   ELSE
066900971121     C                   MOVEL     FDE(24)       VFD008
067000971121     C                   ENDIF
067100971121     C                   ENDIF                                                  <-- 2 ---
067200971121     C                   ENDIF                                                  <-- 1 ---
067300971121     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
067400971121     C                   ENDSR
067500971121     C*------------------------------------------------------------------------*
067600971121     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
067700971121     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
067800971121     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
067900971121     C*                   - ERRORE SE FUNZIONE NON ATTIVA
068000971121     C*------------------------------------------------------------------------*
068100971121     C     CONFUN        BEGSR
068200971121     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
068300971121     C     1             DO        31            I                              --- 1 -->
068400971121     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
068500971121     C* CONTROLLO SE IL TASTO E' ATTIVO
068600971121     C     FAT(I)        IFNE      'S'                                          --- 3 -->
068700971121     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
068800971121     C                   EXSR      CARMSG
068900971121     C                   MOVEL     '1'           $ERR
069000971121     C                   ENDIF                                                  <-- 3 ---
069100971121     C                   ENDIF                                                  <-- 2 ---
069200971121     C                   ENDDO                                                  <-- 1 ---
069300971121     C                   ENDSR
069400971121     O*------------------------------------------------------------------------*
069500971121** MSG - (MESSAGGI)
069600971121TISI27R- Tasto funzionale non valido.                                          1
069700971121TISI27R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
069800971121TISI27R- Opzione non valida.                                                   3
069900971121TISI27R- Opzione abilitata ma non gestita. Avvertire CED.                      4
070000971121TISI27R- Inizio elenco.                                                        5
070100971121TISI27R- Fine elenco.                                                          6
070200971121TISI27R- Immettere uno dei valori possibili                                    7
070300051216TISI27R- Data mancante o errata
070400061205TISI27R- P.O. indicato errato o non è un terminal di partenza
070500971121TISI27R- Automatismo applicato ...                                             10
070600061205Immettere Terminal da cui copiare solo se immesso terminal per creazione tempi
070700061205Immettere Terminal solo per tempi di Trazione
070800971121** CMD  (COMANDI)
070900971121WRKSPLF
0710009711212
0711009711213
0712009711214
0713009711215
071400971121** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
071500971121F1    -                   -    NN     NN
071600971121F2    -                   -    NN     NN
071700971121F3    -F3=Fine            -    SS     SS
071800971121F4    -                   -    NN     NN
071900971121F5    -                   -    NN     NN
072000971121F6    -F6=Conferma        -    SS     SS
072100971121F7    -                   -    NN     NN
072200971121F8    -                   -    NN     NN
072300971121F9    -                   -    NN     NN
072400971121F10   -                   -    NN     NN
072500971121F11   -                   -    NN     NN
072600971121F12   -F12=Ritorno        -    SS     SS
072700971121F13   -                   -    NN     NN
072800971121F14   -                   -    NN     NN
072900971121F15   -                   -    NN     NN
073000971121F16   -                   -    NN     NN
073100971121F17   -                   -    NN     NN
073200971121F18   -                   -    NN     NN
073300971121F19   -                   -    NN     NN
073400971121F20   -                   -    NN     NN
073500971121F21   -                   -    NN     NN
073600971121F22   -F22=Gestione stampe-    EE     EE
073700971121F23   -                   -    NN     NN
073800971121F24   -F24=Altri tasti    -    NN     NN
073900971121ENTER -                   -    SS     SS
074000971121ROLLUP-                   -    NN     NN
074100971121ROLLDN-                   -    NN     NN
074200971121HELP  -                   -    NN     NN
074300971121PRINT -                   -    NN     NN
074400971121BSPACE-                   -    NN     NN
074500971121CLEAR -                   -    NN     NN
