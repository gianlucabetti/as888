000100990802      *------------------------------------------------------------------------*
000200990802      * INTERROGAZIONE VARIAZIONI BOLLE DI SEDE
000300990802      *------------------------------------------------------------------------*
000400990802
000500000000     H DECEDIT('0,') DATEDIT(*DMY.)
000600990802      *------------------------------------------------------------------------*
000700990802     FTNSB54D   CF   E             WORKSTN INFDS(DEVDS1)
000800990802     F                                     SFILE(SB54DVM:MSGULT)
000900990802     F                                     SFILE(SB54S01:SFLULT)
001000990802     FTITASV1L  IF   E           K DISK
001100990802     FTITA7V1L  IF   E           K DISK
001200000000     FTABEL00F  IF   E           K DISK
001300021119     fAzorg01L  if   e           K Disk
001400990802      *------------------------------------------------------------------------*
001500000000     D MSG             S             78    DIM(30) CTDATA PERRCD(1)             *MESSAGGI
001600000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
001700990802      * SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
001800000000     D FFU             S              6    DIM(31)                              *FUNZIONE
001900000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002000000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
002100990802      * TABELLA DESCRIZIONI A VIDEO
002200000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
002300990802      * TABELLA TASTI FUNZIONALI PREMUTI
002400000000     D KEYS            S              1    DIM(40)
002500990802      * TABELLA CAPICONTO
002600990802      * FILIALI DI UNA STESSA AREA
002700000000     D L1              S              3    DIM(29)                              *FILIALI DELL'AREA
002800990802      * SCHIERA PER IL CAMPO DESCRITTIVO DELLA VARIAZIONE
002900000000     D VRZ             S              1    DIM(56) CTDATA PERRCD(56)            DECOD.VARIAZIONE
003000990802      *
003100990802      *-------------------
003200990802      * RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
003300990802      *-------------------
003400000000     D DSFUN         E DS
003500000000     D  FUNRIG                 1     40
003600990802      *-------------------
003700990802      * PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
003800990802      *-------------------
003900000000     D DSMSG         E DS
004000020724      *---------------------
004100020724     D TIBS02DS      E DS
004200020724     D TIBS34DS      e ds                  inz
004300020724     D DDATIUTE      e ds                  inz
004400020724     D AZUTEDS       e ds                  inz extname(AZUTE00F)
004500020724     D DUTE01        e ds                  inz
004600020724     D DLAT          e ds                  inz
004700990802      *-------------------
004800990802      * DS PER GESTIONE TASTI FUNZIONALI
004900990802      *-------------------
005000000000     D DSKEYS        E DS
005100000000     D  KEYRIG                 1     40
005200990802      *-------------------
005300990802      * RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005400990802      *-------------------
005500000000     D                SDS
005600990802     D  VTCPGM                 1     10
005700000000     D  USER                 254    263                                         UTENTE
005800990802      *-------------------
005900990802      * RIDEFINIZIONE INFDS DEL FILE VIDEO
006000990802      *-------------------
006100000000     D DEVDS1          DS
006200990802      * CHIAVE FUNZIONALE BATTUTA
006300000000     D  KEY                  369    369
006400990802      * POSIZIONE CURSORE (IN HEX)
006500000000     D  CURSOR               370    371
006600990802      * RRN DELLA PRIMA RIGA PAG. CORRENTE SFL VISUALIZZATA
006700000000     D  FRTSFL               378    379B 0
006800990802      *------------------
006900000000     D KPJBA         E DS
007000990802     D TNSB01DS      E DS
007100990802     D DS_CNACO      E DS                  EXTNAME(CNACO00F)
007200990802     D DS_CNIND      E DS                  EXTNAME(CNIND00F)
007300990802     D DS_CNCLP      E DS                  EXTNAME(CNCLP00F)
007400990802     D DS_FNCLS      E DS                  EXTNAME(FNCLS00F)
007500990802     D TIBS69DS      E DS
007600000000     D DSTB          E DS
007700020328     D DSCC          E DS
007800021119     d Og143         e ds
007900120510     d*dvpodeco      e ds
008000990802      *------------------
008100990802      * SCOMPOSIZIONE CAMPI DI TITAS
008200990802      *------------------
008300000000     D                 DS
008400000000     D  TASDTV                 1      8  0
008500000000     D  TAS§AN                 1      4
008600000000     D  TAS§ME                 5      6
008700000000     D  TAS§GI                 7      8
008800000000     D                 DS
008900000000     D  TASORV                 1      6  0
009000000000     D  TAS§OR                 1      2
009100000000     D  TAS§MI                 3      4
009200000000     D  TAS§SE                 5      6
009300990802      *------------------
009400990802     D DATA_eur        S               D   DATFMT(*eur)
009500990802
009600990802      *------------------------------------------------------------------------*
009700990802      * INDICATORI UTILIZZATI:
009800990802      *------------------------------------------------------------------------*
009900061107      * 15   ON   visualizzo partita iva
010000061107      * 15   OFF  visualizzo codice fiscale
010100990802      * 20...44   GHESTIONE SUBFILE E MESSAGGI
010200990802      * 92   ON   IMPORTO DA ASSICURARE INSERITO DALLA FATTURAZIONE
010300990802      * 98        GENERICO, UTILIZZABILE A PIACERE
010400990802      * 99        GENERICO, UTILIZZABILE A PIACERE
010500990802      *------------------------------------------------------------------------*
010600990802
010700990802      * SOLO LA PRIMA VOLTA ENTRO NELLA ROUTINE DI INIZIALIZZAZIONE.
010800990802      * NON L'HO CHIAMATA *INZSR POICHE' IN USCITA DA *INZSR VENGONO
010900990802      * AZZERATI GLI INDICATORI DI TIPO RECORD (SUI FILE).
011000000000     C                   EXSR      INIZIA
011100990802      *
011200990802      * SE VI E' STATO UN ERRORE NELLA ROUTINE INIZIALE ESCO PGM
011300990802     C     $FINE         IFEQ      '1'
011400990802      *
011500990802      * MANDO MESSAGGIO DI ERRORE AL PGM CHIAMANTE
011600000000     C                   MOVEL     '1'           D01ERR
011700990802      *
011800990802      * SE CONTROLLI INIZIALI OK, GESTISCO VIDEATE
011900990802     C                   ELSE
012000990802      *
012100990802      * CICLO FINO A CHE:
012200990802     C     $FINE         DOWEQ     '0'
012300000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
012400990802      *
012500990802      * CONTROLLA GESTIONE VIDEO
012600990802     C     $GES          IFEQ      'V1'
012700000000     C                   EXSR      GESV1                                        *MANUTENZIONE TEMPO
012800990802     C                   ENDIF
012900990802      *
013000990802     C                   ENDDO
013100990802     C                   ENDIF
013200990802      *
013300990802      * SE E' STATO PREMUTO F03 IMPOSTO FLAG DELLA DS DI PROCEDURA
013400990802     C     $F03          IFEQ      '1'
013500000000     C                   MOVEL     '1'           D01F03
013600990802     C                   ENDIF
013700990802      * SE E' STATO PREMUTO F12 IMPOSTO FLAG DELLA DS DI PROCEDURA
013800990802     C     $F12          IFEQ      '1'
013900000000     C                   MOVEL     '1'           D01F12
014000990802     C                   ENDIF
014100990802      * FINE PGM
014200990802     C                   MOVEL     TNSB01DS      KPJBU
014300990802      *
014400980204     C     WBS69         IFNE      *BLANKS
014500980204     C                   MOVEL     'C'           I69TLA
014600980204     C                   CALL      'TIBS69R'
014700990802     C                   PARM                    TIBS69DS
014800980204     C                   END
014900990802      *
015000000000     C                   SETON                                        LR
015100990802      *------------------------------------------------------------------------*
015200990802      * CARV1  - CARICA GESTIONE PRIMO VIDEO
015300990802      *------------------------------------------------------------------------*
015400000000     C     CARV1         BEGSR
015500990802      *
015600000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
015700990802      *
015800000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
015900000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
016000990802      *
016100990802     C                   WRITE     SB54DVT                                      *TESTATA
016200990802     C                   WRITE     SB54DVF                                      *FUNZIONI
016300990802      *
016400000000     C                   ENDSR
016500990802      *------------------------------------------------------------------------*
016600990802      * GESV1  - GESTIONE PRIMO VIDEO
016700990802      *------------------------------------------------------------------------*
016800000000     C     GESV1         BEGSR
016900990802      *
017000990802     C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
017100000000     C                   EXSR      CARV1                                        *CARICA VIDEO MOVIM
017200990802     C                   ENDIF
017300990802      *
017400990802     C     MSGULT        IFNE      *ZEROS                                       *SE CI SONO MESSAGGG
017500000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
017600990802     C                   ENDIF
017700990802      *
017800990802     C                   EXFMT     SB54C01
017900990802      *
018000000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
018100990802      *
018200990802     C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
018300990802     C                   SELECT
018400000000     C     KEY           WHENEQ    ENTER
018500000000     C                   EXSR      ENTV1                                        *GEST ENTER VIDEO V1
018600000000     C     KEY           WHENEQ    F8
018700000000     C                   EXSR      F08GES                                       *GEST F8 PRECEDENTE
018800000000     C     KEY           WHENEQ    F9
018900000000     C                   EXSR      F09GES                                       *GEST F9 SUCCESSIVA
019000000000     C     KEY           WHENEQ    ROLLUP
019100000000     C                   EXSR      RUPV1
019200000000     C     KEY           WHENEQ    ROLLDN
019300000000     C                   EXSR      RDNV1
019400000000     C                   OTHER
019500000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
019600990802     C                   ENDSL
019700990802     C                   ENDIF
019800990802      *
019900000000     C                   ENDSR
020000990802      *------------------------------------------------------------------------*
020100990802      * ENTV1  - GESTIONE TASTO ENTER PRIMO VIDEO
020200990802      *------------------------------------------------------------------------*
020300000000     C     ENTV1         BEGSR
020400990802      *
020500990802      * CONTROLLA I CAMPI IMMESSI
020600000000     C                   EXSR      DECODI
020700990802      * RITORNA AL PGM CHIAMANTE
020800000000     C                   MOVEL     '1'           $FINE
020900990802      *
021000000000     C                   ENDSR
021100990802      *------------------------------------------------------------------------*
021200990802      * F08GES -GEST.TASTO FUNZ. F8 ===> VARIAZIONE BOLLA PRECEDENTE
021300990802      *------------------------------------------------------------------------*
021400000000     C     F08GES        BEGSR
021500990802      *
021600990802      * LEGGO IL RECORD PRECEDENTE
021700000000     C                   Z-ADD     V1CDTV        KASDTV
021800000000     C                   Z-ADD     V1CORV        KASORV
021900000000     C                   MOVEL     V1CPRU        KASPRU
022000990802     C     KEYTAS        SETLL     TITASV1L
022100990802     C     KEYTA1        READPE    TITASV1L                               99
022200990802     C     *IN99         IFEQ      *OFF
022300000000     C                   EXSR      RECVID
022400000000     C                   EXSR      CARV1
022500000000     C                   EXSR      DECODI
022600990802     C                   ELSE
022700000000     C                   MOVEL     '1'           $ERR
022800000000     C                   MOVEL     MSG(7)        $MSG
022900000000     C                   EXSR      CARMSG
023000990802     C                   ENDIF
023100990802      *
023200000000     C                   ENDSR
023300990802      *------------------------------------------------------------------------*
023400990802      * F09GES -GEST.TASTO FUNZ. F9 ===> VARIAZIONE BOLLA SUCCESSIVA
023500990802      *------------------------------------------------------------------------*
023600000000     C     F09GES        BEGSR
023700990802      *
023800990802      * LEGGO IL RECORD SUCCESSIVO
023900000000     C                   Z-ADD     V1CDTV        KASDTV
024000000000     C                   Z-ADD     V1CORV        KASORV
024100000000     C                   MOVEL     V1CPRU        KASPRU
024200990802     C     KEYTAS        SETGT     TITASV1L
024300990802     C     KEYTA1        READE     TITASV1L                               99
024400990802     C     *IN99         IFEQ      *OFF
024500000000     C                   EXSR      RECVID
024600000000     C                   EXSR      CARV1
024700000000     C                   EXSR      DECODI
024800990802     C                   ELSE
024900000000     C                   MOVEL     '1'           $ERR
025000000000     C                   MOVEL     MSG(8)        $MSG
025100000000     C                   EXSR      CARMSG
025200990802     C                   ENDIF
025300990802      *
025400000000     C                   ENDSR
025500990802      *------------------------------------------------------------------------*
025600990802      * RUPV1  -GEST.TASTO FUNZ. ROLLUP VIDEO 1  ===> PAGINA SUCCESSIVA
025700990802      *------------------------------------------------------------------------*
025800000000     C     RUPV1         BEGSR
025900990802      * SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
026000990802      * SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
026100000000     C     CURSOR        IFGE      POSMIN                                       *POSIZ.INIZIO RIGA 2
026200000000     C     CURSOR        ANDLE     POSMAX                                       *POSIZ.FINE RIGA 24
026300000000     C                   ELSE
026400000000     C                   Z-ADD     SFLULT        SFLRCD
026500000000     C                   ENDIF
026600000000     C                   ENDSR
026700990802      *------------------------------------------------------------------------*
026800990802      * RDNV1  -GEST.TASTO FUNZ. ROLLDOWN VIDEO 1  ===> PAGINA PRECEDENTE
026900990802      *------------------------------------------------------------------------*
027000000000     C     RDNV1         BEGSR
027100990802      * SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
027200990802      * SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
027300000000     C     CURSOR        IFGE      POSMIN
027400000000     C     CURSOR        ANDLE     POSMAX
027500000000     C                   ELSE
027600000000     C                   Z-ADD     1             SFLRCD
027700000000     C                   ENDIF
027800000000     C                   ENDSR
027900990802      *------------------------------------------------------------------------*
028000990802      *  DECODI - CONTROLLA CAMPI E DECODIFICA
028100990802      *------------------------------------------------------------------------*
028200000000     C     DECODI        BEGSR
028300990802      *
028400990802      *--------------------
028500990802      * F L A G    V O L U M E    F A T T U R A R E
028600990802      *--------------------
028700990802     C     $ERR          IFEQ      '0'
028800000000     C                   MOVEL     *BLANKS       V1DFVF
028900990802     C     V1CFVF        IFNE      *BLANKS
029000000000     C                   MOVEL     '7T'          §COD                           *CODICE TABELLA
029100990802     C                   MOVEL(P)  V1CFVF        §KEY                           *CODICE ELEMENTO
029200000000     C                   EXSR      CHKTAB
029300000000     C                   MOVEL     §DES          V1DFVF                         *DESCRIZIONE
029400990802     C                   ENDIF
029500990802     C                   ENDIF                                                  $ERR = '0'
029600990802      *--------------------
029700990802      * F L A G    V O L U M E    B O L L E T T A T O
029800990802      *--------------------
029900990802     C     $ERR          IFEQ      '0'
030000990802     C                   CLEAR                   V1DFVB
030100990802     C                   if        V1CFVB <> *blanks
030200990802     C                   MOVEL     '7T'          §COD                           *CODICE TABELLA
030300990802     C                   MOVEL(P)  V1CFVB        §KEY                           *CODICE ELEMENTO
030400990802     C                   EXSR      CHKTAB
030500990802     C                   MOVEL     §DES          V1DFVB                         *DESCRIZIONE
030600990802     C                   endif
030700990802     C                   ENDIF                                                  $ERR = '0'
030800990802      *--------------------
030900990802      * T I P O    S E R V I Z I O
031000990802      *--------------------
031100990802     C     $ERR          IFEQ      '0'
031200000000     C                   MOVE      *BLANKS       V1DTSP                         *DESCRIZIONE
031300000000     C                   MOVEL     '5E'          §COD                           *CODICE TABELLA
031400000000     C                   MOVEL     *BLANKS       §KEY
031500000000     C                   MOVEL     V1CTSP        §KEY                           *CODICE ELEMENTO
031600000000     C                   EXSR      CHKTAB
031700000000     C                   MOVEL     §DES          V1DTSP                         *DESCRIZIONE
031800990802     C                   ENDIF                                                  $ERR = '0'
031900990802      *--------------------
032000990802      * T A R I F F A
032100990802      *--------------------
032200990802     C     $ERR          IFEQ      '0'
032300990802     C     V1DCTR        IFEQ      *BLANKS
032400000000     C     V1CCTR        ANDNE     *BLANKS
032500000000     C                   MOVEL     V1CCTR        WUNO              1            DI COMODO
032600000000     C                   MOVEL     'TR'          §COD
032700000000     C                   MOVEL(P)  WUNO          §KEY
032800000000     C                   EXSR      CHKTAB
032900000000     C                   MOVEL     §DES          V1DCTR                         *DESCRIZIONE
033000990802     C                   ENDIF
033100990802     C                   ENDIF                                                  $ERR = '0'
033200990802      *--------------------
033300990802      * M I T T E N T E
033400990802      *--------------------
033500000000     C                   MOVE      V1CMIT        WQUATT            4 0          ULTIME QUATTRO DX
033600990802     C     $ERR          IFEQ      '0'
033700990802     C                   MOVEL     TASnzm        V1CNZM                         NAZIONE
033800990802     C                   MOVEL     TASCAM        V1CCAM                         C.A.P.
033900990802      *
034000990802      * MITTENTE CODIFICATO O 8888
034100990802     C     WQUATT        IFNE      8888
034200980204     C     V1CMIT        ANDNE     *ZERO
034300980204     C                   MOVE      '1'           WBS69             1
034400990802     C                   CLEAR                   TIBS69DS
034500980204     C                   MOVEL     V1CMIT        I69KAC
034600980204     C                   MOVEL     V1CMIT        I69KIN
034700980204     C                   MOVEL     KNSIF         I69SIF
034800980204     C                   CALL      'TIBS69R'
034900990802     C                   PARM                    TIBS69DS
035000990802     C                   PARM                    DS_CNACO
035100990802     C                   PARM                    DS_CNIND
035200990802     C                   PARM                    DS_CNCLP
035300990802     C                   PARM                    DS_FNCLS
035400990802      *
035500990802     C     O69ERR        IFEQ      ' '
035600980204     C                   MOVEL     ACORAG        V1CRSM
035700980204     C                   MOVEL     INDVIA        V1CINM
035800980204     C                   MOVEL     INDCIT        V1CLOM
035900980204     C                   MOVEL     INDPRV        V1CPRM
036000061107     c   15              eval      V1Cp1m = %subst(INDIVA:1:14)
036100061107     c   15              eval      V1Cp2m = %subst(INDIVA:15:2)
036200061107     c  n15              if        v1cdfm = *blanks
036300061107     c                   eval      v1cdfm = indcdf
036400061107     c                   endif
036500990802     C                   ENDIF
036600990802     C                   ENDIF
036700990802      *
036800990802     C                   ENDIF                                                  $ERR = '0'
036900990802      *--------------------
037000990802      * C O N S E G N A    P A R T I C O L A R E    (1)
037100990802      *--------------------
037200990802     C     $ERR          IFEQ      '0'
037300000000     C                   MOVEL     *BLANKS       V1DFTC
037400990802     C     V1CFTC        IFNE      *BLANKS
037500000000     C                   MOVEL     '1P'          §COD                           *CODICE TABELLA
037600000000     C                   MOVEL     *BLANKS       §KEY
037700000000     C                   MOVEL     V1CFTC        §KEY                           *CODICE ELEMENTO
037800000000     C                   EXSR      CHKTAB
037900000000     C                   MOVEL     §DES          V1DFTC                         *DESCRIZIONE
038000990802     C                   ENDIF
038100990802     C                   ENDIF                                                  $ERR = '0'
038200990802      *--------------------
038300990802      * C O N S E G N A    P A R T I C O L A R E    (2)
038400990802      *--------------------
038500990802     C     $ERR          IFEQ      '0'
038600000000     C                   MOVEL     *BLANKS       V1DTC2
038700990802     C     V1CTC2        IFNE      *BLANKS
038800000000     C                   MOVEL     '1P'          §COD                           *CODICE TABELLA
038900000000     C                   MOVEL     *BLANKS       §KEY
039000000000     C                   MOVEL     V1CTC2        §KEY                           *CODICE ELEMENTO
039100000000     C                   EXSR      CHKTAB
039200000000     C                   MOVEL     §KEY          V1CTC2
039300000000     C                   MOVEL     §DES          V1DTC2                         *DESCRIZIONE
039400990802     C                   ENDIF
039500990802     C                   ENDIF                                                  $ERR = '0'
039600990802      *--------------------
039700990802      * E S E N Z I O N E    I. V. A.
039800990802      *--------------------
039900990802     C     $ERR          IFEQ      '0'
040000000000     C                   MOVEL     *BLANKS       V1DFEI
040100990802     C     V1CFEI        IFNE      *BLANKS
040200000000     C                   MOVEL     'EI'          §COD                           *CODICE TABELLA
040300000000     C                   MOVEL     *BLANKS       §KEY
040400000000     C                   MOVEL     V1CFEI        §KEY                           *CODICE ELEMENTO
040500000000     C                   EXSR      CHKTAB
040600000000     C                   MOVEL     §KEY          V1CFEI
040700000000     C                   MOVEL     §DES          V1DFEI                         *DESCRIZIONE
040800990802     C                   ENDIF
040900990802     C                   ENDIF                                                  $ERR = '0'
041000990802      *--------------------
041100990802      * V A R I E
041200990802      *--------------------
041300990802     C     $ERR          IFEQ      '0'
041400000000     C                   Z-ADD     1             WA                3 0
041500990802     C     WA            CHAIN     SB54S01                            99
041600990802     C     WA            DOWLE     SFLULT
041700990802     C     V2CSV1        IFNE      *BLANKS
041800000000     C                   MOVEL     'CC'          §COD                           *CODICE TABELLA
041900000000     C                   MOVEL     'VARIE   '    §KEY
042000000000     C                   MOVE      V2CSV1        §KEY                           *CODICE ELEMENTO
042100000000     C                   EXSR      CHKTAB
042200020329     C                   movel     TBLuni        DSCC
042300020329     C     §ccdfe        ifeq      'S'
042400020329      *
042500020329      * Verifico se si tratta di una bolla Fedex
042600021119     c                   If        §OgNtw = 'FED'
042700020329      * recupero decodifica da tabella CB
042800020329     C                   eval      §COD = 'CB'
042900020329     C                   movel(P)  'VARIE'       §KEY
043000020329     C                   move      v2csv1        §KEY
043100020329     C                   EXSR      CHKTAB
043200020329     c                   endif
043300020329     c                   endif
043400000000     C                   MOVEL     §DES          V2DSV1                         *DESCRIZIONE
043500990802     C                   ELSE
043600000000     C                   MOVEL     *BLANKS       V2DSV1
043700990802     C                   ENDIF
043800990802     C                   UPDATE    SB54S01
043900000000     C                   ADD       1             WA
044000990802     C     WA            CHAIN     SB54S01                            99
044100990802     C                   ENDDO
044200990802     C                   ENDIF                                                  $ERR = '0'
044300990802      *
044400990802      * SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL
044500990802      * CURSORE SUL CAMPO DEL CONTROL
044600990802     C     $ERR          IFEQ      '1'
044700000000     C                   MOVEL     '0'           *IN43                          *NON EMETTO SFL
044800990802     C                   ELSE
044900000000     C                   MOVEL     '1'           *IN43                          *EMETTO SFL
045000990802     C                   ENDIF
045100990802      *
045200000000     C                   ENDSR
045300990802      *------------------------------------------------------------------------*
045400990802      * CHKTAB - CONTROLLA UN ELEMENTO IN TABELLA
045500990802      *          INPUT --> §COD: CODICE TABELLA
045600990802      *                    $KEY: CODICE ELEMENTO
045700990802      *------------------------------------------------------------------------*
045800000000     C     CHKTAB        BEGSR
045900990802      *
046000000000     C     *LIKE         DEFINE    TBLCOD        §COD
046100000000     C     *LIKE         DEFINE    TBLKEY        §KEY
046200990802      *
046300020724     C                   MOVEL     '1'           KTBKUT                         *CODICE UTENTE
046400000000     C                   MOVEL     §COD          KTBCOD                         *CODICE TABELLA
046500000000     C                   MOVE      *BLANKS       KTBKEY
046600000000     C                   MOVEL     §KEY          KTBKEY                         *CODICE TENTATIVO
046700000000     C     KEYTAB        CHAIN     TABEL00F                           99
046800000000     C     *IN99         IFEQ      '1'                                          --- 2 -->
046900000000     C     TBLFLG        ORNE      *BLANKS
047000000000     C                   MOVEL     *ALL'*'       §DES             30            *DESCRIZIONE
047100000000     C                   CLEAR                   TBLUNI
047200000000     C                   ELSE                                                   <-- 2 -->
047300000000     C                   MOVEL     TBLUNI        §DES                           *DESCRIZIONE
047400000000     C                   ENDIF                                                  <-- 2 ---
047500990802      *
047600000000     C                   ENDSR
047700990802      *------------------------------------------------------------------------*
047800990802      *  RECVID - RIEMPE I CAMPI DEL VIDEO CON QUELLI DEL FILE
047900990802      *------------------------------------------------------------------------*
048000000000     C     RECVID        BEGSR
048100990802      *
048200000000     C                   Z-ADD     TASAAS        V1CAAS
048300000000     C                   Z-ADD     D01DSP        V1CDSP                         GG/MM/AAAA
048400000000     C                   Z-ADD     D01LNP        V1CLNP                         LNP
048500000000     C                   Z-ADD     D01NRS        V1CNRS                         SERIE
048600000000     C                   Z-ADD     D01NSP        V1CNSP                         SPEDIZIONE
048700000000     C                   MOVEL     TASTBL        V1CTBL                         TIPO BOLLA
048800990802      *
048900020724     C                   MOVEL     '1'           KTBKUT                         *CODICE UTENTE
049000000000     C                   MOVEL     'TB'          KTBCOD                         *CODICE TABELLA
049100000000     C                   MOVE      *BLANKS       KTBKEY
049200000000     C                   MOVEL     TASTBL        KTBKEY                         *CODICE TENTATIVO
049300000000     C     KEYTAB        CHAIN     TABEL00F                           99
049400000000     C                   MOVEL     TBLUNI        DSTB
049500000000     C                   MOVEL     §TBDES        V1DTBL
049600990802      *
049700990802     C                   MOVEL     TASFPF        V1CFPF
049800990802     C                   Z-ADD     TASPKF        V1CPKF
049900990802     C                   MOVEL     TASFPB        V1CFPB
050000000000     C                   Z-ADD     TASPKB        V1CPKB
050100990802     C                   MOVEL     TASFVB        V1CFVB
050200990802     C                   Z-ADD     TASVLB        V1CVLB
050300000000     C                   MOVEL     TASFVF        V1CFVF
050400000000     C                   Z-ADD     TASVLF        V1CVLF
050500000000     C                   Z-ADD     TASQFT        V1CQFT
050600000000     C                   MOVEL     TASTSP        V1CTSP
050700000000     C                   MOVEL     TASCTR        V1CCTR
050800990802      *
050900990802     C                   clear                   V1CMIT
051000990802     C                   clear                   V1CRSM
051100990802     C                   clear                   V1CINM
051200990802     C                   clear                   V1CLOM
051300990802     C                   clear                   V1CPRM
051400061107     c                   clear                   v1cdfm
051500061107     c                   clear                   v1cdfd
051600990802      *
051700990802      * SE SPEDIZIONE IN FRANCO NON USARE COD.CLIENTE MITTENTE
051800990802     C     §TBTPO        IFEQ      'F'                                          * 'F'=FRANCO
051900000000     C                   Z-ADD     TASKSC        V1CMIT
052000990802     C                   MOVE      TASKSC        WQUATT                         *ULTIME QUATTRO DX
052100990802     C                   clear                   V1CDES
052200061107      * imposto il codice fiscale
052300061107     c  n15              eval      v1cdfm = taspiv
052400990802     C     WQUATT        IFEQ      8888
052500990802     C                   MOVEL     TASRSC        V1CRSM
052600990802     C                   MOVEL     TASINM        V1CINM
052700990802     C                   MOVEL     TASLOC        V1CLOM
052800000000     C                   MOVEL     TASPRV        V1CPRM
052900061107     c   15              eval      V1Cp1m = %subst(TASPIV:1:14)
053000061107     c   15              eval      V1Cp2m = %subst(TASPIV:15:2)
053100990802     C                   ELSE
053200990802     C                   clear                   V1CRSM
053300990802     C                   clear                   V1CINM
053400990802     C                   clear                   V1CLOM
053500990802     C                   clear                   V1CPRM
053600990802     C                   clear                   V1CP1M
053700990802     C                   clear                   V1CP2M
053800990802     C                   ENDIF
053900990802      *
054000990802     C                   ELSE                                                   * 'A'=ASSEGNATO
054100990802      *
054200990802     C                   Z-ADD     TASKSC        V1CDES
054300000000     C                   Z-ADD     TASCCM        V1CMIT
054400990802     C                   MOVE      TASCCM        WQUATT                         *ULTIME QUATTRO DX
054500061107      * imposto il codice fiscale
054600061107     c  n15              eval      v1cdfd = taspiv
054700990802     C     WQUATT        IFEQ      8888
054800990802     C                   MOVEL     TASRSC        V1CRSM
054900990802     C                   MOVEL     TASINM        V1CINM
055000990802     C                   MOVEL     TASLOC        V1CLOM
055100990802     C                   MOVEL     TASPRV        V1CPRM
055200990802     C                   ENDIF
055300990802     C                   ENDIF
055400990802      *
055500000000     C                   MOVEL     TASCAM        V1CCAM
055600000000     C                   MOVEL     TASNZM        V1CNZM
055700000000     C                   MOVEL     TASFAP        V1CFAP
055800990802     C                   MOVEL     TASRSD        V1CRSD
055900990802     C                   MOVEL     TASCAD        V1CCAD
056000000000     C                   MOVEL     TASNZD        V1CNZD
056100000000     C                   MOVEL     TASFIN        V1CFIN
056200000000     C                   MOVEL     TASFTC        V1CFTC
056300000000     C                   MOVEL     TASTC2        V1CTC2
056400051108     c                   select
056500051108     C                   when      tasffd='S'
056600051108     C                   MOVEL     'SI '         V1CFFD
056700051108     C                   when      tasffd='M'
056800051108     C                   MOVEL     'SAB'         V1CFFD
056900051108     C                   other
057000000000     C                   MOVEL     *BLANKS       V1CFFD
057100051108     C                   ENDsl
057200990802     C                   Z-ADD     TASIMV        V1CIMV
057300000000     C                   Z-ADD     TASPOR        V1CPOR
057400000000     C                   MOVEL     TASFCA        V1CFCA
057500000000     C                   Z-ADD     TASIAS        V1CIAS
057600000000     C                   MOVEL     TASVAS        V1CVAS
057700990802     c                   eval      *in92 = (TASFCM = 'F')
057800000000     C                   MOVEL     TASFEI        V1CFEI
057900990802      * CERCO ANCHE LE VARIE: LE PRIME TRE DIRETTAMENTE SU TITASV0F, EVENTUALI ALTRE SU TITA7V0F
058000000000     C                   SETOFF                                       2021
058100990802     C                   WRITE     SB54C01                                      SFLCTL
058200000000     C                   SETON                                        2021      SFLDSP - SFLDSPCTL
058300000000     C                   MOVEL     'CC'          §COD                           *CODICE TABELLA
058400990802      * 1° CAMPO "VARIE"
058500000000     C                   MOVEL     TASSV1        V2CSV1                         SIGLA VARIE 1
058600000000     C                   Z-ADD     TASVA1        V2CVA1                         VARIE 1
058700990802     C     TASSV1        IFNE      *BLANKS
058800000000     C                   MOVEL     'VARIE   '    §KEY
058900000000     C                   MOVE      TASSV1        §KEY                           *CODICE ELEMENTO
059000000000     C                   EXSR      CHKTAB
059100020328     C                   movel     TBLuni        DSCC
059200020328     C     §ccdfe        ifeq      'S'
059300020328      *
059400020328      * Verifico se si tratta di una bolla Fedex
059500021119     c                   If        §OgNtw = 'FED'
059600020328      * recupero decodifica da tabella CB
059700020328     C                   eval      §COD = 'CB'
059800020328     C                   movel(P)  'VARIE'       §KEY
059900020328     C                   move      tassv1        §KEY
060000020328     C                   EXSR      CHKTAB
060100020328     c                   endif
060200020328     c                   endif
060300020328      *
060400000000     C                   MOVEL     §DES          V2DSV1                         *DESCRIZIONE
060500990802     C                   ELSE
060600000000     C                   MOVEL     *BLANKS       V2DSV1
060700990802     C                   ENDIF
060800000000     C                   Z-ADD     1             SFLULT
060900990802     C                   WRITE     SB54S01                                      SFL
061000990802      * 2° CAMPO "VARIE"
061100000000     C                   MOVEL     TASSV2        V2CSV1                         SIGLA VARIE 2
061200000000     C                   Z-ADD     TASVA2        V2CVA1                         VARIE 2
061300990802     C     TASSV2        IFNE      *BLANKS
061400000000     C                   MOVEL     'VARIE   '    §KEY
061500000000     C                   MOVE      TASSV2        §KEY                           *CODICE ELEMENTO
061600000000     C                   EXSR      CHKTAB
061700020328     C                   movel     TBLuni        DSCC
061800020328     C     §ccdfe        ifeq      'S'
061900020328      *
062000020328      * Verifico se si tratta di una bolla Fedex
062100021119     c                   If        §OgNtw = 'FED'
062200020328      * recupero decodifica da tabella CB
062300020328     C                   eval      §COD = 'CB'
062400020328     C                   movel(P)  'VARIE'       §KEY
062500020328     C                   move      tassv2        §KEY
062600020328     C                   EXSR      CHKTAB
062700020328     c                   endif
062800020328     c                   endif
062900000000     C                   MOVEL     §DES          V2DSV1                         *DESCRIZIONE
063000990802     C                   ELSE
063100000000     C                   MOVEL     *BLANKS       V2DSV1
063200990802     C                   ENDIF
063300000000     C                   ADD       1             SFLULT
063400990802     C                   WRITE     SB54S01                                      SFL
063500990802      * 3° CAMPO "VARIE"
063600000000     C                   MOVEL     TASSV3        V2CSV1                         SIGLA VARIE 3
063700000000     C                   Z-ADD     TASVA3        V2CVA1                         VARIE 3
063800990802     C     TASSV3        IFNE      *BLANKS
063900000000     C                   MOVEL     'VARIE   '    §KEY
064000000000     C                   MOVE      TASSV3        §KEY                           *CODICE ELEMENTO
064100000000     C                   EXSR      CHKTAB
064200020328     C                   movel     TBLuni        DSCC
064300020328     C     §ccdfe        ifeq      'S'
064400020328      *
064500020328      * Verifico se si tratta di una bolla Fedex
064600021119     c                   If        §OgNtw = 'FED'
064700020328      * recupero decodifica da tabella CB
064800020328     C                   eval      §COD = 'CB'
064900020328     C                   movel(P)  'VARIE'       §KEY
065000020328     C                   move      tassv3        §KEY
065100020328     C                   EXSR      CHKTAB
065200020328     c                   endif
065300020328     c                   endif
065400000000     C                   MOVEL     §DES          V2DSV1                         *DESCRIZIONE
065500990802     C                   ELSE
065600000000     C                   MOVEL     *BLANKS       V2DSV1
065700990802     C                   ENDIF
065800000000     C                   ADD       1             SFLULT
065900990802     C                   WRITE     SB54S01                                      SFL
066000990802      *
066100990802      * REIMPOSTO I CAMPI CHIAVE PERCHE' PER F8/F9 SONO VECCHI
066200000000     C                   Z-ADD     TASDTV        KASDTV
066300000000     C                   Z-ADD     TASORV        KASORV
066400000000     C                   MOVEL     TASPRU        KASPRU
066500990802      *
066600990802      * SE IL TERZO CAMPO E' PIENO TENTO SUL FILE TITA7V0F
066700000000     C     TASSV3        IFNE      *BLANKS
066800990802     C     KEYTAS        SETLL     TITA7V1L
066900990802     C     KEYTAS        READE     TITA7V1L                               99
067000000000     C     *IN99         DOWEQ     *OFF
067100000000     C     SFLULT        ANDLT     20                                           AL MASSIMO 20 VARIE
067200000000     C                   MOVEL     TA7SVN        V2CSV1                         SIGLA VARIE
067300000000     C                   Z-ADD     TA7VAN        V2CVA1                         VARIE
067400000000     C                   MOVEL     'VARIE   '    §KEY
067500000000     C                   MOVE      TA7SVN        §KEY                           *CODICE ELEMENTO
067600000000     C                   EXSR      CHKTAB
067700020329     C                   movel     TBLuni        DSCC
067800020329     C     §ccdfe        ifeq      'S'
067900020329      *
068000020329      * Verifico se si tratta di una bolla Fedex
068100021119     c                   If        §OgNtw = 'FED'
068200020329      * recupero decodifica da tabella CB
068300020329     C                   eval      §COD = 'CB'
068400020329     C                   movel(P)  'VARIE'       §KEY
068500020329     C                   move      tassv1        §KEY
068600020329     C                   EXSR      CHKTAB
068700020329     c                   endif
068800020329     c                   endif
068900000000     C                   MOVEL     §DES          V2DSV1                         *DESCRIZIONE
069000000000     C                   ADD       1             SFLULT
069100990802     C                   WRITE     SB54S01                                      SFL
069200990802     C     KEYTAS        READE     TITA7V1L                               99
069300000000     C                   ENDDO
069400000000     C                   ENDIF
069500990802      *
069600000000     C                   Z-ADD     1             SFLRCD
069700990802      *
069800000000     C                   Z-ADD     TASDTV        V1CDTV                         DATA VARIAZIONE
069900000000     C                   Z-ADD     TASORV        V1CORV                         ORA
070000000000     C                   MOVEL     TASPRU        V1CPRU                         UTENTE
070100990802      *
070200990802      * COMPONGO IL CAMPO DESCRITTIVO DELLA VARIAZIONE
070300000000     C                   MOVEA     TAS§GI        VRZ(16)                        GIORNO
070400000000     C                   MOVEA     TAS§ME        VRZ(19)                        MESE
070500000000     C                   MOVEA     TAS§AN        VRZ(22)                        ANNO
070600000000     C                   MOVEA     TAS§OR        VRZ(31)                        ORE
070700000000     C                   MOVEA     TAS§MI        VRZ(34)                        MINUTI
070800000000     C                   MOVEA     TAS§SE        VRZ(37)                        SECONDI
070900000000     C                   MOVEA     TASPRU        VRZ(47)                        UTENTE
071000000000     C                   MOVEA     VRZ           VARIAZ                         DICITURA VARIAZIONE
071100990802      *
071200000000     C                   MOVEL     '0'           $ERR
071300990802      *
071400000000     C                   ENDSR
071500990802      *------------------------------------------------------------------------*
071600990802      *  INIZIA - ROUTINE INIZIALE
071700990802      *------------------------------------------------------------------------*
071800000000     C     INIZIA        BEGSR
071900990802      *
072000990802      * RICEVIMENTO PARAMETRI
072100000000     C     *ENTRY        PLIST
072200000000     C                   PARM                    KPJBA
072300990802     C                   MOVEL     KPJBU         TNSB01DS
072400990802      *
072500990802      * DEFINIZIONE VARIABILI
072600000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
072700000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
072800000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
072900990802     C     *LIKE         DEFINE    TASAAS        KASAAS                         *TITASV0F
073000000000     C     *LIKE         DEFINE    TASLNP        KASLNP
073100000000     C     *LIKE         DEFINE    TASNRS        KASNRS
073200000000     C     *LIKE         DEFINE    TASNSP        KASNSP
073300990802     C     *LIKE         DEFINE    TASDTV        KASDTV                         *TITA7V0F
073400000000     C     *LIKE         DEFINE    TASORV        KASORV
073500000000     C     *LIKE         DEFINE    TASPRU        KASPRU
073600000000     C                   Z-ADD     *ZERO         SFLULT            4 0
073700990802      *
073800000000     C     KEYTAB        KLIST                                                  TABEL00F
073900000000     C                   KFLD                    KTBKUT
074000000000     C                   KFLD                    KTBCOD
074100000000     C                   KFLD                    KTBKEY
074200000000     C     KEYTA2        KLIST                                                  TABEL00F
074300000000     C                   KFLD                    KTBKUT
074400000000     C                   KFLD                    KTBCOD
074500990802     C     KEYTAS        KLIST                                                  TITASV1L
074600000000     C                   KFLD                    KASAAS                         ..ANNO
074700000000     C                   KFLD                    KASLNP                         ..LNP
074800000000     C                   KFLD                    KASNRS                         ..SERIE
074900000000     C                   KFLD                    KASNSP                         ..SPEDIZIONE
075000000000     C                   KFLD                    KASDTV                         ..DATA VARIAZIONE
075100000000     C                   KFLD                    KASORV                         ..ORA
075200000000     C                   KFLD                    KASPRU                         ..UTENTE
075300990802     C     KEYTA1        KLIST                                                  TITASV1L
075400000000     C                   KFLD                    KASAAS                         ..ANNO
075500000000     C                   KFLD                    KASLNP                         ..LNP
075600000000     C                   KFLD                    KASNRS                         ..SERIE
075700000000     C                   KFLD                    KASNSP                         ..SPEDIZIONE
075800990802      *
075900000000     C                   MOVE      D01OP0        INTGES            2
076000990802     C                   MOVEL     'I'           $TGES             1            *I=INTERROGAZIONE
076100990802      *
076200990802      * ESEGUE OPERAZIONI INIZIALI STANDARD
076300000000     C                   EXSR      INZSTD
076400020724     C     $Fine         cabeq     *on           EndINZ
076500990802      *
076600990802      * RAGIONE SOCIALE UTENTE
076700020724     C                   MOVEL     RSUT          VTDRSU
076800021119
076900021119      * Cerco il Network della Lina di Arrivo
077000021119     c                   Clear                   Og143
077100021119     c     D01Lna        Chain     Azorg01L
077200021119     c                   If        %Found(Azorg01l)
077300021119     c                   Movel     Orgde3        Og143
077400021119     c                   EndIf
077500061107
077600061107      * reperisco variabili programmi operativi
077700120510     c****               clear                   tibs02ds
077800120510     c****               clear                   dvpodeco
077900120510     c****               eval      t02mod = 'C'
078000120510     c****               eval      t02sif = knsif
078100120510     c****               eval      t02cod = 'VPO'
078200120510     c****               eval      t02ke1 = 'DECO'
078300120510     c****               call      'TIBS02R'
078400120510     c****               parm                    kpjba
078500120510     c****               parm                    tibs02ds
078600120510    2c****               if        t02err = *blanks
078700120510     c****               eval      dvpodeco = t02uni
078800120510    2c***                endif
078900990802      *
079000990802      * LEGGO IL RECORD
079100990802     C     D01OP1        IFEQ      'O05'
079200000000     C                   Z-ADD     D01AAS        KASAAS                         *ANNO
079300000000     C                   Z-ADD     D01LNP        KASLNP                         *LNP
079400000000     C                   Z-ADD     D01NRS        KASNRS                         *SERIE
079500000000     C                   Z-ADD     D01NSP        KASNSP                         *SPEDIZIONE
079600000000     C                   MOVEL     D01DTV        KASDTV                         *DATA VARIAZIONE
079700000000     C                   MOVEL     D01ORV        KASORV                         *ORA
079800000000     C                   MOVEL     D01PRU        KASPRU                         *UTENTE
079900990802     C     KEYTAS        CHAIN     TITASV1L                           9998
080000990802     C     *IN99         IFEQ      '1'                                          *NON TROVATO
080100000000     C                   MOVEL     '1'           $ERR
080200000000     C                   MOVEL     '1'           $FINE
080300000000     C                   MOVEL     MSG(21)       D01MSG
080400990802     C                   ELSE
080500990802     C     *IN98         IFEQ      '1'                                          *ALLOCAZIONE
080600000000     C                   MOVEL     '1'           $ERR
080700000000     C                   MOVEL     '1'           $FINE
080800000000     C                   MOVEL     MSG(22)       D01MSG
080900990802     C                   ELSE
081000120510      * accendo indicatore 15 per visualizzare sempre il codice fiscale
081100120510     c                   eval      *in15 = *off
081200990802      *
081300000000     C                   EXSR      RECVID                                       *RIEMPE IL VIDEO
081400000000     C                   EXSR      CARV1                                        CARICA TESTA/FUNZION
081500990802      *
081600990802     C                   ENDIF
081700990802     C                   ENDIF
081800990802     C                   ENDIF
081900990802      *
082000990802      * DECODIFICO
082100000000     C     V1CAAS        IFGT      *ZERO
082200000000     C     V1CLNP        ANDGT     *ZERO
082300000000     C     V1CNSP        ANDGT     *ZERO
082400000000     C     V1CTBL        ANDNE     *BLANKS
082500000000     C                   EXSR      DECODI                                       *DECODIFICHE
082600000000     C                   ENDIF
082700990802      *
082800020724     C     EndINZ        tag
082900000000     C                   ENDSR
083000990802      *------------------------------------------------------------------------*
083100990802      *    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
083200990802      *    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
083300990802      *    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
083400990802      *    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
083500990802      *    NO E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
083600990802      *    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
083700990802      *    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
083800990802      *------------------------------------------------------------------------*
083900990802      * INZSTD - ROUTINE INIZIALE STANDARD
084000990802      *------------------------------------------------------------------------*
084100000000     C     INZSTD        BEGSR
084200990802      * CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
084300000000     C                   CALL      'XKEYS'
084400000000     C                   PARM                    DSKEYS
084500000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
084600990802      * PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
084700000000     C     XMSGPA        PLIST
084800000000     C                   PARM                    DSMSMS
084900000000     C                   PARM                    DSMSPG
085000000000     C                   PARM                    DSMSKE
085100000000     C                   PARM                    DSMSER
085200990802      * IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
085300990802     C                   MOVEL     'TNSB54R'     DSMSPG                         *NOME PGM CHIAMANTE
085400990802     C                   MOVEL     'TNSB54R'     VMCQUE                         *CODA MESSAGGI DESTI
085500000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
085600000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
085700990802      * INIZIALIZZO FLAG CONTROLLO PGM
085800000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
085900000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
086000000000     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
086100000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
086200000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
086300000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
086400000000     C                   MOVEL     '0'           $TESVD            1            *1=RIEMETT.TEST.VIDE
086500990802      * POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
086600000000     C                   BITOFF    '01234567'    POSMI1            1
086700000000     C                   BITOFF    '01234567'    POSMI2            1
086800000000     C                   BITOFF    '01234567'    POSMA1            1
086900000000     C                   BITOFF    '01234567'    POSMA2            1
087000000000     C                   BITON     '34'          POSMI1            1            *18
087100000000     C                   BITON     '7'           POSMI2            1            *01
087200000000     C                   BITON     '34'          POSMA1            1            *18
087300000000     C                   BITON     '0'           POSMA2            1            *80
087400000000     C                   MOVE      POSMI2        POSMIN            2
087500000000     C                   MOVEL     POSMI1        POSMIN
087600000000     C                   MOVE      POSMA2        POSMAX            2
087700000000     C                   MOVEL     POSMA1        POSMAX
087800020724      *
087900020724      *-- Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
088000020724     c     *dtaara       define    §azute        azuteds
088100020724     c     *dtaara       define    §datiute      ddatiute
088200020724      *
088300020724     c                   in(E)     *dtaara
088400020724if  1c                   IF        %Error  or  RSUT = *blanks
088500020724     c                   call      'TIBS34R'
088600020724     c                   parm                    Tibs34Ds
088700020724     c                   in        *dtaara
088800020724e   1c                   ENDIF
088900020724      *-- Verifica errori e autorità profilo
089000020724sel 1C                   SELECT
089100020724      *-- controllo se ho errori nei dati utente
089200020724      *--   nel qual caso NON risulta un profilo abilitato
089300020724w   1C                   WHEN      DUTERR = 'E'
089400020724     c                   eval      D01msg = DUTMSG
089500020724     C                   eval      $Fine  = '1'
089600020724      *
089700020724      *-- CONTROLLO AUTORITA'
089800020724      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
089900020724      *-- se il chiamante non richiede autorità specifica verificare
090000020724      *--   quella del profilo
090100020724      *-- se il chiamante richiede autorità specifica verificarla,
090200020724      *--  se è blank verificare quella del profilo
090300020724      *
090400020724      * se UTEAUT = *BLANK non siamo in sede
090500020724w   1C                   WHEN      UTEAUT = *blank
090600020724      *
090700020724x   1c                   OTHER
090800020724      *--
090900020724     c                   movel     UTEFAF        DUTE01
091000020724      *-- Autorizzazione alla manutenzione delle bolle di sede
091100020724     c                   clear                   tibs02ds
091200020724     c                   movel     'C'           t02mod
091300020724     c                   movel     knsif         t02sif
091400020724     c                   movel     'LAT'         t02cod
091500020724if  2c                   IF        §UTEibs <> *blanks
091600020724      *--   Con AUTORITÀ SPECIFICA
091700020724     c                   movel(p)  §UTEibs       t02ke1
091800020724x   2c                   ELSE
091900020724      *--   Senza AUTORITÀ SPECIFICA
092000020724     c                   movel(p)  UTEAUT        t02ke1
092100020724e   2c                   ENDIF
092200020724      *
092300020724     c                   call      'TIBS02R'
092400020724     c                   parm                    kpjba
092500020724     c                   parm                    tibs02ds
092600020724if  2c                   if        t02err = *blanks
092700020724     c                   movel     t02uni        DLAT
092800020724e   2c                   endif
092900020724if  2c                   if        t02err <> *blanks   or
093000020724     c                             §LATabi = 'S'
093100020724     c                   eval      D01msg  = MSG(9)
093200020724     c                   eval      $Fine   = *on
093300020724e   2c                   endif
093400020724      *
093500020724e   1C                   ENDSL
093600020724      *
093700000000     C                   ENDSR
093800990802      *------------------------------------------------------------------------*
093900990802      * FUNSTD - LANCIA SUBROUTINE STANDARD
094000990802      *------------------------------------------------------------------------*
094100000000     C     FUNSTD        BEGSR
094200000000     C                   SELECT
094300000000     C     KEY           WHENEQ    F3
094400000000     C                   EXSR      F03STD
094500000000     C     KEY           WHENEQ    F12
094600000000     C                   EXSR      F12STD
094700000000     C     KEY           WHENEQ    F24
094800000000     C                   EXSR      F24STD
094900000000     C                   OTHER
095000000000     C                   MOVEL     MSG(2)        $MSG
095100000000     C                   EXSR      CARMSG
095200000000     C                   ENDSL
095300000000     C                   ENDSR
095400990802      *------------------------------------------------------------------------*
095500990802      * F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
095600990802      *------------------------------------------------------------------------*
095700000000     C     F03STD        BEGSR
095800990802      * IMPOSTO FLAG DI PREMUTO TASTO F03
095900000000     C                   MOVEL     '1'           $F03
096000990802      * IMPOSTO FLAG DI FINE PROGRAMMA
096100000000     C                   MOVEL     '1'           $FINE
096200000000     C                   ENDSR
096300990802      *------------------------------------------------------------------------*
096400990802      * F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
096500990802      *------------------------------------------------------------------------*
096600000000     C     F12STD        BEGSR
096700990802      * IMPOSTO FLAG DI PREMUTO TASTO F12
096800000000     C                   MOVEL     '1'           $F12
096900990802      * IMPOSTO FLAG DI FIN PGM
097000000000     C                   MOVEL     '1'           $FINE
097100000000     C                   ENDSR
097200990802      *------------------------------------------------------------------------*
097300990802      * F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
097400990802      *------------------------------------------------------------------------*
097500000000     C     F24STD        BEGSR
097600990802      * GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
097700000000     C                   EXSR      OUTFUN
097800000000     C                   ENDSR
097900990802      *------------------------------------------------------------------------*
098000990802      * CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
098100990802      *          CODA MESSAGGI DEL PGM
098200990802      *------------------------------------------------------------------------*
098300000000     C     CARMSG        BEGSR
098400990802      * SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
098500990802      * PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
098600000000     C     MSGULT        IFEQ      0
098700000000     C                   MOVEL     '1'           *IN40
098800990802     C                   WRITE     SB54DVK
098900000000     C                   MOVEL     '0'           *IN40
099000000000     C                   ENDIF
099100990802      * INVIA MESSAGGIO ALLA CODA DEL PGM
099200000000     C                   MOVEL     $MSG          DSMSMS
099300000000     C                   CALL      'XMSG'        XMSGPA
099400000000     C     DSMSER        IFEQ      0
099500000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
099600000000     C                   ENDIF
099700990802      * CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
099800000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
099900990802     C                   WRITE     SB54DVM                                      *SFL MESS
100000990802      * PULISCO CAMPO MESSAGGIO APPENA CARICATO
100100000000     C                   MOVEL     *BLANKS       $MSG
100200000000     C                   ENDSR
100300990802      *------------------------------------------------------------------------*
100400990802      * OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
100500990802      *------------------------------------------------------------------------*
100600000000     C     OUTMSG        BEGSR
100700990802      * VISUALIZZAZIONE MESSAGGIO
100800000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
100900990802     C                   WRITE     SB54DVK                                      *CONTR MESS
101000990802      * AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
101100000000     C                   Z-ADD     *ZEROS        MSGULT
101200000000     C                   ENDSR
101300990802      *------------------------------------------------------------------------*
101400990802      * CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
101500990802      *          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
101600990802      *          OUTPUT: - FFU    (SCHIERA FUNZIONI)
101700990802      *                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
101800990802      *                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
101900990802      *------------------------------------------------------------------------*
102000000000     C     CARFUN        BEGSR
102100000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
102200000000     C     1             DO        31            I                 5 0          --- 1 -->
102300000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
102400000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
102500000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
102600020724     C     DUTlpo        IFEQ      'S'                                          --- 2 --> *SEDE
102700000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
102800000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
102900000000     C                   ELSE                                                   *INTERROG.
103000000000     C                   MOVEL     FUNASI        FAT(I)
103100000000     C                   ENDIF
103200000000     C                   ELSE                                                   <-- 2 --> *FILIALE
103300000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
103400000000     C                   MOVEL     FUNAFG        FAT(I)
103500000000     C                   ELSE                                                   *INTERROG.
103600000000     C                   MOVEL     FUNAFI        FAT(I)
103700000000     C                   ENDIF
103800000000     C                   ENDIF                                                  <-- 2 ---
103900000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
104000000000     C                   MOVEL     USER          WEDP              3            *PROFILO
104100000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
104200000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
104300000000     C                   ELSE
104400000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
104500000000     C                   ENDIF
104600000000     C                   ENDIF
104700000000     C                   ENDDO                                                  <-- 1 ---
104800000000     C                   ENDSR
104900990802      *------------------------------------------------------------------------*
105000990802      * OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
105100990802      *          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
105200990802      *          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
105300990802      *                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
105400990802      *------------------------------------------------------------------------*
105500000000     C     OUTFUN        BEGSR
105600000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
105700000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
105800000000     C                   ENDDO
105900990802      * CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
106000000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
106100000000     C     1             DO        31            I
106200000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
106300000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
106400000000     C                   ADD       1             FUNNAT
106500000000     C                   ENDIF
106600000000     C                   ENDDO
106700990802      * SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
106800000000     C     FUNNAT        IFGT      8
106900000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
107000000000     C                   ENDIF
107100990802      * IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
107200990802      * (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
107300000000     C                   Z-ADD     8             FUNVIS            5 0
107400000000     C     FAT(23)       IFEQ      'S'
107500000000     C                   SUB       1             FUNVIS
107600000000     C                   ENDIF
107700000000     C     FAT(24)       IFEQ      'S'
107800000000     C                   SUB       1             FUNVIS
107900000000     C                   ENDIF
108000990802      * AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
108100000000     C                   Z-ADD     *ZEROS        W                 5 0
108200000000     C                   ADD       1             FUNULT
108300000000     C     FUNULT        IFGT      31
108400000000     C                   Z-ADD     1             FUNULT
108500000000     C                   ENDIF
108600990802      * RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
108700000000     C     FUNULT        DO        31            I                              --- 1 -->
108800990802      * SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
108900000000     C     W             IFLT      FUNVIS                                       --- 2 -->
109000990802      * SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
109100000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
109200000000     C     FDE(I)        ANDNE     *BLANKS
109300000000     C     I             IFEQ      23                                           *TOLGO F23
109400000000     C     I             OREQ      24                                           *E F24
109500000000     C                   ELSE
109600000000     C                   ADD       1             W
109700000000     C                   MOVEL     FDE(I)        FUV(W)
109800000000     C                   ENDIF
109900000000     C                   ENDIF                                                  <-- 3 ---
110000990802      * MEMORIZZO L'ULTIMO INDICE TRATTATO
110100000000     C                   Z-ADD     I             FUNULT
110200000000     C                   ENDIF                                                  <-- 2 ---
110300000000     C                   ENDDO                                                  <-- 1 ---
110400990802      * TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
110500000000     C                   MOVEL     FUV(1)        VFD001
110600000000     C                   MOVEL     FUV(2)        VFD002
110700000000     C                   MOVEL     FUV(3)        VFD003
110800000000     C                   MOVEL     FUV(4)        VFD004
110900000000     C                   MOVEL     FUV(5)        VFD005
111000000000     C                   MOVEL     FUV(6)        VFD006
111100000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
111200000000     C     FAT(24)       ANDEQ     'N'
111300000000     C                   MOVEL     FUV(7)        VFD007
111400000000     C                   MOVEL     FUV(8)        VFD008
111500000000     C                   ELSE                                                   <-- 1 -->
111600000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
111700000000     C     FAT(24)       ANDEQ     'S'
111800000000     C                   MOVEL     FDE(23)       VFD007
111900000000     C                   MOVEL     FDE(24)       VFD008
112000000000     C                   ELSE                                                   <-- 2 -->
112100000000     C                   MOVEL     FUV(7)        VFD007
112200000000     C     FAT(23)       IFEQ      'S'
112300000000     C                   MOVEL     FDE(23)       VFD008
112400000000     C                   ELSE
112500000000     C                   MOVEL     FDE(24)       VFD008
112600000000     C                   ENDIF
112700000000     C                   ENDIF                                                  <-- 2 ---
112800000000     C                   ENDIF                                                  <-- 1 ---
112900000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
113000000000     C                   ENDSR
113100990802      *------------------------------------------------------------------------*
113200990802      *  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
113300990802      *           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
113400990802      *           OUTPUT: - FUNOK  (OPZIONE OK S/N)
113500990802      *                   - ERRORE SE FUNZIONE NON ATTIVA
113600990802      *------------------------------------------------------------------------*
113700000000     C     CONFUN        BEGSR
113800990802      * RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
113900000000     C     1             DO        31            I                              --- 1 -->
114000000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
114100990802      * CONTROLLO SE IL TASTO E' ATTIVO
114200000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
114300000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
114400000000     C                   EXSR      CARMSG
114500000000     C                   MOVEL     '1'           $ERR
114600000000     C                   ENDIF                                                  <-- 3 ---
114700000000     C                   ENDIF                                                  <-- 2 ---
114800000000     C                   ENDDO                                                  <-- 1 ---
114900000000     C                   ENDSR
115000000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
115100990802TNSB54R- Tasto funzionale non valido.                                          1
115200990802TNSB54R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
115300990802TNSB54R- Opzione non valida.                                                   3
115400990802TNSB54R- Opzione abilitata ma non gestita. Avvertire CED.                      4
115500990802TNSB54R- Inizio elenco.                                                        5
115600990802TNSB54R- Fine elenco.                                                          6
115700990802TNSB54R- Non vi sono variazioni precedenti                                     7
115800990802TNSB54R- Non vi sono variazioni successive                                     8
115900020724TNSB54R- Utente non autorizzato.                                               9
116000990802TNSB54R- 10
116100990802TNSB54R- 11
116200990802TNSB54R- 12
116300990802TNSB54R- 13
116400990802TNSB54R- 14
116500990802TNSB54R- 15
116600990802TNSB54R- 16
116700990802TNSB54R- Inserimento effettuato.                                              17
116800990802TNSB54R- Modifica effettuata.                                                 18
116900990802TNSB54R- Cancellazione effettuata.                                            19
117000990802TNSB54R- Ripristino effettuato.                                               20
117100990802TNSB54R- Richiesta funzione per registrazione gia' esistente                  21
117200990802TNSB54R- Registrazione momentaneamente allocata: riprovare piu' tardi         22
117300990802TNSB54R- 23
117400990802TNSB54R- 24
117500990802TNSB54R- 25
117600990802TNSB54R- 26
117700990802TNSB54R- 27
117800990802TNSB54R- 28
117900990802TNSB54R- 29
118000990802TNSB54R- 30
118100000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
118200000000F1    -                   -    NN     NN
118300000000F2    -                   -    NN     NN
118400000000F3    -F3=Fine            -    SS     SS
118500000000F4    -                   -    NN     NN
118600000000F5    -                   -    NN     NN
118700000000F6    -                   -    NN     NN
118800000000F7    -                   -    NN     NN
118900000000F8    -F8=Variaz.preced.  -    SS     SS
119000000000F9    -F9=Variaz.success. -    SS     SS
119100000000F10   -                   -    NN     NN
119200000000F11   -                   -    NN     NN
119300000000F12   -F12=Ritorno        -    SS     SS
119400000000F13   -                   -    NN     NN
119500000000F14   -                   -    NN     NN
119600000000F15   -                   -    NN     NN
119700000000F16   -                   -    NN     NN
119800000000F17   -                   -    NN     NN
119900000000F18   -                   -    NN     NN
120000000000F19   -                   -    NN     NN
120100000000F20   -                   -    NN     NN
120200000000F21   -                   -    NN     NN
120300000000F22   -                   -    NN     NN
120400000000F23   -                   -    NN     NN
120500000000F24   -F24=Altri tasti    -    NN     NN
120600000000ENTER -                   -    SS     SS
120700000000ROLLUP-                   -    SS     SS
120800000000ROLLDN-                   -    SS     SS
120900000000HELP  -                   -    NN     NN
121000000000PRINT -                   -    NN     NN
121100000000BSPACE-                   -    NN     NN
121200000000CLEAR -                   -    NN     NN
121300000000** 456789012345678901234567890123456789012345678901234567890
121400000000Variazione del xx/xx/xxxx ore xx:xx:xx utente xxxxxxxxxx
