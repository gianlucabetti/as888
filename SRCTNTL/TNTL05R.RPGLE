000100020715     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*------------------------------------------------------------------------*
000300000000     H* GESTIONE TRAINI - FERMATE - ROUTINE DI GESTIONE
000400000000     H*------------------------------------------------------------------------*
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800000000     FTNTL05D   CF   E             WORKSTN INFDS(DEVDS1)
000900000000     F                                     SFILE(TL05DVM:MSGULT)
001000000000     FAZORG01L  IF   E           K DISK
001100000000     FTNTLT01L  UF A E           K DISK
001200000000     F                                     COMMIT
001300000000     D*------------------------------------------------------------------------*
001400000000     D* SCHIERE
001500000000     D*------------------------------------------------------------------------*
001600000000     D*-------------------
001700000000     D* MESSAGGI E FUNZIONI
001800000000     D*-------------------
001900000000     D MSG             S             78    DIM(30) CTDATA PERRCD(1)             *MESSAGGI
002000000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
002100000000     D*-------------------
002200000000     D* DECODIFICA OPZIONE RICHIAMATA
002300000000     D*-------------------
002400000000     D*---                DOPZ    1   5 20               *DECODIFICA OPZIONE
002500000000     D*-------------------
002600000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002700000000     D*-------------------
002800000000     D FFU             S              6    DIM(31)                              *FUNZIONE
002900000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
003000000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
003100000000     D*-------------------
003200000000     D* TABELLA DESCRIZIONI A VIDEO
003300000000     D*-------------------
003400000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
003500000000     D*-------------------
003600000000     D* TABELLA TASTI FUNZIONALI PREMUTI
003700000000     D*-------------------
003800000000     D KEYS            S              1    DIM(40)
003900000000     D*-------------------
004000000000     D* PROGRESSIVO GIORNI
004100000000     D*-------------------
004200000000     D DALF            S             21    DIM(1) CTDATA PERRCD(1)              *ALFABETO (1÷21)
004300000000     D ALF             S              1    DIM(21)                              *ALFABETO (1÷21)
004400000000     D*-------------------
004500000000     D* FILIALI
004600000000     D*-------------------
004700971125     D ORG             S              3  0 DIM(500)                             *CODICE
004800971125     D DORG            S             30    DIM(500)                             *DESCRIZIONE
004900000000     D*------------------------------------------------------------------------*
005000000000     D* INPUT
005100000000     D*------------------------------------------------------------------------*
005200000000     D*-------------------
005300000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
005400000000     D*-------------------
005500000000     D DSFUN         E DS
005600000000     D  FUNRIG                 1     40
005700000000     D*-------------------
005800000000     D* DS PER GESTIONE TASTI FUNZIONALI
005900000000     D*-------------------
006000000000     D DSKEYS        E DS
006100000000     D  KEYRIG                 1     40
006200000000     D*-------------------
006300000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
006400000000     D*-------------------
006500000000     D                SDS
006600000000     D  USER                 254    263
006700000000     D*-------------------
006800000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
006900000000     D*-------------------
007000000000     D DEVDS1          DS
007100000000     D  KEY                  369    369
007200000000     D  CURSOR               370    371
007300000000     D  FRTSFL               378    379B 0
007400000000     D*-------------------
007500000000     D* ARCHITETTURA
007600000000     D*-------------------
007700000000     D KPJBA         E DS
007800000000     D*---------------------
007900000000     D* DS ESTERNA DI PROCEDURA
008000000000     D*---------------------
008100000000     D DS00          E DS                  EXTNAME(TNTL00DS)
008200960208     D*---------------------
008300960208     D* DS 148^ DESCRIZIONE ORGANIGRAMMA
008400960208     D*---------------------
008500960208     D OG148         E DS
008600000000     D*-------------------
008700000000     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
008800000000     D*-------------------
008900000000     D DSMSG         E DS
009000020715
009100020715     d DdatiUte      e ds
009200020715     d AzuteDs       e ds                  ExtName(Azute00f)
009300020715     d Tibs34Ds      e ds                  Inz
009400020715
009500000000     D*------------------
009600000000     D* DS "XSRDA8" - CONTROLLA DATA (8)
009700000000     D*------------------
009800000000     D WLBDA8          DS
009900000000     D  G08DAT                 1      8  0
010000000000     D  G08INV                 9     16  0
010100000000     D  G08ERR                17     17
010200000000     D  G08TGI                18     22  0
010300000000     C*------------------------------------------------------------------------*
010400000000     C* MAIN LINE
010500000000     C*------------------------------------------------------------------------*
010600000000     C*
010700000000     C* SE VI E' STATO UN ERRORE NELLA ROUTINE INIZIALE ESCE DAL PGM
010800000000IF  1C     $FINE         IFEQ      '1'
010900000000     C                   MOVEL     '1'           D00ERR
011000000000X   1C                   ELSE
011100000000     C*
011200000000DO  2C     $FINE         DOWEQ     '0'
011300000000     C                   MOVEL     '0'           $ERR
011400000000     C*
011500000000IF  3C     $GES          IFEQ      'V1'
011600000000     C                   EXSR      GESV1
011700000000E   3C                   ENDIF
011800000000     C*
011900000000E   2C                   ENDDO
012000000000E   1C                   ENDIF
012100000000     C*
012200000000IF  1C* SE E' STATO PREMUTO F03 O F12 IMPOSTA FLAG DELLA DS DI PROCEDURA
012300000000     C     $F03          IFEQ      '1'
012400000000     C                   MOVEL     '1'           D00F03
012500000000E   1C                   ENDIF
012600000000     C     $F12          IFEQ      '1'
012700000000     C                   MOVEL     '1'           D00F12
012800000000E   1C                   ENDIF
012900000000     C*
013000000000     C                   MOVEL     DS00          KPJBU
013100000000     C                   SETON                                        LR
013200000000     C*------------------------------------------------------------------------*
013300000000     C* CARV1  - CARICA GESTIONE PRIMO VIDEO
013400000000     C*------------------------------------------------------------------------*
013500000000     C     CARV1         BEGSR
013600000000     C*
013700000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
013800000000     C*
013900000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
014000000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
014100000000     C*
014200000000     C                   WRITE     TL05DVT                                      *TESTATA
014300000000     C*---                 WRITETL05DVF                    *FUNZIONI
014400000000     C*
014500000000     C                   ENDSR
014600000000     C*------------------------------------------------------------------------*
014700000000     C* GESV1  - GESTIONE PRIMO VIDEO
014800000000     C*------------------------------------------------------------------------*
014900000000     C     GESV1         BEGSR
015000000000     C*
015100000000IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
015200000000     C                   EXSR      CARV1                                        *CARICA VIDEO MOVIM
015300000000E   1C                   ENDIF
015400000000     C*
015500000000IF  1C     MSGULT        IFNE      *ZEROS                                       *SE CI SONO MESSAGGG
015600000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
015700000000E   1C                   ENDIF
015800000000     C*
015900000000     C                   EXFMT     TL05DV1                                      *TEMPO DELIVERY
016000000000     C*
016100000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
016200000000     C*
016300000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
016400000000SL  2C                   SELECT
016500000000     C     KEY           WHENEQ    ENTER
016600000000     C                   EXSR      ENTV1                                        *GEST ENTER VIDEO V1
016700000000     C                   OTHER
016800000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
016900000000E   2C                   ENDSL
017000000000E   1C                   ENDIF
017100000000     C*
017200000000     C                   ENDSR
017300000000     C*------------------------------------------------------------------------*
017400000000     C* ENTV1  - GESTIONE TASTO ENTER PRIMO VIDEO
017500000000     C*------------------------------------------------------------------------*
017600000000     C     ENTV1         BEGSR
017700000000     C*
017800000000     C* CONTROLLA I CAMPI IMMESSI
017900000000     C                   EXSR      DECODI
018000000000     C*
018100000000     C* NO ERRORI O RICHIESTO ANNULLAMENTO
018200000000IF  1C     $ERR          IFEQ      '0'
018300000000     C     D00OP1        OREQ      'O04'
018400000000     C                   Z-ADD     V1CSEQ        D00SEQ                         *SEQUENZA
018500000000     C                   MOVEL     V1CTFM        D00TFM                         *TIPO FERMATA
018600000000     C                   Z-ADD     V1CFIL        D00FIL                         *FILIALE FERMATA
018700000000     C                   MOVEL     V1CGGA        D00GGA                         *GIORNO ARRIVO
018800000000     C                   Z-ADD     V1CHHA        D00HHA                         *ORA ARRIVO
018900000000     C                   MOVEL     V1CSCA        D00SCA                         *SCARICO
019000000000     C                   MOVEL     V1CGGP        D00GGP                         *GIORNO PARTENZA
019100000000     C                   Z-ADD     V1CHHP        D00HHP                         *ORA PARTENZA
019200000000     C                   MOVEL     V1CCAR        D00CAR                         *CARICO
019300000000     C                   Z-ADD     V1CKMT        D00KMT                         *CHILOMETRI
019400000000     C                   Z-ADD     V1CTEM        D00TEM                         *TEMPO
019500000000     C                   MOVEL     V1CGFV        D00GFV                         *APERTURA F.V.
019600000000     C                   MOVEL     '1'           $FINE
019700000000E   1C                   ENDIF
019800000000     C*
019900000000     C                   ENDSR
020000000000     C*------------------------------------------------------------------------*
020100000000     C* DECODI - CONTROLLA LE SCHIERE DEL GIORNO
020200000000     C*------------------------------------------------------------------------*
020300000000     C     DECODI        BEGSR
020400000000     C*
020500000000     C* REIMPOSTA INDICATORI DI ERRORE
020600000000     C                   SETOFF                                       606162
020700000000     C                   SETOFF                                       636465
020800000000     C                   SETOFF                                       666768
020900000000     C                   SETOFF                                       697071
021000000000     C*
021100000000DO  1C                   DO
021200000000     C***
021300000000     C* SEQUENZA
021400000000     C***
021500000000IF  2C     V1CSEQ        IFEQ      *ZEROS
021600000000     C                   MOVEL     '1'           $ERR
021700000000     C                   MOVEL     MSG(11)       $MSG
021800000000     C                   EXSR      CARMSG
021900000000     C                   SETON                                        60
022000000000     C                   LEAVE
022100000000E   2C                   ENDIF
022200000000     C***
022300000000     C* FERMATE DI TIPO "A"=ANDATA O "R"=RITORNO
022400000000     C***
022500000000IF  2C     V1CTFM        IFNE      'A'
022600000000     C     V1CTFM        ANDNE     'R'
022700000000     C                   MOVEL     '1'           $ERR
022800000000     C                   MOVEL     MSG(16)       $MSG
022900000000     C                   EXSR      CARMSG
023000000000     C                   SETON                                        61
023100000000     C                   LEAVE
023200000000E   2C                   ENDIF
023300000000     C***
023400000000     C* FERMATE DI TIPO "R"=RITORNO SOLO PER TRAINO DI TIPO "C"=CHIUSO
023500000000     C***
023600000000IF  2C     TLTTTR        IFNE      'C'
023700000000     C     V1CTFM        ANDEQ     'R'
023800000000     C                   MOVEL     '1'           $ERR
023900000000     C                   MOVEL     MSG(12)       $MSG
024000000000     C                   EXSR      CARMSG
024100000000     C                   SETON                                        61
024200000000     C                   LEAVE
024300000000E   2C                   ENDIF
024400000000     C***
024500000000     C* FILIALE VALIDA
024600000000     C***
024700000000     C                   Z-ADD     V1CFIL        WORG
024800000000     C                   MOVEL     *BLANKS       WDORG
024900000000     C                   EXSR      CHKFIL
025000000000     C                   MOVEL     WDORG         V1DFIL
025100000000IF  2C     $ERR          IFEQ      '1'
025200000000     C                   MOVEL     MSG(17)       $MSG
025300000000     C                   EXSR      CARMSG
025400000000     C                   SETON                                        62
025500000000     C                   LEAVE
025600000000E   2C                   ENDIF
025700000000     C***
025800000000     C* PROGRESSIVO GIORNO ARRIVO
025900000000     C***
026000000000IF  2C     V1CGGA        IFNE      *BLANKS
026100000000     C                   SETOFF                                           28
026200000000     C     V1CGGA        LOOKUP    ALF                                    28
026300000000IF  3C     *IN28         IFEQ      *OFF
026400000000     C                   MOVEL     '1'           $ERR
026500000000     C                   MOVEL     MSG(18)       $MSG
026600000000     C                   EXSR      CARMSG
026700000000     C                   SETON                                        63
026800000000     C                   LEAVE
026900000000E   3C                   ENDIF
027000000000E   2C                   ENDIF
027100000000     C***
027200000000     C* ORA ARRIVO
027300000000     C***
027400000000IF  2C     V1CHHA        IFGT      *ZEROS
027500000000     C                   Z-ADD     V1CHHA        WORA
027600000000     C                   EXSR      CHKORA
027700000000IF  3C     $ERR          IFEQ      '1'
027800000000     C                   MOVEL     MSG(20)       $MSG
027900000000     C                   EXSR      CARMSG
028000000000     C                   SETON                                        64
028100000000     C                   LEAVE
028200000000E   3C                   ENDIF
028300000000E   2C                   ENDIF
028400000000     C***
028500000000     C* SCARICO
028600000000     C***
028700000000IF  2C     V1CSCA        IFNE      'S'
028800000000     C     V1CSCA        ANDNE     *BLANKS
028900000000     C                   MOVEL     '1'           $ERR
029000000000     C                   MOVEL     MSG(22)       $MSG
029100000000     C                   EXSR      CARMSG
029200000000     C                   SETON                                        65
029300000000     C                   LEAVE
029400000000E   2C                   ENDIF
029500000000     C***
029600000000     C* SCARICO E DATI ARRIVO
029700000000     C***
029800000000IF  2C     V1CSCA        IFEQ      'S'
029900000000IF  3C     V1CGGA        IFEQ      *BLANKS
030000000000     C     V1CHHA        OREQ      *ZEROS
030100000000     C                   MOVEL     '1'           $ERR
030200000000     C                   MOVEL     MSG(27)       $MSG
030300000000     C                   EXSR      CARMSG
030400000000     C                   SETON                                        636465
030500000000     C                   LEAVE
030600951212E   3C                   ENDIF
030700951212X   2C                   ELSE
030800951212IF  3C     V1CGGA        IFNE      *BLANKS
030900951212     C     V1CHHA        ORNE      *ZEROS
031000951212     C                   MOVEL     '1'           $ERR
031100951212     C                   MOVEL     MSG(27)       $MSG
031200951212     C                   EXSR      CARMSG
031300951212     C                   SETON                                        636465
031400951212     C                   LEAVE
031500951212E   3C                   ENDIF
031600951212E   2C                   ENDIF
031700000000     C***
031800000000     C* PROGRESSIVO E ORA ARRIVO
031900000000     C***
032000000000IF  2C     V1CGGA        IFNE      *BLANKS
032100000000IF  3C     V1CHHA        IFEQ      *ZEROS
032200000000     C                   MOVEL     '1'           $ERR
032300000000     C                   MOVEL     MSG(28)       $MSG
032400000000     C                   EXSR      CARMSG
032500000000     C                   SETON                                        6364
032600000000     C                   LEAVE
032700000000E   3C                   ENDIF
032800000000X   2C                   ELSE
032900000000IF  3C     V1CHHA        IFGT      *ZEROS
033000000000     C                   MOVEL     '1'           $ERR
033100000000     C                   MOVEL     MSG(28)       $MSG
033200000000     C                   EXSR      CARMSG
033300000000     C                   SETON                                        6364
033400000000     C                   LEAVE
033500000000E   3C                   ENDIF
033600000000E   2C                   ENDIF
033700000000     C***
033800000000     C* PROGRESSIVO GIORNO PARTENZA
033900000000     C***
034000000000IF  2C     V1CGGP        IFNE      *BLANKS
034100000000     C                   SETOFF                                           28
034200000000     C     V1CGGP        LOOKUP    ALF                                    28
034300000000IF  3C     *IN28         IFEQ      *OFF
034400000000     C                   MOVEL     '1'           $ERR
034500000000     C                   MOVEL     MSG(19)       $MSG
034600000000     C                   EXSR      CARMSG
034700000000     C                   SETON                                        66
034800000000     C                   LEAVE
034900000000E   3C                   ENDIF
035000000000E   2C                   ENDIF
035100000000     C***
035200000000     C* ORA PARTENZA
035300000000     C***
035400000000IF  2C     V1CHHP        IFGT      *ZEROS
035500000000     C                   Z-ADD     V1CHHP        WORA
035600000000     C                   EXSR      CHKORA
035700000000IF  3C     $ERR          IFEQ      '1'
035800000000     C                   MOVEL     MSG(21)       $MSG
035900000000     C                   EXSR      CARMSG
036000000000     C                   SETON                                        67
036100000000     C                   LEAVE
036200000000E   3C                   ENDIF
036300000000E   2C                   ENDIF
036400000000     C***
036500000000     C* CARICO
036600000000     C***
036700000000IF  2C     V1CCAR        IFNE      'S'
036800000000     C     V1CCAR        ANDNE     *BLANKS
036900000000     C                   MOVEL     '1'           $ERR
037000000000     C                   MOVEL     MSG(23)       $MSG
037100000000     C                   EXSR      CARMSG
037200000000     C                   SETON                                        68
037300000000     C                   LEAVE
037400000000E   2C                   ENDIF
037500000000     C***
037600951212     C* CARICO E DATI PARTENZA
037700000000     C***
037800000000IF  2C     V1CCAR        IFEQ      'S'
037900000000IF  3C     V1CGGP        IFEQ      *BLANKS
038000000000     C     V1CHHP        OREQ      *ZEROS
038100000000     C                   MOVEL     '1'           $ERR
038200000000     C                   MOVEL     MSG(29)       $MSG
038300000000     C                   EXSR      CARMSG
038400000000     C                   SETON                                        666768
038500000000     C                   LEAVE
038600000000E   3C                   ENDIF
038700951212X   2C                   ELSE
038800951212IF  3C     V1CGGP        IFNE      *BLANKS
038900951212     C     V1CHHP        ORNE      *ZEROS
039000951212     C                   MOVEL     '1'           $ERR
039100951212     C                   MOVEL     MSG(29)       $MSG
039200951212     C                   EXSR      CARMSG
039300951212     C                   SETON                                        666768
039400951212     C                   LEAVE
039500951212E   3C                   ENDIF
039600000000E   2C                   ENDIF
039700000000     C***
039800000000     C* PROGRESSIVO E ORA PARTENZA
039900000000     C***
040000000000IF  2C     V1CGGP        IFNE      *BLANKS
040100000000IF  3C     V1CHHP        IFEQ      *ZEROS
040200000000     C                   MOVEL     '1'           $ERR
040300000000     C                   MOVEL     MSG(30)       $MSG
040400000000     C                   EXSR      CARMSG
040500000000     C                   SETON                                        6667
040600000000     C                   LEAVE
040700000000E   3C                   ENDIF
040800000000X   2C                   ELSE
040900000000IF  3C     V1CHHP        IFGT      *ZEROS
041000000000     C                   MOVEL     '1'           $ERR
041100000000     C                   MOVEL     MSG(30)       $MSG
041200000000     C                   EXSR      CARMSG
041300000000     C                   SETON                                        6667
041400000000     C                   LEAVE
041500000000E   3C                   ENDIF
041600000000E   2C                   ENDIF
041700000000     C***
041800000000     C* GIORNO APERTURA FOGLIO DI VIAGGIO
041900000000     C***
042000000000IF  2C     V1CCAR        IFEQ      'S'
042100000000     C                   SETOFF                                           28
042200000000     C     V1CGFV        LOOKUP    ALF                                    28
042300000000IF  3C     *IN28         IFEQ      *OFF
042400000000     C                   MOVEL     '1'           $ERR
042500000000     C                   MOVEL     MSG(13)       $MSG
042600000000     C                   EXSR      CARMSG
042700000000     C                   SETON                                        71
042800000000     C                   LEAVE
042900000000E   3C                   ENDIF
043000000000X   2C                   ELSE
043100000000IF  3C     V1CGFV        IFNE      *BLANKS
043200000000     C                   MOVEL     '1'           $ERR
043300000000     C                   MOVEL     MSG(14)       $MSG
043400000000     C                   EXSR      CARMSG
043500000000     C                   SETON                                        71
043600000000     C                   LEAVE
043700000000E   3C                   ENDIF
043800000000E   2C                   ENDIF
043900000000     C***
044000000000     C* TEMPO E CHILOMETRI
044100000000     C***
044200951212     C* SE NON STATI SPECIFICATI I DATI ARRIVO, TEMPO E KM DEVONO ESSERE VUOTI
044300030716IF  2C**!!!V1CGGA        IFEQ      *BLANKS
044400030716IF  3C**!!!V1CTEM        IFNE      *ZEROS
044500030716     C**!!!V1CKMT        ORNE      *ZEROS
044600030716     C**!!!              MOVEL     '1'           $ERR
044700030716     C**!!!              MOVEL     MSG(26)       $MSG
044800030716     C**!!!              EXSR      CARMSG
044900030716     C**!!!              SETON                                        6970
045000030716     C**!!!              LEAVE
045100030716E   3C**!!!              ENDIF
045200030716E   2C**!!!              ENDIF
045300000000     C***
045400000000     C* TEMPO
045500000000     C***
045600030715IF  2C**!!!V1CTEM        IFGT      *ZEROS
045700030715     C**!!!              Z-ADD     V1CTEM        WORA
045800030715     C**!!!              EXSR      CHKORA
045900030715IF  3C**!!!$ERR          IFEQ      '1'
046000030715     C**!!!              MOVEL     MSG(25)       $MSG
046100030715     C**!!!              EXSR      CARMSG
046200030715     C**!!!              SETON                                        70
046300030715     C**!!!              LEAVE
046400030715E   3C**!!!              ENDIF
046500030715E   2C**!!!              ENDIF
046600000000     C*
046700000000E   1C                   ENDDO
046800000000     C*
046900000000     C                   ENDSR
047000000000     C*------------------------------------------------------------------------*
047100000000     C* CHKFIL - CONTROLLA LA FILIALE
047200000000     C*------------------------------------------------------------------------*
047300000000     C     CHKFIL        BEGSR
047400000000     C*
047500000000     C     *LIKE         DEFINE    V1CFIL        WORG
047600000000     C     *LIKE         DEFINE    V1DFIL        WDORG
047700000000     C*
047800000000IF  1C     WORG          IFEQ      *ZEROS
047900000000     C                   MOVEL     '1'           $ERR
048000000000X   1C                   ELSE
048100000000     C                   SETOFF                                           28
048200000000     C                   Z-ADD     1             J
048300000000     C     WORG          LOOKUP    ORG(J)                                 28
048400000000IF  2C     *IN28         IFEQ      *OFF
048500000000     C                   MOVEL     '1'           $ERR
048600000000     C                   MOVEL     *ALL'*'       WDORG
048700000000X   2C                   ELSE
048800000000     C                   MOVEL     DORG(J)       WDORG
048900000000E   2C                   ENDIF
049000000000E   1C                   ENDIF
049100000000     C*
049200000000     C                   ENDSR
049300000000     C*------------------------------------------------------------------------*
049400000000     C* CHKORA - CONTROLLA L'ORA
049500000000     C*------------------------------------------------------------------------*
049600000000     C     CHKORA        BEGSR
049700000000     C*
049800000000     C     *LIKE         DEFINE    V1CHHA        WORA
049900000000     C                   Z-ADD     *ZEROS        WHH               2 0
050000000000     C                   Z-ADD     *ZEROS        WMM               2 0
050100000000     C* ORE E MINUTI
050200000000IF  1C     WORA          IFEQ      *ZEROS
050300000000     C                   MOVEL     '1'           $ERR
050400000000E   1C                   ENDIF
050500000000     C* ORE
050600000000     C                   MOVEL     WORA          WHH
050700000000IF  1C     WHH           IFGE      0                                            0 -> 24
050800000000     C     WHH           ANDLE     24
050900000000X   1C                   ELSE
051000000000     C                   MOVEL     '1'           $ERR
051100000000E   1C                   ENDIF
051200000000     C* MINUTI
051300000000     C                   MOVE      WORA          WMM
051400000000IF  1C     WMM           IFGE      0                                            0 -> 59
051500000000     C     WMM           ANDLE     59
051600000000     C                   ELSE
051700000000     C                   MOVEL     '1'           $ERR
051800000000E   1C                   ENDIF
051900000000     C* ORE 24:00
052000000000IF  1C     WHH           IFEQ      24                                           *SOLO 24:00
052100000000     C     WMM           ANDGT     0
052200000000     C                   MOVEL     '1'           $ERR
052300000000E   1C                   ENDIF
052400000000     C*
052500000000     C                   ENDSR
052600000000     C*------------------------------------------------------------------------*
052700000000     C*  RECVID - RIEMPE I CAMPI DEL VIDEO CON QUELLI DEL FILE
052800000000     C*------------------------------------------------------------------------*
052900000000     C     RECVID        BEGSR
053000000000     C*
053100000000     C                   Z-ADD     D00SEQ        V1CSEQ                         *SEQUENZA
053200000000     C                   MOVEL     D00TFM        V1CTFM                         *TIPO FERMATA
053300000000     C                   Z-ADD     D00FIL        V1CFIL                         *FILIALE FERMATA
053400000000     C                   MOVEL     D00GGA        V1CGGA                         *GIORNO ARRIVO
053500000000     C                   Z-ADD     D00HHA        V1CHHA                         *ORA ARRIVO
053600000000     C                   MOVEL     D00SCA        V1CSCA                         *SCARICO
053700000000     C                   MOVEL     D00GGP        V1CGGP                         *GIORNO PARTENZA
053800000000     C                   Z-ADD     D00HHP        V1CHHP                         *ORA PARTENZA
053900000000     C                   MOVEL     D00CAR        V1CCAR                         *CARICO
054000000000     C                   Z-ADD     D00KMT        V1CKMT                         *CHILOMETRI
054100000000     C                   Z-ADD     D00TEM        V1CTEM                         *TEMPO
054200000000     C                   MOVEL     D00GFV        V1CGFV                         *APERTURA F.V.
054300000000     C* DECODIFICHE
054400000000     C                   SETOFF                                           28
054500000000     C                   Z-ADD     1             J
054600000000     C     V1CFIL        LOOKUP    ORG(J)                                 28
054700000000IF  1C     *IN28         IFEQ      *ON
054800000000     C                   MOVEL     DORG(J)       V1DFIL                         *DESCRIZIONE FILIALE
054900000000X   1C                   ELSE
055000000000     C                   MOVEL     *ALL'*'       V1DFIL                         *DESCRIZIONE FILIALE
055100000000E   1C                   ENDIF
055200000000     C*
055300000000     C                   ENDSR
055400000000     C*------------------------------------------------------------------------*
055500000000     C*  AZZVID - PULISCE TUTTI I CAMPI DEL VIDEO
055600000000     C*------------------------------------------------------------------------*
055700000000     C     AZZVID        BEGSR
055800000000     C*
055900000000     C                   Z-ADD     *ZEROS        V1CSEQ                         *SEQUENZA
056000000000     C                   MOVEL     *BLANKS       V1CTFM                         *TIPO FERMATA
056100000000     C                   Z-ADD     *ZEROS        V1CFIL                         *FILIALE FERMATA
056200000000     C                   MOVEL     *BLANKS       V1CGGA                         *GIORNO ARRIVO
056300000000     C                   Z-ADD     *ZEROS        V1CHHA                         *ORA ARRIVO
056400000000     C                   MOVEL     *BLANKS       V1CSCA                         *SCARICO
056500000000     C                   MOVEL     *BLANKS       V1CGGP                         *GIORNO PARTENZA
056600000000     C                   Z-ADD     *ZEROS        V1CHHP                         *ORA PARTENZA
056700000000     C                   MOVEL     *BLANKS       V1CCAR                         *CARICO
056800000000     C                   Z-ADD     *ZEROS        V1CKMT                         *CHILOMETRI
056900000000     C                   Z-ADD     *ZEROS        V1CTEM                         *TEMPO
057000000000     C                   MOVEL     *BLANKS       V1CGFV                         *APERTURA F.V.
057100000000     C* DECODIFICHE
057200000000     C                   MOVEL     *BLANKS       V1DFIL                         *DESCRIZIONE FILIALE
057300000000     C*
057400000000     C                   ENDSR
057500000000     C*------------------------------------------------------------------------*
057600000000     C* CARTAB - CARICA LE TABELLE
057700000000     C*------------------------------------------------------------------------*
057800000000     C     CARTAB        BEGSR
057900000000     C*--------------------
058000000000     C* FILIALI
058100000000     C*--------------------
058200000000     C                   Z-ADD     *ZEROS        J                 4 0
058300000000     C     *LOVAL        SETLL     AZORG01L
058400000000     C                   READ      AZORG01L                               99
058500000000DO  1C     *IN99         DOWEQ     *OFF
058600000000IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
058700960208     C                   MOVEL     ORGDE8        OG148
058800960208IF  3C     §OGLFV        IFNE      'N'                                          *IN GESTIONE FV
058900000000     C                   ADD       1             J
059000000000     C                   Z-ADD     ORGFIL        ORG(J)                         *CODICE
059100000000     C                   MOVEL     ORGDES        DORG(J)                        *DESCRIZIONE
059200960208E   3C                   ENDIF
059300960208E   2C                   ENDIF
059400000000     C                   READ      AZORG01L                               99
059500000000E   1C                   ENDDO
059600000000     C*
059700000000     C                   ENDSR
059800000000     C*------------------------------------------------------------------------*
059900000000     C* IMPVID - IMPOSTAZIONI VIDEO
060000000000     C*------------------------------------------------------------------------*
060100000000     C     IMPVID        BEGSR
060200000000     C*
060300000000     C* CALCOLA LA DATA CORRENTE
060400000000     C                   TIME                    WNUM14           14 0          *ORA (6)+ DATA (8)
060500000000     C                   MOVE      WNUM14        WNUM8             8 0          *DATA (8) IN G/M/AA
060600000000     C                   Z-ADD     WNUM8         G08DAT
060700000000     C                   Z-ADD     *ZEROS        G08INV
060800000000     C                   MOVEL     '0'           G08ERR
060900000000     C                   CALL      'XSRDA8'
061000000000     C                   PARM                    WLBDA8
061100000000     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
061200000000     C*
061300000000     C* CARICA L'ALFABETO
061400000000     C                   MOVEA     DALF          ALF
061500000000     C*
061600000000     C* 1°) DECODIFICA L'OPZIONE RICHIAMATA
061700000000     C* 2°) IMPOSTA LA PROTEZIONE DEI CAMPI -> 90=CAMPI CHIAVE / 91=ALTRI CAMPI
061800000000SL  1C                   SELECT
061900000000     C     D00OP1        WHENEQ    'F06'
062000000000     C*---                 MOVELDOPZ,1    VTDFUN           *INSERIMENTO
062100000000     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
062200000000     C     D00OP1        WHENEQ    'O02'
062300000000     C*---                 MOVELDOPZ,2    VTDFUN           *VARIAZIONE
062400000000     C*---                 SETON                     90    *CHIAVE PROTETTA
062500000000     C     D00OP1        WHENEQ    'O04'
062600000000     C*---                 MOVELDOPZ,3    VTDFUN           *CANCELLAZIONE
062700000000     C                   SETON                                        90        *PROTEZIONE COMPLETA
062800000000     C*---       D00OP1    WHEQ 'O07'
062900000000     C*---                 MOVELDOPZ,4    VTDFUN           *RIPRISTINO
063000000000     C*---                 SETON                     90    *PROTEZIONE COMPLETA
063100000000     C     D00OP1        WHENEQ    'O05'
063200000000     C*---                 MOVELDOPZ,5    VTDFUN           *VISUALIZZAZIONE
063300000000     C                   SETON                                        90        *PROTEZIONE COMPLETA
063400000000     C*---                 MOVELFUN,6     FUNRIG           *TOGLIE FUN F6=AGGIOR
063500000000     C*---                 MOVEL'N'       FUNASG
063600000000     C*---                 MOVEL'N'       FUNASI
063700000000     C*---                 MOVEL'N'       FUNAFG
063800000000     C*---                 MOVEL'N'       FUNAFG
063900000000     C*---                 MOVELFUNRIG    FUN,6
064000000000     C                   OTHER
064100000000E   1C                   ENDSL
064200000000     C* LEGGE IL TRAINO
064300000000     C                   Z-ADD     D00TRN        KLTTRN                         *TRAINO
064400000000     C                   Z-ADD     D00DDE        KLTDDE                         *DATA DECORRENZA
064500000000     C     KEYTLT        CHAIN     TNTLT01L                           99
064600000000     C*---
064700000000     C* CONTROLLA ESISTENZA E DISPONIBILITA' RECORD (NO SE INSERIMENTO)
064800000000SL  1C*---                 SELEC
064900000000     C*---       D00OP1    WHEQ 'F06'
065000000000     C*---                 OTHER
065100000000     C*---                 Z-ADDD00TRN    KLTTRN           *TRAINO
065200000000     C*---                 Z-ADDD00DDE    KLTDDE           *DATA DECORRENZA
065300000000     C*---       KEYTLT    CHAINTNTLT01L             9998
065400000000IF  2C*---       *IN99     IFEQ *ON                        *INSESISTENTE
065500000000     C*---                 MOVEL'1'       $ERR
065600000000     C*---                 MOVEL'1'       $FINE
065700000000     C*---                 MOVELMSG,25    D00MSG
065800000000X   2C*---                 ELSE
065900000000IF  3C*---       *IN98     IFEQ *ON                        *ALLOCATO
066000000000     C*---                 MOVEL'1'       $ERR
066100000000     C*---                 MOVEL'1'       $FINE
066200000000     C*---                 MOVELMSG,26    D00MSG
066300000000E   3C*---                 ENDIF
066400000000E   2C*---                 ENDIF
066500000000E   1C*---                 ENDSL
066600000000     C* RIEMPE IL VIDEO
066700000000IF  1C     $ERR          IFEQ      '0'
066800000000SL  2C                   SELECT
066900000000     C     D00OP1        WHENEQ    'F06'
067000000000     C                   EXSR      AZZVID                                       *AZZERA CAMPI VIDEO
067100000000     C                   OTHER
067200000000     C                   EXSR      RECVID                                       *RIEMPE IL VIDEO
067300000000IF  3C     D00OP1        IFNE      'O04'                                        *NO IN ANNULLAMENTO
067400000000     C*---                 EXSR DECODI                     *DECODIFICA CAMPI
067500000000E   3C                   ENDIF
067600000000E   2C                   ENDSL
067700000000E   1C                   ENDIF
067800000000     C*
067900000000     C                   ENDSR
068000000000     C*------------------------------------------------------------------------*
068100000000     C* *INZSR - IMPOSTAZIONI INIZIALI
068200000000     C*------------------------------------------------------------------------*
068300000000     C     *INZSR        BEGSR
068400000000     C*--------------------
068500000000     C* RICEVIMENTO PARAMETRI
068600000000     C*--------------------
068700000000     C     *ENTRY        PLIST
068800000000     C                   PARM                    KPJBA
068900000000     C                   MOVEL     KPJBU         DS00
069000000000     C*--------------------
069100000000     C* VARIABILI RIFERITE AL DATA BASE
069200000000     C*--------------------
069300000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
069400000000     C     *LIKE         DEFINE    TLTTRN        KLTTRN                         *TNTLT00F
069500000000     C     *LIKE         DEFINE    TLTDDE        KLTDDE
069600000000     C*--------------------
069700000000     C* VARIABILI NON RIFERITE AL DATA BASE
069800000000     C*--------------------
069900000000     C     *LIKE         DEFINE    D00OP0        WOP0                           *DS00
070000000000     C     *LIKE         DEFINE    D00OP1        WOP1
070100000000     C     *LIKE         DEFINE    D00F03        WF03
070200000000     C     *LIKE         DEFINE    D00F12        WF12
070300000000     C     *LIKE         DEFINE    D00ERR        WERR
070400000000     C     *LIKE         DEFINE    D00MSG        WMSG
070500000000     C     *LIKE         DEFINE    D00TRN        WTRN
070600000000     C     *LIKE         DEFINE    D00DDE        WDDE
070700000000     C*--------------------
070800000000     C* CHIAVI DI LETTURA
070900000000     C*--------------------
071000000000     C* LETTURA AZORG01L - COMPLETA
071100000000     C     KEYORG        KLIST
071200000000     C                   KFLD                    KORFIL                         *FILIALE
071300000000     C* LETTURA TNTLT01L - COMPLETA
071400000000     C     KEYTLT        KLIST
071500000000     C                   KFLD                    KLTTRN                         *NUMERO TRAINO
071600000000     C                   KFLD                    KLTDDE                         *DATA DECORRENZA
071700000000     C*
071800000000     C* 02 -> GESTIONE / 05 -> INTERROGAZIONE
071900000000     C                   MOVE      D00OP0        INTGES            2
072000000000IF  1C     INTGES        IFEQ      '02'
072100000000     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
072200000000X   1C                   ELSE
072300000000     C                   MOVEL     'I'           $TGES             1            *I=INTERROGAZIONE
072400000000E   1C                   ENDIF
072500000000     C*
072600000000     C* I FILES SONO STATI APERTI IN I/O PERCHE' COSI' VUOLE OVRDBF SHARE(*YES);
072700000000     C* SONO PERTANTO NECESSARIE DELLE VARIAZIONI/AGGIUNTE FITTIZIE AI FILES
072800000000IF  1C     *IN99         IFEQ      *ON
072900000000     C     *IN99         ANDEQ     *OFF
073000000000     C                   UPDATE    TNTLT000
073100000000     C                   WRITE     TNTLT000
073200000000E   1C                   ENDIF
073300000000     C*
073400000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
073500000000     C                   EXSR      INZSTD
073600000000     C*
073700000000     C* CARICA LE TABELLE
073800000000     C                   EXSR      CARTAB
073900000000     C*
074000000000     C* IMPOSTAZIONI VIDEO
074100000000     C                   EXSR      IMPVID
074200000000     C*
074300000000     C* EMETTE VIDEATA
074400000000     C* IL TEST SULL'ERRORE SERVE PER GESTIRE L'ALLOCAZIONE (NON IN QUESTO CASO)
074500000000IF  1C*---       $ERR      IFEQ '0'
074600000000     C                   EXSR      CARV1
074700000000E   1C*---                 ENDIF
074800000000     C*
074900000000     C                   ENDSR
075000000000     C*------------------------------------------------------------------------*
075100000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
075200000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
075300000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
075400000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
075500000000     C*    NO E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
075600000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
075700000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
075800000000     C*------------------------------------------------------------------------*
075900000000     C* INZSTD - ROUTINE INIZIALE STANDARD
076000000000     C*------------------------------------------------------------------------*
076100000000     C     INZSTD        BEGSR
076200000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
076300000000     C                   CALL      'XKEYS'
076400000000     C                   PARM                    DSKEYS
076500000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
076600000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
076700000000     C     XMSGPA        PLIST
076800000000     C                   PARM                    DSMSMS
076900000000     C                   PARM                    DSMSPG
077000000000     C                   PARM                    DSMSKE
077100000000     C                   PARM                    DSMSER
077200000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
077300000000     C                   MOVEL     'TNTL05R'     DSMSPG                         *NOME PGM CHIAMANTE
077400000000     C                   MOVEL     'TNTL05R'     VMCQUE                         *CODA MESSAGGI DESTI
077500000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
077600000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
077700000000     C* INIZIALIZZO FLAG CONTROLLO PGM
077800000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
077900000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
078000000000     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
078100000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
078200000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
078300000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
078400000000     C                   MOVEL     '0'           $TESVD            1            *1=RIEMETT.TEST.VIDE
078500000000     C* LANCIO ROUTINE PER VERIFICARE SE
078600000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
078700000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
078800020715
078900020715     c     *dtaara       define    §azute        azuteds
079000020715     c     *dtaara       define    §datiute      ddatiute
079100020715
079200020715     c                   in(E)     *dtaara
079300020715     c                   if        %Error or rsut = *blanks
079400020715     c                   clear                   tibs34ds
079500020715     c                   call      'TIBS34R'
079600020715     c                   parm                    Tibs34Ds
079700020715     c                   in        *dtaara
079800020715     c                   endif
079900000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
080000000000     C                   BITOFF    '01234567'    POSMI1            1
080100000000     C                   BITOFF    '01234567'    POSMI2            1
080200000000     C                   BITOFF    '01234567'    POSMA1            1
080300000000     C                   BITOFF    '01234567'    POSMA2            1
080400000000     C                   BITON     '34'          POSMI1            1            *18
080500000000     C                   BITON     '7'           POSMI2            1            *01
080600000000     C                   BITON     '34'          POSMA1            1            *18
080700000000     C                   BITON     '0'           POSMA2            1            *80
080800000000     C                   MOVE      POSMI2        POSMIN            2
080900000000     C                   MOVEL     POSMI1        POSMIN
081000000000     C                   MOVE      POSMA2        POSMAX            2
081100000000     C                   MOVEL     POSMA1        POSMAX
081200000000     C                   ENDSR
081300000000     C*------------------------------------------------------------------------*
081400000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
081500000000     C*------------------------------------------------------------------------*
081600000000     C     FUNSTD        BEGSR
081700000000     C                   SELECT
081800000000     C     KEY           WHENEQ    F3
081900000000     C                   EXSR      F03STD
082000000000     C     KEY           WHENEQ    F12
082100000000     C                   EXSR      F12STD
082200000000     C     KEY           WHENEQ    F24
082300000000     C                   EXSR      F24STD
082400000000     C                   OTHER
082500000000     C                   MOVEL     MSG(2)        $MSG
082600000000     C                   EXSR      CARMSG
082700000000     C                   ENDSL
082800000000     C                   ENDSR
082900000000     C*------------------------------------------------------------------------*
083000000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
083100000000     C*------------------------------------------------------------------------*
083200000000     C     F03STD        BEGSR
083300000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03
083400000000     C                   MOVEL     '1'           $F03
083500000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
083600000000     C                   MOVEL     '1'           $FINE
083700951212     C* PULISCE IL MESSAGGIO
083800951212     C                   MOVEL     *BLANKS       D00MSG
083900000000     C                   ENDSR
084000000000     C*------------------------------------------------------------------------*
084100000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
084200000000     C*------------------------------------------------------------------------*
084300000000     C     F12STD        BEGSR
084400000000     C* IMPOSTO FLAG DI PREMUTO TASTO F12
084500000000     C                   MOVEL     '1'           $F12
084600000000     C* IMPOSTO FLAG DI FIN PGM
084700000000     C                   MOVEL     '1'           $FINE
084800951212     C* PULISCE IL MESSAGGIO
084900951212     C                   MOVEL     *BLANKS       D00MSG
085000000000     C                   ENDSR
085100000000     C*------------------------------------------------------------------------*
085200000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
085300000000     C*------------------------------------------------------------------------*
085400000000     C     F24STD        BEGSR
085500000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
085600000000     C                   EXSR      OUTFUN
085700000000     C                   ENDSR
085800000000     C*------------------------------------------------------------------------*
085900000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
086000000000     C*          CODA MESSAGGI DEL PGM
086100000000     C*------------------------------------------------------------------------*
086200000000     C     CARMSG        BEGSR
086300000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
086400000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
086500000000     C     MSGULT        IFEQ      0
086600000000     C                   MOVEL     '1'           *IN40
086700000000     C                   WRITE     TL05DVK
086800000000     C                   MOVEL     '0'           *IN40
086900000000     C                   ENDIF
087000000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
087100000000     C                   MOVEL     $MSG          DSMSMS
087200000000     C                   CALL      'XMSG'        XMSGPA
087300000000     C     DSMSER        IFEQ      0
087400000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
087500000000     C                   ENDIF
087600000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
087700000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
087800000000     C                   WRITE     TL05DVM                                      *SFL MESS
087900000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
088000000000     C                   MOVEL     *BLANKS       $MSG
088100000000     C                   ENDSR
088200000000     C*------------------------------------------------------------------------*
088300000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
088400000000     C*------------------------------------------------------------------------*
088500000000     C     OUTMSG        BEGSR
088600000000     C* VISUALIZZAZIONE MESSAGGIO
088700000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
088800000000     C                   WRITE     TL05DVK                                      *CONTR MESS
088900000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
089000000000     C                   Z-ADD     *ZEROS        MSGULT
089100000000     C                   ENDSR
089200000000     C*------------------------------------------------------------------------*
089300000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
089400000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
089500000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
089600000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
089700000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
089800000000     C*------------------------------------------------------------------------*
089900000000     C     CARFUN        BEGSR
090000000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
090100000000     C     1             DO        31            I                 5 0          --- 1 -->
090200000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
090300000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
090400000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
090500020715     c                   If        DutLpo = 'S'
090600000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
090700000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
090800000000     C                   ELSE                                                   *INTERROG.
090900000000     C                   MOVEL     FUNASI        FAT(I)
091000000000     C                   ENDIF
091100000000     C                   ELSE                                                   <-- 2 --> *FILIALE
091200000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
091300000000     C                   MOVEL     FUNAFG        FAT(I)
091400000000     C                   ELSE                                                   *INTERROG.
091500000000     C                   MOVEL     FUNAFI        FAT(I)
091600000000     C                   ENDIF
091700000000     C                   ENDIF                                                  <-- 2 ---
091800000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
091900000000     C                   MOVEL     USER          WEDP              3            *PROFILO
092000000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
092100000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
092200000000     C                   ELSE
092300000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
092400000000     C                   ENDIF
092500000000     C                   ENDIF
092600000000     C                   ENDDO                                                  <-- 1 ---
092700000000     C                   ENDSR
092800000000     C*------------------------------------------------------------------------*
092900000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
093000000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
093100000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
093200000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
093300000000     C*------------------------------------------------------------------------*
093400000000     C     OUTFUN        BEGSR
093500000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
093600000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
093700000000     C                   ENDDO
093800000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
093900000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
094000000000     C     1             DO        31            I
094100000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
094200000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
094300000000     C                   ADD       1             FUNNAT
094400000000     C                   ENDIF
094500000000     C                   ENDDO
094600000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
094700000000     C     FUNNAT        IFGT      8
094800000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
094900000000     C                   ENDIF
095000000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
095100000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
095200000000     C                   Z-ADD     8             FUNVIS            5 0
095300000000     C     FAT(23)       IFEQ      'S'
095400000000     C                   SUB       1             FUNVIS
095500000000     C                   ENDIF
095600000000     C     FAT(24)       IFEQ      'S'
095700000000     C                   SUB       1             FUNVIS
095800000000     C                   ENDIF
095900000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
096000000000     C                   Z-ADD     *ZEROS        W                 5 0
096100000000     C                   ADD       1             FUNULT
096200000000     C     FUNULT        IFGT      31
096300000000     C                   Z-ADD     1             FUNULT
096400000000     C                   ENDIF
096500000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
096600000000     C     FUNULT        DO        31            I                              --- 1 -->
096700000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
096800000000     C     W             IFLT      FUNVIS                                       --- 2 -->
096900000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
097000000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
097100000000     C     FDE(I)        ANDNE     *BLANKS
097200000000     C     I             IFEQ      23                                           *TOLGO F23
097300000000     C     I             OREQ      24                                           *E F24
097400000000     C                   ELSE
097500000000     C                   ADD       1             W
097600000000     C                   MOVEL     FDE(I)        FUV(W)
097700000000     C                   ENDIF
097800000000     C                   ENDIF                                                  <-- 3 ---
097900000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
098000000000     C                   Z-ADD     I             FUNULT
098100000000     C                   ENDIF                                                  <-- 2 ---
098200000000     C                   ENDDO                                                  <-- 1 ---
098300000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
098400000000     C                   MOVEL     FUV(1)        VFD001
098500000000     C                   MOVEL     FUV(2)        VFD002
098600000000     C                   MOVEL     FUV(3)        VFD003
098700000000     C                   MOVEL     FUV(4)        VFD004
098800000000     C                   MOVEL     FUV(5)        VFD005
098900000000     C                   MOVEL     FUV(6)        VFD006
099000000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
099100000000     C     FAT(24)       ANDEQ     'N'
099200000000     C                   MOVEL     FUV(7)        VFD007
099300000000     C                   MOVEL     FUV(8)        VFD008
099400000000     C                   ELSE                                                   <-- 1 -->
099500000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
099600000000     C     FAT(24)       ANDEQ     'S'
099700000000     C                   MOVEL     FDE(23)       VFD007
099800000000     C                   MOVEL     FDE(24)       VFD008
099900000000     C                   ELSE                                                   <-- 2 -->
100000000000     C                   MOVEL     FUV(7)        VFD007
100100000000     C     FAT(23)       IFEQ      'S'
100200000000     C                   MOVEL     FDE(23)       VFD008
100300000000     C                   ELSE
100400000000     C                   MOVEL     FDE(24)       VFD008
100500000000     C                   ENDIF
100600000000     C                   ENDIF                                                  <-- 2 ---
100700000000     C                   ENDIF                                                  <-- 1 ---
100800000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
100900000000     C                   ENDSR
101000000000     C*------------------------------------------------------------------------*
101100000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
101200000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
101300000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
101400000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
101500000000     C*------------------------------------------------------------------------*
101600000000     C     CONFUN        BEGSR
101700000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
101800000000     C     1             DO        31            I                              --- 1 -->
101900000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
102000000000     C* CONTROLLO SE IL TASTO E' ATTIVO
102100000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
102200000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
102300000000     C                   EXSR      CARMSG
102400000000     C                   MOVEL     '1'           $ERR
102500000000     C                   ENDIF                                                  <-- 3 ---
102600000000     C                   ENDIF                                                  <-- 2 ---
102700000000     C                   ENDDO                                                  <-- 1 ---
102800000000     C                   ENDSR
102900000000** - MSG MESSAGGI
103000000000TNTL05R- Tasto funzionale non valido.                                          1
103100000000TNTL05R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
103200000000TNTL05R- Opzione non valida.                                                   3
103300000000TNTL05R- Opzione abilitata ma non gestita. Avvertire CED.                      4
103400000000TNTL05R- Inizio elenco.                                                        5
103500000000TNTL05R- Fine elenco.                                                          6
103600000000TNTL05R- 7
103700000000TNTL05R- 8
103800000000TNTL05R- 9
103900000000TNTL05R- 10
104000000000TNTL05R- Sequenza fermata obbligatoria                                        11
104100000000TNTL05R- Impossibile inserire fermate di ritorno per traino non chiuso        12
104200000000TNTL05R- Giorno apertura foglio viaggio errato. Inserire A,B,C,...            13
104300000000TNTL05R- Inserire il giorno di apertura F.V. solo se la fermata è di carico   14
104400000000TNTL05R- 15
104500000000TNTL05R- Tipo fermata errato. Inserire "A"=Andata o "R"=Ritorno               16
104600000000TNTL05R- Filiale errata                                                       17
104700000000TNTL05R- Progressivo giorno arrivo errato. Inserire A,B,C,...                 18
104800000000TNTL05R- Progressivo giorno partenza errato. Inserire A,B,C,...               19
104900000000TNTL05R- Ora arrivo errata (hh:mm)                                            20
105000000000TNTL05R- Ora partenza errata (hh:mm)                                          21
105100000000TNTL05R- Inserire "S" solo se la merce viene scaricata                        22
105200000000TNTL05R- Inserire "S" solo se la merce viene caricata                         23
105300951212TNTL05R- 24
105400030716TNTL05R- 25                                                                   25
105500030716TNTL05R- 26                                                                   26
105600951212TNTL05R- Se fermata di scarico inserire anche gg e ora arrivo, e viceversa    27
105700000000TNTL05R- Se è inserito il gg arrivo bisogna inserire anche l'ora e viceversa  28
105800951212TNTL05R- Se fermata di carico inserire anche gg e ora partenza, e viceversa   29
105900000000TNTL05R- Se è inserito il gg parenza bisogna inserire anche l'ora e viceversa 30
106000000000** - FUN  FUNZIONI        SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
106100000000F1    -                   -    NN     NN
106200000000F2    -                   -    NN     NN
106300000000F3    -F3=Fine            -    NN     NN
106400000000F4    -                   -    NN     NN
106500000000F5    -                   -    NN     NN
106600000000F6    -                   -    NN     NN
106700000000F7    -                   -    NN     NN
106800000000F8    -                   -    NN     NN
106900000000F9    -                   -    NN     NN
107000000000F10   -                   -    NN     NN
107100000000F11   -                   -    NN     NN
107200000000F12   -F12=Ritorno        -    SS     SS
107300000000F13   -                   -    NN     NN
107400000000F14   -                   -    NN     NN
107500000000F15   -                   -    NN     NN
107600000000F16   -                   -    NN     NN
107700000000F17   -                   -    NN     NN
107800000000F18   -                   -    NN     NN
107900000000F19   -                   -    NN     NN
108000000000F20   -                   -    NN     NN
108100000000F21   -                   -    NN     NN
108200000000F22   -                   -    NN     NN
108300000000F23   -                   -    NN     NN
108400000000F24   -F24=Altri tasti    -    NN     NN
108500000000ENTER -                   -    SS     SS
108600000000ROLLUP-                   -    NN     NN
108700000000ROLLDN-                   -    NN     NN
108800000000HELP  -                   -    NN     NN
108900000000PRINT -                   -    NN     NN
109000000000BSPACE-                   -    NN     NN
109100000000CLEAR -                   -    NN     NN
109200000000** -DALF- ALFABETO
109300000000ABCDEFGHILMNOPQRSTUVZ
