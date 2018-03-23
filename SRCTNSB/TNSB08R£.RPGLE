000100000000     H*------------------------------------------------------------------------*
000200000000     H*  Nome programma:  TNSB08R
000300000000     H*  Descrizione   :  INTERROGAZIONE COMPETENZE DI CONTO ECONOMICO
000400000000     H*                   SUBFILE DI VISUALIZZAZIONE
000500000000     H*  Autore        :  RAFFAELLA (RN)
000600000000     H*  Data creazione:  GIUGNO 1995
000700000000     H*------------------------------------------------------------------------*
000800000000     H DECEDIT('0,') DATEDIT(*DMY.)
000900000000     F*------------------------------------------------------------------------*
001000000000     F* DATA BASE
001100000000     F*------------------------------------------------------------------------*
001200000000     FTNSB08D   CF   E             WORKSTN INFDS(DEVDS1)
001300000000     F                                     SFILE(SB08DVS:SFLULT)
001400000000     F                                     SFILE(SB08DVM:MSGULT)
001500000000     FECCET30C  IF   E           K DISK
001600000000     FECCED30C  IF   E           K DISK
001700000000     FECECT01L  IF   E           K DISK
001800000000     D*------------------------------------------------------------------------*
001900000000     D* SCHIERE
002000000000     D*------------------------------------------------------------------------*
002100000000     D*-------------------
002200000000     D* MESSAGGI, OPZIONI E FUNZIONI
002300000000     D*-------------------
002400000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             *MESSAGGI
002500000000     D OPZ             S             40    DIM(24) CTDATA PERRCD(1)             *RIGA DESCR. E ABIL.
002600000000     D FUN2            S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO SUBFILE
002700000000     D FUN             S             40    DIM(31)                              *RIGA DESCR. E ABIL.
002800000000     D*-------------------
002900000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
003000000000     D*-------------------
003100000000     D FFU             S              6    DIM(31)                              *FUNZIONE
003200000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
003300000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
003400000000     D*-------------------
003500000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA OPZIONI
003600000000     D*-------------------
003700000000     D OPD             S              2    DIM(24)                              *OPZIONE (ALLIN. DX)
003800000000     D OPS             S              2    DIM(24)                              *OPZIONE (ALLIN. SX)
003900000000     D ODE             S             19    DIM(24)                              *DESCRIZIONE OPZIONE
004000000000     D OAT             S              1    DIM(24)                              *OPZIONE ATTIVA S/N
004100000000     D*-------------------
004200000000     D* DESCRIZIONI A VIDEO
004300000000     D*-------------------
004400000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
004500000000     D OPV             S             19    DIM(8)                               *OPZIONI A VIDEO
004600000000     D*-------------------
004700000000     D* TASTI FUNZIONALI PREMUTI
004800000000     D*-------------------
004900000000     D KEYS            S              1    DIM(40)
005000000000     D*-------------------
005100000000     D* COMPETENZE DI C/E
005200000000     D*-------------------
005300000000     D CMP             S              3  0 DIM(100)                             *CODICE
005400090909     D IMP             S             13  5 DIM(100)                             *IMPORTO
005500000000     D CMD             S             30    DIM(100)                             *DESCRIZIONE
005600000000     D*------------------------------------------------------------------------*
005700000000     D* INPUT
005800000000     D*------------------------------------------------------------------------*
005900000000     D*---------------------
006000000000     D* RIDEFINIZIONE SCHIERA -OPD- (OPZIONI)
006100000000     D*---------------------
006200000000     D DSOPZ         E DS
006300000000     D  OPZRIG                 1     40
006400000000     D*---------------------
006500000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
006600000000     D*---------------------
006700000000     D DSFUN         E DS
006800000000     D  FUNRIG                 1     40
006900000000     D*---------------------
007000000000     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
007100000000     D*---------------------
007200000000     D DSMSG         E DS
007300000000     D*---------------------
007400000000     D* DS PER GESTIONE TASTI FUNZIONALI
007500000000     D*---------------------
007600000000     D DSKEYS        E DS
007700000000     D  KEYRIG                 1     40
007800000000     D*---------------------
007900000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
008000000000     D*---------------------
008100000000     D                SDS
008200000000     D  USER                 254    263
008300000000     D*---------------------
008400000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
008500000000     D*---------------------
008600000000     D DEVDS1          DS
008700000000     D  KEY                  369    369
008800000000     D  CURSOR               370    371
008900000000     D  FRTSFL               378    379B 0
009000000000     D*---------------------
009100000000     D* ARCHITETTURA
009200000000     D*---------------------
009300000000     D KPJBA         E DS
009400000000     D*---------------------
009500000000     D* DS PROCEDURA
009600000000     D*---------------------
009700000000     D DSSB01        E DS                  EXTNAME(TNSB01DS)
009800000000     D*---------------------
009900020920     D* DS REPERIMENTO DATI UTENTE
010000020920     D*---------------------
010100020920     D TIBS34DS      E DS                                                       *Profili utente
010200020920     D DDATIUTE      E DS                                                       *Dati utente
010300020920     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
010400000000     D*------------------
010500000000     D* CALL XSRDA8 - CONTROLLO DATE
010600000000     D*------------------
010700000000     D WLBDA8          DS                  INZ
010800000000     D  G08DAT                 1      8  0
010900000000     D  G08INV                 9     16  0
011000000000     D  G08ERR                17     17
011100000000     D  G08TGI                18     22  0
011200000000     D*------------------
011300000000     D* DATA SPEDIZIONE
011400000000     D*------------------
011500000000     D                 DS
011600000000     D  TASAAS                 1      4  0
011700000000     D  TASMGS                 5      8  0
011800000000     D  TASDSP                 1      8  0
011900000000     D*------------------
012000000000     D* DESCRIZIONE TOTALE COMPETENZE
012100000000     D*------------------
012200000000     D TODIMP          C                   CONST('TOTALE COMPETENZE')
012300000000     I*------------------
012400000000     I* DESCRIZIONE OPERAZIONE IN TESTATA
012500000000     I*------------------
012600000000     I**********    '*  GESTIONE  *'      C         DESGES
012700000000     I**********    'INTERROGAZIONE'      C         DESINT
012800000000     C*------------------------------------------------------------------------*
012900000000     C* MAIN LINE
013000000000     C*------------------------------------------------------------------------*
013100000000     C*
013200000000     C* CICLO FINO A CHE:
013300000000DO  1C     $FINE         DOUEQ     '1'
013400000000     C*
013500000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
013600000000     C*
013700000000     C* CONTROLLA GESTIONE VIDEO
013800000000IF  2C     $GES          IFEQ      'VS'
013900000000     C                   EXSR      GESVS                                        *SUBFILE
014000000000E   2C                   ENDIF
014100000000     C*
014200000000E   1C                   ENDDO
014300000000     C*
014400000000     C* SE E' STATO PREMUTO F03 IMPOSTO FLAG DELLA DS DI PROCEDURA
014500000000IF  1C     $F03          IFEQ      '1'
014600000000     C                   MOVEL     '1'           D01F03
014700000000E   1C                   ENDIF
014800000000     C*
014900000000     C* SE E' STATO PREMUTO F12 IMPOSTO FLAG DELLA DS DI PROCEDURA
015000000000IF  1C     $F12          IFEQ      '1'
015100000000     C                   MOVEL     '1'           D01F12
015200000000E   1C                   ENDIF
015300000000     C* FINE PGM
015400000000     C                   MOVEL     DSSB01        KPJBU
015500000000     C                   SETON                                        LR
015600000000     C*------------------------------------------------------------------------*
015700000000     C* CARVS  - CARICA GESTIONE VIDEO 'SUBFILE'
015800000000     C*------------------------------------------------------------------------*
015900000000     C     CARVS         BEGSR
016000000000     C*
016100000000     C                   MOVEL     'VS'          $GES                           *GESTIONE ATTUALE
016200000000     C*
016300000000     C                   EXSR      CAROPZ                                       *CARICA OPZIONI
016400000000     C                   EXSR      OUTOPZ                                       *EMETTE OPZIONI
016500000000     C                   MOVEA     FUN2          FUN                            *SCHIERA FUNZ.VIDEO
016600000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
016700000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
016800000000     C*
016900000000     C                   WRITE     SB08DVT                                      *TESTATA
017000000000     C                   WRITE     SB08DVO                                      *OPZIONI
017100000000     C                   WRITE     SB08DVF                                      *FUNZIONI
017200000000     C*
017300000000     C                   EXSR      POSSFL                                       *CARICA SFL
017400000000     C*
017500000000     C                   ENDSR
017600000000     C*------------------------------------------------------------------------*
017700000000     C* GESVS  - GESTIONE SUBFILE
017800000000     C*------------------------------------------------------------------------*
017900000000     C     GESVS         BEGSR
018000000000     C*
018100000000IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
018200000000     C                   WRITE     SB08DVT                                      *TESTATA
018300000000     C                   WRITE     SB08DVO                                      *OPZIONI
018400000000     C                   WRITE     SB08DVF                                      *FUNZIONI
018500000000E   1C                   ENDIF
018600000000     C*
018700000000     C                   EXSR      OUTSFL                                       *EMETTE SUBFILE
018800000000     C*
018900000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTO
019000000000     C*
019100000000IF  1C     $ERR          IFEQ      '0'
019200000000SL  2C                   SELECT
019300000000     C     KEY           WHENEQ    F12
019400000000     C                   EXSR      F12GES
019500000000     C                   OTHER
019600000000     C                   EXSR      FUNSFL                                       *FUNZIONI STND SUBFI
019700000000E   2C                   ENDSL
019800000000E   1C                   ENDIF
019900000000     C*
020000000000     C                   ENDSR
020100000000     C*------------------------------------------------------------------------*
020200000000     C* F12GES -GEST.TASTO FUNZ. F12 ===> PRECEDENTE
020300000000     C*------------------------------------------------------------------------*
020400000000     C     F12GES        BEGSR
020500000000     C*
020600000000     C                   MOVEL     *BLANKS       D01OP1
020700000000     C                   MOVEL     '1'           $FINE
020800000000     C*
020900000000     C                   ENDSR
021000000000     C*------------------------------------------------------------------------*
021100000000     C*  GESOPZ - GESTIONE OPZIONI
021200000000     C*------------------------------------------------------------------------*
021300000000     C     GESOPZ        BEGSR
021400000000     C*
021500000000SL  1C                   SELECT
021600000000     C*********  VSCOPZ    WHEQ ' 2'
021700000000     C*********            EXSR O02GES                     *MODIFICA
021800000000     C                   OTHER
021900000000     C                   MOVEL     MSG(4)        $MSG                           *OPZ.ABIL.MA NON GES
022000000000     C                   EXSR      CARMSG                                       *CARICA SFL MESSAGGI
022100000000E   1C                   ENDSL
022200000000     C*
022300000000     C                   ENDSR
022400000000     C*------------------------------------------------------------------------*
022500000000     C*  O02GES - GESTIONE OPZIONE 2=MODIFICA
022600000000     C*------------------------------------------------------------------------*
022700000000     C*********  O02GES    BEGSR
022800000000     C*
022900000000     C*********            ENDSR
023000000000     C*------------------------------------------------------------------------*
023100000000     C* NEWPOS - CONTROLLO DELLA RICHIESTA DI NUOVO POSIZIONAMENTO
023200000000     C*------------------------------------------------------------------------*
023300000000     C     NEWPOS        BEGSR
023400000000     C*
023500000000     C* SWPOSI 0 = NESSUNA RICHIESTA DI POSIZIONAMENTO
023600000000     C*        1 = RICHIESTO POSIZIONAMENTO
023700000000     C                   Z-ADD     0             SWPOSI            7 0
023800000000     C*
023900000000     C* SWMODI 0 = NESSUN AGGIORNAMENTO AFFETTUATO DALL'UTENTE CON L'OPZIONE 2,
024000000000     C*            NON OCCORRE RICARICARE IL SUBFILE
024100000000     C*        1 = AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
024200000000     C                   Z-ADD     0             SWMODI            1 0
024300000000     C*
024400000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO
024500000000     C*----------------
024600000000     C* N.B.:  PER COMODITA' HO LASCIATO L'IMPOSTAZIONE CLASSICA DEL
024700000000     C*        PROGRAMMA, CIOE' CON I CAMPI POSIZIONAMENTO DI I/O NEL
024800000000     C*        SFLCONTROL (QUINDI CON I CAMPI "DEPOSITO").
024900000000     C*        ORA TALI CAMPI NON VI SONO A VIDEO MA METTENDOLI,
025000000000     C*        IL PGM E' GIA' PREDISPOSTO.
025100000000     C*----------------
025200000000IF  1C     VCCAAS        IFNE      DEPAAS                                       *ANNO
025300000000     C*
025400000000     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI
025500000000     C                   EXSR      CHKPAR
025600000000     C*
025700000000     C* SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
025800000000IF  2C     $ERR          IFEQ      '0'
025900000000     C                   Z-ADD     1             SWPOSI                         *RICHIESTO POSIZ.TO
026000000000     C                   Z-ADD     VCCAAS        DEPAAS                         *SALVATO NEW POSIZ.T
026100000000E   2C                   ENDIF
026200000000E   1C                   ENDIF
026300000000     C*
026400000000     C                   ENDSR
026500000000     C*------------------------------------------------------------------------*
026600000000     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
026700000000     C*------------------------------------------------------------------------*
026800000000     C     CHKPAR        BEGSR
026900000000     C*
027000000000     C* REIMPOSTO INDICATORI DI ERRORE
027100000000     C                   SETOFF                                         81
027200000000     C*
027300000000     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL CURSORE
027400000000     C* SUL CAMPO DEL CONTROL
027500000000IF  1C     $ERR          IFEQ      '1'
027600000000     C                   MOVEL     '0'           *IN43                          *NON EMETTO SFL
027700000000X   1C                   ELSE
027800000000     C                   MOVEL     '1'           *IN43                          *EMETTO SFL
027900000000E   1C                   ENDIF
028000000000     C*
028100000000     C                   ENDSR
028200000000     C*------------------------------------------------------------------------*
028300000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
028400000000     C*------------------------------------------------------------------------*
028500000000     C     SETFIL        BEGSR
028600000000     C*
028700000000     C                   MOVEL     '1'           *IN43                          *EMETTO SFILE
028800000000     C*
028900000000     C* POSIZIONAMENTO ARCHIVIO (SCHIERE PRECEDENTEMENTE CARICATE)
029000000000     C                   Z-ADD     *ZEROS        J
029100000000     C                   EXSR      LETFIL
029200000000     C*
029300000000     C*---                 Z-ADD1         J
029400000000IF  1C*---       IMP,J     IFGT *ZEROS
029500000000     C*---                 EXSR LETFIL
029600000000X   1C*---                 ELSE                            *FINE FILE
029700000000     C*---                 Z-ADD1         SFLEOF
029800000000E   1C*---                 ENDIF
029900000000     C*
030000000000     C                   ENDSR
030100000000     C*------------------------------------------------------------------------*
030200000000     C* LETFIL - LEGGE RECORD SUCESSIVO
030300000000     C*------------------------------------------------------------------------*
030400000000     C     LETFIL        BEGSR
030500000000     C*
030600000000     C                   ADD       1             J                              *INDICE SCHIERE VIDE
030700000000     C*
030800000000     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
030900000000IF  1C*---       J         IFGT 100                        LUNGHEZZA MAX SCHIERE
031000000000     C     IMP(J)        IFEQ      *ZEROS                                       *SCHIERA VUOTA
031100000000     C                   Z-ADD     1             SFLEOF
031200000000E   1C                   ENDIF
031300000000     C*
031400000000     C                   ENDSR
031500000000     C*------------------------------------------------------------------------*
031600000000     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO
031700000000     C*------------------------------------------------------------------------*
031800000000     C     IRESFL        BEGSR
031900000000     C*
032000000000     C                   Z-ADD     CMP(J)        VSCCMP
032100000000     C                   MOVEL     CMD(J)        VSCCMD
032200000000     C                   Z-ADD     IMP(J)        VSCIMP
032300000000     C*
032400000000     C                   ENDSR
032500000000     C*------------------------------------------------------------------------*
032600000000     C* CARCMP - CARICA TUTTE LE COMPETENZE DELLA BOLLA IN SCHIERA
032700000000     C*------------------------------------------------------------------------*
032800000000     C     CARCMP        BEGSR
032900000000     C*
033000000000     C     *LIKE         DEFINE    CEDIMP        TOTIMP
033100000000     C                   Z-ADD     *ZEROS        J                 3 0
033200000000     C*
033300000000     C* LEGGE LE COMPETENZE FISSE
033400000000     C                   Z-ADD     VCCAAS        KCTAAS                         *ANNO
033500000000     C                   Z-ADD     VCCLNP        KCTLNP                         *LINEA DI PARTENZA
033600000000     C                   Z-ADD     VCCNRS        KCTNRS                         *SERIE
033700000000     C                   Z-ADD     VCCNSP        KCTNSP                         *SPEDIZIONE
033800000000     C                   MOVEL     VCCTBL        KCTTBL                         *TIPO BOLLA
033900000000     C     KEYCET        CHAIN     ECCET30C                           99
034000000000IF  1C     *IN99         IFEQ      *OFF                                         *TROVATE
034100000000     C*
034200000000     C* CARICA LE COMPETENZE FISSE
034300000000IF  2C     CETPAR        IFGT      *ZEROS
034400000000     C                   ADD       1             J
034500000000     C                   Z-ADD     1             CMP(J)                         *CODICE  COMPETENZA
034600000000     C                   Z-ADD     CETPAR        IMP(J)                         *IMPORTO COMPETENZA
034700000000E   2C                   ENDIF
034800000000     C*
034900000000IF  2C     CETPAM        IFGT      *ZEROS
035000000000     C                   ADD       1             J
035100000000     C                   Z-ADD     2             CMP(J)                         *CODICE  COMPETENZA
035200000000     C                   Z-ADD     CETPAM        IMP(J)                         *IMPORTO COMPETENZA
035300000000E   2C                   ENDIF
035400000000     C*
035500000000IF  2C     CETPAU        IFGT      *ZEROS
035600000000     C                   ADD       1             J
035700000000     C                   Z-ADD     3             CMP(J)                         *CODICE  COMPETENZA
035800000000     C                   Z-ADD     CETPAU        IMP(J)                         *IMPORTO COMPETENZA
035900000000E   2C                   ENDIF
036000000000     C*
036100000000IF  2C     CETPAV        IFGT      *ZEROS
036200000000     C                   ADD       1             J
036300000000     C                   Z-ADD     4             CMP(J)                         *CODICE  COMPETENZA
036400000000     C                   Z-ADD     CETPAV        IMP(J)                         *IMPORTO COMPETENZA
036500000000E   2C                   ENDIF
036600000000     C*
036700000000IF  2C     CETARD        IFGT      *ZEROS
036800000000     C                   ADD       1             J
036900000000     C                   Z-ADD     5             CMP(J)                         *CODICE  COMPETENZA
037000000000     C                   Z-ADD     CETARD        IMP(J)                         *IMPORTO COMPETENZA
037100000000E   2C                   ENDIF
037200000000     C*
037300000000IF  2C     CETARM        IFGT      *ZEROS
037400000000     C                   ADD       1             J
037500000000     C                   Z-ADD     6             CMP(J)                         *CODICE  COMPETENZA
037600000000     C                   Z-ADD     CETARM        IMP(J)                         *IMPORTO COMPETENZA
037700000000E   2C                   ENDIF
037800000000     C*
037900000000IF  2C     CETARU        IFGT      *ZEROS
038000000000     C                   ADD       1             J
038100000000     C                   Z-ADD     7             CMP(J)                         *CODICE  COMPETENZA
038200000000     C                   Z-ADD     CETARU        IMP(J)                         *IMPORTO COMPETENZA
038300000000E   2C                   ENDIF
038400000000     C*
038500000000IF  2C     CETARV        IFGT      *ZEROS
038600000000     C                   ADD       1             J
038700000000     C                   Z-ADD     8             CMP(J)                         *CODICE  COMPETENZA
038800000000     C                   Z-ADD     CETARV        IMP(J)                         *IMPORTO COMPETENZA
038900000000E   2C                   ENDIF
039000000000     C*
039100000000IF  2C     CETTRL        IFGT      *ZEROS
039200000000     C                   ADD       1             J
039300000000     C                   Z-ADD     13            CMP(J)                         *CODICE  COMPETENZA
039400000000     C                   Z-ADD     CETTRL        IMP(J)                         *IMPORTO COMPETENZA
039500000000E   2C                   ENDIF
039600000000     C*
039700000000IF  2C     CETPAG        IFGT      *ZEROS
039800000000     C                   ADD       1             J
039900000000     C                   Z-ADD     29            CMP(J)                         *CODICE  COMPETENZA
040000000000     C                   Z-ADD     CETPAG        IMP(J)                         *IMPORTO COMPETENZA
040100000000E   2C                   ENDIF
040200000000     C*
040300000000IF  2C     CETSEA        IFGT      *ZEROS
040400000000     C                   ADD       1             J
040500000000     C                   Z-ADD     26            CMP(J)                         *CODICE  COMPETENZA
040600000000     C                   Z-ADD     CETSEA        IMP(J)                         *IMPORTO COMPETENZA
040700000000E   2C                   ENDIF
040800000000     C*
040900000000IF  2C     CETSEC        IFGT      *ZEROS
041000000000     C                   ADD       1             J
041100000000     C                   Z-ADD     25            CMP(J)                         *CODICE  COMPETENZA
041200000000     C                   Z-ADD     CETSEC        IMP(J)                         *IMPORTO COMPETENZA
041300000000E   2C                   ENDIF
041400000000     C*
041500000000IF  2C     CETRCV        IFGT      *ZEROS
041600000000     C                   ADD       1             J
041700000000     C                   Z-ADD     27            CMP(J)                         *CODICE  COMPETENZA
041800000000     C                   Z-ADD     CETRCV        IMP(J)                         *IMPORTO COMPETENZA
041900000000E   2C                   ENDIF
042000000000     C*
042100000000IF  2C     CETMDE        IFGT      *ZEROS
042200000000     C                   ADD       1             J
042300000000     C                   Z-ADD     10            CMP(J)                         *CODICE  COMPETENZA
042400000000     C                   Z-ADD     CETMDE        IMP(J)                         *IMPORTO COMPETENZA
042500000000E   2C                   ENDIF
042600000000     C*
042700000000IF  2C     CETTDE        IFGT      *ZEROS
042800000000     C                   ADD       1             J
042900000000     C                   Z-ADD     15            CMP(J)                         *CODICE  COMPETENZA
043000000000     C                   Z-ADD     CETTDE        IMP(J)                         *IMPORTO COMPETENZA
043100000000E   2C                   ENDIF
043200000000     C*
043300000000IF  2C     CETMAF        IFGT      *ZEROS
043400000000     C                   ADD       1             J
043500000000     C                   Z-ADD     9             CMP(J)                         *CODICE  COMPETENZA
043600000000     C                   Z-ADD     CETMAF        IMP(J)                         *IMPORTO COMPETENZA
043700000000E   2C                   ENDIF
043800000000     C*
043900000000IF  2C     CETTAF        IFGT      *ZEROS
044000000000     C                   ADD       1             J
044100000000     C                   Z-ADD     14            CMP(J)                         *CODICE  COMPETENZA
044200000000     C                   Z-ADD     CETTAF        IMP(J)                         *IMPORTO COMPETENZA
044300000000E   2C                   ENDIF
044400010829     C*
044500010829IF  2C     CETSES        IFGT      *ZEROS
044600010829     C                   ADD       1             J
044700010829     C                   Z-ADD     33            CMP(J)                         *CODICE  COMPETENZA
044800010829     C                   Z-ADD     CETSES        IMP(J)                         *IMPORTO COMPETENZA
044900010829E   2C                   ENDIF
045000000000     C*
045100000000     C* LEGGE LE COMPETENZE VARIABILI
045200000000IF  2C     CETDET        IFEQ      'S'                                          *SE CI SONO
045300000000     C                   Z-ADD     VCCAAS        KCDAAS                         *ANNO
045400000000     C                   Z-ADD     VCCLNP        KCDLNP                         *LINEA DI PARTENZA
045500000000     C                   Z-ADD     VCCNRS        KCDNRS                         *SERIE
045600000000     C                   Z-ADD     VCCNSP        KCDNSP                         *SPEDIZIONE
045700000000     C                   MOVEL     VCCTBL        KCDTBL                         *TIPO BOLLA
045800000000     C     KEYCED        SETLL     ECCED30C
045900000000     C     KEYCED        READE     ECCED30C                               99
046000000000DO  3C     *IN99         DOWEQ     *OFF
046100000000     C*
046200000000     C* CARICA LE COMPETENZE VARIABILI
046300000000IF  4C     CEDIMP        IFGT      *ZEROS
046400000000     C                   ADD       1             J
046500000000     C                   Z-ADD     CEDCMP        CMP(J)                         *CODICE  COMPETENZA
046600000000     C                   Z-ADD     CEDIMP        IMP(J)                         *IMPORTO COMPETENZA
046700000000E   4C                   ENDIF
046800000000     C     KEYCED        READE     ECCED30C                               99
046900000000E   3C                   ENDDO
047000000000E   2C                   ENDIF
047100000000     C*
047200000000     C* CALCOLA IL TOTALE COMPETENZE
047300000000     C                   XFOOT     IMP           TOTIMP
047400000000     C                   ADD       1             J
047500000000     C                   Z-ADD     *ZEROS        CMP(J)                         *CODICE  COMPETENZA
047600000000     C                   Z-ADD     TOTIMP        IMP(J)                         *IMPORTO COMPETENZA
047700000000     C                   MOVEL     TODIMP        CMD(J)                         *DESCRIZ.COMPETENZA
047800000000E   1C                   ENDIF
047900000000     C*
048000000000     C* DECODIFICA LE COMPETENZE (NON L'ULTIMO ELEMENTO CHE E' IL TOTALE)
048100000000DO  1C     1             DO        100           J
048200000000IF  2C     IMP(J)        IFGT      *ZEROS
048300000000     C     CMP(J)        ANDGT     *ZEROS
048400000000     C                   Z-ADD     CETNBC        WNBC
048500000000     C                   Z-ADD     CMP(J)        WCMP
048600000000     C                   MOVEL     *BLANKS       WCMD
048700000000     C                   EXSR      CHKCMP
048800000000     C                   MOVEL     WCMD          CMD(J)
048900000000E   2C                   ENDIF
049000000000E   1C                   ENDDO
049100000000     C*
049200000000     C                   ENDSR
049300000000     C*------------------------------------------------------------------------*
049400000000     C* CHKCMP - DECODIFICA LE COMPETENZE
049500000000     C*------------------------------------------------------------------------*
049600000000     C     CHKCMP        BEGSR
049700000000     C*
049800000000     C     *LIKE         DEFINE    ECTNBC        WNBC
049900000000     C     *LIKE         DEFINE    ECTCMP        WCMP
050000000000     C     *LIKE         DEFINE    ECTCMD        WCMD
050100000000     C*
050200000000     C* LEGGE, DEL BLOCCO, L'ULTIMA COMPETENZA IN DECORRENZA
050300000000     C                   Z-ADD     WNBC          KETNBC                         *NUMERO BLOCCO
050400000000     C                   Z-ADD     WCMP          KETCMP                         *CODICE COMPETENZA
050500000000     C                   Z-ADD     99999999      KETDDC                         *DATA DECORRENZA
050600000000     C     KEYECT        SETLL     ECECT01L
050700000000     C     KE2ECT        READPE    ECECT01L                               99
050800000000IF  1C     *IN99         IFEQ      *OFF
050900000000     C                   MOVEL     ECTCMD        WCMD
051000000000E   1C                   ENDIF
051100000000     C*
051200000000     C                   ENDSR
051300000000     C*------------------------------------------------------------------------*
051400000000     C* LANPGM - LANCIA UN PROGRAMMA
051500000000     C*------------------------------------------------------------------------*
051600000000     C*---       LANPGM    BEGSR
051700000000     C*--------------------
051800000000     C* PARAMETRI INFORMATIVI
051900000000     C*--------------------
052000000000     C*---                 MOVELWOP0      D01OP0           *LIVELLO PROCEDURA
052100000000     C*---                 MOVELWOP1      D01OP1           *LIVELLO PROGRAMMA
052200000000     C*---                 MOVEL'0'       D01F03           *USCITA PGM
052300000000     C*---                 MOVEL'0'       D01F12           *USCITA PGM
052400000000     C*---                 MOVEL'0'       D01ERR           *ERRORE
052500000000     C*---                 MOVEL*BLANKS   D01MSG           *MESSAGGIO ERRORE
052600000000     C*--------------------
052700000000     C* PARAMETRI
052800000000     C*--------------------
052900000000     C*---                 Z-ADDWAAS      D01AAS           *ANNO
053000000000     C*---                 Z-ADDWLNP      D01LNP           *LNP
053100000000     C*---                 Z-ADDWNRS      D01NRS           *SERIE
053200000000     C*---                 Z-ADDWNSP      D01NSP           *SPEDIZIONE
053300000000     C*---                 MOVELWTBL      D01TBL           *TIPO BOLLA
053400000000     C*---                 Z-ADDWDSP      D01DSP           *DATA SPEDIZIONE GMA
053500000000     C*---                 Z-ADDWDTV      D01DTV           *DATA VARIAZIONE AMG
053600000000     C*---                 Z-ADDWORV      D01ORV           *ORA
053700000000     C*---                 MOVELWPRU      D01PRU           *UTENTE
053800000000     C* LANCIO
053900000000     C*---                 MOVELDSSB01    KPJBU
054000000000     C*---                 CALL WPGM                   99
054100000000     C*---                 PARM           KPJBA
054200000000     C*---                 MOVELKPJBU     DSSB01
054300000000     C*---
054400000000     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
054500000000IF  1C*---       D01F03    IFEQ '1'
054600000000     C*---                 MOVEL'1'       $F03
054700000000     C*---                 MOVEL'1'       $ERR
054800000000     C*---                 MOVEL'1'       $FINE
054900000000     C*---
055000000000     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
055100000000X   1C*---                 ELSE
055200000000     C*---                 MOVELD01MSG    $MSG
055300000000     C*---                 EXSR CARMSG
055400000000E   1C*---                 ENDIF
055500000000     C*---
055600000000     C* PREMUTO F12 DAL CHIAMATO
055700000000IF  1C*---       D01F12    IFEQ '1'
055800000000     C*---                 MOVEL'1'       $ERR
055900000000E   1C*---                 ENDIF
056000000000     C*---
056100000000     C*---                 CLEARD01OP1
056200000000     C*---                 MOVEL'0'       D01F12
056300000000     C*---
056400000000     C*---                 ENDSR
056500020920     C*--------------------------------------------------------------------------------------------*
056600020920     C* REPERISCE I DATI UTENTE
056700020920     C*--------------------------------------------------------------------------------------------*
056800020920     C     REPDATIUTE    BEGSR
056900020920     C*
057000020920     C* INIZIALIZZA VARIABILI DI WRK
057100020920     C                   CLEAR                   TIBS34DS
057200020920     C                   CLEAR                   AZUTEDS
057300020920     C                   CLEAR                   DDATIUTE
057400020920     C*
057500020920     C     *DTAARA       DEFINE    §azute        azuteds
057600020920     C     *DTAARA       DEFINE    §datiute      ddatiute
057700020920     C                   IN(E)     *DTAARA
057800020920if  1C                   IF        %Error
057900020920     c                   EVAL      I34Tla = 'L'
058000020920     C                   CALL      'TIBS34R'
058100020920     C                   PARM                    Tibs34Ds
058200020920     C                   IN        *DTAARA
058300020920e   1C                   ENDIF
058400020920     C*
058500020920     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
058600020920     C                   MOVEL(P)  RSUT          DSFIRS           20
058700020920     C*
058800020920     C                   ENDSR
058900000000     C*------------------------------------------------------------------------*
059000000000     C*  *INZSR - ROUTINE INIZIALE
059100000000     C*------------------------------------------------------------------------*
059200000000     C     *INZSR        BEGSR
059300000000     C*--------------------
059400000000     C* RICEVIMENTO PARAMETRI
059500000000     C*--------------------
059600000000     C     *ENTRY        PLIST
059700000000     C                   PARM                    KPJBA
059800000000     C                   MOVEL     KPJBU         DSSB01
059900000000     C*--------------------
060000000000     C* VARIABILI RIFERITE AL DATA BASE
060100000000     C*--------------------
060200000000     C     *LIKE         DEFINE    CETAAS        KCTAAS                         *ECCET30C
060300000000     C     *LIKE         DEFINE    CETLNP        KCTLNP
060400000000     C     *LIKE         DEFINE    CETNRS        KCTNRS
060500000000     C     *LIKE         DEFINE    CETNSP        KCTNSP
060600000000     C     *LIKE         DEFINE    CETTBL        KCTTBL
060700000000     C     *LIKE         DEFINE    CEDAAS        KCDAAS                         *ECCED30C
060800000000     C     *LIKE         DEFINE    CEDLNP        KCDLNP
060900000000     C     *LIKE         DEFINE    CEDNRS        KCDNRS
061000000000     C     *LIKE         DEFINE    CEDNSP        KCDNSP
061100000000     C     *LIKE         DEFINE    CEDTBL        KCDTBL
061200000000     C     *LIKE         DEFINE    ECTNBC        KETNBC                         *ECECT01L
061300000000     C     *LIKE         DEFINE    ECTCMP        KETCMP
061400000000     C     *LIKE         DEFINE    ECTDDC        KETDDC
061500000000     C     *LIKE         DEFINE    D01OP0        WOP0                           *DI LAVORO
061600000000     C     *LIKE         DEFINE    D01OP1        WOP1
061700000000     C     *LIKE         DEFINE    D01AAS        WAAS
061800000000     C     *LIKE         DEFINE    D01LNP        WLNP
061900000000     C     *LIKE         DEFINE    D01NRS        WNRS
062000000000     C     *LIKE         DEFINE    D01NSP        WNSP
062100000000     C     *LIKE         DEFINE    D01TBL        WTBL
062200000000     C     *LIKE         DEFINE    D01DSP        WDSP
062300000000     C     *LIKE         DEFINE    D01DTV        WDTV
062400000000     C     *LIKE         DEFINE    D01ORV        WORV
062500000000     C     *LIKE         DEFINE    D01PRU        WPRU
062600000000     C     *LIKE         DEFINE    VCCAAS        DEPAAS                         *DEPOSITI
062700000000     C*--------------------
062800000000     C* VARIABILI DI CONTROLLO
062900000000     C*--------------------
063000000000     C                   TIME                    ORADAT           14 0          *ORA(6) + DATA(8)
063100000000     C                   MOVEL     ORADAT        ORA               6 0          *ORA
063200000000     C                   MOVE      ORADAT        DATA              8 0          *DATA GG/MM/AAAA
063300000000     C*--------------------
063400000000     C* CHIAVI DI LETTURA
063500000000     C*--------------------
063600000000     C* LETTURA ECCET30C - COMPELTA
063700000000     C     KEYCET        KLIST
063800000000     C                   KFLD                    KCTAAS                         *ANNO
063900000000     C                   KFLD                    KCTLNP                         *LINEA DI PARTENZA
064000000000     C                   KFLD                    KCTNRS                         *SERIE
064100000000     C                   KFLD                    KCTNSP                         *SPEDIZIONE
064200000000     C                   KFLD                    KCTTBL                         *TIPO BOLLA
064300000000     C* LETTURA ECCED30C - COMPELTA
064400000000     C     KEYCED        KLIST
064500000000     C                   KFLD                    KCDAAS                         *ANNO
064600000000     C                   KFLD                    KCDLNP                         *LINEA DI PARTENZA
064700000000     C                   KFLD                    KCDNRS                         *SERIE
064800000000     C                   KFLD                    KCDNSP                         *SPEDIZIONE
064900000000     C                   KFLD                    KCDTBL                         *TIPO BOLLA
065000000000     C* LETTURA ECECT01L - COMPELTA
065100000000     C     KEYECT        KLIST
065200000000     C                   KFLD                    KETNBC                         *NUMERO BLOCCO
065300000000     C                   KFLD                    KETCMP                         *CODICE COMPETENZA
065400000000     C                   KFLD                    KETDDC                         *DATA DECORRENZA
065500000000     C* LETTURA ECECT01L - PARZIALE
065600000000     C     KE2ECT        KLIST
065700000000     C                   KFLD                    KETNBC                         *NUMERO BLOCCO
065800000000     C                   KFLD                    KETCMP                         *CODICE COMPETENZA
065900000000     C*
066000000000     C* V02 --> GESTIONE;  V05 --> INTERROGAZIONE
066100000000     C                   MOVE      D01OP0        INTGES            2
066200000000IF  1C     INTGES        IFEQ      '02'
066300000000     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
066400000000     C*************        MOVELDESGES    VTDOPE           *DESCR OPERAZIONE
066500000000X   1C                   ELSE
066600000000     C                   MOVEL     'I'           $TGES             1            *I=INTERROGAZIONE
066700000000     C*************        MOVELDESINT    VTDOPE           *DESCR OPERAZIONE
066800000000E   1C                   ENDIF
066900000000     C*
067000000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
067100000000     C                   EXSR      INZSTD
067200000000     C*
067300000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
067400000000     C                   EXSR      INZSFL
067500000000     C*
067600000000     C* IMPOSTO CAMPI TESTATA VIDEO
067700000000     C                   MOVEL     DSFIRS        VTDRSU                         *RAGIONE SOC. UTENTE
067800000000     C*
067900000000     C* IMPOSTO ANCHE I DEPOSITI PER CONFRONTI SUCCESSIVI
068000000000     C                   Z-ADD     D01AAS        VCCAAS                         *ANNO
068100000000     C                   Z-ADD     VCCAAS        DEPAAS                         *DEPOSITO
068200000000     C                   Z-ADD     D01LNP        VCCLNP                         *LINEA DI PARTENZA
068300000000     C                   Z-ADD     D01NRS        VCCNRS                         *SERIE
068400000000     C                   Z-ADD     D01NSP        VCCNSP                         *SPEDIZIONE
068500000000     C                   MOVEL     D01TBL        VCCTBL                         *TIPO BOLLA
068600000000     C                   Z-ADD     D01DSP        VCCDSP                         *DATA SPEDIZIONE
068700000000     C*
068800000000     C* CARICA TUTTE LE COMPETENZE DELLA BOLLA IN SCHIERA
068900000000     C                   EXSR      CARCMP
069000000000     C*
069100000000     C* NUMERO BLOCCO COMPETENZE
069200000000     C                   Z-ADD     CETNBC        VCCNBC
069300000000     C*
069400000000     C* EMETTO VIDEATA SUBFILE
069500000000     C                   EXSR      CARVS                                        CARICA E EMETTE SFIL
069600000000     C*
069700000000     C                   ENDSR
069800000000     C*------------------------------------------------------------------------*
069900000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
070000000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
070100000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
070200000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
070300000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
070400000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
070500000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
070600000000     C*------------------------------------------------------------------------*
070700000000     C* INZSTD - ROUTINE INIZIALE STANDARD
070800000000     C*------------------------------------------------------------------------*
070900000000     C     INZSTD        BEGSR
071000000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
071100000000     C                   CALL      'XKEYS'
071200000000     C                   PARM                    DSKEYS
071300000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
071400000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
071500000000     C     XMSGPA        PLIST
071600000000     C                   PARM                    DSMSMS
071700000000     C                   PARM                    DSMSPG
071800000000     C                   PARM                    DSMSKE
071900000000     C                   PARM                    DSMSER
072000000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
072100000000     C                   MOVEL     'TNSB08R'     DSMSPG                         *NOME PGM CHIAMANTE
072200000000     C                   MOVEL     'TNSB08R'     VMCQUE                         *CODA MESSAGGI DESTI
072300000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
072400000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
072500000000     C* INIZIALIZZO FLAG CONTROLLO PGM
072600000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
072700000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
072800000000     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
072900000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
073000000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
073100000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
073200000000     C                   MOVEL     '0'           $TESVD            1            *1=RIEMETT.TEST.VIDE
073300000000     C* LANCIO ROUTINE PER VERIFICARE SE
073400000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
073500000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
073600020920     C                   EXSR      REPDATIUTE
073700000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
073800000000     C                   BITOFF    '01234567'    POSMI1            1
073900000000     C                   BITOFF    '01234567'    POSMI2            1
074000000000     C                   BITOFF    '01234567'    POSMA1            1
074100000000     C                   BITOFF    '01234567'    POSMA2            1
074200000000     C                   BITON     '34'          POSMI1            1            *18
074300000000     C                   BITON     '7'           POSMI2            1            *01
074400000000     C                   BITON     '34'          POSMA1            1            *18
074500000000     C                   BITON     '0'           POSMA2            1            *80
074600000000     C                   MOVE      POSMI2        POSMIN            2
074700000000     C                   MOVEL     POSMI1        POSMIN
074800000000     C                   MOVE      POSMA2        POSMAX            2
074900000000     C                   MOVEL     POSMA1        POSMAX
075000000000     C                   ENDSR
075100000000     C*------------------------------------------------------------------------*
075200000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
075300000000     C*------------------------------------------------------------------------*
075400000000     C     FUNSTD        BEGSR
075500000000     C                   SELECT
075600000000     C     KEY           WHENEQ    F3
075700000000     C                   EXSR      F03STD
075800000000     C     KEY           WHENEQ    F12
075900000000     C                   EXSR      F12STD
076000000000     C     KEY           WHENEQ    F24
076100000000     C                   EXSR      F24STD
076200000000     C                   OTHER
076300000000     C                   MOVEL     MSG(2)        $MSG
076400000000     C                   EXSR      CARMSG
076500000000     C                   ENDSL
076600000000     C                   ENDSR
076700000000     C*------------------------------------------------------------------------*
076800000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
076900000000     C*------------------------------------------------------------------------*
077000000000     C     F03STD        BEGSR
077100000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
077200000000     C                   MOVEL     '1'           $F03
077300000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
077400000000     C                   MOVEL     '1'           $FINE
077500000000     C                   ENDSR
077600000000     C*------------------------------------------------------------------------*
077700000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
077800000000     C*------------------------------------------------------------------------*
077900000000     C     F12STD        BEGSR
078000000000     C* IMPOSTO FLAG DI PREMUTO TASTO F12 (A LIVELLO PROCEDURA)
078100000000     C                   MOVEL     '1'           $F12
078200000000     C* IMPOSTO FLAG DI FIN PGM
078300000000     C                   MOVEL     '1'           $FINE
078400000000     C                   ENDSR
078500000000     C*------------------------------------------------------------------------*
078600000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
078700000000     C*------------------------------------------------------------------------*
078800000000     C     F24STD        BEGSR
078900000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
079000000000     C                   EXSR      OUTFUN
079100000000     C                   ENDSR
079200000000     C*------------------------------------------------------------------------*
079300000000     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
079400000000     C*------------------------------------------------------------------------*
079500000000     C     INZSFL        BEGSR
079600000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
079700000000     C     *LIKE         DEFINE    VCCNBR        SFLPAG
079800000000     C     *LIKE         DEFINE    VCCNBR        SFLEOF
079900000000     C     *LIKE         DEFINE    VCCNBR        SFLERR
080000000000     C     *LIKE         DEFINE    VCCNBR        SFLULT
080100000000     C     *LIKE         DEFINE    VCCNBR        WFLULT
080200000000     C     *LIKE         DEFINE    VCCNBR        SFLRIG
080300000000     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
080400000000     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
080500000000     C                   Z-ADD     16            SFLPAG
080600000000     C                   ENDSR
080700000000     C*------------------------------------------------------------------------*
080800000000     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
080900000000     C*------------------------------------------------------------------------*
081000000000     C     FUNSFL        BEGSR
081100000000     C                   SELECT
081200000000     C     KEY           WHENEQ    ENTER
081300000000     C                   EXSR      ENTSFL
081400000000     C     KEY           WHENEQ    F3
081500000000     C                   EXSR      F03STD
081600000000     C     KEY           WHENEQ    F5
081700000000     C                   EXSR      F05STD
081800000000     C     KEY           WHENEQ    F12
081900000000     C                   EXSR      F12STD
082000000000     C     KEY           WHENEQ    F17
082100000000     C                   EXSR      F17STD
082200000000     C     KEY           WHENEQ    F18
082300000000     C                   EXSR      F18STD
082400000000     C     KEY           WHENEQ    F23
082500000000     C                   EXSR      F23STD
082600000000     C     KEY           WHENEQ    F24
082700000000     C                   EXSR      F24STD
082800000000     C     KEY           WHENEQ    ROLLUP
082900000000     C                   EXSR      RUPSTD
083000000000     C     KEY           WHENEQ    ROLLDN
083100000000     C                   EXSR      RDNSTD
083200000000     C                   OTHER
083300000000     C                   MOVEL     MSG(2)        $MSG
083400000000     C                   EXSR      CARMSG
083500000000     C                   ENDSL
083600000000     C                   ENDSR
083700000000     C*------------------------------------------------------------------------*
083800000000     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
083900000000     C*------------------------------------------------------------------------*
084000000000     C     F05STD        BEGSR
084100000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
084200000000     C                   EXSR      POSSFL
084300000000     C                   ENDSR
084400000000     C*------------------------------------------------------------------------*
084500000000     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
084600000000     C*------------------------------------------------------------------------*
084700000000     C     F17STD        BEGSR
084800000000     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
084900000000     C                   Z-ADD     1             VCCNBR
085000000000     C                   ENDSR
085100000000     C*------------------------------------------------------------------------*
085200000000     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
085300000000     C*------------------------------------------------------------------------*
085400000000     C     F18STD        BEGSR
085500000000     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
085600000000     C                   Z-ADD     SFLULT        VCCNBR
085700000000     C                   ENDSR
085800000000     C*------------------------------------------------------------------------*
085900000000     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
086000000000     C*------------------------------------------------------------------------*
086100000000     C     F23STD        BEGSR
086200000000     C* GESTIONE VISUALIZZAZIONE OPZIONI
086300000000     C                   EXSR      OUTOPZ
086400000000     C                   ENDSR
086500000000     C*------------------------------------------------------------------------*
086600000000     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
086700000000     C*------------------------------------------------------------------------*
086800000000     C     RUPSTD        BEGSR
086900000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
087000000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
087100000000     C     CURSOR        IFGE      POSMIN                                       *POSIZ.INIZIO RIGA 2
087200000000     C     CURSOR        ANDLE     POSMAX                                       *POSIZ.FINE RIGA 24
087300000000     C                   ELSE
087400000000     C                   EXSR      CARSFL                                       *CARICA SFL DAL FILE
087500000000     C                   ENDIF
087600000000     C                   ENDSR
087700000000     C*------------------------------------------------------------------------*
087800000000     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
087900000000     C*------------------------------------------------------------------------*
088000000000     C     RDNSTD        BEGSR
088100000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
088200000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
088300000000     C     CURSOR        IFGE      POSMIN
088400000000     C     CURSOR        ANDLE     POSMAX
088500000000     C                   ELSE
088600000000     C* INVIO MESSAGGIO DI INIZIO ELENCO
088700000000     C                   MOVEL     '1'           $ERR
088800000000     C                   MOVEL     MSG(5)        $MSG
088900000000     C                   EXSR      CARMSG
089000000000     C                   ENDIF
089100000000     C                   ENDSR
089200000000     C*------------------------------------------------------------------------*
089300000000     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
089400000000     C*------------------------------------------------------------------------*
089500000000     C     ENTSFL        BEGSR
089600000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
089700000000     C                   EXSR      NEWPOS
089800000000     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
089900000000     C     $ERR          IFEQ      '0'                                          --- 1 -->
090000000000     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
090100000000     C     SWPOSI        IFNE      0                                            --- 2 -->
090200000000     C                   EXSR      POSSFL
090300000000     C                   ELSE                                                   <-- 2 -->
090400000000     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
090500000000     C     SFLULT        IFNE      0                                            --- 3 -->
090600000000     C                   EXSR      OPZSFL
090700000000     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
090800000000     C     SWMODI        IFNE      0                                            --- 4 -->
090900000000     C                   EXSR      POSSFL
091000000000     C                   ENDIF                                                  <-- 4 ---
091100000000     C                   ENDIF                                                  <-- 3 ---
091200000000     C                   ENDIF                                                  <-- 2 ---
091300000000     C                   ENDIF                                                  <-- 1 ---
091400000000     C                   ENDSR
091500000000     C*------------------------------------------------------------------------*
091600000000     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
091700000000     C*------------------------------------------------------------------------*
091800000000     C     POSSFL        BEGSR
091900000000     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
092000000000     C                   EXSR      CLRSFL
092100000000     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
092200000000     C                   EXSR      SETFIL
092300000000     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
092400000000     C     SFLEOF        IFEQ      0
092500000000     C                   EXSR      CARSFL
092600000000     C                   ENDIF
092700000000     C                   ENDSR
092800000000     C*------------------------------------------------------------------------*
092900000000     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
093000000000     C*------------------------------------------------------------------------*
093100000000     C     OPZSFL        BEGSR
093200000000     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
093300000000     C                   MOVEL     'S'           OPZOK             1
093400000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
093500000000     C                   Z-ADD     0             SFLVAR            5 0
093600000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
093700000000     C                   Z-ADD     0             SFLERR
093800000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
093900000000     C                   Z-ADD     SFLULT        WFLULT
094000000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
094100000000     C                   READC     SB08DVS                                49
094200000000     C     *IN49         DOWEQ     '0'                                          --- 1 -->
094300000000     C     $ERR          ANDEQ     '0'
094400000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
094500000000     C                   ADD       1             SFLVAR
094600000000     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
094700000000     C     OPZOK         IFEQ      'S'                                          --- 2 -->
094800000000     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
094900000000     C     VSCOPZ        IFNE      *BLANKS                                      --- 3 -->
095000000000     C                   EXSR      CONOPZ
095100000000     C     OPZOK         IFEQ      'S'
095200000000     C     $ERR          ANDEQ     '0'
095300000000     C                   EXSR      GESOPZ
095400000000     C                   ENDIF
095500000000     C                   ENDIF                                                  <-- 3 ---
095600000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
095700000000     C*            2. PULIZIA SCELTA IN SFL
095800000000     C     OPZOK         IFEQ      'S'                                          --- 3 -->
095900000000     C                   MOVEL     '0'           *IN41
096000000000     C                   MOVEL     *BLANKS       VSCOPZ
096100000000     C                   ELSE                                                   <-- 3 -->
096200000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
096300000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
096400000000     C                   MOVEL     '1'           *IN41
096500000000     C                   Z-ADD     SFLULT        SFLERR
096600000000     C                   ENDIF                                                  <-- 3 ---
096700000000     C                   ELSE                                                   <-- 2 -->
096800000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
096900000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
097000000000     C                   MOVEL     '1'           *IN41
097100000000     C                   ENDIF                                                  <-- 2 ---
097200000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
097300000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
097400000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
097500000000     C     OPZOK         IFNE      'S'
097600000000     C                   Z-ADD     SFLERR        VCCNBR
097700000000     C                   ELSE
097800000000     C                   Z-ADD     SFLULT        VCCNBR
097900000000     C                   ENDIF
098000000000     C* AGGIORNO RECORD SFL
098100000000     C                   UPDATE    SB08DVS
098200000000     C     $ERR          IFEQ      '0'
098300000000     C                   READC     SB08DVS                                49
098400000000     C                   ENDIF
098500000000     C                   ENDDO                                                  <-- 1 ---
098600000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
098700000000     C                   Z-ADD     WFLULT        SFLULT
098800000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
098900000000     C     SFLVAR        IFEQ      0
099000000000     C                   MOVEL     '1'           $FINE
099100000000     C                   ENDIF
099200000000     C                   ENDSR
099300000000     C*------------------------------------------------------------------------*
099400000000     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
099500000000     C*------------------------------------------------------------------------*
099600000000     C     CLRSFL        BEGSR
099700000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
099800000000     C* (0=NON FINE FILE  1=FINE FILE)
099900000000     C                   Z-ADD     0             SFLEOF
100000000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
100100000000     C                   Z-ADD     0             SFLULT
100200000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
100300000000     C                   MOVEL     '1'           *IN40
100400000000     C                   WRITE     SB08DVC
100500000000     C                   MOVEL     '0'           *IN40
100600000000     C                   ENDSR
100700000000     C*------------------------------------------------------------------------*
100800000000     C* CARSFL - CARICAMENTO SFL
100900000000     C*------------------------------------------------------------------------*
101000000000     C     CARSFL        BEGSR
101100000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
101200000000     C     SFLULT        IFEQ      0
101300000000     C                   EXSR      SETFIL
101400000000     C                   ENDIF
101500000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
101600000000     C                   Z-ADD     SFLULT        WFLULT
101700000000     C* CICLO:
101800000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
101900000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
102000000000     C                   Z-ADD     1             SFLRIG
102100000000     C     SFLRIG        DOWLE     SFLPAG                                       --- 1 -->
102200000000     C     SFLEOF        ANDEQ     0
102300000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
102400000000     C                   MOVEL     *BLANKS       VSCOPZ
102500000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
102600000000     C                   EXSR      IRESFL
102700000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
102800000000     C                   ADD       1             SFLULT
102900000000     C* SCRITTURA RECORD SFL
103000000000     C                   WRITE     SB08DVS
103100000000     C                   ADD       1             SFLRIG
103200000000     C* LETTURA FILE DA CARICARE
103300000000     C                   EXSR      LETFIL
103400000000     C                   ENDDO                                                  <-- 1 ---
103500000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
103600000000     C     WFLULT        IFNE      SFLULT
103700000000     C                   Z-ADD     WFLULT        VCCNBR
103800000000     C                   ADD       1             VCCNBR
103900000000     C                   ENDIF
104000000000     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
104100000000     C     SFLEOF        IFEQ      1                                            --- 1 -->
104200000000     C     SFLULT        ANDGT     *ZEROS
104300000000     C                   MOVEL     '1'           $ERR
104400000000     C                   MOVEL     MSG(6)        $MSG
104500000000     C                   EXSR      CARMSG
104600000000     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
104700000000     C******     SFLULT    CHAINSB08DVS              99
104800000000     C******     *IN99     IFEQ '0'
104900000000     C******               SETON                     45
105000000000     C******               UPDATSB08DVS
105100000000     C******               SETOF                     45
105200000000     C******               ENDIF
105300000000     C                   ENDIF                                                  <-- 1 ---
105400000000     C                   ENDSR
105500000000     C*------------------------------------------------------------------------*
105600000000     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
105700000000     C*------------------------------------------------------------------------*
105800000000     C     OUTSFL        BEGSR
105900000000     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
106000000000     C     MSGULT        IFNE      *ZEROS
106100000000     C                   EXSR      OUTMSG
106200000000     C                   ENDIF
106300000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
106400000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
106500000000     C     SFLEOF        IFEQ      0
106600000000     C                   MOVEL     '0'           *IN42
106700000000     C                   ELSE
106800000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
106900000000     C                   MOVEL     '1'           *IN42
107000000000     C                   ENDIF
107100000000     C* EMISSIONE SUBFILE
107200000000     C     SFLULT        IFLE      0
107300000000     C*  --> CASO DI SFL VUOTO
107400000000     C                   MOVEL     '0'           *IN43                          *NON VISUALIZZA SFL
107500000000     C                   WRITE     SB08DVV                                      *'NON VI SONO DATI'
107600000000     C                   WRITE     SB08DVC                                      *SFL CONTROL
107700000000     C                   READ      SB08DVC                                99    *SFL CONTROL
107800000000     C                   ELSE
107900000000     C*  --> CASO DI SFL CON RECORDS
108000000000     C                   WRITE     SB08DVC                                      *SFL CONTROL
108100000000     C                   READ      SB08DVC                                99    *SFL CONTROL
108200000000     C                   ENDIF
108300000000     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
108400000000     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
108500000000     C                   Z-ADD     FRTSFL        VCCNBR
108600000000     C                   ENDSR
108700000000     C*------------------------------------------------------------------------*
108800000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
108900000000     C*          CODA MESSAGGI DEL PGM
109000000000     C*------------------------------------------------------------------------*
109100000000     C     CARMSG        BEGSR
109200000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
109300000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
109400000000     C     MSGULT        IFEQ      0
109500000000     C                   MOVEL     '1'           *IN40
109600000000     C                   WRITE     SB08DVK
109700000000     C                   MOVEL     '0'           *IN40
109800000000     C                   ENDIF
109900000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
110000000000     C                   MOVEL     $MSG          DSMSMS
110100000000     C                   CALL      'XMSG'        XMSGPA
110200000000     C     DSMSER        IFEQ      0
110300000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
110400000000     C                   ENDIF
110500000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
110600000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
110700000000     C                   WRITE     SB08DVM                                      *SFL MESS
110800000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
110900000000     C                   MOVEL     *BLANKS       $MSG
111000000000     C                   ENDSR
111100000000     C*------------------------------------------------------------------------*
111200000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
111300000000     C*------------------------------------------------------------------------*
111400000000     C     OUTMSG        BEGSR
111500000000     C* VISUALIZZAZIONE MESSAGGIO
111600000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
111700000000     C                   WRITE     SB08DVK                                      *CONTR MESS
111800000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
111900000000     C                   Z-ADD     *ZEROS        MSGULT
112000000000     C                   ENDSR
112100000000     C*------------------------------------------------------------------------*
112200000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
112300000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
112400000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
112500000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
112600000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
112700000000     C*------------------------------------------------------------------------*
112800000000     C     CARFUN        BEGSR
112900000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
113000000000     C     1             DO        31            I                 5 0          --- 1 -->
113100000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
113200000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
113300000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
113400020920     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
113500000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
113600000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
113700000000     C                   ELSE                                                   *INTERROG.
113800000000     C                   MOVEL     FUNASI        FAT(I)
113900000000     C                   ENDIF
114000000000     C                   ELSE                                                   <-- 2 --> *FILIALE
114100000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
114200000000     C                   MOVEL     FUNAFG        FAT(I)
114300000000     C                   ELSE                                                   *INTERROG.
114400000000     C                   MOVEL     FUNAFI        FAT(I)
114500000000     C                   ENDIF
114600000000     C                   ENDIF                                                  <-- 2 ---
114700000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
114800000000     C                   MOVEL     USER          WEDP              3            *PROFILO
114900000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
115000000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
115100000000     C                   ELSE
115200000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
115300000000     C                   ENDIF
115400000000     C                   ENDIF
115500000000     C                   ENDDO                                                  <-- 1 ---
115600000000     C                   ENDSR
115700000000     C*------------------------------------------------------------------------*
115800000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
115900000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
116000000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
116100000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
116200000000     C*------------------------------------------------------------------------*
116300000000     C     OUTFUN        BEGSR
116400000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
116500000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
116600000000     C                   ENDDO
116700000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
116800000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
116900000000     C     1             DO        31            I
117000000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
117100000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
117200000000     C                   ADD       1             FUNNAT
117300000000     C                   ENDIF
117400000000     C                   ENDDO
117500000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
117600000000     C     FUNNAT        IFGT      8
117700000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
117800000000     C                   ENDIF
117900000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
118000000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
118100000000     C                   Z-ADD     8             FUNVIS            5 0
118200000000     C     FAT(23)       IFEQ      'S'
118300000000     C                   SUB       1             FUNVIS
118400000000     C                   ENDIF
118500000000     C     FAT(24)       IFEQ      'S'
118600000000     C                   SUB       1             FUNVIS
118700000000     C                   ENDIF
118800000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
118900000000     C                   Z-ADD     *ZEROS        W                 5 0
119000000000     C                   ADD       1             FUNULT
119100000000     C     FUNULT        IFGT      31
119200000000     C                   Z-ADD     1             FUNULT
119300000000     C                   ENDIF
119400000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
119500000000     C     FUNULT        DO        31            I                              --- 1 -->
119600000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
119700000000     C     W             IFLT      FUNVIS                                       --- 2 -->
119800000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
119900000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
120000000000     C     FDE(I)        ANDNE     *BLANKS
120100000000     C     I             IFEQ      23                                           *TOLGO F23
120200000000     C     I             OREQ      24                                           *E F24
120300000000     C                   ELSE
120400000000     C                   ADD       1             W
120500000000     C                   MOVEL     FDE(I)        FUV(W)
120600000000     C                   ENDIF
120700000000     C                   ENDIF                                                  <-- 3 ---
120800000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
120900000000     C                   Z-ADD     I             FUNULT
121000000000     C                   ENDIF                                                  <-- 2 ---
121100000000     C                   ENDDO                                                  <-- 1 ---
121200000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
121300000000     C                   MOVEL     FUV(1)        VFD001
121400000000     C                   MOVEL     FUV(2)        VFD002
121500000000     C                   MOVEL     FUV(3)        VFD003
121600000000     C                   MOVEL     FUV(4)        VFD004
121700000000     C                   MOVEL     FUV(5)        VFD005
121800000000     C                   MOVEL     FUV(6)        VFD006
121900000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
122000000000     C     FAT(24)       ANDEQ     'N'
122100000000     C                   MOVEL     FUV(7)        VFD007
122200000000     C                   MOVEL     FUV(8)        VFD008
122300000000     C                   ELSE                                                   <-- 1 -->
122400000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
122500000000     C     FAT(24)       ANDEQ     'S'
122600000000     C                   MOVEL     FDE(23)       VFD007
122700000000     C                   MOVEL     FDE(24)       VFD008
122800000000     C                   ELSE                                                   <-- 2 -->
122900000000     C                   MOVEL     FUV(7)        VFD007
123000000000     C     FAT(23)       IFEQ      'S'
123100000000     C                   MOVEL     FDE(23)       VFD008
123200000000     C                   ELSE
123300000000     C                   MOVEL     FDE(24)       VFD008
123400000000     C                   ENDIF
123500000000     C                   ENDIF                                                  <-- 2 ---
123600000000     C                   ENDIF                                                  <-- 1 ---
123700000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
123800000000     C                   ENDSR
123900000000     C*------------------------------------------------------------------------*
124000000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
124100000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
124200000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
124300000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
124400000000     C*------------------------------------------------------------------------*
124500000000     C     CONFUN        BEGSR
124600000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
124700000000     C     1             DO        31            I                              --- 1 -->
124800000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
124900000000     C* CONTROLLO SE IL TASTO E' ATTIVO
125000000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
125100000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
125200000000     C                   EXSR      CARMSG
125300000000     C                   MOVEL     '1'           $ERR
125400000000     C                   ENDIF                                                  <-- 3 ---
125500000000     C                   ENDIF                                                  <-- 2 ---
125600000000     C                   ENDDO                                                  <-- 1 ---
125700000000     C                   ENDSR
125800000000     C*------------------------------------------------------------------------*
125900000000     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
126000000000     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
126100000000     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
126200000000     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
126300000000     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
126400000000     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
126500000000     C*------------------------------------------------------------------------*
126600000000     C     CAROPZ        BEGSR
126700000000     C                   Z-ADD     *ZEROS        OPZULT            5 0          *ULTIMA OPZIONE VISU
126800000000     C     1             DO        24            I                              --- 1 -->
126900000000     C                   MOVEL     OPZ(I)        OPZRIG                         *DS RIGA OPZIONI
127000000000     C                   MOVEL     OPZOPD        OPD(I)                         *OPZ ALLINEATA DESTR
127100000000     C                   MOVEL     OPZOPS        OPS(I)                         *OPZ ALLINEATA SINIS
127200000000     C                   MOVEL     OPZDES        ODE(I)                         *DESCRIZIONE OPZ
127300020920     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
127400000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
127500000000     C                   MOVEL     OPZASG        OAT(I)                         *OPZ ATTIVA S/N
127600000000     C                   ELSE                                                   *INTERROG.
127700000000     C                   MOVEL     OPZASI        OAT(I)                         *OPZ ATTIVA S/N
127800000000     C                   ENDIF
127900000000     C                   ELSE                                                   <-- 2 --> *FILIALE
128000000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
128100000000     C                   MOVEL     OPZAFG        OAT(I)                         *OPZ ATTIVA S/N
128200000000     C                   ELSE                                                   *INTERROG.
128300000000     C                   MOVEL     OPZAFI        OAT(I)                         *OPZ ATTIVA S/N
128400000000     C                   ENDIF
128500000000     C                   ENDIF                                                  <-- 2 ---
128600000000     C     OAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
128700000000     C                   MOVEL     USER          WEDP              3            *PROFILO
128800000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
128900000000     C                   MOVEL     'S'           OAT(I)                         *ABILITO
129000000000     C                   ELSE
129100000000     C                   MOVEL     'N'           OAT(I)                         *NON ABILITO
129200000000     C                   ENDIF
129300000000     C                   ENDIF
129400000000     C                   ENDDO                                                  <-- 1 ---
129500000000     C                   ENDSR
129600000000     C*------------------------------------------------------------------------*
129700000000     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
129800000000     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
129900000000     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
130000000000     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
130100000000     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
130200000000     C*------------------------------------------------------------------------*
130300000000     C     OUTOPZ        BEGSR
130400000000     C     1             DO        8             I                              *CICLO PULIZIA CAMPI
130500000000     C                   MOVEL     *BLANKS       OPV(I)                         *DEL VIDEO
130600000000     C                   ENDDO
130700000000     C                   Z-ADD     *ZEROS        OPZNAT            2 0          *N.OPZIONI ATTIVE
130800000000     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
130900000000     C                   ADD       1             OPZULT
131000000000     C     OPZULT        IFGT      24
131100000000     C                   Z-ADD     1             OPZULT
131200000000     C                   END
131300000000     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
131400000000     C     1             DO        24            I
131500000000     C     OAT(I)        IFEQ      'S'                                          *FLAG ATTIVA S/N
131600000000     C     ODE(I)        ANDNE     *BLANKS                                      *DESCRIZ. <> SPAZIO
131700000000     C                   ADD       1             OPZNAT
131800000000     C                   ENDIF
131900000000     C                   ENDDO
132000000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
132100000000     C                   Z-ADD     0             W
132200000000     C     OPZULT        DO        24            I                              --- 1 -->
132300000000     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
132400000000     C     W             IFLT      8                                            --- 2 -->
132500000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
132600000000     C     OAT(I)        IFEQ      'S'
132700000000     C     ODE(I)        ANDNE     *BLANKS
132800000000     C                   ADD       1             W
132900000000     C                   MOVEL     ODE(I)        OPV(W)
133000000000     C                   ENDIF
133100000000     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
133200000000     C                   Z-ADD     I             OPZULT
133300000000     C                   ENDIF                                                  <-- 2 ---
133400000000     C                   ENDDO                                                  <-- 1 ---
133500000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
133600000000     C                   MOVEL     OPV(1)        VOD001
133700000000     C                   MOVEL     OPV(2)        VOD002
133800000000     C                   MOVEL     OPV(3)        VOD003
133900000000     C                   MOVEL     OPV(4)        VOD004
134000000000     C                   MOVEL     OPV(5)        VOD005
134100000000     C                   MOVEL     OPV(6)        VOD006
134200000000     C                   MOVEL     OPV(7)        VOD007
134300000000     C                   MOVEL     OPV(8)        VOD008
134400000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
134500000000     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
134600000000     C     OPZNAT        IFGT      8
134700000000     C                   MOVEL     'S'           FAT(23)
134800000000     C                   ENDIF
134900000000     C                   ENDSR
135000000000     C*------------------------------------------------------------------------*
135100000000     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
135200000000     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
135300000000     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
135400000000     C*                   - ERRORE SE OPZIONE NON ATTIVA
135500000000     C*------------------------------------------------------------------------*
135600000000     C     CONOPZ        BEGSR
135700000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
135800000000     C                   MOVEL     *BLANKS       OPZOK             1
135900000000     C     1             DO        24            I                 5 0          --- 1 -->CICLO 24 OP
136000000000     C     VSCOPZ        IFEQ      OPD(I)                                       --- 2 --> *ALLIN.DX
136100000000     C     VSCOPZ        OREQ      OPS(I)                                                 *ALLIN.SX
136200000000     C     OAT(I)        IFEQ      'S'
136300000000     C                   MOVEL     OPD(I)        VSCOPZ                         *PRENDO COMUNQUE DX
136400000000     C                   MOVEL     'S'           OPZOK
136500000000     C                   ELSE
136600000000     C                   MOVEL     'N'           OPZOK             1
136700000000     C                   ENDIF
136800000000     C                   ENDIF                                                  <-- 2 ---
136900000000     C                   ENDDO                                                  <-- 1 ---
137000000000     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
137100000000     C     OPZOK         IFNE      'S'
137200000000     C                   MOVEL     MSG(3)        $MSG                           *OPZ NON ACCETTABILE
137300000000     C                   EXSR      CARMSG
137400000000     C                   MOVEL     '1'           $ERR
137500000000     C                   MOVEL     'N'           OPZOK             1
137600000000     C                   ENDIF
137700000000     C                   ENDSR
137800000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
137900000000TNSB08R- Tasto funzionale non valido.                                          1
138000000000TNSB08R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
138100000000TNSB08R- Opzione non valida.                                                   3
138200000000TNSB08R- Opzione abilitata ma non gestita. Avvertire CED.                      4
138300000000TNSB08R- Inizio elenco.                                                        5
138400000000TNSB08R- Fine elenco.                                                          6
138500000000TNSB08R- 7
138600000000TNSB08R- 8
138700000000TNSB08R- 9
138800000000TNSB08R- 10
138900000000TNSB08R- 11
139000000000TNSB08R- 12
139100000000TNSB08R- 13
139200000000TNSB08R- 14
139300000000TNSB08R- 15
139400000000TNSB08R- 16
139500000000TNSB08R- 17
139600000000TNSB08R- 18
139700000000TNSB08R- 19
139800000000TNSB08R- 20
139900000000** OPZ  (OPZIONI)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
140000000000- 1-1 -                   -    NN     NN
140100000000- 2-2 -2=Modifica         -    NN     NN
140200000000- 3-3 -                   -    NN     NN
140300000000- 4-4 -4=Cancellazione    -    NN     NN
140400000000- 5-5 -5=Visualizzazione  -    NN     NN
140500000000- 6-6 -                   -    NN     NN
140600000000- 7-7 -7=Ripristino       -    NN     NN
140700000000- 8-8 -                   -    NN     NN
140800000000- 9-9 -                   -    NN     NN
140900000000-10-  -                   -    NN     NN
141000000000-11-  -                   -    NN     NN
141100000000-12-  -                   -    NN     NN
141200000000-13-  -                   -    NN     NN
141300000000-14-  -                   -    NN     NN
141400000000-15-  -                   -    NN     NN
141500000000-16-  -                   -    NN     NN
141600000000-17-  -                   -    NN     NN
141700000000-18-  -                   -    NN     NN
141800000000-19-  -                   -    NN     NN
141900000000-20-  -                   -    NN     NN
142000000000-21-  -                   -    NN     NN
142100000000-22-  -                   -    NN     NN
142200000000-23-  -                   -    NN     NN
142300000000-24-  -                   -    NN     NN
142400000000** FUN2 (SUBFILE)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
142500000000F1    -                   -    NN     NN
142600000000F2    -                   -    NN     NN
142700000000F3    -F3=Fine            -    SS     SS
142800000000F4    -                   -    NN     NN
142900000000F5    -                   -    NN     NN
143000000000F6    -                   -    NN     NN
143100000000F7    -                   -    NN     NN
143200000000F8    -                   -    NN     NN
143300000000F9    -                   -    NN     NN
143400000000F10   -                   -    NN     NN
143500000000F11   -                   -    NN     NN
143600000000F12   -F12=Ritorno        -    SS     SS
143700000000F13   -                   -    NN     NN
143800000000F14   -                   -    NN     NN
143900000000F15   -                   -    NN     NN
144000000000F16   -                   -    NN     NN
144100000000F17   -F17=Inizio dati    -    SS     SS
144200000000F18   -F18=Fine dati vis. -    SS     SS
144300000000F19   -                   -    NN     NN
144400000000F20   -                   -    NN     NN
144500000000F21   -                   -    NN     NN
144600000000F22   -                   -    NN     NN
144700000000F23   -                   -    NN     NN
144800000000F24   -F24=Altri tasti    -    NN     NN
144900000000ENTER -                   -    SS     SS
145000000000ROLLUP-                   -    SS     SS
145100000000ROLLDN-                   -    SS     SS
145200000000HELP  -                   -    NN     NN
145300000000PRINT -                   -    NN     NN
145400000000BSPACE-                   -    NN     NN
145500000000CLEAR -                   -    NN     NN
