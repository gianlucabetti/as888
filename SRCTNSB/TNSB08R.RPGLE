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
010400140530     D*---------------------
010500140530     D TNSB08DS1     E DS                                                       *Profili utente
010600140530     d  svKPJBU        s                   like(KPJBU)
010700140530     D*------------------
010800140530     D WLBDAT          DS
010900140530     D  G02DAT                 1      8  0
011000140530     D  G02INV                 9     16  0
011100140530     D  G02ERR                17     17
011200140530     D  G02TGI                18     22  0
011300000000     D*------------------
011400000000     D* CALL XSRDA8 - CONTROLLO DATE
011500000000     D*------------------
011600000000     D WLBDA8          DS                  INZ
011700000000     D  G08DAT                 1      8  0
011800000000     D  G08INV                 9     16  0
011900000000     D  G08ERR                17     17
012000000000     D  G08TGI                18     22  0
012100000000     D*------------------
012200000000     D* DATA SPEDIZIONE
012300000000     D*------------------
012400000000     D                 DS
012500000000     D  TASAAS                 1      4  0
012600000000     D  TASMGS                 5      8  0
012700000000     D  TASDSP                 1      8  0
012800000000     D*------------------
012900000000     D* DESCRIZIONE TOTALE COMPETENZE
013000000000     D*------------------
013100000000     D TODIMP          C                   CONST('TOTALE COMPETENZE')
013200000000     I*------------------
013300000000     I* DESCRIZIONE OPERAZIONE IN TESTATA
013400000000     I*------------------
013500000000     I**********    '*  GESTIONE  *'      C         DESGES
013600000000     I**********    'INTERROGAZIONE'      C         DESINT
013700000000     C*------------------------------------------------------------------------*
013800000000     C* MAIN LINE
013900000000     C*------------------------------------------------------------------------*
014000000000     C*
014100000000     C* CICLO FINO A CHE:
014200000000DO  1C     $FINE         DOUEQ     '1'
014300000000     C*
014400000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
014500000000     C*
014600000000     C* CONTROLLA GESTIONE VIDEO
014700000000IF  2C     $GES          IFEQ      'VS'
014800000000     C                   EXSR      GESVS                                        *SUBFILE
014900000000E   2C                   ENDIF
015000000000     C*
015100000000E   1C                   ENDDO
015200000000     C*
015300000000     C* SE E' STATO PREMUTO F03 IMPOSTO FLAG DELLA DS DI PROCEDURA
015400000000IF  1C     $F03          IFEQ      '1'
015500000000     C                   MOVEL     '1'           D01F03
015600000000E   1C                   ENDIF
015700000000     C*
015800000000     C* SE E' STATO PREMUTO F12 IMPOSTO FLAG DELLA DS DI PROCEDURA
015900000000IF  1C     $F12          IFEQ      '1'
016000000000     C                   MOVEL     '1'           D01F12
016100000000E   1C                   ENDIF
016200000000     C* FINE PGM
016300000000     C                   MOVEL     DSSB01        KPJBU
016400000000     C                   SETON                                        LR
016500000000     C*------------------------------------------------------------------------*
016600000000     C* CARVS  - CARICA GESTIONE VIDEO 'SUBFILE'
016700000000     C*------------------------------------------------------------------------*
016800000000     C     CARVS         BEGSR
016900000000     C*
017000000000     C                   MOVEL     'VS'          $GES                           *GESTIONE ATTUALE
017100000000     C*
017200000000     C                   EXSR      CAROPZ                                       *CARICA OPZIONI
017300000000     C                   EXSR      OUTOPZ                                       *EMETTE OPZIONI
017400000000     C                   MOVEA     FUN2          FUN                            *SCHIERA FUNZ.VIDEO
017500140612      * alcuni tasti funzionali devono essere attivi solo x EDPxxx
017600140612     c                   if        %subst(Knmus:1:3)='EDP'
017700140612     c                              or Knmus ='RIC008'
017800140612     C                   EXSR      eccez_EDP                                    *CARICA FUNZIONI
017900140612     c                   end
018000000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
018100000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
018200000000     C*
018300000000     C                   WRITE     SB08DVT                                      *TESTATA
018400000000     C                   WRITE     SB08DVO                                      *OPZIONI
018500000000     C                   WRITE     SB08DVF                                      *FUNZIONI
018600000000     C*
018700000000     C                   EXSR      POSSFL                                       *CARICA SFL
018800000000     C*
018900000000     C                   ENDSR
019000000000     C*------------------------------------------------------------------------*
019100140612     C* Eccezione alle FUZIONI attivabili solo come EDP
019200000000     C*------------------------------------------------------------------------*
019300140612     C     eccez_EDP     BEGSR
019400140612     C*
019500140612     C*  il tasto F9=Voci deve essere visto solo da EDP e non altri
019600140612     C*    quindi occorre attivare
019700140612     C*  SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.) che in default sono a "N"
019800140612     C*                         F9-F9=Voci
019900140612     c                   eval      %subst(FUN(9):32:2) ='SS'
020000140612     c                   eval      %subst(FUN(9):39:2) ='SS'
020100140612     C*
020200140612     C                   ENDSR
020300140612     C*------------------------------------------------------------------------*
020400140612     C* GESVS  - GESTIONE SUBFILE
020500140612     C*------------------------------------------------------------------------*
020600140612     C     GESVS         BEGSR
020700000000     C*
020800000000IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
020900000000     C                   WRITE     SB08DVT                                      *TESTATA
021000000000     C                   WRITE     SB08DVO                                      *OPZIONI
021100000000     C                   WRITE     SB08DVF                                      *FUNZIONI
021200000000E   1C                   ENDIF
021300000000     C*
021400000000     C                   EXSR      OUTSFL                                       *EMETTE SUBFILE
021500000000     C*
021600000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTO
021700000000     C*
021800000000IF  1C     $ERR          IFEQ      '0'
021900000000SL  2C                   SELECT
022000000000     C     KEY           WHENEQ    F12
022100000000     C                   EXSR      F12GES
022200000000     C                   OTHER
022300000000     C                   EXSR      FUNSFL                                       *FUNZIONI STND SUBFI
022400000000E   2C                   ENDSL
022500000000E   1C                   ENDIF
022600000000     C*
022700000000     C                   ENDSR
022800000000     C*------------------------------------------------------------------------*
022900000000     C* F12GES -GEST.TASTO FUNZ. F12 ===> PRECEDENTE
023000000000     C*------------------------------------------------------------------------*
023100000000     C     F12GES        BEGSR
023200000000     C*
023300000000     C                   MOVEL     *BLANKS       D01OP1
023400000000     C                   MOVEL     '1'           $FINE
023500000000     C*
023600000000     C                   ENDSR
023700000000     C*------------------------------------------------------------------------*
023800000000     C*  GESOPZ - GESTIONE OPZIONI
023900000000     C*------------------------------------------------------------------------*
024000000000     C     GESOPZ        BEGSR
024100000000     C*
024200000000SL  1C                   SELECT
024300000000     C*********  VSCOPZ    WHEQ ' 2'
024400000000     C*********            EXSR O02GES                     *MODIFICA
024500000000     C                   OTHER
024600000000     C                   MOVEL     MSG(4)        $MSG                           *OPZ.ABIL.MA NON GES
024700000000     C                   EXSR      CARMSG                                       *CARICA SFL MESSAGGI
024800000000E   1C                   ENDSL
024900000000     C*
025000000000     C                   ENDSR
025100000000     C*------------------------------------------------------------------------*
025200000000     C*  O02GES - GESTIONE OPZIONE 2=MODIFICA
025300000000     C*------------------------------------------------------------------------*
025400000000     C*********  O02GES    BEGSR
025500000000     C*
025600000000     C*********            ENDSR
025700000000     C*------------------------------------------------------------------------*
025800000000     C* NEWPOS - CONTROLLO DELLA RICHIESTA DI NUOVO POSIZIONAMENTO
025900000000     C*------------------------------------------------------------------------*
026000000000     C     NEWPOS        BEGSR
026100000000     C*
026200000000     C* SWPOSI 0 = NESSUNA RICHIESTA DI POSIZIONAMENTO
026300000000     C*        1 = RICHIESTO POSIZIONAMENTO
026400000000     C                   Z-ADD     0             SWPOSI            7 0
026500000000     C*
026600000000     C* SWMODI 0 = NESSUN AGGIORNAMENTO AFFETTUATO DALL'UTENTE CON L'OPZIONE 2,
026700000000     C*            NON OCCORRE RICARICARE IL SUBFILE
026800000000     C*        1 = AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
026900000000     C                   Z-ADD     0             SWMODI            1 0
027000000000     C*
027100000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO
027200000000     C*----------------
027300000000     C* N.B.:  PER COMODITA' HO LASCIATO L'IMPOSTAZIONE CLASSICA DEL
027400000000     C*        PROGRAMMA, CIOE' CON I CAMPI POSIZIONAMENTO DI I/O NEL
027500000000     C*        SFLCONTROL (QUINDI CON I CAMPI "DEPOSITO").
027600000000     C*        ORA TALI CAMPI NON VI SONO A VIDEO MA METTENDOLI,
027700000000     C*        IL PGM E' GIA' PREDISPOSTO.
027800000000     C*----------------
027900000000IF  1C     VCCAAS        IFNE      DEPAAS                                       *ANNO
028000000000     C*
028100000000     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI
028200000000     C                   EXSR      CHKPAR
028300000000     C*
028400000000     C* SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
028500000000IF  2C     $ERR          IFEQ      '0'
028600000000     C                   Z-ADD     1             SWPOSI                         *RICHIESTO POSIZ.TO
028700000000     C                   Z-ADD     VCCAAS        DEPAAS                         *SALVATO NEW POSIZ.T
028800000000E   2C                   ENDIF
028900000000E   1C                   ENDIF
029000000000     C*
029100000000     C                   ENDSR
029200000000     C*------------------------------------------------------------------------*
029300000000     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
029400000000     C*------------------------------------------------------------------------*
029500000000     C     CHKPAR        BEGSR
029600000000     C*
029700000000     C* REIMPOSTO INDICATORI DI ERRORE
029800000000     C                   SETOFF                                         81
029900000000     C*
030000000000     C* SE C'E' UN ERRORE NON EMETTO IL SUBFILE PER FARE POSIZIONARE IL CURSORE
030100000000     C* SUL CAMPO DEL CONTROL
030200000000IF  1C     $ERR          IFEQ      '1'
030300000000     C                   MOVEL     '0'           *IN43                          *NON EMETTO SFL
030400000000X   1C                   ELSE
030500000000     C                   MOVEL     '1'           *IN43                          *EMETTO SFL
030600000000E   1C                   ENDIF
030700000000     C*
030800000000     C                   ENDSR
030900000000     C*------------------------------------------------------------------------*
031000000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
031100000000     C*------------------------------------------------------------------------*
031200000000     C     SETFIL        BEGSR
031300000000     C*
031400000000     C                   MOVEL     '1'           *IN43                          *EMETTO SFILE
031500000000     C*
031600000000     C* POSIZIONAMENTO ARCHIVIO (SCHIERE PRECEDENTEMENTE CARICATE)
031700000000     C                   Z-ADD     *ZEROS        J
031800000000     C                   EXSR      LETFIL
031900000000     C*
032000000000     C*---                 Z-ADD1         J
032100000000IF  1C*---       IMP,J     IFGT *ZEROS
032200000000     C*---                 EXSR LETFIL
032300000000X   1C*---                 ELSE                            *FINE FILE
032400000000     C*---                 Z-ADD1         SFLEOF
032500000000E   1C*---                 ENDIF
032600000000     C*
032700000000     C                   ENDSR
032800000000     C*------------------------------------------------------------------------*
032900000000     C* LETFIL - LEGGE RECORD SUCESSIVO
033000000000     C*------------------------------------------------------------------------*
033100000000     C     LETFIL        BEGSR
033200000000     C*
033300000000     C                   ADD       1             J                              *INDICE SCHIERE VIDE
033400000000     C*
033500000000     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
033600000000IF  1C*---       J         IFGT 100                        LUNGHEZZA MAX SCHIERE
033700000000     C     IMP(J)        IFEQ      *ZEROS                                       *SCHIERA VUOTA
033800000000     C                   Z-ADD     1             SFLEOF
033900000000E   1C                   ENDIF
034000000000     C*
034100000000     C                   ENDSR
034200000000     C*------------------------------------------------------------------------*
034300000000     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO
034400000000     C*------------------------------------------------------------------------*
034500000000     C     IRESFL        BEGSR
034600000000     C*
034700000000     C                   Z-ADD     CMP(J)        VSCCMP
034800000000     C                   MOVEL     CMD(J)        VSCCMD
034900000000     C                   Z-ADD     IMP(J)        VSCIMP
035000000000     C*
035100000000     C                   ENDSR
035200000000     C*------------------------------------------------------------------------*
035300000000     C* CARCMP - CARICA TUTTE LE COMPETENZE DELLA BOLLA IN SCHIERA
035400000000     C*------------------------------------------------------------------------*
035500000000     C     CARCMP        BEGSR
035600000000     C*
035700000000     C     *LIKE         DEFINE    CEDIMP        TOTIMP
035800000000     C                   Z-ADD     *ZEROS        J                 3 0
035900000000     C*
036000000000     C* LEGGE LE COMPETENZE FISSE
036100000000     C                   Z-ADD     VCCAAS        KCTAAS                         *ANNO
036200000000     C                   Z-ADD     VCCLNP        KCTLNP                         *LINEA DI PARTENZA
036300000000     C                   Z-ADD     VCCNRS        KCTNRS                         *SERIE
036400000000     C                   Z-ADD     VCCNSP        KCTNSP                         *SPEDIZIONE
036500000000     C                   MOVEL     VCCTBL        KCTTBL                         *TIPO BOLLA
036600000000     C     KEYCET        CHAIN     ECCET30C                           99
036700000000IF  1C     *IN99         IFEQ      *OFF                                         *TROVATE
036800000000     C*
036900000000     C* CARICA LE COMPETENZE FISSE
037000000000IF  2C     CETPAR        IFGT      *ZEROS
037100000000     C                   ADD       1             J
037200000000     C                   Z-ADD     1             CMP(J)                         *CODICE  COMPETENZA
037300000000     C                   Z-ADD     CETPAR        IMP(J)                         *IMPORTO COMPETENZA
037400000000E   2C                   ENDIF
037500000000     C*
037600000000IF  2C     CETPAM        IFGT      *ZEROS
037700000000     C                   ADD       1             J
037800000000     C                   Z-ADD     2             CMP(J)                         *CODICE  COMPETENZA
037900000000     C                   Z-ADD     CETPAM        IMP(J)                         *IMPORTO COMPETENZA
038000000000E   2C                   ENDIF
038100000000     C*
038200000000IF  2C     CETPAU        IFGT      *ZEROS
038300000000     C                   ADD       1             J
038400000000     C                   Z-ADD     3             CMP(J)                         *CODICE  COMPETENZA
038500000000     C                   Z-ADD     CETPAU        IMP(J)                         *IMPORTO COMPETENZA
038600000000E   2C                   ENDIF
038700000000     C*
038800000000IF  2C     CETPAV        IFGT      *ZEROS
038900000000     C                   ADD       1             J
039000000000     C                   Z-ADD     4             CMP(J)                         *CODICE  COMPETENZA
039100000000     C                   Z-ADD     CETPAV        IMP(J)                         *IMPORTO COMPETENZA
039200000000E   2C                   ENDIF
039300000000     C*
039400000000IF  2C     CETARD        IFGT      *ZEROS
039500000000     C                   ADD       1             J
039600000000     C                   Z-ADD     5             CMP(J)                         *CODICE  COMPETENZA
039700000000     C                   Z-ADD     CETARD        IMP(J)                         *IMPORTO COMPETENZA
039800000000E   2C                   ENDIF
039900000000     C*
040000000000IF  2C     CETARM        IFGT      *ZEROS
040100000000     C                   ADD       1             J
040200000000     C                   Z-ADD     6             CMP(J)                         *CODICE  COMPETENZA
040300000000     C                   Z-ADD     CETARM        IMP(J)                         *IMPORTO COMPETENZA
040400000000E   2C                   ENDIF
040500000000     C*
040600000000IF  2C     CETARU        IFGT      *ZEROS
040700000000     C                   ADD       1             J
040800000000     C                   Z-ADD     7             CMP(J)                         *CODICE  COMPETENZA
040900000000     C                   Z-ADD     CETARU        IMP(J)                         *IMPORTO COMPETENZA
041000000000E   2C                   ENDIF
041100000000     C*
041200000000IF  2C     CETARV        IFGT      *ZEROS
041300000000     C                   ADD       1             J
041400000000     C                   Z-ADD     8             CMP(J)                         *CODICE  COMPETENZA
041500000000     C                   Z-ADD     CETARV        IMP(J)                         *IMPORTO COMPETENZA
041600000000E   2C                   ENDIF
041700000000     C*
041800000000IF  2C     CETTRL        IFGT      *ZEROS
041900000000     C                   ADD       1             J
042000000000     C                   Z-ADD     13            CMP(J)                         *CODICE  COMPETENZA
042100000000     C                   Z-ADD     CETTRL        IMP(J)                         *IMPORTO COMPETENZA
042200000000E   2C                   ENDIF
042300000000     C*
042400000000IF  2C     CETPAG        IFGT      *ZEROS
042500000000     C                   ADD       1             J
042600000000     C                   Z-ADD     29            CMP(J)                         *CODICE  COMPETENZA
042700000000     C                   Z-ADD     CETPAG        IMP(J)                         *IMPORTO COMPETENZA
042800000000E   2C                   ENDIF
042900000000     C*
043000000000IF  2C     CETSEA        IFGT      *ZEROS
043100000000     C                   ADD       1             J
043200000000     C                   Z-ADD     26            CMP(J)                         *CODICE  COMPETENZA
043300000000     C                   Z-ADD     CETSEA        IMP(J)                         *IMPORTO COMPETENZA
043400000000E   2C                   ENDIF
043500000000     C*
043600000000IF  2C     CETSEC        IFGT      *ZEROS
043700000000     C                   ADD       1             J
043800000000     C                   Z-ADD     25            CMP(J)                         *CODICE  COMPETENZA
043900000000     C                   Z-ADD     CETSEC        IMP(J)                         *IMPORTO COMPETENZA
044000000000E   2C                   ENDIF
044100000000     C*
044200000000IF  2C     CETRCV        IFGT      *ZEROS
044300000000     C                   ADD       1             J
044400000000     C                   Z-ADD     27            CMP(J)                         *CODICE  COMPETENZA
044500000000     C                   Z-ADD     CETRCV        IMP(J)                         *IMPORTO COMPETENZA
044600000000E   2C                   ENDIF
044700000000     C*
044800000000IF  2C     CETMDE        IFGT      *ZEROS
044900000000     C                   ADD       1             J
045000000000     C                   Z-ADD     10            CMP(J)                         *CODICE  COMPETENZA
045100000000     C                   Z-ADD     CETMDE        IMP(J)                         *IMPORTO COMPETENZA
045200000000E   2C                   ENDIF
045300000000     C*
045400000000IF  2C     CETTDE        IFGT      *ZEROS
045500000000     C                   ADD       1             J
045600000000     C                   Z-ADD     15            CMP(J)                         *CODICE  COMPETENZA
045700000000     C                   Z-ADD     CETTDE        IMP(J)                         *IMPORTO COMPETENZA
045800000000E   2C                   ENDIF
045900000000     C*
046000000000IF  2C     CETMAF        IFGT      *ZEROS
046100000000     C                   ADD       1             J
046200000000     C                   Z-ADD     9             CMP(J)                         *CODICE  COMPETENZA
046300000000     C                   Z-ADD     CETMAF        IMP(J)                         *IMPORTO COMPETENZA
046400000000E   2C                   ENDIF
046500000000     C*
046600000000IF  2C     CETTAF        IFGT      *ZEROS
046700000000     C                   ADD       1             J
046800000000     C                   Z-ADD     14            CMP(J)                         *CODICE  COMPETENZA
046900000000     C                   Z-ADD     CETTAF        IMP(J)                         *IMPORTO COMPETENZA
047000000000E   2C                   ENDIF
047100010829     C*
047200010829IF  2C     CETSES        IFGT      *ZEROS
047300010829     C                   ADD       1             J
047400010829     C                   Z-ADD     33            CMP(J)                         *CODICE  COMPETENZA
047500010829     C                   Z-ADD     CETSES        IMP(J)                         *IMPORTO COMPETENZA
047600010829E   2C                   ENDIF
047700000000     C*
047800000000     C* LEGGE LE COMPETENZE VARIABILI
047900000000IF  2C     CETDET        IFEQ      'S'                                          *SE CI SONO
048000000000     C                   Z-ADD     VCCAAS        KCDAAS                         *ANNO
048100000000     C                   Z-ADD     VCCLNP        KCDLNP                         *LINEA DI PARTENZA
048200000000     C                   Z-ADD     VCCNRS        KCDNRS                         *SERIE
048300000000     C                   Z-ADD     VCCNSP        KCDNSP                         *SPEDIZIONE
048400000000     C                   MOVEL     VCCTBL        KCDTBL                         *TIPO BOLLA
048500000000     C     KEYCED        SETLL     ECCED30C
048600000000     C     KEYCED        READE     ECCED30C                               99
048700000000DO  3C     *IN99         DOWEQ     *OFF
048800000000     C*
048900000000     C* CARICA LE COMPETENZE VARIABILI
049000000000IF  4C     CEDIMP        IFGT      *ZEROS
049100000000     C                   ADD       1             J
049200000000     C                   Z-ADD     CEDCMP        CMP(J)                         *CODICE  COMPETENZA
049300000000     C                   Z-ADD     CEDIMP        IMP(J)                         *IMPORTO COMPETENZA
049400000000E   4C                   ENDIF
049500000000     C     KEYCED        READE     ECCED30C                               99
049600000000E   3C                   ENDDO
049700000000E   2C                   ENDIF
049800000000     C*
049900000000     C* CALCOLA IL TOTALE COMPETENZE
050000000000     C                   XFOOT     IMP           TOTIMP
050100000000     C                   ADD       1             J
050200000000     C                   Z-ADD     *ZEROS        CMP(J)                         *CODICE  COMPETENZA
050300000000     C                   Z-ADD     TOTIMP        IMP(J)                         *IMPORTO COMPETENZA
050400000000     C                   MOVEL     TODIMP        CMD(J)                         *DESCRIZ.COMPETENZA
050500000000E   1C                   ENDIF
050600000000     C*
050700000000     C* DECODIFICA LE COMPETENZE (NON L'ULTIMO ELEMENTO CHE E' IL TOTALE)
050800000000DO  1C     1             DO        100           J
050900000000IF  2C     IMP(J)        IFGT      *ZEROS
051000000000     C     CMP(J)        ANDGT     *ZEROS
051100000000     C                   Z-ADD     CETNBC        WNBC
051200000000     C                   Z-ADD     CMP(J)        WCMP
051300000000     C                   MOVEL     *BLANKS       WCMD
051400000000     C                   EXSR      CHKCMP
051500000000     C                   MOVEL     WCMD          CMD(J)
051600000000E   2C                   ENDIF
051700000000E   1C                   ENDDO
051800000000     C*
051900000000     C                   ENDSR
052000000000     C*------------------------------------------------------------------------*
052100000000     C* CHKCMP - DECODIFICA LE COMPETENZE
052200000000     C*------------------------------------------------------------------------*
052300000000     C     CHKCMP        BEGSR
052400000000     C*
052500000000     C     *LIKE         DEFINE    ECTNBC        WNBC
052600000000     C     *LIKE         DEFINE    ECTCMP        WCMP
052700000000     C     *LIKE         DEFINE    ECTCMD        WCMD
052800000000     C*
052900000000     C* LEGGE, DEL BLOCCO, L'ULTIMA COMPETENZA IN DECORRENZA
053000000000     C                   Z-ADD     WNBC          KETNBC                         *NUMERO BLOCCO
053100000000     C                   Z-ADD     WCMP          KETCMP                         *CODICE COMPETENZA
053200000000     C                   Z-ADD     99999999      KETDDC                         *DATA DECORRENZA
053300000000     C     KEYECT        SETLL     ECECT01L
053400000000     C     KE2ECT        READPE    ECECT01L                               99
053500000000IF  1C     *IN99         IFEQ      *OFF
053600000000     C                   MOVEL     ECTCMD        WCMD
053700000000E   1C                   ENDIF
053800000000     C*
053900000000     C                   ENDSR
054000000000     C*------------------------------------------------------------------------*
054100000000     C* LANPGM - LANCIA UN PROGRAMMA
054200000000     C*------------------------------------------------------------------------*
054300000000     C*---       LANPGM    BEGSR
054400000000     C*--------------------
054500000000     C* PARAMETRI INFORMATIVI
054600000000     C*--------------------
054700000000     C*---                 MOVELWOP0      D01OP0           *LIVELLO PROCEDURA
054800000000     C*---                 MOVELWOP1      D01OP1           *LIVELLO PROGRAMMA
054900000000     C*---                 MOVEL'0'       D01F03           *USCITA PGM
055000000000     C*---                 MOVEL'0'       D01F12           *USCITA PGM
055100000000     C*---                 MOVEL'0'       D01ERR           *ERRORE
055200000000     C*---                 MOVEL*BLANKS   D01MSG           *MESSAGGIO ERRORE
055300000000     C*--------------------
055400000000     C* PARAMETRI
055500000000     C*--------------------
055600000000     C*---                 Z-ADDWAAS      D01AAS           *ANNO
055700000000     C*---                 Z-ADDWLNP      D01LNP           *LNP
055800000000     C*---                 Z-ADDWNRS      D01NRS           *SERIE
055900000000     C*---                 Z-ADDWNSP      D01NSP           *SPEDIZIONE
056000000000     C*---                 MOVELWTBL      D01TBL           *TIPO BOLLA
056100000000     C*---                 Z-ADDWDSP      D01DSP           *DATA SPEDIZIONE GMA
056200000000     C*---                 Z-ADDWDTV      D01DTV           *DATA VARIAZIONE AMG
056300000000     C*---                 Z-ADDWORV      D01ORV           *ORA
056400000000     C*---                 MOVELWPRU      D01PRU           *UTENTE
056500000000     C* LANCIO
056600000000     C*---                 MOVELDSSB01    KPJBU
056700000000     C*---                 CALL WPGM                   99
056800000000     C*---                 PARM           KPJBA
056900000000     C*---                 MOVELKPJBU     DSSB01
057000000000     C*---
057100000000     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
057200000000IF  1C*---       D01F03    IFEQ '1'
057300000000     C*---                 MOVEL'1'       $F03
057400000000     C*---                 MOVEL'1'       $ERR
057500000000     C*---                 MOVEL'1'       $FINE
057600000000     C*---
057700000000     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
057800000000X   1C*---                 ELSE
057900000000     C*---                 MOVELD01MSG    $MSG
058000000000     C*---                 EXSR CARMSG
058100000000E   1C*---                 ENDIF
058200000000     C*---
058300000000     C* PREMUTO F12 DAL CHIAMATO
058400000000IF  1C*---       D01F12    IFEQ '1'
058500000000     C*---                 MOVEL'1'       $ERR
058600000000E   1C*---                 ENDIF
058700000000     C*---
058800000000     C*---                 CLEARD01OP1
058900000000     C*---                 MOVEL'0'       D01F12
059000000000     C*---
059100000000     C*---                 ENDSR
059200020920     C*--------------------------------------------------------------------------------------------*
059300020920     C* REPERISCE I DATI UTENTE
059400020920     C*--------------------------------------------------------------------------------------------*
059500020920     C     REPDATIUTE    BEGSR
059600020920     C*
059700020920     C* INIZIALIZZA VARIABILI DI WRK
059800020920     C                   CLEAR                   TIBS34DS
059900020920     C                   CLEAR                   AZUTEDS
060000020920     C                   CLEAR                   DDATIUTE
060100020920     C*
060200020920     C     *DTAARA       DEFINE    §azute        azuteds
060300020920     C     *DTAARA       DEFINE    §datiute      ddatiute
060400020920     C                   IN(E)     *DTAARA
060500020920if  1C                   IF        %Error
060600020920     c                   EVAL      I34Tla = 'L'
060700020920     C                   CALL      'TIBS34R'
060800020920     C                   PARM                    Tibs34Ds
060900020920     C                   IN        *DTAARA
061000020920e   1C                   ENDIF
061100020920     C*
061200020920     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
061300020920     C                   MOVEL(P)  RSUT          DSFIRS           20
061400020920     C*
061500020920     C                   ENDSR
061600000000     C*------------------------------------------------------------------------*
061700000000     C*  *INZSR - ROUTINE INIZIALE
061800000000     C*------------------------------------------------------------------------*
061900000000     C     *INZSR        BEGSR
062000000000     C*--------------------
062100000000     C* RICEVIMENTO PARAMETRI
062200000000     C*--------------------
062300000000     C     *ENTRY        PLIST
062400000000     C                   PARM                    KPJBA
062500000000     C                   MOVEL     KPJBU         DSSB01
062600000000     C*--------------------
062700000000     C* VARIABILI RIFERITE AL DATA BASE
062800000000     C*--------------------
062900000000     C     *LIKE         DEFINE    CETAAS        KCTAAS                         *ECCET30C
063000000000     C     *LIKE         DEFINE    CETLNP        KCTLNP
063100000000     C     *LIKE         DEFINE    CETNRS        KCTNRS
063200000000     C     *LIKE         DEFINE    CETNSP        KCTNSP
063300000000     C     *LIKE         DEFINE    CETTBL        KCTTBL
063400000000     C     *LIKE         DEFINE    CEDAAS        KCDAAS                         *ECCED30C
063500000000     C     *LIKE         DEFINE    CEDLNP        KCDLNP
063600000000     C     *LIKE         DEFINE    CEDNRS        KCDNRS
063700000000     C     *LIKE         DEFINE    CEDNSP        KCDNSP
063800000000     C     *LIKE         DEFINE    CEDTBL        KCDTBL
063900000000     C     *LIKE         DEFINE    ECTNBC        KETNBC                         *ECECT01L
064000000000     C     *LIKE         DEFINE    ECTCMP        KETCMP
064100000000     C     *LIKE         DEFINE    ECTDDC        KETDDC
064200000000     C     *LIKE         DEFINE    D01OP0        WOP0                           *DI LAVORO
064300000000     C     *LIKE         DEFINE    D01OP1        WOP1
064400000000     C     *LIKE         DEFINE    D01AAS        WAAS
064500000000     C     *LIKE         DEFINE    D01LNP        WLNP
064600000000     C     *LIKE         DEFINE    D01NRS        WNRS
064700000000     C     *LIKE         DEFINE    D01NSP        WNSP
064800000000     C     *LIKE         DEFINE    D01TBL        WTBL
064900000000     C     *LIKE         DEFINE    D01DSP        WDSP
065000000000     C     *LIKE         DEFINE    D01DTV        WDTV
065100000000     C     *LIKE         DEFINE    D01ORV        WORV
065200000000     C     *LIKE         DEFINE    D01PRU        WPRU
065300000000     C     *LIKE         DEFINE    VCCAAS        DEPAAS                         *DEPOSITI
065400000000     C*--------------------
065500000000     C* VARIABILI DI CONTROLLO
065600000000     C*--------------------
065700000000     C                   TIME                    ORADAT           14 0          *ORA(6) + DATA(8)
065800000000     C                   MOVEL     ORADAT        ORA               6 0          *ORA
065900000000     C                   MOVE      ORADAT        DATA              8 0          *DATA GG/MM/AAAA
066000000000     C*--------------------
066100000000     C* CHIAVI DI LETTURA
066200000000     C*--------------------
066300000000     C* LETTURA ECCET30C - COMPELTA
066400000000     C     KEYCET        KLIST
066500000000     C                   KFLD                    KCTAAS                         *ANNO
066600000000     C                   KFLD                    KCTLNP                         *LINEA DI PARTENZA
066700000000     C                   KFLD                    KCTNRS                         *SERIE
066800000000     C                   KFLD                    KCTNSP                         *SPEDIZIONE
066900000000     C                   KFLD                    KCTTBL                         *TIPO BOLLA
067000000000     C* LETTURA ECCED30C - COMPELTA
067100000000     C     KEYCED        KLIST
067200000000     C                   KFLD                    KCDAAS                         *ANNO
067300000000     C                   KFLD                    KCDLNP                         *LINEA DI PARTENZA
067400000000     C                   KFLD                    KCDNRS                         *SERIE
067500000000     C                   KFLD                    KCDNSP                         *SPEDIZIONE
067600000000     C                   KFLD                    KCDTBL                         *TIPO BOLLA
067700000000     C* LETTURA ECECT01L - COMPELTA
067800000000     C     KEYECT        KLIST
067900000000     C                   KFLD                    KETNBC                         *NUMERO BLOCCO
068000000000     C                   KFLD                    KETCMP                         *CODICE COMPETENZA
068100000000     C                   KFLD                    KETDDC                         *DATA DECORRENZA
068200000000     C* LETTURA ECECT01L - PARZIALE
068300000000     C     KE2ECT        KLIST
068400000000     C                   KFLD                    KETNBC                         *NUMERO BLOCCO
068500000000     C                   KFLD                    KETCMP                         *CODICE COMPETENZA
068600000000     C*
068700000000     C* V02 --> GESTIONE;  V05 --> INTERROGAZIONE
068800000000     C                   MOVE      D01OP0        INTGES            2
068900000000IF  1C     INTGES        IFEQ      '02'
069000000000     C                   MOVEL     'G'           $TGES             1            *G=GESTIONE
069100000000     C*************        MOVELDESGES    VTDOPE           *DESCR OPERAZIONE
069200000000X   1C                   ELSE
069300000000     C                   MOVEL     'I'           $TGES             1            *I=INTERROGAZIONE
069400000000     C*************        MOVELDESINT    VTDOPE           *DESCR OPERAZIONE
069500000000E   1C                   ENDIF
069600000000     C*
069700000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
069800000000     C                   EXSR      INZSTD
069900000000     C*
070000000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
070100000000     C                   EXSR      INZSFL
070200000000     C*
070300000000     C* IMPOSTO CAMPI TESTATA VIDEO
070400000000     C                   MOVEL     DSFIRS        VTDRSU                         *RAGIONE SOC. UTENTE
070500000000     C*
070600000000     C* IMPOSTO ANCHE I DEPOSITI PER CONFRONTI SUCCESSIVI
070700000000     C                   Z-ADD     D01AAS        VCCAAS                         *ANNO
070800000000     C                   Z-ADD     VCCAAS        DEPAAS                         *DEPOSITO
070900000000     C                   Z-ADD     D01LNP        VCCLNP                         *LINEA DI PARTENZA
071000000000     C                   Z-ADD     D01NRS        VCCNRS                         *SERIE
071100000000     C                   Z-ADD     D01NSP        VCCNSP                         *SPEDIZIONE
071200000000     C                   MOVEL     D01TBL        VCCTBL                         *TIPO BOLLA
071300000000     C                   Z-ADD     D01DSP        VCCDSP                         *DATA SPEDIZIONE
071400000000     C*
071500000000     C* CARICA TUTTE LE COMPETENZE DELLA BOLLA IN SCHIERA
071600000000     C                   EXSR      CARCMP
071700000000     C*
071800000000     C* NUMERO BLOCCO COMPETENZE
071900000000     C                   Z-ADD     CETNBC        VCCNBC
072000000000     C*
072100000000     C* EMETTO VIDEATA SUBFILE
072200000000     C                   EXSR      CARVS                                        CARICA E EMETTE SFIL
072300000000     C*
072400000000     C                   ENDSR
072500000000     C*------------------------------------------------------------------------*
072600000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
072700000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
072800000000     C*    LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
072900000000     C*    STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
073000000000     C*    NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
073100000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
073200000000     C*    §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
073300000000     C*------------------------------------------------------------------------*
073400000000     C* INZSTD - ROUTINE INIZIALE STANDARD
073500000000     C*------------------------------------------------------------------------*
073600000000     C     INZSTD        BEGSR
073700000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
073800000000     C                   CALL      'XKEYS'
073900000000     C                   PARM                    DSKEYS
074000000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
074100000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
074200000000     C     XMSGPA        PLIST
074300000000     C                   PARM                    DSMSMS
074400000000     C                   PARM                    DSMSPG
074500000000     C                   PARM                    DSMSKE
074600000000     C                   PARM                    DSMSER
074700000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
074800000000     C                   MOVEL     'TNSB08R'     DSMSPG                         *NOME PGM CHIAMANTE
074900000000     C                   MOVEL     'TNSB08R'     VMCQUE                         *CODA MESSAGGI DESTI
075000000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
075100000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
075200000000     C* INIZIALIZZO FLAG CONTROLLO PGM
075300000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
075400000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
075500000000     C                   MOVEL     '0'           $F12              1            *PREMUTO TASTO F12
075600000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
075700000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
075800000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
075900000000     C                   MOVEL     '0'           $TESVD            1            *1=RIEMETT.TEST.VIDE
076000000000     C* LANCIO ROUTINE PER VERIFICARE SE
076100000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
076200000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
076300020920     C                   EXSR      REPDATIUTE
076400000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
076500000000     C                   BITOFF    '01234567'    POSMI1            1
076600000000     C                   BITOFF    '01234567'    POSMI2            1
076700000000     C                   BITOFF    '01234567'    POSMA1            1
076800000000     C                   BITOFF    '01234567'    POSMA2            1
076900000000     C                   BITON     '34'          POSMI1            1            *18
077000000000     C                   BITON     '7'           POSMI2            1            *01
077100000000     C                   BITON     '34'          POSMA1            1            *18
077200000000     C                   BITON     '0'           POSMA2            1            *80
077300000000     C                   MOVE      POSMI2        POSMIN            2
077400000000     C                   MOVEL     POSMI1        POSMIN
077500000000     C                   MOVE      POSMA2        POSMAX            2
077600000000     C                   MOVEL     POSMA1        POSMAX
077700000000     C                   ENDSR
077800000000     C*------------------------------------------------------------------------*
077900000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
078000000000     C*------------------------------------------------------------------------*
078100000000     C     FUNSTD        BEGSR
078200000000     C                   SELECT
078300000000     C     KEY           WHENEQ    F3
078400000000     C                   EXSR      F03STD
078500000000     C     KEY           WHENEQ    F12
078600000000     C                   EXSR      F12STD
078700000000     C     KEY           WHENEQ    F24
078800000000     C                   EXSR      F24STD
078900000000     C                   OTHER
079000000000     C                   MOVEL     MSG(2)        $MSG
079100000000     C                   EXSR      CARMSG
079200000000     C                   ENDSL
079300000000     C                   ENDSR
079400000000     C*------------------------------------------------------------------------*
079500000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
079600000000     C*------------------------------------------------------------------------*
079700000000     C     F03STD        BEGSR
079800000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PROCEDURA)
079900000000     C                   MOVEL     '1'           $F03
080000000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
080100000000     C                   MOVEL     '1'           $FINE
080200000000     C                   ENDSR
080300000000     C*------------------------------------------------------------------------*
080400000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
080500000000     C*------------------------------------------------------------------------*
080600000000     C     F12STD        BEGSR
080700000000     C* IMPOSTO FLAG DI PREMUTO TASTO F12 (A LIVELLO PROCEDURA)
080800000000     C                   MOVEL     '1'           $F12
080900000000     C* IMPOSTO FLAG DI FIN PGM
081000000000     C                   MOVEL     '1'           $FINE
081100000000     C                   ENDSR
081200000000     C*------------------------------------------------------------------------*
081300000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
081400000000     C*------------------------------------------------------------------------*
081500000000     C     F24STD        BEGSR
081600000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
081700000000     C                   EXSR      OUTFUN
081800000000     C                   ENDSR
081900000000     C*------------------------------------------------------------------------*
082000000000     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
082100000000     C*------------------------------------------------------------------------*
082200000000     C     INZSFL        BEGSR
082300000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
082400000000     C     *LIKE         DEFINE    VCCNBR        SFLPAG
082500000000     C     *LIKE         DEFINE    VCCNBR        SFLEOF
082600000000     C     *LIKE         DEFINE    VCCNBR        SFLERR
082700000000     C     *LIKE         DEFINE    VCCNBR        SFLULT
082800000000     C     *LIKE         DEFINE    VCCNBR        WFLULT
082900000000     C     *LIKE         DEFINE    VCCNBR        SFLRIG
083000000000     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
083100000000     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
083200000000     C                   Z-ADD     16            SFLPAG
083300000000     C                   ENDSR
083400000000     C*------------------------------------------------------------------------*
083500000000     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
083600000000     C*------------------------------------------------------------------------*
083700000000     C     FUNSFL        BEGSR
083800000000     C                   SELECT
083900000000     C     KEY           WHENEQ    ENTER
084000000000     C                   EXSR      ENTSFL
084100000000     C     KEY           WHENEQ    F3
084200000000     C                   EXSR      F03STD
084300000000     C     KEY           WHENEQ    F5
084400000000     C                   EXSR      F05STD
084500140530     C     KEY           WHENEQ    F9
084600140530     C                   EXSR      F09STD
084700000000     C     KEY           WHENEQ    F12
084800000000     C                   EXSR      F12STD
084900000000     C     KEY           WHENEQ    F17
085000000000     C                   EXSR      F17STD
085100000000     C     KEY           WHENEQ    F18
085200000000     C                   EXSR      F18STD
085300000000     C     KEY           WHENEQ    F23
085400000000     C                   EXSR      F23STD
085500000000     C     KEY           WHENEQ    F24
085600000000     C                   EXSR      F24STD
085700000000     C     KEY           WHENEQ    ROLLUP
085800000000     C                   EXSR      RUPSTD
085900000000     C     KEY           WHENEQ    ROLLDN
086000000000     C                   EXSR      RDNSTD
086100000000     C                   OTHER
086200000000     C                   MOVEL     MSG(2)        $MSG
086300000000     C                   EXSR      CARMSG
086400000000     C                   ENDSL
086500000000     C                   ENDSR
086600000000     C*------------------------------------------------------------------------*
086700000000     C* F05STD -GEST.TASTO FUNZ. F05 STANDARD ===> RIVISUALIZZAZIONE
086800000000     C*------------------------------------------------------------------------*
086900000000     C     F05STD        BEGSR
087000000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
087100000000     C                   EXSR      POSSFL
087200000000     C                   ENDSR
087300140530     C*------------------------------------------------------------------------*
087400140530     C* F09STD -GEST.TASTO FUNZ. F09 STANDARD ===> visualizza WIND con VOCI
087500140530     C*------------------------------------------------------------------------*
087600140530     C     F09STD        BEGSR
087700140530     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
087800140530     C                   EXSR      VOCI_WND
087900140530     C                   ENDSR
088000000000     C*------------------------------------------------------------------------*
088100000000     C* F17STD -GEST.TASTO FUNZ. F17 STANDARD ===> INIZIO DATI
088200000000     C*------------------------------------------------------------------------*
088300000000     C     F17STD        BEGSR
088400000000     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
088500000000     C                   Z-ADD     1             VCCNBR
088600000000     C                   ENDSR
088700000000     C*------------------------------------------------------------------------*
088800000000     C* F18STD -GEST.TASTO FUNZ. F18 STANDARD ===> FINE DATI
088900000000     C*------------------------------------------------------------------------*
089000000000     C     F18STD        BEGSR
089100000000     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
089200000000     C                   Z-ADD     SFLULT        VCCNBR
089300000000     C                   ENDSR
089400000000     C*------------------------------------------------------------------------*
089500000000     C* F23STD -GEST.TASTO FUNZ. F23 STANDARD ===> ALTRE OPZIONI
089600000000     C*------------------------------------------------------------------------*
089700000000     C     F23STD        BEGSR
089800000000     C* GESTIONE VISUALIZZAZIONE OPZIONI
089900000000     C                   EXSR      OUTOPZ
090000000000     C                   ENDSR
090100000000     C*------------------------------------------------------------------------*
090200140530     C* VOCI_WND- Apre una nuova FINSETRA do esplode le singole VOCI
090300000000     C*------------------------------------------------------------------------*
090400140530     C     VOCI_WND      BEGSR
090500140530      **
090600140530     c                   movel     kpjbu         svkpjbu
090700140530     c                   clear                   TNSB08DS1
090800140530     c                   eval      D08AAS  =  VCCAAS
090900140530     c                   eval      D08LNP  =  VCCLNP
091000140530     c                   eval      D08NRS  =  VCCNRS
091100140530     c                   eval      D08NSP  =  VCCNSP
091200140530     c                   eval      D08TBL  =  VCCTBL
091300140530     C*
091400140530     C                   Z-ADD     VCCDSP        G02DAT
091500140530     C                   MOVEL     *BLANK        G02ERR
091600140530     C                   CALL      'XSRDA8'
091700140530     C                   PARM                    WLBDAT
091800140530     C* UDATE A 8 IN AAAA/MM/GG
091900140530     C                   move      G02INV        D08DSP
092000140530      **
092100140530     c                   movel     TNSB08DS1     kpjbu
092200140530     c                   call      'TNSB08R1'
092300140530     c                   parm                    kpjba
092400140530     c                   movel     svkpjbu       kpjbu
092500140530      **
092600140530     C                   ENDSR
092700140530     C*------------------------------------------------------------------------*
092800140530     C* RUPSTD -GEST.TASTO FUNZ. ROLLUP STANDRAD ===> PAGINA SUCCESSIVA
092900140530     C*------------------------------------------------------------------------*
093000140530     C     RUPSTD        BEGSR
093100000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLUP
093200000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLUP SUL SUBFILE DATI
093300000000     C     CURSOR        IFGE      POSMIN                                       *POSIZ.INIZIO RIGA 2
093400000000     C     CURSOR        ANDLE     POSMAX                                       *POSIZ.FINE RIGA 24
093500000000     C                   ELSE
093600000000     C                   EXSR      CARSFL                                       *CARICA SFL DAL FILE
093700000000     C                   ENDIF
093800000000     C                   ENDSR
093900000000     C*------------------------------------------------------------------------*
094000000000     C* RDNSTD -GEST.TASTO FUNZ. ROLLDOWN STANDRAD ===> PAGINA PRECEDENTE
094100000000     C*------------------------------------------------------------------------*
094200000000     C     RDNSTD        BEGSR
094300000000     C* SE IL CURSORE E' SULLA RIGA 24 SIGNIFICA CHE E' RICHIESTO IL ROLLDOWN
094400000000     C* SUL SUBFILE MESSAGGI QUINDI NON ESEGUO RULLDOWN SUL SUBFILE DATI
094500000000     C     CURSOR        IFGE      POSMIN
094600000000     C     CURSOR        ANDLE     POSMAX
094700000000     C                   ELSE
094800000000     C* INVIO MESSAGGIO DI INIZIO ELENCO
094900000000     C                   MOVEL     '1'           $ERR
095000000000     C                   MOVEL     MSG(5)        $MSG
095100000000     C                   EXSR      CARMSG
095200000000     C                   ENDIF
095300000000     C                   ENDSR
095400000000     C*------------------------------------------------------------------------*
095500000000     C* ENTSFL - GEST.TASTO FUNZ. ENTER CON SUBFILE ===> INVIO
095600000000     C*------------------------------------------------------------------------*
095700000000     C     ENTSFL        BEGSR
095800000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
095900000000     C                   EXSR      NEWPOS
096000000000     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
096100000000     C     $ERR          IFEQ      '0'                                          --- 1 -->
096200000000     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
096300000000     C     SWPOSI        IFNE      0                                            --- 2 -->
096400000000     C                   EXSR      POSSFL
096500000000     C                   ELSE                                                   <-- 2 -->
096600000000     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
096700000000     C     SFLULT        IFNE      0                                            --- 3 -->
096800000000     C                   EXSR      OPZSFL
096900000000     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
097000000000     C     SWMODI        IFNE      0                                            --- 4 -->
097100000000     C                   EXSR      POSSFL
097200000000     C                   ENDIF                                                  <-- 4 ---
097300000000     C                   ENDIF                                                  <-- 3 ---
097400000000     C                   ENDIF                                                  <-- 2 ---
097500000000     C                   ENDIF                                                  <-- 1 ---
097600000000     C                   ENDSR
097700000000     C*------------------------------------------------------------------------*
097800000000     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
097900000000     C*------------------------------------------------------------------------*
098000000000     C     POSSFL        BEGSR
098100000000     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
098200000000     C                   EXSR      CLRSFL
098300000000     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
098400000000     C                   EXSR      SETFIL
098500000000     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
098600000000     C     SFLEOF        IFEQ      0
098700000000     C                   EXSR      CARSFL
098800000000     C                   ENDIF
098900000000     C                   ENDSR
099000000000     C*------------------------------------------------------------------------*
099100000000     C* OPZSFL - GESTIONE OPZIONE SFL CON TASTO FUNZIONALE ENTER
099200000000     C*------------------------------------------------------------------------*
099300000000     C     OPZSFL        BEGSR
099400000000     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: N=ERRORE S=OK)
099500000000     C                   MOVEL     'S'           OPZOK             1
099600000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
099700000000     C                   Z-ADD     0             SFLVAR            5 0
099800000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
099900000000     C                   Z-ADD     0             SFLERR
100000000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
100100000000     C                   Z-ADD     SFLULT        WFLULT
100200000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
100300000000     C                   READC     SB08DVS                                49
100400000000     C     *IN49         DOWEQ     '0'                                          --- 1 -->
100500000000     C     $ERR          ANDEQ     '0'
100600000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
100700000000     C                   ADD       1             SFLVAR
100800000000     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: N=ERRORE S=OK)
100900000000     C     OPZOK         IFEQ      'S'                                          --- 2 -->
101000000000     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: N=ERRORE S=OK)
101100000000     C     VSCOPZ        IFNE      *BLANKS                                      --- 3 -->
101200000000     C                   EXSR      CONOPZ
101300000000     C     OPZOK         IFEQ      'S'
101400000000     C     $ERR          ANDEQ     '0'
101500000000     C                   EXSR      GESOPZ
101600000000     C                   ENDIF
101700000000     C                   ENDIF                                                  <-- 3 ---
101800000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
101900000000     C*            2. PULIZIA SCELTA IN SFL
102000000000     C     OPZOK         IFEQ      'S'                                          --- 3 -->
102100000000     C                   MOVEL     '0'           *IN41
102200000000     C                   MOVEL     *BLANKS       VSCOPZ
102300000000     C                   ELSE                                                   <-- 3 -->
102400000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
102500000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
102600000000     C                   MOVEL     '1'           *IN41
102700000000     C                   Z-ADD     SFLULT        SFLERR
102800000000     C                   ENDIF                                                  <-- 3 ---
102900000000     C                   ELSE                                                   <-- 2 -->
103000000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
103100000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
103200000000     C                   MOVEL     '1'           *IN41
103300000000     C                   ENDIF                                                  <-- 2 ---
103400000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
103500000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
103600000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
103700000000     C     OPZOK         IFNE      'S'
103800000000     C                   Z-ADD     SFLERR        VCCNBR
103900000000     C                   ELSE
104000000000     C                   Z-ADD     SFLULT        VCCNBR
104100000000     C                   ENDIF
104200000000     C* AGGIORNO RECORD SFL
104300000000     C                   UPDATE    SB08DVS
104400000000     C     $ERR          IFEQ      '0'
104500000000     C                   READC     SB08DVS                                49
104600000000     C                   ENDIF
104700000000     C                   ENDDO                                                  <-- 1 ---
104800000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
104900000000     C                   Z-ADD     WFLULT        SFLULT
105000000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
105100000000     C     SFLVAR        IFEQ      0
105200000000     C                   MOVEL     '1'           $FINE
105300000000     C                   ENDIF
105400000000     C                   ENDSR
105500000000     C*------------------------------------------------------------------------*
105600000000     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
105700000000     C*------------------------------------------------------------------------*
105800000000     C     CLRSFL        BEGSR
105900000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
106000000000     C* (0=NON FINE FILE  1=FINE FILE)
106100000000     C                   Z-ADD     0             SFLEOF
106200000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
106300000000     C                   Z-ADD     0             SFLULT
106400000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
106500000000     C                   MOVEL     '1'           *IN40
106600000000     C                   WRITE     SB08DVC
106700000000     C                   MOVEL     '0'           *IN40
106800000000     C                   ENDSR
106900000000     C*------------------------------------------------------------------------*
107000000000     C* CARSFL - CARICAMENTO SFL
107100000000     C*------------------------------------------------------------------------*
107200000000     C     CARSFL        BEGSR
107300000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
107400000000     C     SFLULT        IFEQ      0
107500000000     C                   EXSR      SETFIL
107600000000     C                   ENDIF
107700000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
107800000000     C                   Z-ADD     SFLULT        WFLULT
107900000000     C* CICLO:
108000000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
108100000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
108200000000     C                   Z-ADD     1             SFLRIG
108300000000     C     SFLRIG        DOWLE     SFLPAG                                       --- 1 -->
108400000000     C     SFLEOF        ANDEQ     0
108500000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
108600000000     C                   MOVEL     *BLANKS       VSCOPZ
108700000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
108800000000     C                   EXSR      IRESFL
108900000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
109000000000     C                   ADD       1             SFLULT
109100000000     C* SCRITTURA RECORD SFL
109200000000     C                   WRITE     SB08DVS
109300000000     C                   ADD       1             SFLRIG
109400000000     C* LETTURA FILE DA CARICARE
109500000000     C                   EXSR      LETFIL
109600000000     C                   ENDDO                                                  <-- 1 ---
109700000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
109800000000     C     WFLULT        IFNE      SFLULT
109900000000     C                   Z-ADD     WFLULT        VCCNBR
110000000000     C                   ADD       1             VCCNBR
110100000000     C                   ENDIF
110200000000     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
110300000000     C     SFLEOF        IFEQ      1                                            --- 1 -->
110400000000     C     SFLULT        ANDGT     *ZEROS
110500000000     C                   MOVEL     '1'           $ERR
110600000000     C                   MOVEL     MSG(6)        $MSG
110700000000     C                   EXSR      CARMSG
110800000000     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
110900000000     C******     SFLULT    CHAINSB08DVS              99
111000000000     C******     *IN99     IFEQ '0'
111100000000     C******               SETON                     45
111200000000     C******               UPDATSB08DVS
111300000000     C******               SETOF                     45
111400000000     C******               ENDIF
111500000000     C                   ENDIF                                                  <-- 1 ---
111600000000     C                   ENDSR
111700000000     C*------------------------------------------------------------------------*
111800000000     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
111900000000     C*------------------------------------------------------------------------*
112000000000     C     OUTSFL        BEGSR
112100000000     C* SE IL N.DI MESSAGGIO E' > 0 LANCIO ROUTINE VISUALIZZAZIONE MESSAGGIO
112200000000     C     MSGULT        IFNE      *ZEROS
112300000000     C                   EXSR      OUTMSG
112400000000     C                   ENDIF
112500000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
112600000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
112700000000     C     SFLEOF        IFEQ      0
112800000000     C                   MOVEL     '0'           *IN42
112900000000     C                   ELSE
113000000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
113100000000     C                   MOVEL     '1'           *IN42
113200000000     C                   ENDIF
113300000000     C* EMISSIONE SUBFILE
113400000000     C     SFLULT        IFLE      0
113500000000     C*  --> CASO DI SFL VUOTO
113600000000     C                   MOVEL     '0'           *IN43                          *NON VISUALIZZA SFL
113700000000     C                   WRITE     SB08DVV                                      *'NON VI SONO DATI'
113800000000     C                   WRITE     SB08DVC                                      *SFL CONTROL
113900000000     C                   READ      SB08DVC                                99    *SFL CONTROL
114000000000     C                   ELSE
114100000000     C*  --> CASO DI SFL CON RECORDS
114200000000     C                   WRITE     SB08DVC                                      *SFL CONTROL
114300000000     C                   READ      SB08DVC                                99    *SFL CONTROL
114400000000     C                   ENDIF
114500000000     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
114600000000     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
114700000000     C                   Z-ADD     FRTSFL        VCCNBR
114800000000     C                   ENDSR
114900000000     C*------------------------------------------------------------------------*
115000000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
115100000000     C*          CODA MESSAGGI DEL PGM
115200000000     C*------------------------------------------------------------------------*
115300000000     C     CARMSG        BEGSR
115400000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
115500000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
115600000000     C     MSGULT        IFEQ      0
115700000000     C                   MOVEL     '1'           *IN40
115800000000     C                   WRITE     SB08DVK
115900000000     C                   MOVEL     '0'           *IN40
116000000000     C                   ENDIF
116100000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
116200000000     C                   MOVEL     $MSG          DSMSMS
116300000000     C                   CALL      'XMSG'        XMSGPA
116400000000     C     DSMSER        IFEQ      0
116500000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
116600000000     C                   ENDIF
116700000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
116800000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
116900000000     C                   WRITE     SB08DVM                                      *SFL MESS
117000000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
117100000000     C                   MOVEL     *BLANKS       $MSG
117200000000     C                   ENDSR
117300000000     C*------------------------------------------------------------------------*
117400000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
117500000000     C*------------------------------------------------------------------------*
117600000000     C     OUTMSG        BEGSR
117700000000     C* VISUALIZZAZIONE MESSAGGIO
117800000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
117900000000     C                   WRITE     SB08DVK                                      *CONTR MESS
118000000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
118100000000     C                   Z-ADD     *ZEROS        MSGULT
118200000000     C                   ENDSR
118300000000     C*------------------------------------------------------------------------*
118400000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
118500000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
118600000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
118700000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
118800000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
118900000000     C*------------------------------------------------------------------------*
119000000000     C     CARFUN        BEGSR
119100000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
119200000000     C     1             DO        31            I                 5 0          --- 1 -->
119300000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
119400000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
119500000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
119600020920     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
119700000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
119800000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
119900000000     C                   ELSE                                                   *INTERROG.
120000000000     C                   MOVEL     FUNASI        FAT(I)
120100000000     C                   ENDIF
120200000000     C                   ELSE                                                   <-- 2 --> *FILIALE
120300000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
120400000000     C                   MOVEL     FUNAFG        FAT(I)
120500000000     C                   ELSE                                                   *INTERROG.
120600000000     C                   MOVEL     FUNAFI        FAT(I)
120700000000     C                   ENDIF
120800000000     C                   ENDIF                                                  <-- 2 ---
120900000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
121000000000     C                   MOVEL     USER          WEDP              3            *PROFILO
121100000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
121200000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
121300000000     C                   ELSE
121400000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
121500000000     C                   ENDIF
121600000000     C                   ENDIF
121700000000     C                   ENDDO                                                  <-- 1 ---
121800000000     C                   ENDSR
121900000000     C*------------------------------------------------------------------------*
122000000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
122100000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
122200000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
122300000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
122400000000     C*------------------------------------------------------------------------*
122500000000     C     OUTFUN        BEGSR
122600000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
122700000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
122800000000     C                   ENDDO
122900000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
123000000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
123100000000     C     1             DO        31            I
123200000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
123300000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
123400000000     C                   ADD       1             FUNNAT
123500000000     C                   ENDIF
123600000000     C                   ENDDO
123700000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
123800000000     C     FUNNAT        IFGT      8
123900000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
124000000000     C                   ENDIF
124100000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
124200000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
124300000000     C                   Z-ADD     8             FUNVIS            5 0
124400000000     C     FAT(23)       IFEQ      'S'
124500000000     C                   SUB       1             FUNVIS
124600000000     C                   ENDIF
124700000000     C     FAT(24)       IFEQ      'S'
124800000000     C                   SUB       1             FUNVIS
124900000000     C                   ENDIF
125000000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
125100000000     C                   Z-ADD     *ZEROS        W                 5 0
125200000000     C                   ADD       1             FUNULT
125300000000     C     FUNULT        IFGT      31
125400000000     C                   Z-ADD     1             FUNULT
125500000000     C                   ENDIF
125600000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
125700000000     C     FUNULT        DO        31            I                              --- 1 -->
125800000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
125900000000     C     W             IFLT      FUNVIS                                       --- 2 -->
126000000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
126100000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
126200000000     C     FDE(I)        ANDNE     *BLANKS
126300000000     C     I             IFEQ      23                                           *TOLGO F23
126400000000     C     I             OREQ      24                                           *E F24
126500000000     C                   ELSE
126600000000     C                   ADD       1             W
126700000000     C                   MOVEL     FDE(I)        FUV(W)
126800000000     C                   ENDIF
126900000000     C                   ENDIF                                                  <-- 3 ---
127000000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
127100000000     C                   Z-ADD     I             FUNULT
127200000000     C                   ENDIF                                                  <-- 2 ---
127300000000     C                   ENDDO                                                  <-- 1 ---
127400000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
127500000000     C                   MOVEL     FUV(1)        VFD001
127600000000     C                   MOVEL     FUV(2)        VFD002
127700000000     C                   MOVEL     FUV(3)        VFD003
127800000000     C                   MOVEL     FUV(4)        VFD004
127900000000     C                   MOVEL     FUV(5)        VFD005
128000000000     C                   MOVEL     FUV(6)        VFD006
128100000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
128200000000     C     FAT(24)       ANDEQ     'N'
128300000000     C                   MOVEL     FUV(7)        VFD007
128400000000     C                   MOVEL     FUV(8)        VFD008
128500000000     C                   ELSE                                                   <-- 1 -->
128600000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
128700000000     C     FAT(24)       ANDEQ     'S'
128800000000     C                   MOVEL     FDE(23)       VFD007
128900000000     C                   MOVEL     FDE(24)       VFD008
129000000000     C                   ELSE                                                   <-- 2 -->
129100000000     C                   MOVEL     FUV(7)        VFD007
129200000000     C     FAT(23)       IFEQ      'S'
129300000000     C                   MOVEL     FDE(23)       VFD008
129400000000     C                   ELSE
129500000000     C                   MOVEL     FDE(24)       VFD008
129600000000     C                   ENDIF
129700000000     C                   ENDIF                                                  <-- 2 ---
129800000000     C                   ENDIF                                                  <-- 1 ---
129900000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
130000000000     C                   ENDSR
130100000000     C*------------------------------------------------------------------------*
130200000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
130300000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
130400000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
130500000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
130600000000     C*------------------------------------------------------------------------*
130700000000     C     CONFUN        BEGSR
130800000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
130900000000     C     1             DO        31            I                              --- 1 -->
131000000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
131100000000     C* CONTROLLO SE IL TASTO E' ATTIVO
131200000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
131300000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
131400000000     C                   EXSR      CARMSG
131500000000     C                   MOVEL     '1'           $ERR
131600000000     C                   ENDIF                                                  <-- 3 ---
131700000000     C                   ENDIF                                                  <-- 2 ---
131800000000     C                   ENDDO                                                  <-- 1 ---
131900000000     C                   ENDSR
132000000000     C*------------------------------------------------------------------------*
132100000000     C* CAROPZ - CARICA LE SCHIERE PER LA GESTIONE DELLE OPZIONI
132200000000     C*          INPUT:  - OPZ    (SCHIERA CARICATA AUTOMATICAMENTE DAL PGM)
132300000000     C*          OUTPUT: - OPD    (SCHIERA CON OPZIONE ALLINEATA A DESTRA)
132400000000     C*                  - OPS    (SCHIERA CON OPZIONE ALLINEATA A SINISTRA)
132500000000     C*                  - ODE    (SCHIERA DESCRIZIONI OPZIONI)
132600000000     C*                  - OAT    (SCHIERA FLAG DI OPZIONE ATTIVA S/N)
132700000000     C*------------------------------------------------------------------------*
132800000000     C     CAROPZ        BEGSR
132900000000     C                   Z-ADD     *ZEROS        OPZULT            5 0          *ULTIMA OPZIONE VISU
133000000000     C     1             DO        24            I                              --- 1 -->
133100000000     C                   MOVEL     OPZ(I)        OPZRIG                         *DS RIGA OPZIONI
133200000000     C                   MOVEL     OPZOPD        OPD(I)                         *OPZ ALLINEATA DESTR
133300000000     C                   MOVEL     OPZOPS        OPS(I)                         *OPZ ALLINEATA SINIS
133400000000     C                   MOVEL     OPZDES        ODE(I)                         *DESCRIZIONE OPZ
133500020920     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
133600000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
133700000000     C                   MOVEL     OPZASG        OAT(I)                         *OPZ ATTIVA S/N
133800000000     C                   ELSE                                                   *INTERROG.
133900000000     C                   MOVEL     OPZASI        OAT(I)                         *OPZ ATTIVA S/N
134000000000     C                   ENDIF
134100000000     C                   ELSE                                                   <-- 2 --> *FILIALE
134200000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
134300000000     C                   MOVEL     OPZAFG        OAT(I)                         *OPZ ATTIVA S/N
134400000000     C                   ELSE                                                   *INTERROG.
134500000000     C                   MOVEL     OPZAFI        OAT(I)                         *OPZ ATTIVA S/N
134600000000     C                   ENDIF
134700000000     C                   ENDIF                                                  <-- 2 ---
134800000000     C     OAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
134900000000     C                   MOVEL     USER          WEDP              3            *PROFILO
135000000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
135100000000     C                   MOVEL     'S'           OAT(I)                         *ABILITO
135200000000     C                   ELSE
135300000000     C                   MOVEL     'N'           OAT(I)                         *NON ABILITO
135400000000     C                   ENDIF
135500000000     C                   ENDIF
135600000000     C                   ENDDO                                                  <-- 1 ---
135700000000     C                   ENDSR
135800000000     C*------------------------------------------------------------------------*
135900000000     C* OUTOPZ - EMETTE A VIDEO LE OPZIONI ABILITATE
136000000000     C*          INPUT:  - OPZULT (ULTIMA OPZIONE EMESSA NEL VIDEO PRECEDENTE)
136100000000     C*          OUTPUT: - OPZNAT (N.DI OPZIONI ATTIVE)
136200000000     C*                  - VOD001-008 (CAMPI OPZIONI A VIDEO)
136300000000     C*                  - OPZULT (ULTIMA OPZIONE EMESSA A VIDEO)
136400000000     C*------------------------------------------------------------------------*
136500000000     C     OUTOPZ        BEGSR
136600000000     C     1             DO        8             I                              *CICLO PULIZIA CAMPI
136700000000     C                   MOVEL     *BLANKS       OPV(I)                         *DEL VIDEO
136800000000     C                   ENDDO
136900000000     C                   Z-ADD     *ZEROS        OPZNAT            2 0          *N.OPZIONI ATTIVE
137000000000     C* AGGIUNGO UNO ALL'ULTIMA OPZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALIZ
137100000000     C                   ADD       1             OPZULT
137200000000     C     OPZULT        IFGT      24
137300000000     C                   Z-ADD     1             OPZULT
137400000000     C                   END
137500000000     C* CONTROLLO QUANTE OPZIONI ATTIVE E DA VISUALIZZARE CI SONO
137600000000     C     1             DO        24            I
137700000000     C     OAT(I)        IFEQ      'S'                                          *FLAG ATTIVA S/N
137800000000     C     ODE(I)        ANDNE     *BLANKS                                      *DESCRIZ. <> SPAZIO
137900000000     C                   ADD       1             OPZNAT
138000000000     C                   ENDIF
138100000000     C                   ENDDO
138200000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
138300000000     C                   Z-ADD     0             W
138400000000     C     OPZULT        DO        24            I                              --- 1 -->
138500000000     C* SE SONO GIA' STATE EMESSE 8 OPZIONI VADO A FINE CICLO
138600000000     C     W             IFLT      8                                            --- 2 -->
138700000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
138800000000     C     OAT(I)        IFEQ      'S'
138900000000     C     ODE(I)        ANDNE     *BLANKS
139000000000     C                   ADD       1             W
139100000000     C                   MOVEL     ODE(I)        OPV(W)
139200000000     C                   ENDIF
139300000000     C* MEMORIZZO L'ULTIMA DESCRIZIONE EMESSA
139400000000     C                   Z-ADD     I             OPZULT
139500000000     C                   ENDIF                                                  <-- 2 ---
139600000000     C                   ENDDO                                                  <-- 1 ---
139700000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
139800000000     C                   MOVEL     OPV(1)        VOD001
139900000000     C                   MOVEL     OPV(2)        VOD002
140000000000     C                   MOVEL     OPV(3)        VOD003
140100000000     C                   MOVEL     OPV(4)        VOD004
140200000000     C                   MOVEL     OPV(5)        VOD005
140300000000     C                   MOVEL     OPV(6)        VOD006
140400000000     C                   MOVEL     OPV(7)        VOD007
140500000000     C                   MOVEL     OPV(8)        VOD008
140600000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
140700000000     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVO IL TASTO F23
140800000000     C     OPZNAT        IFGT      8
140900000000     C                   MOVEL     'S'           FAT(23)
141000000000     C                   ENDIF
141100000000     C                   ENDSR
141200000000     C*------------------------------------------------------------------------*
141300000000     C*  CONOPZ - CONTROLLO SE L'OPZIONE SCELTA E' ATTIVA
141400000000     C*           INPUT:  - VSCOPZ (OPZIONE SCELTA A VIDEO)
141500000000     C*           OUTPUT: - OPZOK  (OPZIONE OK S/N)
141600000000     C*                   - ERRORE SE OPZIONE NON ATTIVA
141700000000     C*------------------------------------------------------------------------*
141800000000     C     CONOPZ        BEGSR
141900000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
142000000000     C                   MOVEL     *BLANKS       OPZOK             1
142100000000     C     1             DO        24            I                 5 0          --- 1 -->CICLO 24 OP
142200000000     C     VSCOPZ        IFEQ      OPD(I)                                       --- 2 --> *ALLIN.DX
142300000000     C     VSCOPZ        OREQ      OPS(I)                                                 *ALLIN.SX
142400000000     C     OAT(I)        IFEQ      'S'
142500000000     C                   MOVEL     OPD(I)        VSCOPZ                         *PRENDO COMUNQUE DX
142600000000     C                   MOVEL     'S'           OPZOK
142700000000     C                   ELSE
142800000000     C                   MOVEL     'N'           OPZOK             1
142900000000     C                   ENDIF
143000000000     C                   ENDIF                                                  <-- 2 ---
143100000000     C                   ENDDO                                                  <-- 1 ---
143200000000     C* SE L'OPZIONE NON E' ATTIVA MANDO MESSAGGIO
143300000000     C     OPZOK         IFNE      'S'
143400000000     C                   MOVEL     MSG(3)        $MSG                           *OPZ NON ACCETTABILE
143500000000     C                   EXSR      CARMSG
143600000000     C                   MOVEL     '1'           $ERR
143700000000     C                   MOVEL     'N'           OPZOK             1
143800000000     C                   ENDIF
143900000000     C                   ENDSR
144000000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
144100000000TNSB08R- Tasto funzionale non valido.                                          1
144200000000TNSB08R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
144300000000TNSB08R- Opzione non valida.                                                   3
144400000000TNSB08R- Opzione abilitata ma non gestita. Avvertire CED.                      4
144500000000TNSB08R- Inizio elenco.                                                        5
144600000000TNSB08R- Fine elenco.                                                          6
144700000000TNSB08R- 7
144800000000TNSB08R- 8
144900000000TNSB08R- 9
145000000000TNSB08R- 10
145100000000TNSB08R- 11
145200000000TNSB08R- 12
145300000000TNSB08R- 13
145400000000TNSB08R- 14
145500000000TNSB08R- 15
145600000000TNSB08R- 16
145700000000TNSB08R- 17
145800000000TNSB08R- 18
145900000000TNSB08R- 19
146000000000TNSB08R- 20
146100000000** OPZ  (OPZIONI)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
146200000000- 1-1 -                   -    NN     NN
146300000000- 2-2 -2=Modifica         -    NN     NN
146400000000- 3-3 -                   -    NN     NN
146500000000- 4-4 -4=Cancellazione    -    NN     NN
146600000000- 5-5 -5=Visualizzazione  -    NN     NN
146700000000- 6-6 -                   -    NN     NN
146800000000- 7-7 -7=Ripristino       -    NN     NN
146900000000- 8-8 -                   -    NN     NN
147000140530- 9-9 -                   -    NN     NN
147100000000-10-  -                   -    NN     NN
147200000000-11-  -                   -    NN     NN
147300000000-12-  -                   -    NN     NN
147400000000-13-  -                   -    NN     NN
147500000000-14-  -                   -    NN     NN
147600000000-15-  -                   -    NN     NN
147700000000-16-  -                   -    NN     NN
147800000000-17-  -                   -    NN     NN
147900000000-18-  -                   -    NN     NN
148000000000-19-  -                   -    NN     NN
148100000000-20-  -                   -    NN     NN
148200000000-21-  -                   -    NN     NN
148300000000-22-  -                   -    NN     NN
148400000000-23-  -                   -    NN     NN
148500000000-24-  -                   -    NN     NN
148600000000** FUN2 (SUBFILE)         SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
148700000000F1    -                   -    NN     NN
148800000000F2    -                   -    NN     NN
148900000000F3    -F3=Fine            -    SS     SS
149000000000F4    -                   -    NN     NN
149100000000F5    -                   -    NN     NN
149200000000F6    -                   -    NN     NN
149300000000F7    -                   -    NN     NN
149400000000F8    -                   -    NN     NN
149500140612F9    -F9=Voci            -    NN     NN
149600000000F10   -                   -    NN     NN
149700000000F11   -                   -    NN     NN
149800000000F12   -F12=Ritorno        -    SS     SS
149900000000F13   -                   -    NN     NN
150000000000F14   -                   -    NN     NN
150100000000F15   -                   -    NN     NN
150200000000F16   -                   -    NN     NN
150300000000F17   -F17=Inizio dati    -    SS     SS
150400000000F18   -F18=Fine dati vis. -    SS     SS
150500000000F19   -                   -    NN     NN
150600000000F20   -                   -    NN     NN
150700000000F21   -                   -    NN     NN
150800000000F22   -                   -    NN     NN
150900000000F23   -                   -    NN     NN
151000000000F24   -F24=Altri tasti    -    NN     NN
151100000000ENTER -                   -    SS     SS
151200000000ROLLUP-                   -    SS     SS
151300000000ROLLDN-                   -    SS     SS
151400000000HELP  -                   -    NN     NN
151500000000PRINT -                   -    NN     NN
151600000000BSPACE-                   -    NN     NN
151700000000CLEAR -                   -    NN     NN
