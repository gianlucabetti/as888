000100930604     H DECEDIT('0,') DATEDIT(*DMY/)
000200930603     H* TRTB42R *-----------------------------------------------------*
000300930603     H*               - GESTIONE TABELLA TIPI VOLUME -                *
000400930304     H*---------------------------------------------------------------*
000500930603     H* FILE   : -------- - -------------------------------------------
000600930604     H*          TRTB42D  - FILE VIDEO
000700930604     H*                     TB42D1 / RICHIESTA TIPO VOLUME
000800930604     H*                     TB42D2 / RICHIESTA DATI TIPO VOLUME
000900930603     H*          TABEL00F - TABELLE GENERICHE
001000930603     H*                     7T / TIPI VOLUME
001100930603     H* PGM    : -------- - -------------------------------------------
001200930603     H*          X�PARUT  - RICERCA RAGIONE SOCIALE
001300930604     H*          X�TABER  - RICERCA GENERICA SUL FILE TABELLE
001400930603     H* SBR    : -------- - -------------------------------------------
001500930603     H*          DEFVAR   - DEFINIZIONE VARIABILI, KLIST E PLIST
001600930604     H*          CTLD1    - CONTROLLO DATI PRIMO FORMATO VIDEO
001700930609     H*          RIEMP    - RIEMPIMENTO II FORMATO CON CAMPI TABELLA
001800930609     H*          VALOR    - RIEMPIMENTO II FORMATO CON CAMPI VUOTI
001900930609     H*          CTLD2    - CONTROLLO DATI SECONDO FORMATO VIDEO
002000930609     H*          RIPRI    - RIPRISTINO    RECORD TABELLA
002100930609     H*          AGGIO    - AGGIORNAMENTO RECORD TABELLA
002200930609     H*          ANNUL    - ANNULLAMENTO  RECORD TABELLA
002300930603     H*---------------------------------------------------------------*
002400930603     H* INDICATORI USATI                                              *
002500930603     H*---------------------------------------------------------------*
002600930604     H* 01 - TIPO VOLUME OBBLIGATORIO
002700930604     H* 02 - DESCRIZIONE OBBLIGATORIA
002800930609     H* 03 - INVIARE SE SOSTITUISCO PER I FRANCHI
002900930609     H* 04 - INVIARE SE SOSTITUISCO PER GLI ASSEGNATI
003000930604     H* 05 - CM5-RIPRISTINO  ABILITATO
003100930604     H* 06 - CM5-AGGIORNAMENTO  ABILITATO
003200930604     H* 07 - CAMPI PROTETTI PER CM5-RIPRISTINO
003300930609     H* 08 - SOSTITUIRE SE INVIO PER I FRANCHI
003400930609     H* 09 - SOSTITUIRE SE INVIO PER GLI ASSEGNATI
003500930604     H* 16 - CM16-ANNULLAMENTO  ABILITATO
003600930604     H* 30 - LETTURA TABELLE
003700930604     H* 90 - ERRORE GENERICO
003800930603     H*---------------------------------------------------------------*
003900930604     FTABEL00F  UF A E           K DISK
004000930604     FTRTB42D   CF   E             WORKSTN
004100930604     D*---------------------------------------------------------------*
004200930604     D COM             S             10    DIM(3) CTDATA PERRCD(1)
004300930603     D*---------------------------------------------------------------*
004400930603     D KPJBA         E DS
004500930603     D  LIBSYS                92    100
004600930603     D UT�DSE        E DS                  EXTNAME(UT�DSE0F)
004700930603     D DS7T          E DS
004800930603     C*---------------------------------------------------------------*
004900930603     C*                    MAIN                                       *
005000930603     C* PROGRAMMA DI GESTIONE TABELLA TIPI VOLUME                     *
005100930603     C*---------------------------------------------------------------*
005200930603     C*
005300930603     C* DEFINIZIONI VARIABILI E KLIST
005400930603     C                   EXSR      DEFVAR
005500930603     C*
005600930604     C* EMISSIONE I FORMATO - RICHIESTA TIPO VOLUME
005700930604     C                   MOVEL     '?'           VIDKEY
005800930604     C     FMTD1         TAG
005900930604     C                   EXFMT     TB42D1
006000930604     C*
006100930604     C* CM3 - FINE LAVORO
006200930604     C   KC              GOTO      FINE
006300930604     C*
006400930604     C* CONTROLLO DATI I FORMATO
006500930604     C                   EXSR      CTLD1
006600930604     C*
006700930604     C* ERRORI: RIEMETTO IL PRIMO FORMATO
006800930604IF  1C     *IN90         IFEQ      '1'
006900930604     C                   GOTO      FMTD1
007000930604X   1C                   ELSE
007100930604     C*
007200930604     C* NO ERRORI: EMISSIONE II FORMATO - DATI TIPO VOLUME
007300930604     C     FMTD2         TAG
007400930604     C                   EXFMT     TB42D2
007500930604     C*
007600930604     C* CM3 - FINE LAVORO
007700930604     C   KC              GOTO      FINE
007800930604     C*
007900930604     C* CM12 - RITORNO VIDEATA PRECEDENTE
008000930604     C   KL              GOTO      FMTD1
008100930604     C*
008200930604     C* CONTROLLO DATI II FORMATO
008300930604     C                   EXSR      CTLD2
008400930604     C*
008500930604     C* ERRORI: RIEMETTO IL SECONDO FORMATO
008600930604IF  2C     *IN90         IFEQ      '1'
008700930604     C                   GOTO      FMTD2
008800930604X   2C                   ELSE
008900930604     C*
009000930604     C* CM5 - RIPRISTINO
009100930604     C   KE              EXSR      RIPRI
009200930604     C   KE              GOTO      FMTD1
009300930604     C*
009400930604     C* CM6 - AGGIORNAMENTO/INSERIMENTO
009500930604     C   KF              EXSR      AGGIO
009600930604     C   KF              GOTO      FMTD1
009700930604     C*
009800930604     C* CM16 - ANNULLAMENTO
009900930604     C   KQ              EXSR      ANNUL
010000930604     C   KQ              GOTO      FMTD1
010100930604     C*
010200930604     C* ENTER - RIMANE NELLA SECONDA VIDEATA
010300930604IF  3C     *INKE         IFEQ      '0'
010400930604     C     *INKF         ANDEQ     '0'
010500930604     C     *INKQ         ANDEQ     '0'
010600930604     C                   GOTO      FMTD2
010700930604E   3C                   ENDIF
010800930604E   2C                   ENDIF
010900930604E   1C                   ENDIF
011000930604     C*
011100930604     C     FINE          TAG
011200930604     C                   SETON                                        LR
011300930604     C*---------------------------------------------------------------*
011400930604     C*                    CTLD1                                      *
011500930604     C* ROUTINE DI CONTROLLO DATI PRIMO FORMATO VIDEO                 *
011600930604     C*---------------------------------------------------------------*
011700930604     C     CTLD1         BEGSR
011800930604     C*
011900930604     C                   SETOFF                                         9007
012000930611     C                   SETOFF                                       160506
012100930604     C                   MOVEL     *BLANKS       VI2OPE
012200930604     C*
012300930604     C* RICERCA IN TABELLA TIPO VOLUME PER '?'
012400930604IF  1C     VIDKEY        IFEQ      '?'
012500930604     C*
012600930604     C                   MOVEL     '7T'          COD
012700930604     C                   MOVEL     *BLANKS       KEY
012800930604     C                   MOVEL     *BLANKS       DES
012900930604     C                   CALL      'X�TABER'
013000930604     C                   PARM                    CODUT
013100930604     C                   PARM                    COD
013200930604     C                   PARM                    KEY
013300930604     C                   PARM                    DES              40
013400930604     C*
013500930604     C* TIPO VOLUME SELEZIONATO
013600930604IF  2C     KEY           IFNE      *BLANKS
013700930604     C                   MOVEL     KEY           VIDKEY
013800930604E   2C                   ENDIF
013900930604     C*
014000930604     C* SIA PER VOLUME SELEZIONATO CHE NON RIEMETTO IL FORMATO
014100930604     C                   SETON                                        90
014200930604     C                   GOTO      ENDC1
014300930604E   1C                   ENDIF
014400930604     C*
014500930604     C* CONTROLLO ESISTENZA TIPO VOLUME
014600930604IF  1C     VIDKEY        IFNE      *BLANKS
014700930604     C*
014800930607     C                   MOVEL     '7T'          COD
014900930604     C                   MOVEL     *BLANKS       KEY
015000930604     C                   MOVEL     VIDKEY        KEY
015100930604     C     KTAB          CHAIN     TABEL00F                           30
015200930604     C*
015300930604     C* TIPO VOLUME ESISTENTE: RIEMPO II FORMATO CON I DATI IN TABELLA
015400930604IF  2C     *IN30         IFEQ      '0'
015500930604     C                   MOVEL     TBLUNI        DS7T
015600930604     C                   EXSR      RIEMP
015700930604     C*
015800930604     C* TABELLA ANNULLATA: ABILITO CM5-RIPRISTINO E PROTEGGO I CAMPI
015900930604IF  3C     TBLFLG        IFNE      *BLANKS
016000930604     C                   SETON                                        0507
016100930607     C                   SETOFF                                       0616
016200930604     C                   MOVEL     COM(1)        VI2OPE
016300930604X   3C                   ELSE
016400930604     C*
016500930604     C* NON ANNULLATA: ABILITO CM6-AGGIORNAMENTO E CM16-ANNULLAMENTO
016600930604     C                   SETON                                        0616
016700930607     C                   SETOFF                                       0507
016800930604     C                   MOVEL     COM(2)        VI2OPE
016900930604E   3C                   ENDIF
017000930604     C*
017100930604     C* TESTO INESISTENTE: CAMPI PULITI E CM6-AGGIORNAMENTO ABILITATO
017200930604X   2C                   ELSE
017300930604     C                   MOVEL     COM(3)        VI2OPE
017400930604     C                   SETON                                        06
017500930604     C                   EXSR      VALOR
017600930604E   2C                   ENDIF
017700930604     C*
017800930604     C* TIPO VOLUME NON INSERITO: ERRORE
017900930604X   1C                   ELSE
018000930604     C                   SETON                                        0190
018100930604     C                   GOTO      ENDC1
018200930604E   1C                   ENDIF
018300930604     C*
018400930604     C     ENDC1         TAG
018500930604     C                   ENDSR
018600930604     C*---------------------------------------------------------------*
018700930604     C*                    RIEMP                                      *
018800930604     C* ROUTINE DI RIEMPIMENTO II FORMATO SE RECORD ESISTENTE         *
018900930604     C*---------------------------------------------------------------*
019000930604     C     RIEMP         BEGSR
019100930604     C*
019200930604     C                   MOVEL     VIDKEY        VI2KEY
019300930604     C                   MOVEL     �7TDES        VI2DES
019400930604     C                   MOVEL     �7TVRP        VI2VRP
019500930604     C                   MOVEL     �7TVRA        VI2VRA
019600120705     C                   MOVEL     �7TVRPexp     VI2VRPexp
019700120705     C                   MOVEL     �7TVRAexp     VI2VRAexp
019800930604     C                   MOVEL     �7TPSF        VI2PSF
019900930604     C                   MOVEL     �7TPSA        VI2PSA
020000930604     C                   MOVEL     �7TAPF        VI2APF
020100930604     C                   MOVEL     �7TAPA        VI2APA
020200930604     C                   MOVEL     �7TASF        VI2ASF
020300930604     C                   MOVEL     �7TASA        VI2ASA
020400930604     C                   MOVEL     �7TTPF        VI2TPF
020500930604     C                   MOVEL     �7TTPA        VI2TPA
020600930604     C                   MOVEL     �7TTSF        VI2TSF
020700930604     C                   MOVEL     �7TTSA        VI2TSA
020800930604     C                   MOVEL     �7TRVL        VI2RVL
020900930604     C                   MOVEL     �7TAFA        VI2AFA
021000930604     C                   MOVEL     �7TAAA        VI2AAA
021100930604     C                   MOVEL     �7TAFN        VI2AFN
021200930604     C                   MOVEL     �7TAAN        VI2AAN
021300951211     C                   MOVEL     �7TSOZ        VI2SOZ
021400951211     C                   MOVEL     �7TSOT        VI2SOT
021500070615     C                   MOVEL     �7TSOZf       VI2SOZf
021600070615     C                   MOVEL     �7TSOTf       VI2SOTf
021700070615     C                   MOVEL     �7TSOZa       VI2SOZa
021800070615     C                   MOVEL     �7TSOTa       VI2SOTa
021900951211     C* SE E' VOLUME CML ACCENDO INDICATORE
022000951211     C     VIDKEY        IFEQ      'Z'
022100951211     C     VIDKEY        OREQ      'T'
022200951211     C                   SETON                                        20
022300951211     C                   ELSE
022400951211     C                   SETOFF                                       20
022500951211     C                   ENDIF
022600930604     C*
022700930604     C                   ENDSR
022800930604     C*---------------------------------------------------------------*
022900930604     C*                    VALOR                                      *
023000930604     C* ROUTINE DI RIEMPIMENTO II FORMATO SE RECORD INESISTENTE       *
023100930604     C*---------------------------------------------------------------*
023200930604     C     VALOR         BEGSR
023300930604     C*
023400951211     C                   SETOFF                                       20
023500930604     C                   MOVEL     VIDKEY        VI2KEY
023600930604     C                   MOVEL     *BLANKS       VI2DES
023700930604     C                   MOVEL     *BLANKS       VI2VRP
023800930604     C                   MOVEL     *BLANKS       VI2VRA
023900120705     C                   MOVEL     *BLANKS       VI2VRPexp
024000120705     C                   MOVEL     *BLANKS       VI2VRAexp
024100930604     C                   MOVEL     *BLANKS       VI2PSF
024200930604     C                   MOVEL     *BLANKS       VI2PSA
024300930604     C                   MOVEL     *BLANKS       VI2APF
024400930604     C                   MOVEL     *BLANKS       VI2APA
024500930604     C                   MOVEL     *BLANKS       VI2ASF
024600930604     C                   MOVEL     *BLANKS       VI2ASA
024700930604     C                   MOVEL     *BLANKS       VI2TPF
024800930604     C                   MOVEL     *BLANKS       VI2TPA
024900930604     C                   MOVEL     *BLANKS       VI2TSF
025000930604     C                   MOVEL     *BLANKS       VI2TSA
025100951211     C                   MOVEL     'R'           VI2RVL
025200930604     C                   MOVEL     *BLANKS       VI2AFA
025300930604     C                   MOVEL     *BLANKS       VI2AAA
025400930604     C                   MOVEL     *BLANKS       VI2AFN
025500930604     C                   MOVEL     *BLANKS       VI2AAN
025600951211     C                   MOVEL     *BLANKS       VI2SOT
025700951211     C                   MOVEL     *BLANKS       VI2SOZ
025800070615     C                   MOVEL     *BLANKS       VI2SOTa
025900070615     C                   MOVEL     *BLANKS       VI2SOZa
026000070615     C                   MOVEL     *BLANKS       VI2SOTf
026100070615     C                   MOVEL     *BLANKS       VI2SOZf
026200930604     C*
026300930604     C                   ENDSR
026400930604     C*---------------------------------------------------------------*
026500930604     C*                    CTLD2                                      *
026600930604     C* ROUTINE DI CONTROLLO DATI SECONDO FORMATO VIDEO               *
026700930604     C*---------------------------------------------------------------*
026800930604     C     CTLD2         BEGSR
026900930604     C*
027000930604     C                   SETOFF                                       90
027100930604     C*
027200930604     C* DESCRIZIONE OBBLIGATORIA
027300930604IF  1C     VI2DES        IFEQ      *BLANKS
027400930604     C                   SETON                                        0290
027500930604     C                   GOTO      ENDC2
027600930604E   1C                   ENDIF
027700070615     c* Sostituzione volume vdl: se tutto, non posso impostare Franchi a Ass
027800070615     c                   if        vi2soz='S' and (vi2sozf='S' or vi2soza='S')
027900070615     C                   SETON                                        4090
028000070615     C                   GOTO      ENDC2
028100070615E   1C                   ENDIF
028200070615     c                   if        vi2sot='S' and (vi2sotf='S' or vi2sota='S')
028300070615     C                   SETON                                        4190
028400070615     C                   GOTO      ENDC2
028500070615E   1C                   ENDIF
028600930609     C*
028700930604     C     ENDC2         TAG
028800930604     C                   ENDSR
028900930604     C*---------------------------------------------------------------*
029000930604     C*                    RIPRI                                      *
029100930604     C* ROUTINE DI RIPRISTINO RECORD TABELLA                          *
029200930604     C*---------------------------------------------------------------*
029300930604     C     RIPRI         BEGSR
029400930604     C*
029500930623     C                   MOVEL     *BLANKS       TBLFTR
029600930623     C                   Z-ADD     *ZEROS        TBLDTR
029700930623     C*
029800930604     C                   MOVEL     *BLANKS       TBLFLG
029900930604     C                   UPDATE    TABEL
030000930604     C*
030100930604     C                   ENDSR
030200930604     C*---------------------------------------------------------------*
030300930604     C*                    ANNUL                                      *
030400930604     C* ROUTINE DI ANNULLAMENTO RECORD TABELLA                        *
030500930604     C*---------------------------------------------------------------*
030600930604     C     ANNUL         BEGSR
030700930604     C*
030800930623     C                   MOVEL     *BLANKS       TBLFTR
030900930623     C                   Z-ADD     *ZEROS        TBLDTR
031000930623     C*
031100930604     C                   MOVEL     '*'           TBLFLG
031200930604     C                   UPDATE    TABEL
031300930604     C*
031400930604     C                   ENDSR
031500930604     C*---------------------------------------------------------------*
031600930604     C*                    AGGIO                                      *
031700930604     C* ROUTINE DI AGGIORNAMENTO RECORD TABELLA                       *
031800930604     C*---------------------------------------------------------------*
031900930604     C     AGGIO         BEGSR
032000930604     C*
032100970826     C                   CLEAR                   DS7T
032200930604     C                   MOVEL     VI2DES        �7TDES
032300930604     C                   MOVEL     VI2VRP        �7TVRP
032400930604     C                   MOVEL     VI2VRA        �7TVRA
032500120705     C                   MOVEL     VI2VRPexp     �7TVRPexp
032600120705     C                   MOVEL     VI2VRAexp     �7TVRAexp
032700930604     C                   MOVEL     VI2PSF        �7TPSF
032800930604     C                   MOVEL     VI2PSA        �7TPSA
032900930604     C                   MOVEL     VI2APF        �7TAPF
033000930604     C                   MOVEL     VI2APA        �7TAPA
033100930604     C                   MOVEL     VI2ASF        �7TASF
033200930604     C                   MOVEL     VI2ASA        �7TASA
033300930604     C                   MOVEL     VI2TPF        �7TTPF
033400930604     C                   MOVEL     VI2TPA        �7TTPA
033500930604     C                   MOVEL     VI2TSF        �7TTSF
033600930604     C                   MOVEL     VI2TSA        �7TTSA
033700930604     C                   MOVEL     VI2RVL        �7TRVL
033800930604     C                   MOVEL     VI2AFA        �7TAFA
033900930604     C                   MOVEL     VI2AAA        �7TAAA
034000930604     C                   MOVEL     VI2AFN        �7TAFN
034100930604     C                   MOVEL     VI2AAN        �7TAAN
034200951211     C                   MOVEL     VI2SOT        �7TSOT
034300951211     C                   MOVEL     VI2SOZ        �7TSOZ
034400070615     C                   MOVEL     VI2SOTf       �7TSOTf
034500070615     C                   MOVEL     VI2SOZf       �7TSOZf
034600070615     C                   MOVEL     VI2SOTa       �7TSOTa
034700070615     C                   MOVEL     VI2SOZa       �7TSOZa
034800930604     C*
034900930604     C                   MOVEL     DS7T          TBLUNI
035000930623     C*
035100930623     C                   MOVEL     *BLANKS       TBLFTR
035200930623     C                   Z-ADD     *ZEROS        TBLDTR
035300930604     C*
035400930604     C* V A R I A Z I O N E
035500930604IF  1C     *IN30         IFEQ      '0'
035600930604     C                   UPDATE    TABEL
035700930604X   1C                   ELSE
035800930604     C*
035900930604     C* I N S E R I M E N T O
036000930604     C                   Z-ADD     1             TBLKUT
036100930604     C                   MOVEL     '7T'          TBLCOD
036200930604     C                   MOVEL     VI2KEY        TBLKEY
036300930604     C                   MOVEL     *BLANKS       TBLFLG
036400930604     C                   MOVEL     '1'           TBLFTT
036500930604     C                   MOVEL     *BLANKS       TBLFLT
036600930604     C                   WRITE     TABEL
036700930604E   1C                   ENDIF
036800930604     C*
036900930604     C                   ENDSR
037000930603     C*---------------------------------------------------------------*
037100930603     C*                    DEFVAR                                     *
037200930603     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
037300930603     C*---------------------------------------------------------------*
037400930603     C     DEFVAR        BEGSR
037500930603     C*
037600930603     C     *ENTRY        PLIST
037700930603     C                   PARM                    KPJBA
037800930603     C*
037900930604     C                   Z-ADD     1             CODUT
038000930603     C                   CALL      'X�PARUT'
038100930603     C                   PARM                    UT�DSE
038200930603     C                   MOVEL     RAGUT         RSUT             20
038300930603     C*
038400930604     C* LETTURA TABEL00F
038500930604     C     KTAB          KLIST
038600930604     C                   KFLD                    CODUT             1 0
038700930604     C                   KFLD                    COD               2
038800930604     C                   KFLD                    KEY               8
038900930604     C*
039000930603     C                   ENDSR
039100930604     O*---------------------------------------------------------------*
039200930604     O* DESCRIZIONI OPERAZIONI SECONDO FORMATO VIDEO                  *
039300930604     O*---------------------------------------------------------------*
039400930604**
039500930604ANNULLATO
039600930604VARIAZIONE
039700930604IMMISSIONE
