000100000000     H*------------------------------------------------------------------------*
000200980131     H* STATISTICA CLIENTI VAS - ESEGUE
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800990810     FTITAS38C  IF   E           K DISK
000900000000     FWFSTP01L  UF A E           K DISK    USROPN
001000000316     FWFSTP03L  UF   E           K DISK    USROPN
001100000316     F                                     RENAME(WFSTP:WFSTP3)
001200000000     FWFSTP02L  IF   E           K DISK    USROPN
001300000000     F                                     RENAME(WFSTP:WFSTP2)
001301110817     FWFSTV00F  o  A E             DISK    USROPN
001400000000     FAZORG01L  IF   E           K DISK
001500000000     FTABEL00F  IF   E           K DISK
001600110401     F***TIVSS04L  IF   E           K DISK
001700110624     FTIVIR01L  IF   E           K DISK
001800000000     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
001900000000     F                                     FORMLEN(66)
002000000000     F                                     FORMOFL(60)
002100010626     FQSYSPRT   O    F  132        PRINTER
002200000000     L*------------------------------------------------------------------------*
002300000000     L* ESTENSIONI
002400000000     L*------------------------------------------------------------------------*
002500000000     D*------------------------------------------------------------------------*
002600000000     D* SCHIERE
002700000000     D*------------------------------------------------------------------------*
002800000321     d*-------------------
002900000321     d* COMANDI CLP
003000000321     d*-------------------
003100110817     D CMD1            S             55    DIM(2) CTDATA PERRCD(1)
003200000000     D*-------------------
003300000000     D* RIGHE DI TESTATA
003400000000     D*-------------------
003500010626     D TES             S             66    DIM(8) CTDATA PERRCD(1)
003600010626     D*-------------------
003700010626     D* RIGHE DI TESTATA X QSYSPRT
003800010626     D*-------------------
003900010626     D TE1             S             66    DIM(5) CTDATA PERRCD(1)
004000000000     D*-------------------
004100000000     D* CLIENTI CON DISCHETTO
004200000000     D*-------------------
004300970205     D TIB             S              5    DIM(100)                             TIPO INVIO BOL
004400970205     D NIB             S              5  0 DIM(100)                             NUM.X TIPO INV
004500111205     d c_MaxCli3C      c                   const(30000)
004600100928     D CLI             S              7  0 DIM(c_MaxCli3C)                      *CLIENTI
004700100928     D CUN             S              7  0 DIM(c_MaxCli3C)                      *CLIENTI UNIF.
004800100928     D CBA             S              5    DIM(c_MaxCli3C)                      *SUPPORTO CL.BARTOLI
004900100928     D DET             S              1    DIM(c_MaxCli3C)                      *DETT.SEGNACOLLI S/N
005000100928     D CAP             S              1    DIM(c_MaxCli3C)                      *TIPO CAPPARIO   S/N
005100100928     D PES             S              1    DIM(c_MaxCli3C)                      *GEST.PESO
005200100928     D BAC             S              5    DIM(c_MaxCli3C)                      *SUPPORTO CL.BARTOLI
005300000000     D*-------------------
005400000000     D* CODICI TRATTAMENTO MERCE
005500000000     D*-------------------
005600000000     D CTM             S              2    DIM(100)                             *COD.TRATTAMENTO MER
005700000710     D FG7             S              1    DIM(100)                             *DETT.SEGNACOLLI S/N
005800010418     D TMC             S              2    DIM(100)                             *DETT.SEGNACOLLI S/N
005900010418     D TMT             S              1    DIM(100)                             *DETT.SEGNACOLLI S/N
006000000710     D*-------------------
006100000710     D* SCHIERA FILIALI
006200000710     D*-------------------
006300000710     D FIL             S              3  0 DIM(500)                             *CODICE FILIALE
006400000710     D DFIL            S             20    DIM(500)                             *DESCRIZIONE FILIALE
006500000000     D*------------------------------------------------------------------------*
006600000000     D* INPUT
006700000000     D*------------------------------------------------------------------------*
006800000000     D*---------------------
006900000000     D* ARCHITETTURA
007000000000     D*---------------------
007100980131     D KPJBA         E DS
007200980131     D  WNSIF                 98    100
007300000000     D*---------------------
007400000000     D* DS PARAMETRI RICEVUTI
007500000000     D*------------------
007600000000     D PARAM           DS                  INZ
007700000000     D  PARDTI                 1      8  0
007800000000     D  PARDTF                 9     16  0
007900000000     D  PARNBO                17     19  0
008000970919     D  PARNCO                20     22  0
008100970919     D  PARPAR                23     23
008200970919     D  PARSTE                24     24
008300000316     D  PARTRK                25     25
008400010626     D  PARCOM                27     27
008500000000     D*---------------------
008600000000     D* DS3C - CLIENTI CON DISCHETTO
008700000000     D*---------------------
008800000000     D DS3C          E DS
008900970130     D*---------------------
009000970130     D* DS3K - CLIENTI CON VAS DA FILIALE
009100970130     D*---------------------
009200970130     D DS3K          E DS
009300970130     D*---------------------
009400970130     D* DS4C - CLIENTI CON VAS DA SEDE
009500970130     D*---------------------
009600970130     D DS4C          E DS
009700970130     D*---------------------
009800970130     D* DS3R - NOTE COMODATI
009900970130     D*---------------------
010000970130     D DS3R          E DS
010100000000     D*---------------------
010200000000     D* DS1B - CODICI TRATTAMENTO MERCE
010300000000     D*---------------------
010400000000     D DS1B          E DS
010500971128     D*---------------------
010600971128     D* DS REPEIMENTO TERMINALs
010700971128     D*---------------------
010800971128     D DSLV55        E DS                  EXTNAME(FNLV55DS)
010900000710     D*-------------------
011000000710     D* LETTURA CAMPO ORGDE£ DI AZORG
011100000710     D*-------------------
011200000710     D OG143         E DS
011300980116     D*------------------
011400980116     D* DS REPERIMENTO DATI UTENTE
011500980116     D*-------------------
011600980406     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
011700980406     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
011800980406     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
011900980406     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
012000980406     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
012100971128     D*---------------------
012200971128     D* DS "XSRDA8" - CONTROLLA DATA (8)
012300971128     D*---------------------
012400971128     D WLBDA8          DS                  INZ
012500971128     D  G08DAT                 1      8  0
012600971128     D  G08INV                 9     16  0
012700971128     D  G08ERR                17     17
012800971128     D  G08TGI                18     22  0
012900000000     D*---------------------
013000000000     D* SUDDIVISIONE DATA
013100000000     D*---------------------
013200000000     D                 DS
013300000000     D  DSSEC                  1      2  0
013400000000     D  DSANN                  3      4  0
013500000000     D  DSAAS                  1      4  0
013600000000     D  DSMES                  5      6  0
013700000000     D  DSGIO                  7      8  0
013800000000     D  DSMGS                  5      8  0
013900000000     D  DSDAT6                 3      8  0
014000000000     D  DSDAT8                 1      8  0
014100000000     D*---------------------
014200000000     D* SUDDIVISIONE DATA
014300000000     D*---------------------
014400000000     D                 DS
014500000000     D  DTORA                  1      2  0
014600000000     D  DTMIN                  3      4  0
014700000000     D  DTSEC                  5      6  0
014800000000     D  DTGIO                  7      8  0
014900000000     D  DTMES                  9     10  0
015000000000     D  DTANN                 11     12  0
015100000000     D  DTTIME                 1     12  0
015200000000     D  DTORA6                 1      6  0
015300000000     D  DTDAT6                 7     12  0
015400000000     C*------------------------------------------------------------------------*
015500000000     C* MAIN LINE
015600000000     C*------------------------------------------------------------------------*
015700000000     C*
015800000000     C* SE NON DEVE ESSERE ESEGITO, CHIUDE IL PGM
015900000000DO  1C     $FINE         IFEQ      '0'
016000000000     C*
016100970130     C* SE SOLO STAMPA IMPOSTO FINE
016200970130     C     PARSTE        IFEQ      'S'
016300970130     C                   MOVEL     '1'           $FINE
016400970130     C                   ENDIF
016500000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
016600000000     C                   EXSR      SETFIL
016700000000     C*
016800000000     C* CICLO FINO A FINE FILE
016900000000DO  2C     $FINE         DOWEQ     '0'
017000000000     C*
017100000000     C* AGGIORNA IL FILE DI LAVORO
017200000322     C                   EXSR      AGGSTP
017300000000     C*
017400000000     C* LEGGE IL RECORD SUCCESSIVO
017500000000     C                   EXSR      LETFIL
017600000000DO  2C                   ENDDO
017700000000     C*
017800970912     C* GENERA NEL FILE UN RECORD PER OGNI CLIENTE
017900970912     C* PRESENTE NELLA TABELLA 3R E NON GIA' PRESENTE NEL FILE
018000970912     C                   EXSR      ADD3R
018100000316     C* GENERA NEL FILE UN RECORD PER OGNI CLIENTE
018200000316     C* PRESENTE NEL FILE DEI CLIENTI ABILITATI AL TRACK & TRACING
018300110401     C****               EXSR      ADDTRK
018400110624     C* AGGIORNA IL WRK-FILE CON L'INDICAZIONE DI VAS VB/VT PERSONALIZZATO/STANDARD/?
018500110624     C                   EXSR      UPDVAS
018600000000     C* LEGGE IL FILE APPENA GENERATO E STAMPA
018700000000     C                   EXSR      STAMPA
018800000000E   1C                   ENDIF                                                  <-- 1 ---
018900970131     C* STAMPA TOTALI ULTIMA FILIALE
019000990204     C     *IN88         IFEQ      '1'
019100010626     C     *IN14         ANDEQ     *OFF
019200970131     C                   EXSR      PERC                                         PERCENTUALI
019300010627     C     *IN14         IFEQ      *OFF
019400970131     C                   EXCEPT    TFIL
019500990204     C                   ENDIF
019600010627     C                   ENDIF
019700000000     C* STAMPA RIGA FINALE
019800970131     C                   Z-ADD     STFSPA        STTSPA
019900970131     C                   Z-ADD     STFSPB        STTSPB
020000010423     C                   Z-ADD     STFSPK        STTSPK
020100970131     C                   Z-ADD     STFSPT        STTSPT
020200970131     C                   Z-ADD     STFCOB        STTCOB
020300970131     C                   Z-ADD     STFCOT        STTCOT
020400990204     C                   Z-ADD     S2FSPA        S2TSPA
020500010423     C                   Z-ADD     S2FSPK        S2TSPK
020600990204     C                   Z-ADD     S2FSPB        S2TSPB
020700990204     C                   Z-ADD     S2FSPT        S2TSPT
020800990204     C                   Z-ADD     S2FCOB        S2TCOB
020900990204     C                   Z-ADD     S2FCOT        S2TCOT
021000970131     C                   EXSR      PERC                                         PERCENTUALI
021100010626     C  N14              EXCEPT    TESTA
021200010626     C  N14              EXCEPT    STAFIN
021300970205     C                   DO        100           I
021400970205     C     TIB(I)        IFGT      *BLANK
021500010626     C  N14              EXCEPT    STAFI2
021600970205     C                   ENDIF
021700970205     C                   ENDDO
021800971128     C*
021900971128     C* OPERAZIONI FINALI
022000971128     C                   EXSR      FINSR
022100971128     C*
022200971128     C                   SETON                                        LR
022300971128     C*------------------------------------------------------------------------*
022400971128     C* FINSR - OPERAZIONI FINALI
022500971128     C*------------------------------------------------------------------------*
022600971128     C     FINSR         BEGSR
022700971128     C*
022800971128     C* CHIUDE I FILES
022900971128     C                   CLOSE     WFSTP01L
023000971128     C                   CLOSE     WFSTP02L
023100971128     C*
023200971128     C* CHIUDE I PGM
023300971128     C                   CLEAR                   DSLV55
023400971128     C                   MOVEL     'C'           D55TLA
023500971128     C                   CALL      'FNLV55R'
023600971128     C                   PARM                    DSLV55
023700980116     C*
023800980116     C                   CLEAR                   BS69DS
023900980116     C                   CLEAR                   ACODS
024000980116     C                   CLEAR                   INDDS
024100980116     C                   CLEAR                   CLPDS
024200980116     C                   CLEAR                   CLSDS
024300980116     C                   MOVEL     'C'           I69TLA                         *TIPO LANCIO
024400980116     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
024500980116     C                   CALL      'TIBS69R'
024600980116     C                   PARM                    BS69DS
024700980116     C                   PARM                    ACODS
024800980116     C                   PARM                    INDDS
024900980116     C                   PARM                    CLPDS
025000980116     C                   PARM                    CLSDS
025100971128     C*
025200971128     C                   ENDSR
025300000000     C*------------------------------------------------------------------------*
025400000000     C* SETFIL - POSIZIONAMENTO SUL FILE E PRIMA LETTURA
025500000000     C*------------------------------------------------------------------------*
025600000000     C     SETFIL        BEGSR
025700000000     C*
025800000000     C* POSIZIONAMENTO E PRIMA LETTURA
025900000316     C                   MOVEL     PARDTI        TASAAS
026000000316     C                   MOVE      PARDTI        TASMGS
026100990810     C     KEYTAS        SETLL     TITAS38C                           99        *FINE ARCHIVIO
026200000000     C*
026300000000IF  1C     *IN99         IFEQ      *ON
026400000000     C                   MOVEL     '1'           $FINE
026500000000X   1C                   ELSE
026600000000     C                   EXSR      LETFIL
026700000000E   1C                   ENDIF
026800000000     C*
026900000000     C                   ENDSR
027000000000     C*------------------------------------------------------------------------*
027100000000     C* LETFIL - LETTURA PROSSIMO RECORD
027200000000     C*------------------------------------------------------------------------*
027300000000     C     LETFIL        BEGSR
027400000000     C*
027500000000     C* LEGGE FINO A:
027600000000     C*     - FINE FILE O SUPERATO LIMITE FINALE
027700000000     C*     - TROVATO RECORD VALIDO
027800000000     C                   MOVEL     'N'           WRECOK
027900000000DO  1C     $FINE         DOWEQ     '0'
028000000000     C     WRECOK        ANDEQ     'N'
028100000000     C*
028200990810     C                   READ      TITAS38C                               99
028300980209     C                   MOVEL     TASAAS        WDT8              8 0
028400980209     C                   MOVE      TASMGS        WDT8
028500000000     C*
028600000000IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
028700980209     C     WDT8          ORGT      PARDTF                                       *SUPERATA DATA FINE
028800000000     C                   MOVEL     '1'           $FINE
028900000000X   2C                   ELSE
029000000000     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
029100000000E   2C                   ENDIF
029200000000E   1C                   ENDDO
029300000000     C*
029400000000IF  1C     $FINE         IFEQ      '0'                                          *RECORD VALIDO
029500000000     C                   ADD       1             CONLET                         *CONTEGGIO
029600000000E   1C                   ENDIF
029700000000     C*
029800000000     C                   ENDSR
029900000000     C*------------------------------------------------------------------------*
030000000000     C* CHKREC - CONTROLLA VALIDITA' RECORD
030100000000     C*------------------------------------------------------------------------*
030200000000     C     CHKREC        BEGSR
030300000000     C*
030400000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
030500000000     C*
030600000000     C* CONTROLLA IL TIPO BOLLA
030700000000IF  1C     TASTBL        IFEQ      'AP'
030800000000     C     TASTBL        OREQ      'AS'
030900000000     C     TASTBL        OREQ      'A3'
031000000000     C     TASTBL        OREQ      'FS'
031100000000     C     TASTBL        OREQ      'F3'
031200000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
031300000000E   1C                   ENDIF
031400990810     C*
031500990810     C* MITTENTE NON CODIFICATO, ESCLUDE
031600990810     C                   MOVE      TASCCM        WCLI4             4
031700990810IF  1C     WCLI4         IFEQ      *ZEROS
031800990810     C     WCLI4         OREQ      '8888'
031900990810     C     WCLI4         OREQ      '9999'
032000990810     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
032100990810X   1C                   ELSE
032200990810     C                   Z-ADD     TASCCM        WCLI              7 0
032300990810E   1C                   ENDIF
032400000000     C*
032500000000     C                   ENDSR
032600000000     C*------------------------------------------------------------------------*
032700000000     C* AGGSTP - AGGIORNA IL FILE WFSTP01L (NUMERO DI STOP)
032800000000     C*------------------------------------------------------------------------*
032900000000     C     AGGSTP        BEGSR
033000010418     c*
033100010418     c*
033200970129     C* RECUPERA I DATI CLIENTE
033300970129     C                   Z-ADD     1             I
033400970129     C     WCLI          LOOKUP    CLI(I)                                 99
033500970129IF  3C     *IN99         IFEQ      *ON
033600970129     C                   Z-ADD     CUN(I)        STPKSC
033700970129X   3C                   ELSE
033800970129     C                   Z-ADD     WCLI          STPKSC
033900970129E   3C                   ENDIF
034000010418     C*
034100010418     c* 36 ON = BOLLA DISK C
034200010418     c                   z-add     1             ii
034300010418     c                   clear                   wtmt
034400010418     c     tasctm        lookup    tmc(ii)                                36
034500010418     c   36              move      tmt(ii)       wtmt              1
034600000000     C*
034700970130     C                   MOVE      *ZERO         STPNUA                                        PEDIZ
034800970130     C                   MOVE      *ZERO         STPIMA
034900010418     C                   MOVE      *ZERO         STPNUC                                        PEDIZ
035000010418     C                   MOVE      *ZERO         STPIMC
035100010418     C                   MOVE      *ZERO         STPNUK                                        PEDIZ
035200010418     C                   MOVE      *ZERO         STPIMK
035300970130     C                   MOVE      *ZERO         STPNUB                                        PEDIZ
035400970130     C                   MOVE      *ZERO         STPIMB
035500970130     C                   MOVE      *ZERO         STPNUM                                        PEDIZ
035600970130     C                   MOVE      *ZERO         STPIMP
035700970130     C                   MOVE      *ZERO         STPNCA                                        PEDIZ
035800000316     C                   MOVE      *BLANKS       STPT3R
035900110624     C***                MOVE      *BLANKS       STPTRK
036000110624     C                   MOVE      '?'           STPTRK
036100970130     C*
036200970129     C                   Z-ADD     TASAAS        DSAAS
036300970129     C                   Z-ADD     TASMGS        DSMGS
036400970129     C                   Z-ADD     DSDAT8        STPDAT
036500000000     C* CONTROLLA SE RECORD GIA' ESISTENTE
036600000000     C     KEYSTP        CHAIN     WFSTP01L                           99
036700000000     C*
036800000000IF  1C     *IN99         IFEQ      *OFF                                         E S I S T E
036900000000IF  2C     TASSOP        IFEQ      '§'
037000000000     C     TASSOP        OREQ      '%'
037100010423     C     TASSOP        OREQ      ':'
037200000000IF  3C     TASNRS        IFEQ      *ZEROS
037300010423     c* disk a o disk a+c bartolini --> memorizzo come disk A
037400010418     c     *in36         ifeq      *off
037500010423     c     wtmt          oreq      'B'
037600970129     C                   ADD       1             STPNUA                         *AGGIUNGE UNA SPEDIZ
037700970129     C                   ADD       TASNCL        STPIMA                         *COLLI
037800010418     c                   else
037900010423     c* disk c cliente
038000010418     C                   ADD       1             STPNUK                         *AGGIUNGE UNA SPEDIZ
038100010418     C                   ADD       TASNCL        STPIMK                         *COLLI
038200010418     c                   endif
038300000000X   3C                   ELSE
038400970129     C                   ADD       1             STPNUB                         *AGGIUNGE UNA SPEDIZ
038500970129     C                   ADD       TASNCL        STPIMB                         *COLLI
038600000000E   3C                   ENDIF
038700000000E   2C                   ENDIF
038800010423     c* disk c bartolini
038900010423     c     *in36         ifeq      *on
039000010423     c     wtmt          andeq     'B'
039100010423     C                   ADD       1             STPNUC                         *AGGIUNGE UNA SPEDIZ
039200010423     C                   ADD       TASNCL        STPIMC                         *COLLI
039300010423     c                   endif
039400000000     C                   ADD       1             STPNUM                         *AGGIUNGE UNA SPEDIZ
039500000000     C                   ADD       TASNCL        STPIMP                         *COLLI
039600970129IF  3C     TASFCA        IFEQ      'S'
039700970129     C                   ADD       1             STPNCA                         *CONTRASSEGNO  PEDIZ
039800970129E   2C                   ENDIF
039900000000     C                   UPDATE    WFSTP                                        *AGGIORNA
040000000000     C*
040100000000X   1C                   ELSE                                                   N O N   E S I S T E
040200000000     C*
040300970129IF  2C     TASSOP        IFEQ      '§'
040400970129     C     TASSOP        OREQ      '%'
040500010423     C     TASSOP        OREQ      ':'
040600970129IF  3C     TASNRS        IFEQ      *ZEROS
040700010418     c     *in36         ifeq      *off
040800010423     c     wtmt          oreq      'B'
040900970129     C                   Z-ADD     1             STPNUA                         *AGGIUNGE UNA SPEDIZ
041000970129     C                   Z-ADD     TASNCL        STPIMA                         *COLLI
041100010418     c                   else
041200010418     C                   Z-ADD     1             STPNUK                         *AGGIUNGE UNA SPEDIZ
041300010418     C                   Z-ADD     TASNCL        STPIMK                         *COLLI
041400010418     c                   endif
041500970129X   3C                   ELSE
041600970129     C                   Z-ADD     1             STPNUB                         *AGGIUNGE UNA SPEDIZ
041700970129     C                   Z-ADD     TASNCL        STPIMB                         *COLLI
041800970129E   3C                   ENDIF
041900970129E   2C                   ENDIF
042000010423     c* disk c bartolini
042100010423     c     *in36         ifeq      *on
042200010423     c     wtmt          andeq     'B'
042300010423     C                   Z-ADD     1             STPNUC                         *AGGIUNGE UNA SPEDIZ
042400010423     C                   Z-ADD     TASNCL        STPIMC                         *COLLI
042500010423     c                   endif
042600970129     C                   Z-ADD     1             STPNUM                         *AGGIUNGE UNA SPEDIZ
042700970129     C                   Z-ADD     TASNCL        STPIMP                         *COLLI
042800970129IF  3C     TASFCA        IFEQ      'S'
042900000517     C                   Z-ADD     1             STPNCA                         *CONTRASSEGNO  PEDIZ
043000970129E   2C                   ENDIF
043100970130     C                   MOVEL     STPKSC        STPFIP
043200970130     C                   MOVEL     STPKSC        KORFIL
043300970129     C     KORFIL        CHAIN     AZORG01L                           97
043400971128     C*
043500971128     C* REPERISCE IL TERMINAL
043600110325     C*                  CLEAR                   DSLV55
043700110325     C*                  MOVEL     'P'           D55TPT
043800110325     C*                  Z-ADD     ORGFIL        D55LIN
043900110325     C*                  Z-ADD     DATCOR        D55DRF
044000110325     C*                  CALL      'FNLV55R'
044100110325     C*                  PARM                    DSLV55
044200110325IF  2C*    D55ERR        IFNE      *BLANKS
044300110325     C*                  Z-ADD     ORGFIL        D55TFP
044400110325E   2C*                  ENDIF
044500110325     C*                  MOVEL     D55TFP        STPAS4
044600110325     C                   MOVEL     orgcar        STPAS4
044700970129     C                   Z-ADD     STPKSC        DEPKSC
044800000000     C                   EXSR      DECCLI                                       *DESCRIZIONE CLIENTE
044900000000     C                   MOVEL     DESCLI        STPDCL
045000000000     C                   WRITE     WFSTP                                        *INSERISCE
045100000000E   1C                   ENDIF
045200000000     C*
045300000000     C                   ENDSR
045400970131     C*------------------------------------------------------------------------*
045500970131     C* CALCOLA PERCENTUALI
045600970131     C*------------------------------------------------------------------------*
045700970131     C     PERC          BEGSR
045800990204     C* SUL TOTALE
045900970131     C     STTSPA        ADD       STTSPB        WSPDSK            8 0
046000010423     c                   add       sttspk        wspdsk
046100970131     C     WSPDSK        IFGT      *ZERO
046200990204     C     WSPDSK        MULT      100           WW10             10 0
046300970131     C     WW10          DIV(H)    STTSPT        WPDSK             3 0          % DSK
046400970131     C                   ENDIF
046500970131     C     STTCOB        IFGT      *ZERO
046600990204     C     STTCOB        MULT      100           WW10
046700970131     C     WW10          DIV(H)    STTCOT        WPETI             3 0          % DISK B
046800970131     C                   ENDIF
046900990204     C* SOLO SUI "POTENZIALI"
047000990204     C     S2TSPA        ADD       S2TSPB        WSPDSK
047100010423     c                   add       s2tspk        wspdsk
047200990204     C     WSPDSK        IFGT      *ZERO
047300990204     C     WSPDSK        MULT      100           WW10
047400990204     C     WW10          DIV(H)    S2TSPT        W2DSK             3 0          % DSK
047500990204     C                   ENDIF
047600990204     C     S2TCOB        IFGT      *ZERO
047700990204     C     S2TCOB        MULT      100           WW10
047800990204     C     WW10          DIV(H)    S2TCOT        W2ETI             3 0          % DISK B
047900990204     C                   ENDIF
048000970131     C                   ENDSR
048100000000     C*------------------------------------------------------------------------*
048200110624     C* STAMPA - LEGGE IL FILE DI LAVORO E STAMPA
048300000000     C*------------------------------------------------------------------------*
048400000000     C     STAMPA        BEGSR
048500010626     C*
048600000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
048700000000     C                   EXSR      SETSTP
048800000000     C*
048900000000     C* CICLO FINO A FINE FILE DI LAVORO
049000000000DO  1C     $FINE         DOWEQ     '0'
049100000000     C*
049200970130     C* STAMPO TOTALI FILIALE PRECEDENTE
049300970130     C     DEPFIP        IFGT      *ZERO
049400990204     C     *IN88         ANDEQ     '1'                                          ALMENO UN CLI
049500010626     C     *IN14         ANDEQ     *OFF
049600970131     C                   EXSR      PERC                                         PERCENTUALI
049700970130     C                   EXCEPT    TFIL
049800970130     C                   ENDIF
049900970130     C* AZZERA I TOTALI FILIALE
050000990204     C                   SETOFF                                       88
050100971021     C                   MOVE      *ZERO         WBLCF
050200971021     C                   MOVE      *ZERO         WCAPVF
050300971021     C                   MOVE      *ZERO         WCAPNF
050400971021     C                   MOVE      *ZERO         WCAPLF
050500110401     C***                MOVE      *ZERO         WCATRF
050600970130     C                   MOVE      *ZERO         WSPDSK
050700970130     C                   MOVE      *ZERO         WPDSK
050800970130     C                   MOVE      *ZERO         WPETI
050900970130     C                   MOVE      *ZEROS        STTSPT
051000970130     C                   MOVE      *ZEROS        STTCOT
051100970130     C                   MOVE      *ZEROS        STTSPA
051200010419     C                   MOVE      *ZEROS        STTSPK
051300970130     C                   MOVE      *ZEROS        STTSPB
051400970130     C                   MOVE      *ZEROS        STTSPC
051500970130     C                   MOVE      *ZEROS        STTCOB
051600970130     C                   MOVE      *ZEROS        STTCLI
051700970130     C                   MOVE      *ZEROS        STTCLA
051800970130     C                   MOVE      *ZEROS        STTCLB
051900010423     C                   MOVE      *ZEROS        STTCLC
052000970130     C*
052100990204     C                   MOVE      *ZERO         W2DSK
052200990204     C                   MOVE      *ZERO         W2ETI
052300990204     C                   MOVE      *ZEROS        S2TSPT
052400990204     C                   MOVE      *ZEROS        S2TCOT
052500990204     C                   MOVE      *ZEROS        S2TSPA
052600010419     C                   MOVE      *ZEROS        S2TSPK
052700990204     C                   MOVE      *ZEROS        S2TSPB
052800990204     C                   MOVE      *ZEROS        S2TSPC
052900990204     C                   MOVE      *ZEROS        S2TCOB
053000990204     C                   MOVE      *ZEROS        S2TCLI
053100990204     C                   MOVE      *ZEROS        S2TCLA
053200990204     C                   MOVE      *ZEROS        S2TCLB
053300010423     C                   MOVE      *ZEROS        S2TCLC
053400970130     C* OPERAZIONI PER NUOVA FILIALE
053500970130     C                   Z-ADD     STPFIP        DEPFIP            3 0
053600970130     C* CICLO FINO A ROTTURA DI FILIALE
053700000000DO  2C     $FINE         DOWEQ     '0'
053800970130     C     STPFIP        ANDEQ     DEPFIP
053900000000     C*
054000000000     C* OPERAZIONI PER NUOVO CLIENTE
054100000000     C                   Z-ADD     STPKSC        DEPKSC            7 0
054200110401     C***                MOVEL     *BLANKS       STATRK            1
054300000517     C                   MOVEL     *BLANKS       STAT3R            1
054400000000     C*
054500000000     C* CICLO FINO A ROTTURA DI CLIENTE
054600000000DO  3C     $FINE         DOWEQ     '0'
054700970130     C     STPFIP        ANDEQ     DEPFIP
054800000000     C     STPKSC        ANDEQ     DEPKSC
054900000000     C*
055000000000     C* MEMORIZZA LE SPEDIZIONI
055100000000     C                   ADD       1             STAGIO
055200000000     C                   ADD       STPNUM        STASPT
055300000000     C                   ADD       STPIMP        STACOT
055400970129     C                   ADD       STPNUA        STASPA
055500970129     C                   ADD       STPNUB        STASPB
055600110817     C****               ADD       STPNUC        STASPH
055700010418     C                   ADD       STPNUK        STASPK
055800970130     C                   ADD       STPNCA        STASPC
055900970130     C                   ADD       STPIMB        STACOB
056000110624     C***   TPTRK        IFNE      *BLANKS
056100110624     C***                MOVEL     STPTRK        STATRK
056200110401     C***                ENDIF
056300000517     C     STPT3R        IFNE      *BLANKS
056400000517     C                   MOVEL     STPT3R        STAT3R
056500000517     C                   ENDIF
056600000322     C     WINDSK        IFEQ      *ZERO
056700000517     C     STPNUA        IFGT      *ZERO
056800000517     C     STPNUB        ORGT      *ZERO
056900010418     C     STPNUK        ORGT      *ZERO
057000970203     C                   MOVE      STPDAT        WINDSK
057100000517     C                   ENDIF
057200970203     C                   ENDIF
057300110624     C                   MOVEL     STPTRK        STAVAB            1
057400000000     C*
057500000000     C* LEGGE IL RECORD SUCCESSIVO
057600000000     C                   EXSR      LETSTP
057700000000E   3C                   ENDDO
057800000000     C*
057900000000     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
058000000000IF  3C     *INOF         IFEQ      *ON
058100010626     C  N14              EXCEPT    TESTA
058200010626     C   14              EXCEPT    TESTAT
058300000000E   3C                   ENDIF
058400000000     C*
058500000000     C* CALCOLA LE SPEDIZIONI AL GIORNO
058600970129     C     STASPT        DIV       STAGIO        STASPG            8 0
058700970129     C     STACOT        DIV       STAGIO        STACOG            8 0
058800000000     C*
058900000000     C* RECUPERA I DATI CLIENTE
059000000000     C                   Z-ADD     1             I                 6 0
059100000000     C     DEPKSC        LOOKUP    CLI(I)                                 99
059200970129IF  3C     *IN99         IFEQ      *OFF
059300000000     C                   MOVEL     *BLANKS       STADET            1            *DETT.SEGN.
059400990225     C                   MOVE      *BLANKS       STACAP            1            *TIPO CAPPARIO
059500000000     C                   MOVEL     *BLANKS       STACBA            5            *SUPP. CLI > BART
059600990225     C                   MOVEL     *BLANKS       STAPES            1            GEST.PESO
059700990225     C                   MOVE      *BLANK        STACAP
059800010920     C                   MOVEL     *BLANKS       STABAC            5            *SUPP. CLI > BART
059900000000X   3C                   ELSE
060000000000     C                   MOVEL     DET(I)        STADET                         *DETT.SEGN.
060100000000     C                   MOVEL     CBA(I)        STACBA                         *SUPP. CLI > BART
060200990225     C                   MOVEL     PES(I)        STAPES                         GEST.PESO
060300010920     C                   MOVEL     BAC(I)        STABAC                         *SUPP. CLI > BART
060400990225     C                   MOVEL     '1'           STACAP                         AZIST
060500971024     C     CAP(I)        IFEQ      'S'
060600990225     C                   MOVEL     '2'           STACAP                         AZCAP
060700971024     C                   ENDIF
060800971024     C     CAP(I)        IFEQ      'L'
060900990225     C                   MOVEL     '3'           STACAP                         AZCPC
061000971024     C                   ENDIF
061100000000E   3C                   ENDIF
061200000000     C*
061300970130     C* VAS DA FILIALE (DATI CONS./GIACENZE)
061400970130     C*
061500970130     C                   MOVE      *BLANK        DS3K
061600970130     C                   MOVEL     '3K'          KTBCOD
061700970130     C                   MOVEL     DEPKSC        KTBKEY
061800970130     C     KEYTAC        CHAIN     TABEL00F                           99
061900970130     C  N99              MOVEL     TBLUNI        DS3K
062000970130     C*
062100970130     C* VAS DA SEDE (C/A / FATTURE)
062200970130     C*
062300970130     C                   MOVE      *BLANK        DS4C
062400970130     C                   MOVEL     '4C'          KTBCOD
062500970130     C                   MOVEL     DEPKSC        KTBKEY
062600970130     C     KEYTAC        CHAIN     TABEL00F                           99
062700970130     C  N99              MOVEL     TBLUNI        DS4C
062800970130     C*
062900970130     C* COMODATI
063000970130     C*
063100970130     C                   MOVE      *BLANK        DS3R
063200970130     C                   MOVEL     '3R'          KTBCOD
063300970130     C                   MOVEL     DEPKSC        KTBKEY
063400970130     C     KEYTAC        CHAIN     TABEL00F                           99
063500970130     C  N99              MOVEL     TBLUNI        DS3R
063600970130     C*
063700970131     C                   MOVE      'S'           WSCART            1
063800990204     C                   MOVE      'S'           WSCAR2            1
063900000517     C* SOLO CLIENTI CON VAS BOLLE  SOLUZ.B
064000000322     C     PARPAR        IFEQ      'B'                                          SOLO VAS BOLLEB
064100980606     C     STASPB        ANDEQ     *ZERO
064200980606     C                   MOVEL     'N'           WSCART
064300980606E   1C                   ENDIF
064400970131     C* SOLO CLIENTI CON VAS BOLLE
064500970131     C     PARPAR        IFEQ      'V'                                          SOLO VAS
064600970131     C     STASPA        ANDEQ     *ZERO
064700970131     C     STASPB        ANDEQ     *ZERO
064800010423     C     STASPK        ANDEQ     *ZERO
064900970131     C                   MOVEL     'N'           WSCART
065000970131E   1C                   ENDIF
065100970131     C* SOLO CLIENTI SENZA VAS BOLLE
065200970131     C     PARPAR        IFEQ      'N'                                          SOLO NO VAS
065300970131     C     STASPA        IFGT      *ZERO
065400970131     C     STASPB        ORGT      *ZERO
065500010423     C     STASPK        ORGT      *ZERO
065600970131     C                   MOVEL     'N'           WSCART
065700970131     C                   ENDIF
065800970131     C                   ENDIF
065900970131     C*
066000970919     C* SE NUMERO BOLLE/COLLI MEDIO INF. AL RICHIESTO, ESCLUDE
066100970131IF  1C     STASPG        IFLE      PARNBO
066200970919IF  1C     STACOG        ANDLE     PARNCO
066300970131     C     STASPA        ANDEQ     *ZERO
066400970131     C     STASPB        ANDEQ     *ZERO
066500010423     C     STASPK        ANDEQ     *ZERO
066600970131     C                   MOVEL     'N'           WSCART
066700990204     C                   MOVEL     'N'           WSCAR2
066800970131E   1C                   ENDIF
066900000322     C*
067000000322     C* SE LANCIATO "ANCHE SE SOLO T&T" ED IL RECOTD E' DI T&T
067100000322     C* By-Passo COMUNQUE I CONTROLLI PRECEDENTI
067200110401     C***  PARTRK        IFEQ      'S'
067300110401     C***  STATRK        ANDEQ     'X'
067400110401     C***                MOVEL     'S'           WSCART
067500110401     C***                MOVEL     'S'           WSCAR2
067600110401     C***                ENDIF
067700000517     C*
067800000517     C* SE IL CLIENTE E' DI QUELLI "3R" ==>
067900000517     C* By-Passo COMUNQUE I CONTROLLI PRECEDENTI
068000000517     C     STAT3R        IFNE      *BLANKS
068100000517     C                   MOVEL     'S'           WSCART
068200000517     C                   MOVEL     'S'           WSCAR2
068300000517     C                   ENDIF
068400000419     C*
068500000000     C* STAMPA IL TOTALE CLIENTE
068600970131     C     STASPB        IFLE      *ZERO
068700970131     C                   MOVEL     *BLANKS       STACAP            1                        IO
068800010920     C                   MOVEL     *BLANKS       STABAC                                     IO
068900970131     C                   MOVEL     *BLANKS       STADET            1                        IO
069000970131     C                   ENDIF
069100971021     C     STASPB        IFLE      *ZERO
069200971021     C     STASPA        ANDLE     *ZERO
069300010423     C     STASPK        ANDLE     *ZERO
069400971021     C                   MOVEL     *BLANKS       STAVAB                                     IO
069500010322     C                   MOVEL     *BLANKS       STAcba                                     IO
069600971021     C                   ENDIF
069700970131     C                   EXSR      DECCLI
069800970203     C*
069900970203     C* STAMPO IL MESE DI INZIO DSK SE SUPERIORE AL DAL
070000970203     C                   MOVE      *BLANK        WINDSD            2
070100970203     C                   MOVEL     WINDSK        WW6DSK            6 0
070200970203     C                   MOVEL     PARDTI        WW6DTI            6 0
070300970203     C     WW6DSK        IFGT      WW6DTI
070400970203     C                   MOVE      WW6DSK        WINDSD
070500970203     C                   ENDIF
070600970205     C* TOTALIZZO IL NUMERO DI CLIENTI X TIPO SUPPORTO
070700970205     C     STACBA        IFGT      *BLANKS                                                   BART
070800970205     C                   MOVEL     STACBA        WAA2              2
070900970205     C                   MOVEL     STACBA        WSTCBA            5
071000970205     C     WAA2          IFEQ      'HL'                                         HALLEY  XX
071100970205     C     WAA2          OREQ      'PC'                                         PC+HALL XX
071200000329     C     WAA2          OREQ      'WW'                                         PC+HALL XX
071300980131     C                   MOVE      '  '          WSTCBA
071400970205     C                   ENDIF
071500970205     C                   Z-ADD     1             I
071600970205     C     WSTCBA        LOOKUP    TIB(I)                                 99
071700970205     C     *IN99         IFEQ      *OFF
071800970205     C                   Z-ADD     1             I
071900970205     C     *BLANK        LOOKUP    TIB(I)                                 99
072000970205     C                   ENDIF
072100970205     C                   ADD       1             NIB(I)
072200970205     C                   MOVEL     WSTCBA        TIB(I)                                     IO
072300970205     C                   ENDIF
072400971021     C* TOTALIZZO IL NUMERO DI CLIENTI
072500110624     C* CON VAS VAB/VAT STANDARD
072600110624     C     STAVAB        IFEQ      'S'                                                       BART
072700971021     C                   ADD       1             WBLCF             5 0
072800971021     C                   ADD       1             WBLCT             5 0
072900971021     C                   ENDIF
073000971021     C* CON CAPPARIO:                                                   BART
073100971021     C     STASPB        IFGT      *ZERO
073200971021     C* CON CAPPARIO AZIST
073300971024     C     STACAP        IFEQ      '1'                                                       BART
073400971021     C                   ADD       1             WCAPVF            5 0
073500971021     C                   ADD       1             WCAPVT            5 0
073600971021     C                   ENDIF
073700971021     C* CON CAPPARIO AZCAP
073800971024     C     STACAP        IFEQ      '2'                                                       BART
073900971021     C                   ADD       1             WCAPNF            5 0
074000971021     C                   ADD       1             WCAPNT            5 0
074100971021     C                   ENDIF
074200971021     C* CON CAPPARIO AZCPC
074300971024     C     STACAP        IFEQ      '3'                                                       BART
074400971021     C                   ADD       1             WCAPLF            5 0
074500971021     C                   ADD       1             WCAPLT            5 0
074600000419     C                   ENDIF
074700000419     C                   ENDIF
074800000419     C* TOTALIZZO I CLIENTI ABILITATI AL TRACK & TRACING
074900110401     C***  STATRK        IFEQ      'X'
075000110401     C***                ADD       1             WCATRF            5 0
075100110401     C***                ADD       1             WCATRT            5 0
075200110401     C***                ENDIF
075300990204     C* TOTALIZZO SEMPRE MA STAMPO SOLO SE RIENTRA NEI PARAMETRI        BART
075400990204     C     WSCART        IFEQ      'S'
075500990204     C* PER PRIMO CLIENTE STAMPATO DELLA FILIALE STAMPO TESTATA         BART
075600990204     C     *IN88         IFEQ      '0'
075700990204     C                   EXSR      DECFIP
075800010626     C     *IN14         IFEQ      *OFF
075900990204     C                   EXCEPT    TESTA                                        *SALTO PAGINA
076000000710     C                   EXCEPT    FIP                                          *DECODIFICA FILIALE
076100990204     C                   EXCEPT    SPAZIO                                       *DECODIFICA FILIALE
076200010627     C                   SETON                                        88
076300010626     C                   ELSE
076400010627     C     STASPB        IFEQ      0                                            TESTATA SOLO SE RIGA
076500010627     c     §3rnot        ANDNE     *blanks
076600010626     C                   EXCEPT    TESTAT                                       *SALTO PAGINA
076700010626     C                   EXCEPT    FIP1                                         *DECODIFICA FILIALE
076800010626     C                   EXCEPT    SPAZIO1                                      *DECODIFICA FILIALE
076900010627     C                   SETON                                        88
077000010626     C                   ENDIF
077100010627     C                   ENDIF
077200990204     C                   ENDIF
077300990204     C*
077400010626     C  N14              EXCEPT    RIGA
077500010626     C     STASPB        IFEQ      0
077600010626     c     §3rnot        ANDNE     *blanks
077700010626     C   14              EXCEPT    RIGA2
077800010626     C                   ENDIF
077900010419     c* se presente materiale in comodato lo stampo in seconda riga
078000010419     c     §3rnot        ifne      *blanks
078100010626     c  N14              except    RIGA1
078200010419     c                   endif
078300010627     C*****              SETON                                        88
078301110817     c* scarico i dati stampati nel file di work
078302110817     c                   if        not *in14 or (staspb=0 and §3rnot<>*blanks)
078303110817     c                   exsr      sr_wrtstv
078304110817     c                   endif
078400990204     C                   ENDIF
078500000000     C*
078600990204     C**  TUTTI E NON SOLO I "POTENZIALI"
078700990204     C*
078800970130     C* TOTALIZZO COME FILIALE
078900970130     C                   ADD       STASPT        STTSPT            8 0
079000970130     C                   ADD       STACOT        STTCOT            8 0
079100970130     C                   ADD       STASPA        STTSPA            8 0
079200970130     C                   ADD       STASPB        STTSPB            8 0
079300010419     C                   ADD       STASPK        STTSPK            8 0
079400970130     C                   ADD       STASPC        STTSPC            8 0
079500970130     C                   ADD       STACOB        STTCOB            8 0
079600970130     C                   ADD       1             STTCLI            6 0          TOT.CLI
079700970130     C     STASPA        IFGT      *ZERO
079800970130     C     STASPB        ANDEQ     *ZERO
079900010423     C     STASPK        ANDEQ     *ZERO
080000970130     C                   ADD       1             STTCLA            6 0          CLI DSK A
080100970130     C                   ENDIF
080200010423     C     staspk        ifgt      *zeros
080300010423     c     staspb        andeq     *zeros
080400010423     c                   add       1             sttclc            6 0
080500010423     c                   endif
080600970130     C     STASPB        IFGT      *ZERO
080700970130     C                   ADD       1             STTCLB            6 0          CLI DSK B
080800970130     C                   ENDIF
080900970130     C* TOTALIZZO COME FINALE
081000970130     C                   ADD       STASPT        STFSPT            8 0
081100970130     C                   ADD       STACOT        STFCOT            8 0
081200970130     C                   ADD       STASPA        STFSPA            8 0
081300010419     C                   ADD       STASPK        STFSPK            8 0
081400970130     C                   ADD       STASPB        STFSPB            8 0
081500970130     C                   ADD       STASPC        STFSPC            8 0
081600970130     C                   ADD       STACOB        STFCOB            8 0
081700970130     C                   ADD       1             STFCLI            6 0          TOT.CLI
081800970130     C     STASPA        IFGT      *ZERO
081900970130     C     STASPB        ANDEQ     *ZERO
082000010423     C     STASPK        ANDEQ     *ZERO
082100970130     C                   ADD       1             STFCLA            6 0          CLI DSK A
082200970130     C                   ENDIF
082300010423     C     staspk        ifgt      *zeros
082400010423     c     staspb        andeq     *zeros
082500010423     c                   add       1             stfclc            6 0
082600010423     c                   endif
082700970130     C     STASPB        IFGT      *ZERO
082800970130     C                   ADD       1             STFCLB            6 0          CLI DSK B
082900970130     C                   ENDIF
083000990204     C**  SOLO I "POTENZIALI"
083100990204     C*
083200990204     C     WSCAR2        IFEQ      'S'
083300990204     C* TOTALIZZO COME FILIALE
083400990204     C                   ADD       STASPT        S2TSPT            8 0
083500990204     C                   ADD       STACOT        S2TCOT            8 0
083600990204     C                   ADD       STASPA        S2TSPA            8 0
083700010419     C                   ADD       STASPK        S2TSPK            8 0
083800990204     C                   ADD       STASPB        S2TSPB            8 0
083900990204     C                   ADD       STASPC        S2TSPC            8 0
084000990204     C                   ADD       STACOB        S2TCOB            8 0
084100990204     C                   ADD       1             S2TCLI            6 0          TOT.CLI
084200990204     C     STASPA        IFGT      *ZERO
084300990204     C     STASPB        ANDEQ     *ZERO
084400010424     C     STASPK        ANDEQ     *ZERO
084500990204     C                   ADD       1             S2TCLA            6 0          CLI DSK A
084600990204     C                   ENDIF
084700010423     C     staspk        ifgt      *zeros
084800010423     c     staspb        andeq     *zeros
084900010423     c                   add       1             s2tclc            6 0
085000010423     c                   endif
085100990204     C     STASPB        IFGT      *ZERO
085200990204     C                   ADD       1             S2TCLB            6 0          CLI DSK B
085300990204     C                   ENDIF
085400990204     C* TOTALIZZO COME FINALE
085500990204     C                   ADD       STASPT        S2FSPT            8 0
085600990204     C                   ADD       STACOT        S2FCOT            8 0
085700990204     C                   ADD       STASPA        S2FSPA            8 0
085800010419     C                   ADD       STASPK        S2FSPK            8 0
085900990204     C                   ADD       STASPB        S2FSPB            8 0
086000990204     C                   ADD       STASPC        S2FSPC            8 0
086100990204     C                   ADD       STACOB        S2FCOB            8 0
086200990204     C                   ADD       1             S2FCLI            6 0          TOT.CLI
086300990204     C     STASPA        IFGT      *ZERO
086400990204     C     STASPB        ANDEQ     *ZERO
086500010426     C     STASPK        ANDEQ     *ZERO
086600990204     C                   ADD       1             S2FCLA            6 0          CLI DSK A
086700990204     C                   ENDIF
086800010423     C     staspk        ifgt      *zeros
086900010423     c     staspb        andeq     *zeros
087000010423     c                   add       1             s2fclc            6 0
087100010423     c                   endif
087200990204     C     STASPB        IFGT      *ZERO
087300990204     C                   ADD       1             S2FCLB            6 0          CLI DSK B
087400990204     C                   ENDIF
087500990204     C*
087600990204     C                   ENDIF
087700970130     C*
087800970130     C* AZZERA I TOTALI CLIENTE
087900970130     C                   MOVE      *ZEROS        STASPT            8 0
088000970130     C                   MOVE      *ZEROS        STACOT            8 0
088100990225     C                   MOVE      *ZEROS        STAGIO            4 0
088200970130     C                   MOVE      *ZEROS        STASPA            8 0
088300110817     C***                MOVE      *ZEROS        STASPH            8 0
088400010419     C                   MOVE      *ZEROS        STASPK            8 0
088500970130     C                   MOVE      *ZEROS        STASPB            8 0
088600970130     C                   MOVE      *ZEROS        STASPC            8 0
088700970130     C                   MOVE      *ZEROS        STACOB            8 0
088800970203     C                   MOVE      *ZEROS        WINDSK            8 0
088900970130E   2C                   ENDDO                                                  *ROTTURA AS/400
089000000000E   1C                   ENDDO                                                  *FINE FILE
089100000000     C*
089200000000     C                   ENDSR
089300000000     C*------------------------------------------------------------------------*
089400000000     C* SETSTP - POSIZIONAMENTO E PRIMA LETTURA DEL FILE
089500000000     C*------------------------------------------------------------------------*
089600000000     C     SETSTP        BEGSR
089700000000     C*
089800000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
089900000000     C                   MOVEL     '0'           $FINE
090000970130     C                   MOVE      *ZEROS        STPAS4
090100970130     C                   MOVE      *ZEROS        STPFIP
090200000000     C                   MOVEL     *BLANKS       STPDCL
090300970130     C                   MOVE      *ZEROS        STPKSC
090400970130     C                   MOVE      *ZEROS        STPDAT
090500000000     C     KEYST2        SETLL     WFSTP02L                           99        *FINE-ARCHIV
090600000000     C*
090700000000IF  1C     *IN99         IFEQ      *ON
090800000000     C                   MOVEL     '1'           $FINE
090900000000X   1C                   ELSE
091000000000     C                   EXSR      LETSTP
091100000000E   1C                   ENDIF
091200000000     C*
091300000000     C                   ENDSR
091400000000     C*------------------------------------------------------------------------*
091500000000     C* LETSTP - LETTURA PROSSIMO RECORD
091600000000     C*------------------------------------------------------------------------*
091700000000     C     LETSTP        BEGSR
091800000000     C*
091900000000     C* LEGGO FINO A:
092000000000     C*     - FINE FILE O SUPERATO LIMITE FINALE
092100000000     C*     - TROVATO RECORD VALIDO
092200000000     C                   MOVEL     'N'           WRECOK
092300000000DO  1C     $FINE         DOWEQ     '0'
092400000000     C     WRECOK        ANDEQ     'N'
092500000000     C*
092600000000     C                   READ      WFSTP02L                               99
092700000000     C*
092800000000IF  2C     *IN99         IFEQ      *ON                                          *FINA FILE
092900000000     C                   MOVEL     '1'           $FINE
093000000000X   2C                   ELSE
093100000000     C                   EXSR      CHKSTP                                       *CONTROLLA RECORD
093200000000E   2C                   ENDIF
093300000000E   1C                   ENDDO
093400000000     C*
093500000000     C                   ENDSR
093600000000     C*------------------------------------------------------------------------*
093700000000     C* CHKSTP - CONTROLLA VALIDITA' RECORD
093800000000     C*------------------------------------------------------------------------*
093900000000     C     CHKSTP        BEGSR
094000000000     C*
094100000000     C                   MOVEL     'S'           WRECOK
094200000000     C*
094300970131     C* CONTROLLO PERIODO
094400970131IF  1C     STPDAT        IFLT      PARDTI
094500970131     C     STPDAT        ORGT      PARDTF
094600000000     C                   MOVEL     'N'           WRECOK
094700000000E   1C                   ENDIF
094800000000     C*
094900000000     C                   ENDSR
095000000000     C*------------------------------------------------------------------------*
095100000000     C* DECCLI - DECODIFICA IL CLIENTE
095200000000     C*------------------------------------------------------------------------*
095300000000     C     DECCLI        BEGSR
095400980116     C*
095500980116     C                   CLEAR                   BS69DS
095600980116     C                   CLEAR                   ACODS
095601110901     C                   CLEAR                   clpds
095700980116     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
095800980116     C                   Z-ADD     DEPKSC        I69KAC                         *CLIENTE X CNACO
095801110901     C                   Z-ADD     DEPKSC        I69Kcp                         *CLIENTE X CNclp
095900980116     C                   CALL      'TIBS69R'
096000980116     C                   PARM                    BS69DS
096100980116     C                   PARM                    ACODS
096200980116     C                   PARM                    INDDS
096300980116     C                   PARM                    CLPDS
096400980116     C                   PARM                    CLSDS
096500980116IF  1C     O69ERR        IFEQ      '1'                                          *ERRORE
096600980116     C                   MOVEL     *ALL'*'       DESCLI           30
096700980116X   1C                   ELSE
096800980116     C                   MOVEL     ACORAG        DESCLI                         *NO ERRORE
096900130321IF  2C***  ACOABL        IFEQ      '8'
096901130321IF  2C     ACOABL        IFne      *blank
097000980116     C                   MOVE      '(*)'         DESCLI                         BLOCCATO
097100980116E   2C                   ENDIF
097200980116E   1C                   ENDIF
097300980116     C*
097400000000     C                   ENDSR
097500000000     C*------------------------------------------------------------------------*
097600970130     C* DECFIP - DECODIFICA AS/400 PARTENZA
097700000000     C*------------------------------------------------------------------------*
097800970130     C     DECFIP        BEGSR
097900000000     C*
098000970130     C                   MOVE      STPFIP        KORFIL
098100990225     C     DEPFIP        IFGT      *ZERO
098200990225     C                   MOVE      DEPFIP        KORFIL
098300990225     C                   ENDIF
098400990225     C                   MOVE      KORFIL        WORFIL            3 0
098500000000     C     KORFIL        CHAIN     AZORG01L                           99
098600000000IF  1C     *IN99         IFEQ      *ON
098700970130     C                   MOVEL     *ALL'*'       DESFIP           30
098800000000X   1C                   ELSE
098900970130     C                   MOVEL     ORGDES        DESFIP
099000971128     C*
099100971128     C* REPERISCE IL TERMINAL
099200110325     C***                CLEAR                   DSLV55
099300110325     C***                MOVEL     'P'           D55TPT
099400110325     C**                 Z-ADD     ORGFIL        D55LIN
099500110325     C**                 Z-ADD     DATCOR        D55DRF
099600110325     C**                 CALL      'FNLV55R'
099700110325     C**                 PARM                    DSLV55
099800110325IF  2C**   D55ERR        IFNE      *BLANKS
099900110325     C**                 Z-ADD     ORGFIL        D55TFP
100000110325E   2C**                 ENDIF
100100110325     C***                MOVEL     D55TFP        DESTRP            3 0
100200110325     C                   MOVEL     stpas4        DESTRP            3 0
100300000000E   1C                   ENDIF
100400000000     C*
100500000000     C                   ENDSR
100600000000     C*------------------------------------------------------------------------*
100700000000     C* CARICA LE TABELLE
100800000000     C*------------------------------------------------------------------------*
100900000000     C     CARTAB        BEGSR
101000000000     C*--------------------
101100000000     C* CODICI TRATTAMENTO MERCE
101200000000     C*--------------------
101300000000     C                   Z-ADD     0             I
101400010418     C                   Z-ADD     0             ii
101500000000     C                   Z-ADD     1             KTBUTE
101600000000     C                   MOVEL     '1B'          KTBCOD
101700000000     C     KEYTAB        SETLL     TABEL00F                           99
101800000000     C     KEYTAB        READE     TABEL00F                               99
101900000000DO  1C     *IN99         DOWEQ     *OFF
102000000000IF  2C     TBLFLG        IFEQ      *BLANKS
102100000000     C                   ADD       1             I
102200000000     C                   MOVEL     TBLKEY        CTM(I)                         *COD.TRATTAM.MERCE
102300000000     C                   MOVEL     TBLUNI        DS1B
102400000000     C                   MOVEL     §1BFG7        FG7(I)                         *DETT.SEGNAC. S/N
102500010418     c     §1bdkc        ifeq      'B'
102600010418     c     §1bdkc        oreq      'C'
102700010418     c                   add       1             ii                3 0
102800010418     c                   movel     tblkey        tmc(ii)
102900010418     c                   movel     §1bdkc        tmt(ii)
103000010418     c                   endif
103100000000E   2C                   ENDIF
103200000000     C     KEYTAB        READE     TABEL00F                               99
103300000000E   1C                   ENDDO
103400000000     C*--------------------
103500000000     C* CLIENTI CON DISCHETTO
103600000000     C*--------------------
103700000000     C                   Z-ADD     0             I
103800000000     C                   Z-ADD     1             KTBUTE
103900000000     C                   MOVEL     '3C'          KTBCOD
104000000000     C     KEYTAB        SETLL     TABEL00F                           99
104100000000     C     KEYTAB        READE     TABEL00F                               99
104200000000DO  1C     *IN99         DOWEQ     *OFF
104300970130     C                   MOVEL     TBLUNI        DS3C
104400000000IF  2C     TBLFLG        IFEQ      *BLANKS
104500980131     C**         §3COKD    ANDEQ'*'                        PARTITO
104600000000     C                   ADD       1             I
104700000000     C                   MOVEL     TBLKEY        CLI(I)                         *CLIENTE
104800000000     C                   MOVEL     §3CCBA        CBA(I)                         *SUPPORTO CLI>BART
104900990225     C                   MOVEL     §3CCKS        CUN(I)                         *CLI. UNIF.
105000990225     C                   MOVEL     §3CNWC        CAP(I)                         *CAPPARIO
105100990225     C                   MOVEL     §3CPES        PES(I)                         *GEST.PESO
105200010920     c                   movel     §3cbac        BAC(I)
105300000000     C                   Z-ADD     1             Y                 6 0
105400000000     C     §3CCTM        LOOKUP    CTM(Y)                                 98
105500000000IF  3C     *IN98         IFEQ      *OFF
105600000000     C                   MOVEL     *ALL'*'       DET(I)                         *CTM NON TROVATO
105700000000X   3C                   ELSE
105800000000     C                   MOVEL     FG7(Y)        DET(I)                         *CTM TROVATO
105900000000E   3C                   ENDIF
106000000000E   2C                   ENDIF
106100000000     C     KEYTAB        READE     TABEL00F                               99
106200000000E   1C                   ENDDO
106300000710     C*---
106400000710     C* ORGANIGRAMMA
106500000710     C*---
106600000710     C                   Z-ADD     *ZEROS        I
106700000710     C     *LOVAL        SETLL     AZORG01L
106800000710     C                   READ      AZORG01L                               98
106900000710DO  1C     *IN98         DOWEQ     *OFF
107000000710IF  2C     ORGFAG        IFNE      'V'
107100000710     C                   ADD       1             I
107200000710     C                   Z-ADD     ORGFIL        FIL(I)
107300000710     C                   MOVEL     ORGDES        DFIL(I)
107400000710     C                   MOVEL     ORGDE3        OG143
107500000710E   2C                   ENDIF
107600000710     C                   READ      AZORG01L                               98
107700000710E   1C                   ENDDO
107800000710     C*
107900000000     C                   ENDSR
108000970912     C*------------------------------------------------------------------------*
108100970912     C* ADD3R  - GENERA NEL FILE UN RECORD PER OGNI CLIENTE
108200970912     C*          PRESENTE NELLA TABELLA 3R E NON GIA' PRESENTE NEL FILE
108300970912     C*------------------------------------------------------------------------*
108400970912     C     ADD3R         BEGSR
108500970912     C                   SETOFF                                       9596
108600970912     C                   MOVEL     '3R'          KTBCOD
108700990226     C     KEYTAB        SETLL     TABEL00F
108800970912     C     KEYTAB        READE     TABEL00F                               96
108900970912     C     *IN96         DOWEQ     *OFF
109000970915     C                   MOVEL     TBLUNI        DS3R
109100970912     C     TBLFLG        IFEQ      *BLANKS
109200970915     C     §3RNOT        ANDGT     *BLANKS
109300990225     C                   MOVEL     TBLKEY        STPKSC
109400970915     C                   Z-ADD     PARDTF        STPDAT
109500970915     C     KEYSTP        CHAIN     WFSTP01L                           95
109600970915     C     *IN95         IFEQ      *ON
109700000316     C                   MOVE      *ZERO         STPNUA
109800000316     C                   MOVE      *ZERO         STPNUM
109900970912     C                   MOVE      *ZERO         STPIMA
110000990225     C                   MOVE      *ZERO         STPNUB
110100970912     C                   MOVE      *ZERO         STPIMB
110200010418     C                   MOVE      *ZERO         STPNUC
110300010418     C                   MOVE      *ZERO         STPIMC
110400010418     C                   MOVE      *ZERO         STPNUK
110500010418     C                   MOVE      *ZERO         STPIMK
110600970912     C                   MOVE      *ZERO         STPIMP
110700990225     C                   MOVE      *ZERO         STPNCA
110800000316     C                   MOVE      'X'           STPT3R
110900110624     C***                MOVE      *BLANKS       STPTRK
111000110624     C                   MOVE      '?'           STPTRK
111100970912     C*
111200970912     C                   MOVEL     STPKSC        STPFIP
111300970912     C                   MOVEL     STPKSC        KORFIL
111400970912     C     KORFIL        CHAIN     AZORG01L                           97
111500971128     C*
111600971128     C* REPERISCE IL TERMINAL
111700110325     C*                  CLEAR                   DSLV55
111800110325     C*                  MOVEL     'P'           D55TPT
111900110325     C*                  Z-ADD     ORGFIL        D55LIN
112000110325     C*                  Z-ADD     DATCOR        D55DRF
112100110325     C*                  CALL      'FNLV55R'
112200110325     C*                  PARM                    DSLV55
112300110325     C*    D55ERR        IFNE      *BLANKS
112400110325     C*                  Z-ADD     ORGFIL        D55TFP
112500110325     C*                  ENDIF
112600110325     C*                  MOVEL     D55TFP        STPAS4
112700110325     C                   MOVEL     orgcar        STPAS4
112800970912     C                   Z-ADD     STPKSC        DEPKSC
112900970912     C                   EXSR      DECCLI                                       *DESCRIZIONE CLIENTE
113000970912     C                   MOVEL     DESCLI        STPDCL
113100970912     C                   WRITE     WFSTP                                        *INSERISCE
113200970915     C                   ELSE
113300000317     C                   MOVE      'X'           STPT3R
113400970915     C                   UPDATE    WFSTP
113500970912     C                   ENDIF
113600970912     C                   ENDIF
113700970912     C     KEYTAB        READE     TABEL00F                               96
113800970912E   1C                   ENDDO
113900970912     C                   ENDSR
114000000316     C*------------------------------------------------------------------------*
114100110624     C* ADDTRK - GENERA NEL FILE UN RECORD PER OGNI CLIENTE PADRE
114200000316     C*          PRESENTE NEL ARCHIVIO DEI CLIENTI ABILITATI AL TRACK & TRACING
114300000316     C*------------------------------------------------------------------------*
114400110401     C*    ADDTRK        BEGSR
114500110401     C*                  SETOFF                                       9495
114600110401     C*                  MOVEL     'TT'          KVSISV
114700110401     C*    KEYVSS        SETLL     TIVSS04L
114800110401     C*    KEYVSS        READE     TIVSS04L                               94
114900110401     C*    *IN94         DOWEQ     *OFF
115000110401     C*    VSSVAT        IFEQ      *BLANKS
115100110401     C*    VSSDDE        ANDLE     PARDTF
115200110401     C*    VSSDSC        ANDGE     PARDTI
115300110401     C*                  MOVE      VSSKSU        STPKSC
115400110401     C*                  Z-ADD     PARDTF        STPDAT
115500110401     C*    KEYSTP        CHAIN     WFSTP01L                           95
115600110401     C*    *IN95         IFEQ      *ON
115700110401     C*    PARTRK        IFEQ      'S'
115800110401     C*                  MOVE      *ZERO         STPNUA
115900110401     C*                  MOVE      *ZERO         STPNUM
116000110401     C*                  MOVE      *ZERO         STPIMA
116100110401     C*                  MOVE      *ZERO         STPNUB
116200110401     C*                  MOVE      *ZERO         STPIMB
116300110401     C*                  MOVE      *ZERO         STPNUC
116400110401     C*                  MOVE      *ZERO         STPIMC
116500110401     C*                  MOVE      *ZERO         STPNUK
116600110401     C*                  MOVE      *ZERO         STPIMK
116700110401     C*                  MOVE      *ZERO         STPIMP
116800110401     C*                  MOVE      *ZERO         STPNCA
116900110401     C*                  MOVE      *BLANKS       STPT3R
117000110401     C*                  MOVE      'X'           STPTRK
117100110401     C*
117200110401     C*                  MOVEL     STPKSC        STPFIP
117300110401     C*                  MOVEL     STPKSC        KORFIL
117400110401     C*    KORFIL        CHAIN     AZORG01L                           97
117500000316     C*
117600110401     C*                  MOVEL     orgcar        STPAS4
117700110401     C*                  Z-ADD     STPKSC        DEPKSC
117800110401     C*                  EXSR      DECCLI                                       *DESCRIZIONE CLIENTE
117900110401     C*                  MOVEL     DESCLI        STPDCL
118000110401     C*                  WRITE     WFSTP                                        *INSERISCE
118100110401     C*                  ENDIF
118200110401     C*                  ELSE
118300110401     C*                  MOVE      'X'           STPTRK
118400110401     C*                  UPDATE    WFSTP
118500110401     C*                  ENDIF
118600110401     C*                  ENDIF
118700110401     C*    KEYVSS        READE     TIVSS04L                               94
118800110401E   1C*                  ENDDO
118900110401     C*                  ENDSR
119000110624     C*------------------------------------------------------------------------*
119100110624     C* UPDVAS - AGGIORNA OGNI CLIENTE PADRE CON L'INDICAZIONE DI VAS PERSONALIZZATO
119200110624     C*          O STANDARD X FILE VAB E VAT
119300110624     C*------------------------------------------------------------------------*
119400110624     C     UPDVAS        BEGSR
119500110624     C*
119600110624     C* Leggo tutta l'anagrafica clienti/tipi-file
119700110624     C     *START        SETLL     TIVIR01L
119800110624     C                   READ      TIVIR01L
119900110624     C*
120000110624     C                   MOVEL     '?'           wTipVAS           1
120100110624     C                   MOVEL     VIRKSC        savVIRKSC         8
120200110624     C*
120300110624     C                   DOW       not %eof(TIVIR01L)
120400110624     C*
120500110624     C* Gestisco rottura d codice x cliente
120600110624     C                   IF        VIRKSC <> savVIRKSC
120700110624     C*
120800110624     C* Aggiorno il wrk-file
120900110624     C                   MOVE      savVIRKSC     STPKSC
121000110624     C     STPKSC        SETLL     WFSTP01L
121100110624     C                   IF        %equal(WFSTP01L)
121200110624     C     STPKSC        READE     WFSTP01L
121300110624     C                   DOW       not %eof(WFSTP01L)
121400110704     C                   EVAL      STPTRK = wTipVAS
121500110704     C*
121600110704     C* RECUPERA I DATI CLIENTE X FORZATURA RISPETTO A TIPO INVIO DATI CLI2BRT
121700110704     C                   Z-ADD     1             I                 6 0
121800110704     C     STPKSC        LOOKUP    CLI(I)                                 99
121900110704     C     *IN99         IFNE      *OFF
122000110704     C                   IF        CBA(I) = 'SNA'   OR
122100110704     C                             CBA(I) = 'ESWEB' OR
122200110704     C                             CBA(I) = 'ESYSP'
122300110704     C                   EVAL      STPTRK = 'S'
122400110704     C                   ENDIF
122500110704     C                   ENDIF
122600110704     C*
122700110624     C                   UPDATE    WFSTP
122800110624     C     STPKSC        READE     WFSTP01L
122900110624     C                   ENDDO
123000110624     C                   ENDIF
123100110624     C*
123200110624     C                   MOVEL     '?'           wTipVAS           1
123300110624     C                   MOVEL     VIRKSC        savVIRKSC
123400110624     C                   ENDIF
123500110624     C*
123600110624     C                   IF        wTipVAS <> 'P'
123700110624     C*
123800110624     C* Solo tipi file VB e VT
123900110624     C                   IF        VIRTIP = 'VB' OR VIRTIP='VT'
124000110624     C*
124100110624     C* Solo se anagrafica correntemente valida
124200110624     C                   IF        DATCOR >= VIRDTI AND
124300110624     C                             DATCOR <= VIRDTF
124400110624     C*
124500110624     C* Verifico se trattasi d personalizzato
124600110624     C                   IF        VIRPGT <> *blanks AND
124700110624     C                             (%subst(VIRPGT:1:5) <> 'TIS7T' OR
124800110624     C                              %subst(VIRPGT:1:6)  = 'TIS7TL')
124900110624     C                   EVAL      wTipVAS = 'P'
125000110624     C                   ELSE
125100110624     C                   EVAL      wTipVAS = 'S'
125200110624     C                   ENDIF
125300110624     C*
125400110624     C                   ENDIF
125500110624     C                   ENDIF
125600110624     C                   ENDIF
125700110624     C*
125800110624     C                   READ      TIVIR01L
125900110624     C                   ENDDO
126000110624     C*
126100110624     C                   ENDSR
126200000316     C*------------------------------------------------------------------------*
126300000316     C* CANWFS - CANCELLAZIONE PERIODO RICHIESTO
126400000316     C*------------------------------------------------------------------------*
126500000316     C     CANWFS        BEGSR
126600000316     C*
126700000316     C* PER PRIMA COSA GESTISCO L'APERTURA MANUALE
126800000316     C                   OPEN      WFSTP03L
126900000316     C*
127000000316     C     PARDTI        SETLL     WFSTP03L                           44
127100000316     C  N44              READ      WFSTP03L                               44
127200000316     C     *IN44         DOWEQ     *OFF
127300000316     C     STPDAT        IFGE      PARDTI
127400000710     C     STPDAT        ANDLE     PARDTI
127500000316     C                   DELETE    WFSTP3
127600000710     C                   ENDIF
127700000316     C                   READ      WFSTP03L                               44
127800000316     C                   ENDDO
127900000316     C*
128000000316     C* ORA INVECE LO CHIUDO
128100000316     C                   CLOSE     WFSTP03L
128200000316     C*
128300000316     C                   ENDSR
128301110817     C*------------------------------------------------------------------------*
128302110817     c* Scrittura file di work WFSTV00F
128303110817     C*------------------------------------------------------------------------*
128304110817     C     sr_wrtstv     BEGSR
128305110817     c                   clear                   wfstv000
128307110817     c                   z-add     datcor        STVDEL
128308110817     c                   eval      STVUSR=knmus
128309110817     c                   z-add     pardti        STVDAL
128310110817     c                   z-add     pardtf        sTVAL
128311110817     c                   z-add     depksc        STVKSC
128312110901     c* reperisco il commerciale del cliente
128321110901     c                   eval      stvage=clpage
128323110901
128324110817     c                   eval      STVRAG=descli
128325130321     c                   if        acoabl<>*blank
128326110817     c                   eval      STVABL='*'
128327110817     c                   endif
128328110817     c                   z-add     staspt        STVSPT
128329110817     c  n14              z-add     stacot        STVCOT
128330110817     c  n14              z-add     stagio        STVGIO
128331110817     c  n14              z-add     staspg        STVSPG
128332110817     c  n14              z-add     stacog        sTVCOG
128333110817     c                   z-add     staspa        stvspa
128334110817     c                   z-add     staspb        stvspb
128335110817     c                   z-add     staspk        stvspk
128336110817     c  n14              eval      STVDCL=stadet
128337110817     c  n14              eval      STVVCP=stacap
128338110817     c  n14              eval      STVVSP=stavab
128339110817     c  n14              eval      STVPES=stapes
128340110817     c  n14              eval      STVCBA=stacba
128341110817     c  n14              z-add     staspc        stvspc
128342110817     c  n14              eval      STVTSD=§4CTSD
128343110817     c  n14              eval      STVGSD=§3KGSD
128344110817     c  n14              eval      STVGSR=§3KGSR
128345110817     c  n14              eval      STVFSD=§4CFSD
128346110817     c  n14              eval      STVCSD=§3KCSD
128347110817     c  n14              eval      STVDSM=WINDSD
128348110817     c  n14              eval      STVBAC=stabac
128349110817     c                   if        §3rnot<>*blanks
128350110817     c                   eval      STVCOM=§3rnot
128351110817     c                   endif
128352110817     c                   z-add     worfil        STVFIP
128353110817     c                   z-add     destrp        STVAS4
128354110817     c                   write     wfstv000
128355110817     C                   ENDSR
128400000000     C*------------------------------------------------------------------------*
128500010626     C* *§NZSR - IMPOSTAZIONI INIZIALI
128600000000     C*------------------------------------------------------------------------*
128700000000     C     *INZSR        BEGSR
128800000000     C*--------------------
128900000000     C* RICEVIMENTO PARAMETRI
129000000000     C*--------------------
129100000000     C     *ENTRY        PLIST
129200000000     C                   PARM                    KPJBA
129300000000     C                   MOVEL     KPJBU         PARAM
129400970131     C     PARPAR        COMP      'V'                                    70    SOLO VAS
129500970131     C     PARPAR        COMP      'N'                                    71    SOLO NO VAS
129600980606     C     PARPAR        COMP      'B'                                    72    SOLO VAS B
129700000000     C*--------------------
129800000000     C* VARIABILI RIFERITE AL DATABASE
129900000000     C*--------------------
130000000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
130100000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE                         *TABEL00F
130200000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
130300970130     C     *LIKE         DEFINE    TBLKEY        KTBKEY
130400110401     C**** *LIKE         DEFINE    VSSISV        KVSISV
130500000000     C*--------------------
130600000000     C* CHIAVI DI LETTURA
130700000000     C*--------------------
130800000000     C* LETTURA AZORG01L - COMPLETA
130900000000     C     KEYORG        KLIST
131000000000     C                   KFLD                    KORFIL
131100000322     C* LETTURA TIVSS04L - PARZIALE
131200110401     C*    KEYVSS        KLIST
131300110401     C*                  KFLD                    KVSISV
131400000000     C* LETTURA TABEL00F - PARZIALE
131500000000     C     KEYTAB        KLIST
131600000000     C                   KFLD                    KTBUTE
131700000000     C                   KFLD                    KTBCOD
131800970130     C* LETTURA TABEL00F - TOTALE
131900970130     C     KEYTAC        KLIST
132000970130     C                   KFLD                    KTBUTE
132100970130     C                   KFLD                    KTBCOD
132200970130     C                   KFLD                    KTBKEY
132300990810     C* LETTURA TITAS28C - PARZIALE
132400000000     C     KEYTAS        KLIST
132500000000     C                   KFLD                    TASAAS
132600000000     C                   KFLD                    TASMGS
132700000000     C* LETTURA WFSTP01L - COMPLETA
132800000000     C     KEYSTP        KLIST
132900000000     C                   KFLD                    STPKSC
133000000000     C                   KFLD                    STPDAT
133100000000     C* LETTURA WFSTP01L - COMPLETA
133200000000     C     KEYST2        KLIST
133300000000     C                   KFLD                    STPAS4
133400970130     C                   KFLD                    STPFIP
133500000000     C                   KFLD                    STPDCL
133600000000     C                   KFLD                    STPKSC
133700000000     C                   KFLD                    STPDAT
133800000321     C*
133900010627     C* CONTROLLO SE SELEZIONATO CLIENTI "B" CON COMODATO
134000010627     C                   IF        PARCOM = 'S'
134100010627     C                   EVAL      *IN14 = *ON
134200010627     C                   ENDIF
134300010627     c*
134400000321     C* PULISCE SE NON IMPOSTATO A SOLO STAMPA E APRE IL FILE DI LAVORO
134500000321     C     PARSTE        IFEQ      ' '
134600000321     C                   Z-ADD     55            LUNG             15 5
134700000321     C                   MOVEL     *BLANKS       COMMAN
134800000321     C                   MOVEA     CMD1(1)       COMMAN           80
134900000322     C                   CALL      'QCMDEXC'
135000000322     C                   PARM                    COMMAN
135100000322     C                   PARM                    LUNG
135200000321     C                   ENDIF
135300000000     C*--------------------
135400000000     C* VARIABILI DI CONTROLLO
135500000000     C*--------------------
135600000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
135700000000     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
135800000000     C                   Z-ADD     *ZEROS        CONLET            9 0          *N° RECORD LETTI
135900000000     C*
136000980116     C* PULISCE SE NON IMPOSTATO A SOLO STAMPA E APRE IL FILE DI LAVORO
136100970130     C     PARSTE        IFEQ      ' '
136200000316     C*
136300000316     C* ESEGUE ROUTINE DI CANCELLAZIONE DEL PERIODO RICHIESTO
136400000322     C                   EXSR      CANWFS
136500000316     C                   ENDIF
136600000316     C*
136700000000     C                   OPEN      WFSTP01L
136800000000     C                   OPEN      WFSTP02L
136801110817     C*
136802110817     c* Pulizia del file di work contenente i dati che vengono stampati
136803110817     C                   Z-ADD     55            LUNG             15 5
136804110817     C                   MOVEL     *BLANKS       COMMAN
136805110817     C                   MOVEA     CMD1(2)       COMMAN           80
136806110817     C                   CALL      'QCMDEXC'
136807110817     C                   PARM                    COMMAN
136808110817     C                   PARM                    LUNG
136809110817
136810110817     C                   OPEN      WFSTV00F
136900000000     C*
137000000000     C* INVERTE LE DATE DI LANCIO IN G/M/AA PER LA STAMPA
137100000000     C                   Z-ADD     PARDTI        DSDAT6
137200000000     C                   Z-ADD     DSANN         DTANN
137300000000     C                   Z-ADD     DSMES         DTMES
137400000000     C                   Z-ADD     DSGIO         DTGIO
137500000000     C                   Z-ADD     DTDAT6        STADTI            6 0
137600000000     C                   Z-ADD     PARDTF        DSDAT6
137700000000     C                   Z-ADD     DSANN         DTANN
137800000000     C                   Z-ADD     DSMES         DTMES
137900000000     C                   Z-ADD     DSGIO         DTGIO
138000000000     C                   Z-ADD     DTDAT6        STADTF            6 0
138100000000     C*
138200000000     C* IMPOSTA DATA INIZIO E FINE DA PARAMETRI
138300000000     C                   Z-ADD     PARDTI        DSDAT6
138400000000     C                   Z-ADD     19            DSSEC
138500000000     C                   Z-ADD     DSAAS         INIAAS            4 0
138600000000     C                   Z-ADD     DSMGS         INIMGS            4 0
138700000000     C                   Z-ADD     PARDTF        DSDAT6
138800000000     C                   Z-ADD     19            DSSEC
138900000000     C                   Z-ADD     DSAAS         FINAAS            4 0
139000000000     C                   Z-ADD     DSMGS         FINMGS            4 0
139100971128     C*
139200971128     C* CALCOLA LA DATA CORRENTE
139300971128     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
139400971128     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
139500971128     C                   Z-ADD     WN8           DATA              8 0
139600971128     C                   Z-ADD     WN8           G08DAT
139700971128     C                   Z-ADD     *ZEROS        G08INV
139800971128     C                   MOVEL     '0'           G08ERR
139900971128     C                   CALL      'XSRDA8'
140000971128     C                   PARM                    WLBDA8
140100971128     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
140200000000     C*
140300000000     C* CARICA LE TABELLE
140400000000     C                   EXSR      CARTAB
140500000000     C*
140600000000     C                   ENDSR
140700000000     O*------------------------------------------------------------------------*
140800000000     O* RIGHE DI STAMPA
140900000000     O*------------------------------------------------------------------------*
141000970131     OPRTF198   E            TESTA          1 02
141100980131     O                       KNSIF               10
141200970919     O                                           32 'CLIENTI CON UNA MEDIA'
141300970919     O                                           52 'A RITIRO DI PIU'' DI'
141400970919     O                                           64 'SPEDIZIONI:'
141500970919     O                       PARNBO        Z     67
141600970919     O                                           76 'O COLLI:'
141700970919     O                       PARNCO        Z     79
141800970919     O                                           99 'O CON "VAS  BOLLE"'
141900970919     O                                          110 'DAL:'
142000970919     O                       STADTI             119 '  /  /  '
142100970919     O                                          130 'AL:'
142200970919     O                       STADTF             139 '  /  /  '
142300000000     O                                          150 'Data:'
142400971128     O                       DATA               161 '  /  /    '
142500980131     O                                          180 'TNSS21R '
142600000000     O                                          190 'Pagina:'
142700000000     O                       PAGE          Z    198
142800970131     O          E            TESTA          1
142900970131     O               70                          35 'STAMPA SOLO I CLIENTI'
143000970131     O               70                          57 'CON VAS "BOLLE"      '
143100970131     O               71                          35 'STAMPA SOLO I CLIENTI'
143200970131     O               71                          57 'SENZA VAS "BOLLE"    '
143300980606     O               72                          35 'STAMPA SOLO I CLIENTI'
143400980606     O               72                          57 'CON VAS "BOLLE" B/B2 '
143500970131     O          E            TESTA          1
143600970131     O                       TES(1)              66
143700970130     O                       TES(1)             132
143800970130     O                       TES(1)             198
143900970130     O          E            TESTA          1
144000000000     O                       TES(2)              66
144100000000     O                       TES(3)             132
144200000000     O                       TES(4)             198
144300970130     O          E            TESTA          1
144400970130     O                       TES(5)              66
144500970130     O                       TES(6)             132
144600970130     O                       TES(7)             198
144700000000     O          E            TESTA          1
144800000000     O                       TES(1)              66
144900000000     O                       TES(1)             132
145000000000     O                       TES(1)             198
145100000000     O*------------------------------------
145200000000     O          E            SPAZIO         1
145300000000     O                                            3 ' '
145400000000     O*------------------------------------
145500000710     O          E            FIP            1
145600110325     O****                                       23 'TERMINAL/P.O. PARTENZA:'
145700110401     O                                           22 'AREA/FILIALE PARTENZA:'
145800970205     O                       DESTRP            +  1
145900970205     O                                         +  1 '/'
146000990225     O                       WORFIL            +  1
146100970205     O                       DESFIP            +  1
146200000000     O*------------------------------------
146300000000     O          E            RIGA           1
146400000000     O                       DEPKSC               7
146500000000     O                       DESCLI            +  1
146600970129     O                       STASPT        Z   +  1
146700970129     O                       STACOT        Z   +  1
146800970129     O                       STAGIO        Z   +  1
146900970129     O                       STASPG        Z   +  1
147000970129     O                       STACOG        Z   +  1
147100970129     O                       STASPA        Z   +  1
147200970129     O                       STASPB        Z   +  1
147300010423     O                       STASPK        Z   +  1
147400110817     O****                   STASPH        Z   +  1
147500970130     O                       STADET            +  2
147600970130     O                       STACAP            +  2
147700971021     O                       STAVAB            +  2
147800990225     O                       STAPES            +  2
147900970129     O                       STACBA            +  1
148000970130     O                       STASPC        Z   +  1
148100970130     O                       §4CTSD            +  2
148200971021     O                       §3KGSD            +  3
148300971021     O                       §3KGSR            +  3
148400971021     O                       §4CFSD            +  3
148500000317     O                       §3KCSD            +  3
148600110401     O***                    STATRK            +  4
148700010419     O*                      §3RNOT            +  3
148800110401     O                       WINDSD            +  6
148900010920     O                       STABAC            +  4
149000010419     O*------------------------------------
149100010419     O          E            RIGA1          1
149200010423     O                       §3RNOT             198
149300000316     O*------------------------------------
149400000316     O          E            TFIL        2  1
149500110401     O                                           12 'TOTALI FIL.:'
149600010423     O                                         +  1 'TUTTE LE SPEDIZIONI   :'
149700010423     O                                         +  2 'SPEDIZ.:'
149800000316     O                       STTSPT        2   +  0
149900010423     O                                         +  2 'COLLI:'
150000000316     O                       STTCOT        2   +  0
150100010423     O                                         +  2 'CLIENTI:'
150200000316     O                       STTCLI        2   +  0
150300010423     O                                         +  2 'CLI CON DSK A:'
150400000316     O                       STTCLA        2   +  0
150500010423     O                                         +  2 'CLI CON DSK B:'
150600000316     O                       STTCLB        2   +  0
150700010423     O                                         +  2 'CLI CON DSK C CLI:'
150800010423     O                       STTCLC        2   +  0
150900010423     O                                         +  2 '% COLLI "B":'
151000000316     O                       WPETI         2   +  0
151100010423     O                                         +  2 '% SPED. DSK:'
151200000316     O                       WPDSK         2   +  0
151300000316     O          E            TFIL           1
151400000316     O                                           12 '            '
151500010423     O                                         +  1 'SOLO SPED."POTENZIALI":'
151600010423     O                                         +  2 'SPEDIZ.:'
151700000316     O                       S2TSPT        2   +  0
151800010423     O                                         +  2 'COLLI:'
151900000316     O                       S2TCOT        2   +  0
152000010423     O                                         +  2 'CLIENTI:'
152100000316     O                       S2TCLI        2   +  0
152200010423     O                                         +  2 'CLI CON DSK A:'
152300000316     O                       S2TCLA        2   +  0
152400010423     O                                         +  2 'CLI CON DSK B:'
152500000316     O                       S2TCLB        2   +  0
152600010423     O                                         +  2 'CLI CON DSK C CLI:'
152700010423     O                       S2TCLC        2   +  0
152800010423     O                                         +  2 '% COLLI "B":'
152900000316     O                       W2ETI         2   +  0
153000010423     O                                         +  2 '% SPED. DSK:'
153100000316     O                       W2DSK         2   +  0
153200000316     O          E            TFIL        1  1
153300000316     O                                           20 '(*)= CODICE BLOCCATO'
153400000316     O          E            TFIL           1
153500000316     O                                           24 'TOTALE CLIENTI CON:     '
153600110624     O                                         +  1 'TRACCIATO VAB/VAT *STD ='
153700000316     O                       WBLCF         2   +  1
153800000316     O                                         +  3 'CAPPARIO 1='
153900000316     O                       WCAPVF        2   +  1
154000000316     O                                         +  3 'CAPPARIO 2='
154100000316     O                       WCAPNF        2   +  1
154200000316     O                                         +  3 'CAPPARIO 3='
154300000316     O                       WCAPLF        2   +  1
154400110401     O***                                      +  3 'T&T='
154500110401     O***                    WCATRF        2   +  1
154600000316     O*------------------------------------
154700000316     O          E            STAFIN         2
154800000316     O                                           12 'TOTALI     :'
154900010423     O                                         +  1 'TUTTE LE SPEDIZIONI   :'
155000010423     O                                         +  2 'SPEDIZ.:'
155100000316     O                       STFSPT        2   +  0
155200010423     O                                         +  2 'COLLI:'
155300000316     O                       STFCOT        2   +  0
155400010423     O                                         +  2 'CLIENTI:'
155500000316     O                       STFCLI        2   +  0
155600010423     O                                         +  2 'CLI CON DSK A:'
155700000316     O                       STFCLA        2   +  0
155800010423     O                                         +  2 'CLI CON DSK B:'
155900000316     O                       STFCLB        2   +  0
156000010423     O                                         +  2 'CLI CON DSK C CLI:'
156100010423     O                       STFCLC        2   +  0
156200010423     O                                         +  2 '% COLLI "B":'
156300000316     O                       WPETI         2   +  0
156400010423     O                                         +  2 '% SPED. DSK:'
156500000316     O                       WPDSK         2   +  0
156600000316     O          E            STAFIN         3
156700000316     O                                           12 '            '
156800010423     O                                         +  1 'SOLO SPED."POTENZIALI":'
156900010423     O                                         +  2 'SPEDIZ.:'
157000000316     O                       S2FSPT        2   +  0
157100010423     O                                         +  2 'COLLI:'
157200000316     O                       S2FCOT        2   +  0
157300010423     O                                         +  2 'CLIENTI:'
157400000316     O                       S2FCLI        2   +  0
157500010423     O                                         +  2 'CLI CON DSK A:'
157600000316     O                       S2FCLA        2   +  0
157700010423     O                                         +  2 'CLI CON DSK B:'
157800000316     O                       S2FCLB        2   +  0
157900010423     O                                         +  2 'CLI CON DSK C CLI:'
158000010423     O                       S2FCLC        2   +  0
158100010423     O                                         +  2 '% COLLI "B":'
158200000316     O                       W2ETI         2   +  0
158300010423     O                                         +  2 '% SPED. DSK:'
158400000316     O                       W2DSK         2   +  0
158500000316     O          E            STAFIN      2  2
158600000316     O                                           24 'TOTALE CLIENTI CON:     '
158700110624     O                                         +  1 'TRACCIATO VAB/VAT *STD ='
158800000316     O                       WBLCT         2   +  1
158900000316     O                                         +  3 'CAPPARIO 1='
159000000316     O                       WCAPVT        2   +  1
159100000316     O                                         +  3 'CAPPARIO 2='
159200000316     O                       WCAPNT        2   +  1
159300000316     O                                         +  3 'CAPPARIO 3='
159400000316     O                       WCAPLT        2   +  1
159500110401     O****                                     +  3 'T&T='
159600110401     O****                   WCATRT        2   +  1
159700000316     O          E            STAFIN         1
159800000316     O                                           24 'TOTALI CLIENTI PER TIPO '
159900000316     O                                         +  1 'SUPPORTO VAS BOLLE'
160000000316     O          E            STAFIN         1
160100000316     O                                           11 'TIPO NUMERO'
160200000316     O          E            STAFI2         1
160300000316     O                       TIB(I)               5
160400000316     O                       NIB(I)        2   +  1
160500010626     O*------------------------------------------------------------------------*
160600010626     O* RIGHE DI STAMPA QSYSPRT
160700010626     O*------------------------------------------------------------------------*
160800010626     OQSYSPRT   E            TESTAT         1 02
160900010626     O                       KNSIF               10
161000010626     O                                           32 'CLIENTI CON UNA MEDIA'
161100010626     O                                           52 'A RITIRO DI PIU'' DI'
161200010626     O                                           64 'SPEDIZIONI:'
161300010626     O                       PARNBO        Z     67
161400010626     O                                           76 'O COLLI:'
161500010626     O                       PARNCO        Z     79
161600010626     O                                           99 'O CON "VAS  BOLLE"'
161700010626     O                                          105 'DAL:'
161800010626     O                       STADTI             114 '  /  /  '
161900010626     O                                          120 'AL:'
162000010626     O                       STADTF             129 '  /  /  '
162100010626     O          E            TESTAT         1
162200010626     O               70                          35 'STAMPA SOLO I CLIENTI'
162300010626     O               70                          57 'CON VAS "BOLLE"      '
162400010626     O               71                          35 'STAMPA SOLO I CLIENTI'
162500010626     O               71                          57 'SENZA VAS "BOLLE"    '
162600010626     O               72                          35 'STAMPA SOLO I CLIENTI'
162700010626     O               72                          57 'CON VAS "BOLLE" B/B2 '
162800010626     O               14                          80 'E MATERIALE IN COMODATO'
162900010626     O                                          100 'Data:'
163000010626     O                       DATA               111 '  /  /    '
163100010626     O                                          121 'TNSS21R'
163200010626     O                                          128 'Pag.'
163300010626     O                       PAGE          Z    132
163400010626     O          E            TESTAT         1
163500010626     O                       TE1(1)              66
163600010626     O                       TE1(1)             132
163700010626     O          E            TESTAT         1
163800010626     O                       TE1(2)              66
163900010626     O                       TE1(3)             132
164000010626     O          E            TESTAT         1
164100010626     O                       TE1(4)              66
164200010626     O                       TE1(5)             132
164300010626     O          E            TESTAT         1
164400010626     O                       TE1(1)              66
164500010626     O                       TE1(1)             132
164600010626     O*------------------------------------
164700010626     O          E            SPAZIO1        1
164800010626     O                                            3 ' '
164900010626     O*------------------------------------
165000010626     O          E            FIP1           1
165100110325     O***                                        23 'TERMINAL/P.O. PARTENZA:'
165200110401     O                                           22 'AREA/FILIALE PARTENZA:'
165300010626     O                       DESTRP            +  1
165400010626     O                                         +  1 '/'
165500010626     O                       WORFIL            +  1
165600010626     O                       DESFIP            +  1
165700010626     O*------------------------------------
165800010626     O          E            RIGA2          1
165900070626     O                       DEPKSC
166000070626     O                       DESCLI             + 1
166100070626     O                       STASPT        Z    + 3
166200070626     O                       STASPA        Z    + 3
166300070626     O                       STASPB        Z    + 1
166400070626     O                       STASPK        Z    + 1
166500110817     O***                    STASPH        Z    + 1
166600070626     O                       §3RNOT             + 3
166700070626      *
166800000321** CMD1 - COMANDI CLP
166900000321CLRPFM FILE(*LIBL/WFSTP00F)
166901110817CLRPFM FILE(*LIBL/WFSTV00F)
167000970130** TES  - RIGHE DI TESTATA
167100000316------------------------------------------------------------------  1
167200000316                                        TOTALE    TOTALE  GG   SPE  2
167300110817DIZ.    COLLI ---- SPEDIZIONI DISK ----- DT VR TR PE SUPP.   SPEDI  3
167400110817Z ---- S U P P O R T O ---     MM  SUPP.CAPPARIO                    4
167500000316CLIENTE                                 SPEDIZ.    COLLI  RIT X GI  52
167600110817ORNO X GIORNO      "A"       "B" "C" CLI CL CP SP SO BOLLE      C/  63
167700110817A C/A  GIA  DISP FATT POD      DK MATERIALE IN COMODATO             74
167800970130                                                                    8
167900010626** TE1  - RIGHE DI TESTATA QSYSPRT
168000010626------------------------------------------------------------------  1
168100070626                                   ABL    TOTALE    ---- SPEDIZION  2
168200110817I  DISK ----                                                        3
168300070627CLIENTE                            (*)    SPEDIZ.        "A"        4
168400110817"B"   "C"CLI   MATERIALE IN COMODATO                                5
