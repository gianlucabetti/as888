000100991116     H*------------------------------------------------------------------------*
000200991116     H* SIMULAZIONE CONTO ECONOMICO - ESEGUE                                   *
000300991116     H*------------------------------------------------------------------------*
000400991116     H DECEDIT('0,') DATEDIT(*DMY.)
000500991116     F*------------------------------------------------------------------------*
000600991116     F* DATA BASE
000700991116     F*------------------------------------------------------------------------*
000800991116     F*-------------------
000900991116     F* BOLLE -> AAS + LNP
001000991116     F*-------------------
001100991116     FTITAS30C  IF   E           K DISK    USROPN
001200030129     F*---                                 IGNORE(TITAS000)
001300991116     F                                     IGNORE(TITASP00)
001400991116     F                                     RENAME(TITAS010:TAS130)
001500030129     F                                     RENAME(TITAS000:TAS030)
001600991116     F*-------------------
001700991116     F* BOLLE -> KSC
001800991116     F*-------------------
001900991116     FTITAS31C  IF   E           K DISK    USROPN
002000030129     F*---                                 IGNORE(TITAS000)
002100991116     F                                     IGNORE(TITASP00)
002200991116     F                                     RENAME(TITAS010:TAS131)
002300030129     F                                     RENAME(TITAS000:TAS031)
002400991116     F*-------------------
002500991116     F* BOLLE -> AAS + MGS
002600991116     F*-------------------
002700991116     FTITAS38C  IF   E           K DISK    USROPN
002800030129     F*---                                 IGNORE(TITAS000)
002900991116     F                                     IGNORE(TITASP00)
003000991116     F                                     RENAME(TITAS010:TAS138)
003100030129     F                                     RENAME(TITAS000:TAS038)
003200991116     FAZORG01L  IF   E           K DISK
003300991116     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
003400991116     D*------------------------------------------------------------------------*
003500991116     D* SCHIERE
003600991116     D*------------------------------------------------------------------------*
003700991116     D*------------------
003800991116     D* RIGHE IN STAMPA
003900991116     D*------------------
004000991116     D STA             S             66    DIM(10) CTDATA PERRCD(1)
004100991116     D*------------------
004200991116     D* ORGANIGRAMMA
004300991116     D*------------------
004400991116     D ORG             S              3  0 DIM(500)
004500991116     D ODES            S             20    DIM(500)
004600991116     D ODIT            S              3    DIM(500)
004700991116     D*---
004800991116     D* SCHIERE CODICI/IMPORTI
004900991116     D*---
005000991116     D CAN             S              2    DIM(50)
005100020103     D IAN             S              7    DIM(50)
005200991116     D*------------------------------------------------------------------------*
005300991116     D* INPUT
005400991116     D*------------------------------------------------------------------------*
005500991116     D*-------------------
005600991116     D* ARCHITETTURA
005700991116     D*-------------------
005800991116     D KPJBA         E DS
005900991116     D*-------------
006000991116     D* DS REPERIMENTO DATI UTENTE
006100991116     D*-------------
006200020923     D TIBS34DS      E DS
006300020923     D DDATIUTE      E DS
006400020923     D AZUTEDS       E DS                  extname(AZUTE00F)
006500991116     D*-------------
006600991116     D* DS DATI SIMULAZIONE
006700991116     D*-------------
006800991116     D DSD$G         E DS
006900991116     D*-------------------
007000991116     D* DS DATA SPEDIZIONE
007100991116     D*-------------------
007200991116     D                 DS
007300991116     D  TASAAS                 1      4  0
007400991116     D  TASMGS                 5      8  0
007500991116     D  TASDSP                 1      8  0
007600991116     D*-------------------
007700991116     D* CONTROLLO DATA (8)
007800991116     D*-------------------
007900991116     D WLBDA8          DS                  INZ
008000991116     D  G08DAT                 1      8  0
008100991116     D  G08INV                 9     16  0
008200991116     D  G08ERR                17     17
008300991116     D  G08TGI                18     22  0
008400991116     D*-------------------
008500991116     D* PARAMETRI IN ENTRATA
008600991116     D*-------------------
008700991116     D PARAM           DS
008800991116     D  PARORD                 1      1
008900991116     D  PARFIL                 2      4  0
009000991116     D  PARKS1                 5     11  0
009100991116     D  PARDS1                12     19  0
009200991116     D  PARAA1                12     15  0
009300991116     D  PARMG1                16     19  0
009400991116     D  PARDS2                20     27  0
009500991116     D  PARAA2                20     23  0
009600991116     D  PARMG2                24     27  0
009700991116     D  PARDF1                28     35  0
009800991116     D  PARDF2                36     43  0
009900991116     D  PARKS2                44     50  0
010000991116     D  PARASI                51     54  0
010100010925     D  PARCLR                55     55
010200991116     D*---
010300991116     D* DS PER PASSAGGIO CAMPI PACKED -CODICE/IMPORTO-
010400991116     D*---
010500991116     D                 DS
010600991116     D  DSWCN                  1      3P 0
010700991116     D  DSWCA                  1      3
010800020103     D  DSWIN                  4     13P 4
010900020103     D  DSWIA                  4     13
011000991116     D*---
011100991116     D* DS PER LANCIO PROGRAMMI
011200991116     D*---
011300991116     D DS$ECO        E DS
011400991116     D DS£ECO        E DS
011500991116     D*---
011600991116     D* DS PER DEFINIZIONE CAMPONE
011700991116     D*---
011800991116     D TRECDS        E DS
011900991116     D  D$PDS1                 1    250
012000991116     D  D$PDS2               251    500
012100991116     D  D$DDS1               501    750
012200991116     D  D$DDS2               751   1000
012300991116     D  D$ODS1              1001   1250
012400991116     D  D$ODS2              1251   1500
012500991116     D*---
012600991116     D* DS INFORMAZIONI JOB
012700991116     D*---
012800991116     D                SDS
012900991116     D  JOB                  244    253
013000991116     D  USER                 254    263
013100991116     D*---
013200991116     D* COSTANTI
013300991116     D*---
013400991116     D CPGF            C                   CONST('TREC11R  ')
013500030129     D CFIL1           C                   CONST('TITAS10F ')
013600030129     D CMBR1           C                   CONST('TITAS10F ')
013700991116     D CLIB            C                   CONST('GAITRAGRU')
013800030129     D CFIL0           C                   CONST('TITAS00F ')
013900030129     D CMBR0           C                   CONST('TITAS00F ')
014000991116     C*------------------------------------------------------------------------*
014100991116     C* MAIN LINE
014200991116     C*------------------------------------------------------------------------*
014300991116     C*
014400991116     C* APERTURA FILES
014500991116     C                   EXSR      APRE
014600991123     C*
014700991123     C* PULISCE VARIABILI DI LAVORO
014800991123     C                   Z-ADD     *ZEROS        D$GPRE
014900991116     C*
015000991116     C* POSIZIONAMENTO E PRIMA LETTURA
015100991116     C                   EXSR      SETFIL
015200991116     C*
015300991116     C* LETTURA FINO A FINE FILE
015400991116DO  1C     $FINE         DOWEQ     '0'
015500991116     C*
015600991116     C* LANCIA IL PROGRAMMA DI CALCOLO
015700991116     C                   EXSR      LANCEC
015800991116     C*
015900991116     C* LETTURA RECORD SUCESSIVO
016000991116     C                   EXSR      LETFIL
016100991116E   1C                   ENDDO
016200991116     C*
016300991116     C* STAMPA TABULATO DI CONTROLLO
016400991116     C                   EXSR      STACTL
016500991116     C*
016600991116     C* CHIUSURA FILES
016700991116     C                   EXSR      CHIUDE
016800991116     C*
016900991116     C* RILANCIA PROGRAMMA CON 'C' PER CHIUDERE I FILE E SCRIVERE WFMOV
017000991116     C                   MOVEL     'C'           D$GSTS
017100991116     C                   CALL      'TRECD6R'
017200991116     C                   PARM                    DS$ECO
017300991116     C                   PARM                    DS£ECO
017400991116     C*
017500991116     C                   SETON                                        LR
017600991116     C*------------------------------------------------------------------------*
017700991116     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
017800991116     C*------------------------------------------------------------------------*
017900991116     C     SETFIL        BEGSR
018000991116     C*
018100991116     C* IMPOSTA LE CHIAVI DI LETTURA
018200991116     C                   Z-ADD     PARAA1        KTAAAS
018300991116     C                   Z-ADD     PARFIL        KTALNP
018400991116     C                   Z-ADD     PARKS1        KTAKSC
018500991116     C                   Z-ADD     PARMG1        KTAMGS
018600991116     C* POSIZIONAMENTO
018700991116SL  1C                   SELECT
018800991116     C     PARORD        WHENEQ    '1'
018900991116     C     KEYT30        SETLL     TITAS30C                           99
019000991116     C     PARORD        WHENEQ    '2'
019100991116     C     KEYT31        SETLL     TITAS31C                           99
019200991116     C     PARORD        WHENEQ    '3'
019300991116     C     KEYT38        SETLL     TITAS38C                           99
019400991116E   1C                   ENDSL
019500991116     C*
019600991116IF  1C     *IN99         IFEQ      *ON
019700991116     C                   MOVEL     '1'           $FINE
019800991116X   1C                   ELSE
019900991116     C                   EXSR      LETFIL
020000991116E   1C                   END
020100991116     C*
020200991116     C                   ENDSR
020300991116     C*------------------------------------------------------------------------*
020400991116     C* LETFIL - LETTURA RECORD
020500991116     C*------------------------------------------------------------------------*
020600991116     C     LETFIL        BEGSR
020700991116     C*
020800991116     C* LEGGE FINO A:
020900991116     C*     - FINE FILE
021000991116     C*     - TROVATO RECORD VALIDO
021100991116     C                   MOVEL     'N'           WRECOK
021200991116DO  1C     $FINE         DOWEQ     '0'
021300991116     C     WRECOK        ANDEQ     'N'
021400991116SL  2C                   SELECT
021500991116     C     PARORD        WHENEQ    '1'
021600991116     C                   READ      TITAS30C                               99
021700991116     C     PARORD        WHENEQ    '2'
021800991116     C                   READ      TITAS31C                               99
021900991116     C     PARORD        WHENEQ    '3'
022000991116     C                   READ      TITAS38C                               99
022100991116E   2C                   ENDSL
022200991116     C*
022300991116IF  2C     *IN99         IFEQ      *ON
022400991116     C                   MOVEL     '1'           $FINE
022500991116X   2C                   ELSE
022600991116     C                   EXSR      CHKREC
022700991116E   2C                   ENDIF
022800991116E   1C                   ENDDO
022900991116     C*
023000991116     C                   ENDSR
023100991116     C*------------------------------------------------------------------------*
023200991116     C* CHKREC - CONTROLLA VALIDITA' RECORD
023300991116     C*------------------------------------------------------------------------*
023400991116     C     CHKREC        BEGSR
023500991116     C*
023600991116     C                   MOVEL     'S'           WRECOK            1
023700991116     C*
023800991116     C* FILIALE NON SELEZIONATA, ESCLUDE
023900991116DO  1C                   DO
024000991116IF  2C     PARFIL        IFGT      *ZEROS
024100991116     C     PARFIL        ANDNE     TASLNP
024200991116IF  3C     PARORD        IFEQ      '1'
024300991116     C                   MOVEL     '1'           $FINE
024400991116E   3C                   ENDIF
024500991116     C                   MOVEL     'N'           WRECOK
024600991116     C                   LEAVE
024700991116E   2C                   ENDIF
024800991116     C*
024900991116     C* CLIENTE NON SELEZIONATO, ESCLUDE
025000991116IF  2C     PARKS1        IFGT      *ZEROS
025100991116IF  3C     TASKSC        IFLT      PARKS1
025200991116     C     TASKSC        ORGT      PARKS2
025300991116IF  4C     PARORD        IFEQ      '2'
025400991116     C     TASKSC        ANDGT     PARKS2
025500991116     C                   MOVEL     '1'           $FINE
025600991116E   4C                   ENDIF
025700991116     C                   MOVEL     'N'           WRECOK
025800991116     C                   LEAVE
025900991116E   3C                   ENDIF
026000991116E   2C                   ENDIF
026100991116     C*
026200991116     C* DATA SPEDIZIONE NON SELEZIONATA, ESCLUDE
026300991116IF  2C     PARDS1        IFGT      *ZEROS
026400991116IF  3C     TASDSP        IFLT      PARDS1
026500991116     C     TASDSP        ORGT      PARDS2
026600991116IF  4C     PARORD        IFEQ      '3'
026700991116     C     TASDSP        ANDGT     PARDS2
026800991116     C                   MOVEL     '1'           $FINE
026900991116E   4C                   ENDIF
027000991116     C                   MOVEL     'N'           WRECOK
027100991116     C                   LEAVE
027200991116E   3C                   ENDIF
027300991116E   2C                   ENDIF
027400991116     C*
027500991116     C* DATA FATTURA NON SELEZIONATA, ESCLUDE
027600991116IF  2C     PARDF1        IFGT      *ZEROS
027700991116IF  3C     TASDFT        IFLT      PARDF1
027800991116     C     TASDFT        ORGT      PARDF2
027900991116     C                   MOVEL     'N'           WRECOK
028000991116     C                   LEAVE
028100991116E   3C                   ENDIF
028200991116E   2C                   ENDIF
028300991116     C*
028400991116E   1C                   ENDDO
028500991116     C*
028600991116     C                   ENDSR
028700991116     C*------------------------------------------------------------------------*
028800991116     C* LANCEC - LANCIA IL PROGRAMMA DI CALCOLO
028900991116     C*------------------------------------------------------------------------*
029000991116     C     LANCEC        BEGSR
029100991116     C*
029200991116     C* INCREMENTA CONTATORI RECORD LANCIATI
029300991116     C                   ADD       1             RECLAN
029400991116     C*
029500991116     C* IMPOSTA LA DS CON I CAMPI DEL LANCIO
029600991116     C                   EXSR      IMPLAN
029700991116     C*
029800991116     C* IMPOSTA LA DS CON I CAMPI DELLA BOLLA
029900991116     C                   EXSR      IMPBOL
030000991116     C*
030100991116     C* LANCIA IL PROGRAMMA
030200991116     C                   CALL      'TRECD6R'
030300991116     C                   PARM                    DS$ECO
030400991116     C                   PARM                    DS£ECO
030500991116     C*
030600991116     C                   ENDSR
030700991116     C*------------------------------------------------------------------------*
030800991116     C* IMPLAN - IMPOSTA LA DS CON I CAMPI DEL TIPO LANCIO
030900991116     C*------------------------------------------------------------------------*
031000991116     C     IMPLAN        BEGSR
031100991116     C* STATO
031200991116     C                   MOVEL     'I'           D$GSTS
031300991116     C* DATA + ORA
031400991116     C                   TIME                    N14              14 0
031500991116     C                   MOVEL     N14           ORACOR            6 0
031600991116     C                   MOVE      N14           N8                8 0
031700991116     C                   Z-ADD     N8            G08DAT
031800991116     C                   Z-ADD     *ZEROS        G08INV
031900991116     C                   MOVEL     '0'           G08ERR
032000991116     C                   CALL      'XSRDA8'
032100991116     C                   PARM                    WLBDA8
032200991116     C                   Z-ADD     G08INV        DATCOR            6 0
032300991116     C                   Z-ADD     DATCOR        D$GDAT
032400991116     C                   Z-ADD     DATCOR        D$GTIM
032500991116     C* UTENTE
032600991116     C                   MOVEL     USER          D$GUSR
032700991116     C* PROGRAMMA
032800991116     C                   MOVEL     CPGF          D$GPGF
032900991116     C* FILE
033000030129     C                   MOVEL     CFIL1         D$GFIL
033100991116     C* MEMBRO
033200030129     C                   MOVEL     CMBR1         D$GMBR
033300991116     C* LIBRERIA
033400991116     C                   MOVEL     CLIB          D$GLIB
033500991116     C* FLAGs
033600011227     C                   MOVEL     'S'           D$GSDA
033700991116     C                   MOVEL     'S'           D$GSDC
033800011227     C                   MOVEL     'S'           D$GSDF
033900011227     C                   MOVEL     'S'           D$GSDO
034000011227     C                   MOVEL     'S'           D$GSDP
034100011227     C                   MOVEL     'S'           D$GSDR
034200011227     C                   MOVEL     'S'           D$GSDT
034300011227     C                   MOVEL     'S'           D$GCET
034400991116     C                   MOVEL     'S'           D$GWFM
034500020531     C                   MOVEL     'S'           D$GFIM
034600020531     C                   MOVEL     'S'           D$GFCE
034700030128     C                   MOVEL     'S'           D$GCOP
034800991116     C* ERRORI
034900991116     C                   MOVEL     *BLANKS       D$GERR
035000991116     C                   MOVEL     *BLANKS       D$GMSG
035100991116     C                   MOVEL     *BLANKS       D$GPDE
035200991116     C* STATUS ORIGINALE
035300991116     C                   MOVEL     D$GSTS        D$GSTO
035400991116     C* DATI SIMUALZIONE
035500991116     C                   MOVEL     PARASI        D$GASI
035600991116     C                   MOVEL     'S'           D$GFSI
035700991116     C                   MOVEL     DSD$G         D$GXXX
035800991116     C*
035900991116     C* AZZERA SCHIERE DI RITORNO
036000991116     C                   EXSR      AZZSCH
036100991116     C*
036200991116     C* IMPOSTA SCHIERE DI RITORNO
036300991116     C                   MOVEA     CAN           D$DCCD
036400991116     C                   MOVEA     IAN           D$DICD
036500991116     C                   MOVEA     CAN           D$DCVD
036600991116     C                   MOVEA     CAN           D$DPVD
036700991116     C                   MOVEA     IAN           D$DIVD
036800991116     C*
036900991116     C                   ENDSR
037000991116     C*------------------------------------------------------------------------*
037100991116     C* IMPBOL - IMPOSTAZIONE LA DS CON I CAMPI DELLA BOLLA
037200991116     C*------------------------------------------------------------------------*
037300991116     C     IMPBOL        BEGSR
037400991116     C*
037500991116     C* IMPOSTA LA DS -ORIGINALE-
037600991116     C                   EXSR      AODTAQ
037700991116     C*
037800991116     C* AZZERA LA DS -PRIMA-
037900991116     C                   EXSR      APDTAQ
038000991116     C*
038100991116     C* IMPOSTA LA DS -DOPO-
038200991116     C                   EXSR      CDDTAQ
038300001129     C*
038400001129     C* FORZATURE DATI SIMUALZIONE -SEMPRE BOLLE BARTOLINI-
038500001129     C                   MOVEL     'B'           D$DSCL
038600001129     C                   MOVEL     'B'           D$OSCL
038700991116     C*
038800991116     C* IMPOSTA LA DS$ECO
038900991116     C                   MOVEL     D$PDS1        D$PRE1
039000991116     C                   MOVEL     D$PDS2        D$PRE2
039100991116     C                   MOVEL     D$DDS1        D$DRE1
039200991116     C                   MOVEL     D$DDS2        D$DRE2
039300991116     C*---                 MOVELD$ODS1    D$ORE1           *-ORIGINALE- 1/2
039400991116     C*---                 MOVELD$ODS2    D$ORE2           *-ORIGINALE- 2/2
039500991116     C*
039600991116     C                   ENDSR
039700991116     C*------------------------------------------------------------------------*
039800991116     C* AZZSCH - AZZERA LE SCHIERE DI RITORNO
039900991116     C*------------------------------------------------------------------------*
040000991116     C     AZZSCH        BEGSR
040100991116     C*
040200991116DO  1C     1             DO        50            I                 6 0
040300991116     C                   Z-ADD     *ZEROS        DSWCN
040400991116     C                   MOVE      DSWCA         CAN(I)
040500991116     C                   Z-ADD     *ZEROS        DSWIN
040600991116     C                   MOVE      DSWIA         IAN(I)
040700991116E   1C                   ENDDO
040800991116     C                   MOVEA     CAN           D$DCCD
040900991116     C                   MOVEA     IAN           D$DICD
041000991116     C                   MOVEA     CAN           D$DCVD
041100991116     C                   MOVEA     CAN           D$DPVD
041200991116     C                   MOVEA     IAN           D$DIVD
041300991116     C*
041400991116     C                   ENDSR
041500991116     C*------------------------------------------------------------------------*
041600991116     C* STACTL - STAMPA DI CONTROLLO
041700991116     C*------------------------------------------------------------------------*
041800991116     C     STACTL        BEGSR
041900991116     C*--------------------
042000991116     C* DECODIFICHE
042100991116     C*--------------------
042200991116IF  1C     PARFIL        IFGT      *ZEROS
042300991116     C                   SETOFF                                           28
042400991116     C                   Z-ADD     1             I
042500991116     C     PARFIL        LOOKUP    ORG(I)                                 28
042600991116     C                   MOVEL     ODES(I)       DECFIL           25
042700991116E   1C                   ENDIF
042800991116     C*
042900991116IF  1C     PARDS1        IFGT      *ZEROS
043000991116     C                   Z-ADD     PARDS1        G08INV
043100991116     C                   Z-ADD     *ZEROS        G08DAT
043200991116     C                   MOVEL     '3'           G08ERR
043300991116     C                   CALL      'XSRDA8'
043400991116     C                   PARM                    WLBDA8
043500991116     C                   Z-ADD     G08DAT        DECDS1            8 0
043600991116     C*
043700991116     C                   Z-ADD     PARDS2        G08INV
043800991116     C                   Z-ADD     *ZEROS        G08DAT
043900991116     C                   MOVEL     '3'           G08ERR
044000991116     C                   CALL      'XSRDA8'
044100991116     C                   PARM                    WLBDA8
044200991116     C                   Z-ADD     G08DAT        DECDS2            8 0
044300991116E   1C                   ENDIF
044400011205     C*
044500011205     C* IMPOSTA DATA E ORA CORRENTE
044600011210     C                   TIME                    n14              14 0
044700011210     C                   MOVEL     n14           oraf              6 0
044800011210     C                   MOVE      n14           dataf             8 0
044900991116     C*
045000991116     C* S T A M P A
045100991116     C                   EXCEPT    STM
045200991116     C*
045300991116     C                   ENDSR
045400991116     C*------------------------------------------------------------------------*
045500991116     C* APRE - APERTURA FILES
045600991116     C*------------------------------------------------------------------------*
045700991116     C     APRE          BEGSR
045800991116     C*
045900991116SL  1C                   SELECT
046000991116     C     PARORD        WHENEQ    '1'
046100991116     C                   OPEN      TITAS30C
046200991116     C     PARORD        WHENEQ    '2'
046300991116     C                   OPEN      TITAS31C
046400991116     C     PARORD        WHENEQ    '3'
046500991116     C                   OPEN      TITAS38C
046600991116E   1C                   ENDSL
046700991116     C*
046800991116     C                   ENDSR
046900991116     C*------------------------------------------------------------------------*
047000991116     C* CHIUDE - CHIUSURA FILES
047100991116     C*------------------------------------------------------------------------*
047200991116     C     CHIUDE        BEGSR
047300991116     C*
047400991116SL  1C                   SELECT
047500991116     C     PARORD        WHENEQ    '1'
047600991116     C                   CLOSE     TITAS30C
047700991116     C     PARORD        WHENEQ    '2'
047800991116     C                   CLOSE     TITAS31C
047900991116     C     PARORD        WHENEQ    '3'
048000991116     C                   CLOSE     TITAS38C
048100991116E   1C                   ENDSL
048200991116     C*
048300991116     C                   ENDSR
048400991116     C*------------------------------------------------------------------------*
048500991116     C* CARTAB - CARICAMENTO TABELLE
048600991116     C*------------------------------------------------------------------------*
048700991116     C     CARTAB        BEGSR
048800991116     C*---
048900991116     C* ORGANIGRAMMA
049000991116     C*---
049100991116     C                   Z-ADD     *ZEROS        I                 6 0
049200991116     C     *LOVAL        SETLL     AZORG01L
049300991116     C                   READ      AZORG01L                               99
049400991116DO  1C     *IN99         DOWEQ     *OFF
049500991116     C                   ADD       1             I
049600991116     C                   Z-ADD     ORGFIL        ORG(I)
049700991116     C                   MOVEL     ORGDES        ODES(I)
049800991116     C                   MOVEL     ORGDIT        ODIT(I)
049900991116     C                   READ      AZORG01L                               99
050000991116E   1C                   ENDDO
050100991116     C*
050200991116     C                   ENDSR
050300020923     C*-------------------------------------------------------------------------
050400020923     C* REPERISCE I DATI UTENTE
050500020923     C*-------------------------------------------------------------------------
050600020923     C     REPDATIUTE    BEGSR
050700020923     C*
050800020923     C* INIZIALIZZA VARIABILI DI WRK
050900020923     C                   CLEAR                   TIBS34DS
051000020923     C                   CLEAR                   AZUTEDS
051100020923     C                   CLEAR                   DDATIUTE
051200020923     C*
051300020923     C     *DTAARA       DEFINE    §azute        azuteds
051400020923     C     *DTAARA       DEFINE    §datiute      ddatiute
051500020923     C                   IN(E)     *DTAARA
051600020923if  1C                   IF        %Error
051700020923     c                   EVAL      I34Tla = 'L'
051800020923     C                   CALL      'TIBS34R'
051900020923     C                   PARM                    Tibs34Ds
052000020923     C                   IN        *DTAARA
052100020923e   1C                   ENDIF
052200020923     C*
052300020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
052400020923     C                   MOVEL(P)  RSUT          DSFIRS           20
052500020923     C*
052600020923     C                   ENDSR
052700991116     C*------------------------------------------------------------------------*
052800991116     C* *INZSR - ROUTINE INIZIALE
052900991116     C*------------------------------------------------------------------------*
053000991116     C     *INZSR        BEGSR
053100991116     C*--------------------
053200991116     C* RICEVIMENTO PARAMETRI
053300991116     C*--------------------
053400991116     C     *ENTRY        PLIST
053500991116     C                   PARM                    KPJBA
053600991116     C                   MOVEL     KPJBU         PARAM
053700991116     C*--------------------
053800991116     C* CAMPI RIFERITI AL DATA BASE
053900991116     C*--------------------
054000991116     C     *LIKE         DEFINE    TASAAS        KTAAAS
054100991116     C     *LIKE         DEFINE    TASLNP        KTALNP
054200991116     C     *LIKE         DEFINE    TASKSC        KTAKSC
054300991116     C     *LIKE         DEFINE    TASMGS        KTAMGS
054400991116     C     *LIKE         DEFINE    ORGDIT        DITLNP
054500991116     C     *LIKE         DEFINE    ORGDIT        DITLNA
054600991116     C     *LIKE         DEFINE    ORGDIT        DITTFP
054700991116     C     *LIKE         DEFINE    ORGDIT        DITTFA
054800991116     C     *LIKE         DEFINE    ORGDIT        DITFLP
054900991116     C     *LIKE         DEFINE    ORGDIT        DITFL2
055000991116     C*--------------------
055100991116     C* CHIAVI DI LETTURA
055200991116     C*--------------------
055300991116     C* LETTURA TITAS30C - PARZIALE
055400991116     C     KEYT30        KLIST
055500991116     C                   KFLD                    KTAAAS
055600991116     C                   KFLD                    KTALNP
055700991116     C* LETTURA TITAS31C - PARZIALE
055800991116     C     KEYT31        KLIST
055900991116     C                   KFLD                    KTAKSC
056000991116     C* LETTURA TITAS38C - COMPLETA
056100991116     C     KEYT38        KLIST
056200991116     C                   KFLD                    KTAAAS
056300991116     C                   KFLD                    KTAMGS
056400991116     C                   KFLD                    KTAKSC
056500991116     C* LETTURA TITAS38C - PARZIALE
056600991116     C     KE2T38        KLIST
056700991116     C                   KFLD                    KTAAAS
056800991116     C                   KFLD                    KTAMGS
056900991116     C*--------------------
057000991116     C* CAMPI DI CONTROLLO
057100991116     C*--------------------
057200991116     C                   MOVEL     '0'           $FINE             1
057300991116     C                   MOVEL     'N'           WRECOK            1
057400991116     C                   Z-ADD     *ZEROS        RECLAN           13 0
057500991116     C*
057600991116     C* IMPOSTA DATA E ORA CORRENTE
057700011210     C                   TIME                    n14              14 0
057800011210     C                   MOVEL     n14           orai              6 0
057900011210     C                   MOVE      n14           datai             8 0
058000991116     C*
058100991116     C* REPERISCE I DATI DELL'UTENTE
058200020923     C                   EXSR      REPDATIUTE
058300991116     C*
058400991116     C* CARICA TABELLE
058500991116     C                   EXSR      CARTAB
058600991116     C*
058700991116     C                   ENDSR
058800991116     C*------------------------------------------------------------------------*
058900991116      /COPY GAITRASRC/SRCTREC,TRECDT
059000991116     C*------------------------------------------------------------------------*
059100991116     O*------------------------------------------------------------------------*
059200991116     O* STAMPA DI CONTROLLO
059300991116     O*------------------------------------------------------------------------*
059400991116     OQSYSPRT   E            STM               2
059500991116     O                       STA(1)              66
059600991116     O                       STA(2)             132
059700991116     O                       DSFIRS              20
059800011210     O                       DATAI               85 '  /  /    '
059900011210     O                       ORAI                94 '  :  :  '
060000011210     O                       DATAF              111 '  /  /    '
060100011210     O                       ORAF               120 '  :  :  '
060200991116     O                       PAGE          Z    132
060300991116     O*------------------------
060400991116     O          E            STM               3
060500991116     O                       STA(3)              66
060600991116     O                       STA(3)             132
060700991116     O*------------------------
060800991116     O          E            STM               4
060900991116     O                       STA(4)              66
061000991116     O                       STA(5)             132
061100991116     O                       PARFIL        Z     21
061200991116     O                       DECFIL              47
061300991116     O                       PARKS1        Z     65
061400991116     O                       PARKS2        Z     75
061500991116     O                       DECDS1             103 '  /  /    '
061600991116     O                       DECDS2             116 '  /  /    '
061700991117     O                       PARASI        Z    126
061800991117     O                       PARCLR             132
061900991116     O*------------------------
062000991116     O          E            STM               5
062100991116     O                       STA(3)              66
062200991116     O                       STA(3)             132
062300991116     O*------------------------
062400991116     O          E            STM         2
062500991116     O                       STA(6)              66
062600991116     O                       RECLAN        2     33
062700991116     O*------------------------
062800991116     O          E            STM              55
062900991116     O                       STA(8)              99
063000991116     O*------------------------------------------------------------------------*
063100991116**   STA
063200011205XXXXXXXXXXXXXXXXXXXX      TREC11R         SIMULAZIONE C/ECONOMICO 1
063300011210  INIZIO 99/99/9999 99:99:99  FINE 99/99/9999 99:99:99     P. 99992
063400991116------------------------------------------------------------------3
063500991116LINEA DI PARTENZA 999 XXXXXXXXXXXXXXXXXXXXXXXXX   CLIENTE 9999999 4
063600010925- 9999999  DATA SPEDIZIONE 99/99/9999 - 99/99/9999 ANNO 9999     X5
063700991116RECORD LANCIATI 9.999.999.999.999                                 6
0638009911167
063900991116                           ***  F I N E   P R O S P E T T O  ***  8
0640009911169
06410099111610
