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
001701140310     Ffnlbl01l  IF   E           K DISK
001702140408     Funcmd01l  IF   E           K DISK
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
004300140304     D*TIB             S              5    DIM(100)                             TIPO INVIO BOL
004400140304     D*NIB             S              5  0 DIM(100)                             NUM.X TIPO INV
004500111205     d c_MaxCli3C      c                   const(30000)
004600100928     D CLI             S              7  0 DIM(c_MaxCli3C)                      *CLIENTI
004700100928     D CUN             S              7  0 DIM(c_MaxCli3C)                      *CLIENTI UNIF.
004701140225     D Cks             S              7  0 DIM(c_MaxCli3C)                      *CLIENTI UNIF.
004702140303     D leg             S              1    DIM(c_MaxCli3C)
004703140310     D pfi             S              1    DIM(c_MaxCli3C)                      cli con cod in + fil
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
006401140317     D car             S              3  0 DIM(500)                             *DESCRIZIONE FILIALE
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
008401140520
008402140520     d dtasflo       e ds
008403140520
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
009601140411
010001140411     D DUNR          E DS
010002140411
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
012001140221
012002140411      // - Ricerca/Controllo tabelle
012003140411     d TIBS02ds      e ds                  inz
012004140221     d tibs10ds      e ds
012005140221     d skc                           11    DIM(500) overlay(d10skc)
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
015301140307
015302140307     d                 ds
015303140311     d  sfil                   1     33  0 dim(11)
015304140311     d  STPFI1                 1      3  0
015305140311     d  STPFI2                 4      6  0
015306140311     d  STPFI3                 7      9  0
015307140311     d  STPFI4                10     12  0
015308140311     d  STPFI5                13     15  0
015309140311     d  STPFI6                16     18  0
015310140311     d  STPFI7                19     21  0
015311140311     d  STPFI8                22     24  0
015312140311     d  STPFI9                25     27  0
015313140311     d  STPFI10               28     30  0
015314140311     d  STPFI11               31     33  0
015315140310     d                 ds
015316140311     d  svfil                  1     33  0 dim(11)
015317140311     d  STvFI1                 1      3  0
015318140311     d  STvFI2                 4      6  0
015319140311     d  STvFI3                 7      9  0
015320140311     d  STvFI4                10     12  0
015321140311     d  STvFI5                13     15  0
015322140311     d  STvFI6                16     18  0
015323140311     d  STvFI7                19     21  0
015324140311     d  STvFI8                22     24  0
015325140311     d  STvFI9                25     27  0
015326140312     d  STvFI10               28     30  0
015327140311     d  STvFI11               31     33  0
015328140311     d                 ds
015329140311     d  CmK                    1     70  0 dim(10)
015330140311     d  STvks1                 1      7  0
015331140311     d  STvks2                 8     14  0
015332140311     d  STvks3                15     21  0
015333140311     d  STvks4                22     28  0
015334140311     d  STvks5                29     35  0
015335140311     d  STvks6                36     42  0
015336140311     d  STvks7                43     49  0
015337140311     d  STvks8                50     56  0
015338140311     d  STvks9                57     63  0
015339140311     d  STvks10               64     70  0
015340140311     d                 ds
015341140415     d  CmDt                   1     80  0 dim(10)
015342140415     d  STvdt1                 1      8  0
015343140415     d  STvdt2                 9     16  0
015344140415     d  STvdt3                17     24  0
015345140415     d  STvdt4                25     32  0
015346140415     d  STvdt5                33     40  0
015347140415     d  STvdt6                41     48  0
015348140415     d  STvdt7                49     56  0
015349140415     d  STvdt8                57     64  0
015350140415     d  STvdt9                65     72  0
015351140415     d  STvdt10               73     80  0
015352140415     d                 ds
015353140415     d  CmQt                   1     30  0 dim(10)
015354140415     d  STvQt1                 1      3  0
015355140415     d  STvQt2                 4      6  0
015356140415     d  STvQt3                 7      9  0
015357140415     d  STvQt4                10     12  0
015358140415     d  STvQt5                13     15  0
015359140415     d  STvQt6                16     18  0
015360140415     d  STvQt7                19     21  0
015361140415     d  STvQt8                22     24  0
015362140415     d  STvQt9                25     27  0
015363140415     d  STvQt10               28     30  0
015364140415     d                 ds
015365140415     d  Cmds                   1    350    dim(10)
015366140415     d  STvds1                 1     35
015367140415     d  STvds2                36     70
015368140415     d  STvds3                71    105
015369140415     d  STvds4               106    140
015370140415     d  STvds5               141    175
015371140415     d  STvds6               176    210
015372140415     d  STvds7               211    245
015373140415     d  STvds8               246    280
015374140415     d  STvds9               281    315
015375140415     d  STvds10              316    350
015376140307
015377140221      * - Campi di comodo
015378140221     d w_bsksc         s                   like(d10cod)
015379140225     d wlegame         s              1
015380140227     d wTipVAS         s                   like(stptrk)
015381140227     d wpes            s                   like(stppes)
015382140228     d wcass           s                   like(§4ctsd)
015383140228     d wgia            s                   like(§3KGSD)
015384140228     d wdisp           s                   like(§3KGSr)
015385140228     d wfatt           s                   like(§4CFSD)
015386140228     d wpod            s                   like(§3KCSD)
015387140303     d savleg          s                   like(stpleg)
015388140310     d savpfi          s                   like(stppfi)
015389140303     d stacass         s                   like(stpcass)
015390140303     d stagia          s                   like(stpgia)
015391140303     d stadisp         s                   like(stpdisp)
015392140303     d stafatt         s                   like(stpfatt)
015393140303     d stapod          s                   like(stppod)
015394140303     d w_kscvir        s                   like(virksc)
015395140303     d sav_i           s                   like(i)
015396140310     d is              s              6  0
015397140317     d in              s              3  0
015398140317     d ind             s              4  0
015399140310     d wpfi            s                   like(stppfi)
015400140313     d wfcli           s                   like(orgfil)
015401140313     d wcarcli         s                   like(orgcar)
015402140313     d wcarcli_sav     s                   like(orgcar)
015403140311     d stafilp         s             45
015404140311     d savfilp         s                   like(stafilp)
015405140408     d sav_cmdksc      s                   like(cmdksc)
015406140612     d staalr          s              6  0
015407140612     d wstaalr         s                   like(stvalr)
015408140415     d wdes            s             35
015409140415     d wcomod          s             39
015410140411     d wksccomod       s                   like(depksc)
015411140411     d Datadmy         s               d   datfmt(*dmy)
015413140411     D wgmy            s              6  0
015414140411     D w004a           s              4
015415140411     D ap              s              6  0                                      indice
015416140415     D wf              s              1                                         indice
015417140221
015418140221      // ? PROTOTIPI ?
015419140221      /copy gaitrasrc/srcprotopr,tibs10r
015420140411      /copy gaitrasrc/srcprotopr,tibs02r
015421000000     C*------------------------------------------------------------------------*
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
018000140221     C                   EXSR      ADD3R
018100000316     C* GENERA NEL FILE UN RECORD PER OGNI CLIENTE
018200000316     C* PRESENTE NEL FILE DEI CLIENTI ABILITATI AL TRACK & TRACING
018300110401     C****               EXSR      ADDTRK
018400110624     C* AGGIORNA IL WRK-FILE CON L'INDICAZIONE DI VAS VB/VT PERSONALIZZATO/STANDARD/?
018500140227     C******             EXSR      UPDVAS
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
021300140304     C*                  DO        100           I
021400140304     C*    TIB(I)        IFGT      *BLANK
021500140304     C* N14              EXCEPT    STAFI2
021600140304     C*                  ENDIF
021700140304     C*                  ENDDO
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
032700140520     C* AGGSTP - AGGIORNA IL FILE WFSTP01L
032800000000     C*------------------------------------------------------------------------*
032900000000     C     AGGSTP        BEGSR
033000010418     c*
033001140228     c                   eval      wpes=' '
033002140228     c                   eval      wcass='  '
033003140228     c                   eval      wgia='  '
033004140228     c                   eval      wdisp='  '
033005140228     c                   eval      wfatt='  '
033006140228     c                   eval      wpod='  '
033100010418     c*
033200970129     C* RECUPERA I DATI CLIENTE
033300970129     C                   Z-ADD     1             I
033400970129     C     WCLI          LOOKUP    CLI(I)                                 99
033500970129IF  3C     *IN99         IFEQ      *ON
033600970129     C                   Z-ADD     CUN(I)        STPKSC
033601140225     c                   eval      stpleg=leg(i)
033602140310     c                   eval      stppfi=pfi(i)
033603140303     c                   move      *zeros        w_kscvir
033604140303     c                   move      cks(i)        w_kscvir
033605140228     c                   eval      wpes=pes(i)
033606140228     c                   exsr      sr_3K4C
033607140228     c                   eval      wcass=§4ctsd
033609140228     c                   eval      wgia=§3KGSD
033612140228     c                   eval      wdisp=§3KGSr
033615140228     c                   eval      wfatt=§4CFSD
033618140228     c                   eval      wpod=§3KCSD
033700970129X   3C                   ELSE
033800140225     C****               Z-ADD     WCLI          STPKSC
033801140225     c                   eval      w_bsksc=wcli
033802140225     c                   exsr      sr_tibs10
033803140303     C                   MOVE      d10cop        stpksc                         *CLI. UNIF.
033804140225     c                   eval      stpleg=wlegame
033805140310     c                   exsr      sr_wpfi
033806140310     c                   eval      stppfi=wpfi
033807140303     c                   move      *zeros        w_kscvir
033808140303     c                   move      wcli          w_kscvir
033900970129E   3C                   ENDIF
033901140227     c* esecuzione routine per determinare se per il cliente in esame il vas è standard
033902140227     c* o personalizzato
033903140227     c                   exsr      sr_trk
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
035700140228     C***                MOVE      *ZERO         STPNCA                                        PEDIZ
035800000316     C                   MOVE      *BLANKS       STPT3R
035801140311     c                   clear                   sfil
035802140520     c                   clear                   stpalr
035803140520
035804140520     c* reperisco eventuale presenza di alert
035805140520     c                   eval      dtasflo=tasflo
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
039600140228IF  3C*    TASFCA        IFEQ      'S'
039700140228     C*                  ADD       1             STPNCA                         *CONTRASSEGNO  PEDIZ
039800140228E   2C*                  ENDIF
039801140227     c* Tipo VAS (elencati in ordine decrescente di priorità):
039802140227     c*  P=Personalizzato
039803140227     c*  E=EasySped/EasyWeb
039804140227     c*  S=Standard
039805140227     c*  ?=Non determinato
039806140227     c                   if        stptrk<>wTipVas and stptrk<>'P'
039808140227     c                   if        (stptrk='E' and wTipVas='P') or
039809140227     c                             (stptrk='?')                 or
039810140227     c                             (stptrk='S' and (wTipVas='P' or wTipVas='E'))
039811140227     c                   eval      stptrk=WtipVas
039816140227     c                   endif
039817140227     c                   endif
039818140227     c* Flag gestione peso
039819140227     c                   if        wpes=' '
039820140227     c                   eval      stppes=wpes
039821140227     c                   endif
039822140228     c                   if        stpcass=*blanks
039823140228     c                   eval      stpcass=wcass
039824140228     c                   endif
039825140228     c                   if        stpgia=*blanks
039826140228     c                   eval      stpgia=wgia
039827140228     c                   endif
039828140228     c                   if        stpdisp=*blanks
039829140228     c                   eval      stpdisp=wdisp
039830140228     c                   endif
039831140228     c                   if        stpfatt=*blanks
039833140228     c                   eval      stpfatt=wfatt
039834140228     c                   endif
039835140228     c                   if        stppod=*blanks
039836140228     c                   eval      stppod=wpod
039837140228     c                   endif
039838140311     c                   exsr      sr_memosfil
039839140520     c                   if        §floemd<>*blanks
039840140611     c                   eval      stpalr+=1
039841140520     c                   endif
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
042800140228IF  3C*    TASFCA        IFEQ      'S'
042900140228     C*                  Z-ADD     1             STPNCA                         *CONTRASSEGNO  PEDIZ
043000140228E   2C*                  ENDIF
043100970130     C                   MOVEL     STPKSC        STPFIP
043200140317     C                   MOVEL     STPKSC        KORFIL
043300140317     C***  KORFIL        CHAIN     AZORG01L                           97
043301140317     c                   eval      ind=%lookup(korfil:fil)
043302140317     c                   clear                   stpas4
043303140317     c                   if        ind>0
043304140317     c                   eval      stpas4=car(ind)
043305140317     c                   endif
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
044600140317     C*                  MOVEL     orgcar        STPAS4
044700970129     C                   Z-ADD     STPKSC        DEPKSC
044800000000     C                   EXSR      DECCLI                                       *DESCRIZIONE CLIENTE
044900000000     C                   MOVEL     DESCLI        STPDCL
044901140227     c                   eval      stptrk=WtipVas
044902140227     c                   eval      stppes=Wpes
044903140228     c                   eval      STPCASS=wcass
044904140228     c                   eval      STPGIA=wgia
044905140228     c                   eval      STPDISP=wdisp
044906140228     c                   eval      STPFATT=wfatt
044907140228     c                   eval      STPPOD=wpod
044908140311     c                   exsr      sr_memosfil
044909140520     c                   if        §floemd<>*blanks
044910140611     c                   z-add     1             stpalr
044911140520     c                   endif
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
050200140303     C***                MOVE      *ZERO         WCAPVF
050300140303     C***                MOVE      *ZERO         WCAPNF
050400140303     C***                MOVE      *ZERO         WCAPLF
050500110401     C***                MOVE      *ZERO         WCATRF
050600970130     C                   MOVE      *ZERO         WSPDSK
050700970130     C                   MOVE      *ZERO         WPDSK
050800970130     C                   MOVE      *ZERO         WPETI
050900970130     C                   MOVE      *ZEROS        STTSPT
051000970130     C                   MOVE      *ZEROS        STTCOT
051100970130     C                   MOVE      *ZEROS        STTSPA
051200010419     C                   MOVE      *ZEROS        STTSPK
051300970130     C                   MOVE      *ZEROS        STTSPB
051400140228     C***                MOVE      *ZEROS        STTSPC
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
052800140228     C***                MOVE      *ZEROS        S2TSPC
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
054101140228     C                   MOVEL     STPleg        savleg
054102140310     C                   MOVEL     STPpfi        savpfi
054200110401     C***                MOVEL     *BLANKS       STATRK            1
054300000517     C                   MOVEL     *BLANKS       STAT3R            1
054301140520     C                   clear                   STAalr
054302140612     C                   clear                   wSTAalr
054303140310     c                   clear                   svfil
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
055800140228     C****               ADD       STPNCA        STASPC
055900970130     C                   ADD       STPIMB        STACOB
056000110624     C***   TPTRK        IFNE      *BLANKS
056100110624     C***                MOVEL     STPTRK        STATRK
056200110401     C***                ENDIF
056300000517     C     STPT3R        IFNE      *BLANKS
056400000517     C                   MOVEL     STPT3R        STAT3R
056500000517     C                   ENDIF
056501140611     C**   STPalr        IFNE      *BLANKS
056502140611     C**                 MOVEL     STPalr        STAalr
056503140611     C**                 ENDIF
056509140612     c                   eval      wstaalr+=stpalr
056510140612     c                   if        wstaalr>999999
056511140612     c                   z-add     999999        staalr
056512140612     c                   else
056513140612     c                   z-add     wstaalr       staalr
056514140612     c                   endif
056600000322     C     WINDSK        IFEQ      *ZERO
056700000517     C     STPNUA        IFGT      *ZERO
056800000517     C     STPNUB        ORGT      *ZERO
056900010418     C     STPNUK        ORGT      *ZERO
057000970203     C                   MOVE      STPDAT        WINDSK
057100000517     C                   ENDIF
057200970203     C                   ENDIF
057300110624     C                   MOVEL     STPTRK        STAVAB            1
057301140228     C                   MOVEL     STPpes        STApes            1
057304140228     c                   eval      stacass=stpcass
057305140228     c                   eval      stagia=stpgia
057306140228     c                   eval      stadisp=stpdisp
057307140228     c                   eval      stafatt=stpfatt
057308140228     c                   eval      stapod=stppod
057309140310     c* schiera delle filiali di partenza
057310140310     c                   z-add     1             i
057311140311     c                   dow       i<=%elem(sfil) and sfil(i)>0
057312140310     c                   eval      in=%lookup(sfil(i):svfil)
057313140310     c                   if        in=0
057314140310     c                   eval      in=%lookup(000:svfil)
057315140310     c                   if        in>0
057316140310     c                   if        in=11
057317140310     c                   z-add     999           svfil(11)
057318140310     c                   else
057319140310     c                   z-add     sfil(i)       svfil(in)
057320140310     c                   endif
057321140310     c                   endif
057322140310     c                   endif
057323140310     c                   add       1             i
057324140310     c                   enddo
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
059000140227     C*                  Z-ADD     1             I                 6 0
059100140227     C*    DEPKSC        LOOKUP    CLI(I)                                 99
059200140227IF  3C*    *IN99         IFEQ      *OFF
059300140227     C*                  MOVEL     *BLANKS       STADET            1            *DETT.SEGN.
059400140227     C*                  MOVE      *BLANKS       STACAP            1            *TIPO CAPPARIO
059500140227     C*                  MOVEL     *BLANKS       STACBA            5            *SUPP. CLI > BART
059600140227     C*                  MOVEL     *BLANKS       STAPES            1            GEST.PESO
059700140227     C*                  MOVE      *BLANK        STACAP
059800140227     C*                  MOVEL     *BLANKS       STABAC            5            *SUPP. CLI > BART
059900140227X   3C*                  ELSE
060000140227     C*                  MOVEL     DET(I)        STADET                         *DETT.SEGN.
060100140227     C*                  MOVEL     CBA(I)        STACBA                         *SUPP. CLI > BART
060200140227     C*                  MOVEL     PES(I)        STAPES                         GEST.PESO
060300140227     C*                  MOVEL     BAC(I)        STABAC                         *SUPP. CLI > BART
060400140227     C*                  MOVEL     '1'           STACAP                         AZIST
060500140227     C*    CAP(I)        IFEQ      'S'
060600140227     C*                  MOVEL     '2'           STACAP                         AZCAP
060700140227     C*                  ENDIF
060800140227     C*    CAP(I)        IFEQ      'L'
060900140227     C*                  MOVEL     '3'           STACAP                         AZCPC
061000140227     C*                  ENDIF
061100140227E   3C*                  ENDIF
061200000000     C*
061300970130     C* VAS DA FILIALE (DATI CONS./GIACENZE)
061400970130     C*
061500140228     C*                  MOVE      *BLANK        DS3K
061600140228     C*                  MOVEL     '3K'          KTBCOD
061700140228     C*                  MOVEL     DEPKSC        KTBKEY
061800140228     C*    KEYTAC        CHAIN     TABEL00F                           99
061900140228     C* N99              MOVEL     TBLUNI        DS3K
062000970130     C*
062100970130     C* VAS DA SEDE (C/A / FATTURE)
062200970130     C*
062300140228     C*                  MOVE      *BLANK        DS4C
062400140228     C*                  MOVEL     '4C'          KTBCOD
062500140228     C*                  MOVEL     DEPKSC        KTBKEY
062600140228     C*    KEYTAC        CHAIN     TABEL00F                           99
062700140228     C* N99              MOVEL     TBLUNI        DS4C
062800970130     C*
062900970130     C* COMODATI
063000970130     C*
063100140228     C*                  MOVE      *BLANK        DS3R
063200140228     C*                  MOVEL     '3R'          KTBCOD
063300140228     C*                  MOVEL     DEPKSC        KTBKEY
063400140228     C*    KEYTAC        CHAIN     TABEL00F                           99
063500140228     C* N99              MOVEL     TBLUNI        DS3R
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
068600140228     C*    STASPB        IFLE      *ZERO
068700140228     C*                  MOVEL     *BLANKS       STACAP            1                        IO
068800140228     C*                  MOVEL     *BLANKS       STABAC                                     IO
068900140228     C*                  MOVEL     *BLANKS       STADET            1                        IO
069000140228     C*                  ENDIF
069100971021     C     STASPB        IFLE      *ZERO
069200971021     C     STASPA        ANDLE     *ZERO
069300010423     C     STASPK        ANDLE     *ZERO
069400971021     C                   MOVEL     *BLANKS       STAVAB                                     IO
069500140228     C******             MOVEL     *BLANKS       STAcba                                     IO
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
070700140228     C*    STACBA        IFGT      *BLANKS                                                   BART
070800140228     C*                  MOVEL     STACBA        WAA2              2
070900140228     C*                  MOVEL     STACBA        WSTCBA            5
071000140228     C*    WAA2          IFEQ      'HL'                                         HALLEY  XX
071100140228     C*    WAA2          OREQ      'PC'                                         PC+HALL XX
071200140228     C*    WAA2          OREQ      'WW'                                         PC+HALL XX
071300140228     C*                  MOVE      '  '          WSTCBA
071400140228     C*                  ENDIF
071500140228     C*                  Z-ADD     1             I
071600140228     C*    WSTCBA        LOOKUP    TIB(I)                                 99
071700140228     C*    *IN99         IFEQ      *OFF
071800140228     C*                  Z-ADD     1             I
071900140228     C*    *BLANK        LOOKUP    TIB(I)                                 99
072000140228     C*                  ENDIF
072100140228     C*                  ADD       1             NIB(I)
072200140228     C*                  MOVEL     WSTCBA        TIB(I)                                     IO
072300140228     C*                  ENDIF
072400971021     C* TOTALIZZO IL NUMERO DI CLIENTI
072500110624     C* CON VAS VAB/VAT STANDARD
072600110624     C     STAVAB        IFEQ      'S'                                                       BART
072601140228     C     STAVAB        oreq      'E'                                                       BART
072700971021     C                   ADD       1             WBLCF             5 0
072800971021     C                   ADD       1             WBLCT             5 0
072900971021     C                   ENDIF
073000971021     C* CON CAPPARIO:                                                   BART
073100140228     C*    STASPB        IFGT      *ZERO
073200971021     C* CON CAPPARIO AZIST
073300140228     C*    STACAP        IFEQ      '1'                                                       BART
073400140228     C*                  ADD       1             WCAPVF            5 0
073500140228     C*                  ADD       1             WCAPVT            5 0
073600140228     C*                  ENDIF
073700971021     C* CON CAPPARIO AZCAP
073800140228     C*    STACAP        IFEQ      '2'                                                       BART
073900140228     C*                  ADD       1             WCAPNF            5 0
074000140228     C*                  ADD       1             WCAPNT            5 0
074100140228     C*                  ENDIF
074200971021     C* CON CAPPARIO AZCPC
074300140228     C*    STACAP        IFEQ      '3'                                                       BART
074400140228     C*                  ADD       1             WCAPLF            5 0
074500140228     C*                  ADD       1             WCAPLT            5 0
074600140228     C*                  ENDIF
074700140228     C*                  ENDIF
074800000419     C* TOTALIZZO I CLIENTI ABILITATI AL TRACK & TRACING
074900110401     C***  STATRK        IFEQ      'X'
075000110401     C***                ADD       1             WCATRF            5 0
075100110401     C***                ADD       1             WCATRT            5 0
075200110401     C***                ENDIF
075300990204     C* TOTALIZZO SEMPRE MA STAMPO SOLO SE RIENTRA NEI PARAMETRI        BART
075400990204     C     WSCART        IFEQ      'S'
075401140304     C*  COMODATI
075402140411     c                   clear                   wcomod
075403140304     c                   clear                   sav_i
075404140415     c                   clear                   CmK
075405140415     c                   clear                   CmDt
075406140415     c                   clear                   CmQt
075407140415     c                   clear                   CmDs
075408140415     c                   clear                   stvksal
075409140415     c                   z-add     1             in
075410140304    1c                   if        stat3r<>*blanks
075411140410     C****               MOVEL     '3R'          KTBCOD
075412140415     c*  Cliente singolo --> reperisco i comodati del cliente
075413140304    2c                   if        savleg=' '
075414140410     C****               MOVEL     DEPKSC        KTBKEY
075415140410     C***  KEYTAC        CHAIN     TABEL00F
075416140410    3c****               if        %found(tabel00f)
075417140410     C****               MOVEL     TBLUNI        DS3R
075418140410    3c***                endif
075419140411     c                   move      depksc        wksccomod
075420140410     c                   exsr      Sr_Comodato
075432140304   x2c                   else
075433140304     c*  Cliente non singolo --> reperisco clienti della famiglia
075434140304     c                   eval      w_bsksc=depksc
075435140304     c                   exsr      sr_tibs10
075436140304     c                   clear                   wtrov             1
075437140304     c                   z-add     1             i
075438140304    3c                   dow       i<=%elem(skc) and skc(i)<>*blanks
075439140304     c                             and wtrov=' '
075440140411     c****               eval      ktbkey=%subst(skc(i):5:7)
075441140411     C***  KEYTAC        CHAIN     TABEL00F
075442140411    4c***                if        %found(tabel00f)
075443140411     C***                MOVEL     TBLUNI        DS3R
075444140411     c                   move      skc(i)        wksccomod
075445140411     c                   exsr      sr_comodato
075446140411     c                   if        wcomod<>*blanks
075447140304     c                   eval      wtrov='1'
075448140304     c                   eval      sav_i=i+1
075449140304    4c                   endif
075450140304     c                   add       1             i
075451140304    3c                   enddo
075452140304    2c                   endif
075453140304    1c                   endif
075454140306     c* se non ho comodato da stampare pulisco anche il codice
075455140411     c                   if        wcomod=*blanks
075456140411     c                   clear                   wksccomod
075457140306     c                   endif
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
076500140411     c     wcomod        ANDNE     *blanks
076600010626     C                   EXCEPT    TESTAT                                       *SALTO PAGINA
076700010626     C                   EXCEPT    FIP1                                         *DECODIFICA FILIALE
076800010626     C                   EXCEPT    SPAZIO1                                      *DECODIFICA FILIALE
076900010627     C                   SETON                                        88
077000010626     C                   ENDIF
077100010627     C                   ENDIF
077200990204     C                   ENDIF
077332140228
077333140304    1c                   if        not *in14
077334140304    2c                   if        savleg='S'
077335140304     c                   except    rigahi
077336140304    2c                   endif
077400140304     C                   EXCEPT    RIGA
077401140304    1c                   endif
077402140310
077500140304    1C   14STASPB        IFEQ      0
077600140411     c     wcomod        ANDNE     *blanks
077601140304    2c                   if        savleg='S'
077602140304     c                   except    riga2hi
077603140304    2c                   endif
077700140228     C                   EXCEPT    RIGA2
077800140304    1C                   ENDIF
077900010419     c* se presente materiale in comodato lo stampo in seconda riga
078000140303     c* n14§3rnot        ifne      *blanks
078100140303     c*                  except    RIGA1
078200140303     c*                  endif
078300010627     C*****              SETON                                        88
078307140311     c* ----------------------------------
078308140311     c* SOLO SE HO STAMPATO La PRIMA RIGA:
078309140311     c* ----------------------------------
078310140411    0c                   if        not *in14 or (staspb=0 and wcomod<>*blanks)
078313140311     c* Devo stampare anche la SECONDA RIGA se:
078314140311     c* 1) il cliente padre ha più di un cliente con comodato
078315140310     c*    oppure
078316140311    1c                   if        (savleg='S' and sav_i>0) or
078317140317     c* 2) il cliente ha almeno una filiale di partenza bolle
078318140317     c                              svfil(1)>*zeros
078319140311     c
078320140311     c* preparo la stringa delle filiali di partenza
078321140311     c                   clear                   stafilp
078322140311     c                   clear                   savfilp
078323140317     c                   if        svfil(1)>*zeros
078324140311     C                   EXSR      sr_StringaFilP
078325140311     c                   endif
078326140310     c* se presenti più comodati li recupero e stampo
078327140310    2c                   if        savleg='S' and sav_i>0
078328140311     c                   exsr      Sr_AltriComo
078329140310   x2c                   else
078330140311     c* stampo la riga delle sole filiali di partenza se non presenti altri comodati
078331140411     c                   clear                   wcomod
078332140411     c                   clear                   wksccomod
078333140311     c                   exsr      sr_AltraRiga
078334140310    2c                   endif
078335140310    1c                   endif
078336140311     c* SCARICO I Dati Stampati nel FILE Di Work
078337140311     c****               if        not *in14 or (staspb=0 and §3rnot<>*blanks)
078338110817     c                   exsr      sr_wrtstv
078339140311    0c                   endif
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
079400140228     C****               ADD       STASPC        STTSPC            8 0
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
081500140228     C****               ADD       STASPC        STFSPC            8 0
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
083900140228     C****               ADD       STASPC        S2TSPC            8 0
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
086000140228     C****               ADD       STASPC        S2FSPC            8 0
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
088600140228     C****               MOVE      *ZEROS        STASPC            8 0
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
098500140317     C***  KORFIL        CHAIN     AZORG01L                           99
098600140317IF  1C**** *IN99         IFEQ      *ON
098601140317     c                   eval      ind=%lookup(korfil:fil)
098602140317     c                   if        ind=0
098700970130     C                   MOVEL     *ALL'*'       DESFIP           30
098800000000X   1C                   ELSE
098900140317     C*****              MOVEL     ORGDES        DESFIP
098901140317     C                   MOVEL     dfil(ind)     DESFIP
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
100901140317     C*---
100902140317     C* ORGANIGRAMMA
100903140317     C*---
100904140317     C                   Z-ADD     *ZEROS        I
100905140317     C     *LOVAL        SETLL     AZORG01L
100906140317     C                   READ      AZORG01L                               98
100907140317DO  1C     *IN98         DOWEQ     *OFF
100908140317IF  2C     ORGFAG        IFNE      'V'
100909140317     C                   ADD       1             I
100910140317     C                   Z-ADD     ORGFIL        FIL(I)
100911140317     C                   MOVEL     ORGDES        DFIL(I)
100912140317     C                   MOVEL     ORGDE3        OG143
100913140317     c                   movel     orgcar        car(i)
100914140317E   2C                   ENDIF
100915140317     C                   READ      AZORG01L                               98
100916140317E   1C                   ENDDO
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
104801140221     c                   eval      w_bsksc=cli(I)
104802140221     c                   exsr      sr_tibs10
104803140411     c                   eval      ap=%lookup(d10cop:cun)
104900140303     C                   MOVE      d10cop        CUN(I)                         *CLI. UNIF.
104901140225     C                   eval      leg(i)=wlegame
104902140310     c
104904140313     c* cliente in più aree  in base alle filiali cliente del gruppo
104905140411     c                   if        ap>0
104906140411     c                   eval      pfi(i)=pfi(ap)
104907140411     c                   else
104908140310     c                   exsr      sr_wpfi
104916140310     c                   move      wpfi          pfi(i)
104917140411     c                   endif
104918140310     C
104919140225     C                   MOVEL     §3CCKS        CKS(I)                         *CLI. UNIF.
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
107800000710     C*
107900000000     C                   ENDSR
108000140227     C*------------------------------------------------------------------------*
108100970912     C* ADD3R  - GENERA NEL FILE UN RECORD PER OGNI CLIENTE
108200970912     C*          PRESENTE NELLA TABELLA 3R E NON GIA' PRESENTE NEL FILE
108300970912     C*------------------------------------------------------------------------*
108400970912     C     ADD3R         BEGSR
108500140408     C***                SETOFF                                       9596
108501140408     c                   clear                   sav_cmdksc
108600140408     C***                MOVEL     '3R'          KTBCOD
108700140408     C***  KEYTAB        SETLL     TABEL00F
108800140408     C***  KEYTAB        READE     TABEL00F                               96
108900140408     C***  *IN96         DOWEQ     *OFF
109000140408     C***                MOVEL     TBLUNI        DS3R
109100140408     C***  TBLFLG        IFEQ      *BLANKS
109200140408     C**   §3RNOT        ANDGT     *BLANKS
109204140408     c***                movel     tblkey        w0070             7 0
109300140408     C***                z-add     w0070         w_bsksc
109301140408     c     *loval        setll     uncmd01l
109302140408     c                   read      uncmd01l
109303140408     c                   dow       not %eof(uncmd01l)
109304140408     c* escludo contratti annullati o chiusi ed elaboro solo a nuovo cod cliente
109305140411     c                   if        cmdcfg<>'A' and cmdcfg<>'C'
109306140411     c                             and cmdksc<>sav_cmdksc
109307140408     C                   z-add     cmdksc        w_bsksc
109308140221     c                   exsr      sr_tibs10
109309140303     c                   move      d10cop        stpksc
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
110700140228     C***                MOVE      *ZERO         STPNCA
110800000316     C                   MOVE      'X'           STPT3R
110900110624     C***                MOVE      *BLANKS       STPTRK
111000110624     C                   MOVE      '?'           STPTRK
111001140227     C                   clear                   STPpes
111002140520     C                   clear                   STPalr
111003140227     c                   eval      stpleg=wlegame
111004140411     c                   eval      ap=%lookup(d10cop:cun)
111005140411     c                   if        ap>0
111006140411     c                   eval      stppfi=pfi(ap)
111007140411     c                   else
111008140310     c                   exsr      sr_wpfi
111009140310     c                   eval      stppfi=wpfi
111010140411     c                   endif
111100970912     C*
111200970912     C                   MOVEL     STPKSC        STPFIP
111300970912     C                   MOVEL     STPKSC        KORFIL
111400140317     C***  KORFIL        CHAIN     AZORG01L                           97
111401140317     c                   eval      ind=%lookup(korfil:fil)
111402140317     c                   clear                   stpas4
111403140317     c                   if        ind>0
111404140317     c                   eval      stpas4=car(ind)
111405140317     c                   endif
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
112700140317     C***                MOVEL     orgcar        STPAS4
112800970912     C                   Z-ADD     STPKSC        DEPKSC
112900970912     C                   EXSR      DECCLI                                       *DESCRIZIONE CLIENTE
113000970912     C                   MOVEL     DESCLI        STPDCL
113100970912     C                   WRITE     WFSTP                                        *INSERISCE
113200970915     C                   ELSE
113300000317     C                   MOVE      'X'           STPT3R
113400970915     C                   UPDATE    WFSTP
113500970912     C                   ENDIF
113501140408     c                   eval      sav_cmdksc=cmdksc
113502140408     c     cmdksc        setgt     uncmd01l
113600970912     C                   ENDIF
113700140408     C***  KEYTAB        READE     TABEL00F                               96
113701140408     C                   READ      uncmd01l
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
128305140311     c* pulisco i campi che possono non essere impostati
128306140311     c                   clear                   stvabl
128307140311     c                   clear                   stvcot
128308140311     c                   clear                   stvgio
128309140311     c                   clear                   stvspg
128310140311     c                   clear                   stvcog
128311140311     c                   clear                   stvvsp
128312140311     c                   clear                   stvpes
128313140311     c                   clear                   stvtsd
128314140311     c                   clear                   stvgsd
128315140311     c                   clear                   stvgsr
128316140311     c                   clear                   stvfsd
128317140311     c                   clear                   stvcsd
128318140311     c                   clear                   stvdsm
128319140520     c                   clear                   stvalr
128320140311
128321110817     c                   z-add     datcor        STVDEL
128322110817     c                   eval      STVUSR=knmus
128323110817     c                   z-add     pardti        STVDAL
128324110817     c                   z-add     pardtf        sTVAL
128325110817     c                   z-add     depksc        STVKSC
128326140311     c                   eval      STVRAG=descli
128327140311     c                   if        acoabl<>*blank
128328140311     c                   eval      STVABL='*'
128329140311     c                   endif
128330140228     c                   movel     savleg        STVleg
128331140311     c                   movel     savpfi        STVpfi
128332110901     c* reperisco il commerciale del cliente
128333140311     c                   eval      stvage=clpage
128334110901
128335110817     c                   z-add     staspt        STVSPT
128341110817     c  n14              z-add     stacot        STVCOT
128342110817     c  n14              z-add     stagio        STVGIO
128343110817     c  n14              z-add     staspg        STVSPG
128344110817     c  n14              z-add     stacog        sTVCOG
128345110817     c                   z-add     staspa        stvspa
128346110817     c                   z-add     staspb        stvspb
128347110817     c                   z-add     staspk        stvspk
128348140228     c* n14              eval      STVDCL=stadet
128349140228     c* n14              eval      STVVCP=stacap
128350110817     c  n14              eval      STVVSP=stavab
128351110817     c  n14              eval      STVPES=stapes
128352140228     c* n14              eval      STVCBA=stacba
128353140228     c* n14              z-add     staspc        stvspc
128354140228     c  n14              eval      STVTSD=stacass
128355140228     c  n14              eval      STVGSD=stagia
128356140228     c  n14              eval      STVGSR=stadisp
128357140228     c  n14              eval      STVFSD=stafatt
128358140228     c  n14              eval      STVCSD=stapod
128359110817     c  n14              eval      STVDSM=WINDSD
128360140612     c  n14              eval      STValr=Wstaalr
128361140228     c* n14              eval      STVBAC=stabac
128362140311     c*
128363140311     c****               if        §3rnot<>*blanks
128364140311     c****               eval      STVCOM=§3rnot
128365140311     c****               movel     ktbkey        stvage
128366140311     c****               endif
128367110817     c                   z-add     worfil        STVFIP
128368110817     c                   z-add     destrp        STVAS4
128369110817     c                   write     wfstv000
128370140228     c* se cliente con legame verifico presenza di altri comodati stampare e memorizzare
128371140311     c***                if        stvleg='S' and sav_i>0
128372140311     c***                exsr      sr_AltriComo
128373140311     c***                endif
128374110817     C                   ENDSR
128375140221     C*------------------------------------------------------------------------*
128376140227     C* Ricerca codice padre del legame "ST"
128377140221     C*------------------------------------------------------------------------*
128378140221     C     sr_tibs10     BEGSR
128379140225     c                   eval      wlegame='S'
128380140221     c* verifico se codice presente come Padre
128381140221     c                   clear                   tibs10ds
128382140221     c                   eval      d10tle='ST'
128383140221     c                   eval      d10paf='F'
128384140221     c                   eval      d10cod=w_bsksc
128385140221     c                   call      'TIBS10R'
128386140221     c                   parm                    tibs10ds
128387140221     c                   if        d10cop>0
128388140221     c                   leavesr
128389140221     c                   endif
128390140221     c* verifico se codice presente come Figlio
128391140221     c                   eval      d10paf='P'
128392140221     c                   call      'TIBS10R'
128393140221     c                   parm                    tibs10ds
128394140221     c                   if        d10cop>0
128395140221     c                   leavesr
128396140221     c                   endif
128397140221     c* Se non è nè padre nè figlio è Singolo
128398140221     c                   eval      d10cop=d10cod
128399140225     c                   clear                   wlegame
128400140221     c
128401140221     c                   endsr
128402140227     C*------------------------------------------------------------------------*
128403140227     C* Detemina tipo VAS: Personalizzato, easyweb/speb, o standard
128404140227     C*------------------------------------------------------------------------*
128405140227     C     sr_trk        BEGSR
128406140227     c                   eval      wTipVas='?'
128407140227     c     w_kscvir      setll     tivir01l
128408140227     c     w_kscvir      reade     tivir01l
128409140227     C                   DOW       not %eof(TIVIR01L) and wtipVaS<>'P'
128410140227     C                   IF        wTipVAS <> 'P'
128411140227     C* Solo tipi file VB e VT
128412140227     C                   IF        VIRTIP = 'VB' OR VIRTIP='VT'
128413140227     C*
128414140227     C* Solo se anagrafica correntemente valida
128415140227     C                   IF        DATCOR >= VIRDTI AND
128416140227     C                             DATCOR <= VIRDTF
128417140227     C*
128418140227     C* Verifico se trattasi d personalizzato
128419140227     C                   IF        VIRPGT <> *blanks AND
128420140227     C                             (%subst(VIRPGT:1:5) <> 'TIS7T' OR
128421140227     C                              %subst(VIRPGT:1:6)  = 'TIS7TL')
128422140227     C                   EVAL      wTipVAS = 'P'
128423140227     c                   else
128424140227     C                   EVAL      wTipVAS = 'S'
128425140227     c                   endif
128426140227     c                   endif
128427140227     c                   endif
128428140303     c                   endif
128429140227     c     w_kscvir      reade     tivir01l
128430140227     c                   enddo
128431140303     c                   eval      i=%lookup(%dec(%subst(w_kscvir:2:7):7:0)
128432140303     c                             :cli)
128433140303     c                   if        i>0
128434140227     c                   select
128435140227     c                   when      cba(i) = 'SNA'
128436140227     c                   eval      wTipVas='S'
128437140227     c                   when      cba(i) = 'ESWEB' or
128438140227     c                             cba(i) = 'ESYSP'
128439140227     c                   eval      wTipVas='E'
128440140227     c                   endsl
128441140303     c                   endif
128442140227     c                   endsr
128443140228     C*------------------------------------------------------------------------*
128444140228     C* Recupero dati da tabella 3k e 4c
128445140228     C*------------------------------------------------------------------------*
128446140228     C     sr_3k4c       BEGSR
128447140228     C* VAS DA FILIALE (DATI CONS./GIACENZE)
128448140228     C*
128449140228     C                   MOVE      *BLANK        DS3K
128450140228     C                   MOVEL     '3K'          KTBCOD
128451140228     C                   MOVEL     wcli          KTBKEY
128452140228     C     KEYTAC        CHAIN     TABEL00F
128453140228     c                   if        %found(tabel00f)
128454140228     C                   MOVEL     TBLUNI        DS3K
128455140228     c                   endif
128456140228     C*
128457140228     C* VAS DA SEDE (C/A / FATTURE)
128458140228     C*
128459140228     C                   MOVE      *BLANK        DS4C
128460140228     C                   MOVEL     '4C'          KTBCOD
128461140228     C                   MOVEL     wcli          KTBKEY
128462140228     C     KEYTAC        CHAIN     TABEL00F
128463140228     c                   if        %found(tabel00f)
128464140228     C                   MOVEL     TBLUNI        DS4C
128465140228     c                   endif
128466140228     c                   endsr
128467140228     C*------------------------------------------------------------------------*
128468140228     C* Reperimento e stampa dei comodati per clienti legati
128469140228     C*------------------------------------------------------------------------*
128470140228     C     sr_AltriComo  BEGSR
128471140303     c                   z-add     sav_I         i
128472140303     c                   dow       i<=%elem(skc) and skc(i)<>*blanks
128473140411     C*****              MOVEL     '3R'          KTBCOD
128474140411     c****               eval      ktbkey=%subst(skc(i):5:7)
128475140411     C**** KEYTAC        CHAIN     TABEL00F
128476140411     c****               if        %found(tabel00f)
128477140411     C***                MOVEL     TBLUNI        DS3R
128478140411     c                   move      skc(i)        wksccomod
128479140411     c                   exsr      sr_comodato
128480140411     c                   if        wcomod<>*blanks
128481140311     c                   exsr      sr_AltraRiga
128494140311
128495140310     c*                  eval      STVCOM=§3rnot
128496140310     c*                  movel     ktbkey        stvage
128497140310     c*                  clear                   STVSPT
128498140310     c*                  clear                   STVCOT
128499140310     c*                  clear                   STVGIO
128500140310     c*                  clear                   STVSPG
128501140310     c*                  clear                   STVCOG
128502140310     c*                  clear                   STVSPA
128503140310     c*                  clear                   STVSPB
128504140310     c*                  clear                   STVSPK
128505140310     c*                  clear                   STVVSP
128506140310     c*                  clear                   STVPES
128507140310     c*                  clear                   STVTSD
128508140310     c*                  clear                   STVGSD
128509140310     c*                  clear                   STVGSR
128510140310     c*                  clear                   STVFSD
128511140310     c*                  clear                   STVCSD
128512140310     c*                  clear                   STVDSM
128513140310     c*                  write     wfstv000
128514140411     c**                 endif
128515140228     c                   endif
128516140303     c                   add       1             i
128517140228     c                   enddo
128518140228     c                   endsr
128519140310     C*------------------------------------------------------------------------*
128520140310     C*  Dato elenco clienti del legame st verifica se codici di più filiali
128521140310     C*------------------------------------------------------------------------*
128522140310     C     sr_wpfi       BEGSR
128541140313     c                   z-add     1             is
128542140313     c                   clear                   wpfi
128543140313     c                   clear                   wfcli
128544140313     c                   clear                   wcarcli_sav
128545140313     c                   clear                   wcarcli
128546140313    3c                   dow       is<=%elem(skc) and skc(is)>*zeros
128547140313     c                             and wpfi=' '
128548140313     c* reperisco il codice area della filiale cliente
128549140313     c                   eval      wfcli=%dec(%subst(skc(is):5:3):3:0)
128550140313     c                   clear                   orgcar
128551140317     c***  wfcli         chain     azorg01l
128552140317     c                   eval      ind=%lookup(wfcli:fil)
128553140317     c                   if        ind>0
128554140317     c                   eval      wcarcli=car(ind)
128555140313     c                   if        wcarcli_sav>*zeros and wcarcli_sav<>wcarcli
128556140313     c                   eval      wpfi='S'
128557140313     c                   endif
128558140313     c                   eval      wcarcli_sav=wcarcli
128559140317     c                   endif
128560140313     c                   add       1             is
128561140313     c                   enddo
128562140313     c
128563140310     c                   endsr
128564140310     C*------------------------------------------------------------------------*
128565140310     C*  Memorizza in schiera i comodati del cliente padre
128566140310     C*------------------------------------------------------------------------*
128567140310     C     sr_Memcomo    BEGSR
128568140311     c                   if        in>=11
128569140311     c                   eval      stvksal='+'
128570140311     c                   else
128571140411     c                   if        wcomod<>*blanks
128572140411     c                   eval      CmK(in)=wksccomod
128573140415     c                   eval      CmDt(in)=cmddim
128574140415     c                   eval      CmQt(in)=cmdqta
128575140415     c                   eval      CmDs(in)=wdes
128576140310     c                   add       1             in
128577140310     c                   endif
128578140311     c                   endif
128579140310     c                   endsr
128580140311     C*------------------------------------------------------------------------*
128581140311     C*  Imposta stringa dielle filiali di partenza per la stampa
128582140311     C*------------------------------------------------------------------------*
128583140311     C     sr_StringaFilPBEGSR
128584140311     c                   movel     '('           stafilp
128585140311     c                   z-add     1             i
128586140311     c                   dow       i<=%elem(svfil) and svfil(i)>*zeros
128587140311     c                   if        i>1
128588140311     c                   eval      stafilp=%trim(stafilp)+'/'
128589140311     c                   endif
128590140311     c                   if        i<11
128591140311     c                   eval      stafilp=%trim(stafilp)+%editc(svfil(i):'X')
128592140311     c                   else
128593140311     c                   eval      stafilp=%trim(stafilp)+'...'
128594140311     c                   endif
128595140311     c                   add       1             i
128596140311     c                   enddo
128597140311     c                   eval      stafilp=%trim(stafilp)+')'
128598140311     c                   eval      savfilp=stafilp
128599140311     c                   endsr
128600140311     C*------------------------------------------------------------------------*
128601140311     C*  Stampa dalla seconda riga in poi per comodati/fiiliali di partenza
128602140311     C*------------------------------------------------------------------------*
128603140311     C     sr_AltraRiga  BEGSR
128604140311IF  3C     *INOF         IFEQ      *ON
128605140311     C  N14              EXCEPT    TESTA
128606140311     C   14              EXCEPT    TESTAT
128607140311    2c                   if        savleg='S'
128608140311     c   14              except    riga2hi
128609140311     c  n14              except    rigahi
128610140311    2c                   endif
128611140311     c                   eval      stafilp=savfilp
128612140311     C   14              EXCEPT    RIGA2bof
128613140311     c  n14              except    RIGA1of
128614140311     c                   else
128615140311     C   14              EXCEPT    RIGA2b
128616140311     c  n14              except    RIGA1
128617140311     c                   clear                   stafilp
128618140311E   3C                   ENDIF
128619140311     c                   endsr
128620140311     C*------------------------------------------------------------------------*
128621140311     C*  memorizza in schiera le filiali di partenza bolle
128622140311     C*------------------------------------------------------------------------*
128623140311     C     sr_memosfil   BEGSR
128624140317     c* solo se la linea di partenza bolla <> dalla filiale cliente
128625140317     c                   if        stpfip<>taslnp
128626140317     c* solo se l'area della linea di partenza bolla <> dall'area della filiale cliente
128627140317     c                   eval      ind=%lookup(taslnp:fil)
128628140317     c                   if        ind>0 and car(ind)<>stpas4
128629140317     c* se non è bolla figlia
128630140311     c     keylbl        setll     fnlbl01l
128631140311     c                   if        not %equal(fnlbl01l)
128632140311     c* devo memorizzare la linea di partenza della bolla
128633140311     c                   if        %lookup(taslnp:sfil)=0
128634140311     c                   eval      i=%lookup(0:sfil)
128635140311     c                   if        i>0
128636140311     c                   if        i=11
128637140311     c                   z-add     999           sfil(I)
128638140311     c                   else
128639140311     c                   z-add     taslnp        sfil(i)
128640140311     c                   endif
128641140311     c                   endif
128642140311     c                   endif
128643140311     c                   endif
128644140317     c                   endif
128645140317     c                   endif
128646140311     c                   endsr
128647140410     C*------------------------------------------------------------------------*
128648140410     C*  Recupera comodato cliente per la stampa / WFSTV
128649140410     C*------------------------------------------------------------------------*
128650140410     C     Sr_Comodato   BEGSR
128651140411     c                   clear                   wcomod
128652140415     c                   clear                   wf
128653140411     c     wksccomod     setll     uncmd01l
128654140411     c     wksccomod     reade     uncmd01l
128655140415     c                   dow       not %eof(uncmd01l)
128656140410     c                   if        cmdcfg<>'A' and cmdcfg<>'C'
128657140411     c* - decodifica del tipo/modello da tabella UNR
128658140411     c                   exsr      ric_dunr
128659140415     c                   if        wcomod<>*blanks
128660140415     c                   if        wf=*blanks
128661140415     c                   eval      wcomod = %trim(wcomod) + ' + Altri'
128662140415     c                   eval      wf='1'
128663140415     c                   endif
128664140415     c                   else
128665140411     c* compongo stringa da stampare/memorizzare come segue
128666140411     c* data immissione nel formato gg/mm
128667140411     c                   eval      datadmy=%date(cmddim:*iso)
128668140411     c                   eval      wgmy=%dec(datadmy)
128669140411     c                   movel     wgmy          w004a
128670140415     c                   eval      wcomod=%trim(wcomod) +
128671140411     c                             %subst(w004a:1:2) + '/' +
128672140411     c                             %subst(w004a:3:2) + ':'
128678140411     c* - quantità + decodifica modello
128679140415     c                   eval      wcomod=%trim(wcomod) +
128680140415     c                             %trim(%editc(cmdqta:'Z')) + ' ' +
128681140415     c                             %trim(%subst(wdes:1:21))
128682140415     c                   endif
128683140415     c* memorizzo il comodato nel file WFSTV
128684140415     c                   exsr      sr_memcomo
128688140410     c                   endif
128689140411     c     wksccomod     reade     uncmd01l
128690140410     c                   enddo
128695140410     c                   endsr
128696140411     C*------------------------------------------------------------------------*
128697140411     C*  Ricerca decodifica tipo/modello  - tab UNR
128698140411     C*------------------------------------------------------------------------*
128699140411     c     ric_dunr      begsr
128700140411      /free
128701140411       // Tipo/modello e relativa decodifica
128702140411         clear dunr;
128703140411         clear wdes;
128704140411         clear tibs02ds  ;
128705140411         t02tla=' '      ;
128706140411         t02mod='C'      ;
128707140411         t02sif=knsif    ;
128708140411         t02cod='UNR'    ;
128709140411         t02ke1=cmdtip   ;
128710140411         TNTBE_RicercaControllo  (kpjba : tibs02ds);
128711140411
128712140411         if  t02err=*blanks ;
128713140411            dunr=t02uni;
128714140411            wdes   = D§UNRDES;
128715140411         ELSE;
128716140411            wdes   = *ALL'?' ;
128717140411         endif;
128718140411      /end-free
128719140411     c                   endsr
128720000000     C*------------------------------------------------------------------------*
128721010626     C* *§NZSR - IMPOSTAZIONI INIZIALI
128722000000     C*------------------------------------------------------------------------*
128723000000     C     *INZSR        BEGSR
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
133701140310     C* Legami bolla
133702140310     C     KEYlbl        KLIST
133703140310     C                   KFLD                    TASAAS
133704140310     C                   KFLD                    TASlnp
133705140310     C                   KFLD                    TASnrs
133706140310     C                   KFLD                    TASnsp
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
146201140304     O          E            RIGAHI         0
146202140304     O                       DEPKSC               7
146300000000     O          E            RIGA           1
146400000000     O                       DEPKSC               7
146500000000     O                       DESCLI            +  1
146501140311     O                       savpfi            +  2
146600140311     O                       STASPT        Z   +  2
146700970129     O                       STACOT        Z   +  1
146800970129     O                       STAGIO        Z   +  1
146900970129     O                       STASPG        Z   +  1
147000970129     O                       STACOG        Z   +  1
147100970129     O                       STASPA        Z   +  1
147200970129     O                       STASPB        Z   +  1
147300010423     O                       STASPK        Z   +  1
147400110817     O****                   STASPH        Z   +  1
147500140228     O****                   STADET            +  2
147600140228     O****                   STACAP            +  2
147700971021     O                       STAVAB            +  2
147800990225     O                       STAPES            +  2
147900140228     O***                    STACBA            +  1
148000140228     O****                   STASPC        Z   +  1
148100140228     O                       stacass           +  2
148200140611     O                       stagia            +  2
148300140611     O                       stadisp           +  3
148400140304     O                       stafatt           +  3
148500140304     O                       stapod            +  2
148600110401     O***                    STATRK            +  4
148700140306     O*                      §3RNOT            +  3
148701140611     o                       staalr        z   +  1
148800140611     O                       WINDSD            +  1
148900140228     O*****                  STABAC            +  4
148901140611     o                       wksccomod     z   +  2
148902140411     O                       wcomod             198
149000010419     O*------------------------------------
149100010419     O          E            RIGA1          1
149101140311     o                       stafilp           +  8
149102140411     o                       wksccomod     z    157
149200140411     O                       wcomod             198
149201140305     O          E            RIGA1OF        1
149203140305     O                       DEPKSC               7
149204140311     O                       stafilp           +  1
149205140411     o                       wksccomod     z    157
149206140411     O                       wcomod             198
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
153800140303     O*                                        +  3 'CAPPARIO 1='
153900140303     O*                      WCAPVF        2   +  1
154000140303     O*                                        +  3 'CAPPARIO 2='
154100140303     O*                      WCAPNF        2   +  1
154200140303     O*                                        +  3 'CAPPARIO 3='
154300140303     O*                      WCAPLF        2   +  1
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
158900140303     O**                                       +  3 'CAPPARIO 1='
159000140303     O**                     WCAPVT        2   +  1
159100140303     O**                                       +  3 'CAPPARIO 2='
159200140303     O**                     WCAPNT        2   +  1
159300140303     O***                                      +  3 'CAPPARIO 3='
159400140303     O***                    WCAPLT        2   +  1
159500110401     O****                                     +  3 'T&T='
159600110401     O****                   WCATRT        2   +  1
159700140304     O*         E            STAFIN         1
159800140304     O*                                          24 'TOTALI CLIENTI PER TIPO '
159900140304     O*                                        +  1 'SUPPORTO VAS BOLLE'
160000140304     O*         E            STAFIN         1
160100140304     O*                                          11 'TIPO NUMERO'
160200140304     O*         E            STAFI2         1
160300140304     O*                      TIB(I)               5
160400140304     O*                      NIB(I)        2   +  1
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
165701140304     O          E            RIGA2HI        0
165702140304     O                       DEPKSC
165800010626     O          E            RIGA2          1
165900070626     O                       DEPKSC
166000070626     O                       DESCLI             + 1
166001140311     O                       savpfi             + 2
166100140311     O                       STASPT        Z    + 3
166200070626     O                       STASPA        Z    + 3
166300070626     O                       STASPB        Z    + 1
166400070626     O                       STASPK        Z    + 1
166500110817     O***                    STASPH        Z    + 1
166501140611     o                       wksccomod     z    + 1
166600140611     O                       wcomod             + 1
166601140228     O          E            RIGA2b         1
166602140311     o                       stafilp            + 8
166603140611     o                       wksccomod     z     89
166609140611     O                       wcomod             + 1
166610140305     O          E            RIGA2bOf       1
166611140305     O                       DEPKSC
166612140311     O                       stafilp            + 1
166613140611     o                       wksccomod     z     89
166614140611     O                       wcomod             + 1
166700070626      *
166800000321** CMD1 - COMANDI CLP
166900000321CLRPFM FILE(*LIBL/WFSTP00F)
166901110817CLRPFM FILE(*LIBL/WFSTV00F)
167000970130** TES  - RIGHE DI TESTATA
167100000316------------------------------------------------------------------  1
167200140321CLIENTE                                Cli  TOTALE    TOTALE  GG    2
167300140611 SPEDIZ.    COLLI ---- SPEDIZIONI DISK ----- TR PE -- S U P P O R   3
167400140611T O -- TOTALE MM  MATERIALE IN COMODATO                             4
167500140404        Partenza Bolle Altre Aree(xxx) aAr  SPEDIZ.    COLLI  RIT   52
167600140611X GIORNO X GIORNO      "A"       "B" "C" CLI SP SO C/A GIA DISP FA  63
167700140611TT POD  ALERT DK  Codice   Descrizione                              74
167800970130                                                                    8
167900010626** TE1  - RIGHE DI TESTATA QSYSPRT
168000010626------------------------------------------------------------------  1
168100140321CLIENTE                            ABL Cli   TOTALE    ---- SPEDIZ  2
168200140311IONI  DISK ---- MATERIALE IN COMODATO                               3
168300140404        Part.Bolle Altre Aree(xxx) (*) aAr   SPEDIZ.        "A"     4
168400140611   "B"   "C"CLI Codice  Descrizione                                 5
