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
001400110817     FWFSTV00F  o  A E             DISK    USROPN
001500000000     FAZORG01L  IF   E           K DISK
001600000000     FTABEL00F  IF   E           K DISK
001700110401     F***TIVSS04L  IF   E           K DISK
001800110624     FTIVIR01L  IF   E           K DISK
001900140310     Ffnlbl01l  IF   E           K DISK
002000140408     Funcmd01l  IF   E           K DISK
002100000000     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
002200000000     F                                     FORMLEN(66)
002300000000     F                                     FORMOFL(60)
002400010626     FQSYSPRT   O    F  132        PRINTER
002500000000     L*------------------------------------------------------------------------*
002600000000     L* ESTENSIONI
002700000000     L*------------------------------------------------------------------------*
002800000000     D*------------------------------------------------------------------------*
002900000000     D* SCHIERE
003000000000     D*------------------------------------------------------------------------*
003100000321     d*-------------------
003200000321     d* COMANDI CLP
003300000321     d*-------------------
003400110817     D CMD1            S             55    DIM(2) CTDATA PERRCD(1)
003500000000     D*-------------------
003600000000     D* RIGHE DI TESTATA
003700000000     D*-------------------
003800010626     D TES             S             66    DIM(8) CTDATA PERRCD(1)
003900010626     D*-------------------
004000010626     D* RIGHE DI TESTATA X QSYSPRT
004100010626     D*-------------------
004200010626     D TE1             S             66    DIM(5) CTDATA PERRCD(1)
004300000000     D*-------------------
004400000000     D* CLIENTI CON DISCHETTO
004500000000     D*-------------------
004600140304     D*TIB             S              5    DIM(100)                             TIPO INVIO BOL
004700140304     D*NIB             S              5  0 DIM(100)                             NUM.X TIPO INV
004800111205     d c_MaxCli3C      c                   const(30000)
004900100928     D CLI             S              7  0 DIM(c_MaxCli3C)                      *CLIENTI
005000100928     D CUN             S              7  0 DIM(c_MaxCli3C)                      *CLIENTI UNIF.
005100140225     D Cks             S              7  0 DIM(c_MaxCli3C)                      *CLIENTI UNIF.
005200140303     D leg             S              1    DIM(c_MaxCli3C)
005300140310     D pfi             S              1    DIM(c_MaxCli3C)                      cli con cod in + fil
005400100928     D CBA             S              5    DIM(c_MaxCli3C)                      *SUPPORTO CL.BARTOLI
005500100928     D DET             S              1    DIM(c_MaxCli3C)                      *DETT.SEGNACOLLI S/N
005600100928     D CAP             S              1    DIM(c_MaxCli3C)                      *TIPO CAPPARIO   S/N
005700100928     D PES             S              1    DIM(c_MaxCli3C)                      *GEST.PESO
005800100928     D BAC             S              5    DIM(c_MaxCli3C)                      *SUPPORTO CL.BARTOLI
005900000000     D*-------------------
006000000000     D* CODICI TRATTAMENTO MERCE
006100000000     D*-------------------
006200000000     D CTM             S              2    DIM(100)                             *COD.TRATTAMENTO MER
006300000710     D FG7             S              1    DIM(100)                             *DETT.SEGNACOLLI S/N
006400010418     D TMC             S              2    DIM(100)                             *DETT.SEGNACOLLI S/N
006500010418     D TMT             S              1    DIM(100)                             *DETT.SEGNACOLLI S/N
006600000710     D*-------------------
006700000710     D* SCHIERA FILIALI
006800000710     D*-------------------
006900000710     D FIL             S              3  0 DIM(500)                             *CODICE FILIALE
007000000710     D DFIL            S             20    DIM(500)                             *DESCRIZIONE FILIALE
007100140317     D car             S              3  0 DIM(500)                             *DESCRIZIONE FILIALE
007200000000     D*------------------------------------------------------------------------*
007300000000     D* INPUT
007400000000     D*------------------------------------------------------------------------*
007500000000     D*---------------------
007600000000     D* ARCHITETTURA
007700000000     D*---------------------
007800980131     D KPJBA         E DS
007900980131     D  WNSIF                 98    100
008000000000     D*---------------------
008100000000     D* DS PARAMETRI RICEVUTI
008200000000     D*------------------
008300000000     D PARAM           DS                  INZ
008400000000     D  PARDTI                 1      8  0
008500000000     D  PARDTF                 9     16  0
008600000000     D  PARNBO                17     19  0
008700970919     D  PARNCO                20     22  0
008800970919     D  PARPAR                23     23
008900970919     D  PARSTE                24     24
009000000316     D  PARTRK                25     25
009100010626     D  PARCOM                27     27
009200140520
009300140520     d dtasflo       e ds
009400140520
009500000000     D*---------------------
009600000000     D* DS3C - CLIENTI CON DISCHETTO
009700000000     D*---------------------
009800000000     D DS3C          E DS
009900970130     D*---------------------
010000970130     D* DS3K - CLIENTI CON VAS DA FILIALE
010100970130     D*---------------------
010200970130     D DS3K          E DS
010300970130     D*---------------------
010400970130     D* DS4C - CLIENTI CON VAS DA SEDE
010500970130     D*---------------------
010600970130     D DS4C          E DS
010700140411
010800140411     D DUNR          E DS
010900140411
011000000000     D*---------------------
011100000000     D* DS1B - CODICI TRATTAMENTO MERCE
011200000000     D*---------------------
011300000000     D DS1B          E DS
011400971128     D*---------------------
011500971128     D* DS REPEIMENTO TERMINALs
011600971128     D*---------------------
011700971128     D DSLV55        E DS                  EXTNAME(FNLV55DS)
011800000710     D*-------------------
011900000710     D* LETTURA CAMPO ORGDE£ DI AZORG
012000000710     D*-------------------
012100000710     D OG143         E DS
012200980116     D*------------------
012300980116     D* DS REPERIMENTO DATI UTENTE
012400980116     D*-------------------
012500980406     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
012600980406     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
012700980406     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
012800980406     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
012900980406     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
013000140221
013100140411      // - Ricerca/Controllo tabelle
013200140411     d TIBS02ds      e ds                  inz
013300140221     d tibs10ds      e ds
013400140221     d skc                           11    DIM(500) overlay(d10skc)
013500971128     D*---------------------
013600971128     D* DS "XSRDA8" - CONTROLLA DATA (8)
013700971128     D*---------------------
013800971128     D WLBDA8          DS                  INZ
013900971128     D  G08DAT                 1      8  0
014000971128     D  G08INV                 9     16  0
014100971128     D  G08ERR                17     17
014200971128     D  G08TGI                18     22  0
014300000000     D*---------------------
014400000000     D* SUDDIVISIONE DATA
014500000000     D*---------------------
014600000000     D                 DS
014700000000     D  DSSEC                  1      2  0
014800000000     D  DSANN                  3      4  0
014900000000     D  DSAAS                  1      4  0
015000000000     D  DSMES                  5      6  0
015100000000     D  DSGIO                  7      8  0
015200000000     D  DSMGS                  5      8  0
015300000000     D  DSDAT6                 3      8  0
015400000000     D  DSDAT8                 1      8  0
015500000000     D*---------------------
015600000000     D* SUDDIVISIONE DATA
015700000000     D*---------------------
015800000000     D                 DS
015900000000     D  DTORA                  1      2  0
016000000000     D  DTMIN                  3      4  0
016100000000     D  DTSEC                  5      6  0
016200000000     D  DTGIO                  7      8  0
016300000000     D  DTMES                  9     10  0
016400000000     D  DTANN                 11     12  0
016500000000     D  DTTIME                 1     12  0
016600000000     D  DTORA6                 1      6  0
016700000000     D  DTDAT6                 7     12  0
016800140307
016900140307     d                 ds
017000140311     d  sfil                   1     33  0 dim(11)
017100140311     d  STPFI1                 1      3  0
017200140311     d  STPFI2                 4      6  0
017300140311     d  STPFI3                 7      9  0
017400140311     d  STPFI4                10     12  0
017500140311     d  STPFI5                13     15  0
017600140311     d  STPFI6                16     18  0
017700140311     d  STPFI7                19     21  0
017800140311     d  STPFI8                22     24  0
017900140311     d  STPFI9                25     27  0
018000140311     d  STPFI10               28     30  0
018100140311     d  STPFI11               31     33  0
018200140310     d                 ds
018300140311     d  svfil                  1     33  0 dim(11)
018400140311     d  STvFI1                 1      3  0
018500140311     d  STvFI2                 4      6  0
018600140311     d  STvFI3                 7      9  0
018700140311     d  STvFI4                10     12  0
018800140311     d  STvFI5                13     15  0
018900140311     d  STvFI6                16     18  0
019000140311     d  STvFI7                19     21  0
019100140311     d  STvFI8                22     24  0
019200140311     d  STvFI9                25     27  0
019300140312     d  STvFI10               28     30  0
019400140311     d  STvFI11               31     33  0
019500140311     d                 ds
019600140311     d  CmK                    1     70  0 dim(10)
019700140311     d  STvks1                 1      7  0
019800140311     d  STvks2                 8     14  0
019900140311     d  STvks3                15     21  0
020000140311     d  STvks4                22     28  0
020100140311     d  STvks5                29     35  0
020200140311     d  STvks6                36     42  0
020300140311     d  STvks7                43     49  0
020400140311     d  STvks8                50     56  0
020500140311     d  STvks9                57     63  0
020600140311     d  STvks10               64     70  0
020700140311     d                 ds
020800140415     d  CmDt                   1     80  0 dim(10)
020900140415     d  STvdt1                 1      8  0
021000140415     d  STvdt2                 9     16  0
021100140415     d  STvdt3                17     24  0
021200140415     d  STvdt4                25     32  0
021300140415     d  STvdt5                33     40  0
021400140415     d  STvdt6                41     48  0
021500140415     d  STvdt7                49     56  0
021600140415     d  STvdt8                57     64  0
021700140415     d  STvdt9                65     72  0
021800140415     d  STvdt10               73     80  0
021900140415     d                 ds
022000140415     d  CmQt                   1     30  0 dim(10)
022100140415     d  STvQt1                 1      3  0
022200140415     d  STvQt2                 4      6  0
022300140415     d  STvQt3                 7      9  0
022400140415     d  STvQt4                10     12  0
022500140415     d  STvQt5                13     15  0
022600140415     d  STvQt6                16     18  0
022700140415     d  STvQt7                19     21  0
022800140415     d  STvQt8                22     24  0
022900140415     d  STvQt9                25     27  0
023000140415     d  STvQt10               28     30  0
023100140415     d                 ds
023200140415     d  Cmds                   1    350    dim(10)
023300140415     d  STvds1                 1     35
023400140415     d  STvds2                36     70
023500140415     d  STvds3                71    105
023600140415     d  STvds4               106    140
023700140415     d  STvds5               141    175
023800140415     d  STvds6               176    210
023900140415     d  STvds7               211    245
024000140415     d  STvds8               246    280
024100140415     d  STvds9               281    315
024200140415     d  STvds10              316    350
024300140307
024400140221      * - Campi di comodo
024500140221     d w_bsksc         s                   like(d10cod)
024600140225     d wlegame         s              1
024700140227     d wTipVAS         s                   like(stptrk)
024800140227     d wpes            s                   like(stppes)
024900140228     d wcass           s                   like(§4ctsd)
025000140228     d wgia            s                   like(§3KGSD)
025100140228     d wdisp           s                   like(§3KGSr)
025200140228     d wfatt           s                   like(§4CFSD)
025300140228     d wpod            s                   like(§3KCSD)
025400140303     d savleg          s                   like(stpleg)
025500140310     d savpfi          s                   like(stppfi)
025600140303     d stacass         s                   like(stpcass)
025700140303     d stagia          s                   like(stpgia)
025800140303     d stadisp         s                   like(stpdisp)
025900140303     d stafatt         s                   like(stpfatt)
026000140303     d stapod          s                   like(stppod)
026100140303     d w_kscvir        s                   like(virksc)
026200140303     d sav_i           s                   like(i)
026300140310     d is              s              6  0
026400140317     d in              s              3  0
026500140317     d ind             s              4  0
026600140310     d wpfi            s                   like(stppfi)
026700140313     d wfcli           s                   like(orgfil)
026800140313     d wcarcli         s                   like(orgcar)
026900140313     d wcarcli_sav     s                   like(orgcar)
027000140311     d stafilp         s             45
027100140311     d savfilp         s                   like(stafilp)
027200140408     d sav_cmdksc      s                   like(cmdksc)
027300140612     d staalr          s              6  0
027400140612     d wstaalr         s                   like(stvalr)
027500140415     d wdes            s             35
027600140415     d wcomod          s             39
027700140411     d wksccomod       s                   like(depksc)
027800140411     d Datadmy         s               d   datfmt(*dmy)
027900140411     D wgmy            s              6  0
028000140411     D w004a           s              4
028100140411     D ap              s              6  0                                      indice
028200140415     D wf              s              1                                         indice
028300140221
028400140221      // ? PROTOTIPI ?
028500140221      /copy gaitrasrc/srcprotopr,tibs10r
028600140411      /copy gaitrasrc/srcprotopr,tibs02r
028700000000     C*------------------------------------------------------------------------*
028800000000     C* MAIN LINE
028900000000     C*------------------------------------------------------------------------*
029000000000     C*
029100000000     C* SE NON DEVE ESSERE ESEGITO, CHIUDE IL PGM
029200000000DO  1C     $FINE         IFEQ      '0'
029300000000     C*
029400970130     C* SE SOLO STAMPA IMPOSTO FINE
029500970130     C     PARSTE        IFEQ      'S'
029600970130     C                   MOVEL     '1'           $FINE
029700970130     C                   ENDIF
029800000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
029900000000     C                   EXSR      SETFIL
030000000000     C*
030100000000     C* CICLO FINO A FINE FILE
030200000000DO  2C     $FINE         DOWEQ     '0'
030300000000     C*
030400000000     C* AGGIORNA IL FILE DI LAVORO
030500000322     C                   EXSR      AGGSTP
030600000000     C*
030700000000     C* LEGGE IL RECORD SUCCESSIVO
030800000000     C                   EXSR      LETFIL
030900000000DO  2C                   ENDDO
031000000000     C*
031100970912     C* GENERA NEL FILE UN RECORD PER OGNI CLIENTE
031200970912     C* PRESENTE NELLA TABELLA 3R E NON GIA' PRESENTE NEL FILE
031300140221     C                   EXSR      ADD3R
031400000316     C* GENERA NEL FILE UN RECORD PER OGNI CLIENTE
031500000316     C* PRESENTE NEL FILE DEI CLIENTI ABILITATI AL TRACK & TRACING
031600110401     C****               EXSR      ADDTRK
031700110624     C* AGGIORNA IL WRK-FILE CON L'INDICAZIONE DI VAS VB/VT PERSONALIZZATO/STANDARD/?
031800140227     C******             EXSR      UPDVAS
031900000000     C* LEGGE IL FILE APPENA GENERATO E STAMPA
032000000000     C                   EXSR      STAMPA
032100000000E   1C                   ENDIF                                                  <-- 1 ---
032200970131     C* STAMPA TOTALI ULTIMA FILIALE
032300990204     C     *IN88         IFEQ      '1'
032400010626     C     *IN14         ANDEQ     *OFF
032500970131     C                   EXSR      PERC                                         PERCENTUALI
032600010627     C     *IN14         IFEQ      *OFF
032700970131     C                   EXCEPT    TFIL
032800990204     C                   ENDIF
032900010627     C                   ENDIF
033000000000     C* STAMPA RIGA FINALE
033100970131     C                   Z-ADD     STFSPA        STTSPA
033200970131     C                   Z-ADD     STFSPB        STTSPB
033300010423     C                   Z-ADD     STFSPK        STTSPK
033400970131     C                   Z-ADD     STFSPT        STTSPT
033500970131     C                   Z-ADD     STFCOB        STTCOB
033600970131     C                   Z-ADD     STFCOT        STTCOT
033700990204     C                   Z-ADD     S2FSPA        S2TSPA
033800010423     C                   Z-ADD     S2FSPK        S2TSPK
033900990204     C                   Z-ADD     S2FSPB        S2TSPB
034000990204     C                   Z-ADD     S2FSPT        S2TSPT
034100990204     C                   Z-ADD     S2FCOB        S2TCOB
034200990204     C                   Z-ADD     S2FCOT        S2TCOT
034300970131     C                   EXSR      PERC                                         PERCENTUALI
034400010626     C  N14              EXCEPT    TESTA
034500010626     C  N14              EXCEPT    STAFIN
034600140304     C*                  DO        100           I
034700140304     C*    TIB(I)        IFGT      *BLANK
034800140304     C* N14              EXCEPT    STAFI2
034900140304     C*                  ENDIF
035000140304     C*                  ENDDO
035100971128     C*
035200971128     C* OPERAZIONI FINALI
035300971128     C                   EXSR      FINSR
035400971128     C*
035500971128     C                   SETON                                        LR
035600971128     C*------------------------------------------------------------------------*
035700971128     C* FINSR - OPERAZIONI FINALI
035800971128     C*------------------------------------------------------------------------*
035900971128     C     FINSR         BEGSR
036000971128     C*
036100971128     C* CHIUDE I FILES
036200971128     C                   CLOSE     WFSTP01L
036300971128     C                   CLOSE     WFSTP02L
036400971128     C*
036500971128     C* CHIUDE I PGM
036600971128     C                   CLEAR                   DSLV55
036700971128     C                   MOVEL     'C'           D55TLA
036800971128     C                   CALL      'FNLV55R'
036900971128     C                   PARM                    DSLV55
037000980116     C*
037100980116     C                   CLEAR                   BS69DS
037200980116     C                   CLEAR                   ACODS
037300980116     C                   CLEAR                   INDDS
037400980116     C                   CLEAR                   CLPDS
037500980116     C                   CLEAR                   CLSDS
037600980116     C                   MOVEL     'C'           I69TLA                         *TIPO LANCIO
037700980116     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
037800980116     C                   CALL      'TIBS69R'
037900980116     C                   PARM                    BS69DS
038000980116     C                   PARM                    ACODS
038100980116     C                   PARM                    INDDS
038200980116     C                   PARM                    CLPDS
038300980116     C                   PARM                    CLSDS
038400971128     C*
038500971128     C                   ENDSR
038600000000     C*------------------------------------------------------------------------*
038700000000     C* SETFIL - POSIZIONAMENTO SUL FILE E PRIMA LETTURA
038800000000     C*------------------------------------------------------------------------*
038900000000     C     SETFIL        BEGSR
039000000000     C*
039100000000     C* POSIZIONAMENTO E PRIMA LETTURA
039200000316     C                   MOVEL     PARDTI        TASAAS
039300000316     C                   MOVE      PARDTI        TASMGS
039400990810     C     KEYTAS        SETLL     TITAS38C                           99        *FINE ARCHIVIO
039500000000     C*
039600000000IF  1C     *IN99         IFEQ      *ON
039700000000     C                   MOVEL     '1'           $FINE
039800000000X   1C                   ELSE
039900000000     C                   EXSR      LETFIL
040000000000E   1C                   ENDIF
040100000000     C*
040200000000     C                   ENDSR
040300000000     C*------------------------------------------------------------------------*
040400000000     C* LETFIL - LETTURA PROSSIMO RECORD
040500000000     C*------------------------------------------------------------------------*
040600000000     C     LETFIL        BEGSR
040700000000     C*
040800000000     C* LEGGE FINO A:
040900000000     C*     - FINE FILE O SUPERATO LIMITE FINALE
041000000000     C*     - TROVATO RECORD VALIDO
041100000000     C                   MOVEL     'N'           WRECOK
041200000000DO  1C     $FINE         DOWEQ     '0'
041300000000     C     WRECOK        ANDEQ     'N'
041400000000     C*
041500990810     C                   READ      TITAS38C                               99
041600980209     C                   MOVEL     TASAAS        WDT8              8 0
041700980209     C                   MOVE      TASMGS        WDT8
041800000000     C*
041900000000IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
042000980209     C     WDT8          ORGT      PARDTF                                       *SUPERATA DATA FINE
042100000000     C                   MOVEL     '1'           $FINE
042200000000X   2C                   ELSE
042300000000     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
042400000000E   2C                   ENDIF
042500000000E   1C                   ENDDO
042600000000     C*
042700000000IF  1C     $FINE         IFEQ      '0'                                          *RECORD VALIDO
042800000000     C                   ADD       1             CONLET                         *CONTEGGIO
042900000000E   1C                   ENDIF
043000000000     C*
043100000000     C                   ENDSR
043200000000     C*------------------------------------------------------------------------*
043300000000     C* CHKREC - CONTROLLA VALIDITA' RECORD
043400000000     C*------------------------------------------------------------------------*
043500000000     C     CHKREC        BEGSR
043600000000     C*
043700000000     C                   MOVEL     'S'           WRECOK                         *RECORD VALIDO
043800000000     C*
043900000000     C* CONTROLLA IL TIPO BOLLA
044000000000IF  1C     TASTBL        IFEQ      'AP'
044100000000     C     TASTBL        OREQ      'AS'
044200000000     C     TASTBL        OREQ      'A3'
044300000000     C     TASTBL        OREQ      'FS'
044400000000     C     TASTBL        OREQ      'F3'
044500000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
044600000000E   1C                   ENDIF
044700990810     C*
044800990810     C* MITTENTE NON CODIFICATO, ESCLUDE
044900990810     C                   MOVE      TASCCM        WCLI4             4
045000990810IF  1C     WCLI4         IFEQ      *ZEROS
045100990810     C     WCLI4         OREQ      '8888'
045200990810     C     WCLI4         OREQ      '9999'
045300990810     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
045400990810X   1C                   ELSE
045500990810     C                   Z-ADD     TASCCM        WCLI              7 0
045600990810E   1C                   ENDIF
045700000000     C*
045800000000     C                   ENDSR
045900000000     C*------------------------------------------------------------------------*
046000140520     C* AGGSTP - AGGIORNA IL FILE WFSTP01L
046100000000     C*------------------------------------------------------------------------*
046200000000     C     AGGSTP        BEGSR
046300010418     c*
046400140228     c                   eval      wpes=' '
046500140228     c                   eval      wcass='  '
046600140228     c                   eval      wgia='  '
046700140228     c                   eval      wdisp='  '
046800140228     c                   eval      wfatt='  '
046900140228     c                   eval      wpod='  '
047000010418     c*
047100970129     C* RECUPERA I DATI CLIENTE
047200970129     C                   Z-ADD     1             I
047300970129     C     WCLI          LOOKUP    CLI(I)                                 99
047400970129IF  3C     *IN99         IFEQ      *ON
047500970129     C                   Z-ADD     CUN(I)        STPKSC
047600140225     c                   eval      stpleg=leg(i)
047700140310     c                   eval      stppfi=pfi(i)
047800140303     c                   move      *zeros        w_kscvir
047900140303     c                   move      cks(i)        w_kscvir
048000140228     c                   eval      wpes=pes(i)
048100140228     c                   exsr      sr_3K4C
048200140228     c                   eval      wcass=§4ctsd
048300140228     c                   eval      wgia=§3KGSD
048400140228     c                   eval      wdisp=§3KGSr
048500140228     c                   eval      wfatt=§4CFSD
048600140228     c                   eval      wpod=§3KCSD
048700970129X   3C                   ELSE
048800140225     C****               Z-ADD     WCLI          STPKSC
048900140225     c                   eval      w_bsksc=wcli
049000140225     c                   exsr      sr_tibs10
049100140303     C                   MOVE      d10cop        stpksc                         *CLI. UNIF.
049200140225     c                   eval      stpleg=wlegame
049300140310     c                   exsr      sr_wpfi
049400140310     c                   eval      stppfi=wpfi
049500140303     c                   move      *zeros        w_kscvir
049600140303     c                   move      wcli          w_kscvir
049700970129E   3C                   ENDIF
049800140227     c* esecuzione routine per determinare se per il cliente in esame il vas è standard
049900140227     c* o personalizzato
050000140227     c                   exsr      sr_trk
050100010418     C*
050200010418     c* 36 ON = BOLLA DISK C
050300010418     c                   z-add     1             ii
050400010418     c                   clear                   wtmt
050500010418     c     tasctm        lookup    tmc(ii)                                36
050600010418     c   36              move      tmt(ii)       wtmt              1
050700000000     C*
050800970130     C                   MOVE      *ZERO         STPNUA                                        PEDIZ
050900970130     C                   MOVE      *ZERO         STPIMA
051000010418     C                   MOVE      *ZERO         STPNUC                                        PEDIZ
051100010418     C                   MOVE      *ZERO         STPIMC
051200010418     C                   MOVE      *ZERO         STPNUK                                        PEDIZ
051300010418     C                   MOVE      *ZERO         STPIMK
051400970130     C                   MOVE      *ZERO         STPNUB                                        PEDIZ
051500970130     C                   MOVE      *ZERO         STPIMB
051600970130     C                   MOVE      *ZERO         STPNUM                                        PEDIZ
051700970130     C                   MOVE      *ZERO         STPIMP
051800140228     C***                MOVE      *ZERO         STPNCA                                        PEDIZ
051900000316     C                   MOVE      *BLANKS       STPT3R
052000140311     c                   clear                   sfil
052100140520     c                   clear                   stpalr
052200140520
052300140520     c* reperisco eventuale presenza di alert
052400140520     c                   eval      dtasflo=tasflo
052500970130     C*
052600970129     C                   Z-ADD     TASAAS        DSAAS
052700970129     C                   Z-ADD     TASMGS        DSMGS
052800970129     C                   Z-ADD     DSDAT8        STPDAT
052900000000     C* CONTROLLA SE RECORD GIA' ESISTENTE
053000000000     C     KEYSTP        CHAIN     WFSTP01L                           99
053100000000     C*
053200000000IF  1C     *IN99         IFEQ      *OFF                                         E S I S T E
053300000000IF  2C     TASSOP        IFEQ      '§'
053400000000     C     TASSOP        OREQ      '%'
053500010423     C     TASSOP        OREQ      ':'
053600000000IF  3C     TASNRS        IFEQ      *ZEROS
053700010423     c* disk a o disk a+c bartolini --> memorizzo come disk A
053800010418     c     *in36         ifeq      *off
053900010423     c     wtmt          oreq      'B'
054000970129     C                   ADD       1             STPNUA                         *AGGIUNGE UNA SPEDIZ
054100970129     C                   ADD       TASNCL        STPIMA                         *COLLI
054200010418     c                   else
054300010423     c* disk c cliente
054400010418     C                   ADD       1             STPNUK                         *AGGIUNGE UNA SPEDIZ
054500010418     C                   ADD       TASNCL        STPIMK                         *COLLI
054600010418     c                   endif
054700000000X   3C                   ELSE
054800970129     C                   ADD       1             STPNUB                         *AGGIUNGE UNA SPEDIZ
054900970129     C                   ADD       TASNCL        STPIMB                         *COLLI
055000000000E   3C                   ENDIF
055100000000E   2C                   ENDIF
055200010423     c* disk c bartolini
055300010423     c     *in36         ifeq      *on
055400010423     c     wtmt          andeq     'B'
055500010423     C                   ADD       1             STPNUC                         *AGGIUNGE UNA SPEDIZ
055600010423     C                   ADD       TASNCL        STPIMC                         *COLLI
055700010423     c                   endif
055800000000     C                   ADD       1             STPNUM                         *AGGIUNGE UNA SPEDIZ
055900000000     C                   ADD       TASNCL        STPIMP                         *COLLI
056000140228IF  3C*    TASFCA        IFEQ      'S'
056100140228     C*                  ADD       1             STPNCA                         *CONTRASSEGNO  PEDIZ
056200140228E   2C*                  ENDIF
056300140227     c* Tipo VAS (elencati in ordine decrescente di priorità):
056400140227     c*  P=Personalizzato
056500140227     c*  E=EasySped/EasyWeb
056600140227     c*  S=Standard
056700140227     c*  ?=Non determinato
056800140227     c                   if        stptrk<>wTipVas and stptrk<>'P'
056900140227     c                   if        (stptrk='E' and wTipVas='P') or
057000140227     c                             (stptrk='?')                 or
057100140227     c                             (stptrk='S' and (wTipVas='P' or wTipVas='E'))
057200140227     c                   eval      stptrk=WtipVas
057300140227     c                   endif
057400140227     c                   endif
057500140227     c* Flag gestione peso
057600140227     c                   if        wpes=' '
057700140227     c                   eval      stppes=wpes
057800140227     c                   endif
057900140228     c                   if        stpcass=*blanks
058000140228     c                   eval      stpcass=wcass
058100140228     c                   endif
058200140228     c                   if        stpgia=*blanks
058300140228     c                   eval      stpgia=wgia
058400140228     c                   endif
058500140228     c                   if        stpdisp=*blanks
058600140228     c                   eval      stpdisp=wdisp
058700140228     c                   endif
058800140228     c                   if        stpfatt=*blanks
058900140228     c                   eval      stpfatt=wfatt
059000140228     c                   endif
059100140228     c                   if        stppod=*blanks
059200140228     c                   eval      stppod=wpod
059300140228     c                   endif
059400140311     c                   exsr      sr_memosfil
059500140520     c                   if        §floemd<>*blanks
059600140611     c                   eval      stpalr+=1
059700140520     c                   endif
059800000000     C                   UPDATE    WFSTP                                        *AGGIORNA
059900000000     C*
060000000000X   1C                   ELSE                                                   N O N   E S I S T E
060100000000     C*
060200970129IF  2C     TASSOP        IFEQ      '§'
060300970129     C     TASSOP        OREQ      '%'
060400010423     C     TASSOP        OREQ      ':'
060500970129IF  3C     TASNRS        IFEQ      *ZEROS
060600010418     c     *in36         ifeq      *off
060700010423     c     wtmt          oreq      'B'
060800970129     C                   Z-ADD     1             STPNUA                         *AGGIUNGE UNA SPEDIZ
060900970129     C                   Z-ADD     TASNCL        STPIMA                         *COLLI
061000010418     c                   else
061100010418     C                   Z-ADD     1             STPNUK                         *AGGIUNGE UNA SPEDIZ
061200010418     C                   Z-ADD     TASNCL        STPIMK                         *COLLI
061300010418     c                   endif
061400970129X   3C                   ELSE
061500970129     C                   Z-ADD     1             STPNUB                         *AGGIUNGE UNA SPEDIZ
061600970129     C                   Z-ADD     TASNCL        STPIMB                         *COLLI
061700970129E   3C                   ENDIF
061800970129E   2C                   ENDIF
061900010423     c* disk c bartolini
062000010423     c     *in36         ifeq      *on
062100010423     c     wtmt          andeq     'B'
062200010423     C                   Z-ADD     1             STPNUC                         *AGGIUNGE UNA SPEDIZ
062300010423     C                   Z-ADD     TASNCL        STPIMC                         *COLLI
062400010423     c                   endif
062500970129     C                   Z-ADD     1             STPNUM                         *AGGIUNGE UNA SPEDIZ
062600970129     C                   Z-ADD     TASNCL        STPIMP                         *COLLI
062700140228IF  3C*    TASFCA        IFEQ      'S'
062800140228     C*                  Z-ADD     1             STPNCA                         *CONTRASSEGNO  PEDIZ
062900140228E   2C*                  ENDIF
063000970130     C                   MOVEL     STPKSC        STPFIP
063100140317     C                   MOVEL     STPKSC        KORFIL
063200140317     C***  KORFIL        CHAIN     AZORG01L                           97
063300140317     c                   eval      ind=%lookup(korfil:fil)
063400140317     c                   clear                   stpas4
063500140317     c                   if        ind>0
063600140317     c                   eval      stpas4=car(ind)
063700140317     c                   endif
063800971128     C*
063900971128     C* REPERISCE IL TERMINAL
064000110325     C*                  CLEAR                   DSLV55
064100110325     C*                  MOVEL     'P'           D55TPT
064200110325     C*                  Z-ADD     ORGFIL        D55LIN
064300110325     C*                  Z-ADD     DATCOR        D55DRF
064400110325     C*                  CALL      'FNLV55R'
064500110325     C*                  PARM                    DSLV55
064600110325IF  2C*    D55ERR        IFNE      *BLANKS
064700110325     C*                  Z-ADD     ORGFIL        D55TFP
064800110325E   2C*                  ENDIF
064900110325     C*                  MOVEL     D55TFP        STPAS4
065000140317     C*                  MOVEL     orgcar        STPAS4
065100970129     C                   Z-ADD     STPKSC        DEPKSC
065200000000     C                   EXSR      DECCLI                                       *DESCRIZIONE CLIENTE
065300000000     C                   MOVEL     DESCLI        STPDCL
065400140227     c                   eval      stptrk=WtipVas
065500140227     c                   eval      stppes=Wpes
065600140228     c                   eval      STPCASS=wcass
065700140228     c                   eval      STPGIA=wgia
065800140228     c                   eval      STPDISP=wdisp
065900140228     c                   eval      STPFATT=wfatt
066000140228     c                   eval      STPPOD=wpod
066100140311     c                   exsr      sr_memosfil
066200140520     c                   if        §floemd<>*blanks
066300140611     c                   z-add     1             stpalr
066400140520     c                   endif
066500000000     C                   WRITE     WFSTP                                        *INSERISCE
066600000000E   1C                   ENDIF
066700000000     C*
066800000000     C                   ENDSR
066900970131     C*------------------------------------------------------------------------*
067000970131     C* CALCOLA PERCENTUALI
067100970131     C*------------------------------------------------------------------------*
067200970131     C     PERC          BEGSR
067300990204     C* SUL TOTALE
067400970131     C     STTSPA        ADD       STTSPB        WSPDSK            8 0
067500010423     c                   add       sttspk        wspdsk
067600970131     C     WSPDSK        IFGT      *ZERO
067700990204     C     WSPDSK        MULT      100           WW10             10 0
067800970131     C     WW10          DIV(H)    STTSPT        WPDSK             3 0          % DSK
067900970131     C                   ENDIF
068000970131     C     STTCOB        IFGT      *ZERO
068100990204     C     STTCOB        MULT      100           WW10
068200970131     C     WW10          DIV(H)    STTCOT        WPETI             3 0          % DISK B
068300970131     C                   ENDIF
068400990204     C* SOLO SUI "POTENZIALI"
068500990204     C     S2TSPA        ADD       S2TSPB        WSPDSK
068600010423     c                   add       s2tspk        wspdsk
068700990204     C     WSPDSK        IFGT      *ZERO
068800990204     C     WSPDSK        MULT      100           WW10
068900990204     C     WW10          DIV(H)    S2TSPT        W2DSK             3 0          % DSK
069000990204     C                   ENDIF
069100990204     C     S2TCOB        IFGT      *ZERO
069200990204     C     S2TCOB        MULT      100           WW10
069300990204     C     WW10          DIV(H)    S2TCOT        W2ETI             3 0          % DISK B
069400990204     C                   ENDIF
069500970131     C                   ENDSR
069600000000     C*------------------------------------------------------------------------*
069700110624     C* STAMPA - LEGGE IL FILE DI LAVORO E STAMPA
069800000000     C*------------------------------------------------------------------------*
069900000000     C     STAMPA        BEGSR
070000010626     C*
070100000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
070200000000     C                   EXSR      SETSTP
070300000000     C*
070400000000     C* CICLO FINO A FINE FILE DI LAVORO
070500000000DO  1C     $FINE         DOWEQ     '0'
070600000000     C*
070700970130     C* STAMPO TOTALI FILIALE PRECEDENTE
070800970130     C     DEPFIP        IFGT      *ZERO
070900990204     C     *IN88         ANDEQ     '1'                                          ALMENO UN CLI
071000010626     C     *IN14         ANDEQ     *OFF
071100970131     C                   EXSR      PERC                                         PERCENTUALI
071200970130     C                   EXCEPT    TFIL
071300970130     C                   ENDIF
071400970130     C* AZZERA I TOTALI FILIALE
071500990204     C                   SETOFF                                       88
071600971021     C                   MOVE      *ZERO         WBLCF
071700140303     C***                MOVE      *ZERO         WCAPVF
071800140303     C***                MOVE      *ZERO         WCAPNF
071900140303     C***                MOVE      *ZERO         WCAPLF
072000110401     C***                MOVE      *ZERO         WCATRF
072100970130     C                   MOVE      *ZERO         WSPDSK
072200970130     C                   MOVE      *ZERO         WPDSK
072300970130     C                   MOVE      *ZERO         WPETI
072400970130     C                   MOVE      *ZEROS        STTSPT
072500970130     C                   MOVE      *ZEROS        STTCOT
072600970130     C                   MOVE      *ZEROS        STTSPA
072700010419     C                   MOVE      *ZEROS        STTSPK
072800970130     C                   MOVE      *ZEROS        STTSPB
072900140228     C***                MOVE      *ZEROS        STTSPC
073000970130     C                   MOVE      *ZEROS        STTCOB
073100970130     C                   MOVE      *ZEROS        STTCLI
073200970130     C                   MOVE      *ZEROS        STTCLA
073300970130     C                   MOVE      *ZEROS        STTCLB
073400010423     C                   MOVE      *ZEROS        STTCLC
073500970130     C*
073600990204     C                   MOVE      *ZERO         W2DSK
073700990204     C                   MOVE      *ZERO         W2ETI
073800990204     C                   MOVE      *ZEROS        S2TSPT
073900990204     C                   MOVE      *ZEROS        S2TCOT
074000990204     C                   MOVE      *ZEROS        S2TSPA
074100010419     C                   MOVE      *ZEROS        S2TSPK
074200990204     C                   MOVE      *ZEROS        S2TSPB
074300140228     C***                MOVE      *ZEROS        S2TSPC
074400990204     C                   MOVE      *ZEROS        S2TCOB
074500990204     C                   MOVE      *ZEROS        S2TCLI
074600990204     C                   MOVE      *ZEROS        S2TCLA
074700990204     C                   MOVE      *ZEROS        S2TCLB
074800010423     C                   MOVE      *ZEROS        S2TCLC
074900970130     C* OPERAZIONI PER NUOVA FILIALE
075000970130     C                   Z-ADD     STPFIP        DEPFIP            3 0
075100970130     C* CICLO FINO A ROTTURA DI FILIALE
075200000000DO  2C     $FINE         DOWEQ     '0'
075300970130     C     STPFIP        ANDEQ     DEPFIP
075400000000     C*
075500000000     C* OPERAZIONI PER NUOVO CLIENTE
075600000000     C                   Z-ADD     STPKSC        DEPKSC            7 0
075700140228     C                   MOVEL     STPleg        savleg
075800140310     C                   MOVEL     STPpfi        savpfi
075900110401     C***                MOVEL     *BLANKS       STATRK            1
076000000517     C                   MOVEL     *BLANKS       STAT3R            1
076100140520     C                   clear                   STAalr
076200140612     C                   clear                   wSTAalr
076300140310     c                   clear                   svfil
076301140801     c                   clear                   sTAVAB
076400000000     C*
076500000000     C* CICLO FINO A ROTTURA DI CLIENTE
076600000000DO  3C     $FINE         DOWEQ     '0'
076700970130     C     STPFIP        ANDEQ     DEPFIP
076800000000     C     STPKSC        ANDEQ     DEPKSC
076900000000     C*
077000000000     C* MEMORIZZA LE SPEDIZIONI
077100000000     C                   ADD       1             STAGIO
077200000000     C                   ADD       STPNUM        STASPT
077300000000     C                   ADD       STPIMP        STACOT
077400970129     C                   ADD       STPNUA        STASPA
077500970129     C                   ADD       STPNUB        STASPB
077600110817     C****               ADD       STPNUC        STASPH
077700010418     C                   ADD       STPNUK        STASPK
077800140228     C****               ADD       STPNCA        STASPC
077900970130     C                   ADD       STPIMB        STACOB
078000110624     C***   TPTRK        IFNE      *BLANKS
078100110624     C***                MOVEL     STPTRK        STATRK
078200110401     C***                ENDIF
078300000517     C     STPT3R        IFNE      *BLANKS
078400000517     C                   MOVEL     STPT3R        STAT3R
078500000517     C                   ENDIF
078600140611     C**   STPalr        IFNE      *BLANKS
078700140611     C**                 MOVEL     STPalr        STAalr
078800140611     C**                 ENDIF
078900140612     c                   eval      wstaalr+=stpalr
079000140612     c                   if        wstaalr>999999
079100140612     c                   z-add     999999        staalr
079200140612     c                   else
079300140612     c                   z-add     wstaalr       staalr
079400140612     c                   endif
079500000322     C     WINDSK        IFEQ      *ZERO
079600000517     C     STPNUA        IFGT      *ZERO
079700000517     C     STPNUB        ORGT      *ZERO
079800010418     C     STPNUK        ORGT      *ZERO
079900970203     C                   MOVE      STPDAT        WINDSK
080000000517     C                   ENDIF
080100970203     C                   ENDIF
080101140801     C                   IF        STAVAB=*BLANKS
080200110624     C                   MOVEL     STPTRK        STAVAB            1
080201140801     C                   ELSE
080202140801     c* Tipo VAS (elencati in ordine decrescente di priorità):
080203140801     c*  P=Personalizzato
080204140801     c*  E=EasySped/EasyWeb
080205140801     c*  S=Standard
080206140801     c*  ?=Non determinato
080207140801     c                   if        STAVAB<>STPTRK  and stavab<>'P'
080208140801     c                   if        (stavab='E' and stptrk='P') or
080209140801     c                             (stavab='?')                 or
080210140801     c                             (stavab='S' and (stptrk='P' or stptrk='E'))
080211140801     c                   eval      stavab=stptrk
080212140801     c                   endif
080213140801     c                   endif
080214140801     C                   ENDIF
080300140228     C                   MOVEL     STPpes        STApes            1
080400140228     c                   eval      stacass=stpcass
080500140228     c                   eval      stagia=stpgia
080600140228     c                   eval      stadisp=stpdisp
080700140228     c                   eval      stafatt=stpfatt
080800140228     c                   eval      stapod=stppod
080900140310     c* schiera delle filiali di partenza
081000140310     c                   z-add     1             i
081100140311     c                   dow       i<=%elem(sfil) and sfil(i)>0
081200140310     c                   eval      in=%lookup(sfil(i):svfil)
081300140310     c                   if        in=0
081400140310     c                   eval      in=%lookup(000:svfil)
081500140310     c                   if        in>0
081600140310     c                   if        in=11
081700140310     c                   z-add     999           svfil(11)
081800140310     c                   else
081900140310     c                   z-add     sfil(i)       svfil(in)
082000140310     c                   endif
082100140310     c                   endif
082200140310     c                   endif
082300140310     c                   add       1             i
082400140310     c                   enddo
082500000000     C*
082600000000     C* LEGGE IL RECORD SUCCESSIVO
082700000000     C                   EXSR      LETSTP
082800000000E   3C                   ENDDO
082900000000     C*
083000000000     C* SE FINE PAGINA STAMPA TESTATA SU FOGLIO SUCCESSIVO
083100000000IF  3C     *INOF         IFEQ      *ON
083200010626     C  N14              EXCEPT    TESTA
083300010626     C   14              EXCEPT    TESTAT
083400000000E   3C                   ENDIF
083500000000     C*
083600000000     C* CALCOLA LE SPEDIZIONI AL GIORNO
083700970129     C     STASPT        DIV       STAGIO        STASPG            8 0
083800970129     C     STACOT        DIV       STAGIO        STACOG            8 0
083900000000     C*
084000000000     C* RECUPERA I DATI CLIENTE
084100140227     C*                  Z-ADD     1             I                 6 0
084200140227     C*    DEPKSC        LOOKUP    CLI(I)                                 99
084300140227IF  3C*    *IN99         IFEQ      *OFF
084400140227     C*                  MOVEL     *BLANKS       STADET            1            *DETT.SEGN.
084500140227     C*                  MOVE      *BLANKS       STACAP            1            *TIPO CAPPARIO
084600140227     C*                  MOVEL     *BLANKS       STACBA            5            *SUPP. CLI > BART
084700140227     C*                  MOVEL     *BLANKS       STAPES            1            GEST.PESO
084800140227     C*                  MOVE      *BLANK        STACAP
084900140227     C*                  MOVEL     *BLANKS       STABAC            5            *SUPP. CLI > BART
085000140227X   3C*                  ELSE
085100140227     C*                  MOVEL     DET(I)        STADET                         *DETT.SEGN.
085200140227     C*                  MOVEL     CBA(I)        STACBA                         *SUPP. CLI > BART
085300140227     C*                  MOVEL     PES(I)        STAPES                         GEST.PESO
085400140227     C*                  MOVEL     BAC(I)        STABAC                         *SUPP. CLI > BART
085500140227     C*                  MOVEL     '1'           STACAP                         AZIST
085600140227     C*    CAP(I)        IFEQ      'S'
085700140227     C*                  MOVEL     '2'           STACAP                         AZCAP
085800140227     C*                  ENDIF
085900140227     C*    CAP(I)        IFEQ      'L'
086000140227     C*                  MOVEL     '3'           STACAP                         AZCPC
086100140227     C*                  ENDIF
086200140227E   3C*                  ENDIF
086300000000     C*
086400970130     C* VAS DA FILIALE (DATI CONS./GIACENZE)
086500970130     C*
086600140228     C*                  MOVE      *BLANK        DS3K
086700140228     C*                  MOVEL     '3K'          KTBCOD
086800140228     C*                  MOVEL     DEPKSC        KTBKEY
086900140228     C*    KEYTAC        CHAIN     TABEL00F                           99
087000140228     C* N99              MOVEL     TBLUNI        DS3K
087100970130     C*
087200970130     C* VAS DA SEDE (C/A / FATTURE)
087300970130     C*
087400140228     C*                  MOVE      *BLANK        DS4C
087500140228     C*                  MOVEL     '4C'          KTBCOD
087600140228     C*                  MOVEL     DEPKSC        KTBKEY
087700140228     C*    KEYTAC        CHAIN     TABEL00F                           99
087800140228     C* N99              MOVEL     TBLUNI        DS4C
087900970130     C*
088000970130     C* COMODATI
088100970130     C*
088200140228     C*                  MOVE      *BLANK        DS3R
088300140228     C*                  MOVEL     '3R'          KTBCOD
088400140228     C*                  MOVEL     DEPKSC        KTBKEY
088500140228     C*    KEYTAC        CHAIN     TABEL00F                           99
088600140228     C* N99              MOVEL     TBLUNI        DS3R
088700970130     C*
088800970131     C                   MOVE      'S'           WSCART            1
088900990204     C                   MOVE      'S'           WSCAR2            1
089000000517     C* SOLO CLIENTI CON VAS BOLLE  SOLUZ.B
089100000322     C     PARPAR        IFEQ      'B'                                          SOLO VAS BOLLEB
089200980606     C     STASPB        ANDEQ     *ZERO
089300980606     C                   MOVEL     'N'           WSCART
089400980606E   1C                   ENDIF
089500970131     C* SOLO CLIENTI CON VAS BOLLE
089600970131     C     PARPAR        IFEQ      'V'                                          SOLO VAS
089700970131     C     STASPA        ANDEQ     *ZERO
089800970131     C     STASPB        ANDEQ     *ZERO
089900010423     C     STASPK        ANDEQ     *ZERO
090000970131     C                   MOVEL     'N'           WSCART
090100970131E   1C                   ENDIF
090200970131     C* SOLO CLIENTI SENZA VAS BOLLE
090300970131     C     PARPAR        IFEQ      'N'                                          SOLO NO VAS
090400970131     C     STASPA        IFGT      *ZERO
090500970131     C     STASPB        ORGT      *ZERO
090600010423     C     STASPK        ORGT      *ZERO
090700970131     C                   MOVEL     'N'           WSCART
090800970131     C                   ENDIF
090900970131     C                   ENDIF
091000970131     C*
091100970919     C* SE NUMERO BOLLE/COLLI MEDIO INF. AL RICHIESTO, ESCLUDE
091200970131IF  1C     STASPG        IFLE      PARNBO
091300970919IF  1C     STACOG        ANDLE     PARNCO
091400970131     C     STASPA        ANDEQ     *ZERO
091500970131     C     STASPB        ANDEQ     *ZERO
091600010423     C     STASPK        ANDEQ     *ZERO
091700970131     C                   MOVEL     'N'           WSCART
091800990204     C                   MOVEL     'N'           WSCAR2
091900970131E   1C                   ENDIF
092000000322     C*
092100000322     C* SE LANCIATO "ANCHE SE SOLO T&T" ED IL RECOTD E' DI T&T
092200000322     C* By-Passo COMUNQUE I CONTROLLI PRECEDENTI
092300110401     C***  PARTRK        IFEQ      'S'
092400110401     C***  STATRK        ANDEQ     'X'
092500110401     C***                MOVEL     'S'           WSCART
092600110401     C***                MOVEL     'S'           WSCAR2
092700110401     C***                ENDIF
092800000517     C*
092900000517     C* SE IL CLIENTE E' DI QUELLI "3R" ==>
093000000517     C* By-Passo COMUNQUE I CONTROLLI PRECEDENTI
093100000517     C     STAT3R        IFNE      *BLANKS
093200000517     C                   MOVEL     'S'           WSCART
093300000517     C                   MOVEL     'S'           WSCAR2
093400000517     C                   ENDIF
093500000419     C*
093600000000     C* STAMPA IL TOTALE CLIENTE
093700140228     C*    STASPB        IFLE      *ZERO
093800140228     C*                  MOVEL     *BLANKS       STACAP            1                        IO
093900140228     C*                  MOVEL     *BLANKS       STABAC                                     IO
094000140228     C*                  MOVEL     *BLANKS       STADET            1                        IO
094100140228     C*                  ENDIF
094200971021     C     STASPB        IFLE      *ZERO
094300971021     C     STASPA        ANDLE     *ZERO
094400010423     C     STASPK        ANDLE     *ZERO
094500971021     C                   MOVEL     *BLANKS       STAVAB                                     IO
094600140228     C******             MOVEL     *BLANKS       STAcba                                     IO
094700971021     C                   ENDIF
094800970131     C                   EXSR      DECCLI
094900970203     C*
095000970203     C* STAMPO IL MESE DI INZIO DSK SE SUPERIORE AL DAL
095100970203     C                   MOVE      *BLANK        WINDSD            2
095200970203     C                   MOVEL     WINDSK        WW6DSK            6 0
095300970203     C                   MOVEL     PARDTI        WW6DTI            6 0
095400970203     C     WW6DSK        IFGT      WW6DTI
095500970203     C                   MOVE      WW6DSK        WINDSD
095600970203     C                   ENDIF
095700970205     C* TOTALIZZO IL NUMERO DI CLIENTI X TIPO SUPPORTO
095800140228     C*    STACBA        IFGT      *BLANKS                                                   BART
095900140228     C*                  MOVEL     STACBA        WAA2              2
096000140228     C*                  MOVEL     STACBA        WSTCBA            5
096100140228     C*    WAA2          IFEQ      'HL'                                         HALLEY  XX
096200140228     C*    WAA2          OREQ      'PC'                                         PC+HALL XX
096300140228     C*    WAA2          OREQ      'WW'                                         PC+HALL XX
096400140228     C*                  MOVE      '  '          WSTCBA
096500140228     C*                  ENDIF
096600140228     C*                  Z-ADD     1             I
096700140228     C*    WSTCBA        LOOKUP    TIB(I)                                 99
096800140228     C*    *IN99         IFEQ      *OFF
096900140228     C*                  Z-ADD     1             I
097000140228     C*    *BLANK        LOOKUP    TIB(I)                                 99
097100140228     C*                  ENDIF
097200140228     C*                  ADD       1             NIB(I)
097300140228     C*                  MOVEL     WSTCBA        TIB(I)                                     IO
097400140228     C*                  ENDIF
097500971021     C* TOTALIZZO IL NUMERO DI CLIENTI
097600110624     C* CON VAS VAB/VAT STANDARD
097700110624     C     STAVAB        IFEQ      'S'                                                       BART
097800140228     C     STAVAB        oreq      'E'                                                       BART
097900971021     C                   ADD       1             WBLCF             5 0
098000971021     C                   ADD       1             WBLCT             5 0
098100971021     C                   ENDIF
098200971021     C* CON CAPPARIO:                                                   BART
098300140228     C*    STASPB        IFGT      *ZERO
098400971021     C* CON CAPPARIO AZIST
098500140228     C*    STACAP        IFEQ      '1'                                                       BART
098600140228     C*                  ADD       1             WCAPVF            5 0
098700140228     C*                  ADD       1             WCAPVT            5 0
098800140228     C*                  ENDIF
098900971021     C* CON CAPPARIO AZCAP
099000140228     C*    STACAP        IFEQ      '2'                                                       BART
099100140228     C*                  ADD       1             WCAPNF            5 0
099200140228     C*                  ADD       1             WCAPNT            5 0
099300140228     C*                  ENDIF
099400971021     C* CON CAPPARIO AZCPC
099500140228     C*    STACAP        IFEQ      '3'                                                       BART
099600140228     C*                  ADD       1             WCAPLF            5 0
099700140228     C*                  ADD       1             WCAPLT            5 0
099800140228     C*                  ENDIF
099900140228     C*                  ENDIF
100000000419     C* TOTALIZZO I CLIENTI ABILITATI AL TRACK & TRACING
100100110401     C***  STATRK        IFEQ      'X'
100200110401     C***                ADD       1             WCATRF            5 0
100300110401     C***                ADD       1             WCATRT            5 0
100400110401     C***                ENDIF
100500990204     C* TOTALIZZO SEMPRE MA STAMPO SOLO SE RIENTRA NEI PARAMETRI        BART
100600990204     C     WSCART        IFEQ      'S'
100700140304     C*  COMODATI
100800140411     c                   clear                   wcomod
100900140304     c                   clear                   sav_i
101000140415     c                   clear                   CmK
101100140415     c                   clear                   CmDt
101200140415     c                   clear                   CmQt
101300140415     c                   clear                   CmDs
101400140415     c                   clear                   stvksal
101500140415     c                   z-add     1             in
101600140304    1c                   if        stat3r<>*blanks
101700140410     C****               MOVEL     '3R'          KTBCOD
101800140415     c*  Cliente singolo --> reperisco i comodati del cliente
101900140304    2c                   if        savleg=' '
102000140410     C****               MOVEL     DEPKSC        KTBKEY
102100140410     C***  KEYTAC        CHAIN     TABEL00F
102200140410    3c****               if        %found(tabel00f)
102300140410     C****               MOVEL     TBLUNI        DS3R
102400140410    3c***                endif
102500140411     c                   move      depksc        wksccomod
102600140410     c                   exsr      Sr_Comodato
102700140304   x2c                   else
102800140304     c*  Cliente non singolo --> reperisco clienti della famiglia
102900140304     c                   eval      w_bsksc=depksc
103000140304     c                   exsr      sr_tibs10
103100140304     c                   clear                   wtrov             1
103200140304     c                   z-add     1             i
103300140304    3c                   dow       i<=%elem(skc) and skc(i)<>*blanks
103400140304     c                             and wtrov=' '
103500140411     c****               eval      ktbkey=%subst(skc(i):5:7)
103600140411     C***  KEYTAC        CHAIN     TABEL00F
103700140411    4c***                if        %found(tabel00f)
103800140411     C***                MOVEL     TBLUNI        DS3R
103900140411     c                   move      skc(i)        wksccomod
104000140411     c                   exsr      sr_comodato
104100140411     c                   if        wcomod<>*blanks
104200140304     c                   eval      wtrov='1'
104300140304     c                   eval      sav_i=i+1
104400140304    4c                   endif
104500140304     c                   add       1             i
104600140304    3c                   enddo
104700140304    2c                   endif
104800140304    1c                   endif
104900140306     c* se non ho comodato da stampare pulisco anche il codice
105000140411     c                   if        wcomod=*blanks
105100140411     c                   clear                   wksccomod
105200140306     c                   endif
105300990204     C* PER PRIMO CLIENTE STAMPATO DELLA FILIALE STAMPO TESTATA         BART
105400990204     C     *IN88         IFEQ      '0'
105500990204     C                   EXSR      DECFIP
105600010626     C     *IN14         IFEQ      *OFF
105700990204     C                   EXCEPT    TESTA                                        *SALTO PAGINA
105800000710     C                   EXCEPT    FIP                                          *DECODIFICA FILIALE
105900990204     C                   EXCEPT    SPAZIO                                       *DECODIFICA FILIALE
106000010627     C                   SETON                                        88
106100010626     C                   ELSE
106200010627     C     STASPB        IFEQ      0                                            TESTATA SOLO SE RIGA
106300140411     c     wcomod        ANDNE     *blanks
106400010626     C                   EXCEPT    TESTAT                                       *SALTO PAGINA
106500010626     C                   EXCEPT    FIP1                                         *DECODIFICA FILIALE
106600010626     C                   EXCEPT    SPAZIO1                                      *DECODIFICA FILIALE
106700010627     C                   SETON                                        88
106800010626     C                   ENDIF
106900010627     C                   ENDIF
107000990204     C                   ENDIF
107100140228
107200140304    1c                   if        not *in14
107300140304    2c                   if        savleg='S'
107400140304     c                   except    rigahi
107500140304    2c                   endif
107600140304     C                   EXCEPT    RIGA
107700140304    1c                   endif
107800140310
107900140304    1C   14STASPB        IFEQ      0
108000140411     c     wcomod        ANDNE     *blanks
108100140304    2c                   if        savleg='S'
108200140304     c                   except    riga2hi
108300140304    2c                   endif
108400140228     C                   EXCEPT    RIGA2
108500140304    1C                   ENDIF
108600010419     c* se presente materiale in comodato lo stampo in seconda riga
108700140303     c* n14§3rnot        ifne      *blanks
108800140303     c*                  except    RIGA1
108900140303     c*                  endif
109000010627     C*****              SETON                                        88
109100140311     c* ----------------------------------
109200140311     c* SOLO SE HO STAMPATO La PRIMA RIGA:
109300140311     c* ----------------------------------
109400140411    0c                   if        not *in14 or (staspb=0 and wcomod<>*blanks)
109500140311     c* Devo stampare anche la SECONDA RIGA se:
109600140311     c* 1) il cliente padre ha più di un cliente con comodato
109700140310     c*    oppure
109800140311    1c                   if        (savleg='S' and sav_i>0) or
109900140317     c* 2) il cliente ha almeno una filiale di partenza bolle
110000140317     c                              svfil(1)>*zeros
110100140311     c
110200140311     c* preparo la stringa delle filiali di partenza
110300140311     c                   clear                   stafilp
110400140311     c                   clear                   savfilp
110500140317     c                   if        svfil(1)>*zeros
110600140311     C                   EXSR      sr_StringaFilP
110700140311     c                   endif
110800140310     c* se presenti più comodati li recupero e stampo
110900140310    2c                   if        savleg='S' and sav_i>0
111000140311     c                   exsr      Sr_AltriComo
111100140310   x2c                   else
111200140311     c* stampo la riga delle sole filiali di partenza se non presenti altri comodati
111300140411     c                   clear                   wcomod
111400140411     c                   clear                   wksccomod
111500140311     c                   exsr      sr_AltraRiga
111600140310    2c                   endif
111700140310    1c                   endif
111800140311     c* SCARICO I Dati Stampati nel FILE Di Work
111900140311     c****               if        not *in14 or (staspb=0 and §3rnot<>*blanks)
112000110817     c                   exsr      sr_wrtstv
112100140311    0c                   endif
112200990204     C                   ENDIF
112300000000     C*
112400990204     C**  TUTTI E NON SOLO I "POTENZIALI"
112500990204     C*
112600970130     C* TOTALIZZO COME FILIALE
112700970130     C                   ADD       STASPT        STTSPT            8 0
112800970130     C                   ADD       STACOT        STTCOT            8 0
112900970130     C                   ADD       STASPA        STTSPA            8 0
113000970130     C                   ADD       STASPB        STTSPB            8 0
113100010419     C                   ADD       STASPK        STTSPK            8 0
113200140228     C****               ADD       STASPC        STTSPC            8 0
113300970130     C                   ADD       STACOB        STTCOB            8 0
113400970130     C                   ADD       1             STTCLI            6 0          TOT.CLI
113500970130     C     STASPA        IFGT      *ZERO
113600970130     C     STASPB        ANDEQ     *ZERO
113700010423     C     STASPK        ANDEQ     *ZERO
113800970130     C                   ADD       1             STTCLA            6 0          CLI DSK A
113900970130     C                   ENDIF
114000010423     C     staspk        ifgt      *zeros
114100010423     c     staspb        andeq     *zeros
114200010423     c                   add       1             sttclc            6 0
114300010423     c                   endif
114400970130     C     STASPB        IFGT      *ZERO
114500970130     C                   ADD       1             STTCLB            6 0          CLI DSK B
114600970130     C                   ENDIF
114700970130     C* TOTALIZZO COME FINALE
114800970130     C                   ADD       STASPT        STFSPT            8 0
114900970130     C                   ADD       STACOT        STFCOT            8 0
115000970130     C                   ADD       STASPA        STFSPA            8 0
115100010419     C                   ADD       STASPK        STFSPK            8 0
115200970130     C                   ADD       STASPB        STFSPB            8 0
115300140228     C****               ADD       STASPC        STFSPC            8 0
115400970130     C                   ADD       STACOB        STFCOB            8 0
115500970130     C                   ADD       1             STFCLI            6 0          TOT.CLI
115600970130     C     STASPA        IFGT      *ZERO
115700970130     C     STASPB        ANDEQ     *ZERO
115800010423     C     STASPK        ANDEQ     *ZERO
115900970130     C                   ADD       1             STFCLA            6 0          CLI DSK A
116000970130     C                   ENDIF
116100010423     C     staspk        ifgt      *zeros
116200010423     c     staspb        andeq     *zeros
116300010423     c                   add       1             stfclc            6 0
116400010423     c                   endif
116500970130     C     STASPB        IFGT      *ZERO
116600970130     C                   ADD       1             STFCLB            6 0          CLI DSK B
116700970130     C                   ENDIF
116800990204     C**  SOLO I "POTENZIALI"
116900990204     C*
117000990204     C     WSCAR2        IFEQ      'S'
117100990204     C* TOTALIZZO COME FILIALE
117200990204     C                   ADD       STASPT        S2TSPT            8 0
117300990204     C                   ADD       STACOT        S2TCOT            8 0
117400990204     C                   ADD       STASPA        S2TSPA            8 0
117500010419     C                   ADD       STASPK        S2TSPK            8 0
117600990204     C                   ADD       STASPB        S2TSPB            8 0
117700140228     C****               ADD       STASPC        S2TSPC            8 0
117800990204     C                   ADD       STACOB        S2TCOB            8 0
117900990204     C                   ADD       1             S2TCLI            6 0          TOT.CLI
118000990204     C     STASPA        IFGT      *ZERO
118100990204     C     STASPB        ANDEQ     *ZERO
118200010424     C     STASPK        ANDEQ     *ZERO
118300990204     C                   ADD       1             S2TCLA            6 0          CLI DSK A
118400990204     C                   ENDIF
118500010423     C     staspk        ifgt      *zeros
118600010423     c     staspb        andeq     *zeros
118700010423     c                   add       1             s2tclc            6 0
118800010423     c                   endif
118900990204     C     STASPB        IFGT      *ZERO
119000990204     C                   ADD       1             S2TCLB            6 0          CLI DSK B
119100990204     C                   ENDIF
119200990204     C* TOTALIZZO COME FINALE
119300990204     C                   ADD       STASPT        S2FSPT            8 0
119400990204     C                   ADD       STACOT        S2FCOT            8 0
119500990204     C                   ADD       STASPA        S2FSPA            8 0
119600010419     C                   ADD       STASPK        S2FSPK            8 0
119700990204     C                   ADD       STASPB        S2FSPB            8 0
119800140228     C****               ADD       STASPC        S2FSPC            8 0
119900990204     C                   ADD       STACOB        S2FCOB            8 0
120000990204     C                   ADD       1             S2FCLI            6 0          TOT.CLI
120100990204     C     STASPA        IFGT      *ZERO
120200990204     C     STASPB        ANDEQ     *ZERO
120300010426     C     STASPK        ANDEQ     *ZERO
120400990204     C                   ADD       1             S2FCLA            6 0          CLI DSK A
120500990204     C                   ENDIF
120600010423     C     staspk        ifgt      *zeros
120700010423     c     staspb        andeq     *zeros
120800010423     c                   add       1             s2fclc            6 0
120900010423     c                   endif
121000990204     C     STASPB        IFGT      *ZERO
121100990204     C                   ADD       1             S2FCLB            6 0          CLI DSK B
121200990204     C                   ENDIF
121300990204     C*
121400990204     C                   ENDIF
121500970130     C*
121600970130     C* AZZERA I TOTALI CLIENTE
121700970130     C                   MOVE      *ZEROS        STASPT            8 0
121800970130     C                   MOVE      *ZEROS        STACOT            8 0
121900990225     C                   MOVE      *ZEROS        STAGIO            4 0
122000970130     C                   MOVE      *ZEROS        STASPA            8 0
122100110817     C***                MOVE      *ZEROS        STASPH            8 0
122200010419     C                   MOVE      *ZEROS        STASPK            8 0
122300970130     C                   MOVE      *ZEROS        STASPB            8 0
122400140228     C****               MOVE      *ZEROS        STASPC            8 0
122500970130     C                   MOVE      *ZEROS        STACOB            8 0
122600970203     C                   MOVE      *ZEROS        WINDSK            8 0
122700970130E   2C                   ENDDO                                                  *ROTTURA AS/400
122800000000E   1C                   ENDDO                                                  *FINE FILE
122900000000     C*
123000000000     C                   ENDSR
123100000000     C*------------------------------------------------------------------------*
123200000000     C* SETSTP - POSIZIONAMENTO E PRIMA LETTURA DEL FILE
123300000000     C*------------------------------------------------------------------------*
123400000000     C     SETSTP        BEGSR
123500000000     C*
123600000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
123700000000     C                   MOVEL     '0'           $FINE
123800970130     C                   MOVE      *ZEROS        STPAS4
123900970130     C                   MOVE      *ZEROS        STPFIP
124000000000     C                   MOVEL     *BLANKS       STPDCL
124100970130     C                   MOVE      *ZEROS        STPKSC
124200970130     C                   MOVE      *ZEROS        STPDAT
124300000000     C     KEYST2        SETLL     WFSTP02L                           99        *FINE-ARCHIV
124400000000     C*
124500000000IF  1C     *IN99         IFEQ      *ON
124600000000     C                   MOVEL     '1'           $FINE
124700000000X   1C                   ELSE
124800000000     C                   EXSR      LETSTP
124900000000E   1C                   ENDIF
125000000000     C*
125100000000     C                   ENDSR
125200000000     C*------------------------------------------------------------------------*
125300000000     C* LETSTP - LETTURA PROSSIMO RECORD
125400000000     C*------------------------------------------------------------------------*
125500000000     C     LETSTP        BEGSR
125600000000     C*
125700000000     C* LEGGO FINO A:
125800000000     C*     - FINE FILE O SUPERATO LIMITE FINALE
125900000000     C*     - TROVATO RECORD VALIDO
126000000000     C                   MOVEL     'N'           WRECOK
126100000000DO  1C     $FINE         DOWEQ     '0'
126200000000     C     WRECOK        ANDEQ     'N'
126300000000     C*
126400000000     C                   READ      WFSTP02L                               99
126500000000     C*
126600000000IF  2C     *IN99         IFEQ      *ON                                          *FINA FILE
126700000000     C                   MOVEL     '1'           $FINE
126800000000X   2C                   ELSE
126900000000     C                   EXSR      CHKSTP                                       *CONTROLLA RECORD
127000000000E   2C                   ENDIF
127100000000E   1C                   ENDDO
127200000000     C*
127300000000     C                   ENDSR
127400000000     C*------------------------------------------------------------------------*
127500000000     C* CHKSTP - CONTROLLA VALIDITA' RECORD
127600000000     C*------------------------------------------------------------------------*
127700000000     C     CHKSTP        BEGSR
127800000000     C*
127900000000     C                   MOVEL     'S'           WRECOK
128000000000     C*
128100970131     C* CONTROLLO PERIODO
128200970131IF  1C     STPDAT        IFLT      PARDTI
128300970131     C     STPDAT        ORGT      PARDTF
128400000000     C                   MOVEL     'N'           WRECOK
128500000000E   1C                   ENDIF
128600000000     C*
128700000000     C                   ENDSR
128800000000     C*------------------------------------------------------------------------*
128900000000     C* DECCLI - DECODIFICA IL CLIENTE
129000000000     C*------------------------------------------------------------------------*
129100000000     C     DECCLI        BEGSR
129200980116     C*
129300980116     C                   CLEAR                   BS69DS
129400980116     C                   CLEAR                   ACODS
129500110901     C                   CLEAR                   clpds
129600980116     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
129700980116     C                   Z-ADD     DEPKSC        I69KAC                         *CLIENTE X CNACO
129800110901     C                   Z-ADD     DEPKSC        I69Kcp                         *CLIENTE X CNclp
129900980116     C                   CALL      'TIBS69R'
130000980116     C                   PARM                    BS69DS
130100980116     C                   PARM                    ACODS
130200980116     C                   PARM                    INDDS
130300980116     C                   PARM                    CLPDS
130400980116     C                   PARM                    CLSDS
130500980116IF  1C     O69ERR        IFEQ      '1'                                          *ERRORE
130600980116     C                   MOVEL     *ALL'*'       DESCLI           30
130700980116X   1C                   ELSE
130800980116     C                   MOVEL     ACORAG        DESCLI                         *NO ERRORE
130900130321IF  2C***  ACOABL        IFEQ      '8'
131000130321IF  2C     ACOABL        IFne      *blank
131100980116     C                   MOVE      '(*)'         DESCLI                         BLOCCATO
131200980116E   2C                   ENDIF
131300980116E   1C                   ENDIF
131400980116     C*
131500000000     C                   ENDSR
131600000000     C*------------------------------------------------------------------------*
131700970130     C* DECFIP - DECODIFICA AS/400 PARTENZA
131800000000     C*------------------------------------------------------------------------*
131900970130     C     DECFIP        BEGSR
132000000000     C*
132100970130     C                   MOVE      STPFIP        KORFIL
132200990225     C     DEPFIP        IFGT      *ZERO
132300990225     C                   MOVE      DEPFIP        KORFIL
132400990225     C                   ENDIF
132500990225     C                   MOVE      KORFIL        WORFIL            3 0
132600140317     C***  KORFIL        CHAIN     AZORG01L                           99
132700140317IF  1C**** *IN99         IFEQ      *ON
132800140317     c                   eval      ind=%lookup(korfil:fil)
132900140317     c                   if        ind=0
133000970130     C                   MOVEL     *ALL'*'       DESFIP           30
133100000000X   1C                   ELSE
133200140317     C*****              MOVEL     ORGDES        DESFIP
133300140317     C                   MOVEL     dfil(ind)     DESFIP
133400971128     C*
133500971128     C* REPERISCE IL TERMINAL
133600110325     C***                CLEAR                   DSLV55
133700110325     C***                MOVEL     'P'           D55TPT
133800110325     C**                 Z-ADD     ORGFIL        D55LIN
133900110325     C**                 Z-ADD     DATCOR        D55DRF
134000110325     C**                 CALL      'FNLV55R'
134100110325     C**                 PARM                    DSLV55
134200110325IF  2C**   D55ERR        IFNE      *BLANKS
134300110325     C**                 Z-ADD     ORGFIL        D55TFP
134400110325E   2C**                 ENDIF
134500110325     C***                MOVEL     D55TFP        DESTRP            3 0
134600110325     C                   MOVEL     stpas4        DESTRP            3 0
134700000000E   1C                   ENDIF
134800000000     C*
134900000000     C                   ENDSR
135000000000     C*------------------------------------------------------------------------*
135100000000     C* CARICA LE TABELLE
135200000000     C*------------------------------------------------------------------------*
135300000000     C     CARTAB        BEGSR
135400140317     C*---
135500140317     C* ORGANIGRAMMA
135600140317     C*---
135700140317     C                   Z-ADD     *ZEROS        I
135800140317     C     *LOVAL        SETLL     AZORG01L
135900140317     C                   READ      AZORG01L                               98
136000140317DO  1C     *IN98         DOWEQ     *OFF
136100140317IF  2C     ORGFAG        IFNE      'V'
136200140317     C                   ADD       1             I
136300140317     C                   Z-ADD     ORGFIL        FIL(I)
136400140317     C                   MOVEL     ORGDES        DFIL(I)
136500140317     C                   MOVEL     ORGDE3        OG143
136600140317     c                   movel     orgcar        car(i)
136700140317E   2C                   ENDIF
136800140317     C                   READ      AZORG01L                               98
136900140317E   1C                   ENDDO
137000000000     C*--------------------
137100000000     C* CODICI TRATTAMENTO MERCE
137200000000     C*--------------------
137300000000     C                   Z-ADD     0             I
137400010418     C                   Z-ADD     0             ii
137500000000     C                   Z-ADD     1             KTBUTE
137600000000     C                   MOVEL     '1B'          KTBCOD
137700000000     C     KEYTAB        SETLL     TABEL00F                           99
137800000000     C     KEYTAB        READE     TABEL00F                               99
137900000000DO  1C     *IN99         DOWEQ     *OFF
138000000000IF  2C     TBLFLG        IFEQ      *BLANKS
138100000000     C                   ADD       1             I
138200000000     C                   MOVEL     TBLKEY        CTM(I)                         *COD.TRATTAM.MERCE
138300000000     C                   MOVEL     TBLUNI        DS1B
138400000000     C                   MOVEL     §1BFG7        FG7(I)                         *DETT.SEGNAC. S/N
138500010418     c     §1bdkc        ifeq      'B'
138600010418     c     §1bdkc        oreq      'C'
138700010418     c                   add       1             ii                3 0
138800010418     c                   movel     tblkey        tmc(ii)
138900010418     c                   movel     §1bdkc        tmt(ii)
139000010418     c                   endif
139100000000E   2C                   ENDIF
139200000000     C     KEYTAB        READE     TABEL00F                               99
139300000000E   1C                   ENDDO
139400000000     C*--------------------
139500000000     C* CLIENTI CON DISCHETTO
139600000000     C*--------------------
139700000000     C                   Z-ADD     0             I
139800000000     C                   Z-ADD     1             KTBUTE
139900000000     C                   MOVEL     '3C'          KTBCOD
140000000000     C     KEYTAB        SETLL     TABEL00F                           99
140100000000     C     KEYTAB        READE     TABEL00F                               99
140200000000DO  1C     *IN99         DOWEQ     *OFF
140300970130     C                   MOVEL     TBLUNI        DS3C
140400000000IF  2C     TBLFLG        IFEQ      *BLANKS
140500980131     C**         §3COKD    ANDEQ'*'                        PARTITO
140600000000     C                   ADD       1             I
140700000000     C                   MOVEL     TBLKEY        CLI(I)                         *CLIENTE
140800000000     C                   MOVEL     §3CCBA        CBA(I)                         *SUPPORTO CLI>BART
140900140221     c                   eval      w_bsksc=cli(I)
141000140221     c                   exsr      sr_tibs10
141100140411     c                   eval      ap=%lookup(d10cop:cun)
141200140303     C                   MOVE      d10cop        CUN(I)                         *CLI. UNIF.
141300140225     C                   eval      leg(i)=wlegame
141400140310     c
141500140313     c* cliente in più aree  in base alle filiali cliente del gruppo
141600140411     c                   if        ap>0
141700140411     c                   eval      pfi(i)=pfi(ap)
141800140411     c                   else
141900140310     c                   exsr      sr_wpfi
142000140310     c                   move      wpfi          pfi(i)
142100140411     c                   endif
142200140310     C
142300140225     C                   MOVEL     §3CCKS        CKS(I)                         *CLI. UNIF.
142400990225     C                   MOVEL     §3CNWC        CAP(I)                         *CAPPARIO
142500990225     C                   MOVEL     §3CPES        PES(I)                         *GEST.PESO
142600010920     c                   movel     §3cbac        BAC(I)
142700000000     C                   Z-ADD     1             Y                 6 0
142800000000     C     §3CCTM        LOOKUP    CTM(Y)                                 98
142900000000IF  3C     *IN98         IFEQ      *OFF
143000000000     C                   MOVEL     *ALL'*'       DET(I)                         *CTM NON TROVATO
143100000000X   3C                   ELSE
143200000000     C                   MOVEL     FG7(Y)        DET(I)                         *CTM TROVATO
143300000000E   3C                   ENDIF
143400000000E   2C                   ENDIF
143500000000     C     KEYTAB        READE     TABEL00F                               99
143600000000E   1C                   ENDDO
143700000710     C*
143800000000     C                   ENDSR
143900140227     C*------------------------------------------------------------------------*
144000970912     C* ADD3R  - GENERA NEL FILE UN RECORD PER OGNI CLIENTE
144100970912     C*          PRESENTE NELLA TABELLA 3R E NON GIA' PRESENTE NEL FILE
144200970912     C*------------------------------------------------------------------------*
144300970912     C     ADD3R         BEGSR
144400140408     C***                SETOFF                                       9596
144500140408     c                   clear                   sav_cmdksc
144600140408     C***                MOVEL     '3R'          KTBCOD
144700140408     C***  KEYTAB        SETLL     TABEL00F
144800140408     C***  KEYTAB        READE     TABEL00F                               96
144900140408     C***  *IN96         DOWEQ     *OFF
145000140408     C***                MOVEL     TBLUNI        DS3R
145100140408     C***  TBLFLG        IFEQ      *BLANKS
145200140408     C**   §3RNOT        ANDGT     *BLANKS
145300140408     c***                movel     tblkey        w0070             7 0
145400140408     C***                z-add     w0070         w_bsksc
145500140408     c     *loval        setll     uncmd01l
145600140408     c                   read      uncmd01l
145700140408     c                   dow       not %eof(uncmd01l)
145800140408     c* escludo contratti annullati o chiusi ed elaboro solo a nuovo cod cliente
145900140411     c                   if        cmdcfg<>'A' and cmdcfg<>'C'
146000140411     c                             and cmdksc<>sav_cmdksc
146100140408     C                   z-add     cmdksc        w_bsksc
146200140221     c                   exsr      sr_tibs10
146300140303     c                   move      d10cop        stpksc
146400970915     C                   Z-ADD     PARDTF        STPDAT
146500970915     C     KEYSTP        CHAIN     WFSTP01L                           95
146600970915     C     *IN95         IFEQ      *ON
146700000316     C                   MOVE      *ZERO         STPNUA
146800000316     C                   MOVE      *ZERO         STPNUM
146900970912     C                   MOVE      *ZERO         STPIMA
147000990225     C                   MOVE      *ZERO         STPNUB
147100970912     C                   MOVE      *ZERO         STPIMB
147200010418     C                   MOVE      *ZERO         STPNUC
147300010418     C                   MOVE      *ZERO         STPIMC
147400010418     C                   MOVE      *ZERO         STPNUK
147500010418     C                   MOVE      *ZERO         STPIMK
147600970912     C                   MOVE      *ZERO         STPIMP
147700140228     C***                MOVE      *ZERO         STPNCA
147800000316     C                   MOVE      'X'           STPT3R
147900110624     C***                MOVE      *BLANKS       STPTRK
148000110624     C                   MOVE      '?'           STPTRK
148100140227     C                   clear                   STPpes
148200140520     C                   clear                   STPalr
148300140708     c                   clear                   sfil
148400140227     c                   eval      stpleg=wlegame
148500140411     c                   eval      ap=%lookup(d10cop:cun)
148600140411     c                   if        ap>0
148700140411     c                   eval      stppfi=pfi(ap)
148800140411     c                   else
148900140310     c                   exsr      sr_wpfi
149000140310     c                   eval      stppfi=wpfi
149100140411     c                   endif
149200970912     C*
149300970912     C                   MOVEL     STPKSC        STPFIP
149400970912     C                   MOVEL     STPKSC        KORFIL
149500140317     C***  KORFIL        CHAIN     AZORG01L                           97
149600140317     c                   eval      ind=%lookup(korfil:fil)
149700140317     c                   clear                   stpas4
149800140317     c                   if        ind>0
149900140317     c                   eval      stpas4=car(ind)
150000140317     c                   endif
150100971128     C*
150200971128     C* REPERISCE IL TERMINAL
150300110325     C*                  CLEAR                   DSLV55
150400110325     C*                  MOVEL     'P'           D55TPT
150500110325     C*                  Z-ADD     ORGFIL        D55LIN
150600110325     C*                  Z-ADD     DATCOR        D55DRF
150700110325     C*                  CALL      'FNLV55R'
150800110325     C*                  PARM                    DSLV55
150900110325     C*    D55ERR        IFNE      *BLANKS
151000110325     C*                  Z-ADD     ORGFIL        D55TFP
151100110325     C*                  ENDIF
151200110325     C*                  MOVEL     D55TFP        STPAS4
151300140317     C***                MOVEL     orgcar        STPAS4
151400970912     C                   Z-ADD     STPKSC        DEPKSC
151500970912     C                   EXSR      DECCLI                                       *DESCRIZIONE CLIENTE
151600970912     C                   MOVEL     DESCLI        STPDCL
151700970912     C                   WRITE     WFSTP                                        *INSERISCE
151800970915     C                   ELSE
151900000317     C                   MOVE      'X'           STPT3R
152000970915     C                   UPDATE    WFSTP
152100970912     C                   ENDIF
152200140408     c                   eval      sav_cmdksc=cmdksc
152300140408     c     cmdksc        setgt     uncmd01l
152400970912     C                   ENDIF
152500140408     C***  KEYTAB        READE     TABEL00F                               96
152600140408     C                   READ      uncmd01l
152700970912E   1C                   ENDDO
152800970912     C                   ENDSR
152900000316     C*------------------------------------------------------------------------*
153000110624     C* ADDTRK - GENERA NEL FILE UN RECORD PER OGNI CLIENTE PADRE
153100000316     C*          PRESENTE NEL ARCHIVIO DEI CLIENTI ABILITATI AL TRACK & TRACING
153200000316     C*------------------------------------------------------------------------*
153300110401     C*    ADDTRK        BEGSR
153400110401     C*                  SETOFF                                       9495
153500110401     C*                  MOVEL     'TT'          KVSISV
153600110401     C*    KEYVSS        SETLL     TIVSS04L
153700110401     C*    KEYVSS        READE     TIVSS04L                               94
153800110401     C*    *IN94         DOWEQ     *OFF
153900110401     C*    VSSVAT        IFEQ      *BLANKS
154000110401     C*    VSSDDE        ANDLE     PARDTF
154100110401     C*    VSSDSC        ANDGE     PARDTI
154200110401     C*                  MOVE      VSSKSU        STPKSC
154300110401     C*                  Z-ADD     PARDTF        STPDAT
154400110401     C*    KEYSTP        CHAIN     WFSTP01L                           95
154500110401     C*    *IN95         IFEQ      *ON
154600110401     C*    PARTRK        IFEQ      'S'
154700110401     C*                  MOVE      *ZERO         STPNUA
154800110401     C*                  MOVE      *ZERO         STPNUM
154900110401     C*                  MOVE      *ZERO         STPIMA
155000110401     C*                  MOVE      *ZERO         STPNUB
155100110401     C*                  MOVE      *ZERO         STPIMB
155200110401     C*                  MOVE      *ZERO         STPNUC
155300110401     C*                  MOVE      *ZERO         STPIMC
155400110401     C*                  MOVE      *ZERO         STPNUK
155500110401     C*                  MOVE      *ZERO         STPIMK
155600110401     C*                  MOVE      *ZERO         STPIMP
155700110401     C*                  MOVE      *ZERO         STPNCA
155800110401     C*                  MOVE      *BLANKS       STPT3R
155900110401     C*                  MOVE      'X'           STPTRK
156000110401     C*
156100110401     C*                  MOVEL     STPKSC        STPFIP
156200110401     C*                  MOVEL     STPKSC        KORFIL
156300110401     C*    KORFIL        CHAIN     AZORG01L                           97
156400000316     C*
156500110401     C*                  MOVEL     orgcar        STPAS4
156600110401     C*                  Z-ADD     STPKSC        DEPKSC
156700110401     C*                  EXSR      DECCLI                                       *DESCRIZIONE CLIENTE
156800110401     C*                  MOVEL     DESCLI        STPDCL
156900110401     C*                  WRITE     WFSTP                                        *INSERISCE
157000110401     C*                  ENDIF
157100110401     C*                  ELSE
157200110401     C*                  MOVE      'X'           STPTRK
157300110401     C*                  UPDATE    WFSTP
157400110401     C*                  ENDIF
157500110401     C*                  ENDIF
157600110401     C*    KEYVSS        READE     TIVSS04L                               94
157700110401E   1C*                  ENDDO
157800110401     C*                  ENDSR
157900110624     C*------------------------------------------------------------------------*
158000110624     C* UPDVAS - AGGIORNA OGNI CLIENTE PADRE CON L'INDICAZIONE DI VAS PERSONALIZZATO
158100110624     C*          O STANDARD X FILE VAB E VAT
158200110624     C*------------------------------------------------------------------------*
158300110624     C     UPDVAS        BEGSR
158400110624     C*
158500110624     C* Leggo tutta l'anagrafica clienti/tipi-file
158600110624     C     *START        SETLL     TIVIR01L
158700110624     C                   READ      TIVIR01L
158800110624     C*
158900110624     C                   MOVEL     '?'           wTipVAS           1
159000110624     C                   MOVEL     VIRKSC        savVIRKSC         8
159100110624     C*
159200110624     C                   DOW       not %eof(TIVIR01L)
159300110624     C*
159400110624     C* Gestisco rottura d codice x cliente
159500110624     C                   IF        VIRKSC <> savVIRKSC
159600110624     C*
159700110624     C* Aggiorno il wrk-file
159800110624     C                   MOVE      savVIRKSC     STPKSC
159900110624     C     STPKSC        SETLL     WFSTP01L
160000110624     C                   IF        %equal(WFSTP01L)
160100110624     C     STPKSC        READE     WFSTP01L
160200110624     C                   DOW       not %eof(WFSTP01L)
160300110704     C                   EVAL      STPTRK = wTipVAS
160400110704     C*
160500110704     C* RECUPERA I DATI CLIENTE X FORZATURA RISPETTO A TIPO INVIO DATI CLI2BRT
160600110704     C                   Z-ADD     1             I                 6 0
160700110704     C     STPKSC        LOOKUP    CLI(I)                                 99
160800110704     C     *IN99         IFNE      *OFF
160900110704     C                   IF        CBA(I) = 'SNA'   OR
161000110704     C                             CBA(I) = 'ESWEB' OR
161100110704     C                             CBA(I) = 'ESYSP'
161200110704     C                   EVAL      STPTRK = 'S'
161300110704     C                   ENDIF
161400110704     C                   ENDIF
161500110704     C*
161600110624     C                   UPDATE    WFSTP
161700110624     C     STPKSC        READE     WFSTP01L
161800110624     C                   ENDDO
161900110624     C                   ENDIF
162000110624     C*
162100110624     C                   MOVEL     '?'           wTipVAS           1
162200110624     C                   MOVEL     VIRKSC        savVIRKSC
162300110624     C                   ENDIF
162400110624     C*
162500110624     C                   IF        wTipVAS <> 'P'
162600110624     C*
162700110624     C* Solo tipi file VB e VT
162800110624     C                   IF        VIRTIP = 'VB' OR VIRTIP='VT'
162900110624     C*
163000110624     C* Solo se anagrafica correntemente valida
163100110624     C                   IF        DATCOR >= VIRDTI AND
163200110624     C                             DATCOR <= VIRDTF
163300110624     C*
163400110624     C* Verifico se trattasi d personalizzato
163500110624     C                   IF        VIRPGT <> *blanks AND
163600110624     C                             (%subst(VIRPGT:1:5) <> 'TIS7T' OR
163700110624     C                              %subst(VIRPGT:1:6)  = 'TIS7TL')
163800110624     C                   EVAL      wTipVAS = 'P'
163900110624     C                   ELSE
164000110624     C                   EVAL      wTipVAS = 'S'
164100110624     C                   ENDIF
164200110624     C*
164300110624     C                   ENDIF
164400110624     C                   ENDIF
164500110624     C                   ENDIF
164600110624     C*
164700110624     C                   READ      TIVIR01L
164800110624     C                   ENDDO
164900110624     C*
165000110624     C                   ENDSR
165100000316     C*------------------------------------------------------------------------*
165200000316     C* CANWFS - CANCELLAZIONE PERIODO RICHIESTO
165300000316     C*------------------------------------------------------------------------*
165400000316     C     CANWFS        BEGSR
165500000316     C*
165600000316     C* PER PRIMA COSA GESTISCO L'APERTURA MANUALE
165700000316     C                   OPEN      WFSTP03L
165800000316     C*
165900000316     C     PARDTI        SETLL     WFSTP03L                           44
166000000316     C  N44              READ      WFSTP03L                               44
166100000316     C     *IN44         DOWEQ     *OFF
166200000316     C     STPDAT        IFGE      PARDTI
166300000710     C     STPDAT        ANDLE     PARDTI
166400000316     C                   DELETE    WFSTP3
166500000710     C                   ENDIF
166600000316     C                   READ      WFSTP03L                               44
166700000316     C                   ENDDO
166800000316     C*
166900000316     C* ORA INVECE LO CHIUDO
167000000316     C                   CLOSE     WFSTP03L
167100000316     C*
167200000316     C                   ENDSR
167300110817     C*------------------------------------------------------------------------*
167400110817     c* Scrittura file di work WFSTV00F
167500110817     C*------------------------------------------------------------------------*
167600110817     C     sr_wrtstv     BEGSR
167700140311     c* pulisco i campi che possono non essere impostati
167800140311     c                   clear                   stvabl
167900140311     c                   clear                   stvcot
168000140311     c                   clear                   stvgio
168100140311     c                   clear                   stvspg
168200140311     c                   clear                   stvcog
168300140311     c                   clear                   stvvsp
168400140311     c                   clear                   stvpes
168500140311     c                   clear                   stvtsd
168600140311     c                   clear                   stvgsd
168700140311     c                   clear                   stvgsr
168800140311     c                   clear                   stvfsd
168900140311     c                   clear                   stvcsd
169000140311     c                   clear                   stvdsm
169100140520     c                   clear                   stvalr
169200140311
169300110817     c                   z-add     datcor        STVDEL
169400110817     c                   eval      STVUSR=knmus
169500110817     c                   z-add     pardti        STVDAL
169600110817     c                   z-add     pardtf        sTVAL
169700110817     c                   z-add     depksc        STVKSC
169800140311     c                   eval      STVRAG=descli
169900140311     c                   if        acoabl<>*blank
170000140311     c                   eval      STVABL='*'
170100140311     c                   endif
170200140228     c                   movel     savleg        STVleg
170300140311     c                   movel     savpfi        STVpfi
170400110901     c* reperisco il commerciale del cliente
170500140311     c                   eval      stvage=clpage
170600110901
170700110817     c                   z-add     staspt        STVSPT
170800110817     c  n14              z-add     stacot        STVCOT
170900110817     c  n14              z-add     stagio        STVGIO
171000110817     c  n14              z-add     staspg        STVSPG
171100110817     c  n14              z-add     stacog        sTVCOG
171200110817     c                   z-add     staspa        stvspa
171300110817     c                   z-add     staspb        stvspb
171400110817     c                   z-add     staspk        stvspk
171500140228     c* n14              eval      STVDCL=stadet
171600140228     c* n14              eval      STVVCP=stacap
171700110817     c  n14              eval      STVVSP=stavab
171800110817     c  n14              eval      STVPES=stapes
171900140228     c* n14              eval      STVCBA=stacba
172000140228     c* n14              z-add     staspc        stvspc
172100140228     c  n14              eval      STVTSD=stacass
172200140228     c  n14              eval      STVGSD=stagia
172300140228     c  n14              eval      STVGSR=stadisp
172400140228     c  n14              eval      STVFSD=stafatt
172500140228     c  n14              eval      STVCSD=stapod
172600110817     c  n14              eval      STVDSM=WINDSD
172700140612     c  n14              eval      STValr=Wstaalr
172800140228     c* n14              eval      STVBAC=stabac
172900140311     c*
173000140311     c****               if        §3rnot<>*blanks
173100140311     c****               eval      STVCOM=§3rnot
173200140311     c****               movel     ktbkey        stvage
173300140311     c****               endif
173400110817     c                   z-add     worfil        STVFIP
173500110817     c                   z-add     destrp        STVAS4
173600110817     c                   write     wfstv000
173700140228     c* se cliente con legame verifico presenza di altri comodati stampare e memorizzare
173800140311     c***                if        stvleg='S' and sav_i>0
173900140311     c***                exsr      sr_AltriComo
174000140311     c***                endif
174100110817     C                   ENDSR
174200140221     C*------------------------------------------------------------------------*
174300140227     C* Ricerca codice padre del legame "ST"
174400140221     C*------------------------------------------------------------------------*
174500140221     C     sr_tibs10     BEGSR
174600140225     c                   eval      wlegame='S'
174700140221     c* verifico se codice presente come Padre
174800140221     c                   clear                   tibs10ds
174900140221     c                   eval      d10tle='ST'
175000140221     c                   eval      d10paf='F'
175100140221     c                   eval      d10cod=w_bsksc
175200140221     c                   call      'TIBS10R'
175300140221     c                   parm                    tibs10ds
175400140221     c                   if        d10cop>0
175500140221     c                   leavesr
175600140221     c                   endif
175700140221     c* verifico se codice presente come Figlio
175800140221     c                   eval      d10paf='P'
175900140221     c                   call      'TIBS10R'
176000140221     c                   parm                    tibs10ds
176100140221     c                   if        d10cop>0
176200140221     c                   leavesr
176300140221     c                   endif
176400140221     c* Se non è nè padre nè figlio è Singolo
176500140221     c                   eval      d10cop=d10cod
176600140225     c                   clear                   wlegame
176700140221     c
176800140221     c                   endsr
176900140227     C*------------------------------------------------------------------------*
177000140227     C* Detemina tipo VAS: Personalizzato, easyweb/speb, o standard
177100140227     C*------------------------------------------------------------------------*
177200140227     C     sr_trk        BEGSR
177300140227     c                   eval      wTipVas='?'
177400140227     c     w_kscvir      setll     tivir01l
177500140227     c     w_kscvir      reade     tivir01l
177600140227     C                   DOW       not %eof(TIVIR01L) and wtipVaS<>'P'
177700140227     C                   IF        wTipVAS <> 'P'
177800140227     C* Solo tipi file VB e VT
177900140227     C                   IF        VIRTIP = 'VB' OR VIRTIP='VT'
178000140227     C*
178100140227     C* Solo se anagrafica correntemente valida
178200140227     C                   IF        DATCOR >= VIRDTI AND
178300140227     C                             DATCOR <= VIRDTF
178400140227     C*
178500140227     C* Verifico se trattasi d personalizzato
178600140227     C                   IF        VIRPGT <> *blanks AND
178700140227     C                             (%subst(VIRPGT:1:5) <> 'TIS7T' OR
178800140227     C                              %subst(VIRPGT:1:6)  = 'TIS7TL')
178900140227     C                   EVAL      wTipVAS = 'P'
179000140227     c                   else
179100140227     C                   EVAL      wTipVAS = 'S'
179200140227     c                   endif
179300140227     c                   endif
179400140227     c                   endif
179500140303     c                   endif
179600140227     c     w_kscvir      reade     tivir01l
179700140227     c                   enddo
179800140303     c                   eval      i=%lookup(%dec(%subst(w_kscvir:2:7):7:0)
179900140303     c                             :cli)
180000140303     c                   if        i>0
180100140227     c                   select
180200140227     c                   when      cba(i) = 'SNA'
180300140227     c                   eval      wTipVas='S'
180400140227     c                   when      cba(i) = 'ESWEB' or
180500140227     c                             cba(i) = 'ESYSP'
180600140227     c                   eval      wTipVas='E'
180700140227     c                   endsl
180800140303     c                   endif
180900140227     c                   endsr
181000140228     C*------------------------------------------------------------------------*
181100140228     C* Recupero dati da tabella 3k e 4c
181200140228     C*------------------------------------------------------------------------*
181300140228     C     sr_3k4c       BEGSR
181400140228     C* VAS DA FILIALE (DATI CONS./GIACENZE)
181500140228     C*
181600140228     C                   MOVE      *BLANK        DS3K
181700140228     C                   MOVEL     '3K'          KTBCOD
181800140228     C                   MOVEL     wcli          KTBKEY
181900140228     C     KEYTAC        CHAIN     TABEL00F
182000140228     c                   if        %found(tabel00f)
182100140228     C                   MOVEL     TBLUNI        DS3K
182200140228     c                   endif
182300140228     C*
182400140228     C* VAS DA SEDE (C/A / FATTURE)
182500140228     C*
182600140228     C                   MOVE      *BLANK        DS4C
182700140228     C                   MOVEL     '4C'          KTBCOD
182800140228     C                   MOVEL     wcli          KTBKEY
182900140228     C     KEYTAC        CHAIN     TABEL00F
183000140228     c                   if        %found(tabel00f)
183100140228     C                   MOVEL     TBLUNI        DS4C
183200140228     c                   endif
183300140228     c                   endsr
183400140228     C*------------------------------------------------------------------------*
183500140228     C* Reperimento e stampa dei comodati per clienti legati
183600140228     C*------------------------------------------------------------------------*
183700140228     C     sr_AltriComo  BEGSR
183800140303     c                   z-add     sav_I         i
183900140303     c                   dow       i<=%elem(skc) and skc(i)<>*blanks
184000140411     C*****              MOVEL     '3R'          KTBCOD
184100140411     c****               eval      ktbkey=%subst(skc(i):5:7)
184200140411     C**** KEYTAC        CHAIN     TABEL00F
184300140411     c****               if        %found(tabel00f)
184400140411     C***                MOVEL     TBLUNI        DS3R
184500140411     c                   move      skc(i)        wksccomod
184600140411     c                   exsr      sr_comodato
184700140411     c                   if        wcomod<>*blanks
184800140311     c                   exsr      sr_AltraRiga
184900140311
185000140310     c*                  eval      STVCOM=§3rnot
185100140310     c*                  movel     ktbkey        stvage
185200140310     c*                  clear                   STVSPT
185300140310     c*                  clear                   STVCOT
185400140310     c*                  clear                   STVGIO
185500140310     c*                  clear                   STVSPG
185600140310     c*                  clear                   STVCOG
185700140310     c*                  clear                   STVSPA
185800140310     c*                  clear                   STVSPB
185900140310     c*                  clear                   STVSPK
186000140310     c*                  clear                   STVVSP
186100140310     c*                  clear                   STVPES
186200140310     c*                  clear                   STVTSD
186300140310     c*                  clear                   STVGSD
186400140310     c*                  clear                   STVGSR
186500140310     c*                  clear                   STVFSD
186600140310     c*                  clear                   STVCSD
186700140310     c*                  clear                   STVDSM
186800140310     c*                  write     wfstv000
186900140411     c**                 endif
187000140228     c                   endif
187100140303     c                   add       1             i
187200140228     c                   enddo
187300140228     c                   endsr
187400140310     C*------------------------------------------------------------------------*
187500140310     C*  Dato elenco clienti del legame st verifica se codici di più filiali
187600140310     C*------------------------------------------------------------------------*
187700140310     C     sr_wpfi       BEGSR
187800140313     c                   z-add     1             is
187900140313     c                   clear                   wpfi
188000140313     c                   clear                   wfcli
188100140313     c                   clear                   wcarcli_sav
188200140313     c                   clear                   wcarcli
188300140313    3c                   dow       is<=%elem(skc) and skc(is)>*zeros
188400140313     c                             and wpfi=' '
188500140313     c* reperisco il codice area della filiale cliente
188600140313     c                   eval      wfcli=%dec(%subst(skc(is):5:3):3:0)
188700140313     c                   clear                   orgcar
188800140317     c***  wfcli         chain     azorg01l
188900140317     c                   eval      ind=%lookup(wfcli:fil)
189000140317     c                   if        ind>0
189100140317     c                   eval      wcarcli=car(ind)
189200140313     c                   if        wcarcli_sav>*zeros and wcarcli_sav<>wcarcli
189300140313     c                   eval      wpfi='S'
189400140313     c                   endif
189500140313     c                   eval      wcarcli_sav=wcarcli
189600140317     c                   endif
189700140313     c                   add       1             is
189800140313     c                   enddo
189900140313     c
190000140310     c                   endsr
190100140310     C*------------------------------------------------------------------------*
190200140310     C*  Memorizza in schiera i comodati del cliente padre
190300140310     C*------------------------------------------------------------------------*
190400140310     C     sr_Memcomo    BEGSR
190500140311     c                   if        in>=11
190600140311     c                   eval      stvksal='+'
190700140311     c                   else
190800140411     c                   if        wcomod<>*blanks
190900140411     c                   eval      CmK(in)=wksccomod
191000140415     c                   eval      CmDt(in)=cmddim
191100140415     c                   eval      CmQt(in)=cmdqta
191200140415     c                   eval      CmDs(in)=wdes
191300140310     c                   add       1             in
191400140310     c                   endif
191500140311     c                   endif
191600140310     c                   endsr
191700140311     C*------------------------------------------------------------------------*
191800140311     C*  Imposta stringa dielle filiali di partenza per la stampa
191900140311     C*------------------------------------------------------------------------*
192000140311     C     sr_StringaFilPBEGSR
192100140311     c                   movel     '('           stafilp
192200140311     c                   z-add     1             i
192300140311     c                   dow       i<=%elem(svfil) and svfil(i)>*zeros
192400140311     c                   if        i>1
192500140311     c                   eval      stafilp=%trim(stafilp)+'/'
192600140311     c                   endif
192700140311     c                   if        i<11
192800140311     c                   eval      stafilp=%trim(stafilp)+%editc(svfil(i):'X')
192900140311     c                   else
193000140311     c                   eval      stafilp=%trim(stafilp)+'...'
193100140311     c                   endif
193200140311     c                   add       1             i
193300140311     c                   enddo
193400140311     c                   eval      stafilp=%trim(stafilp)+')'
193500140311     c                   eval      savfilp=stafilp
193600140311     c                   endsr
193700140311     C*------------------------------------------------------------------------*
193800140311     C*  Stampa dalla seconda riga in poi per comodati/fiiliali di partenza
193900140311     C*------------------------------------------------------------------------*
194000140311     C     sr_AltraRiga  BEGSR
194100140311IF  3C     *INOF         IFEQ      *ON
194200140311     C  N14              EXCEPT    TESTA
194300140311     C   14              EXCEPT    TESTAT
194400140311    2c                   if        savleg='S'
194500140311     c   14              except    riga2hi
194600140311     c  n14              except    rigahi
194700140311    2c                   endif
194800140311     c                   eval      stafilp=savfilp
194900140311     C   14              EXCEPT    RIGA2bof
195000140311     c  n14              except    RIGA1of
195100140311     c                   else
195200140311     C   14              EXCEPT    RIGA2b
195300140311     c  n14              except    RIGA1
195400140311     c                   clear                   stafilp
195500140311E   3C                   ENDIF
195600140311     c                   endsr
195700140311     C*------------------------------------------------------------------------*
195800140311     C*  memorizza in schiera le filiali di partenza bolle
195900140311     C*------------------------------------------------------------------------*
196000140311     C     sr_memosfil   BEGSR
196100140317     c* solo se la linea di partenza bolla <> dalla filiale cliente
196200140317     c                   if        stpfip<>taslnp
196300140317     c* solo se l'area della linea di partenza bolla <> dall'area della filiale cliente
196400140317     c                   eval      ind=%lookup(taslnp:fil)
196500140317     c                   if        ind>0 and car(ind)<>stpas4
196600140317     c* se non è bolla figlia
196700140311     c     keylbl        setll     fnlbl01l
196800140311     c                   if        not %equal(fnlbl01l)
196900140311     c* devo memorizzare la linea di partenza della bolla
197000140311     c                   if        %lookup(taslnp:sfil)=0
197100140311     c                   eval      i=%lookup(0:sfil)
197200140311     c                   if        i>0
197300140311     c                   if        i=11
197400140311     c                   z-add     999           sfil(I)
197500140311     c                   else
197600140311     c                   z-add     taslnp        sfil(i)
197700140311     c                   endif
197800140311     c                   endif
197900140311     c                   endif
198000140311     c                   endif
198100140317     c                   endif
198200140317     c                   endif
198300140311     c                   endsr
198400140410     C*------------------------------------------------------------------------*
198500140410     C*  Recupera comodato cliente per la stampa / WFSTV
198600140410     C*------------------------------------------------------------------------*
198700140410     C     Sr_Comodato   BEGSR
198800140411     c                   clear                   wcomod
198900140415     c                   clear                   wf
199000140411     c     wksccomod     setll     uncmd01l
199100140411     c     wksccomod     reade     uncmd01l
199200140415     c                   dow       not %eof(uncmd01l)
199300140410     c                   if        cmdcfg<>'A' and cmdcfg<>'C'
199400140411     c* - decodifica del tipo/modello da tabella UNR
199500140411     c                   exsr      ric_dunr
199600140415     c                   if        wcomod<>*blanks
199700140415     c                   if        wf=*blanks
199800140415     c                   eval      wcomod = %trim(wcomod) + ' + Altri'
199900140415     c                   eval      wf='1'
200000140415     c                   endif
200100140415     c                   else
200200140411     c* compongo stringa da stampare/memorizzare come segue
200300140411     c* data immissione nel formato gg/mm
200400140411     c                   eval      datadmy=%date(cmddim:*iso)
200500140411     c                   eval      wgmy=%dec(datadmy)
200600140411     c                   movel     wgmy          w004a
200700140415     c                   eval      wcomod=%trim(wcomod) +
200800140411     c                             %subst(w004a:1:2) + '/' +
200900140411     c                             %subst(w004a:3:2) + ':'
201000140411     c* - quantità + decodifica modello
201100140415     c                   eval      wcomod=%trim(wcomod) +
201200140415     c                             %trim(%editc(cmdqta:'Z')) + ' ' +
201300140415     c                             %trim(%subst(wdes:1:21))
201400140415     c                   endif
201500140415     c* memorizzo il comodato nel file WFSTV
201600140415     c                   exsr      sr_memcomo
201700140410     c                   endif
201800140411     c     wksccomod     reade     uncmd01l
201900140410     c                   enddo
202000140410     c                   endsr
202100140411     C*------------------------------------------------------------------------*
202200140411     C*  Ricerca decodifica tipo/modello  - tab UNR
202300140411     C*------------------------------------------------------------------------*
202400140411     c     ric_dunr      begsr
202500140411      /free
202600140411       // Tipo/modello e relativa decodifica
202700140411         clear dunr;
202800140411         clear wdes;
202900140411         clear tibs02ds  ;
203000140411         t02tla=' '      ;
203100140411         t02mod='C'      ;
203200140411         t02sif=knsif    ;
203300140411         t02cod='UNR'    ;
203400140411         t02ke1=cmdtip   ;
203500140411         TNTBE_RicercaControllo  (kpjba : tibs02ds);
203600140411
203700140411         if  t02err=*blanks ;
203800140411            dunr=t02uni;
203900140411            wdes   = D§UNRDES;
204000140411         ELSE;
204100140411            wdes   = *ALL'?' ;
204200140411         endif;
204300140411      /end-free
204400140411     c                   endsr
204500000000     C*------------------------------------------------------------------------*
204600010626     C* *§NZSR - IMPOSTAZIONI INIZIALI
204700000000     C*------------------------------------------------------------------------*
204800000000     C     *INZSR        BEGSR
204900000000     C*--------------------
205000000000     C* RICEVIMENTO PARAMETRI
205100000000     C*--------------------
205200000000     C     *ENTRY        PLIST
205300000000     C                   PARM                    KPJBA
205400000000     C                   MOVEL     KPJBU         PARAM
205500970131     C     PARPAR        COMP      'V'                                    70    SOLO VAS
205600970131     C     PARPAR        COMP      'N'                                    71    SOLO NO VAS
205700980606     C     PARPAR        COMP      'B'                                    72    SOLO VAS B
205800000000     C*--------------------
205900000000     C* VARIABILI RIFERITE AL DATABASE
206000000000     C*--------------------
206100000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
206200000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE                         *TABEL00F
206300000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
206400970130     C     *LIKE         DEFINE    TBLKEY        KTBKEY
206500110401     C**** *LIKE         DEFINE    VSSISV        KVSISV
206600000000     C*--------------------
206700000000     C* CHIAVI DI LETTURA
206800000000     C*--------------------
206900000000     C* LETTURA AZORG01L - COMPLETA
207000000000     C     KEYORG        KLIST
207100000000     C                   KFLD                    KORFIL
207200000322     C* LETTURA TIVSS04L - PARZIALE
207300110401     C*    KEYVSS        KLIST
207400110401     C*                  KFLD                    KVSISV
207500000000     C* LETTURA TABEL00F - PARZIALE
207600000000     C     KEYTAB        KLIST
207700000000     C                   KFLD                    KTBUTE
207800000000     C                   KFLD                    KTBCOD
207900970130     C* LETTURA TABEL00F - TOTALE
208000970130     C     KEYTAC        KLIST
208100970130     C                   KFLD                    KTBUTE
208200970130     C                   KFLD                    KTBCOD
208300970130     C                   KFLD                    KTBKEY
208400990810     C* LETTURA TITAS28C - PARZIALE
208500000000     C     KEYTAS        KLIST
208600000000     C                   KFLD                    TASAAS
208700000000     C                   KFLD                    TASMGS
208800000000     C* LETTURA WFSTP01L - COMPLETA
208900000000     C     KEYSTP        KLIST
209000000000     C                   KFLD                    STPKSC
209100000000     C                   KFLD                    STPDAT
209200000000     C* LETTURA WFSTP01L - COMPLETA
209300000000     C     KEYST2        KLIST
209400000000     C                   KFLD                    STPAS4
209500970130     C                   KFLD                    STPFIP
209600000000     C                   KFLD                    STPDCL
209700000000     C                   KFLD                    STPKSC
209800000000     C                   KFLD                    STPDAT
209900140310     C* Legami bolla
210000140310     C     KEYlbl        KLIST
210100140310     C                   KFLD                    TASAAS
210200140310     C                   KFLD                    TASlnp
210300140310     C                   KFLD                    TASnrs
210400140310     C                   KFLD                    TASnsp
210500000321     C*
210600010627     C* CONTROLLO SE SELEZIONATO CLIENTI "B" CON COMODATO
210700010627     C                   IF        PARCOM = 'S'
210800010627     C                   EVAL      *IN14 = *ON
210900010627     C                   ENDIF
211000010627     c*
211100000321     C* PULISCE SE NON IMPOSTATO A SOLO STAMPA E APRE IL FILE DI LAVORO
211200000321     C     PARSTE        IFEQ      ' '
211300000321     C                   Z-ADD     55            LUNG             15 5
211400000321     C                   MOVEL     *BLANKS       COMMAN
211500000321     C                   MOVEA     CMD1(1)       COMMAN           80
211600000322     C                   CALL      'QCMDEXC'
211700000322     C                   PARM                    COMMAN
211800000322     C                   PARM                    LUNG
211900000321     C                   ENDIF
212000000000     C*--------------------
212100000000     C* VARIABILI DI CONTROLLO
212200000000     C*--------------------
212300000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
212400000000     C                   MOVEL     'N'           WRECOK            1            *VALIDITA' RECORD
212500000000     C                   Z-ADD     *ZEROS        CONLET            9 0          *N° RECORD LETTI
212600000000     C*
212700980116     C* PULISCE SE NON IMPOSTATO A SOLO STAMPA E APRE IL FILE DI LAVORO
212800970130     C     PARSTE        IFEQ      ' '
212900000316     C*
213000000316     C* ESEGUE ROUTINE DI CANCELLAZIONE DEL PERIODO RICHIESTO
213100000322     C                   EXSR      CANWFS
213200000316     C                   ENDIF
213300000316     C*
213400000000     C                   OPEN      WFSTP01L
213500000000     C                   OPEN      WFSTP02L
213600110817     C*
213700110817     c* Pulizia del file di work contenente i dati che vengono stampati
213800110817     C                   Z-ADD     55            LUNG             15 5
213900110817     C                   MOVEL     *BLANKS       COMMAN
214000110817     C                   MOVEA     CMD1(2)       COMMAN           80
214100110817     C                   CALL      'QCMDEXC'
214200110817     C                   PARM                    COMMAN
214300110817     C                   PARM                    LUNG
214400110817
214500110817     C                   OPEN      WFSTV00F
214600000000     C*
214700000000     C* INVERTE LE DATE DI LANCIO IN G/M/AA PER LA STAMPA
214800000000     C                   Z-ADD     PARDTI        DSDAT6
214900000000     C                   Z-ADD     DSANN         DTANN
215000000000     C                   Z-ADD     DSMES         DTMES
215100000000     C                   Z-ADD     DSGIO         DTGIO
215200000000     C                   Z-ADD     DTDAT6        STADTI            6 0
215300000000     C                   Z-ADD     PARDTF        DSDAT6
215400000000     C                   Z-ADD     DSANN         DTANN
215500000000     C                   Z-ADD     DSMES         DTMES
215600000000     C                   Z-ADD     DSGIO         DTGIO
215700000000     C                   Z-ADD     DTDAT6        STADTF            6 0
215800000000     C*
215900000000     C* IMPOSTA DATA INIZIO E FINE DA PARAMETRI
216000000000     C                   Z-ADD     PARDTI        DSDAT6
216100000000     C                   Z-ADD     19            DSSEC
216200000000     C                   Z-ADD     DSAAS         INIAAS            4 0
216300000000     C                   Z-ADD     DSMGS         INIMGS            4 0
216400000000     C                   Z-ADD     PARDTF        DSDAT6
216500000000     C                   Z-ADD     19            DSSEC
216600000000     C                   Z-ADD     DSAAS         FINAAS            4 0
216700000000     C                   Z-ADD     DSMGS         FINMGS            4 0
216800971128     C*
216900971128     C* CALCOLA LA DATA CORRENTE
217000971128     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
217100971128     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
217200971128     C                   Z-ADD     WN8           DATA              8 0
217300971128     C                   Z-ADD     WN8           G08DAT
217400971128     C                   Z-ADD     *ZEROS        G08INV
217500971128     C                   MOVEL     '0'           G08ERR
217600971128     C                   CALL      'XSRDA8'
217700971128     C                   PARM                    WLBDA8
217800971128     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
217900000000     C*
218000000000     C* CARICA LE TABELLE
218100000000     C                   EXSR      CARTAB
218200000000     C*
218300000000     C                   ENDSR
218400000000     O*------------------------------------------------------------------------*
218500000000     O* RIGHE DI STAMPA
218600000000     O*------------------------------------------------------------------------*
218700970131     OPRTF198   E            TESTA          1 02
218800980131     O                       KNSIF               10
218900970919     O                                           32 'CLIENTI CON UNA MEDIA'
219000970919     O                                           52 'A RITIRO DI PIU'' DI'
219100970919     O                                           64 'SPEDIZIONI:'
219200970919     O                       PARNBO        Z     67
219300970919     O                                           76 'O COLLI:'
219400970919     O                       PARNCO        Z     79
219500970919     O                                           99 'O CON "VAS  BOLLE"'
219600970919     O                                          110 'DAL:'
219700970919     O                       STADTI             119 '  /  /  '
219800970919     O                                          130 'AL:'
219900970919     O                       STADTF             139 '  /  /  '
220000000000     O                                          150 'Data:'
220100971128     O                       DATA               161 '  /  /    '
220200980131     O                                          180 'TNSS21R '
220300000000     O                                          190 'Pagina:'
220400000000     O                       PAGE          Z    198
220500970131     O          E            TESTA          1
220600970131     O               70                          35 'STAMPA SOLO I CLIENTI'
220700970131     O               70                          57 'CON VAS "BOLLE"      '
220800970131     O               71                          35 'STAMPA SOLO I CLIENTI'
220900970131     O               71                          57 'SENZA VAS "BOLLE"    '
221000980606     O               72                          35 'STAMPA SOLO I CLIENTI'
221100980606     O               72                          57 'CON VAS "BOLLE" B/B2 '
221200970131     O          E            TESTA          1
221300970131     O                       TES(1)              66
221400970130     O                       TES(1)             132
221500970130     O                       TES(1)             198
221600970130     O          E            TESTA          1
221700000000     O                       TES(2)              66
221800000000     O                       TES(3)             132
221900000000     O                       TES(4)             198
222000970130     O          E            TESTA          1
222100970130     O                       TES(5)              66
222200970130     O                       TES(6)             132
222300970130     O                       TES(7)             198
222400000000     O          E            TESTA          1
222500000000     O                       TES(1)              66
222600000000     O                       TES(1)             132
222700000000     O                       TES(1)             198
222800000000     O*------------------------------------
222900000000     O          E            SPAZIO         1
223000000000     O                                            3 ' '
223100000000     O*------------------------------------
223200000710     O          E            FIP            1
223300110325     O****                                       23 'TERMINAL/P.O. PARTENZA:'
223400110401     O                                           22 'AREA/FILIALE PARTENZA:'
223500970205     O                       DESTRP            +  1
223600970205     O                                         +  1 '/'
223700990225     O                       WORFIL            +  1
223800970205     O                       DESFIP            +  1
223900000000     O*------------------------------------
224000140304     O          E            RIGAHI         0
224100140304     O                       DEPKSC               7
224200000000     O          E            RIGA           1
224300000000     O                       DEPKSC               7
224400000000     O                       DESCLI            +  1
224500140311     O                       savpfi            +  2
224600140311     O                       STASPT        Z   +  2
224700970129     O                       STACOT        Z   +  1
224800970129     O                       STAGIO        Z   +  1
224900970129     O                       STASPG        Z   +  1
225000970129     O                       STACOG        Z   +  1
225100970129     O                       STASPA        Z   +  1
225200970129     O                       STASPB        Z   +  1
225300010423     O                       STASPK        Z   +  1
225400110817     O****                   STASPH        Z   +  1
225500140228     O****                   STADET            +  2
225600140228     O****                   STACAP            +  2
225700971021     O                       STAVAB            +  2
225800990225     O                       STAPES            +  2
225900140228     O***                    STACBA            +  1
226000140228     O****                   STASPC        Z   +  1
226100140228     O                       stacass           +  2
226200140611     O                       stagia            +  2
226300140611     O                       stadisp           +  3
226400140304     O                       stafatt           +  3
226500140304     O                       stapod            +  2
226600110401     O***                    STATRK            +  4
226700140306     O*                      §3RNOT            +  3
226800140611     o                       staalr        z   +  1
226900140611     O                       WINDSD            +  1
227000140228     O*****                  STABAC            +  4
227100140611     o                       wksccomod     z   +  2
227200140411     O                       wcomod             198
227300010419     O*------------------------------------
227400010419     O          E            RIGA1          1
227500140311     o                       stafilp           +  8
227600140411     o                       wksccomod     z    157
227700140411     O                       wcomod             198
227800140305     O          E            RIGA1OF        1
227900140305     O                       DEPKSC               7
228000140311     O                       stafilp           +  1
228100140411     o                       wksccomod     z    157
228200140411     O                       wcomod             198
228300000316     O*------------------------------------
228400000316     O          E            TFIL        2  1
228500110401     O                                           12 'TOTALI FIL.:'
228600010423     O                                         +  1 'TUTTE LE SPEDIZIONI   :'
228700010423     O                                         +  2 'SPEDIZ.:'
228800000316     O                       STTSPT        2   +  0
228900010423     O                                         +  2 'COLLI:'
229000000316     O                       STTCOT        2   +  0
229100010423     O                                         +  2 'CLIENTI:'
229200000316     O                       STTCLI        2   +  0
229300010423     O                                         +  2 'CLI CON DSK A:'
229400000316     O                       STTCLA        2   +  0
229500010423     O                                         +  2 'CLI CON DSK B:'
229600000316     O                       STTCLB        2   +  0
229700010423     O                                         +  2 'CLI CON DSK C CLI:'
229800010423     O                       STTCLC        2   +  0
229900010423     O                                         +  2 '% COLLI "B":'
230000000316     O                       WPETI         2   +  0
230100010423     O                                         +  2 '% SPED. DSK:'
230200000316     O                       WPDSK         2   +  0
230300000316     O          E            TFIL           1
230400000316     O                                           12 '            '
230500010423     O                                         +  1 'SOLO SPED."POTENZIALI":'
230600010423     O                                         +  2 'SPEDIZ.:'
230700000316     O                       S2TSPT        2   +  0
230800010423     O                                         +  2 'COLLI:'
230900000316     O                       S2TCOT        2   +  0
231000010423     O                                         +  2 'CLIENTI:'
231100000316     O                       S2TCLI        2   +  0
231200010423     O                                         +  2 'CLI CON DSK A:'
231300000316     O                       S2TCLA        2   +  0
231400010423     O                                         +  2 'CLI CON DSK B:'
231500000316     O                       S2TCLB        2   +  0
231600010423     O                                         +  2 'CLI CON DSK C CLI:'
231700010423     O                       S2TCLC        2   +  0
231800010423     O                                         +  2 '% COLLI "B":'
231900000316     O                       W2ETI         2   +  0
232000010423     O                                         +  2 '% SPED. DSK:'
232100000316     O                       W2DSK         2   +  0
232200000316     O          E            TFIL        1  1
232300000316     O                                           20 '(*)= CODICE BLOCCATO'
232400000316     O          E            TFIL           1
232500000316     O                                           24 'TOTALE CLIENTI CON:     '
232600110624     O                                         +  1 'TRACCIATO VAB/VAT *STD ='
232700000316     O                       WBLCF         2   +  1
232800140303     O*                                        +  3 'CAPPARIO 1='
232900140303     O*                      WCAPVF        2   +  1
233000140303     O*                                        +  3 'CAPPARIO 2='
233100140303     O*                      WCAPNF        2   +  1
233200140303     O*                                        +  3 'CAPPARIO 3='
233300140303     O*                      WCAPLF        2   +  1
233400110401     O***                                      +  3 'T&T='
233500110401     O***                    WCATRF        2   +  1
233600000316     O*------------------------------------
233700000316     O          E            STAFIN         2
233800000316     O                                           12 'TOTALI     :'
233900010423     O                                         +  1 'TUTTE LE SPEDIZIONI   :'
234000010423     O                                         +  2 'SPEDIZ.:'
234100000316     O                       STFSPT        2   +  0
234200010423     O                                         +  2 'COLLI:'
234300000316     O                       STFCOT        2   +  0
234400010423     O                                         +  2 'CLIENTI:'
234500000316     O                       STFCLI        2   +  0
234600010423     O                                         +  2 'CLI CON DSK A:'
234700000316     O                       STFCLA        2   +  0
234800010423     O                                         +  2 'CLI CON DSK B:'
234900000316     O                       STFCLB        2   +  0
235000010423     O                                         +  2 'CLI CON DSK C CLI:'
235100010423     O                       STFCLC        2   +  0
235200010423     O                                         +  2 '% COLLI "B":'
235300000316     O                       WPETI         2   +  0
235400010423     O                                         +  2 '% SPED. DSK:'
235500000316     O                       WPDSK         2   +  0
235600000316     O          E            STAFIN         3
235700000316     O                                           12 '            '
235800010423     O                                         +  1 'SOLO SPED."POTENZIALI":'
235900010423     O                                         +  2 'SPEDIZ.:'
236000000316     O                       S2FSPT        2   +  0
236100010423     O                                         +  2 'COLLI:'
236200000316     O                       S2FCOT        2   +  0
236300010423     O                                         +  2 'CLIENTI:'
236400000316     O                       S2FCLI        2   +  0
236500010423     O                                         +  2 'CLI CON DSK A:'
236600000316     O                       S2FCLA        2   +  0
236700010423     O                                         +  2 'CLI CON DSK B:'
236800000316     O                       S2FCLB        2   +  0
236900010423     O                                         +  2 'CLI CON DSK C CLI:'
237000010423     O                       S2FCLC        2   +  0
237100010423     O                                         +  2 '% COLLI "B":'
237200000316     O                       W2ETI         2   +  0
237300010423     O                                         +  2 '% SPED. DSK:'
237400000316     O                       W2DSK         2   +  0
237500000316     O          E            STAFIN      2  2
237600000316     O                                           24 'TOTALE CLIENTI CON:     '
237700110624     O                                         +  1 'TRACCIATO VAB/VAT *STD ='
237800000316     O                       WBLCT         2   +  1
237900140303     O**                                       +  3 'CAPPARIO 1='
238000140303     O**                     WCAPVT        2   +  1
238100140303     O**                                       +  3 'CAPPARIO 2='
238200140303     O**                     WCAPNT        2   +  1
238300140303     O***                                      +  3 'CAPPARIO 3='
238400140303     O***                    WCAPLT        2   +  1
238500110401     O****                                     +  3 'T&T='
238600110401     O****                   WCATRT        2   +  1
238700140304     O*         E            STAFIN         1
238800140304     O*                                          24 'TOTALI CLIENTI PER TIPO '
238900140304     O*                                        +  1 'SUPPORTO VAS BOLLE'
239000140304     O*         E            STAFIN         1
239100140304     O*                                          11 'TIPO NUMERO'
239200140304     O*         E            STAFI2         1
239300140304     O*                      TIB(I)               5
239400140304     O*                      NIB(I)        2   +  1
239500010626     O*------------------------------------------------------------------------*
239600010626     O* RIGHE DI STAMPA QSYSPRT
239700010626     O*------------------------------------------------------------------------*
239800010626     OQSYSPRT   E            TESTAT         1 02
239900010626     O                       KNSIF               10
240000010626     O                                           32 'CLIENTI CON UNA MEDIA'
240100010626     O                                           52 'A RITIRO DI PIU'' DI'
240200010626     O                                           64 'SPEDIZIONI:'
240300010626     O                       PARNBO        Z     67
240400010626     O                                           76 'O COLLI:'
240500010626     O                       PARNCO        Z     79
240600010626     O                                           99 'O CON "VAS  BOLLE"'
240700010626     O                                          105 'DAL:'
240800010626     O                       STADTI             114 '  /  /  '
240900010626     O                                          120 'AL:'
241000010626     O                       STADTF             129 '  /  /  '
241100010626     O          E            TESTAT         1
241200010626     O               70                          35 'STAMPA SOLO I CLIENTI'
241300010626     O               70                          57 'CON VAS "BOLLE"      '
241400010626     O               71                          35 'STAMPA SOLO I CLIENTI'
241500010626     O               71                          57 'SENZA VAS "BOLLE"    '
241600010626     O               72                          35 'STAMPA SOLO I CLIENTI'
241700010626     O               72                          57 'CON VAS "BOLLE" B/B2 '
241800010626     O               14                          80 'E MATERIALE IN COMODATO'
241900010626     O                                          100 'Data:'
242000010626     O                       DATA               111 '  /  /    '
242100010626     O                                          121 'TNSS21R'
242200010626     O                                          128 'Pag.'
242300010626     O                       PAGE          Z    132
242400010626     O          E            TESTAT         1
242500010626     O                       TE1(1)              66
242600010626     O                       TE1(1)             132
242700010626     O          E            TESTAT         1
242800010626     O                       TE1(2)              66
242900010626     O                       TE1(3)             132
243000010626     O          E            TESTAT         1
243100010626     O                       TE1(4)              66
243200010626     O                       TE1(5)             132
243300010626     O          E            TESTAT         1
243400010626     O                       TE1(1)              66
243500010626     O                       TE1(1)             132
243600010626     O*------------------------------------
243700010626     O          E            SPAZIO1        1
243800010626     O                                            3 ' '
243900010626     O*------------------------------------
244000010626     O          E            FIP1           1
244100110325     O***                                        23 'TERMINAL/P.O. PARTENZA:'
244200110401     O                                           22 'AREA/FILIALE PARTENZA:'
244300010626     O                       DESTRP            +  1
244400010626     O                                         +  1 '/'
244500010626     O                       WORFIL            +  1
244600010626     O                       DESFIP            +  1
244700010626     O*------------------------------------
244800140304     O          E            RIGA2HI        0
244900140304     O                       DEPKSC
245000010626     O          E            RIGA2          1
245100070626     O                       DEPKSC
245200070626     O                       DESCLI             + 1
245300140311     O                       savpfi             + 2
245400140311     O                       STASPT        Z    + 3
245500070626     O                       STASPA        Z    + 3
245600070626     O                       STASPB        Z    + 1
245700070626     O                       STASPK        Z    + 1
245800110817     O***                    STASPH        Z    + 1
245900140611     o                       wksccomod     z    + 1
246000140611     O                       wcomod             + 1
246100140228     O          E            RIGA2b         1
246200140311     o                       stafilp            + 8
246300140611     o                       wksccomod     z     89
246400140611     O                       wcomod             + 1
246500140305     O          E            RIGA2bOf       1
246600140305     O                       DEPKSC
246700140311     O                       stafilp            + 1
246800140611     o                       wksccomod     z     89
246900140611     O                       wcomod             + 1
247000070626      *
247100000321** CMD1 - COMANDI CLP
247200000321CLRPFM FILE(*LIBL/WFSTP00F)
247300110817CLRPFM FILE(*LIBL/WFSTV00F)
247400970130** TES  - RIGHE DI TESTATA
247500000316------------------------------------------------------------------  1
247600140321CLIENTE                                Cli  TOTALE    TOTALE  GG    2
247700140611 SPEDIZ.    COLLI ---- SPEDIZIONI DISK ----- TR PE -- S U P P O R   3
247800140611T O -- TOTALE MM  MATERIALE IN COMODATO                             4
247900140404        Partenza Bolle Altre Aree(xxx) aAr  SPEDIZ.    COLLI  RIT   52
248000140611X GIORNO X GIORNO      "A"       "B" "C" CLI SP SO C/A GIA DISP FA  63
248100140611TT POD  ALERT DK  Codice   Descrizione                              74
248200970130                                                                    8
248300010626** TE1  - RIGHE DI TESTATA QSYSPRT
248400010626------------------------------------------------------------------  1
248500140321CLIENTE                            ABL Cli   TOTALE    ---- SPEDIZ  2
248600140311IONI  DISK ---- MATERIALE IN COMODATO                               3
248700140404        Part.Bolle Altre Aree(xxx) (*) aAr   SPEDIZ.        "A"     4
248800140611   "B"   "C"CLI Codice  Descrizione                                 5
