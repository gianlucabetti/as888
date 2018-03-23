000100990519     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200990519     H*PARMS ACTGRP(QILE)
000300990203     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000400000000     F* OFN100R  *---------------------------------------------------*
000500000000     F*----------*                                                   *
000600990203     F*          - IMMISSIONE/REVISIONE ORDINI FORNITORI -           *
000700000000     F*          -----------------------------------------           *
000800000000     F*                                                    AZ.: N100 *
000900000000     F*                                                              *
001000990203     F*         FILE : OFN100D  - tmmof00F - tmmof01L - tmfop01L     *
001100000000     F*                CNACO00F - CNIND00F - ANTAB00F - TABEL00F     *
001200990203     F*                tmlmp01L - tmamp01L - tmsmp01L                *
001300990203     F*                         - BFTAB00F                           *
001400000000     F*                                                              *
001500000000     F*         PGM. : OFN800R - LANCIO STAMPA ORDINI                *
001600990317     F*                tii006R - INTERROG.PARTICOLARITA'FORNITORI    *
001700990317     F*                tii103R - RICERCA ALFABETICA FORNITORI        *
001800990317     F*                tii104R - RICERCA ALFABETICA MATERIE PRIME    *
001900990317     F*                tii105R - RICERCA PAGAMENTI                   *
002000990317     F*                tii107R - RICERCA SEDI DI SPEDIZIONI          *
002100990317     F*                tii108R - RICERCA MAGAZZINI                   *
002200990317     F*                TII113R - RICERCA IMBALLI                     *
002300990317     F*                TII114R - RICERCA GRUPPI MERCEOLOGICI         *
002400990317     F*                TII115R - RICERCA SETTORI MERCEOLOGICI        *
002500990317     F*                TII116R - RICERCA LISTINI MATERIE PRIME       *
002600990317     F*                TII117R - RICERCA TIPOLOGIE ORDINI            *
002700990317     F*                TII202R - RICERCA DISPONIBILITA' M.P.         *
002800860522     F*--------------------------------------------------------------*
002900000000     F* TIPO RIGA :                                                  *
003000000000     F*------------                                                  *
003100000000     F*              A - R I G A    A N N U L L A T A                *
003200880104CNF  F*              1 - RIGA ACQUISTO                               *
003300000000     F*              2 - RIGA CONTO LAVORAZIONE                      *
003400000000     F*              3 - RIGA PRODUZIONE                             *
003500000000     F*------------                                                  *
003600000000     F*              D - RIGA DI ADDEBITO  AL FORNITORE              *
003700000000     F*              C - RIGA DI ACCREDITO AL FORNITORE              *
003800000000     F*              I - RIGA DI I.V.A.                              *
003900000000     F*              S - RIGA DI SPESE                               *
004000000000     F*------------                                                  *
004100000000     F*              6 - RIGA COMMENTO PER FORNITORE                 *
004200000000     F*              7 - RIGA COMMENTO PER USO INTERNO               *
004300000000     F*              8 - RIGA COMMENTO PER MAGAZZINO                 *
004400000000     F*              9 - RIGA COMMENTO GENERICO                      *
004500000000     F*--------------------------------------------------------------*
004600000000     F* FASE RIGA :                                                  *
004700000000     F*------------                                                  *
004800000000     F*             OR - IN ORDINE          (T.R.= 1-2-6-7-8-9)      *
004900000000     F*             BB - BOLLETTATA         (T.R.= TUTTE)            *
005000000000     F*             FA - FATTURATA          (T.R.= TUTTE)            *
005100000000     F*--------------------------------------------------------------*
005200990317     FTII400D   CF   E             WORKSTN
005300000000     F                                     SFILE(SFL1:REC1)
005400000000     F                                     SFILE(SFL2:REC2)
005500990203     Ftmmof00F  UF A E             DISK
005600990203     F                                     RENAME(tmmof000:ORDFIS)
005700000000     F                                     INFDS(INF2)
005800990203     Ftmmof01L  UF   E           K DISK
005900990203     F                                     RENAME(tmmof000:ORDRIG)
006000000000     F                                     INFDS(INF1)
006100990203     Ftmfop01L  IF   E           K DISK
006200000000     FANTAB00F  IF   E           K DISK
006300990203     Ftmlmp01L  IF   E           K DISK
006400990203     Ftmamp01L  IF   E           K DISK
006500990203     Ftmsmp01L  UF A E           K DISK
006600990203     Ftmcnf01L  UF A E           K DISK
006700990203     Ftmsfc01L  IF   E           K DISK
006800860522     F****************************************************************
006900990203RIC  Fanfrn01l  IF   E           K DISK
007000990203RIC  Fanind01l  IF   E           K DISK
007100990203RIC  Fansog01l  IF   E           K DISK
007200990203RIC  Fanent01l  IF   E           K DISK
007300860522RIC  FTABEL00F  IF   E           K DISK
007400010725     FTNTBE01l  IF   E           k disk
007500860522     F*
007600860522     F****************************************************************
007700000000     D ORD             S             23    DIM(3) CTDATA PERRCD(1)              * SEGNALAZIONI
007800000000     D ERR             S             40    DIM(15) CTDATA PERRCD(1)             * ERRORI
007900871106     D MPHDT1          DS           512
008000871106     D*-----DATA AREA NUMERATORE MOVIM. MAGAZZINO
008100871106     D  NUMDTA                 1      5  0
008200871106     D  FILL1                  6      8
008300871106     D  DATDTA                 9     14  0
008400871106     D  FILL2                 15     17
008500871106     D  ANNDTA                18     19  0
008600871106     D  FIL03                 20     50
008700871106     D  DGIO                  51     56  0
008800871106     D  FIL04                 57    100
008900871106     D  FILL5                101    300
009000871106     D  GIOTOL               301    302  0
009100871106     D  FILL6                303    512
009200000000     D DTAORF          DS
009300000000     D*--- DATA AREA NUMERAZIONE ORDINI :
009400000000     D  ORDNUO                 1      5  0
009500860522     D OFNDT1          DS
009600860522     D*--- DATA AREA ORDINI FORNITORI   :
009700860522     D  F1KKKK                 1     50
009800860523     D  MASTE1                51     51
009900860523     D  MASTE2                52     52
010000860523     D  MASTE3                53     53
010100860523     D  MASTER                51     53
010200860522     D  F2KKKK                54    256
010300860522     D  F3KKKK               257    512
010400860523     D                 DS
010500860523     D*--- SCOMPOSIZIONE MASTER PROCEDURA :
010600860523     D  EDP1                   1      1
010700860523     D  EDP2                   2      2
010800860523     D  EDP3                   3      3
010900940328     D  EDP                    1      6
011000000000     D INF1            DS           512
011100000000     D*--- RELREC LOGICO
011200000000     D  NREL1                397    400B 0
011300000000     D INF2            DS           512
011400000000     D*--- RELREC FISICO
011500000000     D  NREL2                397    400B 0
011600011001      *Richiamo driver DVCDP
011700011001     D DviCdpDS      E DS
011800011001     D                                     EXTNAME(DvCdpDS)
011900011001     D                                     INZ
012000011001     D DvoCdpDS      E DS
012100011001     D                                     INZ
012200860522RIC  D****************************************************************
012300860522RIC  D PAGAM           DS
012400860522RIC  D*------------- KEY PAGAMENTI PER TABEL00F X PAGAMENTI :
012500860522RIC  D  PAGVID                 1      3
012600860522RIC  D  LINGUA                 4      4
012700860522RIC  D  BLANCO                 5      8
012800860522RIC  D  KEYTBL                 1      8
012900000000     D PAR820          DS
013000860522     D*------------- PARAMETRI PER INTERROGAZIONE FORNITORE :
013100000000     D  FOR820                 1      7
013200000000     D  RAG820                 8     37
013300000000     D  FIL820                38    255
013400000000     D  RIC820               256    256
013500000000     D DESAPP        E DS                  EXTNAME(ANTAB157)
013600860522     D*------------- TABELLA CLASSI APPROVVIGIONAMENTO :
013700000000     D DESDEM        E DS                  EXTNAME(ANTAB028)
013800860522     D*------------- TABELLA DESTINAZIONE MERCE :
013900000000     D DSORD         E DS                  EXTNAME(ANTAB162)
014000860522     D*------------- TABELLA TIPOLOGIE ORDINI :
014100000000     D DSMAG         E DS                  EXTNAME(ANTAB153)
014200860522     D*------------- TABELLA MAGAZZINI :
014300000000     D PAR800          DS
014400860522     D*------------- PARAMETRI PER STAMPA ORDINE :
014500990203     D  FOR800                 1      8
014600990203     D  RAG800                 9     38
014700990203     D  ORD800                39     43  0
014800990203     D  RIG800                44     44
014900990203     D  STA800                45     46
015000000000     D PAR8A0          DS
015100860522     D*------------- PARAMETRI PER INTERROGAZIONE LISTINI
015200000000     D  LIS8A0                 1      3
015300000000     D  CLA8A0                 4      6
015400000000     D  SOT8A0                 7     11
015500000000     D  CAT8A0                12     13
015600000000     D  ART8A0                14     20
015700000000     D  FIL8A0                21    255
015800000000     D  RIC8A0               256    256
015900000000     D PH115           DS
016000860522     D*------------- PARAMETRI PER RICERCA DISPONIBILITA' :
016100990203     D  Anno                   1      4
016200990203     D  Aaa115                 1      4  0
016300990203     D  MAG115                 5     11
016400990203     D  MAT115                12     23
016500990203     D  UMI115                24     24
016600990203     D  SCE115                25     25
016700860210     D KPJBA         E DS
016800860522     D*------------- PASSAGGIO PARAMETRI :
016900990203     D  FOR150               247    254
017000990203     D  FIL150               255    393
017100990203     D  nOR150               394    398  0
017200990203     D  dOR150               399    406  0
017300990203     D  RIC150               442    442
017400990203     D  UTE150               443    452
017500990203     D  MSGERR               453    502
017600990203     D soc001        E DS                  EXTNAME(xsoc001ds)
017700990203     D xsocds          DS          1000
017800010725    >D DGED          E DS
017900990203     d dataiso         s               d   datfmt(*iso)
018000990203     d dataeur         s               d   datfmt(*eur)
018100990310     D* Ricerca divise
018200990310     D A015DS        E DS                  EXTNAME(ANA015DS) INZ
018300990519     D*-------------
018400990519     D Esito           S              1
018500990519     D Datigest        S              1A
018600990519     D NoctrlScos      S              1A
018700990519     D Gesmsg          S              1
018800011001      *Costanti di Lavoro
018900011001     D Chain           C                   '1'
019000011001     D Principale      C                   '1'
019100990519     D*-------------
019200990519     D*  Prototipazione delle procedure
019300990519     D/COPY *LIBL/SRCCPY,PJX002PR
019400000000     C*----------------------------------------------------*
019500000000     C     *ENTRY        PLIST                                                  *
019600000000     C                   PARM                    KPJBA                          *
019700990203     C*---------- RICERCA DITTA :
019800990203     C                   MOVEL     'SOC001'      TIPXSC
019900990203     C                   MOVEL     *blank        SOCXSC
020000990203     C                   EXSR      REPSOC
020100990203     C     RTNXSC        IFNE      '1'
020200990203     C                   MOVEL     XSOCDS        SOC001
020300990203     C                   MOVEL     xscrgs        desdit
020400990203     C                   MOVEL     xscsoc        wsoc              3
020500990203     C                   MOVEL     xscsoc        socpas            3
020600990310     C                   MOVEL     xscdiv        tesval
020700990203     c                   end
020800990203     c                   time                    como14           14 0
020900990203     c                   move      como14        woggi             8 0
021000990203     c                   move      woggi         dataeur
021100990203     c                   move      dataeur       dataiso
021200990203     c                   move      dataiso       woggig            8 0
021300010725     C*--- Reperisco la divisa corrente ------------------------------*
021400010725     C     ktbe          KLIST
021500010725     C                   KFLD                    kcod
021600010725     C                   KFLD                    kke1
021700010725     C     *like         Define    tbecod        kcod
021800010725     C     *like         Define    tbeke1        kke1
021900010725     C                   move      'GED'         kcod
022000010725     C                   move      *blanks       kke1
022100010725     C                   movel     'MAGAZZINO'   kke1
022200010725     c                   clear                   dged
022300010725     C     ktbe          CHAIN     tntbe01l                           53
022400010725     C  n53              movel     tbeuni        dged
022500010725     C  n53              movel     §GEDCN        xscdiv
022600010725     C  n53              movel     §GEDCN        tesval
022700010725     C*                                                    *
022800000000     C     *DTAARA       DEFINE                  DTAORF                         *
022900860522     C     *DTAARA       DEFINE                  OFNDT1                         *
023000871106     C     *DTAARA       DEFINE                  MPHDT1                         *
023100990203     C*--- FILE ORDINI FORNIT. "tmmof01L"                  *
023200000000     C     KEYORD        KLIST                                                  *
023300000000     C                   KFLD                    CODFOR                         *
023400000000     C                   KFLD                    NUMORD                         *
023500860522RIC  C*--- FILES ANAGRAFICI FORNIT. "CNACO00F/CNIND00F"    *
023600860522RIC  C     KEYFOR        KLIST                                                  *
023700990203RIC  C                   KFLD                    wsoc                           *
023800990203RIC  C                   KFLD                    codfor                         *
023900860522RIC  C*--- KEY RICERCA C.D.C. E AGENTE SU "CNACO00F"       *
024000990203RIC  C     KEYCDC        KLIST                                                  *
024100990203RIC  C                   KFLD                    wsoc                           *
024200990203RIC  C                   KFLD                    tipocdc           3            *
024300990203RIC  C                   KFLD                    CCOSTO            8            *
024400990203RIC  C                   move      'CDC'         tipocdc                        *
024500860522RIC  C*--- FILE ANAGRAFICO TABELLE "TABEL00F" X COD. PAGAM.*
024600011001RIC  C*****KEYPAG        KLIST                                                  *
024700011001RIC  C*****              KFLD                    CODUTE            1 0          *
024800011001RIC  C*****              KFLD                    TIPTAB            2            *
024900011001RIC  C*****              KFLD                    KEYTBL                         *
025000011001     c*****              z-add     1             codute
025100860522RIC  C*--- FILE ANAGRAFICO TABELLE "TABEL00F" X CAMBIO VALUTA
025200011001RIC  C*****KEYVAL        KLIST                                                  *
025300011001RIC  C*****              KFLD                    CODUTE            1 0          *
025400011001RIC  C*****              KFLD                    TIPTAB            2            *
025500011001RIC  C*****              KFLD                    VALUTA            8            *
025600860522     C*--- FILE ANAGRAFICO TABELLE "ANTAB00F" -------------*
025700000000     C     KEYANT        KLIST                                                  *
025800000000     C                   KFLD                    TABGRU                         *
025900000000     C                   KFLD                    TABCOD                         *
026000990203     C*--- FILE LISTINI "tmlmp01L"                         *
026100000000     C     KEYLIS        KLIST                                                  *
026200000000     C                   KFLD                    SC2LIS                         *
026300000000     C                   KFLD                    SC2MAT                         *
026400990310     C                   KFLD                    SC2val                         *
026500990203     C*--- FILE MATERIE PRIME "tmamp01L"                   *
026600000000     C     KEYART        KLIST                                                  *
026700000000     C                   KFLD                    SC2MAT                         *
026800990203     C*--- FILE SALDI MAGAZZINO M.P. "tmsmp01L"            *
026900000000     C     KEYSAL        KLIST                                                  *
027000990203     C                   KFLD                    ANNDTA4                        *
027100000000     C                   KFLD                    MOFMAG                         *
027200000000     C                   KFLD                    SC2MAT                         *
027300860522     C     *LIKE         DEFINE    KPJBU         KPJBUW                         *
027400860522     C*---------- RICERCA MASTER --------------------------*
027500860523     C                   MOVEL     KNMUS         EDP                            *
027600860522     C     SUDT1         TAG                                                    *
027700860522     C     *LOCK         IN        OFNDT1                               46      *
027800860522     C   46              GOTO      SUDT1                                        *
027900860522     C                   OUT       OFNDT1                                       *
028000871106     C*----------------
028100871106     C     SUDT2         TAG                                                    *
028200871106     C     *LOCK         IN        MPHDT1                               45      *
028300871106     C   45              GOTO      SUDT2                                        *
028400871106     C                   OUT       MPHDT1                                       *
028500860522RIC  C                   MOVE      '1'           LINGUA                         *
028600860210     C*---- RICHIAMO PROGRAMMA ----------------------------*
028700080205     c                   z-add     anndta        anndta4           4 0
028800080205     c                   if        anndta4 > 40
028900080205     c                   add       1900          anndta4
029000080205     c                   else
029100080205     c                   add       2000          anndta4
029200080205     c                   end
029300860210     C     RIC150        IFEQ      *BLANKS                                      *
029400860210     C                   Z-ADD     0             DOR150                         *
029500860210     C                   Z-ADD     0             NOR150                         *
029600860210     C                   ELSE                                                   *
029700860210     C                   MOVE      FOR150        CODFOR                         *
029800860210     C                   GOTO      RICFOR                                       *FORNIT.
029900860210     C                   END                                                    *
030000000000     C     INIZIO        TAG                                                    *
030100000000     C*          -------------                             *
030200000000     C                   MOVE      *BLANKS       CODULT                         *
030300000000     C                   MOVE      *ZEROS        ORDULT                         *
030400000000     C                   MOVEA     *ZEROS        *IN(1)                         *
030500871007     C                   MOVEL     '?'           INTERR           12            *
030600000000     C     TAG001        TAG                                                    *
030700000000     C*          -------------                             *
030800000000     C                   SETOFF                                       08        *
030900000000     C*--- EMISSIONE TESTATA                               *
031000000000     C                   WRITE     OFN100T1                                     *
031100000000     C*--- EMISSIONE PROMPT RICHIESTA FORNITORE :          *
031200990409     C     ORDULT        COMP      0                                  08        *
031300000000     C                   EXFMT     OFN100PT                                     *
031400000000     C                   MOVEA     *ZEROS        *IN(75)                        *
031500990409     C   Kc              GOTO      FINE                                         *
031600000000     C*--- INTERROGAZIONE ANAGRAFICO PARTICOLARITA' -------*
031700000000     C   KB              DO                                                     *
031800000000     C                   MOVEL     KPJBU         KPJBUW                         *
031900000000     C                   MOVE      CODFOR        FOR820                         *
032000000000     C                   MOVE      NOMF30        RAG820                         *
032100000000     C                   MOVE      'R'           RIC820                         *
032200000000     C                   MOVEL     PAR820        KPJBU                          *
032300990317     C                   CALL      'TII006R'                                    *
032400000000     C                   PARM                    KPJBA                          *
032500000000     C                   MOVEL     KPJBU         PAR820                         *
032600000000     C                   MOVE      FOR820        CODFOR                         *
032700000000     C                   MOVE      RAG820        NOMF30                         *
032800000000     C                   MOVEL     KPJBUW        KPJBU                          *
032900000000     C                   GOTO      TAG001                                       *
033000000000     C                   END                                                    *
033100000000     C*--- LANCIO STAMPA ORDINE ---------------------------*
033200990409     C   Kh              DO                                                     *
033300000000     C                   MOVEL     KPJBU         KPJBUW                         *
033400000000     C                   MOVE      CODULT        FOR800                         *
033500000000     C                   MOVE      NOMF30        RAG800                         *
033600000000     C                   MOVE      ORDULT        ORD800                         *
033700000000     C                   MOVE      'C'           RIG800                         *
033800000000     C                   MOVE      'NO'          STA800                         *
033900000000     C                   MOVE      *BLANKS       KPJBU                          *
034000000000     C                   MOVEL     PAR800        KPJBU                          *
034100990317     C                   CALL      'TII405R'                                    *
034200000000     C                   PARM                    KPJBA                          *
034300000000     C                   MOVEL     KPJBUW        KPJBU                          *
034400000000     C                   MOVE      *BLANKS       CODULT                         *
034500000000     C                   MOVE      *ZEROS        ORDULT                         *
034600000000     C                   GOTO      INIZIO                                       *
034700000000     C                   END                                                    *
034800000000     C*--- RICERCA ALFABETICA PARTICOLARITA' FORNITORE ----*
034900990409     c                   if        codfor = '?      ' or codfor = '0000000?'
035000990409     C*    CODFOR        IFEQ      '?       '                                   *
035100990317     C                   CALL      'TII103R'                                    *
035200000000     C                   PARM                    CODFOR                         *
035300000000     C                   PARM                    NOMF30                         *
035400000000     C                   GOTO      TAG001                                       *
035500000000     C                   END                                                    *
035600860522RIC  C*--- RICERCA PER CODICE FORNITORE -------------------*
035700860522RIC  C     RICFOR        TAG                                                    *
035800860522RIC  C                   TESTN                   CODFOR               46        *
035900860522RIC  C     *IN(46)       CABEQ     '0'           TAG001                   91    *
036000990203RIC  C     CODFOR        CHAIN     tmfop01L                           91        *
036100860522RIC  C   91              GOTO      TAG001                                       *
036200990203RIC  C     KEYFOR        CHAIN     anfrn01l                           91        *
036300990203RIC  C   91              GOTO      TAG001                                       *
036400990203     C     frnsogg       chain     ansog01l                           91
036500990203     C   91              GOTO      TAG001
036600990203RIC  C                   MOVE      sogdes        NOMFOR                         *
036700990203RIC  C                   MOVEL     sogdes        NOMF30                         *
036800990203RIC  C     frnsogg       CHAIN     anind01l                           91        *
036900860522RIC  C   91              GOTO      TAG001                                       *
037000990203     C                   MOVE      frncondpag    forCDP            3
037100990203     C                   MOVE      indprov       provfo
037200990203     C                   MOVEL     INDindriz     INDFOR                         *
037300990203     C                   MOVEL     INDlocalit    LOCFOR                         *
037400990203     C                   MOVE      INDCAP        CAPFOR                         *
037500860522     C     RIC150        IFEQ      'O'                                          *ORDINE
037600860210     C                   Z-ADD     NOR150        NUMORD                         *
037700860210     C                   Z-ADD     DOR150        TESDOR                         *
037800860210     C                   GOTO      TAG005                                       *
037900860210     C                   END                                                    *
038000000000     C*--- PULIZIA SUBFILE "1" ----------------------------*
038100000000     C                   Z-ADD     0             REC1              3 0    70    *
038200000000     C   70              WRITE     SFL1CT                                       *
038300000000     C                   MOVEA     *ZEROS        *IN(10)                        *
038400000000     C*--- CARICAMENTO SUBFILE "1" ------------------------*
038500990203     C     CODFOR        SETLL     tmmof01L                                     *
038600000000     C*--- SCRIVO RIGA "ORDINE NUOVO" ---------------------*
038700000000     C     REC1          ADD       1             REC1                           *
038800000000     C                   MOVE      *BLANKS       SF1TOR                         *
038900000000     C                   Z-ADD     0             NUMORD                         *
039000000000     C                   Z-ADD     0             TESDOR                         *
039100000000     C                   MOVEL     ORD(1)        SF1NOT                         *
039200000000     C                   WRITE     SFL1                                         *
039300000000     C*--- RICERCA TESTATE ORDINI -------------------------*
039400000000     C     TAG002        TAG                                                    *
039500000000     C*          -------------                             *
039600990203     C     CODFOR        READE     tmmof01L                               46    * ORDINE NUOVO
039700000000     C   46              GOTO      TAG003                                       *
039800000000     C     REC1          ADD       1             REC1                           *
039900000000     C                   MOVE      MOFTOR        SF1TOR                         *
040000000000     C                   Z-ADD     MOFNOR        NUMORD                         *
040100990203     c                   if        mofdor > 0
040200990203     C                   move      MOfDOR        DATAIso                        *
040300990203     C                   move      dataiso       dataeur                        *
040400990203     C                   move      DATAeur       TESDOR                         *
040500990203     c                   end
040600000000     C                   MOVE      '162'         TABGRU                         *
040700000000     C                   MOVE      *ZEROS        TABCOD                         *
040800000000     C                   MOVE      MOFTOR        TABCOD                         *
040900000000     C     KEYANT        CHAIN     ANTAB00F                           46        *
041000000000     C   46              MOVEL     'ERRORE'      SF1NOT                         *
041100000000     C  N46              MOVEL     TABDES        SF1NOT                         *
041200000000     C  N46              MOVEL     TABDES        DSORD                          *
041300000000     C*--- SCRIVO TESTATE ORDINI --------------------------*
041400000000     C                   WRITE     SFL1                                         *
041500990203     C     KEYORD        SETGT     tmmof01L                           46        *
041600000000     C  N46              GOTO      TAG002                                       *
041700000000     C     TAG003        TAG                                                    *
041800000000     C*          -------------                             *
041900000000     C                   SETON                                        1112      *
042000000000     C                   WRITE     OFN100T1                                     *
042100000000     C*--- EMETTO SUBFILE 1 -------------------------------*
042200000000     C                   EXFMT     SFL1CT                                       *
042300000000     C                   SETOFF                                       1112      *
042400990203     C   Kl              DO                                                     *
042500860210     C     RIC150        IFNE      *BLANKS                                      * RICHIAMATO
042600860210     C                   MOVE      'P'           RIC150                         *
042700860210     C                   GOTO      FINE                                         *
042800860210     C                   END                                                    *
042900860210     C                   GOTO      INIZIO                                       *
043000860210     C                   END                                                    *
043100000000     C                   READC     SFL1                                   46    *
043200000000     C   46              GOTO      TAG003                                       *
043300000000     C     SCELTA        CABEQ     *BLANKS       TAG003                         *
043400000000     C*----------------------------------------------------*
043500000000     C     TAG005        TAG                                                    *
043600000000     C*          -------------                             *
043700000000     C     NUMORD        IFEQ      0                                            *
043800000000     C*--- LETTURA NUMERO ORDINE NUOVO DA AREA DATI -------*
043900000000     C     SULOCK        TAG                                                    *
044000000000     C*          -------------                             *
044100000000     C     *LOCK         IN        DTAORF                               46      *
044200000000     C   46              GOTO      SULOCK                                       *
044300000000     C                   Z-ADD     ORDNUO        NUMORD                         *
044400000000     C                   ADD       1             ORDNUO                         *
044500000000     C                   OUT       DTAORF                                       *
044600990203     C                   MOVE      woggi         TESDOR                         *
044700000000     C*--- IMPOSTA DATI TESTATA PER ORDINE NUOVO ----------*
044800000000     C                   MOVE      '? '          TESTOR                         *
044900000000     C                   MOVE      *BLANKS       TESDTO                         *
045000871207     C                   MOVE      '5'           TESPTY                         *
045100000000     C*--- CALCOLA DATA CONSEGNA PREVISTA :                *
045200000000     C                   MOVE      '157'         TABGRU                         *
045300000000     C                   MOVE      *ZEROS        TABCOD                         *
045400000000     C                   MOVE      FOPAPP        TABCOD                         *
045500000000     C     KEYANT        CHAIN     ANTAB00F                           46        *
045600000000     C     *IN(46)       IFEQ      '1'                                          *
045700000000     C                   Z-ADD     TESDOR        TESDCN                         *
045800000000     C                   ELSE                                                   *
045900000000     C                   MOVEL     TABDES        DESAPP                         *
046000000000     C                   TESTN                   GGM157               46        *
046100000000     C     *IN(46)       IFEQ      '0'                                          *
046200000000     C                   MOVE      *ZEROS        GGM157                         *
046300000000     C                   END                                                    *
046400990204     c                   move      ggm157        gg157             3 0
046500990204     c                   move      tesdor        dataeur
046600990204     c                   adddur    gg157:*d      dataeur
046700990204     c                   move      dataeur       tesdcn
046800000000     C                   END                                                    *
046900860210     C*----------------------------------------------------*
047000000000     C                   MOVE      FOPLIS        TESLIS                         *
047100000000     C                   MOVE      FOPVAL        TESVAL                         *
047200990310RIC  C     TESVAL        IFEQ      xscdiv                                       *
047300860522RIC  C                   Z-ADD     1             TESCMB                         *
047400860522RIC  C                   ELSE                                                   *
047500990519     C                   move      tesval        divisa0202
047600990519     C                   z-add     0             Erro              1 0
047700990519     C                   Eval      Erro = X0201rpdvs(divisa0202:dtrif0202 :
047800990519     C                                           ANDVS00F:%size(ANDVS00F) :
047900990519     C                                           ANDVX00F:%size(ANDVX00F))
048000990519     C                   if        Erro<> 1 and Erro<> 3
048100990519     C                   reset                   PJX00202DS
048200990519     C                   movel     tesval        divisa0202
048300990519     C                   movel     xscdiv        divmc0202
048400990519     C                   z-add     1000          impord0202
048500990519     C                   movel     *on           Gesmsg
048600990519     C                   movel     *blanks       Esito
048700990519     C                   movel     *on           Noctrlscos
048800990519     C                   callp     x0202testc(PJX00202DS:
048900990519     C                                  %size(PJX00202DS):
049000990519     C                                  *off : Esito : Noctrlscos :
049100990519     C                                  Datigest :
049200990519     C                                  %size(ANDVS00F):
049300990519     C                                  %size(ANDVX00F):
049400990519     C                                  ANDVS00F : ANDVX00F )
049500990519     C                   If        Esito = '0'
049600990519     c                   z-add     cambio0202    tescmb
049700990519     c                   end
049800990310     c                   end
049900990310     c                   END
050000990203     C     CODFOR        SETLL     tmsfc000                               49
050100870127     C   49              Z-ADD     0             TESSC1
050200870127     C   49              Z-ADD     0             TESSC2
050300870127     C  N49              Z-ADD     FOPSC1        TESSC1                         *
050400870127     C  N49              Z-ADD     FOPSC2        TESSC2                         *
050500870127     C** SE ESISTE GESTIONE SCONTI FORNIT/CLASSE
050600870127     C** PROPONGO SEMPRE 0 IN TESTATA
050700870127     C** ALTREMENTI PROPONGO GLI SCONTI DEL FORNITORE
050800870130     C                   Z-ADD     0             TESSC3
050900870130     C                   Z-ADD     FOPSC1        TESSF1
051000870130     C                   Z-ADD     FOPSC2        TESSF2
051100870126     C                   Z-ADD     1             TESCLP                         *
051200000000     C                   MOVE      FOPDCC        TESCCO                         *
051300871207     C                   MOVE      '1  '         TESDEM                         *
051400000000     C                   MOVE      *BLANKS       TESDDM                         *
051500000000     C                   MOVE      *BLANKS       TESLDM                         *
051600870401     C                   MOVE      FORCDP        TESCPG                         *
051700000000     C                   MOVE      FOPGPM        TESGPM                         *
051800000000     C                   MOVE      FOPSTM        TESSTM                         *
051900870127     C                   Z-ADD     0             TESC01
052000870127     C                   Z-ADD     0             TESC02
052100870127     C                   Z-ADD     0             TESC03
052200870127     C                   Z-ADD     0             TESC04
052300870127     C                   Z-ADD     0             TESC05
052400000000     C*--- CONTROLLO DATI TESTATA ORDINE NUOVO ------------*
052500000000     C                   EXSR      RUTTES                                       *
052600000000     C*--- PULIZIA PROMPT RIGA DI FONDO -------------------*
052700000000     C                   EXSR      PULFON                                       *
052800000000     C*--- CARICAMENTO PROMPT RIGA DI FONDO ---------------*
052900000000     C                   EXSR      CARFON                                       *
053000000000     C                   END                                                    *
053100000000     C*--- RICERCA ORDINE ---------------------------------*
053200000000     C                   Z-ADD     0             REC2              5 0          *
053300000000     C                   Z-ADD     0             RECT              5 0          *
053400000000     C                   Z-ADD     0             SC1REC                         *
053500000000     C                   Z-ADD     0             SC1VID                         *
053600000000     C*--- PULIZIA SFL2CT                                  *
053700000000     C                   SETON                                            70    *
053800000000     C                   WRITE     SFL2CT                                       *
053900000000     C                   SETOFF                                           70    *
054000860210     C*----------------------------------------------------*
054100000000     C                   MOVEA     *ZEROS        *IN(75)                        *
054200000000     C                   Z-ADD     1             CURSOR                         * POSIZ.CURSORE
054300990203     C     KEYORD        SETLL     tmmof01L                           41  42    *
054400000000     C   41                                                                     *
054500871007     CORN42              MOVE      *BLANKS       CODSAV           12            *
054600000000     C   41                                                                     *
054700000000     CORN42              GOTO      TAG007                                       * ORDINE NUOVO
054800000000     C*--- LETTURA RIGHE ORDINE ESISTENTE -----------------*
054900000000     C     TAG006        TAG                                                    *
055000000000     C*          -------------                             *
055100990203     C     KEYORD        READE     tmmof01L                               46    *
055200000000     C   46              Z-ADD     REC2          RECT                           *
055300990827     c*  46              movel     *blank        mofdmtv
055400000000     C   46              GOTO      TAG007                                       *
055500990827     c*                  movel     mofdmt        mofdmtv
055600000000     C*--- DATI DA 1° RIGA ORDINE ESISTENTE A TESTATA -----*
055700000000     C  N08              DO                                                     *
055800000000     C                   SETON                                        08        *
055900000000     C*---                                                 *
056000000000     C                   MOVE      '162'         TABGRU                         *
056100000000     C                   MOVE      *ZEROS        TABCOD                         *
056200000000     C                   MOVE      MOFTOR        TABCOD                         *
056300000000     C     KEYANT        CHAIN     ANTAB00F                           46        *
056400000000     C  N46              MOVEL     TABDES        DESTOR                         *
056500000000     C  N46              MOVEL     TABDES        DSORD                          *
056600000000     C   46              MOVE      *BLANKS       DESTOR                         *
056700000000     C*---                                                 *
056800990203     c                   if        mofdor > 0
056900990203     C                   move      MOfDOR        DATAIso                        *
057000990203     C                   move      dataiso       dataeur                        *
057100990203     C                   move      DATAeur       TESDOR                         *
057200990203     c                   end
057300990203     c                   if        mofdcn > 0
057400990203     C                   move      MOfDcn        DATAIso                        *
057500990203     C                   move      dataiso       dataeur                        *
057600990203     C                   move      DATAeur       TESDcn                         *
057700990203     c                   end
057800000000     C                   MOVE      MOFPTY        TESPTY                         *
057900000000     C                   MOVE      MOFTOR        TESTOR                         *
058000000000     C                   MOVE      MOFLIS        TESLIS                         *
058100000000     C                   MOVE      MOFVAL        TESVAL                         *
058200990310     C                   z-add     MOFCMB        TESCMB                         *
058300990203     C     CODFOR        SETLL     tmsfc000                               49
058400870127     C   49              Z-ADD     0             TESSC1
058500870127     C   49              Z-ADD     0             TESSC2
058600870127     C  N49              Z-ADD     MOFSC1        TESSC1                         *
058700870127     C  N49              Z-ADD     MOFSC2        TESSC2                         *
058800870127     C** SE ESISTE GESTIONE SCONTI FOR/CLASSE
058900870127     C** PROPONGO SEMPRE 0 COLE SCONTI DI TESTATA
059000870127     C** ALTREMENTI PROPONGO QUELLI DELL'ORDINE
059100870130     C                   Z-ADD     MOFSC3        TESSC3
059200870130     C                   Z-ADD     FOPSC1        TESSF1
059300870130     C                   Z-ADD     FOPSC2        TESSF2
059400870130     C                   Z-ADD     MOFCLP        TESCLP                         *
059500000000     C                   MOVE      MOFCCO        TESCCO                         *
059600000000     C                   MOVE      MOFDEM        TESDEM                         *
059700000000     C                   MOVEL     MOFCPG        TESCPG                         *
059800000000     C                   MOVE      MOFGPM        TESGPM                         *
059900000000     C                   MOVE      MOFSTM        TESSTM                         *
060000870127     C     KCNF          KLIST
060100870127     C                   KFLD                    NUMORD
060200880212     C                   KFLD                    MOFDOR
060300990203     C     KCNF          CHAIN     tmcnf000                           48
060400870127     C  N48              DO
060500870127     C                   Z-ADD     CNFC01        TESC01
060600870127     C                   Z-ADD     CNFC02        TESC02
060700870127     C                   Z-ADD     CNFC03        TESC03
060800870127     C                   Z-ADD     CNFC04        TESC04
060900870127     C                   Z-ADD     CNFC05        TESC05
061000870127     C                   END
061100870127     C   48              DO
061200870127     C                   Z-ADD     0             TESC01
061300870127     C                   Z-ADD     0             TESC02
061400870127     C                   Z-ADD     0             TESC03
061500870127     C                   Z-ADD     0             TESC04
061600870127     C                   Z-ADD     0             TESC05
061700000000     C                   END                                                    *
061800870127     C*+ CARICA CONFERME D'ORDINE I  TESTATA
061900870127     C                   END
062000000000     C*--- CARICAMENTO SUBFILE 2 --------------------------*
062100000000     C     REC2          ADD       1             REC2                           *
062200000000     C                   Z-ADD     NREL1         SC1REC                         * NRR FISICO
062300000000     C                   Z-ADD     REC2          SC1VID                         * NRR VIDEO
062400000000     C                   WRITE     SFL2                                         *
062500000000     C                   GOTO      TAG006                                       *
062600000000     C*----------------------------------------------------*
062700000000     C     TAG007        TAG                                                    *
062800000000     C*          -------------                             *
062900860522     C                   SETOFF                                       46  73    *
063000000000     C                   MOVE      MOFMAT        CODSAV                         *
063100000000     C*--- PULIZIA PROMPT RIGA DI FONDO                    *
063200000000     C                   EXSR      PULFON                                       *
063300000000     C*--- CARICAMENTO PROMPT RIGA DI FONDO                *
063400000000     C                   EXSR      CARFON                                       *
063500000000     C*--- EMISSIONE RIGA DI FONDO CON ERRORI -------------*
063600000000     C     TAG008        TAG                                                    *
063700000000     C*          -------------                             *
063800000000     C     CURSOR        DIV       9             CURSOR                         * POSIZ.CURSORE
063900000000     C                   MVR                     RESTO             5 065        *
064000000000     C     CURSOR        MULT      9             CURSOR                         *
064100000000     C   65CURSOR        ADD       1             CURSOR                         *
064200000000     C*--- EMISSIONE PROMPT RIGA --------------------------*
064300000000     C     EMISF2        TAG                                                    *
064400000000     C*          -------------                             *
064500000000     C     RECT          COMP      *ZEROS                             7171      *
064600000000     C                   SETON                                        1113      *
064700000000     C                   WRITE     OFN100T1                                     * TESTATA
064800000000     C  N71              WRITE     PULISCI                                      * PULIZIA
064900000000     C                   EXFMT     SFL2CT                                       * SFL2CT
065000000000     C*--- RICHIESTO HELP ---------------------------------*
065100000000     C   99              DO                                                     *
065200000000     C                   EXFMT     HELP                                         *
065300000000     C                   GOTO      EMISF2                                       *
065400000000     C                   END                                                    *
065500000000     C*----------------------------------------------------*
065600000000     C                   SETOFF                                       111372    *
065700860522     C                   MOVEA     *ZEROS        *IN(75)                        *
065800000000     C*--- FINE LAVORO ------------------------------------*
065900990203     C   Kl              DO                                                     *
066000860210     C     RIC150        IFNE      *BLANKS                                      * RICHIAMATO
066100860210     C                   MOVE      'P'           RIC150                         *
066200860210     C                   GOTO      FINE                                         *
066300860210     C                   END                                                    *
066400000000     C*--- DATI PER STAMPA ORDINE                          *
066500000000     C                   MOVE      CODFOR        CODULT                         *
066600000000     C                   Z-ADD     NUMORD        ORDULT                         *
066700000000     C                   SETOFF                                       02        *
066800000000     C*---                                                 *
066900000000     C                   GOTO      TAG001                                       *
067000000000     C                   END                                                    *
067100000000     C*--- RICERCA DATI TESTATA ---------------------------*
067200000000     C   KA              DO                                                     *
067300000000     C                   SETON                                        07        * AGGIOR.RIGHE
067400000000     C                   EXSR      RUTTES                                       * PROMPT TESTA.
067500000000     C                   SETOFF                                       07        *
067600000000     C                   GOTO      TAG005                                       *
067700000000     C                   END                                                    *
067800000000     C*--- VISUALIZZAZIONE DISPONIBILITA' -----------------*
067900000000     C   KD              DO                                                     *
068000000000     C     SC2TRG        IFGE      '1'                                          *
068100000000     C     SC2TRG        IFLE      '3'                                          *
068200000000     C                   EXSR      RUTDIS                                       *
068300000000     C                   GOTO      EMISF2                                       *
068400000000     C                   END                                                    *
068500000000     C                   END                                                    *
068600000000     C                   END                                                    *
068700000000     C*--- LEGGO RIGHE RICHIAMATE -------------------------*
068800000000     C   71              READC     SFL2                                   10    *
068900000000     C   71
069000000000     CANN10              DO                                                     *
069100000000     C     SCELTA        CABEQ     *BLANKS       EMISF2                         *
069200000000     C     SC1REC        CHAIN     ORDFIS                             93        * AIUTO !
069300000000     C   93              MOVEL     ORD(2)        AIUTO                          *
069400000000     C   93              GOTO      TAG001                                       *
069500000000     C                   EXCEPT    RILORD                                       *
069600860522     C*---------- TEST POSSIBILITA AGGIORNAMENTO -BB/FA- --*
069700860522     C     MOFFAS        COMP      'BB'                                   80    *
069800860522     C  N80MOFFAS        COMP      'FA'                                   80    *
069900860523     C   80              DO                                                     *
070000860523     C     SCELTA        CABEQ     'A'           NOMAST                         *
070100860523     C*                                                    *
070200860523     C     MASTE1        IFNE      '*'                                          *
070300860523     C     MASTE1        CABNE     EDP1          NOMAST                         *
070400860523     C                   END                                                    *
070500860523     C*                                                    *
070600860523     C     MASTE2        IFNE      '*'                                          *
070700860523     C     MASTE2        CABNE     EDP2          NOMAST                         *
070800860523     C                   END                                                    *
070900860523     C*                                                    *
071000860523     C     MASTE3        IFNE      '*'                                          *
071100860523     C     MASTE3        CABNE     EDP3          NOMAST                         *
071200860523     C                   END                                                    *
071300860523     C*                                                    *
071400860522     C                   SETOFF                                           80    *
071500860522     C                   SETON                                            73    *
071600860522     C                   END                                                    *
071700860523     C*----------------------------------------------------*
071800860523     C     NOMAST        TAG                                                    *
071900000000     C  N80              SETON                                            14    * HI+BL RIGA
072000000000     C     SC1VID        CHAIN     SFL2                               94        * AIUTO !
072100000000     C   94              MOVEL     ORD(3)        AIUTO                          *
072200000000     C   94              GOTO      TAG001                                       *
072300000000     C                   MOVE      SCELTA        SCELTW                         *
072400000000     C                   MOVE      *BLANKS       SCELTA                         *
072500000000     C                   UPDATE    SFL2                                         *
072600000000     C                   Z-ADD     SC1VID        CURSOR               15  14    * POSIZ.CURSORE
072700000000     C*----------                                          *
072800000000     C   80              GOTO      ERRORI                                       *
072900000000     C                   SETON                                            40    * CODICE PR
073000000000     C                   EXSR      CARFON                                       *
073100000000     C                   GOTO      EMISF2                                       *
073200000000     C                   END                                                    *
073300000000     C*--- RIGA RICHIAMATA PER ERRORE ---------------------*
073400000000     C     SC2TRG        IFEQ      *BLANKS                                      *
073500000000     C     SC2VID        IFGT      *ZEROS                                       *
073600000000     C     SC2VID        CHAIN     SFL2                               17        *
073700860522     C                   SETOFF                                       1480      *
073800000000     C  N17              UPDATE    SFL2                                 17      *
073900000000     C                   END                                                    *
074000000000     C                   GOTO      TAG007                                       *
074100000000     C                   END                                                    *
074200000000     C*--- RIGA ANNULLATA ---------------------------------*
074300000000     C     SC2TRG        IFEQ      'A'                                          *
074400000000     C     SCELTW        CABNE     SC2TRG        ERRORI               7575      *
074500000000     C     SC2REC        CABEQ     0             TAG007                         *
074600000000     C     SC2REC        CHAIN     ORDFIS                             93        * AIUTO !
074700000000     C   93              MOVEL     ORD(2)        AIUTO                          *
074800000000     C   93              GOTO      TAG001                                       *
074900880105     C     MOFFAS        CABEQ     'FA'          ERRORI                   75
075000880105     C** FASE FA NON E' POSSIBILE ANNULLAMENTO
075100990203     C     KEYART        CHAIN     tmamp01L                           76
075200880105     C** AGGANCIO ARTICOLO
075300880106     C     MOFTRG        IFGE      '1'                                          *
075400880106     C     MOFTRG        IFLE      '3'                                          *
075500000000     C     MOFQTA        MULT      -1            MOFQTA                         *
075600860522     C                   EXSR      AGGSAL                                       *
075700000000     C                   END                                                    *
075800000000     C                   END                                                    *
075900000000     C                   MOVE      'A'           MOFATT                         *
076000000000     C                   EXSR      UPORD                                        *
076100860523     C                   SETOFF                                       7173      *
076200000000     C                   GOTO      TAG005                                       *
076300000000     C                   END                                                    *
076400000000     C*--- CAMBIO TIPO RIGA -------------------------------*
076500000000     C     SC2REC        IFGT      0                                            * RIGA VECCHIA
076600000000     C     MOFTRG        CABNE     SC2TRG        ERRORI               7575      *
076700000000     C                   END                                                    *
076800000000     C*--- RICERCA ALFABETICA ARTICOLI E ------------------*
076900000000     C*--- ACQUISIZIONE DATI DELLA STESSA -----------------*
077000000000     C     SC2TRG        IFGE      '1'                                          *
077100000000     C     SC2TRG        IFLE      '3'                                          *
077200871007     C     SC2MAT        IFEQ      INTERR                                       *
077300000000     C                   MOVE      *BLANKS       SC2MAT                         *
077400990317     C                   CALL      'TII104R'                                    *
077500000000     C                   PARM                    SC2MAT                         *
077600000000     C                   PARM                    SC2DMT                         *
077700000000     C                   END                                                    *
077800000000     C*--- CONTROLLO CODICE PRODOTTO ----------------------*
077900990203     C     KEYART        CHAIN     tmamp01L                           76        *
078000000000     C  N76              DO                                                     *
078100000000     C                   MOVEL     AMPDMT        SC2DMT                         *
078200000000     C                   MOVE      AMPUMQ        SC2UMQ                         *
078300000000     C     SC2MAG        IFEQ      *BLANKS                                      *
078400000000     C                   MOVE      AMPMAG        SC2MAG                         *
078500000000     C                   END                                                    *
078600000000     C     SC2MAG        IFEQ      '9999999'                                    *
078700000000     C                   MOVE      '?      '     SC2MAG                         *
078800000000     C                   END                                                    *
078900000000     C     SC2IMB        IFEQ      *BLANKS                                      *
079000000000     C                   MOVE      AMPIMB        SC2IMB                         *
079100000000     C                   END                                                    *
079200000000     C     SC2AIA        IFEQ      *ZEROS                                       *
079300000000     C                   MOVE      AMPAIA        SC2AIA                         *
079400000000     C                   END                                                    *
079500000000     C                   END                                                    *
079600000000     C                   END                                                    *
079700000000     C                   END                                                    *
079800870205     C     SC2FAS        COMP      'FA'                                   80
079900870205     C  N80SC2FAS        COMP      'BB'                                   80
080000000000     C*--- CONTROLLO QUANTITA' ----------------------------*
080100000000     C     SC2TRG        IFGE      '6'                                          *
080200000000     C     SC2TRG        IFLE      '9'                                          *
080300000000     C                   Z-ADD     1             SC2QTA                         *
080400000000     C                   END                                                    *
080500000000     C                   END                                                    *
080600000000     C     SC2QTA        COMP      *ZEROS                                 79    *
080700000000     C*--- RICERCA LISTINO --------------------------------*
080800000000     C     SC2LIS        CABEQ     '999'         NOLIST                         *
080900000000     C     SC2LIS        IFEQ      '?  '                                        *
081000000000     C                   MOVE      *BLANKS       SC2LIS                         *
081100000000     C                   MOVE      *BLANKS       DESL75           75            *
081200990317     C                   CALL      'TII116R'                                    *
081300000000     C                   PARM                    SC2LIS                         *
081400000000     C                   PARM                    DESL75                         *
081500000000     C                   END                                                    *
081600000000     C                   MOVE      '161'         TABGRU                         *
081700000000     C                   MOVE      *ZEROS        TABCOD                         *
081800000000     C                   MOVE      SC2LIS        TABCOD                         *
081900000000     C     KEYANT        CHAIN     ANTAB00F                           81        *
082000000000     C     NOLIST        TAG                                                    *
082100990310RIC  C*--- reperimento cambio------------------------------*
082200990519     C                   move      tesval        divisa0202
082300990519     C                   z-add     0             Erro              1 0
082400990519     C                   Eval      Erro = X0201rpdvs(divisa0202:dtrif0202 :
082500990519     C                                           ANDVS00F:%size(ANDVS00F) :
082600990519     C                                           ANDVX00F:%size(ANDVX00F))
082700000000     C*--- CONTROLLO PREZZO -------------------------------*
082800870127     C     KSFC          KLIST
082900870127     C                   KFLD                    CODFOR
083000870127     C                   KFLD                    AMPCLM
083100000000     C     SC2TRG        IFGE      '1'                                          *
083200000000     C     SC2TRG        IFLE      '3'                                          *
083300000000     C     SC2PRZ        IFEQ      *ZEROS                                       *
083400870127     C     SC2SC1        CABNE     0             POISC
083500870127     C     SC2SC2        CABNE     0             POISC
083600990203     C     KSFC          CHAIN     tmsfc000                           49
083700870127     C  N49              Z-ADD     SFCSC1        SC2SC1
083800870127     C  N49              Z-ADD     SFCSC2        SC2SC2
083900871218     C  N49              GOTO      POISC
084000990203     C   49CODFOR        SETLL     tmsfc000                               49
084100871218     C  N49              Z-ADD     FOPSC1        SC2SC1
084200871218     C  N49              Z-ADD     FOPSC2        SC2SC2
084300871218     C** SE TROVO SCONTI PER LA CLASSE DESIDERATA LI APPLICO
084400870127     C** ALTREMENTI SE TROVO GESTIONE SCONTI FOR/CLASSE    CONSIDERO SCONT
084500870127     C** FORNITORI
084600870127     C     POISC         TAG
084700860210     C*----------------------------------------------------*
084800990203     C     KEYLIS        CHAIN     tmlmp01L                           83        *
084900000000     C  N83              DO                                                     *
085000000000     C     SC2CLP        IFEQ      1                                            *
085100000000     C                   Z-ADD     LMPVP1        SC2PRZ                         *
085200000000     C                   END                                                    *
085300000000     C     SC2CLP        IFEQ      2                                            *
085400000000     C                   Z-ADD     LMPVP2        SC2PRZ                         *
085500000000     C                   END                                                    *
085600000000     C     SC2CLP        IFEQ      3                                            *
085700000000     C                   Z-ADD     LMPVP3        SC2PRZ                         *
085800000000     C                   END                                                    *
085900000000     C     SC2CLP        IFEQ      4                                            *
086000000000     C                   Z-ADD     LMPVP4        SC2PRZ                         *
086100000000     C                   END                                                    *
086200000000     C     SC2CLP        IFEQ      5                                            *
086300000000     C                   Z-ADD     LMPVP5        SC2PRZ                         *
086400000000     C                   END                                                    *
086500000000     C                   END                                                    *
086600000000     C                   END                                                    *
086700000000     C                   END                                                    *
086800000000     C                   END                                                    *
086900000000     C     SC2TRG        IFLE      '3'                                          *
087000000000     C     SC2PRZ        COMP      *ZEROS                                 83    *
087100000000     C                   END                                                    *
087200000000     C*--- CONTROLLO PAGAMENTO ----------------------------*
087300000000     C     SC2TRG        IFLT      '6'                                          *
087400000000     C     SC2CPG        IFEQ      '?  '                                        *
087500000000     C                   MOVE      *ZEROS        SC2CPG                         *
087600000000     C                   MOVE      *BLANKS       DESL30           30            *
087700990317     C                   CALL      'TII105R'                                    *
087800000000     C                   PARM                    SC2CPG                         *
087900000000     C                   PARM                    DESL30                         *
088000000000     C     SC2CPG        CABNE     '999'         SOTPAG                         *
088100000000     C                   END                                                    *
088200000000     C     SC2CPG        CABEQ     '999'         SOTPAG                   84    *
088300011001      *
088400011001      *Reperisco condizione di pagamento
088500011001     C                   CLEAR                   DviCdpDS
088600011001     C                   EVAL      DcpCod = SC2CPG
088700011001      *impostazione
088800011001     C                   EVAL      DcpRic = Chain
088900011001     C                   EVAL      DcpSoc = XscSoc
089000011001     C                   EVAL      DcpVis = Principale
089100011001     C                   EVAL      DcpNrk = 1
089200011001     C                   CLEAR                   DvoCdpDS
089300011001      *posizionamento
089400011001     C                   CALLB     'DVCDP'
089500011001     C                   PARM                    DviCdpDS
089600011001     C                   PARM                    DvoCdpDS
089700011001      *Si Errore
089800011001     C     DcpErr        IFne      '0'
089900011001     C                   EVAL      *IN84 = *ON
090000011001     C                   ENDIF
090100860522RIC  C******************************************************
090200011001RIC  C*****              MOVE      'FA'          TIPTAB                         *
090300011001RIC  C*****              MOVE      SC2CPG        PAGVID                         *
090400011001RIC  C*****KEYPAG        CHAIN     TABEL00F                           84        *
090500860522     C*----------------------------------------------------*
090600000000     C     SOTPAG        TAG                                                    *
090700000000     C                   END                                                    *
090800860522     C*---------- RIGHE BB/FA -----------------------------*
090900860522     C   73              GOTO      ERRORI                                       *  FA/BB
091000000000     C*--- CONTROLLO CODICE MAGAZZINO ---------------------*
091100000000     C     SC2TRG        IFGE      '1'                                          *
091200000000     C     SC2TRG        IFLE      '3'                                          *
091300000000     C     SC2MAG        IFEQ      '?      '                                    *
091400000000     C                   MOVE      *BLANKS       SC2MAG                         *
091500000000     C                   MOVE      *BLANKS       DESMAG           75            *
091600990317     C                   CALL      'TII108R'                                    *
091700000000     C                   PARM                    SC2MAG                         *
091800000000     C                   PARM                    DESMAG                         *
091900000000     C                   END                                                    *
092000000000     C                   MOVE      '153'         TABGRU                         *
092100000000     C                   MOVE      SC2MAG        TABCOD                         *
092200000000     C     KEYANT        CHAIN     ANTAB00F                           85        *
092300000000     C  N85              DO                                                     *
092400000000     C                   MOVEL     TABDES        DSMAG                          *
092500000000     C     SC2CDC        IFEQ      *BLANKS                                      *
092600990318     C                   MOVEl     CDC153        SC2CDC                         *
092700000000     C                   END                                                    *
092800000000     C                   END                                                    *
092900000000     C                   END                                                    *
093000000000     C                   END                                                    *
093100000000     C*--- CONTROLLO CODICE IMBALLO -----------------------*
093200000000     C     SC2TRG        IFGE      '1'                                          *
093300000000     C     SC2TRG        IFLE      '3'                                          *
093400000000     C     SC2IMB        IFEQ      '?      '                                    *
093500000000     C                   MOVE      *BLANKS       SC2IMB                         *
093600000000     C                   MOVE      *BLANKS       DESL25           25            *
093700990317     C                   CALL      'TII113R'                                    *
093800000000     C                   PARM                    SC2IMB                         *
093900000000     C                   PARM                    DESL25                         *
094000000000     C                   END                                                    *
094100000000     C                   MOVE      '158'         TABGRU                         *
094200000000     C                   MOVE      SC2IMB        TABCOD                         *
094300000000     C     KEYANT        CHAIN     ANTAB00F                           86        *
094400000000     C                   END                                                    *
094500000000     C                   END                                                    *
094600860522RIC  C*--- CONTROLLO C.D.C.--------------------------------*
094700990203RIC  C     SC2CDC        CABEQ     '99999999'    NONCDC                         *
094800860522RIC  C     SC2TRG        IFLT      '6'                                          *
094900860522RIC  C     SC2CDC        CABEQ     *ZEROS        NONCDC                   87    *
095000860522RIC  C                   MOVE      SC2CDC        CCOSTO                         *
095100990203RIC  C     KEYCDC        CHAIN     anent01l                           87        *
095200860522RIC  C                   END                                                    *
095300000000     C     NONCDC        TAG                                                    *
095400000000     C*--- CONTROLLO DATA CONSEGNA RICHIESTA --------------*
095500990203     c     sc2dcn        ifgt      0
095600990203     c                   move      sc2dcn        dataeur
095700990203     c                   move      dataeur       dataiso
095800990203     c                   move      dataiso       datcon            8 0
095900990203     c                   end
096000000000     C*--- CONTROLLO PRESENZA ERRORI ----------------------*
096100000000     C     ERRORI        TAG                                                    *
096200000000     C     75            DO        89            X                 2 0          *
096300000000     C     *IN(X)        COMP      '1'                                    72    *
096400000000     C  N72              END                                                    *
096500000000     C   72              DO                                                     * ERRORI
096600000000     C                   SETOFF                                       15        *
096700000000     C     X             SUB       74            IN                2 0          *
096800000000     C                   MOVEA     ERR(IN)       ERRORE                         *
096900000000     C                   GOTO      TAG008                                       *
097000000000     C                   END                                                    *
097100000000     C*---------- MANUTENZIONE RIGHE ORDINI --ADD----------*
097200000000     C     SC2REC        IFEQ      0                                            * RIGA NUOVA
097300000000     C*--- DATI DA VIDEO A FILE E AGGIORNAMENTO -----------*
097400000000     C                   SETON                                            39    *
097500000000     C                   EXSR      CARFIL                                       *
097600000000     C                   EXSR      UPORD                                        *
097700000000     C                   Z-ADD     NREL2         SC1REC                         *
097800000000     C*--- AGGIUNGO RIGA SFL2 -----------------------------*
097900000000     C     RECT          ADD       1             RECT                           *
098000000000     C                   Z-ADD     RECT          REC2                           *
098100000000     C                   Z-ADD     RECT          SC1VID                         *
098200000000     C                   Z-ADD     RECT          CURSOR                         *
098300000000     C                   WRITE     SFL2                                         *
098400000000     C*--- AGGIORNO CAMPI SU SALDI ------------------------*
098500000000     C     SC2TRG        IFGE      '1'                                          *
098600000000     C     SC2TRG        IFLE      '3'                                          *
098700000000     C                   EXSR      AGGSAL                                       *
098800000000     C                   END                                                    *
098900000000     C                   END                                                    *
099000000000     C                   GOTO      TAG007                                       *
099100000000     C                   END                                                    *
099200000000     C*--- MANUTENZIONE RIGA ORDINI --UDP------------------*
099300000000     C*----------------------------------------------------*
099400000000     C     SC2REC        IFGT      0                                            * RIGA VECCHIA
099500000000     C*--- RICERCA RIGA FILE E RILASCIO RECORD ------------*
099600000000     C     SC2REC        CHAIN     ORDFIS                             93        * AIUTO !
099700000000     C   93              MOVEL     ORD(2)        AIUTO                          *
099800000000     C   93              GOTO      TAG001                                       *
099900000000     C                   EXCEPT    RILORD                                       *
100000000000     C*--- RICARICO VECCHIA DISPONIBILITA' ----------------*
100100880106     C     SC2TRG        IFGE      '1'                                          *
100200880106     C     SC2TRG        IFLE      '3'                                          *
100300000000     C     MOFQTA        MULT      -1            MOFQTA                         *
100400000000     C                   EXSR      AGGSAL                                       *
100500000000     C                   END                                                    *
100600000000     C                   END                                                    *
100700000000     C*--- RICERCA RIGA FILE ------------------------------*
100800000000     C     SC2REC        CHAIN     ORDFIS                             93        * AIUTO !
100900000000     C   93              MOVEL     ORD(2)        AIUTO                          *
101000000000     C   93              GOTO      TAG001                                       *
101100000000     C*--- RICERCA RIGA SFL2 ------------------------------*
101200000000     C     SC2VID        CHAIN     SFL2                               94        * AIUTO !
101300000000     C   94              MOVEL     ORD(3)        AIUTO                          *
101400000000     C   94              GOTO      TAG001                                       *
101500000000     C*--- AGGIORNO RIGA FILE -----------------------------*
101600000000     C     SC2TRG        IFGE      '6'                                          *
101700000000     C                   MOVE      *BLANKS       SC2UMQ                         *
101800000000     C                   END                                                    *
101900000000     C                   SETOFF                                           39    *
102000000000     C                   EXSR      CARFIL                                       *
102100000000     C                   EXSR      UPORD                                        *
102200000000     C*--- AGGIORNO RIGA SFL2 -----------------------------*
102300000000     C                   UPDATE    SFL2                                         *
102400000000     C*--- SCARICO DISPONIBILITA' -------------------------*
102500880106     C     SC2TRG        IFGE      '1'                                          *
102600880106     C     SC2TRG        IFLE      '3'                                          *
102700000000     C                   EXSR      AGGSAL                                       *
102800000000     C                   END                                                    *
102900000000     C                   END                                                    *
103000000000     C                   GOTO      TAG007                                       *
103100000000     C                   END                                                    *
103200000000     C*--- FINE LAVORO ------------------------------------*
103300000000     C     FINE          TAG                                                    *
103400860210     C*--- ERRORE -----------------------------------------*
103500000000     C     FINERR        TAG                                                    *
103600000000     C   90              MOVEL     'KPJB207'     KMSMN                          *
103700000000     C                   SETON                                            LR    *
103800000000     C**--- ROUTINE PULIZIA PROMPT RIGA DI FONDO ---------**
103900000000     C***------------------------------------------------***
104000000000     CSR   PULFON        BEGSR                                                  *
104100000000     C                   MOVE      ' '           MOFTRG                         *
104200000000     C                   MOVE      *BLANKS       MOFMAT                         *
104300990203     C                   MOVE      *BLANKS       MOFDMT                         *
104400990827     C*                  MOVE      *BLANKS       MOFDMTv                        *
104500000000     C                   MOVE      *BLANKS       MOFUMQ                         *
104600000000     C                   Z-ADD     0             MOFQTA                         *
104700000000     C                   MOVE      *BLANKS       MOFFAS                         *
104800000000     C                   MOVE      *BLANKS       MOFLIS                         *
104900000000     C                   MOVE      *BLANKS       MOFVAL                         *
105000000000     C                   MOVE      *ZEROS        MOFCMB                         *
105100000000     C                   Z-ADD     0             MOFPRZ                         *
105200000000     C                   Z-ADD     0             PREZZK           11 2          *
105300000000     C                   Z-ADD     0             MOFSC1                         *
105400000000     C                   Z-ADD     0             MOFSC2                         *
105500000000     C                   MOVE      *BLANKS       MOFCCO                         *
105600000000     C                   MOVE      *BLANKS       MOFCPG                         *
105700000000     C                   MOVE      *BLANKS       MOFCOM                         *
105800000000     C                   MOVE      *BLANKS       MOFMAG                         *
105900000000     C                   MOVE      *BLANKS       MOFIMB                         *
106000000000     C                   MOVE      *BLANKS       MOFCDC                         *
106100000000     C                   Z-ADD     0             MOFAIA                         *
106200000000     C                   Z-ADD     0             MOFDCN                         *
106300000000     C*--                                                  *
106400000000     C                   Z-ADD     0             SC1REC                         * NRR FIS. SFL2
106500000000     C                   Z-ADD     0             SC1VID                   11    * NRR VIDEO
106600000000     C                   Z-ADD     0             SC2REC                         * NRR FIS.FONDO
106700000000     C                   Z-ADD     0             SC2VID                         * NRR VID.FONDO
106800000000     C                   SETOFF                                           40    *
106900000000     CSR                 ENDSR                                                  *
107000000000     C***------------------------------------------------***
107100000000     C**--- ROUTINE CARICAMENTO DATI PROMPT RIGA FONDO ---**
107200000000     C***------------------------------------------------***
107300000000     CSR   CARFON        BEGSR                                                  *
107400000000     C     MOFTRG        IFEQ      *BLANKS                                      *
107500000000     C                   MOVE      '1'           SC2TRG                         *
107600000000     C                   ELSE                                                   *
107700000000     C     SCELTW        IFEQ      'A'                                          *
107800000000     C                   MOVE      'A'           SC2TRG                         *
107900000000     C                   ELSE                                                   *
108000000000     C                   MOVE      MOFTRG        SC2TRG                         *
108100000000     C                   END                                                    *
108200000000     C                   END                                                    *
108300000000     C     MOFMAT        IFEQ      *BLANKS                                      *
108400870105     C                   MOVE      INTERR        SC2MAT                         *
108500000000     C                   ELSE                                                   *
108600000000     C                   MOVE      MOFMAT        SC2MAT                         *
108700000000     C                   END                                                    *
108800000000     C                   MOVEL     MOFDMT        SC2DMT                         *
108900000000     C                   MOVE      MOFUMQ        SC2UMQ                         *
109000000000     C                   Z-ADD     MOFQTA        SC2QTA                         *
109100000000     C     MOFFAS        IFEQ      *BLANKS                                      *
109200000000     C                   MOVE      'OR'          SC2FAS                         *
109300000000     C                   ELSE                                                   *
109400000000     C                   MOVE      MOFFAS        SC2FAS                         *
109500000000     C                   END                                                    *
109600000000     C     MOFLIS        IFEQ      *BLANKS                                      *
109700000000     C                   MOVE      TESLIS        SC2LIS                         *
109800000000     C                   ELSE                                                   *
109900000000     C                   MOVE      MOFLIS        SC2LIS                         *
110000000000     C                   END                                                    *
110100000000     C     MOFVAL        IFEQ      *BLANKS                                      *
110200000000     C                   MOVE      TESVAL        SC2VAL                         *
110300000000     C                   ELSE                                                   *
110400000000     C                   MOVE      MOFVAL        SC2VAL                         *
110500000000     C                   END                                                    *
110600000000     C     MOFCMB        IFEQ      *ZEROS                                       *
110700990310     C                   z-add     TESCMB        SC2CMB                         *
110800000000     C                   ELSE                                                   *
110900990310     C                   z-add     MOFCMB        SC2CMB                         *
111000000000     C                   END                                                    *
111100000000     C                   Z-ADD     MOFPRZ        SC2PRZ                         *
111200000000     C                   Z-ADD     SC2PRZ        PREZZK                         *
111300000000     C     SC1VID        IFEQ      *ZEROS                                       *
111400000000     C                   Z-ADD     TESSC1        SC2SC1                         *
111500000000     C                   Z-ADD     TESSC2        SC2SC2                         *
111600870130     C                   MOVE      TESCLP        SC2CLP                         *
111700000000     C                   ELSE                                                   *
111800870130     C                   Z-ADD     MOFSC1        SC2SC1                         *
111900870130     C                   Z-ADD     MOFSC2        SC2SC2                         *
112000870130     C                   Z-ADD     MOFCLP        SC2CLP
112100000000     C                   END                                                    *
112200860210     C*----------------------------------------------------*
112300000000     C     MOFCCO        IFEQ      *BLANKS                                      *
112400000000     C                   MOVEL     TESCCO        SC2CCO                         *
112500000000     C                   ELSE                                                   *
112600000000     C                   MOVEL     MOFCCO        SC2CCO                         *
112700000000     C                   END                                                    *
112800000000     C     MOFCPG        IFEQ      *BLANKS                                      *
112900000000     C                   MOVE      TESCPG        SC2CPG                         *
113000000000     C                   ELSE                                                   *
113100000000     C                   MOVEL     MOFCPG        SC2CPG                         *
113200000000     C                   END                                                    *
113300000000     C                   MOVE      MOFCOM        SC2COM                         *
113400000000     C                   MOVE      MOFMAG        SC2MAG                         *
113500000000     C                   MOVE      MOFIMB        SC2IMB                         *
113600000000     C                   MOVE      MOFCDC        SC2CDC                         *
113700000000     C     MOFAIA        IFEQ      *ZEROS                                       *
113800000000     C                   MOVE      TESAIA        SC2AIA                         *
113900000000     C                   ELSE                                                   *
114000000000     C                   MOVE      MOFAIA        SC2AIA                         *
114100000000     C                   END                                                    *
114200000000     C     MOFDCN        IFEQ      *ZEROS                                       *
114300000000     C                   MOVE      TESDCN        SC2DCN                         *
114400000000     C                   ELSE                                                   *
114500990203     C                   move      MOFDCN        DATAIso                        *
114600990203     C                   move      dataiso       dataeur                        *
114700990203     C                   move      DATAeur       SC2DCN                         *
114800000000     C                   END                                                    *
114900000000     C                   Z-ADD     SC1REC        SC2REC                         *NRR FIS.FONDO
115000000000     C                   Z-ADD     SC1VID        SC2VID                         *NRR VID.FONDO
115100000000     CSR                 ENDSR                                                  *
115200000000     C***------------------------------------------------***
115300000000     C**--- AGGIORNAMENTO RIGHE ORDINI -------------------**
115400000000     C***------------------------------------------------***
115500000000     CSR   UPORD         BEGSR                                                  *
115600000000     C  N39              UPDATE    ORDFIS                                       * UPDATE
115700000000     C   39              WRITE     ORDFIS                                       * WRITE
115800000000     C                   SETOFF                                           39    *
115900000000     C                   MOVE      *BLANKS       SCELTW            1            *
116000000000     CSR                 ENDSR                                                  *
116100000000     C***------------------------------------------------***
116200000000     C**--- VISUALIZZAZIONE DISPONIBILITA' ---------------**
116300000000     C***------------------------------------------------***
116400000000     CSR   RUTDIS        BEGSR                                                  *
116500000000     C                   MOVEL     KPJBU         KPJBUW                         *
116600000000     C                   MOVE      TESDOR        AAA115                         *
116700000000     C                   MOVE      SC2MAG        MAG115                         *
116800000000     C                   MOVE      SC2MAT        MAT115                         *
116900000000     C                   MOVE      'Q'           UMI115                         *
117000000000     C                   MOVE      'D'           SCE115                         *
117100000000     C                   MOVEL     PH115         KPJBU                          *
117200990317     C                   CALL      'TII202R'                                    *
117300000000     C                   PARM                    KPJBA                          *
117400000000     C                   MOVEL     KPJBUW        KPJBU                          *
117500000000     CSR                 ENDSR                                                  *
117600000000     C***------------------------------------------------***
117700000000     C**--- ROUTINE DATI DA VIDEO A FILE -----------------**
117800000000     C***------------------------------------------------***
117900000000     CSR   CARFIL        BEGSR                                                  *
118000880106     C                   MOVE      ' '           MOFATT                         *
118100990203     C                   move      xsccut        MOFUTE                         *
118200990203     C                   Z-ADD     woggig        MOFDMN                         *
118300880106     C                   MOVE      CODFOR        MOFFOR                         *
118400990203     c                   if        tesdor > 0
118500990203     C                   move      TESDOR        DATAeur                        *
118600990203     C                   move      dataeur       dataiso                        *
118700990203     C                   move      DATAIso       MOfDOR                         *
118800990203     c                   else
118900990203     c                   z-add     0             mofdor
119000990203     c                   end
119100880106     C                   Z-ADD     NUMORD        MOFNOR                         *
119200880106     C                   MOVE      TESTOR        MOFTOR                         *
119300880106     C                   MOVE      TESPTY        MOFPTY                         *
119400990203     c                   if        sc2dcn > 0
119500990203     C                   move      sc2dcn        DATAeur                        *
119600990203     C                   move      dataeur       dataiso                        *
119700990203     C                   move      DATAIso       MOfDcn                         *
119800990203     c                   else
119900990203     c                   z-add     0             mofdcn
120000990203     c                   end
120100000000     C                   MOVE      SC2LIS        MOFLIS                         *
120200000000     C                   MOVE      SC2CCO        MOFCCO                         *
120300000000     C                   MOVEL     SC2CPG        MOFCPG                         *
120400000000     C                   Z-ADD     SC2AIA        MOFAIA                         *
120500000000     C                   MOVE      SC2VAL        MOFVAL                         *
120600000000     C                   Z-ADD     SC2CMB        MOFCMB                         *
120700880106     C   39              MOVE      *BLANKS       MOFFES                         *
120800880106     C                   MOVE      SC2TRG        MOFTRG                         *
120900000000     C     SC2TRG        CABEQ     '1'           NOFASE                         *
121000000000     C     SC2TRG        CABEQ     '2'           NOFASE                         *
121100000000     C     SC2TRG        CABEQ     '3'           NOFASE                         *
121200880106     C                   MOVE      'OR'          SC2FAS                         *
121300000000     C     NOFASE        TAG                                                    *
121400880106     C                   MOVE      SC2FAS        MOFFAS                         *
121500880106     C   39              MOVE      *BLANKS       MOFTSR                         *
121600880106     C                   MOVE      SC2MAT        MOFMAT                         *
121700000000     C     SC2TRG        CABEQ     '1'           NOCODI                         *
121800000000     C     SC2TRG        CABEQ     '2'           NOCODI                         *
121900000000     C     SC2TRG        CABEQ     '3'           NOCODI                         *
122000860411     C                   MOVE      *BLANKS       MOFMAT                         *
122100000000     C     NOCODI        TAG                                                    *
122200860411     C     SC2TRG        IFGE      '6'                                          *
122300000000     C     SC2TRG        IFLE      '8'                                          *
122400880106     C                   MOVE      CODSAV        MOFMAT                         *
122500000000     C                   END                                                    *
122600000000     C                   END                                                    *
122700000000     C                   MOVEL     SC2DMT        MOFDMT                         *
122800000000     C                   MOVE      SC2UMQ        MOFUMQ                         *
122900880106     C     SC2TRG        IFLT      '1'                                          *
123000000000     C                   MOVE      'NR '         MOFUMQ                         *
123100000000     C                   END                                                    *
123200000000     C                   Z-ADD     SC2QTA        MOFQTA                         *
123300000000     C                   Z-ADD     SC2PRZ        MOFPRZ                         *
123400000000     C                   Z-ADD     SC2SC1        MOFSC1                         *
123500870130     C                   Z-ADD     SC2SC2        MOFSC2                         *
123600870130     C                   Z-ADD     TESSC3        MOFSC3
123700870130     C                   Z-ADD     SC2CLP        MOFCLP
123800000000     C                   MOVEL     SC2COM        MOFCOM                         *
123900880106     C                   MOVE      SC2IMB        MOFIMB                         *
124000880106     C   39              Z-ADD     0             MOFDBL                         *
124100880106     C   39              Z-ADD     0             MOFNBL                         *
124200880106     C   39              Z-ADD     0             MOFDFT                         *
124300880106     C   39              Z-ADD     0             MOFNFT                         *
124400880106     C   39              Z-ADD     0             MOFDPT                         *
124500880106     C   39              Z-ADD     0             MOFNPT                         *
124600880106     C                   MOVE      SC2MAG        MOFMAG                         *
124700880106     C                   MOVE      SC2CDC        MOFCDC                         *
124800880106     C   39              Z-ADD     0             MOFCRG                         *
124900880106     C                   MOVE      TESDEM        MOFDEM                         *
125000880106     C                   MOVE      TESGPM        MOFGPM                         *
125100880106     C                   MOVE      TESSTM        MOFSTM                         *
125200880106     C   39              MOVE      *BLANKS       MOFFLG                         *
125300000000     CSR                 ENDSR                                                  *
125400000000     C***------------------------------------------------***
125500000000     C**--- AGGIORNAMENTO CAMPI SU SALDI M.P. ------------**
125600000000     C***------------------------------------------------***
125700000000     CSR   AGGSAL        BEGSR                                                  *
125800000000     C*---                                                 *
125900990203     C     KEYSAL        CHAIN     tmsmp01L                           39        *
126000000000     C   39              DO                                                     *
126100990203     C                   MOVE      ANNDTA4       SMPAAC                         *
126200000000     C                   MOVE      SC2MAG        SMPMAG                         *
126300000000     C                   MOVE      SC2MAT        SMPMAT                         *
126400000000     C                   MOVE      AMPUMA        SMPUMA                         *
126500000000     C                   Z-ADD     0             SMPPLF                         *
126600000000     C                   Z-ADD     0             SMPPLV                         *
126700000000     C                   Z-ADD     0             SMPQGI                         *
126800000000     C                   Z-ADD     0             SMPQOR                         *
126900000000     C                   Z-ADD     0             SMPQPR                         *
127000000000     C                   Z-ADD     0             SMPQIM                         *
127100000000     C                   Z-ADD     0             SMPQGF                         *
127200000000     C                   Z-ADD     0             SMPQE1                         *
127300000000     C                   Z-ADD     0             SMPQE2                         *
127400000000     C                   Z-ADD     0             SMPQE3                         *
127500000000     C                   Z-ADD     0             SMPQE4                         *
127600000000     C                   Z-ADD     0             SMPQE5                         *
127700000000     C                   Z-ADD     0             SMPQU1                         *
127800000000     C                   Z-ADD     0             SMPQU2                         *
127900000000     C                   Z-ADD     0             SMPQU3                         *
128000000000     C                   Z-ADD     0             SMPQU4                         *
128100000000     C                   Z-ADD     0             SMPQU5                         *
128200990203     C                   Z-ADD     woggig        SMPDMS                         *
128300000000     C                   Z-ADD     0             SMPNMS                         *
128400000000     C                   Z-ADD     0             SMPPMC                         *
128500000000     C                   MOVE      *BLANKS       SMPFLG                         *
128600000000     C                   END                                                    *
128700000000     C*---                                                 *
128800880106     C     MOFFAS        CABNE     'OR'          NOSAL
128900000000     C                   Z-ADD     0             QTSALD           11 2          *
129000000000     C     MOFQTA        MULT(H)   AMPFCQ        QTSALD                         *
129100000000     C     MOFFAS        IFEQ      'OR'                                         *
129200000000     C  N39SMPQOR        ADD       QTSALD        SMPQOR                         *
129300000000     C   39              Z-ADD     QTSALD        SMPQOR                         *
129400000000     C                   END                                                    *
129500000000     C**--- AGGIORNO SALDI M.P. --------------------------**
129600000000     C                   MOVE      ' '           SMPATT                         *
129700990203     C                   move      xsccut        SMPUTE                         *
129800990203     C                   Z-ADD     woggig        SMPDMN                         *
129900990203     C  N39              UPDATE    tmsmp000                                     *
130000990203     C   39              WRITE     tmsmp000                                     *
130100880106     CSR   NOSAL         ENDSR                                                  *
130200000000     C***------------------------------------------------***
130300000000     C**--- ROUTINE RICHIAMO DATI TESTATA ----------------**
130400000000     C***------------------------------------------------***
130500000000     CSR   RUTTES        BEGSR                                                  *
130600000000     C                   MOVE      '162'         TABGRU                         *
130700000000     C                   MOVE      *ZEROS        TABCOD                         *
130800000000     C                   MOVE      TESTOR        TABCOD                         *
130900000000     C     KEYANT        CHAIN     ANTAB00F                           46        *
131000000000     C  N46              MOVEL     TABDES        DSORD                          *
131100000000     C  N46              MOVEL     TABDES        TESDTO                         *
131200000000     C   46              MOVE      *BLANKS       TESDTO                         *
131300000000     C                   MOVE      '161'         TABGRU                         *
131400000000     C                   MOVE      *ZEROS        TABCOD                         *
131500000000     C                   MOVE      TESLIS        TABCOD                         *
131600000000     C     KEYANT        CHAIN     ANTAB00F                           46        *
131700000000     C                   MOVE      '028'         TABGRU                         *
131800000000     C                   MOVE      *ZEROS        TABCOD                         *
131900000000     C                   MOVE      TESDEM        TABCOD                         *
132000000000     C     KEYANT        CHAIN     ANTAB00F                           46        *
132100000000     C  N46              MOVEL     TABDES        DESDEM                         *
132200000000     C   46              MOVE      *BLANKS       DESDEM                         *
132300000000     C                   MOVEL     RAG028        TESDDM                         *
132400000000     C                   MOVEL     LOC028        TESLDM                         *
132500011001     C******************************************************
132600011001      *Reperisco condizione di pagamento
132700011001     C                   CLEAR                   DviCdpDS
132800011001     C                   EVAL      DcpCod = TESCPG
132900011001      *impostazione
133000011001     C                   EVAL      DcpRic = Chain
133100011001     C                   EVAL      DcpSoc = XscSoc
133200011001     C                   EVAL      DcpVis = Principale
133300011001     C                   EVAL      DcpNrk = 1
133400011001     C                   CLEAR                   DvoCdpDS
133500011001      *posizionamento
133600011001     C                   CALLB     'DVCDP'
133700011001     C                   PARM                    DviCdpDS
133800011001     C                   PARM                    DvoCdpDS
133900011001      *Si Errore
134000011001RIC  C                   CLEAR                   TESDPG                         *
134100011001     C     DcpErr        IFne      '0'
134200011001     C                   EVAL      *IN46 = *ON
134300011001     C                   Else
134400011001RIC  C                   MOVEL     CDPDESCR      TESDPG                         *
134500011001     C                   ENDIF
134600011001      *
134700011001     C******************************************************
134800011001RIC  C*****              MOVE      'FA'          TIPTAB                         *
134900011001RIC  C*****              MOVE      TESCPG        PAGVID                         *
135000011001RIC  C*****KEYPAG        CHAIN     TABEL00F                           46        *
135100011001RIC  C**N46****          MOVEL     TBLUNI        TESDPG                         *
135200011001      *
135300860522RIC  C   46              MOVE      *BLANKS       TESDPG                         *
135400860522     C*----------------------------------------------------*
135500860522     C                   MOVE      '159'         TABGRU                         *
135600000000     C                   MOVE      *ZEROS        TABCOD                         *
135700000000     C                   MOVE      TESGPM        TABCOD                         *
135800000000     C     KEYANT        CHAIN     ANTAB00F                           46        *
135900000000     C  N46              MOVEL     TABDES        TESDGP                         *
136000000000     C   46              MOVE      *BLANKS       TESDGP                         *
136100000000     C                   MOVE      '160'         TABGRU                         *
136200000000     C                   MOVE      *ZEROS        TABCOD                         *
136300000000     C                   MOVE      TESSTM        TABCOD                         *
136400000000     C     KEYANT        CHAIN     ANTAB00F                           46        *
136500000000     C  N46              MOVEL     TABDES        TESDST                         *
136600000000     C   46              MOVE      *BLANKS       TESDST                         *
136700990310     C                   z-add     0             TEScmb                         *
136800860210     C*----------------------------------------------------*
136900000000     C     TESTA         TAG                                                    *
137000000000     C*          -------------                             *
137100000000     C*--- EMISSIONE PROMPT TESTATA -----------------------*
137200000000     C                   EXFMT     OFN100TS                                     *
137300990203     C   Kl              DO                                                     *
137400000000     C                   GOTO      INIZIO                                       *
137500000000     C                   END                                                    *
137600000000     C*--- CONTROLLO SE MODIFICATI CAMPI TESTATA :         *
137700000000     C   0920            DO        29            YYY               2 0          *
137800000000     C     *IN(YYY)      IFEQ      '1'                                          *
137900000000     C     YYY           ADD       30            KK                2 0          *
138000000000     C                   MOVE      '1'           *IN(KK)                        *
138100000000     C                   END                                                    *
138200000000     C                   END                                                    *
138300000000     C*--- INTERROGAZIONE ANAGRAFICO PARTICOLARITA' -------*
138400000000     C   KB              DO                                                     *
138500000000     C                   MOVEL     KPJBU         KPJBUW                         *
138600000000     C                   MOVE      CODFOR        FOR820                         *
138700000000     C                   MOVE      NOMF30        RAG820                         *
138800000000     C                   MOVE      'R'           RIC820                         *
138900000000     C                   MOVEL     PAR820        KPJBU                          *
139000990317     C                   CALL      'TII006R'                                    *
139100000000     C                   PARM                    KPJBA                          *
139200000000     C                   MOVEL     KPJBUW        KPJBU                          *
139300000000     C                   GOTO      TESTA                                        *
139400000000     C                   END                                                    *
139500000000     C*--- CONTROLLO DATI TESTATA -------------------------*
139600000000     C   20              DO                                                     *
139700990203     C     *eur          test(d)                 tesdor                 90      *
139800990203     C   90*dmy          test(d)                 tesdor                 90      *
139900990203     C   90              goto      testa                                        *
140000860210     C*----------------------------------------------------*
140100000000     C  N21              DO                                                     *
140200000000     C*--- CALCOLA DATA CONSEGNA PREVISTA :                *
140300000000     C                   MOVE      '157'         TABGRU                         *
140400000000     C                   MOVE      *ZEROS        TABCOD                         *
140500000000     C                   MOVE      FOPAPP        TABCOD                         *
140600000000     C     KEYANT        CHAIN     ANTAB00F                           46        *
140700000000     C     *IN(46)       IFEQ      '1'                                          *
140800000000     C                   Z-ADD     TESDOR        TESDCN                         *
140900000000     C                   ELSE                                                   *
141000000000     C                   MOVEL     TABDES        DESAPP                         *
141100000000     C                   TESTN                   GGM157               46        *
141200000000     C     *IN(46)       IFEQ      '0'                                          *
141300000000     C                   MOVE      *ZEROS        GGM157                         *
141400000000     C                   END                                                    *
141500990204     c                   move      ggm157        gg157
141600990204     c                   move      tesdor        dataeur
141700990204     c                   adddur    gg157:*d      dataeur
141800990204     c                   move      dataeur       tesdcn
141900000000     C                   SETON                                        51        * X CMD 7
142000000000     C                   END                                                    *
142100000000     C                   END                                                    *
142200000000     C                   END                                                    *
142300000000     C   21              DO                                                     *
142400990203     c                   if        tesdcn > 0
142500990203     C     *eur          test(d)                 tesdcn                 91      *
142600990203     C   91*dmy          test(d)                 tesdcn                 91      *
142700990203     C   91              goto      testa                                        *
142800000000     C                   END                                                    *
142900990203     C                   END                                                    *
143000000000     C*---------                                           * TIPO ORDINE
143100000000     C     TESTOR        IFEQ      '? '                                         *
143200000000     C                   MOVE      *BLANKS       TESTOR                         *
143300000000     C                   MOVE      *BLANKS       TESDTO                         *
143400990317     C                   CALL      'TII117R'                                    *
143500000000     C                   PARM                    TESTOR                         *
143600000000     C                   PARM                    TESDTO                         *
143700000000     C     TESDTO        COMP      '99'                                   92    *
143800000000     C                   GOTO      TESTA                                        *
143900000000     C                   ELSE                                                   *
144000000000     C                   MOVE      '162'         TABGRU                         *
144100000000     C                   MOVE      *ZEROS        TABCOD                         *
144200000000     C                   MOVE      TESTOR        TABCOD                         *
144300000000     C     KEYANT        CHAIN     ANTAB00F                           92        *
144400000000     C   92              GOTO      TESTA                                        *
144500000000     C                   MOVEL     TABDES        TESDTO                         *
144600000000     C                   MOVEL     TABDES        DESTOR                         *
144700000000     C                   MOVEL     TABDES        DSORD                          *
144800000000     C                   END                                                    *
144900860210     C*----------LISTINO-----------------------------------*
145000000000     C     TESLIS        IFEQ      '?  '                                        *
145100000000     C                   MOVE      *BLANKS       TESLIS                         *
145200000000     C                   MOVE      *BLANKS       DESL75           75            *
145300990317     C                   CALL      'TII116R'                                    *
145400000000     C                   PARM                    TESLIS                         *
145500000000     C                   PARM                    DESL75                         *
145600000000     C                   GOTO      TESTA                                        *
145700000000     C                   END                                                    *
145800000000     C                   MOVE      '161'         TABGRU                         *
145900000000     C                   MOVE      *ZEROS        TABCOD                         *
146000000000     C                   MOVE      TESLIS        TABCOD                         *
146100000000     C     KEYANT        CHAIN     ANTAB00F                           46        *
146200000000     C     *IN(46)       CABEQ     '1'           TESTA                    93    *
146300000000     C*---------                                           * VALUTA
146400990310     C*----------RICERCA valuta  :                         *
146500990310     c                   if        tesval = '?   '
146600990310     C                   MOVEL     '01'          OPZ015
146700990310     C                   MOVEL     *blank        Div015
146800990310     C                   MOVEL     A015DS        KPJBU
146900990310     C                   CALL      'ANA015R'
147000990310     C                   PARM                    KPJBA
147100990310     C                   MOVEL     KPJBU         A015DS
147200990310     C     Div015        IFNE      *BLANK
147300990310     C                   MOVEL     Div015        tesval
147400990310     C                   GOTO      testa                                        *
147500990310     C                   ENDIF
147600990310     C                   ENDIF
147700990310     C     tesval        CABEQ     *BLANKS       testa                    94    *
147800990519     C                   if        not *in94
147900990519     C                   move      tesval        divisa0202
148000990519     C                   z-add     0             Erro              1 0
148100990519     C                   Eval      Erro = X0201rpdvs(divisa0202:dtrif0202 :
148200990519     C                                           ANDVS00F:%size(ANDVS00F) :
148300990519     C                                           ANDVX00F:%size(ANDVX00F))
148400990519     C                   if        Erro= 1 or Erro= 3
148500990519     C                   seton                                        94
148600990519     C                   end
148700990519     C                   end
148800990310     c   94              goto      testa
148900990310      *imposta cambio
149000990310     C     TESVAL        IFEQ      xscdiv                                       *
149100000000     C                   Z-ADD     1             TESCMB                         *
149200000000     C                   ELSE                                                   *
149300990519     C                   reset                   PJX00202DS
149400990519     C                   movel     tesval        divisa0202
149500990519     C                   movel     xscdiv        divmc0202
149600990519     C                   z-add     1000          impord0202
149700990519     C                   movel     *on           Gesmsg
149800990519     C                   movel     *blanks       Esito
149900990519     C                   movel     *on           Noctrlscos
150000990519     C                   callp     x0202testc(PJX00202DS:
150100990519     C                                  %size(PJX00202DS):
150200990519     C                                  *off : Esito : Noctrlscos :
150300990519     C                                  Datigest :
150400990519     C                                  %size(ANDVS00F):
150500990519     C                                  %size(ANDVX00F):
150600990519     C                                  ANDVS00F : ANDVX00F )
150700990519     C                   If        Esito = '0'
150800990519     c                   z-add     cambio0202    tescmb
150900990519     c                   end
151000860522RIC  C                   END                                                    *
151100860522     C*----------------------------------------------------* DESTIN.MERCE
151200000000     C     TESDEM        IFEQ      '?  '                                        *
151300000000     C                   MOVE      *BLANKS       TESDEM                         *
151400000000     C                   MOVE      *BLANKS       DESL75           75            *
151500990317     C                   CALL      'TII107R'                                    *
151600000000     C                   PARM                    TESDEM                         *
151700000000     C                   PARM                    DESL75                         *
151800000000     C                   MOVEL     DESL75        DESDEM                         *
151900000000     C                   MOVEL     RAG028        TESDDM                         *
152000000000     C                   MOVEL     LOC028        TESLDM                         *
152100000000     C                   GOTO      TESTA                                        *
152200000000     C                   END                                                    *
152300000000     C                   MOVE      '028'         TABGRU                         *
152400000000     C                   MOVE      *ZEROS        TABCOD                         *
152500000000     C                   MOVE      TESDEM        TABCOD                         *
152600000000     C     KEYANT        CHAIN     ANTAB00F                           46        *
152700000000     C     *IN(46)       CABEQ     '1'           TESTA                    98    *
152800000000     C                   MOVEL     TABDES        DESDEM                         *
152900000000     C                   MOVEL     RAG028        TESDDM                         *
153000000000     C                   MOVEL     LOC028        TESLDM                         *
153100860210     C*----------PAGAMENTO---------------------------------*
153200000000     C     TESCPG        IFEQ      '?  '                                        *
153300000000     C                   MOVE      *ZEROS        TESCPG                         *
153400000000     C                   MOVE      *BLANKS       TESDPG                         *
153500990317     C                   CALL      'TII105R'                                    *
153600000000     C                   PARM                    TESCPG                         *
153700000000     C                   PARM                    TESDPG                         *
153800000000     C                   GOTO      TESTA                                        *
153900000000     C                   ELSE                                                   *
154000860522     C     TESCPG        CABEQ     '999'         TESTA                    95    *
154100011001      *
154200011001      *Reperisco condizione di pagamento
154300011001     C                   CLEAR                   DviCdpDS
154400011001     C                   EVAL      DcpCod = TESCPG
154500011001      *impostazione
154600011001     C                   EVAL      DcpRic = Chain
154700011001     C                   EVAL      DcpSoc = XscSoc
154800011001     C                   EVAL      DcpVis = Principale
154900011001     C                   EVAL      DcpNrk = 1
155000011001     C                   CLEAR                   DvoCdpDS
155100011001      *posizionamento
155200011001     C                   CALLB     'DVCDP'
155300011001     C                   PARM                    DviCdpDS
155400011001     C                   PARM                    DvoCdpDS
155500011001      *Si Errore
155600011001RIC  C                   CLEAR                   TESDPG                         *
155700011001     C     DcpErr        IFne      '0'
155800011001     C                   EVAL      *IN46 = *ON
155900011001     C                   Else
156000011001RIC  C                   MOVEL     CDPDESCR      TESDPG                         *
156100011001     C                   ENDIF
156200011001RIC  C******************************************************
156300011001RIC  C*****              MOVE      'FA'          TIPTAB                         *
156400011001RIC  C*****              MOVE      TESCPG        PAGVID                         *
156500011001RIC  C*****KEYPAG        CHAIN     TABEL00F                           46        *
156600011001RIC  C**N46***           MOVEL     TBLUNI        TESDPG                         *
156700011001      *
156800860522RIC  C     *IN(46)       CABEQ     '1'           TESTA                    95    *
156900860522RIC  C                   END                                                    *
157000860522     C*----------------------------------------------------* GRUPPO MERC.
157100000000     C     TESGPM        IFEQ      '? '                                         *
157200000000     C                   MOVE      *ZEROS        TESGPM                         *
157300000000     C                   MOVE      *BLANKS       TESDGP                         *
157400990317     C                   CALL      'TII114R'                                    *
157500000000     C                   PARM                    TESGPM                         *
157600000000     C                   PARM                    TESDGP                         *
157700000000     C                   GOTO      TESTA                                        *
157800000000     C                   ELSE                                                   *
157900000000     C                   MOVE      '159'         TABGRU                         *
158000000000     C                   MOVE      *ZEROS        TABCOD                         *
158100000000     C                   MOVE      TESGPM        TABCOD                         *
158200000000     C     KEYANT        CHAIN     ANTAB00F                           46        *
158300000000     C  N46              MOVEL     TABDES        TESDGP                         *
158400000000     C     *IN(46)       CABEQ     '1'           TESTA                    96    *
158500000000     C                   END                                                    *
158600000000     C*---------                                           * SETTORE MERC.
158700000000     C     TESSTM        IFEQ      '? '                                         *
158800000000     C                   MOVE      *ZEROS        TESSTM                         *
158900000000     C                   MOVE      *BLANKS       TESDST                         *
159000990317     C                   CALL      'TII115R'                                    *
159100000000     C                   PARM                    TESSTM                         *
159200000000     C                   PARM                    TESDST                         *
159300000000     C                   GOTO      TESTA                                        *
159400000000     C                   ELSE                                                   *
159500000000     C                   MOVE      '160'         TABGRU                         *
159600000000     C                   MOVE      *ZEROS        TABCOD                         *
159700000000     C                   MOVE      TESSTM        TABCOD                         *
159800000000     C     KEYANT        CHAIN     ANTAB00F                           46        *
159900000000     C  N46              MOVEL     TABDES        TESDST                         *
160000000000     C     *IN(46)       CABEQ     '1'           TESTA                    97    *
160100000000     C                   END                                                    *
160200871028     C*---------                                           *
160300000000     C                   MOVEA     *ZEROS        *IN(90)                        *
160400990203     C*---------                                           *
160500000000     C   KG              DO                                                     *
160600990203     C     KEYORD        SETLL     tmmof01L                           47  46    *
160700000000     C   47                                                                     *
160800000000     CORN46              GOTO      NORIGA                                       *
160900000000     C                   DO        *HIVAL                                       *
161000990203     C     KEYORD        READE     tmmof01L                               46    *
161100860509     C   46              GOTO      NORIGA                                       *
161200860509     C*---------                                           *
161300860509     C     MOFFAS        IFNE      'FA'                                         *
161400860509     C     MOFFAS        IFNE      'BB'                                         *
161500990203     C   50              move      TESDOR        DATAeur                        *
161600990203     C   50              move      dataeur       dataiso                        *
161700990203     C   50              move      DATAIso       MOFDOR                         *
161800860509     C   50              MOVE      TESGPM        MOFGPM                         *
161900860509     C   50              MOVE      TESSTM        MOFSTM                         *
162000860509     C   50              MOVE      TESDEM        MOFDEM                         *
162100990203     C   51              move      TESDCN        DATAeur                        *
162200990203     C   51              move      dataeur       dataiso                        *
162300990203     C   51              move      DATAIso       MOFDCN                         *
162400000000     C   52              MOVE      TESPTY        MOFPTY                         *
162500000000     C   54              MOVE      TESTOR        MOFTOR                         *
162600860509     C   55              MOVE      TESLIS        MOFLIS                         *
162700860509     C   55              MOVE      TESVAL        MOFVAL                         *
162800860509     C   55              Z-ADD     TESCMB        MOFCMB                         *
162900000000     C   56              MOVE      TESCCO        MOFCCO                         *
163000000000     C   57              MOVEL     TESCPG        MOFCPG                         *
163100870130     C                   Z-ADD     TESSC3        MOFSC3
163200000000     C                   UPDATE    ORDRIG                                       *
163300000000     C                   END                                                    *
163400860509     C                   END                                                    *
163500860509     C                   END                                                    *
163600860509     C*----------                                          *
163700000000     C     NORIGA        TAG                                                    *
163800000000     C                   SETOFF                                       4647      *
163900000000     C     20            DO        29            J                 2 0          *
164000000000     C                   MOVE      '0'           *IN(J)                         *
164100000000     C                   END                                                    *
164200000000     C     50            DO        59            J                              *
164300000000     C                   MOVE      '0'           *IN(J)                         *
164400000000     C                   END                                                    *
164500000000     C                   END                                                    *
164600870127     C                   EXSR      AGGCNF
164700870127     C** AGGIORNA FILE CONFERME D'RODINE
164800000000     CSR                 ENDSR                                                  *
164900870127     CSR   AGGCNF        BEGSR                                                  *
165000870127     C***------------------------------------------------***
165100870127     C**--- AGGIORNE CONFEREME D'ORDINE      -------------**
165200870127     C***------------------------------------------------***
165300870127     C                   Z-ADD     0             INCNF             2 0
165400870127     C     TESC01        IFGT      0
165500870127     C                   ADD       1             INCNF
165600870127     C                   END
165700870127     C     TESC02        IFGT      0
165800870127     C                   ADD       1             INCNF
165900870127     C                   END
166000870127     C     TESC03        IFGT      0
166100870127     C                   ADD       1             INCNF
166200870127     C                   END
166300870127     C     TESC04        IFGT      0
166400870127     C                   ADD       1             INCNF
166500870127     C                   END
166600870127     C     TESC05        IFGT      0
166700870127     C                   ADD       1             INCNF
166800870127     C                   END
166900990203     C     KCNF          CHAIN     tmcnf000                           48
167000870127     C     INCNF         IFEQ      0
167100990203     C  N48              DELETE    tmcnf000
167200870127     C                   ELSE
167300870127     C                   Z-ADD     TESC01        CNFC01
167400870127     C                   Z-ADD     TESC02        CNFC02
167500870127     C                   Z-ADD     TESC03        CNFC03
167600870127     C                   Z-ADD     TESC04        CNFC04
167700870127     C                   Z-ADD     TESC05        CNFC05
167800990203     C  N48              UPDATE    tmcnf000
167900870127     C   48              Z-ADD     NUMORD        CNFNOR
168000880104     C   48              Z-ADD     MOFDOR        CNFDOR
168100990203     C   48              move      xsccut        CNFUTE
168200990203     C   48              WRITE     tmcnf000
168300870127     C                   END
168400870127     C                   ENDSR
168500990203     C*----------------------------------------------------*
168600990203     C* Reperimento dati società
168700990203     C*----------------------------------------------------*
168800990203     C     REPSOC        BEGSR
168900990203     C*
169000990203     C                   CALLB     'XSOC'
169100990203     C                   PARM                    TIPXSC            6
169200990203     C                   PARM                    SOCXSC            3
169300990203     C                   PARM                    CDSXSC            9 0
169400990203     C                   PARM                    MODXSC            3
169500990203     C                   PARM      *blanks       RTNXSC            1
169600990203     C                   PARM                    XSOCDS
169700990203     C                   PARM                    KPJBA
169800990203     C*
169900990203     C                   ENDSR
170000860210     O*--- RILASCIO RIGA ORDINI ----*
170100000000     OORDFIS    E            RILORD
170200860210**   ORD
170300000000*-ORDINE NUOVO*
170400860210*-MANCA RECORD FISICO-*
170500860210*-MANCA RECORD VIDEO -*
170600860210**     ERR                              * ERRORE
170700860210VARIAZIONE TIPO RIGA NON AMMESSO      -*  ERR,1
170800860210CODICE ARTICOLO ERRATO                -*  ERR,2
170900860210VERSIONE ARTICOLO ERRATA              -*  ERR,3
171000860210UNITA' DI MISURA ERRATA               -*  ERR,4
171100860210DIGITARE QUANTITA'                    -*  ERR,5
171200860210VARIAZIONE NON AMMESSA IN QUESTA FASE -*  ERR,6
171300860210CODICE LISTINO ERRATO                 -*  ERR,7
171400860210CODICE VALUTA ERRATA                  -*  ERR,8
171500860210DIGITARE PREZZO                       -*  ERR,9
171600860210CODICE CONDIZIONI DI PAGAMENTO ERRATO -*  ERR,10
171700860210CODICE MAGAZZINO ERRATO               -*  ERR,11
171800860210CODICE IMBALLO ERRATO                 -*  ERR,12
171900860210CODICE CENTRO DI COSTO ERRATO         -*  ERR,13
172000860210NUMERO LOTTO/COMMESSA OBBLIGATORIO    -*  ERR,14
172100860210DATA DI CONSEGNA PREVISTA ERRATA      -*  ERR,15
