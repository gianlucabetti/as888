000100000000     H*------------------------------------------------------------------------*
000200950626     H* RIGENERA FILE DI LAVORO PER STATISTICHE AFFIDABILITA'                  *
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800041021     FTITAS26C  IF   E           K DISK
000900000000     FAZORG01L  IF   E           K DISK
001000000000     FTABEL00F  IF   E           K DISK
001100100209     FWFDLC01L  iF   E           K DISK
001200100209     FWFDLA01L  iF   E           K DISK
001300100209     FWFDLP02L  UF A E           K DISK
001400100209     FPCDLC01L  iF   E           K DISK
001500100209     FPCDLA01L  iF   E           K DISK
001600100209     FPCDLP01L  iF   E           K DISK
001700020702     FTNTBE01L  IF   E           K DISK
001800000000     D*------------------------------------------------------------------------*
001900000000     D* SCHIERE
002000000000     D*------------------------------------------------------------------------*
002100000000     D*-------------------
002200000000     D* FILIALI CON RELATIVA DIVISIONE E AREA
002300000000     D*-------------------
002400971124     D FIL             S              3  0 DIM(500)                             *FILIALE
002500971124     D ARE             S              3  0 DIM(500)                             *AREA
002600971124     D DIV             S              1    DIM(500)                             *DIVISIONE
002700971204     D FL1             S              1    DIM(500)                             *FLAG ITALIA/ESTERO
002800100125     D*** POS             S              3    DIM(500)                             *NETWORK POSTE
002900000000     D*-------------------
003000000000     D* SCHIERA FLAG SPEDIZIONE/RICAVO E RAGGRUPPAMENTO TIPO BOLLA
003100000000     D*-------------------
003200000000     D RBL             S              1    DIM(5)                               RAGGRUPPAMENTO BOLLA
003300000000     D FSP             S              1    DIM(5)                               FLAG SPEDIZIONE
003400000000     D FRI             S              1    DIM(5)                               FLAG RICAVO
003500000000     D*-------------------
003600000000     D* SCHIERA TIPO BOLLA CON RELATIVI FLAG SPEDIZIONE/RICAVO
003700000000     D*-------------------
003800000000     D TB              S              2    DIM(20)                              TIP0 BOLLA
003900000000     D SP              S              1    DIM(20)                              FLAG SPEDIZIONE
004000000000     D RI              S              1    DIM(20)                              FLAG RICAVO
004100000000     D*------------------------------------------------------------------------*
004200000000     D* INPUT
004300000000     D*------------------------------------------------------------------------*
004400000000     D*-------------------
004500000000     D* ARCHITETTURA
004600000000     D*-------------------
004700000000     D KPJBA         E DS
004800000000     D*-------------------
004900000000     D* DS CODICI BOLLA
005000000000     D*-------------------
005100000000     D DS3A          E DS
005200000000     D*-------------------
005300000000     D* DS TIPO BOLLA
005400000000     D*-------------------
005500000000     D DSTB          E DS
005600000000     D*-------------------
005700000000     D* DS FLAG SPEDIZIONI/RICAVI RAGGRUPPAMENTO BOLLA
005800000000     D*-------------------
005900000000     D DS2W          E DS
006000000000     D*-------------------
006100000000     D* CONTROLLO DATE
006200000000     D*-------------------
006300000000     D G02PAR          DS
006400000000     D  G02DAT                 1      8  0
006500000000     D  G02INV                 9     16  0
006600000000     D  G02ERR                17     17
006700000000     D  G02TGI                18     22  0
006800000000     D*-------------------
006900941209     D* DS SCOMPOSIZIONE DATA GENERICA
007000000000     D*-------------------
007100000000     D                 DS
007200000000     D  DSANN                  1      4  0
007300000000     D  DSMES                  5      6  0
007400000000     D  DSGIO                  7      8  0
007500000000     D  DSMGS                  5      8  0
007600000000     D  DSDAT                  1      8  0
007700941209     D*-------------------
007800941209     D* DS SCOMPOSIZIONE DATA CONSEGNA MERCE
007900941209     D*-------------------
008000941209     D  WAACM                  1      4  0
008100941209     D  WMMCM                  5      6  0
008200941209     D  TASDCM                 1      8  0
008300941209     D*-------------------
008400941209     D* DS COMPOSIZIONE DATA SPEDIZIONE
008500941209     D*-------------------
008600941209     D                 DS
008700941209     D  TASAAS                 1      4  0
008800941209     D  TASMGS                 5      8  0
008900941209     D  TASDSP                 1      8  0
009000000000     D*-------------------
009100000000     D* PARAMETRI PASSAGGIO
009200000000     D*-------------------
009300950626     D PARAM           DS
009400000000     D  PARANN                 1      4  0
009500000000     D  PARMMI                 5      6  0
009600000000     D  PARMMF                 7      8  0
009700000000     D  PARSNC                 9      9
009800000000     D  PARSDC                10     10
009900000000     D  PARSNA                11     11
010000000000     D  PARSDA                12     12
010100000000     D  PARSNR                13     13
010200000000     D  PARDIV                14     14
010300000000     D  PARARE                15     17  0
010400000000     D  PARFLP                18     20  0
010500000000     D  PARTFP                21     23  0
010600000000     D  PARFLA                24     26  0
010700000000     D  PARTFA                27     29  0
010800000000     D  PARDTI                30     37  0
010900000000     D  PARDTF                38     45  0
011000000000     D  PARTLA                46     46
011100000000     D  PARSNP                47     47
011200950712     D  PARTOT                48     48
011300950712     D  PARIOE                49     49
011400970826     D  PARCFP                50     50
011500000216     D*---
011600000216     D* DS X LETTURA DESCRIZIONE 143 ORGANIGRAMMA (X DPD)
011700020702     D*------------------
011800000216     D OG143         E DS
011900020702     D*------------------
012000020702     D* DS PER LETTURA TABELLA "NTW"
012100020702     D*------------------
012200020702     D DNTW          E DS
012300000000     C*------------------------------------------------------------------------*
012400000000     C* MAIN LINE
012500000000     C*------------------------------------------------------------------------*
012600000000     C*
012700000000     C* SE NON DEVE ESSERE ESEGUITO IL PGM VADO A FINE
012800000000IF  1C     FINL00        IFEQ      '0'
012900000000     C*
013000000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
013100000000     C                   EXSR      SETFIL
013200000000     C*
013300000000     C* CICLO FINO A FINE FILE
013400000000DO  2C     FINL00        DOWEQ     '0'
013500000000     C*
013600000000     C* CREA IL RECORD SUI FILE DI LAVORO
013700000000     C                   EXSR      CREARE
013800000000     C*
013900000000     C* LEGGE IL RECORD SUCCESSIVO
014000000000     C                   EXSR      LETFIL
014100000000     C*
014200000000E   2C                   ENDDO
014300000000E   1C                   ENDIF
014400000000     C*
014500000000     C                   SETON                                        LR
014600000000     C*------------------------------------------------------------------------*
014700000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE
014800000000     C*------------------------------------------------------------------------*
014900000000     C     SETFIL        BEGSR
015000000000     C*
015100000000     C* POSIZIONAMENTO E PRIMA LETTURA
015200000000     C                   Z-ADD     PARDCI        KBLDCM
015300041021     C     KEYTAS        SETLL     TITAS26C                           99        *FINE-ARCHIV
015400000000     C*
015500000000     C     *IN99         IFEQ      '1'
015600000000     C                   MOVEL     '1'           FINL00
015700000000     C                   ELSE
015800000000     C                   EXSR      LETFIL
015900000000     C                   END
016000000000     C*
016100000000     C                   ENDSR
016200000000     C*------------------------------------------------------------------------*
016300000000     C* LETFIL - LETTURA PROSSIMO RECORD
016400000000     C*---------------------------------------------------------------------*
016500000000     C     LETFIL        BEGSR
016600000000     C*
016700000000     C* LEGGO FINO A:
016800000000     C*     - FINE FILE O SUPERATO LIMITE FINALE
016900000000     C*     - TROVATO RECORD VALIDO
017000000000     C                   MOVEL     'N'           WRECOK
017100000000     C     FINL00        DOWEQ     '0'                                          --- 1 -->
017200000000     C     WRECOK        ANDEQ     'N'
017300000000     C*
017400000000     C* LETTURE SUCCESSIVE ALLA PRIMA
017500041021     C                   READ      TITAS26C                               99
017600000000     C*
017700000000     C     *IN99         IFEQ      '1'                                          *FINE FILE
017800000000     C     TASDCM        ORGT      PARDCF                                       *SUPERATA DATA FINE
017900000000     C                   MOVEL     '1'           FINL00
018000000000     C                   ELSE
018100000000     C                   EXSR      CHKREC                                       *CONTR.VALIDITA REC.
018200000000     C                   END
018300000000     C                   END                                                    <-- 1 ---
018400000000     C*
018500000000     C                   ENDSR
018600000000     C*---------------------------------------------------------------------*
018700000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
018800000000     C*---------------------------------------------------------------------*
018900000000     C     CHKREC        BEGSR
019000000000     C*
019100000000     C                   MOVEL     'S'           WRECOK            1
019200990608     C*
019300990608     C* SOLO BOLLE CONSEGNATE
019400990608     C     TASDCM        IFEQ      *ZEROS
019500990608     C                   MOVEL     'N'           WRECOK
019600990608     C                   ENDIF
019700100209     C* SOLO BOLLE "D"
019800100209     C     TAStsp        IFne      'D'
019900100209     C                   MOVEL     'N'           WRECOK
020000100209     C                   ENDIF
020100950530     C*
020200950530     C* NON CONSIDERA LE BOLLE CON UNA CONSEGNA ANOMALA
020300950530     C     TASCCA        IFNE      *BLANKS
020400950530     C                   MOVEL     'N'           WRECOK
020500950530     C                   ENDIF
020600950530     C*
020700950530     C* PRENDE SOLO LE BOLLE VALIDE SIA COME "SPEDIZIONE" CHE COME "RICAVO"
020800000706     C     WRECOK        IFEQ      'S'
020900950530     C                   Z-ADD     1             J
021000950530     C                   SETOFF                                           28
021100950530     C     TASTBL        LOOKUP    TB(J)                                  28
021200950530     C     SP(J)         IFEQ      'N'
021300950530     C     RI(J)         OREQ      'N'
021400950530     C                   MOVEL     'N'           WRECOK
021500950530     C                   ENDIF
021600000706     C                   ENDIF
021700000706     C*
021800000706     C* ESCLUDE LE BOLLE CON LINEA PARTENZA POSTE
021900100125IF  1C**   WRECOK        IFEQ      'S'
022000100125     C**                 Z-ADD     1             J
022100100125     C**                 SETOFF                                           28
022200100125     C**   TASLNP        LOOKUP    FIL(J)                                 28
022300100125IF  2C**   *IN28         IFEQ      *OFF
022400100125     C*+   POS(J)        OREQ      'PPT'
022500100125     C**                 MOVEL     'N'           WRECOK
022600100125E   2C**                 ENDIF
022700100125E   1C**                 ENDIF
022800000706     C*
022900000706     C* ESCLUDE LE BOLLE CON LINEA ARRIVO POSTE
023000100125IF  1C**   WRECOK        IFEQ      'S'
023100100125     C**                 Z-ADD     1             J
023200100125     C**                 SETOFF                                           28
023300100125     C**   TASLNA        LOOKUP    FIL(J)                                 28
023400100125IF  2C**   *IN28         IFEQ      *OFF
023500100125     C**   POS(J)        OREQ      'PPT'
023600100125     C**                 MOVEL     'N'           WRECOK
023700100125E   2C**                 ENDIF
023800100126E   1C**                 ENDIF
023900950530     C*
024000100125     C* RECUPERA DIVISIONE ED AREA
024100100125     c* ES 25 01 10 --> al momento le parzializzaizoni per divisione e area
024200100125     c**                sono state tolte per cui non mi preoccupo
024300100125     c**                se fanno riferimento alla linea e non al terminal per l'aff.partenza
024400000706IF  0C     WRECOK        IFEQ      'S'
024500100125     C                   Z-ADD     TASLNP        WFIL              3 0
024600971204     C                   EXSR      DATFIL
024700000000     C                   MOVEL     WDIV          WDIVP             1
024800000000     C                   Z-ADD     WARE          WAREP             3 0
024900971204     C                   MOVEL     WFL1          DLPFL1            1
025000000000     C*
025100000000     C* RECUPERA DIVISIONE ED AREA ARRIVO
025200000000     C                   Z-ADD     TASLNA        WFIL
025300971204     C                   EXSR      DATFIL
025400000000     C                   MOVEL     WDIV          WDIVA             1
025500000000     C                   Z-ADD     WARE          WAREA             3 0
025600971204     C                   MOVEL     WFL1          DLAFL1            1
025700000000     C*
025800000000     C* SE LANCIO ESTEMPORANEO NON CARICO I RECORD ESCLUSI DALLE PARZIALIZ.NI
025900000000IF  1C     PARTLA        IFEQ      'E'
026000000000     C*
026100000000     C* SE DIVISIONE DEL RECORD (PART O ARR) <> DIVISIONE RICHIESTA, ESCLUDO
026200000000IF  2C     PARDIV        IFNE      *BLANKS
026300000000IF  3C     WDIVP         IFEQ      PARDIV
026400000000     C     WDIVA         OREQ      PARDIV
026500000000X   3C                   ELSE
026600000000     C                   MOVEL     'N'           WRECOK
026700000000E   3C                   ENDIF
026800000000E   2C                   ENDIF
026900000000     C*
027000000000     C* SE AREA DEL RECORD (PART O ARR) <> AREA RICHIESTA, ESCLUDO
027100000000IF  2C     PARARE        IFNE      *ZEROS
027200000000IF  3C     WAREP         IFEQ      PARARE
027300000000     C     WAREA         OREQ      PARARE
027400000000X   3C                   ELSE
027500000000     C                   MOVEL     'N'           WRECOK
027600000000E   3C                   ENDIF
027700000000E   2C                   ENDIF
027800000000     C*
027900000000     C* SE FILIALE PARTENZA DEL RECORD <> FILIALE PARTENZA RICHIESTA, ESCLUDO
028000000000IF  2C     PARFLP        IFNE      *ZEROS
028100000000IF  3C     TASLNP        IFNE      PARFLP
028200000000     C                   MOVEL     'N'           WRECOK
028300000000E   3C                   ENDIF
028400000000E   2C                   ENDIF
028500000000     C*
028600000000     C* SE TERMINAL PARTENZA DEL RECORD <> TERMINAL PARTENZA RICHIESTA, ESCLUDO
028700000000IF  2C     PARTFP        IFNE      *ZEROS
028800000000IF  3C     TASTFP        IFNE      PARTFP
028900000000     C                   MOVEL     'N'           WRECOK
029000000000E   3C                   ENDIF
029100000000E   2C                   ENDIF
029200000000     C*
029300000000     C* SE FILIALE ARRIVO DEL RECORD <> FILIALE ARRIVO RICHIESTA, ESCLUDO
029400000000IF  2C     PARFLA        IFNE      *ZEROS
029500000000IF  3C     TASLNA        IFNE      PARFLA
029600000000     C                   MOVEL     'N'           WRECOK
029700000000E   3C                   ENDIF
029800000000E   2C                   ENDIF
029900000000     C*
030000000000     C* SE TERMINAL ARRIVO DEL RECORD <> TERMINAL ARRIVO RICHIESTA, ESCLUDO
030100000000IF  2C     PARTFA        IFNE      *ZEROS
030200000000IF  3C     TASTFA        IFNE      PARTFA
030300000000     C                   MOVEL     'N'           WRECOK
030400000000E   3C                   ENDIF
030500000000E   2C                   ENDIF
030600000000     C*
030700000000E   1C                   ENDIF
030800950530E   0C                   ENDIF
030900000000     C*
031000000000     C                   ENDSR
031100000000     C*------------------------------------------------------------------------*
031200000000     C* CREARE - CREA I RECORD SUI FILE DI LAVORO
031300000000     C*------------------------------------------------------------------------*
031400000000     C     CREARE        BEGSR
031500970827     C*---
031600970827     C* NB: SE IL LANCIO E' PER SALDI RIGENERA I W.F. X TUTTE LE STATISTICHE
031700000000     C*       SE IL LANCIO E' ESTEMPORANEO SOLO PER LE STATISTICHE RICHIESTE
031800970827     C* NB: SE IL LANCIO E' PER SALDI RIGENERA I FILE PC X TUTTE LE STATISTICHE
031900970827     C*       SE IL LANCIO E' ESTEMPORANEO SOLO PER LE STATISTICHE RICHIESTE
032000000000     C*-------------------
032100950626     C* P A R T E N Z A
032200000000     C*-------------------
032300000000     C*
032400950321     C* CERCA IL RANGE A CUI SOMMARE LA SPEDIZIONE (1,2,3,4,5...)
032500100209     C                   Z-ADD     TASNPI        WNUORE            3 0          *ORE NON CONFORMITA'
032600950321     C                   Z-ADD     *ZEROS        WRANGE
032700950321     C                   EXSR      CRANGE
032800950626     C*
032900950626     C* RECUPERA IL FLAG DEL TIPO DI DATI IN STATISTICA (I,X,M)
033000100209     C                   MOVEL     'P'           WSTA              1            *TIPO STATISTICA
033100950626     C                   MOVEL     *BLANKS       WDA1                           *TIPO DATO 1
033200950626     C                   MOVEL     *BLANKS       WDA2                           *TIPO DATO 2
033300950626     C                   EXSR      CREIOE
033400950626     C*
033500950626     C* SCRIVE O RISCRIVE IL FILE DI LAVORO PER L'AFFIDABILITA' PARTENZA
033600970827IF  3C     WDA1          IFNE      *BLANKS                                      *SEMPRE
033700950626     C                   MOVEL     WDA1          WIOE                           *TIPO DATI IN STAT.
033800950626     C                   EXSR      SWFDLP
033900970827E   3C                   ENDIF
034000970827IF  3C     WDA2          IFNE      *BLANKS
034100950626     C                   MOVEL     WDA2          WIOE                           *TIPO DATI IN STAT.
034200950626     C                   EXSR      SWFDLP
034300970827E   3C                   ENDIF
034400970827     C*
034500000000     C*
034600000000     C                   ENDSR
034700950626     C*------------------------------------------------------------------------*
034800950626     C* SWFDLP - SCRIVE O RISCRIVE FILE DI LAVORO PER L'AFFIDABILITA' PARTENZA *
034900950626     C*------------------------------------------------------------------------*
035000950626     C     SWFDLP        BEGSR
035100100126     c                   EXSR      RecupAreaP
035200950626     C*
035300950626     C* CONTROLLA ESISTENZA RECORD
035400950626     C                   Z-ADD     WAACM         KWPANN
035500950626     C                   MOVEL     WDIVP         KWPDIV
035600950626     C                   Z-ADD     WAREP         KWPARE
035700950628     C                   Z-ADD     WLNP          KWPFLP
035800100125     C                   Z-ADD     Wtfp          KWPtfp
035900950626     C                   Z-ADD     WMMCM         KWPMES
036000950626     C                   MOVEL     TASTSP        KWPTSP
036100950626     C                   Z-ADD     WRANGE        KWPRAN
036200950626     C                   MOVEL     WIOE          KWPIOE
036300100127     C     KEYWFP        CHAIN     WFDLP02L                           99
036400950626IF  1C     *IN99         IFEQ      *ON                                          N O N   E S I S T E
036500950626     C                   Z-ADD     KWPANN        WFPANN
036600950626     C                   Z-ADD     KWPMES        WFPMES
036700950626     C                   MOVEL     KWPDIV        WFPDIV
036800950626     C                   Z-ADD     KWPARE        WFPARE
036900950628     C                   Z-ADD     WLNP          WFPFLP
037000950628     C                   Z-ADD     WTFP          WFPTFP
037100950626     C                   MOVEL     KWPTSP        WFPTSP
037200950626     C                   Z-ADD     KWPRAN        WFPRAN
037300950626     C                   MOVEL     KWPIOE        WFPIOE
037400950626     C                   EXSR      AZZWFP                                       *AZZERA CAMPI RANGE
037500950626     C                   EXSR      ADDWFP                                       *AGGIUNGE SPED.AL RE
037600950626     C                   WRITE     WFDLP
037700950626X   1C                   ELSE                                                   E S I S T E
037800950626     C                   EXSR      ADDWFP                                       *AGGIUNGE SPED.AL RE
037900950626     C                   UPDATE    WFDLP
038000950626E   1C                   ENDIF
038100950626     C*
038200950626     C                   ENDSR
038300100126     C*------------------------------------------------------------------------*
038400100126     C* Per l'afidabilità partenza recupero in modi diveris area e dir.commerciale
038500100126     C*------------------------------------------------------------------------*
038600100126     c     RecupAreaP    BEGSR
038700100126     c
038800100126     c                   select
038900100126     C* SE DATI IN STATISTICA = 'X'-EXPORT INVERTE L'ORDINAMENTO
039000100126IF  1C     WIOE          wheneq    'X'
039100100126     C                   MOVEL     TASLNA        WLNP                           *INVERTITO
039200100126     C***                MOVEL     TASTFA        WTFP
039300100126     c* Per linea di arrivo non scrivo il terminal di arrivo
039400100126     c*     per cui l'area è quella della linea
039500100126     c                   clear                   wtfp
039600100126     C*
039700100126     C* AREA E DIVISIONE linea di arrivo
039800100126     C                   Z-ADD     WLNP          WFIL              3 0
039900100126     c*
040000100126     c* SE DATI IN STATISTICA = 'M'-IMPORT
040100100126IF  1C     WIOE          wheneq    'M'
040200100126     C                   MOVEL     TASLNP        WLNP                           *NORMALE
040300100126     c                   clear                   wtfp
040400100126     C* AREA E DIVISIONE linea di partenza
040500100126     C                   Z-ADD     WLNP          WFIL              3 0
040600100126
040700100126     c* SE DATI IN STATISTICA = 'I'-ITALIA
040800100126X   1C                   other
040900100126     C                   MOVEL     TASLNP        WLNP                           *NORMALE
041000100126     C                   MOVEL     TASTFP        WTFP
041100100126     C*
041200100126     C* AREA E DIVISIONE terminal di PARTENZA
041300100126     C**                 Z-ADD     TASLNP        WFIL              3 0
041400100126     C                   Z-ADD     TAStfp        WFIL              3 0
041500100126     c                   ENDSL
041600100126
041700100126     C                   EXSR      DATFIL
041800100126     C                   MOVEL     WDIV          WDIVP             1
041900100126     C                   Z-ADD     WARE          WAREP             3 0
042000100126
042100100126     c                   ENDSR
042200000000     C*------------------------------------------------------------------------*
042300000000     C* CRANGE - CONTROLLA IN QUALE RANGE RIENTRA IL RITARDO
042400000000     C* INPUT:  -WNUORE  (NUMERO DI ORE)
042500000000     C* OUTPUT: -WRANGE  (RANGE (0,1,2,3,4,5))
042600000000     C*------------------------------------------------------------------------*
042700000000     C     CRANGE        BEGSR
042800000000     C*
042900100209     C     WNUORE        IFeq      *ZEROS
043000100209     C                   Z-ADD     0             WRANGE            2 0
043100000000     C                   END
043200100209     C     WNUORE        IFLt      *ZEROS
043300100209     C                   Z-ADD     -1            WRANGE            2 0
043400100209     C                   END
043500000000     C     WNUORE        IFGT      *ZEROS
043600000000     C     WNUORE        ANDLE     12
043700000000     C                   Z-ADD     2             WRANGE
043800000000     C                   END
043900000000     C     WNUORE        IFGT      12
044000000000     C     WNUORE        ANDLE     24
044100000000     C                   Z-ADD     3             WRANGE
044200000000     C                   END
044300000000     C     WNUORE        IFGT      24
044400000000     C     WNUORE        ANDLE     48
044500000000     C                   Z-ADD     4             WRANGE
044600000000     C                   END
044700000000     C     WNUORE        IFGT      48
044800000000     C     WNUORE        ANDLE     72
044900000000     C                   Z-ADD     5             WRANGE
045000000000     C                   END
045100000000     C     WNUORE        IFGT      72
045200000000     C                   Z-ADD     6             WRANGE
045300000000     C                   END
045400000000     C*
045500000000     C                   ENDSR
045600950626     C*------------------------------------------------------------------------*
045700950626     C* CREIOE - RECUPERA IL TIPO DI DATO IN STATISTICA (I,X,M)
045800950626     C*        INPUT : WSTA - TIPO STATISTICA
045900950626     C*        OUTPUT: WDA1 - PRIMO   VALORE
046000950626     C*                WDA2 - SECONDO VALORE
046100950626     C*------------------------------------------------------------------------*
046200950626     C     CREIOE        BEGSR
046300950626     C*--------------------
046400950626     C* P A R T E N Z A
046500950626     C*--------------------
046600100209     c                   select
046700950626     C     WSTA          WHENEQ    'P'
046800971204IF  2C     DLPFL1        IFEQ      *BLANKS                                      *LNP=ITALIA
046900950626     C                   MOVEL     'I'           WDA1                           ITALIA
047000971204IF  3C     DLAFL1        IFNE      *BLANKS                                      *LNA=ESTERO
047100950626     C                   MOVEL     'X'           WDA2                           EXPORT
047200950626E   3C                   ENDIF
047300971204X   2C                   ELSE                                                   *LNP=ESTERO
047400971204IF  3C     DLAFL1        IFEQ      *BLANKS                                      *LNA=ITALIA
047500950626     C                   MOVEL     'M'           WDA1                           IMPORT
047600950626E   3C                   ENDIF
047700950626E   2C                   ENDIF
047800950626     C*
047900950626E   1C                   ENDSL
048000950626     C*
048100950626     C                   ENDSR
048200000000     C*------------------------------------------------------------------------*
048300000000     C* ADDWFP - AGGIUNGE LA SPEDIZIONE AL RECORD WFDLP00F
048400000000     C*------------------------------------------------------------------------*
048500000000     C     ADDWFP        BEGSR
048600000000     C*
048700000000     C                   ADD       1             WFPNSP
048800000000     C*
048900000000     C                   ENDSR
049000000000     C*------------------------------------------------------------------------*
049100000000     C* AZZWFP - AZZERA CAMPI RANGE IN WFDLP00F
049200000000     C*------------------------------------------------------------------------*
049300000000     C     AZZWFP        BEGSR
049400000000     C*
049500000000     C                   Z-ADD     *ZEROS        WFPNSP
049600000000     C*
049700000000     C                   ENDSR
049800000000     C*------------------------------------------------------------------------*
049900971204     C* DATFIL - RECUPERA DATI FILIALE
050000000000     C*------------------------------------------------------------------------*
050100971204     C     DATFIL        BEGSR
050200000000     C*
050300000000     C                   Z-ADD     1             I
050400000000     C     WFIL          LOOKUP    FIL(I)                                 99
050500000000     C     *IN99         IFEQ      '0'                                          --- 3 -->
050600000000     C                   MOVEL     '*'           WDIV              1
050700000000     C                   Z-ADD     999           WARE              3 0
050800971204     C                   MOVEL     *BLANKS       WFL1              1
050900000000     C                   ELSE                                                   <-- 3 -->
051000000000     C                   MOVEL     DIV(I)        WDIV
051100000000     C                   Z-ADD     ARE(I)        WARE
051200971204     C                   MOVEL     FL1(I)        WFL1
051300000000     C                   END                                                    <-- 3 ---
051400000000     C*
051500000000     C                   ENDSR
051600000000     C*------------------------------------------------------------------------*
051700000000     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
051800000000     C*---------------------------------------------------------------------*
051900000000     C     CARORG        BEGSR
052000000000     C*
052100000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
052200000000     C                   Z-ADD     *ZEROS        KORFIL
052300000000     C     KEYORG        SETLL     AZORG01L                           99        *FINE-ARCHIV
052400000000     C  N99              READ      AZORG01L                               99
052500000000     C                   Z-ADD     0             I                 3 0          *INDICE SCHIERA
052600000000     C     *IN99         DOWEQ     '0'                                          --- 1 -->
052700000000     C     ORGFVA        IFEQ      *BLANKS                                      --- 2 -->
052800000000     C     ORGFAG        IFNE      'V'
052900000706     C                   CLEAR                   OG143
053000000706     C                   MOVEL     ORGDE3        OG143
053100000000     C                   ADD       1             I
053200000000     C                   Z-ADD     ORGFIL        FIL(I)
053300000000     C                   Z-ADD     ORGCAR        ARE(I)
053400000000     C                   MOVEL     ORGFL3        DIV(I)
053500020702     C*
053600020702     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
053700020702     C                   CLEAR                   DNTW
053800020702     C*
053900020702     C                   MOVEL(P)  'NTW'         KTECOD
054000020702     C                   MOVEL(P)  §OGNTW        KTEKE1
054100020702     C     KEYTBE        CHAIN     TNTBE01L
054200020702     C                   IF        %found(TNTBE01L)
054300020702     C                   MOVEL     TBEUNI        DNTW
054400020702     C                   ENDIF
054500020702     C*
054600020702     C                   MOVEL     §NTWFIE       FL1(I)
054700020702     C                   IF        §OGNTW = 'DPD'
054800020702     C                   MOVEL     'E'           FL1(I)
054900020702     C                   ENDIF
055000020627     C*
055100100125     C*****              MOVEL     §OGNTW        POS(I)
055200000000     C                   END
055300000000     C                   END                                                    <-- 2 ---
055400000000     C                   READ      AZORG01L                               99
055500000000     C                   END                                                    <-- 1 ---
055600000000     C*
055700000000     C                   ENDSR
055800000000     C*---------------------------------------------------------------------*
055900000000     C* CANWFP - CANCELLA PER WFDLP00F TUTTI I RECORD DEI MESI RICHIESTI
056000000000     C*---------------------------------------------------------------------*
056100000000     C     CANWFP        BEGSR
056200000000     C*
056300000000     C                   Z-ADD     PARANN        KWPANN
056400000000     C                   MOVEL     *BLANKS       KWPDIV
056500000000     C                   Z-ADD     *ZEROS        KWPARE
056600000000     C                   Z-ADD     *ZEROS        KWPFLP
056700100127     C                   Z-ADD     *ZEROS        KWPtfp
056800000000     C                   Z-ADD     *ZEROS        KWPMES
056900000000     C                   MOVEL     *BLANKS       KWPTSP
057000000000     C                   Z-ADD     *ZEROS        KWPRAN
057100950626     C                   MOVEL     *BLANKS       KWPIOE
057200100127     C     KEYWFP        SETLL     WFDLP02L                           99        *FINE ARCHIVIO
057300100127     C  N99              READ      WFDLP02L                               99
057400000000DO  1C     *IN99         DOWEQ     '0'
057500000000     C     WFPANN        ANDEQ     PARANN
057600000000IF  2C     WFPMES        IFGE      PARMMI
057700000000     C     WFPMES        ANDLE     PARMMF
057800000000     C*
057900000000     C                   DELETE    WFDLP
058000000000E   2C                   ENDIF
058100100127     C                   READ      WFDLP02L                               99
058200000000E   1C                   ENDDO
058300000000     C*
058400000000     C                   ENDSR
058500000000     C*------------------------------------------------------------------------*
058600000000     C* CARTBL - CARICA I TIPI BOLLA CON RELATIVO RAGGRUPPAMENTO E FLAGS
058700000000     C*------------------------------------------------------------------------*
058800000000     C     CARTBL        BEGSR
058900000000     C*--------------------
059000000000     C* FLAG SPEDIZIONI /RICAVI PER RAGGRUPPAMENTO TIPO BOLLA
059100000000     C*--------------------
059200000000     C                   Z-ADD     0             I
059300000000     C                   Z-ADD     1             KTBUTE
059400000000     C                   MOVEL     '2W'          KTBCOD
059500000000     C                   MOVEL     *BLANKS       KTBKEY
059600000000     C     KEYTAB        SETLL     TABEL00F
059700000000     C                   READ      TABEL00F                               99
059800000000     C     *IN99         DOWEQ     *OFF
059900000000     C     TBLCOD        ANDEQ     '2W'
060000000000     C     TBLFLG        IFEQ      *BLANKS
060100000000     C                   ADD       1             I
060200000000     C                   MOVEL     TBLKEY        RBL(I)                         *RAGGRUPPAMENTO BOLL
060300000000     C                   MOVEL     TBLUNI        DS2W
060400000000     C                   MOVEL     §2WFSP        FSP(I)                         *FLAG SPEDIZIONE
060500000000     C                   MOVEL     §2WFRI        FRI(I)                         *FLAG RICAVO
060600000000     C                   END
060700000000     C                   READ      TABEL00F                               99
060800000000     C                   END
060900000000     C*--------------------
061000000000     C* TIPI BOLLA E RELATIVI FLAGS SPEDIZIONE/RICAVO
061100000000     C*--------------------
061200000000     C                   Z-ADD     0             I
061300000000     C                   Z-ADD     1             KTBUTE
061400000000     C                   MOVEL     'TB'          KTBCOD
061500000000     C                   MOVEL     *BLANKS       KTBKEY
061600000000     C     KEYTAB        SETLL     TABEL00F
061700000000     C                   READ      TABEL00F                               99
061800000000     C     *IN99         DOWEQ     *OFF
061900000000     C     TBLCOD        ANDEQ     'TB'
062000000000     C     TBLFLG        IFEQ      *BLANKS
062100000000     C                   ADD       1             I
062200000000     C                   MOVEL     TBLKEY        TB(I)                          *TIPO BOLLA
062300000000     C                   MOVEL     TBLUNI        DSTB
062400000000     C                   Z-ADD     1             J                 4 0
062500000000     C                   SETOFF                                       28
062600000000     C     §TBRBL        LOOKUP    RBL(J)                                 28
062700000000     C     *IN28         IFEQ      *ON
062800000000     C                   MOVEL     FSP(J)        SP(I)                          *FLAG SPEDIZIONE
062900000000     C                   MOVEL     FRI(J)        RI(I)                          *FLAG RICAVO
063000000000     C                   ELSE
063100000000     C                   MOVEL     'N'           SP(I)
063200000000     C                   MOVEL     'N'           RI(I)
063300000000     C                   ENDIF
063400000000     C                   END
063500000000     C                   READ      TABEL00F                               99
063600000000     C                   END
063700000000     C*
063800000000     C                   ENDSR
063900000000     C*---------------------------------------------------------------------*
064000000000     C* *INZSR - ROUTINE INIZIALE
064100000000     C*---------------------------------------------------------------------*
064200000000     C     *INZSR        BEGSR
064300000000     C*--------------------
064400000000     C* RICEVIMENTO PARAMETRI
064500000000     C*--------------------
064600000000     C     *ENTRY        PLIST
064700000000     C                   PARM                    KPJBA
064800950626     C                   MOVEL     KPJBU         PARAM
064900000000     C*--------------------
065000000000     C* CAMPI RIFERITI AL DATA BASE
065100000000     C*--------------------
065200990810     C     *LIKE         DEFINE    TASDCM        KBLDCM                         *TITAS00F+10F
065300000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
065400000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE                         *TABEL00F
065500000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
065600950628     C     *LIKE         DEFINE    TBLKEY        KTBKEY
065700970827     C     *LIKE         DEFINE    WFCANN        KWCANN                         *WFDLC00F
065800000000     C     *LIKE         DEFINE    WFCDIV        KWCDIV
065900000000     C     *LIKE         DEFINE    WFCARE        KWCARE
066000000000     C     *LIKE         DEFINE    WFCFLP        KWCFLP
066100000000     C     *LIKE         DEFINE    WFCTFA        KWCTFA
066200000000     C     *LIKE         DEFINE    WFCFLA        KWCFLA
066300000000     C     *LIKE         DEFINE    WFCMES        KWCMES
066400000000     C     *LIKE         DEFINE    WFCTSP        KWCTSP
066500000000     C     *LIKE         DEFINE    WFCRAN        KWCRAN
066600950626     C     *LIKE         DEFINE    WFCIOE        KWCIOE
066700970827     C     *LIKE         DEFINE    PCCANN        KPCANN                         *PCDLC00F
066800970827     C     *LIKE         DEFINE    PCCMES        KPCMES
066900970827     C     *LIKE         DEFINE    PCCDIV        KPCDIV
067000970827     C     *LIKE         DEFINE    PCCARE        KPCARE
067100970827     C     *LIKE         DEFINE    PCCFLP        KPCFLP
067200970827     C     *LIKE         DEFINE    PCCIOE        KPCIOE
067300970827     C     *LIKE         DEFINE    PCCTSP        KPCTSP
067400970827     C     *LIKE         DEFINE    WFAANN        KWAANN                         *WFDLA00F
067500000000     C     *LIKE         DEFINE    WFADIV        KWADIV
067600000000     C     *LIKE         DEFINE    WFAARE        KWAARE
067700000000     C     *LIKE         DEFINE    WFAFLA        KWAFLA
067800000000     C     *LIKE         DEFINE    WFAZNC        KWAZNC
067900950626     C     *LIKE         DEFINE    WFANZD        KWANZD
068000000000     C     *LIKE         DEFINE    WFACAD        KWACAD
068100000000     C     *LIKE         DEFINE    WFAMES        KWAMES
068200000000     C     *LIKE         DEFINE    WFATSP        KWATSP
068300000000     C     *LIKE         DEFINE    WFARAN        KWARAN
068400950626     C     *LIKE         DEFINE    WFAIOE        KWAIOE
068500970827     C     *LIKE         DEFINE    PCAANN        KPAANN                         *PCDLA00F
068600970827     C     *LIKE         DEFINE    PCAMES        KPAMES
068700970827     C     *LIKE         DEFINE    PCADIV        KPADIV
068800970827     C     *LIKE         DEFINE    PCAARE        KPAARE
068900970827     C     *LIKE         DEFINE    PCAFLA        KPAFLA
069000970827     C     *LIKE         DEFINE    PCAIOE        KPAIOE
069100970827     C     *LIKE         DEFINE    PCATSP        KPATSP
069200970827     C     *LIKE         DEFINE    WFPANN        KWPANN                         *WFDLP00F
069300000000     C     *LIKE         DEFINE    WFPDIV        KWPDIV
069400000000     C     *LIKE         DEFINE    WFPARE        KWPARE
069500000000     C     *LIKE         DEFINE    WFPFLP        KWPFLP
069600100125     C     *LIKE         DEFINE    WFPtfp        KWPtfp
069700000000     C     *LIKE         DEFINE    WFPMES        KWPMES
069800000000     C     *LIKE         DEFINE    WFPTSP        KWPTSP
069900000000     C     *LIKE         DEFINE    WFPRAN        KWPRAN
070000950626     C     *LIKE         DEFINE    WFPIOE        KWPIOE
070100970827     C     *LIKE         DEFINE    PCPANN        KPPANN                         *PCDLP00F
070200970827     C     *LIKE         DEFINE    PCPMES        KPPMES
070300970827     C     *LIKE         DEFINE    PCPDIV        KPPDIV
070400970827     C     *LIKE         DEFINE    PCPARE        KPPARE
070500970827     C     *LIKE         DEFINE    PCPFLP        KPPFLP
070600100125     C     *LIKE         DEFINE    PCPtfp        KPPtfp
070700970827     C     *LIKE         DEFINE    PCPIOE        KPPIOE
070800970827     C     *LIKE         DEFINE    PCPTSP        KPPTSP
070900950626     C     *LIKE         DEFINE    WFPIOE        WDA1                           *DI LAVORO
071000950626     C     *LIKE         DEFINE    WFPIOE        WDA2
071100950628     C     *LIKE         DEFINE    WFPIOE        WIOE
071200950628     C     *LIKE         DEFINE    TASLNP        WLNP
071300950628     C     *LIKE         DEFINE    TASTFP        WTFP
071400950628     C     *LIKE         DEFINE    TASLNA        WLNA
071500950628     C     *LIKE         DEFINE    TASTFA        WTFA
071600950628     C     *LIKE         DEFINE    TASZNC        WZNC
071700950628     C     *LIKE         DEFINE    TASNZD        WNZD
071800950628     C     *LIKE         DEFINE    TASCAD        WCAD
071900020702     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
072000020702     C     *LIKE         DEFINE    TBEKE1        KTEKE1
072100950628     C*--------------------
072200000000     C* CHIAVI DI LETTURA
072300000000     C*--------------------
072400041021     C* CHIAVE LETTURA TITAS26C - COMPLETA
072500000000     C     KEYTAS        KLIST
072600000000     C                   KFLD                    KBLDCM
072700000000     C*
072800000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
072900000000     C     KEYORG        KLIST
073000000000     C                   KFLD                    KORFIL
073100000000     C*
073200000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
073300000000     C     KEYTAB        KLIST
073400000000     C                   KFLD                    KTBUTE
073500000000     C                   KFLD                    KTBCOD
073600000000     C                   KFLD                    KTBKEY
073700000000     C*
073800000000     C* CHIAVE LETTURA WFDLC01L - COMPLETA
073900000000     C     KEYWFC        KLIST
074000000000     C                   KFLD                    KWCANN
074100000000     C                   KFLD                    KWCDIV
074200000000     C                   KFLD                    KWCARE
074300000000     C                   KFLD                    KWCFLP
074400000000     C                   KFLD                    KWCTFA
074500000000     C                   KFLD                    KWCFLA
074600000000     C                   KFLD                    KWCMES
074700000000     C                   KFLD                    KWCTSP
074800000000     C                   KFLD                    KWCRAN
074900950626     C                   KFLD                    KWCIOE
075000000000     C*
075100000000     C* CHIAVE LETTURA WFDLA01L - COMPLETA
075200000000     C     KEYWFA        KLIST
075300000000     C                   KFLD                    KWAANN
075400000000     C                   KFLD                    KWADIV
075500000000     C                   KFLD                    KWAARE
075600000000     C                   KFLD                    KWAFLA
075700000000     C                   KFLD                    KWAZNC
075800950626     C                   KFLD                    KWANZD
075900000000     C                   KFLD                    KWACAD
076000000000     C                   KFLD                    KWAMES
076100000000     C                   KFLD                    KWATSP
076200000000     C                   KFLD                    KWARAN
076300950626     C                   KFLD                    KWAIOE
076400000000     C*
076500000000     C* CHIAVE LETTURA WFDLP01L - COMPLETA
076600000000     C     KEYWFP        KLIST
076700000000     C                   KFLD                    KWPANN
076800000000     C                   KFLD                    KWPDIV
076900000000     C                   KFLD                    KWPARE
077000100125     C                   KFLD                    KWPtfp
077100100125     C                   KFLD                    KWPFLP
077200000000     C                   KFLD                    KWPMES
077300000000     C                   KFLD                    KWPTSP
077400000000     C                   KFLD                    KWPRAN
077500950626     C                   KFLD                    KWPIOE
077600970827     C*
077700970827     C* CHIAVE LETTURA PCDLC01L - COMPLETA
077800970827     C     KEYPCC        KLIST
077900970827     C                   KFLD                    KPCANN
078000970827     C                   KFLD                    KPCMES
078100970827     C                   KFLD                    KPCDIV
078200970827     C                   KFLD                    KPCARE
078300970827     C                   KFLD                    KPCFLP
078400970827     C                   KFLD                    KPCIOE
078500970827     C                   KFLD                    KPCTSP
078600970827     C*
078700970827     C* CHIAVE LETTURA PCDLA01L - COMPLETA
078800970827     C     KEYPCA        KLIST
078900970827     C                   KFLD                    KPAANN
079000970827     C                   KFLD                    KPAMES
079100970827     C                   KFLD                    KPADIV
079200970827     C                   KFLD                    KPAARE
079300970827     C                   KFLD                    KPAFLA
079400970827     C                   KFLD                    KPAIOE
079500970827     C                   KFLD                    KPATSP
079600970827     C*
079700970827     C* CHIAVE LETTURA PCDLP01L - COMPLETA
079800970827     C     KEYPCP        KLIST
079900970827     C                   KFLD                    KPPANN
080000970827     C                   KFLD                    KPPMES
080100970827     C                   KFLD                    KPPDIV
080200970827     C                   KFLD                    KPPARE
080300100125     C                   KFLD                    KPPtfp
080400100125     C                   KFLD                    KPPFLP
080500970827     C                   KFLD                    KPPIOE
080600970827     C                   KFLD                    KPPTSP
080700020702     C*
080800020702     C* LETTURA TNTBE01L - PARZIALE
080900020702     C     KEYTBE        KLIST
081000020702     C                   KFLD                    KTECOD                         *UTENTE
081100020702     C                   KFLD                    KTEKE1                         *CODICE
081200000000     C*--------------------
081300000000     C* CAMPI DI CONTROLLO
081400000000     C*--------------------
081500000000     C* IMPOSTAZIONI INIZIALI
081600000000     C                   MOVEL     '0'           FINL00            1            *FINE PGM
081700000000     C***
081800000000     C* LANCIO PER SALDI
081900000000     C***
082000000000     C                   Z-ADD     PARANN        DSANN
082100000000     C                   Z-ADD     PARMMI        DSMES
082200000000     C                   Z-ADD     *ZEROS        DSGIO
082300000000     C                   Z-ADD     DSDAT         PARDCI            8 0          *DATA INIZIALE
082400000000     C                   Z-ADD     PARANN        DSANN
082500000000     C                   Z-ADD     PARMMF        DSMES
082600000000     C                   Z-ADD     99            DSGIO
082700000000     C                   Z-ADD     DSDAT         PARDCF            8 0          *DATA FINALE
082800000000     C*
082900000000     C* CARICA I TIPI BOLLA CON RELATIVI FLAG RICAVI/SPEDIZIONI
083000000000     C                   EXSR      CARTBL
083100000000     C*
083200000000     C* CARICA DA ORGANIGRAMMA TUTTE LE FILIALI CON RELATIVA DIVISIONE E AREA
083300000000     C                   EXSR      CARORG
083400000000     C*
083500000000     C* CANCELLA TUTTI I RECORD DEL PERIODO RICHIESTO DA WFDLP00F
083600000000     C                   EXSR      CANWFP
083700970827     C*
083800000000     C                   ENDSR
