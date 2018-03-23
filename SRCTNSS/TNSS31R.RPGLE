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
001100000000     FWFDLC01L  UF A E           K DISK
001200000000     FWFDLA01L  UF A E           K DISK
001300100127     FWFDLP02L  UF A E           K DISK
001400970827     FPCDLC01L  UF A E           K DISK
001500970827     FPCDLA01L  UF A E           K DISK
001600970827     FPCDLP01L  UF A E           K DISK
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
019700950530     C*
019800950530     C* NON CONSIDERA LE BOLLE CON UNA CONSEGNA ANOMALA
019900950530     C     TASCCA        IFNE      *BLANKS
020000950530     C                   MOVEL     'N'           WRECOK
020100950530     C                   ENDIF
020200950530     C*
020300950530     C* PRENDE SOLO LE BOLLE VALIDE SIA COME "SPEDIZIONE" CHE COME "RICAVO"
020400000706     C     WRECOK        IFEQ      'S'
020500950530     C                   Z-ADD     1             J
020600950530     C                   SETOFF                                           28
020700950530     C     TASTBL        LOOKUP    TB(J)                                  28
020800950530     C     SP(J)         IFEQ      'N'
020900950530     C     RI(J)         OREQ      'N'
021000950530     C                   MOVEL     'N'           WRECOK
021100950530     C                   ENDIF
021200000706     C                   ENDIF
021300000706     C*
021400000706     C* ESCLUDE LE BOLLE CON LINEA PARTENZA POSTE
021500100125IF  1C**   WRECOK        IFEQ      'S'
021600100125     C**                 Z-ADD     1             J
021700100125     C**                 SETOFF                                           28
021800100125     C**   TASLNP        LOOKUP    FIL(J)                                 28
021900100125IF  2C**   *IN28         IFEQ      *OFF
022000100125     C*+   POS(J)        OREQ      'PPT'
022100100125     C**                 MOVEL     'N'           WRECOK
022200100125E   2C**                 ENDIF
022300100125E   1C**                 ENDIF
022400000706     C*
022500000706     C* ESCLUDE LE BOLLE CON LINEA ARRIVO POSTE
022600100125IF  1C**   WRECOK        IFEQ      'S'
022700100125     C**                 Z-ADD     1             J
022800100125     C**                 SETOFF                                           28
022900100125     C**   TASLNA        LOOKUP    FIL(J)                                 28
023000100125IF  2C**   *IN28         IFEQ      *OFF
023100100125     C**   POS(J)        OREQ      'PPT'
023200100125     C**                 MOVEL     'N'           WRECOK
023300100125E   2C**                 ENDIF
023400100126E   1C**                 ENDIF
023500950530     C*
023600100125     C* RECUPERA DIVISIONE ED AREA
023700100125     c* ES 25 01 10 --> al momento le parzializzaizoni per divisione e area
023800100125     c**                sono state tolte per cui non mi preoccupo
023900100125     c**                se fanno riferimento alla linea e non al terminal per l'aff.partenza
024000000706IF  0C     WRECOK        IFEQ      'S'
024100100125     C                   Z-ADD     TASLNP        WFIL              3 0
024200971204     C                   EXSR      DATFIL
024300000000     C                   MOVEL     WDIV          WDIVP             1
024400000000     C                   Z-ADD     WARE          WAREP             3 0
024500971204     C                   MOVEL     WFL1          DLPFL1            1
024600000000     C*
024700000000     C* RECUPERA DIVISIONE ED AREA ARRIVO
024800000000     C                   Z-ADD     TASLNA        WFIL
024900971204     C                   EXSR      DATFIL
025000000000     C                   MOVEL     WDIV          WDIVA             1
025100000000     C                   Z-ADD     WARE          WAREA             3 0
025200971204     C                   MOVEL     WFL1          DLAFL1            1
025300000000     C*
025400000000     C* SE LANCIO ESTEMPORANEO NON CARICO I RECORD ESCLUSI DALLE PARZIALIZ.NI
025500000000IF  1C     PARTLA        IFEQ      'E'
025600000000     C*
025700000000     C* SE DIVISIONE DEL RECORD (PART O ARR) <> DIVISIONE RICHIESTA, ESCLUDO
025800000000IF  2C     PARDIV        IFNE      *BLANKS
025900000000IF  3C     WDIVP         IFEQ      PARDIV
026000000000     C     WDIVA         OREQ      PARDIV
026100000000X   3C                   ELSE
026200000000     C                   MOVEL     'N'           WRECOK
026300000000E   3C                   ENDIF
026400000000E   2C                   ENDIF
026500000000     C*
026600000000     C* SE AREA DEL RECORD (PART O ARR) <> AREA RICHIESTA, ESCLUDO
026700000000IF  2C     PARARE        IFNE      *ZEROS
026800000000IF  3C     WAREP         IFEQ      PARARE
026900000000     C     WAREA         OREQ      PARARE
027000000000X   3C                   ELSE
027100000000     C                   MOVEL     'N'           WRECOK
027200000000E   3C                   ENDIF
027300000000E   2C                   ENDIF
027400000000     C*
027500000000     C* SE FILIALE PARTENZA DEL RECORD <> FILIALE PARTENZA RICHIESTA, ESCLUDO
027600000000IF  2C     PARFLP        IFNE      *ZEROS
027700000000IF  3C     TASLNP        IFNE      PARFLP
027800000000     C                   MOVEL     'N'           WRECOK
027900000000E   3C                   ENDIF
028000000000E   2C                   ENDIF
028100000000     C*
028200000000     C* SE TERMINAL PARTENZA DEL RECORD <> TERMINAL PARTENZA RICHIESTA, ESCLUDO
028300000000IF  2C     PARTFP        IFNE      *ZEROS
028400000000IF  3C     TASTFP        IFNE      PARTFP
028500000000     C                   MOVEL     'N'           WRECOK
028600000000E   3C                   ENDIF
028700000000E   2C                   ENDIF
028800000000     C*
028900000000     C* SE FILIALE ARRIVO DEL RECORD <> FILIALE ARRIVO RICHIESTA, ESCLUDO
029000000000IF  2C     PARFLA        IFNE      *ZEROS
029100000000IF  3C     TASLNA        IFNE      PARFLA
029200000000     C                   MOVEL     'N'           WRECOK
029300000000E   3C                   ENDIF
029400000000E   2C                   ENDIF
029500000000     C*
029600000000     C* SE TERMINAL ARRIVO DEL RECORD <> TERMINAL ARRIVO RICHIESTA, ESCLUDO
029700000000IF  2C     PARTFA        IFNE      *ZEROS
029800000000IF  3C     TASTFA        IFNE      PARTFA
029900000000     C                   MOVEL     'N'           WRECOK
030000000000E   3C                   ENDIF
030100000000E   2C                   ENDIF
030200000000     C*
030300000000E   1C                   ENDIF
030400950530E   0C                   ENDIF
030500000000     C*
030600000000     C                   ENDSR
030700000000     C*------------------------------------------------------------------------*
030800000000     C* CREARE - CREA I RECORD SUI FILE DI LAVORO
030900000000     C*------------------------------------------------------------------------*
031000000000     C     CREARE        BEGSR
031100970827     C*---
031200970827     C* NB: SE IL LANCIO E' PER SALDI RIGENERA I W.F. X TUTTE LE STATISTICHE
031300000000     C*       SE IL LANCIO E' ESTEMPORANEO SOLO PER LE STATISTICHE RICHIESTE
031400970827     C* NB: SE IL LANCIO E' PER SALDI RIGENERA I FILE PC X TUTTE LE STATISTICHE
031500970827     C*       SE IL LANCIO E' ESTEMPORANEO SOLO PER LE STATISTICHE RICHIESTE
031600970827     C*---
031700000000     C     PARTLA        IFEQ      'E'                                          *LANCIO ESTEMPORANEO
031800000000     C                   Z-ADD     *ZEROS        WAACM             4 0
031900000000     C                   Z-ADD     *ZEROS        WMMCM             2 0
032000000000     C                   END
032100000000     C*-------------------
032200950626     C* C L I E N T E
032300000000     C*-------------------
032400000000IF  1C     PARTLA        IFEQ      'S'                                          *PER SALDI
032500000000     C     PARTLA        OREQ      'E'                                          *ESTEMPORANEO
032600000000     C     PARSNC        ANDEQ     'S'                                          *RICHIESTA CLENTE
032700000000     C*
032800000000     C* CERCA IL RANGE A CUI SOMMARE LA SPEDIZIONE (1,2,3,4,5...)
032900000000     C                   Z-ADD     TASNRC        WNUORE            3 0          *ORE RESA ESTERNE->
033000000000     C                   Z-ADD     *ZEROS        WRANGE            2 0
033100000000     C                   EXSR      CRANGE
033200950626     C*
033300950626     C* RECUPERA IL FLAG DEL TIPO DI DATI IN STATISTICA (I,X,M)
033400950626     C                   MOVEL     'C'           WSTA              1            *TIPO STATISTICA
033500950626     C                   MOVEL     *BLANKS       WDA1                           *TIPO DATO 1
033600950626     C                   MOVEL     *BLANKS       WDA2                           *TIPO DATO 2
033700950626     C                   EXSR      CREIOE
033800950626     C*
033900950626     C* SCRIVE O RISCRIVE IL FILE DI LAVORO PER L'AFFIDABILITA' CLIENTE
034000970827IF  2C     PARSNR        IFEQ      'S'                                          *SE RICHIESTO
034100970827IF  3C     WDA1          IFNE      *BLANKS                                      *SEMPRE
034200950626     C                   MOVEL     WDA1          WIOE                           *TIPO DATI IN STAT.
034300950626     C                   EXSR      SWFDLC
034400970827E   3C                   ENDIF
034500970827IF  3C     WDA2          IFNE      *BLANKS
034600950626     C                   MOVEL     WDA2          WIOE                           *TIPO DATI IN STAT.
034700950626     C                   EXSR      SWFDLC
034800970827E   3C                   ENDIF
034900970827E   2C                   ENDIF
035000970827     C*
035100970827     C* SCRIVE O RISCRIVE IL FILE PC PER L'AFFIDABILITA' CLIENTE
035200970827IF  2C     PARCFP        IFEQ      'S'                                          *SE RICHIESTO
035300970827IF  3C     WDA1          IFNE      *BLANKS                                      *SEMPRE
035400970827     C                   MOVEL     WDA1          WIOE                           *TIPO DATI IN STAT.
035500970827     C                   EXSR      SPCDLC
035600970827E   3C                   ENDIF
035700970827IF  3C     WDA2          IFNE      *BLANKS
035800970827     C                   MOVEL     WDA2          WIOE                           *TIPO DATI IN STAT.
035900970827     C                   EXSR      SPCDLC
036000970827E   3C                   ENDIF
036100970827E   2C                   ENDIF
036200000000E   1C                   ENDIF
036300000000     C*-------------------
036400950626     C* C O N S E G N A
036500000000     C*-------------------
036600000000IF  1C     PARTLA        IFEQ      'S'                                          *PER SALDI
036700000000     C     PARTLA        OREQ      'E'                                          *ESTEMPORANEO
036800000000     C     PARSNA        ANDEQ     'S'                                          *RICHIESTA CONSEGNE
036900000000     C*
037000000000     C* CERCA IL RANGE A CUI SOMMARE LA SPEDIZIONE
037100000000     C                   Z-ADD     TASNCI        WNUORE                         *ORE CONS.INTERNE->
037200000000     C                   Z-ADD     *ZEROS        WRANGE
037300000000     C                   EXSR      CRANGE
037400950626     C*
037500950626     C* RECUPERA IL FLAG DEL TIPO DI DATI IN STATISTICA (I,X,M)
037600950626     C                   MOVEL     'A'           WSTA                           *TIPO STATISTICA
037700950626     C                   MOVEL     *BLANKS       WDA1                           *TIPO DATO 1
037800950626     C                   MOVEL     *BLANKS       WDA2                           *TIPO DATO 2
037900950626     C                   EXSR      CREIOE
038000950626     C*
038100950626     C* SCRIVE O RISCRIVE IL FILE DI LAVORO PER L'AFFIDABILITA' CONSEGNA
038200970827IF  2C     PARSNR        IFEQ      'S'                                          *SE RICHIESTO
038300970827IF  3C     WDA1          IFNE      *BLANKS                                      *SEMPRE
038400950626     C                   MOVEL     WDA1          WIOE                           *TIPO DATI IN STAT.
038500950626     C                   EXSR      SWFDLA
038600970827E   3C                   ENDIF
038700970827IF  3C     WDA2          IFNE      *BLANKS
038800950626     C                   MOVEL     WDA2          WIOE                           *TIPO DATI IN STAT.
038900950626     C                   EXSR      SWFDLA
039000970827E   3C                   ENDIF
039100970827E   2C                   ENDIF
039200970827     C*
039300970827     C* SCRIVE O RISCRIVE IL FILE PC PER L'AFFIDABILITA' CONSEGNA
039400970827IF  2C     PARCFP        IFEQ      'S'                                          *SE RICHIESTO
039500970827IF  3C     WDA1          IFNE      *BLANKS                                      *SEMPRE
039600970827     C                   MOVEL     WDA1          WIOE                           *TIPO DATI IN STAT.
039700970827     C                   EXSR      SPCDLA
039800970827E   3C                   ENDIF
039900970827IF  3C     WDA2          IFNE      *BLANKS
040000970827     C                   MOVEL     WDA2          WIOE                           *TIPO DATI IN STAT.
040100970827     C                   EXSR      SPCDLA
040200970827E   3C                   ENDIF
040300970827E   2C                   ENDIF
040400970827E   1C                   ENDIF
040500000000     C*-------------------
040600950626     C* P A R T E N Z A
040700000000     C*-------------------
040800000000IF  1C     PARTLA        IFEQ      'S'                                          *PER SALDI
040900000000     C     PARTLA        OREQ      'E'                                          *ESTEMPORANEO
041000000000     C     PARSNP        ANDEQ     'S'                                          *RICHIESTA PARTENZE
041100000000     C*
041200950321     C* CERCA IL RANGE A CUI SOMMARE LA SPEDIZIONE (1,2,3,4,5...)
041300950321     C                   Z-ADD     TASNPI        WNUORE                         *ORE NON CONFORMITA'
041400950321     C                   Z-ADD     *ZEROS        WRANGE
041500950321     C                   EXSR      CRANGE
041600950626     C*
041700950626     C* RECUPERA IL FLAG DEL TIPO DI DATI IN STATISTICA (I,X,M)
041800950626     C                   MOVEL     'P'           WSTA                           *TIPO STATISTICA
041900950626     C                   MOVEL     *BLANKS       WDA1                           *TIPO DATO 1
042000950626     C                   MOVEL     *BLANKS       WDA2                           *TIPO DATO 2
042100950626     C                   EXSR      CREIOE
042200950626     C*
042300950626     C* SCRIVE O RISCRIVE IL FILE DI LAVORO PER L'AFFIDABILITA' PARTENZA
042400970827IF  2C     PARSNR        IFEQ      'S'                                          *SE RICHIESTO
042500970827IF  3C     WDA1          IFNE      *BLANKS                                      *SEMPRE
042600950626     C                   MOVEL     WDA1          WIOE                           *TIPO DATI IN STAT.
042700950626     C                   EXSR      SWFDLP
042800970827E   3C                   ENDIF
042900970827IF  3C     WDA2          IFNE      *BLANKS
043000950626     C                   MOVEL     WDA2          WIOE                           *TIPO DATI IN STAT.
043100950626     C                   EXSR      SWFDLP
043200970827E   3C                   ENDIF
043300970827E   2C                   ENDIF
043400970827     C*
043500970827     C* SCRIVE O RISCRIVE IL FILE PC PER L'AFFIDABILITA' PARTENZA
043600970827IF  2C     PARCFP        IFEQ      'S'                                          *SE RICHIESTO
043700970827IF  3C     WDA1          IFNE      *BLANKS                                      *SEMPRE
043800970827     C                   MOVEL     WDA1          WIOE                           *TIPO DATI IN STAT.
043900970827     C                   EXSR      SPCDLP
044000970827E   3C                   ENDIF
044100970827IF  3C     WDA2          IFNE      *BLANKS
044200970827     C                   MOVEL     WDA2          WIOE                           *TIPO DATI IN STAT.
044300970827     C                   EXSR      SPCDLP
044400970827E   3C                   ENDIF
044500970827E   2C                   ENDIF
044600950626E   1C                   ENDIF
044700000000     C*
044800000000     C                   ENDSR
044900950626     C*------------------------------------------------------------------------*
045000950626     C* SWFDLC - SCRIVE O RISCRIVE FILE DI LAVORO PER L'AFFIDABILITA' CLIENTE  *
045100950626     C*------------------------------------------------------------------------*
045200950626     C     SWFDLC        BEGSR
045300950628     C*
045400950628     C* SE DATI IN STATISTICA = 'X'-EXPORT INVERTE L'ORDINAMENTO
045500950628IF  1C     WIOE          IFEQ      'X'
045600950628     C                   MOVEL     TASLNA        WLNP                           *INVERTITO
045700950628     C                   MOVEL     TASTFA        WTFP
045800950628     C                   MOVEL     TASLNP        WLNA
045900950628     C                   MOVEL     TASTFP        WTFA
046000970827     C*
046100950703     C* "NUOVA" AREA E DIVISIONE
046200950703     C                   Z-ADD     WLNP          WFIL              3 0
046300971204     C                   EXSR      DATFIL
046400950703     C                   MOVEL     WDIV          WDIVP             1
046500950703     C                   Z-ADD     WARE          WAREP             3 0
046600950628X   1C                   ELSE
046700950628     C                   MOVEL     TASLNP        WLNP                           *NORMALE
046800950628     C                   MOVEL     TASTFP        WTFP
046900950628     C                   MOVEL     TASLNA        WLNA
047000950628     C                   MOVEL     TASTFA        WTFA
047100970828     C*
047200970828     C* "VECCHIA" AREA E DIVISIONE PARTENZA
047300970828     C                   Z-ADD     TASLNP        WFIL              3 0
047400971204     C                   EXSR      DATFIL
047500970828     C                   MOVEL     WDIV          WDIVP             1
047600970828     C                   Z-ADD     WARE          WAREP             3 0
047700950628E   1C                   ENDIF
047800950626     C*
047900950626     C* CONTROLLA ESISTENZA RECORD
048000950626     C                   Z-ADD     WAACM         KWCANN
048100950626     C                   MOVEL     WDIVP         KWCDIV
048200950626     C                   Z-ADD     WAREP         KWCARE
048300950628     C                   Z-ADD     WLNP          KWCFLP
048400950628     C                   Z-ADD     WTFA          KWCTFA
048500950628     C                   Z-ADD     WLNA          KWCFLA
048600950626     C                   Z-ADD     WMMCM         KWCMES
048700950626     C                   MOVEL     TASTSP        KWCTSP
048800950626     C                   Z-ADD     WRANGE        KWCRAN
048900950626     C                   MOVEL     WIOE          KWCIOE
049000950626     C     KEYWFC        CHAIN     WFDLC01L                           99
049100950626IF  1C     *IN99         IFEQ      *ON                                          N O N   E S I S T E
049200950626     C                   Z-ADD     KWCANN        WFCANN
049300950626     C                   Z-ADD     KWCMES        WFCMES
049400950626     C                   MOVEL     KWCDIV        WFCDIV
049500950626     C                   Z-ADD     KWCARE        WFCARE
049600950628     C                   Z-ADD     WLNP          WFCFLP
049700950628     C                   Z-ADD     WTFP          WFCTFP
049800950628     C                   Z-ADD     WLNA          WFCFLA
049900950628     C                   Z-ADD     WTFA          WFCTFA
050000950626     C                   MOVEL     KWCTSP        WFCTSP
050100950626     C                   Z-ADD     KWCRAN        WFCRAN
050200950626     C                   MOVEL     KWCIOE        WFCIOE
050300950626     C                   EXSR      AZZWFC                                       *AZZERA CAMPI RANGE
050400950626     C                   EXSR      ADDWFC                                       *AGGIUNGE SPED.AL RE
050500950626     C                   WRITE     WFDLC
050600950626X   1C                   ELSE                                                   E S I S T E
050700950626     C                   EXSR      ADDWFC                                       *AGGIUNGE SPED.AL RE
050800950626     C                   UPDATE    WFDLC
050900950626E   1C                   ENDIF
051000950626     C*
051100950626     C                   ENDSR
051200970827     C*------------------------------------------------------------------------*
051300970827     C* SPCDLC - SCRIVE O RISCRIVE FILE PC PER L'AFFIDABILITA' CLIENTE         *
051400970827     C*------------------------------------------------------------------------*
051500970827     C     SPCDLC        BEGSR
051600970827     C*
051700970827     C* SE DATI IN STATISTICA = 'X'-EXPORT INVERTE L'ORDINAMENTO
051800970827IF  1C     WIOE          IFEQ      'X'
051900970827     C                   MOVEL     TASLNA        WLNP                           *INVERTITO
052000970827     C                   MOVEL     TASTFA        WTFP
052100970827     C                   MOVEL     TASLNP        WLNA
052200970827     C                   MOVEL     TASTFP        WTFA
052300970827     C*
052400970827     C* "NUOVA" AREA E DIVISIONE
052500970827     C                   Z-ADD     WLNP          WFIL              3 0
052600971204     C                   EXSR      DATFIL
052700970827     C                   MOVEL     WDIV          WDIVP             1
052800970827     C                   Z-ADD     WARE          WAREP             3 0
052900970827X   1C                   ELSE
053000970827     C                   MOVEL     TASLNP        WLNP                           *NORMALE
053100970827     C                   MOVEL     TASTFP        WTFP
053200970827     C                   MOVEL     TASLNA        WLNA
053300970827     C                   MOVEL     TASTFA        WTFA
053400970828     C*
053500970828     C* "VECCHIA" AREA E DIVISIONE PARTENZA
053600970828     C                   Z-ADD     TASLNP        WFIL              3 0
053700971204     C                   EXSR      DATFIL
053800970828     C                   MOVEL     WDIV          WDIVP             1
053900970828     C                   Z-ADD     WARE          WAREP             3 0
054000970827E   1C                   ENDIF
054100970827     C*
054200970827     C* CONTROLLA ESISTENZA RECORD
054300970827     C                   Z-ADD     WAACM         KPCANN
054400970827     C                   Z-ADD     WMMCM         KPCMES
054500970827     C                   MOVEL     WDIVP         KPCDIV
054600970827     C                   Z-ADD     WAREP         KPCARE
054700970827     C                   Z-ADD     WLNP          KPCFLP
054800970827     C                   MOVEL     WIOE          KPCIOE
054900970827     C                   MOVEL     TASTSP        KPCTSP
055000970827     C     KEYPCC        CHAIN     PCDLC01L                           99
055100970827IF  1C     *IN99         IFEQ      *ON                                          I N S E R I M E N T
055200970827     C                   Z-ADD     KPCANN        PCCANN
055300970827     C                   Z-ADD     KPCMES        PCCMES
055400970827     C                   MOVEL     KPCDIV        PCCDIV
055500970827     C                   Z-ADD     KPCARE        PCCARE
055600970827     C                   Z-ADD     KPCFLP        PCCFLP
055700970827     C                   MOVEL     KPCIOE        PCCIOE
055800970827     C                   MOVEL     KPCTSP        PCCTSP
055900970827     C                   EXSR      AZZPCC                                       *AZZERA CAMPI
056000970827     C                   EXSR      ADDPCC                                       *AGGIUNGE CAMPI
056100970827     C                   WRITE     PCDLC000
056200970827X   1C                   ELSE                                                   V A R I A Z I O N E
056300970827     C                   EXSR      ADDPCC                                       *AGGIUNGE CAMPI
056400970827     C                   UPDATE    PCDLC000
056500970827E   1C                   ENDIF
056600970827     C*
056700970827     C                   ENDSR
056800950626     C*------------------------------------------------------------------------*
056900950626     C* SWFDLA - SCRIVE O RISCRIVE FILE DI LAVORO PER L'AFFIDABILITA' CONSEGNA *
057000950626     C*------------------------------------------------------------------------*
057100950626     C     SWFDLA        BEGSR
057200970827     C*
057300970827     C* SE DATI IN STATISTICA = 'M'-IMPORT INVERTE L'ORDINAMENTO
057400970827IF  1C     WIOE          IFEQ      'M'
057500970827     C                   Z-ADD     TASLNP        WLNA                           *NORMALE
057600970827     C                   Z-ADD     TASTFP        WTFA
057700970827     C*
057800970827     C* "NUOVA" AREA E DIVISIONE
057900970827     C                   Z-ADD     WLNA          WFIL
058000971204     C                   EXSR      DATFIL
058100970827     C                   MOVEL     WDIV          WDIVA             1
058200970827     C                   Z-ADD     WARE          WAREA             3 0
058300970827X   1C                   ELSE
058400970827     C                   Z-ADD     TASLNA        WLNA                           *NORMALE
058500970827     C                   Z-ADD     TASTFA        WTFA
058600970828     C*
058700970828     C* "VECCHIA" AREA E DIVISIONE ARRIVO
058800970828     C                   Z-ADD     TASLNA        WFIL
058900971204     C                   EXSR      DATFIL
059000970828     C                   MOVEL     WDIV          WDIVA             1
059100970828     C                   Z-ADD     WARE          WAREA             3 0
059200970827E   1C                   ENDIF
059300970827     C                   Z-ADD     TASZNC        WZNC
059400970827     C                   MOVEL     TASNZD        WNZD
059500970827     C                   MOVEL     TASCAD        WCAD
059600950626     C*
059700950626     C* CONTROLLA ESISTENZA RECORD
059800950626     C                   Z-ADD     WAACM         KWAANN
059900950626     C                   MOVEL     WDIVA         KWADIV
060000950626     C                   Z-ADD     WAREA         KWAARE
060100950628     C                   Z-ADD     WLNA          KWAFLA
060200950628     C                   Z-ADD     WZNC          KWAZNC
060300950628     C                   MOVEL     WNZD          KWANZD
060400950628     C                   MOVEL     WCAD          KWACAD
060500950626     C                   Z-ADD     WMMCM         KWAMES
060600950626     C                   MOVEL     TASTSP        KWATSP
060700950626     C                   Z-ADD     WRANGE        KWARAN
060800950626     C                   MOVEL     WIOE          KWAIOE
060900950626     C     KEYWFA        CHAIN     WFDLA01L                           99
061000950626IF  1C     *IN99         IFEQ      *ON                                          N O N   E S I S T E
061100950626     C                   Z-ADD     KWAANN        WFAANN
061200950626     C                   MOVEL     KWADIV        WFADIV
061300950626     C                   Z-ADD     KWAARE        WFAARE
061400950628     C                   Z-ADD     WLNA          WFAFLA
061500950628     C                   Z-ADD     WZNC          WFAZNC
061600950628     C                   MOVEL     WNZD          WFANZD
061700950628     C                   MOVEL     WCAD          WFACAD
061800950626     C                   Z-ADD     KWAMES        WFAMES
061900950626     C                   MOVEL     KWATSP        WFATSP
062000950626     C                   Z-ADD     KWARAN        WFARAN
062100950628     C                   Z-ADD     WTFA          WFATFA
062200950626     C                   MOVEL     KWAIOE        WFAIOE
062300950626     C                   EXSR      AZZWFA                                       *AZZERA CAMPI RANGE
062400950626     C                   EXSR      ADDWFA                                       *AGGIUNGE SPED.AL RE
062500950626     C                   WRITE     WFDLA
062600950626X   1C                   ELSE                                                   E S I S T E
062700950626     C                   EXSR      ADDWFA                                       *AGGIUNGE SPED.AL RE
062800950626     C                   UPDATE    WFDLA
062900950626E   1C                   ENDIF
063000950626     C*
063100950626     C                   ENDSR
063200970827     C*------------------------------------------------------------------------*
063300970827     C* SPCDLA - SCRIVE O RISCRIVE FILE PC PER L'AFFIDABILITA' CONSEGNA        *
063400970827     C*------------------------------------------------------------------------*
063500970827     C     SPCDLA        BEGSR
063600970827     C*
063700970827     C* SE DATI IN STATISTICA = 'M'-IMPORT INVERTE L'ORDINAMENTO
063800970827IF  1C     WIOE          IFEQ      'M'
063900970827     C                   Z-ADD     TASLNP        WLNA                           *NORMALE
064000970827     C                   Z-ADD     TASTFP        WTFA
064100970827     C*
064200970827     C* "NUOVA" AREA E DIVISIONE
064300970827     C                   Z-ADD     WLNA          WFIL
064400971204     C                   EXSR      DATFIL
064500970827     C                   MOVEL     WDIV          WDIVA             1
064600970827     C                   Z-ADD     WARE          WAREA             3 0
064700970827X   1C                   ELSE
064800970827     C                   Z-ADD     TASLNA        WLNA                           *NORMALE
064900970827     C                   Z-ADD     TASTFA        WTFA
065000970828     C*
065100970828     C* "VECCHIA" AREA E DIVISIONE ARRIVO
065200970828     C                   Z-ADD     TASLNA        WFIL
065300971204     C                   EXSR      DATFIL
065400970828     C                   MOVEL     WDIV          WDIVA             1
065500970828     C                   Z-ADD     WARE          WAREA             3 0
065600970827E   1C                   ENDIF
065700970827     C                   Z-ADD     TASZNC        WZNC
065800970827     C                   MOVEL     TASNZD        WNZD
065900970827     C                   MOVEL     TASCAD        WCAD
066000970827     C*
066100970827     C* CONTROLLA ESISTENZA RECORD
066200970827     C                   Z-ADD     WAACM         KPAANN
066300970827     C                   Z-ADD     WMMCM         KPAMES
066400970827     C                   MOVEL     WDIVA         KPADIV
066500970827     C                   Z-ADD     WAREA         KPAARE
066600970827     C                   Z-ADD     WLNA          KPAFLA
066700970827     C                   MOVEL     WIOE          KPAIOE
066800970827     C                   MOVEL     TASTSP        KPATSP
066900970827     C     KEYPCA        CHAIN     PCDLA01L                           99
067000970827IF  1C     *IN99         IFEQ      *ON                                          I N S E R I M E N T
067100970827     C                   Z-ADD     KPAANN        PCAANN
067200970827     C                   Z-ADD     KPAMES        PCAMES
067300970827     C                   MOVEL     KPADIV        PCADIV
067400970827     C                   Z-ADD     KPAARE        PCAARE
067500970827     C                   Z-ADD     KPAFLA        PCAFLA
067600970827     C                   MOVEL     KPAIOE        PCAIOE
067700970827     C                   MOVEL     KPATSP        PCATSP
067800970827     C                   EXSR      AZZPCA                                       *AZZERA CAMPI
067900970827     C                   EXSR      ADDPCA                                       *AGGIUNGE CAMPI
068000970827     C                   WRITE     PCDLA000
068100970827X   1C                   ELSE                                                   V A R I A Z I O N E
068200970827     C                   EXSR      ADDPCA                                       *AGGIUNGE CAMPI
068300970827     C                   UPDATE    PCDLA000
068400970827E   1C                   ENDIF
068500970827     C*
068600970827     C                   ENDSR
068700950626     C*------------------------------------------------------------------------*
068800950626     C* SWFDLP - SCRIVE O RISCRIVE FILE DI LAVORO PER L'AFFIDABILITA' PARTENZA *
068900950626     C*------------------------------------------------------------------------*
069000950626     C     SWFDLP        BEGSR
069100100126     c                   EXSR      RecupAreaP
069200950626     C*
069300950626     C* CONTROLLA ESISTENZA RECORD
069400950626     C                   Z-ADD     WAACM         KWPANN
069500950626     C                   MOVEL     WDIVP         KWPDIV
069600950626     C                   Z-ADD     WAREP         KWPARE
069700950628     C                   Z-ADD     WLNP          KWPFLP
069800100125     C                   Z-ADD     Wtfp          KWPtfp
069900950626     C                   Z-ADD     WMMCM         KWPMES
070000950626     C                   MOVEL     TASTSP        KWPTSP
070100950626     C                   Z-ADD     WRANGE        KWPRAN
070200950626     C                   MOVEL     WIOE          KWPIOE
070300100127     C     KEYWFP        CHAIN     WFDLP02L                           99
070400950626IF  1C     *IN99         IFEQ      *ON                                          N O N   E S I S T E
070500950626     C                   Z-ADD     KWPANN        WFPANN
070600950626     C                   Z-ADD     KWPMES        WFPMES
070700950626     C                   MOVEL     KWPDIV        WFPDIV
070800950626     C                   Z-ADD     KWPARE        WFPARE
070900950628     C                   Z-ADD     WLNP          WFPFLP
071000950628     C                   Z-ADD     WTFP          WFPTFP
071100950626     C                   MOVEL     KWPTSP        WFPTSP
071200950626     C                   Z-ADD     KWPRAN        WFPRAN
071300950626     C                   MOVEL     KWPIOE        WFPIOE
071400950626     C                   EXSR      AZZWFP                                       *AZZERA CAMPI RANGE
071500950626     C                   EXSR      ADDWFP                                       *AGGIUNGE SPED.AL RE
071600950626     C                   WRITE     WFDLP
071700950626X   1C                   ELSE                                                   E S I S T E
071800950626     C                   EXSR      ADDWFP                                       *AGGIUNGE SPED.AL RE
071900950626     C                   UPDATE    WFDLP
072000950626E   1C                   ENDIF
072100950626     C*
072200950626     C                   ENDSR
072300100126     C*------------------------------------------------------------------------*
072400100126     C* Per l'afidabilità partenza recupero in modi diveris area e dir.commerciale
072500100126     C*------------------------------------------------------------------------*
072600100126     c     RecupAreaP    BEGSR
072700100126     c
072800100126     c                   select
072900100126     C* SE DATI IN STATISTICA = 'X'-EXPORT INVERTE L'ORDINAMENTO
073000100126IF  1C     WIOE          wheneq    'X'
073100100126     C                   MOVEL     TASLNA        WLNP                           *INVERTITO
073200100126     C***                MOVEL     TASTFA        WTFP
073300100126     c* Per linea di arrivo non scrivo il terminal di arrivo
073400100126     c*     per cui l'area è quella della linea
073500100126     c                   clear                   wtfp
073600100126     C*
073700100126     C* AREA E DIVISIONE linea di arrivo
073800100126     C                   Z-ADD     WLNP          WFIL              3 0
073900100126     c*
074000100126     c* SE DATI IN STATISTICA = 'M'-IMPORT
074100100126IF  1C     WIOE          wheneq    'M'
074200100126     C                   MOVEL     TASLNP        WLNP                           *NORMALE
074300100126     c                   clear                   wtfp
074400100126     C* AREA E DIVISIONE linea di partenza
074500100126     C                   Z-ADD     WLNP          WFIL              3 0
074600100126
074700100126     c* SE DATI IN STATISTICA = 'I'-ITALIA
074800100126X   1C                   other
074900100126     C                   MOVEL     TASLNP        WLNP                           *NORMALE
075000100126     C                   MOVEL     TASTFP        WTFP
075100100126     C*
075200100126     C* AREA E DIVISIONE terminal di PARTENZA
075300100126     C**                 Z-ADD     TASLNP        WFIL              3 0
075400100126     C                   Z-ADD     TAStfp        WFIL              3 0
075500100126     c                   ENDSL
075600100126
075700100126     C                   EXSR      DATFIL
075800100126     C                   MOVEL     WDIV          WDIVP             1
075900100126     C                   Z-ADD     WARE          WAREP             3 0
076000100126
076100100126     c                   ENDSR
076200970827     C*------------------------------------------------------------------------*
076300970827     C* SPCDLP - SCRIVE O RISCRIVE FILE PC PER L'AFFIDABILITA' PARTENZA        *
076400970827     C*------------------------------------------------------------------------*
076500970827     C     SPCDLP        BEGSR
076600100126
076700100126     c                   EXSR      RecupAreaP
076800970827     C*
076900970827     C* CONTROLLA ESISTENZA RECORD
077000970827     C                   Z-ADD     WAACM         KPPANN
077100970827     C                   Z-ADD     WMMCM         KPPMES
077200970827     C                   MOVEL     WDIVP         KPPDIV
077300970827     C                   Z-ADD     WAREP         KPPARE
077400100125     C                   Z-ADD     Wtfp          KPPtfp
077500100125     C                   Z-ADD     WLNP          KPPFLP
077600970827     C                   MOVEL     WIOE          KPPIOE
077700970827     C                   MOVEL     TASTSP        KPPTSP
077800970827     C     KEYPCP        CHAIN     PCDLP01L                           99
077900970827IF  1C     *IN99         IFEQ      *ON                                          I N S E R I M E N T
078000970827     C                   Z-ADD     KPPANN        PCPANN
078100970827     C                   Z-ADD     KPPMES        PCPMES
078200970827     C                   MOVEL     KPPDIV        PCPDIV
078300970827     C                   Z-ADD     KPPARE        PCPARE
078400970827     C                   Z-ADD     KPPFLP        PCPFLP
078500100125     C                   Z-ADD     KPPtfp        PCPtfp
078600970827     C                   MOVEL     KPPIOE        PCPIOE
078700970827     C                   MOVEL     KPPTSP        PCPTSP
078800970827     C                   EXSR      AZZPCP                                       *AZZERA CAMPI
078900970827     C                   EXSR      ADDPCP                                       *AGGIUNGE CAMPI
079000970827     C                   WRITE     PCDLP000
079100970827X   1C                   ELSE                                                   V A R I A Z I O N E
079200970827     C                   EXSR      ADDPCP                                       *AGGIUNGE CAMPI
079300970827     C                   UPDATE    PCDLP000
079400970827E   1C                   ENDIF
079500970827     C*
079600970827     C                   ENDSR
079700000000     C*------------------------------------------------------------------------*
079800000000     C* CRANGE - CONTROLLA IN QUALE RANGE RIENTRA IL RITARDO
079900000000     C* INPUT:  -WNUORE  (NUMERO DI ORE)
080000000000     C* OUTPUT: -WRANGE  (RANGE (0,1,2,3,4,5))
080100000000     C*------------------------------------------------------------------------*
080200000000     C     CRANGE        BEGSR
080300000000     C*
080400000000     C     WNUORE        IFLE      *ZEROS
080500000000     C                   Z-ADD     1             WRANGE            2 0
080600000000     C                   END
080700000000     C     WNUORE        IFGT      *ZEROS
080800000000     C     WNUORE        ANDLE     12
080900000000     C                   Z-ADD     2             WRANGE
081000000000     C                   END
081100000000     C     WNUORE        IFGT      12
081200000000     C     WNUORE        ANDLE     24
081300000000     C                   Z-ADD     3             WRANGE
081400000000     C                   END
081500000000     C     WNUORE        IFGT      24
081600000000     C     WNUORE        ANDLE     48
081700000000     C                   Z-ADD     4             WRANGE
081800000000     C                   END
081900000000     C     WNUORE        IFGT      48
082000000000     C     WNUORE        ANDLE     72
082100000000     C                   Z-ADD     5             WRANGE
082200000000     C                   END
082300000000     C     WNUORE        IFGT      72
082400000000     C                   Z-ADD     6             WRANGE
082500000000     C                   END
082600000000     C*
082700000000     C                   ENDSR
082800950626     C*------------------------------------------------------------------------*
082900950626     C* CREIOE - RECUPERA IL TIPO DI DATO IN STATISTICA (I,X,M)
083000950626     C*        INPUT : WSTA - TIPO STATISTICA
083100950626     C*        OUTPUT: WDA1 - PRIMO   VALORE
083200950626     C*                WDA2 - SECONDO VALORE
083300950626     C*------------------------------------------------------------------------*
083400950626     C     CREIOE        BEGSR
083500950626     C*--------------------
083600950626     C* C L I E N T E
083700950626     C*--------------------
083800950626SL  1C                   SELECT
083900950626     C     WSTA          WHENEQ    'C'
084000971204IF  2C     DLPFL1        IFEQ      *BLANKS                                      *LNP=ITALIA
084100950626     C                   MOVEL     'I'           WDA1                           ITALIA
084200971204IF  3C     DLAFL1        IFNE      *BLANKS                                      *LNA=ESTERO
084300950626     C                   MOVEL     'X'           WDA2                           EXPORT
084400950626E   3C                   ENDIF
084500971204X   2C                   ELSE                                                   *LNP=ESTERO
084600971204IF  3C     DLAFL1        IFEQ      *BLANKS                                      *LNA=ITALIA
084700950626     C                   MOVEL     'M'           WDA1                           IMPORT
084800950626E   3C                   ENDIF
084900950626E   2C                   ENDIF
085000950626     C*--------------------
085100950626     C* C O N S E G N A
085200950626     C*--------------------
085300950626     C     WSTA          WHENEQ    'A'
085400971204IF  2C     DLAFL1        IFEQ      *BLANKS                                      *LNA=ITALIA
085500950626     C                   MOVEL     'I'           WDA1                           ITALIA
085600971204IF  3C     DLPFL1        IFNE      *BLANKS                                      *LNP=ESTERO
085700950626     C                   MOVEL     'M'           WDA2                           IMPORT
085800950626E   3C                   ENDIF
085900971204X   2C                   ELSE                                                   *LNA=ESTERO
086000971204IF  3C     DLPFL1        IFEQ      *BLANKS                                      *LNP=ITALIA
086100950626     C                   MOVEL     'X'           WDA1                           EXPORT
086200950626E   3C                   ENDIF
086300950626E   2C                   ENDIF
086400950626     C*--------------------
086500950626     C* P A R T E N Z A
086600950626     C*--------------------
086700950626     C     WSTA          WHENEQ    'P'
086800971204IF  2C     DLPFL1        IFEQ      *BLANKS                                      *LNP=ITALIA
086900950626     C                   MOVEL     'I'           WDA1                           ITALIA
087000971204IF  3C     DLAFL1        IFNE      *BLANKS                                      *LNA=ESTERO
087100950626     C                   MOVEL     'X'           WDA2                           EXPORT
087200950626E   3C                   ENDIF
087300971204X   2C                   ELSE                                                   *LNP=ESTERO
087400971204IF  3C     DLAFL1        IFEQ      *BLANKS                                      *LNA=ITALIA
087500950626     C                   MOVEL     'M'           WDA1                           IMPORT
087600950626E   3C                   ENDIF
087700950626E   2C                   ENDIF
087800950626     C*
087900950626E   1C                   ENDSL
088000950626     C*
088100950626     C                   ENDSR
088200000000     C*------------------------------------------------------------------------*
088300000000     C* ADDWFC - AGGIUNGE LA SPEDIZIONE AL RECORD WFDLC00F
088400000000     C*------------------------------------------------------------------------*
088500000000     C     ADDWFC        BEGSR
088600000000     C*
088700000000     C                   ADD       1             WFCNSP
088800000000     C*
088900000000     C                   ENDSR
089000000000     C*------------------------------------------------------------------------*
089100000000     C* ADDWFA - AGGIUNGE LA SPEDIZIONE AL RECORD WFDLA00F
089200000000     C*------------------------------------------------------------------------*
089300000000     C     ADDWFA        BEGSR
089400000000     C*
089500000000     C                   ADD       1             WFANSP
089600000000     C*
089700000000     C                   ENDSR
089800000000     C*------------------------------------------------------------------------*
089900000000     C* ADDWFP - AGGIUNGE LA SPEDIZIONE AL RECORD WFDLP00F
090000000000     C*------------------------------------------------------------------------*
090100000000     C     ADDWFP        BEGSR
090200000000     C*
090300000000     C                   ADD       1             WFPNSP
090400000000     C*
090500000000     C                   ENDSR
090600000000     C*------------------------------------------------------------------------*
090700000000     C* AZZWFC - AZZERA CAMPI RANGE IN WFDLC00F
090800000000     C*------------------------------------------------------------------------*
090900000000     C     AZZWFC        BEGSR
091000000000     C*
091100000000     C                   Z-ADD     *ZEROS        WFCNSP
091200000000     C*
091300000000     C                   ENDSR
091400000000     C*------------------------------------------------------------------------*
091500000000     C* AZZWFA - AZZERA CAMPI RANGE IN WFDLA00F
091600000000     C*------------------------------------------------------------------------*
091700000000     C     AZZWFA        BEGSR
091800000000     C*
091900000000     C                   Z-ADD     *ZEROS        WFANSP
092000000000     C*
092100000000     C                   ENDSR
092200000000     C*------------------------------------------------------------------------*
092300000000     C* AZZWFP - AZZERA CAMPI RANGE IN WFDLP00F
092400000000     C*------------------------------------------------------------------------*
092500000000     C     AZZWFP        BEGSR
092600000000     C*
092700000000     C                   Z-ADD     *ZEROS        WFPNSP
092800000000     C*
092900000000     C                   ENDSR
093000970827     C*------------------------------------------------------------------------*
093100970827     C* ADDPCC - AGGIUNGE CAMPI AL RECORD DI PCDLC00F
093200970827     C*------------------------------------------------------------------------*
093300970827     C     ADDPCC        BEGSR
093400970827     C*
093500970827     C                   ADD       1             PCCNSP                         *SPED TOTALI
093600970827IF  1C     WNUORE        IFLE      *ZEROS
093700970827     C                   ADD       1             PCCNSD                         *SPED IN ORARIO
093800970827E   1C                   ENDIF
093900970827     C*
094000970827     C                   ENDSR
094100970827     C*------------------------------------------------------------------------*
094200970827     C* ADDPCA - AGGIUNGE CAMPI AL RECORD DI PCDLA00F
094300970827     C*------------------------------------------------------------------------*
094400970827     C     ADDPCA        BEGSR
094500970827     C*
094600970827     C                   ADD       1             PCANSP                         *SPED TOTALI
094700970827IF  1C     WNUORE        IFLE      *ZEROS
094800970827     C                   ADD       1             PCANSD                         *SPED IN ORARIO
094900970827E   1C                   ENDIF
095000970827     C*
095100970827     C                   ENDSR
095200970827     C*------------------------------------------------------------------------*
095300970827     C* ADDPCP - AGGIUNGE CAMPI AL RECORD DI PCDLP00F
095400970827     C*------------------------------------------------------------------------*
095500970827     C     ADDPCP        BEGSR
095600970827     C*
095700970827     C                   ADD       1             PCPNSP                         *SPED TOTALI
095800970827IF  1C     WNUORE        IFLE      *ZEROS
095900970827     C                   ADD       1             PCPNSD                         *SPED IN ORARIO
096000970827E   1C                   ENDIF
096100970827     C*
096200970827     C                   ENDSR
096300970827     C*------------------------------------------------------------------------*
096400970827     C* AZZPCC - AZZERA CAMPI AL RECORD DI PCDLC00F
096500970827     C*------------------------------------------------------------------------*
096600970827     C     AZZPCC        BEGSR
096700970827     C*
096800970827     C                   Z-ADD     *ZEROS        PCCNSP                         *SPED TOTALI
096900970827     C                   Z-ADD     *ZEROS        PCCNSD                         *SPED IN ORARIO
097000970827     C*
097100970827     C                   ENDSR
097200970827     C*------------------------------------------------------------------------*
097300970827     C* AZZPCA - AZZERA CAMPI AL RECORD DI PCDLA00F
097400970827     C*------------------------------------------------------------------------*
097500970827     C     AZZPCA        BEGSR
097600970827     C*
097700970827     C                   Z-ADD     *ZEROS        PCANSP                         *SPED TOTALI
097800970827     C                   Z-ADD     *ZEROS        PCANSD                         *SPED IN ORARIO
097900970827     C*
098000970827     C                   ENDSR
098100970827     C*------------------------------------------------------------------------*
098200970827     C* AZZPCP - AZZERA CAMPI AL RECORD DI PCDLP00F
098300970827     C*------------------------------------------------------------------------*
098400970827     C     AZZPCP        BEGSR
098500970827     C*
098600970827     C                   Z-ADD     *ZEROS        PCPNSP                         *SPED TOTALI
098700970827     C                   Z-ADD     *ZEROS        PCPNSD                         *SPED IN ORARIO
098800970827     C*
098900970827     C                   ENDSR
099000000000     C*------------------------------------------------------------------------*
099100971204     C* DATFIL - RECUPERA DATI FILIALE
099200000000     C*------------------------------------------------------------------------*
099300971204     C     DATFIL        BEGSR
099400000000     C*
099500000000     C                   Z-ADD     1             I
099600000000     C     WFIL          LOOKUP    FIL(I)                                 99
099700000000     C     *IN99         IFEQ      '0'                                          --- 3 -->
099800000000     C                   MOVEL     '*'           WDIV              1
099900000000     C                   Z-ADD     999           WARE              3 0
100000971204     C                   MOVEL     *BLANKS       WFL1              1
100100000000     C                   ELSE                                                   <-- 3 -->
100200000000     C                   MOVEL     DIV(I)        WDIV
100300000000     C                   Z-ADD     ARE(I)        WARE
100400971204     C                   MOVEL     FL1(I)        WFL1
100500000000     C                   END                                                    <-- 3 ---
100600000000     C*
100700000000     C                   ENDSR
100800000000     C*------------------------------------------------------------------------*
100900000000     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
101000000000     C*---------------------------------------------------------------------*
101100000000     C     CARORG        BEGSR
101200000000     C*
101300000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
101400000000     C                   Z-ADD     *ZEROS        KORFIL
101500000000     C     KEYORG        SETLL     AZORG01L                           99        *FINE-ARCHIV
101600000000     C  N99              READ      AZORG01L                               99
101700000000     C                   Z-ADD     0             I                 3 0          *INDICE SCHIERA
101800000000     C     *IN99         DOWEQ     '0'                                          --- 1 -->
101900000000     C     ORGFVA        IFEQ      *BLANKS                                      --- 2 -->
102000000000     C     ORGFAG        IFNE      'V'
102100000706     C                   CLEAR                   OG143
102200000706     C                   MOVEL     ORGDE3        OG143
102300000000     C                   ADD       1             I
102400000000     C                   Z-ADD     ORGFIL        FIL(I)
102500000000     C                   Z-ADD     ORGCAR        ARE(I)
102600000000     C                   MOVEL     ORGFL3        DIV(I)
102700020702     C*
102800020702     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
102900020702     C                   CLEAR                   DNTW
103000020702     C*
103100020702     C                   MOVEL(P)  'NTW'         KTECOD
103200020702     C                   MOVEL(P)  §OGNTW        KTEKE1
103300020702     C     KEYTBE        CHAIN     TNTBE01L
103400020702     C                   IF        %found(TNTBE01L)
103500020702     C                   MOVEL     TBEUNI        DNTW
103600020702     C                   ENDIF
103700020702     C*
103800020702     C                   MOVEL     §NTWFIE       FL1(I)
104200020627     C*
104300100125     C*****              MOVEL     §OGNTW        POS(I)
104400000000     C                   END
104500000000     C                   END                                                    <-- 2 ---
104600000000     C                   READ      AZORG01L                               99
104700000000     C                   END                                                    <-- 1 ---
104800000000     C*
104900000000     C                   ENDSR
105000000000     C*------------------------------------------------------------------------*
105100000000     C* CANWFC - CANCELLA PER WFDLC00F TUTTI I RECORD DEI MESI RICHIESTI
105200000000     C*------------------------------------------------------------------------*
105300000000     C     CANWFC        BEGSR
105400970827     C*
105500970827IF  1C     PARSNR        IFEQ      'S'                                          *SE CHIESTA CREAZION
105600000000     C                   Z-ADD     PARANN        KWCANN
105700000000     C                   MOVEL     *BLANKS       KWCDIV
105800000000     C                   Z-ADD     *ZEROS        KWCARE
105900000000     C                   Z-ADD     *ZEROS        KWCFLP
106000000000     C                   Z-ADD     *ZEROS        KWCTFA
106100000000     C                   Z-ADD     *ZEROS        KWCFLA
106200000000     C                   Z-ADD     *ZEROS        KWCMES
106300000000     C                   MOVEL     *BLANKS       KWCTSP
106400000000     C                   Z-ADD     *ZEROS        KWCRAN
106500950626     C                   MOVEL     *BLANKS       KWCIOE
106600000000     C     KEYWFC        SETLL     WFDLC01L                           99        *FINE-ARCHIV
106700000000     C  N99              READ      WFDLC01L                               99
106800000000     C     *IN99         DOWEQ     '0'                                          --- 1 -->
106900000000     C     WFCANN        ANDEQ     PARANN
107000000000     C     WFCMES        IFGE      PARMMI
107100000000     C     WFCMES        ANDLE     PARMMF
107200000000     C                   DELETE    WFDLC
107300000000     C                   END
107400000000     C                   READ      WFDLC01L                               99
107500000000     C                   END                                                    <-- 1 ---
107600970827     C                   ENDIF
107700000000     C*
107800000000     C                   ENDSR
107900000000     C*---------------------------------------------------------------------*
108000000000     C* CANWFA - CANCELLA IL WFDLA00F TUTTI I RECORD DEI MESI RICHIESTI
108100000000     C*---------------------------------------------------------------------*
108200000000     C     CANWFA        BEGSR
108300000000     C*
108400970827IF  1C     PARSNR        IFEQ      'S'                                          *SE CHIESTA CREAZION
108500000000     C                   Z-ADD     PARANN        KWAANN
108600000000     C                   MOVEL     *BLANKS       KWADIV
108700000000     C                   Z-ADD     *ZEROS        KWAARE
108800000000     C                   Z-ADD     *ZEROS        KWAFLA
108900000000     C                   Z-ADD     *ZEROS        KWAZNC
109000950626     C                   MOVEL     *BLANKS       KWANZD
109100000000     C                   MOVEL     *BLANKS       KWACAD
109200000000     C                   Z-ADD     *ZEROS        KWAMES
109300000000     C                   MOVEL     *BLANKS       KWATSP
109400000000     C                   Z-ADD     *ZEROS        KWARAN
109500950626     C                   MOVEL     *BLANKS       KWAIOE
109600000000     C     KEYWFA        SETLL     WFDLA01L                           99        *FINE-ARCHIV
109700000000     C  N99              READ      WFDLA01L                               99
109800000000     C     *IN99         DOWEQ     '0'                                          --- 1 -->
109900000000     C     WFAANN        ANDEQ     PARANN
110000000000     C     WFAMES        IFGE      PARMMI
110100000000     C     WFAMES        ANDLE     PARMMF
110200000000     C                   DELETE    WFDLA
110300000000     C                   END
110400000000     C                   READ      WFDLA01L                               99
110500000000     C                   END                                                    <-- 1 ---
110600970827     C                   ENDIF
110700000000     C*
110800000000     C                   ENDSR
110900000000     C*---------------------------------------------------------------------*
111000000000     C* CANWFP - CANCELLA PER WFDLP00F TUTTI I RECORD DEI MESI RICHIESTI
111100000000     C*---------------------------------------------------------------------*
111200000000     C     CANWFP        BEGSR
111300000000     C*
111400970827IF  0C     PARSNR        IFEQ      'S'                                          *SE CHIESTA CREAZION
111500000000     C                   Z-ADD     PARANN        KWPANN
111600000000     C                   MOVEL     *BLANKS       KWPDIV
111700000000     C                   Z-ADD     *ZEROS        KWPARE
111800000000     C                   Z-ADD     *ZEROS        KWPFLP
111900100127     C                   Z-ADD     *ZEROS        KWPtfp
112000000000     C                   Z-ADD     *ZEROS        KWPMES
112100000000     C                   MOVEL     *BLANKS       KWPTSP
112200000000     C                   Z-ADD     *ZEROS        KWPRAN
112300950626     C                   MOVEL     *BLANKS       KWPIOE
112400100127     C     KEYWFP        SETLL     WFDLP02L                           99        *FINE ARCHIVIO
112500100127     C  N99              READ      WFDLP02L                               99
112600000000DO  1C     *IN99         DOWEQ     '0'
112700000000     C     WFPANN        ANDEQ     PARANN
112800000000IF  2C     WFPMES        IFGE      PARMMI
112900000000     C     WFPMES        ANDLE     PARMMF
113000000000     C*
113100000000     C                   DELETE    WFDLP
113200000000E   2C                   ENDIF
113300100127     C                   READ      WFDLP02L                               99
113400000000E   1C                   ENDDO
113500970827E   0C                   ENDIF
113600000000     C*
113700000000     C                   ENDSR
113800970827     C*------------------------------------------------------------------------*
113900970827     C* CANPCC - CANCELLA PER PCDLC00F TUTTI I RECORD DEI MESI RICHIESTI
114000970827     C*------------------------------------------------------------------------*
114100970827     C     CANPCC        BEGSR
114200970827     C*
114300970827IF  1C     PARCFP        IFEQ      'S'                                          *SE CHIESTA CREAZION
114400970827     C                   Z-ADD     PARANN        KPCANN
114500970827     C                   Z-ADD     PARMMI        KPCMES
114600970827     C                   MOVEL     *BLANKS       KPCDIV
114700970827     C                   Z-ADD     *ZEROS        KPCARE
114800970827     C                   Z-ADD     *ZEROS        KPCFLP
114900970827     C                   MOVEL     *BLANKS       KPCIOE
115000970827     C                   MOVEL     *BLANKS       KPCTSP
115100970827     C     KEYPCC        SETLL     PCDLC01L
115200970827     C                   READ      PCDLC01L                               99
115300970827DO  2C     *IN99         DOWEQ     *OFF
115400970827     C     PCCANN        ANDEQ     PARANN
115500970827IF  3C     PCCMES        IFGE      PARMMI
115600970827     C     PCCMES        ANDLE     PARMMF
115700970827     C                   DELETE    PCDLC000
115800970827E   3C                   ENDIF
115900970827     C                   READ      PCDLC01L                               99
116000970827E   2C                   ENDDO
116100970827E   1C                   ENDIF
116200970827     C*
116300970827     C                   ENDSR
116400970827     C*------------------------------------------------------------------------*
116500970827     C* CANPCA - CANCELLA PER PCDLA00F TUTTI I RECORD DEI MESI RICHIESTI
116600970827     C*------------------------------------------------------------------------*
116700970827     C     CANPCA        BEGSR
116800970827     C*
116900970827IF  1C     PARCFP        IFEQ      'S'                                          *SE CHIESTA CREAZION
117000970827     C                   Z-ADD     PARANN        KPAANN
117100970827     C                   Z-ADD     PARMMI        KPAMES
117200970827     C                   MOVEL     *BLANKS       KPADIV
117300970827     C                   Z-ADD     *ZEROS        KPAARE
117400970827     C                   Z-ADD     *ZEROS        KPAFLA
117500970827     C                   MOVEL     *BLANKS       KPAIOE
117600970827     C                   MOVEL     *BLANKS       KPATSP
117700970827     C     KEYPCA        SETLL     PCDLA01L
117800970827     C                   READ      PCDLA01L                               99
117900970827DO  2C     *IN99         DOWEQ     *OFF
118000970827     C     PCAANN        ANDEQ     PARANN
118100970827IF  3C     PCAMES        IFGE      PARMMI
118200970827     C     PCAMES        ANDLE     PARMMF
118300970827     C                   DELETE    PCDLA000
118400970827E   3C                   ENDIF
118500970827     C                   READ      PCDLA01L                               99
118600970827E   2C                   ENDDO
118700970827E   1C                   ENDIF
118800970827     C*
118900970827     C                   ENDSR
119000970827     C*------------------------------------------------------------------------*
119100970827     C* CANPCP - CANCELLA PER PCDLP00F TUTTI I RECORD DEI MESI RICHIESTI
119200970827     C*------------------------------------------------------------------------*
119300970827     C     CANPCP        BEGSR
119400970827     C*
119500970827IF  1C     PARCFP        IFEQ      'S'                                          *SE CHIESTA CREAZION
119600970827     C                   Z-ADD     PARANN        KPPANN
119700970827     C                   Z-ADD     PARMMI        KPPMES
119800970827     C                   MOVEL     *BLANKS       KPPDIV
119900970827     C                   Z-ADD     *ZEROS        KPPARE
120000970827     C                   Z-ADD     *ZEROS        KPPFLP
120100970827     C                   MOVEL     *BLANKS       KPPIOE
120200970827     C                   MOVEL     *BLANKS       KPPTSP
120300970827     C     KEYPCP        SETLL     PCDLP01L
120400970827     C                   READ      PCDLP01L                               99
120500970827DO  2C     *IN99         DOWEQ     *OFF
120600970827     C     PCPANN        ANDEQ     PARANN
120700970827IF  3C     PCPMES        IFGE      PARMMI
120800970827     C     PCPMES        ANDLE     PARMMF
120900970827     C                   DELETE    PCDLP000
121000970827E   3C                   ENDIF
121100970827     C                   READ      PCDLP01L                               99
121200970827E   2C                   ENDDO
121300970827E   1C                   ENDIF
121400970827     C*
121500970827     C                   ENDSR
121600000000     C*------------------------------------------------------------------------*
121700000000     C* CARTBL - CARICA I TIPI BOLLA CON RELATIVO RAGGRUPPAMENTO E FLAGS
121800000000     C*------------------------------------------------------------------------*
121900000000     C     CARTBL        BEGSR
122000000000     C*--------------------
122100000000     C* FLAG SPEDIZIONI /RICAVI PER RAGGRUPPAMENTO TIPO BOLLA
122200000000     C*--------------------
122300000000     C                   Z-ADD     0             I
122400000000     C                   Z-ADD     1             KTBUTE
122500000000     C                   MOVEL     '2W'          KTBCOD
122600000000     C                   MOVEL     *BLANKS       KTBKEY
122700000000     C     KEYTAB        SETLL     TABEL00F
122800000000     C                   READ      TABEL00F                               99
122900000000     C     *IN99         DOWEQ     *OFF
123000000000     C     TBLCOD        ANDEQ     '2W'
123100000000     C     TBLFLG        IFEQ      *BLANKS
123200000000     C                   ADD       1             I
123300000000     C                   MOVEL     TBLKEY        RBL(I)                         *RAGGRUPPAMENTO BOLL
123400000000     C                   MOVEL     TBLUNI        DS2W
123500000000     C                   MOVEL     §2WFSP        FSP(I)                         *FLAG SPEDIZIONE
123600000000     C                   MOVEL     §2WFRI        FRI(I)                         *FLAG RICAVO
123700000000     C                   END
123800000000     C                   READ      TABEL00F                               99
123900000000     C                   END
124000000000     C*--------------------
124100000000     C* TIPI BOLLA E RELATIVI FLAGS SPEDIZIONE/RICAVO
124200000000     C*--------------------
124300000000     C                   Z-ADD     0             I
124400000000     C                   Z-ADD     1             KTBUTE
124500000000     C                   MOVEL     'TB'          KTBCOD
124600000000     C                   MOVEL     *BLANKS       KTBKEY
124700000000     C     KEYTAB        SETLL     TABEL00F
124800000000     C                   READ      TABEL00F                               99
124900000000     C     *IN99         DOWEQ     *OFF
125000000000     C     TBLCOD        ANDEQ     'TB'
125100000000     C     TBLFLG        IFEQ      *BLANKS
125200000000     C                   ADD       1             I
125300000000     C                   MOVEL     TBLKEY        TB(I)                          *TIPO BOLLA
125400000000     C                   MOVEL     TBLUNI        DSTB
125500000000     C                   Z-ADD     1             J                 4 0
125600000000     C                   SETOFF                                       28
125700000000     C     §TBRBL        LOOKUP    RBL(J)                                 28
125800000000     C     *IN28         IFEQ      *ON
125900000000     C                   MOVEL     FSP(J)        SP(I)                          *FLAG SPEDIZIONE
126000000000     C                   MOVEL     FRI(J)        RI(I)                          *FLAG RICAVO
126100000000     C                   ELSE
126200000000     C                   MOVEL     'N'           SP(I)
126300000000     C                   MOVEL     'N'           RI(I)
126400000000     C                   ENDIF
126500000000     C                   END
126600000000     C                   READ      TABEL00F                               99
126700000000     C                   END
126800000000     C*
126900000000     C                   ENDSR
127000000000     C*---------------------------------------------------------------------*
127100000000     C* *INZSR - ROUTINE INIZIALE
127200000000     C*---------------------------------------------------------------------*
127300000000     C     *INZSR        BEGSR
127400000000     C*--------------------
127500000000     C* RICEVIMENTO PARAMETRI
127600000000     C*--------------------
127700000000     C     *ENTRY        PLIST
127800000000     C                   PARM                    KPJBA
127900950626     C                   MOVEL     KPJBU         PARAM
128000000000     C*--------------------
128100000000     C* CAMPI RIFERITI AL DATA BASE
128200000000     C*--------------------
128300990810     C     *LIKE         DEFINE    TASDCM        KBLDCM                         *TITAS00F+10F
128400000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
128500000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE                         *TABEL00F
128600000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
128700950628     C     *LIKE         DEFINE    TBLKEY        KTBKEY
128800970827     C     *LIKE         DEFINE    WFCANN        KWCANN                         *WFDLC00F
128900000000     C     *LIKE         DEFINE    WFCDIV        KWCDIV
129000000000     C     *LIKE         DEFINE    WFCARE        KWCARE
129100000000     C     *LIKE         DEFINE    WFCFLP        KWCFLP
129200000000     C     *LIKE         DEFINE    WFCTFA        KWCTFA
129300000000     C     *LIKE         DEFINE    WFCFLA        KWCFLA
129400000000     C     *LIKE         DEFINE    WFCMES        KWCMES
129500000000     C     *LIKE         DEFINE    WFCTSP        KWCTSP
129600000000     C     *LIKE         DEFINE    WFCRAN        KWCRAN
129700950626     C     *LIKE         DEFINE    WFCIOE        KWCIOE
129800970827     C     *LIKE         DEFINE    PCCANN        KPCANN                         *PCDLC00F
129900970827     C     *LIKE         DEFINE    PCCMES        KPCMES
130000970827     C     *LIKE         DEFINE    PCCDIV        KPCDIV
130100970827     C     *LIKE         DEFINE    PCCARE        KPCARE
130200970827     C     *LIKE         DEFINE    PCCFLP        KPCFLP
130300970827     C     *LIKE         DEFINE    PCCIOE        KPCIOE
130400970827     C     *LIKE         DEFINE    PCCTSP        KPCTSP
130500970827     C     *LIKE         DEFINE    WFAANN        KWAANN                         *WFDLA00F
130600000000     C     *LIKE         DEFINE    WFADIV        KWADIV
130700000000     C     *LIKE         DEFINE    WFAARE        KWAARE
130800000000     C     *LIKE         DEFINE    WFAFLA        KWAFLA
130900000000     C     *LIKE         DEFINE    WFAZNC        KWAZNC
131000950626     C     *LIKE         DEFINE    WFANZD        KWANZD
131100000000     C     *LIKE         DEFINE    WFACAD        KWACAD
131200000000     C     *LIKE         DEFINE    WFAMES        KWAMES
131300000000     C     *LIKE         DEFINE    WFATSP        KWATSP
131400000000     C     *LIKE         DEFINE    WFARAN        KWARAN
131500950626     C     *LIKE         DEFINE    WFAIOE        KWAIOE
131600970827     C     *LIKE         DEFINE    PCAANN        KPAANN                         *PCDLA00F
131700970827     C     *LIKE         DEFINE    PCAMES        KPAMES
131800970827     C     *LIKE         DEFINE    PCADIV        KPADIV
131900970827     C     *LIKE         DEFINE    PCAARE        KPAARE
132000970827     C     *LIKE         DEFINE    PCAFLA        KPAFLA
132100970827     C     *LIKE         DEFINE    PCAIOE        KPAIOE
132200970827     C     *LIKE         DEFINE    PCATSP        KPATSP
132300970827     C     *LIKE         DEFINE    WFPANN        KWPANN                         *WFDLP00F
132400000000     C     *LIKE         DEFINE    WFPDIV        KWPDIV
132500000000     C     *LIKE         DEFINE    WFPARE        KWPARE
132600000000     C     *LIKE         DEFINE    WFPFLP        KWPFLP
132700100125     C     *LIKE         DEFINE    WFPtfp        KWPtfp
132800000000     C     *LIKE         DEFINE    WFPMES        KWPMES
132900000000     C     *LIKE         DEFINE    WFPTSP        KWPTSP
133000000000     C     *LIKE         DEFINE    WFPRAN        KWPRAN
133100950626     C     *LIKE         DEFINE    WFPIOE        KWPIOE
133200970827     C     *LIKE         DEFINE    PCPANN        KPPANN                         *PCDLP00F
133300970827     C     *LIKE         DEFINE    PCPMES        KPPMES
133400970827     C     *LIKE         DEFINE    PCPDIV        KPPDIV
133500970827     C     *LIKE         DEFINE    PCPARE        KPPARE
133600970827     C     *LIKE         DEFINE    PCPFLP        KPPFLP
133700100125     C     *LIKE         DEFINE    PCPtfp        KPPtfp
133800970827     C     *LIKE         DEFINE    PCPIOE        KPPIOE
133900970827     C     *LIKE         DEFINE    PCPTSP        KPPTSP
134000950626     C     *LIKE         DEFINE    WFPIOE        WDA1                           *DI LAVORO
134100950626     C     *LIKE         DEFINE    WFPIOE        WDA2
134200950628     C     *LIKE         DEFINE    WFPIOE        WIOE
134300950628     C     *LIKE         DEFINE    TASLNP        WLNP
134400950628     C     *LIKE         DEFINE    TASTFP        WTFP
134500950628     C     *LIKE         DEFINE    TASLNA        WLNA
134600950628     C     *LIKE         DEFINE    TASTFA        WTFA
134700950628     C     *LIKE         DEFINE    TASZNC        WZNC
134800950628     C     *LIKE         DEFINE    TASNZD        WNZD
134900950628     C     *LIKE         DEFINE    TASCAD        WCAD
135000020702     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
135100020702     C     *LIKE         DEFINE    TBEKE1        KTEKE1
135200950628     C*--------------------
135300000000     C* CHIAVI DI LETTURA
135400000000     C*--------------------
135500041021     C* CHIAVE LETTURA TITAS26C - COMPLETA
135600000000     C     KEYTAS        KLIST
135700000000     C                   KFLD                    KBLDCM
135800000000     C*
135900000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
136000000000     C     KEYORG        KLIST
136100000000     C                   KFLD                    KORFIL
136200000000     C*
136300000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
136400000000     C     KEYTAB        KLIST
136500000000     C                   KFLD                    KTBUTE
136600000000     C                   KFLD                    KTBCOD
136700000000     C                   KFLD                    KTBKEY
136800000000     C*
136900000000     C* CHIAVE LETTURA WFDLC01L - COMPLETA
137000000000     C     KEYWFC        KLIST
137100000000     C                   KFLD                    KWCANN
137200000000     C                   KFLD                    KWCDIV
137300000000     C                   KFLD                    KWCARE
137400000000     C                   KFLD                    KWCFLP
137500000000     C                   KFLD                    KWCTFA
137600000000     C                   KFLD                    KWCFLA
137700000000     C                   KFLD                    KWCMES
137800000000     C                   KFLD                    KWCTSP
137900000000     C                   KFLD                    KWCRAN
138000950626     C                   KFLD                    KWCIOE
138100000000     C*
138200000000     C* CHIAVE LETTURA WFDLA01L - COMPLETA
138300000000     C     KEYWFA        KLIST
138400000000     C                   KFLD                    KWAANN
138500000000     C                   KFLD                    KWADIV
138600000000     C                   KFLD                    KWAARE
138700000000     C                   KFLD                    KWAFLA
138800000000     C                   KFLD                    KWAZNC
138900950626     C                   KFLD                    KWANZD
139000000000     C                   KFLD                    KWACAD
139100000000     C                   KFLD                    KWAMES
139200000000     C                   KFLD                    KWATSP
139300000000     C                   KFLD                    KWARAN
139400950626     C                   KFLD                    KWAIOE
139500000000     C*
139600000000     C* CHIAVE LETTURA WFDLP01L - COMPLETA
139700000000     C     KEYWFP        KLIST
139800000000     C                   KFLD                    KWPANN
139900000000     C                   KFLD                    KWPDIV
140000000000     C                   KFLD                    KWPARE
140100100125     C                   KFLD                    KWPtfp
140200100125     C                   KFLD                    KWPFLP
140300000000     C                   KFLD                    KWPMES
140400000000     C                   KFLD                    KWPTSP
140500000000     C                   KFLD                    KWPRAN
140600950626     C                   KFLD                    KWPIOE
140700970827     C*
140800970827     C* CHIAVE LETTURA PCDLC01L - COMPLETA
140900970827     C     KEYPCC        KLIST
141000970827     C                   KFLD                    KPCANN
141100970827     C                   KFLD                    KPCMES
141200970827     C                   KFLD                    KPCDIV
141300970827     C                   KFLD                    KPCARE
141400970827     C                   KFLD                    KPCFLP
141500970827     C                   KFLD                    KPCIOE
141600970827     C                   KFLD                    KPCTSP
141700970827     C*
141800970827     C* CHIAVE LETTURA PCDLA01L - COMPLETA
141900970827     C     KEYPCA        KLIST
142000970827     C                   KFLD                    KPAANN
142100970827     C                   KFLD                    KPAMES
142200970827     C                   KFLD                    KPADIV
142300970827     C                   KFLD                    KPAARE
142400970827     C                   KFLD                    KPAFLA
142500970827     C                   KFLD                    KPAIOE
142600970827     C                   KFLD                    KPATSP
142700970827     C*
142800970827     C* CHIAVE LETTURA PCDLP01L - COMPLETA
142900970827     C     KEYPCP        KLIST
143000970827     C                   KFLD                    KPPANN
143100970827     C                   KFLD                    KPPMES
143200970827     C                   KFLD                    KPPDIV
143300970827     C                   KFLD                    KPPARE
143400100125     C                   KFLD                    KPPtfp
143500100125     C                   KFLD                    KPPFLP
143600970827     C                   KFLD                    KPPIOE
143700970827     C                   KFLD                    KPPTSP
143800020702     C*
143900020702     C* LETTURA TNTBE01L - PARZIALE
144000020702     C     KEYTBE        KLIST
144100020702     C                   KFLD                    KTECOD                         *UTENTE
144200020702     C                   KFLD                    KTEKE1                         *CODICE
144300000000     C*--------------------
144400000000     C* CAMPI DI CONTROLLO
144500000000     C*--------------------
144600000000     C* IMPOSTAZIONI INIZIALI
144700000000     C                   MOVEL     '0'           FINL00            1            *FINE PGM
144800000000     C***
144900000000     C* LANCIO PER SALDI
145000000000     C***
145100000000     C     PARTLA        IFEQ      'S'
145200000000     C                   Z-ADD     PARANN        DSANN
145300000000     C                   Z-ADD     PARMMI        DSMES
145400000000     C                   Z-ADD     *ZEROS        DSGIO
145500000000     C                   Z-ADD     DSDAT         PARDCI            8 0          *DATA INIZIALE
145600000000     C                   Z-ADD     PARANN        DSANN
145700000000     C                   Z-ADD     PARMMF        DSMES
145800000000     C                   Z-ADD     99            DSGIO
145900000000     C                   Z-ADD     DSDAT         PARDCF            8 0          *DATA FINALE
146000000000     C                   ENDIF
146100000000     C***
146200000000     C* LANCIO ESTEMPORANEO
146300000000     C***
146400000000     C     PARTLA        IFEQ      'E'
146500000000     C                   Z-ADD     PARDTI        G02DAT
146600000000     C                   Z-ADD     *ZEROS        G02INV
146700000000     C                   MOVEL     '0'           G02ERR
146800000000     C                   CALL      'XSRDA8'
146900000000     C                   PARM                    G02PAR
147000000000     C                   Z-ADD     G02INV        PARDCI            8 0          *DATA INIZIALE
147100000000     C                   Z-ADD     PARDTF        G02DAT
147200000000     C                   Z-ADD     *ZEROS        G02INV
147300000000     C                   MOVEL     '0'           G02ERR
147400000000     C                   CALL      'XSRDA8'
147500000000     C                   PARM                    G02PAR
147600000000     C                   Z-ADD     G02INV        PARDCF            8 0          *DATA FINALE
147700000000     C                   ENDIF
147800000000     C*
147900000000     C* CARICA I TIPI BOLLA CON RELATIVI FLAG RICAVI/SPEDIZIONI
148000000000     C                   EXSR      CARTBL
148100000000     C*
148200000000     C* CARICA DA ORGANIGRAMMA TUTTE LE FILIALI CON RELATIVA DIVISIONE E AREA
148300000000     C                   EXSR      CARORG
148400000000     C*
148500000000     C* CANCELLA TUTTI I RECORD DEL PERIODO RICHIESTO DA WFDLC00F
148600000000     C                   EXSR      CANWFC
148700000000     C*
148800000000     C* CANCELLA TUTTI I RECORD DEL PERIODO RICHIESTO DA WFDLA00F
148900000000     C                   EXSR      CANWFA
149000000000     C*
149100000000     C* CANCELLA TUTTI I RECORD DEL PERIODO RICHIESTO DA WFDLP00F
149200000000     C                   EXSR      CANWFP
149300970827     C*
149400970827     C* CANCELLA TUTTI I RECORD DEL PERIODO RICHIESTO DA PCDLC00F
149500970827     C                   EXSR      CANPCC
149600970827     C*
149700970827     C* CANCELLA TUTTI I RECORD DEL PERIODO RICHIESTO DA PCDLA00F
149800970827     C                   EXSR      CANPCA
149900970827     C*
150000970827     C* CANCELLA TUTTI I RECORD DEL PERIODO RICHIESTO DA PCDLP00F
150100970827     C                   EXSR      CANPCP
150200000000     C*
150300000000     C                   ENDSR
