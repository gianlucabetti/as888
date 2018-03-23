000100000000     H*------------------------------------------------------------------------*
000200030626     H* RIGENERA FILE DI LAVORO PER STATISTICA AFFIDABILITA' RITIRO            *
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800041021     FTITAS26C  IF   E           K DISK
000900000000     FAZORG01L  IF   E           K DISK
001000000000     FTABEL00F  IF   E           K DISK
001100031218     FWFDLPR1L  UF A E           K DISK
001200020702     FTNTBE01L  IF   E           K DISK
001300000000     D*------------------------------------------------------------------------*
001400000000     D* SCHIERE
001500000000     D*------------------------------------------------------------------------*
001600000000     D*-------------------
001700000000     D* FILIALI CON RELATIVA DIVISIONE E AREA
001800000000     D*-------------------
001900971124     D FIL             S              3  0 DIM(500)                             *FILIALE
002000971124     D ARE             S              3  0 DIM(500)                             *AREA
002100971124     D DIV             S              1    DIM(500)                             *DIVISIONE
002200971204     D FL1             S              1    DIM(500)                             *FLAG ITALIA/ESTERO
002300020627     D POS             S              3    DIM(500)                             *NETWORK POSTE
002400000000     D*-------------------
002500000000     D* SCHIERA FLAG SPEDIZIONE/RICAVO E RAGGRUPPAMENTO TIPO BOLLA
002600000000     D*-------------------
002700000000     D RBL             S              1    DIM(5)                               RAGGRUPPAMENTO BOLLA
002800000000     D FSP             S              1    DIM(5)                               FLAG SPEDIZIONE
002900000000     D FRI             S              1    DIM(5)                               FLAG RICAVO
003000000000     D*-------------------
003100000000     D* SCHIERA TIPO BOLLA CON RELATIVI FLAG SPEDIZIONE/RICAVO
003200000000     D*-------------------
003300000000     D TB              S              2    DIM(20)                              TIP0 BOLLA
003400000000     D SP              S              1    DIM(20)                              FLAG SPEDIZIONE
003500000000     D RI              S              1    DIM(20)                              FLAG RICAVO
003600000000     D*------------------------------------------------------------------------*
003700000000     D* INPUT
003800000000     D*------------------------------------------------------------------------*
003900000000     D*-------------------
004000000000     D* ARCHITETTURA
004100000000     D*-------------------
004200000000     D KPJBA         E DS
004300000000     D*-------------------
004400000000     D* DS CODICI BOLLA
004500000000     D*-------------------
004600000000     D DS3A          E DS
004700000000     D*-------------------
004800000000     D* DS TIPO BOLLA
004900000000     D*-------------------
005000000000     D DSTB          E DS
005100000000     D*-------------------
005200000000     D* DS FLAG SPEDIZIONI/RICAVI RAGGRUPPAMENTO BOLLA
005300000000     D*-------------------
005400000000     D DS2W          E DS
005500000000     D*-------------------
005600000000     D* CONTROLLO DATE
005700000000     D*-------------------
005800000000     D G02PAR          DS
005900000000     D  G02DAT                 1      8  0
006000000000     D  G02INV                 9     16  0
006100000000     D  G02ERR                17     17
006200000000     D  G02TGI                18     22  0
006300000000     D*-------------------
006400941209     D* DS SCOMPOSIZIONE DATA GENERICA
006500000000     D*-------------------
006600000000     D                 DS
006700000000     D  DSANN                  1      4  0
006800000000     D  DSMES                  5      6  0
006900000000     D  DSGIO                  7      8  0
007000000000     D  DSMGS                  5      8  0
007100000000     D  DSDAT                  1      8  0
007200941209     D*-------------------
007300941209     D* DS SCOMPOSIZIONE DATA CONSEGNA MERCE
007400941209     D*-------------------
007500941209     D  WAACM                  1      4  0
007600941209     D  WMMCM                  5      6  0
007700941209     D  TASDCM                 1      8  0
007800000000     D*-------------------
007900000000     D* PARAMETRI PASSAGGIO
008000000000     D*-------------------
008100950626     D PARAM           DS
008200000000     D  PARANN                 1      4  0
008300000000     D  PARMMI                 5      6  0
008400000000     D  PARMMF                 7      8  0
008500000000     D  PARSNC                 9      9
008600000000     D  PARSDC                10     10
008700000000     D  PARSNA                11     11
008800000000     D  PARSDA                12     12
008900000000     D  PARSNR                13     13
009000000000     D  PARDIV                14     14
009100000000     D  PARARE                15     17  0
009200000000     D  PARFLP                18     20  0
009300000000     D  PARTFP                21     23  0
009400000000     D  PARFLA                24     26  0
009500000000     D  PARTFA                27     29  0
009600000000     D  PARDTI                30     37  0
009700000000     D  PARDTF                38     45  0
009800000000     D  PARTLA                46     46
009900000000     D  PARSNP                47     47
010000950712     D  PARTOT                48     48
010100950712     D  PARIOE                49     49
010200970826     D  PARCFP                50     50
010300000216     D*---
010400000216     D* DS X LETTURA DESCRIZIONE 143 ORGANIGRAMMA (X DPD)
010500020702     D*------------------
010600000216     D OG143         E DS
010700020702     D*------------------
010800020702     D* DS PER LETTURA TABELLA "NTW"
010900020702     D*------------------
011000020702     D DNTW          E DS
011100030626     D*------------------
011200030626     D* DS X PASSAGGIO PARAMETRI TRUL XSRLAV8
011300030626     D*------------------
011400030626     D WDAT8           DS
011500030626     D  DATADA                        8  0
011600030626     D  DATAA                         8  0
011700030626     D  GGLAV                         5  0
011800000000     C*------------------------------------------------------------------------*
011900000000     C* MAIN LINE
012000000000     C*------------------------------------------------------------------------*
012100000000     C*
012200000000     C* SE NON DEVE ESSERE ESEGUITO IL PGM VADO A FINE
012300000000IF  1C     FINL00        IFEQ      '0'
012400000000     C*
012500000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
012600000000     C                   EXSR      SETFIL
012700000000     C*
012800000000     C* CICLO FINO A FINE FILE
012900000000DO  2C     FINL00        DOWEQ     '0'
013000000000     C*
013100000000     C* CREA IL RECORD SUI FILE DI LAVORO
013200000000     C                   EXSR      CREARE
013300000000     C*
013400000000     C* LEGGE IL RECORD SUCCESSIVO
013500000000     C                   EXSR      LETFIL
013600000000     C*
013700000000E   2C                   ENDDO
013800000000E   1C                   ENDIF
013900000000     C*
014000000000     C                   SETON                                        LR
014100000000     C*------------------------------------------------------------------------*
014200000000     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL FILE
014300000000     C*------------------------------------------------------------------------*
014400000000     C     SETFIL        BEGSR
014500000000     C*
014600000000     C* POSIZIONAMENTO E PRIMA LETTURA
014700000000     C                   Z-ADD     PARDCI        KBLDCM
014800041021     C     KEYTAS        SETLL     TITAS26C                           99        *FINE-ARCHIV
014900000000     C*
015000000000     C     *IN99         IFEQ      '1'
015100000000     C                   MOVEL     '1'           FINL00
015200000000     C                   ELSE
015300000000     C                   EXSR      LETFIL
015400000000     C                   END
015500000000     C*
015600000000     C                   ENDSR
015700000000     C*------------------------------------------------------------------------*
015800000000     C* LETFIL - LETTURA PROSSIMO RECORD
015900000000     C*---------------------------------------------------------------------*
016000000000     C     LETFIL        BEGSR
016100000000     C*
016200000000     C* LEGGO FINO A:
016300000000     C*     - FINE FILE O SUPERATO LIMITE FINALE
016400000000     C*     - TROVATO RECORD VALIDO
016500000000     C                   MOVEL     'N'           WRECOK
016600000000     C     FINL00        DOWEQ     '0'                                          --- 1 -->
016700000000     C     WRECOK        ANDEQ     'N'
016800000000     C*
016900000000     C* LETTURE SUCCESSIVE ALLA PRIMA
017000041021     C                   READ      TITAS26C                               99
017100000000     C*
017200000000     C     *IN99         IFEQ      '1'                                          *FINE FILE
017300000000     C     TASDCM        ORGT      PARDCF                                       *SUPERATA DATA FINE
017400000000     C                   MOVEL     '1'           FINL00
017500000000     C                   ELSE
017600000000     C                   EXSR      CHKREC                                       *CONTR.VALIDITA REC.
017700000000     C                   END
017800000000     C                   END                                                    <-- 1 ---
017900000000     C*
018000000000     C                   ENDSR
018100000000     C*---------------------------------------------------------------------*
018200000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
018300000000     C*---------------------------------------------------------------------*
018400000000     C     CHKREC        BEGSR
018500000000     C*
018600000000     C                   MOVEL     'S'           WRECOK            1
018700990608     C*
018800990608     C* SOLO BOLLE CONSEGNATE
018900990608     C     TASDCM        IFEQ      *ZEROS
019000990608     C                   MOVEL     'N'           WRECOK
019100990608     C                   ENDIF
019200950530     C*
019300950530     C* NON CONSIDERA LE BOLLE CON UNA CONSEGNA ANOMALA
019400950530     C     TASCCA        IFNE      *BLANKS
019500950530     C                   MOVEL     'N'           WRECOK
019600950530     C                   ENDIF
019700950530     C*
019800950530     C* PRENDE SOLO LE BOLLE VALIDE SIA COME "SPEDIZIONE" CHE COME "RICAVO"
019900000706     C     WRECOK        IFEQ      'S'
020000950530     C                   Z-ADD     1             J
020100950530     C                   SETOFF                                           28
020200950530     C     TASTBL        LOOKUP    TB(J)                                  28
020300950530     C     SP(J)         IFEQ      'N'
020400950530     C     RI(J)         OREQ      'N'
020500950530     C                   MOVEL     'N'           WRECOK
020600950530     C                   ENDIF
020700000706     C                   ENDIF
020800000706     C*
020900000706     C* ESCLUDE LE BOLLE CON LINEA PARTENZA POSTE
021000000706IF  1C     WRECOK        IFEQ      'S'
021100000706     C                   Z-ADD     1             J
021200000706     C                   SETOFF                                           28
021300000706     C     TASLNP        LOOKUP    FIL(J)                                 28
021400000706IF  2C     *IN28         IFEQ      *OFF
021500020627     C     POS(J)        OREQ      'PPT'
021600000706     C                   MOVEL     'N'           WRECOK
021700000706E   2C                   ENDIF
021800000706E   1C                   ENDIF
021900000706     C*
022000000706     C* ESCLUDE LE BOLLE CON LINEA ARRIVO POSTE
022100000706IF  1C     WRECOK        IFEQ      'S'
022200000706     C                   Z-ADD     1             J
022300000706     C                   SETOFF                                           28
022400000706     C     TASLNA        LOOKUP    FIL(J)                                 28
022500000706IF  2C     *IN28         IFEQ      *OFF
022600020627     C     POS(J)        OREQ      'PPT'
022700000706     C                   MOVEL     'N'           WRECOK
022800000706E   2C                   ENDIF
022900000706E   1C                   ENDIF
023000950530     C*
023100000000     C* RECUPERA DIVISIONE ED AREA PARTENZA
023200000706IF  0C     WRECOK        IFEQ      'S'
023300000000     C                   Z-ADD     TASLNP        WFIL              3 0
023400971204     C                   EXSR      DATFIL
023500000000     C                   MOVEL     WDIV          WDIVP             1
023600000000     C                   Z-ADD     WARE          WAREP             3 0
023700971204     C                   MOVEL     WFL1          DLPFL1            1
023800000000     C*
023900000000     C* RECUPERA DIVISIONE ED AREA ARRIVO
024000000000     C                   Z-ADD     TASLNA        WFIL
024100971204     C                   EXSR      DATFIL
024200000000     C                   MOVEL     WDIV          WDIVA             1
024300000000     C                   Z-ADD     WARE          WAREA             3 0
024400971204     C                   MOVEL     WFL1          DLAFL1            1
024500000000     C*
024600000000     C* SE LANCIO ESTEMPORANEO NON CARICO I RECORD ESCLUSI DALLE PARZIALIZ.NI
024700000000IF  1C     PARTLA        IFEQ      'E'
024800000000     C*
024900000000     C* SE DIVISIONE DEL RECORD (PART O ARR) <> DIVISIONE RICHIESTA, ESCLUDO
025000000000IF  2C     PARDIV        IFNE      *BLANKS
025100000000IF  3C     WDIVP         IFEQ      PARDIV
025200000000     C     WDIVA         OREQ      PARDIV
025300000000X   3C                   ELSE
025400000000     C                   MOVEL     'N'           WRECOK
025500000000E   3C                   ENDIF
025600000000E   2C                   ENDIF
025700000000     C*
025800000000     C* SE AREA DEL RECORD (PART O ARR) <> AREA RICHIESTA, ESCLUDO
025900000000IF  2C     PARARE        IFNE      *ZEROS
026000000000IF  3C     WAREP         IFEQ      PARARE
026100000000     C     WAREA         OREQ      PARARE
026200000000X   3C                   ELSE
026300000000     C                   MOVEL     'N'           WRECOK
026400000000E   3C                   ENDIF
026500000000E   2C                   ENDIF
026600000000     C*
026700000000     C* SE FILIALE PARTENZA DEL RECORD <> FILIALE PARTENZA RICHIESTA, ESCLUDO
026800000000IF  2C     PARFLP        IFNE      *ZEROS
026900000000IF  3C     TASLNP        IFNE      PARFLP
027000000000     C                   MOVEL     'N'           WRECOK
027100000000E   3C                   ENDIF
027200000000E   2C                   ENDIF
027300000000     C*
027400000000     C* SE TERMINAL PARTENZA DEL RECORD <> TERMINAL PARTENZA RICHIESTA, ESCLUDO
027500000000IF  2C     PARTFP        IFNE      *ZEROS
027600000000IF  3C     TASTFP        IFNE      PARTFP
027700000000     C                   MOVEL     'N'           WRECOK
027800000000E   3C                   ENDIF
027900000000E   2C                   ENDIF
028000000000     C*
028100000000     C* SE FILIALE ARRIVO DEL RECORD <> FILIALE ARRIVO RICHIESTA, ESCLUDO
028200000000IF  2C     PARFLA        IFNE      *ZEROS
028300000000IF  3C     TASLNA        IFNE      PARFLA
028400000000     C                   MOVEL     'N'           WRECOK
028500000000E   3C                   ENDIF
028600000000E   2C                   ENDIF
028700000000     C*
028800000000     C* SE TERMINAL ARRIVO DEL RECORD <> TERMINAL ARRIVO RICHIESTA, ESCLUDO
028900000000IF  2C     PARTFA        IFNE      *ZEROS
029000000000IF  3C     TASTFA        IFNE      PARTFA
029100000000     C                   MOVEL     'N'           WRECOK
029200000000E   3C                   ENDIF
029300000000E   2C                   ENDIF
029400000000     C*
029500000000E   1C                   ENDIF
029600950530E   0C                   ENDIF
029700000000     C*
029800000000     C                   ENDSR
029900000000     C*------------------------------------------------------------------------*
030000000000     C* CREARE - CREA I RECORD SUI FILE DI LAVORO
030100000000     C*------------------------------------------------------------------------*
030200000000     C     CREARE        BEGSR
030300970827     C*---
030400970827     C* NB: SE IL LANCIO E' PER SALDI RIGENERA I W.F. X TUTTE LE STATISTICHE
030500000000     C*       SE IL LANCIO E' ESTEMPORANEO SOLO PER LE STATISTICHE RICHIESTE
030600970827     C* NB: SE IL LANCIO E' PER SALDI RIGENERA I FILE PC X TUTTE LE STATISTICHE
030700970827     C*       SE IL LANCIO E' ESTEMPORANEO SOLO PER LE STATISTICHE RICHIESTE
030800970827     C*---
030900000000     C     PARTLA        IFEQ      'E'                                          *LANCIO ESTEMPORANEO
031000000000     C                   Z-ADD     *ZEROS        WAACM             4 0
031100000000     C                   Z-ADD     *ZEROS        WMMCM             2 0
031200000000     C                   END
031300000000     C*-------------------
031400950626     C* P A R T E N Z A
031500000000     C*-------------------
031600031218     C                   Z-ADD     *ZEROS        WNUORE            3 0          *ORE RESA ESTERNE->
031700031218     C                   Z-ADD     *ZEROS        WRANGE            2 0
031800031218     C*
031900000000IF  1C     PARTLA        IFEQ      'S'                                          *PER SALDI
032000000000     C     PARTLA        OREQ      'E'                                          *ESTEMPORANEO
032100000000     C     PARSNP        ANDEQ     'S'                                          *RICHIESTA PARTENZE
032200030626     C*
032300030626     C* AGGIUNGO ALLE ORE NON CONFORMITA PARTENZA (TASNPI) LE ORE TRA DATA BORDERO E DATA RITIRO
032400030626     C                   EXSR      REPORERIT
032500000000     C*
032600950321     C* CERCA IL RANGE A CUI SOMMARE LA SPEDIZIONE (1,2,3,4,5...)
032700030711     C                   IF        TASNPI <= 240
032800030626     C                   EVAL      WNUORE = TASNPI + WORERIT                    *ORE NON CONFORMITA'
032900030711     C                   ELSE
033000030711     C                   EVAL      WNUORE = TASNPI                              *ORE NON CONFORMITA'
033100030711     C                   ENDIF
033200950321     C                   Z-ADD     *ZEROS        WRANGE
033300950321     C                   EXSR      CRANGE
033400950626     C*
033500950626     C* RECUPERA IL FLAG DEL TIPO DI DATI IN STATISTICA (I,X,M)
033600031218     C                   MOVEL     'P'           WSTA              1            *TIPO STATISTICA
033700950626     C                   MOVEL     *BLANKS       WDA1                           *TIPO DATO 1
033800950626     C                   MOVEL     *BLANKS       WDA2                           *TIPO DATO 2
033900950626     C                   EXSR      CREIOE
034000950626     C*
034100950626     C* SCRIVE O RISCRIVE IL FILE DI LAVORO PER L'AFFIDABILITA' PARTENZA
034200970827IF  2C     PARSNR        IFEQ      'S'                                          *SE RICHIESTO
034300970827IF  3C     WDA1          IFNE      *BLANKS                                      *SEMPRE
034400950626     C                   MOVEL     WDA1          WIOE                           *TIPO DATI IN STAT.
034500950626     C                   EXSR      SWFDLP
034600970827E   3C                   ENDIF
034700970827IF  3C     WDA2          IFNE      *BLANKS
034800950626     C                   MOVEL     WDA2          WIOE                           *TIPO DATI IN STAT.
034900950626     C                   EXSR      SWFDLP
035000970827E   3C                   ENDIF
035100970827E   2C                   ENDIF
035200031218E   1C                   ENDIF
035300000000     C*
035400000000     C                   ENDSR
035500950626     C*------------------------------------------------------------------------*
035600950626     C* SWFDLP - SCRIVE O RISCRIVE FILE DI LAVORO PER L'AFFIDABILITA' PARTENZA *
035700950626     C*------------------------------------------------------------------------*
035800950626     C     SWFDLP        BEGSR
035900950628     C*
036000950628     C* SE DATI IN STATISTICA = 'X'-EXPORT INVERTE L'ORDINAMENTO
036100950628IF  1C     WIOE          IFEQ      'X'
036200950628     C                   MOVEL     TASLNA        WLNP                           *INVERTITO
036300950628     C                   MOVEL     TASTFA        WTFP
036400970827     C*
036500950703     C* "NUOVA" AREA E DIVISIONE
036600950703     C                   Z-ADD     WLNP          WFIL              3 0
036700971204     C                   EXSR      DATFIL
036800950703     C                   MOVEL     WDIV          WDIVP             1
036900950703     C                   Z-ADD     WARE          WAREP             3 0
037000950628X   1C                   ELSE
037100950628     C                   MOVEL     TASLNP        WLNP                           *NORMALE
037200950628     C                   MOVEL     TASTFP        WTFP
037300970828     C*
037400970828     C* "VECCHIA" AREA E DIVISIONE PARTENZA
037500970828     C                   Z-ADD     TASLNP        WFIL              3 0
037600971204     C                   EXSR      DATFIL
037700970828     C                   MOVEL     WDIV          WDIVP             1
037800970828     C                   Z-ADD     WARE          WAREP             3 0
037900950628E   1C                   ENDIF
038000950626     C*
038100950626     C* CONTROLLA ESISTENZA RECORD
038200950626     C                   Z-ADD     WAACM         KWPANN
038300950626     C                   MOVEL     WDIVP         KWPDIV
038400950626     C                   Z-ADD     WAREP         KWPARE
038500950628     C                   Z-ADD     WLNP          KWPFLP
038600950626     C                   Z-ADD     WMMCM         KWPMES
038700950626     C                   MOVEL     TASTSP        KWPTSP
038800950626     C                   Z-ADD     WRANGE        KWPRAN
038900950626     C                   MOVEL     WIOE          KWPIOE
039000031218     C     KEYWFP        CHAIN     WFDLPR1L                           99
039100950626IF  1C     *IN99         IFEQ      *ON                                          N O N   E S I S T E
039200950626     C                   Z-ADD     KWPANN        WFPANN
039300950626     C                   Z-ADD     KWPMES        WFPMES
039400950626     C                   MOVEL     KWPDIV        WFPDIV
039500950626     C                   Z-ADD     KWPARE        WFPARE
039600950628     C                   Z-ADD     WLNP          WFPFLP
039700950628     C                   Z-ADD     WTFP          WFPTFP
039800950626     C                   MOVEL     KWPTSP        WFPTSP
039900950626     C                   Z-ADD     KWPRAN        WFPRAN
040000950626     C                   MOVEL     KWPIOE        WFPIOE
040100950626     C                   EXSR      AZZWFP                                       *AZZERA CAMPI RANGE
040200950626     C                   EXSR      ADDWFP                                       *AGGIUNGE SPED.AL RE
040300950626     C                   WRITE     WFDLP
040400950626X   1C                   ELSE                                                   E S I S T E
040500950626     C                   EXSR      ADDWFP                                       *AGGIUNGE SPED.AL RE
040600950626     C                   UPDATE    WFDLP
040700950626E   1C                   ENDIF
040800950626     C*
040900950626     C                   ENDSR
041000000000     C*------------------------------------------------------------------------*
041100000000     C* CRANGE - CONTROLLA IN QUALE RANGE RIENTRA IL RITARDO
041200000000     C* INPUT:  -WNUORE  (NUMERO DI ORE)
041300000000     C* OUTPUT: -WRANGE  (RANGE (0,1,2,3,4,5))
041400000000     C*------------------------------------------------------------------------*
041500000000     C     CRANGE        BEGSR
041600000000     C*
041700000000     C     WNUORE        IFLE      *ZEROS
041800000000     C                   Z-ADD     1             WRANGE            2 0
041900000000     C                   END
042000000000     C     WNUORE        IFGT      *ZEROS
042100000000     C     WNUORE        ANDLE     12
042200000000     C                   Z-ADD     2             WRANGE
042300000000     C                   END
042400000000     C     WNUORE        IFGT      12
042500000000     C     WNUORE        ANDLE     24
042600000000     C                   Z-ADD     3             WRANGE
042700000000     C                   END
042800000000     C     WNUORE        IFGT      24
042900000000     C     WNUORE        ANDLE     48
043000000000     C                   Z-ADD     4             WRANGE
043100000000     C                   END
043200000000     C     WNUORE        IFGT      48
043300000000     C     WNUORE        ANDLE     72
043400000000     C                   Z-ADD     5             WRANGE
043500000000     C                   END
043600000000     C     WNUORE        IFGT      72
043700000000     C                   Z-ADD     6             WRANGE
043800000000     C                   END
043900000000     C*
044000000000     C                   ENDSR
044100950626     C*------------------------------------------------------------------------*
044200950626     C* CREIOE - RECUPERA IL TIPO DI DATO IN STATISTICA (I,X,M)
044300950626     C*        INPUT : WSTA - TIPO STATISTICA
044400950626     C*        OUTPUT: WDA1 - PRIMO   VALORE
044500950626     C*                WDA2 - SECONDO VALORE
044600950626     C*------------------------------------------------------------------------*
044700950626     C     CREIOE        BEGSR
044800950626     C*--------------------
044900950626     C* P A R T E N Z A
045000950626     C*--------------------
045100031218SL  1C                   SELECT
045200950626     C     WSTA          WHENEQ    'P'
045300971204IF  2C     DLPFL1        IFEQ      *BLANKS                                      *LNP=ITALIA
045400950626     C                   MOVEL     'I'           WDA1                           ITALIA
045500971204IF  3C     DLAFL1        IFNE      *BLANKS                                      *LNA=ESTERO
045600950626     C                   MOVEL     'X'           WDA2                           EXPORT
045700950626E   3C                   ENDIF
045800971204X   2C                   ELSE                                                   *LNP=ESTERO
045900971204IF  3C     DLAFL1        IFEQ      *BLANKS                                      *LNA=ITALIA
046000950626     C                   MOVEL     'M'           WDA1                           IMPORT
046100950626E   3C                   ENDIF
046200950626E   2C                   ENDIF
046300950626     C*
046400950626E   1C                   ENDSL
046500950626     C*
046600950626     C                   ENDSR
046700000000     C*------------------------------------------------------------------------*
046800031218     C* ADDWFP - AGGIUNGE LA SPEDIZIONE AL RECORD WFDLPR0F
046900000000     C*------------------------------------------------------------------------*
047000000000     C     ADDWFP        BEGSR
047100000000     C*
047200000000     C                   ADD       1             WFPNSP
047300000000     C*
047400000000     C                   ENDSR
047500000000     C*------------------------------------------------------------------------*
047600031218     C* AZZWFP - AZZERA CAMPI RANGE IN WFDLPR0F
047700000000     C*------------------------------------------------------------------------*
047800000000     C     AZZWFP        BEGSR
047900000000     C*
048000000000     C                   Z-ADD     *ZEROS        WFPNSP
048100000000     C*
048200000000     C                   ENDSR
048300000000     C*------------------------------------------------------------------------*
048400971204     C* DATFIL - RECUPERA DATI FILIALE
048500000000     C*------------------------------------------------------------------------*
048600971204     C     DATFIL        BEGSR
048700000000     C*
048800000000     C                   Z-ADD     1             I
048900000000     C     WFIL          LOOKUP    FIL(I)                                 99
049000000000     C     *IN99         IFEQ      '0'                                          --- 3 -->
049100000000     C                   MOVEL     '*'           WDIV              1
049200000000     C                   Z-ADD     999           WARE              3 0
049300971204     C                   MOVEL     *BLANKS       WFL1              1
049400000000     C                   ELSE                                                   <-- 3 -->
049500000000     C                   MOVEL     DIV(I)        WDIV
049600000000     C                   Z-ADD     ARE(I)        WARE
049700971204     C                   MOVEL     FL1(I)        WFL1
049800000000     C                   END                                                    <-- 3 ---
049900000000     C*
050000000000     C                   ENDSR
050100000000     C*------------------------------------------------------------------------*
050200000000     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
050300000000     C*---------------------------------------------------------------------*
050400000000     C     CARORG        BEGSR
050500000000     C*
050600000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
050700000000     C                   Z-ADD     *ZEROS        KORFIL
050800000000     C     KEYORG        SETLL     AZORG01L                           99        *FINE-ARCHIV
050900000000     C  N99              READ      AZORG01L                               99
051000000000     C                   Z-ADD     0             I                 3 0          *INDICE SCHIERA
051100000000     C     *IN99         DOWEQ     '0'                                          --- 1 -->
051200000000     C     ORGFVA        IFEQ      *BLANKS                                      --- 2 -->
051300000000     C     ORGFAG        IFNE      'V'
051400000706     C                   CLEAR                   OG143
051500000706     C                   MOVEL     ORGDE3        OG143
051600000000     C                   ADD       1             I
051700000000     C                   Z-ADD     ORGFIL        FIL(I)
051800000000     C                   Z-ADD     ORGCAR        ARE(I)
051900000000     C                   MOVEL     ORGFL3        DIV(I)
052000020702     C*
052100020702     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
052200020702     C                   CLEAR                   DNTW
052300020702     C*
052400020702     C                   MOVEL(P)  'NTW'         KTECOD
052500020702     C                   MOVEL(P)  §OGNTW        KTEKE1
052600020702     C     KEYTBE        CHAIN     TNTBE01L
052700020702     C                   IF        %found(TNTBE01L)
052800020702     C                   MOVEL     TBEUNI        DNTW
052900020702     C                   ENDIF
053000020702     C*
053100020702     C                   MOVEL     §NTWFIE       FL1(I)
053200020702     C                   IF        §OGNTW = 'DPD'
053300020702     C                   MOVEL     'E'           FL1(I)
053400020702     C                   ENDIF
053500020627     C*
053600020627     C                   MOVEL     §OGNTW        POS(I)
053700000000     C                   END
053800000000     C                   END                                                    <-- 2 ---
053900000000     C                   READ      AZORG01L                               99
054000000000     C                   END                                                    <-- 1 ---
054100000000     C*
054200000000     C                   ENDSR
054300000000     C*---------------------------------------------------------------------*
054400031218     C* CANWFP - CANCELLA PER WFDLPR0F TUTTI I RECORD DEI MESI RICHIESTI
054500000000     C*---------------------------------------------------------------------*
054600000000     C     CANWFP        BEGSR
054700000000     C*
054800970827IF  0C     PARSNR        IFEQ      'S'                                          *SE CHIESTA CREAZION
054900000000     C                   Z-ADD     PARANN        KWPANN
055000000000     C                   MOVEL     *BLANKS       KWPDIV
055100000000     C                   Z-ADD     *ZEROS        KWPARE
055200000000     C                   Z-ADD     *ZEROS        KWPFLP
055300000000     C                   Z-ADD     *ZEROS        KWPMES
055400000000     C                   MOVEL     *BLANKS       KWPTSP
055500000000     C                   Z-ADD     *ZEROS        KWPRAN
055600950626     C                   MOVEL     *BLANKS       KWPIOE
055700031218     C     KEYWFP        SETLL     WFDLPR1L                           99        *FINE ARCHIVIO
055800031218     C  N99              READ      WFDLPR1L                               99
055900000000DO  1C     *IN99         DOWEQ     '0'
056000000000     C     WFPANN        ANDEQ     PARANN
056100000000IF  2C     WFPMES        IFGE      PARMMI
056200000000     C     WFPMES        ANDLE     PARMMF
056300000000     C*
056400000000     C                   DELETE    WFDLP
056500000000E   2C                   ENDIF
056600031218     C                   READ      WFDLPR1L                               99
056700000000E   1C                   ENDDO
056800970827E   0C                   ENDIF
056900000000     C*
057000000000     C                   ENDSR
057100000000     C*------------------------------------------------------------------------*
057200000000     C* CARTBL - CARICA I TIPI BOLLA CON RELATIVO RAGGRUPPAMENTO E FLAGS
057300000000     C*------------------------------------------------------------------------*
057400000000     C     CARTBL        BEGSR
057500000000     C*--------------------
057600000000     C* FLAG SPEDIZIONI /RICAVI PER RAGGRUPPAMENTO TIPO BOLLA
057700000000     C*--------------------
057800000000     C                   Z-ADD     0             I
057900000000     C                   Z-ADD     1             KTBUTE
058000000000     C                   MOVEL     '2W'          KTBCOD
058100000000     C                   MOVEL     *BLANKS       KTBKEY
058200000000     C     KEYTAB        SETLL     TABEL00F
058300000000     C                   READ      TABEL00F                               99
058400000000     C     *IN99         DOWEQ     *OFF
058500000000     C     TBLCOD        ANDEQ     '2W'
058600000000     C     TBLFLG        IFEQ      *BLANKS
058700000000     C                   ADD       1             I
058800000000     C                   MOVEL     TBLKEY        RBL(I)                         *RAGGRUPPAMENTO BOLL
058900000000     C                   MOVEL     TBLUNI        DS2W
059000000000     C                   MOVEL     §2WFSP        FSP(I)                         *FLAG SPEDIZIONE
059100000000     C                   MOVEL     §2WFRI        FRI(I)                         *FLAG RICAVO
059200000000     C                   END
059300000000     C                   READ      TABEL00F                               99
059400000000     C                   END
059500000000     C*--------------------
059600000000     C* TIPI BOLLA E RELATIVI FLAGS SPEDIZIONE/RICAVO
059700000000     C*--------------------
059800000000     C                   Z-ADD     0             I
059900000000     C                   Z-ADD     1             KTBUTE
060000000000     C                   MOVEL     'TB'          KTBCOD
060100000000     C                   MOVEL     *BLANKS       KTBKEY
060200000000     C     KEYTAB        SETLL     TABEL00F
060300000000     C                   READ      TABEL00F                               99
060400000000     C     *IN99         DOWEQ     *OFF
060500000000     C     TBLCOD        ANDEQ     'TB'
060600000000     C     TBLFLG        IFEQ      *BLANKS
060700000000     C                   ADD       1             I
060800000000     C                   MOVEL     TBLKEY        TB(I)                          *TIPO BOLLA
060900000000     C                   MOVEL     TBLUNI        DSTB
061000000000     C                   Z-ADD     1             J                 4 0
061100000000     C                   SETOFF                                       28
061200000000     C     §TBRBL        LOOKUP    RBL(J)                                 28
061300000000     C     *IN28         IFEQ      *ON
061400000000     C                   MOVEL     FSP(J)        SP(I)                          *FLAG SPEDIZIONE
061500000000     C                   MOVEL     FRI(J)        RI(I)                          *FLAG RICAVO
061600000000     C                   ELSE
061700000000     C                   MOVEL     'N'           SP(I)
061800000000     C                   MOVEL     'N'           RI(I)
061900000000     C                   ENDIF
062000000000     C                   END
062100000000     C                   READ      TABEL00F                               99
062200000000     C                   END
062300000000     C*
062400000000     C                   ENDSR
062500030626     C*------------------------------------------------------------------------*
062600030626     C* REPORERIT - CALCOLA IL NUMERO DI ORE (SU BASE 24) TRA DATA DORDERO E DATA RITIRO
062700030626     C* INPUT:  -TASDBR TASDRT
062800030626     C* OUTPUT: -WORERIT
062900030626     C*------------------------------------------------------------------------*
063000030626     C     REPORERIT     BEGSR
063100030626     C*
063200030626     C                   Z-ADD     *ZEROS        WORERIT           5 0
063300030626     C*
063400030626     C* Solo se le date borderò e ritiro sono diverse
063500030626     C                   IF        TASDRT <> TASDBR
063600030626     C                   CLEAR                   WDAT8
063700030626     C                   Z-ADD     TASDRT        DATADA
063800030626     C                   Z-ADD     TASDBR        DATAA
063900030626     C*
064000030626     C* CHIAMO IL PROGRAMMA DI REPERIMENTO
064100030626     C                   CALL      'XSRLAV8'
064200030626     C                   PARM                    WDAT8
064300030626     C* CONVERTO IN ORE IL NUMERO D GIORNI RITORNATI
064400030701     C                   EVAL      WORERIT = (GGLAV-1) * 24
064500030626     C                   ENDIF
064600030626     C*
064700030626     C                   ENDSR
064800000000     C*---------------------------------------------------------------------*
064900000000     C* *INZSR - ROUTINE INIZIALE
065000000000     C*---------------------------------------------------------------------*
065100000000     C     *INZSR        BEGSR
065200000000     C*--------------------
065300000000     C* RICEVIMENTO PARAMETRI
065400000000     C*--------------------
065500000000     C     *ENTRY        PLIST
065600000000     C                   PARM                    KPJBA
065700950626     C                   MOVEL     KPJBU         PARAM
065800000000     C*--------------------
065900000000     C* CAMPI RIFERITI AL DATA BASE
066000000000     C*--------------------
066100990810     C     *LIKE         DEFINE    TASDCM        KBLDCM                         *TITAS00F+10F
066200000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
066300000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE                         *TABEL00F
066400000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
066500950628     C     *LIKE         DEFINE    TBLKEY        KTBKEY
066600031218     C     *LIKE         DEFINE    WFPANN        KWPANN                         *WFDLPR0F
066700000000     C     *LIKE         DEFINE    WFPDIV        KWPDIV
066800000000     C     *LIKE         DEFINE    WFPARE        KWPARE
066900000000     C     *LIKE         DEFINE    WFPFLP        KWPFLP
067000000000     C     *LIKE         DEFINE    WFPMES        KWPMES
067100000000     C     *LIKE         DEFINE    WFPTSP        KWPTSP
067200000000     C     *LIKE         DEFINE    WFPRAN        KWPRAN
067300950626     C     *LIKE         DEFINE    WFPIOE        KWPIOE
067400950626     C     *LIKE         DEFINE    WFPIOE        WDA1                           *DI LAVORO
067500950626     C     *LIKE         DEFINE    WFPIOE        WDA2
067600950628     C     *LIKE         DEFINE    WFPIOE        WIOE
067700950628     C     *LIKE         DEFINE    TASLNP        WLNP
067800950628     C     *LIKE         DEFINE    TASTFP        WTFP
067900950628     C     *LIKE         DEFINE    TASLNA        WLNA
068000950628     C     *LIKE         DEFINE    TASTFA        WTFA
068100950628     C     *LIKE         DEFINE    TASZNC        WZNC
068200950628     C     *LIKE         DEFINE    TASNZD        WNZD
068300950628     C     *LIKE         DEFINE    TASCAD        WCAD
068400020702     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
068500020702     C     *LIKE         DEFINE    TBEKE1        KTEKE1
068600950628     C*--------------------
068700000000     C* CHIAVI DI LETTURA
068800000000     C*--------------------
068900041021     C* CHIAVE LETTURA TITAS26C - COMPLETA
069000000000     C     KEYTAS        KLIST
069100000000     C                   KFLD                    KBLDCM
069200000000     C*
069300000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
069400000000     C     KEYORG        KLIST
069500000000     C                   KFLD                    KORFIL
069600000000     C*
069700000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
069800000000     C     KEYTAB        KLIST
069900000000     C                   KFLD                    KTBUTE
070000000000     C                   KFLD                    KTBCOD
070100000000     C                   KFLD                    KTBKEY
070200000000     C*
070300031218     C* CHIAVE LETTURA WFDLPR1L - COMPLETA
070400000000     C     KEYWFP        KLIST
070500000000     C                   KFLD                    KWPANN
070600000000     C                   KFLD                    KWPDIV
070700000000     C                   KFLD                    KWPARE
070800000000     C                   KFLD                    KWPFLP
070900000000     C                   KFLD                    KWPMES
071000000000     C                   KFLD                    KWPTSP
071100000000     C                   KFLD                    KWPRAN
071200950626     C                   KFLD                    KWPIOE
071300020702     C*
071400020702     C* LETTURA TNTBE01L - PARZIALE
071500020702     C     KEYTBE        KLIST
071600020702     C                   KFLD                    KTECOD                         *UTENTE
071700020702     C                   KFLD                    KTEKE1                         *CODICE
071800000000     C*--------------------
071900000000     C* CAMPI DI CONTROLLO
072000000000     C*--------------------
072100000000     C* IMPOSTAZIONI INIZIALI
072200000000     C                   MOVEL     '0'           FINL00            1            *FINE PGM
072300000000     C***
072400000000     C* LANCIO PER SALDI
072500000000     C***
072600000000     C     PARTLA        IFEQ      'S'
072700000000     C                   Z-ADD     PARANN        DSANN
072800000000     C                   Z-ADD     PARMMI        DSMES
072900000000     C                   Z-ADD     *ZEROS        DSGIO
073000000000     C                   Z-ADD     DSDAT         PARDCI            8 0          *DATA INIZIALE
073100000000     C                   Z-ADD     PARANN        DSANN
073200000000     C                   Z-ADD     PARMMF        DSMES
073300000000     C                   Z-ADD     99            DSGIO
073400000000     C                   Z-ADD     DSDAT         PARDCF            8 0          *DATA FINALE
073500000000     C                   ENDIF
073600000000     C***
073700000000     C* LANCIO ESTEMPORANEO
073800000000     C***
073900000000     C     PARTLA        IFEQ      'E'
074000000000     C                   Z-ADD     PARDTI        G02DAT
074100000000     C                   Z-ADD     *ZEROS        G02INV
074200000000     C                   MOVEL     '0'           G02ERR
074300000000     C                   CALL      'XSRDA8'
074400000000     C                   PARM                    G02PAR
074500000000     C                   Z-ADD     G02INV        PARDCI            8 0          *DATA INIZIALE
074600000000     C                   Z-ADD     PARDTF        G02DAT
074700000000     C                   Z-ADD     *ZEROS        G02INV
074800000000     C                   MOVEL     '0'           G02ERR
074900000000     C                   CALL      'XSRDA8'
075000000000     C                   PARM                    G02PAR
075100000000     C                   Z-ADD     G02INV        PARDCF            8 0          *DATA FINALE
075200000000     C                   ENDIF
075300000000     C*
075400000000     C* CARICA I TIPI BOLLA CON RELATIVI FLAG RICAVI/SPEDIZIONI
075500000000     C                   EXSR      CARTBL
075600000000     C*
075700000000     C* CARICA DA ORGANIGRAMMA TUTTE LE FILIALI CON RELATIVA DIVISIONE E AREA
075800000000     C                   EXSR      CARORG
075900000000     C*
076000031218     C* CANCELLA TUTTI I RECORD DEL PERIODO RICHIESTO DA WFDLPR0F
076100000000     C                   EXSR      CANWFP
076200000000     C*
076300000000     C                   ENDSR
