000100950616     H*------------------------------------------------------------------------*
000200970905     H* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§ *
000300970905     H* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§ *
000400971127     H*                                                                        *
000500991209     H*        NB: UNA VOLTA LANCIATO NON RICOMPILARE IL PROGRAMMA !!!.        *
000600971127     H*                                                                        *
000700970905     H* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§ *
000800970905     H* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§ *
000900970904     H*------------------------------------------------------------------------*
001000030520     H* RICALCOLA LE COMPETENZE DI CONTO ECONOMICO - RICHIESTA PARAMETRI
001100020318     H* 2002 per TASFA2='S'                 -> USATO '8'
001200020408     H* 2002 ritassa marzo 0631996/1465/1466-> USATO '8'
001300020503     H* 2002 solo bolle FEDEX               -> USATO '9'
001400020520     H* DIC 2001 e GEN-APR 2002             -> USATO '0'
001500020520     H* APR 2002                            -> USATO 'X'
001600020605     H* GIU 2002                            -> USATO '*'
001700020607     H* MAG 2002                            -> USATO 'Z'
001800020611     H* GIU 2002                            -> USATO 'K'
001900020613     H* bolle non passate 2002              -> USATO 'J'
002000020618     H* errori di TITASQUE                  -> USATO 'W'
002100020619     H* Tutto GIU 2002                      -> USATO 'L'
002200020807     H* LUG E AGO 2002                      -> USATO '.'
002300020821     H* bolle 340 doppie                    -> USATO 'Q'
002400021031     H* Cliente 0891495 per voce 595        -> USATO 'M'
002500030205     H* Bolle GEN 03 dal TREC89R1           -> USATO 'N'
002600030303     H* Bolle GEN 03 con lna/lnp=136 POSTE  -> USATO 'O'
002700030303     H* Bolle GEN 03 INS lna/lnp=136 POSTE  -> USATO 'R'
002800030303     H* Bolle GEN 03 INS lna/lnp=136 POSTE  -> USATO 'S'
002900030430     H* Bolle GEN/FEB/MAR import o export   -> USATO 'T'
003000030507     H* Bolle GEN/FEB/MAR alcune bolle       > USATO 'U'
003100030507     H* Bolle GEN/FEB/MAR import o export DPD> USATO 'V'
003200030520     H* Bolle GEN/FEB/MAR lna o lnp FEDEX   -> USATO 'A'
003300030520     H* Bolle GEN/FEB/MAR casi particolari  -> USATO '1'
003400030520     H* Bolle GEN/FEB/MAR casi particolari  -> USATO '2'
003500030520     H* Bolle GEN/FEB     casi particolari  -> USATO '3'
003600030528     H* Bolle di alcuni clienti di APR      -> USATO '4'
003700030530     H* Bolle MAG         casi particolari  -> USATO '5'
003800030530     H* Bolle GEN->APR  diskc Bart.flag.coop-> USATO '6'
003900030613     H* Bolle di alcuni clienti             -> USATO '7'
004000950616     H*------------------------------------------------------------------------*
004100950616     H DECEDIT('0,') DATEDIT(*DMY.)
004200950616     F*------------------------------------------------------------------------*
004300950616     F* DATA BASE
004400950616     F*------------------------------------------------------------------------*
004500950616     F*-------------------
004600950616     F* BOLLE -> AAS + LNP
004700950616     F*-------------------
004800990812     FTITAS30C  UF   E           K DISK    USROPN
004900990812     F                                     IGNORE(TITASP00)
005000991227     F                                     RENAME(TITAS010:TAS130)
005100020611     F                                     RENAME(TITAS000:TAS030)
005200950616     F*-------------------
005300950616     F* BOLLE -> KSC
005400950616     F*-------------------
005500990812     FTITAS31C  UF   E           K DISK    USROPN
005600990812     F                                     IGNORE(TITASP00)
005700990812     F                                     RENAME(TITAS010:TAS131)
005800020611     F                                     RENAME(TITAS000:TAS031)
005900950616     F*-------------------
006000950616     F* BOLLE -> AAS + MGS
006100950616     F*-------------------
006200990812     FTITAS38C  UF   E           K DISK    USROPN
006300990812     F                                     IGNORE(TITASP00)
006400990812     F                                     RENAME(TITAS010:TAS138)
006500020611     F                                     RENAME(TITAS000:TAS038)
006600950616     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
006700950616     FAZORG01L  IF   E           K DISK
006800950616     D*------------------------------------------------------------------------*
006900950616     D* SCHIERE
007000950616     D*------------------------------------------------------------------------*
007100950616     D*------------------
007200950616     D* RIGHE IN STAMPA
007300950616     D*------------------
007400950616     D STA             S             66    DIM(10) CTDATA PERRCD(1)
007500981027     D*------------------
007600981027     D* ORGANIGRAMMA
007700981027     D*------------------
007800981027     D ORG             S              3  0 DIM(500)
007900981027     D ODES            S             20    DIM(500)
008000981027     D ODIT            S              3    DIM(500)
008100950616     D*------------------
008200950616     D* TABELLE CAPICONTO
008300950616     D*------------------
008400950616     D RI              S              4  0 DIM(4)
008500950616     D*-------------------
008600950616     D* ARCHITETTURA
008700950616     D*-------------------
008800950616     D KPJBA         E DS
008900950616     D*-------------
009000950616     D* DS REPERIMENTO DATI UTENTE
009100950616     D*-------------
009200020923     D TIBS34DS      E DS
009300020923     D DDATIUTE      E DS
009400020923     D AZUTEDS       E DS                  extname(AZUTE00F)
009500950616     D*-------------------
009600950616     D* DS DATA SPEDIZIONE
009700950616     D*-------------------
009800950616     D                 DS
009900970205     D  TASAAS                 1      4  0
010000970205     D  TASMGS                 5      8  0
010100020221     D  TASMES                 5      6  0
010200970205     D  TASDSP                 1      8  0
010300020221     D*-------------------
010400020221     D* DS DATA CONSEGNA MERCE REALE
010500020221     D*-------------------
010600020221     D                 DS
010700020221     D  DCMAAS                 1      4  0
010800020221     D  DCMMGS                 5      8  0
010900020221     D  DCMMES                 5      6  0
011000020221     D  TASDCM                 1      8  0
011100950616     D*-------------------
011200950616     D* CONTROLLO DATA (8)
011300950616     D*-------------------
011400950616     D WLBDA8          DS                  INZ
011500950616     D  G08DAT                 1      8  0
011600950616     D  G08INV                 9     16  0
011700950616     D  G08ERR                17     17
011800950616     D  G08TGI                18     22  0
011900950616     D*-------------------
012000950616     D* PARAMETRI IN ENTRATA
012100950616     D*-------------------
012200950616     D PARAM           DS
012300970205     D  PARORD                 1      1
012400970205     D  PARFIL                 2      4  0
012500980206     D  PARKS1                 5     11  0
012600970205     D  PARDS1                12     19  0
012700970205     D  PARAA1                12     15  0
012800970205     D  PARMG1                16     19  0
012900970205     D  PARDS2                20     27  0
013000970205     D  PARAA2                20     23  0
013100970205     D  PARMG2                24     27  0
013200970606     D  PARDF1                28     35  0
013300970606     D  PARDF2                36     43  0
013400980206     D  PARKS2                44     50  0
013500020611     D  PARTAS                51     51
013600020611     I*---------------------
013700020611     I* LETTURE FORMATI RECORD
013800020611     I*---------------------
013900020611     ITAS030        01
014000020611     ITAS130        02
014100020611     I*
014200020611     ITAS031        11
014300020611     ITAS131        12
014400020611     I*
014500020611     ITAS038        81
014600020611     ITAS138        82
014700950616     C*------------------------------------------------------------------------*
014800950616     C* MAIN LINE
014900950616     C*------------------------------------------------------------------------*
015000970205     C*
015100970205     C* APERTURA FILES
015200970205     C                   EXSR      APRE
015300950616     C*
015400950616     C* POSIZIONAMENTO E PRIMA LETTURA
015500950616     C                   EXSR      SETFIL
015600950616     C*
015700950616     C* LETTURA FINO A FINE FILE
015800950616DO  1C     $FINE         DOWEQ     '0'
015900950616     C*
016000950616     C* AGGIORNAMENTO BOLLE
016100950616     C                   EXSR      AGGTAS
016200950616     C*
016300950616     C* LETTURA RECORD SUCESSIVO
016400950616     C                   EXSR      LETFIL
016500950616E   1C                   ENDDO
016600950616     C*
016700950616     C* STAMPA TABULATO DI CONTROLLO
016800950616     C                   EXSR      STACTL
016900950616     C*
017000970205     C* CHIUSURA FILES
017100970205     C                   EXSR      CHIUDE
017200970205     C*
017300950616     C                   SETON                                        LR
017400950616     C*------------------------------------------------------------------------*
017500950616     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
017600950616     C*------------------------------------------------------------------------*
017700950616     C     SETFIL        BEGSR
017800950616     C*
017900950616     C* IMPOSTA LE CHIAVI DI LETTURA
018000950616     C                   Z-ADD     PARAA1        KTAAAS
018100950616     C                   Z-ADD     PARFIL        KTALNP
018200980206     C                   Z-ADD     PARKS1        KTAKSC
018300950616     C                   Z-ADD     PARMG1        KTAMGS
018400950616     C* POSIZIONAMENTO
018500950616SL  1C                   SELECT
018600950616     C     PARORD        WHENEQ    '1'
018700990812     C     KEYT30        SETLL     TITAS30C                           99
018800950616     C     PARORD        WHENEQ    '2'
018900990812     C     KEYT31        SETLL     TITAS31C                           99
019000950616     C     PARORD        WHENEQ    '3'
019100990812     C     KEYT38        SETLL     TITAS38C                           99
019200950616E   1C                   ENDSL
019300950616     C*
019400950616IF  1C     *IN99         IFEQ      *ON
019500950616     C                   MOVEL     '1'           $FINE
019600950616X   1C                   ELSE
019700950616     C                   EXSR      LETFIL
019800950616E   1C                   END
019900950616     C*
020000950616     C                   ENDSR
020100950616     C*------------------------------------------------------------------------*
020200950616     C* LETFIL - LETTURA RECORD
020300950616     C*------------------------------------------------------------------------*
020400950616     C     LETFIL        BEGSR
020500950616     C*
020600950616     C* LEGGE FINO A:
020700950616     C*     - FINE FILE
020800950616     C*     - TROVATO RECORD VALIDO
020900950616     C                   MOVEL     'N'           WRECOK
021000950616DO  1C     $FINE         DOWEQ     '0'
021100950616     C     WRECOK        ANDEQ     'N'
021200020611     C                   SETOFF                                       0102
021300020611     C                   SETOFF                                       1112
021400020611     C                   SETOFF                                       8182
021500950616SL  2C                   SELECT
021600950616     C     PARORD        WHENEQ    '1'
021700990812     C                   READ      TITAS30C                               99
021800950616     C     PARORD        WHENEQ    '2'
021900990812     C                   READ      TITAS31C                               99
022000950616     C     PARORD        WHENEQ    '3'
022100990812     C                   READ      TITAS38C                               99
022200950616E   2C                   ENDSL
022300950616     C*
022400950616IF  2C     *IN99         IFEQ      *ON
022500950616     C                   MOVEL     '1'           $FINE
022600950616X   2C                   ELSE
022700950616     C                   EXSR      CHKREC
022800970902     C                   ADD       1             RECLET
022900970902E   2C                   ENDIF
023000950616E   1C                   ENDDO
023100950616     C*
023200950616     C                   ENDSR
023300950616     C*------------------------------------------------------------------------*
023400950616     C* CHKREC - CONTROLLA VALIDITA' RECORD
023500950616     C*------------------------------------------------------------------------*
023600950616     C     CHKREC        BEGSR
023700950616     C*
023800971126     C                   MOVEL     'S'           WRECOK            1
023900970205     C*
024000950616     C* FILIALE NON SELEZIONATA, ESCLUDE
024100970205DO  1C                   DO
024200950616IF  2C     PARFIL        IFGT      *ZEROS
024300950616     C     PARFIL        ANDNE     TASLNP
024400950616IF  3C     PARORD        IFEQ      '1'
024500950616     C                   MOVEL     '1'           $FINE
024600950616E   3C                   ENDIF
024700950616     C                   MOVEL     'N'           WRECOK
024800950616     C                   LEAVE
024900950616E   2C                   ENDIF
025000980206     C*
025100981027     C* CLIENTE NON SELEZIONATO, ESCLUDE
025200980206IF  2C     PARKS1        IFGT      *ZEROS
025300980206IF  3C     TASKSC        IFLT      PARKS1
025400980206     C     TASKSC        ORGT      PARKS2
025500980206IF  4C     PARORD        IFEQ      '2'
025600980206     C     TASKSC        ANDGT     PARKS2
025700980206     C                   MOVEL     '1'           $FINE
025800980206E   4C                   ENDIF
025900980206     C                   MOVEL     'N'           WRECOK
026000980206     C                   LEAVE
026100980206E   3C                   ENDIF
026200980206E   2C                   ENDIF
026300950616     C*
026400950616     C* DATA SPEDIZIONE NON SELEZIONATA, ESCLUDE
026500950616IF  2C     PARDS1        IFGT      *ZEROS
026600950616IF  3C     TASDSP        IFLT      PARDS1
026700950616     C     TASDSP        ORGT      PARDS2
026800950616IF  4C     PARORD        IFEQ      '3'
026900950616     C     TASDSP        ANDGT     PARDS2
027000950616     C                   MOVEL     '1'           $FINE
027100950616E   4C                   ENDIF
027200950616     C                   MOVEL     'N'           WRECOK
027300950616     C                   LEAVE
027400950616E   3C                   ENDIF
027500950616E   2C                   ENDIF
027600970606     C*
027700970606     C* DATA FATTURA NON SELEZIONATA, ESCLUDE
027800970606IF  2C     PARDF1        IFGT      *ZEROS
027900970606IF  3C     TASDFT        IFLT      PARDF1
028000970606     C     TASDFT        ORGT      PARDF2
028100970606     C                   MOVEL     'N'           WRECOK
028200970606     C                   LEAVE
028300970606E   3C                   ENDIF
028400970606E   2C                   ENDIF
028500020611     C*
028600020611     C* BOLLE DI TITAS00F
028700020611IF  2C     PARTAS        IFEQ      '0'
028800020611IF  3C     *IN01         IFEQ      *ON
028900020611     C     *IN11         OREQ      *ON
029000020611     C     *IN81         OREQ      *ON
029100020611X   3C                   ELSE
029200020611     C                   MOVEL     'N'           WRECOK
029300020611     C                   LEAVE
029400020611E   3C                   ENDIF
029500020611E   2C                   ENDIF
029600020611     C*
029700020611     C* BOLLE DI TITAS10F
029800020611IF  2C     PARTAS        IFEQ      '1'
029900020611IF  3C     *IN02         IFEQ      *ON
030000020611     C     *IN12         OREQ      *ON
030100020611     C     *IN82         OREQ      *ON
030200020611X   3C                   ELSE
030300020611     C                   MOVEL     'N'           WRECOK
030400020611     C                   LEAVE
030500020611E   3C                   ENDIF
030600020611E   2C                   ENDIF
030700030613     C*
030800000327E   1C                   ENDDO
030900981102     C*
031000950616     C                   ENDSR
031100950616     C*------------------------------------------------------------------------*
031200950616     C* AGGTAS - AGGIORNA LE BOLLE
031300950616     C*------------------------------------------------------------------------*
031400950616     C     AGGTAS        BEGSR
031500960424     C*
031600960517     C* OCCORRE FARE UN AGGIORNAMENTO REALE PER FARE ENTRARE LA BOLLA NEL JRN
031700030613     C                   MOVEL     '7'           TASLL2
031800950616     C*
031900950616SL  1C                   SELECT
032000950616     C     PARORD        WHENEQ    '1'
032100990812     C                   EXCEPT    UPD130
032200950616     C     PARORD        WHENEQ    '2'
032300990812     C                   EXCEPT    UPD131
032400950616     C     PARORD        WHENEQ    '3'
032500990812     C                   EXCEPT    UPD138
032600950616E   1C                   ENDSL
032700950616     C                   ADD       1             RECAGG
032800950616     C*
032900950616     C                   ENDSR
033000950616     C*------------------------------------------------------------------------*
033100950616     C* STACTL - STAMPA DI CONTROLLO
033200950616     C*------------------------------------------------------------------------*
033300950616     C     STACTL        BEGSR
033400950616     C*--------------------
033500950616     C* DECODIFICHE
033600950616     C*--------------------
033700950616IF  1C     PARFIL        IFGT      *ZEROS
033800981027     C                   SETOFF                                           28
033900981027     C                   Z-ADD     1             I
034000981027     C     PARFIL        LOOKUP    ORG(I)                                 28
034100981027     C                   MOVEL     ODES(I)       DECFIL           25
034200950616E   1C                   ENDIF
034300950616     C*
034400950616IF  1C     PARDS1        IFGT      *ZEROS
034500950616     C                   Z-ADD     PARDS1        G08INV
034600950616     C                   Z-ADD     *ZEROS        G08DAT
034700950616     C                   MOVEL     '3'           G08ERR
034800950616     C                   CALL      'XSRDA8'
034900950616     C                   PARM                    WLBDA8
035000950616     C                   Z-ADD     G08DAT        DECDS1            8 0
035100950616     C*
035200950616     C                   Z-ADD     PARDS2        G08INV
035300950616     C                   Z-ADD     *ZEROS        G08DAT
035400950616     C                   MOVEL     '3'           G08ERR
035500950616     C                   CALL      'XSRDA8'
035600950616     C                   PARM                    WLBDA8
035700950616     C                   Z-ADD     G08DAT        DECDS2            8 0
035800950616E   1C                   ENDIF
035900950616     C* S T A M P A
036000950616     C                   EXCEPT    STM
036100950616     C*
036200950616     C                   ENDSR
036300950616     C*------------------------------------------------------------------------*
036400950616     C* APRE - APERTURA FILES
036500950616     C*------------------------------------------------------------------------*
036600950616     C     APRE          BEGSR
036700950616     C*
036800950616SL  1C                   SELECT
036900950616     C     PARORD        WHENEQ    '1'
037000990812     C                   OPEN      TITAS30C
037100950616     C     PARORD        WHENEQ    '2'
037200990812     C                   OPEN      TITAS31C
037300950616     C     PARORD        WHENEQ    '3'
037400990812     C                   OPEN      TITAS38C
037500950616E   1C                   ENDSL
037600950616     C*
037700950616     C                   ENDSR
037800950616     C*------------------------------------------------------------------------*
037900950616     C* CHIUDE - CHIUSURA FILES
038000950616     C*------------------------------------------------------------------------*
038100950616     C     CHIUDE        BEGSR
038200950616     C*
038300950616SL  1C                   SELECT
038400950616     C     PARORD        WHENEQ    '1'
038500990812     C                   CLOSE     TITAS30C
038600950616     C     PARORD        WHENEQ    '2'
038700990812     C                   CLOSE     TITAS31C
038800950616     C     PARORD        WHENEQ    '3'
038900990812     C                   CLOSE     TITAS38C
039000950616E   1C                   ENDSL
039100950616     C*
039200950616     C                   ENDSR
039300981027     C*------------------------------------------------------------------------*
039400981027     C* CARTAB - CARICAMENTO TABELLE
039500981027     C*------------------------------------------------------------------------*
039600981027     C     CARTAB        BEGSR
039700981027     C*---
039800981027     C* ORGANIGRAMMA
039900981027     C*---
040000981027     C                   Z-ADD     *ZEROS        I                 4 0
040100981027     C     *LOVAL        SETLL     AZORG01L
040200981027     C                   READ      AZORG01L                               99
040300981027DO  1C     *IN99         DOWEQ     *OFF
040400981027     C                   ADD       1             I
040500981027     C                   Z-ADD     ORGFIL        ORG(I)
040600981027     C                   MOVEL     ORGDES        ODES(I)
040700981027     C                   MOVEL     ORGDIT        ODIT(I)
040800981027     C                   READ      AZORG01L                               99
040900981027E   1C                   ENDDO
041000981027     C*
041100981027     C                   ENDSR
041200020923     C*-------------------------------------------------------------------------
041300020923     C* REPERISCE I DATI UTENTE
041400020923     C*-------------------------------------------------------------------------
041500020923     C     REPDATIUTE    BEGSR
041600020923     C*
041700020923     C* INIZIALIZZA VARIABILI DI WRK
041800020923     C                   CLEAR                   TIBS34DS
041900020923     C                   CLEAR                   AZUTEDS
042000020923     C                   CLEAR                   DDATIUTE
042100020923     C*
042200020923     C     *DTAARA       DEFINE    §azute        azuteds
042300020923     C     *DTAARA       DEFINE    §datiute      ddatiute
042400020923     C                   IN(E)     *DTAARA
042500020923if  1C                   IF        %Error
042600020923     c                   EVAL      I34Tla = 'L'
042700020923     C                   CALL      'TIBS34R'
042800020923     C                   PARM                    Tibs34Ds
042900020923     C                   IN        *DTAARA
043000020923e   1C                   ENDIF
043100020923     C*
043200020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
043300020923     C                   MOVEL(P)  RSUT          DSFIRS           20
043400020923     C*
043500020923     C                   ENDSR
043600950616     C*------------------------------------------------------------------------*
043700950616     C* *INZSR - ROUTINE INIZIALE
043800950616     C*------------------------------------------------------------------------*
043900950616     C     *INZSR        BEGSR
044000950616     C*--------------------
044100950616     C* RICEVIMENTO PARAMETRI
044200950616     C*--------------------
044300950616     C     *ENTRY        PLIST
044400950616     C                   PARM                    KPJBA
044500950616     C                   MOVEL     KPJBU         PARAM
044600950616     C*--------------------
044700950616     C* CAMPI RIFERITI AL DATA BASE
044800950616     C*--------------------
044900990812     C     *LIKE         DEFINE    TASAAS        KTAAAS
045000950616     C     *LIKE         DEFINE    TASLNP        KTALNP
045100950616     C     *LIKE         DEFINE    TASKSC        KTAKSC
045200950616     C     *LIKE         DEFINE    TASMGS        KTAMGS
045300981027     C     *LIKE         DEFINE    ORGDIT        DITLNP
045400981027     C     *LIKE         DEFINE    ORGDIT        DITLNA
045500981027     C     *LIKE         DEFINE    ORGDIT        DITTFP
045600981027     C     *LIKE         DEFINE    ORGDIT        DITTFA
045700981027     C     *LIKE         DEFINE    ORGDIT        DITFLP
045800981027     C     *LIKE         DEFINE    ORGDIT        DITFL2
045900950616     C*--------------------
046000950616     C* CHIAVI DI LETTURA
046100950616     C*--------------------
046200990812     C* LETTURA TITAS30C - PARZIALE
046300950616     C     KEYT30        KLIST
046400950616     C                   KFLD                    KTAAAS
046500950616     C                   KFLD                    KTALNP
046600990812     C* LETTURA TITAS31C - PARZIALE
046700950616     C     KEYT31        KLIST
046800950616     C                   KFLD                    KTAKSC
046900990812     C* LETTURA TITAS38C - COMPLETA
047000950616     C     KEYT38        KLIST
047100950616     C                   KFLD                    KTAAAS
047200950616     C                   KFLD                    KTAMGS
047300950616     C                   KFLD                    KTAKSC
047400990812     C* LETTURA TITAS38C - PARZIALE
047500950616     C     KE2T38        KLIST
047600950616     C                   KFLD                    KTAAAS
047700950616     C                   KFLD                    KTAMGS
047800950616     C*--------------------
047900950616     C* CAMPI DI CONTROLLO
048000950616     C*--------------------
048100950616     C                   MOVEL     '0'           $FINE             1
048200950616     C                   MOVEL     'N'           WRECOK            1
048300950616     C                   Z-ADD     *ZEROS        RECLET            9 0
048400950616     C                   Z-ADD     *ZEROS        RECAGG            9 0
048500950616     C                   TIME                    ORA               6 0
048600950616     C                   Z-ADD     UDATE         DATA              6 0
048700980209     C                   Z-ADD     *ZEROS        NUMDTA            9 0
048800950616     C*
048900950616     C* REPERISCE I DATI DELL'UTENTE
049000020923     C                   EXSR      REPDATIUTE
049100981027     C*
049200981027     C* CARICA TABELLE
049300981027     C                   EXSR      CARTAB
049400950616     C*
049500950616     C                   ENDSR
049600950616     O*------------------------------------------------------------------------*
049700990812     O* AGGIORNAMENTO RECORD DI TITAS10F (CAMPO QUALSIASI)
049800950616     O*------------------------------------------------------------------------*
049900950616     OTAS130    E            UPD130
050000960420     O                       TASLL2
050100950616     OTAS131    E            UPD131
050200960420     O                       TASLL2
050300950616     OTAS138    E            UPD138
050400960420     O                       TASLL2
050500950616     O*------------------------------------------------------------------------*
050600950616     O* STAMPA DI CONTROLLO
050700950616     O*------------------------------------------------------------------------*
050800950616     OQSYSPRT   E            STM               2
050900950616     O                       STA(1)              66
051000950616     O                       STA(2)             132
051100950616     O                       DSFIRS              20
051200950721     O                       DATA               106 '  /  /  '
051300950616     O                       ORA                117 '  :  :  '
051400950616     O                       PAGE          Z    132
051500950616     O*------------------------
051600950616     O          E            STM               3
051700950616     O                       STA(3)              66
051800950616     O                       STA(3)             132
051900950616     O*------------------------
052000950616     O          E            STM               4
052100950616     O                       STA(4)              66
052200950616     O                       STA(5)             132
052300950616     O                       PARFIL        Z     21
052400950616     O                       DECFIL              47
052500980206     O                       PARKS1        Z     65
052600980206     O                       PARKS2        Z     75
052700020611     O                       PARTAS              87
052800020611     O                                           88 '0'
052900950616     O                       DECDS1             119 '  /  /    '
053000950616     O                       DECDS2             132 '  /  /    '
053100950616     O*------------------------
053200950616     O          E            STM               5
053300950616     O                       STA(3)              66
053400950616     O                       STA(3)             132
053500950616     O*------------------------
053600950616     O          E            STM         2
053700950616     O                       STA(6)              66
053800950616     O                       STA(7)             132
053900950616     O                       RECLET        2     30
054000950616     O                       RECAGG        2     96
054100950616     O*------------------------
054200950616     O          E            STM              55
054300950616     O                       STA(8)              99
054400950616     O*------------------------------------------------------------------------*
054500980209**   STA
054600950616XXXXXXXXXXXXXXXXXXXX            TRECE9R    ***  RICALCOLO COMPETEN1
054700950710ZE DI CONTO ECONOMICO ***       99/99/99   99:99:99        P. 99992
054800950616------------------------------------------------------------------3
054900950616LINEA DI PARTENZA 999 XXXXXXXXXXXXXXXXXXXXXXXXX   CLIENTE 9999999 4
055000020611- 9999999  BOLLE IN XXF    DATA SPEDIZIONE 99/99/9999 - 99/99/99995
055100950710RECORD LETTI       999.999.999                                    6
055200950710RECORD AGGIORNATI  999.999.999                                    7
055300950616                           ***  F I N E   P R O S P E T T O  ***  8
0554009506169
05550095061610
