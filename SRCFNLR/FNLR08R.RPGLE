000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200940713     H* FNLR08R *----------------------------------------------------*
000300940713     H*         - ELENCO FOGLI VARI/VERBALI
000400000000     H*--------------------------------------------------------------*
000500910322     FTABEL00F  IF   E           K DISK
000600910322     FAZORG01L  IF   E           K DISK
000700020905     fFnfvv08l  if   e           k Disk
000800940713     FFNFVV01L  IF   E           K DISK
000900940713     F                                     RENAME(FNFVV000:FNFVV001)
001000940715     FSYSPRT    O    F  132        PRINTER OFLIND(*INOF)
001100940715     F                                     USROPN
001200940714     F                                     FORMLEN(66)
001300940714     F                                     FORMOFL(63)
001400940713     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOE)
001500940713     F                                     USROPN
001600940713     D RG              S             66    DIM(13) CTDATA PERRCD(1)
001700940713     D VE              S             66    DIM(10) CTDATA PERRCD(1)
001800940713     D CM1             S              1    DIM(42) CTDATA PERRCD(42)            COMANDO QCMDEXC
001900940713     D CM2             S              1    DIM(42) CTDATA PERRCD(42)            COMANDO QCMDEXC
002000910322     D CAN             S             15    DIM(9)
002100940713     D***
002200940713     D* PARAMETRI PER CONTROLLO DATA E TRASFORM IN GIORNI - XSRDA8 -
002300940713     D***
002400940713     D WLBDAT          DS                  INZ
002500940713     D  G02DAT                 1      8  0
002600940713     D  G02INV                 9     16  0
002700940713     D  G02ERR                17     17
002800940713     D  G02TGI                18     22  0
002900910322     D                 DS
003000910322     D  FL3E                   1      1
003100910322     D  UGUA                   2      2
003200910322     D  DE3E                   3     15
003300910322     D  COMCAN                 1     15
003400940802     D* DS STANDARD FOGLI VARI - DATI DAL FNLR07R
003500940802     D DSLR01        E DS
003600940802     D*
003700161006     D DS7B          E DS
003800161006     D DS7N          E DS
003900050512     D DFVVSPN       E DS
004000940713     D KPJBA         E DS
004100900604     D CNCR80        E DS
004200900601     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004300940713     D***
004400940713     D* COSTANTI
004500940713     D***
004600940713     D CANNUL          C                   CONST('ANNULLATO')
004700940713     D CBLOCC          C                   CONST('BLOCCATO ')
004800940713     D CCHIUS          C                   CONST('CHIUSO   ')
004900900625      *---------------------------------------------------------------*
005000900625      * INDICATORI USATI
005100940713      *---------------------------------------------------------------*
005200910322      * 02    = DATA DAL AL IMMESSA
005300940713      * 11    = ON -  RICHIESTO UNO SPECIFICO FOGLIO
005400940713      * 30    = LETTURA
005500940713      * 31    = COMODO
005600940713      * 90    = ERRORE GENERICO
005700000000     C*---------------------------------------------------------------*
005800000000     C     *ENTRY        PLIST
005900000000     C                   PARM                    KPJBA
006000940802     C                   MOVEL     KPJBU         DSLR01
006100000000     C                   Z-ADD     1             CODUT
006200900601     C                   CALL      'X§PARUT'
006300900601     C                   PARM                    UT§DSE
006400940713     C                   MOVEL     RAGUT         P0CDSA           20
006500900601     C                   MOVEL     REC80         CNCR80
006600900511     C*---------------------------------------------------------------*
006700940713     C* SE HA DATO ERROE IL QCMDEXC SUL FILE PRINTER ESCO DAL PGM
006800940713    0C     *IN90         IFEQ      *OFF
006900940713     C*
007000900601     C* CONTROLLO RICHIESTE FATTE
007100940713     C                   EXSR      CTRPAR
007200900523     C*
007300940713     C* NON E' STATA SCELTO UNO SPECIFICO FOGLIO
007400940713    1C     *IN11         IFEQ      *OFF
007500940713     C*
007600940713     C     KFVV          SETLL     FNFVV000
007700940713     C  N02KFVV          READE     FNFVV000                               30
007800020905     C*!!02DLRNPG        READE     FNFVV000                               30
007900020905     C   02Kfvv1         READE     FNFVV000                               30
008000940713     C*
008100940713   X1C                   ELSE
008200940713     C*
008300940713     C* SE IMMESSO UN FOGLIO FACCIO CHAIN
008400940713     C     KFVV2         CHAIN     FNFVV001                           30
008500940713    1C                   ENDIF
008600940713     C*
008700940713    1C     *IN30         DOWEQ     *OFF
008800940713     C                   CLEAR                   W001A
008900940713     C*
009000900601     C* CONTROLLO FINE LETTURA SE IMMESSA DATA AL
009100940713    2C     *IN02         IFEQ      *ON
009200940802     C     FVVDFV        ANDGT     DLRAAL
009300941027     C*
009400900601     C                   SETON                                        30
009500940713     C*
009600940713   X2C                   ELSE
009700941027     C*
009800941027     C* SE LA FIL IN GESTIONE NON E' QUELLA CHE VOGLIO LA SALTO
009900941027    3C     FVVFGS        IFEQ      DLRFGS
010000910322     C*
010100940713     C* STAMPA ELENCO FOGLI
010200940713     C*                       W001A = N --> DA NON STAMPARE
010300941027    4C     DLRFS1        IFEQ      'S'
010400940713     C     *IN11         ANDEQ     *OFF
010500940713     C*
010600940713     C* SCELTI SOLO I FV CHIUSI
010700940802     C     DLRTFV        IFEQ      'C'
010800940713     C*
010900941027    5C     FVVATB        ANDEQ     *BLANKS
011000940713     C     FVVFCF        ANDNE     'S'
011100940713     C                   MOVEL     'N'           W001A             1
011200941027    5C                   ENDIF
011300940713     C*
011400940713     C* SCELTI SOLO I FV APERTI
011500940802     C     DLRTFV        IFEQ      'A'
011600940713     C*
011700941027    5C     FVVFCF        ANDEQ     'S'
011800940713     C                   MOVEL     'N'           W001A
011900941027    5C                   ENDIF
012000941027     C*
012100941027    4C                   ENDIF
012200911009     C*
012300940713     C* S T A M P O   E L E N C O   F O G L I
012400941027    4C     DLRFS1        IFEQ      'S'
012500940713     C     W001A         ANDEQ     ' '
012600940713     C                   EXSR      DECOD
012700940713     C                   EXSR      STAELE
012800941027    4C                   ENDIF
012900900601     C*
013000940713     C* S T A M P O   V E R B A L E
013100940713     C* SOLO SE RICHISTO X FOGLI APERTI E NON ANNULLATI
013200941027    4C     DLRFS2        IFEQ      'S'
013300940713     C     W001A         ANDEQ     ' '
013400940713     C     FVVATB        ANDEQ     ' '
013500940713     C     FVVFCF        ANDNE     'S'
013600940713     C* DECODIFICHE
013700940713     C                   EXSR      DECOD
013800940713     C* PRIMA PARTE
013900910322     C                   EXCEPT    STVC
014000940713     C* RIGHE DA SCRIVERE
014100940713     C                   DO        23
014200940713     C                   EXCEPT    STVCRG
014300940713     C                   ENDDO
014400940713     C* PARTE FINALE
014500940713     C                   EXCEPT    STVCFI
014600941027    4C                   ENDIF
014700941027    3C                   ENDIF
014800940713     C*
014900940713     C* RILETTURA
015000940713    3C     *IN11         IFEQ      *OFF
015100940713     C  N02KFVV          READE     FNFVV000                               30
015200020905     C*!!02DLRNPG        READE     FNFVV000                               30
015300020905     c     Kfvv1         READE     FNFVV000                               30
015400940713   X3C                   ELSE
015500940713     C                   SETON                                        30
015600940713    3C                   ENDIF
015700940713    2C                   ENDIF
015800940713     C*
015900940713    1C                   ENDDO
016000940713     C*
016100940713    0C                   ENDIF
016200940713     C*
016300940713     C*  E L E N C O   F O G L I  --> RIGA FINALE
016400940802    3C     DLRFS1        IFEQ      'S'
016500940713     C                   EXCEPT    FONFV
016600940713    3C                   ENDIF
016700900601     C*
016800000000     C                   SETON                                        LR
016900940713     C*----------------------------------------------------------------
017000940713     C*--- CONTROLLI PARAMETRI PASSATI
017100940713     C*----------------------------------------------------------------
017200940713     C     CTRPAR        BEGSR
017300911009     C*
017400940713     C* 11 ON - IMMESSO NUMERO FOGLIO
017500940802     C     DLRNFV        IFNE      0
017600900608     C                   SETON                                        11
017700940715     C*
017800940802     C                   CLEAR                   DLRTFV
017900900608     C                   ELSE
018000940713     C*
018100940713     C* 02 ON - IMMESSA DATA AL
018200940802     C     DLRGAL        IFNE      0
018300900601     C                   SETON                                        02
018400940713     C                   ENDIF
018500940713     C*
018600940713     C* DECODIFICO TIPO FOGLIO
018700940714     C                   SELECT
018800940802     C     DLRTFV        WHENEQ    'C'
018900940713     C                   MOVEL     'CHIUSO'      P0DTFV            6
019000940802     C     DLRTFV        WHENEQ    'A'
019100940713     C                   MOVEL     'APERTO'      P0DTFV
019200940714     C*
019300940714     C                   OTHER
019400940714     C                   MOVEL     'TUTTI '      P0DTFV
019500940714     C                   ENDSL
019600940715     C                   ENDIF
019700911009     C*
019800940713     C* DECODIFICO CATEGORIA FOGLI
019900920807     C                   MOVEL     '7N'          COD
020000920807     C                   MOVEL     *BLANKS       KEY
020100940802     C                   MOVEL     DLRNPG        KEY
020200920807     C     KTAB2         CHAIN     TABEL                              30
020300940713     C     *IN30         IFEQ      *OFF
020400940713     C  N30              MOVEL     TBLUNI        DS7N
020500940713     C  N30              MOVEL     §7NDSV        P0CDSV
020600940713     C  N30              MOVEL     §7NDES        P0DNPG           20
020700940713     C                   ENDIF
020800920807     C*
020900940713     C* DECODIFICO FILIALE GESTIONE
021000940802     C     DLRFGS        CHAIN     AZORG                              30
021100940713     C  N30              MOVEL     ORGDES        P0DFGS           20
021200940713     C   30              MOVEL     *BLANKS       P0DFGS
021300940713     C***
021400910322     C* CARICO CODICI ANOMALIE
021500940713     C***
021600940713     C* SOLO SE STAMPA VERBALI
021700940802     C     DLRFS2        IFEQ      'S'
021800940713     C*
021900910322     C                   MOVEL     '3E'          COD
022000910322     C                   Z-ADD     1             I
022100910322     C                   MOVEL     '='           UGUA
022200910322     C*
022300910322     C                   SETOFF                                       30
022400910322     C     KTAB          SETLL     TABEL
022500910322     C     *IN30         DOWEQ     '0'
022600910322     C     KTAB          READE     TABEL                                  30
022700910322     C  N30TBLFLG        IFEQ      *BLANKS
022800910322     C                   MOVEL     TBLKEY        FL3E
022900910322     C                   MOVEL     TBLUNI        DE3E
023000910322     C                   MOVEL     COMCAN        CAN(I)
023100910322     C                   ADD       1             I                 2 0
023200910322     C* SE GIA' TROVATI 9 ESCO DAL CARICAMENTO
023300910322     C     I             IFGT      9
023400910322     C                   SETON                                        30
023500910322     C                   END
023600910322     C                   END
023700910322     C*
023800910322     C                   END
023900940713     C                   END
024000940713     C***
024100940713     C* DATA E ORA DEL GIORNO
024200940713     C***
024300940713     C                   TIME                    W0120            12 0
024400940713     C                   MOVEL     W0120         P0CTIM            6 0
024500940713     C                   MOVE      W0120         W0060             6 0
024600940713     C* DATA A 8
024700940713     C                   MOVE      *ZEROS        G02INV
024800940713     C                   MOVE      W0060         G02DAT
024900940713     C                   MOVEL     ' '           G02ERR
025000940713     C                   CALL      'XSRDA8'
025100940713     C                   PARM                    WLBDAT
025200940713     C                   Z-ADD     G02DAT        P0CDAT
025300900601     C                   ENDSR
025400940713     C*----------------------------------------------------------------
025500940713     C* DECODIFICHE
025600940713     C*----------------------------------------------------------------
025700940713     C     DECOD         BEGSR
025800940713     C* DECODIFICA TIPO INVENTARIO
025900940713     C     FVVSPG        IFNE      ' '
026000940713     C                   MOVEL     '7B'          COD
026100940713     C                   MOVEL     *BLANKS       KEY
026200940713     C                   MOVEL     FVVSPG        KEY
026300940713     C     KTAB2         CHAIN     TABEL                              30
026400161006     c   30              clear                   ds7b
026500161006     C  N30              MOVEL     TBLUNI        ds7b
026600161006     C                   MOVEL     §7bdesb       P0DSPG            6
026700940713     C                   END
026800940713     C*
026900940713     C* DATA FOGLIO
027000940713     C                   MOVE      FVVDFV        G02INV
027100940713     C                   MOVE      *ZEROS        G02DAT
027200940713     C                   MOVEL     3             G02ERR
027300940713     C                   CALL      'XSRDA8'
027400940713     C                   PARM                    WLBDAT
027500940713     C                   Z-ADD     G02DAT        P0CDFV
027600050512     c* spuntatore
027700050512     c                   movel     fvvspn        dfvvspn
027800940713     C                   ENDSR
027900940713     C*----------------------------------------------------------------
028000940713     C*--- STAMPO ELENCO FOGLI
028100940713     C*----------------------------------------------------------------
028200940713     C     STAELE        BEGSR
028300940713     C                   CLEAR                   P0CCHI
028400910322     C* CHIUSO
028500940713     C     FVVFCF        IFEQ      'S'
028600940713     C                   MOVEL     CCHIUS        P0CCHI
028700940713     C                   ENDIF
028800940713     C* BLOCCATO
028900940713     C     FVVFCF        IFNE      ' '
029000940713     C     FVVFCF        ANDNE     'S'
029100940713     C                   MOVEL     CBLOCC        P0CCHI
029200940713     C                   ENDIF
029300940713     C*  ANNULLATO
029400940713     C     FVVATB        IFEQ      '*'
029500940713     C                   MOVEL     CANNUL        P0CCHI            9
029600940713     C                   ENDIF
029700910322     C* TESTATA
029800900914     C   OF              DO
029900900914     C                   EXCEPT    TESTA
030000900914     C                   SETOFF                                       OF
030100900914     C                   END
030200910322     C*
030300940713     C* DETTAGLIO FOGLIO
030400910322     C                   EXCEPT    STFV
030500900601     C*
030600900601     C                   ENDSR
030700940713      *----------------------------------------------------------------
030800940713      * INZSR INIZIALE
030900940713      *----------------------------------------------------------------
031000940713     C     *INZSR        BEGSR
031100940713      ***
031200940713      * CHIAVI
031300940713      ***
031400940713     C     KFVV          KLIST
031500020905     C                   KFLD                    DLRFGS
031600940802     C                   KFLD                    DLRNPG
031700940802     C                   KFLD                    DLRADA
031800020905     c     Kfvv1         Klist
031900020905     c                   Kfld                    DlrFgs
032000020905     c                   Kfld                    DlrNpg
032100940713     C     KFVV2         KLIST
032200940802     C                   KFLD                    DLRNPG
032300940802     C                   KFLD                    DLRNFV
032400941027     C                   KFLD                    DLRFGS
032500940713     C     KTAB          KLIST
032600940713     C                   KFLD                    CODUT
032700940713     C                   KFLD                    COD               2
032800940713     C     KTAB2         KLIST
032900940713     C                   KFLD                    CODUT
033000940713     C                   KFLD                    COD               2
033100940713     C                   KFLD                    KEY               8
033200940713     C***
033300940713     C* LIKE DEFN
033400940713     C***
033500940713     C     *LIKE         DEFINE    FVVDFV        P0CDFV
033600940713     C     *LIKE         DEFINE    FVVDFV        P0CDAT
033700940713     C     *LIKE         DEFINE    §7NDSV        P0CDSV
033800940713     C***
033900940713     C* INDICATORE DI ECCEDENZA RIGA IN STAMPA
034000940713     C***
034100020906     C**!!!              SETON                                        OF
034200940713     C***
034300940713     c* Eseguo OVRPRTF sul file di stampa
034400940713     C***
034500940713     C                   Z-ADD     42            LUNG             15 5
034600940713     C                   MOVEL     *BLANKS       COMMAN           80
034700940713     C                   MOVEA     CM1           COMMAN
034800940713     C                   CALL      'QCMDEXC'                            90      ERRORE=USCITA
034900940713     C                   PARM                    COMMAN
035000940713     C                   PARM                    LUNG
035100940713     C                   OPEN      SYSPRT                               90      PRINTER FILE
035200020906
035300020906      * Convertendo il pgm in ile l'indicatore OF dopo la open del file risulta spento
035400020906      * così ho spostato l'istruzione
035500020906     C                   SETON                                        OF
035600940713     C*
035700940713     C                   MOVEL     *BLANKS       COMMAN           80
035800940713     C                   MOVEA     CM2           COMMAN
035900940713     C                   CALL      'QCMDEXC'                            90      ERRORE=USCITA
036000940713     C                   PARM                    COMMAN
036100940713     C                   PARM                    LUNG
036200940713     C                   OPEN      QSYSPRT                              90      PRINTER FILE
036300940713     C                   ENDSR
036400900606     C*---------------------------------------------------------------*
036500940714     O***
036600940714     O* ELENCO FOGLI VARI
036700940714     O***
036800940713     OSYSPRT    E            TESTA            02
036900940713     O                       RG(1)               66
037000940713     O                       RG(2)              132
037100940713     O                       P0CDSA              20
037200911009     O                                           21 '/'
037300911009     O                       SIMFEL              24
037400940713     O                       P0CDAT             115 '  .  .    '
037500910322     O                       PAGE          Z    132
037600940713     O          E            TESTA            03
037700940713     O                       RG(3)               66
037800940713     O                       RG(4)              132
037900940713     O                       KNSIF               10
038000940713     O                       KNMUS               24
038100940802     O                       DLRNPG              66
038200940713     O                       P0DNPG              87
038300940713     O                       P0CTIM             115 '  :  :  '
038400940713     O          E            TESTA            05
038500940713     O                       RG(5)               66
038600940713     O                       RG(6)              132
038700940802     O                       DLRFGS              20
038800940713     O                       P0DFGS              41
038900940802     O                       DLRNFV        Z     57
039000940802     O                       DLRGDA              79 '  /  /    '
039100940802     O                       DLRGAL              94 '  /  /    '
039200940802     O                       DLRTFV             110
039300940713     O                       P0DTFV             117
039400940713     O          E            TESTA            07
039500940713     O                       RG(11)              66
039600940713     O                       RG(11)             132
039700940713     O          E            TESTA            08
039800940713     O                       RG(7)               66
039900940713     O                       RG(9)              132
040000940714     O          E            TESTA            09
040100940713     O                       RG(11)              66
040200940713     O                       RG(11)             132
040300940713     O          E            STFV        1
040400940713     O                       FVVNFV        Z
040500940713     O                       P0DSPG            +  1
040600940713     O                       P0CDFV            +  2 '  /  /    '
040700940713     O                       FVVDSF            +  2
040800050512     O                       §fvvspn           +  2
040900940713     O                       P0CCHI            +  5
041000940713     O          E            FONFV       2
041100940713     O                       RG(12)              66
041200940713     O                       RG(13)             132
041300940714     O***
041400940714     O* STAMPA VERBALI
041500940714     O***
041600900628     OQSYSPRT   E            STVC             02
041700900628     O                       VE(1)               66
041800960926     O                       DLRFGS              20
041900940713     O                       P0CDSV              47
042000940713     O                       P0CDAT              66 '  /  /    '
042100940713     O          E            STVC             03
042200940713     O                       P0CTIM              66 '  :  :  '
042300910322     O          E            STVC             04
042400900628     O                       VE(2)               66
042500940802     O                       DLRNPG              14
042600940713     O                       P0DNPG              35
042700940713     O                       FVVNFV        Z     48
042800940713     O                       P0CDFV              66 '  /  /    '
042900910322     O          E            STVC             06
043000910322     O                       VE(3)               66
043100940713     O                       FVVDSF              48
043200910322     O          E            STVC             08
043300910322     O                       VE(4)               66
043400940713     O          E            STVC             10
043500910322     O                       VE(5)               66
043600940713     O          E            STVC             12
043700940713     O                       VE(6)               66
043800940713     O          E            STVCRG      2
043900940713     O                       VE(7)               66
044000940713     O          E            STVCFI           61
044100940713     O                       VE(8)               66
044200940713     O          E            STVCFI           63
044300940713     O                       VE(9)               66
044400910322     O                       CAN(1)              33
044500910322     O                       CAN(2)              49
044600910322     O                       CAN(3)              66
044700940713     O          E            STVCFI           64
044800910322     O                       CAN(4)              33
044900910322     O                       CAN(5)              49
045000910322     O                       CAN(6)              66
045100940713     O          E            STVCFI           65
045200910322     O                       CAN(7)              33
045300910322     O                       CAN(8)              49
045400910322     O                       CAN(9)              66
045500940713** RG
045600940713XXXXXXXXXXXXXXXXXXXX/XXX               ***  E L E N C O   F O G L    1
045700940713I   V A R I  ***        FNLR08R        XX.XX.XXXX        PAG. XXXX   2
045800940713xxxxxxxxxx    xxxxxxxxxx                       CATEGORIA FOGLIO: X
045900940713-XXXXXXXXXXXXXXXXXXXX                    xx:xx:Xx                    4
046000960926P.O. IN GESTIONE XXX-XXXXXXXXXXXXXXXXXXX   N.FOGLIO XXXXX   DATA D   5
046100940713AL XX/XX/XXXX  AL XX/XX/XXXX   TIPO FOGLIO X-XXXXXX                  6
046200940713Numero          Data      Descrizione                          Spu   7
046300940713XXXXX XXXXXX  xx/xx/xxxx  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  xxx   8
046400940713ntatore                                                              9
046500940713xxxxxxxxxxxxxxxxx   Annullato                                       10
046600940713------------------------------------------------------------------  11
046700940713------------------------------------------------------- *** FINE S  12
046800940713TAMPA *** --------------------------------------------------------  13
046900940713** VE
047000960926PUNTO OPERATIVO: XXX   ** VERBALE DI XXXXXXXXXX **      XX/XX/XXXX   1
047100940713Foglio     : X-XXXXXXXXXXXXXXXXXXXX   N.ro XXXXX    del XX/XX/XXXX   2
047200940713Descrizione: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                     3
047300940713Ha Spuntato:_____________________________________________________    4
047400940713           SEGNACOLLI  NON  LETTI  DALLA  PISTOLA                    5
047500940713LIN.PARTENZA    LIN.ARRIVO     SERIE     SEGNACOLLO      ANOMALIA    6
047600940713____________   ____________    _____    ____________     ________    7
047700940713FIRMA  __________________________                                    8
047800940713LEGENDA ANOMALIE: X=XXXXXXXXXXXXX X=XXXXXXXXXXXXX  X=XXXXXXXXXXXXX   9
047900940713                                                                    10
048000940713** Comandi    567890123456789012345678901234567890123456789012345678901234567890
048100940713OVRPRTF FILE(SYSPRT)  USRDTA('Fogli Vari')
048200940713** Comandi    567890123456789012345678901234567890123456789012345678901234567890
048300940713OVRPRTF FILE(QSYSPRT) USRDTA('Verbali')
