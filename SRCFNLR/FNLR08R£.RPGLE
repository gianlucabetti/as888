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
003700940713     D DS7N          E DS
003800050512     D DFVVSPN       E DS
003900940713     D KPJBA         E DS
004000900604     D CNCR80        E DS
004100900601     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004200940713     D***
004300940713     D* COSTANTI
004400940713     D***
004500940713     D CANNUL          C                   CONST('ANNULLATO')
004600940713     D CBLOCC          C                   CONST('BLOCCATO ')
004700940713     D CCHIUS          C                   CONST('CHIUSO   ')
004800900625      *---------------------------------------------------------------*
004900900625      * INDICATORI USATI
005000940713      *---------------------------------------------------------------*
005100910322      * 02    = DATA DAL AL IMMESSA
005200940713      * 11    = ON -  RICHIESTO UNO SPECIFICO FOGLIO
005300940713      * 30    = LETTURA
005400940713      * 31    = COMODO
005500940713      * 90    = ERRORE GENERICO
005600000000     C*---------------------------------------------------------------*
005700000000     C     *ENTRY        PLIST
005800000000     C                   PARM                    KPJBA
005900940802     C                   MOVEL     KPJBU         DSLR01
006000000000     C                   Z-ADD     1             CODUT
006100900601     C                   CALL      'X§PARUT'
006200900601     C                   PARM                    UT§DSE
006300940713     C                   MOVEL     RAGUT         P0CDSA           20
006400900601     C                   MOVEL     REC80         CNCR80
006500900511     C*---------------------------------------------------------------*
006600940713     C* SE HA DATO ERROE IL QCMDEXC SUL FILE PRINTER ESCO DAL PGM
006700940713    0C     *IN90         IFEQ      *OFF
006800940713     C*
006900900601     C* CONTROLLO RICHIESTE FATTE
007000940713     C                   EXSR      CTRPAR
007100900523     C*
007200940713     C* NON E' STATA SCELTO UNO SPECIFICO FOGLIO
007300940713    1C     *IN11         IFEQ      *OFF
007400940713     C*
007500940713     C     KFVV          SETLL     FNFVV000
007600940713     C  N02KFVV          READE     FNFVV000                               30
007700020905     C*!!02DLRNPG        READE     FNFVV000                               30
007800020905     C   02Kfvv1         READE     FNFVV000                               30
007900940713     C*
008000940713   X1C                   ELSE
008100940713     C*
008200940713     C* SE IMMESSO UN FOGLIO FACCIO CHAIN
008300940713     C     KFVV2         CHAIN     FNFVV001                           30
008400940713    1C                   ENDIF
008500940713     C*
008600940713    1C     *IN30         DOWEQ     *OFF
008700940713     C                   CLEAR                   W001A
008800940713     C*
008900900601     C* CONTROLLO FINE LETTURA SE IMMESSA DATA AL
009000940713    2C     *IN02         IFEQ      *ON
009100940802     C     FVVDFV        ANDGT     DLRAAL
009200941027     C*
009300900601     C                   SETON                                        30
009400940713     C*
009500940713   X2C                   ELSE
009600941027     C*
009700941027     C* SE LA FIL IN GESTIONE NON E' QUELLA CHE VOGLIO LA SALTO
009800941027    3C     FVVFGS        IFEQ      DLRFGS
009900910322     C*
010000940713     C* STAMPA ELENCO FOGLI
010100940713     C*                       W001A = N --> DA NON STAMPARE
010200941027    4C     DLRFS1        IFEQ      'S'
010300940713     C     *IN11         ANDEQ     *OFF
010400940713     C*
010500940713     C* SCELTI SOLO I FV CHIUSI
010600940802     C     DLRTFV        IFEQ      'C'
010700940713     C*
010800941027    5C     FVVATB        ANDEQ     *BLANKS
010900940713     C     FVVFCF        ANDNE     'S'
011000940713     C                   MOVEL     'N'           W001A             1
011100941027    5C                   ENDIF
011200940713     C*
011300940713     C* SCELTI SOLO I FV APERTI
011400940802     C     DLRTFV        IFEQ      'A'
011500940713     C*
011600941027    5C     FVVFCF        ANDEQ     'S'
011700940713     C                   MOVEL     'N'           W001A
011800941027    5C                   ENDIF
011900941027     C*
012000941027    4C                   ENDIF
012100911009     C*
012200940713     C* S T A M P O   E L E N C O   F O G L I
012300941027    4C     DLRFS1        IFEQ      'S'
012400940713     C     W001A         ANDEQ     ' '
012500940713     C                   EXSR      DECOD
012600940713     C                   EXSR      STAELE
012700941027    4C                   ENDIF
012800900601     C*
012900940713     C* S T A M P O   V E R B A L E
013000940713     C* SOLO SE RICHISTO X FOGLI APERTI E NON ANNULLATI
013100941027    4C     DLRFS2        IFEQ      'S'
013200940713     C     W001A         ANDEQ     ' '
013300940713     C     FVVATB        ANDEQ     ' '
013400940713     C     FVVFCF        ANDNE     'S'
013500940713     C* DECODIFICHE
013600940713     C                   EXSR      DECOD
013700940713     C* PRIMA PARTE
013800910322     C                   EXCEPT    STVC
013900940713     C* RIGHE DA SCRIVERE
014000940713     C                   DO        23
014100940713     C                   EXCEPT    STVCRG
014200940713     C                   ENDDO
014300940713     C* PARTE FINALE
014400940713     C                   EXCEPT    STVCFI
014500941027    4C                   ENDIF
014600941027    3C                   ENDIF
014700940713     C*
014800940713     C* RILETTURA
014900940713    3C     *IN11         IFEQ      *OFF
015000940713     C  N02KFVV          READE     FNFVV000                               30
015100020905     C*!!02DLRNPG        READE     FNFVV000                               30
015200020905     c     Kfvv1         READE     FNFVV000                               30
015300940713   X3C                   ELSE
015400940713     C                   SETON                                        30
015500940713    3C                   ENDIF
015600940713    2C                   ENDIF
015700940713     C*
015800940713    1C                   ENDDO
015900940713     C*
016000940713    0C                   ENDIF
016100940713     C*
016200940713     C*  E L E N C O   F O G L I  --> RIGA FINALE
016300940802    3C     DLRFS1        IFEQ      'S'
016400940713     C                   EXCEPT    FONFV
016500940713    3C                   ENDIF
016600900601     C*
016700000000     C                   SETON                                        LR
016800940713     C*----------------------------------------------------------------
016900940713     C*--- CONTROLLI PARAMETRI PASSATI
017000940713     C*----------------------------------------------------------------
017100940713     C     CTRPAR        BEGSR
017200911009     C*
017300940713     C* 11 ON - IMMESSO NUMERO FOGLIO
017400940802     C     DLRNFV        IFNE      0
017500900608     C                   SETON                                        11
017600940715     C*
017700940802     C                   CLEAR                   DLRTFV
017800900608     C                   ELSE
017900940713     C*
018000940713     C* 02 ON - IMMESSA DATA AL
018100940802     C     DLRGAL        IFNE      0
018200900601     C                   SETON                                        02
018300940713     C                   ENDIF
018400940713     C*
018500940713     C* DECODIFICO TIPO FOGLIO
018600940714     C                   SELECT
018700940802     C     DLRTFV        WHENEQ    'C'
018800940713     C                   MOVEL     'CHIUSO'      P0DTFV            6
018900940802     C     DLRTFV        WHENEQ    'A'
019000940713     C                   MOVEL     'APERTO'      P0DTFV
019100940714     C*
019200940714     C                   OTHER
019300940714     C                   MOVEL     'TUTTI '      P0DTFV
019400940714     C                   ENDSL
019500940715     C                   ENDIF
019600911009     C*
019700940713     C* DECODIFICO CATEGORIA FOGLI
019800920807     C                   MOVEL     '7N'          COD
019900920807     C                   MOVEL     *BLANKS       KEY
020000940802     C                   MOVEL     DLRNPG        KEY
020100920807     C     KTAB2         CHAIN     TABEL                              30
020200940713     C     *IN30         IFEQ      *OFF
020300940713     C  N30              MOVEL     TBLUNI        DS7N
020400940713     C  N30              MOVEL     §7NDSV        P0CDSV
020500940713     C  N30              MOVEL     §7NDES        P0DNPG           20
020600940713     C                   ENDIF
020700920807     C*
020800940713     C* DECODIFICO FILIALE GESTIONE
020900940802     C     DLRFGS        CHAIN     AZORG                              30
021000940713     C  N30              MOVEL     ORGDES        P0DFGS           20
021100940713     C   30              MOVEL     *BLANKS       P0DFGS
021200940713     C***
021300910322     C* CARICO CODICI ANOMALIE
021400940713     C***
021500940713     C* SOLO SE STAMPA VERBALI
021600940802     C     DLRFS2        IFEQ      'S'
021700940713     C*
021800910322     C                   MOVEL     '3E'          COD
021900910322     C                   Z-ADD     1             I
022000910322     C                   MOVEL     '='           UGUA
022100910322     C*
022200910322     C                   SETOFF                                       30
022300910322     C     KTAB          SETLL     TABEL
022400910322     C     *IN30         DOWEQ     '0'
022500910322     C     KTAB          READE     TABEL                                  30
022600910322     C  N30TBLFLG        IFEQ      *BLANKS
022700910322     C                   MOVEL     TBLKEY        FL3E
022800910322     C                   MOVEL     TBLUNI        DE3E
022900910322     C                   MOVEL     COMCAN        CAN(I)
023000910322     C                   ADD       1             I                 2 0
023100910322     C* SE GIA' TROVATI 9 ESCO DAL CARICAMENTO
023200910322     C     I             IFGT      9
023300910322     C                   SETON                                        30
023400910322     C                   END
023500910322     C                   END
023600910322     C*
023700910322     C                   END
023800940713     C                   END
023900940713     C***
024000940713     C* DATA E ORA DEL GIORNO
024100940713     C***
024200940713     C                   TIME                    W0120            12 0
024300940713     C                   MOVEL     W0120         P0CTIM            6 0
024400940713     C                   MOVE      W0120         W0060             6 0
024500940713     C* DATA A 8
024600940713     C                   MOVE      *ZEROS        G02INV
024700940713     C                   MOVE      W0060         G02DAT
024800940713     C                   MOVEL     ' '           G02ERR
024900940713     C                   CALL      'XSRDA8'
025000940713     C                   PARM                    WLBDAT
025100940713     C                   Z-ADD     G02DAT        P0CDAT
025200900601     C                   ENDSR
025300940713     C*----------------------------------------------------------------
025400940713     C* DECODIFICHE
025500940713     C*----------------------------------------------------------------
025600940713     C     DECOD         BEGSR
025700940713     C* DECODIFICA TIPO INVENTARIO
025800940713     C     FVVSPG        IFNE      ' '
025900940713     C                   MOVEL     '7B'          COD
026000940713     C                   MOVEL     *BLANKS       KEY
026100940713     C                   MOVEL     FVVSPG        KEY
026200940713     C     KTAB2         CHAIN     TABEL                              30
026300940713     C  N30              MOVEL     TBLUNI        P0DSPG            6
026400940713     C                   END
026500940713     C*
026600940713     C* DATA FOGLIO
026700940713     C                   MOVE      FVVDFV        G02INV
026800940713     C                   MOVE      *ZEROS        G02DAT
026900940713     C                   MOVEL     3             G02ERR
027000940713     C                   CALL      'XSRDA8'
027100940713     C                   PARM                    WLBDAT
027200940713     C                   Z-ADD     G02DAT        P0CDFV
027300050512     c* spuntatore
027400050512     c                   movel     fvvspn        dfvvspn
027500940713     C                   ENDSR
027600940713     C*----------------------------------------------------------------
027700940713     C*--- STAMPO ELENCO FOGLI
027800940713     C*----------------------------------------------------------------
027900940713     C     STAELE        BEGSR
028000940713     C                   CLEAR                   P0CCHI
028100910322     C* CHIUSO
028200940713     C     FVVFCF        IFEQ      'S'
028300940713     C                   MOVEL     CCHIUS        P0CCHI
028400940713     C                   ENDIF
028500940713     C* BLOCCATO
028600940713     C     FVVFCF        IFNE      ' '
028700940713     C     FVVFCF        ANDNE     'S'
028800940713     C                   MOVEL     CBLOCC        P0CCHI
028900940713     C                   ENDIF
029000940713     C*  ANNULLATO
029100940713     C     FVVATB        IFEQ      '*'
029200940713     C                   MOVEL     CANNUL        P0CCHI            9
029300940713     C                   ENDIF
029400910322     C* TESTATA
029500900914     C   OF              DO
029600900914     C                   EXCEPT    TESTA
029700900914     C                   SETOFF                                       OF
029800900914     C                   END
029900910322     C*
030000940713     C* DETTAGLIO FOGLIO
030100910322     C                   EXCEPT    STFV
030200900601     C*
030300900601     C                   ENDSR
030400940713      *----------------------------------------------------------------
030500940713      * INZSR INIZIALE
030600940713      *----------------------------------------------------------------
030700940713     C     *INZSR        BEGSR
030800940713      ***
030900940713      * CHIAVI
031000940713      ***
031100940713     C     KFVV          KLIST
031200020905     C                   KFLD                    DLRFGS
031300940802     C                   KFLD                    DLRNPG
031400940802     C                   KFLD                    DLRADA
031500020905     c     Kfvv1         Klist
031600020905     c                   Kfld                    DlrFgs
031700020905     c                   Kfld                    DlrNpg
031800940713     C     KFVV2         KLIST
031900940802     C                   KFLD                    DLRNPG
032000940802     C                   KFLD                    DLRNFV
032100941027     C                   KFLD                    DLRFGS
032200940713     C     KTAB          KLIST
032300940713     C                   KFLD                    CODUT
032400940713     C                   KFLD                    COD               2
032500940713     C     KTAB2         KLIST
032600940713     C                   KFLD                    CODUT
032700940713     C                   KFLD                    COD               2
032800940713     C                   KFLD                    KEY               8
032900940713     C***
033000940713     C* LIKE DEFN
033100940713     C***
033200940713     C     *LIKE         DEFINE    FVVDFV        P0CDFV
033300940713     C     *LIKE         DEFINE    FVVDFV        P0CDAT
033400940713     C     *LIKE         DEFINE    §7NDSV        P0CDSV
033500940713     C***
033600940713     C* INDICATORE DI ECCEDENZA RIGA IN STAMPA
033700940713     C***
033800020906     C**!!!              SETON                                        OF
033900940713     C***
034000940713     c* Eseguo OVRPRTF sul file di stampa
034100940713     C***
034200940713     C                   Z-ADD     42            LUNG             15 5
034300940713     C                   MOVEL     *BLANKS       COMMAN           80
034400940713     C                   MOVEA     CM1           COMMAN
034500940713     C                   CALL      'QCMDEXC'                            90      ERRORE=USCITA
034600940713     C                   PARM                    COMMAN
034700940713     C                   PARM                    LUNG
034800940713     C                   OPEN      SYSPRT                               90      PRINTER FILE
034900020906
035000020906      * Convertendo il pgm in ile l'indicatore OF dopo la open del file risulta spento
035100020906      * così ho spostato l'istruzione
035200020906     C                   SETON                                        OF
035300940713     C*
035400940713     C                   MOVEL     *BLANKS       COMMAN           80
035500940713     C                   MOVEA     CM2           COMMAN
035600940713     C                   CALL      'QCMDEXC'                            90      ERRORE=USCITA
035700940713     C                   PARM                    COMMAN
035800940713     C                   PARM                    LUNG
035900940713     C                   OPEN      QSYSPRT                              90      PRINTER FILE
036000940713     C                   ENDSR
036100900606     C*---------------------------------------------------------------*
036200940714     O***
036300940714     O* ELENCO FOGLI VARI
036400940714     O***
036500940713     OSYSPRT    E            TESTA            02
036600940713     O                       RG(1)               66
036700940713     O                       RG(2)              132
036800940713     O                       P0CDSA              20
036900911009     O                                           21 '/'
037000911009     O                       SIMFEL              24
037100940713     O                       P0CDAT             115 '  .  .    '
037200910322     O                       PAGE          Z    132
037300940713     O          E            TESTA            03
037400940713     O                       RG(3)               66
037500940713     O                       RG(4)              132
037600940713     O                       KNSIF               10
037700940713     O                       KNMUS               24
037800940802     O                       DLRNPG              66
037900940713     O                       P0DNPG              87
038000940713     O                       P0CTIM             115 '  :  :  '
038100940713     O          E            TESTA            05
038200940713     O                       RG(5)               66
038300940713     O                       RG(6)              132
038400940802     O                       DLRFGS              20
038500940713     O                       P0DFGS              41
038600940802     O                       DLRNFV        Z     57
038700940802     O                       DLRGDA              79 '  /  /    '
038800940802     O                       DLRGAL              94 '  /  /    '
038900940802     O                       DLRTFV             110
039000940713     O                       P0DTFV             117
039100940713     O          E            TESTA            07
039200940713     O                       RG(11)              66
039300940713     O                       RG(11)             132
039400940713     O          E            TESTA            08
039500940713     O                       RG(7)               66
039600940713     O                       RG(9)              132
039700940714     O          E            TESTA            09
039800940713     O                       RG(11)              66
039900940713     O                       RG(11)             132
040000940713     O          E            STFV        1
040100940713     O                       FVVNFV        Z
040200940713     O                       P0DSPG            +  1
040300940713     O                       P0CDFV            +  2 '  /  /    '
040400940713     O                       FVVDSF            +  2
040500050512     O                       §fvvspn           +  2
040600940713     O                       P0CCHI            +  5
040700940713     O          E            FONFV       2
040800940713     O                       RG(12)              66
040900940713     O                       RG(13)             132
041000940714     O***
041100940714     O* STAMPA VERBALI
041200940714     O***
041300900628     OQSYSPRT   E            STVC             02
041400900628     O                       VE(1)               66
041500960926     O                       DLRFGS              20
041600940713     O                       P0CDSV              47
041700940713     O                       P0CDAT              66 '  /  /    '
041800940713     O          E            STVC             03
041900940713     O                       P0CTIM              66 '  :  :  '
042000910322     O          E            STVC             04
042100900628     O                       VE(2)               66
042200940802     O                       DLRNPG              14
042300940713     O                       P0DNPG              35
042400940713     O                       FVVNFV        Z     48
042500940713     O                       P0CDFV              66 '  /  /    '
042600910322     O          E            STVC             06
042700910322     O                       VE(3)               66
042800940713     O                       FVVDSF              48
042900910322     O          E            STVC             08
043000910322     O                       VE(4)               66
043100940713     O          E            STVC             10
043200910322     O                       VE(5)               66
043300940713     O          E            STVC             12
043400940713     O                       VE(6)               66
043500940713     O          E            STVCRG      2
043600940713     O                       VE(7)               66
043700940713     O          E            STVCFI           61
043800940713     O                       VE(8)               66
043900940713     O          E            STVCFI           63
044000940713     O                       VE(9)               66
044100910322     O                       CAN(1)              33
044200910322     O                       CAN(2)              49
044300910322     O                       CAN(3)              66
044400940713     O          E            STVCFI           64
044500910322     O                       CAN(4)              33
044600910322     O                       CAN(5)              49
044700910322     O                       CAN(6)              66
044800940713     O          E            STVCFI           65
044900910322     O                       CAN(7)              33
045000910322     O                       CAN(8)              49
045100910322     O                       CAN(9)              66
045200940713** RG
045300940713XXXXXXXXXXXXXXXXXXXX/XXX               ***  E L E N C O   F O G L    1
045400940713I   V A R I  ***        FNLR08R        XX.XX.XXXX        PAG. XXXX   2
045500940713xxxxxxxxxx    xxxxxxxxxx                       CATEGORIA FOGLIO: X
045600940713-XXXXXXXXXXXXXXXXXXXX                    xx:xx:Xx                    4
045700960926P.O. IN GESTIONE XXX-XXXXXXXXXXXXXXXXXXX   N.FOGLIO XXXXX   DATA D   5
045800940713AL XX/XX/XXXX  AL XX/XX/XXXX   TIPO FOGLIO X-XXXXXX                  6
045900940713Numero          Data      Descrizione                          Spu   7
046000940713XXXXX XXXXXX  xx/xx/xxxx  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  xxx   8
046100940713ntatore                                                              9
046200940713xxxxxxxxxxxxxxxxx   Annullato                                       10
046300940713------------------------------------------------------------------  11
046400940713------------------------------------------------------- *** FINE S  12
046500940713TAMPA *** --------------------------------------------------------  13
046600940713** VE
046700960926PUNTO OPERATIVO: XXX   ** VERBALE DI XXXXXXXXXX **      XX/XX/XXXX   1
046800940713Foglio     : X-XXXXXXXXXXXXXXXXXXXX   N.ro XXXXX    del XX/XX/XXXX   2
046900940713Descrizione: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                     3
047000940713Ha Spuntato:_____________________________________________________    4
047100940713           SEGNACOLLI  NON  LETTI  DALLA  PISTOLA                    5
047200940713LIN.PARTENZA    LIN.ARRIVO     SERIE     SEGNACOLLO      ANOMALIA    6
047300940713____________   ____________    _____    ____________     ________    7
047400940713FIRMA  __________________________                                    8
047500940713LEGENDA ANOMALIE: X=XXXXXXXXXXXXX X=XXXXXXXXXXXXX  X=XXXXXXXXXXXXX   9
047600940713                                                                    10
047700940713** Comandi    567890123456789012345678901234567890123456789012345678901234567890
047800940713OVRPRTF FILE(SYSPRT)  USRDTA('Fogli Vari')
047900940713** Comandi    567890123456789012345678901234567890123456789012345678901234567890
048000940713OVRPRTF FILE(QSYSPRT) USRDTA('Verbali')
