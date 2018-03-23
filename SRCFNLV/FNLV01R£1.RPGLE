000100941215    ?H* FNLV01R *-----------------------------------------------------*
000200930126    ?H*            PROGRAMMA MANUTENZIONE FILE NUMERATORI             *
000300930126?    H*---------------------------------------------------------------*
000400930120     H DECEDIT('0,') DATEDIT(*DMY.)
000500930120     FFLNUF01L  UF A E           K DISK
000600991228     FFLNUF02L  UF   E           K DISK
000700991228     F                                     RENAME(FLNUF:FLNUF2)
000800900608     FAZORG01L  IF   E           K DISK
000900900608     FTABEL00F  IF   E           K DISK
001000941215     FFNLV01D   CF   E             WORKSTN
001100930120     F                                     INFDS(CA02DS)
001200941215     F                                     SFILE(LV01SFL:NRR)
001300930126?    D*---------------------------------------------------------------*
001400020521     D*PARAM1          DS
001500020521     D* CODUTE                 1      1  0
001600020521     D* RAGUTE                 2     49
001700020521     D* RAG                   50     65
001800020521     D* CLIITA                66     69  0
001900020521     D* CLIEST                70     73  0
002000020521     D* FORITA                74     77  0
002100020521     D* FOREST                78     81  0
002200020521     D* AGEITA                82     85  0
002300020521     D* AGEEST                86     89  0
002400020521     D* CCNIVA                90     93  0
002500020521     D* CCNBAN                94     97  0
002600020521     D* CCNIND                98    101  0
002700000000     D KPJBA         E DS
002800930125     D  PROFIL                 1      3
002900991227     D WLBDAT          DS                  INZ
003000991227     D  G02DAT                 1      8  0
003100991227     D  G02INV                 9     16  0
003200991227     D  G02ERR                17     17
003300991227     D  G02TGI                18     22  0
003400930126     D*------------------
003500930126     D* DS PAGINA DEL RECORD DI SFILE MODIFICATO
003600930126     D*------------------
003700930120     D CA02DS          DS
003800930120     D  PRIMA                378    379B 0
003900991228     D* DS TABELLA 1C - CODICI NUMERATORI
004000930120     D DS1C          E DS
004100020521     D*
004200020521     D tibs34ds      E DS                  inz
004300020521     D ddatiute      e ds
004400020521     D azuteds       e ds                  extname(AZUTE00F)
004500020521     D dLAT          e ds
004600020521     D codut           S                   LIKE(TBLkut) inz(1)
004700991227    ?H*---------------------------------------------------------------*
004800991227    ?H* INDICATORI USATI                                              *
004900991227    ?H*---------------------------------------------------------------*
005000991227     H* 10 - POSIZIONAMENTO CURSORE SU COD.NUMERATORE 1°VIDEATA
005100991228     H* 11/12-ERORRE SU ANNO
005200991227     H* 30 - DI COMODO
005300000113     H* 40/49- ERRORI
005400991228     H* 48 - OF NUOVA IMMISSIONE
005500991228     H* 70 - EMETTE RICHIESTA IMMISSIONE NUOVI NUMERATORI
005600991228     H* 80 - SFLNEXC
005700991228     H* 80 - PULIZIA ED EMISSIONE SFL
005800991228     H* 90 - GENERICO ERRORE O RIEMISSIONE VIDEATA
005900991228     H* 98 - MANUTENZIONE SOLO EDP
006000930125    ?C*---------------------------------------------------------------*
006100930120     C*
006200930120     C* DEFINIZIONI VARIABILI E KLIST
006300930126    ?C                   EXSR      DEFCAM
006400930120     C*
006500991227     C                   Z-ADD     0             V1CANN
006600991227     C                   MOVE      ANNO          V1CANN
006700930122     C                   MOVEL     *ZEROS        VCNUM
006800000113     C                   CLEAR                   VIDFIL
006900000113     C                   CLEAR                   VIDFL2
007000930121     C*
007100930126    ?C     INIZIO        TAG
007200991227     C* EMISSIONE I FORMATO -
007300941215     C                   WRITE     LV01D1
007400930121     C*
007500930126    ?C     FMTD1         TAG
007600941215     C                   EXFMT     LV01D2
007700930120     C*
007800991227     C* F3 - FINE LAVORO
007900991227    ?C   KC              GOTO      FINE
008000930120     C*
008100930120     C* CONTROLLO I FORMATO VIDEO
008200991227     C                   EXSR      CTRD01
008300991227     C* ERRORE
008400991227     C   90
008500991227     CAN 10              GOTO      INIZIO
008600991227     C   90              GOTO      FMTD1
008700991227     C* CARCO SFL
008800991227     C                   EXSR      CARSFL
008900991228     C**
009000991228     C* NESSUN NUMERATORE TROVATO: CHIEDO SE NE VOGLIONO IMMETTERE
009100991228    1C     NRR           IFEQ      0
009200991228     C*
009300991228     C                   SETON                                        70
009400991228     C                   EXFMT     LV01D2
009500991228     C* F3 - FINE LAVORO
009600991228    ?C   KC              GOTO      FINE
009700991228     C*
009800991228    2C     CONF          IFEQ      'NO'
009900991228     C                   SETOFF                                       70
010000991228    ?C                   GOTO      INIZIO
010100991228   X2C                   ELSE
010200991228    ?C                   EXSR      CARVUO
010300991228    2C                   ENDIF
010400991228    1C                   ENDIF
010500930120     C*
010600930120     C* EMMISIONE SUBFILE
010700991228     C                   Z-ADD     NRR           WNRR
010800930120     C                   Z-ADD     1             NRR
010900930125     C                   Z-ADD     1             HSRN
011000930120     C*
011100941215     C                   WRITE     LV01D1
011200941215     C                   WRITE     LV01D5
011300930120     C*
011400930126    ?C     VIDEO         TAG
011500930120     C*
011600930120     C                   SETOFF                                           90
011700941215     C                   EXFMT     LV01CTL
011800930121     C*
011900991228     C* F3 - FINE LAVORO
012000991228    ?C   KC              GOTO      FINE
012100930121     C*
012200991228     C* F12 - RITORNO
012300930125     C   KL              Z-ADD     *ZEROS        NRR
012400930126    ?C   KL              GOTO      INIZIO
012500930121     C*
012600991228     C* F8 - RICHIESTE RIGHE DI IMMISSIONE (N° 17)
012700930121DO  1C   08              DO
012800930125     C     WNRR          ADD       1             HSRN
012900930125     C*
013000930126    ?C                   EXSR      CARVUO
013100930120     C*
013200930126    ?C                   GOTO      VIDEO
013300930121E   1C                   ENDDO
013400930121     C*
013500930120     C* ELABORAZIONE SUBFILE
013600991228     C                   SETOFF                                           90
013700930125     C                   Z-ADD     PRIMA         HSRN
013800930121     C*
013900941215     C                   READC     LV01SFL                                60
014000991228    1C     *IN60         DOWEQ     *OFF
014100930121     C*
014200930125     C* CONTROLLO CAMPI SFILE
014300930126    ?C                   EXSR      CNTSFL
014400930125     C                   SETON                                        80
014500930121     C*
014600991228    2C     HHHNRR        IFNE      0
014700930120     C                   SETON                                        48
014800991228    2C                   ELSE
014900930120     C                   SETOFF                                           48
015000991228    2C                   ENDIF
015100930121     C*
015200941215     C                   UPDATE    LV01SFL
015300930125     C                   SETOFF                                           48
015400991228     C**
015500991228     C                   READC     LV01SFL                                60
015600991228E   1C                   ENDDO
015700930120     C*
015800991228    ?C   90              GOTO      VIDEO
015900930120     C*
016000930125     C* AGGIORNAMENTI RECORD
016100930121IF  1C     *IN06         IFEQ      '1'
016200930126    ?C                   EXSR      AGGIOR
016300930125     C                   Z-ADD     *ZEROS        NRR
016400930126    ?C                   GOTO      INIZIO
016500930121     C*
016600930121X   1C                   ELSE
016700930121     C*
016800930126    ?C                   GOTO      VIDEO
016900930121E   1C                   ENDIF
017000930120     C*
017100930126    ?C     FINE          TAG
017200930120     C                   SETON                                        LR
017300991227    ?C*---------------------------------------------------------------*
017400991227     C* CONTROLLO FORMATO 1
017500991227    ?C*---------------------------------------------------------------*
017600991227     C     CTRD01        BEGSR
017700991227     C                   SETOFF                                       1090
017800991227     C                   CLEAR                   SAVNUM
017900991227     C**
018000991227     C* L'ANNO DEVE ESSERE COMPRESO DA -3 E + 3
018100991227    1C     V1CANN        IFGT      0
018200991227    2C     V1CANN        IFLT      ANNOME
018300991227     C     V1CANN        ORGT      ANNOPI
018400991227     C                   SETON                                        4090
018500991227     C                   GOTO      ENDCT1
018600991227    2C                   ENDIF
018700991227    1C                   ENDIF
018800991227     C* CODICE NUMERATORE
018900991227IF  1C     VCNUM         IFNE      *BLANKS
019000991227     C     VCNUM         ANDNE     *ZEROS
019100991227     C*
019200991227     C* CONTROLLO TIPO NUMERATORE
019300991228     C     '?'           SCAN      VCNUM                                  90
019400991227     C*
019500991227IF  2C     *IN90         IFEQ      *ON
019600991227     C                   SETON                                        10
019700991227     C                   MOVEL     '1C'          COD
019800991227     C                   MOVEL     *BLANKS       KEY
019900991227     C                   MOVEL     *BLANKS       DES
020000991227     C                   CALL      'X§TABER'
020100020521     C                   PARM                    CODUT
020200991227     C                   PARM                    COD               2
020300991227     C                   PARM                    KEY               8
020400991227     C                   PARM                    DES              25
020500991227     C*
020600991227     C* CODICE SELEZIONATO
020700991227IF  3C     KEY           IFNE      *BLANKS
020800991227     C                   MOVEL     KEY           VCNUM
020900991227X   3C                   ELSE
021000991227     C                   MOVEL     *ZEROS        VCNUM
021100991227E   3C                   ENDIF
021200991227    ?C                   GOTO      ENDCT1
021300991227E   2C                   ENDIF
021400991227     C*
021500991227     C* CONTROLLO ESISTENZA CODICE
021600991227     C                   MOVEL     '1C'          CODTAB
021700991227     C                   MOVEL     *BLANKS       KEYTAB
021800991227     C                   MOVEL     VCNUM         KEYTAB
021900991227     C     KTAB          CHAIN     TABEL00F                           30
022000991227IF  2C     *IN30         IFEQ      *ON
022100991227     C     TBLFLG        ORNE      ' '
022200991227     C                   SETON                                        4190
022300991227    ?C                   GOTO      ENDCT1
022400991227E   2C                   ENDIF
022500991227     C* DEVE ESSERE CONGRUENTE CON L'ANNO
022600991227     C                   MOVEL     TBLUNI        DS1C
022700991227    2C     §1CFAA        IFEQ      'S'
022800991227     C     V1CANN        ANDEQ     0
022900991227     C                   SETON                                        4290
023000991227    ?C                   GOTO      ENDCT1
023100991227E   2C                   ENDIF
023200991227     C*
023300991227    2C     §1CFAA        IFEQ      ' '
023400991227     C     V1CANN        ANDGT     0
023500991227     C                   SETON                                        4390
023600991227    ?C                   GOTO      ENDCT1
023700991227E   2C                   ENDIF
023800991227    2C     §1CDAT        IFEQ      'S'
023900991227     C     V1CDAT        ANDEQ     'N'
024000991227     C                   SETON                                        4590
024100991227    ?C                   GOTO      ENDCT1
024200991227E   2C                   ENDIF
024300991227    2C     §1CDAT        IFEQ      ' '
024400991227     C     V1CDAT        ANDEQ     'S'
024500991227     C                   SETON                                        4490
024600991227    ?C                   GOTO      ENDCT1
024700991227E   2C                   ENDIF
024800991228     C                   MOVEL     VCNUM         SAVNUM            3 0
024900991227E   1C                   ENDIF
025000000113     C**
025100000113     C* CONTROLLO I P.O. SE IMMESSI
025200000113     C     VIDFIL        IFGT      0
025300000113     C     VIDFIL        CHAIN     AZORG01L                           30
025400000113     C     *IN30         IFEQ      *ON
025500000113     C     ORGFVA        ORNE      ' '
025600000113     C     ORGFAG        ORNE      'A'
025700000113     C     ORGFAG        ANDNE     'F'
025800000113     C                   SETON                                        4790
025900000113    ?C                   GOTO      ENDCT1
026000000113     C                   ENDIF
026100000113     C                   ENDIF
026200000113     C     VIDFL2        IFGT      0
026300000113     C     VIDFL2        CHAIN     AZORG01L                           30
026400000113     C     *IN30         IFEQ      *ON
026500000113     C     ORGFVA        ORNE      ' '
026600000113     C     ORGFAG        ORNE      'A'
026700000113     C     ORGFAG        ANDNE     'F'
026800000113     C                   SETON                                        4990
026900000113    ?C                   GOTO      ENDCT1
027000000113     C                   ENDIF
027100000113     C                   ENDIF
027200991227     C     ENDCT1        ENDSR
027300991227    ?C*---------------------------------------------------------------*
027400991227     C* CARICO SFL
027500991227    ?C*---------------------------------------------------------------*
027600991227     C     CARSFL        BEGSR
027700991228     C                   EXSR      PULSFL
027800991227     C                   MOVE      V1CANN        KANN
027900991227     C                   MOVE      VCNUM         KNUM
028000991228     C                   MOVEL     *BLANKS       CONF
028100991228     C                   SETOFF                                       70
028200991228     C                   SETON                                        48
028300991227     C**
028400991228    1C     V1CDAT        IFEQ      'S'
028500991228    2C     VCNUM         IFNE      *BLANKS
028600991228     C     VCNUM         ANDNE     *ZEROS
028700991228     C     KNUM          SETLL     FLNUF02L
028800991228     C                   ELSE
028900991228     C     *LOVAL        SETLL     FLNUF02L
029000991228    2C                   ENDIF
029100991228     C**
029200991228     C                   READ      FLNUF02L                               31
029300991228   X1C                   ELSE
029400991228     C**
029500991228    2C     VCNUM         IFNE      *BLANKS
029600991228     C     VCNUM         ANDNE     *ZEROS
029700000113    3C     VIDFIL        IFGT      0
029800000113    4C     VIDFL2        IFGT      0
029900000113     C* MI POSIZIONO PER ANNO, COD NUM, P.O.1 E P.O.2
030000000113     C     KNUF11        SETLL     FLNUF01L
030100000113   X4C                   ELSE
030200000113     C* MI POSIZIONO PER ANNO, COD NUM, P.O.1
030300000113     C     KNUF1         SETLL     FLNUF01L
030400000113    4C                   ENDIF
030500000113   X3C                   ELSE
030600000113     C* MI POSIZIONO PER ANNO, COD NUM
030700991227     C     KNUF          SETLL     FLNUF01L
030800000113    3C                   ENDIF
030900000113     C**
031000991228     C     KANN          READE     FLNUF01L                               31
031100991228   X2C                   ELSE
031200991227     C     KANN          SETLL     FLNUF01L
031300991227     C     KANN          READE     FLNUF01L                               31
031400991228    2C                   ENDIF
031500991228    1C                   ENDIF
031600991227     C**
031700991228    1C     *IN31         DOWEQ     *OFF
031800991228   1AC     NUFAAA        IFEQ      KANN
031900991228     C**
032000991228    2C     SAVNUM        IFNE      NUFCNU
032100991227     C                   MOVEL     '1C'          CODTAB
032200991227     C                   MOVEL     *BLANKS       KEYTAB
032300991228     C                   MOVEL     NUFCNU        KEYTAB
032400991227     C     KTAB          CHAIN     TABEL00F                           30
032500991228     C  N30              MOVEL     TBLUNI        DS1C
032600991228     C   30              CLEAR                   DS1C
032700991228     C                   MOVE      NUFCNU        SAVNUM
032800991228    2C                   ENDIF
032900991228     C**
033000991228     C* SE LE CARATTERISTICHE CORRISPONDO A QUELLE DELLE RICHIESTE
033100991228     C*  CARICO
033200000113    2C                   SELECT
033300991228     C     §1CFAA        WHENEQ    'S'
033400991228     C     V1CANN        ANDEQ     0
033500991228     C     §1CFAA        WHENEQ    ' '
033600991228     C     V1CANN        ANDGT     0
033700991228     C     §1CDAT        WHENEQ    'S'
033800991228     C     V1CDAT        ANDEQ     'N'
033900991228     C     §1CDAT        WHENEQ    ' '
034000991228     C     V1CDAT        ANDEQ     'S'
034100991228     C                   OTHER
034200991228     C**
034300000113     C** SE IMMESSI I P.O. DEVONO CORRISPONDERE
034400000113    3C     VIDFIL        IFEQ      0
034500000113     C     VIDFIL        OREQ      NUFFIL
034600000113     C*
034700000113    4C     VIDFL2        IFEQ      0
034800000113     C     VIDFL2        OREQ      NUFFL2
034900991228     C                   ADD       1             NRR
035000991228     C                   EXSR      CARICA
035100000113    4C                   ENDIF
035200000113    3C                   ENDIF
035300000113     C**
035400000113    2C                   ENDSL
035500991228   1AC                   ENDIF
035600991227     C**
035700991228     C     V1CDAT        IFEQ      'S'
035800991228     C                   READ      FLNUF02L                               31
035900991228     C                   ELSE
036000991227     C     KANN          READE     FLNUF01L                               31
036100991228     C                   ENDIF
036200991228    1C                   ENDDO
036300991227     C**
036400991227     C                   ENDSR
036500991228    ?C*---------------------------------------------------------------*
036600991228     C* PULIZIA SUBFILE
036700991228    ?C*---------------------------------------------------------------*
036800991228     C     PULSFL        BEGSR
036900991228     C                   SETON                                        85
037000991228     C                   WRITE     LV01CTL
037100991228     C                   SETOFF                                       85
037200991228     C                   Z-ADD     0             WNRR
037300991228     C                   ENDSR
037400930125    ?C*---------------------------------------------------------------*
037500930125    ?C*                    CARICA                                     *
037600930125    ?C* ROUTINE DI CARICAMENTO SFILE PER ANNO NUMERATORI ESISTENTE    *
037700930125    ?C*---------------------------------------------------------------*
037800930122     C     CARICA        BEGSR
037900930122     C*
038000930125     C                   SETOFF                                       323398
038100930122     C*
038200930122     C                   MOVE      ' '           VIDANN
038300930122     C                   Z-ADD     NUFNUM        VNUM
038400930125     C                   Z-ADD     NUFFIL        VFIL1
038500930125     C                   Z-ADD     NUFFL2        VFIL2
038600930122     C                   Z-ADD     NUFRIN        VRIN
038700930122     C                   Z-ADD     NUFRFI        VRFI
038800930122     C                   Z-ADD     NUFNUM        HHHNRR
038900991228     C                   Z-ADD     V1CANN        VAAA
039000930125     C* HHHNRR: SONO IN VARIAZIONE O IN INSERIMENTO
039100930122IF  1C     HHHNRR        IFEQ      0
039200930122     C                   MOVE      *HIVAL        HHHNRR
039300930122E   1C                   ENDIF
039400930122     C*
039500930122     C* GIRO LA DATA DOCUMENTO
039600991228     C                   Z-ADD     NUFDAT        HDAT
039700991228     C                   Z-ADD     NUFDAT        VDTP
039800930125     C                   MOVEL     NUFDAT        AA                2 0
039900930125     C                   MOVE      NUFDAT        GG                2 0
040000930125     C                   MOVE      AA            VDTP
040100930125     C                   MOVEL     GG            VDTP
040200930122     C*
040300930125     C* DECODIFICA CODICE NUMERATORE
040400930122     C                   MOVEL     §1CDES        VIDDS1
040500930122     C*
040600930125     C* NUMERATORE MANUTENZIONABILE SOLO DA 'EDP'
040700930122IF  2C     §1CFG1        IFEQ      'S'
040800930125IF  3C     PROFIL        IFNE      'EDP'
040900930122     C                   SETON                                        98
041000930122E   3C                   ENDIF
041100930122E   2C                   ENDIF
041200930122     C*
041300930126     C* SE = 'S' POSSIBILITA' DI INSERIRE CODICI FILIALI INESISTENTI
041400930122IF  2C     §1CFG2        IFNE      'S'
041500930125     C     VFIL1         CHAIN     AZORG01L                           31
041600930122     C                   MOVEL     *BLANKS       VIDDS2
041700930122     C  N31              MOVEL     ORGDES        VIDDS2
041800930122     C   31              MOVEL     *ALL'?'       VIDDS2
041900930122X   2C                   ELSE
042000930122     C                   MOVEL     *BLANKS       VIDDS2
042100930122E   2C                   ENDIF
042200930122     C*
042300930126     C* SE = 'S' CONTROLLO SUBITO CHE I CAMPI SIANO PIENI
042400930122IF  2C     §1CFG3        IFEQ      'S'
042500930122     C     VRFI          COMP      *ZEROS                               3333
042600930122E   2C                   ENDIF
042700930122     C*
042800930122     C                   SETOFF                                           80
042900941215     C                   WRITE     LV01SFL
043000930122     C*
043100930122     C                   ENDSR
043200930125    ?C*---------------------------------------------------------------*
043300930125    ?C*                    CARVUO                                     *
043400930125    ?C* ROUTINE DI CARICAMENTO SFILE PER ANNO NUMERATORI INESISTENTE  *
043500930125    ?C*---------------------------------------------------------------*
043600930122     C     CARVUO        BEGSR
043700930122     C*
043800930126     C                   SETOFF                                       98
043900930126     C*
044000930122     C                   Z-ADD     WNRR          NRR
044100930125     C                   SETOFF                                       4880
044200930122     C*
044300991228DO  1C                   DO        19
044400930122     C                   ADD       1             NRR
044500930125     C                   MOVEL     *BLANKS       VIDANN
044600930122     C                   Z-ADD     *ZEROS        NUFCNU
044700930122     C                   Z-ADD     0             NUFAAA
044800930125     C                   Z-ADD     *ZEROS        VFIL1
044900930125     C                   Z-ADD     *ZEROS        VFIL2
045000930122     C                   Z-ADD     0             VNUM
045100930122     C                   Z-ADD     0             VDTP
045200930122     C                   Z-ADD     0             HHHNRR
045300930122     C                   Z-ADD     0             VRIN
045400930122     C                   Z-ADD     0             VRFI
045500930122     C                   MOVE      *BLANKS       VIDDS1
045600930122     C                   MOVE      *BLANKS       VIDDS2
045700941215     C                   WRITE     LV01SFL
045800930122E   1C                   ENDDO
045900930125     C*
046000930122     C*
046100930122     C                   ENDSR
046200930125    ?C*---------------------------------------------------------------*
046300930125    ?C*                    CNTSFL                                     *
046400930125    ?C* CONTROLLO CAMPI SFILE                                         *
046500930125    ?C*---------------------------------------------------------------*
046600930120     C     CNTSFL        BEGSR
046700991228     C                   SETOFF                                       90
046800991228     C                   CLEAR                   VIDDS2
046900930126     C*
047000940110     C* NON CONTROLLO DATI SUBFILE SE ANNULLO LA RIGA
047100991228    1C     VIDANN        IFEQ      ' '
047200940110     C*
047300930126     C                   MOVEL     '1C'          CODTAB
047400930126     C                   MOVEL     *BLANKS       KEYTAB
047500930126     C                   MOVEL     NUFCNU        KEYTAB
047600991228     C     KTAB          CHAIN     TABEL00F                           30
047700930126     C*
047800991228IF  2C     *IN30         IFEQ      *OFF
047900930126     C                   MOVEL     TBLUNI        DS1C
048000930126     C                   MOVEL     §1CDES        VIDDS1
048100930126     C*
048200991228     C* IMMESSA UNA NUOVA RIGA
048300991228IF  3C     HHHNRR        IFEQ      0
048400991228     C*
048500991228    4C     §1CDAT        IFEQ      ' '
048600991228     C                   MOVE      VAAA          KANN
048700991228     C                   Z-ADD     VFIL1         KFIL1
048800991228     C                   Z-ADD     VFIL2         KFIL2
048900991228     C     KEYNUF        CHAIN     FLNUF01L                           30
049000991228    5C     *IN30         IFNE      '1'
049100991228     C                   SETON                                        9011
049200991228    5C                   ENDIF
049300991228    4C                   ENDIF
049400991228     C*
049500991228     C* CONTROLLO SE IL PROFILO UTENTE CONSENTE DI INSERIRE IL CODICE
049600991228    4C  N90§1CFG1        IFEQ      'S'
049700991228     C     PROFIL        ANDNE     'EDP'
049800991228     C                   SETON                                        9016
049900991228    4C                   ENDIF
050000991228     C*
050100991228    3C                   ENDIF
050200991228     C   90              GOTO      ENDCNT
050300930126     C* IMMETTERE I RANGE SOLO SE PERMESSO
050400991228    3C     §1CFG3        IFEQ      'S'
050500930126     C     VRFI          COMP      *ZEROS                               3333
050600930126     C*
050700930126     C* RANGE A NON < RANGE DA
050800991228    4C     VRFI          IFGT      *ZEROS
050900991228     C     VRFI          ANDLT     VRIN
051000930126     C                   SETON                                          34
051100991228    4C                   ENDIF
051200930126     C   34
051300991228     COR 33              SETON                                        90
051400930126     C*
051500930126     C* NON INSERIRE RANGE DI VALORI PER QUESTO CODICE
051600930126X   3C                   ELSE
051700930126     C*
051800930126IF  4C     VRIN          IFNE      *ZEROS
051900930126     C     VRFI          ORNE      *ZEROS
052000991228     C                   SETON                                        9017
052100991228    4C                   ENDIF
052200991228    3C                   ENDIF
052300991228     C**
052400991228     C* CONTROLLO SE ANNO CONGRUENTE CON COD.NUMERATORE
052500991228     C     VAAA          IFGT      0
052600991228     C     §1CFAA        ANDEQ     ' '
052700991228     C                   SETON                                        9013
052800991228     C                   ENDIF
052900991228     C**
053000991228     C* CONTROLLO SE ANNO CONGRUENTE CON COD.NUMERATORE
053100991228     C     VAAA          IFEQ      0
053200991228     C     §1CFAA        ANDEQ     'S'
053300991228     C                   SETON                                        9013
053400991228     C                   ENDIF
053500991228     C*
053600991228     C     §1CFAA        IFEQ      'S'
053700991228     C     VAAA          IFLT      ANNOME
053800991228     C     VAAA          ORGT      ANNOPI
053900991228     C                   SETON                                        9012
054000991228     C                   ENDIF
054100991228     C                   ENDIF
054200930126     C*
054300930126     C* CODICE NUMERATORE INESISTENTE
054400991228    2C                   ELSE
054500991228     C                   SETON                                        9046
054600991228    2C                   ENDIF
054700991228     C   90              GOTO      ENDCNT
054800930120     C*
054900930126     C* CONTROLLO FILIALI SOLO SE IMMESSE E SE NON E' PERMESSO INSERIRE
055000930126     C* CODICI INESISTENTI
055100991228    2C     §1CFG2        IFNE      'S'
055200930126     C*
055300991228    3C     VFIL1         IFNE      *ZEROS
055400991228     C     VFIL1         CHAIN     AZORG01L                           30
055500991228    4C     *IN30         IFEQ      *ON
055600991228     C     ORGFAG        ORNE      'A'
055700991228     C     ORGFAG        ANDNE     'F'
055800991228     C     ORGFVA        ORNE      ' '
055900991228     C                   SETON                                        9014
056000991228E   4C                   ENDIF
056100991228     C                   MOVEL     ORGDES        VIDDS2
056200991228E   3C                   ENDIF
056300000509E   2C                   ENDIF
056400930125     C*
056500000509    2C     §1CFG5        IFNE      'S'
056600991228IF  3C  N90VFIL2         IFNE      *ZEROS
056700991228     C     VFIL2         CHAIN     AZORG01L                           30
056800991228IF  4C     *IN30         IFEQ      *ON
056900991228     C     ORGFAG        ORNE      'A'
057000991228     C     ORGFAG        ANDNE     'F'
057100991228     C     ORGFVA        ORNE      ' '
057200991228     C                   SETON                                        9015
057300991228E   4C                   ENDIF
057400991228E   3C                   ENDIF
057500991228E   2C                   ENDIF
057600930126     C*
057700930126     C* DATA OBBLIGATORIA
057800991228IF  2C  N90VDTP          IFEQ      *ZEROS
057900991228     C                   SETON                                        9042
058000930126     C*
058100930126     C* DATA FORMALMENTE CORRETTA
058200991228X   2C                   ELSE
058300930126     C                   Z-ADD     VDTP          G02DAT
058400930126     C                   MOVEL     *BLANKS       G02ERR
058500991228     C                   CALL      'XSRDA8'
058600930126     C                   PARM                    WLBDAT
058700930126     C*
058800991228 F  3C     G02ERR        IFEQ      '1'
058900991228     C                   SETON                                        5190
059000991228    3C                   ENDIF
059100991228    2C                   ENDIF
059200991228     C*
059300991228     C* IMMESSA UNA NUOVA RIGA PER NUMERATORE PER DATA
059400991228IF  2C     HHHNRR        IFEQ      0
059500991228     C     §1CDAT        ANDEQ     'S'
059600991228     C                   Z-ADD     G02INV        KDAT
059700991228     C                   MOVEL     NUFCNU        KNUM
059800991228     C     KNUF2         CHAIN     FLNUF02L                           31
059900991228    3C     *IN31         IFEQ      *OFF
060000991228     C                   SETON                                        9011
060100991228    3C                   ENDIF
060200930120     C*
060300991228    2C                   ENDIF
060400991228    1C                   ENDIF
060500940110     C*
060600930126     C* SE ERRORE MI POSIZIONO SULLA PAGINA DEL RECORD SBAGLIATO
060700991228     C     ENDCNT        TAG
060800991228     C   90              Z-ADD     NRR           HSRN
060900930120     C*
061000930120     C                   ENDSR
061100930125    ?C*---------------------------------------------------------------*
061200930125    ?C*                    AGGIOR                                     *
061300930125    ?C* ROUTINE DI GESTIONE AGGIORNAMENTI RECORD FLNUF01L             *
061400930125    ?C*---------------------------------------------------------------*
061500930120     C     AGGIOR        BEGSR
061600930120     C*
061700941215     C                   READC     LV01SFL                                60
061800930120     C*
061900991228    1C     *IN60         DOWEQ     *OFF
062000930120     C*
062100991228    2C     HHHNRR        IFEQ      0
062200991228    3C     VIDANN        IFNE      'A'
062300930126    ?C                   EXSR      WRTREC
062400991228    3C                   ENDIF
062500930121     C*
062600991228   X2C                   ELSE
062700930120     C*
062800991228    3C     VIDANN        IFNE      'A'
062900991228     C                   MOVEL     'U'           WREC              1
063000930126    ?C                   EXSR      UPDREC
063100991228     C                   ELSE
063200991228     C                   MOVEL     'D'           WREC              1
063300991228    ?C                   EXSR      UPDREC
063400991228    3C                   ENDIF
063500991228    2C                   ENDIF
063600930120     C*
063700991228     C                   READC     LV01SFL                                60
063800991228    1C                   ENDDO
063900930120     C*
064000930120     C                   ENDSR
064100930125    ?C*---------------------------------------------------------------*
064200930125    ?C*                    WRTREC                                     *
064300930125    ?C* ROUTINE DI INSERIMENTO RECORD FLNUF01L                        *
064400930125    ?C*---------------------------------------------------------------*
064500930120     C     WRTREC        BEGSR
064600991228     C                   EXSR      CARREC
064700930120     C*
064800930120     C                   WRITE     FLNUF
064900930120     C*
065000930120     C                   ENDSR
065100991228    ?C*---------------------------------------------------------------*
065200991228     C* CQRICO DATI DEL SFL NEL RECORD DI FLNUF
065300991228    ?C*---------------------------------------------------------------*
065400991228     C     CARREC        BEGSR
065500991228     C* GIRO DATA IN A/M/G
065600991228     C                   Z-ADD     VDTP          NUFDAT
065700991228     C                   MOVEL     NUFDAT        GG
065800991228     C                   MOVE      NUFDAT        AA
065900991228     C                   MOVE      GG            NUFDAT
066000991228     C                   MOVEL     AA            NUFDAT
066100991228     C*
066200991228     C                   Z-ADD     VFIL1         NUFFIL
066300991228     C                   Z-ADD     VFIL2         NUFFL2
066400991228     C                   Z-ADD     VNUM          NUFNUM
066500991228     C                   Z-ADD     VRIN          NUFRIN
066600991228     C                   Z-ADD     VRFI          NUFRFI
066700991228     C                   MOVE      VAAA          NUFAAA
066800991228     C*
066900991228     C                   ENDSR
067000930125    ?C*---------------------------------------------------------------*
067100930125    ?C*                    UPDREC                                     *
067200930125    ?C* ROUTINE DI AGGIORNAMENTO RECORD FLNUF01L                      *
067300930125    ?C*---------------------------------------------------------------*
067400930120     C     UPDREC        BEGSR
067500991228     C**
067600991228     C     SAVNUM        IFNE      NUFNUM
067700991228     C                   MOVEL     '1C'          CODTAB
067800991228     C                   MOVEL     *BLANKS       KEYTAB
067900991228     C                   MOVEL     NUFCNU        KEYTAB
068000991228     C     KTAB          CHAIN     TABEL00F                           30
068100991228     C*
068200991228     C  N30              MOVEL     TBLUNI        DS1C
068300991228     C   30              CLEAR                   DS1C
068400991228     C                   MOVEL     NUFNUM        SAVNUM
068500991228     C                   ENDIF
068600930120     C*
068700991228     C                   MOVE      VAAA          KANN
068800991228     C                   Z-ADD     VFIL1         KFIL1
068900991228     C                   Z-ADD     VFIL2         KFIL2
069000991228     C     §1CDAT        IFEQ      ' '
069100991228     C     KEYNUF        CHAIN     FLNUF01L                           95
069200991228     C                   ELSE
069300991228     C                   MOVEL     HDAT          KDAT
069400991228     C     KNUF22        CHAIN     FLNUF02L                           95
069500991228     C                   ENDIF
069600930121     C*
069700991228     C     WREC          IFEQ      'U'
069800991228     C                   EXSR      CARREC
069900991228     C  N95§1CDAT        IFEQ      ' '
070000991228     C                   EXCEPT    UFLNUF
070100991228     C                   ELSE
070200991228     C                   EXCEPT    UFLNU2
070300991228     C                   ENDIF
070400991228     C**
070500991228     C   95              WRITE     FLNUF
070600991228     C                   ENDIF
070700991228     C*
070800991228     C  N95WREC          IFEQ      'D'
070900991228     C     §1CDAT        IFEQ      ' '
071000991228     C                   DELETE    FLNUF
071100991228     C                   ELSE
071200991228     C                   DELETE    FLNUF2
071300991228     C                   ENDIF
071400991228     C                   ENDIF
071500930120     C*
071600930120     C                   ENDSR
071700930125    ?C*---------------------------------------------------------------*
071800930125    ?C*                    DEFCAM                                     *
071900930125    ?C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
072000930125    ?C*---------------------------------------------------------------*
072100930121     C     DEFCAM        BEGSR
072200991227     C     *LIKE         DEFINE    NUFAAA        KANN
072300930121     C*
072400930121     C     *ENTRY        PLIST
072500930121     C                   PARM                    KPJBA
072600930121     C*
072700020521     C*                  MOVE      *ZEROS        PARAM1
072800020521     C*                  Z-ADD     1             CODUT             1 0
072900020521     C*                  Z-ADD     1             CODUTE
073000020521     C*                  CALL      'XPARCCC'
073100020521     C*                  PARM                    PARAM1
073200020521     C*                  MOVEL     RAGUTE        RSUT
073300020521     C*                  MOVEL     RAGUTE        PARDUT           30
073400020521     c*
073500020521     c     *dtaara       define    §azute        azuteds
073600020521     c     *dtaara       define    §datiute      ddatiute
073700020521     C                   in(E)     *dtaara
073800020521     C                   IF        %error  or RSUT = *blanks
073900020521     C                   CLEAR                   tibs34ds
074000020521     C                   CALL      'TIBS34R'
074100020521     C                   PARM                    tibs34ds
074200020521     C                   in        *dtaara
074300020521     c                   endif
074400020521     C                   MOVEL     RSUT          PARDUT           30
074500930121     C*
074600991227     C* LETTURA FLNUF01L PER L'INTERA CHIAVE
074700930121     C     KEYNUF        KLIST
074800991228     C                   KFLD                    KANN
074900930125     C                   KFLD                    NUFCNU
075000930125     C                   KFLD                    KFIL1             3 0
075100930125     C                   KFLD                    KFIL2             3 0
075200930121     C*
075300930122     C*
075400930122     C* LETTURA FLNUF01L PER ANNO E CODICE NUMERATORE
075500991227     C     KNUF          KLIST
075600991227     C                   KFLD                    KANN
075700991227     C                   KFLD                    KNUM              3 0
075800000113     C     KNUF1         KLIST
075900000113     C                   KFLD                    KANN
076000000113     C                   KFLD                    KNUM              3 0
076100000113     C                   KFLD                    VIDFIL
076200000113     C     KNUF11        KLIST
076300000113     C                   KFLD                    KANN
076400000113     C                   KFLD                    KNUM              3 0
076500000113     C                   KFLD                    VIDFIL
076600000113     C                   KFLD                    VIDFL2
076700991228     C     KNUF2         KLIST
076800991228     C                   KFLD                    KNUM
076900991228     C                   KFLD                    KDAT              6 0
077000991228     C     KNUF22        KLIST
077100991228     C                   KFLD                    NUFCNU
077200991228     C                   KFLD                    KDAT              6 0
077300991228     C                   KFLD                    KFIL1             3 0
077400991228     C                   KFLD                    KFIL2             3 0
077500930121     C*
077600930121     C* LETTURA TABEL00F
077700930121     C     KTAB          KLIST
077800930121     C                   KFLD                    CODUT
077900930121     C                   KFLD                    CODTAB            2
078000930121     C                   KFLD                    KEYTAB            8
078100930121     C*
078200930121     C                   Z-ADD     0             NRR               4 0
078300930121     C                   Z-ADD     0             WNRR              4 0
078400930121     C                   Z-ADD     0             XDTGMA            6 0
078500991227     C**
078600991227     C                   TIME                    W0140            14 0
078700991227     C* UDATE IN GGMMAAAA
078800991227     C                   MOVE      W0140         WDTGIO            8 0
078900991227     C*
079000991227     C* UDATE IN AAAAMMGG
079100991227     C                   Z-ADD     WDTGIO        G02DAT
079200991227     C                   MOVEL     *BLANK        G02ERR
079300991227     C                   CALL      'XSRDA8'
079400991227     C                   PARM                    WLBDAT
079500991227     C                   MOVEL     G02INV        DATEU             8 0
079600991227     C                   MOVEL     G02INV        ANNO              4 0
079700991227     C     ANNO          SUB       3             ANNOME            4 0
079800991227     C     ANNO          ADD       3             ANNOPI            4 0
079900930121     C*
080000930121     C                   ENDSR
080100930125    ?C*---------------------------------------------------------------*
080200900607     OFLNUF     E            UFLNUF
080300900607     O                       NUFDAT
080400900607     O                       NUFNUM
080500910220     O                       NUFRIN
080600910220     O                       NUFRFI
080700991228     OFLNUF2    E            UFLNU2
080800991228     O                       NUFDAT
080900991228     O                       NUFNUM
081000991228     O                       NUFRIN
081100991228     O                       NUFRFI
