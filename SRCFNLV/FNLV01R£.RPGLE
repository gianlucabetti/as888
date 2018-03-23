000100941215    ?H* FNLV01R *-----------------------------------------------------*
000200930126    ?H*            PROGRAMMA MANUTENZIONE FILE NUMERATORI             *
000300930126?    H*---------------------------------------------------------------*
000400930120     H DECEDIT('0,') DATEDIT(*DMY.)
000500061204     FFLNUF01L  UF A E           K DISK    usropn
000600061204     FFLNUF02L  UF   E           K DISK    usropn
000700991228     F                                     RENAME(FLNUF:FLNUF2)
000800900608     FAZORG01L  IF   E           K DISK
000900900608     FTABEL00F  IF   E           K DISK
001000941215     FFNLV01D   CF   E             WORKSTN
001100930120     F                                     INFDS(CA02DS)
001200941215     F                                     SFILE(LV01SFL:NRR)
001300930126?    D*---------------------------------------------------------------*
001400061204     d cmdt            s             60    dim(02) ctdata perrcd(1)
001500061204     d msg             s             78    dim(01) ctdata perrcd(1)
001600061204     d
001700000000     D KPJBA         E DS
001800930125     D  PROFIL                 1      3
001900991227     D WLBDAT          DS                  INZ
002000991227     D  G02DAT                 1      8  0
002100991227     D  G02INV                 9     16  0
002200991227     D  G02ERR                17     17
002300991227     D  G02TGI                18     22  0
002400930126     D*------------------
002500930126     D* DS PAGINA DEL RECORD DI SFILE MODIFICATO
002600930126     D*------------------
002700930120     D CA02DS          DS
002800930120     D  PRIMA                378    379B 0
002900991228     D* DS TABELLA 1C - CODICI NUMERATORI
003000930120     D DS1C          E DS
003100020521     D*
003200061204     D tibs55ds      E DS                  inz
003300061204
003400061204     d comman          s             80
003500061204     d lenght          s             15  5 inz(80)
003600061204     D tibs34ds      E DS                  inz
003700020521     D ddatiute      e ds
003800020521     D azuteds       e ds                  extname(AZUTE00F)
003900020521     D dLAT          e ds
004000020521     D codut           S                   LIKE(TBLkut) inz(1)
004100991227    ?H*---------------------------------------------------------------*
004200991227    ?H* INDICATORI USATI                                              *
004300991227    ?H*---------------------------------------------------------------*
004400991227     H* 10 - POSIZIONAMENTO CURSORE SU COD.NUMERATORE 1°VIDEATA
004500991228     H* 11/12-ERORRE SU ANNO
004600991227     H* 30 - DI COMODO
004700000113     H* 40/49- ERRORI
004800991228     H* 48 - OF NUOVA IMMISSIONE
004900991228     H* 70 - EMETTE RICHIESTA IMMISSIONE NUOVI NUMERATORI
005000991228     H* 80 - SFLNEXC
005100991228     H* 80 - PULIZIA ED EMISSIONE SFL
005200991228     H* 90 - GENERICO ERRORE O RIEMISSIONE VIDEATA
005300991228     H* 98 - MANUTENZIONE SOLO EDP
005400930125    ?C*---------------------------------------------------------------*
005500930120     C*
005600930120     C* DEFINIZIONI VARIABILI E KLIST
005700930126    ?C                   EXSR      DEFCAM
005800930120     C*
005900991227     C                   Z-ADD     0             V1CANN
006000991227     C                   MOVE      ANNO          V1CANN
006100930122     C                   MOVEL     *ZEROS        VCNUM
006200000113     C                   CLEAR                   VIDFIL
006300000113     C                   CLEAR                   VIDFL2
006400930121     C*
006500930126    ?C     INIZIO        TAG
006600991227     C* EMISSIONE I FORMATO -
006700941215     C                   WRITE     LV01D1
006800930121     C*
006900930126    ?C     FMTD1         TAG
007000941215     C                   EXFMT     LV01D2
007100930120     C*
007200991227     C* F3 - FINE LAVORO
007300991227    ?C   KC              GOTO      FINE
007400930120     C*
007500930120     C* CONTROLLO I FORMATO VIDEO
007600991227     C                   EXSR      CTRD01
007700991227     C* ERRORE
007800991227     C   90
007900991227     CAN 10              GOTO      INIZIO
008000991227     C   90              GOTO      FMTD1
008100991227     C* CARCO SFL
008200991227     C                   EXSR      CARSFL
008300991228     C**
008400991228     C* NESSUN NUMERATORE TROVATO: CHIEDO SE NE VOGLIONO IMMETTERE
008500991228    1C     NRR           IFEQ      0
008600991228     C*
008700991228     C                   SETON                                        70
008800991228     C                   EXFMT     LV01D2
008900991228     C* F3 - FINE LAVORO
009000991228    ?C   KC              GOTO      FINE
009100991228     C*
009200991228    2C     CONF          IFEQ      'NO'
009300991228     C                   SETOFF                                       70
009400991228    ?C                   GOTO      INIZIO
009500991228   X2C                   ELSE
009600991228    ?C                   EXSR      CARVUO
009700991228    2C                   ENDIF
009800991228    1C                   ENDIF
009900930120     C*
010000930120     C* EMMISIONE SUBFILE
010100991228     C                   Z-ADD     NRR           WNRR
010200930120     C                   Z-ADD     1             NRR
010300930125     C                   Z-ADD     1             HSRN
010400930120     C*
010500941215     C                   WRITE     LV01D1
010600941215     C                   WRITE     LV01D5
010700930120     C*
010800930126    ?C     VIDEO         TAG
010900930120     C*
011000930120     C                   SETOFF                                           90
011100941215     C                   EXFMT     LV01CTL
011200061204     c* spengo indicatore di msg
011300061204     c                   setoff                                       28
011400930121     C*
011500991228     C* F3 - FINE LAVORO
011600991228    ?C   KC              GOTO      FINE
011700930121     C*
011800991228     C* F12 - RITORNO
011900930125     C   KL              Z-ADD     *ZEROS        NRR
012000930126    ?C   KL              GOTO      INIZIO
012100930121     C*
012200991228     C* F8 - RICHIESTE RIGHE DI IMMISSIONE (N° 17)
012300930121DO  1C   08              DO
012400930125     C     WNRR          ADD       1             HSRN
012500930125     C*
012600930126    ?C                   EXSR      CARVUO
012700930120     C*
012800930126    ?C                   GOTO      VIDEO
012900930121E   1C                   ENDDO
013000930121     C*
013100930120     C* ELABORAZIONE SUBFILE
013200991228     C                   SETOFF                                           90
013300930125     C                   Z-ADD     PRIMA         HSRN
013400930121     C*
013500941215     C                   READC     LV01SFL                                60
013600991228    1C     *IN60         DOWEQ     *OFF
013700930121     C*
013800930125     C* CONTROLLO CAMPI SFILE
013900930126    ?C                   EXSR      CNTSFL
014000930125     C                   SETON                                        80
014100930121     C*
014200991228    2C     HHHNRR        IFNE      0
014300930120     C                   SETON                                        48
014400991228    2C                   ELSE
014500930120     C                   SETOFF                                           48
014600991228    2C                   ENDIF
014700930121     C*
014800941215     C                   UPDATE    LV01SFL
014900930125     C                   SETOFF                                           48
015000991228     C**
015100991228     C                   READC     LV01SFL                                60
015200991228E   1C                   ENDDO
015300930120     C*
015400991228    ?C   90              GOTO      VIDEO
015500930120     C*
015600930125     C* AGGIORNAMENTI RECORD
015700930121IF  1C     *IN06         IFEQ      '1'
015800930126    ?C                   EXSR      AGGIOR
015900930125     C                   Z-ADD     *ZEROS        NRR
016000930126    ?C                   GOTO      INIZIO
016100930121     C*
016200930121X   1C                   ELSE
016300930121     C*
016400930126    ?C                   GOTO      VIDEO
016500930121E   1C                   ENDIF
016600930120     C*
016700930126    ?C     FINE          TAG
016800930120     C                   SETON                                        LR
016900991227    ?C*---------------------------------------------------------------*
017000991227     C* CONTROLLO FORMATO 1
017100991227    ?C*---------------------------------------------------------------*
017200991227     C     CTRD01        BEGSR
017300991227     C                   SETOFF                                       1090
017400991227     C                   CLEAR                   SAVNUM
017500991227     C**
017600991227     C* L'ANNO DEVE ESSERE COMPRESO DA -3 E + 3
017700991227    1C     V1CANN        IFGT      0
017800991227    2C     V1CANN        IFLT      ANNOME
017900991227     C     V1CANN        ORGT      ANNOPI
018000991227     C                   SETON                                        4090
018100991227     C                   GOTO      ENDCT1
018200991227    2C                   ENDIF
018300991227    1C                   ENDIF
018400991227     C* CODICE NUMERATORE
018500991227IF  1C     VCNUM         IFNE      *BLANKS
018600991227     C     VCNUM         ANDNE     *ZEROS
018700991227     C*
018800991227     C* CONTROLLO TIPO NUMERATORE
018900991228     C     '?'           SCAN      VCNUM                                  90
019000991227     C*
019100991227IF  2C     *IN90         IFEQ      *ON
019200991227     C                   SETON                                        10
019300991227     C                   MOVEL     '1C'          COD
019400991227     C                   MOVEL     *BLANKS       KEY
019500991227     C                   MOVEL     *BLANKS       DES
019600991227     C                   CALL      'X§TABER'
019700020521     C                   PARM                    CODUT
019800991227     C                   PARM                    COD               2
019900991227     C                   PARM                    KEY               8
020000991227     C                   PARM                    DES              25
020100991227     C*
020200991227     C* CODICE SELEZIONATO
020300991227IF  3C     KEY           IFNE      *BLANKS
020400991227     C                   MOVEL     KEY           VCNUM
020500991227X   3C                   ELSE
020600991227     C                   MOVEL     *ZEROS        VCNUM
020700991227E   3C                   ENDIF
020800991227    ?C                   GOTO      ENDCT1
020900991227E   2C                   ENDIF
021000991227     C*
021100991227     C* CONTROLLO ESISTENZA CODICE
021200991227     C                   MOVEL     '1C'          CODTAB
021300991227     C                   MOVEL     *BLANKS       KEYTAB
021400991227     C                   MOVEL     VCNUM         KEYTAB
021500991227     C     KTAB          CHAIN     TABEL00F                           30
021600991227IF  2C     *IN30         IFEQ      *ON
021700991227     C     TBLFLG        ORNE      ' '
021800991227     C                   SETON                                        4190
021900991227    ?C                   GOTO      ENDCT1
022000991227E   2C                   ENDIF
022100991227     C* DEVE ESSERE CONGRUENTE CON L'ANNO
022200991227     C                   MOVEL     TBLUNI        DS1C
022300991227    2C     §1CFAA        IFEQ      'S'
022400991227     C     V1CANN        ANDEQ     0
022500991227     C                   SETON                                        4290
022600991227    ?C                   GOTO      ENDCT1
022700991227E   2C                   ENDIF
022800991227     C*
022900991227    2C     §1CFAA        IFEQ      ' '
023000991227     C     V1CANN        ANDGT     0
023100991227     C                   SETON                                        4390
023200991227    ?C                   GOTO      ENDCT1
023300991227E   2C                   ENDIF
023400991227    2C     §1CDAT        IFEQ      'S'
023500991227     C     V1CDAT        ANDEQ     'N'
023600991227     C                   SETON                                        4590
023700991227    ?C                   GOTO      ENDCT1
023800991227E   2C                   ENDIF
023900991227    2C     §1CDAT        IFEQ      ' '
024000991227     C     V1CDAT        ANDEQ     'S'
024100991227     C                   SETON                                        4490
024200991227    ?C                   GOTO      ENDCT1
024300991227E   2C                   ENDIF
024400991228     C                   MOVEL     VCNUM         SAVNUM            3 0
024500991227E   1C                   ENDIF
024600000113     C**
024700000113     C* CONTROLLO I P.O. SE IMMESSI
024800000113     C     VIDFIL        IFGT      0
024900000113     C     VIDFIL        CHAIN     AZORG01L                           30
025000000113     C     *IN30         IFEQ      *ON
025100000113     C     ORGFVA        ORNE      ' '
025200000113     C     ORGFAG        ORNE      'A'
025300000113     C     ORGFAG        ANDNE     'F'
025400000113     C                   SETON                                        4790
025500000113    ?C                   GOTO      ENDCT1
025600000113     C                   ENDIF
025700000113     C                   ENDIF
025800000113     C     VIDFL2        IFGT      0
025900000113     C     VIDFL2        CHAIN     AZORG01L                           30
026000000113     C     *IN30         IFEQ      *ON
026100000113     C     ORGFVA        ORNE      ' '
026200000113     C     ORGFAG        ORNE      'A'
026300000113     C     ORGFAG        ANDNE     'F'
026400000113     C                   SETON                                        4990
026500000113    ?C                   GOTO      ENDCT1
026600000113     C                   ENDIF
026700000113     C                   ENDIF
026800991227     C     ENDCT1        ENDSR
026900991227    ?C*---------------------------------------------------------------*
027000991227     C* CARICO SFL
027100991227    ?C*---------------------------------------------------------------*
027200991227     C     CARSFL        BEGSR
027300991228     C                   EXSR      PULSFL
027400991227     C                   MOVE      V1CANN        KANN
027500991227     C                   MOVE      VCNUM         KNUM
027600991228     C                   MOVEL     *BLANKS       CONF
027700991228     C                   SETOFF                                       70
027800991228     C                   SETON                                        48
027900991227     C**
028000991228    1C     V1CDAT        IFEQ      'S'
028100991228    2C     VCNUM         IFNE      *BLANKS
028200991228     C     VCNUM         ANDNE     *ZEROS
028300991228     C     KNUM          SETLL     FLNUF02L
028400991228     C                   ELSE
028500991228     C     *LOVAL        SETLL     FLNUF02L
028600991228    2C                   ENDIF
028700991228     C**
028800991228     C                   READ      FLNUF02L                               31
028900991228   X1C                   ELSE
029000991228     C**
029100991228    2C     VCNUM         IFNE      *BLANKS
029200991228     C     VCNUM         ANDNE     *ZEROS
029300000113    3C     VIDFIL        IFGT      0
029400000113    4C     VIDFL2        IFGT      0
029500000113     C* MI POSIZIONO PER ANNO, COD NUM, P.O.1 E P.O.2
029600000113     C     KNUF11        SETLL     FLNUF01L
029700000113   X4C                   ELSE
029800000113     C* MI POSIZIONO PER ANNO, COD NUM, P.O.1
029900000113     C     KNUF1         SETLL     FLNUF01L
030000000113    4C                   ENDIF
030100000113   X3C                   ELSE
030200000113     C* MI POSIZIONO PER ANNO, COD NUM
030300991227     C     KNUF          SETLL     FLNUF01L
030400000113    3C                   ENDIF
030500000113     C**
030600991228     C     KANN          READE     FLNUF01L                               31
030700991228   X2C                   ELSE
030800991227     C     KANN          SETLL     FLNUF01L
030900991227     C     KANN          READE     FLNUF01L                               31
031000991228    2C                   ENDIF
031100991228    1C                   ENDIF
031200991227     C**
031300991228    1C     *IN31         DOWEQ     *OFF
031400991228   1AC     NUFAAA        IFEQ      KANN
031500991228     C**
031600991228    2C     SAVNUM        IFNE      NUFCNU
031700991227     C                   MOVEL     '1C'          CODTAB
031800991227     C                   MOVEL     *BLANKS       KEYTAB
031900991228     C                   MOVEL     NUFCNU        KEYTAB
032000991227     C     KTAB          CHAIN     TABEL00F                           30
032100991228     C  N30              MOVEL     TBLUNI        DS1C
032200991228     C   30              CLEAR                   DS1C
032300991228     C                   MOVE      NUFCNU        SAVNUM
032400991228    2C                   ENDIF
032500991228     C**
032600991228     C* SE LE CARATTERISTICHE CORRISPONDO A QUELLE DELLE RICHIESTE
032700991228     C*  CARICO
032800000113    2C                   SELECT
032900991228     C     §1CFAA        WHENEQ    'S'
033000991228     C     V1CANN        ANDEQ     0
033100991228     C     §1CFAA        WHENEQ    ' '
033200991228     C     V1CANN        ANDGT     0
033300991228     C     §1CDAT        WHENEQ    'S'
033400991228     C     V1CDAT        ANDEQ     'N'
033500991228     C     §1CDAT        WHENEQ    ' '
033600991228     C     V1CDAT        ANDEQ     'S'
033700991228     C                   OTHER
033800991228     C**
033900000113     C** SE IMMESSI I P.O. DEVONO CORRISPONDERE
034000000113    3C     VIDFIL        IFEQ      0
034100000113     C     VIDFIL        OREQ      NUFFIL
034200000113     C*
034300000113    4C     VIDFL2        IFEQ      0
034400000113     C     VIDFL2        OREQ      NUFFL2
034500061204     c* se ne carico più di 9.990 smetto e emetto msg di avviso
034600061204     c                   if        nrr>9990
034700061204     c                   seton                                        2831
034800061204     c                   movel     msg(1)        vidmsg
034900061204     c                   else
035000991228     C                   ADD       1             NRR
035100991228     C                   EXSR      CARICA
035200061204     c                   endif
035300061204     c
035400000113    4C                   ENDIF
035500000113    3C                   ENDIF
035600000113     C**
035700000113    2C                   ENDSL
035800991228   1AC                   ENDIF
035900991227     C**
036000061204     C  n28V1CDAT        IFEQ      'S'
036100991228     C                   READ      FLNUF02L                               31
036200991228     C                   ELSE
036300991227     C     KANN          READE     FLNUF01L                               31
036400991228     C                   ENDIF
036500991228    1C                   ENDDO
036600991227     C**
036700991227     C                   ENDSR
036800991228    ?C*---------------------------------------------------------------*
036900991228     C* PULIZIA SUBFILE
037000991228    ?C*---------------------------------------------------------------*
037100991228     C     PULSFL        BEGSR
037200991228     C                   SETON                                        85
037300991228     C                   WRITE     LV01CTL
037400991228     C                   SETOFF                                       85
037500991228     C                   Z-ADD     0             WNRR
037600991228     C                   ENDSR
037700930125    ?C*---------------------------------------------------------------*
037800930125    ?C*                    CARICA                                     *
037900930125    ?C* ROUTINE DI CARICAMENTO SFILE PER ANNO NUMERATORI ESISTENTE    *
038000930125    ?C*---------------------------------------------------------------*
038100930122     C     CARICA        BEGSR
038200930122     C*
038300930125     C                   SETOFF                                       323398
038400930122     C*
038500930122     C                   MOVE      ' '           VIDANN
038600930122     C                   Z-ADD     NUFNUM        VNUM
038700930125     C                   Z-ADD     NUFFIL        VFIL1
038800930125     C                   Z-ADD     NUFFL2        VFIL2
038900930122     C                   Z-ADD     NUFRIN        VRIN
039000930122     C                   Z-ADD     NUFRFI        VRFI
039100930122     C                   Z-ADD     NUFNUM        HHHNRR
039200991228     C                   Z-ADD     V1CANN        VAAA
039300930125     C* HHHNRR: SONO IN VARIAZIONE O IN INSERIMENTO
039400930122IF  1C     HHHNRR        IFEQ      0
039500930122     C                   MOVE      *HIVAL        HHHNRR
039600930122E   1C                   ENDIF
039700930122     C*
039800930122     C* GIRO LA DATA DOCUMENTO
039900991228     C                   Z-ADD     NUFDAT        HDAT
040000991228     C                   Z-ADD     NUFDAT        VDTP
040100930125     C                   MOVEL     NUFDAT        AA                2 0
040200930125     C                   MOVE      NUFDAT        GG                2 0
040300930125     C                   MOVE      AA            VDTP
040400930125     C                   MOVEL     GG            VDTP
040500930122     C*
040600930125     C* DECODIFICA CODICE NUMERATORE
040700930122     C                   MOVEL     §1CDES        VIDDS1
040800930122     C*
040900930125     C* NUMERATORE MANUTENZIONABILE SOLO DA 'EDP'
041000930122IF  2C     §1CFG1        IFEQ      'S'
041100930125IF  3C     PROFIL        IFNE      'EDP'
041200930122     C                   SETON                                        98
041300930122E   3C                   ENDIF
041400930122E   2C                   ENDIF
041500930122     C*
041600930126     C* SE = 'S' POSSIBILITA' DI INSERIRE CODICI FILIALI INESISTENTI
041700930122IF  2C     §1CFG2        IFNE      'S'
041800930125     C     VFIL1         CHAIN     AZORG01L                           31
041900930122     C                   MOVEL     *BLANKS       VIDDS2
042000930122     C  N31              MOVEL     ORGDES        VIDDS2
042100930122     C   31              MOVEL     *ALL'?'       VIDDS2
042200930122X   2C                   ELSE
042300930122     C                   MOVEL     *BLANKS       VIDDS2
042400930122E   2C                   ENDIF
042500930122     C*
042600930126     C* SE = 'S' CONTROLLO SUBITO CHE I CAMPI SIANO PIENI
042700930122IF  2C     §1CFG3        IFEQ      'S'
042800930122     C     VRFI          COMP      *ZEROS                               3333
042900930122E   2C                   ENDIF
043000930122     C*
043100930122     C                   SETOFF                                           80
043200941215     C                   WRITE     LV01SFL
043300930122     C*
043400930122     C                   ENDSR
043500930125    ?C*---------------------------------------------------------------*
043600930125    ?C*                    CARVUO                                     *
043700930125    ?C* ROUTINE DI CARICAMENTO SFILE PER ANNO NUMERATORI INESISTENTE  *
043800930125    ?C*---------------------------------------------------------------*
043900930122     C     CARVUO        BEGSR
044000930122     C*
044100930126     C                   SETOFF                                       98
044200930126     C*
044300930122     C                   Z-ADD     WNRR          NRR
044400930125     C                   SETOFF                                       4880
044500930122     C*
044600991228DO  1C                   DO        19
044700930122     C                   ADD       1             NRR
044800930125     C                   MOVEL     *BLANKS       VIDANN
044900930122     C                   Z-ADD     *ZEROS        NUFCNU
045000930122     C                   Z-ADD     0             NUFAAA
045100930125     C                   Z-ADD     *ZEROS        VFIL1
045200930125     C                   Z-ADD     *ZEROS        VFIL2
045300930122     C                   Z-ADD     0             VNUM
045400930122     C                   Z-ADD     0             VDTP
045500930122     C                   Z-ADD     0             HHHNRR
045600930122     C                   Z-ADD     0             VRIN
045700930122     C                   Z-ADD     0             VRFI
045800930122     C                   MOVE      *BLANKS       VIDDS1
045900930122     C                   MOVE      *BLANKS       VIDDS2
046000941215     C                   WRITE     LV01SFL
046100930122E   1C                   ENDDO
046200930125     C*
046300930122     C*
046400930122     C                   ENDSR
046500930125    ?C*---------------------------------------------------------------*
046600930125    ?C*                    CNTSFL                                     *
046700930125    ?C* CONTROLLO CAMPI SFILE                                         *
046800930125    ?C*---------------------------------------------------------------*
046900930120     C     CNTSFL        BEGSR
047000991228     C                   SETOFF                                       90
047100991228     C                   CLEAR                   VIDDS2
047200930126     C*
047300940110     C* NON CONTROLLO DATI SUBFILE SE ANNULLO LA RIGA
047400991228    1C     VIDANN        IFEQ      ' '
047500940110     C*
047600930126     C                   MOVEL     '1C'          CODTAB
047700930126     C                   MOVEL     *BLANKS       KEYTAB
047800930126     C                   MOVEL     NUFCNU        KEYTAB
047900991228     C     KTAB          CHAIN     TABEL00F                           30
048000930126     C*
048100991228IF  2C     *IN30         IFEQ      *OFF
048200930126     C                   MOVEL     TBLUNI        DS1C
048300930126     C                   MOVEL     §1CDES        VIDDS1
048400930126     C*
048500991228     C* IMMESSA UNA NUOVA RIGA
048600991228IF  3C     HHHNRR        IFEQ      0
048700991228     C*
048800991228    4C     §1CDAT        IFEQ      ' '
048900991228     C                   MOVE      VAAA          KANN
049000991228     C                   Z-ADD     VFIL1         KFIL1
049100991228     C                   Z-ADD     VFIL2         KFIL2
049200991228     C     KEYNUF        CHAIN     FLNUF01L                           30
049300991228    5C     *IN30         IFNE      '1'
049400991228     C                   SETON                                        9011
049500991228    5C                   ENDIF
049600991228    4C                   ENDIF
049700991228     C*
049800991228     C* CONTROLLO SE IL PROFILO UTENTE CONSENTE DI INSERIRE IL CODICE
049900991228    4C  N90§1CFG1        IFEQ      'S'
050000991228     C     PROFIL        ANDNE     'EDP'
050100991228     C                   SETON                                        9016
050200991228    4C                   ENDIF
050300991228     C*
050400991228    3C                   ENDIF
050500991228     C   90              GOTO      ENDCNT
050600930126     C* IMMETTERE I RANGE SOLO SE PERMESSO
050700991228    3C     §1CFG3        IFEQ      'S'
050800930126     C     VRFI          COMP      *ZEROS                               3333
050900930126     C*
051000930126     C* RANGE A NON < RANGE DA
051100991228    4C     VRFI          IFGT      *ZEROS
051200991228     C     VRFI          ANDLT     VRIN
051300930126     C                   SETON                                          34
051400991228    4C                   ENDIF
051500930126     C   34
051600991228     COR 33              SETON                                        90
051700930126     C*
051800930126     C* NON INSERIRE RANGE DI VALORI PER QUESTO CODICE
051900930126X   3C                   ELSE
052000930126     C*
052100930126IF  4C     VRIN          IFNE      *ZEROS
052200930126     C     VRFI          ORNE      *ZEROS
052300991228     C                   SETON                                        9017
052400991228    4C                   ENDIF
052500991228    3C                   ENDIF
052600991228     C**
052700991228     C* CONTROLLO SE ANNO CONGRUENTE CON COD.NUMERATORE
052800991228     C     VAAA          IFGT      0
052900991228     C     §1CFAA        ANDEQ     ' '
053000991228     C                   SETON                                        9013
053100991228     C                   ENDIF
053200991228     C**
053300991228     C* CONTROLLO SE ANNO CONGRUENTE CON COD.NUMERATORE
053400991228     C     VAAA          IFEQ      0
053500991228     C     §1CFAA        ANDEQ     'S'
053600991228     C                   SETON                                        9013
053700991228     C                   ENDIF
053800991228     C*
053900991228     C     §1CFAA        IFEQ      'S'
054000991228     C     VAAA          IFLT      ANNOME
054100991228     C     VAAA          ORGT      ANNOPI
054200991228     C                   SETON                                        9012
054300991228     C                   ENDIF
054400991228     C                   ENDIF
054500930126     C*
054600930126     C* CODICE NUMERATORE INESISTENTE
054700991228    2C                   ELSE
054800991228     C                   SETON                                        9046
054900991228    2C                   ENDIF
055000991228     C   90              GOTO      ENDCNT
055100930120     C*
055200930126     C* CONTROLLO FILIALI SOLO SE IMMESSE E SE NON E' PERMESSO INSERIRE
055300930126     C* CODICI INESISTENTI
055400991228    2C     §1CFG2        IFNE      'S'
055500930126     C*
055600991228    3C     VFIL1         IFNE      *ZEROS
055700991228     C     VFIL1         CHAIN     AZORG01L                           30
055800991228    4C     *IN30         IFEQ      *ON
055900991228     C     ORGFAG        ORNE      'A'
056000991228     C     ORGFAG        ANDNE     'F'
056100991228     C     ORGFVA        ORNE      ' '
056200991228     C                   SETON                                        9014
056300991228E   4C                   ENDIF
056400991228     C                   MOVEL     ORGDES        VIDDS2
056500991228E   3C                   ENDIF
056600000509E   2C                   ENDIF
056700930125     C*
056800000509    2C     §1CFG5        IFNE      'S'
056900991228IF  3C  N90VFIL2         IFNE      *ZEROS
057000991228     C     VFIL2         CHAIN     AZORG01L                           30
057100991228IF  4C     *IN30         IFEQ      *ON
057200991228     C     ORGFAG        ORNE      'A'
057300991228     C     ORGFAG        ANDNE     'F'
057400991228     C     ORGFVA        ORNE      ' '
057500991228     C                   SETON                                        9015
057600991228E   4C                   ENDIF
057700991228E   3C                   ENDIF
057800991228E   2C                   ENDIF
057900930126     C*
058000930126     C* DATA OBBLIGATORIA
058100991228IF  2C  N90VDTP          IFEQ      *ZEROS
058200991228     C                   SETON                                        9042
058300930126     C*
058400930126     C* DATA FORMALMENTE CORRETTA
058500991228X   2C                   ELSE
058600930126     C                   Z-ADD     VDTP          G02DAT
058700930126     C                   MOVEL     *BLANKS       G02ERR
058800991228     C                   CALL      'XSRDA8'
058900930126     C                   PARM                    WLBDAT
059000930126     C*
059100991228 F  3C     G02ERR        IFEQ      '1'
059200991228     C                   SETON                                        5190
059300991228    3C                   ENDIF
059400991228    2C                   ENDIF
059500991228     C*
059600991228     C* IMMESSA UNA NUOVA RIGA PER NUMERATORE PER DATA
059700991228IF  2C     HHHNRR        IFEQ      0
059800991228     C     §1CDAT        ANDEQ     'S'
059900991228     C                   Z-ADD     G02INV        KDAT
060000991228     C                   MOVEL     NUFCNU        KNUM
060100991228     C     KNUF2         CHAIN     FLNUF02L                           31
060200991228    3C     *IN31         IFEQ      *OFF
060300991228     C                   SETON                                        9011
060400991228    3C                   ENDIF
060500930120     C*
060600991228    2C                   ENDIF
060700991228    1C                   ENDIF
060800940110     C*
060900930126     C* SE ERRORE MI POSIZIONO SULLA PAGINA DEL RECORD SBAGLIATO
061000991228     C     ENDCNT        TAG
061100991228     C   90              Z-ADD     NRR           HSRN
061200930120     C*
061300930120     C                   ENDSR
061400930125    ?C*---------------------------------------------------------------*
061500930125    ?C*                    AGGIOR                                     *
061600930125    ?C* ROUTINE DI GESTIONE AGGIORNAMENTI RECORD FLNUF01L             *
061700930125    ?C*---------------------------------------------------------------*
061800930120     C     AGGIOR        BEGSR
061900930120     C*
062000941215     C                   READC     LV01SFL                                60
062100930120     C*
062200991228    1C     *IN60         DOWEQ     *OFF
062300930120     C*
062400991228    2C     HHHNRR        IFEQ      0
062500991228    3C     VIDANN        IFNE      'A'
062600930126    ?C                   EXSR      WRTREC
062700991228    3C                   ENDIF
062800930121     C*
062900991228   X2C                   ELSE
063000930120     C*
063100991228    3C     VIDANN        IFNE      'A'
063200991228     C                   MOVEL     'U'           WREC              1
063300930126    ?C                   EXSR      UPDREC
063400991228     C                   ELSE
063500991228     C                   MOVEL     'D'           WREC              1
063600991228    ?C                   EXSR      UPDREC
063700991228    3C                   ENDIF
063800991228    2C                   ENDIF
063900930120     C*
064000991228     C                   READC     LV01SFL                                60
064100991228    1C                   ENDDO
064200930120     C*
064300930120     C                   ENDSR
064400930125    ?C*---------------------------------------------------------------*
064500930125    ?C*                    WRTREC                                     *
064600930125    ?C* ROUTINE DI INSERIMENTO RECORD FLNUF01L                        *
064700930125    ?C*---------------------------------------------------------------*
064800930120     C     WRTREC        BEGSR
064900991228     C                   EXSR      CARREC
065000930120     C*
065100930120     C                   WRITE     FLNUF
065200930120     C*
065300930120     C                   ENDSR
065400991228    ?C*---------------------------------------------------------------*
065500991228     C* CQRICO DATI DEL SFL NEL RECORD DI FLNUF
065600991228    ?C*---------------------------------------------------------------*
065700991228     C     CARREC        BEGSR
065800991228     C* GIRO DATA IN A/M/G
065900991228     C                   Z-ADD     VDTP          NUFDAT
066000991228     C                   MOVEL     NUFDAT        GG
066100991228     C                   MOVE      NUFDAT        AA
066200991228     C                   MOVE      GG            NUFDAT
066300991228     C                   MOVEL     AA            NUFDAT
066400991228     C*
066500991228     C                   Z-ADD     VFIL1         NUFFIL
066600991228     C                   Z-ADD     VFIL2         NUFFL2
066700991228     C                   Z-ADD     VNUM          NUFNUM
066800991228     C                   Z-ADD     VRIN          NUFRIN
066900991228     C                   Z-ADD     VRFI          NUFRFI
067000991228     C                   MOVE      VAAA          NUFAAA
067100991228     C*
067200991228     C                   ENDSR
067300930125    ?C*---------------------------------------------------------------*
067400930125    ?C*                    UPDREC                                     *
067500930125    ?C* ROUTINE DI AGGIORNAMENTO RECORD FLNUF01L                      *
067600930125    ?C*---------------------------------------------------------------*
067700930120     C     UPDREC        BEGSR
067800991228     C**
067900991228     C     SAVNUM        IFNE      NUFNUM
068000991228     C                   MOVEL     '1C'          CODTAB
068100991228     C                   MOVEL     *BLANKS       KEYTAB
068200991228     C                   MOVEL     NUFCNU        KEYTAB
068300991228     C     KTAB          CHAIN     TABEL00F                           30
068400991228     C*
068500991228     C  N30              MOVEL     TBLUNI        DS1C
068600991228     C   30              CLEAR                   DS1C
068700991228     C                   MOVEL     NUFNUM        SAVNUM
068800991228     C                   ENDIF
068900930120     C*
069000991228     C                   MOVE      VAAA          KANN
069100991228     C                   Z-ADD     VFIL1         KFIL1
069200991228     C                   Z-ADD     VFIL2         KFIL2
069300991228     C     §1CDAT        IFEQ      ' '
069400991228     C     KEYNUF        CHAIN     FLNUF01L                           95
069500991228     C                   ELSE
069600991228     C                   MOVEL     HDAT          KDAT
069700991228     C     KNUF22        CHAIN     FLNUF02L                           95
069800991228     C                   ENDIF
069900930121     C*
070000991228     C     WREC          IFEQ      'U'
070100991228     C                   EXSR      CARREC
070200991228     C  N95§1CDAT        IFEQ      ' '
070300991228     C                   EXCEPT    UFLNUF
070400991228     C                   ELSE
070500991228     C                   EXCEPT    UFLNU2
070600991228     C                   ENDIF
070700991228     C**
070800991228     C   95              WRITE     FLNUF
070900991228     C                   ENDIF
071000991228     C*
071100991228     C  N95WREC          IFEQ      'D'
071200991228     C     §1CDAT        IFEQ      ' '
071300991228     C                   DELETE    FLNUF
071400991228     C                   ELSE
071500991228     C                   DELETE    FLNUF2
071600991228     C                   ENDIF
071700991228     C                   ENDIF
071800930120     C*
071900930120     C                   ENDSR
072000930125    ?C*---------------------------------------------------------------*
072100930125    ?C*                    DEFCAM                                     *
072200930125    ?C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
072300930125    ?C*---------------------------------------------------------------*
072400930121     C     DEFCAM        BEGSR
072500991227     C     *LIKE         DEFINE    NUFAAA        KANN
072600930121     C*
072700930121     C     *ENTRY        PLIST
072800930121     C                   PARM                    KPJBA
072900020521     c*
073000020521     c     *dtaara       define    §azute        azuteds
073100020521     c     *dtaara       define    §datiute      ddatiute
073200020521     C                   in(E)     *dtaara
073300020521     C                   IF        %error  or RSUT = *blanks
073400020521     C                   CLEAR                   tibs34ds
073500020521     C                   CALL      'TIBS34R'
073600020521     C                   PARM                    tibs34ds
073700020521     C                   in        *dtaara
073800020521     c                   endif
073900020521     C                   MOVEL     RSUT          PARDUT           30
074000061204     c* Apertura file sempre da filtra
074100061204     c* Reperisco S.I.
074200061204     c                   clear                   tibs55ds
074300061204     c                   movel     'L'           i50tla
074400061204     c* come p.o. di partenza un p.o. di filiale
074500061204     c                   eval      i50ppo=001
074600061204     c                   call      'TIBS55R'
074700061204     c                   parm                    tibs55ds
074800061204     c
074900061204     c                   Clear                   comman
075000061204     c                   Movel(p)  cmdt(1)       comman
075100061204     c                   Eval      %Subst(comman:30:10) = o50pla
075200061204     c                   Eval      comman =
075300061204     c                             %trim(comman) + '/FLNUF01L)'
075400061204     c                   Call      'QCMDEXC'                            99
075500061204     c                   Parm                    comman
075600061204     c                   Parm                    lenght
075700061204     c
075800061204     c  n99              Open      FLNUF01L
075900061204     c
076000061204     c                   Clear                   comman
076100061204     c                   Movel(p)  cmdt(2)       comman
076200061204     c                   Eval      %Subst(comman:30:10) = o50pla
076300061204     c                   Eval      comman =
076400061204     c                             %trim(comman) + '/FLNUF02L)'
076500061204     c                   Call      'QCMDEXC'                            99
076600061204     c                   Parm                    comman
076700061204     c                   Parm                    lenght
076800061204     c
076900061204     c  n99              Open      FLNUF02L
077000930121     C*
077100991227     C* LETTURA FLNUF01L PER L'INTERA CHIAVE
077200930121     C     KEYNUF        KLIST
077300991228     C                   KFLD                    KANN
077400930125     C                   KFLD                    NUFCNU
077500930125     C                   KFLD                    KFIL1             3 0
077600930125     C                   KFLD                    KFIL2             3 0
077700930121     C*
077800930122     C*
077900930122     C* LETTURA FLNUF01L PER ANNO E CODICE NUMERATORE
078000991227     C     KNUF          KLIST
078100991227     C                   KFLD                    KANN
078200991227     C                   KFLD                    KNUM              3 0
078300000113     C     KNUF1         KLIST
078400000113     C                   KFLD                    KANN
078500000113     C                   KFLD                    KNUM              3 0
078600000113     C                   KFLD                    VIDFIL
078700000113     C     KNUF11        KLIST
078800000113     C                   KFLD                    KANN
078900000113     C                   KFLD                    KNUM              3 0
079000000113     C                   KFLD                    VIDFIL
079100000113     C                   KFLD                    VIDFL2
079200991228     C     KNUF2         KLIST
079300991228     C                   KFLD                    KNUM
079400991228     C                   KFLD                    KDAT              6 0
079500991228     C     KNUF22        KLIST
079600991228     C                   KFLD                    NUFCNU
079700991228     C                   KFLD                    KDAT              6 0
079800991228     C                   KFLD                    KFIL1             3 0
079900991228     C                   KFLD                    KFIL2             3 0
080000930121     C*
080100930121     C* LETTURA TABEL00F
080200930121     C     KTAB          KLIST
080300930121     C                   KFLD                    CODUT
080400930121     C                   KFLD                    CODTAB            2
080500930121     C                   KFLD                    KEYTAB            8
080600930121     C*
080700930121     C                   Z-ADD     0             NRR               4 0
080800930121     C                   Z-ADD     0             WNRR              4 0
080900930121     C                   Z-ADD     0             XDTGMA            6 0
081000991227     C**
081100991227     C                   TIME                    W0140            14 0
081200991227     C* UDATE IN GGMMAAAA
081300991227     C                   MOVE      W0140         WDTGIO            8 0
081400991227     C*
081500991227     C* UDATE IN AAAAMMGG
081600991227     C                   Z-ADD     WDTGIO        G02DAT
081700991227     C                   MOVEL     *BLANK        G02ERR
081800991227     C                   CALL      'XSRDA8'
081900991227     C                   PARM                    WLBDAT
082000991227     C                   MOVEL     G02INV        DATEU             8 0
082100991227     C                   MOVEL     G02INV        ANNO              4 0
082200991227     C     ANNO          SUB       3             ANNOME            4 0
082300991227     C     ANNO          ADD       3             ANNOPI            4 0
082400930121     C*
082500930121     C                   ENDSR
082600930125    ?C*---------------------------------------------------------------*
082700900607     OFLNUF     E            UFLNUF
082800900607     O                       NUFDAT
082900900607     O                       NUFNUM
083000910220     O                       NUFRIN
083100910220     O                       NUFRFI
083200991228     OFLNUF2    E            UFLNU2
083300991228     O                       NUFDAT
083400991228     O                       NUFNUM
083500991228     O                       NUFRIN
083600991228     O                       NUFRFI
083700061204** cmdt
083800061204OVRDBF FILE(FLNUF01L) TOFILE(
083900061204OVRDBF FILE(FLNUF02L) TOFILE(
084000061204**
084100061204Caricati 9.990 numeratori ma ne sono presenti ancora. SFL pieno!!             10
