000100000000     H*------------------------------------------------------------------------*
000200000000     H* REPERISCE RICAVI/COMPETENZE/VOCI DI C/ECONOMICO PER BOLLA - PARAMETRI  *
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800990812     FTITAS30C  IF   E           K DISK
000900020530     F*---                                 IGNORE(TITAS000)
001000990812     F                                     IGNORE(TITASP00)
001100000000     FTREC78D   CF   E             WORKSTN INFDS(DEVDS1)
001200000000     F                                     SFILE(EC78DVM:MSGULT)
001300000000     D*------------------------------------------------------------------------*
001400000000     D* SCHIERE
001500000000     D*------------------------------------------------------------------------*
001600000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
001700000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)
001800000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
001900000000     D FFU             S              6    DIM(31)
002000000000     D FDE             S             19    DIM(31)
002100000000     D FAT             S              1    DIM(31)
002200000000     D* TABELLA DESCRIZIONI A VIDEO
002300000000     D FUV             S             19    DIM(8)
002400000000     D* TABELLA TASTI FUNZIONALI PREMUTI
002500000000     D KEYS            S              1    DIM(40)
002600000000     D*------------------------------------------------------------------------*
002700000000     D* INPUT
002800000000     D*------------------------------------------------------------------------*
002900000000     D*------------------
003000000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
003100000000     D*------------------
003200000000     D DSFUN         E DS
003300000000     D  FUNRIG                 1     40
003400000000     D*------------------
003500000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
003600000000     D*------------------
003700000000     D DSMSG         E DS
003800000000     D*------------------
003900000000     D* DS PER GESTIONE TASTI FUNZIONALI
004000000000     D*------------------
004100000000     D DSKEYS        E DS
004200000000     D  KEYRIG                 1     40
004300000000     D*------------------
004400000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
004500000000     D*------------------
004600000000     D                SDS
004700000000     D* NOME UTENTE
004800000000     D  USER                 254    263
004900000000     D*------------------
005000000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005100000000     D*------------------
005200000000     D DEVDS1          DS
005300000000     D* CHIAVE FUNZIONALE BATTUTA
005400000000     D  KEY                  369    369
005500000000     D* POSIZIONE CURSORE (IN HEX)
005600000000     D  CURSOR               370    371
005700000000     D*------------------
005800020923     D* DS REPERIMENTO DATI UTENTE
005900020923     D*-------------------
006000020923     D TIBS34DS      E DS
006100020923     D DDATIUTE      E DS
006200020923     D AZUTEDS       E DS                  extname(AZUTE00F)
006300021216     D*-------------
006400021216     D* DS DATI SIMULAZIONE
006500021216     D*-------------
006600021216     D DSD$G         E DS
006700000000     D*------------------
006800000000     D* ARCHITETTURA
006900000000     D*------------------
007000000000     D KPJBA         E DS
007100000000     D*-------------------
007200000000     D* DS ESTERNA PER LANCIO AGGIORNAMENTO SALDI + C/ECONOMICO
007300000000     D*-------------------
007400020510     D DS$ECO        E DS                  INZ
007500000000     D*-------------------
007600000000     D* DS ESTERNA PER CALCOLO IMPORTI E RICAVI - RIDEFINIZIONE IMMAGINE BOLLA -
007700000000     D*-------------------
007800020513     D TRECDS        E DS
007900000000     D  D$PDS1                 1    250
008000000000     D  D$PDS2               251    500
008100000000     D  D$DDS1               501    750
008200000000     D  D$DDS2               751   1000
008300000000     D  D$ODS1              1001   1250
008400000000     D  D$ODS2              1251   1500
008500000000     D*------------------
008600000000     D* PARAMETRI DI PASSAGGIO
008700000000     D*------------------
008800020620     D*PARAM           DS                  INZ
008900020620     D* PARAAS                 1      4  0
009000020620     D* PARLNP                 5      7  0
009100020620     D* PARNRS                 8      9  0
009200020620     D* PARNSP                10     16  0
009300020620     D* PARTBL                17     18
009400020620     D* PARASI                19     22  0
009500020510     D*------------------
009600020510     D* VARIABILI DI WRK
009700020510     D*------------------
009800020510     D  WNAMEVID       S             10
009900020530     I*---
010000020530     I* DEFINIZIONE DEI TIPI RECORD DEL FILE TITAS30C
010100020530     I*---
010200020530     ITITAS000      01
010300020530     ITITAS010      02
010400000000     C*------------------------------------------------------------------------*
010500000000     C* MAIN LINE
010600000000     C*------------------------------------------------------------------------*
010700000000     C*
010800000000     C* CICLO FINO A CHE:
010900000000DO  1C     $FINE         DOUEQ     '1'
011000000000     C                   MOVEL     '0'           $ERR
011100000000     C*
011200000000     C* CONTROLLA GESTIONE VIDEO
011300020513IF  2C     WNAMEVID      IFEQ      'EC78DV1'
011400020514     C     $FINE         ANDNE     '1'
011500000000     C                   EXSR      GESV1
011600000000E   2C                   ENDIF
011700020513IF  2C     WNAMEVID      IFEQ      'EC78DV2'
011800020514     C     $FINE         ANDNE     '1'
011900020513     C                   EXSR      GESV2
012000020513E   2C                   ENDIF
012100020513IF  2C     WNAMEVID      IFEQ      'EC78DV3'
012200020514     C     $FINE         ANDNE     '1'
012300020513     C                   EXSR      GESV3
012400020513E   2C                   ENDIF
012500020513IF  2C     WNAMEVID      IFEQ      'EC78DV4'
012600020514     C     $FINE         ANDNE     '1'
012700020513     C                   EXSR      GESV4
012800020513E   2C                   ENDIF
012900020621IF  2C     WNAMEVID      IFEQ      'EC78DV5'
013000020621     C     $FINE         ANDNE     '1'
013100020621     C                   EXSR      GESV5
013200020621E   2C                   ENDIF
013300020621IF  2C     WNAMEVID      IFEQ      'EC78DV6'
013400020621     C     $FINE         ANDNE     '1'
013500020621     C                   EXSR      GESV6
013600020621E   2C                   ENDIF
013700020621IF  2C     WNAMEVID      IFEQ      'EC78DV7'
013800020621     C     $FINE         ANDNE     '1'
013900020621     C                   EXSR      GESV7
014000020621E   2C                   ENDIF
014100020621IF  2C     WNAMEVID      IFEQ      'EC78DV8'
014200020621     C     $FINE         ANDNE     '1'
014300020621     C                   EXSR      GESV8
014400020621E   2C                   ENDIF
014500000000     C*
014600000000E   1C                   ENDDO
014700000000     C*
014800000000     C* LANCIA PROGRAMMA CON CHIUSURA
014900000000IF  1C     LAND7R        IFEQ      'S'
015000000000     C                   MOVEL     'C'           D$GSTS
015100950405     C                   CALL      'TRECD6R'
015200000000     C                   PARM                    DS$ECO
015300000000E   1C                   ENDIF
015400000000     C*
015500000000     C                   SETON                                        LR
015600000000     C*------------------------------------------------------------------------*
015700000000     C* GESV1 - GESTIONE PRIMO FORMATO VIDEO
015800000000     C*------------------------------------------------------------------------*
015900000000     C     GESV1         BEGSR
016000000000     C*
016100000000IF  1C     $TESVD        IFEQ      '1'
016200000000     C                   EXSR      CARV1
016300000000E   1C                   ENDIF
016400000000     C*
016500000000IF  1C     MSGULT        IFEQ      *ZEROS
016600000000     C                   MOVEL     *BLANKS       $MSG
016700000000     C                   EXSR      CARMSG
016800000000E   1C                   ENDIF
016900000000     C                   EXSR      OUTMSG
017000000000     C*
017100000000     C                   EXFMT     EC78DV1
017200000000     C*
017300020514     C                   EXSR      GESKEY1
017400000000     C*
017500000000     C                   ENDSR
017600020513     C*------------------------------------------------------------------------*
017700020513     C* GESV2 - GESTIONE SECONDO FORMATO VIDEO
017800020513     C*------------------------------------------------------------------------*
017900020513     C     GESV2         BEGSR
018000020513     C*
018100020513IF  1C     $TESVD        IFEQ      '1'
018200020513     C                   EXSR      CARV1
018300020513E   1C                   ENDIF
018400020513     C*
018500020513IF  1C     MSGULT        IFEQ      *ZEROS
018600020513     C                   MOVEL     *BLANKS       $MSG
018700020513     C                   EXSR      CARMSG
018800020513E   1C                   ENDIF
018900020513     C                   EXSR      OUTMSG
019000020513     C*
019100020513     C                   EXFMT     EC78DV2
019200020517     C* MARCO IL PASSAGGIO SU QUESTO FORMATO VIDEO
019300020517     C                   MOVEL     '1'           FLGVIDP
019400020517     C                   MOVEL     '1'           FLGVIDD
019500020513     C*
019600020514     C                   EXSR      GESKEY2
019700020513     C*
019800020513     C                   ENDSR
019900020513     C*------------------------------------------------------------------------*
020000020513     C* GESV3 - GESTIONE TERZO FORMATO VIDEO
020100020513     C*------------------------------------------------------------------------*
020200020513     C     GESV3         BEGSR
020300020513     C*
020400020513IF  1C     $TESVD        IFEQ      '1'
020500020513     C                   EXSR      CARV1
020600020513E   1C                   ENDIF
020700020513     C*
020800020513IF  1C     MSGULT        IFEQ      *ZEROS
020900020513     C                   MOVEL     *BLANKS       $MSG
021000020513     C                   EXSR      CARMSG
021100020513E   1C                   ENDIF
021200020513     C                   EXSR      OUTMSG
021300020513     C*
021400020513     C                   EXFMT     EC78DV3
021500020517     C* MARCO IL PASSAGGIO SU QUESTO FORMATO VIDEO
021600020517     C                   MOVEL     '1'           FLGVIDP
021700020517     C                   MOVEL     '1'           FLGVIDD
021800020513     C*
021900020514     C                   EXSR      GESKEY3
022000020513     C*
022100020513     C                   ENDSR
022200020513     C*------------------------------------------------------------------------*
022300020513     C* GESV4 - GESTIONE QUARTO FORMATO VIDEO
022400020513     C*------------------------------------------------------------------------*
022500020513     C     GESV4         BEGSR
022600020513     C*
022700020513IF  1C     $TESVD        IFEQ      '1'
022800020513     C                   EXSR      CARV1
022900020513E   1C                   ENDIF
023000020513     C*
023100020513IF  1C     MSGULT        IFEQ      *ZEROS
023200020513     C                   MOVEL     *BLANKS       $MSG
023300020513     C                   EXSR      CARMSG
023400020513E   1C                   ENDIF
023500020513     C                   EXSR      OUTMSG
023600020513     C*
023700020513     C                   EXFMT     EC78DV4
023800020517     C* MARCO IL PASSAGGIO SU QUESTO FORMATO VIDEO
023900020517     C                   MOVEL     '1'           FLGVIDP
024000020517     C                   MOVEL     '1'           FLGVIDD
024100020513     C*
024200020514     C                   EXSR      GESKEY4
024300020513     C*
024400020513     C                   ENDSR
024500020621     C*------------------------------------------------------------------------*
024600020621     C* GESV5 - GESTIONE QUINTO FORMATO VIDEO
024700020621     C*------------------------------------------------------------------------*
024800020621     C     GESV5         BEGSR
024900020621     C*
025000020621IF  1C     $TESVD        IFEQ      '1'
025100020621     C                   EXSR      CARV1
025200020621E   1C                   ENDIF
025300020621     C*
025400020621IF  1C     MSGULT        IFEQ      *ZEROS
025500020621     C                   MOVEL     *BLANKS       $MSG
025600020621     C                   EXSR      CARMSG
025700020621E   1C                   ENDIF
025800020621     C                   EXSR      OUTMSG
025900020621     C*
026000020621     C                   EXFMT     EC78DV5
026100020621     C* MARCO IL PASSAGGIO SU QUESTO FORMATO VIDEO
026200020621     C                   MOVEL     '1'           FLGVIDP
026300020621     C                   MOVEL     '1'           FLGVIDD
026400020621     C*
026500020621     C                   EXSR      GESKEY5
026600020621     C*
026700020621     C                   ENDSR
026800020621     C*------------------------------------------------------------------------*
026900020621     C* GESV6 - GESTIONE SESTO FORMATO VIDEO
027000020621     C*------------------------------------------------------------------------*
027100020621     C     GESV6         BEGSR
027200020621     C*
027300020621IF  1C     $TESVD        IFEQ      '1'
027400020621     C                   EXSR      CARV1
027500020621E   1C                   ENDIF
027600020621     C*
027700020621IF  1C     MSGULT        IFEQ      *ZEROS
027800020621     C                   MOVEL     *BLANKS       $MSG
027900020621     C                   EXSR      CARMSG
028000020621E   1C                   ENDIF
028100020621     C                   EXSR      OUTMSG
028200020621     C*
028300020621     C                   EXFMT     EC78DV6
028400020621     C* MARCO IL PASSAGGIO SU QUESTO FORMATO VIDEO
028500020621     C                   MOVEL     '1'           FLGVIDP
028600020621     C                   MOVEL     '1'           FLGVIDD
028700020621     C*
028800020621     C                   EXSR      GESKEY6
028900020621     C*
029000020621     C                   ENDSR
029100020621     C*------------------------------------------------------------------------*
029200020621     C* GESV7 - GESTIONE SETTIMO FORMATO VIDEO
029300020621     C*------------------------------------------------------------------------*
029400020621     C     GESV7         BEGSR
029500020621     C*
029600020621IF  1C     $TESVD        IFEQ      '1'
029700020621     C                   EXSR      CARV1
029800020621E   1C                   ENDIF
029900020621     C*
030000020621IF  1C     MSGULT        IFEQ      *ZEROS
030100020621     C                   MOVEL     *BLANKS       $MSG
030200020621     C                   EXSR      CARMSG
030300020621E   1C                   ENDIF
030400020621     C                   EXSR      OUTMSG
030500020621     C*
030600020621     C                   EXFMT     EC78DV7
030700020621     C* MARCO IL PASSAGGIO SU QUESTO FORMATO VIDEO
030800020621     C                   MOVEL     '1'           FLGVIDP
030900020621     C                   MOVEL     '1'           FLGVIDD
031000020621     C*
031100020621     C                   EXSR      GESKEY7
031200020621     C*
031300020621     C                   ENDSR
031400020621     C*------------------------------------------------------------------------*
031500020621     C* GESV8 - GESTIONE OTTAVO FORMATO VIDEO
031600020621     C*------------------------------------------------------------------------*
031700020621     C     GESV8         BEGSR
031800020621     C*
031900020621IF  1C     $TESVD        IFEQ      '1'
032000020621     C                   EXSR      CARV1
032100020621E   1C                   ENDIF
032200020621     C*
032300020621IF  1C     MSGULT        IFEQ      *ZEROS
032400020621     C                   MOVEL     *BLANKS       $MSG
032500020621     C                   EXSR      CARMSG
032600020621E   1C                   ENDIF
032700020621     C                   EXSR      OUTMSG
032800020621     C*
032900020621     C                   EXFMT     EC78DV8
033000020621     C* MARCO IL PASSAGGIO SU QUESTO FORMATO VIDEO
033100020621     C                   MOVEL     '1'           FLGVIDP
033200020621     C                   MOVEL     '1'           FLGVIDD
033300020621     C*
033400020621     C                   EXSR      GESKEY8
033500020621     C*
033600020621     C                   ENDSR
033700020513     C*------------------------------------------------------------------------*
033800020514     C* GESKEY1 - GESTIONE TASTI VIDEO 1
033900020513     C*------------------------------------------------------------------------*
034000020514     C     GESKEY1       BEGSR
034100020513     C*
034200020514     C                   MOVEL     '1'           $CHGVID
034300020514     C*
034400020513     C                   EXSR      CONFUN
034500020513     C*
034600020513     C     $ERR          IFEQ      '0'
034700020513     C                   SELECT
034800020517     C     KEY           WHENEQ    F4
034900020517     C                   EXSR      F04V1
035000020513     C     KEY           WHENEQ    F6
035100020513     C                   EXSR      F06V1
035200020513     C     KEY           WHENEQ    F7
035300020513     C                   EXSR      F07V1
035400020517     C     KEY           WHENEQ    F8
035500020517     C                   EXSR      F08V1
035600020513     C     KEY           WHENEQ    ROLLUP
035700020513     C                   EXSR      RDGES
035800020513     C     KEY           WHENEQ    ROLLDN
035900020513     C                   EXSR      RUGES
036000020513     C     KEY           WHENEQ    ENTER
036100020513     C                   EXSR      ENTV1
036200020513     C                   OTHER
036300020513     C                   EXSR      FUNSTD
036400020513     C                   ENDSL
036500020513     C                   ENDIF
036600020513     C*
036700020513     C                   ENDSR
036800020514     C*------------------------------------------------------------------------*
036900020514     C* GESKEY2 - GESTIONE TASTI VIDEO 2
037000020514     C*------------------------------------------------------------------------*
037100020514     C     GESKEY2       BEGSR
037200020514     C*
037300020514     C                   MOVEL     '1'           $CHGVID
037400020514     C*
037500020514     C                   EXSR      CONFUN
037600020514     C*
037700020514     C     $ERR          IFEQ      '0'
037800020514     C                   SELECT
037900020517     C     KEY           WHENEQ    F4
038000020517     C                   EXSR      F04V1
038100020514     C     KEY           WHENEQ    F6
038200020514     C                   EXSR      F06V1
038300020514     C     KEY           WHENEQ    F7
038400020514     C                   EXSR      F07V1
038500020517     C     KEY           WHENEQ    F8
038600020517     C                   EXSR      F08V1
038700020514     C     KEY           WHENEQ    ROLLUP
038800020514     C                   EXSR      RDGES
038900020514     C     KEY           WHENEQ    ROLLDN
039000020514     C                   EXSR      RUGES
039100020514     C     KEY           WHENEQ    ENTER
039200020514     C                   EXSR      ENTV1
039300020514     C                   OTHER
039400020514     C                   EXSR      FUNSTD
039500020514     C                   ENDSL
039600020514     C                   ENDIF
039700020514     C*
039800020514     C                   ENDSR
039900020514     C*------------------------------------------------------------------------*
040000020514     C* GESKEY3 - GESTIONE TASTI VIDEO 3
040100020514     C*------------------------------------------------------------------------*
040200020514     C     GESKEY3       BEGSR
040300020514     C*
040400020514     C                   MOVEL     '1'           $CHGVID
040500020514     C*
040600020514     C                   EXSR      CONFUN
040700020514     C*
040800020514     C     $ERR          IFEQ      '0'
040900020514     C                   SELECT
041000020517     C     KEY           WHENEQ    F4
041100020517     C                   EXSR      F04V1
041200020514     C     KEY           WHENEQ    F6
041300020514     C                   EXSR      F06V1
041400020514     C     KEY           WHENEQ    F7
041500020514     C                   EXSR      F07V1
041600020517     C     KEY           WHENEQ    F8
041700020517     C                   EXSR      F08V1
041800020514     C     KEY           WHENEQ    ROLLUP
041900020514     C                   EXSR      RDGES
042000020514     C     KEY           WHENEQ    ROLLDN
042100020514     C                   EXSR      RUGES
042200020514     C     KEY           WHENEQ    ENTER
042300020514     C                   EXSR      ENTV1
042400020514     C                   OTHER
042500020514     C                   EXSR      FUNSTD
042600020514     C                   ENDSL
042700020514     C                   ENDIF
042800020514     C*
042900020514     C                   ENDSR
043000020514     C*------------------------------------------------------------------------*
043100020514     C* GESKEY4 - GESTIONE TASTI VIDEO 4
043200020514     C*------------------------------------------------------------------------*
043300020514     C     GESKEY4       BEGSR
043400020514     C*
043500020514     C                   MOVEL     '1'           $CHGVID
043600020514     C*
043700020514     C                   EXSR      CONFUN
043800020514     C*
043900020514     C     $ERR          IFEQ      '0'
044000020514     C                   SELECT
044100020517     C     KEY           WHENEQ    F4
044200020517     C                   EXSR      F04V1
044300020514     C     KEY           WHENEQ    F6
044400020514     C                   EXSR      F06V1
044500020514     C     KEY           WHENEQ    F7
044600020514     C                   EXSR      F07V1
044700020517     C     KEY           WHENEQ    F8
044800020517     C                   EXSR      F08V1
044900020514     C     KEY           WHENEQ    ROLLUP
045000020514     C                   EXSR      RDGES
045100020514     C     KEY           WHENEQ    ROLLDN
045200020514     C                   EXSR      RUGES
045300020514     C     KEY           WHENEQ    ENTER
045400020514     C                   EXSR      ENTV1
045500020514     C                   OTHER
045600020514     C                   EXSR      FUNSTD
045700020514     C                   ENDSL
045800020514     C                   ENDIF
045900020514     C*
046000020514     C                   ENDSR
046100020621     C*------------------------------------------------------------------------*
046200020621     C* GESKEY5 - GESTIONE TASTI VIDEO 5
046300020621     C*------------------------------------------------------------------------*
046400020621     C     GESKEY5       BEGSR
046500020621     C*
046600020621     C                   MOVEL     '1'           $CHGVID
046700020621     C*
046800020621     C                   EXSR      CONFUN
046900020621     C*
047000020621     C     $ERR          IFEQ      '0'
047100020621     C                   SELECT
047200020621     C     KEY           WHENEQ    F4
047300020621     C                   EXSR      F04V1
047400020621     C     KEY           WHENEQ    F6
047500020621     C                   EXSR      F06V1
047600020621     C     KEY           WHENEQ    F7
047700020621     C                   EXSR      F07V1
047800020621     C     KEY           WHENEQ    F8
047900020621     C                   EXSR      F08V1
048000020621     C     KEY           WHENEQ    ROLLUP
048100020621     C                   EXSR      RDGES
048200020621     C     KEY           WHENEQ    ROLLDN
048300020621     C                   EXSR      RUGES
048400020621     C     KEY           WHENEQ    ENTER
048500020621     C                   EXSR      ENTV1
048600020621     C                   OTHER
048700020621     C                   EXSR      FUNSTD
048800020621     C                   ENDSL
048900020621     C                   ENDIF
049000020621     C*
049100020621     C                   ENDSR
049200020621     C*------------------------------------------------------------------------*
049300020621     C* GESKEY6 - GESTIONE TASTI VIDEO 6
049400020621     C*------------------------------------------------------------------------*
049500020621     C     GESKEY6       BEGSR
049600020621     C*
049700020621     C                   MOVEL     '1'           $CHGVID
049800020621     C*
049900020621     C                   EXSR      CONFUN
050000020621     C*
050100020621     C     $ERR          IFEQ      '0'
050200020621     C                   SELECT
050300020621     C     KEY           WHENEQ    F4
050400020621     C                   EXSR      F04V1
050500020621     C     KEY           WHENEQ    F6
050600020621     C                   EXSR      F06V1
050700020621     C     KEY           WHENEQ    F7
050800020621     C                   EXSR      F07V1
050900020621     C     KEY           WHENEQ    F8
051000020621     C                   EXSR      F08V1
051100020621     C     KEY           WHENEQ    ROLLUP
051200020621     C                   EXSR      RDGES
051300020621     C     KEY           WHENEQ    ROLLDN
051400020621     C                   EXSR      RUGES
051500020621     C     KEY           WHENEQ    ENTER
051600020621     C                   EXSR      ENTV1
051700020621     C                   OTHER
051800020621     C                   EXSR      FUNSTD
051900020621     C                   ENDSL
052000020621     C                   ENDIF
052100020621     C*
052200020621     C                   ENDSR
052300020621     C*------------------------------------------------------------------------*
052400020621     C* GESKEY7 - GESTIONE TASTI VIDEO 7
052500020621     C*------------------------------------------------------------------------*
052600020621     C     GESKEY7       BEGSR
052700020621     C*
052800020621     C                   MOVEL     '1'           $CHGVID
052900020621     C*
053000020621     C                   EXSR      CONFUN
053100020621     C*
053200020621     C     $ERR          IFEQ      '0'
053300020621     C                   SELECT
053400020621     C     KEY           WHENEQ    F4
053500020621     C                   EXSR      F04V1
053600020621     C     KEY           WHENEQ    F6
053700020621     C                   EXSR      F06V1
053800020621     C     KEY           WHENEQ    F7
053900020621     C                   EXSR      F07V1
054000020621     C     KEY           WHENEQ    F8
054100020621     C                   EXSR      F08V1
054200020621     C     KEY           WHENEQ    ROLLUP
054300020621     C                   EXSR      RDGES
054400020621     C     KEY           WHENEQ    ROLLDN
054500020621     C                   EXSR      RUGES
054600020621     C     KEY           WHENEQ    ENTER
054700020621     C                   EXSR      ENTV1
054800020621     C                   OTHER
054900020621     C                   EXSR      FUNSTD
055000020621     C                   ENDSL
055100020621     C                   ENDIF
055200020621     C*
055300020621     C                   ENDSR
055400020621     C*------------------------------------------------------------------------*
055500020621     C* GESKEY8 - GESTIONE TASTI VIDEO 8
055600020621     C*------------------------------------------------------------------------*
055700020621     C     GESKEY8       BEGSR
055800020621     C*
055900020621     C                   MOVEL     '1'           $CHGVID
056000020621     C*
056100020621     C                   EXSR      CONFUN
056200020621     C*
056300020621     C     $ERR          IFEQ      '0'
056400020621     C                   SELECT
056500020621     C     KEY           WHENEQ    F4
056600020621     C                   EXSR      F04V1
056700020621     C     KEY           WHENEQ    F6
056800020621     C                   EXSR      F06V1
056900020621     C     KEY           WHENEQ    F7
057000020621     C                   EXSR      F07V1
057100020621     C     KEY           WHENEQ    F8
057200020621     C                   EXSR      F08V1
057300020621     C     KEY           WHENEQ    ROLLUP
057400020621     C                   EXSR      RDGES
057500020621     C     KEY           WHENEQ    ROLLDN
057600020621     C                   EXSR      RUGES
057700020621     C     KEY           WHENEQ    ENTER
057800020621     C                   EXSR      ENTV1
057900020621     C                   OTHER
058000020621     C                   EXSR      FUNSTD
058100020621     C                   ENDSL
058200020621     C                   ENDIF
058300020621     C*
058400020621     C                   ENDSR
058500000000     C*------------------------------------------------------------------------*
058600020513     C* CARV1 - CARICA VIDEO INIZIALI
058700000000     C*------------------------------------------------------------------------*
058800000000     C     CARV1         BEGSR
058900000000     C*
059000000000     C                   EXSR      CARFUN
059100000000     C                   EXSR      OUTFUN
059200000000     C*
059300000000     C                   WRITE     EC78DVT
059400000000     C                   WRITE     EC78DVF
059500000000     C*
059600000000     C                   MOVEL     '0'           $TESVD
059700000000     C*
059800000000     C                   ENDSR
059900000000     C*------------------------------------------------------------------------*
060000000000     C* ENTV1 - GESTIONE TASTO ENTER PRIMO FORMATO VIDEO
060100000000     C*------------------------------------------------------------------------*
060200000000     C     ENTV1         BEGSR
060300020513     C*
060400000000     C* CONTROLLA I CAMPI IMMESSI
060500000000     C                   EXSR      DECODI
060600000000     C*
060700000000     C                   ENDSR
060800020517     C*------------------------------------------------------------------------*
060900020517     C* F04V1  - RIPRISTINA DATI ORGIGINALI BOLLA
061000020517     C*------------------------------------------------------------------------*
061100020517     C     F04V1         BEGSR
061200020517     C*
061300020517     C                   MOVEL     '0'           FLGVIDP
061400020517     C                   MOVEL     '0'           FLGVIDD
061500020517     C*
061600020517     C                   EXSR      DECODI
061700020517     C*
061800020517     C                   ENDSR
061900000000     C*------------------------------------------------------------------------*
062000000000     C* F06V1  - LANCIA PROGRAMMA CHE REPERISCE I RICAVI E LE COMPETENZE
062100000000     C*------------------------------------------------------------------------*
062200000000     C     F06V1         BEGSR
062300000000     C*
062400000000     C* CONTROLLA I CAMPI IMMESSI
062500000000     C                   EXSR      DECODI
062600000000     C*
062700000000     C     $ERR          IFEQ      '0'
062800000000     C*
062900000000     C* IMPOSTA PARAMETRI DI PASSAGGIO -DOPO-
063000020620     C***                Z-ADD     V1CAA2        PARAAS
063100020620     C***                Z-ADD     V1CLN2        PARLNP
063200020620     C***                Z-ADD     V1CNR2        PARNRS
063300020620     C***                Z-ADD     V1CNS2        PARNSP
063400020620     C***                MOVEL     V1CTB2        PARTBL
063500020620     C                   Z-ADD     V1CASI        PARASI            4 0
063600020620     C*
063700020620     C* CERCA TIPO LANCIO
063800020620     C     V1CAAS        IFEQ      *ZEROS
063900020620     C     V1CAA2        ANDNE     *ZEROS
064000020620     C                   MOVEL     'I'           TIPO              1
064100020620     C                   ENDIF
064200020620     C     V1CAAS        IFNE      *ZEROS
064300020620     C     V1CAA2        ANDEQ     *ZEROS
064400020620     C                   MOVEL     'D'           TIPO
064500020620     C                   ENDIF
064600020620     C     V1CAAS        IFNE      *ZEROS
064700020620     C     V1CAA2        ANDNE     *ZEROS
064800020620     C                   MOVEL     'U'           TIPO
064900020620     C                   ENDIF
065000020620     C*
065100020620     C* IMPOSTA CAMPI COMUNI
065200020620     C                   MOVEL     TIPO          D$GSTS
065300020620     C                   MOVEL     TIPO          D$GSTO
065400020620     C                   Z-ADD     UDATE         D$GDAT
065500020620     C                   TIME                    D$GTIM
065600020620     C                   MOVEL     'EDPRN'       D$GUSR
065700020620     C                   MOVEL     'TREC78R'     D$GPGF
065800020620     C                   MOVEL     'GAITRAPRV '  D$GLIB
065900020620     C                   MOVEL     'TRECD6R'     D$GPGS
066000020620     C     *IN01         IFEQ      *ON
066100020620     C                   MOVEL     'TITAS00F'    D$GFIL
066200020620     C                   MOVEL     'TITAS00F'    D$GMBR
066300020620     C                   MOVEL     'N'           D$GSDA
066400020620     C                   MOVEL     'N'           D$GSDC
066500020620     C                   MOVEL     'N'           D$GSDF
066600020620     C                   MOVEL     'N'           D$GSDO
066700020620     C                   MOVEL     'N'           D$GSDP
066800020620     C                   MOVEL     'N'           D$GSDR
066900020620     C                   MOVEL     'N'           D$GSDT
067000020621     C                   MOVEL     'N'           D$GCET
067100020620     C                   MOVEL     'N'           D$GWFM
067200020620     C                   MOVEL     'N'           D$GFIM
067300020621     C                   MOVEL     'N'           D$GFCE
067400030123     C                   MOVEL     'N'           D$GCOP
067500020620     C                   ENDIF
067600020620     C     *IN02         IFEQ      *ON
067700020620     C                   MOVEL     'TITAS10F'    D$GFIL
067800020620     C                   MOVEL     'TITAS10F'    D$GMBR
067900020621     C                   MOVEL     'N'           D$GSDA
068000020621     C                   MOVEL     'N'           D$GSDC
068100020621     C                   MOVEL     'N'           D$GSDF
068200020621     C                   MOVEL     'N'           D$GSDO
068300020621     C                   MOVEL     'N'           D$GSDP
068400020621     C                   MOVEL     'N'           D$GSDR
068500020621     C                   MOVEL     'N'           D$GSDT
068600020621     C                   MOVEL     'N'           D$GCET
068700020621     C                   MOVEL     'N'           D$GWFM
068800020621     C                   MOVEL     'N'           D$GFIM
068900020621     C                   MOVEL     'N'           D$GFCE
069000030123     C                   MOVEL     'N'           D$GCOP
069100020620     C                   ENDIF
069200020620     C                   MOVEL     *BLANKS       D$GERR
069300020620     C                   MOVEL     *BLANKS       D$GMSG
069400020620     C                   Z-ADD     *ZEROS        D$GPRE
069500020620     C                   MOVEL     *BLANKS       D$GXXX
069600000000     C*
069700000000     C* LANCIA PROGRAMMA DI VISUALIZZAZIONE
069800020620     C***                MOVEL     PARAM         KPJBU
069900020620     C***  KNSIF         IFEQ      'GAITRAPRV '
070000020620     C***                CALL      'TREC79C'
070100020620     C***                PARM                    KPJBA
070200020620     C***                ELSE
070300960304     C                   CALL      'TREC79R'
070400031212     C                   PARM                    KPJBA
070500020620     C                   PARM                    DS$ECO
070600020620     C                   PARM                    PARASI
070700020620     C***                ENDIF
070800000000     C*
070900000000     C                   MOVEL     '1'           $TESVD
071000020517     C* RE-INIZIALIZZO A VIDEO I CAMPI DELLA CHIAVE BOLLA
071100020517     C                   Z-ADD     *ZEROS        V1CAAS
071200020517     C                   Z-ADD     *ZEROS        V1CAA2
071300020517     C                   Z-ADD     *ZEROS        V1CLNP
071400020517     C                   Z-ADD     *ZEROS        V1CLN2
071500020517     C                   Z-ADD     *ZEROS        V1CNRS
071600020517     C                   Z-ADD     *ZEROS        V1CNR2
071700020517     C                   Z-ADD     *ZEROS        V1CNSP
071800020517     C                   Z-ADD     *ZEROS        V1CNS2
071900020517     C                   MOVEL     *BLANKS       V1CTBL
072000020517     C                   MOVEL     *BLANKS       V1CTB2
072100020517     C                   MOVEL     '0'           FLGVIDP
072200020517     C                   MOVEL     '0'           FLGVIDD
072300000000     C*
072400000000     C                   ENDIF
072500000000     C*
072600000000     C                   ENDSR
072700000000     C*------------------------------------------------------------------------*
072800000000     C* F07V1  - LANCIA PROGRAMMA 'GUFO' CHE AGGIORNA SALDI E C.ECON.
072900000000     C*------------------------------------------------------------------------*
073000000000     C     F07V1         BEGSR
073100000000     C*
073200000000     C* CONTROLLA I CAMPI IMMESSI
073300000000     C                   EXSR      DECODI
073400000000     C*
073500000000     C     $ERR          IFEQ      '0'
073600000000     C*
073700000000     C* CERCA TIPO LANCIO
073800000000     C     V1CAAS        IFEQ      *ZEROS
073900000000     C     V1CAA2        ANDNE     *ZEROS
074000000000     C                   MOVEL     'I'           TIPO              1
074100000000     C                   ENDIF
074200000000     C     V1CAAS        IFNE      *ZEROS
074300000000     C     V1CAA2        ANDEQ     *ZEROS
074400000000     C                   MOVEL     'D'           TIPO
074500000000     C                   ENDIF
074600000000     C     V1CAAS        IFNE      *ZEROS
074700000000     C     V1CAA2        ANDNE     *ZEROS
074800000000     C                   MOVEL     'U'           TIPO
074900000000     C                   ENDIF
075000000000     C*
075100000000     C* IMPOSTA CAMPI COMUNI
075200000000     C                   MOVEL     TIPO          D$GSTS
075300000000     C                   Z-ADD     UDATE         D$GDAT
075400000000     C                   TIME                    D$GTIM
075500000000     C                   MOVEL     'EDPRN'       D$GUSR
075600000000     C                   MOVEL     'TREC78R'     D$GPGF
075700020430     C                   MOVEL     'GAITRAPRV '  D$GLIB
075800000000     C                   MOVEL     'TRECD6R'     D$GPGS
075900020530     C     *IN01         IFEQ      *ON
076000020530     C                   MOVEL     'TITAS00F'    D$GFIL
076100020530     C                   MOVEL     'TITAS00F'    D$GMBR
076200020530     C                   MOVEL     'N'           D$GSDA
076300020530     C                   MOVEL     'N'           D$GSDC
076400020530     C                   MOVEL     'N'           D$GSDF
076500020530     C                   MOVEL     'N'           D$GSDO
076600020530     C                   MOVEL     'N'           D$GSDP
076700020530     C                   MOVEL     'N'           D$GSDR
076800020530     C                   MOVEL     'N'           D$GSDT
076900020530     C                   MOVEL     'S'           D$GCET
077000020530     C                   MOVEL     'N'           D$GWFM
077100020531     C                   MOVEL     'N'           D$GFIM
077200020531     C                   MOVEL     'S'           D$GFCE
077300030123     C                   MOVEL     'S'           D$GCOP
077400020530     C                   ENDIF
077500020530     C     *IN02         IFEQ      *ON
077600020530     C                   MOVEL     'TITAS10F'    D$GFIL
077700020530     C                   MOVEL     'TITAS10F'    D$GMBR
077800020221     C                   MOVEL     'S'           D$GSDA
077900020221     C                   MOVEL     'S'           D$GSDC
078000020221     C                   MOVEL     'S'           D$GSDF
078100020221     C                   MOVEL     'S'           D$GSDO
078200020221     C                   MOVEL     'S'           D$GSDP
078300020221     C                   MOVEL     'S'           D$GSDR
078400020221     C                   MOVEL     'S'           D$GSDT
078500020221     C                   MOVEL     'S'           D$GCET
078600000000     C                   MOVEL     'S'           D$GWFM
078700020531     C                   MOVEL     'S'           D$GFIM
078800020531     C                   MOVEL     'S'           D$GFCE
078900030123     C                   MOVEL     'S'           D$GCOP
079000020530     C                   ENDIF
079100000000     C                   MOVEL     *BLANKS       D$GERR
079200000000     C                   MOVEL     *BLANKS       D$GMSG
079300000000     C                   Z-ADD     *ZEROS        D$GPRE
079400021216     C                   MOVEL     *BLANKS       D$GXXX
079500021216     C     V1CASI        IFGT      *ZEROS
079600021216     C                   MOVEL     V1CASI        D$GASI
079700021216     C                   MOVEL     'S'           D$GFSI
079800021216     C                   MOVEL     DSD$G         D$GXXX
079900021216     C                   ENDIF
080000000000     C*
080100000000     C* LANCIA PROGRAMMA
080200950405     C                   CALL      'TRECD6R'
080300000000     C                   PARM                    DS$ECO
080400000000     C                   MOVEL     'S'           LAND7R
080500950127     C*
080600950127     C     D$GERR        IFNE      *BLANKS
080700950127     C                   MOVEL     '1'           $ERR
080800950127     C                   MOVEL     D$GMSG        $MSG
080900950127     C                   EXSR      CARMSG
081000950127     C                   ENDIF
081100000000     C*
081200000000     C                   MOVEL     '1'           $TESVD
081300020517     C* RE-INIZIALIZZO A VIDEO I CAMPI DELLA CHIAVE BOLLA
081400020517     C                   Z-ADD     *ZEROS        V1CAAS
081500020517     C                   Z-ADD     *ZEROS        V1CAA2
081600020517     C                   Z-ADD     *ZEROS        V1CLNP
081700020517     C                   Z-ADD     *ZEROS        V1CLN2
081800020517     C                   Z-ADD     *ZEROS        V1CNRS
081900020517     C                   Z-ADD     *ZEROS        V1CNR2
082000020517     C                   Z-ADD     *ZEROS        V1CNSP
082100020517     C                   Z-ADD     *ZEROS        V1CNS2
082200020517     C                   MOVEL     *BLANKS       V1CTBL
082300020517     C                   MOVEL     *BLANKS       V1CTB2
082400020517     C                   MOVEL     '0'           FLGVIDP
082500020517     C                   MOVEL     '0'           FLGVIDD
082600000000     C*
082700000000     C                   ENDIF
082800000000     C*
082900000000     C                   ENDSR
083000020517     C*------------------------------------------------------------------------*
083100020517     C* F08V1  - INIZIALIZZA VIDEO X NUOVA BOLLA
083200020517     C*------------------------------------------------------------------------*
083300020517     C     F08V1         BEGSR
083400020517     C*
083500020517     C* RE-INIZIALIZZO A VIDEO I CAMPI DELLA CHIAVE BOLLA
083600020517     C                   Z-ADD     *ZEROS        V1CAAS
083700020517     C                   Z-ADD     *ZEROS        V1CAA2
083800020517     C                   Z-ADD     *ZEROS        V1CLNP
083900020517     C                   Z-ADD     *ZEROS        V1CLN2
084000020517     C                   Z-ADD     *ZEROS        V1CNRS
084100020517     C                   Z-ADD     *ZEROS        V1CNR2
084200020517     C                   Z-ADD     *ZEROS        V1CNSP
084300020517     C                   Z-ADD     *ZEROS        V1CNS2
084400020517     C                   MOVEL     *BLANKS       V1CTBL
084500020517     C                   MOVEL     *BLANKS       V1CTB2
084600020517     C                   MOVEL     '0'           FLGVIDP
084700020517     C                   MOVEL     '0'           FLGVIDD
084800020517     C*
084900020517     C                   EXSR      DECODI
085000020517     C*
085100020517     C                   ENDSR
085200000000     C*------------------------------------------------------------------------*
085300000000     C* DECODI - CONTROLLA CAMPI FORMATO VIDEO
085400000000     C*------------------------------------------------------------------------*
085500000000     C     DECODI        BEGSR
085600000000     C*
085700000000     C* REIMPOSTO INDICATORI DI ERRORE SUI CAMPI
085800000000     C                   SETOFF                                       515253
085900000000     C                   SETOFF                                       5455
086000000000     C                   SETOFF                                       616263
086100000000     C                   SETOFF                                       6465
086200020517     C*
086300020517     C* VERIFICA EVENTUALI CAMBIAMENTI SULLA CHIAVE
086400020517     C     V1CAAS        IFNE      DEPAASP
086500020517     C     V1CLNP        ORNE      DEPLNPP
086600020517     C     V1CNRS        ORNE      DEPNRSP
086700020517     C     V1CNSP        ORNE      DEPNSPP
086800020517     C     V1CTBL        ORNE      DEPTBLP
086900020517     C                   MOVEL     '0'           FLGVIDP
087000020517     C                   EVAL      DEPAASP = V1CAAS
087100020517     C                   EVAL      DEPLNPP = V1CLNP
087200020517     C                   EVAL      DEPNRSP = V1CNRS
087300020517     C                   EVAL      DEPNSPP = V1CNSP
087400020517     C                   EVAL      DEPTBLP = V1CTBL
087500020517     C                   ENDIF
087600020517     C     V1CAA2        IFNE      DEPAASD
087700020517     C     V1CLN2        ORNE      DEPLNPD
087800020517     C     V1CNR2        ORNE      DEPNRSD
087900020517     C     V1CNS2        ORNE      DEPNSPD
088000020517     C     V1CTB2        ORNE      DEPTBLD
088100020517     C                   MOVEL     '0'           FLGVIDD
088200020517     C                   EVAL      DEPAASD = V1CAA2
088300020517     C                   EVAL      DEPLNPD = V1CLN2
088400020517     C                   EVAL      DEPNRSD = V1CNR2
088500020517     C                   EVAL      DEPNSPD = V1CNS2
088600020517     C                   EVAL      DEPTBLD = V1CTB2
088700020517     C                   ENDIF
088800000000     C*
088900000000     C* AZZERA I TRACCIATI BOLLA -PRIMA- E -DOPO-
089000020517     C                   IF        FLGVIDP = '0'
089100000000     C                   EXSR      APDTAQ
089200020517     C                   ENDIF
089300020517     C                   IF        FLGVIDD = '0'
089400000000     C                   EXSR      ADDTAQ
089500020517     C                   ENDIF
089600000000     C                   MOVEL     D$PDS1        D$PRE1
089700000000     C                   MOVEL     D$PDS2        D$PRE2
089800000000     C                   MOVEL     D$DDS1        D$DRE1
089900000000     C                   MOVEL     D$DDS2        D$DRE2
090000000000     C***
090100000000     C*  BOLLA  -P R I M A-
090200000000     C***
090300000000IF  1C     $ERR          IFEQ      '0'
090400000000     C*
090500000000     C     V1CAAS        IFEQ      *ZEROS
090600000000     C     V1CLNP        ANDEQ     *ZEROS
090700000000     C     V1CNRS        ANDEQ     *ZEROS
090800000000     C     V1CNSP        ANDEQ     *ZEROS
090900000000     C     V1CTBL        ANDEQ     *BLANKS
091000000000     C                   ELSE
091100020530     C                   SETOFF                                       010203
091200000000     C                   Z-ADD     V1CAAS        KTAAAS
091300000000     C                   Z-ADD     V1CLNP        KTALNP
091400000000     C                   Z-ADD     V1CNRS        KTANRS
091500000000     C                   Z-ADD     V1CNSP        KTANSP
091600000000     C                   MOVEL     V1CTBL        KTATBL
091700990812     C     KEYTAS        CHAIN     TITAS30C                           99
091800000000IF  2C     *IN99         IFEQ      *ON
091900000000     C                   MOVEL     '1'           $ERR
092000000000     C                   MOVEL     MSG(11)       $MSG
092100000000     C                   EXSR      CARMSG
092200000000     C                   SETON                                        515253
092300000000     C                   SETON                                        5455
092400000000X   2C                   ELSE
092500020517     C                   IF        FLGVIDP = '0'
092600000000     C                   EXSR      CPDTAQ
092700020517     C                   ENDIF
092800000000     C                   MOVEL     D$PDS1        D$PRE1
092900000000     C                   MOVEL     D$PDS2        D$PRE2
093000000000E   2C                   ENDIF
093100000000     C                   ENDIF
093200000000E   1C                   ENDIF
093300000000     C***
093400000000     C* BOLLA   -D O P O-
093500000000     C***
093600000000IF  1C     $ERR          IFEQ      '0'
093700000000     C*
093800000000     C     V1CAA2        IFEQ      *ZEROS
093900000000     C     V1CLN2        ANDEQ     *ZEROS
094000000000     C     V1CNR2        ANDEQ     *ZEROS
094100000000     C     V1CNS2        ANDEQ     *ZEROS
094200000000     C     V1CTB2        ANDEQ     *BLANKS
094300000000     C     KEY           IFEQ      F6
094400000000     C                   MOVEL     '1'           $ERR
094500000000     C                   MOVEL     MSG(13)       $MSG
094600000000     C                   EXSR      CARMSG
094700000000     C                   SETON                                        616263
094800000000     C                   SETON                                        6465
094900000000     C                   ENDIF
095000000000     C                   ELSE
095100020530     C                   SETOFF                                       010203
095200000000     C                   Z-ADD     V1CAA2        KTAAAS
095300000000     C                   Z-ADD     V1CLN2        KTALNP
095400000000     C                   Z-ADD     V1CNR2        KTANRS
095500000000     C                   Z-ADD     V1CNS2        KTANSP
095600000000     C                   MOVEL     V1CTB2        KTATBL
095700990812     C     KEYTAS        CHAIN     TITAS30C                           99
095800000000IF  2C     *IN99         IFEQ      *ON
095900000000     C                   MOVEL     '1'           $ERR
096000000000     C                   MOVEL     MSG(11)       $MSG
096100000000     C                   EXSR      CARMSG
096200000000     C                   SETON                                        616263
096300000000     C                   SETON                                        6465
096400000000X   2C                   ELSE
096500020517     C                   IF        FLGVIDD = '0'
096600000000     C                   EXSR      CDDTAQ
096700020517     C                   ENDIF
096800950127     C     V1CAAS        IFNE      *ZEROS
096900950127     C                   Z-ADD     V1CAAS        D$DAAS
097000950127     C                   Z-ADD     V1CLNP        D$DLNP
097100950127     C                   Z-ADD     V1CNRS        D$DNRS
097200950127     C                   Z-ADD     V1CNSP        D$DNSP
097300950127     C                   MOVEL     V1CTBL        D$DTBL
097400950127     C                   ENDIF
097500000000     C                   MOVEL     D$DDS1        D$DRE1
097600000000     C                   MOVEL     D$DDS2        D$DRE2
097700000000E   2C                   ENDIF
097800000000     C                   ENDIF
097900000000E   1C                   ENDIF
098000000000     C*
098100000000     C* CONTROLLO -PRIMA- E -DOPO-
098200000000IF  1C     $ERR          IFEQ      '0'
098300000000     C     V1CAAS        IFEQ      *ZEROS
098400000000     C     V1CLNP        ANDEQ     *ZEROS
098500000000     C     V1CNRS        ANDEQ     *ZEROS
098600000000     C     V1CNSP        ANDEQ     *ZEROS
098700000000     C     V1CTBL        ANDEQ     *BLANKS
098800000000     C     V1CAA2        ANDEQ     *ZEROS
098900000000     C     V1CLN2        ANDEQ     *ZEROS
099000000000     C     V1CNR2        ANDEQ     *ZEROS
099100000000     C     V1CNS2        ANDEQ     *ZEROS
099200000000     C     V1CTB2        ANDEQ     *BLANKS
099300000000     C                   MOVEL     '1'           $ERR
099400000000     C                   MOVEL     MSG(12)       $MSG
099500000000     C                   EXSR      CARMSG
099600000000     C                   SETON                                        515253
099700000000     C                   SETON                                        5455
099800000000     C                   SETON                                        616263
099900000000     C                   SETON                                        6465
100000000000     C                   ENDIF
100100000000     C                   ENDIF
100200000000     C*
100300000000     C                   ENDSR
100400020510     C*------------------------------------------------------------------------*
100500020510     C* RDGES  - GESTISCE LA PRESSIONE DEL TASTO "PAGINA GIU'"
100600020510     C*------------------------------------------------------------------------*
100700020510     C     RDGES         BEGSR
100800020514     C*
100900020514     C                   MOVEL     '0'           $CHGVID
101000020514     C*
101100020517     C                   EXSR      DECODI
101200020517     C*
101300020517     C                   IF        $ERR = '0'
101400020514     C                   IF        WNAMEVID = *blanks
101500020510     C                   EVAL      WNAMEVID = 'EC78DV1'
101600020510     C                   ENDIF
101700020514     C                   IF        WNAMEVID = 'EC78DV1' AND
101800020514     C                             $CHGVID = '0'
101900020513     C                   EVAL      WNAMEVID = 'EC78DV2'
102000020514     C                   MOVEL     '1'           $CHGVID
102100020514     C                   ENDIF
102200020514     C                   IF        WNAMEVID = 'EC78DV2' AND
102300020514     C                             $CHGVID = '0'
102400020513     C                   EVAL      WNAMEVID = 'EC78DV3'
102500020514     C                   MOVEL     '1'           $CHGVID
102600020514     C                   ENDIF
102700020514     C                   IF        WNAMEVID = 'EC78DV3' AND
102800020514     C                             $CHGVID = '0'
102900020513     C                   EVAL      WNAMEVID = 'EC78DV4'
103000020514     C                   MOVEL     '1'           $CHGVID
103100020514     C                   ENDIF
103200020514     C                   IF        WNAMEVID = 'EC78DV4' AND
103300020514     C                             $CHGVID = '0'
103400020621     C                   EVAL      WNAMEVID = 'EC78DV5'
103500020514     C                   MOVEL     '1'           $CHGVID
103600020510     C                   ENDIF
103700020621     C                   IF        WNAMEVID = 'EC78DV5' AND
103800020621     C                             $CHGVID = '0'
103900020621     C                   EVAL      WNAMEVID = 'EC78DV6'
104000020621     C                   MOVEL     '1'           $CHGVID
104100020621     C                   ENDIF
104200020621     C                   IF        WNAMEVID = 'EC78DV6' AND
104300020621     C                             $CHGVID = '0'
104400020621     C                   EVAL      WNAMEVID = 'EC78DV7'
104500020621     C                   MOVEL     '1'           $CHGVID
104600020621     C                   ENDIF
104700020621     C                   IF        WNAMEVID = 'EC78DV7' AND
104800020621     C                             $CHGVID = '0'
104900020621     C                   EVAL      WNAMEVID = 'EC78DV8'
105000020621     C                   MOVEL     '1'           $CHGVID
105100020621     C                   ENDIF
105200020621     C                   IF        WNAMEVID = 'EC78DV8' AND
105300020621     C                             $CHGVID = '0'
105400020621     C                   EVAL      WNAMEVID = 'EC78DV1'
105500020621     C                   MOVEL     '1'           $CHGVID
105600020621     C                   ENDIF
105700020517     C                   ENDIF
105800020510     C*
105900020510     C                   ENDSR
106000020510     C*------------------------------------------------------------------------*
106100020510     C* RUGES  - GESTISCE LA PRESSIONE DEL TASTO "PAGINA SU'"
106200020510     C*------------------------------------------------------------------------*
106300020510     C     RUGES         BEGSR
106400020514     C*
106500020514     C                   MOVEL     '0'           $CHGVID
106600020517     C*
106700020517     C                   EXSR      DECODI
106800020510     C*
106900020517     C                   IF        $ERR = '0'
107000020510     C                   IF        WNAMEVID = *blanks
107100020510     C                   EVAL      WNAMEVID = 'EC78DV1'
107200020510     C                   ENDIF
107300020514     C                   IF        WNAMEVID = 'EC78DV1' AND
107400020514     C                             $CHGVID = '0'
107500020621     C                   EVAL      WNAMEVID = 'EC78DV8'
107600020514     C                   MOVEL     '1'           $CHGVID
107700020514     C                   ENDIF
107800020514     C                   IF        WNAMEVID = 'EC78DV2' AND
107900020514     C                             $CHGVID = '0'
108000020513     C                   EVAL      WNAMEVID = 'EC78DV1'
108100020514     C                   MOVEL     '1'           $CHGVID
108200020514     C                   ENDIF
108300020514     C                   IF        WNAMEVID = 'EC78DV3' AND
108400020514     C                             $CHGVID = '0'
108500020513     C                   EVAL      WNAMEVID = 'EC78DV2'
108600020514     C                   MOVEL     '1'           $CHGVID
108700020514     C                   ENDIF
108800020514     C                   IF        WNAMEVID = 'EC78DV4' AND
108900020514     C                             $CHGVID = '0'
109000020513     C                   EVAL      WNAMEVID = 'EC78DV3'
109100020514     C                   MOVEL     '1'           $CHGVID
109200020510     C                   ENDIF
109300020621     C                   IF        WNAMEVID = 'EC78DV5' AND
109400020621     C                             $CHGVID = '0'
109500020621     C                   EVAL      WNAMEVID = 'EC78DV4'
109600020621     C                   MOVEL     '1'           $CHGVID
109700020621     C                   ENDIF
109800020621     C                   IF        WNAMEVID = 'EC78DV6' AND
109900020621     C                             $CHGVID = '0'
110000020621     C                   EVAL      WNAMEVID = 'EC78DV5'
110100020621     C                   MOVEL     '1'           $CHGVID
110200020621     C                   ENDIF
110300020621     C                   IF        WNAMEVID = 'EC78DV7' AND
110400020621     C                             $CHGVID = '0'
110500020621     C                   EVAL      WNAMEVID = 'EC78DV6'
110600020621     C                   MOVEL     '1'           $CHGVID
110700020621     C                   ENDIF
110800020621     C                   IF        WNAMEVID = 'EC78DV8' AND
110900020621     C                             $CHGVID = '0'
111000020621     C                   EVAL      WNAMEVID = 'EC78DV7'
111100020621     C                   MOVEL     '1'           $CHGVID
111200020621     C                   ENDIF
111300020517     C                   ENDIF
111400020510     C*
111500020510     C                   ENDSR
111600020923     C*-------------------------------------------------------------------------
111700020923     C* REPERISCE I DATI UTENTE
111800020923     C*-------------------------------------------------------------------------
111900020923     C     REPDATIUTE    BEGSR
112000020923     C*
112100020923     C* INIZIALIZZA VARIABILI DI WRK
112200020923     C                   CLEAR                   TIBS34DS
112300020923     C                   CLEAR                   AZUTEDS
112400020923     C                   CLEAR                   DDATIUTE
112500020923     C*
112600020923     C     *DTAARA       DEFINE    §azute        azuteds
112700020923     C     *DTAARA       DEFINE    §datiute      ddatiute
112800020923     C                   IN(E)     *DTAARA
112900020923if  1C                   IF        %Error
113000020923     c                   EVAL      I34Tla = 'L'
113100020923     C                   CALL      'TIBS34R'
113200020923     C                   PARM                    Tibs34Ds
113300020923     C                   IN        *DTAARA
113400020923e   1C                   ENDIF
113500020923     C*
113600020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
113700020923     C                   MOVEL(P)  RSUT          DSFIRS           20
113800020923     C*
113900020923     C                   ENDSR
114000000000     C*------------------------------------------------------------------------*
114100000000     C* *INZSR - ROUTINE INIZIALE
114200000000     C*------------------------------------------------------------------------*
114300000000     C     *INZSR        BEGSR
114400000000     C*
114500000000     C* RICEVIMENTO PARAMETRI
114600000000     C     *ENTRY        PLIST
114700000000     C                   PARM                    KPJBA
114800000000     C*
114900000000     C* CAMPI RIFERITI AL D.B.
115000990812     C     *LIKE         DEFINE    TASAAS        KTAAAS
115100000000     C     *LIKE         DEFINE    TASLNP        KTALNP
115200000000     C     *LIKE         DEFINE    TASNRS        KTANRS
115300000000     C     *LIKE         DEFINE    TASNSP        KTANSP
115400000000     C     *LIKE         DEFINE    TASTBL        KTATBL
115500020517     C     *LIKE         DEFINE    TASAAS        DEPAASP
115600020517     C     *LIKE         DEFINE    TASLNP        DEPLNPP
115700020517     C     *LIKE         DEFINE    TASNRS        DEPNRSP
115800020517     C     *LIKE         DEFINE    TASNSP        DEPNSPP
115900020517     C     *LIKE         DEFINE    TASTBL        DEPTBLP
116000020517     C     *LIKE         DEFINE    TASAAS        DEPAASD
116100020517     C     *LIKE         DEFINE    TASLNP        DEPLNPD
116200020517     C     *LIKE         DEFINE    TASNRS        DEPNRSD
116300020517     C     *LIKE         DEFINE    TASNSP        DEPNSPD
116400020517     C     *LIKE         DEFINE    TASTBL        DEPTBLD
116500000000     C*--------------------
116600000000     C* K L I S T
116700000000     C*--------------------
116800990812     C* CHIAVE LETTURA TITAS30C - COMPLETA
116900000000     C     KEYTAS        KLIST
117000000000     C                   KFLD                    KTAAAS
117100000000     C                   KFLD                    KTALNP
117200000000     C                   KFLD                    KTANRS
117300000000     C                   KFLD                    KTANSP
117400000000     C                   KFLD                    KTATBL
117500020513     C*
117600020513     C* INIZIALIZZO LE VARIABILI DI WRK
117700020513     C                   EVAL      WNAMEVID = 'EC78DV1'
117800020510     C*
117900020513     C* INIZIALIZZO I FORMATI VIDEO
118000020513     C                   CLEAR                   EC78DV2
118100020513     C                   CLEAR                   EC78DV3
118200020513     C                   CLEAR                   EC78DV4
118300000000     C*
118400000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
118500000000     C                   EXSR      INZSTD
118600000000     C*
118700000000     C* IMPOSTO CAMPI TESTATA VIDEO
118800000000     C                   MOVEL     DSFIRS        VTDRSU
118900000000     C                   MOVEL     'N'           LAND7R            1
119000001206     C                   Z-ADD     *ZEROS        V1CASI
119100000000     C*
119200000000     C* AZZERA DATI BOLLA ORIGINALE
119300020517     C                   IF        FLGVIDD = '0'
119400000000     C                   EXSR      AODTAQ
119500020517     C                   ENDIF
119600000000     C                   MOVEL     D$ODS1        D$ORE1
119700000000     C                   MOVEL     D$ODS2        D$ORE2
119800000000     C*
119900000000     C* EMETTO FORMATO VIDEO
120000000000     C                   EXSR      CARV1
120100000000     C*
120200000000     C                   ENDSR
120300000000     C*------------------------------------------------------------------------*
120400000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
120500000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
120600000000     C*     LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
120700000000     C*     STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
120800000000     C*     NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
120900000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
121000000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
121100000000     C*------------------------------------------------------------------------*
121200000000     C* INZSTD - ROUTINE INIZIALE STANDARD
121300000000     C*------------------------------------------------------------------------*
121400000000     C     INZSTD        BEGSR
121500000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
121600000000     C                   CALL      'XKEYS'
121700000000     C                   PARM                    DSKEYS
121800000000     C                   MOVEA     KEYRIG        KEYS
121900000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
122000000000     C     XMSGPA        PLIST
122100000000     C                   PARM                    DSMSMS
122200000000     C                   PARM                    DSMSPG
122300000000     C                   PARM                    DSMSKE
122400000000     C                   PARM                    DSMSER
122500000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
122600000000     C                   MOVEL     'TREC78R'     DSMSPG
122700000000     C                   MOVEL     'TREC78R'     VMCQUE
122800000000     C                   MOVEL     *BLANKS       VMCKEY
122900000000     C                   MOVEL     *BLANKS       DSMSMS
123000000000     C* INIZIALIZZO FLAG CONTROLLO PGM
123100000000     C                   MOVEL     '0'           $ERR              1
123200000000     C                   MOVEL     '0'           $F03              1
123300000000     C                   MOVEL     *BLANKS       $MSG             80
123400000000     C                   MOVEL     '0'           $FINE             1
123500000000     C                   MOVEL     '1'           $TESVD            1
123600000000     C                   MOVEL     'I'           $TGES             1
123700020514     C                   MOVEL     '0'           $CHGVID           1
123800020517     C                   MOVEL     '0'           FLGVIDP           1
123900020517     C                   MOVEL     '0'           FLGVIDD           1
124000000000     C* LANCIO ROUTINE PER VERIFICARE SE
124100000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
124200000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
124300020923     C                   EXSR      REPDATIUTE
124400000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
124500000000     C                   BITOFF    '01234567'    POSMI1            1
124600000000     C                   BITOFF    '01234567'    POSMI2            1
124700000000     C                   BITOFF    '01234567'    POSMA1            1
124800000000     C                   BITOFF    '01234567'    POSMA2            1
124900000000     C                   BITON     '34'          POSMI1            1
125000000000     C                   BITON     '7'           POSMI2            1
125100000000     C                   BITON     '34'          POSMA1            1
125200000000     C                   BITON     '0'           POSMA2            1
125300000000     C                   MOVE      POSMI2        POSMIN            2
125400000000     C                   MOVEL     POSMI1        POSMIN
125500000000     C                   MOVE      POSMA2        POSMAX            2
125600000000     C                   MOVEL     POSMA1        POSMAX
125700000000     C                   ENDSR
125800000000     C*------------------------------------------------------------------------*
125900000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
126000000000     C*------------------------------------------------------------------------*
126100000000     C     FUNSTD        BEGSR
126200000000     C                   SELECT
126300000000     C     KEY           WHENEQ    F3
126400000000     C                   EXSR      F03STD
126500000000     C     KEY           WHENEQ    F12
126600000000     C                   EXSR      F12STD
126700000000     C     KEY           WHENEQ    F24
126800000000     C                   EXSR      F24STD
126900000000     C                   OTHER
127000000000     C                   MOVEL     MSG(2)        $MSG
127100000000     C                   EXSR      CARMSG
127200000000     C                   ENDSL
127300000000     C                   ENDSR
127400000000     C*------------------------------------------------------------------------*
127500000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
127600000000     C*------------------------------------------------------------------------*
127700000000     C     F03STD        BEGSR
127800000000     C*
127900000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
128000000000     C                   MOVEL     '1'           $FINE
128100000000     C*
128200000000     C                   ENDSR
128300000000     C*------------------------------------------------------------------------*
128400000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
128500000000     C*------------------------------------------------------------------------*
128600000000     C     F12STD        BEGSR
128700000000     C*
128800000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
128900000000     C                   MOVEL     '1'           $FINE
129000000000     C*
129100000000     C                   ENDSR
129200000000     C*------------------------------------------------------------------------*
129300000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
129400000000     C*------------------------------------------------------------------------*
129500000000     C     F24STD        BEGSR
129600000000     C*
129700000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
129800000000     C                   EXSR      OUTFUN
129900000000     C*
130000000000     C                   ENDSR
130100000000     C*------------------------------------------------------------------------*
130200000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
130300000000     C*          CODA MESSAGGI DEL PGM
130400000000     C*------------------------------------------------------------------------*
130500000000     C     CARMSG        BEGSR
130600000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
130700000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
130800000000     C     MSGULT        IFEQ      0
130900000000     C                   MOVEL     '1'           *IN40
131000000000     C                   WRITE     EC78DVK
131100000000     C                   MOVEL     '0'           *IN40
131200000000     C                   ENDIF
131300000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
131400000000     C                   MOVEL     $MSG          DSMSMS
131500000000     C                   CALL      'XMSG'        XMSGPA
131600000000     C     DSMSER        IFEQ      0
131700000000     C                   MOVEL     DSMSKE        VMCKEY
131800000000     C                   ENDIF
131900000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
132000000000     C                   ADD       1             MSGULT            4 0
132100000000     C                   WRITE     EC78DVM
132200000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
132300000000     C                   MOVEL     *BLANKS       $MSG
132400000000     C                   ENDSR
132500000000     C*------------------------------------------------------------------------*
132600000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
132700000000     C*------------------------------------------------------------------------*
132800000000     C     OUTMSG        BEGSR
132900000000     C* VISUALIZZAZIONE MESSAGGIO
133000000000     C                   MOVEL     '1'           *IN44
133100000000     C                   WRITE     EC78DVK
133200000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
133300000000     C                   Z-ADD     *ZEROS        MSGULT
133400000000     C                   ENDSR
133500000000     C*------------------------------------------------------------------------*
133600000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
133700000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
133800000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
133900000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
134000000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
134100000000     C*------------------------------------------------------------------------*
134200000000     C     CARFUN        BEGSR
134300000000     C                   Z-ADD     *ZEROS        FUNULT            5 0
134400000000     C     1             DO        31            I                 5 0
134500000000     C                   MOVEL     FUN(I)        FUNRIG
134600000000     C                   MOVEL     FUNFUN        FFU(I)
134700000000     C                   MOVEL     FUNDES        FDE(I)
134800020923     C     DUTLPO        IFEQ      'S'
134900000000     C     $TGES         IFEQ      'G'
135000000000     C                   MOVEL     FUNASG        FAT(I)
135100000000     C                   ELSE
135200000000     C                   MOVEL     FUNASI        FAT(I)
135300000000     C                   ENDIF
135400000000     C                   ELSE
135500000000     C     $TGES         IFEQ      'G'
135600000000     C                   MOVEL     FUNAFG        FAT(I)
135700000000     C                   ELSE
135800000000     C                   MOVEL     FUNAFI        FAT(I)
135900000000     C                   ENDIF
136000000000     C                   ENDIF
136100000000     C     FAT(I)        IFEQ      'E'
136200000000     C                   MOVEL     USER          WEDP              3
136300000000     C     WEDP          IFEQ      'EDP'
136400000000     C                   MOVEL     'S'           FAT(I)
136500000000     C                   ELSE
136600000000     C                   MOVEL     'N'           FAT(I)
136700000000     C                   ENDIF
136800000000     C                   ENDIF
136900000000     C                   ENDDO
137000000000     C                   ENDSR
137100000000     C*------------------------------------------------------------------------*
137200000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
137300000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
137400000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
137500000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
137600000000     C*------------------------------------------------------------------------*
137700000000     C     OUTFUN        BEGSR
137800000000     C     1             DO        8             I
137900000000     C                   MOVEL     *BLANKS       FUV(I)
138000000000     C                   ENDDO
138100000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
138200000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0
138300000000     C     1             DO        31            I
138400000000     C     FAT(I)        IFEQ      'S'
138500000000     C     FDE(I)        ANDNE     *BLANKS
138600000000     C                   ADD       1             FUNNAT
138700000000     C                   ENDIF
138800000000     C                   ENDDO
138900000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
139000000000     C     FUNNAT        IFGT      8
139100000000     C                   MOVEL     'S'           FAT(24)
139200000000     C                   ENDIF
139300000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
139400000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
139500000000     C                   Z-ADD     8             FUNVIS            5 0
139600000000     C     FAT(23)       IFEQ      'S'
139700000000     C                   SUB       1             FUNVIS
139800000000     C                   ENDIF
139900000000     C     FAT(24)       IFEQ      'S'
140000000000     C                   SUB       1             FUNVIS
140100000000     C                   ENDIF
140200000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
140300000000     C                   Z-ADD     *ZEROS        W                 5 0
140400000000     C                   ADD       1             FUNULT
140500000000     C     FUNULT        IFGT      31
140600000000     C                   Z-ADD     1             FUNULT
140700000000     C                   ENDIF
140800000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
140900000000     C     FUNULT        DO        31            I
141000000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
141100000000     C     W             IFLT      FUNVIS
141200000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
141300000000     C     FAT(I)        IFEQ      'S'
141400000000     C     FDE(I)        ANDNE     *BLANKS
141500000000     C     I             IFEQ      23
141600000000     C     I             OREQ      24
141700000000     C                   ELSE
141800000000     C                   ADD       1             W
141900000000     C                   MOVEL     FDE(I)        FUV(W)
142000000000     C                   ENDIF
142100000000     C                   ENDIF
142200000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
142300000000     C                   Z-ADD     I             FUNULT
142400000000     C                   ENDIF
142500000000     C                   ENDDO
142600000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
142700000000     C                   MOVEL     FUV(1)        VFD001
142800000000     C                   MOVEL     FUV(2)        VFD002
142900000000     C                   MOVEL     FUV(3)        VFD003
143000000000     C                   MOVEL     FUV(4)        VFD004
143100000000     C                   MOVEL     FUV(5)        VFD005
143200000000     C                   MOVEL     FUV(6)        VFD006
143300000000     C     FAT(23)       IFEQ      'N'
143400000000     C     FAT(24)       ANDEQ     'N'
143500000000     C                   MOVEL     FUV(7)        VFD007
143600001206     C                   MOVEL     FUV(8)        VFD008
143700000000     C                   ELSE
143800000000     C     FAT(23)       IFEQ      'S'
143900000000     C     FAT(24)       ANDEQ     'S'
144000000000     C                   MOVEL     FDE(23)       VFD007
144100000000     C                   MOVEL     FDE(24)       VFD008
144200000000     C                   ELSE
144300000000     C                   MOVEL     FUV(7)        VFD007
144400000000     C     FAT(23)       IFEQ      'S'
144500000000     C                   MOVEL     FDE(23)       VFD008
144600000000     C                   ELSE
144700000000     C                   MOVEL     FDE(24)       VFD008
144800000000     C                   ENDIF
144900000000     C                   ENDIF
145000000000     C                   ENDIF
145100000000     C                   MOVEL     '0'           $TESVD
145200000000     C                   ENDSR
145300000000     C*------------------------------------------------------------------------*
145400000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
145500000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
145600000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
145700000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
145800000000     C*------------------------------------------------------------------------*
145900000000     C     CONFUN        BEGSR
146000000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
146100000000     C     1             DO        31            I
146200000000     C     KEY           IFEQ      KEYS(I)
146300000000     C* CONTROLLO SE IL TASTO E' ATTIVO
146400000000     C     FAT(I)        IFNE      'S'
146500000000     C                   MOVEL     MSG(1)        $MSG
146600000000     C                   EXSR      CARMSG
146700000000     C                   MOVEL     '1'           $ERR
146800000000     C                   ENDIF
146900000000     C                   ENDIF
147000000000     C                   ENDDO
147100000000     C                   ENDSR
147200000000     C*------------------------------------------------------------------------*
147300990812     C* ROUTINE GENERALIZZATA PASSAGGIO IMMAGINE BOLLA DI TITAS30C
147400000000     C*------------------------------------------------------------------------*
147500000000      /COPY GAITRASRC/SRCTREC,TRECDT
147600000000     C*------------------------------------------------------------------------*
147700000000**   SCHIERA: MSG  (MESSAGGI)
147800000000TREC78R- Tasto funzionale non valido.                                          1
147900000000TREC78R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
148000000000TREC78R- Opzione non valida.                                                   3
148100000000TREC78R- Opzione abilitata ma non gestita. Avvertire CED.                      4
148200000000TREC78R- Inizio elenco.                                                        5
148300000000TREC78R- Fine elenco.                                                          6
148400000000TREC78R- Inserire l'anno di spedizione                                         7
148500000000TREC78R- Inserire la linea di partenza                                         8
148600000000TREC78R- Inserire il numero di spedizione                                      9
148700000000TREC78R- Inserire il tipo bolla                                               10
148800000000TREC78R- Bolla inesistente                                                    11
148900000000TREC78R- Inserire i dati di almeno una bolla.                                 12
149000000000TREC78R- Per la visualizzazione competenze mettere i dati bolla nel 'DOPO'.   13
149100000000TREC78R- 14
149200000000TREC78R- 15
149300000000TREC78R- 16
149400000000TREC78R- 17
149500000000TREC78R- 18
149600000000TREC78R- 19
149700000000TREC78R- 20
149800000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
149900000000F1    -                   -    NN     NN
150000000000F2    -                   -    NN     NN
150100000000F3    -F3=Fine            -    SS     SS
150200020517F4    -F4=Ripr. dati bolla-    EE     EE
150300000000F5    -                   -    NN     NN
150400950124F6    -F6=Visualizza      -    SS     SS
150500011213F7    -F7=Aggiorna        -    EE     EE
150600020517F8    -F8=Inizializza     -    SS     SS
150700000000F9    -                   -    NN     NN
150800000000F10   -                   -    NN     NN
150900020510F11   -                   -    NN     NN
151000000000F12   -F12=Ritorno        -    SS     SS
151100000000F13   -                   -    NN     NN
151200000000F14   -                   -    NN     NN
151300000000F15   -                   -    NN     NN
151400000000F16   -                   -    NN     NN
151500000000F17   -                   -    NN     NN
151600000000F18   -                   -    NN     NN
151700000000F19   -                   -    NN     NN
151800000000F20   -                   -    NN     NN
151900000000F21   -                   -    NN     NN
152000000000F22   -                   -    NN     NN
152100000000F23   -                   -    NN     NN
152200000000F24   -F24=Altri tasti    -    NN     NN
152300000000ENTER -                   -    SS     SS
152400020510ROLLUP-Pagina precedente  -    EE     EE
152500020510ROLLDN-Pagina successiva  -    EE     EE
152600000000HELP  -                   -    NN     NN
152700000000PRINT -                   -    NN     NN
152800000000BSPACE-                   -    NN     NN
152900000000CLEAR -                   -    NN     NN
