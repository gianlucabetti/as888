000100000000     H*------------------------------------------------------------------------*
000200000000     H* AZZERAMENTO FILES DI C/E PER RICALCOLO COMPETENZE - PARAMETRI
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800000000     FTRECE6D   CF   E             WORKSTN INFDS(DEVDS1)
000900000000     F                                     SFILE(ECE6DVM:MSGULT)
001000000000     D*------------------------------------------------------------------------*
001100000000     D* SCHIERE
001200000000     D*------------------------------------------------------------------------*
001300000000     D*-------------------
001400000000     D* SCHIERE A TEMPO DI COMPILAZIONE  PER VIDEO
001500000000     D*-------------------
001600000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
001700000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)
001800000000     D*-------------------
001900000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002000000000     D*-------------------
002100000000     D FFU             S              6    DIM(31)
002200000000     D FDE             S             19    DIM(31)
002300000000     D FAT             S              1    DIM(31)
002400000000     D*-------------------
002500000000     D* TABELLA DESCRIZIONI A VIDEO
002600000000     D*-------------------
002700000000     D FUV             S             19    DIM(8)
002800000000     D*-------------------
002900000000     D* TABELLA TASTI FUNZIONALI PREMUTI
003000000000     D*-------------------
003100000000     D KEYS            S              1    DIM(40)
003200000000     D*------------------------------------------------------------------------*
003300000000     D* INPUT
003400000000     D*------------------------------------------------------------------------*
003500000000     D*-------------------
003600000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
003700000000     D*-------------------
003800000000     D DSFUN         E DS
003900000000     D  FUNRIG                 1     40
004000000000     D*-------------------
004100000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
004200000000     D*-------------------
004300000000     D DSMSG         E DS
004400000000     D*-------------------
004500000000     D* DS PER GESTIONE TASTI FUNZIONALI
004600000000     D*-------------------
004700000000     D DSKEYS        E DS
004800000000     D  KEYRIG                 1     40
004900000000     D*-------------------
005000000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
005100000000     D*-------------------
005200000000     D                SDS
005300000000     D  USER                 254    263
005400000000     D*-------------------
005500000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
005600000000     D*-------------------
005700000000     D DEVDS1          DS
005800000000     D  KEY                  369    369
005900000000     D  CURSOR               370    371
006000000000     D*-------------------
006100020923     D* DS REPERIMENTO DATI UTENTE
006200020923     D*-------------------
006300020923     D TIBS34DS      E DS
006400020923     D DDATIUTE      E DS
006500020923     D AZUTEDS       E DS                  extname(AZUTE00F)
006600990610     D*------------------
006700990610     D* DS CONTROLLO DATA (8)
006800990610     D*------------------
006900990610     D WLBDA8          DS                  INZ
007000990610     D  G08DAT                 1      8  0
007100990610     D  G08INV                 9     16  0
007200990610     D  G08AAV                 9     12  0
007300990610     D  G08MMV                13     14  0
007400990610     D  G08GGV                15     16  0
007500990610     D  G08ERR                17     17
007600990610     D  G08TGI                18     22  0
007700030123     D*-------------------
007800030123     D* DS COMPOSIZIONE DATA
007900030123     D*-------------------
008000030123     D                 DS
008100030123     D  DSANN                  1      4  0
008200030123     D  DSMES                  5      6  0
008300030123     D  DSGIO                  7      8  0
008400030123     D  DSDATA                 1      8  0
008500000000     D*-------------------
008600000000     D* PARAMETRI LANCIO
008700000000     D*-------------------
008800000000     D PARAM           DS                  INZ
008900990610     D  PARANN                 1      4  0
009000990610     D  PARMMI                 5      6  0
009100990610     D  PARMMF                 7      8  0
009200990610     D  PARSDO                 9      9
009300990610     D  PARSDC                10     10
009400990610     D  PARSDA                11     11
009500990610     D  PARSDP                12     12
009600990610     D  PARSDF                13     13
009700990610     D  PARSDR                14     14
009800990610     D  PARSDT                15     15
009900990610     D  PARWFM                16     16
010000990610     D  PARCET                17     17
010100990610     D  PARCED                18     18
010200011127     D  PARFIM                19     19
010300030123     D  PARCOP                20     20
010301040316     D  PARSDE                21     21
010400000000     D*-------------------
010500000000     D* ARCHITETTURA
010600000000     D*-------------------
010700000000     D KPJBA         E DS
010800000000     D*-------------------
010900000000     D* COSTANTI
011000000000     D*-------------------
011100000000     D LIB201          C                   CONST('GAITRA201 ')
011200000000     C*------------------------------------------------------------------------*
011300000000     C* MAIN LINE
011400000000     C*------------------------------------------------------------------------*
011500000000     C*
011600000000     C* CICLO FINO A CHE:
011700000000DO  1C     $FINE         DOUEQ     '1'
011800000000     C*
011900000000     C                   MOVEL     '0'           $ERR
012000000000     C*
012100000000     C* CONTROLLA GESTIONE VIDEO
012200000000IF  2C     $GES          IFEQ      'V1'
012300000000     C                   EXSR      GESV1
012400000000E   2C                   ENDIF
012500000000     C*
012600000000E   1C                   ENDDO
012700000000     C*
012800000000     C                   SETON                                        LR
012900000000     C*------------------------------------------------------------------------*
013000000000     C* GESV1 - GESTIONE PRIMO VIDEO
013100000000     C*------------------------------------------------------------------------*
013200000000     C     GESV1         BEGSR
013300000000     C*
013400000000IF  1C     $TESVD        IFEQ      '1'
013500000000     C                   EXSR      CARV1
013600000000E   1C                   ENDIF
013700000000     C*
013800000000IF  1C     MSGULT        IFEQ      *ZEROS
013900000000     C                   MOVEL     *BLANKS       $MSG
014000000000     C                   EXSR      CARMSG
014100000000E   1C                   ENDIF
014200000000     C                   EXSR      OUTMSG
014300000000     C*
014400000000     C                   EXFMT     ECE6DV1
014500000000     C*
014600000000     C                   EXSR      CONFUN
014700000000     C*
014800000000IF  1C     $ERR          IFEQ      '0'
014900000000SL  2C                   SELECT
015000000000     C     KEY           WHENEQ    ENTER
015100000000     C                   EXSR      ENTV1
015200000000     C     KEY           WHENEQ    F6
015300000000     C                   EXSR      F06GES
015400000000     C                   OTHER
015500000000     C                   EXSR      FUNSTD
015600000000E   2C                   ENDSL
015700000000E   1C                   ENDIF
015800000000     C*
015900000000     C                   ENDSR
016000000000     C*------------------------------------------------------------------------*
016100000000     C* CARV1 - CARICA PRIMO VIDEO
016200000000     C*------------------------------------------------------------------------*
016300000000     C     CARV1         BEGSR
016400000000     C*
016500000000     C                   MOVEL     'V1'          $GES
016600000000     C*
016700000000     C                   EXSR      CARFUN
016800000000     C                   EXSR      OUTFUN
016900000000     C*
017000000000     C                   WRITE     ECE6DVT
017100000000     C                   WRITE     ECE6DVF
017200000000     C*
017300000000     C                   ENDSR
017400000000     C*------------------------------------------------------------------------*
017500000000     C* ENTV1 - GESTIONE TASTO ENTER PRIMO VIDEO
017600000000     C*------------------------------------------------------------------------*
017700000000     C     ENTV1         BEGSR
017800000000     C*
017900000000     C* CONTROLLA I CAMPI IMMESSI
018000000000     C                   EXSR      DECODI
018100000000     C*
018200000000     C                   ENDSR
018300000000     C*------------------------------------------------------------------------*
018400000000     C* F06GES - GEST. TASTO FUNZ. F06 ==> CONFERMA LANCIO
018500000000     C*------------------------------------------------------------------------*
018600000000     C     F06GES        BEGSR
018700000000     C*
018800000000     C                   EXSR      DECODI
018900000000IF  1C     $ERR          IFEQ      '0'
019000000000     C                   Z-ADD     V1CANN        PARANN
019100000000     C                   Z-ADD     V1CMMI        PARMMI
019200000000     C                   Z-ADD     V1CMMF        PARMMF
019300000000     C                   MOVEL     V1CSDO        PARSDO
019400000000     C                   MOVEL     V1CSDC        PARSDC
019500000000     C                   MOVEL     V1CSDA        PARSDA
019600000000     C                   MOVEL     V1CSDP        PARSDP
019700000000     C                   MOVEL     V1CSDF        PARSDF
019800000000     C                   MOVEL     V1CSDR        PARSDR
019900000000     C                   MOVEL     V1CSDT        PARSDT
020000000000     C                   MOVEL     V1CWFM        PARWFM
020100000000     C                   MOVEL     V1CCET        PARCET
020200000000     C                   MOVEL     V1CCED        PARCED
020300990219     C                   MOVEL     V1CFIM        PARFIM
020400030123     C                   MOVEL     V1CCOP        PARCOP
020401040316     C                   MOVEL     V1CSDE        PARSDE
020500000000     C                   MOVEL     PARAM         KPJBU
020600000000     C                   MOVEL     'ECE7'        KCOAZ
020700000000     C                   CALL      'BCH10'
020800000000     C                   PARM                    KPJBA
020900000000E   1C                   ENDIF
021000000000     C*
021100000000     C                   ENDSR
021200000000     C*------------------------------------------------------------------------*
021300000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
021400000000     C*------------------------------------------------------------------------*
021500000000     C     DECODI        BEGSR
021600000000     C*
021700000000     C* REIMPOSTA INIDICATORI
021800000000     C                   SETOFF                                       606162
021900000000     C                   SETOFF                                       636465
022000000000     C                   SETOFF                                       666768
022100000000     C                   SETOFF                                       697071
022200030123     C                   SETOFF                                       727475
022201040316     C                   SETOFF                                       76
022300000000     C*
022400000000DO  1C                   DO
022500000000     C*---
022600000000     C* ANNO
022700000000     C*---
022800000000IF  2C     V1CANN        IFLE      *ZEROS
022900000000     C                   MOVEL     '1'           $ERR
023000000000     C                   MOVEL     MSG(10)       $MSG
023100000000     C                   EXSR      CARMSG
023200000000     C                   SETON                                        60
023300000000     C                   LEAVE
023400000000E   2C                   ENDIF
023500000000     C*---
023600000000     C* MESE INIZIALE
023700000000     C*---
023800990610     C                   Z-ADD     V1CANN        G08AAV
023900990610     C                   Z-ADD     V1CMMI        G08MMV
024000990610     C                   Z-ADD     01            G08GGV
024100990610     C                   Z-ADD     *ZEROS        G08DAT
024200990610     C                   MOVEL     '3'           G08ERR
024300990610     C                   CALL      'XSRDA8'
024400990610     C                   PARM                    WLBDA8
024500990610IF  2C     G08ERR        IFEQ      '1'
024600000000     C                   MOVEL     '1'           $ERR
024700000000     C                   MOVEL     MSG(11)       $MSG
024800000000     C                   EXSR      CARMSG
024900000000     C                   SETON                                        61
025000000000     C                   LEAVE
025100000000E   2C                   ENDIF
025200000000     C*---
025300000000     C* MESE FINALE
025400000000     C*---
025500990610     C                   Z-ADD     V1CANN        G08AAV
025600990610     C                   Z-ADD     V1CMMF        G08MMV
025700990610     C                   Z-ADD     01            G08GGV
025800990610     C                   Z-ADD     *ZEROS        G08DAT
025900990610     C                   MOVEL     '3'           G08ERR
026000990610     C                   CALL      'XSRDA8'
026100990610     C                   PARM                    WLBDA8
026200990610IF  2C     G08ERR        IFEQ      '1'
026300000000     C                   MOVEL     '1'           $ERR
026400000000     C                   MOVEL     MSG(11)       $MSG
026500000000     C                   EXSR      CARMSG
026600000000     C                   SETON                                        62
026700000000     C                   LEAVE
026800000000E   2C                   ENDIF
026900000000     C*---
027000000000     C* MESE INIZIALE + FINALE
027100000000     C*---
027200000000IF  2C     V1CMMF        IFLT      V1CMMI
027300000000     C                   MOVEL     '1'           $ERR
027400000000     C                   MOVEL     MSG(12)       $MSG
027500000000     C                   EXSR      CARMSG
027600000000     C                   SETON                                        6162
027700000000     C                   LEAVE
027800000000E   2C                   ENDIF
027900000000     C*---
028000000000     C* AZZERA SLSDO
028100000000     C*---
028200000000IF  2C     V1CSDO        IFNE      'S'
028300000000     C     V1CSDO        ANDNE     *BLANKS
028400000000     C                   MOVEL     '1'           $ERR
028500000000     C                   MOVEL     MSG(13)       $MSG
028600000000     C                   EXSR      CARMSG
028700000000     C                   SETON                                        63
028800000000     C                   LEAVE
028900000000E   2C                   ENDIF
029000000000     C*---
029100000000     C* AZZERA SLSDC
029200000000     C*---
029300000000IF  2C     V1CSDC        IFNE      'S'
029400000000     C     V1CSDC        ANDNE     *BLANKS
029500000000     C                   MOVEL     '1'           $ERR
029600000000     C                   MOVEL     MSG(13)       $MSG
029700000000     C                   EXSR      CARMSG
029800000000     C                   SETON                                        64
029900000000     C                   LEAVE
030000000000E   2C                   ENDIF
030100000000     C*---
030200000000     C* AZZERA SLSDA
030300000000     C*---
030400000000IF  2C     V1CSDA        IFNE      'S'
030500000000     C     V1CSDA        ANDNE     *BLANKS
030600000000     C                   MOVEL     '1'           $ERR
030700000000     C                   MOVEL     MSG(13)       $MSG
030800000000     C                   EXSR      CARMSG
030900000000     C                   SETON                                        65
031000000000     C                   LEAVE
031100000000E   2C                   ENDIF
031200000000     C*---
031300000000     C* AZZERA SLSDP
031400000000     C*---
031500000000IF  2C     V1CSDP        IFNE      'S'
031600000000     C     V1CSDP        ANDNE     *BLANKS
031700000000     C                   MOVEL     '1'           $ERR
031800000000     C                   MOVEL     MSG(13)       $MSG
031900000000     C                   EXSR      CARMSG
032000000000     C                   SETON                                        66
032100000000     C                   LEAVE
032200000000E   2C                   ENDIF
032300000000     C*---
032400000000     C* AZZERA SLSDF
032500000000     C*---
032600000000IF  2C     V1CSDF        IFNE      'S'
032700000000     C     V1CSDF        ANDNE     *BLANKS
032800000000     C                   MOVEL     '1'           $ERR
032900000000     C                   MOVEL     MSG(13)       $MSG
033000000000     C                   EXSR      CARMSG
033100000000     C                   SETON                                        67
033200000000     C                   LEAVE
033300000000E   2C                   ENDIF
033400000000     C*---
033500000000     C* AZZERA SLSDR
033600000000     C*---
033700000000IF  2C     V1CSDR        IFNE      'S'
033800000000     C     V1CSDR        ANDNE     *BLANKS
033900000000     C                   MOVEL     '1'           $ERR
034000000000     C                   MOVEL     MSG(13)       $MSG
034100000000     C                   EXSR      CARMSG
034200000000     C                   SETON                                        68
034300000000     C                   LEAVE
034400000000E   2C                   ENDIF
034500000000     C*---
034600000000     C* AZZERA SLSDT
034700000000     C*---
034800000000IF  2C     V1CSDT        IFNE      'S'
034900000000     C     V1CSDT        ANDNE     *BLANKS
035000000000     C                   MOVEL     '1'           $ERR
035100000000     C                   MOVEL     MSG(13)       $MSG
035200000000     C                   EXSR      CARMSG
035300000000     C                   SETON                                        69
035400000000     C                   LEAVE
035500000000E   2C                   ENDIF
035501040316     C*---
035502040316     C* AZZERA SISDE
035503040316     C*---
035504040316IF  2C     V1CSDE        IFNE      'S'
035505040316     C     V1CSDE        ANDNE     *BLANKS
035506040316     C                   MOVEL     '1'           $ERR
035507040316     C                   MOVEL     MSG(13)       $MSG
035508040316     C                   EXSR      CARMSG
035509040316     C                   SETON                                        76
035510040316     C                   LEAVE
035511040316E   2C                   ENDIF
035600000000     C*---
035700000000     C* CANCELLA WFMOV
035800000000     C*---
035900000000IF  2C     V1CWFM        IFNE      'S'
036000000000     C     V1CWFM        ANDNE     *BLANKS
036100000000     C                   MOVEL     '1'           $ERR
036200000000     C                   MOVEL     MSG(13)       $MSG
036300000000     C                   EXSR      CARMSG
036400000000     C                   SETON                                        70
036500000000     C                   LEAVE
036600000000E   2C                   ENDIF
036700000000     C*---
036800000000     C* CANCELLA ECCET
036900000000     C*---
037000000000IF  2C     V1CCET        IFNE      'S'
037100000000     C     V1CCET        ANDNE     *BLANKS
037200000000     C                   MOVEL     '1'           $ERR
037300000000     C                   MOVEL     MSG(13)       $MSG
037400000000     C                   EXSR      CARMSG
037500000000     C                   SETON                                        71
037600000000     C                   LEAVE
037700000000E   2C                   ENDIF
037800000000     C*---
037900000000     C* CANCELLA ECCED
038000000000     C*---
038100000000IF  2C     V1CCED        IFNE      'S'
038200000000     C     V1CCED        ANDNE     *BLANKS
038300000000     C                   MOVEL     '1'           $ERR
038400000000     C                   MOVEL     MSG(13)       $MSG
038500000000     C                   EXSR      CARMSG
038600000000     C                   SETON                                        72
038700000000     C                   LEAVE
038800000000E   2C                   ENDIF
038900990219     C*---
039000990219     C* CANCELLA ECFIM
039100990219     C*---
039200990219IF  2C     V1CFIM        IFNE      'S'
039300990219     C     V1CFIM        ANDNE     *BLANKS
039400990219     C                   MOVEL     '1'           $ERR
039500990219     C                   MOVEL     MSG(13)       $MSG
039600990219     C                   EXSR      CARMSG
039700990219     C                   SETON                                        74
039800990219     C                   LEAVE
039900990219E   2C                   ENDIF
040000030123     C*---
040100030123     C* CANCELLA SICOP E ECCEV
040200030123     C*---
040300030123IF  2C     V1CCOP        IFNE      'S'
040400030123     C     V1CCOP        ANDNE     *BLANKS
040500030123     C                   MOVEL     '1'           $ERR
040600030123     C                   MOVEL     MSG(13)       $MSG
040700030123     C                   EXSR      CARMSG
040800030123     C                   SETON                                        75
040900030123     C                   LEAVE
041000030123E   2C                   ENDIF
041100000000     C*---
041200000000     C* CONTROLLO PER MENU' DI PROVA
041300000000     C*---
041400020514IF  2C*          KNSIF     IFNE LIB201
041500020514     C*                    MOVEL'1'       $ERR
041600020514     C*                    MOVELMSG,20    $MSG
041700020514     C*                    EXSR CARMSG
041800020514     C*                    LEAVE
041900020514E   2C*                    ENDIF
042000000000E   1C                   ENDDO
042100000000     C*
042200000000     C                   ENDSR
042300000000     C*------------------------------------------------------------------------*
042400000000     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
042500000000     C*------------------------------------------------------------------------*
042600000000     C     IMPVID        BEGSR
042700000000     C*
042800000000     C* SELEZIONI FILES
042900000000     C                   MOVEL     'S'           V1CSDO
043000000000     C                   MOVEL     'S'           V1CSDC
043100000000     C                   MOVEL     'S'           V1CSDA
043200000000     C                   MOVEL     'S'           V1CSDP
043300000000     C                   MOVEL     'S'           V1CSDF
043400000000     C                   MOVEL     'S'           V1CSDR
043500000000     C                   MOVEL     'S'           V1CSDT
043501040316     C                   MOVEL     'S'           V1CSDE
043600000000     C                   MOVEL     'S'           V1CWFM
043700000000     C                   MOVEL     'S'           V1CCET
043800000000     C                   MOVEL     'S'           V1CCED
043900990219     C                   MOVEL     'S'           V1CFIM
044000030123     C                   MOVEL     'S'           V1CCOP
044100000000     C*
044200000000     C                   ENDSR
044300020923     C*-------------------------------------------------------------------------
044400020923     C* REPERISCE I DATI UTENTE
044500020923     C*-------------------------------------------------------------------------
044600020923     C     REPDATIUTE    BEGSR
044700020923     C*
044800020923     C* INIZIALIZZA VARIABILI DI WRK
044900020923     C                   CLEAR                   TIBS34DS
045000020923     C                   CLEAR                   AZUTEDS
045100020923     C                   CLEAR                   DDATIUTE
045200020923     C*
045300020923     C     *DTAARA       DEFINE    §azute        azuteds
045400020923     C     *DTAARA       DEFINE    §datiute      ddatiute
045500020923     C                   IN(E)     *DTAARA
045600020923if  1C                   IF        %Error
045700020923     c                   EVAL      I34Tla = 'L'
045800020923     C                   CALL      'TIBS34R'
045900020923     C                   PARM                    Tibs34Ds
046000020923     C                   IN        *DTAARA
046100020923e   1C                   ENDIF
046200020923     C*
046300020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
046400020923     C                   MOVEL(P)  RSUT          DSFIRS           20
046500020923     C*
046600020923     C                   ENDSR
046700000000     C*------------------------------------------------------------------------*
046800000000     C* *INZSR  - ROUTINE INIZIALE
046900000000     C*------------------------------------------------------------------------*
047000000000     C     *INZSR        BEGSR
047100000000     C*
047200000000     C* RICEVIMENTO PARAMETRI
047300000000     C     *ENTRY        PLIST
047400000000     C                   PARM                    KPJBA
047500000000     C*
047600000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
047700000000     C                   EXSR      INZSTD
047800000000     C*
047900000000     C* IMPOSTAZIONI INIZIALI VIDOE
048000000000     C                   EXSR      IMPVID
048100000000     C*
048200000000     C* EMETTE FORMATO VIDEO
048300000000     C                   EXSR      CARV1
048400000000     C*
048500000000     C                   ENDSR
048600000000     C*------------------------------------------------------------------------*
048700000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
048800000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
048900000000     C*      LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
049000000000     C*      STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
049100000000     C*      NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
049200000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
049300000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
049400000000     C*------------------------------------------------------------------------*
049500000000     C* INZSTD - ROUTINE INIZIALE STANDARD
049600000000     C*------------------------------------------------------------------------*
049700000000     C     INZSTD        BEGSR
049800000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
049900000000     C                   CALL      'XKEYS'
050000000000     C                   PARM                    DSKEYS
050100000000     C                   MOVEA     KEYRIG        KEYS
050200000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
050300000000     C     XMSGPA        PLIST
050400000000     C                   PARM                    DSMSMS
050500000000     C                   PARM                    DSMSPG
050600000000     C                   PARM                    DSMSKE
050700000000     C                   PARM                    DSMSER
050800000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
050900000000     C                   MOVEL     'TRECE6R'     DSMSPG
051000000000     C                   MOVEL     'TRECE6R'     VMCQUE
051100000000     C                   MOVEL     *BLANKS       VMCKEY
051200000000     C                   MOVEL     *BLANKS       DSMSMS
051300000000     C* INIZIALIZZO FLAG CONTROLLO PGM
051400000000     C                   MOVEL     '0'           $ERR              1
051500000000     C                   MOVEL     '0'           $F03              1
051600000000     C                   MOVEL     *BLANKS       $GES              2
051700000000     C                   MOVEL     *BLANKS       $MSG             80
051800000000     C                   MOVEL     '0'           $FINE             1
051900000000     C                   MOVEL     '1'           $TESVD            1
052000000000     C                   MOVEL     'I'           $TGES             1
052100000000     C* LANCIO ROUTINE PER VERIFICARE SE
052200000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
052300000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
052400020923     C                   EXSR      REPDATIUTE
052500000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
052600000000     C                   BITOFF    '01234567'    POSMI1            1
052700000000     C                   BITOFF    '01234567'    POSMI2            1
052800000000     C                   BITOFF    '01234567'    POSMA1            1
052900000000     C                   BITOFF    '01234567'    POSMA2            1
053000000000     C                   BITON     '34'          POSMI1            1
053100000000     C                   BITON     '7'           POSMI2            1
053200000000     C                   BITON     '34'          POSMA1            1
053300000000     C                   BITON     '0'           POSMA2            1
053400000000     C                   MOVE      POSMI2        POSMIN            2
053500000000     C                   MOVEL     POSMI1        POSMIN
053600000000     C                   MOVE      POSMA2        POSMAX            2
053700000000     C                   MOVEL     POSMA1        POSMAX
053800000000     C                   ENDSR
053900000000     C*------------------------------------------------------------------------*
054000000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
054100000000     C*------------------------------------------------------------------------*
054200000000     C     FUNSTD        BEGSR
054300000000     C                   SELECT
054400000000     C     KEY           WHENEQ    F3
054500000000     C                   EXSR      F03STD
054600000000     C     KEY           WHENEQ    F12
054700000000     C                   EXSR      F12STD
054800000000     C     KEY           WHENEQ    F24
054900000000     C                   EXSR      F24STD
055000000000     C                   OTHER
055100000000     C                   MOVEL     MSG(2)        $MSG
055200000000     C                   EXSR      CARMSG
055300000000     C                   ENDSL
055400000000     C                   ENDSR
055500000000     C*------------------------------------------------------------------------*
055600000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
055700000000     C*------------------------------------------------------------------------*
055800000000     C     F03STD        BEGSR
055900000000     C*
056000000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
056100000000     C                   MOVEL     '1'           $FINE
056200000000     C*
056300000000     C                   ENDSR
056400000000     C*------------------------------------------------------------------------*
056500000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
056600000000     C*------------------------------------------------------------------------*
056700000000     C     F12STD        BEGSR
056800000000     C*
056900000000     C* IMPOSTO FLAG DI FIN PGM
057000000000     C                   MOVEL     '1'           $FINE
057100000000     C*
057200000000     C                   ENDSR
057300000000     C*------------------------------------------------------------------------*
057400000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
057500000000     C*------------------------------------------------------------------------*
057600000000     C     F24STD        BEGSR
057700000000     C*
057800000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
057900000000     C                   EXSR      OUTFUN
058000000000     C*
058100000000     C                   ENDSR
058200000000     C*------------------------------------------------------------------------*
058300000000     C* CARMSG - CARICA MSG NEL SUBFILE MSG E LO INVIA ALLA CODA MSG DEL PGM
058400000000     C*------------------------------------------------------------------------*
058500000000     C     CARMSG        BEGSR
058600000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
058700000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
058800000000     C     MSGULT        IFEQ      0
058900000000     C                   MOVEL     '1'           *IN40
059000000000     C                   WRITE     ECE6DVK
059100000000     C                   MOVEL     '0'           *IN40
059200000000     C                   ENDIF
059300000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
059400000000     C                   MOVEL     $MSG          DSMSMS
059500000000     C                   CALL      'XMSG'        XMSGPA
059600000000     C     DSMSER        IFEQ      0
059700000000     C                   MOVEL     DSMSKE        VMCKEY
059800000000     C                   ENDIF
059900000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
060000000000     C                   ADD       1             MSGULT            4 0
060100000000     C                   WRITE     ECE6DVM
060200000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
060300000000     C                   MOVEL     *BLANKS       $MSG
060400000000     C                   ENDSR
060500000000     C*------------------------------------------------------------------------*
060600000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
060700000000     C*------------------------------------------------------------------------*
060800000000     C     OUTMSG        BEGSR
060900000000     C* VISUALIZZAZIONE MESSAGGIO
061000000000     C                   MOVEL     '1'           *IN44
061100000000     C                   WRITE     ECE6DVK
061200000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
061300000000     C                   Z-ADD     *ZEROS        MSGULT
061400000000     C                   ENDSR
061500000000     C*------------------------------------------------------------------------*
061600000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
061700000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
061800000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
061900000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
062000000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
062100000000     C*------------------------------------------------------------------------*
062200000000     C     CARFUN        BEGSR
062300000000     C                   Z-ADD     *ZEROS        FUNULT            5 0
062400000000     C     1             DO        31            I                 5 0
062500000000     C                   MOVEL     FUN(I)        FUNRIG
062600000000     C                   MOVEL     FUNFUN        FFU(I)
062700000000     C                   MOVEL     FUNDES        FDE(I)
062800020923     C     DUTLPO        IFEQ      'S'
062900000000     C     $TGES         IFEQ      'G'
063000000000     C                   MOVEL     FUNASG        FAT(I)
063100000000     C                   ELSE
063200000000     C                   MOVEL     FUNASI        FAT(I)
063300000000     C                   ENDIF
063400000000     C                   ELSE
063500000000     C     $TGES         IFEQ      'G'
063600000000     C                   MOVEL     FUNAFG        FAT(I)
063700000000     C                   ELSE
063800000000     C                   MOVEL     FUNAFI        FAT(I)
063900000000     C                   ENDIF
064000000000     C                   ENDIF
064100000000     C     FAT(I)        IFEQ      'E'
064200000000     C                   MOVEL     USER          WEDP              3
064300000000     C     WEDP          IFEQ      'EDP'
064400000000     C                   MOVEL     'S'           FAT(I)
064500000000     C                   ELSE
064600000000     C                   MOVEL     'N'           FAT(I)
064700000000     C                   ENDIF
064800000000     C                   ENDIF
064900000000     C                   ENDDO
065000000000     C                   ENDSR
065100000000     C*------------------------------------------------------------------------*
065200000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
065300000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
065400000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
065500000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
065600000000     C*------------------------------------------------------------------------*
065700000000     C     OUTFUN        BEGSR
065800000000     C     1             DO        8             I
065900000000     C                   MOVEL     *BLANKS       FUV(I)
066000000000     C                   ENDDO
066100000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
066200000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0
066300000000     C     1             DO        31            I
066400000000     C     FAT(I)        IFEQ      'S'
066500000000     C     FDE(I)        ANDNE     *BLANKS
066600000000     C                   ADD       1             FUNNAT
066700000000     C                   ENDIF
066800000000     C                   ENDDO
066900000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
067000000000     C     FUNNAT        IFGT      8
067100000000     C                   MOVEL     'S'           FAT(24)
067200000000     C                   ENDIF
067300000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
067400000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
067500000000     C                   Z-ADD     8             FUNVIS            5 0
067600000000     C     FAT(23)       IFEQ      'S'
067700000000     C                   SUB       1             FUNVIS
067800000000     C                   ENDIF
067900000000     C     FAT(24)       IFEQ      'S'
068000000000     C                   SUB       1             FUNVIS
068100000000     C                   ENDIF
068200000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
068300000000     C                   Z-ADD     *ZEROS        W                 5 0
068400000000     C                   ADD       1             FUNULT
068500000000     C     FUNULT        IFGT      31
068600000000     C                   Z-ADD     1             FUNULT
068700000000     C                   ENDIF
068800000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
068900000000     C     FUNULT        DO        31            I
069000000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
069100000000     C     W             IFLT      FUNVIS
069200000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
069300000000     C     FAT(I)        IFEQ      'S'
069400000000     C     FDE(I)        ANDNE     *BLANKS
069500000000     C     I             IFEQ      23
069600000000     C     I             OREQ      24
069700000000     C                   ELSE
069800000000     C                   ADD       1             W
069900000000     C                   MOVEL     FDE(I)        FUV(W)
070000000000     C                   ENDIF
070100000000     C                   ENDIF
070200000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
070300000000     C                   Z-ADD     I             FUNULT
070400000000     C                   ENDIF
070500000000     C                   ENDDO
070600000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
070700000000     C                   MOVEL     FUV(1)        VFD001
070800000000     C                   MOVEL     FUV(2)        VFD002
070900000000     C                   MOVEL     FUV(3)        VFD003
071000000000     C                   MOVEL     FUV(4)        VFD004
071100000000     C                   MOVEL     FUV(5)        VFD005
071200000000     C                   MOVEL     FUV(6)        VFD006
071300000000     C     FAT(23)       IFEQ      'N'
071400000000     C     FAT(24)       ANDEQ     'N'
071500000000     C                   MOVEL     FUV(7)        VFD007
071600000000     C                   MOVEL     FUV(8)        VFD008
071700000000     C                   ELSE
071800000000     C     FAT(23)       IFEQ      'S'
071900000000     C     FAT(24)       ANDEQ     'S'
072000000000     C                   MOVEL     FDE(23)       VFD007
072100000000     C                   MOVEL     FDE(24)       VFD008
072200000000     C                   ELSE
072300000000     C                   MOVEL     FUV(7)        VFD007
072400000000     C     FAT(23)       IFEQ      'S'
072500000000     C                   MOVEL     FDE(23)       VFD008
072600000000     C                   ELSE
072700000000     C                   MOVEL     FDE(24)       VFD008
072800000000     C                   ENDIF
072900000000     C                   ENDIF
073000000000     C                   ENDIF
073100000000     C                   MOVEL     '0'           $TESVD
073200000000     C                   ENDSR
073300000000     C*------------------------------------------------------------------------*
073400000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
073500000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
073600000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
073700000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
073800000000     C*------------------------------------------------------------------------*
073900000000     C     CONFUN        BEGSR
074000000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
074100000000     C     1             DO        31            I
074200000000     C     KEY           IFEQ      KEYS(I)
074300000000     C* CONTROLLO SE IL TASTO E' ATTIVO
074400000000     C     FAT(I)        IFNE      'S'
074500000000     C                   MOVEL     MSG(1)        $MSG
074600990219     C                   EXSR      CARMSG
074700000000     C                   MOVEL     '1'           $ERR
074800000000     C                   ENDIF
074900000000     C                   ENDIF
075000000000     C                   ENDDO
075100000000     C                   ENDSR
075200000000     O*------------------------------------------------------------------------*
075300000000** - MSG -
075400000000TRECE6R- Tasto funzionale non valido.                                          1
075500000000TRECE6R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
075600000000TRECE6R- Opzione non valida.                                                   3
075700000000TRECE6R- Opzione abilitata ma non gestita. Avvertire CED.                      4
075800000000TRECE6R- Inizio elenco.                                                        5
075900000000TRECE6R- Fine elenco.                                                          6
076000000000TRECE6R- 7
076100000000TRECE6R- 8
076200000000TRECE6R- 9
076300000000TRECE6R- Anno errato                                                          10
076400000000TRECE6R- Mese errato                                                          11
076500000000TRECE6R- Il mese finale deve essere maggiore o uguale di quello iniziale      12
076600000000TRECE6R- Immettere uno dei valori possibili                                   13
076700000000TRECE6R- 14
076800000000TRECE6R- 15
076900000000TRECE6R- 16
077000000000TRECE6R- 17
077100000000TRECE6R- 18
077200000000TRECE6R- 19
077300000000TRECE6R- Il programma non può girare in prova !!!                             20
077400000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
077500000000F1    -                   -    NN     NN
077600000000F2    -                   -    NN     NN
077700000000F3    -F3=Fine            -    SS     SS
077800000000F4    -                   -    NN     NN
077900000000F5    -                   -    NN     NN
078000000000F6    -F6=Lancio          -    SS     SS
078100000000F7    -                   -    NN     NN
078200000000F8    -                   -    NN     NN
078300000000F9    -                   -    NN     NN
078400000000F10   -                   -    NN     NN
078500000000F11   -                   -    NN     NN
078600000000F12   -F12=Ritorno        -    SS     SS
078700000000F13   -                   -    NN     NN
078800000000F14   -                   -    NN     NN
078900000000F15   -                   -    NN     NN
079000000000F16   -                   -    NN     NN
079100000000F17   -                   -    NN     NN
079200000000F18   -                   -    NN     NN
079300000000F19   -                   -    NN     NN
079400000000F20   -                   -    NN     NN
079500000000F21   -                   -    NN     NN
079600000000F22   -                   -    NN     NN
079700000000F23   -                   -    NN     NN
079800000000F24   -F24=Altri tasti    -    NN     NN
079900000000ENTER -                   -    SS     SS
080000000000ROLLUP-                   -    NN     NN
080100000000ROLLDN-                   -    NN     NN
080200000000HELP  -                   -    NN     NN
080300000000PRINT -                   -    NN     NN
080400000000BSPACE-                   -    NN     NN
080500000000CLEAR -                   -    NN     NN
