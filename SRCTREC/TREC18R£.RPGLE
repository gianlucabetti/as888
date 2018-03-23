000100130130     /*END
000200000000     H*------------------------------------------------------------------------*
000300000000     H* STATISTICHE DELTA PER FASCE DI PESO - RICHIESTA PARAMETRI              *
000400000000     H*------------------------------------------------------------------------*
000500000000     H DECEDIT('0,') DATEDIT(*DMY.)
000600000000     F*------------------------------------------------------------------------*
000700170302     FTREC18D   CF   E             WORKSTN INFDS(DEVDS1)
000800170302     F                                     SFILE(TREC18M:MSGULT)
000900000000     D*------------------------------------------------------------------------*
001000170306     D DEC             S             35    DIM(5) CTDATA PERRCD(1)
001100010508     D MSG             S             78    DIM(30) CTDATA PERRCD(1)
001200170306     D*------------------------------------------------------------------------*
001300000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)
001400000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
001500000000     D FFU             S              6    DIM(31)
001600000000     D FDE             S             19    DIM(31)
001700000000     D FAT             S              1    DIM(31)
001800000000     D* TABELLA DESCRIZIONI A VIDEO
001900000000     D FUV             S             19    DIM(8)
002000000000     D* TABELLA TASTI FUNZIONALI PREMUTI
002100000000     D KEYS            S              1    DIM(40)
002200000000     D*------------------------------------------------------------------------*
002300000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
002400000000     D DSFUN         E DS
002500000000     D  FUNRIG                 1     40
002600000000     D*------------------------------------------------------------------------*
002700000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
002800000000     D DSMSG         E DS
002900170306     D*------------------------------------------------------------------------*
003000020916     D* DS REPERIMENTO DATI UTENTE
003100020916     D*-------------------
003200020916     D TIBS34DS      E DS
003300020916     D DDATIUTE      E DS
003400020916     D AZUTEDS       E DS                  extname(AZUTE00F)
003500000000     D*------------------------------------------------------------------------*
003600000000     D* DS PER GESTIONE TASTI FUNZIONALI
003700000000     D DSKEYS        E DS
003800000000     D  KEYRIG                 1     40
003900000000     D*------------------------------------------------------------------------*
004000000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
004100000000     D                SDS
004200000000     D  USER                 254    263
004300000000     D*------------------------------------------------------------------------*
004400000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
004500000000     D DEVDS1          DS
004600000000     D* CHIAVE FUNZIONALE BATTUTA
004700000000     D  KEY                  369    369
004800000000     D* POSIZIONE CURSORE (IN HEX)
004900000000     D  CURSOR               370    371
005000000000     D*------------------------------------------------------------------------*
005100000000     D* ARCHITETTURA
005200000000     D KPJBA         E DS
005300170306     D*------------------
005400170306     D* DS X CONTROLLO ABILITAZIONI
005500170306     D*------------------
005600170306     D TAA1DS        E DS                  EXTNAME(TNTAA1DS)
005700980129     D*------------------------------------------------------------------------*
005800990524     D* DS CONTROLLO DATA (8)
005900990524     D WLBDA8          DS                  INZ
006000990524     D  G08DAT                 1      8  0
006100990524     D  G08INV                 9     16  0
006200990524     D  G08ERR                17     17
006300990524     D  G08TGI                18     22  0
006400981117     D*------------------------------------------------------------------------*
006500000000     D* PARAMETRI LANCIO
006600170306     D*------------------------------------------------------------------------*
006700170306     D Trec17DS      E DS
006800170306     D*------------------
006900170306     D* DS "XCLIR"
007000170306     D*------------------
007100170306     D DSXC          E DS                  EXTNAME(XCLIRDS)
007200170306     D*------------------
007300170306     D* DS "TIBS10R"
007400170306     D*------------------
007500170306     D BS10DS        E DS                  EXTNAME(TIBS10DS)
007600170306     D*------------------
007700170306     D* DS PER PASSAGGIO CAMPI PACKED
007800170306     D*------------------
007900170306     D                 DS
008000170306     D  DSKSN                  1      4P 0
008100170306     D  DSKSA                  1      4
008200170306     D KSC             S              7  0 DIM(28)
008300170306     D KSA             S              4    DIM(28)
008400170306     D*------------------
008500170306     D wabi            S              2
008600170306     D*------------------
008700170306     D* DS REPERIMENTO DATI UTENTE
008800170306     D*-------------------
008900170306     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
009000170306     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
009100170306     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
009200170306     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
009300170306     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
009400170306     D*------------
009500000000     C*------------------------------------------------------------------------*
009600000000     C* MAIN LINE
009700000000     C*------------------------------------------------------------------------*
009800000000     C*
009900000000     C* CICLO FINO A CHE:
010000000000DO  1C     $FINE         DOUEQ     '1'
010100000000     C*
010200000000     C                   MOVEL     '0'           $ERR
010300000000     C*
010400000000     C* CONTROLLA GESTIONE VIDEO
010500000000IF  2C     $GES          IFEQ      'V1'
010600000000     C                   EXSR      GESV1
010700000000E   2C                   ENDIF
010800000000     C*
010900000000E   1C                   ENDDO
011000000000     C*
011100000000     C                   SETON                                        LR
011200000000     C*------------------------------------------------------------------------*
011300000000     C* GESV1 - GESIONE PRIMO VIDEO
011400000000     C*------------------------------------------------------------------------*
011500000000     C     GESV1         BEGSR
011600000000     C*
011700000000IF  1C     $TESVD        IFEQ      '1'
011800000000     C                   EXSR      CARV1
011900000000E   1C                   ENDIF
012000000000     C*
012100000000IF  1C     MSGULT        IFEQ      *ZEROS
012200000000     C                   MOVEL     *BLANKS       $MSG
012300000000     C                   EXSR      CARMSG
012400000000E   1C                   ENDIF
012500000000     C                   EXSR      OUTMSG
012600000000     C*
012700170306     C                   EXFMT     TREC18
012800000000     C*
012900000000     C                   EXSR      CONFUN
013000000000     C*
013100000000IF  1C     $ERR          IFEQ      '0'
013200000000SL  2C                   SELECT
013300000000     C     KEY           WHENEQ    ENTER
013400000000     C                   EXSR      ENTV1
013500000000     C     KEY           WHENEQ    F6
013600000000     C                   EXSR      F06GES
013700000000     C                   OTHER
013800000000     C                   EXSR      FUNSTD
013900000000E   2C                   ENDSL
014000000000E   1C                   ENDIF
014100000000     C*
014200000000     C                   ENDSR
014300000000     C*------------------------------------------------------------------------*
014400000000     C* CARV1 - CARICAMENTO PRIMO VIDEO
014500000000     C*------------------------------------------------------------------------*
014600000000     C     CARV1         BEGSR
014700000000     C*
014800000000     C                   MOVEL     'V1'          $GES
014900000000     C*
015000000000     C                   EXSR      CARFUN
015100000000     C                   EXSR      OUTFUN
015200000000     C*
015300170302     C                   WRITE     TREC18T
015400170302     C                   WRITE     TREC18F
015500000000     C*
015600000000     C                   ENDSR
015700000000     C*------------------------------------------------------------------------*
015800000000     C* ENTV1 - GESTIONE TASTO "ENTER" PRIMO VIDEO
015900000000     C*------------------------------------------------------------------------*
016000000000     C     ENTV1         BEGSR
016100000000     C*
016200000000     C* CONTROLLA I CAMPI IMMESSI
016300000000     C                   EXSR      DECODI
016400000000     C*
016500000000     C                   ENDSR
016600000000     C*------------------------------------------------------------------------*
016700000000     C* F06GES - GEST. TASTO FUNZ. F06 ==> CONFERMA LANCIO
016800000000     C*------------------------------------------------------------------------*
016900000000     C     F06GES        BEGSR
017000000000     C*
017100000000     C* CONTROLLA I CAMPI IMMESSI
017200000000     C                   EXSR      DECODI
017300000000     C*
017400000000     C* NON CI SONO ERRORI
017500000000IF  1C     $ERR          IFEQ      '0'
017600000000     C*
017700000000     C* LANCIA PROGRAMMA
017800170306     C                   MOVEL     v1cksc        I17KSC
017900170306     C                   MOVEL     V1CDAT        I17ANNOI
018000170306     C                   MOVE      V1CDAT        I17MESEI
018100170306     C                   MOVEL     V1CDA2        I17ANNOF
018200170306     C                   MOVE      V1CDA2        I17MESEF
018300170310     C                   MOVE      V1UNIF        I17UNI
018400170302     C                   MOVEL     'EC17'        KCOAZ
018500170310      **
018600170310     C                   eval       KPJBU = trec17ds
018700170302 MOM C*
018800170302     c                   if        %subst(KNMUS:1:3)='EDP'
018900170302     C                   CALL      'BCH09'
019000170302     C                   PARM                    KPJBA
019100170302     c                   end
019200170302     C*
019300000614     C                   CALL      'BCH10'
019400000000     C                   PARM                    KPJBA
019500000000     C*
019600000000     C                   MOVEL     '1'           $FINE
019700000000E   1C                   ENDIF
019800000000     C*
019900000000     C                   ENDSR
020000000000     C*------------------------------------------------------------------------*
020100000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
020200000000     C*------------------------------------------------------------------------*
020300000000     C     DECODI        BEGSR
020400000000     C*
020500000000     C* IMPOSTO INDICATORI DI ERRORE SUI CAMPI
020600170309     C                   SETOFF                                       606162
020700170309     C                   SETOFF                                       63
020800170309     C                   SETOFF                                       717273
020900000000     C                   Z-ADD     *ZEROS        RISUL             4 0
021000170309     C                   MOVEL     *BLANKS       V1D001
021100170309     C                   MOVEL     *BLANKS       V1D002
021200170309     C                   MOVEL     *BLANKS       V1D003
021300170309     C                   MOVEL     *BLANKS       V1D004
021400000000     C*--------------------
021500000000     C* ANNO/MESE INIZIALE
021600000000     C*--------------------
021700000000     C* OBBLIGATORIO
021800000000IF  1C     $ERR          IFEQ      '0'
021900000000IF  2C     V1CDAT        IFEQ      *ZEROS
022000000000     C                   MOVEL     '1'           $ERR
022100170306     C                   MOVEL     MSG(7)        $MSG
022200000000     C                   EXSR      CARMSG
022300000000     C                   SETON                                        60
022400000000E   2C                   ENDIF
022500000000E   1C                   ENDIF
022600000000     C* CORRETTO
022700000000IF  1C     $ERR          IFEQ      '0'
022800990524     C                   MOVEL     V1CDAT        G08INV
022900990524     C                   MOVE      01            G08INV
023000990524     C                   Z-ADD     *ZEROS        G08DAT
023100990524     C                   MOVEL     '3'           G08ERR
023200990524     C                   CALL      'XSRDA8'
023300990524     C                   PARM                    WLBDA8
023400990524IF  2C     G08ERR        IFEQ      '1'
023500000000     C                   MOVEL     '1'           $ERR
023600000000     C                   MOVEL     MSG(8)        $MSG
023700000000     C                   EXSR      CARMSG
023800000000     C                   SETON                                        60
023900000000E   2C                   ENDIF
024000000000E   1C                   ENDIF
024100000000     C*--------------------
024200000000     C* ANNO/MESE FINALE
024300000000     C*--------------------
024400000000     C* OBBLIGATORIO
024500000000IF  1C     $ERR          IFEQ      '0'
024600000000IF  2C     V1CDA2        IFEQ      *ZEROS
024700000000     C                   MOVEL     '1'           $ERR
024800000000     C                   MOVEL     MSG(7)        $MSG
024900000000     C                   EXSR      CARMSG
025000000000     C                   SETON                                        61
025100000000E   2C                   ENDIF
025200000000E   1C                   ENDIF
025300000000     C* CORRETTO
025400000000IF  1C     $ERR          IFEQ      '0'
025500990524     C                   MOVEL     V1CDA2        G08INV
025600990524     C                   MOVE      01            G08INV
025700990524     C                   Z-ADD     *ZEROS        G08DAT
025800990524     C                   MOVEL     '3'           G08ERR
025900990524     C                   CALL      'XSRDA8'
026000990524     C                   PARM                    WLBDA8
026100990524IF  2C     G08ERR        IFEQ      '1'
026200000000     C                   MOVEL     '1'           $ERR
026300000000     C                   MOVEL     MSG(8)        $MSG
026400000000     C                   EXSR      CARMSG
026500000000     C                   SETON                                        61
026600000000E   2C                   ENDIF
026700000000E   1C                   ENDIF
026800000000     C*
026900000000     C* ANNO/MESE FINALE SUPERIORE/UGUALE A QUELLO INIZIALE
027000000000IF  1C     $ERR          IFEQ      '0'
027100000000IF  2C     V1CDA2        IFLT      V1CDAT
027200000000     C                   MOVEL     '1'           $ERR
027300170303     C                   MOVEL     MSG(09)       $MSG
027400000000     C                   EXSR      CARMSG
027500000000     C                   SETON                                        61
027600000000E   2C                   ENDIF
027700000000E   1C                   ENDIF
027800000000     C*
027900000000     C* RICHIEDERE NON PIU' DI DODICI MESI
028000000000IF  1C     $ERR          IFEQ      '0'
028100000000     C     V1CDA2        SUB       V1CDAT        RISUL
028200000000IF  2C     RISUL         IFGE      100
028300000000     C                   MOVEL     '1'           $ERR
028400170303     C                   MOVEL     MSG(10)       $MSG
028500000000     C                   EXSR      CARMSG
028600000000     C                   SETON                                        6160
028700000000E   2C                   ENDIF
028800000000E   1C                   ENDIF
028900020325     C*
029000020325     C* ANNO INIZIALE DIVERSO DA ANNO FINALE (NN E' + POSSIBILE LANCIARE LA STAT
029100020325IF  1C     $ERR          IFEQ      '0'
029200020325     C                   MOVEL     V1CDAT        ANNODA            4 0
029300020325     C                   MOVEL     V1CDA2        ANNOA             4 0
029400020325IF  2C     ANNODA        IFNE      ANNOA
029500020325     C                   MOVEL     '1'           $ERR
029600170303     C                   MOVEL     MSG(11)       $MSG
029700020325     C                   EXSR      CARMSG
029800020325     C                   SETON                                        6061
029900020325E   2C                   ENDIF
030000020325E   1C                   ENDIF
030100000000     C*--------------------
030200170302     C* Controlla AUTISTA
030300000000     C*--------------------
030400000000IF  1C     $ERR          IFEQ      '0'
030500170306      *
030600170306     C* CODICE CLIENTE OBBLIGATORIO
030700170306IF  1C     V1CKSC        IFEQ      *BLANKS
030800170306     C     V1CKSC        OREQ      *ZEROS
030900170306     C                   MOVEL     '1'           $ERR
031000170306     C                   MOVEL     MSG(12)       $MSG
031100170306     C                   EXSR      CARMSG
031200170309     C                   SETON                                        72
031300170306E   1C                   ENDIF
031400170306     C*
031500170306     C* RICHIESTA RICERCA CON IL ?
031600170306IF  1C     $ERR          IFEQ      '0'
031700170306     C     '?'           SCAN      V1CKSC                                 21
031800170306IF  2C     *IN21         IFEQ      *ON
031900170306     C                   CLEAR                   DSXC
032000170306     C                   Z-ADD     DUTKCI        DXCCAP
032100170306     C                   MOVEL     WAbi          DXCAUT
032200170306     C                   CALL      'XCLIR'
032300170306     C                   PARM                    DSXC
032400170306     C                   MOVEA     DXCKSC        KSA
032500170306DO  3C     1             DO        28            J                 4 0
032600170306     C                   MOVE      KSA(J)        DSKSA
032700170306     C                   Z-ADD     DSKSN         KSC(J)
032800170306E   3C                   ENDDO
032900170306     C                   MOVEL     KSC(1)        V1CKSC
033000170306     C                   MOVEL     '1'           $TESVD
033100170306E   2C                   ENDIF
033200170306E   1C                   ENDIF
033300170306     C*
033400170306     C* CONTROLLA SE CLIENTE NUMERICO
033500170306IF  1C     $ERR          IFEQ      '0'
033600170306     C                   SETOFF                                       21
033700170306     C                   TESTN                   V1CKSC               21
033800170306IF  2C     *IN21         IFEQ      *ON
033900170306     C                   MOVE      V1CKSC        W001              1
034000170306IF  3C     W001          IFLT      '0'
034100170306     C                   MOVEL     '1'           $ERR
034200170306     C                   MOVEL     MSG(12)       $MSG
034300170306     C                   EXSR      CARMSG
034400170309     C                   SETON                                        72
034500170306E   3C                   ENDIF
034600170306X   2C                   ELSE
034700170306     C                   MOVEL     '1'           $ERR
034800170306     C                   MOVEL     MSG(12)       $MSG
034900170306     C                   EXSR      CARMSG
035000170309     C                   SETON                                        72
035100170306E   2C                   ENDIF
035200170306E   1C                   ENDIF
035300170306     C*
035400170306     C* CONTROLLA SE CLIENTE VALIDO
035500170306IF  1C     $ERR          IFEQ      '0'
035600170306     C                   CLEAR                   BS69DS
035700170306     C                   CLEAR                   ACODS
035800170306     C                   MOVEL     KNSIF         I69SIF
035900170306     C                   MOVEL     V1CKSC        I69KAC
036000170306     C                   CALL      'TIBS69R'
036100170306     C                   PARM                    BS69DS
036200170306     C                   PARM                    ACODS
036300170306     C                   PARM                    INDDS
036400170306     C                   PARM                    CLPDS
036500170306     C                   PARM                    CLSDS
036600170306IF  2C     O69ERR        IFNE      '1'
036700170309     C                   MOVEL     acorag        V1DKSC
036800170306X   2C                   ELSE
036900170306     C                   MOVEL     '1'           $ERR
037000170306     C                   MOVEL     MSG(12)       $MSG
037100170306     C                   EXSR      CARMSG
037200170309     C                   SETON                                        72
037300170306E   2C                   ENDIF
037400170306E   1C                   ENDIF
037500170306     C*
037600170306     C* CONTROLLA SE CLIENTE GESTIBILE:
037700170306IF  1C     $ERR          IFEQ      '0'
037800170306      * CONTROLLA SE PUÒ ESSERE VISTO OPPURE NO CON ABILITAZIONI ANCHE X FUORI A
037900170306      * MA PER AGENTE UNIFICANTE:
038000170306     C                   clear                   TAA1DS
038100170306     C                   EVAL        ITAA1CAUT = '§UTEIST'
038200170306     C                   move      V1CKSC        ITAA1KSC
038300170306     C                   EVAL      KPJBU = TAA1DS
038400170306     C                   CALL      'TNTAA1C'
038500170306     C                   PARM                    KPJBA
038600170306     C                   EVAL              TAA1DS = KPJBU
038700170306      *
038800170306      *  SE NON ABILITATO PROCEDO CON IL VECCHIO CONTROLLO SULLA FILIALE GESTITA
038900170306     C                   IF        OTAA1FABI <> 'S'
039000170306     C                   MOVEL     '1'           $ERR
039100170306     C                   MOVEL     MSG(14)       $MSG
039200170306     C                   EXSR      CARMSG
039300170309     C                   SETON                                        72
039400170306     c                   end
039500170309     C*
039600170309     C*------- GIORNO   di   F I N E   M E S E ---------------------*
039700170309     C*
039800170309     C* COMPONE IL PRIMO GIORNO DELLA DATA FINALE
039900170309     C                   MOVEL     V1CDA2        WDFIN             8 0
040000170309     C                   MOVE      28            WDFIN
040100170309     C*
040200170309     C*  Ultimo giorno del mese
040300170309     C                   MOVE      WDFIN         MMGG_4            4 0
040400170309     C                   MOVEL     MMGG_4        Mm_2              2 0
040500170309     C                   MOVEl     WDFIN         Anno_4            4 0
040600170309      * se è febbraio
040700170309     c                   If        Mm_2 = 2
040800170309      *
040900170309      *  potrebbe essere bisestile 29 gg.
041000170309     c     Anno_4        div       4             risulta           3 0
041100170309     c                   mvr                     resto             3 0
041200170309      *  Se bisestile
041300170309     c                   If        resto = 0
041400170309     C                   MOVE      '29'          WDFIN
041500170309     c                   end
041600170309      *
041700170309     c                   else
041800170309      *
041900170309      *  se invece Aprile, Giugno, Settembre, Novembre con 30 gg.
042000170309     c                   If           Mm_2 = 4 or Mm_2 = 6 or
042100170309     c                                Mm_2 = 9 or Mm_2 = 11
042200170309     C                   MOVE      '30'          WDFIN
042300170309     c                   else
042400170309     C                   MOVE      '31'          WDFIN
042500170309     c                   end
042600170309      *
042700170309     c                   endIf
042800170309     C*
042900170309     C*----------- F I N E    M E S E ---------------------------*
043000170306      *
043100170306     C* SE SCELTA CLIENTE UNIFICANTE, CONTROLLA E NEL CASO REPERISCE IL CLIENTE
043200170306IF  1C     $ERR          IFEQ      '0'
043300170306IF  2C     V1UNIF        IFEQ      'S'
043400170306     C                   CLEAR                   BS10DS
043500170309     C                   Z-ADD     WDFIN         D10DRF
043600170306     C                   MOVEL     'ST'          D10TLE
043700170306     C                   MOVEL     'P'           D10PAF
043800170306     C                   MOVE      V1CKSC        D10COD
043900170306     C                   CALL      'TIBS10R'
044000170306     C                   PARM                    BS10DS
044100170306IF  3C     D10ERR        IFEQ      *BLANKS
044200170306     C     D10COD        ANDNE     D10COP
044300170309     C                   MOVEL     DEC(1)        V1D001
044400170310     c                   eval      V1D001 = 'Attenzione. Il codice NON è unif-
044500170310     c                             icante.'
044600170310     c                   MOVE      D10COP        campo7            7
044700170310     c                   eval      V1D002 = 'L''unificante da utilizzare sare-
044800170310     c                             bbe -->' + campo7
044900170310     c                   eval      V1D003 = 'Sostituire il codice con l''unifi-
045000170310     c                             cante - oppure mettere "N" '
045100170309     C                   SETON                                        73
045200170306E   3C                   ENDIF
045300170306E   2C                   ENDIF
045400170306E   1C                   ENDIF
045500170306     C*
045600000000E   1C                   ENDIF
045700000614     C*
045800170306E   1C                   ENDIF
045900170306     C*
046000000000     C                   ENDSR
046100000000     C*------------------------------------------------------------------------*
046200950214     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
046300000000     C*------------------------------------------------------------------------*
046400000000     C     IMPVID        BEGSR
046500000614     C*
046600170303     C* IMPOSTO DI DEFAULT IL FLAG clienti
046700170309     c                   movel     'S'           V1unif
046800170306     c                   movel     '?'           v1cksc
046900000000     C*--------------------
047000000000     C* RAGIONE SOCIALE
047100000000     C*--------------------
047200020916     C                   MOVEL     RSUT          VTDRSU
047300000000     C*--------------------
047400000000     C* DATE DI ELABORAZIONE
047500000000     C*--------------------
047600000000     C***
047700170303     C* AA/MM FINALE   -->
047800170303     C* AA/MM INIZIALE -->
047900000000     C***
048000000000     C*--------------------
048100000000     C* D A T A   F I N E
048200000000     C*--------------------
048300170303     c                   move      *year         anno              4
048400170303     c                   move      *month        mese              2
048500170303     c                   movel     anno          annmes            6
048600170303     c                   move      mese          annmes
048700170306     C                   move      AnnMes        V1CDA2
048800000000     C*--------------------
048900000000     C* D A T A   I N I Z I O
049000000000     C*--------------------
049100170303     c                   move      '01'          mese              2
049200170309     c                   move      mese          annmes
049300170306     C                   move      AnnMes        V1CDAT
049400000000     C*--------------------
049500170303     C* MESSAGGI
049600000000     C*--------------------
049700170303     C                   MOVEL     *BLANKS       V1D001
049800170303     C                   MOVEL     *BLANKS       V1D002
049900170303     C                   MOVEL     *BLANKS       V1D003
050000170309     C                   MOVEL     *BLANKS       V1D004
050100000000     C*
050200000000     C                   ENDSR
050300020916     C*-------------------------------------------------------------------------
050400020916     C* REPERISCE I DATI UTENTE
050500020916     C*-------------------------------------------------------------------------
050600020916     C     REPDATIUTE    BEGSR
050700020916     C*
050800020916     C* INIZIALIZZA VARIABILI DI WRK
050900020916     C                   CLEAR                   TIBS34DS
051000020916     C                   CLEAR                   AZUTEDS
051100020916     C                   CLEAR                   DDATIUTE
051200020916     C*
051300020916     C     *DTAARA       DEFINE    §azute        azuteds
051400020916     C     *DTAARA       DEFINE    §datiute      ddatiute
051500020916     C                   IN(E)     *DTAARA
051600020916if  1C                   IF        %Error
051700020916     c                   EVAL      I34Tla = 'L'
051800020916     C                   CALL      'TIBS34R'
051900020916     C                   PARM                    Tibs34Ds
052000020916     C                   IN        *DTAARA
052100020916e   1C                   ENDIF
052200020916     C*
052300170306     C                   clear                   TAA1DS
052400170306     C                   EVAL        ITAA1CAUT = '§UTEIST'
052500170306     C                   EVAL      KPJBU = TAA1DS
052600170306     C                   CALL      'TNTAA1C'
052700170306     C                   PARM                    KPJBA
052800170306     C                   EVAL              TAA1DS = KPJBU
052900170306      *
053000170306x   2C                   if         OTAA1ERR ='S'
053100170306     C                   EVAL      $ERR  = '1'
053200170306     C                   EVAL      $FINE = '1'
053300170306e   2C                   Else
053400170306x   2C                   eval      wABI =  OTAA1CABI
053500170306e   2C                   ENDIF
053600170306     C*
053700020916     C                   ENDSR
053800000000     C*------------------------------------------------------------------------*
053900000000     C* *INZSR - ROUTINE INIZIALE
054000000000     C*------------------------------------------------------------------------*
054100000000     C     *INZSR        BEGSR
054200000000     C*
054300000000     C* RICEVIMENTO PARAMETRI
054400000000     C     *ENTRY        PLIST
054500000000     C                   PARM                    KPJBA
054600170306     C*
054700170306     C* IMNPOSTA DATA / ORA CORRENTE
054800170306     C                   TIME                    N14              14 0
054900170306     C                   MOVE      N14           G08DAT            8 0
055000170306     C                   Z-ADD     *ZEROS        G08INV
055100170306     C                   MOVEL     '0'           G08ERR
055200170306     C                   CALL      'XSRDA8'
055300170306     C                   PARM                    WLBDA8
055400170306     C                   Z-ADD     G08INV        DATCOR            8 0
055500000000     C*
055600000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
055700000000     C                   EXSR      INZSTD
055800000000     C*
055900000000     C* IMPOSTAZIONI INIZIALI VIDEO
056000000000     C                   EXSR      IMPVID
056100000000     C*
056200000000     C* EMETTO PRIMO FORMATO VIDEO
056300000000     C                   EXSR      CARV1
056400000000     C*
056500000000     C                   ENDSR
056600000000     C*------------------------------------------------------------------------*
056700000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
056800000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
056900000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
057000000000     C*      LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
057100000000     C*      STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
057200000000     C*      NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
057300000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
057400000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
057500000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
057600000000     C*------------------------------------------------------------------------*
057700000000     C* INZSTD - ROUTINE INIZIALE STANDARD
057800000000     C*------------------------------------------------------------------------*
057900000000     C     INZSTD        BEGSR
058000000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
058100000000     C                   CALL      'XKEYS'
058200000000     C                   PARM                    DSKEYS
058300000000     C                   MOVEA     KEYRIG        KEYS
058400000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
058500000000     C     XMSGPA        PLIST
058600000000     C                   PARM                    DSMSMS
058700000000     C                   PARM                    DSMSPG
058800000000     C                   PARM                    DSMSKE
058900000000     C                   PARM                    DSMSER
059000000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
059100010508     C                   MOVEL     'TISEA1R'     DSMSPG
059200010508     C                   MOVEL     'TISEA1R'     VMCQUE
059300000000     C                   MOVEL     *BLANKS       VMCKEY
059400000000     C                   MOVEL     *BLANKS       DSMSMS
059500000000     C* INIZIALIZZO FLAG CONTROLLO PGM
059600000000     C                   MOVEL     '0'           $ERR              1
059700000000     C                   MOVEL     '0'           $F03              1
059800000000     C                   MOVEL     *BLANKS       $GES              2
059900000000     C                   MOVEL     *BLANKS       $MSG             80
060000000000     C                   MOVEL     '0'           $FINE             1
060100000000     C                   MOVEL     '1'           $TESVD            1
060200000000     C                   MOVEL     'I'           $TGES             1
060300000000     C* LANCIO ROUTINE PER VERIFICARE SE
060400000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
060500000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
060600020916     C                   EXSR      REPDATIUTE
060700000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
060800000000     C                   BITOFF    '01234567'    POSMI1            1
060900000000     C                   BITOFF    '01234567'    POSMI2            1
061000000000     C                   BITOFF    '01234567'    POSMA1            1
061100000000     C                   BITOFF    '01234567'    POSMA2            1
061200000000     C                   BITON     '34'          POSMI1            1
061300000000     C                   BITON     '7'           POSMI2            1
061400000000     C                   BITON     '34'          POSMA1            1
061500000000     C                   BITON     '0'           POSMA2            1
061600000000     C                   MOVE      POSMI2        POSMIN            2
061700000000     C                   MOVEL     POSMI1        POSMIN
061800000000     C                   MOVE      POSMA2        POSMAX            2
061900000000     C                   MOVEL     POSMA1        POSMAX
062000000000     C                   ENDSR
062100000000     C*------------------------------------------------------------------------*
062200000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
062300000000     C*------------------------------------------------------------------------*
062400000000     C     FUNSTD        BEGSR
062500000000     C                   SELECT
062600000000     C     KEY           WHENEQ    F3
062700000000     C                   EXSR      F03STD
062800000000     C     KEY           WHENEQ    F12
062900000000     C                   EXSR      F12STD
063000000000     C     KEY           WHENEQ    F24
063100000000     C                   EXSR      F24STD
063200000000     C                   OTHER
063300000000     C                   MOVEL     MSG(2)        $MSG
063400000000     C                   EXSR      CARMSG
063500000000     C                   ENDSL
063600000000     C                   ENDSR
063700000000     C*------------------------------------------------------------------------*
063800000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
063900000000     C*------------------------------------------------------------------------*
064000000000     C     F03STD        BEGSR
064100000000     C*
064200000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
064300000000     C                   MOVEL     '1'           $FINE
064400000000     C*
064500000000     C                   ENDSR
064600000000     C*------------------------------------------------------------------------*
064700000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
064800000000     C*------------------------------------------------------------------------*
064900000000     C     F12STD        BEGSR
065000000000     C*
065100000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
065200000000     C                   MOVEL     '1'           $FINE
065300000000     C*
065400000000     C                   ENDSR
065500000000     C*------------------------------------------------------------------------*
065600000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
065700000000     C*------------------------------------------------------------------------*
065800000000     C     F24STD        BEGSR
065900000000     C*
066000000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
066100000000     C                   EXSR      OUTFUN
066200000000     C*
066300000000     C                   ENDSR
066400000000     C*------------------------------------------------------------------------*
066500000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
066600000000     C*          CODA MESSAGGI DEL PGM
066700000000     C*------------------------------------------------------------------------*
066800000000     C     CARMSG        BEGSR
066900000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
067000000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
067100000000     C     MSGULT        IFEQ      0
067200000000     C                   MOVEL     '1'           *IN40
067300170302     C                   WRITE     TREC18K
067400000000     C                   MOVEL     '0'           *IN40
067500000000     C                   ENDIF
067600000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
067700000000     C                   MOVEL     $MSG          DSMSMS
067800000000     C                   CALL      'XMSG'        XMSGPA
067900000000     C     DSMSER        IFEQ      0
068000000000     C                   MOVEL     DSMSKE        VMCKEY
068100000000     C                   ENDIF
068200000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
068300000000     C                   ADD       1             MSGULT            4 0
068400170302     C                   WRITE     TREC18M
068500000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
068600000000     C                   MOVEL     *BLANKS       $MSG
068700000000     C                   ENDSR
068800000000     C*------------------------------------------------------------------------*
068900000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
069000000000     C*------------------------------------------------------------------------*
069100000000     C     OUTMSG        BEGSR
069200000000     C* VISUALIZZAZIONE MESSAGGIO
069300000000     C                   MOVEL     '1'           *IN44
069400170302     C                   WRITE     TREC18K
069500000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
069600000000     C                   Z-ADD     *ZEROS        MSGULT
069700000000     C                   ENDSR
069800000000     C*------------------------------------------------------------------------*
069900000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
070000000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
070100000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
070200000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
070300000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
070400000000     C*------------------------------------------------------------------------*
070500000000     C     CARFUN        BEGSR
070600000000     C                   Z-ADD     *ZEROS        FUNULT            5 0
070700000000     C     1             DO        31            I                 5 0
070800000000     C                   MOVEL     FUN(I)        FUNRIG
070900000000     C                   MOVEL     FUNFUN        FFU(I)
071000000000     C                   MOVEL     FUNDES        FDE(I)
071100020916     C     DUTLPO        IFEQ      'S'
071200000000     C     $TGES         IFEQ      'G'
071300000000     C                   MOVEL     FUNASG        FAT(I)
071400000000     C                   ELSE
071500000000     C                   MOVEL     FUNASI        FAT(I)
071600000000     C                   ENDIF
071700000000     C                   ELSE
071800000000     C     $TGES         IFEQ      'G'
071900000000     C                   MOVEL     FUNAFG        FAT(I)
072000000000     C                   ELSE
072100000000     C                   MOVEL     FUNAFI        FAT(I)
072200000000     C                   ENDIF
072300000000     C                   ENDIF
072400000000     C     FAT(I)        IFEQ      'E'
072500000000     C                   MOVEL     USER          WEDP              3
072600000000     C     WEDP          IFEQ      'EDP'
072700000000     C                   MOVEL     'S'           FAT(I)
072800000000     C                   ELSE
072900000000     C                   MOVEL     'N'           FAT(I)
073000000000     C                   ENDIF
073100000000     C                   ENDIF
073200000000     C                   ENDDO
073300000000     C                   ENDSR
073400000000     C*------------------------------------------------------------------------*
073500000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
073600000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
073700000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
073800000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
073900000000     C*------------------------------------------------------------------------*
074000000000     C     OUTFUN        BEGSR
074100000000     C     1             DO        8             I
074200000000     C                   MOVEL     *BLANKS       FUV(I)
074300000000     C                   ENDDO
074400000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
074500000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0
074600000000     C     1             DO        31            I
074700000000     C     FAT(I)        IFEQ      'S'
074800000000     C     FDE(I)        ANDNE     *BLANKS
074900000000     C                   ADD       1             FUNNAT
075000000000     C                   ENDIF
075100000000     C                   ENDDO
075200000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
075300000000     C     FUNNAT        IFGT      8
075400000000     C                   MOVEL     'S'           FAT(24)
075500000000     C                   ENDIF
075600000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
075700000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
075800000000     C                   Z-ADD     8             FUNVIS            5 0
075900000000     C     FAT(23)       IFEQ      'S'
076000000000     C                   SUB       1             FUNVIS
076100000000     C                   ENDIF
076200000000     C     FAT(24)       IFEQ      'S'
076300000000     C                   SUB       1             FUNVIS
076400000000     C                   ENDIF
076500000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
076600000000     C                   Z-ADD     *ZEROS        W                 5 0
076700000000     C                   ADD       1             FUNULT
076800000000     C     FUNULT        IFGT      31
076900000000     C                   Z-ADD     1             FUNULT
077000000000     C                   ENDIF
077100000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
077200000000     C     FUNULT        DO        31            I
077300000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
077400000000     C     W             IFLT      FUNVIS
077500000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
077600000000     C     FAT(I)        IFEQ      'S'
077700000000     C     FDE(I)        ANDNE     *BLANKS
077800000000     C     I             IFEQ      23
077900000000     C     I             OREQ      24
078000000000     C                   ELSE
078100000000     C                   ADD       1             W
078200000000     C                   MOVEL     FDE(I)        FUV(W)
078300000000     C                   ENDIF
078400000000     C                   ENDIF
078500000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
078600000000     C                   Z-ADD     I             FUNULT
078700000000     C                   ENDIF
078800000000     C                   ENDDO
078900000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
079000000000     C                   MOVEL     FUV(1)        VFD001
079100000000     C                   MOVEL     FUV(2)        VFD002
079200000000     C                   MOVEL     FUV(3)        VFD003
079300000000     C                   MOVEL     FUV(4)        VFD004
079400000000     C                   MOVEL     FUV(5)        VFD005
079500000000     C                   MOVEL     FUV(6)        VFD006
079600000000     C     FAT(23)       IFEQ      'N'
079700000000     C     FAT(24)       ANDEQ     'N'
079800000000     C                   MOVEL     FUV(7)        VFD007
079900000000     C                   MOVEL     FUV(8)        VFD008
080000000000     C                   ELSE
080100000000     C     FAT(23)       IFEQ      'S'
080200000000     C     FAT(24)       ANDEQ     'S'
080300000000     C                   MOVEL     FDE(23)       VFD007
080400000000     C                   MOVEL     FDE(24)       VFD008
080500000000     C                   ELSE
080600000000     C                   MOVEL     FUV(7)        VFD007
080700000000     C     FAT(23)       IFEQ      'S'
080800000000     C                   MOVEL     FDE(23)       VFD008
080900000000     C                   ELSE
081000000000     C                   MOVEL     FDE(24)       VFD008
081100000000     C                   ENDIF
081200000000     C                   ENDIF
081300000000     C                   ENDIF
081400000000     C                   MOVEL     '0'           $TESVD
081500000000     C                   ENDSR
081600000000     C*------------------------------------------------------------------------*
081700000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
081800000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
081900000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
082000000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
082100000000     C*------------------------------------------------------------------------*
082200000000     C     CONFUN        BEGSR
082300000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
082400000000     C     1             DO        31            I
082500000000     C     KEY           IFEQ      KEYS(I)
082600000000     C* CONTROLLO SE IL TASTO E' ATTIVO
082700000000     C     FAT(I)        IFNE      'S'
082800000000     C                   MOVEL     MSG(1)        $MSG
082900000000     C                   EXSR      CARMSG
083000000000     C                   MOVEL     '1'           $ERR
083100000000     C                   ENDIF
083200000000     C                   ENDIF
083300000000     C                   ENDDO
083400000000     C                   ENDSR
083500170306** DEC  (DECODIFICHE)
083600170309Cliente selezionato      -->
083700170309Unificante da utilizzare -->
083800170306--
083900170306--
084000170306--
084100000000**   SCHIERA: MSG  (MESSAGGI)
084200170309TREC18R- Tasto funzionale non valido.                                          1
084300170309TREC18R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
084400170309TREC18R- Opzione non valida.                                                   3
084500170309TREC18R- Opzione abilitata ma non gestita. Avvertire CED.                      4
084600170309TREC18R- Inizio elenco.                                                        5
084700170309TREC18R- Fine elenco.                                                          6
084800170309TREC18R- Anno/Mese obbligatorio                                                7
084900170309TREC18R- Anno/Mese errato                                                      8
085000170309TREC18R- Anno/Mese finale inferiore all'Anno/Mese iniziale                     9
085100170309TREC18R- Impossibile richiedere più di 12 mesi                                10
085200170309TREC18R- Impossibile lanciare la statistica su anni differenti                11
085300170309TREC18R- Cliente errato. Inserire il codice cliente.                          12
085400170309TREC18R- Il cliente richiesto non è unificante                                13
085500170309TREC18R- Cliente non gestibile                                                14
085600170309TREC18R-                                                                      15
085700170309TREC18R-                                                                      16
085800170303TISE90R-                                                                      17
085900170309TREC18R-                                                                      18
086000170309TREC18R-                                                                      19
086100170309TREC18R-                                                                      20
086200170309TREC18R-                                                                      21
086300170309TREC18R-                                                                      22
086400170309TREC18R- Opzione obbligatoria.                                                23
086500170309TREC18R-                                                                      24
086600170309TREC18R-                                                                      25
086700170309TREC18R-                                                                      26
086800170309TREC18R-                                                                      27
086900170309TREC18R-                                                                      28
087000170309TREC18R-                                                                      29
087100170309TREC18R-                                                                      30
087200000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
087300000000F1    -                   -    NN     NN
087400000000F2    -                   -    NN     NN
087500000000F3    -F3=Fine            -    SS     SS
087600000000F4    -                   -    NN     NN
087700000000F5    -                   -    NN     NN
087800000000F6    -F6=Lancio          -    SS     SS
087900000000F7    -                   -    NN     NN
088000000000F8    -                   -    NN     NN
088100000000F9    -                   -    NN     NN
088200000000F10   -                   -    NN     NN
088300000000F11   -                   -    NN     NN
088400000000F12   -F12=Ritorno        -    SS     SS
088500000000F13   -                   -    NN     NN
088600000000F14   -                   -    NN     NN
088700000000F15   -                   -    NN     NN
088800000000F16   -                   -    NN     NN
088900000000F17   -                   -    NN     NN
089000000000F18   -                   -    NN     NN
089100000000F19   -                   -    NN     NN
089200000000F20   -                   -    NN     NN
089300000000F21   -                   -    NN     NN
089400000000F22   -                   -    NN     NN
089500000000F23   -                   -    NN     NN
089600000000F24   -F24=Altri tasti    -    NN     NN
089700000000ENTER -                   -    SS     SS
089800000000ROLLUP-                   -    NN     NN
089900000000ROLLDN-                   -    NN     NN
090000000000HELP  -                   -    NN     NN
090100000000PRINT -                   -    NN     NN
090200000000BSPACE-                   -    NN     NN
090300000000CLEAR -                   -    NN     NN
