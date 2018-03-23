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
018500170317      **  x premere F3
018600170317     C                   MOVEL     'Z'           KRITB
018700170310      **
018800170310     C                   eval       KPJBU = trec17ds
018900170302 MOM C*
019000170302     c                   if        %subst(KNMUS:1:3)='EDP'
019100170302     C                   CALL      'BCH09'
019200170302     C                   PARM                    KPJBA
019300170302     c                   end
019400170302     C*
019500170317     C*   Se NON premuto F3
019600170317     c                   if        kritb =' '
019700000614     C                   CALL      'BCH10'
019800000000     C                   PARM                    KPJBA
019900170317     c                   end
020000000000     C*
020100000000     C                   MOVEL     '1'           $FINE
020200000000E   1C                   ENDIF
020300000000     C*
020400000000     C                   ENDSR
020500000000     C*------------------------------------------------------------------------*
020600000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
020700000000     C*------------------------------------------------------------------------*
020800000000     C     DECODI        BEGSR
020900000000     C*
021000000000     C* IMPOSTO INDICATORI DI ERRORE SUI CAMPI
021100170309     C                   SETOFF                                       606162
021200170309     C                   SETOFF                                       63
021300170309     C                   SETOFF                                       717273
021400000000     C                   Z-ADD     *ZEROS        RISUL             4 0
021500170309     C                   MOVEL     *BLANKS       V1D001
021600170309     C                   MOVEL     *BLANKS       V1D002
021700170309     C                   MOVEL     *BLANKS       V1D003
021800170309     C                   MOVEL     *BLANKS       V1D004
021900000000     C*--------------------
022000000000     C* ANNO/MESE INIZIALE
022100000000     C*--------------------
022200000000     C* OBBLIGATORIO
022300000000IF  1C     $ERR          IFEQ      '0'
022400000000IF  2C     V1CDAT        IFEQ      *ZEROS
022500000000     C                   MOVEL     '1'           $ERR
022600170306     C                   MOVEL     MSG(7)        $MSG
022700000000     C                   EXSR      CARMSG
022800000000     C                   SETON                                        60
022900000000E   2C                   ENDIF
023000000000E   1C                   ENDIF
023100000000     C* CORRETTO
023200000000IF  1C     $ERR          IFEQ      '0'
023300990524     C                   MOVEL     V1CDAT        G08INV
023400990524     C                   MOVE      01            G08INV
023500990524     C                   Z-ADD     *ZEROS        G08DAT
023600990524     C                   MOVEL     '3'           G08ERR
023700990524     C                   CALL      'XSRDA8'
023800990524     C                   PARM                    WLBDA8
023900990524IF  2C     G08ERR        IFEQ      '1'
024000000000     C                   MOVEL     '1'           $ERR
024100000000     C                   MOVEL     MSG(8)        $MSG
024200000000     C                   EXSR      CARMSG
024300000000     C                   SETON                                        60
024400000000E   2C                   ENDIF
024500000000E   1C                   ENDIF
024600000000     C*--------------------
024700000000     C* ANNO/MESE FINALE
024800000000     C*--------------------
024900000000     C* OBBLIGATORIO
025000000000IF  1C     $ERR          IFEQ      '0'
025100000000IF  2C     V1CDA2        IFEQ      *ZEROS
025200000000     C                   MOVEL     '1'           $ERR
025300000000     C                   MOVEL     MSG(7)        $MSG
025400000000     C                   EXSR      CARMSG
025500000000     C                   SETON                                        61
025600000000E   2C                   ENDIF
025700000000E   1C                   ENDIF
025800000000     C* CORRETTO
025900000000IF  1C     $ERR          IFEQ      '0'
026000990524     C                   MOVEL     V1CDA2        G08INV
026100990524     C                   MOVE      01            G08INV
026200990524     C                   Z-ADD     *ZEROS        G08DAT
026300990524     C                   MOVEL     '3'           G08ERR
026400990524     C                   CALL      'XSRDA8'
026500990524     C                   PARM                    WLBDA8
026600990524IF  2C     G08ERR        IFEQ      '1'
026700000000     C                   MOVEL     '1'           $ERR
026800000000     C                   MOVEL     MSG(8)        $MSG
026900000000     C                   EXSR      CARMSG
027000000000     C                   SETON                                        61
027100000000E   2C                   ENDIF
027200000000E   1C                   ENDIF
027300000000     C*
027400000000     C* ANNO/MESE FINALE SUPERIORE/UGUALE A QUELLO INIZIALE
027500000000IF  1C     $ERR          IFEQ      '0'
027600000000IF  2C     V1CDA2        IFLT      V1CDAT
027700000000     C                   MOVEL     '1'           $ERR
027800170303     C                   MOVEL     MSG(09)       $MSG
027900000000     C                   EXSR      CARMSG
028000000000     C                   SETON                                        61
028100000000E   2C                   ENDIF
028200000000E   1C                   ENDIF
028300000000     C*
028400000000     C* RICHIEDERE NON PIU' DI DODICI MESI
028500000000IF  1C     $ERR          IFEQ      '0'
028600000000     C     V1CDA2        SUB       V1CDAT        RISUL
028700000000IF  2C     RISUL         IFGE      100
028800000000     C                   MOVEL     '1'           $ERR
028900170303     C                   MOVEL     MSG(10)       $MSG
029000000000     C                   EXSR      CARMSG
029100000000     C                   SETON                                        6160
029200000000E   2C                   ENDIF
029300000000E   1C                   ENDIF
029400020325     C*
029500020325     C* ANNO INIZIALE DIVERSO DA ANNO FINALE (NN E' + POSSIBILE LANCIARE LA STAT
029600020325IF  1C     $ERR          IFEQ      '0'
029700020325     C                   MOVEL     V1CDAT        ANNODA            4 0
029800020325     C                   MOVEL     V1CDA2        ANNOA             4 0
029900020325IF  2C     ANNODA        IFNE      ANNOA
030000020325     C                   MOVEL     '1'           $ERR
030100170303     C                   MOVEL     MSG(11)       $MSG
030200020325     C                   EXSR      CARMSG
030300020325     C                   SETON                                        6061
030400020325E   2C                   ENDIF
030500020325E   1C                   ENDIF
030600000000     C*--------------------
030700170302     C* Controlla AUTISTA
030800000000     C*--------------------
030900000000IF  1C     $ERR          IFEQ      '0'
031000170306      *
031100170306     C* CODICE CLIENTE OBBLIGATORIO
031200170306IF  1C     V1CKSC        IFEQ      *BLANKS
031300170306     C     V1CKSC        OREQ      *ZEROS
031400170306     C                   MOVEL     '1'           $ERR
031500170306     C                   MOVEL     MSG(12)       $MSG
031600170306     C                   EXSR      CARMSG
031700170309     C                   SETON                                        72
031800170306E   1C                   ENDIF
031900170306     C*
032000170306     C* RICHIESTA RICERCA CON IL ?
032100170306IF  1C     $ERR          IFEQ      '0'
032200170306     C     '?'           SCAN      V1CKSC                                 21
032300170306IF  2C     *IN21         IFEQ      *ON
032400170306     C                   CLEAR                   DSXC
032500170306     C                   Z-ADD     DUTKCI        DXCCAP
032600170306     C                   MOVEL     WAbi          DXCAUT
032700170306     C                   CALL      'XCLIR'
032800170306     C                   PARM                    DSXC
032900170306     C                   MOVEA     DXCKSC        KSA
033000170306DO  3C     1             DO        28            J                 4 0
033100170306     C                   MOVE      KSA(J)        DSKSA
033200170306     C                   Z-ADD     DSKSN         KSC(J)
033300170306E   3C                   ENDDO
033400170306     C                   MOVEL     KSC(1)        V1CKSC
033500170306     C                   MOVEL     '1'           $TESVD
033600170306E   2C                   ENDIF
033700170306E   1C                   ENDIF
033800170306     C*
033900170306     C* CONTROLLA SE CLIENTE NUMERICO
034000170306IF  1C     $ERR          IFEQ      '0'
034100170306     C                   SETOFF                                       21
034200170306     C                   TESTN                   V1CKSC               21
034300170306IF  2C     *IN21         IFEQ      *ON
034400170306     C                   MOVE      V1CKSC        W001              1
034500170306IF  3C     W001          IFLT      '0'
034600170306     C                   MOVEL     '1'           $ERR
034700170306     C                   MOVEL     MSG(12)       $MSG
034800170306     C                   EXSR      CARMSG
034900170309     C                   SETON                                        72
035000170306E   3C                   ENDIF
035100170306X   2C                   ELSE
035200170306     C                   MOVEL     '1'           $ERR
035300170306     C                   MOVEL     MSG(12)       $MSG
035400170306     C                   EXSR      CARMSG
035500170309     C                   SETON                                        72
035600170306E   2C                   ENDIF
035700170306E   1C                   ENDIF
035800170306     C*
035900170306     C* CONTROLLA SE CLIENTE VALIDO
036000170306IF  1C     $ERR          IFEQ      '0'
036100170306     C                   CLEAR                   BS69DS
036200170306     C                   CLEAR                   ACODS
036300170306     C                   MOVEL     KNSIF         I69SIF
036400170306     C                   MOVEL     V1CKSC        I69KAC
036500170306     C                   CALL      'TIBS69R'
036600170306     C                   PARM                    BS69DS
036700170306     C                   PARM                    ACODS
036800170306     C                   PARM                    INDDS
036900170306     C                   PARM                    CLPDS
037000170306     C                   PARM                    CLSDS
037100170306IF  2C     O69ERR        IFNE      '1'
037200170309     C                   MOVEL     acorag        V1DKSC
037300170306X   2C                   ELSE
037400170306     C                   MOVEL     '1'           $ERR
037500170306     C                   MOVEL     MSG(12)       $MSG
037600170306     C                   EXSR      CARMSG
037700170309     C                   SETON                                        72
037800170306E   2C                   ENDIF
037900170306E   1C                   ENDIF
038000170306     C*
038100170306     C* CONTROLLA SE CLIENTE GESTIBILE:
038200170306IF  1C     $ERR          IFEQ      '0'
038300170306      * CONTROLLA SE PUÒ ESSERE VISTO OPPURE NO CON ABILITAZIONI ANCHE X FUORI A
038400170306      * MA PER AGENTE UNIFICANTE:
038500170306     C                   clear                   TAA1DS
038600170306     C                   EVAL        ITAA1CAUT = '§UTEIST'
038700170306     C                   move      V1CKSC        ITAA1KSC
038800170306     C                   EVAL      KPJBU = TAA1DS
038900170306     C                   CALL      'TNTAA1C'
039000170306     C                   PARM                    KPJBA
039100170306     C                   EVAL              TAA1DS = KPJBU
039200170306      *
039300170306      *  SE NON ABILITATO PROCEDO CON IL VECCHIO CONTROLLO SULLA FILIALE GESTITA
039400170306     C                   IF        OTAA1FABI <> 'S'
039500170306     C                   MOVEL     '1'           $ERR
039600170306     C                   MOVEL     MSG(14)       $MSG
039700170306     C                   EXSR      CARMSG
039800170309     C                   SETON                                        72
039900170306     c                   end
040000170309     C*
040100170309     C*------- GIORNO   di   F I N E   M E S E ---------------------*
040200170309     C*
040300170309     C* COMPONE IL PRIMO GIORNO DELLA DATA FINALE
040400170309     C                   MOVEL     V1CDA2        WDFIN             8 0
040500170309     C                   MOVE      28            WDFIN
040600170309     C*
040700170309     C*  Ultimo giorno del mese
040800170309     C                   MOVE      WDFIN         MMGG_4            4 0
040900170309     C                   MOVEL     MMGG_4        Mm_2              2 0
041000170309     C                   MOVEl     WDFIN         Anno_4            4 0
041100170309      * se è febbraio
041200170309     c                   If        Mm_2 = 2
041300170309      *
041400170309      *  potrebbe essere bisestile 29 gg.
041500170309     c     Anno_4        div       4             risulta           3 0
041600170309     c                   mvr                     resto             3 0
041700170309      *  Se bisestile
041800170309     c                   If        resto = 0
041900170309     C                   MOVE      '29'          WDFIN
042000170309     c                   end
042100170309      *
042200170309     c                   else
042300170309      *
042400170309      *  se invece Aprile, Giugno, Settembre, Novembre con 30 gg.
042500170309     c                   If           Mm_2 = 4 or Mm_2 = 6 or
042600170309     c                                Mm_2 = 9 or Mm_2 = 11
042700170309     C                   MOVE      '30'          WDFIN
042800170309     c                   else
042900170309     C                   MOVE      '31'          WDFIN
043000170309     c                   end
043100170309      *
043200170309     c                   endIf
043300170309     C*
043400170309     C*----------- F I N E    M E S E ---------------------------*
043500170306      *
043600170306     C* SE SCELTA CLIENTE UNIFICANTE, CONTROLLA E NEL CASO REPERISCE IL CLIENTE
043700170306IF  1C     $ERR          IFEQ      '0'
043800170306IF  2C     V1UNIF        IFEQ      'S'
043900170306     C                   CLEAR                   BS10DS
044000170309     C                   Z-ADD     WDFIN         D10DRF
044100170306     C                   MOVEL     'ST'          D10TLE
044200170306     C                   MOVEL     'P'           D10PAF
044300170306     C                   MOVE      V1CKSC        D10COD
044400170306     C                   CALL      'TIBS10R'
044500170306     C                   PARM                    BS10DS
044600170306IF  3C     D10ERR        IFEQ      *BLANKS
044700170306     C     D10COD        ANDNE     D10COP
044800170309     C                   MOVEL     DEC(1)        V1D001
044900170310     c                   eval      V1D001 = 'Attenzione. Il codice NON è unif-
045000170310     c                             icante.'
045100170310     c                   MOVE      D10COP        campo7            7
045200170310     c                   eval      V1D002 = 'L''unificante da utilizzare sare-
045300170310     c                             bbe -->' + campo7
045400170310     c                   eval      V1D003 = 'Sostituire il codice con l''unifi-
045500170310     c                             cante - oppure mettere "N" '
045600170309     C                   SETON                                        73
045700170306E   3C                   ENDIF
045800170306E   2C                   ENDIF
045900170306E   1C                   ENDIF
046000170306     C*
046100000000E   1C                   ENDIF
046200000614     C*
046300170306E   1C                   ENDIF
046400170306     C*
046500000000     C                   ENDSR
046600000000     C*------------------------------------------------------------------------*
046700950214     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
046800000000     C*------------------------------------------------------------------------*
046900000000     C     IMPVID        BEGSR
047000000614     C*
047100170303     C* IMPOSTO DI DEFAULT IL FLAG clienti
047200170309     c                   movel     'S'           V1unif
047300170306     c                   movel     '?'           v1cksc
047400000000     C*--------------------
047500000000     C* RAGIONE SOCIALE
047600000000     C*--------------------
047700020916     C                   MOVEL     RSUT          VTDRSU
047800000000     C*--------------------
047900000000     C* DATE DI ELABORAZIONE
048000000000     C*--------------------
048100000000     C***
048200170303     C* AA/MM FINALE   -->
048300170303     C* AA/MM INIZIALE -->
048400000000     C***
048500000000     C*--------------------
048600000000     C* D A T A   F I N E
048700000000     C*--------------------
048800170303     c                   move      *year         anno              4
048900170303     c                   move      *month        mese              2
049000170303     c                   movel     anno          annmes            6
049100170303     c                   move      mese          annmes
049200170306     C                   move      AnnMes        V1CDA2
049300000000     C*--------------------
049400000000     C* D A T A   I N I Z I O
049500000000     C*--------------------
049600170303     c                   move      '01'          mese              2
049700170309     c                   move      mese          annmes
049800170306     C                   move      AnnMes        V1CDAT
049900000000     C*--------------------
050000170303     C* MESSAGGI
050100000000     C*--------------------
050200170303     C                   MOVEL     *BLANKS       V1D001
050300170303     C                   MOVEL     *BLANKS       V1D002
050400170303     C                   MOVEL     *BLANKS       V1D003
050500170309     C                   MOVEL     *BLANKS       V1D004
050600000000     C*
050700000000     C                   ENDSR
050800020916     C*-------------------------------------------------------------------------
050900020916     C* REPERISCE I DATI UTENTE
051000020916     C*-------------------------------------------------------------------------
051100020916     C     REPDATIUTE    BEGSR
051200020916     C*
051300020916     C* INIZIALIZZA VARIABILI DI WRK
051400020916     C                   CLEAR                   TIBS34DS
051500020916     C                   CLEAR                   AZUTEDS
051600020916     C                   CLEAR                   DDATIUTE
051700020916     C*
051800020916     C     *DTAARA       DEFINE    §azute        azuteds
051900020916     C     *DTAARA       DEFINE    §datiute      ddatiute
052000020916     C                   IN(E)     *DTAARA
052100020916if  1C                   IF        %Error
052200020916     c                   EVAL      I34Tla = 'L'
052300020916     C                   CALL      'TIBS34R'
052400020916     C                   PARM                    Tibs34Ds
052500020916     C                   IN        *DTAARA
052600020916e   1C                   ENDIF
052700020916     C*
052800170306     C                   clear                   TAA1DS
052900170306     C                   EVAL        ITAA1CAUT = '§UTEIST'
053000170306     C                   EVAL      KPJBU = TAA1DS
053100170306     C                   CALL      'TNTAA1C'
053200170306     C                   PARM                    KPJBA
053300170306     C                   EVAL              TAA1DS = KPJBU
053400170306      *
053500170306x   2C                   if         OTAA1ERR ='S'
053600170306     C                   EVAL      $ERR  = '1'
053700170306     C                   EVAL      $FINE = '1'
053800170306e   2C                   Else
053900170306x   2C                   eval      wABI =  OTAA1CABI
054000170306e   2C                   ENDIF
054100170306     C*
054200020916     C                   ENDSR
054300000000     C*------------------------------------------------------------------------*
054400000000     C* *INZSR - ROUTINE INIZIALE
054500000000     C*------------------------------------------------------------------------*
054600000000     C     *INZSR        BEGSR
054700000000     C*
054800000000     C* RICEVIMENTO PARAMETRI
054900000000     C     *ENTRY        PLIST
055000000000     C                   PARM                    KPJBA
055100170306     C*
055200170306     C* IMNPOSTA DATA / ORA CORRENTE
055300170306     C                   TIME                    N14              14 0
055400170306     C                   MOVE      N14           G08DAT            8 0
055500170306     C                   Z-ADD     *ZEROS        G08INV
055600170306     C                   MOVEL     '0'           G08ERR
055700170306     C                   CALL      'XSRDA8'
055800170306     C                   PARM                    WLBDA8
055900170306     C                   Z-ADD     G08INV        DATCOR            8 0
056000000000     C*
056100000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
056200000000     C                   EXSR      INZSTD
056300000000     C*
056400000000     C* IMPOSTAZIONI INIZIALI VIDEO
056500000000     C                   EXSR      IMPVID
056600000000     C*
056700000000     C* EMETTO PRIMO FORMATO VIDEO
056800000000     C                   EXSR      CARV1
056900000000     C*
057000000000     C                   ENDSR
057100000000     C*------------------------------------------------------------------------*
057200000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
057300000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
057400000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
057500000000     C*      LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
057600000000     C*      STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
057700000000     C*      NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
057800000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
057900000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
058000000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
058100000000     C*------------------------------------------------------------------------*
058200000000     C* INZSTD - ROUTINE INIZIALE STANDARD
058300000000     C*------------------------------------------------------------------------*
058400000000     C     INZSTD        BEGSR
058500000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
058600000000     C                   CALL      'XKEYS'
058700000000     C                   PARM                    DSKEYS
058800000000     C                   MOVEA     KEYRIG        KEYS
058900000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
059000000000     C     XMSGPA        PLIST
059100000000     C                   PARM                    DSMSMS
059200000000     C                   PARM                    DSMSPG
059300000000     C                   PARM                    DSMSKE
059400000000     C                   PARM                    DSMSER
059500000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
059600010508     C                   MOVEL     'TISEA1R'     DSMSPG
059700010508     C                   MOVEL     'TISEA1R'     VMCQUE
059800000000     C                   MOVEL     *BLANKS       VMCKEY
059900000000     C                   MOVEL     *BLANKS       DSMSMS
060000000000     C* INIZIALIZZO FLAG CONTROLLO PGM
060100000000     C                   MOVEL     '0'           $ERR              1
060200000000     C                   MOVEL     '0'           $F03              1
060300000000     C                   MOVEL     *BLANKS       $GES              2
060400000000     C                   MOVEL     *BLANKS       $MSG             80
060500000000     C                   MOVEL     '0'           $FINE             1
060600000000     C                   MOVEL     '1'           $TESVD            1
060700000000     C                   MOVEL     'I'           $TGES             1
060800000000     C* LANCIO ROUTINE PER VERIFICARE SE
060900000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
061000000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
061100020916     C                   EXSR      REPDATIUTE
061200000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
061300000000     C                   BITOFF    '01234567'    POSMI1            1
061400000000     C                   BITOFF    '01234567'    POSMI2            1
061500000000     C                   BITOFF    '01234567'    POSMA1            1
061600000000     C                   BITOFF    '01234567'    POSMA2            1
061700000000     C                   BITON     '34'          POSMI1            1
061800000000     C                   BITON     '7'           POSMI2            1
061900000000     C                   BITON     '34'          POSMA1            1
062000000000     C                   BITON     '0'           POSMA2            1
062100000000     C                   MOVE      POSMI2        POSMIN            2
062200000000     C                   MOVEL     POSMI1        POSMIN
062300000000     C                   MOVE      POSMA2        POSMAX            2
062400000000     C                   MOVEL     POSMA1        POSMAX
062500000000     C                   ENDSR
062600000000     C*------------------------------------------------------------------------*
062700000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
062800000000     C*------------------------------------------------------------------------*
062900000000     C     FUNSTD        BEGSR
063000000000     C                   SELECT
063100000000     C     KEY           WHENEQ    F3
063200000000     C                   EXSR      F03STD
063300000000     C     KEY           WHENEQ    F12
063400000000     C                   EXSR      F12STD
063500000000     C     KEY           WHENEQ    F24
063600000000     C                   EXSR      F24STD
063700000000     C                   OTHER
063800000000     C                   MOVEL     MSG(2)        $MSG
063900000000     C                   EXSR      CARMSG
064000000000     C                   ENDSL
064100000000     C                   ENDSR
064200000000     C*------------------------------------------------------------------------*
064300000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
064400000000     C*------------------------------------------------------------------------*
064500000000     C     F03STD        BEGSR
064600000000     C*
064700000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
064800000000     C                   MOVEL     '1'           $FINE
064900000000     C*
065000000000     C                   ENDSR
065100000000     C*------------------------------------------------------------------------*
065200000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
065300000000     C*------------------------------------------------------------------------*
065400000000     C     F12STD        BEGSR
065500000000     C*
065600000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
065700000000     C                   MOVEL     '1'           $FINE
065800000000     C*
065900000000     C                   ENDSR
066000000000     C*------------------------------------------------------------------------*
066100000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
066200000000     C*------------------------------------------------------------------------*
066300000000     C     F24STD        BEGSR
066400000000     C*
066500000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
066600000000     C                   EXSR      OUTFUN
066700000000     C*
066800000000     C                   ENDSR
066900000000     C*------------------------------------------------------------------------*
067000000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
067100000000     C*          CODA MESSAGGI DEL PGM
067200000000     C*------------------------------------------------------------------------*
067300000000     C     CARMSG        BEGSR
067400000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
067500000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
067600000000     C     MSGULT        IFEQ      0
067700000000     C                   MOVEL     '1'           *IN40
067800170302     C                   WRITE     TREC18K
067900000000     C                   MOVEL     '0'           *IN40
068000000000     C                   ENDIF
068100000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
068200000000     C                   MOVEL     $MSG          DSMSMS
068300000000     C                   CALL      'XMSG'        XMSGPA
068400000000     C     DSMSER        IFEQ      0
068500000000     C                   MOVEL     DSMSKE        VMCKEY
068600000000     C                   ENDIF
068700000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
068800000000     C                   ADD       1             MSGULT            4 0
068900170302     C                   WRITE     TREC18M
069000000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
069100000000     C                   MOVEL     *BLANKS       $MSG
069200000000     C                   ENDSR
069300000000     C*------------------------------------------------------------------------*
069400000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
069500000000     C*------------------------------------------------------------------------*
069600000000     C     OUTMSG        BEGSR
069700000000     C* VISUALIZZAZIONE MESSAGGIO
069800000000     C                   MOVEL     '1'           *IN44
069900170302     C                   WRITE     TREC18K
070000000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
070100000000     C                   Z-ADD     *ZEROS        MSGULT
070200000000     C                   ENDSR
070300000000     C*------------------------------------------------------------------------*
070400000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
070500000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
070600000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
070700000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
070800000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
070900000000     C*------------------------------------------------------------------------*
071000000000     C     CARFUN        BEGSR
071100000000     C                   Z-ADD     *ZEROS        FUNULT            5 0
071200000000     C     1             DO        31            I                 5 0
071300000000     C                   MOVEL     FUN(I)        FUNRIG
071400000000     C                   MOVEL     FUNFUN        FFU(I)
071500000000     C                   MOVEL     FUNDES        FDE(I)
071600020916     C     DUTLPO        IFEQ      'S'
071700000000     C     $TGES         IFEQ      'G'
071800000000     C                   MOVEL     FUNASG        FAT(I)
071900000000     C                   ELSE
072000000000     C                   MOVEL     FUNASI        FAT(I)
072100000000     C                   ENDIF
072200000000     C                   ELSE
072300000000     C     $TGES         IFEQ      'G'
072400000000     C                   MOVEL     FUNAFG        FAT(I)
072500000000     C                   ELSE
072600000000     C                   MOVEL     FUNAFI        FAT(I)
072700000000     C                   ENDIF
072800000000     C                   ENDIF
072900000000     C     FAT(I)        IFEQ      'E'
073000000000     C                   MOVEL     USER          WEDP              3
073100000000     C     WEDP          IFEQ      'EDP'
073200000000     C                   MOVEL     'S'           FAT(I)
073300000000     C                   ELSE
073400000000     C                   MOVEL     'N'           FAT(I)
073500000000     C                   ENDIF
073600000000     C                   ENDIF
073700000000     C                   ENDDO
073800000000     C                   ENDSR
073900000000     C*------------------------------------------------------------------------*
074000000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
074100000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
074200000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
074300000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
074400000000     C*------------------------------------------------------------------------*
074500000000     C     OUTFUN        BEGSR
074600000000     C     1             DO        8             I
074700000000     C                   MOVEL     *BLANKS       FUV(I)
074800000000     C                   ENDDO
074900000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
075000000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0
075100000000     C     1             DO        31            I
075200000000     C     FAT(I)        IFEQ      'S'
075300000000     C     FDE(I)        ANDNE     *BLANKS
075400000000     C                   ADD       1             FUNNAT
075500000000     C                   ENDIF
075600000000     C                   ENDDO
075700000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
075800000000     C     FUNNAT        IFGT      8
075900000000     C                   MOVEL     'S'           FAT(24)
076000000000     C                   ENDIF
076100000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
076200000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
076300000000     C                   Z-ADD     8             FUNVIS            5 0
076400000000     C     FAT(23)       IFEQ      'S'
076500000000     C                   SUB       1             FUNVIS
076600000000     C                   ENDIF
076700000000     C     FAT(24)       IFEQ      'S'
076800000000     C                   SUB       1             FUNVIS
076900000000     C                   ENDIF
077000000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
077100000000     C                   Z-ADD     *ZEROS        W                 5 0
077200000000     C                   ADD       1             FUNULT
077300000000     C     FUNULT        IFGT      31
077400000000     C                   Z-ADD     1             FUNULT
077500000000     C                   ENDIF
077600000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
077700000000     C     FUNULT        DO        31            I
077800000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
077900000000     C     W             IFLT      FUNVIS
078000000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
078100000000     C     FAT(I)        IFEQ      'S'
078200000000     C     FDE(I)        ANDNE     *BLANKS
078300000000     C     I             IFEQ      23
078400000000     C     I             OREQ      24
078500000000     C                   ELSE
078600000000     C                   ADD       1             W
078700000000     C                   MOVEL     FDE(I)        FUV(W)
078800000000     C                   ENDIF
078900000000     C                   ENDIF
079000000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
079100000000     C                   Z-ADD     I             FUNULT
079200000000     C                   ENDIF
079300000000     C                   ENDDO
079400000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
079500000000     C                   MOVEL     FUV(1)        VFD001
079600000000     C                   MOVEL     FUV(2)        VFD002
079700000000     C                   MOVEL     FUV(3)        VFD003
079800000000     C                   MOVEL     FUV(4)        VFD004
079900000000     C                   MOVEL     FUV(5)        VFD005
080000000000     C                   MOVEL     FUV(6)        VFD006
080100000000     C     FAT(23)       IFEQ      'N'
080200000000     C     FAT(24)       ANDEQ     'N'
080300000000     C                   MOVEL     FUV(7)        VFD007
080400000000     C                   MOVEL     FUV(8)        VFD008
080500000000     C                   ELSE
080600000000     C     FAT(23)       IFEQ      'S'
080700000000     C     FAT(24)       ANDEQ     'S'
080800000000     C                   MOVEL     FDE(23)       VFD007
080900000000     C                   MOVEL     FDE(24)       VFD008
081000000000     C                   ELSE
081100000000     C                   MOVEL     FUV(7)        VFD007
081200000000     C     FAT(23)       IFEQ      'S'
081300000000     C                   MOVEL     FDE(23)       VFD008
081400000000     C                   ELSE
081500000000     C                   MOVEL     FDE(24)       VFD008
081600000000     C                   ENDIF
081700000000     C                   ENDIF
081800000000     C                   ENDIF
081900000000     C                   MOVEL     '0'           $TESVD
082000000000     C                   ENDSR
082100000000     C*------------------------------------------------------------------------*
082200000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
082300000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
082400000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
082500000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
082600000000     C*------------------------------------------------------------------------*
082700000000     C     CONFUN        BEGSR
082800000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
082900000000     C     1             DO        31            I
083000000000     C     KEY           IFEQ      KEYS(I)
083100000000     C* CONTROLLO SE IL TASTO E' ATTIVO
083200000000     C     FAT(I)        IFNE      'S'
083300000000     C                   MOVEL     MSG(1)        $MSG
083400000000     C                   EXSR      CARMSG
083500000000     C                   MOVEL     '1'           $ERR
083600000000     C                   ENDIF
083700000000     C                   ENDIF
083800000000     C                   ENDDO
083900000000     C                   ENDSR
084000170306** DEC  (DECODIFICHE)
084100170309Cliente selezionato      -->
084200170309Unificante da utilizzare -->
084300170306--
084400170306--
084500170306--
084600000000**   SCHIERA: MSG  (MESSAGGI)
084700170309TREC18R- Tasto funzionale non valido.                                          1
084800170309TREC18R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
084900170309TREC18R- Opzione non valida.                                                   3
085000170309TREC18R- Opzione abilitata ma non gestita. Avvertire CED.                      4
085100170309TREC18R- Inizio elenco.                                                        5
085200170309TREC18R- Fine elenco.                                                          6
085300170309TREC18R- Anno/Mese obbligatorio                                                7
085400170309TREC18R- Anno/Mese errato                                                      8
085500170309TREC18R- Anno/Mese finale inferiore all'Anno/Mese iniziale                     9
085600170309TREC18R- Impossibile richiedere più di 12 mesi                                10
085700170309TREC18R- Impossibile lanciare la statistica su anni differenti                11
085800170309TREC18R- Cliente errato. Inserire il codice cliente.                          12
085900170309TREC18R- Il cliente richiesto non è unificante                                13
086000170309TREC18R- Cliente non gestibile                                                14
086100170309TREC18R-                                                                      15
086200170309TREC18R-                                                                      16
086300170303TISE90R-                                                                      17
086400170309TREC18R-                                                                      18
086500170309TREC18R-                                                                      19
086600170309TREC18R-                                                                      20
086700170309TREC18R-                                                                      21
086800170309TREC18R-                                                                      22
086900170309TREC18R- Opzione obbligatoria.                                                23
087000170309TREC18R-                                                                      24
087100170309TREC18R-                                                                      25
087200170309TREC18R-                                                                      26
087300170309TREC18R-                                                                      27
087400170309TREC18R-                                                                      28
087500170309TREC18R-                                                                      29
087600170309TREC18R-                                                                      30
087700000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
087800000000F1    -                   -    NN     NN
087900000000F2    -                   -    NN     NN
088000000000F3    -F3=Fine            -    SS     SS
088100000000F4    -                   -    NN     NN
088200000000F5    -                   -    NN     NN
088300000000F6    -F6=Lancio          -    SS     SS
088400000000F7    -                   -    NN     NN
088500000000F8    -                   -    NN     NN
088600000000F9    -                   -    NN     NN
088700000000F10   -                   -    NN     NN
088800000000F11   -                   -    NN     NN
088900000000F12   -F12=Ritorno        -    SS     SS
089000000000F13   -                   -    NN     NN
089100000000F14   -                   -    NN     NN
089200000000F15   -                   -    NN     NN
089300000000F16   -                   -    NN     NN
089400000000F17   -                   -    NN     NN
089500000000F18   -                   -    NN     NN
089600000000F19   -                   -    NN     NN
089700000000F20   -                   -    NN     NN
089800000000F21   -                   -    NN     NN
089900000000F22   -                   -    NN     NN
090000000000F23   -                   -    NN     NN
090100000000F24   -F24=Altri tasti    -    NN     NN
090200000000ENTER -                   -    SS     SS
090300000000ROLLUP-                   -    NN     NN
090400000000ROLLDN-                   -    NN     NN
090500000000HELP  -                   -    NN     NN
090600000000PRINT -                   -    NN     NN
090700000000BSPACE-                   -    NN     NN
090800000000CLEAR -                   -    NN     NN
