000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200980528     F* FNLV02R  *---------------------------------------------------*
000300980528     F* - INTERROGAZIONE BOLLE INTRAGRUPPO                           *
000400000000     F*--------------------------------------------------------------*
000500020718     FAZORG01L  IF   E           K DISK
000600980528     D CK              S              1    DIM(53) CTDATA PERRCD(53)
000700980528     D RM              S              1    DIM(53) CTDATA PERRCD(53)
000800980528     D AD              S              1    DIM(53) CTDATA PERRCD(53)
000900980528     D S201            S             10    DIM(1) CTDATA PERRCD(1)
001000980528     D S101            S             10    DIM(1) CTDATA PERRCD(1)
001100980528     D L201            S             10    DIM(1) CTDATA PERRCD(1)
001200980528     D L101            S             10    DIM(1) CTDATA PERRCD(1)
001300021002     D DLTD            S             32    DIM(2) CTDATA PERRCD(1)
001400980528     D KPJBA         E DS
001500020718     D DSBS55        E DS                  EXTNAME(TIBS55DS)
001600020718     D CNCR80        E DS
001700020718     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001800980528     D PARAM           DS
001900980603     D  CALPGM               201    210
002000980603     D  PARAZI               211    211
002100980603     D  PARF9                212    212
002200980603     D  PARF3                213    213
002300980528     I***************************************************************
002400980528     C**
002500980528     C* SE CE UN SOLO S.I. FACCIO UN CALL DIRETTA
002600980528     C*  E AVVISO IL PGM CHIAMATO DI NON ABILITARE IL TASTO CAMBIO AZIE
002700980528    1C     WES201        IFEQ      'N'
002800980528     C     WES101        OREQ      'N'
002900980528     C                   MOVEL     'N'           PARAZI
003000980528     C                   ELSE
003100980528     C                   MOVEL     'S'           PARAZI
003200021002     C                   EXSR      NODTAARA
003300980528     C                   ENDIF
003400980528     C**
003500980603     C     CALLP         TAG
003600980528     C                   MOVEL     PARAM         KPJBU
003700980528     C                   CALL      CALPGM
003800980528     C                   PARM                    KPJBA
003900980528     C                   MOVEL     KPJBU         PARAM
004000980608     C*
004100020718     C* SE C'E' SOLO UNA LIbRERIA E' PER FORZA USCITA
004200980608    1C     PARAZI        IFEQ      'N'
004300020719     C                   GOTO      FINETOTAL
004400980608   X1C                   ELSE
004500980603     C* F3 - FINE LAVORO
004600980608    2C     PARF3         IFEQ      '1'
004700980528     C                   GOTO      FINE
004800980608    2C                   ENDIF
004900980603     C**
005000980603     C* F9 - CAMBIO AZIENDA
005100980528     C**
005200980528     C                   EXSR      SELEZ
005300980603     C**
005400980603     C                   CLEAR                   PARF3
005500980603     C                   CLEAR                   PARF9
005600021002     C**
005700021002     C* CANCELLO LE DATA AREA DI AZUTE CHE POSSONO DARE PROBLEMI
005800021002     C                   EXSR      NODTAARA
005900021002     C**
006000980603     C                   GOTO      CALLP
006100980528     C**
006200980528     C     FINE          TAG
006300021114
006400021114     C                   EXSR      NODTAARA
006500021114
006600980528     C* VEDO ORA IN QUALE S.I. SONO
006700020718     C                   CLEAR                   DSBS55
006800020718     C                   CALL      'TIBS55R'
006900020718     C                   PARM                    DSBS55
007000020718     C                   MOVEL     O50PlA        W001RA            1
007100020718     C                   MOVEL     WPlA          W001SI            1
007200980528     C**
007300980528     C** REIMPOSTO LE LIBRERIE INIZIALI
007400980608    2C     W001RA        IFNE      W001SI
007500980528     C                   EXSR      RMVLIB
007600980528     C**
007700980528     C                   MOVEL     WPSI          KNSIF
007800980528     C** ADDLIBLE
007900020718     C                   MOVEA     O50PLR        AD(43)
008000980528     C                   MOVEA     WPLA          AD(14)
008100980528     C**
008200980528     C                   MOVEA     AD            COMMAN
008300980528     C                   CALL      'QCMDEXC'                            90
008400980528     C                   PARM                    COMMAN
008500980528     C                   PARM                    LUNG
008600020718    1C                   ENDIF
008700980603     C**
008800020719     c     FINETOTAL     TAG
008900020718     C                   CLEAR                   DSBS55
009000980603     C                   MOVEL     'C'           I50TLA
009100020718     C                   CALL      'TIBS55R'
009200020718     C                   PARM                    DSBS55
009300980608    1C                   ENDIF
009400980528     C**
009500900514     C                   SETON                                        LR
009600980528     C****************************************************************
009700980528     C     *INZSR        BEGSR
009800020718     c*
009900980528     C     *ENTRY        PLIST
010000980528     C                   PARM                    KPJBA
010100980528     C                   MOVEL     KPJBU         PARAM
010200020718     C**
010300020718     C                   Z-ADD     1             CODUT
010400020718     C                   CALL      'X§PARUT'
010500020718     C                   PARM                    UT§DSE
010600020718     C                   MOVEL     REC80         CNCR80
010700020718     C**
010800980528     c**
010900020718     C* SE utente di sede che interroga le bolle dell'ambiente di filiale in
011000020718     C*  sede, oppure sono un utente nell'ambiente di filiale in sede
011100020718     c*  --> non abilito il cambio azienda perchè non c'e'
011200020718     c     simfel        chain     azorg01l                           30
011300020718     c                   if        *in30
011400020718     c                   eval      orgfel=simfel
011500020718     c                   endif
011600020718     c
011700020718     c                   select
011800020718     c                   when      orgfel=46 or orgfel=0
011900020718     c                   eval      wes101='N'
012000020718     c                   eval      wes201='N'
012100020718
012200020718     c* Se sono un p.o. sdi, per forza abilito il cambio S.I.
012300020718     c                   when      orgdit='SDI'
012400020718     c                   eval      wes101='S'
012500020718     c                   eval      wes201='S'
012600020718     c                   exsr      SALVADATI
012700020718     c
012800020718     c                   other
012900020718     c                   exsr      SALVADATI
013000980528     C**
013100980528     C                   MOVEL     'S'           WES201            1
013200980528     C                   MOVEL     'S'           WES101            1
013300980528     C* CONTROLLO ESISTENZA SDITRA101
013400980528     C                   MOVEA     L101(1)       CK(14)
013500980528     C                   MOVEA     CK            COMMAN           80
013600980528     C                   CALL      'QCMDEXC'                            41
013700980528     C                   PARM                    COMMAN
013800980528     C                   PARM                    LUNG             15 5
013900980528     C** 41 ON - NON TROVATA
014000980528     C   41              MOVEL     'N'           WES101
014100020718     C                   ENDSL
014200980528     C                   ENDSR
014300980528     C**
014400980528     C* SELEZIONE INTERROGAZIONE -------------------------------------*
014500980528     C     SELEZ         BEGSR
014600980528     C* VEDO ORA IN QUALE S.I. SONO
014700020718     C                   CLEAR                   DSBS55
014800020718     C                   CALL      'TIBS55R'
014900020718     C                   PARM                    DSBS55
015000980528     C**
015100020718     C                   MOVEL     O50pla        W001RA            1
015200980603     C** W001RA - FLAG S .I. IN CUI SONO
015300980603     C** W001SI - FLAG S .I. IN CUI DEVO ANDARE
015400020718    1C     W001RA        IFEQ      'G'
015500980603     C                   MOVEL     'S'           W001SI
015600980603     C                   ELSE
015700020718     C                   MOVEL     'G'           W001SI
015800980603    1C                   ENDIF
015900980528     C**
016000980528     C** RIMUOVO LIBERIE
016100980528     C                   EXSR      RMVLIB
016200980528     C**
016300980528     C** ADDLIBLE
016400020718     C                   MOVEA     O50PLR        AD(43)
016500020718    1C     W001SI        IFEQ      'G'
016600980528     C                   MOVEL     S201(1)       KNSIF
016700980528     C                   MOVEA     L201(1)       AD(14)
016800980528     C                   ELSE
016900980528     C                   MOVEL     S101(1)       KNSIF
017000980528     C                   MOVEA     L101(1)       AD(14)
017100980603    1C                   ENDIF
017200980528     C**
017300980528     C                   MOVEA     AD            COMMAN
017400020409     C                   CALL      'QCMDEXC'                            90
017500980528     C                   PARM                    COMMAN
017600980528     C                   PARM                    LUNG
017700980528     C**
017800980528     C                   ENDSR
017900980528     C**
018000980528     C** RIMOZIONE LIBRERIE ------------------------------------------*
018100980528     C     RMVLIB        BEGSR
018200020718     C* RIMUOVO LA LIBRERIA DATi
018300980528     C                   MOVEA     O50PLA        RM(14)
018400980528     C                   MOVEA     RM            COMMAN
018500980528     C                   CALL      'QCMDEXC'                            90
018600980528     C                   PARM                    COMMAN
018700980528     C                   PARM                    LUNG
018800980528     C                   ENDSR
018900020718     C**
019000020718     C* SELVA DATI DELLE LIBREREI E S.I. IN CUI SONO -----------------*
019100020718     c     SALVADATI     BEGSR
019200020718     C*  ESEGUO CALL PER REPERIRE LIBRERIA E S.I.
019300020718     C                   CLEAR                   DSBS55
019400020718     C                   MOVEL     SIMFEL        I50PPO
019500020718     C                   CALL      'TIBS55R'
019600020718     C                   PARM                    DSBS55
019700020718     C**
019800020718     C* SALVO LA LIBRERIA DELL'AZIENDA
019900020718     C                   MOVEL     O50PLA        WPLA
020000020718     C     *LIKE         DEFINE    O50PLA        WPLA
020100020718     C* SALVO SISTEMA INFORMATIVO
020200020718     C                   MOVEL     O50PSI        WPSI
020300020718     C     *LIKE         DEFINE    O50PSI        WPSI
020400020718     C**
020500020718     C**  SISTEMO EVENTUALE S.I.  E LIBRERIA DATI PER I S.I. DI PROVA
020600020826     C                   MOVE      WPLA          W002A             2
020700020826     C                   if        w002a='D '
020800020826     C                   MOVE      WPLA          W004A             4
020900020718     C                   MOVE      W004A         L201(1)
021000020718     C                   MOVE      W004A         L101(1)
021100020718     C**
021200020718     C                   MOVE      WPSI          W004A             4
021300020718     C                   MOVE      W004A         S201(1)
021400020718     C                   MOVE      W004A         S101(1)
021500020826     c                   endif
021600020826     c
021700020718     C                   Z-ADD     53            LUNG
021800020718     c                   endsr
021900021002     C**
022000021002     C* CANCELLO LE DATA AREA DI QTEMP CHE POSSONO DARMI PROBLEMI-----*
022100021002     C     NODTAARA      BEGSR
022200021002     C                   MOVEA(P)  DLTD(1)       COMMAN
022300021002     C                   Z-ADD     32            LUNG
022400021002     C                   CALL      'QCMDEXC'                            90
022500021002     C                   PARM                    COMMAN
022600021002     C                   PARM                    LUNG
022700021002     C                   MOVEA(P)  DLTD(2)       COMMAN
022800021002     C                   CALL      'QCMDEXC'                            90
022900021002     C                   PARM                    COMMAN
023000021002     C                   PARM                    LUNG
023100021002     C                   Z-ADD     53            LUNG
023200021002     C                   ENDSR
023300980528**
023400980528CHKOBJ   OBJ(XXXXXXXXXX) OBJTYPE(*LIB)
023500980528**
023600980528RMVLIBLE LIB(XXXXXXXXXX)
023700980528**
023800980528ADDLIBLE LIB(XXXXXXXXXX)  POSITION(*AFTER XXXXXXXXXX)
023900020718**
024000020718GAITRA201
024100980528**
024200980528SDITRA101
024300980528**
024400980528GAITRA201
024500980528**
024600980528SDITRA101
024700021002**
024800021002DLTDTAARA   DTAARA(QTEMP/§AZUTE)
024900021002DLTDTAARA DTAARA(QTEMP/§DATIUTE)
