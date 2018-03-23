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
006300980528     C* VEDO ORA IN QUALE S.I. SONO
006400020718     C                   CLEAR                   DSBS55
006500020718     C                   CALL      'TIBS55R'
006600020718     C                   PARM                    DSBS55
006700020718     C                   MOVEL     O50PlA        W001RA            1
006800020718     C                   MOVEL     WPlA          W001SI            1
006900980528     C**
007000980528     C** REIMPOSTO LE LIBRERIE INIZIALI
007100980608    2C     W001RA        IFNE      W001SI
007200980528     C                   EXSR      RMVLIB
007300980528     C**
007400980528     C                   MOVEL     WPSI          KNSIF
007500980528     C** ADDLIBLE
007600020718     C                   MOVEA     O50PLR        AD(43)
007700980528     C                   MOVEA     WPLA          AD(14)
007800980528     C**
007900980528     C                   MOVEA     AD            COMMAN
008000980528     C                   CALL      'QCMDEXC'                            90
008100980528     C                   PARM                    COMMAN
008200980528     C                   PARM                    LUNG
008300020718    1C                   ENDIF
008400980603     C**
008500020719     c     FINETOTAL     TAG
008600020718     C                   CLEAR                   DSBS55
008700980603     C                   MOVEL     'C'           I50TLA
008800020718     C                   CALL      'TIBS55R'
008900020718     C                   PARM                    DSBS55
009000980608    1C                   ENDIF
009100980528     C**
009200900514     C                   SETON                                        LR
009300980528     C****************************************************************
009400980528     C     *INZSR        BEGSR
009500020718     c*
009600980528     C     *ENTRY        PLIST
009700980528     C                   PARM                    KPJBA
009800980528     C                   MOVEL     KPJBU         PARAM
009900020718     C**
010000020718     C                   Z-ADD     1             CODUT
010100020718     C                   CALL      'X§PARUT'
010200020718     C                   PARM                    UT§DSE
010300020718     C                   MOVEL     REC80         CNCR80
010400020718     C**
010500980528     c**
010600020718     C* SE utente di sede che interroga le bolle dell'ambiente di filiale in
010700020718     C*  sede, oppure sono un utente nell'ambiente di filiale in sede
010800020718     c*  --> non abilito il cambio azienda perchè non c'e'
010900020718     c     simfel        chain     azorg01l                           30
011000020718     c                   if        *in30
011100020718     c                   eval      orgfel=simfel
011200020718     c                   endif
011300020718     c
011400020718     c                   select
011500020718     c                   when      orgfel=46 or orgfel=0
011600020718     c                   eval      wes101='N'
011700020718     c                   eval      wes201='N'
011800020718
011900020718     c* Se sono un p.o. sdi, per forza abilito il cambio S.I.
012000020718     c                   when      orgdit='SDI'
012100020718     c                   eval      wes101='S'
012200020718     c                   eval      wes201='S'
012300020718     c                   exsr      SALVADATI
012400020718     c
012500020718     c                   other
012600020718     c                   exsr      SALVADATI
012700980528     C**
012800980528     C                   MOVEL     'S'           WES201            1
012900980528     C                   MOVEL     'S'           WES101            1
013000980528     C* CONTROLLO ESISTENZA SDITRA101
013100980528     C                   MOVEA     L101(1)       CK(14)
013200980528     C                   MOVEA     CK            COMMAN           80
013300980528     C                   CALL      'QCMDEXC'                            41
013400980528     C                   PARM                    COMMAN
013500980528     C                   PARM                    LUNG             15 5
013600980528     C** 41 ON - NON TROVATA
013700980528     C   41              MOVEL     'N'           WES101
013800020718     C                   ENDSL
013900980528     C                   ENDSR
014000980528     C**
014100980528     C* SELEZIONE INTERROGAZIONE -------------------------------------*
014200980528     C     SELEZ         BEGSR
014300980528     C* VEDO ORA IN QUALE S.I. SONO
014400020718     C                   CLEAR                   DSBS55
014500020718     C                   CALL      'TIBS55R'
014600020718     C                   PARM                    DSBS55
014700980528     C**
014800020718     C                   MOVEL     O50pla        W001RA            1
014900980603     C** W001RA - FLAG S .I. IN CUI SONO
015000980603     C** W001SI - FLAG S .I. IN CUI DEVO ANDARE
015100020718    1C     W001RA        IFEQ      'G'
015200980603     C                   MOVEL     'S'           W001SI
015300980603     C                   ELSE
015400020718     C                   MOVEL     'G'           W001SI
015500980603    1C                   ENDIF
015600980528     C**
015700980528     C** RIMUOVO LIBERIE
015800980528     C                   EXSR      RMVLIB
015900980528     C**
016000980528     C** ADDLIBLE
016100020718     C                   MOVEA     O50PLR        AD(43)
016200020718    1C     W001SI        IFEQ      'G'
016300980528     C                   MOVEL     S201(1)       KNSIF
016400980528     C                   MOVEA     L201(1)       AD(14)
016500980528     C                   ELSE
016600980528     C                   MOVEL     S101(1)       KNSIF
016700980528     C                   MOVEA     L101(1)       AD(14)
016800980603    1C                   ENDIF
016900980528     C**
017000980528     C                   MOVEA     AD            COMMAN
017100020409     C                   CALL      'QCMDEXC'                            90
017200980528     C                   PARM                    COMMAN
017300980528     C                   PARM                    LUNG
017400980528     C**
017500980528     C                   ENDSR
017600980528     C**
017700980528     C** RIMOZIONE LIBRERIE ------------------------------------------*
017800980528     C     RMVLIB        BEGSR
017900020718     C* RIMUOVO LA LIBRERIA DATi
018000980528     C                   MOVEA     O50PLA        RM(14)
018100980528     C                   MOVEA     RM            COMMAN
018200980528     C                   CALL      'QCMDEXC'                            90
018300980528     C                   PARM                    COMMAN
018400980528     C                   PARM                    LUNG
018500980528     C                   ENDSR
018600020718     C**
018700020718     C* SELVA DATI DELLE LIBREREI E S.I. IN CUI SONO -----------------*
018800020718     c     SALVADATI     BEGSR
018900020718     C*  ESEGUO CALL PER REPERIRE LIBRERIA E S.I.
019000020718     C                   CLEAR                   DSBS55
019100020718     C                   MOVEL     SIMFEL        I50PPO
019200020718     C                   CALL      'TIBS55R'
019300020718     C                   PARM                    DSBS55
019400020718     C**
019500020718     C* SALVO LA LIBRERIA DELL'AZIENDA
019600020718     C                   MOVEL     O50PLA        WPLA
019700020718     C     *LIKE         DEFINE    O50PLA        WPLA
019800020718     C* SALVO SISTEMA INFORMATIVO
019900020718     C                   MOVEL     O50PSI        WPSI
020000020718     C     *LIKE         DEFINE    O50PSI        WPSI
020100020718     C**
020200020718     C**  SISTEMO EVENTUALE S.I.  E LIBRERIA DATI PER I S.I. DI PROVA
020300020826     C                   MOVE      WPLA          W002A             2
020400020826     C                   if        w002a='D '
020500020826     C                   MOVE      WPLA          W004A             4
020600020718     C                   MOVE      W004A         L201(1)
020700020718     C                   MOVE      W004A         L101(1)
020800020718     C**
020900020718     C                   MOVE      WPSI          W004A             4
021000020718     C                   MOVE      W004A         S201(1)
021100020718     C                   MOVE      W004A         S101(1)
021200020826     c                   endif
021300020826     c
021400020718     C                   Z-ADD     53            LUNG
021500020718     c                   endsr
021600021002     C**
021700021002     C* CANCELLO LE DATA AREA DI QTEMP CHE POSSONO DARMI PROBLEMI-----*
021800021002     C     NODTAARA      BEGSR
021900021002     C                   MOVEA(P)  DLTD(1)       COMMAN
022000021002     C                   Z-ADD     32            LUNG
022100021002     C                   CALL      'QCMDEXC'                            90
022200021002     C                   PARM                    COMMAN
022300021002     C                   PARM                    LUNG
022400021002     C                   MOVEA(P)  DLTD(2)       COMMAN
022500021002     C                   CALL      'QCMDEXC'                            90
022600021002     C                   PARM                    COMMAN
022700021002     C                   PARM                    LUNG
022800021002     C                   Z-ADD     53            LUNG
022900021002     C                   ENDSR
023000980528**
023100980528CHKOBJ   OBJ(XXXXXXXXXX) OBJTYPE(*LIB)
023200980528**
023300980528RMVLIBLE LIB(XXXXXXXXXX)
023400980528**
023500980528ADDLIBLE LIB(XXXXXXXXXX)  POSITION(*AFTER XXXXXXXXXX)
023600020718**
023700020718GAITRA201
023800980528**
023900980528SDITRA101
024000980528**
024100980528GAITRA201
024200980528**
024300980528SDITRA101
024400021002**
024500021002DLTDTAARA   DTAARA(QTEMP/§AZUTE)
024600021002DLTDTAARA DTAARA(QTEMP/§DATIUTE)
