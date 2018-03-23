000100041117     H DECEDIT('0,') DATEDIT(*YMD.)
000200041117      * fnlg25r *----------------------------------------------------*
000300041117      *  Aggiorna flag legami bolla sulle bolle in titas
000400041117      *  richiamato da fnlg20r: l'unico che fa write su tfnlbl
000500021008      *--------------------------------------------------------------*
000600041130     FFNLBL02L  uF A E           K DISK
000700990809     FTITAS30C  UF   E           K DISK    USROPN
000800021008
000900041122     D OVRTAS          S              1    DIM(39) CTDATA PERRCD(39)
001000021008      *
001100041117     drecordlbl      E DS                  extname(fnlbl00f)
001200041117     D DSBS50        E DS                  EXTNAME(TIBS55DS)
001300041119     D wsavft1         s                   like(lblft1)
001400061211     D wagglbl         s              1
001500041122     D Ctas            C                   CONST('/TITAS30C)')
001600021008
001700990809     I* PREDISPONGO GLI INDICATORI DI TIPO RECORD PER TITAS
001800990809     ITITAS000      22
001900990809     ITITAS010      23
002000990809     ITITASP00      24
002100000000     C*---------------------------------------------------------------*
002200000000     C     *ENTRY        PLIST
002300041117     C                   PARM                    recordlbl
002400041117     C                   PARM                    I25tla            1
002500061211     C*
002600061211     C                   PARM                    I25agglbl         1
002700061211     c* dati di output se si vuole aggiornare titas non in questo pgm
002800061211     c* e di conseguenza anche il legame bolla (che quindi viene letto
002900061211     c*  non in update. In realtà serve solo per il TRTR36R quando
003000061211     c*  riceve in sede una bolla figlia per non aggiornare il record
003100061211     c*  figlia per il flag di legame bolla dopo la write)
003200061211     C                   PARM                    o25fa1            1
003300061211     C                   PARM                    o25fca            1
003400061211     C                   PARM                    o25bofi           1
003500061211     C                   PARM                    o25boma           1
003600061211     c
003700061211     c                   if        %parms>2
003800061211     c                   clear                   o25fa1
003900061211     c                   clear                   o25fca
004000061211     c                   clear                   o25bofi
004100061211     c                   clear                   o25boma
004200061211     c                   eval      wagglbl=i25agglbl
004300061211     c                   else
004400061211     c                   eval      wagglbl='S'
004500061211     c                   endif
004600941122     C*
004700900618     C*---------------------------------------------------------------*
004800941122     C     KLBL2         KLIST
004900950104     C                   KFLD                    LBLAAP
005000911106     C                   KFLD                    LBLLPP
005100911106     C                   KFLD                    LBLNRP
005200911106     C                   KFLD                    LBLNSP
005300950104     C                   KFLD                    LBLAAN
005400911106     C                   KFLD                    LBLLPN
005500911106     C                   KFLD                    LBLNRN
005600911106     C                   KFLD                    LBLNSN
005700941007     C     KLBLP         KLIST
005800950104     C                   KFLD                    LBLAAP
005900941007     C                   KFLD                    LBLLPP
006000941007     C                   KFLD                    LBLNRP
006100941007     C                   KFLD                    LBLNSP
006200941007     C     KLBLN         KLIST
006300950104     C                   KFLD                    LBLAAN
006400941007     C                   KFLD                    LBLLPN
006500941007     C                   KFLD                    LBLNRN
006600941007     C                   KFLD                    LBLNSN
006700941007     C*---------------------------------------------------------------*
006800041117     c* I25tla= C solo chiusura files
006900041117    1c                   if        i25tla<>'C'
007000041117     c
007100061211     c                   if        wagglbl='N'
007200061211     C     KLBL2         chain(N)  FNLBL02L
007300061211     c                   else
007400061211     C     KLBL2         chain     FNLBL02L
007500061211     c                   endif
007600061211     c
007700041119    2C                   if        %found(fnlbl02l)  and
007800041119     C                             (lblft1=' ' or lblft2=' ')
007900041119     c* Se primo record sflaggato e secondo flaggato, riaggiorno anche il
008000041119     c* secondo
008100041119     c                   eval       wsavft1=lblft1
008200041119     c
008300941007     C***
008400041117     C* LEGGO TITAS CON BOLLA MAMMA se non l'ho già aggiornata
008500941007     C***
008600941128     C                   CLEAR                   WFCA
008700941126     C                   SETOFF                                       222324
008800050408     C     KLBLP         chain     TITAS30C                           32
008900061211     c
009000061211     c                   if        *in32 and %parms>2
009100061211     c                   eval      o25boma='N'
009200061211     c                   endif
009300061211     c
009400941007    3C     *IN32         DOWEQ     *OFF
009500941007     C                   MOVEL     'S'           TASFA1
009600941128     C* SE LA MAMMA HA IL C/A LO MEMORIZZO ANCHE SULLA FIGLIA
009700941128     C     TASFCA        IFEQ      'S'
009800941128     C                   MOVEL     'S'           WFCA              1
009900941128     C                   ENDIF
010000941128     C*
010100941126     C   22              EXCEPT    AGTAS0
010200941126     C   23              EXCEPT    AGTAS1
010300941126     C   24              EXCEPT    AGTASP
010400041117     c                   eval      lblft1='S'
010500041119     c* solo la prima volta aggiorno la data
010600041119     c                   if        lbldt1=0
010700041117     c                   movel     *date         lbldt1
010800041119     c                   endif
010900061211     c
011000061211     c                   if        %parms>2
011100061211     c                   eval      o25boma='S'
011200061211     c                   endif
011300941007     C*
011400941126     C                   SETOFF                                       222324
011500990809     C     KLBLP         READE     TITAS30C                               32
011600941007    3C                   ENDDO
011700941007     C**
011800041117     C* LEGGO TITAS CON BOLLA FIGLIA  solo se la bolla c'e' (ft2=*blanks)
011900041117     c*  altrimenti ft2 = N impostato da fnlg20r)
012000061211     c*  se non trovo la bolla, passo in output i flag da aggiornare
012100941122D    C**
012200041119    3c                   if        lblft2=' '   or wsavft1=' '
012300941126     C                   SETOFF                                       222324
012400050408     C     KLBLN         chain     TITAS30C                           32
012500061211     c
012600061211   3ac                   if        *in32
012700061211     c* Dati di output
012800061211    4c                   if        %parms>2
012900061211     c                   eval      o25fa1='S'
013000061211     c                   eval      o25fca=wfca
013100061211     c                   eval      o25bofi='N'
013200061211    4c                   endif
013300061211     c
013400061211   3ac                   else
013500061211     c
013600041117    4C     *IN32         DOWEQ     *OFF
013700941007     C                   MOVEL     'S'           TASFA1
013800941128     C                   MOVEL     WFCA          TASFCA
013900061211     c                   eval      o25bofi='S'
014000941128     C   22              EXCEPT    AGTAS0
014100941128     C   23              EXCEPT    AGTAS1
014200941128     C   24              EXCEPT    AGTASP
014300041117     c                   eval      lblft2='S'
014400041117     c                   movel     *date         lbldt2
014500941007     C*
014600941126     C                   SETOFF                                       222324
014700990809     C     KLBLN         READE     TITAS30C                               32
014800041117    4C                   ENDDO
014900061211   3ac                   endif
015000061211    3c                   endif
015100041117    c
015200061211     c                   if        wagglbl<>'N'
015300041130     c                   update    fnlbl000
015400041119    2c                   endif
015500061211     c
015600061211    2c                   endif
015700041117    1c                   endif
015800941123     C*
015900041117     c                   if        i25tla=' '
016000041117     C                   MOVEL     *on           *INRT
016100041117     c                   else
016200021008     C                   MOVEL     *ON           *INLR
016300041117     c                   endif
016400041117     C**************************************************************************
016500041117     C* PER FARLO FUNZIONARE SIA IN PROVA CHE IN BUONA
016600041117     C     *INZSR        begsr
016700041117     C* CERCO CONIL TIBS55 LA LIBRERIA DI GRUPPO DI SEDE
016800041117     C                   CLEAR                   DSBS50
016900041117     c                   movel     'L'           i50tla
017000041119     c                   Movel     046           I50apo
017100041117     C                   CALL      'TIBS55R'
017200041117     C                   PARM                    DSBS50
017300041117     C
017400041117     C* overdbf  TITAS30C
017500041122     c                   movea     o50alg        ovrtas(30)
017600941007     C                   SETOFF                                       01
017700041117     C                   CLEAR                   COMMAN           80
017800041117     C                   MOVEA     ovrtas        COMMAN
017900041122     c                   eval      comman=%trim(comman)+ctas
018000041117     C                   CALL      'QCMDEXC'
018100941007     C                   PARM                    COMMAN
018200041119     C                   PARM      50            LUNG             15 5
018300990809     C* 01 OFF - C'E' TITAS
018400041117     C                   OPEN      TITAS30C
018500950104     C*
018600911001     C                   ENDSR
018700950104     C**************************************************************************
018800990809     OTITAS000  E            AGTAS0
018900941122     O                       TASFA1
019000941129     O                       TASFCA
019100990809     OTITAS010  E            AGTAS1
019200941122     O                       TASFA1
019300941129     O                       TASFCA
019400990809     OTITASP00  E            AGTASP
019500941126     O                       TASFA1
019600941129     O                       TASFCA
019700950104     O**************************************************************************
019800041117**         OVRTAS
019900041122OVRDBF FILE(TITAS30C) TOFILE(XXXXXXXXXX
