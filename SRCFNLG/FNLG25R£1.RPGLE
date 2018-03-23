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
001400041122     D Ctas            C                   CONST('/TITAS30C)')
001500021008
001600990809     I* PREDISPONGO GLI INDICATORI DI TIPO RECORD PER TITAS
001700990809     ITITAS000      22
001800990809     ITITAS010      23
001900990809     ITITASP00      24
002000000000     C*---------------------------------------------------------------*
002100000000     C     *ENTRY        PLIST
002200041117     C                   PARM                    recordlbl
002300041117     C                   PARM                    I25tla            1
002400941122     C*
002500900618     C*---------------------------------------------------------------*
002600941122     C     KLBL2         KLIST
002700950104     C                   KFLD                    LBLAAP
002800911106     C                   KFLD                    LBLLPP
002900911106     C                   KFLD                    LBLNRP
003000911106     C                   KFLD                    LBLNSP
003100950104     C                   KFLD                    LBLAAN
003200911106     C                   KFLD                    LBLLPN
003300911106     C                   KFLD                    LBLNRN
003400911106     C                   KFLD                    LBLNSN
003500941007     C     KLBLP         KLIST
003600950104     C                   KFLD                    LBLAAP
003700941007     C                   KFLD                    LBLLPP
003800941007     C                   KFLD                    LBLNRP
003900941007     C                   KFLD                    LBLNSP
004000941007     C     KLBLN         KLIST
004100950104     C                   KFLD                    LBLAAN
004200941007     C                   KFLD                    LBLLPN
004300941007     C                   KFLD                    LBLNRN
004400941007     C                   KFLD                    LBLNSN
004500941007     C*---------------------------------------------------------------*
004600041117     c* I25tla= C solo chiusura files
004700041117    1c                   if        i25tla<>'C'
004800041117     c
004900041117     C     KLBL2         chain     FNLBL02L
005000041119    2C                   if        %found(fnlbl02l)  and
005100041119     C                             (lblft1=' ' or lblft2=' ')
005200041119     c* Se primo record sflaggato e secondo flaggato, riaggiorno anche il
005300041119     c* secondo
005400041119     c                   eval       wsavft1=lblft1
005500041119     c
005600941007     C***
005700041117     C* LEGGO TITAS CON BOLLA MAMMA se non l'ho già aggiornata
005800941007     C***
005900941128     C                   CLEAR                   WFCA
006000941126     C                   SETOFF                                       222324
006100050408     C     KLBLP         chain     TITAS30C                           32
006200941007    3C     *IN32         DOWEQ     *OFF
006300941007     C                   MOVEL     'S'           TASFA1
006400941128     C* SE LA MAMMA HA IL C/A LO MEMORIZZO ANCHE SULLA FIGLIA
006500941128     C     TASFCA        IFEQ      'S'
006600941128     C                   MOVEL     'S'           WFCA              1
006700941128     C                   ENDIF
006800941128     C*
006900941126     C   22              EXCEPT    AGTAS0
007000941126     C   23              EXCEPT    AGTAS1
007100941126     C   24              EXCEPT    AGTASP
007200041117     c                   eval      lblft1='S'
007300041119     c* solo la prima volta aggiorno la data
007400041119     c                   if        lbldt1=0
007500041117     c                   movel     *date         lbldt1
007600041119     c                   endif
007700941007     C*
007800941126     C                   SETOFF                                       222324
007900990809     C     KLBLP         READE     TITAS30C                               32
008000941007    3C                   ENDDO
008100941007     C**
008200041117     C* LEGGO TITAS CON BOLLA FIGLIA  solo se la bolla c'e' (ft2=*blanks)
008300041117     c*  altrimenti ft2 = N impostato da fnlg20r)
008400941122D    C**
008500041119    3c                   if        lblft2=' '   or wsavft1=' '
008600941126     C                   SETOFF                                       222324
008700050408     C     KLBLN         chain     TITAS30C                           32
008800041117    4C     *IN32         DOWEQ     *OFF
008900941007     C                   MOVEL     'S'           TASFA1
009000941128     C                   MOVEL     WFCA          TASFCA
009100941128     C   22              EXCEPT    AGTAS0
009200941128     C   23              EXCEPT    AGTAS1
009300941128     C   24              EXCEPT    AGTASP
009400041117     c                   eval      lblft2='S'
009500041117     c                   movel     *date         lbldt2
009600941007     C*
009700941126     C                   SETOFF                                       222324
009800990809     C     KLBLN         READE     TITAS30C                               32
009900041117    4C                   ENDDO
010000041117    3c                   endif
010100041117    c
010200041130     c                   update    fnlbl000
010300041119    2c                   endif
010400041117    1c                   endif
010500941123     C*
010600041117     c                   if        i25tla=' '
010700041117     C                   MOVEL     *on           *INRT
010800041117     c                   else
010900021008     C                   MOVEL     *ON           *INLR
011000041117     c                   endif
011100041117     C**************************************************************************
011200041117     C* PER FARLO FUNZIONARE SIA IN PROVA CHE IN BUONA
011300041117     C     *INZSR        begsr
011400041117     C* CERCO CONIL TIBS55 LA LIBRERIA DI GRUPPO DI SEDE
011500041117     C                   CLEAR                   DSBS50
011600041117     c                   movel     'L'           i50tla
011700041119     c                   Movel     046           I50apo
011800041117     C                   CALL      'TIBS55R'
011900041117     C                   PARM                    DSBS50
012000041117     C
012100041117     C* overdbf  TITAS30C
012200041122     c                   movea     o50alg        ovrtas(30)
012300941007     C                   SETOFF                                       01
012400041117     C                   CLEAR                   COMMAN           80
012500041117     C                   MOVEA     ovrtas        COMMAN
012600041122     c                   eval      comman=%trim(comman)+ctas
012700041117     C                   CALL      'QCMDEXC'
012800941007     C                   PARM                    COMMAN
012900041119     C                   PARM      50            LUNG             15 5
013000990809     C* 01 OFF - C'E' TITAS
013100041117     C                   OPEN      TITAS30C
013200950104     C*
013300911001     C                   ENDSR
013400950104     C**************************************************************************
013500990809     OTITAS000  E            AGTAS0
013600941122     O                       TASFA1
013700941129     O                       TASFCA
013800990809     OTITAS010  E            AGTAS1
013900941122     O                       TASFA1
014000941129     O                       TASFCA
014100990809     OTITASP00  E            AGTASP
014200941126     O                       TASFA1
014300941129     O                       TASFCA
014400950104     O**************************************************************************
014500041117**         OVRTAS
014600041122OVRDBF FILE(TITAS30C) TOFILE(XXXXXXXXXX
