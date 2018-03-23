000100921021     H DECEDIT('0,') DATEDIT(*DMY.)
000200941027     H* FNLV50R  *---------------------------------------------------*
000300941027     H*     CONTROLLO FILIALE GESTIONE                               *
000400921021     H*--------------------------------------------------------------*
000500170424     D MSG             S             75    DIM(2) CTDATA PERRCD(1)
000600941027     D*
000700941027     D L1              S              3  0 DIM(30)
000800941027     D L6S             S              3  0 DIM(30)
000900940218      *
001000170424     d Tibs34ds      e ds                  inz
001100170424     d §azute        e ds                  Extname(Azute00f)
001200170424     d                                     dtaara
001300170424     d §Datiute      e ds                  EXTname(ddatiute)
001400170424     d                                     dtaara
001500941027     D* DS PER FNLV50R - DS DEI CAMPI PER CONTROLLO FILIALE GESTIONE
001600170424     D FNLV50ds      E DS
001700170424
001800941027     D* DS PER TRUL06R - CARICAMENTO £X
001900941027     D DSUL06        E DS                  EXTNAME(TRUL06DS)
002000941027     D  LIN                    1     90  0
002100941027     D                                     DIM(30)
002200170424     d
002300170424     d* DS per caricamento terminal di arrivo
002400170424     D trul09ds      E DS
002500170424     d  TFA                   19    108  0 Dim(30)
002600170424
002700941027     D KPJBA         E DS
002800170424      // ? PROTOTIPI ?
002900170424      /copy gaitrasrc/srcprotopr,tibs34r
003000921021     C*---------------------------------------------------------------*
003100921021     C     *ENTRY        PLIST
003200170424     C                   PARM                    FNLV50ds
003300000000     C*---------------------------------------------------------------*
003400941027     C* TIPO LANCIO, D50TLA "C" -           CHIUSO CON LR
003500941027     C* TIPO LANCIO, D50TLA "L" - ELABORO E CHIUDO CON LR
003600941027     C* TIPO LANCIO, D50TLA " " - ELABORO E CHIUDO IN RETRN
003700941027    1C     D50TLA        IFNE      'C'
003800941027     C                   CLEAR                   D50ERR
003900941027     C                   CLEAR                   D50MSG
004000170424     C                   CLEAR                   D50val_P
004100170424     C                   CLEAR                   D50val_A
004200941027     C                   SETOFF                                       90
004300941027     C*
004400941027     C* OPERAZIONI INIZIALI
004500941027    2C     WFLG1         IFEQ      ' '
004600941027     C                   EXSR      INIZIO
004700941027     C                   MOVEL     '1'           WFLG1             1
004800941027    2C                   ENDIF
004900941027     C*
005000170424     C* Filiale utente
005100170424     C                   Z-ADD     dutPOU        WFGS              3 0
005200941027     C*
005300170424     C* NON DEVE ESSERE GESTITA DA ALTRE FILIALI
005400170424    2C                   IF        %LOOKUP(d50fgs:l6s)>0
005500170424     C                   MOVEL     '2'           D50ERR
005600170424     C                   MOVEL     MSG(2)        D50MSG
005700170424     C                   SETON                                        90
005800170424
005900170424   x2C                   Else
006000941027     C*
006100170424     c* controlli in PARTENZA
006200170424    3C                   IF        D50tcn<>'A'
006300170424     C                   EXSR      CTRPar
006400170424    3c                   endif
006500170424     c
006600170424    3C                   IF        D50tcn= 'A'  or d50tcn='E'
006700170424     C                   EXSR      CTRArr
006800170424    3c                   endif
006900170424     c
007000170424     c* Imposto l'errore se non c'è già ed entrambi i falg non "S"
007100170424    3c                   if        d50val_p<>'S' and d50val_a<>'S'
007200170424     C                   MOVEL     '1'           D50ERR
007300170424     C                   MOVEL     MSG(1)        D50MSG
007400170424    3C                   ENDIF
007500170424     c
007600170424    2C                   ENDIF
007700170424    1C                   ENDIF
007800941025     C*
007900941025     C* CHIUSURA PGM
008000941027    1C     D50TLA        IFEQ      ' '
008100941025     C                   RETURN
008200941025     C                   ELSE
008300941025     C                   SETON                                        LR
008400941025    1C                   ENDIF
008500941024     C*
008600941027     C* OPERAZIONI CHE ESEGUO AD APERTURA PGM ------------------------*
008700941027     C     INIZIO        BEGSR
008800170424     c* dati utente
008900170424     c                   in(E)     *dtaara
009000170424    1c                   If        %error  or RSUT = *blanks
009100170424     c                   Clear                   Tibs34ds
009200170424     c                   Call      'TIBS34R'
009300170424     c                   Parm                    Tibs34ds
009400170424     c                   In        *dtaara
009500170424    1c                   EndIf
009600170424     c
009700941027     C***
009800941027     C* CARICO TUTTE LE  FILIALI GESTITE DA ALTRE (DA £6)
009900941027     C***
010000941027     C                   CLEAR                   DSUL06
010100941027     C                   MOVE      '£6'          D06COD
010200020509     c                   eval      d06esc = 'G'
010300020509     c                   movel     simfel        d06key
010400941027     C                   MOVEL     DSUL06        KPJBU
010500941027     C                   CALL      'TRUL06R'
010600941027     C                   PARM                    KPJBA
010700941027     C                   MOVEL     KPJBU         DSUL06
010800941027     C                   MOVEA     LIN           L6S
010900941027     C***
011000941027     C* CARICO TABELLA FILIALI GESTITE £1
011100941027     C***
011200941027     C                   CLEAR                   DSUL06
011300941027     C                   MOVE      '£1'          D06COD
011400941027     C                   MOVEL     SIMFEL        D06KEY
011500941027     C                   MOVEL     'L'           D06TLA
011600941027     C                   MOVEL     DSUL06        KPJBU
011700941027     C*
011800941027     C                   CALL      'TRUL06R'
011900941027     C                   PARM                    KPJBA
012000941027     C                   MOVEL     KPJBU         DSUL06
012100941027     C                   MOVEA     LIN           L1
012200170424     C
012300941027     C                   ENDSR
012400170424     c*---------------------------------------------------------------
012500170424     C     CTRPAR        BEGSR
012600170424     C* un secondo livello non più fare cambio filiale gestione
012700170424     c*  in partenza
012800170424    1C     WFGS          IFNE      SIMFEL
012900170424     C     D50FGS        ANDNE     WFGS
013000170424     C     D50FGS        OREQ      0
013100170424     C                   eval      d50val_p='N'
013200170424    1C                   ENDIF
013300170424
013400170424     C* SE SONO SIMFEL NON PUO' ESSERE FIL GESTITA DA ALTRE
013500170424    1C     wfgs          ifeq      SIMFEL
013600170424     C     D50FGS        andne     SIMFEL
013700170424     C*
013800170424     C* DEVE APPARTENERE ALLA L1
013900170424     C     D50FGS        LOOKUP    L1                                     30
014000170424    2C     *IN30         IFEQ      *OFF
014100170424     C                   eval      d50val_p='N'
014200170424    2C                   ENDIF
014300170424     C*
014400170424    1C                   ENDIF
014500170424     c
014600170424     c* Se il flag non è a "N" allora imposto --> "S"
014700170424     c                   if        d50val_p=' '
014800170424     C                   eval      d50val_p='S'
014900170424     C                   ENDIF
015000170424
015100170424     c                   ENDSR
015200170424     c*---------------------------------------------------------------
015300170424     C     CTRARR        BEGSR
015400170424     c*
015500170424     c* Carico i terminal di arrivo della filiale utente
015600170424     c                   Clear                   Trul09ds
015700170424     c                   Eval      I09Mod = 'TFA'
015800170424     c                   Eval      I09Tfa = dutpou
015900170424     c                   Eval      I09Tla = 'L'
016000170424     c                   Call      'TRUL09R'
016100170424     c                   Parm                    Trul09ds
016200170428     c
016300170428     c* se non è terminal di nessuno, se stesso sempre abilitato
016400170428     c                   if        tfa(1)=000
016500170428     c                   if        dutpou=simfel
016600170428     c                   movea     L1            tfa
016700170428     c                   else
016800170428     c                   movel     dutpou        tfa(1)
016900170428     c                   endif
017000170428     c                   endif
017100170424
017200170424     c* la filiale scelta deve essere tra questi
017300170424     c                   if        %lookup(d50fgs:tfa)=0
017400170424     C                   eval      d50val_a='N'
017500170424     c                   endif
017600170424
017700170424     c* Se il flag non è a "N" allora imposto --> "S"
017800170424     c                   if        d50val_a=' '
017900170424     C                   eval      d50val_a='S'
018000170424     C                   ENDIF
018100170424     c
018200170424     c                   ENDSR
018300941027**
018400941027Filiale in Gestione errata
018500170424Filiale non operativa : cambio non effettuabile
