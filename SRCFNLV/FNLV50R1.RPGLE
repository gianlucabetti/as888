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
001700170714     D* DS PER FNLV55R - DS per avere term. arrivo
001800170714     D FNLV55ds      E DS
001900170424
002000941027     D* DS PER TRUL06R - CARICAMENTO £X
002100941027     D DSUL06        E DS                  EXTNAME(TRUL06DS)
002200941027     D  LIN                    1     90  0
002300941027     D                                     DIM(30)
002400170424     d
002500170424     d* DS per caricamento terminal di arrivo
002600170424     D trul09ds      E DS
002700170424     d  TFA                   19    108  0 Dim(30)
002800170424
002900941027     D KPJBA         E DS
003000170714     d DataSYS         s               d   inz(*sys)
003100170424      // ? PROTOTIPI ?
003200170424      /copy gaitrasrc/srcprotopr,tibs34r
003300921021     C*---------------------------------------------------------------*
003400921021     C     *ENTRY        PLIST
003500170424     C                   PARM                    FNLV50ds
003600000000     C*---------------------------------------------------------------*
003700941027     C* TIPO LANCIO, D50TLA "C" -           CHIUSO CON LR
003800941027     C* TIPO LANCIO, D50TLA "L" - ELABORO E CHIUDO CON LR
003900941027     C* TIPO LANCIO, D50TLA " " - ELABORO E CHIUDO IN RETRN
004000941027    1C     D50TLA        IFNE      'C'
004100941027     C                   CLEAR                   D50ERR
004200941027     C                   CLEAR                   D50MSG
004300170424     C                   CLEAR                   D50val_P
004400170424     C                   CLEAR                   D50val_A
004500941027     C                   SETOFF                                       90
004600941027     C*
004700941027     C* OPERAZIONI INIZIALI
004800941027    2C     WFLG1         IFEQ      ' '
004900941027     C                   EXSR      INIZIO
005000941027     C                   MOVEL     '1'           WFLG1             1
005100941027    2C                   ENDIF
005200941027     C*
005300170424     C* Filiale utente
005400170424     C                   Z-ADD     dutPOU        WFGS              3 0
005500941027     C*
005600170424     C* NON DEVE ESSERE GESTITA DA ALTRE FILIALI
005700171016    2C***                IF        %LOOKUP(d50fgs:l6s)>0
005800171016     C***                MOVEL     '2'           D50ERR
005900171016     C***                MOVEL     MSG(2)        D50MSG
006000171016     C***                SETON                                        90
006100170424
006200171016   x2C***                Else
006300941027     C*
006400170424     c* controlli in PARTENZA
006500170424    3C                   IF        D50tcn<>'A'
006600170424     C                   EXSR      CTRPar
006700170424    3c                   endif
006800170424     c
006900170424    3C                   IF        D50tcn= 'A'  or d50tcn='E'
007000170424     C                   EXSR      CTRArr
007100170424    3c                   endif
007200170424     c
007300170424     c* Imposto l'errore se non c'è già ed entrambi i falg non "S"
007400170424    3c                   if        d50val_p<>'S' and d50val_a<>'S'
007500170424     C                   MOVEL     '1'           D50ERR
007600170424     C                   MOVEL     MSG(1)        D50MSG
007700170424    3C                   ENDIF
007800170424     c
007900171016    2C****               ENDIF
008000170424    1C                   ENDIF
008100941025     C*
008200941025     C* CHIUSURA PGM
008300941027    1C     D50TLA        IFEQ      ' '
008400941025     C                   RETURN
008500941025     C                   ELSE
008600941025     C                   SETON                                        LR
008700941025    1C                   ENDIF
008800941024     C*
008900941027     C* OPERAZIONI CHE ESEGUO AD APERTURA PGM ------------------------*
009000941027     C     INIZIO        BEGSR
009100170424     c* dati utente
009200170424     c                   in(E)     *dtaara
009300170424    1c                   If        %error  or RSUT = *blanks
009400170424     c                   Clear                   Tibs34ds
009500170424     c                   Call      'TIBS34R'
009600170424     c                   Parm                    Tibs34ds
009700170424     c                   In        *dtaara
009800170424    1c                   EndIf
009900170424     c
010000941027     C***
010100941027     C* CARICO TUTTE LE  FILIALI GESTITE DA ALTRE (DA £6)
010200941027     C***
010300941027     C                   CLEAR                   DSUL06
010400941027     C                   MOVE      '£6'          D06COD
010500020509     c                   eval      d06esc = 'G'
010600020509     c                   movel     simfel        d06key
010700941027     C                   MOVEL     DSUL06        KPJBU
010800941027     C                   CALL      'TRUL06R'
010900941027     C                   PARM                    KPJBA
011000941027     C                   MOVEL     KPJBU         DSUL06
011100941027     C                   MOVEA     LIN           L6S
011200941027     C***
011300941027     C* CARICO TABELLA FILIALI GESTITE £1
011400941027     C***
011500941027     C                   CLEAR                   DSUL06
011600941027     C                   MOVE      '£1'          D06COD
011700941027     C                   MOVEL     SIMFEL        D06KEY
011800941027     C                   MOVEL     'L'           D06TLA
011900941027     C                   MOVEL     DSUL06        KPJBU
012000941027     C*
012100941027     C                   CALL      'TRUL06R'
012200941027     C                   PARM                    KPJBA
012300941027     C                   MOVEL     KPJBU         DSUL06
012400941027     C                   MOVEA     LIN           L1
012500170424     C
012600941027     C                   ENDSR
012700170424     c*---------------------------------------------------------------
012800170424     C     CTRPAR        BEGSR
012900170424     C* un secondo livello non più fare cambio filiale gestione
013000170424     c*  in partenza
013100170424    1C     WFGS          IFNE      SIMFEL
013200170424     C     D50FGS        ANDNE     WFGS
013300170424     C     D50FGS        OREQ      0
013400170424     C                   eval      d50val_p='N'
013500170424    1C                   ENDIF
013600170424
013700170424     C* SE SONO SIMFEL NON PUO' ESSERE FIL GESTITA DA ALTRE
013800170424    1C     wfgs          ifeq      SIMFEL
013900170424     C     D50FGS        andne     SIMFEL
014000170424     C*
014100170424     C* DEVE APPARTENERE ALLA L1
014200170424     C     D50FGS        LOOKUP    L1                                     30
014300170424    2C     *IN30         IFEQ      *OFF
014400170424     C                   eval      d50val_p='N'
014500170424    2C                   ENDIF
014600170424     C*
014700170424    1C                   ENDIF
014800170424     c
014900170424     c* Se il flag non è a "N" allora imposto --> "S"
015000170424     c                   if        d50val_p=' '
015100170424     C                   eval      d50val_p='S'
015200170424     C                   ENDIF
015300170424
015400170424     c                   ENDSR
015500170424     c*---------------------------------------------------------------
015600170424     C     CTRARR        BEGSR
015700170424     c*
015800170424     c* Carico i terminal di arrivo della filiale utente
015900170424     c                   Clear                   Trul09ds
016000170424     c                   Eval      I09Mod = 'TFA'
016100170424     c                   Eval      I09Tfa = dutpou
016200170424     c                   Eval      I09Tla = 'L'
016300170424     c                   Call      'TRUL09R'
016400170424     c                   Parm                    Trul09ds
016500170428     c
016600170428     c* se non è terminal di nessuno, se stesso sempre abilitato
016700170428     c                   if        tfa(1)=000
016800170428     c                   if        dutpou=simfel
016900170428     c                   movea     L1            tfa
017000170428     c                   else
017100170428     c                   movel     dutpou        tfa(1)
017200170428     c                   endif
017300170428     c                   endif
017400170424
017500170424     c* la filiale scelta deve essere tra questi
017600170714    1c                   if        %lookup(d50fgs:tfa)=0
017700170714     C                   eval      d50val_a='N'
017800170714     c*
017900170714     c* Se si tratta di un secondo liv in partenza e  term di arrivo
018000170714     c*  devo abilitare lo stesso il suo TFP perchè non esiste profilo utente
018100170714     c*  per l'arrivo
018200170714    2c                   if        d50fgs<>simfel
018300170714     c                   clear                   fnlv55ds
018400170714     c                   eval      d55tla='L'
018500170714     c                   eval      d55tpt='A'
018600170714     c                   eval      d55lin=d50fgs
018700170714     c                   eval      d55drf=%dec(datasys)
018800170714     c                   call      'FNLV55R'
018900170714     c                   parm                    fnlv55ds
019000170714    3c                   if        d55tfa=d50fgs
019100170714    4c                   if        %lookup(d50fgs:l1)>0
019200170714     C                   eval      d50val_a=' '
019300170714    4c                   endif
019400170714    3c                   endif
019500170714    2c                   endif
019600170714    1c                   endif
019700170424
019800170424     c* Se il flag non è a "N" allora imposto --> "S"
019900170424     c                   if        d50val_a=' '
020000170424     C                   eval      d50val_a='S'
020100170424     C                   ENDIF
020200170424     c
020300170424     c                   ENDSR
020400941027**
020500941027Filiale in Gestione errata
020600170424Filiale non operativa : cambio non effettuabile
