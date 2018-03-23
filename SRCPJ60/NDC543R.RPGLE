000100020924     H*PARMS OPTION(*NOXREF *NODEBUGIO) DFTACTGRP(*NO)
000200020924     H*PARMS BNDDIR(PJXBND PJCBND)
000300020924     H*PARMS ACTGRP(QILE)
000400020924      *
000500020924    > *    Sottomette la generazione dei giroconti di c/c di pool
000600020924    > *    con la coda di stampa reperita dal parametro modulo
000700020924      *    della società di collegamento
000800020924      *           ------------------------------
000900020924      *
001000020924      *
001100020924      * DOCUMENTI MICROANALISI :
001200020924      *
001300020924      * NOTE TECNICHE :
001400020924      *
001500020924      *
001600020924      * PERSONALIZZAZIONI
001700020924      *
001800020924      *
001900020924      *
002000020924      *               MAPPA INDICATORI
002100020924      *
002200020924      *  21           GENERICO OPERAZIONI I/O
002300020924      *  22           GENERICO ERRORE OPERAZIONI I/O
002400020924      *  30           SFLDSP
002500020924      * N31           SFLCLR
002600020924      *  31           SFLDSPCTL
002700020924      *  32           SFLNXTCHG
002800020924      *  33           SFLEND
002900020924      *  39           OF PRTF
003000020924      *  40 <---> 49  DSPATR ERRORI SU SFL
003100020924      *  50 <---> 98  ERRORI SU VIDEO
003200020924      *  96           ERRORE SPECIALE : ERRORI IN ALTRE VIDEATE
003300020924      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
003400020924      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
003500020924      *  99           INDIC. GENERALE DI ERRORE
003600020924     F*----------------------------------------------------*
003700020924     F*-------------
003800020924     F* Parametri moduli   Soc/Parame
003900020924     FANPRM01L  IF   E           K DISK
004000020924     D*-------------
004100020924     D* Passaggio Parametri
004200020924     D KPJBA         E DS
004300020924     D*-------------
004400020924      * Dati di ambiente ottenuti da XSOC
004500020924     D SOC001        E DS                  EXTNAME(XSOC001DS)
004600020924      *-------------
004700020924     D* DS Interna per dati di output di XSOC
004800020924     D XSOCDS          DS          1000
004900020924     D*-------------
005000020924     D* DS Tabella parametri moduli
005100020924     D AngG14DS      E DS                  INZ
005200020924     D*-------------
005300020924     D* Richiamo a XATB
005400020924     D XATBDS        E DS
005500020924     D*----
005600020924      * parametro modulo 'CCPOOL'
005700020924     D ANP067DS      E DS                  INZ
005800020924     C*----------------------------------------------------*
005900020924     C*                MAIN LINE PROGRAM
006000020924     C*----------------------------------------------------*
006100020924     C*
006200020924     C*Inizializzazioni
006300020924     C                   EXSR      INZVAR
006400020924     C*
006500020924     C* sottometto in batch le elaborazioni
006600020924     C                   EXSR      EXELAN
006700020924     C*
006800020924     C* fine programma
006900020924     C                   EXSR      ENDPGM
007000020924     C*
007100020924      ************************************************************
007200020924      * Fine pgm
007300020924      ************************************************************
007400020924     C     ENDPGM        BEGSR
007500020924      *
007600020924     C                   SETON                                            LR
007700020924     C                   RETURN
007800020924      *
007900020924     C     XEndPgm       ENDSR
008000020924      ************************************************************
008100020924      * INIZIALIZZAZIONE VARIABILI
008200020924      ************************************************************
008300020924     C     INZVAR        BEGSR
008400020924      *
008500020924      * Reperimento dati società
008600020924     C                   MOVEL     'SOC001'      TIPXSC
008700020924     C                   MOVEL     *BLANK        SOCXSC
008800020924     C                   EXSR      REPSOC
008900020924     C     RTNXSC        IFNE      '1'
009000020924     C                   MOVEL     XSOCDS        SOC001
009100020924     C                   ELSE
009200020924     C                   RETURN
009300020924     C                   ENDIF
009400020924      *
009500020924      * Reperimento parametro modulo ccpool
009600020924     C                   EXSR      RepPar
009700020924      *
009800020924      *
009900020924     C     XINZVAR       ENDSR
010000020924      ************************************************************
010100020924      * REPERIMENTO DATI SOCIETA'
010200020924      ************************************************************
010300020924     C     REPSOC        BEGSR
010400020924      *
010500020924     C                   CALLB     'XSOC'
010600020924     C                   PARM                    TIPXSC            6
010700020924     C                   PARM                    SOCXSC            3
010800020924     C                   PARM                    CDSXSC            9 0
010900020924     C                   PARM                    MODXSC            3
011000020924     C                   PARM      *BLANKS       RTNXSC            1
011100020924     C                   PARM                    XSOCDS
011200020924     C                   PARM                    KPJBA
011300020924      *
011400020924     C     XRepSoc       ENDSR
011500020924      ************************************************************
011600020924      * Rerimento parametro modulo
011700020924      ************************************************************
011800020924     C     RepPar        BEGSR
011900020924     C*
012000020924     C* verifico se in tab. G14 esiste il parametro modulo dello
012100020924     C* dei conti correnti di pool
012200020924     C*
012300020924     C                   Clear                   XATBDS
012400020924     C                   MOVE      '1'           XTBRIC
012500020924     C                   MOVE      XScSoc        XTBAZI
012600020924     C                   MOVE      *ZERO         XTBKEY
012700020924     C                   MOVE      'G14'         XTBCOD
012800020924     C                   MOVE      'CCPOOL  '    XTBKEY
012900020924     C                   CALLB     'XATB'
013000020924     C                   PARM                    XATBDS
013100020924     C*
013200020924     C                   if        XTBERR      = '0'
013300020924     C                   MoveL     XTBUNI        AngG14DS
013400020924     C                   EXSR      RepANPRM
013500020924     C                   endif
013600020924     C*
013700020924     C                   ENDSR
013800020924     C/EJECT
013900020924     C************************************************************
014000020924     C* Verifico su in gestione parametri moduli esiste CCPOOL
014100020924     C************************************************************
014200020924     C     RepANPRM      BEGSR
014300020924     C*
014400020924     C                   Clear                   ANP067DS
014500020924     C*
014600020924     C                   MOVEL(P)  'CCPOOL  '    PRMParame
014700020924     C* se modulo di gruppo, lo memorizzo con la capofila anagrafica
014800020924     C                   if        GruG14     <> *On
014900020924     C                   MOVE      XScSoc        PRMSocieta
015000020924     C                   else
015100020924     C                   MOVE      XScCpa        PRMSocieta
015200020924     C                   endif
015300020924     C*
015400020924     C     K02PRM01      CHAIN     ANPRM01L                           21
015500020924     C                   if        *In21       = *Off
015600020924     C                   MOVEL     PRMDati       ANP067DS
015700020924     C                   endif
015800020924     C*
015900020924     C                   ENDSR
016000020924     C/EJECT
016100020924      ************************************************************
016200020924      * Lancio
016300020924      ************************************************************
016400020924     C     ExeLan        BEGSR
016500020924      *
016600020924     C                   movel     KQOfr067      KQOfr
016700020924     C*
016800020924     C                   move      *off          $DEBUG            1
016900020924     C*
017000020924     C                   IF        $DEBUG     =*ON
017100020924     C                   call      'NDC542R'                            21
017200020924     C                   parm                    Kpjba
017300020924      *
017400020924     C                   else
017500020924     C                   move      'C542'        KCoAz
017600041018>>>>>C                   CLEAR                   KBuff
017700020924     C                   call      'BCH10'
017800020924     C                   parm                    Kpjba
017900020924     C                   endif
018000020924      *
018100020924     C     XExeLanBatch  ENDSR
018200020924     C************************************************************
018300020924     C* OPERAZIONI INIZIALI
018400020924     C************************************************************
018500020924     C     *INZSR        BEGSR
018600020924     C*
018700020924     C     *ENTRY        PLIST
018800020924     C                   PARM                    KPJBA
018900020924     C*
019000020924     C                   ENDSR
019100020924     C************************************************************
019200020924     C* DEFINIZIONE KLIST
019300020924     C************************************************************
019400020924     C     DefKlist      BEGSR
019500020924     C*
019600020924     C*
019700020924     C* ANPRM01L
019800020924     C     K02PRM01      KLIST
019900020924     C                   KFLD                    PRMSocieta
020000020924     C                   KFLD                    PRMParame
020100020924     C*
020200020924     C                   ENDSR
