000100980924     H*PARMS DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND) ACTGRP(QILE)
000200160301     H*PARMS COMMIT(*NONE) DATFMT(*ISO) CLOSQLCSR(*ENDACTGRP)
000300980924     H*PARMS OPTION(*NOXREF)
000400140715     H*PARMS DYNUSRPRF(*OWNER) RDB(*NONE)
000500941130     H DECEDIT('0,') DATEDIT(*DMY.)
000600941130      *
000700150915      *            REPERIMENTO DATI DA NDMOV00F
000800981109      *            MEDIANTE SQL DINAMICO
000900941130      *
001000941130     D*----------------------------------------------------*
001100981109     D* Stringa sql
001200150915     D  SqlMov         S           1000
001300941130     D*-------------
001400941130     D* Passaggio Parametri
001500941130     D KPJBA         E DS
001600941130     D*-------------
001700941130     D* Parametri in ricezione
001800150915     D SDGSD26DS     E DS
001900020123     D*-------------
002000160317R291 X***  NDMOV00F      E DS
002001160317R291 D NDMOV00F      E DS                  occurs(1000)
002002160317  "  D NDMOVDS       E DS                  Extname(Ndmov00f) prefix(X:1)
002003160317  "  D*Indice DsMultipla
002004160317  "  D $Ind            S              4  0
002005160317  "  D*
002006160317R291 D $FineMov        S              1
002007160317R291 D $OkMov          S              1
002100980924     D*-------------
002200981109     D* campi di appoggio
002300150915     D DTRegDAamg      S             10A
002400150915     D DTRegAamg       S             10A
002500150915     D DTDocDAamg      S             10A
002600150915     D DTDocAamg       S             10A
002700941130     C*----------------------------------------------------*
002800941130     C*                MAIN LINE PROGRAM
002900941130     C*----------------------------------------------------*
003000941130     C*
003100941130     C* inizializzazione variabili
003200941130     C                   EXSR      INZVAR
003300941130     C*
003400941130     C* lettura cursore
003500941130     C                   EXSR      LETCUR
003600941130     C*
003700941130     C* fine programma
003800941130     C                   EXSR      ENDPGM
003900941130     C*
004000941130     C/EJECT
004100941130     C************************************************************
004200941130     C* FINE PROGRAMMA
004300941130     C************************************************************
004400941130     C     ENDPGM        BEGSR
004500941130     C*
004600941130     C* passaggio dati al pgm chiamante
004700150916     C                   MOVEL     SDGSD26DS     KPJBU
004800941130     C*
004900160317     C     ENDD26        IFEQ      *OFF
005000941130     C                   SETON                                            RT
005100941130     C                   ELSE
005200941130     C                   EXSR      CLOCUR
005300941130     C                   SETON                                            LR
005400941130     C                   ENDIF
005500941130     C*
005600941130     C                   RETURN
005700941130     C*
005800941130     C                   ENDSR
005900941130     C/EJECT
006000941130     C************************************************************
006100150915     C* SQL
006200941130     C************************************************************
006300941130     C     ORD01         BEGSR
006400140715     C*
006500140715     C* Preparazione stringa sql per lettura
006600150915     C                   Clear                   SqlMov
006601160318R2192X***                Eval      SqlMOV =
006602160318R2192X***                           'SELECT * FROM NDMOV08L '  +
006603160318R2192C                   if        TPDTRIFD26 = 'R'
006801160316  "  C                   Eval      SqlMOV = 'SELECT * FROM NDMOV05L'
006802160316  "  C                   Else
006803160316  "  C                   Eval      SqlMOV = 'SELECT * FROM NDMOV08L'
006804160316  "  C                   Endif
006805160316  "  C                   Eval      SqlMOV      = %trimr(SqlMOV) +
006806160318R2192C                             ' '                          +
006900160316     C                              'Left Join Anrco01l On '   +
007000150916     C                              'MovSocieta = Rcosocieta ' +
007100150916     C                              'and MovKcc = RcoKcc '     +
007200150916     C                              'and MovKsc = RcoKsc '     +
007300150915     C                             ' WHERE MOVSOCIETA=' + '''' +
007400150915     C                             SocietaD26 + ''''
007500150915     C*
007600160318R2192X***                Eval      SqlMOV      = %trimr(SqlMOV)+
007700160318R2192X***                          ' AND MOVNrRigaM  =  1 '
007800150915     C*
007900150915     C                   Eval      SqlMOV      = %trimr(SqlMOV)+
008000150915     C                             ' AND MOVCtb      =' + '''' +
008100150916     C                             CTBD26 + ''''
008200150915     C*
008300150915     C                   Eval      SqlMOV      = %trimr(SqlMOV)+
008400150915     C                             ' AND MOVSNatura  =' + '''F'''
008500150924     C* Bolle doganali
008600150924     C                   If        SiBollD26 = *on
008700150916     C                   Eval      SqlMOV      = %trimr(SqlMOV)+
008800150916     C                             ' AND RCOAlleIva  =' + '''02'''
008900150929     C                   Else
009000150929     C                   Eval      SqlMOV      = %trimr(SqlMOV)+
009100150929     C                             ' AND RCOAlleIva  <>' + '''02'''
009200150929     C                   EndIf
009300150916     C*
009400160125     C                   if        TPDTRIFD26 = 'R'
009500150915     C                   Eval      SqlMOV      = %trimr(SqlMOV) +
009600150916     C                             ' AND (MOVDtReg'
009700150915     C                   Eval      SqlMOV      = %trimr(SqlMOV) +
009800150916     C                             ' BETWEEN ''' + DTRegDAamg +
009900150916     C                             ''' AND ''' + DTRegAamg + ''''
010000150915     C                             + ' ) '
010100160125     C*
010200160125     C                   else
010300150916     C                   Eval      SqlMOV      = %trimr(SqlMOV) +
010400150916     C                             ' AND (MOVDtDoc'
010500150916     C                   Eval      SqlMOV      = %trimr(SqlMOV) +
010600150916     C                             ' BETWEEN ''' + DTDocDAamg +
010700150916     C                             ''' AND ''' + DTDocAamg + ''''
010800150916     C                             + ' ) '
010900160125     C                   endif
011000150915     C*
011100150916     C                   if        KccD26     <> *Blank
011200150915     C                   Eval      SqlMOV      = %trimr(SqlMOV)+
011300150915     C                             ' AND MOVKcc  =' + '''' +
011400150916     C                             KccD26 + ''''
011500150915     C                   endif
011600150915     C*
011700150916     C                   if        KscD26     <> *Blank
011800150915     C                   Eval      SqlMOV      = %trimr(SqlMOV)+
011900150915     C                             ' AND MOVKsc =' + '''' +
012000150916     C                             KscD26 + ''''
012100150915     C                   endif
012200150915     C*
012300160316     C                   if        TPDTRIFD26 = 'R'
012400150915     C                   Eval      SqlMOV      = %trimr(SqlMOV)+
012401160318R2192X***                            ' ORDER BY MOVKCC, '+
012402160318R2192C                               ' ORDER BY MOVSOCIETA,' +
012403160318R2192C                               ' MOVCTB,  MOVKCC, '+
012404160318R2192C                               ' MOVKSC, MOVDTREG '
012600160318R2192X***                            ' MOVKSC, MOVDTREG,' +
012700160318R2192X***                            ' MOVSYS, MOVNRASREG'
012800160303     C                   else
012900160303     C                   Eval      SqlMOV      = %trimr(SqlMOV)+
012901160318R2192X****                           ' ORDER BY MOVKCC, '+
012902160318R2192C                               ' ORDER BY MOVSOCIETA,' +
013000160318R2192C                               ' MOVCTB,  MOVKCC, '+
013100160316     C                               ' MOVKSC, MOVDTDOC,' +
013101160317     C                               ' MOVNRDOC'
013200160318R2192X**                             ' MOVSYS, MOVNRASREG'
013300160303     C                   endif
013400150915     C*
013500150915     C* For Fetch Only                                                          *
013600150915     C                   Eval      SqlMOV      = %trimr(SqlMOV)+
013700150915     C                               ' FOR FETCH ONLY'
013800981109     C*
013900981109     C/EXEC SQL
014000150916     C+     PREPARE S1 FROM :SQLMOV
014100981109     C/END-EXEC
014200981109     C*
014300981109     C/EXEC SQL
014400981109     C+     DECLARE A1 CURSOR FOR S1
014500981109     C/END-EXEC
014600981109     C*
014700981109     C/EXEC SQL
014800981109     C+     OPEN A1
014900981109     C/END-EXEC
015000981109     C*
015100941130     C                   ENDSR
015200941130     C/EJECT
015300941130     C************************************************************
015400941130     C* APERTURA CURSORE
015500941130     C************************************************************
015600941130     C     OPNCUR        BEGSR
015700941130     C*
015800941229     C                   EXSR      CLOCUR
015900941130     C*
016000941130     C/EXEC SQL
016100941130     C+     OPEN A1
016200941130     C/END-EXEC
016300941130     C*
016400941201     C                   MOVE      *ON           $OPEN
016500941201     C*
016600941130     C                   ENDSR
016700941130     C/EJECT
016800941130     C************************************************************
016900941130     C* LETTURA CURSORE
017000941130     C************************************************************
017100941130     C     LETCUR        BEGSR
017200160318R2192X***
017300160317  "  X***/EXEC SQL
017400160317  "  X***     FETCH A1 INTO :NDMOV00F
017500160318R2192X***/END-EXEC
017600941130     C*
017601160318R2192C                   Exsr      ExeSqlMoV
017602160318R2192C*
017700160318R2192X***                move      *on           OkSqlCod          1
017800160318R2192X***                exsr      ImpSqlCod
017900160318     C*
018000160318R2192X***  OKSqlCod      IfNe      *On
018001160318R2192C     $FineMov      Ifeq      *On
018100150916     C                   MOVE      *ON           ENDD26
018200941130     C                   ELSE
018300150916     C                   MOVE      *OFF          ENDD26
018400941130     C                   END
018500980925     C*
018600941130     C                   ENDSR
018700941130     C/EJECT
018800941130     C************************************************************
018900941130     C* CHIUSURA CURSORE
019000941130     C************************************************************
019100941130     C     CLOCUR        BEGSR
019200941130     C*
019300941130     C/EXEC SQL
019400941130     C+     CLOSE A1
019500941130     C/END-EXEC
019600941130     C*
019700941201     C                   MOVE      *OFF          $OPEN
019800941201     C*
019900941130     C                   ENDSR
020000941130     C/EJECT
020002160318R2192C************************************************************
020003160317  "  C* Eseguo lettura dei movimenti contabili
020004160317  "  C************************************************************
020005160318R2192C     ExeSqlMOV     BEGSR
020006160317     C*
020010160317     C                   Move      *OFF          $OkMov
020013160317     C*
020014160317     C                   DoW       $OKMOV   = *Off  and $FineMov = *off
020015160317     C*
020016160317     C* lettura file
020017160317     C                   EXSR      FetchMOV
020018160317     C*
020020160317     C                   move      *on           OkSqlCod          1
020021160317     C                   Exsr      ImpSqlCod
020022160317     C*
020023160317     C                   Select
020024160317     C*-----------------------
020025160317     C* OK
020026160317     C                   When      OkSqlCod   = *on
020028160317     C*
020029160317     C     $Ind          OCCUR     NDMOV00F
020030160317     C*
020031160317     C                   EXSR      ElabMOV
020032160317     C*
020033160317     C*-----------------------
020034160317     C* fine lettura
020035160317     C                   When      OkSqlCod    = *off
020036160317     C*
020037160317     C                   Move      *On           $FineMov
020038160317     C                   Move      *Off          $OkMov
020040160317     C*
020046160317     C                   endsl
020047160317     C*
020048160317     C                   endDo
020051160317     C*
020052160317     C                   ENDSR
020053160317     C/EJECT
020054160318R2192C******************************************************
020055160317  "  C* Eseguo lettura
020056160317  "  C******************************************************
020057160318R2192C     FetchMOV      BEGSR
020058160317     C*
020059160317     C     $Ind          Ifeq      SqlEr3
020060160317     C/EXEC SQL
020061160317     C+     FETCH A1 FOR 1000 ROWS INTO :NDMOV00F
020062160317     C/END-EXEC
020063160317     C                   Z-ADD     1             $Ind
020064160317     C                   else
020065160317     C                   ADD       1             $Ind
020066160317     C                   endif
020067160317     C*
020068160318R2192C                   ENDSR
020069160317  "  C/EJECT
020070160317  "  C********************************************************************
020071160317  "  C* Elaboro i movimenti contabili
020072160317  "  C********************************************************************
020073160318R2192C     ElabMOV       BEGSR
020074160317     C*
020078160317     C                   Eval      $OkMOV      = *OFF
020080160317     C*
020082160317     C                   if        MovNrRigam  = 1
020083160317     C                   Eval      NdMovDs = Ndmov00f
020084160317     C                   Eval      $OkMOV      = *On
020085160317     C                   endif
020093160317     C*
020094160318R2192C                   ENDSR
020095160318R2192C/EJECT
020100941130     C************************************************************
020200941130     C* OPERAZIONI INIZIALI
020300941130     C************************************************************
020400941130     C     *INZSR        BEGSR
020500941130     C*
020600941130     C* Reperimento parametri
020700941130     C*
020800941130     C     *ENTRY        PLIST
020900150916     C                   PARM                    SDGSD26DS
021000160317     C                   PARM                    NDMOVds
021100941130     C*
021200941130     C                   ENDSR
021300941130     C/EJECT
021400941130     C************************************************************
021500941130     C* INIZIALIZZAZIONE VARIABILI
021600941130     C************************************************************
021700941130     C     INZVAR        BEGSR
021800941130     C*
021900941201     C* Se è la prima chiamata eseguo l'apertura del cursore SQL
022000941130     C*
022100150915     C     RICD26        IFEQ      '0'
022200941130     C*
022201160317     C                   Move      *Off          $FineMov
022202160318R2192C                   Z-Add     0             $Ind
022203160318R2192C                   Z-Add     0             SqlEr3
022300941201     C* Chiudo il cursore se era già aperto
022400941130     C*
022500941201     C     $OPEN         IFEQ      *ON
022600941201     C                   EXSR      CLOCUR
022700941201     C                   ENDIF
022800941201     C*
022900941201     C* Variabili gestione pgm
023000941201     C*
023100150915     C* ho un unico ordinamento (per ksc e registrazione)
023200941130     C                   SELECT
023300150915     C     ORDD26        WHENEQ    1
023400941130     C                   MOVE      '01'          $GEST
023500150915     C     ORDD26        WHENEQ    99
023600950321     C                   MOVE      '01'          $GEST
023700150915     C                   Z-ADD     1             ORDD26
023800941130     C                   ENDSL
023900941205     C*
024000150915     C* Date
024100150915     C                   Movel     DtRegDaD26    DTRegDAamg
024200150915     C                   Movel     DtRegAD26     DTRegAamg
024300150915     C                   Movel     DtDocDAD26    DTDocDAamg
024400150915     C                   Movel     DtDocAD26     DTDocAamg
024500941213     C*
024600941213     C* richiesta Inizio elenco da...
024700941213     C                   EXSR      POSSQL
024800941130     C*
024900941130     C* DECLARE
025000941130     C     $GEST         CASEQ     '01'          ORD01
025100941130     C                   ENDCS
025200941130     C*
025300941130     C* OPEN
025400941130     C                   EXSR      OPNCUR
025500941130     C*
025600941201     C                   ENDIF
025700941201     C*
025800941201     C                   ENDSR
025900941213     C/EJECT
026000941213     C************************************************************
026100941213     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
026200941213     C************************************************************
026300941213     C     POSSQL        BEGSR
026400941213     C*
026500941213     C* Gestione posizionamento
026600941213     C                   SELECT
026700980924     C* Ord.1 = data finanziamento
026800150916     C     ORDD26        WHENEQ    1
026900150916     C     POSD26        ANDNE     *LOVAL
027000150916     C                   MOVEL     POSD26        DTREGDAD26
027100941213     C                   ENDSL
027200941213     C*
027300941213     C                   ENDSR
027400941130     C/EJECT
027500160301     C************************************************************
027600160301     C* Imposta Ok sqlcod
027700160301     C************************************************************
027800160301     C     ImpSqlCod     BEGSR
027900160301     C*
028000160301     C                   select
028100160301     C* Errore sql eseguo cambio job per ottenere spool del joblog
028200160301     C                   when      sqlcod < 0
028300160301     C                   call      'X66CHGJOB'
028400160301     C                   eval      OkSqlCod = *off
028500160301     C* fine lettura
028600160301     C                   when      sqlcod = 100
028700160301     C                   eval      OkSqlCod = *off
028800160301     C* ok sql
028900160301     C                   other
029000160301     C                   eval      OkSqlCod = *on
029100160301     C*
029200160301     C                   endsl
029300160301     C*
029400160301     C                   endsr
029500160301     C/EJECT
029600941130     C************************************************************
029700941130     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
029800941130     C************************************************************
029900941130     C     DEFCAM        BEGSR
030000941130     C*
030100941130     C* klist
030200941130     C*
030300941130     C*
030400941130     C* Variabili per gestione pgm
030500941130     C*
030600941130     C                   MOVE      *BLANK        $GEST             2            fmt in gestione
030700941201     C                   MOVE      *BLANK        $OPEN             1
030800941130     C*
030900941130     C*
031000941130     C                   ENDSR
