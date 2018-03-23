000100040220      *------------------------------------------------------------------------*
000200160408      * REPERIMENTO competenze di logistica                                    *
000300040220      *------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500040220
000600160408     FSISDE01L  IF   E           K DISK
000800040220
000900041220     D*-------------------
001000041220     D* DS DI PROCEDURA
001100041220     D*-------------------
001200160408     D TISE72DS      E DS
001201160408     D dsdelog       E DS
002300041220     D*-------------------
002400041220     D* DS REPERIMENTI UNIFICANTI
002500040220     D*------------------
002600040622     D TIBS10DS      E DS
002700041221     D  skCLI                 21   5520  0 DIM(500) INZ(*zeros)                 *SCHIERA CLIENTI PADRI/FIGLI
002800041220     D*------------------
002900041220     D* DS SUDDIVISIONE DATA INIZIALE
003000041220     D*------------------
003100041220     D                 DS
003200041220     D  ANNINI                 1      4  0
003300041220     D  MESINI                 5      6  0
003400041220     D  DATINI                 1      6  0
003500070516
003600160408     d  wctr           s                   like(sdectr) inz
003601160420     d  totale         s             18  5
003700070516
003800041220     C*------------------
003900041220     C* MAIN
004000041220     C*------------------
004100070515      *
004200070516      * verifico se passato codice tariffa
004300160408     c                   if        i72ctr <> *blanks
004400160408     c                   move      i72ctr        wctr
004500070516     c                   endif
004900041220     C*
005000041220     C* INNANZITUTTO VERIFICO SE RICHIESTA ELABORAZIONE CLIENTE X UNIFICANTE
005100160408     C                   IF        I72TLA = 'U'
005200041220     C                   EXSR      RTVPARENT
005300041220     C                   ELSE
005400160408     C                   Z-ADD     I72KSC        skCLI(1)
005500041220     C                   ENDIF
005600041220     C*
005700041220     C* A QUESTO PUNTO CALCOLO I SALDI DEL/DEI CLIENTI COSI REPERITI
005800160408     C                   Z-ADD     I72DAI        DATINI                         *DS DATA INIZIALE
005900041220     C*
006000041220     C     1             DO        500           jCLI              3 0
006100041220     C*
006200041220     C                   IF        skCLI(jCLI) = *zeros
006300041220     C                   LEAVE
006400041220     C                   ELSE
006500160408     C                   Z-ADD     skCLI(jCLI)   sdeKSC
006600160408     C                   Z-ADD     ANNINI        sdeANN                         *ANNO INIZIALE
006700160408     C                   Z-ADD     MESINI        sdeMES                         *MESE INIZIALE
006800041220     C*
006900041220     C* POSIZIONAMENTO INIZIALE X KSC/ANN/MES
007000160408     C     KEYsde        SETLL     SIsde01L
007100041220     C*
007200041220     C* CICLO X KSC X TOTALIZZARE PERIODO RICHIESTO NEL LANCIO
007300041220     C                   DO        *HIVAL
007400160408     C     KEYsde_KSC    READE     SIsde01L
007500160408     C                   IF        %eof(SIsde01L)
007600160408     C                             OR  sdeANN*100+sdeMES > I72DAF
007700041220     C                   LEAVE
007800041220     C                   ELSE
007900070516      * se richiesto codice tariffa ignoro record se codice tariffa <>
008000160408     c                   if        i72ctr <> *blanks and wctr <> sdectr
008100070516     c                   iter
008200070516     c                   endif
008201160408     C*
008202160408     c                   eval      dsdelog = sdeuni
009200160420     C                   add       §logicmp      totale
011000041222     C*
011100041220     C                   ENDIF
011200041220     C                   ENDDO
011300000000     C*
011400041220     C                   ENDIF
011600041220     C                   ENDDO
011601160420     C                   EVAL      O72icmp = totale
011700040622     C*
011800041220     C                   SETON                                        LR
011900041220     C*------------------------------------------------------------------------*
012000041220     C* RTVPARENT - REPERIMENTO PARENTI (PADRI/FIGLI)
012100041220     C*------------------------------------------------------------------------*
012200041220     C     RTVPARENT     BEGSR
012300041220     C*
012400041220     C* 1° STEP: considero il codice corrente come figlio e cerco il padre e i fratelli
012500041220     C                   CLEAR                   TIBS10DS
012600160408     C                   EVAL      D10TLE = I72TLE
012700041220     C                   EVAL      D10PAF = 'P'
012800160408     C                   EVAL      D10COD = I72KSC
012900041220     C                   CALL      'TIBS10R'
013000041220     C                   PARM                    TIBS10DS
013100041220     C                   PARM      'E'           TIPEXE            1
013200041220     C*
013300041220     C* 2° STEP: se non è figlio allora lo considero padre e cerco i suoi figli
013400041220     C                   IF        D10ERR = '1'
013500041220     C                   CLEAR                   TIBS10DS
013600160408     C                   EVAL      D10TLE = I72TLE
013700041220     C                   EVAL      D10PAF = 'F'
013800160408     C                   EVAL      D10COD = I72KSC
013900041220     C                   CALL      'TIBS10R'
014000041220     C                   PARM                    TIBS10DS
014100041220     C                   PARM      'E'           TIPEXE
014200041220     C*
014300041220     C* A questo punto traggo le conclusioni e valorizzo la schiera finale
014400041220     C                   IF        D10ERR = '1'
014500041220     C                   MOVE(P)   D10COD        skCLI(1)
014600041220     C                   ENDIF
014700041220     C                   ENDIF
014800041220     C*
014900041220     C                   ENDSR
017300041220     C*------------------------------------------------------------------------*
017400041220     C*?*INZSR - ROUTINE INIZIALE DI IMPOSTAZIONI E DICHIARAZIONI
017500041220     C*------------------------------------------------------------------------*
017600040219     C     *INZSR        BEGSR
017700041220     C*
017800041220     C* RICEVIMENTO PARAMETRI
017900040219     C     *ENTRY        PLIST
018000160408     C                   PARM                    TISE72DS
018100041220     C*
018900160408     C                   CLEAR                   O72icmp
018901160420     C                   CLEAR                   totale
018902160408     c                   eval      SDEFILE = 'SDC'
018903160408     c                   eval      SDETRD = 'LOG'
019000041220     C*
019100160408     C* CHIAVE LETTURA SIsde01L - COMPLETA
019200160408     C     KEYsde        KLIST
019300160408     C                   KFLD                    SDEFILE                        *CLIENTE
019301160408     C                   KFLD                    SDETRD                         *CLIENTE
019302160408     C                   KFLD                    sdeKSC                         *CLIENTE
019400160408     C                   KFLD                    sdeANN                         *ANNO
019500160408     C                   KFLD                    sdeMES                         *MESE
019700160408     C* CHIAVE LETTURA SIsde01L - PARZIALE (SOLO CLIENTE)
019800160408     C     KEYsde_KSC    KLIST
019801160408     C                   KFLD                    SDEFILE                        *CLIENTE
019802160408     C                   KFLD                    SDETRD                         *CLIENTE
019900160408     C                   KFLD                    sdeKSC                         *CLIENTE
020600040219     C                   ENDSR
