000100041129     H DECEDIT('0,') DATEDIT(*DMY.)
000200980408      ***********************************************************************
000300041129      *   SELEZIONA fra i vari Msg IFTSTA E.D.I.   ESTERO solo quelli       *
000400041129      *   x LINEA appartenente all'UTENTE che utilizza la funzione.         *
000500041129      *   Quindi l'utente deve gestire solo i messaggi dei propri Partner.  *
000600930128      ***********************************************************************
000700041201     FRCVIFTSTA1uF   E           k DISK
000800041201     FEDIFTSTA  o  a E             DISK    usropn
000900041201     FEDTAB01L  iF   E           k DISK
001000980312      *---------------------------------------------------------------------*
001100980312      * Schiere
001200980312      *---------------------------------------------------------------------*
001300040604     D* DS PER TRUL06R - CARICAMENTO £X
001400040604     D DSUL06        E DS                  EXTNAME(TRUL06DS)
001500040604     D  LIN                    1     90  0
001600040604     D                                     DIM(30)                              FIL. COMODO
001700040604      *
001800041201     D LnpUser         S              3  0 DIM(30)                              FIL GESTITE
001900000119      *---------------------------------------------------------------------*
002000000119      * DS
002100000119      *---------------------------------------------------------------------*
002200960528     D KPJBA         E DS
002300000119     D UT§DSE0F      E DS
002400021114     D Tibs36ds      E DS
002500960528     D CNCR80        E DS
002600041201      *
002700041201     D EDIDSMS       E DS
002800041201     D Sav_Nr784       s              7  0 INZ(0)
002900041201     D Sav_Nr781       s              7  0 INZ(0)
003000041201     D  MSGDS          S            100
003100041201     D  LENGH          S             15  5
003200041201     D X               s              3  0 INZ(0)
003300041201     D Almeno_Uno      s              2    INZ('  ')
003400041201     D CMD             S             78    DIM(1) CTDATA PERRCD(1)
003500980225      *---------------------------------------------------------------------*
003600980225      * Ciclo principale
003700980225      *---------------------------------------------------------------------*
003800040604     C* Lettura file
003900041201     c                   move      'OK'          Stato_OK          2
004000041201     C     Stato_OK      SETLL     RCVIFTSTA1
004100041201     C     Stato_OK      READe     RCVIFTSTA1
004200041201     c                   Dow       not %eof(RCVIFTSTA1)
004300980225      *
004400041201      *  Controlla con il tipo di messaggio il relativo numeratore
004500041201      *   quindi se è gestito come LNP dall'Utente che ha lanciato la Procedura
004600041201     **
004700041201     c                   if        RRtipo = '784' and RRnumero < sav_nr784 or
004800041201     c                             RRtipo = '781' and RRnumero < sav_nr781
004900041201     **
005000041201     **  A questo punto controlla se la Linea appartiene all'utente
005100041201     C                   eval      x = 1
005200041201     C     RRlinea       lookup    LnpUser(X)                             55
005300041201      *  Se appartiene: può elaborare i records
005400041201     c                   if        *in55 = *ON
005500041201     **
005600041201     **            **---------------------------------------**
005700041201     **  Scrittura del file EDIFTSTA che verrà letto dal TRTC84R
005800041201     **    al posto del vecchio TRIFTSTA multimembro
005900041201     c                   clear                   IFTSTA00
006000041201     c                   movel     RRIFTSTA      SSIFTSTA
006100041201     c                   write     IFTSTA00
006200041201     c                   eval      Almeno_Uno='OK'
006300041201     **            **---------------------------------------**
006400041201     **
006500041201     ** Dopo aver scritto e passato il record del Flat file
006600041201     **  aggiorna il record che ha generato la scrittura
006700041201     **   impostando lo stato di avanzamento del record a RX = Ricevuto
006800041201     c                   move      'RX'          RRelabora
006900041201     c                   update    RCVdati
007000041201     **
007100041201     c                   end
007200041201     c                   end
007300041201      *
007400041201     C     Stato_OK      READe     RCVIFTSTA1
007500040604     c                   end
007600980225      * Fine pgm.
007700040604     c     fine          TAG
007800041201      *
007900041201      *      Imposta se ci sono dei records oppure no
008000041201     c                   clear                   kpjbu
008100041201     c                   movel     Almeno_Uno    kpjbu
008200040604      *
008300960531     C                   SETON                                        LR
008400041201      *----------------------------------------------------------------
008500041201      *    Fase iniziale
008600041201      *----------------------------------------------------------------
008700041201     c     *inzsr        begsr
008800041201      *
008900041201     C     *ENTRY        PLIST
009000041201     C                   PARM                    KPJBA
009100041201     c                   clear                   kpjbu
009200041201      *
009300041201     C     Ktab          KLIST
009400041201     C                   KFLD                    tabcod
009500041201     C                   KFLD                    tabkey
009600041201      *
009700041201      * Pulisce il file creato in QTEMP quindi lo apre
009800041201     C                   clear                   MSGDS
009900041201     C                   MOVEL     CMD(1)        MSGDS
010000041201     C                   Z-ADD     100           LENGH
010100041201     C                   CALL      'QCMDEXC'
010200041201     C                   PARM                    MSGDS
010300041201     C                   PARM                    LENGH
010400041201     c                   open      EDIFTSTA
010500041201      *
010600041201      * Recupera dati dell'utente
010700041201     C                   Z-ADD     1             CODUT             1 0
010800041201     C                   CALL      'X§PARUT'
010900041201     C                   PARM                    UT§DSE0F
011000041201     C                   MOVEL     RAGUT         RSUT             20
011100041201     C                   MOVEL     REC80         CNCR80
011200041201      *
011300041201      *  Quindi decodifica il profilo generico EDPCEDxxx
011400041201      *   dove xxx è il TERMINAL
011500041201     C                   CLEAR                   Tibs36Ds
011600041201     c                   EVAL      I36ute = Knmus
011700041201     c                   EVAL      I36Tla = 'L'
011800041201     C                   CALL      'TIBS36R'
011900041201     C                   PARM                    Tibs36Ds
012000041201     C***
012100041201     C* CARICO TABELLA FILIALI GESTITE £1
012200041201     C                   CLEAR                   DSUL06
012300041201     C                   MOVE      '£1'          D06COD
012400041201     C                   MOVEL     SIMPOU        D06KEY
012500041201     C                   MOVEL     ' '           D06TLA
012600041201     C                   MOVEL     DSUL06        KPJBU
012700041201     C                   CALL      'TRUL06R'
012800041201     C                   PARM                    KPJBA
012900041201     C                   MOVEL     KPJBU         DSUL06
013000041201     C                   MOVEA     LIN           LnpUser
013100041201      *
013200041201      * Reperisce il numeratore del messaggio
013300041201     C                   MOVEL(p)  'MS'          TABCOD
013400041201     c                   MOVEL(p)  '784SEDE'     TABKEY
013500041201     C     Ktab          chain(n)  EDTAB01L
013600041201     C                   if        %found(EDTAB01L)
013700041201     C                   MOVEL     TABUNI        edidsms
013800041201     c                   z-add     §MSNUM        sav_nr784
013900041201     c                   end
014000041201     C*
014100041201     C                   MOVEL(p)  'MS'          TABCOD
014200041201     c                   MOVEL(p)  '781SEDE'     TABKEY
014300041201     C     Ktab          chain(n)  EDTAB01L
014400041201     C                   if        %found(EDTAB01L)
014500041201     C                   MOVEL     TABUNI        edidsms
014600041201     c                   z-add     §MSNUM        sav_nr781
014700041201     c                   end
014800041201     C*
014900041201     c                   endsr
015000041201      *----------------------------------------------------------------
015100041201** (CMD)
015200041201 CLRPFM FILE(EDIFTSTA)
