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
001400120910     D***** DSUL06        E DS                  EXTNAME(TRUL06DS)
001500120910     D****  LIN                    1     90  0 DIM(30)                              P.O. COMODO
001600120910     D TRUL09DS      E DS
001700120910     D  LIN                   19    108  0 DIM(30)                              P.O. COMODO
001800040604      *
001900120910     D LnaUser         S              3  0 DIM(30)                              FIL GESTITE
002000000119      *---------------------------------------------------------------------*
002100000119      * DS
002200000119      *---------------------------------------------------------------------*
002300960528     D KPJBA         E DS
002400000119     D UT§DSE0F      E DS
002500021114     D Tibs36ds      E DS
002600960528     D CNCR80        E DS
002700041201      *
002800041201     D EDIDSMS       E DS
002900041201     D Sav_Nr784       s              7  0 INZ(0)
003000041201     D Sav_Nr781       s              7  0 INZ(0)
003100041201     D  MSGDS          S            100
003200041201     D  LENGH          S             15  5
003300041201     D X               s              3  0 INZ(0)
003400041201     D Almeno_Uno      s              2    INZ('  ')
003500041201     D CMD             S             78    DIM(1) CTDATA PERRCD(1)
003600980225      *---------------------------------------------------------------------*
003700980225      * Ciclo principale
003800980225      *---------------------------------------------------------------------*
003900040604     C* Lettura file
004000041201     c                   move      'OK'          Stato_OK          2
004100041201     C     Stato_OK      SETLL     RCVIFTSTA1
004200041201     C     Stato_OK      READe     RCVIFTSTA1
004300041201     c                   Dow       not %eof(RCVIFTSTA1)
004400980225      *
004500041201      *  Controlla con il tipo di messaggio il relativo numeratore
004600120910      *   quindi se è gestito come LNA dall'Utente che ha lanciato la Procedura
004700041201     **
004800041201     c                   if        RRtipo = '784' and RRnumero < sav_nr784 or
004900041201     c                             RRtipo = '781' and RRnumero < sav_nr781
005000041201     **
005100041201     **  A questo punto controlla se la Linea appartiene all'utente
005200041201     C                   eval      x = 1
005300120910     C     RRlinea       lookup    LnaUser(X)                             55
005400041201      *  Se appartiene: può elaborare i records
005500041201     c                   if        *in55 = *ON
005600041201     **
005700041201     **            **---------------------------------------**
005800041201     **  Scrittura del file EDIFTSTA che verrà letto dal TRTC84R
005900041201     **    al posto del vecchio TRIFTSTA multimembro
006000041201     c                   clear                   IFTSTA00
006100041201     c                   movel     RRIFTSTA      SSIFTSTA
006200041201     c                   write     IFTSTA00
006300041201     c                   eval      Almeno_Uno='OK'
006400041201     **            **---------------------------------------**
006500041201     **
006600041201     ** Dopo aver scritto e passato il record del Flat file
006700041201     **  aggiorna il record che ha generato la scrittura
006800041201     **   impostando lo stato di avanzamento del record a RX = Ricevuto
006900041201     c                   move      'RX'          RRelabora
007000041201     c                   update    RCVdati
007100041201     **
007200041201     c                   end
007300041201     c                   end
007400041201      *
007500041201     C     Stato_OK      READe     RCVIFTSTA1
007600040604     c                   end
007700980225      * Fine pgm.
007800040604     c     fine          TAG
007900041201      *
008000041201      *      Imposta se ci sono dei records oppure no
008100041201     c                   clear                   kpjbu
008200041201     c                   movel     Almeno_Uno    kpjbu
008300040604      *
008400960531     C                   SETON                                        LR
008500041201      *----------------------------------------------------------------
008600041201      *    Fase iniziale
008700041201      *----------------------------------------------------------------
008800041201     c     *inzsr        begsr
008900041201      *
009000041201     C     *ENTRY        PLIST
009100041201     C                   PARM                    KPJBA
009200041201     c                   clear                   kpjbu
009300041201      *
009400041201     C     Ktab          KLIST
009500041201     C                   KFLD                    tabcod
009600041201     C                   KFLD                    tabkey
009700041201      *
009800041201      * Pulisce il file creato in QTEMP quindi lo apre
009900041201     C                   clear                   MSGDS
010000041201     C                   MOVEL     CMD(1)        MSGDS
010100041201     C                   Z-ADD     100           LENGH
010200041201     C                   CALL      'QCMDEXC'
010300041201     C                   PARM                    MSGDS
010400041201     C                   PARM                    LENGH
010500041201     c                   open      EDIFTSTA
010600041201      *
010700041201      * Recupera dati dell'utente
010800041201     C                   Z-ADD     1             CODUT             1 0
010900041201     C                   CALL      'X§PARUT'
011000041201     C                   PARM                    UT§DSE0F
011100041201     C                   MOVEL     RAGUT         RSUT             20
011200041201     C                   MOVEL     REC80         CNCR80
011300041201      *
011400041201      *  Quindi decodifica il profilo generico EDPCEDxxx
011500041201      *   dove xxx è il TERMINAL
011600041201     C                   CLEAR                   Tibs36Ds
011700041201     c                   EVAL      I36ute = Knmus
011800041201     c                   EVAL      I36Tla = 'L'
011900041201     C                   CALL      'TIBS36R'
012000041201     C                   PARM                    Tibs36Ds
012100041201     C***
012200041201     C* CARICO TABELLA FILIALI GESTITE £1
012300120910     C***                CLEAR                   DSUL06
012400120910     C***                MOVE      '£1'          D06COD
012500120910     C***                MOVEL     SIMPOU        D06KEY
012600120910     C***                MOVEL     ' '           D06TLA
012700120910     C***                MOVEL     DSUL06        KPJBU
012800120910     C***                CALL      'TRUL06R'
012900120910     C***                PARM                    KPJBA
013000120910     C***                MOVEL     KPJBU         DSUL06
013100120910     C***                MOVEA     LIN           LnpUser
013200120910      ***
013300120910     c* Carico x terminal  di arrivo  anzichè quelli di partenza
013400120910      *** stiamo trattando i dati di ritorno del EXPORT
013500120910     c                   clear                   trul09ds
013600120910     c                   movel     simPOU        i09tfa
013700120910     c                   movel     'TFA'         i09mod
013800120910     c                   call      'TRUL09R'
013900120910     c                   parm                    trul09ds
014000120910     C                   MOVEA     LIN           LnaUser
014100120910     c
014200041201      *
014300041201      * Reperisce il numeratore del messaggio
014400041201     C                   MOVEL(p)  'MS'          TABCOD
014500041201     c                   MOVEL(p)  '784SEDE'     TABKEY
014600041201     C     Ktab          chain(n)  EDTAB01L
014700041201     C                   if        %found(EDTAB01L)
014800041201     C                   MOVEL     TABUNI        edidsms
014900041201     c                   z-add     §MSNUM        sav_nr784
015000041201     c                   end
015100041201     C*
015200041201     C                   MOVEL(p)  'MS'          TABCOD
015300041201     c                   MOVEL(p)  '781SEDE'     TABKEY
015400041201     C     Ktab          chain(n)  EDTAB01L
015500041201     C                   if        %found(EDTAB01L)
015600041201     C                   MOVEL     TABUNI        edidsms
015700041201     c                   z-add     §MSNUM        sav_nr781
015800041201     c                   end
015900041201     C*
016000041201     c                   endsr
016100041201      *----------------------------------------------------------------
016200041201** (CMD)
016300041201 CLRPFM FILE(EDIFTSTA)
