000100041129     H DECEDIT('0,') DATEDIT(*DMY.)
000200980408      ***********************************************************************
000300041129      *   SELEZIONA fra i vari Msg IFTSTA E.D.I.   ESTERO solo quelli       *
000400041129      *   x LINEA appartenente all'UTENTE che utilizza la funzione.         *
000500041129      *   Quindi l'utente deve gestire solo i messaggi dei propri Partner.  *
000600930128      ***********************************************************************
000700170510     FTRTC84D1  CF   E             WORKSTN usropn
000800041201     FRCVIFTSTA1uF   E           k DISK
000900041201     FEDIFTSTA  o  a E             DISK    usropn
001000041201     FEDTAB01L  iF   E           k DISK
001100170510     FazORG01L  iF   E           k DISK
001200980312      *---------------------------------------------------------------------*
001300980312      * Schiere
001400980312      *---------------------------------------------------------------------*
001500040604     D* DS PER TRUL06R - CARICAMENTO £X
001600120910     D***** DSUL06        E DS                  EXTNAME(TRUL06DS)
001700120910     D****  LIN                    1     90  0 DIM(30)                              P.O. COMODO
001800120910     D TRUL09DS      E DS
001900120910     D  LIN                   19    108  0 DIM(30)                              P.O. COMODO
002000040604      *
002100170510      ****-?Caricamento filiali contabili
002200170510     d TRUL31ds      e ds                  inz
002300170510     d $POg            s              3    inz   dim(250)
002400170510      ****-?
002500170510     D LnaUser         S              3  0 DIM(100)                             FIL. GESTITE
002600000119      *---------------------------------------------------------------------*
002700000119      * DS
002800000119      *---------------------------------------------------------------------*
002900960528     D KPJBA         E DS
003000000119     D UT§DSE0F      E DS
003100021114     D Tibs36ds      E DS
003200960528     D CNCR80        E DS
003300041201      *
003400041201     D EDIDSMS       E DS
003500041201     D Sav_Nr784       s              7  0 INZ(0)
003600041201     D Sav_Nr781       s              7  0 INZ(0)
003700041201     D  MSGDS          S            100
003800041201     D  LENGH          S             15  5
003900041201     D X               s              3  0 INZ(0)
004000041201     D Almeno_Uno      s              2    INZ('  ')
004100041201     D CMD             S             78    DIM(1) CTDATA PERRCD(1)
004200980225      *---------------------------------------------------------------------*
004300980225      * Ciclo principale
004400980225      *---------------------------------------------------------------------*
004500040604     C* Lettura file
004600041201     c                   move      'OK'          Stato_OK          2
004700041201     C     Stato_OK      SETLL     RCVIFTSTA1
004800041201     C     Stato_OK      READe     RCVIFTSTA1
004900041201     c                   Dow       not %eof(RCVIFTSTA1)
005000980225      *
005100041201      *  Controlla con il tipo di messaggio il relativo numeratore
005200120910      *   quindi se è gestito come LNA dall'Utente che ha lanciato la Procedura
005300041201     **
005400041201     c                   if        RRtipo = '784' and RRnumero < sav_nr784 or
005500041201     c                             RRtipo = '781' and RRnumero < sav_nr781
005600041201     **
005700041201     **  A questo punto controlla se la Linea appartiene all'utente
005800041201     C                   eval      x = 1
005900120910     C     RRlinea       lookup    LnaUser(X)                             55
006000041201      *  Se appartiene: può elaborare i records
006100041201     c                   if        *in55 = *ON
006200041201     **
006300041201     **            **---------------------------------------**
006400041201     **  Scrittura del file EDIFTSTA che verrà letto dal TRTC84R
006500041201     **    al posto del vecchio TRIFTSTA multimembro
006600041201     c                   clear                   IFTSTA00
006700041201     c                   movel     RRIFTSTA      SSIFTSTA
006800041201     c                   write     IFTSTA00
006900041201     c                   eval      Almeno_Uno='OK'
007000041201     **            **---------------------------------------**
007100041201     **
007200041201     ** Dopo aver scritto e passato il record del Flat file
007300041201     **  aggiorna il record che ha generato la scrittura
007400041201     **   impostando lo stato di avanzamento del record a RX = Ricevuto
007500041201     c                   move      'RX'          RRelabora
007600041201     c                   update    RCVdati
007700041201     **
007800041201     c                   end
007900041201     c                   end
008000041201      *
008100041201     C     Stato_OK      READe     RCVIFTSTA1
008200040604     c                   end
008300980225      * Fine pgm.
008400040604     c     fine          TAG
008500041201      *
008600041201      *      Imposta se ci sono dei records oppure no
008700041201     c                   clear                   kpjbu
008800041201     c                   movel     Almeno_Uno    kpjbu
008900040604      *
009000960531     C                   SETON                                        LR
009100041201      *----------------------------------------------------------------
009200041201      *    Fase iniziale
009300041201      *----------------------------------------------------------------
009400041201     c     *inzsr        begsr
009500041201      *
009600041201     C     *ENTRY        PLIST
009700041201     C                   PARM                    KPJBA
009800041201     c                   clear                   kpjbu
009900041201      *
010000041201     C     Ktab          KLIST
010100041201     C                   KFLD                    tabcod
010200041201     C                   KFLD                    tabkey
010300041201      *
010400041201      * Pulisce il file creato in QTEMP quindi lo apre
010500041201     C                   clear                   MSGDS
010600041201     C                   MOVEL     CMD(1)        MSGDS
010700041201     C                   Z-ADD     100           LENGH
010800041201     C                   CALL      'QCMDEXC'
010900041201     C                   PARM                    MSGDS
011000041201     C                   PARM                    LENGH
011100041201     c                   open      EDIFTSTA
011200041201      *
011300041201      * Recupera dati dell'utente
011400041201     C                   Z-ADD     1             CODUT             1 0
011500041201     C                   CALL      'X§PARUT'
011600041201     C                   PARM                    UT§DSE0F
011700041201     C                   MOVEL     RAGUT         RSUT             20
011800041201     C                   MOVEL     REC80         CNCR80
011900041201      *
012000041201      *  Quindi decodifica il profilo generico EDPCEDxxx
012100041201      *   dove xxx è il TERMINAL
012200041201     C                   CLEAR                   Tibs36Ds
012300041201     c                   EVAL      I36ute = Knmus
012400041201     c                   EVAL      I36Tla = 'L'
012500041201     C                   CALL      'TIBS36R'
012600041201     C                   PARM                    Tibs36Ds
012700170510      ** ---------
012800170510      **    >> P.O.CONTABILE è la chiave di tutto il meccanismo <<
012900170510     c     simPOU        chain     azORG01L
013000170510     c                   if        %Found(azORG01L)
013100170510     c                   if        simpou <> orgFC0
013200170510     c                   open      TRTC84D1
013300170510      *  messaggio a video "UTENTE non abilitato alla CHIUSURA DISTINTA" e
013400170510      *  chiusura del pgm
013500170510     c                   dow       *inKC = '0'
013600170510     c                   exfmt     TC84ALT
013700170510     c                   enddo
013800170510     C                   goto      FINE
013900170510     c                   end
014000170510     c                   end
014100170510      ** ---------
014200041201     C* CARICO TABELLA FILIALI GESTITE £1
014300120910     C***                CLEAR                   DSUL06
014400120910     C***                MOVE      '£1'          D06COD
014500120910     C***                MOVEL     SIMPOU        D06KEY
014600120910     C***                MOVEL     ' '           D06TLA
014700120910     C***                MOVEL     DSUL06        KPJBU
014800120910     C***                CALL      'TRUL06R'
014900120910     C***                PARM                    KPJBA
015000120910     C***                MOVEL     KPJBU         DSUL06
015100120910     C***                MOVEA     LIN           LnpUser
015200120910      ***
015300120910     c* Carico x terminal  di arrivo  anzichè quelli di partenza
015400120910      *** stiamo trattando i dati di ritorno del EXPORT
015500170510     c******             clear                   trul09ds
015600170510     c******             movel     simPOU        i09tfa
015700170510     c******             movel     'TFA'         i09mod
015800170510     c******             call      'TRUL09R'
015900170510     c******             parm                    trul09ds
016000170510     C******             MOVEA     LIN           LnaUser
016100170510      **
016200170510      **  ATTENZIONE: l'UTENTE che deve eseguire la chiusura DISTINTA ESTERO
016300170510      **   deve essere quello che appartiene al P.O.CONTABILE
016400170510      **  come avviene per il FNLR63R le linee trattate devono essere impostate
016500170510      **  con il TRUL31R mediante la filiale di contabilità
016600170510      **
016700170510      * Reperimento dei P.O. di CONTABILITA' gestibili dalla filiale
016800170510     c                   clear                   TRUL31DS
016900170510     c                   clear                   LNAuser
017000170510     c                   eval      I31abi = 'AC'
017100170510     c                   eval      I31cpo = simPOU
017200170510     c                   call      'TRUL31R'
017300170510     c                   parm                    KPJBA
017400170510     c                   parm                    TRUL31DS
017500170510     c                   if        o31pog > *zeros
017600170510     c                   movea     o31pog        $POg
017700170510     c                   z-add     0             LUsr              3 0
017800170510     c                   do        100           USR               3 0
017900170510     c                   if        $POg(usr) <> *blank
018000170510     c                   add       1             LUsr
018100170510     c                   movel     $POg(usr)     LnaUser(LUsr)
018200170510     c                   end
018300170510     c                   enddo
018400170510     c                   endif
018500041201      *
018600041201      * Reperisce il numeratore del messaggio
018700041201     C                   MOVEL(p)  'MS'          TABCOD
018800041201     c                   MOVEL(p)  '784SEDE'     TABKEY
018900041201     C     Ktab          chain(n)  EDTAB01L
019000041201     C                   if        %found(EDTAB01L)
019100041201     C                   MOVEL     TABUNI        edidsms
019200041201     c                   z-add     §MSNUM        sav_nr784
019300041201     c                   end
019400041201     C*
019500041201     C                   MOVEL(p)  'MS'          TABCOD
019600041201     c                   MOVEL(p)  '781SEDE'     TABKEY
019700041201     C     Ktab          chain(n)  EDTAB01L
019800041201     C                   if        %found(EDTAB01L)
019900041201     C                   MOVEL     TABUNI        edidsms
020000041201     c                   z-add     §MSNUM        sav_nr781
020100041201     c                   end
020200041201     C*
020300041201     c                   endsr
020400041201      *----------------------------------------------------------------
020500041201** (CMD)
020600041201 CLRPFM FILE(EDIFTSTA)
