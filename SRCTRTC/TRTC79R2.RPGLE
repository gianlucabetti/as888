000100041129     H DECEDIT('0,') DATEDIT(*DMY.)
000200980408      ***********************************************************************
000300041202      *   Prepara i Msg IFCSUM E.D.I. per essere letti e importati sugli    *
000400041202      *   archivi Bartolini.                                                *
000500930128      ***********************************************************************
000600041202     FRCVIFCSUM1uF   E           k DISK
000700041202     FEDIFCSUM  o  a E             DISK    usropn rename(RRIFCSUM:RECORDSUM)
000800041202     F                                     prefix(OUT)
000900041201     FEDTAB01L  iF   E           k DISK
001000980312      *---------------------------------------------------------------------*
001100980312      * Schiere
001200000119      *---------------------------------------------------------------------*
001300000119      * DS
001400000119      *---------------------------------------------------------------------*
001500960528     D KPJBA         E DS
001600041203     D ds_KPJBU        DS
001700041203     D   num_nr                       7  0
001800041203     D   sede_alf                     4
001900041203     D   Lnp_nr                       3  0
002000041203     D   Hub_nr                       3  0
002100000119     D UT§DSE0F      E DS
002200021114     D Tibs36ds      E DS
002300960528     D CNCR80        E DS
002400041201      *
002500041201     D EDIDSMS       E DS
002600041202     D Sav_Nr785       s              7  0 INZ(0)
002601061214     D Sav_Nr730       s              7  0 INZ(0)
002602070108     D Sav_Nr350       s              7  0 INZ(0)
002700041201     D  MSGDS          S            100
002800041201     D  LENGH          S             15  5
002900041201     D X               s              3  0 INZ(0)
003000041201     D Almeno_Uno      s              2    INZ('  ')
003100041201     D CMD             S             78    DIM(1) CTDATA PERRCD(1)
003200980225      *---------------------------------------------------------------------*
003300980225      * Ciclo principale
003400980225      *---------------------------------------------------------------------*
003500041203     c     OK_x_LNP      klist
003600041203     c                   kfld                    Stato_OK
003700041203     c                   kfld                    Lnp_OK
003800040604     C* Lettura file
003900041201     c                   move      'OK'          Stato_OK          2
004000041203     c                   move      Lnp_nr        Lnp_OK            3 0
004100041203     C     OK_x_LNP      SETLL     RCVIFCSUM1
004200041203     C     OK_x_LNP      READe     RCVIFCSUM1
004300041202     c                   Dow       not %eof(RCVIFCSUM1)
004400980225      *
004500041201      *  Controlla con il tipo di messaggio il relativo numeratore
004600041201      *   quindi se è gestito come LNP dall'Utente che ha lanciato la Procedura
004700061214     c                   if        RRtipo = '785' and RRnumero < sav_nr785 or
004701070108     c                             RRtipo = '730' and RRnumero < sav_nr730 or
004702070108     c                             RRtipo = '350' and RRnumero < sav_nr350
004900041220     **
005000041220     ** L'unico problema potrebbe essere all'inizio dell'anno quando i numeratori
005100041220     **  vengono fatti ripartire potrebbe rimanere qualche messaggio sulla mezzanotte
005200041220     **   non tradotto poichè il messaggio ha un numero superiore al nuovo nr.in tabella
005300041220     **  Però questo è un caso veramente difficile da verificarsi.
005400041201     **
005500041201     **            **---------------------------------------**
005600070104     **  Scrittura del file EDIFCSUM che verrà letto dal TRTC79R
005700041202     **    al posto del vecchio TRIFCSUM multimembro
005800041202     c                   clear                   outRRIFCSUM
005900041202     c                   movel     RRIFCSUM      outRRIFCSUM
006000041202     c                   write     RECORDSUM
006100041201     c                   eval      Almeno_Uno='OK'
006200041201     **            **---------------------------------------**
006300041201     **
006400041201     ** Dopo aver scritto e passato il record del Flat file
006500041201     **  aggiorna il record che ha generato la scrittura
006600041201     **   impostando lo stato di avanzamento del record a RX = Ricevuto
006700041201     c                   move      'RX'          RRelabora
006800041201     c                   update    RCVdati
006900041201     **
007100041220     c                   end
007200041201      *
007300041203     C     OK_x_LNP      READe     RCVIFCSUM1
007400040604     c                   end
007500980225      * Fine pgm.
007600040604     c     fine          TAG
007700041201      *
007800041201      *      Imposta se ci sono dei records oppure no
007900041203     c                   move      kpjbu_sav     kpjbu
008000041201     c                   movel     Almeno_Uno    kpjbu
008100040604      *
008200960531     C                   SETON                                        LR
008300041201      *----------------------------------------------------------------
008400041201      *    Fase iniziale
008500041201      *----------------------------------------------------------------
008600041201     c     *inzsr        begsr
008700041201      *
008800041201     C     *ENTRY        PLIST
008900041201     C                   PARM                    KPJBA
009000041203     c     *like         define    kpjbu         kpjbu_sav
009100041203     c                   move      kpjbu         kpjbu_sav
009200041203     c                   move      kpjbu         ds_kpjbu
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
010500041202     c                   open      EDIFCSUM
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
012200041201      * Reperisce il numeratore del messaggio
012300041201     C                   MOVEL(p)  'MS'          TABCOD
012400041202     c                   MOVEL(p)  '785SEDE'     TABKEY
012500041201     C     Ktab          chain(n)  EDTAB01L
012600041201     C                   if        %found(EDTAB01L)
012700041201     C                   MOVEL     TABUNI        edidsms
012800041202     c                   z-add     §MSNUM        sav_nr785
012900041201     c                   end
012901061214     C***
012902061214      * Reperisce il numeratore del messaggio
012903061214     C                   MOVEL(p)  'MS'          TABCOD
012904061214     c                   MOVEL(p)  '730SEDE'     TABKEY
012905061214     C     Ktab          chain(n)  EDTAB01L
012906061214     C                   if        %found(EDTAB01L)
012907061214     C                   MOVEL     TABUNI        edidsms
012908061214     c                   z-add     §MSNUM        sav_nr730
012909061214     c                   end
012910070108     C***
012911070108      * Reperisce il numeratore del messaggio
012912070108     C                   MOVEL(p)  'MS'          TABCOD
012913070108     c                   MOVEL(p)  '350SEDE'     TABKEY
012914070108     C     Ktab          chain(n)  EDTAB01L
012915070108     C                   if        %found(EDTAB01L)
012916070108     C                   MOVEL     TABUNI        edidsms
012917070108     c                   z-add     §MSNUM        sav_nr350
012918070108     c                   end
013000041201     C*
013100041201     c                   endsr
013200041201      *----------------------------------------------------------------
013300041201** (CMD)
013400041202 CLRPFM FILE(EDIFCSUM)
