000100070104     H DECEDIT('0,') DATEDIT(*DMY.)
000200980408      ***********************************************************************
000300041206      *   Con i records non cancellati da EDIFCSUM in QTEMP deve            *
000400041206      *   aggiornare il file RCVIFCSUM riportando l'errore tab.PT           *
000500930128      ***********************************************************************
000600041206     FRCVIFCSUM2uF   E           k DISK
000700041206     FEDIFCSUM  iF   E             DISK    rename(RRIFCSUM:RECORDSUM)
000800041202     F                                     prefix(OUT)
000900000119      *---------------------------------------------------------------------*
001000000119      * DS
001100000119      *---------------------------------------------------------------------*
001200960528     D KPJBA         E DS
001300041203     D ds_KPJBU        DS
001400041203     D   num_nr                       7  0
001500041203     D   sede_alf                     4
001600041203     D   Lnp_nr                       3  0
001700041203     D   Hub_nr                       3  0
001800041206     D EDTT00DS      E DS                  prefix(SM_)                          Sum
001900041206     D Sav_nome        S             35
002000041206     D sav_date        S              8  0 INZ(0)
002100041206     D sav_time        S              4  0 INZ(0)
002200041201      *
002300041201     D X               s              3  0 INZ(0)
002400041201     D Almeno_Uno      s              2    INZ('  ')
002500041206      *---------------------------------------------------------------------*
002600041206      *                    .. 5   8  : tipo record letto
002700041206     D DSdati          DS           587
002800041206     D  SUMTPR                 5      8
002900041206     D  SUMINR                 5      6
003000980225      *---------------------------------------------------------------------*
003100980225      * Ciclo principale
003200980225      *---------------------------------------------------------------------*
003300041206     C     *ENTRY        PLIST
003400041206     C                   PARM                    KPJBA
003500041206     c     *like         define    kpjbu         kpjbu_sav
003600041206      *
003700041206     c                   move      kpjbu         kpjbu_sav
003800041206     c                   move      kpjbu         ds_kpjbu
003900041206      *
004000041206     c                   read      edifcsum
004100041206     c                   dow       not %eof(edifcsum)
004200041206      *                          ++++++++++++++++++++++++
004300041206     C                   MOVEL     outRRIFCSUM   DSdati
004400041206      * -----------              ++++++++++++++++++++++++
004500041206      * Testata MSG
004600041206      * -----------
004700041206     C                   If        SUMTPR = 'TT00'                              TESTATA
004800041206      **              *--------------------------*
004900041206     C                   EXSR      DATI_Testata
005000041206      **              *--------------------------*
005100041206     C                   EndIF
005200041206      *
005300041206     c                   read      edifcsum
005400041206     c                   enddo
005500041206      * Fine pgm.
005600041206     c     fine          TAG
005700041206      *
005800041206      *      Imposta se ci sono dei records oppure no
005900041206     c                   move      kpjbu_sav     kpjbu
006000041206      *
006100041206     C                   SETON                                        LR
006200041206     C*-------------------------------------------------------------------------------------
006300041206     C*?    Controlla la testata a quale Filiale appartiene il Partner
006400041206     C*-------------------------------------------------------------------------------------
006500041206     C     DATI_Testata  BEGSR
006600041206      *
006700041206     c     RX_x_Nome     klist
006800041206     c                   kfld                    Stato_RX
006900041206     c                   kfld                    sav_date
007000041206     c                   kfld                    time06            6 0
007100041206     c                   kfld                    sav_nome
007200041206     c                   move      'RX'          Stato_RX          2
007300041206      *
007400041206     C* inizializzo i campi di salvataggio codici
007500041206     C                   clear                   EDTT00DS
007600041206     C                   clear                   sav_nome
007700041206     C                   clear                   sav_date
007800041206     C                   clear                   sav_time
007900041206      *    Bolle
008000041206     C                   MOVEL     outRRIFCSUM   EDTT00DS
008100041206      * dati da riportare sul file x identificare il messaggio
008200041206     C                   MOVE      Sm_TA1004     sav_nome
008300041206     C                   MOVE      Sm_TA0017     sav_date
008400041206     C                   MOVEL     '20'          sav_date
008500041206     C                   MOVE      Sm_TA0019     sav_time
008600041206     C     sav_time      mult      100           time06
008700041206      *
008800041206     C* Legge e aggiorna i records x la traduzione (ERRATI)
008900041206     C     RX_x_Nome     SETLL     RCVIFCSUM2
009000041206     C     RX_x_Nome     READe     RCVIFCSUM2
009100041206     c                   Dow       not %eof(RCVIFCSUM2)
009200041216     **   impostando lo stato di avanzamento del record a EF = Errato in Filiale
009300041216     c                   move      'EF'          RRelabora
009400041206     c                   update    RCVdati
009500041206     C     RX_x_Nome     READe     RCVIFCSUM2
009600041206     c                   endDo
009700041206      *
009800041206     C                   Endsr
009900041206     C*----------------------------------------------------------------
