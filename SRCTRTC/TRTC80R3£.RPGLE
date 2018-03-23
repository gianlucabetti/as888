000100041129     H DECEDIT('0,') DATEDIT(*DMY.)
000200980408      ***********************************************************************
000300041206      *   Con i records non cancellati da EDIFCSUM in QTEMP deve            *
000400041206      *   aggiornare il file RCVIFCSUM riportando l'errore tab.PT           *
000500930128      ***********************************************************************
000600041206     FRCVIFCSUM2uF   E           k DISK
000700041206     FEDIFCSUM  uF   E             DISK    rename(RRIFCSUM:RECORDSUM)
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
001900041201      *
002000041201     D X               s              3  0 INZ(0)
002100041201     D Almeno_Uno      s              2    INZ('  ')
002200041206      *---------------------------------------------------------------------*
002300041206      *                    .. 5   8  : tipo record letto
002400041206     D DSdati          DS           587
002500041206     D  SUMTPR                 5      8
002600041206     D  SUMINR                 5      6
002700980225      *---------------------------------------------------------------------*
002800980225      * Ciclo principale
002900980225      *---------------------------------------------------------------------*
003000041206     C     *ENTRY        PLIST
003100041206     C                   PARM                    KPJBA
003200041206     c     *like         define    kpjbu         kpjbu_sav
003300041206      *
003400041206     c                   move      kpjbu         kpjbu_sav
003500041206     c                   move      kpjbu         ds_kpjbu
003600041206      *
003700041206     c                   read      edifcsum
003800041206     c                   dow       not %eof(edifcsum)
003900041206      *                          ++++++++++++++++++++++++
004000041206     C                   MOVEL     outRRIFCSUM   DSdati
004100041206      * -----------              ++++++++++++++++++++++++
004200041206      * Testata MSG
004300041206      * -----------
004400041206     C                   If        SUMTPR = 'TT00'                              TESTATA
004500041206      **              *--------------------------*
004600041206     C                   EXSR      DATI_Testata
004700041206      **              *--------------------------*
004800041206     C                   EndIF
004900041206      *
005000041206     c                   read      edifcsum
005100041206     c                   enddo
005200041206      * Fine pgm.
005300041206     c     fine          TAG
005400041206      *
005500041206      *      Imposta se ci sono dei records oppure no
005600041206     c                   move      kpjbu_sav     kpjbu
005700041206      *
005800041206     C                   SETON                                        LR
005900041206     C*-------------------------------------------------------------------------------------
006000041206     C*?    Controlla la testata a quale Filiale appartiene il Partner
006100041206     C*-------------------------------------------------------------------------------------
006200041206     C     DATI_Testata  BEGSR
006300041206      *
006400041206     c     RX_x_Nome     klist
006500041206     c                   kfld                    Stato_RX
006600041206     c                   kfld                    sav_date
006700041206     c                   kfld                    sav_time
006800041206     c                   kfld                    sav_nome
006900041206     c                   move      'RX'          Stato_RX          2
007000041206      *
007100041206     C* inizializzo i campi di salvataggio codici
007200041206     C                   clear                   EDTT00DS
007300041206     C                   clear                   sav_nome
007400041206     C                   clear                   sav_date
007500041206     C                   clear                   sav_time
007600041206      *    Bolle
007700041206     C                   MOVEL     outRRIFCSUM   EDTT00DS
007800041206      * dati da riportare sul file x identificare il messaggio
007900041206     C                   MOVE      Sm_TA1004     sav_nome
008000041206     C                   MOVE      Sm_TA0017     sav_date
008100041206     C                   MOVEL     '20'          sav_date
008200041206     C                   MOVE      Sm_TA0019     sav_time
008300041206      *
008400041206     C* Lettura file
008500041206     C     RX_x_Nome     SETLL     RCVIFCSUM2
008600041206     C     RX_x_Nome     READe     RCVIFCSUM2
008700041206     c                   Dow       not %eof(RCVIFCSUM2)
008800041206     **   impostando lo stato di avanzamento del record a ER = Errato
008900041206     c                   move      'ER'          RRelabora
009000041206     c                   update    RCVdati
009100041206     c                   end
009200041206      *
009300041206     C     RX_x_Nome     READe     RCVIFCSUM2
009400041206     c                   end
009500041206      *
009600041206     C                   Endsr
009700041206     C*----------------------------------------------------------------
