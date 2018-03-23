000100050131     H DECEDIT('0,') DATEDIT(*YMD.)
000200050127      ***********************************************************************
000300050127      * - Se i primi 2 byte della KPJBU sono Blank:
000400050127      *   Prepara i Msg E.D.I. da spedire sulla EDFLATOUT2 SSIFCSUM/SSIFTSTA
000500050127      *   passando x QTEMP con EDIFCSUM/EDIFTSTA
000600050127      * - Se i primi 2 byte della KPJBU sono "EF" :
000700050127      *   Aggiorna il msg come errato sui SNDIFCSUM/SNDIFTSTA e invia un msg
000800050127      *   di avviso.
000900930128      ***********************************************************************
001000050127     FSNDIFCSUM1uF   E           k DISK    usropn rename(SNDDATI:SNDSUM)
001100050131     f                                     commit
001200050127     FSNDIFTSTA1uF   E           k DISK    usropn rename(SNDDATI:SNDSTA)
001300050131     f                                     commit
001400050901     Fwfsnd00w  o  a E             DISK
001500000119      *---------------------------------------------------------------------*
001600000119      * DS
001700000119      *---------------------------------------------------------------------*
001800960528     D KPJBA         E DS
001900050127      *
002000041203     D ds_KPJBU        DS
002100050127     D   Tip_Fil                     10
002200050131     D   cod_Fil                      3
002300050127     D   num_nr                       7  0
002400050131     D   Emme                         1
002500050131     D   Lnp_nr                       3  0
002600050131     D   sede_alf                     3
002700041203     D   Hub_nr                       3  0
002800041201      *
002900041201     D  MSGDS          S            100
003000041201     D  LENGH          S             15  5
003100050127      *
003200050127     D Stato_Elab      s              2
003300041201     D Almeno_Uno      s              2    INZ('  ')
003400050131      *
003500050131     D data_iso        s               d   datfmt(*iso)
003600050131     D data_old        s               d   datfmt(*iso)
003700050131      *
003800050131     D                SDS
003900050131     D  §PGMname               1     10
004000050131     D  §USRname             254    263
004100050131      *---------------------------------------------------------------------*
004200050824     D EDIDSGG       e ds
004300050824      **
004400050131     D PuliSND         DS            30
004500050131     d  DataPulizia                   8  0
004600050131     d  GG_Piu_old                    3  0
004700050824     d**Free_Field                   19
004800980225      *---------------------------------------------------------------------*
004900050131     D Digits          C                   CONST('0123456789')
005000050131      *---------------------------------------------------------------------*
005100980225      * Ciclo principale
005200980225      *---------------------------------------------------------------------*
005300050127     C     *ENTRY        PLIST
005400050127     C                   PARM                    KPJBA
005500050131     C*
005600050131     C* RECUPERO DATA E ORA
005700050131     C                   TIME                    WHHD12           12 0
005800050131     C                   MOVE      WHHD12        DATA6             6 0
005900050131     C                   MOVEL     DATA6         DATA              6 0
006000050131     C                   TIME                    WORA              6 0
006100050127      *
006200050127     c     *like         define    kpjbu         kpjbu_sav
006300050127     c                   move      kpjbu         kpjbu_sav
006400050127     c                   move      kpjbu         ds_kpjbu
006500050127     c                   moveL     kpjbu         Stato_Elab
006600050127      *
006700050127     c     OK_x_LNP      klist
006800050127     c                   kfld                    Stato_OK
006900050127     c                   kfld                    Lnp_OK
007000050127     c                   kfld                    Num_OK
007100050127     C* Lettura file x chiavi
007200050127     c                   move      Lnp_nr        Lnp_OK            3 0
007300050127     c                   move      Num_nr        Num_OK            7 0
007400050127      *   Flussi
007500050127     c                   select
007600050127      *  Ciclo di trasmissione
007700050127     c                   when      Stato_Elab = *blank
007800050131      *      imposta chiave di lettura
007900050127     c                   move      'OK'          Stato_OK          2
008000050127     c                   EXSR      SND_FILE
008100050131      *
008200050131      * Prima di uscire esegue una pulizia dei vecchi records
008300050131      *     + vecchi di xx giorni
008400050131      **              *--------------------------*
008500050131     c                   exsr      Pulizia_old
008600050131      **              *--------------------------*
008700050127      *
008800050127      *  Ciclo di trasmissione errata Errore File
008900050127     c                   when      Stato_Elab = 'EF'
009000050131      *      imposta chiave di lettura
009100050127     c                   move      'RX'          Stato_OK          2
009200050127     c                   EXSR      ERR_FILE
009300050127      *
009400050127     c                   endsl
009500980225      * Fine pgm.
009600040604     c     fine          TAG
009700041201      *
009800041201      *      Imposta se ci sono dei records oppure no
009900041203     c                   move      kpjbu_sav     kpjbu
010000041201     c                   movel     Almeno_Uno    kpjbu
010100040604      *
010200960531     C                   SETON                                        LR
010300041201      *----------------------------------------------------------------
010400050127      *  Ciclo di Trasmissione
010500050127      *----------------------------------------------------------------
010600050127     c     SND_FILE      Begsr
010700050127      *
010800050127      **  Bolle
010900050127     c                   if        Tip_Fil = 'IFCSUM'
011000050127     c                   open      SNDIFCSUM1
011100050127     c                   endIF
011200050127      **  Stati
011300050127     c                   if        Tip_Fil = 'IFTSTA'
011400050127     c                   open      SNDIFTSTA1
011500050127     c                   endIF
011600050127      **  Bolle
011700050127     c                   if        Tip_Fil = 'IFCSUM'
011800050127     C     OK_x_LNP      SETLL     SNDIFCSUM1
011900050127     C     OK_x_LNP      READe     SNDIFCSUM1
012000050127     c                   Dow       not %eof(SNDIFCSUM1)
012100050127     **            **---------------------------------------**
012200050127     **  Scrittura del file EDIFCSUM in QTEMP
012300050901     c                   clear                   wfsnd000
012400050901     c                   movel     SSIFCSUM      flatrec
012500050901     c                   write     wfsnd000
012600050127     c                   eval      Almeno_Uno='OK'
012700050127     **            **---------------------------------------**
012800050127     ** Dopo aver scritto e passato il record del Flat file
012900050127     **  aggiorna il record che ha generato la scrittura
013000050127     **   impostando lo stato di avanzamento del record a RX = Ricevuto
013100050127     c                   move      'RX'          SSelabora
013200050127     c                   update    SNDSUM
013300050127     **
013400050127     C     OK_x_LNP      READe     SNDIFCSUM1
013500050127     c                   end
013600050127     c                   endIF
013700050127      **  Stati
013800050127     c                   if        Tip_Fil = 'IFTSTA'
013900050127     C     OK_x_LNP      SETLL     SNDIFTSTA1
014000050127     C     OK_x_LNP      READe     SNDIFTSTA1
014100050127     c                   Dow       not %eof(SNDIFTSTA1)
014200050127     **            **---------------------------------------**
014300050127     **  Scrittura del file EDIFTSTA in QTEMP
014400050901     c                   clear                   wfsnd000
014500050901     c                   movel     SSIFTSTA      flatrec
014600050901     c                   write     wfsnd000
014700050127     c                   eval      Almeno_Uno='OK'
014800050127     **            **---------------------------------------**
014900050127     ** Dopo aver scritto e passato il record del Flat file
015000050127     **  aggiorna il record che ha generato la scrittura
015100050127     **   impostando lo stato di avanzamento del record a RX = Ricevuto
015200050127     c                   move      'RX'          SSelabora
015300050127     c                   update    SNDSTA
015400050127     **
015500050127     C     OK_x_LNP      READe     SNDIFTSTA1
015600050127     c                   end
015700050127     c                   endIF
015800050127     **
015900050127     c                   endSR
016000050127      *----------------------------------------------------------------
016100050127      *  Ciclo di Errori durante la Trasmissione
016200050127      *----------------------------------------------------------------
016300050127     c     ERR_FILE      Begsr
016400050127      *
016500050127      **  Bolle
016600050127     c                   if        Tip_Fil = 'IFCSUM'
016700050127     c                   open      SNDIFCSUM1
016800050127     C     OK_x_LNP      SETLL     SNDIFCSUM1
016900050127     C     OK_x_LNP      READe     SNDIFCSUM1
017000050127     c                   Dow       not %eof(SNDIFCSUM1)
017100050127     ** Imposta il flag di trasmissione errata
017200050127     c                   move      'EF'          SSelabora
017300050127     c                   update    SNDSUM
017400050127     C     OK_x_LNP      READe     SNDIFCSUM1
017500050127     c                   end
017600050127     c                   endIF
017700050127      **  Stati
017800050127     c                   if        Tip_Fil = 'IFTSTA'
017900050127     c                   open      SNDIFTSTA1
018000050127     C     OK_x_LNP      SETLL     SNDIFTSTA1
018100050127     C     OK_x_LNP      READe     SNDIFTSTA1
018200050127     c                   Dow       not %eof(SNDIFTSTA1)
018300050127     ** Imposta il flag di trasmissione errata
018400050127     c                   move      'EF'          SSelabora
018500050127     c                   update    SNDSTA
018600050127     C     OK_x_LNP      READe     SNDIFTSTA1
018700050127     c                   end
018800050127     c                   endIF
018900050127     **
019000050127     c                   endSR
019100050131      *----------------------------------------------------------------
019200050131     C*? Pulisce i vecchi records.
019300050131     C*----------------------------------------------------------------
019400050131     C     Pulizia_old   BEGSR
019500050131      *
019600050131     c                   move      *date         dataOGGI          8 0
019700050131      *
019800050824      * ?Call x reperire l'ultima data pulizia ed i giorni oltre i quali
019900050824      * ?pulire i records. (x non aggiornare tabella KPJBU deve essere pulita)
020000050824     c                   clear                   kpjbu
020100050824     c                   call      'TRTC96BR'
020200050824     c                   parm                    kpjba
020300050824     c                   movel     kpjbu         edidsGG
020400050824      **
020500050824      * ?se x assurdo la tabella fosse vuota  imposta fisso a 15 gg.
020600050824     c                   IF        §ggGGS = '000' or §ggGGS = *blank
020700060215     c                   z-add     30            GG_Piu_old
020800050824     c                   z-add     dataOGGI      dataPulizia
020900050824     c                   else
021000050824      * ?i campi su DS sono Alfa
021100050824     c                   move      §ggDTS        dataPulizia
021200050824     c                   move      §ggGGS        GG_Piu_old
021300050824     c                   end
021400050824      * ?     /* ---------------------------------------------- */
021500050131      *  solo se la data presente sull'area Dati
021600050131     C                   IF        dataOGGI <> DataPulizia
021700050131      *
021800050131     c                   move      *date         data_iso
021900050131     c     data_iso      subdur    GG_Piu_old:*d data_old
022000050131     c                   move      data_old      data08            8 0
022100050131      *
022200050131     C/EXEC SQL
022300050131     C+ DELETE   FROM SNDIFTSTA  where
022400050131     C+ SSDATELA <  :DATA08      and
022500050131     C+ SSELABORA IN ('RX')
022600050131     C/END-EXEC
022700050131      *
022800050131     C/EXEC SQL
022900050131     C+ DELETE   FROM SNDIFCSUM  where
023000050131     C+ SSDATELA <  :DATA08      and
023100050131     C+ SSELABORA IN ('RX')
023200050131     C/END-EXEC
023300050831      *  quindi aggiorna con la data di oggi
023400050131      *   x non rieseguire in seguito ancora la pulizia che deve essere giornaliera
023500050131      *
023600050824      * ?Call x aggiornare l'ultima data pulizia
023700050824      * ? (x aggiornare tabella passa in KPJBU la data SND)
023800050824      * ?------------------------------------------------------------------ */
023900050824     c                   clear                   edidsGG
024000050824     c                   move      dataOGGI      §ggDTS
024100050824     c                   clear                   kpjbu
024200050824     c                   movel     edidsGG       kpjbu
024300050824     c                   call      'TRTC96BR'
024400050824     c                   parm                    kpjba
024500050824      *
024600050131     C                   EndIF
024700050131      *
024800050131     C                   Endsr
024900050127      *----------------------------------------------------------------
