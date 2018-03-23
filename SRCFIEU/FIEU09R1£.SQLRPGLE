000100140917      **-----------------------------------------------------------------***
000200140917      *  STAMPA il WorkFile
000300140917      **-----------------------------------------------------------------***
000400040820     H DECEDIT('0,') DATEDIT(*dmy.)
000500050510      **********************************************************************
000600151001-->  fFIEU09P   o    E             PRINTER OFLIND(*in66)
000700140916      *
000800151001     FDPcdp01L  IF   E           K DISK
000900151002     FTABEL00F  IF   E           K DISK
001000050510      **-----------------------------------------------------------------***
001100040820     d KPJBA         E DS
001200050511     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001300110511     D CNCR42        E DS
001400151002     D DS15          E DS                  INZ
001500151002     D skNaz           S              2    DIM(500)
001600151002     D skNazKey        S              3    DIM(500)
001700151002     D skLad           S              3s 0 DIM(500)
001800151002     D jNaz            S              4  0 INZ
001900151002     D*
002000040820     D*
002100151001-->  D DsPassaggio   e DS                  EXTNAME(FIEU08DS)
002200151001     D FIEU09F       e DS                  EXTNAME(WFIEU09F)
002300040820     D*
002400040820     D WLBDAT          DS
002500040820     D  G02DAT                 1      8  0
002600040820     D  G02INV                 9     16  0
002700040820     D  G02ERR                17     17
002800040820     D  G02TGI                18     22  0
002900040820      *
003000040820     D  data_Oggi      s                   like(G02INV) INZ(0)
003100040826     D  almeno_uno     s              1
003200050510      *
003300050511     D*-------------
003400050511     D* Reperimento nome PGM
003500050511     D STATUS         SDS           333
003600050511     D  DSPGM            *PROC
003700050511     D*-------------
003800040820     D digits          C                   CONST('0123456789')
003900050511      * ?------------------------------------------------------------------ */
004000140917      * ? Stampa quanto elaborato e memoprizzato sul workfile.
004100050511      * ?------------------------------------------------------------------ */
004200050510     C/EXEC SQL
004300050510     C+ Declare A1 Cursor for
004400151001     C+ select * from WFIEU09F
004500151001     C+   order by  F09DEPOT, F09LNP
004600050510     C/END-EXEC
004700050510      *          apertura cursore
004800050510     C/EXEC SQL
004900050510     C+ OPEN A1
005000050510     C/END-EXEC
005100050510     c                   clear                   almeno_uno
005200050510      **
005300050510     C                   DOU       SqlCod <> 0
005400050510      **         lettura cursore
005500050510     C/EXEC SQL
005600151001     C+ FETCH NEXT FROM A1 INTO :FIEU09F
005700050510     C/END-EXEC
005800050510     C                   SELECT
005900050510     **
006000050510     C                   WHEN      SqlCod = 100
006100050510     c* a fine file
006200050510     c                   if        almeno_uno = 'S'
006300140916     c                   exsr      daFare_a_EoF
006400050510     c                   else
006500050510     c                   end
006600050510     c                   leave
006700050510     c*
006800050510     C                   WHEN      SqlCod < 0
006900050510     C                   seton                                        H1
007000050510     c                   goto      fine
007100050510     c*
007200050510     C                   OTHER
007300050510     **
007400050510     c* DETTAGLIO
007500050510     c* riga   dettaglio
007600140916     c                   exsr      ElabRig_det
007700050510     C                   ENDSL
007800050510
007900050510     C                   ENDDO
008000050510     C/EXEC SQL
008100050510     C+ Close A1
008200050510     C/END-EXEC
008300050510     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
008400041007     c     fine          tag
008500020326     C                   SETON                                        LR
008600050511     ?****************************************************************************
008700140916      *?   Elabora Riga File di Work
008800050511     ?****************************************************************************
008900140916     c     ElabRig_det   begsr
009000050510      *
009100050511     c                   move      'S'           almeno_uno
009200140917      *
009300151001     c                   eval      PDEPOT   =  F09DEPOT
009400151001     c                   eval      PLNP     =  F09LNP
009500151001     c                   eval      PnrSPED  =  F09NSP
009600151001     c                   eval      Pperc    =  F09PERC
009700151001     c                   eval      PNAZDPT  =  *blank
009800151007     c                   clear                   pdepagg
009900151007     c                   if        Pperc > 50
010000151007     c                   movel     '->'          pdepagg
010100151007     c                   end
010200151001      *
010300151001      * sul cappario DPD
010400151001     c     DPD_capp      chain     dpcdp01l
010500151001      *
010600151001      * non esiste sul cappario ultimo DPD
010700151001     c                   if        %Found(dpcdp01l)
010800151002     c                   eval      PNAZDPT  =  CDPISO2
010900151002      *
011000151002     C                   IF        CDPISO2 <> *blanks
011100151002     C                   Z-ADD     1             jNaz
011200151002     C     CDPISO2       LOOKUP    skNaz(jNaz)                            55
011300151002     C                   IF        %equal
011400151002     c                   eval      PNAZDPT  =  skNazKey(jNaz)
011500151002     C                   ENDIF
011600151002     C                   ENDIF
011700151001     c                   end
011800151001      *
011900151001      *  MOTIVI segnalazione
012000151001     c                   eval      Pdescriz =  *blank
012100151002     c                   eval      Ptratt   =  *blank
012200151001     c                   if        F09flg = '*'
012300151002     c                   eval      Pdescriz =  'NON c''è nel Cappario DPD -
012400151001     c                             / ult.versione'
012500151001      *
012600151001     c                   elseif    F09flg = 'N'
012700151002     c                   eval      Ptratt   =  '>>>'
012800151002     c                   eval      Pdescriz =  '     :NON trovate spedizioni -
012900151002     c                             dal DEPOT di :' + %trim(cdpCTY)
013000151001     c                   else
013100151001     c                   eval      Pdescriz =    %trim(cdpCTY) + '(' +
013200151002     C                             %Trim(PNAZDPT) + ')'
013300151001     c                   end
013400140917      *
013500151001     c                   if        PASNOG ='S' and F09flg = *blank
013600151001     c                   goto      salta
013700151001     c                   end
013800151001      *
013900140917     c                   exsr      PRT_rig_det
014000140916      *
014100151001     c     salta         endsr
014200140916     ?****************************************************************************
014300140916      *?   operazioni Finali dopo aver finito il ciclo di lettura SQL
014400140916     ?****************************************************************************
014500140916     c     daFare_a_EoF  begsr
014600140916      *
014700140917     c                   If         almeno_uno <> 'S'
014800140917     c                   exsr      PRT_NO_Dati
014900140917     c                   end
015000140916      *
015100140916     c                   endsr
015200050511     ?****************************************************************************
015300050511      *?   Stampa      File di Work Una riga x P.O.
015400050511     ?****************************************************************************
015500050511     c     PRT_rig_det   begsr
015600050511      *
015700050511     c                   if        *in66
015800050511      *                 * ------------------ *
015900050511     c                   write     testa
016000050511      *                 * ------------------ *
016100050511     c                   setoff                                       66
016200050511     c                   end
016300050511      *
016400050511      *                 * ------------------ *
016500050511     c                   write     dettagli
016600050511      *                 * ------------------ *
016700050511      *
016800050511     c                   endsr
016900050511     ?****************************************************************************
017000050511      *?   Stampa      Avviso NO DATI x la SELEZIONE
017100050511     ?****************************************************************************
017200050511     c     PRT_NO_Dati   begsr
017300050511      *
017400050511     c                   if        *in66
017500050511      *                 * ------------------ *
017600050511     c                   write     testa
017700050511      *                 * ------------------ *
017800050511     c                   setoff                                       66
017900050511     c                   end
018000050511      *
018100050511      *                 * ------------------ *
018200050511     c                   write     nodati
018300050511      *                 * ------------------ *
018400050511      *
018500050511     c                   endsr
018600050511     ?****************************************************************************
018700050511      *?   INIZIALIZZAZIONE
018800050511     ?****************************************************************************
018900040820     C     *inzsr        BEGSR
019000140916     C*
019100040820     C     *ENTRY        PLIST
019200040820     C                   PARM                    KPJBA
019300050510     c                   movel     kpjbu         DsPassaggio
019400050511      **
019500050511     C                   Z-ADD     1             CODUT
019600050511     C                   CALL      'X§PARUT'
019700050511     C                   PARM                    UT§DSE
019800110511     C                   MOVEL     REC42         CNCR42
019900110511     C                   MOVEL     simRGS        RSUT             20
020000041103      *
020100040820      *  imposta UDATE di elaborazione
020200040820     C                   move      *date         G02DAT
020300040820     C                   MOVE      *ZEROS        G02INV
020400040820     C                   MOVE      *BLANKS       G02ERR
020500040820     C                   CALL      'XSRDA8'
020600040820     C                   PARM                    WLBDAT
020700140917      *
020800040820     C                   Z-ADD     G02INV        data_oggi
020900050511     C                   move      G02dat        day
021000040820     C                   TIME                    W0140            14 0
021100050511     C                   MOVEl     W0140         ore
021200140916      **
021300050510     c                   move      PASdti        dadata            8 0
021400050510     c                   move      PASdtf        adata             8 0
021500040823      **
021600050511     c                   seton                                        66
021700040826     c                   clear                   almeno_uno
021800050511     C                   MOVEL     DSPGM         NOMPGM
021900050511      **
022000050511     C                   move      *zeros        G02DAT
022100050511     C                   MOVE      dadata        G02INV
022200050511     C                   MOVE      '3'           G02ERR
022300050511     C                   CALL      'XSRDA8'
022400050511     C                   PARM                    WLBDAT
022500050511     C                   Z-ADD     G02Dat        PDadata
022600050511      **
022700050511     C                   move      *zeros        G02DAT
022800050511     C                   MOVE      adata         G02INV
022900050511     C                   MOVE      '3'           G02ERR
023000050511     C                   CALL      'XSRDA8'
023100050511     C                   PARM                    WLBDAT
023200050511     C                   Z-ADD     G02dat        PAdata
023300050510     c*
023400151002     C* Carico le tabelle occorrenti
023500151002     C                   EXSR      CARTAB
023600151002     c*
023700151001      * x verificare esistenza sul cappario ultimo DPD
023800151001     c                   clear                   ultVER            5 0
023900151001     C     *hival        setll     dpcdp01l
024000151001     C                   readp     dpcdp01l
024100151001     c                   if        not %Eof(dpcdp01l)
024200151001     c                   z-add     CDPVER        ultVER
024300151001     c                   end
024400151001     c*
024500151001     c     DPD_capp      klist
024600151001     c                   KFLD                    ULTver
024700151001     c                   KFLD                    F09Depot
024800151001      *
024900040820     C                   ENDSR
025000140916     ?****************************************************************************
025100151002     C* CARTAB - CARICAMENTO DATI TABELLATI
025200151002     C*------------------------------------------------------------------------*
025300151002     C     cartab        BEGSR
025400151002     C*
025500151002     C* Reperisco tutte le nazioni (ISO2) servite da DPD
025600151002     C                   Z-ADD     *zeros        jNaz
025700151002     C                   CLEAR                   skNaz
025800151002     C                   CLEAR                   skNazKey
025900151002     C                   CLEAR                   DS15
026000151002     C                   EVAL      tblKUT = 1
026100151002     C                   EVAL      tblCOD = '15'
026200151002     C     KEYtabel_P    SETLL     TABEL00F
026300151002     C                   IF        %found(TABEL00F)
026400151002     C     KEYtabel_P    READE     TABEL00F
026500151002     C                   DOW       not %eof(TABEL00F)
026600151002     C                   IF        tblFLG = *blanks
026700151002     C                   EVAL      DS15 = tblUNI
026800151002     C                   IF        §15LAD > *zeros OR
026900151002     C                             §15RED = 'S'
027000151002     C                   ADD       1             jNaz
027100151002     C                   MOVEL(P)  §15COD        skNaz(jNaz)
027200151002     C                   MOVEL(P)  tblKEY        skNazKey(jNaz)
027300151002     C                   MOVEL(P)  §15LAD        skLAD(jNaz)
027400151002     C                   ENDIF
027500151002     C                   ENDIF
027600151002     C     KEYtabel_P    READE     TABEL00F
027700151002     C                   ENDDO
027800151002     C                   ENDIF
027900151002     C*
028000151002     C* TABEL00F - Parziale
028100151002     C     KEYtabel_P    KLIST
028200151002     C                   KFLD                    tblKUT
028300151002     C                   KFLD                    tblCOD
028400151002     C*
028500151002     C                   ENDSR
028600151002     ?****************************************************************************
