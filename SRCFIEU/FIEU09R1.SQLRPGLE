000100140917      **-----------------------------------------------------------------***
000200140917      *  STAMPA il WorkFile
000300140917      **-----------------------------------------------------------------***
000400040820     H DECEDIT('0,') DATEDIT(*dmy.)
000500050510      **********************************************************************
000600151001-->  fFIEU09P   o    E             PRINTER OFLIND(*in66)
000700140916      *
000800171106     FDPcdp12i  IF   E           K DISK
000900151002     FTABEL00F  IF   E           K DISK
001000171106     FDPcve11i  IF   E           K DISK
001100050510      **-----------------------------------------------------------------***
001200040820     d KPJBA         E DS
001300050511     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001400110511     D CNCR42        E DS
001500151002     D DS15          E DS                  INZ
001600151002     D skNaz           S              2    DIM(500)
001700151002     D skNazKey        S              3    DIM(500)
001800151002     D skLad           S              3s 0 DIM(500)
001900151002     D jNaz            S              4  0 INZ
002000151002     D*
002100040820     D*
002200151001-->  D DsPassaggio   e DS                  EXTNAME(FIEU08DS)
002300151001     D FIEU09F       e DS                  EXTNAME(WFIEU09F)
002400040820     D*
002500040820     D WLBDAT          DS
002600040820     D  G02DAT                 1      8  0
002700040820     D  G02INV                 9     16  0
002800040820     D  G02ERR                17     17
002900040820     D  G02TGI                18     22  0
003000040820      *
003100040820     D  data_Oggi      s                   like(G02INV) INZ(0)
003200040826     D  almeno_uno     s              1
003300050510      *
003400050511     D*-------------
003500050511     D* Reperimento nome PGM
003600050511     D STATUS         SDS           333
003700050511     D  DSPGM            *PROC
003800050511     D*-------------
003900040820     D digits          C                   CONST('0123456789')
004000050511      * ?------------------------------------------------------------------ */
004100140917      * ? Stampa quanto elaborato e memoprizzato sul workfile.
004200050511      * ?------------------------------------------------------------------ */
004300050510     C/EXEC SQL
004400050510     C+ Declare A1 Cursor for
004500151001     C+ select * from WFIEU09F
004600151001     C+   order by  F09DEPOT, F09LNP
004700050510     C/END-EXEC
004800050510      *          apertura cursore
004900050510     C/EXEC SQL
005000050510     C+ OPEN A1
005100050510     C/END-EXEC
005200050510     c                   clear                   almeno_uno
005300050510      **
005400050510     C                   DOU       SqlCod <> 0
005500050510      **         lettura cursore
005600050510     C/EXEC SQL
005700151001     C+ FETCH NEXT FROM A1 INTO :FIEU09F
005800050510     C/END-EXEC
005900050510     C                   SELECT
006000050510     **
006100050510     C                   WHEN      SqlCod = 100
006200050510     c* a fine file
006300050510     c                   if        almeno_uno = 'S'
006400140916     c                   exsr      daFare_a_EoF
006500050510     c                   else
006600050510     c                   end
006700050510     c                   leave
006800050510     c*
006900050510     C                   WHEN      SqlCod < 0
007000050510     C                   seton                                        H1
007100050510     c                   goto      fine
007200050510     c*
007300050510     C                   OTHER
007400050510     **
007500050510     c* DETTAGLIO
007600050510     c* riga   dettaglio
007700140916     c                   exsr      ElabRig_det
007800050510     C                   ENDSL
007900050510
008000050510     C                   ENDDO
008100050510     C/EXEC SQL
008200050510     C+ Close A1
008300050510     C/END-EXEC
008400050510     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
008500041007     c     fine          tag
008600020326     C                   SETON                                        LR
008700050511     ?****************************************************************************
008800140916      *?   Elabora Riga File di Work
008900050511     ?****************************************************************************
009000140916     c     ElabRig_det   begsr
009100050510      *
009200050511     c                   move      'S'           almeno_uno
009300140917      *
009400151001     c                   eval      PDEPOT   =  F09DEPOT
009500151001     c                   eval      PLNP     =  F09LNP
009600151001     c                   eval      PnrSPED  =  F09NSP
009700151001     c                   eval      Pperc    =  F09PERC
009800151001     c                   eval      PNAZDPT  =  *blank
009900151007     c                   clear                   pdepagg
010000151007     c                   if        Pperc > 50
010100151007     c                   movel     '->'          pdepagg
010200151007     c                   end
010300151001      *
010400151001      * sul cappario DPD
010500171106     c     DPD_capp      chain     dpcdp12i
010600151001      *
010700151001      * non esiste sul cappario ultimo DPD
010800171106     c                   if        %Found(dpcdp12i)
010900151002     c                   eval      PNAZDPT  =  CDPISO2
011000151002      *
011100151002     C                   IF        CDPISO2 <> *blanks
011200151002     C                   Z-ADD     1             jNaz
011300151002     C     CDPISO2       LOOKUP    skNaz(jNaz)                            55
011400151002     C                   IF        %equal
011500151002     c                   eval      PNAZDPT  =  skNazKey(jNaz)
011600151002     C                   ENDIF
011700151002     C                   ENDIF
011800151001     c                   end
011900151001      *
012000151001      *  MOTIVI segnalazione
012100151001     c                   eval      Pdescriz =  *blank
012200151002     c                   eval      Ptratt   =  *blank
012300151001     c                   if        F09flg = '*'
012400151002     c                   eval      Pdescriz =  'NON c''è nel Cappario DPD -
012500151001     c                             / ult.versione'
012600151001      *
012700151001     c                   elseif    F09flg = 'N'
012800151002     c                   eval      Ptratt   =  '>>>'
012900151002     c                   eval      Pdescriz =  '     :NON trovate spedizioni -
013000171106     c                             dal DEPOT di :' + %trim(cdpCiTY)
013100151001     c                   else
013200171106     c                   eval      Pdescriz =    %trim(cdpCiTY) + '(' +
013300151002     C                             %Trim(PNAZDPT) + ')'
013400151001     c                   end
013500140917      *
013600151001     c                   if        PASNOG ='S' and F09flg = *blank
013700151001     c                   goto      salta
013800151001     c                   end
013900151001      *
014000140917     c                   exsr      PRT_rig_det
014100140916      *
014200151001     c     salta         endsr
014300140916     ?****************************************************************************
014400140916      *?   operazioni Finali dopo aver finito il ciclo di lettura SQL
014500140916     ?****************************************************************************
014600140916     c     daFare_a_EoF  begsr
014700140916      *
014800140917     c                   If         almeno_uno <> 'S'
014900140917     c                   exsr      PRT_NO_Dati
015000140917     c                   end
015100140916      *
015200140916     c                   endsr
015300050511     ?****************************************************************************
015400050511      *?   Stampa      File di Work Una riga x P.O.
015500050511     ?****************************************************************************
015600050511     c     PRT_rig_det   begsr
015700050511      *
015800050511     c                   if        *in66
015900050511      *                 * ------------------ *
016000050511     c                   write     testa
016100050511      *                 * ------------------ *
016200050511     c                   setoff                                       66
016300050511     c                   end
016400050511      *
016500050511      *                 * ------------------ *
016600050511     c                   write     dettagli
016700050511      *                 * ------------------ *
016800050511      *
016900050511     c                   endsr
017000050511     ?****************************************************************************
017100050511      *?   Stampa      Avviso NO DATI x la SELEZIONE
017200050511     ?****************************************************************************
017300050511     c     PRT_NO_Dati   begsr
017400050511      *
017500050511     c                   if        *in66
017600050511      *                 * ------------------ *
017700050511     c                   write     testa
017800050511      *                 * ------------------ *
017900050511     c                   setoff                                       66
018000050511     c                   end
018100050511      *
018200050511      *                 * ------------------ *
018300050511     c                   write     nodati
018400050511      *                 * ------------------ *
018500050511      *
018600050511     c                   endsr
018700050511     ?****************************************************************************
018800050511      *?   INIZIALIZZAZIONE
018900050511     ?****************************************************************************
019000040820     C     *inzsr        BEGSR
019100140916     C*
019200040820     C     *ENTRY        PLIST
019300040820     C                   PARM                    KPJBA
019400050510     c                   movel     kpjbu         DsPassaggio
019500050511      **
019600050511     C                   Z-ADD     1             CODUT
019700050511     C                   CALL      'X§PARUT'
019800050511     C                   PARM                    UT§DSE
019900110511     C                   MOVEL     REC42         CNCR42
020000110511     C                   MOVEL     simRGS        RSUT             20
020100041103      *
020200040820      *  imposta UDATE di elaborazione
020300040820     C                   move      *date         G02DAT
020400040820     C                   MOVE      *ZEROS        G02INV
020500040820     C                   MOVE      *BLANKS       G02ERR
020600040820     C                   CALL      'XSRDA8'
020700040820     C                   PARM                    WLBDAT
020800140917      *
020900040820     C                   Z-ADD     G02INV        data_oggi
021000050511     C                   move      G02dat        day
021100040820     C                   TIME                    W0140            14 0
021200050511     C                   MOVEl     W0140         ore
021300140916      **
021400050510     c                   move      PASdti        dadata            8 0
021500050510     c                   move      PASdtf        adata             8 0
021600040823      **
021700050511     c                   seton                                        66
021800040826     c                   clear                   almeno_uno
021900050511     C                   MOVEL     DSPGM         NOMPGM
022000050511      **
022100050511     C                   move      *zeros        G02DAT
022200050511     C                   MOVE      dadata        G02INV
022300050511     C                   MOVE      '3'           G02ERR
022400050511     C                   CALL      'XSRDA8'
022500050511     C                   PARM                    WLBDAT
022600050511     C                   Z-ADD     G02Dat        PDadata
022700050511      **
022800050511     C                   move      *zeros        G02DAT
022900050511     C                   MOVE      adata         G02INV
023000050511     C                   MOVE      '3'           G02ERR
023100050511     C                   CALL      'XSRDA8'
023200050511     C                   PARM                    WLBDAT
023300050511     C                   Z-ADD     G02dat        PAdata
023400050510     c*
023500151002     C* Carico le tabelle occorrenti
023600151002     C                   EXSR      CARTAB
023700151002     c*
023800151001      * x verificare esistenza sul cappario ultimo DPD
023900151001     c                   clear                   ultVER            5 0
024000171106     C     *hival        setll     dpcve11i
024100171106     C                   readp     dpcve11i
024200171106     c                   if        not %Eof(dpcve11i)
024300171106     c                   z-add     cveVER        ultVER
024400171106     c                   end
024500151001     c*
024600151001     c     DPD_capp      klist
024700171106     c                   KFLD                    FLGATB
024800151001     c                   KFLD                    ULTver
024900151001     c                   KFLD                    F09Depot
025000151001      *
025100171106     c                   clear                   FLGATB            1
025200171106     c*
025300040820     C                   ENDSR
025400140916     ?****************************************************************************
025500151002     C* CARTAB - CARICAMENTO DATI TABELLATI
025600151002     C*------------------------------------------------------------------------*
025700151002     C     cartab        BEGSR
025800151002     C*
025900151002     C* Reperisco tutte le nazioni (ISO2) servite da DPD
026000151002     C                   Z-ADD     *zeros        jNaz
026100151002     C                   CLEAR                   skNaz
026200151002     C                   CLEAR                   skNazKey
026300151002     C                   CLEAR                   DS15
026400151002     C                   EVAL      tblKUT = 1
026500151002     C                   EVAL      tblCOD = '15'
026600151002     C     KEYtabel_P    SETLL     TABEL00F
026700151002     C                   IF        %found(TABEL00F)
026800151002     C     KEYtabel_P    READE     TABEL00F
026900151002     C                   DOW       not %eof(TABEL00F)
027000151002     C                   IF        tblFLG = *blanks
027100151002     C                   EVAL      DS15 = tblUNI
027200151002     C                   IF        §15LAD > *zeros OR
027300151002     C                             §15RED = 'S'
027400151002     C                   ADD       1             jNaz
027500151002     C                   MOVEL(P)  §15COD        skNaz(jNaz)
027600151002     C                   MOVEL(P)  tblKEY        skNazKey(jNaz)
027700151002     C                   MOVEL(P)  §15LAD        skLAD(jNaz)
027800151002     C                   ENDIF
027900151002     C                   ENDIF
028000151002     C     KEYtabel_P    READE     TABEL00F
028100151002     C                   ENDDO
028200151002     C                   ENDIF
028300151002     C*
028400151002     C* TABEL00F - Parziale
028500151002     C     KEYtabel_P    KLIST
028600151002     C                   KFLD                    tblKUT
028700151002     C                   KFLD                    tblCOD
028800151002     C*
028900151002     C                   ENDSR
029000151002     ?****************************************************************************
