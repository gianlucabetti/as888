000100040820     H DECEDIT('0,') DATEDIT(*dmy.)
000200050510      **********************************************************************
000300151001     fwFieu09F  uf a E           K DISK    UsrOPN
000400050511     f                                     prefix(M_)
000500151002     FTABEL00F  IF   E           K DISK
000600171011      *--
000700151001     FDPcdp01L  IF   E           K DISK
000800151002     FDPpdc01L  uF   E           K DISK
000900050510      **-----------------------------------------------------------------***
001000040820     d KPJBA         E DS
001100050511     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001200110511     D CNCR42        E DS
001300151002     D DPDCFLO       E DS
001400151002     D FIEU09        E DS                  EXTNAME(WFIEU09F)
001500151001     D**-----------------------------------------------------------------***
001600151001-->  D DsPassaggio   e DS                  EXTNAME(FIEU08ds)
001700151002     D DS15          E DS                  INZ
001800151002     D skNaz           S              2    DIM(500)
001900151002     D skNazKey        S              3    DIM(500)
002000151002     D skLad           S              3s 0 DIM(500)
002100151002     D jNaz            S              4  0 INZ
002200040820     D*
002300040820     D WLBDAT          DS
002400040820     D  G02DAT                 1      8  0
002500040820     D  G02INV                 9     16  0
002600040820     D  G02ERR                17     17
002700040820     D  G02TGI                18     22  0
002800040820      *
002900040820     D  data_Oggi      s                   like(G02INV) INZ(0)
003000040826     D  almeno_uno     s              1
003100040820      *
003200151001     D  FLD_Nr         s              7  0
003300151001     D  FLD_Depot      s              4a
003400151001     D  FLD_Linea      s              3s 0
003500050510      *
003600151001     D  TOT_NumSped    s              7  0
003700050510      *
003800050511     D*-------------
003900050511     D* Reperimento nome PGM
004000050511     D STATUS         SDS           333
004100050511     D  DSPGM            *PROC
004200050511     D*-------------
004300050511      *
004400040820     D digits          C                   CONST('0123456789')
004500050511      * ?------------------------------------------------------------------ */
004600151001      * ? Controlla da TITAS le Bolle IMPORT FRANCO (F1)
004700151001      * ? da data a data per provenienza DEPOT e conta quante sono DEPOT/LNP
004800050511      * ?------------------------------------------------------------------ */
004900151002     C                   SETON                                        LR
005000050510     C/EXEC SQL
005100151001     C+ Declare TAS10 Cursor for
005200151001     C+ select substr(digits(TASRMN), 2, 4), TASLNP, count(*)
005300151001     C+ from titas10f
005400151001     C+ WHERE  (tasaas * 10000) + tasmgs between :dadata and :adata
005500151001     C+ and tastbl='F1'  and
005600151001     C+ TASLNP in
005700151001     C+ (SELECT orgfil FROM azorg00f WHERE orgde3 like '%DPD%')
005800151001     C+ group by substr(digits(TASRMN), 2, 4), TASLNP
005900151001     C+ order by substr(digits(TASRMN), 2, 4), TASLNP
006000050510     C/END-EXEC
006100050510      *          apertura cursore
006200050510     C/EXEC SQL
006300151001     C+ OPEN TAS10
006400050510     C/END-EXEC
006500151001     c                   clear                   almeno_uno
006600050510     C                   DOU       SqlCod <> 0
006700050510      **         lettura cursore
006800050510     C/EXEC SQL
006900151001     C+ FETCH NEXT FROM TAS10  INTO :FLD_Depot, :FLD_Linea, :FLD_nr
007000050510     C/END-EXEC
007100050510     C                   SELECT
007200050510     **
007300050510     C                   WHEN      SqlCod = 100
007400050510     c* a fine file
007500050510     c                   if        almeno_uno = 'S'
007600050510     c                   else
007700050510     c                   end
007800050510     c                   leave
007900050510     c*
008000050510     C                   WHEN      SqlCod < 0
008100050510     C                   seton                                        H1
008200151002     c                   RETURN
008300050510     c*
008400050510     C                   OTHER
008500050510     c* DETTAGLIO
008600050510     c* riga   dettaglio
008700151001     c                   exsr      Wrt_rig_det
008800151002      *
008900050510     C                   ENDSL
009000151002      *
009100050510     C                   ENDDO
009200050510     C/EXEC SQL
009300151001     C+ Close TAS10
009400050510     C/END-EXEC
009500151001     c*---------------------------------
009600151001     C/EXEC SQL
009700151001     C+ Declare TAS00  Cursor for
009800151001     C+ select substr(digits(TASRMN), 2, 4), TASLNP, count(*)
009900151001     C+ from titas00f
010000151001     C+ WHERE  (tasaas * 10000) + tasmgs between :dadata and :adata
010100151001     C+ and tastbl='F1'  and
010200151001     C+ TASLNP in
010300151001     C+ (SELECT orgfil FROM azorg00f WHERE orgde3 like '%DPD%')
010400151001     C+ group by substr(digits(TASRMN), 2, 4), TASLNP
010500151001     C+ order by substr(digits(TASRMN), 2, 4), TASLNP
010600151001     C/END-EXEC
010700151001      *          apertura cursore
010800151001     C/EXEC SQL
010900151001     C+ OPEN TAS00
011000151001     C/END-EXEC
011100151001     c                   clear                   almeno_uno
011200151001     C                   DOU       SqlCod <> 0
011300151001      **         lettura cursore
011400151001     C/EXEC SQL
011500151001     C+ FETCH NEXT FROM TAS00 INTO :FLD_Depot, :FLD_Linea, :FLD_nr
011600151001     C/END-EXEC
011700151001     C                   SELECT
011800151001     **
011900151001     C                   WHEN      SqlCod = 100
012000151001     c* a fine file
012100151001     c                   if        almeno_uno = 'S'
012200151001     c                   else
012300151001     c                   end
012400151001     c                   leave
012500151001     c*
012600151001     C                   WHEN      SqlCod < 0
012700151002     C                   seton                                        H2
012800151002     c                   RETURN
012900151001     c*
013000151001     C                   OTHER
013100151001     c* DETTAGLIO
013200151001     c* riga   dettaglio
013300151001     c                   exsr      WRT_rig_det
013400151001     c*
013500151001     C                   ENDSL
013600151001
013700151001     C                   ENDDO
013800151001     C/EXEC SQL
013900151001     C+ Close TAS00
014000151001     C/END-EXEC
014100151001     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
014200050510     c*  Chiude il File per eseguire SQL finale di totalizzazione
014300151001     c                   Close     WFIEU09F
014400050510     c* Totali
014500050510     c                   exsr      totalizza
014600050510     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
014700151001     c*
014800041007     c     fine          tag
014900050511     ?****************************************************************************
015000151001      *?  Scrive   Riga File di Work
015100151001     ?****************************************************************************
015200151001     c     WRT_rig_det   begsr
015300151001      *
015400151001     c                   move      'S'           almeno_uno
015500151001     c     k009          chain     wfieu09f
015600151001     c                   if        %Found(wfieu09f)
015700151001     c                   add       FLD_Nr        M_F09NSP
015800151001     c                   update    Fieu009
015900151001     c                   else
016000151001      *
016100151001     c                   clear                   Fieu009
016200151001     c                   movel     FLD_Depot     M_F09DEPOT
016300151001     c                   z-add     FLD_Linea     M_F09LNP
016400151001     c                   z-add     FLD_Nr        M_F09NSP
016500151001      *
016600151001     c                   write     Fieu009
016700151001     c                   end
016800151001      *
016900151001     c                   endsr
017000050511     ?****************************************************************************
017100151001      *?   Totali da  File di Work  calcola la percentuale
017200050511     ?****************************************************************************
017300050510     c     Totalizza     begsr
017400050510      *
017500151001     c                   Open      WFieu09F
017600050511      * ?------------------------------------------------------------------ */
017700151001      **  ricava la percentuale per ogni singolo DEPOT / LINEA
017800050511      * ?------------------------------------------------------------------ */
017900050510     C/EXEC SQL
018000151001     C+ Declare TOT Cursor for
018100151001     C+  SELECT F09DEPOT, Sum(F09NSP)
018200151001     C+  FROM WFIEU09F
018300151001     C+  GROUP BY F09DEPOT
018400151001     C+  ORDER BY F09DEPOT
018500050510     C/END-EXEC
018600050510      *          apertura cursore
018700050510     C/EXEC SQL
018800151001     C+ OPEN TOT
018900050510     C/END-EXEC
019000050510     c                   clear                   almeno_uno
019100050510      **
019200050510     C                   DOU       SqlCod <> 0
019300050510      **         lettura cursore
019400050510     C/EXEC SQL
019500151001     C+ Fetch Next From TOT Into  :FLD_Depot, :TOT_NumSped
019600050510     C/END-EXEC
019700050510     C                   SELECT
019800050510     **
019900050510     C                   WHEN      SqlCod = 100
020000050510     c* a fine file
020100050510     c                   if        almeno_uno = 'S'
020200050510     c                   else
020300050510     c                   end
020400050510     c                   leave
020500050510     c*
020600050510     C                   WHEN      SqlCod < 0
020700151002     C                   seton                                        H3
020800151002     c                   RETURN
020900050510     c*
021000050510     C                   OTHER
021100050510     c* DETTAGLIO
021200050510     c* riga   dettaglio
021300050510     c                   exsr      Agg_rig_det
021400050510     c*
021500050510     C                   ENDSL
021600050510
021700050510     C                   ENDDO
021800050510     C/EXEC SQL
021900151001     C+ Close TOT
022000050510     C/END-EXEC
022100050510     c
022200050510     c     fine_SR       tag
022300151001     c                   Close     WFieu09F
022400151001     c
022500151001     c                   if        almeno_uno = 'S'
022600151002     c                   exsr      daFare_a_LR
022700151001     c                   else
022800151001     c                   end
022900050510      *
023000050510     c                   endsr
023100151001     ?****************************************************************************
023200151001      *?   Aggiorna    File di Work
023300151001     ?****************************************************************************
023400151001     c     Agg_rig_det   begsr
023500151001      *
023600151001     c                   move      'S'           almeno_uno
023700151001      *
023800151001      * x verificare esistenza sul cappario ultimo DPD
023900151001     c                   clear                   ultVER            5 0
024000151001     C     *hival        setll     dpcdp01l
024100151001     C                   readp     dpcdp01l
024200151001     c                   if        not %Eof(dpcdp01l)
024300151001     c                   z-add     CDPVER        ultVER
024400151001     c                   end
024500151001      *
024600151001     C     FLD_Depot     setll     WFieu09F
024700151001     C     FLD_Depot     reade     WFieu09F
024800151001      *
024900151001     c                   dow       not %Eof(WFieu09F)
025000151001      *
025100151001     c                   eval      M_F09PERC = M_F09NSP * 100 / TOT_NumSped
025200151001      * sul cappario DPD
025300151001     c     DPD_capp      setll     dpcdp01l
025400151001      *
025500151001      * non esiste sul cappario ultimo DPD
025600151001     c                   if        not %Equal(dpcdp01l)
025700151001     c                   move      '*'           M_F09flg
025800151001     c                   end
025900151001      *
026000151001     c                   update    Fieu009
026100151001      *
026200151001     C     FLD_Depot     reade     wFieu09F
026300151001     c                   enddo
026400151001      * -------
026500151001     C     ultVER        setll     dpcdp01l
026600151001     C     ultVER        reade     dpcdp01l
026700151001     c                   Dow       not %EoF(dpcdp01l)
026800151001      * sul cappario DPD
026900151001     c     CDPDPC        setll     wfieu09f
027000151001      *
027100151001      * non esiste fra i parcel ricevuti
027200151001     c                   if        not %Equal(wfieu09f)
027300151001      *
027400151001     c                   clear                   Fieu009
027500151001     c                   movel     CDPDPC        M_F09DEPOT
027600151002     c                   z-add     190           M_F09LNP
027700151002      *
027800151002     C                   IF        CDPISO2 <> *blanks
027900151002     C                   Z-ADD     1             jNaz
028000151002     C     CDPISO2       LOOKUP    skNaz(jNaz)                            55
028100151002     C                   IF        %equal
028200151002     c                   z-add     skLAD(Jnaz)   M_F09LNP
028300151002     C                   ENDIF
028400151002     C                   ENDIF
028500151001     c                   move      'N'           M_F09flg
028600151001      *
028700151001     c                   write     Fieu009
028800151001     c                   end
028900151001      *
029000151001     C     ultVER        reade     dpcdp01l
029100151001     c                   endDo
029200151001      * -------
029300151001     c                   endsr
029400151002     ?****************************************************************************
029500151002      *?   operazioni Finali dopo aver finito il ciclo di lettura SQL
029600151002     ?****************************************************************************
029700151002     c     daFare_a_LR   begsr
029800151002      *
029900151001      *  se richiesto richiamare il programma di stampa
030000151001     c                   if        PASLST = 'S'
030100151001     c                   call      'FIEU09R1'
030200151001     c                   parm                    KPJBA
030300151001     c                   end
030400151002     c* Aggiorna
030500151002     c                   if        PASUPD = 'S'
030600151002     c                   exsr      Aggiorna
030700151002     c                   end
030800151001      *
030900151001     c                   endsr
031000151002     ?****************************************************************************
031100151002      *?   Aggiorna la Linea sui legami se > 50%
031200151002     ?****************************************************************************
031300151002     c     Aggiorna      begsr
031400151002      * -------
031500151002     c                   Open      WFieu09F
031600151002      * ?------------------------------------------------------------------ */
031700151002      **  prende con la percentuale > 50% --> solo i records Blk
031800151002      * ?------------------------------------------------------------------ */
031900151002     C/EXEC SQL
032000151002     C+ Declare PRC Cursor for
032100151013     C+  select f09DEPOT, F09LNP, F09PERC  from wfieu09f where
032200151013     C+  f09DEPOT concat digits(F09PERC) in (
032300151013     C+  SELECT f09DEPOT concat digits(dec(max( F09PERC ), 5, 1))
032400151013     c+  FROM WFIEU09F WHERE f09FLG <>'*' GROUP BY F09DEPOT)
032500151013     c+  ORDER BY F09DEPOT
032600151002     C/END-EXEC
032700151002      *          apertura cursore
032800151002     C/EXEC SQL
032900151002     C+ OPEN PRC
033000151002     C/END-EXEC
033100151002     c                   clear                   almeno_uno
033200151002      **
033300151002     C                   DOU       SqlCod <> 0
033400151002      **         lettura cursore
033500151002     C/EXEC SQL
033600151013     C+ Fetch Next From PRC Into  :f09DEPOT, :F09LNP, :F09PERC
033700151002     C/END-EXEC
033800151002     C                   SELECT
033900151002     **
034000151002     C                   WHEN      SqlCod = 100
034100151002     c* a fine file
034200151002     c                   if        almeno_uno = 'S'
034300151002     c                   else
034400151002     c                   end
034500151002     c                   leave
034600151002     c*
034700151002     C                   WHEN      SqlCod < 0
034800151002     C                   seton                                        H4
034900151002     c                   RETURN
035000151002     c*
035100151002     C                   OTHER
035200151002     c* DETTAGLIO   riga   dettaglio
035300151002     c                   exsr      Agg_dep_LNP
035400151002     c*
035500151002     C                   ENDSL
035600151002
035700151002     C                   ENDDO
035800151002     C/EXEC SQL
035900151002     C+ Close PRC
036000151002     C/END-EXEC
036100151002     c     esce_LNP      tag
036200151002     c                   Close     WFieu09F
036300151002      * -------
036400151002     c                   endsr
036500151002     ?****************************************************************************
036600151002      *?   Aggiorna file LEGAMI con la Linea
036700151002     ?****************************************************************************
036800151002     C     Agg_dep_LNP   BEGSR
036900151002      **
037000151002     C     F09DEPOT      setll     dppdc01l
037100151002     C     F09DEPOT      reade     dppdc01l
037200151002     c                   DOw       not %EoF(dppdc01l)
037300151002      **
037400151002     c                   if        PDCDDE <= data_oggi and data_oggi <= PDCDSC
037500151002     c                   movel     pdcflo        dpdcflo
037600151016      **  chiodo per Linee di Backup
037700151016     c                   if        F09lnp = 192 or F09lnp = 291
037800151016     c                   eval      F09Lnp   = 190
037900151016     c                   end
038000151002     c                   movel     F09Lnp        §PDCFLOLNP
038100151002     c                   movel     dpdcflo       pdcflo
038200151002     c                   update    dppdc000
038300151002     c                   leave
038400151002     c                   end
038500151002      **
038600151002     C     F09DEPOT      reade     dppdc01l
038700151002     c                   endDO
038800151002      **
038900151002     c                   endsr
039000151002     ?****************************************************************************
039100151002      *?   INIZIALIZZAZIONE
039200151002     ?****************************************************************************
039300151002     C     *inzsr        BEGSR
039400151002      **
039500040820     C     *ENTRY        PLIST
039600040820     C                   PARM                    KPJBA
039700050510     c                   movel     kpjbu         DsPassaggio
039800050511      **
039900050511     C                   Z-ADD     1             CODUT
040000050511     C                   CALL      'X§PARUT'
040100050511     C                   PARM                    UT§DSE
040200110511     C                   MOVEL     REC42         CNCR42
040300110511     C                   MOVEL     simRGS        RSUT             20
040400041103      *
040500040820      *  imposta UDATE di elaborazione
040600040820     C                   move      *date         G02DAT
040700040820     C                   MOVE      *ZEROS        G02INV
040800040820     C                   MOVE      *BLANKS       G02ERR
040900040820     C                   CALL      'XSRDA8'
041000040820     C                   PARM                    WLBDAT
041100040820     C                   Z-ADD     G02INV        data_oggi
041200151001     C                   move      G02dat        day               8 0
041300040820     C                   TIME                    W0140            14 0
041400151001     C                   MOVEl     W0140         ore               6 0
041500040823      **
041600050510     c                   move      PASdti        dadata            8 0
041700050510     c                   move      PASdtf        adata             8 0
041800040823      **
041900050511     c                   seton                                        66
042000040825     c                   clear                   prima_volta       1
042100040826     c                   clear                   almeno_uno
042200151001     C                   MOVEL     DSPGM         NOMPGM           10
042300050511      **
042400050511     C                   move      *zeros        G02DAT
042500050511     C                   MOVE      dadata        G02INV
042600050511     C                   MOVE      '3'           G02ERR
042700050511     C                   CALL      'XSRDA8'
042800050511     C                   PARM                    WLBDAT
042900151001     C                   Z-ADD     G02Dat        PDadata           8 0
043000050511      **
043100050511     C                   move      *zeros        G02DAT
043200050511     C                   MOVE      adata         G02INV
043300050511     C                   MOVE      '3'           G02ERR
043400050511     C                   CALL      'XSRDA8'
043500050511     C                   PARM                    WLBDAT
043600151001     C                   Z-ADD     G02dat        PAdata            8 0
043700151002     C*
043800151002     C* Carico le tabelle occorrenti
043900151002     C                   EXSR      CARTAB
044000040820     c*
044100151001     c                   Open      Wfieu09F
044200050510     c*
044300151001     c     K009          klist
044400151001     c                   KFLD                    FLD_Depot
044500151001     c                   KFLD                    FLD_Linea
044600151001     c*
044700151001     c*
044800151001     c     DPD_capp      klist
044900151001     c                   KFLD                    ULTver
045000151001     c                   KFLD                    FLD_Depot
045100151001     c*
045200040820     C                   ENDSR
045300151002     C*------------------------------------------------------------------------*
045400151002     C* CARTAB - CARICAMENTO DATI TABELLATI
045500151002     C*------------------------------------------------------------------------*
045600151002     C     cartab        BEGSR
045700151002     C*
045800151002     C* Reperisco tutte le nazioni (ISO2) servite da DPD
045900151002     C                   Z-ADD     *zeros        jNaz
046000151002     C                   CLEAR                   skNaz
046100151002     C                   CLEAR                   skNazKey
046200151002     C                   CLEAR                   DS15
046300151002     C                   EVAL      tblKUT = 1
046400151002     C                   EVAL      tblCOD = '15'
046500151002     C     KEYtabel_P    SETLL     TABEL00F
046600151002     C                   IF        %found(TABEL00F)
046700151002     C     KEYtabel_P    READE     TABEL00F
046800151002     C                   DOW       not %eof(TABEL00F)
046900151002     C                   IF        tblFLG = *blanks
047000151002     C                   EVAL      DS15 = tblUNI
047100151002     C                   IF        §15LAD > *zeros OR
047200151002     C                             §15RED = 'S'
047300151002     C                   ADD       1             jNaz
047400151002     C                   MOVEL(P)  §15COD        skNaz(jNaz)
047500151002     C                   MOVEL(P)  tblKEY        skNazKey(jNaz)
047600151002     C                   MOVEL(P)  §15LAD        skLAD(jNaz)
047700151002     C                   ENDIF
047800151002     C                   ENDIF
047900151002     C     KEYtabel_P    READE     TABEL00F
048000151002     C                   ENDDO
048100151002     C                   ENDIF
048200151002     C*
048300151002     C* TABEL00F - Parziale
048400151002     C     KEYtabel_P    KLIST
048500151002     C                   KFLD                    tblKUT
048600151002     C                   KFLD                    tblCOD
048700151002     C*
048800151002     C                   ENDSR
048900050511     ?****************************************************************************
