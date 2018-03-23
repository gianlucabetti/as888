000100040820     H DECEDIT('0,') DATEDIT(*dmy.)
000200050510      **********************************************************************
000300151001     fwFieu09F  uf a E           K DISK    UsrOPN
000400050511     f                                     prefix(M_)
000500151002     FTABEL00F  IF   E           K DISK
000600171011      *--
000700171106     FDPcdp12i  IF   E           K DISK
000800171106     FDPcve11i  IF   E           K DISK
000900171106     FDPleg14L  uF   E           K DISK
001000050510      **-----------------------------------------------------------------***
001100040820     d KPJBA         E DS
001200050511     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001300110511     D CNCR42        E DS
001400171106     D DlegFLO       E DS
001500151002     D FIEU09        E DS                  EXTNAME(WFIEU09F)
001600151001     D**-----------------------------------------------------------------***
001700151001-->  D DsPassaggio   e DS                  EXTNAME(FIEU08ds)
001800151002     D DS15          E DS                  INZ
001900151002     D skNaz           S              2    DIM(500)
002000151002     D skNazKey        S              3    DIM(500)
002100151002     D skLad           S              3s 0 DIM(500)
002200151002     D jNaz            S              4  0 INZ
002300040820     D*
002400040820     D WLBDAT          DS
002500040820     D  G02DAT                 1      8  0
002600040820     D  G02INV                 9     16  0
002700040820     D  G02ERR                17     17
002800040820     D  G02TGI                18     22  0
002900040820      *
003000040820     D  data_Oggi      s                   like(G02INV) INZ(0)
003100040826     D  almeno_uno     s              1
003200040820      *
003300151001     D  FLD_Nr         s              7  0
003400151001     D  FLD_Depot      s              4a
003500151001     D  FLD_Linea      s              3s 0
003600050510      *
003700151001     D  TOT_NumSped    s              7  0
003800050510      *
003900050511     D*-------------
004000050511     D* Reperimento nome PGM
004100050511     D STATUS         SDS           333
004200050511     D  DSPGM            *PROC
004300050511     D*-------------
004400050511      *
004500040820     D digits          C                   CONST('0123456789')
004600050511      * ?------------------------------------------------------------------ */
004700151001      * ? Controlla da TITAS le Bolle IMPORT FRANCO (F1)
004800151001      * ? da data a data per provenienza DEPOT e conta quante sono DEPOT/LNP
004900050511      * ?------------------------------------------------------------------ */
005000151002     C                   SETON                                        LR
005100050510     C/EXEC SQL
005200151001     C+ Declare TAS10 Cursor for
005300151001     C+ select substr(digits(TASRMN), 2, 4), TASLNP, count(*)
005400151001     C+ from titas10f
005500151001     C+ WHERE  (tasaas * 10000) + tasmgs between :dadata and :adata
005600151001     C+ and tastbl='F1'  and
005700151001     C+ TASLNP in
005800151001     C+ (SELECT orgfil FROM azorg00f WHERE orgde3 like '%DPD%')
005900151001     C+ group by substr(digits(TASRMN), 2, 4), TASLNP
006000151001     C+ order by substr(digits(TASRMN), 2, 4), TASLNP
006100050510     C/END-EXEC
006200050510      *          apertura cursore
006300050510     C/EXEC SQL
006400151001     C+ OPEN TAS10
006500050510     C/END-EXEC
006600151001     c                   clear                   almeno_uno
006700050510     C                   DOU       SqlCod <> 0
006800050510      **         lettura cursore
006900050510     C/EXEC SQL
007000151001     C+ FETCH NEXT FROM TAS10  INTO :FLD_Depot, :FLD_Linea, :FLD_nr
007100050510     C/END-EXEC
007200050510     C                   SELECT
007300050510     **
007400050510     C                   WHEN      SqlCod = 100
007500050510     c* a fine file
007600050510     c                   if        almeno_uno = 'S'
007700050510     c                   else
007800050510     c                   end
007900050510     c                   leave
008000050510     c*
008100050510     C                   WHEN      SqlCod < 0
008200050510     C                   seton                                        H1
008300151002     c                   RETURN
008400050510     c*
008500050510     C                   OTHER
008600050510     c* DETTAGLIO
008700050510     c* riga   dettaglio
008800151001     c                   exsr      Wrt_rig_det
008900151002      *
009000050510     C                   ENDSL
009100151002      *
009200050510     C                   ENDDO
009300050510     C/EXEC SQL
009400151001     C+ Close TAS10
009500050510     C/END-EXEC
009600151001     c*---------------------------------
009700151001     C/EXEC SQL
009800151001     C+ Declare TAS00  Cursor for
009900151001     C+ select substr(digits(TASRMN), 2, 4), TASLNP, count(*)
010000151001     C+ from titas00f
010100151001     C+ WHERE  (tasaas * 10000) + tasmgs between :dadata and :adata
010200151001     C+ and tastbl='F1'  and
010300151001     C+ TASLNP in
010400151001     C+ (SELECT orgfil FROM azorg00f WHERE orgde3 like '%DPD%')
010500151001     C+ group by substr(digits(TASRMN), 2, 4), TASLNP
010600151001     C+ order by substr(digits(TASRMN), 2, 4), TASLNP
010700151001     C/END-EXEC
010800151001      *          apertura cursore
010900151001     C/EXEC SQL
011000151001     C+ OPEN TAS00
011100151001     C/END-EXEC
011200151001     c                   clear                   almeno_uno
011300151001     C                   DOU       SqlCod <> 0
011400151001      **         lettura cursore
011500151001     C/EXEC SQL
011600151001     C+ FETCH NEXT FROM TAS00 INTO :FLD_Depot, :FLD_Linea, :FLD_nr
011700151001     C/END-EXEC
011800151001     C                   SELECT
011900151001     **
012000151001     C                   WHEN      SqlCod = 100
012100151001     c* a fine file
012200151001     c                   if        almeno_uno = 'S'
012300151001     c                   else
012400151001     c                   end
012500151001     c                   leave
012600151001     c*
012700151001     C                   WHEN      SqlCod < 0
012800151002     C                   seton                                        H2
012900151002     c                   RETURN
013000151001     c*
013100151001     C                   OTHER
013200151001     c* DETTAGLIO
013300151001     c* riga   dettaglio
013400151001     c                   exsr      WRT_rig_det
013500151001     c*
013600151001     C                   ENDSL
013700151001
013800151001     C                   ENDDO
013900151001     C/EXEC SQL
014000151001     C+ Close TAS00
014100151001     C/END-EXEC
014200151001     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
014300050510     c*  Chiude il File per eseguire SQL finale di totalizzazione
014400151001     c                   Close     WFIEU09F
014500050510     c* Totali
014600050510     c                   exsr      totalizza
014700050510     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
014800151001     c*
014900041007     c     fine          tag
015000050511     ?****************************************************************************
015100151001      *?  Scrive   Riga File di Work
015200151001     ?****************************************************************************
015300151001     c     WRT_rig_det   begsr
015400151001      *
015500151001     c                   move      'S'           almeno_uno
015600151001     c     k009          chain     wfieu09f
015700151001     c                   if        %Found(wfieu09f)
015800151001     c                   add       FLD_Nr        M_F09NSP
015900151001     c                   update    Fieu009
016000151001     c                   else
016100151001      *
016200151001     c                   clear                   Fieu009
016300151001     c                   movel     FLD_Depot     M_F09DEPOT
016400151001     c                   z-add     FLD_Linea     M_F09LNP
016500151001     c                   z-add     FLD_Nr        M_F09NSP
016600151001      *
016700151001     c                   write     Fieu009
016800151001     c                   end
016900151001      *
017000151001     c                   endsr
017100050511     ?****************************************************************************
017200151001      *?   Totali da  File di Work  calcola la percentuale
017300050511     ?****************************************************************************
017400050510     c     Totalizza     begsr
017500050510      *
017600151001     c                   Open      WFieu09F
017700050511      * ?------------------------------------------------------------------ */
017800151001      **  ricava la percentuale per ogni singolo DEPOT / LINEA
017900050511      * ?------------------------------------------------------------------ */
018000050510     C/EXEC SQL
018100151001     C+ Declare TOT Cursor for
018200151001     C+  SELECT F09DEPOT, Sum(F09NSP)
018300151001     C+  FROM WFIEU09F
018400151001     C+  GROUP BY F09DEPOT
018500151001     C+  ORDER BY F09DEPOT
018600050510     C/END-EXEC
018700050510      *          apertura cursore
018800050510     C/EXEC SQL
018900151001     C+ OPEN TOT
019000050510     C/END-EXEC
019100050510     c                   clear                   almeno_uno
019200050510      **
019300050510     C                   DOU       SqlCod <> 0
019400050510      **         lettura cursore
019500050510     C/EXEC SQL
019600151001     C+ Fetch Next From TOT Into  :FLD_Depot, :TOT_NumSped
019700050510     C/END-EXEC
019800050510     C                   SELECT
019900050510     **
020000050510     C                   WHEN      SqlCod = 100
020100050510     c* a fine file
020200050510     c                   if        almeno_uno = 'S'
020300050510     c                   else
020400050510     c                   end
020500050510     c                   leave
020600050510     c*
020700050510     C                   WHEN      SqlCod < 0
020800151002     C                   seton                                        H3
020900151002     c                   RETURN
021000050510     c*
021100050510     C                   OTHER
021200050510     c* DETTAGLIO
021300050510     c* riga   dettaglio
021400050510     c                   exsr      Agg_rig_det
021500050510     c*
021600050510     C                   ENDSL
021700050510
021800050510     C                   ENDDO
021900050510     C/EXEC SQL
022000151001     C+ Close TOT
022100050510     C/END-EXEC
022200050510     c
022300050510     c     fine_SR       tag
022400151001     c                   Close     WFieu09F
022500151001     c
022600151001     c                   if        almeno_uno = 'S'
022700151002     c                   exsr      daFare_a_LR
022800151001     c                   else
022900151001     c                   end
023000050510      *
023100050510     c                   endsr
023200151001     ?****************************************************************************
023300151001      *?   Aggiorna    File di Work
023400151001     ?****************************************************************************
023500151001     c     Agg_rig_det   begsr
023600151001      *
023700151001     c                   move      'S'           almeno_uno
023800151001      *
023900151001      * x verificare esistenza sul cappario ultimo DPD
024000151001     c                   clear                   ultVER            5 0
024100171106     C     *hival        setll     dpcve11i
024200171106     C                   readp     dpcve11i
024300171106     c                   if        not %Eof(dpcve11i)
024400171106     c                   z-add     cveVER        ultVER
024500151001     c                   end
024600151001      *
024700151001     C     FLD_Depot     setll     WFieu09F
024800151001     C     FLD_Depot     reade     WFieu09F
024900151001      *
025000151001     c                   dow       not %Eof(WFieu09F)
025100151001      *
025200151001     c                   eval      M_F09PERC = M_F09NSP * 100 / TOT_NumSped
025300151001      * sul cappario DPD
025400171106     c     DPD_capp      setll     dpcdp12i
025500151001      *
025600151001      * non esiste sul cappario ultimo DPD
025700171106     c                   if        not %Equal(dpcdp12i)
025800151001     c                   move      '*'           M_F09flg
025900151001     c                   end
026000151001      *
026100151001     c                   update    Fieu009
026200151001      *
026300151001     C     FLD_Depot     reade     wFieu09F
026400151001     c                   enddo
026500151001      * -------
026600171106     C     KultVER       setll     dpCDP12i
026700171106     C     KultVER       reade     dpCDP12i
026800171106     c                   Dow       not %EoF(dpCDP12i)
026900151001      * sul cappario DPD
027000171106     c     CDPdstr       setll     wfieu09f
027100151001      *
027200151001      * non esiste fra i parcel ricevuti
027300151001     c                   if        not %Equal(wfieu09f)
027400151001      *
027500151001     c                   clear                   Fieu009
027600171106     c                   movel     CDPdstr       M_F09DEPOT
027700151002     c                   z-add     190           M_F09LNP
027800151002      *
027900151002     C                   IF        CDPISO2 <> *blanks
028000151002     C                   Z-ADD     1             jNaz
028100151002     C     CDPISO2       LOOKUP    skNaz(jNaz)                            55
028200151002     C                   IF        %equal
028300151002     c                   z-add     skLAD(Jnaz)   M_F09LNP
028400151002     C                   ENDIF
028500151002     C                   ENDIF
028600151001     c                   move      'N'           M_F09flg
028700151001      *
028800151001     c                   write     Fieu009
028900151001     c                   end
029000151001      *
029100171106     C     KultVER       reade     dpCDP12i
029200151001     c                   endDo
029300151001      * -------
029400151001     c                   endsr
029500151002     ?****************************************************************************
029600151002      *?   operazioni Finali dopo aver finito il ciclo di lettura SQL
029700151002     ?****************************************************************************
029800151002     c     daFare_a_LR   begsr
029900151002      *
030000151001      *  se richiesto richiamare il programma di stampa
030100151001     c                   if        PASLST = 'S'
030200151001     c                   call      'FIEU09R1'
030300151001     c                   parm                    KPJBA
030400151001     c                   end
030500151002     c* Aggiorna
030600151002     c                   if        PASUPD = 'S'
030700151002     c                   exsr      Aggiorna
030800151002     c                   end
030900151001      *
031000151001     c                   endsr
031100151002     ?****************************************************************************
031200151002      *?   Aggiorna la Linea sui legami se > 50%
031300151002     ?****************************************************************************
031400151002     c     Aggiorna      begsr
031500151002      * -------
031600151002     c                   Open      WFieu09F
031700151002      * ?------------------------------------------------------------------ */
031800151002      **  prende con la percentuale > 50% --> solo i records Blk
031900151002      * ?------------------------------------------------------------------ */
032000151002     C/EXEC SQL
032100151002     C+ Declare PRC Cursor for
032200151013     C+  select f09DEPOT, F09LNP, F09PERC  from wfieu09f where
032300151013     C+  f09DEPOT concat digits(F09PERC) in (
032400151013     C+  SELECT f09DEPOT concat digits(dec(max( F09PERC ), 5, 1))
032500151013     c+  FROM WFIEU09F WHERE f09FLG <>'*' GROUP BY F09DEPOT)
032600151013     c+  ORDER BY F09DEPOT
032700151002     C/END-EXEC
032800151002      *          apertura cursore
032900151002     C/EXEC SQL
033000151002     C+ OPEN PRC
033100151002     C/END-EXEC
033200151002     c                   clear                   almeno_uno
033300151002      **
033400151002     C                   DOU       SqlCod <> 0
033500151002      **         lettura cursore
033600151002     C/EXEC SQL
033700151013     C+ Fetch Next From PRC Into  :f09DEPOT, :F09LNP, :F09PERC
033800151002     C/END-EXEC
033900151002     C                   SELECT
034000151002     **
034100151002     C                   WHEN      SqlCod = 100
034200151002     c* a fine file
034300151002     c                   if        almeno_uno = 'S'
034400151002     c                   else
034500151002     c                   end
034600151002     c                   leave
034700151002     c*
034800151002     C                   WHEN      SqlCod < 0
034900151002     C                   seton                                        H4
035000151002     c                   RETURN
035100151002     c*
035200151002     C                   OTHER
035300151002     c* DETTAGLIO   riga   dettaglio
035400151002     c                   exsr      Agg_dep_LNP
035500151002     c*
035600151002     C                   ENDSL
035700151002
035800151002     C                   ENDDO
035900151002     C/EXEC SQL
036000151002     C+ Close PRC
036100151002     C/END-EXEC
036200151002     c     esce_LNP      tag
036300151002     c                   Close     WFieu09F
036400151002      * -------
036500151002     c                   endsr
036600151002     ?****************************************************************************
036700151002      *?   Aggiorna file LEGAMI con la Linea
036800151002     ?****************************************************************************
036900151002     C     Agg_dep_LNP   BEGSR
037000151002      **
037100171106     C     F09DEPOT      setll     dpLEG14l
037200171106     C     F09DEPOT      reade     dpLEG14l
037300171106     c                   DOw       not %EoF(dpLEG14l)
037400151002      **
037500171106     c                   if        legDDE <= data_oggi and data_oggi <= legDSC
037600171106     c                   movel     legflo        dLEGflo
037700151016      **  chiodo per Linee di Backup
037800151016     c                   if        F09lnp = 192 or F09lnp = 291
037900151016     c                   eval      F09Lnp   = 190
038000151016     c                   end
038100171106     c                   movel     F09Lnp        §legFLOLNP
038200171106     c                   movel     dLEGflo       LEGflo
038300171106     c                   update    dpLEG000
038400151002     c                   leave
038500151002     c                   end
038600151002      **
038700171106     C     F09DEPOT      reade     dpLEG14l
038800151002     c                   endDO
038900151002      **
039000151002     c                   endsr
039100151002     ?****************************************************************************
039200151002      *?   INIZIALIZZAZIONE
039300151002     ?****************************************************************************
039400151002     C     *inzsr        BEGSR
039500151002      **
039600040820     C     *ENTRY        PLIST
039700040820     C                   PARM                    KPJBA
039800050510     c                   movel     kpjbu         DsPassaggio
039900050511      **
040000050511     C                   Z-ADD     1             CODUT
040100050511     C                   CALL      'X§PARUT'
040200050511     C                   PARM                    UT§DSE
040300110511     C                   MOVEL     REC42         CNCR42
040400110511     C                   MOVEL     simRGS        RSUT             20
040500041103      *
040600040820      *  imposta UDATE di elaborazione
040700040820     C                   move      *date         G02DAT
040800040820     C                   MOVE      *ZEROS        G02INV
040900040820     C                   MOVE      *BLANKS       G02ERR
041000040820     C                   CALL      'XSRDA8'
041100040820     C                   PARM                    WLBDAT
041200040820     C                   Z-ADD     G02INV        data_oggi
041300151001     C                   move      G02dat        day               8 0
041400040820     C                   TIME                    W0140            14 0
041500151001     C                   MOVEl     W0140         ore               6 0
041600040823      **
041700050510     c                   move      PASdti        dadata            8 0
041800050510     c                   move      PASdtf        adata             8 0
041900040823      **
042000050511     c                   seton                                        66
042100040825     c                   clear                   prima_volta       1
042200040826     c                   clear                   almeno_uno
042300151001     C                   MOVEL     DSPGM         NOMPGM           10
042400050511      **
042500050511     C                   move      *zeros        G02DAT
042600050511     C                   MOVE      dadata        G02INV
042700050511     C                   MOVE      '3'           G02ERR
042800050511     C                   CALL      'XSRDA8'
042900050511     C                   PARM                    WLBDAT
043000151001     C                   Z-ADD     G02Dat        PDadata           8 0
043100050511      **
043200050511     C                   move      *zeros        G02DAT
043300050511     C                   MOVE      adata         G02INV
043400050511     C                   MOVE      '3'           G02ERR
043500050511     C                   CALL      'XSRDA8'
043600050511     C                   PARM                    WLBDAT
043700151001     C                   Z-ADD     G02dat        PAdata            8 0
043800151002     C*
043900151002     C* Carico le tabelle occorrenti
044000151002     C                   EXSR      CARTAB
044100040820     c*
044200151001     c                   Open      Wfieu09F
044300050510     c*
044400151001     c     K009          klist
044500151001     c                   KFLD                    FLD_Depot
044600151001     c                   KFLD                    FLD_Linea
044700151001     c*
044800171106     c     kULTver       klist
044900171106     c                   KFLD                    FLGATB
045000171106     c                   KFLD                    ULTver
045100151001     c*
045200151001     c     DPD_capp      klist
045300171106     c                   KFLD                    FLGATB
045400171106     c                   KFLD                    ULTver
045500151001     c                   KFLD                    FLD_Depot
045600171106     c*
045700171106     c                   clear                   FLGATB            1
045800151001     c*
045900040820     C                   ENDSR
046000151002     C*------------------------------------------------------------------------*
046100151002     C* CARTAB - CARICAMENTO DATI TABELLATI
046200151002     C*------------------------------------------------------------------------*
046300151002     C     cartab        BEGSR
046400151002     C*
046500151002     C* Reperisco tutte le nazioni (ISO2) servite da DPD
046600151002     C                   Z-ADD     *zeros        jNaz
046700151002     C                   CLEAR                   skNaz
046800151002     C                   CLEAR                   skNazKey
046900151002     C                   CLEAR                   DS15
047000151002     C                   EVAL      tblKUT = 1
047100151002     C                   EVAL      tblCOD = '15'
047200151002     C     KEYtabel_P    SETLL     TABEL00F
047300151002     C                   IF        %found(TABEL00F)
047400151002     C     KEYtabel_P    READE     TABEL00F
047500151002     C                   DOW       not %eof(TABEL00F)
047600151002     C                   IF        tblFLG = *blanks
047700151002     C                   EVAL      DS15 = tblUNI
047800151002     C                   IF        §15LAD > *zeros OR
047900151002     C                             §15RED = 'S'
048000151002     C                   ADD       1             jNaz
048100151002     C                   MOVEL(P)  §15COD        skNaz(jNaz)
048200151002     C                   MOVEL(P)  tblKEY        skNazKey(jNaz)
048300151002     C                   MOVEL(P)  §15LAD        skLAD(jNaz)
048400151002     C                   ENDIF
048500151002     C                   ENDIF
048600151002     C     KEYtabel_P    READE     TABEL00F
048700151002     C                   ENDDO
048800151002     C                   ENDIF
048900151002     C*
049000151002     C* TABEL00F - Parziale
049100151002     C     KEYtabel_P    KLIST
049200151002     C                   KFLD                    tblKUT
049300151002     C                   KFLD                    tblCOD
049400151002     C*
049500151002     C                   ENDSR
049600050511     ?****************************************************************************
