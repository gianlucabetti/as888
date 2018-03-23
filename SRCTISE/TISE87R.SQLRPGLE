000100040820     H DECEDIT('0,') DATEDIT(*dmy.)
000200050510      **********************************************************************
000300061012     fWFFM200F  uf a E           K DISK    UsrOPN
000400050511     f                                     prefix(M_)
000500061128     fWFFM300F  uf a E           K DISK    UsrOPN rename(aitra000:wffm3)
000600050524     ffiapd01l  if   E           K DISK
000700061012     ffiftt02l  if   E           K DISK
000800061012     faitra03l  if   E           K DISK
000900050510      **-----------------------------------------------------------------***
001000040820     d KPJBA         E DS
001100050511     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001200110511     D CNCR42        E DS
001300050905     D dapdflr       E DS
001400040820     D*
001500061127-->  D DsPassaggio   e DS                  EXTNAME(TISE88ds)
001600050510     D fiftt00f      e DS
001700050510     D azorg00f      e DS
001800061012     D aitra00f      e DS
001900040820     D*
002000040820     D WLBDAT          DS
002100040820     D  G02DAT                 1      8  0
002200040820     D  G02INV                 9     16  0
002300040820     D  G02ERR                17     17
002400040820     D  G02TGI                18     22  0
002500040820      *
002600040820     D  data_Oggi      s                   like(G02INV) INZ(0)
002700040826     D  almeno_uno     s              1
002800040820      *
002900050510     D  FLD_Distr      s                   like(orgfl3)
003000050510     D  FLD_Area       s                   like(orgcar)
003100050510     D  FLD_Fgs        s                   like(fttfgs)
003200050510     D  FLD_Padr       s                   like(fttpdr)
003300050905     D  FLD_Spediz     s              7  0
003400051111     D  FLD_Ritiri     s              7  0
003500051111     D  FLD_Consegne   s              7  0
003600050510     D  FLD_TotVal     s             11  3
003700050510     D  FLD_GioLav     s              5  0
003800050510     D  FLD_MedPadr    s             11  3
003900050510      *
004000050511     D  TOT_Dist       s                   like(M_FmeDist)
004100050511     D  TOT_Area       s                   like(M_FmeArea)
004200050511     D  TOT_POper      s                   like(M_FmePOper)
004300050510     D  TOT_SumMed     s             11  3
004400050510     D  TOT_NumAut     s              5  0
004500050510     D  TOT_MedGiorPO  s             11  3
004600050510      *
004700050511     D*-------------
004800050511     D* Reperimento nome PGM
004900050511     D STATUS         SDS           333
005000050511     D  DSPGM            *PROC
005100050511     D*-------------
005200050511      *
005300040820     D digits          C                   CONST('0123456789')
005400050511      * ?------------------------------------------------------------------ */
005500050511      * ? In questa Fase si rilevano i dati per singolo Autista
005600050511      * ? dai Conteggi Padroncini prendendo i costi x giornata.
005700050511      * ? Poi si possono individuare, contandole, le giornate che
005800050511      * ? l'Autista ha lavorato.
005900050511      * ? Con il Totale x Autista diviso le sue giornate lavorate
006000050511      * ? si ha il costo medio di ogni singolo Autista
006100050511      * ? e scrivo un record sul File Statistico.
006200050510      **
006300050510      **  Il File è organizzato x Distretto/Area/P.O./Autista
006400050510      **   i cui dati sono:
006500050510      **     TOTALE VALORE x AUTISTA (di tutti i Giorni Lavorati)
006600050510      **     TOTALE Giorni LAVORATI
006700050510      **     MEDIA Giornaliera x Autista
006800050510      **           ottenuta come TOT_VALORE diviso TOT_Giorni_LAVORATI
006900050510      **  Con questa base si ottengono i valori x Singola Giornata Autista.
007000050511      * ?------------------------------------------------------------------ */
007100050510     C/EXEC SQL
007200050510     C+ Declare A1 Cursor for
007300050510     C+ SELECT orgfl3, orgcar, fttfgs, fttpdr,
007400050905     C+   decimal(sum(Sped_gior),  7, 0) as Tot_Sped,
007500050905     C+   decimal(sum(Tot_Costo), 11, 3) as Tot_Valore,
007600050510     C+   decimal(count(*), 5, 0) as Num_Giorni,
007700050510     C+   decimal(sum(TOT_Costo)/count(*), 11, 3) as Media_Giornata
007800050510     C+ FROM (
007900050510     C+ SELECT orgfl3, orgcar, fttfgs, FTTPDR, fttddc,
008000050905     C+ decimal(sum(fttset), 7, 0) as Sped_gior,
008100081010     C+ decimal(sum(FTTITT + FTTITA + FTTTPE + FTTTBN + FTTTIM + FTTMNT
008200081010     C+ + FTTpCAR), 11, 3) as Tot_Costo
008300081010     C+  FROM FIFTT00F join AZORG01L on FTTFGS=ORGFIL
008400050517     C+ WHERE ftttsr =' ' and fttfvl = 'C' and fttNFF <> 999999999
008500050510     C+   AND fttddc between :DADATA  and :ADATA
008600050510     C+ GROUP BY  orgfl3, orgcar, fttpdr, fttddc, fttfgs
008700050510     C+ ) as A
008800050510     C+ GROUP BY  orgfl3, orgcar, fttfgs, fttpdr
008900050510     C+ ORDER BY  orgfl3, orgcar, fttfgs, fttpdr
009000050510     C/END-EXEC
009100050510      *          apertura cursore
009200050510     C/EXEC SQL
009300050510     C+ OPEN A1
009400050510     C/END-EXEC
009500050510     c                   clear                   almeno_uno
009600050510      **
009700050510     C                   DOU       SqlCod <> 0
009800050510      **         lettura cursore
009900050510     C/EXEC SQL
010000050905     C+ FETCH NEXT FROM A1 INTO :FLD_Distr, :FLD_Area, :FLD_Fgs,
010100050905     C+ :FLD_Padr, :FLD_Spediz, :FLD_TotVal, :FLD_GioLav, :FLD_MedPadr
010200050510     C/END-EXEC
010300050510     C                   SELECT
010400050510     **
010500050510     C                   WHEN      SqlCod = 100
010600050510     c* a fine file
010700050510     c                   leave
010800050510     c*
010900050510     C                   WHEN      SqlCod < 0
011000050510     C                   seton                                        H1
011100061016     c                   goto      fine1
011200050510     c*
011300050510     C                   OTHER
011400050510     **
011500050510     c* DETTAGLIO
011600050510     c* riga   dettaglio
011700050510     c                   exsr      Wri_rig_det
011800050510     c*
011900050510     C                   ENDSL
012000050510
012100050510     C                   ENDDO
012200061016
012300061016     c     fine1         tag
012400061016
012500050510     C/EXEC SQL
012600050510     C+ Close A1
012700050510     C/END-EXEC
012800061016
012900061016     c   h1              goto      fine
013000050510     c*
013100050510     c*  Chiude il File per eseguire SQL finale di totalizzazione
013200061012     c                   Close     WFFM200F
013300050510     c* Totali
013400050510     c                   exsr      totalizza
013500061016     c* automezzi autisti non valorizzati nel periodo
013600061016     c                   exsr      autnoval
013700050510     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
013800061127     c* generazione file con codici autisti doppi in archivio automezzi
013900061127     c                   if        pasdop = 'S'
014000061127     c                   exsr      autdoppi
014100061127     c                   endif
014200040823      *
014300041007     c     fine          tag
014400020326     C                   SETON                                        LR
014500061016     ?*_____________________________________________________________________******
014600050511      *?    Scrive Riga File di Work
014700061016     ?*_____________________________________________________________________******
014800050510     c     Wri_rig_det   begsr
014900050510      *
015000050511     c                   move      'S'           almeno_uno
015100050524      *
015200061012     c                   clear                   WFFM2000
015300050510      *
015400050524     c                   eval      APDTIP = 'A'
015500050524     c                   eval      APDPDR =  FLD_Padr
015600050524     c     K_aut         chain     fiapd01l
015700050524     c                   if        %Found(fiapd01l)
015800050905     c                   movel     apdflr        dapdflr
015900050905      * è alfanumerico --> campo filler
016000050905     c                   if        §apdKM <> *blank
016100050905     c                   move      §apdKM        M_FMKMETRI
016200050905     c                   end
016300050905      *
016400050524     c                   eval      M_FMEPESO   = apdPKL
016500050524     c                   eval      M_FMEVOLUME = apdVLU
016600061129     c                   eval      M_FMEADAPD  = apdrsf
016700061016      * campi anagrafica automezzi
016800061016     c                   clear                   m_FMEAaut
016900061016     c                   clear                   m_FMEDTIN
017000061017     c                   clear                   m_FMEtkau
017100061016     c     apdpdr        setll     aitra03l
017200061016     c                   do        *hival
017300061016     c     apdpdr        reade     aitra03l
017400061016     c                   if        %eof(aitra03l)
017500061016     c                   leave
017600061016     c                   endif
017700061016      * inserisco il dato se accreditato
017800061127     c                   if        tradfi = 0 or tradfi > dadata
017900061012     c                   movel     traaut        m_FMEAaut
018000061016     c                   z-add     tradin        m_FMEDTIN
018100061017     c                   z-add     trakau        m_FMEtkau
018200061127     c                   z-add     tradfi        m_FMEDTfi
018300061016     c                   endif
018400061016     c                   enddo
018500061012     c                   end
018600061012      *
018700050511     c                   eval      M_FMEDIST   = FLD_Distr
018800050511     c                   eval      M_FMEAREA   = FLD_Area
018900050511     c                   eval      M_FMEPOPER  = FLD_Fgs
019000050511     c                   eval      M_FMEAUTIST = FLD_Padr
019100050905     c                   eval      M_FMSPEDIZ  = FLD_Spediz
019200051111      **
019300051111     c                   clear                   FLD_Ritiri
019400051111     c                   clear                   FLD_Consegne
019500051111      **
019600051111     c                   eval      fttfvl = 'C'
019700051111     c                   eval      fttpdr = APDPDR
019800051111     c                   eval      fttddc = dadata
019900051111      **
020000051111     C     K_FTT         setll     fiftt02l
020100051111     C     K_FTT_Pdr     reade     fiftt02l
020200051111      **
020300051111     c                   dow       not %EoF(fiftt02l)
020400051111      **  se supera la data limite del periodo selezionato
020500051111     c                   if        fttddc > adata
020600051111     c                   leave
020700051111     c                   end
020800051111      **
020900051111     c                   select
021000051111     c                   when      ftttsr = 'C'
021100051111     c                   add       fttSET        FLD_Consegne
021200051111     c                   when      ftttsr = 'R'
021300051111     c                   add       fttSET        FLD_Ritiri
021400051111     c                   endSL
021500051111      **
021600051111     C     K_FTT_Pdr     reade     fiftt02l
021700051111     c                   endDO
021800051111      **
021900051111     c                   eval      M_FMSPERIT  = FLD_Ritiri
022000051111     c                   eval      M_FMSPECON  = FLD_Consegne
022100051111      **
022200050511     c                   eval      M_FMETOTVAL = FLD_TotVal
022300050511     c                   eval      M_FMENGILAV = FLD_GioLav
022400050511     c                   eval      M_FMEMEDGIO = FLD_MedPadr
022500050511     c                   eval      M_FMEDADATA = pasdti
022600050511     c                   eval      M_FMEADATA  = pasdtf
022700050510      *
022800061012     c                   write     WFFM2000
022900050510      *
023000050510     c                   endsr
023100061016     ?*_____________________________________________________________________******
023200050511      *?   Totali da  File di Work
023300061016     ?*_____________________________________________________________________******
023400050510     c     Totalizza     begsr
023500050510      *
023600061012     c                   Open      WFFM200F
023700050511      * ?------------------------------------------------------------------ */
023800050511      **  Con il file Ricavato Precedentemente x Autista è possibile
023900050511      **   totalizzare e ricavare le medie x P.O..
024000050511      **   Con il totale di tutti gli Autisti del P.O. diviso il numero
024100050510      **   stesso degli Autisti del P.O. si ottiene il valore medio
024200050510      **   a giornata del P.O.
024300050511      * ?------------------------------------------------------------------ */
024400050510     C/EXEC SQL
024500050510     C+ Declare B2 Cursor for
024600050510     C+  SELECT FMEDIST, FMEAREA, FMEPOPER,
024700050510     C+  decimal(sum(FMEMEDGIO), 11, 3) as TOT_PO_MED,
024800050510     C+  decimal( COUNT(*), 5, 0) as NUM_AUT_PO,
024900050510     C+  decimal(sum(FMEMEDGIO)/COUNT(*), 11, 3) as MEDIA_PO
025000061012     C+  FROM WFFM200F
025100050510     C+  GROUP BY FMEDIST, FMEAREA, FMEPOPER
025200050510     C+  ORDER BY FMEDIST, FMEAREA, FMEPOPER
025300050510     C/END-EXEC
025400050510      *          apertura cursore
025500050510     C/EXEC SQL
025600050510     C+ OPEN B2
025700050510     C/END-EXEC
025800050510     c                   clear                   almeno_uno
025900050510      **
026000050510     C                   DOU       SqlCod <> 0
026100050510      **         lettura cursore
026200050510     C/EXEC SQL
026300050510     C+ Fetch Next From B2 Into :TOT_Dist, :TOT_Area, :TOT_POper,
026400050510     C+       :TOT_SumMed, :TOT_NumAut, :TOT_MedGiorPO
026500050510     C/END-EXEC
026600050510     C                   SELECT
026700050510     **
026800050510     C                   WHEN      SqlCod = 100
026900050510     c                   leave
027000050510     c*
027100050510     C                   WHEN      SqlCod < 0
027200050510     C                   seton                                        H2
027300050510     c                   goto      fine_SR
027400050510     c*
027500050510     C                   OTHER
027600050510     **
027700050510     c* DETTAGLIO
027800050510     c* riga   dettaglio
027900050510     c                   exsr      Agg_rig_det
028000050510     c*
028100050510     C                   ENDSL
028200050510
028300050510     C                   ENDDO
028400061016
028500061016     c     fine_SR       tag
028600061016
028700050510     C/EXEC SQL
028800050510     C+ Close B2
028900050510     C/END-EXEC
029000050510     c
029100061012     c                   Close     WFFM200F
029200050510      *
029300050510     c                   endsr
029400061016     ?*_____________________________________________________________________******
029500050511      *?   Aggiorna    File di Work
029600061016     ?*_____________________________________________________________________******
029700050511     c     Agg_rig_det   begsr
029800050511      *
029900050511     c                   move      'S'           almeno_uno
030000050511      *
030100061012     C     K_MED         setll     WFFM200F
030200061012     C     K_MED         reade     WFFM200F
030300050511      *
030400061012     c                   dow       not %Eof(WFFM200F)
030500050511      *
030600050511     c                   eval      M_FMENUMAUT = TOT_NumAut
030700050511     c                   eval      M_FMEMEDPO  = TOT_MedGiorPO
030800050511      *
030900061012     c                   update    WFFM2000
031000050511      *
031100061012     C     K_MED         reade     WFFM200F
031200050511     c                   enddo
031300050510      *
031400050510     c                   endsr
031500061016     ?*_____________________________________________________________________******
031600061016      *?   Inserisce righe con codici autisti presenti in anagrafica autisti
031700061016      *?   ma non valorizzati nel periodo estratto
031800061016     ?*_____________________________________________________________________******
031900061016     c     autnoval      begsr
032000061016     c                   Open      WFFM200F
032100061016
032200061016     C/EXEC SQL
032300061016     C+ Declare C3 Cursor for
032400061016     C+  SELECT traaut, trakau, tradin, trafil FROM AITRA00F
032500061017     C+ WHERE traann = ' ' and tradfi = 0 and  tradin < :adata and
032600070129     C+  traris = ' 'and
032700061016     C+  trakau not in (SELECT fmeautist FROM wffm200f)
032800061016     C/END-EXEC
032900061016      *          apertura cursore
033000061016     C/EXEC SQL
033100061016     C+ OPEN C3
033200061016     C/END-EXEC
033300061016     c                   clear                   almeno_uno
033400061016      **
033500061016     C                   DOU       SqlCod <> 0
033600061016      **         lettura cursore
033700061016     C/EXEC SQL
033800061016     C+ Fetch Next From C3 Into :traaut, :trakau, :tradin, :trafil
033900061016     C/END-EXEC
034000061016     C                   SELECT
034100061016     **
034200061016     C                   WHEN      SqlCod = 100
034300061016     c                   leave
034400061016     c*
034500061016     C                   WHEN      SqlCod < 0
034600061016     C                   seton                                        H2
034700061016     c                   goto      fine_SR2
034800061016     c*
034900061016     C                   OTHER
035000061016     **
035100061016     c* DETTAGLIO
035200061016     c* riga   dettaglio
035300061016     c                   exsr      agg_autnoval
035400061016     c*
035500061016     C                   ENDSL
035600061016
035700061016     C                   ENDDO
035800061016
035900061016     c     fine_SR2      tag
036000061016
036100061016     C/EXEC SQL
036200061016     C+ Close C3
036300061016     C/END-EXEC
036400061016     c
036500061016     c                   Close     WFFM200F
036600061016      *
036700061016     c                   endsr
036800061016     ?*_____________________________________________________________________******
036900061016     C     agg_autnoval  BEGSR
037000061016     ?*_____________________________________________________________________******
037100061016      **
037200061016     c                   clear                   WFFM2000
037300061016     c                   movel     traaut        m_FMEAaut
037400061016     c                   z-add     tradin        m_FMEDTIN
037500061017     c                   z-add     trakau        m_FMEtkau
037600061016     c                   z-add     trafil        m_FMEpoper
037700061016     c                   eval      APDTIP = 'A'
037800061016     c                   eval      APDPDR =  trakau
037900061016     c     K_aut         chain     fiapd01l
038000061016     c                   if        %Found(fiapd01l)
038100061016     c                   movel     apdflr        dapdflr
038200061129     c                   eval      M_FMEADAPD  = apdrsf
038300061016     c                   z-add     apdpdr        m_FMEautist
038400061016     c                   endif
038500061016      *
038600061016     c                   write     WFFM2000
038700061016     c                   endsr
038800061127     ?*_____________________________________________________________________******
038900061127      *?   Estrae codici autisti doppi in anagrafica automezzi
039000061127     ?*_____________________________________________________________________******
039100061127     c     autdoppi      begsr
039200061127     c                   Open      WFFM300F
039300061127
039400061127     C/EXEC SQL
039500061127     C+ Declare D4 Cursor for
039600061127     C+ SELECT * FROM aitra01l
039700070125     C+ wHERE tradfi = 0 and traaut <> 'MACCHINA DI SCORTA'
039800070125     C+ and trakau in ( select trakau from (
039900061127     C+ SELECT TRAKAU, count(*) FROM aitra01l WHERE tradfi = 0 GROUP BY
040000061127     C+ trakau HAVING count(*) > 1 )as b   ) and trakau>0
040100061127     C+ ORDER BY trakau
040200061127     C/END-EXEC
040300061127      *          apertura cursore
040400061127     C/EXEC SQL
040500061127     C+ OPEN D4
040600061127     C/END-EXEC
040700061127     c                   clear                   almeno_uno
040800061127      **
040900061127     C                   DOU       SqlCod <> 0
041000061127      **         lettura cursore
041100061127     C/EXEC SQL
041200061127     C+ Fetch Next From D4 Into :aitra00f
041300061127     C/END-EXEC
041400061127     C                   SELECT
041500061127     **
041600061127     C                   WHEN      SqlCod = 100
041700061127     c                   leave
041800061127     c*
041900061127     C                   WHEN      SqlCod < 0
042000061127     C                   seton                                        H3
042100061127     c                   goto      fine_SR3
042200061127     c*
042300061127     C                   OTHER
042400061127     **
042500061127     c* DETTAGLIO
042600061127     c* riga   dettaglio
042700061128     c                   write     wffm3
042800061127     c*
042900061127     C                   ENDSL
043000061127
043100061127     C                   ENDDO
043200061127
043300061127     c     fine_SR3      tag
043400061127
043500061127     C/EXEC SQL
043600061127     C+ Close D4
043700061127     C/END-EXEC
043800061127     c
043900061127     c                   Close     WFFM300F
044000061127      *
044100061127     c                   endsr
044200061016     ?*_____________________________________________________________________******
044300061016     C     *inzsr        BEGSR
044400061016     ?*_____________________________________________________________________******
044500040820      **
044600050511     C     K_MED         KLIST
044700050511     C                   Kfld                    TOT_Dist
044800050511     C                   Kfld                    TOT_Area
044900050511     C                   Kfld                    TOT_POper
045000050524      **
045100050524     C     K_aut         KLIST
045200050524     C                   Kfld                    APDTIP
045300050524     C                   Kfld                    APDPDR
045400051111      **
045500051111     C     K_FTT         KLIST
045600051111     C                   Kfld                    fttPDR
045700051111     C                   Kfld                    fttFVL
045800051111     C                   Kfld                    fttDDC
045900051111      **
046000051111     C     K_FTT_Pdr     KLIST
046100051111     C                   Kfld                    fttPDR
046200051111     C                   Kfld                    fttFVL
046300050511      **
046400040820     C     *ENTRY        PLIST
046500040820     C                   PARM                    KPJBA
046600050510     c                   movel     kpjbu         DsPassaggio
046700050511      **
046800050511     C                   Z-ADD     1             CODUT
046900050511     C                   CALL      'X§PARUT'
047000050511     C                   PARM                    UT§DSE
047100110511     C*****              MOVEL     RAGUT         RSUT             20
047200110511     C                   MOVEL     REC42         CNCR42
047300110511     C                   MOVEL     simRGS        RSUT             20
047400041103      *
047500040820      *  imposta UDATE di elaborazione
047600040820     C                   move      *date         G02DAT
047700040820     C                   MOVE      *ZEROS        G02INV
047800040820     C                   MOVE      *BLANKS       G02ERR
047900040820     C                   CALL      'XSRDA8'
048000040820     C                   PARM                    WLBDAT
048100040820     C                   Z-ADD     G02INV        data_oggi
048200040820     C                   TIME                    W0140            14 0
048300040823      **
048400050510     c                   move      PASdti        dadata            8 0
048500050510     c                   move      PASdtf        adata             8 0
048600040823      **
048700050511     c                   seton                                        66
048800040825     c                   clear                   prima_volta       1
048900040826     c                   clear                   almeno_uno
049000040820     c*
049100061012     c                   Open      WFFM200F
049200050510     c*
049300040820     C                   ENDSR
