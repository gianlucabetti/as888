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
001200050905     D dapdflr       E DS
001300040820     D*
001400061127-->  D DsPassaggio   e DS                  EXTNAME(TISE88ds)
001500050510     D fiftt00f      e DS
001600050510     D azorg00f      e DS
001700061012     D aitra00f      e DS
001800040820     D*
001900040820     D WLBDAT          DS
002000040820     D  G02DAT                 1      8  0
002100040820     D  G02INV                 9     16  0
002200040820     D  G02ERR                17     17
002300040820     D  G02TGI                18     22  0
002400040820      *
002500040820     D  data_Oggi      s                   like(G02INV) INZ(0)
002600040826     D  almeno_uno     s              1
002700040820      *
002800050510     D  FLD_Distr      s                   like(orgfl3)
002900050510     D  FLD_Area       s                   like(orgcar)
003000050510     D  FLD_Fgs        s                   like(fttfgs)
003100050510     D  FLD_Padr       s                   like(fttpdr)
003200050905     D  FLD_Spediz     s              7  0
003300051111     D  FLD_Ritiri     s              7  0
003400051111     D  FLD_Consegne   s              7  0
003500050510     D  FLD_TotVal     s             11  3
003600050510     D  FLD_GioLav     s              5  0
003700050510     D  FLD_MedPadr    s             11  3
003800050510      *
003900050511     D  TOT_Dist       s                   like(M_FmeDist)
004000050511     D  TOT_Area       s                   like(M_FmeArea)
004100050511     D  TOT_POper      s                   like(M_FmePOper)
004200050510     D  TOT_SumMed     s             11  3
004300050510     D  TOT_NumAut     s              5  0
004400050510     D  TOT_MedGiorPO  s             11  3
004500050510      *
004600050511     D*-------------
004700050511     D* Reperimento nome PGM
004800050511     D STATUS         SDS           333
004900050511     D  DSPGM            *PROC
005000050511     D*-------------
005100050511      *
005200040820     D digits          C                   CONST('0123456789')
005300050511      * ?------------------------------------------------------------------ */
005400050511      * ? In questa Fase si rilevano i dati per singolo Autista
005500050511      * ? dai Conteggi Padroncini prendendo i costi x giornata.
005600050511      * ? Poi si possono individuare, contandole, le giornate che
005700050511      * ? l'Autista ha lavorato.
005800050511      * ? Con il Totale x Autista diviso le sue giornate lavorate
005900050511      * ? si ha il costo medio di ogni singolo Autista
006000050511      * ? e scrivo un record sul File Statistico.
006100050510      **
006200050510      **  Il File è organizzato x Distretto/Area/P.O./Autista
006300050510      **   i cui dati sono:
006400050510      **     TOTALE VALORE x AUTISTA (di tutti i Giorni Lavorati)
006500050510      **     TOTALE Giorni LAVORATI
006600050510      **     MEDIA Giornaliera x Autista
006700050510      **           ottenuta come TOT_VALORE diviso TOT_Giorni_LAVORATI
006800050510      **  Con questa base si ottengono i valori x Singola Giornata Autista.
006900050511      * ?------------------------------------------------------------------ */
007000050510     C/EXEC SQL
007100050510     C+ Declare A1 Cursor for
007200050510     C+ SELECT orgfl3, orgcar, fttfgs, fttpdr,
007300050905     C+   decimal(sum(Sped_gior),  7, 0) as Tot_Sped,
007400050905     C+   decimal(sum(Tot_Costo), 11, 3) as Tot_Valore,
007500050510     C+   decimal(count(*), 5, 0) as Num_Giorni,
007600050510     C+   decimal(sum(TOT_Costo)/count(*), 11, 3) as Media_Giornata
007700050510     C+ FROM (
007800050510     C+ SELECT orgfl3, orgcar, fttfgs, FTTPDR, fttddc,
007900050905     C+ decimal(sum(fttset), 7, 0) as Sped_gior,
008000081010     C+ decimal(sum(FTTITT + FTTITA + FTTTPE + FTTTBN + FTTTIM + FTTMNT
008100081010     C+ + FTTpCAR), 11, 3) as Tot_Costo
008200081010     C+  FROM FIFTT00F join AZORG01L on FTTFGS=ORGFIL
008300050517     C+ WHERE ftttsr =' ' and fttfvl = 'C' and fttNFF <> 999999999
008400050510     C+   AND fttddc between :DADATA  and :ADATA
008500050510     C+ GROUP BY  orgfl3, orgcar, fttpdr, fttddc, fttfgs
008600050510     C+ ) as A
008700050510     C+ GROUP BY  orgfl3, orgcar, fttfgs, fttpdr
008800050510     C+ ORDER BY  orgfl3, orgcar, fttfgs, fttpdr
008900050510     C/END-EXEC
009000050510      *          apertura cursore
009100050510     C/EXEC SQL
009200050510     C+ OPEN A1
009300050510     C/END-EXEC
009400050510     c                   clear                   almeno_uno
009500050510      **
009600050510     C                   DOU       SqlCod <> 0
009700050510      **         lettura cursore
009800050510     C/EXEC SQL
009900050905     C+ FETCH NEXT FROM A1 INTO :FLD_Distr, :FLD_Area, :FLD_Fgs,
010000050905     C+ :FLD_Padr, :FLD_Spediz, :FLD_TotVal, :FLD_GioLav, :FLD_MedPadr
010100050510     C/END-EXEC
010200050510     C                   SELECT
010300050510     **
010400050510     C                   WHEN      SqlCod = 100
010500050510     c* a fine file
010600050510     c                   leave
010700050510     c*
010800050510     C                   WHEN      SqlCod < 0
010900050510     C                   seton                                        H1
011000061016     c                   goto      fine1
011100050510     c*
011200050510     C                   OTHER
011300050510     **
011400050510     c* DETTAGLIO
011500050510     c* riga   dettaglio
011600050510     c                   exsr      Wri_rig_det
011700050510     c*
011800050510     C                   ENDSL
011900050510
012000050510     C                   ENDDO
012100061016
012200061016     c     fine1         tag
012300061016
012400050510     C/EXEC SQL
012500050510     C+ Close A1
012600050510     C/END-EXEC
012700061016
012800061016     c   h1              goto      fine
012900050510     c*
013000050510     c*  Chiude il File per eseguire SQL finale di totalizzazione
013100061012     c                   Close     WFFM200F
013200050510     c* Totali
013300050510     c                   exsr      totalizza
013400061016     c* automezzi autisti non valorizzati nel periodo
013500061016     c                   exsr      autnoval
013600050510     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
013700061127     c* generazione file con codici autisti doppi in archivio automezzi
013800061127     c                   if        pasdop = 'S'
013900061127     c                   exsr      autdoppi
014000061127     c                   endif
014100040823      *
014200041007     c     fine          tag
014300020326     C                   SETON                                        LR
014400061016     ?*_____________________________________________________________________******
014500050511      *?    Scrive Riga File di Work
014600061016     ?*_____________________________________________________________________******
014700050510     c     Wri_rig_det   begsr
014800050510      *
014900050511     c                   move      'S'           almeno_uno
015000050524      *
015100061012     c                   clear                   WFFM2000
015200050510      *
015300050524     c                   eval      APDTIP = 'A'
015400050524     c                   eval      APDPDR =  FLD_Padr
015500050524     c     K_aut         chain     fiapd01l
015600050524     c                   if        %Found(fiapd01l)
015700050905     c                   movel     apdflr        dapdflr
015800050905      * è alfanumerico --> campo filler
015900050905     c                   if        §apdKM <> *blank
016000050905     c                   move      §apdKM        M_FMKMETRI
016100050905     c                   end
016200050905      *
016300050524     c                   eval      M_FMEPESO   = apdPKL
016400050524     c                   eval      M_FMEVOLUME = apdVLU
016500061129     c                   eval      M_FMEADAPD  = apdrsf
016600061016      * campi anagrafica automezzi
016700061016     c                   clear                   m_FMEAaut
016800061016     c                   clear                   m_FMEDTIN
016900061017     c                   clear                   m_FMEtkau
017000061016     c     apdpdr        setll     aitra03l
017100061016     c                   do        *hival
017200061016     c     apdpdr        reade     aitra03l
017300061016     c                   if        %eof(aitra03l)
017400061016     c                   leave
017500061016     c                   endif
017600061016      * inserisco il dato se accreditato
017700061127     c                   if        tradfi = 0 or tradfi > dadata
017800061012     c                   movel     traaut        m_FMEAaut
017900061016     c                   z-add     tradin        m_FMEDTIN
018000061017     c                   z-add     trakau        m_FMEtkau
018100061127     c                   z-add     tradfi        m_FMEDTfi
018200061016     c                   endif
018300061016     c                   enddo
018400061012     c                   end
018500061012      *
018600050511     c                   eval      M_FMEDIST   = FLD_Distr
018700050511     c                   eval      M_FMEAREA   = FLD_Area
018800050511     c                   eval      M_FMEPOPER  = FLD_Fgs
018900050511     c                   eval      M_FMEAUTIST = FLD_Padr
019000050905     c                   eval      M_FMSPEDIZ  = FLD_Spediz
019100051111      **
019200051111     c                   clear                   FLD_Ritiri
019300051111     c                   clear                   FLD_Consegne
019400051111      **
019500051111     c                   eval      fttfvl = 'C'
019600051111     c                   eval      fttpdr = APDPDR
019700051111     c                   eval      fttddc = dadata
019800051111      **
019900051111     C     K_FTT         setll     fiftt02l
020000051111     C     K_FTT_Pdr     reade     fiftt02l
020100051111      **
020200051111     c                   dow       not %EoF(fiftt02l)
020300051111      **  se supera la data limite del periodo selezionato
020400051111     c                   if        fttddc > adata
020500051111     c                   leave
020600051111     c                   end
020700051111      **
020800051111     c                   select
020900051111     c                   when      ftttsr = 'C'
021000051111     c                   add       fttSET        FLD_Consegne
021100051111     c                   when      ftttsr = 'R'
021200051111     c                   add       fttSET        FLD_Ritiri
021300051111     c                   endSL
021400051111      **
021500051111     C     K_FTT_Pdr     reade     fiftt02l
021600051111     c                   endDO
021700051111      **
021800051111     c                   eval      M_FMSPERIT  = FLD_Ritiri
021900051111     c                   eval      M_FMSPECON  = FLD_Consegne
022000051111      **
022100050511     c                   eval      M_FMETOTVAL = FLD_TotVal
022200050511     c                   eval      M_FMENGILAV = FLD_GioLav
022300050511     c                   eval      M_FMEMEDGIO = FLD_MedPadr
022400050511     c                   eval      M_FMEDADATA = pasdti
022500050511     c                   eval      M_FMEADATA  = pasdtf
022600050510      *
022700061012     c                   write     WFFM2000
022800050510      *
022900050510     c                   endsr
023000061016     ?*_____________________________________________________________________******
023100050511      *?   Totali da  File di Work
023200061016     ?*_____________________________________________________________________******
023300050510     c     Totalizza     begsr
023400050510      *
023500061012     c                   Open      WFFM200F
023600050511      * ?------------------------------------------------------------------ */
023700050511      **  Con il file Ricavato Precedentemente x Autista è possibile
023800050511      **   totalizzare e ricavare le medie x P.O..
023900050511      **   Con il totale di tutti gli Autisti del P.O. diviso il numero
024000050510      **   stesso degli Autisti del P.O. si ottiene il valore medio
024100050510      **   a giornata del P.O.
024200050511      * ?------------------------------------------------------------------ */
024300050510     C/EXEC SQL
024400050510     C+ Declare B2 Cursor for
024500050510     C+  SELECT FMEDIST, FMEAREA, FMEPOPER,
024600050510     C+  decimal(sum(FMEMEDGIO), 11, 3) as TOT_PO_MED,
024700050510     C+  decimal( COUNT(*), 5, 0) as NUM_AUT_PO,
024800050510     C+  decimal(sum(FMEMEDGIO)/COUNT(*), 11, 3) as MEDIA_PO
024900061012     C+  FROM WFFM200F
025000050510     C+  GROUP BY FMEDIST, FMEAREA, FMEPOPER
025100050510     C+  ORDER BY FMEDIST, FMEAREA, FMEPOPER
025200050510     C/END-EXEC
025300050510      *          apertura cursore
025400050510     C/EXEC SQL
025500050510     C+ OPEN B2
025600050510     C/END-EXEC
025700050510     c                   clear                   almeno_uno
025800050510      **
025900050510     C                   DOU       SqlCod <> 0
026000050510      **         lettura cursore
026100050510     C/EXEC SQL
026200050510     C+ Fetch Next From B2 Into :TOT_Dist, :TOT_Area, :TOT_POper,
026300050510     C+       :TOT_SumMed, :TOT_NumAut, :TOT_MedGiorPO
026400050510     C/END-EXEC
026500050510     C                   SELECT
026600050510     **
026700050510     C                   WHEN      SqlCod = 100
026800050510     c                   leave
026900050510     c*
027000050510     C                   WHEN      SqlCod < 0
027100050510     C                   seton                                        H2
027200050510     c                   goto      fine_SR
027300050510     c*
027400050510     C                   OTHER
027500050510     **
027600050510     c* DETTAGLIO
027700050510     c* riga   dettaglio
027800050510     c                   exsr      Agg_rig_det
027900050510     c*
028000050510     C                   ENDSL
028100050510
028200050510     C                   ENDDO
028300061016
028400061016     c     fine_SR       tag
028500061016
028600050510     C/EXEC SQL
028700050510     C+ Close B2
028800050510     C/END-EXEC
028900050510     c
029000061012     c                   Close     WFFM200F
029100050510      *
029200050510     c                   endsr
029300061016     ?*_____________________________________________________________________******
029400050511      *?   Aggiorna    File di Work
029500061016     ?*_____________________________________________________________________******
029600050511     c     Agg_rig_det   begsr
029700050511      *
029800050511     c                   move      'S'           almeno_uno
029900050511      *
030000061012     C     K_MED         setll     WFFM200F
030100061012     C     K_MED         reade     WFFM200F
030200050511      *
030300061012     c                   dow       not %Eof(WFFM200F)
030400050511      *
030500050511     c                   eval      M_FMENUMAUT = TOT_NumAut
030600050511     c                   eval      M_FMEMEDPO  = TOT_MedGiorPO
030700050511      *
030800061012     c                   update    WFFM2000
030900050511      *
031000061012     C     K_MED         reade     WFFM200F
031100050511     c                   enddo
031200050510      *
031300050510     c                   endsr
031400061016     ?*_____________________________________________________________________******
031500061016      *?   Inserisce righe con codici autisti presenti in anagrafica autisti
031600061016      *?   ma non valorizzati nel periodo estratto
031700061016     ?*_____________________________________________________________________******
031800061016     c     autnoval      begsr
031900061016     c                   Open      WFFM200F
032000061016
032100061016     C/EXEC SQL
032200061016     C+ Declare C3 Cursor for
032300061016     C+  SELECT traaut, trakau, tradin, trafil FROM AITRA00F
032400061017     C+ WHERE traann = ' ' and tradfi = 0 and  tradin < :adata and
032500070129     C+  traris = ' 'and
032600061016     C+  trakau not in (SELECT fmeautist FROM wffm200f)
032700061016     C/END-EXEC
032800061016      *          apertura cursore
032900061016     C/EXEC SQL
033000061016     C+ OPEN C3
033100061016     C/END-EXEC
033200061016     c                   clear                   almeno_uno
033300061016      **
033400061016     C                   DOU       SqlCod <> 0
033500061016      **         lettura cursore
033600061016     C/EXEC SQL
033700061016     C+ Fetch Next From C3 Into :traaut, :trakau, :tradin, :trafil
033800061016     C/END-EXEC
033900061016     C                   SELECT
034000061016     **
034100061016     C                   WHEN      SqlCod = 100
034200061016     c                   leave
034300061016     c*
034400061016     C                   WHEN      SqlCod < 0
034500061016     C                   seton                                        H2
034600061016     c                   goto      fine_SR2
034700061016     c*
034800061016     C                   OTHER
034900061016     **
035000061016     c* DETTAGLIO
035100061016     c* riga   dettaglio
035200061016     c                   exsr      agg_autnoval
035300061016     c*
035400061016     C                   ENDSL
035500061016
035600061016     C                   ENDDO
035700061016
035800061016     c     fine_SR2      tag
035900061016
036000061016     C/EXEC SQL
036100061016     C+ Close C3
036200061016     C/END-EXEC
036300061016     c
036400061016     c                   Close     WFFM200F
036500061016      *
036600061016     c                   endsr
036700061016     ?*_____________________________________________________________________******
036800061016     C     agg_autnoval  BEGSR
036900061016     ?*_____________________________________________________________________******
037000061016      **
037100061016     c                   clear                   WFFM2000
037200061016     c                   movel     traaut        m_FMEAaut
037300061016     c                   z-add     tradin        m_FMEDTIN
037400061017     c                   z-add     trakau        m_FMEtkau
037500061016     c                   z-add     trafil        m_FMEpoper
037600061016     c                   eval      APDTIP = 'A'
037700061016     c                   eval      APDPDR =  trakau
037800061016     c     K_aut         chain     fiapd01l
037900061016     c                   if        %Found(fiapd01l)
038000061016     c                   movel     apdflr        dapdflr
038100061129     c                   eval      M_FMEADAPD  = apdrsf
038200061016     c                   z-add     apdpdr        m_FMEautist
038300061016     c                   endif
038400061016      *
038500061016     c                   write     WFFM2000
038600061016     c                   endsr
038700061127     ?*_____________________________________________________________________******
038800061127      *?   Estrae codici autisti doppi in anagrafica automezzi
038900061127     ?*_____________________________________________________________________******
039000061127     c     autdoppi      begsr
039100061127     c                   Open      WFFM300F
039200061127
039300061127     C/EXEC SQL
039400061127     C+ Declare D4 Cursor for
039500061127     C+ SELECT * FROM aitra01l
039600070125     C+ wHERE tradfi = 0 and traaut <> 'MACCHINA DI SCORTA'
039700070125     C+ and trakau in ( select trakau from (
039800061127     C+ SELECT TRAKAU, count(*) FROM aitra01l WHERE tradfi = 0 GROUP BY
039900061127     C+ trakau HAVING count(*) > 1 )as b   ) and trakau>0
040000061127     C+ ORDER BY trakau
040100061127     C/END-EXEC
040200061127      *          apertura cursore
040300061127     C/EXEC SQL
040400061127     C+ OPEN D4
040500061127     C/END-EXEC
040600061127     c                   clear                   almeno_uno
040700061127      **
040800061127     C                   DOU       SqlCod <> 0
040900061127      **         lettura cursore
041000061127     C/EXEC SQL
041100061127     C+ Fetch Next From D4 Into :aitra00f
041200061127     C/END-EXEC
041300061127     C                   SELECT
041400061127     **
041500061127     C                   WHEN      SqlCod = 100
041600061127     c                   leave
041700061127     c*
041800061127     C                   WHEN      SqlCod < 0
041900061127     C                   seton                                        H3
042000061127     c                   goto      fine_SR3
042100061127     c*
042200061127     C                   OTHER
042300061127     **
042400061127     c* DETTAGLIO
042500061127     c* riga   dettaglio
042600061128     c                   write     wffm3
042700061127     c*
042800061127     C                   ENDSL
042900061127
043000061127     C                   ENDDO
043100061127
043200061127     c     fine_SR3      tag
043300061127
043400061127     C/EXEC SQL
043500061127     C+ Close D4
043600061127     C/END-EXEC
043700061127     c
043800061127     c                   Close     WFFM300F
043900061127      *
044000061127     c                   endsr
044100061016     ?*_____________________________________________________________________******
044200061016     C     *inzsr        BEGSR
044300061016     ?*_____________________________________________________________________******
044400040820      **
044500050511     C     K_MED         KLIST
044600050511     C                   Kfld                    TOT_Dist
044700050511     C                   Kfld                    TOT_Area
044800050511     C                   Kfld                    TOT_POper
044900050524      **
045000050524     C     K_aut         KLIST
045100050524     C                   Kfld                    APDTIP
045200050524     C                   Kfld                    APDPDR
045300051111      **
045400051111     C     K_FTT         KLIST
045500051111     C                   Kfld                    fttPDR
045600051111     C                   Kfld                    fttFVL
045700051111     C                   Kfld                    fttDDC
045800051111      **
045900051111     C     K_FTT_Pdr     KLIST
046000051111     C                   Kfld                    fttPDR
046100051111     C                   Kfld                    fttFVL
046200050511      **
046300040820     C     *ENTRY        PLIST
046400040820     C                   PARM                    KPJBA
046500050510     c                   movel     kpjbu         DsPassaggio
046600050511      **
046700050511     C                   Z-ADD     1             CODUT
046800050511     C                   CALL      'X§PARUT'
046900050511     C                   PARM                    UT§DSE
047000050511     C                   MOVEL     RAGUT         RSUT             20
047100041103      *
047200040820      *  imposta UDATE di elaborazione
047300040820     C                   move      *date         G02DAT
047400040820     C                   MOVE      *ZEROS        G02INV
047500040820     C                   MOVE      *BLANKS       G02ERR
047600040820     C                   CALL      'XSRDA8'
047700040820     C                   PARM                    WLBDAT
047800040820     C                   Z-ADD     G02INV        data_oggi
047900040820     C                   TIME                    W0140            14 0
048000040823      **
048100050510     c                   move      PASdti        dadata            8 0
048200050510     c                   move      PASdtf        adata             8 0
048300040823      **
048400050511     c                   seton                                        66
048500040825     c                   clear                   prima_volta       1
048600040826     c                   clear                   almeno_uno
048700040820     c*
048800061012     c                   Open      WFFM200F
048900050510     c*
049000040820     C                   ENDSR
