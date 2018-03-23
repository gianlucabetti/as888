000100040820     H DECEDIT('0,') DATEDIT(*dmy.)
000200020409     H**********************************************************************
000300041102     fFIEU84f   if   E           k disk
000400041014     fwfieu85w  if a E           k disk    usropn
000500041014     fwFIEU85f  o  a E             disk    usropn
000600040826     fFIEU85P   o    E             PRINTER OFLIND(*in66) usropn
000700150129     fFIEU85P1  o    E             PRINTER OFLIND(*in67) usropn
000800041102      **
000900041102      **  x decodifiche
001000041102     ftitas30C  if   E           k disk
001100041102     ftitaa30C  if   E           k disk
001200041103     fcnaco00F  if   E           k disk
001300150429      **
001400150429      **  x decodifiche particolari DPD (monitorare clienti DANFOSS e VTG
001500150429     ftnCLD00F  if   E           k disk
001600040820     H**********************************************************************
001700040820     d KPJBA         E DS
001800040820     D tnecf00f      e DS
001900040826     D*
002000040825     D  spedbar        s             16
002100040825     D  datspe         s                   like(ecfdfp) INZ(0)
002200040824     D  deltar         s                   like(ecfimr) INZ(0)
002300040824     D  deltac         s                   like(ecfimc) INZ(0)
002400040824     D  porto          s              1    INZ(' ')
002500040824     D*
002600041103     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002700041103     D CNCR80        E DS
002800040823     D fieu84ds      e DS                  inz
002900040901     D  exlgrp                75    104
003000040820     D*
003100040820     D WLBDAT          DS
003200040820     D  G02DAT                 1      8  0
003300040820     D  G02INV                 9     16  0
003400040820     D  G02ERR                17     17
003500040820     D  G02TGI                18     22  0
003600040820      *
003700040825     D WrkSqlCmd       S           1020
003800040825     D Ln_sql          s             68    DIM(15)
003900040824      *
004000150429      * tab. con i Depot identificanti i Clienti da monitorare (solo x DPD)
004100150429     D DPD_DEP         s              4    DIM(50)
004200150429     D DPD_CLI         s              1    DIM(50)
004300150429     D x_Dunfoss       s              1    INZ(' ')
004400150429     D x_VTG           s              1    INZ(' ')
004500150429      *
004600040820     D  data_Oggi      s                   like(G02INV) INZ(0)
004700040820      *
004800041011     D  calrim         s                   like(totri1)
004900041011     D  calrid         s                   like(totrid)
005000040820     D  savdfp         s                   like(ecfdfp)
005100040820     D  savnfp         s                   like(ecfnfp)
005200040824     D  savrif         s                   like(ecfrif)
005300040825     D  savgrp         s                   like(ecfgrp)
005400040825     D  savaas         s                   like(ecfaas)
005500040825     D  savlnp         s                   like(ecflnp)
005600040825     D  savnrs         s                   like(ecfnrs)
005700040825     D  savnsp         s                   like(ecfnsp)
005800040825     D  savsped        s             16
005900040826     D  almeno_uno     s              1
006000040827     D stampa_detta    s              1
006100040831     D det_Spediz      s              1
006200151217     D ciclo_exec      s              2  0 INZ(1)
006300041008     D Salta_det       s              1
006400151126     d dataiso         s               d   datfmt(*iso)
006500040825      *
006600040823     D GRp             S              3    DIM(50) INZ(' ')
006700040823     D IMp             S             16  3 DIM(50) INZ(0)
006800040824     D IMr             S             16  3 DIM(50) INZ(0)
006900040824     D IMc             S             16  3 DIM(50) INZ(0)
007000040824     D IMd             S             16  3 DIM(50) INZ(0)
007100040820      *
007200150129      * Totali x stampa riepilogativa
007300151217     D TSPD            S             11  0 DIM(11) INZ(0)
007400151217     D TBRT            S             15  3 DIM(11) INZ(0)
007500151217     D TPTN            S             15  3 DIM(11) INZ(0)
007600151217     D TDIF            S             15  3 DIM(11) INZ(0)
007700151118      *       RESI
007800151217     D TSPDR           S             11  0 DIM(11) INZ(0)
007900151217     D TBRTR           S             15  3 DIM(11) INZ(0)
008000151217     D TPTNR           S             15  3 DIM(11) INZ(0)
008100151217     D TDIFR           S             15  3 DIM(11) INZ(0)
008200151118      *
008300150429      *    Clienti DPD Particolari a Parte
008400150429      *    DANFOSS
008500150429     D TSPDD           S             11  0 DIM(4) INZ(0)
008600150429     D TBRTD           S             15  3 DIM(4) INZ(0)
008700150429     D TPTND           S             15  3 DIM(4) INZ(0)
008800150429     D TDIFD           S             15  3 DIM(4) INZ(0)
008900150429      *    VTG
009000150429     D TSPDV           S             11  0 DIM(4) INZ(0)
009100150429     D TBRTV           S             15  3 DIM(4) INZ(0)
009200150429     D TPTNV           S             15  3 DIM(4) INZ(0)
009300150429     D TDIFV           S             15  3 DIM(4) INZ(0)
009400150429      *
009500090721     d CMD             S             62    DIM(3) CTDATA PERRCD(1)              clr file
009600090721      *
009700040820     D digits          C                   CONST('0123456789')
009800041014      *
009900071108     D AbbFraImp       C                   CONST('(1) Franchi   Import Incas. ')
010000071108     D AbbFraExp       C                   CONST('(2) Franchi   Export Incas. ')
010100071108     D AbbAssImp       C                   CONST('(3) Assegnati Import Incas. ')
010200071108     D AbbAssExp       C                   CONST('(4) Assegnati Export Incas. ')
010300120305     D NabFraImp       C                   CONST('(5) NON Incasell.BRT Fra.Imp')
010400120305     D NabFraExp       C                   CONST('(6) NON Incasell.BRT Fra.Exp')
010500120305     D NabAssImp       C                   CONST('(7) NON Incasell.BRT Ass.Imp')
010600120305     D NabAssExp       C                   CONST('(8) NON Incasell.BRT Ass.Exp')
010700071108     D NabAddPtn       C                   CONST('(9) NON Incasell.Addeb. PTN.')
010800151217     D OrmItaItaN      C                   CONST('(10)NON Incasell.ORM Ita-Ita')
010900151217     D OrmItaIta       C                   CONST('(11)Assegnati    ORM Ita-Ita')
011000040820      *****************************************************************
011100040824     c* Il ciclo può essere ripetuto 4 volte dovendo generare:
011200040824      *****************************************************************
011300151217     c* volte 1      Import Franchi         S/N        incasellati
011400151217     c*  .... 2      Import Assegnati       S/N        incasellati
011500151217     c*   ... 3      Export Franchi         S/N        incasellati
011600151217     c*    .. 4      Export Assegnati       S/N        incasellati
011700151217      *****************************************************************
011800151217     c* volte 5      Import Franchi         S/N  NON   incasellati
011900151217     c*  .... 6      Import Assegnati       S/N  NON   incasellati
012000151217     c*   ... 7      Export Franchi         S/N  NON   incasellati
012100151217     c*    .. 8      Export Assegnati       S/N  NON   incasellati
012200040824     c*****************************************************************
012300151217     c*    .. 9      solo del Partner            NON   incasellati
012400151217     c*    ..10      solo del Partner   ORM ITA  NON   incasellati
012500151217     c*    ..11      solo del Partner   ORM ITA        incasellati
012600151217     c*****************************************************************
012700040903     c*  Ciclo principale
012800040903     c                   exsr      ciclo_1
012900040903     c*  Ciclo secondario
013000040903     c                   if        e84sna <> 'E' or e84sni <> 'E'
013100040903     c                   exsr      ciclo_2
013200040903     c                   end
013300040903      *
013400150129      *  stampa di Riepilogo finale
013500150129     c                   open      FIEU85P1
013600150129     c                   exsr      imposta_Riep
013700150129     c                   write     pagRIE
013800150429     c                   if        E84NTW = 'DPD'
013900150429     c                   write     pagCLIDPD
014000150429     c                   end
014100150205     c                   close     FIEU85P1
014200150129      *
014300020326     C                   SETON                                        LR
014400040820     C**************************************************************************
014500040820     C* INIZIALIZZAZIONE
014600040820     C**************************************************************************
014700040820     C     *inzsr        BEGSR
014800040820      **
014900040820     C     *ENTRY        PLIST
015000040820     C                   PARM                    KPJBA
015100040823     c                   movel     kpjbu         fieu84ds
015200040820      *
015300041103     C                   Z-ADD     1             CODUT
015400041103     C                   CALL      'X§PARUT'
015500041103     C                   PARM                    UT§DSE
015600041103     C                   MOVEL     REC80         CNCR80
015700041103     C                   MOVEL     RAGUT         RSUT             20
015800041103     C*
015900040820      *  imposta UDATE di elaborazione
016000040820     C                   move      *date         G02DAT
016100040820     C                   MOVE      *ZEROS        G02INV
016200040820     C                   MOVE      *BLANKS       G02ERR
016300040820     C                   CALL      'XSRDA8'
016400040820     C                   PARM                    WLBDAT
016500040820     C                   Z-ADD     G02INV        data_oggi
016600040820     C                   move      data_oggi     day
016700040820     C                   TIME                    W0140            14 0
016800040820     C                   MOVEl     W0140         ore
016900040901      **
017000041015     c                   clear                   tipo             15
017100040823     c                   eval      partner = e84ntw
017200150504      **
017300150504     c                   setoff                                       44
017400150504     c                   if        E84NTW = 'DPD'
017500150504     c                   setoN                                        44
017600150504     c                   end
017700150504      **
017800040901      **  x incasellamento
017900040901     c                   if        e84dii <> *zero
018000040901     c                   move      e84dii        dadata
018100040901     c                   move      e84dfi        adata
018200041103     c                   move(p)   'Incasellate' periodo
018300041015     c                   eval      tipo = 'Incasellamento'
018400040824     c                   end
018500040901      **  x spedizione
018600040901     c                   if        e84dis <> *zero
018700040901     c                   move      e84dis        dadata
018800040901     c                   move      e84dfs        adata
018900041103     c                   move(p)   'Spedizioni'  periodo
019000041015     c                   eval      tipo = 'Spedizione   '
019100040820     c                   end
019200040901      **  x fattura
019300040901     c                   if        e84dif <> *zero
019400040901     c                   move      e84dif        dadata
019500040901     c                   move      e84dff        adata
019600041103     c                   move(p)   'Fatturate '  periodo
019700041015     c                   eval      tipo = 'Fattura      '
019800040901     c                   end
019900040824      **  prima pagina
020000040831     c                   eval      det_spediz = e84det
020100040831     c                   eval      totf   = e84tof
020200040824     c                   eval      totr   = e84tor
020300040824     c                   eval      tots   = e84tos
020400040825     c                   eval      totp   = e84top
020500040824     c                   eval      spbnoa = e84sna
020600040824     c                   eval      spbnoi = e84sni
020700040824     c                   eval      delta  = e84sde
020800040824     c                   eval      filepc = e84fpc
020900040827     c                   eval      grp1   = e84gr1
021000040827     c                   eval      grp2   = e84gr2
021100040827     c                   eval      grp3   = e84gr3
021200040827     c                   eval      grp4   = e84gr4
021300040827     c                   eval      grp5   = e84gr5
021400040827     c                   eval      grp6   = e84gr6
021500040827     c                   eval      grp7   = e84gr7
021600040827     c                   eval      grp8   = e84gr8
021700040827     c                   eval      grp9   = e84gr9
021800040827     c                   eval      grp0   = e84gr0
021900040827     c                   if        e84gie = 'I'
022000040827     c                   eval      gried  = 'Inclusi'
022100040827     c                   else
022200040827     c                   eval      gried  = 'Esclusi'
022300040827     c                   end
022400040824      **
022500150203      **  se deve stampare solo la pagina di riepilogo  e non stampare
022600150203      **   tutte le altre 9 stampe di dettaglio
022700150203     c     e84rie        comp      'S'                                    22
022800150203      **
022900040826      **  se deve creare il file PC
023000040826     c                   if        e84fpc = 'S'
023100090721      **
023200040826     C                   z-add     62            lung             15 5
023300040826     C                   clear                   comman
023400040826     C                   movea     cmd(2)        comman           80
023500040826     C                   call      'QCMDEXC'
023600040826     C                   parm                    comman
023700040826     C                   parm                    lung
023800041014     c                   open      wfieu85f
023900090721      **
024000090721      **  DEVE RESTITUIRE  i totali fattura Partner in modo da avere la certezza
024100090721      **    che tutti i dati siano stati importati sul file di esportazione
024200090721      **    e procedere correttamente con le verifiche.
024300090721      **
024400090901     c                   if        E84NTW = 'DPD'
024500090721     C                   z-add     62            lung             15 5
024600090721     C                   clear                   comman
024700090721     C                   movea     cmd(3)        comman           80
024800090721     C                   call      'QCMDEXC'
024900090721     C                   parm                    comman
025000090721     C                   parm                    lung
025100090901     c                   end
025200090721      **
025300040826     c                   end
025400040826      **
025500150429      **    Solo per DPD occorre pre-caricare la schiera dei clienti Particolari
025600150429      **     da monitorare (DANFOSS e VTG)
025700150429      **    Imposta due schiere con il depot nella prima schiera ed in parallelo
025800150429      **    con la D=DANFOSS e V=VTG sulla seconda schiera leggendo dal TNCLD00F
025900150429     c                   if        E84NTW = 'DPD'
026000150429      **
026100150429     c                   z-add     0             nrCLD             3 0
026200150429     c                   clear                   DPD_DEP
026300150429     c                   clear                   DPD_CLI
026400150429      **
026500150429      **  max. 50 depot da monitorare
026600150429     c                   read      tnCLD00f
026700150429     c                   dow       not %EoF(tnCLD00f) and nrCLD < 50
026800150429      **
026900150429     c                   add       1             nrCLD
027000150429     c                   if        cldCOD = 'DANFOSS'
027100150429     c                   eval      DPD_DEP(nrCLD) =  %editc(cldDEP:'X')
027200150429     c                   eval      DPD_CLI(nrCLD) = 'D'
027300150429     c                   elseif    cldCOD = 'VTG'
027400150429     c                   eval      DPD_DEP(nrCLD) =  %editc(cldDEP:'X')
027500150429     c                   eval      DPD_CLI(nrCLD) = 'V'
027600150429     c                   end
027700150429      **
027800150429     c                   read      tnCLD00f
027900150429     c                   enddo
028000150429      **
028100150429     c                   end
028200150429      **
028300040820     C                   ENDSR
028400040824     C**************************************************************************
028500041007     C* Stampa la testata con il salto a nuova pagina
028600040824     C**************************************************************************
028700041007     C     Prt_testa     BEGSR                                                  *
028800040824      **
028900041007     c                   add       1             pagnum
029000150203     c  n22              write     testa
029100041007      **
029200041007     C                   ENDSR
029300041007     C**************************************************************************
029400041007     C* Ciclo Principale
029500041007     C**************************************************************************
029600041007     C     Ciclo_1       BEGSR                                                  *
029700041007      **
029800040903     c                   DO        4             VOLTE             3 0
029900150129     c                   z-add     VOLTE         vlt               3 0
030000041007     c                   z-add     0             pagnum
030100040903     c*
030200040903     c* se non richiesta evita il giro
030300040903     c                   if        volte = 1 and e84if ='N' or                  Fr.Import
030400040903     c                             volte = 2 and e84ia ='N' or                  As.Import
030500040903     c                             volte = 3 and e84ef ='N' or                  Fr.Export
030600040903     c                             volte = 4 and e84ea ='N'                     As.Export
030700040903     c                   iter
030800040903     c                   end
030900040903     c*
031000040903     c*   Apre il file di stampa e predispone a stampare testata con 66 = On
031100040903     c                   open      fieu85p
031200040903     c* compongo istruzione sql
031300040903     c                   exsr      istruz
031400040903     c*  stampa prima pagina
031500040903     c                   exsr      pripag
031600041007     c                   seton                                        66
031700041011     c*
031800151126     c* Attenzione: solo se selezionato una (%) di delta
031900041011     c* esegue un ciclo precedentemente per vedere quali spedizioni non stampare poichè
032000041011     c* non presenti entro i limiti richiesti.    --> CREA un WORKFILE con le Spedizioni.
032100041011     c                   if         e84sde <> 99
032200041011     c                   exsr      pre_ciclo
032300041011     c                   end
032400040903     c*
032500040903     C/EXEC SQL
032600040903     C+ PREPARE S1 FROM :WrkSqlCmd
032700040903     C/END-EXEC
032800040903
032900040903     C/EXEC SQL
033000040903     C+ DECLARE A1 CURSOR FOR S1
033100040903     C/END-EXEC
033200040903
033300040903     C/EXEC SQL
033400040903     C+ OPEN A1
033500040903     C/END-EXEC
033600041011     c                   clear                   almeno_uno
033700040903     C                   DOU       SqlCod <> 0
033800040903     C/EXEC SQL
033900040903     C+ FETCH NEXT FROM A1 INTO :tnecf00f, :spedbar, :datspe, :deltar,
034000040903     C+ :deltac, :porto
034100040903     C/END-EXEC
034200040903     C                   SELECT
034300041007     **
034400041007     C                   WHEN      SqlCod = 100
034500041007     c* a fine file
034600041007     c* Totali x rotture
034700041007     c                   if        almeno_uno = 'S'
034800041007     c                   exsr      TOTals
034900041007     c                   else
035000041007      *                 * ------------------ *
035100150203     c  n22              write     NODATI
035200041007      *                 * ------------------ *
035300041007     c                   end
035400041007     c                   leave
035500041007     c*
035600041007     C                   WHEN      SqlCod < 0
035700041007     C                   seton                                        H1
035800041007     c                   goto      fine_ciclo
035900040903     c*
036000041007     C                   OTHER
036100041011     **
036200041011     c* Controlla se richiesta stampa x Spedizione con limite di DELTA %
036300041011     c*  se la spedizione deve essere saltata.
036400041011     c                   eval      salta_det = 'S'
036500041011     **
036600041011     c                   if        e84sde <> 99
036700041014     c     spedbar       chain     wfieu85w
036800041014     c                   if        %found(wfieu85w)
036900041011     c                   eval      salta_det = 'N'
037000041011     c                   end
037100041011     c                   end
037200040903     **
037300040903     c* Totali x rotture
037400040903     c                   exsr      TOTals
037500040903     c* DETTAGLIO
037600040903     c                   if        *in66
037700040903      *                 * ------------------ *
037800041007     c                   exsr      Prt_testa
037900041007      *                 * ------------------ *
038000150203     c  n22              write     tesdetta
038100040903      *                 * ------------------ *
038200040903     c                   eval      prima_volta = 'N'
038300040903     c                   setoff                                       66
038400040903     c                   end
038500040903      **
038600040903      ** se non deve stampare il dettaglio:
038700040903      **    x spedizioni non addebitate                (quindi totalizzarle a parte)
038800040903      **    x spedizioni addebitate ma non incasellate (quindi totalizzarle a parte)
038900040903     c                   eval      stampa_detta = 'S'
039000040903      **
039100040903     c                   if         (e84sna <>'S' and
039200130318     c                                 ecfdfp = 0)
039300040903     c                             or
039400041011     c                              (e84sni <>'S' and
039500041012     c                               ecfdft = 0)
039600040903     c                             or
039700040903     c                              (e84det = 'N')
039800041008     c                             or
039900041011     c                              (salta_det = 'S' and e84sde <> 99 and
040000041011     c                               e84det = 'S')
040100041008      **
040200040903     c                   eval      stampa_detta = 'N'
040300040903     c                   end
040400040903     c* stampa dettaglio
040500040903     c                   exsr      imp_rig_det
040600040903      **
040700040903      **   abilita esclusivamente la stampa della riga di dettaglio
040800040903     c                   if        stampa_detta = 'S'
040900150203     c  n22              write     dettagli
041000040903     c                   end
041100040903     c* Totalizzo
041200040903     c                   exsr      totalizza
041300040903     c*
041400040903     C                   ENDSL
041500040903
041600040903     C                   ENDDO
041700040903     C/EXEC SQL
041800040903     C+ CLOSE A1
041900040903     C/END-EXEC
042000040903     c*
042100040903     C* totali generali
042200041007     c                   if        almeno_uno = 'S'
042300040903     c                   if        *in66
042400040903      *                 * ------------------ *
042500041007     c                   exsr      Prt_testa
042600041007      *                 * ------------------ *
042700150203     c  n22              write     tesdetta
042800040903      *                 * ------------------ *
042900040903     c                   setoff                                       66
043000040903     c                   end
043100150203     c  n22              write     totale
043200041007     c                   end
043300041007      *
043400041007     c     fine_ciclo    tag
043500041007      *
043600040903     c                   clear                   totgep
043700040903     c                   clear                   totge1
043800040903     c                   clear                   totge2
043900040903     c                   clear                   totged
044000040903     c                   close     fieu85p
044100040903      *
044200040903     c                   ENDDO
044300040903      **
044400040903     C                   ENDSR
044500040903     C**************************************************************************
044600040903     C* Ciclo Spedizioni anomale stampate a parte
044700040903     C*  addebitate ma non associate e Non Addebitate
044800040903     C**************************************************************************
044900040903     C     Ciclo_2       BEGSR                                                  *
045000040903      **
045100151126      **    x le NON incasellate cerca la data più antica
045200151126     c                   clear                   data_MINIMA       8 0
045300151126      **
045400040903      **  Addebitate e non Associate
045500040903     c                   if        e84sni <> 'E'
045600040903     c                   eval      ciclo_exec = 2
045700040903     c                   exsr      Addeb_No_Assoc
045800151217     c                   eval      ciclo_exec = 10
045900151217     c                   exsr      ORM_ITA_ITA_N
046000151217     c                   eval      ciclo_exec = 11
046100151217     c                   exsr      ORM_ITA_ITA
046200040903     c                   end
046300040903      **
046400040903      **  Non Addebitate
046500040903     c                   if        e84sna <> 'E'
046600040903     c                   eval      ciclo_exec = 3
046700040903     c                   exsr      Non_Addebitate
046800040903     c                   end
046900040903      **
047000040903     C                   ENDSR
047100040903     C**************************************************************************
047200040903     C* Addebitate dal Partner ma non associate
047300040903     C**************************************************************************
047400040903     C     Addeb_No_AssocBEGSR
047500040903      **
047600040903     c*   Apre il file di stampa e predispone a stampare testata con 66 = On
047700040903     c                   open      fieu85p
047800150129     c                   z-add     9             vlt
047900041007     c                   z-add     0             pagnum
048000040903     c* compongo istruzione sql
048100040903     c                   exsr      istruz2
048200040903     c*  stampa prima pagina
048300040903     c                   exsr      pripag
048400041007     c                   seton                                        66
048500040903     C/EXEC SQL
048600040903     C+ PREPARE S2 FROM :WrkSqlCmd
048700040903     C/END-EXEC
048800040903
048900040903     C/EXEC SQL
049000040903     C+ DECLARE B1 CURSOR FOR S2
049100040903     C/END-EXEC
049200040903
049300040903     C/EXEC SQL
049400040903     C+ OPEN B1
049500040903     C/END-EXEC
049600041011     c                   clear                   almeno_uno
049700040903     C                   DOU       SqlCod <> 0
049800040903     C/EXEC SQL
049900040903     C+ FETCH NEXT FROM B1 INTO :tnecf00f, :spedbar, :datspe, :deltar,
050000040903     C+ :deltac, :porto
050100040903     C/END-EXEC
050200040903     C                   SELECT
050300041007     c*
050400041007     C                   WHEN      SqlCod = 100
050500041007     c* a fine file
050600041007     c* Totali x rotture
050700041007     c                   if        almeno_uno = 'S'
050800041007     c                   exsr      TOTals
050900041007     c                   else
051000041007      *                 * ------------------ *
051100150203     c  n22              write     tesdetta
051200041007      *                 * ------------------ *
051300150203     c  n22              write     NODATI
051400041007     c                   end
051500041007     c                   leave
051600041007     **
051700041007     C                   WHEN      SqlCod < 0
051800041007     C                   seton                                        H2
051900041007     c                   goto      fine_ciclo2
052000040903     c*
052100041007     C                   OTHER
052200040903     c* Totali x rotture
052300040903     c                   exsr      TOTals
052400040903     c* DETTAGLIO
052500040903     c                   if        *in66
052600040903      *                 * ------------------ *
052700041007     c                   exsr      Prt_testa
052800041007      *                 * ------------------ *
052900150203     c  n22              write     tesdetta
053000040903      *                 * ------------------ *
053100040903     c                   eval      prima_volta = 'N'
053200040903     c                   setoff                                       66
053300040903     c                   end
053400040903      **
053500040903      ** se non deve stampare il dettaglio quindi solo i totali
053600040903      **    x spedizioni addebitate ma non incasellate
053700040903     c                   eval      stampa_detta = 'S'
053800040903     c                   if         (e84sni <>'S' and ecfdfp > 0)
053900040903     c                   eval      stampa_detta = 'N'
054000040903     c                   end
054100040903     c* stampa dettaglio
054200040903     c                   exsr      imp_rig_det
054300040903      **
054400040903      **   abilita esclusivamente la stampa della riga di dettaglio
054500040903     c                   if        stampa_detta = 'S'
054600150203     c  n22              write     dettagli
054700040903     c                   end
054800040903     c* Totalizzo
054900040903     c                   exsr      totalizza
055000041014     c                   exsr      tot_A_Parte
055100040903     c*
055200040903     C                   ENDSL
055300040903
055400040903     C                   ENDDO
055500040903     C/EXEC SQL
055600040903     C+ CLOSE B1
055700040903     C/END-EXEC
055800040903     C* totali di riepilogo
055900041007     c                   if        almeno_uno = 'S'
056000040903     c                   exsr      tot_riepilogo
056100041007     c                   end
056200041007      ***
056300041007     c     fine_ciclo2   tag
056400040903     c                   clear                   totgep
056500040903     c                   clear                   totge1
056600040903     c                   clear                   totge2
056700040903     c                   clear                   totged
056800040903     c                   close     fieu85p
056900040903      **
057000040903     C                   ENDSR
057100150325     C**************************************************************************
057200150325     C* Accreditate dal Partner ma non associate perchè ORM Italia-Italia
057300150325     C*   quindi la LNP o la LNA non hanno linee PTN.
057400150325     C**************************************************************************
057500151217     C     ORM_ITA_ITA_N BEGSR
057600150325      **
057700150325     c*   Apre il file di stampa e predispone a stampare testata con 66 = On
057800150325     c                   open      fieu85p
057900150325     c                   z-add     10            vlt
058000150325     c                   z-add     0             pagnum
058100150325     c* compongo istruzione sql
058200151217     c                   exsr      istruz2_ORM_N
058300150325     c*  stampa prima pagina
058400150325     c                   exsr      pripag
058500150325     c                   seton                                        66
058600150325     C/EXEC SQL
058700150325     C+ PREPARE O2 FROM :WrkSqlCmd
058800150325     C/END-EXEC
058900150325
059000150325     C/EXEC SQL
059100150325     C+ DECLARE O1 CURSOR FOR O2
059200150325     C/END-EXEC
059300150325
059400150325     C/EXEC SQL
059500150325     C+ OPEN O1
059600150325     C/END-EXEC
059700150325     c                   clear                   almeno_uno
059800150325     C                   DOU       SqlCod <> 0
059900150325     C/EXEC SQL
060000150325     C+ FETCH NEXT FROM O1 INTO :tnecf00f, :spedbar, :datspe, :deltar,
060100150325     C+ :deltac, :porto
060200150325     C/END-EXEC
060300150325     C                   SELECT
060400150325     c*
060500150325     C                   WHEN      SqlCod = 100
060600150325     c* a fine file
060700150325     c* Totali x rotture
060800150325     c                   if        almeno_uno = 'S'
060900150325     c                   exsr      TOTals
061000150325     c                   else
061100150325      *                 * ------------------ *
061200150325     c  n22              write     tesdetta
061300150325      *                 * ------------------ *
061400150325     c  n22              write     NODATI
061500150325     c                   end
061600150325     c                   leave
061700150325     **
061800150325     C                   WHEN      SqlCod < 0
061900150325     C                   seton                                        H2
062000151217     c                   goto      fine_ciclo2ORM
062100150325     c*
062200150325     C                   OTHER
062300150325     c* Totali x rotture
062400150325     c                   exsr      TOTals
062500150325     c* DETTAGLIO
062600150325     c                   if        *in66
062700150325      *                 * ------------------ *
062800150325     c                   exsr      Prt_testa
062900150325      *                 * ------------------ *
063000150325     c  n22              write     tesdetta
063100150325      *                 * ------------------ *
063200150325     c                   eval      prima_volta = 'N'
063300150325     c                   setoff                                       66
063400150325     c                   end
063500150325      **
063600150325      ** se non deve stampare il dettaglio quindi solo i totali
063700150325      **    x spedizioni addebitate ma non incasellate
063800150325     c                   eval      stampa_detta = 'S'
063900150325     c                   if         (e84sni <>'S' and ecfdfp > 0)
064000150325     c                   eval      stampa_detta = 'N'
064100150325     c                   end
064200150325     c* stampa dettaglio
064300150325     c                   exsr      imp_rig_det
064400150325      **
064500150325      **   abilita esclusivamente la stampa della riga di dettaglio
064600150325     c                   if        stampa_detta = 'S'
064700150325     c  n22              write     dettagli
064800150325     c                   end
064900150325     c* Totalizzo
065000150325     c                   exsr      totalizza
065100150325     c                   exsr      tot_A_Parte
065200150325     c*
065300150325     C                   ENDSL
065400150325
065500150325     C                   ENDDO
065600150325     C/EXEC SQL
065700150325     C+ CLOSE O1
065800150325     C/END-EXEC
065900150325     C* totali di riepilogo
066000150325     c                   if        almeno_uno = 'S'
066100150325     c                   exsr      tot_riepilogo
066200150325     c                   end
066300150325      ***
066400150325     c     fine_ciclo2ORMtag
066500150325     c                   clear                   totgep
066600150325     c                   clear                   totge1
066700150325     c                   clear                   totge2
066800150325     c                   clear                   totged
066900150325     c                   close     fieu85p
067000150325      **
067100150325     C                   ENDSR
067200151217     C**************************************************************************
067300151217     C* Accreditate dal Partner e associate a BOLLE ITA-ITA da ORM IMPORT
067400151217     C*   quindi la LNP o la LNA non hanno linee PTN.
067500151217     C**************************************************************************
067600151217     C     ORM_ITA_ITA   BEGSR
067700151217      **
067800151217     c*   Apre il file di stampa e predispone a stampare testata con 66 = On
067900151217     c                   open      fieu85p
068000151217     c                   z-add     11            vlt
068100151217     c                   z-add     0             pagnum
068200151217     c* compongo istruzione sql
068300151217     c                   exsr      istruz2_ORM
068400151217     c*  stampa prima pagina
068500151217     c                   exsr      pripag
068600151217     c                   seton                                        66
068700151218     c                   eval      tipo_confr = 'R'
068800151217     C/EXEC SQL
068900151217     C+ PREPARE O4 FROM :WrkSqlCmd
069000151217     C/END-EXEC
069100151217
069200151217     C/EXEC SQL
069300151217     C+ DECLARE O3 CURSOR FOR O4
069400151217     C/END-EXEC
069500151217
069600151217     C/EXEC SQL
069700151217     C+ OPEN O3
069800151217     C/END-EXEC
069900151217     c                   clear                   almeno_uno
070000151217     C                   DOU       SqlCod <> 0
070100151217     C/EXEC SQL
070200151217     C+ FETCH NEXT FROM O3 INTO :tnecf00f, :spedbar, :datspe, :deltar,
070300151217     C+ :deltac, :porto
070400151217     C/END-EXEC
070500151217     C                   SELECT
070600151217     c*
070700151217     C                   WHEN      SqlCod = 100
070800151217     c* a fine file
070900151217     c* Totali x rotture
071000151217     c                   if        almeno_uno = 'S'
071100151217     c                   exsr      TOTals
071200151217     c                   else
071300151217      *                 * ------------------ *
071400151217     c  n22              write     tesdetta
071500151217      *                 * ------------------ *
071600151217     c  n22              write     NODATI
071700151217     c                   end
071800151217     c                   leave
071900151217     **
072000151217     C                   WHEN      SqlCod < 0
072100151217     C                   seton                                        H2
072200151217     c                   goto      fine_ciclo2ITA
072300151217     c*
072400151217     C                   OTHER
072500151217     c* Totali x rotture
072600151217     c                   exsr      TOTals
072700151217     c* DETTAGLIO
072800151217     c                   if        *in66
072900151217      *                 * ------------------ *
073000151217     c                   exsr      Prt_testa
073100151217      *                 * ------------------ *
073200151217     c  n22              write     tesdetta
073300151217      *                 * ------------------ *
073400151217     c                   eval      prima_volta = 'N'
073500151217     c                   setoff                                       66
073600151217     c                   end
073700151217      **
073800151217      ** se non deve stampare il dettaglio quindi solo i totali
073900151217      **    x spedizioni addebitate ma non incasellate
074000151217     c                   eval      stampa_detta = 'S'
074100151217     c                   if         (e84sni <>'S' and ecfdfp > 0)
074200151217     c                   eval      stampa_detta = 'N'
074300151217     c                   end
074400151217     c* stampa dettaglio
074500151217     c                   exsr      imp_rig_det
074600151217      **
074700151217      **   abilita esclusivamente la stampa della riga di dettaglio
074800151217     c                   if        stampa_detta = 'S'
074900151217     c  n22              write     dettagli
075000151217     c                   end
075100151217     c* Totalizzo
075200151217     c                   exsr      totalizza
075300151217     c                   exsr      tot_A_Parte
075400151217     c*
075500151217     C                   ENDSL
075600151217
075700151217     C                   ENDDO
075800151217     C/EXEC SQL
075900151217     C+ CLOSE O3
076000151217     C/END-EXEC
076100151217     C* totali di riepilogo
076200151217     c                   if        almeno_uno = 'S'
076300151217     c                   exsr      tot_riepilogo
076400151217     c                   end
076500151217      ***
076600151217     c     fine_ciclo2ITAtag
076700151217     c                   clear                   totgep
076800151217     c                   clear                   totge1
076900151217     c                   clear                   totge2
077000151217     c                   clear                   totged
077100151217     c                   close     fieu85p
077200151217      **
077300151217     C                   ENDSR
077400040903     C**************************************************************************
077500040903     C* Non Addebitate dal Partner
077600040903     C**************************************************************************
077700040903     C     Non_AddebitateBEGSR                                                  *
077800040903      **
077900040903      **    Non Addebitate ma solo nostre spedizioni
078000040903     c                   DO        4             VOLTE             3 0
078100150129     c     VOLTE         add       4             vlt
078200041007     c                   z-add     0             pagnum
078300040903     c*
078400040903     c* se non richiesta evita il giro
078500040903     c                   if        volte = 1 and e84if ='N' or                  Fr.Import
078600040903     c                             volte = 2 and e84ia ='N' or                  As.Import
078700040903     c                             volte = 3 and e84ef ='N' or                  Fr.Export
078800040903     c                             volte = 4 and e84ea ='N'                     As.Export
078900040903     c                   iter
079000040903     c                   end
079100040903     c*
079200040903     c*   Apre il file di stampa e predispone a stampare testata con 66 = On
079300040903     c                   open      fieu85p
079400040903     c* compongo istruzione sql
079500040903     c                   exsr      istruz3
079600040903     c*  stampa prima pagina
079700040903     c                   exsr      pripag
079800041007     c                   seton                                        66
079900040903     C/EXEC SQL
080000040903     C+ PREPARE S3 FROM :WrkSqlCmd
080100040903     C/END-EXEC
080200040903
080300040903     C/EXEC SQL
080400040903     C+ DECLARE C1 CURSOR FOR S3
080500040903     C/END-EXEC
080600040903
080700040903     C/EXEC SQL
080800040903     C+ OPEN C1
080900040903     C/END-EXEC
081000041011     c                   clear                   almeno_uno
081100040903     C                   DOU       SqlCod <> 0
081200040903     C/EXEC SQL
081300040903     C+ FETCH NEXT FROM C1 INTO :tnecf00f, :spedbar, :datspe, :deltar,
081400040903     C+ :deltac, :porto
081500040903     C/END-EXEC
081600040903     C                   SELECT
081700041007     c*
081800041007     C                   WHEN      SqlCod = 100
081900041007     c* a fine file
082000041007     c* Totali x rotture
082100041007     c                   if        almeno_uno = 'S'
082200041007     c                   exsr      TOTals
082300041007     c                   else
082400041007      *                 * ------------------ *
082500150203     c  n22              write     tesdetta
082600041007      *                 * ------------------ *
082700150203     c  n22              write     NODATI
082800041007     c                   end
082900041007     c                   leave
083000041007     **
083100041007     C                   WHEN      SqlCod < 0
083200041007     C                   seton                                        H3
083300041007     c                   goto      fine_ciclo3
083400040903     c*
083500041007     C                   OTHER
083600040903     c* Totali x rotture
083700040903     c                   exsr      TOTals
083800040903     c* DETTAGLIO
083900040903     c                   if        *in66
084000040903      *                 * ------------------ *
084100041007     c                   exsr      Prt_testa
084200041007      *                 * ------------------ *
084300150203     c  n22              write     tesdetta
084400040903      *                 * ------------------ *
084500040903     c                   eval      prima_volta = 'N'
084600040903     c                   setoff                                       66
084700040903     c                   end
084800040903      **
084900040903      ** se non deve stampare il dettaglio quindi solo i totali
085000040903      **    x spedizioni non addebitate
085100040903     c                   eval      stampa_detta = 'S'
085200040903     c                   if         (e84sna <>'S' and
085300130318     c                                 ecfdfp = 0)
085400040903     c                   eval      stampa_detta = 'N'
085500040903     c                   end
085600040903     c* stampa dettaglio
085700040903     c                   exsr      imp_rig_det
085800040903      **
085900040903      **   abilita esclusivamente la stampa della riga di dettaglio
086000040903     c                   if        stampa_detta = 'S'
086100150203     c  n22              write     dettagli
086200040903     c                   end
086300040903     c* Totalizzo
086400040903     c                   exsr      totalizza
086500041014     c                   exsr      tot_A_Parte
086600040903     c*
086700040903     C                   ENDSL
086800040903
086900040903     C                   ENDDO
087000040903     C/EXEC SQL
087100040903     C+ CLOSE C1
087200040903     C/END-EXEC
087300040903     C* totali di riepilogo
087400041007     c                   if        almeno_uno = 'S'
087500040903     c                   exsr      tot_riepilogo
087600041007     c                   end
087700041007      *
087800041007     c     fine_ciclo3   tag
087900041007      *
088000040903     c                   clear                   totgep
088100040903     c                   clear                   totge1
088200040903     c                   clear                   totge2
088300040903     c                   clear                   totged
088400040903     c                   close     fieu85p
088500040903      *
088600040903     c                   ENDDO
088700040903      **
088800040903     C                   ENDSR
088900040903     C**************************************************************************
089000040903     C* prima pagina
089100040903     C**************************************************************************
089200040903     C     pripag        BEGSR                                                  *
089300040903      **
089400040824     c                   movea     '0000'        *in(1)
089500040825     c                   movea     '0000'        *in(11)
089600040824     c                   eval      *in(volte) = *on
089700040826      **
089800040826      ** Descrizioni varie di testata
089900041013     c                   clear                   desvari
090000040826     c                   select
090100040826     c                   when      volte = 1
090200040826     c                   eval      desvari = 'Import Franchi'
090300040826     c                   when      volte = 2
090400040826     c                   eval      desvari = 'Import Assegnati'
090500040826     c                   when      volte = 3
090600040826     c                   eval      desvari = 'Export Franchi'
090700040826     c                   when      volte = 4
090800040826     c                   eval      desvari = 'Export Assegnati'
090900040826     c                   endsl
091000040826      **
091100040903     c                   clear                   desprt
091200040903     c                   select
091300040903     c                   when      ciclo_exec = 1
091400040903     c                   eval      desprt = 'Addebitate e Associate' +
091500120305     c                             ' a sped.   B R T '
091600040903     c                   when      ciclo_exec = 2
091700040903     c                   eval      desprt = 'Addebitate e NON Associate'+
091800120305     c                             ' a sped.   B R T '
091900040903     c                   when      ciclo_exec = 3
092000040903     c                   eval      desprt = 'Non Addebitate dal Partner'
092100040903     c                   endsl
092200040903      **
092300040825     c                   select
092400040825     c                   when      totf = 'S'
092500040825     c                   seton                                        11
092600040826     c                   eval      desvari = %TrimR(desvari) +
092700040826     c                             '        Totali x Fattura'
092800040825     c                   when      totr = 'S'
092900040825     c                   seton                                        12
093000040826     c                   eval      desvari = %TrimR(desvari) +
093100040826     c                             '        Totali x Gruppo'
093200040825     c                   when      tots = 'S'
093300040825     c                   seton                                        13
093400040826     c                   eval      desvari = %TrimR(desvari) +
093500040826     c                             '        Totali x Spedizione'
093600040825     c                   when      totp = 'S'
093700040825     c                   seton                                        14
093800040826     c                   eval      desvari = %TrimR(desvari) +
093900040826     c                             '        Totali x Riferimento'
094000040825     c                   endsl
094100040824      **
094200040825     c                   clear                   prima_volta       1
094300040825     c                   clear                   tipo_confr        1
094400040826      **
094500040824      ** confronto con Competenze
094600040824     c                   if        volte = 1 and e84ifc = 'X' or
094700040824     c                             volte = 2 and e84iac = 'X' or
094800040824     c                             volte = 3 and e84efc = 'X' or
094900040824     c                             volte = 4 and e84eac = 'X'
095000040824     c                   eval      tipo_confr = 'C'
095100040825     c                   eval      ifdes  = 'Competenze'
095200040825     c                   eval      iades  = 'Competenze'
095300040825     c                   eval      efdes  = 'Competenze'
095400040825     c                   eval      eades  = 'Competenze'
095500040825     c                   eval      desim1 = 'Competenze'
095600041011     c                   move      'Diffe.Comp'  desdel
095700040824     c                   end
095800040824      **
095900040824      ** confronto con Ricavi
096000040824     c                   if        volte = 1 and e84ifr = 'X' or
096100040824     c                             volte = 2 and e84iar = 'X' or
096200040824     c                             volte = 3 and e84efr = 'X' or
096300040824     c                             volte = 4 and e84ear = 'X'
096400040824     c                   eval      tipo_confr = 'R'
096500040825     c                   eval      ifdes  = '    Ricavi'
096600040825     c                   eval      iades  = '    Ricavi'
096700040825     c                   eval      efdes  = '    Ricavi'
096800040825     c                   eval      eades  = '    Ricavi'
096900040825     c                   eval      desim1 = '    Ricavi'
097000041011     c                   move      'Diffe.Ric '  desdel
097100040824     c                   end
097200040824      **
097300040826     c     e84ntw        chain     fieu84F
097400040826     c                   if        %found(fieu84F)
097500040826     c                   movel     t84des        desntw
097600040826     c                   end
097700040826      **
097800041007      *                 * ------------------ *
097900041007     c                   exsr      Prt_testa
098000041007      *                 * ------------------ *
098100150203     c  n22              write     pripage
098200041007      *                 * ------------------ *
098300041007     c                   if        knmus = 'EDPAB'
098400150203     c  n22              write     secpage
098500040831     c                   end
098600041007      *                 * ------------------ *
098700040824      **
098800040826      ** pulisce comunque i campi x le rotture di livello
098900040826     c                   clear                   savdfp
099000040826     c                   clear                   savnfp
099100040826     c                   clear                   savrif
099200040826     c                   clear                   savgrp
099300040826     c                   clear                   savaas
099400040826     c                   clear                   savlnp
099500040826     c                   clear                   savnrs
099600040826     c                   clear                   savnsp
099700040826     c                   clear                   savsped
099800040826      **
099900040903     C                   ENDSR
100000040903     C**************************************************************************
100100040903     C* mi compongo l'istruzione sql
100200040903     C**************************************************************************
100300040903     C     istruz        BEGSR                                                  *
100400040903      **
100500040903     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file TNECF
100600040903     C                   EVAL      WrkSqlCmd
100700080617     C                             = 'SELECT TNECF00F.*, digits(ecfaas) concat'+
100800080617     C                             ' digits(ecflnp) concat digits(ecfnrs)'     +
100900080617     C                             ' concat digits(ecfnsp),'                   +
101000040903     C                             ' (ecfaas*10000 +ecfmgs), '                 +
101100041012     C                             ' (ecfimr * (-1) - ecfimp), '               +
101200041012     C                             ' (ecfimc - ecfimp), '                      +
101300040903     C                             ' substr(ecftbl, 1, 1) '                    +
101400080710     C                             ' FROM TNECF00F WHERE ECFDTG=0 '
101500160128      **
101600040903      **  + Network
101700040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)      +
101800080710     c                             ' and ecfntw =' + '''' + %TRIM(e84ntw) + ''''
101900040903      **
102000040903      **************************************************
102100040903     c* volte 1      Import Franchi         S/N
102200040903     c*  .... 2      Import Assegnati       S/N
102300040903     c*   ... 3      Export Franchi         S/N
102400040903     c*    .. 4      Export Assegnati       S/N
102500040903      **************************************************
102600040903      **  + porto Franco
102700040903     c                   if        volte = 1 or volte = 3
102800040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)
102900040903     c                              + ' and substr(ecftbl, 1, 1) = ''F'' '
103000040903      * se Exp.Franco
103100040903     c                   if        volte = 3
103200040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)
103300040903     c                              + ' and ecftbl <> ''F3'' '
103400040903     c                   end
103500040903     c                   end
103600040903      **  + porto Assegnato
103700040903     c                   if        volte = 2 or volte = 4
103800040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)
103900040903     c                              + ' and substr(ecftbl, 1, 1) = ''A'' '
104000040903      * se Imp.Assegnato
104100040903     c                   if        volte = 2
104200040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)
104300040903     c                              + ' and ecftbl <> ''A3'' '
104400040903     c                   end
104500040903     c                   end
104600040903      **  + import
104700040903     c                   if        volte = 1 or volte = 2
104800040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)
104900130619     c                              + ' and (ecflnp '
105000040903     c                   end
105100040903      **  + export
105200040903     c                   if        volte = 3 or volte = 4
105300040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)
105400130619     c                              + ' and (ecflna '
105500040903     c                   end
105600040903      *
105700040903      **  + Linea Partenza/Arrivo
105800040903     c                   select
105900040903      *
106000040903     c                   when      e84ntw = 'FED'
106100040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)
106200130619     c                              + ' = 400' + ')'
106300040903      *
106400040903     c                   when      e84ntw = 'DPD'
106500130705     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)
106600130705     c                              + ' in (select orgfil from azorg00f where '
106700130705     c                              + ' substr(orgde3, 11, 3)=''DPD'') ) '
106800130705      *------
106900040903     c                   other
107000040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)
107100130619     c                              + ' = ' + %TRIMR(e84NTW) + ')'
107200040903     c                   endsl
107300130619      **
107400040903      **  + data incasellamento
107500040903     c                   if        e84dii > 0
107600040903     C                   EVAL      WrkSqlCmd
107700040903     C                             =
107800040903     C                             %TRIMR(WrkSqlCmd)
107900040903     c                              + ' and  ecfdta between '
108000040903     c                              + %trim(%char(e84dii)) +  ' and '
108100040903     c                              + %trim(%char(e84dfi))
108200040903     c                   end
108300040903      **  + data spedizione
108400040903     c                   if        e84dis > 0
108500040903     C                   EVAL      WrkSqlCmd
108600040903     C                             =
108700040903     C                             %TRIMR(WrkSqlCmd)
108800040903     c                              + ' and (ecfaas*10000 +ecfmgs) between '
108900040903     c                              + %trim(%char(e84dis)) +  ' and '
109000040903     c                              + %trim(%char(e84dfs))
109100040903     c                   end
109200040903      **  + data fattura partner
109300040903     c                   if        e84diF > 0
109400040903     C                   EVAL      WrkSqlCmd
109500040903     C                             =
109600040903     C                             %TRIMR(WrkSqlCmd)
109700040903     c                              + ' and  ecfdfp between '
109800040903     c                              + %trim(%char(e84dif)) +  ' and '
109900040903     c                              + %trim(%char(e84dff))
110000040903     c                   end
110100040903      **
110200040903      ** altre selezioni
110300040903      **
110400040903      **  in questo giro non deve prendere le spedizioni:
110500040903      **        Addebitate dal Partner e non corrispondenti a nostre spedizioni
110600040903      **  e  nostre spedizioni non addebitate dal Partner.
110700040903     C                   EVAL      WrkSqlCmd
110800040903     C                             =
110900040903     C                             %TRIMR(WrkSqlCmd)
111000040903     c                              + ' and ecflnp > 0  '
111100130318     c                              + ' and ecfdfp <> 0 '
111200040903      **
111300040903      **  Esclusioni/Inclusioni di raggruppamenti specifici
111400040903     c                   if        exlgrp <> *blank
111500040903     c                   if        e84gie = 'E'
111600040903     C                   EVAL      WrkSqlCmd
111700040903     C                             = %TRIMR(WrkSqlCmd) + ' and ecfgrp NOT IN ('
111800040903     c                   else
111900040903     C                   EVAL      WrkSqlCmd
112000040903     C                             = %TRIMR(WrkSqlCmd) + ' and ecfgrp IN ('
112100040903     c                   end
112200040903      **
112300040903     c                   if        e84gr1 <> *blank
112400040903     C                   EVAL      WrkSqlCmd
112500040903     C                             = %TRIMR(WrkSqlCmd) + '''' + e84gr1 + ''''
112600040903     c                   end
112700040903     c                   if        e84gr2 <> *blank
112800040903     C                   EVAL      WrkSqlCmd
112900040903     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr2 + ''''
113000040903     c                   end
113100040903     c                   if        e84gr3 <> *blank
113200040903     C                   EVAL      WrkSqlCmd
113300040903     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr3 + ''''
113400040903     c                   end
113500040903     c                   if        e84gr4 <> *blank
113600040903     C                   EVAL      WrkSqlCmd
113700040903     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr4 + ''''
113800040903     c                   end
113900040903     c                   if        e84gr5 <> *blank
114000040903     C                   EVAL      WrkSqlCmd
114100041012     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr5 + ''''
114200040903     c                   end
114300040903     c                   if        e84gr6 <> *blank
114400040903     C                   EVAL      WrkSqlCmd
114500040903     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr6 + ''''
114600040903     c                   end
114700040903     c                   if        e84gr7 <> *blank
114800040903     C                   EVAL      WrkSqlCmd
114900040903     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr7 + ''''
115000040903     c                   end
115100040903     c                   if        e84gr8 <> *blank
115200040903     C                   EVAL      WrkSqlCmd
115300040903     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr8 + ''''
115400040903     c                   end
115500040903     c                   if        e84gr9 <> *blank
115600040903     C                   EVAL      WrkSqlCmd
115700040903     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr9 + ''''
115800040903     c                   end
115900040903     c                   if        e84gr0 <> *blank
116000040903     C                   EVAL      WrkSqlCmd
116100040903     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr0 + ''''
116200040903     c                   end
116300040903      **
116400040903     C                   EVAL      WrkSqlCmd
116500040903     C                             = %TRIMR(WrkSqlCmd) + ' )'
116600040903     c                   end
116700040903      **
116800150429      **   Aggiunta x estrarre i dati DUNFOSS o VTG
116900150429      **
117000150429      **
117100040903      **  Ordinamento di lettura x totalizzazioni
117200040903     C                   EVAL      WrkSqlCmd
117300040903     C                             = %TRIMR(WrkSqlCmd)
117400040903     c                             + ' ORDER BY '
117500041011      **    spedizione
117600040903     c                   if        e84tos = 'S'
117700040903     C                   EVAL      WrkSqlCmd
117800040903     C                             = %TRIMR(WrkSqlCmd)
117900080617     C                             + ' digits(ecfaas) concat digits(ecflnp)'
118000080617     C                             + ' concat digits(ecfnrs) concat'
118100080617     C                             + ' digits(ecfnsp), ecfrif,'
118200041015     C                             + ' ecfgrp, ecfdfp, ecfnfp, ecfmgs '
118300040903     c                   end
118400041011      **    riferimento
118500040903     c                   if        e84top = 'S'
118600040903     C                   EVAL      WrkSqlCmd
118700040903     C                             = %TRIMR(WrkSqlCmd)
118800040903     c                             + ' ecfrif, '
118900080618     C                             + 'digits(ecfaas) concat digits(ecflnp) '
119000080618     c                             + 'concat '
119100080618     C                             + 'digits(ecfnrs) concat digits(ecfnsp),'
119200041015     C                             + ' ecfgrp, ecfdfp, ecfnfp, ecfmgs '
119300040903     c                   end
119400041011     **    raggruppamento
119500040903     c                   if        e84tor = 'S'
119600040903     C                   EVAL      WrkSqlCmd
119700040903     C                             = %TRIMR(WrkSqlCmd)
119800040903     c                             + ' ecfgrp, ecfrif, '
119900080618     C                             + 'digits(ecfaas) concat digits(ecflnp) '
120000080618     c                             + 'concat '
120100080618     C                             + 'digits(ecfnrs) concat digits(ecfnsp),'
120200041015     C                             + ' ecfdfp, ecfnfp, ecfmgs '
120300040903     c                   end
120400041011     **    fattura
120500040903     c                   if        e84tof = 'S'
120600040903     C                   EVAL      WrkSqlCmd
120700040903     C                             = %TRIMR(WrkSqlCmd)
120800040903     c                             + ' ecfnfp, ecfdfp, '
120900080618     C                             + 'digits(ecfaas) concat digits(ecflnp) '
121000080618     c                             + 'concat '
121100080618     C                             + 'digits(ecfnrs) concat digits(ecfnsp),'
121200041015     C                             + ' ecfrif, ecfgrp, ecfmgs '
121300040903     c                   end
121400040903     **
121500040903     c                   if        e84dii > 0
121600040903     C                   EVAL      WrkSqlCmd
121700040903     C                             = %TRIMR(WrkSqlCmd)
121800040903     c                             + ', ecfdta '
121900040903     c                   end
122000040903     c                   if        e84dis > 0
122100040903     C                   EVAL      WrkSqlCmd
122200040903     C                             = %TRIMR(WrkSqlCmd)
122300040903     c                             + ', (ecfaas*10000 +ecfmgs) '
122400040903     c                   end
122500040903      **
122600040903      ** Stampa in prima pagina
122700040903     c                   movea     WrkSqlCmd     Ln_sql
122800040903     c                   eval      lin1 = Ln_sql(1)
122900040903     c                   eval      lin2 = Ln_sql(2)
123000040903     c                   eval      lin3 = Ln_sql(3)
123100040903     c                   eval      lin4 = Ln_sql(4)
123200040903     c                   eval      lin5 = Ln_sql(5)
123300040903     c                   eval      lin6 = Ln_sql(6)
123400040903     c                   eval      lin7 = Ln_sql(7)
123500040903     c                   eval      lin8 = Ln_sql(8)
123600040903     c                   eval      lin9 = Ln_sql(9)
123700040903     c                   eval      lin10 = Ln_sql(10)
123800040903     c                   eval      lin11 = Ln_sql(11)
123900040903     c                   eval      lin12 = Ln_sql(12)
124000040903     c                   eval      lin13 = Ln_sql(13)
124100040903     c                   eval      lin14 = Ln_sql(14)
124200040903     c                   eval      lin15 = Ln_sql(15)
124300040903      **
124400040903     C                   ENDSR
124500040824     C**************************************************************************
124600150325     C* mi compongo l'istruzione sql    NON ORM
124700040824     C**************************************************************************
124800040903     C     istruz2       BEGSR                                                  *
124900040824      **
125000040824     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file TNECF
125100040824     C                   EVAL      WrkSqlCmd
125200080618     C                             = 'SELECT TNECF00F.*, digits(ecfaas) '      +
125300080618     C                             ' concat '                                  +
125400080618     C                             'digits(ecflnp) concat digits(ecfnrs) '     +
125500080618     C                             ' concat '                                  +
125600040825     C                             'digits(ecfnsp),'                           +
125700040825     C                             ' (ecfaas*10000 +ecfmgs), '                 +
125800041012     C                             ' (ecfimr * (-1) - ecfimp), '               +
125900041012     C                             ' (ecfimc - ecfimp), '                      +
126000040824     C                             ' substr(ecftbl, 1, 1) '                    +
126100150325     C                             ' FROM TNECF00F WHERE ECFDTG=0 and ecfLNP=0'+
126200150325     C                             ' and ecfTPR <> ''ORM'' '
126300040824      **  + Network
126400040824     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)      +
126500040824     c                             ' and ecfntw =' + '''' + %TRIM(e84ntw) + ''''
126600040824      **
126700040903      **  + Non associate ma Addebitate
126800040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)    +
126900041012     c                             ' and ecfdft = 0' +
127000130318     c                             ' and ecfdfp > 0 '
127100041007      **
127200041007      **  se richiesta x totali fattura occorre impostare il limite di data max.
127300150325     c                   if        totf = 'S' and e84dff >0
127400041007     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)    +
127500041007     c                             ' and ecfdfp <= ' + %trim(%char(e84dff))
127600150325     c                   elseif    totf = 'S' and e84dff =0 and e84dfs >0
127700150325     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)    +
127800150325     c                             ' and ecfdfp <= ' + %trim(%char(e84dfs))
127900150325     c                   elseif    totf = 'S' and e84dff =0 and e84dfs =0
128000150325     c                                                      and e84dfi =0
128100150325     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)    +
128200150325     c                             ' and ecfdfp <= ' + %trim(%char(e84dfi))
128300041007     c                   end
128400041007      **
128500040827      **  Esclusioni/Inclusioni di raggruppamenti specifici
128600040827     c                   if        exlgrp <> *blank
128700040827     c                   if        e84gie = 'E'
128800040827     C                   EVAL      WrkSqlCmd
128900040827     C                             = %TRIMR(WrkSqlCmd) + ' and ecfgrp NOT IN ('
129000040827     c                   else
129100040827     C                   EVAL      WrkSqlCmd
129200040827     C                             = %TRIMR(WrkSqlCmd) + ' and ecfgrp IN ('
129300040827     c                   end
129400040827      **
129500040827     c                   if        e84gr1 <> *blank
129600040827     C                   EVAL      WrkSqlCmd
129700040827     C                             = %TRIMR(WrkSqlCmd) + '''' + e84gr1 + ''''
129800040827     c                   end
129900040827     c                   if        e84gr2 <> *blank
130000040827     C                   EVAL      WrkSqlCmd
130100040827     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr2 + ''''
130200040827     c                   end
130300040827     c                   if        e84gr3 <> *blank
130400040827     C                   EVAL      WrkSqlCmd
130500040827     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr3 + ''''
130600040827     c                   end
130700040827     c                   if        e84gr4 <> *blank
130800040827     C                   EVAL      WrkSqlCmd
130900040827     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr4 + ''''
131000040827     c                   end
131100040827     c                   if        e84gr5 <> *blank
131200040827     C                   EVAL      WrkSqlCmd
131300040827     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr5 + ''''
131400040827     c                   end
131500040827     c                   if        e84gr6 <> *blank
131600040827     C                   EVAL      WrkSqlCmd
131700040827     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr6 + ''''
131800040827     c                   end
131900040827     c                   if        e84gr7 <> *blank
132000040827     C                   EVAL      WrkSqlCmd
132100040827     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr7 + ''''
132200040827     c                   end
132300040827     c                   if        e84gr8 <> *blank
132400040827     C                   EVAL      WrkSqlCmd
132500040827     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr8 + ''''
132600040827     c                   end
132700040827     c                   if        e84gr9 <> *blank
132800040827     C                   EVAL      WrkSqlCmd
132900040827     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr9 + ''''
133000040827     c                   end
133100040827     c                   if        e84gr0 <> *blank
133200040827     C                   EVAL      WrkSqlCmd
133300040827     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr0 + ''''
133400040827     c                   end
133500040827      **
133600040827     C                   EVAL      WrkSqlCmd
133700040827     C                             = %TRIMR(WrkSqlCmd) + ' )'
133800040827     c                   end
133900040827      **
134000040825      **  Ordinamento di lettura x totalizzazioni
134100040824     C                   EVAL      WrkSqlCmd
134200040824     C                             = %TRIMR(WrkSqlCmd)
134300040907     c                             + ' ORDER BY ecfnfp, ecfdfp, ecfrif '
134400040824      **
134500040825      ** Stampa in prima pagina
134600040825     c                   movea     WrkSqlCmd     Ln_sql
134700040824     c                   eval      lin1 = Ln_sql(1)
134800040824     c                   eval      lin2 = Ln_sql(2)
134900040824     c                   eval      lin3 = Ln_sql(3)
135000040824     c                   eval      lin4 = Ln_sql(4)
135100040824     c                   eval      lin5 = Ln_sql(5)
135200040824     c                   eval      lin6 = Ln_sql(6)
135300040824     c                   eval      lin7 = Ln_sql(7)
135400040824     c                   eval      lin8 = Ln_sql(8)
135500040824     c                   eval      lin9 = Ln_sql(9)
135600040825     c                   eval      lin10 = Ln_sql(10)
135700040825     c                   eval      lin11 = Ln_sql(11)
135800040825     c                   eval      lin12 = Ln_sql(12)
135900040825     c                   eval      lin13 = Ln_sql(13)
136000040825     c                   eval      lin14 = Ln_sql(14)
136100040825     c                   eval      lin15 = Ln_sql(15)
136200040903      **
136300040903     C                   ENDSR
136400150325     C**************************************************************************
136500151217     C* mi compongo l'istruzione sql  ORM non associati a BOLLE
136600150325     C**************************************************************************
136700151217     C     istruz2_ORM_N BEGSR                                                  *
136800150325      **
136900150325     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file TNECF
137000150325     C                   EVAL      WrkSqlCmd
137100150325     C                             = 'SELECT TNECF00F.*, digits(ecfaas) '      +
137200150325     C                             ' concat '                                  +
137300150325     C                             'digits(ecflnp) concat digits(ecfnrs) '     +
137400150325     C                             ' concat '                                  +
137500150325     C                             'digits(ecfnsp),'                           +
137600150325     C                             ' (ecfaas*10000 +ecfmgs), '                 +
137700150325     C                             ' (ecfimr * (-1) - ecfimp), '               +
137800150325     C                             ' (ecfimc - ecfimp), '                      +
137900150325     C                             ' substr(ecftbl, 1, 1) '                    +
138000151216     C                             ' FROM TNECF00F WHERE ECFDTG=0 and ecfLNP=0'+
138100150325     C                             ' and ecfTPR = ''ORM'' '
138200150325      **  + Network
138300150325     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)      +
138400150325     c                             ' and ecfntw =' + '''' + %TRIM(e84ntw) + ''''
138500150325      **
138600150325      **  + Non associate ma Addebitate
138700150325     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)    +
138800150325     c                             ' and ecfdft = 0' +
138900150325     c                             ' and ecfdfp > 0 '
139000150325      **
139100150325      **  se richiesta x totali fattura occorre impostare il limite di data max.
139200150325     c                   if        totf = 'S' and e84dff >0
139300150325     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)    +
139400150325     c                             ' and ecfdfp <= ' + %trim(%char(e84dff))
139500150325     c                   elseif    totf = 'S' and e84dff =0 and e84dfs >0
139600150325     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)    +
139700150325     c                             ' and ecfdfp <= ' + %trim(%char(e84dfs))
139800150325     c                   elseif    totf = 'S' and e84dff =0 and e84dfs =0
139900150325     c                                                      and e84dfi =0
140000150325     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)    +
140100150325     c                             ' and ecfdfp <= ' + %trim(%char(e84dfi))
140200150325     c                   end
140300150325      **
140400150325      **  Esclusioni/Inclusioni di raggruppamenti specifici
140500150325     c                   if        exlgrp <> *blank
140600150325     c                   if        e84gie = 'E'
140700150325     C                   EVAL      WrkSqlCmd
140800150325     C                             = %TRIMR(WrkSqlCmd) + ' and ecfgrp NOT IN ('
140900150325     c                   else
141000150325     C                   EVAL      WrkSqlCmd
141100150325     C                             = %TRIMR(WrkSqlCmd) + ' and ecfgrp IN ('
141200150325     c                   end
141300150325      **
141400150325     c                   if        e84gr1 <> *blank
141500150325     C                   EVAL      WrkSqlCmd
141600150325     C                             = %TRIMR(WrkSqlCmd) + '''' + e84gr1 + ''''
141700150325     c                   end
141800150325     c                   if        e84gr2 <> *blank
141900150325     C                   EVAL      WrkSqlCmd
142000150325     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr2 + ''''
142100150325     c                   end
142200150325     c                   if        e84gr3 <> *blank
142300150325     C                   EVAL      WrkSqlCmd
142400150325     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr3 + ''''
142500150325     c                   end
142600150325     c                   if        e84gr4 <> *blank
142700150325     C                   EVAL      WrkSqlCmd
142800150325     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr4 + ''''
142900150325     c                   end
143000150325     c                   if        e84gr5 <> *blank
143100150325     C                   EVAL      WrkSqlCmd
143200150325     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr5 + ''''
143300150325     c                   end
143400150325     c                   if        e84gr6 <> *blank
143500150325     C                   EVAL      WrkSqlCmd
143600150325     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr6 + ''''
143700150325     c                   end
143800150325     c                   if        e84gr7 <> *blank
143900150325     C                   EVAL      WrkSqlCmd
144000150325     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr7 + ''''
144100150325     c                   end
144200150325     c                   if        e84gr8 <> *blank
144300150325     C                   EVAL      WrkSqlCmd
144400150325     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr8 + ''''
144500150325     c                   end
144600150325     c                   if        e84gr9 <> *blank
144700150325     C                   EVAL      WrkSqlCmd
144800150325     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr9 + ''''
144900150325     c                   end
145000150325     c                   if        e84gr0 <> *blank
145100150325     C                   EVAL      WrkSqlCmd
145200150325     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr0 + ''''
145300150325     c                   end
145400150325      **
145500150325     C                   EVAL      WrkSqlCmd
145600150325     C                             = %TRIMR(WrkSqlCmd) + ' )'
145700150325     c                   end
145800150325      **
145900150325      **  Ordinamento di lettura x totalizzazioni
146000150325     C                   EVAL      WrkSqlCmd
146100150325     C                             = %TRIMR(WrkSqlCmd)
146200150325     c                             + ' ORDER BY ecfnfp, ecfdfp, ecfrif '
146300150325      **
146400150325      ** Stampa in prima pagina
146500150325     c                   movea     WrkSqlCmd     Ln_sql
146600150325     c                   eval      lin1 = Ln_sql(1)
146700150325     c                   eval      lin2 = Ln_sql(2)
146800150325     c                   eval      lin3 = Ln_sql(3)
146900150325     c                   eval      lin4 = Ln_sql(4)
147000150325     c                   eval      lin5 = Ln_sql(5)
147100150325     c                   eval      lin6 = Ln_sql(6)
147200150325     c                   eval      lin7 = Ln_sql(7)
147300150325     c                   eval      lin8 = Ln_sql(8)
147400150325     c                   eval      lin9 = Ln_sql(9)
147500150325     c                   eval      lin10 = Ln_sql(10)
147600150325     c                   eval      lin11 = Ln_sql(11)
147700150325     c                   eval      lin12 = Ln_sql(12)
147800150325     c                   eval      lin13 = Ln_sql(13)
147900150325     c                   eval      lin14 = Ln_sql(14)
148000150325     c                   eval      lin15 = Ln_sql(15)
148100150325      **
148200150325     C                   ENDSR
148300151217     C**************************************************************************
148400151217     C* mi compongo l'istruzione sql  ORM Italia-Italia ASSOCIATE a BOLLE
148500151217     C**************************************************************************
148600151217     C     istruz2_ORM   BEGSR                                                  *
148700151217      **
148800151217     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file TNECF
148900151217     C                   EVAL      WrkSqlCmd
149000151217     C                             = 'SELECT TNECF00F.*, digits(ecfaas) '      +
149100151217     C                             ' concat '                                  +
149200151217     C                             'digits(ecflnp) concat digits(ecfnrs) '     +
149300151217     C                             ' concat '                                  +
149400151217     C                             'digits(ecfnsp),'                           +
149500151217     C                             ' (ecfaas*10000 +ecfmgs), '                 +
149600151217     C                             ' (ecfimr * (-1) - ecfimp), '               +
149700151217     C                             ' (ecfimc - ecfimp), '                      +
149800151217     C                             ' substr(ecftbl, 1, 1) '                    +
149900151217     C                             ' FROM TNECF00F WHERE ECFDTG=0 and ecfLNP>0'+
150000151217     C                             ' and ecfTPR = ''ORM'' '
150100151217      **  + Network
150200151217     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)      +
150300151217     c                             ' and ecfntw =' + '''' + %TRIM(e84ntw) + ''''
150400151217      **
150500151217      **  + Associate a DPD
150600151217     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)    +
150700151217     c                             ' and ecfdfp > 0 '
150800151217      **
150900151217      **  se richiesta x totali fattura occorre impostare il limite di data max.
151000151217     c                   if        totf = 'S' and e84dff >0
151100151217     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)    +
151200151217     c                             ' and ecfdfp <= ' + %trim(%char(e84dff))
151300151217     c                   elseif    totf = 'S' and e84dff =0 and e84dfs >0
151400151217     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)    +
151500151217     c                             ' and ecfdfp <= ' + %trim(%char(e84dfs))
151600151217     c                   elseif    totf = 'S' and e84dff =0 and e84dfs =0
151700151217     c                                                      and e84dfi =0
151800151217     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)    +
151900151217     c                             ' and ecfdfp <= ' + %trim(%char(e84dfi))
152000151217     c                   end
152100151217      **
152200151217      **  Esclusioni/Inclusioni di raggruppamenti specifici
152300151217     c                   if        exlgrp <> *blank
152400151217     c                   if        e84gie = 'E'
152500151217     C                   EVAL      WrkSqlCmd
152600151217     C                             = %TRIMR(WrkSqlCmd) + ' and ecfgrp NOT IN ('
152700151217     c                   else
152800151217     C                   EVAL      WrkSqlCmd
152900151217     C                             = %TRIMR(WrkSqlCmd) + ' and ecfgrp IN ('
153000151217     c                   end
153100151217      **
153200151217     c                   if        e84gr1 <> *blank
153300151217     C                   EVAL      WrkSqlCmd
153400151217     C                             = %TRIMR(WrkSqlCmd) + '''' + e84gr1 + ''''
153500151217     c                   end
153600151217     c                   if        e84gr2 <> *blank
153700151217     C                   EVAL      WrkSqlCmd
153800151217     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr2 + ''''
153900151217     c                   end
154000151217     c                   if        e84gr3 <> *blank
154100151217     C                   EVAL      WrkSqlCmd
154200151217     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr3 + ''''
154300151217     c                   end
154400151217     c                   if        e84gr4 <> *blank
154500151217     C                   EVAL      WrkSqlCmd
154600151217     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr4 + ''''
154700151217     c                   end
154800151217     c                   if        e84gr5 <> *blank
154900151217     C                   EVAL      WrkSqlCmd
155000151217     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr5 + ''''
155100151217     c                   end
155200151217     c                   if        e84gr6 <> *blank
155300151217     C                   EVAL      WrkSqlCmd
155400151217     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr6 + ''''
155500151217     c                   end
155600151217     c                   if        e84gr7 <> *blank
155700151217     C                   EVAL      WrkSqlCmd
155800151217     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr7 + ''''
155900151217     c                   end
156000151217     c                   if        e84gr8 <> *blank
156100151217     C                   EVAL      WrkSqlCmd
156200151217     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr8 + ''''
156300151217     c                   end
156400151217     c                   if        e84gr9 <> *blank
156500151217     C                   EVAL      WrkSqlCmd
156600151217     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr9 + ''''
156700151217     c                   end
156800151217     c                   if        e84gr0 <> *blank
156900151217     C                   EVAL      WrkSqlCmd
157000151217     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr0 + ''''
157100151217     c                   end
157200151217      **
157300151217     C                   EVAL      WrkSqlCmd
157400151217     C                             = %TRIMR(WrkSqlCmd) + ' )'
157500151217     c                   end
157600151217      **
157700151217      **  Ordinamento di lettura x totalizzazioni
157800151217     C                   EVAL      WrkSqlCmd
157900151217     C                             = %TRIMR(WrkSqlCmd)
158000151217     c                             + ' ORDER BY ecfnfp, ecfdfp, ecfrif '
158100151217      **
158200151217      ** Stampa in prima pagina
158300151217     c                   movea     WrkSqlCmd     Ln_sql
158400151217     c                   eval      lin1 = Ln_sql(1)
158500151217     c                   eval      lin2 = Ln_sql(2)
158600151217     c                   eval      lin3 = Ln_sql(3)
158700151217     c                   eval      lin4 = Ln_sql(4)
158800151217     c                   eval      lin5 = Ln_sql(5)
158900151217     c                   eval      lin6 = Ln_sql(6)
159000151217     c                   eval      lin7 = Ln_sql(7)
159100151217     c                   eval      lin8 = Ln_sql(8)
159200151217     c                   eval      lin9 = Ln_sql(9)
159300151217     c                   eval      lin10 = Ln_sql(10)
159400151217     c                   eval      lin11 = Ln_sql(11)
159500151217     c                   eval      lin12 = Ln_sql(12)
159600151217     c                   eval      lin13 = Ln_sql(13)
159700151217     c                   eval      lin14 = Ln_sql(14)
159800151217     c                   eval      lin15 = Ln_sql(15)
159900151217      **
160000151217     C                   ENDSR
160100040903     C**************************************************************************
160200040903     C* mi compongo l'istruzione sql
160300040903     C**************************************************************************
160400040903     C     istruz3       BEGSR                                                  *
160500040903      **
160600040903     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file TNECF
160700040903     C                   EVAL      WrkSqlCmd
160800080618     C                             = 'SELECT TNECF00F.*, digits(ecfaas) '      +
160900080618     C                             'concat '                                   +
161000080618     C                             'digits(ecflnp) concat digits(ecfnrs) '     +
161100080618     C                             'concat '                                   +
161200040903     C                             'digits(ecfnsp),'                           +
161300040903     C                             ' (ecfaas*10000 +ecfmgs), '                 +
161400041012     C                             ' (ecfimr * (-1) - ecfimp), '               +
161500041012     C                             ' (ecfimc - ecfimp), '                      +
161600040903     C                             ' substr(ecftbl, 1, 1) '                    +
161700040907     C                             ' FROM TNECF00F WHERE ECFDTG = 0 '
161800040903      **  + Network
161900040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)      +
162000040903     c                             ' and ecfntw =' + '''' + %TRIM(e84ntw) + ''''
162100040903      **
162200040903      **************************************************
162300040903     c* volte 1      Import Franchi         S/N
162400040903     c*  .... 2      Import Assegnati       S/N
162500040903     c*   ... 3      Export Franchi         S/N
162600040903     c*    .. 4      Export Assegnati       S/N
162700040903      **************************************************
162800040903      **  + porto Franco
162900040903     c                   if        volte = 1 or volte = 3
163000040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)
163100040903     c                              + ' and substr(ecftbl, 1, 1) = ''F'' '
163200040903      * se Exp.Franco
163300040903     c                   if        volte = 3
163400040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)
163500040903     c                              + ' and ecftbl <> ''F3'' '
163600040903     c                   end
163700040903     c                   end
163800040903      **  + porto Assegnato
163900040903     c                   if        volte = 2 or volte = 4
164000040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)
164100040903     c                              + ' and substr(ecftbl, 1, 1) = ''A'' '
164200040903      * se Imp.Assegnato
164300040903     c                   if        volte = 2
164400040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)
164500040903     c                              + ' and ecftbl <> ''A3'' '
164600040903     c                   end
164700040903     c                   end
164800040903      **  + import
164900040903     c                   if        volte = 1 or volte = 2
165000040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)
165100130619     c                              + ' and (ecflnp '
165200040903     c                   end
165300040903      **  + export
165400040903     c                   if        volte = 3 or volte = 4
165500040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)
165600130619     c                              + ' and (ecflna '
165700040903     c                   end
165800040903      *
165900040903      **  + Linea Partenza/Arrivo
166000040903     c                   select
166100040903      *
166200040903     c                   when      e84ntw = 'FED'
166300040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)
166400130619     c                              + ' = 400' + ')'
166500040903      *
166600040903     c                   when      e84ntw = 'DPD'
166700130705     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)
166800130705     c                              + ' in (select orgfil from azorg00f where '
166900130705     c                              + ' substr(orgde3, 11, 3)=''DPD'') ) '
167000130705      *------
167100040903     c                   other
167200040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)
167300130619     c                              + ' = ' + %TRIMR(e84NTW) + ')'
167400040903     c                   endsl
167500040903      **
167600040903      ** altre selezioni
167700040903      **
167800040903      **  in questo giro  deve prendere solo le nostre spedizioni
167900040903      **    non addebitate dal Partner.
168000040903     C                   EVAL      WrkSqlCmd
168100040903     C                             =
168200040903     C                             %TRIMR(WrkSqlCmd)
168300130318     c                              + ' and ecfdfp = 0 '
168400040903      **
168500040903      **  Esclusioni/Inclusioni di raggruppamenti specifici
168600040903     c                   if        exlgrp <> *blank
168700040903     c                   if        e84gie = 'E'
168800040903     C                   EVAL      WrkSqlCmd
168900040903     C                             = %TRIMR(WrkSqlCmd) + ' and ecfgrp NOT IN ('
169000040903     c                   else
169100040903     C                   EVAL      WrkSqlCmd
169200040903     C                             = %TRIMR(WrkSqlCmd) + ' and ecfgrp IN ('
169300040903     c                   end
169400040903      **
169500040903     c                   if        e84gr1 <> *blank
169600040903     C                   EVAL      WrkSqlCmd
169700040903     C                             = %TRIMR(WrkSqlCmd) + '''' + e84gr1 + ''''
169800040903     c                   end
169900040903     c                   if        e84gr2 <> *blank
170000040903     C                   EVAL      WrkSqlCmd
170100040903     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr2 + ''''
170200040903     c                   end
170300040903     c                   if        e84gr3 <> *blank
170400040903     C                   EVAL      WrkSqlCmd
170500040903     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr3 + ''''
170600040903     c                   end
170700040903     c                   if        e84gr4 <> *blank
170800040903     C                   EVAL      WrkSqlCmd
170900040903     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr4 + ''''
171000040903     c                   end
171100040903     c                   if        e84gr5 <> *blank
171200040903     C                   EVAL      WrkSqlCmd
171300040903     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr5 + ''''
171400040903     c                   end
171500040903     c                   if        e84gr6 <> *blank
171600040903     C                   EVAL      WrkSqlCmd
171700040903     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr6 + ''''
171800040903     c                   end
171900040903     c                   if        e84gr7 <> *blank
172000040903     C                   EVAL      WrkSqlCmd
172100040903     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr7 + ''''
172200040903     c                   end
172300040903     c                   if        e84gr8 <> *blank
172400040903     C                   EVAL      WrkSqlCmd
172500040903     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr8 + ''''
172600040903     c                   end
172700040903     c                   if        e84gr9 <> *blank
172800040903     C                   EVAL      WrkSqlCmd
172900040903     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr9 + ''''
173000040903     c                   end
173100040903     c                   if        e84gr0 <> *blank
173200040903     C                   EVAL      WrkSqlCmd
173300040903     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr0 + ''''
173400040903     c                   end
173500040903      **
173600040903     C                   EVAL      WrkSqlCmd
173700040903     C                             = %TRIMR(WrkSqlCmd) + ' )'
173800040903     c                   end
173900040903      **
174000040903      **  Ordinamento di lettura x totalizzazioni
174100040903     C                   EVAL      WrkSqlCmd
174200040903     C                             = %TRIMR(WrkSqlCmd)
174300040903     c                             + ' ORDER BY '
174400041026     c                             + ' ecfnfp, ecfdfp, ecfaas, ecfmgs, '
174500080618     C                             + 'digits(ecfaas) concat digits(ecflnp) '
174600080618     C                             + 'concat '
174700080618     C                             + 'digits(ecfnrs) concat digits(ecfnsp),'
174800040903     C                             + ' ecfrif, ecfgrp '
174900040903      **
175000040903      ** Stampa in prima pagina
175100040903     c                   movea     WrkSqlCmd     Ln_sql
175200040903     c                   eval      lin1 = Ln_sql(1)
175300040903     c                   eval      lin2 = Ln_sql(2)
175400040903     c                   eval      lin3 = Ln_sql(3)
175500040903     c                   eval      lin4 = Ln_sql(4)
175600040903     c                   eval      lin5 = Ln_sql(5)
175700040903     c                   eval      lin6 = Ln_sql(6)
175800040903     c                   eval      lin7 = Ln_sql(7)
175900040903     c                   eval      lin8 = Ln_sql(8)
176000040903     c                   eval      lin9 = Ln_sql(9)
176100040903     c                   eval      lin10 = Ln_sql(10)
176200040903     c                   eval      lin11 = Ln_sql(11)
176300040903     c                   eval      lin12 = Ln_sql(12)
176400040903     c                   eval      lin13 = Ln_sql(13)
176500040903     c                   eval      lin14 = Ln_sql(14)
176600040903     c                   eval      lin15 = Ln_sql(15)
176700040824      **
176800040824     C                   ENDSR
176900020326     C**************************************************************************
177000040820      *
177100040820     C**************************************************************************
177200040831     C     imp_rig_det   begSR
177300040820      **
177400040826     c                   z-add     ecfimp        detimp
177500040826     c                   if        tipo_confr = 'R'
177600040826     c                   z-add     deltar        detdel
177700041012     c                   z-sub     ecfimr        detim1
177800040826     c                   else
177900040826     c                   z-add     deltac        detdel
178000040826     c                   z-add     ecfimc        detim1
178100040826     c                   end
178200041011      *
178300041011      * ricava la % del delta
178400041011     c                   clear                   detprc
178500041011     c                   clear                   simprc
178600041014      *
178700041011      * solo se richiesto dettaglio in totale x Fattura o Raggruppamento
178800041011     c                   if        e84tof ='S' or e84tor ='S'
178900041011     c                   if        detim1 <> 0 and e84sde <> 99
179000041011     c                   eval(h)   totdel = (detdel) * 100 / detim1
179100041011     c                   move      '%'           simprc
179200041011     c                   if        %abs(totdel) < 999,99
179300041011     c                   move      totdel        detprc
179400041011     c                   else
179500041011     c                   move      *all'9'       detprc
179600041011     c                   end
179700041011     c                   end
179800041011     c                   end
179900041011     c                   clear                   totdel
180000040826      **
180100040826      **  se deve creare il file PC
180200040826     c                   if        e84fpc = 'S'
180300040826     c                   exsr      wrt_PCfile
180400040901     c                   end
180500040826      **
180600040826     C                   ENDSR
180700040826     C**************************************************************************
180800040826      *
180900040826     C**************************************************************************
181000040826     C     totalizza     begSR
181100150429      **
181200150429      **  Solo x "DPD" controlla se il Rif.Parcel proviene dai DEPOT Particolari
181300150429      **   da Monitorare.
181400150429     c                   clear                   CliDPD_Partic     1
181500150429     c                   IF        E84NTW = 'DPD'
181600150429     c                   movel     ecfRIF        Depot4            4
181700150429     c                   z-add     1             dep               3 0
181800150429     c     Depot4        lookup    DPD_DEP(dep)                           21
181900150429     c                   If        %Equal
182000150429     c                   eval       CliDPD_Partic = DPD_CLI(dep)
182100150429     c                   end
182200150429     c                   endIF
182300150429      **
182400150429      **  Se DPD e si tratta di un Cliente DANFOSS o VTG e siamo nel primo ciclo
182500150429      **  ossia fra 1 e 4.
182600150429     c                   If         CliDPD_Partic <> *blank
182700150429     c                              and VLT <=4
182800150429      **
182900150429     c                   If         CliDPD_Partic = 'D'
183000150429     c                   add       1             tSPDD(vlt)
183100150429     c                   add       ecfimp        tPTND(vlt)
183200150429     c                   add       detim1        tBRTD(vlt)
183300150429     c                   if        tipo_confr = 'R'
183400150429     c                   add       deltar        tDIFD(vlt)
183500150429     c                   else
183600150429     c                   add       deltac        tDIFD(vlt)
183700150429     c                   end
183800150429     c                   elseIf     CliDPD_Partic = 'V'
183900150429     c                   add       1             tSPDV(vlt)
184000150429     c                   add       ecfimp        tPTNV(vlt)
184100150429     c                   add       detim1        tBRTV(vlt)
184200150429     c                   if        tipo_confr = 'R'
184300150429     c                   add       deltar        tDIFV(vlt)
184400150429     c                   else
184500150429     c                   add       deltac        tDIFV(vlt)
184600150429     c                   end
184700150429     c                   end
184800150429      **
184900150429     c                   endIF
185000151118      **
185100151118      **  memorizza i RESI
185200151118     c                   IF           ecfFBR = 'S'
185300151118     c                   IF         VLT =2 or VLT =4 or
185400151118     c                              VLT =6 or VLT =8
185500151118     c                   add       1             tSPDR(vlt)
185600151118     c                   add       ecfimp        tPTNR(vlt)
185700151118     c                   add       detim1        tBRTR(vlt)
185800151118     c                   if        tipo_confr = 'R'
185900151118     c                   add       deltar        tDIFR(vlt)
186000151118     c                   else
186100151118     c                   add       deltac        tDIFR(vlt)
186200151118     c                   end
186300151118     c                   endIF
186400151118     c                   endIF
186500040826      **
186600040826     c                   move      'S'           almeno_uno
186700150129     c                   add       1             tSPD(vlt)
186800150129     c                   add       ecfimp        tPTN(vlt)
186900040825     c                   add       ecfimp        totrip
187000040825     c                   add       ecfimp        totfap
187100040825     c                   add       ecfimp        totgep
187200040825     c                   add       ecfimp        totgrp
187300150129     c                   add       detim1        tBRT(vlt)
187400040825     c                   add       detim1        totri1
187500040825     c                   add       detim1        totfa1
187600040825     c                   add       detim1        totge1
187700040825     c                   add       detim1        totgr1
187800040825     c                   if        tipo_confr = 'R'
187900150129     c                   add       deltar        tDIF(vlt)
188000040825     c                   add       deltar        totrid
188100040825     c                   add       deltar        totfad
188200040825     c                   add       deltar        totged
188300040825     c                   add       deltar        totgrd
188400040825     c                   else
188500150129     c                   add       deltac        tDIF(vlt)
188600040825     c                   add       deltac        totrid
188700040825     c                   add       deltac        totfad
188800040825     c                   add       deltac        totged
188900040825     c                   add       deltac        totgrd
189000040825     c                   end
189100040825      *
189200040825      * Incasella l'importo nel gruppo di appartenenza
189300040825     c                   z-add     1             Ip                3 0
189400040831     c                   if        ecfgrp <> *blank
189500040825     c     ecfGRP        lookup    Grp(Ip)                                21
189600040831     c                   else
189700040831      * x gruppo blank
189800040831     c     'blk'         lookup    Grp(Ip)                                21
189900040831     c                   end
190000040825     c  n21              z-add     1             Ip
190100040825     c  n21*blank        lookup    Grp(Ip)                                21
190200040831     c                   if        ecfgrp <> *blank
190300040825     c                   movel     ecfGRP        grP(Ip)
190400040831     c                   else
190500040831     c                   movel     'blk'         grP(Ip)
190600040831     c                   end
190700040825     c                   add       ecfIMP        ImP(Ip)
190800041012     c                   sub       ecfIMR        ImR(Ip)
190900040825     c                   add       ecfIMC        ImC(Ip)
191000040825     c                   if        tipo_confr = 'R'
191100040825     c                   add       deltar        ImD(Ip)
191200040825     c                   else
191300040825     c                   add       deltac        ImD(Ip)
191400040825     c                   end
191500040825      **
191600040825     C                   ENDSR
191700040825     C**************************************************************************
191800040827      *   Totali a parte x Non Addebitate o x Non incasellate
191900040825     C**************************************************************************
192000041014     C     tot_A_Parte   begSR
192100040902      **
192200040902     c                   if        tipo_confr = 'R'
192300041012     c                   z-sub     ecfimr        detim1
192400040902     c                   else
192500040902     c                   z-add     ecfimc        detim1
192600040902     c                   end
192700040902      **
192800130318     c                   if        (ecfdfp = 0)
192900040902     c                   if        tipo_confr = 'R'
193000040902     c                   add       deltar        tnadel
193100040902     c                   else
193200040902     c                   add       deltac        tnadel
193300040902     c                   end
193400040902     c                   add       ecfimp        tnaimp
193500040902     c                   add       detim1        tnaim1
193600040902     c                   end
193700040902      **
193800041014      **
193900040902     c                   if        ecfdfp > 0
194000040902     c                   if        tipo_confr = 'R'
194100040902     c                   add       deltar        tnidel
194200040902     c                   else
194300040902     c                   add       deltac        tnidel
194400040902     c                   end
194500040902     c                   add       ecfimp        tniimp
194600040902     c                   add       detim1        tniim1
194700040902     c                   end
194800040902      **
194900040827     C                   ENDSR
195000040824     C**************************************************************************
195100040824      *
195200040820     C**************************************************************************
195300040825     C     TOTals        begSR
195400040826      *
195500151126      *  solo per i NON incasellati qual'è la data più vecchia
195600151126     c                   if        vlt > 4 and vlt < 9
195700151126     c                   if        data_MINIMA > datspe or data_MINIMA =0
195800151126     c                   eval      data_MINIMA = datspe
195900151126     c                   movel     data_MINIMA   dataiso
196000151126     c                   eval      danoninc    = dataISO
196100151126     c                   end
196200151126     c                   end
196300040823      *
196400040825     c* ROTTURA Spedizione
196500040826     c                   if        e84tos ='S'
196600040825     c                   if        spedbar <> savsped or
196700040825     C                             SqlCod = 100
196800040831     c                   if        almeno_uno ='S'
196900040825     c                   exsr      TOTalSpe
197000040831     c                   end
197100040825     c                   eval      savsped =  spedbar
197200040825     c                   eval      savaas  =  ecfaas
197300040825     c                   eval      savlnp  =  ecflnp
197400040825     c                   eval      savnrs  =  ecfnrs
197500040825     c                   eval      savnsp  =  ecfnsp
197600040825     c                   end
197700040826     c                   end
197800040825      *
197900040825     c* ROTTURA Riferimento
198000040826     c                   if        e84top ='S'
198100040825     c                   if        ecfrif <> savrif or
198200040825     C                             SqlCod = 100
198300040831     c                   if        almeno_uno ='S'
198400040825     c                   exsr      TOTalRif
198500040831     c                   end
198600040825     c                   eval      savrif  =  ecfrif
198700040825     c                   end
198800040826     c                   end
198900040825      *
199000040825     c* ROTTURA Fattura
199100040826     c                   if        e84tof ='S'
199200040825     c                   if        ecfnfp <> savnfp or
199300040825     C                             SqlCod = 100
199400040831     c                   if        almeno_uno ='S'
199500040825     c                   exsr      TOTalfat
199600040831     c                   end
199700040825     c                   eval      savnfp =  ecfnfp
199800040901     c                   eval      savdfp =  ecfdfp
199900040825     c                   end
200000040826     c                   end
200100040825      *
200200040825     c* ROTTURA Gruppo
200300040826     c                   if        e84tor ='S'
200400040825     c                   if        ecfgrp <> savgrp or
200500040825     C                             SqlCod = 100
200600040831     c                   if        almeno_uno ='S'
200700040825     c                   exsr      TOTalgrp
200800040831     c                   end
200900040825     c                   eval      savgrp =  ecfgrp
201000040825     c                   end
201100040826     c                   end
201200040825      *
201300040825     C                   ENDSR
201400040825     C**************************************************************************
201500040825      *
201600040825     C**************************************************************************
201700040825     C     TOTalSpe      begSR
201800040825      *
201900040825     c                   if        prima_volta = 'N'
202000040824     C* totale  Spedizione (Riferimento)
202100040823     c                   if        *in66
202200040823      *                 * ------------------ *
202300041007     c                   exsr      Prt_testa
202400041007      *                 * ------------------ *
202500150203     c  n22              write     tesdetta
202600040823      *                 * ------------------ *
202700040823     c                   setoff                                       66
202800040823     c                   end
202900040826      *
203000040826      * ricava la % del delta
203100040826     c                   if        totri1 <> 0
203200040901     c                   eval(h)   totdel = (totrid) * 100 / totri1
203300040901     c                   if        %abs(totdel) < 999,99
203400040901     c                   move      totdel        prcdel
203500040901     c                   else
203600040901     c                   move      *all'9'       prcdel
203700040901     c                   end
203800040826     c                   end
203900040826      *
204000041011      *** attenzione stampo in base alla soglia richiesta
204100041011      *
204200041011     c                   if        e84sde = 99 or %abs(prcdel) > e84sde
204300150203     c  n22              write     totspedz
204400041011     c                   end
204500040824     c                   end
204600040826      *
204700040824     c                   clear                   totrip
204800040825     c                   clear                   totri1
204900040825     c                   clear                   totri2
205000040824     c                   clear                   totrid
205100040826     c                   clear                   prcdel
205200040901     c                   clear                   totdel           11 2
205300040820      *
205400040820     C                   ENDSR
205500040825     C**************************************************************************
205600040825      *
205700040825     C**************************************************************************
205800040825     C     TOTalRif      begSR
205900040825      *
206000040825     c                   if        prima_volta = 'N'
206100040825     C* totale  Spedizione (Riferimento)
206200040825     c                   if        *in66
206300040825      *                 * ------------------ *
206400041007     c                   exsr      Prt_testa
206500041007      *                 * ------------------ *
206600150203     c  n22              write     tesdetta
206700040825      *                 * ------------------ *
206800040825     c                   setoff                                       66
206900040825     c                   end
207000040826      *
207100040826      * ricava la % del delta
207200040826     c                   if        totri1 <> 0
207300040901     c                   eval(h)   totdel = (totrid) * 100 / totri1
207400040901     c                   if        %abs(totdel) < 999,99
207500040901     c                   move      totdel        prcdel
207600040901     c                   else
207700040901     c                   move      *all'9'       prcdel
207800040901     c                   end
207900040826     c                   end
208000040826      *
208100041011      *** attenzione stampo in base alla soglia richiesta
208200041011      *
208300041011     c                   if        e84sde = 99 or %abs(prcdel) > e84sde
208400150203     c  n22              write     totrifer
208500041011     c                   end
208600040825     c                   end
208700040826      *
208800040825     c                   clear                   totrip
208900040825     c                   clear                   totri1
209000040825     c                   clear                   totri2
209100040825     c                   clear                   totrid
209200040826     c                   clear                   prcdel
209300040901     c                   clear                   totdel           11 2
209400040825      *
209500040825     C                   ENDSR
209600040824     C**************************************************************************
209700040824      *
209800040820     C**************************************************************************
209900040825     C     TOTalfat      begSR
210000040824      *
210100040825     c                   if        prima_volta = 'N'
210200040831      *
210300040901      *    tot   schiere x gruppo
210400040831     c                   exsr      riep_grp
210500040831      *
210600040824     C* totale
210700040824     c                   if        *in66
210800040824      *                 * ------------------ *
210900041007     c                   exsr      Prt_testa
211000041007      *                 * ------------------ *
211100150203     c  n22              write     tesdetta
211200040824      *                 * ------------------ *
211300040824     c                   setoff                                       66
211400040824     c                   end
211500150203     c  n22              write     totfattu
211600040824     c                   end
211700040826      *
211800040824     c                   clear                   totfap
211900040825     c                   clear                   totfa1
212000040825     c                   clear                   totfa2
212100040824     c                   clear                   totfad
212200040824      *
212300040824     C                   ENDSR
212400040824     C**************************************************************************
212500040825      *
212600040825     C**************************************************************************
212700040825     C     TOTalgrp      begSR
212800040825      *
212900040825     c                   if        prima_volta = 'N'
213000040825     C* totale
213100040825     c                   if        *in66
213200040825      *                 * ------------------ *
213300041007     c                   exsr      Prt_testa
213400041007      *                 * ------------------ *
213500150203     c  n22              write     tesdetta
213600040825      *                 * ------------------ *
213700040825     c                   setoff                                       66
213800040825     c                   end
213900150203     c  n22              write     totgrup
214000040825     c                   end
214100040826      *
214200040825     c                   clear                   totgrp
214300040825     c                   clear                   totgr1
214400040825     c                   clear                   totgr2
214500040825     c                   clear                   totgrd
214600040825      *
214700040825     C                   ENDSR
214800040825     C**************************************************************************
214900040827      *  Preciclo se attivato x totale a Spedizione
215000040825     C**************************************************************************
215100040826     C     pre_ciclo     begsr
215200040826      *
215300040826      * prova a chiuderlo sempre prima di pulirlo
215400040826      *  Pulisce il workfile  su cui impostare le spedizioni da NON stampare
215500041014     c                   close     wfieu85w                             99
215600040826      *
215700040826     C                   z-add     62            lung             15 5
215800040826     C                   clear                   comman
215900040826     C                   movea     cmd(1)        comman           80
216000040826     C                   call      'QCMDEXC'
216100040826     C                   parm                    comman
216200040826     C                   parm                    lung
216300041014     c                   open      wfieu85w
216400040826      *
216500040826     C/EXEC SQL
216600040903     C+ PREPARE P1 FROM :WrkSqlCmd
216700040826     C/END-EXEC
216800040826
216900040826     C/EXEC SQL
217000040903     C+ DECLARE A2 CURSOR FOR P1
217100040826     C/END-EXEC
217200040826
217300040826     C/EXEC SQL
217400040826     C+ OPEN A2
217500040826     C/END-EXEC
217600041011     c                   clear                   almeno_uno
217700040826     C                   DOU       SqlCod <> 0
217800040826     C/EXEC SQL
217900040826     C+ FETCH NEXT FROM A2 INTO :tnecf00f, :spedbar, :datspe, :deltar,
218000040826     C+ :deltac, :porto
218100040826     C/END-EXEC
218200040826     C                   SELECT
218300041008     c* a fine file
218400041008     C                   WHEN      SqlCod = 100
218500041008     c* ROTTURA Spedizione
218600041008     c                   if        almeno_uno = 'S'
218700041008     c                   if        spedbar <> savsped or
218800041008     C                             SqlCod = 100
218900041008     c                   exsr      WRTxSpe
219000041008     c                   eval      savsped =  spedbar
219100041008     c                   end
219200041008     c                   else
219300041008     c                   end
219400041008      * poi esce
219500041008     c                   leave
219600041008     **
219700041008     C                   WHEN      SqlCod < 0
219800041008     c*
219900041008     C                   seton                                        H4
220000041008     c                   goto      fine_preciclo
220100041008     c*
220200041008     C                   OTHER
220300041008     **
220400040826     c* ROTTURA Spedizione
220500040826     c                   if        spedbar <> savsped or
220600040826     C                             SqlCod = 100
220700040826     c                   exsr      WRTxSpe
220800040826     c                   eval      savsped =  spedbar
220900040826     c                   end
221000040826      *
221100041011     c                   move      'S'           almeno_uno
221200041011      **
221300041011     c                   if        tipo_confr = 'R'
221400041012     c                   sub       ecfimr        calrim
221500041011     c                   add       deltar        calrid
221600041011     c                   else
221700041011     c                   add       ecfimc        calrim
221800041011     c                   add       deltac        calrid
221900041011     c                   end
222000041011      **
222100040826     C                   ENDSL
222200040826
222300040826     C                   ENDDO
222400040826     C/EXEC SQL
222500040826     C+ CLOSE A2
222600040826     C/END-EXEC
222700041008     c     fine_preciclo tag
222800040826     C                   ENDSR
222900040826     C**************************************************************************
223000040826      *   scrive le Spedizioni da scartare in seguito
223100040826     C**************************************************************************
223200040826     C     WRTxSpe       begSR
223300040826      *
223400040826      * ricava la % del delta
223500040826     c                   clear                   fieuw85
223600041011     c                   if        calrim <> 0
223700070427     c                   clear                   totale_prc        9 2
223800070427     c                   eval(h)   totale_prc = (calrid) * 100 / calrim
223900070427     c                   if        totale_prc > 999,99 or totale_prc < -999,99
224000070427     c                   eval(h)   w85prc = 999,99
224100070427     c                   else
224200070427     c                   eval(h)   w85prc = (calrid) * 100 / calrim
224300070427     c                   end
224400040826     c                   end
224500040826      *  confronta un valore assoluto di scostamento
224600040826     c                   if        savsped <> *blank
224700041011     c                   if        e84sde < %abs(w85prc) or calrim = 0
224800040826     c                   eval      w85spb = savsped
224900040826     c                   write     fieuw85
225000040826     c                   end
225100040826     c                   end
225200040826      *
225300041011     c                   clear                   calrim
225400041011     c                   clear                   calrid
225500040826      *
225600040826     C                   ENDSR
225700040826     C**************************************************************************
225800040827      *  Totali Riepilocativi prima del Totale generale
225900040827     C**************************************************************************
226000040827     C     tot_riepilogo begsr
226100040827      *
226200040827     C* totale non addebitate
226300040903     c                   if        e84sna <>'E' and ciclo_exec = 3
226400040827     c                   if        *in66
226500040827      *                 * ------------------ *
226600041007     c                   exsr      Prt_testa
226700041007      *                 * ------------------ *
226800150203     c  n22              write     tesdetta
226900040827      *                 * ------------------ *
227000040827     c                   setoff                                       66
227100040827     c                   end
227200150203     c  n22              write     totTNA
227300040903     c                   end
227400040901      ********
227500040827     C* totale non incasellate
227600040903     c                   if        e84sni <>'E' and ciclo_exec = 2
227700040827     c                   if        *in66
227800040827      *                 * ------------------ *
227900041007     c                   exsr      Prt_testa
228000041007      *                 * ------------------ *
228100150203     c  n22              write     tesdetta
228200040827      *                 * ------------------ *
228300040827     c                   setoff                                       66
228400040827     c                   end
228500150203     c  n22              write     totTNI
228600040903     c                   end
228700040827      *
228800040827     c                   clear                   tnadel
228900040827     c                   clear                   tnaimp
229000040827     c                   clear                   tnaim1
229100040827     c                   clear                   tnaim2
229200040827     c                   clear                   tnidel
229300040827     c                   clear                   tniimp
229400040827     c                   clear                   tniim1
229500040827     c                   clear                   tniim2
229600040827      *
229700040827     C                   ENDSR
229800040827     C**************************************************************************
229900040901      *  prima di fattura riepiloga x gruppo
230000040827     C**************************************************************************
230100040827     C     riep_grp      begsr
230200040827      **
230300040827     c                   do        50            cnt               3 0
230400040827     c                   if        grp(cnt) = *blank
230500040827     c                             and cnt > 1
230600040827     c                   leave
230700040827     c                   end
230800040827     c                   movel     grp(cnt)      savgrp
230900040827     c                   z-add     ImP(cnt)      totGrp
231000040827     c                   if        tipo_confr = 'R'
231100041012     c                   z-add     ImR(cnt)      totGr1
231200040827     c                   else
231300040827     c                   z-add     ImC(cnt)      totGr1
231400040827     c                   end
231500040827     c                   z-add     ImD(cnt)      totGrd
231600040831     c                   if        *in66
231700040831      *                 * ------------------ *
231800041007     c                   exsr      Prt_testa
231900041007      *                 * ------------------ *
232000150203     c  n22              write     tesdetta
232100040831      *                 * ------------------ *
232200040831     c                   setoff                                       66
232300040831     c                   end
232400150203     c  n22              write     Totgrup
232500040901      **
232600040827     c                   EndDO
232700040827      *
232800040827     c                   clear                   grP
232900040827     c                   clear                   imP
233000040827     c                   clear                   imR
233100040827     c                   clear                   imC
233200040827     c                   clear                   imD
233300040827      *
233400040827     C                   ENDSR
233500040827     C**************************************************************************
233600150129     C     imposta_Riep  BEGSR
233700150203      *
233800150505     c                   eval      ptn1    = e84ntw
233900150505     c                   eval      ptn2    = e84ntw
234000150505     c                   eval      ptn4    = e84ntw
234100150203      * non incasellato
234200150203     c                   z-add     TSPD(5)       SFITSP
234300150203     c                   z-add     TBRT(5)       SFIIM1
234400151118      *
234500151118     c                   z-add     TSPDR(6)      SAITSP
234600151118     c     TSPD(6)       sub       TSPDR(6)      SAITSP2
234700151118     c                   z-add     TBRTR(6)      SAIIM1
234800151118     c     TBRT(6)       sub       TBRTR(6)      SAIIM12
234900151118      *
235000150203     c                   z-add     TSPD(7)       SFETSP
235100150203     c                   z-add     TBRT(7)       SFEIM1
235200151118      *
235300151118     c                   z-add     TSPDR(8)      SAETSP
235400151118     c     TSPD(8)       sub       TSPDR(8)      SAETSP2
235500151118     c                   z-add     TBRTR(8)      SAEIM1
235600151118     c     TBRT(8)       sub       TBRTR(8)      SAEIM12
235700150203      *
235800150203      *  in un unico totale
235900151118     c                   add       TSPD(5)       TNBTSP0
236000151118     c                   add       TSPD(8)       TNBTSP0
236100151118     c                   add       TSPD(7)       TNBTSP
236200151118     c                   add       TSPD(6)       TNBTSP
236300150203      *
236400151118     c                   add       TBRT(5)       TNBIM10
236500151118     c                   add       TBRT(8)       TNBIM10
236600151118     c                   add       TBRT(7)       TNBIM1
236700151118     c                   add       TBRT(6)       TNBIM1
236800150203      *   tot
236900150203     c                   z-add     TNBIM1        TNBDEL
237000151118     c                   z-add     TNBIM10       TNBDEL0
237100150203      *
237200150203      *   DPD non incasellato
237300150203     c                   z-add     TSPD(9)       TNDTSP
237400150203     c                   z-add     TPTN(9)       TNDIMP
237500150203      *   tot
237600150203     c                   z-add     TNDIMP        TNDDEL
237700150325      *
237800150325      *   DPD non incasellato  ORM ITA-ITA
237900150325     c                   z-add     TSPD(10)      TORTSP
238000150325     c                   z-add     TPTN(10)      TORIMP
238100150325      *   tot
238200150325     c                   z-add     TORIMP        TORDEL
238300150129      *
238400150203      *  incasellati
238500150129     c                   z-add     TSPD(1)       TFITSP
238600150129     c                   z-add     TBRT(1)       TFIIM1
238700150129     c                   z-add     TPTN(1)       TFIIMP
238800150203     c     TFIIMP        sub       TFIIM1        TFIDEL
238900150129      *
239000151118     c                   z-add     TSPDR(2)      TAITSP
239100151118     c                   z-add     TBRTR(2)      TAIIM1
239200151118     c                   z-add     TPTNR(2)      TAIIMP
239300151118     c     TAIIMP        sub       TAIIM1        TAIDEL
239400151118      *
239500151118     c     TSPD(2)       sub       TSPDR(2)      TAITSP2
239600151118     c     TBRT(2)       sub       TBRTR(2)      TAIIM12
239700151118     c     TPTN(2)       sub       TPTNR(2)      TAIIMP2
239800151118     c     TAIIMP2       sub       TAIIM12       TAIDEL2
239900150129      *
240000150129     c                   z-add     TSPD(3)       TFETSP
240100150129     c                   z-add     TBRT(3)       TFEIM1
240200150129     c                   z-add     TPTN(3)       TFEIMP
240300150203     c     TFEIMP        sub       TFEIM1        TFEDEL
240400151118      *      resi
240500151118     c                   z-add     TSPDR(4)      TAETSP
240600151118     c                   z-add     TBRTR(4)      TAEIM1
240700151118     c                   z-add     TPTNR(4)      TAEIMP
240800150203     c     TAEIMP        sub       TAEIM1        TAEDEL
240900151217      *   ORM ITA-ITA
241000151217     c                   z-add     TSPD(11)      TAETSP1
241100151217     c                   z-add     TBRT(11)      TAEIM11
241200151217     c                   z-add     TPTN(11)      TAEIMP1
241300151217     c     TAEIMP1       sub       TAEIM11       TAEDEL1
241400151217      *
241500151118     c     TSPD(4)       sub       TSPDR(4)      TAETSP2
241600151118     c     TBRT(4)       sub       TBRTR(4)      TAEIM12
241700151118     c     TPTN(4)       sub       TPTNR(4)      TAEIMP2
241800151118     c     TAEIMP2       sub       TAEIM12       TAEDEL2
241900151118      *
242000150203      *  tot solo incasellato
242100151118     c     TSPD(1)       add       TSPD(4)       TOiTSPR
242200151217     c                   add       TSPD(11)      TOiTSPR
242300151118     c     TSPD(2)       add       TSPD(3)       TOiTSPC
242400151118      *
242500151118     c     TBRT(1)       add       TBRT(4)       TOiGE1R
242600151217     c                   add       TBRT(11)      TOiGE1R
242700151118     c     TBRT(2)       add       TBRT(3)       TOiGE1C
242800151118      *
242900151118     c     TPTN(1)       add       TPTN(4)       TOiGEPR
243000151217     c                   add       TPTN(11)      TOiGEPR
243100151118     c     TPTN(2)       add       TPTN(3)       TOiGEPC
243200151118      *
243300151118     c     TOiGEPR       sub       TOiGE1R       TOiGEDR
243400151118     c     TOiGEPC       sub       TOiGE1C       TOiGEDC
243500151118      *
243600150129      *
243700150429      *      Calcola i dati solo per il Depot 0124
243800150429     c                   exsr      DUNFOSS
243900150429      *
244000150429      *      Calcola i dati solo per i Depot 094*
244100150429     c                   exsr      VTG
244200150129      *
244300150129     C                   ENDSR
244400150129     C**************************************************************************
244500150429      *  deve eseguire l'estrazione dei dati come x incasellati 1 e 4
244600040901     C**************************************************************************
244700150429     C     DUNFOSS       begSR
244800041102      *
244900150429     c                   eval      x_Dunfoss = 'S'
245000150429     c                   eval       TFITSP124  =   TSPDD(1)
245100150429     c                   eval       TFIIM1124  =   TBRTD(1)
245200150429     c                   eval       TFIIMP124  =   TPTND(1)
245300150429     c                   eval       TFIDEL124  =   TDIFD(1)
245400150429      *
245500150429     C                   ENDSR
245600150429     C**************************************************************************
245700150429      *  deve eseguire l'estrazione dei dati come x incasellati 1 e 4
245800150429     C**************************************************************************
245900150429     C     VTG           begSR
246000150429      *
246100150429     c                   eval      x_VTG     = 'S'
246200150429     c                   eval       TFITSP094  =   TSPDV(1)
246300150429     c                   eval       TFIIM1094  =   TBRTV(1)
246400150429     c                   eval       TFIIMP094  =   TPTNV(1)
246500150429     c                   eval       TFIDEL094  =   TDIFV(1)
246600150429      *
246700150429     C                   ENDSR
246800150429     C**************************************************************************
246900150429      *  Scrive il File PC in 2 modi diversi
247000150429      *   a Spedizione/Riferimento/Raggruppamento         (max dett.)
247100150429      *     o a Fattura     (riepilogata)
247200150429     C**************************************************************************
247300150429     C     wrt_PCfile    begSR
247400150429      *
247500041102     C     KTas          Klist
247600041102     c                   Kfld                    ecfaas
247700041102     c                   Kfld                    ecflnp
247800041102     c                   Kfld                    ecfnrs
247900041102     c                   Kfld                    ecfnsp
248000041103      *
248100041103     C     KAco          Klist
248200041103     c                   Kfld                    acokut
248300041103     c                   Kfld                    acokcc
248400041103     c                   Kfld                    acoksc
248500040901      **
248600040901     c                   clear                   fieu085
248700040901      **
248800040901     c                   eval      f85ntw = partner
248900041014     c                   select
249000041014     c                   when      ciclo_exec = 1
249100041014     c                   select
249200041014     c                   when      volte = 1
249300041014     c                   movel     AbbFraImp     f85des
249400041014     c                   when      volte = 2
249500041014     c                   movel     AbbAssImp     f85des
249600041014     c                   when      volte = 3
249700041014     c                   movel     AbbFraExp     f85des
249800041014     c                   when      volte = 4
249900041014     c                   movel     AbbAssExp     f85des
250000041014     c                   endsl
250100041014      **
250200041014     c                   when      ciclo_exec = 2
250300041014     c                   movel     NabAddPtn     f85des
250400151217      **
250500151217     c                   when      ciclo_exec = 10
250600151217     c                   movel     ORMItaItaN    f85des
250700041014      **
250800151217     c                   when      ciclo_exec = 11
250900151217     c                   movel     ORMItaIta     f85des
251000151217      **
251100041014     c                   when      ciclo_exec = 3
251200041014     c                   select
251300041014     c                   when      volte = 1
251400041014     c                   movel     NabFraImp     f85des
251500041014     c                   when      volte = 2
251600041014     c                   movel     NabAssImp     f85des
251700041014     c                   when      volte = 3
251800041014     c                   movel     NabFraExp     f85des
251900041014     c                   when      volte = 4
252000041014     c                   movel     NabAssExp     f85des
252100041014     c                   endsl
252200041014      **
252300041014     c                   endsl
252400041014      *
252500041015     c                   movel     tipo          f85xda
252600040901     c                   move      dadata        f85dal
252700040901     c                   move      adata         f85al
252800040901      **
252900041014      ** a dettaglio
253000040901     c                   eval      f85rif = ecfrif
253100040901     c                   eval      f85spb = %editc(ecfaas:'X') + ' '
253200040901     c                                    + %editc(ecflnp:'X') + ' '
253300040901     c                                    + %editc(ecfnrs:'X') + ' '
253400040901     c                                    + %editc(ecfnsp:'X')
253500040901     c                   eval      f85aas = ecfaas
253600040901     c                   eval      f85lnp = ecflnp
253700040901     c                   eval      f85nrs = ecfnrs
253800040901     c                   eval      f85nsp = ecfnsp
253900040901     c                   eval      f85dts = datspe
254000040901     c                   eval      f85lna = ecflna
254100040901     c                   eval      f85tpb = ecftbl
254200040901     c                   eval      f85fbr = ecffbr
254300040901      **
254400041014     c                   eval      f85nft = ecfnft
254500041014     c                   eval      f85dft = ecfdft
254600040901     c                   eval      f85grp = ecfgrp
254700040901     c                   eval      f85nfp = ecfnfp
254800040901     c                   eval      f85dfp = ecfdfp
254900040901     c                   eval      f85imp = ecfimp
255000040901     c                   eval      f85imc = ecfimc
255100041012     c                   eval      f85imr = ecfimr * (-1)
255200040901     c                   eval      f85del = detdel
255300040901     c                   eval      f85tde = tipo_confr
255400041102      **
255500151217     c                   if        f85lnp  > 0
255600151217      **
255700041102      **   + mittente e destinatario
255800041102     c     Ktas          chain     titas30C
255900041102     c                   if        %found(titas30C)
256000041102     c                   eval      f85dst = tasrsd
256100041102     c                   end
256200041102      **
256300041102      ** x il mittente occorre verificare il tipo di spedizione import/export
256400041102     c     Ktas          chain     titaa30C
256500041103     c                   if        %found(titaa30C)
256600041103     c                   if        taatrc= 'O' or taatrc= 'M'
256700041102     c                   eval      f85mit = taarsc
256800041103     c                   end
256900041102     c                   end
257000041103      **  Se non l'ha trovato
257100041103     c                   if        f85mit = *blank
257200041103     c                   z-add     1             acokut
257300041103     c                   z-add     151           acokcc
257400041103     c                   z-add     tasccm        acoksc
257500041103     C     KAco          chain     Cnaco00F
257600041103     c                   if        %found(Cnaco00F)
257700041103     c                   eval      f85mit = acorag
257800041103     c                   end
257900041103     c                   end
258000151217     c                   endif
258100150504      ** UTENTE
258200150504     c                   eval      f85ute = knmus
258300150504      **
258400040901     c                   write     fieu085
258500040901      **
258600040901     C                   ENDSR
258700040901     C**************************************************************************
258800040826**
258900041014CLRPFM FILE(WFIEU85W)
259000041014CLRPFM FILE(WFIEU85F)
259100090721RUNQRY QRY(WFIEU85TQ)
