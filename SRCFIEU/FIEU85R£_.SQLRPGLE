000100040820     H DECEDIT('0,') DATEDIT(*dmy.)
000200020409     H**********************************************************************
000300041102     fFIEU84f   if   E           k disk
000400041014     fwfieu85w  if a E           k disk    usropn
000500041014     fwFIEU85f  o  a E             disk    usropn
000600160224     fwFIEU85f1 o  a E             disk    usropn
000700160615     fwFIEU85nt uf a E           k disk    usropn
000800160624     fdppdc01l  if   e           k disk
000900040826     fFIEU85P   o    E             PRINTER OFLIND(*in66) usropn
001000150129     fFIEU85P1  o    E             PRINTER OFLIND(*in67) usropn
001100041102      **
001200041102      **  x decodifiche
001300041102     ftitas30C  if   E           k disk
001400041102     ftitaa30C  if   E           k disk
001500041103     fcnaco00F  if   E           k disk
001600150429      **
001700150429      **  x decodifiche particolari DPD (monitorare clienti DANFOSS e VTG
001800150429     ftnCLD00F  if   E           k disk
001900040820     H**********************************************************************
002000040820     d KPJBA         E DS
002100040820     D tnecf00f      e DS
002200040826     D*
002300040825     D  spedbar        s             16
002400040825     D  datspe         s                   like(ecfdfp) INZ(0)
002500040824     D  deltar         s                   like(ecfimr) INZ(0)
002600040824     D  deltac         s                   like(ecfimc) INZ(0)
002700040824     D  porto          s              1    INZ(' ')
002800040824     D*
002900041103     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003000041103     D CNCR80        E DS
003100040823     D fieu84ds      e DS                  inz
003200040901     D  exlgrp                75    104
003300040820     D*
003400040820     D WLBDAT          DS
003500040820     D  G02DAT                 1      8  0
003600040820     D  G02INV                 9     16  0
003700040820     D  G02ERR                17     17
003800040820     D  G02TGI                18     22  0
003900040820      *
004000040825     D WrkSqlCmd       S           1020
004100040825     D Ln_sql          s             68    DIM(15)
004200040824      *
004300150429      * tab. con i Depot identificanti i Clienti da monitorare (solo x DPD)
004400150429     D DPD_DEP         s              4    DIM(50)
004500150429     D DPD_CLI         s              1    DIM(50)
004600150429     D x_Dunfoss       s              1    INZ(' ')
004700150429     D x_VTG           s              1    INZ(' ')
004800150429      *
004900040820     D  data_Oggi      s                   like(G02INV) INZ(0)
005000040820      *
005100041011     D  calrim         s                   like(totri1)
005200041011     D  calrid         s                   like(totrid)
005300040820     D  savdfp         s                   like(ecfdfp)
005400040820     D  savnfp         s                   like(ecfnfp)
005500040824     D  savrif         s                   like(ecfrif)
005600040825     D  savgrp         s                   like(ecfgrp)
005700040825     D  savaas         s                   like(ecfaas)
005800040825     D  savlnp         s                   like(ecflnp)
005900040825     D  savnrs         s                   like(ecfnrs)
006000040825     D  savnsp         s                   like(ecfnsp)
006100040825     D  savsped        s             16
006200040826     D  almeno_uno     s              1
006300040827     D stampa_detta    s              1
006400040831     D det_Spediz      s              1
006500151217     D ciclo_exec      s              2  0 INZ(1)
006600041008     D Salta_det       s              1
006700151126     d dataiso         s               d   datfmt(*iso)
006800160624     d dataeur         s               d   datfmt(*eur)
006900040825      *
007000040823     D GRp             S              3    DIM(50) INZ(' ')
007100040823     D IMp             S             16  3 DIM(50) INZ(0)
007200040824     D IMr             S             16  3 DIM(50) INZ(0)
007300040824     D IMc             S             16  3 DIM(50) INZ(0)
007400040824     D IMd             S             16  3 DIM(50) INZ(0)
007500040820      *
007600150129      * Totali x stampa riepilogativa
007700151217     D TSPD            S             11  0 DIM(11) INZ(0)
007800151217     D TBRT            S             15  3 DIM(11) INZ(0)
007900151217     D TPTN            S             15  3 DIM(11) INZ(0)
008000151217     D TDIF            S             15  3 DIM(11) INZ(0)
008100151118      *       RESI
008200151217     D TSPDR           S             11  0 DIM(11) INZ(0)
008300151217     D TBRTR           S             15  3 DIM(11) INZ(0)
008400151217     D TPTNR           S             15  3 DIM(11) INZ(0)
008500151217     D TDIFR           S             15  3 DIM(11) INZ(0)
008600151118      *
008700160531      *-----------------------------
008800160531      * Totali x stampa riepilogativa mese 0
008900160531     D TSPD0           S             11  0 DIM(11) INZ(0)
009000160531     D TBRT0           S             15  3 DIM(11) INZ(0)
009100160531      *       RESI
009200160531     D TSPDR0          S             11  0 DIM(11) INZ(0)
009300160531     D TBRTR0          S             15  3 DIM(11) INZ(0)
009400160531      *
009500160531      * Totali x stampa riepilogativa mese -1
009600160531     D TSPD1           S             11  0 DIM(11) INZ(0)
009700160531     D TBRT1           S             15  3 DIM(11) INZ(0)
009800160531      *       RESI
009900160531     D TSPDR1          S             11  0 DIM(11) INZ(0)
010000160531     D TBRTR1          S             15  3 DIM(11) INZ(0)
010100160531      *
010200160531      * Totali x stampa riepilogativa mese -2
010300160531     D TSPD2           S             11  0 DIM(11) INZ(0)
010400160531     D TBRT2           S             15  3 DIM(11) INZ(0)
010500160531      *       RESI
010600160531     D TSPDR2          S             11  0 DIM(11) INZ(0)
010700160531     D TBRTR2          S             15  3 DIM(11) INZ(0)
010800160601      *
010900160601      * Totali x stampa riepilogativa mese -3
011000160601     D TSPD3           S             11  0 DIM(11) INZ(0)
011100160601     D TBRT3           S             15  3 DIM(11) INZ(0)
011200160601      *       RESI
011300160601     D TSPDR3          S             11  0 DIM(11) INZ(0)
011400160601     D TBRTR3          S             15  3 DIM(11) INZ(0)
011500160531      *-----------------------------
011600150429      *    Clienti DPD Particolari a Parte
011700150429      *    DANFOSS
011800150429     D TSPDD           S             11  0 DIM(4) INZ(0)
011900150429     D TBRTD           S             15  3 DIM(4) INZ(0)
012000150429     D TPTND           S             15  3 DIM(4) INZ(0)
012100150429     D TDIFD           S             15  3 DIM(4) INZ(0)
012200150429      *    VTG
012300150429     D TSPDV           S             11  0 DIM(4) INZ(0)
012400150429     D TBRTV           S             15  3 DIM(4) INZ(0)
012500150429     D TPTNV           S             15  3 DIM(4) INZ(0)
012600150429     D TDIFV           S             15  3 DIM(4) INZ(0)
012700150429      *
012800160615     d CMD             S             62    DIM(5) CTDATA PERRCD(1)              clr file
012900090721      *
013000040820     D digits          C                   CONST('0123456789')
013100041014      *
013200071108     D AbbFraImp       C                   CONST('(1) Franchi   Import Incas. ')
013300071108     D AbbFraExp       C                   CONST('(2) Franchi   Export Incas. ')
013400071108     D AbbAssImp       C                   CONST('(3) Assegnati Import Incas. ')
013500071108     D AbbAssExp       C                   CONST('(4) Assegnati Export Incas. ')
013600120305     D NabFraImp       C                   CONST('(5) NON Incasell.BRT Fra.Imp')
013700120305     D NabFraExp       C                   CONST('(6) NON Incasell.BRT Fra.Exp')
013800120305     D NabAssImp       C                   CONST('(7) NON Incasell.BRT Ass.Imp')
013900120305     D NabAssExp       C                   CONST('(8) NON Incasell.BRT Ass.Exp')
014000071108     D NabAddPtn       C                   CONST('(9) NON Incasell.Addeb. PTN.')
014100151217     D OrmItaItaN      C                   CONST('(10)NON Incasell.ORM Ita-Ita')
014200151217     D OrmItaIta       C                   CONST('(11)Assegnati    ORM Ita-Ita')
014300040820      *****************************************************************
014400040824     c* Il ciclo può essere ripetuto 4 volte dovendo generare:
014500040824      *****************************************************************
014600151217     c* volte 1      Import Franchi         S/N        incasellati
014700151217     c*  .... 2      Import Assegnati       S/N        incasellati
014800151217     c*   ... 3      Export Franchi         S/N        incasellati
014900151217     c*    .. 4      Export Assegnati       S/N        incasellati
015000151217      *****************************************************************
015100151217     c* volte 5      Import Franchi         S/N  NON   incasellati
015200151217     c*  .... 6      Import Assegnati       S/N  NON   incasellati
015300151217     c*   ... 7      Export Franchi         S/N  NON   incasellati
015400151217     c*    .. 8      Export Assegnati       S/N  NON   incasellati
015500040824     c*****************************************************************
015600151217     c*    .. 9      solo del Partner            NON   incasellati
015700151217     c*    ..10      solo del Partner   ORM ITA  NON   incasellati
015800151217     c*    ..11      solo del Partner   ORM ITA        incasellati
015900151217     c*****************************************************************
016000040903     c*  Ciclo principale
016100040903     c                   exsr      ciclo_1
016200040903     c*  Ciclo secondario
016300040903     c                   if        e84sna <> 'E' or e84sni <> 'E'
016400040903     c                   exsr      ciclo_2
016500040903     c                   end
016600040903      *
016700150129      *  stampa di Riepilogo finale
016800150129     c                   open      FIEU85P1
016900160530     c                   exsr      STAMPA_Riep
017000150205     c                   close     FIEU85P1
017100150129      *
017200020326     C                   SETON                                        LR
017300040820     C**************************************************************************
017400040820     C* INIZIALIZZAZIONE
017500040820     C**************************************************************************
017600040820     C     *inzsr        BEGSR
017700040820      **
017800040820     C     *ENTRY        PLIST
017900040820     C                   PARM                    KPJBA
018000040823     c                   movel     kpjbu         fieu84ds
018100040820      *
018200041103     C                   Z-ADD     1             CODUT
018300041103     C                   CALL      'X§PARUT'
018400041103     C                   PARM                    UT§DSE
018500041103     C                   MOVEL     REC80         CNCR80
018600041103     C                   MOVEL     RAGUT         RSUT             20
018700041103     C*
018800040820      *  imposta UDATE di elaborazione
018900040820     C                   move      *date         G02DAT
019000040820     C                   MOVE      *ZEROS        G02INV
019100040820     C                   MOVE      *BLANKS       G02ERR
019200040820     C                   CALL      'XSRDA8'
019300040820     C                   PARM                    WLBDAT
019400040820     C                   Z-ADD     G02INV        data_oggi
019500040820     C                   move      data_oggi     day
019600040820     C                   TIME                    W0140            14 0
019700040820     C                   MOVEl     W0140         ore
019800040901      **
019900160624     C                   MOVE      W0140         oggi              8 0
020000160624     c                   move      oggi          dataeur
020100160624     c                   move      dataeur       dataiso
020200160624     c                   move      dataiso       oggig             8 0
020300160624      *
020400041015     c                   clear                   tipo             15
020500040823     c                   eval      partner = e84ntw
020600150504      **
020700150504     c                   setoff                                       44
020800150504     c                   if        E84NTW = 'DPD'
020900150504     c                   setoN                                        44
021000160615      **
021100160615     C                   z-add     62            lung             15 5
021200160615     C                   clear                   comman
021300160615     C                   movea     cmd(5)        comman           80
021400160615     C                   call      'QCMDEXC'
021500160615     C                   parm                    comman
021600160615     C                   parm                    lung
021700160615     c                   open      wfieu85nt
021800160615      **
021900150504     c                   end
022000150504      **
022100040901      **  x incasellamento
022200040901     c                   if        e84dii <> *zero
022300040901     c                   move      e84dii        dadata
022400040901     c                   move      e84dfi        adata
022500041103     c                   move(p)   'Incasellate' periodo
022600041015     c                   eval      tipo = 'Incasellamento'
022700040824     c                   end
022800040901      **  x spedizione
022900040901     c                   if        e84dis <> *zero
023000040901     c                   move      e84dis        dadata
023100040901     c                   move      e84dfs        adata
023200041103     c                   move(p)   'Spedizioni'  periodo
023300041015     c                   eval      tipo = 'Spedizione   '
023400040820     c                   end
023500040901      **  x fattura
023600040901     c                   if        e84dif <> *zero
023700040901     c                   move      e84dif        dadata
023800040901     c                   move      e84dff        adata
023900041103     c                   move(p)   'Fatturate '  periodo
024000041015     c                   eval      tipo = 'Fattura      '
024100040901     c                   end
024200040824      **  prima pagina
024300040831     c                   eval      det_spediz = e84det
024400040831     c                   eval      totf   = e84tof
024500040824     c                   eval      totr   = e84tor
024600040824     c                   eval      tots   = e84tos
024700040825     c                   eval      totp   = e84top
024800040824     c                   eval      spbnoa = e84sna
024900040824     c                   eval      spbnoi = e84sni
025000040824     c                   eval      delta  = e84sde
025100040824     c                   eval      filepc = e84fpc
025200040827     c                   eval      grp1   = e84gr1
025300040827     c                   eval      grp2   = e84gr2
025400040827     c                   eval      grp3   = e84gr3
025500040827     c                   eval      grp4   = e84gr4
025600040827     c                   eval      grp5   = e84gr5
025700040827     c                   eval      grp6   = e84gr6
025800040827     c                   eval      grp7   = e84gr7
025900040827     c                   eval      grp8   = e84gr8
026000040827     c                   eval      grp9   = e84gr9
026100040827     c                   eval      grp0   = e84gr0
026200040827     c                   if        e84gie = 'I'
026300040827     c                   eval      gried  = 'Inclusi'
026400040827     c                   else
026500040827     c                   eval      gried  = 'Esclusi'
026600040827     c                   end
026700160531      **
026800160531      ** imposto i valori dei 3 mesi da analizzare per le NON incasellate BRT
026900160531     c                   move      dadata        data80            8 0
027000160531     c                   move      data80        mmgg              4 0
027100160531      **
027200160531     c                   movel     mmgg          MES0              2 0
027300160531     c                   if        mes0 = 1
027400160531     c                   z-add     12            MES1
027500160531     c                   else
027600160531     c     mes0          sub       1             MES1              2 0
027700160531     c                   end
027800160531     c                   if        mes1 = 1
027900160531     c                   z-add     12            MES2
028000160531     c                   else
028100160531     c     mes1          sub       1             MES2              2 0
028200160531     c                   end
028300160601     c                   if        mes2 = 1
028400160601     c                   z-add     12            MES3
028500160601     c                   else
028600160601     c     mes2          sub       1             MES3              2 0
028700160601     c                   end
028800040824      **
028900150203      **  se deve stampare solo la pagina di riepilogo  e non stampare
029000150203      **   tutte le altre 9 stampe di dettaglio
029100150203     c     e84rie        comp      'S'                                    22
029200150203      **
029300040826      **  se deve creare il file PC
029400040826     c                   if        e84fpc = 'S'
029500090721      **
029600040826     C                   z-add     62            lung             15 5
029700040826     C                   clear                   comman
029800040826     C                   movea     cmd(2)        comman           80
029900040826     C                   call      'QCMDEXC'
030000040826     C                   parm                    comman
030100040826     C                   parm                    lung
030200041014     c                   open      wfieu85f
030300160615      **
030400090721      **  DEVE RESTITUIRE  i totali fattura Partner in modo da avere la certezza
030500090721      **    che tutti i dati siano stati importati sul file di esportazione
030600090721      **    e procedere correttamente con le verifiche.
030700090721      **
030800090901     c                   if        E84NTW = 'DPD'
030900090721     C                   z-add     62            lung             15 5
031000090721     C                   clear                   comman
031100090721     C                   movea     cmd(3)        comman           80
031200090721     C                   call      'QCMDEXC'
031300090721     C                   parm                    comman
031400090721     C                   parm                    lung
031500160224      **
031600160224     C                   z-add     62            lung             15 5
031700160224     C                   clear                   comman
031800160224     C                   movea     cmd(4)        comman           80
031900160224     C                   call      'QCMDEXC'
032000160224     C                   parm                    comman
032100160224     C                   parm                    lung
032200160615      **
032300160224     c                   open      wfieu85f1
032400160615      **
032500090901     c                   end
032600090721      **
032700040826     c                   end
032800040826      **
032900150429      **    Solo per DPD occorre pre-caricare la schiera dei clienti Particolari
033000150429      **     da monitorare (DANFOSS e VTG)
033100150429      **    Imposta due schiere con il depot nella prima schiera ed in parallelo
033200150429      **    con la D=DANFOSS e V=VTG sulla seconda schiera leggendo dal TNCLD00F
033300150429     c                   if        E84NTW = 'DPD'
033400150429      **
033500150429     c                   z-add     0             nrCLD             3 0
033600150429     c                   clear                   DPD_DEP
033700150429     c                   clear                   DPD_CLI
033800150429      **
033900150429      **  max. 50 depot da monitorare
034000150429     c                   read      tnCLD00f
034100150429     c                   dow       not %EoF(tnCLD00f) and nrCLD < 50
034200150429      **
034300150429     c                   add       1             nrCLD
034400150429     c                   if        cldCOD = 'DANFOSS'
034500150429     c                   eval      DPD_DEP(nrCLD) =  %editc(cldDEP:'X')
034600150429     c                   eval      DPD_CLI(nrCLD) = 'D'
034700150429     c                   elseif    cldCOD = 'VTG'
034800150429     c                   eval      DPD_DEP(nrCLD) =  %editc(cldDEP:'X')
034900150429     c                   eval      DPD_CLI(nrCLD) = 'V'
035000150429     c                   end
035100150429      **
035200150429     c                   read      tnCLD00f
035300150429     c                   enddo
035400150429      **
035500150429     c                   end
035600150429      **
035700040820     C                   ENDSR
035800040824     C**************************************************************************
035900041007     C* Stampa la testata con il salto a nuova pagina
036000040824     C**************************************************************************
036100041007     C     Prt_testa     BEGSR                                                  *
036200040824      **
036300041007     c                   add       1             pagnum
036400150203     c  n22              write     testa
036500041007      **
036600041007     C                   ENDSR
036700041007     C**************************************************************************
036800041007     C* Ciclo Principale
036900041007     C**************************************************************************
037000041007     C     Ciclo_1       BEGSR                                                  *
037100041007      **
037200040903     c                   DO        4             VOLTE             3 0
037300150129     c                   z-add     VOLTE         vlt               3 0
037400041007     c                   z-add     0             pagnum
037500040903     c*
037600040903     c* se non richiesta evita il giro
037700040903     c                   if        volte = 1 and e84if ='N' or                  Fr.Import
037800040903     c                             volte = 2 and e84ia ='N' or                  As.Import
037900040903     c                             volte = 3 and e84ef ='N' or                  Fr.Export
038000040903     c                             volte = 4 and e84ea ='N'                     As.Export
038100040903     c                   iter
038200040903     c                   end
038300040903     c*
038400040903     c*   Apre il file di stampa e predispone a stampare testata con 66 = On
038500040903     c                   open      fieu85p
038600040903     c* compongo istruzione sql
038700040903     c                   exsr      istruz
038800040903     c*  stampa prima pagina
038900040903     c                   exsr      pripag
039000041007     c                   seton                                        66
039100041011     c*
039200151126     c* Attenzione: solo se selezionato una (%) di delta
039300041011     c* esegue un ciclo precedentemente per vedere quali spedizioni non stampare poichè
039400041011     c* non presenti entro i limiti richiesti.    --> CREA un WORKFILE con le Spedizioni.
039500041011     c                   if         e84sde <> 99
039600041011     c                   exsr      pre_ciclo
039700041011     c                   end
039800040903     c*
039900040903     C/EXEC SQL
040000040903     C+ PREPARE S1 FROM :WrkSqlCmd
040100040903     C/END-EXEC
040200040903
040300040903     C/EXEC SQL
040400040903     C+ DECLARE A1 CURSOR FOR S1
040500040903     C/END-EXEC
040600040903
040700040903     C/EXEC SQL
040800040903     C+ OPEN A1
040900040903     C/END-EXEC
041000041011     c                   clear                   almeno_uno
041100040903     C                   DOU       SqlCod <> 0
041200040903     C/EXEC SQL
041300040903     C+ FETCH NEXT FROM A1 INTO :tnecf00f, :spedbar, :datspe, :deltar,
041400040903     C+ :deltac, :porto
041500040903     C/END-EXEC
041600040903     C                   SELECT
041700041007     **
041800041007     C                   WHEN      SqlCod = 100
041900041007     c* a fine file
042000041007     c* Totali x rotture
042100041007     c                   if        almeno_uno = 'S'
042200041007     c                   exsr      TOTals
042300041007     c                   else
042400041007      *                 * ------------------ *
042500150203     c  n22              write     NODATI
042600041007      *                 * ------------------ *
042700041007     c                   end
042800041007     c                   leave
042900041007     c*
043000041007     C                   WHEN      SqlCod < 0
043100041007     C                   seton                                        H1
043200041007     c                   goto      fine_ciclo
043300040903     c*
043400041007     C                   OTHER
043500041011     **
043600041011     c* Controlla se richiesta stampa x Spedizione con limite di DELTA %
043700041011     c*  se la spedizione deve essere saltata.
043800041011     c                   eval      salta_det = 'S'
043900041011     **
044000041011     c                   if        e84sde <> 99
044100041014     c     spedbar       chain     wfieu85w
044200041014     c                   if        %found(wfieu85w)
044300041011     c                   eval      salta_det = 'N'
044400041011     c                   end
044500041011     c                   end
044600040903     **
044700040903     c* Totali x rotture
044800040903     c                   exsr      TOTals
044900040903     c* DETTAGLIO
045000040903     c                   if        *in66
045100040903      *                 * ------------------ *
045200041007     c                   exsr      Prt_testa
045300041007      *                 * ------------------ *
045400150203     c  n22              write     tesdetta
045500040903      *                 * ------------------ *
045600040903     c                   eval      prima_volta = 'N'
045700040903     c                   setoff                                       66
045800040903     c                   end
045900040903      **
046000040903      ** se non deve stampare il dettaglio:
046100040903      **    x spedizioni non addebitate                (quindi totalizzarle a parte)
046200040903      **    x spedizioni addebitate ma non incasellate (quindi totalizzarle a parte)
046300040903     c                   eval      stampa_detta = 'S'
046400040903      **
046500040903     c                   if         (e84sna <>'S' and
046600130318     c                                 ecfdfp = 0)
046700040903     c                             or
046800041011     c                              (e84sni <>'S' and
046900041012     c                               ecfdft = 0)
047000040903     c                             or
047100040903     c                              (e84det = 'N')
047200041008     c                             or
047300041011     c                              (salta_det = 'S' and e84sde <> 99 and
047400041011     c                               e84det = 'S')
047500041008      **
047600040903     c                   eval      stampa_detta = 'N'
047700040903     c                   end
047800040903     c* stampa dettaglio
047900040903     c                   exsr      imp_rig_det
048000040903      **
048100040903      **   abilita esclusivamente la stampa della riga di dettaglio
048200040903     c                   if        stampa_detta = 'S'
048300150203     c  n22              write     dettagli
048400040903     c                   end
048500040903     c* Totalizzo
048600040903     c                   exsr      totalizza
048700040903     c*
048800040903     C                   ENDSL
048900040903
049000040903     C                   ENDDO
049100040903     C/EXEC SQL
049200040903     C+ CLOSE A1
049300040903     C/END-EXEC
049400040903     c*
049500040903     C* totali generali
049600041007     c                   if        almeno_uno = 'S'
049700040903     c                   if        *in66
049800040903      *                 * ------------------ *
049900041007     c                   exsr      Prt_testa
050000041007      *                 * ------------------ *
050100150203     c  n22              write     tesdetta
050200040903      *                 * ------------------ *
050300040903     c                   setoff                                       66
050400040903     c                   end
050500150203     c  n22              write     totale
050600041007     c                   end
050700041007      *
050800041007     c     fine_ciclo    tag
050900041007      *
051000040903     c                   clear                   totgep
051100040903     c                   clear                   totge1
051200040903     c                   clear                   totge2
051300040903     c                   clear                   totged
051400040903     c                   close     fieu85p
051500040903      *
051600040903     c                   ENDDO
051700040903      **
051800040903     C                   ENDSR
051900040903     C**************************************************************************
052000040903     C* Ciclo Spedizioni anomale stampate a parte
052100040903     C*  addebitate ma non associate e Non Addebitate
052200040903     C**************************************************************************
052300040903     C     Ciclo_2       BEGSR                                                  *
052400040903      **
052500151126      **    x le NON incasellate cerca la data più antica
052600151126     c                   clear                   data_MINIMA       8 0
052700151126      **
052800040903      **  Addebitate e non Associate
052900040903     c                   if        e84sni <> 'E'
053000160530      *
053100040903     c                   eval      ciclo_exec = 2
053200040903     c                   exsr      Addeb_No_Assoc
053300160530      *
053400151217     c                   eval      ciclo_exec = 10
053500151217     c                   exsr      ORM_ITA_ITA_N
053600160530      *
053700151217     c                   eval      ciclo_exec = 11
053800151217     c                   exsr      ORM_ITA_ITA
053900040903     c                   end
054000040903      **
054100040903      **  Non Addebitate
054200040903     c                   if        e84sna <> 'E'
054300040903     c                   eval      ciclo_exec = 3
054400040903     c                   exsr      Non_Addebitate
054500040903     c                   end
054600040903      **
054700040903     C                   ENDSR
054800040903     C**************************************************************************
054900040903     C* Addebitate dal Partner ma non associate
055000040903     C**************************************************************************
055100040903     C     Addeb_No_AssocBEGSR
055200040903      **
055300040903     c*   Apre il file di stampa e predispone a stampare testata con 66 = On
055400040903     c                   open      fieu85p
055500150129     c                   z-add     9             vlt
055600041007     c                   z-add     0             pagnum
055700040903     c* compongo istruzione sql
055800040903     c                   exsr      istruz2
055900040903     c*  stampa prima pagina
056000040903     c                   exsr      pripag
056100041007     c                   seton                                        66
056200040903     C/EXEC SQL
056300040903     C+ PREPARE S2 FROM :WrkSqlCmd
056400040903     C/END-EXEC
056500040903
056600040903     C/EXEC SQL
056700040903     C+ DECLARE B1 CURSOR FOR S2
056800040903     C/END-EXEC
056900040903
057000040903     C/EXEC SQL
057100040903     C+ OPEN B1
057200040903     C/END-EXEC
057300041011     c                   clear                   almeno_uno
057400040903     C                   DOU       SqlCod <> 0
057500040903     C/EXEC SQL
057600040903     C+ FETCH NEXT FROM B1 INTO :tnecf00f, :spedbar, :datspe, :deltar,
057700040903     C+ :deltac, :porto
057800040903     C/END-EXEC
057900040903     C                   SELECT
058000041007     c*
058100041007     C                   WHEN      SqlCod = 100
058200041007     c* a fine file
058300041007     c* Totali x rotture
058400041007     c                   if        almeno_uno = 'S'
058500041007     c                   exsr      TOTals
058600041007     c                   else
058700041007      *                 * ------------------ *
058800150203     c  n22              write     tesdetta
058900041007      *                 * ------------------ *
059000150203     c  n22              write     NODATI
059100041007     c                   end
059200041007     c                   leave
059300041007     **
059400041007     C                   WHEN      SqlCod < 0
059500041007     C                   seton                                        H2
059600041007     c                   goto      fine_ciclo2
059700040903     c*
059800041007     C                   OTHER
059900040903     c* Totali x rotture
060000040903     c                   exsr      TOTals
060100040903     c* DETTAGLIO
060200040903     c                   if        *in66
060300040903      *                 * ------------------ *
060400041007     c                   exsr      Prt_testa
060500041007      *                 * ------------------ *
060600150203     c  n22              write     tesdetta
060700040903      *                 * ------------------ *
060800040903     c                   eval      prima_volta = 'N'
060900040903     c                   setoff                                       66
061000040903     c                   end
061100040903      **
061200040903      ** se non deve stampare il dettaglio quindi solo i totali
061300040903      **    x spedizioni addebitate ma non incasellate
061400040903     c                   eval      stampa_detta = 'S'
061500040903     c                   if         (e84sni <>'S' and ecfdfp > 0)
061600040903     c                   eval      stampa_detta = 'N'
061700040903     c                   end
061800040903     c* stampa dettaglio
061900040903     c                   exsr      imp_rig_det
062000040903      **
062100040903      **   abilita esclusivamente la stampa della riga di dettaglio
062200040903     c                   if        stampa_detta = 'S'
062300150203     c  n22              write     dettagli
062400040903     c                   end
062500040903     c* Totalizzo
062600040903     c                   exsr      totalizza
062700041014     c                   exsr      tot_A_Parte
062800040903     c*
062900040903     C                   ENDSL
063000040903
063100040903     C                   ENDDO
063200040903     C/EXEC SQL
063300040903     C+ CLOSE B1
063400040903     C/END-EXEC
063500040903     C* totali di riepilogo
063600041007     c                   if        almeno_uno = 'S'
063700040903     c                   exsr      tot_riepilogo
063800041007     c                   end
063900041007      ***
064000041007     c     fine_ciclo2   tag
064100040903     c                   clear                   totgep
064200040903     c                   clear                   totge1
064300040903     c                   clear                   totge2
064400040903     c                   clear                   totged
064500040903     c                   close     fieu85p
064600040903      **
064700040903     C                   ENDSR
064800150325     C**************************************************************************
064900150325     C* Accreditate dal Partner ma non associate perchè ORM Italia-Italia
065000150325     C*   quindi la LNP o la LNA non hanno linee PTN.
065100150325     C**************************************************************************
065200151217     C     ORM_ITA_ITA_N BEGSR
065300150325      **
065400150325     c*   Apre il file di stampa e predispone a stampare testata con 66 = On
065500150325     c                   open      fieu85p
065600150325     c                   z-add     10            vlt
065700150325     c                   z-add     0             pagnum
065800150325     c* compongo istruzione sql
065900151217     c                   exsr      istruz2_ORM_N
066000150325     c*  stampa prima pagina
066100150325     c                   exsr      pripag
066200150325     c                   seton                                        66
066300150325     C/EXEC SQL
066400150325     C+ PREPARE O2 FROM :WrkSqlCmd
066500150325     C/END-EXEC
066600150325
066700150325     C/EXEC SQL
066800150325     C+ DECLARE O1 CURSOR FOR O2
066900150325     C/END-EXEC
067000150325
067100150325     C/EXEC SQL
067200150325     C+ OPEN O1
067300150325     C/END-EXEC
067400150325     c                   clear                   almeno_uno
067500150325     C                   DOU       SqlCod <> 0
067600150325     C/EXEC SQL
067700150325     C+ FETCH NEXT FROM O1 INTO :tnecf00f, :spedbar, :datspe, :deltar,
067800150325     C+ :deltac, :porto
067900150325     C/END-EXEC
068000150325     C                   SELECT
068100150325     c*
068200150325     C                   WHEN      SqlCod = 100
068300150325     c* a fine file
068400150325     c* Totali x rotture
068500150325     c                   if        almeno_uno = 'S'
068600150325     c                   exsr      TOTals
068700150325     c                   else
068800150325      *                 * ------------------ *
068900150325     c  n22              write     tesdetta
069000150325      *                 * ------------------ *
069100150325     c  n22              write     NODATI
069200150325     c                   end
069300150325     c                   leave
069400150325     **
069500150325     C                   WHEN      SqlCod < 0
069600150325     C                   seton                                        H2
069700151217     c                   goto      fine_ciclo2ORM
069800150325     c*
069900150325     C                   OTHER
070000150325     c* Totali x rotture
070100150325     c                   exsr      TOTals
070200150325     c* DETTAGLIO
070300150325     c                   if        *in66
070400150325      *                 * ------------------ *
070500150325     c                   exsr      Prt_testa
070600150325      *                 * ------------------ *
070700150325     c  n22              write     tesdetta
070800150325      *                 * ------------------ *
070900150325     c                   eval      prima_volta = 'N'
071000150325     c                   setoff                                       66
071100150325     c                   end
071200150325      **
071300150325      ** se non deve stampare il dettaglio quindi solo i totali
071400150325      **    x spedizioni addebitate ma non incasellate
071500150325     c                   eval      stampa_detta = 'S'
071600150325     c                   if         (e84sni <>'S' and ecfdfp > 0)
071700150325     c                   eval      stampa_detta = 'N'
071800150325     c                   end
071900150325     c* stampa dettaglio
072000150325     c                   exsr      imp_rig_det
072100150325      **
072200150325      **   abilita esclusivamente la stampa della riga di dettaglio
072300150325     c                   if        stampa_detta = 'S'
072400150325     c  n22              write     dettagli
072500150325     c                   end
072600150325     c* Totalizzo
072700150325     c                   exsr      totalizza
072800150325     c                   exsr      tot_A_Parte
072900150325     c*
073000150325     C                   ENDSL
073100150325
073200150325     C                   ENDDO
073300150325     C/EXEC SQL
073400150325     C+ CLOSE O1
073500150325     C/END-EXEC
073600150325     C* totali di riepilogo
073700150325     c                   if        almeno_uno = 'S'
073800150325     c                   exsr      tot_riepilogo
073900150325     c                   end
074000150325      ***
074100150325     c     fine_ciclo2ORMtag
074200150325     c                   clear                   totgep
074300150325     c                   clear                   totge1
074400150325     c                   clear                   totge2
074500150325     c                   clear                   totged
074600150325     c                   close     fieu85p
074700150325      **
074800150325     C                   ENDSR
074900151217     C**************************************************************************
075000151217     C* Accreditate dal Partner e associate a BOLLE ITA-ITA da ORM IMPORT
075100151217     C*   quindi la LNP o la LNA non hanno linee PTN.
075200151217     C**************************************************************************
075300151217     C     ORM_ITA_ITA   BEGSR
075400151217      **
075500151217     c*   Apre il file di stampa e predispone a stampare testata con 66 = On
075600151217     c                   open      fieu85p
075700151217     c                   z-add     11            vlt
075800151217     c                   z-add     0             pagnum
075900151217     c* compongo istruzione sql
076000151217     c                   exsr      istruz2_ORM
076100151217     c*  stampa prima pagina
076200151217     c                   exsr      pripag
076300151217     c                   seton                                        66
076400151218     c                   eval      tipo_confr = 'R'
076500151217     C/EXEC SQL
076600151217     C+ PREPARE O4 FROM :WrkSqlCmd
076700151217     C/END-EXEC
076800151217
076900151217     C/EXEC SQL
077000151217     C+ DECLARE O3 CURSOR FOR O4
077100151217     C/END-EXEC
077200151217
077300151217     C/EXEC SQL
077400151217     C+ OPEN O3
077500151217     C/END-EXEC
077600151217     c                   clear                   almeno_uno
077700151217     C                   DOU       SqlCod <> 0
077800151217     C/EXEC SQL
077900151217     C+ FETCH NEXT FROM O3 INTO :tnecf00f, :spedbar, :datspe, :deltar,
078000151217     C+ :deltac, :porto
078100151217     C/END-EXEC
078200151217     C                   SELECT
078300151217     c*
078400151217     C                   WHEN      SqlCod = 100
078500151217     c* a fine file
078600151217     c* Totali x rotture
078700151217     c                   if        almeno_uno = 'S'
078800151217     c                   exsr      TOTals
078900151217     c                   else
079000151217      *                 * ------------------ *
079100151217     c  n22              write     tesdetta
079200151217      *                 * ------------------ *
079300151217     c  n22              write     NODATI
079400151217     c                   end
079500151217     c                   leave
079600151217     **
079700151217     C                   WHEN      SqlCod < 0
079800151217     C                   seton                                        H2
079900151217     c                   goto      fine_ciclo2ITA
080000151217     c*
080100151217     C                   OTHER
080200151217     c* Totali x rotture
080300151217     c                   exsr      TOTals
080400151217     c* DETTAGLIO
080500151217     c                   if        *in66
080600151217      *                 * ------------------ *
080700151217     c                   exsr      Prt_testa
080800151217      *                 * ------------------ *
080900151217     c  n22              write     tesdetta
081000151217      *                 * ------------------ *
081100151217     c                   eval      prima_volta = 'N'
081200151217     c                   setoff                                       66
081300151217     c                   end
081400151217      **
081500151217      ** se non deve stampare il dettaglio quindi solo i totali
081600151217      **    x spedizioni addebitate ma non incasellate
081700151217     c                   eval      stampa_detta = 'S'
081800151217     c                   if         (e84sni <>'S' and ecfdfp > 0)
081900151217     c                   eval      stampa_detta = 'N'
082000151217     c                   end
082100151217     c* stampa dettaglio
082200151217     c                   exsr      imp_rig_det
082300151217      **
082400151217      **   abilita esclusivamente la stampa della riga di dettaglio
082500151217     c                   if        stampa_detta = 'S'
082600151217     c  n22              write     dettagli
082700151217     c                   end
082800151217     c* Totalizzo
082900151217     c                   exsr      totalizza
083000151217     c                   exsr      tot_A_Parte
083100151217     c*
083200151217     C                   ENDSL
083300151217
083400151217     C                   ENDDO
083500151217     C/EXEC SQL
083600151217     C+ CLOSE O3
083700151217     C/END-EXEC
083800151217     C* totali di riepilogo
083900151217     c                   if        almeno_uno = 'S'
084000151217     c                   exsr      tot_riepilogo
084100151217     c                   end
084200151217      ***
084300151217     c     fine_ciclo2ITAtag
084400151217     c                   clear                   totgep
084500151217     c                   clear                   totge1
084600151217     c                   clear                   totge2
084700151217     c                   clear                   totged
084800151217     c                   close     fieu85p
084900151217      **
085000151217     C                   ENDSR
085100040903     C**************************************************************************
085200040903     C* Non Addebitate dal Partner
085300040903     C**************************************************************************
085400040903     C     Non_AddebitateBEGSR                                                  *
085500040903      **
085600040903      **    Non Addebitate ma solo nostre spedizioni
085700040903     c                   DO        4             VOLTE             3 0
085800150129     c     VOLTE         add       4             vlt
085900041007     c                   z-add     0             pagnum
086000040903     c*
086100040903     c* se non richiesta evita il giro
086200040903     c                   if        volte = 1 and e84if ='N' or                  Fr.Import
086300040903     c                             volte = 2 and e84ia ='N' or                  As.Import
086400040903     c                             volte = 3 and e84ef ='N' or                  Fr.Export
086500040903     c                             volte = 4 and e84ea ='N'                     As.Export
086600040903     c                   iter
086700040903     c                   end
086800040903     c*
086900040903     c*   Apre il file di stampa e predispone a stampare testata con 66 = On
087000040903     c                   open      fieu85p
087100040903     c* compongo istruzione sql
087200040903     c                   exsr      istruz3
087300040903     c*  stampa prima pagina
087400040903     c                   exsr      pripag
087500041007     c                   seton                                        66
087600040903     C/EXEC SQL
087700040903     C+ PREPARE S3 FROM :WrkSqlCmd
087800040903     C/END-EXEC
087900040903
088000040903     C/EXEC SQL
088100040903     C+ DECLARE C1 CURSOR FOR S3
088200040903     C/END-EXEC
088300040903
088400040903     C/EXEC SQL
088500040903     C+ OPEN C1
088600040903     C/END-EXEC
088700041011     c                   clear                   almeno_uno
088800040903     C                   DOU       SqlCod <> 0
088900040903     C/EXEC SQL
089000040903     C+ FETCH NEXT FROM C1 INTO :tnecf00f, :spedbar, :datspe, :deltar,
089100040903     C+ :deltac, :porto
089200040903     C/END-EXEC
089300040903     C                   SELECT
089400041007     c*
089500041007     C                   WHEN      SqlCod = 100
089600041007     c* a fine file
089700041007     c* Totali x rotture
089800041007     c                   if        almeno_uno = 'S'
089900041007     c                   exsr      TOTals
090000041007     c                   else
090100041007      *                 * ------------------ *
090200150203     c  n22              write     tesdetta
090300041007      *                 * ------------------ *
090400150203     c  n22              write     NODATI
090500041007     c                   end
090600041007     c                   leave
090700041007     **
090800041007     C                   WHEN      SqlCod < 0
090900041007     C                   seton                                        H3
091000041007     c                   goto      fine_ciclo3
091100040903     c*
091200041007     C                   OTHER
091300040903     c* Totali x rotture
091400040903     c                   exsr      TOTals
091500040903     c* DETTAGLIO
091600040903     c                   if        *in66
091700040903      *                 * ------------------ *
091800041007     c                   exsr      Prt_testa
091900041007      *                 * ------------------ *
092000150203     c  n22              write     tesdetta
092100040903      *                 * ------------------ *
092200040903     c                   eval      prima_volta = 'N'
092300040903     c                   setoff                                       66
092400040903     c                   end
092500040903      **
092600040903      ** se non deve stampare il dettaglio quindi solo i totali
092700040903      **    x spedizioni non addebitate
092800040903     c                   eval      stampa_detta = 'S'
092900040903     c                   if         (e84sna <>'S' and
093000130318     c                                 ecfdfp = 0)
093100040903     c                   eval      stampa_detta = 'N'
093200040903     c                   end
093300040903     c* stampa dettaglio
093400040903     c                   exsr      imp_rig_det
093500040903      **
093600040903      **   abilita esclusivamente la stampa della riga di dettaglio
093700040903     c                   if        stampa_detta = 'S'
093800150203     c  n22              write     dettagli
093900040903     c                   end
094000040903     c* Totalizzo
094100040903     c                   exsr      totalizza
094200041014     c                   exsr      tot_A_Parte
094300160531     c                   if          e84ntw = 'DPD'
094400160530     c                   exsr      tot_Mensili
094500160531     c                   end
094600040903     c*
094700040903     C                   ENDSL
094800040903
094900040903     C                   ENDDO
095000040903     C/EXEC SQL
095100040903     C+ CLOSE C1
095200040903     C/END-EXEC
095300040903     C* totali di riepilogo
095400041007     c                   if        almeno_uno = 'S'
095500040903     c                   exsr      tot_riepilogo
095600041007     c                   end
095700041007      *
095800041007     c     fine_ciclo3   tag
095900041007      *
096000040903     c                   clear                   totgep
096100040903     c                   clear                   totge1
096200040903     c                   clear                   totge2
096300040903     c                   clear                   totged
096400040903     c                   close     fieu85p
096500040903      *
096600040903     c                   ENDDO
096700040903      **
096800040903     C                   ENDSR
096900040903     C**************************************************************************
097000040903     C* prima pagina
097100040903     C**************************************************************************
097200040903     C     pripag        BEGSR                                                  *
097300040903      **
097400040824     c                   movea     '0000'        *in(1)
097500040825     c                   movea     '0000'        *in(11)
097600040824     c                   eval      *in(volte) = *on
097700040826      **
097800040826      ** Descrizioni varie di testata
097900041013     c                   clear                   desvari
098000040826     c                   select
098100040826     c                   when      volte = 1
098200040826     c                   eval      desvari = 'Import Franchi'
098300040826     c                   when      volte = 2
098400040826     c                   eval      desvari = 'Import Assegnati'
098500040826     c                   when      volte = 3
098600040826     c                   eval      desvari = 'Export Franchi'
098700040826     c                   when      volte = 4
098800040826     c                   eval      desvari = 'Export Assegnati'
098900040826     c                   endsl
099000040826      **
099100040903     c                   clear                   desprt
099200040903     c                   select
099300040903     c                   when      ciclo_exec = 1
099400040903     c                   eval      desprt = 'Addebitate e Associate' +
099500120305     c                             ' a sped.   B R T '
099600040903     c                   when      ciclo_exec = 2
099700040903     c                   eval      desprt = 'Addebitate e NON Associate'+
099800120305     c                             ' a sped.   B R T '
099900040903     c                   when      ciclo_exec = 3
100000040903     c                   eval      desprt = 'Non Addebitate dal Partner'
100100040903     c                   endsl
100200040903      **
100300040825     c                   select
100400040825     c                   when      totf = 'S'
100500040825     c                   seton                                        11
100600040826     c                   eval      desvari = %TrimR(desvari) +
100700040826     c                             '        Totali x Fattura'
100800040825     c                   when      totr = 'S'
100900040825     c                   seton                                        12
101000040826     c                   eval      desvari = %TrimR(desvari) +
101100040826     c                             '        Totali x Gruppo'
101200040825     c                   when      tots = 'S'
101300040825     c                   seton                                        13
101400040826     c                   eval      desvari = %TrimR(desvari) +
101500040826     c                             '        Totali x Spedizione'
101600040825     c                   when      totp = 'S'
101700040825     c                   seton                                        14
101800040826     c                   eval      desvari = %TrimR(desvari) +
101900040826     c                             '        Totali x Riferimento'
102000040825     c                   endsl
102100040824      **
102200040825     c                   clear                   prima_volta       1
102300040825     c                   clear                   tipo_confr        1
102400040826      **
102500040824      ** confronto con Competenze
102600040824     c                   if        volte = 1 and e84ifc = 'X' or
102700040824     c                             volte = 2 and e84iac = 'X' or
102800040824     c                             volte = 3 and e84efc = 'X' or
102900040824     c                             volte = 4 and e84eac = 'X'
103000040824     c                   eval      tipo_confr = 'C'
103100040825     c                   eval      ifdes  = 'Competenze'
103200040825     c                   eval      iades  = 'Competenze'
103300040825     c                   eval      efdes  = 'Competenze'
103400040825     c                   eval      eades  = 'Competenze'
103500040825     c                   eval      desim1 = 'Competenze'
103600041011     c                   move      'Diffe.Comp'  desdel
103700040824     c                   end
103800040824      **
103900040824      ** confronto con Ricavi
104000040824     c                   if        volte = 1 and e84ifr = 'X' or
104100040824     c                             volte = 2 and e84iar = 'X' or
104200040824     c                             volte = 3 and e84efr = 'X' or
104300040824     c                             volte = 4 and e84ear = 'X'
104400040824     c                   eval      tipo_confr = 'R'
104500040825     c                   eval      ifdes  = '    Ricavi'
104600040825     c                   eval      iades  = '    Ricavi'
104700040825     c                   eval      efdes  = '    Ricavi'
104800040825     c                   eval      eades  = '    Ricavi'
104900040825     c                   eval      desim1 = '    Ricavi'
105000041011     c                   move      'Diffe.Ric '  desdel
105100040824     c                   end
105200040824      **
105300040826     c     e84ntw        chain     fieu84F
105400040826     c                   if        %found(fieu84F)
105500040826     c                   movel     t84des        desntw
105600040826     c                   end
105700040826      **
105800041007      *                 * ------------------ *
105900041007     c                   exsr      Prt_testa
106000041007      *                 * ------------------ *
106100150203     c  n22              write     pripage
106200041007      *                 * ------------------ *
106300041007     c                   if        knmus = 'EDPAB'
106400150203     c  n22              write     secpage
106500040831     c                   end
106600041007      *                 * ------------------ *
106700040824      **
106800040826      ** pulisce comunque i campi x le rotture di livello
106900040826     c                   clear                   savdfp
107000040826     c                   clear                   savnfp
107100040826     c                   clear                   savrif
107200040826     c                   clear                   savgrp
107300040826     c                   clear                   savaas
107400040826     c                   clear                   savlnp
107500040826     c                   clear                   savnrs
107600040826     c                   clear                   savnsp
107700040826     c                   clear                   savsped
107800040826      **
107900040903     C                   ENDSR
108000040903     C**************************************************************************
108100040903     C* mi compongo l'istruzione sql
108200040903     C**************************************************************************
108300040903     C     istruz        BEGSR                                                  *
108400040903      **
108500040903     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file TNECF
108600160129     c                   if        E84NTW = 'DPD'
108700040903     C                   EVAL      WrkSqlCmd
108800080617     C                             = 'SELECT TNECF00F.*, digits(ecfaas) concat'+
108900080617     C                             ' digits(ecflnp) concat digits(ecfnrs)'     +
109000080617     C                             ' concat digits(ecfnsp),'                   +
109100040903     C                             ' (ecfaas*10000 +ecfmgs), '                 +
109200041012     C                             ' (ecfimr * (-1) - ecfimp), '               +
109300041012     C                             ' (ecfimc - ecfimp), '                      +
109400040903     C                             ' substr(ecftbl, 1, 1) '                    +
109500080710     C                             ' FROM TNECF00F WHERE ECFDTG=0 '
109600160129     c                   else
109700160129     C                   EVAL      WrkSqlCmd
109800160129     C                             = 'SELECT TNECF00F.*, digits(ecfaas) concat'+
109900160129     C                             ' digits(ecflnp) concat digits(ecfnrs)'     +
110000160129     C                             ' concat digits(ecfnsp),'                   +
110100160129     C                             ' (ecfaas*10000 +ecfmgs), '                 +
110200160129     C                             ' (ecfimp - ecfimr), '                      +
110300160129     C                             ' (ecfimp - ecfimc), '                      +
110400160129     C                             ' substr(ecftbl, 1, 1) '                    +
110500160129     C                             ' FROM TNECF00F WHERE ECFDTG=0 '
110600160129     c                   end
110700160128      **
110800040903      **  + Network
110900040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)      +
111000080710     c                             ' and ecfntw =' + '''' + %TRIM(e84ntw) + ''''
111100040903      **
111200040903      **************************************************
111300040903     c* volte 1      Import Franchi         S/N
111400040903     c*  .... 2      Import Assegnati       S/N
111500040903     c*   ... 3      Export Franchi         S/N
111600040903     c*    .. 4      Export Assegnati       S/N
111700040903      **************************************************
111800040903      **  + porto Franco
111900040903     c                   if        volte = 1 or volte = 3
112000040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)
112100040903     c                              + ' and substr(ecftbl, 1, 1) = ''F'' '
112200040903      * se Exp.Franco
112300040903     c                   if        volte = 3
112400040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)
112500040903     c                              + ' and ecftbl <> ''F3'' '
112600040903     c                   end
112700040903     c                   end
112800040903      **  + porto Assegnato
112900040903     c                   if        volte = 2 or volte = 4
113000040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)
113100040903     c                              + ' and substr(ecftbl, 1, 1) = ''A'' '
113200040903      * se Imp.Assegnato
113300040903     c                   if        volte = 2
113400040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)
113500040903     c                              + ' and ecftbl <> ''A3'' '
113600040903     c                   end
113700040903     c                   end
113800040903      **  + import
113900040903     c                   if        volte = 1 or volte = 2
114000040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)
114100130619     c                              + ' and (ecflnp '
114200040903     c                   end
114300040903      **  + export
114400040903     c                   if        volte = 3 or volte = 4
114500040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)
114600130619     c                              + ' and (ecflna '
114700040903     c                   end
114800040903      *
114900040903      **  + Linea Partenza/Arrivo
115000040903     c                   select
115100040903      *
115200040903     c                   when      e84ntw = 'FED'
115300040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)
115400130619     c                              + ' = 400' + ')'
115500040903      *
115600040903     c                   when      e84ntw = 'DPD'
115700130705     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)
115800130705     c                              + ' in (select orgfil from azorg00f where '
115900130705     c                              + ' substr(orgde3, 11, 3)=''DPD'') ) '
116000130705      *------
116100040903     c                   other
116200040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)
116300130619     c                              + ' = ' + %TRIMR(e84NTW) + ')'
116400040903     c                   endsl
116500130619      **
116600040903      **  + data incasellamento
116700040903     c                   if        e84dii > 0
116800040903     C                   EVAL      WrkSqlCmd
116900040903     C                             =
117000040903     C                             %TRIMR(WrkSqlCmd)
117100040903     c                              + ' and  ecfdta between '
117200040903     c                              + %trim(%char(e84dii)) +  ' and '
117300040903     c                              + %trim(%char(e84dfi))
117400040903     c                   end
117500040903      **  + data spedizione
117600040903     c                   if        e84dis > 0
117700040903     C                   EVAL      WrkSqlCmd
117800040903     C                             =
117900040903     C                             %TRIMR(WrkSqlCmd)
118000040903     c                              + ' and (ecfaas*10000 +ecfmgs) between '
118100040903     c                              + %trim(%char(e84dis)) +  ' and '
118200040903     c                              + %trim(%char(e84dfs))
118300040903     c                   end
118400040903      **  + data fattura partner
118500040903     c                   if        e84diF > 0
118600040903     C                   EVAL      WrkSqlCmd
118700040903     C                             =
118800040903     C                             %TRIMR(WrkSqlCmd)
118900040903     c                              + ' and  ecfdfp between '
119000040903     c                              + %trim(%char(e84dif)) +  ' and '
119100040903     c                              + %trim(%char(e84dff))
119200040903     c                   end
119300040903      **
119400040903      ** altre selezioni
119500040903      **
119600040903      **  in questo giro non deve prendere le spedizioni:
119700040903      **        Addebitate dal Partner e non corrispondenti a nostre spedizioni
119800040903      **  e  nostre spedizioni non addebitate dal Partner.
119900040903     C                   EVAL      WrkSqlCmd
120000040903     C                             =
120100040903     C                             %TRIMR(WrkSqlCmd)
120200040903     c                              + ' and ecflnp > 0  '
120300130318     c                              + ' and ecfdfp <> 0 '
120400040903      **
120500040903      **  Esclusioni/Inclusioni di raggruppamenti specifici
120600040903     c                   if        exlgrp <> *blank
120700040903     c                   if        e84gie = 'E'
120800040903     C                   EVAL      WrkSqlCmd
120900040903     C                             = %TRIMR(WrkSqlCmd) + ' and ecfgrp NOT IN ('
121000040903     c                   else
121100040903     C                   EVAL      WrkSqlCmd
121200040903     C                             = %TRIMR(WrkSqlCmd) + ' and ecfgrp IN ('
121300040903     c                   end
121400040903      **
121500040903     c                   if        e84gr1 <> *blank
121600040903     C                   EVAL      WrkSqlCmd
121700040903     C                             = %TRIMR(WrkSqlCmd) + '''' + e84gr1 + ''''
121800040903     c                   end
121900040903     c                   if        e84gr2 <> *blank
122000040903     C                   EVAL      WrkSqlCmd
122100040903     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr2 + ''''
122200040903     c                   end
122300040903     c                   if        e84gr3 <> *blank
122400040903     C                   EVAL      WrkSqlCmd
122500040903     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr3 + ''''
122600040903     c                   end
122700040903     c                   if        e84gr4 <> *blank
122800040903     C                   EVAL      WrkSqlCmd
122900040903     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr4 + ''''
123000040903     c                   end
123100040903     c                   if        e84gr5 <> *blank
123200040903     C                   EVAL      WrkSqlCmd
123300041012     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr5 + ''''
123400040903     c                   end
123500040903     c                   if        e84gr6 <> *blank
123600040903     C                   EVAL      WrkSqlCmd
123700040903     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr6 + ''''
123800040903     c                   end
123900040903     c                   if        e84gr7 <> *blank
124000040903     C                   EVAL      WrkSqlCmd
124100040903     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr7 + ''''
124200040903     c                   end
124300040903     c                   if        e84gr8 <> *blank
124400040903     C                   EVAL      WrkSqlCmd
124500040903     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr8 + ''''
124600040903     c                   end
124700040903     c                   if        e84gr9 <> *blank
124800040903     C                   EVAL      WrkSqlCmd
124900040903     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr9 + ''''
125000040903     c                   end
125100040903     c                   if        e84gr0 <> *blank
125200040903     C                   EVAL      WrkSqlCmd
125300040903     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr0 + ''''
125400040903     c                   end
125500040903      **
125600040903     C                   EVAL      WrkSqlCmd
125700040903     C                             = %TRIMR(WrkSqlCmd) + ' )'
125800040903     c                   end
125900040903      **
126000150429      **   Aggiunta x estrarre i dati DUNFOSS o VTG
126100150429      **
126200150429      **
126300040903      **  Ordinamento di lettura x totalizzazioni
126400040903     C                   EVAL      WrkSqlCmd
126500040903     C                             = %TRIMR(WrkSqlCmd)
126600040903     c                             + ' ORDER BY '
126700041011      **    spedizione
126800040903     c                   if        e84tos = 'S'
126900040903     C                   EVAL      WrkSqlCmd
127000040903     C                             = %TRIMR(WrkSqlCmd)
127100080617     C                             + ' digits(ecfaas) concat digits(ecflnp)'
127200080617     C                             + ' concat digits(ecfnrs) concat'
127300080617     C                             + ' digits(ecfnsp), ecfrif,'
127400041015     C                             + ' ecfgrp, ecfdfp, ecfnfp, ecfmgs '
127500040903     c                   end
127600041011      **    riferimento
127700040903     c                   if        e84top = 'S'
127800040903     C                   EVAL      WrkSqlCmd
127900040903     C                             = %TRIMR(WrkSqlCmd)
128000040903     c                             + ' ecfrif, '
128100080618     C                             + 'digits(ecfaas) concat digits(ecflnp) '
128200080618     c                             + 'concat '
128300080618     C                             + 'digits(ecfnrs) concat digits(ecfnsp),'
128400041015     C                             + ' ecfgrp, ecfdfp, ecfnfp, ecfmgs '
128500040903     c                   end
128600041011     **    raggruppamento
128700040903     c                   if        e84tor = 'S'
128800040903     C                   EVAL      WrkSqlCmd
128900040903     C                             = %TRIMR(WrkSqlCmd)
129000040903     c                             + ' ecfgrp, ecfrif, '
129100080618     C                             + 'digits(ecfaas) concat digits(ecflnp) '
129200080618     c                             + 'concat '
129300080618     C                             + 'digits(ecfnrs) concat digits(ecfnsp),'
129400041015     C                             + ' ecfdfp, ecfnfp, ecfmgs '
129500040903     c                   end
129600041011     **    fattura
129700040903     c                   if        e84tof = 'S'
129800040903     C                   EVAL      WrkSqlCmd
129900040903     C                             = %TRIMR(WrkSqlCmd)
130000040903     c                             + ' ecfnfp, ecfdfp, '
130100080618     C                             + 'digits(ecfaas) concat digits(ecflnp) '
130200080618     c                             + 'concat '
130300080618     C                             + 'digits(ecfnrs) concat digits(ecfnsp),'
130400041015     C                             + ' ecfrif, ecfgrp, ecfmgs '
130500040903     c                   end
130600040903     **
130700040903     c                   if        e84dii > 0
130800040903     C                   EVAL      WrkSqlCmd
130900040903     C                             = %TRIMR(WrkSqlCmd)
131000040903     c                             + ', ecfdta '
131100040903     c                   end
131200040903     c                   if        e84dis > 0
131300040903     C                   EVAL      WrkSqlCmd
131400040903     C                             = %TRIMR(WrkSqlCmd)
131500040903     c                             + ', (ecfaas*10000 +ecfmgs) '
131600040903     c                   end
131700040903      **
131800040903      ** Stampa in prima pagina
131900040903     c                   movea     WrkSqlCmd     Ln_sql
132000040903     c                   eval      lin1 = Ln_sql(1)
132100040903     c                   eval      lin2 = Ln_sql(2)
132200040903     c                   eval      lin3 = Ln_sql(3)
132300040903     c                   eval      lin4 = Ln_sql(4)
132400040903     c                   eval      lin5 = Ln_sql(5)
132500040903     c                   eval      lin6 = Ln_sql(6)
132600040903     c                   eval      lin7 = Ln_sql(7)
132700040903     c                   eval      lin8 = Ln_sql(8)
132800040903     c                   eval      lin9 = Ln_sql(9)
132900040903     c                   eval      lin10 = Ln_sql(10)
133000040903     c                   eval      lin11 = Ln_sql(11)
133100040903     c                   eval      lin12 = Ln_sql(12)
133200040903     c                   eval      lin13 = Ln_sql(13)
133300040903     c                   eval      lin14 = Ln_sql(14)
133400040903     c                   eval      lin15 = Ln_sql(15)
133500040903      **
133600040903     C                   ENDSR
133700040824     C**************************************************************************
133800150325     C* mi compongo l'istruzione sql    NON ORM
133900040824     C**************************************************************************
134000040903     C     istruz2       BEGSR                                                  *
134100040824      **
134200040824     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file TNECF
134300160129     c                   if        E84NTW = 'DPD'
134400040824     C                   EVAL      WrkSqlCmd
134500080618     C                             = 'SELECT TNECF00F.*, digits(ecfaas) '      +
134600080618     C                             ' concat '                                  +
134700080618     C                             'digits(ecflnp) concat digits(ecfnrs) '     +
134800080618     C                             ' concat '                                  +
134900040825     C                             'digits(ecfnsp),'                           +
135000040825     C                             ' (ecfaas*10000 +ecfmgs), '                 +
135100041012     C                             ' (ecfimr * (-1) - ecfimp), '               +
135200041012     C                             ' (ecfimc - ecfimp), '                      +
135300040824     C                             ' substr(ecftbl, 1, 1) '                    +
135400150325     C                             ' FROM TNECF00F WHERE ECFDTG=0 and ecfLNP=0'+
135500150325     C                             ' and ecfTPR <> ''ORM'' '
135600160129     c                   else
135700160129     C                   EVAL      WrkSqlCmd
135800160129     C                             = 'SELECT TNECF00F.*, digits(ecfaas) '      +
135900160129     C                             ' concat '                                  +
136000160129     C                             'digits(ecflnp) concat digits(ecfnrs) '     +
136100160129     C                             ' concat '                                  +
136200160129     C                             'digits(ecfnsp),'                           +
136300160129     C                             ' (ecfaas*10000 +ecfmgs), '                 +
136400160129     C                             ' (ecfimp - ecfimr), '                      +
136500160129     C                             ' (ecfimp - ecfimc), '                      +
136600160129     C                             ' substr(ecftbl, 1, 1) '                    +
136700160129     C                             ' FROM TNECF00F WHERE ECFDTG=0 and ecfLNP=0'+
136800160129     C                             ' and ecfTPR <> ''ORM'' '
136900160129     c                   end
137000040824      **  + Network
137100040824     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)      +
137200040824     c                             ' and ecfntw =' + '''' + %TRIM(e84ntw) + ''''
137300040824      **
137400040903      **  + Non associate ma Addebitate
137500040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)    +
137600041012     c                             ' and ecfdft = 0' +
137700130318     c                             ' and ecfdfp > 0 '
137800041007      **
137900041007      **  se richiesta x totali fattura occorre impostare il limite di data max.
138000150325     c                   if        totf = 'S' and e84dff >0
138100041007     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)    +
138200041007     c                             ' and ecfdfp <= ' + %trim(%char(e84dff))
138300150325     c                   elseif    totf = 'S' and e84dff =0 and e84dfs >0
138400150325     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)    +
138500150325     c                             ' and ecfdfp <= ' + %trim(%char(e84dfs))
138600150325     c                   elseif    totf = 'S' and e84dff =0 and e84dfs =0
138700150325     c                                                      and e84dfi =0
138800150325     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)    +
138900150325     c                             ' and ecfdfp <= ' + %trim(%char(e84dfi))
139000041007     c                   end
139100041007      **
139200040827      **  Esclusioni/Inclusioni di raggruppamenti specifici
139300040827     c                   if        exlgrp <> *blank
139400040827     c                   if        e84gie = 'E'
139500040827     C                   EVAL      WrkSqlCmd
139600040827     C                             = %TRIMR(WrkSqlCmd) + ' and ecfgrp NOT IN ('
139700040827     c                   else
139800040827     C                   EVAL      WrkSqlCmd
139900040827     C                             = %TRIMR(WrkSqlCmd) + ' and ecfgrp IN ('
140000040827     c                   end
140100040827      **
140200040827     c                   if        e84gr1 <> *blank
140300040827     C                   EVAL      WrkSqlCmd
140400040827     C                             = %TRIMR(WrkSqlCmd) + '''' + e84gr1 + ''''
140500040827     c                   end
140600040827     c                   if        e84gr2 <> *blank
140700040827     C                   EVAL      WrkSqlCmd
140800040827     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr2 + ''''
140900040827     c                   end
141000040827     c                   if        e84gr3 <> *blank
141100040827     C                   EVAL      WrkSqlCmd
141200040827     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr3 + ''''
141300040827     c                   end
141400040827     c                   if        e84gr4 <> *blank
141500040827     C                   EVAL      WrkSqlCmd
141600040827     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr4 + ''''
141700040827     c                   end
141800040827     c                   if        e84gr5 <> *blank
141900040827     C                   EVAL      WrkSqlCmd
142000040827     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr5 + ''''
142100040827     c                   end
142200040827     c                   if        e84gr6 <> *blank
142300040827     C                   EVAL      WrkSqlCmd
142400040827     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr6 + ''''
142500040827     c                   end
142600040827     c                   if        e84gr7 <> *blank
142700040827     C                   EVAL      WrkSqlCmd
142800040827     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr7 + ''''
142900040827     c                   end
143000040827     c                   if        e84gr8 <> *blank
143100040827     C                   EVAL      WrkSqlCmd
143200040827     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr8 + ''''
143300040827     c                   end
143400040827     c                   if        e84gr9 <> *blank
143500040827     C                   EVAL      WrkSqlCmd
143600040827     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr9 + ''''
143700040827     c                   end
143800040827     c                   if        e84gr0 <> *blank
143900040827     C                   EVAL      WrkSqlCmd
144000040827     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr0 + ''''
144100040827     c                   end
144200040827      **
144300040827     C                   EVAL      WrkSqlCmd
144400040827     C                             = %TRIMR(WrkSqlCmd) + ' )'
144500040827     c                   end
144600040827      **
144700040825      **  Ordinamento di lettura x totalizzazioni
144800040824     C                   EVAL      WrkSqlCmd
144900040824     C                             = %TRIMR(WrkSqlCmd)
145000040907     c                             + ' ORDER BY ecfnfp, ecfdfp, ecfrif '
145100040824      **
145200040825      ** Stampa in prima pagina
145300040825     c                   movea     WrkSqlCmd     Ln_sql
145400040824     c                   eval      lin1 = Ln_sql(1)
145500040824     c                   eval      lin2 = Ln_sql(2)
145600040824     c                   eval      lin3 = Ln_sql(3)
145700040824     c                   eval      lin4 = Ln_sql(4)
145800040824     c                   eval      lin5 = Ln_sql(5)
145900040824     c                   eval      lin6 = Ln_sql(6)
146000040824     c                   eval      lin7 = Ln_sql(7)
146100040824     c                   eval      lin8 = Ln_sql(8)
146200040824     c                   eval      lin9 = Ln_sql(9)
146300040825     c                   eval      lin10 = Ln_sql(10)
146400040825     c                   eval      lin11 = Ln_sql(11)
146500040825     c                   eval      lin12 = Ln_sql(12)
146600040825     c                   eval      lin13 = Ln_sql(13)
146700040825     c                   eval      lin14 = Ln_sql(14)
146800040825     c                   eval      lin15 = Ln_sql(15)
146900040903      **
147000040903     C                   ENDSR
147100150325     C**************************************************************************
147200151217     C* mi compongo l'istruzione sql  ORM non associati a BOLLE
147300150325     C**************************************************************************
147400151217     C     istruz2_ORM_N BEGSR                                                  *
147500150325      **
147600150325     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file TNECF
147700160129     c                   if        E84NTW = 'DPD'
147800150325     C                   EVAL      WrkSqlCmd
147900150325     C                             = 'SELECT TNECF00F.*, digits(ecfaas) '      +
148000150325     C                             ' concat '                                  +
148100150325     C                             'digits(ecflnp) concat digits(ecfnrs) '     +
148200150325     C                             ' concat '                                  +
148300150325     C                             'digits(ecfnsp),'                           +
148400150325     C                             ' (ecfaas*10000 +ecfmgs), '                 +
148500150325     C                             ' (ecfimr * (-1) - ecfimp), '               +
148600150325     C                             ' (ecfimc - ecfimp), '                      +
148700150325     C                             ' substr(ecftbl, 1, 1) '                    +
148800151216     C                             ' FROM TNECF00F WHERE ECFDTG=0 and ecfLNP=0'+
148900150325     C                             ' and ecfTPR = ''ORM'' '
149000160129     c                   else
149100160129     C                   EVAL      WrkSqlCmd
149200160129     C                             = 'SELECT TNECF00F.*, digits(ecfaas) '      +
149300160129     C                             ' concat '                                  +
149400160129     C                             'digits(ecflnp) concat digits(ecfnrs) '     +
149500160129     C                             ' concat '                                  +
149600160129     C                             'digits(ecfnsp),'                           +
149700160129     C                             ' (ecfaas*10000 +ecfmgs), '                 +
149800160129     C                             ' (ecfimp - ecfimr), '                      +
149900160129     C                             ' (ecfimp - ecfimc), '                      +
150000160129     C                             ' substr(ecftbl, 1, 1) '                    +
150100160129     C                             ' FROM TNECF00F WHERE ECFDTG=0 and ecfLNP=0'+
150200160129     C                             ' and ecfTPR = ''ORM'' '
150300160129     c                   end
150400150325      **  + Network
150500150325     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)      +
150600150325     c                             ' and ecfntw =' + '''' + %TRIM(e84ntw) + ''''
150700150325      **
150800150325      **  + Non associate ma Addebitate
150900150325     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)    +
151000150325     c                             ' and ecfdft = 0' +
151100150325     c                             ' and ecfdfp > 0 '
151200150325      **
151300150325      **  se richiesta x totali fattura occorre impostare il limite di data max.
151400150325     c                   if        totf = 'S' and e84dff >0
151500150325     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)    +
151600150325     c                             ' and ecfdfp <= ' + %trim(%char(e84dff))
151700150325     c                   elseif    totf = 'S' and e84dff =0 and e84dfs >0
151800150325     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)    +
151900150325     c                             ' and ecfdfp <= ' + %trim(%char(e84dfs))
152000150325     c                   elseif    totf = 'S' and e84dff =0 and e84dfs =0
152100150325     c                                                      and e84dfi =0
152200150325     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)    +
152300150325     c                             ' and ecfdfp <= ' + %trim(%char(e84dfi))
152400150325     c                   end
152500150325      **
152600150325      **  Esclusioni/Inclusioni di raggruppamenti specifici
152700150325     c                   if        exlgrp <> *blank
152800150325     c                   if        e84gie = 'E'
152900150325     C                   EVAL      WrkSqlCmd
153000150325     C                             = %TRIMR(WrkSqlCmd) + ' and ecfgrp NOT IN ('
153100150325     c                   else
153200150325     C                   EVAL      WrkSqlCmd
153300150325     C                             = %TRIMR(WrkSqlCmd) + ' and ecfgrp IN ('
153400150325     c                   end
153500150325      **
153600150325     c                   if        e84gr1 <> *blank
153700150325     C                   EVAL      WrkSqlCmd
153800150325     C                             = %TRIMR(WrkSqlCmd) + '''' + e84gr1 + ''''
153900150325     c                   end
154000150325     c                   if        e84gr2 <> *blank
154100150325     C                   EVAL      WrkSqlCmd
154200150325     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr2 + ''''
154300150325     c                   end
154400150325     c                   if        e84gr3 <> *blank
154500150325     C                   EVAL      WrkSqlCmd
154600150325     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr3 + ''''
154700150325     c                   end
154800150325     c                   if        e84gr4 <> *blank
154900150325     C                   EVAL      WrkSqlCmd
155000150325     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr4 + ''''
155100150325     c                   end
155200150325     c                   if        e84gr5 <> *blank
155300150325     C                   EVAL      WrkSqlCmd
155400150325     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr5 + ''''
155500150325     c                   end
155600150325     c                   if        e84gr6 <> *blank
155700150325     C                   EVAL      WrkSqlCmd
155800150325     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr6 + ''''
155900150325     c                   end
156000150325     c                   if        e84gr7 <> *blank
156100150325     C                   EVAL      WrkSqlCmd
156200150325     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr7 + ''''
156300150325     c                   end
156400150325     c                   if        e84gr8 <> *blank
156500150325     C                   EVAL      WrkSqlCmd
156600150325     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr8 + ''''
156700150325     c                   end
156800150325     c                   if        e84gr9 <> *blank
156900150325     C                   EVAL      WrkSqlCmd
157000150325     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr9 + ''''
157100150325     c                   end
157200150325     c                   if        e84gr0 <> *blank
157300150325     C                   EVAL      WrkSqlCmd
157400150325     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr0 + ''''
157500150325     c                   end
157600150325      **
157700150325     C                   EVAL      WrkSqlCmd
157800150325     C                             = %TRIMR(WrkSqlCmd) + ' )'
157900150325     c                   end
158000150325      **
158100150325      **  Ordinamento di lettura x totalizzazioni
158200150325     C                   EVAL      WrkSqlCmd
158300150325     C                             = %TRIMR(WrkSqlCmd)
158400150325     c                             + ' ORDER BY ecfnfp, ecfdfp, ecfrif '
158500150325      **
158600150325      ** Stampa in prima pagina
158700150325     c                   movea     WrkSqlCmd     Ln_sql
158800150325     c                   eval      lin1 = Ln_sql(1)
158900150325     c                   eval      lin2 = Ln_sql(2)
159000150325     c                   eval      lin3 = Ln_sql(3)
159100150325     c                   eval      lin4 = Ln_sql(4)
159200150325     c                   eval      lin5 = Ln_sql(5)
159300150325     c                   eval      lin6 = Ln_sql(6)
159400150325     c                   eval      lin7 = Ln_sql(7)
159500150325     c                   eval      lin8 = Ln_sql(8)
159600150325     c                   eval      lin9 = Ln_sql(9)
159700150325     c                   eval      lin10 = Ln_sql(10)
159800150325     c                   eval      lin11 = Ln_sql(11)
159900150325     c                   eval      lin12 = Ln_sql(12)
160000150325     c                   eval      lin13 = Ln_sql(13)
160100150325     c                   eval      lin14 = Ln_sql(14)
160200150325     c                   eval      lin15 = Ln_sql(15)
160300150325      **
160400150325     C                   ENDSR
160500151217     C**************************************************************************
160600151217     C* mi compongo l'istruzione sql  ORM Italia-Italia ASSOCIATE a BOLLE
160700151217     C**************************************************************************
160800151217     C     istruz2_ORM   BEGSR                                                  *
160900151217      **
161000151217     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file TNECF
161100160129     c                   if        E84NTW = 'DPD'
161200151217     C                   EVAL      WrkSqlCmd
161300151217     C                             = 'SELECT TNECF00F.*, digits(ecfaas) '      +
161400151217     C                             ' concat '                                  +
161500151217     C                             'digits(ecflnp) concat digits(ecfnrs) '     +
161600151217     C                             ' concat '                                  +
161700151217     C                             'digits(ecfnsp),'                           +
161800151217     C                             ' (ecfaas*10000 +ecfmgs), '                 +
161900151217     C                             ' (ecfimr * (-1) - ecfimp), '               +
162000151217     C                             ' (ecfimc - ecfimp), '                      +
162100151217     C                             ' substr(ecftbl, 1, 1) '                    +
162200151217     C                             ' FROM TNECF00F WHERE ECFDTG=0 and ecfLNP>0'+
162300151217     C                             ' and ecfTPR = ''ORM'' '
162400160129     c                   else
162500160129     C                   EVAL      WrkSqlCmd
162600160129     C                             = 'SELECT TNECF00F.*, digits(ecfaas) '      +
162700160129     C                             ' concat '                                  +
162800160129     C                             'digits(ecflnp) concat digits(ecfnrs) '     +
162900160129     C                             ' concat '                                  +
163000160129     C                             'digits(ecfnsp),'                           +
163100160129     C                             ' (ecfaas*10000 +ecfmgs), '                 +
163200160129     C                             ' (ecfimp - ecfimr), '                      +
163300160129     C                             ' (ecfimp - ecfimc), '                      +
163400160129     C                             ' substr(ecftbl, 1, 1) '                    +
163500160129     C                             ' FROM TNECF00F WHERE ECFDTG=0 and ecfLNP>0'+
163600160129     C                             ' and ecfTPR = ''ORM'' '
163700160129     c                   end
163800151217      **  + Network
163900151217     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)      +
164000151217     c                             ' and ecfntw =' + '''' + %TRIM(e84ntw) + ''''
164100151217      **
164200151217      **  + Associate a DPD
164300151217     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)    +
164400151217     c                             ' and ecfdfp > 0 '
164500151217      **
164600151217      **  se richiesta x totali fattura occorre impostare il limite di data max.
164700151217     c                   if        totf = 'S' and e84dff >0
164800151217     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)    +
164900151217     c                             ' and ecfdfp <= ' + %trim(%char(e84dff))
165000151217     c                   elseif    totf = 'S' and e84dff =0 and e84dfs >0
165100151217     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)    +
165200151217     c                             ' and ecfdfp <= ' + %trim(%char(e84dfs))
165300151217     c                   elseif    totf = 'S' and e84dff =0 and e84dfs =0
165400151217     c                                                      and e84dfi =0
165500151217     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)    +
165600151217     c                             ' and ecfdfp <= ' + %trim(%char(e84dfi))
165700151217     c                   end
165800151217      **
165900151217      **  Esclusioni/Inclusioni di raggruppamenti specifici
166000151217     c                   if        exlgrp <> *blank
166100151217     c                   if        e84gie = 'E'
166200151217     C                   EVAL      WrkSqlCmd
166300151217     C                             = %TRIMR(WrkSqlCmd) + ' and ecfgrp NOT IN ('
166400151217     c                   else
166500151217     C                   EVAL      WrkSqlCmd
166600151217     C                             = %TRIMR(WrkSqlCmd) + ' and ecfgrp IN ('
166700151217     c                   end
166800151217      **
166900151217     c                   if        e84gr1 <> *blank
167000151217     C                   EVAL      WrkSqlCmd
167100151217     C                             = %TRIMR(WrkSqlCmd) + '''' + e84gr1 + ''''
167200151217     c                   end
167300151217     c                   if        e84gr2 <> *blank
167400151217     C                   EVAL      WrkSqlCmd
167500151217     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr2 + ''''
167600151217     c                   end
167700151217     c                   if        e84gr3 <> *blank
167800151217     C                   EVAL      WrkSqlCmd
167900151217     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr3 + ''''
168000151217     c                   end
168100151217     c                   if        e84gr4 <> *blank
168200151217     C                   EVAL      WrkSqlCmd
168300151217     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr4 + ''''
168400151217     c                   end
168500151217     c                   if        e84gr5 <> *blank
168600151217     C                   EVAL      WrkSqlCmd
168700151217     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr5 + ''''
168800151217     c                   end
168900151217     c                   if        e84gr6 <> *blank
169000151217     C                   EVAL      WrkSqlCmd
169100151217     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr6 + ''''
169200151217     c                   end
169300151217     c                   if        e84gr7 <> *blank
169400151217     C                   EVAL      WrkSqlCmd
169500151217     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr7 + ''''
169600151217     c                   end
169700151217     c                   if        e84gr8 <> *blank
169800151217     C                   EVAL      WrkSqlCmd
169900151217     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr8 + ''''
170000151217     c                   end
170100151217     c                   if        e84gr9 <> *blank
170200151217     C                   EVAL      WrkSqlCmd
170300151217     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr9 + ''''
170400151217     c                   end
170500151217     c                   if        e84gr0 <> *blank
170600151217     C                   EVAL      WrkSqlCmd
170700151217     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr0 + ''''
170800151217     c                   end
170900151217      **
171000151217     C                   EVAL      WrkSqlCmd
171100151217     C                             = %TRIMR(WrkSqlCmd) + ' )'
171200151217     c                   end
171300151217      **
171400151217      **  Ordinamento di lettura x totalizzazioni
171500151217     C                   EVAL      WrkSqlCmd
171600151217     C                             = %TRIMR(WrkSqlCmd)
171700151217     c                             + ' ORDER BY ecfnfp, ecfdfp, ecfrif '
171800151217      **
171900151217      ** Stampa in prima pagina
172000151217     c                   movea     WrkSqlCmd     Ln_sql
172100151217     c                   eval      lin1 = Ln_sql(1)
172200151217     c                   eval      lin2 = Ln_sql(2)
172300151217     c                   eval      lin3 = Ln_sql(3)
172400151217     c                   eval      lin4 = Ln_sql(4)
172500151217     c                   eval      lin5 = Ln_sql(5)
172600151217     c                   eval      lin6 = Ln_sql(6)
172700151217     c                   eval      lin7 = Ln_sql(7)
172800151217     c                   eval      lin8 = Ln_sql(8)
172900151217     c                   eval      lin9 = Ln_sql(9)
173000151217     c                   eval      lin10 = Ln_sql(10)
173100151217     c                   eval      lin11 = Ln_sql(11)
173200151217     c                   eval      lin12 = Ln_sql(12)
173300151217     c                   eval      lin13 = Ln_sql(13)
173400151217     c                   eval      lin14 = Ln_sql(14)
173500151217     c                   eval      lin15 = Ln_sql(15)
173600151217      **
173700151217     C                   ENDSR
173800040903     C**************************************************************************
173900040903     C* mi compongo l'istruzione sql
174000040903     C**************************************************************************
174100040903     C     istruz3       BEGSR                                                  *
174200040903      **
174300040903     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file TNECF
174400160129     c                   if        E84NTW = 'DPD'
174500040903     C                   EVAL      WrkSqlCmd
174600080618     C                             = 'SELECT TNECF00F.*, digits(ecfaas) '      +
174700080618     C                             'concat '                                   +
174800080618     C                             'digits(ecflnp) concat digits(ecfnrs) '     +
174900080618     C                             'concat '                                   +
175000040903     C                             'digits(ecfnsp),'                           +
175100040903     C                             ' (ecfaas*10000 +ecfmgs), '                 +
175200041012     C                             ' (ecfimr * (-1) - ecfimp), '               +
175300041012     C                             ' (ecfimc - ecfimp), '                      +
175400040903     C                             ' substr(ecftbl, 1, 1) '                    +
175500040907     C                             ' FROM TNECF00F WHERE ECFDTG = 0 '
175600160129     c                   else
175700160129     C                   EVAL      WrkSqlCmd
175800160129     C                             = 'SELECT TNECF00F.*, digits(ecfaas) '      +
175900160129     C                             'concat '                                   +
176000160129     C                             'digits(ecflnp) concat digits(ecfnrs) '     +
176100160129     C                             'concat '                                   +
176200160129     C                             'digits(ecfnsp),'                           +
176300160129     C                             ' (ecfaas*10000 +ecfmgs), '                 +
176400160129     C                             ' (ecfimp - ecfimr), '                      +
176500160129     C                             ' (ecfimp - ecfimc), '                      +
176600160129     C                             ' substr(ecftbl, 1, 1) '                    +
176700160129     C                             ' FROM TNECF00F WHERE ECFDTG = 0 '
176800160129     c                   end
176900040903      **  + Network
177000040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)      +
177100040903     c                             ' and ecfntw =' + '''' + %TRIM(e84ntw) + ''''
177200040903      **
177300040903      **************************************************
177400040903     c* volte 1      Import Franchi         S/N
177500040903     c*  .... 2      Import Assegnati       S/N
177600040903     c*   ... 3      Export Franchi         S/N
177700040903     c*    .. 4      Export Assegnati       S/N
177800040903      **************************************************
177900040903      **  + porto Franco
178000040903     c                   if        volte = 1 or volte = 3
178100040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)
178200040903     c                              + ' and substr(ecftbl, 1, 1) = ''F'' '
178300040903      * se Exp.Franco
178400040903     c                   if        volte = 3
178500040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)
178600040903     c                              + ' and ecftbl <> ''F3'' '
178700040903     c                   end
178800040903     c                   end
178900040903      **  + porto Assegnato
179000040903     c                   if        volte = 2 or volte = 4
179100040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)
179200040903     c                              + ' and substr(ecftbl, 1, 1) = ''A'' '
179300040903      * se Imp.Assegnato
179400040903     c                   if        volte = 2
179500040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)
179600040903     c                              + ' and ecftbl <> ''A3'' '
179700040903     c                   end
179800040903     c                   end
179900040903      **  + import
180000040903     c                   if        volte = 1 or volte = 2
180100040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)
180200130619     c                              + ' and (ecflnp '
180300040903     c                   end
180400040903      **  + export
180500040903     c                   if        volte = 3 or volte = 4
180600040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)
180700130619     c                              + ' and (ecflna '
180800040903     c                   end
180900040903      *
181000040903      **  + Linea Partenza/Arrivo
181100040903     c                   select
181200040903      *
181300040903     c                   when      e84ntw = 'FED'
181400040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)
181500130619     c                              + ' = 400' + ')'
181600040903      *
181700040903     c                   when      e84ntw = 'DPD'
181800130705     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)
181900130705     c                              + ' in (select orgfil from azorg00f where '
182000130705     c                              + ' substr(orgde3, 11, 3)=''DPD'') ) '
182100130705      *------
182200040903     c                   other
182300040903     C                   EVAL      WrkSqlCmd = %TRIMR(WrkSqlCmd)
182400130619     c                              + ' = ' + %TRIMR(e84NTW) + ')'
182500040903     c                   endsl
182600040903      **
182700040903      ** altre selezioni
182800040903      **
182900040903      **  in questo giro  deve prendere solo le nostre spedizioni
183000040903      **    non addebitate dal Partner.
183100040903     C                   EVAL      WrkSqlCmd
183200040903     C                             =
183300040903     C                             %TRIMR(WrkSqlCmd)
183400130318     c                              + ' and ecfdfp = 0 '
183500040903      **
183600040903      **  Esclusioni/Inclusioni di raggruppamenti specifici
183700040903     c                   if        exlgrp <> *blank
183800040903     c                   if        e84gie = 'E'
183900040903     C                   EVAL      WrkSqlCmd
184000040903     C                             = %TRIMR(WrkSqlCmd) + ' and ecfgrp NOT IN ('
184100040903     c                   else
184200040903     C                   EVAL      WrkSqlCmd
184300040903     C                             = %TRIMR(WrkSqlCmd) + ' and ecfgrp IN ('
184400040903     c                   end
184500040903      **
184600040903     c                   if        e84gr1 <> *blank
184700040903     C                   EVAL      WrkSqlCmd
184800040903     C                             = %TRIMR(WrkSqlCmd) + '''' + e84gr1 + ''''
184900040903     c                   end
185000040903     c                   if        e84gr2 <> *blank
185100040903     C                   EVAL      WrkSqlCmd
185200040903     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr2 + ''''
185300040903     c                   end
185400040903     c                   if        e84gr3 <> *blank
185500040903     C                   EVAL      WrkSqlCmd
185600040903     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr3 + ''''
185700040903     c                   end
185800040903     c                   if        e84gr4 <> *blank
185900040903     C                   EVAL      WrkSqlCmd
186000040903     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr4 + ''''
186100040903     c                   end
186200040903     c                   if        e84gr5 <> *blank
186300040903     C                   EVAL      WrkSqlCmd
186400040903     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr5 + ''''
186500040903     c                   end
186600040903     c                   if        e84gr6 <> *blank
186700040903     C                   EVAL      WrkSqlCmd
186800040903     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr6 + ''''
186900040903     c                   end
187000040903     c                   if        e84gr7 <> *blank
187100040903     C                   EVAL      WrkSqlCmd
187200040903     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr7 + ''''
187300040903     c                   end
187400040903     c                   if        e84gr8 <> *blank
187500040903     C                   EVAL      WrkSqlCmd
187600040903     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr8 + ''''
187700040903     c                   end
187800040903     c                   if        e84gr9 <> *blank
187900040903     C                   EVAL      WrkSqlCmd
188000040903     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr9 + ''''
188100040903     c                   end
188200040903     c                   if        e84gr0 <> *blank
188300040903     C                   EVAL      WrkSqlCmd
188400040903     C                             = %TRIMR(WrkSqlCmd) + ', ''' + e84gr0 + ''''
188500040903     c                   end
188600040903      **
188700040903     C                   EVAL      WrkSqlCmd
188800040903     C                             = %TRIMR(WrkSqlCmd) + ' )'
188900040903     c                   end
189000040903      **
189100040903      **  Ordinamento di lettura x totalizzazioni
189200040903     C                   EVAL      WrkSqlCmd
189300040903     C                             = %TRIMR(WrkSqlCmd)
189400040903     c                             + ' ORDER BY '
189500041026     c                             + ' ecfnfp, ecfdfp, ecfaas, ecfmgs, '
189600080618     C                             + 'digits(ecfaas) concat digits(ecflnp) '
189700080618     C                             + 'concat '
189800080618     C                             + 'digits(ecfnrs) concat digits(ecfnsp),'
189900040903     C                             + ' ecfrif, ecfgrp '
190000040903      **
190100040903      ** Stampa in prima pagina
190200040903     c                   movea     WrkSqlCmd     Ln_sql
190300040903     c                   eval      lin1 = Ln_sql(1)
190400040903     c                   eval      lin2 = Ln_sql(2)
190500040903     c                   eval      lin3 = Ln_sql(3)
190600040903     c                   eval      lin4 = Ln_sql(4)
190700040903     c                   eval      lin5 = Ln_sql(5)
190800040903     c                   eval      lin6 = Ln_sql(6)
190900040903     c                   eval      lin7 = Ln_sql(7)
191000040903     c                   eval      lin8 = Ln_sql(8)
191100040903     c                   eval      lin9 = Ln_sql(9)
191200040903     c                   eval      lin10 = Ln_sql(10)
191300040903     c                   eval      lin11 = Ln_sql(11)
191400040903     c                   eval      lin12 = Ln_sql(12)
191500040903     c                   eval      lin13 = Ln_sql(13)
191600040903     c                   eval      lin14 = Ln_sql(14)
191700040903     c                   eval      lin15 = Ln_sql(15)
191800040824      **
191900040824     C                   ENDSR
192000020326     C**************************************************************************
192100040820      *
192200040820     C**************************************************************************
192300040831     C     imp_rig_det   begSR
192400040820      **
192500040826     c                   z-add     ecfimp        detimp
192600040826     c                   if        tipo_confr = 'R'
192700040826     c                   z-add     deltar        detdel
192800041012     c                   z-sub     ecfimr        detim1
192900040826     c                   else
193000040826     c                   z-add     deltac        detdel
193100040826     c                   z-add     ecfimc        detim1
193200040826     c                   end
193300041011      *
193400041011      * ricava la % del delta
193500041011     c                   clear                   detprc
193600041011     c                   clear                   simprc
193700041014      *
193800041011      * solo se richiesto dettaglio in totale x Fattura o Raggruppamento
193900041011     c                   if        e84tof ='S' or e84tor ='S'
194000041011     c                   if        detim1 <> 0 and e84sde <> 99
194100041011     c                   eval(h)   totdel = (detdel) * 100 / detim1
194200041011     c                   move      '%'           simprc
194300041011     c                   if        %abs(totdel) < 999,99
194400041011     c                   move      totdel        detprc
194500041011     c                   else
194600041011     c                   move      *all'9'       detprc
194700041011     c                   end
194800041011     c                   end
194900041011     c                   end
195000041011     c                   clear                   totdel
195100040826      **
195200040826      **  se deve creare il file PC
195300040826     c                   if        e84fpc = 'S'
195400040826     c                   exsr      wrt_PCfile
195500040901     c                   end
195600040826      **
195700040826     C                   ENDSR
195800040826     C**************************************************************************
195900040826      *
196000040826     C**************************************************************************
196100040826     C     totalizza     begSR
196200160615      **
196300160615      **--------
196400160615      **  Per DPD creiamo il file di work per monitorare i totali per
196500160615      **   NAZIONE/Cod.TASSAZIONE
196600160615     c                   IF        E84NTW = 'DPD'
196700160616     c                   IF           VLT =2 or VLT =3
196800160616     c                   if           VLT =3
196900160622     c                   if        ecfFBR = *blank
197000160616     c                   eval         n85TIPO= 'FRA.EXPORT'
197100160622     c                   else
197200160622     c                   eval         n85TIPO= 'FRA.  RESI'
197300160622     c                   end
197400160616     c                   elseif       VLT =2
197500160622     c                   eval         n85RESO= ecfFBR
197600160622     c                   if        ecfFBR = *blank
197700160622     c                   eval         n85TIPO= 'ASS.IMPORT'
197800160622     c                   else
197900160622     c                   eval         n85TIPO= 'ASS.  RESO'
198000160622     c                   end
198100160616     c                   endif
198200160628     c                   eval         n85cau = %subst(ECFFLR:1:3)
198300160615     C     K85nt         chain     wfieu85nt
198400160615      **
198500160615     c                   if        not %Found(wfieu85nt)
198600160616     c                   clear                   FIEUn85
198700160627     c                   move      dadata        n85dadta
198800160627     c                   move      adata         n85adta
198900160627     c                   eval        n85ute = knmus
199000160615     c                   eval         n85NAZ = ecfNAZ
199100160615     c                   eval         n85CTS = ecfCTS
199200160622     c                   eval         n85RESO= ecfFBR
199300160628     c                   eval         n85cau = %subst(ECFFLR:1:3)
199400160616     c                   if           VLT =3
199500160622     c                   if        ecfFBR = *blank
199600160616     c                   eval         n85TIPO= 'FRA.EXPORT'
199700160622     c                   else
199800160622     c                   eval         n85TIPO= 'FRA.  RESI'
199900160622     c                   end
200000160616     c                   elseif       VLT =2
200100160622     c                   if        ecfFBR = *blank
200200160622     c                   eval         n85TIPO= 'ASS.IMPORT'
200300160622     c                   else
200400160622     c                   eval         n85TIPO= 'ASS.  RESO'
200500160622     c                   end
200600160622     c                   endif
200700160615     c                   endif
200800160615      **
200900160615     c                   add       1             n85SPD
201000160615     c                   add       ecfimp        n85PTN
201100160615     c                   add       detim1        n85BRT
201200160615      **
201300160615     c                   if        tipo_confr = 'R'
201400160615     c                   add       deltar        n85DIF
201500160615     c                   else
201600160615     c                   add       deltac        n85DIF
201700160615     c                   end
201800160615      **
201900160615      **    uso il campo come flag x capire se updat o write
202000160622     c                   if        not %Found(wfieu85nt)
202100160622     c                   write     FIEUN85
202200160622     c                   else
202300160615     c                   update    FIEUN85
202400160615     c                   endIF
202500160615      **
202600160615     c                   end
202700160615     c                   endIF
202800160615      **--------
202900160615      **
203000150429      **  Solo x "DPD" controlla se il Rif.Parcel proviene dai DEPOT Particolari
203100150429      **   da Monitorare.
203200150429     c                   clear                   CliDPD_Partic     1
203300150429     c                   IF        E84NTW = 'DPD'
203400150429     c                   movel     ecfRIF        Depot4            4
203500150429     c                   z-add     1             dep               3 0
203600150429     c     Depot4        lookup    DPD_DEP(dep)                           21
203700150429     c                   If        %Equal
203800150429     c                   eval       CliDPD_Partic = DPD_CLI(dep)
203900150429     c                   end
204000150429     c                   endIF
204100150429      **
204200150429      **  Se DPD e si tratta di un Cliente DANFOSS o VTG e siamo nel primo ciclo
204300150429      **  ossia fra 1 e 4.
204400150429     c                   If         CliDPD_Partic <> *blank
204500150429     c                              and VLT <=4
204600150429      **
204700150429     c                   If         CliDPD_Partic = 'D'
204800150429     c                   add       1             tSPDD(vlt)
204900150429     c                   add       ecfimp        tPTND(vlt)
205000150429     c                   add       detim1        tBRTD(vlt)
205100150429     c                   if        tipo_confr = 'R'
205200150429     c                   add       deltar        tDIFD(vlt)
205300150429     c                   else
205400150429     c                   add       deltac        tDIFD(vlt)
205500150429     c                   end
205600150429     c                   elseIf     CliDPD_Partic = 'V'
205700150429     c                   add       1             tSPDV(vlt)
205800150429     c                   add       ecfimp        tPTNV(vlt)
205900150429     c                   add       detim1        tBRTV(vlt)
206000150429     c                   if        tipo_confr = 'R'
206100150429     c                   add       deltar        tDIFV(vlt)
206200150429     c                   else
206300150429     c                   add       deltac        tDIFV(vlt)
206400150429     c                   end
206500150429     c                   end
206600150429      **
206700150429     c                   endIF
206800151118      **
206900151118      **  memorizza i RESI
207000151118     c                   IF           ecfFBR = 'S'
207100151118     c                   IF         VLT =2 or VLT =4 or
207200151118     c                              VLT =6 or VLT =8
207300151118     c                   add       1             tSPDR(vlt)
207400151118     c                   add       ecfimp        tPTNR(vlt)
207500151118     c                   add       detim1        tBRTR(vlt)
207600151118     c                   if        tipo_confr = 'R'
207700151118     c                   add       deltar        tDIFR(vlt)
207800151118     c                   else
207900151118     c                   add       deltac        tDIFR(vlt)
208000151118     c                   end
208100151118     c                   endIF
208200151118     c                   endIF
208300040826      **
208400040826     c                   move      'S'           almeno_uno
208500150129     c                   add       1             tSPD(vlt)
208600150129     c                   add       ecfimp        tPTN(vlt)
208700040825     c                   add       ecfimp        totrip
208800040825     c                   add       ecfimp        totfap
208900040825     c                   add       ecfimp        totgep
209000040825     c                   add       ecfimp        totgrp
209100150129     c                   add       detim1        tBRT(vlt)
209200040825     c                   add       detim1        totri1
209300040825     c                   add       detim1        totfa1
209400040825     c                   add       detim1        totge1
209500040825     c                   add       detim1        totgr1
209600040825     c                   if        tipo_confr = 'R'
209700150129     c                   add       deltar        tDIF(vlt)
209800040825     c                   add       deltar        totrid
209900040825     c                   add       deltar        totfad
210000040825     c                   add       deltar        totged
210100040825     c                   add       deltar        totgrd
210200040825     c                   else
210300150129     c                   add       deltac        tDIF(vlt)
210400040825     c                   add       deltac        totrid
210500040825     c                   add       deltac        totfad
210600040825     c                   add       deltac        totged
210700040825     c                   add       deltac        totgrd
210800040825     c                   end
210900040825      *
211000040825      * Incasella l'importo nel gruppo di appartenenza
211100040825     c                   z-add     1             Ip                3 0
211200040831     c                   if        ecfgrp <> *blank
211300040825     c     ecfGRP        lookup    Grp(Ip)                                21
211400040831     c                   else
211500040831      * x gruppo blank
211600040831     c     'blk'         lookup    Grp(Ip)                                21
211700040831     c                   end
211800040825     c  n21              z-add     1             Ip
211900040825     c  n21*blank        lookup    Grp(Ip)                                21
212000040831     c                   if        ecfgrp <> *blank
212100040825     c                   movel     ecfGRP        grP(Ip)
212200040831     c                   else
212300040831     c                   movel     'blk'         grP(Ip)
212400040831     c                   end
212500040825     c                   add       ecfIMP        ImP(Ip)
212600041012     c                   sub       ecfIMR        ImR(Ip)
212700040825     c                   add       ecfIMC        ImC(Ip)
212800040825     c                   if        tipo_confr = 'R'
212900040825     c                   add       deltar        ImD(Ip)
213000040825     c                   else
213100040825     c                   add       deltac        ImD(Ip)
213200040825     c                   end
213300160530      **
213400160530     C                   ENDSR
213500160530     C**************************************************************************
213600160530      *   Totali Mensili x Non Addebitate da DPD  e NON ACCREDITATE
213700160530     C**************************************************************************
213800160530     C     tot_Mensili   begSR
213900160531      **
214000160531     c                   move      ecfdcm        mmgg
214100160601     c                   movel     ecfdcm        annox             4 0
214200160531     c                   movel     mmgg          meseX             2 0
214300160531      **
214400160531      **  memorizza i RESI
214500160531     c                   if        meseX = MES0
214600160531     c                   IF           ecfFBR = 'S'
214700160531     c                   IF         VLT =6 or VLT =8
214800160531     c                   add       1             tSPDR0(vlt)
214900160531     c                   add       detim1        tBRTR0(vlt)
215000160531     c                   endIF
215100160531     c                   endIF
215200160531      **
215300160531     c                   add       1             tSPD0(vlt)
215400160531     c                   add       detim1        tBRT0(vlt)
215500160531      **
215600160601     c                   elseIF    meseX = MES1
215700160531     c                   IF           ecfFBR = 'S'
215800160531     c                   IF         VLT =6 or VLT =8
215900160531     c                   add       1             tSPDR1(vlt)
216000160531     c                   add       detim1        tBRTR1(vlt)
216100160531     c                   endIF
216200160531     c                   endIF
216300160531      **
216400160531     c                   add       1             tSPD1(vlt)
216500160531     c                   add       detim1        tBRT1(vlt)
216600160531      **
216700160601     c                   elseIF    meseX = MES2
216800160531     c                   IF           ecfFBR = 'S'
216900160531     c                   IF         VLT =6 or VLT =8
217000160531     c                   add       1             tSPDR2(vlt)
217100160531     c                   add       detim1        tBRTR2(vlt)
217200160531     c                   endIF
217300160531     c                   endIF
217400160531      **
217500160531     c                   add       1             tSPD2(vlt)
217600160531     c                   add       detim1        tBRT2(vlt)
217700160601      **
217800160601     c                   else
217900040825      **
218000160601      **  Metto tutto ciò che non è inquadrato nelle precedenti e che non STAMPO
218100160601     c*************      if        MES3 > MES0  and
218200160601     c*************                (meseX > MES0 and meseX <= MES3)
218300160601     c*************                   or
218400160601     c*************                MES3 < MES0  and
218500160601     c*************                (meseX > MES0  or meseX <= MES3)
218600160601     c*************
218700160601     c                   IF           ecfFBR = 'S'
218800160601     c                   IF         VLT =6 or VLT =8
218900160601     c                   add       1             tSPDR3(vlt)
219000160601     c                   add       detim1        tBRTR3(vlt)
219100160601     c                   endIF
219200160601     c                   endIF
219300160601      **
219400160601     c                   add       1             tSPD3(vlt)
219500160601     c                   add       detim1        tBRT3(vlt)
219600160601     c*************      endIF
219700160601      **
219800160601     c                   endIF
219900160601      **
220000040825     C                   ENDSR
220100040825     C**************************************************************************
220200040827      *   Totali a parte x Non Addebitate o x Non incasellate
220300040825     C**************************************************************************
220400041014     C     tot_A_Parte   begSR
220500040902      **
220600040902     c                   if        tipo_confr = 'R'
220700041012     c                   z-sub     ecfimr        detim1
220800040902     c                   else
220900040902     c                   z-add     ecfimc        detim1
221000040902     c                   end
221100040902      **
221200130318     c                   if        (ecfdfp = 0)
221300040902     c                   if        tipo_confr = 'R'
221400040902     c                   add       deltar        tnadel
221500040902     c                   else
221600040902     c                   add       deltac        tnadel
221700040902     c                   end
221800040902     c                   add       ecfimp        tnaimp
221900040902     c                   add       detim1        tnaim1
222000040902     c                   end
222100040902      **
222200041014      **
222300040902     c                   if        ecfdfp > 0
222400040902     c                   if        tipo_confr = 'R'
222500040902     c                   add       deltar        tnidel
222600040902     c                   else
222700040902     c                   add       deltac        tnidel
222800040902     c                   end
222900040902     c                   add       ecfimp        tniimp
223000040902     c                   add       detim1        tniim1
223100040902     c                   end
223200040902      **
223300040827     C                   ENDSR
223400040824     C**************************************************************************
223500040824      *
223600040820     C**************************************************************************
223700040825     C     TOTals        begSR
223800040826      *
223900151126      *  solo per i NON incasellati qual'è la data più vecchia
224000151126     c                   if        vlt > 4 and vlt < 9
224100151126     c                   if        data_MINIMA > datspe or data_MINIMA =0
224200151126     c                   eval      data_MINIMA = datspe
224300151126     c                   movel     data_MINIMA   dataiso
224400151126     c                   eval      danoninc    = dataISO
224500151126     c                   end
224600151126     c                   end
224700040823      *
224800040825     c* ROTTURA Spedizione
224900040826     c                   if        e84tos ='S'
225000040825     c                   if        spedbar <> savsped or
225100040825     C                             SqlCod = 100
225200040831     c                   if        almeno_uno ='S'
225300040825     c                   exsr      TOTalSpe
225400040831     c                   end
225500040825     c                   eval      savsped =  spedbar
225600040825     c                   eval      savaas  =  ecfaas
225700040825     c                   eval      savlnp  =  ecflnp
225800040825     c                   eval      savnrs  =  ecfnrs
225900040825     c                   eval      savnsp  =  ecfnsp
226000040825     c                   end
226100040826     c                   end
226200040825      *
226300040825     c* ROTTURA Riferimento
226400040826     c                   if        e84top ='S'
226500040825     c                   if        ecfrif <> savrif or
226600040825     C                             SqlCod = 100
226700040831     c                   if        almeno_uno ='S'
226800040825     c                   exsr      TOTalRif
226900040831     c                   end
227000040825     c                   eval      savrif  =  ecfrif
227100040825     c                   end
227200040826     c                   end
227300040825      *
227400040825     c* ROTTURA Fattura
227500040826     c                   if        e84tof ='S'
227600040825     c                   if        ecfnfp <> savnfp or
227700040825     C                             SqlCod = 100
227800040831     c                   if        almeno_uno ='S'
227900040825     c                   exsr      TOTalfat
228000040831     c                   end
228100040825     c                   eval      savnfp =  ecfnfp
228200040901     c                   eval      savdfp =  ecfdfp
228300040825     c                   end
228400040826     c                   end
228500040825      *
228600040825     c* ROTTURA Gruppo
228700040826     c                   if        e84tor ='S'
228800040825     c                   if        ecfgrp <> savgrp or
228900040825     C                             SqlCod = 100
229000040831     c                   if        almeno_uno ='S'
229100040825     c                   exsr      TOTalgrp
229200040831     c                   end
229300040825     c                   eval      savgrp =  ecfgrp
229400040825     c                   end
229500040826     c                   end
229600040825      *
229700040825     C                   ENDSR
229800040825     C**************************************************************************
229900040825      *
230000040825     C**************************************************************************
230100040825     C     TOTalSpe      begSR
230200040825      *
230300040825     c                   if        prima_volta = 'N'
230400040824     C* totale  Spedizione (Riferimento)
230500040823     c                   if        *in66
230600040823      *                 * ------------------ *
230700041007     c                   exsr      Prt_testa
230800041007      *                 * ------------------ *
230900150203     c  n22              write     tesdetta
231000040823      *                 * ------------------ *
231100040823     c                   setoff                                       66
231200040823     c                   end
231300040826      *
231400040826      * ricava la % del delta
231500040826     c                   if        totri1 <> 0
231600040901     c                   eval(h)   totdel = (totrid) * 100 / totri1
231700040901     c                   if        %abs(totdel) < 999,99
231800040901     c                   move      totdel        prcdel
231900040901     c                   else
232000040901     c                   move      *all'9'       prcdel
232100040901     c                   end
232200040826     c                   end
232300040826      *
232400041011      *** attenzione stampo in base alla soglia richiesta
232500041011      *
232600041011     c                   if        e84sde = 99 or %abs(prcdel) > e84sde
232700150203     c  n22              write     totspedz
232800041011     c                   end
232900040824     c                   end
233000040826      *
233100040824     c                   clear                   totrip
233200040825     c                   clear                   totri1
233300040825     c                   clear                   totri2
233400040824     c                   clear                   totrid
233500040826     c                   clear                   prcdel
233600040901     c                   clear                   totdel           11 2
233700040820      *
233800040820     C                   ENDSR
233900040825     C**************************************************************************
234000040825      *
234100040825     C**************************************************************************
234200040825     C     TOTalRif      begSR
234300040825      *
234400040825     c                   if        prima_volta = 'N'
234500040825     C* totale  Spedizione (Riferimento)
234600040825     c                   if        *in66
234700040825      *                 * ------------------ *
234800041007     c                   exsr      Prt_testa
234900041007      *                 * ------------------ *
235000150203     c  n22              write     tesdetta
235100040825      *                 * ------------------ *
235200040825     c                   setoff                                       66
235300040825     c                   end
235400040826      *
235500040826      * ricava la % del delta
235600040826     c                   if        totri1 <> 0
235700040901     c                   eval(h)   totdel = (totrid) * 100 / totri1
235800040901     c                   if        %abs(totdel) < 999,99
235900040901     c                   move      totdel        prcdel
236000040901     c                   else
236100040901     c                   move      *all'9'       prcdel
236200040901     c                   end
236300040826     c                   end
236400040826      *
236500041011      *** attenzione stampo in base alla soglia richiesta
236600041011      *
236700041011     c                   if        e84sde = 99 or %abs(prcdel) > e84sde
236800150203     c  n22              write     totrifer
236900041011     c                   end
237000040825     c                   end
237100040826      *
237200040825     c                   clear                   totrip
237300040825     c                   clear                   totri1
237400040825     c                   clear                   totri2
237500040825     c                   clear                   totrid
237600040826     c                   clear                   prcdel
237700040901     c                   clear                   totdel           11 2
237800040825      *
237900040825     C                   ENDSR
238000040824     C**************************************************************************
238100040824      *
238200040820     C**************************************************************************
238300040825     C     TOTalfat      begSR
238400040824      *
238500040825     c                   if        prima_volta = 'N'
238600040831      *
238700040901      *    tot   schiere x gruppo
238800040831     c                   exsr      riep_grp
238900040831      *
239000040824     C* totale
239100040824     c                   if        *in66
239200040824      *                 * ------------------ *
239300041007     c                   exsr      Prt_testa
239400041007      *                 * ------------------ *
239500150203     c  n22              write     tesdetta
239600040824      *                 * ------------------ *
239700040824     c                   setoff                                       66
239800040824     c                   end
239900150203     c  n22              write     totfattu
240000040824     c                   end
240100040826      *
240200040824     c                   clear                   totfap
240300040825     c                   clear                   totfa1
240400040825     c                   clear                   totfa2
240500040824     c                   clear                   totfad
240600040824      *
240700040824     C                   ENDSR
240800040824     C**************************************************************************
240900040825      *
241000040825     C**************************************************************************
241100040825     C     TOTalgrp      begSR
241200040825      *
241300040825     c                   if        prima_volta = 'N'
241400040825     C* totale
241500040825     c                   if        *in66
241600040825      *                 * ------------------ *
241700041007     c                   exsr      Prt_testa
241800041007      *                 * ------------------ *
241900150203     c  n22              write     tesdetta
242000040825      *                 * ------------------ *
242100040825     c                   setoff                                       66
242200040825     c                   end
242300150203     c  n22              write     totgrup
242400040825     c                   end
242500040826      *
242600040825     c                   clear                   totgrp
242700040825     c                   clear                   totgr1
242800040825     c                   clear                   totgr2
242900040825     c                   clear                   totgrd
243000040825      *
243100040825     C                   ENDSR
243200040825     C**************************************************************************
243300040827      *  Preciclo se attivato x totale a Spedizione
243400040825     C**************************************************************************
243500040826     C     pre_ciclo     begsr
243600040826      *
243700040826      * prova a chiuderlo sempre prima di pulirlo
243800040826      *  Pulisce il workfile  su cui impostare le spedizioni da NON stampare
243900041014     c                   close     wfieu85w                             99
244000040826      *
244100040826     C                   z-add     62            lung             15 5
244200040826     C                   clear                   comman
244300040826     C                   movea     cmd(1)        comman           80
244400040826     C                   call      'QCMDEXC'
244500040826     C                   parm                    comman
244600040826     C                   parm                    lung
244700041014     c                   open      wfieu85w
244800040826      *
244900040826     C/EXEC SQL
245000040903     C+ PREPARE P1 FROM :WrkSqlCmd
245100040826     C/END-EXEC
245200040826
245300040826     C/EXEC SQL
245400040903     C+ DECLARE A2 CURSOR FOR P1
245500040826     C/END-EXEC
245600040826
245700040826     C/EXEC SQL
245800040826     C+ OPEN A2
245900040826     C/END-EXEC
246000041011     c                   clear                   almeno_uno
246100040826     C                   DOU       SqlCod <> 0
246200040826     C/EXEC SQL
246300040826     C+ FETCH NEXT FROM A2 INTO :tnecf00f, :spedbar, :datspe, :deltar,
246400040826     C+ :deltac, :porto
246500040826     C/END-EXEC
246600040826     C                   SELECT
246700041008     c* a fine file
246800041008     C                   WHEN      SqlCod = 100
246900041008     c* ROTTURA Spedizione
247000041008     c                   if        almeno_uno = 'S'
247100041008     c                   if        spedbar <> savsped or
247200041008     C                             SqlCod = 100
247300041008     c                   exsr      WRTxSpe
247400041008     c                   eval      savsped =  spedbar
247500041008     c                   end
247600041008     c                   else
247700041008     c                   end
247800041008      * poi esce
247900041008     c                   leave
248000041008     **
248100041008     C                   WHEN      SqlCod < 0
248200041008     c*
248300041008     C                   seton                                        H4
248400041008     c                   goto      fine_preciclo
248500041008     c*
248600041008     C                   OTHER
248700041008     **
248800040826     c* ROTTURA Spedizione
248900040826     c                   if        spedbar <> savsped or
249000040826     C                             SqlCod = 100
249100040826     c                   exsr      WRTxSpe
249200040826     c                   eval      savsped =  spedbar
249300040826     c                   end
249400040826      *
249500041011     c                   move      'S'           almeno_uno
249600041011      **
249700041011     c                   if        tipo_confr = 'R'
249800041012     c                   sub       ecfimr        calrim
249900041011     c                   add       deltar        calrid
250000041011     c                   else
250100041011     c                   add       ecfimc        calrim
250200041011     c                   add       deltac        calrid
250300041011     c                   end
250400041011      **
250500040826     C                   ENDSL
250600040826
250700040826     C                   ENDDO
250800040826     C/EXEC SQL
250900040826     C+ CLOSE A2
251000040826     C/END-EXEC
251100041008     c     fine_preciclo tag
251200040826     C                   ENDSR
251300040826     C**************************************************************************
251400040826      *   scrive le Spedizioni da scartare in seguito
251500040826     C**************************************************************************
251600040826     C     WRTxSpe       begSR
251700040826      *
251800040826      * ricava la % del delta
251900040826     c                   clear                   fieuw85
252000041011     c                   if        calrim <> 0
252100070427     c                   clear                   totale_prc        9 2
252200070427     c                   eval(h)   totale_prc = (calrid) * 100 / calrim
252300070427     c                   if        totale_prc > 999,99 or totale_prc < -999,99
252400070427     c                   eval(h)   w85prc = 999,99
252500070427     c                   else
252600070427     c                   eval(h)   w85prc = (calrid) * 100 / calrim
252700070427     c                   end
252800040826     c                   end
252900040826      *  confronta un valore assoluto di scostamento
253000040826     c                   if        savsped <> *blank
253100041011     c                   if        e84sde < %abs(w85prc) or calrim = 0
253200040826     c                   eval      w85spb = savsped
253300040826     c                   write     fieuw85
253400040826     c                   end
253500040826     c                   end
253600040826      *
253700041011     c                   clear                   calrim
253800041011     c                   clear                   calrid
253900040826      *
254000040826     C                   ENDSR
254100040826     C**************************************************************************
254200040827      *  Totali Riepilocativi prima del Totale generale
254300040827     C**************************************************************************
254400040827     C     tot_riepilogo begsr
254500040827      *
254600040827     C* totale non addebitate
254700040903     c                   if        e84sna <>'E' and ciclo_exec = 3
254800040827     c                   if        *in66
254900040827      *                 * ------------------ *
255000041007     c                   exsr      Prt_testa
255100041007      *                 * ------------------ *
255200150203     c  n22              write     tesdetta
255300040827      *                 * ------------------ *
255400040827     c                   setoff                                       66
255500040827     c                   end
255600150203     c  n22              write     totTNA
255700040903     c                   end
255800040901      ********
255900040827     C* totale non incasellate
256000040903     c                   if        e84sni <>'E' and ciclo_exec = 2
256100040827     c                   if        *in66
256200040827      *                 * ------------------ *
256300041007     c                   exsr      Prt_testa
256400041007      *                 * ------------------ *
256500150203     c  n22              write     tesdetta
256600040827      *                 * ------------------ *
256700040827     c                   setoff                                       66
256800040827     c                   end
256900150203     c  n22              write     totTNI
257000040903     c                   end
257100040827      *
257200040827     c                   clear                   tnadel
257300040827     c                   clear                   tnaimp
257400040827     c                   clear                   tnaim1
257500040827     c                   clear                   tnaim2
257600040827     c                   clear                   tnidel
257700040827     c                   clear                   tniimp
257800040827     c                   clear                   tniim1
257900040827     c                   clear                   tniim2
258000040827      *
258100040827     C                   ENDSR
258200040827     C**************************************************************************
258300040901      *  prima di fattura riepiloga x gruppo
258400040827     C**************************************************************************
258500040827     C     riep_grp      begsr
258600040827      **
258700040827     c                   do        50            cnt               3 0
258800040827     c                   if        grp(cnt) = *blank
258900040827     c                             and cnt > 1
259000040827     c                   leave
259100040827     c                   end
259200040827     c                   movel     grp(cnt)      savgrp
259300040827     c                   z-add     ImP(cnt)      totGrp
259400040827     c                   if        tipo_confr = 'R'
259500041012     c                   z-add     ImR(cnt)      totGr1
259600040827     c                   else
259700040827     c                   z-add     ImC(cnt)      totGr1
259800040827     c                   end
259900040827     c                   z-add     ImD(cnt)      totGrd
260000040831     c                   if        *in66
260100040831      *                 * ------------------ *
260200041007     c                   exsr      Prt_testa
260300041007      *                 * ------------------ *
260400150203     c  n22              write     tesdetta
260500040831      *                 * ------------------ *
260600040831     c                   setoff                                       66
260700040831     c                   end
260800150203     c  n22              write     Totgrup
260900040901      **
261000040827     c                   EndDO
261100040827      *
261200040827     c                   clear                   grP
261300040827     c                   clear                   imP
261400040827     c                   clear                   imR
261500040827     c                   clear                   imC
261600040827     c                   clear                   imD
261700040827      *
261800040827     C                   ENDSR
261900040827     C**************************************************************************
262000160530      * Esegue STAMPA riepilogativa
262100160530     C**************************************************************************
262200160530     C     STAMPA_Riep   BEGSR
262300150203      *
262400150505     c                   eval      ptn1    = e84ntw
262500150505     c                   eval      ptn2    = e84ntw
262600150505     c                   eval      ptn4    = e84ntw
262700160530      *                *------------------*
262800160530     c                   write     tesPAG
262900160530      *                *------------------*
263000160530      *
263100160531     c                   if        e84ntw <>'DPD'
263200160531     c                   exsr      NON_incas
263300160531      *
263400160531     c                   else
263500160531      *
263600160531      * DPD splittiamo in 3 mesi
263700160531     c                   move      mes0          mese02
263800160531     c                   exsr      NON_incas0
263900160531     c                   move      mes1          mese02
264000160531     c                   exsr      NON_incas1
264100160531     c                   move      mes2          mese02
264200160531     c                   exsr      NON_incas2
264300160601     c                   move      'XX'          mese02
264400160601     c                   exsr      NON_incas3
264500160531     c                   end
264600150203      *
264700160530     c                   exsr      NON_incasDPD
264800160530      *
264900160530     c                   exsr      IncaseLLATI
265000160530      *
265100160530     c                   if        E84NTW = 'DPD'
265200160623      *                *------------------*
265300160623     c                   write     tesPAG
265400160623      *                *------------------*
265500160623      *
265600150429      *      Calcola i dati solo per il Depot 0124
265700150429     c                   exsr      DUNFOSS
265800150429      *      Calcola i dati solo per i Depot 094*
265900150429     c                   exsr      VTG
266000160530     c                   write     pagCLIDPD
266100160530     c                   end
266200150129      *
266300150129     C                   ENDSR
266400150129     C**************************************************************************
266500160530      *  NON incasellati BRT
266600040901     C**************************************************************************
266700160530     C     NON_incas     begSR
266800041102      *
266900160530      * non incasellato
267000160530     c                   z-add     TSPD(5)       SFITSP
267100160530     c                   z-add     TBRT(5)       SFIIM1
267200160530      *
267300160530     c                   z-add     TSPDR(6)      SAITSP
267400160530     c     TSPD(6)       sub       TSPDR(6)      SAITSP2
267500160530     c                   z-add     TBRTR(6)      SAIIM1
267600160530     c     TBRT(6)       sub       TBRTR(6)      SAIIM12
267700160530      *
267800160530     c                   z-add     TSPD(7)       SFETSP
267900160530     c                   z-add     TBRT(7)       SFEIM1
268000160530      *
268100160530     c                   z-add     TSPDR(8)      SAETSP
268200160530     c     TSPD(8)       sub       TSPDR(8)      SAETSP2
268300160530     c                   z-add     TBRTR(8)      SAEIM1
268400160530     c     TBRT(8)       sub       TBRTR(8)      SAEIM12
268500160530      *
268600160530      *  in un unico totale
268700160530     c                   z-add     TSPD(5)       TNBTSP0
268800160530     c                   add       TSPD(8)       TNBTSP0
268900160530     c                   z-add     TSPD(7)       TNBTSP
269000160530     c                   add       TSPD(6)       TNBTSP
269100160530      *
269200160530     c                   z-add     TBRT(5)       TNBIM10
269300160530     c                   add       TBRT(8)       TNBIM10
269400160530     c                   z-add     TBRT(7)       TNBIM1
269500160530     c                   add       TBRT(6)       TNBIM1
269600160530      *   tot
269700160530      * x DPD deve sommare
269800160530     c                   if        E84NTW = 'DPD'
269900160530     c                   z-add     TNBIM1        TNBDEL
270000160530     c                   else
270100160530      * x gli altri sottrae perchè il valore è positivo
270200160530     c                   z-sub     TNBIM1        TNBDEL
270300160530     c                   end
270400160530      *
270500160530     c                   z-add     TNBIM10       TNBDEL0
270600160530      *
270700160530      *                *------------------*
270800160530     c                   write     RnonINC
270900160530      *                *------------------*
271000160530      *
271100160530     C                   ENDSR
271200160531     C**************************************************************************
271300160531      *  NON incasellati BRT  x DPD   MESE0    mese in corso analisi
271400160531     C**************************************************************************
271500160531     C     NON_incas0    begSR
271600160531      *
271700160531      * non incasellato
271800160531     c                   z-add     TSPD0(5)      SFITSP
271900160531     c                   z-add     TBRT0(5)      SFIIM1
272000160531      *
272100160531     c                   z-add     TSPDR0(6)     SAITSP
272200160531     c     TSPD0(6)      sub       TSPDR0(6)     SAITSP2
272300160531     c                   z-add     TBRTR0(6)     SAIIM1
272400160531     c     TBRT0(6)      sub       TBRTR0(6)     SAIIM12
272500160531      *
272600160531     c                   z-add     TSPD0(7)      SFETSP
272700160531     c                   z-add     TBRT0(7)      SFEIM1
272800160531      *
272900160531     c                   z-add     TSPDR0(8)     SAETSP
273000160531     c     TSPD0(8)      sub       TSPDR0(8)     SAETSP2
273100160531     c                   z-add     TBRTR0(8)     SAEIM1
273200160531     c     TBRT0(8)      sub       TBRTR0(8)     SAEIM12
273300160531      *
273400160531      *  in un unico totale
273500160531     c                   z-add     TSPD0(5)      TNBTSP0
273600160531     c                   add       TSPD0(8)      TNBTSP0
273700160531     c                   z-add     TSPD0(7)      TNBTSP
273800160531     c                   add       TSPD0(6)      TNBTSP
273900160531      *
274000160531     c                   z-add     TBRT0(5)      TNBIM10
274100160531     c                   add       TBRT0(8)      TNBIM10
274200160531     c                   z-add     TBRT0(7)      TNBIM1
274300160531     c                   add       TBRT0(6)      TNBIM1
274400160531      *   tot
274500160531      * x DPD deve sommare
274600160531     c                   z-add     TNBIM1        TNBDEL
274700160531     c                   z-add     TNBIM10       TNBDEL0
274800160531      *
274900160531      *                *------------------*
275000160531     c                   write     RnonINC
275100160531      *                *------------------*
275200160531      *
275300160531     C                   ENDSR
275400160531     C**************************************************************************
275500160531      *  NON incasellati BRT x DPD    MESE1  mese precedente
275600160531     C**************************************************************************
275700160531     C     NON_incas1    begSR
275800160531      *
275900160531      * non incasellato
276000160531     c                   z-add     TSPD1(5)      SFITSP
276100160531     c                   z-add     TBRT1(5)      SFIIM1
276200160531      *
276300160531     c                   z-add     TSPDR1(6)     SAITSP
276400160531     c     TSPD1(6)      sub       TSPDR1(6)     SAITSP2
276500160531     c                   z-add     TBRTR1(6)     SAIIM1
276600160531     c     TBRT1(6)      sub       TBRTR1(6)     SAIIM12
276700160531      *
276800160531     c                   z-add     TSPD1(7)      SFETSP
276900160531     c                   z-add     TBRT1(7)      SFEIM1
277000160531      *
277100160531     c                   z-add     TSPDR1(8)     SAETSP
277200160531     c     TSPD1(8)      sub       TSPDR1(8)     SAETSP2
277300160531     c                   z-add     TBRTR1(8)     SAEIM1
277400160531     c     TBRT1(8)      sub       TBRTR1(8)     SAEIM12
277500160531      *
277600160531      *  in un unico totale
277700160531     c                   z-add     TSPD1(5)      TNBTSP0
277800160531     c                   add       TSPD1(8)      TNBTSP0
277900160531     c                   z-add     TSPD1(7)      TNBTSP
278000160531     c                   add       TSPD1(6)      TNBTSP
278100160531      *
278200160531     c                   z-add     TBRT1(5)      TNBIM10
278300160531     c                   add       TBRT1(8)      TNBIM10
278400160531     c                   z-add     TBRT1(7)      TNBIM1
278500160531     c                   add       TBRT1(6)      TNBIM1
278600160531      *   tot
278700160531      * x DPD deve sommare
278800160531     c                   z-add     TNBIM1        TNBDEL
278900160531     c                   z-add     TNBIM10       TNBDEL0
279000160531      *
279100160531      *                *------------------*
279200160531     c                   write     RnonINC
279300160531      *                *------------------*
279400160531      *
279500160531     C                   ENDSR
279600160531     C**************************************************************************
279700160531      *  NON incasellati BRT x DPD    MESE2  2 mesi fa
279800160531     C**************************************************************************
279900160531     C     NON_incas2    begSR
280000160531      *
280100160531      * non incasellato
280200160531     c                   z-add     TSPD2(5)      SFITSP
280300160531     c                   z-add     TBRT2(5)      SFIIM1
280400160531      *
280500160531     c                   z-add     TSPDR2(6)     SAITSP
280600160531     c     TSPD2(6)      sub       TSPDR2(6)     SAITSP2
280700160531     c                   z-add     TBRTR2(6)     SAIIM1
280800160531     c     TBRT2(6)      sub       TBRTR2(6)     SAIIM12
280900160531      *
281000160531     c                   z-add     TSPD2(7)      SFETSP
281100160531     c                   z-add     TBRT2(7)      SFEIM1
281200160531      *
281300160531     c                   z-add     TSPDR2(8)     SAETSP
281400160531     c     TSPD2(8)      sub       TSPDR2(8)     SAETSP2
281500160531     c                   z-add     TBRTR2(8)     SAEIM1
281600160531     c     TBRT2(8)      sub       TBRTR2(8)     SAEIM12
281700160531      *
281800160531      *  in un unico totale
281900160531     c                   z-add     TSPD2(5)      TNBTSP0
282000160531     c                   add       TSPD2(8)      TNBTSP0
282100160531     c                   z-add     TSPD2(7)      TNBTSP
282200160531     c                   add       TSPD2(6)      TNBTSP
282300160531      *
282400160531     c                   z-add     TBRT2(5)      TNBIM10
282500160531     c                   add       TBRT2(8)      TNBIM10
282600160531     c                   z-add     TBRT2(7)      TNBIM1
282700160531     c                   add       TBRT2(6)      TNBIM1
282800160531      *   tot
282900160531      * x DPD deve sommare
283000160531     c                   z-add     TNBIM1        TNBDEL
283100160531     c                   z-add     TNBIM10       TNBDEL0
283200160531      *
283300160531      *                *------------------*
283400160531     c                   write     RnonINC
283500160531      *                *------------------*
283600160531      *
283700160531     C                   ENDSR
283800160601     C**************************************************************************
283900160601      *  NON incasellati BRT x DPD    MESE2  2 mesi fa
284000160601     C**************************************************************************
284100160601     C     NON_incas3    begSR
284200160601      *
284300160601      * non incasellato
284400160601     c                   z-add     TSPD3(5)      SFITSP
284500160601     c                   z-add     TBRT3(5)      SFIIM1
284600160601      *
284700160601     c                   z-add     TSPDR3(6)     SAITSP
284800160601     c     TSPD3(6)      sub       TSPDR3(6)     SAITSP2
284900160601     c                   z-add     TBRTR3(6)     SAIIM1
285000160601     c     TBRT3(6)      sub       TBRTR3(6)     SAIIM12
285100160601      *
285200160601     c                   z-add     TSPD3(7)      SFETSP
285300160601     c                   z-add     TBRT3(7)      SFEIM1
285400160601      *
285500160601     c                   z-add     TSPDR3(8)     SAETSP
285600160601     c     TSPD3(8)      sub       TSPDR3(8)     SAETSP2
285700160601     c                   z-add     TBRTR3(8)     SAEIM1
285800160601     c     TBRT3(8)      sub       TBRTR3(8)     SAEIM12
285900160601      *
286000160601      *  in un unico totale
286100160601     c                   z-add     TSPD3(5)      TNBTSP0
286200160601     c                   add       TSPD3(8)      TNBTSP0
286300160601     c                   z-add     TSPD3(7)      TNBTSP
286400160601     c                   add       TSPD3(6)      TNBTSP
286500160601      *
286600160601     c                   z-add     TBRT3(5)      TNBIM10
286700160601     c                   add       TBRT3(8)      TNBIM10
286800160601     c                   z-add     TBRT3(7)      TNBIM1
286900160601     c                   add       TBRT3(6)      TNBIM1
287000160601      *   tot
287100160601      * x DPD deve sommare
287200160601     c                   z-add     TNBIM1        TNBDEL
287300160601     c                   z-add     TNBIM10       TNBDEL0
287400160601      *
287500160601      ***** Al momento NON stampiamo anche se li calcoliamo
287600160601      *                *------------------*
287700160601     c**********         write     RnonINC
287800160601      *                *------------------*
287900160601      *
288000160601     C                   ENDSR
288100160530     C**************************************************************************
288200160530      *  NON incasellati DPD e gli ORM ITA-ITA
288300160530     C**************************************************************************
288400160530     C     NON_incasDPD  begSR
288500160530      *
288600160530      *   DPD non incasellato
288700160530     c                   z-add     TSPD(9)       TNDTSP
288800160530     c                   z-add     TPTN(9)       TNDIMP
288900160530      *   tot
289000160530     c                   z-add     TNDIMP        TNDDEL
289100160530      *
289200160530      *   DPD non incasellato  ORM ITA-ITA
289300160530     c                   z-add     TSPD(10)      TORTSP
289400160530     c                   z-add     TPTN(10)      TORIMP
289500160530      *   tot
289600160530     c                   z-add     TORIMP        TORDEL
289700160530      *
289800160530      *                *------------------*
289900160530     c                   write     RormITA
290000160530      *                *------------------*
290100160530      *
290200160530     C                   ENDSR
290300160530     C**************************************************************************
290400160530      *  Solo gli incasellato a contronto
290500160530     C**************************************************************************
290600160530     C     IncaseLLATI   begSR
290700160530      *
290800160530      *  incasellati
290900160530     c                   z-add     TSPD(1)       TFITSP
291000160530     c                   z-add     TBRT(1)       TFIIM1
291100160530     c                   z-add     TPTN(1)       TFIIMP
291200160530      * x DPD deve sottrarre
291300160530     c                   if        E84NTW = 'DPD'
291400160530     c     TFIIMP        sub       TFIIM1        TFIDEL
291500160530     c                   else
291600160530      * x gli altri aggiunge perchè il valore è già negativo e si sommerebbe
291700160530     c     TFIIMP        add       TFIIM1        TFIDEL
291800160530     c                   end
291900160530      *
292000160530     c                   z-add     TSPDR(2)      TAITSP
292100160530     c                   z-add     TBRTR(2)      TAIIM1
292200160530     c                   z-add     TPTNR(2)      TAIIMP
292300160530     c     TAIIMP        sub       TAIIM1        TAIDEL
292400160530      *
292500160530     c     TSPD(2)       sub       TSPDR(2)      TAITSP2
292600160530     c     TBRT(2)       sub       TBRTR(2)      TAIIM12
292700160530     c     TPTN(2)       sub       TPTNR(2)      TAIIMP2
292800160530     c     TAIIMP2       sub       TAIIM12       TAIDEL2
292900160530      *
293000160530     c                   z-add     TSPD(3)       TFETSP
293100160530     c                   z-add     TBRT(3)       TFEIM1
293200160530     c                   z-add     TPTN(3)       TFEIMP
293300160530     c     TFEIMP        sub       TFEIM1        TFEDEL
293400160530      *      resi
293500160530     c                   z-add     TSPDR(4)      TAETSP
293600160530     c                   z-add     TBRTR(4)      TAEIM1
293700160530     c                   z-add     TPTNR(4)      TAEIMP
293800160530      * x DPD deve sottrarre
293900160530     c                   if        E84NTW = 'DPD'
294000160530     c     TAEIMP        sub       TAEIM1        TAEDEL
294100160530     c                   else
294200160530      * x gli altri aggiunge perchè il valore è già negativo e si sommerebbe
294300160530     c     TAEIMP        add       TAEIM1        TAEDEL
294400160530     c                   end
294500160530      *
294600160530      *   ORM ITA-ITA
294700160530     c                   z-add     TSPD(11)      TAETSP1
294800160530     c                   z-add     TBRT(11)      TAEIM11
294900160530     c                   z-add     TPTN(11)      TAEIMP1
295000160530      * x DPD deve sottrarre
295100160530     c                   if        E84NTW = 'DPD'
295200160530     c     TAEIMP1       sub       TAEIM11       TAEDEL1
295300160530     c                   else
295400160530      * x gli altri aggiunge perchè il valore è già negativo e si sommerebbe
295500160530     c     TAEIMP1       add       TAEIM11       TAEDEL1
295600160530     c                   end
295700160530      *
295800160530     c     TSPD(4)       sub       TSPDR(4)      TAETSP2
295900160530     c     TBRT(4)       sub       TBRTR(4)      TAEIM12
296000160530     c     TPTN(4)       sub       TPTNR(4)      TAEIMP2
296100160530      * x DPD deve sottrarre
296200160530     c                   if        E84NTW = 'DPD'
296300160530     c     TAEIMP2       sub       TAEIM12       TAEDEL2
296400160530     c                   else
296500160530      * x gli altri aggiunge perchè il valore è già negativo e si sommerebbe
296600160530     c     TAEIMP2       add       TAEIM12       TAEDEL2
296700160530     c                   end
296800160530      *
296900160530      *  tot solo incasellato
297000160530     c     TSPD(1)       add       TSPD(4)       TOiTSPR
297100160530     c                   add       TSPD(11)      TOiTSPR
297200160530     c     TSPD(2)       add       TSPD(3)       TOiTSPC
297300160530      *
297400160530     c     TBRT(1)       add       TBRT(4)       TOiGE1R
297500160530     c                   add       TBRT(11)      TOiGE1R
297600160530     c     TBRT(2)       add       TBRT(3)       TOiGE1C
297700160530      *
297800160530     c     TPTN(1)       add       TPTN(4)       TOiGEPR
297900160530     c                   add       TPTN(11)      TOiGEPR
298000160530     c     TPTN(2)       add       TPTN(3)       TOiGEPC
298100160530      *
298200160530      * x DPD deve sottrarre
298300160530     c                   if        E84NTW = 'DPD'
298400160530     c     TOiGEPR       sub       TOiGE1R       TOiGEDR
298500160530     c                   else
298600160530      * x gli altri aggiunge perchè il valore è già negativo e si sommerebbe
298700160530     c     TOiGEPR       add       TOiGE1R       TOiGEDR
298800160530     c                   end
298900160530     c     TOiGEPC       sub       TOiGE1C       TOiGEDC
299000160530      *
299100160530      *                *------------------*
299200160530     c                   write     RincaSEL
299300160530      *                *------------------*
299400160530      *
299500160530      *
299600160530     C                   ENDSR
299700160530     C**************************************************************************
299800160530      *  deve eseguire l'estrazione dei dati come x incasellati 1 e 4
299900160530     C**************************************************************************
300000160530     C     DUNFOSS       begSR
300100160530      *
300200150429     c                   eval      x_Dunfoss = 'S'
300300150429     c                   eval       TFITSP124  =   TSPDD(1)
300400150429     c                   eval       TFIIM1124  =   TBRTD(1)
300500150429     c                   eval       TFIIMP124  =   TPTND(1)
300600150429     c                   eval       TFIDEL124  =   TDIFD(1)
300700150429      *
300800150429     C                   ENDSR
300900150429     C**************************************************************************
301000150429      *  deve eseguire l'estrazione dei dati come x incasellati 1 e 4
301100150429     C**************************************************************************
301200150429     C     VTG           begSR
301300150429      *
301400150429     c                   eval      x_VTG     = 'S'
301500150429     c                   eval       TFITSP094  =   TSPDV(1)
301600150429     c                   eval       TFIIM1094  =   TBRTV(1)
301700150429     c                   eval       TFIIMP094  =   TPTNV(1)
301800150429     c                   eval       TFIDEL094  =   TDIFV(1)
301900150429      *
302000150429     C                   ENDSR
302100150429     C**************************************************************************
302200150429      *  Scrive il File PC in 2 modi diversi
302300150429      *   a Spedizione/Riferimento/Raggruppamento         (max dett.)
302400150429      *     o a Fattura     (riepilogata)
302500150429     C**************************************************************************
302600150429     C     wrt_PCfile    begSR
302700150429      *
302800041102     C     KTas          Klist
302900041102     c                   Kfld                    ecfaas
303000041102     c                   Kfld                    ecflnp
303100041102     c                   Kfld                    ecfnrs
303200041102     c                   Kfld                    ecfnsp
303300160614      *
303400160614     C     Keu85         Klist
303500160614     c                   Kfld                    ecfaas
303600160614     c                   Kfld                    ecflnp
303700160614     c                   Kfld                    ecfnrs
303800160614     c                   Kfld                    ecfnsp
303900041103      *
304000041103     C     KAco          Klist
304100041103     c                   Kfld                    acokut
304200041103     c                   Kfld                    acokcc
304300041103     c                   Kfld                    acoksc
304400160615      *
304500160615     C     K85nt         Klist
304600160615     c                   Kfld                    ecfnaz
304700160615     c                   Kfld                    ecfcts
304800160616     c                   Kfld                    n85tipo
304900160622     c                   Kfld                    n85reso
305000160628     c                   Kfld                    n85cau
305100040901      **
305200040901     c                   clear                   fieu085
305300160224     c                   clear                   fieu0851
305400040901      **
305500040901     c                   eval      f85ntw = partner
305600041014     c                   select
305700041014     c                   when      ciclo_exec = 1
305800041014     c                   select
305900041014     c                   when      volte = 1
306000041014     c                   movel     AbbFraImp     f85des
306100041014     c                   when      volte = 2
306200041014     c                   movel     AbbAssImp     f85des
306300041014     c                   when      volte = 3
306400041014     c                   movel     AbbFraExp     f85des
306500041014     c                   when      volte = 4
306600041014     c                   movel     AbbAssExp     f85des
306700041014     c                   endsl
306800041014      **
306900041014     c                   when      ciclo_exec = 2
307000041014     c                   movel     NabAddPtn     f85des
307100151217      **
307200151217     c                   when      ciclo_exec = 10
307300151217     c                   movel     ORMItaItaN    f85des
307400041014      **
307500151217     c                   when      ciclo_exec = 11
307600151217     c                   movel     ORMItaIta     f85des
307700151217      **
307800041014     c                   when      ciclo_exec = 3
307900041014     c                   select
308000041014     c                   when      volte = 1
308100041014     c                   movel     NabFraImp     f85des
308200041014     c                   when      volte = 2
308300041014     c                   movel     NabAssImp     f85des
308400041014     c                   when      volte = 3
308500041014     c                   movel     NabFraExp     f85des
308600041014     c                   when      volte = 4
308700041014     c                   movel     NabAssExp     f85des
308800041014     c                   endsl
308900041014      **
309000041014     c                   endsl
309100041014      *
309200041015     c                   movel     tipo          f85xda
309300040901     c                   move      dadata        f85dal
309400040901     c                   move      adata         f85al
309500040901      **
309600041014      ** a dettaglio
309700040901     c                   eval      f85rif = ecfrif
309800040901     c                   eval      f85spb = %editc(ecfaas:'X') + ' '
309900040901     c                                    + %editc(ecflnp:'X') + ' '
310000040901     c                                    + %editc(ecfnrs:'X') + ' '
310100040901     c                                    + %editc(ecfnsp:'X')
310200040901     c                   eval      f85aas = ecfaas
310300040901     c                   eval      f85lnp = ecflnp
310400040901     c                   eval      f85nrs = ecfnrs
310500040901     c                   eval      f85nsp = ecfnsp
310600040901     c                   eval      f85dts = datspe
310700040901     c                   eval      f85lna = ecflna
310800040901     c                   eval      f85tpb = ecftbl
310900040901     c                   eval      f85fbr = ecffbr
311000040901      **
311100041014     c                   eval      f85nft = ecfnft
311200041014     c                   eval      f85dft = ecfdft
311300040901     c                   eval      f85grp = ecfgrp
311400040901     c                   eval      f85nfp = ecfnfp
311500040901     c                   eval      f85dfp = ecfdfp
311600040901     c                   eval      f85imp = ecfimp
311700040901     c                   eval      f85imc = ecfimc
311800041012     c                   eval      f85imr = ecfimr * (-1)
311900040901     c                   eval      f85del = detdel
312000040901     c                   eval      f85tde = tipo_confr
312100041102      **
312200151217     c                   if        f85lnp  > 0
312300151217      **
312400041102      **   + mittente e destinatario
312500041102     c     Ktas          chain     titas30C
312600041102     c                   if        %found(titas30C)
312700041102     c                   eval      f85dst = tasrsd
312800041102     c                   end
312900041102      **
313000041102      ** x il mittente occorre verificare il tipo di spedizione import/export
313100041102     c     Ktas          chain     titaa30C
313200041103     c                   if        %found(titaa30C)
313300041103     c                   if        taatrc= 'O' or taatrc= 'M'
313400041102     c                   eval      f85mit = taarsc
313500041103     c                   end
313600041102     c                   end
313700041103      **  Se non l'ha trovato
313800041103     c                   if        f85mit = *blank
313900041103     c                   z-add     1             acokut
314000041103     c                   z-add     151           acokcc
314100041103     c                   z-add     tasccm        acoksc
314200041103     C     KAco          chain     Cnaco00F
314300041103     c                   if        %found(Cnaco00F)
314400041103     c                   eval      f85mit = acorag
314500041103     c                   end
314600041103     c                   end
314700151217     c                   endif
314800150504      ** UTENTE
314900150504     c                   eval      f85ute = knmus
315000150504      **
315100040901     c                   write     fieu085
315200040901      **
315300160224      **  Solo i NON incasellati BRT e che non siano dei nostri EXPORT
315400160224     c                   if        f85ntw ='DPD'
315500160224     c                   if        %subst(f85des:1:3) ='(5)' or
315600160224     c                             %subst(f85des:1:3) ='(8)'
315700160307     c                   if           f85rif <> *blank
315800160307     c                   if        %subst(f85rif:1:4) <>'0844' and
315900160307     c                             %subst(f85rif:1:4) <>'0845' and
316000160307     c                             %subst(f85tpb:1:1) = 'F'    or
316100160307     c                             %subst(f85rif:1:4) = '0844' and
316200160307     c                             %subst(f85tpb:1:1) = 'A'    or
316300160307     c                             %subst(f85rif:1:4) = '0845' and
316400160307     c                             %subst(f85tpb:1:1) = 'A'
316500160614     c                   clear                   fieu0851
316600160614      **
316700160614      **   x Data Consegna da impostare sui NON INCASELLATI
316800160614     c     Keu85         chain     titas30C
316900160614     c                   if        %found(titas30C)
317000160614     c                   eval      F851datC= tasDCM
317100160614     c                   end
317200160224     c                   eval      F851NTW = F85NTW
317300160614     c                   eval      F851data= F85DTS
317400160224     c                   eval      F851DES = F85DES
317500160224     c                   eval      F851RIF = F85RIF
317600160224     c                   eval      F851IMC = F85IMC
317700160224     c                   eval      F851IMR = F85IMR
317800160624      **** scrivo solo se il Parcel ha un DEPOT valido per DPD
317900160624     c                   movel     F85RIF        wparcel          14
318000160624     c                   exsr      chkdepot
318100160624      **
318200160624     c                   if        scrivere = 'S'
318300160224     c                   write     fieu0851
318400160624     c                   endif
318500160624      **
318600160224     c                   end
318700160307     c                   end
318800160224     c                   end
318900160224     c                   end
319000160224      **
319100040901     C                   ENDSR
319200160624      *______________________________________________________________
319300160624     c     chkdepot      begsr
319400160624      *______________________________________________________________
319500160624     c                   move      'N'           scrivere          1
319600160624     c                   setoff                                       35
319700160624     c     digits        check     wparcel                                35
319800160624     C                   if        not *in35
319900160624     c                   movel     wparcel       k4                4
320000160624     c     k4            setll     dppdc01l
320100160624     c     k4            reade     dppdc01l
320200160624     c                   dow       not %EoF(dppdc01l)
320300160624     c                   if            pdcdsc >=oggig
320400160624     c                   move      'S'           scrivere
320500160624     C                   leave
320600160624     C                   endif
320700160624     c     k4            reade     dppdc01l
320800160624     C                   endDO
320900160624     C                   endif
321000160624     c                   endsr
321100160624      *______________________________________________________________
321200040901     C**************************************************************************
321300040826**
321400041014CLRPFM FILE(WFIEU85W)
321500041014CLRPFM FILE(WFIEU85F)
321600090721RUNQRY QRY(WFIEU85TQ)
321700160224CLRPFM FILE(WFIEU85F1)
321800160615CLRPFM FILE(WFIEU85NT)
