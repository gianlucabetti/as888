000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300030505     ffifce99l  uf   e           k disk
000400020610     feccet30c  if   e           k disk
000500020521     fecced30c  if   e           k disk
000600020517     ftitas30c  if   e           k disk
000700021203     ffiapd01l  if   e           k disk
000800030414     ffiftd05l  if   e           k disk
000900030417     ffiftt01l  if   e           k disk
001000020521     ffifss01l  uf a e           k disk
001100030321     ftabel00f  if   e           k disk
001200020517     fqsysprt   o    f  132        printer infds(stamp)
001300030414      *--------------------------------------------------------------------
001400020626     d kpjba         e ds
001500030321     d ds3a          e ds
001600030414     d Bolla_trs       s              1    inz('S')
001700030415     d Sum_record      s              1    inz('S')
001800030321     d vuoto           s              2    inz
001900030414     d Calc_Cmp        s              1
002000030321     d i               s              3  0 inz
002100030321     d sk3a            s              2    dim(50)
002200030414      *
002300030414     d NrFCE           s              9  0 inz
002400030414     d NoCnsNr         s              9  0 inz
002500030414     d NoCetNr         s              9  0 inz
002600030415     d NoFtdNr         s              9  0 inz
002700030417     d TotValC         s             12  3 inz
002800030417     d TotValR         s             12  3 inz
002900030414      *
003000030414     d  savaas         s                   like(fceaas)
003100030414     d  savlnp         s                   like(fcelnp)
003200030414     d  savnrs         s                   like(fcenrs)
003300030414     d  savnsp         s                   like(fcensp)
003400030414     d  savddc         s                   like(fceddc)
003500030417     d  savndc         s                   like(fcendc)
003600030414     d  savpdr         s                   like(fcepdr)
003700030414     d  savfgs         s                   like(fcefgs)
003800030414     d  savtsr         s                   like(fcetsr)
003900030414      *
004000030414     d Costi_fss       ds
004100030414     d  savfss001                          like(fss001)
004200030414     d  savfss005                          like(fss005)
004300030414     d  savfss016                          like(fss016)
004400030414     d  savfss019                          like(fss019)
004500030414     d  savfss034                          like(fss034)
004600030414     d  savfss035                          like(fss035)
004700030414     d  savfssXX1                          like(fssXX1)
004800030414     d  savfssXX2                          like(fssXX2)
004900030414     d  savfss997                          like(fss997)
005000030414     d  savfss998                          like(fss998)
005100030414      *
005200030414     d Compe_fss       ds
005300030414     d  savfss001b                         like(fss001b)
005400030414     d  savfss005b                         like(fss005b)
005500030414     d  savfss016b                         like(fss016b)
005600030414     d  savfss019b                         like(fss019b)
005700030414     d  savfss034b                         like(fss034b)
005800030414     d  savfss035b                         like(fss035b)
005900030414     d  savfssXX1b                         like(fssXX1b)
006000030414     d  savfssXX2b                         like(fssXX2b)
006100030414     d  savfss997b                         like(fss997b)
006200030414     d  savfss998b                         like(fss998b)
006300030414      *
006400020517     d stamp           ds
006500020517     d  lin                  367    368b 0
006600000607      *--------------------------------------------------------------------
006700020521      *loop sui record di fce ancora da trattare
006800030505     c     *loval        setll     fifce99l
006900030505     c                   read      fifce99l
007000030414      *  x EoF
0071000305051    c                   dow       not %Eof(fifce99l)
007200030414      *
007300030414      *  salva chiave con cui fare i saldi
007400030414     c                   eval      savfgs = fcefgs
007500030414     c                   eval      savddc = fceddc
007600030414     c                   eval      savpdr = fcepdr
007700030414      *
007800030414      *  Pulizia campi da totalizzare x DDC/PDR
007900030414     c                   clear                   Costi_Fss
008000030414     c                   clear                   Compe_Fss
008100030414      *
008200030414      *  x FGS/DDC/PDR
0083000305052    c                   dow       not %Eof(fifce99l) and
008400030414     c                             savfgs  =  fcefgs  and
008500030414     c                             savddc  =  fceddc  and
008600030414     c                             savpdr  =  fcepdr
008700030414      *  salva chiave Bolla
008800030417     c                   eval      savndc = fcendc
008900030414     c                   eval      savaas = fceaas
009000030414     c                   eval      savlnp = fcelnp
009100030414     c                   eval      savnrs = fcenrs
009200030414     c                   eval      savnsp = fcensp
009300030414     c                   eval      savtsr = fcetsr
009400030414      *
009500030414      * A rottura di Bolla
009600030414     C                   exsr      Check_Bolla
009700030414      *
009800030414      *  x FGS/DDC/PDR/Bolla/TSR
0099000305053    c                   dow       not %Eof(fifce99l) and
010000030414     c                             savfgs  =  fcefgs  and
010100030414     c                             savddc  =  fceddc  and
010200030414     c                             savpdr  =  fcepdr  and
010300030417     c                             savndc  =  fcendc  and
010400030414     c                             savaas  =  fceaas  and
010500030414     c                             savlnp  =  fcelnp  and
010600030414     c                             savnrs  =  fcenrs  and
010700030414     c                             savnsp  =  fcensp  and
010800030414     c                             savtsr  =  fcetsr
010900030414      * Totalizza i Costi
011000030415     c                   if        sum_record ='S'
011100030414     c                   exsr      somma_Costi
011200030414      *
011300030414      *  Una volta letto e totalizzato:
011400030414      *   mette il flag di saldato sul record FCE
011500030414     c                   add       1             NrFCE
011600030415     c*******            movel     'S'           fcesal
011700030415     c                   endIf
011800030415      *
011900030415      * aggiorna
012000030416     c   LR              update    fifce000
012100030505     c                   read      fifce99l
0122000304143    c                   enddo
012300030414      * a rottura di Bolla
0124000304142    c                   enddo
012500030414      *
012600030414      * a rottura di FGS/DDC/PDR
012700030414     C                   exsr      wri_Saldi
012800030414      *
0129000304141-   c                   enddo
013000030414      *
013100030414      *                 * ------------------ *
013200030414     c                   except    elaborati
013300030414      *                 * ------------------ *
013400030414     c                   seton                                        lr
013500030414     c*==================================================================*
013600030414      *  Controlli sulla Bolla
013700030414     c*==================================================================*
013800030414     c     Check_Bolla   begsr
013900030414      *
014000030415      * imposta il default per aggiornare il flag di saldato su FIFCE
014100030415      *  e sommare costi/competenze
014200030415     c                   eval      sum_record ='S'
014300030415      *
014400030415      * ------------------ *
014500030415      * Verifico se è una di quelle bolle da non trasmettere             .
014600030415      *  utilizzando la tab.: 3A
014700030415     c                   move      'S'           Bolla_trs
0148000304152    c                   if        fcecbo <> *blank
014900030415     c     fcecbo        lookup    sk3a                                   91
015000030415     c   91              move      'N'           Bolla_trs
0151000304152-   c                   endif
015200030415      *
015300030414      * ------------------ *
015400030414      *  Controlla data di consegna sul dettaglio Conteggi Padroncini
015500030414     c                   z-add     0             data_Cons         8 0
015600030414     c     kftd          chain     fiftd05l
015700030415      *
015800030414      * x prendere la data consegna
0159000304141    c                   if        %Found(fiftd05l)
016000030414     c                   z-add     ftdDCM        data_Cons
0161000304141e   c                   else
016200030417      *
016300030415      *  Controlla se fittizio (Escluso da Fatturazione)
016400030415     c     kapdfit       chain     fiapd01l                           95
016500030415     c  n95              if        apdPDD <> 'S'
016600030415      *
016700030415      * se il padroncino non era fittizio poichè la bolla doveva essere
016800030415      *  stata trasmessa segnala la mancanza del FTD per possibili problemi
016900030415      *   sulle trasmissioni.
017000030415     c                   if        Bolla_trs = 'S'
017100030417      *
017200030417     c     kftt          chain     fiftt01l
017300030417      *
0174000304171    c                   if        %Found(fiftt01l)
017500030415      *  Manca il FIFTD
017600030415     c                   add       1             NoFtdNr
017700030415      *                 * ------------------ *
017800030415     c                   except    noFtd
017900030415      *                 * ------------------ *
018000030417     c                   if        fcetsr = 'C'
018100030417     c                   add       fceice        totValC
018200030417     c                   end
018300030417     c                   if        fcetsr = 'R'
018400030417     c                   add       fceice        totValR
018500030417     c                   end
018600030415      *
018700030415      *  ......e non deve sommare  e va a fine routine
018800030415     c                   eval      sum_record ='N'
018900030415     c                   goto      end_chkbolla
019000030417      *
019100030417     c                   end
019200030417      *
019300030415     c                   end
019400030415      *
019500030415     c                   endIf
019600030415      *
019700030414      *   Se non c'è il dettaglio padroncini
019800030415      *   ...e quindi il padroncino non è stato valorizzato perchè fittizio
019900030414     c     ktas          Chain     titas30c
020000030415     c                   if        %Found(titas30c)
020100030414     c                   z-add     tasDCM        data_Cons
020200030414     c                   end
020300030414      *
0204000304141-   c                   endIf
020500030414      *
020600030414      * ------------------ *
020700030415      * solo per le prestazioni di consegna, se data consegna a zero,
020800030414      *  non sommo le competenze. Per i Ritiri invece sempre.
020900030414     c                   move      'S'           Calc_Cmp
021000030414      *
021100030414     c                   if        Data_Cons = 0 and fcetsr='C'
021200030414     c******             add       1             NoCnsNr
021300030414     c******             except    nocons
021400030414     c                   move      'N'           Calc_Cmp
021500030414     c                   end
021600030414      *
021700030414      * ------------------ * * ------------------ * * ------------------ *
021800030415      *  Quindi solo se deve calcolare le competenze
021900030415     c                   if        Calc_Cmp = 'S'
022000030415      *
022100030414      *  Cerca record su CE
022200030414     c     kcet          chain     eccet30c
022300030414      *
022400030414      *  Non c'è il C/E
0225000304141    c                   if        not %Found(eccet30c)
022600030414      *  Se la bolla è stata trasmessa e non ho il C/E segnalo errore
0227000304142    C                   IF        Bolla_Trs ='S'
022800030417      *
022900030417      *  Controlla che ci sia la bolla in sede altrimenti è una "£" di cui deve prendere
023000030417      *   i costi e non le competenze
023100030417     c     ktas          Chain     titas30c
023200030417     c                   if        %Found(titas30c)
023300030417      *
023400030414      *                 * ------------------ *
023500030414     c                   except    nocet
023600030414      *                 * ------------------ *
023700030414     c                   add       1             NoCetNr
023800030417     c                   if        fcetsr = 'C'
023900030417     c                   add       fceice        totValC
024000030417     c                   end
024100030417     c                   if        fcetsr = 'R'
024200030417     c                   add       fceice        totValR
024300030417     c                   end
024400030415      *
024500030415      *  ......e non deve sommare  e va a fine routine
024600030415     c                   eval      sum_record ='N'
024700030415     c                   goto      end_chkbolla
024800030417      *
0249000304172-   c                   end
025000030415      *
0251000304142-   c                   end
025200030414      *
0253000304141e   c                   else
025400030414      *  Ho le Competenze
025500030414      *   solo se devono essere calcolate le sommo
0256000304142    c                   if        Calc_Cmp = 'S'
025700030414      *
025800030414      *  e somma relativamente al Tipo Servizio le relative competenze
025900030415     c                   select
0260000304153    c                   when      fcetsr = 'R'
026100030414     c                   add       cetpar        savfss001b
0262000304153    c                   when      fcetsr = 'C'
026300030414     c                   add       cetard        savfss005b
0264000304153-   c                   endsl
026500030414      *
026600030414      *  quindi cerca le competenze di dettaglio se ci sono e le somma
0267000304143    c                   if        cetdet = 'S'
026800030414     c                   exsr      somma_Compe
0269000304143-   c                   endif
027000030414      *
0271000304142-   c                   endif
027200030415      *
0273000304141-   c                   endif
027400030415      *
0275000304151-   c                   endif
027600030414      *
027700030415     c     end_chkbolla  endsr
027800030414     c*==================================================================*
027900030414     c     Somma_Compe   begsr
028000030414      *
028100020521     c     kced          setll     ecced30c
028200030414     c     kced          reade     ecced30c
028300030414      *
028400030414     c                   dow       not %Eof(ecced30c)
028500030414      *
028600020521     c                   select
028700030415     c                   when      cedcmp = 16 and fcetsr = 'C'
028800030414     c                   add       cedimp        savfss016b
028900030414      *
029000030415     c                   when      cedcmp = 19 and fcetsr = 'C'
029100030414     c                   add       cedimp        savfss019b
029200030414      *
029300030415     c                   when      cedcmp = 34 and fcetsr = 'C'
029400030414     c                   add       cedimp        savfss034b
029500030414      *
029600030415     c                   when      cedcmp = 35 and fcetsr = 'R'
029700030414     c                   add       cedimp        savfss035b
029800030430      *
029900030430     c                   when      cedcmp = 24 and fcetsr = 'C'
030000030430     c                   add       cedimp        savfssxx1b
030100030430      *
030200030430     c                   when      cedcmp = 43 and fcetsr = 'C'
030300030430     c                   add       cedimp        savfssxx2b
030400030430      *
030500030430     c                   when      cedcmp = 62 and fcetsr = 'C'
030600030430     c                   add       cedimp        savfssxx2b
030700030430      *
030800020521     c                   endsl
030900030414      *
031000030414     c     kced          reade     ecced30c
031100020521     c                   enddo
031200030414      *
031300020521     c                   endsr
031400030414     c*==================================================================*
031500030414     c     somma_costi   begsr
031600030414      *
031700020521     c                   select
031800020521     c                   when      fcecce = '001'
031900030414     c                   add       fceice        savfss001
032000030414      *
032100020521     c                   when      fcecce = '005'
032200030414     c                   add       fceice        savfss005
032300030414      *
032400020521     c                   when      fcecce = '016'
032500030414     c                   add       fceice        savfss016
032600030414      *
032700020521     c                   when      fcecce = '019'
032800030414     c                   add       fceice        savfss019
032900030414      *
033000020521     c                   when      fcecce = '034'
033100030414     c                   add       fceice        savfss034
033200030414      *
033300020521     c                   when      fcecce = '035'
033400030414     c                   add       fceice        savfss035
033500030414      *
033600020521     c                   when      fcecce = '997'
033700030414     c                   add       fceice        savfss997
033800030414      *
033900020521     c                   when      fcecce = '998'
034000030414     c                   add       fceice        savfss998
034100020521     c                   endsl
034200030414      *
034300020521     c                   endsr
034400030414     c*==================================================================*
034500030414      * Scrive i saldi
034600030414     c*==================================================================*
034700030414     c     Wri_saldi     begsr
034800030414      *
034900030414      * Controlla esistenza Saldi giornalieri
035000030414     c     kfss          chain     fifss01l
035100030414     c                   if        not %Found(fifss01l)
035200030414      *
035300030414     c                   clear                   fifss000
035400030414      *
035500030414     c     kapdf         chain     fiapd01l                           95
035600030414     c  n95              move      apdpdd        fsspdd
035700030414      *
035800030414     c                   move      savpdr        fsspdr
035900030414     c                   move      savddc        fssddc
036000030414     c                   move      savfgs        fssfgs
036100030414      *
036200030414      *sommo comunque i costi da FCE e le Competenze
036300030414     c                   exsr      Costi_in_Fss
036400030414     c                   exsr      Compe_in_Fss
036500030414      *
036600030414     c                   write     fifss000
036700030414      *
036800030414     c                   else
036900030414      *
037000030414      *sommo comunque i costi da FCE e le Competenze
037100030414     c                   exsr      Costi_in_Fss
037200030414     c                   exsr      Compe_in_Fss
037300030414      *
037400030414     c                   update    fifss000
037500030414     c                   endif
037600030414      *
037700030414     c                   endsr
037800030414     c*==================================================================*
037900030414      *  Imposta i valori Costi
038000030414     c*==================================================================*
038100030414     c     Costi_in_Fss  begsr
038200030414     c                   add       savfss001     fss001
038300030414     c                   add       savfss005     fss005
038400030414     c                   add       savfss016     fss016
038500030414     c                   add       savfss019     fss019
038600030414     c                   add       savfss034     fss034
038700030414     c                   add       savfss035     fss035
038800030414     c                   add       savfssXX1     fssXX1
038900030414     c                   add       savfssXX2     fssXX2
039000030414     c                   add       savfss997     fss997
039100030414     c                   add       savfss998     fss998
039200030414     c                   endsr
039300030414     c*==================================================================*
039400030414      *  Imposta i valori Competenze
039500030414     c*==================================================================*
039600030414     c     Compe_in_Fss  begsr
039700030414     c                   add       savfss001b    fss001b
039800030414     c                   add       savfss005b    fss005b
039900030414     c                   add       savfss016b    fss016b
040000030414     c                   add       savfss019b    fss019b
040100030414     c                   add       savfss034b    fss034b
040200030414     c                   add       savfss035b    fss035b
040300030414     c                   add       savfssXX1b    fssXX1b
040400030414     c                   add       savfssXX2b    fssXX2b
040500030414     c                   endsr
040600030414     c*==================================================================*
040700030414     c     *inzsr        begsr
040800030414     c*==================================================================*
040900020626     c     *entry        plist
041000020626     c                   parm                    kpjba
041100030414      *
041200020521     c                   z-add     66            lin
041300030414      *                 * ------------------ *
041400020521     c                   except    testa
041500030414      *                 * ------------------ *
041600030414      *
041700020521     c     ktas          klist
041800020521     c                   kfld                    fceaas
041900020521     c                   kfld                    fcelnp
042000020521     c                   kfld                    fcenrs
042100020521     c                   kfld                    fcensp
042200030414      *
042300020521     c     kcet          klist
042400030414     c                   kfld                    fceaas
042500030414     c                   kfld                    fcelnp
042600030414     c                   kfld                    fcenrs
042700030414     c                   kfld                    fcensp
042800030414      *
042900020521     c     kced          klist
043000020521     c                   kfld                    cetaas
043100020521     c                   kfld                    cetlnp
043200020521     c                   kfld                    cetnrs
043300020521     c                   kfld                    cetnsp
043400020521
043500020521     c     kfss          klist
043600030414     c                   kfld                    savfgs
043700030414     c                   kfld                    savddc
043800030414     c                   kfld                    savpdr
043900030414      *
044000030414     c     kftd          klist
044100030414     c                   kfld                    fcepdr
044200030414     c                   kfld                    fcetsr
044300030414     c                   kfld                    fcendc
044400030414     c                   kfld                    fceddc
044500030414     c                   kfld                    fceaas
044600030414     c                   kfld                    fcelnp
044700030414     c                   kfld                    fcenrs
044800030414     c                   kfld                    fcensp
044900030417      *
045000030417     c     kftt          klist
045100030417     c                   kfld                    fcepdr
045200030417     c                   kfld                    fcetsr
045300030417     c                   kfld                    fcendc
045400030417     c                   kfld                    fceddc
045500030414      *
045600030415     c     kapdfit       klist
045700021203     c                   kfld                    apdtip
045800030415     c                   kfld                    fcepdr
045900030415      *
046000030415     c     kapdf         klist
046100030415     c                   kfld                    apdtip
046200030415     c                   kfld                    savpdr
046300021203     c                   movel     'A'           apdtip
046400030321      *
046500030321     c     ktab          klist
046600030321     c                   kfld                    tblkut
046700030321     c                   kfld                    tblcod
046800030321     c                   z-add     1             tblkut
046900030321     c                   move      '3A'          tblcod
047000030321      *
047100030321      *carico in skiera i codici bolla che non vengono trasmessi in sede x
047200030321      *forzare la somma dei rekord senza corrispondenza
047300030321     c     ktab          setll     tabel00f
047400030321     c                   do        *hival
047500030321     c     ktab          reade     tabel00f                               93
047600030321     c   93              leave
047700030321     c                   if        tblflg <> *blank
047800030321     c                   iter
047900030321     c                   end
048000030321     c                   movel     tbluni        ds3a
048100030321     c                   if        §3absd = *blank
048200030321     c                   iter
048300030321     c                   end
048400030321     c                   z-add     1             i
048500030321     c     vuoto         lookup    sk3a(i)                                92
048600030321     c   92              movel     tblkey        sk3a(i)
048700030321     c                   enddo
048800020521     c                   endsr
048900030414     c*==================================================================*
049000020517     OQSYSPRT   e            TESTA             2
049100020517     O                                           70 'Anomalie riscontrate fluss-
049200020517     O                                              o C/E di filiale'
049300030414     O*****     e            nocons      1
049400030414     o*****                  fceaas
049500030414     o*****                  fcelnp             + 1
049600030414     o*****                  fcenrs             + 1
049700030414     o*****                  fcensp             + 1
049800030414     o*****                  fcecce             + 1
049900030414     o*****                  fceice        2    + 1
050000030414     o*****                                     + 2 'Data CONSEGNA  = 0  '
050100030415     O          e            noFtd       1
050200030415     o                       fceaas
050300030415     o                       fcelnp             + 1
050400030415     o                       fcenrs             + 1
050500030415     o                       fcensp             + 1
050600030415     o                       fcecce             + 1
050700030415     o                       fceice        2    + 1
050800030417     o                                          + 2 ' non c''è  FIFTD --->   '
050900030416     o                       fcetsr             + 1
051000030416     o                       fcepdr        Z    + 2
051100030416     o                       fcendc        Z    + 2
051200030416     o                       fceddc             + 2
051300030417     o                       fcesal             + 2
051400030415     o
051500020517     O          e            nocet       1
051600020517     o                       fceaas
051700020517     o                       fcelnp             + 1
051800020517     o                       fcenrs             + 1
051900020517     o                       fcensp             + 1
052000020517     o                       fcecce             + 1
052100020610     o                       fceice        2    + 1
052200020517     o                                          + 2 ' non trovato Rec. ECCET'
052300030416     o                       fcetsr             + 1
052400030416     o                       fcepdr        Z    + 2
052500030416     o                       fcendc        Z    + 2
052600030416     o                       fceddc             + 2
052700030417     o                       fcesal             + 2
052800020517     o
052900020517     O          e            elaborati   1
053000030414     o                       NrFCE         z    + 2
053100030415     o                                          + 2 'Rec.FIFCE elaborati'
053200030414     o*****                  NoCnsNr       z    + 2
053300030415     o*****                                     + 2 'Rec.TITAS no cons.'
053400030415     o                       NoFtdNr       z    + 2
053500030415     o                                          + 2 'Rec.FIFTD non trasm.'
053600030415     o                       NoCetNr       z    + 2
053700030415     o                                          + 2 'Rec.ECCET non trov.'
053800030417     O          e            elaborati   1
053900030417     o                                          + 2 'Totale Valori Costi:'
054000030417     o                       TotValC       K    + 2
054100030417     o                                          + 2 'Consegne'
054200030417     o                       TotValR       K    + 2
054300030417     o                                          + 2 'Ritiri'
