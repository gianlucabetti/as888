000100031030     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200031017
000300031017      *---------------------------------------------------------------*
000400031017      *                                                               *
000500031030      *       IMPOSTA FLAG DI STAMPATO DDT SI IN ARRIVO               *
000600060214      *       PER AVVIO P.O. AL PICKING DIRETTO                       *
000700031017      *                                                               *
000800031017      *---------------------------------------------------------------*
000900031017
001000060214     fFnarb57j  if   e           k Disk
001100060214     fFiar401l  uf   e           k Disk
001200040706     fAzorg01l  if   e           k Disk
001300050914     fQsysprt   o    f  132        Printer Oflind(*InOf) usropn
001400070308     ffims99d   cf   e             workstn sfile(ms99s02:nrr2)
001500070308
001600070308      *------------------------------------------------------------------------*
001700070308      * riepilogo indicatori
001800070308      *------------------------------------------------------------------------*
001900070308      * 20 - gestione subfile
002000070308      * 21 - gestione subfile
002100070308      * 28 - messaggio errore
002200070308      * 31 - fine subfile
002300031030
002400070308      *------------------------------------------------------------------------*
002500070308      * campi di work
002600070308      *------------------------------------------------------------------------*
002700050914     d command         s             80
002800031030     d data            s              8  0
002900040209     d dataoggi        s              8  0
003000031104     d kTrc            s                   Like(Ar4Trc) Inz('A')
003100050914     d lung            s             15  5 inz(80)
003200070308     d nrr2            s                   like(recsf2)
003300031030     d oldnfv          s                   Like(ArbNfv)
003400031030     d poela           s              3
003500031030     d wagg            s              1
003600031030     d wdata           s               d   datfmt(*iso)
003700050914     d wtot            s              7  0 inz
003800070308     d w0030           s                   like(orgfil)
003900070308     d xx              s              2  0 inz
004000050914
004100070308      *------------------------------------------------------------------------*
004200070308      * schiere
004300070308      *------------------------------------------------------------------------*
004400050914     d cmd             s             80    dim(1)  ctdata perrcd(1)
004500031030
004600070308      *------------------------------------------------------------------------*
004700070308      * ds interne/esterne
004800070308      *------------------------------------------------------------------------*
004900031030     d                 ds
005000031030     d ArbAas                  1      4  0
005100031030     d ArbMgs                  5      8  0
005200031030     d ArbDsp                  1      8  0
005300031017
005400031030     d Fnlv53ds      e ds
005500031104     d dsBl4a        e ds
005600040706     d Og143         e ds
005700070308     d kpjba         e ds
005800070308     d trul06ds      e ds                  inz
005900070308     d  lin                    1     90    dim(30)
006000070308
006100070308      *------------------------------------------------------------------------*
006200070308
006300070308     c                   exsr      sr_pulsfl
006400070308     c                   exsr      sr_carsfl
006500070308     c                   exsr      sr_emisfl
006600070308
006700070308     c     fine          tag
006800070308
006900070308     c                   eval      *inlr = *on
007000070308
007100070308      *------------------------------------------------------------------------*
007200070308      * pulizia del subfile
007300070308      *------------------------------------------------------------------------*
007400070308     c     sr_pulsfl     begsr
007500070308
007600070308     c                   clear                   nrr2
007700070308     c                   eval      *in20 = *off
007800070308     c                   eval      *in21 = *off
007900070308     c                   write     ms99c02
008000070308     c                   eval      *in21 = *on
008100070308     c                   eval      *in20 = *on
008200070308
008300070308     c                   eval      recsf2 = 1
008400070308     c                   eval      *in31 = *off
008500070308
008600070308     c                   endsr
008700070308
008800070308      *------------------------------------------------------------------------*
008900070308      * carico subfile con le filiali
009000070308      *------------------------------------------------------------------------*
009100070308     c     sr_carsfl     begsr
009200070308
009300070308      * leggo la schiera delle filiali
009400070308     c                   do        30            xx
009500070308     c                   if        lin(xx) = *blanks or lin(xx) = *zeros
009600070308     c                   iter
009700070308     c                   endif
009800070308     c                   move      lin(xx)       w0030
009900070308     c     w0030         chain     azorg01l
010000070308     c                   if        not %found(azorg01l)
010100070308     c                   iter
010200070308     c                   endif
010300070308     c                   eval      w2cfil = w0030
010400070308     c                   eval      w2dfil = orgdes
010500070308     c                   eval      w2csce = '1'
010600070308     c                   eval      nrr2 = xx
010700070308     c                   write     ms99s02
010800070308     c                   enddo
010900070308
011000070308     c                   eval      *in31 = *on
011100070308
011200070308     c                   endsr
011300070308
011400070308      *------------------------------------------------------------------------*
011500070308      * emetto il subfile
011600070308      *------------------------------------------------------------------------*
011700070308     c     sr_emisfl     begsr
011800070308
011900070308     c                   do        *hival
012000070308
012100070308     c                   if        recsf2 > nrr2
012200070308     c                   eval      recsf2 = 1
012300070308     c                   endif
012400070308
012500070308     c                   write     ms99p02
012600070308     c                   exfmt     ms99c02
012700070308
012800070308     c                   eval      *in28 = *off
012900070308     c                   clear                   w2cmsg
013000070308
013100070308      * f3=fine
013200070308     c                   if        *inkc
013300070308     c                   goto      fine
013400070308     c                   endif
013500070308
013600070308      * f6=conferma
013700070308     c                   if        *inkf
013800070308     c                   exsr      sr_contrsfl
013900070308     c                   leave
014000070308     c                   endif
014100070308
014200070308     c                   enddo
014300070308
014400070308     c                   endsr
014500070308
014600070308      *------------------------------------------------------------------------*
014700070308      *  controllo i dati del subfile
014800070308      *------------------------------------------------------------------------*
014900070308     c     sr_contrsfl   begsr
015000070308
015100070308     c                   clear                   nrr2
015200070308     c                   do        *hival
015300070308     c                   add       1             nrr2
015400070308     c     nrr2          chain     ms99s02                            32
015500070308      * esco se fine sfl
015600070308     c                   if        *in32
015700070308     c                   leave
015800070308     c                   endif
015900070308
016000070308      * scelta = 1 lancio pgm
016100070308     c                   if        w2csce = '1'
016200070308     c                   exsr      sr_aggiorna
016300070308     c                   eval      recsf2 = nrr2
016400070308     c                   clear                   w2csce
016500070308     c                   update    ms99s02
016600070308
016700070308     c                   else
016800070308     c                   update    ms99s02
016900070308     c                   endif
017000070308
017100070308     c                   enddo
017200070308
017300070308     c                   endsr
017400070308
017500070308      *------------------------------------------------------------------------*
017600070308      *  lancio il pgm per aggiornare il flag ddt SI
017700070308      *------------------------------------------------------------------------*
017800070308     c     sr_aggiorna   begsr
017900070308
018000070308     c                   eval      *in90 = *off
018100070308     c                   clear                   wtot
018200031017
018300060214     c     *Loval        Setll     Fnarb57j
018400031021do  1c                   Do        *Hival
018500060214     c                   Read      Fnarb57j
018600031030      * Fine file
018700060214     c                   If        %Eof(Fnarb57j)
018800031017     c                   Leave
018900031017     c                   EndIf
019000031030      * Leggo solo le bolle con Lna = a p.o. passato
019100070308     c                   If        ArbLna <> w2cfil
019200031030     c                   Iter
019300031030     c                   EndIf
019400031113      * Leggo solo le bolle con Ar4Bam = 'S' o 'C' o 'K'
019500031030     c                   If        Ar4Bam <> 'S' and Ar4Bam <> 'C'
019600040706     c                             and Ar4Bam <> 'K' and Ar4Bam <> 'Y'
019700031017     c                   Iter
019800031017     c                   EndIf
019900031030
020000031030      * Se bolla consegnata o data spedizione inferiore a 5 gg. da oggi
020100031030      * imposto sempre come stampata
020200031030if  2c                   If        ArbDcm > *Zeros or ArbDsp < Data
020300060214     c     kAr4          Chain     Fiar401l
020400060214if  3c                   If        %Found(Fiar401l)
020500031104     c                   Movel     Ar4Not        dsBl4a
020600031104     c                   If        §B4Abm = 'S'
020700031104     c                   Eval      §B4Abm = 'P'
020800031113     c                   EndIf
020900031113     c                   If        §B4Abm = 'C'
021000031104     c                   Eval      §B4Abm = 'Q'
021100031030     c                   EndIf
021200031113     c                   If        §B4Abm = 'K'
021300031113     c                   Eval      §B4Abm = 'W'
021400031113     c                   EndIf
021500040706     c                   If        §B4Abm = 'Y'
021600040706     c                   Eval      §B4Abm = 'J'
021700040706     c                   EndIf
021800031104     c                   Movel     dsBl4a        Ar4Not
021900060214     c                   Z-add     *date         ar4duv
022000060214     c                   Update    Fiar4000
022100050914     c                   add       1             wtot
022200031104     c                   GoTo      ok
022300031104    3c                   EndIf
022400031030    2c                   EndIf
022500031017
022600040209      * Se linea partenza bolla = a linea arrivo bolla e la data spedizione non è
022700040209      * = a oggi imposto sempre come stampata
022800040209if  2c                   If        ArbLnp = ArbLna and ArbDsp < Dataoggi
022900060214     c     kAr4          Chain     Fiar401l
023000060214if  3c                   If        %Found(Fiar401l)
023100040209     c                   Movel     Ar4Not        dsBl4a
023200040209     c                   If        §B4Abm = 'S'
023300040209     c                   Eval      §B4Abm = 'P'
023400040209     c                   EndIf
023500040209     c                   If        §B4Abm = 'C'
023600040209     c                   Eval      §B4Abm = 'Q'
023700040209     c                   EndIf
023800040209     c                   If        §B4Abm = 'K'
023900040209     c                   Eval      §B4Abm = 'W'
024000040209     c                   EndIf
024100040706     c                   If        §B4Abm = 'Y'
024200040706     c                   Eval      §B4Abm = 'J'
024300040706     c                   EndIf
024400040209     c                   Movel     dsBl4a        Ar4Not
024500060214     c                   Z-add     *date         ar4duv
024600060214     c                   Update    Fiar4000
024700050914     c                   add       1             wtot
024800040209     c                   GoTo      ok
024900040209    3c                   EndIf
025000040209    2c                   EndIf
025100040209
025200031030      * Controllo se il foglio viaggio partenza è abbinato
025300031030if  2c                   If        ArbNfv <> OldNfv
025400031030     c                   Clear                   wagg
025500031030     c                   Clear                   Fnlv53ds
025600031030     c                   Eval      D53Tfo = 'A'
025700031030     c                   Eval      D53Nfv = ArbNfv
025800160509     c                   Eval      D53Fgs = Arbtfp
025900040209     c                   Eval      D53Abb = 'B'
026000031030     c                   Eval      D53Lna = ArbLna
026100031030     c                   Eval      D53Tfa = ArbTfa
026200031030     c                   Call      'FNLV53R'
026300031030     c                   Parm                    Fnlv53ds
026400031030      * Foglio abbinato aggiorno
026500031030if  3c                   If        D53Err = *Blanks and D53Nfa > *Zeros
026600031030     c                   Eval      wagg = '1'
026700031030    3c                   EndIf
026800031030     c                   Eval      OldNfv = ArbNfv
026900031030    2c                   EndIf
027000031030
027100031030if  2c                   If        wagg = '1'
027200060214     c     kAr4          Chain     Fiar401l
027300060214if  3c                   If        %Found(Fiar401l)
027400031104     c                   Movel     Ar4Not        dsBl4a
027500031104     c                   If        §B4Abm = 'S'
027600031104     c                   Eval      §B4Abm = 'P'
027700031113     c                   Endif
027800031113     c                   If        §B4Abm = 'C'
027900031104     c                   Eval      §B4Abm = 'Q'
028000031104     c                   EndIf
028100031113     c                   If        §B4Abm = 'K'
028200031113     c                   Eval      §B4Abm = 'W'
028300031113     c                   EndIf
028400040706     c                   If        §B4Abm = 'Y'
028500040706     c                   Eval      §B4Abm = 'J'
028600040706     c                   EndIf
028700031104     c                   Movel     dsBl4a        Ar4Not
028800060214     c                   Z-add     *date         ar4duv
028900060214     c                   Update    Fiar4000
029000050914     c                   add       1             wtot
029100040209     c                   GoTo      ok
029200031104    3c                   EndIf
029300031030    2c                   EndIf
029400040209
029500040209      * Come ultimo controllo
029600040209      * se bolla in consegna imposto sempre come stampata
029700040209if  2c                   If        ArbNdc > *Zeros
029800060214     c     kAr4          Chain     Fiar401l
029900060214if  3c                   If        %Found(Fiar401l)
030000040209     c                   Movel     Ar4Not        dsBl4a
030100040209     c                   If        §B4Abm = 'S'
030200040209     c                   Eval      §B4Abm = 'P'
030300040209     c                   EndIf
030400040209     c                   If        §B4Abm = 'C'
030500040209     c                   Eval      §B4Abm = 'Q'
030600040209     c                   EndIf
030700040209     c                   If        §B4Abm = 'K'
030800040209     c                   Eval      §B4Abm = 'W'
030900040209     c                   EndIf
031000040706     c                   If        §B4Abm = 'Y'
031100040706     c                   Eval      §B4Abm = 'J'
031200040706     c                   EndIf
031300040209     c                   Movel     dsBl4a        Ar4Not
031400060214     c                   Z-add     *date         ar4duv
031500060214     c                   Update    Fiar4000
031600050914     c                   add       1             wtot
031700040706     c                   GoTo      ok
031800040209    3c                   EndIf
031900040209    2c                   EndIf
032000040706
032100040712      * Ultimissimi controlli
032200040706      * se lnp poste e flag 'Y' devo mettere 'J'
032300040706     c                   Clear                   Og143
032400040706     c     ArbLnp        Chain     Azorg01l
032500040706if  2c                   If        %Found(Azorg01l)
032600040706     c                   Eval      Og143 = OrgDe3
032700040706    2c                   EndIf
032800040712if  2c                   If        §OgNtw = 'PPT' and Ar4Bam = 'Y'
032900060214     c     kAr4          Chain     Fiar401l
033000060214if  3c                   If        %Found(Fiar401l)
033100040706     c                   Movel     Ar4Not        dsBl4a
033200040706if  4c                   If        §B4Abm = 'Y'
033300040706     c                   Eval      §B4Abm = 'J'
033400040706    4c                   EndIf
033500040706     c                   Movel     dsBl4a        Ar4Not
033600060214     c                   Z-add     *date         ar4duv
033700060214     c                   Update    Fiar4000
033800050914     c                   add       1             wtot
033900040712     c                   GoTo      ok
034000040706    3c                   EndIf
034100040706    2c                   EndIf
034200040712
034300040712      * se flag 'Y' devo mettere 'J'
034400040712if  2c                   If        Ar4Bam = 'Y'
034500060214     c     kAr4          Chain     Fiar401l
034600060214if  3c                   If        %Found(Fiar401l)
034700040712     c                   Movel     Ar4Not        dsBl4a
034800040712if  4c                   If        §B4Abm = 'Y'
034900040712     c                   Eval      §B4Abm = 'J'
035000040712    4c                   EndIf
035100040712     c                   Movel     dsBl4a        Ar4Not
035200060214     c                   Z-add     *date         ar4duv
035300060214     c                   Update    Fiar4000
035400050914     c                   add       1             wtot
035500050914     c                   GoTo      ok
035600040712    3c                   EndIf
035700040712    2c                   EndIf
035800050927
035900050927      * Se arrivo qua vuol dire che non ho aggiornato il flag
036000050927      * Se c'era errore su lv53 stampo il motivo
036100050927if  2c                   If        d53err <> *Blanks
036200050927if  3c                   If        *In90 = *Off or *InOf
036300050927     c                   Except    Testa
036400050927     c  n90              Eval      *In90 = *On
036500050927    3c                   EndIf
036600050927     c                   Except    Riga
036700050927    2c                   EndIf
036800031104
036900031104     c     ok            Tag
037000031030
037100031021    1c                   EndDo
037200031017
037300031030     c                   Clear                   Fnlv53ds
037400031030     c                   Eval      D53Tla = 'C'
037500031030     c                   Call      'FNLV53R'
037600031030     c                   Parm                    Fnlv53ds
037700050927
037800050914      * Stampo il totale dei rcd aggiornati
037900050914     c                   If        *In90 = *Off or *InOf
038000050914     c                   Except    Testa
038100050914     c  n90              Eval      *In90 = *On
038200050914     c                   EndIf
038300050914     c                   Except    Totale
038400070308
038500070308     c                   endsr
038600070308
038700070308      *------------------------------------------------------------------------*
038800070308      * routine iniziale
038900070308      *------------------------------------------------------------------------*
039000070308     c     *inzsr        begsr
039100070308
039200070308     c     *entry        plist
039300070308     c                   parm                    poela
039400070308
039500070308     c                   clear                   trul06ds
039600070308     c                   eval      d06cod = '£6'
039700070308     c                   eval      d06key = poela
039800070308     c                   eval      d06tla = 'L'
039900070308     c                   eval      kpjbu = trul06ds
040000070308     c                   call      'TRUL06R'
040100070308     c                   parm                    kpjba
040200070308     c                   eval      trul06ds = kpjbu
040300070308
040400070308     c                   Z-add     *date         dataoggi
040500070308     c                   Z-add     *date         data
040600070308     c     *iso          Move      data          wdata
040700070308     c                   Subdur    5:*d          wdata
040800070308     c                   Move      wdata         data
040900070308
041000070308     c     kAr4          Klist
041100070308     c                   Kfld                    ArbAas
041200070308     c                   Kfld                    ArbLnp
041300070308     c                   Kfld                    ArbNrs
041400070308     c                   Kfld                    ArbNsp
041500070308     c                   Kfld                    kTrc
041600070308
041700070308     c                   Clear                   command
041800070308     c                   Movea     cmd(1)        command
041900070308     c                   call      'QCMDEXC'
042000070308     c                   parm                    command
042100070308     c                   parm                    lung
042200070308      *   Apro file stampa
042300070308     c                   Open      qsysprt
042400070308
042500070308     c                   endsr
042600031017
042700050914     oQsysprt   e            Testa          2 02
042800050914     o                                         +  5 'Variazione flag stampa'
042900050914     o                                         +  1 'DDT in arrivo'
043000050914     o                                         +  1 'LNA'
043100070308     o                       w2cfil            +  1
043200060214     o                                           85 'FIMS99R'
043300050914     o                       *Date         y    110
043400050914     o                                          128 'Pag.'
043500050914     o                       Page          z    132
043600050914     o          e            Riga           1
043700050914     o                                           +1 'Spedizione:'
043800050914     o                       arbAas              +1
043900050914     o                       arbLnp              +1
044000050914     o                       arbNrs        z     +1
044100050914     o                       arbNsp        z     +1
044200050914     o                                           +2 'Foglio'
044300050914     o                       arbnfv              +1
044400050914     o          e            Riga           1
044500050914     o                                           +1 'Errore'
044600050914     o                       d53msg              +1
044700050914     o          e            Totale         2
044800050914     o                                           +1 'Totale dei rcd aggiornati:'
044900050914     o                       wtot          4     +1
045000050914** CMD   (Lunga 80)                                                            *
045100050914OVRPRTF QSYSPRT SAVE(*YES)                                                       1
