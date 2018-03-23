000100040426      *--------------------------------------------------------------*
000200040426      *               RISTAMPA AVVISO DANNO                          *
000300040426      *--------------------------------------------------------------*
000400981016      *
000500981014      ****************************************************************
000600981014      *  FASI C.A. FISSATE A PROGRAMMA
000700981014      ****************************************************************
000800990429      *
000900990429      *    660 = Invio Documenti alla Consuldanni              (WCONS)
001000000303      *    545 = Stampa avviso danno                           (WAVVD)
001001081029      *    190 = Stampa avviso danno LT                        (WADLT)
001100990429      *
001200990311      ****************************************************************
001300990311      *  CAUSALI FISSATE A PROGRAMMA
001400990311      ****************************************************************
001500990429      *
001600040426      ****************************************************************
001700981013      *
001800980521      ****************************************************************
001900980521      *  RIEPILOGO INDICATORI
002000040426      ****************************************************************
002100060629      * 06 - SONO IN FILIALE
002200980923      * 28 - ERRORE GENERICO DSPF
002300980904      * 30 - COMODO
002400980904      * 31 - COMODO
002500981016      * 32 - COMODO
002600000303      * 33 - COMODO
002700040426      * 39 - Condiziona richiesta parzializzazione per data C.A.
002800980904      * 40 - ERRORE p.o. in gestione
002900981013      * 41 - ERRORE numero C.A.
003000981013      * 42 - ERRORE causale di chiusura
003001081029      * 43 - ERRORE manca il tipo avviso danno
003100040426      ****************************************************************
003200980623
003300980521     H DECEDIT('0,') DATEDIT(*DMY.)
003400980623
003500990429     fFIDN33D   CF   E             WORKSTN
003600990430     fFNDCT01L  IF   E           K DISK
003700000303     fFNDCF01L  IF   E           K DISK
003800980623
003900040426      *--------------------------------------------------------------*
004000040426
004100040426      *   S C H I E R E
004200040426
004300081029     D MSG             S             78    DIM(6) CTDATA PERRCD(1)              MSG VIDEO
004400060629     d cmdt            s             60    dim(04) ctdata perrcd(1)
004500040426
004600040426      *   V A R I A B I L I
004700040426
004800981013     D Kaac            S                   LIKE(DCTaac)
004900981013     D Kfil            S                   LIKE(DCTfil)
005000981013     D Knca            S                   LIKE(DCTnca)
005100990429     d Wcons           s                   like(dctfca)
005200990429     d                                     inz(660)
005300000303     d Wavvd           s                   like(dctfca)
005400000303     d                                     inz(545)
005401081029     d Wadlt           s                   like(dctfca)
005402081029     d                                     inz(190)
005500000303     d avvisato        s              1
005600040426     d $X_Data         s              1    inz(*off)
005700060629
005800060629     d wlib            s             10
005900060629     d comman          s             80
006000060629     d lenght          s             15  5 inz(80)
006100980923
006200980923     D WORA            S              4  0
006300980721     D W0140           S             14  0
006400980521     D Wdtgio          S              8  0
006500980521     D dateu           S              8  0
006600040426     D wdataca         S              8  0 inz
006700040428     d W1Cdad          s              8  0 inz
006800980923
006900040426      *   C O S T A N T I
007000040426
007100980622      *  titolo videata (lunghezza massima 34)
007200990429     D TITOLO          C                   CONST('     RISTAMPA AVVISO DANNO   -
007300981013     D                                          ')
007400980923      *
007500980923     D WLBDAT          DS                  INZ
007600980923     D  G02DAT                 1      8  0
007700980923     D  G02INV                 9     16  0
007800980923     D  G02ERR                17     17
007900980923     D  G02TGI                18     22  0
008000040426      *
008100040426     d dsnumca         ds
008200040426     d  aac                    1      4  0
008300040426     d  fil                    5      7  0
008400040426     d  nca                    8     14  0
008500980915      *----------------------------------
008600980529     D UT§DSE0F      E DS
008700981013     D CNCR80        E DS
008800981016     D Fidn00DS      E DS
008900981014     D DSDN10        E DS                  EXTNAME(FIDN10DS)
009000980521      *
009100980521     D KPJBA         E DS
009200980521      *
009300980521     D                SDS
009400980521     D  VTCPGM                 1     10
009500980623
009600040426     c*--------------------------------------------------------------*
009700990122     c*
009800990122     c* GESTIONE CHIUSURA RIAPERTURA A VIDEO
009900990122     c*
010000981013     C                   do        *hival
010100980623
010200980521      * Emissione VIDEO1
010300990429     c                   write     FI33t01
010400990429     c                   write     FI33z01
010500990429     c                   exfmt     FI33d01
010600980923
010700980923     c                   setoff                                       284041
010800990419     c                   setoff                                       422190
010801081029     c                   setoff                                       43
010900980923     c                   clear                   v1cmsg
011000980623
011100990429      * f03=Fine
011200990429     c   kc              leave
011300980623
011400981014      * Controlli  videata
011500980521     c                   exsr      ctrd01
011600990429      * se è acceso il  28 ritorno ad emettere il formato video
011700990429     c                   if        *in28
011800981105     c                   iter
011900981105     c                   endif
012000981015      * f10=Note C.A.
012100990429     C                   IF        *INKJ
012200981015     c                   exsr      SUB_F10
012300981015     C                   iter
012400981015     c                   ENDIF
012500981015
012600981015      * f14=Interrogazione C.A.
012700990429     C                   IF        *INKN
012800981015     c                   exsr      SUB_F14
012900981015     C                   iter
013000981015     c                   ENDIF
013100990429      *
013200981014      * Se non ho confermato o ci sono errori riemetto video
013300990429     c                   IF        *INKF
013400980910     c                   exsr      SUB_confer
013500981015     c                   leave
013600980908     c                   ELSE
013700981013     c                   iter
013800980908     c                   ENDIF
013900981014      *
014000981014     c                   enddo
014100980623
014200981021     c                   clear                   dsdn10
014300981021     c                   movel     'C'           i10tla
014400981021     c                   movel     dsdn10        kpjbu
014500981021     c                   call      'FIDN10R'
014600981021     c                   parm                    kpjba
014700981021      *
014800990122      *
014900990122     c*
015000990122     c     fine          tag
015100981021     c*
015200060629     c                   If        simfel <> *zeros
015300060629     c                   Clear                   comman
015400060629     c                   Movel(p)  cmdt(4)       comman
015500060629     c                   Call      'QCMDEXC'                            99
015600060629     c                   Parm                    comman
015700060629     c                   Parm                    lenght
015800060629     c                   endif
015900060629
016000980907     c                   EVAL      *INLR = *ON
016100040426      ****************************************************************
016200040426      * CONTROLLI VIDEO 1
016300040426      ****************************************************************
016400980923     C     CTRD01        BEGSR
016500040426
016600040426      * Controllo se chiesta la ristampa di tutte le C.A. di una data:
016700040426      * ee richieste tutte le C.A. di una data non controllo la singola
016800040426     c                   reset                   $X_Data
016900040426if  1c                   if            *in39
017000040428     c                             and V1Cdad <> *zeros
017100040426     c                             and V1Cfil =  *zeros
017200040426     c                             and V1Cnca =  *zeros
017300040426     c                   eval      $X_Data = *on
017400040426     c                   goto      X_Data
017500040426     c                   endif
017600980904
017700981013      * Se previsto controllo P.O. C.A.
017800981014    1C                   IF        V1CFIL = *zeros
017900981013     C                   movel     MSG(2)        V1Cmsg
018000981012     C                   seton                                        2840
018100990429     c                   goto      Endct1
018200990429    1C                   ENDIF
018300981012
018400981013      * Controllo Anno C.A.
018500981014    1C                   IF        V1Caac = *zeros
018600981013     C                   movel     MSG(2)        V1Cmsg
018700980923     C                   seton                                        2841
018800990429     c                   goto      Endct1
018900990429    1C                   Endif
019000980923      *   sistemo anno di due cifre
019100990429     C                   if        V1Caac < 100  and  V1Caac > 60
019200981013     C                   ADD       1900          V1Caac
019300990429     C                   endif
019400990429     C                   if        V1Caac < 100  and  V1Caac <= 60
019500981014     C                   ADD       2000          V1Caac
019600990429     C                   endif
019700980923
019800981013      * Controllo N° C.A.
019900981014    1C                   IF        V1Cnca = *zeros
020000981013     C                   movel     MSG(2)        V1Cmsg
020100980923     C                   seton                                        2841
020200990429     c                   goto      Endct1
020300981014   x1C                   ELSE
020400980923      *
020500981014     c* valorizzo la chiave CA
020600981014     C                   z-add     V1Cfil        Kfil
020700981014     C                   z-add     V1Caac        Kaac
020800981014     C                   z-add     V1Cnca        Knca
020900981014     C     Keynca        CHAIN     FNDCT000                           31
021000981230      *
021100980904      *
021200981014    2C                   IF        *in31 = *on
021300980923     C                   movel     MSG(1)        V1Cmsg
021400980923     C                   seton                                        2841
021500990429     c                   goto      endct1
021600990429    2c                   endif
021700990429     c* se esiste la CA ma è annullata non ristampo
021800990429     c*
021900990429     c                   if        not *in31 and  dctatb <> ' '
022000990429     c*
022100990429     C                   movel     MSG(3)        V1Cmsg
022200990429     C                   seton                                        2841
022300990429     c                   goto      endct1
022400990429     c                   endif
022500000303     c* se esiste controllo che sia stato fatto almeno una volta la stampa Avviso Danno
022600000303     c                   exsr      ctr_dcf
022700000303     c* se avvisato è uguale a ' '  errore oppure è uguale a 'S' e non c'è numero pratica
022800981016     c*
022900000303     c*
023000000303     c                   if        avvisato = ' '   or
023100081030     c                             (avvisato <> ' ' and dctprn = 0 and
023101081030     c                             dctfpr = 'P')
023200000303     c*
023300000303     C                   movel     MSG(4)        V1Cmsg
023400000303     C                   seton                                        2841
023500000303     c                   endif
023600000303     c*
023700990429     c                   endif
023800040426
023900040426     c     X_Data        tag
024000040426      *
024100040426      * Controlli per le selzioni dei soli utenti EDP*:
024200040426     c     *in39         cabeq     *off          EndCt1
024300040426      *
024400040428      * - Data Avviso Danno corretta
024500040428if  1c                   if        V1Cdad <> *zeros
024600040426     c                   clear                   WLBdat
024700040428     c                   z-add     V1Cdad        G02dat
024800040426     c                   call      'XSRDA8'
024900040426     c                   parm                    WLBdat
025000040426if  2c                   if        G02err = *off
025100040428     c                   z-add     G02dat        V1Cdad
025200040428     c                   z-add     G02inv        W1Cdad
025300040426x   2c                   else
025400040426     c                   seton                                        28  42
025500040426     c                   movel     Msg(2)        V1Cmsg
025600040426     c                   goto      EndCt1
025700040426e   2c                   endif
025800040426if  2c                   if            V1Cfil <> *zeros
025900040428     c                             and V1Cdad <> *zeros
026000040426     c                   seton                                        28  42
026100040426     c                   movel     Msg(5)        V1Cmsg
026200040426     c                   goto      EndCt1
026300040426e   2c                   endif
026301081029      * deve essere scelto il tipo gestione (pratica liquidazione transattiva)
026302081029     c                   if        v1ctav = ' '
026303081029     c                   seton                                        28  43
026304081029     c                   movel     Msg(6)        V1Cmsg
026305081029     c                   goto      EndCt1
026306081029e   2c                   endif
026400040426e   1c                   endif
026500980923
026600990429     c     Endct1        ENDSR
026700981014      ****************************************************************
026800981014      * F10 = GESTIONE NOTE
026900981014      ****************************************************************
027000981014     C     SUB_F10       BEGSR
027100981014      *
027200981015     c*
027300981015     c                   eval      aac = kaac
027400981015     c                   eval      fil = kfil
027500981015     c                   eval      nca = knca
027600981015     c*
027700981014     C                   clear                   DSDN10
027800981015      * calcolo data apertura C.A.
027900981015     C                   eval      wdataca = (DCTaac * 10000) + DCTmgc
028000981015     c*
028100981015     C                   CLEAR                   WLBDAT
028200981015     C                   Z-ADD     wdataca       G02inv
028300981015     C                   movel     '3'           G02err
028400981015     C                   CALL      'XSRDA8'
028500981015     C                   PARM                    WLBDAT
028600981015     c                   z-add     g02dat        wdataca
028700981015     c*
028800981015     c                   eval      i10dta = wdataca
028900981014     c                   movel     'N'           i10trc
029000981014     c                   movel     'C'           i10tpd
029100981014      *
029200981015     c                   movel     'M'           i10flm
029300990429     c                   movel     dctfca        i10nks
029400981014     c                   movel     dsnumca       i10nkt
029500981014      *
029600981014     c                   movel(P)  dsdn10        kpjbu
029700981014     C                   CALL      'FIDN10R'
029800981014     C                   PARM                    kpjba
029900981014      *
030000981014     c                   ENDSR
030100981015      ****************************************************************
030200981015      * F14 = INTERROGAZIONE C.A.
030300981015      ****************************************************************
030400981015     C     SUB_F14       BEGSR
030500981016      *
030600981016     c                   clear                   FIDN00DS
030700981016      *
030800981016     c                   movel     'I'           I00mod
030900990429     c                   movel     dctfca        I00fca
031000981016     c                   z-add     dctgfc        I00fgs
031100981016     c                   z-add     dctaac        I00aac
031200981016     c                   z-add     dctfil        I00fil
031300981016     c                   z-add     dctnca        I00nca
031400981016     c                   z-add     dctaas        I00aas
031500981016     c                   z-add     dctlnp        I00lnp
031600981016     c                   z-add     dctnrs        I00nrs
031700981016     c                   z-add     dctnsp        I00nsp
031800981016     c                   z-add     dctlna        I00lna
031900981016     c                   movel     dcttad        I00tad
032000981016     c                   if        dctgfc = 046
032100981016     c                   movel     'S'           I00tpb
032200981016     c                   endif
032300981016      *
032400981016     c                   movel     FIDN00DS      kpjbu
032500981016      *
032600981016     c                   call      'FIDN01R'
032700981016     c                   parm                    kpjba
032800981016      *
032900981015     c                   endsr
033000040426
033100980608      *****************************************************************
033200081103      * Eseguo i richiami ai pgm di ristampa
033300980608      *****************************************************************
033400980923     C     SUB_confer    BEGSR
033500981007
033700990429     c                   clear                   FIDN00DS
033800990429      *
033900990429     c                   movel     'G'           I00mod
034000990429     c                   movel     999           I00fca
034100990429     c                   z-add     dctgfc        I00fgs
034200990429     c                   z-add     dctaac        I00aac
034300990429     c                   z-add     dctfil        I00fil
034400990429     c                   z-add     dctnca        I00nca
034500990429     c                   z-add     dctaas        I00aas
034600990429     c                   z-add     dctlnp        I00lnp
034700990429     c                   z-add     dctnrs        I00nrs
034800990429     c                   z-add     dctnsp        I00nsp
034900990429     c                   z-add     dctlna        I00lna
035000990429     c                   movel     dcttad        I00tad
035100990429     c                   movel     'S'           I00tpb
035200990430     c                   z-add     1             I00nce
035300990429      *
035400040428if  1c                   if        $X_Data = *off
035500040428      *
035502081029      *  Avviso danno Pratica Assicurativa
035503081030     c                   If        dctfpr = 'P'
035700990429      *
035701081029     c                   movel(p)  FIDN00DS      kpjbu
035800990429     c                   call      'FIDN35R'
035900990429     c                   parm                    kpjba
035901081029     c                   endif
036000990429      *
036001081029      *  Avviso danno Liquidazione Transattiva
036002081030     c                   If        dctfpr = 'T'
036007081030     c                   call      'FIDN66R'
036008081029     c                   parm                    kpjba
036009081029     c                   parm                    fidn00ds
036010081029     c                   endif
036011081029      *
036100981015      * note fase ca
036200981015     C                   CLEAR                   DSDN10
036300981015     c                   z-add     Wdataca       i10dta
036400981015     C                   movel     'C'           I10FLM
036500981015     c                   movel(P)  dsnumca       i10nkt
036600990429     c                   movel     dctfca        i10nks
036700981015     c                   movel     'N'           i10trc
036800981015     c                   movel(P)  dsdn10        kpjbu
036900981015     C                   CALL      'FIDN10R'
037000981015     C                   PARM                    kpjba
037100040426      *
037200040426x   1c                   else
037300040426      *
037400040428      * (Ri)stampa avvisi danno di quella data (I00DSB)
037500050324if  1c                   if        V1Cnis = *blanks
037600050324     c                   movel     '*'           I00tpb
037700050324e   1c                   endif
037701081103      * Se tipo ristampa è relativa alla Liquidazione Transattiva imposto il tipo proseguimento
037702081103      * CA  uguale a "T" altrimenti uguale a "P"
037703081103      *
037705081103     c                   eval      i00fpr = v1ctav
037800040428     c                   movel     998           I00fca
037900040428     c                   z-add     W1Cdad        I00dsb
038000040428     c                   movel(p)  FIDN00ds      KPJBU
038100040427     c                   call      'FIDN34R'
038200040426     c                   parm                    KPJBA
038300040426      *
038400040426e   1c                   endif
038500981009      *
038600980923     c                   ENDSR
038700000303      *****************************************************************
038800000303      * Controllo fasi ca
038900000303      *****************************************************************
039000000303     C     CTR_DCF       BEGSR
039100000303      *
039200000303      * verifico se è stata eseguita la fase 545
039300000303     c                   clear                   avvisato
039400000303     c     keynca        setll     FNDCF01l
039500000303     c*
039600000303     c                   DO        *hival
039700000303      *
039800000303     c     keynca        reade     fndcf01l                               33
039900000303     c                   if        *in33
040000000303     c                   leave
040100000303     c                   endif
040200000303      *
040201081029      * pratica assicurativa
040300081029     c                   if        (dcffca = Wavvd  and dcfatb = ' '
040301081029     c                             and dctfpr = 'P') or
040302081029      * liquidazione transattiva
040303081029     c                             (dcffca = Wadlt  and dcfatb = ' '
040304081029     c                             and dctfpr = 'T')
040400000303     c                   eval      avvisato = 'S'
040500000303     c                   leave
040600000303     c                   endif
040700000303      *
040800000303     c  n33              enddo
040900000303      *
041000000303     c                   ENDSR
041100000303      *
041200980910      *****************************************************************
041300980527      * ROUTINE INIZIALE
041400980527      *****************************************************************
041500980527     C     *INZSR        BEGSR
041600980615      *
041700980521     C     *ENTRY        PLIST
041800980521     C                   PARM                    KPJBA
041900980907      *
042000980907     C                   Z-ADD     1             CODUT
042100980907     C                   CALL      'X§PARUT'
042200980907     C                   PARM                    UT§DSE0F
042300980907     C                   MOVEL     RAGUT         RSUT
042400980907     C                   MOVEL     REC80         CNCR80
042500040426
042600040426     c                   if        %subst(KNMUS:1:3) = 'EDP'
042700040426     c                   eval      *in39 = *on
042800040426     c                   endif
042900060629      * verifico se sono in filiale faccio le ovrdbf per i file di sede
043000060629     c                   If        simfel <> *zeros
043100060629      * accendo indicatore
043200060629     c                   seton                                        06
043300060629      * librerie
043400060629
043500060629      * Se sono in ambiente buono - GAITRAGRU
043600060629     c                   If        knsif = 'FILTRA201 '
043700060629     c                   Eval      wlib = 'GAITRAGRU '
043800060629      *  Se sono in ambiente di prova - GAITRAGRPS
043900060629     c                   Else
044000060629     c                   Eval      wlib = 'GAITRAGRPS'
044100060629     c                   EndIf
044200060629      * TITAS30C
044300060629     c                   Clear                   comman
044400060629     c                   Movel(p)  cmdt(1)       comman
044500060629     c                   Eval      %Subst(comman:30:10) = wlib
044600060629     c                   Eval      comman =
044700060629     c                             %trim(comman) + '/TITAS30C)'
044800060629     c                   Call      'QCMDEXC'                            99
044900060629     c                   Parm                    comman
045000060629     c                   Parm                    lenght
045100060629      * TITA430C
045200060629     c                   Clear                   comman
045300060629     c                   Movel(p)  cmdt(2)       comman
045400060629     c                   Eval      %Subst(comman:30:10) = wlib
045500060629     c                   Eval      comman =
045600060629     c                             %trim(comman) + '/TITA430C)'
045700060629     c                   Call      'QCMDEXC'                            99
045800060629     c                   Parm                    comman
045900060629     c                   Parm                    lenght
046000060629      * TNTMD01L
046100060629     c                   Clear                   comman
046200060629     c                   Movel(p)  cmdt(3)       comman
046300060629     c                   Eval      %Subst(comman:30:10) = wlib
046400060629     c                   Eval      comman =
046500060629     c                             %trim(comman) + '/TNTMD01L)'
046600060629     c                   Call      'QCMDEXC'                            99
046700060629     c                   Parm                    comman
046800060629     c                   Parm                    lenght
046900980907
047000060629     c                   endif
047100981007      *
047200981007      * Imposto il TITOLO
047300990429      *
047400990429     c                   movel     TITOLO        VTCtit
047500980721      * reperisco data e ora
047600980721     C                   TIME                    W0140
047700980521      * UDATE IN GGMMAAAA
047800980521     C                   MOVE      W0140         WDTGIO
047900980923     C                   MOVEL     W0140         WORA
048000980521      * UDATE IN AAAAMMGG
048100980521     C                   Z-ADD     WDTGIO        G02DAT
048200980521     C                   MOVEL     *BLANK        G02ERR
048300980521     C                   CALL      'XSRDA8'
048400980521     C                   PARM                    WLBDAT
048500980521     C                   MOVEL     G02INV        DATEU
048600980923
048700981013      *    Imposto anno C.A.
048800981013     c                   move      wdtgio        V1Caac
048900980721
049000981014     C     Keynca        klist
049100981013     C                   KFLD                    kaac
049200981013     C                   KFLD                    kfil
049300981013     C                   KFLD                    knca
049400980915
049500980521     C                   ENDSR
049600040426      *--------------------------------------------------------------*
049700040426** - MSG  Lungh. 78                                                            *
049800990429Il numero C.A. immesso non esiste                                              1
049900981014Il valore immesso nel campo non è valido                                       2
050000990429La C.A. è annullata                                                            3
050100000303Impossibile RISTAMPARE Avviso Danno in quanto non è mai stato stampato         4
050200040426Impossibile selezionare contemporaneamente per singola C.A. e per data         5
050201081029In caso di RISTAMPA Avviso Danno selezionare Pratica o Liquidazione            6
050300060629** cmdt
050400060629OVRDBF FILE(TITAS30C) TOFILE(
050500060629OVRDBF FILE(TITA430C) TOFILE(
050600060629OVRDBF FILE(TNTMD01L) TOFILE(
050700060629DLTOVR FILE(*ALL)
