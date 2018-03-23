000100050707     H DECEDIT('0.') DATEDIT(*YMD.)
000200060814     H* Fieu51R *----------------------------------------------------*
000300900418     H*                                                              *
000400060814     H* Scrittura del fidposnd(CRDATA01)                             *
000500050927     h* record in stato 50/30 invio a dpd ORM                        *
000600050927      **?__________________________________________________________________ */
000700050707     Ffidpo02l  uf   E           k DISK    commit
000800050812     Ffnorm01l  if   E           k DISK
000900060814     Ftabel00f  if   E           k DISK
001000060814
001100080424     ftivgd00f  if a E             DISK    commit
001200080403     D*** dpoRecord       S                   LIKE(vgddta)
001300080403     D dpoRecord       S           2048
001400060814     d trul47ds      e ds
001500060814     D W0140           S             14  0 inz
001600060814     d ds15          e ds
001700171205     d                 ds
001800171205     d d5_lnpds                     255
001900171205     d d5_lnp                         3    dim(85) overlay(d5_lnpds)
002000080925      *
002100080925     d DSCAP           ds
002200080925     d  CAPinSK                1      9
002300080925     D SkCAP                   1      9    dim(9)
002400060814
002500900418     D KPJBA         E DS
002600050705     D trul33ds      E DS
002700080505     d**** TISID3ds      e ds                  inz
002800080505     d TISIE3ds      e ds                  inz
002900171205     D TISID4DS      E DS                  INZ
003000171205     D* DS PER TIBS02R - GESTIONE TNTBE00F
003100171205     D DSBS02        E DS                  EXTNAME(TIBS02DS)
003200080505     d ds3idp        e ds
003300050704
003400050705     D KPJBus          s                   like(kpjbu)
003500050705     D progr           s              7s 0
003600050705     D primo           s              1
003700060920     D status          s                   like(dposts)
003800050711     D dpodepw         s                   like(dpodep)
003900080520     D Cod_ordinante   s                   like(ormcor)
004000171205     D NEW_CAPDPD      s              1    inz('N')
004100050706     D ident           s              1
004200090715     D Info1           s             30
004300090715     D Info2           s             30
004400050927      **?__________________________________________________________________ */
004500050704      *    main
004600050927      **?__________________________________________________________________ */
004700060920     c                   z-add     10            status
004800060920
004900050705     c     rielabora     tag
005000050705
005100050705     c     status        setll     fidpo02l
005200050705     c                   do        *hival
005300050705     c     status        reade     fidpo02l
005400050705
005500050705     c                   if        %eof(fidpo02l)
005600050705     c                   leave
005700050705     c                   endif
005800050705
005900050705     c     korm          chain     fnorm01l
006000050705     c                   if        %found(fnorm01l)
006100060920
006200060920      ** ORM chiuso
006300060920     c                   if        ormFAO =999
006400060920     c                   move      'A'           dpoANN
006500080513     c*******            else
006600080513     c                   end
006700060920      *
006800060920      * operazioni da eseguire una sola volta reperimento numero di
006900060920      * trasmissione e intestazione del file da inviare
007000060920     c                   if        primo = *blank
007100060920     c                   exsr      file_dpdt
007200060920     c                   move      'X'           primo
007300060920     c                   endif
007400060920
007500050705     c                   exsr      file_dpdD
007600060920
007700050705     c                   z-add     progr         DPONTR
007800080513     c********           end
007900060920
008000060920     c                   move      *date         DPODTR
008100050704     c                   update    fidpo000                             99
008200060814
008300050705      * non trovato lascio il rekord spia intatto e ne leggo un'altro
008400050705     c                   else
008500050705     c                   iter
008600050705     c                   endif
008700050705
008800050705     c                   enddo
008900050927      **?controllo sincronia per loop a status 50 +                         */
009000050927      * chiamata controllo rekord trasmessi ma con orm non eseguiti
009100050927      * i rekord con orm in fase 999 vengono riaggiornati per trasmettere
009200050927      * l'informazione che l'orm non è stato effettuato
009300060920     c                   if        status = 10
009400050927     c                   commit
009500050927
009600050927     c                   call      'FIEU52R'
009700050927     c                   parm                    kpjba
009800050927
009900060920      * rilancio l'elaborazione con status a 19
010000060920     c                   z-add     19            status
010100050706     c                   move      '3'           ident
010200050705     c                   goto      rielabora
010300050705     c                   endif
010400060920
010500050705      * chiusura file da inviare
010600050707     c                   if        primo <> *blank
010700050707     c                   exsr      file_dpdP
010800050707     c                   commit
010900060814
011000060814     C* Infine elimino il blocco elaborazione TIVGD
011100060814     C                   clear                   trul47ds
011200060814     C                   eval      d47opz  = 'F'
011300060814     C                   eval      d47tip  = '$8'
011400060814     C                   call      'TRUL47R'
011500060814     C                   parm                    trul47ds
011600050707     c                   endif
011700050707      *
011800050705     c                   seton                                        lr
011900050927      **?__________________________________________________________________ */
012000050705     c     File_dpdT     begsr
012100050927      **?__________________________________________________________________ */
012200060814
012300060814      *  istruzioni apertura blocco scrittura TIVGD
012400060814     C                   clear                   trul47ds
012500060814     C                   eval      d47opz  = 'I'
012600060814     C                   eval      d47tip  = '$8'
012700060814     C                   eval      d47lck  = 'N'
012800060814     C                   eval      d47chkj = 'N'
012900060814     C                   eval      d47pgm  = 'FIEU51R'
013000060814     C                   call      'TRUL47R'
013100060814     C                   parm                    trul47ds
013200060814
013300060814      * preleva il numero Parcel
013400050705     c                   exsr      numero
013500050705      * testata depot
013600050705     c                   eval      DPorecord =
013700060814     c                             '# FILE'+ ';' + 'DEPOT' +
013800060814     c                             %trim(dpodep) + ';' +
013900060814     c                             %trim(dpodep) + ';' +
014000050705     c                             %trim(%editc(*date:'Z')) + ';' +
014100050705     c                             %trim(%editc(oratr:'Z')) + ';' +
014200050705     c                             %trim(%editc(progr:'Z'))+ ';'
014300060814     C                   exsr      tivgd
014400050705
014500060814
014600060814      * testata depot
014700060814     c                   eval      DPorecord =
014800060814     c                             '# VERSION;0;'
014900060814     C                   exsr      tivgd
015000060814
015100050705      * testata depot
015200050705     c                   eval      DPorecord =
015300050707     c                             '# DEF;CR:CRDATA01;ODEPOT;ORDERNR;CDEPOT;OCU-
015400050707     c                             STNO;ONAME;ONAME2;OSTREET;OCOUNTRY;OPOSTAL;-
015500050705     c                             OCITY;OPHONE;OEMAIL;CNAME;CNAME2;CNAME3;CN-
015600050705     c                             AME4;CSTREET;CCOUNTRY;CPOSTAL;CCITY;CPHONE-
015700050705     c                             ;CEMAIL;RNAME;RNAME2;RSTREET;RCOUNTRY;RPOS-
015800050705     c                             TAL;RCITY;RPHONE;REMAIL;RDEPOT;PKNR;STATUS-
015900060814     c                             ;IDENT;CUSTREF;WEBNR;NEUDAT;AENDDAT;SERVICE-
016000060814     c                             ;CUSTOM;INFO1;INFO2;WEIGHT;'
016100090617      *
016200090617     c                   if        udtymd > 20090809
016300090617     c                   eval      DPorecord = %Trim(DPorecord) +
016400090617     c                             'CONFIRM;PICKUPDATE;CONTENT;DELISID;-
016500090617     c                             CANCELUSER;;'
016600090617     c                   end
016700090617      *
016800060814     C                   exsr      tivgd
016900050705
017000050704     c                   endsr
017100050927      **?__________________________________________________________________ */
017200050705     c     File_dpdD     begsr
017300050927      **?__________________________________________________________________ */
017400050705      * dati orm
017500050711      * forzatura nazione italia se non specificato sull'orm
017600050711     c                   if        ormnao= *blank
017700050711     c                   movel     'IT'          ormnao
017800050711     c                   endif
017900050711     c                   if        ormnar= *blank
018000050711     c                   movel     'IT'          ormnar
018100050711     c                   endif
018200050711     c                   if        ormnac= *blank
018300050711     c                   movel     'IT'          ormnac
018400050711     c                   endif
018500080925      **
018600080925      * CAP "EIRE" x l'Irlanda "DPD" chiede di mettere "2" al posto di "EIRE"
018700080925     c                   if        ormcao= 'EIRE' or ormnao='IRL'
018800080925     c                   eval      ormcao = '2'
018900080925     c                   endif
019000080925     c                   if        ormcar= 'EIRE' or ormnar='IRL'
019100080925     c                   eval      ormcar = '2'
019200080925     c                   endif
019300080925     c                   if        ormcac= 'EIRE' or ormnac='IRL'
019400080925     c                   eval      ormcac = '2'
019500080925     c                   endif
019600080925      **
019700080925      * CAP Inglesi "DPD" chiede di non avere alcuno spazio inmezzo alle lettere del CAP
019800080925      **    compatta i CAP x l'Inghilterra
019900080925     c                   if         ormnao='GB'
020000080925     c                   eval       CAPinSK = ormcao
020100080925     c                   exsr      Compatta_CAP
020200080925     c                   eval       ormcao = CAPinSK
020300080925     c                   endif
020400080925      *
020500080925     c                   if         ormnar='GB'
020600080925     c                   eval       CAPinSK = ormcar
020700080925     c                   exsr      Compatta_CAP
020800080925     c                   eval       ormcar = CAPinSK
020900080925     c                   endif
021000080925      *
021100080925     c                   if         ormnac='GB'
021200080925     c                   eval       CAPinSK = ormcac
021300080925     c                   exsr      Compatta_CAP
021400080925     c                   eval       ormcac = CAPinSK
021500080925     c                   endif
021600080925      **
021700171205      ** verifica quale instradamento CAPPARIO DPD deve utilizzare
021800171205      **    se andare con il vecchio oppure con il nuovo 2017
021900171205     c                   exsr      Vers_Instradam
022000050711
022100050711      *reperisce codice depot ritiro da cappario dpd
022200050711     c                   exsr      cappariodpd
022300060814
022400060814      * Nazioni formato codice ISO
022500060814     c                   movel(p)  ormNAO        TBLKEY
022600060814     c                   clear                   c15NAO            3 0
022700080505     c                   movel     '15'          TBLCOD
022800060814     c     ktab          chain     tabel00f
022900060814     c                   if        %Found(tabel00f)
023000060814     c                   movel     tbluni        ds15
023100060814     c                   z-add     §15CIE        c15NAO            3 0
023200080514     c                   end
023300080514      * Italia non è codificata
023400080514     c                   if        c15Nao = 0
023500080514     c                   z-add     380           c15Nao
023600080514     c                   end
023700050711
023800060814     c                   movel(p)  ormNAR        TBLKEY
023900060814     c                   clear                   c15NAR            3 0
024000060814     c     ktab          chain     tabel00f
024100060814     c                   if        %Found(tabel00f)
024200060814     c                   movel     tbluni        ds15
024300060814     c                   z-add     §15CIE        c15NAR            3 0
024400060814     c                   end
024500080514      * Italia non è codificata
024600080514     c                   if        c15Nar = 0
024700080514     c                   z-add     380           c15Nar
024800080514     c                   end
024900060814
025000060814     c                   movel(p)  ormNAC        TBLKEY
025100060814     c                   clear                   c15NAC            3 0
025200060814     c     ktab          chain     tabel00f
025300060814     c                   if        %Found(tabel00f)
025400060814     c                   movel     tbluni        ds15
025500060814     c                   z-add     §15CIE        c15NAC            3 0
025600060814     c                   end
025700080514      * Italia non è codificata
025800080514     c                   if        c15Nac = 0
025900080514     c                   z-add     380           c15Nac
026000080514     c                   end
026100080925      **
026200080520      ** il codice del cliente Ordinante x DPD non può essere = (0)
026300080520     c                   clear                   Cod_ordinante
026400080520     c                   if        ormcor > 0
026500080520     c                   eval       Cod_ordinante = ormcor
026600080520     c                   else
026700080520      ** altrimenti metto il numero dell'ordine DPD
026800080520     c                   eval       Cod_ordinante = dpoord
026900080520     c                   end
027000060814
027100060814      * I CAMPI TRACCIATI SONO ALLA SINISTRA SONO SEGNATI I NUMERI
027200050711      * DEI CAMPI IN MODO DA POTER RICONDURRE ALLA COMPOSIZIONE DEL
027300050711      * MESSAGGIO
027400110111     c     ';':' '       xlate     ORMrso        ORMrso
027500110111     c     ';':' '       xlate     ORMino        ORMino
027600110111     c     ';':' '       xlate     ORMcao        ORMcao
027700110111     c     ';':' '       xlate     ORMloo        ORMloo
027800110111     c     ';':' '       xlate     ORMpro        ORMpro
027900110111     c     ';':' '       xlate     ORMnao        ORMnao
028000110111      *
028100110111     c     ';':' '       xlate     ORMrsr        ORMrsr
028200110111     c     ';':' '       xlate     ORMinr        ORMinr
028300110111     c     ';':' '       xlate     ORMlor        ORMlor
028400110111     c     ';':' '       xlate     ORMcar        ORMcar
028500110111     c     ';':' '       xlate     ORMprr        ORMprr
028600110111     c     ';':' '       xlate     ORMnar        ORMnar
028700110111      *
028800110111     c     ';':' '       xlate     ORMrer        ORMrer
028900110111     c     ';':' '       xlate     ORMter        ORMter
029000110111      *
029100110111     c     ';':' '       xlate     ORMrsc        ORMrsc
029200110111     c     ';':' '       xlate     ORMinc        ORMinc
029300110111     c     ';':' '       xlate     ORMcac        ORMcac
029400110111     c     ';':' '       xlate     ORMloc        ORMloc
029500110111     c     ';':' '       xlate     ORMprc        ORMprc
029600110111     c     ';':' '       xlate     ORMnac        ORMnac
029700110111      *
029800110111     c     ';':' '       xlate     ORMnam        ORMnam
029900110111     c     ';':' '       xlate     ORMno1        ORMno1
030000110111     c     ';':' '       xlate     ORMno2        ORMno2
030100110111     c     ';':' '       xlate     ORMzor        ORMzor
030200110111     c     ';':' '       xlate     ORMrfa        ORMrfa
030300110111     c     ';':' '       xlate     ORMflo        ORMflo
030400081202
030500090515      *
030600090515      * Se manca l'anagrafica dell'ordinante imposto quella del Mittente
030700090515      *  Altrimenti DPD non esegue l'ORM (C.Burke maggio 2009)
030800090515      * Poichè il nostro sistema invece permette di non avere l'ordinante
030900090515      *  così si è aggirato l'ostacolo.
031000090515     c                   if        ormrso=*blank and ormino=*blank and
031100090515     c                             ormcao=*blank and ormloo=*blank
031200090515     c                   eval      ormrso = ormrsc
031300090515     c                   eval      ormino = orminc
031400090515     c                   eval      ormnao = ormnac
031500090515     c                   eval      ormcao = ormcac
031600090515     c                   eval      ormloo = ormloc
031700090515     c                   end
031800090715      *
031900090715     c                   eval      Info1 = ormNo1
032000090715     c                   eval      Info2 = ormNo2
032100090715      * aggiunge il riferimento (persona/telefono x il ritiro nelle note se c'è spazio
032200090715     c                   If        Info1 = *blank and Info2 = *blank
032300090715     c                   eval      Info1 = ormRER
032400090715     c                   eval      Info2 = ormTER
032500090715     c                   elseIf    Info1 <> *blank and Info2 = *blank
032600090715     c                   eval      Info2 = ormRER
032700090715     c                   eval      Info2 = %Trim(Info2) + ':' + %trim(ormRER)
032800090715     c                             + ':' + %trim(ormTER)
032900090715     c                   endIf
033000090715      *
033100061023     c                   move      dpoord        dpoordA           6
033200060814 1   c                   eval      DPorecord = %trim(dpodep) +';'+
033300061023 2   c                             %trim(dpoordA) +';'+
033400060814 3   c                             %trim(dpodepw) +';'+
033500080520 4   c**********                   %trim(%editc(ormcor:'X')) +';'+
033600080520 4   c                             %trim(%editc(Cod_ordinante:'X')) +';'+
033700060814 5   c                             %trim(ormrso) +';;'+
033800060814 7   c                             %trim(ormino) +';'+
033900060814 8   c                             %trim(%editc(c15NAO:'Z')) +';'+
034000060814 9/10c                             %trim(ormcao) +';'+ %trim(ormloo) +';;;'+
034100140318 13  c                             %trim(ormrsr) +';'+
034200140318      ** Aggiunto ormRER nel CNAME2 come riferimento di ritiro
034300140318 13  c                             %trim(ormrEr) +';;;'+
034400060814 17  c                             %trim(orminr) +';'+
034500060814 18  c                             %trim(%editc(c15NAR:'Z')) +';'+
03460006081419/20c                             %trim(ormcar) +';'+ %trim(ormlor) +';'+
034700060814 21  c                             %trim(ormter) +';;'+
034800060814 23  c                             %trim(ormrsc) +';;'+
034900060814 25  c                             %trim(orminc) +';'+
035000060814 26  c                             %trim(%editc(c15NAC:'Z')) +';'+
03510006081427/28c                             %trim(ormcac) +';'+ %trim(ormloc) +';;;'+
035200060814 31  c                             %trim(dpodep) +';;;'+
035300060814 34  c                             %trim(ident) +';;;;;;;'+
03540009071541/42c                             %trim(Info1) +';'+ %trim(Info2) +';'+
035500060814 43  c                             %trim(%editc(ormpkg:'2')) +';'
035600090617      *
035700090617     c                   if        udtymd > 20090809
035800100624      *  Data ritiro (PICKUPDATE) e Natura Merce (CONTENT)
035900100624     c                   move      ormDAR        data_ritiro       6
036000100624     c                   eval      DPorecord = %Trim(DPorecord) + ';' +
036100100624     c                             %trim(data_ritiro) + ';' +
036200100624     c                             %trim(ormNAM) + ';;;'
036300090617     c                   end
036400090617      *
036500060814     C                   exsr      tivgd
036600050705
036700050705     c                   endsr
036800080925      **?__________________________________________________________________ */
036900080925     c     Compatta_CAP  begsr
037000080925      **?__________________________________________________________________ */
037100080925     c                   z-add     0             to                3 0
037200080925     c                   do        9             from              3 0
037300080925     c                   if        skCAP(from) = *blank
037400080925     c                   if          to = 0
037500080925     c                   z-add     from          to
037600080925     c                   end
037700080925     c                   else
037800080925     c                   if          to > 0
037900080925     c                   move      skCAP(from)   skCAP(to)
038000080926     c                   move      *blank        skCAP(from)
038100080925     c                   add       1             to
038200080925     c                   end
038300080925     c                   end
038400080925     c                   enddo
038500080925
038600080925     c                   endsr
038700050927      **?__________________________________________________________________ */
038800050705     c     File_dpdP     begsr
038900050927      **?__________________________________________________________________ */
039000050705      * piede messaggio
039100050705     c                   eval      DPorecord =
039200050705     c                             '# END;' +
039300050705     c                             %trim(%editc(progr:'Z')) + ';'
039400060814     C                   exsr      tivgd
039500050705
039600050705     c                   endsr
039700050927      **?__________________________________________________________________ */
039800050705     c     numero        begsr
039900050927      **?__________________________________________________________________ */
040000050705     c                   eval      kpjbus=kpjbu
040100050705     c                   clear                   trul33ds
040200050705     c                   move      'L'           i33tla
040300050705     c                   z-add     305           i33cnu
040400050705     c                   z-add     1             i33num
040500050705     c                   movel     trul33ds      kpjbu
040600050705     c                   call      'TRUL33R'
040700050705     c                   PARM                    kpjba
040800050705     c                   movel     kpjbu         trul33ds
040900050705     c                   z-add     o33nri        progr
041000050705     c                   eval      kpjbu=kpjbus
041100050705
041200050705     c                   endsr
041300050927      **?__________________________________________________________________ */
041400050711     c     capparioDpd   begsr
041500050927      **?__________________________________________________________________ */
041600171205      *
041700171205      * Se deve usare il vecchio Cappario ancora per l'INSTRADAMENTO
041800171205     c                   if        NEW_CAPDPD ='N'
041900171205      *
042000080505      * Cerco da cappario DPD
042100080505      * quello nuovo, ora il depot è lungo 4 e non più 3
042200080505     c                   clear                   tisie3ds
042300080505     c                   eval      isie3dri = udtymd
042400080505     c                   eval      isie3dsp = ormdar
042500080505     c                   time                    isie3hsp
042600080505     c                   eval      isie3nzd = ormnar
042700080505     c                   eval      isie3cad = ormcar
042800080505     c                   if        ormpkg > §3ilmp
042900080505     c                   eval      isie3srv = 101
043000080505     c                   else
043100080505     c                   eval      isie3srv = 136
043200080505     c                   endif
043300080505     c                   eval      isie3lnp = ormpoe
043400080505     c                   call      'TISIE3R'
043500080505     c                   parm                    tisie3ds
043600080505if  2c                   if        osie3err = *blanks
043700080505     c                   move(p)   osie3ddep     dpodepw
043800080505e   2c                   endif
043900171205      *
044000171205      *  OPPURE con il nuovo CAPPARIO 2017
044100171205     c                   else
044200171205      **  nuovo cappario
044300171205     c                   clear                   tisid4ds
044400171205     c                   if        ormdar >0 and ormdar > udtymd
044500171205     c                   z-add     ormdar        isid4dri
044600171205     c                   else
044700171205     c                   z-add     udtymd        isid4dri
044800171205     c                   end
044900171205     C                   MOVEL     ormnar        isid4nzd
045000171205     C                   MOVEL     ormcar        Isid4cad
045100171205     c                   movel     ormlor        isid4lod
045200171205     c                   z-add     ormpoe        isid4lnp
045300171205     c                   if        ormpkg > §3ilmp
045400171205     c                   z-add     101           isid4sorc
045500171205     c                   else
045600171205     c                   z-add     136           isid4sorc
045700171205     c                   endif
045800171205     C                   CALL      'TISID4R'
045900171205     C                   PARM                    tisid4ds
046000171205if  2c                   if        OSID4ERR = *blanks
046100171205     c                   move(p)   osid4dstr     dpodepw
046200171205e   2c                   endif
046300171205      **
046400171205     c                   end
046500080505      *
046600080505     c                   endsr
046700050927      **?__________________________________________________________________ */
046800050512     c     *inzsr        begsr
046900050927      **?__________________________________________________________________ */
047000050512     c*
047100050705     c     *entry        plist
047200050705     C                   parm                    kpjba
047300060920     c*
047400050705     C                   TIME                    ORADAT           14 0
047500050705     C                   MOVEL     ORADAT        ORATR             6 0
047600060814     c                   move      *date         udtymd            8 0
047700060814     C                   TIME                    W0140
047800060814     C                   MOVE      W0140         W0080             8 0
047900050704
048000050706     c     korm          klist
048100050706     c                   kfld                    DPOPOE
048200050706     c                   kfld                    DPONRS
048300050706     c                   kfld                    DPONOR
048400050706     c                   kfld                    DPONRV
048500060814
048600060814     c     ktab          klist
048700060814     c                   kfld                    TBLKUT
048800060814     c                   kfld                    TBLCOD
048900060814     c                   kfld                    TBLKEY
049000060814     c                   z-add     1             TBLKUT
049100080505      * carico tabella DPD
049200080505     c                   movel     '3I'          tblcod
049300080505     c                   movel(p)  'DPD'         tblkey
049400080505     c     ktab          chain     tabel00f
049500080505     c                   clear                   ds3idp
049600080505     c                   if        %found(tabel00f) and tblflg = *blanks
049700080505     c                   eval      ds3idp = tbluni
049800080505     c                   endif
049900080505      *
050000060814
050100050512     c                   endsr
050200060814      *----------------------------------------------------*
050300060814     C     tivgd         Begsr
050400060814      *
050500060814     c                   clear                   tivgd000
050600080403     c                   eval      vgddta = %TrimR(dpoRecord)
050700060814     c                   eval      vgdtip = '$8'
050800060814     c                   eval      vgdksu = '0DPD0OUT'
050900060814     c                   eval      vgdtsc = 'WW'
051000060814     c                   eval      vgdpgm = 'FIEU51R'
051100061023     c                   eval      vgddat = udtymd
051200060814      *
051300060814     C                   WRITE     tivgd000
051400060814      *
051500060814     C                   Endsr
051600171205      *--------------------------------------------------------------*
051700171205      * che tipo instradamento utilizzare fra CAPPARI DPD vecchio/nuovo
051800171205      *--------------------------------------------------------------*
051900171205     C     Vers_InstradamBegsr
052000171205      *
052100171205      *- imposta come default il vecchio cappario
052200171205     c                   eval      NEW_CAPDPD ='N'
052300171205      *-
052400171205      *  e, se attivata la tabella, e la linea si trova abilitata
052500171205      *  prende il nuovo cappario.
052600171205     c                   eval      kpjbus = kpjbu
052700171205     c                   clear                   dsbs02
052800171205     c                   eval      T02mod = 'C'
052900171205     c                   eval      T02cod = 'DP5'
053000171205     c                   eval      t02sif = knsif
053100171205     c                   eval      T02ke1 = %editc(ormPOR:'X')
053200171205     c                   call      'TIBS02R'
053300171205     c                   parm                    kpjba
053400171205     c                   parm                    dsbs02
053500171205     c                   eval      kpjbu = kpjbus
053600171205      *
053700171205      **  Trovato record DP5 per la linea di arrivo
053800171205     c                   IF        T02err = *blank
053900171205     c                   eval        d5_lnpds = T02uni
054000171205      *
054100171205      **  Se è presente la lnp
054200171205      **    oppure sono state tutte attivate (999)
054300171205     c                   if        %lookup(%editc(ormPOE:'X'):d5_lnp)>0
054400171205     c                             or %lookup('999':d5_lnp)>0
054500171205     c                   eval      NEW_CAPDPD ='S'
054600171205     c                   endif
054700171205      *
054800171205     C                   End
054900171205      *
055000171205     C                   Endsr
055100171205      *--------------------------------------------------------------*
