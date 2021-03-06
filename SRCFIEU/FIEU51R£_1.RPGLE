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
001700080925      *
001800080925     d DSCAP           ds
001900080925     d  CAPinSK                1      9
002000080925     D SkCAP                   1      9    dim(9)
002100060814
002200900418     D KPJBA         E DS
002300050705     D trul33ds      E DS
002400080505     d**** TISID3ds      e ds                  inz
002500080505     d TISIE3ds      e ds                  inz
002600080505     d ds3idp        e ds
002700050704
002800050705     D KPJBus          s                   like(kpjbu)
002900050705     D progr           s              7s 0
003000050705     D primo           s              1
003100060920     D status          s                   like(dposts)
003200050711     D dpodepw         s                   like(dpodep)
003300080520     D Cod_ordinante   s                   like(ormcor)
003400050706     D ident           s              1
003500090715     D Info1           s             30
003600090715     D Info2           s             30
003700050927      **?__________________________________________________________________ */
003800050704      *    main
003900050927      **?__________________________________________________________________ */
004000060920     c                   z-add     10            status
004100060920
004200050705     c     rielabora     tag
004300050705
004400050705     c     status        setll     fidpo02l
004500050705     c                   do        *hival
004600050705     c     status        reade     fidpo02l
004700050705
004800050705     c                   if        %eof(fidpo02l)
004900050705     c                   leave
005000050705     c                   endif
005100050705
005200050705     c     korm          chain     fnorm01l
005300050705     c                   if        %found(fnorm01l)
005400060920
005500060920      ** ORM chiuso
005600060920     c                   if        ormFAO =999
005700060920     c                   move      'A'           dpoANN
005800080513     c*******            else
005900080513     c                   end
006000060920      *
006100060920      * operazioni da eseguire una sola volta reperimento numero di
006200060920      * trasmissione e intestazione del file da inviare
006300060920     c                   if        primo = *blank
006400060920     c                   exsr      file_dpdt
006500060920     c                   move      'X'           primo
006600060920     c                   endif
006700060920
006800050705     c                   exsr      file_dpdD
006900060920
007000050705     c                   z-add     progr         DPONTR
007100080513     c********           end
007200060920
007300060920     c                   move      *date         DPODTR
007400050704     c                   update    fidpo000                             99
007500060814
007600050705      * non trovato lascio il rekord spia intatto e ne leggo un'altro
007700050705     c                   else
007800050705     c                   iter
007900050705     c                   endif
008000050705
008100050705     c                   enddo
008200050927      **?controllo sincronia per loop a status 50 +                         */
008300050927      * chiamata controllo rekord trasmessi ma con orm non eseguiti
008400050927      * i rekord con orm in fase 999 vengono riaggiornati per trasmettere
008500050927      * l'informazione che l'orm non � stato effettuato
008600060920     c                   if        status = 10
008700050927     c                   commit
008800050927
008900050927     c                   call      'FIEU52R'
009000050927     c                   parm                    kpjba
009100050927
009200060920      * rilancio l'elaborazione con status a 19
009300060920     c                   z-add     19            status
009400050706     c                   move      '3'           ident
009500050705     c                   goto      rielabora
009600050705     c                   endif
009700060920
009800050705      * chiusura file da inviare
009900050707     c                   if        primo <> *blank
010000050707     c                   exsr      file_dpdP
010100050707     c                   commit
010200060814
010300060814     C* Infine elimino il blocco elaborazione TIVGD
010400060814     C                   clear                   trul47ds
010500060814     C                   eval      d47opz  = 'F'
010600060814     C                   eval      d47tip  = '$8'
010700060814     C                   call      'TRUL47R'
010800060814     C                   parm                    trul47ds
010900050707     c                   endif
011000050707      *
011100050705     c                   seton                                        lr
011200050927      **?__________________________________________________________________ */
011300050705     c     File_dpdT     begsr
011400050927      **?__________________________________________________________________ */
011500060814
011600060814      *  istruzioni apertura blocco scrittura TIVGD
011700060814     C                   clear                   trul47ds
011800060814     C                   eval      d47opz  = 'I'
011900060814     C                   eval      d47tip  = '$8'
012000060814     C                   eval      d47lck  = 'N'
012100060814     C                   eval      d47chkj = 'N'
012200060814     C                   eval      d47pgm  = 'FIEU51R'
012300060814     C                   call      'TRUL47R'
012400060814     C                   parm                    trul47ds
012500060814
012600060814      * preleva il numero Parcel
012700050705     c                   exsr      numero
012800050705      * testata depot
012900050705     c                   eval      DPorecord =
013000060814     c                             '# FILE'+ ';' + 'DEPOT' +
013100060814     c                             %trim(dpodep) + ';' +
013200060814     c                             %trim(dpodep) + ';' +
013300050705     c                             %trim(%editc(*date:'Z')) + ';' +
013400050705     c                             %trim(%editc(oratr:'Z')) + ';' +
013500050705     c                             %trim(%editc(progr:'Z'))+ ';'
013600060814     C                   exsr      tivgd
013700050705
013800060814
013900060814      * testata depot
014000060814     c                   eval      DPorecord =
014100060814     c                             '# VERSION;0;'
014200060814     C                   exsr      tivgd
014300060814
014400050705      * testata depot
014500050705     c                   eval      DPorecord =
014600050707     c                             '# DEF;CR:CRDATA01;ODEPOT;ORDERNR;CDEPOT;OCU-
014700050707     c                             STNO;ONAME;ONAME2;OSTREET;OCOUNTRY;OPOSTAL;-
014800050705     c                             OCITY;OPHONE;OEMAIL;CNAME;CNAME2;CNAME3;CN-
014900050705     c                             AME4;CSTREET;CCOUNTRY;CPOSTAL;CCITY;CPHONE-
015000050705     c                             ;CEMAIL;RNAME;RNAME2;RSTREET;RCOUNTRY;RPOS-
015100050705     c                             TAL;RCITY;RPHONE;REMAIL;RDEPOT;PKNR;STATUS-
015200060814     c                             ;IDENT;CUSTREF;WEBNR;NEUDAT;AENDDAT;SERVICE-
015300060814     c                             ;CUSTOM;INFO1;INFO2;WEIGHT;'
015400090617      *
015500090617     c                   if        udtymd > 20090809
015600090617     c                   eval      DPorecord = %Trim(DPorecord) +
015700090617     c                             'CONFIRM;PICKUPDATE;CONTENT;DELISID;-
015800090617     c                             CANCELUSER;;'
015900090617     c                   end
016000090617      *
016100060814     C                   exsr      tivgd
016200050705
016300050704     c                   endsr
016400050927      **?__________________________________________________________________ */
016500050705     c     File_dpdD     begsr
016600050927      **?__________________________________________________________________ */
016700050705      * dati orm
016800050711      * forzatura nazione italia se non specificato sull'orm
016900050711     c                   if        ormnao= *blank
017000050711     c                   movel     'IT'          ormnao
017100050711     c                   endif
017200050711     c                   if        ormnar= *blank
017300050711     c                   movel     'IT'          ormnar
017400050711     c                   endif
017500050711     c                   if        ormnac= *blank
017600050711     c                   movel     'IT'          ormnac
017700050711     c                   endif
017800080925      **
017900080925      * CAP "EIRE" x l'Irlanda "DPD" chiede di mettere "2" al posto di "EIRE"
018000080925     c                   if        ormcao= 'EIRE' or ormnao='IRL'
018100080925     c                   eval      ormcao = '2'
018200080925     c                   endif
018300080925     c                   if        ormcar= 'EIRE' or ormnar='IRL'
018400080925     c                   eval      ormcar = '2'
018500080925     c                   endif
018600080925     c                   if        ormcac= 'EIRE' or ormnac='IRL'
018700080925     c                   eval      ormcac = '2'
018800080925     c                   endif
018900080925      **
019000080925      * CAP Inglesi "DPD" chiede di non avere alcuno spazio inmezzo alle lettere del CAP
019100080925      **    compatta i CAP x l'Inghilterra
019200080925     c                   if         ormnao='GB'
019300080925     c                   eval       CAPinSK = ormcao
019400080925     c                   exsr      Compatta_CAP
019500080925     c                   eval       ormcao = CAPinSK
019600080925     c                   endif
019700080925      *
019800080925     c                   if         ormnar='GB'
019900080925     c                   eval       CAPinSK = ormcar
020000080925     c                   exsr      Compatta_CAP
020100080925     c                   eval       ormcar = CAPinSK
020200080925     c                   endif
020300080925      *
020400080925     c                   if         ormnac='GB'
020500080925     c                   eval       CAPinSK = ormcac
020600080925     c                   exsr      Compatta_CAP
020700080925     c                   eval       ormcac = CAPinSK
020800080925     c                   endif
020900080925      **
021000050711
021100050711      *reperisce codice depot ritiro da cappario dpd
021200050711     c                   exsr      cappariodpd
021300060814
021400060814      * Nazioni formato codice ISO
021500060814     c                   movel(p)  ormNAO        TBLKEY
021600060814     c                   clear                   c15NAO            3 0
021700080505     c                   movel     '15'          TBLCOD
021800060814     c     ktab          chain     tabel00f
021900060814     c                   if        %Found(tabel00f)
022000060814     c                   movel     tbluni        ds15
022100060814     c                   z-add     �15CIE        c15NAO            3 0
022200080514     c                   end
022300080514      * Italia non � codificata
022400080514     c                   if        c15Nao = 0
022500080514     c                   z-add     380           c15Nao
022600080514     c                   end
022700050711
022800060814     c                   movel(p)  ormNAR        TBLKEY
022900060814     c                   clear                   c15NAR            3 0
023000060814     c     ktab          chain     tabel00f
023100060814     c                   if        %Found(tabel00f)
023200060814     c                   movel     tbluni        ds15
023300060814     c                   z-add     �15CIE        c15NAR            3 0
023400060814     c                   end
023500080514      * Italia non � codificata
023600080514     c                   if        c15Nar = 0
023700080514     c                   z-add     380           c15Nar
023800080514     c                   end
023900060814
024000060814     c                   movel(p)  ormNAC        TBLKEY
024100060814     c                   clear                   c15NAC            3 0
024200060814     c     ktab          chain     tabel00f
024300060814     c                   if        %Found(tabel00f)
024400060814     c                   movel     tbluni        ds15
024500060814     c                   z-add     �15CIE        c15NAC            3 0
024600060814     c                   end
024700080514      * Italia non � codificata
024800080514     c                   if        c15Nac = 0
024900080514     c                   z-add     380           c15Nac
025000080514     c                   end
025100080925      **
025200080520      ** il codice del cliente Ordinante x DPD non pu� essere = (0)
025300080520     c                   clear                   Cod_ordinante
025400080520     c                   if        ormcor > 0
025500080520     c                   eval       Cod_ordinante = ormcor
025600080520     c                   else
025700080520      ** altrimenti metto il numero dell'ordine DPD
025800080520     c                   eval       Cod_ordinante = dpoord
025900080520     c                   end
026000060814
026100060814      * I CAMPI TRACCIATI SONO ALLA SINISTRA SONO SEGNATI I NUMERI
026200050711      * DEI CAMPI IN MODO DA POTER RICONDURRE ALLA COMPOSIZIONE DEL
026300050711      * MESSAGGIO
026400110111     c     ';':' '       xlate     ORMrso        ORMrso
026500110111     c     ';':' '       xlate     ORMino        ORMino
026600110111     c     ';':' '       xlate     ORMcao        ORMcao
026700110111     c     ';':' '       xlate     ORMloo        ORMloo
026800110111     c     ';':' '       xlate     ORMpro        ORMpro
026900110111     c     ';':' '       xlate     ORMnao        ORMnao
027000110111      *
027100110111     c     ';':' '       xlate     ORMrsr        ORMrsr
027200110111     c     ';':' '       xlate     ORMinr        ORMinr
027300110111     c     ';':' '       xlate     ORMlor        ORMlor
027400110111     c     ';':' '       xlate     ORMcar        ORMcar
027500110111     c     ';':' '       xlate     ORMprr        ORMprr
027600110111     c     ';':' '       xlate     ORMnar        ORMnar
027700110111      *
027800110111     c     ';':' '       xlate     ORMrer        ORMrer
027900110111     c     ';':' '       xlate     ORMter        ORMter
028000110111      *
028100110111     c     ';':' '       xlate     ORMrsc        ORMrsc
028200110111     c     ';':' '       xlate     ORMinc        ORMinc
028300110111     c     ';':' '       xlate     ORMcac        ORMcac
028400110111     c     ';':' '       xlate     ORMloc        ORMloc
028500110111     c     ';':' '       xlate     ORMprc        ORMprc
028600110111     c     ';':' '       xlate     ORMnac        ORMnac
028700110111      *
028800110111     c     ';':' '       xlate     ORMnam        ORMnam
028900110111     c     ';':' '       xlate     ORMno1        ORMno1
029000110111     c     ';':' '       xlate     ORMno2        ORMno2
029100110111     c     ';':' '       xlate     ORMzor        ORMzor
029200110111     c     ';':' '       xlate     ORMrfa        ORMrfa
029300110111     c     ';':' '       xlate     ORMflo        ORMflo
029400081202
029500090515      *
029600090515      * Se manca l'anagrafica dell'ordinante imposto quella del Mittente
029700090515      *  Altrimenti DPD non esegue l'ORM (C.Burke maggio 2009)
029800090515      * Poich� il nostro sistema invece permette di non avere l'ordinante
029900090515      *  cos� si � aggirato l'ostacolo.
030000090515     c                   if        ormrso=*blank and ormino=*blank and
030100090515     c                             ormcao=*blank and ormloo=*blank
030200090515     c                   eval      ormrso = ormrsc
030300090515     c                   eval      ormino = orminc
030400090515     c                   eval      ormnao = ormnac
030500090515     c                   eval      ormcao = ormcac
030600090515     c                   eval      ormloo = ormloc
030700090515     c                   end
030800090715      *
030900090715     c                   eval      Info1 = ormNo1
031000090715     c                   eval      Info2 = ormNo2
031100090715      * aggiunge il riferimento (persona/telefono x il ritiro nelle note se c'� spazio
031200090715     c                   If        Info1 = *blank and Info2 = *blank
031300090715     c                   eval      Info1 = ormRER
031400090715     c                   eval      Info2 = ormTER
031500090715     c                   elseIf    Info1 <> *blank and Info2 = *blank
031600090715     c                   eval      Info2 = ormRER
031700090715     c                   eval      Info2 = %Trim(Info2) + ':' + %trim(ormRER)
031800090715     c                             + ':' + %trim(ormTER)
031900090715     c                   endIf
032000090715      *
032100061023     c                   move      dpoord        dpoordA           6
032200060814 1   c                   eval      DPorecord = %trim(dpodep) +';'+
032300061023 2   c                             %trim(dpoordA) +';'+
032400060814 3   c                             %trim(dpodepw) +';'+
032500080520 4   c**********                   %trim(%editc(ormcor:'X')) +';'+
032600080520 4   c                             %trim(%editc(Cod_ordinante:'X')) +';'+
032700060814 5   c                             %trim(ormrso) +';;'+
032800060814 7   c                             %trim(ormino) +';'+
032900060814 8   c                             %trim(%editc(c15NAO:'Z')) +';'+
033000060814 9/10c                             %trim(ormcao) +';'+ %trim(ormloo) +';;;'+
033100060814 13  c                             %trim(ormrsr) +';;;;'+
033200060814 17  c                             %trim(orminr) +';'+
033300060814 18  c                             %trim(%editc(c15NAR:'Z')) +';'+
03340006081419/20c                             %trim(ormcar) +';'+ %trim(ormlor) +';'+
033500060814 21  c                             %trim(ormter) +';;'+
033600060814 23  c                             %trim(ormrsc) +';;'+
033700060814 25  c                             %trim(orminc) +';'+
033800060814 26  c                             %trim(%editc(c15NAC:'Z')) +';'+
03390006081427/28c                             %trim(ormcac) +';'+ %trim(ormloc) +';;;'+
034000060814 31  c                             %trim(dpodep) +';;;'+
034100060814 34  c                             %trim(ident) +';;;;;;;'+
03420009071541/42c                             %trim(Info1) +';'+ %trim(Info2) +';'+
034300060814 43  c                             %trim(%editc(ormpkg:'2')) +';'
034400090617      *
034500090617     c                   if        udtymd > 20090809
034600100624      *  Data ritiro (PICKUPDATE) e Natura Merce (CONTENT)
034700100624     c                   move      ormDAR        data_ritiro       6
034800100624     c                   eval      DPorecord = %Trim(DPorecord) + ';' +
034900100624     c                             %trim(data_ritiro) + ';' +
035000100624     c                             %trim(ormNAM) + ';;;'
035100090617     c                   end
035200090617      *
035300060814     C                   exsr      tivgd
035400050705
035500050705     c                   endsr
035600080925      **?__________________________________________________________________ */
035700080925     c     Compatta_CAP  begsr
035800080925      **?__________________________________________________________________ */
035900080925     c                   z-add     0             to                3 0
036000080925     c                   do        9             from              3 0
036100080925     c                   if        skCAP(from) = *blank
036200080925     c                   if          to = 0
036300080925     c                   z-add     from          to
036400080925     c                   end
036500080925     c                   else
036600080925     c                   if          to > 0
036700080925     c                   move      skCAP(from)   skCAP(to)
036800080926     c                   move      *blank        skCAP(from)
036900080925     c                   add       1             to
037000080925     c                   end
037100080925     c                   end
037200080925     c                   enddo
037300080925
037400080925     c                   endsr
037500050927      **?__________________________________________________________________ */
037600050705     c     File_dpdP     begsr
037700050927      **?__________________________________________________________________ */
037800050705      * piede messaggio
037900050705     c                   eval      DPorecord =
038000050705     c                             '# END;' +
038100050705     c                             %trim(%editc(progr:'Z')) + ';'
038200060814     C                   exsr      tivgd
038300050705
038400050705     c                   endsr
038500050927      **?__________________________________________________________________ */
038600050705     c     numero        begsr
038700050927      **?__________________________________________________________________ */
038800050705     c                   eval      kpjbus=kpjbu
038900050705     c                   clear                   trul33ds
039000050705     c                   move      'L'           i33tla
039100050705     c                   z-add     305           i33cnu
039200050705     c                   z-add     1             i33num
039300050705     c                   movel     trul33ds      kpjbu
039400050705     c                   call      'TRUL33R'
039500050705     c                   PARM                    kpjba
039600050705     c                   movel     kpjbu         trul33ds
039700050705     c                   z-add     o33nri        progr
039800050705     c                   eval      kpjbu=kpjbus
039900050705
040000050705     c                   endsr
040100050927      **?__________________________________________________________________ */
040200050711     c     capparioDpd   begsr
040300050927      **?__________________________________________________________________ */
040400080505      * Cerco da cappario DPD
040500080505      * quello nuovo, ora il depot � lungo 4 e non pi� 3
040600080505     c                   clear                   tisie3ds
040700080505     c                   eval      isie3dri = udtymd
040800080505     c                   eval      isie3dsp = ormdar
040900080505     c                   time                    isie3hsp
041000080505     c                   eval      isie3nzd = ormnar
041100080505     c                   eval      isie3cad = ormcar
041200080505     c                   if        ormpkg > �3ilmp
041300080505     c                   eval      isie3srv = 101
041400080505     c                   else
041500080505     c                   eval      isie3srv = 136
041600080505     c                   endif
041700080505     c                   eval      isie3lnp = ormpoe
041800080505     c                   call      'TISIE3R'
041900080505     c                   parm                    tisie3ds
042000080505if  2c                   if        osie3err = *blanks
042100080505     c                   move(p)   osie3ddep     dpodepw
042200080505e   2c                   endif
042300080505
042400050711      * Cerco da cappario DPD
042500080505     c**!!!              clear                   TISID3ds
042600080505     c**!!!              clear                   dpodepw
042700080505     c**!!!              z-add     ORMdar        ID3dat
042800080505     c**!!!              movel     ORMnar        ID3naz
042900080505     c**!!!              movel     ORMcar        ID3cap
043000080505     c**!!!              call      'TISID3R'
043100080505     c**!!!              parm                    TISID3ds
043200080505if  2c**!!!              if        OD3err = *blanks
043300080505     c**!!!              move      OD3rtn        dpodepw
043400080505e   2c**!!!              endif
043500080505      *
043600080505     c                   endsr
043700050927      **?__________________________________________________________________ */
043800050512     c     *inzsr        begsr
043900050927      **?__________________________________________________________________ */
044000050512     c*
044100050705     c     *entry        plist
044200050705     C                   parm                    kpjba
044300060920     c*
044400050705     C                   TIME                    ORADAT           14 0
044500050705     C                   MOVEL     ORADAT        ORATR             6 0
044600060814     c                   move      *date         udtymd            8 0
044700060814     C                   TIME                    W0140
044800060814     C                   MOVE      W0140         W0080             8 0
044900050704
045000050706     c     korm          klist
045100050706     c                   kfld                    DPOPOE
045200050706     c                   kfld                    DPONRS
045300050706     c                   kfld                    DPONOR
045400050706     c                   kfld                    DPONRV
045500060814
045600060814     c     ktab          klist
045700060814     c                   kfld                    TBLKUT
045800060814     c                   kfld                    TBLCOD
045900060814     c                   kfld                    TBLKEY
046000060814     c                   z-add     1             TBLKUT
046100080505      * carico tabella DPD
046200080505     c                   movel     '3I'          tblcod
046300080505     c                   movel(p)  'DPD'         tblkey
046400080505     c     ktab          chain     tabel00f
046500080505     c                   clear                   ds3idp
046600080505     c                   if        %found(tabel00f) and tblflg = *blanks
046700080505     c                   eval      ds3idp = tbluni
046800080505     c                   endif
046900080505      *
047000060814
047100050512     c                   endsr
047200060814      *----------------------------------------------------*
047300060814     C     tivgd         Begsr
047400060814      *
047500060814     c                   clear                   tivgd000
047600080403     c                   eval      vgddta = %TrimR(dpoRecord)
047700060814     c                   eval      vgdtip = '$8'
047800060814     c                   eval      vgdksu = '0DPD0OUT'
047900060814     c                   eval      vgdtsc = 'WW'
048000060814     c                   eval      vgdpgm = 'FIEU51R'
048100061023     c                   eval      vgddat = udtymd
048200060814      *
048300060814     C                   WRITE     tivgd000
048400060814      *
048500060814     C                   Endsr
048600060814      *----------------------------------------------------*
