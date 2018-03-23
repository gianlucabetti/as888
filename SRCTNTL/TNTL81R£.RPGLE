000100091001----- *-------------------------------------------------------------
000200160419      * La stampa dei Traini è con la Firma di PEDINI.
000300091001----- *-------------------------------------------------------------
000400091001      * il prtf va compilato con i seguenti parametri
000500091001      * CRTPRTF FILE(GAITRAFIL/TNTL81P)
000600091001      * DEVTYPE(*AFPDS)
000700091001      * TEXT('Allegato contratto traini.')
000800091001      * PAGESIZE(70 120 *ROWCOL)
000900091001      * CPI(15)
001000160419      * OVRFLW(60)
001100091001      * DUPLEX(*YES)
001200091001      * FORMTYPE(ALLEGTRAIN)
001300091001      * USRDTA(ALLEGTRAIN)
001400091001----- *-------------------------------------------------------------
001500090511     H DFTACTGRP(*NO) BNDDIR('PRNPGM')
001600981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
001700091001     H*--------------------------------------------------------------*
001800060320     Ftitll02l  if   e           k disk
001900110504     Ftntlt01l  if   e           k disk
002000060320     Ftntln01l  if   e           k disk
002100060320     Fanfrn01l  if   e           k disk
002200080918     Fancdp01l  if   e           k disk
002300060321     Fansog01l  if   e           k disk
002400080916     Fanind01l  if   e           k disk
002500060320     Fazorg01l  if   e           k disk
002600160419     Ftntl81p   O    e             PRINTER OFLIND(*IN60) usropn
002700011026
002800020304     dkpjba          e ds
002900020304
003000020304     dparam            ds
003100060320     D  datae                         8s 0
003200060321     D  traz1                         8  0
003300060320     D  socie                         3
003400090512     D  dataCerta                     8s 0
003500090512
003600020402     d modo            s              8
003700020114     d base            s              1
003800020412     d cost3b          s            120
003900020412     d cost3c          s            120
004000080916     d cost3D          s            120
004100080916     d cost3E          s            120
004200090417     d cost3f          s            120
004300090417     d cost3g          s            120
004400090417     d cost3h          s            120
004500090417     d cost3i          s            120
004600060421     d costt           s             27
004700060321     d costa           s             74
004800060321     d costa2          s             35
004900060321     d costa2b         s             16
005000020411     d costa3          s            120
005100020411     d costa4          s            120
005200020411     d costb           s            120
005300111206     d costc           s            120
005400020411     d cost1           s            120
005500020411     d cost2           s            120
005600020306     d cost3           s            120
005700020306     d cost4           s            120
005800020306     d cost5           s            120
005900020411     d cost5b          s            120
006000060526     d cost5c          s            120
006100060526     d cost5d          s            120
006200020306     d cost6           s            120
006300020411     d cost6a          s            120
006400090417     d cost6b          s            120
006500020411     d costi           s            120
006600020411     d costia          s            120
006700020411     d costib          s            120
006800020411     d costic          s            120
006900020411     d costid          s            120
007000020411     d costie          s            120
007100020423     d costif          s            120
007200080916     d costiH          s            120
007300080916     d costiI          s            120
007400020306     d cost7           s            120
007500070302     d cost7a          s            120
007600020411     d cost7b          s            120
007700020412     d cost7c          s            120
007800020412     d cost7d          s            120
007900020306     d cost8           s            120
008000020402     d cost8b          s            120
008100020402     d cost8c          s            120
008200020402     d cost8f          s            120
008300090417     d cost8g          s            120
008400090417     d cost8h          s            120
008500060510     d cost9           s             35
008600060322     d cost10          s             60
008700060322     d cost11          s             60
008800020313     d cost13          s             70
008900020305     d dataiso         s               d   datfmt(*iso)
009000020305     d dataeur         s               d   datfmt(*eur)
009100090511     d copiaVettore...
009200090511     d                 s               n
009300090512     D certaData       s              8s 0
009400111223     D condpag         s                     like(frncondpag)
009500080916     d locemi          s             40
009600090930     d saltato         s              1    INZ(' ')
009700080916     D DVCOBADS      E DS
009800080916     D                                     INZ
009900080916     D CBAIBANDS     E DS
010000090114     d fnlvt1ds      e ds
010100090114     d ficn05ds      e ds
010200080916     ddgpi           e ds
010300110504     ***************************************************************************
010400110504     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
010500110504     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
010600110504     ***************************************************************************
010700110504     ** Definizione strutture dati.
010800110504      **************************************************************************
010900110504     D tibsSocI0...
011000110504     D               E DS                  QUALIFIED
011100110504     D                                     INZ
011200110504     D tibsSocO0...
011300110504     D               E DS                  QUALIFIED
011400110504     D                                     INZ
011500110504     ***************************************************************************
011600110504     ** Definizione variabili modulo/programma.
011700110504     ***************************************************************************
011800110504     D prmRqsOpCode...
011900110504     D                 S             10A
012000110504     D prmRpyOpCode...
012100110504     D                 S             10A
012200110504     D prmRpyIdMsg...
012300110504     D                 S             10I 0
012400110504     D prmRqsFormato...
012500110504     D                 S             10A
012600110504     D prmRqsDataSize...
012700110504     D                 S             10I 0
012800110504     D prmRpyFormato...
012900110504     D                 S             10A
013000110504     D prmRpyDataSize...
013100110504     D                 S             10I 0
013200090511     ***************************************************************************
013300090511     ** Main.
013400090511     ***************************************************************************
013500160419     c                   seton                                        lr
013600090511
013700090511     ** Stampo la copia per il vettore.
013800090511     c                   eval      copiaVettore = *on
013900090511     c
014000090511     c                   reset                   prima
014100060321     c                   move      traz1         kpdr              7 0
014200020308     c
014300060321     c     kpdr          setll     titll02l
014400020308     c                   do        *hival
014500060321     c                   read      titll02l
014600060320      *fine file
014700060321     c                   if        %eof(titll02l)
014800060320     c                   leave
014900060320     c                   endif
015000060320      *trazionista singolo
015100060320     c                   if        kpdr > 0 and kpdr <> tllpdr
015200060320     c                   leave
015300060320     c                   endif
015400060324      *trazione a costo fittizio 1 euro da non inserire in elenco
015500060324     c                   if        tllili = 1
015600060324     c                   iter
015700060324     c                   endif
015800060321      *trazionista generico 9999999 valido solo se richiesto singolarmente
015900060321     c                   if        kpdr <> 9999999 and tllpdr = 9999999
016000060321     c                   leave
016100060321     c                   endif
016200060320      *leggo traino
016300060320     c     ktlt          chain     tntlt01l
016400060320     c                   if        %found(tntlt01l)
016500060321     c                   if        datae  >= tltdde and
016600060321     c                             datae  <= tltdsc
016700060321     c                   if        primo_giro = *blank
016800060321     c                   move      tllpdr        expdr
016900060321     c                   move      'X'           primo_giro        1
017000060321     c                   endif
017100060320     c                   exsr      elabora
017200060320     c                   else
017300060320     c                   iter
017400060320     c                   endif
017500020308
017600060320     c                   else
017700060320     c                   iter
017800060320     c                   endif
017900060320      *
018000020308     c                   enddo
018100090511
018200060321     c                   if        prima <> *BLANK
018300160419      *
018400160419     C                   write     piede2
018500160419      *
018600160419     C                   exsr      finepagina
018700160419     C                   write     piede4
018800160419      *
018900160419     C                   exsr      finepagina
019000160419     C                   write     piede6
019100160419      *
019200160419     C                   exsr      finepagina
019300160419     C                   write     piede12
019400160419      *
019500160419     C                   exsr      finepagina
019600160419     C                   write     piede14
019700160419      *
019800160419     C                   exsr      finepagina
019900160419     C                   write     piede16
020000160419      *
020100160419     C                   exsr      finepagina
020200160419     C                   write     piede18
020300160419      *
020400160419     C                   exsr      finepagina
020500160419     C                   write     piede20
020600160419      *
020700160419     C                   exsr      finepagina
020800160419     C                   write     piede22
020900160419      *
021000160419     C                   exsr      finepagina
021100160419     C                   write     piede24
021200160419      *
021300160419     C                   exsr      finepagina
021400160419     C                   write     piede26
021500160419      *
021600160419     C                   exsr      finepagina
021700160419     C                   write     piede28
021800160419      *
021900160419     C                   exsr      finepagina
022000160419     C                   write     piede30
022100160419      *
022200160419     C                   exsr      finepagina
022300160419     C                   write     piede32
022400160419      *
022500160419     C                   exsr      finepagina
022600160419     C                   write     piede34
022700160419      *
022800160419     C                   exsr      finepagina
022900160419     C                   write     piede36
023000160419      *
023100160419     C                   exsr      finepagina
023200160419     C                   write     piede38
023300160419      *
023400160419     C                   exsr      finepagina
023500160419     C                   write     piede40
023600160419      *
023700160419     C                   exsr      finepagina
023800160419     C                   write     piede42
023900160419      *
024000160419     C                   exsr      finepagina
024100160419     C                   write     piede44
024200160419      *
024300160419     C                   exsr      finepagina
024400160419     C                   write     piede46
024500160419      *
024600160419     C                   exsr      finepagina
024700160419     C                   write     piede48
024800160419      *
024900160419     C                   exsr      finepagina
025000160419     C                   write     piede50
025100160419      *
025200160419     C                   exsr      finepagina
025300160419     C                   write     piede52
025400160419      *
025500160419     C                   exsr      finepagina
025600160419     C                   write     piede54
025700160419      *
025800160419     C                   exsr      finepagina
025900160419     C                   write     piede56
026000160419      *
026100160419     C                   exsr      finepagina
026200160419     C                   write     piede58
026300160419      *
026400160419     C                   exsr      finepagina
026500160419     C                   write     piede60
026600160419      *
026700160419     C                   exsr      finepagina
026800160419     C                   write     piede62
026900160419      *
027000160419     C                   exsr      finepagina
027100160419     c                   write     piede70
027200160419      *
027300160419     C                   exsr      finepagina
027400160419     c                   write     piede72
027500160419      *
027600160419     C                   exsr      finepagina
027700160419     c                   write     piede74
027800160419      *
027900160419     C                   exsr      finepagina
028000160419     c                   write     piede80
028100160419      *
028200160419     C                   exsr      finepagina
028300160419     c                   write     piede82
028400160419      *
028500160419     C                   exsr      finepagina
028600160419     c                   write     piede84
028700160419      *
028800160419     C                   exsr      finepagina
028900160419     c                   write     piede86
029000160419      *
029100160419     C                   exsr      finepagina
029200160419     c                   write     piede88
029300090930      *
029400090511     c                   if        copiaVettore
029500160419     c                   write     firmaMitt
029600160419     c                   write     Firma
029700090511     c                   ENDIF
029800090930      *
029900160516     c                   if        saltato = *blank
030000160516     c*******            write     PagBIANCA
030100160516     c*******            write     Firma
030200160516     c                   end
030300090930      *
030400090511     C                   close     tntl81p
030500090511     c                   endif
030600090511
030700160419     c                   return
030800090511
030900020308      *--------------------------------------------------
031000020308     c     elabora       begsr
031100020308      *--------------------------------------------------
031200020411     c
031300060320     c                   if        tllpdr <> expdr and prima <> *blank
031400160419     C                   write     piede2
031500160419      *
031600160419     C                   exsr      finepagina
031700160419     C                   write     piede4
031800160419      *
031900160419     C                   exsr      finepagina
032000160419     C                   write     piede6
032100160419      *
032200160419     C                   exsr      finepagina
032300160419     C                   write     piede12
032400160419      *
032500160419     C                   exsr      finepagina
032600160419     C                   write     piede14
032700160419      *
032800160419     C                   exsr      finepagina
032900160419     C                   write     piede16
033000160419      *
033100160419     C                   exsr      finepagina
033200160419     C                   write     piede18
033300160419      *
033400160419     C                   exsr      finepagina
033500160419     C                   write     piede20
033600160419      *
033700160419     C                   exsr      finepagina
033800160419     C                   write     piede22
033900160419      *
034000160419     C                   exsr      finepagina
034100160419     C                   write     piede24
034200160419      *
034300160419     C                   exsr      finepagina
034400160419     C                   write     piede26
034500160419      *
034600160419     C                   exsr      finepagina
034700160419     C                   write     piede28
034800160419      *
034900160419     C                   exsr      finepagina
035000160419     C                   write     piede30
035100160419      *
035200160419     C                   exsr      finepagina
035300160419     C                   write     piede32
035400160419      *
035500160419     C                   exsr      finepagina
035600160419     C                   write     piede34
035700160419      *
035800160419     C                   exsr      finepagina
035900160419     C                   write     piede36
036000160419      *
036100160419     C                   exsr      finepagina
036200160419     C                   write     piede38
036300160419      *
036400160419     C                   exsr      finepagina
036500160419     C                   write     piede40
036600160419      *
036700160419     C                   exsr      finepagina
036800160419     C                   write     piede42
036900160419      *
037000160419     C                   exsr      finepagina
037100160419     C                   write     piede44
037200160419      *
037300160419     C                   exsr      finepagina
037400160419     C                   write     piede46
037500160419      *
037600160419     C                   exsr      finepagina
037700160419     C                   write     piede48
037800160419      *
037900160419     C                   exsr      finepagina
038000160419     C                   write     piede50
038100160419      *
038200160419     C                   exsr      finepagina
038300160419     C                   write     piede52
038400160419      *
038500160419     C                   exsr      finepagina
038600160419     C                   write     piede54
038700160419      *
038800160419     C                   exsr      finepagina
038900160419     C                   write     piede56
039000160419      *
039100160419     C                   exsr      finepagina
039200160419     C                   write     piede58
039300160419      *
039400160419     C                   exsr      finepagina
039500160419     C                   write     piede60
039600160419      *
039700160419     C                   exsr      finepagina
039800160419     C                   write     piede62
039900160419      *
040000160419     C                   exsr      finepagina
040100160419     c                   write     piede70
040200160419      *
040300160419     C                   exsr      finepagina
040400160419     c                   write     piede72
040500160419      *
040600160419     C                   exsr      finepagina
040700160419     c                   write     piede74
040800160419      *
040900160419     C                   exsr      finepagina
041000160419     c                   write     piede80
041100160419      *
041200160419     C                   exsr      finepagina
041300160419     c                   write     piede82
041400160419      *
041500160419     C                   exsr      finepagina
041600160419     c                   write     piede84
041700160419      *
041800160419     C                   exsr      finepagina
041900160419     c                   write     piede86
042000160419      *
042100160419     C                   exsr      finepagina
042200160419     c                   write     piede88
042300090930      *
042400090511     c                   if        copiaVettore
042500160419     c                   write     firmaMitt
042600160419     c                   write     Firma
042700090511     c                   ENDIF
042800060321     c                   move      tllpdr        expdr             7 0
042900060321     c                   clear                   prima             1
043000090511     C                   close     tntl81p
043100060320     c                   endif
043200060320
043300060320     c                   if        prima = *blank
043400090511     C                   OPEN      tntl81p
043500060321     c                   z-add     0             page
043600060321     c                   move      expdr         fornit
043700080916     c* imposto costanti nel caso non reperisco anagrafico
043800080916     c                   eval      costia= 'Ragione sociale ___________________+
043900080916     c                             ____________________________________________'
044000080916
044100080916     c                   eval      costib= 'Indirizzo__________________________+
044200080916     c                             ____________________________________________'
044300080916
044400080916     c                   eval      costic= 'CAP______________ Città____________+
044500080916     c                             ____________________________________________'
044600080916
044700080916     c                   eval      costif= 'Recapito __________________________+
044800080916     c                             ____________________________________________'
044900080916     c                   eval      costid= 'Partita IVA _______________________+
045000080916     c                             ____________________________________________'
045100080916     c                   eval      cost8c= '|__|__|__|__|  -
045200080916     c                             |__|__|__|__|  |__|__|__|__|  |__|__|__|__| -
045300080916     c                              |__|__|__|__|  |__|__|__|__|  |__|__|__|'
045400060321      * decodifico fornitore
045500060321     C                   clear                   ragsvet
045600080918     c                   movel     *all'_'       cdpdescr
045700060321     C     kfrn          chain     anfrn01l
045800060321     c                   if        %found(anfrn01l)
045900111223     c                   if        datae = 20120111 and frncondpag = 'B70'
046000111223     c                   eval      condpag = 'B55'
046100111223     c                   else
046200111223     c                   eval      condpag = frncondpag
046300111223     c                   end
046400080918     C     kcdp          chain     ancdp01l
046500060321     C     frnsogg       chain     ansog01l
046600060321     c                   if        %found(ansog01l)
046700080919     C                   MOVEL     sogdes        ragsvet          44
046800060321     c                   endif
046900060321     c                   endif
047000080918     c                   eval      cost8b= %trimr(cdpdescr) +' '+
047100080918     c                             'disposto su codice IBAN:'
047200080916     c* indirizzo
047300080916     c                   clear                   tipo
047400080916     c                   clear                   codice
047500080916     C     Kind          CHAIN     anind01l
047600080916     c                   if        %found(anind01l)
047700080916     C                   eval      costia = 'Ragione sociale ' + sogdes
047800080916     C                   eval      costib = 'Indirizzo ' + indindriz
047900080916     c                   if        indprov <> ' '
048000080916     C                   eval      locemi = %trimr(indlocalit)+' ('+indprov+')'
048100080916     c                   else
048200080916     C                   eval      locemi = indlocalit
048300080916     c                   end
048400080916     C                   eval      costic = 'CAP ' + indcap + ' Città ' +
048500080916     c                             locemi
048600080916     c                   end
048700080916     C                   eval      costid = 'Partita IVA ' + sogpartiva
048800091218      **
048900091218      **  oltre alla P.IVA con la nuova legge introdotto dal gennaio 2010
049000091218      **   si devono esporre il Capitale Sociale il Cod.Fiscale e il Reg.Imprese
049100091218      **
049200091218      **  Aggiunge il Codice Fiscale
049300091218     c                   if        sogcdfisc <> ' ' and indprov <> ' '
049400091218     c                   eval      costiD = %Trim(costiD) +
049500091218     c                             ' C.F.-R.IMPRESE ' + %trimr(indprov) +
049600091218     C                             ': ' + %trimr(sogcdfisc)
049700091218     c                   end
049800091218      **
049900091218     c                   if        indtelex <> ' ' and
050000091218     c                             %subst(indtelex: 1: 8) = 'CAP.SOC.'
050100091218     c                   eval      costiD = %Trim(costiD) + ' ' + indtelex
050200091218     c
050300091218     c                   end
050400091218      **
050500080916     c* indirizzo recapito
050600080916     C                   SETOFF                                       89
050700080916     c                   eval      tipo ='IF'
050800080916     c                   eval      codice ='000'
050900080916     C     Kind          CHAIN     anind01l
051000080916     c                   if        %found(anind01l)
051100080916     C                   SETON                                        89
051200080916     c                   eval      costif= 'Recapito ' + sogdes
051300080916     C                   eval      costiH = indindriz
051400080916     c                   if        indprov <> ' '
051500080916     C                   eval      locemi = %trimr(indlocalit)+' ('+indprov+')'
051600080916     c                   else
051700080916     C                   eval      locemi = indlocalit
051800080916     c                   end
051900080916     C                   eval      costiI = indcap + ' ' +
052000080916     c                             locemi
052100080916     c                   end
052200080916     c* reperisco IBAN
052300080916     C                   RESET                   DVCOBADS
052400080916     C                   RESET                   CBAIBANDS
052500080916
052600080916     C                   EVAL      CoBSoc = socie
052700080916     C                   EVAL      CoBCOF = 'F'
052800080916     C                   EVAL      CoBKsc = fornit
052900080916     C                   CALL      'DVCOBA'
053000080916     C                   PARM                    DVCOBADS
053100080916A0846C                   PARM                    CBAIBANDS
053200080916     C                   IF        CoBErr = *OFF
053300080916     C                   EVAL      cost8c = CBaStato + CbaCheck
053400080916     C                             + %TRIMR(CBaCoorI)
053500080916     c                   end
053600110504      *
053700110504     C*** SPOSTATA QUI LA DECODIFICA DELLA RAGIONE SOCIALE MITTENTE
053800110504     c                   exsr      decod_societa
053900110504      *
054000060321      *stampa testata allegato
054100160420     c                   setoff                                       60
054200160419     c                   write     testata
054300160419     c                   write     testo
054400060320     c                   move      'X'           prima
054500020306     c                   endif
054600160419      *--
054700160419     C                   exsr      finepagina
054800160419      *--
054900060320      *decodifica partenza
055000060320     c     tlttfp        chain     azorg01l
055100060320     c                   if        %found(azorg01l)
055200060320     c                   movel     orgdes        partenza         20
055300060320     c                   else
055400060320     c                   clear                   partenza
055500060320     c                   endif
055600060320      *decodifica arrivo
055700060320     c     tlttfa        chain     azorg01l
055800060320     c                   if        %found(azorg01l)
055900060321     c                   movel     orgdes        arrivo           20
056000060320     c                   else
056100060321     c                   clear                   arrivo
056200060320     c                   endif
056300160419      *--
056400060320      * stampa riga listino attiva
056500160419     c                   write     dettaglio
056600060320      *reperimento nota
056700060320     c     ktln          chain     tntln01l
056800060320     c                   if        %found(tntln01l)
056900160419     c                   write     dettanote
057000060320     c                   endif
057100060320
057200020308     c                   endsr
057300160419     ***************************************************************************
057400160419     **  Controlla OVERFLOW
057500160419     ***************************************************************************
057600160419     c     finepagina    begsr
057700160419     C                   exsr      saltoPaginaF
057800160419     C                   exsr      saltoPaginaI
057900160419     C                   endsr
058000090511     ***************************************************************************
058100090511     ** Salto pagina: fine pagina.
058200090511     ***************************************************************************
058300090511     C     saltoPaginaF  BEGSR
058400160419     c                   if        *in60 AND copiaVettore
058500160419     c                   setoff                                       60
058600160419     c                   write     firmaMitt
058700160419     c                   write     firma
058800090930     c                   eval      saltato = 'S'
058900090511     c                   ENDIF
059000090511     C                   ENDSR
059100090511     ***************************************************************************
059200090511     ** Salto pagina: inizio nuova pagina.
059300090511     ***************************************************************************
059400090511     C     saltoPaginaI  BEGSR
059500160419     c                   if        *in60
059600160419     c                   setoff                                       60
059700160419     c                   write     testata
059800090512     c                   if        %rem(page : 2) = 1
059900090930     c                   clear                   saltato
060000160516     c*********          write     saltino
060100090511     c                   ENDIF
060200090511     c                   endif
060300090511     C                   ENDSR
060400011026     c*-------------------------------------------------
060500011026     c     *inzsr        begsr
060600011026     c*-------------------------------------------------
060700020304     c     *entry        plist
060800020304     c                   parm                    kpjba
060900020304     c                   movel     kpjbu         param
061000020304
061100060320     c     kfrn          klist
061200060320     c                   kfld                    socie
061300060320     c                   kfld                    fornit            8
061400060320     c                   move      *all'0'       fornit
061500080918     c     kcdp          klist
061600080918     c                   kfld                    socie
061700111223     c                   kfld                    condpag
061800080916     c     kind          klist
061900080916     c                   kfld                    frnsogg
062000080916     c                   kfld                    tipo              2
062100080916     c                   kfld                    codice            3
062200021203
062300060320     c     ktlt          klist
062400060320     c                   kfld                    tlltrn
062500060320     c                   kfld                    tlldde
062600020402
062700060320     c     ktln          klist
062800060320     c                   kfld                    tlltrn
062900060320     c                   kfld                    tlldde
063000060320     c                   kfld                    numfe             3 0
063100060320     c                   kfld                    numpro            3 0
063200060322     c                   z-add     1             numpro
063300060320
063400020218     C                   time                    w0120            14 0
063500020218     C                   move      w0120         wdat              8 0
063600020308      * data da impostare su data stampa e convalida se lancio = 'C'
063700020308     c                   move      wdat          dataeur
063800020308     c                   move      dataeur       dataiso
063900020308     c                   move      dataiso       dataoggi          8 0
064000020308      * data da elaborare
064100111223     c                   if        datae = 20120111
064200111223     c                   eval      alladata = 01012012
064300111223     c                   else
064400060321     c                   move      datae         dataiso
064500020304     c                   move      dataiso       dataeur
064600020304     c                   move      dataeur       alladata          8 0
064700111223     c                   end
064800090512      * data certa
064900160420     c                   if        datacerta > 0
065000090512     c                   move      dataCerta     dataiso
065100090512     c                   move      dataiso       dataeur
065200090512     c                   move      dataeur       certaData
065300160420     c                   end
065400011026
065500160420     c                   seton                                        60
065600020305     c                   move      *all'_'       separa          100
065700110504      *****
065800080916     c* prezzo iniziale carburante legge 133
065900090114     c                   clear                   ficn05ds
066000090114     c                   call      'FICN05R'
066100090114     c                   parm                    ficn05ds
066200090114     c                   eval      dgpi = o05gpi
066300090114     c                   z-add     D§GPIGPI      prezzo            5 3
066400090114
066500090417     c                   eval      costt = 'ALLEGATO  B   IN VIGORE DAL'
066600020412     c
066700080916     c* imposto costanti nel caso non reperisco anagrafico
066800080916     c                   eval      costia= 'Ragione sociale ___________________+
066900080916     c                             ____________________________________________'
067000080916
067100080916     c                   eval      costib= 'Indirizzo__________________________+
067200080916     c                             ____________________________________________'
067300080916
067400080916     c                   eval      costic= 'CAP______________ Città____________+
067500080916     c                             ____________________________________________'
067600080916
067700080916     c                   eval      costif= 'Recapito __________________________+
067800080916     c                             ____________________________________________'
067900080916     c                   eval      costid= 'Partita IVA _______________________+
068000080916     c                             ____________________________________________'
068100080916     c                   eval      cost8c= '|__|__|__|__|  -
068200080916     c                             |__|__|__|__|  |__|__|__|__|  |__|__|__|__| -
068300080916     c                              |__|__|__|__|  |__|__|__|__|  |__|__|__|'
068400020411     c*costanti pretariffa
068500020411     c                   eval      costa = 'Allegato tariffario al contratto +
068600060321     c                             di trasporto sottoscritto tra la società '
068700060321     c                   eval      costa2= '(di seguito denominata MITTENTE) e '
068800060321     c                   eval      costa2b= 'codice fornitore'
068900060321     c                   eval      costa3= '(di seguito denominata VETTORE)'
069000060321     c
069100060321     c                   eval      cost1 = 'Le parti come convenuto all''art.+
069200060321     c                             10 del suddetto contratto di trasporto, con+
069300111212     c                             cordano i corrispettivi per i trasporti,'
069400111206     c                   eval      cost2 = 'comprensivi delle eventu-
069500111206     c                             ali operazioni connesse al trasporto stesso,-
069600111206     c                              nonchè di quals+
069700111206     c                             iasi onere necessario per'
069800111206     c                   eval      costb = 'l''esecuzione -
069900111206     c                             del trasporto (pedaggi, diritti di +
070000111206     c                             transito, equipaggiamento del veicolo, -
070100111206     c                             tempi di carico e scarico,'
070200111206     c                   eval      costc ='ecc.) secondo la seguente tariffa :'
070300020306
070400020411     c*costanti piede
070500020411     c                   eval      cost3 = 'Le parti convengono espressamente c+
070600020411     c                             he i corrispettivi indicati nelle sopraccita+
070700020411     c                             te tariffe possano essere oggetto di'
070800020411     c                   eval      cost3b= 'variazioni, in aumento e/o diminuzi+
070900020411     c                             one, a seguito di modifiche pattuite tra le +
071000020412     c                             parti del progetto operativo'
071100020412     c                   eval      cost3c= 'originariamente concordato.'
071200080916     c                   eval      cost3d= 'Il vettore dichiara espressamente -
071300080916     c                             che i corrispettivi di cui al presente alleg-
071400080916     c                             ato sono stati concordati'
071500080916     c                   eval      cost3e= 'con il mittente sulla base del -
071600080916     c                             costo del carburante pari a euro xx,xxx/lt.'
071700090417     c                   eval      cost3f= 'Ai sensi e per gli effetti dell''ar+
071800090417     c                             t. 6, comma 3 lett. e-bis D. Lgs. 286/05, +
071900090417     c                             il contratto di trasporto '
072000090417     c                   eval      cost3g= 'di cui il presente costituisce alle+
072100090417     c                             gato, deve intendersi integrato con l''indi+
072200090417     c                             cazione dei tempi massimi '
072300090417     c                   eval      cost3h= 'di carico e scarico che le stesse +
072400090417     c                             parti indicano in complessive ore 2 (due) +
072500090417     c                             in relazione a ciascun'
072600111206     c                   eval      cost3i= 'trasporto da effettuars+
072700090417     c                             i nell''arco della giornata.'
072800080916
072900020402     c                   eval      cost4 = 'Il vettore autorizza il mittente o +
073000020402     c                              società da questi delegata ad emetter+
073100020411     c                              e alla fine di ogni mese in nome e per '
073200020411     c                   eval      cost5 = 'conto del vettore stesso una fattur+
073300111206     c                             a relativa ai trasporti ed alle o+
073400111212     c                             perazioni connesse effettuati in tale perio-
073500111206     c                             do.'
073600111206     c                   eval      cost5b= 'Tale fattura com-
073700060526     c                             prenderà anche i corrispettivi per le presta+
073800060526     c                             zioni di trasporto non previste'
073900020306
074000090417     c                   eval      cost5c= 'dal presente allegato B, alle quali-
074100060526     c                              le parti in ogni caso riconoscono la piena +
074200060526     c                             validità ed efficacia del '
074300060526
074400060526     c                   eval      cost5d= 'contenuto degli articoli 10 e 14 -
074500060526     c                             del contratto di trasporto.'
074600060526
074700020411     c                   eval      cost6 = 'La fattura verrà emessa sulla base +
074800060321     c                             dei fogli viaggio  che contengono in dettagl+
074900111206     c                             io i trasporti e le '
075000020411
075100111212     c                   eval      cost6a= 'operazioni connesse effettuati.'
075200090417     c                   eval      cost6b= 'La fattura sarà emessa con IVA +
075300090417     c                             ad esigibilità immediata.'
075400020411
075500020411     c                   eval      costi = 'L''intestazione del vettore da rip+
075600020411     c                             ortare sulla fattura emessa è:'
075700020411
075800020411     c                   eval      costie= 'Il vettore in caso di variazione de+
075900020412     c                             ll''intestazione ne darà immediata comunic+
076000020412     c                             azione al mittente.'
076100020411
076200020423     c                   eval      cost7 = 'Il presente allegato decorre d+
076300070302     c                             alla data del __________,annulla e sostituis+
076400070302     c                             ce qualsiasi precedente patto verbale o'
076500070302     c                   eval      cost7a= 'scritto relativo al corrispettivo +
076600111206     c                             per l''esecuzione di trasporti +
076700070302     c                             e costituisce parte integrante del '
076800070302     c                   eval      cost7b= 'contratto di trasporto suddetto.'
076900020412     c                   eval      cost7c= 'Le parti in ogni caso riconoscono +
077000020412     c                             la piena validità ed efficacia del contenut+
077100060321     c                             o degli articoli 10 e 14 del'
077200020412     c                   eval      cost7d='contratto di trasporto.'
077300020411     c                   eval      cost8 = 'Le parti concordano altresì che il +
077400020411     c                             pagamento dei corrispettivi fatturati verrà +
077500080918     c                             effettuato dalla mittente mediante'
077600020402
077700020411     c                   eval      cost8f= 'Il vettore in caso di variazione de+
077800020411     c                             lle coordinate bancarie ne darà +
077900020412     c                             immediata comunicazione al mittente.'
078000090417     c                   eval      cost8g='Il vettore approva espressamente il +
078100090417     c                             contenuto del presente allegato ed in partic+
078200090417     c                             olare le pattuizioni'
078300090417     c                   eval      cost8h= 'relative alla quantificazione, +
078400090417     c                             modalità e termini di pagamento dei +
078500090417     c                             corrispettivi.'
078600090417
078700060510     c                   eval      cost9 = 'Bologna li, _______________________'
078800020411     c                   eval      cost10= 'Firma del Vettore _________________+
078900060322     c                             _________________________'
079000090511     c*                  eval      cost11= 'Firma del Mittente_________________+
079100090511     c*                            _________________________'
079200020313     c                   eval      cost13= 'NON E STATA TROVATA NESSUNA TARIFFA+
079300020313     c                              VALIDA CON I REFERIMENTI RICHIESTI'
079400020313
079500011026     c                   endsr
079600011129     C*--------------------------------------------------------------------
079700110504     C* decodifica la società
079800110504     C*--------------------------------------------------------------------
079900110504     C     Decod_societa begSR
080000110504      *
080100110504     c                   move      DataE         dataiso
080200110504     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
080300110504     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
080400110504     C                   RESET                   tibsSocI0
080500110504     C                   EVAL      tibsSocI0.idSocieta = socie
080600110504     C                   EVAL      tibsSocI0.DTVALIDITA = dataiso
080700110504     C
080800110504     C                   CALL      'TIBSSOCR'
080900110504     C                   PARM      'GETANAGRAF'  prmRqsOpCode
081000110504     C                   PARM      *BLANK        prmRpyOpCode
081100110504     C                   PARM      *ZERO         prmRpyIdMsg
081200110504     C                   PARM      'TIBSSOCI0'   prmRqsFormato
081300110504     C                   PARM                    tibsSocI0
081400110504     C                   PARM                    prmRqsDataSize
081500110504     C                   PARM      'TIBSSOCO0'   prmRpyFormato
081600110504     C                   PARM                    tibsSocO0
081700110504     C                   PARM                    prmRpyDataSize
081800110504      *
081900110504     c                   clear                   ragsmit          30
082000110504     c                   if         PRMRPYIDMSG >= 0
082100110504     c                              and TIBSSOCO0.IDSOCIETA <> *blank
082200110504     c                   eval       ragsmit = tibsSocO0.RAGSOCIALE
082300110504     c                   end
082400110504      *
082500110504     C                   ENDSR
082600110504     C************************************************************
