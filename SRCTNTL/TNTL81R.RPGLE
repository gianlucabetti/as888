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
013600161118      **
013700161118     ** Inizializzo il programma.
013800161118     C                   CALL      'TIBSSOCR'
013900161118     C                   PARM      'INIT'        prmRqsOpCode
014000161118     C                   PARM                    prmRpyOpCode
014100161118     C                   PARM                    prmRpyIdMsg
014200161118      *
014300090511
014400090511     ** Stampo la copia per il vettore.
014500090511     c                   eval      copiaVettore = *on
014600090511     c
014700090511     c                   reset                   prima
014800060321     c                   move      traz1         kpdr              7 0
014900020308     c
015000060321     c     kpdr          setll     titll02l
015100020308     c                   do        *hival
015200060321     c                   read      titll02l
015300060320      *fine file
015400060321     c                   if        %eof(titll02l)
015500060320     c                   leave
015600060320     c                   endif
015700060320      *trazionista singolo
015800060320     c                   if        kpdr > 0 and kpdr <> tllpdr
015900060320     c                   leave
016000060320     c                   endif
016100060324      *trazione a costo fittizio 1 euro da non inserire in elenco
016200060324     c                   if        tllili = 1
016300060324     c                   iter
016400060324     c                   endif
016500060321      *trazionista generico 9999999 valido solo se richiesto singolarmente
016600060321     c                   if        kpdr <> 9999999 and tllpdr = 9999999
016700060321     c                   leave
016800060321     c                   endif
016900060320      *leggo traino
017000060320     c     ktlt          chain     tntlt01l
017100060320     c                   if        %found(tntlt01l)
017200060321     c                   if        datae  >= tltdde and
017300060321     c                             datae  <= tltdsc
017400060321     c                   if        primo_giro = *blank
017500060321     c                   move      tllpdr        expdr
017600060321     c                   move      'X'           primo_giro        1
017700060321     c                   endif
017800060320     c                   exsr      elabora
017900060320     c                   else
018000060320     c                   iter
018100060320     c                   endif
018200020308
018300060320     c                   else
018400060320     c                   iter
018500060320     c                   endif
018600060320      *
018700020308     c                   enddo
018800090511
018900060321     c                   if        prima <> *BLANK
019000160419      *
019100160419     C                   write     piede2
019200160419      *
019300160419     C                   exsr      finepagina
019400160419     C                   write     piede4
019500160419      *
019600160419     C                   exsr      finepagina
019700160419     C                   write     piede6
019800160419      *
019900160419     C                   exsr      finepagina
020000160419     C                   write     piede12
020100160419      *
020200160419     C                   exsr      finepagina
020300160419     C                   write     piede14
020400160419      *
020500160419     C                   exsr      finepagina
020600160419     C                   write     piede16
020700160419      *
020800160419     C                   exsr      finepagina
020900160419     C                   write     piede18
021000160419      *
021100160419     C                   exsr      finepagina
021200160419     C                   write     piede20
021300160419      *
021400160419     C                   exsr      finepagina
021500160419     C                   write     piede22
021600160419      *
021700160419     C                   exsr      finepagina
021800160419     C                   write     piede24
021900160419      *
022000160419     C                   exsr      finepagina
022100160419     C                   write     piede26
022200160419      *
022300160419     C                   exsr      finepagina
022400160419     C                   write     piede28
022500160419      *
022600160419     C                   exsr      finepagina
022700160419     C                   write     piede30
022800160419      *
022900160419     C                   exsr      finepagina
023000160419     C                   write     piede32
023100160419      *
023200160419     C                   exsr      finepagina
023300160419     C                   write     piede34
023400160419      *
023500160419     C                   exsr      finepagina
023600160419     C                   write     piede36
023700160419      *
023800160419     C                   exsr      finepagina
023900160419     C                   write     piede38
024000160419      *
024100160419     C                   exsr      finepagina
024200160419     C                   write     piede40
024300160419      *
024400160419     C                   exsr      finepagina
024500160419     C                   write     piede42
024600160419      *
024700160419     C                   exsr      finepagina
024800160419     C                   write     piede44
024900160419      *
025000160419     C                   exsr      finepagina
025100160419     C                   write     piede46
025200160419      *
025300160419     C                   exsr      finepagina
025400160419     C                   write     piede48
025500160419      *
025600160419     C                   exsr      finepagina
025700160419     C                   write     piede50
025800160419      *
025900160419     C                   exsr      finepagina
026000160419     C                   write     piede52
026100160419      *
026200160419     C                   exsr      finepagina
026300160419     C                   write     piede54
026400160419      *
026500160419     C                   exsr      finepagina
026600160419     C                   write     piede56
026700160419      *
026800160419     C                   exsr      finepagina
026900160419     C                   write     piede58
027000160419      *
027100160419     C                   exsr      finepagina
027200160419     C                   write     piede60
027300160419      *
027400160419     C                   exsr      finepagina
027500160419     C                   write     piede62
027600160419      *
027700160419     C                   exsr      finepagina
027800160419     c                   write     piede70
027900160419      *
028000160419     C                   exsr      finepagina
028100160419     c                   write     piede72
028200160419      *
028300160419     C                   exsr      finepagina
028400160419     c                   write     piede74
028500160419      *
028600160419     C                   exsr      finepagina
028700160419     c                   write     piede80
028800160419      *
028900160419     C                   exsr      finepagina
029000160419     c                   write     piede82
029100160419      *
029200160419     C                   exsr      finepagina
029300160419     c                   write     piede84
029400160419      *
029500160419     C                   exsr      finepagina
029600160419     c                   write     piede86
029700160419      *
029800160419     C                   exsr      finepagina
029900160419     c                   write     piede88
030000090930      *
030100090511     c                   if        copiaVettore
030200160419     c                   write     firmaMitt
030300160419     c                   write     Firma
030400090511     c                   ENDIF
030500090930      *
030600160516     c                   if        saltato = *blank
030700160516     c*******            write     PagBIANCA
030800160516     c*******            write     Firma
030900160516     c                   end
031000090930      *
031100090511     C                   close     tntl81p
031200090511     c                   endif
031300090511
031400161118     ** Chiudo il programma.
031500161118     C                   CALL      'TIBSSOCR'
031600161118     C                   PARM      'FINALIZE'    prmRqsOpCode
031700161118     C                   PARM                    prmRpyOpCode
031800161118     C                   PARM                    prmRpyIdMsg
031900161118      *
032000160419     c                   return
032100090511
032200020308      *--------------------------------------------------
032300020308     c     elabora       begsr
032400020308      *--------------------------------------------------
032500020411     c
032600060320     c                   if        tllpdr <> expdr and prima <> *blank
032700160419     C                   write     piede2
032800160419      *
032900160419     C                   exsr      finepagina
033000160419     C                   write     piede4
033100160419      *
033200160419     C                   exsr      finepagina
033300160419     C                   write     piede6
033400160419      *
033500160419     C                   exsr      finepagina
033600160419     C                   write     piede12
033700160419      *
033800160419     C                   exsr      finepagina
033900160419     C                   write     piede14
034000160419      *
034100160419     C                   exsr      finepagina
034200160419     C                   write     piede16
034300160419      *
034400160419     C                   exsr      finepagina
034500160419     C                   write     piede18
034600160419      *
034700160419     C                   exsr      finepagina
034800160419     C                   write     piede20
034900160419      *
035000160419     C                   exsr      finepagina
035100160419     C                   write     piede22
035200160419      *
035300160419     C                   exsr      finepagina
035400160419     C                   write     piede24
035500160419      *
035600160419     C                   exsr      finepagina
035700160419     C                   write     piede26
035800160419      *
035900160419     C                   exsr      finepagina
036000160419     C                   write     piede28
036100160419      *
036200160419     C                   exsr      finepagina
036300160419     C                   write     piede30
036400160419      *
036500160419     C                   exsr      finepagina
036600160419     C                   write     piede32
036700160419      *
036800160419     C                   exsr      finepagina
036900160419     C                   write     piede34
037000160419      *
037100160419     C                   exsr      finepagina
037200160419     C                   write     piede36
037300160419      *
037400160419     C                   exsr      finepagina
037500160419     C                   write     piede38
037600160419      *
037700160419     C                   exsr      finepagina
037800160419     C                   write     piede40
037900160419      *
038000160419     C                   exsr      finepagina
038100160419     C                   write     piede42
038200160419      *
038300160419     C                   exsr      finepagina
038400160419     C                   write     piede44
038500160419      *
038600160419     C                   exsr      finepagina
038700160419     C                   write     piede46
038800160419      *
038900160419     C                   exsr      finepagina
039000160419     C                   write     piede48
039100160419      *
039200160419     C                   exsr      finepagina
039300160419     C                   write     piede50
039400160419      *
039500160419     C                   exsr      finepagina
039600160419     C                   write     piede52
039700160419      *
039800160419     C                   exsr      finepagina
039900160419     C                   write     piede54
040000160419      *
040100160419     C                   exsr      finepagina
040200160419     C                   write     piede56
040300160419      *
040400160419     C                   exsr      finepagina
040500160419     C                   write     piede58
040600160419      *
040700160419     C                   exsr      finepagina
040800160419     C                   write     piede60
040900160419      *
041000160419     C                   exsr      finepagina
041100160419     C                   write     piede62
041200160419      *
041300160419     C                   exsr      finepagina
041400160419     c                   write     piede70
041500160419      *
041600160419     C                   exsr      finepagina
041700160419     c                   write     piede72
041800160419      *
041900160419     C                   exsr      finepagina
042000160419     c                   write     piede74
042100160419      *
042200160419     C                   exsr      finepagina
042300160419     c                   write     piede80
042400160419      *
042500160419     C                   exsr      finepagina
042600160419     c                   write     piede82
042700160419      *
042800160419     C                   exsr      finepagina
042900160419     c                   write     piede84
043000160419      *
043100160419     C                   exsr      finepagina
043200160419     c                   write     piede86
043300160419      *
043400160419     C                   exsr      finepagina
043500160419     c                   write     piede88
043600090930      *
043700090511     c                   if        copiaVettore
043800160419     c                   write     firmaMitt
043900160419     c                   write     Firma
044000090511     c                   ENDIF
044100060321     c                   move      tllpdr        expdr             7 0
044200060321     c                   clear                   prima             1
044300090511     C                   close     tntl81p
044400060320     c                   endif
044500060320
044600060320     c                   if        prima = *blank
044700090511     C                   OPEN      tntl81p
044800060321     c                   z-add     0             page
044900060321     c                   move      expdr         fornit
045000080916     c* imposto costanti nel caso non reperisco anagrafico
045100080916     c                   eval      costia= 'Ragione sociale ___________________+
045200080916     c                             ____________________________________________'
045300080916
045400080916     c                   eval      costib= 'Indirizzo__________________________+
045500080916     c                             ____________________________________________'
045600080916
045700080916     c                   eval      costic= 'CAP______________ Città____________+
045800080916     c                             ____________________________________________'
045900080916
046000080916     c                   eval      costif= 'Recapito __________________________+
046100080916     c                             ____________________________________________'
046200080916     c                   eval      costid= 'Partita IVA _______________________+
046300080916     c                             ____________________________________________'
046400080916     c                   eval      cost8c= '|__|__|__|__|  -
046500080916     c                             |__|__|__|__|  |__|__|__|__|  |__|__|__|__| -
046600080916     c                              |__|__|__|__|  |__|__|__|__|  |__|__|__|'
046700060321      * decodifico fornitore
046800060321     C                   clear                   ragsvet
046900080918     c                   movel     *all'_'       cdpdescr
047000060321     C     kfrn          chain     anfrn01l
047100060321     c                   if        %found(anfrn01l)
047200111223     c                   if        datae = 20120111 and frncondpag = 'B70'
047300111223     c                   eval      condpag = 'B55'
047400111223     c                   else
047500111223     c                   eval      condpag = frncondpag
047600111223     c                   end
047700080918     C     kcdp          chain     ancdp01l
047800060321     C     frnsogg       chain     ansog01l
047900060321     c                   if        %found(ansog01l)
048000080919     C                   MOVEL     sogdes        ragsvet          44
048100060321     c                   endif
048200060321     c                   endif
048300080918     c                   eval      cost8b= %trimr(cdpdescr) +' '+
048400080918     c                             'disposto su codice IBAN:'
048500080916     c* indirizzo
048600080916     c                   clear                   tipo
048700080916     c                   clear                   codice
048800080916     C     Kind          CHAIN     anind01l
048900080916     c                   if        %found(anind01l)
049000080916     C                   eval      costia = 'Ragione sociale ' + sogdes
049100080916     C                   eval      costib = 'Indirizzo ' + indindriz
049200080916     c                   if        indprov <> ' '
049300080916     C                   eval      locemi = %trimr(indlocalit)+' ('+indprov+')'
049400080916     c                   else
049500080916     C                   eval      locemi = indlocalit
049600080916     c                   end
049700080916     C                   eval      costic = 'CAP ' + indcap + ' Città ' +
049800080916     c                             locemi
049900080916     c                   end
050000080916     C                   eval      costid = 'Partita IVA ' + sogpartiva
050100091218      **
050200091218      **  oltre alla P.IVA con la nuova legge introdotto dal gennaio 2010
050300091218      **   si devono esporre il Capitale Sociale il Cod.Fiscale e il Reg.Imprese
050400091218      **
050500091218      **  Aggiunge il Codice Fiscale
050600091218     c                   if        sogcdfisc <> ' ' and indprov <> ' '
050700091218     c                   eval      costiD = %Trim(costiD) +
050800091218     c                             ' C.F.-R.IMPRESE ' + %trimr(indprov) +
050900091218     C                             ': ' + %trimr(sogcdfisc)
051000091218     c                   end
051100091218      **
051200091218     c                   if        indtelex <> ' ' and
051300091218     c                             %subst(indtelex: 1: 8) = 'CAP.SOC.'
051400091218     c                   eval      costiD = %Trim(costiD) + ' ' + indtelex
051500091218     c
051600091218     c                   end
051700091218      **
051800080916     c* indirizzo recapito
051900080916     C                   SETOFF                                       89
052000080916     c                   eval      tipo ='IF'
052100080916     c                   eval      codice ='000'
052200080916     C     Kind          CHAIN     anind01l
052300080916     c                   if        %found(anind01l)
052400080916     C                   SETON                                        89
052500080916     c                   eval      costif= 'Recapito ' + sogdes
052600080916     C                   eval      costiH = indindriz
052700080916     c                   if        indprov <> ' '
052800080916     C                   eval      locemi = %trimr(indlocalit)+' ('+indprov+')'
052900080916     c                   else
053000080916     C                   eval      locemi = indlocalit
053100080916     c                   end
053200080916     C                   eval      costiI = indcap + ' ' +
053300080916     c                             locemi
053400080916     c                   end
053500080916     c* reperisco IBAN
053600080916     C                   RESET                   DVCOBADS
053700080916     C                   RESET                   CBAIBANDS
053800080916
053900080916     C                   EVAL      CoBSoc = socie
054000080916     C                   EVAL      CoBCOF = 'F'
054100080916     C                   EVAL      CoBKsc = fornit
054200080916     C                   CALL      'DVCOBA'
054300080916     C                   PARM                    DVCOBADS
054400080916A0846C                   PARM                    CBAIBANDS
054500080916     C                   IF        CoBErr = *OFF
054600080916     C                   EVAL      cost8c = CBaStato + CbaCheck
054700080916     C                             + %TRIMR(CBaCoorI)
054800080916     c                   end
054900110504      *
055000110504     C*** SPOSTATA QUI LA DECODIFICA DELLA RAGIONE SOCIALE MITTENTE
055100110504     c                   exsr      decod_societa
055200110504      *
055300060321      *stampa testata allegato
055400160420     c                   setoff                                       60
055500160419     c                   write     testata
055600160419     c                   write     testo
055700060320     c                   move      'X'           prima
055800020306     c                   endif
055900160419      *--
056000160419     C                   exsr      finepagina
056100160419      *--
056200060320      *decodifica partenza
056300060320     c     tlttfp        chain     azorg01l
056400060320     c                   if        %found(azorg01l)
056500060320     c                   movel     orgdes        partenza         20
056600060320     c                   else
056700060320     c                   clear                   partenza
056800060320     c                   endif
056900060320      *decodifica arrivo
057000060320     c     tlttfa        chain     azorg01l
057100060320     c                   if        %found(azorg01l)
057200060321     c                   movel     orgdes        arrivo           20
057300060320     c                   else
057400060321     c                   clear                   arrivo
057500060320     c                   endif
057600160419      *--
057700060320      * stampa riga listino attiva
057800160419     c                   write     dettaglio
057900060320      *reperimento nota
058000060320     c     ktln          chain     tntln01l
058100060320     c                   if        %found(tntln01l)
058200160419     c                   write     dettanote
058300060320     c                   endif
058400060320
058500020308     c                   endsr
058600160419     ***************************************************************************
058700160419     **  Controlla OVERFLOW
058800160419     ***************************************************************************
058900160419     c     finepagina    begsr
059000160419     C                   exsr      saltoPaginaF
059100160419     C                   exsr      saltoPaginaI
059200160419     C                   endsr
059300090511     ***************************************************************************
059400090511     ** Salto pagina: fine pagina.
059500090511     ***************************************************************************
059600090511     C     saltoPaginaF  BEGSR
059700160419     c                   if        *in60 AND copiaVettore
059800160419     c                   setoff                                       60
059900160419     c                   write     firmaMitt
060000160419     c                   write     firma
060100090930     c                   eval      saltato = 'S'
060200090511     c                   ENDIF
060300090511     C                   ENDSR
060400090511     ***************************************************************************
060500090511     ** Salto pagina: inizio nuova pagina.
060600090511     ***************************************************************************
060700090511     C     saltoPaginaI  BEGSR
060800160419     c                   if        *in60
060900160419     c                   setoff                                       60
061000160419     c                   write     testata
061100090512     c                   if        %rem(page : 2) = 1
061200090930     c                   clear                   saltato
061300160516     c*********          write     saltino
061400090511     c                   ENDIF
061500090511     c                   endif
061600090511     C                   ENDSR
061700011026     c*-------------------------------------------------
061800011026     c     *inzsr        begsr
061900011026     c*-------------------------------------------------
062000020304     c     *entry        plist
062100020304     c                   parm                    kpjba
062200020304     c                   movel     kpjbu         param
062300020304
062400060320     c     kfrn          klist
062500060320     c                   kfld                    socie
062600060320     c                   kfld                    fornit            8
062700060320     c                   move      *all'0'       fornit
062800080918     c     kcdp          klist
062900080918     c                   kfld                    socie
063000111223     c                   kfld                    condpag
063100080916     c     kind          klist
063200080916     c                   kfld                    frnsogg
063300080916     c                   kfld                    tipo              2
063400080916     c                   kfld                    codice            3
063500021203
063600060320     c     ktlt          klist
063700060320     c                   kfld                    tlltrn
063800060320     c                   kfld                    tlldde
063900020402
064000060320     c     ktln          klist
064100060320     c                   kfld                    tlltrn
064200060320     c                   kfld                    tlldde
064300060320     c                   kfld                    numfe             3 0
064400060320     c                   kfld                    numpro            3 0
064500060322     c                   z-add     1             numpro
064600060320
064700020218     C                   time                    w0120            14 0
064800020218     C                   move      w0120         wdat              8 0
064900020308      * data da impostare su data stampa e convalida se lancio = 'C'
065000020308     c                   move      wdat          dataeur
065100020308     c                   move      dataeur       dataiso
065200020308     c                   move      dataiso       dataoggi          8 0
065300020308      * data da elaborare
065400111223     c                   if        datae = 20120111
065500111223     c                   eval      alladata = 01012012
065600111223     c                   else
065700060321     c                   move      datae         dataiso
065800020304     c                   move      dataiso       dataeur
065900020304     c                   move      dataeur       alladata          8 0
066000111223     c                   end
066100090512      * data certa
066200160420     c                   if        datacerta > 0
066300090512     c                   move      dataCerta     dataiso
066400090512     c                   move      dataiso       dataeur
066500090512     c                   move      dataeur       certaData
066600160420     c                   end
066700011026
066800160420     c                   seton                                        60
066900020305     c                   move      *all'_'       separa          100
067000110504      *****
067100080916     c* prezzo iniziale carburante legge 133
067200090114     c                   clear                   ficn05ds
067300090114     c                   call      'FICN05R'
067400090114     c                   parm                    ficn05ds
067500090114     c                   eval      dgpi = o05gpi
067600090114     c                   z-add     D§GPIGPI      prezzo            5 3
067700090114
067800090417     c                   eval      costt = 'ALLEGATO  B   IN VIGORE DAL'
067900020412     c
068000080916     c* imposto costanti nel caso non reperisco anagrafico
068100080916     c                   eval      costia= 'Ragione sociale ___________________+
068200080916     c                             ____________________________________________'
068300080916
068400080916     c                   eval      costib= 'Indirizzo__________________________+
068500080916     c                             ____________________________________________'
068600080916
068700080916     c                   eval      costic= 'CAP______________ Città____________+
068800080916     c                             ____________________________________________'
068900080916
069000080916     c                   eval      costif= 'Recapito __________________________+
069100080916     c                             ____________________________________________'
069200080916     c                   eval      costid= 'Partita IVA _______________________+
069300080916     c                             ____________________________________________'
069400080916     c                   eval      cost8c= '|__|__|__|__|  -
069500080916     c                             |__|__|__|__|  |__|__|__|__|  |__|__|__|__| -
069600080916     c                              |__|__|__|__|  |__|__|__|__|  |__|__|__|'
069700020411     c*costanti pretariffa
069800020411     c                   eval      costa = 'Allegato tariffario al contratto +
069900060321     c                             di trasporto sottoscritto tra la società '
070000060321     c                   eval      costa2= '(di seguito denominata MITTENTE) e '
070100060321     c                   eval      costa2b= 'codice fornitore'
070200060321     c                   eval      costa3= '(di seguito denominata VETTORE)'
070300060321     c
070400060321     c                   eval      cost1 = 'Le parti come convenuto all''art.+
070500060321     c                             10 del suddetto contratto di trasporto, con+
070600111212     c                             cordano i corrispettivi per i trasporti,'
070700111206     c                   eval      cost2 = 'comprensivi delle eventu-
070800111206     c                             ali operazioni connesse al trasporto stesso,-
070900111206     c                              nonchè di quals+
071000111206     c                             iasi onere necessario per'
071100111206     c                   eval      costb = 'l''esecuzione -
071200111206     c                             del trasporto (pedaggi, diritti di +
071300111206     c                             transito, equipaggiamento del veicolo, -
071400111206     c                             tempi di carico e scarico,'
071500111206     c                   eval      costc ='ecc.) secondo la seguente tariffa :'
071600020306
071700020411     c*costanti piede
071800020411     c                   eval      cost3 = 'Le parti convengono espressamente c+
071900020411     c                             he i corrispettivi indicati nelle sopraccita+
072000020411     c                             te tariffe possano essere oggetto di'
072100020411     c                   eval      cost3b= 'variazioni, in aumento e/o diminuzi+
072200020411     c                             one, a seguito di modifiche pattuite tra le +
072300020412     c                             parti del progetto operativo'
072400020412     c                   eval      cost3c= 'originariamente concordato.'
072500080916     c                   eval      cost3d= 'Il vettore dichiara espressamente -
072600080916     c                             che i corrispettivi di cui al presente alleg-
072700080916     c                             ato sono stati concordati'
072800080916     c                   eval      cost3e= 'con il mittente sulla base del -
072900080916     c                             costo del carburante pari a euro xx,xxx/lt.'
073000090417     c                   eval      cost3f= 'Ai sensi e per gli effetti dell''ar+
073100090417     c                             t. 6, comma 3 lett. e-bis D. Lgs. 286/05, +
073200090417     c                             il contratto di trasporto '
073300090417     c                   eval      cost3g= 'di cui il presente costituisce alle+
073400090417     c                             gato, deve intendersi integrato con l''indi+
073500090417     c                             cazione dei tempi massimi '
073600090417     c                   eval      cost3h= 'di carico e scarico che le stesse +
073700090417     c                             parti indicano in complessive ore 2 (due) +
073800090417     c                             in relazione a ciascun'
073900111206     c                   eval      cost3i= 'trasporto da effettuars+
074000090417     c                             i nell''arco della giornata.'
074100080916
074200020402     c                   eval      cost4 = 'Il vettore autorizza il mittente o +
074300020402     c                              società da questi delegata ad emetter+
074400020411     c                              e alla fine di ogni mese in nome e per '
074500020411     c                   eval      cost5 = 'conto del vettore stesso una fattur+
074600111206     c                             a relativa ai trasporti ed alle o+
074700111212     c                             perazioni connesse effettuati in tale perio-
074800111206     c                             do.'
074900111206     c                   eval      cost5b= 'Tale fattura com-
075000060526     c                             prenderà anche i corrispettivi per le presta+
075100060526     c                             zioni di trasporto non previste'
075200020306
075300090417     c                   eval      cost5c= 'dal presente allegato B, alle quali-
075400060526     c                              le parti in ogni caso riconoscono la piena +
075500060526     c                             validità ed efficacia del '
075600060526
075700060526     c                   eval      cost5d= 'contenuto degli articoli 10 e 14 -
075800060526     c                             del contratto di trasporto.'
075900060526
076000020411     c                   eval      cost6 = 'La fattura verrà emessa sulla base +
076100060321     c                             dei fogli viaggio  che contengono in dettagl+
076200111206     c                             io i trasporti e le '
076300020411
076400111212     c                   eval      cost6a= 'operazioni connesse effettuati.'
076500090417     c                   eval      cost6b= 'La fattura sarà emessa con IVA +
076600090417     c                             ad esigibilità immediata.'
076700020411
076800020411     c                   eval      costi = 'L''intestazione del vettore da rip+
076900020411     c                             ortare sulla fattura emessa è:'
077000020411
077100020411     c                   eval      costie= 'Il vettore in caso di variazione de+
077200020412     c                             ll''intestazione ne darà immediata comunic+
077300020412     c                             azione al mittente.'
077400020411
077500020423     c                   eval      cost7 = 'Il presente allegato decorre d+
077600070302     c                             alla data del __________,annulla e sostituis+
077700070302     c                             ce qualsiasi precedente patto verbale o'
077800070302     c                   eval      cost7a= 'scritto relativo al corrispettivo +
077900111206     c                             per l''esecuzione di trasporti +
078000070302     c                             e costituisce parte integrante del '
078100070302     c                   eval      cost7b= 'contratto di trasporto suddetto.'
078200020412     c                   eval      cost7c= 'Le parti in ogni caso riconoscono +
078300020412     c                             la piena validità ed efficacia del contenut+
078400060321     c                             o degli articoli 10 e 14 del'
078500020412     c                   eval      cost7d='contratto di trasporto.'
078600020411     c                   eval      cost8 = 'Le parti concordano altresì che il +
078700020411     c                             pagamento dei corrispettivi fatturati verrà +
078800080918     c                             effettuato dalla mittente mediante'
078900020402
079000020411     c                   eval      cost8f= 'Il vettore in caso di variazione de+
079100020411     c                             lle coordinate bancarie ne darà +
079200020412     c                             immediata comunicazione al mittente.'
079300090417     c                   eval      cost8g='Il vettore approva espressamente il +
079400090417     c                             contenuto del presente allegato ed in partic+
079500090417     c                             olare le pattuizioni'
079600090417     c                   eval      cost8h= 'relative alla quantificazione, +
079700090417     c                             modalità e termini di pagamento dei +
079800090417     c                             corrispettivi.'
079900090417
080000060510     c                   eval      cost9 = 'Bologna li, _______________________'
080100020411     c                   eval      cost10= 'Firma del Vettore _________________+
080200060322     c                             _________________________'
080300090511     c*                  eval      cost11= 'Firma del Mittente_________________+
080400090511     c*                            _________________________'
080500020313     c                   eval      cost13= 'NON E STATA TROVATA NESSUNA TARIFFA+
080600020313     c                              VALIDA CON I REFERIMENTI RICHIESTI'
080700020313
080800011026     c                   endsr
080900011129     C*--------------------------------------------------------------------
081000110504     C* decodifica la società
081100110504     C*--------------------------------------------------------------------
081200110504     C     Decod_societa begSR
081300110504      *
081400110504     c                   move      DataE         dataiso
081500110504     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
081600110504     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
081700110504     C                   RESET                   tibsSocI0
081800110504     C                   EVAL      tibsSocI0.idSocieta = socie
081900110504     C                   EVAL      tibsSocI0.DTVALIDITA = dataiso
082000110504     C
082100110504     C                   CALL      'TIBSSOCR'
082200110504     C                   PARM      'GETANAGRAF'  prmRqsOpCode
082300110504     C                   PARM      *BLANK        prmRpyOpCode
082400110504     C                   PARM      *ZERO         prmRpyIdMsg
082500110504     C                   PARM      'TIBSSOCI0'   prmRqsFormato
082600110504     C                   PARM                    tibsSocI0
082700110504     C                   PARM                    prmRqsDataSize
082800110504     C                   PARM      'TIBSSOCO0'   prmRpyFormato
082900110504     C                   PARM                    tibsSocO0
083000110504     C                   PARM                    prmRpyDataSize
083100110504      *
083200161118     c                   clear                   ragsmit
083300110504     c                   if         PRMRPYIDMSG >= 0
083400110504     c                              and TIBSSOCO0.IDSOCIETA <> *blank
083500110504     c                   eval       ragsmit = tibsSocO0.RAGSOCIALE
083600110504     c                   end
083700110504      *
083800110504     C                   ENDSR
083900110504     C************************************************************
