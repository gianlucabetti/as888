000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400020305
000500020305     h* OVRPRTF FILE(QSYSPRT) LPI(6) CPI(15) PAGRTT(0)
000600020305
000700060320     Ftitll02l  if   e           k disk
000800060320     Ftntlt01l  if   e           k disk
000900060320     Ftntln01l  if   e           k disk
001000060320     Fanfrn01l  if   e           k disk
001001080918     Fancdp01l  if   e           k disk
001100060321     Fansog01l  if   e           k disk
001200080916     Ftntbe01l  if   e           k disk
001300080916     Fanind01l  if   e           k disk
001400060320     Fazorg01l  if   e           k disk
001500060321     Fansif01l  if   e           k disk
001600020308     Fqsysprt   O    F  132        PRINTER prtctl(prtds) usropn
001700011026
001800020304     dkpjba          e ds
001900020304
002000020304     dparam            ds
002100060320     D  datae                         8s 0
002200060321     D  traz1                         8  0
002300060320     D  socie                         3
002400020304
002500020402     d modo            s              8
002600020114     d base            s              1
002700020412     d cost3b          s            120
002800020412     d cost3c          s            120
002900080916     d cost3D          s            120
003000080916     d cost3E          s            120
003100060421     d costt           s             27
003200060321     d costa           s             74
003300060321     d costa2          s             35
003400060321     d costa2b         s             16
003500020411     d costa3          s            120
003600020411     d costa4          s            120
003700020411     d costb           s            120
003800020411     d cost1           s            120
003900020411     d cost2           s            120
004000020306     d cost3           s            120
004100020306     d cost4           s            120
004200020306     d cost5           s            120
004300020411     d cost5b          s            120
004400060526     d cost5c          s            120
004500060526     d cost5d          s            120
004600020306     d cost6           s            120
004700020411     d cost6a          s            120
004800020411     d costi           s            120
004900020411     d costia          s            120
005000020411     d costib          s            120
005100020411     d costic          s            120
005200020411     d costid          s            120
005300020411     d costie          s            120
005400020423     d costif          s            120
005500080916     d costiH          s            120
005600080916     d costiI          s            120
005700020306     d cost7           s            120
005800070302     d cost7a          s            120
005900020411     d cost7b          s            120
006000020412     d cost7c          s            120
006100020412     d cost7d          s            120
006200020306     d cost8           s            120
006300020402     d cost8b          s            120
006400020402     d cost8c          s            120
006500020402     d cost8f          s            120
006600060510     d cost9           s             35
006700060322     d cost10          s             60
006800060322     d cost11          s             60
006900020313     d cost13          s             70
007000020305     d dataiso         s               d   datfmt(*iso)
007100020305     d dataeur         s               d   datfmt(*eur)
007200020117     d prtds           ds
007300020117     d   spab                         3  0
007400020117     d   spaa                         3  0
007500020117     d   skab                         3  0
007600020117     d   skaa                         3  0
007700020117     d   line                         3  0
007800020308     D CMD             S             80    DIM(2) CTDATA PERRCD(1)              OVRPRTF
007900080916     d locemi          s             40
008000080916     D DVCOBADS      E DS
008100080916     D                                     INZ
008200080916     D CBAIBANDS     E DS
008300080916     ddgpi           e ds
008400000000     C*---------------------------------------------------------------*
008500020304      *
008600060321     c                   move      traz1         kpdr              7 0
008700020308     c
008800060321     c     kpdr          setll     titll02l
008900020308     c                   do        *hival
009000060321     c                   read      titll02l
009100060320      *fine file
009200060321     c                   if        %eof(titll02l)
009300060320     c                   leave
009400060320     c                   endif
009500060320      *trazionista singolo
009600060320     c                   if        kpdr > 0 and kpdr <> tllpdr
009700060320     c                   leave
009800060320     c                   endif
009900060324      *trazione a costo fittizio 1 euro da non inserire in elenco
010000060324     c                   if        tllili = 1
010100060324     c                   iter
010200060324     c                   endif
010300060321      *trazionista generico 9999999 valido solo se richiesto singolarmente
010400060321     c                   if        kpdr <> 9999999 and tllpdr = 9999999
010500060321     c                   leave
010600060321     c                   endif
010700060320      *leggo traino
010800060320     c     ktlt          chain     tntlt01l
010900060320     c                   if        %found(tntlt01l)
011000060321     c                   if        datae  >= tltdde and
011100060321     c                             datae  <= tltdsc
011200060321     c                   if        primo_giro = *blank
011300060321     c                   move      tllpdr        expdr
011400060321     c                   move      'X'           primo_giro        1
011500060321     c                   endif
011600060320     c                   exsr      elabora
011700060320     c                   else
011800060320     c                   iter
011900060320     c                   endif
012000020308
012100060320     c                   else
012200060320     c                   iter
012300060320     c                   endif
012400060320      *
012500020308     c                   enddo
012600060321     c                   if        prima <> *BLANK
012700060321     C                   except    piede
012800060322     c                   if        line > 58
012900060322     c                   except    testata
013000060322     c                   endif
013100060322     c                   except    piede2
013200060324     c                   if        line > 58
013300060324     c                   except    testata
013400060324     c                   endif
013500060324     c                   except    piede3
013600060321     c                   endif
013700020308     C* ESEGUO L'OVERRIDE DEL FILE DI STAMPA
013800020308     C                   close     qsysprt
013900020308     C                   MOVEL(p)  CMD(2)        COMMAN           80
014000020308     C                   Z-ADD     80            LUNG             15 5
014100020308     C                   CALL      'QCMDEXC'
014200020308     C                   PARM                    COMMAN
014300020308     C                   PARM                    LUNG
014400020304
014500020308     c                   seton                                        lr
014600020308      *--------------------------------------------------
014700020308     c     elabora       begsr
014800020308      *--------------------------------------------------
014900020411     c
015000060320     c                   if        tllpdr <> expdr and prima <> *blank
015100060320     c                   except    piede
015200060322     c                   if        line > 58
015300060322     c                   except    testata
015400060322     c                   endif
015500060322     c                   except    piede2
015600060324     c                   if        line > 58
015700060324     c                   except    testata
015800060324     c                   endif
015900060324     c                   except    piede3
016000060321     c                   move      tllpdr        expdr             7 0
016100060321     c                   clear                   prima             1
016200060320     c                   endif
016300060320
016400060320     c                   if        prima = *blank
016500060321     c                   z-add     0             page
016600060321     c                   move      expdr         fornit
016700080916     c* imposto costanti nel caso non reperisco anagrafico
016800080916     c                   eval      costia= 'Ragione sociale ___________________+
016900080916     c                             ____________________________________________'
017000080916
017100080916     c                   eval      costib= 'Indirizzo__________________________+
017200080916     c                             ____________________________________________'
017300080916
017400080916     c                   eval      costic= 'CAP______________ Città____________+
017500080916     c                             ____________________________________________'
017600080916
017700080916     c                   eval      costif= 'Recapito __________________________+
017800080916     c                             ____________________________________________'
017900080916     c                   eval      costid= 'Partita IVA _______________________+
018000080916     c                             ____________________________________________'
018100080916     c                   eval      cost8c= '|__|__|__|__|  -
018200080916     c                             |__|__|__|__|  |__|__|__|__|  |__|__|__|__| -
018300080916     c                              |__|__|__|__|  |__|__|__|__|  |__|__|__|'
018400060321      * decodifico fornitore
018500060321     C                   clear                   ragsvet
018502080918     c                   movel     *all'_'       cdpdescr
018600060321     C     kfrn          chain     anfrn01l
018700060321     c                   if        %found(anfrn01l)
018701080918     C     kcdp          chain     ancdp01l
018800060321     C     frnsogg       chain     ansog01l
018900060321     c                   if        %found(ansog01l)
019000060321     C                   MOVEL     sogdes        ragsvet          40
019100060321     c                   endif
019200060321     c                   endif
019201080918     c                   eval      cost8b= %trimr(cdpdescr) +' '+
019202080918     c                             'disposto su codice IBAN:'
019300080916     c* indirizzo
019400080916     c                   clear                   tipo
019500080916     c                   clear                   codice
019600080916     C     Kind          CHAIN     anind01l
019700080916     c                   if        %found(anind01l)
019800080916     C                   eval      costia = 'Ragione sociale ' + sogdes
019900080916     C                   eval      costib = 'Indirizzo ' + indindriz
020000080916     c                   if        indprov <> ' '
020100080916     C                   eval      locemi = %trimr(indlocalit)+' ('+indprov+')'
020200080916     c                   else
020300080916     C                   eval      locemi = indlocalit
020400080916     c                   end
020500080916     C                   eval      costic = 'CAP ' + indcap + ' Città ' +
020600080916     c                             locemi
020700080916     c                   end
020800080916     C                   eval      costid = 'Partita IVA ' + sogpartiva
020900080916     c* indirizzo recapito
021000080916     C                   SETOFF                                       89
021100080916     c                   eval      tipo ='IF'
021200080916     c                   eval      codice ='000'
021300080916     C     Kind          CHAIN     anind01l
021400080916     c                   if        %found(anind01l)
021500080916     C                   SETON                                        89
021600080916     c                   eval      costif= 'Recapito ' + sogdes
021700080916     C                   eval      costiH = indindriz
021800080916     c                   if        indprov <> ' '
021900080916     C                   eval      locemi = %trimr(indlocalit)+' ('+indprov+')'
022000080916     c                   else
022100080916     C                   eval      locemi = indlocalit
022200080916     c                   end
022300080916     C                   eval      costiI = indcap + ' ' +
022400080916     c                             locemi
022500080916     c                   end
022600080916     c* reperisco IBAN
022700080916     C                   RESET                   DVCOBADS
022800080916     C                   RESET                   CBAIBANDS
022900080916
023000080916     C                   EVAL      CoBSoc = socie
023100080916     C                   EVAL      CoBCOF = 'F'
023200080916     C                   EVAL      CoBKsc = fornit
023300080916     C                   CALL      'DVCOBA'
023400080916     C                   PARM                    DVCOBADS
023500080916A0846C                   PARM                    CBAIBANDS
023600080916     C                   IF        CoBErr = *OFF
023700080916     C                   EVAL      cost8c = CBaStato + CbaCheck
023800080916     C                             + %TRIMR(CBaCoorI)
023900080916     c                   end
024000060321      *stampa testata allegato
024100020306     c                   except    testata
024200060320     c                   except    testo
024300060320     c                   move      'X'           prima
024400020306     c                   endif
024500060322     c                   if        line > 58
024600060320     c                   except    testata
024700060320     c                   endif
024800060320      *decodifica partenza
024900060320     c     tlttfp        chain     azorg01l
025000060320     c                   if        %found(azorg01l)
025100060320     c                   movel     orgdes        partenza         20
025200060320     c                   else
025300060320     c                   clear                   partenza
025400060320     c                   endif
025500060320      *decodifica arrivo
025600060320     c     tlttfa        chain     azorg01l
025700060320     c                   if        %found(azorg01l)
025800060321     c                   movel     orgdes        arrivo           20
025900060320     c                   else
026000060321     c                   clear                   arrivo
026100060320     c                   endif
026200060320      * stampa riga listino attiva
026300060320     c                   except    dettaglio
026400060320      *reperimento nota
026500060320     c     ktln          chain     tntln01l
026600060320     c                   if        %found(tntln01l)
026700060320     c                   except    dettanote
026800060320     c                   endif
026900060320
027000020308     c                   endsr
027100020304
027200011026     c*-------------------------------------------------
027300011026     c     *inzsr        begsr
027400011026     c*-------------------------------------------------
027500020304     c     *entry        plist
027600020304     c                   parm                    kpjba
027700020304     c                   movel     kpjbu         param
027800020304
027900060320     c     kfrn          klist
028000060320     c                   kfld                    socie
028100060320     c                   kfld                    fornit            8
028200060320     c                   move      *all'0'       fornit
028201080918     c     kcdp          klist
028202080918     c                   kfld                    socie
028203080918     c                   kfld                    frncondpag
028300080916     c     kind          klist
028400080916     c                   kfld                    frnsogg
028500080916     c                   kfld                    tipo              2
028600080916     c                   kfld                    codice            3
028700021203
028800060320     c     ktlt          klist
028900060320     c                   kfld                    tlltrn
029000060320     c                   kfld                    tlldde
029100020402
029200060320     c     ktln          klist
029300060320     c                   kfld                    tlltrn
029400060320     c                   kfld                    tlldde
029500060320     c                   kfld                    numfe             3 0
029600060320     c                   kfld                    numpro            3 0
029700060322     c                   z-add     1             numpro
029800080916     c     ktbe          klist
029900080916     c                   kfld                    tbecod
030000080916     c                   kfld                    tbeke1
030100080916     c                   eval      tbecod = 'GPI'
030200080916     c                   eval      tbeke1 = '1'
030300060320
030400020218     C                   time                    w0120            14 0
030500020218     C                   move      w0120         wdat              8 0
030600020308      * data da impostare su data stampa e convalida se lancio = 'C'
030700020308     c                   move      wdat          dataeur
030800020308     c                   move      dataeur       dataiso
030900020308     c                   move      dataiso       dataoggi          8 0
031000020308      * data da elaborare
031100060321     c                   move      datae         dataiso
031200020304     c                   move      dataiso       dataeur
031300020304     c                   move      dataeur       alladata          8 0
031400011026
031500020117     c                   z-add     99            line
031600020305     c                   move      *all'_'       separa          100
031700060321      * ragione sociale società mittente
031800060321     c     socie         chain     ansif01l
031900060321     c                   if        %found(ansif01l)
032000060321     c                   movel     sifrgs        ragsmit          30
032100060321     c                   endif
032200080916     c* prezzo iniziale carburante legge 133
032300080916     c     ktbe          chain     tntbe01l
032400080916     c                   if        %found(tntbe01l) and tbeatb = ' '
032500080916     c                   movel     tbeuni        dgpi
032600080916     c                   z-add     d§gpigpi      prezzo            5 3
032700080916     c                   else
032800080916     c                   eval      prezzo = 1,518
032900080916     c                   end
033000020304
033100060421     c                   eval      costt = 'ALLEGATO  C   IN VIGORE DAL'
033200020412     c
033300080916     c* imposto costanti nel caso non reperisco anagrafico
033400080916     c                   eval      costia= 'Ragione sociale ___________________+
033500080916     c                             ____________________________________________'
033600080916
033700080916     c                   eval      costib= 'Indirizzo__________________________+
033800080916     c                             ____________________________________________'
033900080916
034000080916     c                   eval      costic= 'CAP______________ Città____________+
034100080916     c                             ____________________________________________'
034200080916
034300080916     c                   eval      costif= 'Recapito __________________________+
034400080916     c                             ____________________________________________'
034500080916     c                   eval      costid= 'Partita IVA _______________________+
034600080916     c                             ____________________________________________'
034700080916     c                   eval      cost8c= '|__|__|__|__|  -
034800080916     c                             |__|__|__|__|  |__|__|__|__|  |__|__|__|__| -
034900080916     c                              |__|__|__|__|  |__|__|__|__|  |__|__|__|'
035000020411     c*costanti pretariffa
035100020411     c                   eval      costa = 'Allegato tariffario al contratto +
035200060321     c                             di trasporto sottoscritto tra la società '
035300060321     c                   eval      costa2= '(di seguito denominata MITTENTE) e '
035400060321     c                   eval      costa2b= 'codice fornitore'
035500060321     c                   eval      costa3= '(di seguito denominata VETTORE)'
035600060321     c
035700060321     c                   eval      cost1 = 'Le parti come convenuto all''art.+
035800060321     c                             10 del suddetto contratto di trasporto, con+
035900020411     c                             cordano i corrispettivi per i servizi di'
036000020423     c                   eval      cost2 = 'trasporto, comprensivi delle eventu-
036100060321     c                             ali operazioni accessorie, nonchè di quals+
036200060321     c                             iasi onere necessario per l''esecuzione '
036300060321     c                   eval      costb = 'del trasporto (pedaggi, diritti di +
036400060321     c                             transito, equipaggiamento del veicolo, ecc.)+
036500060321     c                              secondo la seguente tariffa :'
036600020306
036700020411     c*costanti piede
036800020411     c                   eval      cost3 = 'Le parti convengono espressamente c+
036900020411     c                             he i corrispettivi indicati nelle sopraccita+
037000020411     c                             te tariffe possano essere oggetto di'
037100020411     c                   eval      cost3b= 'variazioni, in aumento e/o diminuzi+
037200020411     c                             one, a seguito di modifiche pattuite tra le +
037300020412     c                             parti del progetto operativo'
037400020412     c                   eval      cost3c= 'originariamente concordato.'
037500080916     c                   eval      cost3d= 'Il vettore dichiara espressamente -
037600080916     c                             che i corrispettivi di cui al presente alleg-
037700080916     c                             ato sono stati concordati'
037800080916     c                   eval      cost3e= 'con il mittente sulla base del -
037900080916     c                             costo del carburante pari a euro xx,xxx/lt.'
038000080916
038100020402     c                   eval      cost4 = 'Il vettore autorizza il mittente o +
038200020402     c                              società da questi delegata ad emetter+
038300020411     c                              e alla fine di ogni mese in nome e per '
038400020411     c                   eval      cost5 = 'conto del vettore stesso una fattur+
038500020411     c                             a relativa ai servizi di trasporto ed alle o+
038600020423     c                             perazioni accessorie effettuate'
038700060526     c                   eval      cost5b= 'in tale periodo. Tale fattura com-
038800060526     c                             prenderà anche i corrispettivi per le presta+
038900060526     c                             zioni di trasporto non previste'
039000020306
039100060526     c                   eval      cost5c= 'dal presente allegato C, alle quali-
039200060526     c                              le parti in ogni caso riconoscono la piena +
039300060526     c                             validità ed efficacia del '
039400060526
039500060526     c                   eval      cost5d= 'contenuto degli articoli 10 e 14 -
039600060526     c                             del contratto di trasporto.'
039700060526
039800020411     c                   eval      cost6 = 'La fattura verrà emessa sulla base +
039900060321     c                             dei fogli viaggio  che contengono in dettagl+
040000060321     c                             io i servizi di trasporto e le '
040100020411
040200060321     c                   eval      cost6a= 'operazioni accessorie effettuate.'
040300020411
040400020411     c                   eval      costi = 'L''intestazione del vettore da rip+
040500020411     c                             ortare sulla fattura emessa è:'
040600020411
040700020411     c                   eval      costie= 'Il vettore in caso di variazione de+
040800020412     c                             ll''intestazione ne darà immediata comunic+
040900020412     c                             azione al mittente.'
041000020411
041100020423     c                   eval      cost7 = 'Il presente allegato decorre d+
041200070302     c                             alla data del __________,annulla e sostituis+
041300070302     c                             ce qualsiasi precedente patto verbale o'
041400070302     c                   eval      cost7a= 'scritto relativo al corrispettivo +
041500070302     c                             per l''esecuzione di servizi di trasporto +
041600070302     c                             e costituisce parte integrante del '
041700070302     c                   eval      cost7b= 'contratto di trasporto suddetto.'
041800020412     c                   eval      cost7c= 'Le parti in ogni caso riconoscono +
041900020412     c                             la piena validità ed efficacia del contenut+
042000060321     c                             o degli articoli 10 e 14 del'
042100020412     c                   eval      cost7d='contratto di trasporto.'
042200020411     c                   eval      cost8 = 'Le parti concordano altresì che il +
042300020411     c                             pagamento dei corrispettivi fatturati verrà +
042400080918     c                             effettuato dalla mittente mediante'
042500020402
042800020411     c                   eval      cost8f= 'Il vettore in caso di variazione de+
042900020411     c                             lle coordinate bancarie ne darà +
043000020412     c                             immediata comunicazione al mittente.'
043100080916     c                   if        datae = 20080831
043200080916     c                   eval      cost9 = 'Bologna li, 29/08/2008'
043300080916     c                   else
043400060510     c                   eval      cost9 = 'Bologna li, _______________________'
043500080916     c                   end
043600020411     c                   eval      cost10= 'Firma del Vettore _________________+
043700060322     c                             _________________________'
043800020411     c                   eval      cost11= 'Firma del Mittente_________________+
043900060322     c                             _________________________'
044000020313     c                   eval      cost13= 'NON E STATA TROVATA NESSUNA TARIFFA+
044100020313     c                              VALIDA CON I REFERIMENTI RICHIESTI'
044200020313
044300020308     C* ESEGUO L'OVERRIDE DEL FILE DI STAMPA
044400020308     C                   MOVEL(p)  CMD(1)        COMMAN           80
044500020308     C                   Z-ADD     80            LUNG             15 5
044600020308     C                   CALL      'QCMDEXC'
044700020308     C                   PARM                    COMMAN
044800020308     C                   PARM                    LUNG
044900020308     C                   OPEN      qsysprt
045000011026     c                   endsr
045100011129     C*--------------------------------------------------------------------
045200060322     Oqsysprt   E            testata          06
045300060421     o                       costt               27
045400020412     o                       alladata          +  2 '  /  /    '
045500020411     o                                          108 'Pag.'
045600020117     O                       page          z    115
045700020411     O          E            testata     0
045800060421     o                       costt               27
045900020412     o                       alladata          +  2 '  /  /    '
046000060322     O          E            testata     0
046100060421     o                       costt               27
046200020412     o                       alladata          +  2 '  /  /    '
046300020411      *testo previsto per collegamento al contratto
046400020411     O          E            testo       1
046500060321     o                       costa               74
046600060321     o                       ragsmit            110
046700020411     O          E            testo       1
046800060321     o                       costa2              35
046900060321     o                       ragsvet             80
047000060321     o                       costa2b            100
047100060321     o                       frnksc             110
047200020411     O          E            testo       1
047300020411     o                       costa3             120
047400020306     O          E            testo       1
047500020411     o                       cost1              120
047600020306     O          E            testo       1
047700020411     o                       cost2              120
047800020306      *
047900020402     O          E            testo       1
048000020411     o                       costb              120
048100020402      *
048200060320     O          E            testo       1
048300020305     o                       separa             100
048400060320     O          E            testo       2
048500060320     o                                           10 'Nr.Traino'
048600060321     o                                           21 'Partenza'
048700060321     o                                           45 'Arrivo'
048800060321     o                                           77 'Importo'
048900060320     O          E            testo       0
049000060320     o                                           10 'Nr.Traino'
049100060321     o                                           21 'Partenza'
049200060321     o                                           45 'Arrivo'
049300060321     o                                           77 'Importo'
049400060320     O          E            testo       0
049500060320     o                                           10 'Nr.Traino'
049600060321     o                                           21 'Partenza'
049700060321     o                                           45 'Arrivo'
049800060321     o                                           77 'Importo'
049900060320      *dettaglio
050000060320     O          E            dettaglio   1
050100060320     o                       tlltrn        z      8
050200060322     o                       partenza          +  5
050300060322     o                       arrivo            +  6
050400060320     o                       tllili        2   +  2
050500060320     O          E            dettaglio   0
050600060324     o                       tlltrn        z      8
050700060324     o                       partenza          +  5
050800060324     o                       arrivo            +  6
050900060324     o                       tllili        2   +  2
051000060324     O          E            dettaglio   0
051100060324     o                       tlltrn        z      8
051200060324     o                       partenza          +  5
051300060324     o                       arrivo            +  6
051400060324     o                       tllili        2   +  2
051500060320     O          E            dettanote   1
051600060324     o                                           10 'Percorso'
051700060322     o                       tlntno              73
051800020411      *testo previsto per collegamento al contratto piede
051900020411      * parte prima
052000020402     O          E            piede       2
052100020306     o                       cost3              120
052200020306     O          E            piede       1
052300020411     o                       cost3b             120
052400020412     O          E            piede       1
052500020412     o                       cost3c             120
052600080916     O          E            piede       1
052700080916     o                       cost3D             120
052800080916     O          E            piede       1
052900080916     o                       cost3E             120
053000080916     o                       prezzo        2     70
053100020306     O          E            piede       1
053200020306     o                       cost4              120
053300020306     O          E            piede       1
053400020306     o                       cost5              120
053500020411     O          E            piede       1
053600020411     o                       cost5b             120
053700060526     O          E            piede       1
053800060526     o                       cost5c             120
053900060526     O          E            piede       1
054000060526     o                       cost5d             120
054100020411      * parte seconda
054200060321     O          E            piede       1
054300020306     o                       cost6              120
054400060321     O          E            piede       1
054500020411     o                       cost6a             120
054600020411      * parte terza
054700060321     O          E            piede       1
054800020411     o                       costi              120
054900060321     O          E            piede       1
055000020411     o                       costia             120
055100060321     O          E            piede       1
055200020411     o                       costib             120
055300060321     O          E            piede       1
055400020411     o                       costic             120
055500080916     O          E            piede       1
055600080916     o                       costid             120
055700060321     O          E            piede       1
055800020423     o                       costif             120
055900080916     O          E    89      piede       1
056000080916     o                       costiH             120
056100080916     O          E    89      piede       1
056200080916     o                       costiI             120
056300060321     O          E            piede       1
056400020411     o                       costie             120
056500020411      * parte quarta
056600060321     O          E            piede       1
056700020306     o                       cost7              120
056800020423     o                       alladata            54 '  /  /    '
056900070302     O          E            piede       1
057000070302     o                       cost7a             120
057100060321     O          E            piede       1
057200020411     o                       cost7b             120
057300060321     O          E            piede       1
057400020412     o                       cost7c             120
057500060321     O          E            piede       1
057600020412     o                       cost7d             120
057700020411      * parte quinta
057800060324     O          E            piede2      1
057900020306     o                       cost8              120
058000060324     O          E            piede2      1
058100020402     o                       cost8b             120
058200060324     O          E            piede2      1
058300020402     o                       cost8c             120
058400060324     O          E            piede3      1
058500020402     o                       cost8f             120
058600020411      * parte sesta
058700060324     O          E            piede3      2
058800060510     o                       cost9               35
058900060324     O          E            piede3      2
059000060322     o                       cost11              60
059100060321     o                       cost10             120
059200020313**
059300020308OVRPRTF FILE(QSYSPRT) LPI(6) CPI(15) PAGRTT(0)
059400020308DLTOVR FILE(QSYSPRT)
