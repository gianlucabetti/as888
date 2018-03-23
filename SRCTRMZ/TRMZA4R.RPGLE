000100120426      *--------------------------------------------------------------*
000200120426      *
000300120427      *  Controllo solo se Trazionisti con contratto 999999
000400120503      *   stampa di controllo fra allegati trazionisti e CONTRATTI
000500120426      *
000600120426     H*--------------------------------------------------------------*
000700981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000800000000     H*--------------------------------------------------------------*
000900120426      ****
001000120426     Ftrmza4p   O    e             PRINTER OFLIND(*IN23) INFDS(PRTFBK)
001100120426      ****
001200120430     Fanfrn01l  iF   E           K DISK
001300120430     Fansog01l  iF   E           K DISK
001400120430      ****
001500090522     Ffiadt01l  if   e           k disk
001600021203     Ffiapd01l  if   e           k disk
001700090612     FTABEL00F  IF   E           K DISK
001800090617      *
001900090617     D PRTFBK          ds
002000090617     D   CUR_LINE            367    368i 0                                      * Current line num
002100090617     D   CUR_PAGE            369    372i 0                                      * Current page num
002200090617      *
002300020304     dkpjba          e ds
002400090612     D ds5Aaut       E DS
002500120808     D ficn27ds      e ds
002600090721     D ddatiute      e ds
002700090525     D azuteds       e ds                  extname(AZUTE00F)
002800120426      ***
002900120426     D trmz70s4ds    E DS                  prefix(S4_)
003000120426
003100120502     D partraz         DS
003200120518     D  parsoc                        3s 0
003300120502     D  parivat                      16
003400120502     D  pardtin                       8s 0
003500120502     D  pardtfi                       8s 0
003600120502     d  esitot                        1
003700120502     d  errmsgt                      75
003800120426      ***
003900090525     D tibs34ds      E DS                  inz
004000090525     D trmz70s1ds    E DS                  inz
004100090619     d NRC             s              7  0 DIM(100)
004200090612     D WLBDAT          DS
004300090612     D  G02DAT                 1      8  0
004400090612     D  G02INV                 9     16  0
004500090612     D  G02ERR                17     17
004600090612     D  G02TGI                18     22  0
004700090612     d dtaisodts       s               d   datfmt(*iso)
004800090612     d isooggi         s               d   datfmt(*iso)
004900090612     d dataiso         s               d   datfmt(*iso)
005000020305     d dataeur         s               d   datfmt(*eur)
005100090525     d compdr          s              3  0
005200120427     d filADTPDR       s              3  0
005300090825     d savfil          s              3  0
005400090612     d gg              s              3  0
005500090525     d comsoc          s              3
005600090525     d comksc          s              8
005700120430     d comiva          s             16
005800090617     d conta_righe     s              3  0
005900120426     D Trazionista     s              2
006000120426     D dsAIATS       e DS                  EXTNAME(AIATS00F)
006100120426     D SI              C                   CONST('SI')
006200120426     D NO              C                   CONST('NO')
006300000000     C*---------------------------------------------------------------*
006400120427     c*
006500120427     C*---------------------------------------------------------------*
006600120427     c*
006700090616     c                   exsr      azzera_TOTali
006800090616     c*
006900120427     c                   exsr      TARIFFE
007000120426     c*
007100090616     c* se stampato almeno una riga emetto leggenda
007200090522     c                   if        una <> ' '
007300090617      *  Forza il salto pagina per stampare i totali sulla pagina seguente
007400120426     c*
007500090616     c                   exsr      totali
007600090522     c                   end
007700090616      *
007800020308     c                   seton                                        lr
007900090522     c*-------------------------------------------------
008000120427     c     TARIFFE       begsr
008100090522     c*-------------------------------------------------
008200090522     c                   setoff                                           99
008300120427     c                   clear                   una               1
008400090522     c     *loval        setll     fiadt01l
008500120426     c*
008600090522     c                   do        *hival
008700090522     c                   read      fiadt01l                               99
008800090522     c   99              leave
008900120426     c*
009000091106     c* non elaboro se stampa e ricezione firma sono 20090501
009100120427     c* se record annullato o NON 9999999
009200120612      *  e
009300120612      * deve saltare se la data scadenza è inferiore a oggi
009400120607     c                   if         adtnrc < 9000000
009500120427     c                             or adtatb <> ' '
009600120612     c                             or (adtdst < oggi_amg and adtdst >0)
009700090616     c                   iter
009800090616     c                   end
009900120426      *
010000120612      *
010100090525     c* se profilo di filiale chiudo il file di stampa a rottura
010200120427     c                   movel     adtpdr        filADTPDR
010300090525     c                   if        dutpou <> 046
010400090525     c                   seton                                        24
010500120808     c                   if        filADTPDR   <> d27fil
010600090525     c                   iter
010700090525     c                   end
010800120427     c                   movel     filADTPDR     compdr
010900090825     c                   else
011000091015      * profilo di sede ma richiesta singola filiale
011100120808     c                   if        d27fil > 0  and filADTPDR <> d27fil
011200091015     c                   iter
011300091015     c                   endif
011400090825     c* per sede a cambio di po salta riga
011500120427     c                   if        filADTPDR <> savfil
011600120427     c                   eval      savfil = filADTPDR
011700090825     c                   seton                                        25
011800090825     c                   end
011900090525     c                   end
012000120510     c*--+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
012100090525     c                   clear                   msga
012200090525     c                   clear                   msgc
012300090525     c                   clear                   msgs
012400120510     c                   clear                   msg1
012500120510     c                   clear                   msg2
012600120510     c                   clear                   msg3
012700120510     c*--+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
012800120430     c* aggancio il fornitore della tariffa
012900120430     c                   movel     adtcsf        comsoc
013000120430     c                   move      *zeros        comksc
013100120430     c                   move      adtcdf        comksc
013200120503     c*    x la tariffa il fornitore è
013300120430     c                   exsr      srcdf
013400120430     c*
013500120427     c*  Aggancia Anagrafica FIAPD per reperire il FORNITORE
013600120427     c*   con il quale arrivare alla P.IVA da confrontare
013700120427     c*   sui trazionisti
013800090522     c                   eval      apdtip = 'D'
013900090522     c                   eval      apdpdr = adtpdr
014000090522     c     kapdf         chain     fiapd01l                           97
014100120427      *se l'anagrafica non corrisponde non stampo ma vado oltre
014200090522     c   97              iter
014300120427      *
014400090611     c                   movel     apdrsf        apdrsfs
014500120508     c                   movel     apdATB        ANNULLO
014600120510     c                   if        annullo = 'A'
014700120510     c                   eval          msg1= 'A'
014800120510     c                   eval          msga= 'N'
014900120510     c                   eval          msg2= 'N'
015000120510     c                   eval          msgc= 'U'
015100120510     c                   eval          msg3= 'L'
015200120510     c                   eval          msgs= 'L'
015300120510     c                   end
015400120426      *
015500120426      **  pulisce le descrizioni da Decodificare
015600120427     c                   movel     *blank        DESFRN           44
015700120426     c                   movel     *blank        PIVA             16
015800120426      * ricava la Partita IVA
015900120426     c                   exsr      Decod_P_IVA
016000120426      *
016100120426      **  con P.IVA controlla se è un trazionista
016200120426     c                   exsr      se_TRAZIONISTA
016300120426      **
016400120510     c*   PUlizia date in stampa
016500120510     c                   clear                   TRADINS
016600120510     c                   clear                   TRADFIS
016700120612     c                   clear                   tarDDTS
016800120612     c                   clear                   tarDRCS
016900120612     c                   clear                   tarDTSS
017000120612     c                   clear                   tarDCNS
017100120510     c                   clear                   DATADEC
017200120510     c                   clear                   TRSDRCS
017300120503      **
017400120503      **  il TRULTRAZ  non ha trovato l'AIATS.... oppure è stato annullato
017500120717      ***
017600120717      ***  si utilizza il Blank x capire se NON è un Trazionista.
017700120717     c                   IF        esitoT =  ' '
017800120717     c                                 or
017900120717     c                             esitoT <> ' ' and atsATB <> ' '
018000120503      *  NON TROVATO AIATS
018100120510     c                   if            msgc = ' '
018200120510     c                   eval          msgc = 'G'
018300120510     c                   end
018400120503     c*
018500120503     c                   eLSE
018600120503      **
018700120503      **  DATA Decorrenza contratto  o  DATA Accreditamento
018800120503      **      maggiore         della decorrenza TARIFFA
018900120503      **  OPPURE
019000120503     c* Se DATA Decorrenza tariffa è maggiore della DATA fine accreditamento
019100120503     c*                                                o fine contratto
019200120503     c* o se DATA Scadenza tariffa è maggiore della DATA fine accreditamento
019300120503     c*                                                o fine contratto
019400120510      **  OPPURE
019500120510      **      NON è accreditato.
019600120503     c                   IF              adtdDT < ATSdEC
019700120503     c                               or  adtdDT < ATSdtINC
019800120503     c                               or
019900120504     c                                   adtdDT > atsdtFIC and atsdtFIC >0
020000120508     c                               or  adtdDT > atsdFC   and atsDFC   >0
020100120503     c                               or
020200120504     c                                   adtdST > atsdtFIC and atsdtFIC >0
020300120508     c                               or  adtdST > atsdFC   and atsDFC   >0
020400120510     c                               or
020500120510     c                                   adtdDT > 0  and atsdtINC = 0
020600120510     c                   if        msgs = *blank
020700120510     c                   eval          msgs = 'F'
020800120510     c                   end
020900120503      *
021000120510     c                   end
021100120426      *
021200090612     c* se è accreditato ma non stampato
021300120503     c                   if        adtDCN = 0
021400120510     c                               and msga = *blank
021500120510     c                   eval            msga = 'E'
021600090612     c                   end
021700120510      *
021800120510     c                   if        adtdcn <> 0 and adtdrc = 0
021900120510     c                                and msga = ' '
022000120510     c                   eval             msga = 'C'
022100120510     c                   end
022200120427      *------
022300120510     c* se  la sede non ha ricevuto COPIA firmata
022400120510     c                   if        msgC = ' ' and
022500120510     c                             (atsdec > 0 and atsDRC = 0)
022600120510     c                   eval           msgC = 'H'
022700120510     c                   end
022800120510      *---
022900120510     c                   end
023000120510      *
023100090522     c* fornitori
023200120427     c                   eval      RAGSOCKSCs = DESFRN
023300120427      **
023400090522     c                   exsr      fornitori
023500090522     c* stampa
023600090522     c                   exsr      stampa
023700090522
023800090522     c                   enddo
023900090522
024000090522     c                   endsr
024100120427      * ?___________________________________________________________________
024200120427     C     fornitori     BegSR
024300120427      * ?___________________________________________________________________
024400120510      **
024500120427     c                   if        PIVA   = *Blank
024600120427     c                   clear                   RAGSOCKSCs
024700120427     c                   end
024800120427      **
024900120427     c                   if        ATSPIVA <> PIVA and ATSPIVA <> ' '
025000120427     c                   seton                                        37
025100120427     c                   else
025200120427     c                   setoff                                       37
025300120427     c                   end
025400120510      **
025500120427     c                   endsr
025600090522      * ?___________________________________________________________________
025700090522     C     stampa        BegSR
025800090522      * ?___________________________________________________________________
025900090525     c                   if        *in23
026000090525     c                   write     testa
026100090525     c                   setoff                                       23
026200090522     c                   end
026300090612     c*
026400120427     c                   if        ATSDTINC <> 0
026500120427     c                   move      ATSDTINC      dataiso
026600090522     c                   move      dataiso       dataeur
026700090522     c                   move      dataeur       tradins           8 0
026800090522     c                   end
026900120427      *
027000120427     c                   if        ATSDTFIC <> 0
027100120427     c                   move      ATSDTFIC      dataiso
027200090525     c                   move      dataiso       dataeur
027300090525     c                   move      dataeur       tradfis           8 0
027400090525     c                   end
027500120427      *
027600120427     c                   if        atsdrc <> 0
027700120427     c                   move      atsdrc        dataiso
027800090522     c                   move      dataiso       dataeur
027900090522     c                   move      dataeur       trsdrcs           8 0
028000090522     c                   end
028100120508      *
028200120508     c                   if        atsdec <> 0
028300120508     c                   move      atsdec        dataiso
028400120508     c                   move      dataiso       dataeur
028500120508     c                   move      dataeur       DataDEC           8 0
028600120508     c                   end
028700120508      *
028800120510     c                   if        adtdcn <> 0
028900120510     c                   move      adtdcn        dataiso
029000120510     c                   move      dataiso       dataeur
029100120612     c                   move      dataeur       tardcns
029200120510     c                   end
029300120510      *
029400120510     c                   if        adtdts <> 0
029500120510     c                   move      adtdts        dataiso
029600120510     c                   move      dataiso       dataeur
029700120612     c                   move      dataeur       tardtss
029800120510     c                   end
029900120510      *
030000120510     c                   if        adtddt <> 0
030100120510     c                   move      adtddt        dataiso
030200120510     c                   move      dataiso       dataeur
030300120612     c                   move      dataeur       tarddts
030400120510     c                   end
030500120510      *
030600120612     c                   if        adtdrc <> 0
030700120612     c                   move      adtdrc        dataiso
030800120510     c                   move      dataiso       dataeur
030900120612     c                   move      dataeur       tardrcs
031000120510     c                   end
031100120612      *
031200120612      *
031300090612     c* evidenzio le anomalie che sono a rischio
031400090612     c                   setoff                                       404142
031500120511      *
031600120612     c                   if        tardtss <> 0
031700120612     c                   move      tardtss       dataeur
031800090612     c                   move      dataeur       dtaisodts
031900091014     c                   else
032000091014     c                   movel     *loval        dtaisodts
032100091014     c                   end
032200090612     c* allegato
032300090612     c     isooggi       subdur    dtaisodts     gg:*d
032400120510     c                   if         msga = 'C'
032500120510     c                               and
032600120510     c                             gg > §5AGGERRC
032700090612     c                   seton                                        4041
032800090612     c                   end
032900090612     c* contratto
033000120510     c                   if        (msgc = 'H' or msgc = 'G')
033100120510     c                               and
033200120510     c                             gg > §5AGGERRD
033300090612     c                   seton                                        4042
033400090612     c                   end
033500090617      *
033600090616     c                   exsr      somma_TOTali
033700090617      *
033800090612     c* stampo solo se ho evidenziato l'anomalia
033900120503     c                   if        (soloanomal = 'S' and *in40 ) or
034000120503     c                              soloanomal = ' '
034100090617     c                   if             msga <> *blank or
034200090617     c                                  msgc <> *blank or
034300090617     c                                  msgs <> *blank
034400090825     c   25              write     riga1
034500120511     c*
034600120511     c                   if        *in23
034700120511     c                   write     testa
034800120511     c                   setoff                                       23
034900120511     c                   end
035000120511     c*
035100090825     c                   write     riga
035200090825     c                   setoff                                       25
035300090617     c                   end
035400090617      *
035500090612     c                   eval      una = '1'
035600090612     c                   end
035700120510      *
035800090522     c                   endsr
035900120427      * ?___________________________________________________________________
036000090616     c     azzera_TOTali begsr
036100090616     c*
036200090616     c                   clear                   msgCTOT           5 0
036300090616     c                   clear                   rscCTOT           5 0
036400090616     c                   clear                   msgDTOT           5 0
036500090616     c                   clear                   rscDTOT           5 0
036600090616     c                   clear                   msgETOT           5 0
036700090617     c                   clear                   rscETOT           5 0
036800120508     c                   clear                   msgFTOT           5 0
036900120508     c                   clear                   rscFTOT           5 0
037000120508     c                   clear                   msgGTOT           5 0
037100120508     c                   clear                   rscGTOT           5 0
037200090616     c                   clear                   msg9TOT           5 0
037300090617     c                   clear                   msgCTOT_a         5 0
037400090617     c                   clear                   rscCTOT_a         5 0
037500090617     c                   clear                   msgDTOT_a         5 0
037600090617     c                   clear                   rscDTOT_a         5 0
037700090617     c                   clear                   msgETOT_a         5 0
037800090617     c                   clear                   rscETOT_a         5 0
037900120508     c                   clear                   msgFTOT_a         5 0
038000120508     c                   clear                   rscFTOT_a         5 0
038100120508     c                   clear                   msgGTOT_a         5 0
038200120508     c                   clear                   rscGTOT_a         5 0
038300090617     c                   clear                   msg9TOT_a         5 0
038400090616     c                   clear                   Senza_contrC      5 0
038500090616     c                   clear                   Senza_contrD      5 0
038600090616     c                   clear                   Senza_contrE      5 0
038700120508     c                   clear                   Senza_contrF      5 0
038800120508     c                   clear                   Senza_contrG      5 0
038900090616     c                   clear                   Senza_contr9      5 0
039000090617     c                   clear                   Senza_contrC_a    5 0
039100090617     c                   clear                   Senza_contrD_a    5 0
039200090617     c                   clear                   Senza_contrE_a    5 0
039300120508     c                   clear                   Senza_contrF_a    5 0
039400120508     c                   clear                   Senza_contrG_a    5 0
039500090617     c                   clear                   Senza_contr9_a    5 0
039600090616     c*
039700090616     C                   ENDSR
039800120427      * ?___________________________________________________________________
039900090616     c     somma_TOTali  begsr
040000090617      *
040100120510     c                   IF        msgA ='C'
040200090617     c                   add       1             msgCTOT
040300090617     c   41              add       1             rscCTOT
040400120509     c                   if        atsDEC = 0
040500120509     c                   add       1             Senza_contrC
040600120509     c                   end
040700090617     c                   add       1             msgCTOT_a
040800090617     c   41              add       1             rsCCTOT_a
040900090617      *
041000090617     c                   elseIf    msgA ='E'
041100090617     c                   add       1             msgETOT
041200090617     c   41              add       1             rscETOT
041300120509     c                   if        atsDEC = 0
041400120509     c                   add       1             Senza_contrE
041500120509     c                   end
041600090617     c                   add       1             msgETOT_a
041700090617     c   41              add       1             rsCETOT_a
041800120508      *
041900090616     c                   end
042000090616     c*
042100090619     c*  Le D vanno raggruppate per contratto quindi conta x una volta sola
042200120510     c                   if        msgC ='H'
042300090617     c                   add       1             msgDTOT
042400090617     c   42              add       1             rscDTOT
042500120509     c                   if        atsDEC = 0
042600120509     c                   add       1             Senza_contrD
042700120509     c                   end
042800090617     c                   add       1             msgDTOT_a
042900090617     c   42              add       1             rsCDTOT_a
043000120510      *
043100120510     c                   elseIf    msgC ='G'
043200120510     c                   add       1             msgGTOT
043300120510     c   42              add       1             rscGTOT
043400120510     c                   if        atsDEC = 0
043500120510     c                   add       1             Senza_contrG
043600120510     c                   end
043700120510     c                   add       1             msgGTOT_a
043800120510     c   42              add       1             rsCGTOT_a
043900090619      ***
044000120427     c                   end
044100090616      *
044200120510     c                   if        msgS ='9'
044300090616     c                   add       1             msg9TOT
044400090617      *
044500090617     c                   add       1             msg9TOT_a
044600120509     c                   if        atsDEC = 0
044700120509     c                   add       1             Senza_contr9
044800120509     c                   end
044900090617      *
045000120717     c                   elseIf    msgs ='F'
045100120717     c                   add       1             msgFTOT
045200120717     c   41              add       1             rscFTOT
045300120717     c                   if        atsDEC = 0
045400120717     c                   add       1             Senza_contrF
045500120717     c                   end
045600120717     c                   add       1             msgFTOT_a
045700120717     c   41              add       1             rsCFTOT_a
045800120717      *
045900120427     c                   end
046000090616      *
046100090616     C                   ENDSR
046200120427      * ?___________________________________________________________________
046300090616     c     Totali        begsr
046400120427      * ?___________________________________________________________________
046500090616      *
046600090616     c                   movel     'C'           tipoERR
046700090616     c                   z-add     msgCTOT       numERR
046800090616     c                   z-add     rscCTOT       rscERR
046900090616     c                   z-add     Senza_contrC  NoContra
047000090616     c                   if        *in23
047100090616     c                   write     testa
047200090616     c                   write     testaTOT
047300090616     c                   setoff                                       23
047400120511     c                   else
047500120511     c                   write     testaTOT
047600090616     c                   end
047700090616     c                   write     rigaTOT
047800090616      *
047900120510     c                   movel     'H'           tipoERR
048000090616     c                   z-add     msgDTOT       numERR
048100090616     c                   z-add     rscDTOT       rscERR
048200090616     c                   z-add     Senza_contrD  NoContra
048300090616     c                   if        *in23
048400090616     c                   write     testa
048500090616     c                   write     testaTOT
048600090616     c                   setoff                                       23
048700090616     c                   end
048800090616     c                   write     rigaTOT
048900120508      *
049000120508     c                   movel     'E'           tipoERR
049100120508     c                   z-add     msgETOT       numERR
049200120508     c                   z-add     rscETOT       rscERR
049300120508     c                   z-add     Senza_contrE  NoContra
049400120508     c                   if        *in23
049500120508     c                   write     testa
049600120508     c                   write     testaTOT
049700120508     c                   setoff                                       23
049800120508     c                   end
049900120508     c                   write     rigaTOT
050000090616      *
050100120508     c                   movel     'F'           tipoERR
050200120508     c                   z-add     msgFTOT       numERR
050300120508     c                   z-add     rscFTOT       rscERR
050400120508     c                   z-add     Senza_contrF  NoContra
050500090616     c                   if        *in23
050600090616     c                   write     testa
050700090616     c                   write     testaTOT
050800090616     c                   setoff                                       23
050900090616     c                   end
051000090616     c                   write     rigaTOT
051100120503      *
051200120508     c                   movel     'G'           tipoERR
051300120508     c                   z-add     msgGTOT       numERR
051400120508     c                   z-add     rscGTOT       rscERR
051500120508     c                   z-add     Senza_contrG  NoContra
051600120503     c                   if        *in23
051700120503     c                   write     testa
051800120503     c                   write     testaTOT
051900120503     c                   setoff                                       23
052000120503     c                   end
052100120503     c                   write     rigaTOT
052200090616      *
052300090616     c                   movel     '9'           tipoERR
052400090616     c                   z-add     msg9TOT       numERR
052500090616     c                   z-add     0             rscERR
052600090616     c                   z-add     Senza_contr9  NoContra
052700090616     c                   if        *in23
052800090616     c                   write     testa
052900090616     c                   write     testaTOT
053000090616     c                   setoff                                       23
053100090616     c                   end
053200090616     c                   write     rigaTOT
053300090616      *
053400090616     C                   ENDSR
053500120426     C/EJECT
053600120427      * ?___________________________________________________________________
053700120426     c* tramite Società e Unità decodifica P.IVA su PROJ
053800120427      * ?___________________________________________________________________
053900120426     C     Decod_P_IVA   BegSR
054000120426      **
054100120426      **  Routine x Reperire dati PIVA:
054200120426      **    La routine in base alla sottonatura può funzionare
054300120426      **     x F=Fornitore/C=Cliente
054400120426     C                   clear                   trmz70s4ds                     Input
054500120426     C                   moveL(P)  *all'0'       s4_keyKSC                      Input
054600120426     C                   move      APDKSC        s4_keyKSC                      Input
054700120426     C                   movel(p)  'F'           s4_SottoNatur                  Input "F/C"
054800120426     C                   movel(p)  APDCSF        s4_Societa                     Input/Output
054900120426     C                   movel(p)  apdPDR        s4_Unita                       Input/Output
055000120426     c                   call      'TRMZ70SR4'
055100120426     c                   parm                    trmz70s4ds
055200120426      *
055300120426      *  se torna con l'errore riprova e aveva passato l'unità
055400120426      *   riprova passandogli l'unità vuota
055500120426     c                   if        s4_errore <> '0'
055600120426     C                   clear                   trmz70s4ds                     Input
055700120426     C                   moveL(P)  *all'0'       s4_keyKSC                      Input
055800120426     C                   move      apdKSC        s4_keyKSC                      Input
055900120426     C                   movel(p)  'F'           s4_SottoNatur                  Input "F/C"
056000120426     C                   movel(p)  apdCSF        s4_Societa                     Input/Output
056100120426     c                   call      'TRMZ70SR4'
056200120426     c                   parm                    trmz70s4ds
056300120426     c                   end
056400120426      *
056500120426      *  Descrizione Ragione Sociale FORNITORE
056600120426     c                   if        s4_errore = '0'
056700120426     c                   eval        DESFRN = s4_RAGSOCKSC
056800120426     c                   eval        PIVA   = s4_PARTITAIVA
056900120427     c                   else
057000120510     c                   if        msgs = *blank
057100120427     c                   eval      msgs = '9'
057200120426     c                   end
057300120510     c                   end
057400120426      *
057500120426     C                   ENDSR
057600120426     C/EJECT
057700120427      * ?___________________________________________________________________
057800120426      **   esegue un controllo tramite la PARTITA IVA
057900120427      * ?___________________________________________________________________
058000120426     c     se_TRAZIONISTAbegsr
058100120426      **
058200120426     c                   eval      trazionista = NO
058300120426      **
058400120426      **  controlla se la P.IVA esiste sul file dei Trazionisti
058500120426     c                   clear                   partraz
058600120503     c                   clear                   dsaiats
058700120611     c                   eval      pardtin = adtddt
058800120611     c                   eval      pardtfi = adtdst
058900120518     c                   movel     s4_societa    parsoc
059000120426     c                   movel     s4_PARTITAIVA parivaT
059100120426     c                   call      'TRULTRAZ'
059200120426     c                   parm                    partraz
059300120426     c                   parm                    dsAIATS
059400120426      *
059500120717     c                   if         esitoT <> ' '
059600120426     c                   eval      trazionista = SI
059700120426     c                   end
059800120426      *
059900120426     C                   ENDSR
060000120426     C/EJECT
060100120430      * ?___________________________________________________________________
060200120430     c     srcdf         begsr
060300120430      * ?___________________________________________________________________
060400120430      *
060500120430     c                   clear                   sogdess
060600120430     c                   clear                   comiva
060700120430     c     kfrn          chain     anfrn01l
060800120430     c                   if        %found(anfrn01l)
060900120430     c     frnsogg       chain     ansog01l
061000120430     c                   if        %found(ansog01l)
061100120430     c                   movel     sogdes        sogdess
061200120430     c                   movel     sogpartiva    comiva
061300120430     c                   else
061400120430     c                   clear                   comiva
061500120430     c                   end
061600120430     c                   end
061700120510      *
061800120430     c                   endsr
061900120427      * ?___________________________________________________________________
062000090616     c     *inzsr        begsr
062100120427      * ?___________________________________________________________________
062200020304     c     *entry        plist
062300020304     c                   parm                    kpjba
062400120426      *
062500120808     c                   movel     kpjbu         ficn27ds
062600120808     c                   movel     d27solor      soloanomal        1
062700090525     c                   seton                                        23
062800120426      **
062900120511     c                   seton                                        23
063000120426     c                   eval      nompgm = 'TRMZA4R'
063100090525     c     *dtaara       define    §azute        azuteds
063200090525     c     *dtaara       define    §datiute      ddatiute
063300090525     C                   in(E)     *dtaara
063400090612     C
063500090525     C                   call      'TIBS34R'
063600090525     C                   parm                    Tibs34Ds
063700090525     C                   in        *dtaara
063800090525     c                   movel     *all'_'       cmp198          198
063900090525     c                   movel     *all'_'       cmp198a         198
064000120508     c                   movel     *all'_'       cmp055           55
064100120508     c                   movel     *all'_'       cmp055a          55
064200090617      *
064300090617      * stampa la legenda in testa all'inizio della stampa
064400090617     c                   seton                                        93
064500090617      *
064600020218     C                   time                    w0120            14 0
064700090525     C                   move      w0120         woggi
064800090612     C                   MOVE      W0120         UDATE8            8 0
064900090612     C                   Z-ADD     UDATE8        G02DAT
065000090612     C                   MOVE      *ZEROS        G02INV
065100090612     C                   MOVE      *BLANKS       G02ERR
065200090612     C                   CALL      'XSRDA8'
065300090612     C                   PARM                    WLBDAT
065400090612     C                   MOVE      g02inv        isooggi
065500120612     C                   MOVE      g02inv        oggi_amg          8 0
065600090525     C                   movel     w0120         times
065700090612     C     KTAB          KLIST
065800090612     C                   KFLD                    tblkut
065900090612     C                   KFLD                    tblcod
066000090612     C                   KFLD                    tblkey
066100090612     C                   Z-ADD     1             tblkut
066200021203     c     kapdf         klist
066300090424     c                   kfld                    apdtip
066400090525     c                   kfld                    apdpdr
066500090424     c                   movel     'A'           apdtip
066600090525     C     Kfrn          KLIST
066700090525     C                   KFLD                    comsoc
066800090525     C                   KFLD                    comksc
066900120427      *
067000090612     C***  giorni da calcolare x date limite
067100090612     C                   clear                   ds5Aaut
067200090612     C                   movel     '5A'          tblcod
067300090612     C                   MOVEL(P)  'AUT'         tblKEY
067400090612     C     KTAB          CHAIN     TABEL00F                           30
067500090612     c                   if        %found(tabel00f) and tblflg = ' '
067600090612     c                   movel     tbluni        ds5aaut
067700090619      **
067800090619      **  Attenzione se ci si trova nel periodo delle Trazioni Ridotte deve
067900090619      **  incrementare di x giorni i giorni di rischio per i 4 tipi di errori
068000090619     c                   if             §5ADTAINI <= Udate8 and
068100090619     c                                  §5ADTAFIN >= Udate8
068200090619     c                   eval      §5AGGERRc = §5AGGERRc + §5AGGINPIU
068300090619     c                   eval      §5AGGERRd = §5AGGERRd + §5AGGINPIU
068400090619     c                   end
068500090619      **
068600090612     c                   end
068700090617      *
068800090617     c                   write     leggenda
068900090617     c                   setoff                                       23
069000090617      *
069100070307     c                   endsr
069200120427      * ?___________________________________________________________________
