000100120607     H*--------------------------------------------------------------*
000200120607      *
000300120607      *   report autocontrollo allegati e contratti
000400120607      *
000500120607     H*--------------------------------------------------------------*
000600981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000700000000     H*--------------------------------------------------------------*
000800090617     Fficn28p   O    e             PRINTER OFLIND(*IN23) INFDS(PRTFBK)
000900090526     Ffitgt01l  if   e           k disk
001000090522     Ffiadt01l  if   e           k disk
001100021203     Ffiapd01l  if   e           k disk
001200090522     Fanfrn01l  iF   E           K DISK
001300090522     Fansog01l  iF   E           K DISK
001400090526     Faitrs02l  iF   E           K DISK
001500090522     Faitra03l  iF   E           K DISK
001600090522     Faitra05l  iF   E           K DISK    rename(aitra000:aitra005)
001700090612     FTABEL00F  IF   E           K DISK
001800090617      *
001900090617     D PRTFBK          ds
002000090617     D   CUR_LINE            367    368i 0                                      * Current line num
002100090617     D   CUR_PAGE            369    372i 0                                      * Current page num
002200090617      *
002300020304     dkpjba          e ds
002400090612     D ds5Aaut       E DS
002500090721     D ficn27ds      e ds
002600090721     D ddatiute      e ds
002700090525     D azuteds       e ds                  extname(AZUTE00F)
002800090525     D aitrads       e ds                  extname(aitra00f)
002900090525     D aitrsds       e ds                  extname(aitrs00f)
003000090525     D tibs34ds      E DS                  inz
003100090525     D trmz70s1ds    E DS                  inz
003200090619     d NRC             s              7  0 DIM(100)
003300090612     D WLBDAT          DS
003400090612     D  G02DAT                 1      8  0
003500090612     D  G02INV                 9     16  0
003600090612     D  G02ERR                17     17
003700090612     D  G02TGI                18     22  0
003800090612     d dtaisodts       s               d   datfmt(*iso)
003900090612     d isooggi         s               d   datfmt(*iso)
004000090612     d dataiso         s               d   datfmt(*iso)
004100020305     d dataeur         s               d   datfmt(*eur)
004200090525     d compdr          s              3  0
004300090619     d compd           s              3  0
004400090825     d savfil          s              3  0
004500090612     d gg              s              3  0
004600090525     d comsoc          s              3
004700090526     d comiva          s             16
004800090525     d comksc          s              8
004900090617     d conta_righe     s              3  0
005000000000     C*---------------------------------------------------------------*
005100090616     c                   exsr      azzera_TOTali
005200090616     c*
005300090522     c                   exsr      fitgt
005400020304      *
005500090522     c                   exsr      fiadt
005600090616     c* se stampato almeno una riga emetto leggenda
005700090522     c                   if        una <> ' '
005800090617      *  Forza il salto pagina per stampare i totali sulla pagina seguente
005900090617     c                   if        CUR_LINE >= 55
006000090617     c                   seton                                        23
006100090617     c                   end
006200090616     c                   exsr      totali
006300090522     c                   end
006400090616      *
006500020308     c                   seton                                        lr
006600990113     c*-------------------------------------------------
006700090424     c     fitgt         begsr
006800990113     c*-------------------------------------------------
006900990113     c                   setoff                                           99
007000020404     c                   clear                   una               1
007100090424     c     *loval        setll     fitgt01l
007200981029     c                   do        *hival
007300090424     c                   read      fitgt01l                               99
007400981029     c   99              leave
007500090424     c*
007600090424     c                   if        tgtsml <> ' ' or tgtatb <> ' '
007700090424     c                   iter
007800090424     c                   end
007900091106     c* non elaboro se stampa e ricezione firma sono 20090501
008000091106     c                   if        tgtdcn = 20090501 and tgtdrc = 20090501
008100090525     c                   iter
008200090525     c                   end
008300090522     c* solo con data convalida e no ricezione copia firmata
008400091014     c*                  if        tgtdts = 0  or tgtdrc <> 0
008500091014     c*                  iter
008600091014     c*                  end
008700090525     c* se profilo di filiale chiudo il file di stampa a rottura
008800090825     c                   movel     tgtpdr        compd
008900090525     c                   if        dutpou <> 046
009000090525     c                   seton                                        24
009100090721     c                   if        compd  <> d27fil
009200090525     c                   iter
009300090525     c                   end
009400090619     c                   movel     compd         compdr
009500090825     c                   else
009600091009      * profilo di sede ma richiesta singola filiale
009700091009     c                   if        d27fil > 0  and compd <> d27fil
009800091009     c                   iter
009900091009     c                   endif
010000090825     c* per sede a cambio di po salta riga
010100090825     c                   if        compd  <> savfil
010200090825     c                   eval      savfil = compd
010300090825     c                   seton                                        25
010400090825     c                   end
010500090525     c                   end
010600090525     c*
010700090525     c                   clear                   msga
010800090525     c                   clear                   msgc
010900090525     c                   clear                   msgs
011000090525     c*
011100090522     c                   eval      apdtip = 'A'
011200090522     c                   eval      apdpdr = tgtpdr
011300090522     c     kapdf         chain     fiapd01l                           97
011400090522      *se l'anagrafica non corrisponde non stampo
011500090522     c   97              iter
011600090525     c                   movel     apdrsf        apdrsfs
011700090522     c                   clear                   aitrads
011800090522     c                   clear                   aitrsds
011900090526     c* aggancio il fornitore della tariffa
012000090526     c                   movel     tgtsoc        comsoc
012100090526     c                   movel     tgtcdf        comksc
012200090526     c                   exsr      srcdf
012300090526     c* leggo i contratti
012400090617     c                   clear                   trovato_trs       1
012500091014     c     ktrs          setll     aitrs02l
012600091014     c                   do        *hival
012700091014     c     ktrs          reade     aitrs02l
012800091014     c                   if        %eof(aitrs02l)
012900091014     c                   leave
013000091014     c                   end
013100120321     c                   if        trsann <> ' ' or
013200120321     c                             trsdec > tgtddt
013300091014     c                   clear                   aitrsds
013400091014     c                   iter
013500091014     c                   end
013600120321     c                   move      'S'           trovato_trs       1
013700090526     c     apdpdr        setll     aitra03l
013800090526     c                   do        *hival
013900090526     c     apdpdr        reade     aitra03l
014000090526     c                   if        %eof(aitra03l)
014100090526     c                   leave
014200090525     c                   end
014300090526     c                   if        tranrc <> trsnrc
014400090526     c                   iter
014500090525     c                   end
014600090526     c                   if        traann <> ' '
014700090526     c                   clear                   aitrads
014800090526     c                   iter
014900090526     c                   end
015000090526      *
015100090617      *  qui ho trovato il contratto
015200090617      *
015300090617      *  x AUT NON disaccreditato esce
015400090526     c                   if        tradfi = 0
015500090526     c                   leave
015600090526     c                   end
015700090526     c*
015800091014     c                   clear                   msga
015900091116     c                   if        tgtddt > tradfi
016000091116     c                   if        (tgtdts = 0 or tgtdrc = 0 )
016100090525     c                   eval      msga = 'A'
016200091116     c                   end
016300090612     c                   else
016400090612     c* se è accreditato ma non stampato
016500090612     c                   if        tgtdcn = 0
016600090612     c                   eval      msga = 'E'
016700090612     c                   end
016800090612     c                   end
016900090526     c                   enddo
017000091014     c                   if        trovato_trs = 'S'
017100091014     c                   leave
017200091014     c                   end
017300091014     c                   enddo
017400090612     c* non accreditato
017500091014     c                   if        tradin = 0 and tgtdts <> 0
017600091116     c                             and tgtdrc = 0
017700090526     c                   eval      msga = 'A'
017800090526     c                   end
017900090525      * data di stampa
018000090424     c                   if        tgtdcn <> 0
018100090525     c                   setoff                                       30
018200090424     c                   move      tgtdcn        dataiso
018300020404     c                   move      dataiso       dataeur
018400090424     c                   move      dataeur       tgtdcns           8 0
018500090424     c                   else
018600090525     c                   seton                                        30
018700090612     c* se non accreditato non do l'errore di allegato non stmapato
018800091014     c                   if        msga = ' '  and  tgtdts <> 0 and
018900091014     c                             Trovato_trs <> 'S'
019000090525     c                   eval      msga = 'B'
019100090525     c                   end
019200090424     c                   clear                   tgtdcns
019300090424     c                   end
019400090525      * data di convalida
019500090525     c                   if        tgtdts <> 0
019600090525     c                   setoff                                       31
019700090525     c                   move      tgtdts        dataiso
019800020404     c                   move      dataiso       dataeur
019900090525     c                   move      dataeur       tgtdtss           8 0
020000090424     c                   else
020100090525     c                   seton                                        31
020200090525     c                   clear                   tgtdtss
020300090424     c                   end
020400090424     c                   if        tgtddt <> 0
020500090525     c                   setoff                                       32
020600090424     c                   move      tgtddt        dataiso
020700020404     c                   move      dataiso       dataeur
020800090424     c                   move      dataeur       tgtddts           8 0
020900090424     c                   else
021000090525     c                   seton                                        32
021100090424     c                   clear                   tgtddts
021200090424     c                   end
021300090522     c                   if        tgtdrc <> 0
021400090525     c                   setoff                                       33
021500090522     c                   move      tgtdrc        dataiso
021600090522     c                   move      dataiso       dataeur
021700090522     c                   move      dataeur       tgtdrcs           8 0
021800090522     c                   else
021900090525     c                   if        tgtdcn <> 0
022000090525     c                   eval      msga = 'C'
022100090525     c                   end
022200090525     c                   seton                                        33
022300090522     c                   clear                   tgtdrcs
022400090522     c                   end
022500090522     c* fornitori
022600090522     c                   exsr      fornitori
022700090522     c* stampa
022800090522     c                   exsr      stampa
022900020404
023000020304     c                   enddo
023100020304
023200020304     c                   endsr
023300090522     c*-------------------------------------------------
023400090522     c     fiadt         begsr
023500090522     c*-------------------------------------------------
023600090522     c                   setoff                                           99
023700090522     c     *loval        setll     fiadt01l
023800090522     c                   do        *hival
023900090522     c                   read      fiadt01l                               99
024000090522     c   99              leave
024100090522     c*
024200090522     c                   if        adtatb <> ' '
024300090522     c                   iter
024400090522     c                   end
024500091106     c* non elaboro se stampa e ricezione firma sono 20090501
024600091106     c                   if        adtdcn = 20090501 and adtdrc = 20090501
024700120607     c                             or adtnrc >=9000000
024800120607     c******************           or adtnrc = 9999999
024900090616     c                   iter
025000090616     c                   end
025100090522     c* solo con data convalida e no ricezione copia firmata
025200091014     c*                  if        adtdts = 0  or adtdrc <> 0
025300091014     c*                  iter
025400091014     c*                  end
025500090525     c* se profilo di filiale chiudo il file di stampa a rottura
025600090825     c                   movel     adtpdr        compd
025700090525     c                   if        dutpou <> 046
025800090525     c                   seton                                        24
025900090721     c                   if        compd  <> d27fil
026000090525     c                   iter
026100090525     c                   end
026200090619     c                   movel     compd         compdr
026300090825     c                   else
026400091015      * profilo di sede ma richiesta singola filiale
026500091015     c                   if        d27fil > 0  and compd <> d27fil
026600091015     c                   iter
026700091015     c                   endif
026800090825     c* per sede a cambio di po salta riga
026900090825     c                   if        compd  <> savfil
027000090825     c                   eval      savfil = compd
027100090825     c                   seton                                        25
027200090825     c                   end
027300090525     c                   end
027400090525     c*
027500090525     c                   clear                   msga
027600090525     c                   clear                   msgc
027700090525     c                   clear                   msgs
027800090525     c*
027900090522     c                   eval      apdtip = 'D'
028000090522     c                   eval      apdpdr = adtpdr
028100090522     c     kapdf         chain     fiapd01l                           97
028200090522      *se l'anagrafica non corrisponde non stampo
028300090522     c   97              iter
028400090611     c                   movel     apdrsf        apdrsfs
028500090522     c                   clear                   aitrads
028600090522     c                   clear                   aitrsds
028700090526     c* aggancio il fornitore della tariffa
028800090526     c                   movel     adtcsf        comsoc
028900090526     c                   movel     adtcdf        comksc
029000090526     c                   exsr      srcdf
029100090617     c                   clear                   trovato_trs       1
029200091014     c     ktrs          setll     aitrs02l
029300091014     c                   do        *hival
029400091014     c     ktrs          reade     aitrs02l
029500091014     c                   if        %eof(aitrs02l)
029600091014     c                   leave
029700091014     c                   end
029800120321     c                   if        trsann <> ' ' or
029900120321     c                             trsdec > adtddt
030000091014     c                   clear                   aitrsds
030100091014     c                   iter
030200091014     c                   end
030300120321     c                   move      'S'           trovato_trs       1
030400090526     c     apdpdr        setll     aitra05l
030500090526     c                   do        *hival
030600090526     c     apdpdr        reade     aitra05l
030700090526     c                   if        %eof(aitra05l)
030800090526     c                   leave
030900090526     c                   end
031000090526     c                   if        tranrc <> trsnrc
031100090526     c                   iter
031200090526     c                   end
031300090526     c                   if        traann <> ' '
031400090526     c                   clear                   aitrads
031500090526     c                   iter
031600090526     c                   end
031700090617      *
031800090617      *  qui ho trovato il contratto
031900090526      *
032000090526     c                   if        tradfi = 0
032100090526     c                   leave
032200090526     c                   end
032300090526     c*
032400091014     c                   clear                   msga
032500090612     c                   if        adtddt > tradfi
032600091116     c                   if        (adtdts = 0 or adtdrc = 0 )
032700090526     c                   eval      msga = 'A'
032800091116     c                   end
032900090612     c                   else
033000090612     c* se è accreditato ma non stampato
033100090612     c                   if        adtdcn = 0
033200090612     c                   eval      msga = 'E'
033300090612     c                   end
033400090526     c                   end
033500090526     c                   enddo
033600091014     c                   if        trovato_trs = 'S'
033700091014     c                   leave
033800091014     c                   end
033900091014     c                   enddo
034000090526     c*
034100091014     c                   if        tradin = 0 and adtdts <> 0
034200091116     c                             and adtdrc = 0
034300090526     c                   eval      msga = 'A'
034400090612     c                   end
034500090525      * data di stampa
034600090522     c                   if        adtdcn <> 0
034700090525     c                   setoff                                       30
034800090522     c                   move      adtdcn        dataiso
034900090522     c                   move      dataiso       dataeur
035000090522     c                   move      dataeur       tgtdcns           8 0
035100090522     c                   else
035200090612     c                   seton                                        30
035300090612     c* se non accreditato non do l'errore di allegato non stmapato
035400091014     c                   if        msga = ' ' and adtdts <> 0
035500090617     c                             and trovato_trs <> 'S'
035600090525     c                   eval      msga = 'B'
035700090525     c                   end
035800090522     c                   clear                   tgtdcns
035900090522     c                   end
036000090525     c                   if        adtdts <> 0
036100090525     c                   setoff                                       31
036200090525     c                   move      adtdts        dataiso
036300090522     c                   move      dataiso       dataeur
036400090525     c                   move      dataeur       tgtdtss           8 0
036500090522     c                   else
036600090525     c                   seton                                        31
036700090525     c                   clear                   tgtdtss
036800090522     c                   end
036900090522     c                   if        adtddt <> 0
037000090525     c                   setoff                                       32
037100090522     c                   move      adtddt        dataiso
037200090522     c                   move      dataiso       dataeur
037300090522     c                   move      dataeur       tgtddts           8 0
037400090522     c                   else
037500090525     c                   seton                                        32
037600090522     c                   clear                   tgtddts
037700090522     c                   end
037800090522     c                   if        adtdrc <> 0
037900090525     c                   setoff                                       33
038000090522     c                   move      adtdrc        dataiso
038100090522     c                   move      dataiso       dataeur
038200090522     c                   move      dataeur       tgtdrcs           8 0
038300090522     c                   else
038400090525     c                   seton                                        33
038500090525     c                   if        adtdcn <> 0
038600090525     c                   eval      msga = 'C'
038700090525     c                   end
038800090522     c                   clear                   tgtdrcs
038900090522     c                   end
039000090522     c* fornitori
039100090525     c                   movel     adtcsf        comksc
039200090525     c                   movel     adtcdf        comksc
039300090522     c                   exsr      fornitori
039400090522     c* stampa
039500090522     c                   exsr      stampa
039600090522
039700090522     c                   enddo
039800090522
039900090522     c                   endsr
040000090522      * ?___________________________________________________________________
040100090522     C     stampa        BegSR
040200090522      * ?___________________________________________________________________
040300090525     c                   if        *in23
040400090525     c                   write     testa
040500090525     c                   setoff                                       23
040600090522     c                   end
040700090525     c                   if        apdtip = 'A'
040800090525     c                   eval      tipo = 'C'
040900090525     c                   else
041000090525     c                   eval      tipo = 'A'
041100090525     c                   end
041200090825     c                   eval      trsnrcs = trsnrc
041300090612     c*
041400090522     c                   if        tradin <> 0
041500090525     c                   setoff                                       34
041600090522     c                   move      tradin        dataiso
041700090522     c                   move      dataiso       dataeur
041800090522     c                   move      dataeur       tradins           8 0
041900090522     c                   else
042000090525     c                   seton                                        34
042100090522     c                   clear                   tradins
042200090522     c                   end
042300090525     c                   if        tradfi <> 0
042400090525     c                   setoff                                       38
042500090525     c                   move      tradfi        dataiso
042600090525     c                   move      dataiso       dataeur
042700090525     c                   move      dataeur       tradfis           8 0
042800090525     c                   else
042900090525     c                   seton                                        38
043000090525     c                   clear                   tradfis
043100090525     c                   end
043200090522     c                   if        trsdsc <> 0
043300090525     c                   setoff                                       35
043400090522     c                   move      trsdsc        dataiso
043500090522     c                   move      dataiso       dataeur
043600090522     c                   move      dataeur       trsdscs           8 0
043700090522     c                   else
043800090525     c                   seton                                        35
043900090522     c                   clear                   trsdscs
044000090522     c                   end
044100090522     c                   if        trsdrc <> 0
044200090525     c                   setoff                                       36
044300090522     c                   move      trsdrc        dataiso
044400090522     c                   move      dataiso       dataeur
044500090522     c                   move      dataeur       trsdrcs           8 0
044600090522     c                   else
044700090525     c                   if        trsdsc <> 0
044800090525     c                   eval      msgc = 'D'
044900090525     c                   end
045000090525     c                   seton                                        36
045100090522     c                   clear                   trsdrcs
045200090522     c                   end
045300090612     c* evidenzio le anomalie che sono a rischio
045400090612     c                   setoff                                       404142
045500091014     c                   if        tgtdtss <> 0
045600090612     c                   move      tgtdtss       dataeur
045700090612     c                   move      dataeur       dtaisodts
045800091014     c                   else
045900091014     c                   movel     *loval        dtaisodts
046000091014     c                   end
046100090612     c* allegato
046200090612     c     isooggi       subdur    dtaisodts     gg:*d
046300090612     c                   if        (msga = 'A' and
046400090612     c                             gg > §5AGGERRA) or
046500090612     c                             (msga = 'B' and
046600090612     c                             gg > §5AGGERRB) or
046700090612     c                             (msga = 'C' and
046800090612     c                             gg > §5AGGERRC)
046900090612     c                   seton                                        4041
047000090612     c                   end
047100090612     c* contratto
047200090612     c                   if        (msgc = 'D' and
047300090612     c                             gg > §5AGGERRD)
047400090612     c                   seton                                        4042
047500090612     c                   end
047600090617      *
047700090616     c                   exsr      somma_TOTali
047800090617      *
047900090612     c* stampo solo se ho evidenziato l'anomalia
048000090612     c                   if        (soloano = 'S' and *in40 ) or
048100090612     c                             soloano = ' '
048200090617     c                   if             msga <> *blank or
048300090617     c                                  msgc <> *blank or
048400090617     c                                  msgs <> *blank
048500090825     c   25              write     riga1
048600090825     c                   write     riga
048700090825     c                   setoff                                       25
048800090617      * emette 2° riga
048900090617     c                   if             msga = 'B'
049000090617     c                   eval      soctra = trasoc
049100090617     c                   if             SOCTRA <> *blank
049200090617     c                   write     riga_b
049300090617     c                   end
049400090617     c                   end
049500090617     c                   end
049600090617      *
049700090612     c                   eval      una = '1'
049800090612     c                   end
049900090522     c                   endsr
050000090522      * ?___________________________________________________________________
050100090522     C     fornitori     BegSR
050200090522      * ?___________________________________________________________________
050300090522     c                   if        trsiva <> ' '
050400090522     c                   exsr      ragsoccont
050500090525     c                   else
050600090525     c                   clear                   RAGSOCKSCs
050700090522     c                   end
050800090526     c                   if        trsiva <> comiva and trsiva <> ' '
050900090617     c                   if        msga = ' '
051000090617     c                   eval      msga = 'B'
051100090617     c                   end
051200090525     c                   seton                                        37
051300090525     c                   else
051400090525     c                   setoff                                       37
051500090522     c                   end
051600090522     c                   endsr
051700090526      * ?___________________________________________________________________
051800090522     c     srcdf         begsr
051900090526      * ?___________________________________________________________________
052000090525     c                   clear                   sogdess
052100090526     c                   clear                   comiva
052200090522     c     kfrn          chain     anfrn01l
052300090522     c                   if        %found(anfrn01l)
052400090522     c     frnsogg       chain     ansog01l
052500090525     c                   if        %found(ansog01l)
052600090525     c                   movel     sogdes        sogdess
052700090526     c                   movel     sogpartiva    comiva
052800090525     c                   else
052900090526     c                   clear                   comiva
053000090522     c                   end
053100090525     c                   end
053200090522     c                   endsr
053300090522      * ?___________________________________________________________________
053400090522     C     RagSocCont    BegSR
053500090522      * ?___________________________________________________________________
053600090522     C                   clear                   Trmz70s1ds                     Output
053700090522     C                   movel(p)  TRSiva        PartitaIVA                     Input
053800090522     C                   movel(p)  'F'           SottoNatur                     Input "F/C"
053900090522     C                   movel(p)  TRSFIL        Unita                          Input/Output
054000090608     C                   movel(p)  TRSsocg       societa                        Input/Output
054100090522      *
054200090522     c                   call      'TRMZ70SR1'
054300090522     C                   PARM                    trmz70s1ds
054400090522     c                   if        errore <> '0'
054500090525     c                   eval      msgs = '9'
054600090525     c                   clear                   RAGSOCKSCs
054700090525     c                   else
054800090525     c                   eval      RAGSOCKSCs = RAGSOCKSC
054900090522     c                   endif
055000090522     c*
055100090522     C                   ENDSR
055200090616     c*-------------------------------------------------
055300090616     c     azzera_TOTali begsr
055400090616     c*
055500090616     c                   clear                   msgATOT           5 0
055600090616     c                   clear                   rscATOT           5 0
055700090616     c                   clear                   msgBTOT           5 0
055800090616     c                   clear                   rscBTOT           5 0
055900090616     c                   clear                   msgCTOT           5 0
056000090616     c                   clear                   rscCTOT           5 0
056100090616     c                   clear                   msgDTOT           5 0
056200090616     c                   clear                   rscDTOT           5 0
056300090616     c                   clear                   msgETOT           5 0
056400090617     c                   clear                   rscETOT           5 0
056500090616     c                   clear                   msg9TOT           5 0
056600090617     c                   clear                   msgATOT_c         5 0
056700090617     c                   clear                   rscATOT_c         5 0
056800090617     c                   clear                   msgBTOT_c         5 0
056900090617     c                   clear                   rscBTOT_c         5 0
057000090617     c                   clear                   msgCTOT_c         5 0
057100090617     c                   clear                   rscCTOT_c         5 0
057200090617     c                   clear                   msgDTOT_c         5 0
057300090617     c                   clear                   rscDTOT_c         5 0
057400090617     c                   clear                   msgETOT_c         5 0
057500090617     c                   clear                   rscETOT_c         5 0
057600090617     c                   clear                   msg9TOT_c         5 0
057700090617     c                   clear                   msgATOT_a         5 0
057800090617     c                   clear                   rscATOT_a         5 0
057900090617     c                   clear                   msgBTOT_a         5 0
058000090617     c                   clear                   rscBTOT_a         5 0
058100090617     c                   clear                   msgCTOT_a         5 0
058200090617     c                   clear                   rscCTOT_a         5 0
058300090617     c                   clear                   msgDTOT_a         5 0
058400090617     c                   clear                   rscDTOT_a         5 0
058500090617     c                   clear                   msgETOT_a         5 0
058600090617     c                   clear                   rscETOT_a         5 0
058700090617     c                   clear                   msg9TOT_a         5 0
058800090616     c                   clear                   Senza_contrA      5 0
058900090616     c                   clear                   Senza_contrB      5 0
059000090616     c                   clear                   Senza_contrC      5 0
059100090616     c                   clear                   Senza_contrD      5 0
059200090616     c                   clear                   Senza_contrE      5 0
059300090616     c                   clear                   Senza_contr9      5 0
059400090617     c                   clear                   Senza_contrA_c    5 0
059500090617     c                   clear                   Senza_contrB_c    5 0
059600090617     c                   clear                   Senza_contrC_c    5 0
059700090617     c                   clear                   Senza_contrD_c    5 0
059800090617     c                   clear                   Senza_contrE_c    5 0
059900090617     c                   clear                   Senza_contr9_c    5 0
060000090617     c                   clear                   Senza_contrA_a    5 0
060100090617     c                   clear                   Senza_contrB_a    5 0
060200090617     c                   clear                   Senza_contrC_a    5 0
060300090617     c                   clear                   Senza_contrD_a    5 0
060400090617     c                   clear                   Senza_contrE_a    5 0
060500090617     c                   clear                   Senza_contr9_a    5 0
060600090616     c*
060700090616     C                   ENDSR
060800090616     c*-------------------------------------------------
060900090616     c     somma_TOTali  begsr
061000090616     c*
061100090616     c                   if        msgA ='A'
061200090616     c                   add       1             msgATOT
061300090616     c   41              add       1             rscATOT
061400090616     c                   if        TRSDSCS = 0
061500090616     c                   add       1             Senza_contrA
061600090616     c                   end
061700090617      *
061800090617     c                   if        tipo = 'C'
061900090617     c                   add       1             msgATOT_c
062000090617     c   41              add       1             rscATOT_c
062100090617     c                   if        TRSDSCS = 0
062200090617     c                   add       1             Senza_contrA_c
062300090617     c                   end
062400090617     c                   elseIf    tipo = 'A'
062500090617     c                   add       1             msgATOT_a
062600090617     c   41              add       1             rscATOT_a
062700090617     c                   if        TRSDSCS = 0
062800090617     c                   add       1             Senza_contrA_a
062900090617     c                   end
063000090617     c                   end
063100090617      *
063200090616     c                   elseIf    msgA ='B'
063300090616     c                   add       1             msgBTOT
063400090616     c   41              add       1             rscBTOT
063500090616     c                   if        TRSDSCS = 0
063600090616     c                   add       1             Senza_contrB
063700090617     c                   end
063800090617      *
063900090617     c                   if        tipo = 'C'
064000090617     c                   add       1             msgBTOT_c
064100090617     c   41              add       1             rscBTOT_c
064200090617     c                   if        TRSDSCS = 0
064300090617     c                   add       1             Senza_contrB_c
064400090617     c                   end
064500090617     c                   elseIf    tipo = 'A'
064600090617     c                   add       1             msgBTOT_a
064700090617     c   41              add       1             rscBTOT_a
064800090617     c                   if        TRSDSCS = 0
064900090617     c                   add       1             Senza_contrB_a
065000090617     c                   end
065100090617     c                   end
065200090617      *
065300090617     c                   elseIf    msgA ='C'
065400090617     c                   add       1             msgCTOT
065500090617     c   41              add       1             rscCTOT
065600090617     c                   if        TRSDSCS = 0
065700090617     c                   add       1             Senza_contrC
065800090616     c                   end
065900090617      *
066000090617     c                   if        tipo = 'C'
066100090617     c                   add       1             msgCTOT_c
066200090617     c   41              add       1             rscCTOT_c
066300090617     c                   if        TRSDSCS = 0
066400090617     c                   add       1             Senza_contrC_c
066500090617     c                   end
066600090617     c                   elseIf    tipo = 'A'
066700090617     c                   add       1             msgCTOT_a
066800090617     c   41              add       1             rsCCTOT_a
066900090617     c                   if        TRSDSCS = 0
067000090617     c                   add       1             Senza_contrC_a
067100090617     c                   end
067200090617     c                   end
067300090617      *
067400090617     c                   elseIf    msgA ='E'
067500090617     c                   add       1             msgETOT
067600090617     c   41              add       1             rscETOT
067700090617     c                   if        TRSDSCS = 0
067800090617     c                   add       1             Senza_contrE
067900090617     c                   end
068000090617      *
068100090617     c                   if        tipo = 'C'
068200090617     c                   add       1             msgETOT_c
068300090617     c   41              add       1             rscETOT_c
068400090617     c                   if        TRSDSCS = 0
068500090617     c                   add       1             Senza_contrE_c
068600090617     c                   end
068700090617     c                   elseIf    tipo = 'A'
068800090617     c                   add       1             msgETOT_a
068900090617     c   41              add       1             rsCETOT_a
069000090617     c                   if        TRSDSCS = 0
069100090617     c                   add       1             Senza_contrE_a
069200090617     c                   end
069300090617     c                   end
069400090617      *
069500090616     c                   end
069600090616     c*
069700090619     c*  Le D vanno raggruppate per contratto quindi conta x una volta sola
069800090617     c                   if        msgC ='D'
069900090619      **
070000090619     c     trsnrc        lookup    nrc                                    55
070100090619     c                   if        not *in55
070200090619     c                   z-add     1             y                 3 0
070300090619     c     *zero         lookup    nrc(y)                                 55
070400090619     c                   eval      nrc(y) = trsnrc
070500090619      **
070600090617     c                   add       1             msgDTOT
070700090617     c   42              add       1             rscDTOT
070800090616     c                   if        TRSDSCS = 0
070900090617     c                   add       1             Senza_contrD
071000090616     c                   end
071100090617      *
071200090617     c                   if        tipo = 'C'
071300090617     c                   add       1             msgDTOT_c
071400090617     c   42              add       1             rscDTOT_c
071500090617     c                   if        TRSDSCS = 0
071600090617     c                   add       1             Senza_contrD_c
071700090617     c                   end
071800090617     c                   elseIf    tipo = 'A'
071900090617     c                   add       1             msgDTOT_a
072000090617     c   42              add       1             rsCDTOT_a
072100090617     c                   if        TRSDSCS = 0
072200090617     c                   add       1             Senza_contrD_a
072300090617     c                   end
072400090617     c                   end
072500090619      ***
072600090619     c                   end
072700090617      *
072800090616     c                   end
072900090616      *
073000090616     c                   if        msgS ='9'
073100090616     c                   add       1             msg9TOT
073200090616     c                   if        TRSDSCS = 0
073300090616     c                   add       1             Senza_contr9
073400090616     c                   end
073500090617      *
073600090617     c                   if        tipo = 'C'
073700090617     c                   add       1             msg9TOT_c
073800090617     c                   if        TRSDSCS = 0
073900090617     c                   add       1             Senza_contr9_c
074000090617     c                   end
074100090617     c                   elseIf    tipo = 'A'
074200090617     c                   add       1             msg9TOT_a
074300090617     c                   if        TRSDSCS = 0
074400090617     c                   add       1             Senza_contr9_a
074500090617     c                   end
074600090617     c                   end
074700090617      *
074800090616     c                   end
074900090616      *
075000090616     C                   ENDSR
075100011026     c*-------------------------------------------------
075200090616     c     Totali        begsr
075300011026     c*-------------------------------------------------
075400090616      *
075500090616     c                   movel     'A'           tipoERR
075600090616     c                   z-add     msgATOT       numERR
075700090616     c                   z-add     rscATOT       rscERR
075800090617     c                   z-add     Senza_contrA  NoContra
075900090617     c                   z-add     msgATOT_c     numERRc
076000090617     c                   z-add     rscATOT_c     rscERRc
076100090617     c                   z-add     Senza_contrA_cNoContraC
076200090617     c                   z-add     msgATOT_a     numERRa
076300090617     c                   z-add     rscATOT_a     rscERRa
076400090617     c                   z-add     Senza_contrA_aNoContraA
076500090616     c                   if        *in23
076600090616     c                   write     testa
076700090616     c                   write     testaTOT
076800090616     c                   setoff                                       23
076900090616     c                   else
077000090616     c                   write     testaTOT
077100090616     c                   end
077200090616     c                   write     rigaTOT
077300090616      *
077400090616     c                   movel     'B'           tipoERR
077500090616     c                   z-add     msgBTOT       numERR
077600090616     c                   z-add     rscBTOT       rscERR
077700090616     c                   z-add     Senza_contrB  NoContra
077800090617     c                   z-add     msgBTOT_c     numERRc
077900090617     c                   z-add     rscBTOT_c     rscERRc
078000090617     c                   z-add     Senza_contrB_cNoContraC
078100090617     c                   z-add     msgBTOT_a     numERRa
078200090617     c                   z-add     rscBTOT_a     rscERRa
078300090617     c                   z-add     Senza_contrB_aNoContraA
078400090616     c                   if        *in23
078500090616     c                   write     testa
078600090616     c                   write     testaTOT
078700090616     c                   setoff                                       23
078800090616     c                   end
078900090616     c                   write     rigaTOT
079000090616      *
079100090616     c                   movel     'C'           tipoERR
079200090616     c                   z-add     msgCTOT       numERR
079300090616     c                   z-add     rscCTOT       rscERR
079400090616     c                   z-add     Senza_contrC  NoContra
079500090617     c                   z-add     msgCTOT_c     numERRc
079600090617     c                   z-add     rscCTOT_c     rscERRc
079700090617     c                   z-add     Senza_contrC_cNoContraC
079800090617     c                   z-add     msgCTOT_a     numERRa
079900090617     c                   z-add     rscCTOT_a     rscERRa
080000090617     c                   z-add     Senza_contrC_aNoContraA
080100090616     c                   if        *in23
080200090616     c                   write     testa
080300090616     c                   write     testaTOT
080400090616     c                   setoff                                       23
080500090616     c                   end
080600090616     c                   write     rigaTOT
080700090616      *
080800090616     c                   movel     'D'           tipoERR
080900090616     c                   z-add     msgDTOT       numERR
081000090616     c                   z-add     rscDTOT       rscERR
081100090616     c                   z-add     Senza_contrD  NoContra
081200090617     c                   z-add     msgDTOT_c     numERRc
081300090617     c                   z-add     rscDTOT_c     rscERRc
081400090617     c                   z-add     Senza_contrD_cNoContraC
081500090617     c                   z-add     msgDTOT_a     numERRa
081600090617     c                   z-add     rscDTOT_a     rscERRa
081700090617     c                   z-add     Senza_contrD_aNoContraA
081800090616     c                   if        *in23
081900090616     c                   write     testa
082000090616     c                   write     testaTOT
082100090616     c                   setoff                                       23
082200090616     c                   end
082300090616     c                   write     rigaTOT
082400090616      *
082500090616     c                   movel     'E'           tipoERR
082600090616     c                   z-add     msgETOT       numERR
082700090617     c                   z-add     rscETOT       rscERR
082800090616     c                   z-add     Senza_contrE  NoContra
082900090617     c                   z-add     msgETOT_c     numERRc
083000090617     c                   z-add     rscETOT_c     rscERRc
083100090617     c                   z-add     Senza_contrE_cNoContraC
083200090617     c                   z-add     msgETOT_a     numERRa
083300090617     c                   z-add     rscETOT_a     rscERRa
083400090617     c                   z-add     Senza_contrE_aNoContraA
083500090616     c                   if        *in23
083600090616     c                   write     testa
083700090616     c                   write     testaTOT
083800090616     c                   setoff                                       23
083900090616     c                   end
084000090616     c                   write     rigaTOT
084100090616      *
084200090616     c                   movel     '9'           tipoERR
084300090616     c                   z-add     msg9TOT       numERR
084400090616     c                   z-add     0             rscERR
084500090616     c                   z-add     Senza_contr9  NoContra
084600090617     c                   z-add     msg9TOT_c     numERRc
084700090617     c                   z-add     0             rscERRc
084800090617     c                   z-add     Senza_contr9_cNoContraC
084900090617     c                   z-add     msg9TOT_a     numERRa
085000090617     c                   z-add     0             rscERRa
085100090617     c                   z-add     Senza_contr9_aNoContraA
085200090616     c                   if        *in23
085300090616     c                   write     testa
085400090616     c                   write     testaTOT
085500090616     c                   setoff                                       23
085600090616     c                   end
085700090616     c                   write     rigaTOT
085800090616      *
085900090616      *
086000090616     C                   ENDSR
086100090616     c*-------------------------------------------------
086200090616     c     *inzsr        begsr
086300090616     c*-------------------------------------------------
086400020304     c     *entry        plist
086500020304     c                   parm                    kpjba
086600090721     c                   movel     kpjbu         ficn27ds
086700090721     c                   movel     d27solor      soloano           1
086800090525     c                   seton                                        23
086900090525     c                   eval      nompgm = 'FICN28R'
087000090525     c     *dtaara       define    §azute        azuteds
087100090525     c     *dtaara       define    §datiute      ddatiute
087200090525     C                   in(E)     *dtaara
087300090612     C
087400090525     C                   call      'TIBS34R'
087500090525     C                   parm                    Tibs34Ds
087600090525     C                   in        *dtaara
087700090525     c                   movel     *all'_'       cmp198          198
087800090525     c                   movel     *all'_'       cmp198a         198
087900090617     c                   movel     *all'_'       cmp139          139
088000090617     c                   movel     *all'_'       cmp139a         139
088100090617      *
088200090617      * stampa la legenda in testa all'inizio della stampa
088300090617     c                   seton                                        93
088400090617      *
088500020218     C                   time                    w0120            14 0
088600090525     C                   move      w0120         woggi
088700090612     C                   MOVE      W0120         UDATE8            8 0
088800090612     C                   Z-ADD     UDATE8        G02DAT
088900090612     C                   MOVE      *ZEROS        G02INV
089000090612     C                   MOVE      *BLANKS       G02ERR
089100090612     C                   CALL      'XSRDA8'
089200090612     C                   PARM                    WLBDAT
089300090612     C                   MOVE      g02inv        isooggi
089400090525     C                   movel     w0120         times
089500090612     C     KTAB          KLIST
089600090612     C                   KFLD                    tblkut
089700090612     C                   KFLD                    tblcod
089800090612     C                   KFLD                    tblkey
089900090612     C                   Z-ADD     1             tblkut
090000021203     c     kapdf         klist
090100090424     c                   kfld                    apdtip
090200090525     c                   kfld                    apdpdr
090300090424     c                   movel     'A'           apdtip
090400090525     C     Kfrn          KLIST
090500090525     C                   KFLD                    comsoc
090600090525     C                   KFLD                    comksc
090700090526     C     Ktrs          KLIST
090800090526     C                   KFLD                    comsoc
090900090526     C                   KFLD                    comiva
091000090612     C***  giorni da calcolare x date limite
091100090612     C                   clear                   ds5Aaut
091200090612     C                   movel     '5A'          tblcod
091300090612     C                   MOVEL(P)  'AUT'         tblKEY
091400090612     C     KTAB          CHAIN     TABEL00F                           30
091500090612     c                   if        %found(tabel00f) and tblflg = ' '
091600090612     c                   movel     tbluni        ds5aaut
091700090619      **
091800090619      **  Attenzione se ci si trova nel periodo delle Trazioni Ridotte deve
091900090619      **  incrementare di x giorni i giorni di rischio per i 4 tipi di errori
092000090619     c                   if             §5ADTAINI <= Udate8 and
092100090619     c                                  §5ADTAFIN >= Udate8
092200090619     c                   eval      §5AGGERRa = §5AGGERRa + §5AGGINPIU
092300090619     c                   eval      §5AGGERRb = §5AGGERRb + §5AGGINPIU
092400090619     c                   eval      §5AGGERRc = §5AGGERRc + §5AGGINPIU
092500090619     c                   eval      §5AGGERRd = §5AGGERRd + §5AGGINPIU
092600090619     c                   end
092700090619      **
092800090612     c                   end
092900090617      *
093000090617     c                   write     leggenda
093100090617     c                   setoff                                       23
093200090617      *
093300070307     c                   endsr
