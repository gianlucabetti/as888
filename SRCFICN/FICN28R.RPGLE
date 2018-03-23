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
001800120813     Fazorg01l  if   e           k disk
001900090617      *
002000090617     D PRTFBK          ds
002100090617     D   CUR_LINE            367    368i 0                                      * Current line num
002200090617     D   CUR_PAGE            369    372i 0                                      * Current page num
002300090617      *
002400020304     dkpjba          e ds
002500090612     D ds5Aaut       E DS
002600090721     D ficn27ds      e ds
002700090721     D ddatiute      e ds
002800090525     D azuteds       e ds                  extname(AZUTE00F)
002900090525     D aitrads       e ds                  extname(aitra00f)
003000090525     D aitrsds       e ds                  extname(aitrs00f)
003100090525     D tibs34ds      E DS                  inz
003200090525     D trmz70s1ds    E DS                  inz
003300090619     d NRC             s              7  0 DIM(100)
003400090612     D WLBDAT          DS
003500090612     D  G02DAT                 1      8  0
003600090612     D  G02INV                 9     16  0
003700090612     D  G02ERR                17     17
003800090612     D  G02TGI                18     22  0
003900090612     d dtaisodts       s               d   datfmt(*iso)
004000090612     d isooggi         s               d   datfmt(*iso)
004100090612     d dataiso         s               d   datfmt(*iso)
004200020305     d dataeur         s               d   datfmt(*eur)
004300090525     d compdr          s              3  0
004400090619     d compd           s              3  0
004500090825     d savfil          s              3  0
004600090612     d gg              s              3  0
004700090525     d comsoc          s              3
004800090526     d comiva          s             16
004900090525     d comksc          s              8
005000090617     d conta_righe     s              3  0
005100000000     C*---------------------------------------------------------------*
005200090616     c                   exsr      azzera_TOTali
005300090616     c*
005400090522     c                   exsr      fitgt
005500020304      *
005600090522     c                   exsr      fiadt
005700090616     c* se stampato almeno una riga emetto leggenda
005800090522     c                   if        una <> ' '
005900090617      *  Forza il salto pagina per stampare i totali sulla pagina seguente
006000090617     c                   if        CUR_LINE >= 55
006100090617     c                   seton                                        23
006200090617     c                   end
006300090616     c                   exsr      totali
006400090522     c                   end
006500120808      * se utente di filiale di secondo livello non lancia stampe AFF/def
006501120926     c*                  if        dutlpo =  'S'
006600120926     c                   if        dutlpo <> '2' and level <> '2'
006700120808     C                   CALL      'TRMZA4R'
006800120808     C                   PARM                    KPJBA
006900120808     C                   CALL      'TRMZA3R'
007000120808     C                   PARM                    KPJBA
007100120808     c                   endif
007200090616      *
007300020308     c                   seton                                        lr
007400990113     c*-------------------------------------------------
007500090424     c     fitgt         begsr
007600990113     c*-------------------------------------------------
007700990113     c                   setoff                                           99
007800020404     c                   clear                   una               1
007900090424     c     *loval        setll     fitgt01l
008000981029     c                   do        *hival
008100090424     c                   read      fitgt01l                               99
008200981029     c   99              leave
008300090424     c*
008400090424     c                   if        tgtsml <> ' ' or tgtatb <> ' '
008500090424     c                   iter
008600090424     c                   end
008700091106     c* non elaboro se stampa e ricezione firma sono 20090501
008800091106     c                   if        tgtdcn = 20090501 and tgtdrc = 20090501
008900090525     c                   iter
009000090525     c                   end
009100090522     c* solo con data convalida e no ricezione copia firmata
009200091014     c*                  if        tgtdts = 0  or tgtdrc <> 0
009300091014     c*                  iter
009400091014     c*                  end
009500090525     c* se profilo di filiale chiudo il file di stampa a rottura
009600090825     c                   movel     tgtpdr        compd
009700090525     c                   if        dutpou <> 046
009800090525     c                   seton                                        24
009900090721     c                   if        compd  <> d27fil
010000090525     c                   iter
010100090525     c                   end
010200090619     c                   movel     compd         compdr
010300090825     c                   else
010400091009      * profilo di sede ma richiesta singola filiale
010500091009     c                   if        d27fil > 0  and compd <> d27fil
010600091009     c                   iter
010700091009     c                   endif
010800090825     c* per sede a cambio di po salta riga
010900090825     c                   if        compd  <> savfil
011000090825     c                   eval      savfil = compd
011100090825     c                   seton                                        25
011200090825     c                   end
011300090525     c                   end
011400090525     c*
011500090525     c                   clear                   msga
011600090525     c                   clear                   msgc
011700090525     c                   clear                   msgs
011800090525     c*
011900090522     c                   eval      apdtip = 'A'
012000090522     c                   eval      apdpdr = tgtpdr
012100090522     c     kapdf         chain     fiapd01l                           97
012200090522      *se l'anagrafica non corrisponde non stampo
012300090522     c   97              iter
012400090525     c                   movel     apdrsf        apdrsfs
012500090522     c                   clear                   aitrads
012600090522     c                   clear                   aitrsds
012700090526     c* aggancio il fornitore della tariffa
012800090526     c                   movel     tgtsoc        comsoc
012900090526     c                   movel     tgtcdf        comksc
013000090526     c                   exsr      srcdf
013100090526     c* leggo i contratti
013200090617     c                   clear                   trovato_trs       1
013300091014     c     ktrs          setll     aitrs02l
013400091014     c                   do        *hival
013500091014     c     ktrs          reade     aitrs02l
013600091014     c                   if        %eof(aitrs02l)
013700091014     c                   leave
013800091014     c                   end
013900120321     c                   if        trsann <> ' ' or
014000120321     c                             trsdec > tgtddt
014100091014     c                   clear                   aitrsds
014200091014     c                   iter
014300091014     c                   end
014400120321     c                   move      'S'           trovato_trs       1
014500090526     c     apdpdr        setll     aitra03l
014600090526     c                   do        *hival
014700090526     c     apdpdr        reade     aitra03l
014800090526     c                   if        %eof(aitra03l)
014900090526     c                   leave
015000090525     c                   end
015100090526     c                   if        tranrc <> trsnrc
015101120925     c                   clear                   aitrads
015200090526     c                   iter
015300090525     c                   end
015400090526     c                   if        traann <> ' '
015500090526     c                   clear                   aitrads
015600090526     c                   iter
015700090526     c                   end
015800090526      *
015900090617      *  qui ho trovato il contratto
016000090617      *
016100090617      *  x AUT NON disaccreditato esce
016200090526     c                   if        tradfi = 0
016300090526     c                   leave
016400090526     c                   end
016500090526     c*
016600091014     c                   clear                   msga
016700091116     c                   if        tgtddt > tradfi
016800091116     c                   if        (tgtdts = 0 or tgtdrc = 0 )
016900090525     c                   eval      msga = 'A'
017000091116     c                   end
017100090612     c                   else
017200090612     c* se è accreditato ma non stampato
017300090612     c                   if        tgtdcn = 0
017400090612     c                   eval      msga = 'E'
017500090612     c                   end
017600090612     c                   end
017700090526     c                   enddo
017800091014     c                   if        trovato_trs = 'S'
017900091014     c                   leave
018000091014     c                   end
018100091014     c                   enddo
018200090612     c* non accreditato
018300091014     c                   if        tradin = 0 and tgtdts <> 0
018400091116     c                             and tgtdrc = 0
018500090526     c                   eval      msga = 'A'
018600090526     c                   end
018700090525      * data di stampa
018800090424     c                   if        tgtdcn <> 0
018900090525     c                   setoff                                       30
019000090424     c                   move      tgtdcn        dataiso
019100020404     c                   move      dataiso       dataeur
019200090424     c                   move      dataeur       tgtdcns           8 0
019300090424     c                   else
019400090525     c                   seton                                        30
019500090612     c* se non accreditato non do l'errore di allegato non stmapato
019600091014     c                   if        msga = ' '  and  tgtdts <> 0 and
019700091014     c                             Trovato_trs <> 'S'
019800090525     c                   eval      msga = 'B'
019900090525     c                   end
020000090424     c                   clear                   tgtdcns
020100090424     c                   end
020200090525      * data di convalida
020300090525     c                   if        tgtdts <> 0
020400090525     c                   setoff                                       31
020500090525     c                   move      tgtdts        dataiso
020600020404     c                   move      dataiso       dataeur
020700090525     c                   move      dataeur       tgtdtss           8 0
020800090424     c                   else
020900090525     c                   seton                                        31
021000090525     c                   clear                   tgtdtss
021100090424     c                   end
021200090424     c                   if        tgtddt <> 0
021300090525     c                   setoff                                       32
021400090424     c                   move      tgtddt        dataiso
021500020404     c                   move      dataiso       dataeur
021600090424     c                   move      dataeur       tgtddts           8 0
021700090424     c                   else
021800090525     c                   seton                                        32
021900090424     c                   clear                   tgtddts
022000090424     c                   end
022100090522     c                   if        tgtdrc <> 0
022200090525     c                   setoff                                       33
022300090522     c                   move      tgtdrc        dataiso
022400090522     c                   move      dataiso       dataeur
022500090522     c                   move      dataeur       tgtdrcs           8 0
022600090522     c                   else
022700090525     c                   if        tgtdcn <> 0
022800090525     c                   eval      msga = 'C'
022900090525     c                   end
023000090525     c                   seton                                        33
023100090522     c                   clear                   tgtdrcs
023200090522     c                   end
023300090522     c* fornitori
023400090522     c                   exsr      fornitori
023500090522     c* stampa
023600090522     c                   exsr      stampa
023700020404
023800020304     c                   enddo
023900020304
024000020304     c                   endsr
024100090522     c*-------------------------------------------------
024200090522     c     fiadt         begsr
024300090522     c*-------------------------------------------------
024400090522     c                   setoff                                           99
024500090522     c     *loval        setll     fiadt01l
024600090522     c                   do        *hival
024700090522     c                   read      fiadt01l                               99
024800090522     c   99              leave
024900090522     c*
025000090522     c                   if        adtatb <> ' '
025100090522     c                   iter
025200090522     c                   end
025300091106     c* non elaboro se stampa e ricezione firma sono 20090501
025400091106     c                   if        adtdcn = 20090501 and adtdrc = 20090501
025500120607     c                             or adtnrc >=9000000
025600120607     c******************           or adtnrc = 9999999
025700090616     c                   iter
025800090616     c                   end
025900090522     c* solo con data convalida e no ricezione copia firmata
026000091014     c*                  if        adtdts = 0  or adtdrc <> 0
026100091014     c*                  iter
026200091014     c*                  end
026300090525     c* se profilo di filiale chiudo il file di stampa a rottura
026400090825     c                   movel     adtpdr        compd
026500090525     c                   if        dutpou <> 046
026600090525     c                   seton                                        24
026700090721     c                   if        compd  <> d27fil
026800090525     c                   iter
026900090525     c                   end
027000090619     c                   movel     compd         compdr
027100090825     c                   else
027200091015      * profilo di sede ma richiesta singola filiale
027300091015     c                   if        d27fil > 0  and compd <> d27fil
027400091015     c                   iter
027500091015     c                   endif
027600090825     c* per sede a cambio di po salta riga
027700090825     c                   if        compd  <> savfil
027800090825     c                   eval      savfil = compd
027900090825     c                   seton                                        25
028000090825     c                   end
028100090525     c                   end
028200090525     c*
028300090525     c                   clear                   msga
028400090525     c                   clear                   msgc
028500090525     c                   clear                   msgs
028600090525     c*
028700090522     c                   eval      apdtip = 'D'
028800090522     c                   eval      apdpdr = adtpdr
028900090522     c     kapdf         chain     fiapd01l                           97
029000090522      *se l'anagrafica non corrisponde non stampo
029100090522     c   97              iter
029200090611     c                   movel     apdrsf        apdrsfs
029300090522     c                   clear                   aitrads
029400090522     c                   clear                   aitrsds
029500090526     c* aggancio il fornitore della tariffa
029600090526     c                   movel     adtcsf        comsoc
029700090526     c                   movel     adtcdf        comksc
029800090526     c                   exsr      srcdf
029900090617     c                   clear                   trovato_trs       1
030000091014     c     ktrs          setll     aitrs02l
030100091014     c                   do        *hival
030200091014     c     ktrs          reade     aitrs02l
030300091014     c                   if        %eof(aitrs02l)
030400091014     c                   leave
030500091014     c                   end
030600120321     c                   if        trsann <> ' ' or
030700120321     c                             trsdec > adtddt
030800091014     c                   clear                   aitrsds
030900091014     c                   iter
031000091014     c                   end
031100120321     c                   move      'S'           trovato_trs       1
031200090526     c     apdpdr        setll     aitra05l
031300090526     c                   do        *hival
031400090526     c     apdpdr        reade     aitra05l
031500090526     c                   if        %eof(aitra05l)
031600090526     c                   leave
031700090526     c                   end
031800090526     c                   if        tranrc <> trsnrc
031900090526     c                   iter
032000090526     c                   end
032100090526     c                   if        traann <> ' '
032200090526     c                   clear                   aitrads
032300090526     c                   iter
032400090526     c                   end
032500090617      *
032600090617      *  qui ho trovato il contratto
032700090526      *
032800090526     c                   if        tradfi = 0
032900090526     c                   leave
033000090526     c                   end
033100090526     c*
033200091014     c                   clear                   msga
033300090612     c                   if        adtddt > tradfi
033400091116     c                   if        (adtdts = 0 or adtdrc = 0 )
033500090526     c                   eval      msga = 'A'
033600091116     c                   end
033700090612     c                   else
033800090612     c* se è accreditato ma non stampato
033900090612     c                   if        adtdcn = 0
034000090612     c                   eval      msga = 'E'
034100090612     c                   end
034200090526     c                   end
034300090526     c                   enddo
034400091014     c                   if        trovato_trs = 'S'
034500091014     c                   leave
034600091014     c                   end
034700091014     c                   enddo
034800090526     c*
034900091014     c                   if        tradin = 0 and adtdts <> 0
035000091116     c                             and adtdrc = 0
035100090526     c                   eval      msga = 'A'
035200090612     c                   end
035300090525      * data di stampa
035400090522     c                   if        adtdcn <> 0
035500090525     c                   setoff                                       30
035600090522     c                   move      adtdcn        dataiso
035700090522     c                   move      dataiso       dataeur
035800090522     c                   move      dataeur       tgtdcns           8 0
035900090522     c                   else
036000090612     c                   seton                                        30
036100090612     c* se non accreditato non do l'errore di allegato non stmapato
036200091014     c                   if        msga = ' ' and adtdts <> 0
036300090617     c                             and trovato_trs <> 'S'
036400090525     c                   eval      msga = 'B'
036500090525     c                   end
036600090522     c                   clear                   tgtdcns
036700090522     c                   end
036800090525     c                   if        adtdts <> 0
036900090525     c                   setoff                                       31
037000090525     c                   move      adtdts        dataiso
037100090522     c                   move      dataiso       dataeur
037200090525     c                   move      dataeur       tgtdtss           8 0
037300090522     c                   else
037400090525     c                   seton                                        31
037500090525     c                   clear                   tgtdtss
037600090522     c                   end
037700090522     c                   if        adtddt <> 0
037800090525     c                   setoff                                       32
037900090522     c                   move      adtddt        dataiso
038000090522     c                   move      dataiso       dataeur
038100090522     c                   move      dataeur       tgtddts           8 0
038200090522     c                   else
038300090525     c                   seton                                        32
038400090522     c                   clear                   tgtddts
038500090522     c                   end
038600090522     c                   if        adtdrc <> 0
038700090525     c                   setoff                                       33
038800090522     c                   move      adtdrc        dataiso
038900090522     c                   move      dataiso       dataeur
039000090522     c                   move      dataeur       tgtdrcs           8 0
039100090522     c                   else
039200090525     c                   seton                                        33
039300090525     c                   if        adtdcn <> 0
039400090525     c                   eval      msga = 'C'
039500090525     c                   end
039600090522     c                   clear                   tgtdrcs
039700090522     c                   end
039800090522     c* fornitori
039900090525     c                   movel     adtcsf        comksc
040000090525     c                   movel     adtcdf        comksc
040100090522     c                   exsr      fornitori
040200090522     c* stampa
040300090522     c                   exsr      stampa
040400090522
040500090522     c                   enddo
040600090522
040700090522     c                   endsr
040800090522      * ?___________________________________________________________________
040900090522     C     stampa        BegSR
041000090522      * ?___________________________________________________________________
041100090525     c                   if        *in23
041200090525     c                   write     testa
041300090525     c                   setoff                                       23
041400090522     c                   end
041500090525     c                   if        apdtip = 'A'
041600090525     c                   eval      tipo = 'C'
041700090525     c                   else
041800090525     c                   eval      tipo = 'A'
041900090525     c                   end
042000090825     c                   eval      trsnrcs = trsnrc
042100090612     c*
042200090522     c                   if        tradin <> 0
042300090525     c                   setoff                                       34
042400090522     c                   move      tradin        dataiso
042500090522     c                   move      dataiso       dataeur
042600090522     c                   move      dataeur       tradins           8 0
042700090522     c                   else
042800090525     c                   seton                                        34
042900090522     c                   clear                   tradins
043000090522     c                   end
043100090525     c                   if        tradfi <> 0
043200090525     c                   setoff                                       38
043300090525     c                   move      tradfi        dataiso
043400090525     c                   move      dataiso       dataeur
043500090525     c                   move      dataeur       tradfis           8 0
043600090525     c                   else
043700090525     c                   seton                                        38
043800090525     c                   clear                   tradfis
043900090525     c                   end
044000090522     c                   if        trsdsc <> 0
044100090525     c                   setoff                                       35
044200090522     c                   move      trsdsc        dataiso
044300090522     c                   move      dataiso       dataeur
044400090522     c                   move      dataeur       trsdscs           8 0
044500090522     c                   else
044600090525     c                   seton                                        35
044700090522     c                   clear                   trsdscs
044800090522     c                   end
044900090522     c                   if        trsdrc <> 0
045000090525     c                   setoff                                       36
045100090522     c                   move      trsdrc        dataiso
045200090522     c                   move      dataiso       dataeur
045300090522     c                   move      dataeur       trsdrcs           8 0
045400090522     c                   else
045500090525     c                   if        trsdsc <> 0
045600090525     c                   eval      msgc = 'D'
045700090525     c                   end
045800090525     c                   seton                                        36
045900090522     c                   clear                   trsdrcs
046000090522     c                   end
046100090612     c* evidenzio le anomalie che sono a rischio
046200090612     c                   setoff                                       404142
046300091014     c                   if        tgtdtss <> 0
046400090612     c                   move      tgtdtss       dataeur
046500090612     c                   move      dataeur       dtaisodts
046600091014     c                   else
046700091014     c                   movel     *loval        dtaisodts
046800091014     c                   end
046900090612     c* allegato
047000090612     c     isooggi       subdur    dtaisodts     gg:*d
047100090612     c                   if        (msga = 'A' and
047200090612     c                             gg > §5AGGERRA) or
047300090612     c                             (msga = 'B' and
047400090612     c                             gg > §5AGGERRB) or
047500090612     c                             (msga = 'C' and
047600090612     c                             gg > §5AGGERRC)
047700090612     c                   seton                                        4041
047800090612     c                   end
047900090612     c* contratto
048000090612     c                   if        (msgc = 'D' and
048100090612     c                             gg > §5AGGERRD)
048200090612     c                   seton                                        4042
048300090612     c                   end
048400090617      *
048500090616     c                   exsr      somma_TOTali
048600090617      *
048700090612     c* stampo solo se ho evidenziato l'anomalia
048800090612     c                   if        (soloano = 'S' and *in40 ) or
048900090612     c                             soloano = ' '
049000090617     c                   if             msga <> *blank or
049100090617     c                                  msgc <> *blank or
049200090617     c                                  msgs <> *blank
049300090825     c   25              write     riga1
049400090825     c                   write     riga
049500090825     c                   setoff                                       25
049600090617      * emette 2° riga
049700090617     c                   if             msga = 'B'
049800090617     c                   eval      soctra = trasoc
049900090617     c                   if             SOCTRA <> *blank
050000090617     c                   write     riga_b
050100090617     c                   end
050200090617     c                   end
050300090617     c                   end
050400090617      *
050500090612     c                   eval      una = '1'
050600090612     c                   end
050700090522     c                   endsr
050800090522      * ?___________________________________________________________________
050900090522     C     fornitori     BegSR
051000090522      * ?___________________________________________________________________
051100090522     c                   if        trsiva <> ' '
051200090522     c                   exsr      ragsoccont
051300090525     c                   else
051400090525     c                   clear                   RAGSOCKSCs
051500090522     c                   end
051600090526     c                   if        trsiva <> comiva and trsiva <> ' '
051700090617     c                   if        msga = ' '
051800090617     c                   eval      msga = 'B'
051900090617     c                   end
052000090525     c                   seton                                        37
052100090525     c                   else
052200090525     c                   setoff                                       37
052300090522     c                   end
052400090522     c                   endsr
052500090526      * ?___________________________________________________________________
052600090522     c     srcdf         begsr
052700090526      * ?___________________________________________________________________
052800090525     c                   clear                   sogdess
052900090526     c                   clear                   comiva
053000090522     c     kfrn          chain     anfrn01l
053100090522     c                   if        %found(anfrn01l)
053200090522     c     frnsogg       chain     ansog01l
053300090525     c                   if        %found(ansog01l)
053400090525     c                   movel     sogdes        sogdess
053500090526     c                   movel     sogpartiva    comiva
053600090525     c                   else
053700090526     c                   clear                   comiva
053800090522     c                   end
053900090525     c                   end
054000090522     c                   endsr
054100090522      * ?___________________________________________________________________
054200090522     C     RagSocCont    BegSR
054300090522      * ?___________________________________________________________________
054400090522     C                   clear                   Trmz70s1ds                     Output
054500090522     C                   movel(p)  TRSiva        PartitaIVA                     Input
054600090522     C                   movel(p)  'F'           SottoNatur                     Input "F/C"
054700090522     C                   movel(p)  TRSFIL        Unita                          Input/Output
054800090608     C                   movel(p)  TRSsocg       societa                        Input/Output
054900090522      *
055000090522     c                   call      'TRMZ70SR1'
055100090522     C                   PARM                    trmz70s1ds
055200090522     c                   if        errore <> '0'
055300090525     c                   eval      msgs = '9'
055400090525     c                   clear                   RAGSOCKSCs
055500090525     c                   else
055600090525     c                   eval      RAGSOCKSCs = RAGSOCKSC
055700090522     c                   endif
055800090522     c*
055900090522     C                   ENDSR
056000090616     c*-------------------------------------------------
056100090616     c     azzera_TOTali begsr
056200090616     c*
056300090616     c                   clear                   msgATOT           5 0
056400090616     c                   clear                   rscATOT           5 0
056500090616     c                   clear                   msgBTOT           5 0
056600090616     c                   clear                   rscBTOT           5 0
056700090616     c                   clear                   msgCTOT           5 0
056800090616     c                   clear                   rscCTOT           5 0
056900090616     c                   clear                   msgDTOT           5 0
057000090616     c                   clear                   rscDTOT           5 0
057100090616     c                   clear                   msgETOT           5 0
057200090617     c                   clear                   rscETOT           5 0
057300090616     c                   clear                   msg9TOT           5 0
057400090617     c                   clear                   msgATOT_c         5 0
057500090617     c                   clear                   rscATOT_c         5 0
057600090617     c                   clear                   msgBTOT_c         5 0
057700090617     c                   clear                   rscBTOT_c         5 0
057800090617     c                   clear                   msgCTOT_c         5 0
057900090617     c                   clear                   rscCTOT_c         5 0
058000090617     c                   clear                   msgDTOT_c         5 0
058100090617     c                   clear                   rscDTOT_c         5 0
058200090617     c                   clear                   msgETOT_c         5 0
058300090617     c                   clear                   rscETOT_c         5 0
058400090617     c                   clear                   msg9TOT_c         5 0
058500090617     c                   clear                   msgATOT_a         5 0
058600090617     c                   clear                   rscATOT_a         5 0
058700090617     c                   clear                   msgBTOT_a         5 0
058800090617     c                   clear                   rscBTOT_a         5 0
058900090617     c                   clear                   msgCTOT_a         5 0
059000090617     c                   clear                   rscCTOT_a         5 0
059100090617     c                   clear                   msgDTOT_a         5 0
059200090617     c                   clear                   rscDTOT_a         5 0
059300090617     c                   clear                   msgETOT_a         5 0
059400090617     c                   clear                   rscETOT_a         5 0
059500090617     c                   clear                   msg9TOT_a         5 0
059600090616     c                   clear                   Senza_contrA      5 0
059700090616     c                   clear                   Senza_contrB      5 0
059800090616     c                   clear                   Senza_contrC      5 0
059900090616     c                   clear                   Senza_contrD      5 0
060000090616     c                   clear                   Senza_contrE      5 0
060100090616     c                   clear                   Senza_contr9      5 0
060200090617     c                   clear                   Senza_contrA_c    5 0
060300090617     c                   clear                   Senza_contrB_c    5 0
060400090617     c                   clear                   Senza_contrC_c    5 0
060500090617     c                   clear                   Senza_contrD_c    5 0
060600090617     c                   clear                   Senza_contrE_c    5 0
060700090617     c                   clear                   Senza_contr9_c    5 0
060800090617     c                   clear                   Senza_contrA_a    5 0
060900090617     c                   clear                   Senza_contrB_a    5 0
061000090617     c                   clear                   Senza_contrC_a    5 0
061100090617     c                   clear                   Senza_contrD_a    5 0
061200090617     c                   clear                   Senza_contrE_a    5 0
061300090617     c                   clear                   Senza_contr9_a    5 0
061400090616     c*
061500090616     C                   ENDSR
061600090616     c*-------------------------------------------------
061700090616     c     somma_TOTali  begsr
061800090616     c*
061900090616     c                   if        msgA ='A'
062000090616     c                   add       1             msgATOT
062100090616     c   41              add       1             rscATOT
062200090616     c                   if        TRSDSCS = 0
062300090616     c                   add       1             Senza_contrA
062400090616     c                   end
062500090617      *
062600090617     c                   if        tipo = 'C'
062700090617     c                   add       1             msgATOT_c
062800090617     c   41              add       1             rscATOT_c
062900090617     c                   if        TRSDSCS = 0
063000090617     c                   add       1             Senza_contrA_c
063100090617     c                   end
063200090617     c                   elseIf    tipo = 'A'
063300090617     c                   add       1             msgATOT_a
063400090617     c   41              add       1             rscATOT_a
063500090617     c                   if        TRSDSCS = 0
063600090617     c                   add       1             Senza_contrA_a
063700090617     c                   end
063800090617     c                   end
063900090617      *
064000090616     c                   elseIf    msgA ='B'
064100090616     c                   add       1             msgBTOT
064200090616     c   41              add       1             rscBTOT
064300090616     c                   if        TRSDSCS = 0
064400090616     c                   add       1             Senza_contrB
064500090617     c                   end
064600090617      *
064700090617     c                   if        tipo = 'C'
064800090617     c                   add       1             msgBTOT_c
064900090617     c   41              add       1             rscBTOT_c
065000090617     c                   if        TRSDSCS = 0
065100090617     c                   add       1             Senza_contrB_c
065200090617     c                   end
065300090617     c                   elseIf    tipo = 'A'
065400090617     c                   add       1             msgBTOT_a
065500090617     c   41              add       1             rscBTOT_a
065600090617     c                   if        TRSDSCS = 0
065700090617     c                   add       1             Senza_contrB_a
065800090617     c                   end
065900090617     c                   end
066000090617      *
066100090617     c                   elseIf    msgA ='C'
066200090617     c                   add       1             msgCTOT
066300090617     c   41              add       1             rscCTOT
066400090617     c                   if        TRSDSCS = 0
066500090617     c                   add       1             Senza_contrC
066600090616     c                   end
066700090617      *
066800090617     c                   if        tipo = 'C'
066900090617     c                   add       1             msgCTOT_c
067000090617     c   41              add       1             rscCTOT_c
067100090617     c                   if        TRSDSCS = 0
067200090617     c                   add       1             Senza_contrC_c
067300090617     c                   end
067400090617     c                   elseIf    tipo = 'A'
067500090617     c                   add       1             msgCTOT_a
067600090617     c   41              add       1             rsCCTOT_a
067700090617     c                   if        TRSDSCS = 0
067800090617     c                   add       1             Senza_contrC_a
067900090617     c                   end
068000090617     c                   end
068100090617      *
068200090617     c                   elseIf    msgA ='E'
068300090617     c                   add       1             msgETOT
068400090617     c   41              add       1             rscETOT
068500090617     c                   if        TRSDSCS = 0
068600090617     c                   add       1             Senza_contrE
068700090617     c                   end
068800090617      *
068900090617     c                   if        tipo = 'C'
069000090617     c                   add       1             msgETOT_c
069100090617     c   41              add       1             rscETOT_c
069200090617     c                   if        TRSDSCS = 0
069300090617     c                   add       1             Senza_contrE_c
069400090617     c                   end
069500090617     c                   elseIf    tipo = 'A'
069600090617     c                   add       1             msgETOT_a
069700090617     c   41              add       1             rsCETOT_a
069800090617     c                   if        TRSDSCS = 0
069900090617     c                   add       1             Senza_contrE_a
070000090617     c                   end
070100090617     c                   end
070200090617      *
070300090616     c                   end
070400090616     c*
070500090619     c*  Le D vanno raggruppate per contratto quindi conta x una volta sola
070600090617     c                   if        msgC ='D'
070700090619      **
070800090619     c     trsnrc        lookup    nrc                                    55
070900090619     c                   if        not *in55
071000090619     c                   z-add     1             y                 3 0
071100090619     c     *zero         lookup    nrc(y)                                 55
071200090619     c                   eval      nrc(y) = trsnrc
071300090619      **
071400090617     c                   add       1             msgDTOT
071500090617     c   42              add       1             rscDTOT
071600090616     c                   if        TRSDSCS = 0
071700090617     c                   add       1             Senza_contrD
071800090616     c                   end
071900090617      *
072000090617     c                   if        tipo = 'C'
072100090617     c                   add       1             msgDTOT_c
072200090617     c   42              add       1             rscDTOT_c
072300090617     c                   if        TRSDSCS = 0
072400090617     c                   add       1             Senza_contrD_c
072500090617     c                   end
072600090617     c                   elseIf    tipo = 'A'
072700090617     c                   add       1             msgDTOT_a
072800090617     c   42              add       1             rsCDTOT_a
072900090617     c                   if        TRSDSCS = 0
073000090617     c                   add       1             Senza_contrD_a
073100090617     c                   end
073200090617     c                   end
073300090619      ***
073400090619     c                   end
073500090617      *
073600090616     c                   end
073700090616      *
073800090616     c                   if        msgS ='9'
073900090616     c                   add       1             msg9TOT
074000090616     c                   if        TRSDSCS = 0
074100090616     c                   add       1             Senza_contr9
074200090616     c                   end
074300090617      *
074400090617     c                   if        tipo = 'C'
074500090617     c                   add       1             msg9TOT_c
074600090617     c                   if        TRSDSCS = 0
074700090617     c                   add       1             Senza_contr9_c
074800090617     c                   end
074900090617     c                   elseIf    tipo = 'A'
075000090617     c                   add       1             msg9TOT_a
075100090617     c                   if        TRSDSCS = 0
075200090617     c                   add       1             Senza_contr9_a
075300090617     c                   end
075400090617     c                   end
075500090617      *
075600090616     c                   end
075700090616      *
075800090616     C                   ENDSR
075900011026     c*-------------------------------------------------
076000090616     c     Totali        begsr
076100011026     c*-------------------------------------------------
076200090616      *
076300090616     c                   movel     'A'           tipoERR
076400090616     c                   z-add     msgATOT       numERR
076500090616     c                   z-add     rscATOT       rscERR
076600090617     c                   z-add     Senza_contrA  NoContra
076700090617     c                   z-add     msgATOT_c     numERRc
076800090617     c                   z-add     rscATOT_c     rscERRc
076900090617     c                   z-add     Senza_contrA_cNoContraC
077000090617     c                   z-add     msgATOT_a     numERRa
077100090617     c                   z-add     rscATOT_a     rscERRa
077200090617     c                   z-add     Senza_contrA_aNoContraA
077300090616     c                   if        *in23
077400090616     c                   write     testa
077500090616     c                   write     testaTOT
077600090616     c                   setoff                                       23
077700090616     c                   else
077800090616     c                   write     testaTOT
077900090616     c                   end
078000090616     c                   write     rigaTOT
078100090616      *
078200090616     c                   movel     'B'           tipoERR
078300090616     c                   z-add     msgBTOT       numERR
078400090616     c                   z-add     rscBTOT       rscERR
078500090616     c                   z-add     Senza_contrB  NoContra
078600090617     c                   z-add     msgBTOT_c     numERRc
078700090617     c                   z-add     rscBTOT_c     rscERRc
078800090617     c                   z-add     Senza_contrB_cNoContraC
078900090617     c                   z-add     msgBTOT_a     numERRa
079000090617     c                   z-add     rscBTOT_a     rscERRa
079100090617     c                   z-add     Senza_contrB_aNoContraA
079200090616     c                   if        *in23
079300090616     c                   write     testa
079400090616     c                   write     testaTOT
079500090616     c                   setoff                                       23
079600090616     c                   end
079700090616     c                   write     rigaTOT
079800090616      *
079900090616     c                   movel     'C'           tipoERR
080000090616     c                   z-add     msgCTOT       numERR
080100090616     c                   z-add     rscCTOT       rscERR
080200090616     c                   z-add     Senza_contrC  NoContra
080300090617     c                   z-add     msgCTOT_c     numERRc
080400090617     c                   z-add     rscCTOT_c     rscERRc
080500090617     c                   z-add     Senza_contrC_cNoContraC
080600090617     c                   z-add     msgCTOT_a     numERRa
080700090617     c                   z-add     rscCTOT_a     rscERRa
080800090617     c                   z-add     Senza_contrC_aNoContraA
080900090616     c                   if        *in23
081000090616     c                   write     testa
081100090616     c                   write     testaTOT
081200090616     c                   setoff                                       23
081300090616     c                   end
081400090616     c                   write     rigaTOT
081500090616      *
081600090616     c                   movel     'D'           tipoERR
081700090616     c                   z-add     msgDTOT       numERR
081800090616     c                   z-add     rscDTOT       rscERR
081900090616     c                   z-add     Senza_contrD  NoContra
082000090617     c                   z-add     msgDTOT_c     numERRc
082100090617     c                   z-add     rscDTOT_c     rscERRc
082200090617     c                   z-add     Senza_contrD_cNoContraC
082300090617     c                   z-add     msgDTOT_a     numERRa
082400090617     c                   z-add     rscDTOT_a     rscERRa
082500090617     c                   z-add     Senza_contrD_aNoContraA
082600090616     c                   if        *in23
082700090616     c                   write     testa
082800090616     c                   write     testaTOT
082900090616     c                   setoff                                       23
083000090616     c                   end
083100090616     c                   write     rigaTOT
083200090616      *
083300090616     c                   movel     'E'           tipoERR
083400090616     c                   z-add     msgETOT       numERR
083500090617     c                   z-add     rscETOT       rscERR
083600090616     c                   z-add     Senza_contrE  NoContra
083700090617     c                   z-add     msgETOT_c     numERRc
083800090617     c                   z-add     rscETOT_c     rscERRc
083900090617     c                   z-add     Senza_contrE_cNoContraC
084000090617     c                   z-add     msgETOT_a     numERRa
084100090617     c                   z-add     rscETOT_a     rscERRa
084200090617     c                   z-add     Senza_contrE_aNoContraA
084300090616     c                   if        *in23
084400090616     c                   write     testa
084500090616     c                   write     testaTOT
084600090616     c                   setoff                                       23
084700090616     c                   end
084800090616     c                   write     rigaTOT
084900090616      *
085000090616     c                   movel     '9'           tipoERR
085100090616     c                   z-add     msg9TOT       numERR
085200090616     c                   z-add     0             rscERR
085300090616     c                   z-add     Senza_contr9  NoContra
085400090617     c                   z-add     msg9TOT_c     numERRc
085500090617     c                   z-add     0             rscERRc
085600090617     c                   z-add     Senza_contr9_cNoContraC
085700090617     c                   z-add     msg9TOT_a     numERRa
085800090617     c                   z-add     0             rscERRa
085900090617     c                   z-add     Senza_contr9_aNoContraA
086000090616     c                   if        *in23
086100090616     c                   write     testa
086200090616     c                   write     testaTOT
086300090616     c                   setoff                                       23
086400090616     c                   end
086500090616     c                   write     rigaTOT
086600090616      *
086700090616      *
086800090616     C                   ENDSR
086900090616     c*-------------------------------------------------
087000090616     c     *inzsr        begsr
087100090616     c*-------------------------------------------------
087200020304     c     *entry        plist
087300020304     c                   parm                    kpjba
087400090721     c                   movel     kpjbu         ficn27ds
087500090721     c                   movel     d27solor      soloano           1
087600090525     c                   seton                                        23
087700090525     c                   eval      nompgm = 'FICN28R'
087800090525     c     *dtaara       define    §azute        azuteds
087900090525     c     *dtaara       define    §datiute      ddatiute
088000090525     C                   in(E)     *dtaara
088100090612     C
088200090525     C                   call      'TIBS34R'
088300090525     C                   parm                    Tibs34Ds
088400090525     C                   in        *dtaara
088500090525     c                   movel     *all'_'       cmp198          198
088600090525     c                   movel     *all'_'       cmp198a         198
088700090617     c                   movel     *all'_'       cmp139          139
088800090617     c                   movel     *all'_'       cmp139a         139
088900090617      *
089000090617      * stampa la legenda in testa all'inizio della stampa
089100090617     c                   seton                                        93
089200120813     c                   clear                   level             1
089300120813     c                   if        d27fil <>  0
089400120813     c     d27fil        chain     azorg01l
089500120813     c                   if        %found(azorg01l)
089600120813     c                   movel     orgde8        level
089700120813     c                   endif
089800120813     c                   endif
089900090617      *
090000020218     C                   time                    w0120            14 0
090100090525     C                   move      w0120         woggi
090200090612     C                   MOVE      W0120         UDATE8            8 0
090300090612     C                   Z-ADD     UDATE8        G02DAT
090400090612     C                   MOVE      *ZEROS        G02INV
090500090612     C                   MOVE      *BLANKS       G02ERR
090600090612     C                   CALL      'XSRDA8'
090700090612     C                   PARM                    WLBDAT
090800090612     C                   MOVE      g02inv        isooggi
090900090525     C                   movel     w0120         times
091000090612     C     KTAB          KLIST
091100090612     C                   KFLD                    tblkut
091200090612     C                   KFLD                    tblcod
091300090612     C                   KFLD                    tblkey
091400090612     C                   Z-ADD     1             tblkut
091500021203     c     kapdf         klist
091600090424     c                   kfld                    apdtip
091700090525     c                   kfld                    apdpdr
091800090424     c                   movel     'A'           apdtip
091900090525     C     Kfrn          KLIST
092000090525     C                   KFLD                    comsoc
092100090525     C                   KFLD                    comksc
092200090526     C     Ktrs          KLIST
092300090526     C                   KFLD                    comsoc
092400090526     C                   KFLD                    comiva
092500090612     C***  giorni da calcolare x date limite
092600090612     C                   clear                   ds5Aaut
092700090612     C                   movel     '5A'          tblcod
092800090612     C                   MOVEL(P)  'AUT'         tblKEY
092900090612     C     KTAB          CHAIN     TABEL00F                           30
093000090612     c                   if        %found(tabel00f) and tblflg = ' '
093100090612     c                   movel     tbluni        ds5aaut
093200090619      **
093300090619      **  Attenzione se ci si trova nel periodo delle Trazioni Ridotte deve
093400090619      **  incrementare di x giorni i giorni di rischio per i 4 tipi di errori
093500090619     c                   if             §5ADTAINI <= Udate8 and
093600090619     c                                  §5ADTAFIN >= Udate8
093700090619     c                   eval      §5AGGERRa = §5AGGERRa + §5AGGINPIU
093800090619     c                   eval      §5AGGERRb = §5AGGERRb + §5AGGINPIU
093900090619     c                   eval      §5AGGERRc = §5AGGERRc + §5AGGINPIU
094000090619     c                   eval      §5AGGERRd = §5AGGERRd + §5AGGINPIU
094100090619     c                   end
094200090619      **
094300090612     c                   end
094400090617      *
094500090617     c                   write     leggenda
094600090617     c                   setoff                                       23
094700090617      *
094800070307     c                   endsr
