000100040708      /TITLE Upload via Internet: traduzione in FNVAOWWR.
000200130114     H dftactgrp(*no) actgrp(*caller)
000300000313     F*
000400040723     Ffnacr01l  if   e           k disk
000500090617     Ftntbe01l  if   e           k disk
000600990910     Ftivin00r  uF   E             DISK    usropn
000700040708     FFnvaowwr  O    E             DISK    usropn
000800040723     Ftiori00f  O    E             DISK
000900050826     FTIVGD00F  O    E             DISK
001000000313     D*
001100010330     D*----------------------------------------------------
001200010330     D* DICHIARAZIOINE VARIABILI DI WRK
001300010330     D*----------------------------------------------------
001400010330     D dscmz         e ds                  inz
001500010330     D psds           sds
001600010330     D  procname         *PROC
001700010330     D tivlrds       e ds                  extname(tivlr00f)
001800050826     D fnvapds       e ds                  extname(fnvap00f)
001900040723     D dorm01        e ds
002000040723     D dfar          e ds
002100090617     d trul33ds      e ds
002200090617     d kpjba         e ds
002300010330     D esito           s              1
002400010330     D prmlit          s             10
002500010330     D prmfir          s             10
002600010330     D wrkesito        s                   like(esito)
002700010330     D rrnum           s              6  0 INZ(*zeros)
002800060306     D Cli_Abil        s              1    inz(*off)
002900060306     D woriIDC         s                   like(oriIDC) inz(*blanks)
003000090617     d §numori         s              7  0 inz
003100030715     D*------------------
003200030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
003300030715     D*------------------
003400030715     D SkSplitFLD      S             10    DIM(100)
003500030715     D SkSplitCSV      S            256    DIM(100)
003600030715     D CharCSV         S              1
003700030715     D CharTXT         S              1
003800030715     D CharNUM         S              1
003900090403     D CharSOS         S              1
004000030715     D posDa           S              3  0 INZ(*zeros)
004100030715     D posA            S              3  0 INZ(*zeros)
004200030715     D i               s              3  0 INZ(1)
004300030715     D wGiro           s              1  0 INZ(*zeros)
004400030822     D*------------------
004500030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
004600030822     D*------------------
004700030822     D posDaDft        S              3  0 INZ(*zeros)
004800030822     D posADft         S              3  0 INZ(*zeros)
004900030822     D j               s              3  0 INZ(1)
005000030822     D wGiroDft        s              1  0 INZ(*zeros)
005100031201     D*------------------
005200031201     D* Costanti
005300031201     D*------------------
005400031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
005500031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
005600010330
005700091026
005800091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
005900091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
006000091026
006100010330
006200010330
006300000913     C                   reset                   rrnum
006400990921     C                   reset                   esito
006500990921     C                   reset                   wrkesito
006600010601     C*
006700010601     C                   exsr      opeini
006800040708     C                   exsr      rwvao
006900010601     C*
007000010601     C                   seton                                        lr
007100010601
007200010601
007300010601
007400010601
007500010601     C*--------------------------------------------------------
007600010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007700010601     C*--------------------------------------------------------
007800010601     C     PREELA        BEGSR
007900010601     C*
008000010601     C* SE OCCORRE SPEDIRE IN FILIALE
008100010601     C                   if        invfil <> *zeros and
008200010601     C                             flgGiro = '0'
008300010601     C*
008400010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
008500010601     C                   eval      flgGiro = '1'
008600010601     C*
008700010601     C                   endif
008800010601     C*
008900010601     C                   ENDSR
009000010601     C***
009100010601
009200010601
009300010601
009400010601
009500010601
009600010601
009700010601
009800010601     C*--------------------------------------------------------
009900010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010000010601     C*--------------------------------------------------------
010100010601     C     ENDELA        BEGSR
010200990915     C*
010300000616     C*
010400010601     C                   ENDSR
010500010601     C***
010600000613
010700010330
010800010601
010900010601
011000010601
011100010330     C*--------------------------------------------------------
011200040708     C* RWVAO   LEGGE TIVIN00R E SCRIVE FNVAOWWF              *
011300010330     C*--------------------------------------------------------
011400040708     C     RWVAO         BEGSR
011500010330     C*
011600010330     C                   if        not %open(tivin00r)
011700010330     C                   open      tivin00r
011800010330     C                   endif
011900040708     C                   if        not %open(fnvaowwr)
012000040708     C                   open      fnvaowwr
012100010330     C                   endif
012200010330     C*
012300010604     C                   clear                   §CTROK
012400010604     C                   clear                   §CTRMO
012500010604     C                   clear                   §CTRNO
012600010330     C*
012700030822     C                   do        *HIVAL
012800010330     C*
012900030822     C                   read      tivin00r                               70
013000010618     C*
013100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
013200010618     C                   if        vinflg = '1'
013300010618     C                   eval      flgOk = '1'
013400010618     C                   endif
013500010618     C*
013600040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
013700040510     C                   z-add     *zeros        wLenVINDTA        4 0
013800040510     C                   z-add     *zeros        wFlgVINDTA        1 0
013900040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
014000040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
014100040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
014200040510     C                   z-add     1             wFlgVINDTA
014300040510     C                   leave
014400040510     C                   endif
014500040510     C                   enddo
014600040510     C*
014700040218     C                   if        vindta > *blanks AND
014800040510     C                             wFlgVINDTA = 1
014900010330     C                   add       1             rrnum
015000010330     C*
015100010601     C                   if        *in70 = *off and
015200010330     C                             (vinflg = *blanks
015300010330     C                              or vinflg = '0'
015400010330     C                              or vinflg = '2')
015500010330     C*
015600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
015700010711     C                   if        vinflg = *blanks or vinflg = '0'
015800010711     C                   clear                   vinmsg
015900010711     C                   endif
016000010601     C*
016100010330     C                   exsr      inzvar
016200090403     C*
016300090403     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
016400090403     C* separatori campi
016500090403     C                   if        CharTXT <> *blanks
016600090403     C                   z-add     *zeros        wLenVINDTA        4 0
016700090403     C                   z-add     *zeros        wFlgVINDTA        1 0
016800090403     C                   setoff                                       80
016900090403     C                   dow       wLenVINDTA < %len(%trim(vindta))
017000090403     C                   eval      wLenVINDTA = wLenVINDTA + 1
017100090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
017200090403     C                   if        not *in80
017300090403     C                   seton                                        80
017400090403     C                   else
017500090403     C                   setoff                                       80
017600090403     C                   endif
017700090403     C                   endif
017800090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
017900090403     C                             and *in80 = *on
018000090403     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
018100090403     C                   endif
018200090403     C                   enddo
018300090403     C                   endif
018400090403     C*
018500040708     C                   exsr      impvao
018600010601     C*
018700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
018800101210     C                   if        not *IN31
018900040708     C                   eval      depfil = VAOPOE
019000130125     C***                exsr      repfil
019100130125     C***                if        depfil = invfil
019200021025     C                   if        vlrpoi = 999
019300130125     C***                move(P)   invfil        VAOPOE
019400021025     C                   else
019500040708     C                   move(P)   vlrpoi        VAOPOE
019600021025     C                   endif
019700130125     C***                endif
019800101210     C                   endif
019900010601     C*
020000010601     C                   exsr      PREELA
020100010601     C*
020200010604     C* Ebbene...
020300010604     C*
020400030822     C  N31              add       1             §CTROK            7 0
020500030822     C   32              add       1             §CTRMO            7 0
020600030822     C   31              add       1             §CTRNO            7 0
020700030822     C                   if        wGiro = 2
020800040723     C  N31              EXSR      CHKCOR
020900040723     C  N31              EXSR      WRIORI
021000050826     C  N31              EXSR      WRIVGD
021100040708     C  N31              write     FNVAO000
021200030822     C                   endif
021300010604     C*
021400010604     C                   if        *in31 = *off and
021500010604     C                             *in32 = *off
021600010604     C                   eval      vinflg = '1'
021700010604     C                   else
021800010604     C                   eval      vinflg = '2'
021900010604     C                   endif
022000010601     C*
022100010604     C                   endif
022200010604     C*
022300010330     C                   else
022400010330     C                   eval      vinflg = '1'
022500010330     C                   endif
022600010601     C*
022700010601     C  N70              update    tivin000
022800010330     C*
022900030822     C  N70              enddo
023000010601     C
023100010601     C*
023200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
023300010601     C                   if        cntNonEl = *zeros or
023400010601     C                             flgMulti = '0'
023500010330     C* Se non ci sono record con errori ...
023600010601     C                   if        §ctrno = 0 and
023700010604     C                             §ctrmo = 0 and
023800010601     C                             flgStato <> '2'
023900010330     C* ... restituisco esito OK.
024000010330     C                   eval      wrkesito = '0'
024100010330     C                   else
024200010330     C                   if        §ctrok > 0
024300010330     C                   eval      wrkesito = '1'
024400010330     C                   else
024500010615     C                   if        flgOk = '0'
024600010615     C                   eval      wrkesito = '2'
024700010615     C                   else
024800010615     C                   eval      wrkesito = '6'
024900010615     C                   endif
025000010330     C                   endif
025100010330     C                   endif
025200010601     C                   else
025300010601     C                   eval      wrkesito = '9'
025400010601     C                   endif
025500010330     C*
025600010330     C                   if        %open(tivin00r)
025700010330     C                   close     tivin00r
025800010330     C                   endif
025900040708     C                   if        %open(fnvaowwr)
026000040708     C                   close     fnvaowwr
026100010330     C                   endif
026200010601     C*
026300010601     C                   if        vlrpoi <> 999
026400010601     C                   eval      invfil = vlrpoi
026500010601     C                   endif
026600101210     C*
026700101210     C                   if        invfil = 0 and
026800101210     C                             depfil > 0
026900101210     C                   eval      invfil = depfil
027000101210     C                   endif
027100010330     C*
027200010330     C                   if        §ctrok > 0
027300010601     C                             and invfil > *zeros
027400010330     C                   exsr      invio
027500010330     C                   endif
027600010601     C*
027700010618     C                   if        flgGiro = '1'
027800010601     C                   exsr      endela
027900010618     C                   endif
028000010330     C*
028100010330     C                   ENDSR
028200010330     C***
028300050323
028400050323
028500050323     C*------------------------------------------------------------------------*
028600050323     C* CHKCOR - Considerazioni su VAOCOR
028700050323     C*------------------------------------------------------------------------*
028800050323     C     CHKCOR        BEGSR
028900050323     C*
029000050323     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
029100050323     C                   if        vaoTCO = 'F'
029200050323     C* Quindi verifico se il codice ordinante non è valorizzato
029300050323     C                   if        vaoCOR = *zeros
029400050323     C*                  if        vaoCOR = *zeros  AND
029500050323     C*                            vaoRSO = *blanks AND
029600050323     C*                            vaoINO = *blanks AND
029700050323     C*                            vaoCAO = *blanks AND
029800050323     C*                            vaoLOO = *blanks AND
029900050323     C*                            vaoPRO = *blanks AND
030000050323     C*                            vaoNAO = *blanks
030100050323     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
030200050323     C                   move      vlrKSC        wCli              7 0
030300050323     C                   movel(p)  wCli          acrCRO
030400050323     C     acrCRO        setll     fnacr01l
030500050323     C                   if        %found(fnacr01l)
030600050323     C                   read      fnacr01l
030700050323     C                   dow       not %eof(fnacr01l)
030800050323     C                   movel     acrCRO        wCliAcr           7 0
030900050323     C                   if        wCliAcr = wCli
031000050323     C                   eval      vaoCOR = acrCRO
031100050323     C                   leave
031200050323     C                   else
031300050323     C                   leave
031400050323     C                   endif
031500050323     C                   read      fnacr01l
031600050323     C                   enddo
031700050323     C                   endif
031800050323     C                   endif
031900050323     C                   endif
032000050323     C*
032100050323     C                   ENDSR
032200040723
032300040723
032400040723
032500040723     C*------------------------------------------------------------------------*
032600040723     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
032700040723     C*------------------------------------------------------------------------*
032800040723     C     WRIORI        BEGSR
032900040723     C*
033000090617     C* Come prima cosa stacco un numeratore da AZNUM
033100090617     C                   clear                   trul33ds
033200090617     C                   eval      I33OPE = *zeros
033300090617     C                   eval      I33CNU = 600
033400090617     C                   eval      I33NUM = 1
033500090617     C                   movel     TRUL33DS      KPJBU
033600090617     C                   call      'TRUL33R'
033700090617     C                   parm                    KPJBA
033800090617     C                   movel     KPJBU         TRUL33DS
033900090617     C                   if        O33ERR = *zeros
034000090617     C                   z-add     O33NRF        §numori
034100090618     c                   else
034200090618     c                   eval      §numori = *all'9'
034300090617     C                   endif
034400040723     C*
034500040723     C* Quindi imposto i campi "extra-VAO"
034600040723     C                   if        vlrKSC = '0ORM0000'
034700040723     C*
034800040723     C                   if        vaoCOR > *zeros
034900040723     C                   movel     vaoCOR        wrkCOR7           7 0
035000040723     C*
035100040723     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
035200040723     C* codice padre. Se è un padre assegno già il codice ordinante.
035300040723     C                   clear                   tibs10ds
035400040723     C                   eval      d10drf = datcor                              *data riferimento
035500040723     C                   eval      d10paf = 'P'                                 *cerca il padre
035600040723     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
035700091026     C*
035800091026     C* Preparazione chiamata al TIBS10R1
035900091026     C                   clear                   tibs10r1ds
036000091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
036100091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
036200091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
036300091026     C*
036400091026     C                   call      'TIBS10R1'
036500091026     C                   parm                    tibs10r1ds
036600091026     C*
036700091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
036800040723     C*
036900040723     C                   if        d10err <> *blanks or                         *No unif. o padre
037000040723     C                             d10cop =  *zeros  or
037100040723     C                             d10cop =  d10cod
037200040723     C                   move      d10cod        oriIDC
037300040723     C                   else                                                   *Figlio
037400040723     C                   move      d10cop        oriIDC
037500040723     C                   endif
037600040723     C*
037700040723     C                   else
037800040723     C                   eval      oriIDC = *zeros
037900040723     C                   endif
038000040723     C*
038100040723     C                   else
038200040723     C                   eval      oriIDC = vlrKSC
038300040723     C                   endif
038400040723     C*
038500040723     C                   eval      oriPRG = §numori
038600040723     C                   eval      oriDER = datcor
038700040723     C                   eval      oriOER = wn6
038800040723     C                   eval      oriPOE = vaoPOE
038900040723     C                   eval      oriNSR = vaoNSR
039000040723     C                   eval      oriNOR = vaoNOR
039100040723     C                   eval      oriNRV = vaoNRV
039200040723     C                   eval      oriFLO = *blanks
039300040723     C*
039400040723     C* Infine valorizzo la chiave esterna sul file FNVAO
039500040723     C                   eval      dorm01 = vaoFLO
039600040723     C                   movel     §numori       §ormpg
039700040723     C                   eval      vaoFLO = dorm01
039800040723     C*
039900040723     C* ...quindi scrivo il file TIORI00F
040000131008     C  N55              WRITE     TIORI000
040100040723     C*
040200040723     C                   ENDSR
040300050826
040400050826
040500050826
040600050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
040700050826     C     wriVGD        BEGSR
040800050826     C*
040900050826     C* Reperisco la descrizione della fase da tabella
041000050826     C                   movel(p)  'FAR'         KteCOD
041100050826     C                   movel(p)  '000'         KteKE1
041200050826     C     KEYtbe        chain     tntbe01l
041300050826     C                   if        %found(tntbe01l)
041400050826     C                   movel(P)  tbeuni        DFAR
041500050826     C                   else
041600050826     C                   clear                   DFAR
041700050826     C                   endif
041800050826     C*
041900050826     C* Valorizzo buffer record
042000050826     C                   clear                   FNVAPDS
042100050826     C                   eval      vapIDC = oriIDC
042200050826     C                   eval      vapPOE = oriPOE
042300050826     C                   eval      vapRFA = vaoRFA
042400050826     C                   eval      vapPOG = oriPOE
042500050826     C                   eval      vapDAE = oriDER
042600050826     C                   eval      vapORE = oriOER
042700050826     C                   if        d§fardva <> *blanks
042800050826     C                   eval      vapDFA = d§fardva
042900050826     C                   else
043000050826     C                   eval      vapDFA = d§fardes
043100050826     C                   endif
043200050826     C*
043300050826     C                   clear                   tivgd000
043400080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
043500050826     C                   eval      vgdTIP = 'VP'
043600050826     C                   eval      vgdKSU = vapIDC
043700050826     C                   eval      vgdTSC = 'WW'
043800050826     C                   eval      vgdDAT = datcor
043900050826     C                   eval      vgdPGM = 'TITVOXLR'
044000060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
044100060306     C                   exsr      sr_Control
044200060306     C                   if        Cli_Abil = *on
044300060306     C                   eval      vgdKSU = woriIDC
044400131008     C  N55              write     tivgd000
044500060306     C                   endif
044600050826     C*
044700050826     C                   ENDSR
044800050826     C*------------------------------------------------------------------------*
044900010601
045000010601
045100010601
045200010330     C*----------------------------------------------------*
045300030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
045400010330     C*----------------------------------------------------*
045500010330     C     INZVAR        BEGSR
045600010330     C*
045700030822     C* Inizializzo variabili di wrk
045800020204     C                   Z-ADD     *zeros        Num5_0            5 0
045900090403     C                   z-add     *zeros        invfil
046000030822     C*
046100030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
046200040708     C                   CLEAR                   FNVAO000
046300040723     C                   CLEAR                   TIORI000
046400030822     C                   CLEAR                   SkSplitCSV
046500030822     C                   EVAL      i = 1
046600030822     C                   EVAL      posDa = *zeros
046700030822     C                   EVAL      posA  = *zeros
046800030822     C*
046900030822     C* Reimposto i valori di default
047000030822     C                   EXSR      DEFCAM
047100030822     C*
047200010330     C                   ENDSR
047300010330     C*----------------------------------------------------*
047400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
047500010330     C*----------------------------------------------------*
047600010330     C     DEFCAM        BEGSR
047700030822     C*
047800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
047900030715     C* e delimitatore testo.
048000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
048100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
048200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
048300090403     C*
048400090403     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
048500090403     C                   EVAL      CharSOS = CharNUM
048600030822     C*
048700030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
048800030715     C                   IF        %subst(vlrppt:1:1) = 'S'
048900030715     C                   SETON                                        50
049000030715     C                   ELSE
049100030715     C                   SETOFF                                       50
049200131008     C                   ENDIF
049300131008     C* aggiungo la gestyione dei test (tanto la stamapa non c'è più)
049400131008     C                   SETOFF                                       55
049500131008     C                   IF        %subst(vlrppt:1:1) = 'T'
049600131008     C* inibisco la scrittura dei file TIORI e TIVGD
049700131008     C                   SETON                                        55
049800131008     C* imposto il PO di emissione come filiale fittizia
049900131008     C                   EVAL      VAOPOE = 999
050000131008     C                   ENDIF
050100030822     C*
050200030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
050300030822     C                   EVAL      posDaDft = 1
050400030822     C                   EVAL      posADft  = 0
050500030822     C                   EVAL      wGiroDft = 0
050600030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
050700030822     C                             posDaDft > 0
050800030822     C*
050900030822     C* Gestisco il 1° giro
051000030822     C                   IF        wGiroDft = 0
051100030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
051200030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
051300030822     C* Incremento il contatore dei "giri"
051400030822     C                   EVAL      wGiroDft = 1
051500030822     C                   ELSE
051600030822     C                   EVAL      posDaDft = posADft
051700030822     C                   ENDIF
051800030822     C* Eseguo lo scan x trovare la fine del campo corrente
051900030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
052000030822     C*
052100030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
052200030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
052300030822     C                   IF        posDaDft > 0 AND
052400030822     C                             posADft  > 0
052500040708     C* POE
052600040708     C                   IF        %subst(
052700040708     C                             %subst(vlrppt:posDaDft+1:
052800040708     C                             posADft-posDaDft-1):1:3)
052900040708     C                             = 'POE'
053000040708     C                   EVAL      PiStr=%trim(%subst(
053100040708     C                             %subst(vlrppt:posDaDft+1:
053200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053300040708     C                   EXSR      CHKNUM
053400040708     C                   IF        PiInt=*on
053500040708     C                   Z-ADD     PiVal         VAOPOE
053600040708     C                   ENDIF
053700040708     C                   ENDIF
053800040708     C* TOR
053900040708     C                   IF        %subst(
054000040708     C                             %subst(vlrppt:posDaDft+1:
054100040708     C                             posADft-posDaDft-1):1:3)
054200040708     C                             = 'TOR'
054300040708     C                   EVAL      VAOTOR=%trim(%subst(
054400040708     C                             %subst(vlrppt:posDaDft+1:
054500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054600040708     C                   ENDIF
054700040708     C* TCO
054800040708     C                   IF        %subst(
054900040708     C                             %subst(vlrppt:posDaDft+1:
055000040708     C                             posADft-posDaDft-1):1:3)
055100040708     C                             = 'TCO'
055200040708     C                   EVAL      VAOTCO=%trim(%subst(
055300040708     C                             %subst(vlrppt:posDaDft+1:
055400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055500040708     C                   ENDIF
055600040707     C* COR
055700030822     C                   IF        %subst(
055800030822     C                             %subst(vlrppt:posDaDft+1:
055900030822     C                             posADft-posDaDft-1):1:3)
056000040707     C                             = 'COR'
056100030822     C                   EVAL      PiStr=%trim(%subst(
056200030822     C                             %subst(vlrppt:posDaDft+1:
056300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056400030822     C                   EXSR      CHKNUM
056500030822     C                   IF        PiInt=*on
056600040708     C                   Z-ADD     PiVal         VAOCOR
056700030822     C                   ENDIF
056800030822     C                   ENDIF
056900040708     C* CRC
057000040708     C                   IF        %subst(
057100040708     C                             %subst(vlrppt:posDaDft+1:
057200040708     C                             posADft-posDaDft-1):1:3)
057300040708     C                             = 'CRC'
057400040708     C                   EVAL      PiStr=%trim(%subst(
057500040708     C                             %subst(vlrppt:posDaDft+1:
057600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057700040708     C                   EXSR      CHKNUM
057800040708     C                   IF        PiInt=*on
057900040708     C                   Z-ADD     PiVal         VAOCRC
058000040708     C                   ENDIF
058100040708     C                   ENDIF
058200040708     C* FFD
058300040708     C                   IF        %subst(
058400040708     C                             %subst(vlrppt:posDaDft+1:
058500040708     C                             posADft-posDaDft-1):1:3)
058600040708     C                             = 'FFD'
058700040708     C                   EVAL      VAOFFD=%trim(%subst(
058800040708     C                             %subst(vlrppt:posDaDft+1:
058900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059000040708     C                   ENDIF
059100040708     C* ORR
059200040708     C                   IF        %subst(
059300040708     C                             %subst(vlrppt:posDaDft+1:
059400040708     C                             posADft-posDaDft-1):1:3)
059500040708     C                             = 'ORR'
059600040708     C                   EVAL      PiStr=%trim(%subst(
059700040708     C                             %subst(vlrppt:posDaDft+1:
059800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059900040708     C                   EXSR      CHKNUM
060000040708     C                   IF        PiInt=*on
060100040708     C                   Z-ADD     PiVal         VAOORR
060200040708     C                   ENDIF
060300040708     C                   ENDIF
060400040712     C* NCL
060500040712     C                   IF        %subst(
060600040712     C                             %subst(vlrppt:posDaDft+1:
060700040712     C                             posADft-posDaDft-1):1:3)
060800040712     C                             = 'NCL'
060900040712     C                   EVAL      PiStr=%trim(%subst(
061000040712     C                             %subst(vlrppt:posDaDft+1:
061100040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061200040712     C                   EXSR      CHKNUM
061300040712     C                   IF        PiInt=*on
061400040712     C                   Z-ADD     PiVal         VAONCL
061500040712     C                   ENDIF
061600040712     C                   ENDIF
061700040712     C* PKG
061800040712     C                   IF        %subst(
061900040712     C                             %subst(vlrppt:posDaDft+1:
062000040712     C                             posADft-posDaDft-1):1:3)
062100040712     C                             = 'PKG'
062200040712     C                   EVAL      PiStr=%trim(%subst(
062300040712     C                             %subst(vlrppt:posDaDft+1:
062400040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062500040712     C                   EXSR      CHKNUM
062600040712     C                   IF        PiNum=*on
062700040712     C                   Z-ADD     PiVal         VAOPKG
062800040712     C                   ENDIF
062900040712     C                   ENDIF
063000040712     C* VLM
063100040712     C                   IF        %subst(
063200040712     C                             %subst(vlrppt:posDaDft+1:
063300040712     C                             posADft-posDaDft-1):1:3)
063400040712     C                             = 'VLM'
063500040712     C                   EVAL      PiStr=%trim(%subst(
063600040712     C                             %subst(vlrppt:posDaDft+1:
063700040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063800040712     C                   EXSR      CHKNUM
063900040712     C                   IF        PiNum=*on
064000040712     C                   Z-ADD     PiVal         VAOVLM
064100040712     C                   ENDIF
064200040712     C                   ENDIF
064300040708     C* NAM
064400040708     C                   IF        %subst(
064500040708     C                             %subst(vlrppt:posDaDft+1:
064600040708     C                             posADft-posDaDft-1):1:3)
064700040708     C                             = 'NAM'
064800040708     C                   EVAL      VAONAM=%trim(%subst(
064900040708     C                             %subst(vlrppt:posDaDft+1:
065000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065100040708     C                   ENDIF
065200040708     C* PAG
065300030822     C                   IF        %subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):1:3)
065600040708     C                             = 'PAG'
065700040708     C                   EVAL      VAOPAG=%trim(%subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066000030822     C                   ENDIF
066100040708     C* KSC
066200040708     C                   IF        %subst(
066300040708     C                             %subst(vlrppt:posDaDft+1:
066400040708     C                             posADft-posDaDft-1):1:3)
066500040708     C                             = 'KSC'
066600040708     C                   EVAL      PiStr=%trim(%subst(
066700040708     C                             %subst(vlrppt:posDaDft+1:
066800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066900040708     C                   EXSR      CHKNUM
067000040708     C                   IF        PiInt=*on
067100040708     C                   Z-ADD     PiVal         VAOKSC
067200040708     C                   ENDIF
067300040708     C                   ENDIF
067400040708     C* CTR
067500040708     C                   IF        %subst(
067600040708     C                             %subst(vlrppt:posDaDft+1:
067700040708     C                             posADft-posDaDft-1):1:3)
067800040708     C                             = 'CTR'
067900040708     C                   EVAL      VAOCTR=%trim(%subst(
068000040708     C                             %subst(vlrppt:posDaDft+1:
068100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068200040708     C                   ENDIF
068300040708     C* POC
068400040708     C                   IF        %subst(
068500040708     C                             %subst(vlrppt:posDaDft+1:
068600040708     C                             posADft-posDaDft-1):1:3)
068700040708     C                             = 'POC'
068800040708     C                   EVAL      PiStr=%trim(%subst(
068900040708     C                             %subst(vlrppt:posDaDft+1:
069000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069100040708     C                   EXSR      CHKNUM
069200040708     C                   IF        PiInt=*on
069300040708     C                   Z-ADD     PiVal         VAOPOC
069400040708     C                   ENDIF
069500040708     C                   ENDIF
069600040708     C* NO1
069700040708     C                   IF        %subst(
069800040708     C                             %subst(vlrppt:posDaDft+1:
069900040708     C                             posADft-posDaDft-1):1:3)
070000040708     C                             = 'NO1'
070100040708     C                   EVAL      VAONO1=%trim(%subst(
070200040708     C                             %subst(vlrppt:posDaDft+1:
070300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070400040708     C                   ENDIF
070500040708     C* DDT
070600030822     C                   IF        %subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):1:3)
070900040708     C                             = 'DDT'
071000040708     C                   EVAL      VAODDT=%trim(%subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071300030822     C                   ENDIF
071400040708     C* RFA
071500040708     C                   IF        %subst(
071600040708     C                             %subst(vlrppt:posDaDft+1:
071700040708     C                             posADft-posDaDft-1):1:3)
071800040708     C                             = 'RFA'
071900040708     C                   EVAL      VAORFA=%trim(%subst(
072000040708     C                             %subst(vlrppt:posDaDft+1:
072100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072200040708     C                   ENDIF
072300040708     C* FLO
072400040708     C                   IF        %subst(
072500040708     C                             %subst(vlrppt:posDaDft+1:
072600040708     C                             posADft-posDaDft-1):1:3)
072700040708     C                             = 'FLO'
072800040708     C                   EVAL      VAOFLO=%trim(%subst(
072900040708     C                             %subst(vlrppt:posDaDft+1:
073000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073100040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
073200040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
073300040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
073400040708     C                   ENDIF
073500030822     C* ...
073600030822     C                   ENDIF
073700030822     C                   ENDDO
073800020204     C*
073900010330     C                   ENDSR
074000010607     C*----------------------------------------------------*
074100040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
074200010607     C*----------------------------------------------------*
074300040708     C     IMPVAO        BEGSR
074400010607     C*
074500010607     C                   SETOFF                                       3132
074600030515     C*
074700040708     C                   MOVEL     datcor        VAODAO
074800040708     C                   MOVEL     wn14          VAOOAO
074900010607     C*
075000030715     C**********
075100030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
075200030715     C**********
075300030715     C                   IF        wGiro = *zeros
075400031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
075500031201     C     minu:maiu     XLATE     vindta        vindta
075600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
075700040324     C                   DOW       posDa <= %len(%trimr(vindta))
075800030715     C*
075900030715     C* Gestisco il 1° campo
076000030715     C                   IF        i = 1
076100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
076200030715     C                   EVAL      posDa = 1
076300030715     C* Eseguo lo scan x trovare la fine del primo campo
076400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
076500030715     C* A questo punto "estraggo" il campo corrente
076600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
076700030715     C                                                    (posA-posDa))
076800030715     C* X i campi successivi al 1°
076900030715     C                   ELSE
077000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
077100030715     C                   EVAL      posDa = posA + 1
077200030715     C* Eseguo lo scan x trovare la fine del campo corrente
077300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
077400030715     C* Gestisco l'ultimo campo
077500030715     C                   IF        posA = *zeros
077600040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
077700030715     C* A questo punto "estraggo" il campo corrente
077800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
077900030715     C                                                    (posA-posDa))
078000030715     C                   LEAVE
078100030715     C                   ELSE
078200030715     C* A questo punto "estraggo" il campo corrente
078300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
078400030715     C                                                    (posA-posDa))
078500030715     C                   ENDIF
078600030715     C                   ENDIF
078700030715     C* Incremento il contatore d campo
078800030715     C                   EVAL      i = i +1
078900030715     C                   ENDDO
079000030715     C*
079100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
079200030715     C* delimitazione testo
079300030715     C                   EVAL      i = 1
079400030715     C                   DOW       i < %elem(SkSplitFLD)
079500030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
079600030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
079700030715     C                   EVAL      i = i + 1
079800030715     C                   ENDDO
079900030715     C                   EVAL      wGiro = 1
080000030715     C                   ELSE
080100030715     C                   EVAL      wGiro = 2
080200031204     C**********
080300031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
080400031204     C* d separatore campo
080500031204     C**********
080600040304     C                   IF        %subst(vindta:1:1) = CharCSV                 * all'inizio
080700040304     C                   EVAL      vindta = ' ' + vindta
080800040304     C                   ENDIF
080900040304     C***                DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
081000040304     C***                EVAL      vindta = %subst(vindta:2)
081100040304     C***                ENDDO
081200031204     C*
081300031204     C                   Z-ADD     *zeros        lunghInput        4 0
081400040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
081500040324     C                   IF        %subst(%trimr(vindta):lunghInput:1) = CharCSV
081600040304     C                   EVAL      vindta = vindta + ' '
081700040304     C                   ENDIF
081800040324     C***                DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
081900040324     C***                EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
082000040304     C***                EVAL      lunghInput = %len(%trim(vindta))
082100040304     C***                ENDDO
082200030715     C**********
082300030715     C* Effettuo lo split del campo dati d input
082400030715     C**********
082500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
082600040324     C                   DOW       posDa <= %len(%trimr(vindta))
082700030715     C*
082800030715     C* Gestisco il 1° campo
082900030715     C                   IF        i = 1
083000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
083100030715     C                   EVAL      posDa = 1
083200030715     C* Eseguo lo scan x trovare la fine del primo campo
083300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083400030715     C* A questo punto "estraggo" il campo corrente
083500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
083600030715     C                                                    (posA-posDa))
083700030715     C* X i campi successivi al 1°
083800030715     C                   ELSE
083900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
084000030715     C                   EVAL      posDa = posA + 1
084100031204     C* Verifico che nn vi sia il campo nn valorizzato
084200031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
084300031204     C* Se campo nn valorizzato skippo al prossimo
084400031204     C                   EVAL      SkSplitCSV(i) = *blanks
084500031204     C                   EVAL      posA  = posA + 1
084600031204     C                   ELSE
084700030715     C* Eseguo lo scan x trovare la fine del campo corrente
084800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084900030715     C* Gestisco l'ultimo campo
085000030715     C                   IF        posA = *zeros
085100040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
085200030715     C* A questo punto "estraggo" il campo corrente
085300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
085400030715     C                                                    (posA-posDa))
085500030715     C                   LEAVE
085600030715     C                   ELSE
085700030715     C* A questo punto "estraggo" il campo corrente
085800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
085900030715     C                                                    (posA-posDa))
086000030715     C                   ENDIF
086100031204     C                   ENDIF
086200030715     C                   ENDIF
086300030715     C* Incremento il contatore d campo
086400030715     C                   EVAL      i = i +1
086500030715     C                   ENDDO
086600030715     C*
086700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
086800030715     C* delimitazione testo
086900030715     C                   EVAL      i = 1
087000030715     C                   DOW       i < %elem(SkSplitCSV)
087100030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
087200030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
087300030715     C                   EVAL      i = i + 1
087400030715     C                   ENDDO
087500030715     C*
087600030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
087700030715     C                   EVAL      i = 1
087800030715     C                   DOW       i < %elem(SkSplitFLD)
087900030715     C*
088000030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
088100030715     C                   IF        SkSplitFLD(i) <> *blanks AND
088200030715     C                             SkSplitCSV(i) <> *blanks
088300030721     C*** ATB
088400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATB'
088500040708     C                   EVAL      VAOATB = SkSplitCSV(i)
088600030721     C                   ENDIF
088700040708     C*** TOR
088800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTOR'
088900040708     C                   EVAL      VAOTOR = SkSplitCSV(i)
089000040708     C                   ENDIF
089100040708     C*** TCO
089200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTCO'
089300040708     C                   EVAL      VAOTCO = SkSplitCSV(i)
089400040708     C                   ENDIF
089500040708     C*** RSO
089600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSO'
089700040708     C                   EVAL      VAORSO = SkSplitCSV(i)
089800030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
089900040708     C     '@':'A'       XLATE     VAORSO        VAORSO
090000030715     C* ==
090100030715     C                   ENDIF
090200040708     C*** INO
090300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINO'
090400040708     C                   EVAL      VAOINO = SkSplitCSV(i)
090500030715     C                   ENDIF
090600040708     C*** LOO
090700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOO'
090800040708     C                   EVAL      VAOLOO = SkSplitCSV(i)
090900030715     C                   ENDIF
091000040708     C*** PRO
091100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRO'
091200040708     C                   EVAL      VAOPRO = SkSplitCSV(i)
091300030715     C                   ENDIF
091400040708     C*** NAO
091500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAO'
091600040708     C                   EVAL      VAONAO = SkSplitCSV(i)
091700030721     C                   ENDIF
091800040708     C*** RSR
091900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSR'
092000040708     C                   EVAL      VAORSR = SkSplitCSV(i)
092100040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
092200040708     C     '@':'A'       XLATE     VAORSR        VAORSR
092300040708     C* ==
092400040708     C                   IF        VAORSR = *blanks
092500040708     C                   SETON                                        31
092600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
092700040708     C                             + ' ' + 'VAORSR'
092800040708     C                   ENDIF
092900040708     C                   ENDIF
093000040708     C*** INR
093100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINR'
093200040708     C                   EVAL      VAOINR = SkSplitCSV(i)
093300040708     C                   IF        VAOINR = *blanks
093400040708     C                   SETON                                        31
093500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
093600040708     C                             + ' ' + 'VAOINR'
093700040708     C                   ENDIF
093800040708     C                   ENDIF
093900040708     C*** LOR
094000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOR'
094100040708     C                   EVAL      VAOLOR = SkSplitCSV(i)
094200040708     C                   IF        VAOLOR = *blanks
094300040708     C                   SETON                                        31
094400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
094500040708     C                             + ' ' + 'VAOLOR'
094600040708     C                   ENDIF
094700040708     C                   ENDIF
094800040708     C*** PRR
094900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRR'
095000040708     C                   EVAL      VAOPRR = SkSplitCSV(i)
095100040708     C                   IF        VAOPRR = *blanks
095200040708     C                   SETON                                        31
095300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
095400040708     C                             + ' ' + 'VAOPRR'
095500040708     C                   ENDIF
095600040708     C                   ENDIF
095700040708     C*** NAR
095800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAR'
095900040708     C                   EVAL      VAONAR = SkSplitCSV(i)
096000040708     C                   ENDIF
096100040708     C*** RER
096200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORER'
096300040708     C                   EVAL      VAORER = SkSplitCSV(i)
096400040708     C                   ENDIF
096500040708     C*** TER
096600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTER'
096700040708     C                   EVAL      VAOTER = SkSplitCSV(i)
096800040708     C                   ENDIF
096900040708     C*** RSC
097000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSC'
097100040708     C                   EVAL      VAORSC = SkSplitCSV(i)
097200040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
097300040708     C     '@':'A'       XLATE     VAORSC        VAORSC
097400040708     C* ==
097500040708     C                   ENDIF
097600040708     C*** INC
097700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINC'
097800040708     C                   EVAL      VAOINC = SkSplitCSV(i)
097900040708     C                   ENDIF
098000040708     C*** LOC
098100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOC'
098200040708     C                   EVAL      VAOLOC = SkSplitCSV(i)
098300040708     C                   ENDIF
098400040708     C*** PRC
098500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRC'
098600040708     C                   EVAL      VAOPRC = SkSplitCSV(i)
098700040708     C                   ENDIF
098800040708     C*** NAC
098900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAC'
099000040708     C                   EVAL      VAONAC = SkSplitCSV(i)
099100040708     C                   ENDIF
099200040708     C*** FFD
099300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFFD'
099400040708     C                   EVAL      VAOFFD = SkSplitCSV(i)
099500040708     C                   ENDIF
099600040708     C*** RMP
099700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORMP'
099800040708     C                   EVAL      VAORMP = SkSplitCSV(i)
099900040708     C                   ENDIF
100000040708     C*** NAM
100100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAM'
100200040708     C                   EVAL      VAONAM = SkSplitCSV(i)
100300040708     C                   ENDIF
100400040708     C*** PAG
100500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPAG'
100600040708     C                   EVAL      VAOPAG = SkSplitCSV(i)
100700040708     C                   ENDIF
100800040708     C*** CTR
100900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCTR'
101000040708     C                   EVAL      VAOCTR = SkSplitCSV(i)
101100040708     C                   ENDIF
101200040708     C*** ZOR
101300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOZOR'
101400040708     C                   EVAL      VAOZOR = SkSplitCSV(i)
101500040708     C                   ENDIF
101600040708     C*** NO1
101700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO1'
101800040708     C                   EVAL      VAONO1 = SkSplitCSV(i)
101900030715     C                   ENDIF
102000040708     C*** NO2
102100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO2'
102200040708     C                   EVAL      VAONO2 = SkSplitCSV(i)
102300030715     C                   ENDIF
102400040708     C*** DDT
102500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODDT'
102600040708     C                   EVAL      VAODDT = SkSplitCSV(i)
102700030715     C                   ENDIF
102800040708     C*** RFA
102900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORFA'
103000040708     C                   EVAL      VAORFA = SkSplitCSV(i)
103100030715     C                   ENDIF
103200040708     C*** SPI
103300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSPI'
103400040708     C                   EVAL      VAOSPI = SkSplitCSV(i)
103500030715     C                   ENDIF
103600040708     C*** FLO
103700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
103800040708     C                   EVAL      VAOFLO = SkSplitCSV(i)
103900040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
104000040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
104100040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
104200030715     C                   ENDIF
104300030715     C
104400030721     C*
104500030721     C* Reperisco quindi i campi numerici...
104600040708     C*** POE
104700040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
104800040723     C                   EVAL      PiStr=SkSplitCSV(i)
104900040723     C                   EXSR      CHKNUM
105000040723     C                   IF        PiInt=*on    AND
105100040723     C                             PiVal<=999   AND
105200040723     C                             PiVal>*zeros
105300040723     C                   Z-ADD     PiVal         VAOPOE
105400040723     C                   ELSE
105500040723     C                   SETON                                        32
105600040723     C                   EVAL      VAOPOE = *zeros
105700040723     C                   EVAL      vinmsg = %trimr(vinmsg)
105800040723     C                             + ' ' + 'VAOPOE'
105900040723     C                   ENDIF
106000040723     C                   ENDIF
106100040708     C*** COR
106200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
106300040708     C                   EVAL      PiStr=SkSplitCSV(i)
106400040708     C                   EXSR      CHKNUM
106500040708     C                   IF        PiInt=*on
106600040708     C                   Z-ADD     PiVal         VAOCOR
106700040708     C                   ELSE
106800040708     C                   SETON                                        31
106900040708     C                   EVAL      VAOCOR = *zeros
107000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
107100040708     C                             + ' ' + 'VAOCOR'
107200040708     C                   ENDIF
107300040708     C                   ENDIF
107400040708     C*** CAO
107500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
107600130502     C                   EVAL      VAOCAO = SkSplitCSV(i)
107700130502     C***                EVAL      PiStr=SkSplitCSV(i)
107800130502     C***                EXSR      CHKNUM
107900130502     C***                IF        PiInt=*on
108000130502     C***                Z-ADD     PiVal         Num5_0
108100130502     C***                MOVEL(p)  Num5_0        VAOCAO
108200130502     C***                ELSE
108300130502     C***                SETON                                        32
108400130502     C***                EVAL      VAOCAO = *zeros
108500130502     C***                EVAL      vinmsg = %trimr(vinmsg)
108600130502     C***                          + ' ' + 'VAOCAO'
108700130502     C***                ENDIF
108800030721     C                   ENDIF
108900040708     C*** CRA
109000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
109100030721     C                   EVAL      PiStr=SkSplitCSV(i)
109200030721     C                   EXSR      CHKNUM
109300030721     C                   IF        PiInt=*on
109400040708     C                   Z-ADD     PiVal         VAOCRA
109500030721     C                   ELSE
109600030721     C                   SETON                                        32
109700040708     C                   EVAL      VAOCRA = *zeros
109800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109900040708     C                             + ' ' + 'VAOCRA'
110000030721     C                   ENDIF
110100030721     C                   ENDIF
110200040708     C*** CAR
110300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
110400130502     C                   EVAL      VAOCAR = SkSplitCSV(i)
110500130502     C***                EVAL      PiStr=SkSplitCSV(i)
110600130502     C***                EXSR      CHKNUM
110700130502     C***                IF        PiInt=*on
110800130502     C***                Z-ADD     PiVal         Num5_0
110900130502     C***                MOVEL(p)  Num5_0        VAOCAR
111000130502     C***                ELSE
111100130502     C***                SETON                                        31
111200130502     C***                EVAL      VAOCAR = *zeros
111300130502     C***                EVAL      vinmsg = %trimr(vinmsg)
111400130502     C***                          + ' ' + 'VAOCAR'
111500130502     C***                ENDIF
111600040708     C                   ENDIF
111700040708     C*** CRC
111800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
111900040708     C                   EVAL      PiStr=SkSplitCSV(i)
112000040708     C                   EXSR      CHKNUM
112100040708     C                   IF        PiInt=*on
112200040708     C                   Z-ADD     PiVal         VAOCRC
112300040708     C                   ELSE
112400040708     C                   SETON                                        32
112500040708     C                   EVAL      VAOCRC = *zeros
112600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
112700040708     C                             + ' ' + 'VAOCRC'
112800040708     C                   ENDIF
112900040708     C                   ENDIF
113000040708     C*** CAC
113100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
113200130502     C                   EVAL      VAOCAC = SkSplitCSV(i)
113300130502     C***                EVAL      PiStr=SkSplitCSV(i)
113400130502     C***                EXSR      CHKNUM
113500130502     C***                IF        PiInt=*on
113600130502     C***                Z-ADD     PiVal         Num5_0
113700130502     C***                MOVEL(p)  Num5_0        VAOCAC
113800130502     C***                ELSE
113900130502     C***                SETON                                        32
114000130502     C***                EVAL      VAOCAC = *zeros
114100130502     C***                EVAL      vinmsg = %trimr(vinmsg)
114200130502     C***                          + ' ' + 'VAOCAC'
114300130502     C***                ENDIF
114400040708     C                   ENDIF
114500040708     C*** DAR
114600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
114700040708     C                   EVAL      PiStr=SkSplitCSV(i)
114800040708     C                   EXSR      CHKNUM
114900040708     C                   IF        PiInt=*on
115000040708     C                   Z-ADD     PiVal         VAODAR
115100040708     C                   ELSE
115200040708     C                   SETON                                        32
115300040708     C                   EVAL      VAODAR = *zeros
115400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
115500040708     C                             + ' ' + 'VAODAR'
115600040708     C                   ENDIF
115700040708     C                   ENDIF
115800040708     C*** ORR
115900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
116000040708     C                   EVAL      PiStr=SkSplitCSV(i)
116100040708     C                   EXSR      CHKNUM
116200040708     C                   IF        PiInt=*on
116300040708     C                   Z-ADD     PiVal         VAOORR
116400040708     C                   ELSE
116500040708     C                   SETON                                        32
116600040708     C                   EVAL      VAOORR = *zeros
116700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
116800040708     C                             + ' ' + 'VAOORR'
116900040708     C                   ENDIF
117000040708     C                   ENDIF
117100030721     C*** NCL
117200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
117300030721     C                   EVAL      PiStr=SkSplitCSV(i)
117400030721     C                   EXSR      CHKNUM
117500030721     C                   IF        PiInt=*on
117600040708     C                   Z-ADD     PiVal         VAONCL
117700030721     C                   ELSE
117800030721     C                   SETON                                        32
117900040708     C                   EVAL      VAONCL = *zeros
118000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118100040708     C                             + ' ' + 'VAONCL'
118200030721     C                   ENDIF
118300030721     C                   ENDIF
118400040708     C*** PKG
118500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
118600030721     C                   EVAL      PiStr=SkSplitCSV(i)
118700030721     C                   EXSR      CHKNUM
118800030721     C                   IF        PiNum=*on
118900040708     C                   Z-ADD     PiVal         VAOPKG
119000030721     C                   ELSE
119100030721     C                   SETON                                        32
119200040708     C                   EVAL      VAOPKG = *zeros
119300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119400040708     C                             + ' ' + 'VAOPKG'
119500030721     C                   ENDIF
119600030721     C                   ENDIF
119700040708     C*** VLM
119800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
119900040708     C                   EVAL      PiStr=SkSplitCSV(i)
120000040708     C                   EXSR      CHKNUM
120100040708     C                   IF        PiNum=*on
120200040708     C                   Z-ADD     PiVal         VAOVLM
120300040708     C                   ELSE
120400040708     C                   SETON                                        32
120500040708     C                   EVAL      VAOVLM = *zeros
120600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
120700040708     C                             + ' ' + 'VAOVLM'
120800040708     C                   ENDIF
120900040708     C                   ENDIF
121000040708     C*** BNC
121100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
121200040708     C                   EVAL      PiStr=SkSplitCSV(i)
121300040708     C                   EXSR      CHKNUM
121400040708     C                   IF        PiInt=*on
121500040708     C                   Z-ADD     PiVal         VAOBNC
121600040708     C                   ELSE
121700040708     C                   SETON                                        32
121800040708     C                   EVAL      VAOBNC = *zeros
121900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
122000040708     C                             + ' ' + 'VAOBNC'
122100040708     C                   ENDIF
122200040708     C                   ENDIF
122300040708     C*** BLC
122400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
122500040708     C                   EVAL      PiStr=SkSplitCSV(i)
122600040708     C                   EXSR      CHKNUM
122700040708     C                   IF        PiInt=*on
122800040708     C                   Z-ADD     PiVal         VAOBLC
122900040708     C                   ELSE
123000040708     C                   SETON                                        32
123100040708     C                   EVAL      VAOBLC = *zeros
123200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
123300040708     C                             + ' ' + 'VAOBLC'
123400040708     C                   ENDIF
123500040708     C                   ENDIF
123600040708     C*** ATT
123700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
123800040708     C                   EVAL      PiStr=SkSplitCSV(i)
123900040708     C                   EXSR      CHKNUM
124000040708     C                   IF        PiInt=*on
124100040708     C                   Z-ADD     PiVal         VAOATT
124200040708     C                   ELSE
124300040708     C                   SETON                                        32
124400040708     C                   EVAL      VAOATT = *zeros
124500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
124600040708     C                             + ' ' + 'VAOATT'
124700040708     C                   ENDIF
124800040708     C                   ENDIF
124900040708     C*** MTC
125000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
125100040708     C                   EVAL      PiStr=SkSplitCSV(i)
125200040708     C                   EXSR      CHKNUM
125300040708     C                   IF        PiInt=*on
125400040708     C                   Z-ADD     PiVal         VAOMTC
125500040708     C                   ELSE
125600040708     C                   SETON                                        32
125700040708     C                   EVAL      VAOMTC = *zeros
125800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
125900040708     C                             + ' ' + 'VAOMTC'
126000040708     C                   ENDIF
126100040708     C                   ENDIF
126200040708     C*** KSC
126300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
126400030721     C                   EVAL      PiStr=SkSplitCSV(i)
126500030721     C                   EXSR      CHKNUM
126600030721     C                   IF        PiInt=*on      AND
126700030721     C                             PiVal<=9999999 AND
126800030721     C                             PiVal>*zeros
126900040708     C                   Z-ADD     PiVal         VAOKSC
127000030721     C                   ELSE
127100030721     C                   SETON                                        32
127200040708     C                   EVAL      VAOKSC = *zeros
127300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127400040708     C                             + ' ' + 'VAOKSC'
127500030721     C                   ENDIF
127600030721     C                   ENDIF
127700040708     C*** POR
127800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
127900030721     C                   EVAL      PiStr=SkSplitCSV(i)
128000030721     C                   EXSR      CHKNUM
128100030721     C                   IF        PiInt=*on    AND
128200030721     C                             PiVal<=999   AND
128300030721     C                             PiVal>*zeros
128400040708     C                   Z-ADD     PiVal         VAOPOR
128500030721     C                   ELSE
128600030721     C                   SETON                                        32
128700040708     C                   EVAL      VAOPOR = *zeros
128800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128900040708     C                             + ' ' + 'VAOPOR'
129000030721     C                   ENDIF
129100030721     C                   ENDIF
129200040708     C*** POC
129300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
129400040708     C                   EVAL      PiStr=SkSplitCSV(i)
129500040708     C                   EXSR      CHKNUM
129600040708     C                   IF        PiInt=*on    AND
129700040708     C                             PiVal<=999   AND
129800040708     C                             PiVal>*zeros
129900040708     C                   Z-ADD     PiVal         VAOPOC
130000040708     C                   ELSE
130100040708     C                   SETON                                        32
130200040708     C                   EVAL      VAOPOC = *zeros
130300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
130400040708     C                             + ' ' + 'VAOPOC'
130500040708     C                   ENDIF
130600040708     C                   ENDIF
130700040708     C*** STO
130800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
130900040708     C                   EVAL      PiStr=SkSplitCSV(i)
131000040708     C                   EXSR      CHKNUM
131100040708     C                   IF        PiInt=*on
131200040708     C                   Z-ADD     PiVal         VAOSTO
131300040708     C                   ELSE
131400040708     C                   SETON                                        32
131500040708     C                   EVAL      VAOSTO = *zeros
131600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
131700040708     C                             + ' ' + 'VAOSTO'
131800040708     C                   ENDIF
131900040708     C                   ENDIF
132000030715     C*
132100030715     C                   ENDIF
132200030715     C                   EVAL      i = i + 1
132300030715     C                   ENDDO
132400101210     C*
132500101210     C* Se al termine del parsing il POE e/o il COR nn risultano valorizzato => errore
132600101210     C                   IF        VAOPOE = *zeros OR
132700101210     C                             VAOCOR = *zeros
132800101210     C                   SETON                                        31
132900101210     C                   ENDIF
133000020204     C*
133100030715     C                   ENDIF
133200020204     C*
133300010607     C                   ENDSR
133400010607     C*----------------------------------------------------*
133500020204
133600010330
133700010330
133800010330     C*----------------------------------------------------*
133900010330     C*  CONTROLLO NUMERICITA' CAMPI
134000010330     C*----------------------------------------------------*
134100010330     C     CHKNUM        BEGSR
134200010330     C*
134300010606     C                   IF        PiDecChr = *blanks
134400030715     C                   EVAL      PiDecChr = CharNUM
134500010606     C                   ENDIF
134600010606     C*
134700010606     C                   CALL(e)   'ISNUMERIC'
134800010330     C                   PARM                    PiStr            30
134900010606     C                   PARM                    PiDecChr          1
135000010330     C                   PARM      *ZEROS        PiVal            30 9
135100010330     C                   PARM      '0'           PiInt             1
135200010330     C                   PARM      '0'           PiNum             1
135300010330     C                   IF        %error
135400010606     C                   EVAL      PiNum=*off
135500010330     C                   ENDIF
135600010330     C*
135700010330     C                   ENDSR
135800010330     C***
135900010330
136000010601
136100010601
136200010601
136300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
136400130125     C***  repfil        BEGSR
136500010601     C*
136600130125     C***                if        invfil = *zeros and
136700130125     C***                          depfil > *zeros and
136800130125     C***                          (vinflg = *blanks or
136900130125     C***                           vinflg = *zeros)
137000130125     C***
137100130125     C***                eval      invfil = depfil
137200130125     C***                endif
137300010601     C*
137400130125     C***                if        depfil <> invfil and
137500130125     C***                          invfil > *zeros
137600130125     C***                eval      flgMulti = '1'
137700130125     C***                if        vinflg = *blanks
137800130125     C***                add       1             cntNonEl
137900130125     C***                endif
138000130125     C***                endif
138100010601     C*
138200130125     C***                if        vinflg = '2'
138300130125     C***                eval      flgStato = '2'
138400130125     C***                endif
138500010601     C*
138600130125     C***                ENDSR
138700010601     C***
138800010601
138900010601
139000010601
139100010330
139200010330
139300010330
139400990920      /TITLE Invio dei dati al punto operativo.
139500000613     C     invio         BEGSR
139600990920     C*
139700990920     C                   reset                   dscmz
139800010601     C                   move      invfil        cmzdst
139900990920     C                   eval      cmzfld = vlrfou
140000990920     C                   eval      cmzmbd = vlrhdl
140100990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
140200000710     C                   if        prmfir = *blanks
140300040708     C                   eval      cmzfla = 'FNVAO00F'
140400040708     C                   eval      cmzmba = 'FNVAO00F'
140500000710     C                   else
140600000710     C                   eval      cmzfla = prmfir
140700000710     C                   eval      cmzmba = prmfir
140800000710     C                   endif
140900990920     C                   eval      cmznrr = *zeros
141000990920     C                   move      §ctrok        cmznrr
141100021018     C                   eval      cmzlba = vlrfl1
141200990920     C                   call(e)   'TIS711C'
141300990920     C                   parm                    dscmz
141400990921     C                   parm      *blanks       esito
141500990920     C                   if        %error
141600990920     C                             or cmzerr = '1'
141700990921     C                             or esito  = '1'
141800000710     C                   eval      wrkesito = '3'
141900990920     C                   endif
142000990920     C*
142100000613     C                   ENDSR
142200990910
142300010601
142400010601
142500010601
142600010601
142700010601      /TITLE Invio dei dati al punto operativo.
142800010601     C     opeini        BEGSR
142900010601     C*
143000010601     C* Inizializzo flag e contatori operativi
143100010601     C                   movel     '0'           flgGiro           1
143200010601     C                   movel     '0'           flgMulti          1
143300010601     C                   movel     '1'           flgStato          1
143400010615     C                   movel     '0'           flgOk             1
143500010601     C                   z-add     *zeros        cntNonEl         10 0
143600010601     C                   z-add     *zeros        depfil            3 0
143700010601     C                   z-add     *zeros        invfil            3 0
143800010601     C*
143900010601     C                   ENDSR
144000010601     C***
144100050421
144200050421
144300050421
144400050421     C     *pssr         BEGSR
144500050421     C*
144600050421     C                   eval      wrkesito = '2'
144700050421     C*
144800050421     C                   ENDSR     '*CANCL'
144900050421     C***
145000060306
145100060306
145200060306     C*-------------------------------------------------------------------------*
145300060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
145400060306     C*-------------------------------------------------------------------------*
145500060306     C     sr_Control    begsr
145600060306     C*
145700060306     C* inizializzo a ok il flag d controllo abilitazione
145800060306     C                   eval      Cli_Abil = *on
145900060306     C
146000060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
146100060306     C                   clear                   woriIDC
146200060306     C                   eval      kteCOD = 'VAP'
146300060306     C                   eval      kteKE1 = oriIDC
146400060306     C     keytbe        chain     TNTBE01L
146500060306     C                   if        %found(Tntbe01l)
146600060306     C                   eval      woriIDC = TbeKe2
146700060306     C                   else
146800060306     C                   eval      Cli_Abil = *off
146900060306     C                   endif
147000060306     C*
147100060306     C                   endsr
147200010601
147300010601
147400010601
147500010330
147600010330
147700000613     C     *inzsr        BEGSR
147800990910     C*
147900990910     C     *entry        plist
148000990920     C                   parm                    tivlrds
148100990921     C                   parm      wrkesito      esito
148200000724     C                   parm                    prmlit
148300000710     C                   parm                    prmfir
148400010330     C*
148500010330     C* CALCOLA LA DATA CORRENTE
148600010330     C                   time                    wn14             14 0
148700040723     C                   movel     wn14          wn6               6 0          *ORA (6)
148800130125     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
148900130125     C                   eval      datcor = %dec(%date() : *ISO)
149000040723     C*
149100040723     C* Campi riferito al database
149200040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
149300040723     C     *like         define    tbeke1        kteKE1
149400040723     C*
149500040723     C* Definizione chiavi di lettura
149600040723     C* TNTBE01L - parziale
149700040723     C     keytbe        klist
149800040723     C                   kfld                    kteCOD                         *UTENTE
149900040723     C                   kfld                    kteKE1                         *CODICE
150000000613     C*
150100000613     C                   ENDSR
150200000613     C***
