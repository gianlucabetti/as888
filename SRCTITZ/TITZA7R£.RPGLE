000100040708      /TITLE Upload via Internet: traduzione in FNVAOWWR.
000200140124      *
000300140124      * PARTICOLARITA':
000400140124      * è esattamente uno standard (per cui copiamolo pari pari dal *pgm TITVOXLR e creiamo un TITZ)
000500140124      * tranne nella gestione del campo VAOFLO
000600140124      *
000700130114     H dftactgrp(*no) actgrp(*caller)
000800000313     F*
000900040723     Ffnacr01l  if   e           k disk
001000090617     Ftntbe01l  if   e           k disk
001100990910     Ftivin00r  uF   E             DISK    usropn
001200040708     FFnvaowwr  O    E             DISK    usropn
001300040723     Ftiori00f  O    E             DISK
001400050826     FTIVGD00F  O    E             DISK
001500000313     D*
001600010330     D*----------------------------------------------------
001700010330     D* DICHIARAZIOINE VARIABILI DI WRK
001800010330     D*----------------------------------------------------
001900010330     D dscmz         e ds                  inz
002000010330     D psds           sds
002100010330     D  procname         *PROC
002200010330     D tivlrds       e ds                  extname(tivlr00f)
002300050826     D fnvapds       e ds                  extname(fnvap00f)
002400040723     D dorm01        e ds
002500040723     D dfar          e ds
002600090617     d trul33ds      e ds
002700090617     d kpjba         e ds
002800010330     D esito           s              1
002900010330     D prmlit          s             10
003000010330     D prmfir          s             10
003100010330     D wrkesito        s                   like(esito)
003200010330     D rrnum           s              6  0 INZ(*zeros)
003300060306     D Cli_Abil        s              1    inz(*off)
003400060306     D woriIDC         s                   like(oriIDC) inz(*blanks)
003500090617     d §numori         s              7  0 inz
003600030715     D*------------------
003700030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
003800030715     D*------------------
003900030715     D SkSplitFLD      S             10    DIM(100)
004000030715     D SkSplitCSV      S            256    DIM(100)
004100030715     D CharCSV         S              1
004200030715     D CharTXT         S              1
004300030715     D CharNUM         S              1
004400090403     D CharSOS         S              1
004500030715     D posDa           S              3  0 INZ(*zeros)
004600030715     D posA            S              3  0 INZ(*zeros)
004700030715     D i               s              3  0 INZ(1)
004800030715     D wGiro           s              1  0 INZ(*zeros)
004900030822     D*------------------
005000030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005100030822     D*------------------
005200030822     D posDaDft        S              3  0 INZ(*zeros)
005300030822     D posADft         S              3  0 INZ(*zeros)
005400030822     D j               s              3  0 INZ(1)
005500030822     D wGiroDft        s              1  0 INZ(*zeros)
005600031201     D*------------------
005700031201     D* Costanti
005800031201     D*------------------
005900031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006000031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006100010330
006200091026
006300091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
006400091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
006500091026
006600010330
006700010330
006800000913     C                   reset                   rrnum
006900990921     C                   reset                   esito
007000990921     C                   reset                   wrkesito
007100010601     C*
007200010601     C                   exsr      opeini
007300040708     C                   exsr      rwvao
007400010601     C*
007500010601     C                   seton                                        lr
007600010601
007700010601
007800010601
007900010601
008000010601     C*--------------------------------------------------------
008100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008200010601     C*--------------------------------------------------------
008300010601     C     PREELA        BEGSR
008400010601     C*
008500010601     C* SE OCCORRE SPEDIRE IN FILIALE
008600010601     C                   if        invfil <> *zeros and
008700010601     C                             flgGiro = '0'
008800010601     C*
008900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009000010601     C                   eval      flgGiro = '1'
009100010601     C*
009200010601     C                   endif
009300010601     C*
009400010601     C                   ENDSR
009500010601     C***
009600010601
009700010601
009800010601
009900010601
010000010601
010100010601
010200010601
010300010601     C*--------------------------------------------------------
010400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010500010601     C*--------------------------------------------------------
010600010601     C     ENDELA        BEGSR
010700990915     C*
010800000616     C*
010900010601     C                   ENDSR
011000010601     C***
011100000613
011200010330
011300010601
011400010601
011500010601
011600010330     C*--------------------------------------------------------
011700040708     C* RWVAO   LEGGE TIVIN00R E SCRIVE FNVAOWWF              *
011800010330     C*--------------------------------------------------------
011900040708     C     RWVAO         BEGSR
012000010330     C*
012100010330     C                   if        not %open(tivin00r)
012200010330     C                   open      tivin00r
012300010330     C                   endif
012400040708     C                   if        not %open(fnvaowwr)
012500040708     C                   open      fnvaowwr
012600010330     C                   endif
012700010330     C*
012800010604     C                   clear                   §CTROK
012900010604     C                   clear                   §CTRMO
013000010604     C                   clear                   §CTRNO
013100010330     C*
013200030822     C                   do        *HIVAL
013300010330     C*
013400030822     C                   read      tivin00r                               70
013500010618     C*
013600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
013700010618     C                   if        vinflg = '1'
013800010618     C                   eval      flgOk = '1'
013900010618     C                   endif
014000010618     C*
014100040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
014200040510     C                   z-add     *zeros        wLenVINDTA        4 0
014300040510     C                   z-add     *zeros        wFlgVINDTA        1 0
014400040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
014500040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
014600040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
014700040510     C                   z-add     1             wFlgVINDTA
014800040510     C                   leave
014900040510     C                   endif
015000040510     C                   enddo
015100040510     C*
015200040218     C                   if        vindta > *blanks AND
015300040510     C                             wFlgVINDTA = 1
015400010330     C                   add       1             rrnum
015500010330     C*
015600010601     C                   if        *in70 = *off and
015700010330     C                             (vinflg = *blanks
015800010330     C                              or vinflg = '0'
015900010330     C                              or vinflg = '2')
016000010330     C*
016100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
016200010711     C                   if        vinflg = *blanks or vinflg = '0'
016300010711     C                   clear                   vinmsg
016400010711     C                   endif
016500010601     C*
016600010330     C                   exsr      inzvar
016700090403     C*
016800090403     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
016900090403     C* separatori campi
017000090403     C                   if        CharTXT <> *blanks
017100090403     C                   z-add     *zeros        wLenVINDTA        4 0
017200090403     C                   z-add     *zeros        wFlgVINDTA        1 0
017300090403     C                   setoff                                       80
017400090403     C                   dow       wLenVINDTA < %len(%trim(vindta))
017500090403     C                   eval      wLenVINDTA = wLenVINDTA + 1
017600090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
017700090403     C                   if        not *in80
017800090403     C                   seton                                        80
017900090403     C                   else
018000090403     C                   setoff                                       80
018100090403     C                   endif
018200090403     C                   endif
018300090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
018400090403     C                             and *in80 = *on
018500090403     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
018600090403     C                   endif
018700090403     C                   enddo
018800090403     C                   endif
018900090403     C*
019000040708     C                   exsr      impvao
019100010601     C*
019200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019300101210     C                   if        not *IN31
019400040708     C                   eval      depfil = VAOPOE
019500130125     C***                exsr      repfil
019600130125     C***                if        depfil = invfil
019700021025     C                   if        vlrpoi = 999
019800130125     C***                move(P)   invfil        VAOPOE
019900021025     C                   else
020000040708     C                   move(P)   vlrpoi        VAOPOE
020100021025     C                   endif
020200130125     C***                endif
020300101210     C                   endif
020400010601     C*
020500010601     C                   exsr      PREELA
020600010601     C*
020700010604     C* Ebbene...
020800010604     C*
020900030822     C  N31              add       1             §CTROK            7 0
021000030822     C   32              add       1             §CTRMO            7 0
021100030822     C   31              add       1             §CTRNO            7 0
021200030822     C                   if        wGiro = 2
021300040723     C  N31              EXSR      CHKCOR
021400040723     C  N31              EXSR      WRIORI
021500050826     C  N31              EXSR      WRIVGD
021600040708     C  N31              write     FNVAO000
021700030822     C                   endif
021800010604     C*
021900010604     C                   if        *in31 = *off and
022000010604     C                             *in32 = *off
022100010604     C                   eval      vinflg = '1'
022200010604     C                   else
022300010604     C                   eval      vinflg = '2'
022400010604     C                   endif
022500010601     C*
022600010604     C                   endif
022700010604     C*
022800010330     C                   else
022900010330     C                   eval      vinflg = '1'
023000010330     C                   endif
023100010601     C*
023200010601     C  N70              update    tivin000
023300010330     C*
023400030822     C  N70              enddo
023500010601     C
023600010601     C*
023700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
023800010601     C                   if        cntNonEl = *zeros or
023900010601     C                             flgMulti = '0'
024000010330     C* Se non ci sono record con errori ...
024100010601     C                   if        §ctrno = 0 and
024200010604     C                             §ctrmo = 0 and
024300010601     C                             flgStato <> '2'
024400010330     C* ... restituisco esito OK.
024500010330     C                   eval      wrkesito = '0'
024600010330     C                   else
024700010330     C                   if        §ctrok > 0
024800010330     C                   eval      wrkesito = '1'
024900010330     C                   else
025000010615     C                   if        flgOk = '0'
025100010615     C                   eval      wrkesito = '2'
025200010615     C                   else
025300010615     C                   eval      wrkesito = '6'
025400010615     C                   endif
025500010330     C                   endif
025600010330     C                   endif
025700010601     C                   else
025800010601     C                   eval      wrkesito = '9'
025900010601     C                   endif
026000010330     C*
026100010330     C                   if        %open(tivin00r)
026200010330     C                   close     tivin00r
026300010330     C                   endif
026400040708     C                   if        %open(fnvaowwr)
026500040708     C                   close     fnvaowwr
026600010330     C                   endif
026700010601     C*
026800010601     C                   if        vlrpoi <> 999
026900010601     C                   eval      invfil = vlrpoi
027000010601     C                   endif
027100101210     C*
027200101210     C                   if        invfil = 0 and
027300101210     C                             depfil > 0
027400101210     C                   eval      invfil = depfil
027500101210     C                   endif
027600010330     C*
027700010330     C                   if        §ctrok > 0
027800010601     C                             and invfil > *zeros
027900010330     C                   exsr      invio
028000010330     C                   endif
028100010601     C*
028200010618     C                   if        flgGiro = '1'
028300010601     C                   exsr      endela
028400010618     C                   endif
028500010330     C*
028600010330     C                   ENDSR
028700010330     C***
028800050323
028900050323
029000050323     C*------------------------------------------------------------------------*
029100050323     C* CHKCOR - Considerazioni su VAOCOR
029200050323     C*------------------------------------------------------------------------*
029300050323     C     CHKCOR        BEGSR
029400050323     C*
029500050323     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
029600050323     C                   if        vaoTCO = 'F'
029700050323     C* Quindi verifico se il codice ordinante non è valorizzato
029800050323     C                   if        vaoCOR = *zeros
029900050323     C*                  if        vaoCOR = *zeros  AND
030000050323     C*                            vaoRSO = *blanks AND
030100050323     C*                            vaoINO = *blanks AND
030200050323     C*                            vaoCAO = *blanks AND
030300050323     C*                            vaoLOO = *blanks AND
030400050323     C*                            vaoPRO = *blanks AND
030500050323     C*                            vaoNAO = *blanks
030600050323     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
030700050323     C                   move      vlrKSC        wCli              7 0
030800050323     C                   movel(p)  wCli          acrCRO
030900050323     C     acrCRO        setll     fnacr01l
031000050323     C                   if        %found(fnacr01l)
031100050323     C                   read      fnacr01l
031200050323     C                   dow       not %eof(fnacr01l)
031300050323     C                   movel     acrCRO        wCliAcr           7 0
031400050323     C                   if        wCliAcr = wCli
031500050323     C                   eval      vaoCOR = acrCRO
031600050323     C                   leave
031700050323     C                   else
031800050323     C                   leave
031900050323     C                   endif
032000050323     C                   read      fnacr01l
032100050323     C                   enddo
032200050323     C                   endif
032300050323     C                   endif
032400050323     C                   endif
032500050323     C*
032600050323     C                   ENDSR
032700040723
032800040723
032900040723
033000040723     C*------------------------------------------------------------------------*
033100040723     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
033200040723     C*------------------------------------------------------------------------*
033300040723     C     WRIORI        BEGSR
033400040723     C*
033500090617     C* Come prima cosa stacco un numeratore da AZNUM
033600090617     C                   clear                   trul33ds
033700090617     C                   eval      I33OPE = *zeros
033800090617     C                   eval      I33CNU = 600
033900090617     C                   eval      I33NUM = 1
034000090617     C                   movel     TRUL33DS      KPJBU
034100090617     C                   call      'TRUL33R'
034200090617     C                   parm                    KPJBA
034300090617     C                   movel     KPJBU         TRUL33DS
034400090617     C                   if        O33ERR = *zeros
034500090617     C                   z-add     O33NRF        §numori
034600090618     c                   else
034700090618     c                   eval      §numori = *all'9'
034800090617     C                   endif
034900040723     C*
035000040723     C* Quindi imposto i campi "extra-VAO"
035100040723     C                   if        vlrKSC = '0ORM0000'
035200040723     C*
035300040723     C                   if        vaoCOR > *zeros
035400040723     C                   movel     vaoCOR        wrkCOR7           7 0
035500040723     C*
035600040723     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
035700040723     C* codice padre. Se è un padre assegno già il codice ordinante.
035800040723     C                   clear                   tibs10ds
035900040723     C                   eval      d10drf = datcor                              *data riferimento
036000040723     C                   eval      d10paf = 'P'                                 *cerca il padre
036100040723     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
036200091026     C*
036300091026     C* Preparazione chiamata al TIBS10R1
036400091026     C                   clear                   tibs10r1ds
036500091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
036600091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
036700091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
036800091026     C*
036900091026     C                   call      'TIBS10R1'
037000091026     C                   parm                    tibs10r1ds
037100091026     C*
037200091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
037300040723     C*
037400040723     C                   if        d10err <> *blanks or                         *No unif. o padre
037500040723     C                             d10cop =  *zeros  or
037600040723     C                             d10cop =  d10cod
037700040723     C                   move      d10cod        oriIDC
037800040723     C                   else                                                   *Figlio
037900040723     C                   move      d10cop        oriIDC
038000040723     C                   endif
038100040723     C*
038200040723     C                   else
038300040723     C                   eval      oriIDC = *zeros
038400040723     C                   endif
038500040723     C*
038600040723     C                   else
038700040723     C                   eval      oriIDC = vlrKSC
038800040723     C                   endif
038900040723     C*
039000040723     C                   eval      oriPRG = §numori
039100040723     C                   eval      oriDER = datcor
039200040723     C                   eval      oriOER = wn6
039300040723     C                   eval      oriPOE = vaoPOE
039400040723     C                   eval      oriNSR = vaoNSR
039500040723     C                   eval      oriNOR = vaoNOR
039600040723     C                   eval      oriNRV = vaoNRV
039700040723     C                   eval      oriFLO = *blanks
039800040723     C*
039900040723     C* Infine valorizzo la chiave esterna sul file FNVAO
040000040723     C                   eval      dorm01 = vaoFLO
040100040723     C                   movel     §numori       §ormpg
040200040723     C                   eval      vaoFLO = dorm01
040300040723     C*
040400040723     C* ...quindi scrivo il file TIORI00F
040500131008     C  N55              WRITE     TIORI000
040600040723     C*
040700040723     C                   ENDSR
040800050826
040900050826
041000050826
041100050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
041200050826     C     wriVGD        BEGSR
041300050826     C*
041400050826     C* Reperisco la descrizione della fase da tabella
041500050826     C                   movel(p)  'FAR'         KteCOD
041600050826     C                   movel(p)  '000'         KteKE1
041700050826     C     KEYtbe        chain     tntbe01l
041800050826     C                   if        %found(tntbe01l)
041900050826     C                   movel(P)  tbeuni        DFAR
042000050826     C                   else
042100050826     C                   clear                   DFAR
042200050826     C                   endif
042300050826     C*
042400050826     C* Valorizzo buffer record
042500050826     C                   clear                   FNVAPDS
042600050826     C                   eval      vapIDC = oriIDC
042700050826     C                   eval      vapPOE = oriPOE
042800050826     C                   eval      vapRFA = vaoRFA
042900050826     C                   eval      vapPOG = oriPOE
043000050826     C                   eval      vapDAE = oriDER
043100050826     C                   eval      vapORE = oriOER
043200050826     C                   if        d§fardva <> *blanks
043300050826     C                   eval      vapDFA = d§fardva
043400050826     C                   else
043500050826     C                   eval      vapDFA = d§fardes
043600050826     C                   endif
043700050826     C*
043800050826     C                   clear                   tivgd000
043900080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
044000050826     C                   eval      vgdTIP = 'VP'
044100050826     C                   eval      vgdKSU = vapIDC
044200050826     C                   eval      vgdTSC = 'WW'
044300050826     C                   eval      vgdDAT = datcor
044400050826     C                   eval      vgdPGM = 'TITVOXLR'
044500060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
044600060306     C                   exsr      sr_Control
044700060306     C                   if        Cli_Abil = *on
044800060306     C                   eval      vgdKSU = woriIDC
044900131008     C  N55              write     tivgd000
045000060306     C                   endif
045100050826     C*
045200050826     C                   ENDSR
045300050826     C*------------------------------------------------------------------------*
045400010601
045500010601
045600010601
045700010330     C*----------------------------------------------------*
045800030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
045900010330     C*----------------------------------------------------*
046000010330     C     INZVAR        BEGSR
046100010330     C*
046200030822     C* Inizializzo variabili di wrk
046300020204     C                   Z-ADD     *zeros        Num5_0            5 0
046400090403     C                   z-add     *zeros        invfil
046500030822     C*
046600030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
046700040708     C                   CLEAR                   FNVAO000
046800040723     C                   CLEAR                   TIORI000
046900030822     C                   CLEAR                   SkSplitCSV
047000030822     C                   EVAL      i = 1
047100030822     C                   EVAL      posDa = *zeros
047200030822     C                   EVAL      posA  = *zeros
047300030822     C*
047400030822     C* Reimposto i valori di default
047500030822     C                   EXSR      DEFCAM
047600030822     C*
047700010330     C                   ENDSR
047800010330     C*----------------------------------------------------*
047900030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
048000010330     C*----------------------------------------------------*
048100010330     C     DEFCAM        BEGSR
048200030822     C*
048300030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
048400030715     C* e delimitatore testo.
048500030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
048600030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
048700030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
048800090403     C*
048900090403     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
049000090403     C                   EVAL      CharSOS = CharNUM
049100030822     C*
049200030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
049300030715     C                   IF        %subst(vlrppt:1:1) = 'S'
049400030715     C                   SETON                                        50
049500030715     C                   ELSE
049600030715     C                   SETOFF                                       50
049700131008     C                   ENDIF
049800131008     C* aggiungo la gestyione dei test (tanto la stamapa non c'è più)
049900131008     C                   SETOFF                                       55
050000131008     C                   IF        %subst(vlrppt:1:1) = 'T'
050100131008     C* inibisco la scrittura dei file TIORI e TIVGD
050200131008     C                   SETON                                        55
050300131008     C* imposto il PO di emissione come filiale fittizia
050400131008     C                   EVAL      VAOPOE = 999
050500131008     C                   ENDIF
050600030822     C*
050700030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
050800030822     C                   EVAL      posDaDft = 1
050900030822     C                   EVAL      posADft  = 0
051000030822     C                   EVAL      wGiroDft = 0
051100030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
051200030822     C                             posDaDft > 0
051300030822     C*
051400030822     C* Gestisco il 1° giro
051500030822     C                   IF        wGiroDft = 0
051600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
051700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
051800030822     C* Incremento il contatore dei "giri"
051900030822     C                   EVAL      wGiroDft = 1
052000030822     C                   ELSE
052100030822     C                   EVAL      posDaDft = posADft
052200030822     C                   ENDIF
052300030822     C* Eseguo lo scan x trovare la fine del campo corrente
052400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
052500030822     C*
052600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
052700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
052800030822     C                   IF        posDaDft > 0 AND
052900030822     C                             posADft  > 0
053000040708     C* POE
053100040708     C                   IF        %subst(
053200040708     C                             %subst(vlrppt:posDaDft+1:
053300040708     C                             posADft-posDaDft-1):1:3)
053400040708     C                             = 'POE'
053500040708     C                   EVAL      PiStr=%trim(%subst(
053600040708     C                             %subst(vlrppt:posDaDft+1:
053700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053800040708     C                   EXSR      CHKNUM
053900040708     C                   IF        PiInt=*on
054000040708     C                   Z-ADD     PiVal         VAOPOE
054100040708     C                   ENDIF
054200040708     C                   ENDIF
054300040708     C* TOR
054400040708     C                   IF        %subst(
054500040708     C                             %subst(vlrppt:posDaDft+1:
054600040708     C                             posADft-posDaDft-1):1:3)
054700040708     C                             = 'TOR'
054800040708     C                   EVAL      VAOTOR=%trim(%subst(
054900040708     C                             %subst(vlrppt:posDaDft+1:
055000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055100040708     C                   ENDIF
055200040708     C* TCO
055300040708     C                   IF        %subst(
055400040708     C                             %subst(vlrppt:posDaDft+1:
055500040708     C                             posADft-posDaDft-1):1:3)
055600040708     C                             = 'TCO'
055700040708     C                   EVAL      VAOTCO=%trim(%subst(
055800040708     C                             %subst(vlrppt:posDaDft+1:
055900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056000040708     C                   ENDIF
056100040707     C* COR
056200030822     C                   IF        %subst(
056300030822     C                             %subst(vlrppt:posDaDft+1:
056400030822     C                             posADft-posDaDft-1):1:3)
056500040707     C                             = 'COR'
056600030822     C                   EVAL      PiStr=%trim(%subst(
056700030822     C                             %subst(vlrppt:posDaDft+1:
056800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056900030822     C                   EXSR      CHKNUM
057000030822     C                   IF        PiInt=*on
057100040708     C                   Z-ADD     PiVal         VAOCOR
057200030822     C                   ENDIF
057300030822     C                   ENDIF
057400040708     C* CRC
057500040708     C                   IF        %subst(
057600040708     C                             %subst(vlrppt:posDaDft+1:
057700040708     C                             posADft-posDaDft-1):1:3)
057800040708     C                             = 'CRC'
057900040708     C                   EVAL      PiStr=%trim(%subst(
058000040708     C                             %subst(vlrppt:posDaDft+1:
058100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058200040708     C                   EXSR      CHKNUM
058300040708     C                   IF        PiInt=*on
058400040708     C                   Z-ADD     PiVal         VAOCRC
058500040708     C                   ENDIF
058600040708     C                   ENDIF
058700040708     C* FFD
058800040708     C                   IF        %subst(
058900040708     C                             %subst(vlrppt:posDaDft+1:
059000040708     C                             posADft-posDaDft-1):1:3)
059100040708     C                             = 'FFD'
059200040708     C                   EVAL      VAOFFD=%trim(%subst(
059300040708     C                             %subst(vlrppt:posDaDft+1:
059400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059500040708     C                   ENDIF
059600040708     C* ORR
059700040708     C                   IF        %subst(
059800040708     C                             %subst(vlrppt:posDaDft+1:
059900040708     C                             posADft-posDaDft-1):1:3)
060000040708     C                             = 'ORR'
060100040708     C                   EVAL      PiStr=%trim(%subst(
060200040708     C                             %subst(vlrppt:posDaDft+1:
060300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060400040708     C                   EXSR      CHKNUM
060500040708     C                   IF        PiInt=*on
060600040708     C                   Z-ADD     PiVal         VAOORR
060700040708     C                   ENDIF
060800040708     C                   ENDIF
060900040712     C* NCL
061000040712     C                   IF        %subst(
061100040712     C                             %subst(vlrppt:posDaDft+1:
061200040712     C                             posADft-posDaDft-1):1:3)
061300040712     C                             = 'NCL'
061400040712     C                   EVAL      PiStr=%trim(%subst(
061500040712     C                             %subst(vlrppt:posDaDft+1:
061600040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061700040712     C                   EXSR      CHKNUM
061800040712     C                   IF        PiInt=*on
061900040712     C                   Z-ADD     PiVal         VAONCL
062000040712     C                   ENDIF
062100040712     C                   ENDIF
062200040712     C* PKG
062300040712     C                   IF        %subst(
062400040712     C                             %subst(vlrppt:posDaDft+1:
062500040712     C                             posADft-posDaDft-1):1:3)
062600040712     C                             = 'PKG'
062700040712     C                   EVAL      PiStr=%trim(%subst(
062800040712     C                             %subst(vlrppt:posDaDft+1:
062900040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063000040712     C                   EXSR      CHKNUM
063100040712     C                   IF        PiNum=*on
063200040712     C                   Z-ADD     PiVal         VAOPKG
063300040712     C                   ENDIF
063400040712     C                   ENDIF
063500040712     C* VLM
063600040712     C                   IF        %subst(
063700040712     C                             %subst(vlrppt:posDaDft+1:
063800040712     C                             posADft-posDaDft-1):1:3)
063900040712     C                             = 'VLM'
064000040712     C                   EVAL      PiStr=%trim(%subst(
064100040712     C                             %subst(vlrppt:posDaDft+1:
064200040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064300040712     C                   EXSR      CHKNUM
064400040712     C                   IF        PiNum=*on
064500040712     C                   Z-ADD     PiVal         VAOVLM
064600040712     C                   ENDIF
064700040712     C                   ENDIF
064800040708     C* NAM
064900040708     C                   IF        %subst(
065000040708     C                             %subst(vlrppt:posDaDft+1:
065100040708     C                             posADft-posDaDft-1):1:3)
065200040708     C                             = 'NAM'
065300040708     C                   EVAL      VAONAM=%trim(%subst(
065400040708     C                             %subst(vlrppt:posDaDft+1:
065500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065600040708     C                   ENDIF
065700040708     C* PAG
065800030822     C                   IF        %subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):1:3)
066100040708     C                             = 'PAG'
066200040708     C                   EVAL      VAOPAG=%trim(%subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066500030822     C                   ENDIF
066600040708     C* KSC
066700040708     C                   IF        %subst(
066800040708     C                             %subst(vlrppt:posDaDft+1:
066900040708     C                             posADft-posDaDft-1):1:3)
067000040708     C                             = 'KSC'
067100040708     C                   EVAL      PiStr=%trim(%subst(
067200040708     C                             %subst(vlrppt:posDaDft+1:
067300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067400040708     C                   EXSR      CHKNUM
067500040708     C                   IF        PiInt=*on
067600040708     C                   Z-ADD     PiVal         VAOKSC
067700040708     C                   ENDIF
067800040708     C                   ENDIF
067900040708     C* CTR
068000040708     C                   IF        %subst(
068100040708     C                             %subst(vlrppt:posDaDft+1:
068200040708     C                             posADft-posDaDft-1):1:3)
068300040708     C                             = 'CTR'
068400040708     C                   EVAL      VAOCTR=%trim(%subst(
068500040708     C                             %subst(vlrppt:posDaDft+1:
068600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068700040708     C                   ENDIF
068800040708     C* POC
068900040708     C                   IF        %subst(
069000040708     C                             %subst(vlrppt:posDaDft+1:
069100040708     C                             posADft-posDaDft-1):1:3)
069200040708     C                             = 'POC'
069300040708     C                   EVAL      PiStr=%trim(%subst(
069400040708     C                             %subst(vlrppt:posDaDft+1:
069500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069600040708     C                   EXSR      CHKNUM
069700040708     C                   IF        PiInt=*on
069800040708     C                   Z-ADD     PiVal         VAOPOC
069900040708     C                   ENDIF
070000040708     C                   ENDIF
070100040708     C* NO1
070200040708     C                   IF        %subst(
070300040708     C                             %subst(vlrppt:posDaDft+1:
070400040708     C                             posADft-posDaDft-1):1:3)
070500040708     C                             = 'NO1'
070600040708     C                   EVAL      VAONO1=%trim(%subst(
070700040708     C                             %subst(vlrppt:posDaDft+1:
070800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070900040708     C                   ENDIF
071000040708     C* DDT
071100030822     C                   IF        %subst(
071200030822     C                             %subst(vlrppt:posDaDft+1:
071300030822     C                             posADft-posDaDft-1):1:3)
071400040708     C                             = 'DDT'
071500040708     C                   EVAL      VAODDT=%trim(%subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071800030822     C                   ENDIF
071900040708     C* RFA
072000040708     C                   IF        %subst(
072100040708     C                             %subst(vlrppt:posDaDft+1:
072200040708     C                             posADft-posDaDft-1):1:3)
072300040708     C                             = 'RFA'
072400040708     C                   EVAL      VAORFA=%trim(%subst(
072500040708     C                             %subst(vlrppt:posDaDft+1:
072600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072700040708     C                   ENDIF
072800040708     C* FLO
072900040708     C                   IF        %subst(
073000040708     C                             %subst(vlrppt:posDaDft+1:
073100040708     C                             posADft-posDaDft-1):1:3)
073200040708     C                             = 'FLO'
073300040708     C                   EVAL      VAOFLO=%trim(%subst(
073400040708     C                             %subst(vlrppt:posDaDft+1:
073500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073600040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
073700040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
073800040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
073900040708     C                   ENDIF
074000030822     C* ...
074100030822     C                   ENDIF
074200030822     C                   ENDDO
074300020204     C*
074400010330     C                   ENDSR
074500010607     C*----------------------------------------------------*
074600040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
074700010607     C*----------------------------------------------------*
074800040708     C     IMPVAO        BEGSR
074900010607     C*
075000010607     C                   SETOFF                                       3132
075100030515     C*
075200040708     C                   MOVEL     datcor        VAODAO
075300040708     C                   MOVEL     wn14          VAOOAO
075400010607     C*
075500030715     C**********
075600030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
075700030715     C**********
075800030715     C                   IF        wGiro = *zeros
075900031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
076000031201     C     minu:maiu     XLATE     vindta        vindta
076100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
076200040324     C                   DOW       posDa <= %len(%trimr(vindta))
076300030715     C*
076400030715     C* Gestisco il 1° campo
076500030715     C                   IF        i = 1
076600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
076700030715     C                   EVAL      posDa = 1
076800030715     C* Eseguo lo scan x trovare la fine del primo campo
076900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
077000030715     C* A questo punto "estraggo" il campo corrente
077100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
077200030715     C                                                    (posA-posDa))
077300030715     C* X i campi successivi al 1°
077400030715     C                   ELSE
077500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
077600030715     C                   EVAL      posDa = posA + 1
077700030715     C* Eseguo lo scan x trovare la fine del campo corrente
077800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
077900030715     C* Gestisco l'ultimo campo
078000030715     C                   IF        posA = *zeros
078100040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
078200030715     C* A questo punto "estraggo" il campo corrente
078300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
078400030715     C                                                    (posA-posDa))
078500030715     C                   LEAVE
078600030715     C                   ELSE
078700030715     C* A questo punto "estraggo" il campo corrente
078800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
078900030715     C                                                    (posA-posDa))
079000030715     C                   ENDIF
079100030715     C                   ENDIF
079200030715     C* Incremento il contatore d campo
079300030715     C                   EVAL      i = i +1
079400030715     C                   ENDDO
079500030715     C*
079600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
079700030715     C* delimitazione testo
079800030715     C                   EVAL      i = 1
079900030715     C                   DOW       i < %elem(SkSplitFLD)
080000030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
080100030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
080200030715     C                   EVAL      i = i + 1
080300030715     C                   ENDDO
080400030715     C                   EVAL      wGiro = 1
080500030715     C                   ELSE
080600030715     C                   EVAL      wGiro = 2
080700031204     C**********
080800031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
080900031204     C* d separatore campo
081000031204     C**********
081100040304     C                   IF        %subst(vindta:1:1) = CharCSV                 * all'inizio
081200040304     C                   EVAL      vindta = ' ' + vindta
081300040304     C                   ENDIF
081400040304     C***                DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
081500040304     C***                EVAL      vindta = %subst(vindta:2)
081600040304     C***                ENDDO
081700031204     C*
081800031204     C                   Z-ADD     *zeros        lunghInput        4 0
081900040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
082000040324     C                   IF        %subst(%trimr(vindta):lunghInput:1) = CharCSV
082100040304     C                   EVAL      vindta = vindta + ' '
082200040304     C                   ENDIF
082300040324     C***                DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
082400040324     C***                EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
082500040304     C***                EVAL      lunghInput = %len(%trim(vindta))
082600040304     C***                ENDDO
082700030715     C**********
082800030715     C* Effettuo lo split del campo dati d input
082900030715     C**********
083000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
083100040324     C                   DOW       posDa <= %len(%trimr(vindta))
083200030715     C*
083300030715     C* Gestisco il 1° campo
083400030715     C                   IF        i = 1
083500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
083600030715     C                   EVAL      posDa = 1
083700030715     C* Eseguo lo scan x trovare la fine del primo campo
083800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083900030715     C* A questo punto "estraggo" il campo corrente
084000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
084100030715     C                                                    (posA-posDa))
084200030715     C* X i campi successivi al 1°
084300030715     C                   ELSE
084400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
084500030715     C                   EVAL      posDa = posA + 1
084600031204     C* Verifico che nn vi sia il campo nn valorizzato
084700031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
084800031204     C* Se campo nn valorizzato skippo al prossimo
084900031204     C                   EVAL      SkSplitCSV(i) = *blanks
085000031204     C                   EVAL      posA  = posA + 1
085100031204     C                   ELSE
085200030715     C* Eseguo lo scan x trovare la fine del campo corrente
085300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085400030715     C* Gestisco l'ultimo campo
085500030715     C                   IF        posA = *zeros
085600040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
085700030715     C* A questo punto "estraggo" il campo corrente
085800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
085900030715     C                                                    (posA-posDa))
086000030715     C                   LEAVE
086100030715     C                   ELSE
086200030715     C* A questo punto "estraggo" il campo corrente
086300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
086400030715     C                                                    (posA-posDa))
086500030715     C                   ENDIF
086600031204     C                   ENDIF
086700030715     C                   ENDIF
086800030715     C* Incremento il contatore d campo
086900030715     C                   EVAL      i = i +1
087000030715     C                   ENDDO
087100030715     C*
087200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
087300030715     C* delimitazione testo
087400030715     C                   EVAL      i = 1
087500030715     C                   DOW       i < %elem(SkSplitCSV)
087600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
087700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
087800030715     C                   EVAL      i = i + 1
087900030715     C                   ENDDO
088000030715     C*
088100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
088200030715     C                   EVAL      i = 1
088300030715     C                   DOW       i < %elem(SkSplitFLD)
088400030715     C*
088500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
088600030715     C                   IF        SkSplitFLD(i) <> *blanks AND
088700030715     C                             SkSplitCSV(i) <> *blanks
088800030721     C*** ATB
088900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATB'
089000040708     C                   EVAL      VAOATB = SkSplitCSV(i)
089100030721     C                   ENDIF
089200040708     C*** TOR
089300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTOR'
089400040708     C                   EVAL      VAOTOR = SkSplitCSV(i)
089500040708     C                   ENDIF
089600040708     C*** TCO
089700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTCO'
089800040708     C                   EVAL      VAOTCO = SkSplitCSV(i)
089900040708     C                   ENDIF
090000040708     C*** RSO
090100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSO'
090200040708     C                   EVAL      VAORSO = SkSplitCSV(i)
090300030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
090400040708     C     '@':'A'       XLATE     VAORSO        VAORSO
090500030715     C* ==
090600030715     C                   ENDIF
090700040708     C*** INO
090800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINO'
090900040708     C                   EVAL      VAOINO = SkSplitCSV(i)
091000030715     C                   ENDIF
091100040708     C*** LOO
091200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOO'
091300040708     C                   EVAL      VAOLOO = SkSplitCSV(i)
091400030715     C                   ENDIF
091500040708     C*** PRO
091600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRO'
091700040708     C                   EVAL      VAOPRO = SkSplitCSV(i)
091800030715     C                   ENDIF
091900040708     C*** NAO
092000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAO'
092100040708     C                   EVAL      VAONAO = SkSplitCSV(i)
092200140124     C                   IF        VAONAO = 'I' or
092300140124     C                             VAONAO = 'IT'
092400140124     C                   EVAL      VAONAO = *blank
092500030721     C                   ENDIF
092600140124     C                   ENDIF
092700040708     C*** RSR
092800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSR'
092900040708     C                   EVAL      VAORSR = SkSplitCSV(i)
093000040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
093100040708     C     '@':'A'       XLATE     VAORSR        VAORSR
093200040708     C* ==
093300040708     C                   IF        VAORSR = *blanks
093400040708     C                   SETON                                        31
093500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
093600040708     C                             + ' ' + 'VAORSR'
093700040708     C                   ENDIF
093800040708     C                   ENDIF
093900040708     C*** INR
094000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINR'
094100040708     C                   EVAL      VAOINR = SkSplitCSV(i)
094200040708     C                   IF        VAOINR = *blanks
094300040708     C                   SETON                                        31
094400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
094500040708     C                             + ' ' + 'VAOINR'
094600040708     C                   ENDIF
094700040708     C                   ENDIF
094800040708     C*** LOR
094900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOR'
095000040708     C                   EVAL      VAOLOR = SkSplitCSV(i)
095100040708     C                   IF        VAOLOR = *blanks
095200040708     C                   SETON                                        31
095300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
095400040708     C                             + ' ' + 'VAOLOR'
095500040708     C                   ENDIF
095600040708     C                   ENDIF
095700040708     C*** PRR
095800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRR'
095900040708     C                   EVAL      VAOPRR = SkSplitCSV(i)
096000040708     C                   IF        VAOPRR = *blanks
096100040708     C                   SETON                                        31
096200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
096300040708     C                             + ' ' + 'VAOPRR'
096400040708     C                   ENDIF
096500040708     C                   ENDIF
096600040708     C*** NAR
096700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAR'
096800040708     C                   EVAL      VAONAR = SkSplitCSV(i)
096900140124     C                   IF        VAONAR = 'I' or
097000140124     C                             VAONAR = 'IT'
097100140124     C                   EVAL      VAONAR = *blank
097200140124     C                   ENDIF
097300040708     C                   ENDIF
097400040708     C*** RER
097500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORER'
097600040708     C                   EVAL      VAORER = SkSplitCSV(i)
097700040708     C                   ENDIF
097800040708     C*** TER
097900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTER'
098000040708     C                   EVAL      VAOTER = SkSplitCSV(i)
098100040708     C                   ENDIF
098200040708     C*** RSC
098300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSC'
098400040708     C                   EVAL      VAORSC = SkSplitCSV(i)
098500040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
098600040708     C     '@':'A'       XLATE     VAORSC        VAORSC
098700040708     C* ==
098800040708     C                   ENDIF
098900040708     C*** INC
099000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINC'
099100040708     C                   EVAL      VAOINC = SkSplitCSV(i)
099200040708     C                   ENDIF
099300040708     C*** LOC
099400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOC'
099500040708     C                   EVAL      VAOLOC = SkSplitCSV(i)
099600040708     C                   ENDIF
099700040708     C*** PRC
099800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRC'
099900040708     C                   EVAL      VAOPRC = SkSplitCSV(i)
100000040708     C                   ENDIF
100100040708     C*** NAC
100200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAC'
100300040708     C                   EVAL      VAONAC = SkSplitCSV(i)
100400140124     C                   IF        VAONAC = 'I' or
100500140124     C                             VAONAC = 'IT'
100600140124     C                   EVAL      VAONAC = *blank
100700140124     C                   ENDIF
100800040708     C                   ENDIF
100900040708     C*** FFD
101000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFFD'
101100040708     C                   EVAL      VAOFFD = SkSplitCSV(i)
101200040708     C                   ENDIF
101300040708     C*** RMP
101400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORMP'
101500040708     C                   EVAL      VAORMP = SkSplitCSV(i)
101600040708     C                   ENDIF
101700040708     C*** NAM
101800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAM'
101900040708     C                   EVAL      VAONAM = SkSplitCSV(i)
102000040708     C                   ENDIF
102100040708     C*** PAG
102200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPAG'
102300040708     C                   EVAL      VAOPAG = SkSplitCSV(i)
102400040708     C                   ENDIF
102500040708     C*** CTR
102600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCTR'
102700040708     C                   EVAL      VAOCTR = SkSplitCSV(i)
102800040708     C                   ENDIF
102900040708     C*** ZOR
103000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOZOR'
103100040708     C                   EVAL      VAOZOR = SkSplitCSV(i)
103200040708     C                   ENDIF
103300040708     C*** NO1
103400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO1'
103500040708     C                   EVAL      VAONO1 = SkSplitCSV(i)
103600030715     C                   ENDIF
103700040708     C*** NO2
103800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO2'
103900040708     C                   EVAL      VAONO2 = SkSplitCSV(i)
104000030715     C                   ENDIF
104100040708     C*** DDT
104200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODDT'
104300040708     C                   EVAL      VAODDT = SkSplitCSV(i)
104400030715     C                   ENDIF
104500040708     C*** RFA
104600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORFA'
104700040708     C                   EVAL      VAORFA = SkSplitCSV(i)
104800030715     C                   ENDIF
104900040708     C*** SPI
105000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSPI'
105100040708     C                   EVAL      VAOSPI = SkSplitCSV(i)
105200030715     C                   ENDIF
105300040708     C*** FLO
105400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
105500140124     C                   EVAL      VAOFLO = %trim(SkSplitCSV(i))
105600140124     C* sostituisco ai valori '_' (underscore) il carattere ' ' (*blank)
105700140124     C                   EVAL      VAOFLO = %xlate('_' : ' ' : VAOFLO)
105800030715     C                   ENDIF
105900030715     C
106000030721     C*
106100030721     C* Reperisco quindi i campi numerici...
106200040708     C*** POE
106300040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
106400040723     C                   EVAL      PiStr=SkSplitCSV(i)
106500040723     C                   EXSR      CHKNUM
106600040723     C                   IF        PiInt=*on    AND
106700040723     C                             PiVal<=999   AND
106800040723     C                             PiVal>*zeros
106900040723     C                   Z-ADD     PiVal         VAOPOE
107000040723     C                   ELSE
107100040723     C                   SETON                                        32
107200040723     C                   EVAL      VAOPOE = *zeros
107300040723     C                   EVAL      vinmsg = %trimr(vinmsg)
107400040723     C                             + ' ' + 'VAOPOE'
107500040723     C                   ENDIF
107600040723     C                   ENDIF
107700040708     C*** COR
107800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
107900040708     C                   EVAL      PiStr=SkSplitCSV(i)
108000040708     C                   EXSR      CHKNUM
108100040708     C                   IF        PiInt=*on
108200140124     C                   EVAL      VAOCOR = PiVal * 1000
108300040708     C                   ELSE
108400040708     C                   SETON                                        31
108500040708     C                   EVAL      VAOCOR = *zeros
108600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
108700040708     C                             + ' ' + 'VAOCOR'
108800040708     C                   ENDIF
108900040708     C                   ENDIF
109000040708     C*** CAO
109100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
109200130502     C                   EVAL      VAOCAO = SkSplitCSV(i)
109300130502     C***                EVAL      PiStr=SkSplitCSV(i)
109400130502     C***                EXSR      CHKNUM
109500130502     C***                IF        PiInt=*on
109600130502     C***                Z-ADD     PiVal         Num5_0
109700130502     C***                MOVEL(p)  Num5_0        VAOCAO
109800130502     C***                ELSE
109900130502     C***                SETON                                        32
110000130502     C***                EVAL      VAOCAO = *zeros
110100130502     C***                EVAL      vinmsg = %trimr(vinmsg)
110200130502     C***                          + ' ' + 'VAOCAO'
110300130502     C***                ENDIF
110400030721     C                   ENDIF
110500040708     C*** CRA
110600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
110700030721     C                   EVAL      PiStr=SkSplitCSV(i)
110800030721     C                   EXSR      CHKNUM
110900030721     C                   IF        PiInt=*on
111000040708     C                   Z-ADD     PiVal         VAOCRA
111100030721     C                   ELSE
111200030721     C                   SETON                                        32
111300040708     C                   EVAL      VAOCRA = *zeros
111400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111500040708     C                             + ' ' + 'VAOCRA'
111600030721     C                   ENDIF
111700030721     C                   ENDIF
111800040708     C*** CAR
111900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
112000130502     C                   EVAL      VAOCAR = SkSplitCSV(i)
112100130502     C***                EVAL      PiStr=SkSplitCSV(i)
112200130502     C***                EXSR      CHKNUM
112300130502     C***                IF        PiInt=*on
112400130502     C***                Z-ADD     PiVal         Num5_0
112500130502     C***                MOVEL(p)  Num5_0        VAOCAR
112600130502     C***                ELSE
112700130502     C***                SETON                                        31
112800130502     C***                EVAL      VAOCAR = *zeros
112900130502     C***                EVAL      vinmsg = %trimr(vinmsg)
113000130502     C***                          + ' ' + 'VAOCAR'
113100130502     C***                ENDIF
113200040708     C                   ENDIF
113300040708     C*** CRC
113400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
113500040708     C                   EVAL      PiStr=SkSplitCSV(i)
113600040708     C                   EXSR      CHKNUM
113700040708     C                   IF        PiInt=*on
113800040708     C                   Z-ADD     PiVal         VAOCRC
113900040708     C                   ELSE
114000040708     C                   SETON                                        32
114100040708     C                   EVAL      VAOCRC = *zeros
114200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
114300040708     C                             + ' ' + 'VAOCRC'
114400040708     C                   ENDIF
114500040708     C                   ENDIF
114600040708     C*** CAC
114700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
114800130502     C                   EVAL      VAOCAC = SkSplitCSV(i)
114900130502     C***                EVAL      PiStr=SkSplitCSV(i)
115000130502     C***                EXSR      CHKNUM
115100130502     C***                IF        PiInt=*on
115200130502     C***                Z-ADD     PiVal         Num5_0
115300130502     C***                MOVEL(p)  Num5_0        VAOCAC
115400130502     C***                ELSE
115500130502     C***                SETON                                        32
115600130502     C***                EVAL      VAOCAC = *zeros
115700130502     C***                EVAL      vinmsg = %trimr(vinmsg)
115800130502     C***                          + ' ' + 'VAOCAC'
115900130502     C***                ENDIF
116000040708     C                   ENDIF
116100040708     C*** DAR
116200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
116300040708     C                   EVAL      PiStr=SkSplitCSV(i)
116400040708     C                   EXSR      CHKNUM
116500040708     C                   IF        PiInt=*on
116600040708     C                   Z-ADD     PiVal         VAODAR
116700040708     C                   ELSE
116800040708     C                   SETON                                        32
116900040708     C                   EVAL      VAODAR = *zeros
117000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
117100040708     C                             + ' ' + 'VAODAR'
117200040708     C                   ENDIF
117300040708     C                   ENDIF
117400040708     C*** ORR
117500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
117600040708     C                   EVAL      PiStr=SkSplitCSV(i)
117700040708     C                   EXSR      CHKNUM
117800040708     C                   IF        PiInt=*on
117900040708     C                   Z-ADD     PiVal         VAOORR
118000040708     C                   ELSE
118100040708     C                   SETON                                        32
118200040708     C                   EVAL      VAOORR = *zeros
118300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
118400040708     C                             + ' ' + 'VAOORR'
118500040708     C                   ENDIF
118600040708     C                   ENDIF
118700030721     C*** NCL
118800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
118900030721     C                   EVAL      PiStr=SkSplitCSV(i)
119000030721     C                   EXSR      CHKNUM
119100030721     C                   IF        PiInt=*on
119200040708     C                   Z-ADD     PiVal         VAONCL
119300030721     C                   ELSE
119400030721     C                   SETON                                        32
119500040708     C                   EVAL      VAONCL = *zeros
119600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119700040708     C                             + ' ' + 'VAONCL'
119800030721     C                   ENDIF
119900030721     C                   ENDIF
120000040708     C*** PKG
120100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
120200030721     C                   EVAL      PiStr=SkSplitCSV(i)
120300030721     C                   EXSR      CHKNUM
120400030721     C                   IF        PiNum=*on
120500040708     C                   Z-ADD     PiVal         VAOPKG
120600030721     C                   ELSE
120700030721     C                   SETON                                        32
120800040708     C                   EVAL      VAOPKG = *zeros
120900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121000040708     C                             + ' ' + 'VAOPKG'
121100030721     C                   ENDIF
121200030721     C                   ENDIF
121300040708     C*** VLM
121400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
121500040708     C                   EVAL      PiStr=SkSplitCSV(i)
121600040708     C                   EXSR      CHKNUM
121700040708     C                   IF        PiNum=*on
121800040708     C                   Z-ADD     PiVal         VAOVLM
121900040708     C                   ELSE
122000040708     C                   SETON                                        32
122100040708     C                   EVAL      VAOVLM = *zeros
122200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
122300040708     C                             + ' ' + 'VAOVLM'
122400040708     C                   ENDIF
122500040708     C                   ENDIF
122600040708     C*** BNC
122700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
122800040708     C                   EVAL      PiStr=SkSplitCSV(i)
122900040708     C                   EXSR      CHKNUM
123000040708     C                   IF        PiInt=*on
123100040708     C                   Z-ADD     PiVal         VAOBNC
123200040708     C                   ELSE
123300040708     C                   SETON                                        32
123400040708     C                   EVAL      VAOBNC = *zeros
123500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
123600040708     C                             + ' ' + 'VAOBNC'
123700040708     C                   ENDIF
123800040708     C                   ENDIF
123900040708     C*** BLC
124000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
124100040708     C                   EVAL      PiStr=SkSplitCSV(i)
124200040708     C                   EXSR      CHKNUM
124300040708     C                   IF        PiInt=*on
124400040708     C                   Z-ADD     PiVal         VAOBLC
124500040708     C                   ELSE
124600040708     C                   SETON                                        32
124700040708     C                   EVAL      VAOBLC = *zeros
124800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
124900040708     C                             + ' ' + 'VAOBLC'
125000040708     C                   ENDIF
125100040708     C                   ENDIF
125200040708     C*** ATT
125300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
125400040708     C                   EVAL      PiStr=SkSplitCSV(i)
125500040708     C                   EXSR      CHKNUM
125600040708     C                   IF        PiInt=*on
125700040708     C                   Z-ADD     PiVal         VAOATT
125800040708     C                   ELSE
125900040708     C                   SETON                                        32
126000040708     C                   EVAL      VAOATT = *zeros
126100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
126200040708     C                             + ' ' + 'VAOATT'
126300040708     C                   ENDIF
126400040708     C                   ENDIF
126500040708     C*** MTC
126600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
126700040708     C                   EVAL      PiStr=SkSplitCSV(i)
126800040708     C                   EXSR      CHKNUM
126900040708     C                   IF        PiInt=*on
127000040708     C                   Z-ADD     PiVal         VAOMTC
127100040708     C                   ELSE
127200040708     C                   SETON                                        32
127300040708     C                   EVAL      VAOMTC = *zeros
127400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
127500040708     C                             + ' ' + 'VAOMTC'
127600040708     C                   ENDIF
127700040708     C                   ENDIF
127800040708     C*** KSC
127900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
128000030721     C                   EVAL      PiStr=SkSplitCSV(i)
128100030721     C                   EXSR      CHKNUM
128200030721     C                   IF        PiInt=*on      AND
128300030721     C                             PiVal<=9999999 AND
128400030721     C                             PiVal>*zeros
128500040708     C                   Z-ADD     PiVal         VAOKSC
128600030721     C                   ELSE
128700030721     C                   SETON                                        32
128800040708     C                   EVAL      VAOKSC = *zeros
128900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129000040708     C                             + ' ' + 'VAOKSC'
129100030721     C                   ENDIF
129200030721     C                   ENDIF
129300040708     C*** POR
129400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
129500030721     C                   EVAL      PiStr=SkSplitCSV(i)
129600030721     C                   EXSR      CHKNUM
129700030721     C                   IF        PiInt=*on    AND
129800030721     C                             PiVal<=999   AND
129900030721     C                             PiVal>*zeros
130000040708     C                   Z-ADD     PiVal         VAOPOR
130100030721     C                   ELSE
130200030721     C                   SETON                                        32
130300040708     C                   EVAL      VAOPOR = *zeros
130400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130500040708     C                             + ' ' + 'VAOPOR'
130600030721     C                   ENDIF
130700030721     C                   ENDIF
130800040708     C*** POC
130900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
131000040708     C                   EVAL      PiStr=SkSplitCSV(i)
131100040708     C                   EXSR      CHKNUM
131200040708     C                   IF        PiInt=*on    AND
131300040708     C                             PiVal<=999   AND
131400040708     C                             PiVal>*zeros
131500040708     C                   Z-ADD     PiVal         VAOPOC
131600040708     C                   ELSE
131700040708     C                   SETON                                        32
131800040708     C                   EVAL      VAOPOC = *zeros
131900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
132000040708     C                             + ' ' + 'VAOPOC'
132100040708     C                   ENDIF
132200040708     C                   ENDIF
132300040708     C*** STO
132400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
132500040708     C                   EVAL      PiStr=SkSplitCSV(i)
132600040708     C                   EXSR      CHKNUM
132700040708     C                   IF        PiInt=*on
132800040708     C                   Z-ADD     PiVal         VAOSTO
132900040708     C                   ELSE
133000040708     C                   SETON                                        32
133100040708     C                   EVAL      VAOSTO = *zeros
133200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
133300040708     C                             + ' ' + 'VAOSTO'
133400040708     C                   ENDIF
133500040708     C                   ENDIF
133600030715     C*
133700030715     C                   ENDIF
133800030715     C                   EVAL      i = i + 1
133900030715     C                   ENDDO
134000101210     C*
134100101210     C* Se al termine del parsing il POE e/o il COR nn risultano valorizzato => errore
134200101210     C                   IF        VAOPOE = *zeros OR
134300101210     C                             VAOCOR = *zeros
134400101210     C                   SETON                                        31
134500101210     C                   ENDIF
134600020204     C*
134700030715     C                   ENDIF
134800020204     C*
134900010607     C                   ENDSR
135000010607     C*----------------------------------------------------*
135100020204
135200010330
135300010330
135400010330     C*----------------------------------------------------*
135500010330     C*  CONTROLLO NUMERICITA' CAMPI
135600010330     C*----------------------------------------------------*
135700010330     C     CHKNUM        BEGSR
135800010330     C*
135900010606     C                   IF        PiDecChr = *blanks
136000030715     C                   EVAL      PiDecChr = CharNUM
136100010606     C                   ENDIF
136200010606     C*
136300010606     C                   CALL(e)   'ISNUMERIC'
136400010330     C                   PARM                    PiStr            30
136500010606     C                   PARM                    PiDecChr          1
136600010330     C                   PARM      *ZEROS        PiVal            30 9
136700010330     C                   PARM      '0'           PiInt             1
136800010330     C                   PARM      '0'           PiNum             1
136900010330     C                   IF        %error
137000010606     C                   EVAL      PiNum=*off
137100010330     C                   ENDIF
137200010330     C*
137300010330     C                   ENDSR
137400010330     C***
137500010330
137600010601
137700010601
137800010601
137900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
138000130125     C***  repfil        BEGSR
138100010601     C*
138200130125     C***                if        invfil = *zeros and
138300130125     C***                          depfil > *zeros and
138400130125     C***                          (vinflg = *blanks or
138500130125     C***                           vinflg = *zeros)
138600130125     C***
138700130125     C***                eval      invfil = depfil
138800130125     C***                endif
138900010601     C*
139000130125     C***                if        depfil <> invfil and
139100130125     C***                          invfil > *zeros
139200130125     C***                eval      flgMulti = '1'
139300130125     C***                if        vinflg = *blanks
139400130125     C***                add       1             cntNonEl
139500130125     C***                endif
139600130125     C***                endif
139700010601     C*
139800130125     C***                if        vinflg = '2'
139900130125     C***                eval      flgStato = '2'
140000130125     C***                endif
140100010601     C*
140200130125     C***                ENDSR
140300010601     C***
140400010601
140500010601
140600010601
140700010330
140800010330
140900010330
141000990920      /TITLE Invio dei dati al punto operativo.
141100000613     C     invio         BEGSR
141200990920     C*
141300990920     C                   reset                   dscmz
141400010601     C                   move      invfil        cmzdst
141500990920     C                   eval      cmzfld = vlrfou
141600990920     C                   eval      cmzmbd = vlrhdl
141700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
141800000710     C                   if        prmfir = *blanks
141900040708     C                   eval      cmzfla = 'FNVAO00F'
142000040708     C                   eval      cmzmba = 'FNVAO00F'
142100000710     C                   else
142200000710     C                   eval      cmzfla = prmfir
142300000710     C                   eval      cmzmba = prmfir
142400000710     C                   endif
142500990920     C                   eval      cmznrr = *zeros
142600990920     C                   move      §ctrok        cmznrr
142700021018     C                   eval      cmzlba = vlrfl1
142800990920     C                   call(e)   'TIS711C'
142900990920     C                   parm                    dscmz
143000990921     C                   parm      *blanks       esito
143100990920     C                   if        %error
143200990920     C                             or cmzerr = '1'
143300990921     C                             or esito  = '1'
143400000710     C                   eval      wrkesito = '3'
143500990920     C                   endif
143600990920     C*
143700000613     C                   ENDSR
143800990910
143900010601
144000010601
144100010601
144200010601
144300010601      /TITLE Invio dei dati al punto operativo.
144400010601     C     opeini        BEGSR
144500010601     C*
144600010601     C* Inizializzo flag e contatori operativi
144700010601     C                   movel     '0'           flgGiro           1
144800010601     C                   movel     '0'           flgMulti          1
144900010601     C                   movel     '1'           flgStato          1
145000010615     C                   movel     '0'           flgOk             1
145100010601     C                   z-add     *zeros        cntNonEl         10 0
145200010601     C                   z-add     *zeros        depfil            3 0
145300010601     C                   z-add     *zeros        invfil            3 0
145400010601     C*
145500010601     C                   ENDSR
145600010601     C***
145700050421
145800050421
145900050421
146000050421     C     *pssr         BEGSR
146100050421     C*
146200050421     C                   eval      wrkesito = '2'
146300050421     C*
146400050421     C                   ENDSR     '*CANCL'
146500050421     C***
146600060306
146700060306
146800060306     C*-------------------------------------------------------------------------*
146900060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
147000060306     C*-------------------------------------------------------------------------*
147100060306     C     sr_Control    begsr
147200060306     C*
147300060306     C* inizializzo a ok il flag d controllo abilitazione
147400060306     C                   eval      Cli_Abil = *on
147500060306     C
147600060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
147700060306     C                   clear                   woriIDC
147800060306     C                   eval      kteCOD = 'VAP'
147900060306     C                   eval      kteKE1 = oriIDC
148000060306     C     keytbe        chain     TNTBE01L
148100060306     C                   if        %found(Tntbe01l)
148200060306     C                   eval      woriIDC = TbeKe2
148300060306     C                   else
148400060306     C                   eval      Cli_Abil = *off
148500060306     C                   endif
148600060306     C*
148700060306     C                   endsr
148800010601
148900010601
149000010601
149100010330
149200010330
149300000613     C     *inzsr        BEGSR
149400990910     C*
149500990910     C     *entry        plist
149600990920     C                   parm                    tivlrds
149700990921     C                   parm      wrkesito      esito
149800000724     C                   parm                    prmlit
149900000710     C                   parm                    prmfir
150000010330     C*
150100010330     C* CALCOLA LA DATA CORRENTE
150200010330     C                   time                    wn14             14 0
150300040723     C                   movel     wn14          wn6               6 0          *ORA (6)
150400130125     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
150500130125     C                   eval      datcor = %dec(%date() : *ISO)
150600040723     C*
150700040723     C* Campi riferito al database
150800040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
150900040723     C     *like         define    tbeke1        kteKE1
151000040723     C*
151100040723     C* Definizione chiavi di lettura
151200040723     C* TNTBE01L - parziale
151300040723     C     keytbe        klist
151400040723     C                   kfld                    kteCOD                         *UTENTE
151500040723     C                   kfld                    kteKE1                         *CODICE
151600000613     C*
151700000613     C                   ENDSR
151800000613     C***
