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
002300140519     D fnvao01r      e ds                  inz
002400010330     D esito           s              1
002500010330     D prmlit          s             10
002600010330     D prmfir          s             10
002700010330     D wrkesito        s                   like(esito)
002800010330     D rrnum           s              6  0 INZ(*zeros)
002900060306     D Cli_Abil        s              1    inz(*off)
003000060306     D woriIDC         s                   like(oriIDC) inz(*blanks)
003100090617     d §numori         s              7  0 inz
003200030715     D*------------------
003300030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
003400030715     D*------------------
003500030715     D SkSplitFLD      S             10    DIM(100)
003600030715     D SkSplitCSV      S            256    DIM(100)
003700030715     D CharCSV         S              1
003800030715     D CharTXT         S              1
003900030715     D CharNUM         S              1
004000090403     D CharSOS         S              1
004100030715     D posDa           S              3  0 INZ(*zeros)
004200030715     D posA            S              3  0 INZ(*zeros)
004300030715     D i               s              3  0 INZ(1)
004400030715     D wGiro           s              1  0 INZ(*zeros)
004500030822     D*------------------
004600030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
004700030822     D*------------------
004800030822     D posDaDft        S              3  0 INZ(*zeros)
004900030822     D posADft         S              3  0 INZ(*zeros)
005000030822     D j               s              3  0 INZ(1)
005100030822     D wGiroDft        s              1  0 INZ(*zeros)
005200031201     D*------------------
005300031201     D* Costanti
005400031201     D*------------------
005500031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
005600031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
005700010330
005800091026
005900091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
006000091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
006100091026
006200010330
006300010330
006400000913     C                   reset                   rrnum
006500990921     C                   reset                   esito
006600990921     C                   reset                   wrkesito
006700010601     C*
006800010601     C                   exsr      opeini
006900040708     C                   exsr      rwvao
007000010601     C*
007100010601     C                   seton                                        lr
007200010601
007300010601
007400010601
007500010601
007600010601     C*--------------------------------------------------------
007700010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007800010601     C*--------------------------------------------------------
007900010601     C     PREELA        BEGSR
008000010601     C*
008100010601     C* SE OCCORRE SPEDIRE IN FILIALE
008200010601     C                   if        invfil <> *zeros and
008300010601     C                             flgGiro = '0'
008400010601     C*
008500010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
008600010601     C                   eval      flgGiro = '1'
008700010601     C*
008800010601     C                   endif
008900010601     C*
009000010601     C                   ENDSR
009100010601     C***
009200010601
009300010601
009400010601
009500010601
009600010601
009700010601
009800010601
009900010601     C*--------------------------------------------------------
010000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010100010601     C*--------------------------------------------------------
010200010601     C     ENDELA        BEGSR
010300990915     C*
010400000616     C*
010500010601     C                   ENDSR
010600010601     C***
010700000613
010800010330
010900010601
011000010601
011100010601
011200010330     C*--------------------------------------------------------
011300040708     C* RWVAO   LEGGE TIVIN00R E SCRIVE FNVAOWWF              *
011400010330     C*--------------------------------------------------------
011500040708     C     RWVAO         BEGSR
011600010330     C*
011700010330     C                   if        not %open(tivin00r)
011800010330     C                   open      tivin00r
011900010330     C                   endif
012000040708     C                   if        not %open(fnvaowwr)
012100040708     C                   open      fnvaowwr
012200010330     C                   endif
012300140519     C*
012400140519     C* Mi predispongo per scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
012500140519     C                   call      'TIS7T7ER1'
012600140519     C                   parm      'O'           pIn_Opz           1
012700140519     C                   parm                    tivlrds
012800140519     C                   parm                    FNVAO01R
012900140519     C                   parm                    oriPRG
013000140519     C                   parm      *blanks       pOut_Esito        1
013100140519     C                   if        pOut_Esito = '2'
013200140519     C                   goto      FINE
013300140519     C                   endif
013400010330     C*
013500010604     C                   clear                   §CTROK
013600010604     C                   clear                   §CTRMO
013700010604     C                   clear                   §CTRNO
013800010330     C*
013900030822     C                   do        *HIVAL
014000010330     C*
014100030822     C                   read      tivin00r                               70
014200140519     C                   setoff                                       53
014300010618     C*
014400010618     C* Dopo ogni lettura verifico se ci sono stati record OK
014500010618     C                   if        vinflg = '1'
014600010618     C                   eval      flgOk = '1'
014700010618     C                   endif
014800010618     C*
014900040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
015000040510     C                   z-add     *zeros        wLenVINDTA        4 0
015100040510     C                   z-add     *zeros        wFlgVINDTA        1 0
015200040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
015300040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
015400040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
015500040510     C                   z-add     1             wFlgVINDTA
015600040510     C                   leave
015700040510     C                   endif
015800040510     C                   enddo
015900040510     C*
016000040218     C                   if        vindta > *blanks AND
016100040510     C                             wFlgVINDTA = 1
016200010330     C                   add       1             rrnum
016300010330     C*
016400010601     C                   if        *in70 = *off and
016500010330     C                             (vinflg = *blanks
016600010330     C                              or vinflg = '0'
016700010330     C                              or vinflg = '2')
016800010330     C*
016900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
017000010711     C                   if        vinflg = *blanks or vinflg = '0'
017100010711     C                   clear                   vinmsg
017200010711     C                   endif
017300010601     C*
017400010330     C                   exsr      inzvar
017500090403     C*
017600090403     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
017700090403     C* separatori campi
017800090403     C                   if        CharTXT <> *blanks
017900090403     C                   z-add     *zeros        wLenVINDTA        4 0
018000090403     C                   z-add     *zeros        wFlgVINDTA        1 0
018100090403     C                   setoff                                       80
018200090403     C                   dow       wLenVINDTA < %len(%trim(vindta))
018300090403     C                   eval      wLenVINDTA = wLenVINDTA + 1
018400090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
018500090403     C                   if        not *in80
018600090403     C                   seton                                        80
018700090403     C                   else
018800090403     C                   setoff                                       80
018900090403     C                   endif
019000090403     C                   endif
019100090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
019200090403     C                             and *in80 = *on
019300090403     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
019400090403     C                   endif
019500090403     C                   enddo
019600090403     C                   endif
019700090403     C*
019800040708     C                   exsr      impvao
019900010601     C*
020000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020100101210     C                   if        not *IN31
020200040708     C                   eval      depfil = VAOPOE
020300130125     C***                exsr      repfil
020400130125     C***                if        depfil = invfil
020500021025     C                   if        vlrpoi = 999
020600130125     C***                move(P)   invfil        VAOPOE
020700021025     C                   else
020800040708     C                   move(P)   vlrpoi        VAOPOE
020900021025     C                   endif
021000130125     C***                endif
021100101210     C                   endif
021200010601     C*
021300010601     C                   exsr      PREELA
021400010601     C*
021500010604     C* Ebbene...
021600010604     C*
021700030822     C  N31              add       1             §CTROK            7 0
021800030822     C   32              add       1             §CTRMO            7 0
021900030822     C   31              add       1             §CTRNO            7 0
022000030822     C                   if        wGiro = 2
022100140519     C   53              EXSR      EXEFORZ
022200040723     C  N31              EXSR      CHKCOR
022300040723     C  N31              EXSR      WRIORI
022400050826     C  N31              EXSR      WRIVGD
022500040708     C  N31              write     FNVAO000
022600140519     C*
022700140519     C* Se richiesta scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
022800140519     C                   if        *in53 and not *in31
022900140519     C                   call      'TIS7T7ER1'
023000140519     C                   parm      'W'           pIn_Opz           1
023100140519     C                   parm                    tivlrds
023200140519     C                   parm                    FNVAO01R
023300140519     C                   parm                    oriPRG
023400140519     C                   parm      *blanks       pOut_Esito        1
023500140519     C                   endif
023600140519     C                   if        pOut_Esito = '2'
023700140519     C                   seton                                        32
023800140519     C                   endif
023900030822     C                   endif
024000010604     C*
024100010604     C                   if        *in31 = *off and
024200010604     C                             *in32 = *off
024300010604     C                   eval      vinflg = '1'
024400010604     C                   else
024500010604     C                   eval      vinflg = '2'
024600010604     C                   endif
024700010601     C*
024800010604     C                   endif
024900010604     C*
025000010330     C                   else
025100010330     C                   eval      vinflg = '1'
025200010330     C                   endif
025300010601     C*
025400010601     C  N70              update    tivin000
025500010330     C*
025600030822     C  N70              enddo
025700140519     C*
025800140519     C     FINE          tag
025900140519     C*
026000140519     C* Chiusura per scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
026100140519     C                   call      'TIS7T7ER1'
026200140519     C                   parm      'C'           pIn_Opz           1
026300140519     C                   parm                    tivlrds
026400140519     C                   parm                    FNVAO01R
026500140519     C                   parm                    oriPRG
026600140519     C                   parm      *blanks       pOut_Esito        1
026700010601     C*
026800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
026900010601     C                   if        cntNonEl = *zeros or
027000010601     C                             flgMulti = '0'
027100010330     C* Se non ci sono record con errori ...
027200010601     C                   if        §ctrno = 0 and
027300010604     C                             §ctrmo = 0 and
027400010601     C                             flgStato <> '2'
027500010330     C* ... restituisco esito OK.
027600010330     C                   eval      wrkesito = '0'
027700010330     C                   else
027800010330     C                   if        §ctrok > 0
027900010330     C                   eval      wrkesito = '1'
028000010330     C                   else
028100010615     C                   if        flgOk = '0'
028200010615     C                   eval      wrkesito = '2'
028300010615     C                   else
028400010615     C                   eval      wrkesito = '6'
028500010615     C                   endif
028600010330     C                   endif
028700010330     C                   endif
028800010601     C                   else
028900010601     C                   eval      wrkesito = '9'
029000010601     C                   endif
029100010330     C*
029200010330     C                   if        %open(tivin00r)
029300010330     C                   close     tivin00r
029400010330     C                   endif
029500040708     C                   if        %open(fnvaowwr)
029600040708     C                   close     fnvaowwr
029700010330     C                   endif
029800010601     C*
029900010601     C                   if        vlrpoi <> 999
030000010601     C                   eval      invfil = vlrpoi
030100010601     C                   endif
030200101210     C*
030300101210     C                   if        invfil = 0 and
030400101210     C                             depfil > 0
030500101210     C                   eval      invfil = depfil
030600101210     C                   endif
030700010330     C*
030800010330     C                   if        §ctrok > 0
030900010601     C                             and invfil > *zeros
031000010330     C                   exsr      invio
031100010330     C                   endif
031200010601     C*
031300010618     C                   if        flgGiro = '1'
031400010601     C                   exsr      endela
031500010618     C                   endif
031600010330     C*
031700010330     C                   ENDSR
031800010330     C***
031900140519
032000140519
032100140519     C*------------------------------------------------------------------------*
032200140519     C* EXEFORZ - Effettuo forzature
032300140519     C*------------------------------------------------------------------------*
032400140519     C     EXEFORZ       BEGSR
032500140519     C*
032600140519     C* Se necessario e VAONOR non è valorizzato stacco un numeratore da AZNUM
032700140519     C                   if        vaoNOR = *zeros
032800140519     C                   clear                   trul33ds
032900140519     C                   eval      I33OPE = *zeros
033000140519     C                   eval      I33CNU = 600
033100140519     C                   eval      I33NUM = 1
033200140519     C                   movel     TRUL33DS      KPJBU
033300140519     C                   call      'TRUL33R'
033400140519     C                   parm                    KPJBA
033500140519     C                   movel     KPJBU         TRUL33DS
033600140519     C                   if        O33ERR = *zeros
033700140519     C                   z-add     O33NRF        §numori
033800140519     c                   else
033900140519     c                   eval      §numori = *all'9'
034000140519     C                   seton                                        31        * errore bloccante
034100140519     C                   endif
034200140519     C*
034300140519     C                   eval      vaoNOR = §numori
034400140519     C                   endif
034500140519     C*
034600140519     C                   ENDSR
034700140519
034800140519
034900050323
035000050323
035100050323     C*------------------------------------------------------------------------*
035200050323     C* CHKCOR - Considerazioni su VAOCOR
035300050323     C*------------------------------------------------------------------------*
035400050323     C     CHKCOR        BEGSR
035500050323     C*
035600050323     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
035700050323     C                   if        vaoTCO = 'F'
035800050323     C* Quindi verifico se il codice ordinante non è valorizzato
035900050323     C                   if        vaoCOR = *zeros
036000050323     C*                  if        vaoCOR = *zeros  AND
036100050323     C*                            vaoRSO = *blanks AND
036200050323     C*                            vaoINO = *blanks AND
036300050323     C*                            vaoCAO = *blanks AND
036400050323     C*                            vaoLOO = *blanks AND
036500050323     C*                            vaoPRO = *blanks AND
036600050323     C*                            vaoNAO = *blanks
036700050323     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
036800050323     C                   move      vlrKSC        wCli              7 0
036900050323     C                   movel(p)  wCli          acrCRO
037000050323     C     acrCRO        setll     fnacr01l
037100050323     C                   if        %found(fnacr01l)
037200050323     C                   read      fnacr01l
037300050323     C                   dow       not %eof(fnacr01l)
037400050323     C                   movel     acrCRO        wCliAcr           7 0
037500050323     C                   if        wCliAcr = wCli
037600050323     C                   eval      vaoCOR = acrCRO
037700050323     C                   leave
037800050323     C                   else
037900050323     C                   leave
038000050323     C                   endif
038100050323     C                   read      fnacr01l
038200050323     C                   enddo
038300050323     C                   endif
038400050323     C                   endif
038500050323     C                   endif
038600050323     C*
038700050323     C                   ENDSR
038800040723
038900040723
039000040723
039100040723     C*------------------------------------------------------------------------*
039200040723     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
039300040723     C*------------------------------------------------------------------------*
039400040723     C     WRIORI        BEGSR
039500040723     C*
039600090617     C* Come prima cosa stacco un numeratore da AZNUM
039700090617     C                   clear                   trul33ds
039800090617     C                   eval      I33OPE = *zeros
039900090617     C                   eval      I33CNU = 600
040000090617     C                   eval      I33NUM = 1
040100090617     C                   movel     TRUL33DS      KPJBU
040200090617     C                   call      'TRUL33R'
040300090617     C                   parm                    KPJBA
040400090617     C                   movel     KPJBU         TRUL33DS
040500090617     C                   if        O33ERR = *zeros
040600090617     C                   z-add     O33NRF        §numori
040700090618     c                   else
040800090618     c                   eval      §numori = *all'9'
040900090617     C                   endif
041000040723     C*
041100040723     C* Quindi imposto i campi "extra-VAO"
041200040723     C                   if        vlrKSC = '0ORM0000'
041300040723     C*
041400040723     C                   if        vaoCOR > *zeros
041500040723     C                   movel     vaoCOR        wrkCOR7           7 0
041600040723     C*
041700040723     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
041800040723     C* codice padre. Se è un padre assegno già il codice ordinante.
041900040723     C                   clear                   tibs10ds
042000040723     C                   eval      d10drf = datcor                              *data riferimento
042100040723     C                   eval      d10paf = 'P'                                 *cerca il padre
042200040723     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
042300091026     C*
042400091026     C* Preparazione chiamata al TIBS10R1
042500091026     C                   clear                   tibs10r1ds
042600091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
042700091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
042800091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
042900091026     C*
043000091026     C                   call      'TIBS10R1'
043100091026     C                   parm                    tibs10r1ds
043200091026     C*
043300091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
043400040723     C*
043500040723     C                   if        d10err <> *blanks or                         *No unif. o padre
043600040723     C                             d10cop =  *zeros  or
043700040723     C                             d10cop =  d10cod
043800040723     C                   move      d10cod        oriIDC
043900040723     C                   else                                                   *Figlio
044000040723     C                   move      d10cop        oriIDC
044100040723     C                   endif
044200040723     C*
044300040723     C                   else
044400040723     C                   eval      oriIDC = *zeros
044500040723     C                   endif
044600040723     C*
044700040723     C                   else
044800040723     C                   eval      oriIDC = vlrKSC
044900040723     C                   endif
045000040723     C*
045100040723     C                   eval      oriPRG = §numori
045200040723     C                   eval      oriDER = datcor
045300040723     C                   eval      oriOER = wn6
045400040723     C                   eval      oriPOE = vaoPOE
045500040723     C                   eval      oriNSR = vaoNSR
045600040723     C                   eval      oriNOR = vaoNOR
045700040723     C                   eval      oriNRV = vaoNRV
045800040723     C                   eval      oriFLO = *blanks
045900040723     C*
046000040723     C* Infine valorizzo la chiave esterna sul file FNVAO
046100040723     C                   eval      dorm01 = vaoFLO
046200040723     C                   movel     §numori       §ormpg
046300040723     C                   eval      vaoFLO = dorm01
046400040723     C*
046500040723     C* ...quindi scrivo il file TIORI00F
046600131008     C  N55              WRITE     TIORI000
046700040723     C*
046800040723     C                   ENDSR
046900050826
047000050826
047100050826
047200050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
047300050826     C     wriVGD        BEGSR
047400050826     C*
047500050826     C* Reperisco la descrizione della fase da tabella
047600050826     C                   movel(p)  'FAR'         KteCOD
047700050826     C                   movel(p)  '000'         KteKE1
047800050826     C     KEYtbe        chain     tntbe01l
047900050826     C                   if        %found(tntbe01l)
048000050826     C                   movel(P)  tbeuni        DFAR
048100050826     C                   else
048200050826     C                   clear                   DFAR
048300050826     C                   endif
048400050826     C*
048500050826     C* Valorizzo buffer record
048600050826     C                   clear                   FNVAPDS
048700050826     C                   eval      vapIDC = oriIDC
048800050826     C                   eval      vapPOE = oriPOE
048900050826     C                   eval      vapRFA = vaoRFA
049000050826     C                   eval      vapPOG = oriPOE
049100050826     C                   eval      vapDAE = oriDER
049200050826     C                   eval      vapORE = oriOER
049300050826     C                   if        d§fardva <> *blanks
049400050826     C                   eval      vapDFA = d§fardva
049500050826     C                   else
049600050826     C                   eval      vapDFA = d§fardes
049700050826     C                   endif
049800050826     C*
049900050826     C                   clear                   tivgd000
050000080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
050100050826     C                   eval      vgdTIP = 'VP'
050200050826     C                   eval      vgdKSU = vapIDC
050300050826     C                   eval      vgdTSC = 'WW'
050400050826     C                   eval      vgdDAT = datcor
050500050826     C                   eval      vgdPGM = 'TITVOXLR'
050600060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
050700060306     C                   exsr      sr_Control
050800060306     C                   if        Cli_Abil = *on
050900060306     C                   eval      vgdKSU = woriIDC
051000131008     C  N55              write     tivgd000
051100060306     C                   endif
051200050826     C*
051300050826     C                   ENDSR
051400050826     C*------------------------------------------------------------------------*
051500010601
051600010601
051700010601
051800010330     C*----------------------------------------------------*
051900030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
052000010330     C*----------------------------------------------------*
052100010330     C     INZVAR        BEGSR
052200010330     C*
052300030822     C* Inizializzo variabili di wrk
052400020204     C                   Z-ADD     *zeros        Num5_0            5 0
052500090403     C                   z-add     *zeros        invfil
052600030822     C*
052700030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
052800040708     C                   CLEAR                   FNVAO000
052900040723     C                   CLEAR                   TIORI000
053000030822     C                   CLEAR                   SkSplitCSV
053100030822     C                   EVAL      i = 1
053200030822     C                   EVAL      posDa = *zeros
053300030822     C                   EVAL      posA  = *zeros
053400030822     C*
053500030822     C* Reimposto i valori di default
053600030822     C                   EXSR      DEFCAM
053700030822     C*
053800010330     C                   ENDSR
053900010330     C*----------------------------------------------------*
054000030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
054100010330     C*----------------------------------------------------*
054200010330     C     DEFCAM        BEGSR
054300030822     C*
054400030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
054500030715     C* e delimitatore testo.
054600030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
054700030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
054800030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
054900090403     C*
055000090403     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
055100090403     C                   EVAL      CharSOS = CharNUM
055200030822     C*
055300030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
055400030715     C                   IF        %subst(vlrppt:1:1) = 'S'
055500030715     C                   SETON                                        50
055600030715     C                   ELSE
055700030715     C                   SETOFF                                       50
055800131008     C                   ENDIF
055900131008     C* aggiungo la gestyione dei test (tanto la stamapa non c'è più)
056000131008     C                   SETOFF                                       55
056100131008     C                   IF        %subst(vlrppt:1:1) = 'T'
056200131008     C* inibisco la scrittura dei file TIORI e TIVGD
056300131008     C                   SETON                                        55
056400131008     C* imposto il PO di emissione come filiale fittizia
056500131008     C                   EVAL      VAOPOE = 999
056600131008     C                   ENDIF
056700030822     C*
056800030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
056900030822     C                   EVAL      posDaDft = 1
057000030822     C                   EVAL      posADft  = 0
057100030822     C                   EVAL      wGiroDft = 0
057200030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
057300030822     C                             posDaDft > 0
057400030822     C*
057500030822     C* Gestisco il 1° giro
057600030822     C                   IF        wGiroDft = 0
057700030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
057800030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
057900030822     C* Incremento il contatore dei "giri"
058000030822     C                   EVAL      wGiroDft = 1
058100030822     C                   ELSE
058200030822     C                   EVAL      posDaDft = posADft
058300030822     C                   ENDIF
058400030822     C* Eseguo lo scan x trovare la fine del campo corrente
058500030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
058600030822     C*
058700030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
058800030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
058900030822     C                   IF        posDaDft > 0 AND
059000030822     C                             posADft  > 0
059100040708     C* POE
059200040708     C                   IF        %subst(
059300040708     C                             %subst(vlrppt:posDaDft+1:
059400040708     C                             posADft-posDaDft-1):1:3)
059500040708     C                             = 'POE'
059600040708     C                   EVAL      PiStr=%trim(%subst(
059700040708     C                             %subst(vlrppt:posDaDft+1:
059800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059900040708     C                   EXSR      CHKNUM
060000040708     C                   IF        PiInt=*on
060100040708     C                   Z-ADD     PiVal         VAOPOE
060200040708     C                   ENDIF
060300040708     C                   ENDIF
060400040708     C* TOR
060500040708     C                   IF        %subst(
060600040708     C                             %subst(vlrppt:posDaDft+1:
060700040708     C                             posADft-posDaDft-1):1:3)
060800040708     C                             = 'TOR'
060900040708     C                   EVAL      VAOTOR=%trim(%subst(
061000040708     C                             %subst(vlrppt:posDaDft+1:
061100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061200040708     C                   ENDIF
061300040708     C* TCO
061400040708     C                   IF        %subst(
061500040708     C                             %subst(vlrppt:posDaDft+1:
061600040708     C                             posADft-posDaDft-1):1:3)
061700040708     C                             = 'TCO'
061800040708     C                   EVAL      VAOTCO=%trim(%subst(
061900040708     C                             %subst(vlrppt:posDaDft+1:
062000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062100040708     C                   ENDIF
062200040707     C* COR
062300030822     C                   IF        %subst(
062400030822     C                             %subst(vlrppt:posDaDft+1:
062500030822     C                             posADft-posDaDft-1):1:3)
062600040707     C                             = 'COR'
062700030822     C                   EVAL      PiStr=%trim(%subst(
062800030822     C                             %subst(vlrppt:posDaDft+1:
062900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063000030822     C                   EXSR      CHKNUM
063100030822     C                   IF        PiInt=*on
063200040708     C                   Z-ADD     PiVal         VAOCOR
063300030822     C                   ENDIF
063400030822     C                   ENDIF
063500040708     C* CRC
063600040708     C                   IF        %subst(
063700040708     C                             %subst(vlrppt:posDaDft+1:
063800040708     C                             posADft-posDaDft-1):1:3)
063900040708     C                             = 'CRC'
064000040708     C                   EVAL      PiStr=%trim(%subst(
064100040708     C                             %subst(vlrppt:posDaDft+1:
064200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064300040708     C                   EXSR      CHKNUM
064400040708     C                   IF        PiInt=*on
064500040708     C                   Z-ADD     PiVal         VAOCRC
064600040708     C                   ENDIF
064700040708     C                   ENDIF
064800040708     C* FFD
064900040708     C                   IF        %subst(
065000040708     C                             %subst(vlrppt:posDaDft+1:
065100040708     C                             posADft-posDaDft-1):1:3)
065200040708     C                             = 'FFD'
065300040708     C                   EVAL      VAOFFD=%trim(%subst(
065400040708     C                             %subst(vlrppt:posDaDft+1:
065500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065600040708     C                   ENDIF
065700040708     C* ORR
065800040708     C                   IF        %subst(
065900040708     C                             %subst(vlrppt:posDaDft+1:
066000040708     C                             posADft-posDaDft-1):1:3)
066100040708     C                             = 'ORR'
066200040708     C                   EVAL      PiStr=%trim(%subst(
066300040708     C                             %subst(vlrppt:posDaDft+1:
066400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066500040708     C                   EXSR      CHKNUM
066600040708     C                   IF        PiInt=*on
066700040708     C                   Z-ADD     PiVal         VAOORR
066800040708     C                   ENDIF
066900040708     C                   ENDIF
067000040712     C* NCL
067100040712     C                   IF        %subst(
067200040712     C                             %subst(vlrppt:posDaDft+1:
067300040712     C                             posADft-posDaDft-1):1:3)
067400040712     C                             = 'NCL'
067500040712     C                   EVAL      PiStr=%trim(%subst(
067600040712     C                             %subst(vlrppt:posDaDft+1:
067700040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067800040712     C                   EXSR      CHKNUM
067900040712     C                   IF        PiInt=*on
068000040712     C                   Z-ADD     PiVal         VAONCL
068100040712     C                   ENDIF
068200040712     C                   ENDIF
068300040712     C* PKG
068400040712     C                   IF        %subst(
068500040712     C                             %subst(vlrppt:posDaDft+1:
068600040712     C                             posADft-posDaDft-1):1:3)
068700040712     C                             = 'PKG'
068800040712     C                   EVAL      PiStr=%trim(%subst(
068900040712     C                             %subst(vlrppt:posDaDft+1:
069000040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069100040712     C                   EXSR      CHKNUM
069200040712     C                   IF        PiNum=*on
069300040712     C                   Z-ADD     PiVal         VAOPKG
069400040712     C                   ENDIF
069500040712     C                   ENDIF
069600040712     C* VLM
069700040712     C                   IF        %subst(
069800040712     C                             %subst(vlrppt:posDaDft+1:
069900040712     C                             posADft-posDaDft-1):1:3)
070000040712     C                             = 'VLM'
070100040712     C                   EVAL      PiStr=%trim(%subst(
070200040712     C                             %subst(vlrppt:posDaDft+1:
070300040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070400040712     C                   EXSR      CHKNUM
070500040712     C                   IF        PiNum=*on
070600040712     C                   Z-ADD     PiVal         VAOVLM
070700040712     C                   ENDIF
070800040712     C                   ENDIF
070900040708     C* NAM
071000040708     C                   IF        %subst(
071100040708     C                             %subst(vlrppt:posDaDft+1:
071200040708     C                             posADft-posDaDft-1):1:3)
071300040708     C                             = 'NAM'
071400040708     C                   EVAL      VAONAM=%trim(%subst(
071500040708     C                             %subst(vlrppt:posDaDft+1:
071600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071700040708     C                   ENDIF
071800040708     C* PAG
071900030822     C                   IF        %subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):1:3)
072200040708     C                             = 'PAG'
072300040708     C                   EVAL      VAOPAG=%trim(%subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600030822     C                   ENDIF
072700040708     C* KSC
072800040708     C                   IF        %subst(
072900040708     C                             %subst(vlrppt:posDaDft+1:
073000040708     C                             posADft-posDaDft-1):1:3)
073100040708     C                             = 'KSC'
073200040708     C                   EVAL      PiStr=%trim(%subst(
073300040708     C                             %subst(vlrppt:posDaDft+1:
073400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073500040708     C                   EXSR      CHKNUM
073600040708     C                   IF        PiInt=*on
073700040708     C                   Z-ADD     PiVal         VAOKSC
073800040708     C                   ENDIF
073900040708     C                   ENDIF
074000040708     C* CTR
074100040708     C                   IF        %subst(
074200040708     C                             %subst(vlrppt:posDaDft+1:
074300040708     C                             posADft-posDaDft-1):1:3)
074400040708     C                             = 'CTR'
074500040708     C                   EVAL      VAOCTR=%trim(%subst(
074600040708     C                             %subst(vlrppt:posDaDft+1:
074700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074800040708     C                   ENDIF
074900040708     C* POC
075000040708     C                   IF        %subst(
075100040708     C                             %subst(vlrppt:posDaDft+1:
075200040708     C                             posADft-posDaDft-1):1:3)
075300040708     C                             = 'POC'
075400040708     C                   EVAL      PiStr=%trim(%subst(
075500040708     C                             %subst(vlrppt:posDaDft+1:
075600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075700040708     C                   EXSR      CHKNUM
075800040708     C                   IF        PiInt=*on
075900040708     C                   Z-ADD     PiVal         VAOPOC
076000040708     C                   ENDIF
076100040708     C                   ENDIF
076200040708     C* NO1
076300040708     C                   IF        %subst(
076400040708     C                             %subst(vlrppt:posDaDft+1:
076500040708     C                             posADft-posDaDft-1):1:3)
076600040708     C                             = 'NO1'
076700040708     C                   EVAL      VAONO1=%trim(%subst(
076800040708     C                             %subst(vlrppt:posDaDft+1:
076900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077000040708     C                   ENDIF
077100040708     C* DDT
077200030822     C                   IF        %subst(
077300030822     C                             %subst(vlrppt:posDaDft+1:
077400030822     C                             posADft-posDaDft-1):1:3)
077500040708     C                             = 'DDT'
077600040708     C                   EVAL      VAODDT=%trim(%subst(
077700030822     C                             %subst(vlrppt:posDaDft+1:
077800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077900030822     C                   ENDIF
078000040708     C* RFA
078100040708     C                   IF        %subst(
078200040708     C                             %subst(vlrppt:posDaDft+1:
078300040708     C                             posADft-posDaDft-1):1:3)
078400040708     C                             = 'RFA'
078500040708     C                   EVAL      VAORFA=%trim(%subst(
078600040708     C                             %subst(vlrppt:posDaDft+1:
078700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078800040708     C                   ENDIF
078900040708     C* FLO
079000040708     C                   IF        %subst(
079100040708     C                             %subst(vlrppt:posDaDft+1:
079200040708     C                             posADft-posDaDft-1):1:3)
079300040708     C                             = 'FLO'
079400040708     C                   EVAL      VAOFLO=%trim(%subst(
079500040708     C                             %subst(vlrppt:posDaDft+1:
079600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079700040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
079800040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
079900040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
080000040708     C                   ENDIF
080100030822     C* ...
080200030822     C                   ENDIF
080300030822     C                   ENDDO
080400020204     C*
080500010330     C                   ENDSR
080600010607     C*----------------------------------------------------*
080700040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
080800010607     C*----------------------------------------------------*
080900040708     C     IMPVAO        BEGSR
081000010607     C*
081100010607     C                   SETOFF                                       3132
081200030515     C*
081300040708     C                   MOVEL     datcor        VAODAO
081400040708     C                   MOVEL     wn14          VAOOAO
081500010607     C*
081600030715     C**********
081700030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
081800030715     C**********
081900030715     C                   IF        wGiro = *zeros
082000031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
082100031201     C     minu:maiu     XLATE     vindta        vindta
082200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
082300040324     C                   DOW       posDa <= %len(%trimr(vindta))
082400030715     C*
082500030715     C* Gestisco il 1° campo
082600030715     C                   IF        i = 1
082700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082800030715     C                   EVAL      posDa = 1
082900030715     C* Eseguo lo scan x trovare la fine del primo campo
083000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083100030715     C* A questo punto "estraggo" il campo corrente
083200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083300030715     C                                                    (posA-posDa))
083400030715     C* X i campi successivi al 1°
083500030715     C                   ELSE
083600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
083700030715     C                   EVAL      posDa = posA + 1
083800030715     C* Eseguo lo scan x trovare la fine del campo corrente
083900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084000030715     C* Gestisco l'ultimo campo
084100030715     C                   IF        posA = *zeros
084200040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
084300030715     C* A questo punto "estraggo" il campo corrente
084400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084500030715     C                                                    (posA-posDa))
084600030715     C                   LEAVE
084700030715     C                   ELSE
084800030715     C* A questo punto "estraggo" il campo corrente
084900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085000030715     C                                                    (posA-posDa))
085100030715     C                   ENDIF
085200030715     C                   ENDIF
085300030715     C* Incremento il contatore d campo
085400030715     C                   EVAL      i = i +1
085500030715     C                   ENDDO
085600030715     C*
085700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085800030715     C* delimitazione testo
085900030715     C                   EVAL      i = 1
086000030715     C                   DOW       i < %elem(SkSplitFLD)
086100030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
086200030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
086300030715     C                   EVAL      i = i + 1
086400030715     C                   ENDDO
086500030715     C                   EVAL      wGiro = 1
086600030715     C                   ELSE
086700030715     C                   EVAL      wGiro = 2
086800031204     C**********
086900031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
087000031204     C* d separatore campo
087100031204     C**********
087200040304     C                   IF        %subst(vindta:1:1) = CharCSV                 * all'inizio
087300040304     C                   EVAL      vindta = ' ' + vindta
087400040304     C                   ENDIF
087500040304     C***                DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
087600040304     C***                EVAL      vindta = %subst(vindta:2)
087700040304     C***                ENDDO
087800031204     C*
087900031204     C                   Z-ADD     *zeros        lunghInput        4 0
088000040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
088100040324     C                   IF        %subst(%trimr(vindta):lunghInput:1) = CharCSV
088200040304     C                   EVAL      vindta = vindta + ' '
088300040304     C                   ENDIF
088400040324     C***                DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
088500040324     C***                EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
088600040304     C***                EVAL      lunghInput = %len(%trim(vindta))
088700040304     C***                ENDDO
088800030715     C**********
088900030715     C* Effettuo lo split del campo dati d input
089000030715     C**********
089100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
089200040324     C                   DOW       posDa <= %len(%trimr(vindta))
089300030715     C*
089400030715     C* Gestisco il 1° campo
089500030715     C                   IF        i = 1
089600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
089700030715     C                   EVAL      posDa = 1
089800030715     C* Eseguo lo scan x trovare la fine del primo campo
089900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090000030715     C* A questo punto "estraggo" il campo corrente
090100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090200030715     C                                                    (posA-posDa))
090300030715     C* X i campi successivi al 1°
090400030715     C                   ELSE
090500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
090600030715     C                   EVAL      posDa = posA + 1
090700031204     C* Verifico che nn vi sia il campo nn valorizzato
090800031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
090900031204     C* Se campo nn valorizzato skippo al prossimo
091000031204     C                   EVAL      SkSplitCSV(i) = *blanks
091100031204     C                   EVAL      posA  = posA + 1
091200031204     C                   ELSE
091300030715     C* Eseguo lo scan x trovare la fine del campo corrente
091400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091500030715     C* Gestisco l'ultimo campo
091600030715     C                   IF        posA = *zeros
091700040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
091800030715     C* A questo punto "estraggo" il campo corrente
091900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092000030715     C                                                    (posA-posDa))
092100030715     C                   LEAVE
092200030715     C                   ELSE
092300030715     C* A questo punto "estraggo" il campo corrente
092400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092500030715     C                                                    (posA-posDa))
092600030715     C                   ENDIF
092700031204     C                   ENDIF
092800030715     C                   ENDIF
092900030715     C* Incremento il contatore d campo
093000030715     C                   EVAL      i = i +1
093100030715     C                   ENDDO
093200030715     C*
093300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
093400030715     C* delimitazione testo
093500030715     C                   EVAL      i = 1
093600030715     C                   DOW       i < %elem(SkSplitCSV)
093700030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
093800030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
093900030715     C                   EVAL      i = i + 1
094000030715     C                   ENDDO
094100030715     C*
094200030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
094300030715     C                   EVAL      i = 1
094400030715     C                   DOW       i < %elem(SkSplitFLD)
094500030715     C*
094600030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
094700030715     C                   IF        SkSplitFLD(i) <> *blanks AND
094800030715     C                             SkSplitCSV(i) <> *blanks
094900030721     C*** ATB
095000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATB'
095100040708     C                   EVAL      VAOATB = SkSplitCSV(i)
095200030721     C                   ENDIF
095300040708     C*** TOR
095400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTOR'
095500040708     C                   EVAL      VAOTOR = SkSplitCSV(i)
095600040708     C                   ENDIF
095700040708     C*** TCO
095800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTCO'
095900040708     C                   EVAL      VAOTCO = SkSplitCSV(i)
096000040708     C                   ENDIF
096100040708     C*** RSO
096200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSO'
096300040708     C                   EVAL      VAORSO = SkSplitCSV(i)
096400030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
096500040708     C     '@':'A'       XLATE     VAORSO        VAORSO
096600030715     C* ==
096700030715     C                   ENDIF
096800040708     C*** INO
096900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINO'
097000040708     C                   EVAL      VAOINO = SkSplitCSV(i)
097100030715     C                   ENDIF
097200040708     C*** LOO
097300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOO'
097400040708     C                   EVAL      VAOLOO = SkSplitCSV(i)
097500030715     C                   ENDIF
097600040708     C*** PRO
097700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRO'
097800040708     C                   EVAL      VAOPRO = SkSplitCSV(i)
097900030715     C                   ENDIF
098000040708     C*** NAO
098100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAO'
098200040708     C                   EVAL      VAONAO = SkSplitCSV(i)
098300030721     C                   ENDIF
098400040708     C*** RSR
098500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSR'
098600040708     C                   EVAL      VAORSR = SkSplitCSV(i)
098700040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
098800040708     C     '@':'A'       XLATE     VAORSR        VAORSR
098900040708     C* ==
099000040708     C                   IF        VAORSR = *blanks
099100040708     C                   SETON                                        31
099200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
099300040708     C                             + ' ' + 'VAORSR'
099400040708     C                   ENDIF
099500040708     C                   ENDIF
099600040708     C*** INR
099700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINR'
099800040708     C                   EVAL      VAOINR = SkSplitCSV(i)
099900040708     C                   IF        VAOINR = *blanks
100000040708     C                   SETON                                        31
100100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
100200040708     C                             + ' ' + 'VAOINR'
100300040708     C                   ENDIF
100400040708     C                   ENDIF
100500040708     C*** LOR
100600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOR'
100700040708     C                   EVAL      VAOLOR = SkSplitCSV(i)
100800040708     C                   IF        VAOLOR = *blanks
100900040708     C                   SETON                                        31
101000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
101100040708     C                             + ' ' + 'VAOLOR'
101200040708     C                   ENDIF
101300040708     C                   ENDIF
101400040708     C*** PRR
101500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRR'
101600040708     C                   EVAL      VAOPRR = SkSplitCSV(i)
101700040708     C                   IF        VAOPRR = *blanks
101800040708     C                   SETON                                        31
101900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
102000040708     C                             + ' ' + 'VAOPRR'
102100040708     C                   ENDIF
102200040708     C                   ENDIF
102300040708     C*** NAR
102400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAR'
102500040708     C                   EVAL      VAONAR = SkSplitCSV(i)
102600040708     C                   ENDIF
102700040708     C*** RER
102800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORER'
102900040708     C                   EVAL      VAORER = SkSplitCSV(i)
103000040708     C                   ENDIF
103100040708     C*** TER
103200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTER'
103300040708     C                   EVAL      VAOTER = SkSplitCSV(i)
103400040708     C                   ENDIF
103500040708     C*** RSC
103600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSC'
103700040708     C                   EVAL      VAORSC = SkSplitCSV(i)
103800040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
103900040708     C     '@':'A'       XLATE     VAORSC        VAORSC
104000040708     C* ==
104100040708     C                   ENDIF
104200040708     C*** INC
104300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINC'
104400040708     C                   EVAL      VAOINC = SkSplitCSV(i)
104500040708     C                   ENDIF
104600040708     C*** LOC
104700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOC'
104800040708     C                   EVAL      VAOLOC = SkSplitCSV(i)
104900040708     C                   ENDIF
105000040708     C*** PRC
105100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRC'
105200040708     C                   EVAL      VAOPRC = SkSplitCSV(i)
105300040708     C                   ENDIF
105400040708     C*** NAC
105500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAC'
105600040708     C                   EVAL      VAONAC = SkSplitCSV(i)
105700040708     C                   ENDIF
105800040708     C*** FFD
105900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFFD'
106000040708     C                   EVAL      VAOFFD = SkSplitCSV(i)
106100040708     C                   ENDIF
106200040708     C*** RMP
106300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORMP'
106400040708     C                   EVAL      VAORMP = SkSplitCSV(i)
106500040708     C                   ENDIF
106600040708     C*** NAM
106700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAM'
106800040708     C                   EVAL      VAONAM = SkSplitCSV(i)
106900040708     C                   ENDIF
107000040708     C*** PAG
107100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPAG'
107200040708     C                   EVAL      VAOPAG = SkSplitCSV(i)
107300040708     C                   ENDIF
107400040708     C*** CTR
107500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCTR'
107600040708     C                   EVAL      VAOCTR = SkSplitCSV(i)
107700040708     C                   ENDIF
107800040708     C*** ZOR
107900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOZOR'
108000040708     C                   EVAL      VAOZOR = SkSplitCSV(i)
108100040708     C                   ENDIF
108200040708     C*** NO1
108300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO1'
108400040708     C                   EVAL      VAONO1 = SkSplitCSV(i)
108500030715     C                   ENDIF
108600040708     C*** NO2
108700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO2'
108800040708     C                   EVAL      VAONO2 = SkSplitCSV(i)
108900030715     C                   ENDIF
109000040708     C*** DDT
109100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODDT'
109200040708     C                   EVAL      VAODDT = SkSplitCSV(i)
109300030715     C                   ENDIF
109400040708     C*** RFA
109500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORFA'
109600040708     C                   EVAL      VAORFA = SkSplitCSV(i)
109700030715     C                   ENDIF
109800040708     C*** SPI
109900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSPI'
110000040708     C                   EVAL      VAOSPI = SkSplitCSV(i)
110100030715     C                   ENDIF
110200040708     C*** FLO
110300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
110400040708     C                   EVAL      VAOFLO = SkSplitCSV(i)
110500040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
110600040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
110700040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
110800030715     C                   ENDIF
110900140519     C*
111000140519     C* ---  NUOVI CAMPO MAGGIO 2014 - FNVAOE0F ---
111100140519     C*
111200140519     C*** FLO2
111300140519     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO2'
111400140519     C                   seton                                        53
111500140519     C                   EVAL      VAOFLO2 = SkSplitCSV(i)
111600140519     C                   ENDIF
111700140519     C*** TSPS
111800140519     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTSPS'
111900140519     C                   seton                                        53
112000140519     C                   EVAL      VAOTSPS = SkSplitCSV(i)
112100140519     C                   ENDIF
112200140519     C*** NOTE
112300140519     C                   IF        %trim(SkSplitFLD(i)) = 'VAONOTE'
112400140519     C                   seton                                        53
112500140519     C                   EVAL      VAONOTE = SkSplitCSV(i)
112600140519     C                   ENDIF
112700140519     C*** EMLR
112800140519     C                   IF        %trim(SkSplitFLD(i)) = 'VAOEMLR'
112900140519     C                   seton                                        53
113000140519     C                   EVAL      VAOEMLR = SkSplitCSV(i)
113100140519     C                   ENDIF
113200140519     C*** EMLRF
113300140519     C                   IF        %trim(SkSplitFLD(i)) = 'VAOEMLRF'
113400140519     C                   seton                                        53
113500140519     C                   EVAL      VAOEMLRF = SkSplitCSV(i)
113600140519     C                   ENDIF
113700140519     C*** SMSR
113800140519     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSMSR'
113900140519     C                   seton                                        53
114000140519     C                   EVAL      VAOSMSR = SkSplitCSV(i)
114100140519     C                   ENDIF
114200140519     C*** SMSRF
114300140519     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSMSRF'
114400140519     C                   seton                                        53
114500140519     C                   EVAL      VAOSMSRF = SkSplitCSV(i)
114600140519     C                   ENDIF
114700030721     C*
114800030721     C* Reperisco quindi i campi numerici...
114900040708     C*** POE
115000040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
115100040723     C                   EVAL      PiStr=SkSplitCSV(i)
115200040723     C                   EXSR      CHKNUM
115300040723     C                   IF        PiInt=*on    AND
115400040723     C                             PiVal<=999   AND
115500040723     C                             PiVal>*zeros
115600040723     C                   Z-ADD     PiVal         VAOPOE
115700040723     C                   ELSE
115800040723     C                   SETON                                        32
115900040723     C                   EVAL      VAOPOE = *zeros
116000040723     C                   EVAL      vinmsg = %trimr(vinmsg)
116100040723     C                             + ' ' + 'VAOPOE'
116200040723     C                   ENDIF
116300040723     C                   ENDIF
116400040708     C*** COR
116500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
116600040708     C                   EVAL      PiStr=SkSplitCSV(i)
116700040708     C                   EXSR      CHKNUM
116800040708     C                   IF        PiInt=*on
116900040708     C                   Z-ADD     PiVal         VAOCOR
117000040708     C                   ELSE
117100040708     C                   SETON                                        31
117200040708     C                   EVAL      VAOCOR = *zeros
117300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
117400040708     C                             + ' ' + 'VAOCOR'
117500040708     C                   ENDIF
117600040708     C                   ENDIF
117700040708     C*** CAO
117800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
117900130502     C                   EVAL      VAOCAO = SkSplitCSV(i)
118000130502     C***                EVAL      PiStr=SkSplitCSV(i)
118100130502     C***                EXSR      CHKNUM
118200130502     C***                IF        PiInt=*on
118300130502     C***                Z-ADD     PiVal         Num5_0
118400130502     C***                MOVEL(p)  Num5_0        VAOCAO
118500130502     C***                ELSE
118600130502     C***                SETON                                        32
118700130502     C***                EVAL      VAOCAO = *zeros
118800130502     C***                EVAL      vinmsg = %trimr(vinmsg)
118900130502     C***                          + ' ' + 'VAOCAO'
119000130502     C***                ENDIF
119100030721     C                   ENDIF
119200040708     C*** CRA
119300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
119400030721     C                   EVAL      PiStr=SkSplitCSV(i)
119500030721     C                   EXSR      CHKNUM
119600030721     C                   IF        PiInt=*on
119700040708     C                   Z-ADD     PiVal         VAOCRA
119800030721     C                   ELSE
119900030721     C                   SETON                                        32
120000040708     C                   EVAL      VAOCRA = *zeros
120100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120200040708     C                             + ' ' + 'VAOCRA'
120300030721     C                   ENDIF
120400030721     C                   ENDIF
120500040708     C*** CAR
120600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
120700130502     C                   EVAL      VAOCAR = SkSplitCSV(i)
120800130502     C***                EVAL      PiStr=SkSplitCSV(i)
120900130502     C***                EXSR      CHKNUM
121000130502     C***                IF        PiInt=*on
121100130502     C***                Z-ADD     PiVal         Num5_0
121200130502     C***                MOVEL(p)  Num5_0        VAOCAR
121300130502     C***                ELSE
121400130502     C***                SETON                                        31
121500130502     C***                EVAL      VAOCAR = *zeros
121600130502     C***                EVAL      vinmsg = %trimr(vinmsg)
121700130502     C***                          + ' ' + 'VAOCAR'
121800130502     C***                ENDIF
121900040708     C                   ENDIF
122000040708     C*** CRC
122100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
122200040708     C                   EVAL      PiStr=SkSplitCSV(i)
122300040708     C                   EXSR      CHKNUM
122400040708     C                   IF        PiInt=*on
122500040708     C                   Z-ADD     PiVal         VAOCRC
122600040708     C                   ELSE
122700040708     C                   SETON                                        32
122800040708     C                   EVAL      VAOCRC = *zeros
122900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
123000040708     C                             + ' ' + 'VAOCRC'
123100040708     C                   ENDIF
123200040708     C                   ENDIF
123300040708     C*** CAC
123400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
123500130502     C                   EVAL      VAOCAC = SkSplitCSV(i)
123600130502     C***                EVAL      PiStr=SkSplitCSV(i)
123700130502     C***                EXSR      CHKNUM
123800130502     C***                IF        PiInt=*on
123900130502     C***                Z-ADD     PiVal         Num5_0
124000130502     C***                MOVEL(p)  Num5_0        VAOCAC
124100130502     C***                ELSE
124200130502     C***                SETON                                        32
124300130502     C***                EVAL      VAOCAC = *zeros
124400130502     C***                EVAL      vinmsg = %trimr(vinmsg)
124500130502     C***                          + ' ' + 'VAOCAC'
124600130502     C***                ENDIF
124700040708     C                   ENDIF
124800040708     C*** DAR
124900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
125000040708     C                   EVAL      PiStr=SkSplitCSV(i)
125100040708     C                   EXSR      CHKNUM
125200040708     C                   IF        PiInt=*on
125300040708     C                   Z-ADD     PiVal         VAODAR
125400040708     C                   ELSE
125500040708     C                   SETON                                        32
125600040708     C                   EVAL      VAODAR = *zeros
125700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
125800040708     C                             + ' ' + 'VAODAR'
125900040708     C                   ENDIF
126000040708     C                   ENDIF
126100040708     C*** ORR
126200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
126300040708     C                   EVAL      PiStr=SkSplitCSV(i)
126400040708     C                   EXSR      CHKNUM
126500040708     C                   IF        PiInt=*on
126600040708     C                   Z-ADD     PiVal         VAOORR
126700040708     C                   ELSE
126800040708     C                   SETON                                        32
126900040708     C                   EVAL      VAOORR = *zeros
127000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
127100040708     C                             + ' ' + 'VAOORR'
127200040708     C                   ENDIF
127300040708     C                   ENDIF
127400030721     C*** NCL
127500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
127600030721     C                   EVAL      PiStr=SkSplitCSV(i)
127700030721     C                   EXSR      CHKNUM
127800030721     C                   IF        PiInt=*on
127900040708     C                   Z-ADD     PiVal         VAONCL
128000030721     C                   ELSE
128100030721     C                   SETON                                        32
128200040708     C                   EVAL      VAONCL = *zeros
128300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128400040708     C                             + ' ' + 'VAONCL'
128500030721     C                   ENDIF
128600030721     C                   ENDIF
128700040708     C*** PKG
128800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
128900030721     C                   EVAL      PiStr=SkSplitCSV(i)
129000030721     C                   EXSR      CHKNUM
129100030721     C                   IF        PiNum=*on
129200040708     C                   Z-ADD     PiVal         VAOPKG
129300030721     C                   ELSE
129400030721     C                   SETON                                        32
129500040708     C                   EVAL      VAOPKG = *zeros
129600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129700040708     C                             + ' ' + 'VAOPKG'
129800030721     C                   ENDIF
129900030721     C                   ENDIF
130000040708     C*** VLM
130100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
130200040708     C                   EVAL      PiStr=SkSplitCSV(i)
130300040708     C                   EXSR      CHKNUM
130400040708     C                   IF        PiNum=*on
130500040708     C                   Z-ADD     PiVal         VAOVLM
130600040708     C                   ELSE
130700040708     C                   SETON                                        32
130800040708     C                   EVAL      VAOVLM = *zeros
130900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
131000040708     C                             + ' ' + 'VAOVLM'
131100040708     C                   ENDIF
131200040708     C                   ENDIF
131300040708     C*** BNC
131400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
131500040708     C                   EVAL      PiStr=SkSplitCSV(i)
131600040708     C                   EXSR      CHKNUM
131700040708     C                   IF        PiInt=*on
131800040708     C                   Z-ADD     PiVal         VAOBNC
131900040708     C                   ELSE
132000040708     C                   SETON                                        32
132100040708     C                   EVAL      VAOBNC = *zeros
132200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
132300040708     C                             + ' ' + 'VAOBNC'
132400040708     C                   ENDIF
132500040708     C                   ENDIF
132600040708     C*** BLC
132700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
132800040708     C                   EVAL      PiStr=SkSplitCSV(i)
132900040708     C                   EXSR      CHKNUM
133000040708     C                   IF        PiInt=*on
133100040708     C                   Z-ADD     PiVal         VAOBLC
133200040708     C                   ELSE
133300040708     C                   SETON                                        32
133400040708     C                   EVAL      VAOBLC = *zeros
133500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
133600040708     C                             + ' ' + 'VAOBLC'
133700040708     C                   ENDIF
133800040708     C                   ENDIF
133900040708     C*** ATT
134000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
134100040708     C                   EVAL      PiStr=SkSplitCSV(i)
134200040708     C                   EXSR      CHKNUM
134300040708     C                   IF        PiInt=*on
134400040708     C                   Z-ADD     PiVal         VAOATT
134500040708     C                   ELSE
134600040708     C                   SETON                                        32
134700040708     C                   EVAL      VAOATT = *zeros
134800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
134900040708     C                             + ' ' + 'VAOATT'
135000040708     C                   ENDIF
135100040708     C                   ENDIF
135200040708     C*** MTC
135300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
135400040708     C                   EVAL      PiStr=SkSplitCSV(i)
135500040708     C                   EXSR      CHKNUM
135600040708     C                   IF        PiInt=*on
135700040708     C                   Z-ADD     PiVal         VAOMTC
135800040708     C                   ELSE
135900040708     C                   SETON                                        32
136000040708     C                   EVAL      VAOMTC = *zeros
136100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
136200040708     C                             + ' ' + 'VAOMTC'
136300040708     C                   ENDIF
136400040708     C                   ENDIF
136500040708     C*** KSC
136600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
136700030721     C                   EVAL      PiStr=SkSplitCSV(i)
136800030721     C                   EXSR      CHKNUM
136900030721     C                   IF        PiInt=*on      AND
137000030721     C                             PiVal<=9999999 AND
137100030721     C                             PiVal>*zeros
137200040708     C                   Z-ADD     PiVal         VAOKSC
137300030721     C                   ELSE
137400030721     C                   SETON                                        32
137500040708     C                   EVAL      VAOKSC = *zeros
137600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137700040708     C                             + ' ' + 'VAOKSC'
137800030721     C                   ENDIF
137900030721     C                   ENDIF
138000040708     C*** POR
138100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
138200030721     C                   EVAL      PiStr=SkSplitCSV(i)
138300030721     C                   EXSR      CHKNUM
138400030721     C                   IF        PiInt=*on    AND
138500030721     C                             PiVal<=999   AND
138600030721     C                             PiVal>*zeros
138700040708     C                   Z-ADD     PiVal         VAOPOR
138800030721     C                   ELSE
138900030721     C                   SETON                                        32
139000040708     C                   EVAL      VAOPOR = *zeros
139100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139200040708     C                             + ' ' + 'VAOPOR'
139300030721     C                   ENDIF
139400030721     C                   ENDIF
139500040708     C*** POC
139600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
139700040708     C                   EVAL      PiStr=SkSplitCSV(i)
139800040708     C                   EXSR      CHKNUM
139900040708     C                   IF        PiInt=*on    AND
140000040708     C                             PiVal<=999   AND
140100040708     C                             PiVal>*zeros
140200040708     C                   Z-ADD     PiVal         VAOPOC
140300040708     C                   ELSE
140400040708     C                   SETON                                        32
140500040708     C                   EVAL      VAOPOC = *zeros
140600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
140700040708     C                             + ' ' + 'VAOPOC'
140800040708     C                   ENDIF
140900040708     C                   ENDIF
141000040708     C*** STO
141100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
141200040708     C                   EVAL      PiStr=SkSplitCSV(i)
141300040708     C                   EXSR      CHKNUM
141400040708     C                   IF        PiInt=*on
141500040708     C                   Z-ADD     PiVal         VAOSTO
141600040708     C                   ELSE
141700040708     C                   SETON                                        32
141800040708     C                   EVAL      VAOSTO = *zeros
141900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
142000040708     C                             + ' ' + 'VAOSTO'
142100040708     C                   ENDIF
142200040708     C                   ENDIF
142300140519     C*
142400140519     C* ---  NUOVI CAMPO MAGGIO 2014 - FNVAOE0F ---
142500140519     C*
142600140519     C*** ORDA1
142700140519     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORDA1'
142800140519     C                   seton                                        53
142900140519     C                   EVAL      PiStr=SkSplitCSV(i)
143000140519     C                   EXSR      CHKNUM
143100140519     C                   IF        PiInt=*on
143200140519     C                   Z-ADD     PiVal         VAOORDA1
143300140519     C                   ELSE
143400140519     C                   SETON                                        32
143500140519     C                   EVAL      VAOORDA1 = *zeros
143600140519     C                   EVAL      vinmsg = %trimr(vinmsg)
143700140519     C                             + ' ' + 'VAOORDA1'
143800140519     C                   ENDIF
143900140519     C                   ENDIF
144000140519     C*** ORAA1
144100140519     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORAA1'
144200140519     C                   seton                                        53
144300140519     C                   EVAL      PiStr=SkSplitCSV(i)
144400140519     C                   EXSR      CHKNUM
144500140519     C                   IF        PiInt=*on
144600140519     C                   Z-ADD     PiVal         VAOORAA1
144700140519     C                   ELSE
144800140519     C                   SETON                                        32
144900140519     C                   EVAL      VAOORAA1 = *zeros
145000140519     C                   EVAL      vinmsg = %trimr(vinmsg)
145100140519     C                             + ' ' + 'VAOORAA1'
145200140519     C                   ENDIF
145300140519     C                   ENDIF
145400140519     C*** ORDA2
145500140519     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORDA2'
145600140519     C                   seton                                        53
145700140519     C                   EVAL      PiStr=SkSplitCSV(i)
145800140519     C                   EXSR      CHKNUM
145900140519     C                   IF        PiInt=*on
146000140519     C                   Z-ADD     PiVal         VAOORDA2
146100140519     C                   ELSE
146200140519     C                   SETON                                        32
146300140519     C                   EVAL      VAOORDA2 = *zeros
146400140519     C                   EVAL      vinmsg = %trimr(vinmsg)
146500140519     C                             + ' ' + 'VAOORDA2'
146600140519     C                   ENDIF
146700140519     C                   ENDIF
146800140519     C*** ORAA2
146900140519     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORAA2'
147000140519     C                   seton                                        53
147100140519     C                   EVAL      PiStr=SkSplitCSV(i)
147200140519     C                   EXSR      CHKNUM
147300140519     C                   IF        PiInt=*on
147400140519     C                   Z-ADD     PiVal         VAOORAA2
147500140519     C                   ELSE
147600140519     C                   SETON                                        32
147700140519     C                   EVAL      VAOORAA2 = *zeros
147800140519     C                   EVAL      vinmsg = %trimr(vinmsg)
147900140519     C                             + ' ' + 'VAOORAA2'
148000140519     C                   ENDIF
148100140519     C                   ENDIF
148200030715     C*
148300030715     C                   ENDIF
148400030715     C                   EVAL      i = i + 1
148500030715     C                   ENDDO
148600101210     C*
148700101210     C* Se al termine del parsing il POE e/o il COR nn risultano valorizzato => errore
148800101210     C                   IF        VAOPOE = *zeros OR
148900101210     C                             VAOCOR = *zeros
149000101210     C                   SETON                                        31
149100101210     C                   ENDIF
149200020204     C*
149300030715     C                   ENDIF
149400020204     C*
149500010607     C                   ENDSR
149600010607     C*----------------------------------------------------*
149700020204
149800010330
149900010330
150000010330     C*----------------------------------------------------*
150100010330     C*  CONTROLLO NUMERICITA' CAMPI
150200010330     C*----------------------------------------------------*
150300010330     C     CHKNUM        BEGSR
150400010330     C*
150500010606     C                   IF        PiDecChr = *blanks
150600030715     C                   EVAL      PiDecChr = CharNUM
150700010606     C                   ENDIF
150800010606     C*
150900010606     C                   CALL(e)   'ISNUMERIC'
151000010330     C                   PARM                    PiStr            30
151100010606     C                   PARM                    PiDecChr          1
151200010330     C                   PARM      *ZEROS        PiVal            30 9
151300010330     C                   PARM      '0'           PiInt             1
151400010330     C                   PARM      '0'           PiNum             1
151500010330     C                   IF        %error
151600010606     C                   EVAL      PiNum=*off
151700010330     C                   ENDIF
151800010330     C*
151900010330     C                   ENDSR
152000010330     C***
152100010330
152200010601
152300010601
152400010601
152500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
152600130125     C***  repfil        BEGSR
152700010601     C*
152800130125     C***                if        invfil = *zeros and
152900130125     C***                          depfil > *zeros and
153000130125     C***                          (vinflg = *blanks or
153100130125     C***                           vinflg = *zeros)
153200130125     C***
153300130125     C***                eval      invfil = depfil
153400130125     C***                endif
153500010601     C*
153600130125     C***                if        depfil <> invfil and
153700130125     C***                          invfil > *zeros
153800130125     C***                eval      flgMulti = '1'
153900130125     C***                if        vinflg = *blanks
154000130125     C***                add       1             cntNonEl
154100130125     C***                endif
154200130125     C***                endif
154300010601     C*
154400130125     C***                if        vinflg = '2'
154500130125     C***                eval      flgStato = '2'
154600130125     C***                endif
154700010601     C*
154800130125     C***                ENDSR
154900010601     C***
155000010601
155100010601
155200010601
155300010330
155400010330
155500010330
155600990920      /TITLE Invio dei dati al punto operativo.
155700000613     C     invio         BEGSR
155800990920     C*
155900990920     C                   reset                   dscmz
156000010601     C                   move      invfil        cmzdst
156100990920     C                   eval      cmzfld = vlrfou
156200990920     C                   eval      cmzmbd = vlrhdl
156300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
156400000710     C                   if        prmfir = *blanks
156500040708     C                   eval      cmzfla = 'FNVAO00F'
156600040708     C                   eval      cmzmba = 'FNVAO00F'
156700000710     C                   else
156800000710     C                   eval      cmzfla = prmfir
156900000710     C                   eval      cmzmba = prmfir
157000000710     C                   endif
157100990920     C                   eval      cmznrr = *zeros
157200990920     C                   move      §ctrok        cmznrr
157300021018     C                   eval      cmzlba = vlrfl1
157400990920     C                   call(e)   'TIS711C'
157500990920     C                   parm                    dscmz
157600990921     C                   parm      *blanks       esito
157700990920     C                   if        %error
157800990920     C                             or cmzerr = '1'
157900990921     C                             or esito  = '1'
158000000710     C                   eval      wrkesito = '3'
158100990920     C                   endif
158200990920     C*
158300000613     C                   ENDSR
158400990910
158500010601
158600010601
158700010601
158800010601
158900010601      /TITLE Invio dei dati al punto operativo.
159000010601     C     opeini        BEGSR
159100010601     C*
159200010601     C* Inizializzo flag e contatori operativi
159300010601     C                   movel     '0'           flgGiro           1
159400010601     C                   movel     '0'           flgMulti          1
159500010601     C                   movel     '1'           flgStato          1
159600010615     C                   movel     '0'           flgOk             1
159700010601     C                   z-add     *zeros        cntNonEl         10 0
159800010601     C                   z-add     *zeros        depfil            3 0
159900010601     C                   z-add     *zeros        invfil            3 0
160000010601     C*
160100010601     C                   ENDSR
160200010601     C***
160300050421
160400050421
160500050421
160600050421     C     *pssr         BEGSR
160700050421     C*
160800050421     C                   eval      wrkesito = '2'
160900050421     C*
161000050421     C                   ENDSR     '*CANCL'
161100050421     C***
161200060306
161300060306
161400060306     C*-------------------------------------------------------------------------*
161500060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
161600060306     C*-------------------------------------------------------------------------*
161700060306     C     sr_Control    begsr
161800060306     C*
161900060306     C* inizializzo a ok il flag d controllo abilitazione
162000060306     C                   eval      Cli_Abil = *on
162100060306     C
162200060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
162300060306     C                   clear                   woriIDC
162400060306     C                   eval      kteCOD = 'VAP'
162500060306     C                   eval      kteKE1 = oriIDC
162600060306     C     keytbe        chain     TNTBE01L
162700060306     C                   if        %found(Tntbe01l)
162800060306     C                   eval      woriIDC = TbeKe2
162900060306     C                   else
163000060306     C                   eval      Cli_Abil = *off
163100060306     C                   endif
163200060306     C*
163300060306     C                   endsr
163400010601
163500010601
163600010601
163700010330
163800010330
163900000613     C     *inzsr        BEGSR
164000990910     C*
164100990910     C     *entry        plist
164200990920     C                   parm                    tivlrds
164300990921     C                   parm      wrkesito      esito
164400000724     C                   parm                    prmlit
164500000710     C                   parm                    prmfir
164600010330     C*
164700010330     C* CALCOLA LA DATA CORRENTE
164800010330     C                   time                    wn14             14 0
164900040723     C                   movel     wn14          wn6               6 0          *ORA (6)
165000130125     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
165100130125     C                   eval      datcor = %dec(%date() : *ISO)
165200040723     C*
165300040723     C* Campi riferito al database
165400040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
165500040723     C     *like         define    tbeke1        kteKE1
165600040723     C*
165700040723     C* Definizione chiavi di lettura
165800040723     C* TNTBE01L - parziale
165900040723     C     keytbe        klist
166000040723     C                   kfld                    kteCOD                         *UTENTE
166100040723     C                   kfld                    kteKE1                         *CODICE
166200000613     C*
166300000613     C                   ENDSR
166400000613     C***
