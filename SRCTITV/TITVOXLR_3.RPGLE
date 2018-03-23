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
002300140730     D fnvaoear      e ds                  inz
002400140730     D                                     prefix(w_)
002500010330     D esito           s              1
002600010330     D prmlit          s             10
002700010330     D prmfir          s             10
002800010330     D wrkesito        s                   like(esito)
002900010330     D rrnum           s              6  0 INZ(*zeros)
003000060306     D Cli_Abil        s              1    inz(*off)
003100060306     D woriIDC         s                   like(oriIDC) inz(*blanks)
003200090617     d §numori         s              7  0 inz
003300030715     D*------------------
003400030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
003500030715     D*------------------
003600030715     D SkSplitFLD      S             10    DIM(100)
003700030715     D SkSplitCSV      S            256    DIM(100)
003800030715     D CharCSV         S              1
003900030715     D CharTXT         S              1
004000030715     D CharNUM         S              1
004100090403     D CharSOS         S              1
004200030715     D posDa           S              3  0 INZ(*zeros)
004300030715     D posA            S              3  0 INZ(*zeros)
004400030715     D i               s              3  0 INZ(1)
004500030715     D wGiro           s              1  0 INZ(*zeros)
004600030822     D*------------------
004700030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
004800030822     D*------------------
004900030822     D posDaDft        S              3  0 INZ(*zeros)
005000030822     D posADft         S              3  0 INZ(*zeros)
005100030822     D j               s              3  0 INZ(1)
005200030822     D wGiroDft        s              1  0 INZ(*zeros)
005300031201     D*------------------
005400031201     D* Costanti
005500031201     D*------------------
005600031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
005700031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
005800010330
005900091026
006000091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
006100091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
006200091026
006300010330
006400010330
006500000913     C                   reset                   rrnum
006600990921     C                   reset                   esito
006700990921     C                   reset                   wrkesito
006800010601     C*
006900010601     C                   exsr      opeini
007000040708     C                   exsr      rwvao
007100010601     C*
007200010601     C                   seton                                        lr
007300010601
007400010601
007500010601
007600010601
007700010601     C*--------------------------------------------------------
007800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007900010601     C*--------------------------------------------------------
008000010601     C     PREELA        BEGSR
008100010601     C*
008200010601     C* SE OCCORRE SPEDIRE IN FILIALE
008300010601     C                   if        invfil <> *zeros and
008400010601     C                             flgGiro = '0'
008500010601     C*
008600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
008700010601     C                   eval      flgGiro = '1'
008800010601     C*
008900010601     C                   endif
009000010601     C*
009100010601     C                   ENDSR
009200010601     C***
009300010601
009400010601
009500010601
009600010601
009700010601
009800010601
009900010601
010000010601     C*--------------------------------------------------------
010100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010200010601     C*--------------------------------------------------------
010300010601     C     ENDELA        BEGSR
010400990915     C*
010500000616     C*
010600010601     C                   ENDSR
010700010601     C***
010800000613
010900010330
011000010601
011100010601
011200010601
011300010330     C*--------------------------------------------------------
011400040708     C* RWVAO   LEGGE TIVIN00R E SCRIVE FNVAOWWF              *
011500010330     C*--------------------------------------------------------
011600040708     C     RWVAO         BEGSR
011700010330     C*
011800010330     C                   if        not %open(tivin00r)
011900010330     C                   open      tivin00r
012000010330     C                   endif
012100040708     C                   if        not %open(fnvaowwr)
012200040708     C                   open      fnvaowwr
012300010330     C                   endif
012400140730     C*
012500140730     C* Mi predispongo per scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
012600140730     C                   call      'TIS7T7ER'
012700140730     C                   parm      'O'           pIn_Opz           1
012800140730     C                   parm                    tivlrds
012900140730     C                   parm                    FNVAOEAR
013000140730     C                   parm                    oriPRG
013100140730     C                   parm      *zeros        pIn_Dst           3
013200140730     C                   parm      *blanks       pOut_Esito        1
013300140730     C                   if        pOut_Esito = '2'
013400140730     C                   goto      FINE
013500140730     C                   endif
013600010330     C*
013700010604     C                   clear                   §CTROK
013800010604     C                   clear                   §CTRMO
013900010604     C                   clear                   §CTRNO
014000010330     C*
014100030822     C                   do        *HIVAL
014200010330     C*
014300030822     C                   read      tivin00r                               70
014400010618     C*
014500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
014600010618     C                   if        vinflg = '1'
014700010618     C                   eval      flgOk = '1'
014800010618     C                   endif
014900010618     C*
015000040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
015100040510     C                   z-add     *zeros        wLenVINDTA        4 0
015200040510     C                   z-add     *zeros        wFlgVINDTA        1 0
015300040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
015400040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
015500040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
015600040510     C                   z-add     1             wFlgVINDTA
015700040510     C                   leave
015800040510     C                   endif
015900040510     C                   enddo
016000040510     C*
016100040218     C                   if        vindta > *blanks AND
016200040510     C                             wFlgVINDTA = 1
016300010330     C                   add       1             rrnum
016400010330     C*
016500010601     C                   if        *in70 = *off and
016600010330     C                             (vinflg = *blanks
016700010330     C                              or vinflg = '0'
016800010330     C                              or vinflg = '2')
016900010330     C*
017000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
017100010711     C                   if        vinflg = *blanks or vinflg = '0'
017200010711     C                   clear                   vinmsg
017300010711     C                   endif
017400010601     C*
017500010330     C                   exsr      inzvar
017600090403     C*
017700090403     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
017800090403     C* separatori campi
017900090403     C                   if        CharTXT <> *blanks
018000090403     C                   z-add     *zeros        wLenVINDTA        4 0
018100090403     C                   z-add     *zeros        wFlgVINDTA        1 0
018200090403     C                   setoff                                       80
018300090403     C                   dow       wLenVINDTA < %len(%trim(vindta))
018400090403     C                   eval      wLenVINDTA = wLenVINDTA + 1
018500090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
018600090403     C                   if        not *in80
018700090403     C                   seton                                        80
018800090403     C                   else
018900090403     C                   setoff                                       80
019000090403     C                   endif
019100090403     C                   endif
019200090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
019300090403     C                             and *in80 = *on
019400090403     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
019500090403     C                   endif
019600090403     C                   enddo
019700090403     C                   endif
019800090403     C*
019900040708     C                   exsr      impvao
020000140730     C*
020100140730     C* Verifico se trattasi di formato ORM "esteso"
020200140730     C                   setoff                                       53
020300140730     C                   if        w_VAOFLO2  <> *blanks OR
020400140730     C                             w_VAOTSPS  <> *blanks OR
020500140730     C                             w_VAONOTE  <> *blanks OR
020600140730     C                             w_VAOORDA1 <> *blanks OR
020700140730     C                             w_VAOORAA1 <> *blanks OR
020800140730     C                             w_VAOORDA2 <> *blanks OR
020900140730     C                             w_VAOORAA2 <> *blanks OR
021000140730     C                             w_VAOEMLR  <> *blanks OR
021100140730     C                             w_VAOEMLRF <> *blanks OR
021200140730     C                             w_VAOSMSR  <> *blanks OR
021300140730     C                             w_VAOSMSRF <> *blanks OR
021400140730     C                             w_VAOORIP  <> *blanks OR
021500140730     C                             w_VAODOIO  <> *blanks
021600140730     C                   seton                                        53
021700140730     C                   exsr      EXEFORZ
021800140730     C                   endif
021900010601     C*
022000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022100101210     C                   if        not *IN31
022200040708     C                   eval      depfil = VAOPOE
022300130125     C***                exsr      repfil
022400130125     C***                if        depfil = invfil
022500021025     C                   if        vlrpoi = 999
022600130125     C***                move(P)   invfil        VAOPOE
022700021025     C                   else
022800040708     C                   move(P)   vlrpoi        VAOPOE
022900021025     C                   endif
023000130125     C***                endif
023100101210     C                   endif
023200010601     C*
023300010601     C                   exsr      PREELA
023400010601     C*
023500010604     C* Ebbene...
023600010604     C*
023700030822     C  N31              add       1             §CTROK            7 0
023800030822     C   32              add       1             §CTRMO            7 0
023900030822     C   31              add       1             §CTRNO            7 0
024000030822     C                   if        wGiro = 2
024100040723     C  N31              EXSR      CHKCOR
024200040723     C  N31              EXSR      WRIORI
024300050826     C  N31              EXSR      WRIVGD
024400040708     C  N31              write     FNVAO000
024500140730     C*
024600140730     C* Se richiesta scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
024700140730     C                   if        *in53 and not *in31
024800140730     C                   move      VAOPOE        w_vaopoe                       * forzatura necessar
024900140730     C                   move      VAOCOR        w_vaocor                       * forzatura necessar
025000140730     C                   move      VAONOR        w_vaonor                       * forzatura necessar
025100140730     C                   call      'TIS7T7ER'
025200140730     C                   parm      'W'           pIn_Opz           1
025300140730     C                   parm                    tivlrds
025400140730     C                   parm                    FNVAOEAR
025500140730     C                   parm                    oriPRG
025600140730     C                   parm      *zeros        pIn_Dst           3
025700140730     C                   parm      *blanks       pOut_Esito        1
025800140730     C                   endif
025900140730     C                   if        pOut_Esito = '2'
026000140730     C                   seton                                        32
026100140730     C                   endif
026200030822     C                   endif
026300010604     C*
026400010604     C                   if        *in31 = *off and
026500010604     C                             *in32 = *off
026600010604     C                   eval      vinflg = '1'
026700010604     C                   else
026800010604     C                   eval      vinflg = '2'
026900010604     C                   endif
027000010601     C*
027100010604     C                   endif
027200010604     C*
027300010330     C                   else
027400010330     C                   eval      vinflg = '1'
027500010330     C                   endif
027600010601     C*
027700010601     C  N70              update    tivin000
027800010330     C*
027900030822     C  N70              enddo
028000140730     C*
028100140730     C     FINE          tag
028200140730     C*
028300140730     C* Chiusura per scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
028400140730     C                   call      'TIS7T7ER'
028500140730     C                   parm      'C'           pIn_Opz           1
028600140730     C                   parm                    tivlrds
028700140730     C                   parm                    FNVAOEAR
028800140730     C                   parm                    oriPRG
028900140730     C                   parm      *zeros        pIn_Dst           3
029000140730     C                   parm      *blanks       pOut_Esito        1
029100010601     C
029200010601     C*
029300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
029400010601     C                   if        cntNonEl = *zeros or
029500010601     C                             flgMulti = '0'
029600010330     C* Se non ci sono record con errori ...
029700010601     C                   if        §ctrno = 0 and
029800010604     C                             §ctrmo = 0 and
029900010601     C                             flgStato <> '2'
030000010330     C* ... restituisco esito OK.
030100010330     C                   eval      wrkesito = '0'
030200010330     C                   else
030300010330     C                   if        §ctrok > 0
030400010330     C                   eval      wrkesito = '1'
030500010330     C                   else
030600010615     C                   if        flgOk = '0'
030700010615     C                   eval      wrkesito = '2'
030800010615     C                   else
030900010615     C                   eval      wrkesito = '6'
031000010615     C                   endif
031100010330     C                   endif
031200010330     C                   endif
031300010601     C                   else
031400010601     C                   eval      wrkesito = '9'
031500010601     C                   endif
031600010330     C*
031700010330     C                   if        %open(tivin00r)
031800010330     C                   close     tivin00r
031900010330     C                   endif
032000040708     C                   if        %open(fnvaowwr)
032100040708     C                   close     fnvaowwr
032200010330     C                   endif
032300010601     C*
032400010601     C                   if        vlrpoi <> 999
032500010601     C                   eval      invfil = vlrpoi
032600010601     C                   endif
032700101210     C*
032800101210     C                   if        invfil = 0 and
032900101210     C                             depfil > 0
033000101210     C                   eval      invfil = depfil
033100101210     C                   endif
033200010330     C*
033300010330     C                   if        §ctrok > 0
033400010601     C                             and invfil > *zeros
033500010330     C                   exsr      invio
033600140730     C*
033700140730     C* Invio dati estensione ORM (FNVAOE0F) => richiamo l'apposito driver
033800140730     C                   move      invfil        cmzdst
033900140730     C                   call      'TIS7T7ER'
034000140730     C                   parm      'S'           pIn_Opz           1
034100140730     C                   parm                    tivlrds
034200140730     C                   parm                    FNVAOEAR
034300140730     C                   parm                    oriPRG
034400140730     C                   parm      cmzdst        pIn_Dst           3
034500140730     C                   parm      *blanks       pOut_Esito        1
034600010330     C                   endif
034700010601     C*
034800010618     C                   if        flgGiro = '1'
034900010601     C                   exsr      endela
035000010618     C                   endif
035100010330     C*
035200010330     C                   ENDSR
035300010330     C***
035400140730
035500140730
035600140730     C*------------------------------------------------------------------------*
035700140730     C* EXEFORZ - Effettuo forzature
035800140730     C*------------------------------------------------------------------------*
035900140730     C     EXEFORZ       BEGSR
036000140730     C*
036100140730     C* Se necessario e VAONOR non è valorizzato stacco un numeratore da AZNUM
036200140730     C                   if        vaoNOR = *zeros
036300140730     C                   clear                   trul33ds
036400140730     C                   eval      I33OPE = *zeros
036500140730     C                   eval      I33CNU = 600
036600140730     C                   eval      I33NUM = 1
036700140730     C                   movel     TRUL33DS      KPJBU
036800140730     C                   call      'TRUL33R'
036900140730     C                   parm                    KPJBA
037000140730     C                   movel     KPJBU         TRUL33DS
037100140730     C                   if        O33ERR = *zeros
037200140730     C                   z-add     O33NRF        §numori
037300140730     c                   else
037400140730     c                   eval      §numori = *all'9'
037500140730     C                   seton                                        31        * errore bloccante
037600140730     C                   endif
037700140730     C*
037800140730     C                   eval      vaoNOR = §numori
037900140730     C                   endif
038000140730     C*
038100140730     C                   ENDSR
038200140730
038300050323
038400050323
038500050323     C*------------------------------------------------------------------------*
038600050323     C* CHKCOR - Considerazioni su VAOCOR
038700050323     C*------------------------------------------------------------------------*
038800050323     C     CHKCOR        BEGSR
038900050323     C*
039000050323     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
039100050323     C                   if        vaoTCO = 'F'
039200050323     C* Quindi verifico se il codice ordinante non è valorizzato
039300050323     C                   if        vaoCOR = *zeros
039400050323     C*                  if        vaoCOR = *zeros  AND
039500050323     C*                            vaoRSO = *blanks AND
039600050323     C*                            vaoINO = *blanks AND
039700050323     C*                            vaoCAO = *blanks AND
039800050323     C*                            vaoLOO = *blanks AND
039900050323     C*                            vaoPRO = *blanks AND
040000050323     C*                            vaoNAO = *blanks
040100050323     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
040200050323     C                   move      vlrKSC        wCli              7 0
040300050323     C                   movel(p)  wCli          acrCRO
040400050323     C     acrCRO        setll     fnacr01l
040500050323     C                   if        %found(fnacr01l)
040600050323     C                   read      fnacr01l
040700050323     C                   dow       not %eof(fnacr01l)
040800050323     C                   movel     acrCRO        wCliAcr           7 0
040900050323     C                   if        wCliAcr = wCli
041000050323     C                   eval      vaoCOR = acrCRO
041100050323     C                   leave
041200050323     C                   else
041300050323     C                   leave
041400050323     C                   endif
041500050323     C                   read      fnacr01l
041600050323     C                   enddo
041700050323     C                   endif
041800050323     C                   endif
041900050323     C                   endif
042000050323     C*
042100050323     C                   ENDSR
042200040723
042300040723
042400040723
042500040723     C*------------------------------------------------------------------------*
042600040723     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
042700040723     C*------------------------------------------------------------------------*
042800040723     C     WRIORI        BEGSR
042900040723     C*
043000090617     C* Come prima cosa stacco un numeratore da AZNUM
043100090617     C                   clear                   trul33ds
043200090617     C                   eval      I33OPE = *zeros
043300090617     C                   eval      I33CNU = 600
043400090617     C                   eval      I33NUM = 1
043500090617     C                   movel     TRUL33DS      KPJBU
043600090617     C                   call      'TRUL33R'
043700090617     C                   parm                    KPJBA
043800090617     C                   movel     KPJBU         TRUL33DS
043900090617     C                   if        O33ERR = *zeros
044000090617     C                   z-add     O33NRF        §numori
044100090618     c                   else
044200090618     c                   eval      §numori = *all'9'
044300090617     C                   endif
044400040723     C*
044500040723     C* Quindi imposto i campi "extra-VAO"
044600040723     C                   if        vlrKSC = '0ORM0000'
044700040723     C*
044800040723     C                   if        vaoCOR > *zeros
044900040723     C                   movel     vaoCOR        wrkCOR7           7 0
045000040723     C*
045100040723     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
045200040723     C* codice padre. Se è un padre assegno già il codice ordinante.
045300040723     C                   clear                   tibs10ds
045400040723     C                   eval      d10drf = datcor                              *data riferimento
045500040723     C                   eval      d10paf = 'P'                                 *cerca il padre
045600040723     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
045700091026     C*
045800091026     C* Preparazione chiamata al TIBS10R1
045900091026     C                   clear                   tibs10r1ds
046000091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
046100091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
046200091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
046300091026     C*
046400091026     C                   call      'TIBS10R1'
046500091026     C                   parm                    tibs10r1ds
046600091026     C*
046700091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
046800040723     C*
046900040723     C                   if        d10err <> *blanks or                         *No unif. o padre
047000040723     C                             d10cop =  *zeros  or
047100040723     C                             d10cop =  d10cod
047200040723     C                   move      d10cod        oriIDC
047300040723     C                   else                                                   *Figlio
047400040723     C                   move      d10cop        oriIDC
047500040723     C                   endif
047600040723     C*
047700040723     C                   else
047800040723     C                   eval      oriIDC = *zeros
047900040723     C                   endif
048000040723     C*
048100040723     C                   else
048200040723     C                   eval      oriIDC = vlrKSC
048300040723     C                   endif
048400040723     C*
048500040723     C                   eval      oriPRG = §numori
048600040723     C                   eval      oriDER = datcor
048700040723     C                   eval      oriOER = wn6
048800040723     C                   eval      oriPOE = vaoPOE
048900040723     C                   eval      oriNSR = vaoNSR
049000040723     C                   eval      oriNOR = vaoNOR
049100040723     C                   eval      oriNRV = vaoNRV
049200040723     C                   eval      oriFLO = *blanks
049300040723     C*
049400040723     C* Infine valorizzo la chiave esterna sul file FNVAO
049500040723     C                   eval      dorm01 = vaoFLO
049600040723     C                   movel     §numori       §ormpg
049700040723     C                   eval      vaoFLO = dorm01
049800040723     C*
049900040723     C* ...quindi scrivo il file TIORI00F
050000131008     C  N55              WRITE     TIORI000
050100040723     C*
050200040723     C                   ENDSR
050300050826
050400050826
050500050826
050600050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
050700050826     C     wriVGD        BEGSR
050800050826     C*
050900050826     C* Reperisco la descrizione della fase da tabella
051000050826     C                   movel(p)  'FAR'         KteCOD
051100050826     C                   movel(p)  '000'         KteKE1
051200050826     C     KEYtbe        chain     tntbe01l
051300050826     C                   if        %found(tntbe01l)
051400050826     C                   movel(P)  tbeuni        DFAR
051500050826     C                   else
051600050826     C                   clear                   DFAR
051700050826     C                   endif
051800050826     C*
051900050826     C* Valorizzo buffer record
052000050826     C                   clear                   FNVAPDS
052100050826     C                   eval      vapIDC = oriIDC
052200050826     C                   eval      vapPOE = oriPOE
052300050826     C                   eval      vapRFA = vaoRFA
052400050826     C                   eval      vapPOG = oriPOE
052500050826     C                   eval      vapDAE = oriDER
052600050826     C                   eval      vapORE = oriOER
052700050826     C                   if        d§fardva <> *blanks
052800050826     C                   eval      vapDFA = d§fardva
052900050826     C                   else
053000050826     C                   eval      vapDFA = d§fardes
053100050826     C                   endif
053200050826     C*
053300050826     C                   clear                   tivgd000
053400080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
053500050826     C                   eval      vgdTIP = 'VP'
053600050826     C                   eval      vgdKSU = vapIDC
053700050826     C                   eval      vgdTSC = 'WW'
053800050826     C                   eval      vgdDAT = datcor
053900050826     C                   eval      vgdPGM = 'TITVOXLR'
054000060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
054100060306     C                   exsr      sr_Control
054200060306     C                   if        Cli_Abil = *on
054300060306     C                   eval      vgdKSU = woriIDC
054400131008     C  N55              write     tivgd000
054500060306     C                   endif
054600050826     C*
054700050826     C                   ENDSR
054800050826     C*------------------------------------------------------------------------*
054900010601
055000010601
055100010601
055200010330     C*----------------------------------------------------*
055300030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
055400010330     C*----------------------------------------------------*
055500010330     C     INZVAR        BEGSR
055600010330     C*
055700030822     C* Inizializzo variabili di wrk
055800020204     C                   Z-ADD     *zeros        Num5_0            5 0
055900090403     C                   z-add     *zeros        invfil
056000030822     C*
056100030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
056200040708     C                   CLEAR                   FNVAO000
056300140730     C                   CLEAR                   FNVAOEAR
056400040723     C                   CLEAR                   TIORI000
056500030822     C                   CLEAR                   SkSplitCSV
056600030822     C                   EVAL      i = 1
056700030822     C                   EVAL      posDa = *zeros
056800030822     C                   EVAL      posA  = *zeros
056900150924     C*
057000150924     C* Forzatura campo FNVAO
057100150924     C                   EVAL      VAOTCO = 'F'
057200030822     C*
057300030822     C* Reimposto i valori di default
057400030822     C                   EXSR      DEFCAM
057500030822     C*
057600010330     C                   ENDSR
057700010330     C*----------------------------------------------------*
057800030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
057900010330     C*----------------------------------------------------*
058000010330     C     DEFCAM        BEGSR
058100030822     C*
058200030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
058300030715     C* e delimitatore testo.
058400030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
058500030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
058600030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
058700090403     C*
058800090403     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
058900090403     C                   EVAL      CharSOS = CharNUM
059000030822     C*
059100030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
059200030715     C                   IF        %subst(vlrppt:1:1) = 'S'
059300030715     C                   SETON                                        50
059400030715     C                   ELSE
059500030715     C                   SETOFF                                       50
059600131008     C                   ENDIF
059700131008     C* aggiungo la gestyione dei test (tanto la stamapa non c'è più)
059800131008     C                   SETOFF                                       55
059900131008     C                   IF        %subst(vlrppt:1:1) = 'T'
060000131008     C* inibisco la scrittura dei file TIORI e TIVGD
060100131008     C                   SETON                                        55
060200131008     C* imposto il PO di emissione come filiale fittizia
060300131008     C                   EVAL      VAOPOE = 999
060400131008     C                   ENDIF
060500030822     C*
060600030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
060700030822     C                   EVAL      posDaDft = 1
060800030822     C                   EVAL      posADft  = 0
060900030822     C                   EVAL      wGiroDft = 0
061000030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
061100030822     C                             posDaDft > 0
061200030822     C*
061300030822     C* Gestisco il 1° giro
061400030822     C                   IF        wGiroDft = 0
061500030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
061600030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
061700030822     C* Incremento il contatore dei "giri"
061800030822     C                   EVAL      wGiroDft = 1
061900030822     C                   ELSE
062000030822     C                   EVAL      posDaDft = posADft
062100030822     C                   ENDIF
062200030822     C* Eseguo lo scan x trovare la fine del campo corrente
062300030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
062400030822     C*
062500030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
062600030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
062700030822     C                   IF        posDaDft > 0 AND
062800030822     C                             posADft  > 0
062900040708     C* POE
063000040708     C                   IF        %subst(
063100040708     C                             %subst(vlrppt:posDaDft+1:
063200040708     C                             posADft-posDaDft-1):1:3)
063300040708     C                             = 'POE'
063400040708     C                   EVAL      PiStr=%trim(%subst(
063500040708     C                             %subst(vlrppt:posDaDft+1:
063600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063700040708     C                   EXSR      CHKNUM
063800040708     C                   IF        PiInt=*on
063900040708     C                   Z-ADD     PiVal         VAOPOE
064000040708     C                   ENDIF
064100040708     C                   ENDIF
064200040708     C* TOR
064300040708     C                   IF        %subst(
064400040708     C                             %subst(vlrppt:posDaDft+1:
064500040708     C                             posADft-posDaDft-1):1:3)
064600040708     C                             = 'TOR'
064700040708     C                   EVAL      VAOTOR=%trim(%subst(
064800040708     C                             %subst(vlrppt:posDaDft+1:
064900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065000040708     C                   ENDIF
065100040708     C* TCO
065200040708     C                   IF        %subst(
065300040708     C                             %subst(vlrppt:posDaDft+1:
065400040708     C                             posADft-posDaDft-1):1:3)
065500040708     C                             = 'TCO'
065600040708     C                   EVAL      VAOTCO=%trim(%subst(
065700040708     C                             %subst(vlrppt:posDaDft+1:
065800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065900040708     C                   ENDIF
066000040707     C* COR
066100030822     C                   IF        %subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):1:3)
066400040707     C                             = 'COR'
066500030822     C                   EVAL      PiStr=%trim(%subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066800030822     C                   EXSR      CHKNUM
066900030822     C                   IF        PiInt=*on
067000040708     C                   Z-ADD     PiVal         VAOCOR
067100030822     C                   ENDIF
067200030822     C                   ENDIF
067300040708     C* CRC
067400040708     C                   IF        %subst(
067500040708     C                             %subst(vlrppt:posDaDft+1:
067600040708     C                             posADft-posDaDft-1):1:3)
067700040708     C                             = 'CRC'
067800040708     C                   EVAL      PiStr=%trim(%subst(
067900040708     C                             %subst(vlrppt:posDaDft+1:
068000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068100040708     C                   EXSR      CHKNUM
068200040708     C                   IF        PiInt=*on
068300040708     C                   Z-ADD     PiVal         VAOCRC
068400040708     C                   ENDIF
068500040708     C                   ENDIF
068600040708     C* FFD
068700040708     C                   IF        %subst(
068800040708     C                             %subst(vlrppt:posDaDft+1:
068900040708     C                             posADft-posDaDft-1):1:3)
069000040708     C                             = 'FFD'
069100040708     C                   EVAL      VAOFFD=%trim(%subst(
069200040708     C                             %subst(vlrppt:posDaDft+1:
069300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069400040708     C                   ENDIF
069500040708     C* ORR
069600040708     C                   IF        %subst(
069700040708     C                             %subst(vlrppt:posDaDft+1:
069800040708     C                             posADft-posDaDft-1):1:3)
069900040708     C                             = 'ORR'
070000040708     C                   EVAL      PiStr=%trim(%subst(
070100040708     C                             %subst(vlrppt:posDaDft+1:
070200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070300040708     C                   EXSR      CHKNUM
070400040708     C                   IF        PiInt=*on
070500040708     C                   Z-ADD     PiVal         VAOORR
070600040708     C                   ENDIF
070700040708     C                   ENDIF
070800040712     C* NCL
070900040712     C                   IF        %subst(
071000040712     C                             %subst(vlrppt:posDaDft+1:
071100040712     C                             posADft-posDaDft-1):1:3)
071200040712     C                             = 'NCL'
071300040712     C                   EVAL      PiStr=%trim(%subst(
071400040712     C                             %subst(vlrppt:posDaDft+1:
071500040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071600040712     C                   EXSR      CHKNUM
071700040712     C                   IF        PiInt=*on
071800040712     C                   Z-ADD     PiVal         VAONCL
071900040712     C                   ENDIF
072000040712     C                   ENDIF
072100040712     C* PKG
072200040712     C                   IF        %subst(
072300040712     C                             %subst(vlrppt:posDaDft+1:
072400040712     C                             posADft-posDaDft-1):1:3)
072500040712     C                             = 'PKG'
072600040712     C                   EVAL      PiStr=%trim(%subst(
072700040712     C                             %subst(vlrppt:posDaDft+1:
072800040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072900040712     C                   EXSR      CHKNUM
073000040712     C                   IF        PiNum=*on
073100040712     C                   Z-ADD     PiVal         VAOPKG
073200040712     C                   ENDIF
073300040712     C                   ENDIF
073400040712     C* VLM
073500040712     C                   IF        %subst(
073600040712     C                             %subst(vlrppt:posDaDft+1:
073700040712     C                             posADft-posDaDft-1):1:3)
073800040712     C                             = 'VLM'
073900040712     C                   EVAL      PiStr=%trim(%subst(
074000040712     C                             %subst(vlrppt:posDaDft+1:
074100040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074200040712     C                   EXSR      CHKNUM
074300040712     C                   IF        PiNum=*on
074400040712     C                   Z-ADD     PiVal         VAOVLM
074500040712     C                   ENDIF
074600040712     C                   ENDIF
074700040708     C* NAM
074800040708     C                   IF        %subst(
074900040708     C                             %subst(vlrppt:posDaDft+1:
075000040708     C                             posADft-posDaDft-1):1:3)
075100040708     C                             = 'NAM'
075200040708     C                   EVAL      VAONAM=%trim(%subst(
075300040708     C                             %subst(vlrppt:posDaDft+1:
075400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075500040708     C                   ENDIF
075600040708     C* PAG
075700030822     C                   IF        %subst(
075800030822     C                             %subst(vlrppt:posDaDft+1:
075900030822     C                             posADft-posDaDft-1):1:3)
076000040708     C                             = 'PAG'
076100040708     C                   EVAL      VAOPAG=%trim(%subst(
076200030822     C                             %subst(vlrppt:posDaDft+1:
076300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076400030822     C                   ENDIF
076500040708     C* KSC
076600040708     C                   IF        %subst(
076700040708     C                             %subst(vlrppt:posDaDft+1:
076800040708     C                             posADft-posDaDft-1):1:3)
076900040708     C                             = 'KSC'
077000040708     C                   EVAL      PiStr=%trim(%subst(
077100040708     C                             %subst(vlrppt:posDaDft+1:
077200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077300040708     C                   EXSR      CHKNUM
077400040708     C                   IF        PiInt=*on
077500040708     C                   Z-ADD     PiVal         VAOKSC
077600040708     C                   ENDIF
077700040708     C                   ENDIF
077800040708     C* CTR
077900040708     C                   IF        %subst(
078000040708     C                             %subst(vlrppt:posDaDft+1:
078100040708     C                             posADft-posDaDft-1):1:3)
078200040708     C                             = 'CTR'
078300040708     C                   EVAL      VAOCTR=%trim(%subst(
078400040708     C                             %subst(vlrppt:posDaDft+1:
078500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078600040708     C                   ENDIF
078700040708     C* POC
078800040708     C                   IF        %subst(
078900040708     C                             %subst(vlrppt:posDaDft+1:
079000040708     C                             posADft-posDaDft-1):1:3)
079100040708     C                             = 'POC'
079200040708     C                   EVAL      PiStr=%trim(%subst(
079300040708     C                             %subst(vlrppt:posDaDft+1:
079400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079500040708     C                   EXSR      CHKNUM
079600040708     C                   IF        PiInt=*on
079700040708     C                   Z-ADD     PiVal         VAOPOC
079800040708     C                   ENDIF
079900040708     C                   ENDIF
080000040708     C* NO1
080100040708     C                   IF        %subst(
080200040708     C                             %subst(vlrppt:posDaDft+1:
080300040708     C                             posADft-posDaDft-1):1:3)
080400040708     C                             = 'NO1'
080500040708     C                   EVAL      VAONO1=%trim(%subst(
080600040708     C                             %subst(vlrppt:posDaDft+1:
080700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080800040708     C                   ENDIF
080900040708     C* DDT
081000030822     C                   IF        %subst(
081100030822     C                             %subst(vlrppt:posDaDft+1:
081200030822     C                             posADft-posDaDft-1):1:3)
081300040708     C                             = 'DDT'
081400040708     C                   EVAL      VAODDT=%trim(%subst(
081500030822     C                             %subst(vlrppt:posDaDft+1:
081600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081700030822     C                   ENDIF
081800040708     C* RFA
081900040708     C                   IF        %subst(
082000040708     C                             %subst(vlrppt:posDaDft+1:
082100040708     C                             posADft-posDaDft-1):1:3)
082200040708     C                             = 'RFA'
082300040708     C                   EVAL      VAORFA=%trim(%subst(
082400040708     C                             %subst(vlrppt:posDaDft+1:
082500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082600040708     C                   ENDIF
082700040708     C* FLO
082800040708     C                   IF        %subst(
082900040708     C                             %subst(vlrppt:posDaDft+1:
083000040708     C                             posADft-posDaDft-1):1:3)
083100040708     C                             = 'FLO'
083200040708     C                   EVAL      VAOFLO=%trim(%subst(
083300040708     C                             %subst(vlrppt:posDaDft+1:
083400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083500040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
083600040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
083700040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
083800040708     C                   ENDIF
083900030822     C* ...
084000030822     C                   ENDIF
084100030822     C                   ENDDO
084200020204     C*
084300010330     C                   ENDSR
084400010607     C*----------------------------------------------------*
084500040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
084600010607     C*----------------------------------------------------*
084700040708     C     IMPVAO        BEGSR
084800010607     C*
084900010607     C                   SETOFF                                       3132
085000030515     C*
085100040708     C                   MOVEL     datcor        VAODAO
085200040708     C                   MOVEL     wn14          VAOOAO
085300010607     C*
085400030715     C**********
085500030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
085600030715     C**********
085700030715     C                   IF        wGiro = *zeros
085800031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
085900031201     C     minu:maiu     XLATE     vindta        vindta
086000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
086100040324     C                   DOW       posDa <= %len(%trimr(vindta))
086200030715     C*
086300030715     C* Gestisco il 1° campo
086400030715     C                   IF        i = 1
086500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
086600030715     C                   EVAL      posDa = 1
086700030715     C* Eseguo lo scan x trovare la fine del primo campo
086800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086900030715     C* A questo punto "estraggo" il campo corrente
087000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087100030715     C                                                    (posA-posDa))
087200030715     C* X i campi successivi al 1°
087300030715     C                   ELSE
087400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
087500030715     C                   EVAL      posDa = posA + 1
087600030715     C* Eseguo lo scan x trovare la fine del campo corrente
087700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087800030715     C* Gestisco l'ultimo campo
087900030715     C                   IF        posA = *zeros
088000040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
088100030715     C* A questo punto "estraggo" il campo corrente
088200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088300030715     C                                                    (posA-posDa))
088400030715     C                   LEAVE
088500030715     C                   ELSE
088600030715     C* A questo punto "estraggo" il campo corrente
088700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088800030715     C                                                    (posA-posDa))
088900030715     C                   ENDIF
089000030715     C                   ENDIF
089100030715     C* Incremento il contatore d campo
089200030715     C                   EVAL      i = i +1
089300030715     C                   ENDDO
089400030715     C*
089500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
089600030715     C* delimitazione testo
089700030715     C                   EVAL      i = 1
089800030715     C                   DOW       i < %elem(SkSplitFLD)
089900030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
090000030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
090100030715     C                   EVAL      i = i + 1
090200030715     C                   ENDDO
090300030715     C                   EVAL      wGiro = 1
090400030715     C                   ELSE
090500030715     C                   EVAL      wGiro = 2
090600031204     C**********
090700031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
090800031204     C* d separatore campo
090900031204     C**********
091000040304     C                   IF        %subst(vindta:1:1) = CharCSV                 * all'inizio
091100040304     C                   EVAL      vindta = ' ' + vindta
091200040304     C                   ENDIF
091300040304     C***                DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
091400040304     C***                EVAL      vindta = %subst(vindta:2)
091500040304     C***                ENDDO
091600031204     C*
091700031204     C                   Z-ADD     *zeros        lunghInput        4 0
091800040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
091900040324     C                   IF        %subst(%trimr(vindta):lunghInput:1) = CharCSV
092000040304     C                   EVAL      vindta = vindta + ' '
092100040304     C                   ENDIF
092200040324     C***                DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
092300040324     C***                EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
092400040304     C***                EVAL      lunghInput = %len(%trim(vindta))
092500040304     C***                ENDDO
092600030715     C**********
092700030715     C* Effettuo lo split del campo dati d input
092800030715     C**********
092900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
093000040324     C                   DOW       posDa <= %len(%trimr(vindta))
093100030715     C*
093200030715     C* Gestisco il 1° campo
093300030715     C                   IF        i = 1
093400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
093500030715     C                   EVAL      posDa = 1
093600030715     C* Eseguo lo scan x trovare la fine del primo campo
093700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
093800030715     C* A questo punto "estraggo" il campo corrente
093900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094000030715     C                                                    (posA-posDa))
094100030715     C* X i campi successivi al 1°
094200030715     C                   ELSE
094300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
094400030715     C                   EVAL      posDa = posA + 1
094500031204     C* Verifico che nn vi sia il campo nn valorizzato
094600031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
094700031204     C* Se campo nn valorizzato skippo al prossimo
094800031204     C                   EVAL      SkSplitCSV(i) = *blanks
094900031204     C                   EVAL      posA  = posA + 1
095000031204     C                   ELSE
095100030715     C* Eseguo lo scan x trovare la fine del campo corrente
095200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
095300030715     C* Gestisco l'ultimo campo
095400030715     C                   IF        posA = *zeros
095500040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
095600030715     C* A questo punto "estraggo" il campo corrente
095700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095800030715     C                                                    (posA-posDa))
095900030715     C                   LEAVE
096000030715     C                   ELSE
096100030715     C* A questo punto "estraggo" il campo corrente
096200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096300030715     C                                                    (posA-posDa))
096400030715     C                   ENDIF
096500031204     C                   ENDIF
096600030715     C                   ENDIF
096700030715     C* Incremento il contatore d campo
096800030715     C                   EVAL      i = i +1
096900030715     C                   ENDDO
097000030715     C*
097100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
097200030715     C* delimitazione testo
097300030715     C                   EVAL      i = 1
097400030715     C                   DOW       i < %elem(SkSplitCSV)
097500030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
097600030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
097700030715     C                   EVAL      i = i + 1
097800030715     C                   ENDDO
097900030715     C*
098000030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
098100030715     C                   EVAL      i = 1
098200030715     C                   DOW       i < %elem(SkSplitFLD)
098300030715     C*
098400030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
098500030715     C                   IF        SkSplitFLD(i) <> *blanks AND
098600030715     C                             SkSplitCSV(i) <> *blanks
098700030721     C*** ATB
098800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATB'
098900040708     C                   EVAL      VAOATB = SkSplitCSV(i)
099000030721     C                   ENDIF
099100040708     C*** TOR
099200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTOR'
099300040708     C                   EVAL      VAOTOR = SkSplitCSV(i)
099400040708     C                   ENDIF
099500040708     C*** TCO
099600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTCO'
099700040708     C                   EVAL      VAOTCO = SkSplitCSV(i)
099800040708     C                   ENDIF
099900040708     C*** RSO
100000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSO'
100100040708     C                   EVAL      VAORSO = SkSplitCSV(i)
100200030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
100300040708     C     '@':'A'       XLATE     VAORSO        VAORSO
100400030715     C* ==
100500030715     C                   ENDIF
100600040708     C*** INO
100700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINO'
100800040708     C                   EVAL      VAOINO = SkSplitCSV(i)
100900030715     C                   ENDIF
101000040708     C*** LOO
101100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOO'
101200040708     C                   EVAL      VAOLOO = SkSplitCSV(i)
101300030715     C                   ENDIF
101400040708     C*** PRO
101500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRO'
101600040708     C                   EVAL      VAOPRO = SkSplitCSV(i)
101700030715     C                   ENDIF
101800040708     C*** NAO
101900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAO'
102000040708     C                   EVAL      VAONAO = SkSplitCSV(i)
102100030721     C                   ENDIF
102200040708     C*** RSR
102300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSR'
102400040708     C                   EVAL      VAORSR = SkSplitCSV(i)
102500040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
102600040708     C     '@':'A'       XLATE     VAORSR        VAORSR
102700040708     C* ==
102800040708     C                   IF        VAORSR = *blanks
102900040708     C                   SETON                                        31
103000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
103100040708     C                             + ' ' + 'VAORSR'
103200040708     C                   ENDIF
103300040708     C                   ENDIF
103400040708     C*** INR
103500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINR'
103600040708     C                   EVAL      VAOINR = SkSplitCSV(i)
103700040708     C                   IF        VAOINR = *blanks
103800040708     C                   SETON                                        31
103900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
104000040708     C                             + ' ' + 'VAOINR'
104100040708     C                   ENDIF
104200040708     C                   ENDIF
104300040708     C*** LOR
104400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOR'
104500040708     C                   EVAL      VAOLOR = SkSplitCSV(i)
104600040708     C                   IF        VAOLOR = *blanks
104700040708     C                   SETON                                        31
104800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
104900040708     C                             + ' ' + 'VAOLOR'
105000040708     C                   ENDIF
105100040708     C                   ENDIF
105200040708     C*** PRR
105300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRR'
105400040708     C                   EVAL      VAOPRR = SkSplitCSV(i)
105500040708     C                   IF        VAOPRR = *blanks
105600040708     C                   SETON                                        31
105700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
105800040708     C                             + ' ' + 'VAOPRR'
105900040708     C                   ENDIF
106000040708     C                   ENDIF
106100040708     C*** NAR
106200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAR'
106300040708     C                   EVAL      VAONAR = SkSplitCSV(i)
106400040708     C                   ENDIF
106500040708     C*** RER
106600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORER'
106700040708     C                   EVAL      VAORER = SkSplitCSV(i)
106800040708     C                   ENDIF
106900040708     C*** TER
107000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTER'
107100040708     C                   EVAL      VAOTER = SkSplitCSV(i)
107200040708     C                   ENDIF
107300040708     C*** RSC
107400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSC'
107500040708     C                   EVAL      VAORSC = SkSplitCSV(i)
107600040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
107700040708     C     '@':'A'       XLATE     VAORSC        VAORSC
107800040708     C* ==
107900040708     C                   ENDIF
108000040708     C*** INC
108100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINC'
108200040708     C                   EVAL      VAOINC = SkSplitCSV(i)
108300040708     C                   ENDIF
108400040708     C*** LOC
108500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOC'
108600040708     C                   EVAL      VAOLOC = SkSplitCSV(i)
108700040708     C                   ENDIF
108800040708     C*** PRC
108900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRC'
109000040708     C                   EVAL      VAOPRC = SkSplitCSV(i)
109100040708     C                   ENDIF
109200040708     C*** NAC
109300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAC'
109400040708     C                   EVAL      VAONAC = SkSplitCSV(i)
109500040708     C                   ENDIF
109600040708     C*** FFD
109700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFFD'
109800040708     C                   EVAL      VAOFFD = SkSplitCSV(i)
109900040708     C                   ENDIF
110000040708     C*** RMP
110100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORMP'
110200040708     C                   EVAL      VAORMP = SkSplitCSV(i)
110300040708     C                   ENDIF
110400040708     C*** NAM
110500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAM'
110600040708     C                   EVAL      VAONAM = SkSplitCSV(i)
110700040708     C                   ENDIF
110800040708     C*** PAG
110900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPAG'
111000040708     C                   EVAL      VAOPAG = SkSplitCSV(i)
111100040708     C                   ENDIF
111200040708     C*** CTR
111300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCTR'
111400040708     C                   EVAL      VAOCTR = SkSplitCSV(i)
111500040708     C                   ENDIF
111600040708     C*** ZOR
111700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOZOR'
111800040708     C                   EVAL      VAOZOR = SkSplitCSV(i)
111900040708     C                   ENDIF
112000040708     C*** NO1
112100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO1'
112200040708     C                   EVAL      VAONO1 = SkSplitCSV(i)
112300030715     C                   ENDIF
112400040708     C*** NO2
112500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO2'
112600040708     C                   EVAL      VAONO2 = SkSplitCSV(i)
112700030715     C                   ENDIF
112800040708     C*** DDT
112900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODDT'
113000040708     C                   EVAL      VAODDT = SkSplitCSV(i)
113100030715     C                   ENDIF
113200040708     C*** RFA
113300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORFA'
113400040708     C                   EVAL      VAORFA = SkSplitCSV(i)
113500030715     C                   ENDIF
113600040708     C*** SPI
113700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSPI'
113800040708     C                   EVAL      VAOSPI = SkSplitCSV(i)
113900030715     C                   ENDIF
114000040708     C*** FLO
114100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
114200040708     C                   EVAL      VAOFLO = SkSplitCSV(i)
114300040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
114400040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
114500040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
114600140730     C                   ENDIF
114700140730     C*** FLO2
114800140730     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO2'
114900140730     C                   EVAL      w_VAOFLO2 = SkSplitCSV(i)
115000140730     C                   ENDIF
115100140730     C*** TSPS
115200140730     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTSPS'
115300140730     C                   EVAL      w_VAOTSPS = SkSplitCSV(i)
115400140730     C                   ENDIF
115500140730     C*** NOTE
115600140730     C                   IF        %trim(SkSplitFLD(i)) = 'VAONOTE'
115700140730     C                   EVAL      w_VAONOTE = SkSplitCSV(i)
115800140730     C                   ENDIF
115900140730     C*** ORDA1
116000140730     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORDA1'
116100140730     C                   EVALR     w_VAOORDA1 = %trim(SkSplitCSV(i))
116200140730     C                   ENDIF
116300140730     C*** ORAA1
116400140730     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORAA1'
116500140730     C                   EVALR     w_VAOORAA1 = %trim(SkSplitCSV(i))
116600140730     C                   ENDIF
116700140730     C*** ORDA2
116800140730     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORDA2'
116900140730     C                   EVALR     w_VAOORDA2 = %trim(SkSplitCSV(i))
117000140730     C                   ENDIF
117100140730     C*** ORAA2
117200140730     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORAA2'
117300140730     C                   EVALR     w_VAOORAA2 = %trim(SkSplitCSV(i))
117400140730     C                   ENDIF
117500140730     C*** EMLR
117600140730     C                   IF        %trim(SkSplitFLD(i)) = 'VAOEMLR'
117700140730     C                   EVAL      w_VAOEMLR = SkSplitCSV(i)
117800140730     C                   ENDIF
117900140730     C*** EMLRF
118000140730     C                   IF        %trim(SkSplitFLD(i)) = 'VAOEMLRF'
118100140730     C                   EVAL      w_VAOEMLRF = SkSplitCSV(i)
118200140730     C                   ENDIF
118300140730     C*** SMSR
118400140730     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSMSR'
118500140730     C                   EVAL      w_VAOSMSR = SkSplitCSV(i)
118600140730     C                   ENDIF
118700140730     C*** SMSRF
118800140730     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSMSRF'
118900140730     C                   EVAL      w_VAOSMSRF = SkSplitCSV(i)
119000140730     C                   ENDIF
119100030715     C
119200030721     C*
119300030721     C* Reperisco quindi i campi numerici...
119400040708     C*** POE
119500040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
119600040723     C                   EVAL      PiStr=SkSplitCSV(i)
119700040723     C                   EXSR      CHKNUM
119800040723     C                   IF        PiInt=*on    AND
119900040723     C                             PiVal<=999   AND
120000040723     C                             PiVal>*zeros
120100040723     C                   Z-ADD     PiVal         VAOPOE
120200040723     C                   ELSE
120300040723     C                   SETON                                        32
120400040723     C                   EVAL      VAOPOE = *zeros
120500040723     C                   EVAL      vinmsg = %trimr(vinmsg)
120600040723     C                             + ' ' + 'VAOPOE'
120700040723     C                   ENDIF
120800040723     C                   ENDIF
120900040708     C*** COR
121000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
121100040708     C                   EVAL      PiStr=SkSplitCSV(i)
121200040708     C                   EXSR      CHKNUM
121300040708     C                   IF        PiInt=*on
121400040708     C                   Z-ADD     PiVal         VAOCOR
121500040708     C                   ELSE
121600040708     C                   SETON                                        31
121700040708     C                   EVAL      VAOCOR = *zeros
121800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
121900040708     C                             + ' ' + 'VAOCOR'
122000040708     C                   ENDIF
122100040708     C                   ENDIF
122200040708     C*** CAO
122300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
122400130502     C                   EVAL      VAOCAO = SkSplitCSV(i)
122500130502     C***                EVAL      PiStr=SkSplitCSV(i)
122600130502     C***                EXSR      CHKNUM
122700130502     C***                IF        PiInt=*on
122800130502     C***                Z-ADD     PiVal         Num5_0
122900130502     C***                MOVEL(p)  Num5_0        VAOCAO
123000130502     C***                ELSE
123100130502     C***                SETON                                        32
123200130502     C***                EVAL      VAOCAO = *zeros
123300130502     C***                EVAL      vinmsg = %trimr(vinmsg)
123400130502     C***                          + ' ' + 'VAOCAO'
123500130502     C***                ENDIF
123600030721     C                   ENDIF
123700040708     C*** CRA
123800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
123900030721     C                   EVAL      PiStr=SkSplitCSV(i)
124000030721     C                   EXSR      CHKNUM
124100030721     C                   IF        PiInt=*on
124200040708     C                   Z-ADD     PiVal         VAOCRA
124300030721     C                   ELSE
124400030721     C                   SETON                                        32
124500040708     C                   EVAL      VAOCRA = *zeros
124600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124700040708     C                             + ' ' + 'VAOCRA'
124800030721     C                   ENDIF
124900030721     C                   ENDIF
125000040708     C*** CAR
125100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
125200130502     C                   EVAL      VAOCAR = SkSplitCSV(i)
125300130502     C***                EVAL      PiStr=SkSplitCSV(i)
125400130502     C***                EXSR      CHKNUM
125500130502     C***                IF        PiInt=*on
125600130502     C***                Z-ADD     PiVal         Num5_0
125700130502     C***                MOVEL(p)  Num5_0        VAOCAR
125800130502     C***                ELSE
125900130502     C***                SETON                                        31
126000130502     C***                EVAL      VAOCAR = *zeros
126100130502     C***                EVAL      vinmsg = %trimr(vinmsg)
126200130502     C***                          + ' ' + 'VAOCAR'
126300130502     C***                ENDIF
126400040708     C                   ENDIF
126500040708     C*** CRC
126600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
126700040708     C                   EVAL      PiStr=SkSplitCSV(i)
126800040708     C                   EXSR      CHKNUM
126900040708     C                   IF        PiInt=*on
127000040708     C                   Z-ADD     PiVal         VAOCRC
127100040708     C                   ELSE
127200040708     C                   SETON                                        32
127300040708     C                   EVAL      VAOCRC = *zeros
127400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
127500040708     C                             + ' ' + 'VAOCRC'
127600040708     C                   ENDIF
127700040708     C                   ENDIF
127800040708     C*** CAC
127900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
128000130502     C                   EVAL      VAOCAC = SkSplitCSV(i)
128100130502     C***                EVAL      PiStr=SkSplitCSV(i)
128200130502     C***                EXSR      CHKNUM
128300130502     C***                IF        PiInt=*on
128400130502     C***                Z-ADD     PiVal         Num5_0
128500130502     C***                MOVEL(p)  Num5_0        VAOCAC
128600130502     C***                ELSE
128700130502     C***                SETON                                        32
128800130502     C***                EVAL      VAOCAC = *zeros
128900130502     C***                EVAL      vinmsg = %trimr(vinmsg)
129000130502     C***                          + ' ' + 'VAOCAC'
129100130502     C***                ENDIF
129200040708     C                   ENDIF
129300040708     C*** DAR
129400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
129500040708     C                   EVAL      PiStr=SkSplitCSV(i)
129600040708     C                   EXSR      CHKNUM
129700040708     C                   IF        PiInt=*on
129800040708     C                   Z-ADD     PiVal         VAODAR
129900040708     C                   ELSE
130000040708     C                   SETON                                        32
130100040708     C                   EVAL      VAODAR = *zeros
130200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
130300040708     C                             + ' ' + 'VAODAR'
130400040708     C                   ENDIF
130500040708     C                   ENDIF
130600040708     C*** ORR
130700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
130800040708     C                   EVAL      PiStr=SkSplitCSV(i)
130900040708     C                   EXSR      CHKNUM
131000040708     C                   IF        PiInt=*on
131100040708     C                   Z-ADD     PiVal         VAOORR
131200040708     C                   ELSE
131300040708     C                   SETON                                        32
131400040708     C                   EVAL      VAOORR = *zeros
131500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
131600040708     C                             + ' ' + 'VAOORR'
131700040708     C                   ENDIF
131800040708     C                   ENDIF
131900030721     C*** NCL
132000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
132100030721     C                   EVAL      PiStr=SkSplitCSV(i)
132200030721     C                   EXSR      CHKNUM
132300030721     C                   IF        PiInt=*on
132400040708     C                   Z-ADD     PiVal         VAONCL
132500030721     C                   ELSE
132600030721     C                   SETON                                        32
132700040708     C                   EVAL      VAONCL = *zeros
132800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132900040708     C                             + ' ' + 'VAONCL'
133000030721     C                   ENDIF
133100030721     C                   ENDIF
133200040708     C*** PKG
133300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
133400030721     C                   EVAL      PiStr=SkSplitCSV(i)
133500030721     C                   EXSR      CHKNUM
133600030721     C                   IF        PiNum=*on
133700040708     C                   Z-ADD     PiVal         VAOPKG
133800030721     C                   ELSE
133900030721     C                   SETON                                        32
134000040708     C                   EVAL      VAOPKG = *zeros
134100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134200040708     C                             + ' ' + 'VAOPKG'
134300030721     C                   ENDIF
134400030721     C                   ENDIF
134500040708     C*** VLM
134600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
134700040708     C                   EVAL      PiStr=SkSplitCSV(i)
134800040708     C                   EXSR      CHKNUM
134900040708     C                   IF        PiNum=*on
135000040708     C                   Z-ADD     PiVal         VAOVLM
135100040708     C                   ELSE
135200040708     C                   SETON                                        32
135300040708     C                   EVAL      VAOVLM = *zeros
135400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
135500040708     C                             + ' ' + 'VAOVLM'
135600040708     C                   ENDIF
135700040708     C                   ENDIF
135800040708     C*** BNC
135900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
136000040708     C                   EVAL      PiStr=SkSplitCSV(i)
136100040708     C                   EXSR      CHKNUM
136200040708     C                   IF        PiInt=*on
136300040708     C                   Z-ADD     PiVal         VAOBNC
136400040708     C                   ELSE
136500040708     C                   SETON                                        32
136600040708     C                   EVAL      VAOBNC = *zeros
136700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
136800040708     C                             + ' ' + 'VAOBNC'
136900040708     C                   ENDIF
137000040708     C                   ENDIF
137100040708     C*** BLC
137200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
137300040708     C                   EVAL      PiStr=SkSplitCSV(i)
137400040708     C                   EXSR      CHKNUM
137500040708     C                   IF        PiInt=*on
137600040708     C                   Z-ADD     PiVal         VAOBLC
137700040708     C                   ELSE
137800040708     C                   SETON                                        32
137900040708     C                   EVAL      VAOBLC = *zeros
138000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
138100040708     C                             + ' ' + 'VAOBLC'
138200040708     C                   ENDIF
138300040708     C                   ENDIF
138400040708     C*** ATT
138500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
138600040708     C                   EVAL      PiStr=SkSplitCSV(i)
138700040708     C                   EXSR      CHKNUM
138800040708     C                   IF        PiInt=*on
138900040708     C                   Z-ADD     PiVal         VAOATT
139000040708     C                   ELSE
139100040708     C                   SETON                                        32
139200040708     C                   EVAL      VAOATT = *zeros
139300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
139400040708     C                             + ' ' + 'VAOATT'
139500040708     C                   ENDIF
139600040708     C                   ENDIF
139700040708     C*** MTC
139800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
139900040708     C                   EVAL      PiStr=SkSplitCSV(i)
140000040708     C                   EXSR      CHKNUM
140100040708     C                   IF        PiInt=*on
140200040708     C                   Z-ADD     PiVal         VAOMTC
140300040708     C                   ELSE
140400040708     C                   SETON                                        32
140500040708     C                   EVAL      VAOMTC = *zeros
140600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
140700040708     C                             + ' ' + 'VAOMTC'
140800040708     C                   ENDIF
140900040708     C                   ENDIF
141000040708     C*** KSC
141100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
141200030721     C                   EVAL      PiStr=SkSplitCSV(i)
141300030721     C                   EXSR      CHKNUM
141400030721     C                   IF        PiInt=*on      AND
141500030721     C                             PiVal<=9999999 AND
141600030721     C                             PiVal>*zeros
141700040708     C                   Z-ADD     PiVal         VAOKSC
141800030721     C                   ELSE
141900030721     C                   SETON                                        32
142000040708     C                   EVAL      VAOKSC = *zeros
142100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142200040708     C                             + ' ' + 'VAOKSC'
142300030721     C                   ENDIF
142400030721     C                   ENDIF
142500040708     C*** POR
142600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
142700030721     C                   EVAL      PiStr=SkSplitCSV(i)
142800030721     C                   EXSR      CHKNUM
142900030721     C                   IF        PiInt=*on    AND
143000030721     C                             PiVal<=999   AND
143100030721     C                             PiVal>*zeros
143200040708     C                   Z-ADD     PiVal         VAOPOR
143300030721     C                   ELSE
143400030721     C                   SETON                                        32
143500040708     C                   EVAL      VAOPOR = *zeros
143600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
143700040708     C                             + ' ' + 'VAOPOR'
143800030721     C                   ENDIF
143900030721     C                   ENDIF
144000040708     C*** POC
144100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
144200040708     C                   EVAL      PiStr=SkSplitCSV(i)
144300040708     C                   EXSR      CHKNUM
144400040708     C                   IF        PiInt=*on    AND
144500040708     C                             PiVal<=999   AND
144600040708     C                             PiVal>*zeros
144700040708     C                   Z-ADD     PiVal         VAOPOC
144800040708     C                   ELSE
144900040708     C                   SETON                                        32
145000040708     C                   EVAL      VAOPOC = *zeros
145100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
145200040708     C                             + ' ' + 'VAOPOC'
145300040708     C                   ENDIF
145400040708     C                   ENDIF
145500040708     C*** STO
145600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
145700040708     C                   EVAL      PiStr=SkSplitCSV(i)
145800040708     C                   EXSR      CHKNUM
145900040708     C                   IF        PiInt=*on
146000040708     C                   Z-ADD     PiVal         VAOSTO
146100040708     C                   ELSE
146200040708     C                   SETON                                        32
146300040708     C                   EVAL      VAOSTO = *zeros
146400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
146500040708     C                             + ' ' + 'VAOSTO'
146600040708     C                   ENDIF
146700040708     C                   ENDIF
146800030715     C*
146900030715     C                   ENDIF
147000030715     C                   EVAL      i = i + 1
147100030715     C                   ENDDO
147200101210     C*
147300101210     C* Se al termine del parsing il POE e/o il COR nn risultano valorizzato => errore
147400101210     C                   IF        VAOPOE = *zeros OR
147500101210     C                             VAOCOR = *zeros
147600101210     C                   SETON                                        31
147700101210     C                   ENDIF
147800020204     C*
147900030715     C                   ENDIF
148000020204     C*
148100010607     C                   ENDSR
148200010607     C*----------------------------------------------------*
148300020204
148400010330
148500010330
148600010330     C*----------------------------------------------------*
148700010330     C*  CONTROLLO NUMERICITA' CAMPI
148800010330     C*----------------------------------------------------*
148900010330     C     CHKNUM        BEGSR
149000010330     C*
149100010606     C                   IF        PiDecChr = *blanks
149200030715     C                   EVAL      PiDecChr = CharNUM
149300010606     C                   ENDIF
149400010606     C*
149500010606     C                   CALL(e)   'ISNUMERIC'
149600010330     C                   PARM                    PiStr            30
149700010606     C                   PARM                    PiDecChr          1
149800010330     C                   PARM      *ZEROS        PiVal            30 9
149900010330     C                   PARM      '0'           PiInt             1
150000010330     C                   PARM      '0'           PiNum             1
150100010330     C                   IF        %error
150200010606     C                   EVAL      PiNum=*off
150300010330     C                   ENDIF
150400010330     C*
150500010330     C                   ENDSR
150600010330     C***
150700010330
150800010601
150900010601
151000010601
151100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
151200130125     C***  repfil        BEGSR
151300010601     C*
151400130125     C***                if        invfil = *zeros and
151500130125     C***                          depfil > *zeros and
151600130125     C***                          (vinflg = *blanks or
151700130125     C***                           vinflg = *zeros)
151800130125     C***
151900130125     C***                eval      invfil = depfil
152000130125     C***                endif
152100010601     C*
152200130125     C***                if        depfil <> invfil and
152300130125     C***                          invfil > *zeros
152400130125     C***                eval      flgMulti = '1'
152500130125     C***                if        vinflg = *blanks
152600130125     C***                add       1             cntNonEl
152700130125     C***                endif
152800130125     C***                endif
152900010601     C*
153000130125     C***                if        vinflg = '2'
153100130125     C***                eval      flgStato = '2'
153200130125     C***                endif
153300010601     C*
153400130125     C***                ENDSR
153500010601     C***
153600010601
153700010601
153800010601
153900010330
154000010330
154100010330
154200990920      /TITLE Invio dei dati al punto operativo.
154300000613     C     invio         BEGSR
154400990920     C*
154500990920     C                   reset                   dscmz
154600010601     C                   move      invfil        cmzdst
154700990920     C                   eval      cmzfld = vlrfou
154800990920     C                   eval      cmzmbd = vlrhdl
154900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
155000000710     C                   if        prmfir = *blanks
155100040708     C                   eval      cmzfla = 'FNVAO00F'
155200040708     C                   eval      cmzmba = 'FNVAO00F'
155300000710     C                   else
155400000710     C                   eval      cmzfla = prmfir
155500000710     C                   eval      cmzmba = prmfir
155600000710     C                   endif
155700990920     C                   eval      cmznrr = *zeros
155800990920     C                   move      §ctrok        cmznrr
155900021018     C                   eval      cmzlba = vlrfl1
156000990920     C                   call(e)   'TIS711C'
156100990920     C                   parm                    dscmz
156200990921     C                   parm      *blanks       esito
156300990920     C                   if        %error
156400990920     C                             or cmzerr = '1'
156500990921     C                             or esito  = '1'
156600000710     C                   eval      wrkesito = '3'
156700990920     C                   endif
156800990920     C*
156900000613     C                   ENDSR
157000990910
157100010601
157200010601
157300010601
157400010601
157500010601      /TITLE Invio dei dati al punto operativo.
157600010601     C     opeini        BEGSR
157700010601     C*
157800010601     C* Inizializzo flag e contatori operativi
157900010601     C                   movel     '0'           flgGiro           1
158000010601     C                   movel     '0'           flgMulti          1
158100010601     C                   movel     '1'           flgStato          1
158200010615     C                   movel     '0'           flgOk             1
158300010601     C                   z-add     *zeros        cntNonEl         10 0
158400010601     C                   z-add     *zeros        depfil            3 0
158500010601     C                   z-add     *zeros        invfil            3 0
158600010601     C*
158700010601     C                   ENDSR
158800010601     C***
158900050421
159000050421
159100050421
159200050421     C     *pssr         BEGSR
159300050421     C*
159400050421     C                   eval      wrkesito = '2'
159500050421     C*
159600050421     C                   ENDSR     '*CANCL'
159700050421     C***
159800060306
159900060306
160000060306     C*-------------------------------------------------------------------------*
160100060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
160200060306     C*-------------------------------------------------------------------------*
160300060306     C     sr_Control    begsr
160400060306     C*
160500060306     C* inizializzo a ok il flag d controllo abilitazione
160600060306     C                   eval      Cli_Abil = *on
160700060306     C
160800060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
160900060306     C                   clear                   woriIDC
161000060306     C                   eval      kteCOD = 'VAP'
161100060306     C                   eval      kteKE1 = oriIDC
161200060306     C     keytbe        chain     TNTBE01L
161300060306     C                   if        %found(Tntbe01l)
161400060306     C                   eval      woriIDC = TbeKe2
161500060306     C                   else
161600060306     C                   eval      Cli_Abil = *off
161700060306     C                   endif
161800060306     C*
161900060306     C                   endsr
162000010601
162100010601
162200010601
162300010330
162400010330
162500000613     C     *inzsr        BEGSR
162600990910     C*
162700990910     C     *entry        plist
162800990920     C                   parm                    tivlrds
162900990921     C                   parm      wrkesito      esito
163000000724     C                   parm                    prmlit
163100000710     C                   parm                    prmfir
163200010330     C*
163300010330     C* CALCOLA LA DATA CORRENTE
163400010330     C                   time                    wn14             14 0
163500040723     C                   movel     wn14          wn6               6 0          *ORA (6)
163600130125     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
163700130125     C                   eval      datcor = %dec(%date() : *ISO)
163800040723     C*
163900040723     C* Campi riferito al database
164000040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
164100040723     C     *like         define    tbeke1        kteKE1
164200040723     C*
164300040723     C* Definizione chiavi di lettura
164400040723     C* TNTBE01L - parziale
164500040723     C     keytbe        klist
164600040723     C                   kfld                    kteCOD                         *UTENTE
164700040723     C                   kfld                    kteKE1                         *CODICE
164800000613     C*
164900000613     C                   ENDSR
165000000613     C***
