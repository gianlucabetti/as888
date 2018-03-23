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
020000151019     C*
020100151019     C* Stacco subito un numeratore univoco "ORM"
020200151019     C                   exsr      RTVNUM
020300140730     C*
020400140730     C* Verifico se trattasi di formato ORM "esteso"
020500140730     C                   setoff                                       53
020600140730     C                   if        w_VAOFLO2  <> *blanks OR
020700140730     C                             w_VAOTSPS  <> *blanks OR
020800140730     C                             w_VAONOTE  <> *blanks OR
020900140730     C                             w_VAOORDA1 <> *blanks OR
021000140730     C                             w_VAOORAA1 <> *blanks OR
021100140730     C                             w_VAOORDA2 <> *blanks OR
021200140730     C                             w_VAOORAA2 <> *blanks OR
021300140730     C                             w_VAOEMLR  <> *blanks OR
021400140730     C                             w_VAOEMLRF <> *blanks OR
021500140730     C                             w_VAOSMSR  <> *blanks OR
021600140730     C                             w_VAOSMSRF <> *blanks OR
021700140730     C                             w_VAOORIP  <> *blanks OR
021800140730     C                             w_VAODOIO  <> *blanks
021900140730     C                   seton                                        53
022000140730     C                   endif
022100010601     C*
022200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022300101210     C                   if        not *IN31
022400040708     C                   eval      depfil = VAOPOE
022500130125     C***                exsr      repfil
022600130125     C***                if        depfil = invfil
022700021025     C                   if        vlrpoi = 999
022800130125     C***                move(P)   invfil        VAOPOE
022900021025     C                   else
023000040708     C                   move(P)   vlrpoi        VAOPOE
023100021025     C                   endif
023200130125     C***                endif
023300101210     C                   endif
023400010601     C*
023500010601     C                   exsr      PREELA
023600010601     C*
023700010604     C* Ebbene...
023800010604     C*
023900030822     C  N31              add       1             §CTROK            7 0
024000030822     C   32              add       1             §CTRMO            7 0
024100030822     C   31              add       1             §CTRNO            7 0
024200030822     C                   if        wGiro = 2
024300040723     C  N31              EXSR      CHKCOR
024400040723     C  N31              EXSR      WRIORI
024500050826     C  N31              EXSR      WRIVGD
024600040708     C  N31              write     FNVAO000
024700140730     C*
024800140730     C* Se richiesta scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
024900140730     C                   if        *in53 and not *in31
025000151019     C                   exsr      EXEFORZ
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
035400151019
035500151019
035600151019     C*------------------------------------------------------------------------*
035700151019     C* RTVNUM - Stacco un numeratore univoco "ORM"
035800151019     C*------------------------------------------------------------------------*
035900151019     C     RTVNUM        BEGSR
036000151019     C*
036100151019     C* Stacco un numeratore da AZNUM
036200151019     C                   clear                   §numori
036300151019     C                   clear                   trul33ds
036400151019     C                   eval      I33OPE = *zeros
036500151019     C                   eval      I33CNU = 600
036600151019     C                   eval      I33NUM = 1
036700151019     C                   movel     TRUL33DS      KPJBU
036800151019     C                   call      'TRUL33R'
036900151019     C                   parm                    KPJBA
037000151019     C                   movel     KPJBU         TRUL33DS
037100151019     C                   if        O33ERR = *zeros
037200151019     C                   z-add     O33NRF        §numori
037300151019     c                   else
037400151019     c                   eval      §numori = *all'9'
037500151019     C                   seton                                        31        * errore bloccante
037600151019     C                   endif
037700151019     C*
037800151019     C                   eval      vaoNOR = §numori
037900151019     C*
038000151019     C                   ENDSR
038100140730
038200140730
038300140730     C*------------------------------------------------------------------------*
038400140730     C* EXEFORZ - Effettuo forzature
038500140730     C*------------------------------------------------------------------------*
038600140730     C     EXEFORZ       BEGSR
038700151019     C*
038800151019     C                   move(p)   VAOPOE        w_vaopoe                       * forzatura necessar
038900151019     C                   move(p)   VAOCOR        w_vaocor                       * forzatura necessar
039000151019     C                   move(p)   VAONOR        w_vaonor                       * forzatura necessar
039100140730     C*
039200140730     C                   ENDSR
039300140730
039400050323
039500050323
039600050323     C*------------------------------------------------------------------------*
039700050323     C* CHKCOR - Considerazioni su VAOCOR
039800050323     C*------------------------------------------------------------------------*
039900050323     C     CHKCOR        BEGSR
040000050323     C*
040100050323     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
040200050323     C                   if        vaoTCO = 'F'
040300050323     C* Quindi verifico se il codice ordinante non è valorizzato
040400050323     C                   if        vaoCOR = *zeros
040500050323     C*                  if        vaoCOR = *zeros  AND
040600050323     C*                            vaoRSO = *blanks AND
040700050323     C*                            vaoINO = *blanks AND
040800050323     C*                            vaoCAO = *blanks AND
040900050323     C*                            vaoLOO = *blanks AND
041000050323     C*                            vaoPRO = *blanks AND
041100050323     C*                            vaoNAO = *blanks
041200050323     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
041300050323     C                   move      vlrKSC        wCli              7 0
041400050323     C                   movel(p)  wCli          acrCRO
041500050323     C     acrCRO        setll     fnacr01l
041600050323     C                   if        %found(fnacr01l)
041700050323     C                   read      fnacr01l
041800050323     C                   dow       not %eof(fnacr01l)
041900050323     C                   movel     acrCRO        wCliAcr           7 0
042000050323     C                   if        wCliAcr = wCli
042100050323     C                   eval      vaoCOR = acrCRO
042200050323     C                   leave
042300050323     C                   else
042400050323     C                   leave
042500050323     C                   endif
042600050323     C                   read      fnacr01l
042700050323     C                   enddo
042800050323     C                   endif
042900050323     C                   endif
043000050323     C                   endif
043100050323     C*
043200050323     C                   ENDSR
043300040723
043400040723
043500040723
043600040723     C*------------------------------------------------------------------------*
043700040723     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
043800040723     C*------------------------------------------------------------------------*
043900040723     C     WRIORI        BEGSR
044000040723     C*
044100090617     C* Come prima cosa stacco un numeratore da AZNUM
044200151019     C***                clear                   trul33ds
044300151019     C***                eval      I33OPE = *zeros
044400151019     C***                eval      I33CNU = 600
044500151019     C***                eval      I33NUM = 1
044600151019     C***                movel     TRUL33DS      KPJBU
044700151019     C***                call      'TRUL33R'
044800151019     C***                parm                    KPJBA
044900151019     C***                movel     KPJBU         TRUL33DS
045000151019     C***                if        O33ERR = *zeros
045100151019     C***                z-add     O33NRF        §numori
045200151019     c***                else
045300151019     c***                eval      §numori = *all'9'
045400151019     C***                endif
045500040723     C*
045600040723     C* Quindi imposto i campi "extra-VAO"
045700040723     C                   if        vlrKSC = '0ORM0000'
045800040723     C*
045900040723     C                   if        vaoCOR > *zeros
046000040723     C                   movel     vaoCOR        wrkCOR7           7 0
046100040723     C*
046200040723     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
046300040723     C* codice padre. Se è un padre assegno già il codice ordinante.
046400040723     C                   clear                   tibs10ds
046500040723     C                   eval      d10drf = datcor                              *data riferimento
046600040723     C                   eval      d10paf = 'P'                                 *cerca il padre
046700040723     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
046800091026     C*
046900091026     C* Preparazione chiamata al TIBS10R1
047000091026     C                   clear                   tibs10r1ds
047100091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
047200091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
047300091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
047400091026     C*
047500091026     C                   call      'TIBS10R1'
047600091026     C                   parm                    tibs10r1ds
047700091026     C*
047800091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
047900040723     C*
048000040723     C                   if        d10err <> *blanks or                         *No unif. o padre
048100040723     C                             d10cop =  *zeros  or
048200040723     C                             d10cop =  d10cod
048300040723     C                   move      d10cod        oriIDC
048400040723     C                   else                                                   *Figlio
048500040723     C                   move      d10cop        oriIDC
048600040723     C                   endif
048700040723     C*
048800040723     C                   else
048900040723     C                   eval      oriIDC = *zeros
049000040723     C                   endif
049100040723     C*
049200040723     C                   else
049300040723     C                   eval      oriIDC = vlrKSC
049400040723     C                   endif
049500040723     C*
049600040723     C                   eval      oriPRG = §numori
049700040723     C                   eval      oriDER = datcor
049800040723     C                   eval      oriOER = wn6
049900040723     C                   eval      oriPOE = vaoPOE
050000040723     C                   eval      oriNSR = vaoNSR
050100040723     C                   eval      oriNOR = vaoNOR
050200040723     C                   eval      oriNRV = vaoNRV
050300040723     C                   eval      oriFLO = *blanks
050400040723     C*
050500040723     C* Infine valorizzo la chiave esterna sul file FNVAO
050600040723     C                   eval      dorm01 = vaoFLO
050700040723     C                   movel     §numori       §ormpg
050800040723     C                   eval      vaoFLO = dorm01
050900040723     C*
051000040723     C* ...quindi scrivo il file TIORI00F
051100131008     C  N55              WRITE     TIORI000
051200040723     C*
051300040723     C                   ENDSR
051400050826
051500050826
051600050826
051700050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
051800050826     C     wriVGD        BEGSR
051900050826     C*
052000050826     C* Reperisco la descrizione della fase da tabella
052100050826     C                   movel(p)  'FAR'         KteCOD
052200050826     C                   movel(p)  '000'         KteKE1
052300050826     C     KEYtbe        chain     tntbe01l
052400050826     C                   if        %found(tntbe01l)
052500050826     C                   movel(P)  tbeuni        DFAR
052600050826     C                   else
052700050826     C                   clear                   DFAR
052800050826     C                   endif
052900050826     C*
053000050826     C* Valorizzo buffer record
053100050826     C                   clear                   FNVAPDS
053200050826     C                   eval      vapIDC = oriIDC
053300050826     C                   eval      vapPOE = oriPOE
053400050826     C                   eval      vapRFA = vaoRFA
053500050826     C                   eval      vapPOG = oriPOE
053600050826     C                   eval      vapDAE = oriDER
053700050826     C                   eval      vapORE = oriOER
053800050826     C                   if        d§fardva <> *blanks
053900050826     C                   eval      vapDFA = d§fardva
054000050826     C                   else
054100050826     C                   eval      vapDFA = d§fardes
054200050826     C                   endif
054300050826     C*
054400050826     C                   clear                   tivgd000
054500080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
054600050826     C                   eval      vgdTIP = 'VP'
054700050826     C                   eval      vgdKSU = vapIDC
054800050826     C                   eval      vgdTSC = 'WW'
054900050826     C                   eval      vgdDAT = datcor
055000050826     C                   eval      vgdPGM = 'TITVOXLR'
055100060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
055200060306     C                   exsr      sr_Control
055300060306     C                   if        Cli_Abil = *on
055400060306     C                   eval      vgdKSU = woriIDC
055500131008     C  N55              write     tivgd000
055600060306     C                   endif
055700050826     C*
055800050826     C                   ENDSR
055900050826     C*------------------------------------------------------------------------*
056000010601
056100010601
056200010601
056300010330     C*----------------------------------------------------*
056400030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
056500010330     C*----------------------------------------------------*
056600010330     C     INZVAR        BEGSR
056700010330     C*
056800030822     C* Inizializzo variabili di wrk
056900020204     C                   Z-ADD     *zeros        Num5_0            5 0
057000090403     C                   z-add     *zeros        invfil
057100030822     C*
057200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
057300040708     C                   CLEAR                   FNVAO000
057400140730     C                   CLEAR                   FNVAOEAR
057500040723     C                   CLEAR                   TIORI000
057600030822     C                   CLEAR                   SkSplitCSV
057700030822     C                   EVAL      i = 1
057800030822     C                   EVAL      posDa = *zeros
057900030822     C                   EVAL      posA  = *zeros
058000150924     C*
058100150924     C* Forzatura campo FNVAO
058200150924     C                   EVAL      VAOTCO = 'F'
058300030822     C*
058400030822     C* Reimposto i valori di default
058500030822     C                   EXSR      DEFCAM
058600030822     C*
058700010330     C                   ENDSR
058800010330     C*----------------------------------------------------*
058900030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
059000010330     C*----------------------------------------------------*
059100010330     C     DEFCAM        BEGSR
059200030822     C*
059300030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
059400030715     C* e delimitatore testo.
059500030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
059600030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
059700030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
059800090403     C*
059900090403     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
060000090403     C                   EVAL      CharSOS = CharNUM
060100030822     C*
060200030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
060300030715     C                   IF        %subst(vlrppt:1:1) = 'S'
060400030715     C                   SETON                                        50
060500030715     C                   ELSE
060600030715     C                   SETOFF                                       50
060700131008     C                   ENDIF
060800131008     C* aggiungo la gestyione dei test (tanto la stamapa non c'è più)
060900131008     C                   SETOFF                                       55
061000131008     C                   IF        %subst(vlrppt:1:1) = 'T'
061100131008     C* inibisco la scrittura dei file TIORI e TIVGD
061200131008     C                   SETON                                        55
061300131008     C* imposto il PO di emissione come filiale fittizia
061400131008     C                   EVAL      VAOPOE = 999
061500131008     C                   ENDIF
061600030822     C*
061700030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
061800030822     C                   EVAL      posDaDft = 1
061900030822     C                   EVAL      posADft  = 0
062000030822     C                   EVAL      wGiroDft = 0
062100030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
062200030822     C                             posDaDft > 0
062300030822     C*
062400030822     C* Gestisco il 1° giro
062500030822     C                   IF        wGiroDft = 0
062600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
062700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
062800030822     C* Incremento il contatore dei "giri"
062900030822     C                   EVAL      wGiroDft = 1
063000030822     C                   ELSE
063100030822     C                   EVAL      posDaDft = posADft
063200030822     C                   ENDIF
063300030822     C* Eseguo lo scan x trovare la fine del campo corrente
063400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
063500030822     C*
063600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
063700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
063800030822     C                   IF        posDaDft > 0 AND
063900030822     C                             posADft  > 0
064000040708     C* POE
064100040708     C                   IF        %subst(
064200040708     C                             %subst(vlrppt:posDaDft+1:
064300040708     C                             posADft-posDaDft-1):1:3)
064400040708     C                             = 'POE'
064500040708     C                   EVAL      PiStr=%trim(%subst(
064600040708     C                             %subst(vlrppt:posDaDft+1:
064700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064800040708     C                   EXSR      CHKNUM
064900040708     C                   IF        PiInt=*on
065000040708     C                   Z-ADD     PiVal         VAOPOE
065100040708     C                   ENDIF
065200040708     C                   ENDIF
065300040708     C* TOR
065400040708     C                   IF        %subst(
065500040708     C                             %subst(vlrppt:posDaDft+1:
065600040708     C                             posADft-posDaDft-1):1:3)
065700040708     C                             = 'TOR'
065800040708     C                   EVAL      VAOTOR=%trim(%subst(
065900040708     C                             %subst(vlrppt:posDaDft+1:
066000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066100040708     C                   ENDIF
066200040708     C* TCO
066300040708     C                   IF        %subst(
066400040708     C                             %subst(vlrppt:posDaDft+1:
066500040708     C                             posADft-posDaDft-1):1:3)
066600040708     C                             = 'TCO'
066700040708     C                   EVAL      VAOTCO=%trim(%subst(
066800040708     C                             %subst(vlrppt:posDaDft+1:
066900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067000040708     C                   ENDIF
067100040707     C* COR
067200030822     C                   IF        %subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):1:3)
067500040707     C                             = 'COR'
067600030822     C                   EVAL      PiStr=%trim(%subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067900030822     C                   EXSR      CHKNUM
068000030822     C                   IF        PiInt=*on
068100040708     C                   Z-ADD     PiVal         VAOCOR
068200030822     C                   ENDIF
068300030822     C                   ENDIF
068400040708     C* CRC
068500040708     C                   IF        %subst(
068600040708     C                             %subst(vlrppt:posDaDft+1:
068700040708     C                             posADft-posDaDft-1):1:3)
068800040708     C                             = 'CRC'
068900040708     C                   EVAL      PiStr=%trim(%subst(
069000040708     C                             %subst(vlrppt:posDaDft+1:
069100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069200040708     C                   EXSR      CHKNUM
069300040708     C                   IF        PiInt=*on
069400040708     C                   Z-ADD     PiVal         VAOCRC
069500040708     C                   ENDIF
069600040708     C                   ENDIF
069700040708     C* FFD
069800040708     C                   IF        %subst(
069900040708     C                             %subst(vlrppt:posDaDft+1:
070000040708     C                             posADft-posDaDft-1):1:3)
070100040708     C                             = 'FFD'
070200040708     C                   EVAL      VAOFFD=%trim(%subst(
070300040708     C                             %subst(vlrppt:posDaDft+1:
070400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070500040708     C                   ENDIF
070600040708     C* ORR
070700040708     C                   IF        %subst(
070800040708     C                             %subst(vlrppt:posDaDft+1:
070900040708     C                             posADft-posDaDft-1):1:3)
071000040708     C                             = 'ORR'
071100040708     C                   EVAL      PiStr=%trim(%subst(
071200040708     C                             %subst(vlrppt:posDaDft+1:
071300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071400040708     C                   EXSR      CHKNUM
071500040708     C                   IF        PiInt=*on
071600040708     C                   Z-ADD     PiVal         VAOORR
071700040708     C                   ENDIF
071800040708     C                   ENDIF
071900040712     C* NCL
072000040712     C                   IF        %subst(
072100040712     C                             %subst(vlrppt:posDaDft+1:
072200040712     C                             posADft-posDaDft-1):1:3)
072300040712     C                             = 'NCL'
072400040712     C                   EVAL      PiStr=%trim(%subst(
072500040712     C                             %subst(vlrppt:posDaDft+1:
072600040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072700040712     C                   EXSR      CHKNUM
072800040712     C                   IF        PiInt=*on
072900040712     C                   Z-ADD     PiVal         VAONCL
073000040712     C                   ENDIF
073100040712     C                   ENDIF
073200040712     C* PKG
073300040712     C                   IF        %subst(
073400040712     C                             %subst(vlrppt:posDaDft+1:
073500040712     C                             posADft-posDaDft-1):1:3)
073600040712     C                             = 'PKG'
073700040712     C                   EVAL      PiStr=%trim(%subst(
073800040712     C                             %subst(vlrppt:posDaDft+1:
073900040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074000040712     C                   EXSR      CHKNUM
074100040712     C                   IF        PiNum=*on
074200040712     C                   Z-ADD     PiVal         VAOPKG
074300040712     C                   ENDIF
074400040712     C                   ENDIF
074500040712     C* VLM
074600040712     C                   IF        %subst(
074700040712     C                             %subst(vlrppt:posDaDft+1:
074800040712     C                             posADft-posDaDft-1):1:3)
074900040712     C                             = 'VLM'
075000040712     C                   EVAL      PiStr=%trim(%subst(
075100040712     C                             %subst(vlrppt:posDaDft+1:
075200040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075300040712     C                   EXSR      CHKNUM
075400040712     C                   IF        PiNum=*on
075500040712     C                   Z-ADD     PiVal         VAOVLM
075600040712     C                   ENDIF
075700040712     C                   ENDIF
075800040708     C* NAM
075900040708     C                   IF        %subst(
076000040708     C                             %subst(vlrppt:posDaDft+1:
076100040708     C                             posADft-posDaDft-1):1:3)
076200040708     C                             = 'NAM'
076300040708     C                   EVAL      VAONAM=%trim(%subst(
076400040708     C                             %subst(vlrppt:posDaDft+1:
076500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076600040708     C                   ENDIF
076700040708     C* PAG
076800030822     C                   IF        %subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):1:3)
077100040708     C                             = 'PAG'
077200040708     C                   EVAL      VAOPAG=%trim(%subst(
077300030822     C                             %subst(vlrppt:posDaDft+1:
077400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077500030822     C                   ENDIF
077600040708     C* KSC
077700040708     C                   IF        %subst(
077800040708     C                             %subst(vlrppt:posDaDft+1:
077900040708     C                             posADft-posDaDft-1):1:3)
078000040708     C                             = 'KSC'
078100040708     C                   EVAL      PiStr=%trim(%subst(
078200040708     C                             %subst(vlrppt:posDaDft+1:
078300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078400040708     C                   EXSR      CHKNUM
078500040708     C                   IF        PiInt=*on
078600040708     C                   Z-ADD     PiVal         VAOKSC
078700040708     C                   ENDIF
078800040708     C                   ENDIF
078900040708     C* CTR
079000040708     C                   IF        %subst(
079100040708     C                             %subst(vlrppt:posDaDft+1:
079200040708     C                             posADft-posDaDft-1):1:3)
079300040708     C                             = 'CTR'
079400040708     C                   EVAL      VAOCTR=%trim(%subst(
079500040708     C                             %subst(vlrppt:posDaDft+1:
079600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079700040708     C                   ENDIF
079800040708     C* POC
079900040708     C                   IF        %subst(
080000040708     C                             %subst(vlrppt:posDaDft+1:
080100040708     C                             posADft-posDaDft-1):1:3)
080200040708     C                             = 'POC'
080300040708     C                   EVAL      PiStr=%trim(%subst(
080400040708     C                             %subst(vlrppt:posDaDft+1:
080500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080600040708     C                   EXSR      CHKNUM
080700040708     C                   IF        PiInt=*on
080800040708     C                   Z-ADD     PiVal         VAOPOC
080900040708     C                   ENDIF
081000040708     C                   ENDIF
081100040708     C* NO1
081200040708     C                   IF        %subst(
081300040708     C                             %subst(vlrppt:posDaDft+1:
081400040708     C                             posADft-posDaDft-1):1:3)
081500040708     C                             = 'NO1'
081600040708     C                   EVAL      VAONO1=%trim(%subst(
081700040708     C                             %subst(vlrppt:posDaDft+1:
081800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081900040708     C                   ENDIF
082000040708     C* DDT
082100030822     C                   IF        %subst(
082200030822     C                             %subst(vlrppt:posDaDft+1:
082300030822     C                             posADft-posDaDft-1):1:3)
082400040708     C                             = 'DDT'
082500040708     C                   EVAL      VAODDT=%trim(%subst(
082600030822     C                             %subst(vlrppt:posDaDft+1:
082700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082800030822     C                   ENDIF
082900040708     C* RFA
083000040708     C                   IF        %subst(
083100040708     C                             %subst(vlrppt:posDaDft+1:
083200040708     C                             posADft-posDaDft-1):1:3)
083300040708     C                             = 'RFA'
083400040708     C                   EVAL      VAORFA=%trim(%subst(
083500040708     C                             %subst(vlrppt:posDaDft+1:
083600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083700040708     C                   ENDIF
083800040708     C* FLO
083900040708     C                   IF        %subst(
084000040708     C                             %subst(vlrppt:posDaDft+1:
084100040708     C                             posADft-posDaDft-1):1:3)
084200040708     C                             = 'FLO'
084300160308     C                   EVAL      VAOFLO=%subst(
084400040708     C                             %subst(vlrppt:posDaDft+1:
084500160308     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3)
084600040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
084700040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
084800040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
084900040708     C                   ENDIF
085000030822     C* ...
085100030822     C                   ENDIF
085200030822     C                   ENDDO
085300020204     C*
085400010330     C                   ENDSR
085500010607     C*----------------------------------------------------*
085600040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
085700010607     C*----------------------------------------------------*
085800040708     C     IMPVAO        BEGSR
085900010607     C*
086000010607     C                   SETOFF                                       3132
086100030515     C*
086200040708     C                   MOVEL     datcor        VAODAO
086300040708     C                   MOVEL     wn14          VAOOAO
086400010607     C*
086500030715     C**********
086600030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
086700030715     C**********
086800030715     C                   IF        wGiro = *zeros
086900031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
087000031201     C     minu:maiu     XLATE     vindta        vindta
087100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
087200040324     C                   DOW       posDa <= %len(%trimr(vindta))
087300030715     C*
087400030715     C* Gestisco il 1° campo
087500030715     C                   IF        i = 1
087600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
087700030715     C                   EVAL      posDa = 1
087800030715     C* Eseguo lo scan x trovare la fine del primo campo
087900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088000030715     C* A questo punto "estraggo" il campo corrente
088100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088200030715     C                                                    (posA-posDa))
088300030715     C* X i campi successivi al 1°
088400030715     C                   ELSE
088500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
088600030715     C                   EVAL      posDa = posA + 1
088700030715     C* Eseguo lo scan x trovare la fine del campo corrente
088800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088900030715     C* Gestisco l'ultimo campo
089000030715     C                   IF        posA = *zeros
089100040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
089200030715     C* A questo punto "estraggo" il campo corrente
089300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089400030715     C                                                    (posA-posDa))
089500030715     C                   LEAVE
089600030715     C                   ELSE
089700030715     C* A questo punto "estraggo" il campo corrente
089800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089900030715     C                                                    (posA-posDa))
090000030715     C                   ENDIF
090100030715     C                   ENDIF
090200030715     C* Incremento il contatore d campo
090300030715     C                   EVAL      i = i +1
090400030715     C                   ENDDO
090500030715     C*
090600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
090700030715     C* delimitazione testo
090800030715     C                   EVAL      i = 1
090900030715     C                   DOW       i < %elem(SkSplitFLD)
091000030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
091100030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
091200030715     C                   EVAL      i = i + 1
091300030715     C                   ENDDO
091400030715     C                   EVAL      wGiro = 1
091500030715     C                   ELSE
091600030715     C                   EVAL      wGiro = 2
091700031204     C**********
091800031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
091900031204     C* d separatore campo
092000031204     C**********
092100040304     C                   IF        %subst(vindta:1:1) = CharCSV                 * all'inizio
092200040304     C                   EVAL      vindta = ' ' + vindta
092300040304     C                   ENDIF
092400040304     C***                DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
092500040304     C***                EVAL      vindta = %subst(vindta:2)
092600040304     C***                ENDDO
092700031204     C*
092800031204     C                   Z-ADD     *zeros        lunghInput        4 0
092900040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
093000040324     C                   IF        %subst(%trimr(vindta):lunghInput:1) = CharCSV
093100040304     C                   EVAL      vindta = vindta + ' '
093200040304     C                   ENDIF
093300040324     C***                DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
093400040324     C***                EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
093500040304     C***                EVAL      lunghInput = %len(%trim(vindta))
093600040304     C***                ENDDO
093700030715     C**********
093800030715     C* Effettuo lo split del campo dati d input
093900030715     C**********
094000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
094100040324     C                   DOW       posDa <= %len(%trimr(vindta))
094200030715     C*
094300030715     C* Gestisco il 1° campo
094400030715     C                   IF        i = 1
094500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
094600030715     C                   EVAL      posDa = 1
094700030715     C* Eseguo lo scan x trovare la fine del primo campo
094800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094900030715     C* A questo punto "estraggo" il campo corrente
095000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095100030715     C                                                    (posA-posDa))
095200030715     C* X i campi successivi al 1°
095300030715     C                   ELSE
095400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
095500030715     C                   EVAL      posDa = posA + 1
095600031204     C* Verifico che nn vi sia il campo nn valorizzato
095700031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
095800031204     C* Se campo nn valorizzato skippo al prossimo
095900031204     C                   EVAL      SkSplitCSV(i) = *blanks
096000031204     C                   EVAL      posA  = posA + 1
096100031204     C                   ELSE
096200030715     C* Eseguo lo scan x trovare la fine del campo corrente
096300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
096400030715     C* Gestisco l'ultimo campo
096500030715     C                   IF        posA = *zeros
096600040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
096700030715     C* A questo punto "estraggo" il campo corrente
096800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096900030715     C                                                    (posA-posDa))
097000030715     C                   LEAVE
097100030715     C                   ELSE
097200030715     C* A questo punto "estraggo" il campo corrente
097300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
097400030715     C                                                    (posA-posDa))
097500030715     C                   ENDIF
097600031204     C                   ENDIF
097700030715     C                   ENDIF
097800030715     C* Incremento il contatore d campo
097900030715     C                   EVAL      i = i +1
098000030715     C                   ENDDO
098100030715     C*
098200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
098300030715     C* delimitazione testo
098400030715     C                   EVAL      i = 1
098500030715     C                   DOW       i < %elem(SkSplitCSV)
098600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
098700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
098800030715     C                   EVAL      i = i + 1
098900030715     C                   ENDDO
099000030715     C*
099100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
099200030715     C                   EVAL      i = 1
099300030715     C                   DOW       i < %elem(SkSplitFLD)
099400030715     C*
099500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
099600030715     C                   IF        SkSplitFLD(i) <> *blanks AND
099700030715     C                             SkSplitCSV(i) <> *blanks
099800030721     C*** ATB
099900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATB'
100000040708     C                   EVAL      VAOATB = SkSplitCSV(i)
100100030721     C                   ENDIF
100200040708     C*** TOR
100300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTOR'
100400040708     C                   EVAL      VAOTOR = SkSplitCSV(i)
100500040708     C                   ENDIF
100600040708     C*** TCO
100700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTCO'
100800040708     C                   EVAL      VAOTCO = SkSplitCSV(i)
100900040708     C                   ENDIF
101000040708     C*** RSO
101100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSO'
101200040708     C                   EVAL      VAORSO = SkSplitCSV(i)
101300030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
101400040708     C     '@':'A'       XLATE     VAORSO        VAORSO
101500030715     C* ==
101600030715     C                   ENDIF
101700040708     C*** INO
101800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINO'
101900040708     C                   EVAL      VAOINO = SkSplitCSV(i)
102000030715     C                   ENDIF
102100040708     C*** LOO
102200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOO'
102300040708     C                   EVAL      VAOLOO = SkSplitCSV(i)
102400030715     C                   ENDIF
102500040708     C*** PRO
102600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRO'
102700040708     C                   EVAL      VAOPRO = SkSplitCSV(i)
102800030715     C                   ENDIF
102900040708     C*** NAO
103000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAO'
103100040708     C                   EVAL      VAONAO = SkSplitCSV(i)
103200030721     C                   ENDIF
103300040708     C*** RSR
103400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSR'
103500040708     C                   EVAL      VAORSR = SkSplitCSV(i)
103600040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
103700040708     C     '@':'A'       XLATE     VAORSR        VAORSR
103800040708     C* ==
103900040708     C                   IF        VAORSR = *blanks
104000040708     C                   SETON                                        31
104100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
104200040708     C                             + ' ' + 'VAORSR'
104300040708     C                   ENDIF
104400040708     C                   ENDIF
104500040708     C*** INR
104600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINR'
104700040708     C                   EVAL      VAOINR = SkSplitCSV(i)
104800040708     C                   IF        VAOINR = *blanks
104900040708     C                   SETON                                        31
105000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
105100040708     C                             + ' ' + 'VAOINR'
105200040708     C                   ENDIF
105300040708     C                   ENDIF
105400040708     C*** LOR
105500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOR'
105600040708     C                   EVAL      VAOLOR = SkSplitCSV(i)
105700040708     C                   IF        VAOLOR = *blanks
105800040708     C                   SETON                                        31
105900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
106000040708     C                             + ' ' + 'VAOLOR'
106100040708     C                   ENDIF
106200040708     C                   ENDIF
106300040708     C*** PRR
106400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRR'
106500040708     C                   EVAL      VAOPRR = SkSplitCSV(i)
106600040708     C                   IF        VAOPRR = *blanks
106700040708     C                   SETON                                        31
106800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
106900040708     C                             + ' ' + 'VAOPRR'
107000040708     C                   ENDIF
107100040708     C                   ENDIF
107200040708     C*** NAR
107300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAR'
107400040708     C                   EVAL      VAONAR = SkSplitCSV(i)
107500040708     C                   ENDIF
107600040708     C*** RER
107700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORER'
107800040708     C                   EVAL      VAORER = SkSplitCSV(i)
107900040708     C                   ENDIF
108000040708     C*** TER
108100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTER'
108200040708     C                   EVAL      VAOTER = SkSplitCSV(i)
108300040708     C                   ENDIF
108400040708     C*** RSC
108500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSC'
108600040708     C                   EVAL      VAORSC = SkSplitCSV(i)
108700040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
108800040708     C     '@':'A'       XLATE     VAORSC        VAORSC
108900040708     C* ==
109000040708     C                   ENDIF
109100040708     C*** INC
109200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINC'
109300040708     C                   EVAL      VAOINC = SkSplitCSV(i)
109400040708     C                   ENDIF
109500040708     C*** LOC
109600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOC'
109700040708     C                   EVAL      VAOLOC = SkSplitCSV(i)
109800040708     C                   ENDIF
109900040708     C*** PRC
110000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRC'
110100040708     C                   EVAL      VAOPRC = SkSplitCSV(i)
110200040708     C                   ENDIF
110300040708     C*** NAC
110400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAC'
110500040708     C                   EVAL      VAONAC = SkSplitCSV(i)
110600040708     C                   ENDIF
110700040708     C*** FFD
110800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFFD'
110900040708     C                   EVAL      VAOFFD = SkSplitCSV(i)
111000040708     C                   ENDIF
111100040708     C*** RMP
111200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORMP'
111300040708     C                   EVAL      VAORMP = SkSplitCSV(i)
111400040708     C                   ENDIF
111500040708     C*** NAM
111600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAM'
111700040708     C                   EVAL      VAONAM = SkSplitCSV(i)
111800040708     C                   ENDIF
111900040708     C*** PAG
112000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPAG'
112100040708     C                   EVAL      VAOPAG = SkSplitCSV(i)
112200040708     C                   ENDIF
112300040708     C*** CTR
112400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCTR'
112500040708     C                   EVAL      VAOCTR = SkSplitCSV(i)
112600040708     C                   ENDIF
112700040708     C*** ZOR
112800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOZOR'
112900040708     C                   EVAL      VAOZOR = SkSplitCSV(i)
113000040708     C                   ENDIF
113100040708     C*** NO1
113200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO1'
113300040708     C                   EVAL      VAONO1 = SkSplitCSV(i)
113400030715     C                   ENDIF
113500040708     C*** NO2
113600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO2'
113700040708     C                   EVAL      VAONO2 = SkSplitCSV(i)
113800030715     C                   ENDIF
113900040708     C*** DDT
114000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODDT'
114100040708     C                   EVAL      VAODDT = SkSplitCSV(i)
114200030715     C                   ENDIF
114300040708     C*** RFA
114400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORFA'
114500040708     C                   EVAL      VAORFA = SkSplitCSV(i)
114600030715     C                   ENDIF
114700040708     C*** SPI
114800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSPI'
114900040708     C                   EVAL      VAOSPI = SkSplitCSV(i)
115000030715     C                   ENDIF
115100040708     C*** FLO
115200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
115300040708     C                   EVAL      VAOFLO = SkSplitCSV(i)
115400160308     C     '_':' '       XLATE     VAOFLO        VAOFLO
115500040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
115600040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
115700040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
115800140730     C                   ENDIF
115900140730     C*** FLO2
116000140730     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO2'
116100140730     C                   EVAL      w_VAOFLO2 = SkSplitCSV(i)
116200140730     C                   ENDIF
116300140730     C*** TSPS
116400140730     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTSPS'
116500140730     C                   EVAL      w_VAOTSPS = SkSplitCSV(i)
116600140730     C                   ENDIF
116700140730     C*** NOTE
116800140730     C                   IF        %trim(SkSplitFLD(i)) = 'VAONOTE'
116900140730     C                   EVAL      w_VAONOTE = SkSplitCSV(i)
117000140730     C                   ENDIF
117100140730     C*** ORDA1
117200140730     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORDA1'
117300140730     C                   EVALR     w_VAOORDA1 = %trim(SkSplitCSV(i))
117400140730     C                   ENDIF
117500140730     C*** ORAA1
117600140730     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORAA1'
117700140730     C                   EVALR     w_VAOORAA1 = %trim(SkSplitCSV(i))
117800140730     C                   ENDIF
117900140730     C*** ORDA2
118000140730     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORDA2'
118100140730     C                   EVALR     w_VAOORDA2 = %trim(SkSplitCSV(i))
118200140730     C                   ENDIF
118300140730     C*** ORAA2
118400140730     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORAA2'
118500140730     C                   EVALR     w_VAOORAA2 = %trim(SkSplitCSV(i))
118600140730     C                   ENDIF
118700140730     C*** EMLR
118800140730     C                   IF        %trim(SkSplitFLD(i)) = 'VAOEMLR'
118900140730     C                   EVAL      w_VAOEMLR = SkSplitCSV(i)
119000140730     C                   ENDIF
119100140730     C*** EMLRF
119200140730     C                   IF        %trim(SkSplitFLD(i)) = 'VAOEMLRF'
119300140730     C                   EVAL      w_VAOEMLRF = SkSplitCSV(i)
119400140730     C                   ENDIF
119500140730     C*** SMSR
119600140730     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSMSR'
119700140730     C                   EVAL      w_VAOSMSR = SkSplitCSV(i)
119800140730     C                   ENDIF
119900140730     C*** SMSRF
120000140730     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSMSRF'
120100140730     C                   EVAL      w_VAOSMSRF = SkSplitCSV(i)
120200140730     C                   ENDIF
120300030715     C
120400030721     C*
120500030721     C* Reperisco quindi i campi numerici...
120600040708     C*** POE
120700040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
120800040723     C                   EVAL      PiStr=SkSplitCSV(i)
120900040723     C                   EXSR      CHKNUM
121000040723     C                   IF        PiInt=*on    AND
121100040723     C                             PiVal<=999   AND
121200040723     C                             PiVal>*zeros
121300040723     C                   Z-ADD     PiVal         VAOPOE
121400040723     C                   ELSE
121500040723     C                   SETON                                        32
121600040723     C                   EVAL      VAOPOE = *zeros
121700040723     C                   EVAL      vinmsg = %trimr(vinmsg)
121800040723     C                             + ' ' + 'VAOPOE'
121900040723     C                   ENDIF
122000040723     C                   ENDIF
122100040708     C*** COR
122200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
122300040708     C                   EVAL      PiStr=SkSplitCSV(i)
122400040708     C                   EXSR      CHKNUM
122500040708     C                   IF        PiInt=*on
122600040708     C                   Z-ADD     PiVal         VAOCOR
122700040708     C                   ELSE
122800040708     C                   SETON                                        31
122900040708     C                   EVAL      VAOCOR = *zeros
123000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
123100040708     C                             + ' ' + 'VAOCOR'
123200040708     C                   ENDIF
123300040708     C                   ENDIF
123400040708     C*** CAO
123500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
123600130502     C                   EVAL      VAOCAO = SkSplitCSV(i)
123700130502     C***                EVAL      PiStr=SkSplitCSV(i)
123800130502     C***                EXSR      CHKNUM
123900130502     C***                IF        PiInt=*on
124000130502     C***                Z-ADD     PiVal         Num5_0
124100130502     C***                MOVEL(p)  Num5_0        VAOCAO
124200130502     C***                ELSE
124300130502     C***                SETON                                        32
124400130502     C***                EVAL      VAOCAO = *zeros
124500130502     C***                EVAL      vinmsg = %trimr(vinmsg)
124600130502     C***                          + ' ' + 'VAOCAO'
124700130502     C***                ENDIF
124800030721     C                   ENDIF
124900040708     C*** CRA
125000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
125100030721     C                   EVAL      PiStr=SkSplitCSV(i)
125200030721     C                   EXSR      CHKNUM
125300030721     C                   IF        PiInt=*on
125400040708     C                   Z-ADD     PiVal         VAOCRA
125500030721     C                   ELSE
125600030721     C                   SETON                                        32
125700040708     C                   EVAL      VAOCRA = *zeros
125800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125900040708     C                             + ' ' + 'VAOCRA'
126000030721     C                   ENDIF
126100030721     C                   ENDIF
126200040708     C*** CAR
126300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
126400130502     C                   EVAL      VAOCAR = SkSplitCSV(i)
126500130502     C***                EVAL      PiStr=SkSplitCSV(i)
126600130502     C***                EXSR      CHKNUM
126700130502     C***                IF        PiInt=*on
126800130502     C***                Z-ADD     PiVal         Num5_0
126900130502     C***                MOVEL(p)  Num5_0        VAOCAR
127000130502     C***                ELSE
127100130502     C***                SETON                                        31
127200130502     C***                EVAL      VAOCAR = *zeros
127300130502     C***                EVAL      vinmsg = %trimr(vinmsg)
127400130502     C***                          + ' ' + 'VAOCAR'
127500130502     C***                ENDIF
127600040708     C                   ENDIF
127700040708     C*** CRC
127800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
127900040708     C                   EVAL      PiStr=SkSplitCSV(i)
128000040708     C                   EXSR      CHKNUM
128100040708     C                   IF        PiInt=*on
128200040708     C                   Z-ADD     PiVal         VAOCRC
128300040708     C                   ELSE
128400040708     C                   SETON                                        32
128500040708     C                   EVAL      VAOCRC = *zeros
128600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
128700040708     C                             + ' ' + 'VAOCRC'
128800040708     C                   ENDIF
128900040708     C                   ENDIF
129000040708     C*** CAC
129100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
129200130502     C                   EVAL      VAOCAC = SkSplitCSV(i)
129300130502     C***                EVAL      PiStr=SkSplitCSV(i)
129400130502     C***                EXSR      CHKNUM
129500130502     C***                IF        PiInt=*on
129600130502     C***                Z-ADD     PiVal         Num5_0
129700130502     C***                MOVEL(p)  Num5_0        VAOCAC
129800130502     C***                ELSE
129900130502     C***                SETON                                        32
130000130502     C***                EVAL      VAOCAC = *zeros
130100130502     C***                EVAL      vinmsg = %trimr(vinmsg)
130200130502     C***                          + ' ' + 'VAOCAC'
130300130502     C***                ENDIF
130400040708     C                   ENDIF
130500040708     C*** DAR
130600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
130700040708     C                   EVAL      PiStr=SkSplitCSV(i)
130800040708     C                   EXSR      CHKNUM
130900040708     C                   IF        PiInt=*on
131000040708     C                   Z-ADD     PiVal         VAODAR
131100040708     C                   ELSE
131200040708     C                   SETON                                        32
131300040708     C                   EVAL      VAODAR = *zeros
131400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
131500040708     C                             + ' ' + 'VAODAR'
131600040708     C                   ENDIF
131700040708     C                   ENDIF
131800040708     C*** ORR
131900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
132000040708     C                   EVAL      PiStr=SkSplitCSV(i)
132100040708     C                   EXSR      CHKNUM
132200040708     C                   IF        PiInt=*on
132300040708     C                   Z-ADD     PiVal         VAOORR
132400040708     C                   ELSE
132500040708     C                   SETON                                        32
132600040708     C                   EVAL      VAOORR = *zeros
132700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
132800040708     C                             + ' ' + 'VAOORR'
132900040708     C                   ENDIF
133000040708     C                   ENDIF
133100030721     C*** NCL
133200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
133300030721     C                   EVAL      PiStr=SkSplitCSV(i)
133400030721     C                   EXSR      CHKNUM
133500030721     C                   IF        PiInt=*on
133600040708     C                   Z-ADD     PiVal         VAONCL
133700030721     C                   ELSE
133800030721     C                   SETON                                        32
133900040708     C                   EVAL      VAONCL = *zeros
134000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134100040708     C                             + ' ' + 'VAONCL'
134200030721     C                   ENDIF
134300030721     C                   ENDIF
134400040708     C*** PKG
134500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
134600030721     C                   EVAL      PiStr=SkSplitCSV(i)
134700030721     C                   EXSR      CHKNUM
134800030721     C                   IF        PiNum=*on
134900040708     C                   Z-ADD     PiVal         VAOPKG
135000030721     C                   ELSE
135100030721     C                   SETON                                        32
135200040708     C                   EVAL      VAOPKG = *zeros
135300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135400040708     C                             + ' ' + 'VAOPKG'
135500030721     C                   ENDIF
135600030721     C                   ENDIF
135700040708     C*** VLM
135800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
135900040708     C                   EVAL      PiStr=SkSplitCSV(i)
136000040708     C                   EXSR      CHKNUM
136100040708     C                   IF        PiNum=*on
136200040708     C                   Z-ADD     PiVal         VAOVLM
136300040708     C                   ELSE
136400040708     C                   SETON                                        32
136500040708     C                   EVAL      VAOVLM = *zeros
136600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
136700040708     C                             + ' ' + 'VAOVLM'
136800040708     C                   ENDIF
136900040708     C                   ENDIF
137000040708     C*** BNC
137100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
137200040708     C                   EVAL      PiStr=SkSplitCSV(i)
137300040708     C                   EXSR      CHKNUM
137400040708     C                   IF        PiInt=*on
137500040708     C                   Z-ADD     PiVal         VAOBNC
137600040708     C                   ELSE
137700040708     C                   SETON                                        32
137800040708     C                   EVAL      VAOBNC = *zeros
137900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
138000040708     C                             + ' ' + 'VAOBNC'
138100040708     C                   ENDIF
138200040708     C                   ENDIF
138300040708     C*** BLC
138400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
138500040708     C                   EVAL      PiStr=SkSplitCSV(i)
138600040708     C                   EXSR      CHKNUM
138700040708     C                   IF        PiInt=*on
138800040708     C                   Z-ADD     PiVal         VAOBLC
138900040708     C                   ELSE
139000040708     C                   SETON                                        32
139100040708     C                   EVAL      VAOBLC = *zeros
139200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
139300040708     C                             + ' ' + 'VAOBLC'
139400040708     C                   ENDIF
139500040708     C                   ENDIF
139600040708     C*** ATT
139700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
139800040708     C                   EVAL      PiStr=SkSplitCSV(i)
139900040708     C                   EXSR      CHKNUM
140000040708     C                   IF        PiInt=*on
140100040708     C                   Z-ADD     PiVal         VAOATT
140200040708     C                   ELSE
140300040708     C                   SETON                                        32
140400040708     C                   EVAL      VAOATT = *zeros
140500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
140600040708     C                             + ' ' + 'VAOATT'
140700040708     C                   ENDIF
140800040708     C                   ENDIF
140900040708     C*** MTC
141000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
141100040708     C                   EVAL      PiStr=SkSplitCSV(i)
141200040708     C                   EXSR      CHKNUM
141300040708     C                   IF        PiInt=*on
141400040708     C                   Z-ADD     PiVal         VAOMTC
141500040708     C                   ELSE
141600040708     C                   SETON                                        32
141700040708     C                   EVAL      VAOMTC = *zeros
141800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
141900040708     C                             + ' ' + 'VAOMTC'
142000040708     C                   ENDIF
142100040708     C                   ENDIF
142200040708     C*** KSC
142300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
142400030721     C                   EVAL      PiStr=SkSplitCSV(i)
142500030721     C                   EXSR      CHKNUM
142600030721     C                   IF        PiInt=*on      AND
142700030721     C                             PiVal<=9999999 AND
142800030721     C                             PiVal>*zeros
142900040708     C                   Z-ADD     PiVal         VAOKSC
143000030721     C                   ELSE
143100030721     C                   SETON                                        32
143200040708     C                   EVAL      VAOKSC = *zeros
143300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
143400040708     C                             + ' ' + 'VAOKSC'
143500030721     C                   ENDIF
143600030721     C                   ENDIF
143700040708     C*** POR
143800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
143900030721     C                   EVAL      PiStr=SkSplitCSV(i)
144000030721     C                   EXSR      CHKNUM
144100030721     C                   IF        PiInt=*on    AND
144200030721     C                             PiVal<=999   AND
144300030721     C                             PiVal>*zeros
144400040708     C                   Z-ADD     PiVal         VAOPOR
144500030721     C                   ELSE
144600030721     C                   SETON                                        32
144700040708     C                   EVAL      VAOPOR = *zeros
144800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
144900040708     C                             + ' ' + 'VAOPOR'
145000030721     C                   ENDIF
145100030721     C                   ENDIF
145200040708     C*** POC
145300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
145400040708     C                   EVAL      PiStr=SkSplitCSV(i)
145500040708     C                   EXSR      CHKNUM
145600040708     C                   IF        PiInt=*on    AND
145700040708     C                             PiVal<=999   AND
145800040708     C                             PiVal>*zeros
145900040708     C                   Z-ADD     PiVal         VAOPOC
146000040708     C                   ELSE
146100040708     C                   SETON                                        32
146200040708     C                   EVAL      VAOPOC = *zeros
146300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
146400040708     C                             + ' ' + 'VAOPOC'
146500040708     C                   ENDIF
146600040708     C                   ENDIF
146700040708     C*** STO
146800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
146900040708     C                   EVAL      PiStr=SkSplitCSV(i)
147000040708     C                   EXSR      CHKNUM
147100040708     C                   IF        PiInt=*on
147200040708     C                   Z-ADD     PiVal         VAOSTO
147300040708     C                   ELSE
147400040708     C                   SETON                                        32
147500040708     C                   EVAL      VAOSTO = *zeros
147600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
147700040708     C                             + ' ' + 'VAOSTO'
147800040708     C                   ENDIF
147900040708     C                   ENDIF
148000030715     C*
148100030715     C                   ENDIF
148200030715     C                   EVAL      i = i + 1
148300030715     C                   ENDDO
148400101210     C*
148500101210     C* Se al termine del parsing il POE e/o il COR nn risultano valorizzato => errore
148600101210     C                   IF        VAOPOE = *zeros OR
148700101210     C                             VAOCOR = *zeros
148800101210     C                   SETON                                        31
148900101210     C                   ENDIF
149000020204     C*
149100030715     C                   ENDIF
149200020204     C*
149300010607     C                   ENDSR
149400010607     C*----------------------------------------------------*
149500020204
149600010330
149700010330
149800010330     C*----------------------------------------------------*
149900010330     C*  CONTROLLO NUMERICITA' CAMPI
150000010330     C*----------------------------------------------------*
150100010330     C     CHKNUM        BEGSR
150200010330     C*
150300010606     C                   IF        PiDecChr = *blanks
150400030715     C                   EVAL      PiDecChr = CharNUM
150500010606     C                   ENDIF
150600010606     C*
150700010606     C                   CALL(e)   'ISNUMERIC'
150800010330     C                   PARM                    PiStr            30
150900010606     C                   PARM                    PiDecChr          1
151000010330     C                   PARM      *ZEROS        PiVal            30 9
151100010330     C                   PARM      '0'           PiInt             1
151200010330     C                   PARM      '0'           PiNum             1
151300010330     C                   IF        %error
151400010606     C                   EVAL      PiNum=*off
151500010330     C                   ENDIF
151600010330     C*
151700010330     C                   ENDSR
151800010330     C***
151900010330
152000010601
152100010601
152200010601
152300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
152400130125     C***  repfil        BEGSR
152500010601     C*
152600130125     C***                if        invfil = *zeros and
152700130125     C***                          depfil > *zeros and
152800130125     C***                          (vinflg = *blanks or
152900130125     C***                           vinflg = *zeros)
153000130125     C***
153100130125     C***                eval      invfil = depfil
153200130125     C***                endif
153300010601     C*
153400130125     C***                if        depfil <> invfil and
153500130125     C***                          invfil > *zeros
153600130125     C***                eval      flgMulti = '1'
153700130125     C***                if        vinflg = *blanks
153800130125     C***                add       1             cntNonEl
153900130125     C***                endif
154000130125     C***                endif
154100010601     C*
154200130125     C***                if        vinflg = '2'
154300130125     C***                eval      flgStato = '2'
154400130125     C***                endif
154500010601     C*
154600130125     C***                ENDSR
154700010601     C***
154800010601
154900010601
155000010601
155100010330
155200010330
155300010330
155400990920      /TITLE Invio dei dati al punto operativo.
155500000613     C     invio         BEGSR
155600990920     C*
155700990920     C                   reset                   dscmz
155800010601     C                   move      invfil        cmzdst
155900990920     C                   eval      cmzfld = vlrfou
156000990920     C                   eval      cmzmbd = vlrhdl
156100990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
156200000710     C                   if        prmfir = *blanks
156300040708     C                   eval      cmzfla = 'FNVAO00F'
156400040708     C                   eval      cmzmba = 'FNVAO00F'
156500000710     C                   else
156600000710     C                   eval      cmzfla = prmfir
156700000710     C                   eval      cmzmba = prmfir
156800000710     C                   endif
156900990920     C                   eval      cmznrr = *zeros
157000990920     C                   move      §ctrok        cmznrr
157100021018     C                   eval      cmzlba = vlrfl1
157200990920     C                   call(e)   'TIS711C'
157300990920     C                   parm                    dscmz
157400990921     C                   parm      *blanks       esito
157500990920     C                   if        %error
157600990920     C                             or cmzerr = '1'
157700990921     C                             or esito  = '1'
157800000710     C                   eval      wrkesito = '3'
157900990920     C                   endif
158000990920     C*
158100000613     C                   ENDSR
158200990910
158300010601
158400010601
158500010601
158600010601
158700010601      /TITLE Invio dei dati al punto operativo.
158800010601     C     opeini        BEGSR
158900010601     C*
159000010601     C* Inizializzo flag e contatori operativi
159100010601     C                   movel     '0'           flgGiro           1
159200010601     C                   movel     '0'           flgMulti          1
159300010601     C                   movel     '1'           flgStato          1
159400010615     C                   movel     '0'           flgOk             1
159500010601     C                   z-add     *zeros        cntNonEl         10 0
159600010601     C                   z-add     *zeros        depfil            3 0
159700010601     C                   z-add     *zeros        invfil            3 0
159800010601     C*
159900010601     C                   ENDSR
160000010601     C***
160100050421
160200050421
160300050421
160400050421     C     *pssr         BEGSR
160500050421     C*
160600050421     C                   eval      wrkesito = '2'
160700050421     C*
160800050421     C                   ENDSR     '*CANCL'
160900050421     C***
161000060306
161100060306
161200060306     C*-------------------------------------------------------------------------*
161300060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
161400060306     C*-------------------------------------------------------------------------*
161500060306     C     sr_Control    begsr
161600060306     C*
161700060306     C* inizializzo a ok il flag d controllo abilitazione
161800060306     C                   eval      Cli_Abil = *on
161900060306     C
162000060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
162100060306     C                   clear                   woriIDC
162200060306     C                   eval      kteCOD = 'VAP'
162300060306     C                   eval      kteKE1 = oriIDC
162400060306     C     keytbe        chain     TNTBE01L
162500060306     C                   if        %found(Tntbe01l)
162600060306     C                   eval      woriIDC = TbeKe2
162700060306     C                   else
162800060306     C                   eval      Cli_Abil = *off
162900060306     C                   endif
163000060306     C*
163100060306     C                   endsr
163200010601
163300010601
163400010601
163500010330
163600010330
163700000613     C     *inzsr        BEGSR
163800990910     C*
163900990910     C     *entry        plist
164000990920     C                   parm                    tivlrds
164100990921     C                   parm      wrkesito      esito
164200000724     C                   parm                    prmlit
164300000710     C                   parm                    prmfir
164400010330     C*
164500010330     C* CALCOLA LA DATA CORRENTE
164600010330     C                   time                    wn14             14 0
164700040723     C                   movel     wn14          wn6               6 0          *ORA (6)
164800130125     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
164900130125     C                   eval      datcor = %dec(%date() : *ISO)
165000040723     C*
165100040723     C* Campi riferito al database
165200040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
165300040723     C     *like         define    tbeke1        kteKE1
165400040723     C*
165500040723     C* Definizione chiavi di lettura
165600040723     C* TNTBE01L - parziale
165700040723     C     keytbe        klist
165800040723     C                   kfld                    kteCOD                         *UTENTE
165900040723     C                   kfld                    kteKE1                         *CODICE
166000000613     C*
166100000613     C                   ENDSR
166200000613     C***
