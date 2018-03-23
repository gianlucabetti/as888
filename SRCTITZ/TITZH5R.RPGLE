000100040708      /TITLE Upload via Internet: traduzione in FNVAOWWR.
000200121129      *
000300121129      * PARTICOLARITA':
000400151111      *
000500151124      * Questo traduttore VAO con CSV sa da analisi che la prima colonna è
000600151111      * vuota, per cui deve mettere un '*' per evitare che siano elise delle colonne
000700151111      *
000800121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000900120209     F*
001000040723     Ffnacr01l  if   e           k disk
001100090618     Ftntbe01l  if   e           k disk
001200990910     Ftivin00r  uF   E             DISK    usropn
001300040708     FFnvaowwr  O    E             DISK    usropn
001400040723     Ftiori00f  O    E             DISK
001500050826     FTIVGD00F  O    E             DISK
001600000313     D*
001700040709     D*------------
001800040709     D* SCHIERE A PROGRAMMA
001900040709     D*------------
002000040709     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
002100010330     D*----------------------------------------------------
002200010330     D* DICHIARAZIOINE VARIABILI DI WRK
002300010330     D*----------------------------------------------------
002400010330     D dscmz         e ds                  inz
002500010330     D psds           sds
002600010330     D  procname         *PROC
002700010330     D tivlrds       e ds                  extname(tivlr00f)
002800050826     D fnvapds       e ds                  extname(fnvap00f)
002900040723     D dorm01        e ds
003000040723     D dfar          e ds
003100090618     d trul33ds      e ds
003200090618     d kpjba         e ds
003300150923     D fnvaoear      e ds                  inz
003400150923     D                                     prefix(w_)
003500010330     D esito           s              1
003600010330     D prmlit          s             10
003700010330     D prmfir          s             10
003800010330     D wrkesito        s                   like(esito)
003900010330     D rrnum           s              6  0 INZ(*zeros)
004000040709     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
004100060306     D Cli_Abil        s              1    inz(*off)
004200060306     D woriIDC         s                   like(oriIDC) inz(*blanks)
004300090618     d §numori         s              7  0 inz
004400120123     d wData_EUR       s               d   datfmt(*eur)
004500120123     d wData_ISO       s               d   datfmt(*iso)
004600120905     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
004700120905     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
004800151124     D Intestazioni    s               n   inz(*on)
004900030715     D*------------------
005000030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005100030715     D*------------------
005200030715     D SkSplitFLD      S             10    DIM(100)
005300030715     D SkSplitCSV      S            256    DIM(100)
005400030715     D CharCSV         S              1
005500030715     D CharTXT         S              1
005600030715     D CharNUM         S              1
005700030715     D posDa           S              3  0 INZ(*zeros)
005800030715     D posA            S              3  0 INZ(*zeros)
005900030715     D i               s              3  0 INZ(1)
006000030715     D wGiro           s              1  0 INZ(*zeros)
006100151125     D wSaltaRiga      s               n
006200030822     D*------------------
006300030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006400030822     D*------------------
006500030822     D posDaDft        S              3  0 INZ(*zeros)
006600030822     D posADft         S              3  0 INZ(*zeros)
006700030822     D j               s              3  0 INZ(1)
006800030822     D wGiroDft        s              1  0 INZ(*zeros)
006900040723     D*------------------
007000040723     D* DS REPERIMENTO DATI CLIENTE
007100040723     D*-------------------
007200040723     D BS69DS        E DS                  EXTNAME(TIBS69DS)
007300040723     D ACODS         E DS                  EXTNAME(CNACO00F)
007400040723     D INDDS         E DS                  EXTNAME(CNIND00F)
007500040723     D CLPDS         E DS                  EXTNAME(CNCLP00F)
007600040723     D CLSDS         E DS                  EXTNAME(FNCLS00F)
007700121129     D*------------------
007800150923     D w70             s             70
007900080702
008000080702     D*------------------
008100080702     D* LINKING A DEFINIZIONI ESTERNE
008200080702     D*------------------
008300090428     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
008400090428     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008500091026
008600091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
008700091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
008800091026
008900080702
009000031201     D*------------------
009100031201     D* Costanti
009200031201     D*------------------
009300031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009400031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009500010330
009600010330
009700010330
009800000913     C                   reset                   rrnum
009900990921     C                   reset                   esito
010000990921     C                   reset                   wrkesito
010100010601     C*
010200010601     C                   exsr      opeini
010300041011     C                   exsr      PREELA
010400040708     C                   exsr      rwvao
010500010601     C*
010600010601     C                   seton                                        lr
010700010601
010800010601
010900010601
011000010601
011100010601     C*--------------------------------------------------------
011200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011300010601     C*--------------------------------------------------------
011400010601     C     PREELA        BEGSR
011500010601     C*
011600010601     C* SE OCCORRE SPEDIRE IN FILIALE
011700041011     C                   if        flgGiro = '0'
011800010601     C*
011900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012000010601     C                   eval      flgGiro = '1'
012100041011     C*
012200041011     C                   endif
012300010601     C*
012400010601     C                   ENDSR
012500010601     C***
012600010601
012700010601
012800010601     C*--------------------------------------------------------
012900010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013000010601     C*--------------------------------------------------------
013100010601     C     ENDELA        BEGSR
013200000616     C*
013300120209     C                   ENDSR
013400010601
013500010601
013600010330     C*--------------------------------------------------------
013700040708     C* RWVAO   LEGGE TIVIN00R E SCRIVE FNVAOWWF              *
013800010330     C*--------------------------------------------------------
013900040708     C     RWVAO         BEGSR
014000010330     C*
014100010330     C                   if        not %open(tivin00r)
014200010330     C                   open      tivin00r
014300010330     C                   endif
014400040708     C                   if        not %open(fnvaowwr)
014500040708     C                   open      fnvaowwr
014600010330     C                   endif
014700150923     C*
014800150923     C* Mi predispongo per scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
014900150923     C                   call      'TIS7T7ER'
015000150923     C                   parm      'O'           pIn_Opz           1
015100150923     C                   parm                    tivlrds
015200150923     C                   parm                    FNVAOEAR
015300150923     C                   parm                    oriPRG
015400150923     C                   parm      *zeros        pIn_Dst           3
015500150923     C                   parm      *blanks       pOut_Esito        1
015600150923     C                   if        pOut_Esito = '2'
015700150923     C                   goto      FINE
015800150923     C                   endif
015900010330     C*
016000010604     C                   clear                   §CTROK
016100010604     C                   clear                   §CTRMO
016200010604     C                   clear                   §CTRNO
016300120123     C*
016400120905     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
016500120905     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
016600120905     C                   IF        Intestazioni = *off
016700120123     C* Eseguo primo giro x by-passare la mancanza della 1° riga con le intestazioni
016800120123     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
016900120123     C                   exsr      inzvar
017000120123     C                   exsr      impvao
017100151125     C*
017200151125     C                   ELSE
017300151125     C                   EVAL      wSaltaRiga = *on
017400120905     C                   ENDIF
017500010330     C*
017600030822     C                   do        *HIVAL
017700010330     C*
017800030822     C                   read      tivin00r                               70
017900010618     C*
018000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018100010618     C                   if        vinflg = '1'
018200010618     C                   eval      flgOk = '1'
018300010618     C                   endif
018400010618     C*
018500040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
018600040510     C                   z-add     *zeros        wLenVINDTA        4 0
018700040510     C                   z-add     *zeros        wFlgVINDTA        1 0
018800040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
018900040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
019000040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
019100040510     C                   z-add     1             wFlgVINDTA
019200040510     C                   leave
019300040510     C                   endif
019400040510     C                   enddo
019500040510     C*
019600040218     C                   if        vindta > *blanks AND
019700040510     C                             wFlgVINDTA = 1
019800010330     C                   add       1             rrnum
019900010330     C*
020000010601     C                   if        *in70 = *off and
020100010330     C                             (vinflg = *blanks
020200010330     C                              or vinflg = '0'
020300010330     C                              or vinflg = '2')
020400010330     C*
020500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020600010711     C                   if        vinflg = *blanks or vinflg = '0'
020700010711     C                   clear                   vinmsg
020800010711     C                   endif
020900151111     C*
021000151111     C* Aggiungo un byte a blanks all'inizio d ogni record
021100151111     C* siccome sappiamo che la prima colonna è vuota, mettiamo un carattere non vuoto per
021200151111     C* evitare che vengano elisi i blank
021300151111     C                   eval      vindta  = '*' + %trim(vindta)
021400010601     C*
021500010330     C                   exsr      inzvar
021600040708     C                   exsr      impvao
021700151019     C*
021800151019     C* Stacco subito un numeratore univoco "ORM"
021900151019     C                   exsr      RTVNUM
022000150923     C*
022100150923     C* Verifico se trattasi di formato ORM "esteso"
022200150923     C                   setoff                                       53
022300151124     C                   if        w_VAOEMLR  <> *blanks OR
022400151124     C                             w_VAOORDA1 <> *blanks OR
022500151124     C                             w_VAOORAA1 <> *blanks OR
022600151124     C                             w_VAOORDA2 <> *blanks OR
022700151124     C                             w_VAOORAA2 <> *blanks
022800150923     C                   seton                                        53
022900150923     C                   endif
023000010601     C*
023100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
023200040708     C                   eval      depfil = VAOPOE
023300010601     C                   exsr      repfil
023400010601     C                   if        depfil = invfil
023500021025     C                   if        vlrpoi = 999
023600040708     C                   move(P)   invfil        VAOPOE
023700021025     C                   else
023800040708     C                   move(P)   vlrpoi        VAOPOE
023900021025     C                   endif
024000010601     C*
024100010604     C* Ebbene...
024200010604     C*
024300030822     C  N31              add       1             §CTROK            7 0
024400030822     C   32              add       1             §CTRMO            7 0
024500030822     C   31              add       1             §CTRNO            7 0
024600030822     C                   if        wGiro = 2
024700120123     C  N31              EXSR      CHKCOR
024800120123     C  N31              EXSR      WRIORI
024900120123     C  N31              EXSR      WRIVGD
025000120123     C  N31              write     FNVAO000
025100150923     C*
025200150923     C* Se richiesta scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
025300150923     C                   if        *in53 and not *in31
025400151019     C                   exsr      EXEFORZ
025500150923     C                   call      'TIS7T7ER'
025600150923     C                   parm      'W'           pIn_Opz           1
025700150923     C                   parm                    tivlrds
025800150923     C                   parm                    FNVAOEAR
025900150923     C                   parm                    oriPRG
026000150923     C                   parm      *zeros        pIn_Dst           3
026100150923     C                   parm      *blanks       pOut_Esito        1
026200150923     C                   endif
026300150923     C                   if        pOut_Esito = '2'
026400150923     C                   seton                                        32
026500150923     C                   endif
026600030822     C                   endif
026700010604     C*
026800010604     C                   if        *in31 = *off and
026900010604     C                             *in32 = *off
027000010604     C                   eval      vinflg = '1'
027100010604     C                   else
027200010604     C                   eval      vinflg = '2'
027300010604     C                   endif
027400010604     C                   endif
027500010601     C*
027600010604     C                   endif
027700010604     C*
027800010330     C                   else
027900010330     C                   eval      vinflg = '1'
028000010330     C                   endif
028100010601     C*
028200010601     C  N70              update    tivin000
028300010330     C*
028400030822     C  N70              enddo
028500150923     C*
028600150923     C     FINE          tag
028700150923     C*
028800150923     C* Chiusura per scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
028900150923     C                   call      'TIS7T7ER'
029000150923     C                   parm      'C'           pIn_Opz           1
029100150923     C                   parm                    tivlrds
029200150923     C                   parm                    FNVAOEAR
029300150923     C                   parm                    oriPRG
029400150923     C                   parm      *zeros        pIn_Dst           3
029500150923     C                   parm      *blanks       pOut_Esito        1
029600010601     C*
029700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
029800010601     C                   if        cntNonEl = *zeros or
029900010601     C                             flgMulti = '0'
030000010330     C* Se non ci sono record con errori ...
030100010601     C                   if        §ctrno = 0 and
030200010604     C                             §ctrmo = 0 and
030300010601     C                             flgStato <> '2'
030400010330     C* ... restituisco esito OK.
030500010330     C                   eval      wrkesito = '0'
030600010330     C                   else
030700010330     C                   if        §ctrok > 0
030800010330     C                   eval      wrkesito = '1'
030900010330     C                   else
031000010615     C                   if        flgOk = '0'
031100010615     C                   eval      wrkesito = '2'
031200010615     C                   else
031300010615     C                   eval      wrkesito = '6'
031400010615     C                   endif
031500010330     C                   endif
031600010330     C                   endif
031700010601     C                   else
031800010601     C                   eval      wrkesito = '9'
031900010601     C                   endif
032000010330     C*
032100010330     C                   if        %open(tivin00r)
032200010330     C                   close     tivin00r
032300010330     C                   endif
032400040708     C                   if        %open(fnvaowwr)
032500040708     C                   close     fnvaowwr
032600010330     C                   endif
032700010601     C*
032800010601     C                   if        vlrpoi <> 999
032900010601     C                   eval      invfil = vlrpoi
033000010601     C                   endif
033100010330     C*
033200010330     C                   if        §ctrok > 0
033300010601     C                             and invfil > *zeros
033400010330     C                   exsr      invio
033500150923     C*
033600150923     C* Invio dati estensione ORM (FNVAOE0F) => richiamo l'apposito driver
033700150923     C                   move      invfil        cmzdst
033800150923     C                   call      'TIS7T7ER'
033900150923     C                   parm      'S'           pIn_Opz           1
034000150923     C                   parm                    tivlrds
034100150923     C                   parm                    FNVAOEAR
034200150923     C                   parm                    oriPRG
034300150923     C                   parm      cmzdst        pIn_Dst           3
034400150923     C                   parm      *blanks       pOut_Esito        1
034500010330     C                   endif
034600010601     C*
034700010618     C                   if        flgGiro = '1'
034800010601     C                   exsr      endela
034900010618     C                   endif
035000010330     C*
035100010330     C                   ENDSR
035200010330     C***
035300040723
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
038100151019
038200151019
038300151019     C*------------------------------------------------------------------------*
038400151019     C* EXEFORZ - Eseguo forzatire di base su formato ORM "esteso"
038500151019     C*------------------------------------------------------------------------*
038600151019     C     EXEFORZ       BEGSR
038700151019     C*
038800151019     C                   move(p)   VAOPOE        w_vaopoe                       * forzatura necessar
038900151019     C                   move(p)   VAOCOR        w_vaocor                       * forzatura necessar
039000151019     C                   move(p)   VAONOR        w_vaonor                       * forzatura necessar
039100151019     C*
039200151019     C                   ENDSR
039300040723
039400151019
039500040723     C*------------------------------------------------------------------------*
039600040723     C* CHKCOR - Considerazioni su VAOCOR
039700040723     C*------------------------------------------------------------------------*
039800040723     C     CHKCOR        BEGSR
039900040723     C*
040000040723     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
040100040723     C                   if        vaoTCO = 'F'
040200040723     C* Quindi verifico se il codice ordinante non è valorizzato
040300040723     C                   if        vaoCOR = *zeros  AND
040400040723     C                             vaoRSO = *blanks AND
040500040723     C                             vaoINO = *blanks AND
040600040723     C                             vaoCAO = *blanks AND
040700040723     C                             vaoLOO = *blanks AND
040800040723     C                             vaoPRO = *blanks AND
040900040723     C                             vaoNAO = *blanks
041000040723     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
041100040723     C                   movel     '0'           wFlgAcr           1
041200040723     C                   move      vlrKSC        wCli              7 0
041300040723     C                   move(p)   wCli          acrCRO
041400040723     C     acrCRO        setll     fnacr01l
041500040723     C                   if        %found(fnacr01l)
041600040723     C                   read      fnacr01l
041700040723     C                   dow       not %eof(fnacr01l)
041800040723     C                   movel     acrCRO        wCliAcr           7 0
041900040723     C                   if        wCliAcr = wCli
042000040723     C                   eval      vaoCOR = acrCRO
042100040723     C                   movel     '1'           wFlgAcr
042200040723     C                   leave
042300040723     C                   else
042400040723     C                   leave
042500040723     C                   endif
042600040723     C                   read      fnacr01l
042700040723     C                   enddo
042800040723     C                   endif
042900040723     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
043000040723     C* dati anagrafici dell'ordinante dal piano dei conti
043100040723     C                   if        wFlgAcr = '0'
043200040723     C                   clear                   BS69DS
043300040723     C                   clear                   ACODS
043400040723     C                   clear                   INDDS
043500040723     C                   clear                   CLPDS
043600040723     C                   clear                   CLSDS
043700040723     C                   move(P)   vlrKSC        I69KAC
043800040723     C                   move(P)   vlrKSC        I69KIN
043900040723     C                   call      'TIBS69R'
044000040723     C                   parm                    BS69DS
044100040723     C                   parm                    ACODS
044200040723     C                   parm                    INDDS
044300040723     C                   parm                    CLPDS
044400040723     C                   parm                    CLSDS
044500040723     C     O69ERR        ifne      '1'
044600040723     C                   eval      vaoRSO = ACORAG
044700040723     C                   eval      vaoINO = INDVIA
044800040723     C                   movel(P)  INDCAP        vaoCAO
044900040723     C                   eval      vaoLOO = INDCIT
045000040723     C                   eval      vaoPRO = INDPRV
045100040723     C                   eval      vaoNAO = INDSTA
045200040723     C                   endif
045300040723     C                   endif
045400040723     C                   endif
045500040723     C                   endif
045600040723     C*
045700040723     C                   ENDSR
045800040723
045900040723
046000040723     C*------------------------------------------------------------------------*
046100040723     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
046200040723     C*------------------------------------------------------------------------*
046300040723     C     WRIORI        BEGSR
046400040723     C*
046500090618     C* Come prima cosa stacco un numeratore da AZNUM
046600151019     C***                clear                   trul33ds
046700151019     C***                eval      I33OPE = *zeros
046800151019     C***                eval      I33CNU = 600
046900151019     C***                eval      I33NUM = 1
047000151019     C***                movel     TRUL33DS      KPJBU
047100151019     C***                call      'TRUL33R'
047200151019     C***                parm                    KPJBA
047300151019     C***                movel     KPJBU         TRUL33DS
047400151019     C***                if        O33ERR = *zeros
047500151019     C***                z-add     O33NRF        §numori
047600151019     c***                else
047700151019     c***                eval      §numori = *all'9'
047800151019     C***                endif
047900040723     C*
048000040723     C* Quindi imposto i campi "extra-VAO"
048100040723     C                   if        vlrKSC = '0ORM0000'
048200040723     C*
048300040723     C                   if        vaoCOR > *zeros
048400040723     C                   movel     vaoCOR        wrkCOR7           7 0
048500040723     C*
048600040723     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
048700040723     C* codice padre. Se è un padre assegno già il codice ordinante.
048800040723     C                   clear                   tibs10ds
048900040723     C                   eval      d10drf = datcor                              *data riferimento
049000040723     C                   eval      d10paf = 'P'                                 *cerca il padre
049100040723     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
049200091026     C*
049300091026     C* Preparazione chiamata al TIBS10R1
049400091026     C                   clear                   tibs10r1ds
049500091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
049600091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
049700091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
049800091026     C*
049900091026     C                   call      'TIBS10R1'
050000091026     C                   parm                    tibs10r1ds
050100091026     C*
050200091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
050300040723     C*
050400040723     C                   if        d10err <> *blanks or                         *No unif. o padre
050500040723     C                             d10cop =  *zeros  or
050600040723     C                             d10cop =  d10cod
050700040723     C                   move      d10cod        oriIDC
050800040723     C                   else                                                   *Figlio
050900040723     C                   move      d10cop        oriIDC
051000040723     C                   endif
051100040723     C*
051200040723     C                   else
051300040723     C                   eval      oriIDC = *zeros
051400040723     C                   endif
051500040723     C*
051600040723     C                   else
051700040723     C                   eval      oriIDC = vlrKSC
051800040723     C                   endif
051900040723     C*
052000040723     C                   eval      oriPRG = §numori
052100040723     C                   eval      oriDER = datcor
052200040723     C                   eval      oriOER = wn6
052300040723     C                   eval      oriPOE = vaoPOE
052400040723     C                   eval      oriNSR = vaoNSR
052500040723     C                   eval      oriNOR = vaoNOR
052600040723     C                   eval      oriNRV = vaoNRV
052700040723     C                   eval      oriFLO = *blanks
052800040723     C*
052900040723     C* Infine valorizzo la chiave esterna sul file FNVAO
053000150923     C* per ora VAOFLO è blank
053100150923     C                   eval      vaoflo = *blank
053200040723     C                   eval      dorm01 = vaoFLO
053300151210     C                   movel     §numori       §ormpg
053400150923     C*                  EVAL      §ORMCB = 'N'
053500150923     C*                  EVAL      §ORMFR = 'S'
053600150923     C*                  EVAL      §ORMKS = 'N'
053700150923     C*                  EVAL      §ORMFD = 'S'
053800050412     C*
053900050412     C* Infine valorizzo i flag operativi necessari x la particolare gestione corrente
054000130326     C*                   in questo caso non ci sono default
054100050412     C*
054200130326     C* Riassegno il campo VAO contenete i flag operativi precedentemente ridefiniti
054300040723     C                   eval      vaoFLO = dorm01
054400130326     C* Infine, per sicurezza, pulisco alcuni flag che devono essere a blank
054500130326     C                   eval      %subst(VAOFLO:5:1) = *blanks
054600130326     C                   eval      %subst(VAOFLO:6:1) = *blanks
054700040723     C*
054800040723     C* ...quindi scrivo il file TIORI00F
054900120905     C  N55              WRITE     TIORI000
055000040723     C*
055100040723     C                   ENDSR
055200050826
055300050826
055400050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
055500050826     C     wriVGD        BEGSR
055600050826     C*
055700050826     C* Reperisco la descrizione della fase da tabella
055800050826     C                   movel(p)  'FAR'         KteCOD
055900050826     C                   movel(p)  '000'         KteKE1
056000050826     C     KEYtbe        chain     tntbe01l
056100050826     C                   if        %found(tntbe01l)
056200050826     C                   movel(P)  tbeuni        DFAR
056300050826     C                   else
056400050826     C                   clear                   DFAR
056500050826     C                   endif
056600050826     C*
056700050826     C* Valorizzo buffer record
056800050826     C                   clear                   FNVAPDS
056900050826     C                   eval      vapIDC = oriIDC
057000050826     C                   eval      vapPOE = oriPOE
057100050826     C                   eval      vapRFA = vaoRFA
057200050826     C                   eval      vapPOG = oriPOE
057300050826     C                   eval      vapDAE = oriDER
057400050826     C                   eval      vapORE = oriOER
057500050826     C                   if        d§fardva <> *blanks
057600050826     C                   eval      vapDFA = d§fardva
057700050826     C                   else
057800050826     C                   eval      vapDFA = d§fardes
057900050826     C                   endif
058000050826     C*
058100050826     C                   clear                   tivgd000
058200080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
058300050826     C                   eval      vgdTIP = 'VP'
058400050826     C                   eval      vgdKSU = vapIDC
058500050826     C                   eval      vgdTSC = 'WW'
058600050826     C                   eval      vgdDAT = datcor
058700151207     C                   eval      vgdPGM = procname
058800060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
058900060306     C                   exsr      sr_Control
059000060306     C                   if        Cli_Abil = *on
059100060306     C                   eval      vgdKSU = woriIDC
059200120905     C  N55              write     tivgd000
059300060306     C                   endif
059400050826     C*
059500050826     C                   ENDSR
059600050826     C*------------------------------------------------------------------------*
059700010601
059800010601
059900010330     C*----------------------------------------------------*
060000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
060100010330     C*----------------------------------------------------*
060200010330     C     INZVAR        BEGSR
060300010330     C*
060400030822     C* Inizializzo variabili di wrk
060500020204     C                   Z-ADD     *zeros        Num5_0            5 0
060600120124
060700030822     C*
060800030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
060900120123     C                   CLEAR                   FNVAO000
061000150923     C                   CLEAR                   FNVAOEAR
061100120123     C                   CLEAR                   TIORI000
061200030822     C                   CLEAR                   SkSplitCSV
061300030822     C                   EVAL      i = 1
061400030822     C                   EVAL      posDa = *zeros
061500030822     C                   EVAL      posA  = *zeros
061600030822     C*
061700030822     C* Reimposto i valori di default
061800030822     C                   EXSR      DEFCAM
061900030822     C*
062000010330     C                   ENDSR
062100010330     C*----------------------------------------------------*
062200030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
062300010330     C*----------------------------------------------------*
062400010330     C     DEFCAM        BEGSR
062500030822     C*
062600030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
062700030715     C* e delimitatore testo.
062800030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
062900030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
063000030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
063100040723     C*
063200080707     C* Imposto i valori d default d alcuni campi
063300080708     C*
063400080708     C                   CLEAR                   DORM01
063500120822     C                   EVAL      VAOFLO = DORM01
063600080707     C*
063700151124     C                   EVAL      VAOPOE = 020
063800151124     C                   EVAL      VAOTOR = 'S'
063900151124     C                   EVAL      VAOCOR = 0192344000
064000151124     C                   EVAL      VAOKSC = 0192344
064100151125     C                   EVAL      VAOCTR = '102'
064200151111     C                   EVAL      VAOPAG = 'O'
064300151124     C                   EVAL      VAOORR = 0900
064400151124     C                   EVAL      VAONAM = 'VARIE'
064500130513
064600120905     C* se il 1° char dei parametri è T=Test
064700120905     C                   SETOFF                                       55
064800120905     C                   IF        %subst(vlrppt:1:1) = 'T'
064900120905     C* inibisco la scrittura dei file TIORI e TIVGD
065000120905     C                   SETON                                        55
065100120905     C* imposto il PO di emissione come filiale fittizia
065200120905     C                   EVAL      VAOPOE = 999
065300120905     C                   ENDIF
065400091127     C*
065500091204     C* IMPOSTO I CAMPI PERSONALIZZATI
065600100416     C
065700030822     C*
065800030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
065900030822     C                   EVAL      posDaDft = 1
066000030822     C                   EVAL      posADft  = 0
066100030822     C                   EVAL      wGiroDft = 0
066200030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
066300030822     C                             posDaDft > 0
066400030822     C*
066500030822     C* Gestisco il 1° giro
066600030822     C                   IF        wGiroDft = 0
066700030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
066800030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
066900030822     C* Incremento il contatore dei "giri"
067000030822     C                   EVAL      wGiroDft = 1
067100030822     C                   ELSE
067200030822     C                   EVAL      posDaDft = posADft
067300030822     C                   ENDIF
067400030822     C* Eseguo lo scan x trovare la fine del campo corrente
067500030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
067600030822     C*
067700030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
067800030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
067900030822     C                   IF        posDaDft > 0 AND
068000030822     C                             posADft  > 0
068100040708     C* POE
068200040708     C                   IF        %subst(
068300040708     C                             %subst(vlrppt:posDaDft+1:
068400040708     C                             posADft-posDaDft-1):1:3)
068500040708     C                             = 'POE'
068600040708     C                   EVAL      PiStr=%trim(%subst(
068700040708     C                             %subst(vlrppt:posDaDft+1:
068800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068900040708     C                   EXSR      CHKNUM
069000040708     C                   IF        PiInt=*on
069100040708     C                   Z-ADD     PiVal         VAOPOE
069200040708     C                   ENDIF
069300040708     C                   ENDIF
069400040708     C* TOR
069500040708     C                   IF        %subst(
069600040708     C                             %subst(vlrppt:posDaDft+1:
069700040708     C                             posADft-posDaDft-1):1:3)
069800040708     C                             = 'TOR'
069900040708     C                   EVAL      VAOTOR=%trim(%subst(
070000040708     C                             %subst(vlrppt:posDaDft+1:
070100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070200040708     C                   ENDIF
070300040708     C* TCO
070400040708     C                   IF        %subst(
070500040708     C                             %subst(vlrppt:posDaDft+1:
070600040708     C                             posADft-posDaDft-1):1:3)
070700040708     C                             = 'TCO'
070800040708     C                   EVAL      VAOTCO=%trim(%subst(
070900040708     C                             %subst(vlrppt:posDaDft+1:
071000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071100040708     C                   ENDIF
071200040707     C* COR
071300030822     C                   IF        %subst(
071400030822     C                             %subst(vlrppt:posDaDft+1:
071500030822     C                             posADft-posDaDft-1):1:3)
071600040707     C                             = 'COR'
071700030822     C                   EVAL      PiStr=%trim(%subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072000030822     C                   EXSR      CHKNUM
072100030822     C                   IF        PiInt=*on
072200040708     C                   Z-ADD     PiVal         VAOCOR
072300030822     C                   ENDIF
072400030822     C                   ENDIF
072500040708     C* CRC
072600040708     C                   IF        %subst(
072700040708     C                             %subst(vlrppt:posDaDft+1:
072800040708     C                             posADft-posDaDft-1):1:3)
072900040708     C                             = 'CRC'
073000040708     C                   EVAL      PiStr=%trim(%subst(
073100040708     C                             %subst(vlrppt:posDaDft+1:
073200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073300040708     C                   EXSR      CHKNUM
073400040708     C                   IF        PiInt=*on
073500040708     C                   Z-ADD     PiVal         VAOCRC
073600040708     C                   ENDIF
073700040708     C                   ENDIF
073800040708     C* FFD
073900040708     C                   IF        %subst(
074000040708     C                             %subst(vlrppt:posDaDft+1:
074100040708     C                             posADft-posDaDft-1):1:3)
074200040708     C                             = 'FFD'
074300040708     C                   EVAL      VAOFFD=%trim(%subst(
074400040708     C                             %subst(vlrppt:posDaDft+1:
074500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074600040708     C                   ENDIF
074700040708     C* ORR
074800040708     C                   IF        %subst(
074900040708     C                             %subst(vlrppt:posDaDft+1:
075000040708     C                             posADft-posDaDft-1):1:3)
075100040708     C                             = 'ORR'
075200040708     C                   EVAL      PiStr=%trim(%subst(
075300040708     C                             %subst(vlrppt:posDaDft+1:
075400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075500040708     C                   EXSR      CHKNUM
075600040708     C                   IF        PiInt=*on
075700040708     C                   Z-ADD     PiVal         VAOORR
075800040708     C                   ENDIF
075900040708     C                   ENDIF
076000040712     C* NCL
076100040712     C                   IF        %subst(
076200040712     C                             %subst(vlrppt:posDaDft+1:
076300040712     C                             posADft-posDaDft-1):1:3)
076400040712     C                             = 'NCL'
076500040712     C                   EVAL      PiStr=%trim(%subst(
076600040712     C                             %subst(vlrppt:posDaDft+1:
076700040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076800040712     C                   EXSR      CHKNUM
076900040712     C                   IF        PiInt=*on
077000040712     C                   Z-ADD     PiVal         VAONCL
077100040712     C                   ENDIF
077200040712     C                   ENDIF
077300040712     C* PKG
077400040712     C                   IF        %subst(
077500040712     C                             %subst(vlrppt:posDaDft+1:
077600040712     C                             posADft-posDaDft-1):1:3)
077700040712     C                             = 'PKG'
077800040712     C                   EVAL      PiStr=%trim(%subst(
077900040712     C                             %subst(vlrppt:posDaDft+1:
078000040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078100040712     C                   EXSR      CHKNUM
078200040712     C                   IF        PiNum=*on
078300040712     C                   Z-ADD     PiVal         VAOPKG
078400040712     C                   ENDIF
078500040712     C                   ENDIF
078600040712     C* VLM
078700040712     C                   IF        %subst(
078800040712     C                             %subst(vlrppt:posDaDft+1:
078900040712     C                             posADft-posDaDft-1):1:3)
079000040712     C                             = 'VLM'
079100040712     C                   EVAL      PiStr=%trim(%subst(
079200040712     C                             %subst(vlrppt:posDaDft+1:
079300040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079400040712     C                   EXSR      CHKNUM
079500040712     C                   IF        PiNum=*on
079600040712     C                   Z-ADD     PiVal         VAOVLM
079700040712     C                   ENDIF
079800040712     C                   ENDIF
079900040708     C* NAM
080000040708     C                   IF        %subst(
080100040708     C                             %subst(vlrppt:posDaDft+1:
080200040708     C                             posADft-posDaDft-1):1:3)
080300040708     C                             = 'NAM'
080400040708     C                   EVAL      VAONAM=%trim(%subst(
080500040708     C                             %subst(vlrppt:posDaDft+1:
080600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080700040708     C                   ENDIF
080800040708     C* PAG
080900030822     C                   IF        %subst(
081000030822     C                             %subst(vlrppt:posDaDft+1:
081100030822     C                             posADft-posDaDft-1):1:3)
081200040708     C                             = 'PAG'
081300040708     C                   EVAL      VAOPAG=%trim(%subst(
081400030822     C                             %subst(vlrppt:posDaDft+1:
081500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081600030822     C                   ENDIF
081700040708     C* KSC
081800040708     C                   IF        %subst(
081900040708     C                             %subst(vlrppt:posDaDft+1:
082000040708     C                             posADft-posDaDft-1):1:3)
082100040708     C                             = 'KSC'
082200040708     C                   EVAL      PiStr=%trim(%subst(
082300040708     C                             %subst(vlrppt:posDaDft+1:
082400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082500040708     C                   EXSR      CHKNUM
082600040708     C                   IF        PiInt=*on
082700040708     C                   Z-ADD     PiVal         VAOKSC
082800040708     C                   ENDIF
082900040708     C                   ENDIF
083000040708     C* CTR
083100040708     C                   IF        %subst(
083200040708     C                             %subst(vlrppt:posDaDft+1:
083300040708     C                             posADft-posDaDft-1):1:3)
083400040708     C                             = 'CTR'
083500040708     C                   EVAL      VAOCTR=%trim(%subst(
083600040708     C                             %subst(vlrppt:posDaDft+1:
083700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083800040708     C                   ENDIF
083900040708     C* POC
084000040708     C                   IF        %subst(
084100040708     C                             %subst(vlrppt:posDaDft+1:
084200040708     C                             posADft-posDaDft-1):1:3)
084300040708     C                             = 'POC'
084400040708     C                   EVAL      PiStr=%trim(%subst(
084500040708     C                             %subst(vlrppt:posDaDft+1:
084600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084700040708     C                   EXSR      CHKNUM
084800040708     C                   IF        PiInt=*on
084900040708     C                   Z-ADD     PiVal         VAOPOC
085000040708     C                   ENDIF
085100040708     C                   ENDIF
085200040708     C* NO1
085300040708     C                   IF        %subst(
085400040708     C                             %subst(vlrppt:posDaDft+1:
085500121129     C                             posADft-posDaDft-1):1:3)
085600040708     C                             = 'NO1'
085700040708     C                   EVAL      VAONO1=%trim(%subst(
085800040708     C                             %subst(vlrppt:posDaDft+1:
085900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086000040708     C                   ENDIF
086100040708     C* DDT
086200030822     C                   IF        %subst(
086300030822     C                             %subst(vlrppt:posDaDft+1:
086400030822     C                             posADft-posDaDft-1):1:3)
086500040708     C                             = 'DDT'
086600040708     C                   EVAL      VAODDT=%trim(%subst(
086700030822     C                             %subst(vlrppt:posDaDft+1:
086800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086900030822     C                   ENDIF
087000040708     C* RFA
087100040708     C                   IF        %subst(
087200040708     C                             %subst(vlrppt:posDaDft+1:
087300040708     C                             posADft-posDaDft-1):1:3)
087400040708     C                             = 'RFA'
087500040708     C                   EVAL      VAORFA=%trim(%subst(
087600040708     C                             %subst(vlrppt:posDaDft+1:
087700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
087800040708     C                   ENDIF
087900040708     C* FLO
088000040708     C                   IF        %subst(
088100040708     C                             %subst(vlrppt:posDaDft+1:
088200040708     C                             posADft-posDaDft-1):1:3)
088300040708     C                             = 'FLO'
088400040708     C                   EVAL      VAOFLO=%trim(%subst(
088500040708     C                             %subst(vlrppt:posDaDft+1:
088600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
088700091103     C*
088800040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
088900080702     C* e posizione 7 del VAOFLO
089000040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
089100040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
089200050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
089300040708     C                   ENDIF
089400030822     C* ...
089500030822     C                   ENDIF
089600030822     C                   ENDDO
089700020204     C*
089800010330     C                   ENDSR
089900010607     C*----------------------------------------------------*
090000040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
090100010607     C*----------------------------------------------------*
090200040708     C     IMPVAO        BEGSR
090300010607     C*
090400010607     C                   SETOFF                                       3132
090500120124
090600030515     C*
090700040708     C                   MOVEL     datcor        VAODAO
090800040708     C                   MOVEL     wn14          VAOOAO
090900050314     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
091000050314     C     minu:maiu     XLATE     vindta        vindta
091100010607     C*
091200030715     C**********
091300030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
091400030715     C**********
091500030715     C                   IF        wGiro = *zeros
091600151125     C*
091700151125     C* da analisi il cliente passa 2 righe di intestazione di colonna
091800151125     C* la prima riga non sono vere intestazioni, ma il cliente la passa lo stesso e noi dobbiamo
091900151125     C* saltarla
092000151125     C                   IF        wSaltaRiga = *off
092100040709     C*
092200040709     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
092300040709     C                   EVAL      depvinDTA = vinDTA
092400040709     C*
092500151111     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
092600151111     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
092700151111     C                             %trim(intcol(3))<>*blanks
092800040709     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
092900040709     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
093000040709     C                                      %trim(intcol(3))
093100151111     C     ';':CharCSV   XLATE     vindta        vindta
093200151111     C                   ENDIF
093300031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
093400050314     C**!!!minu:maiu     XLATE     vindta        vindta
093500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
093600040324     C                   DOW       posDa <= %len(%trimr(vindta))
093700030715     C*
093800030715     C* Gestisco il 1° campo
093900030715     C                   IF        i = 1
094000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
094100030715     C                   EVAL      posDa = 1
094200030715     C* Eseguo lo scan x trovare la fine del primo campo
094300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094400030715     C* A questo punto "estraggo" il campo corrente
094500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
094600030715     C                                                    (posA-posDa))
094700030715     C* X i campi successivi al 1°
094800030715     C                   ELSE
094900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
095000030715     C                   EVAL      posDa = posA + 1
095100030715     C* Eseguo lo scan x trovare la fine del campo corrente
095200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
095300030715     C* Gestisco l'ultimo campo
095400030715     C                   IF        posA = *zeros
095500040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
095600030715     C* A questo punto "estraggo" il campo corrente
095700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
095800030715     C                                                    (posA-posDa))
095900030715     C                   LEAVE
096000030715     C                   ELSE
096100030715     C* A questo punto "estraggo" il campo corrente
096200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
096300030715     C                                                    (posA-posDa))
096400030715     C                   ENDIF
096500030715     C                   ENDIF
096600030715     C* Incremento il contatore d campo
096700030715     C                   EVAL      i = i +1
096800030715     C                   ENDDO
096900030715     C*
097000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
097100030715     C* delimitazione testo
097200030715     C                   EVAL      i = 1
097300030715     C                   DOW       i < %elem(SkSplitFLD)
097400030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
097500030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
097600030715     C                   EVAL      i = i + 1
097700030715     C                   ENDDO
097800030715     C                   EVAL      wGiro = 1
097900040709     C* Ripristino la 1° riga originale
098000040709     C                   EVAL      vinDTA = depvinDTA
098100151125     C                   ELSE
098200151125     C                   EVAL      wSaltaRiga = *off
098300151125     C                   ENDIF
098400030715     C                   ELSE
098500030715     C                   EVAL      wGiro = 2
098600031204     C**********
098700031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
098800031204     C* d separatore campo
098900031204     C**********
099000040709     C                   EVAL      vindta = %trim(vindta)
099100040709     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
099200040709     C                   EVAL      vindta = %subst(vindta:2)
099300040709     C                   ENDDO
099400040709     C*
099500040709     C                   Z-ADD     *zeros        lunghInput        4 0
099600040709     C                   EVAL      lunghInput = %len(%trim(vindta))
099700040709     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
099800040709     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
099900040709     C                   EVAL      lunghInput = %len(%trim(vindta))
100000040709     C                   ENDDO
100100030715     C**********
100200030715     C* Effettuo lo split del campo dati d input
100300030715     C**********
100400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
100500040324     C                   DOW       posDa <= %len(%trimr(vindta))
100600030715     C*
100700030715     C* Gestisco il 1° campo
100800030715     C                   IF        i = 1
100900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
101000030715     C                   EVAL      posDa = 1
101100030715     C* Eseguo lo scan x trovare la fine del primo campo
101200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
101300030715     C* A questo punto "estraggo" il campo corrente
101400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
101500030715     C                                                    (posA-posDa))
101600030715     C* X i campi successivi al 1°
101700030715     C                   ELSE
101800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
101900030715     C                   EVAL      posDa = posA + 1
102000031204     C* Verifico che nn vi sia il campo nn valorizzato
102100031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
102200031204     C* Se campo nn valorizzato skippo al prossimo
102300031204     C                   EVAL      SkSplitCSV(i) = *blanks
102400031204     C                   EVAL      posA  = posA + 1
102500031204     C                   ELSE
102600030715     C* Eseguo lo scan x trovare la fine del campo corrente
102700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
102800030715     C* Gestisco l'ultimo campo
102900030715     C                   IF        posA = *zeros
103000040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
103100030715     C* A questo punto "estraggo" il campo corrente
103200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
103300030715     C                                                    (posA-posDa))
103400030715     C                   LEAVE
103500030715     C                   ELSE
103600030715     C* A questo punto "estraggo" il campo corrente
103700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
103800030715     C                                                    (posA-posDa))
103900030715     C                   ENDIF
104000031204     C                   ENDIF
104100030715     C                   ENDIF
104200030715     C* Incremento il contatore d campo
104300030715     C                   EVAL      i = i +1
104400030715     C                   ENDDO
104500030715     C*
104600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
104700030715     C* delimitazione testo
104800030715     C                   EVAL      i = 1
104900030715     C                   DOW       i < %elem(SkSplitCSV)
105000030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
105100030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
105200030715     C                   EVAL      i = i + 1
105300030715     C                   ENDDO
105400030715     C*
105500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
105600030715     C                   EVAL      i = 1
105700030715     C                   DOW       i < %elem(SkSplitFLD)
105800030715     C*
105900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
106000030715     C                   IF        SkSplitFLD(i) <> *blanks AND
106100030715     C                             SkSplitCSV(i) <> *blanks
106200030721     C*** ATB
106300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATB'
106400040708     C                   EVAL      VAOATB = SkSplitCSV(i)
106500030721     C                   ENDIF
106600040708     C*** TOR
106700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTOR'
106800040708     C                   EVAL      VAOTOR = SkSplitCSV(i)
106900040708     C                   ENDIF
107000040708     C*** TCO
107100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTCO'
107200040708     C                   EVAL      VAOTCO = SkSplitCSV(i)
107300040708     C                   ENDIF
107400040708     C*** RSO
107500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSO'
107600040708     C                   EVAL      VAORSO = SkSplitCSV(i)
107700030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
107800040708     C     '@':'A'       XLATE     VAORSO        VAORSO
107900030715     C* ==
108000030715     C                   ENDIF
108100040708     C*** INO
108200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINO'
108300040708     C                   EVAL      VAOINO = SkSplitCSV(i)
108400030715     C                   ENDIF
108500040708     C*** LOO
108600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOO'
108700040708     C                   EVAL      VAOLOO = SkSplitCSV(i)
108800030715     C                   ENDIF
108900040708     C*** PRO
109000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRO'
109100040708     C                   EVAL      VAOPRO = SkSplitCSV(i)
109200030715     C                   ENDIF
109300040708     C*** NAO
109400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAO'
109500040708     C                   EVAL      VAONAO = SkSplitCSV(i)
109600030721     C                   ENDIF
109700040708     C*** RSR
109800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSR'
109900040708     C                   EVAL      VAORSR = SkSplitCSV(i)
110000040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
110100040708     C     '@':'A'       XLATE     VAORSR        VAORSR
110200040708     C* ==
110300040708     C                   IF        VAORSR = *blanks
110400040708     C                   SETON                                        31
110500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
110600040708     C                             + ' ' + 'VAORSR'
110700040708     C                   ENDIF
110800040708     C                   ENDIF
110900040708     C*** INR
111000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINR'
111100040708     C                   EVAL      VAOINR = SkSplitCSV(i)
111200040708     C                   IF        VAOINR = *blanks
111300040708     C                   SETON                                        31
111400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
111500040708     C                             + ' ' + 'VAOINR'
111600040708     C                   ENDIF
111700040708     C                   ENDIF
111800040708     C*** LOR
111900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOR'
112000040708     C                   EVAL      VAOLOR = SkSplitCSV(i)
112100040708     C                   IF        VAOLOR = *blanks
112200040708     C                   SETON                                        31
112300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
112400040708     C                             + ' ' + 'VAOLOR'
112500040708     C                   ENDIF
112600040708     C                   ENDIF
112700040708     C*** PRR
112800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRR'
112900040708     C                   EVAL      VAOPRR = SkSplitCSV(i)
113000040708     C                   IF        VAOPRR = *blanks
113100040708     C                   SETON                                        31
113200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
113300040708     C                             + ' ' + 'VAOPRR'
113400040708     C                   ENDIF
113500040708     C                   ENDIF
113600040708     C*** NAR
113700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAR'
113800040708     C                   EVAL      VAONAR = SkSplitCSV(i)
113900040708     C                   ENDIF
114000040708     C*** RER
114100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORER'
114200040708     C                   EVAL      VAORER = SkSplitCSV(i)
114300040708     C                   ENDIF
114400040708     C*** TER
114500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTER'
114600040708     C                   EVAL      VAOTER = SkSplitCSV(i)
114700040708     C                   ENDIF
114800151124     C*** ORDA1
114900151124     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORDA1'
115000151124     C                   EVALR     w_VAOORDA1 = %trim(SkSplitCSV(i))
115100151124     C                   ENDIF
115200151124     C*** ORAA1
115300151124     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORAA1'
115400151124     C                   EVALR     w_VAOORAA1 = %trim(SkSplitCSV(i))
115500151124     C                   ENDIF
115600151124     C*** ORDA2
115700151124     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORDA2'
115800151124     C                   EVALR     w_VAOORDA2 = %trim(SkSplitCSV(i))
115900151124     C                   ENDIF
116000151124     C*** ORAA2
116100151124     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORAA2'
116200151124     C                   EVALR     w_VAOORAA2 = %trim(SkSplitCSV(i))
116300151124     C                   ENDIF
116400150923     C*** EMLR
116500150923     C                   IF        %trim(SkSplitFLD(i)) = 'VAOEMLR'
116600150923     C                   EVAL      w_VAOEMLR = SkSplitCSV(i)
116700150923     C                   ENDIF
116800040708     C*** RSC
116900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSC'
117000040708     C                   EVAL      VAORSC = SkSplitCSV(i)
117100040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
117200040708     C     '@':'A'       XLATE     VAORSC        VAORSC
117300040708     C* ==
117400040708     C                   ENDIF
117500040708     C*** INC
117600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINC'
117700040708     C                   EVAL      VAOINC = SkSplitCSV(i)
117800040708     C                   ENDIF
117900040708     C*** LOC
118000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOC'
118100040708     C                   EVAL      VAOLOC = SkSplitCSV(i)
118200040708     C                   ENDIF
118300040708     C*** PRC
118400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRC'
118500040708     C                   EVAL      VAOPRC = SkSplitCSV(i)
118600040708     C                   ENDIF
118700040708     C*** NAC
118800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAC'
118900040708     C                   EVAL      VAONAC = SkSplitCSV(i)
119000040708     C                   ENDIF
119100040708     C*** FFD
119200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFFD'
119300040708     C                   EVAL      VAOFFD = SkSplitCSV(i)
119400040708     C                   ENDIF
119500040708     C*** RMP
119600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORMP'
119700040708     C                   EVAL      VAORMP = SkSplitCSV(i)
119800040708     C                   ENDIF
119900040708     C*** NAM
120000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAM'
120100040708     C                   EVAL      VAONAM = SkSplitCSV(i)
120200040708     C                   ENDIF
120300040708     C*** PAG
120400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPAG'
120500040708     C                   EVAL      VAOPAG = SkSplitCSV(i)
120600040708     C                   ENDIF
120700040708     C*** CTR
120800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCTR'
120900040708     C                   EVAL      VAOCTR = SkSplitCSV(i)
121000040708     C                   ENDIF
121100040708     C*** ZOR
121200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOZOR'
121300040708     C                   EVAL      VAOZOR = SkSplitCSV(i)
121400040708     C                   ENDIF
121500040708     C*** NO1
121600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO1'
121700040708     C                   EVAL      VAONO1 = SkSplitCSV(i)
121800030715     C                   ENDIF
121900040708     C*** NO2
122000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO2'
122100040708     C                   EVAL      VAONO2 = SkSplitCSV(i)
122200030715     C                   ENDIF
122300040708     C*** DDT
122400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODDT'
122500040708     C                   EVAL      VAODDT = SkSplitCSV(i)
122600030715     C                   ENDIF
122700040708     C*** RFA
122800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORFA'
122900040708     C                   EVAL      VAORFA = SkSplitCSV(i)
123000030715     C                   ENDIF
123100040708     C*** SPI
123200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSPI'
123300040708     C                   EVAL      VAOSPI = SkSplitCSV(i)
123400030715     C                   ENDIF
123500040708     C*** FLO
123600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
123700040708     C                   EVAL      VAOFLO = SkSplitCSV(i)
123800040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
123900080708     C* e la posizione 7 del VAOFLO
124000040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
124100040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
124200050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
124300030715     C                   ENDIF
124400091103     C
124500091103     C*** DAR
124600050314     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
124700050412     C                   EVAL      PiStr=SkSplitCSV(i)
124800050412     C                   EXSR      CHKNUM
124900050412     C                   IF        PiInt=*on
125000050412     C                   Z-ADD     PiVal         VAODAR
125100050412     C                   ELSE
125200050412     C                   SETON                                        32
125300050412     C                   EVAL      VAODAR = *zeros
125400050412     C                   EVAL      vinmsg = %trimr(vinmsg)
125500050412     C                             + ' ' + 'VAODAR'
125600050412     C                   ENDIF
125700050314     C                   ENDIF
125800030715     C
125900030721     C*
126000030721     C* Reperisco quindi i campi numerici...
126100040708     C*** POE
126200040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
126300040723     C                   EVAL      PiStr=SkSplitCSV(i)
126400040723     C                   EXSR      CHKNUM
126500040723     C                   IF        PiInt=*on    AND
126600040723     C                             PiVal<=999   AND
126700040723     C                             PiVal>*zeros
126800040723     C                   Z-ADD     PiVal         VAOPOE
126900040723     C                   ELSE
127000040723     C                   SETON                                        32
127100040723     C                   EVAL      VAOPOE = *zeros
127200040723     C                   EVAL      vinmsg = %trimr(vinmsg)
127300040723     C                             + ' ' + 'VAOPOE'
127400040723     C                   ENDIF
127500040723     C                   ENDIF
127600040708     C*** COR
127700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
127800040708     C                   EVAL      PiStr=SkSplitCSV(i)
127900040708     C                   EXSR      CHKNUM
128000040708     C                   IF        PiInt=*on
128100040708     C                   Z-ADD     PiVal         VAOCOR
128200040708     C                   ELSE
128300040708     C                   SETON                                        31
128400040708     C                   EVAL      VAOCOR = *zeros
128500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
128600040708     C                             + ' ' + 'VAOCOR'
128700040708     C                   ENDIF
128800040708     C                   ENDIF
128900040708     C*** CAO
129000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
129100030721     C                   EVAL      PiStr=SkSplitCSV(i)
129200030721     C                   EXSR      CHKNUM
129300030721     C                   IF        PiInt=*on
129400030721     C                   Z-ADD     PiVal         Num5_0
129500040708     C                   MOVEL(p)  Num5_0        VAOCAO
129600030721     C                   ELSE
129700030721     C                   SETON                                        32
129800040708     C                   EVAL      VAOCAO = *zeros
129900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130000040708     C                             + ' ' + 'VAOCAO'
130100030721     C                   ENDIF
130200030721     C                   ENDIF
130300040708     C*** CRA
130400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
130500030721     C                   EVAL      PiStr=SkSplitCSV(i)
130600030721     C                   EXSR      CHKNUM
130700030721     C                   IF        PiInt=*on
130800040708     C                   Z-ADD     PiVal         VAOCRA
130900030721     C                   ELSE
131000030721     C                   SETON                                        32
131100040708     C                   EVAL      VAOCRA = *zeros
131200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131300040708     C                             + ' ' + 'VAOCRA'
131400030721     C                   ENDIF
131500030721     C                   ENDIF
131600040708     C*** CAR
131700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
131800040708     C                   EVAL      PiStr=SkSplitCSV(i)
131900040708     C                   EXSR      CHKNUM
132000040708     C                   IF        PiInt=*on
132100040708     C                   Z-ADD     PiVal         Num5_0
132200040708     C                   MOVEL(p)  Num5_0        VAOCAR
132300040708     C                   ELSE
132400040708     C                   SETON                                        31
132500040708     C                   EVAL      VAOCAR = *zeros
132600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
132700040708     C                             + ' ' + 'VAOCAR'
132800040708     C                   ENDIF
132900040708     C                   ENDIF
133000040708     C*** CRC
133100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
133200040708     C                   EVAL      PiStr=SkSplitCSV(i)
133300040708     C                   EXSR      CHKNUM
133400040708     C                   IF        PiInt=*on
133500040708     C                   Z-ADD     PiVal         VAOCRC
133600040708     C                   ELSE
133700040708     C                   SETON                                        32
133800040708     C                   EVAL      VAOCRC = *zeros
133900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
134000040708     C                             + ' ' + 'VAOCRC'
134100040708     C                   ENDIF
134200040708     C                   ENDIF
134300040708     C*** CAC
134400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
134500040708     C                   EVAL      PiStr=SkSplitCSV(i)
134600040708     C                   EXSR      CHKNUM
134700040708     C                   IF        PiInt=*on
134800040708     C                   Z-ADD     PiVal         Num5_0
134900040708     C                   MOVEL(p)  Num5_0        VAOCAC
135000040708     C                   ELSE
135100040708     C                   SETON                                        32
135200040708     C                   EVAL      VAOCAC = *zeros
135300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
135400040708     C                             + ' ' + 'VAOCAC'
135500040708     C                   ENDIF
135600040708     C                   ENDIF
135700040708     C*** DAR
135800090428     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
135900090428     C                   EVAL      PiStr=SkSplitCSV(i)
136000090428     C                   EXSR      CHKNUM
136100090428     C                   IF        PiInt=*on
136200090428     C                   Z-ADD     PiVal         VAODAR
136300090428     C                   ELSE
136400090428     C                   SETON                                        32
136500090428     C                   EVAL      VAODAR = *zeros
136600090428     C                   EVAL      vinmsg = %trimr(vinmsg)
136700090428     C                             + ' ' + 'VAODAR'
136800090428     C                   ENDIF
136900090428     C                   ENDIF
137000040708     C*** ORR
137100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
137200040708     C                   EVAL      PiStr=SkSplitCSV(i)
137300040708     C                   EXSR      CHKNUM
137400040708     C                   IF        PiInt=*on
137500040708     C                   Z-ADD     PiVal         VAOORR
137600040708     C                   ELSE
137700040708     C                   SETON                                        32
137800040708     C                   EVAL      VAOORR = *zeros
137900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
138000040708     C                             + ' ' + 'VAOORR'
138100040708     C                   ENDIF
138200040708     C                   ENDIF
138300030721     C*** NCL
138400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
138500030721     C                   EVAL      PiStr=SkSplitCSV(i)
138600030721     C                   EXSR      CHKNUM
138700030721     C                   IF        PiInt=*on
138800040723     C                   IF        PiVal > *zeros
138900040708     C                   Z-ADD     PiVal         VAONCL
139000040723     C                   ENDIF
139100030721     C                   ELSE
139200030721     C                   SETON                                        32
139300040708     C                   EVAL      VAONCL = *zeros
139400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139500040708     C                             + ' ' + 'VAONCL'
139600030721     C                   ENDIF
139700030721     C                   ENDIF
139800040708     C*** PKG
139900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
140000030721     C                   EVAL      PiStr=SkSplitCSV(i)
140100030721     C                   EXSR      CHKNUM
140200030721     C                   IF        PiNum=*on
140300040723     C                   IF        PiVal > *zeros
140400040708     C                   Z-ADD     PiVal         VAOPKG
140500040723     C                   ENDIF
140600030721     C                   ELSE
140700030721     C                   SETON                                        32
140800040708     C                   EVAL      VAOPKG = *zeros
140900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141000040708     C                             + ' ' + 'VAOPKG'
141100030721     C                   ENDIF
141200030721     C                   ENDIF
141300040708     C*** VLM
141400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
141500040708     C                   EVAL      PiStr=SkSplitCSV(i)
141600040708     C                   EXSR      CHKNUM
141700040708     C                   IF        PiNum=*on
141800040708     C                   Z-ADD     PiVal         VAOVLM
141900040708     C                   ELSE
142000040708     C                   SETON                                        32
142100040708     C                   EVAL      VAOVLM = *zeros
142200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
142300040708     C                             + ' ' + 'VAOVLM'
142400040708     C                   ENDIF
142500040708     C                   ENDIF
142600040708     C*** BNC
142700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
142800040708     C                   EVAL      PiStr=SkSplitCSV(i)
142900040708     C                   EXSR      CHKNUM
143000040708     C                   IF        PiInt=*on
143100040708     C                   Z-ADD     PiVal         VAOBNC
143200040708     C                   ELSE
143300040708     C                   SETON                                        32
143400040708     C                   EVAL      VAOBNC = *zeros
143500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
143600040708     C                             + ' ' + 'VAOBNC'
143700040708     C                   ENDIF
143800040708     C                   ENDIF
143900040708     C*** BLC
144000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
144100040708     C                   EVAL      PiStr=SkSplitCSV(i)
144200040708     C                   EXSR      CHKNUM
144300040708     C                   IF        PiInt=*on
144400040708     C                   Z-ADD     PiVal         VAOBLC
144500040708     C                   ELSE
144600040708     C                   SETON                                        32
144700040708     C                   EVAL      VAOBLC = *zeros
144800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
144900040708     C                             + ' ' + 'VAOBLC'
145000040708     C                   ENDIF
145100040708     C                   ENDIF
145200040708     C*** ATT
145300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
145400040708     C                   EVAL      PiStr=SkSplitCSV(i)
145500040708     C                   EXSR      CHKNUM
145600040708     C                   IF        PiInt=*on
145700040708     C                   Z-ADD     PiVal         VAOATT
145800040708     C                   ELSE
145900040708     C                   SETON                                        32
146000040708     C                   EVAL      VAOATT = *zeros
146100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
146200040708     C                             + ' ' + 'VAOATT'
146300040708     C                   ENDIF
146400040708     C                   ENDIF
146500040708     C*** MTC
146600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
146700040708     C                   EVAL      PiStr=SkSplitCSV(i)
146800040708     C                   EXSR      CHKNUM
146900040708     C                   IF        PiInt=*on
147000040708     C                   Z-ADD     PiVal         VAOMTC
147100040708     C                   ELSE
147200040708     C                   SETON                                        32
147300040708     C                   EVAL      VAOMTC = *zeros
147400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
147500040708     C                             + ' ' + 'VAOMTC'
147600040708     C                   ENDIF
147700040708     C                   ENDIF
147800040708     C*** KSC
147900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
148000030721     C                   EVAL      PiStr=SkSplitCSV(i)
148100030721     C                   EXSR      CHKNUM
148200030721     C                   IF        PiInt=*on      AND
148300030721     C                             PiVal<=9999999 AND
148400030721     C                             PiVal>*zeros
148500040708     C                   Z-ADD     PiVal         VAOKSC
148600030721     C                   ELSE
148700030721     C                   SETON                                        32
148800040708     C                   EVAL      VAOKSC = *zeros
148900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
149000040708     C                             + ' ' + 'VAOKSC'
149100030721     C                   ENDIF
149200030721     C                   ENDIF
149300040708     C*** POR
149400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
149500030721     C                   EVAL      PiStr=SkSplitCSV(i)
149600030721     C                   EXSR      CHKNUM
149700030721     C                   IF        PiInt=*on    AND
149800030721     C                             PiVal<=999   AND
149900030721     C                             PiVal>*zeros
150000040708     C                   Z-ADD     PiVal         VAOPOR
150100030721     C                   ELSE
150200030721     C                   SETON                                        32
150300040708     C                   EVAL      VAOPOR = *zeros
150400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
150500040708     C                             + ' ' + 'VAOPOR'
150600030721     C                   ENDIF
150700030721     C                   ENDIF
150800040708     C*** POC
150900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
151000040708     C                   EVAL      PiStr=SkSplitCSV(i)
151100040708     C                   EXSR      CHKNUM
151200040708     C                   IF        PiInt=*on    AND
151300040708     C                             PiVal<=999   AND
151400040708     C                             PiVal>*zeros
151500040708     C                   Z-ADD     PiVal         VAOPOC
151600040708     C                   ELSE
151700040708     C                   SETON                                        32
151800040708     C                   EVAL      VAOPOC = *zeros
151900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
152000040708     C                             + ' ' + 'VAOPOC'
152100040708     C                   ENDIF
152200040708     C                   ENDIF
152300040708     C*** STO
152400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
152500040708     C                   EVAL      PiStr=SkSplitCSV(i)
152600040708     C                   EXSR      CHKNUM
152700040708     C                   IF        PiInt=*on
152800040708     C                   Z-ADD     PiVal         VAOSTO
152900040708     C                   ELSE
153000040708     C                   SETON                                        32
153100040708     C                   EVAL      VAOSTO = *zeros
153200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
153300040708     C                             + ' ' + 'VAOSTO'
153400040708     C                   ENDIF
153500040708     C                   ENDIF
153600090428     C*
153700091103     C********** GESTIONE CAMPI PERSONALIZZATI
153800151125
153900151125     C*** DAR
154000151125     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAODAR'
154100151125     C                   EVAL      PiStr=%subst(%trim(SkSplitCSV(i)):5:4) +
154200151125     C                             %subst(%trim(SkSplitCSV(i)):3:2) +
154300151125     C                             %subst(%trim(SkSplitCSV(i)):1:2)
154400151125     C                   EXSR      CHKNUM
154500151125     C                   IF        PiInt=*on
154600151125     C                   Z-ADD     PiVal         VAODAR
154700151125     C                   ELSE
154800151125     C                   SETON                                        32
154900151125     C                   EVAL      VAODAR = *zeros
155000151125     C                   EVAL      vinmsg = %trimr(vinmsg)
155100151125     C                             + ' ' + 'VAODAR'
155200151125     C                   ENDIF
155300151125     C                   ENDIF
155400150923
155500090428     C*
155600090428     C****************************************
155700090428     C*
155800030715     C*
155900030715     C                   ENDIF
156000030715     C                   EVAL      i = i + 1
156100030715     C                   ENDDO
156200040723     C*
156300121207     C***  ===>  Gestione campi "particolari" fuori ciclo
156400121207     C*
156500140108
156600140108     C*
156700140108     C* se VAORER non è valorizzato copiare il valore inserito nel campo VAORSR
156800140108     C                   IF        VAORER = *blank
156900140108     C                   EVAL      VAORER = VAORSR
157000140108     C                   ENDIF
157100140108     C*
157200130513
157300030715     C                   ENDIF
157400020204     C*
157500010607     C                   ENDSR
157600010607     C*----------------------------------------------------*
157700080702
157800080702
157900080702     C*----------------------------------------------------*
158000080702     C*  CONTROLLO NUMERICITA' CAMPI
158100080702     C*----------------------------------------------------*
158200080702     C     CHKNUM        BEGSR
158300080702     C*
158400080702     C                   IF        PiDecChr = *blanks
158500080702     C                   EVAL      PiDecChr = ','
158600080702     C                   ENDIF
158700080702     C*
158800080702     C                   callp     UBISNUM_Check(PiStr
158900080702     C                                          :PiDecChr
159000080702     C                                          :PiVal
159100080702     C                                          :PiNum
159200080702     C                                          :PiInt)
159300080702     C*
159400080702     C                   ENDSR
159500080702     C***
159600080702
159700010601
159800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
159900010601     C     repfil        BEGSR
160000010601     C*
160100010601     C                   if        invfil = *zeros and
160200010601     C                             depfil > *zeros and
160300010629     C                             (vinflg = *blanks or
160400010629     C                              vinflg = *zeros)
160500010601     C
160600010601     C                   eval      invfil = depfil
160700010601     C                   endif
160800010601     C*
160900010601     C                   if        depfil <> invfil and
161000010601     C                             invfil > *zeros
161100010601     C                   eval      flgMulti = '1'
161200010601     C                   if        vinflg = *blanks
161300010601     C                   add       1             cntNonEl
161400010601     C                   endif
161500010601     C                   endif
161600010601     C*
161700010601     C                   if        vinflg = '2'
161800010601     C                   eval      flgStato = '2'
161900010601     C                   endif
162000010601     C*
162100010601     C                   ENDSR
162200010601     C***
162300010330
162400010330
162500990920      /TITLE Invio dei dati al punto operativo.
162600000613     C     invio         BEGSR
162700990920     C*
162800990920     C                   reset                   dscmz
162900010601     C                   move      invfil        cmzdst
163000990920     C                   eval      cmzfld = vlrfou
163100990920     C                   eval      cmzmbd = vlrhdl
163200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
163300000710     C                   if        prmfir = *blanks
163400040708     C                   eval      cmzfla = 'FNVAO00F'
163500040708     C                   eval      cmzmba = 'FNVAO00F'
163600000710     C                   else
163700000710     C                   eval      cmzfla = prmfir
163800000710     C                   eval      cmzmba = prmfir
163900000710     C                   endif
164000990920     C                   eval      cmznrr = *zeros
164100990920     C                   move      §ctrok        cmznrr
164200021018     C                   eval      cmzlba = vlrfl1
164300990920     C                   call(e)   'TIS711C'
164400990920     C                   parm                    dscmz
164500990921     C                   parm      *blanks       esito
164600990920     C                   if        %error
164700990920     C                             or cmzerr = '1'
164800990921     C                             or esito  = '1'
164900000710     C                   eval      wrkesito = '3'
165000990920     C                   endif
165100990920     C*
165200000613     C                   ENDSR
165300010601
165400010601
165500010601      /TITLE Invio dei dati al punto operativo.
165600010601     C     opeini        BEGSR
165700010601     C*
165800010601     C* Inizializzo flag e contatori operativi
165900010601     C                   movel     '0'           flgGiro           1
166000010601     C                   movel     '0'           flgMulti          1
166100010601     C                   movel     '1'           flgStato          1
166200010615     C                   movel     '0'           flgOk             1
166300010601     C                   z-add     *zeros        cntNonEl         10 0
166400010601     C                   z-add     *zeros        depfil            3 0
166500010601     C                   z-add     *zeros        invfil            3 0
166600010601     C*
166700010601     C                   ENDSR
166800010601     C***
166900050421
167000050421
167100091130     C     *pssr         BEGSR
167200050421     C*
167300091130     C                   eval      wrkesito = '2'
167400050421     C*
167500091130     C                   ENDSR     '*CANCL'
167600050421     C***
167700060306
167800060306
167900060306     C*-------------------------------------------------------------------------*
168000060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
168100060306     C*-------------------------------------------------------------------------*
168200060306     C     sr_Control    begsr
168300060306     C*
168400060306     C* inizializzo a ok il flag d controllo abilitazione
168500060306     C                   eval      Cli_Abil = *on
168600060306     C
168700060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
168800060306     C                   clear                   woriIDC
168900060306     C                   eval      kteCOD = 'VAP'
169000060306     C                   eval      kteKE1 = oriIDC
169100060306     C     keytbe        chain     TNTBE01L
169200060306     C                   if        %found(Tntbe01l)
169300060306     C                   eval      woriIDC = TbeKe2
169400060306     C                   else
169500060306     C                   eval      Cli_Abil = *off
169600060306     C                   endif
169700060306     C*
169800060306     C                   endsr
169900010330
170000010330
170100000613     C     *inzsr        BEGSR
170200990910     C*
170300990910     C     *entry        plist
170400990920     C                   parm                    tivlrds
170500990921     C                   parm      wrkesito      esito
170600000724     C                   parm                    prmlit
170700000710     C                   parm                    prmfir
170800010330     C*
170900010330     C* CALCOLA LA DATA CORRENTE
171000091127     C                   z-add     *zeros        datcor            8 0
171100091127     C                   time                    wn14             14 0
171200091127     C                   movel     wn14          wn6               6 0          *ORA (6)
171300130516     C                   eval      datcor = %dec(%date() : *ISO)
171400040723     C*
171500040723     C* Campi riferito al database
171600040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
171700040723     C     *like         define    tbeke1        kteKE1
171800040723     C*
171900040723     C* Definizione chiavi di lettura
172000040723     C* TNTBE01L - parziale
172100040723     C     keytbe        klist
172200040723     C                   kfld                    kteCOD                         *UTENTE
172300040723     C                   kfld                    kteKE1                         *CODICE
172400000613     C*
172500000613     C                   ENDSR
172600000613     C***
172700990908
172800040709** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
172900151124X;VAORSR;VAOINR;VAOCAR;VAOLOR;VAOPRR;VAOTER;VAORER;VAOEMLR;X;VAORSC;VAOINC;VAOCAC;VAOLOC;VAOPRC;
173000151125X;W_VAODAR;VAOORDA1;VAOORAA1;VAOORDA2;VAOORAA2;X;X;X;VAONCL;VAOPKG;X;X;VAORFA;X;X;X;X;X;X;X;
173100130222
