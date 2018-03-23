000100040708      /TITLE Upload via Internet: traduzione in FNVAOWWR.
000200121129      *
000300121129      * PARTICOLARITA':
000400121129      *
000500151111      * Questo pgm contiene una normalizzazione della riga di intestazione std da divisione della
000600151111      * colonna con ; al carattere nei parametri.
000700151111      * L'analisi prevede che il cliente mandi un file senza riga di intestazione e un carattere
000800151111      * separatore di colonna che non è il ;
000900151111      * Senza questa normalizzazione i traduttori che gestiscono un CSV senza riga di intestazione e
001000151111      * con carattere separatore di colonna diverso da ; si spaccano.
001100151111      * Questo (TITZH4R) è il primo traduttore VAO con queste specifiche.
001200151111      *
001300151111      * Questo (TITZH4R) è il primo traduttore VAO con CSV che sa da analisi che la prima colonna è
001400151111      * vuota, per cui deve mettere un '*' per evitare che siano elise delle colonne
001500160316      *
001600160316      * Questo pgm ha la schiera a tempo di compilazione INTCOL di 4 righe e non 3 come a standard
001700151111      *
001800121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
001900120209     F*
002000040723     Ffnacr01l  if   e           k disk
002100090618     Ftntbe01l  if   e           k disk
002200990910     Ftivin00r  uF   E             DISK    usropn
002300040708     FFnvaowwr  O    E             DISK    usropn
002400040723     Ftiori00f  O    E             DISK
002500050826     FTIVGD00F  O    E             DISK
002600000313     D*
002700040709     D*------------
002800040709     D* SCHIERE A PROGRAMMA
002900040709     D*------------
003000160316     D intcol          S            100    DIM(4) CTDATA PERRCD(1)
003100010330     D*----------------------------------------------------
003200010330     D* DICHIARAZIOINE VARIABILI DI WRK
003300010330     D*----------------------------------------------------
003400010330     D dscmz         e ds                  inz
003500010330     D psds           sds
003600010330     D  procname         *PROC
003700010330     D tivlrds       e ds                  extname(tivlr00f)
003800050826     D fnvapds       e ds                  extname(fnvap00f)
003900040723     D dorm01        e ds
004000040723     D dfar          e ds
004100090618     d trul33ds      e ds
004200090618     d kpjba         e ds
004300150923     D fnvaoear      e ds                  inz
004400150923     D                                     prefix(w_)
004500010330     D esito           s              1
004600010330     D prmlit          s             10
004700010330     D prmfir          s             10
004800010330     D wrkesito        s                   like(esito)
004900010330     D rrnum           s              6  0 INZ(*zeros)
005000040709     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
005100060306     D Cli_Abil        s              1    inz(*off)
005200060306     D woriIDC         s                   like(oriIDC) inz(*blanks)
005300090618     d §numori         s              7  0 inz
005400120123     d wData_EUR       s               d   datfmt(*eur)
005500120123     d wData_ISO       s               d   datfmt(*iso)
005600120905     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
005700120905     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
005800151111     D Intestazioni    s               n   inz(*off)
005900030715     D*------------------
006000030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
006100030715     D*------------------
006200030715     D SkSplitFLD      S             10    DIM(100)
006300030715     D SkSplitCSV      S            256    DIM(100)
006400030715     D CharCSV         S              1
006500030715     D CharTXT         S              1
006600030715     D CharNUM         S              1
006700030715     D posDa           S              3  0 INZ(*zeros)
006800030715     D posA            S              3  0 INZ(*zeros)
006900030715     D i               s              3  0 INZ(1)
007000030715     D wGiro           s              1  0 INZ(*zeros)
007100030822     D*------------------
007200030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
007300030822     D*------------------
007400030822     D posDaDft        S              3  0 INZ(*zeros)
007500030822     D posADft         S              3  0 INZ(*zeros)
007600030822     D j               s              3  0 INZ(1)
007700030822     D wGiroDft        s              1  0 INZ(*zeros)
007800040723     D*------------------
007900040723     D* DS REPERIMENTO DATI CLIENTE
008000040723     D*-------------------
008100040723     D BS69DS        E DS                  EXTNAME(TIBS69DS)
008200040723     D ACODS         E DS                  EXTNAME(CNACO00F)
008300040723     D INDDS         E DS                  EXTNAME(CNIND00F)
008400040723     D CLPDS         E DS                  EXTNAME(CNCLP00F)
008500040723     D CLSDS         E DS                  EXTNAME(FNCLS00F)
008600121129     D*------------------
008700150923     D w70             s             70
008800080702
008900080702     D*------------------
009000080702     D* LINKING A DEFINIZIONI ESTERNE
009100080702     D*------------------
009200090428     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009300090428     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009400091026
009500091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
009600091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
009700091026
009800080702
009900031201     D*------------------
010000031201     D* Costanti
010100031201     D*------------------
010200031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
010300031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
010400010330
010500010330
010600010330
010700000913     C                   reset                   rrnum
010800990921     C                   reset                   esito
010900990921     C                   reset                   wrkesito
011000010601     C*
011100010601     C                   exsr      opeini
011200041011     C                   exsr      PREELA
011300040708     C                   exsr      rwvao
011400010601     C*
011500010601     C                   seton                                        lr
011600010601
011700010601
011800010601
011900010601
012000010601     C*--------------------------------------------------------
012100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
012200010601     C*--------------------------------------------------------
012300010601     C     PREELA        BEGSR
012400010601     C*
012500010601     C* SE OCCORRE SPEDIRE IN FILIALE
012600041011     C                   if        flgGiro = '0'
012700010601     C*
012800010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012900010601     C                   eval      flgGiro = '1'
013000041011     C*
013100041011     C                   endif
013200010601     C*
013300010601     C                   ENDSR
013400010601     C***
013500010601
013600010601
013700010601     C*--------------------------------------------------------
013800010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013900010601     C*--------------------------------------------------------
014000010601     C     ENDELA        BEGSR
014100000616     C*
014200120209     C                   ENDSR
014300010601
014400010601
014500010330     C*--------------------------------------------------------
014600040708     C* RWVAO   LEGGE TIVIN00R E SCRIVE FNVAOWWF              *
014700010330     C*--------------------------------------------------------
014800040708     C     RWVAO         BEGSR
014900010330     C*
015000010330     C                   if        not %open(tivin00r)
015100010330     C                   open      tivin00r
015200010330     C                   endif
015300040708     C                   if        not %open(fnvaowwr)
015400040708     C                   open      fnvaowwr
015500010330     C                   endif
015600150923     C*
015700150923     C* Mi predispongo per scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
015800150923     C                   call      'TIS7T7ER'
015900150923     C                   parm      'O'           pIn_Opz           1
016000150923     C                   parm                    tivlrds
016100150923     C                   parm                    FNVAOEAR
016200150923     C                   parm                    oriPRG
016300150923     C                   parm      *zeros        pIn_Dst           3
016400150923     C                   parm      *blanks       pOut_Esito        1
016500150923     C                   if        pOut_Esito = '2'
016600150923     C                   goto      FINE
016700150923     C                   endif
016800010330     C*
016900010604     C                   clear                   §CTROK
017000010604     C                   clear                   §CTRMO
017100010604     C                   clear                   §CTRNO
017200120123     C*
017300120905     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
017400120905     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
017500120905     C                   IF        Intestazioni = *off
017600120123     C* Eseguo primo giro x by-passare la mancanza della 1° riga con le intestazioni
017700120123     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
017800120123     C                   exsr      inzvar
017900120123     C                   exsr      impvao
018000120905     C                   ENDIF
018100010330     C*
018200030822     C                   do        *HIVAL
018300010330     C*
018400030822     C                   read      tivin00r                               70
018500010618     C*
018600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018700010618     C                   if        vinflg = '1'
018800010618     C                   eval      flgOk = '1'
018900010618     C                   endif
019000010618     C*
019100040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
019200040510     C                   z-add     *zeros        wLenVINDTA        4 0
019300040510     C                   z-add     *zeros        wFlgVINDTA        1 0
019400040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
019500040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
019600040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
019700040510     C                   z-add     1             wFlgVINDTA
019800040510     C                   leave
019900040510     C                   endif
020000040510     C                   enddo
020100040510     C*
020200040218     C                   if        vindta > *blanks AND
020300040510     C                             wFlgVINDTA = 1
020400010330     C                   add       1             rrnum
020500010330     C*
020600010601     C                   if        *in70 = *off and
020700010330     C                             (vinflg = *blanks
020800010330     C                              or vinflg = '0'
020900010330     C                              or vinflg = '2')
021000010330     C*
021100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021200010711     C                   if        vinflg = *blanks or vinflg = '0'
021300010711     C                   clear                   vinmsg
021400010711     C                   endif
021500151111     C*
021600151111     C* Aggiungo un byte a blanks all'inizio d ogni record
021700151111     C* siccome sappiamo che la prima colonna è vuota, mettiamo un carattere non vuoto per
021800151111     C* evitare che vengano elisi i blank
021900151111     C                   eval      vindta  = '*' + %trim(vindta)
022000010601     C*
022100010330     C                   exsr      inzvar
022200040708     C                   exsr      impvao
022300151019     C*
022400151019     C* Stacco subito un numeratore univoco "ORM"
022500151019     C                   exsr      RTVNUM
022600150923     C*
022700150923     C* Verifico se trattasi di formato ORM "esteso"
022800150923     C                   setoff                                       53
022900150923     C                   if        w_VAOEMLR  <> *blanks
023000150923     C                   seton                                        53
023100150923     C                   endif
023200010601     C*
023300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
023400040708     C                   eval      depfil = VAOPOE
023500010601     C                   exsr      repfil
023600010601     C                   if        depfil = invfil
023700021025     C                   if        vlrpoi = 999
023800040708     C                   move(P)   invfil        VAOPOE
023900021025     C                   else
024000040708     C                   move(P)   vlrpoi        VAOPOE
024100021025     C                   endif
024200010601     C*
024300010604     C* Ebbene...
024400010604     C*
024500030822     C  N31              add       1             §CTROK            7 0
024600030822     C   32              add       1             §CTRMO            7 0
024700030822     C   31              add       1             §CTRNO            7 0
024800030822     C                   if        wGiro = 2
024900120123     C  N31              EXSR      CHKCOR
025000120123     C  N31              EXSR      WRIORI
025100120123     C  N31              EXSR      WRIVGD
025200120123     C  N31              write     FNVAO000
025300150923     C*
025400150923     C* Se richiesta scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
025500150923     C                   if        *in53 and not *in31
025600151019     C                   exsr      EXEFORZ
025700150923     C                   call      'TIS7T7ER'
025800150923     C                   parm      'W'           pIn_Opz           1
025900150923     C                   parm                    tivlrds
026000150923     C                   parm                    FNVAOEAR
026100150923     C                   parm                    oriPRG
026200150923     C                   parm      *zeros        pIn_Dst           3
026300150923     C                   parm      *blanks       pOut_Esito        1
026400150923     C                   endif
026500150923     C                   if        pOut_Esito = '2'
026600150923     C                   seton                                        32
026700150923     C                   endif
026800030822     C                   endif
026900010604     C*
027000010604     C                   if        *in31 = *off and
027100010604     C                             *in32 = *off
027200010604     C                   eval      vinflg = '1'
027300010604     C                   else
027400010604     C                   eval      vinflg = '2'
027500010604     C                   endif
027600010604     C                   endif
027700010601     C*
027800010604     C                   endif
027900010604     C*
028000010330     C                   else
028100010330     C                   eval      vinflg = '1'
028200010330     C                   endif
028300010601     C*
028400010601     C  N70              update    tivin000
028500010330     C*
028600030822     C  N70              enddo
028700150923     C*
028800150923     C     FINE          tag
028900150923     C*
029000150923     C* Chiusura per scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
029100150923     C                   call      'TIS7T7ER'
029200150923     C                   parm      'C'           pIn_Opz           1
029300150923     C                   parm                    tivlrds
029400150923     C                   parm                    FNVAOEAR
029500150923     C                   parm                    oriPRG
029600150923     C                   parm      *zeros        pIn_Dst           3
029700150923     C                   parm      *blanks       pOut_Esito        1
029800010601     C*
029900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
030000010601     C                   if        cntNonEl = *zeros or
030100010601     C                             flgMulti = '0'
030200010330     C* Se non ci sono record con errori ...
030300010601     C                   if        §ctrno = 0 and
030400010604     C                             §ctrmo = 0 and
030500010601     C                             flgStato <> '2'
030600010330     C* ... restituisco esito OK.
030700010330     C                   eval      wrkesito = '0'
030800010330     C                   else
030900010330     C                   if        §ctrok > 0
031000010330     C                   eval      wrkesito = '1'
031100010330     C                   else
031200010615     C                   if        flgOk = '0'
031300010615     C                   eval      wrkesito = '2'
031400010615     C                   else
031500010615     C                   eval      wrkesito = '6'
031600010615     C                   endif
031700010330     C                   endif
031800010330     C                   endif
031900010601     C                   else
032000010601     C                   eval      wrkesito = '9'
032100010601     C                   endif
032200010330     C*
032300010330     C                   if        %open(tivin00r)
032400010330     C                   close     tivin00r
032500010330     C                   endif
032600040708     C                   if        %open(fnvaowwr)
032700040708     C                   close     fnvaowwr
032800010330     C                   endif
032900010601     C*
033000010601     C                   if        vlrpoi <> 999
033100010601     C                   eval      invfil = vlrpoi
033200010601     C                   endif
033300010330     C*
033400010330     C                   if        §ctrok > 0
033500010601     C                             and invfil > *zeros
033600010330     C                   exsr      invio
033700150923     C*
033800150923     C* Invio dati estensione ORM (FNVAOE0F) => richiamo l'apposito driver
033900150923     C                   move      invfil        cmzdst
034000150923     C                   call      'TIS7T7ER'
034100150923     C                   parm      'S'           pIn_Opz           1
034200150923     C                   parm                    tivlrds
034300150923     C                   parm                    FNVAOEAR
034400150923     C                   parm                    oriPRG
034500150923     C                   parm      cmzdst        pIn_Dst           3
034600150923     C                   parm      *blanks       pOut_Esito        1
034700010330     C                   endif
034800010601     C*
034900010618     C                   if        flgGiro = '1'
035000010601     C                   exsr      endela
035100010618     C                   endif
035200010330     C*
035300010330     C                   ENDSR
035400010330     C***
035500040723
035600151019
035700151019
035800151019     C*------------------------------------------------------------------------*
035900151019     C* RTVNUM - Stacco un numeratore univoco "ORM"
036000151019     C*------------------------------------------------------------------------*
036100151019     C     RTVNUM        BEGSR
036200151019     C*
036300151019     C* Stacco un numeratore da AZNUM
036400151019     C                   clear                   §numori
036500151019     C                   clear                   trul33ds
036600151019     C                   eval      I33OPE = *zeros
036700151019     C                   eval      I33CNU = 600
036800151019     C                   eval      I33NUM = 1
036900151019     C                   movel     TRUL33DS      KPJBU
037000151019     C                   call      'TRUL33R'
037100151019     C                   parm                    KPJBA
037200151019     C                   movel     KPJBU         TRUL33DS
037300151019     C                   if        O33ERR = *zeros
037400151019     C                   z-add     O33NRF        §numori
037500151019     c                   else
037600151019     c                   eval      §numori = *all'9'
037700151019     C                   seton                                        31        * errore bloccante
037800151019     C                   endif
037900151019     C*
038000151019     C                   eval      vaoNOR = §numori
038100151019     C*
038200151019     C                   ENDSR
038300151019
038400151019
038500151019     C*------------------------------------------------------------------------*
038600151019     C* EXEFORZ - Eseguo forzatire di base su formato ORM "esteso"
038700151019     C*------------------------------------------------------------------------*
038800151019     C     EXEFORZ       BEGSR
038900151019     C*
039000151019     C                   move(p)   VAOPOE        w_vaopoe                       * forzatura necessar
039100151019     C                   move(p)   VAOCOR        w_vaocor                       * forzatura necessar
039200151019     C                   move(p)   VAONOR        w_vaonor                       * forzatura necessar
039300151019     C*
039400151019     C                   ENDSR
039500040723
039600151019
039700040723     C*------------------------------------------------------------------------*
039800040723     C* CHKCOR - Considerazioni su VAOCOR
039900040723     C*------------------------------------------------------------------------*
040000040723     C     CHKCOR        BEGSR
040100040723     C*
040200040723     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
040300040723     C                   if        vaoTCO = 'F'
040400040723     C* Quindi verifico se il codice ordinante non è valorizzato
040500040723     C                   if        vaoCOR = *zeros  AND
040600040723     C                             vaoRSO = *blanks AND
040700040723     C                             vaoINO = *blanks AND
040800040723     C                             vaoCAO = *blanks AND
040900040723     C                             vaoLOO = *blanks AND
041000040723     C                             vaoPRO = *blanks AND
041100040723     C                             vaoNAO = *blanks
041200040723     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
041300040723     C                   movel     '0'           wFlgAcr           1
041400040723     C                   move      vlrKSC        wCli              7 0
041500040723     C                   move(p)   wCli          acrCRO
041600040723     C     acrCRO        setll     fnacr01l
041700040723     C                   if        %found(fnacr01l)
041800040723     C                   read      fnacr01l
041900040723     C                   dow       not %eof(fnacr01l)
042000040723     C                   movel     acrCRO        wCliAcr           7 0
042100040723     C                   if        wCliAcr = wCli
042200040723     C                   eval      vaoCOR = acrCRO
042300040723     C                   movel     '1'           wFlgAcr
042400040723     C                   leave
042500040723     C                   else
042600040723     C                   leave
042700040723     C                   endif
042800040723     C                   read      fnacr01l
042900040723     C                   enddo
043000040723     C                   endif
043100040723     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
043200040723     C* dati anagrafici dell'ordinante dal piano dei conti
043300040723     C                   if        wFlgAcr = '0'
043400040723     C                   clear                   BS69DS
043500040723     C                   clear                   ACODS
043600040723     C                   clear                   INDDS
043700040723     C                   clear                   CLPDS
043800040723     C                   clear                   CLSDS
043900040723     C                   move(P)   vlrKSC        I69KAC
044000040723     C                   move(P)   vlrKSC        I69KIN
044100040723     C                   call      'TIBS69R'
044200040723     C                   parm                    BS69DS
044300040723     C                   parm                    ACODS
044400040723     C                   parm                    INDDS
044500040723     C                   parm                    CLPDS
044600040723     C                   parm                    CLSDS
044700040723     C     O69ERR        ifne      '1'
044800040723     C                   eval      vaoRSO = ACORAG
044900040723     C                   eval      vaoINO = INDVIA
045000040723     C                   movel(P)  INDCAP        vaoCAO
045100040723     C                   eval      vaoLOO = INDCIT
045200040723     C                   eval      vaoPRO = INDPRV
045300040723     C                   eval      vaoNAO = INDSTA
045400040723     C                   endif
045500040723     C                   endif
045600040723     C                   endif
045700040723     C                   endif
045800040723     C*
045900040723     C                   ENDSR
046000040723
046100040723
046200040723     C*------------------------------------------------------------------------*
046300040723     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
046400040723     C*------------------------------------------------------------------------*
046500040723     C     WRIORI        BEGSR
046600040723     C*
046700090618     C* Come prima cosa stacco un numeratore da AZNUM
046800151019     C***                clear                   trul33ds
046900151019     C***                eval      I33OPE = *zeros
047000151019     C***                eval      I33CNU = 600
047100151019     C***                eval      I33NUM = 1
047200151019     C***                movel     TRUL33DS      KPJBU
047300151019     C***                call      'TRUL33R'
047400151019     C***                parm                    KPJBA
047500151019     C***                movel     KPJBU         TRUL33DS
047600151019     C***                if        O33ERR = *zeros
047700151019     C***                z-add     O33NRF        §numori
047800151019     c***                else
047900151019     c***                eval      §numori = *all'9'
048000151019     C***                endif
048100040723     C*
048200040723     C* Quindi imposto i campi "extra-VAO"
048300040723     C                   if        vlrKSC = '0ORM0000'
048400040723     C*
048500040723     C                   if        vaoCOR > *zeros
048600040723     C                   movel     vaoCOR        wrkCOR7           7 0
048700040723     C*
048800040723     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
048900040723     C* codice padre. Se è un padre assegno già il codice ordinante.
049000040723     C                   clear                   tibs10ds
049100040723     C                   eval      d10drf = datcor                              *data riferimento
049200040723     C                   eval      d10paf = 'P'                                 *cerca il padre
049300040723     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
049400091026     C*
049500091026     C* Preparazione chiamata al TIBS10R1
049600091026     C                   clear                   tibs10r1ds
049700091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
049800091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
049900091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
050000091026     C*
050100091026     C                   call      'TIBS10R1'
050200091026     C                   parm                    tibs10r1ds
050300091026     C*
050400091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
050500040723     C*
050600040723     C                   if        d10err <> *blanks or                         *No unif. o padre
050700040723     C                             d10cop =  *zeros  or
050800040723     C                             d10cop =  d10cod
050900040723     C                   move      d10cod        oriIDC
051000040723     C                   else                                                   *Figlio
051100040723     C                   move      d10cop        oriIDC
051200040723     C                   endif
051300040723     C*
051400040723     C                   else
051500040723     C                   eval      oriIDC = *zeros
051600040723     C                   endif
051700040723     C*
051800040723     C                   else
051900040723     C                   eval      oriIDC = vlrKSC
052000040723     C                   endif
052100040723     C*
052200040723     C                   eval      oriPRG = §numori
052300040723     C                   eval      oriDER = datcor
052400040723     C                   eval      oriOER = wn6
052500040723     C                   eval      oriPOE = vaoPOE
052600040723     C                   eval      oriNSR = vaoNSR
052700040723     C                   eval      oriNOR = vaoNOR
052800040723     C                   eval      oriNRV = vaoNRV
052900040723     C                   eval      oriFLO = *blanks
053000040723     C*
053100040723     C* Infine valorizzo la chiave esterna sul file FNVAO
053200150923     C* per ora VAOFLO è blank
053300150923     C                   eval      vaoflo = *blank
053400040723     C                   eval      dorm01 = vaoFLO
053500151210     C                   movel     §numori       §ormpg
053600150923     C*                  EVAL      §ORMCB = 'N'
053700150923     C*                  EVAL      §ORMFR = 'S'
053800150923     C*                  EVAL      §ORMKS = 'N'
053900150923     C*                  EVAL      §ORMFD = 'S'
054000050412     C*
054100050412     C* Infine valorizzo i flag operativi necessari x la particolare gestione corrente
054200130326     C*                   in questo caso non ci sono default
054300050412     C*
054400130326     C* Riassegno il campo VAO contenete i flag operativi precedentemente ridefiniti
054500040723     C                   eval      vaoFLO = dorm01
054600130326     C* Infine, per sicurezza, pulisco alcuni flag che devono essere a blank
054700130326     C                   eval      %subst(VAOFLO:5:1) = *blanks
054800130326     C                   eval      %subst(VAOFLO:6:1) = *blanks
054900040723     C*
055000040723     C* ...quindi scrivo il file TIORI00F
055100120905     C  N55              WRITE     TIORI000
055200040723     C*
055300040723     C                   ENDSR
055400050826
055500050826
055600050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
055700050826     C     wriVGD        BEGSR
055800050826     C*
055900050826     C* Reperisco la descrizione della fase da tabella
056000050826     C                   movel(p)  'FAR'         KteCOD
056100050826     C                   movel(p)  '000'         KteKE1
056200050826     C     KEYtbe        chain     tntbe01l
056300050826     C                   if        %found(tntbe01l)
056400050826     C                   movel(P)  tbeuni        DFAR
056500050826     C                   else
056600050826     C                   clear                   DFAR
056700050826     C                   endif
056800050826     C*
056900050826     C* Valorizzo buffer record
057000050826     C                   clear                   FNVAPDS
057100050826     C                   eval      vapIDC = oriIDC
057200050826     C                   eval      vapPOE = oriPOE
057300050826     C                   eval      vapRFA = vaoRFA
057400050826     C                   eval      vapPOG = oriPOE
057500050826     C                   eval      vapDAE = oriDER
057600050826     C                   eval      vapORE = oriOER
057700050826     C                   if        d§fardva <> *blanks
057800050826     C                   eval      vapDFA = d§fardva
057900050826     C                   else
058000050826     C                   eval      vapDFA = d§fardes
058100050826     C                   endif
058200050826     C*
058300050826     C                   clear                   tivgd000
058400080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
058500050826     C                   eval      vgdTIP = 'VP'
058600050826     C                   eval      vgdKSU = vapIDC
058700050826     C                   eval      vgdTSC = 'WW'
058800050826     C                   eval      vgdDAT = datcor
058900151207     C                   eval      vgdPGM = procname
059000060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
059100060306     C                   exsr      sr_Control
059200060306     C                   if        Cli_Abil = *on
059300060306     C                   eval      vgdKSU = woriIDC
059400120905     C  N55              write     tivgd000
059500060306     C                   endif
059600050826     C*
059700050826     C                   ENDSR
059800050826     C*------------------------------------------------------------------------*
059900010601
060000010601
060100010330     C*----------------------------------------------------*
060200030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
060300010330     C*----------------------------------------------------*
060400010330     C     INZVAR        BEGSR
060500010330     C*
060600030822     C* Inizializzo variabili di wrk
060700020204     C                   Z-ADD     *zeros        Num5_0            5 0
060800120124
060900030822     C*
061000030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
061100120123     C                   CLEAR                   FNVAO000
061200150923     C                   CLEAR                   FNVAOEAR
061300120123     C                   CLEAR                   TIORI000
061400030822     C                   CLEAR                   SkSplitCSV
061500030822     C                   EVAL      i = 1
061600030822     C                   EVAL      posDa = *zeros
061700030822     C                   EVAL      posA  = *zeros
061800030822     C*
061900030822     C* Reimposto i valori di default
062000030822     C                   EXSR      DEFCAM
062100030822     C*
062200010330     C                   ENDSR
062300010330     C*----------------------------------------------------*
062400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
062500010330     C*----------------------------------------------------*
062600010330     C     DEFCAM        BEGSR
062700030822     C*
062800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
062900030715     C* e delimitatore testo.
063000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
063100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
063200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
063300040723     C*
063400080707     C* Imposto i valori d default d alcuni campi
063500080708     C*
063600080708     C                   CLEAR                   DORM01
063700120822     C                   EVAL      VAOFLO = DORM01
063800080707     C*
063900160209     C                   EVAL      VAOCOR = 0016164000
064000160209     C                   EVAL      VAOKSC = 0016164
064100160209     C                   EVAL      VAOPOE = 001
064200160209     C                   EVAL      VAOTCO = 'F'
064300160209     C                   EVAL      VAORER = 'REFERENTE'
064400130513
064500120905     C* se il 1° char dei parametri è T=Test
064600120905     C                   SETOFF                                       55
064700120905     C                   IF        %subst(vlrppt:1:1) = 'T'
064800120905     C* inibisco la scrittura dei file TIORI e TIVGD
064900120905     C                   SETON                                        55
065000120905     C* imposto il PO di emissione come filiale fittizia
065100120905     C                   EVAL      VAOPOE = 999
065200120905     C                   ENDIF
065300091127     C*
065400091204     C* IMPOSTO I CAMPI PERSONALIZZATI
065500100416     C
065600030822     C*
065700030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
065800030822     C                   EVAL      posDaDft = 1
065900030822     C                   EVAL      posADft  = 0
066000030822     C                   EVAL      wGiroDft = 0
066100030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
066200030822     C                             posDaDft > 0
066300030822     C*
066400030822     C* Gestisco il 1° giro
066500030822     C                   IF        wGiroDft = 0
066600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
066700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
066800030822     C* Incremento il contatore dei "giri"
066900030822     C                   EVAL      wGiroDft = 1
067000030822     C                   ELSE
067100030822     C                   EVAL      posDaDft = posADft
067200030822     C                   ENDIF
067300030822     C* Eseguo lo scan x trovare la fine del campo corrente
067400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
067500030822     C*
067600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
067700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
067800030822     C                   IF        posDaDft > 0 AND
067900030822     C                             posADft  > 0
068000040708     C* POE
068100040708     C                   IF        %subst(
068200040708     C                             %subst(vlrppt:posDaDft+1:
068300040708     C                             posADft-posDaDft-1):1:3)
068400040708     C                             = 'POE'
068500040708     C                   EVAL      PiStr=%trim(%subst(
068600040708     C                             %subst(vlrppt:posDaDft+1:
068700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068800040708     C                   EXSR      CHKNUM
068900040708     C                   IF        PiInt=*on
069000040708     C                   Z-ADD     PiVal         VAOPOE
069100040708     C                   ENDIF
069200040708     C                   ENDIF
069300040708     C* TOR
069400040708     C                   IF        %subst(
069500040708     C                             %subst(vlrppt:posDaDft+1:
069600040708     C                             posADft-posDaDft-1):1:3)
069700040708     C                             = 'TOR'
069800040708     C                   EVAL      VAOTOR=%trim(%subst(
069900040708     C                             %subst(vlrppt:posDaDft+1:
070000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070100040708     C                   ENDIF
070200040708     C* TCO
070300040708     C                   IF        %subst(
070400040708     C                             %subst(vlrppt:posDaDft+1:
070500040708     C                             posADft-posDaDft-1):1:3)
070600040708     C                             = 'TCO'
070700040708     C                   EVAL      VAOTCO=%trim(%subst(
070800040708     C                             %subst(vlrppt:posDaDft+1:
070900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071000040708     C                   ENDIF
071100040707     C* COR
071200030822     C                   IF        %subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):1:3)
071500040707     C                             = 'COR'
071600030822     C                   EVAL      PiStr=%trim(%subst(
071700030822     C                             %subst(vlrppt:posDaDft+1:
071800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071900030822     C                   EXSR      CHKNUM
072000030822     C                   IF        PiInt=*on
072100040708     C                   Z-ADD     PiVal         VAOCOR
072200030822     C                   ENDIF
072300030822     C                   ENDIF
072400040708     C* CRC
072500040708     C                   IF        %subst(
072600040708     C                             %subst(vlrppt:posDaDft+1:
072700040708     C                             posADft-posDaDft-1):1:3)
072800040708     C                             = 'CRC'
072900040708     C                   EVAL      PiStr=%trim(%subst(
073000040708     C                             %subst(vlrppt:posDaDft+1:
073100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073200040708     C                   EXSR      CHKNUM
073300040708     C                   IF        PiInt=*on
073400040708     C                   Z-ADD     PiVal         VAOCRC
073500040708     C                   ENDIF
073600040708     C                   ENDIF
073700040708     C* FFD
073800040708     C                   IF        %subst(
073900040708     C                             %subst(vlrppt:posDaDft+1:
074000040708     C                             posADft-posDaDft-1):1:3)
074100040708     C                             = 'FFD'
074200040708     C                   EVAL      VAOFFD=%trim(%subst(
074300040708     C                             %subst(vlrppt:posDaDft+1:
074400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074500040708     C                   ENDIF
074600040708     C* ORR
074700040708     C                   IF        %subst(
074800040708     C                             %subst(vlrppt:posDaDft+1:
074900040708     C                             posADft-posDaDft-1):1:3)
075000040708     C                             = 'ORR'
075100040708     C                   EVAL      PiStr=%trim(%subst(
075200040708     C                             %subst(vlrppt:posDaDft+1:
075300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075400040708     C                   EXSR      CHKNUM
075500040708     C                   IF        PiInt=*on
075600040708     C                   Z-ADD     PiVal         VAOORR
075700040708     C                   ENDIF
075800040708     C                   ENDIF
075900040712     C* NCL
076000040712     C                   IF        %subst(
076100040712     C                             %subst(vlrppt:posDaDft+1:
076200040712     C                             posADft-posDaDft-1):1:3)
076300040712     C                             = 'NCL'
076400040712     C                   EVAL      PiStr=%trim(%subst(
076500040712     C                             %subst(vlrppt:posDaDft+1:
076600040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076700040712     C                   EXSR      CHKNUM
076800040712     C                   IF        PiInt=*on
076900040712     C                   Z-ADD     PiVal         VAONCL
077000040712     C                   ENDIF
077100040712     C                   ENDIF
077200040712     C* PKG
077300040712     C                   IF        %subst(
077400040712     C                             %subst(vlrppt:posDaDft+1:
077500040712     C                             posADft-posDaDft-1):1:3)
077600040712     C                             = 'PKG'
077700040712     C                   EVAL      PiStr=%trim(%subst(
077800040712     C                             %subst(vlrppt:posDaDft+1:
077900040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078000040712     C                   EXSR      CHKNUM
078100040712     C                   IF        PiNum=*on
078200040712     C                   Z-ADD     PiVal         VAOPKG
078300040712     C                   ENDIF
078400040712     C                   ENDIF
078500040712     C* VLM
078600040712     C                   IF        %subst(
078700040712     C                             %subst(vlrppt:posDaDft+1:
078800040712     C                             posADft-posDaDft-1):1:3)
078900040712     C                             = 'VLM'
079000040712     C                   EVAL      PiStr=%trim(%subst(
079100040712     C                             %subst(vlrppt:posDaDft+1:
079200040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079300040712     C                   EXSR      CHKNUM
079400040712     C                   IF        PiNum=*on
079500040712     C                   Z-ADD     PiVal         VAOVLM
079600040712     C                   ENDIF
079700040712     C                   ENDIF
079800040708     C* NAM
079900040708     C                   IF        %subst(
080000040708     C                             %subst(vlrppt:posDaDft+1:
080100040708     C                             posADft-posDaDft-1):1:3)
080200040708     C                             = 'NAM'
080300040708     C                   EVAL      VAONAM=%trim(%subst(
080400040708     C                             %subst(vlrppt:posDaDft+1:
080500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080600040708     C                   ENDIF
080700040708     C* PAG
080800030822     C                   IF        %subst(
080900030822     C                             %subst(vlrppt:posDaDft+1:
081000030822     C                             posADft-posDaDft-1):1:3)
081100040708     C                             = 'PAG'
081200040708     C                   EVAL      VAOPAG=%trim(%subst(
081300030822     C                             %subst(vlrppt:posDaDft+1:
081400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081500030822     C                   ENDIF
081600040708     C* KSC
081700040708     C                   IF        %subst(
081800040708     C                             %subst(vlrppt:posDaDft+1:
081900040708     C                             posADft-posDaDft-1):1:3)
082000040708     C                             = 'KSC'
082100040708     C                   EVAL      PiStr=%trim(%subst(
082200040708     C                             %subst(vlrppt:posDaDft+1:
082300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082400040708     C                   EXSR      CHKNUM
082500040708     C                   IF        PiInt=*on
082600040708     C                   Z-ADD     PiVal         VAOKSC
082700040708     C                   ENDIF
082800040708     C                   ENDIF
082900040708     C* CTR
083000040708     C                   IF        %subst(
083100040708     C                             %subst(vlrppt:posDaDft+1:
083200040708     C                             posADft-posDaDft-1):1:3)
083300040708     C                             = 'CTR'
083400040708     C                   EVAL      VAOCTR=%trim(%subst(
083500040708     C                             %subst(vlrppt:posDaDft+1:
083600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083700040708     C                   ENDIF
083800040708     C* POC
083900040708     C                   IF        %subst(
084000040708     C                             %subst(vlrppt:posDaDft+1:
084100040708     C                             posADft-posDaDft-1):1:3)
084200040708     C                             = 'POC'
084300040708     C                   EVAL      PiStr=%trim(%subst(
084400040708     C                             %subst(vlrppt:posDaDft+1:
084500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084600040708     C                   EXSR      CHKNUM
084700040708     C                   IF        PiInt=*on
084800040708     C                   Z-ADD     PiVal         VAOPOC
084900040708     C                   ENDIF
085000040708     C                   ENDIF
085100040708     C* NO1
085200040708     C                   IF        %subst(
085300040708     C                             %subst(vlrppt:posDaDft+1:
085400121129     C                             posADft-posDaDft-1):1:3)
085500040708     C                             = 'NO1'
085600040708     C                   EVAL      VAONO1=%trim(%subst(
085700040708     C                             %subst(vlrppt:posDaDft+1:
085800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085900040708     C                   ENDIF
086000040708     C* DDT
086100030822     C                   IF        %subst(
086200030822     C                             %subst(vlrppt:posDaDft+1:
086300030822     C                             posADft-posDaDft-1):1:3)
086400040708     C                             = 'DDT'
086500040708     C                   EVAL      VAODDT=%trim(%subst(
086600030822     C                             %subst(vlrppt:posDaDft+1:
086700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086800030822     C                   ENDIF
086900040708     C* RFA
087000040708     C                   IF        %subst(
087100040708     C                             %subst(vlrppt:posDaDft+1:
087200040708     C                             posADft-posDaDft-1):1:3)
087300040708     C                             = 'RFA'
087400040708     C                   EVAL      VAORFA=%trim(%subst(
087500040708     C                             %subst(vlrppt:posDaDft+1:
087600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
087700040708     C                   ENDIF
087800040708     C* FLO
087900040708     C                   IF        %subst(
088000040708     C                             %subst(vlrppt:posDaDft+1:
088100040708     C                             posADft-posDaDft-1):1:3)
088200040708     C                             = 'FLO'
088300040708     C                   EVAL      VAOFLO=%trim(%subst(
088400040708     C                             %subst(vlrppt:posDaDft+1:
088500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
088600091103     C*
088700040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
088800080702     C* e posizione 7 del VAOFLO
088900040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
089000040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
089100050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
089200040708     C                   ENDIF
089300030822     C* ...
089400030822     C                   ENDIF
089500030822     C                   ENDDO
089600020204     C*
089700010330     C                   ENDSR
089800010607     C*----------------------------------------------------*
089900040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
090000010607     C*----------------------------------------------------*
090100040708     C     IMPVAO        BEGSR
090200010607     C*
090300010607     C                   SETOFF                                       3132
090400120124
090500030515     C*
090600040708     C                   MOVEL     datcor        VAODAO
090700040708     C                   MOVEL     wn14          VAOOAO
090800050314     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
090900050314     C     minu:maiu     XLATE     vindta        vindta
091000010607     C*
091100030715     C**********
091200030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
091300030715     C**********
091400030715     C                   IF        wGiro = *zeros
091500040709     C*
091600040709     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
091700040709     C                   EVAL      depvinDTA = vinDTA
091800040709     C*
091900151111     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
092000151111     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
092100160316     C                             %trim(intcol(3))+%trim(intcol(4))<>*blanks
092200160316     C***                          %trim(intcol(3))<>*blanks
092300040709     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
092400040709     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
092500160316     C                                      %trim(intcol(3))+%trim(intcol(4))
092600151111     C     ';':CharCSV   XLATE     vindta        vindta
092700151111     C                   ENDIF
092800031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
092900050314     C**!!!minu:maiu     XLATE     vindta        vindta
093000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
093100040324     C                   DOW       posDa <= %len(%trimr(vindta))
093200030715     C*
093300030715     C* Gestisco il 1° campo
093400030715     C                   IF        i = 1
093500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
093600030715     C                   EVAL      posDa = 1
093700030715     C* Eseguo lo scan x trovare la fine del primo campo
093800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
093900030715     C* A questo punto "estraggo" il campo corrente
094000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
094100030715     C                                                    (posA-posDa))
094200030715     C* X i campi successivi al 1°
094300030715     C                   ELSE
094400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
094500030715     C                   EVAL      posDa = posA + 1
094600030715     C* Eseguo lo scan x trovare la fine del campo corrente
094700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094800030715     C* Gestisco l'ultimo campo
094900030715     C                   IF        posA = *zeros
095000040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
095100030715     C* A questo punto "estraggo" il campo corrente
095200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
095300030715     C                                                    (posA-posDa))
095400030715     C                   LEAVE
095500030715     C                   ELSE
095600030715     C* A questo punto "estraggo" il campo corrente
095700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
095800030715     C                                                    (posA-posDa))
095900030715     C                   ENDIF
096000030715     C                   ENDIF
096100030715     C* Incremento il contatore d campo
096200030715     C                   EVAL      i = i +1
096300030715     C                   ENDDO
096400030715     C*
096500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
096600030715     C* delimitazione testo
096700030715     C                   EVAL      i = 1
096800030715     C                   DOW       i < %elem(SkSplitFLD)
096900030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
097000030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
097100030715     C                   EVAL      i = i + 1
097200030715     C                   ENDDO
097300030715     C                   EVAL      wGiro = 1
097400040709     C* Ripristino la 1° riga originale
097500040709     C                   EVAL      vinDTA = depvinDTA
097600030715     C                   ELSE
097700030715     C                   EVAL      wGiro = 2
097800031204     C**********
097900031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
098000031204     C* d separatore campo
098100031204     C**********
098200040709     C                   EVAL      vindta = %trim(vindta)
098300040709     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
098400040709     C                   EVAL      vindta = %subst(vindta:2)
098500040709     C                   ENDDO
098600040709     C*
098700040709     C                   Z-ADD     *zeros        lunghInput        4 0
098800040709     C                   EVAL      lunghInput = %len(%trim(vindta))
098900040709     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
099000040709     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
099100040709     C                   EVAL      lunghInput = %len(%trim(vindta))
099200040709     C                   ENDDO
099300030715     C**********
099400030715     C* Effettuo lo split del campo dati d input
099500030715     C**********
099600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
099700040324     C                   DOW       posDa <= %len(%trimr(vindta))
099800030715     C*
099900030715     C* Gestisco il 1° campo
100000030715     C                   IF        i = 1
100100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
100200030715     C                   EVAL      posDa = 1
100300030715     C* Eseguo lo scan x trovare la fine del primo campo
100400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
100500030715     C* A questo punto "estraggo" il campo corrente
100600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
100700030715     C                                                    (posA-posDa))
100800030715     C* X i campi successivi al 1°
100900030715     C                   ELSE
101000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
101100030715     C                   EVAL      posDa = posA + 1
101200031204     C* Verifico che nn vi sia il campo nn valorizzato
101300031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
101400031204     C* Se campo nn valorizzato skippo al prossimo
101500031204     C                   EVAL      SkSplitCSV(i) = *blanks
101600031204     C                   EVAL      posA  = posA + 1
101700031204     C                   ELSE
101800030715     C* Eseguo lo scan x trovare la fine del campo corrente
101900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
102000030715     C* Gestisco l'ultimo campo
102100030715     C                   IF        posA = *zeros
102200040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
102300030715     C* A questo punto "estraggo" il campo corrente
102400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
102500030715     C                                                    (posA-posDa))
102600030715     C                   LEAVE
102700030715     C                   ELSE
102800030715     C* A questo punto "estraggo" il campo corrente
102900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
103000030715     C                                                    (posA-posDa))
103100030715     C                   ENDIF
103200031204     C                   ENDIF
103300030715     C                   ENDIF
103400030715     C* Incremento il contatore d campo
103500030715     C                   EVAL      i = i +1
103600030715     C                   ENDDO
103700030715     C*
103800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
103900030715     C* delimitazione testo
104000030715     C                   EVAL      i = 1
104100030715     C                   DOW       i < %elem(SkSplitCSV)
104200030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
104300030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
104400030715     C                   EVAL      i = i + 1
104500030715     C                   ENDDO
104600030715     C*
104700030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
104800030715     C                   EVAL      i = 1
104900030715     C                   DOW       i < %elem(SkSplitFLD)
105000030715     C*
105100030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
105200030715     C                   IF        SkSplitFLD(i) <> *blanks AND
105300030715     C                             SkSplitCSV(i) <> *blanks
105400030721     C*** ATB
105500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATB'
105600040708     C                   EVAL      VAOATB = SkSplitCSV(i)
105700030721     C                   ENDIF
105800040708     C*** TOR
105900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTOR'
106000040708     C                   EVAL      VAOTOR = SkSplitCSV(i)
106100040708     C                   ENDIF
106200040708     C*** TCO
106300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTCO'
106400040708     C                   EVAL      VAOTCO = SkSplitCSV(i)
106500040708     C                   ENDIF
106600040708     C*** RSO
106700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSO'
106800040708     C                   EVAL      VAORSO = SkSplitCSV(i)
106900030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
107000040708     C     '@':'A'       XLATE     VAORSO        VAORSO
107100030715     C* ==
107200030715     C                   ENDIF
107300040708     C*** INO
107400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINO'
107500040708     C                   EVAL      VAOINO = SkSplitCSV(i)
107600030715     C                   ENDIF
107700040708     C*** LOO
107800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOO'
107900040708     C                   EVAL      VAOLOO = SkSplitCSV(i)
108000030715     C                   ENDIF
108100040708     C*** PRO
108200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRO'
108300040708     C                   EVAL      VAOPRO = SkSplitCSV(i)
108400030715     C                   ENDIF
108500040708     C*** NAO
108600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAO'
108700040708     C                   EVAL      VAONAO = SkSplitCSV(i)
108800030721     C                   ENDIF
108900040708     C*** RSR
109000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSR'
109100040708     C                   EVAL      VAORSR = SkSplitCSV(i)
109200040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
109300040708     C     '@':'A'       XLATE     VAORSR        VAORSR
109400040708     C* ==
109500040708     C                   IF        VAORSR = *blanks
109600040708     C                   SETON                                        31
109700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
109800040708     C                             + ' ' + 'VAORSR'
109900040708     C                   ENDIF
110000040708     C                   ENDIF
110100040708     C*** INR
110200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINR'
110300040708     C                   EVAL      VAOINR = SkSplitCSV(i)
110400040708     C                   IF        VAOINR = *blanks
110500040708     C                   SETON                                        31
110600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
110700040708     C                             + ' ' + 'VAOINR'
110800040708     C                   ENDIF
110900040708     C                   ENDIF
111000040708     C*** LOR
111100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOR'
111200040708     C                   EVAL      VAOLOR = SkSplitCSV(i)
111300040708     C                   IF        VAOLOR = *blanks
111400040708     C                   SETON                                        31
111500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
111600040708     C                             + ' ' + 'VAOLOR'
111700040708     C                   ENDIF
111800040708     C                   ENDIF
111900040708     C*** PRR
112000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRR'
112100040708     C                   EVAL      VAOPRR = SkSplitCSV(i)
112200040708     C                   IF        VAOPRR = *blanks
112300040708     C                   SETON                                        31
112400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
112500040708     C                             + ' ' + 'VAOPRR'
112600040708     C                   ENDIF
112700040708     C                   ENDIF
112800040708     C*** NAR
112900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAR'
113000040708     C                   EVAL      VAONAR = SkSplitCSV(i)
113100040708     C                   ENDIF
113200040708     C*** RER
113300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORER'
113400040708     C                   EVAL      VAORER = SkSplitCSV(i)
113500040708     C                   ENDIF
113600040708     C*** TER
113700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTER'
113800040708     C                   EVAL      VAOTER = SkSplitCSV(i)
113900040708     C                   ENDIF
114000150923     C*** EMLR
114100150923     C                   IF        %trim(SkSplitFLD(i)) = 'VAOEMLR'
114200150923     C                   EVAL      w_VAOEMLR = SkSplitCSV(i)
114300150923     C                   ENDIF
114400040708     C*** RSC
114500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSC'
114600040708     C                   EVAL      VAORSC = SkSplitCSV(i)
114700040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
114800040708     C     '@':'A'       XLATE     VAORSC        VAORSC
114900040708     C* ==
115000040708     C                   ENDIF
115100040708     C*** INC
115200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINC'
115300040708     C                   EVAL      VAOINC = SkSplitCSV(i)
115400040708     C                   ENDIF
115500040708     C*** LOC
115600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOC'
115700040708     C                   EVAL      VAOLOC = SkSplitCSV(i)
115800040708     C                   ENDIF
115900040708     C*** PRC
116000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRC'
116100040708     C                   EVAL      VAOPRC = SkSplitCSV(i)
116200040708     C                   ENDIF
116300040708     C*** NAC
116400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAC'
116500040708     C                   EVAL      VAONAC = SkSplitCSV(i)
116600040708     C                   ENDIF
116700040708     C*** FFD
116800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFFD'
116900040708     C                   EVAL      VAOFFD = SkSplitCSV(i)
117000040708     C                   ENDIF
117100040708     C*** RMP
117200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORMP'
117300040708     C                   EVAL      VAORMP = SkSplitCSV(i)
117400040708     C                   ENDIF
117500040708     C*** NAM
117600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAM'
117700040708     C                   EVAL      VAONAM = SkSplitCSV(i)
117800040708     C                   ENDIF
117900040708     C*** PAG
118000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPAG'
118100040708     C                   EVAL      VAOPAG = SkSplitCSV(i)
118200040708     C                   ENDIF
118300040708     C*** CTR
118400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCTR'
118500040708     C                   EVAL      VAOCTR = SkSplitCSV(i)
118600040708     C                   ENDIF
118700040708     C*** ZOR
118800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOZOR'
118900040708     C                   EVAL      VAOZOR = SkSplitCSV(i)
119000040708     C                   ENDIF
119100040708     C*** NO1
119200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO1'
119300040708     C                   EVAL      VAONO1 = SkSplitCSV(i)
119400030715     C                   ENDIF
119500040708     C*** NO2
119600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO2'
119700040708     C                   EVAL      VAONO2 = SkSplitCSV(i)
119800030715     C                   ENDIF
119900040708     C*** DDT
120000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODDT'
120100040708     C                   EVAL      VAODDT = SkSplitCSV(i)
120200030715     C                   ENDIF
120300040708     C*** RFA
120400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORFA'
120500040708     C                   EVAL      VAORFA = SkSplitCSV(i)
120600030715     C                   ENDIF
120700040708     C*** SPI
120800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSPI'
120900040708     C                   EVAL      VAOSPI = SkSplitCSV(i)
121000030715     C                   ENDIF
121100040708     C*** FLO
121200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
121300040708     C                   EVAL      VAOFLO = SkSplitCSV(i)
121400040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
121500080708     C* e la posizione 7 del VAOFLO
121600040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
121700040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
121800050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
121900030715     C                   ENDIF
122000091103     C
122100091103     C*** DAR
122200050314     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
122300050412     C                   EVAL      PiStr=SkSplitCSV(i)
122400050412     C                   EXSR      CHKNUM
122500050412     C                   IF        PiInt=*on
122600050412     C                   Z-ADD     PiVal         VAODAR
122700050412     C                   ELSE
122800050412     C                   SETON                                        32
122900050412     C                   EVAL      VAODAR = *zeros
123000050412     C                   EVAL      vinmsg = %trimr(vinmsg)
123100050412     C                             + ' ' + 'VAODAR'
123200050412     C                   ENDIF
123300050314     C                   ENDIF
123400030715     C
123500030721     C*
123600030721     C* Reperisco quindi i campi numerici...
123700040708     C*** POE
123800040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
123900040723     C                   EVAL      PiStr=SkSplitCSV(i)
124000040723     C                   EXSR      CHKNUM
124100040723     C                   IF        PiInt=*on    AND
124200040723     C                             PiVal<=999   AND
124300040723     C                             PiVal>*zeros
124400040723     C                   Z-ADD     PiVal         VAOPOE
124500040723     C                   ELSE
124600040723     C                   SETON                                        32
124700040723     C                   EVAL      VAOPOE = *zeros
124800040723     C                   EVAL      vinmsg = %trimr(vinmsg)
124900040723     C                             + ' ' + 'VAOPOE'
125000040723     C                   ENDIF
125100040723     C                   ENDIF
125200040708     C*** COR
125300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
125400040708     C                   EVAL      PiStr=SkSplitCSV(i)
125500040708     C                   EXSR      CHKNUM
125600040708     C                   IF        PiInt=*on
125700040708     C                   Z-ADD     PiVal         VAOCOR
125800040708     C                   ELSE
125900040708     C                   SETON                                        31
126000040708     C                   EVAL      VAOCOR = *zeros
126100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
126200040708     C                             + ' ' + 'VAOCOR'
126300040708     C                   ENDIF
126400040708     C                   ENDIF
126500040708     C*** CAO
126600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
126700030721     C                   EVAL      PiStr=SkSplitCSV(i)
126800030721     C                   EXSR      CHKNUM
126900030721     C                   IF        PiInt=*on
127000030721     C                   Z-ADD     PiVal         Num5_0
127100040708     C                   MOVEL(p)  Num5_0        VAOCAO
127200030721     C                   ELSE
127300030721     C                   SETON                                        32
127400040708     C                   EVAL      VAOCAO = *zeros
127500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127600040708     C                             + ' ' + 'VAOCAO'
127700030721     C                   ENDIF
127800030721     C                   ENDIF
127900040708     C*** CRA
128000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
128100030721     C                   EVAL      PiStr=SkSplitCSV(i)
128200030721     C                   EXSR      CHKNUM
128300030721     C                   IF        PiInt=*on
128400040708     C                   Z-ADD     PiVal         VAOCRA
128500030721     C                   ELSE
128600030721     C                   SETON                                        32
128700040708     C                   EVAL      VAOCRA = *zeros
128800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128900040708     C                             + ' ' + 'VAOCRA'
129000030721     C                   ENDIF
129100030721     C                   ENDIF
129200040708     C*** CAR
129300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
129400040708     C                   EVAL      PiStr=SkSplitCSV(i)
129500040708     C                   EXSR      CHKNUM
129600040708     C                   IF        PiInt=*on
129700040708     C                   Z-ADD     PiVal         Num5_0
129800040708     C                   MOVEL(p)  Num5_0        VAOCAR
129900040708     C                   ELSE
130000040708     C                   SETON                                        31
130100040708     C                   EVAL      VAOCAR = *zeros
130200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
130300040708     C                             + ' ' + 'VAOCAR'
130400040708     C                   ENDIF
130500040708     C                   ENDIF
130600040708     C*** CRC
130700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
130800040708     C                   EVAL      PiStr=SkSplitCSV(i)
130900040708     C                   EXSR      CHKNUM
131000040708     C                   IF        PiInt=*on
131100040708     C                   Z-ADD     PiVal         VAOCRC
131200040708     C                   ELSE
131300040708     C                   SETON                                        32
131400040708     C                   EVAL      VAOCRC = *zeros
131500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
131600040708     C                             + ' ' + 'VAOCRC'
131700040708     C                   ENDIF
131800040708     C                   ENDIF
131900040708     C*** CAC
132000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
132100040708     C                   EVAL      PiStr=SkSplitCSV(i)
132200040708     C                   EXSR      CHKNUM
132300040708     C                   IF        PiInt=*on
132400040708     C                   Z-ADD     PiVal         Num5_0
132500040708     C                   MOVEL(p)  Num5_0        VAOCAC
132600040708     C                   ELSE
132700040708     C                   SETON                                        32
132800040708     C                   EVAL      VAOCAC = *zeros
132900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
133000040708     C                             + ' ' + 'VAOCAC'
133100040708     C                   ENDIF
133200040708     C                   ENDIF
133300040708     C*** DAR
133400090428     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
133500090428     C                   EVAL      PiStr=SkSplitCSV(i)
133600090428     C                   EXSR      CHKNUM
133700090428     C                   IF        PiInt=*on
133800090428     C                   Z-ADD     PiVal         VAODAR
133900090428     C                   ELSE
134000090428     C                   SETON                                        32
134100090428     C                   EVAL      VAODAR = *zeros
134200090428     C                   EVAL      vinmsg = %trimr(vinmsg)
134300090428     C                             + ' ' + 'VAODAR'
134400090428     C                   ENDIF
134500090428     C                   ENDIF
134600040708     C*** ORR
134700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
134800040708     C                   EVAL      PiStr=SkSplitCSV(i)
134900040708     C                   EXSR      CHKNUM
135000040708     C                   IF        PiInt=*on
135100040708     C                   Z-ADD     PiVal         VAOORR
135200040708     C                   ELSE
135300040708     C                   SETON                                        32
135400040708     C                   EVAL      VAOORR = *zeros
135500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
135600040708     C                             + ' ' + 'VAOORR'
135700040708     C                   ENDIF
135800040708     C                   ENDIF
135900030721     C*** NCL
136000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
136100030721     C                   EVAL      PiStr=SkSplitCSV(i)
136200030721     C                   EXSR      CHKNUM
136300030721     C                   IF        PiInt=*on
136400040723     C                   IF        PiVal > *zeros
136500040708     C                   Z-ADD     PiVal         VAONCL
136600040723     C                   ENDIF
136700030721     C                   ELSE
136800030721     C                   SETON                                        32
136900040708     C                   EVAL      VAONCL = *zeros
137000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137100040708     C                             + ' ' + 'VAONCL'
137200030721     C                   ENDIF
137300030721     C                   ENDIF
137400040708     C*** PKG
137500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
137600030721     C                   EVAL      PiStr=SkSplitCSV(i)
137700030721     C                   EXSR      CHKNUM
137800030721     C                   IF        PiNum=*on
137900040723     C                   IF        PiVal > *zeros
138000040708     C                   Z-ADD     PiVal         VAOPKG
138100040723     C                   ENDIF
138200030721     C                   ELSE
138300030721     C                   SETON                                        32
138400040708     C                   EVAL      VAOPKG = *zeros
138500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138600040708     C                             + ' ' + 'VAOPKG'
138700030721     C                   ENDIF
138800030721     C                   ENDIF
138900040708     C*** VLM
139000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
139100040708     C                   EVAL      PiStr=SkSplitCSV(i)
139200040708     C                   EXSR      CHKNUM
139300040708     C                   IF        PiNum=*on
139400040708     C                   Z-ADD     PiVal         VAOVLM
139500040708     C                   ELSE
139600040708     C                   SETON                                        32
139700040708     C                   EVAL      VAOVLM = *zeros
139800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
139900040708     C                             + ' ' + 'VAOVLM'
140000040708     C                   ENDIF
140100040708     C                   ENDIF
140200040708     C*** BNC
140300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
140400040708     C                   EVAL      PiStr=SkSplitCSV(i)
140500040708     C                   EXSR      CHKNUM
140600040708     C                   IF        PiInt=*on
140700040708     C                   Z-ADD     PiVal         VAOBNC
140800040708     C                   ELSE
140900040708     C                   SETON                                        32
141000040708     C                   EVAL      VAOBNC = *zeros
141100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
141200040708     C                             + ' ' + 'VAOBNC'
141300040708     C                   ENDIF
141400040708     C                   ENDIF
141500040708     C*** BLC
141600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
141700040708     C                   EVAL      PiStr=SkSplitCSV(i)
141800040708     C                   EXSR      CHKNUM
141900040708     C                   IF        PiInt=*on
142000040708     C                   Z-ADD     PiVal         VAOBLC
142100040708     C                   ELSE
142200040708     C                   SETON                                        32
142300040708     C                   EVAL      VAOBLC = *zeros
142400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
142500040708     C                             + ' ' + 'VAOBLC'
142600040708     C                   ENDIF
142700040708     C                   ENDIF
142800040708     C*** ATT
142900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
143000040708     C                   EVAL      PiStr=SkSplitCSV(i)
143100040708     C                   EXSR      CHKNUM
143200040708     C                   IF        PiInt=*on
143300040708     C                   Z-ADD     PiVal         VAOATT
143400040708     C                   ELSE
143500040708     C                   SETON                                        32
143600040708     C                   EVAL      VAOATT = *zeros
143700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
143800040708     C                             + ' ' + 'VAOATT'
143900040708     C                   ENDIF
144000040708     C                   ENDIF
144100040708     C*** MTC
144200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
144300040708     C                   EVAL      PiStr=SkSplitCSV(i)
144400040708     C                   EXSR      CHKNUM
144500040708     C                   IF        PiInt=*on
144600040708     C                   Z-ADD     PiVal         VAOMTC
144700040708     C                   ELSE
144800040708     C                   SETON                                        32
144900040708     C                   EVAL      VAOMTC = *zeros
145000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
145100040708     C                             + ' ' + 'VAOMTC'
145200040708     C                   ENDIF
145300040708     C                   ENDIF
145400040708     C*** KSC
145500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
145600030721     C                   EVAL      PiStr=SkSplitCSV(i)
145700030721     C                   EXSR      CHKNUM
145800030721     C                   IF        PiInt=*on      AND
145900030721     C                             PiVal<=9999999 AND
146000030721     C                             PiVal>*zeros
146100040708     C                   Z-ADD     PiVal         VAOKSC
146200030721     C                   ELSE
146300030721     C                   SETON                                        32
146400040708     C                   EVAL      VAOKSC = *zeros
146500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
146600040708     C                             + ' ' + 'VAOKSC'
146700030721     C                   ENDIF
146800030721     C                   ENDIF
146900040708     C*** POR
147000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
147100030721     C                   EVAL      PiStr=SkSplitCSV(i)
147200030721     C                   EXSR      CHKNUM
147300030721     C                   IF        PiInt=*on    AND
147400030721     C                             PiVal<=999   AND
147500030721     C                             PiVal>*zeros
147600040708     C                   Z-ADD     PiVal         VAOPOR
147700030721     C                   ELSE
147800030721     C                   SETON                                        32
147900040708     C                   EVAL      VAOPOR = *zeros
148000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
148100040708     C                             + ' ' + 'VAOPOR'
148200030721     C                   ENDIF
148300030721     C                   ENDIF
148400040708     C*** POC
148500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
148600040708     C                   EVAL      PiStr=SkSplitCSV(i)
148700040708     C                   EXSR      CHKNUM
148800040708     C                   IF        PiInt=*on    AND
148900040708     C                             PiVal<=999   AND
149000040708     C                             PiVal>*zeros
149100040708     C                   Z-ADD     PiVal         VAOPOC
149200040708     C                   ELSE
149300040708     C                   SETON                                        32
149400040708     C                   EVAL      VAOPOC = *zeros
149500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
149600040708     C                             + ' ' + 'VAOPOC'
149700040708     C                   ENDIF
149800040708     C                   ENDIF
149900040708     C*** STO
150000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
150100040708     C                   EVAL      PiStr=SkSplitCSV(i)
150200040708     C                   EXSR      CHKNUM
150300040708     C                   IF        PiInt=*on
150400040708     C                   Z-ADD     PiVal         VAOSTO
150500040708     C                   ELSE
150600040708     C                   SETON                                        32
150700040708     C                   EVAL      VAOSTO = *zeros
150800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
150900040708     C                             + ' ' + 'VAOSTO'
151000040708     C                   ENDIF
151100040708     C                   ENDIF
151200090428     C*
151300091103     C********** GESTIONE CAMPI PERSONALIZZATI
151400150923
151500090428     C*
151600090428     C****************************************
151700090428     C*
151800030715     C*
151900030715     C                   ENDIF
152000030715     C                   EVAL      i = i + 1
152100030715     C                   ENDDO
152200040723     C*
152300121207     C***  ===>  Gestione campi "particolari" fuori ciclo
152400121207     C*
152500140108
152600140108     C*
152700140108     C* se VAORER non è valorizzato copiare il valore inserito nel campo VAORSR
152800140108     C                   IF        VAORER = *blank
152900140108     C                   EVAL      VAORER = VAORSR
153000140108     C                   ENDIF
153100140108     C*
153200160209     C* le nazioni non devono essere valorizzate a ITALIA
153300160209     C                   IF        VAONAO = 'I' or
153400160209     C                             VAONAO = 'IT' or
153500160209     C                             VAONAO = 'ITA'
153600160209     C                   EVAL      VAONAO = *blank
153700160209     C                   ENDIF
153800160209     C                   IF        VAONAR = 'I' or
153900160209     C                             VAONAR = 'IT' or
154000160209     C                             VAONAR = 'ITA'
154100160209     C                   EVAL      VAONAR = *blank
154200160209     C                   ENDIF
154300160209     C                   IF        VAONAC = 'I' or
154400160209     C                             VAONAC = 'IT' or
154500160209     C                             VAONAC = 'ITA'
154600160209     C                   EVAL      VAONAC = *blank
154700160209     C                   ENDIF
154800160209     C*
154900130513
155000030715     C                   ENDIF
155100020204     C*
155200010607     C                   ENDSR
155300010607     C*----------------------------------------------------*
155400080702
155500080702
155600080702     C*----------------------------------------------------*
155700080702     C*  CONTROLLO NUMERICITA' CAMPI
155800080702     C*----------------------------------------------------*
155900080702     C     CHKNUM        BEGSR
156000080702     C*
156100080702     C                   IF        PiDecChr = *blanks
156200080702     C                   EVAL      PiDecChr = ','
156300080702     C                   ENDIF
156400080702     C*
156500080702     C                   callp     UBISNUM_Check(PiStr
156600080702     C                                          :PiDecChr
156700080702     C                                          :PiVal
156800080702     C                                          :PiNum
156900080702     C                                          :PiInt)
157000080702     C*
157100080702     C                   ENDSR
157200080702     C***
157300080702
157400010601
157500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
157600010601     C     repfil        BEGSR
157700010601     C*
157800010601     C                   if        invfil = *zeros and
157900010601     C                             depfil > *zeros and
158000010629     C                             (vinflg = *blanks or
158100010629     C                              vinflg = *zeros)
158200010601     C
158300010601     C                   eval      invfil = depfil
158400010601     C                   endif
158500010601     C*
158600010601     C                   if        depfil <> invfil and
158700010601     C                             invfil > *zeros
158800010601     C                   eval      flgMulti = '1'
158900010601     C                   if        vinflg = *blanks
159000010601     C                   add       1             cntNonEl
159100010601     C                   endif
159200010601     C                   endif
159300010601     C*
159400010601     C                   if        vinflg = '2'
159500010601     C                   eval      flgStato = '2'
159600010601     C                   endif
159700010601     C*
159800010601     C                   ENDSR
159900010601     C***
160000010330
160100010330
160200990920      /TITLE Invio dei dati al punto operativo.
160300000613     C     invio         BEGSR
160400990920     C*
160500990920     C                   reset                   dscmz
160600010601     C                   move      invfil        cmzdst
160700990920     C                   eval      cmzfld = vlrfou
160800990920     C                   eval      cmzmbd = vlrhdl
160900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
161000000710     C                   if        prmfir = *blanks
161100040708     C                   eval      cmzfla = 'FNVAO00F'
161200040708     C                   eval      cmzmba = 'FNVAO00F'
161300000710     C                   else
161400000710     C                   eval      cmzfla = prmfir
161500000710     C                   eval      cmzmba = prmfir
161600000710     C                   endif
161700990920     C                   eval      cmznrr = *zeros
161800990920     C                   move      §ctrok        cmznrr
161900021018     C                   eval      cmzlba = vlrfl1
162000990920     C                   call(e)   'TIS711C'
162100990920     C                   parm                    dscmz
162200990921     C                   parm      *blanks       esito
162300990920     C                   if        %error
162400990920     C                             or cmzerr = '1'
162500990921     C                             or esito  = '1'
162600000710     C                   eval      wrkesito = '3'
162700990920     C                   endif
162800990920     C*
162900000613     C                   ENDSR
163000010601
163100010601
163200010601      /TITLE Invio dei dati al punto operativo.
163300010601     C     opeini        BEGSR
163400010601     C*
163500010601     C* Inizializzo flag e contatori operativi
163600010601     C                   movel     '0'           flgGiro           1
163700010601     C                   movel     '0'           flgMulti          1
163800010601     C                   movel     '1'           flgStato          1
163900010615     C                   movel     '0'           flgOk             1
164000010601     C                   z-add     *zeros        cntNonEl         10 0
164100010601     C                   z-add     *zeros        depfil            3 0
164200010601     C                   z-add     *zeros        invfil            3 0
164300010601     C*
164400010601     C                   ENDSR
164500010601     C***
164600050421
164700050421
164800091130     C     *pssr         BEGSR
164900050421     C*
165000091130     C                   eval      wrkesito = '2'
165100050421     C*
165200091130     C                   ENDSR     '*CANCL'
165300050421     C***
165400060306
165500060306
165600060306     C*-------------------------------------------------------------------------*
165700060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
165800060306     C*-------------------------------------------------------------------------*
165900060306     C     sr_Control    begsr
166000060306     C*
166100060306     C* inizializzo a ok il flag d controllo abilitazione
166200060306     C                   eval      Cli_Abil = *on
166300060306     C
166400060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
166500060306     C                   clear                   woriIDC
166600060306     C                   eval      kteCOD = 'VAP'
166700060306     C                   eval      kteKE1 = oriIDC
166800060306     C     keytbe        chain     TNTBE01L
166900060306     C                   if        %found(Tntbe01l)
167000060306     C                   eval      woriIDC = TbeKe2
167100060306     C                   else
167200060306     C                   eval      Cli_Abil = *off
167300060306     C                   endif
167400060306     C*
167500060306     C                   endsr
167600010330
167700010330
167800000613     C     *inzsr        BEGSR
167900990910     C*
168000990910     C     *entry        plist
168100990920     C                   parm                    tivlrds
168200990921     C                   parm      wrkesito      esito
168300000724     C                   parm                    prmlit
168400000710     C                   parm                    prmfir
168500010330     C*
168600010330     C* CALCOLA LA DATA CORRENTE
168700091127     C                   z-add     *zeros        datcor            8 0
168800091127     C                   time                    wn14             14 0
168900091127     C                   movel     wn14          wn6               6 0          *ORA (6)
169000130516     C                   eval      datcor = %dec(%date() : *ISO)
169100040723     C*
169200040723     C* Campi riferito al database
169300040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
169400040723     C     *like         define    tbeke1        kteKE1
169500040723     C*
169600040723     C* Definizione chiavi di lettura
169700040723     C* TNTBE01L - parziale
169800040723     C     keytbe        klist
169900040723     C                   kfld                    kteCOD                         *UTENTE
170000040723     C                   kfld                    kteKE1                         *CODICE
170100000613     C*
170200000613     C                   ENDSR
170300000613     C***
170400990908
170500040709** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
170600160209X;VAOPOE;X;X;X;VAOTOR;X;VAODAO;VAOOAO;VAOCOR;X;X;X;X;X;X;X;VAORSR;VAOINR;VAOCAR;VAOLOR;VAOPRR;
170700160314VAONAR;VAORER;VAOTER;X;VAORSC;VAOINC;VAOCAC;VAOLOC;VAOPRC;VAONAC;X;VAODAR;VAOORR;VAORMP;VAONAM;
170800160316VAONCL;VAOPKG;X;X;X;X;X;VAOPAG;X;X;X;X;X;VAONO1;VAONO2;VAODDT;VAORFA;
170900160316X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;
