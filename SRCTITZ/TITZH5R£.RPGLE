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
001500151111      *
001600121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
001700120209     F*
001800040723     Ffnacr01l  if   e           k disk
001900090618     Ftntbe01l  if   e           k disk
002000990910     Ftivin00r  uF   E             DISK    usropn
002100040708     FFnvaowwr  O    E             DISK    usropn
002200040723     Ftiori00f  O    E             DISK
002300050826     FTIVGD00F  O    E             DISK
002400000313     D*
002500040709     D*------------
002600040709     D* SCHIERE A PROGRAMMA
002700040709     D*------------
002800040709     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
002900010330     D*----------------------------------------------------
003000010330     D* DICHIARAZIOINE VARIABILI DI WRK
003100010330     D*----------------------------------------------------
003200010330     D dscmz         e ds                  inz
003300010330     D psds           sds
003400010330     D  procname         *PROC
003500010330     D tivlrds       e ds                  extname(tivlr00f)
003600050826     D fnvapds       e ds                  extname(fnvap00f)
003700040723     D dorm01        e ds
003800040723     D dfar          e ds
003900090618     d trul33ds      e ds
004000090618     d kpjba         e ds
004100150923     D fnvaoear      e ds                  inz
004200150923     D                                     prefix(w_)
004300010330     D esito           s              1
004400010330     D prmlit          s             10
004500010330     D prmfir          s             10
004600010330     D wrkesito        s                   like(esito)
004700010330     D rrnum           s              6  0 INZ(*zeros)
004800040709     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
004900060306     D Cli_Abil        s              1    inz(*off)
005000060306     D woriIDC         s                   like(oriIDC) inz(*blanks)
005100090618     d §numori         s              7  0 inz
005200120123     d wData_EUR       s               d   datfmt(*eur)
005300120123     d wData_ISO       s               d   datfmt(*iso)
005400120905     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
005500120905     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
005600151111     D Intestazioni    s               n   inz(*off)
005700030715     D*------------------
005800030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005900030715     D*------------------
006000030715     D SkSplitFLD      S             10    DIM(100)
006100030715     D SkSplitCSV      S            256    DIM(100)
006200030715     D CharCSV         S              1
006300030715     D CharTXT         S              1
006400030715     D CharNUM         S              1
006500030715     D posDa           S              3  0 INZ(*zeros)
006600030715     D posA            S              3  0 INZ(*zeros)
006700030715     D i               s              3  0 INZ(1)
006800030715     D wGiro           s              1  0 INZ(*zeros)
006900030822     D*------------------
007000030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
007100030822     D*------------------
007200030822     D posDaDft        S              3  0 INZ(*zeros)
007300030822     D posADft         S              3  0 INZ(*zeros)
007400030822     D j               s              3  0 INZ(1)
007500030822     D wGiroDft        s              1  0 INZ(*zeros)
007600040723     D*------------------
007700040723     D* DS REPERIMENTO DATI CLIENTE
007800040723     D*-------------------
007900040723     D BS69DS        E DS                  EXTNAME(TIBS69DS)
008000040723     D ACODS         E DS                  EXTNAME(CNACO00F)
008100040723     D INDDS         E DS                  EXTNAME(CNIND00F)
008200040723     D CLPDS         E DS                  EXTNAME(CNCLP00F)
008300040723     D CLSDS         E DS                  EXTNAME(FNCLS00F)
008400121129     D*------------------
008500150923     D w70             s             70
008600080702
008700080702     D*------------------
008800080702     D* LINKING A DEFINIZIONI ESTERNE
008900080702     D*------------------
009000090428     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009100090428     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009200091026
009300091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
009400091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
009500091026
009600080702
009700031201     D*------------------
009800031201     D* Costanti
009900031201     D*------------------
010000031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
010100031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
010200010330
010300010330
010400010330
010500000913     C                   reset                   rrnum
010600990921     C                   reset                   esito
010700990921     C                   reset                   wrkesito
010800010601     C*
010900010601     C                   exsr      opeini
011000041011     C                   exsr      PREELA
011100040708     C                   exsr      rwvao
011200010601     C*
011300010601     C                   seton                                        lr
011400010601
011500010601
011600010601
011700010601
011800010601     C*--------------------------------------------------------
011900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
012000010601     C*--------------------------------------------------------
012100010601     C     PREELA        BEGSR
012200010601     C*
012300010601     C* SE OCCORRE SPEDIRE IN FILIALE
012400041011     C                   if        flgGiro = '0'
012500010601     C*
012600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012700010601     C                   eval      flgGiro = '1'
012800041011     C*
012900041011     C                   endif
013000010601     C*
013100010601     C                   ENDSR
013200010601     C***
013300010601
013400010601
013500010601     C*--------------------------------------------------------
013600010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013700010601     C*--------------------------------------------------------
013800010601     C     ENDELA        BEGSR
013900000616     C*
014000120209     C                   ENDSR
014100010601
014200010601
014300010330     C*--------------------------------------------------------
014400040708     C* RWVAO   LEGGE TIVIN00R E SCRIVE FNVAOWWF              *
014500010330     C*--------------------------------------------------------
014600040708     C     RWVAO         BEGSR
014700010330     C*
014800010330     C                   if        not %open(tivin00r)
014900010330     C                   open      tivin00r
015000010330     C                   endif
015100040708     C                   if        not %open(fnvaowwr)
015200040708     C                   open      fnvaowwr
015300010330     C                   endif
015400150923     C*
015500150923     C* Mi predispongo per scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
015600150923     C                   call      'TIS7T7ER'
015700150923     C                   parm      'O'           pIn_Opz           1
015800150923     C                   parm                    tivlrds
015900150923     C                   parm                    FNVAOEAR
016000150923     C                   parm                    oriPRG
016100150923     C                   parm      *zeros        pIn_Dst           3
016200150923     C                   parm      *blanks       pOut_Esito        1
016300150923     C                   if        pOut_Esito = '2'
016400150923     C                   goto      FINE
016500150923     C                   endif
016600010330     C*
016700010604     C                   clear                   §CTROK
016800010604     C                   clear                   §CTRMO
016900010604     C                   clear                   §CTRNO
017000120123     C*
017100120905     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
017200120905     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
017300120905     C                   IF        Intestazioni = *off
017400120123     C* Eseguo primo giro x by-passare la mancanza della 1° riga con le intestazioni
017500120123     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
017600120123     C                   exsr      inzvar
017700120123     C                   exsr      impvao
017800120905     C                   ENDIF
017900010330     C*
018000030822     C                   do        *HIVAL
018100010330     C*
018200030822     C                   read      tivin00r                               70
018300010618     C*
018400010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018500010618     C                   if        vinflg = '1'
018600010618     C                   eval      flgOk = '1'
018700010618     C                   endif
018800010618     C*
018900040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
019000040510     C                   z-add     *zeros        wLenVINDTA        4 0
019100040510     C                   z-add     *zeros        wFlgVINDTA        1 0
019200040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
019300040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
019400040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
019500040510     C                   z-add     1             wFlgVINDTA
019600040510     C                   leave
019700040510     C                   endif
019800040510     C                   enddo
019900040510     C*
020000040218     C                   if        vindta > *blanks AND
020100040510     C                             wFlgVINDTA = 1
020200010330     C                   add       1             rrnum
020300010330     C*
020400010601     C                   if        *in70 = *off and
020500010330     C                             (vinflg = *blanks
020600010330     C                              or vinflg = '0'
020700010330     C                              or vinflg = '2')
020800010330     C*
020900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021000010711     C                   if        vinflg = *blanks or vinflg = '0'
021100010711     C                   clear                   vinmsg
021200010711     C                   endif
021300151111     C*
021400151111     C* Aggiungo un byte a blanks all'inizio d ogni record
021500151111     C* siccome sappiamo che la prima colonna è vuota, mettiamo un carattere non vuoto per
021600151111     C* evitare che vengano elisi i blank
021700151111     C                   eval      vindta  = '*' + %trim(vindta)
021800010601     C*
021900010330     C                   exsr      inzvar
022000040708     C                   exsr      impvao
022100151019     C*
022200151019     C* Stacco subito un numeratore univoco "ORM"
022300151019     C                   exsr      RTVNUM
022400150923     C*
022500150923     C* Verifico se trattasi di formato ORM "esteso"
022600150923     C                   setoff                                       53
022700150923     C                   if        w_VAOEMLR  <> *blanks
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
053300150923     C*                  movel     §numori       §ormpg
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
058700120123     C                   eval      vgdPGM = 'TITZ10R'
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
063700151111     C                   EVAL      VAOCOR = 0013185000
063800151111     C                   EVAL      VAOKSC = 0013185
063900151111     C                   EVAL      VAOCTR = '000'
064000151111     C                   EVAL      VAOPAG = 'O'
064100151111     C                   EVAL      VAODDT = 'N'
064200130513
064300120905     C* se il 1° char dei parametri è T=Test
064400120905     C                   SETOFF                                       55
064500120905     C                   IF        %subst(vlrppt:1:1) = 'T'
064600120905     C* inibisco la scrittura dei file TIORI e TIVGD
064700120905     C                   SETON                                        55
064800120905     C* imposto il PO di emissione come filiale fittizia
064900120905     C                   EVAL      VAOPOE = 999
065000120905     C                   ENDIF
065100091127     C*
065200091204     C* IMPOSTO I CAMPI PERSONALIZZATI
065300100416     C
065400030822     C*
065500030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
065600030822     C                   EVAL      posDaDft = 1
065700030822     C                   EVAL      posADft  = 0
065800030822     C                   EVAL      wGiroDft = 0
065900030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
066000030822     C                             posDaDft > 0
066100030822     C*
066200030822     C* Gestisco il 1° giro
066300030822     C                   IF        wGiroDft = 0
066400030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
066500030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
066600030822     C* Incremento il contatore dei "giri"
066700030822     C                   EVAL      wGiroDft = 1
066800030822     C                   ELSE
066900030822     C                   EVAL      posDaDft = posADft
067000030822     C                   ENDIF
067100030822     C* Eseguo lo scan x trovare la fine del campo corrente
067200030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
067300030822     C*
067400030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
067500030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
067600030822     C                   IF        posDaDft > 0 AND
067700030822     C                             posADft  > 0
067800040708     C* POE
067900040708     C                   IF        %subst(
068000040708     C                             %subst(vlrppt:posDaDft+1:
068100040708     C                             posADft-posDaDft-1):1:3)
068200040708     C                             = 'POE'
068300040708     C                   EVAL      PiStr=%trim(%subst(
068400040708     C                             %subst(vlrppt:posDaDft+1:
068500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068600040708     C                   EXSR      CHKNUM
068700040708     C                   IF        PiInt=*on
068800040708     C                   Z-ADD     PiVal         VAOPOE
068900040708     C                   ENDIF
069000040708     C                   ENDIF
069100040708     C* TOR
069200040708     C                   IF        %subst(
069300040708     C                             %subst(vlrppt:posDaDft+1:
069400040708     C                             posADft-posDaDft-1):1:3)
069500040708     C                             = 'TOR'
069600040708     C                   EVAL      VAOTOR=%trim(%subst(
069700040708     C                             %subst(vlrppt:posDaDft+1:
069800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069900040708     C                   ENDIF
070000040708     C* TCO
070100040708     C                   IF        %subst(
070200040708     C                             %subst(vlrppt:posDaDft+1:
070300040708     C                             posADft-posDaDft-1):1:3)
070400040708     C                             = 'TCO'
070500040708     C                   EVAL      VAOTCO=%trim(%subst(
070600040708     C                             %subst(vlrppt:posDaDft+1:
070700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070800040708     C                   ENDIF
070900040707     C* COR
071000030822     C                   IF        %subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):1:3)
071300040707     C                             = 'COR'
071400030822     C                   EVAL      PiStr=%trim(%subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071700030822     C                   EXSR      CHKNUM
071800030822     C                   IF        PiInt=*on
071900040708     C                   Z-ADD     PiVal         VAOCOR
072000030822     C                   ENDIF
072100030822     C                   ENDIF
072200040708     C* CRC
072300040708     C                   IF        %subst(
072400040708     C                             %subst(vlrppt:posDaDft+1:
072500040708     C                             posADft-posDaDft-1):1:3)
072600040708     C                             = 'CRC'
072700040708     C                   EVAL      PiStr=%trim(%subst(
072800040708     C                             %subst(vlrppt:posDaDft+1:
072900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073000040708     C                   EXSR      CHKNUM
073100040708     C                   IF        PiInt=*on
073200040708     C                   Z-ADD     PiVal         VAOCRC
073300040708     C                   ENDIF
073400040708     C                   ENDIF
073500040708     C* FFD
073600040708     C                   IF        %subst(
073700040708     C                             %subst(vlrppt:posDaDft+1:
073800040708     C                             posADft-posDaDft-1):1:3)
073900040708     C                             = 'FFD'
074000040708     C                   EVAL      VAOFFD=%trim(%subst(
074100040708     C                             %subst(vlrppt:posDaDft+1:
074200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074300040708     C                   ENDIF
074400040708     C* ORR
074500040708     C                   IF        %subst(
074600040708     C                             %subst(vlrppt:posDaDft+1:
074700040708     C                             posADft-posDaDft-1):1:3)
074800040708     C                             = 'ORR'
074900040708     C                   EVAL      PiStr=%trim(%subst(
075000040708     C                             %subst(vlrppt:posDaDft+1:
075100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075200040708     C                   EXSR      CHKNUM
075300040708     C                   IF        PiInt=*on
075400040708     C                   Z-ADD     PiVal         VAOORR
075500040708     C                   ENDIF
075600040708     C                   ENDIF
075700040712     C* NCL
075800040712     C                   IF        %subst(
075900040712     C                             %subst(vlrppt:posDaDft+1:
076000040712     C                             posADft-posDaDft-1):1:3)
076100040712     C                             = 'NCL'
076200040712     C                   EVAL      PiStr=%trim(%subst(
076300040712     C                             %subst(vlrppt:posDaDft+1:
076400040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076500040712     C                   EXSR      CHKNUM
076600040712     C                   IF        PiInt=*on
076700040712     C                   Z-ADD     PiVal         VAONCL
076800040712     C                   ENDIF
076900040712     C                   ENDIF
077000040712     C* PKG
077100040712     C                   IF        %subst(
077200040712     C                             %subst(vlrppt:posDaDft+1:
077300040712     C                             posADft-posDaDft-1):1:3)
077400040712     C                             = 'PKG'
077500040712     C                   EVAL      PiStr=%trim(%subst(
077600040712     C                             %subst(vlrppt:posDaDft+1:
077700040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077800040712     C                   EXSR      CHKNUM
077900040712     C                   IF        PiNum=*on
078000040712     C                   Z-ADD     PiVal         VAOPKG
078100040712     C                   ENDIF
078200040712     C                   ENDIF
078300040712     C* VLM
078400040712     C                   IF        %subst(
078500040712     C                             %subst(vlrppt:posDaDft+1:
078600040712     C                             posADft-posDaDft-1):1:3)
078700040712     C                             = 'VLM'
078800040712     C                   EVAL      PiStr=%trim(%subst(
078900040712     C                             %subst(vlrppt:posDaDft+1:
079000040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079100040712     C                   EXSR      CHKNUM
079200040712     C                   IF        PiNum=*on
079300040712     C                   Z-ADD     PiVal         VAOVLM
079400040712     C                   ENDIF
079500040712     C                   ENDIF
079600040708     C* NAM
079700040708     C                   IF        %subst(
079800040708     C                             %subst(vlrppt:posDaDft+1:
079900040708     C                             posADft-posDaDft-1):1:3)
080000040708     C                             = 'NAM'
080100040708     C                   EVAL      VAONAM=%trim(%subst(
080200040708     C                             %subst(vlrppt:posDaDft+1:
080300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080400040708     C                   ENDIF
080500040708     C* PAG
080600030822     C                   IF        %subst(
080700030822     C                             %subst(vlrppt:posDaDft+1:
080800030822     C                             posADft-posDaDft-1):1:3)
080900040708     C                             = 'PAG'
081000040708     C                   EVAL      VAOPAG=%trim(%subst(
081100030822     C                             %subst(vlrppt:posDaDft+1:
081200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081300030822     C                   ENDIF
081400040708     C* KSC
081500040708     C                   IF        %subst(
081600040708     C                             %subst(vlrppt:posDaDft+1:
081700040708     C                             posADft-posDaDft-1):1:3)
081800040708     C                             = 'KSC'
081900040708     C                   EVAL      PiStr=%trim(%subst(
082000040708     C                             %subst(vlrppt:posDaDft+1:
082100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082200040708     C                   EXSR      CHKNUM
082300040708     C                   IF        PiInt=*on
082400040708     C                   Z-ADD     PiVal         VAOKSC
082500040708     C                   ENDIF
082600040708     C                   ENDIF
082700040708     C* CTR
082800040708     C                   IF        %subst(
082900040708     C                             %subst(vlrppt:posDaDft+1:
083000040708     C                             posADft-posDaDft-1):1:3)
083100040708     C                             = 'CTR'
083200040708     C                   EVAL      VAOCTR=%trim(%subst(
083300040708     C                             %subst(vlrppt:posDaDft+1:
083400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083500040708     C                   ENDIF
083600040708     C* POC
083700040708     C                   IF        %subst(
083800040708     C                             %subst(vlrppt:posDaDft+1:
083900040708     C                             posADft-posDaDft-1):1:3)
084000040708     C                             = 'POC'
084100040708     C                   EVAL      PiStr=%trim(%subst(
084200040708     C                             %subst(vlrppt:posDaDft+1:
084300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084400040708     C                   EXSR      CHKNUM
084500040708     C                   IF        PiInt=*on
084600040708     C                   Z-ADD     PiVal         VAOPOC
084700040708     C                   ENDIF
084800040708     C                   ENDIF
084900040708     C* NO1
085000040708     C                   IF        %subst(
085100040708     C                             %subst(vlrppt:posDaDft+1:
085200121129     C                             posADft-posDaDft-1):1:3)
085300040708     C                             = 'NO1'
085400040708     C                   EVAL      VAONO1=%trim(%subst(
085500040708     C                             %subst(vlrppt:posDaDft+1:
085600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085700040708     C                   ENDIF
085800040708     C* DDT
085900030822     C                   IF        %subst(
086000030822     C                             %subst(vlrppt:posDaDft+1:
086100030822     C                             posADft-posDaDft-1):1:3)
086200040708     C                             = 'DDT'
086300040708     C                   EVAL      VAODDT=%trim(%subst(
086400030822     C                             %subst(vlrppt:posDaDft+1:
086500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086600030822     C                   ENDIF
086700040708     C* RFA
086800040708     C                   IF        %subst(
086900040708     C                             %subst(vlrppt:posDaDft+1:
087000040708     C                             posADft-posDaDft-1):1:3)
087100040708     C                             = 'RFA'
087200040708     C                   EVAL      VAORFA=%trim(%subst(
087300040708     C                             %subst(vlrppt:posDaDft+1:
087400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
087500040708     C                   ENDIF
087600040708     C* FLO
087700040708     C                   IF        %subst(
087800040708     C                             %subst(vlrppt:posDaDft+1:
087900040708     C                             posADft-posDaDft-1):1:3)
088000040708     C                             = 'FLO'
088100040708     C                   EVAL      VAOFLO=%trim(%subst(
088200040708     C                             %subst(vlrppt:posDaDft+1:
088300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
088400091103     C*
088500040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
088600080702     C* e posizione 7 del VAOFLO
088700040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
088800040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
088900050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
089000040708     C                   ENDIF
089100030822     C* ...
089200030822     C                   ENDIF
089300030822     C                   ENDDO
089400020204     C*
089500010330     C                   ENDSR
089600010607     C*----------------------------------------------------*
089700040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
089800010607     C*----------------------------------------------------*
089900040708     C     IMPVAO        BEGSR
090000010607     C*
090100010607     C                   SETOFF                                       3132
090200120124
090300030515     C*
090400040708     C                   MOVEL     datcor        VAODAO
090500040708     C                   MOVEL     wn14          VAOOAO
090600050314     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
090700050314     C     minu:maiu     XLATE     vindta        vindta
090800010607     C*
090900030715     C**********
091000030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
091100030715     C**********
091200030715     C                   IF        wGiro = *zeros
091300040709     C*
091400040709     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
091500040709     C                   EVAL      depvinDTA = vinDTA
091600040709     C*
091700151111     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
091800151111     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
091900151111     C                             %trim(intcol(3))<>*blanks
092000040709     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
092100040709     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
092200040709     C                                      %trim(intcol(3))
092300151111     C     ';':CharCSV   XLATE     vindta        vindta
092400151111     C                   ENDIF
092500031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
092600050314     C**!!!minu:maiu     XLATE     vindta        vindta
092700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
092800040324     C                   DOW       posDa <= %len(%trimr(vindta))
092900030715     C*
093000030715     C* Gestisco il 1° campo
093100030715     C                   IF        i = 1
093200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
093300030715     C                   EVAL      posDa = 1
093400030715     C* Eseguo lo scan x trovare la fine del primo campo
093500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
093600030715     C* A questo punto "estraggo" il campo corrente
093700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
093800030715     C                                                    (posA-posDa))
093900030715     C* X i campi successivi al 1°
094000030715     C                   ELSE
094100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
094200030715     C                   EVAL      posDa = posA + 1
094300030715     C* Eseguo lo scan x trovare la fine del campo corrente
094400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094500030715     C* Gestisco l'ultimo campo
094600030715     C                   IF        posA = *zeros
094700040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
094800030715     C* A questo punto "estraggo" il campo corrente
094900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
095000030715     C                                                    (posA-posDa))
095100030715     C                   LEAVE
095200030715     C                   ELSE
095300030715     C* A questo punto "estraggo" il campo corrente
095400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
095500030715     C                                                    (posA-posDa))
095600030715     C                   ENDIF
095700030715     C                   ENDIF
095800030715     C* Incremento il contatore d campo
095900030715     C                   EVAL      i = i +1
096000030715     C                   ENDDO
096100030715     C*
096200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
096300030715     C* delimitazione testo
096400030715     C                   EVAL      i = 1
096500030715     C                   DOW       i < %elem(SkSplitFLD)
096600030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
096700030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
096800030715     C                   EVAL      i = i + 1
096900030715     C                   ENDDO
097000030715     C                   EVAL      wGiro = 1
097100040709     C* Ripristino la 1° riga originale
097200040709     C                   EVAL      vinDTA = depvinDTA
097300030715     C                   ELSE
097400030715     C                   EVAL      wGiro = 2
097500031204     C**********
097600031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
097700031204     C* d separatore campo
097800031204     C**********
097900040709     C                   EVAL      vindta = %trim(vindta)
098000040709     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
098100040709     C                   EVAL      vindta = %subst(vindta:2)
098200040709     C                   ENDDO
098300040709     C*
098400040709     C                   Z-ADD     *zeros        lunghInput        4 0
098500040709     C                   EVAL      lunghInput = %len(%trim(vindta))
098600040709     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
098700040709     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
098800040709     C                   EVAL      lunghInput = %len(%trim(vindta))
098900040709     C                   ENDDO
099000030715     C**********
099100030715     C* Effettuo lo split del campo dati d input
099200030715     C**********
099300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
099400040324     C                   DOW       posDa <= %len(%trimr(vindta))
099500030715     C*
099600030715     C* Gestisco il 1° campo
099700030715     C                   IF        i = 1
099800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
099900030715     C                   EVAL      posDa = 1
100000030715     C* Eseguo lo scan x trovare la fine del primo campo
100100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
100200030715     C* A questo punto "estraggo" il campo corrente
100300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
100400030715     C                                                    (posA-posDa))
100500030715     C* X i campi successivi al 1°
100600030715     C                   ELSE
100700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
100800030715     C                   EVAL      posDa = posA + 1
100900031204     C* Verifico che nn vi sia il campo nn valorizzato
101000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
101100031204     C* Se campo nn valorizzato skippo al prossimo
101200031204     C                   EVAL      SkSplitCSV(i) = *blanks
101300031204     C                   EVAL      posA  = posA + 1
101400031204     C                   ELSE
101500030715     C* Eseguo lo scan x trovare la fine del campo corrente
101600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
101700030715     C* Gestisco l'ultimo campo
101800030715     C                   IF        posA = *zeros
101900040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
102000030715     C* A questo punto "estraggo" il campo corrente
102100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
102200030715     C                                                    (posA-posDa))
102300030715     C                   LEAVE
102400030715     C                   ELSE
102500030715     C* A questo punto "estraggo" il campo corrente
102600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
102700030715     C                                                    (posA-posDa))
102800030715     C                   ENDIF
102900031204     C                   ENDIF
103000030715     C                   ENDIF
103100030715     C* Incremento il contatore d campo
103200030715     C                   EVAL      i = i +1
103300030715     C                   ENDDO
103400030715     C*
103500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
103600030715     C* delimitazione testo
103700030715     C                   EVAL      i = 1
103800030715     C                   DOW       i < %elem(SkSplitCSV)
103900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
104000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
104100030715     C                   EVAL      i = i + 1
104200030715     C                   ENDDO
104300030715     C*
104400030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
104500030715     C                   EVAL      i = 1
104600030715     C                   DOW       i < %elem(SkSplitFLD)
104700030715     C*
104800030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
104900030715     C                   IF        SkSplitFLD(i) <> *blanks AND
105000030715     C                             SkSplitCSV(i) <> *blanks
105100030721     C*** ATB
105200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATB'
105300040708     C                   EVAL      VAOATB = SkSplitCSV(i)
105400030721     C                   ENDIF
105500040708     C*** TOR
105600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTOR'
105700040708     C                   EVAL      VAOTOR = SkSplitCSV(i)
105800040708     C                   ENDIF
105900040708     C*** TCO
106000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTCO'
106100040708     C                   EVAL      VAOTCO = SkSplitCSV(i)
106200040708     C                   ENDIF
106300040708     C*** RSO
106400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSO'
106500040708     C                   EVAL      VAORSO = SkSplitCSV(i)
106600030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
106700040708     C     '@':'A'       XLATE     VAORSO        VAORSO
106800030715     C* ==
106900030715     C                   ENDIF
107000040708     C*** INO
107100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINO'
107200040708     C                   EVAL      VAOINO = SkSplitCSV(i)
107300030715     C                   ENDIF
107400040708     C*** LOO
107500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOO'
107600040708     C                   EVAL      VAOLOO = SkSplitCSV(i)
107700030715     C                   ENDIF
107800040708     C*** PRO
107900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRO'
108000040708     C                   EVAL      VAOPRO = SkSplitCSV(i)
108100030715     C                   ENDIF
108200040708     C*** NAO
108300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAO'
108400040708     C                   EVAL      VAONAO = SkSplitCSV(i)
108500030721     C                   ENDIF
108600040708     C*** RSR
108700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSR'
108800040708     C                   EVAL      VAORSR = SkSplitCSV(i)
108900040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
109000040708     C     '@':'A'       XLATE     VAORSR        VAORSR
109100040708     C* ==
109200040708     C                   IF        VAORSR = *blanks
109300040708     C                   SETON                                        31
109400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
109500040708     C                             + ' ' + 'VAORSR'
109600040708     C                   ENDIF
109700040708     C                   ENDIF
109800040708     C*** INR
109900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINR'
110000040708     C                   EVAL      VAOINR = SkSplitCSV(i)
110100040708     C                   IF        VAOINR = *blanks
110200040708     C                   SETON                                        31
110300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
110400040708     C                             + ' ' + 'VAOINR'
110500040708     C                   ENDIF
110600040708     C                   ENDIF
110700040708     C*** LOR
110800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOR'
110900040708     C                   EVAL      VAOLOR = SkSplitCSV(i)
111000040708     C                   IF        VAOLOR = *blanks
111100040708     C                   SETON                                        31
111200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
111300040708     C                             + ' ' + 'VAOLOR'
111400040708     C                   ENDIF
111500040708     C                   ENDIF
111600040708     C*** PRR
111700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRR'
111800040708     C                   EVAL      VAOPRR = SkSplitCSV(i)
111900040708     C                   IF        VAOPRR = *blanks
112000040708     C                   SETON                                        31
112100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
112200040708     C                             + ' ' + 'VAOPRR'
112300040708     C                   ENDIF
112400040708     C                   ENDIF
112500040708     C*** NAR
112600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAR'
112700040708     C                   EVAL      VAONAR = SkSplitCSV(i)
112800040708     C                   ENDIF
112900040708     C*** RER
113000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORER'
113100040708     C                   EVAL      VAORER = SkSplitCSV(i)
113200040708     C                   ENDIF
113300040708     C*** TER
113400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTER'
113500040708     C                   EVAL      VAOTER = SkSplitCSV(i)
113600040708     C                   ENDIF
113700150923     C*** EMLR
113800150923     C                   IF        %trim(SkSplitFLD(i)) = 'VAOEMLR'
113900150923     C                   EVAL      w_VAOEMLR = SkSplitCSV(i)
114000150923     C                   ENDIF
114100040708     C*** RSC
114200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSC'
114300040708     C                   EVAL      VAORSC = SkSplitCSV(i)
114400040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
114500040708     C     '@':'A'       XLATE     VAORSC        VAORSC
114600040708     C* ==
114700040708     C                   ENDIF
114800040708     C*** INC
114900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINC'
115000040708     C                   EVAL      VAOINC = SkSplitCSV(i)
115100040708     C                   ENDIF
115200040708     C*** LOC
115300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOC'
115400040708     C                   EVAL      VAOLOC = SkSplitCSV(i)
115500040708     C                   ENDIF
115600040708     C*** PRC
115700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRC'
115800040708     C                   EVAL      VAOPRC = SkSplitCSV(i)
115900040708     C                   ENDIF
116000040708     C*** NAC
116100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAC'
116200040708     C                   EVAL      VAONAC = SkSplitCSV(i)
116300040708     C                   ENDIF
116400040708     C*** FFD
116500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFFD'
116600040708     C                   EVAL      VAOFFD = SkSplitCSV(i)
116700040708     C                   ENDIF
116800040708     C*** RMP
116900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORMP'
117000040708     C                   EVAL      VAORMP = SkSplitCSV(i)
117100040708     C                   ENDIF
117200040708     C*** NAM
117300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAM'
117400040708     C                   EVAL      VAONAM = SkSplitCSV(i)
117500040708     C                   ENDIF
117600040708     C*** PAG
117700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPAG'
117800040708     C                   EVAL      VAOPAG = SkSplitCSV(i)
117900040708     C                   ENDIF
118000040708     C*** CTR
118100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCTR'
118200040708     C                   EVAL      VAOCTR = SkSplitCSV(i)
118300040708     C                   ENDIF
118400040708     C*** ZOR
118500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOZOR'
118600040708     C                   EVAL      VAOZOR = SkSplitCSV(i)
118700040708     C                   ENDIF
118800040708     C*** NO1
118900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO1'
119000040708     C                   EVAL      VAONO1 = SkSplitCSV(i)
119100030715     C                   ENDIF
119200040708     C*** NO2
119300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO2'
119400040708     C                   EVAL      VAONO2 = SkSplitCSV(i)
119500030715     C                   ENDIF
119600040708     C*** DDT
119700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODDT'
119800040708     C                   EVAL      VAODDT = SkSplitCSV(i)
119900030715     C                   ENDIF
120000040708     C*** RFA
120100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORFA'
120200040708     C                   EVAL      VAORFA = SkSplitCSV(i)
120300030715     C                   ENDIF
120400040708     C*** SPI
120500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSPI'
120600040708     C                   EVAL      VAOSPI = SkSplitCSV(i)
120700030715     C                   ENDIF
120800040708     C*** FLO
120900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
121000040708     C                   EVAL      VAOFLO = SkSplitCSV(i)
121100040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
121200080708     C* e la posizione 7 del VAOFLO
121300040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
121400040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
121500050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
121600030715     C                   ENDIF
121700091103     C
121800091103     C*** DAR
121900050314     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
122000050412     C                   EVAL      PiStr=SkSplitCSV(i)
122100050412     C                   EXSR      CHKNUM
122200050412     C                   IF        PiInt=*on
122300050412     C                   Z-ADD     PiVal         VAODAR
122400050412     C                   ELSE
122500050412     C                   SETON                                        32
122600050412     C                   EVAL      VAODAR = *zeros
122700050412     C                   EVAL      vinmsg = %trimr(vinmsg)
122800050412     C                             + ' ' + 'VAODAR'
122900050412     C                   ENDIF
123000050314     C                   ENDIF
123100030715     C
123200030721     C*
123300030721     C* Reperisco quindi i campi numerici...
123400040708     C*** POE
123500040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
123600040723     C                   EVAL      PiStr=SkSplitCSV(i)
123700040723     C                   EXSR      CHKNUM
123800040723     C                   IF        PiInt=*on    AND
123900040723     C                             PiVal<=999   AND
124000040723     C                             PiVal>*zeros
124100040723     C                   Z-ADD     PiVal         VAOPOE
124200040723     C                   ELSE
124300040723     C                   SETON                                        32
124400040723     C                   EVAL      VAOPOE = *zeros
124500040723     C                   EVAL      vinmsg = %trimr(vinmsg)
124600040723     C                             + ' ' + 'VAOPOE'
124700040723     C                   ENDIF
124800040723     C                   ENDIF
124900040708     C*** COR
125000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
125100040708     C                   EVAL      PiStr=SkSplitCSV(i)
125200040708     C                   EXSR      CHKNUM
125300040708     C                   IF        PiInt=*on
125400040708     C                   Z-ADD     PiVal         VAOCOR
125500040708     C                   ELSE
125600040708     C                   SETON                                        31
125700040708     C                   EVAL      VAOCOR = *zeros
125800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
125900040708     C                             + ' ' + 'VAOCOR'
126000040708     C                   ENDIF
126100040708     C                   ENDIF
126200040708     C*** CAO
126300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
126400030721     C                   EVAL      PiStr=SkSplitCSV(i)
126500030721     C                   EXSR      CHKNUM
126600030721     C                   IF        PiInt=*on
126700030721     C                   Z-ADD     PiVal         Num5_0
126800040708     C                   MOVEL(p)  Num5_0        VAOCAO
126900030721     C                   ELSE
127000030721     C                   SETON                                        32
127100040708     C                   EVAL      VAOCAO = *zeros
127200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127300040708     C                             + ' ' + 'VAOCAO'
127400030721     C                   ENDIF
127500030721     C                   ENDIF
127600040708     C*** CRA
127700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
127800030721     C                   EVAL      PiStr=SkSplitCSV(i)
127900030721     C                   EXSR      CHKNUM
128000030721     C                   IF        PiInt=*on
128100040708     C                   Z-ADD     PiVal         VAOCRA
128200030721     C                   ELSE
128300030721     C                   SETON                                        32
128400040708     C                   EVAL      VAOCRA = *zeros
128500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128600040708     C                             + ' ' + 'VAOCRA'
128700030721     C                   ENDIF
128800030721     C                   ENDIF
128900040708     C*** CAR
129000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
129100040708     C                   EVAL      PiStr=SkSplitCSV(i)
129200040708     C                   EXSR      CHKNUM
129300040708     C                   IF        PiInt=*on
129400040708     C                   Z-ADD     PiVal         Num5_0
129500040708     C                   MOVEL(p)  Num5_0        VAOCAR
129600040708     C                   ELSE
129700040708     C                   SETON                                        31
129800040708     C                   EVAL      VAOCAR = *zeros
129900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
130000040708     C                             + ' ' + 'VAOCAR'
130100040708     C                   ENDIF
130200040708     C                   ENDIF
130300040708     C*** CRC
130400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
130500040708     C                   EVAL      PiStr=SkSplitCSV(i)
130600040708     C                   EXSR      CHKNUM
130700040708     C                   IF        PiInt=*on
130800040708     C                   Z-ADD     PiVal         VAOCRC
130900040708     C                   ELSE
131000040708     C                   SETON                                        32
131100040708     C                   EVAL      VAOCRC = *zeros
131200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
131300040708     C                             + ' ' + 'VAOCRC'
131400040708     C                   ENDIF
131500040708     C                   ENDIF
131600040708     C*** CAC
131700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
131800040708     C                   EVAL      PiStr=SkSplitCSV(i)
131900040708     C                   EXSR      CHKNUM
132000040708     C                   IF        PiInt=*on
132100040708     C                   Z-ADD     PiVal         Num5_0
132200040708     C                   MOVEL(p)  Num5_0        VAOCAC
132300040708     C                   ELSE
132400040708     C                   SETON                                        32
132500040708     C                   EVAL      VAOCAC = *zeros
132600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
132700040708     C                             + ' ' + 'VAOCAC'
132800040708     C                   ENDIF
132900040708     C                   ENDIF
133000040708     C*** DAR
133100090428     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
133200090428     C                   EVAL      PiStr=SkSplitCSV(i)
133300090428     C                   EXSR      CHKNUM
133400090428     C                   IF        PiInt=*on
133500090428     C                   Z-ADD     PiVal         VAODAR
133600090428     C                   ELSE
133700090428     C                   SETON                                        32
133800090428     C                   EVAL      VAODAR = *zeros
133900090428     C                   EVAL      vinmsg = %trimr(vinmsg)
134000090428     C                             + ' ' + 'VAODAR'
134100090428     C                   ENDIF
134200090428     C                   ENDIF
134300040708     C*** ORR
134400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
134500040708     C                   EVAL      PiStr=SkSplitCSV(i)
134600040708     C                   EXSR      CHKNUM
134700040708     C                   IF        PiInt=*on
134800040708     C                   Z-ADD     PiVal         VAOORR
134900040708     C                   ELSE
135000040708     C                   SETON                                        32
135100040708     C                   EVAL      VAOORR = *zeros
135200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
135300040708     C                             + ' ' + 'VAOORR'
135400040708     C                   ENDIF
135500040708     C                   ENDIF
135600030721     C*** NCL
135700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
135800030721     C                   EVAL      PiStr=SkSplitCSV(i)
135900030721     C                   EXSR      CHKNUM
136000030721     C                   IF        PiInt=*on
136100040723     C                   IF        PiVal > *zeros
136200040708     C                   Z-ADD     PiVal         VAONCL
136300040723     C                   ENDIF
136400030721     C                   ELSE
136500030721     C                   SETON                                        32
136600040708     C                   EVAL      VAONCL = *zeros
136700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136800040708     C                             + ' ' + 'VAONCL'
136900030721     C                   ENDIF
137000030721     C                   ENDIF
137100040708     C*** PKG
137200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
137300030721     C                   EVAL      PiStr=SkSplitCSV(i)
137400030721     C                   EXSR      CHKNUM
137500030721     C                   IF        PiNum=*on
137600040723     C                   IF        PiVal > *zeros
137700040708     C                   Z-ADD     PiVal         VAOPKG
137800040723     C                   ENDIF
137900030721     C                   ELSE
138000030721     C                   SETON                                        32
138100040708     C                   EVAL      VAOPKG = *zeros
138200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138300040708     C                             + ' ' + 'VAOPKG'
138400030721     C                   ENDIF
138500030721     C                   ENDIF
138600040708     C*** VLM
138700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
138800040708     C                   EVAL      PiStr=SkSplitCSV(i)
138900040708     C                   EXSR      CHKNUM
139000040708     C                   IF        PiNum=*on
139100040708     C                   Z-ADD     PiVal         VAOVLM
139200040708     C                   ELSE
139300040708     C                   SETON                                        32
139400040708     C                   EVAL      VAOVLM = *zeros
139500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
139600040708     C                             + ' ' + 'VAOVLM'
139700040708     C                   ENDIF
139800040708     C                   ENDIF
139900040708     C*** BNC
140000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
140100040708     C                   EVAL      PiStr=SkSplitCSV(i)
140200040708     C                   EXSR      CHKNUM
140300040708     C                   IF        PiInt=*on
140400040708     C                   Z-ADD     PiVal         VAOBNC
140500040708     C                   ELSE
140600040708     C                   SETON                                        32
140700040708     C                   EVAL      VAOBNC = *zeros
140800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
140900040708     C                             + ' ' + 'VAOBNC'
141000040708     C                   ENDIF
141100040708     C                   ENDIF
141200040708     C*** BLC
141300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
141400040708     C                   EVAL      PiStr=SkSplitCSV(i)
141500040708     C                   EXSR      CHKNUM
141600040708     C                   IF        PiInt=*on
141700040708     C                   Z-ADD     PiVal         VAOBLC
141800040708     C                   ELSE
141900040708     C                   SETON                                        32
142000040708     C                   EVAL      VAOBLC = *zeros
142100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
142200040708     C                             + ' ' + 'VAOBLC'
142300040708     C                   ENDIF
142400040708     C                   ENDIF
142500040708     C*** ATT
142600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
142700040708     C                   EVAL      PiStr=SkSplitCSV(i)
142800040708     C                   EXSR      CHKNUM
142900040708     C                   IF        PiInt=*on
143000040708     C                   Z-ADD     PiVal         VAOATT
143100040708     C                   ELSE
143200040708     C                   SETON                                        32
143300040708     C                   EVAL      VAOATT = *zeros
143400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
143500040708     C                             + ' ' + 'VAOATT'
143600040708     C                   ENDIF
143700040708     C                   ENDIF
143800040708     C*** MTC
143900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
144000040708     C                   EVAL      PiStr=SkSplitCSV(i)
144100040708     C                   EXSR      CHKNUM
144200040708     C                   IF        PiInt=*on
144300040708     C                   Z-ADD     PiVal         VAOMTC
144400040708     C                   ELSE
144500040708     C                   SETON                                        32
144600040708     C                   EVAL      VAOMTC = *zeros
144700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
144800040708     C                             + ' ' + 'VAOMTC'
144900040708     C                   ENDIF
145000040708     C                   ENDIF
145100040708     C*** KSC
145200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
145300030721     C                   EVAL      PiStr=SkSplitCSV(i)
145400030721     C                   EXSR      CHKNUM
145500030721     C                   IF        PiInt=*on      AND
145600030721     C                             PiVal<=9999999 AND
145700030721     C                             PiVal>*zeros
145800040708     C                   Z-ADD     PiVal         VAOKSC
145900030721     C                   ELSE
146000030721     C                   SETON                                        32
146100040708     C                   EVAL      VAOKSC = *zeros
146200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
146300040708     C                             + ' ' + 'VAOKSC'
146400030721     C                   ENDIF
146500030721     C                   ENDIF
146600040708     C*** POR
146700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
146800030721     C                   EVAL      PiStr=SkSplitCSV(i)
146900030721     C                   EXSR      CHKNUM
147000030721     C                   IF        PiInt=*on    AND
147100030721     C                             PiVal<=999   AND
147200030721     C                             PiVal>*zeros
147300040708     C                   Z-ADD     PiVal         VAOPOR
147400030721     C                   ELSE
147500030721     C                   SETON                                        32
147600040708     C                   EVAL      VAOPOR = *zeros
147700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
147800040708     C                             + ' ' + 'VAOPOR'
147900030721     C                   ENDIF
148000030721     C                   ENDIF
148100040708     C*** POC
148200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
148300040708     C                   EVAL      PiStr=SkSplitCSV(i)
148400040708     C                   EXSR      CHKNUM
148500040708     C                   IF        PiInt=*on    AND
148600040708     C                             PiVal<=999   AND
148700040708     C                             PiVal>*zeros
148800040708     C                   Z-ADD     PiVal         VAOPOC
148900040708     C                   ELSE
149000040708     C                   SETON                                        32
149100040708     C                   EVAL      VAOPOC = *zeros
149200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
149300040708     C                             + ' ' + 'VAOPOC'
149400040708     C                   ENDIF
149500040708     C                   ENDIF
149600040708     C*** STO
149700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
149800040708     C                   EVAL      PiStr=SkSplitCSV(i)
149900040708     C                   EXSR      CHKNUM
150000040708     C                   IF        PiInt=*on
150100040708     C                   Z-ADD     PiVal         VAOSTO
150200040708     C                   ELSE
150300040708     C                   SETON                                        32
150400040708     C                   EVAL      VAOSTO = *zeros
150500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
150600040708     C                             + ' ' + 'VAOSTO'
150700040708     C                   ENDIF
150800040708     C                   ENDIF
150900090428     C*
151000091103     C********** GESTIONE CAMPI PERSONALIZZATI
151100150923
151200090428     C*
151300090428     C****************************************
151400090428     C*
151500030715     C*
151600030715     C                   ENDIF
151700030715     C                   EVAL      i = i + 1
151800030715     C                   ENDDO
151900040723     C*
152000121207     C***  ===>  Gestione campi "particolari" fuori ciclo
152100121207     C*
152200140108
152300140108     C*
152400140108     C* se VAORER non è valorizzato copiare il valore inserito nel campo VAORSR
152500140108     C                   IF        VAORER = *blank
152600140108     C                   EVAL      VAORER = VAORSR
152700140108     C                   ENDIF
152800140108     C*
152900130513
153000030715     C                   ENDIF
153100020204     C*
153200010607     C                   ENDSR
153300010607     C*----------------------------------------------------*
153400080702
153500080702
153600080702     C*----------------------------------------------------*
153700080702     C*  CONTROLLO NUMERICITA' CAMPI
153800080702     C*----------------------------------------------------*
153900080702     C     CHKNUM        BEGSR
154000080702     C*
154100080702     C                   IF        PiDecChr = *blanks
154200080702     C                   EVAL      PiDecChr = ','
154300080702     C                   ENDIF
154400080702     C*
154500080702     C                   callp     UBISNUM_Check(PiStr
154600080702     C                                          :PiDecChr
154700080702     C                                          :PiVal
154800080702     C                                          :PiNum
154900080702     C                                          :PiInt)
155000080702     C*
155100080702     C                   ENDSR
155200080702     C***
155300080702
155400010601
155500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
155600010601     C     repfil        BEGSR
155700010601     C*
155800010601     C                   if        invfil = *zeros and
155900010601     C                             depfil > *zeros and
156000010629     C                             (vinflg = *blanks or
156100010629     C                              vinflg = *zeros)
156200010601     C
156300010601     C                   eval      invfil = depfil
156400010601     C                   endif
156500010601     C*
156600010601     C                   if        depfil <> invfil and
156700010601     C                             invfil > *zeros
156800010601     C                   eval      flgMulti = '1'
156900010601     C                   if        vinflg = *blanks
157000010601     C                   add       1             cntNonEl
157100010601     C                   endif
157200010601     C                   endif
157300010601     C*
157400010601     C                   if        vinflg = '2'
157500010601     C                   eval      flgStato = '2'
157600010601     C                   endif
157700010601     C*
157800010601     C                   ENDSR
157900010601     C***
158000010330
158100010330
158200990920      /TITLE Invio dei dati al punto operativo.
158300000613     C     invio         BEGSR
158400990920     C*
158500990920     C                   reset                   dscmz
158600010601     C                   move      invfil        cmzdst
158700990920     C                   eval      cmzfld = vlrfou
158800990920     C                   eval      cmzmbd = vlrhdl
158900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
159000000710     C                   if        prmfir = *blanks
159100040708     C                   eval      cmzfla = 'FNVAO00F'
159200040708     C                   eval      cmzmba = 'FNVAO00F'
159300000710     C                   else
159400000710     C                   eval      cmzfla = prmfir
159500000710     C                   eval      cmzmba = prmfir
159600000710     C                   endif
159700990920     C                   eval      cmznrr = *zeros
159800990920     C                   move      §ctrok        cmznrr
159900021018     C                   eval      cmzlba = vlrfl1
160000990920     C                   call(e)   'TIS711C'
160100990920     C                   parm                    dscmz
160200990921     C                   parm      *blanks       esito
160300990920     C                   if        %error
160400990920     C                             or cmzerr = '1'
160500990921     C                             or esito  = '1'
160600000710     C                   eval      wrkesito = '3'
160700990920     C                   endif
160800990920     C*
160900000613     C                   ENDSR
161000010601
161100010601
161200010601      /TITLE Invio dei dati al punto operativo.
161300010601     C     opeini        BEGSR
161400010601     C*
161500010601     C* Inizializzo flag e contatori operativi
161600010601     C                   movel     '0'           flgGiro           1
161700010601     C                   movel     '0'           flgMulti          1
161800010601     C                   movel     '1'           flgStato          1
161900010615     C                   movel     '0'           flgOk             1
162000010601     C                   z-add     *zeros        cntNonEl         10 0
162100010601     C                   z-add     *zeros        depfil            3 0
162200010601     C                   z-add     *zeros        invfil            3 0
162300010601     C*
162400010601     C                   ENDSR
162500010601     C***
162600050421
162700050421
162800091130     C     *pssr         BEGSR
162900050421     C*
163000091130     C                   eval      wrkesito = '2'
163100050421     C*
163200091130     C                   ENDSR     '*CANCL'
163300050421     C***
163400060306
163500060306
163600060306     C*-------------------------------------------------------------------------*
163700060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
163800060306     C*-------------------------------------------------------------------------*
163900060306     C     sr_Control    begsr
164000060306     C*
164100060306     C* inizializzo a ok il flag d controllo abilitazione
164200060306     C                   eval      Cli_Abil = *on
164300060306     C
164400060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
164500060306     C                   clear                   woriIDC
164600060306     C                   eval      kteCOD = 'VAP'
164700060306     C                   eval      kteKE1 = oriIDC
164800060306     C     keytbe        chain     TNTBE01L
164900060306     C                   if        %found(Tntbe01l)
165000060306     C                   eval      woriIDC = TbeKe2
165100060306     C                   else
165200060306     C                   eval      Cli_Abil = *off
165300060306     C                   endif
165400060306     C*
165500060306     C                   endsr
165600010330
165700010330
165800000613     C     *inzsr        BEGSR
165900990910     C*
166000990910     C     *entry        plist
166100990920     C                   parm                    tivlrds
166200990921     C                   parm      wrkesito      esito
166300000724     C                   parm                    prmlit
166400000710     C                   parm                    prmfir
166500010330     C*
166600010330     C* CALCOLA LA DATA CORRENTE
166700091127     C                   z-add     *zeros        datcor            8 0
166800091127     C                   time                    wn14             14 0
166900091127     C                   movel     wn14          wn6               6 0          *ORA (6)
167000130516     C                   eval      datcor = %dec(%date() : *ISO)
167100040723     C*
167200040723     C* Campi riferito al database
167300040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
167400040723     C     *like         define    tbeke1        kteKE1
167500040723     C*
167600040723     C* Definizione chiavi di lettura
167700040723     C* TNTBE01L - parziale
167800040723     C     keytbe        klist
167900040723     C                   kfld                    kteCOD                         *UTENTE
168000040723     C                   kfld                    kteKE1                         *CODICE
168100000613     C*
168200000613     C                   ENDSR
168300000613     C***
168400990908
168500040709** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
168600151111X;VAOPOE;X;X;X;VAOTOR;X;VAODAO;X;X;X;X;X;X;X;X;X;VAORSR;VAOINR;VAOCAR;VAOLOR;VAOPRR;X;X;
168700151111VAOTER;X;X;X;X;X;X;X;X;VAOORR;X;VAONAM;VAONCL;VAOPKG;X;X;X;X;X;X;X;X;X;X;X;X;X;X;VAORFA;
168800130222
