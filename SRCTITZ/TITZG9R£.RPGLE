000100040708      /TITLE Upload via Internet: traduzione in FNVAOWWR.
000200121129      *
000300121129      * PARTICOLARITA':
000400121129      *
000500121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000600120209     F*
000700040723     Ffnacr01l  if   e           k disk
000800090618     Ftntbe01l  if   e           k disk
000900990910     Ftivin00r  uF   E             DISK    usropn
001000040708     FFnvaowwr  O    E             DISK    usropn
001100040723     Ftiori00f  O    E             DISK
001200050826     FTIVGD00F  O    E             DISK
001300000313     D*
001400040709     D*------------
001500040709     D* SCHIERE A PROGRAMMA
001600040709     D*------------
001700040709     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
001800010330     D*----------------------------------------------------
001900010330     D* DICHIARAZIOINE VARIABILI DI WRK
002000010330     D*----------------------------------------------------
002100010330     D dscmz         e ds                  inz
002200010330     D psds           sds
002300010330     D  procname         *PROC
002400010330     D tivlrds       e ds                  extname(tivlr00f)
002500050826     D fnvapds       e ds                  extname(fnvap00f)
002600151117     D tisi95ds      e ds
002700040723     D dorm01        e ds
002800040723     D dfar          e ds
002900090618     d trul33ds      e ds
003000090618     d kpjba         e ds
003100150923     D fnvaoear      e ds                  inz
003200150923     D                                     prefix(w_)
003300010330     D esito           s              1
003400010330     D prmlit          s             10
003500010330     D prmfir          s             10
003600010330     D wrkesito        s                   like(esito)
003700010330     D rrnum           s              6  0 INZ(*zeros)
003800040709     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
003900060306     D Cli_Abil        s              1    inz(*off)
004000060306     D woriIDC         s                   like(oriIDC) inz(*blanks)
004100090618     d §numori         s              7  0 inz
004200120123     d wData_EUR       s               d   datfmt(*eur)
004300120123     d wData_ISO       s               d   datfmt(*iso)
004400120905     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
004500120905     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
004600130513     D Intestazioni    s               n   inz(*on)
004700030715     D*------------------
004800030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004900030715     D*------------------
005000030715     D SkSplitFLD      S             10    DIM(100)
005100030715     D SkSplitCSV      S            256    DIM(100)
005200030715     D CharCSV         S              1
005300030715     D CharTXT         S              1
005400030715     D CharNUM         S              1
005500030715     D posDa           S              3  0 INZ(*zeros)
005600030715     D posA            S              3  0 INZ(*zeros)
005700030715     D i               s              3  0 INZ(1)
005800030715     D wGiro           s              1  0 INZ(*zeros)
005900030822     D*------------------
006000030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006100030822     D*------------------
006200030822     D posDaDft        S              3  0 INZ(*zeros)
006300030822     D posADft         S              3  0 INZ(*zeros)
006400030822     D j               s              3  0 INZ(1)
006500030822     D wGiroDft        s              1  0 INZ(*zeros)
006600040723     D*------------------
006700040723     D* DS REPERIMENTO DATI CLIENTE
006800040723     D*-------------------
006900040723     D BS69DS        E DS                  EXTNAME(TIBS69DS)
007000040723     D ACODS         E DS                  EXTNAME(CNACO00F)
007100040723     D INDDS         E DS                  EXTNAME(CNIND00F)
007200040723     D CLPDS         E DS                  EXTNAME(CNCLP00F)
007300040723     D CLSDS         E DS                  EXTNAME(FNCLS00F)
007400121129     D*------------------
007500150923     D w70             s             70
007600080702
007700080702     D*------------------
007800080702     D* LINKING A DEFINIZIONI ESTERNE
007900080702     D*------------------
008000090428     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
008100090428     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008200091026
008300091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
008400091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
008500091026
008600080702
008700031201     D*------------------
008800031201     D* Costanti
008900031201     D*------------------
009000031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009100031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009200010330
009300010330
009400010330
009500000913     C                   reset                   rrnum
009600990921     C                   reset                   esito
009700990921     C                   reset                   wrkesito
009800010601     C*
009900010601     C                   exsr      opeini
010000041011     C                   exsr      PREELA
010100040708     C                   exsr      rwvao
010200151117     C*
010300151117     C* Esegue lancio TISI95R solo x chiusura
010400151117     C                   CLEAR                   TISI95DS
010500151117     C                   EVAL      I95TLA = 'C'
010600151117     C                   CALL      'TISI95R'
010700151117     C                   PARM                    TISI95DS
010800010601     C*
010900010601     C                   seton                                        lr
011000010601
011100010601
011200010601
011300010601
011400010601     C*--------------------------------------------------------
011500010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011600010601     C*--------------------------------------------------------
011700010601     C     PREELA        BEGSR
011800010601     C*
011900010601     C* SE OCCORRE SPEDIRE IN FILIALE
012000041011     C                   if        flgGiro = '0'
012100010601     C*
012200010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012300010601     C                   eval      flgGiro = '1'
012400041011     C*
012500041011     C                   endif
012600010601     C*
012700010601     C                   ENDSR
012800010601     C***
012900010601
013000010601
013100010601     C*--------------------------------------------------------
013200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013300010601     C*--------------------------------------------------------
013400010601     C     ENDELA        BEGSR
013500000616     C*
013600120209     C                   ENDSR
013700010601
013800010601
013900010330     C*--------------------------------------------------------
014000040708     C* RWVAO   LEGGE TIVIN00R E SCRIVE FNVAOWWF              *
014100010330     C*--------------------------------------------------------
014200040708     C     RWVAO         BEGSR
014300010330     C*
014400010330     C                   if        not %open(tivin00r)
014500010330     C                   open      tivin00r
014600010330     C                   endif
014700040708     C                   if        not %open(fnvaowwr)
014800040708     C                   open      fnvaowwr
014900010330     C                   endif
015000150923     C*
015100150923     C* Mi predispongo per scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
015200150923     C                   call      'TIS7T7ER'
015300150923     C                   parm      'O'           pIn_Opz           1
015400150923     C                   parm                    tivlrds
015500150923     C                   parm                    FNVAOEAR
015600150923     C                   parm                    oriPRG
015700150923     C                   parm      *zeros        pIn_Dst           3
015800150923     C                   parm      *blanks       pOut_Esito        1
015900150923     C                   if        pOut_Esito = '2'
016000150923     C                   goto      FINE
016100150923     C                   endif
016200010330     C*
016300010604     C                   clear                   §CTROK
016400010604     C                   clear                   §CTRMO
016500010604     C                   clear                   §CTRNO
016600120123     C*
016700120905     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
016800120905     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
016900120905     C                   IF        Intestazioni = *off
017000120123     C* Eseguo primo giro x by-passare la mancanza della 1° riga con le intestazioni
017100120123     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
017200120123     C                   exsr      inzvar
017300120123     C                   exsr      impvao
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
020900010601     C*
021000010330     C                   exsr      inzvar
021100040708     C                   exsr      impvao
021200151019     C*
021300151019     C* Stacco subito un numeratore univoco "ORM"
021400151019     C                   exsr      RTVNUM
021500150923     C*
021600150923     C* Verifico se trattasi di formato ORM "esteso"
021700150923     C                   setoff                                       53
021800150923     C                   if        w_VAOEMLR  <> *blanks
021900150923     C                   seton                                        53
022000150923     C                   endif
022100010601     C*
022200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022300040708     C                   eval      depfil = VAOPOE
022400010601     C                   exsr      repfil
022500010601     C                   if        depfil = invfil
022600021025     C                   if        vlrpoi = 999
022700040708     C                   move(P)   invfil        VAOPOE
022800021025     C                   else
022900040708     C                   move(P)   vlrpoi        VAOPOE
023000021025     C                   endif
023100010601     C*
023200010604     C* Ebbene...
023300010604     C*
023400030822     C  N31              add       1             §CTROK            7 0
023500030822     C   32              add       1             §CTRMO            7 0
023600030822     C   31              add       1             §CTRNO            7 0
023700030822     C                   if        wGiro = 2
023800120123     C  N31              EXSR      CHKCOR
023900120123     C  N31              EXSR      WRIORI
024000120123     C  N31              EXSR      WRIVGD
024100120123     C  N31              write     FNVAO000
024200150923     C*
024300150923     C* Se richiesta scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
024400150923     C                   if        *in53 and not *in31
024500151019     C                   exsr      EXEFORZ
024600150923     C                   call      'TIS7T7ER'
024700150923     C                   parm      'W'           pIn_Opz           1
024800150923     C                   parm                    tivlrds
024900150923     C                   parm                    FNVAOEAR
025000150923     C                   parm                    oriPRG
025100150923     C                   parm      *zeros        pIn_Dst           3
025200150923     C                   parm      *blanks       pOut_Esito        1
025300150923     C                   endif
025400150923     C                   if        pOut_Esito = '2'
025500150923     C                   seton                                        32
025600150923     C                   endif
025700030822     C                   endif
025800010604     C*
025900010604     C                   if        *in31 = *off and
026000010604     C                             *in32 = *off
026100010604     C                   eval      vinflg = '1'
026200010604     C                   else
026300010604     C                   eval      vinflg = '2'
026400010604     C                   endif
026500010604     C                   endif
026600010601     C*
026700010604     C                   endif
026800010604     C*
026900010330     C                   else
027000010330     C                   eval      vinflg = '1'
027100010330     C                   endif
027200010601     C*
027300010601     C  N70              update    tivin000
027400010330     C*
027500030822     C  N70              enddo
027600150923     C*
027700150923     C     FINE          tag
027800150923     C*
027900150923     C* Chiusura per scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
028000150923     C                   call      'TIS7T7ER'
028100150923     C                   parm      'C'           pIn_Opz           1
028200150923     C                   parm                    tivlrds
028300150923     C                   parm                    FNVAOEAR
028400150923     C                   parm                    oriPRG
028500150923     C                   parm      *zeros        pIn_Dst           3
028600150923     C                   parm      *blanks       pOut_Esito        1
028700010601     C*
028800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
028900010601     C                   if        cntNonEl = *zeros or
029000010601     C                             flgMulti = '0'
029100010330     C* Se non ci sono record con errori ...
029200010601     C                   if        §ctrno = 0 and
029300010604     C                             §ctrmo = 0 and
029400010601     C                             flgStato <> '2'
029500010330     C* ... restituisco esito OK.
029600010330     C                   eval      wrkesito = '0'
029700010330     C                   else
029800010330     C                   if        §ctrok > 0
029900010330     C                   eval      wrkesito = '1'
030000010330     C                   else
030100010615     C                   if        flgOk = '0'
030200010615     C                   eval      wrkesito = '2'
030300010615     C                   else
030400010615     C                   eval      wrkesito = '6'
030500010615     C                   endif
030600010330     C                   endif
030700010330     C                   endif
030800010601     C                   else
030900010601     C                   eval      wrkesito = '9'
031000010601     C                   endif
031100010330     C*
031200010330     C                   if        %open(tivin00r)
031300010330     C                   close     tivin00r
031400010330     C                   endif
031500040708     C                   if        %open(fnvaowwr)
031600040708     C                   close     fnvaowwr
031700010330     C                   endif
031800010601     C*
031900010601     C                   if        vlrpoi <> 999
032000010601     C                   eval      invfil = vlrpoi
032100010601     C                   endif
032200010330     C*
032300010330     C                   if        §ctrok > 0
032400010601     C                             and invfil > *zeros
032500010330     C                   exsr      invio
032600150923     C*
032700150923     C* Invio dati estensione ORM (FNVAOE0F) => richiamo l'apposito driver
032800150923     C                   move      invfil        cmzdst
032900150923     C                   call      'TIS7T7ER'
033000150923     C                   parm      'S'           pIn_Opz           1
033100150923     C                   parm                    tivlrds
033200150923     C                   parm                    FNVAOEAR
033300150923     C                   parm                    oriPRG
033400150923     C                   parm      cmzdst        pIn_Dst           3
033500150923     C                   parm      *blanks       pOut_Esito        1
033600010330     C                   endif
033700010601     C*
033800010618     C                   if        flgGiro = '1'
033900010601     C                   exsr      endela
034000010618     C                   endif
034100010330     C*
034200010330     C                   ENDSR
034300010330     C***
034400040723
034500151019
034600151019
034700151019     C*------------------------------------------------------------------------*
034800151019     C* RTVNUM - Stacco un numeratore univoco "ORM"
034900151019     C*------------------------------------------------------------------------*
035000151019     C     RTVNUM        BEGSR
035100151019     C*
035200151019     C* Stacco un numeratore da AZNUM
035300151019     C                   clear                   §numori
035400151019     C                   clear                   trul33ds
035500151019     C                   eval      I33OPE = *zeros
035600151019     C                   eval      I33CNU = 600
035700151019     C                   eval      I33NUM = 1
035800151019     C                   movel     TRUL33DS      KPJBU
035900151019     C                   call      'TRUL33R'
036000151019     C                   parm                    KPJBA
036100151019     C                   movel     KPJBU         TRUL33DS
036200151019     C                   if        O33ERR = *zeros
036300151019     C                   z-add     O33NRF        §numori
036400151019     c                   else
036500151019     c                   eval      §numori = *all'9'
036600151019     C                   seton                                        31        * errore bloccante
036700151019     C                   endif
036800151019     C*
036900151019     C                   eval      vaoNOR = §numori
037000151019     C*
037100151019     C                   ENDSR
037200151019
037300151019
037400151019     C*------------------------------------------------------------------------*
037500151019     C* EXEFORZ - Eseguo forzatire di base su formato ORM "esteso"
037600151019     C*------------------------------------------------------------------------*
037700151019     C     EXEFORZ       BEGSR
037800151019     C*
037900151019     C                   move(p)   VAOPOE        w_vaopoe                       * forzatura necessar
038000151019     C                   move(p)   VAOCOR        w_vaocor                       * forzatura necessar
038100151019     C                   move(p)   VAONOR        w_vaonor                       * forzatura necessar
038200151019     C*
038300151019     C                   ENDSR
038400040723
038500151019
038600040723     C*------------------------------------------------------------------------*
038700040723     C* CHKCOR - Considerazioni su VAOCOR
038800040723     C*------------------------------------------------------------------------*
038900040723     C     CHKCOR        BEGSR
039000040723     C*
039100040723     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
039200040723     C                   if        vaoTCO = 'F'
039300040723     C* Quindi verifico se il codice ordinante non è valorizzato
039400040723     C                   if        vaoCOR = *zeros  AND
039500040723     C                             vaoRSO = *blanks AND
039600040723     C                             vaoINO = *blanks AND
039700040723     C                             vaoCAO = *blanks AND
039800040723     C                             vaoLOO = *blanks AND
039900040723     C                             vaoPRO = *blanks AND
040000040723     C                             vaoNAO = *blanks
040100040723     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
040200040723     C                   movel     '0'           wFlgAcr           1
040300040723     C                   move      vlrKSC        wCli              7 0
040400040723     C                   move(p)   wCli          acrCRO
040500040723     C     acrCRO        setll     fnacr01l
040600040723     C                   if        %found(fnacr01l)
040700040723     C                   read      fnacr01l
040800040723     C                   dow       not %eof(fnacr01l)
040900040723     C                   movel     acrCRO        wCliAcr           7 0
041000040723     C                   if        wCliAcr = wCli
041100040723     C                   eval      vaoCOR = acrCRO
041200040723     C                   movel     '1'           wFlgAcr
041300040723     C                   leave
041400040723     C                   else
041500040723     C                   leave
041600040723     C                   endif
041700040723     C                   read      fnacr01l
041800040723     C                   enddo
041900040723     C                   endif
042000040723     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
042100040723     C* dati anagrafici dell'ordinante dal piano dei conti
042200040723     C                   if        wFlgAcr = '0'
042300040723     C                   clear                   BS69DS
042400040723     C                   clear                   ACODS
042500040723     C                   clear                   INDDS
042600040723     C                   clear                   CLPDS
042700040723     C                   clear                   CLSDS
042800040723     C                   move(P)   vlrKSC        I69KAC
042900040723     C                   move(P)   vlrKSC        I69KIN
043000040723     C                   call      'TIBS69R'
043100040723     C                   parm                    BS69DS
043200040723     C                   parm                    ACODS
043300040723     C                   parm                    INDDS
043400040723     C                   parm                    CLPDS
043500040723     C                   parm                    CLSDS
043600040723     C     O69ERR        ifne      '1'
043700040723     C                   eval      vaoRSO = ACORAG
043800040723     C                   eval      vaoINO = INDVIA
043900040723     C                   movel(P)  INDCAP        vaoCAO
044000040723     C                   eval      vaoLOO = INDCIT
044100040723     C                   eval      vaoPRO = INDPRV
044200040723     C                   eval      vaoNAO = INDSTA
044300040723     C                   endif
044400040723     C                   endif
044500040723     C                   endif
044600040723     C                   endif
044700040723     C*
044800040723     C                   ENDSR
044900040723
045000040723
045100040723     C*------------------------------------------------------------------------*
045200040723     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
045300040723     C*------------------------------------------------------------------------*
045400040723     C     WRIORI        BEGSR
045500040723     C*
045600090618     C* Come prima cosa stacco un numeratore da AZNUM
045700151019     C***                clear                   trul33ds
045800151019     C***                eval      I33OPE = *zeros
045900151019     C***                eval      I33CNU = 600
046000151019     C***                eval      I33NUM = 1
046100151019     C***                movel     TRUL33DS      KPJBU
046200151019     C***                call      'TRUL33R'
046300151019     C***                parm                    KPJBA
046400151019     C***                movel     KPJBU         TRUL33DS
046500151019     C***                if        O33ERR = *zeros
046600151019     C***                z-add     O33NRF        §numori
046700151019     c***                else
046800151019     c***                eval      §numori = *all'9'
046900151019     C***                endif
047000040723     C*
047100040723     C* Quindi imposto i campi "extra-VAO"
047200040723     C                   if        vlrKSC = '0ORM0000'
047300040723     C*
047400040723     C                   if        vaoCOR > *zeros
047500040723     C                   movel     vaoCOR        wrkCOR7           7 0
047600040723     C*
047700040723     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
047800040723     C* codice padre. Se è un padre assegno già il codice ordinante.
047900040723     C                   clear                   tibs10ds
048000040723     C                   eval      d10drf = datcor                              *data riferimento
048100040723     C                   eval      d10paf = 'P'                                 *cerca il padre
048200040723     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
048300091026     C*
048400091026     C* Preparazione chiamata al TIBS10R1
048500091026     C                   clear                   tibs10r1ds
048600091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
048700091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
048800091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
048900091026     C*
049000091026     C                   call      'TIBS10R1'
049100091026     C                   parm                    tibs10r1ds
049200091026     C*
049300091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
049400040723     C*
049500040723     C                   if        d10err <> *blanks or                         *No unif. o padre
049600040723     C                             d10cop =  *zeros  or
049700040723     C                             d10cop =  d10cod
049800040723     C                   move      d10cod        oriIDC
049900040723     C                   else                                                   *Figlio
050000040723     C                   move      d10cop        oriIDC
050100040723     C                   endif
050200040723     C*
050300040723     C                   else
050400040723     C                   eval      oriIDC = *zeros
050500040723     C                   endif
050600040723     C*
050700040723     C                   else
050800040723     C                   eval      oriIDC = vlrKSC
050900040723     C                   endif
051000040723     C*
051100040723     C                   eval      oriPRG = §numori
051200040723     C                   eval      oriDER = datcor
051300040723     C                   eval      oriOER = wn6
051400040723     C                   eval      oriPOE = vaoPOE
051500040723     C                   eval      oriNSR = vaoNSR
051600040723     C                   eval      oriNOR = vaoNOR
051700040723     C                   eval      oriNRV = vaoNRV
051800040723     C                   eval      oriFLO = *blanks
051900040723     C*
052000040723     C* Infine valorizzo la chiave esterna sul file FNVAO
052100150923     C* per ora VAOFLO è blank
052200150923     C                   eval      vaoflo = *blank
052300040723     C                   eval      dorm01 = vaoFLO
052400150923     C*                  movel     §numori       §ormpg
052500150923     C*                  EVAL      §ORMCB = 'N'
052600150923     C*                  EVAL      §ORMFR = 'S'
052700150923     C*                  EVAL      §ORMKS = 'N'
052800150923     C*                  EVAL      §ORMFD = 'S'
052900050412     C*
053000050412     C* Infine valorizzo i flag operativi necessari x la particolare gestione corrente
053100130326     C*                   in questo caso non ci sono default
053200050412     C*
053300130326     C* Riassegno il campo VAO contenete i flag operativi precedentemente ridefiniti
053400040723     C                   eval      vaoFLO = dorm01
053500130326     C* Infine, per sicurezza, pulisco alcuni flag che devono essere a blank
053600130326     C                   eval      %subst(VAOFLO:5:1) = *blanks
053700130326     C                   eval      %subst(VAOFLO:6:1) = *blanks
053800040723     C*
053900040723     C* ...quindi scrivo il file TIORI00F
054000120905     C  N55              WRITE     TIORI000
054100040723     C*
054200040723     C                   ENDSR
054300050826
054400050826
054500050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
054600050826     C     wriVGD        BEGSR
054700050826     C*
054800050826     C* Reperisco la descrizione della fase da tabella
054900050826     C                   movel(p)  'FAR'         KteCOD
055000050826     C                   movel(p)  '000'         KteKE1
055100050826     C     KEYtbe        chain     tntbe01l
055200050826     C                   if        %found(tntbe01l)
055300050826     C                   movel(P)  tbeuni        DFAR
055400050826     C                   else
055500050826     C                   clear                   DFAR
055600050826     C                   endif
055700050826     C*
055800050826     C* Valorizzo buffer record
055900050826     C                   clear                   FNVAPDS
056000050826     C                   eval      vapIDC = oriIDC
056100050826     C                   eval      vapPOE = oriPOE
056200050826     C                   eval      vapRFA = vaoRFA
056300050826     C                   eval      vapPOG = oriPOE
056400050826     C                   eval      vapDAE = oriDER
056500050826     C                   eval      vapORE = oriOER
056600050826     C                   if        d§fardva <> *blanks
056700050826     C                   eval      vapDFA = d§fardva
056800050826     C                   else
056900050826     C                   eval      vapDFA = d§fardes
057000050826     C                   endif
057100050826     C*
057200050826     C                   clear                   tivgd000
057300080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
057400050826     C                   eval      vgdTIP = 'VP'
057500050826     C                   eval      vgdKSU = vapIDC
057600050826     C                   eval      vgdTSC = 'WW'
057700050826     C                   eval      vgdDAT = datcor
057800120123     C                   eval      vgdPGM = 'TITZ10R'
057900060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
058000060306     C                   exsr      sr_Control
058100060306     C                   if        Cli_Abil = *on
058200060306     C                   eval      vgdKSU = woriIDC
058300120905     C  N55              write     tivgd000
058400060306     C                   endif
058500050826     C*
058600050826     C                   ENDSR
058700050826     C*------------------------------------------------------------------------*
058800010601
058900010601
059000010330     C*----------------------------------------------------*
059100030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
059200010330     C*----------------------------------------------------*
059300010330     C     INZVAR        BEGSR
059400010330     C*
059500030822     C* Inizializzo variabili di wrk
059600020204     C                   Z-ADD     *zeros        Num5_0            5 0
059700120124
059800030822     C*
059900030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
060000120123     C                   CLEAR                   FNVAO000
060100150923     C                   CLEAR                   FNVAOEAR
060200120123     C                   CLEAR                   TIORI000
060300030822     C                   CLEAR                   SkSplitCSV
060400030822     C                   EVAL      i = 1
060500030822     C                   EVAL      posDa = *zeros
060600030822     C                   EVAL      posA  = *zeros
060700030822     C*
060800030822     C* Reimposto i valori di default
060900030822     C                   EXSR      DEFCAM
061000030822     C*
061100010330     C                   ENDSR
061200010330     C*----------------------------------------------------*
061300030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
061400010330     C*----------------------------------------------------*
061500010330     C     DEFCAM        BEGSR
061600030822     C*
061700030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
061800030715     C* e delimitatore testo.
061900030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
062000030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
062100030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
062200040723     C*
062300080707     C* Imposto i valori d default d alcuni campi
062400080708     C*
062500080708     C                   CLEAR                   DORM01
062600120822     C                   EVAL      VAOFLO = DORM01
062700080707     C*
062800150923     C                   EVAL      VAOPOE = 061
062900150923     C                   EVAL      VAOCOR = 0610701000
063000130716     C                   EVAL      VAOPAG = 'O'
063100150923     C                   EVAL      VAOKSC = 0610701
063200150923     C                   EVAL      VAOCRC = 0
063300130513
063400120905     C* se il 1° char dei parametri è T=Test
063500120905     C                   SETOFF                                       55
063600120905     C                   IF        %subst(vlrppt:1:1) = 'T'
063700120905     C* inibisco la scrittura dei file TIORI e TIVGD
063800120905     C                   SETON                                        55
063900120905     C* imposto il PO di emissione come filiale fittizia
064000120905     C                   EVAL      VAOPOE = 999
064100120905     C                   ENDIF
064200091127     C*
064300091204     C* IMPOSTO I CAMPI PERSONALIZZATI
064400100416     C
064500030822     C*
064600030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
064700030822     C                   EVAL      posDaDft = 1
064800030822     C                   EVAL      posADft  = 0
064900030822     C                   EVAL      wGiroDft = 0
065000030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
065100030822     C                             posDaDft > 0
065200030822     C*
065300030822     C* Gestisco il 1° giro
065400030822     C                   IF        wGiroDft = 0
065500030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
065600030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
065700030822     C* Incremento il contatore dei "giri"
065800030822     C                   EVAL      wGiroDft = 1
065900030822     C                   ELSE
066000030822     C                   EVAL      posDaDft = posADft
066100030822     C                   ENDIF
066200030822     C* Eseguo lo scan x trovare la fine del campo corrente
066300030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
066400030822     C*
066500030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
066600030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
066700030822     C                   IF        posDaDft > 0 AND
066800030822     C                             posADft  > 0
066900040708     C* POE
067000040708     C                   IF        %subst(
067100040708     C                             %subst(vlrppt:posDaDft+1:
067200040708     C                             posADft-posDaDft-1):1:3)
067300040708     C                             = 'POE'
067400040708     C                   EVAL      PiStr=%trim(%subst(
067500040708     C                             %subst(vlrppt:posDaDft+1:
067600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067700040708     C                   EXSR      CHKNUM
067800040708     C                   IF        PiInt=*on
067900040708     C                   Z-ADD     PiVal         VAOPOE
068000040708     C                   ENDIF
068100040708     C                   ENDIF
068200040708     C* TOR
068300040708     C                   IF        %subst(
068400040708     C                             %subst(vlrppt:posDaDft+1:
068500040708     C                             posADft-posDaDft-1):1:3)
068600040708     C                             = 'TOR'
068700040708     C                   EVAL      VAOTOR=%trim(%subst(
068800040708     C                             %subst(vlrppt:posDaDft+1:
068900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069000040708     C                   ENDIF
069100040708     C* TCO
069200040708     C                   IF        %subst(
069300040708     C                             %subst(vlrppt:posDaDft+1:
069400040708     C                             posADft-posDaDft-1):1:3)
069500040708     C                             = 'TCO'
069600040708     C                   EVAL      VAOTCO=%trim(%subst(
069700040708     C                             %subst(vlrppt:posDaDft+1:
069800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069900040708     C                   ENDIF
070000040707     C* COR
070100030822     C                   IF        %subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):1:3)
070400040707     C                             = 'COR'
070500030822     C                   EVAL      PiStr=%trim(%subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070800030822     C                   EXSR      CHKNUM
070900030822     C                   IF        PiInt=*on
071000040708     C                   Z-ADD     PiVal         VAOCOR
071100030822     C                   ENDIF
071200030822     C                   ENDIF
071300040708     C* CRC
071400040708     C                   IF        %subst(
071500040708     C                             %subst(vlrppt:posDaDft+1:
071600040708     C                             posADft-posDaDft-1):1:3)
071700040708     C                             = 'CRC'
071800040708     C                   EVAL      PiStr=%trim(%subst(
071900040708     C                             %subst(vlrppt:posDaDft+1:
072000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072100040708     C                   EXSR      CHKNUM
072200040708     C                   IF        PiInt=*on
072300040708     C                   Z-ADD     PiVal         VAOCRC
072400040708     C                   ENDIF
072500040708     C                   ENDIF
072600040708     C* FFD
072700040708     C                   IF        %subst(
072800040708     C                             %subst(vlrppt:posDaDft+1:
072900040708     C                             posADft-posDaDft-1):1:3)
073000040708     C                             = 'FFD'
073100040708     C                   EVAL      VAOFFD=%trim(%subst(
073200040708     C                             %subst(vlrppt:posDaDft+1:
073300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073400040708     C                   ENDIF
073500040708     C* ORR
073600040708     C                   IF        %subst(
073700040708     C                             %subst(vlrppt:posDaDft+1:
073800040708     C                             posADft-posDaDft-1):1:3)
073900040708     C                             = 'ORR'
074000040708     C                   EVAL      PiStr=%trim(%subst(
074100040708     C                             %subst(vlrppt:posDaDft+1:
074200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074300040708     C                   EXSR      CHKNUM
074400040708     C                   IF        PiInt=*on
074500040708     C                   Z-ADD     PiVal         VAOORR
074600040708     C                   ENDIF
074700040708     C                   ENDIF
074800040712     C* NCL
074900040712     C                   IF        %subst(
075000040712     C                             %subst(vlrppt:posDaDft+1:
075100040712     C                             posADft-posDaDft-1):1:3)
075200040712     C                             = 'NCL'
075300040712     C                   EVAL      PiStr=%trim(%subst(
075400040712     C                             %subst(vlrppt:posDaDft+1:
075500040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075600040712     C                   EXSR      CHKNUM
075700040712     C                   IF        PiInt=*on
075800040712     C                   Z-ADD     PiVal         VAONCL
075900040712     C                   ENDIF
076000040712     C                   ENDIF
076100040712     C* PKG
076200040712     C                   IF        %subst(
076300040712     C                             %subst(vlrppt:posDaDft+1:
076400040712     C                             posADft-posDaDft-1):1:3)
076500040712     C                             = 'PKG'
076600040712     C                   EVAL      PiStr=%trim(%subst(
076700040712     C                             %subst(vlrppt:posDaDft+1:
076800040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076900040712     C                   EXSR      CHKNUM
077000040712     C                   IF        PiNum=*on
077100040712     C                   Z-ADD     PiVal         VAOPKG
077200040712     C                   ENDIF
077300040712     C                   ENDIF
077400040712     C* VLM
077500040712     C                   IF        %subst(
077600040712     C                             %subst(vlrppt:posDaDft+1:
077700040712     C                             posADft-posDaDft-1):1:3)
077800040712     C                             = 'VLM'
077900040712     C                   EVAL      PiStr=%trim(%subst(
078000040712     C                             %subst(vlrppt:posDaDft+1:
078100040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078200040712     C                   EXSR      CHKNUM
078300040712     C                   IF        PiNum=*on
078400040712     C                   Z-ADD     PiVal         VAOVLM
078500040712     C                   ENDIF
078600040712     C                   ENDIF
078700040708     C* NAM
078800040708     C                   IF        %subst(
078900040708     C                             %subst(vlrppt:posDaDft+1:
079000040708     C                             posADft-posDaDft-1):1:3)
079100040708     C                             = 'NAM'
079200040708     C                   EVAL      VAONAM=%trim(%subst(
079300040708     C                             %subst(vlrppt:posDaDft+1:
079400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079500040708     C                   ENDIF
079600040708     C* PAG
079700030822     C                   IF        %subst(
079800030822     C                             %subst(vlrppt:posDaDft+1:
079900030822     C                             posADft-posDaDft-1):1:3)
080000040708     C                             = 'PAG'
080100040708     C                   EVAL      VAOPAG=%trim(%subst(
080200030822     C                             %subst(vlrppt:posDaDft+1:
080300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080400030822     C                   ENDIF
080500040708     C* KSC
080600040708     C                   IF        %subst(
080700040708     C                             %subst(vlrppt:posDaDft+1:
080800040708     C                             posADft-posDaDft-1):1:3)
080900040708     C                             = 'KSC'
081000040708     C                   EVAL      PiStr=%trim(%subst(
081100040708     C                             %subst(vlrppt:posDaDft+1:
081200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081300040708     C                   EXSR      CHKNUM
081400040708     C                   IF        PiInt=*on
081500040708     C                   Z-ADD     PiVal         VAOKSC
081600040708     C                   ENDIF
081700040708     C                   ENDIF
081800040708     C* CTR
081900040708     C                   IF        %subst(
082000040708     C                             %subst(vlrppt:posDaDft+1:
082100040708     C                             posADft-posDaDft-1):1:3)
082200040708     C                             = 'CTR'
082300040708     C                   EVAL      VAOCTR=%trim(%subst(
082400040708     C                             %subst(vlrppt:posDaDft+1:
082500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082600040708     C                   ENDIF
082700040708     C* POC
082800040708     C                   IF        %subst(
082900040708     C                             %subst(vlrppt:posDaDft+1:
083000040708     C                             posADft-posDaDft-1):1:3)
083100040708     C                             = 'POC'
083200040708     C                   EVAL      PiStr=%trim(%subst(
083300040708     C                             %subst(vlrppt:posDaDft+1:
083400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083500040708     C                   EXSR      CHKNUM
083600040708     C                   IF        PiInt=*on
083700040708     C                   Z-ADD     PiVal         VAOPOC
083800040708     C                   ENDIF
083900040708     C                   ENDIF
084000040708     C* NO1
084100040708     C                   IF        %subst(
084200040708     C                             %subst(vlrppt:posDaDft+1:
084300121129     C                             posADft-posDaDft-1):1:3)
084400040708     C                             = 'NO1'
084500040708     C                   EVAL      VAONO1=%trim(%subst(
084600040708     C                             %subst(vlrppt:posDaDft+1:
084700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084800040708     C                   ENDIF
084900040708     C* DDT
085000030822     C                   IF        %subst(
085100030822     C                             %subst(vlrppt:posDaDft+1:
085200030822     C                             posADft-posDaDft-1):1:3)
085300040708     C                             = 'DDT'
085400040708     C                   EVAL      VAODDT=%trim(%subst(
085500030822     C                             %subst(vlrppt:posDaDft+1:
085600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085700030822     C                   ENDIF
085800040708     C* RFA
085900040708     C                   IF        %subst(
086000040708     C                             %subst(vlrppt:posDaDft+1:
086100040708     C                             posADft-posDaDft-1):1:3)
086200040708     C                             = 'RFA'
086300040708     C                   EVAL      VAORFA=%trim(%subst(
086400040708     C                             %subst(vlrppt:posDaDft+1:
086500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086600040708     C                   ENDIF
086700040708     C* FLO
086800040708     C                   IF        %subst(
086900040708     C                             %subst(vlrppt:posDaDft+1:
087000040708     C                             posADft-posDaDft-1):1:3)
087100040708     C                             = 'FLO'
087200040708     C                   EVAL      VAOFLO=%trim(%subst(
087300040708     C                             %subst(vlrppt:posDaDft+1:
087400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
087500091103     C*
087600040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
087700080702     C* e posizione 7 del VAOFLO
087800040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
087900040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
088000050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
088100040708     C                   ENDIF
088200030822     C* ...
088300030822     C                   ENDIF
088400030822     C                   ENDDO
088500020204     C*
088600010330     C                   ENDSR
088700010607     C*----------------------------------------------------*
088800040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
088900010607     C*----------------------------------------------------*
089000040708     C     IMPVAO        BEGSR
089100010607     C*
089200010607     C                   SETOFF                                       3132
089300120124
089400030515     C*
089500040708     C                   MOVEL     datcor        VAODAO
089600040708     C                   MOVEL     wn14          VAOOAO
089700050314     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
089800050314     C     minu:maiu     XLATE     vindta        vindta
089900010607     C*
090000030715     C**********
090100030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
090200030715     C**********
090300030715     C                   IF        wGiro = *zeros
090400040709     C*
090500040709     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
090600040709     C                   EVAL      depvinDTA = vinDTA
090700040709     C*
090800040709     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
090900040709     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
091000040709     C                                      %trim(intcol(3))
091100031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
091200050314     C**!!!minu:maiu     XLATE     vindta        vindta
091300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
091400040324     C                   DOW       posDa <= %len(%trimr(vindta))
091500030715     C*
091600030715     C* Gestisco il 1° campo
091700030715     C                   IF        i = 1
091800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
091900030715     C                   EVAL      posDa = 1
092000030715     C* Eseguo lo scan x trovare la fine del primo campo
092100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092200030715     C* A questo punto "estraggo" il campo corrente
092300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
092400030715     C                                                    (posA-posDa))
092500030715     C* X i campi successivi al 1°
092600030715     C                   ELSE
092700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
092800030715     C                   EVAL      posDa = posA + 1
092900030715     C* Eseguo lo scan x trovare la fine del campo corrente
093000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
093100030715     C* Gestisco l'ultimo campo
093200030715     C                   IF        posA = *zeros
093300040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
093400030715     C* A questo punto "estraggo" il campo corrente
093500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
093600030715     C                                                    (posA-posDa))
093700030715     C                   LEAVE
093800030715     C                   ELSE
093900030715     C* A questo punto "estraggo" il campo corrente
094000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
094100030715     C                                                    (posA-posDa))
094200030715     C                   ENDIF
094300030715     C                   ENDIF
094400030715     C* Incremento il contatore d campo
094500030715     C                   EVAL      i = i +1
094600030715     C                   ENDDO
094700030715     C*
094800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
094900030715     C* delimitazione testo
095000030715     C                   EVAL      i = 1
095100030715     C                   DOW       i < %elem(SkSplitFLD)
095200030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
095300030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
095400030715     C                   EVAL      i = i + 1
095500030715     C                   ENDDO
095600030715     C                   EVAL      wGiro = 1
095700040709     C* Ripristino la 1° riga originale
095800040709     C                   EVAL      vinDTA = depvinDTA
095900030715     C                   ELSE
096000030715     C                   EVAL      wGiro = 2
096100031204     C**********
096200031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
096300031204     C* d separatore campo
096400031204     C**********
096500040709     C                   EVAL      vindta = %trim(vindta)
096600040709     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
096700040709     C                   EVAL      vindta = %subst(vindta:2)
096800040709     C                   ENDDO
096900040709     C*
097000040709     C                   Z-ADD     *zeros        lunghInput        4 0
097100040709     C                   EVAL      lunghInput = %len(%trim(vindta))
097200040709     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
097300040709     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
097400040709     C                   EVAL      lunghInput = %len(%trim(vindta))
097500040709     C                   ENDDO
097600030715     C**********
097700030715     C* Effettuo lo split del campo dati d input
097800030715     C**********
097900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
098000040324     C                   DOW       posDa <= %len(%trimr(vindta))
098100030715     C*
098200030715     C* Gestisco il 1° campo
098300030715     C                   IF        i = 1
098400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
098500030715     C                   EVAL      posDa = 1
098600030715     C* Eseguo lo scan x trovare la fine del primo campo
098700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
098800030715     C* A questo punto "estraggo" il campo corrente
098900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
099000030715     C                                                    (posA-posDa))
099100030715     C* X i campi successivi al 1°
099200030715     C                   ELSE
099300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
099400030715     C                   EVAL      posDa = posA + 1
099500031204     C* Verifico che nn vi sia il campo nn valorizzato
099600031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
099700031204     C* Se campo nn valorizzato skippo al prossimo
099800031204     C                   EVAL      SkSplitCSV(i) = *blanks
099900031204     C                   EVAL      posA  = posA + 1
100000031204     C                   ELSE
100100030715     C* Eseguo lo scan x trovare la fine del campo corrente
100200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
100300030715     C* Gestisco l'ultimo campo
100400030715     C                   IF        posA = *zeros
100500040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
100600030715     C* A questo punto "estraggo" il campo corrente
100700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
100800030715     C                                                    (posA-posDa))
100900030715     C                   LEAVE
101000030715     C                   ELSE
101100030715     C* A questo punto "estraggo" il campo corrente
101200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
101300030715     C                                                    (posA-posDa))
101400030715     C                   ENDIF
101500031204     C                   ENDIF
101600030715     C                   ENDIF
101700030715     C* Incremento il contatore d campo
101800030715     C                   EVAL      i = i +1
101900030715     C                   ENDDO
102000030715     C*
102100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
102200030715     C* delimitazione testo
102300030715     C                   EVAL      i = 1
102400030715     C                   DOW       i < %elem(SkSplitCSV)
102500030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
102600030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
102700030715     C                   EVAL      i = i + 1
102800030715     C                   ENDDO
102900030715     C*
103000030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
103100030715     C                   EVAL      i = 1
103200030715     C                   DOW       i < %elem(SkSplitFLD)
103300030715     C*
103400030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
103500030715     C                   IF        SkSplitFLD(i) <> *blanks AND
103600030715     C                             SkSplitCSV(i) <> *blanks
103700030721     C*** ATB
103800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATB'
103900040708     C                   EVAL      VAOATB = SkSplitCSV(i)
104000030721     C                   ENDIF
104100040708     C*** TOR
104200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTOR'
104300040708     C                   EVAL      VAOTOR = SkSplitCSV(i)
104400040708     C                   ENDIF
104500040708     C*** TCO
104600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTCO'
104700040708     C                   EVAL      VAOTCO = SkSplitCSV(i)
104800040708     C                   ENDIF
104900040708     C*** RSO
105000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSO'
105100040708     C                   EVAL      VAORSO = SkSplitCSV(i)
105200030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
105300040708     C     '@':'A'       XLATE     VAORSO        VAORSO
105400030715     C* ==
105500030715     C                   ENDIF
105600040708     C*** INO
105700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINO'
105800040708     C                   EVAL      VAOINO = SkSplitCSV(i)
105900030715     C                   ENDIF
106000040708     C*** LOO
106100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOO'
106200040708     C                   EVAL      VAOLOO = SkSplitCSV(i)
106300030715     C                   ENDIF
106400040708     C*** PRO
106500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRO'
106600040708     C                   EVAL      VAOPRO = SkSplitCSV(i)
106700030715     C                   ENDIF
106800040708     C*** NAO
106900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAO'
107000040708     C                   EVAL      VAONAO = SkSplitCSV(i)
107100030721     C                   ENDIF
107200040708     C*** RSR
107300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSR'
107400040708     C                   EVAL      VAORSR = SkSplitCSV(i)
107500040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
107600040708     C     '@':'A'       XLATE     VAORSR        VAORSR
107700040708     C* ==
107800040708     C                   IF        VAORSR = *blanks
107900040708     C                   SETON                                        31
108000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
108100040708     C                             + ' ' + 'VAORSR'
108200040708     C                   ENDIF
108300040708     C                   ENDIF
108400040708     C*** INR
108500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINR'
108600040708     C                   EVAL      VAOINR = SkSplitCSV(i)
108700040708     C                   IF        VAOINR = *blanks
108800040708     C                   SETON                                        31
108900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
109000040708     C                             + ' ' + 'VAOINR'
109100040708     C                   ENDIF
109200040708     C                   ENDIF
109300040708     C*** LOR
109400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOR'
109500040708     C                   EVAL      VAOLOR = SkSplitCSV(i)
109600040708     C                   IF        VAOLOR = *blanks
109700040708     C                   SETON                                        31
109800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
109900040708     C                             + ' ' + 'VAOLOR'
110000040708     C                   ENDIF
110100040708     C                   ENDIF
110200040708     C*** PRR
110300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRR'
110400040708     C                   EVAL      VAOPRR = SkSplitCSV(i)
110500040708     C                   ENDIF
110600040708     C*** NAR
110700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAR'
110800040708     C                   EVAL      VAONAR = SkSplitCSV(i)
110900040708     C                   ENDIF
111000040708     C*** RER
111100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORER'
111200040708     C                   EVAL      VAORER = SkSplitCSV(i)
111300040708     C                   ENDIF
111400040708     C*** TER
111500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTER'
111600040708     C                   EVAL      VAOTER = SkSplitCSV(i)
111700040708     C                   ENDIF
111800150923     C*** EMLR
111900150923     C                   IF        %trim(SkSplitFLD(i)) = 'VAOEMLR'
112000150923     C                   EVAL      w_VAOEMLR = SkSplitCSV(i)
112100150923     C                   ENDIF
112200040708     C*** RSC
112300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSC'
112400040708     C                   EVAL      VAORSC = SkSplitCSV(i)
112500040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
112600040708     C     '@':'A'       XLATE     VAORSC        VAORSC
112700040708     C* ==
112800040708     C                   ENDIF
112900040708     C*** INC
113000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINC'
113100040708     C                   EVAL      VAOINC = SkSplitCSV(i)
113200040708     C                   ENDIF
113300040708     C*** LOC
113400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOC'
113500040708     C                   EVAL      VAOLOC = SkSplitCSV(i)
113600040708     C                   ENDIF
113700040708     C*** PRC
113800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRC'
113900040708     C                   EVAL      VAOPRC = SkSplitCSV(i)
114000040708     C                   ENDIF
114100040708     C*** NAC
114200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAC'
114300040708     C                   EVAL      VAONAC = SkSplitCSV(i)
114400040708     C                   ENDIF
114500040708     C*** FFD
114600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFFD'
114700040708     C                   EVAL      VAOFFD = SkSplitCSV(i)
114800040708     C                   ENDIF
114900040708     C*** RMP
115000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORMP'
115100040708     C                   EVAL      VAORMP = SkSplitCSV(i)
115200040708     C                   ENDIF
115300040708     C*** NAM
115400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAM'
115500040708     C                   EVAL      VAONAM = SkSplitCSV(i)
115600040708     C                   ENDIF
115700040708     C*** PAG
115800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPAG'
115900040708     C                   EVAL      VAOPAG = SkSplitCSV(i)
116000040708     C                   ENDIF
116100040708     C*** CTR
116200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCTR'
116300040708     C                   EVAL      VAOCTR = SkSplitCSV(i)
116400040708     C                   ENDIF
116500040708     C*** ZOR
116600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOZOR'
116700040708     C                   EVAL      VAOZOR = SkSplitCSV(i)
116800040708     C                   ENDIF
116900040708     C*** NO1
117000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO1'
117100040708     C                   EVAL      VAONO1 = SkSplitCSV(i)
117200030715     C                   ENDIF
117300040708     C*** NO2
117400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO2'
117500040708     C                   EVAL      VAONO2 = SkSplitCSV(i)
117600030715     C                   ENDIF
117700040708     C*** DDT
117800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODDT'
117900040708     C                   EVAL      VAODDT = SkSplitCSV(i)
118000030715     C                   ENDIF
118100040708     C*** RFA
118200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORFA'
118300040708     C                   EVAL      VAORFA = SkSplitCSV(i)
118400030715     C                   ENDIF
118500040708     C*** SPI
118600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSPI'
118700040708     C                   EVAL      VAOSPI = SkSplitCSV(i)
118800030715     C                   ENDIF
118900040708     C*** FLO
119000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
119100040708     C                   EVAL      VAOFLO = SkSplitCSV(i)
119200040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
119300080708     C* e la posizione 7 del VAOFLO
119400040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
119500040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
119600050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
119700030715     C                   ENDIF
119800091103     C
119900091103     C*** DAR
120000050314     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
120100050412     C                   EVAL      PiStr=SkSplitCSV(i)
120200050412     C                   EXSR      CHKNUM
120300050412     C                   IF        PiInt=*on
120400050412     C                   Z-ADD     PiVal         VAODAR
120500050412     C                   ELSE
120600050412     C                   SETON                                        32
120700050412     C                   EVAL      VAODAR = *zeros
120800050412     C                   EVAL      vinmsg = %trimr(vinmsg)
120900050412     C                             + ' ' + 'VAODAR'
121000050412     C                   ENDIF
121100050314     C                   ENDIF
121200030715     C
121300030721     C*
121400030721     C* Reperisco quindi i campi numerici...
121500040708     C*** POE
121600040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
121700040723     C                   EVAL      PiStr=SkSplitCSV(i)
121800040723     C                   EXSR      CHKNUM
121900040723     C                   IF        PiInt=*on    AND
122000040723     C                             PiVal<=999   AND
122100040723     C                             PiVal>*zeros
122200040723     C                   Z-ADD     PiVal         VAOPOE
122300040723     C                   ELSE
122400040723     C                   SETON                                        32
122500040723     C                   EVAL      VAOPOE = *zeros
122600040723     C                   EVAL      vinmsg = %trimr(vinmsg)
122700040723     C                             + ' ' + 'VAOPOE'
122800040723     C                   ENDIF
122900040723     C                   ENDIF
123000040708     C*** COR
123100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
123200040708     C                   EVAL      PiStr=SkSplitCSV(i)
123300040708     C                   EXSR      CHKNUM
123400040708     C                   IF        PiInt=*on
123500040708     C                   Z-ADD     PiVal         VAOCOR
123600040708     C                   ELSE
123700040708     C                   SETON                                        31
123800040708     C                   EVAL      VAOCOR = *zeros
123900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
124000040708     C                             + ' ' + 'VAOCOR'
124100040708     C                   ENDIF
124200040708     C                   ENDIF
124300040708     C*** CAO
124400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
124500030721     C                   EVAL      PiStr=SkSplitCSV(i)
124600030721     C                   EXSR      CHKNUM
124700030721     C                   IF        PiInt=*on
124800030721     C                   Z-ADD     PiVal         Num5_0
124900040708     C                   MOVEL(p)  Num5_0        VAOCAO
125000030721     C                   ELSE
125100030721     C                   SETON                                        32
125200040708     C                   EVAL      VAOCAO = *zeros
125300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125400040708     C                             + ' ' + 'VAOCAO'
125500030721     C                   ENDIF
125600030721     C                   ENDIF
125700040708     C*** CRA
125800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
125900030721     C                   EVAL      PiStr=SkSplitCSV(i)
126000030721     C                   EXSR      CHKNUM
126100030721     C                   IF        PiInt=*on
126200040708     C                   Z-ADD     PiVal         VAOCRA
126300030721     C                   ELSE
126400030721     C                   SETON                                        32
126500040708     C                   EVAL      VAOCRA = *zeros
126600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126700040708     C                             + ' ' + 'VAOCRA'
126800030721     C                   ENDIF
126900030721     C                   ENDIF
127000040708     C*** CAR
127100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
127200040708     C                   EVAL      PiStr=SkSplitCSV(i)
127300040708     C                   EXSR      CHKNUM
127400040708     C                   IF        PiInt=*on
127500040708     C                   Z-ADD     PiVal         Num5_0
127600040708     C                   MOVEL(p)  Num5_0        VAOCAR
127700040708     C                   ELSE
127800040708     C                   SETON                                        31
127900040708     C                   EVAL      VAOCAR = *zeros
128000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
128100040708     C                             + ' ' + 'VAOCAR'
128200040708     C                   ENDIF
128300040708     C                   ENDIF
128400040708     C*** CRC
128500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
128600040708     C                   EVAL      PiStr=SkSplitCSV(i)
128700040708     C                   EXSR      CHKNUM
128800040708     C                   IF        PiInt=*on
128900040708     C                   Z-ADD     PiVal         VAOCRC
129000040708     C                   ELSE
129100040708     C                   SETON                                        32
129200040708     C                   EVAL      VAOCRC = *zeros
129300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
129400040708     C                             + ' ' + 'VAOCRC'
129500040708     C                   ENDIF
129600040708     C                   ENDIF
129700040708     C*** CAC
129800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
129900040708     C                   EVAL      PiStr=SkSplitCSV(i)
130000040708     C                   EXSR      CHKNUM
130100040708     C                   IF        PiInt=*on
130200040708     C                   Z-ADD     PiVal         Num5_0
130300040708     C                   MOVEL(p)  Num5_0        VAOCAC
130400040708     C                   ELSE
130500040708     C                   SETON                                        32
130600040708     C                   EVAL      VAOCAC = *zeros
130700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
130800040708     C                             + ' ' + 'VAOCAC'
130900040708     C                   ENDIF
131000040708     C                   ENDIF
131100040708     C*** DAR
131200090428     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
131300090428     C                   EVAL      PiStr=SkSplitCSV(i)
131400090428     C                   EXSR      CHKNUM
131500090428     C                   IF        PiInt=*on
131600090428     C                   Z-ADD     PiVal         VAODAR
131700090428     C                   ELSE
131800090428     C                   SETON                                        32
131900090428     C                   EVAL      VAODAR = *zeros
132000090428     C                   EVAL      vinmsg = %trimr(vinmsg)
132100090428     C                             + ' ' + 'VAODAR'
132200090428     C                   ENDIF
132300090428     C                   ENDIF
132400040708     C*** ORR
132500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
132600040708     C                   EVAL      PiStr=SkSplitCSV(i)
132700040708     C                   EXSR      CHKNUM
132800040708     C                   IF        PiInt=*on
132900040708     C                   Z-ADD     PiVal         VAOORR
133000040708     C                   ELSE
133100040708     C                   SETON                                        32
133200040708     C                   EVAL      VAOORR = *zeros
133300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
133400040708     C                             + ' ' + 'VAOORR'
133500040708     C                   ENDIF
133600040708     C                   ENDIF
133700030721     C*** NCL
133800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
133900030721     C                   EVAL      PiStr=SkSplitCSV(i)
134000030721     C                   EXSR      CHKNUM
134100030721     C                   IF        PiInt=*on
134200040723     C                   IF        PiVal > *zeros
134300040708     C                   Z-ADD     PiVal         VAONCL
134400040723     C                   ENDIF
134500030721     C                   ELSE
134600030721     C                   SETON                                        32
134700040708     C                   EVAL      VAONCL = *zeros
134800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134900040708     C                             + ' ' + 'VAONCL'
135000030721     C                   ENDIF
135100030721     C                   ENDIF
135200040708     C*** PKG
135300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
135400030721     C                   EVAL      PiStr=SkSplitCSV(i)
135500030721     C                   EXSR      CHKNUM
135600030721     C                   IF        PiNum=*on
135700040723     C                   IF        PiVal > *zeros
135800040708     C                   Z-ADD     PiVal         VAOPKG
135900040723     C                   ENDIF
136000030721     C                   ELSE
136100030721     C                   SETON                                        32
136200040708     C                   EVAL      VAOPKG = *zeros
136300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136400040708     C                             + ' ' + 'VAOPKG'
136500030721     C                   ENDIF
136600030721     C                   ENDIF
136700040708     C*** VLM
136800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
136900040708     C                   EVAL      PiStr=SkSplitCSV(i)
137000040708     C                   EXSR      CHKNUM
137100040708     C                   IF        PiNum=*on
137200040708     C                   Z-ADD     PiVal         VAOVLM
137300040708     C                   ELSE
137400040708     C                   SETON                                        32
137500040708     C                   EVAL      VAOVLM = *zeros
137600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
137700040708     C                             + ' ' + 'VAOVLM'
137800040708     C                   ENDIF
137900040708     C                   ENDIF
138000040708     C*** BNC
138100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
138200040708     C                   EVAL      PiStr=SkSplitCSV(i)
138300040708     C                   EXSR      CHKNUM
138400040708     C                   IF        PiInt=*on
138500040708     C                   Z-ADD     PiVal         VAOBNC
138600040708     C                   ELSE
138700040708     C                   SETON                                        32
138800040708     C                   EVAL      VAOBNC = *zeros
138900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
139000040708     C                             + ' ' + 'VAOBNC'
139100040708     C                   ENDIF
139200040708     C                   ENDIF
139300040708     C*** BLC
139400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
139500040708     C                   EVAL      PiStr=SkSplitCSV(i)
139600040708     C                   EXSR      CHKNUM
139700040708     C                   IF        PiInt=*on
139800040708     C                   Z-ADD     PiVal         VAOBLC
139900040708     C                   ELSE
140000040708     C                   SETON                                        32
140100040708     C                   EVAL      VAOBLC = *zeros
140200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
140300040708     C                             + ' ' + 'VAOBLC'
140400040708     C                   ENDIF
140500040708     C                   ENDIF
140600040708     C*** ATT
140700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
140800040708     C                   EVAL      PiStr=SkSplitCSV(i)
140900040708     C                   EXSR      CHKNUM
141000040708     C                   IF        PiInt=*on
141100040708     C                   Z-ADD     PiVal         VAOATT
141200040708     C                   ELSE
141300040708     C                   SETON                                        32
141400040708     C                   EVAL      VAOATT = *zeros
141500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
141600040708     C                             + ' ' + 'VAOATT'
141700040708     C                   ENDIF
141800040708     C                   ENDIF
141900040708     C*** MTC
142000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
142100040708     C                   EVAL      PiStr=SkSplitCSV(i)
142200040708     C                   EXSR      CHKNUM
142300040708     C                   IF        PiInt=*on
142400040708     C                   Z-ADD     PiVal         VAOMTC
142500040708     C                   ELSE
142600040708     C                   SETON                                        32
142700040708     C                   EVAL      VAOMTC = *zeros
142800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
142900040708     C                             + ' ' + 'VAOMTC'
143000040708     C                   ENDIF
143100040708     C                   ENDIF
143200040708     C*** KSC
143300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
143400030721     C                   EVAL      PiStr=SkSplitCSV(i)
143500030721     C                   EXSR      CHKNUM
143600030721     C                   IF        PiInt=*on      AND
143700030721     C                             PiVal<=9999999 AND
143800030721     C                             PiVal>*zeros
143900040708     C                   Z-ADD     PiVal         VAOKSC
144000030721     C                   ELSE
144100030721     C                   SETON                                        32
144200040708     C                   EVAL      VAOKSC = *zeros
144300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
144400040708     C                             + ' ' + 'VAOKSC'
144500030721     C                   ENDIF
144600030721     C                   ENDIF
144700040708     C*** POR
144800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
144900030721     C                   EVAL      PiStr=SkSplitCSV(i)
145000030721     C                   EXSR      CHKNUM
145100030721     C                   IF        PiInt=*on    AND
145200030721     C                             PiVal<=999   AND
145300030721     C                             PiVal>*zeros
145400040708     C                   Z-ADD     PiVal         VAOPOR
145500030721     C                   ELSE
145600030721     C                   SETON                                        32
145700040708     C                   EVAL      VAOPOR = *zeros
145800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
145900040708     C                             + ' ' + 'VAOPOR'
146000030721     C                   ENDIF
146100030721     C                   ENDIF
146200040708     C*** POC
146300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
146400040708     C                   EVAL      PiStr=SkSplitCSV(i)
146500040708     C                   EXSR      CHKNUM
146600040708     C                   IF        PiInt=*on    AND
146700040708     C                             PiVal<=999   AND
146800040708     C                             PiVal>*zeros
146900040708     C                   Z-ADD     PiVal         VAOPOC
147000040708     C                   ELSE
147100040708     C                   SETON                                        32
147200040708     C                   EVAL      VAOPOC = *zeros
147300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
147400040708     C                             + ' ' + 'VAOPOC'
147500040708     C                   ENDIF
147600040708     C                   ENDIF
147700040708     C*** STO
147800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
147900040708     C                   EVAL      PiStr=SkSplitCSV(i)
148000040708     C                   EXSR      CHKNUM
148100040708     C                   IF        PiInt=*on
148200040708     C                   Z-ADD     PiVal         VAOSTO
148300040708     C                   ELSE
148400040708     C                   SETON                                        32
148500040708     C                   EVAL      VAOSTO = *zeros
148600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
148700040708     C                             + ' ' + 'VAOSTO'
148800040708     C                   ENDIF
148900040708     C                   ENDIF
149000090428     C*
149100091103     C********** GESTIONE CAMPI PERSONALIZZATI
149200150923     C*** COR/KSC
149300150923     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAOCOR'
149400150923     C                   SELECT
149500150923     C                   WHEN      %trim(SkSplitCSV(i)) = '056' or
149600150923     C                             %trim(SkSplitCSV(i)) = '56'
149700150923     C                   EVAL      VAOCOR = 0610701000
149800150923     C                   EVAL      VAOKSC = 0610701
149900150923     C                   WHEN      %trim(SkSplitCSV(i)) = '094' or
150000150923     C                             %trim(SkSplitCSV(i)) = '94'
150100150923     C                   EVAL      VAOCOR = 0611079000
150200150923     C                   EVAL      VAOKSC = 0611079
150300150923     C                   WHEN      %trim(SkSplitCSV(i)) = '055' or
150400150923     C                             %trim(SkSplitCSV(i)) = '55'
150500151029     C                   EVAL      VAOCOR = 0610329000
150600151029     C                   EVAL      VAOKSC = 0610329
150700151029     C                   WHEN      %trim(SkSplitCSV(i)) = '042' or
150800151029     C                             %trim(SkSplitCSV(i)) = '42'
150900151029     C                   EVAL      VAOCOR = 0611126000
151000151029     C                   EVAL      VAOKSC = 0611126
151100150923     C                   OTHER
151200150923     C                   SETON                                        31
151300150923     C                   EVAL      VAOCOR = *zeros
151400150923     C                   EVAL      VAOKSC = *zeros
151500150923     C                   EVAL      vinmsg = %trimr(vinmsg)
151600150923     C                             + ' ' + 'VAOCOR VAOKSC'
151700150923     C                   ENDSL
151800150923     C                   ENDIF
151900150923     C*** CRC
152000150923     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAOCRC'
152100150923     C                   SELECT
152200150923     C                   WHEN      %trim(SkSplitCSV(i)) = 'ISV'
152300150923     C                   EVAL      VAOCRC = 0610463000
152400150923     C                   WHEN      %trim(SkSplitCSV(i)) = 'LIM'
152500150923     C                   EVAL      VAOCRC = 0062615000
152600150923     C                   WHEN      %trim(SkSplitCSV(i)) = 'MAR'
152700150923     C                   EVAL      VAOCRC = 0611079000
152800150923     C                   OTHER
152900150923     C                   SETON                                        31
153000150923     C                   EVAL      VAOCOR = *zeros
153100150923     C                   EVAL      VAOKSC = *zeros
153200150923     C                   EVAL      vinmsg = %trimr(vinmsg)
153300150923     C                             + ' ' + 'VAOCOR VAOKSC'
153400150923     C                   ENDSL
153500150923     C                   ENDIF
153600150923     C*
153700150923     C* DAR
153800150923     C* la data arriva in formato gg/mm/aaaa e va girata in formato aaaammgg
153900150923     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAODAR'
154000150923     C* non posso usare UBFMTDATE perché la maschera gg/mm/aaaa non è gestita
154100150923     C                   EVAL      VAODAR = %dec(
154200150923     C                              %subst(%trim(SkSplitCSV(i)):7:4) +
154300150923     C                              %subst(%trim(SkSplitCSV(i)):4:2) +
154400150923     C                              %subst(%trim(SkSplitCSV(i)):1:2)
154500150923     C                               : 8 : 0)
154600150923     C                   ENDIF
154700150923     C*
154800150923     C* NO1/NO2
154900150923     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAONO1'
155000150923     C                   EVAL      w70 = %trim(SkSplitCSV(i))
155100150923     C                   EVAL      VAONO1=%subst(w70 : 1 :35)
155200150923     C                   EVAL      VAONO2=%subst(w70 : 36 :35)
155300150923     C                   ENDIF
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
156600151117     C* Reperisco la provincia dal CAP e dalla località
156700151117     C                   IF        VAOCAR <> *blanks AND
156800151117     C                             VAOPRR  = *blanks AND
156900151117     C                             VAONAR  = *blanks
157000151117     C                   CLEAR                   TISI95DS
157100151117     C                   EVAL      I95TCN = '3'
157200151117     C                   Z-ADD     datcor        I95DAT
157300151117     C                   EVAL      I95CAP = VAOCAR
157400151117     C                   EVAL      I95LOC = VAOLOR
157500151117     C                   CALL      'TISI95R'
157600151117     C                   PARM                    TISI95DS
157700151117     C                   EVAL      VAOPRR = O95PRV
157800151117     C                   ENDIF
157900151117
158000140108     C*
158100140108     C* se VAORER non è valorizzato copiare il valore inserito nel campo VAORSR
158200140108     C                   IF        VAORER = *blank
158300140108     C                   EVAL      VAORER = VAORSR
158400140108     C                   ENDIF
158500140108     C*
158600130513
158700030715     C                   ENDIF
158800020204     C*
158900010607     C                   ENDSR
159000010607     C*----------------------------------------------------*
159100080702
159200080702
159300080702     C*----------------------------------------------------*
159400080702     C*  CONTROLLO NUMERICITA' CAMPI
159500080702     C*----------------------------------------------------*
159600080702     C     CHKNUM        BEGSR
159700080702     C*
159800080702     C                   IF        PiDecChr = *blanks
159900080702     C                   EVAL      PiDecChr = ','
160000080702     C                   ENDIF
160100080702     C*
160200080702     C                   callp     UBISNUM_Check(PiStr
160300080702     C                                          :PiDecChr
160400080702     C                                          :PiVal
160500080702     C                                          :PiNum
160600080702     C                                          :PiInt)
160700080702     C*
160800080702     C                   ENDSR
160900080702     C***
161000080702
161100010601
161200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
161300010601     C     repfil        BEGSR
161400010601     C*
161500010601     C                   if        invfil = *zeros and
161600010601     C                             depfil > *zeros and
161700010629     C                             (vinflg = *blanks or
161800010629     C                              vinflg = *zeros)
161900010601     C
162000010601     C                   eval      invfil = depfil
162100010601     C                   endif
162200010601     C*
162300010601     C                   if        depfil <> invfil and
162400010601     C                             invfil > *zeros
162500010601     C                   eval      flgMulti = '1'
162600010601     C                   if        vinflg = *blanks
162700010601     C                   add       1             cntNonEl
162800010601     C                   endif
162900010601     C                   endif
163000010601     C*
163100010601     C                   if        vinflg = '2'
163200010601     C                   eval      flgStato = '2'
163300010601     C                   endif
163400010601     C*
163500010601     C                   ENDSR
163600010601     C***
163700010330
163800010330
163900990920      /TITLE Invio dei dati al punto operativo.
164000000613     C     invio         BEGSR
164100990920     C*
164200990920     C                   reset                   dscmz
164300010601     C                   move      invfil        cmzdst
164400990920     C                   eval      cmzfld = vlrfou
164500990920     C                   eval      cmzmbd = vlrhdl
164600990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
164700000710     C                   if        prmfir = *blanks
164800040708     C                   eval      cmzfla = 'FNVAO00F'
164900040708     C                   eval      cmzmba = 'FNVAO00F'
165000000710     C                   else
165100000710     C                   eval      cmzfla = prmfir
165200000710     C                   eval      cmzmba = prmfir
165300000710     C                   endif
165400990920     C                   eval      cmznrr = *zeros
165500990920     C                   move      §ctrok        cmznrr
165600021018     C                   eval      cmzlba = vlrfl1
165700990920     C                   call(e)   'TIS711C'
165800990920     C                   parm                    dscmz
165900990921     C                   parm      *blanks       esito
166000990920     C                   if        %error
166100990920     C                             or cmzerr = '1'
166200990921     C                             or esito  = '1'
166300000710     C                   eval      wrkesito = '3'
166400990920     C                   endif
166500990920     C*
166600000613     C                   ENDSR
166700010601
166800010601
166900010601      /TITLE Invio dei dati al punto operativo.
167000010601     C     opeini        BEGSR
167100010601     C*
167200010601     C* Inizializzo flag e contatori operativi
167300010601     C                   movel     '0'           flgGiro           1
167400010601     C                   movel     '0'           flgMulti          1
167500010601     C                   movel     '1'           flgStato          1
167600010615     C                   movel     '0'           flgOk             1
167700010601     C                   z-add     *zeros        cntNonEl         10 0
167800010601     C                   z-add     *zeros        depfil            3 0
167900010601     C                   z-add     *zeros        invfil            3 0
168000010601     C*
168100010601     C                   ENDSR
168200010601     C***
168300050421
168400050421
168500091130     C     *pssr         BEGSR
168600050421     C*
168700091130     C                   eval      wrkesito = '2'
168800050421     C*
168900091130     C                   ENDSR     '*CANCL'
169000050421     C***
169100060306
169200060306
169300060306     C*-------------------------------------------------------------------------*
169400060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
169500060306     C*-------------------------------------------------------------------------*
169600060306     C     sr_Control    begsr
169700060306     C*
169800060306     C* inizializzo a ok il flag d controllo abilitazione
169900060306     C                   eval      Cli_Abil = *on
170000060306     C
170100060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
170200060306     C                   clear                   woriIDC
170300060306     C                   eval      kteCOD = 'VAP'
170400060306     C                   eval      kteKE1 = oriIDC
170500060306     C     keytbe        chain     TNTBE01L
170600060306     C                   if        %found(Tntbe01l)
170700060306     C                   eval      woriIDC = TbeKe2
170800060306     C                   else
170900060306     C                   eval      Cli_Abil = *off
171000060306     C                   endif
171100060306     C*
171200060306     C                   endsr
171300010330
171400010330
171500000613     C     *inzsr        BEGSR
171600990910     C*
171700990910     C     *entry        plist
171800990920     C                   parm                    tivlrds
171900990921     C                   parm      wrkesito      esito
172000000724     C                   parm                    prmlit
172100000710     C                   parm                    prmfir
172200010330     C*
172300010330     C* CALCOLA LA DATA CORRENTE
172400091127     C                   z-add     *zeros        datcor            8 0
172500091127     C                   time                    wn14             14 0
172600091127     C                   movel     wn14          wn6               6 0          *ORA (6)
172700130516     C                   eval      datcor = %dec(%date() : *ISO)
172800040723     C*
172900040723     C* Campi riferito al database
173000040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
173100040723     C     *like         define    tbeke1        kteKE1
173200040723     C*
173300040723     C* Definizione chiavi di lettura
173400040723     C* TNTBE01L - parziale
173500040723     C     keytbe        klist
173600040723     C                   kfld                    kteCOD                         *UTENTE
173700040723     C                   kfld                    kteKE1                         *CODICE
173800000613     C*
173900000613     C                   ENDSR
174000000613     C***
174100990908
174200040709** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
174300150923X;W_VAOCOR;W_VAOCRC;X;X;X;W_VAODAR;X;VAORSR;VAOINR;VAOCAR;VAOLOR;VAOPRR;X;VAORER;VAOTER;VAOEMLR;X;
174400151117VAONCL;VAOPKG;W_VAONO1;X;X;X;X;VAORFA;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;
174500130222
