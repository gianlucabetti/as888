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
002600040723     D dorm01        e ds
002700040723     D dfar          e ds
002800090618     d trul33ds      e ds
002900090618     d kpjba         e ds
003000160929     D fnvaoear      e ds                  inz
003100160929     D                                     prefix(w_)
003200010330     D esito           s              1
003300010330     D prmlit          s             10
003400010330     D prmfir          s             10
003500010330     D wrkesito        s                   like(esito)
003600010330     D rrnum           s              6  0 INZ(*zeros)
003700040709     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
003800060306     D Cli_Abil        s              1    inz(*off)
003900060306     D woriIDC         s                   like(oriIDC) inz(*blanks)
004000090618     d §numori         s              7  0 inz
004100120123     d wData_EUR       s               d   datfmt(*eur)
004200120123     d wData_ISO       s               d   datfmt(*iso)
004300120905     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
004400120905     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
004500130513     D Intestazioni    s               n   inz(*on)
004600030715     D*------------------
004700030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004800030715     D*------------------
004900030715     D SkSplitFLD      S             10    DIM(100)
005000030715     D SkSplitCSV      S            256    DIM(100)
005100030715     D CharCSV         S              1
005200030715     D CharTXT         S              1
005300030715     D CharNUM         S              1
005400030715     D posDa           S              3  0 INZ(*zeros)
005500030715     D posA            S              3  0 INZ(*zeros)
005600030715     D i               s              3  0 INZ(1)
005700030715     D wGiro           s              1  0 INZ(*zeros)
005800030822     D*------------------
005900030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006000030822     D*------------------
006100030822     D posDaDft        S              3  0 INZ(*zeros)
006200030822     D posADft         S              3  0 INZ(*zeros)
006300030822     D j               s              3  0 INZ(1)
006400030822     D wGiroDft        s              1  0 INZ(*zeros)
006500040723     D*------------------
006600040723     D* DS REPERIMENTO DATI CLIENTE
006700040723     D*-------------------
006800040723     D BS69DS        E DS                  EXTNAME(TIBS69DS)
006900040723     D ACODS         E DS                  EXTNAME(CNACO00F)
007000040723     D INDDS         E DS                  EXTNAME(CNIND00F)
007100040723     D CLPDS         E DS                  EXTNAME(CNCLP00F)
007200040723     D CLSDS         E DS                  EXTNAME(FNCLS00F)
007300121129     D*------------------
007400130513     D wRSR1           s             35
007500130513     D wRSR2           s             35
007600130513     D wINR1           s             35
007700130513     D wINR2           s             35
007800130513     D w10             s             10
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
014700160929     C*
014800160929     C* Mi predispongo per scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
014900160929     C                   call      'TIS7T7ER'
015000160929     C                   parm      'O'           pIn_Opz           1
015100160929     C                   parm                    tivlrds
015200160929     C                   parm                    FNVAOEAR
015300160929     C                   parm                    oriPRG
015400160929     C                   parm      *zeros        pIn_Dst           3
015500160929     C                   parm      *blanks       pOut_Esito        1
015600160929     C                   if        pOut_Esito = '2'
015700160929     C                   goto      FINE
015800160929     C                   endif
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
017100120905     C                   ENDIF
017200010330     C*
017300030822     C                   do        *HIVAL
017400010330     C*
017500030822     C                   read      tivin00r                               70
017600010618     C*
017700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017800010618     C                   if        vinflg = '1'
017900010618     C                   eval      flgOk = '1'
018000010618     C                   endif
018100010618     C*
018200040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
018300040510     C                   z-add     *zeros        wLenVINDTA        4 0
018400040510     C                   z-add     *zeros        wFlgVINDTA        1 0
018500040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
018600040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
018700040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
018800040510     C                   z-add     1             wFlgVINDTA
018900040510     C                   leave
019000040510     C                   endif
019100040510     C                   enddo
019200040510     C*
019300040218     C                   if        vindta > *blanks AND
019400040510     C                             wFlgVINDTA = 1
019500010330     C                   add       1             rrnum
019600010330     C*
019700010601     C                   if        *in70 = *off and
019800010330     C                             (vinflg = *blanks
019900010330     C                              or vinflg = '0'
020000010330     C                              or vinflg = '2')
020100010330     C*
020200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020300010711     C                   if        vinflg = *blanks or vinflg = '0'
020400010711     C                   clear                   vinmsg
020500010711     C                   endif
020600010601     C*
020700010330     C                   exsr      inzvar
020800040708     C                   exsr      impvao
020900160929     C*
021000160929     C* Stacco subito un numeratore univoco "ORM"
021100160929     C                   exsr      RTVNUM
021200160929     C*
021300160929     C* Verifico se trattasi di formato ORM "esteso"
021400160929     C                   setoff                                       53
021500160929     C                   if        w_VAOFLO2  <> *blanks OR
021600160929     C                             w_VAOTSPS  <> *blanks OR
021700160929     C                             w_VAONOTE  <> *blanks OR
021800160929     C                             w_VAOORDA1 <> *blanks OR
021900160929     C                             w_VAOORAA1 <> *blanks OR
022000160929     C                             w_VAOORDA2 <> *blanks OR
022100160929     C                             w_VAOORAA2 <> *blanks OR
022200160929     C                             w_VAOEMLR  <> *blanks OR
022300160929     C                             w_VAOEMLRF <> *blanks OR
022400160929     C                             w_VAOSMSR  <> *blanks OR
022500160929     C                             w_VAOSMSRF <> *blanks OR
022600160929     C                             w_VAOORIP  <> *blanks OR
022700160929     C                             w_VAODOIO  <> *blanks
022800160929     C                   seton                                        53
022900160929     C                   endif
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
025100160929     C*
025200160929     C* Se richiesta scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
025300160929 xxx C                   if        *in53 and not *in31
025400160929 53  C                   exsr      EXEFORZ
025500160929     C                   call      'TIS7T7ER'
025600160929     C                   parm      'W'           pIn_Opz           1
025700160929     C                   parm                    tivlrds
025800160929     C                   parm                    FNVAOEAR
025900160929     C                   parm                    oriPRG
026000160929     C                   parm      *zeros        pIn_Dst           3
026100160929     C                   parm      *blanks       pOut_Esito        1
026200160929     C                   endif
026300160929     C                   if        pOut_Esito = '2'
026400160929     C                   seton                                        32
026500160929     C                   endif
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
028500160929     C*
028600160929     C     FINE          tag
028700160929     C*
028800160929     C* Chiusura per scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
028900160929     C                   call      'TIS7T7ER'
029000160929     C                   parm      'C'           pIn_Opz           1
029100160929     C                   parm                    tivlrds
029200160929     C                   parm                    FNVAOEAR
029300160929     C                   parm                    oriPRG
029400160929     C                   parm      *zeros        pIn_Dst           3
029500160929     C                   parm      *blanks       pOut_Esito        1
029600010601     C
029700010601     C*
029800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
029900010601     C                   if        cntNonEl = *zeros or
030000010601     C                             flgMulti = '0'
030100010330     C* Se non ci sono record con errori ...
030200010601     C                   if        §ctrno = 0 and
030300010604     C                             §ctrmo = 0 and
030400010601     C                             flgStato <> '2'
030500010330     C* ... restituisco esito OK.
030600010330     C                   eval      wrkesito = '0'
030700010330     C                   else
030800010330     C                   if        §ctrok > 0
030900010330     C                   eval      wrkesito = '1'
031000010330     C                   else
031100010615     C                   if        flgOk = '0'
031200010615     C                   eval      wrkesito = '2'
031300010615     C                   else
031400010615     C                   eval      wrkesito = '6'
031500010615     C                   endif
031600010330     C                   endif
031700010330     C                   endif
031800010601     C                   else
031900010601     C                   eval      wrkesito = '9'
032000010601     C                   endif
032100010330     C*
032200010330     C                   if        %open(tivin00r)
032300010330     C                   close     tivin00r
032400010330     C                   endif
032500040708     C                   if        %open(fnvaowwr)
032600040708     C                   close     fnvaowwr
032700010330     C                   endif
032800010601     C*
032900010601     C                   if        vlrpoi <> 999
033000010601     C                   eval      invfil = vlrpoi
033100010601     C                   endif
033200010330     C*
033300010330     C                   if        §ctrok > 0
033400010601     C                             and invfil > *zeros
033500010330     C                   exsr      invio
033600160929     C*
033700160929     C* Invio dati estensione ORM (FNVAOE0F) => richiamo l'apposito driver
033800160929     C                   move      invfil        cmzdst
033900160929     C                   call      'TIS7T7ER'
034000160929     C                   parm      'S'           pIn_Opz           1
034100160929     C                   parm                    tivlrds
034200160929     C                   parm                    FNVAOEAR
034300160929     C                   parm                    oriPRG
034400160929     C                   parm      cmzdst        pIn_Dst           3
034500160929     C                   parm      *blanks       pOut_Esito        1
034600010330     C                   endif
034700010601     C*
034800010618     C                   if        flgGiro = '1'
034900010601     C                   exsr      endela
035000010618     C                   endif
035100010330     C*
035200010330     C                   ENDSR
035300010330     C***
035400160929
035500160929
035600160929     C*------------------------------------------------------------------------*
035700160929     C* RTVNUM - Stacco un numeratore univoco "ORM"
035800160929     C*------------------------------------------------------------------------*
035900160929     C     RTVNUM        BEGSR
036000160929     C*
036100160929     C* Stacco un numeratore da AZNUM
036200160929     C                   clear                   §numori
036300160929     C                   clear                   trul33ds
036400160929     C                   eval      I33OPE = *zeros
036500160929     C                   eval      I33CNU = 600
036600160929     C                   eval      I33NUM = 1
036700160929     C                   movel     TRUL33DS      KPJBU
036800160929     C                   call      'TRUL33R'
036900160929     C                   parm                    KPJBA
037000160929     C                   movel     KPJBU         TRUL33DS
037100160929     C                   if        O33ERR = *zeros
037200160929     C                   z-add     O33NRF        §numori
037300160929     c                   else
037400160929     c                   eval      §numori = *all'9'
037500160929     C                   seton                                        31        * errore bloccante
037600160929     C                   endif
037700160929     C*
037800160929     C                   eval      vaoNOR = §numori
037900160929     C*
038000160929     C                   ENDSR
038100160929
038200160929
038300160929     C*------------------------------------------------------------------------*
038400160929     C* EXEFORZ - Effettuo forzature
038500160929     C*------------------------------------------------------------------------*
038600160929     C     EXEFORZ       BEGSR
038700160929     C*
038800160929     C                   move(p)   VAOPOE        w_vaopoe                       * forzatura necessar
038900160929     C                   move(p)   VAOCOR        w_vaocor                       * forzatura necessar
039000160929     C                   move(p)   VAONOR        w_vaonor                       * forzatura necessar
039100160929     C*
039200160929     C                   ENDSR
039300160929
039400040723
039500040723
039600040723     C*------------------------------------------------------------------------*
039700040723     C* CHKCOR - Considerazioni su VAOCOR
039800040723     C*------------------------------------------------------------------------*
039900040723     C     CHKCOR        BEGSR
040000040723     C*
040100040723     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
040200040723     C                   if        vaoTCO = 'F'
040300040723     C* Quindi verifico se il codice ordinante non è valorizzato
040400040723     C                   if        vaoCOR = *zeros  AND
040500040723     C                             vaoRSO = *blanks AND
040600040723     C                             vaoINO = *blanks AND
040700040723     C                             vaoCAO = *blanks AND
040800040723     C                             vaoLOO = *blanks AND
040900040723     C                             vaoPRO = *blanks AND
041000040723     C                             vaoNAO = *blanks
041100040723     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
041200040723     C                   movel     '0'           wFlgAcr           1
041300040723     C                   move      vlrKSC        wCli              7 0
041400040723     C                   move(p)   wCli          acrCRO
041500040723     C     acrCRO        setll     fnacr01l
041600040723     C                   if        %found(fnacr01l)
041700040723     C                   read      fnacr01l
041800040723     C                   dow       not %eof(fnacr01l)
041900040723     C                   movel     acrCRO        wCliAcr           7 0
042000040723     C                   if        wCliAcr = wCli
042100040723     C                   eval      vaoCOR = acrCRO
042200040723     C                   movel     '1'           wFlgAcr
042300040723     C                   leave
042400040723     C                   else
042500040723     C                   leave
042600040723     C                   endif
042700040723     C                   read      fnacr01l
042800040723     C                   enddo
042900040723     C                   endif
043000040723     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
043100040723     C* dati anagrafici dell'ordinante dal piano dei conti
043200040723     C                   if        wFlgAcr = '0'
043300040723     C                   clear                   BS69DS
043400040723     C                   clear                   ACODS
043500040723     C                   clear                   INDDS
043600040723     C                   clear                   CLPDS
043700040723     C                   clear                   CLSDS
043800040723     C                   move(P)   vlrKSC        I69KAC
043900040723     C                   move(P)   vlrKSC        I69KIN
044000040723     C                   call      'TIBS69R'
044100040723     C                   parm                    BS69DS
044200040723     C                   parm                    ACODS
044300040723     C                   parm                    INDDS
044400040723     C                   parm                    CLPDS
044500040723     C                   parm                    CLSDS
044600040723     C     O69ERR        ifne      '1'
044700040723     C                   eval      vaoRSO = ACORAG
044800040723     C                   eval      vaoINO = INDVIA
044900040723     C                   movel(P)  INDCAP        vaoCAO
045000040723     C                   eval      vaoLOO = INDCIT
045100040723     C                   eval      vaoPRO = INDPRV
045200040723     C                   eval      vaoNAO = INDSTA
045300040723     C                   endif
045400040723     C                   endif
045500040723     C                   endif
045600040723     C                   endif
045700040723     C*
045800040723     C                   ENDSR
045900040723
046000040723
046100040723     C*------------------------------------------------------------------------*
046200040723     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
046300040723     C*------------------------------------------------------------------------*
046400040723     C     WRIORI        BEGSR
046500040723     C*
046600090618     C* Come prima cosa stacco un numeratore da AZNUM
046700160929     C***                clear                   trul33ds
046800160929     C***                eval      I33OPE = *zeros
046900160929     C***                eval      I33CNU = 600
047000160929     C***                eval      I33NUM = 1
047100160929     C***                movel     TRUL33DS      KPJBU
047200160929     C***                call      'TRUL33R'
047300160929     C***                parm                    KPJBA
047400160929     C***                movel     KPJBU         TRUL33DS
047500160929     C***                if        O33ERR = *zeros
047600160929     C***                z-add     O33NRF        §numori
047700160929     c***                else
047800160929     c***                eval      §numori = *all'9'
047900160929     C***                endif
048000040723     C*
048100040723     C* Quindi imposto i campi "extra-VAO"
048200040723     C                   if        vlrKSC = '0ORM0000'
048300040723     C*
048400040723     C                   if        vaoCOR > *zeros
048500040723     C                   movel     vaoCOR        wrkCOR7           7 0
048600040723     C*
048700040723     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
048800040723     C* codice padre. Se è un padre assegno già il codice ordinante.
048900040723     C                   clear                   tibs10ds
049000040723     C                   eval      d10drf = datcor                              *data riferimento
049100040723     C                   eval      d10paf = 'P'                                 *cerca il padre
049200040723     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
049300091026     C*
049400091026     C* Preparazione chiamata al TIBS10R1
049500091026     C                   clear                   tibs10r1ds
049600091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
049700091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
049800091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
049900091026     C*
050000091026     C                   call      'TIBS10R1'
050100091026     C                   parm                    tibs10r1ds
050200091026     C*
050300091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
050400040723     C*
050500040723     C                   if        d10err <> *blanks or                         *No unif. o padre
050600040723     C                             d10cop =  *zeros  or
050700040723     C                             d10cop =  d10cod
050800040723     C                   move      d10cod        oriIDC
050900040723     C                   else                                                   *Figlio
051000040723     C                   move      d10cop        oriIDC
051100040723     C                   endif
051200040723     C*
051300040723     C                   else
051400040723     C                   eval      oriIDC = *zeros
051500040723     C                   endif
051600040723     C*
051700040723     C                   else
051800040723     C                   eval      oriIDC = vlrKSC
051900040723     C                   endif
052000040723     C*
052100040723     C                   eval      oriPRG = §numori
052200040723     C                   eval      oriDER = datcor
052300040723     C                   eval      oriOER = wn6
052400040723     C                   eval      oriPOE = vaoPOE
052500040723     C                   eval      oriNSR = vaoNSR
052600040723     C                   eval      oriNOR = vaoNOR
052700040723     C                   eval      oriNRV = vaoNRV
052800040723     C                   eval      oriFLO = *blanks
052900040723     C*
053000040723     C* Infine valorizzo la chiave esterna sul file FNVAO
053100040723     C                   eval      dorm01 = vaoFLO
053200040723     C                   movel     §numori       §ormpg
053300130603     C                   EVAL      §ORCOMC = 'N'
053400050412     C*
053500050412     C* Infine valorizzo i flag operativi necessari x la particolare gestione corrente
053600130326     C*                   in questo caso non ci sono default
053700050412     C*
053800130326     C* Riassegno il campo VAO contenete i flag operativi precedentemente ridefiniti
053900040723     C                   eval      vaoFLO = dorm01
054000130326     C* Infine, per sicurezza, pulisco alcuni flag che devono essere a blank
054100130326     C                   eval      %subst(VAOFLO:5:1) = *blanks
054200130326     C                   eval      %subst(VAOFLO:6:1) = *blanks
054300040723     C*
054400040723     C* ...quindi scrivo il file TIORI00F
054500120905     C  N55              WRITE     TIORI000
054600040723     C*
054700040723     C                   ENDSR
054800050826
054900050826
055000050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
055100050826     C     wriVGD        BEGSR
055200050826     C*
055300050826     C* Reperisco la descrizione della fase da tabella
055400050826     C                   movel(p)  'FAR'         KteCOD
055500050826     C                   movel(p)  '000'         KteKE1
055600050826     C     KEYtbe        chain     tntbe01l
055700050826     C                   if        %found(tntbe01l)
055800050826     C                   movel(P)  tbeuni        DFAR
055900050826     C                   else
056000050826     C                   clear                   DFAR
056100050826     C                   endif
056200050826     C*
056300050826     C* Valorizzo buffer record
056400050826     C                   clear                   FNVAPDS
056500050826     C                   eval      vapIDC = oriIDC
056600050826     C                   eval      vapPOE = oriPOE
056700050826     C                   eval      vapRFA = vaoRFA
056800050826     C                   eval      vapPOG = oriPOE
056900050826     C                   eval      vapDAE = oriDER
057000050826     C                   eval      vapORE = oriOER
057100050826     C                   if        d§fardva <> *blanks
057200050826     C                   eval      vapDFA = d§fardva
057300050826     C                   else
057400050826     C                   eval      vapDFA = d§fardes
057500050826     C                   endif
057600050826     C*
057700050826     C                   clear                   tivgd000
057800080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
057900050826     C                   eval      vgdTIP = 'VP'
058000050826     C                   eval      vgdKSU = vapIDC
058100050826     C                   eval      vgdTSC = 'WW'
058200050826     C                   eval      vgdDAT = datcor
058300151207     C                   eval      vgdPGM = procname
058400060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
058500060306     C                   exsr      sr_Control
058600060306     C                   if        Cli_Abil = *on
058700060306     C                   eval      vgdKSU = woriIDC
058800120905     C  N55              write     tivgd000
058900060306     C                   endif
059000050826     C*
059100050826     C                   ENDSR
059200050826     C*------------------------------------------------------------------------*
059300010601
059400010601
059500010330     C*----------------------------------------------------*
059600030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
059700010330     C*----------------------------------------------------*
059800010330     C     INZVAR        BEGSR
059900010330     C*
060000030822     C* Inizializzo variabili di wrk
060100020204     C                   Z-ADD     *zeros        Num5_0            5 0
060200120124
060300030822     C*
060400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
060500120123     C                   CLEAR                   FNVAO000
060600160929     C                   CLEAR                   FNVAOEAR
060700120123     C                   CLEAR                   TIORI000
060800030822     C                   CLEAR                   SkSplitCSV
060900030822     C                   EVAL      i = 1
061000030822     C                   EVAL      posDa = *zeros
061100030822     C                   EVAL      posA  = *zeros
061200160929     C*
061300160929     C* Forzatura campo FNVAO
061400160929     C                   EVAL      VAOTCO = 'F'
061500030822     C*
061600030822     C* Reimposto i valori di default
061700030822     C                   EXSR      DEFCAM
061800030822     C*
061900010330     C                   ENDSR
062000010330     C*----------------------------------------------------*
062100030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
062200010330     C*----------------------------------------------------*
062300010330     C     DEFCAM        BEGSR
062400030822     C*
062500030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
062600030715     C* e delimitatore testo.
062700030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
062800030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
062900030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
063000040723     C*
063100080707     C* Imposto i valori d default d alcuni campi
063200080708     C*
063300080708     C                   CLEAR                   DORM01
063400120822     C                   EVAL      VAOFLO = DORM01
063500080707     C*
063600130603     C                   EVAL      VAOTOR = 'S'
063700160719     C                   EVAL      VAOCOR = 0898884000
063800130716     C                   EVAL      VAOPAG = 'O'
063900160719     C                   EVAL      VAOPOE = 089
064000160719     C                   EVAL      VAOKSC = 0898884
064100130716     C                   EVAL      VAOCTR = '000'
064200130603     C                   EVAL      VAOORR = 1400
064300130513
064400120905     C* se il 1° char dei parametri è T=Test
064500120905     C                   SETOFF                                       55
064600120905     C                   IF        %subst(vlrppt:1:1) = 'T'
064700120905     C* inibisco la scrittura dei file TIORI e TIVGD
064800120905     C                   SETON                                        55
064900120905     C* imposto il PO di emissione come filiale fittizia
065000120905     C                   EVAL      VAOPOE = 999
065100120905     C                   ENDIF
065200091127     C*
065300091204     C* IMPOSTO I CAMPI PERSONALIZZATI
065400100416     C
065500030822     C*
065600030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
065700030822     C                   EVAL      posDaDft = 1
065800030822     C                   EVAL      posADft  = 0
065900030822     C                   EVAL      wGiroDft = 0
066000030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
066100030822     C                             posDaDft > 0
066200030822     C*
066300030822     C* Gestisco il 1° giro
066400030822     C                   IF        wGiroDft = 0
066500030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
066600030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
066700030822     C* Incremento il contatore dei "giri"
066800030822     C                   EVAL      wGiroDft = 1
066900030822     C                   ELSE
067000030822     C                   EVAL      posDaDft = posADft
067100030822     C                   ENDIF
067200030822     C* Eseguo lo scan x trovare la fine del campo corrente
067300030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
067400030822     C*
067500030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
067600030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
067700030822     C                   IF        posDaDft > 0 AND
067800030822     C                             posADft  > 0
067900040708     C* POE
068000040708     C                   IF        %subst(
068100040708     C                             %subst(vlrppt:posDaDft+1:
068200040708     C                             posADft-posDaDft-1):1:3)
068300040708     C                             = 'POE'
068400040708     C                   EVAL      PiStr=%trim(%subst(
068500040708     C                             %subst(vlrppt:posDaDft+1:
068600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068700040708     C                   EXSR      CHKNUM
068800040708     C                   IF        PiInt=*on
068900040708     C                   Z-ADD     PiVal         VAOPOE
069000040708     C                   ENDIF
069100040708     C                   ENDIF
069200040708     C* TOR
069300040708     C                   IF        %subst(
069400040708     C                             %subst(vlrppt:posDaDft+1:
069500040708     C                             posADft-posDaDft-1):1:3)
069600040708     C                             = 'TOR'
069700040708     C                   EVAL      VAOTOR=%trim(%subst(
069800040708     C                             %subst(vlrppt:posDaDft+1:
069900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070000040708     C                   ENDIF
070100040708     C* TCO
070200040708     C                   IF        %subst(
070300040708     C                             %subst(vlrppt:posDaDft+1:
070400040708     C                             posADft-posDaDft-1):1:3)
070500040708     C                             = 'TCO'
070600040708     C                   EVAL      VAOTCO=%trim(%subst(
070700040708     C                             %subst(vlrppt:posDaDft+1:
070800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070900040708     C                   ENDIF
071000040707     C* COR
071100030822     C                   IF        %subst(
071200030822     C                             %subst(vlrppt:posDaDft+1:
071300030822     C                             posADft-posDaDft-1):1:3)
071400040707     C                             = 'COR'
071500030822     C                   EVAL      PiStr=%trim(%subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071800030822     C                   EXSR      CHKNUM
071900030822     C                   IF        PiInt=*on
072000040708     C                   Z-ADD     PiVal         VAOCOR
072100030822     C                   ENDIF
072200030822     C                   ENDIF
072300040708     C* CRC
072400040708     C                   IF        %subst(
072500040708     C                             %subst(vlrppt:posDaDft+1:
072600040708     C                             posADft-posDaDft-1):1:3)
072700040708     C                             = 'CRC'
072800040708     C                   EVAL      PiStr=%trim(%subst(
072900040708     C                             %subst(vlrppt:posDaDft+1:
073000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073100040708     C                   EXSR      CHKNUM
073200040708     C                   IF        PiInt=*on
073300040708     C                   Z-ADD     PiVal         VAOCRC
073400040708     C                   ENDIF
073500040708     C                   ENDIF
073600040708     C* FFD
073700040708     C                   IF        %subst(
073800040708     C                             %subst(vlrppt:posDaDft+1:
073900040708     C                             posADft-posDaDft-1):1:3)
074000040708     C                             = 'FFD'
074100040708     C                   EVAL      VAOFFD=%trim(%subst(
074200040708     C                             %subst(vlrppt:posDaDft+1:
074300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074400040708     C                   ENDIF
074500040708     C* ORR
074600040708     C                   IF        %subst(
074700040708     C                             %subst(vlrppt:posDaDft+1:
074800040708     C                             posADft-posDaDft-1):1:3)
074900040708     C                             = 'ORR'
075000040708     C                   EVAL      PiStr=%trim(%subst(
075100040708     C                             %subst(vlrppt:posDaDft+1:
075200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075300040708     C                   EXSR      CHKNUM
075400040708     C                   IF        PiInt=*on
075500040708     C                   Z-ADD     PiVal         VAOORR
075600040708     C                   ENDIF
075700040708     C                   ENDIF
075800040712     C* NCL
075900040712     C                   IF        %subst(
076000040712     C                             %subst(vlrppt:posDaDft+1:
076100040712     C                             posADft-posDaDft-1):1:3)
076200040712     C                             = 'NCL'
076300040712     C                   EVAL      PiStr=%trim(%subst(
076400040712     C                             %subst(vlrppt:posDaDft+1:
076500040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076600040712     C                   EXSR      CHKNUM
076700040712     C                   IF        PiInt=*on
076800040712     C                   Z-ADD     PiVal         VAONCL
076900040712     C                   ENDIF
077000040712     C                   ENDIF
077100040712     C* PKG
077200040712     C                   IF        %subst(
077300040712     C                             %subst(vlrppt:posDaDft+1:
077400040712     C                             posADft-posDaDft-1):1:3)
077500040712     C                             = 'PKG'
077600040712     C                   EVAL      PiStr=%trim(%subst(
077700040712     C                             %subst(vlrppt:posDaDft+1:
077800040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077900040712     C                   EXSR      CHKNUM
078000040712     C                   IF        PiNum=*on
078100040712     C                   Z-ADD     PiVal         VAOPKG
078200040712     C                   ENDIF
078300040712     C                   ENDIF
078400040712     C* VLM
078500040712     C                   IF        %subst(
078600040712     C                             %subst(vlrppt:posDaDft+1:
078700040712     C                             posADft-posDaDft-1):1:3)
078800040712     C                             = 'VLM'
078900040712     C                   EVAL      PiStr=%trim(%subst(
079000040712     C                             %subst(vlrppt:posDaDft+1:
079100040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079200040712     C                   EXSR      CHKNUM
079300040712     C                   IF        PiNum=*on
079400040712     C                   Z-ADD     PiVal         VAOVLM
079500040712     C                   ENDIF
079600040712     C                   ENDIF
079700040708     C* NAM
079800040708     C                   IF        %subst(
079900040708     C                             %subst(vlrppt:posDaDft+1:
080000040708     C                             posADft-posDaDft-1):1:3)
080100040708     C                             = 'NAM'
080200040708     C                   EVAL      VAONAM=%trim(%subst(
080300040708     C                             %subst(vlrppt:posDaDft+1:
080400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080500040708     C                   ENDIF
080600040708     C* PAG
080700030822     C                   IF        %subst(
080800030822     C                             %subst(vlrppt:posDaDft+1:
080900030822     C                             posADft-posDaDft-1):1:3)
081000040708     C                             = 'PAG'
081100040708     C                   EVAL      VAOPAG=%trim(%subst(
081200030822     C                             %subst(vlrppt:posDaDft+1:
081300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081400030822     C                   ENDIF
081500040708     C* KSC
081600040708     C                   IF        %subst(
081700040708     C                             %subst(vlrppt:posDaDft+1:
081800040708     C                             posADft-posDaDft-1):1:3)
081900040708     C                             = 'KSC'
082000040708     C                   EVAL      PiStr=%trim(%subst(
082100040708     C                             %subst(vlrppt:posDaDft+1:
082200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082300040708     C                   EXSR      CHKNUM
082400040708     C                   IF        PiInt=*on
082500040708     C                   Z-ADD     PiVal         VAOKSC
082600040708     C                   ENDIF
082700040708     C                   ENDIF
082800040708     C* CTR
082900040708     C                   IF        %subst(
083000040708     C                             %subst(vlrppt:posDaDft+1:
083100040708     C                             posADft-posDaDft-1):1:3)
083200040708     C                             = 'CTR'
083300040708     C                   EVAL      VAOCTR=%trim(%subst(
083400040708     C                             %subst(vlrppt:posDaDft+1:
083500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083600040708     C                   ENDIF
083700040708     C* POC
083800040708     C                   IF        %subst(
083900040708     C                             %subst(vlrppt:posDaDft+1:
084000040708     C                             posADft-posDaDft-1):1:3)
084100040708     C                             = 'POC'
084200040708     C                   EVAL      PiStr=%trim(%subst(
084300040708     C                             %subst(vlrppt:posDaDft+1:
084400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084500040708     C                   EXSR      CHKNUM
084600040708     C                   IF        PiInt=*on
084700040708     C                   Z-ADD     PiVal         VAOPOC
084800040708     C                   ENDIF
084900040708     C                   ENDIF
085000040708     C* NO1
085100040708     C                   IF        %subst(
085200040708     C                             %subst(vlrppt:posDaDft+1:
085300121129     C                             posADft-posDaDft-1):1:3)
085400040708     C                             = 'NO1'
085500040708     C                   EVAL      VAONO1=%trim(%subst(
085600040708     C                             %subst(vlrppt:posDaDft+1:
085700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085800040708     C                   ENDIF
085900040708     C* DDT
086000030822     C                   IF        %subst(
086100030822     C                             %subst(vlrppt:posDaDft+1:
086200030822     C                             posADft-posDaDft-1):1:3)
086300040708     C                             = 'DDT'
086400040708     C                   EVAL      VAODDT=%trim(%subst(
086500030822     C                             %subst(vlrppt:posDaDft+1:
086600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086700030822     C                   ENDIF
086800040708     C* RFA
086900040708     C                   IF        %subst(
087000040708     C                             %subst(vlrppt:posDaDft+1:
087100040708     C                             posADft-posDaDft-1):1:3)
087200040708     C                             = 'RFA'
087300040708     C                   EVAL      VAORFA=%trim(%subst(
087400040708     C                             %subst(vlrppt:posDaDft+1:
087500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
087600040708     C                   ENDIF
087700040708     C* FLO
087800040708     C                   IF        %subst(
087900040708     C                             %subst(vlrppt:posDaDft+1:
088000040708     C                             posADft-posDaDft-1):1:3)
088100040708     C                             = 'FLO'
088200040708     C                   EVAL      VAOFLO=%trim(%subst(
088300040708     C                             %subst(vlrppt:posDaDft+1:
088400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
088500091103     C*
088600040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
088700080702     C* e posizione 7 del VAOFLO
088800040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
088900040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
089000050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
089100040708     C                   ENDIF
089200030822     C* ...
089300030822     C                   ENDIF
089400030822     C                   ENDDO
089500020204     C*
089600010330     C                   ENDSR
089700010607     C*----------------------------------------------------*
089800040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
089900010607     C*----------------------------------------------------*
090000040708     C     IMPVAO        BEGSR
090100010607     C*
090200010607     C                   SETOFF                                       3132
090300120124
090400030515     C*
090500040708     C                   MOVEL     datcor        VAODAO
090600040708     C                   MOVEL     wn14          VAOOAO
090700050314     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
090800050314     C     minu:maiu     XLATE     vindta        vindta
090900010607     C*
091000030715     C**********
091100030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
091200030715     C**********
091300030715     C                   IF        wGiro = *zeros
091400040709     C*
091500040709     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
091600040709     C                   EVAL      depvinDTA = vinDTA
091700040709     C*
091800040709     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
091900040709     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
092000040709     C                                      %trim(intcol(3))
092100031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
092200050314     C**!!!minu:maiu     XLATE     vindta        vindta
092300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
092400040324     C                   DOW       posDa <= %len(%trimr(vindta))
092500030715     C*
092600030715     C* Gestisco il 1° campo
092700030715     C                   IF        i = 1
092800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
092900030715     C                   EVAL      posDa = 1
093000030715     C* Eseguo lo scan x trovare la fine del primo campo
093100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
093200030715     C* A questo punto "estraggo" il campo corrente
093300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
093400030715     C                                                    (posA-posDa))
093500030715     C* X i campi successivi al 1°
093600030715     C                   ELSE
093700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
093800030715     C                   EVAL      posDa = posA + 1
093900030715     C* Eseguo lo scan x trovare la fine del campo corrente
094000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094100030715     C* Gestisco l'ultimo campo
094200030715     C                   IF        posA = *zeros
094300040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
094400030715     C* A questo punto "estraggo" il campo corrente
094500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
094600030715     C                                                    (posA-posDa))
094700030715     C                   LEAVE
094800030715     C                   ELSE
094900030715     C* A questo punto "estraggo" il campo corrente
095000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
095100030715     C                                                    (posA-posDa))
095200030715     C                   ENDIF
095300030715     C                   ENDIF
095400030715     C* Incremento il contatore d campo
095500030715     C                   EVAL      i = i +1
095600030715     C                   ENDDO
095700030715     C*
095800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
095900030715     C* delimitazione testo
096000030715     C                   EVAL      i = 1
096100030715     C                   DOW       i < %elem(SkSplitFLD)
096200030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
096300030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
096400030715     C                   EVAL      i = i + 1
096500030715     C                   ENDDO
096600030715     C                   EVAL      wGiro = 1
096700040709     C* Ripristino la 1° riga originale
096800040709     C                   EVAL      vinDTA = depvinDTA
096900030715     C                   ELSE
097000030715     C                   EVAL      wGiro = 2
097100031204     C**********
097200031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
097300031204     C* d separatore campo
097400031204     C**********
097500040709     C                   EVAL      vindta = %trim(vindta)
097600040709     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
097700040709     C                   EVAL      vindta = %subst(vindta:2)
097800040709     C                   ENDDO
097900040709     C*
098000040709     C                   Z-ADD     *zeros        lunghInput        4 0
098100040709     C                   EVAL      lunghInput = %len(%trim(vindta))
098200040709     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
098300040709     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
098400040709     C                   EVAL      lunghInput = %len(%trim(vindta))
098500040709     C                   ENDDO
098600030715     C**********
098700030715     C* Effettuo lo split del campo dati d input
098800030715     C**********
098900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
099000040324     C                   DOW       posDa <= %len(%trimr(vindta))
099100030715     C*
099200030715     C* Gestisco il 1° campo
099300030715     C                   IF        i = 1
099400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
099500030715     C                   EVAL      posDa = 1
099600030715     C* Eseguo lo scan x trovare la fine del primo campo
099700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
099800030715     C* A questo punto "estraggo" il campo corrente
099900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
100000030715     C                                                    (posA-posDa))
100100030715     C* X i campi successivi al 1°
100200030715     C                   ELSE
100300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
100400030715     C                   EVAL      posDa = posA + 1
100500031204     C* Verifico che nn vi sia il campo nn valorizzato
100600031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
100700031204     C* Se campo nn valorizzato skippo al prossimo
100800031204     C                   EVAL      SkSplitCSV(i) = *blanks
100900031204     C                   EVAL      posA  = posA + 1
101000031204     C                   ELSE
101100030715     C* Eseguo lo scan x trovare la fine del campo corrente
101200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
101300030715     C* Gestisco l'ultimo campo
101400030715     C                   IF        posA = *zeros
101500040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
101600030715     C* A questo punto "estraggo" il campo corrente
101700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
101800030715     C                                                    (posA-posDa))
101900030715     C                   LEAVE
102000030715     C                   ELSE
102100030715     C* A questo punto "estraggo" il campo corrente
102200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
102300030715     C                                                    (posA-posDa))
102400030715     C                   ENDIF
102500031204     C                   ENDIF
102600030715     C                   ENDIF
102700030715     C* Incremento il contatore d campo
102800030715     C                   EVAL      i = i +1
102900030715     C                   ENDDO
103000030715     C*
103100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
103200030715     C* delimitazione testo
103300030715     C                   EVAL      i = 1
103400030715     C                   DOW       i < %elem(SkSplitCSV)
103500030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
103600030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
103700030715     C                   EVAL      i = i + 1
103800030715     C                   ENDDO
103900030715     C*
104000030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
104100030715     C                   EVAL      i = 1
104200030715     C                   DOW       i < %elem(SkSplitFLD)
104300030715     C*
104400030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
104500030715     C                   IF        SkSplitFLD(i) <> *blanks AND
104600030715     C                             SkSplitCSV(i) <> *blanks
104700030721     C*** ATB
104800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATB'
104900040708     C                   EVAL      VAOATB = SkSplitCSV(i)
105000030721     C                   ENDIF
105100040708     C*** TOR
105200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTOR'
105300040708     C                   EVAL      VAOTOR = SkSplitCSV(i)
105400040708     C                   ENDIF
105500040708     C*** TCO
105600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTCO'
105700040708     C                   EVAL      VAOTCO = SkSplitCSV(i)
105800040708     C                   ENDIF
105900040708     C*** RSO
106000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSO'
106100040708     C                   EVAL      VAORSO = SkSplitCSV(i)
106200030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
106300040708     C     '@':'A'       XLATE     VAORSO        VAORSO
106400030715     C* ==
106500030715     C                   ENDIF
106600040708     C*** INO
106700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINO'
106800040708     C                   EVAL      VAOINO = SkSplitCSV(i)
106900030715     C                   ENDIF
107000040708     C*** LOO
107100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOO'
107200040708     C                   EVAL      VAOLOO = SkSplitCSV(i)
107300030715     C                   ENDIF
107400040708     C*** PRO
107500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRO'
107600040708     C                   EVAL      VAOPRO = SkSplitCSV(i)
107700030715     C                   ENDIF
107800040708     C*** NAO
107900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAO'
108000040708     C                   EVAL      VAONAO = SkSplitCSV(i)
108100030721     C                   ENDIF
108200040708     C*** RSR
108300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSR'
108400040708     C                   EVAL      VAORSR = SkSplitCSV(i)
108500040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
108600040708     C     '@':'A'       XLATE     VAORSR        VAORSR
108700040708     C* ==
108800040708     C                   IF        VAORSR = *blanks
108900040708     C                   SETON                                        31
109000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
109100040708     C                             + ' ' + 'VAORSR'
109200040708     C                   ENDIF
109300040708     C                   ENDIF
109400040708     C*** INR
109500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINR'
109600040708     C                   EVAL      VAOINR = SkSplitCSV(i)
109700040708     C                   IF        VAOINR = *blanks
109800040708     C                   SETON                                        31
109900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
110000040708     C                             + ' ' + 'VAOINR'
110100040708     C                   ENDIF
110200040708     C                   ENDIF
110300040708     C*** LOR
110400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOR'
110500040708     C                   EVAL      VAOLOR = SkSplitCSV(i)
110600040708     C                   IF        VAOLOR = *blanks
110700040708     C                   SETON                                        31
110800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
110900040708     C                             + ' ' + 'VAOLOR'
111000040708     C                   ENDIF
111100040708     C                   ENDIF
111200040708     C*** PRR
111300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRR'
111400040708     C                   EVAL      VAOPRR = SkSplitCSV(i)
111500040708     C                   IF        VAOPRR = *blanks
111600040708     C                   SETON                                        31
111700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
111800040708     C                             + ' ' + 'VAOPRR'
111900040708     C                   ENDIF
112000040708     C                   ENDIF
112100040708     C*** NAR
112200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAR'
112300040708     C                   EVAL      VAONAR = SkSplitCSV(i)
112400040708     C                   ENDIF
112500040708     C*** RER
112600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORER'
112700040708     C                   EVAL      VAORER = SkSplitCSV(i)
112800040708     C                   ENDIF
112900040708     C*** TER
113000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTER'
113100040708     C                   EVAL      VAOTER = SkSplitCSV(i)
113200040708     C                   ENDIF
113300040708     C*** RSC
113400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSC'
113500040708     C                   EVAL      VAORSC = SkSplitCSV(i)
113600040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
113700040708     C     '@':'A'       XLATE     VAORSC        VAORSC
113800040708     C* ==
113900040708     C                   ENDIF
114000040708     C*** INC
114100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINC'
114200040708     C                   EVAL      VAOINC = SkSplitCSV(i)
114300040708     C                   ENDIF
114400040708     C*** LOC
114500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOC'
114600040708     C                   EVAL      VAOLOC = SkSplitCSV(i)
114700040708     C                   ENDIF
114800040708     C*** PRC
114900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRC'
115000040708     C                   EVAL      VAOPRC = SkSplitCSV(i)
115100040708     C                   ENDIF
115200040708     C*** NAC
115300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAC'
115400040708     C                   EVAL      VAONAC = SkSplitCSV(i)
115500040708     C                   ENDIF
115600040708     C*** FFD
115700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFFD'
115800040708     C                   EVAL      VAOFFD = SkSplitCSV(i)
115900040708     C                   ENDIF
116000040708     C*** RMP
116100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORMP'
116200040708     C                   EVAL      VAORMP = SkSplitCSV(i)
116300040708     C                   ENDIF
116400040708     C*** NAM
116500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAM'
116600040708     C                   EVAL      VAONAM = SkSplitCSV(i)
116700040708     C                   ENDIF
116800040708     C*** PAG
116900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPAG'
117000040708     C                   EVAL      VAOPAG = SkSplitCSV(i)
117100040708     C                   ENDIF
117200040708     C*** CTR
117300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCTR'
117400040708     C                   EVAL      VAOCTR = SkSplitCSV(i)
117500040708     C                   ENDIF
117600040708     C*** ZOR
117700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOZOR'
117800040708     C                   EVAL      VAOZOR = SkSplitCSV(i)
117900040708     C                   ENDIF
118000040708     C*** NO1
118100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO1'
118200040708     C                   EVAL      VAONO1 = SkSplitCSV(i)
118300030715     C                   ENDIF
118400040708     C*** NO2
118500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO2'
118600040708     C                   EVAL      VAONO2 = SkSplitCSV(i)
118700030715     C                   ENDIF
118800040708     C*** DDT
118900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODDT'
119000040708     C                   EVAL      VAODDT = SkSplitCSV(i)
119100030715     C                   ENDIF
119200040708     C*** RFA
119300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORFA'
119400040708     C                   EVAL      VAORFA = SkSplitCSV(i)
119500030715     C                   ENDIF
119600040708     C*** SPI
119700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSPI'
119800040708     C                   EVAL      VAOSPI = SkSplitCSV(i)
119900030715     C                   ENDIF
120000040708     C*** FLO
120100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
120200040708     C                   EVAL      VAOFLO = SkSplitCSV(i)
120300040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
120400080708     C* e la posizione 7 del VAOFLO
120500040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
120600040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
120700050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
120800030715     C                   ENDIF
120900160929     C*** FLO2
121000160929     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO2'
121100160929     C                   EVAL      w_VAOFLO2 = SkSplitCSV(i)
121200160929     C                   ENDIF
121300160929     C*** TSPS
121400160929     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTSPS'
121500160929     C                   EVAL      w_VAOTSPS = SkSplitCSV(i)
121600160929     C                   ENDIF
121700160929     C*** NOTE
121800160929     C                   IF        %trim(SkSplitFLD(i)) = 'VAONOTE'
121900160929     C                   EVAL      w_VAONOTE = SkSplitCSV(i)
122000160929     C                   ENDIF
122100160929     C*** ORDA1
122200160929     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORDA1'
122300160929     C                   EVALR     w_VAOORDA1 = %trim(SkSplitCSV(i))
122400160929     C                   ENDIF
122500160929     C*** ORAA1
122600160929     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORAA1'
122700160929     C                   EVALR     w_VAOORAA1 = %trim(SkSplitCSV(i))
122800160929     C                   ENDIF
122900160929     C*** ORDA2
123000160929     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORDA2'
123100160929     C                   EVALR     w_VAOORDA2 = %trim(SkSplitCSV(i))
123200160929     C                   ENDIF
123300160929     C*** ORAA2
123400160929     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORAA2'
123500160929     C                   EVALR     w_VAOORAA2 = %trim(SkSplitCSV(i))
123600160929     C                   ENDIF
123700160929     C*** EMLR
123800160929     C                   IF        %trim(SkSplitFLD(i)) = 'VAOEMLR'
123900160929     C                   EVAL      w_VAOEMLR = SkSplitCSV(i)
124000160929     C                   ENDIF
124100160929     C*** EMLRF
124200160929     C                   IF        %trim(SkSplitFLD(i)) = 'VAOEMLRF'
124300160929     C                   EVAL      w_VAOEMLRF = SkSplitCSV(i)
124400160929     C                   ENDIF
124500160929     C*** SMSR
124600160929     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSMSR'
124700160929     C                   EVAL      w_VAOSMSR = SkSplitCSV(i)
124800160929     C                   ENDIF
124900160929     C*** SMSRF
125000160929     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSMSRF'
125100160929     C                   EVAL      w_VAOSMSRF = SkSplitCSV(i)
125200160929     C                   ENDIF
125300091103     C*** DAR
125400050314     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
125500050412     C                   EVAL      PiStr=SkSplitCSV(i)
125600050412     C                   EXSR      CHKNUM
125700050412     C                   IF        PiInt=*on
125800050412     C                   Z-ADD     PiVal         VAODAR
125900050412     C                   ELSE
126000050412     C                   SETON                                        32
126100050412     C                   EVAL      VAODAR = *zeros
126200050412     C                   EVAL      vinmsg = %trimr(vinmsg)
126300050412     C                             + ' ' + 'VAODAR'
126400050412     C                   ENDIF
126500050314     C                   ENDIF
126600030715     C
126700030721     C*
126800030721     C* Reperisco quindi i campi numerici...
126900040708     C*** POE
127000040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
127100040723     C                   EVAL      PiStr=SkSplitCSV(i)
127200040723     C                   EXSR      CHKNUM
127300040723     C                   IF        PiInt=*on    AND
127400040723     C                             PiVal<=999   AND
127500040723     C                             PiVal>*zeros
127600040723     C                   Z-ADD     PiVal         VAOPOE
127700040723     C                   ELSE
127800040723     C                   SETON                                        32
127900040723     C                   EVAL      VAOPOE = *zeros
128000040723     C                   EVAL      vinmsg = %trimr(vinmsg)
128100040723     C                             + ' ' + 'VAOPOE'
128200040723     C                   ENDIF
128300040723     C                   ENDIF
128400040708     C*** COR
128500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
128600040708     C                   EVAL      PiStr=SkSplitCSV(i)
128700040708     C                   EXSR      CHKNUM
128800040708     C                   IF        PiInt=*on
128900040708     C                   Z-ADD     PiVal         VAOCOR
129000040708     C                   ELSE
129100040708     C                   SETON                                        31
129200040708     C                   EVAL      VAOCOR = *zeros
129300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
129400040708     C                             + ' ' + 'VAOCOR'
129500040708     C                   ENDIF
129600040708     C                   ENDIF
129700040708     C*** CAO
129800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
129900030721     C                   EVAL      PiStr=SkSplitCSV(i)
130000030721     C                   EXSR      CHKNUM
130100030721     C                   IF        PiInt=*on
130200030721     C                   Z-ADD     PiVal         Num5_0
130300040708     C                   MOVEL(p)  Num5_0        VAOCAO
130400030721     C                   ELSE
130500030721     C                   SETON                                        32
130600040708     C                   EVAL      VAOCAO = *zeros
130700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130800040708     C                             + ' ' + 'VAOCAO'
130900030721     C                   ENDIF
131000030721     C                   ENDIF
131100040708     C*** CRA
131200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
131300030721     C                   EVAL      PiStr=SkSplitCSV(i)
131400030721     C                   EXSR      CHKNUM
131500030721     C                   IF        PiInt=*on
131600040708     C                   Z-ADD     PiVal         VAOCRA
131700030721     C                   ELSE
131800030721     C                   SETON                                        32
131900040708     C                   EVAL      VAOCRA = *zeros
132000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132100040708     C                             + ' ' + 'VAOCRA'
132200030721     C                   ENDIF
132300030721     C                   ENDIF
132400040708     C*** CAR
132500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
132600040708     C                   EVAL      PiStr=SkSplitCSV(i)
132700040708     C                   EXSR      CHKNUM
132800040708     C                   IF        PiInt=*on
132900040708     C                   Z-ADD     PiVal         Num5_0
133000040708     C                   MOVEL(p)  Num5_0        VAOCAR
133100040708     C                   ELSE
133200040708     C                   SETON                                        31
133300040708     C                   EVAL      VAOCAR = *zeros
133400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
133500040708     C                             + ' ' + 'VAOCAR'
133600040708     C                   ENDIF
133700040708     C                   ENDIF
133800040708     C*** CRC
133900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
134000040708     C                   EVAL      PiStr=SkSplitCSV(i)
134100040708     C                   EXSR      CHKNUM
134200040708     C                   IF        PiInt=*on
134300040708     C                   Z-ADD     PiVal         VAOCRC
134400040708     C                   ELSE
134500040708     C                   SETON                                        32
134600040708     C                   EVAL      VAOCRC = *zeros
134700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
134800040708     C                             + ' ' + 'VAOCRC'
134900040708     C                   ENDIF
135000040708     C                   ENDIF
135100040708     C*** CAC
135200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
135300040708     C                   EVAL      PiStr=SkSplitCSV(i)
135400040708     C                   EXSR      CHKNUM
135500040708     C                   IF        PiInt=*on
135600040708     C                   Z-ADD     PiVal         Num5_0
135700040708     C                   MOVEL(p)  Num5_0        VAOCAC
135800040708     C                   ELSE
135900040708     C                   SETON                                        32
136000040708     C                   EVAL      VAOCAC = *zeros
136100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
136200040708     C                             + ' ' + 'VAOCAC'
136300040708     C                   ENDIF
136400040708     C                   ENDIF
136500040708     C*** DAR
136600090428     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
136700090428     C                   EVAL      PiStr=SkSplitCSV(i)
136800090428     C                   EXSR      CHKNUM
136900090428     C                   IF        PiInt=*on
137000090428     C                   Z-ADD     PiVal         VAODAR
137100090428     C                   ELSE
137200090428     C                   SETON                                        32
137300090428     C                   EVAL      VAODAR = *zeros
137400090428     C                   EVAL      vinmsg = %trimr(vinmsg)
137500090428     C                             + ' ' + 'VAODAR'
137600090428     C                   ENDIF
137700090428     C                   ENDIF
137800040708     C*** ORR
137900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
138000040708     C                   EVAL      PiStr=SkSplitCSV(i)
138100040708     C                   EXSR      CHKNUM
138200040708     C                   IF        PiInt=*on
138300040708     C                   Z-ADD     PiVal         VAOORR
138400040708     C                   ELSE
138500040708     C                   SETON                                        32
138600040708     C                   EVAL      VAOORR = *zeros
138700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
138800040708     C                             + ' ' + 'VAOORR'
138900040708     C                   ENDIF
139000040708     C                   ENDIF
139100030721     C*** NCL
139200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
139300030721     C                   EVAL      PiStr=SkSplitCSV(i)
139400030721     C                   EXSR      CHKNUM
139500030721     C                   IF        PiInt=*on
139600040723     C                   IF        PiVal > *zeros
139700040708     C                   Z-ADD     PiVal         VAONCL
139800040723     C                   ENDIF
139900030721     C                   ELSE
140000030721     C                   SETON                                        32
140100040708     C                   EVAL      VAONCL = *zeros
140200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140300040708     C                             + ' ' + 'VAONCL'
140400030721     C                   ENDIF
140500030721     C                   ENDIF
140600040708     C*** PKG
140700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
140800030721     C                   EVAL      PiStr=SkSplitCSV(i)
140900030721     C                   EXSR      CHKNUM
141000030721     C                   IF        PiNum=*on
141100040723     C                   IF        PiVal > *zeros
141200040708     C                   Z-ADD     PiVal         VAOPKG
141300040723     C                   ENDIF
141400030721     C                   ELSE
141500030721     C                   SETON                                        32
141600040708     C                   EVAL      VAOPKG = *zeros
141700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141800040708     C                             + ' ' + 'VAOPKG'
141900030721     C                   ENDIF
142000030721     C                   ENDIF
142100040708     C*** VLM
142200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
142300040708     C                   EVAL      PiStr=SkSplitCSV(i)
142400040708     C                   EXSR      CHKNUM
142500040708     C                   IF        PiNum=*on
142600040708     C                   Z-ADD     PiVal         VAOVLM
142700040708     C                   ELSE
142800040708     C                   SETON                                        32
142900040708     C                   EVAL      VAOVLM = *zeros
143000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
143100040708     C                             + ' ' + 'VAOVLM'
143200040708     C                   ENDIF
143300040708     C                   ENDIF
143400040708     C*** BNC
143500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
143600040708     C                   EVAL      PiStr=SkSplitCSV(i)
143700040708     C                   EXSR      CHKNUM
143800040708     C                   IF        PiInt=*on
143900040708     C                   Z-ADD     PiVal         VAOBNC
144000040708     C                   ELSE
144100040708     C                   SETON                                        32
144200040708     C                   EVAL      VAOBNC = *zeros
144300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
144400040708     C                             + ' ' + 'VAOBNC'
144500040708     C                   ENDIF
144600040708     C                   ENDIF
144700040708     C*** BLC
144800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
144900040708     C                   EVAL      PiStr=SkSplitCSV(i)
145000040708     C                   EXSR      CHKNUM
145100040708     C                   IF        PiInt=*on
145200040708     C                   Z-ADD     PiVal         VAOBLC
145300040708     C                   ELSE
145400040708     C                   SETON                                        32
145500040708     C                   EVAL      VAOBLC = *zeros
145600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
145700040708     C                             + ' ' + 'VAOBLC'
145800040708     C                   ENDIF
145900040708     C                   ENDIF
146000040708     C*** ATT
146100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
146200040708     C                   EVAL      PiStr=SkSplitCSV(i)
146300040708     C                   EXSR      CHKNUM
146400040708     C                   IF        PiInt=*on
146500040708     C                   Z-ADD     PiVal         VAOATT
146600040708     C                   ELSE
146700040708     C                   SETON                                        32
146800040708     C                   EVAL      VAOATT = *zeros
146900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
147000040708     C                             + ' ' + 'VAOATT'
147100040708     C                   ENDIF
147200040708     C                   ENDIF
147300040708     C*** MTC
147400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
147500040708     C                   EVAL      PiStr=SkSplitCSV(i)
147600040708     C                   EXSR      CHKNUM
147700040708     C                   IF        PiInt=*on
147800040708     C                   Z-ADD     PiVal         VAOMTC
147900040708     C                   ELSE
148000040708     C                   SETON                                        32
148100040708     C                   EVAL      VAOMTC = *zeros
148200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
148300040708     C                             + ' ' + 'VAOMTC'
148400040708     C                   ENDIF
148500040708     C                   ENDIF
148600040708     C*** KSC
148700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
148800030721     C                   EVAL      PiStr=SkSplitCSV(i)
148900030721     C                   EXSR      CHKNUM
149000030721     C                   IF        PiInt=*on      AND
149100030721     C                             PiVal<=9999999 AND
149200030721     C                             PiVal>*zeros
149300040708     C                   Z-ADD     PiVal         VAOKSC
149400030721     C                   ELSE
149500030721     C                   SETON                                        32
149600040708     C                   EVAL      VAOKSC = *zeros
149700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
149800040708     C                             + ' ' + 'VAOKSC'
149900030721     C                   ENDIF
150000030721     C                   ENDIF
150100040708     C*** POR
150200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
150300030721     C                   EVAL      PiStr=SkSplitCSV(i)
150400030721     C                   EXSR      CHKNUM
150500030721     C                   IF        PiInt=*on    AND
150600030721     C                             PiVal<=999   AND
150700030721     C                             PiVal>*zeros
150800040708     C                   Z-ADD     PiVal         VAOPOR
150900030721     C                   ELSE
151000030721     C                   SETON                                        32
151100040708     C                   EVAL      VAOPOR = *zeros
151200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
151300040708     C                             + ' ' + 'VAOPOR'
151400030721     C                   ENDIF
151500030721     C                   ENDIF
151600040708     C*** POC
151700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
151800040708     C                   EVAL      PiStr=SkSplitCSV(i)
151900040708     C                   EXSR      CHKNUM
152000040708     C                   IF        PiInt=*on    AND
152100040708     C                             PiVal<=999   AND
152200040708     C                             PiVal>*zeros
152300040708     C                   Z-ADD     PiVal         VAOPOC
152400040708     C                   ELSE
152500040708     C                   SETON                                        32
152600040708     C                   EVAL      VAOPOC = *zeros
152700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
152800040708     C                             + ' ' + 'VAOPOC'
152900040708     C                   ENDIF
153000040708     C                   ENDIF
153100040708     C*** STO
153200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
153300040708     C                   EVAL      PiStr=SkSplitCSV(i)
153400040708     C                   EXSR      CHKNUM
153500040708     C                   IF        PiInt=*on
153600040708     C                   Z-ADD     PiVal         VAOSTO
153700040708     C                   ELSE
153800040708     C                   SETON                                        32
153900040708     C                   EVAL      VAOSTO = *zeros
154000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
154100040708     C                             + ' ' + 'VAOSTO'
154200040708     C                   ENDIF
154300040708     C                   ENDIF
154400090428     C*
154500091103     C********** GESTIONE CAMPI PERSONALIZZATI
154600120120
154700130716     C*** W_VAORSR
154800130716     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAORSR'
154900130716     C                   EVAL      VAORSR = %trim(SkSplitCSV(i))
155000130716     C                   EVAL      VAORER = VAORSR
155100130513     C                   ENDIF
155200121207
155300090428     C*
155400090428     C****************************************
155500090428     C*
155600030715     C*
155700030715     C                   ENDIF
155800030715     C                   EVAL      i = i + 1
155900030715     C                   ENDDO
156000040723     C*
156100121207     C***  ===>  Gestione campi "particolari" fuori ciclo
156200121207     C*
156300130513
156400030715     C                   ENDIF
156500020204     C*
156600010607     C                   ENDSR
156700010607     C*----------------------------------------------------*
156800080702
156900080702
157000080702     C*----------------------------------------------------*
157100080702     C*  CONTROLLO NUMERICITA' CAMPI
157200080702     C*----------------------------------------------------*
157300080702     C     CHKNUM        BEGSR
157400080702     C*
157500080702     C                   IF        PiDecChr = *blanks
157600160719     C                   EVAL      PiDecChr = CharNUM
157700080702     C                   ENDIF
157800080702     C*
157900080702     C                   callp     UBISNUM_Check(PiStr
158000080702     C                                          :PiDecChr
158100080702     C                                          :PiVal
158200080702     C                                          :PiNum
158300080702     C                                          :PiInt)
158400080702     C*
158500080702     C                   ENDSR
158600080702     C***
158700080702
158800010601
158900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
159000010601     C     repfil        BEGSR
159100010601     C*
159200010601     C                   if        invfil = *zeros and
159300010601     C                             depfil > *zeros and
159400010629     C                             (vinflg = *blanks or
159500010629     C                              vinflg = *zeros)
159600010601     C
159700010601     C                   eval      invfil = depfil
159800010601     C                   endif
159900010601     C*
160000010601     C                   if        depfil <> invfil and
160100010601     C                             invfil > *zeros
160200010601     C                   eval      flgMulti = '1'
160300010601     C                   if        vinflg = *blanks
160400010601     C                   add       1             cntNonEl
160500010601     C                   endif
160600010601     C                   endif
160700010601     C*
160800010601     C                   if        vinflg = '2'
160900010601     C                   eval      flgStato = '2'
161000010601     C                   endif
161100010601     C*
161200010601     C                   ENDSR
161300010601     C***
161400010330
161500010330
161600990920      /TITLE Invio dei dati al punto operativo.
161700000613     C     invio         BEGSR
161800990920     C*
161900990920     C                   reset                   dscmz
162000010601     C                   move      invfil        cmzdst
162100990920     C                   eval      cmzfld = vlrfou
162200990920     C                   eval      cmzmbd = vlrhdl
162300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
162400000710     C                   if        prmfir = *blanks
162500040708     C                   eval      cmzfla = 'FNVAO00F'
162600040708     C                   eval      cmzmba = 'FNVAO00F'
162700000710     C                   else
162800000710     C                   eval      cmzfla = prmfir
162900000710     C                   eval      cmzmba = prmfir
163000000710     C                   endif
163100990920     C                   eval      cmznrr = *zeros
163200990920     C                   move      §ctrok        cmznrr
163300021018     C                   eval      cmzlba = vlrfl1
163400990920     C                   call(e)   'TIS711C'
163500990920     C                   parm                    dscmz
163600990921     C                   parm      *blanks       esito
163700990920     C                   if        %error
163800990920     C                             or cmzerr = '1'
163900990921     C                             or esito  = '1'
164000000710     C                   eval      wrkesito = '3'
164100990920     C                   endif
164200990920     C*
164300000613     C                   ENDSR
164400010601
164500010601
164600010601      /TITLE Invio dei dati al punto operativo.
164700010601     C     opeini        BEGSR
164800010601     C*
164900010601     C* Inizializzo flag e contatori operativi
165000010601     C                   movel     '0'           flgGiro           1
165100010601     C                   movel     '0'           flgMulti          1
165200010601     C                   movel     '1'           flgStato          1
165300010615     C                   movel     '0'           flgOk             1
165400010601     C                   z-add     *zeros        cntNonEl         10 0
165500010601     C                   z-add     *zeros        depfil            3 0
165600010601     C                   z-add     *zeros        invfil            3 0
165700010601     C*
165800010601     C                   ENDSR
165900010601     C***
166000050421
166100050421
166200091130     C     *pssr         BEGSR
166300050421     C*
166400091130     C                   eval      wrkesito = '2'
166500050421     C*
166600091130     C                   ENDSR     '*CANCL'
166700050421     C***
166800060306
166900060306
167000060306     C*-------------------------------------------------------------------------*
167100060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
167200060306     C*-------------------------------------------------------------------------*
167300060306     C     sr_Control    begsr
167400060306     C*
167500060306     C* inizializzo a ok il flag d controllo abilitazione
167600060306     C                   eval      Cli_Abil = *on
167700060306     C
167800060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
167900060306     C                   clear                   woriIDC
168000060306     C                   eval      kteCOD = 'VAP'
168100060306     C                   eval      kteKE1 = oriIDC
168200060306     C     keytbe        chain     TNTBE01L
168300060306     C                   if        %found(Tntbe01l)
168400060306     C                   eval      woriIDC = TbeKe2
168500060306     C                   else
168600060306     C                   eval      Cli_Abil = *off
168700060306     C                   endif
168800060306     C*
168900060306     C                   endsr
169000010330
169100010330
169200000613     C     *inzsr        BEGSR
169300990910     C*
169400990910     C     *entry        plist
169500990920     C                   parm                    tivlrds
169600990921     C                   parm      wrkesito      esito
169700000724     C                   parm                    prmlit
169800000710     C                   parm                    prmfir
169900010330     C*
170000010330     C* CALCOLA LA DATA CORRENTE
170100091127     C                   z-add     *zeros        datcor            8 0
170200091127     C                   time                    wn14             14 0
170300091127     C                   movel     wn14          wn6               6 0          *ORA (6)
170400130516     C                   eval      datcor = %dec(%date() : *ISO)
170500040723     C*
170600040723     C* Campi riferito al database
170700040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
170800040723     C     *like         define    tbeke1        kteKE1
170900040723     C*
171000040723     C* Definizione chiavi di lettura
171100040723     C* TNTBE01L - parziale
171200040723     C     keytbe        klist
171300040723     C                   kfld                    kteCOD                         *UTENTE
171400040723     C                   kfld                    kteKE1                         *CODICE
171500000613     C*
171600000613     C                   ENDSR
171700000613     C***
171800990908
171900040709** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
172000160719VAORFA;W_VAORSR;VAOINR;VAOCAR;VAOLOR;VAOPRR;VAOTER;VAONCL;VAOPKG;
172100160929VAORSC;VAOINC;VAOCAC;VAOLOC;VAOPRC;;VAODAR;VAOORR;;VAOEMLR;
172200160719
