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
003000161122     D fnvaoear      e ds                  inz
003100161122     D                                     prefix(w_)
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
014700010330     C*
014800010604     C                   clear                   §CTROK
014900010604     C                   clear                   §CTRMO
015000010604     C                   clear                   §CTRNO
015100161122     C*
015200161122     C* Mi predispongo per scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
015300161122     C                   call      'TIS7T7ER'
015400161122     C                   parm      'O'           pIn_Opz           1
015500161122     C                   parm                    tivlrds
015600161122     C                   parm                    FNVAOEAR
015700161122     C                   parm                    oriPRG
015800161122     C                   parm      *zeros        pIn_Dst           3
015900161122     C                   parm      *blanks       pOut_Esito        1
016000161122     C                   if        pOut_Esito = '2'
016100161122     C                   goto      FINE
016200161122     C                   endif
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
020900161122     C*
021000161122     C* Stacco subito un numeratore univoco "ORM"
021100161122     C                   exsr      RTVNUM
021200161122     C*
021300161122     C* Verifico se trattasi di formato ORM "esteso"
021400161122     C                   setoff                                       53
021500161122     C                   if        w_VAOFLO2  <> *blanks OR
021600161122     C                             w_VAOTSPS  <> *blanks OR
021700161122     C                             w_VAONOTE  <> *blanks OR
021800161122     C                             w_VAOORDA1 <> *blanks OR
021900161122     C                             w_VAOORAA1 <> *blanks OR
022000161122     C                             w_VAOORDA2 <> *blanks OR
022100161122     C                             w_VAOORAA2 <> *blanks OR
022200161122     C                             w_VAOEMLR  <> *blanks OR
022300161122     C                             w_VAOEMLRF <> *blanks OR
022400161122     C                             w_VAOSMSR  <> *blanks OR
022500161122     C                             w_VAOSMSRF <> *blanks OR
022600161122     C                             w_VAOORIP  <> *blanks OR
022700161122     C                             w_VAODOIO  <> *blanks
022800161122     C                   seton                                        53
022900161122     C                   endif
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
025100161122     C*
025200161122     C* Se richiesta scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
025300161122     C                   if        *in53 and not *in31
025400161122     C                   exsr      EXEFORZ
025500161122     C                   call      'TIS7T7ER'
025600161122     C                   parm      'W'           pIn_Opz           1
025700161122     C                   parm                    tivlrds
025800161122     C                   parm                    FNVAOEAR
025900161122     C                   parm                    oriPRG
026000161122     C                   parm      *zeros        pIn_Dst           3
026100161122     C                   parm      *blanks       pOut_Esito        1
026200161122     C                   endif
026300161122     C                   if        pOut_Esito = '2'
026400161122     C                   seton                                        32
026500161122     C                   endif
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
028500161122     C*
028600161122     C     FINE          tag
028700161122     C*
028800161122     C* Chiusura per scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
028900161122     C                   call      'TIS7T7ER'
029000161122     C                   parm      'C'           pIn_Opz           1
029100161122     C                   parm                    tivlrds
029200161122     C                   parm                    FNVAOEAR
029300161122     C                   parm                    oriPRG
029400161122     C                   parm      *zeros        pIn_Dst           3
029500161122     C                   parm      *blanks       pOut_Esito        1
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
033500161122     C*
033600161122     C* Invio dati estensione ORM (FNVAOE0F) => richiamo l'apposito driver
033700161122     C                   move      invfil        cmzdst
033800161122     C                   call      'TIS7T7ER'
033900161122     C                   parm      'S'           pIn_Opz           1
034000161122     C                   parm                    tivlrds
034100161122     C                   parm                    FNVAOEAR
034200161122     C                   parm                    oriPRG
034300161122     C                   parm      cmzdst        pIn_Dst           3
034400161122     C                   parm      *blanks       pOut_Esito        1
034500010330     C                   endif
034600010601     C*
034700010618     C                   if        flgGiro = '1'
034800010601     C                   exsr      endela
034900010618     C                   endif
035000010330     C*
035100010330     C                   ENDSR
035200010330     C***
035300161122
035400161122
035500161122     C*------------------------------------------------------------------------*
035600161122     C* RTVNUM - Stacco un numeratore univoco "ORM"
035700161122     C*------------------------------------------------------------------------*
035800161122     C     RTVNUM        BEGSR
035900161122     C*
036000161122     C* Stacco un numeratore da AZNUM
036100161122     C                   clear                   §numori
036200161122     C                   clear                   trul33ds
036300161122     C                   eval      I33OPE = *zeros
036400161122     C                   eval      I33CNU = 600
036500161122     C                   eval      I33NUM = 1
036600161122     C                   movel     TRUL33DS      KPJBU
036700161122     C                   call      'TRUL33R'
036800161122     C                   parm                    KPJBA
036900161122     C                   movel     KPJBU         TRUL33DS
037000161122     C                   if        O33ERR = *zeros
037100161122     C                   z-add     O33NRF        §numori
037200161122     c                   else
037300161122     c                   eval      §numori = *all'9'
037400161122     C                   seton                                        31        * errore bloccante
037500161122     C                   endif
037600161122     C*
037700161122     C                   eval      vaoNOR = §numori
037800161122     C*
037900161122     C                   ENDSR
038000161122
038100161122
038200161122     C*------------------------------------------------------------------------*
038300161122     C* EXEFORZ - Effettuo forzature
038400161122     C*------------------------------------------------------------------------*
038500161122     C     EXEFORZ       BEGSR
038600161122     C*
038700161122     C                   move(p)   VAOPOE        w_vaopoe                       * forzatura necessar
038800161122     C                   move(p)   VAOCOR        w_vaocor                       * forzatura necessar
038900161122     C                   move(p)   VAONOR        w_vaonor                       * forzatura necessar
039000161122     C*
039100161122     C                   ENDSR
039200040723
039300040723
039400040723     C*------------------------------------------------------------------------*
039500040723     C* CHKCOR - Considerazioni su VAOCOR
039600040723     C*------------------------------------------------------------------------*
039700040723     C     CHKCOR        BEGSR
039800040723     C*
039900040723     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
040000040723     C                   if        vaoTCO = 'F'
040100040723     C* Quindi verifico se il codice ordinante non è valorizzato
040200040723     C                   if        vaoCOR = *zeros  AND
040300040723     C                             vaoRSO = *blanks AND
040400040723     C                             vaoINO = *blanks AND
040500040723     C                             vaoCAO = *blanks AND
040600040723     C                             vaoLOO = *blanks AND
040700040723     C                             vaoPRO = *blanks AND
040800040723     C                             vaoNAO = *blanks
040900040723     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
041000040723     C                   movel     '0'           wFlgAcr           1
041100040723     C                   move      vlrKSC        wCli              7 0
041200040723     C                   move(p)   wCli          acrCRO
041300040723     C     acrCRO        setll     fnacr01l
041400040723     C                   if        %found(fnacr01l)
041500040723     C                   read      fnacr01l
041600040723     C                   dow       not %eof(fnacr01l)
041700040723     C                   movel     acrCRO        wCliAcr           7 0
041800040723     C                   if        wCliAcr = wCli
041900040723     C                   eval      vaoCOR = acrCRO
042000040723     C                   movel     '1'           wFlgAcr
042100040723     C                   leave
042200040723     C                   else
042300040723     C                   leave
042400040723     C                   endif
042500040723     C                   read      fnacr01l
042600040723     C                   enddo
042700040723     C                   endif
042800040723     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
042900040723     C* dati anagrafici dell'ordinante dal piano dei conti
043000040723     C                   if        wFlgAcr = '0'
043100040723     C                   clear                   BS69DS
043200040723     C                   clear                   ACODS
043300040723     C                   clear                   INDDS
043400040723     C                   clear                   CLPDS
043500040723     C                   clear                   CLSDS
043600040723     C                   move(P)   vlrKSC        I69KAC
043700040723     C                   move(P)   vlrKSC        I69KIN
043800040723     C                   call      'TIBS69R'
043900040723     C                   parm                    BS69DS
044000040723     C                   parm                    ACODS
044100040723     C                   parm                    INDDS
044200040723     C                   parm                    CLPDS
044300040723     C                   parm                    CLSDS
044400040723     C     O69ERR        ifne      '1'
044500040723     C                   eval      vaoRSO = ACORAG
044600040723     C                   eval      vaoINO = INDVIA
044700040723     C                   movel(P)  INDCAP        vaoCAO
044800040723     C                   eval      vaoLOO = INDCIT
044900040723     C                   eval      vaoPRO = INDPRV
045000040723     C                   eval      vaoNAO = INDSTA
045100040723     C                   endif
045200040723     C                   endif
045300040723     C                   endif
045400040723     C                   endif
045500040723     C*
045600040723     C                   ENDSR
045700040723
045800040723
045900040723     C*------------------------------------------------------------------------*
046000040723     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
046100040723     C*------------------------------------------------------------------------*
046200040723     C     WRIORI        BEGSR
046300040723     C*
046400090618     C* Come prima cosa stacco un numeratore da AZNUM
046500161122     C***                clear                   trul33ds
046600161122     C***                eval      I33OPE = *zeros
046700161122     C***                eval      I33CNU = 600
046800161122     C***                eval      I33NUM = 1
046900161122     C***                movel     TRUL33DS      KPJBU
047000161122     C***                call      'TRUL33R'
047100161122     C***                parm                    KPJBA
047200161122     C***                movel     KPJBU         TRUL33DS
047300161122     C***                if        O33ERR = *zeros
047400161122     C***                z-add     O33NRF        §numori
047500161122     c***                else
047600161122     c***                eval      §numori = *all'9'
047700161122     C***                endif
047800040723     C*
047900040723     C* Quindi imposto i campi "extra-VAO"
048000040723     C                   if        vlrKSC = '0ORM0000'
048100040723     C*
048200040723     C                   if        vaoCOR > *zeros
048300040723     C                   movel     vaoCOR        wrkCOR7           7 0
048400040723     C*
048500040723     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
048600040723     C* codice padre. Se è un padre assegno già il codice ordinante.
048700040723     C                   clear                   tibs10ds
048800040723     C                   eval      d10drf = datcor                              *data riferimento
048900040723     C                   eval      d10paf = 'P'                                 *cerca il padre
049000040723     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
049100091026     C*
049200091026     C* Preparazione chiamata al TIBS10R1
049300091026     C                   clear                   tibs10r1ds
049400091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
049500091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
049600091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
049700091026     C*
049800091026     C                   call      'TIBS10R1'
049900091026     C                   parm                    tibs10r1ds
050000091026     C*
050100091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
050200040723     C*
050300040723     C                   if        d10err <> *blanks or                         *No unif. o padre
050400040723     C                             d10cop =  *zeros  or
050500040723     C                             d10cop =  d10cod
050600040723     C                   move      d10cod        oriIDC
050700040723     C                   else                                                   *Figlio
050800040723     C                   move      d10cop        oriIDC
050900040723     C                   endif
051000040723     C*
051100040723     C                   else
051200040723     C                   eval      oriIDC = *zeros
051300040723     C                   endif
051400040723     C*
051500040723     C                   else
051600040723     C                   eval      oriIDC = vlrKSC
051700040723     C                   endif
051800040723     C*
051900040723     C                   eval      oriPRG = §numori
052000040723     C                   eval      oriDER = datcor
052100040723     C                   eval      oriOER = wn6
052200040723     C                   eval      oriPOE = vaoPOE
052300040723     C                   eval      oriNSR = vaoNSR
052400040723     C                   eval      oriNOR = vaoNOR
052500040723     C                   eval      oriNRV = vaoNRV
052600040723     C                   eval      oriFLO = *blanks
052700040723     C*
052800040723     C* Infine valorizzo la chiave esterna sul file FNVAO
052900040723     C                   eval      dorm01 = vaoFLO
053000040723     C                   movel     §numori       §ormpg
053100130603     C                   EVAL      §ORCOMC = 'N'
053200050412     C*
053300050412     C* Infine valorizzo i flag operativi necessari x la particolare gestione corrente
053400130326     C*                   in questo caso non ci sono default
053500050412     C*
053600130326     C* Riassegno il campo VAO contenete i flag operativi precedentemente ridefiniti
053700040723     C                   eval      vaoFLO = dorm01
053800130326     C* Infine, per sicurezza, pulisco alcuni flag che devono essere a blank
053900130326     C                   eval      %subst(VAOFLO:5:1) = *blanks
054000130326     C                   eval      %subst(VAOFLO:6:1) = *blanks
054100040723     C*
054200040723     C* ...quindi scrivo il file TIORI00F
054300120905     C  N55              WRITE     TIORI000
054400040723     C*
054500040723     C                   ENDSR
054600050826
054700050826
054800050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
054900050826     C     wriVGD        BEGSR
055000050826     C*
055100050826     C* Reperisco la descrizione della fase da tabella
055200050826     C                   movel(p)  'FAR'         KteCOD
055300050826     C                   movel(p)  '000'         KteKE1
055400050826     C     KEYtbe        chain     tntbe01l
055500050826     C                   if        %found(tntbe01l)
055600050826     C                   movel(P)  tbeuni        DFAR
055700050826     C                   else
055800050826     C                   clear                   DFAR
055900050826     C                   endif
056000050826     C*
056100050826     C* Valorizzo buffer record
056200050826     C                   clear                   FNVAPDS
056300050826     C                   eval      vapIDC = oriIDC
056400050826     C                   eval      vapPOE = oriPOE
056500050826     C                   eval      vapRFA = vaoRFA
056600050826     C                   eval      vapPOG = oriPOE
056700050826     C                   eval      vapDAE = oriDER
056800050826     C                   eval      vapORE = oriOER
056900050826     C                   if        d§fardva <> *blanks
057000050826     C                   eval      vapDFA = d§fardva
057100050826     C                   else
057200050826     C                   eval      vapDFA = d§fardes
057300050826     C                   endif
057400050826     C*
057500050826     C                   clear                   tivgd000
057600080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
057700050826     C                   eval      vgdTIP = 'VP'
057800050826     C                   eval      vgdKSU = vapIDC
057900050826     C                   eval      vgdTSC = 'WW'
058000050826     C                   eval      vgdDAT = datcor
058100151207     C                   eval      vgdPGM = procname
058200060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
058300060306     C                   exsr      sr_Control
058400060306     C                   if        Cli_Abil = *on
058500060306     C                   eval      vgdKSU = woriIDC
058600120905     C  N55              write     tivgd000
058700060306     C                   endif
058800050826     C*
058900050826     C                   ENDSR
059000050826     C*------------------------------------------------------------------------*
059100010601
059200010601
059300010330     C*----------------------------------------------------*
059400030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
059500010330     C*----------------------------------------------------*
059600010330     C     INZVAR        BEGSR
059700010330     C*
059800030822     C* Inizializzo variabili di wrk
059900020204     C                   Z-ADD     *zeros        Num5_0            5 0
060000120124
060100030822     C*
060200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
060300120123     C                   CLEAR                   FNVAO000
060400161122     C                   CLEAR                   FNVAOEAR
060500120123     C                   CLEAR                   TIORI000
060600030822     C                   CLEAR                   SkSplitCSV
060700030822     C                   EVAL      i = 1
060800030822     C                   EVAL      posDa = *zeros
060900030822     C                   EVAL      posA  = *zeros
061000030822     C*
061100030822     C* Reimposto i valori di default
061200030822     C                   EXSR      DEFCAM
061300030822     C*
061400010330     C                   ENDSR
061500010330     C*----------------------------------------------------*
061600030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
061700010330     C*----------------------------------------------------*
061800010330     C     DEFCAM        BEGSR
061900030822     C*
062000030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
062100030715     C* e delimitatore testo.
062200030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
062300030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
062400030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
062500040723     C*
062600080707     C* Imposto i valori d default d alcuni campi
062700080708     C*
062800080708     C                   CLEAR                   DORM01
062900120822     C                   EVAL      VAOFLO = DORM01
063000080707     C*
063100130603     C                   EVAL      VAOTOR = 'S'
063200161122     C                   EVAL      VAOCOR = 0899163000
063300130716     C                   EVAL      VAOPAG = 'O'
063400161122     C                   EVAL      VAOPOE = 089
063500161122     C                   EVAL      VAOKSC = 0899163
063600130716     C                   EVAL      VAOCTR = '000'
063700130716     C                   EVAL      VAORFA = %char(%date())
063800130513
063900120905     C* se il 1° char dei parametri è T=Test
064000120905     C                   SETOFF                                       55
064100120905     C                   IF        %subst(vlrppt:1:1) = 'T'
064200120905     C* inibisco la scrittura dei file TIORI e TIVGD
064300120905     C                   SETON                                        55
064400120905     C* imposto il PO di emissione come filiale fittizia
064500120905     C                   EVAL      VAOPOE = 999
064600120905     C                   ENDIF
064700091127     C*
064800091204     C* IMPOSTO I CAMPI PERSONALIZZATI
064900100416     C
065000030822     C*
065100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
065200030822     C                   EVAL      posDaDft = 1
065300030822     C                   EVAL      posADft  = 0
065400030822     C                   EVAL      wGiroDft = 0
065500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
065600030822     C                             posDaDft > 0
065700030822     C*
065800030822     C* Gestisco il 1° giro
065900030822     C                   IF        wGiroDft = 0
066000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
066100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
066200030822     C* Incremento il contatore dei "giri"
066300030822     C                   EVAL      wGiroDft = 1
066400030822     C                   ELSE
066500030822     C                   EVAL      posDaDft = posADft
066600030822     C                   ENDIF
066700030822     C* Eseguo lo scan x trovare la fine del campo corrente
066800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
066900030822     C*
067000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
067100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
067200030822     C                   IF        posDaDft > 0 AND
067300030822     C                             posADft  > 0
067400040708     C* POE
067500040708     C                   IF        %subst(
067600040708     C                             %subst(vlrppt:posDaDft+1:
067700040708     C                             posADft-posDaDft-1):1:3)
067800040708     C                             = 'POE'
067900040708     C                   EVAL      PiStr=%trim(%subst(
068000040708     C                             %subst(vlrppt:posDaDft+1:
068100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068200040708     C                   EXSR      CHKNUM
068300040708     C                   IF        PiInt=*on
068400040708     C                   Z-ADD     PiVal         VAOPOE
068500040708     C                   ENDIF
068600040708     C                   ENDIF
068700040708     C* TOR
068800040708     C                   IF        %subst(
068900040708     C                             %subst(vlrppt:posDaDft+1:
069000040708     C                             posADft-posDaDft-1):1:3)
069100040708     C                             = 'TOR'
069200040708     C                   EVAL      VAOTOR=%trim(%subst(
069300040708     C                             %subst(vlrppt:posDaDft+1:
069400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069500040708     C                   ENDIF
069600040708     C* TCO
069700040708     C                   IF        %subst(
069800040708     C                             %subst(vlrppt:posDaDft+1:
069900040708     C                             posADft-posDaDft-1):1:3)
070000040708     C                             = 'TCO'
070100040708     C                   EVAL      VAOTCO=%trim(%subst(
070200040708     C                             %subst(vlrppt:posDaDft+1:
070300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070400040708     C                   ENDIF
070500040707     C* COR
070600030822     C                   IF        %subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):1:3)
070900040707     C                             = 'COR'
071000030822     C                   EVAL      PiStr=%trim(%subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071300030822     C                   EXSR      CHKNUM
071400030822     C                   IF        PiInt=*on
071500040708     C                   Z-ADD     PiVal         VAOCOR
071600030822     C                   ENDIF
071700030822     C                   ENDIF
071800040708     C* CRC
071900040708     C                   IF        %subst(
072000040708     C                             %subst(vlrppt:posDaDft+1:
072100040708     C                             posADft-posDaDft-1):1:3)
072200040708     C                             = 'CRC'
072300040708     C                   EVAL      PiStr=%trim(%subst(
072400040708     C                             %subst(vlrppt:posDaDft+1:
072500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600040708     C                   EXSR      CHKNUM
072700040708     C                   IF        PiInt=*on
072800040708     C                   Z-ADD     PiVal         VAOCRC
072900040708     C                   ENDIF
073000040708     C                   ENDIF
073100040708     C* FFD
073200040708     C                   IF        %subst(
073300040708     C                             %subst(vlrppt:posDaDft+1:
073400040708     C                             posADft-posDaDft-1):1:3)
073500040708     C                             = 'FFD'
073600040708     C                   EVAL      VAOFFD=%trim(%subst(
073700040708     C                             %subst(vlrppt:posDaDft+1:
073800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073900040708     C                   ENDIF
074000040708     C* ORR
074100040708     C                   IF        %subst(
074200040708     C                             %subst(vlrppt:posDaDft+1:
074300040708     C                             posADft-posDaDft-1):1:3)
074400040708     C                             = 'ORR'
074500040708     C                   EVAL      PiStr=%trim(%subst(
074600040708     C                             %subst(vlrppt:posDaDft+1:
074700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074800040708     C                   EXSR      CHKNUM
074900040708     C                   IF        PiInt=*on
075000040708     C                   Z-ADD     PiVal         VAOORR
075100040708     C                   ENDIF
075200040708     C                   ENDIF
075300040712     C* NCL
075400040712     C                   IF        %subst(
075500040712     C                             %subst(vlrppt:posDaDft+1:
075600040712     C                             posADft-posDaDft-1):1:3)
075700040712     C                             = 'NCL'
075800040712     C                   EVAL      PiStr=%trim(%subst(
075900040712     C                             %subst(vlrppt:posDaDft+1:
076000040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076100040712     C                   EXSR      CHKNUM
076200040712     C                   IF        PiInt=*on
076300040712     C                   Z-ADD     PiVal         VAONCL
076400040712     C                   ENDIF
076500040712     C                   ENDIF
076600040712     C* PKG
076700040712     C                   IF        %subst(
076800040712     C                             %subst(vlrppt:posDaDft+1:
076900040712     C                             posADft-posDaDft-1):1:3)
077000040712     C                             = 'PKG'
077100040712     C                   EVAL      PiStr=%trim(%subst(
077200040712     C                             %subst(vlrppt:posDaDft+1:
077300040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077400040712     C                   EXSR      CHKNUM
077500040712     C                   IF        PiNum=*on
077600040712     C                   Z-ADD     PiVal         VAOPKG
077700040712     C                   ENDIF
077800040712     C                   ENDIF
077900040712     C* VLM
078000040712     C                   IF        %subst(
078100040712     C                             %subst(vlrppt:posDaDft+1:
078200040712     C                             posADft-posDaDft-1):1:3)
078300040712     C                             = 'VLM'
078400040712     C                   EVAL      PiStr=%trim(%subst(
078500040712     C                             %subst(vlrppt:posDaDft+1:
078600040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078700040712     C                   EXSR      CHKNUM
078800040712     C                   IF        PiNum=*on
078900040712     C                   Z-ADD     PiVal         VAOVLM
079000040712     C                   ENDIF
079100040712     C                   ENDIF
079200040708     C* NAM
079300040708     C                   IF        %subst(
079400040708     C                             %subst(vlrppt:posDaDft+1:
079500040708     C                             posADft-posDaDft-1):1:3)
079600040708     C                             = 'NAM'
079700040708     C                   EVAL      VAONAM=%trim(%subst(
079800040708     C                             %subst(vlrppt:posDaDft+1:
079900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080000040708     C                   ENDIF
080100040708     C* PAG
080200030822     C                   IF        %subst(
080300030822     C                             %subst(vlrppt:posDaDft+1:
080400030822     C                             posADft-posDaDft-1):1:3)
080500040708     C                             = 'PAG'
080600040708     C                   EVAL      VAOPAG=%trim(%subst(
080700030822     C                             %subst(vlrppt:posDaDft+1:
080800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080900030822     C                   ENDIF
081000040708     C* KSC
081100040708     C                   IF        %subst(
081200040708     C                             %subst(vlrppt:posDaDft+1:
081300040708     C                             posADft-posDaDft-1):1:3)
081400040708     C                             = 'KSC'
081500040708     C                   EVAL      PiStr=%trim(%subst(
081600040708     C                             %subst(vlrppt:posDaDft+1:
081700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081800040708     C                   EXSR      CHKNUM
081900040708     C                   IF        PiInt=*on
082000040708     C                   Z-ADD     PiVal         VAOKSC
082100040708     C                   ENDIF
082200040708     C                   ENDIF
082300040708     C* CTR
082400040708     C                   IF        %subst(
082500040708     C                             %subst(vlrppt:posDaDft+1:
082600040708     C                             posADft-posDaDft-1):1:3)
082700040708     C                             = 'CTR'
082800040708     C                   EVAL      VAOCTR=%trim(%subst(
082900040708     C                             %subst(vlrppt:posDaDft+1:
083000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083100040708     C                   ENDIF
083200040708     C* POC
083300040708     C                   IF        %subst(
083400040708     C                             %subst(vlrppt:posDaDft+1:
083500040708     C                             posADft-posDaDft-1):1:3)
083600040708     C                             = 'POC'
083700040708     C                   EVAL      PiStr=%trim(%subst(
083800040708     C                             %subst(vlrppt:posDaDft+1:
083900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084000040708     C                   EXSR      CHKNUM
084100040708     C                   IF        PiInt=*on
084200040708     C                   Z-ADD     PiVal         VAOPOC
084300040708     C                   ENDIF
084400040708     C                   ENDIF
084500040708     C* NO1
084600040708     C                   IF        %subst(
084700040708     C                             %subst(vlrppt:posDaDft+1:
084800121129     C                             posADft-posDaDft-1):1:3)
084900040708     C                             = 'NO1'
085000040708     C                   EVAL      VAONO1=%trim(%subst(
085100040708     C                             %subst(vlrppt:posDaDft+1:
085200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085300040708     C                   ENDIF
085400040708     C* DDT
085500030822     C                   IF        %subst(
085600030822     C                             %subst(vlrppt:posDaDft+1:
085700030822     C                             posADft-posDaDft-1):1:3)
085800040708     C                             = 'DDT'
085900040708     C                   EVAL      VAODDT=%trim(%subst(
086000030822     C                             %subst(vlrppt:posDaDft+1:
086100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086200030822     C                   ENDIF
086300040708     C* RFA
086400040708     C                   IF        %subst(
086500040708     C                             %subst(vlrppt:posDaDft+1:
086600040708     C                             posADft-posDaDft-1):1:3)
086700040708     C                             = 'RFA'
086800040708     C                   EVAL      VAORFA=%trim(%subst(
086900040708     C                             %subst(vlrppt:posDaDft+1:
087000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
087100040708     C                   ENDIF
087200040708     C* FLO
087300040708     C                   IF        %subst(
087400040708     C                             %subst(vlrppt:posDaDft+1:
087500040708     C                             posADft-posDaDft-1):1:3)
087600040708     C                             = 'FLO'
087700040708     C                   EVAL      VAOFLO=%trim(%subst(
087800040708     C                             %subst(vlrppt:posDaDft+1:
087900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
088000091103     C*
088100040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
088200080702     C* e posizione 7 del VAOFLO
088300040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
088400040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
088500050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
088600040708     C                   ENDIF
088700030822     C* ...
088800030822     C                   ENDIF
088900030822     C                   ENDDO
089000020204     C*
089100010330     C                   ENDSR
089200010607     C*----------------------------------------------------*
089300040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
089400010607     C*----------------------------------------------------*
089500040708     C     IMPVAO        BEGSR
089600010607     C*
089700010607     C                   SETOFF                                       3132
089800120124
089900030515     C*
090000040708     C                   MOVEL     datcor        VAODAO
090100040708     C                   MOVEL     wn14          VAOOAO
090200050314     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
090300050314     C     minu:maiu     XLATE     vindta        vindta
090400010607     C*
090500030715     C**********
090600030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
090700030715     C**********
090800030715     C                   IF        wGiro = *zeros
090900040709     C*
091000040709     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
091100040709     C                   EVAL      depvinDTA = vinDTA
091200040709     C*
091300040709     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
091400040709     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
091500040709     C                                      %trim(intcol(3))
091600031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
091700050314     C**!!!minu:maiu     XLATE     vindta        vindta
091800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
091900040324     C                   DOW       posDa <= %len(%trimr(vindta))
092000030715     C*
092100030715     C* Gestisco il 1° campo
092200030715     C                   IF        i = 1
092300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
092400030715     C                   EVAL      posDa = 1
092500030715     C* Eseguo lo scan x trovare la fine del primo campo
092600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092700030715     C* A questo punto "estraggo" il campo corrente
092800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
092900030715     C                                                    (posA-posDa))
093000030715     C* X i campi successivi al 1°
093100030715     C                   ELSE
093200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
093300030715     C                   EVAL      posDa = posA + 1
093400030715     C* Eseguo lo scan x trovare la fine del campo corrente
093500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
093600030715     C* Gestisco l'ultimo campo
093700030715     C                   IF        posA = *zeros
093800040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
093900030715     C* A questo punto "estraggo" il campo corrente
094000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
094100030715     C                                                    (posA-posDa))
094200030715     C                   LEAVE
094300030715     C                   ELSE
094400030715     C* A questo punto "estraggo" il campo corrente
094500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
094600030715     C                                                    (posA-posDa))
094700030715     C                   ENDIF
094800030715     C                   ENDIF
094900030715     C* Incremento il contatore d campo
095000030715     C                   EVAL      i = i +1
095100030715     C                   ENDDO
095200030715     C*
095300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
095400030715     C* delimitazione testo
095500030715     C                   EVAL      i = 1
095600030715     C                   DOW       i < %elem(SkSplitFLD)
095700030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
095800030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
095900030715     C                   EVAL      i = i + 1
096000030715     C                   ENDDO
096100030715     C                   EVAL      wGiro = 1
096200040709     C* Ripristino la 1° riga originale
096300040709     C                   EVAL      vinDTA = depvinDTA
096400030715     C                   ELSE
096500030715     C                   EVAL      wGiro = 2
096600031204     C**********
096700031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
096800031204     C* d separatore campo
096900031204     C**********
097000040709     C                   EVAL      vindta = %trim(vindta)
097100040709     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
097200040709     C                   EVAL      vindta = %subst(vindta:2)
097300040709     C                   ENDDO
097400040709     C*
097500040709     C                   Z-ADD     *zeros        lunghInput        4 0
097600040709     C                   EVAL      lunghInput = %len(%trim(vindta))
097700040709     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
097800040709     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
097900040709     C                   EVAL      lunghInput = %len(%trim(vindta))
098000040709     C                   ENDDO
098100030715     C**********
098200030715     C* Effettuo lo split del campo dati d input
098300030715     C**********
098400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
098500040324     C                   DOW       posDa <= %len(%trimr(vindta))
098600030715     C*
098700030715     C* Gestisco il 1° campo
098800030715     C                   IF        i = 1
098900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
099000030715     C                   EVAL      posDa = 1
099100030715     C* Eseguo lo scan x trovare la fine del primo campo
099200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
099300030715     C* A questo punto "estraggo" il campo corrente
099400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
099500030715     C                                                    (posA-posDa))
099600030715     C* X i campi successivi al 1°
099700030715     C                   ELSE
099800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
099900030715     C                   EVAL      posDa = posA + 1
100000031204     C* Verifico che nn vi sia il campo nn valorizzato
100100031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
100200031204     C* Se campo nn valorizzato skippo al prossimo
100300031204     C                   EVAL      SkSplitCSV(i) = *blanks
100400031204     C                   EVAL      posA  = posA + 1
100500031204     C                   ELSE
100600030715     C* Eseguo lo scan x trovare la fine del campo corrente
100700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
100800030715     C* Gestisco l'ultimo campo
100900030715     C                   IF        posA = *zeros
101000040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
101100030715     C* A questo punto "estraggo" il campo corrente
101200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
101300030715     C                                                    (posA-posDa))
101400030715     C                   LEAVE
101500030715     C                   ELSE
101600030715     C* A questo punto "estraggo" il campo corrente
101700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
101800030715     C                                                    (posA-posDa))
101900030715     C                   ENDIF
102000031204     C                   ENDIF
102100030715     C                   ENDIF
102200030715     C* Incremento il contatore d campo
102300030715     C                   EVAL      i = i +1
102400030715     C                   ENDDO
102500030715     C*
102600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
102700030715     C* delimitazione testo
102800030715     C                   EVAL      i = 1
102900030715     C                   DOW       i < %elem(SkSplitCSV)
103000030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
103100030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
103200030715     C                   EVAL      i = i + 1
103300030715     C                   ENDDO
103400030715     C*
103500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
103600030715     C                   EVAL      i = 1
103700030715     C                   DOW       i < %elem(SkSplitFLD)
103800030715     C*
103900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
104000030715     C                   IF        SkSplitFLD(i) <> *blanks AND
104100030715     C                             SkSplitCSV(i) <> *blanks
104200030721     C*** ATB
104300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATB'
104400040708     C                   EVAL      VAOATB = SkSplitCSV(i)
104500030721     C                   ENDIF
104600040708     C*** TOR
104700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTOR'
104800040708     C                   EVAL      VAOTOR = SkSplitCSV(i)
104900040708     C                   ENDIF
105000040708     C*** TCO
105100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTCO'
105200040708     C                   EVAL      VAOTCO = SkSplitCSV(i)
105300040708     C                   ENDIF
105400040708     C*** RSO
105500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSO'
105600040708     C                   EVAL      VAORSO = SkSplitCSV(i)
105700030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
105800040708     C     '@':'A'       XLATE     VAORSO        VAORSO
105900030715     C* ==
106000030715     C                   ENDIF
106100040708     C*** INO
106200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINO'
106300040708     C                   EVAL      VAOINO = SkSplitCSV(i)
106400030715     C                   ENDIF
106500040708     C*** LOO
106600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOO'
106700040708     C                   EVAL      VAOLOO = SkSplitCSV(i)
106800030715     C                   ENDIF
106900040708     C*** PRO
107000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRO'
107100040708     C                   EVAL      VAOPRO = SkSplitCSV(i)
107200030715     C                   ENDIF
107300040708     C*** NAO
107400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAO'
107500040708     C                   EVAL      VAONAO = SkSplitCSV(i)
107600030721     C                   ENDIF
107700040708     C*** RSR
107800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSR'
107900040708     C                   EVAL      VAORSR = SkSplitCSV(i)
108000040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
108100040708     C     '@':'A'       XLATE     VAORSR        VAORSR
108200040708     C* ==
108300040708     C                   IF        VAORSR = *blanks
108400040708     C                   SETON                                        31
108500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
108600040708     C                             + ' ' + 'VAORSR'
108700040708     C                   ENDIF
108800040708     C                   ENDIF
108900040708     C*** INR
109000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINR'
109100040708     C                   EVAL      VAOINR = SkSplitCSV(i)
109200040708     C                   IF        VAOINR = *blanks
109300040708     C                   SETON                                        31
109400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
109500040708     C                             + ' ' + 'VAOINR'
109600040708     C                   ENDIF
109700040708     C                   ENDIF
109800040708     C*** LOR
109900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOR'
110000040708     C                   EVAL      VAOLOR = SkSplitCSV(i)
110100040708     C                   IF        VAOLOR = *blanks
110200040708     C                   SETON                                        31
110300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
110400040708     C                             + ' ' + 'VAOLOR'
110500040708     C                   ENDIF
110600040708     C                   ENDIF
110700040708     C*** PRR
110800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRR'
110900040708     C                   EVAL      VAOPRR = SkSplitCSV(i)
111000040708     C                   IF        VAOPRR = *blanks
111100040708     C                   SETON                                        31
111200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
111300040708     C                             + ' ' + 'VAOPRR'
111400040708     C                   ENDIF
111500040708     C                   ENDIF
111600040708     C*** NAR
111700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAR'
111800040708     C                   EVAL      VAONAR = SkSplitCSV(i)
111900040708     C                   ENDIF
112000040708     C*** RER
112100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORER'
112200040708     C                   EVAL      VAORER = SkSplitCSV(i)
112300040708     C                   ENDIF
112400040708     C*** TER
112500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTER'
112600040708     C                   EVAL      VAOTER = SkSplitCSV(i)
112700040708     C                   ENDIF
112800040708     C*** RSC
112900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSC'
113000040708     C                   EVAL      VAORSC = SkSplitCSV(i)
113100040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
113200040708     C     '@':'A'       XLATE     VAORSC        VAORSC
113300040708     C* ==
113400040708     C                   ENDIF
113500040708     C*** INC
113600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINC'
113700040708     C                   EVAL      VAOINC = SkSplitCSV(i)
113800040708     C                   ENDIF
113900040708     C*** LOC
114000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOC'
114100040708     C                   EVAL      VAOLOC = SkSplitCSV(i)
114200040708     C                   ENDIF
114300040708     C*** PRC
114400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRC'
114500040708     C                   EVAL      VAOPRC = SkSplitCSV(i)
114600040708     C                   ENDIF
114700040708     C*** NAC
114800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAC'
114900040708     C                   EVAL      VAONAC = SkSplitCSV(i)
115000040708     C                   ENDIF
115100040708     C*** FFD
115200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFFD'
115300040708     C                   EVAL      VAOFFD = SkSplitCSV(i)
115400040708     C                   ENDIF
115500040708     C*** RMP
115600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORMP'
115700040708     C                   EVAL      VAORMP = SkSplitCSV(i)
115800040708     C                   ENDIF
115900040708     C*** NAM
116000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAM'
116100040708     C                   EVAL      VAONAM = SkSplitCSV(i)
116200040708     C                   ENDIF
116300040708     C*** PAG
116400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPAG'
116500040708     C                   EVAL      VAOPAG = SkSplitCSV(i)
116600040708     C                   ENDIF
116700040708     C*** CTR
116800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCTR'
116900040708     C                   EVAL      VAOCTR = SkSplitCSV(i)
117000040708     C                   ENDIF
117100040708     C*** ZOR
117200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOZOR'
117300040708     C                   EVAL      VAOZOR = SkSplitCSV(i)
117400040708     C                   ENDIF
117500040708     C*** NO1
117600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO1'
117700040708     C                   EVAL      VAONO1 = SkSplitCSV(i)
117800030715     C                   ENDIF
117900040708     C*** NO2
118000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO2'
118100040708     C                   EVAL      VAONO2 = SkSplitCSV(i)
118200030715     C                   ENDIF
118300040708     C*** DDT
118400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODDT'
118500040708     C                   EVAL      VAODDT = SkSplitCSV(i)
118600030715     C                   ENDIF
118700040708     C*** RFA
118800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORFA'
118900040708     C                   EVAL      VAORFA = SkSplitCSV(i)
119000030715     C                   ENDIF
119100040708     C*** SPI
119200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSPI'
119300040708     C                   EVAL      VAOSPI = SkSplitCSV(i)
119400030715     C                   ENDIF
119500040708     C*** FLO
119600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
119700040708     C                   EVAL      VAOFLO = SkSplitCSV(i)
119800040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
119900080708     C* e la posizione 7 del VAOFLO
120000040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
120100040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
120200050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
120300030715     C                   ENDIF
120400161122     C*** FLO2
120500161122     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO2'
120600161122     C                   EVAL      w_VAOFLO2 = SkSplitCSV(i)
120700161122     C                   ENDIF
120800161122     C*** TSPS
120900161122     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTSPS'
121000161122     C                   EVAL      w_VAOTSPS = SkSplitCSV(i)
121100161122     C                   ENDIF
121200161122     C*** NOTE
121300161122     C                   IF        %trim(SkSplitFLD(i)) = 'VAONOTE'
121400161122     C                   EVAL      w_VAONOTE = SkSplitCSV(i)
121500161122     C                   ENDIF
121600161122     C*** ORDA1
121700161122     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORDA1'
121800161122     C                   EVALR     w_VAOORDA1 = %trim(SkSplitCSV(i))
121900161122     C                   ENDIF
122000161122     C*** ORAA1
122100161122     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORAA1'
122200161122     C                   EVALR     w_VAOORAA1 = %trim(SkSplitCSV(i))
122300161122     C                   ENDIF
122400161122     C*** ORDA2
122500161122     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORDA2'
122600161122     C                   EVALR     w_VAOORDA2 = %trim(SkSplitCSV(i))
122700161122     C                   ENDIF
122800161122     C*** ORAA2
122900161122     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORAA2'
123000161122     C                   EVALR     w_VAOORAA2 = %trim(SkSplitCSV(i))
123100161122     C                   ENDIF
123200161122     C*** EMLR
123300161122     C                   IF        %trim(SkSplitFLD(i)) = 'VAOEMLR'
123400161122     C                   EVAL      w_VAOEMLR = SkSplitCSV(i)
123500161122     C                   ENDIF
123600161122     C*** EMLRF
123700161122     C                   IF        %trim(SkSplitFLD(i)) = 'VAOEMLRF'
123800161122     C                   EVAL      w_VAOEMLRF = SkSplitCSV(i)
123900161122     C                   ENDIF
124000161122     C*** SMSR
124100161122     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSMSR'
124200161122     C                   EVAL      w_VAOSMSR = SkSplitCSV(i)
124300161122     C                   ENDIF
124400161122     C*** SMSRF
124500161122     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSMSRF'
124600161122     C                   EVAL      w_VAOSMSRF = SkSplitCSV(i)
124700161122     C                   ENDIF
124800091103     C*** DAR
124900050314     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
125000050412     C                   EVAL      PiStr=SkSplitCSV(i)
125100050412     C                   EXSR      CHKNUM
125200050412     C                   IF        PiInt=*on
125300050412     C                   Z-ADD     PiVal         VAODAR
125400050412     C                   ELSE
125500050412     C                   SETON                                        32
125600050412     C                   EVAL      VAODAR = *zeros
125700050412     C                   EVAL      vinmsg = %trimr(vinmsg)
125800050412     C                             + ' ' + 'VAODAR'
125900050412     C                   ENDIF
126000050314     C                   ENDIF
126100030715     C
126200030721     C*
126300030721     C* Reperisco quindi i campi numerici...
126400040708     C*** POE
126500040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
126600040723     C                   EVAL      PiStr=SkSplitCSV(i)
126700040723     C                   EXSR      CHKNUM
126800040723     C                   IF        PiInt=*on    AND
126900040723     C                             PiVal<=999   AND
127000040723     C                             PiVal>*zeros
127100040723     C                   Z-ADD     PiVal         VAOPOE
127200040723     C                   ELSE
127300040723     C                   SETON                                        32
127400040723     C                   EVAL      VAOPOE = *zeros
127500040723     C                   EVAL      vinmsg = %trimr(vinmsg)
127600040723     C                             + ' ' + 'VAOPOE'
127700040723     C                   ENDIF
127800040723     C                   ENDIF
127900040708     C*** COR
128000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
128100040708     C                   EVAL      PiStr=SkSplitCSV(i)
128200040708     C                   EXSR      CHKNUM
128300040708     C                   IF        PiInt=*on
128400040708     C                   Z-ADD     PiVal         VAOCOR
128500040708     C                   ELSE
128600040708     C                   SETON                                        31
128700040708     C                   EVAL      VAOCOR = *zeros
128800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
128900040708     C                             + ' ' + 'VAOCOR'
129000040708     C                   ENDIF
129100040708     C                   ENDIF
129200040708     C*** CAO
129300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
129400030721     C                   EVAL      PiStr=SkSplitCSV(i)
129500030721     C                   EXSR      CHKNUM
129600030721     C                   IF        PiInt=*on
129700030721     C                   Z-ADD     PiVal         Num5_0
129800040708     C                   MOVEL(p)  Num5_0        VAOCAO
129900030721     C                   ELSE
130000030721     C                   SETON                                        32
130100040708     C                   EVAL      VAOCAO = *zeros
130200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130300040708     C                             + ' ' + 'VAOCAO'
130400030721     C                   ENDIF
130500030721     C                   ENDIF
130600040708     C*** CRA
130700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
130800030721     C                   EVAL      PiStr=SkSplitCSV(i)
130900030721     C                   EXSR      CHKNUM
131000030721     C                   IF        PiInt=*on
131100040708     C                   Z-ADD     PiVal         VAOCRA
131200030721     C                   ELSE
131300030721     C                   SETON                                        32
131400040708     C                   EVAL      VAOCRA = *zeros
131500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131600040708     C                             + ' ' + 'VAOCRA'
131700030721     C                   ENDIF
131800030721     C                   ENDIF
131900040708     C*** CAR
132000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
132100040708     C                   EVAL      PiStr=SkSplitCSV(i)
132200040708     C                   EXSR      CHKNUM
132300040708     C                   IF        PiInt=*on
132400040708     C                   Z-ADD     PiVal         Num5_0
132500040708     C                   MOVEL(p)  Num5_0        VAOCAR
132600040708     C                   ELSE
132700040708     C                   SETON                                        31
132800040708     C                   EVAL      VAOCAR = *zeros
132900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
133000040708     C                             + ' ' + 'VAOCAR'
133100040708     C                   ENDIF
133200040708     C                   ENDIF
133300040708     C*** CRC
133400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
133500040708     C                   EVAL      PiStr=SkSplitCSV(i)
133600040708     C                   EXSR      CHKNUM
133700040708     C                   IF        PiInt=*on
133800040708     C                   Z-ADD     PiVal         VAOCRC
133900040708     C                   ELSE
134000040708     C                   SETON                                        32
134100040708     C                   EVAL      VAOCRC = *zeros
134200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
134300040708     C                             + ' ' + 'VAOCRC'
134400040708     C                   ENDIF
134500040708     C                   ENDIF
134600040708     C*** CAC
134700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
134800040708     C                   EVAL      PiStr=SkSplitCSV(i)
134900040708     C                   EXSR      CHKNUM
135000040708     C                   IF        PiInt=*on
135100040708     C                   Z-ADD     PiVal         Num5_0
135200040708     C                   MOVEL(p)  Num5_0        VAOCAC
135300040708     C                   ELSE
135400040708     C                   SETON                                        32
135500040708     C                   EVAL      VAOCAC = *zeros
135600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
135700040708     C                             + ' ' + 'VAOCAC'
135800040708     C                   ENDIF
135900040708     C                   ENDIF
136000040708     C*** DAR
136100090428     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
136200090428     C                   EVAL      PiStr=SkSplitCSV(i)
136300090428     C                   EXSR      CHKNUM
136400090428     C                   IF        PiInt=*on
136500090428     C                   Z-ADD     PiVal         VAODAR
136600090428     C                   ELSE
136700090428     C                   SETON                                        32
136800090428     C                   EVAL      VAODAR = *zeros
136900090428     C                   EVAL      vinmsg = %trimr(vinmsg)
137000090428     C                             + ' ' + 'VAODAR'
137100090428     C                   ENDIF
137200090428     C                   ENDIF
137300040708     C*** ORR
137400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
137500040708     C                   EVAL      PiStr=SkSplitCSV(i)
137600040708     C                   EXSR      CHKNUM
137700040708     C                   IF        PiInt=*on
137800040708     C                   Z-ADD     PiVal         VAOORR
137900040708     C                   ELSE
138000040708     C                   SETON                                        32
138100040708     C                   EVAL      VAOORR = *zeros
138200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
138300040708     C                             + ' ' + 'VAOORR'
138400040708     C                   ENDIF
138500040708     C                   ENDIF
138600030721     C*** NCL
138700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
138800030721     C                   EVAL      PiStr=SkSplitCSV(i)
138900030721     C                   EXSR      CHKNUM
139000030721     C                   IF        PiInt=*on
139100040723     C                   IF        PiVal > *zeros
139200040708     C                   Z-ADD     PiVal         VAONCL
139300040723     C                   ENDIF
139400030721     C                   ELSE
139500030721     C                   SETON                                        32
139600040708     C                   EVAL      VAONCL = *zeros
139700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139800040708     C                             + ' ' + 'VAONCL'
139900030721     C                   ENDIF
140000030721     C                   ENDIF
140100040708     C*** PKG
140200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
140300030721     C                   EVAL      PiStr=SkSplitCSV(i)
140400030721     C                   EXSR      CHKNUM
140500030721     C                   IF        PiNum=*on
140600040723     C                   IF        PiVal > *zeros
140700040708     C                   Z-ADD     PiVal         VAOPKG
140800040723     C                   ENDIF
140900030721     C                   ELSE
141000030721     C                   SETON                                        32
141100040708     C                   EVAL      VAOPKG = *zeros
141200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141300040708     C                             + ' ' + 'VAOPKG'
141400030721     C                   ENDIF
141500030721     C                   ENDIF
141600040708     C*** VLM
141700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
141800040708     C                   EVAL      PiStr=SkSplitCSV(i)
141900040708     C                   EXSR      CHKNUM
142000040708     C                   IF        PiNum=*on
142100040708     C                   Z-ADD     PiVal         VAOVLM
142200040708     C                   ELSE
142300040708     C                   SETON                                        32
142400040708     C                   EVAL      VAOVLM = *zeros
142500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
142600040708     C                             + ' ' + 'VAOVLM'
142700040708     C                   ENDIF
142800040708     C                   ENDIF
142900040708     C*** BNC
143000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
143100040708     C                   EVAL      PiStr=SkSplitCSV(i)
143200040708     C                   EXSR      CHKNUM
143300040708     C                   IF        PiInt=*on
143400040708     C                   Z-ADD     PiVal         VAOBNC
143500040708     C                   ELSE
143600040708     C                   SETON                                        32
143700040708     C                   EVAL      VAOBNC = *zeros
143800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
143900040708     C                             + ' ' + 'VAOBNC'
144000040708     C                   ENDIF
144100040708     C                   ENDIF
144200040708     C*** BLC
144300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
144400040708     C                   EVAL      PiStr=SkSplitCSV(i)
144500040708     C                   EXSR      CHKNUM
144600040708     C                   IF        PiInt=*on
144700040708     C                   Z-ADD     PiVal         VAOBLC
144800040708     C                   ELSE
144900040708     C                   SETON                                        32
145000040708     C                   EVAL      VAOBLC = *zeros
145100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
145200040708     C                             + ' ' + 'VAOBLC'
145300040708     C                   ENDIF
145400040708     C                   ENDIF
145500040708     C*** ATT
145600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
145700040708     C                   EVAL      PiStr=SkSplitCSV(i)
145800040708     C                   EXSR      CHKNUM
145900040708     C                   IF        PiInt=*on
146000040708     C                   Z-ADD     PiVal         VAOATT
146100040708     C                   ELSE
146200040708     C                   SETON                                        32
146300040708     C                   EVAL      VAOATT = *zeros
146400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
146500040708     C                             + ' ' + 'VAOATT'
146600040708     C                   ENDIF
146700040708     C                   ENDIF
146800040708     C*** MTC
146900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
147000040708     C                   EVAL      PiStr=SkSplitCSV(i)
147100040708     C                   EXSR      CHKNUM
147200040708     C                   IF        PiInt=*on
147300040708     C                   Z-ADD     PiVal         VAOMTC
147400040708     C                   ELSE
147500040708     C                   SETON                                        32
147600040708     C                   EVAL      VAOMTC = *zeros
147700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
147800040708     C                             + ' ' + 'VAOMTC'
147900040708     C                   ENDIF
148000040708     C                   ENDIF
148100040708     C*** KSC
148200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
148300030721     C                   EVAL      PiStr=SkSplitCSV(i)
148400030721     C                   EXSR      CHKNUM
148500030721     C                   IF        PiInt=*on      AND
148600030721     C                             PiVal<=9999999 AND
148700030721     C                             PiVal>*zeros
148800040708     C                   Z-ADD     PiVal         VAOKSC
148900030721     C                   ELSE
149000030721     C                   SETON                                        32
149100040708     C                   EVAL      VAOKSC = *zeros
149200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
149300040708     C                             + ' ' + 'VAOKSC'
149400030721     C                   ENDIF
149500030721     C                   ENDIF
149600040708     C*** POR
149700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
149800030721     C                   EVAL      PiStr=SkSplitCSV(i)
149900030721     C                   EXSR      CHKNUM
150000030721     C                   IF        PiInt=*on    AND
150100030721     C                             PiVal<=999   AND
150200030721     C                             PiVal>*zeros
150300040708     C                   Z-ADD     PiVal         VAOPOR
150400030721     C                   ELSE
150500030721     C                   SETON                                        32
150600040708     C                   EVAL      VAOPOR = *zeros
150700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
150800040708     C                             + ' ' + 'VAOPOR'
150900030721     C                   ENDIF
151000030721     C                   ENDIF
151100040708     C*** POC
151200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
151300040708     C                   EVAL      PiStr=SkSplitCSV(i)
151400040708     C                   EXSR      CHKNUM
151500040708     C                   IF        PiInt=*on    AND
151600040708     C                             PiVal<=999   AND
151700040708     C                             PiVal>*zeros
151800040708     C                   Z-ADD     PiVal         VAOPOC
151900040708     C                   ELSE
152000040708     C                   SETON                                        32
152100040708     C                   EVAL      VAOPOC = *zeros
152200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
152300040708     C                             + ' ' + 'VAOPOC'
152400040708     C                   ENDIF
152500040708     C                   ENDIF
152600040708     C*** STO
152700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
152800040708     C                   EVAL      PiStr=SkSplitCSV(i)
152900040708     C                   EXSR      CHKNUM
153000040708     C                   IF        PiInt=*on
153100040708     C                   Z-ADD     PiVal         VAOSTO
153200040708     C                   ELSE
153300040708     C                   SETON                                        32
153400040708     C                   EVAL      VAOSTO = *zeros
153500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
153600040708     C                             + ' ' + 'VAOSTO'
153700040708     C                   ENDIF
153800040708     C                   ENDIF
153900090428     C*
154000091103     C********** GESTIONE CAMPI PERSONALIZZATI
154100120120
154200130716     C*** W_VAORSR
154300130716     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAORSR'
154400130716     C                   EVAL      VAORSR = %trim(SkSplitCSV(i))
154500130716     C                   EVAL      VAORER = VAORSR
154600130513     C                   ENDIF
154700121207
154800090428     C*
154900090428     C****************************************
155000090428     C*
155100030715     C*
155200030715     C                   ENDIF
155300030715     C                   EVAL      i = i + 1
155400030715     C                   ENDDO
155500040723     C*
155600121207     C***  ===>  Gestione campi "particolari" fuori ciclo
155700121207     C*
155800130513
155900030715     C                   ENDIF
156000020204     C*
156100010607     C                   ENDSR
156200010607     C*----------------------------------------------------*
156300080702
156400080702
156500080702     C*----------------------------------------------------*
156600080702     C*  CONTROLLO NUMERICITA' CAMPI
156700080702     C*----------------------------------------------------*
156800080702     C     CHKNUM        BEGSR
156900080702     C*
157000080702     C                   IF        PiDecChr = *blanks
157100080702     C                   EVAL      PiDecChr = ','
157200080702     C                   ENDIF
157300080702     C*
157400080702     C                   callp     UBISNUM_Check(PiStr
157500080702     C                                          :PiDecChr
157600080702     C                                          :PiVal
157700080702     C                                          :PiNum
157800080702     C                                          :PiInt)
157900080702     C*
158000080702     C                   ENDSR
158100080702     C***
158200080702
158300010601
158400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
158500010601     C     repfil        BEGSR
158600010601     C*
158700010601     C                   if        invfil = *zeros and
158800010601     C                             depfil > *zeros and
158900010629     C                             (vinflg = *blanks or
159000010629     C                              vinflg = *zeros)
159100010601     C
159200010601     C                   eval      invfil = depfil
159300010601     C                   endif
159400010601     C*
159500010601     C                   if        depfil <> invfil and
159600010601     C                             invfil > *zeros
159700010601     C                   eval      flgMulti = '1'
159800010601     C                   if        vinflg = *blanks
159900010601     C                   add       1             cntNonEl
160000010601     C                   endif
160100010601     C                   endif
160200010601     C*
160300010601     C                   if        vinflg = '2'
160400010601     C                   eval      flgStato = '2'
160500010601     C                   endif
160600010601     C*
160700010601     C                   ENDSR
160800010601     C***
160900010330
161000010330
161100990920      /TITLE Invio dei dati al punto operativo.
161200000613     C     invio         BEGSR
161300990920     C*
161400990920     C                   reset                   dscmz
161500010601     C                   move      invfil        cmzdst
161600990920     C                   eval      cmzfld = vlrfou
161700990920     C                   eval      cmzmbd = vlrhdl
161800990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
161900000710     C                   if        prmfir = *blanks
162000040708     C                   eval      cmzfla = 'FNVAO00F'
162100040708     C                   eval      cmzmba = 'FNVAO00F'
162200000710     C                   else
162300000710     C                   eval      cmzfla = prmfir
162400000710     C                   eval      cmzmba = prmfir
162500000710     C                   endif
162600990920     C                   eval      cmznrr = *zeros
162700990920     C                   move      §ctrok        cmznrr
162800021018     C                   eval      cmzlba = vlrfl1
162900990920     C                   call(e)   'TIS711C'
163000990920     C                   parm                    dscmz
163100990921     C                   parm      *blanks       esito
163200990920     C                   if        %error
163300990920     C                             or cmzerr = '1'
163400990921     C                             or esito  = '1'
163500000710     C                   eval      wrkesito = '3'
163600990920     C                   endif
163700990920     C*
163800000613     C                   ENDSR
163900010601
164000010601
164100010601      /TITLE Invio dei dati al punto operativo.
164200010601     C     opeini        BEGSR
164300010601     C*
164400010601     C* Inizializzo flag e contatori operativi
164500010601     C                   movel     '0'           flgGiro           1
164600010601     C                   movel     '0'           flgMulti          1
164700010601     C                   movel     '1'           flgStato          1
164800010615     C                   movel     '0'           flgOk             1
164900010601     C                   z-add     *zeros        cntNonEl         10 0
165000010601     C                   z-add     *zeros        depfil            3 0
165100010601     C                   z-add     *zeros        invfil            3 0
165200010601     C*
165300010601     C                   ENDSR
165400010601     C***
165500050421
165600050421
165700091130     C     *pssr         BEGSR
165800050421     C*
165900091130     C                   eval      wrkesito = '2'
166000050421     C*
166100091130     C                   ENDSR     '*CANCL'
166200050421     C***
166300060306
166400060306
166500060306     C*-------------------------------------------------------------------------*
166600060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
166700060306     C*-------------------------------------------------------------------------*
166800060306     C     sr_Control    begsr
166900060306     C*
167000060306     C* inizializzo a ok il flag d controllo abilitazione
167100060306     C                   eval      Cli_Abil = *on
167200060306     C
167300060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
167400060306     C                   clear                   woriIDC
167500060306     C                   eval      kteCOD = 'VAP'
167600060306     C                   eval      kteKE1 = oriIDC
167700060306     C     keytbe        chain     TNTBE01L
167800060306     C                   if        %found(Tntbe01l)
167900060306     C                   eval      woriIDC = TbeKe2
168000060306     C                   else
168100060306     C                   eval      Cli_Abil = *off
168200060306     C                   endif
168300060306     C*
168400060306     C                   endsr
168500010330
168600010330
168700000613     C     *inzsr        BEGSR
168800990910     C*
168900990910     C     *entry        plist
169000990920     C                   parm                    tivlrds
169100990921     C                   parm      wrkesito      esito
169200000724     C                   parm                    prmlit
169300000710     C                   parm                    prmfir
169400010330     C*
169500010330     C* CALCOLA LA DATA CORRENTE
169600091127     C                   z-add     *zeros        datcor            8 0
169700091127     C                   time                    wn14             14 0
169800091127     C                   movel     wn14          wn6               6 0          *ORA (6)
169900130516     C                   eval      datcor = %dec(%date() : *ISO)
170000040723     C*
170100040723     C* Campi riferito al database
170200040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
170300040723     C     *like         define    tbeke1        kteKE1
170400040723     C*
170500040723     C* Definizione chiavi di lettura
170600040723     C* TNTBE01L - parziale
170700040723     C     keytbe        klist
170800040723     C                   kfld                    kteCOD                         *UTENTE
170900040723     C                   kfld                    kteKE1                         *CODICE
171000000613     C*
171100000613     C                   ENDSR
171200000613     C***
171300990908
171400161121** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
171500161121W_VAORSR;VAOINR;VAOCAR;VAOLOR;VAOPRR;VAOTER;VAONCL;VAOPKG;VAORSC;VAOINC;
171600161122VAOCAC;VAOLOC;VAOPRC;VAODAR;VAOORR;VAOEMLR;
171700161121
