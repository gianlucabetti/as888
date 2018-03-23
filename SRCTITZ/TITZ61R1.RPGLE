000100040708      /TITLE Upload via Internet: traduzione in FNVAOWWR.
000200121129      *
000300121129      * PARTICOLARITA':
000400121207      * per esigenze interne nostre deve essere creata una stringa di 15 caratteri comprensivi dei
000500121207      * seguenti elementi
000600121207      * - prime due caratteri devono riportare la dicitura RT
000700121207      * - il terzo carattere indica il marchio che può essere rilevato dal nostro campo Marchio
000800121207      * corrispondente al terzo carattere che può essere 0-1-2-3-4
000900121207      * - dal quarto carattere riportare i dati presenti nel campo N. lista
001000121207      * - dall'undicesimo carattere inserire i dati riportati nel campo Ord.(questo dato deve essere
001100121207      * fisso di 3 caratteri quindi in caso di un dato con un numero di cifre inferiori a 3 si deve
001200121207      * aggiungere di default degli zeri davanti)
001300121207      * - dal quattordicesimo carattere inserire i dati riportati nel campo Mult. (questo dato deve
001400121207      * essere fisso di 2 caratteri quindi in caso di un dato con un numero di cifre inferiori a 2
001500121207      * si deve aggiungere di default lo zero davanti)
001600121207      * - dal quattordicesimo carattere inserire i dati riportati nel campo Mult. (questo dato deve
001700121207      * essere fisso di 2 caratteri quindi in caso di un dato con un numero di cifre inferiori a 2
001800121207      * si deve aggiungere di default lo zero davanti)
001900140401      * - se non passati nei parametri, CRC e KSC vengono presi da COR
002000121129      *
002100121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
002200120209     F*
002300040723     Ffnacr01l  if   e           k disk
002400090618     Ftntbe01l  if   e           k disk
002500990910     Ftivin00r  uF   E             DISK    usropn
002600040708     FFnvaowwr  O    E             DISK    usropn
002700040723     Ftiori00f  O    E             DISK
002800050826     FTIVGD00F  O    E             DISK
002900000313     D*
003000040709     D*------------
003100040709     D* SCHIERE A PROGRAMMA
003200040709     D*------------
003300040709     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
003400010330     D*----------------------------------------------------
003500010330     D* DICHIARAZIOINE VARIABILI DI WRK
003600010330     D*----------------------------------------------------
003700010330     D dscmz         e ds                  inz
003800010330     D psds           sds
003900010330     D  procname         *PROC
004000010330     D tivlrds       e ds                  extname(tivlr00f)
004100050826     D fnvapds       e ds                  extname(fnvap00f)
004200040723     D dorm01        e ds
004300040723     D dfar          e ds
004400090618     d trul33ds      e ds
004500090618     d kpjba         e ds
004600140716     D fnvaoear      e ds                  inz
004700140716     D                                     prefix(w_)
004800010330     D esito           s              1
004900010330     D prmlit          s             10
005000010330     D prmfir          s             10
005100010330     D wrkesito        s                   like(esito)
005200010330     D rrnum           s              6  0 INZ(*zeros)
005300040709     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
005400060306     D Cli_Abil        s              1    inz(*off)
005500060306     D woriIDC         s                   like(oriIDC) inz(*blanks)
005600090618     d §numori         s              7  0 inz
005700120123     d wData_EUR       s               d   datfmt(*eur)
005800120123     d wData_ISO       s               d   datfmt(*iso)
005900120905     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
006000120905     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
006100120905     D Intestazioni    s               n   inz(*on)
006200030715     D*------------------
006300030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
006400030715     D*------------------
006500030715     D SkSplitFLD      S             10    DIM(100)
006600030715     D SkSplitCSV      S            256    DIM(100)
006700030715     D CharCSV         S              1
006800030715     D CharTXT         S              1
006900030715     D CharNUM         S              1
007000030715     D posDa           S              3  0 INZ(*zeros)
007100030715     D posA            S              3  0 INZ(*zeros)
007200030715     D i               s              3  0 INZ(1)
007300030715     D wGiro           s              1  0 INZ(*zeros)
007400030822     D*------------------
007500030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
007600030822     D*------------------
007700030822     D posDaDft        S              3  0 INZ(*zeros)
007800030822     D posADft         S              3  0 INZ(*zeros)
007900030822     D j               s              3  0 INZ(1)
008000030822     D wGiroDft        s              1  0 INZ(*zeros)
008100040723     D*------------------
008200040723     D* DS REPERIMENTO DATI CLIENTE
008300040723     D*-------------------
008400040723     D BS69DS        E DS                  EXTNAME(TIBS69DS)
008500040723     D ACODS         E DS                  EXTNAME(CNACO00F)
008600040723     D INDDS         E DS                  EXTNAME(CNIND00F)
008700040723     D CLPDS         E DS                  EXTNAME(CNCLP00F)
008800040723     D CLSDS         E DS                  EXTNAME(FNCLS00F)
008900121129     D*------------------
009000121207     D wRFA1           s              1
009100121207     D wRFA2           s              7
009200121207     D wRFA3           s              3
009300121207     D wRFA4           s              2
009400141006     D w70             s             70
009500080702
009600080702     D*------------------
009700080702     D* LINKING A DEFINIZIONI ESTERNE
009800080702     D*------------------
009900090428     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
010000090428     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
010100091026
010200091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
010300091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
010400091026
010500080702
010600031201     D*------------------
010700031201     D* Costanti
010800031201     D*------------------
010900031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
011000031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
011100010330
011200010330
011300010330
011400000913     C                   reset                   rrnum
011500990921     C                   reset                   esito
011600990921     C                   reset                   wrkesito
011700010601     C*
011800010601     C                   exsr      opeini
011900041011     C                   exsr      PREELA
012000040708     C                   exsr      rwvao
012100010601     C*
012200010601     C                   seton                                        lr
012300010601
012400010601
012500010601
012600010601
012700010601     C*--------------------------------------------------------
012800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
012900010601     C*--------------------------------------------------------
013000010601     C     PREELA        BEGSR
013100010601     C*
013200010601     C* SE OCCORRE SPEDIRE IN FILIALE
013300041011     C                   if        flgGiro = '0'
013400010601     C*
013500010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
013600010601     C                   eval      flgGiro = '1'
013700041011     C*
013800041011     C                   endif
013900010601     C*
014000010601     C                   ENDSR
014100010601     C***
014200010601
014300010601
014400010601     C*--------------------------------------------------------
014500010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014600010601     C*--------------------------------------------------------
014700010601     C     ENDELA        BEGSR
014800000616     C*
014900120209     C                   ENDSR
015000010601
015100010601
015200010330     C*--------------------------------------------------------
015300040708     C* RWVAO   LEGGE TIVIN00R E SCRIVE FNVAOWWF              *
015400010330     C*--------------------------------------------------------
015500040708     C     RWVAO         BEGSR
015600010330     C*
015700010330     C                   if        not %open(tivin00r)
015800010330     C                   open      tivin00r
015900010330     C                   endif
016000040708     C                   if        not %open(fnvaowwr)
016100040708     C                   open      fnvaowwr
016200010330     C                   endif
016300140716     C*
016400140716     C* Mi predispongo per scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
016500140716     C                   call      'TIS7T7ER'
016600140716     C                   parm      'O'           pIn_Opz           1
016700140716     C                   parm                    tivlrds
016800140716     C                   parm                    FNVAOEAR
016900140716     C                   parm                    oriPRG
017000140716     C                   parm      *zeros        pIn_Dst           3
017100140716     C                   parm      *blanks       pOut_Esito        1
017200140716     C                   if        pOut_Esito = '2'
017300140716     C                   goto      FINE
017400140716     C                   endif
017500010330     C*
017600010604     C                   clear                   §CTROK
017700010604     C                   clear                   §CTRMO
017800010604     C                   clear                   §CTRNO
017900120123     C*
018000120905     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
018100120905     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
018200120905     C                   IF        Intestazioni = *off
018300120123     C* Eseguo primo giro x by-passare la mancanza della 1° riga con le intestazioni
018400120123     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
018500120123     C                   exsr      inzvar
018600120123     C                   exsr      impvao
018700120905     C                   ENDIF
018800010330     C*
018900030822     C                   do        *HIVAL
019000010330     C*
019100030822     C                   read      tivin00r                               70
019200010618     C*
019300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019400010618     C                   if        vinflg = '1'
019500010618     C                   eval      flgOk = '1'
019600010618     C                   endif
019700010618     C*
019800040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
019900040510     C                   z-add     *zeros        wLenVINDTA        4 0
020000040510     C                   z-add     *zeros        wFlgVINDTA        1 0
020100040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
020200040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
020300040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
020400040510     C                   z-add     1             wFlgVINDTA
020500040510     C                   leave
020600040510     C                   endif
020700040510     C                   enddo
020800040510     C*
020900040218     C                   if        vindta > *blanks AND
021000040510     C                             wFlgVINDTA = 1
021100010330     C                   add       1             rrnum
021200010330     C*
021300010601     C                   if        *in70 = *off and
021400010330     C                             (vinflg = *blanks
021500010330     C                              or vinflg = '0'
021600010330     C                              or vinflg = '2')
021700010330     C*
021800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021900010711     C                   if        vinflg = *blanks or vinflg = '0'
022000010711     C                   clear                   vinmsg
022100010711     C                   endif
022200010601     C*
022300010330     C                   exsr      inzvar
022400040708     C                   exsr      impvao
022500151019     C*
022600151019     C* Stacco subito un numeratore univoco "ORM"
022700151019     C                   exsr      RTVNUM
022800140716     C*
022900140728     C* Verifico se trattasi di formato ORM "esteso"
023000140716     C                   setoff                                       53
023100140728     C                   if        w_VAOFLO2  <> *blanks OR
023200140728     C                             w_VAOTSPS  <> *blanks OR
023300140728     C                             w_VAONOTE  <> *blanks OR
023400140728     C                             w_VAOORDA1 <> *blanks OR
023500140728     C                             w_VAOORAA1 <> *blanks OR
023600140728     C                             w_VAOORDA2 <> *blanks OR
023700140728     C                             w_VAOORAA2 <> *blanks OR
023800140728     C                             w_VAOEMLR  <> *blanks OR
023900140728     C                             w_VAOEMLRF <> *blanks OR
024000140728     C                             w_VAOSMSR  <> *blanks OR
024100140728     C                             w_VAOSMSRF <> *blanks OR
024200140728     C                             w_VAOORIP  <> *blanks OR
024300140728     C                             w_VAODOIO  <> *blanks
024400140716     C                   seton                                        53
024500140716     C                   endif
024600010601     C*
024700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
024800040708     C                   eval      depfil = VAOPOE
024900010601     C                   exsr      repfil
025000010601     C                   if        depfil = invfil
025100021025     C                   if        vlrpoi = 999
025200040708     C                   move(P)   invfil        VAOPOE
025300021025     C                   else
025400040708     C                   move(P)   vlrpoi        VAOPOE
025500021025     C                   endif
025600010601     C*
025700010604     C* Ebbene...
025800010604     C*
025900030822     C  N31              add       1             §CTROK            7 0
026000030822     C   32              add       1             §CTRMO            7 0
026100030822     C   31              add       1             §CTRNO            7 0
026200030822     C                   if        wGiro = 2
026300120123     C  N31              EXSR      CHKCOR
026400120123     C  N31              EXSR      WRIORI
026500120123     C  N31              EXSR      WRIVGD
026600120123     C  N31              write     FNVAO000
026700140716     C*
026800140728     C* Se richiesta scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
026900140728     C                   if        *in53 and not *in31
027000151019     C                   exsr      EXEFORZ
027100140716     C                   call      'TIS7T7ER'
027200140716     C                   parm      'W'           pIn_Opz           1
027300140716     C                   parm                    tivlrds
027400140716     C                   parm                    FNVAOEAR
027500140716     C                   parm                    oriPRG
027600140716     C                   parm      *zeros        pIn_Dst           3
027700140716     C                   parm      *blanks       pOut_Esito        1
027800140716     C                   endif
027900140716     C                   if        pOut_Esito = '2'
028000140716     C                   seton                                        32
028100140716     C                   endif
028200030822     C                   endif
028300010604     C*
028400010604     C                   if        *in31 = *off and
028500010604     C                             *in32 = *off
028600010604     C                   eval      vinflg = '1'
028700010604     C                   else
028800010604     C                   eval      vinflg = '2'
028900010604     C                   endif
029000010604     C                   endif
029100010601     C*
029200010604     C                   endif
029300010604     C*
029400010330     C                   else
029500010330     C                   eval      vinflg = '1'
029600010330     C                   endif
029700010601     C*
029800010601     C  N70              update    tivin000
029900010330     C*
030000030822     C  N70              enddo
030100140716     C*
030200140716     C     FINE          tag
030300140716     C*
030400140716     C* Chiusura per scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
030500140716     C                   call      'TIS7T7ER'
030600140716     C                   parm      'C'           pIn_Opz           1
030700140716     C                   parm                    tivlrds
030800140716     C                   parm                    FNVAOEAR
030900140716     C                   parm                    oriPRG
031000140716     C                   parm      *zeros        pIn_Dst           3
031100140716     C                   parm      *blanks       pOut_Esito        1
031200010601     C*
031300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
031400010601     C                   if        cntNonEl = *zeros or
031500010601     C                             flgMulti = '0'
031600010330     C* Se non ci sono record con errori ...
031700010601     C                   if        §ctrno = 0 and
031800010604     C                             §ctrmo = 0 and
031900010601     C                             flgStato <> '2'
032000010330     C* ... restituisco esito OK.
032100010330     C                   eval      wrkesito = '0'
032200010330     C                   else
032300010330     C                   if        §ctrok > 0
032400010330     C                   eval      wrkesito = '1'
032500010330     C                   else
032600010615     C                   if        flgOk = '0'
032700010615     C                   eval      wrkesito = '2'
032800010615     C                   else
032900010615     C                   eval      wrkesito = '6'
033000010615     C                   endif
033100010330     C                   endif
033200010330     C                   endif
033300010601     C                   else
033400010601     C                   eval      wrkesito = '9'
033500010601     C                   endif
033600010330     C*
033700010330     C                   if        %open(tivin00r)
033800010330     C                   close     tivin00r
033900010330     C                   endif
034000040708     C                   if        %open(fnvaowwr)
034100040708     C                   close     fnvaowwr
034200010330     C                   endif
034300010601     C*
034400010601     C                   if        vlrpoi <> 999
034500010601     C                   eval      invfil = vlrpoi
034600010601     C                   endif
034700010330     C*
034800010330     C                   if        §ctrok > 0
034900010601     C                             and invfil > *zeros
035000010330     C                   exsr      invio
035100140716     C*
035200140716     C* Invio dati estensione ORM (FNVAOE0F) => richiamo l'apposito driver
035300140716     C                   move      invfil        cmzdst
035400140716     C                   call      'TIS7T7ER'
035500140716     C                   parm      'S'           pIn_Opz           1
035600140716     C                   parm                    tivlrds
035700140716     C                   parm                    FNVAOEAR
035800140716     C                   parm                    oriPRG
035900140716     C                   parm      cmzdst        pIn_Dst           3
036000140716     C                   parm      *blanks       pOut_Esito        1
036100010330     C                   endif
036200010601     C*
036300010618     C                   if        flgGiro = '1'
036400010601     C                   exsr      endela
036500010618     C                   endif
036600010330     C*
036700010330     C                   ENDSR
036800010330     C***
036900151019
037000151019
037100151019     C*------------------------------------------------------------------------*
037200151019     C* RTVNUM - Stacco un numeratore univoco "ORM"
037300151019     C*------------------------------------------------------------------------*
037400151019     C     RTVNUM        BEGSR
037500151019     C*
037600151019     C* Stacco un numeratore da AZNUM
037700151019     C                   clear                   §numori
037800151019     C                   clear                   trul33ds
037900151019     C                   eval      I33OPE = *zeros
038000151019     C                   eval      I33CNU = 600
038100151019     C                   eval      I33NUM = 1
038200151019     C                   movel     TRUL33DS      KPJBU
038300151019     C                   call      'TRUL33R'
038400151019     C                   parm                    KPJBA
038500151019     C                   movel     KPJBU         TRUL33DS
038600151019     C                   if        O33ERR = *zeros
038700151019     C                   z-add     O33NRF        §numori
038800151019     c                   else
038900151019     c                   eval      §numori = *all'9'
039000151019     C                   seton                                        31        * errore bloccante
039100151019     C                   endif
039200151019     C*
039300151019     C                   eval      vaoNOR = §numori
039400151019     C*
039500151019     C                   ENDSR
039600151019
039700151019
039800151019     C*------------------------------------------------------------------------*
039900151019     C* EXEFORZ - Eseguo forzatire di base su formato ORM "esteso"
040000151019     C*------------------------------------------------------------------------*
040100151019     C     EXEFORZ       BEGSR
040200151019     C*
040300151019     C                   move(p)   VAOPOE        w_vaopoe                       * forzatura necessar
040400151019     C                   move(p)   VAOCOR        w_vaocor                       * forzatura necessar
040500151019     C                   move(p)   VAONOR        w_vaonor                       * forzatura necessar
040600151019     C*
040700151019     C                   ENDSR
040800140716
040900040723
041000040723
041100040723     C*------------------------------------------------------------------------*
041200040723     C* CHKCOR - Considerazioni su VAOCOR
041300040723     C*------------------------------------------------------------------------*
041400040723     C     CHKCOR        BEGSR
041500040723     C*
041600040723     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
041700040723     C                   if        vaoTCO = 'F'
041800040723     C* Quindi verifico se il codice ordinante non è valorizzato
041900040723     C                   if        vaoCOR = *zeros  AND
042000040723     C                             vaoRSO = *blanks AND
042100040723     C                             vaoINO = *blanks AND
042200040723     C                             vaoCAO = *blanks AND
042300040723     C                             vaoLOO = *blanks AND
042400040723     C                             vaoPRO = *blanks AND
042500040723     C                             vaoNAO = *blanks
042600040723     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
042700040723     C                   movel     '0'           wFlgAcr           1
042800040723     C                   move      vlrKSC        wCli              7 0
042900040723     C                   move(p)   wCli          acrCRO
043000040723     C     acrCRO        setll     fnacr01l
043100040723     C                   if        %found(fnacr01l)
043200040723     C                   read      fnacr01l
043300040723     C                   dow       not %eof(fnacr01l)
043400040723     C                   movel     acrCRO        wCliAcr           7 0
043500040723     C                   if        wCliAcr = wCli
043600040723     C                   eval      vaoCOR = acrCRO
043700040723     C                   movel     '1'           wFlgAcr
043800040723     C                   leave
043900040723     C                   else
044000040723     C                   leave
044100040723     C                   endif
044200040723     C                   read      fnacr01l
044300040723     C                   enddo
044400040723     C                   endif
044500040723     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
044600040723     C* dati anagrafici dell'ordinante dal piano dei conti
044700040723     C                   if        wFlgAcr = '0'
044800040723     C                   clear                   BS69DS
044900040723     C                   clear                   ACODS
045000040723     C                   clear                   INDDS
045100040723     C                   clear                   CLPDS
045200040723     C                   clear                   CLSDS
045300040723     C                   move(P)   vlrKSC        I69KAC
045400040723     C                   move(P)   vlrKSC        I69KIN
045500040723     C                   call      'TIBS69R'
045600040723     C                   parm                    BS69DS
045700040723     C                   parm                    ACODS
045800040723     C                   parm                    INDDS
045900040723     C                   parm                    CLPDS
046000040723     C                   parm                    CLSDS
046100040723     C     O69ERR        ifne      '1'
046200040723     C                   eval      vaoRSO = ACORAG
046300040723     C                   eval      vaoINO = INDVIA
046400040723     C                   movel(P)  INDCAP        vaoCAO
046500040723     C                   eval      vaoLOO = INDCIT
046600040723     C                   eval      vaoPRO = INDPRV
046700040723     C                   eval      vaoNAO = INDSTA
046800040723     C                   endif
046900040723     C                   endif
047000040723     C                   endif
047100040723     C                   endif
047200040723     C*
047300040723     C                   ENDSR
047400040723
047500040723
047600040723     C*------------------------------------------------------------------------*
047700040723     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
047800040723     C*------------------------------------------------------------------------*
047900040723     C     WRIORI        BEGSR
048000040723     C*
048100090618     C* Come prima cosa stacco un numeratore da AZNUM
048200151019     C***                clear                   trul33ds
048300151019     C***                eval      I33OPE = *zeros
048400151019     C***                eval      I33CNU = 600
048500151019     C***                eval      I33NUM = 1
048600151019     C***                movel     TRUL33DS      KPJBU
048700151019     C***                call      'TRUL33R'
048800151019     C***                parm                    KPJBA
048900151019     C***                movel     KPJBU         TRUL33DS
049000151019     C***                if        O33ERR = *zeros
049100151019     C***                z-add     O33NRF        §numori
049200151019     c***                else
049300151019     c***                eval      §numori = *all'9'
049400151019     C***                endif
049500040723     C*
049600040723     C* Quindi imposto i campi "extra-VAO"
049700040723     C                   if        vlrKSC = '0ORM0000'
049800040723     C*
049900040723     C                   if        vaoCOR > *zeros
050000040723     C                   movel     vaoCOR        wrkCOR7           7 0
050100040723     C*
050200040723     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
050300040723     C* codice padre. Se è un padre assegno già il codice ordinante.
050400040723     C                   clear                   tibs10ds
050500040723     C                   eval      d10drf = datcor                              *data riferimento
050600040723     C                   eval      d10paf = 'P'                                 *cerca il padre
050700040723     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
050800091026     C*
050900091026     C* Preparazione chiamata al TIBS10R1
051000091026     C                   clear                   tibs10r1ds
051100091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
051200091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
051300091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
051400091026     C*
051500091026     C                   call      'TIBS10R1'
051600091026     C                   parm                    tibs10r1ds
051700091026     C*
051800091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
051900040723     C*
052000040723     C                   if        d10err <> *blanks or                         *No unif. o padre
052100040723     C                             d10cop =  *zeros  or
052200040723     C                             d10cop =  d10cod
052300040723     C                   move      d10cod        oriIDC
052400040723     C                   else                                                   *Figlio
052500040723     C                   move      d10cop        oriIDC
052600040723     C                   endif
052700040723     C*
052800040723     C                   else
052900040723     C                   eval      oriIDC = *zeros
053000040723     C                   endif
053100040723     C*
053200040723     C                   else
053300040723     C                   eval      oriIDC = vlrKSC
053400040723     C                   endif
053500040723     C*
053600040723     C                   eval      oriPRG = §numori
053700040723     C                   eval      oriDER = datcor
053800040723     C                   eval      oriOER = wn6
053900040723     C                   eval      oriPOE = vaoPOE
054000040723     C                   eval      oriNSR = vaoNSR
054100040723     C                   eval      oriNOR = vaoNOR
054200040723     C                   eval      oriNRV = vaoNRV
054300040723     C                   eval      oriFLO = *blanks
054400040723     C*
054500040723     C* Infine valorizzo la chiave esterna sul file FNVAO
054600040723     C                   eval      dorm01 = vaoFLO
054700040723     C                   movel     §numori       §ormpg
054800050412     C*
054900130306     C* valorizzo DORM01 (in questo caso non ci sonodefault)
055000050412     C*
055100050412     C* Infine riassegno il campo VAO contenete i flag operativi precedentemente ridefiniti
055200040723     C                   eval      vaoFLO = dorm01
055300040723     C*
055400040723     C* ...quindi scrivo il file TIORI00F
055500120905     C  N55              WRITE     TIORI000
055600040723     C*
055700040723     C                   ENDSR
055800050826
055900050826
056000050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
056100050826     C     wriVGD        BEGSR
056200050826     C*
056300050826     C* Reperisco la descrizione della fase da tabella
056400050826     C                   movel(p)  'FAR'         KteCOD
056500050826     C                   movel(p)  '000'         KteKE1
056600050826     C     KEYtbe        chain     tntbe01l
056700050826     C                   if        %found(tntbe01l)
056800050826     C                   movel(P)  tbeuni        DFAR
056900050826     C                   else
057000050826     C                   clear                   DFAR
057100050826     C                   endif
057200050826     C*
057300050826     C* Valorizzo buffer record
057400050826     C                   clear                   FNVAPDS
057500050826     C                   eval      vapIDC = oriIDC
057600050826     C                   eval      vapPOE = oriPOE
057700050826     C                   eval      vapRFA = vaoRFA
057800050826     C                   eval      vapPOG = oriPOE
057900050826     C                   eval      vapDAE = oriDER
058000050826     C                   eval      vapORE = oriOER
058100050826     C                   if        d§fardva <> *blanks
058200050826     C                   eval      vapDFA = d§fardva
058300050826     C                   else
058400050826     C                   eval      vapDFA = d§fardes
058500050826     C                   endif
058600050826     C*
058700050826     C                   clear                   tivgd000
058800080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
058900050826     C                   eval      vgdTIP = 'VP'
059000050826     C                   eval      vgdKSU = vapIDC
059100050826     C                   eval      vgdTSC = 'WW'
059200050826     C                   eval      vgdDAT = datcor
059300151207     C                   eval      vgdPGM = procname
059400060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
059500060306     C                   exsr      sr_Control
059600060306     C                   if        Cli_Abil = *on
059700060306     C                   eval      vgdKSU = woriIDC
059800120905     C  N55              write     tivgd000
059900060306     C                   endif
060000050826     C*
060100050826     C                   ENDSR
060200050826     C*------------------------------------------------------------------------*
060300010601
060400010601
060500010330     C*----------------------------------------------------*
060600030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
060700010330     C*----------------------------------------------------*
060800010330     C     INZVAR        BEGSR
060900010330     C*
061000030822     C* Inizializzo variabili di wrk
061100020204     C                   Z-ADD     *zeros        Num5_0            5 0
061200120124
061300030822     C*
061400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
061500120123     C                   CLEAR                   FNVAO000
061600140728     C                   CLEAR                   FNVAOEAR
061700120123     C                   CLEAR                   TIORI000
061800030822     C                   CLEAR                   SkSplitCSV
061900030822     C                   EVAL      i = 1
062000030822     C                   EVAL      posDa = *zeros
062100030822     C                   EVAL      posA  = *zeros
062200030822     C*
062300030822     C* Reimposto i valori di default
062400030822     C                   EXSR      DEFCAM
062500030822     C*
062600010330     C                   ENDSR
062700010330     C*----------------------------------------------------*
062800030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
062900010330     C*----------------------------------------------------*
063000010330     C     DEFCAM        BEGSR
063100030822     C*
063200030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
063300030715     C* e delimitatore testo.
063400030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
063500030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
063600030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
063700040723     C*
063800080707     C* Imposto i valori d default d alcuni campi
063900080708     C*
064000080708     C                   CLEAR                   DORM01
064100120822     C                   EVAL      VAOFLO = DORM01
064200080707     C*
064300121207     C                   EVAL      VAOPOE = 045
064400121207     C                   EVAL      VAOTOR = 'S'
064500121207     C                   EVAL      VAOCOR = 0450652000
064600140401     C***                EVAL      VAOCRC = 0450652000
064700140401     C***                EVAL      VAOKSC = 0450652
064800121207     C                   EVAL      VAONAM = 'RESI'
064900121207     C                   EVAL      VAONCL = 1
065000121207     C                   EVAL      VAOPKG = 1
065100121207     C                   EVAL      VAOPAG = 'O'
065200121207     C                   EVAL      VAOCTR = '000'
065300120905     C* se il 1° char dei parametri è T=Test
065400120905     C                   SETOFF                                       55
065500120905     C                   IF        %subst(vlrppt:1:1) = 'T'
065600120905     C* inibisco la scrittura dei file TIORI e TIVGD
065700120905     C                   SETON                                        55
065800120905     C* imposto il PO di emissione come filiale fittizia
065900120905     C                   EVAL      VAOPOE = 999
066000120905     C                   ENDIF
066100091127     C*
066200091204     C* IMPOSTO I CAMPI PERSONALIZZATI
066300100416     C
066400030822     C*
066500030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
066600030822     C                   EVAL      posDaDft = 1
066700030822     C                   EVAL      posADft  = 0
066800030822     C                   EVAL      wGiroDft = 0
066900030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
067000030822     C                             posDaDft > 0
067100030822     C*
067200030822     C* Gestisco il 1° giro
067300030822     C                   IF        wGiroDft = 0
067400030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
067500030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
067600030822     C* Incremento il contatore dei "giri"
067700030822     C                   EVAL      wGiroDft = 1
067800030822     C                   ELSE
067900030822     C                   EVAL      posDaDft = posADft
068000030822     C                   ENDIF
068100030822     C* Eseguo lo scan x trovare la fine del campo corrente
068200030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
068300030822     C*
068400030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
068500030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
068600030822     C                   IF        posDaDft > 0 AND
068700030822     C                             posADft  > 0
068800040708     C* POE
068900040708     C                   IF        %subst(
069000040708     C                             %subst(vlrppt:posDaDft+1:
069100040708     C                             posADft-posDaDft-1):1:3)
069200040708     C                             = 'POE'
069300040708     C                   EVAL      PiStr=%trim(%subst(
069400040708     C                             %subst(vlrppt:posDaDft+1:
069500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069600040708     C                   EXSR      CHKNUM
069700040708     C                   IF        PiInt=*on
069800040708     C                   Z-ADD     PiVal         VAOPOE
069900040708     C                   ENDIF
070000040708     C                   ENDIF
070100040708     C* TOR
070200040708     C                   IF        %subst(
070300040708     C                             %subst(vlrppt:posDaDft+1:
070400040708     C                             posADft-posDaDft-1):1:3)
070500040708     C                             = 'TOR'
070600040708     C                   EVAL      VAOTOR=%trim(%subst(
070700040708     C                             %subst(vlrppt:posDaDft+1:
070800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070900040708     C                   ENDIF
071000040708     C* TCO
071100040708     C                   IF        %subst(
071200040708     C                             %subst(vlrppt:posDaDft+1:
071300040708     C                             posADft-posDaDft-1):1:3)
071400040708     C                             = 'TCO'
071500040708     C                   EVAL      VAOTCO=%trim(%subst(
071600040708     C                             %subst(vlrppt:posDaDft+1:
071700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071800040708     C                   ENDIF
071900040707     C* COR
072000030822     C                   IF        %subst(
072100030822     C                             %subst(vlrppt:posDaDft+1:
072200030822     C                             posADft-posDaDft-1):1:3)
072300040707     C                             = 'COR'
072400030822     C                   EVAL      PiStr=%trim(%subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072700030822     C                   EXSR      CHKNUM
072800030822     C                   IF        PiInt=*on
072900040708     C                   Z-ADD     PiVal         VAOCOR
073000030822     C                   ENDIF
073100030822     C                   ENDIF
073200040708     C* CRC
073300040708     C                   IF        %subst(
073400040708     C                             %subst(vlrppt:posDaDft+1:
073500040708     C                             posADft-posDaDft-1):1:3)
073600040708     C                             = 'CRC'
073700040708     C                   EVAL      PiStr=%trim(%subst(
073800040708     C                             %subst(vlrppt:posDaDft+1:
073900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074000040708     C                   EXSR      CHKNUM
074100040708     C                   IF        PiInt=*on
074200040708     C                   Z-ADD     PiVal         VAOCRC
074300040708     C                   ENDIF
074400040708     C                   ENDIF
074500040708     C* FFD
074600040708     C                   IF        %subst(
074700040708     C                             %subst(vlrppt:posDaDft+1:
074800040708     C                             posADft-posDaDft-1):1:3)
074900040708     C                             = 'FFD'
075000040708     C                   EVAL      VAOFFD=%trim(%subst(
075100040708     C                             %subst(vlrppt:posDaDft+1:
075200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075300040708     C                   ENDIF
075400040708     C* ORR
075500040708     C                   IF        %subst(
075600040708     C                             %subst(vlrppt:posDaDft+1:
075700040708     C                             posADft-posDaDft-1):1:3)
075800040708     C                             = 'ORR'
075900040708     C                   EVAL      PiStr=%trim(%subst(
076000040708     C                             %subst(vlrppt:posDaDft+1:
076100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076200040708     C                   EXSR      CHKNUM
076300040708     C                   IF        PiInt=*on
076400040708     C                   Z-ADD     PiVal         VAOORR
076500040708     C                   ENDIF
076600040708     C                   ENDIF
076700040712     C* NCL
076800040712     C                   IF        %subst(
076900040712     C                             %subst(vlrppt:posDaDft+1:
077000040712     C                             posADft-posDaDft-1):1:3)
077100040712     C                             = 'NCL'
077200040712     C                   EVAL      PiStr=%trim(%subst(
077300040712     C                             %subst(vlrppt:posDaDft+1:
077400040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077500040712     C                   EXSR      CHKNUM
077600040712     C                   IF        PiInt=*on
077700040712     C                   Z-ADD     PiVal         VAONCL
077800040712     C                   ENDIF
077900040712     C                   ENDIF
078000040712     C* PKG
078100040712     C                   IF        %subst(
078200040712     C                             %subst(vlrppt:posDaDft+1:
078300040712     C                             posADft-posDaDft-1):1:3)
078400040712     C                             = 'PKG'
078500040712     C                   EVAL      PiStr=%trim(%subst(
078600040712     C                             %subst(vlrppt:posDaDft+1:
078700040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078800040712     C                   EXSR      CHKNUM
078900040712     C                   IF        PiNum=*on
079000040712     C                   Z-ADD     PiVal         VAOPKG
079100040712     C                   ENDIF
079200040712     C                   ENDIF
079300040712     C* VLM
079400040712     C                   IF        %subst(
079500040712     C                             %subst(vlrppt:posDaDft+1:
079600040712     C                             posADft-posDaDft-1):1:3)
079700040712     C                             = 'VLM'
079800040712     C                   EVAL      PiStr=%trim(%subst(
079900040712     C                             %subst(vlrppt:posDaDft+1:
080000040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080100040712     C                   EXSR      CHKNUM
080200040712     C                   IF        PiNum=*on
080300040712     C                   Z-ADD     PiVal         VAOVLM
080400040712     C                   ENDIF
080500040712     C                   ENDIF
080600040708     C* NAM
080700040708     C                   IF        %subst(
080800040708     C                             %subst(vlrppt:posDaDft+1:
080900040708     C                             posADft-posDaDft-1):1:3)
081000040708     C                             = 'NAM'
081100040708     C                   EVAL      VAONAM=%trim(%subst(
081200040708     C                             %subst(vlrppt:posDaDft+1:
081300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081400040708     C                   ENDIF
081500040708     C* PAG
081600030822     C                   IF        %subst(
081700030822     C                             %subst(vlrppt:posDaDft+1:
081800030822     C                             posADft-posDaDft-1):1:3)
081900040708     C                             = 'PAG'
082000040708     C                   EVAL      VAOPAG=%trim(%subst(
082100030822     C                             %subst(vlrppt:posDaDft+1:
082200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082300030822     C                   ENDIF
082400040708     C* KSC
082500040708     C                   IF        %subst(
082600040708     C                             %subst(vlrppt:posDaDft+1:
082700040708     C                             posADft-posDaDft-1):1:3)
082800040708     C                             = 'KSC'
082900040708     C                   EVAL      PiStr=%trim(%subst(
083000040708     C                             %subst(vlrppt:posDaDft+1:
083100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083200040708     C                   EXSR      CHKNUM
083300040708     C                   IF        PiInt=*on
083400040708     C                   Z-ADD     PiVal         VAOKSC
083500040708     C                   ENDIF
083600040708     C                   ENDIF
083700040708     C* CTR
083800040708     C                   IF        %subst(
083900040708     C                             %subst(vlrppt:posDaDft+1:
084000040708     C                             posADft-posDaDft-1):1:3)
084100040708     C                             = 'CTR'
084200040708     C                   EVAL      VAOCTR=%trim(%subst(
084300040708     C                             %subst(vlrppt:posDaDft+1:
084400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084500040708     C                   ENDIF
084600040708     C* POC
084700040708     C                   IF        %subst(
084800040708     C                             %subst(vlrppt:posDaDft+1:
084900040708     C                             posADft-posDaDft-1):1:3)
085000040708     C                             = 'POC'
085100040708     C                   EVAL      PiStr=%trim(%subst(
085200040708     C                             %subst(vlrppt:posDaDft+1:
085300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085400040708     C                   EXSR      CHKNUM
085500040708     C                   IF        PiInt=*on
085600040708     C                   Z-ADD     PiVal         VAOPOC
085700040708     C                   ENDIF
085800040708     C                   ENDIF
085900040708     C* NO1
086000040708     C                   IF        %subst(
086100040708     C                             %subst(vlrppt:posDaDft+1:
086200121129     C                             posADft-posDaDft-1):1:3)
086300040708     C                             = 'NO1'
086400040708     C                   EVAL      VAONO1=%trim(%subst(
086500040708     C                             %subst(vlrppt:posDaDft+1:
086600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086700040708     C                   ENDIF
086800040708     C* DDT
086900030822     C                   IF        %subst(
087000030822     C                             %subst(vlrppt:posDaDft+1:
087100030822     C                             posADft-posDaDft-1):1:3)
087200040708     C                             = 'DDT'
087300040708     C                   EVAL      VAODDT=%trim(%subst(
087400030822     C                             %subst(vlrppt:posDaDft+1:
087500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
087600030822     C                   ENDIF
087700040708     C* RFA
087800040708     C                   IF        %subst(
087900040708     C                             %subst(vlrppt:posDaDft+1:
088000040708     C                             posADft-posDaDft-1):1:3)
088100040708     C                             = 'RFA'
088200040708     C                   EVAL      VAORFA=%trim(%subst(
088300040708     C                             %subst(vlrppt:posDaDft+1:
088400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
088500040708     C                   ENDIF
088600040708     C* FLO
088700040708     C                   IF        %subst(
088800040708     C                             %subst(vlrppt:posDaDft+1:
088900040708     C                             posADft-posDaDft-1):1:3)
089000040708     C                             = 'FLO'
089100040708     C                   EVAL      VAOFLO=%trim(%subst(
089200040708     C                             %subst(vlrppt:posDaDft+1:
089300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
089400091103     C*
089500040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
089600080702     C* e posizione 7 del VAOFLO
089700040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
089800040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
089900050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
090000040708     C                   ENDIF
090100030822     C* ...
090200030822     C                   ENDIF
090300030822     C                   ENDDO
090400020204     C*
090500010330     C                   ENDSR
090600010607     C*----------------------------------------------------*
090700040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
090800010607     C*----------------------------------------------------*
090900040708     C     IMPVAO        BEGSR
091000010607     C*
091100010607     C                   SETOFF                                       3132
091200120124
091300030515     C*
091400040708     C                   MOVEL     datcor        VAODAO
091500040708     C                   MOVEL     wn14          VAOOAO
091600050314     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
091700050314     C     minu:maiu     XLATE     vindta        vindta
091800010607     C*
091900030715     C**********
092000030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
092100030715     C**********
092200030715     C                   IF        wGiro = *zeros
092300040709     C*
092400040709     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
092500040709     C                   EVAL      depvinDTA = vinDTA
092600040709     C*
092700040709     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
092800040709     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
092900040709     C                                      %trim(intcol(3))
093000031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
093100050314     C**!!!minu:maiu     XLATE     vindta        vindta
093200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
093300040324     C                   DOW       posDa <= %len(%trimr(vindta))
093400030715     C*
093500030715     C* Gestisco il 1° campo
093600030715     C                   IF        i = 1
093700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
093800030715     C                   EVAL      posDa = 1
093900030715     C* Eseguo lo scan x trovare la fine del primo campo
094000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094100030715     C* A questo punto "estraggo" il campo corrente
094200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
094300030715     C                                                    (posA-posDa))
094400030715     C* X i campi successivi al 1°
094500030715     C                   ELSE
094600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
094700030715     C                   EVAL      posDa = posA + 1
094800030715     C* Eseguo lo scan x trovare la fine del campo corrente
094900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
095000030715     C* Gestisco l'ultimo campo
095100030715     C                   IF        posA = *zeros
095200040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
095300030715     C* A questo punto "estraggo" il campo corrente
095400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
095500030715     C                                                    (posA-posDa))
095600030715     C                   LEAVE
095700030715     C                   ELSE
095800030715     C* A questo punto "estraggo" il campo corrente
095900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
096000030715     C                                                    (posA-posDa))
096100030715     C                   ENDIF
096200030715     C                   ENDIF
096300030715     C* Incremento il contatore d campo
096400030715     C                   EVAL      i = i +1
096500030715     C                   ENDDO
096600030715     C*
096700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
096800030715     C* delimitazione testo
096900030715     C                   EVAL      i = 1
097000030715     C                   DOW       i < %elem(SkSplitFLD)
097100030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
097200030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
097300030715     C                   EVAL      i = i + 1
097400030715     C                   ENDDO
097500030715     C                   EVAL      wGiro = 1
097600040709     C* Ripristino la 1° riga originale
097700040709     C                   EVAL      vinDTA = depvinDTA
097800030715     C                   ELSE
097900030715     C                   EVAL      wGiro = 2
098000031204     C**********
098100031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
098200031204     C* d separatore campo
098300031204     C**********
098400040709     C                   EVAL      vindta = %trim(vindta)
098500040709     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
098600040709     C                   EVAL      vindta = %subst(vindta:2)
098700040709     C                   ENDDO
098800040709     C*
098900040709     C                   Z-ADD     *zeros        lunghInput        4 0
099000040709     C                   EVAL      lunghInput = %len(%trim(vindta))
099100040709     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
099200040709     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
099300040709     C                   EVAL      lunghInput = %len(%trim(vindta))
099400040709     C                   ENDDO
099500030715     C**********
099600030715     C* Effettuo lo split del campo dati d input
099700030715     C**********
099800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
099900040324     C                   DOW       posDa <= %len(%trimr(vindta))
100000030715     C*
100100030715     C* Gestisco il 1° campo
100200030715     C                   IF        i = 1
100300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
100400030715     C                   EVAL      posDa = 1
100500030715     C* Eseguo lo scan x trovare la fine del primo campo
100600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
100700030715     C* A questo punto "estraggo" il campo corrente
100800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
100900030715     C                                                    (posA-posDa))
101000030715     C* X i campi successivi al 1°
101100030715     C                   ELSE
101200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
101300030715     C                   EVAL      posDa = posA + 1
101400031204     C* Verifico che nn vi sia il campo nn valorizzato
101500031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
101600031204     C* Se campo nn valorizzato skippo al prossimo
101700031204     C                   EVAL      SkSplitCSV(i) = *blanks
101800031204     C                   EVAL      posA  = posA + 1
101900031204     C                   ELSE
102000030715     C* Eseguo lo scan x trovare la fine del campo corrente
102100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
102200030715     C* Gestisco l'ultimo campo
102300030715     C                   IF        posA = *zeros
102400040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
102500030715     C* A questo punto "estraggo" il campo corrente
102600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
102700030715     C                                                    (posA-posDa))
102800030715     C                   LEAVE
102900030715     C                   ELSE
103000030715     C* A questo punto "estraggo" il campo corrente
103100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
103200030715     C                                                    (posA-posDa))
103300030715     C                   ENDIF
103400031204     C                   ENDIF
103500030715     C                   ENDIF
103600030715     C* Incremento il contatore d campo
103700030715     C                   EVAL      i = i +1
103800030715     C                   ENDDO
103900030715     C*
104000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
104100030715     C* delimitazione testo
104200030715     C                   EVAL      i = 1
104300030715     C                   DOW       i < %elem(SkSplitCSV)
104400030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
104500030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
104600030715     C                   EVAL      i = i + 1
104700030715     C                   ENDDO
104800030715     C*
104900030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
105000030715     C                   EVAL      i = 1
105100030715     C                   DOW       i < %elem(SkSplitFLD)
105200030715     C*
105300030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
105400030715     C                   IF        SkSplitFLD(i) <> *blanks AND
105500030715     C                             SkSplitCSV(i) <> *blanks
105600030721     C*** ATB
105700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATB'
105800040708     C                   EVAL      VAOATB = SkSplitCSV(i)
105900030721     C                   ENDIF
106000040708     C*** TOR
106100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTOR'
106200040708     C                   EVAL      VAOTOR = SkSplitCSV(i)
106300040708     C                   ENDIF
106400040708     C*** TCO
106500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTCO'
106600040708     C                   EVAL      VAOTCO = SkSplitCSV(i)
106700040708     C                   ENDIF
106800040708     C*** RSO
106900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSO'
107000040708     C                   EVAL      VAORSO = SkSplitCSV(i)
107100030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
107200040708     C     '@':'A'       XLATE     VAORSO        VAORSO
107300030715     C* ==
107400030715     C                   ENDIF
107500040708     C*** INO
107600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINO'
107700040708     C                   EVAL      VAOINO = SkSplitCSV(i)
107800030715     C                   ENDIF
107900040708     C*** LOO
108000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOO'
108100040708     C                   EVAL      VAOLOO = SkSplitCSV(i)
108200030715     C                   ENDIF
108300040708     C*** PRO
108400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRO'
108500040708     C                   EVAL      VAOPRO = SkSplitCSV(i)
108600030715     C                   ENDIF
108700040708     C*** NAO
108800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAO'
108900040708     C                   EVAL      VAONAO = SkSplitCSV(i)
109000030721     C                   ENDIF
109100040708     C*** RSR
109200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSR'
109300040708     C                   EVAL      VAORSR = SkSplitCSV(i)
109400040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
109500040708     C     '@':'A'       XLATE     VAORSR        VAORSR
109600040708     C* ==
109700040708     C                   IF        VAORSR = *blanks
109800040708     C                   SETON                                        31
109900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
110000040708     C                             + ' ' + 'VAORSR'
110100040708     C                   ENDIF
110200040708     C                   ENDIF
110300040708     C*** INR
110400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINR'
110500040708     C                   EVAL      VAOINR = SkSplitCSV(i)
110600040708     C                   IF        VAOINR = *blanks
110700040708     C                   SETON                                        31
110800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
110900040708     C                             + ' ' + 'VAOINR'
111000040708     C                   ENDIF
111100040708     C                   ENDIF
111200040708     C*** LOR
111300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOR'
111400040708     C                   EVAL      VAOLOR = SkSplitCSV(i)
111500040708     C                   IF        VAOLOR = *blanks
111600040708     C                   SETON                                        31
111700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
111800040708     C                             + ' ' + 'VAOLOR'
111900040708     C                   ENDIF
112000040708     C                   ENDIF
112100040708     C*** PRR
112200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRR'
112300040708     C                   EVAL      VAOPRR = SkSplitCSV(i)
112400040708     C                   IF        VAOPRR = *blanks
112500040708     C                   SETON                                        31
112600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
112700040708     C                             + ' ' + 'VAOPRR'
112800040708     C                   ENDIF
112900040708     C                   ENDIF
113000040708     C*** NAR
113100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAR'
113200040708     C                   EVAL      VAONAR = SkSplitCSV(i)
113300040708     C                   ENDIF
113400040708     C*** RER
113500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORER'
113600040708     C                   EVAL      VAORER = SkSplitCSV(i)
113700040708     C                   ENDIF
113800040708     C*** TER
113900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTER'
114000040708     C                   EVAL      VAOTER = SkSplitCSV(i)
114100040708     C                   ENDIF
114200140728     C*** SMSR
114300140728     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSMSR'
114400140728     C                   EVAL      w_VAOSMSR = SkSplitCSV(i)
114500140728     C                   ENDIF
114600140728     C*** EMLR
114700140728     C                   IF        %trim(SkSplitFLD(i)) = 'VAOEMLR'
114800140728     C                   EVAL      w_VAOEMLR = SkSplitCSV(i)
114900140728     C                   ENDIF
115000040708     C*** RSC
115100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSC'
115200040708     C                   EVAL      VAORSC = SkSplitCSV(i)
115300040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
115400040708     C     '@':'A'       XLATE     VAORSC        VAORSC
115500040708     C* ==
115600040708     C                   ENDIF
115700040708     C*** INC
115800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINC'
115900040708     C                   EVAL      VAOINC = SkSplitCSV(i)
116000040708     C                   ENDIF
116100040708     C*** LOC
116200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOC'
116300040708     C                   EVAL      VAOLOC = SkSplitCSV(i)
116400040708     C                   ENDIF
116500040708     C*** PRC
116600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRC'
116700040708     C                   EVAL      VAOPRC = SkSplitCSV(i)
116800040708     C                   ENDIF
116900040708     C*** NAC
117000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAC'
117100040708     C                   EVAL      VAONAC = SkSplitCSV(i)
117200040708     C                   ENDIF
117300040708     C*** FFD
117400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFFD'
117500040708     C                   EVAL      VAOFFD = SkSplitCSV(i)
117600040708     C                   ENDIF
117700040708     C*** RMP
117800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORMP'
117900040708     C                   EVAL      VAORMP = SkSplitCSV(i)
118000040708     C                   ENDIF
118100040708     C*** NAM
118200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAM'
118300040708     C                   EVAL      VAONAM = SkSplitCSV(i)
118400040708     C                   ENDIF
118500040708     C*** PAG
118600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPAG'
118700040708     C                   EVAL      VAOPAG = SkSplitCSV(i)
118800040708     C                   ENDIF
118900040708     C*** CTR
119000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCTR'
119100040708     C                   EVAL      VAOCTR = SkSplitCSV(i)
119200040708     C                   ENDIF
119300040708     C*** ZOR
119400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOZOR'
119500040708     C                   EVAL      VAOZOR = SkSplitCSV(i)
119600040708     C                   ENDIF
119700040708     C*** NO1
119800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO1'
119900040708     C                   EVAL      VAONO1 = SkSplitCSV(i)
120000030715     C                   ENDIF
120100040708     C*** NO2
120200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO2'
120300040708     C                   EVAL      VAONO2 = SkSplitCSV(i)
120400030715     C                   ENDIF
120500040708     C*** DDT
120600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODDT'
120700040708     C                   EVAL      VAODDT = SkSplitCSV(i)
120800030715     C                   ENDIF
120900040708     C*** RFA
121000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORFA'
121100040708     C                   EVAL      VAORFA = SkSplitCSV(i)
121200030715     C                   ENDIF
121300040708     C*** SPI
121400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSPI'
121500040708     C                   EVAL      VAOSPI = SkSplitCSV(i)
121600030715     C                   ENDIF
121700040708     C*** FLO
121800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
121900040708     C                   EVAL      VAOFLO = SkSplitCSV(i)
122000040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
122100080708     C* e la posizione 7 del VAOFLO
122200040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
122300040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
122400050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
122500030715     C                   ENDIF
122600091103     C
122700091103     C*** DAR
122800050314     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
122900050412     C                   EVAL      PiStr=SkSplitCSV(i)
123000050412     C                   EXSR      CHKNUM
123100050412     C                   IF        PiInt=*on
123200050412     C                   Z-ADD     PiVal         VAODAR
123300050412     C                   ELSE
123400050412     C                   SETON                                        32
123500050412     C                   EVAL      VAODAR = *zeros
123600050412     C                   EVAL      vinmsg = %trimr(vinmsg)
123700050412     C                             + ' ' + 'VAODAR'
123800050412     C                   ENDIF
123900050314     C                   ENDIF
124000030715     C
124100030721     C*
124200030721     C* Reperisco quindi i campi numerici...
124300040708     C*** POE
124400040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
124500040723     C                   EVAL      PiStr=SkSplitCSV(i)
124600040723     C                   EXSR      CHKNUM
124700040723     C                   IF        PiInt=*on    AND
124800040723     C                             PiVal<=999   AND
124900040723     C                             PiVal>*zeros
125000040723     C                   Z-ADD     PiVal         VAOPOE
125100040723     C                   ELSE
125200040723     C                   SETON                                        32
125300040723     C                   EVAL      VAOPOE = *zeros
125400040723     C                   EVAL      vinmsg = %trimr(vinmsg)
125500040723     C                             + ' ' + 'VAOPOE'
125600040723     C                   ENDIF
125700040723     C                   ENDIF
125800040708     C*** COR
125900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
126000040708     C                   EVAL      PiStr=SkSplitCSV(i)
126100040708     C                   EXSR      CHKNUM
126200040708     C                   IF        PiInt=*on
126300040708     C                   Z-ADD     PiVal         VAOCOR
126400040708     C                   ELSE
126500040708     C                   SETON                                        31
126600040708     C                   EVAL      VAOCOR = *zeros
126700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
126800040708     C                             + ' ' + 'VAOCOR'
126900040708     C                   ENDIF
127000040708     C                   ENDIF
127100040708     C*** CAO
127200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
127300030721     C                   EVAL      PiStr=SkSplitCSV(i)
127400030721     C                   EXSR      CHKNUM
127500030721     C                   IF        PiInt=*on
127600030721     C                   Z-ADD     PiVal         Num5_0
127700040708     C                   MOVEL(p)  Num5_0        VAOCAO
127800030721     C                   ELSE
127900030721     C                   SETON                                        32
128000040708     C                   EVAL      VAOCAO = *zeros
128100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128200040708     C                             + ' ' + 'VAOCAO'
128300030721     C                   ENDIF
128400030721     C                   ENDIF
128500040708     C*** CRA
128600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
128700030721     C                   EVAL      PiStr=SkSplitCSV(i)
128800030721     C                   EXSR      CHKNUM
128900030721     C                   IF        PiInt=*on
129000040708     C                   Z-ADD     PiVal         VAOCRA
129100030721     C                   ELSE
129200030721     C                   SETON                                        32
129300040708     C                   EVAL      VAOCRA = *zeros
129400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129500040708     C                             + ' ' + 'VAOCRA'
129600030721     C                   ENDIF
129700030721     C                   ENDIF
129800040708     C*** CAR
129900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
130000040708     C                   EVAL      PiStr=SkSplitCSV(i)
130100040708     C                   EXSR      CHKNUM
130200040708     C                   IF        PiInt=*on
130300040708     C                   Z-ADD     PiVal         Num5_0
130400040708     C                   MOVEL(p)  Num5_0        VAOCAR
130500040708     C                   ELSE
130600040708     C                   SETON                                        31
130700040708     C                   EVAL      VAOCAR = *zeros
130800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
130900040708     C                             + ' ' + 'VAOCAR'
131000040708     C                   ENDIF
131100040708     C                   ENDIF
131200040708     C*** CRC
131300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
131400040708     C                   EVAL      PiStr=SkSplitCSV(i)
131500040708     C                   EXSR      CHKNUM
131600040708     C                   IF        PiInt=*on
131700040708     C                   Z-ADD     PiVal         VAOCRC
131800040708     C                   ELSE
131900040708     C                   SETON                                        32
132000040708     C                   EVAL      VAOCRC = *zeros
132100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
132200040708     C                             + ' ' + 'VAOCRC'
132300040708     C                   ENDIF
132400040708     C                   ENDIF
132500040708     C*** CAC
132600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
132700040708     C                   EVAL      PiStr=SkSplitCSV(i)
132800040708     C                   EXSR      CHKNUM
132900040708     C                   IF        PiInt=*on
133000040708     C                   Z-ADD     PiVal         Num5_0
133100040708     C                   MOVEL(p)  Num5_0        VAOCAC
133200040708     C                   ELSE
133300040708     C                   SETON                                        32
133400040708     C                   EVAL      VAOCAC = *zeros
133500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
133600040708     C                             + ' ' + 'VAOCAC'
133700040708     C                   ENDIF
133800040708     C                   ENDIF
133900040708     C*** DAR
134000090428     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
134100090428     C                   EVAL      PiStr=SkSplitCSV(i)
134200090428     C                   EXSR      CHKNUM
134300090428     C                   IF        PiInt=*on
134400090428     C                   Z-ADD     PiVal         VAODAR
134500090428     C                   ELSE
134600090428     C                   SETON                                        32
134700090428     C                   EVAL      VAODAR = *zeros
134800090428     C                   EVAL      vinmsg = %trimr(vinmsg)
134900090428     C                             + ' ' + 'VAODAR'
135000090428     C                   ENDIF
135100090428     C                   ENDIF
135200040708     C*** ORR
135300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
135400040708     C                   EVAL      PiStr=SkSplitCSV(i)
135500040708     C                   EXSR      CHKNUM
135600040708     C                   IF        PiInt=*on
135700040708     C                   Z-ADD     PiVal         VAOORR
135800040708     C                   ELSE
135900040708     C                   SETON                                        32
136000040708     C                   EVAL      VAOORR = *zeros
136100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
136200040708     C                             + ' ' + 'VAOORR'
136300040708     C                   ENDIF
136400040708     C                   ENDIF
136500030721     C*** NCL
136600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
136700030721     C                   EVAL      PiStr=SkSplitCSV(i)
136800030721     C                   EXSR      CHKNUM
136900030721     C                   IF        PiInt=*on
137000040723     C                   IF        PiVal > *zeros
137100040708     C                   Z-ADD     PiVal         VAONCL
137200040723     C                   ENDIF
137300030721     C                   ELSE
137400030721     C                   SETON                                        32
137500040708     C                   EVAL      VAONCL = *zeros
137600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137700040708     C                             + ' ' + 'VAONCL'
137800030721     C                   ENDIF
137900030721     C                   ENDIF
138000040708     C*** PKG
138100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
138200030721     C                   EVAL      PiStr=SkSplitCSV(i)
138300030721     C                   EXSR      CHKNUM
138400030721     C                   IF        PiNum=*on
138500040723     C                   IF        PiVal > *zeros
138600040708     C                   Z-ADD     PiVal         VAOPKG
138700040723     C                   ENDIF
138800030721     C                   ELSE
138900030721     C                   SETON                                        32
139000040708     C                   EVAL      VAOPKG = *zeros
139100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139200040708     C                             + ' ' + 'VAOPKG'
139300030721     C                   ENDIF
139400030721     C                   ENDIF
139500040708     C*** VLM
139600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
139700040708     C                   EVAL      PiStr=SkSplitCSV(i)
139800040708     C                   EXSR      CHKNUM
139900040708     C                   IF        PiNum=*on
140000040708     C                   Z-ADD     PiVal         VAOVLM
140100040708     C                   ELSE
140200040708     C                   SETON                                        32
140300040708     C                   EVAL      VAOVLM = *zeros
140400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
140500040708     C                             + ' ' + 'VAOVLM'
140600040708     C                   ENDIF
140700040708     C                   ENDIF
140800040708     C*** BNC
140900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
141000040708     C                   EVAL      PiStr=SkSplitCSV(i)
141100040708     C                   EXSR      CHKNUM
141200040708     C                   IF        PiInt=*on
141300040708     C                   Z-ADD     PiVal         VAOBNC
141400040708     C                   ELSE
141500040708     C                   SETON                                        32
141600040708     C                   EVAL      VAOBNC = *zeros
141700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
141800040708     C                             + ' ' + 'VAOBNC'
141900040708     C                   ENDIF
142000040708     C                   ENDIF
142100040708     C*** BLC
142200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
142300040708     C                   EVAL      PiStr=SkSplitCSV(i)
142400040708     C                   EXSR      CHKNUM
142500040708     C                   IF        PiInt=*on
142600040708     C                   Z-ADD     PiVal         VAOBLC
142700040708     C                   ELSE
142800040708     C                   SETON                                        32
142900040708     C                   EVAL      VAOBLC = *zeros
143000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
143100040708     C                             + ' ' + 'VAOBLC'
143200040708     C                   ENDIF
143300040708     C                   ENDIF
143400040708     C*** ATT
143500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
143600040708     C                   EVAL      PiStr=SkSplitCSV(i)
143700040708     C                   EXSR      CHKNUM
143800040708     C                   IF        PiInt=*on
143900040708     C                   Z-ADD     PiVal         VAOATT
144000040708     C                   ELSE
144100040708     C                   SETON                                        32
144200040708     C                   EVAL      VAOATT = *zeros
144300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
144400040708     C                             + ' ' + 'VAOATT'
144500040708     C                   ENDIF
144600040708     C                   ENDIF
144700040708     C*** MTC
144800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
144900040708     C                   EVAL      PiStr=SkSplitCSV(i)
145000040708     C                   EXSR      CHKNUM
145100040708     C                   IF        PiInt=*on
145200040708     C                   Z-ADD     PiVal         VAOMTC
145300040708     C                   ELSE
145400040708     C                   SETON                                        32
145500040708     C                   EVAL      VAOMTC = *zeros
145600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
145700040708     C                             + ' ' + 'VAOMTC'
145800040708     C                   ENDIF
145900040708     C                   ENDIF
146000040708     C*** KSC
146100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
146200030721     C                   EVAL      PiStr=SkSplitCSV(i)
146300030721     C                   EXSR      CHKNUM
146400030721     C                   IF        PiInt=*on      AND
146500030721     C                             PiVal<=9999999 AND
146600030721     C                             PiVal>*zeros
146700040708     C                   Z-ADD     PiVal         VAOKSC
146800030721     C                   ELSE
146900030721     C                   SETON                                        32
147000040708     C                   EVAL      VAOKSC = *zeros
147100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
147200040708     C                             + ' ' + 'VAOKSC'
147300030721     C                   ENDIF
147400030721     C                   ENDIF
147500040708     C*** POR
147600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
147700030721     C                   EVAL      PiStr=SkSplitCSV(i)
147800030721     C                   EXSR      CHKNUM
147900030721     C                   IF        PiInt=*on    AND
148000030721     C                             PiVal<=999   AND
148100030721     C                             PiVal>*zeros
148200040708     C                   Z-ADD     PiVal         VAOPOR
148300030721     C                   ELSE
148400030721     C                   SETON                                        32
148500040708     C                   EVAL      VAOPOR = *zeros
148600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
148700040708     C                             + ' ' + 'VAOPOR'
148800030721     C                   ENDIF
148900030721     C                   ENDIF
149000040708     C*** POC
149100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
149200040708     C                   EVAL      PiStr=SkSplitCSV(i)
149300040708     C                   EXSR      CHKNUM
149400040708     C                   IF        PiInt=*on    AND
149500040708     C                             PiVal<=999   AND
149600040708     C                             PiVal>*zeros
149700040708     C                   Z-ADD     PiVal         VAOPOC
149800040708     C                   ELSE
149900040708     C                   SETON                                        32
150000040708     C                   EVAL      VAOPOC = *zeros
150100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
150200040708     C                             + ' ' + 'VAOPOC'
150300040708     C                   ENDIF
150400040708     C                   ENDIF
150500040708     C*** STO
150600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
150700040708     C                   EVAL      PiStr=SkSplitCSV(i)
150800040708     C                   EXSR      CHKNUM
150900040708     C                   IF        PiInt=*on
151000040708     C                   Z-ADD     PiVal         VAOSTO
151100040708     C                   ELSE
151200040708     C                   SETON                                        32
151300040708     C                   EVAL      VAOSTO = *zeros
151400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
151500040708     C                             + ' ' + 'VAOSTO'
151600040708     C                   ENDIF
151700040708     C                   ENDIF
151800090428     C*
151900091103     C********** GESTIONE CAMPI PERSONALIZZATI
152000120120
152100121207     C*** RER
152200121207     C                   EVAL      VAORER = VAORSR
152300121207
152400121207     C*** RFA
152500121207     C* 1 char
152600121207     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAORFA_1'
152700121207     C                   EVAL      wRFA1=%char(%dec(%trim(SkSplitCSV(i)):1:0))
152800121207     C                   ENDIF
152900121207     C* 7 char
153000121207     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAORFA_2'
153100121207     C                   EVAL      wRFA2=%trim(SkSplitCSV(i))
153200121207     C                   ENDIF
153300121207     C* 3 char
153400121207     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAORFA_3'
153500121207     C                   EVAL      wRFA3=%editc(%dec(%trim(SkSplitCSV(i)):3:0)
153600121207     C                                   :'X')
153700121207     C                   ENDIF
153800121207     C* 2 char
153900121207     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAORFA_4'
154000121207     C                   EVAL      wRFA4=%editc(%dec(%trim(SkSplitCSV(i)):2:0)
154100121207     C                                   :'X')
154200121207     C                   ENDIF
154300141006     C* Note 1 e 2
154400141006     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAONO1'
154500141006     C                   EVAL      w70 = %trim(SkSplitCSV(i))
154600141006     C                   EVAL      VAONO1=%subst(w70 : 1 :35)
154700141006     C                   EVAL      VAONO2=%subst(w70 : 36 :35)
154800141006     C                   ENDIF
154900090428     C*
155000090428     C****************************************
155100090428     C*
155200030715     C*
155300030715     C                   ENDIF
155400030715     C                   EVAL      i = i + 1
155500030715     C                   ENDDO
155600040723     C*
155700121207     C***  ===>  Gestione campi "particolari" fuori ciclo
155800121207     C*
155900121207     C* VAORFA va valorizzato concatenando più colonne
156000121207     C                   EVAL      VAORFA='RT' + wRFA1 + wRFA2 + wRFA3 + wRFA4
156100121207     C*
156200140401     C* se non passati nei parametri, CRC e KSC vengono presi da COR
156300140401     C                   IF        VAOCRC = *zeros
156400140401     C                   EVAL      VAOCRC = VAOCOR
156500140401     C                   ENDIF
156600140401     C                   IF        VAOKSC = *zeros
156700140401     C                   EVAL      VAOKSC = VAOCOR/1000
156800140401     C                   ENDIF
156900141014     C*
157000141014     C* se VAOTER non è valorizzato, lo valorizzo con VAOSMSR
157100141014     C                   IF        VAOTER = *blank
157200141014     C                   EVAL      VAOTER = w_VAOSMSR
157300141014     C                   ENDIF
157400140401     C*
157500030715     C                   ENDIF
157600020204     C*
157700010607     C                   ENDSR
157800010607     C*----------------------------------------------------*
157900080702
158000080702
158100080702     C*----------------------------------------------------*
158200080702     C*  CONTROLLO NUMERICITA' CAMPI
158300080702     C*----------------------------------------------------*
158400080702     C     CHKNUM        BEGSR
158500080702     C*
158600080702     C                   IF        PiDecChr = *blanks
158700080702     C                   EVAL      PiDecChr = ','
158800080702     C                   ENDIF
158900080702     C*
159000080702     C                   callp     UBISNUM_Check(PiStr
159100080702     C                                          :PiDecChr
159200080702     C                                          :PiVal
159300080702     C                                          :PiNum
159400080702     C                                          :PiInt)
159500080702     C*
159600080702     C                   ENDSR
159700080702     C***
159800080702
159900010601
160000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
160100010601     C     repfil        BEGSR
160200010601     C*
160300010601     C                   if        invfil = *zeros and
160400010601     C                             depfil > *zeros and
160500010629     C                             (vinflg = *blanks or
160600010629     C                              vinflg = *zeros)
160700010601     C
160800010601     C                   eval      invfil = depfil
160900010601     C                   endif
161000010601     C*
161100010601     C                   if        depfil <> invfil and
161200010601     C                             invfil > *zeros
161300010601     C                   eval      flgMulti = '1'
161400010601     C                   if        vinflg = *blanks
161500010601     C                   add       1             cntNonEl
161600010601     C                   endif
161700010601     C                   endif
161800010601     C*
161900010601     C                   if        vinflg = '2'
162000010601     C                   eval      flgStato = '2'
162100010601     C                   endif
162200010601     C*
162300010601     C                   ENDSR
162400010601     C***
162500010330
162600010330
162700990920      /TITLE Invio dei dati al punto operativo.
162800000613     C     invio         BEGSR
162900990920     C*
163000990920     C                   reset                   dscmz
163100010601     C                   move      invfil        cmzdst
163200990920     C                   eval      cmzfld = vlrfou
163300990920     C                   eval      cmzmbd = vlrhdl
163400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
163500000710     C                   if        prmfir = *blanks
163600040708     C                   eval      cmzfla = 'FNVAO00F'
163700040708     C                   eval      cmzmba = 'FNVAO00F'
163800000710     C                   else
163900000710     C                   eval      cmzfla = prmfir
164000000710     C                   eval      cmzmba = prmfir
164100000710     C                   endif
164200990920     C                   eval      cmznrr = *zeros
164300990920     C                   move      §ctrok        cmznrr
164400021018     C                   eval      cmzlba = vlrfl1
164500990920     C                   call(e)   'TIS711C'
164600990920     C                   parm                    dscmz
164700990921     C                   parm      *blanks       esito
164800990920     C                   if        %error
164900990920     C                             or cmzerr = '1'
165000990921     C                             or esito  = '1'
165100000710     C                   eval      wrkesito = '3'
165200990920     C                   endif
165300990920     C*
165400000613     C                   ENDSR
165500010601
165600010601
165700010601      /TITLE Invio dei dati al punto operativo.
165800010601     C     opeini        BEGSR
165900010601     C*
166000010601     C* Inizializzo flag e contatori operativi
166100010601     C                   movel     '0'           flgGiro           1
166200010601     C                   movel     '0'           flgMulti          1
166300010601     C                   movel     '1'           flgStato          1
166400010615     C                   movel     '0'           flgOk             1
166500010601     C                   z-add     *zeros        cntNonEl         10 0
166600010601     C                   z-add     *zeros        depfil            3 0
166700010601     C                   z-add     *zeros        invfil            3 0
166800010601     C*
166900010601     C                   ENDSR
167000010601     C***
167100050421
167200050421
167300091130     C     *pssr         BEGSR
167400050421     C*
167500091130     C                   eval      wrkesito = '2'
167600050421     C*
167700091130     C                   ENDSR     '*CANCL'
167800050421     C***
167900060306
168000060306
168100060306     C*-------------------------------------------------------------------------*
168200060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
168300060306     C*-------------------------------------------------------------------------*
168400060306     C     sr_Control    begsr
168500060306     C*
168600060306     C* inizializzo a ok il flag d controllo abilitazione
168700060306     C                   eval      Cli_Abil = *on
168800060306     C
168900060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
169000060306     C                   clear                   woriIDC
169100060306     C                   eval      kteCOD = 'VAP'
169200060306     C                   eval      kteKE1 = oriIDC
169300060306     C     keytbe        chain     TNTBE01L
169400060306     C                   if        %found(Tntbe01l)
169500060306     C                   eval      woriIDC = TbeKe2
169600060306     C                   else
169700060306     C                   eval      Cli_Abil = *off
169800060306     C                   endif
169900060306     C*
170000060306     C                   endsr
170100010330
170200010330
170300000613     C     *inzsr        BEGSR
170400990910     C*
170500990910     C     *entry        plist
170600990920     C                   parm                    tivlrds
170700990921     C                   parm      wrkesito      esito
170800000724     C                   parm                    prmlit
170900000710     C                   parm                    prmfir
171000010330     C*
171100010330     C* CALCOLA LA DATA CORRENTE
171200091127     C                   z-add     *zeros        datcor            8 0
171300091127     C                   time                    wn14             14 0
171400091127     C                   movel     wn14          wn6               6 0          *ORA (6)
171500091127     C                   eval      datcor = %dec(%date() : *ISO)
171600040723     C*
171700040723     C* Campi riferito al database
171800040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
171900040723     C     *like         define    tbeke1        kteKE1
172000040723     C*
172100040723     C* Definizione chiavi di lettura
172200040723     C* TNTBE01L - parziale
172300040723     C     keytbe        klist
172400040723     C                   kfld                    kteCOD                         *UTENTE
172500040723     C                   kfld                    kteKE1                         *CODICE
172600000613     C*
172700000613     C                   ENDSR
172800000613     C***
172900990908
173000040709** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
173100121207X;W_VAORFA_1;X;VAORSR;VAOINR;VAOLOR;VAOPRR;VAOCAR;
173200140731W_VAORFA_2;W_VAORFA_3;W_VAORFA_4;X;X;X;X;X;X;X;X;X;
173300141006VAOTER;VAOSMSR;VAOEMLR;W_VAONO1;
