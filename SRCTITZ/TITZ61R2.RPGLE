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
009500150629     D wVAONO2_1       s             35
009600150629     D wVAONO2_2       s             35
009700080702
009800080702     D*------------------
009900080702     D* LINKING A DEFINIZIONI ESTERNE
010000080702     D*------------------
010100090428     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
010200090428     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
010300091026
010400091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
010500091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
010600091026
010700080702
010800031201     D*------------------
010900031201     D* Costanti
011000031201     D*------------------
011100031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
011200031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
011300010330
011400010330
011500010330
011600000913     C                   reset                   rrnum
011700990921     C                   reset                   esito
011800990921     C                   reset                   wrkesito
011900010601     C*
012000010601     C                   exsr      opeini
012100041011     C                   exsr      PREELA
012200040708     C                   exsr      rwvao
012300010601     C*
012400010601     C                   seton                                        lr
012500010601
012600010601
012700010601
012800010601
012900010601     C*--------------------------------------------------------
013000010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
013100010601     C*--------------------------------------------------------
013200010601     C     PREELA        BEGSR
013300010601     C*
013400010601     C* SE OCCORRE SPEDIRE IN FILIALE
013500041011     C                   if        flgGiro = '0'
013600010601     C*
013700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
013800010601     C                   eval      flgGiro = '1'
013900041011     C*
014000041011     C                   endif
014100010601     C*
014200010601     C                   ENDSR
014300010601     C***
014400010601
014500010601
014600010601     C*--------------------------------------------------------
014700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014800010601     C*--------------------------------------------------------
014900010601     C     ENDELA        BEGSR
015000000616     C*
015100120209     C                   ENDSR
015200010601
015300010601
015400010330     C*--------------------------------------------------------
015500040708     C* RWVAO   LEGGE TIVIN00R E SCRIVE FNVAOWWF              *
015600010330     C*--------------------------------------------------------
015700040708     C     RWVAO         BEGSR
015800010330     C*
015900010330     C                   if        not %open(tivin00r)
016000010330     C                   open      tivin00r
016100010330     C                   endif
016200040708     C                   if        not %open(fnvaowwr)
016300040708     C                   open      fnvaowwr
016400010330     C                   endif
016500140716     C*
016600140716     C* Mi predispongo per scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
016700140716     C                   call      'TIS7T7ER'
016800140716     C                   parm      'O'           pIn_Opz           1
016900140716     C                   parm                    tivlrds
017000140716     C                   parm                    FNVAOEAR
017100140716     C                   parm                    oriPRG
017200140716     C                   parm      *zeros        pIn_Dst           3
017300140716     C                   parm      *blanks       pOut_Esito        1
017400140716     C                   if        pOut_Esito = '2'
017500140716     C                   goto      FINE
017600140716     C                   endif
017700010330     C*
017800010604     C                   clear                   §CTROK
017900010604     C                   clear                   §CTRMO
018000010604     C                   clear                   §CTRNO
018100120123     C*
018200120905     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
018300120905     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
018400120905     C                   IF        Intestazioni = *off
018500120123     C* Eseguo primo giro x by-passare la mancanza della 1° riga con le intestazioni
018600120123     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
018700120123     C                   exsr      inzvar
018800120123     C                   exsr      impvao
018900120905     C                   ENDIF
019000010330     C*
019100030822     C                   do        *HIVAL
019200010330     C*
019300030822     C                   read      tivin00r                               70
019400010618     C*
019500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019600010618     C                   if        vinflg = '1'
019700010618     C                   eval      flgOk = '1'
019800010618     C                   endif
019900010618     C*
020000040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
020100040510     C                   z-add     *zeros        wLenVINDTA        4 0
020200040510     C                   z-add     *zeros        wFlgVINDTA        1 0
020300040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
020400040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
020500040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
020600040510     C                   z-add     1             wFlgVINDTA
020700040510     C                   leave
020800040510     C                   endif
020900040510     C                   enddo
021000040510     C*
021100040218     C                   if        vindta > *blanks AND
021200040510     C                             wFlgVINDTA = 1
021300010330     C                   add       1             rrnum
021400010330     C*
021500010601     C                   if        *in70 = *off and
021600010330     C                             (vinflg = *blanks
021700010330     C                              or vinflg = '0'
021800010330     C                              or vinflg = '2')
021900010330     C*
022000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
022100010711     C                   if        vinflg = *blanks or vinflg = '0'
022200010711     C                   clear                   vinmsg
022300010711     C                   endif
022400010601     C*
022500010330     C                   exsr      inzvar
022600040708     C                   exsr      impvao
022700151019     C*
022800151019     C* Stacco subito un numeratore univoco "ORM"
022900151019     C                   exsr      RTVNUM
023000140716     C*
023100140728     C* Verifico se trattasi di formato ORM "esteso"
023200140716     C                   setoff                                       53
023300140728     C                   if        w_VAOFLO2  <> *blanks OR
023400140728     C                             w_VAOTSPS  <> *blanks OR
023500140728     C                             w_VAONOTE  <> *blanks OR
023600140728     C                             w_VAOORDA1 <> *blanks OR
023700140728     C                             w_VAOORAA1 <> *blanks OR
023800140728     C                             w_VAOORDA2 <> *blanks OR
023900140728     C                             w_VAOORAA2 <> *blanks OR
024000140728     C                             w_VAOEMLR  <> *blanks OR
024100140728     C                             w_VAOEMLRF <> *blanks OR
024200140728     C                             w_VAOSMSR  <> *blanks OR
024300140728     C                             w_VAOSMSRF <> *blanks OR
024400140728     C                             w_VAOORIP  <> *blanks OR
024500140728     C                             w_VAODOIO  <> *blanks
024600140716     C                   seton                                        53
024700140716     C                   endif
024800010601     C*
024900010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
025000040708     C                   eval      depfil = VAOPOE
025100010601     C                   exsr      repfil
025200010601     C                   if        depfil = invfil
025300021025     C                   if        vlrpoi = 999
025400040708     C                   move(P)   invfil        VAOPOE
025500021025     C                   else
025600040708     C                   move(P)   vlrpoi        VAOPOE
025700021025     C                   endif
025800010601     C*
025900010604     C* Ebbene...
026000010604     C*
026100030822     C  N31              add       1             §CTROK            7 0
026200030822     C   32              add       1             §CTRMO            7 0
026300030822     C   31              add       1             §CTRNO            7 0
026400030822     C                   if        wGiro = 2
026500120123     C  N31              EXSR      CHKCOR
026600120123     C  N31              EXSR      WRIORI
026700120123     C  N31              EXSR      WRIVGD
026800120123     C  N31              write     FNVAO000
026900140716     C*
027000140728     C* Se richiesta scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
027100140728     C                   if        *in53 and not *in31
027200151019     C                   exsr      EXEFORZ
027300140716     C                   call      'TIS7T7ER'
027400140716     C                   parm      'W'           pIn_Opz           1
027500140716     C                   parm                    tivlrds
027600140716     C                   parm                    FNVAOEAR
027700140716     C                   parm                    oriPRG
027800140716     C                   parm      *zeros        pIn_Dst           3
027900140716     C                   parm      *blanks       pOut_Esito        1
028000140716     C                   endif
028100140716     C                   if        pOut_Esito = '2'
028200140716     C                   seton                                        32
028300140716     C                   endif
028400030822     C                   endif
028500010604     C*
028600010604     C                   if        *in31 = *off and
028700010604     C                             *in32 = *off
028800010604     C                   eval      vinflg = '1'
028900010604     C                   else
029000010604     C                   eval      vinflg = '2'
029100010604     C                   endif
029200010604     C                   endif
029300010601     C*
029400010604     C                   endif
029500010604     C*
029600010330     C                   else
029700010330     C                   eval      vinflg = '1'
029800010330     C                   endif
029900010601     C*
030000010601     C  N70              update    tivin000
030100010330     C*
030200030822     C  N70              enddo
030300140716     C*
030400140716     C     FINE          tag
030500140716     C*
030600140716     C* Chiusura per scrittura estensione ORM (FNVAOE0F) => richiamo l'apposito driver
030700140716     C                   call      'TIS7T7ER'
030800140716     C                   parm      'C'           pIn_Opz           1
030900140716     C                   parm                    tivlrds
031000140716     C                   parm                    FNVAOEAR
031100140716     C                   parm                    oriPRG
031200140716     C                   parm      *zeros        pIn_Dst           3
031300140716     C                   parm      *blanks       pOut_Esito        1
031400010601     C*
031500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
031600010601     C                   if        cntNonEl = *zeros or
031700010601     C                             flgMulti = '0'
031800010330     C* Se non ci sono record con errori ...
031900010601     C                   if        §ctrno = 0 and
032000010604     C                             §ctrmo = 0 and
032100010601     C                             flgStato <> '2'
032200010330     C* ... restituisco esito OK.
032300010330     C                   eval      wrkesito = '0'
032400010330     C                   else
032500010330     C                   if        §ctrok > 0
032600010330     C                   eval      wrkesito = '1'
032700010330     C                   else
032800010615     C                   if        flgOk = '0'
032900010615     C                   eval      wrkesito = '2'
033000010615     C                   else
033100010615     C                   eval      wrkesito = '6'
033200010615     C                   endif
033300010330     C                   endif
033400010330     C                   endif
033500010601     C                   else
033600010601     C                   eval      wrkesito = '9'
033700010601     C                   endif
033800010330     C*
033900010330     C                   if        %open(tivin00r)
034000010330     C                   close     tivin00r
034100010330     C                   endif
034200040708     C                   if        %open(fnvaowwr)
034300040708     C                   close     fnvaowwr
034400010330     C                   endif
034500010601     C*
034600010601     C                   if        vlrpoi <> 999
034700010601     C                   eval      invfil = vlrpoi
034800010601     C                   endif
034900010330     C*
035000010330     C                   if        §ctrok > 0
035100010601     C                             and invfil > *zeros
035200010330     C                   exsr      invio
035300140716     C*
035400140716     C* Invio dati estensione ORM (FNVAOE0F) => richiamo l'apposito driver
035500140716     C                   move      invfil        cmzdst
035600140716     C                   call      'TIS7T7ER'
035700140716     C                   parm      'S'           pIn_Opz           1
035800140716     C                   parm                    tivlrds
035900140716     C                   parm                    FNVAOEAR
036000140716     C                   parm                    oriPRG
036100140716     C                   parm      cmzdst        pIn_Dst           3
036200140716     C                   parm      *blanks       pOut_Esito        1
036300010330     C                   endif
036400010601     C*
036500010618     C                   if        flgGiro = '1'
036600010601     C                   exsr      endela
036700010618     C                   endif
036800010330     C*
036900010330     C                   ENDSR
037000010330     C***
037100151019
037200151019
037300151019     C*------------------------------------------------------------------------*
037400151019     C* RTVNUM - Stacco un numeratore univoco "ORM"
037500151019     C*------------------------------------------------------------------------*
037600151019     C     RTVNUM        BEGSR
037700151019     C*
037800151019     C* Stacco un numeratore da AZNUM
037900151019     C                   clear                   §numori
038000151019     C                   clear                   trul33ds
038100151019     C                   eval      I33OPE = *zeros
038200151019     C                   eval      I33CNU = 600
038300151019     C                   eval      I33NUM = 1
038400151019     C                   movel     TRUL33DS      KPJBU
038500151019     C                   call      'TRUL33R'
038600151019     C                   parm                    KPJBA
038700151019     C                   movel     KPJBU         TRUL33DS
038800151019     C                   if        O33ERR = *zeros
038900151019     C                   z-add     O33NRF        §numori
039000151019     c                   else
039100151019     c                   eval      §numori = *all'9'
039200151019     C                   seton                                        31        * errore bloccante
039300151019     C                   endif
039400151019     C*
039500151019     C                   eval      vaoNOR = §numori
039600151019     C*
039700151019     C                   ENDSR
039800151019
039900151019
040000151019     C*------------------------------------------------------------------------*
040100151019     C* EXEFORZ - Eseguo forzatire di base su formato ORM "esteso"
040200151019     C*------------------------------------------------------------------------*
040300151019     C     EXEFORZ       BEGSR
040400151019     C*
040500151019     C                   move(p)   VAOPOE        w_vaopoe                       * forzatura necessar
040600151019     C                   move(p)   VAOCOR        w_vaocor                       * forzatura necessar
040700151019     C                   move(p)   VAONOR        w_vaonor                       * forzatura necessar
040800151019     C*
040900151019     C                   ENDSR
041000151019
041100040723
041200040723
041300040723     C*------------------------------------------------------------------------*
041400040723     C* CHKCOR - Considerazioni su VAOCOR
041500040723     C*------------------------------------------------------------------------*
041600040723     C     CHKCOR        BEGSR
041700040723     C*
041800040723     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
041900040723     C                   if        vaoTCO = 'F'
042000040723     C* Quindi verifico se il codice ordinante non è valorizzato
042100040723     C                   if        vaoCOR = *zeros  AND
042200040723     C                             vaoRSO = *blanks AND
042300040723     C                             vaoINO = *blanks AND
042400040723     C                             vaoCAO = *blanks AND
042500040723     C                             vaoLOO = *blanks AND
042600040723     C                             vaoPRO = *blanks AND
042700040723     C                             vaoNAO = *blanks
042800040723     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
042900040723     C                   movel     '0'           wFlgAcr           1
043000040723     C                   move      vlrKSC        wCli              7 0
043100040723     C                   move(p)   wCli          acrCRO
043200040723     C     acrCRO        setll     fnacr01l
043300040723     C                   if        %found(fnacr01l)
043400040723     C                   read      fnacr01l
043500040723     C                   dow       not %eof(fnacr01l)
043600040723     C                   movel     acrCRO        wCliAcr           7 0
043700040723     C                   if        wCliAcr = wCli
043800040723     C                   eval      vaoCOR = acrCRO
043900040723     C                   movel     '1'           wFlgAcr
044000040723     C                   leave
044100040723     C                   else
044200040723     C                   leave
044300040723     C                   endif
044400040723     C                   read      fnacr01l
044500040723     C                   enddo
044600040723     C                   endif
044700040723     C* Se nn si è reperito il codice ordinante dall'anagrafico clienti ritiro reperisco i
044800040723     C* dati anagrafici dell'ordinante dal piano dei conti
044900040723     C                   if        wFlgAcr = '0'
045000040723     C                   clear                   BS69DS
045100040723     C                   clear                   ACODS
045200040723     C                   clear                   INDDS
045300040723     C                   clear                   CLPDS
045400040723     C                   clear                   CLSDS
045500040723     C                   move(P)   vlrKSC        I69KAC
045600040723     C                   move(P)   vlrKSC        I69KIN
045700040723     C                   call      'TIBS69R'
045800040723     C                   parm                    BS69DS
045900040723     C                   parm                    ACODS
046000040723     C                   parm                    INDDS
046100040723     C                   parm                    CLPDS
046200040723     C                   parm                    CLSDS
046300040723     C     O69ERR        ifne      '1'
046400040723     C                   eval      vaoRSO = ACORAG
046500040723     C                   eval      vaoINO = INDVIA
046600040723     C                   movel(P)  INDCAP        vaoCAO
046700040723     C                   eval      vaoLOO = INDCIT
046800040723     C                   eval      vaoPRO = INDPRV
046900040723     C                   eval      vaoNAO = INDSTA
047000040723     C                   endif
047100040723     C                   endif
047200040723     C                   endif
047300040723     C                   endif
047400040723     C*
047500040723     C                   ENDSR
047600040723
047700040723
047800040723     C*------------------------------------------------------------------------*
047900040723     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
048000040723     C*------------------------------------------------------------------------*
048100040723     C     WRIORI        BEGSR
048200040723     C*
048300090618     C* Come prima cosa stacco un numeratore da AZNUM
048400151019     C***                clear                   trul33ds
048500151019     C***                eval      I33OPE = *zeros
048600151019     C***                eval      I33CNU = 600
048700151019     C***                eval      I33NUM = 1
048800151019     C***                movel     TRUL33DS      KPJBU
048900151019     C***                call      'TRUL33R'
049000151019     C***                parm                    KPJBA
049100151019     C***                movel     KPJBU         TRUL33DS
049200151019     C***                if        O33ERR = *zeros
049300151019     C***                z-add     O33NRF        §numori
049400151019     c***                else
049500151019     c***                eval      §numori = *all'9'
049600151019     C***                endif
049700040723     C*
049800040723     C* Quindi imposto i campi "extra-VAO"
049900040723     C                   if        vlrKSC = '0ORM0000'
050000040723     C*
050100040723     C                   if        vaoCOR > *zeros
050200040723     C                   movel     vaoCOR        wrkCOR7           7 0
050300040723     C*
050400040723     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
050500040723     C* codice padre. Se è un padre assegno già il codice ordinante.
050600040723     C                   clear                   tibs10ds
050700040723     C                   eval      d10drf = datcor                              *data riferimento
050800040723     C                   eval      d10paf = 'P'                                 *cerca il padre
050900040723     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
051000091026     C*
051100091026     C* Preparazione chiamata al TIBS10R1
051200091026     C                   clear                   tibs10r1ds
051300091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
051400091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
051500091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
051600091026     C*
051700091026     C                   call      'TIBS10R1'
051800091026     C                   parm                    tibs10r1ds
051900091026     C*
052000091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
052100040723     C*
052200040723     C                   if        d10err <> *blanks or                         *No unif. o padre
052300040723     C                             d10cop =  *zeros  or
052400040723     C                             d10cop =  d10cod
052500040723     C                   move      d10cod        oriIDC
052600040723     C                   else                                                   *Figlio
052700040723     C                   move      d10cop        oriIDC
052800040723     C                   endif
052900040723     C*
053000040723     C                   else
053100040723     C                   eval      oriIDC = *zeros
053200040723     C                   endif
053300040723     C*
053400040723     C                   else
053500040723     C                   eval      oriIDC = vlrKSC
053600040723     C                   endif
053700040723     C*
053800040723     C                   eval      oriPRG = §numori
053900040723     C                   eval      oriDER = datcor
054000040723     C                   eval      oriOER = wn6
054100040723     C                   eval      oriPOE = vaoPOE
054200040723     C                   eval      oriNSR = vaoNSR
054300040723     C                   eval      oriNOR = vaoNOR
054400040723     C                   eval      oriNRV = vaoNRV
054500040723     C                   eval      oriFLO = *blanks
054600040723     C*
054700040723     C* Infine valorizzo la chiave esterna sul file FNVAO
054800040723     C                   eval      dorm01 = vaoFLO
054900040723     C                   movel     §numori       §ormpg
055000150506     C                   movel     'S'           §ormfr
055100050412     C*
055200130306     C* valorizzo DORM01 (in questo caso non ci sonodefault)
055300050412     C*
055400050412     C* Infine riassegno il campo VAO contenete i flag operativi precedentemente ridefiniti
055500040723     C                   eval      vaoFLO = dorm01
055600040723     C*
055700040723     C* ...quindi scrivo il file TIORI00F
055800120905     C  N55              WRITE     TIORI000
055900040723     C*
056000040723     C                   ENDSR
056100050826
056200050826
056300050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
056400050826     C     wriVGD        BEGSR
056500050826     C*
056600050826     C* Reperisco la descrizione della fase da tabella
056700050826     C                   movel(p)  'FAR'         KteCOD
056800050826     C                   movel(p)  '000'         KteKE1
056900050826     C     KEYtbe        chain     tntbe01l
057000050826     C                   if        %found(tntbe01l)
057100050826     C                   movel(P)  tbeuni        DFAR
057200050826     C                   else
057300050826     C                   clear                   DFAR
057400050826     C                   endif
057500050826     C*
057600050826     C* Valorizzo buffer record
057700050826     C                   clear                   FNVAPDS
057800050826     C                   eval      vapIDC = oriIDC
057900050826     C                   eval      vapPOE = oriPOE
058000050826     C                   eval      vapRFA = vaoRFA
058100050826     C                   eval      vapPOG = oriPOE
058200050826     C                   eval      vapDAE = oriDER
058300050826     C                   eval      vapORE = oriOER
058400050826     C                   if        d§fardva <> *blanks
058500050826     C                   eval      vapDFA = d§fardva
058600050826     C                   else
058700050826     C                   eval      vapDFA = d§fardes
058800050826     C                   endif
058900050826     C*
059000050826     C                   clear                   tivgd000
059100080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
059200050826     C                   eval      vgdTIP = 'VP'
059300050826     C                   eval      vgdKSU = vapIDC
059400050826     C                   eval      vgdTSC = 'WW'
059500050826     C                   eval      vgdDAT = datcor
059600151207     C                   eval      vgdPGM = procname
059700060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
059800060306     C                   exsr      sr_Control
059900060306     C                   if        Cli_Abil = *on
060000060306     C                   eval      vgdKSU = woriIDC
060100120905     C  N55              write     tivgd000
060200060306     C                   endif
060300050826     C*
060400050826     C                   ENDSR
060500050826     C*------------------------------------------------------------------------*
060600010601
060700010601
060800010330     C*----------------------------------------------------*
060900030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
061000010330     C*----------------------------------------------------*
061100010330     C     INZVAR        BEGSR
061200010330     C*
061300030822     C* Inizializzo variabili di wrk
061400020204     C                   Z-ADD     *zeros        Num5_0            5 0
061500150630     C                   EVAL      wVAONO2_1=*blank
061600150630     C                   EVAL      wVAONO2_2=*blank
061700120124
061800030822     C*
061900030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
062000120123     C                   CLEAR                   FNVAO000
062100140728     C                   CLEAR                   FNVAOEAR
062200120123     C                   CLEAR                   TIORI000
062300030822     C                   CLEAR                   SkSplitCSV
062400030822     C                   EVAL      i = 1
062500030822     C                   EVAL      posDa = *zeros
062600030822     C                   EVAL      posA  = *zeros
062700030822     C*
062800030822     C* Reimposto i valori di default
062900030822     C                   EXSR      DEFCAM
063000030822     C*
063100010330     C                   ENDSR
063200010330     C*----------------------------------------------------*
063300030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
063400010330     C*----------------------------------------------------*
063500010330     C     DEFCAM        BEGSR
063600030822     C*
063700030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
063800030715     C* e delimitatore testo.
063900030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
064000030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
064100030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
064200040723     C*
064300080707     C* Imposto i valori d default d alcuni campi
064400080708     C*
064500080708     C                   CLEAR                   DORM01
064600120822     C                   EVAL      VAOFLO = DORM01
064700080707     C*
064800121207     C                   EVAL      VAOPOE = 045
064900121207     C                   EVAL      VAOTOR = 'S'
065000121207     C                   EVAL      VAOCOR = 0450652000
065100140401     C***                EVAL      VAOCRC = 0450652000
065200140401     C***                EVAL      VAOKSC = 0450652
065300121207     C                   EVAL      VAONAM = 'RESI'
065400121207     C                   EVAL      VAONCL = 1
065500121207     C                   EVAL      VAOPKG = 1
065600121207     C                   EVAL      VAOPAG = 'O'
065700121207     C                   EVAL      VAOCTR = '000'
065800120905     C* se il 1° char dei parametri è T=Test
065900120905     C                   SETOFF                                       55
066000120905     C                   IF        %subst(vlrppt:1:1) = 'T'
066100120905     C* inibisco la scrittura dei file TIORI e TIVGD
066200120905     C                   SETON                                        55
066300120905     C* imposto il PO di emissione come filiale fittizia
066400120905     C                   EVAL      VAOPOE = 999
066500120905     C                   ENDIF
066600091127     C*
066700091204     C* IMPOSTO I CAMPI PERSONALIZZATI
066800100416     C
066900030822     C*
067000030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
067100030822     C                   EVAL      posDaDft = 1
067200030822     C                   EVAL      posADft  = 0
067300030822     C                   EVAL      wGiroDft = 0
067400030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
067500030822     C                             posDaDft > 0
067600030822     C*
067700030822     C* Gestisco il 1° giro
067800030822     C                   IF        wGiroDft = 0
067900030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
068000030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
068100030822     C* Incremento il contatore dei "giri"
068200030822     C                   EVAL      wGiroDft = 1
068300030822     C                   ELSE
068400030822     C                   EVAL      posDaDft = posADft
068500030822     C                   ENDIF
068600030822     C* Eseguo lo scan x trovare la fine del campo corrente
068700030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
068800030822     C*
068900030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
069000030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
069100030822     C                   IF        posDaDft > 0 AND
069200030822     C                             posADft  > 0
069300040708     C* POE
069400040708     C                   IF        %subst(
069500040708     C                             %subst(vlrppt:posDaDft+1:
069600040708     C                             posADft-posDaDft-1):1:3)
069700040708     C                             = 'POE'
069800040708     C                   EVAL      PiStr=%trim(%subst(
069900040708     C                             %subst(vlrppt:posDaDft+1:
070000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070100040708     C                   EXSR      CHKNUM
070200040708     C                   IF        PiInt=*on
070300040708     C                   Z-ADD     PiVal         VAOPOE
070400040708     C                   ENDIF
070500040708     C                   ENDIF
070600040708     C* TOR
070700040708     C                   IF        %subst(
070800040708     C                             %subst(vlrppt:posDaDft+1:
070900040708     C                             posADft-posDaDft-1):1:3)
071000040708     C                             = 'TOR'
071100040708     C                   EVAL      VAOTOR=%trim(%subst(
071200040708     C                             %subst(vlrppt:posDaDft+1:
071300040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071400040708     C                   ENDIF
071500040708     C* TCO
071600040708     C                   IF        %subst(
071700040708     C                             %subst(vlrppt:posDaDft+1:
071800040708     C                             posADft-posDaDft-1):1:3)
071900040708     C                             = 'TCO'
072000040708     C                   EVAL      VAOTCO=%trim(%subst(
072100040708     C                             %subst(vlrppt:posDaDft+1:
072200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072300040708     C                   ENDIF
072400040707     C* COR
072500030822     C                   IF        %subst(
072600030822     C                             %subst(vlrppt:posDaDft+1:
072700030822     C                             posADft-posDaDft-1):1:3)
072800040707     C                             = 'COR'
072900030822     C                   EVAL      PiStr=%trim(%subst(
073000030822     C                             %subst(vlrppt:posDaDft+1:
073100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073200030822     C                   EXSR      CHKNUM
073300030822     C                   IF        PiInt=*on
073400040708     C                   Z-ADD     PiVal         VAOCOR
073500030822     C                   ENDIF
073600030822     C                   ENDIF
073700040708     C* CRC
073800040708     C                   IF        %subst(
073900040708     C                             %subst(vlrppt:posDaDft+1:
074000040708     C                             posADft-posDaDft-1):1:3)
074100040708     C                             = 'CRC'
074200040708     C                   EVAL      PiStr=%trim(%subst(
074300040708     C                             %subst(vlrppt:posDaDft+1:
074400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074500040708     C                   EXSR      CHKNUM
074600040708     C                   IF        PiInt=*on
074700040708     C                   Z-ADD     PiVal         VAOCRC
074800040708     C                   ENDIF
074900040708     C                   ENDIF
075000040708     C* FFD
075100040708     C                   IF        %subst(
075200040708     C                             %subst(vlrppt:posDaDft+1:
075300040708     C                             posADft-posDaDft-1):1:3)
075400040708     C                             = 'FFD'
075500040708     C                   EVAL      VAOFFD=%trim(%subst(
075600040708     C                             %subst(vlrppt:posDaDft+1:
075700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075800040708     C                   ENDIF
075900040708     C* ORR
076000040708     C                   IF        %subst(
076100040708     C                             %subst(vlrppt:posDaDft+1:
076200040708     C                             posADft-posDaDft-1):1:3)
076300040708     C                             = 'ORR'
076400040708     C                   EVAL      PiStr=%trim(%subst(
076500040708     C                             %subst(vlrppt:posDaDft+1:
076600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076700040708     C                   EXSR      CHKNUM
076800040708     C                   IF        PiInt=*on
076900040708     C                   Z-ADD     PiVal         VAOORR
077000040708     C                   ENDIF
077100040708     C                   ENDIF
077200040712     C* NCL
077300040712     C                   IF        %subst(
077400040712     C                             %subst(vlrppt:posDaDft+1:
077500040712     C                             posADft-posDaDft-1):1:3)
077600040712     C                             = 'NCL'
077700040712     C                   EVAL      PiStr=%trim(%subst(
077800040712     C                             %subst(vlrppt:posDaDft+1:
077900040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078000040712     C                   EXSR      CHKNUM
078100040712     C                   IF        PiInt=*on
078200040712     C                   Z-ADD     PiVal         VAONCL
078300040712     C                   ENDIF
078400040712     C                   ENDIF
078500040712     C* PKG
078600040712     C                   IF        %subst(
078700040712     C                             %subst(vlrppt:posDaDft+1:
078800040712     C                             posADft-posDaDft-1):1:3)
078900040712     C                             = 'PKG'
079000040712     C                   EVAL      PiStr=%trim(%subst(
079100040712     C                             %subst(vlrppt:posDaDft+1:
079200040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079300040712     C                   EXSR      CHKNUM
079400040712     C                   IF        PiNum=*on
079500040712     C                   Z-ADD     PiVal         VAOPKG
079600040712     C                   ENDIF
079700040712     C                   ENDIF
079800040712     C* VLM
079900040712     C                   IF        %subst(
080000040712     C                             %subst(vlrppt:posDaDft+1:
080100040712     C                             posADft-posDaDft-1):1:3)
080200040712     C                             = 'VLM'
080300040712     C                   EVAL      PiStr=%trim(%subst(
080400040712     C                             %subst(vlrppt:posDaDft+1:
080500040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080600040712     C                   EXSR      CHKNUM
080700040712     C                   IF        PiNum=*on
080800040712     C                   Z-ADD     PiVal         VAOVLM
080900040712     C                   ENDIF
081000040712     C                   ENDIF
081100040708     C* NAM
081200040708     C                   IF        %subst(
081300040708     C                             %subst(vlrppt:posDaDft+1:
081400040708     C                             posADft-posDaDft-1):1:3)
081500040708     C                             = 'NAM'
081600040708     C                   EVAL      VAONAM=%trim(%subst(
081700040708     C                             %subst(vlrppt:posDaDft+1:
081800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081900040708     C                   ENDIF
082000040708     C* PAG
082100030822     C                   IF        %subst(
082200030822     C                             %subst(vlrppt:posDaDft+1:
082300030822     C                             posADft-posDaDft-1):1:3)
082400040708     C                             = 'PAG'
082500040708     C                   EVAL      VAOPAG=%trim(%subst(
082600030822     C                             %subst(vlrppt:posDaDft+1:
082700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082800030822     C                   ENDIF
082900040708     C* KSC
083000040708     C                   IF        %subst(
083100040708     C                             %subst(vlrppt:posDaDft+1:
083200040708     C                             posADft-posDaDft-1):1:3)
083300040708     C                             = 'KSC'
083400040708     C                   EVAL      PiStr=%trim(%subst(
083500040708     C                             %subst(vlrppt:posDaDft+1:
083600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083700040708     C                   EXSR      CHKNUM
083800040708     C                   IF        PiInt=*on
083900040708     C                   Z-ADD     PiVal         VAOKSC
084000040708     C                   ENDIF
084100040708     C                   ENDIF
084200040708     C* CTR
084300040708     C                   IF        %subst(
084400040708     C                             %subst(vlrppt:posDaDft+1:
084500040708     C                             posADft-posDaDft-1):1:3)
084600040708     C                             = 'CTR'
084700040708     C                   EVAL      VAOCTR=%trim(%subst(
084800040708     C                             %subst(vlrppt:posDaDft+1:
084900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085000040708     C                   ENDIF
085100040708     C* POC
085200040708     C                   IF        %subst(
085300040708     C                             %subst(vlrppt:posDaDft+1:
085400040708     C                             posADft-posDaDft-1):1:3)
085500040708     C                             = 'POC'
085600040708     C                   EVAL      PiStr=%trim(%subst(
085700040708     C                             %subst(vlrppt:posDaDft+1:
085800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085900040708     C                   EXSR      CHKNUM
086000040708     C                   IF        PiInt=*on
086100040708     C                   Z-ADD     PiVal         VAOPOC
086200040708     C                   ENDIF
086300040708     C                   ENDIF
086400040708     C* NO1
086500040708     C                   IF        %subst(
086600040708     C                             %subst(vlrppt:posDaDft+1:
086700121129     C                             posADft-posDaDft-1):1:3)
086800040708     C                             = 'NO1'
086900040708     C                   EVAL      VAONO1=%trim(%subst(
087000040708     C                             %subst(vlrppt:posDaDft+1:
087100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
087200040708     C                   ENDIF
087300040708     C* DDT
087400030822     C                   IF        %subst(
087500030822     C                             %subst(vlrppt:posDaDft+1:
087600030822     C                             posADft-posDaDft-1):1:3)
087700040708     C                             = 'DDT'
087800040708     C                   EVAL      VAODDT=%trim(%subst(
087900030822     C                             %subst(vlrppt:posDaDft+1:
088000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
088100030822     C                   ENDIF
088200040708     C* RFA
088300040708     C                   IF        %subst(
088400040708     C                             %subst(vlrppt:posDaDft+1:
088500040708     C                             posADft-posDaDft-1):1:3)
088600040708     C                             = 'RFA'
088700040708     C                   EVAL      VAORFA=%trim(%subst(
088800040708     C                             %subst(vlrppt:posDaDft+1:
088900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
089000040708     C                   ENDIF
089100040708     C* FLO
089200040708     C                   IF        %subst(
089300040708     C                             %subst(vlrppt:posDaDft+1:
089400040708     C                             posADft-posDaDft-1):1:3)
089500040708     C                             = 'FLO'
089600040708     C                   EVAL      VAOFLO=%trim(%subst(
089700040708     C                             %subst(vlrppt:posDaDft+1:
089800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
089900091103     C*
090000040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
090100080702     C* e posizione 7 del VAOFLO
090200040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
090300040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
090400050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
090500040708     C                   ENDIF
090600030822     C* ...
090700030822     C                   ENDIF
090800030822     C                   ENDDO
090900020204     C*
091000010330     C                   ENDSR
091100010607     C*----------------------------------------------------*
091200040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
091300010607     C*----------------------------------------------------*
091400040708     C     IMPVAO        BEGSR
091500010607     C*
091600010607     C                   SETOFF                                       3132
091700120124
091800030515     C*
091900040708     C                   MOVEL     datcor        VAODAO
092000040708     C                   MOVEL     wn14          VAOOAO
092100050314     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
092200050314     C     minu:maiu     XLATE     vindta        vindta
092300010607     C*
092400030715     C**********
092500030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
092600030715     C**********
092700030715     C                   IF        wGiro = *zeros
092800040709     C*
092900040709     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
093000040709     C                   EVAL      depvinDTA = vinDTA
093100040709     C*
093200040709     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
093300040709     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
093400040709     C                                      %trim(intcol(3))
093500031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
093600050314     C**!!!minu:maiu     XLATE     vindta        vindta
093700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
093800040324     C                   DOW       posDa <= %len(%trimr(vindta))
093900030715     C*
094000030715     C* Gestisco il 1° campo
094100030715     C                   IF        i = 1
094200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
094300030715     C                   EVAL      posDa = 1
094400030715     C* Eseguo lo scan x trovare la fine del primo campo
094500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094600030715     C* A questo punto "estraggo" il campo corrente
094700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
094800030715     C                                                    (posA-posDa))
094900030715     C* X i campi successivi al 1°
095000030715     C                   ELSE
095100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
095200030715     C                   EVAL      posDa = posA + 1
095300030715     C* Eseguo lo scan x trovare la fine del campo corrente
095400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
095500030715     C* Gestisco l'ultimo campo
095600030715     C                   IF        posA = *zeros
095700040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
095800030715     C* A questo punto "estraggo" il campo corrente
095900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
096000030715     C                                                    (posA-posDa))
096100030715     C                   LEAVE
096200030715     C                   ELSE
096300030715     C* A questo punto "estraggo" il campo corrente
096400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
096500030715     C                                                    (posA-posDa))
096600030715     C                   ENDIF
096700030715     C                   ENDIF
096800030715     C* Incremento il contatore d campo
096900030715     C                   EVAL      i = i +1
097000030715     C                   ENDDO
097100030715     C*
097200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
097300030715     C* delimitazione testo
097400030715     C                   EVAL      i = 1
097500030715     C                   DOW       i < %elem(SkSplitFLD)
097600030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
097700030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
097800030715     C                   EVAL      i = i + 1
097900030715     C                   ENDDO
098000030715     C                   EVAL      wGiro = 1
098100040709     C* Ripristino la 1° riga originale
098200040709     C                   EVAL      vinDTA = depvinDTA
098300030715     C                   ELSE
098400030715     C                   EVAL      wGiro = 2
098500031204     C**********
098600031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
098700031204     C* d separatore campo
098800031204     C**********
098900040709     C                   EVAL      vindta = %trim(vindta)
099000040709     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
099100040709     C                   EVAL      vindta = %subst(vindta:2)
099200040709     C                   ENDDO
099300040709     C*
099400040709     C                   Z-ADD     *zeros        lunghInput        4 0
099500040709     C                   EVAL      lunghInput = %len(%trim(vindta))
099600040709     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
099700040709     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
099800040709     C                   EVAL      lunghInput = %len(%trim(vindta))
099900040709     C                   ENDDO
100000030715     C**********
100100030715     C* Effettuo lo split del campo dati d input
100200030715     C**********
100300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
100400040324     C                   DOW       posDa <= %len(%trimr(vindta))
100500030715     C*
100600030715     C* Gestisco il 1° campo
100700030715     C                   IF        i = 1
100800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
100900030715     C                   EVAL      posDa = 1
101000030715     C* Eseguo lo scan x trovare la fine del primo campo
101100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
101200030715     C* A questo punto "estraggo" il campo corrente
101300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
101400030715     C                                                    (posA-posDa))
101500030715     C* X i campi successivi al 1°
101600030715     C                   ELSE
101700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
101800030715     C                   EVAL      posDa = posA + 1
101900031204     C* Verifico che nn vi sia il campo nn valorizzato
102000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
102100031204     C* Se campo nn valorizzato skippo al prossimo
102200031204     C                   EVAL      SkSplitCSV(i) = *blanks
102300031204     C                   EVAL      posA  = posA + 1
102400031204     C                   ELSE
102500030715     C* Eseguo lo scan x trovare la fine del campo corrente
102600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
102700030715     C* Gestisco l'ultimo campo
102800030715     C                   IF        posA = *zeros
102900040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
103000030715     C* A questo punto "estraggo" il campo corrente
103100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
103200030715     C                                                    (posA-posDa))
103300030715     C                   LEAVE
103400030715     C                   ELSE
103500030715     C* A questo punto "estraggo" il campo corrente
103600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
103700030715     C                                                    (posA-posDa))
103800030715     C                   ENDIF
103900031204     C                   ENDIF
104000030715     C                   ENDIF
104100030715     C* Incremento il contatore d campo
104200030715     C                   EVAL      i = i +1
104300030715     C                   ENDDO
104400030715     C*
104500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
104600030715     C* delimitazione testo
104700030715     C                   EVAL      i = 1
104800030715     C                   DOW       i < %elem(SkSplitCSV)
104900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
105000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
105100030715     C                   EVAL      i = i + 1
105200030715     C                   ENDDO
105300030715     C*
105400030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
105500030715     C                   EVAL      i = 1
105600030715     C                   DOW       i < %elem(SkSplitFLD)
105700030715     C*
105800030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
105900030715     C                   IF        SkSplitFLD(i) <> *blanks AND
106000030715     C                             SkSplitCSV(i) <> *blanks
106100030721     C*** ATB
106200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATB'
106300040708     C                   EVAL      VAOATB = SkSplitCSV(i)
106400030721     C                   ENDIF
106500040708     C*** TOR
106600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTOR'
106700040708     C                   EVAL      VAOTOR = SkSplitCSV(i)
106800040708     C                   ENDIF
106900040708     C*** TCO
107000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTCO'
107100040708     C                   EVAL      VAOTCO = SkSplitCSV(i)
107200040708     C                   ENDIF
107300040708     C*** RSO
107400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSO'
107500040708     C                   EVAL      VAORSO = SkSplitCSV(i)
107600030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
107700040708     C     '@':'A'       XLATE     VAORSO        VAORSO
107800030715     C* ==
107900030715     C                   ENDIF
108000040708     C*** INO
108100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINO'
108200040708     C                   EVAL      VAOINO = SkSplitCSV(i)
108300030715     C                   ENDIF
108400040708     C*** LOO
108500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOO'
108600040708     C                   EVAL      VAOLOO = SkSplitCSV(i)
108700030715     C                   ENDIF
108800040708     C*** PRO
108900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRO'
109000040708     C                   EVAL      VAOPRO = SkSplitCSV(i)
109100030715     C                   ENDIF
109200040708     C*** NAO
109300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAO'
109400040708     C                   EVAL      VAONAO = SkSplitCSV(i)
109500030721     C                   ENDIF
109600040708     C*** RSR
109700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSR'
109800040708     C                   EVAL      VAORSR = SkSplitCSV(i)
109900040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
110000040708     C     '@':'A'       XLATE     VAORSR        VAORSR
110100040708     C* ==
110200040708     C                   IF        VAORSR = *blanks
110300040708     C                   SETON                                        31
110400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
110500040708     C                             + ' ' + 'VAORSR'
110600040708     C                   ENDIF
110700040708     C                   ENDIF
110800040708     C*** INR
110900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINR'
111000040708     C                   EVAL      VAOINR = SkSplitCSV(i)
111100040708     C                   IF        VAOINR = *blanks
111200040708     C                   SETON                                        31
111300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
111400040708     C                             + ' ' + 'VAOINR'
111500040708     C                   ENDIF
111600040708     C                   ENDIF
111700040708     C*** LOR
111800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOR'
111900040708     C                   EVAL      VAOLOR = SkSplitCSV(i)
112000040708     C                   IF        VAOLOR = *blanks
112100040708     C                   SETON                                        31
112200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
112300040708     C                             + ' ' + 'VAOLOR'
112400040708     C                   ENDIF
112500040708     C                   ENDIF
112600040708     C*** PRR
112700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRR'
112800040708     C                   EVAL      VAOPRR = SkSplitCSV(i)
112900040708     C                   IF        VAOPRR = *blanks
113000040708     C                   SETON                                        31
113100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
113200040708     C                             + ' ' + 'VAOPRR'
113300040708     C                   ENDIF
113400040708     C                   ENDIF
113500040708     C*** NAR
113600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAR'
113700040708     C                   EVAL      VAONAR = SkSplitCSV(i)
113800040708     C                   ENDIF
113900040708     C*** RER
114000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORER'
114100040708     C                   EVAL      VAORER = SkSplitCSV(i)
114200040708     C                   ENDIF
114300040708     C*** TER
114400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTER'
114500040708     C                   EVAL      VAOTER = SkSplitCSV(i)
114600040708     C                   ENDIF
114700140728     C*** SMSR
114800140728     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSMSR'
114900140728     C                   EVAL      w_VAOSMSR = SkSplitCSV(i)
115000140728     C                   ENDIF
115100140728     C*** EMLR
115200140728     C                   IF        %trim(SkSplitFLD(i)) = 'VAOEMLR'
115300140728     C                   EVAL      w_VAOEMLR = SkSplitCSV(i)
115400140728     C                   ENDIF
115500040708     C*** RSC
115600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSC'
115700040708     C                   EVAL      VAORSC = SkSplitCSV(i)
115800040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
115900040708     C     '@':'A'       XLATE     VAORSC        VAORSC
116000040708     C* ==
116100040708     C                   ENDIF
116200040708     C*** INC
116300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINC'
116400040708     C                   EVAL      VAOINC = SkSplitCSV(i)
116500040708     C                   ENDIF
116600040708     C*** LOC
116700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOC'
116800040708     C                   EVAL      VAOLOC = SkSplitCSV(i)
116900040708     C                   ENDIF
117000040708     C*** PRC
117100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRC'
117200040708     C                   EVAL      VAOPRC = SkSplitCSV(i)
117300040708     C                   ENDIF
117400040708     C*** NAC
117500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAC'
117600040708     C                   EVAL      VAONAC = SkSplitCSV(i)
117700040708     C                   ENDIF
117800040708     C*** FFD
117900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFFD'
118000040708     C                   EVAL      VAOFFD = SkSplitCSV(i)
118100040708     C                   ENDIF
118200040708     C*** RMP
118300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORMP'
118400040708     C                   EVAL      VAORMP = SkSplitCSV(i)
118500040708     C                   ENDIF
118600040708     C*** NAM
118700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAM'
118800040708     C                   EVAL      VAONAM = SkSplitCSV(i)
118900040708     C                   ENDIF
119000040708     C*** PAG
119100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPAG'
119200040708     C                   EVAL      VAOPAG = SkSplitCSV(i)
119300040708     C                   ENDIF
119400040708     C*** CTR
119500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCTR'
119600040708     C                   EVAL      VAOCTR = SkSplitCSV(i)
119700040708     C                   ENDIF
119800040708     C*** ZOR
119900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOZOR'
120000040708     C                   EVAL      VAOZOR = SkSplitCSV(i)
120100040708     C                   ENDIF
120200040708     C*** NO1
120300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO1'
120400040708     C                   EVAL      VAONO1 = SkSplitCSV(i)
120500030715     C                   ENDIF
120600040708     C*** NO2
120700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO2'
120800040708     C                   EVAL      VAONO2 = SkSplitCSV(i)
120900030715     C                   ENDIF
121000040708     C*** DDT
121100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODDT'
121200040708     C                   EVAL      VAODDT = SkSplitCSV(i)
121300030715     C                   ENDIF
121400040708     C*** RFA
121500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORFA'
121600040708     C                   EVAL      VAORFA = SkSplitCSV(i)
121700030715     C                   ENDIF
121800040708     C*** SPI
121900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSPI'
122000040708     C                   EVAL      VAOSPI = SkSplitCSV(i)
122100030715     C                   ENDIF
122200040708     C*** FLO
122300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
122400040708     C                   EVAL      VAOFLO = SkSplitCSV(i)
122500040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
122600080708     C* e la posizione 7 del VAOFLO
122700040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
122800040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
122900050314     C                   EVAL      %subst(VAOFLO:7:1) = *blanks
123000030715     C                   ENDIF
123100091103     C
123200091103     C*** DAR
123300050314     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
123400050412     C                   EVAL      PiStr=SkSplitCSV(i)
123500050412     C                   EXSR      CHKNUM
123600050412     C                   IF        PiInt=*on
123700050412     C                   Z-ADD     PiVal         VAODAR
123800050412     C                   ELSE
123900050412     C                   SETON                                        32
124000050412     C                   EVAL      VAODAR = *zeros
124100050412     C                   EVAL      vinmsg = %trimr(vinmsg)
124200050412     C                             + ' ' + 'VAODAR'
124300050412     C                   ENDIF
124400050314     C                   ENDIF
124500030715     C
124600030721     C*
124700030721     C* Reperisco quindi i campi numerici...
124800040708     C*** POE
124900040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
125000040723     C                   EVAL      PiStr=SkSplitCSV(i)
125100040723     C                   EXSR      CHKNUM
125200040723     C                   IF        PiInt=*on    AND
125300040723     C                             PiVal<=999   AND
125400040723     C                             PiVal>*zeros
125500040723     C                   Z-ADD     PiVal         VAOPOE
125600040723     C                   ELSE
125700040723     C                   SETON                                        32
125800040723     C                   EVAL      VAOPOE = *zeros
125900040723     C                   EVAL      vinmsg = %trimr(vinmsg)
126000040723     C                             + ' ' + 'VAOPOE'
126100040723     C                   ENDIF
126200040723     C                   ENDIF
126300040708     C*** COR
126400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
126500040708     C                   EVAL      PiStr=SkSplitCSV(i)
126600040708     C                   EXSR      CHKNUM
126700040708     C                   IF        PiInt=*on
126800040708     C                   Z-ADD     PiVal         VAOCOR
126900040708     C                   ELSE
127000040708     C                   SETON                                        31
127100040708     C                   EVAL      VAOCOR = *zeros
127200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
127300040708     C                             + ' ' + 'VAOCOR'
127400040708     C                   ENDIF
127500040708     C                   ENDIF
127600040708     C*** CAO
127700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
127800030721     C                   EVAL      PiStr=SkSplitCSV(i)
127900030721     C                   EXSR      CHKNUM
128000030721     C                   IF        PiInt=*on
128100030721     C                   Z-ADD     PiVal         Num5_0
128200040708     C                   MOVEL(p)  Num5_0        VAOCAO
128300030721     C                   ELSE
128400030721     C                   SETON                                        32
128500040708     C                   EVAL      VAOCAO = *zeros
128600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128700040708     C                             + ' ' + 'VAOCAO'
128800030721     C                   ENDIF
128900030721     C                   ENDIF
129000040708     C*** CRA
129100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
129200030721     C                   EVAL      PiStr=SkSplitCSV(i)
129300030721     C                   EXSR      CHKNUM
129400030721     C                   IF        PiInt=*on
129500040708     C                   Z-ADD     PiVal         VAOCRA
129600030721     C                   ELSE
129700030721     C                   SETON                                        32
129800040708     C                   EVAL      VAOCRA = *zeros
129900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130000040708     C                             + ' ' + 'VAOCRA'
130100030721     C                   ENDIF
130200030721     C                   ENDIF
130300040708     C*** CAR
130400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
130500040708     C                   EVAL      PiStr=SkSplitCSV(i)
130600040708     C                   EXSR      CHKNUM
130700040708     C                   IF        PiInt=*on
130800040708     C                   Z-ADD     PiVal         Num5_0
130900040708     C                   MOVEL(p)  Num5_0        VAOCAR
131000040708     C                   ELSE
131100040708     C                   SETON                                        31
131200040708     C                   EVAL      VAOCAR = *zeros
131300040708     C                   EVAL      vinmsg = %trimr(vinmsg)
131400040708     C                             + ' ' + 'VAOCAR'
131500040708     C                   ENDIF
131600040708     C                   ENDIF
131700040708     C*** CRC
131800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
131900040708     C                   EVAL      PiStr=SkSplitCSV(i)
132000040708     C                   EXSR      CHKNUM
132100040708     C                   IF        PiInt=*on
132200040708     C                   Z-ADD     PiVal         VAOCRC
132300040708     C                   ELSE
132400040708     C                   SETON                                        32
132500040708     C                   EVAL      VAOCRC = *zeros
132600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
132700040708     C                             + ' ' + 'VAOCRC'
132800040708     C                   ENDIF
132900040708     C                   ENDIF
133000040708     C*** CAC
133100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
133200040708     C                   EVAL      PiStr=SkSplitCSV(i)
133300040708     C                   EXSR      CHKNUM
133400040708     C                   IF        PiInt=*on
133500040708     C                   Z-ADD     PiVal         Num5_0
133600040708     C                   MOVEL(p)  Num5_0        VAOCAC
133700040708     C                   ELSE
133800040708     C                   SETON                                        32
133900040708     C                   EVAL      VAOCAC = *zeros
134000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
134100040708     C                             + ' ' + 'VAOCAC'
134200040708     C                   ENDIF
134300040708     C                   ENDIF
134400040708     C*** DAR
134500090428     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
134600090428     C                   EVAL      PiStr=SkSplitCSV(i)
134700090428     C                   EXSR      CHKNUM
134800090428     C                   IF        PiInt=*on
134900090428     C                   Z-ADD     PiVal         VAODAR
135000090428     C                   ELSE
135100090428     C                   SETON                                        32
135200090428     C                   EVAL      VAODAR = *zeros
135300090428     C                   EVAL      vinmsg = %trimr(vinmsg)
135400090428     C                             + ' ' + 'VAODAR'
135500090428     C                   ENDIF
135600090428     C                   ENDIF
135700040708     C*** ORR
135800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
135900040708     C                   EVAL      PiStr=SkSplitCSV(i)
136000040708     C                   EXSR      CHKNUM
136100040708     C                   IF        PiInt=*on
136200040708     C                   Z-ADD     PiVal         VAOORR
136300040708     C                   ELSE
136400040708     C                   SETON                                        32
136500040708     C                   EVAL      VAOORR = *zeros
136600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
136700040708     C                             + ' ' + 'VAOORR'
136800040708     C                   ENDIF
136900040708     C                   ENDIF
137000030721     C*** NCL
137100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
137200030721     C                   EVAL      PiStr=SkSplitCSV(i)
137300030721     C                   EXSR      CHKNUM
137400030721     C                   IF        PiInt=*on
137500040723     C                   IF        PiVal > *zeros
137600040708     C                   Z-ADD     PiVal         VAONCL
137700040723     C                   ENDIF
137800030721     C                   ELSE
137900030721     C                   SETON                                        32
138000040708     C                   EVAL      VAONCL = *zeros
138100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138200040708     C                             + ' ' + 'VAONCL'
138300030721     C                   ENDIF
138400030721     C                   ENDIF
138500040708     C*** PKG
138600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
138700030721     C                   EVAL      PiStr=SkSplitCSV(i)
138800030721     C                   EXSR      CHKNUM
138900030721     C                   IF        PiNum=*on
139000040723     C                   IF        PiVal > *zeros
139100040708     C                   Z-ADD     PiVal         VAOPKG
139200040723     C                   ENDIF
139300030721     C                   ELSE
139400030721     C                   SETON                                        32
139500040708     C                   EVAL      VAOPKG = *zeros
139600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139700040708     C                             + ' ' + 'VAOPKG'
139800030721     C                   ENDIF
139900030721     C                   ENDIF
140000040708     C*** VLM
140100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
140200040708     C                   EVAL      PiStr=SkSplitCSV(i)
140300040708     C                   EXSR      CHKNUM
140400040708     C                   IF        PiNum=*on
140500040708     C                   Z-ADD     PiVal         VAOVLM
140600040708     C                   ELSE
140700040708     C                   SETON                                        32
140800040708     C                   EVAL      VAOVLM = *zeros
140900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
141000040708     C                             + ' ' + 'VAOVLM'
141100040708     C                   ENDIF
141200040708     C                   ENDIF
141300040708     C*** BNC
141400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
141500040708     C                   EVAL      PiStr=SkSplitCSV(i)
141600040708     C                   EXSR      CHKNUM
141700040708     C                   IF        PiInt=*on
141800040708     C                   Z-ADD     PiVal         VAOBNC
141900040708     C                   ELSE
142000040708     C                   SETON                                        32
142100040708     C                   EVAL      VAOBNC = *zeros
142200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
142300040708     C                             + ' ' + 'VAOBNC'
142400040708     C                   ENDIF
142500040708     C                   ENDIF
142600040708     C*** BLC
142700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
142800040708     C                   EVAL      PiStr=SkSplitCSV(i)
142900040708     C                   EXSR      CHKNUM
143000040708     C                   IF        PiInt=*on
143100040708     C                   Z-ADD     PiVal         VAOBLC
143200040708     C                   ELSE
143300040708     C                   SETON                                        32
143400040708     C                   EVAL      VAOBLC = *zeros
143500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
143600040708     C                             + ' ' + 'VAOBLC'
143700040708     C                   ENDIF
143800040708     C                   ENDIF
143900040708     C*** ATT
144000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
144100040708     C                   EVAL      PiStr=SkSplitCSV(i)
144200040708     C                   EXSR      CHKNUM
144300040708     C                   IF        PiInt=*on
144400040708     C                   Z-ADD     PiVal         VAOATT
144500040708     C                   ELSE
144600040708     C                   SETON                                        32
144700040708     C                   EVAL      VAOATT = *zeros
144800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
144900040708     C                             + ' ' + 'VAOATT'
145000040708     C                   ENDIF
145100040708     C                   ENDIF
145200040708     C*** MTC
145300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
145400040708     C                   EVAL      PiStr=SkSplitCSV(i)
145500040708     C                   EXSR      CHKNUM
145600040708     C                   IF        PiInt=*on
145700040708     C                   Z-ADD     PiVal         VAOMTC
145800040708     C                   ELSE
145900040708     C                   SETON                                        32
146000040708     C                   EVAL      VAOMTC = *zeros
146100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
146200040708     C                             + ' ' + 'VAOMTC'
146300040708     C                   ENDIF
146400040708     C                   ENDIF
146500040708     C*** KSC
146600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
146700030721     C                   EVAL      PiStr=SkSplitCSV(i)
146800030721     C                   EXSR      CHKNUM
146900030721     C                   IF        PiInt=*on      AND
147000030721     C                             PiVal<=9999999 AND
147100030721     C                             PiVal>*zeros
147200040708     C                   Z-ADD     PiVal         VAOKSC
147300030721     C                   ELSE
147400030721     C                   SETON                                        32
147500040708     C                   EVAL      VAOKSC = *zeros
147600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
147700040708     C                             + ' ' + 'VAOKSC'
147800030721     C                   ENDIF
147900030721     C                   ENDIF
148000040708     C*** POR
148100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
148200030721     C                   EVAL      PiStr=SkSplitCSV(i)
148300030721     C                   EXSR      CHKNUM
148400030721     C                   IF        PiInt=*on    AND
148500030721     C                             PiVal<=999   AND
148600030721     C                             PiVal>*zeros
148700040708     C                   Z-ADD     PiVal         VAOPOR
148800030721     C                   ELSE
148900030721     C                   SETON                                        32
149000040708     C                   EVAL      VAOPOR = *zeros
149100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
149200040708     C                             + ' ' + 'VAOPOR'
149300030721     C                   ENDIF
149400030721     C                   ENDIF
149500040708     C*** POC
149600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
149700040708     C                   EVAL      PiStr=SkSplitCSV(i)
149800040708     C                   EXSR      CHKNUM
149900040708     C                   IF        PiInt=*on    AND
150000040708     C                             PiVal<=999   AND
150100040708     C                             PiVal>*zeros
150200040708     C                   Z-ADD     PiVal         VAOPOC
150300040708     C                   ELSE
150400040708     C                   SETON                                        32
150500040708     C                   EVAL      VAOPOC = *zeros
150600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
150700040708     C                             + ' ' + 'VAOPOC'
150800040708     C                   ENDIF
150900040708     C                   ENDIF
151000040708     C*** STO
151100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
151200040708     C                   EVAL      PiStr=SkSplitCSV(i)
151300040708     C                   EXSR      CHKNUM
151400040708     C                   IF        PiInt=*on
151500040708     C                   Z-ADD     PiVal         VAOSTO
151600040708     C                   ELSE
151700040708     C                   SETON                                        32
151800040708     C                   EVAL      VAOSTO = *zeros
151900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
152000040708     C                             + ' ' + 'VAOSTO'
152100040708     C                   ENDIF
152200040708     C                   ENDIF
152300090428     C*
152400091103     C********** GESTIONE CAMPI PERSONALIZZATI
152500120120
152600121207     C*** RER
152700121207     C                   EVAL      VAORER = VAORSR
152800121207
152900121207     C*** RFA
153000121207     C* 1 char
153100121207     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAORFA_1'
153200121207     C                   EVAL      wRFA1=%char(%dec(%trim(SkSplitCSV(i)):1:0))
153300121207     C                   ENDIF
153400121207     C* 7 char
153500121207     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAORFA_2'
153600121207     C                   EVAL      wRFA2=%trim(SkSplitCSV(i))
153700121207     C                   ENDIF
153800121207     C* 3 char
153900121207     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAORFA_3'
154000121207     C                   EVAL      wRFA3=%editc(%dec(%trim(SkSplitCSV(i)):3:0)
154100121207     C                                   :'X')
154200121207     C                   ENDIF
154300121207     C* 2 char
154400121207     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAORFA_4'
154500121207     C                   EVAL      wRFA4=%editc(%dec(%trim(SkSplitCSV(i)):2:0)
154600121207     C                                   :'X')
154700121207     C                   ENDIF
154800150629     C* Cambiano gestione
154900150629     C***
155000150629     C**Note 1
155100150629     C***                IF        %trim(SkSplitFLD(i)) = 'W_VAONO1'
155200150629     C***                EVAL      w70 = %trim(SkSplitCSV(i))
155300150629     C***                EVAL      VAONO1=%subst(w70 : 1 :35)
155400150629     C***                ENDIF
155500150629     C**Note 2
155600150629     C***                IF        %trim(SkSplitFLD(i)) = 'W_VAONO2'
155700150629     C***                EVAL      wVAONO2 = %trim(SkSplitCSV(i))
155800150629     C***                ENDIF
155900150629     C* Note 1
156000150630     C                   IF        %trim(SkSplitFLD(i)) = 'W_SMSRTER'
156100150629     C                   EVAL      w70 = %trim(SkSplitCSV(i))
156200150629     C                   EVAL      w_VAOSMSR=%subst(w70 : 1 :35)
156300150629     C                   EVAL      VAOTER=%subst(w70 : 1 :35)
156400150629     C                   ENDIF
156500150629     C* Note 2
156600150629     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAONO2_1'
156700150629     C                   EVAL      wVAONO2_1 = %trim(SkSplitCSV(i))
156800150629     C                   ENDIF
156900150629     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAONO2_2'
157000150629     C                   EVAL      wVAONO2_2 = %trim(SkSplitCSV(i))
157100150629     C                   ENDIF
157200090428     C*
157300090428     C****************************************
157400090428     C*
157500030715     C*
157600030715     C                   ENDIF
157700030715     C                   EVAL      i = i + 1
157800030715     C                   ENDDO
157900040723     C*
158000121207     C***  ===>  Gestione campi "particolari" fuori ciclo
158100121207     C*
158200121207     C* VAORFA va valorizzato concatenando più colonne
158300121207     C                   EVAL      VAORFA='RT' + wRFA1 + wRFA2 + wRFA3 + wRFA4
158400121207     C*
158500140401     C* se non passati nei parametri, CRC e KSC vengono presi da COR
158600140401     C                   IF        VAOCRC = *zeros
158700140401     C                   EVAL      VAOCRC = VAOCOR
158800140401     C                   ENDIF
158900140401     C                   IF        VAOKSC = *zeros
159000140401     C                   EVAL      VAOKSC = VAOCOR/1000
159100140401     C                   ENDIF
159200141014     C*
159300141014     C* se VAOTER non è valorizzato, lo valorizzo con VAOSMSR
159400141014     C                   IF        VAOTER = *blank
159500141014     C                   EVAL      VAOTER = w_VAOSMSR
159600141014     C                   ENDIF
159700150420     C*
159800150629     C* VAONO2 è la concatenazione delle colonne wVAONO2_1 e wVAONO2_2
159900150420     C                   IF        VAONO2 = *blank
160000150629     C                   EVAL      VAONO2 = %trim(wVAONO2_1) + ' '  +
160100150629     C                                      %trim(wVAONO2_2)
160200150420     C                   ENDIF
160300140401     C*
160400030715     C                   ENDIF
160500020204     C*
160600010607     C                   ENDSR
160700010607     C*----------------------------------------------------*
160800080702
160900080702
161000080702     C*----------------------------------------------------*
161100080702     C*  CONTROLLO NUMERICITA' CAMPI
161200080702     C*----------------------------------------------------*
161300080702     C     CHKNUM        BEGSR
161400080702     C*
161500080702     C                   IF        PiDecChr = *blanks
161600080702     C                   EVAL      PiDecChr = ','
161700080702     C                   ENDIF
161800080702     C*
161900080702     C                   callp     UBISNUM_Check(PiStr
162000080702     C                                          :PiDecChr
162100080702     C                                          :PiVal
162200080702     C                                          :PiNum
162300080702     C                                          :PiInt)
162400080702     C*
162500080702     C                   ENDSR
162600080702     C***
162700080702
162800010601
162900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
163000010601     C     repfil        BEGSR
163100010601     C*
163200010601     C                   if        invfil = *zeros and
163300010601     C                             depfil > *zeros and
163400010629     C                             (vinflg = *blanks or
163500010629     C                              vinflg = *zeros)
163600010601     C
163700010601     C                   eval      invfil = depfil
163800010601     C                   endif
163900010601     C*
164000010601     C                   if        depfil <> invfil and
164100010601     C                             invfil > *zeros
164200010601     C                   eval      flgMulti = '1'
164300010601     C                   if        vinflg = *blanks
164400010601     C                   add       1             cntNonEl
164500010601     C                   endif
164600010601     C                   endif
164700010601     C*
164800010601     C                   if        vinflg = '2'
164900010601     C                   eval      flgStato = '2'
165000010601     C                   endif
165100010601     C*
165200010601     C                   ENDSR
165300010601     C***
165400010330
165500010330
165600990920      /TITLE Invio dei dati al punto operativo.
165700000613     C     invio         BEGSR
165800990920     C*
165900990920     C                   reset                   dscmz
166000010601     C                   move      invfil        cmzdst
166100990920     C                   eval      cmzfld = vlrfou
166200990920     C                   eval      cmzmbd = vlrhdl
166300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
166400000710     C                   if        prmfir = *blanks
166500040708     C                   eval      cmzfla = 'FNVAO00F'
166600040708     C                   eval      cmzmba = 'FNVAO00F'
166700000710     C                   else
166800000710     C                   eval      cmzfla = prmfir
166900000710     C                   eval      cmzmba = prmfir
167000000710     C                   endif
167100990920     C                   eval      cmznrr = *zeros
167200990920     C                   move      §ctrok        cmznrr
167300021018     C                   eval      cmzlba = vlrfl1
167400990920     C                   call(e)   'TIS711C'
167500990920     C                   parm                    dscmz
167600990921     C                   parm      *blanks       esito
167700990920     C                   if        %error
167800990920     C                             or cmzerr = '1'
167900990921     C                             or esito  = '1'
168000000710     C                   eval      wrkesito = '3'
168100990920     C                   endif
168200990920     C*
168300000613     C                   ENDSR
168400010601
168500010601
168600010601      /TITLE Invio dei dati al punto operativo.
168700010601     C     opeini        BEGSR
168800010601     C*
168900010601     C* Inizializzo flag e contatori operativi
169000010601     C                   movel     '0'           flgGiro           1
169100010601     C                   movel     '0'           flgMulti          1
169200010601     C                   movel     '1'           flgStato          1
169300010615     C                   movel     '0'           flgOk             1
169400010601     C                   z-add     *zeros        cntNonEl         10 0
169500010601     C                   z-add     *zeros        depfil            3 0
169600010601     C                   z-add     *zeros        invfil            3 0
169700010601     C*
169800010601     C                   ENDSR
169900010601     C***
170000050421
170100050421
170200091130     C     *pssr         BEGSR
170300050421     C*
170400091130     C                   eval      wrkesito = '2'
170500050421     C*
170600091130     C                   ENDSR     '*CANCL'
170700050421     C***
170800060306
170900060306
171000060306     C*-------------------------------------------------------------------------*
171100060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
171200060306     C*-------------------------------------------------------------------------*
171300060306     C     sr_Control    begsr
171400060306     C*
171500060306     C* inizializzo a ok il flag d controllo abilitazione
171600060306     C                   eval      Cli_Abil = *on
171700060306     C
171800060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
171900060306     C                   clear                   woriIDC
172000060306     C                   eval      kteCOD = 'VAP'
172100060306     C                   eval      kteKE1 = oriIDC
172200060306     C     keytbe        chain     TNTBE01L
172300060306     C                   if        %found(Tntbe01l)
172400060306     C                   eval      woriIDC = TbeKe2
172500060306     C                   else
172600060306     C                   eval      Cli_Abil = *off
172700060306     C                   endif
172800060306     C*
172900060306     C                   endsr
173000010330
173100010330
173200000613     C     *inzsr        BEGSR
173300990910     C*
173400990910     C     *entry        plist
173500990920     C                   parm                    tivlrds
173600990921     C                   parm      wrkesito      esito
173700000724     C                   parm                    prmlit
173800000710     C                   parm                    prmfir
173900010330     C*
174000010330     C* CALCOLA LA DATA CORRENTE
174100091127     C                   z-add     *zeros        datcor            8 0
174200091127     C                   time                    wn14             14 0
174300091127     C                   movel     wn14          wn6               6 0          *ORA (6)
174400091127     C                   eval      datcor = %dec(%date() : *ISO)
174500040723     C*
174600040723     C* Campi riferito al database
174700040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
174800040723     C     *like         define    tbeke1        kteKE1
174900040723     C*
175000040723     C* Definizione chiavi di lettura
175100040723     C* TNTBE01L - parziale
175200040723     C     keytbe        klist
175300040723     C                   kfld                    kteCOD                         *UTENTE
175400040723     C                   kfld                    kteKE1                         *CODICE
175500000613     C*
175600000613     C                   ENDSR
175700000613     C***
175800990908
175900040709** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
176000121207X;W_VAORFA_1;X;VAORSR;VAOINR;VAOLOR;VAOPRR;VAOCAR;
176100150629W_VAORFA_2;W_VAORFA_3;W_VAORFA_4;X;X;X;X;X;X;X;X;W_SMSRTER;
176200150629W_VAONO2_1;W_VAONO2_2;VAOEMLR;W_VAONO1;
