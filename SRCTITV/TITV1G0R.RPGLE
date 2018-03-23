000100040708      /TITLE Upload via Internet: traduzione in FNVAOWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500040723     Ffnacr01l  if   e           k disk
000600090617     Ftntbe01l  if   e           k disk
000700990910     Ftivin00r  uF   E             DISK    usropn
000800040708     FFnvaowwr  O    E             DISK    usropn
000900040723     Ftiori00f  O    E             DISK
001000050826     FTIVGD00F  O    E             DISK
001100030715     Ftitvxlp   O    f  132        PRINTER usropn
001200000621     F                                     oflind(*inoa)
001300030715     Ftitvxlps  O    f  198        PRINTER usropn
001400000621     F                                     oflind(*inob)
001500000313     D*
001600000613     D*------------
001700000613     D* COMANDI
001800000613     D*------------
001900011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
002000010330     D*----------------------------------------------------
002100010330     D* DICHIARAZIOINE VARIABILI DI WRK
002200010330     D*----------------------------------------------------
002300010330     D dscmz         e ds                  inz
002400010330     D psds           sds
002500010330     D  procname         *PROC
002600010330     D tivlrds       e ds                  extname(tivlr00f)
002700050826     D fnvapds       e ds                  extname(fnvap00f)
002800040723     D dorm01        e ds
002900040723     D dfar          e ds
003000090617     d trul33ds      e ds
003100090617     d kpjba         e ds
003200010330     D esito           s              1
003300010330     D prmlit          s             10
003400010330     D prmfir          s             10
003500010330     D wrkesito        s                   like(esito)
003600010330     D wrkdata         s               d
003700010330     D wrkora          s               t
003800010330     D rrnum           s              6  0 INZ(*zeros)
003900010330     D recko           s            150    INZ(*blanks)
004000011113     D depcmd          s            150    INZ(*blanks)
004100060306     D Cli_Abil        s              1    inz(*off)
004200060306     D woriIDC         s                   like(oriIDC) inz(*blanks)
004300090617     d §numori         s              7  0 inz
004400030715     D*------------------
004500030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004600030715     D*------------------
004700030715     D SkSplitFLD      S             10    DIM(100)
004800030715     D SkSplitCSV      S            256    DIM(100)
004900030715     D CharCSV         S              1
005000030715     D CharTXT         S              1
005100030715     D CharNUM         S              1
005200030715     D posDa           S              3  0 INZ(*zeros)
005300030715     D posA            S              3  0 INZ(*zeros)
005400030715     D i               s              3  0 INZ(1)
005500030715     D wGiro           s              1  0 INZ(*zeros)
005600030822     D*------------------
005700030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005800030822     D*------------------
005900030822     D posDaDft        S              3  0 INZ(*zeros)
006000030822     D posADft         S              3  0 INZ(*zeros)
006100030822     D j               s              3  0 INZ(1)
006200030822     D wGiroDft        s              1  0 INZ(*zeros)
006300010330     D*------------------
006400010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
006500010330     D*------------------
006600010330     D WLBDA8          DS                  INZ
006700010330     D  G08DAT                 1      8  0
006800010330     D  G08INV                 9     16  0
006900010330     D  G08ERR                17     17
007000010330     D  G08TGI                18     22  0
007100031201     D*------------------
007200031201     D* Costanti
007300031201     D*------------------
007400031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007500031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007600010330
007700091026
007800091026     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
007900091026     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
008000091026
008100010330
008200010330
008300990915     C                   time                    wrkdata
008400990915     C                   time                    wrkora
008500000913     C                   reset                   rrnum
008600990921     C                   reset                   esito
008700990921     C                   reset                   wrkesito
008800010601     C*
008900010601     C                   exsr      opeini
009000040708     C                   exsr      rwvao
009100010601     C*
009200010601     C                   seton                                        lr
009300010601
009400010601
009500010601
009600010601
009700010601     C*--------------------------------------------------------
009800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009900010601     C*--------------------------------------------------------
010000010601     C     PREELA        BEGSR
010100010601     C*
010200010601     C* SE OCCORRE SPEDIRE IN FILIALE
010300010601     C                   if        invfil <> *zeros and
010400010601     C                             flgGiro = '0'
010500010601     C*
010600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010700010601     C                   eval      flgGiro = '1'
010800000724     C*
010900000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
011000000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011100010601     C     invfil        chain     azorg01l
011200000724     C                   if        %found
011300000724     C                   movel(p)  CMD(1)        depcmd
011400020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
011500000724     C*
011600000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
011700011113     C                   Z-ADD     150           LENGH            15 5
011800000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011900000724     C                   PARM                    depcmd
012000000724     C                   PARM                    LENGH
012100000724     C*
012200000724     C                   endif
012300000616     C*
012400000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012500000616     C                   movel(p)  CMD(2)        depcmd
012600000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
012700000616     C*
012800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
012900011113     C                   Z-ADD     150           LENGH            15 5
013000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013100000616     C                   PARM                    depcmd
013200000616     C                   PARM                    LENGH
013300000613     C*
013400030715     C                   if        not %open(titvxlps)
013500030715     C                   open      titvxlps
013600030715     C                   except    testdett
013700000613     C                   endif
013800010601     C*
013900010601     C                   endif
014000010601     C*
014100010601     C                   ENDSR
014200010601     C***
014300010601
014400010601
014500010601
014600010601
014700010601
014800010601
014900010601
015000010601     C*--------------------------------------------------------
015100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
015200010601     C*--------------------------------------------------------
015300010601     C     ENDELA        BEGSR
015400990915     C*
015500000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
015600000613     C*
015700030715     C                   if        %open(titvxlps)
015800000616     C                   except    findett
015900030715     C                   close     titvxlps
016000000613     C                   endif
016100000616     C*
016200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
016300011113     C                   Z-ADD     150           LENGH            15 5
016400010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
016500000616     C                   PARM                    CMD(3)
016600000616     C                   PARM                    LENGH
016700000616     C*
016800010601     C                   ENDSR
016900010601     C***
017000010601
017100010601
017200010601
017300010601
017400000613
017500000613     C*--------------------------------------------------------
017600000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
017700000613     C*--------------------------------------------------------
017800000621     C     STPR          BEGSR
017900000613     C*
018000030715     C                   if        not %open(titvxlp)
018100030715     C   50              open      titvxlp
018200990915     C                   endif
018300990915     C*
018400030715     C   50              except    riepilogo
018500990915     C*
018600030715     C                   if        %open(titvxlp)
018700030715     C   50              close     titvxlp
018800990914     C                   endif
018900000613     C*
019000000613     C                   ENDSR
019100000613     C***
019200000613
019300010330
019400010601
019500010601
019600010601
019700010330     C*--------------------------------------------------------
019800040708     C* RWVAO   LEGGE TIVIN00R E SCRIVE FNVAOWWF              *
019900010330     C*--------------------------------------------------------
020000040708     C     RWVAO         BEGSR
020100010330     C*
020200010330     C                   if        not %open(tivin00r)
020300010330     C                   open      tivin00r
020400010330     C                   endif
020500040708     C                   if        not %open(fnvaowwr)
020600040708     C                   open      fnvaowwr
020700010330     C                   endif
020800010330     C*
020900010604     C                   clear                   §CTROK
021000010604     C                   clear                   §CTRMO
021100010604     C                   clear                   §CTRNO
021200010330     C*
021300030822     C                   do        *HIVAL
021400010330     C*
021500030822     C                   read      tivin00r                               70
021600010618     C*
021700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
021800010618     C                   if        vinflg = '1'
021900010618     C                   eval      flgOk = '1'
022000010618     C                   endif
022100010618     C*
022200040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
022300040510     C                   z-add     *zeros        wLenVINDTA        4 0
022400040510     C                   z-add     *zeros        wFlgVINDTA        1 0
022500040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
022600040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
022700040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
022800040510     C                   z-add     1             wFlgVINDTA
022900040510     C                   leave
023000040510     C                   endif
023100040510     C                   enddo
023200040510     C*
023300040218     C                   if        vindta > *blanks AND
023400040510     C                             wFlgVINDTA = 1
023500010330     C                   add       1             rrnum
023600010330     C*
023700010601     C                   if        *in70 = *off and
023800010330     C                             (vinflg = *blanks
023900010330     C                              or vinflg = '0'
024000010330     C                              or vinflg = '2')
024100010330     C*
024200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
024300010711     C                   if        vinflg = *blanks or vinflg = '0'
024400010711     C                   clear                   vinmsg
024500010711     C                   endif
024600010601     C*
024700010330     C                   exsr      inzvar
024800040708     C                   exsr      impvao
024900010601     C*
025000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
025100040708     C                   eval      depfil = VAOPOE
025200010601     C                   exsr      repfil
025300010601     C                   if        depfil = invfil
025400021025     C                   if        vlrpoi = 999
025500040708     C                   move(P)   invfil        VAOPOE
025600021025     C                   else
025700040708     C                   move(P)   vlrpoi        VAOPOE
025800021025     C                   endif
025900050113     C                   endif
026000010601     C*
026100010601     C                   exsr      PREELA
026200010601     C*
026300010604     C* Ebbene...
026400010604     C*
026500030822     C  N31              add       1             §CTROK            7 0
026600030822     C   32              add       1             §CTRMO            7 0
026700030822     C   31              add       1             §CTRNO            7 0
026800030822     C                   if        wGiro = 2
026900040723     C  N31              EXSR      CHKCOR
027000040723     C  N31              EXSR      WRIORI
027100050826     C  N31              EXSR      WRIVGD
027200040708     C  N31              write     FNVAO000
027300030822     C                   endif
027400010604     C*
027500010604     C                   if        *in31 = *off and
027600010604     C                             *in32 = *off
027700010604     C                   eval      vinflg = '1'
027800010604     C                   else
027900010604     C                   eval      recko = vindta
028000010604     C                   except    rigadett
028100010604     C                   eval      vinflg = '2'
028200010604     C                   endif
028300010601     C*
028400010604     C                   endif
028500010604     C*
028600010330     C                   else
028700010330     C                   eval      vinflg = '1'
028800010330     C                   endif
028900010601     C*
029000010601     C  N70              update    tivin000
029100010330     C*
029200030822     C  N70              enddo
029300010601     C
029400010601     C*
029500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
029600010601     C                   if        cntNonEl = *zeros or
029700010601     C                             flgMulti = '0'
029800010330     C* Se non ci sono record con errori ...
029900010601     C                   if        §ctrno = 0 and
030000010604     C                             §ctrmo = 0 and
030100010601     C                             flgStato <> '2'
030200010330     C* ... restituisco esito OK.
030300010330     C                   eval      wrkesito = '0'
030400010330     C                   else
030500010330     C                   if        §ctrok > 0
030600010330     C                   eval      wrkesito = '1'
030700010330     C                   else
030800010615     C                   if        flgOk = '0'
030900010615     C                   eval      wrkesito = '2'
031000010615     C                   else
031100010615     C                   eval      wrkesito = '6'
031200010615     C                   endif
031300010330     C                   endif
031400010330     C                   endif
031500010601     C                   else
031600010601     C                   eval      wrkesito = '9'
031700010601     C                   endif
031800010330     C*
031900010330     C                   if        %open(tivin00r)
032000010330     C                   close     tivin00r
032100010330     C                   endif
032200040708     C                   if        %open(fnvaowwr)
032300040708     C                   close     fnvaowwr
032400010330     C                   endif
032500010601     C*
032600010601     C                   if        vlrpoi <> 999
032700010601     C                   eval      invfil = vlrpoi
032800010601     C                   endif
032900010330     C*
033000010330     C                   if        §ctrok > 0
033100010601     C                             and invfil > *zeros
033200010330     C                   exsr      invio
033300010330     C                   endif
033400010601     C*
033500010618     C                   if        flgGiro = '1'
033600010601     C                   exsr      endela
033700010618     C                   endif
033800010330     C*
033900010330     C                   ENDSR
034000010330     C***
034100050323
034200050323
034300050323     C*------------------------------------------------------------------------*
034400050323     C* CHKCOR - Considerazioni su VAOCOR
034500050323     C*------------------------------------------------------------------------*
034600050323     C     CHKCOR        BEGSR
034700050323     C*
034800050323     C* Verifico la provenienza dei dati: se da file del cliente oppure da immissione via Internet
034900050323     C                   if        vaoTCO = 'F'
035000050323     C* Quindi verifico se il codice ordinante non è valorizzato
035100050323     C                   if        vaoCOR = *zeros
035200050323     C*                  if        vaoCOR = *zeros  AND
035300050323     C*                            vaoRSO = *blanks AND
035400050323     C*                            vaoINO = *blanks AND
035500050323     C*                            vaoCAO = *blanks AND
035600050323     C*                            vaoLOO = *blanks AND
035700050323     C*                            vaoPRO = *blanks AND
035800050323     C*                            vaoNAO = *blanks
035900050323     C* Compongo la chiave x agganciare l'anagrafico clienti ritiro
036000050323     C                   move      vlrKSC        wCli              7 0
036100050323     C                   movel(p)  wCli          acrCRO
036200050323     C     acrCRO        setll     fnacr01l
036300050323     C                   if        %found(fnacr01l)
036400050323     C                   read      fnacr01l
036500050323     C                   dow       not %eof(fnacr01l)
036600050323     C                   movel     acrCRO        wCliAcr           7 0
036700050323     C                   if        wCliAcr = wCli
036800050323     C                   eval      vaoCOR = acrCRO
036900050323     C                   leave
037000050323     C                   else
037100050323     C                   leave
037200050323     C                   endif
037300050323     C                   read      fnacr01l
037400050323     C                   enddo
037500050323     C                   endif
037600050323     C                   endif
037700050323     C                   endif
037800050323     C*
037900050323     C                   ENDSR
038000040723
038100040723
038200040723
038300040723     C*------------------------------------------------------------------------*
038400040723     C* WRIORI - Reperimento informazioni necessarie e scrittura del file spia TIORI00F
038500040723     C*------------------------------------------------------------------------*
038600040723     C     WRIORI        BEGSR
038700040723     C*
038800090617     C* Come prima cosa stacco un numeratore da AZNUM
038900090617     C                   clear                   trul33ds
039000090617     C                   eval      I33OPE = *zeros
039100090617     C                   eval      I33CNU = 600
039200090617     C                   eval      I33NUM = 1
039300090617     C                   movel     TRUL33DS      KPJBU
039400090617     C                   call      'TRUL33R'
039500090617     C                   parm                    KPJBA
039600090617     C                   movel     KPJBU         TRUL33DS
039700090617     C                   if        O33ERR = *zeros
039800090617     C                   z-add     O33NRF        §numori
039900090618     c                   else
040000090618     c                   eval      §numori = *all'9'
040100090617     C                   endif
040200040723     C*
040300040723     C* Quindi imposto i campi "extra-VAO"
040400040723     C                   if        vlrKSC = '0ORM0000'
040500040723     C*
040600040723     C                   if        vaoCOR > *zeros
040700040723     C                   movel     vaoCOR        wrkCOR7           7 0
040800040723     C*
040900040723     C* Controlla se il codice ordinante è un figlio , se sì devo valorizzare l'ORIIDC con il
041000040723     C* codice padre. Se è un padre assegno già il codice ordinante.
041100040723     C                   clear                   tibs10ds
041200040723     C                   eval      d10drf = datcor                              *data riferimento
041300040723     C                   eval      d10paf = 'P'                                 *cerca il padre
041400040723     C                   eval      d10cod = wrkCOR7                             *cod cliente da tst
041500091026     C*
041600091026     C* Preparazione chiamata al TIBS10R1
041700091026     C                   clear                   tibs10r1ds
041800091026     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
041900091026     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
042000091026     C                   eval      tibs10r1ds.d10igrptle = 'W'
042100091026     C*
042200091026     C                   call      'TIBS10R1'
042300091026     C                   parm                    tibs10r1ds
042400091026     C*
042500091026     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
042600040723     C*
042700040723     C                   if        d10err <> *blanks or                         *No unif. o padre
042800040723     C                             d10cop =  *zeros  or
042900040723     C                             d10cop =  d10cod
043000040723     C                   move      d10cod        oriIDC
043100040723     C                   else                                                   *Figlio
043200040723     C                   move      d10cop        oriIDC
043300040723     C                   endif
043400040723     C*
043500040723     C                   else
043600040723     C                   eval      oriIDC = *zeros
043700040723     C                   endif
043800040723     C*
043900040723     C                   else
044000040723     C                   eval      oriIDC = vlrKSC
044100040723     C                   endif
044200040723     C*
044300040723     C                   eval      oriPRG = §numori
044400040723     C                   eval      oriDER = datcor
044500040723     C                   eval      oriOER = wn6
044600040723     C                   eval      oriPOE = vaoPOE
044700040723     C                   eval      oriNSR = vaoNSR
044800040723     C                   eval      oriNOR = vaoNOR
044900040723     C                   eval      oriNRV = vaoNRV
045000040723     C                   eval      oriFLO = *blanks
045100040723     C*
045200040723     C* Infine valorizzo la chiave esterna sul file FNVAO
045300040723     C                   eval      dorm01 = vaoFLO
045400040723     C                   movel     §numori       §ormpg
045500040723     C                   eval      vaoFLO = dorm01
045600040723     C*
045700040723     C* ...quindi scrivo il file TIORI00F
045800040723     C                   WRITE     TIORI000
045900040723     C*
046000040723     C                   ENDSR
046100050826
046200050826
046300050826
046400050826      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
046500050826     C     wriVGD        BEGSR
046600050826     C*
046700050826     C* Reperisco la descrizione della fase da tabella
046800050826     C                   movel(p)  'FAR'         KteCOD
046900050826     C                   movel(p)  '000'         KteKE1
047000050826     C     KEYtbe        chain     tntbe01l
047100050826     C                   if        %found(tntbe01l)
047200050826     C                   movel(P)  tbeuni        DFAR
047300050826     C                   else
047400050826     C                   clear                   DFAR
047500050826     C                   endif
047600050826     C*
047700050826     C* Valorizzo buffer record
047800050826     C                   clear                   FNVAPDS
047900050826     C                   eval      vapIDC = oriIDC
048000050826     C                   eval      vapPOE = oriPOE
048100050826     C                   eval      vapRFA = vaoRFA
048200050826     C                   eval      vapPOG = oriPOE
048300050826     C                   eval      vapDAE = oriDER
048400050826     C                   eval      vapORE = oriOER
048500050826     C                   if        d§fardva <> *blanks
048600050826     C                   eval      vapDFA = d§fardva
048700050826     C                   else
048800050826     C                   eval      vapDFA = d§fardes
048900050826     C                   endif
049000050826     C*
049100050826     C                   clear                   tivgd000
049200080404     C                   eval      vgdDTA = %subst(FNVAPDS:1:%size(FNVAPDS))
049300050826     C                   eval      vgdTIP = 'VP'
049400050826     C                   eval      vgdKSU = vapIDC
049500050826     C                   eval      vgdTSC = 'WW'
049600050826     C                   eval      vgdDAT = datcor
049700050826     C                   eval      vgdPGM = 'TITVOXLR'
049800060306     C* ...quindi scrivo il formato file FNVAP ma solo se cliente abilitato in tabella 'VAP'
049900060306     C                   exsr      sr_Control
050000060306     C                   if        Cli_Abil = *on
050100060306     C                   eval      vgdKSU = woriIDC
050200050826     C                   write     tivgd000
050300060306     C                   endif
050400050826     C*
050500050826     C                   ENDSR
050600050826     C*------------------------------------------------------------------------*
050700010601
050800010601
050900010601
051000010330     C*----------------------------------------------------*
051100030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
051200010330     C*----------------------------------------------------*
051300010330     C     INZVAR        BEGSR
051400010330     C*
051500030822     C* Inizializzo variabili di wrk
051600020204     C                   Z-ADD     *zeros        Num5_0            5 0
051700030822     C*
051800030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
051900040708     C                   CLEAR                   FNVAO000
052000040723     C                   CLEAR                   TIORI000
052100030822     C                   CLEAR                   SkSplitCSV
052200030822     C                   EVAL      i = 1
052300030822     C                   EVAL      posDa = *zeros
052400030822     C                   EVAL      posA  = *zeros
052500030822     C*
052600030822     C* Reimposto i valori di default
052700030822     C                   EXSR      DEFCAM
052800030822     C*
052900010330     C                   ENDSR
053000010330     C*----------------------------------------------------*
053100030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
053200010330     C*----------------------------------------------------*
053300010330     C     DEFCAM        BEGSR
053400030822     C*
053500030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
053600030715     C* e delimitatore testo.
053700030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
053800030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
053900030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
054000030822     C*
054100030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
054200030715     C                   IF        %subst(vlrppt:1:1) = 'S'
054300030715     C                   SETON                                        50
054400030715     C                   ELSE
054500030715     C                   SETOFF                                       50
054600030715     C                   ENDIF
054700030822     C*
054800030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
054900030822     C                   EVAL      posDaDft = 1
055000030822     C                   EVAL      posADft  = 0
055100030822     C                   EVAL      wGiroDft = 0
055200030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
055300030822     C                             posDaDft > 0
055400030822     C*
055500030822     C* Gestisco il 1° giro
055600030822     C                   IF        wGiroDft = 0
055700030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
055800030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
055900030822     C* Incremento il contatore dei "giri"
056000030822     C                   EVAL      wGiroDft = 1
056100030822     C                   ELSE
056200030822     C                   EVAL      posDaDft = posADft
056300030822     C                   ENDIF
056400030822     C* Eseguo lo scan x trovare la fine del campo corrente
056500030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
056600030822     C*
056700030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
056800030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
056900030822     C                   IF        posDaDft > 0 AND
057000030822     C                             posADft  > 0
057100040708     C* POE
057200040708     C                   IF        %subst(
057300040708     C                             %subst(vlrppt:posDaDft+1:
057400040708     C                             posADft-posDaDft-1):1:3)
057500040708     C                             = 'POE'
057600040708     C                   EVAL      PiStr=%trim(%subst(
057700040708     C                             %subst(vlrppt:posDaDft+1:
057800040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057900040708     C                   EXSR      CHKNUM
058000040708     C                   IF        PiInt=*on
058100040708     C                   Z-ADD     PiVal         VAOPOE
058200040708     C                   ENDIF
058300040708     C                   ENDIF
058400040708     C* TOR
058500040708     C                   IF        %subst(
058600040708     C                             %subst(vlrppt:posDaDft+1:
058700040708     C                             posADft-posDaDft-1):1:3)
058800040708     C                             = 'TOR'
058900040708     C                   EVAL      VAOTOR=%trim(%subst(
059000040708     C                             %subst(vlrppt:posDaDft+1:
059100040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059200040708     C                   ENDIF
059300040708     C* TCO
059400040708     C                   IF        %subst(
059500040708     C                             %subst(vlrppt:posDaDft+1:
059600040708     C                             posADft-posDaDft-1):1:3)
059700040708     C                             = 'TCO'
059800040708     C                   EVAL      VAOTCO=%trim(%subst(
059900040708     C                             %subst(vlrppt:posDaDft+1:
060000040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060100040708     C                   ENDIF
060200040707     C* COR
060300030822     C                   IF        %subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):1:3)
060600040707     C                             = 'COR'
060700030822     C                   EVAL      PiStr=%trim(%subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061000030822     C                   EXSR      CHKNUM
061100030822     C                   IF        PiInt=*on
061200040708     C                   Z-ADD     PiVal         VAOCOR
061300030822     C                   ENDIF
061400030822     C                   ENDIF
061500040708     C* CRC
061600040708     C                   IF        %subst(
061700040708     C                             %subst(vlrppt:posDaDft+1:
061800040708     C                             posADft-posDaDft-1):1:3)
061900040708     C                             = 'CRC'
062000040708     C                   EVAL      PiStr=%trim(%subst(
062100040708     C                             %subst(vlrppt:posDaDft+1:
062200040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062300040708     C                   EXSR      CHKNUM
062400040708     C                   IF        PiInt=*on
062500040708     C                   Z-ADD     PiVal         VAOCRC
062600040708     C                   ENDIF
062700040708     C                   ENDIF
062800040708     C* FFD
062900040708     C                   IF        %subst(
063000040708     C                             %subst(vlrppt:posDaDft+1:
063100040708     C                             posADft-posDaDft-1):1:3)
063200040708     C                             = 'FFD'
063300040708     C                   EVAL      VAOFFD=%trim(%subst(
063400040708     C                             %subst(vlrppt:posDaDft+1:
063500040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063600040708     C                   ENDIF
063700040708     C* ORR
063800040708     C                   IF        %subst(
063900040708     C                             %subst(vlrppt:posDaDft+1:
064000040708     C                             posADft-posDaDft-1):1:3)
064100040708     C                             = 'ORR'
064200040708     C                   EVAL      PiStr=%trim(%subst(
064300040708     C                             %subst(vlrppt:posDaDft+1:
064400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064500040708     C                   EXSR      CHKNUM
064600040708     C                   IF        PiInt=*on
064700040708     C                   Z-ADD     PiVal         VAOORR
064800040708     C                   ENDIF
064900040708     C                   ENDIF
065000040712     C* NCL
065100040712     C                   IF        %subst(
065200040712     C                             %subst(vlrppt:posDaDft+1:
065300040712     C                             posADft-posDaDft-1):1:3)
065400040712     C                             = 'NCL'
065500040712     C                   EVAL      PiStr=%trim(%subst(
065600040712     C                             %subst(vlrppt:posDaDft+1:
065700040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065800040712     C                   EXSR      CHKNUM
065900040712     C                   IF        PiInt=*on
066000040712     C                   Z-ADD     PiVal         VAONCL
066100040712     C                   ENDIF
066200040712     C                   ENDIF
066300040712     C* PKG
066400040712     C                   IF        %subst(
066500040712     C                             %subst(vlrppt:posDaDft+1:
066600040712     C                             posADft-posDaDft-1):1:3)
066700040712     C                             = 'PKG'
066800040712     C                   EVAL      PiStr=%trim(%subst(
066900040712     C                             %subst(vlrppt:posDaDft+1:
067000040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067100040712     C                   EXSR      CHKNUM
067200040712     C                   IF        PiNum=*on
067300040712     C                   Z-ADD     PiVal         VAOPKG
067400040712     C                   ENDIF
067500040712     C                   ENDIF
067600040712     C* VLM
067700040712     C                   IF        %subst(
067800040712     C                             %subst(vlrppt:posDaDft+1:
067900040712     C                             posADft-posDaDft-1):1:3)
068000040712     C                             = 'VLM'
068100040712     C                   EVAL      PiStr=%trim(%subst(
068200040712     C                             %subst(vlrppt:posDaDft+1:
068300040712     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068400040712     C                   EXSR      CHKNUM
068500040712     C                   IF        PiNum=*on
068600040712     C                   Z-ADD     PiVal         VAOVLM
068700040712     C                   ENDIF
068800040712     C                   ENDIF
068900040708     C* NAM
069000040708     C                   IF        %subst(
069100040708     C                             %subst(vlrppt:posDaDft+1:
069200040708     C                             posADft-posDaDft-1):1:3)
069300040708     C                             = 'NAM'
069400040708     C                   EVAL      VAONAM=%trim(%subst(
069500040708     C                             %subst(vlrppt:posDaDft+1:
069600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069700040708     C                   ENDIF
069800040708     C* PAG
069900030822     C                   IF        %subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):1:3)
070200040708     C                             = 'PAG'
070300040708     C                   EVAL      VAOPAG=%trim(%subst(
070400030822     C                             %subst(vlrppt:posDaDft+1:
070500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070600030822     C                   ENDIF
070700040708     C* KSC
070800040708     C                   IF        %subst(
070900040708     C                             %subst(vlrppt:posDaDft+1:
071000040708     C                             posADft-posDaDft-1):1:3)
071100040708     C                             = 'KSC'
071200040708     C                   EVAL      PiStr=%trim(%subst(
071300040708     C                             %subst(vlrppt:posDaDft+1:
071400040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071500040708     C                   EXSR      CHKNUM
071600040708     C                   IF        PiInt=*on
071700040708     C                   Z-ADD     PiVal         VAOKSC
071800040708     C                   ENDIF
071900040708     C                   ENDIF
072000040708     C* CTR
072100040708     C                   IF        %subst(
072200040708     C                             %subst(vlrppt:posDaDft+1:
072300040708     C                             posADft-posDaDft-1):1:3)
072400040708     C                             = 'CTR'
072500040708     C                   EVAL      VAOCTR=%trim(%subst(
072600040708     C                             %subst(vlrppt:posDaDft+1:
072700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072800040708     C                   ENDIF
072900040708     C* POC
073000040708     C                   IF        %subst(
073100040708     C                             %subst(vlrppt:posDaDft+1:
073200040708     C                             posADft-posDaDft-1):1:3)
073300040708     C                             = 'POC'
073400040708     C                   EVAL      PiStr=%trim(%subst(
073500040708     C                             %subst(vlrppt:posDaDft+1:
073600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073700040708     C                   EXSR      CHKNUM
073800040708     C                   IF        PiInt=*on
073900040708     C                   Z-ADD     PiVal         VAOPOC
074000040708     C                   ENDIF
074100040708     C                   ENDIF
074200040708     C* NO1
074300040708     C                   IF        %subst(
074400040708     C                             %subst(vlrppt:posDaDft+1:
074500040708     C                             posADft-posDaDft-1):1:3)
074600040708     C                             = 'NO1'
074700040708     C                   EVAL      VAONO1=%trim(%subst(
074800040708     C                             %subst(vlrppt:posDaDft+1:
074900040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075000040708     C                   ENDIF
075100040708     C* DDT
075200030822     C                   IF        %subst(
075300030822     C                             %subst(vlrppt:posDaDft+1:
075400030822     C                             posADft-posDaDft-1):1:3)
075500040708     C                             = 'DDT'
075600040708     C                   EVAL      VAODDT=%trim(%subst(
075700030822     C                             %subst(vlrppt:posDaDft+1:
075800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075900030822     C                   ENDIF
076000040708     C* RFA
076100040708     C                   IF        %subst(
076200040708     C                             %subst(vlrppt:posDaDft+1:
076300040708     C                             posADft-posDaDft-1):1:3)
076400040708     C                             = 'RFA'
076500040708     C                   EVAL      VAORFA=%trim(%subst(
076600040708     C                             %subst(vlrppt:posDaDft+1:
076700040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076800040708     C                   ENDIF
076900040708     C* FLO
077000040708     C                   IF        %subst(
077100040708     C                             %subst(vlrppt:posDaDft+1:
077200040708     C                             posADft-posDaDft-1):1:3)
077300040708     C                             = 'FLO'
077400040708     C                   EVAL      VAOFLO=%trim(%subst(
077500040708     C                             %subst(vlrppt:posDaDft+1:
077600040708     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077700040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
077800040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
077900040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
078000040708     C                   ENDIF
078100030822     C* ...
078200030822     C                   ENDIF
078300030822     C                   ENDDO
078400020204     C*
078500010330     C                   ENDSR
078600010607     C*----------------------------------------------------*
078700040708     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAO)
078800010607     C*----------------------------------------------------*
078900040708     C     IMPVAO        BEGSR
079000010607     C*
079100010607     C                   SETOFF                                       3132
079200030515     C*
079300040708     C                   MOVEL     datcor        VAODAO
079400040708     C                   MOVEL     wn14          VAOOAO
079500010607     C*
079600030715     C**********
079700030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
079800030715     C**********
079900030715     C                   IF        wGiro = *zeros
080000031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
080100031201     C     minu:maiu     XLATE     vindta        vindta
080200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
080300040324     C                   DOW       posDa <= %len(%trimr(vindta))
080400030715     C*
080500030715     C* Gestisco il 1° campo
080600030715     C                   IF        i = 1
080700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
080800030715     C                   EVAL      posDa = 1
080900030715     C* Eseguo lo scan x trovare la fine del primo campo
081000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
081100030715     C* A questo punto "estraggo" il campo corrente
081200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
081300030715     C                                                    (posA-posDa))
081400030715     C* X i campi successivi al 1°
081500030715     C                   ELSE
081600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
081700030715     C                   EVAL      posDa = posA + 1
081800030715     C* Eseguo lo scan x trovare la fine del campo corrente
081900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082000030715     C* Gestisco l'ultimo campo
082100030715     C                   IF        posA = *zeros
082200040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
082300030715     C* A questo punto "estraggo" il campo corrente
082400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082500030715     C                                                    (posA-posDa))
082600030715     C                   LEAVE
082700030715     C                   ELSE
082800030715     C* A questo punto "estraggo" il campo corrente
082900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083000030715     C                                                    (posA-posDa))
083100030715     C                   ENDIF
083200030715     C                   ENDIF
083300030715     C* Incremento il contatore d campo
083400030715     C                   EVAL      i = i +1
083500030715     C                   ENDDO
083600030715     C*
083700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
083800030715     C* delimitazione testo
083900030715     C                   EVAL      i = 1
084000030715     C                   DOW       i < %elem(SkSplitFLD)
084100030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
084200030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
084300030715     C                   EVAL      i = i + 1
084400030715     C                   ENDDO
084500030715     C                   EVAL      wGiro = 1
084600030715     C                   ELSE
084700030715     C                   EVAL      wGiro = 2
084800031204     C**********
084900031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
085000031204     C* d separatore campo
085100031204     C**********
085200040304     C                   IF        %subst(vindta:1:1) = CharCSV                 * all'inizio
085300040304     C                   EVAL      vindta = ' ' + vindta
085400040304     C                   ENDIF
085500040304     C***                DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
085600040304     C***                EVAL      vindta = %subst(vindta:2)
085700040304     C***                ENDDO
085800031204     C*
085900031204     C                   Z-ADD     *zeros        lunghInput        4 0
086000040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
086100040324     C                   IF        %subst(%trimr(vindta):lunghInput:1) = CharCSV
086200040304     C                   EVAL      vindta = vindta + ' '
086300040304     C                   ENDIF
086400040324     C***                DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
086500040324     C***                EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
086600040304     C***                EVAL      lunghInput = %len(%trim(vindta))
086700040304     C***                ENDDO
086800030715     C**********
086900030715     C* Effettuo lo split del campo dati d input
087000030715     C**********
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
088100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
088200030715     C                                                    (posA-posDa))
088300030715     C* X i campi successivi al 1°
088400030715     C                   ELSE
088500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
088600030715     C                   EVAL      posDa = posA + 1
088700031204     C* Verifico che nn vi sia il campo nn valorizzato
088800031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
088900031204     C* Se campo nn valorizzato skippo al prossimo
089000031204     C                   EVAL      SkSplitCSV(i) = *blanks
089100031204     C                   EVAL      posA  = posA + 1
089200031204     C                   ELSE
089300030715     C* Eseguo lo scan x trovare la fine del campo corrente
089400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089500030715     C* Gestisco l'ultimo campo
089600030715     C                   IF        posA = *zeros
089700040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
089800030715     C* A questo punto "estraggo" il campo corrente
089900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090000030715     C                                                    (posA-posDa))
090100030715     C                   LEAVE
090200030715     C                   ELSE
090300030715     C* A questo punto "estraggo" il campo corrente
090400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090500030715     C                                                    (posA-posDa))
090600030715     C                   ENDIF
090700031204     C                   ENDIF
090800030715     C                   ENDIF
090900030715     C* Incremento il contatore d campo
091000030715     C                   EVAL      i = i +1
091100030715     C                   ENDDO
091200030715     C*
091300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
091400030715     C* delimitazione testo
091500030715     C                   EVAL      i = 1
091600030715     C                   DOW       i < %elem(SkSplitCSV)
091700030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
091800030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
091900030715     C                   EVAL      i = i + 1
092000030715     C                   ENDDO
092100030715     C*
092200030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
092300030715     C                   EVAL      i = 1
092400030715     C                   DOW       i < %elem(SkSplitFLD)
092500030715     C*
092600030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
092700030715     C                   IF        SkSplitFLD(i) <> *blanks AND
092800030715     C                             SkSplitCSV(i) <> *blanks
092900030721     C*** ATB
093000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATB'
093100040708     C                   EVAL      VAOATB = SkSplitCSV(i)
093200030721     C                   ENDIF
093300040708     C*** TOR
093400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTOR'
093500040708     C                   EVAL      VAOTOR = SkSplitCSV(i)
093600040708     C                   ENDIF
093700040708     C*** TCO
093800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTCO'
093900040708     C                   EVAL      VAOTCO = SkSplitCSV(i)
094000040708     C                   ENDIF
094100040708     C*** RSO
094200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSO'
094300040708     C                   EVAL      VAORSO = SkSplitCSV(i)
094400030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
094500040708     C     '@':'A'       XLATE     VAORSO        VAORSO
094600030715     C* ==
094700030715     C                   ENDIF
094800040708     C*** INO
094900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINO'
095000040708     C                   EVAL      VAOINO = SkSplitCSV(i)
095100030715     C                   ENDIF
095200040708     C*** LOO
095300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOO'
095400040708     C                   EVAL      VAOLOO = SkSplitCSV(i)
095500030715     C                   ENDIF
095600040708     C*** PRO
095700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRO'
095800040708     C                   EVAL      VAOPRO = SkSplitCSV(i)
095900030715     C                   ENDIF
096000040708     C*** NAO
096100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAO'
096200040708     C                   EVAL      VAONAO = SkSplitCSV(i)
096300030721     C                   ENDIF
096400040708     C*** RSR
096500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSR'
096600040708     C                   EVAL      VAORSR = SkSplitCSV(i)
096700040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
096800040708     C     '@':'A'       XLATE     VAORSR        VAORSR
096900040708     C* ==
097000040708     C                   IF        VAORSR = *blanks
097100040708     C                   SETON                                        31
097200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
097300040708     C                             + ' ' + 'VAORSR'
097400040708     C                   ENDIF
097500040708     C                   ENDIF
097600040708     C*** INR
097700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINR'
097800040708     C                   EVAL      VAOINR = SkSplitCSV(i)
097900040708     C                   IF        VAOINR = *blanks
098000040708     C                   SETON                                        31
098100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
098200040708     C                             + ' ' + 'VAOINR'
098300040708     C                   ENDIF
098400040708     C                   ENDIF
098500040708     C*** LOR
098600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOR'
098700040708     C                   EVAL      VAOLOR = SkSplitCSV(i)
098800040708     C                   IF        VAOLOR = *blanks
098900040708     C                   SETON                                        31
099000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
099100040708     C                             + ' ' + 'VAOLOR'
099200040708     C                   ENDIF
099300040708     C                   ENDIF
099400040708     C*** PRR
099500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRR'
099600040708     C                   EVAL      VAOPRR = SkSplitCSV(i)
099700040708     C                   IF        VAOPRR = *blanks
099800040708     C                   SETON                                        31
099900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
100000040708     C                             + ' ' + 'VAOPRR'
100100040708     C                   ENDIF
100200040708     C                   ENDIF
100300040708     C*** NAR
100400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAR'
100500040708     C                   EVAL      VAONAR = SkSplitCSV(i)
100600040708     C                   ENDIF
100700040708     C*** RER
100800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORER'
100900040708     C                   EVAL      VAORER = SkSplitCSV(i)
101000040708     C                   ENDIF
101100040708     C*** TER
101200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOTER'
101300040708     C                   EVAL      VAOTER = SkSplitCSV(i)
101400040708     C                   ENDIF
101500040708     C*** RSC
101600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORSC'
101700040708     C                   EVAL      VAORSC = SkSplitCSV(i)
101800040708     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
101900040708     C     '@':'A'       XLATE     VAORSC        VAORSC
102000040708     C* ==
102100040708     C                   ENDIF
102200040708     C*** INC
102300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOINC'
102400040708     C                   EVAL      VAOINC = SkSplitCSV(i)
102500040708     C                   ENDIF
102600040708     C*** LOC
102700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOLOC'
102800040708     C                   EVAL      VAOLOC = SkSplitCSV(i)
102900040708     C                   ENDIF
103000040708     C*** PRC
103100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPRC'
103200040708     C                   EVAL      VAOPRC = SkSplitCSV(i)
103300040708     C                   ENDIF
103400040708     C*** NAC
103500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAC'
103600040708     C                   EVAL      VAONAC = SkSplitCSV(i)
103700040708     C                   ENDIF
103800040708     C*** FFD
103900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFFD'
104000040708     C                   EVAL      VAOFFD = SkSplitCSV(i)
104100040708     C                   ENDIF
104200040708     C*** RMP
104300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORMP'
104400040708     C                   EVAL      VAORMP = SkSplitCSV(i)
104500040708     C                   ENDIF
104600040708     C*** NAM
104700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONAM'
104800040708     C                   EVAL      VAONAM = SkSplitCSV(i)
104900040708     C                   ENDIF
105000040708     C*** PAG
105100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPAG'
105200040708     C                   EVAL      VAOPAG = SkSplitCSV(i)
105300040708     C                   ENDIF
105400040708     C*** CTR
105500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCTR'
105600040708     C                   EVAL      VAOCTR = SkSplitCSV(i)
105700040708     C                   ENDIF
105800040708     C*** ZOR
105900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOZOR'
106000040708     C                   EVAL      VAOZOR = SkSplitCSV(i)
106100040708     C                   ENDIF
106200040708     C*** NO1
106300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO1'
106400040708     C                   EVAL      VAONO1 = SkSplitCSV(i)
106500030715     C                   ENDIF
106600040708     C*** NO2
106700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONO2'
106800040708     C                   EVAL      VAONO2 = SkSplitCSV(i)
106900030715     C                   ENDIF
107000040708     C*** DDT
107100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODDT'
107200040708     C                   EVAL      VAODDT = SkSplitCSV(i)
107300030715     C                   ENDIF
107400040708     C*** RFA
107500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAORFA'
107600040708     C                   EVAL      VAORFA = SkSplitCSV(i)
107700030715     C                   ENDIF
107800040708     C*** SPI
107900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSPI'
108000040708     C                   EVAL      VAOSPI = SkSplitCSV(i)
108100030715     C                   ENDIF
108200040708     C*** FLO
108300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOFLO'
108400040708     C                   EVAL      VAOFLO = SkSplitCSV(i)
108500040723     C* Per sicurezza pulisco le posizioni 5 e 6 del campo VAOFLO
108600040723     C                   EVAL      %subst(VAOFLO:5:1) = *blanks
108700040723     C                   EVAL      %subst(VAOFLO:6:1) = *blanks
108800030715     C                   ENDIF
108900030715     C
109000030721     C*
109100030721     C* Reperisco quindi i campi numerici...
109200040708     C*** POE
109300040723     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOE'
109400040723     C                   EVAL      PiStr=SkSplitCSV(i)
109500040723     C                   EXSR      CHKNUM
109600040723     C                   IF        PiInt=*on    AND
109700040723     C                             PiVal<=999   AND
109800040723     C                             PiVal>*zeros
109900040723     C                   Z-ADD     PiVal         VAOPOE
110000040723     C                   ELSE
110100040723     C                   SETON                                        32
110200040723     C                   EVAL      VAOPOE = *zeros
110300040723     C                   EVAL      vinmsg = %trimr(vinmsg)
110400040723     C                             + ' ' + 'VAOPOE'
110500040723     C                   ENDIF
110600040723     C                   ENDIF
110700040708     C*** COR
110800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCOR'
110900040708     C                   EVAL      PiStr=SkSplitCSV(i)
111000040708     C                   EXSR      CHKNUM
111100040708     C                   IF        PiInt=*on
111200040708     C                   Z-ADD     PiVal         VAOCOR
111300040708     C                   ELSE
111400040708     C                   SETON                                        31
111500040708     C                   EVAL      VAOCOR = *zeros
111600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
111700040708     C                             + ' ' + 'VAOCOR'
111800040708     C                   ENDIF
111900040708     C                   ENDIF
112000040708     C*** CAO
112100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAO'
112200030721     C                   EVAL      PiStr=SkSplitCSV(i)
112300030721     C                   EXSR      CHKNUM
112400030721     C                   IF        PiInt=*on
112500030721     C                   Z-ADD     PiVal         Num5_0
112600040708     C                   MOVEL(p)  Num5_0        VAOCAO
112700030721     C                   ELSE
112800030721     C                   SETON                                        32
112900040708     C                   EVAL      VAOCAO = *zeros
113000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113100040708     C                             + ' ' + 'VAOCAO'
113200030721     C                   ENDIF
113300030721     C                   ENDIF
113400040708     C*** CRA
113500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRA'
113600030721     C                   EVAL      PiStr=SkSplitCSV(i)
113700030721     C                   EXSR      CHKNUM
113800030721     C                   IF        PiInt=*on
113900040708     C                   Z-ADD     PiVal         VAOCRA
114000030721     C                   ELSE
114100030721     C                   SETON                                        32
114200040708     C                   EVAL      VAOCRA = *zeros
114300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114400040708     C                             + ' ' + 'VAOCRA'
114500030721     C                   ENDIF
114600030721     C                   ENDIF
114700040708     C*** CAR
114800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAR'
114900040708     C                   EVAL      PiStr=SkSplitCSV(i)
115000040708     C                   EXSR      CHKNUM
115100040708     C                   IF        PiInt=*on
115200040708     C                   Z-ADD     PiVal         Num5_0
115300040708     C                   MOVEL(p)  Num5_0        VAOCAR
115400040708     C                   ELSE
115500040708     C                   SETON                                        31
115600040708     C                   EVAL      VAOCAR = *zeros
115700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
115800040708     C                             + ' ' + 'VAOCAR'
115900040708     C                   ENDIF
116000040708     C                   ENDIF
116100040708     C*** CRC
116200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCRC'
116300040708     C                   EVAL      PiStr=SkSplitCSV(i)
116400040708     C                   EXSR      CHKNUM
116500040708     C                   IF        PiInt=*on
116600040708     C                   Z-ADD     PiVal         VAOCRC
116700040708     C                   ELSE
116800040708     C                   SETON                                        32
116900040708     C                   EVAL      VAOCRC = *zeros
117000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
117100040708     C                             + ' ' + 'VAOCRC'
117200040708     C                   ENDIF
117300040708     C                   ENDIF
117400040708     C*** CAC
117500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOCAC'
117600040708     C                   EVAL      PiStr=SkSplitCSV(i)
117700040708     C                   EXSR      CHKNUM
117800040708     C                   IF        PiInt=*on
117900040708     C                   Z-ADD     PiVal         Num5_0
118000040708     C                   MOVEL(p)  Num5_0        VAOCAC
118100040708     C                   ELSE
118200040708     C                   SETON                                        32
118300040708     C                   EVAL      VAOCAC = *zeros
118400040708     C                   EVAL      vinmsg = %trimr(vinmsg)
118500040708     C                             + ' ' + 'VAOCAC'
118600040708     C                   ENDIF
118700040708     C                   ENDIF
118800040708     C*** DAR
118900040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAODAR'
119000040708     C                   EVAL      PiStr=SkSplitCSV(i)
119100040708     C                   EXSR      CHKNUM
119200040708     C                   IF        PiInt=*on
119300040708     C                   Z-ADD     PiVal         VAODAR
119400040708     C                   ELSE
119500040708     C                   SETON                                        32
119600040708     C                   EVAL      VAODAR = *zeros
119700040708     C                   EVAL      vinmsg = %trimr(vinmsg)
119800040708     C                             + ' ' + 'VAODAR'
119900040708     C                   ENDIF
120000040708     C                   ENDIF
120100040708     C*** ORR
120200040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOORR'
120300040708     C                   EVAL      PiStr=SkSplitCSV(i)
120400040708     C                   EXSR      CHKNUM
120500040708     C                   IF        PiInt=*on
120600040708     C                   Z-ADD     PiVal         VAOORR
120700040708     C                   ELSE
120800040708     C                   SETON                                        32
120900040708     C                   EVAL      VAOORR = *zeros
121000040708     C                   EVAL      vinmsg = %trimr(vinmsg)
121100040708     C                             + ' ' + 'VAOORR'
121200040708     C                   ENDIF
121300040708     C                   ENDIF
121400030721     C*** NCL
121500040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAONCL'
121600030721     C                   EVAL      PiStr=SkSplitCSV(i)
121700030721     C                   EXSR      CHKNUM
121800030721     C                   IF        PiInt=*on
121900040708     C                   Z-ADD     PiVal         VAONCL
122000030721     C                   ELSE
122100030721     C                   SETON                                        32
122200040708     C                   EVAL      VAONCL = *zeros
122300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122400040708     C                             + ' ' + 'VAONCL'
122500030721     C                   ENDIF
122600030721     C                   ENDIF
122700040708     C*** PKG
122800040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPKG'
122900030721     C                   EVAL      PiStr=SkSplitCSV(i)
123000030721     C                   EXSR      CHKNUM
123100030721     C                   IF        PiNum=*on
123200040708     C                   Z-ADD     PiVal         VAOPKG
123300030721     C                   ELSE
123400030721     C                   SETON                                        32
123500040708     C                   EVAL      VAOPKG = *zeros
123600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123700040708     C                             + ' ' + 'VAOPKG'
123800030721     C                   ENDIF
123900030721     C                   ENDIF
124000040708     C*** VLM
124100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOVLM'
124200040708     C                   EVAL      PiStr=SkSplitCSV(i)
124300040708     C                   EXSR      CHKNUM
124400040708     C                   IF        PiNum=*on
124500040708     C                   Z-ADD     PiVal         VAOVLM
124600040708     C                   ELSE
124700040708     C                   SETON                                        32
124800040708     C                   EVAL      VAOVLM = *zeros
124900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
125000040708     C                             + ' ' + 'VAOVLM'
125100040708     C                   ENDIF
125200040708     C                   ENDIF
125300040708     C*** BNC
125400040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBNC'
125500040708     C                   EVAL      PiStr=SkSplitCSV(i)
125600040708     C                   EXSR      CHKNUM
125700040708     C                   IF        PiInt=*on
125800040708     C                   Z-ADD     PiVal         VAOBNC
125900040708     C                   ELSE
126000040708     C                   SETON                                        32
126100040708     C                   EVAL      VAOBNC = *zeros
126200040708     C                   EVAL      vinmsg = %trimr(vinmsg)
126300040708     C                             + ' ' + 'VAOBNC'
126400040708     C                   ENDIF
126500040708     C                   ENDIF
126600040708     C*** BLC
126700040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOBLC'
126800040708     C                   EVAL      PiStr=SkSplitCSV(i)
126900040708     C                   EXSR      CHKNUM
127000040708     C                   IF        PiInt=*on
127100040708     C                   Z-ADD     PiVal         VAOBLC
127200040708     C                   ELSE
127300040708     C                   SETON                                        32
127400040708     C                   EVAL      VAOBLC = *zeros
127500040708     C                   EVAL      vinmsg = %trimr(vinmsg)
127600040708     C                             + ' ' + 'VAOBLC'
127700040708     C                   ENDIF
127800040708     C                   ENDIF
127900040708     C*** ATT
128000040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOATT'
128100040708     C                   EVAL      PiStr=SkSplitCSV(i)
128200040708     C                   EXSR      CHKNUM
128300040708     C                   IF        PiInt=*on
128400040708     C                   Z-ADD     PiVal         VAOATT
128500040708     C                   ELSE
128600040708     C                   SETON                                        32
128700040708     C                   EVAL      VAOATT = *zeros
128800040708     C                   EVAL      vinmsg = %trimr(vinmsg)
128900040708     C                             + ' ' + 'VAOATT'
129000040708     C                   ENDIF
129100040708     C                   ENDIF
129200040708     C*** MTC
129300040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOMTC'
129400040708     C                   EVAL      PiStr=SkSplitCSV(i)
129500040708     C                   EXSR      CHKNUM
129600040708     C                   IF        PiInt=*on
129700040708     C                   Z-ADD     PiVal         VAOMTC
129800040708     C                   ELSE
129900040708     C                   SETON                                        32
130000040708     C                   EVAL      VAOMTC = *zeros
130100040708     C                   EVAL      vinmsg = %trimr(vinmsg)
130200040708     C                             + ' ' + 'VAOMTC'
130300040708     C                   ENDIF
130400040708     C                   ENDIF
130500040708     C*** KSC
130600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOKSC'
130700030721     C                   EVAL      PiStr=SkSplitCSV(i)
130800030721     C                   EXSR      CHKNUM
130900030721     C                   IF        PiInt=*on      AND
131000030721     C                             PiVal<=9999999 AND
131100030721     C                             PiVal>*zeros
131200040708     C                   Z-ADD     PiVal         VAOKSC
131300030721     C                   ELSE
131400030721     C                   SETON                                        32
131500040708     C                   EVAL      VAOKSC = *zeros
131600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131700040708     C                             + ' ' + 'VAOKSC'
131800030721     C                   ENDIF
131900030721     C                   ENDIF
132000040708     C*** POR
132100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOR'
132200030721     C                   EVAL      PiStr=SkSplitCSV(i)
132300030721     C                   EXSR      CHKNUM
132400030721     C                   IF        PiInt=*on    AND
132500030721     C                             PiVal<=999   AND
132600030721     C                             PiVal>*zeros
132700040708     C                   Z-ADD     PiVal         VAOPOR
132800030721     C                   ELSE
132900030721     C                   SETON                                        32
133000040708     C                   EVAL      VAOPOR = *zeros
133100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133200040708     C                             + ' ' + 'VAOPOR'
133300030721     C                   ENDIF
133400030721     C                   ENDIF
133500040708     C*** POC
133600040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOPOC'
133700040708     C                   EVAL      PiStr=SkSplitCSV(i)
133800040708     C                   EXSR      CHKNUM
133900040708     C                   IF        PiInt=*on    AND
134000040708     C                             PiVal<=999   AND
134100040708     C                             PiVal>*zeros
134200040708     C                   Z-ADD     PiVal         VAOPOC
134300040708     C                   ELSE
134400040708     C                   SETON                                        32
134500040708     C                   EVAL      VAOPOC = *zeros
134600040708     C                   EVAL      vinmsg = %trimr(vinmsg)
134700040708     C                             + ' ' + 'VAOPOC'
134800040708     C                   ENDIF
134900040708     C                   ENDIF
135000040708     C*** STO
135100040708     C                   IF        %trim(SkSplitFLD(i)) = 'VAOSTO'
135200040708     C                   EVAL      PiStr=SkSplitCSV(i)
135300040708     C                   EXSR      CHKNUM
135400040708     C                   IF        PiInt=*on
135500040708     C                   Z-ADD     PiVal         VAOSTO
135600040708     C                   ELSE
135700040708     C                   SETON                                        32
135800040708     C                   EVAL      VAOSTO = *zeros
135900040708     C                   EVAL      vinmsg = %trimr(vinmsg)
136000040708     C                             + ' ' + 'VAOSTO'
136100040708     C                   ENDIF
136200040708     C                   ENDIF
136300030715     C*
136400030715     C                   ENDIF
136500030715     C                   EVAL      i = i + 1
136600030715     C                   ENDDO
136700020204     C*
136800080418     C* Considerazioni fuori ciclo
136900080418     C*
137000080418     C* =====> Se il cliente passa lui i dati del destinatario svuoto il CRC eventualmente
137100080418     C*        preso come parametro forzato
137200080418     C                   IF        VAORSC <> *blanks OR
137300080418     C                             VAOINC <> *blanks OR
137400080418     C                             VAOCAC <> *blanks OR
137500080418     C                             VAOLOC <> *blanks OR
137600080418     C                             VAOPRC <> *blanks OR
137700080418     C                             VAONAC <> *blanks
137800080418     C                   Z-ADD     *zeros        VAOCRC
137900080418     C                   ENDIF
138000080418     C* <=====
138100080418     C*
138200030715     C                   ENDIF
138300020204     C*
138400010607     C                   ENDSR
138500010607     C*----------------------------------------------------*
138600020204
138700010330
138800010330
138900010330     C*----------------------------------------------------*
139000010330     C*  CONTROLLO NUMERICITA' CAMPI
139100010330     C*----------------------------------------------------*
139200010330     C     CHKNUM        BEGSR
139300010330     C*
139400010606     C                   IF        PiDecChr = *blanks
139500030715     C                   EVAL      PiDecChr = CharNUM
139600010606     C                   ENDIF
139700010606     C*
139800010606     C                   CALL(e)   'ISNUMERIC'
139900010330     C                   PARM                    PiStr            30
140000010606     C                   PARM                    PiDecChr          1
140100010330     C                   PARM      *ZEROS        PiVal            30 9
140200010330     C                   PARM      '0'           PiInt             1
140300010330     C                   PARM      '0'           PiNum             1
140400010330     C                   IF        %error
140500010606     C                   EVAL      PiNum=*off
140600010330     C                   ENDIF
140700010330     C*
140800010330     C                   ENDSR
140900010330     C***
141000010330
141100010601
141200010601
141300010601
141400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
141500010601     C     repfil        BEGSR
141600010601     C*
141700010601     C                   if        invfil = *zeros and
141800010601     C                             depfil > *zeros and
141900010629     C                             (vinflg = *blanks or
142000010629     C                              vinflg = *zeros)
142100010601     C
142200010601     C                   eval      invfil = depfil
142300010601     C                   endif
142400010601     C*
142500010601     C                   if        depfil <> invfil and
142600010601     C                             invfil > *zeros
142700010601     C                   eval      flgMulti = '1'
142800010601     C                   if        vinflg = *blanks
142900010601     C                   add       1             cntNonEl
143000010601     C                   endif
143100010601     C                   endif
143200010601     C*
143300010601     C                   if        vinflg = '2'
143400010601     C                   eval      flgStato = '2'
143500010601     C                   endif
143600010601     C*
143700010601     C                   ENDSR
143800010601     C***
143900010601
144000010601
144100010601
144200010330
144300010330
144400010330
144500990920      /TITLE Invio dei dati al punto operativo.
144600000613     C     invio         BEGSR
144700990920     C*
144800990920     C                   reset                   dscmz
144900010601     C                   move      invfil        cmzdst
145000990920     C                   eval      cmzfld = vlrfou
145100990920     C                   eval      cmzmbd = vlrhdl
145200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
145300000710     C                   if        prmfir = *blanks
145400040708     C                   eval      cmzfla = 'FNVAO00F'
145500040708     C                   eval      cmzmba = 'FNVAO00F'
145600000710     C                   else
145700000710     C                   eval      cmzfla = prmfir
145800000710     C                   eval      cmzmba = prmfir
145900000710     C                   endif
146000990920     C                   eval      cmznrr = *zeros
146100990920     C                   move      §ctrok        cmznrr
146200021018     C                   eval      cmzlba = vlrfl1
146300990920     C                   call(e)   'TIS711C'
146400990920     C                   parm                    dscmz
146500990921     C                   parm      *blanks       esito
146600990920     C                   if        %error
146700990920     C                             or cmzerr = '1'
146800990921     C                             or esito  = '1'
146900000710     C                   eval      wrkesito = '3'
147000990920     C                   endif
147100990920     C*
147200000613     C                   ENDSR
147300990910
147400010601
147500010601
147600010601
147700010601
147800010601      /TITLE Invio dei dati al punto operativo.
147900010601     C     opeini        BEGSR
148000010601     C*
148100010601     C* Inizializzo flag e contatori operativi
148200010601     C                   movel     '0'           flgGiro           1
148300010601     C                   movel     '0'           flgMulti          1
148400010601     C                   movel     '1'           flgStato          1
148500010615     C                   movel     '0'           flgOk             1
148600010601     C                   z-add     *zeros        cntNonEl         10 0
148700010601     C                   z-add     *zeros        depfil            3 0
148800010601     C                   z-add     *zeros        invfil            3 0
148900010601     C*
149000010601     C                   ENDSR
149100010601     C***
149200050421
149300050421
149400050421
149500050421     C     *pssr         BEGSR
149600050421     C*
149700050421     C                   eval      wrkesito = '2'
149800050421     C*
149900050421     C                   ENDSR     '*CANCL'
150000050421     C***
150100060306
150200060306
150300060306     C*-------------------------------------------------------------------------*
150400060306     C* sr_Control :  controllo abilitazione cliente al ritorno fasi ORM
150500060306     C*-------------------------------------------------------------------------*
150600060306     C     sr_Control    begsr
150700060306     C*
150800060306     C* inizializzo a ok il flag d controllo abilitazione
150900060306     C                   eval      Cli_Abil = *on
151000060306     C
151100060306     C* controllo se il cliente è abilitato al tipo file "VP" in tabella "VAP"
151200060306     C                   clear                   woriIDC
151300060306     C                   eval      kteCOD = 'VAP'
151400060306     C                   eval      kteKE1 = oriIDC
151500060306     C     keytbe        chain     TNTBE01L
151600060306     C                   if        %found(Tntbe01l)
151700060306     C                   eval      woriIDC = TbeKe2
151800060306     C                   else
151900060306     C                   eval      Cli_Abil = *off
152000060306     C                   endif
152100060306     C*
152200060306     C                   endsr
152300010601
152400010601
152500010601
152600010330
152700010330
152800000613     C     *inzsr        BEGSR
152900990910     C*
153000990910     C     *entry        plist
153100990920     C                   parm                    tivlrds
153200990921     C                   parm      wrkesito      esito
153300000724     C                   parm                    prmlit
153400000710     C                   parm                    prmfir
153500010330     C*
153600010330     C* CALCOLA LA DATA CORRENTE
153700010330     C                   time                    wn14             14 0
153800040723     C                   movel     wn14          wn6               6 0          *ORA (6)
153900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
154000010330     C                   z-add     wn8           g08dat
154100010330     C                   z-add     *zeros        g08inv
154200010330     C                   movel     '0'           g08err
154300010330     C                   call      'XSRDA8'
154400010330     C                   parm                    wlbda8
154500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
154600040723     C*
154700040723     C* Campi riferito al database
154800040723     C     *like         define    tbecod        kteCOD                         *TNTBE01L
154900040723     C     *like         define    tbeke1        kteKE1
155000040723     C*
155100040723     C* Definizione chiavi di lettura
155200040723     C* TNTBE01L - parziale
155300040723     C     keytbe        klist
155400040723     C                   kfld                    kteCOD                         *UTENTE
155500040723     C                   kfld                    kteKE1                         *CODICE
155600000613     C*
155700000613     C                   ENDSR
155800000613     C***
155900990908
156000030715     Otitvxlp   E            riepilogo         2
156100990915     O                                              'Upload via Internet'
156200990915     O                                           +1 'Traduzione TIVIN00R -'
156300040708     O                                           +1 'FNVAOWWR'
156400990915     O                                           +1 'Testate bolle'
156500990915     O          E            riepilogo   2
156600990915     O                       wrkdata
156700990915     O                       wrkora              +1
156800990915     O                       procname            +1
156900990915     O          E            riepilogo   2
157000990915     O                                              'Cliente..................:'
157100040708     O                       VAOCOR        z     +1
157200990915     O          E            riepilogo   2
157300990920     O                                              'Riferimento Strategi.....:'
157400990920     O                       vlrhdl              +1
157500990915     O          E            riepilogo   2
157600990915     O                                              'Giusti...................:'
157700971022     O                       §CTROK        2   +  1
157800990915     O          E            riepilogo   2
157900990915     O                                              'Sbagliati e corretti.....:'
158000971022     O                       §CTRMO        2   +  1
158100990915     O          E            riepilogo   2
158200990915     O                                              'Sbagliati e scartati.....:'
158300971022     O                       §CTRNO        2   +  1
158400000613
158500030715     Otitvxlps  E            testdett          2
158600000613     O                                              'Upload via Internet'
158700000613     O                                           +1 'Traduzione TIVIN00R -'
158800040708     O                                           +1 'FNVAOWWR'
158900000613     O                                           +1 'Testate bolle'
159000000616     O          E            testdett    3
159100000613     O                                           +2 'N° rec'
159200000613     O                                           +3 'Anteprima contenuto'
159300000616     O          E            rigadett    2
159400000613     O                       rrnum               +2
159500000621     O                       recko               +3
159600000616     O          E            findett     2
159700000613     O                       wrkdata
159800000613     O                       wrkora              +1
159900000613     O                       procname            +1
160000000616     O          E            findett     2
160100000613     O                                              'Cliente..................:'
160200040708     O                       VAOCOR        z     +1
160300000616     O          E            findett     2
160400000613     O                                              'Riferimento Strategi.....:'
160500000613     O                       vlrhdl              +1
160600000616     O          E            findett     2
160700000613     O                                              'Giusti...................:'
160800000613     O                       §CTROK        2   +  1
160900000616     O          E            findett     2
161000000613     O                                              'Sbagliati e corretti.....:'
161100000613     O                       §CTRMO        2   +  1
161200000616     O          E            findett     2
161300000613     O                                              'Sbagliati e scartati.....:'
161400000613     O                       §CTRNO        2   +  1
161500000613** CMD - COMANDI CL
161600030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
161700030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
161800030715DLTOVR FILE(TITVXLP TITVXLPS) LVL(*)
161900010606
162000000613
