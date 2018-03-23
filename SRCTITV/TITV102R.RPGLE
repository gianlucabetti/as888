000100070117      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVADWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700070117     FFIVADwwr  O    E             DISK    usropn
000800040122     Ftitv92p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000040122     Ftitv92ps  O    f  198        PRINTER usropn
001100000621     F                                     oflind(*inob)
001200000313     D*
001300000613     D*------------
001400000613     D* COMANDI
001500000613     D*------------
001600011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001700040113     D*------------
001800040113     D* SCHIEDE A PROGRAMMA
001900040113     D*------------
002000040113     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
002100010330     D*----------------------------------------------------
002200010330     D* DICHIARAZIOINE VARIABILI DI WRK
002300010330     D*----------------------------------------------------
002400010330     D dscmz         e ds                  inz
002500010330     D psds           sds
002600010330     D  procname         *PROC
002700010330     D tivlrds       e ds                  extname(tivlr00f)
002800040128     D tisi95ds      e ds
002900010330     D esito           s              1
003000010330     D prmlit          s             10
003100010330     D prmfir          s             10
003200010330     D wrkesito        s                   like(esito)
003300010330     D wrkdata         s               d
003400010330     D wrkora          s               t
003500010330     D rrnum           s              6  0 INZ(*zeros)
003600010330     D recko           s            150    INZ(*blanks)
003700011113     D depcmd          s            150    INZ(*blanks)
003800040119     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
003900040510     D parccm          s              8    INZ(*blanks)
004000040119     D parmbr          s             10    INZ(*blanks)
004100040119     D paropz          s              1    INZ(*blanks)
004200040119     D chkcall         s              1    INZ(*blanks)
004300030715     D*------------------
004400030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004500030715     D*------------------
004600030715     D SkSplitFLD      S             10    DIM(100)
004700030715     D SkSplitCSV      S            256    DIM(100)
004800030715     D CharCSV         S              1
004900030715     D CharTXT         S              1
005000030715     D CharNUM         S              1
005100030715     D posDa           S              3  0 INZ(*zeros)
005200030715     D posA            S              3  0 INZ(*zeros)
005300030715     D i               s              3  0 INZ(1)
005400030715     D wGiro           s              1  0 INZ(*zeros)
005500070117     D wVADCDP_1       s                   LIKE(VADCDP)
005600070117     D wVADNCL_1       s                   LIKE(VADNCL)
005700070117     D wVADCDP_2       s                   LIKE(VADCDP)
005800070117     D wVADNCL_2       s                   LIKE(VADNCL)
005900070117     D wVADCDP_3       s                   LIKE(VADCDP)
006000070117     D wVADNCL_3       s                   LIKE(VADNCL)
006100070117     D wVADCDP_4       s                   LIKE(VADCDP)
006200070117     D wVADNCL_4       s                   LIKE(VADNCL)
006300070117     D wVADCDP_5       s                   LIKE(VADCDP)
006400070117     D wVADNCL_5       s                   LIKE(VADNCL)
006500060331     D wVABNSP         s                   LIKE(VABNSP) INZ
006600030822     D*------------------
006700030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006800030822     D*------------------
006900030822     D posDaDft        S              3  0 INZ(*zeros)
007000030822     D posADft         S              3  0 INZ(*zeros)
007100030822     D j               s              3  0 INZ(1)
007200030822     D wGiroDft        s              1  0 INZ(*zeros)
007300010330     D*------------------
007400010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
007500010330     D*------------------
007600010330     D WLBDA8          DS                  INZ
007700010330     D  G08DAT                 1      8  0
007800010330     D  G08INV                 9     16  0
007900010330     D  G08ERR                17     17
008000010330     D  G08TGI                18     22  0
008100031201     D*------------------
008200031201     D* Costanti
008300031201     D*------------------
008400031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008500031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008600010330
008700010330
008800010330
008900990915     C                   time                    wrkdata
009000990915     C                   time                    wrkora
009100000913     C                   reset                   rrnum
009200990921     C                   reset                   esito
009300990921     C                   reset                   wrkesito
009400010601     C*
009500010601     C                   exsr      opeini
009600010605     C                   exsr      rwvab
009700081006     C*
009800081006     C                   exsr      endela
009900010601     C*
010000010601     C                   seton                                        lr
010100010601
010200010601
010300010601
010400010601     C*--------------------------------------------------------
010500010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010600010601     C*--------------------------------------------------------
010700010601     C     PREELA        BEGSR
010800010601     C*
010900010601     C* SE OCCORRE SPEDIRE IN FILIALE
011000010601     C                   if        invfil <> *zeros and
011100010601     C                             flgGiro = '0'
011200010601     C*
011300010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011400010601     C                   eval      flgGiro = '1'
011500000724     C*
011600000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
011700000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011800010601     C     invfil        chain     azorg01l
011900000724     C                   if        %found
012000000724     C                   movel(p)  CMD(1)        depcmd
012100020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
012200000724     C*
012300000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
012400011113     C                   Z-ADD     150           LENGH            15 5
012500000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012600000724     C                   PARM                    depcmd
012700000724     C                   PARM                    LENGH
012800000724     C*
012900000724     C                   endif
013000000616     C*
013100000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
013200000616     C                   movel(p)  CMD(2)        depcmd
013300000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
013400000616     C*
013500000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
013600011113     C                   Z-ADD     150           LENGH            15 5
013700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013800000616     C                   PARM                    depcmd
013900000616     C                   PARM                    LENGH
014000000613     C*
014100040122     C                   if        not %open(titv92ps)
014200040122     C                   open      titv92ps
014300030715     C                   except    testdett
014400000613     C                   endif
014500010601     C*
014600010601     C                   endif
014700010601     C*
014800010601     C                   ENDSR
014900010601     C***
015000010601
015100010601
015200010601
015300010601
015400010601     C*--------------------------------------------------------
015500010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
015600010601     C*--------------------------------------------------------
015700081006     C     ENDELA        BEGSR
015800990915     C*
015900000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
016000081006     C*
016100081006     C                   if        %open(tivin00r)
016200081006     C                   close     tivin00r
016300081006     C                   endif
016400081006     C                   if        %open(fivabwwr)
016500081006     C                   close     fivabwwr
016600081006     C                   endif
016700081006     C                   if        %open(fivadwwr)
016800081006     C                   close     fivadwwr
016900081006     C                   endif
017000040119     C*
017100040119     C* Effettuo la chiamata al CLLE preposto
017200070117     C                   call(e)   'TITVVDC'
017300040510     C                   parm                    parccm
017400040119     C                   parm                    parmbr
017500040119     C                   parm      '2'           paropz
017600050201     C*
017700050201     C* Effettuo lancio TISI95 solo x chiusura
017800050201     C                   CLEAR                   TISI95DS
017900050201     C                   EVAL      I95TLA = 'C'
018000050201     C                   CALL      'TISI95R'
018100050201     C                   PARM                    TISI95DS
018200000613     C*
018300040122     C                   if        %open(titv92ps)
018400000616     C                   except    findett
018500040122     C                   close     titv92ps
018600000613     C                   endif
018700000616     C*
018800000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
018900011113     C                   Z-ADD     150           LENGH            15 5
019000010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
019100000616     C                   PARM                    CMD(3)
019200000616     C                   PARM                    LENGH
019300000616     C*
019400010601     C                   ENDSR
019500010601     C***
019600010601
019700010601
019800010601
019900000613
020000000613     C*--------------------------------------------------------
020100000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
020200000613     C*--------------------------------------------------------
020300000621     C     STPR          BEGSR
020400000613     C*
020500040122     C                   if        not %open(titv92p)
020600040122     C   50              open      titv92p
020700990915     C                   endif
020800990915     C*
020900030715     C   50              except    riepilogo
021000990915     C*
021100040122     C                   if        %open(titv92p)
021200040122     C   50              close     titv92p
021300990914     C                   endif
021400000613     C*
021500000613     C                   ENDSR
021600000613     C***
021700000613
021800010601
021900010601
022000010330     C*--------------------------------------------------------
022100021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
022200010330     C*--------------------------------------------------------
022300010605     C     RWVAB         BEGSR
022400010330     C*
022500010330     C                   if        not %open(tivin00r)
022600010330     C                   open      tivin00r
022700010330     C                   endif
022800021025     C                   if        not %open(fivabwwr)
022900021025     C                   open      fivabwwr
023000010330     C                   endif
023100040119     C*
023200070117     C* Eseguo operazioni di aggiunta nuovo membro in FIVADWWR
023300070117     C                   exsr      prevad
023400040119     C*
023500040119     C                   if        chkcall = '0'
023600040119     C*
023700070117     C                   if        not %open(fivadwwr)
023800070117     C                   open      fivadwwr
023900040119     C                   endif
024000010330     C*
024100040119     C                   clear                   §CTROKVB          7 0
024200070117     C                   clear                   §CTROKVD          7 0
024300040119     C                   clear                   §CTRMO            7 0
024400040119     C                   clear                   §CTRNO            7 0
024500010330     C*
024600030822     C                   do        *HIVAL
024700010330     C*
024800030822     C                   read      tivin00r                               70
024900010618     C*
025000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
025100010618     C                   if        vinflg = '1'
025200010618     C                   eval      flgOk = '1'
025300010618     C                   endif
025400040510     C*
025500040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
025600040510     C                   z-add     *zeros        wLenVINDTA        4 0
025700040510     C                   z-add     *zeros        wFlgVINDTA        1 0
025800040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
025900040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
026000040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
026100040510     C                   z-add     1             wFlgVINDTA
026200040510     C                   leave
026300040510     C                   endif
026400040510     C                   enddo
026500010618     C*
026600040510     C                   if        vindta > *blanks AND
026700040510     C                             wFlgVINDTA = 1
026800010330     C                   add       1             rrnum
026900010330     C*
027000010601     C                   if        *in70 = *off and
027100010330     C                             (vinflg = *blanks
027200010330     C                              or vinflg = '0'
027300010330     C                              or vinflg = '2')
027400010330     C*
027500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
027600010711     C                   if        vinflg = *blanks or vinflg = '0'
027700010711     C                   clear                   vinmsg
027800010711     C                   endif
027900010601     C*
028000010330     C                   exsr      inzvar
028100010605     C                   exsr      impvab
028200010601     C*
028300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
028400010605     C                   eval      depfil = VABLNP
028500010601     C                   exsr      repfil
028600010601     C                   if        depfil = invfil
028700021025     C                   if        vlrpoi = 999
028800030822     C                   move(P)   invfil        VABFGS
028900021025     C                   else
029000030822     C                   move(P)   vlrpoi        VABFGS
029100021025     C                   endif
029200010601     C*
029300010601     C                   exsr      PREELA
029400010601     C*
029500010604     C* Ebbene...
029600010604     C*
029700040119     C  N31              add       1             §CTROKVB
029800040119     C   32              add       1             §CTRMO
029900040119     C   31              add       1             §CTRNO
030000030822     C                   if        wGiro = 2
030100070117     C* Scrivo sempre il VAD
030200070117     C  N31              exsr      wrivad
030300040510     C* Scrivo il VAB solo a rottura d spedizione
030400040119     C                   if        depspe =  *zeros OR
030500040119     C                             depspe <> vabNSP
030600040119     C  N31              exsr      wrivab
030700040119     C  N31              eval      depspe = vabNSP
030800040119     C                   endif
030900040119     C                   endif
031000010604     C*
031100010604     C                   if        *in31 = *off and
031200010604     C                             *in32 = *off
031300010604     C                   eval      vinflg = '1'
031400010604     C                   else
031500010604     C                   eval      recko = vindta
031600010604     C                   except    rigadett
031700010604     C                   eval      vinflg = '2'
031800010604     C                   endif
031900010604     C                   endif
032000010601     C*
032100010604     C                   endif
032200010604     C*
032300010330     C                   else
032400010330     C                   eval      vinflg = '1'
032500010330     C                   endif
032600010601     C*
032700010601     C  N70              update    tivin000
032800010330     C*
032900030822     C  N70              enddo
033000040119     C*
033100040119     C                   endif                                                  (endif - chkcall)
033200010601     C*
033300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
033400010601     C                   if        cntNonEl = *zeros or
033500010601     C                             flgMulti = '0'
033600010330     C* Se non ci sono record con errori ...
033700010601     C                   if        §ctrno = 0 and
033800010604     C                             §ctrmo = 0 and
033900010601     C                             flgStato <> '2'
034000010330     C* ... restituisco esito OK.
034100010330     C                   eval      wrkesito = '0'
034200010330     C                   else
034300040119     C                   if        §ctrokvb > 0
034400010330     C                   eval      wrkesito = '1'
034500010330     C                   else
034600010615     C                   if        flgOk = '0'
034700010615     C                   eval      wrkesito = '2'
034800010615     C                   else
034900010615     C                   eval      wrkesito = '6'
035000010615     C                   endif
035100010330     C                   endif
035200010330     C                   endif
035300010601     C                   else
035400010601     C                   eval      wrkesito = '9'
035500010601     C                   endif
035600010601     C*
035700010601     C                   if        vlrpoi <> 999
035800010601     C                   eval      invfil = vlrpoi
035900010601     C                   endif
036000010330     C*
036100040119     C                   if        §ctrokvb > 0
036200010601     C                             and invfil > *zeros
036300010330     C                   exsr      invio
036400010330     C                   endif
036500010601     C*
036600010618     C                   if        flgGiro = '1'
036700010601     C                   exsr      endela
036800010618     C                   endif
036900010330     C*
037000010330     C                   ENDSR
037100010330     C***
037200040119
037300040119
037400040119
037500040119     C*----------------------------------------------------*
037600040119     C*  SCARICAMENTO BUFFER RECORDS VAB
037700040119     C*----------------------------------------------------*
037800040510     C     WRIVAB        BEGSR
037900040119     C*
038000060331     C* Gestisco l'eventuale rottura x numero spedizione
038100060331     C                   if        VABNSP <> wVABNSP
038200040119     C                   write     fivab000                                     => scarico il VAB
038300060331     C                   eval      wVABNSP = VABNSP
038400060331     C                   endif
038500040119     C*
038600040119     C                   ENDSR
038700040119
038800040119
038900040119
039000040119     C*----------------------------------------------------*
039100070117     C*  SCARICAMENTO BUFFER RECORDS VAD
039200040119     C*----------------------------------------------------*
039300070117     C     WRIVAD        BEGSR
039400060512     C*
039500060512     C* Inizializzo i campi chiave
039600070117     C                   eval      VADFGS = VABFGS
039700070117     C                   eval      VADCCM = VABCCM
039800070117     C                   eval      VADAAS = VABAAS
039900070117     C                   eval      VADLNP = VABLNP
040000070117     C                   eval      VADNRS = VABNRS
040100070117     C                   eval      VADNSP = VABNSP
040200060331     C*
040300070117     C* Gestisco estensioni VAD "specificatamente esplicitate"
040400070117     C                   if        wVADCDP_1 <> *blanks AND
040500070117     C                             wVADNCL_1  > *zeros
040600070117     C                   eval      VADCDP = wVADCDP_1
040700070117     C                   eval      VADNCL = wVADNCL_1
040800070117     C                   write     FIVAD000
040900070117     C                   add       1             §CTROKVD
041000060331     C                   endif
041100040119     C*
041200070117     C                   if        wVADCDP_2 <> *blanks AND
041300070117     C                             wVADNCL_2  > *zeros
041400070117     C                   eval      VADCDP = wVADCDP_2
041500070117     C                   eval      VADNCL = wVADNCL_2
041600070117     C                   write     FIVAD000
041700070117     C                   add       1             §CTROKVD
041800070117     C                   endif
041900070117     C*
042000070117     C                   if        wVADCDP_3 <> *blanks AND
042100070117     C                             wVADNCL_3  > *zeros
042200070117     C                   eval      VADCDP = wVADCDP_3
042300070117     C                   eval      VADNCL = wVADNCL_3
042400070117     C                   write     FIVAD000
042500070117     C                   add       1             §CTROKVD
042600070117     C                   endif
042700070117     C*
042800070117     C                   if        wVADCDP_4 <> *blanks AND
042900070117     C                             wVADNCL_4  > *zeros
043000070117     C                   eval      VADCDP = wVADCDP_4
043100070117     C                   eval      VADNCL = wVADNCL_4
043200070117     C                   write     FIVAD000
043300070117     C                   add       1             §CTROKVD
043400070117     C                   endif
043500070117     C*
043600070117     C                   if        wVADCDP_5 <> *blanks AND
043700070117     C                             wVADNCL_5  > *zeros
043800070117     C                   eval      VADCDP = wVADCDP_5
043900070117     C                   eval      VADNCL = wVADNCL_5
044000070117     C                   write     FIVAD000
044100070117     C                   add       1             §CTROKVD
044200070117     C                   endif
044300070117     C*
044400040119     C                   ENDSR
044500040119
044600040119
044700040119
044800040119     C*----------------------------------------------------*
044900070117     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVADWWR
045000040119     C*----------------------------------------------------*
045100070117     C     PREVAD        BEGSR
045200040119     C*
045300070117     C* Compongo il nome del membro da dare al FIVADWWR
045400040119     C                   eval      parmbr = vlrhdl
045500040119     C                   movel     'M'           parmbr
045600040510     C                   eval      parccm = vlrksc
045700040119     C                   eval      paropz = '1'
045800040119     C* Effettuo la chiamata al CLLE preposto
045900070117     C                   call(e)   'TITVVDC'
046000040119     C                   parm                    parccm
046100040119     C                   parm                    parmbr
046200040119     C                   parm                    paropz
046300040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046400040119     C                   if        %error
046500040119     C                   movel     '1'           chkcall
046600040119     C                   else
046700040119     C                   movel     '0'           chkcall
046800040119     C                   endif
046900040119     C*
047000040119     C                   ENDSR
047100010601
047200010601
047300010601
047400010330     C*----------------------------------------------------*
047500030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
047600010330     C*----------------------------------------------------*
047700040119     C     INZVAR        BEGSR
047800010330     C*
047900030822     C* Inizializzo variabili di wrk
048000020204     C                   Z-ADD     *zeros        Num5_0            5 0
048100070117     C                   MOVEL     *blanks       wVADCDP_1
048200070117     C                   MOVEL     *blanks       wVADCDP_2
048300070117     C                   MOVEL     *blanks       wVADCDP_3
048400070117     C                   MOVEL     *blanks       wVADCDP_4
048500070117     C                   MOVEL     *blanks       wVADCDP_5
048600070117     C                   Z-ADD     *zeros        wVADNCL_1
048700070117     C                   Z-ADD     *zeros        wVADNCL_2
048800070117     C                   Z-ADD     *zeros        wVADNCL_3
048900070117     C                   Z-ADD     *zeros        wVADNCL_4
049000070117     C                   Z-ADD     *zeros        wVADNCL_5
049100030822     C*
049200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
049300030822     C                   CLEAR                   FIVAB000
049400070117     C                   CLEAR                   FIVAD000
049500030822     C                   CLEAR                   SkSplitCSV
049600030822     C                   EVAL      i = 1
049700030822     C                   EVAL      posDa = *zeros
049800030822     C                   EVAL      posA  = *zeros
049900030822     C*
050000030822     C* Reimposto i valori di default
050100030822     C                   EXSR      DEFCAM
050200030822     C*
050300010330     C                   ENDSR
050400010330     C*----------------------------------------------------*
050500030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
050600010330     C*----------------------------------------------------*
050700010330     C     DEFCAM        BEGSR
050800030822     C*
050900030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
051000030715     C* e delimitatore testo.
051100030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
051200030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
051300030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
051400030822     C*
051500030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
051600030715     C                   IF        %subst(vlrppt:1:1) = 'S'
051700030715     C                   SETON                                        50
051800030715     C                   ELSE
051900030715     C                   SETOFF                                       50
052000030715     C                   ENDIF
052100030822     C*
052200030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
052300030822     C                   EVAL      posDaDft = 1
052400030822     C                   EVAL      posADft  = 0
052500030822     C                   EVAL      wGiroDft = 0
052600030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
052700030822     C                             posDaDft > 0
052800030822     C*
052900030822     C* Gestisco il 1° giro
053000030822     C                   IF        wGiroDft = 0
053100030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
053200030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
053300030822     C* Incremento il contatore dei "giri"
053400030822     C                   EVAL      wGiroDft = 1
053500030822     C                   ELSE
053600030822     C                   EVAL      posDaDft = posADft
053700030822     C                   ENDIF
053800030822     C* Eseguo lo scan x trovare la fine del campo corrente
053900030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
054000030822     C*
054100030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
054200030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
054300030822     C                   IF        posDaDft > 0 AND
054400030822     C                             posADft  > 0
054500030822     C* NCL
054600030822     C                   IF        %subst(
054700030822     C                             %subst(vlrppt:posDaDft+1:
054800030822     C                             posADft-posDaDft-1):1:3)
054900030822     C                             = 'NCL'
055000030822     C                   EVAL      PiStr=%trim(%subst(
055100030822     C                             %subst(vlrppt:posDaDft+1:
055200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055300030822     C                   EXSR      CHKNUM
055400030822     C                   IF        PiInt=*on
055500030822     C                   Z-ADD     PiVal         VABNCL
055600030822     C                   ENDIF
055700030822     C                   ENDIF
055800030822     C* CCM
055900030822     C                   IF        %subst(
056000030822     C                             %subst(vlrppt:posDaDft+1:
056100030822     C                             posADft-posDaDft-1):1:3)
056200030822     C                             = 'CCM'
056300030822     C                   EVAL      PiStr=%trim(%subst(
056400030822     C                             %subst(vlrppt:posDaDft+1:
056500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056600030822     C                   EXSR      CHKNUM
056700030822     C                   IF        PiInt=*on
056800030822     C                   Z-ADD     PiVal         VABCCM
056900030822     C                   ENDIF
057000030822     C                   ENDIF
057100030822     C* LNP
057200030822     C                   IF        %subst(
057300030822     C                             %subst(vlrppt:posDaDft+1:
057400030822     C                             posADft-posDaDft-1):1:3)
057500030822     C                             = 'LNP'
057600030822     C                   EVAL      PiStr=%trim(%subst(
057700030822     C                             %subst(vlrppt:posDaDft+1:
057800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057900030822     C                   EXSR      CHKNUM
058000030822     C                   IF        PiInt=*on
058100030822     C                   Z-ADD     PiVal         VABLNP
058200030822     C                   ENDIF
058300030822     C                   ENDIF
058400030822     C* NRS
058500030822     C                   IF        %subst(
058600030822     C                             %subst(vlrppt:posDaDft+1:
058700030822     C                             posADft-posDaDft-1):1:3)
058800030822     C                             = 'NRS'
058900030822     C                   EVAL      PiStr=%trim(%subst(
059000030822     C                             %subst(vlrppt:posDaDft+1:
059100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059200030822     C                   EXSR      CHKNUM
059300030822     C                   IF        PiInt=*on
059400030822     C                   Z-ADD     PiVal         VABNRS
059500030822     C                   ENDIF
059600030822     C                   ENDIF
059700030822     C* CTR
059800030822     C                   IF        %subst(
059900030822     C                             %subst(vlrppt:posDaDft+1:
060000030822     C                             posADft-posDaDft-1):1:3)
060100030822     C                             = 'CTR'
060200030822     C                   EVAL      PiStr=%trim(%subst(
060300030822     C                             %subst(vlrppt:posDaDft+1:
060400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060500030822     C                   EXSR      CHKNUM
060600030822     C                   IF        PiInt=*on
060700030822     C                   Z-ADD     PiVal         VABCTR
060800030822     C                   ENDIF
060900030822     C                   ENDIF
061000030822     C* PKB
061100030822     C                   IF        %subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):1:3)
061400030822     C                             = 'PKB'
061500030822     C                   EVAL      PiStr=%trim(%subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061800030822     C                   EXSR      CHKNUM
061900030822     C                   IF        PiNum=*on
062000030822     C                   Z-ADD     PiVal         VABPKB
062100030822     C                   ENDIF
062200030822     C                   ENDIF
062300030822     C* VLB
062400030822     C                   IF        %subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):1:3)
062700030822     C                             = 'VLB'
062800030822     C                   EVAL      PiStr=%trim(%subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063100030822     C                   EXSR      CHKNUM
063200030822     C                   IF        PiNum=*on
063300030822     C                   Z-ADD     PiVal         VABVLB
063400030822     C                   ENDIF
063500030822     C                   ENDIF
063600030822     C* QFT
063700030822     C                   IF        %subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):1:3)
064000030822     C                             = 'QFT'
064100030822     C                   EVAL      PiStr=%trim(%subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064400030822     C                   EXSR      CHKNUM
064500030822     C                   IF        PiNum=*on
064600030822     C                   Z-ADD     PiVal         VABQFT
064700030822     C                   ENDIF
064800030822     C                   ENDIF
064900030822     C* CBO
065000030822     C                   IF        %subst(
065100030822     C                             %subst(vlrppt:posDaDft+1:
065200030822     C                             posADft-posDaDft-1):1:3)
065300030822     C                             = 'CBO'
065400030822     C                   EVAL      VABCBO=%trim(%subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065700030822     C                   ENDIF
065800030822     C* TSP
065900030822     C                   IF        %subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):1:3)
066200030822     C                             = 'TSP'
066300030822     C                   EVAL      VABTSP=%trim(%subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066600030822     C                   ENDIF
066700030822     C* VAS
066800030822     C                   IF        %subst(
066900030822     C                             %subst(vlrppt:posDaDft+1:
067000030822     C                             posADft-posDaDft-1):1:3)
067100030822     C                             = 'VAS'
067200030822     C                   EVAL      VABVAS=%trim(%subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067500030822     C                   ENDIF
067600030822     C* VCA
067700030822     C                   IF        %subst(
067800030822     C                             %subst(vlrppt:posDaDft+1:
067900030822     C                             posADft-posDaDft-1):1:3)
068000030822     C                             = 'VCA'
068100030822     C                   EVAL      VABVCA=%trim(%subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068400030822     C                   ENDIF
068500030822     C* GCA
068600030822     C                   IF        %subst(
068700030822     C                             %subst(vlrppt:posDaDft+1:
068800030822     C                             posADft-posDaDft-1):1:3)
068900030822     C                             = 'GCA'
069000030822     C                   EVAL      VABGCA=%trim(%subst(
069100030822     C                             %subst(vlrppt:posDaDft+1:
069200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069300030822     C                   ENDIF
069400030822     C* CTM
069500030822     C                   IF        %subst(
069600030822     C                             %subst(vlrppt:posDaDft+1:
069700030822     C                             posADft-posDaDft-1):1:3)
069800030822     C                             = 'CTM'
069900030822     C                   EVAL      VABCTM=%trim(%subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070200030822     C                   ENDIF
070300030822     C* FFD
070400030822     C                   IF        %subst(
070500030822     C                             %subst(vlrppt:posDaDft+1:
070600030822     C                             posADft-posDaDft-1):1:3)
070700030822     C                             = 'FFD'
070800030822     C                   EVAL      VABFFD=%trim(%subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071100030822     C                   ENDIF
071200030822     C* VAD
071300030822     C                   IF        %subst(
071400030822     C                             %subst(vlrppt:posDaDft+1:
071500030822     C                             posADft-posDaDft-1):1:3)
071600030822     C                             = 'VAD'
071700030822     C                   EVAL      VABVAD=%trim(%subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072000030822     C                   ENDIF
072100030822     C* GMA
072200030822     C                   IF        %subst(
072300030822     C                             %subst(vlrppt:posDaDft+1:
072400030822     C                             posADft-posDaDft-1):1:3)
072500030822     C                             = 'GMA'
072600030822     C                   EVAL      VABGMA=%trim(%subst(
072700030822     C                             %subst(vlrppt:posDaDft+1:
072800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072900030822     C                   ENDIF
073000030822     C* GGA
073100030822     C                   IF        %subst(
073200030822     C                             %subst(vlrppt:posDaDft+1:
073300030822     C                             posADft-posDaDft-1):1:3)
073400030822     C                             = 'GGA'
073500030822     C                   EVAL      VABGGA=%trim(%subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073800030822     C                   ENDIF
073900030822     C* GVA
074000030822     C                   IF        %subst(
074100030822     C                             %subst(vlrppt:posDaDft+1:
074200030822     C                             posADft-posDaDft-1):1:3)
074300030822     C                             = 'GVA'
074400030822     C                   EVAL      VABGVA=%trim(%subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074700030822     C                   ENDIF
074800030822     C* TC1
074900030822     C                   IF        %subst(
075000030822     C                             %subst(vlrppt:posDaDft+1:
075100030822     C                             posADft-posDaDft-1):1:3)
075200030822     C                             = 'TC1'
075300030822     C                   EVAL      VABTC1=%trim(%subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075600030822     C                   ENDIF
075700030822     C* TC2
075800030822     C                   IF        %subst(
075900030822     C                             %subst(vlrppt:posDaDft+1:
076000030822     C                             posADft-posDaDft-1):1:3)
076100030822     C                             = 'TC2'
076200030822     C                   EVAL      VABTC2=%trim(%subst(
076300030822     C                             %subst(vlrppt:posDaDft+1:
076400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076500030822     C                   ENDIF
076600030822     C* ...
076700030822     C                   ENDIF
076800030822     C                   ENDDO
076900020204     C*
077000010330     C                   ENDSR
077100010607     C*----------------------------------------------------*
077200021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
077300010607     C*----------------------------------------------------*
077400010607     C     IMPVAB        BEGSR
077500010607     C*
077600010607     C                   SETOFF                                       3132
077700030515     C*
077800010607     C                   MOVEL     datcor        VABAAS
077900010607     C                   MOVE      datcor        VABMGS
078000010607     C*
078100030715     C**********
078200030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
078300030715     C**********
078400030715     C                   IF        wGiro = *zeros
078500040113     C*
078600040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
078700040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
078800040510     C                             %trim(intcol(3))<>*blanks
078900040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
079000040113     C                                      %trim(intcol(3))
079100040510     C                   ENDIF
079200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
079300031201     C     minu:maiu     XLATE     vindta        vindta
079400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
079500030715     C                   DOW       posDa <= %len(%trim(vindta))
079600030715     C*
079700030715     C* Gestisco il 1° campo
079800030715     C                   IF        i = 1
079900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
080000030715     C                   EVAL      posDa = 1
080100030715     C* Eseguo lo scan x trovare la fine del primo campo
080200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
080300030715     C* A questo punto "estraggo" il campo corrente
080400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
080500030715     C                                                    (posA-posDa))
080600030715     C* X i campi successivi al 1°
080700030715     C                   ELSE
080800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
080900030715     C                   EVAL      posDa = posA + 1
081000030715     C* Eseguo lo scan x trovare la fine del campo corrente
081100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
081200030715     C* Gestisco l'ultimo campo
081300030715     C                   IF        posA = *zeros
081400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
081500030715     C* A questo punto "estraggo" il campo corrente
081600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
081700030715     C                                                    (posA-posDa))
081800030715     C                   LEAVE
081900030715     C                   ELSE
082000030715     C* A questo punto "estraggo" il campo corrente
082100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082200030715     C                                                    (posA-posDa))
082300030715     C                   ENDIF
082400030715     C                   ENDIF
082500030715     C* Incremento il contatore d campo
082600030715     C                   EVAL      i = i +1
082700030715     C                   ENDDO
082800030715     C*
082900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
083000030715     C* delimitazione testo
083100030715     C                   EVAL      i = 1
083200030715     C                   DOW       i < %elem(SkSplitFLD)
083300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
083400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
083500030715     C                   EVAL      i = i + 1
083600030715     C                   ENDDO
083700030715     C                   EVAL      wGiro = 1
083800030715     C                   ELSE
083900030715     C                   EVAL      wGiro = 2
084000031204     C**********
084100031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
084200031204     C* d separatore campo
084300031204     C**********
084400031204     C                   EVAL      vindta = %trim(vindta)
084500031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
084600031204     C                   EVAL      vindta = %subst(vindta:2)
084700031204     C                   ENDDO
084800031204     C*
084900031204     C                   Z-ADD     *zeros        lunghInput        4 0
085000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
085100031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
085200031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
085300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
085400031204     C                   ENDDO
085500030715     C**********
085600030715     C* Effettuo lo split del campo dati d input
085700030715     C**********
085800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
085900030715     C                   DOW       posDa <= %len(%trim(vindta))
086000030715     C*
086100030715     C* Gestisco il 1° campo
086200030715     C                   IF        i = 1
086300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
086400030715     C                   EVAL      posDa = 1
086500030715     C* Eseguo lo scan x trovare la fine del primo campo
086600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086700030715     C* A questo punto "estraggo" il campo corrente
086800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
086900030715     C                                                    (posA-posDa))
087000030715     C* X i campi successivi al 1°
087100030715     C                   ELSE
087200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
087300030715     C                   EVAL      posDa = posA + 1
087400031204     C* Verifico che nn vi sia il campo nn valorizzato
087500031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
087600031204     C* Se campo nn valorizzato skippo al prossimo
087700031204     C                   EVAL      SkSplitCSV(i) = *blanks
087800031204     C                   EVAL      posA  = posA + 1
087900031204     C                   ELSE
088000030715     C* Eseguo lo scan x trovare la fine del campo corrente
088100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088200030715     C* Gestisco l'ultimo campo
088300030715     C                   IF        posA = *zeros
088400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
088500030715     C* A questo punto "estraggo" il campo corrente
088600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
088700030715     C                                                    (posA-posDa))
088800030715     C                   LEAVE
088900030715     C                   ELSE
089000030715     C* A questo punto "estraggo" il campo corrente
089100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089200030715     C                                                    (posA-posDa))
089300030715     C                   ENDIF
089400031204     C                   ENDIF
089500030715     C                   ENDIF
089600030715     C* Incremento il contatore d campo
089700030715     C                   EVAL      i = i +1
089800030715     C                   ENDDO
089900030715     C*
090000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
090100030715     C* delimitazione testo
090200030715     C                   EVAL      i = 1
090300030715     C                   DOW       i < %elem(SkSplitCSV)
090400030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
090500030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
090600030715     C                   EVAL      i = i + 1
090700030715     C                   ENDDO
090800030715     C*
090900030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
091000030715     C                   EVAL      i = 1
091100030715     C                   DOW       i < %elem(SkSplitFLD)
091200030715     C*
091300030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
091400030715     C                   IF        SkSplitFLD(i) <> *blanks AND
091500030715     C                             SkSplitCSV(i) <> *blanks
091600030721     C*** ATB
091700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
091800030721     C                   EVAL      VABATB = SkSplitCSV(i)
091900030721     C                   ENDIF
092000030715     C*** RSD
092100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
092200030715     C                   EVAL      VABRSD = SkSplitCSV(i)
092300030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
092400030715     C     '@':'A'       XLATE     VABRSD        VABRSD
092500030715     C* ==
092600030715     C                   ENDIF
092700030721     C*** RD2
092800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
092900030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
093000030721     C                   ENDIF
093100030715     C*** IND
093200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
093300030715     C                   EVAL      VABIND = SkSplitCSV(i)
093400030715     C                   ENDIF
093500030715     C*** LOD
093600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
093700030715     C                   EVAL      VABLOD = SkSplitCSV(i)
093800030715     C                   ENDIF
093900030715     C*** PRD
094000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
094100030715     C                   EVAL      VABPRD = SkSplitCSV(i)
094200030715     C                   ENDIF
094300030721     C*** NZD
094400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
094500030721     C                   EVAL      VABNZD = SkSplitCSV(i)
094600030721     C                   ENDIF
094700030715     C*** RMA
094800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
094900030715     C                   EVAL      VABRMA = SkSplitCSV(i)
095000030715     C                   ENDIF
095100030715     C*** NOT
095200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
095300030715     C                   EVAL      VABNOT = SkSplitCSV(i)
095400030715     C                   ENDIF
095500030715     C*** NT2
095600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
095700030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
095800030715     C                   ENDIF
095900030715     C*** VCA
096000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
096100030715     C                   EVAL      VABVCA = SkSplitCSV(i)
096200030715     C                   ENDIF
096300030715     C*** VAS
096400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
096500030715     C                   EVAL      VABVAS = SkSplitCSV(i)
096600030715     C                   ENDIF
096700030715     C*** FFD
096800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
096900030715     C                   EVAL      VABFFD = SkSplitCSV(i)
097000030715     C                   ENDIF
097100030721     C*** GC1
097200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
097300030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
097400030721     C                   ENDIF
097500030721     C*** GC2
097600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
097700030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
097800030721     C                   ENDIF
097900030721     C*** TSP
098000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
098100030721     C                   EVAL      VABTSP = SkSplitCSV(i)
098200030721     C                   ENDIF
098300040510     C*** CBO
098400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
098500040510     C                   EVAL      VABCBO = SkSplitCSV(i)
098600040510     C                   ENDIF
098700030721     C*** NAS
098800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
098900030721     C                   EVAL      VABNAS = SkSplitCSV(i)
099000030721     C                   ENDIF
099100030721     C*** TIC
099200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
099300030721     C                   EVAL      VABTIC = SkSplitCSV(i)
099400030721     C                   ENDIF
099500030721     C*** GCA
099600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
099700030721     C                   EVAL      VABGCA = SkSplitCSV(i)
099800030721     C                   ENDIF
099900030721     C*** XCO
100000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
100100030721     C                   EVAL      VABXCO = SkSplitCSV(i)
100200030721     C                   ENDIF
100300030721     C*** CTM
100400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
100500030721     C                   EVAL      VABCTM = SkSplitCSV(i)
100600030721     C                   ENDIF
100700030721     C*** TCR
100800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
100900030721     C                   EVAL      VABTCR = SkSplitCSV(i)
101000030721     C                   ENDIF
101100030721     C*** CTS
101200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
101300030721     C                   EVAL      VABCTS = SkSplitCSV(i)
101400030721     C                   ENDIF
101500030721     C*** FTM
101600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
101700030721     C                   EVAL      VABFTM = SkSplitCSV(i)
101800030721     C                   ENDIF
101900030721     C*** VAD
102000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
102100030721     C                   EVAL      VABVAD = SkSplitCSV(i)
102200030721     C                   ENDIF
102300030721     C*** GMA
102400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
102500030721     C                   EVAL      VABGMA = SkSplitCSV(i)
102600030721     C                   ENDIF
102700030721     C*** GGA
102800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
102900030721     C                   EVAL      VABGGA = SkSplitCSV(i)
103000030721     C                   ENDIF
103100030721     C*** GVA
103200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
103300030721     C                   EVAL      VABGVA = SkSplitCSV(i)
103400030721     C                   ENDIF
103500030721     C*** TC1
103600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
103700030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
103800030721     C                   ENDIF
103900030721     C*** TC2
104000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
104100030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
104200030721     C                   ENDIF
104300030721     C*** SCL
104400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
104500030721     C                   EVAL      VABSCL = SkSplitCSV(i)
104600030721     C                   ENDIF
104700030721     C*** RMO
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
104900030721     C                   EVAL      VABRMO = SkSplitCSV(i)
105000030721     C                   ENDIF
105100030721     C*** NMO
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
105300030721     C                   EVAL      VABNMO = SkSplitCSV(i)
105400030721     C                   ENDIF
105500030721     C*
105600030721     C* Reperisco quindi i campi numerici...
105700040510     C*** NSP
105800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
105900040510     C                   EVAL      PiStr=SkSplitCSV(i)
106000040510     C                   EXSR      CHKNUM
106100040510     C                   IF        PiInt=*on
106200040510     C                   Z-ADD     PiVal         VABNSP
106300040510     C                   ELSE
106400040510     C                   SETON                                        31
106500040510     C                   EVAL      VABNSP = *zeros
106600040510     C                   EVAL      vinmsg = %trimr(vinmsg)
106700040510     C                             + ' ' + 'VABNSP'
106800040510     C                   ENDIF
106900040510     C                   ENDIF
107000030721     C*** CAD
107100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
107200030721     C                   EVAL      PiStr=SkSplitCSV(i)
107300030721     C                   EXSR      CHKNUM
107400030721     C                   IF        PiInt=*on
107500030721     C                   Z-ADD     PiVal         Num5_0
107600030721     C                   MOVEL(p)  Num5_0        VABCAD
107700030721     C                   ELSE
107800030721     C                   SETON                                        32
107900030721     C                   EVAL      VABCAD = *zeros
108000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108100030721     C                             + ' ' + 'VABCAD'
108200030721     C                   ENDIF
108300030721     C                   ENDIF
108400030721     C*** NCL
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
108600030721     C                   EVAL      PiStr=SkSplitCSV(i)
108700030721     C                   EXSR      CHKNUM
108800030721     C                   IF        PiInt=*on
108900030721     C                   Z-ADD     PiVal         VABNCL
109000030721     C                   ELSE
109100030721     C                   SETON                                        32
109200030721     C                   EVAL      VABNCL = *zeros
109300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109400040122     C                             + ' ' + 'VABNCL'
109500030721     C                   ENDIF
109600030721     C                   ENDIF
109700030721     C*** PKB
109800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
109900030721     C                   EVAL      PiStr=SkSplitCSV(i)
110000030721     C                   EXSR      CHKNUM
110100030721     C                   IF        PiNum=*on
110200030721     C                   Z-ADD     PiVal         VABPKB
110300030721     C                   ELSE
110400030721     C                   SETON                                        32
110500030721     C                   EVAL      VABPKB = *zeros
110600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110700030721     C                             + ' ' + 'VABPKB'
110800030721     C                   ENDIF
110900030721     C                   ENDIF
111000030721     C*** CAS
111100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
111200030721     C                   EVAL      PiStr=SkSplitCSV(i)
111300030721     C                   EXSR      CHKNUM
111400030721     C                   IF        PiNum=*on
111500030721     C                   Z-ADD     PiVal         VABCAS
111600030721     C                   ELSE
111700030721     C                   SETON                                        32
111800030721     C                   EVAL      VABCAS = *zeros
111900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112000030721     C                             + ' ' + 'VABCAS'
112100030721     C                   ENDIF
112200030721     C                   ENDIF
112300030721     C*** CCM
112400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
112500030721     C                   EVAL      PiStr=SkSplitCSV(i)
112600030721     C                   EXSR      CHKNUM
112700030721     C                   IF        PiInt=*on      AND
112800030721     C                             PiVal<=9999999 AND
112900030721     C                             PiVal>*zeros
113000030721     C                   Z-ADD     PiVal         VABCCM
113100030721     C                   ELSE
113200030721     C                   SETON                                        32
113300030721     C                   EVAL      VABCCM = *zeros
113400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113500040510     C                             + ' ' + 'VABCCM'
113600030721     C                   ENDIF
113700030721     C                   ENDIF
113800030721     C*** LNP
113900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
114000030721     C                   EVAL      PiStr=SkSplitCSV(i)
114100030721     C                   EXSR      CHKNUM
114200030721     C                   IF        PiInt=*on    AND
114300030721     C                             PiVal<=999   AND
114400030721     C                             PiVal>*zeros
114500030721     C                   Z-ADD     PiVal         VABLNP
114600030721     C                   ELSE
114700040715     C                   SETON                                        32
114800030721     C                   EVAL      VABLNP = *zeros
114900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115000040510     C                             + ' ' + 'VABLNP'
115100030721     C                   ENDIF
115200030721     C                   ENDIF
115300030721     C*** NRS
115400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
115500030721     C                   EVAL      PiStr=SkSplitCSV(i)
115600030721     C                   EXSR      CHKNUM
115700030721     C                   IF        PiInt=*on AND
115800030721     C                             PiVal<=99
115900030721     C                   Z-ADD     PiVal         VABNRS
116000030721     C                   ELSE
116100030721     C                   SETON                                        32
116200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116300030721     C                             + ' ' + 'VABNRS'
116400030721     C                   ENDIF
116500030721     C                   ENDIF
116600030721     C*** LNA
116700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
116800030721     C                   EVAL      PiStr=SkSplitCSV(i)
116900030721     C                   EXSR      CHKNUM
117000030721     C                   IF        PiInt=*on    AND
117100030721     C                             PiVal<=999   AND
117200030721     C                             PiVal>*zeros
117300030721     C                   Z-ADD     PiVal         VABLNA
117400030721     C                   ELSE
117500030721     C                   SETON                                        32
117600030721     C                   EVAL      VABLNA = *zeros
117700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117800030721     C                             + ' ' + 'VABLNA'
117900030721     C                   ENDIF
118000030721     C                   ENDIF
118100030721     C*** CTR
118200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
118300030721     C                   EVAL      PiStr=SkSplitCSV(i)
118400030721     C                   EXSR      CHKNUM
118500030721     C                   IF        PiInt=*on  AND
118600030721     C                             PiVal<=999
118700030721     C                   Z-ADD     PiVal         VABCTR
118800030721     C                   ELSE
118900030721     C                   SETON                                        32
119000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119100030721     C                             + ' ' + 'VABCTR'
119200030721     C                   ENDIF
119300030721     C                   ENDIF
119400030721     C*** IAS
119500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
119600030721     C                   EVAL      PiStr=SkSplitCSV(i)
119700030721     C                   EXSR      CHKNUM
119800030721     C                   IF        PiNum=*on
119900030721     C                   Z-ADD     PiVal         VABIAS
120000030721     C                   ELSE
120100030721     C                   SETON                                        32
120200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120300030721     C                             + ' ' + 'VABIAS'
120400030721     C                   ENDIF
120500030721     C                   ENDIF
120600030721     C*** VLB
120700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
120800030721     C                   EVAL      PiStr=SkSplitCSV(i)
120900030721     C                   EXSR      CHKNUM
121000030721     C                   IF        PiNum=*on
121100030721     C                   Z-ADD     PiVal         VABVLB
121200030721     C                   ELSE
121300030721     C                   SETON                                        32
121400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121500030721     C                             + ' ' + 'VABVLB'
121600030721     C                   ENDIF
121700030721     C                   ENDIF
121800030721     C*** QFT
121900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
122000030721     C                   EVAL      PiStr=SkSplitCSV(i)
122100030721     C                   EXSR      CHKNUM
122200030721     C                   IF        PiNum=*on
122300030721     C                   Z-ADD     PiVal         VABQFT
122400030721     C                   ELSE
122500030721     C                   SETON                                        32
122600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122700030721     C                             + ' ' + 'VABQFT'
122800030721     C                   ENDIF
122900030721     C                   ENDIF
123000030721     C*** NCD
123100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
123200030721     C                   EVAL      PiStr=SkSplitCSV(i)
123300030721     C                   EXSR      CHKNUM
123400030721     C                   IF        PiInt=*on      AND
123500030721     C                             PiVal<=9999999
123600030721     C                   Z-ADD     PiVal         VABNCD
123700030721     C                   ELSE
123800030721     C                   SETON                                        32
123900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124000030721     C                             + ' ' + 'VABNCD'
124100030721     C                   ENDIF
124200030721     C                   ENDIF
124300030721     C*** NCA
124400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
124500030721     C                   EVAL      PiStr=SkSplitCSV(i)
124600030721     C                   EXSR      CHKNUM
124700030721     C                   IF        PiInt=*on      AND
124800030721     C                             PiVal<=9999999
124900030721     C                   Z-ADD     PiVal         VABNCA
125000030721     C                   ELSE
125100030721     C                   SETON                                        32
125200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125300030721     C                             + ' ' + 'VABNCA'
125400030721     C                   ENDIF
125500030721     C                   ENDIF
125600030721     C*** ZNC
125700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
125800030721     C                   EVAL      PiStr=SkSplitCSV(i)
125900030721     C                   EXSR      CHKNUM
126000030721     C                   IF        PiInt=*on AND
126100030721     C                             PiVal<=99
126200030721     C                   Z-ADD     PiVal         VABZNC
126300030721     C                   ELSE
126400030721     C                   SETON                                        32
126500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126600030721     C                             + ' ' + 'VABZNC'
126700030721     C                   ENDIF
126800030721     C                   ENDIF
126900030721     C*** DCR
127000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
127100030721     C                   EVAL      PiStr=SkSplitCSV(i)
127200030721     C                   EXSR      CHKNUM
127300030721     C                   IF        PiInt=*on       AND
127400030721     C                             PiVal<=99999999
127500030721     C                   Z-ADD     PiVal         VABDCR
127600030721     C                   ELSE
127700030721     C                   SETON                                        32
127800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127900030721     C                             + ' ' + 'VABDCR'
128000030721     C                   ENDIF
128100030721     C                   ENDIF
128200030721     C*** HCR
128300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
128400030721     C                   EVAL      PiStr=SkSplitCSV(i)
128500030721     C                   EXSR      CHKNUM
128600030721     C                   IF        PiInt=*on   AND
128700030721     C                             PiVal<=9999
128800030721     C                   Z-ADD     PiVal         VABHCR
128900030721     C                   ELSE
129000030721     C                   SETON                                        32
129100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129200030721     C                             + ' ' + 'VABHCR'
129300030721     C                   ENDIF
129400030721     C                   ENDIF
129500030721     C*** VMD
129600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
129700030721     C                   EVAL      PiStr=SkSplitCSV(i)
129800030721     C                   EXSR      CHKNUM
129900030721     C                   IF        PiNum=*on
130000030721     C                   Z-ADD     PiVal         VABVMD
130100030721     C                   ELSE
130200030721     C                   SETON                                        32
130300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130400030721     C                             + ' ' + 'VABVMD'
130500030721     C                   ENDIF
130600030721     C                   ENDIF
130700030721     C*** ANT
130800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
130900030721     C                   EVAL      PiStr=SkSplitCSV(i)
131000030721     C                   EXSR      CHKNUM
131100030721     C                   IF        PiInt=*on        AND
131200030721     C                             PiVal<=999999999
131300030721     C                   Z-ADD     PiVal         VABANT
131400030721     C                   ELSE
131500030721     C                   SETON                                        32
131600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131700030721     C                             + ' ' + 'VABANT'
131800030721     C                   ENDIF
131900030721     C                   ENDIF
132000030721     C*** CMO
132100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
132200030721     C                   EVAL      PiStr=SkSplitCSV(i)
132300030721     C                   EXSR      CHKNUM
132400030721     C                   IF        PiInt=*on
132500030721     C                   Z-ADD     PiVal         Num5_0
132600030721     C                   MOVEL(p)  Num5_0        VABCMO
132700030721     C                   ELSE
132800030721     C                   SETON                                        32
132900030721     C                   EVAL      VABCMO = *zeros
133000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133100030721     C                             + ' ' + 'VABCMO'
133200030721     C                   ENDIF
133300030721     C                   ENDIF
133400040510     C*
133500070117     C* Reperisco i campi del FIVAD
133600070117     C*** VADCDP_1
133700070117     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_1'
133800070117     C                   EVAL      wVADCDP_1 = %trim(SkSplitCSV(i))
133900060331     C                   ENDIF
134000070117     C*** VADCDP_2
134100070117     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_2'
134200070117     C                   EVAL      wVADCDP_2 = %trim(SkSplitCSV(i))
134300070117     C                   ENDIF
134400070117     C*** VADCDP_3
134500070117     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_3'
134600070117     C                   EVAL      wVADCDP_3 = %trim(SkSplitCSV(i))
134700070117     C                   ENDIF
134800070117     C*** VADCDP_4
134900070117     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_4'
135000070117     C                   EVAL      wVADCDP_4 = %trim(SkSplitCSV(i))
135100070117     C                   ENDIF
135200070117     C*** VADCDP_5
135300070117     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP_5'
135400070117     C                   EVAL      wVADCDP_5 = %trim(SkSplitCSV(i))
135500070117     C                   ENDIF
135600070117     C*** VADNCL_1
135700070117     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_1'
135800070117     C                   EVAL      PiStr=SkSplitCSV(i)
135900070117     C                   EXSR      CHKNUM
136000070117     C                   IF        PiInt=*on
136100070117     C                   Z-ADD     PiVal         wVADNCL_1
136200070117     C                   ELSE
136300070117     C                   SETON                                        32
136400070117     C                   EVAL      wVADNCL_1 = *zeros
136500070117     C                   EVAL      vinmsg = %trimr(vinmsg)
136600070117     C                             + ' ' + 'VADNCL_1'
136700070117     C                   ENDIF
136800070117     C                   ENDIF
136900070117     C*** VADNCL_2
137000070117     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_2'
137100070117     C                   EVAL      PiStr=SkSplitCSV(i)
137200070117     C                   EXSR      CHKNUM
137300070117     C                   IF        PiInt=*on
137400070117     C                   Z-ADD     PiVal         wVADNCL_2
137500070117     C                   ELSE
137600070117     C                   SETON                                        32
137700070117     C                   EVAL      wVADNCL_2 = *zeros
137800070117     C                   EVAL      vinmsg = %trimr(vinmsg)
137900070117     C                             + ' ' + 'VADNCL_2'
138000070117     C                   ENDIF
138100070117     C                   ENDIF
138200070117     C*** VADNCL_3
138300070117     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_3'
138400070117     C                   EVAL      PiStr=SkSplitCSV(i)
138500070117     C                   EXSR      CHKNUM
138600070117     C                   IF        PiInt=*on
138700070117     C                   Z-ADD     PiVal         wVADNCL_3
138800070117     C                   ELSE
138900070117     C                   SETON                                        32
139000070117     C                   EVAL      wVADNCL_3 = *zeros
139100070117     C                   EVAL      vinmsg = %trimr(vinmsg)
139200070117     C                             + ' ' + 'VADNCL_3'
139300070117     C                   ENDIF
139400070117     C                   ENDIF
139500070117     C*** VADNCL_4
139600070117     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_4'
139700070117     C                   EVAL      PiStr=SkSplitCSV(i)
139800070117     C                   EXSR      CHKNUM
139900070117     C                   IF        PiInt=*on
140000070117     C                   Z-ADD     PiVal         wVADNCL_4
140100070117     C                   ELSE
140200070117     C                   SETON                                        32
140300070117     C                   EVAL      wVADNCL_4 = *zeros
140400070117     C                   EVAL      vinmsg = %trimr(vinmsg)
140500070117     C                             + ' ' + 'VADNCL_4'
140600070117     C                   ENDIF
140700070117     C                   ENDIF
140800070117     C*** VADNCL_5
140900070117     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL_5'
141000070117     C                   EVAL      PiStr=SkSplitCSV(i)
141100070117     C                   EXSR      CHKNUM
141200070117     C                   IF        PiInt=*on
141300070117     C                   Z-ADD     PiVal         wVADNCL_5
141400070117     C                   ELSE
141500070117     C                   SETON                                        32
141600070117     C                   EVAL      wVADNCL_5 = *zeros
141700070117     C                   EVAL      vinmsg = %trimr(vinmsg)
141800070117     C                             + ' ' + 'VADNCL_5'
141900070117     C                   ENDIF
142000070117     C                   ENDIF
142100040510     C*
142200030715     C                   ENDIF
142300030715     C                   EVAL      i = i + 1
142400030715     C                   ENDDO
142500040510     C*
142600040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
142700040510     C                   IF        VABNSP = *zeros
142800040510     C                   SETON                                        31
142900040510     C                   ENDIF
143000040715     C*
143100040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
143200040715     C                   IF        VABLNP = *zeros
143300040715     C                   SETON                                        31
143400040715     C                   ENDIF
143500040510     C*
143600040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
143700040510     C                   IF        VABRMN = *zeros
143800040510     C                   Z-ADD     VABNSP        VABRMN
143900040510     C                   ENDIF
144000040510     C*
144100040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
144200040510     C                   IF        VABRMA = *blanks
144300040714     C                   MOVEL     VABRMN        VABRMA
144400040510     C                   ENDIF
144500040128     C*
144600040510     C* Se provincia nn valorizzata la reperisco
144700040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
144800040510     C                   IF        VABPRD = *blanks
144900040128     C                   CLEAR                   TISI95DS
145000040128     C                   EVAL      I95TCN = '3'
145100040128     C                   Z-ADD     datcor        I95DAT
145200040128     C                   EVAL      I95NAR = VABNZD
145300040128     C                   EVAL      I95CAP = VABCAD
145400040128     C                   EVAL      I95LOC = VABLOD
145500040128     C                   CALL      'TISI95R'
145600040128     C                   PARM                    TISI95DS
145700040128     C                   EVAL      VABPRD = O95PRV
145800040510     C                   ENDIF
145900040113     C*
146000040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
146100040113     C                   IF        VABCAS > *zeros
146200040119     C                   EVAL      VABVCA = 'EUR'
146300040119     C                   IF        VABCBO = '1'
146400040113     C                   EVAL      VABCBO = '4'
146500040119     C                   ENDIF
146600040119     C                   IF        VABCBO = '2'
146700040119     C                   EVAL      VABCBO = '6'
146800040119     C                   ENDIF
146900040113     C                   ENDIF
147000020204     C*
147100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
147200020204     C                   EXSR      CHKIMPDIV
147300030715     C                   ENDIF
147400020204     C*
147500010607     C                   ENDSR
147600010607     C*----------------------------------------------------*
147700010601
147800020204
147900020204     C*----------------------------------------------------*
148000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
148100020204     C*----------------------------------------------------*
148200020204     C     CHKIMPDIV     BEGSR
148300020204     C*
148400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
148500020204     C                   Z-ADD     *zeros        wrkDec            9 9
148600020204     C*
148700020204     C* Come prima cosa effettuo considerazioni sulla divisa
148800020204     C                   IF        vabIAS > *zeros
148900020204     C                   IF        vabVAS <> 'EUR'
149000020204     C                   EVAL      vabVAS =  'ITL'
149100020204     C                   ENDIF
149200020204     C                   ENDIF
149300020204     C*
149400020204     C                   IF        vabCAS > *zeros
149500020204     C                   IF        vabVCA <> 'EUR'
149600020204     C                   EVAL      vabVCA =  'ITL'
149700020204     C                   ENDIF
149800020204     C                   ENDIF
149900020204     C*
150000020204     C                   IF        vabVMD > *zeros
150100020204     C                   IF        vabVAD <> 'EUR'
150200020204     C                   EVAL      vabVAD =  'ITL'
150300020204     C                   ENDIF
150400020204     C                   ENDIF
150500020204     C*
150600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
150700020204     C                   Z-ADD     vabIAS        wrkDec
150800020204     C                   IF        wrkDec > *zeros
150900020204     C                   IF        vabVAS = 'ITL'
151000020204     C                   EVAL      vabIAS = *zeros
151100020204     C                   ENDIF
151200020204     C                   ENDIF
151300020204     C*
151400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
151500020204     C                   Z-ADD     vabCAS        wrkDec
151600020204     C                   IF        wrkDec > *zeros
151700020204     C                   IF        vabVCA = 'ITL'
151800020204     C                   EVAL      vabCAS = *zeros
151900020204     C                   ENDIF
152000020204     C                   ENDIF
152100020204     C*
152200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
152300020204     C                   Z-ADD     vabVMD        wrkDec
152400020204     C                   IF        wrkDec > *zeros
152500020204     C                   IF        vabVAD = 'ITL'
152600020204     C                   EVAL      vabVMD = *zeros
152700020204     C                   ENDIF
152800020204     C                   ENDIF
152900020204     C*
153000020204     C                   ENDSR
153100020204     C***
153200020204
153300010330
153400010330
153500010330     C*----------------------------------------------------*
153600010330     C*  CONTROLLO NUMERICITA' CAMPI
153700010330     C*----------------------------------------------------*
153800010330     C     CHKNUM        BEGSR
153900010330     C*
154000010606     C                   IF        PiDecChr = *blanks
154100030715     C                   EVAL      PiDecChr = CharNUM
154200010606     C                   ENDIF
154300010606     C*
154400010606     C                   CALL(e)   'ISNUMERIC'
154500010330     C                   PARM                    PiStr            30
154600010606     C                   PARM                    PiDecChr          1
154700010330     C                   PARM      *ZEROS        PiVal            30 9
154800010330     C                   PARM      '0'           PiInt             1
154900010330     C                   PARM      '0'           PiNum             1
155000010330     C                   IF        %error
155100010606     C                   EVAL      PiNum=*off
155200010330     C                   ENDIF
155300010330     C*
155400010330     C                   ENDSR
155500010330     C***
155600010330
155700010601
155800010601
155900010601
156000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
156100010601     C     repfil        BEGSR
156200010601     C*
156300010601     C                   if        invfil = *zeros and
156400010601     C                             depfil > *zeros and
156500010629     C                             (vinflg = *blanks or
156600010629     C                              vinflg = *zeros)
156700010601     C
156800010601     C                   eval      invfil = depfil
156900010601     C                   endif
157000010601     C*
157100010601     C                   if        depfil <> invfil and
157200010601     C                             invfil > *zeros
157300010601     C                   eval      flgMulti = '1'
157400010601     C                   if        vinflg = *blanks
157500010601     C                   add       1             cntNonEl
157600010601     C                   endif
157700010601     C                   endif
157800010601     C*
157900010601     C                   if        vinflg = '2'
158000010601     C                   eval      flgStato = '2'
158100010601     C                   endif
158200010601     C*
158300010601     C                   ENDSR
158400010601     C***
158500010601
158600010601
158700010601
158800010330
158900040119      /TITLE Invio dei dati al punto operativo.
159000040119     C     invio         BEGSR
159100040119     C*
159200070117     C* 1° invio FIVAD
159300040119     C                   reset                   dscmz
159400040119     C                   move      vlrpoi        cmzdst
159500070117     C                   eval      cmzfld = 'FIVADWWR'
159600040119     C                   eval      cmzmbd = vlrhdl
159700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
159800040119     C***                if        prmfir = *blanks
159900070117     C                   eval      cmzfla = 'FIVAD00F'
160000070117     C                   eval      cmzmba = 'FIVAD00F'
160100040119     C***                else
160200040119     C***                eval      cmzfla = prmfir
160300040119     C***                eval      cmzmba = prmfir
160400040119     C***                endif
160500040119     C                   eval      cmznrr = *zeros
160600070117     C                   move      §ctrokvd      cmznrr
160700040119     C                   eval      cmzlba = vlrfl1
160800040119     C                   call(e)   'TIS711C'
160900040119     C                   parm                    dscmz
161000040119     C                   parm      *blanks       esito
161100040119     C                   if        %error
161200040119     C                             or cmzerr = '1'
161300040119     C                             or esito  = '1'
161400040119     C                   eval      wrkesito = '3'
161500040119     C                   else
161600040119     C*
161700040119     C* 2° invio FIVAB
161800040119     C                   reset                   dscmz
161900040119     C                   move      vlrpoi        cmzdst
162000040119     C                   eval      cmzfld = vlrfou
162100040119     C                   eval      cmzmbd = vlrhdl
162200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
162300040119     C***                if        prmfir = *blanks
162400040119     C                   eval      cmzfla = 'FIVAB00F'
162500040119     C                   eval      cmzmba = 'FIVAB00F'
162600040119     C***                else
162700040119     C***                eval      cmzfla = prmfir
162800040119     C***                eval      cmzmba = prmfir
162900040119     C***                endif
163000040119     C                   eval      cmznrr = *zeros
163100040119     C                   move      §ctrokvb      cmznrr
163200040119     C                   eval      cmzlba = vlrfl1
163300040119     C                   call(e)   'TIS711C'
163400040119     C                   parm                    dscmz
163500040119     C                   parm      *blanks       esito
163600040119     C                   if        %error
163700040119     C                             or cmzerr = '1'
163800040119     C                             or esito  = '1'
163900040119     C                   eval      wrkesito = '3'
164000040119     C                   endif
164100040119     C                   endif
164200040119     C*
164300040119     C                   ENDSR
164400040119     C***
164500010601
164600010601
164700010601
164800010601
164900010601      /TITLE Invio dei dati al punto operativo.
165000010601     C     opeini        BEGSR
165100010601     C*
165200010601     C* Inizializzo flag e contatori operativi
165300010601     C                   movel     '0'           flgGiro           1
165400010601     C                   movel     '0'           flgMulti          1
165500010601     C                   movel     '1'           flgStato          1
165600010615     C                   movel     '0'           flgOk             1
165700010601     C                   z-add     *zeros        cntNonEl         10 0
165800010601     C                   z-add     *zeros        depfil            3 0
165900010601     C                   z-add     *zeros        invfil            3 0
166000010601     C*
166100010601     C                   ENDSR
166200010601     C***
166300010601
166400010601
166500050421
166600081006     C     *pssr         BEGSR
166700081006     C*
166800081006     C                   if        %open(tivin00r)
166900081006     C                   close     tivin00r
167000081006     C                   endif
167100081006     C                   if        %open(fivabwwr)
167200081006     C                   close     fivabwwr
167300081006     C                   endif
167400081006     C                   if        %open(fivadwwr)
167500081006     C                   close     fivadwwr
167600081006     C                   endif
167700081006     C*
167800081006     C* Effettuo la chiamata al CLLE preposto
167900081006     C                   call(e)   'TITVVDC'
168000081006     C                   parm                    parccm
168100081006     C                   parm                    parmbr
168200081006     C                   parm      '2'           paropz
168300081006     C*
168400050421     C                   eval      wrkesito = '2'
168500050421     C*
168600050421     C                   ENDSR     '*CANCL'
168700050421     C***
168800050421
168900010601
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
170100010330     C                   time                    wn14             14 0
170200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
170300010330     C                   z-add     wn8           g08dat
170400010330     C                   z-add     *zeros        g08inv
170500010330     C                   movel     '0'           g08err
170600010330     C                   call      'XSRDA8'
170700010330     C                   parm                    wlbda8
170800010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
170900000613     C*
171000000613     C                   ENDSR
171100000613     C***
171200990908
171300040122     Otitv92p   E            riepilogo         2
171400990915     O                                              'Upload via Internet'
171500990915     O                                           +1 'Traduzione TIVIN00R -'
171600021025     O                                           +1 'FIVABWWR'
171700990915     O                                           +1 'Testate bolle'
171800990915     O          E            riepilogo   2
171900990915     O                       wrkdata
172000990915     O                       wrkora              +1
172100990915     O                       procname            +1
172200990915     O          E            riepilogo   2
172300990915     O                                              'Cliente..................:'
172400010605     O                       VABCCM        z     +1
172500990915     O          E            riepilogo   2
172600990920     O                                              'Riferimento Strategi.....:'
172700990920     O                       vlrhdl              +1
172800990915     O          E            riepilogo   2
172900990915     O                                              'Giusti...................:'
173000040119     O                       §CTROKVB      2   +  1
173100990915     O          E            riepilogo   2
173200990915     O                                              'Sbagliati e corretti.....:'
173300971022     O                       §CTRMO        2   +  1
173400990915     O          E            riepilogo   2
173500990915     O                                              'Sbagliati e scartati.....:'
173600971022     O                       §CTRNO        2   +  1
173700000613
173800040122     Otitv92ps  E            testdett          2
173900000613     O                                              'Upload via Internet'
174000000613     O                                           +1 'Traduzione TIVIN00R -'
174100021025     O                                           +1 'FIVABWWR'
174200000613     O                                           +1 'Testate bolle'
174300000616     O          E            testdett    3
174400000613     O                                           +2 'N° rec'
174500000613     O                                           +3 'Anteprima contenuto'
174600000616     O          E            rigadett    2
174700000613     O                       rrnum               +2
174800000621     O                       recko               +3
174900000616     O          E            findett     2
175000000613     O                       wrkdata
175100000613     O                       wrkora              +1
175200000613     O                       procname            +1
175300000616     O          E            findett     2
175400000613     O                                              'Cliente..................:'
175500010605     O                       VABCCM        z     +1
175600000616     O          E            findett     2
175700000613     O                                              'Riferimento Strategi.....:'
175800000613     O                       vlrhdl              +1
175900000616     O          E            findett     2
176000000613     O                                              'Giusti...................:'
176100040119     O                       §CTROKVB      2   +  1
176200000616     O          E            findett     2
176300000613     O                                              'Sbagliati e corretti.....:'
176400000613     O                       §CTRMO        2   +  1
176500000616     O          E            findett     2
176600000613     O                                              'Sbagliati e scartati.....:'
176700000613     O                       §CTRNO        2   +  1
176800000613** CMD - COMANDI CL
176900040122OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV92P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
177000040122OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV92PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
177100040122DLTOVR FILE(TITV92P TITV92PS) LVL(*)
177200010606
177300000613
177400040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
177500040510
177600040122
177700040113
