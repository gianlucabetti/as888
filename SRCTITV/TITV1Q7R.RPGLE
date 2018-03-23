000100090623      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR/FIVAXWWR.
000200080617     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040119     FFIVATwwr  O    E             DISK    usropn
000800090623     FFIVAXwwr  O    E             DISK    usropn
000900090623     Ftitv1q7p  O    f  132        PRINTER usropn
001000000621     F                                     oflind(*inoa)
001100080916     F                                     infsr(*pssr)
001200090623     Ftitv1q7ps O    f  198        PRINTER usropn
001300000621     F                                     oflind(*inob)
001400080916     F                                     infsr(*pssr)
001500000313     D*
001600000613     D*------------
001700000613     D* COMANDI
001800000613     D*------------
001900011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
002000040113     D*------------
002100040113     D* SCHIEDE A PROGRAMMA
002200040113     D*------------
002300090623     D entexp          S            100    DIM(5) CTDATA PERRCD(1)
002400090623     D ligexp          S            100    DIM(5) CTDATA PERRCD(1)
002500090624     D prtconst        S            100    DIM(5) CTDATA PERRCD(1)
002600010330     D*----------------------------------------------------
002700010330     D* DICHIARAZIOINE VARIABILI DI WRK
002800010330     D*----------------------------------------------------
002900010330     D dscmz         e ds                  inz
003000010330     D psds           sds
003100010330     D  procname         *PROC
003200010330     D tivlrds       e ds                  extname(tivlr00f)
003300040128     D tisi95ds      e ds
003400010330     D esito           s              1
003500010330     D prmlit          s             10
003600010330     D prmfir          s             10
003700010330     D wrkesito        s                   like(esito)
003800010330     D wrkdata         s               d
003900010330     D wrkora          s               t
004000010330     D rrnum           s              6  0 INZ(*zeros)
004100010330     D recko           s            150    INZ(*blanks)
004200011113     D depcmd          s            150    INZ(*blanks)
004300040119     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004400040510     D parccm          s              8    INZ(*blanks)
004500040119     D parmbr          s             10    INZ(*blanks)
004600040119     D paropz          s              1    INZ(*blanks)
004700040119     D chkcall         s              1    INZ(*blanks)
004800080125     D wFlgCAS         s              1    INZ(*blanks)
004900080716     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
005000030715     D*------------------
005100030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005200030715     D*------------------
005300090624     D SkSplitFLD      S             15    DIM(100)
005400030715     D SkSplitCSV      S            256    DIM(100)
005500030715     D CharCSV         S              1
005600030715     D CharTXT         S              1
005700030715     D CharNUM         S              1
005800030715     D posDa           S              3  0 INZ(*zeros)
005900030715     D posA            S              3  0 INZ(*zeros)
006000030715     D i               s              3  0 INZ(1)
006100030715     D wGiro           s              1  0 INZ(*zeros)
006200080125     D wVATNOT_A       s                   LIKE(VATNOT)
006300080125     D wVATNOT_B       s                   LIKE(VATNOT)
006400090904     D wVATNOT_E       s                   LIKE(VATNOT)
006500090904     D wVATNOT_E_1     s                   LIKE(VATNOT)
006600090904     D wVATNOT_E_2     s                   LIKE(VATNOT)
006700030822     D*------------------
006800030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006900030822     D*------------------
007000030822     D posDaDft        S              3  0 INZ(*zeros)
007100030822     D posADft         S              3  0 INZ(*zeros)
007200030822     D j               s              3  0 INZ(1)
007300030822     D wGiroDft        s              1  0 INZ(*zeros)
007400010330     D*------------------
007500010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
007600010330     D*------------------
007700010330     D WLBDA8          DS                  INZ
007800010330     D  G08DAT                 1      8  0
007900010330     D  G08INV                 9     16  0
008000010330     D  G08ERR                17     17
008100010330     D  G08TGI                18     22  0
008200031201     D*------------------
008300031201     D* Costanti
008400031201     D*------------------
008500031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008600031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008700080617     D*------------------
008800080617
008900080617     D*------------------
009000080617     D* LINKING A DEFINIZIONI ESTERNE
009100080617     D*------------------
009200080617     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009300090220     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009400010330
009500010330
009600010330
009700990915     C                   time                    wrkdata
009800990915     C                   time                    wrkora
009900000913     C                   reset                   rrnum
010000990921     C                   reset                   esito
010100081014     C                   reset                   wrkesito
010200010601     C*
010300010601     C                   exsr      opeini
010400010605     C                   exsr      rwvab
010500081014     C*
010600081014     C                   exsr      endela
010700010601     C*
010800010601     C                   seton                                        lr
010900010601
011000010601
011100010601
011200010601     C*--------------------------------------------------------
011300010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011400010601     C*--------------------------------------------------------
011500010601     C     PREELA        BEGSR
011600010601     C*
011700010601     C* SE OCCORRE SPEDIRE IN FILIALE
011800010601     C                   if        invfil <> *zeros and
011900010601     C                             flgGiro = '0'
012000010601     C*
012100010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012200010601     C                   eval      flgGiro = '1'
012300000724     C*
012400000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
012500000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012600010601     C     invfil        chain     azorg01l
012700000724     C                   if        %found
012800000724     C                   movel(p)  CMD(1)        depcmd
012900020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
013000000724     C*
013100000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
013200011113     C                   Z-ADD     150           LENGH            15 5
013300000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013400000724     C                   PARM                    depcmd
013500000724     C                   PARM                    LENGH
013600000724     C*
013700000724     C                   endif
013800000616     C*
013900000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
014000000616     C                   movel(p)  CMD(2)        depcmd
014100000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
014200000616     C*
014300000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
014400011113     C                   Z-ADD     150           LENGH            15 5
014500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014600000616     C                   PARM                    depcmd
014700000616     C                   PARM                    LENGH
014800000613     C*
014900090623     C                   if        not %open(titv1q7ps)
015000090623     C                   open      titv1q7ps
015100070329     C                   except    testdett
015200000613     C                   endif
015300010601     C*
015400010601     C                   endif
015500010601     C*
015600010601     C                   ENDSR
015700010601     C***
015800010601
015900010601
016000010601
016100010601
016200010601     C*--------------------------------------------------------
016300010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
016400010601     C*--------------------------------------------------------
016500010601     C     ENDELA        BEGSR
016600081014     C*
016700081014     C                   if        %open(tivin00r)
016800081014     C                   close     tivin00r
016900081014     C                   endif
017000081014     C                   if        %open(fivabwwr)
017100081014     C                   close     fivabwwr
017200081014     C                   endif
017300081014     C                   if        %open(fivatwwr)
017400081014     C                   close     fivatwwr
017500081014     C                   endif
017600090623     C                   if        %open(fivaxwwr)
017700090623     C                   close     fivaxwwr
017800090623     C                   endif
017900040119     C*
018000040119     C* Effettuo la chiamata al CLLE preposto
018100090623     C                   call(e)   'TITVVTXC'
018200040510     C                   parm                    parccm
018300040119     C                   parm                    parmbr
018400040119     C                   parm      '2'           paropz
018500050201     C*
018600050201     C* Effettuo lancio TISI95 solo x chiusura
018700050201     C                   CLEAR                   TISI95DS
018800050201     C                   EVAL      I95TLA = 'C'
018900050201     C                   CALL      'TISI95R'
019000050201     C                   PARM                    TISI95DS
019100000613     C*
019200090623     C                   if        %open(titv1q7ps)
019300000616     C                   except    findett
019400090623     C                   close     titv1q7ps
019500000613     C                   endif
019600000616     C*
019700000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
019800011113     C                   Z-ADD     150           LENGH            15 5
019900010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
020000000616     C                   PARM                    CMD(3)
020100000616     C                   PARM                    LENGH
020200000616     C*
020300010601     C                   ENDSR
020400010601     C***
020500010601
020600010601
020700010601
020800000613
020900000613     C*--------------------------------------------------------
021000000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
021100000613     C*--------------------------------------------------------
021200000621     C     STPR          BEGSR
021300000613     C*
021400090623     C                   if        not %open(titv1q7p)
021500090623     C   50              open      titv1q7p
021600990915     C                   endif
021700990915     C*
021800030715     C   50              except    riepilogo
021900990915     C*
022000090623     C                   if        %open(titv1q7p)
022100090623     C   50              close     titv1q7p
022200990914     C                   endif
022300000613     C*
022400000613     C                   ENDSR
022500000613     C***
022600000613
022700010601
022800010601
022900010330     C*--------------------------------------------------------
023000021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
023100010330     C*--------------------------------------------------------
023200010605     C     RWVAB         BEGSR
023300010330     C*
023400010330     C                   if        not %open(tivin00r)
023500010330     C                   open      tivin00r
023600010330     C                   endif
023700021025     C                   if        not %open(fivabwwr)
023800021025     C                   open      fivabwwr
023900010330     C                   endif
024000040119     C*
024100090623     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR/FIVAXWWR
024200090623     C                   exsr      prevatvax
024300040119     C*
024400040119     C                   if        chkcall = '0'
024500040119     C*
024600040119     C                   if        not %open(fivatwwr)
024700040119     C                   open      fivatwwr
024800040119     C                   endif
024900090623     C*
025000090623     C                   if        not %open(fivaxwwr)
025100090623     C                   open      fivaxwwr
025200090623     C                   endif
025300010330     C*
025400040119     C                   clear                   §CTROKVB          7 0
025500040119     C                   clear                   §CTROKVT          7 0
025600090623     C                   clear                   §CTROKVX          7 0
025700040119     C                   clear                   §CTRMO            7 0
025800040119     C                   clear                   §CTRNO            7 0
025900090623     C*
026000090623     C* Inizializzazioni
026100090623     C                   exsr      inzvar
026200090623     C                   z-add     *zeros        wNewBolla         7 0
026300010330     C*
026400030822     C                   do        *HIVAL
026500010330     C*
026600030822     C                   read      tivin00r                               70
026700010618     C*
026800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
026900010618     C                   if        vinflg = '1'
027000010618     C                   eval      flgOk = '1'
027100010618     C                   endif
027200040510     C*
027300040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
027400040510     C                   z-add     *zeros        wLenVINDTA        4 0
027500040510     C                   z-add     *zeros        wFlgVINDTA        1 0
027600040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
027700040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
027800040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
027900040510     C                   z-add     1             wFlgVINDTA
028000040510     C                   leave
028100040510     C                   endif
028200040510     C                   enddo
028300090420     C*
028400090420     C                   add       1             rrnum
028500010618     C*
028600040510     C                   if        vindta > *blanks AND
028700090420     C                             wFlgVINDTA = 1
028800010330     C*
028900010601     C                   if        *in70 = *off and
029000010330     C                             (vinflg = *blanks
029100010330     C                              or vinflg = '0'
029200010330     C                              or vinflg = '2')
029300010330     C*
029400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
029500010711     C                   if        vinflg = *blanks or vinflg = '0'
029600010711     C                   clear                   vinmsg
029700010711     C                   endif
029800010601     C*
029900090421     C* Aggiungo un byte ad '*' all'inizio d ogni record
030000090421     C                   if        %subst(%trim(vindta):1:1) = CharCSV
030100090421     C                   eval      vindta  = '*' + %trim(vindta)
030200090421     C                   endif
030300090623     C*
030400090623     C* Elaborazione record corrente
030500090623     C                   exsr      elabCurrRec
030600010601     C*
030700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
030800010605     C                   eval      depfil = VABLNP
030900010601     C                   exsr      repfil
031000010601     C                   if        depfil = invfil
031100021025     C                   if        vlrpoi = 999
031200030822     C                   move(P)   invfil        VABFGS
031300021025     C                   else
031400030822     C                   move(P)   vlrpoi        VABFGS
031500021025     C                   endif
031600010601     C*
031700010601     C                   exsr      PREELA
031800010604     C*
031900010604     C                   if        *in31 = *off and
032000010604     C                             *in32 = *off
032100010604     C                   eval      vinflg = '1'
032200010604     C                   else
032300010604     C                   eval      recko = vindta
032400010604     C                   except    rigadett
032500010604     C                   eval      vinflg = '2'
032600010604     C                   endif
032700010604     C                   endif
032800010601     C*
032900010604     C                   endif
033000010604     C*
033100010330     C                   else
033200010330     C                   eval      vinflg = '1'
033300010330     C                   endif
033400010601     C*
033500010601     C  N70              update    tivin000
033600010330     C*
033700030822     C  N70              enddo
033800090623     C*
033900090623     C* Scarico la testata rimasta "in canna"
034000090624     C  N31              exsr      wrivaxFoot
034100090623     C  N31              exsr      wriVAB
034200040119     C*
034300040119     C                   endif                                                  (endif - chkcall)
034400010601     C*
034500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
034600010601     C                   if        cntNonEl = *zeros or
034700010601     C                             flgMulti = '0'
034800010330     C* Se non ci sono record con errori ...
034900010601     C                   if        §ctrno = 0 and
035000010604     C                             §ctrmo = 0 and
035100010601     C                             flgStato <> '2'
035200010330     C* ... restituisco esito OK.
035300010330     C                   eval      wrkesito = '0'
035400010330     C                   else
035500040119     C                   if        §ctrokvb > 0
035600010330     C                   eval      wrkesito = '1'
035700010330     C                   else
035800010615     C                   if        flgOk = '0'
035900010615     C                   eval      wrkesito = '2'
036000010615     C                   else
036100010615     C                   eval      wrkesito = '6'
036200010615     C                   endif
036300010330     C                   endif
036400010330     C                   endif
036500010601     C                   else
036600010601     C                   eval      wrkesito = '9'
036700010601     C                   endif
036800010330     C*
036900010330     C                   if        %open(tivin00r)
037000010330     C                   close     tivin00r
037100010330     C                   endif
037200021025     C                   if        %open(fivabwwr)
037300021025     C                   close     fivabwwr
037400010330     C                   endif
037500040512     C                   if        %open(fivatwwr)
037600040512     C                   close     fivatwwr
037700040512     C                   endif
037800090623     C                   if        %open(fivaxwwr)
037900090623     C                   close     fivaxwwr
038000090623     C                   endif
038100010601     C*
038200010601     C                   if        vlrpoi <> 999
038300010601     C                   eval      invfil = vlrpoi
038400010601     C                   endif
038500010330     C*
038600040119     C                   if        §ctrokvb > 0
038700010601     C                             and invfil > *zeros
038800010330     C                   exsr      invio
038900010330     C                   endif
039000010601     C*
039100010618     C                   if        flgGiro = '1'
039200081014     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
039300010618     C                   endif
039400010330     C*
039500010330     C                   ENDSR
039600010330     C***
039700090623
039800090623
039900090623
040000090623     C*----------------------------------------------------*
040100090623     C*  INIZIALIZZAZIONI CAMPI DI WRK
040200090623     C*----------------------------------------------------*
040300090623     C     INZWRK        BEGSR
040400090623     C*
040500090623     C                   eval      i = 1
040600090623     C                   eval      posDa = *zeros
040700090623     C                   eval      posA  = *zeros
040800090623     C*
040900090623     C                   ENDSR
041000090623
041100090623
041200090623
041300090623     C*----------------------------------------------------*
041400090623     C*  ELABORAZIONE RECORD CORRENTE
041500090623     C*----------------------------------------------------*
041600090623     C     ELABCURRREC   BEGSR
041700090623     C*
041800090623     C                   select
041900090623     C*
042000090623     C* Tipo record "TESTATA" - ENTEXP
042100090623     C                   when      %subst(vindta:1:6) = 'ENTEXP'
042200090623     C                   if        wNewBolla > *zeros
042300090624     C  N31              exsr      wrivaxFoot
042400090623     C  N31              exsr      wriVAB
042500090623     C                   exsr      inzvar
042600090623     C                   endif
042700090624     C                   eval      wNewBolla = 1
042800090623     C                   eval      wGiro = *zeros
042900090623     C                   clear                   SkSplitFLD
043000090624     C                   exsr      inzwrk
043100090623     C                   exsr      impvab
043200090623     C                   exsr      inzwrk
043300090623     C                   exsr      impvab
043400090624     C  N31              exsr      wrivaxHead
043500090623     C*
043600090623     C* Tipo record "DETTAGLIO" - LIGEXP
043700090623     C                   when      %subst(vindta:1:6) = 'LIGEXP'
043800090623     C                   eval      wGiro = *zeros
043900090623     C                   clear                   SkSplitFLD
044000090623     C                   exsr      inzwrk
044100090623     C                   exsr      impvatvax
044200090623     C                   exsr      inzwrk
044300090623     C                   exsr      impvatvax
044400090624     C*
044500090624     C  N31              exsr      wrivat
044600090624     C  N31              exsr      wrivaxBody
044700090623     C*
044800090623     C*
044900090623     C                   endsl
045000090623     C*
045100090623     C                   ENDSR
045200040119
045300040119
045400040119
045500040119     C*----------------------------------------------------*
045600040119     C*  SCARICAMENTO BUFFER RECORDS VAB
045700040119     C*----------------------------------------------------*
045800040510     C     WRIVAB        BEGSR
045900090623     C*
046000090623     C  N31              add       1             §CTROKVB
046100090623     C   32              add       1             §CTRMO
046200090623     C   31              add       1             §CTRNO
046300040119     C*
046400060331     C* Gestisco l'eventuale rottura x numero spedizione
046500070823     C                   if        VABNSP <> depspe
046600070301     C                   if        wVATNOT_E <> *blanks
046700080125     C                   if        VABCTM = *blanks
046800080125     C                   eval      VABCTM = '7Q'
046900080125     C                   endif
047000070301     C                   endif
047100040119     C                   write     fivab000                                     => scarico il VAB
047200060331     C                   endif
047300090623     C*
047400090623     C  N31              eval      depspe = vabNSP
047500040119     C*
047600040119     C                   ENDSR
047700040119
047800040119
047900040119
048000040119     C*----------------------------------------------------*
048100040119     C*  SCARICAMENTO BUFFER RECORDS VAT
048200040119     C*----------------------------------------------------*
048300040510     C     WRIVAT        BEGSR
048400060512     C*
048500060512     C* Inizializzo i campi chiave
048600060512     C                   eval      VATFGS = VABFGS
048700060512     C                   eval      VATAAS = VABAAS
048800060512     C                   eval      VATCCM = VABCCM
048900070823     C                   eval      VATNRS = VABNRS
049000060512     C                   eval      VATNSP = VABNSP
049100060512     C                   eval      VATLNP = VABLNP
049200060331     C*
049300060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
049400060331     C                   if        wVATNOT_A <> *blanks
049500060331     C                   eval      VATNOT = wVATNOT_A
049600060331     C                   eval      VATTRC = 'A'
049700060331     C                   write     FIVAT000
049800060331     C                   add       1             §CTROKVT
049900060331     C                   endif
050000060331     C*
050100060331     C                   if        wVATNOT_B <> *blanks
050200060331     C                   eval      VATNOT = wVATNOT_B
050300060331     C                   eval      VATTRC = 'B'
050400060331     C                   write     FIVAT000
050500060331     C                   add       1             §CTROKVT
050600060331     C                   endif
050700060331     C*
050800090623     C*
050900090623     C* Considero il VAT solo a rottura del "chi sono"
051000090623     C                   if        wVATNOT_E <> *blanks AND
051100090623     C                             wVATNOT_E <> sVATNOT_E
051200060331     C                   eval      VATNOT = wVATNOT_E
051300060331     C                   eval      VATTRC = 'E'
051400080716     C                   write     FIVAT000
051500080716     C                   add       1             §CTROKVT
051600090623     C                   eval      sVATNOT_E = wVATNOT_E
051700090624     C                   add       1             VABNCL
051800080625     C                   endif
051900040119     C*
052000040119     C                   ENDSR
052100090624
052200090624
052300090624
052400090624     C*----------------------------------------------------*
052500090624     C*  SCARICAMENTO BUFFER RECORDS VAX - TESTATA
052600090624     C*----------------------------------------------------*
052700090624     C     WRIVAXHEAD    BEGSR
052800090624     C*
052900090624     C* Inizializzo i campi chiave
053000090624     C                   eval      VAXFGS = VABFGS
053100090624     C                   eval      VAXAAS = VABAAS
053200090624     C                   eval      VAXCCM = VABCCM
053300090624     C                   eval      VAXNRS = VABNRS
053400090624     C                   eval      VAXNSP = VABNSP
053500090624     C                   eval      VAXLNP = VABLNP
053600090703     C*
053700090703     C                   if        VAXTMD = *blanks
053800090703     C                   eval      VAXTMD = 'STD1'
053900090703     C                   endif
054000090624     C*
054100090624     C* Lascio 1 righe vuote prima della stampa della testata
054200090624     C                   eval      VAXPRR = VAXPRR + 1
054300090624     C                   eval      vaxdtt = *blanks
054400090624     C                   write     FIVAX000
054500090624     C                   add       1             §CTROKVX
054600090624     C*
054700090624     C* Quindi compongo la testata d stampa
054800090624     C                   eval      VAXPRR = VAXPRR + 1
054900090624     C                   eval      VAXDTT = prtconst(1)
055000090703     C                   eval      VAXTRK = 'T1'
055100090624     C                   if        vaxdtt <> *blanks
055200090624     C                   write     FIVAX000
055300090624     C                   add       1             §CTROKVX
055400090624     C                   endif
055500090624     C*
055600090624     C                   eval      VAXPRR = VAXPRR + 1
055700090624     C                   eval      VAXDTT = prtconst(2)
055800090703     C                   eval      VAXTRK = 'T1'
055900090624     C                   if        vaxdtt <> *blanks
056000090624     C                   write     FIVAX000
056100090624     C                   add       1             §CTROKVX
056200090624     C                   endif
056300090624     C*
056400090624     C                   eval      VAXPRR = VAXPRR + 1
056500090624     C                   eval      VAXDTT = wVAX_CODCLI + '        ' +
056600090624     C                                      wVAX_NUMORD + '        ' +
056700090624     C                                      wVAX_NUMBP
056800090703     C                   eval      VAXTRK = 'T1'
056900090624     C                   if        vaxdtt <> *blanks
057000090624     C                   write     FIVAX000
057100090624     C                   add       1             §CTROKVX
057200090624     C                   endif
057300090624     C*
057400090624     C* Lascio 2 righe vuote dopo la stampa della testata
057500090624     C                   eval      VAXPRR = VAXPRR + 1
057600090624     C                   eval      vaxdtt = *blanks
057700090624     C                   write     FIVAX000
057800090624     C                   add       1             §CTROKVX
057900090624     C                   eval      VAXPRR = VAXPRR + 1
058000090624     C                   write     FIVAX000
058100090624     C                   add       1             §CTROKVX
058200090624     C*
058300090624     C* Quindi compongo l'intestazione del body
058400090624     C                   eval      VAXPRR = VAXPRR + 1
058500090703     C                   eval      VAXDTT = prtconst(3)
058600090703     C                   eval      VAXTRK = 'C1'
058700090624     C                   if        vaxdtt <> *blanks
058800090624     C                   write     FIVAX000
058900090624     C                   add       1             §CTROKVX
059000090624     C                   endif
059100090624     C*
059200090624     C                   eval      VAXPRR = VAXPRR + 1
059300090703     C                   eval      VAXDTT = prtconst(4)
059400090703     C                   eval      VAXTRK = 'C1'
059500090624     C                   if        vaxdtt <> *blanks
059600090624     C                   write     FIVAX000
059700090624     C                   add       1             §CTROKVX
059800090624     C                   endif
059900090624     C*
060000090624     C* Lascio 1 righe vuote prima della stampa del body
060100090624     C                   eval      VAXPRR = VAXPRR + 1
060200090624     C                   eval      vaxdtt = *blanks
060300090624     C                   write     FIVAX000
060400090624     C                   add       1             §CTROKVX
060500090624     C*
060600090624     C                   ENDSR
060700090623
060800090623
060900090623
061000090623     C*----------------------------------------------------*
061100090624     C*  SCARICAMENTO BUFFER RECORDS VAX - CORPO
061200090623     C*----------------------------------------------------*
061300090624     C     WRIVAXBODY    BEGSR
061400090623     C*
061500090623     C* Inizializzo i campi chiave
061600090623     C                   eval      VAXFGS = VABFGS
061700090623     C                   eval      VAXAAS = VABAAS
061800090623     C                   eval      VAXCCM = VABCCM
061900090623     C                   eval      VAXNRS = VABNRS
062000090623     C                   eval      VAXNSP = VABNSP
062100090623     C                   eval      VAXLNP = VABLNP
062200090623     C*
062300090623     C                   eval      VAXTRK = 'C1'
062400090703     C*
062500090703     C                   if        VAXTMD = *blanks
062600090703     C                   eval      VAXTMD = 'STD1'
062700090703     C                   endif
062800090623     C*
062900090624     C* Allineo a destra il numero di scatole
063000090624     C                   evalr     wVAXDTT_3 = %trim(wVAXDTT_3)
063100090624     C*
063200090624     C                   eval      VAXPRR = VAXPRR + 1
063300090624     C                   eval      VAXDTT = '   ' + wVAXDTT_1 + '     ' +
063400090624     C                                      wVAXDTT_2 + '        ' + WVAXDTT_3
063500090623     C*
063600090623     C                   if        vaxdtt <> *blanks
063700090623     C                   write     FIVAX000
063800090623     C                   add       1             §CTROKVX
063900090623     C                   endif
064000090623     C*
064100090623     C                   ENDSR
064200090624
064300090624
064400090624
064500090624     C*----------------------------------------------------*
064600090624     C*  SCARICAMENTO BUFFER RECORDS VAX - PIEDE
064700090624     C*----------------------------------------------------*
064800090624     C     WRIVAXFOOT    BEGSR
064900090624     C*
065000090624     C* Inizializzo i campi chiave
065100090624     C                   eval      VAXFGS = VABFGS
065200090624     C                   eval      VAXAAS = VABAAS
065300090624     C                   eval      VAXCCM = VABCCM
065400090624     C                   eval      VAXNRS = VABNRS
065500090624     C                   eval      VAXNSP = VABNSP
065600090624     C                   eval      VAXLNP = VABLNP
065700090624     C*
065800090624     C                   eval      VAXTRK = 'T2'
065900090703     C*
066000090703     C                   if        VAXTMD = *blanks
066100090703     C                   eval      VAXTMD = 'STD1'
066200090703     C                   endif
066300090624     C*
066400090624     C* Lascio 1 righe vuote prima della stampa del piede
066500090624     C                   eval      VAXPRR = VAXPRR + 1
066600090624     C                   eval      vaxdtt = *blanks
066700090624     C                   write     FIVAX000
066800090624     C                   add       1             §CTROKVX
066900090624     C*
067000090624     C* Quindi riga di separazione body/foot
067100090624     C                   eval      VAXPRR = VAXPRR + 1
067200090624     C                   eval      VAXDTT = prtconst(5)
067300090624     C                   if        vaxdtt <> *blanks
067400090624     C                   write     FIVAX000
067500090624     C                   add       1             §CTROKVX
067600090624     C                   endif
067700090624     C*
067800090624     C* Lascio 1 righe vuote prima della stampa del piede
067900090624     C                   eval      VAXPRR = VAXPRR + 1
068000090624     C                   eval      vaxdtt = *blanks
068100090624     C                   write     FIVAX000
068200090624     C                   add       1             §CTROKVX
068300090624     C*
068400090624     C* Quindi componfo il piede d stampa
068500090624     C                   eval      VAXPRR = VAXPRR + 1
068600090624     C                   eval      VAXDTT = 'CAUSALE: Vendita'                 +
068700090624     C                                      '               '                  +
068800090624     C                                      'CARTONI: ' + %char(vabNCL)        +
068900090624     C                                      '               '                  +
069000090624     C                                      'PESO (Kg.): ' + %char(vabPKB)
069100090624     C*
069200090624     C                   if        vaxdtt <> *blanks
069300090624     C                   write     FIVAX000
069400090624     C                   add       1             §CTROKVX
069500090624     C                   endif
069600090624     C*
069700090624     C* Quindi riga di chiusura finale
069800090624     C                   eval      VAXPRR = VAXPRR + 1
069900090624     C                   eval      VAXDTT = prtconst(5)
070000090624     C                   if        vaxdtt <> *blanks
070100090624     C                   write     FIVAX000
070200090624     C                   add       1             §CTROKVX
070300090624     C                   endif
070400090624     C*
070500090624     C                   ENDSR
070600040119
070700040119
070800040119
070900040119     C*----------------------------------------------------*
071000090623     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR/FIVAXWWR
071100040119     C*----------------------------------------------------*
071200090623     C     PREVATVAX     BEGSR
071300040119     C*
071400090623     C* Compongo il nome del membro da dare al FIVATWWR/FIVAXWWR
071500040119     C                   eval      parmbr = vlrhdl
071600040119     C                   movel     'M'           parmbr
071700040510     C                   eval      parccm = vlrksc
071800040119     C                   eval      paropz = '1'
071900040119     C* Effettuo la chiamata al CLLE preposto
072000090623     C                   call(e)   'TITVVTXC'
072100040119     C                   parm                    parccm
072200040119     C                   parm                    parmbr
072300040119     C                   parm                    paropz
072400040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
072500040119     C                   if        %error
072600040119     C                   movel     '1'           chkcall
072700040119     C                   else
072800040119     C                   movel     '0'           chkcall
072900040119     C                   endif
073000040119     C*
073100040119     C                   ENDSR
073200010601
073300010601
073400010601
073500010330     C*----------------------------------------------------*
073600030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
073700010330     C*----------------------------------------------------*
073800040119     C     INZVAR        BEGSR
073900010330     C*
074000030822     C* Inizializzo variabili di wrk
074100020204     C                   Z-ADD     *zeros        Num5_0            5 0
074200060331     C                   MOVEL     *blanks       wVATNOT_A
074300060331     C                   MOVEL     *blanks       wVATNOT_B
074400090904     C                   MOVEL     *blanks       wVATNOT_E
074500090904     C                   MOVEL     *blanks       wVATNOT_E_1
074600090904     C                   MOVEL     *blanks       wVATNOT_E_2
074700090623     C*
074800090624     C                   MOVEL     *blanks       sVATNOT_E        35
074900090623     C                   MOVEL     *blanks       wVAXDTT_1        10
075000090623     C                   MOVEL     *blanks       wVAXDTT_2        35
075100090623     C                   MOVEL     *blanks       wVAXDTT_3        10
075200090624     C                   MOVEL     *blanks       wVAX_NUMBP       10
075300090624     C                   MOVEL     *blanks       wVAX_NUMORD      10
075400090624     C                   MOVEL     *blanks       wVAX_CODCLI      15
075500090220     C*
075600080617     C                   EVAL      wFlgCAS = *blanks
075700030822     C*
075800030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
075900030822     C                   CLEAR                   FIVAB000
076000040119     C                   CLEAR                   FIVAT000
076100090623     C                   CLEAR                   FIVAX000
076200030822     C                   CLEAR                   SkSplitCSV
076300090623     C                   EXSR      inzwrk
076400030822     C*
076500030822     C* Reimposto i valori di default
076600030822     C                   EXSR      DEFCAM
076700030822     C*
076800010330     C                   ENDSR
076900010330     C*----------------------------------------------------*
077000030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
077100010330     C*----------------------------------------------------*
077200010330     C     DEFCAM        BEGSR
077300030822     C*
077400030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
077500030715     C* e delimitatore testo.
077600030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
077700030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
077800030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
077900030822     C*
078000030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
078100030715     C                   IF        %subst(vlrppt:1:1) = 'S'
078200030715     C                   SETON                                        50
078300030715     C                   ELSE
078400030715     C                   SETOFF                                       50
078500030715     C                   ENDIF
078600090421     C*
078700090421     C* D default SEMPRE porto franco / SEMPRE lnp = p.o. invio / SEMPRE nsp = rrnum
078800090421     C                   EVAL      VABCBO = '1'
078900090421     C                   EVAL      VABLNP = vlrpoi
079000090421     C                   EVAL      VABNSP = rrnum
079100030822     C*
079200030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
079300030822     C                   EVAL      posDaDft = 1
079400030822     C                   EVAL      posADft  = 0
079500030822     C                   EVAL      wGiroDft = 0
079600030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
079700030822     C                             posDaDft > 0
079800030822     C*
079900030822     C* Gestisco il 1° giro
080000030822     C                   IF        wGiroDft = 0
080100030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
080200030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
080300030822     C* Incremento il contatore dei "giri"
080400030822     C                   EVAL      wGiroDft = 1
080500030822     C                   ELSE
080600030822     C                   EVAL      posDaDft = posADft
080700030822     C                   ENDIF
080800030822     C* Eseguo lo scan x trovare la fine del campo corrente
080900030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
081000030822     C*
081100030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
081200030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
081300030822     C                   IF        posDaDft > 0 AND
081400030822     C                             posADft  > 0
081500030822     C* NCL
081600030822     C                   IF        %subst(
081700030822     C                             %subst(vlrppt:posDaDft+1:
081800030822     C                             posADft-posDaDft-1):1:3)
081900030822     C                             = 'NCL'
082000030822     C                   EVAL      PiStr=%trim(%subst(
082100030822     C                             %subst(vlrppt:posDaDft+1:
082200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082300030822     C                   EXSR      CHKNUM
082400030822     C                   IF        PiInt=*on
082500030822     C                   Z-ADD     PiVal         VABNCL
082600030822     C                   ENDIF
082700030822     C                   ENDIF
082800030822     C* CCM
082900030822     C                   IF        %subst(
083000030822     C                             %subst(vlrppt:posDaDft+1:
083100030822     C                             posADft-posDaDft-1):1:3)
083200030822     C                             = 'CCM'
083300030822     C                   EVAL      PiStr=%trim(%subst(
083400030822     C                             %subst(vlrppt:posDaDft+1:
083500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083600030822     C                   EXSR      CHKNUM
083700030822     C                   IF        PiInt=*on
083800030822     C                   Z-ADD     PiVal         VABCCM
083900030822     C                   ENDIF
084000030822     C                   ENDIF
084100030822     C* LNP
084200030822     C                   IF        %subst(
084300030822     C                             %subst(vlrppt:posDaDft+1:
084400030822     C                             posADft-posDaDft-1):1:3)
084500030822     C                             = 'LNP'
084600030822     C                   EVAL      PiStr=%trim(%subst(
084700030822     C                             %subst(vlrppt:posDaDft+1:
084800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084900030822     C                   EXSR      CHKNUM
085000030822     C                   IF        PiInt=*on
085100030822     C                   Z-ADD     PiVal         VABLNP
085200030822     C                   ENDIF
085300030822     C                   ENDIF
085400030822     C* NRS
085500030822     C                   IF        %subst(
085600030822     C                             %subst(vlrppt:posDaDft+1:
085700030822     C                             posADft-posDaDft-1):1:3)
085800030822     C                             = 'NRS'
085900030822     C                   EVAL      PiStr=%trim(%subst(
086000030822     C                             %subst(vlrppt:posDaDft+1:
086100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086200030822     C                   EXSR      CHKNUM
086300030822     C                   IF        PiInt=*on
086400030822     C                   Z-ADD     PiVal         VABNRS
086500030822     C                   ENDIF
086600030822     C                   ENDIF
086700030822     C* CTR
086800030822     C                   IF        %subst(
086900030822     C                             %subst(vlrppt:posDaDft+1:
087000030822     C                             posADft-posDaDft-1):1:3)
087100030822     C                             = 'CTR'
087200030822     C                   EVAL      PiStr=%trim(%subst(
087300030822     C                             %subst(vlrppt:posDaDft+1:
087400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
087500030822     C                   EXSR      CHKNUM
087600030822     C                   IF        PiInt=*on
087700030822     C                   Z-ADD     PiVal         VABCTR
087800030822     C                   ENDIF
087900030822     C                   ENDIF
088000030822     C* PKB
088100030822     C                   IF        %subst(
088200030822     C                             %subst(vlrppt:posDaDft+1:
088300030822     C                             posADft-posDaDft-1):1:3)
088400030822     C                             = 'PKB'
088500030822     C                   EVAL      PiStr=%trim(%subst(
088600030822     C                             %subst(vlrppt:posDaDft+1:
088700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
088800030822     C                   EXSR      CHKNUM
088900030822     C                   IF        PiNum=*on
089000030822     C                   Z-ADD     PiVal         VABPKB
089100030822     C                   ENDIF
089200030822     C                   ENDIF
089300030822     C* VLB
089400030822     C                   IF        %subst(
089500030822     C                             %subst(vlrppt:posDaDft+1:
089600030822     C                             posADft-posDaDft-1):1:3)
089700030822     C                             = 'VLB'
089800030822     C                   EVAL      PiStr=%trim(%subst(
089900030822     C                             %subst(vlrppt:posDaDft+1:
090000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
090100030822     C                   EXSR      CHKNUM
090200030822     C                   IF        PiNum=*on
090300030822     C                   Z-ADD     PiVal         VABVLB
090400030822     C                   ENDIF
090500030822     C                   ENDIF
090600030822     C* QFT
090700030822     C                   IF        %subst(
090800030822     C                             %subst(vlrppt:posDaDft+1:
090900030822     C                             posADft-posDaDft-1):1:3)
091000030822     C                             = 'QFT'
091100030822     C                   EVAL      PiStr=%trim(%subst(
091200030822     C                             %subst(vlrppt:posDaDft+1:
091300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
091400030822     C                   EXSR      CHKNUM
091500030822     C                   IF        PiNum=*on
091600030822     C                   Z-ADD     PiVal         VABQFT
091700030822     C                   ENDIF
091800030822     C                   ENDIF
091900030822     C* CBO
092000030822     C                   IF        %subst(
092100030822     C                             %subst(vlrppt:posDaDft+1:
092200030822     C                             posADft-posDaDft-1):1:3)
092300030822     C                             = 'CBO'
092400030822     C                   EVAL      VABCBO=%trim(%subst(
092500030822     C                             %subst(vlrppt:posDaDft+1:
092600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
092700030822     C                   ENDIF
092800030822     C* TSP
092900030822     C                   IF        %subst(
093000030822     C                             %subst(vlrppt:posDaDft+1:
093100030822     C                             posADft-posDaDft-1):1:3)
093200030822     C                             = 'TSP'
093300030822     C                   EVAL      VABTSP=%trim(%subst(
093400030822     C                             %subst(vlrppt:posDaDft+1:
093500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
093600030822     C                   ENDIF
093700030822     C* VAS
093800030822     C                   IF        %subst(
093900030822     C                             %subst(vlrppt:posDaDft+1:
094000030822     C                             posADft-posDaDft-1):1:3)
094100030822     C                             = 'VAS'
094200030822     C                   EVAL      VABVAS=%trim(%subst(
094300030822     C                             %subst(vlrppt:posDaDft+1:
094400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
094500030822     C                   ENDIF
094600030822     C* VCA
094700030822     C                   IF        %subst(
094800030822     C                             %subst(vlrppt:posDaDft+1:
094900030822     C                             posADft-posDaDft-1):1:3)
095000030822     C                             = 'VCA'
095100030822     C                   EVAL      VABVCA=%trim(%subst(
095200030822     C                             %subst(vlrppt:posDaDft+1:
095300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
095400030822     C                   ENDIF
095500070925     C* TIC
095600070925     C                   IF        %subst(
095700070925     C                             %subst(vlrppt:posDaDft+1:
095800070925     C                             posADft-posDaDft-1):1:3)
095900070925     C                             = 'TIC'
096000070925     C                   EVAL      VABTIC=%trim(%subst(
096100070925     C                             %subst(vlrppt:posDaDft+1:
096200070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
096300070925     C                   ENDIF
096400030822     C* GCA
096500030822     C                   IF        %subst(
096600030822     C                             %subst(vlrppt:posDaDft+1:
096700030822     C                             posADft-posDaDft-1):1:3)
096800030822     C                             = 'GCA'
096900030822     C                   EVAL      VABGCA=%trim(%subst(
097000030822     C                             %subst(vlrppt:posDaDft+1:
097100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
097200030822     C                   ENDIF
097300030822     C* CTM
097400030822     C                   IF        %subst(
097500030822     C                             %subst(vlrppt:posDaDft+1:
097600030822     C                             posADft-posDaDft-1):1:3)
097700030822     C                             = 'CTM'
097800030822     C                   EVAL      VABCTM=%trim(%subst(
097900030822     C                             %subst(vlrppt:posDaDft+1:
098000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
098100030822     C                   ENDIF
098200030822     C* FFD
098300030822     C                   IF        %subst(
098400030822     C                             %subst(vlrppt:posDaDft+1:
098500030822     C                             posADft-posDaDft-1):1:3)
098600030822     C                             = 'FFD'
098700030822     C                   EVAL      VABFFD=%trim(%subst(
098800030822     C                             %subst(vlrppt:posDaDft+1:
098900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
099000030822     C                   ENDIF
099100030822     C* VAD
099200030822     C                   IF        %subst(
099300030822     C                             %subst(vlrppt:posDaDft+1:
099400030822     C                             posADft-posDaDft-1):1:3)
099500030822     C                             = 'VAD'
099600030822     C                   EVAL      VABVAD=%trim(%subst(
099700030822     C                             %subst(vlrppt:posDaDft+1:
099800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
099900030822     C                   ENDIF
100000030822     C* GMA
100100030822     C                   IF        %subst(
100200030822     C                             %subst(vlrppt:posDaDft+1:
100300030822     C                             posADft-posDaDft-1):1:3)
100400030822     C                             = 'GMA'
100500030822     C                   EVAL      VABGMA=%trim(%subst(
100600030822     C                             %subst(vlrppt:posDaDft+1:
100700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
100800030822     C                   ENDIF
100900030822     C* GGA
101000030822     C                   IF        %subst(
101100030822     C                             %subst(vlrppt:posDaDft+1:
101200030822     C                             posADft-posDaDft-1):1:3)
101300030822     C                             = 'GGA'
101400030822     C                   EVAL      VABGGA=%trim(%subst(
101500030822     C                             %subst(vlrppt:posDaDft+1:
101600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
101700030822     C                   ENDIF
101800030822     C* GVA
101900030822     C                   IF        %subst(
102000030822     C                             %subst(vlrppt:posDaDft+1:
102100030822     C                             posADft-posDaDft-1):1:3)
102200030822     C                             = 'GVA'
102300030822     C                   EVAL      VABGVA=%trim(%subst(
102400030822     C                             %subst(vlrppt:posDaDft+1:
102500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
102600030822     C                   ENDIF
102700030822     C* TC1
102800030822     C                   IF        %subst(
102900030822     C                             %subst(vlrppt:posDaDft+1:
103000030822     C                             posADft-posDaDft-1):1:3)
103100030822     C                             = 'TC1'
103200030822     C                   EVAL      VABTC1=%trim(%subst(
103300030822     C                             %subst(vlrppt:posDaDft+1:
103400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
103500030822     C                   ENDIF
103600030822     C* TC2
103700030822     C                   IF        %subst(
103800030822     C                             %subst(vlrppt:posDaDft+1:
103900030822     C                             posADft-posDaDft-1):1:3)
104000030822     C                             = 'TC2'
104100030822     C                   EVAL      VABTC2=%trim(%subst(
104200030822     C                             %subst(vlrppt:posDaDft+1:
104300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
104400030822     C                   ENDIF
104500040714     C* VATTRC
104600040714     C                   IF        %subst(
104700040714     C                             %subst(vlrppt:posDaDft+1:
104800040714     C                             posADft-posDaDft-1):1:3)
104900040714     C                             = 'TRC'
105000040714     C                   EVAL      VATTRC=%trim(%subst(
105100040714     C                             %subst(vlrppt:posDaDft+1:
105200040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
105300040714     C                   ENDIF
105400090623     C* VAXTMD
105500090623     C                   IF        %subst(
105600090623     C                             %subst(vlrppt:posDaDft+1:
105700090623     C                             posADft-posDaDft-1):1:3)
105800090623     C                             = 'TMD'
105900090623     C                   EVAL      VAXTMD=%trim(%subst(
106000090623     C                             %subst(vlrppt:posDaDft+1:
106100090623     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
106200090623     C                   ENDIF
106300030822     C* ...
106400030822     C                   ENDIF
106500030822     C                   ENDDO
106600020204     C*
106700010330     C                   ENDSR
106800010607     C*----------------------------------------------------*
106900021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
107000010607     C*----------------------------------------------------*
107100090623     C     IMPVAB        BEGSR
107200010607     C*
107300010607     C                   SETOFF                                       3132
107400030515     C*
107500010607     C                   MOVEL     datcor        VABAAS
107600010607     C                   MOVE      datcor        VABMGS
107700010607     C*
107800030715     C**********
107900030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
108000030715     C**********
108100030715     C                   IF        wGiro = *zeros
108200080716     C*
108300080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
108400080716     C                   EVAL      depvinDTA = vinDTA
108500040113     C*
108600040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
108700090623     C                   IF        %trim(entexp(1))+%trim(entexp(2))+
108800090623     C                             %trim(entexp(3))+%trim(entexp(4))+
108900090623     C                             %trim(entexp(5))<>*blanks
109000090623     C                   EVAL      vindta = %trim(entexp(1))+%trim(entexp(2))+
109100090623     C                                      %trim(entexp(3))+%trim(entexp(4))+
109200090623     C                                      %trim(entexp(5))
109300070823     C     ';':CharCSV   XLATE     vindta        vindta
109400040510     C                   ENDIF
109500080626     C*
109600031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
109700031201     C     minu:maiu     XLATE     vindta        vindta
109800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
109900030715     C                   DOW       posDa <= %len(%trim(vindta))
110000030715     C*
110100030715     C* Gestisco il 1° campo
110200030715     C                   IF        i = 1
110300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
110400030715     C                   EVAL      posDa = 1
110500030715     C* Eseguo lo scan x trovare la fine del primo campo
110600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
110700030715     C* A questo punto "estraggo" il campo corrente
110800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
110900030715     C                                                    (posA-posDa))
111000030715     C* X i campi successivi al 1°
111100030715     C                   ELSE
111200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
111300030715     C                   EVAL      posDa = posA + 1
111400030715     C* Eseguo lo scan x trovare la fine del campo corrente
111500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
111600030715     C* Gestisco l'ultimo campo
111700030715     C                   IF        posA = *zeros
111800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
111900030715     C* A questo punto "estraggo" il campo corrente
112000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
112100030715     C                                                    (posA-posDa))
112200030715     C                   LEAVE
112300030715     C                   ELSE
112400030715     C* A questo punto "estraggo" il campo corrente
112500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
112600030715     C                                                    (posA-posDa))
112700030715     C                   ENDIF
112800030715     C                   ENDIF
112900030715     C* Incremento il contatore d campo
113000030715     C                   EVAL      i = i +1
113100030715     C                   ENDDO
113200030715     C*
113300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
113400030715     C* delimitazione testo
113500030715     C                   EVAL      i = 1
113600030715     C                   DOW       i < %elem(SkSplitFLD)
113700030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
113800030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
113900030715     C                   EVAL      i = i + 1
114000030715     C                   ENDDO
114100030715     C                   EVAL      wGiro = 1
114200080716     C* Ripristino la 1° riga originale
114300080716     C                   EVAL      vinDTA = depvinDTA
114400030715     C                   ELSE
114500030715     C                   EVAL      wGiro = 2
114600031204     C**********
114700031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
114800031204     C* d separatore campo
114900031204     C**********
115000031204     C                   EVAL      vindta = %trim(vindta)
115100031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
115200031204     C                   EVAL      vindta = %subst(vindta:2)
115300031204     C                   ENDDO
115400031204     C*
115500031204     C                   Z-ADD     *zeros        lunghInput        4 0
115600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
115700031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
115800031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
115900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
116000031204     C                   ENDDO
116100030715     C**********
116200030715     C* Effettuo lo split del campo dati d input
116300030715     C**********
116400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
116500030715     C                   DOW       posDa <= %len(%trim(vindta))
116600030715     C*
116700030715     C* Gestisco il 1° campo
116800030715     C                   IF        i = 1
116900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
117000030715     C                   EVAL      posDa = 1
117100030715     C* Eseguo lo scan x trovare la fine del primo campo
117200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
117300030715     C* A questo punto "estraggo" il campo corrente
117400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
117500030715     C                                                    (posA-posDa))
117600030715     C* X i campi successivi al 1°
117700030715     C                   ELSE
117800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
117900030715     C                   EVAL      posDa = posA + 1
118000031204     C* Verifico che nn vi sia il campo nn valorizzato
118100031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
118200031204     C* Se campo nn valorizzato skippo al prossimo
118300031204     C                   EVAL      SkSplitCSV(i) = *blanks
118400031204     C                   EVAL      posA  = posA + 1
118500031204     C                   ELSE
118600030715     C* Eseguo lo scan x trovare la fine del campo corrente
118700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
118800030715     C* Gestisco l'ultimo campo
118900030715     C                   IF        posA = *zeros
119000030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
119100030715     C* A questo punto "estraggo" il campo corrente
119200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
119300030715     C                                                    (posA-posDa))
119400030715     C                   LEAVE
119500030715     C                   ELSE
119600030715     C* A questo punto "estraggo" il campo corrente
119700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
119800030715     C                                                    (posA-posDa))
119900030715     C                   ENDIF
120000031204     C                   ENDIF
120100030715     C                   ENDIF
120200030715     C* Incremento il contatore d campo
120300030715     C                   EVAL      i = i +1
120400030715     C                   ENDDO
120500030715     C*
120600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
120700030715     C* delimitazione testo
120800030715     C                   EVAL      i = 1
120900030715     C                   DOW       i < %elem(SkSplitCSV)
121000030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
121100030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
121200030715     C                   EVAL      i = i + 1
121300030715     C                   ENDDO
121400030715     C*
121500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
121600030715     C                   EVAL      i = 1
121700030715     C                   DOW       i < %elem(SkSplitFLD)
121800030715     C*
121900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
122000030715     C                   IF        SkSplitFLD(i) <> *blanks AND
122100090623     C                             SkSplitCSV(i) <> *blanks
122200090623     C*
122300090623     C                   exsr      CARVAL
122400040510     C*
122500030715     C                   ENDIF
122600030715     C                   EVAL      i = i + 1
122700030715     C                   ENDDO
122800070925     C*
122900071029     C***  ===>  Gestione campi "particolari" fuori ciclo
123000080125     C*
123100090623     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
123200090623     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
123300090904     C                   EVAL      VABRMA = %subst(VABRMA:3)
123400070925     C*
123500071029     C***  <===  -----------------------------------------
123600070925     C*
123700070925     C* Considerazioni finali su CBO/CAS
123800080125     C                   IF        wFlgCAS = '1'
123900070925     C                   IF        VABCBO = '1'
124000070925     C                   EVAL      VABCBO = '4'
124100070925     C                   ENDIF
124200070925     C                   IF        VABCBO = '2'
124300070925     C                   EVAL      VABCBO = '6'
124400070925     C                   ENDIF
124500070925     C                   ENDIF
124600070925     C***  <===  ----------------------------
124700040510     C*
124800040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
124900040510     C                   IF        VABNSP = *zeros
125000040510     C                   SETON                                        31
125100040510     C                   ENDIF
125200040715     C*
125300040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
125400040715     C                   IF        VABLNP = *zeros
125500040715     C                   SETON                                        31
125600040715     C                   ENDIF
125700040510     C*
125800040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
125900040510     C                   IF        VABRMN = *zeros
126000040510     C                   Z-ADD     VABNSP        VABRMN
126100040510     C                   ENDIF
126200040510     C*
126300040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
126400040510     C                   IF        VABRMA = *blanks
126500040714     C                   MOVEL     VABRMN        VABRMA
126600040510     C                   ENDIF
126700040128     C*
126800040510     C* Se provincia nn valorizzata la reperisco
126900040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
127000040510     C                   IF        VABPRD = *blanks
127100040128     C                   CLEAR                   TISI95DS
127200040128     C                   EVAL      I95TCN = '3'
127300040128     C                   Z-ADD     datcor        I95DAT
127400040128     C                   EVAL      I95NAR = VABNZD
127500040128     C                   EVAL      I95CAP = VABCAD
127600040128     C                   EVAL      I95LOC = VABLOD
127700040128     C                   CALL      'TISI95R'
127800040128     C                   PARM                    TISI95DS
127900040128     C                   EVAL      VABPRD = O95PRV
128000040510     C                   ENDIF
128100020204     C*
128200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
128300020204     C                   EXSR      CHKIMPDIV
128400030715     C                   ENDIF
128500020204     C*
128600010607     C                   ENDSR
128700010607     C*----------------------------------------------------*
128800090623
128900090623
129000090623
129100090623     C*----------------------------------------------------*
129200090623     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT/FIVAX)
129300090623     C*----------------------------------------------------*
129400090623     C     IMPVATVAX     BEGSR
129500090623     C*
129600090623     C**********
129700090623     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
129800090623     C**********
129900090623     C                   IF        wGiro = *zeros
130000090623     C*
130100090623     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
130200090623     C                   EVAL      depvinDTA = vinDTA
130300090623     C*
130400090623     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
130500090623     C                   IF        %trim(ligexp(1))+%trim(ligexp(2))+
130600090623     C                             %trim(ligexp(3))+%trim(ligexp(4))+
130700090623     C                             %trim(ligexp(5))<>*blanks
130800090623     C                   EVAL      vindta = %trim(ligexp(1))+%trim(ligexp(2))+
130900090623     C                                      %trim(ligexp(3))+%trim(ligexp(4))+
131000090623     C                                      %trim(ligexp(5))
131100090623     C     ';':CharCSV   XLATE     vindta        vindta
131200090623     C                   ENDIF
131300090623     C*
131400090623     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
131500090623     C     minu:maiu     XLATE     vindta        vindta
131600090623     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
131700090623     C                   DOW       posDa <= %len(%trim(vindta))
131800090623     C*
131900090623     C* Gestisco il 1° campo
132000090623     C                   IF        i = 1
132100090623     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
132200090623     C                   EVAL      posDa = 1
132300090623     C* Eseguo lo scan x trovare la fine del primo campo
132400090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
132500090623     C* A questo punto "estraggo" il campo corrente
132600090623     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
132700090623     C                                                    (posA-posDa))
132800090623     C* X i campi successivi al 1°
132900090623     C                   ELSE
133000090623     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
133100090623     C                   EVAL      posDa = posA + 1
133200090623     C* Eseguo lo scan x trovare la fine del campo corrente
133300090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
133400090623     C* Gestisco l'ultimo campo
133500090623     C                   IF        posA = *zeros
133600090623     C                   EVAL      posA = %len(%trim(vindta)) + 1
133700090623     C* A questo punto "estraggo" il campo corrente
133800090623     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
133900090623     C                                                    (posA-posDa))
134000090623     C                   LEAVE
134100090623     C                   ELSE
134200090623     C* A questo punto "estraggo" il campo corrente
134300090623     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
134400090623     C                                                    (posA-posDa))
134500090623     C                   ENDIF
134600090623     C                   ENDIF
134700090623     C* Incremento il contatore d campo
134800090623     C                   EVAL      i = i +1
134900090623     C                   ENDDO
135000090623     C*
135100090623     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
135200090623     C* delimitazione testo
135300090623     C                   EVAL      i = 1
135400090623     C                   DOW       i < %elem(SkSplitFLD)
135500090623     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
135600090623     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
135700090623     C                   EVAL      i = i + 1
135800090623     C                   ENDDO
135900090623     C                   EVAL      wGiro = 1
136000090623     C* Ripristino la 1° riga originale
136100090623     C                   EVAL      vinDTA = depvinDTA
136200090623     C                   ELSE
136300090623     C                   EVAL      wGiro = 2
136400090623     C**********
136500090623     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
136600090623     C* d separatore campo
136700090623     C**********
136800090623     C                   EVAL      vindta = %trim(vindta)
136900090623     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
137000090623     C                   EVAL      vindta = %subst(vindta:2)
137100090623     C                   ENDDO
137200090623     C*
137300090623     C                   Z-ADD     *zeros        lunghInput        4 0
137400090623     C                   EVAL      lunghInput = %len(%trim(vindta))
137500090623     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
137600090623     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
137700090623     C                   EVAL      lunghInput = %len(%trim(vindta))
137800090623     C                   ENDDO
137900090623     C**********
138000090623     C* Effettuo lo split del campo dati d input
138100090623     C**********
138200090623     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
138300090623     C                   DOW       posDa <= %len(%trim(vindta))
138400090623     C*
138500090623     C* Gestisco il 1° campo
138600090623     C                   IF        i = 1
138700090623     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
138800090623     C                   EVAL      posDa = 1
138900090623     C* Eseguo lo scan x trovare la fine del primo campo
139000090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
139100090623     C* A questo punto "estraggo" il campo corrente
139200090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
139300090623     C                                                    (posA-posDa))
139400090623     C* X i campi successivi al 1°
139500090623     C                   ELSE
139600090623     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
139700090623     C                   EVAL      posDa = posA + 1
139800090623     C* Verifico che nn vi sia il campo nn valorizzato
139900090623     C                   IF        %subst(vindta:posDa:1) = CharCSV
140000090623     C* Se campo nn valorizzato skippo al prossimo
140100090623     C                   EVAL      SkSplitCSV(i) = *blanks
140200090623     C                   EVAL      posA  = posA + 1
140300090623     C                   ELSE
140400090623     C* Eseguo lo scan x trovare la fine del campo corrente
140500090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
140600090623     C* Gestisco l'ultimo campo
140700090623     C                   IF        posA = *zeros
140800090623     C                   EVAL      posA = %len(%trim(vindta)) + 1
140900090623     C* A questo punto "estraggo" il campo corrente
141000090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
141100090623     C                                                    (posA-posDa))
141200090623     C                   LEAVE
141300090623     C                   ELSE
141400090623     C* A questo punto "estraggo" il campo corrente
141500090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
141600090623     C                                                    (posA-posDa))
141700090623     C                   ENDIF
141800090623     C                   ENDIF
141900090623     C                   ENDIF
142000090623     C* Incremento il contatore d campo
142100090623     C                   EVAL      i = i +1
142200090623     C                   ENDDO
142300090623     C*
142400090623     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
142500090623     C* delimitazione testo
142600090623     C                   EVAL      i = 1
142700090623     C                   DOW       i < %elem(SkSplitCSV)
142800090623     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
142900090623     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
143000090623     C                   EVAL      i = i + 1
143100090623     C                   ENDDO
143200090623     C*
143300090623     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
143400090623     C                   EVAL      i = 1
143500090623     C                   DOW       i < %elem(SkSplitFLD)
143600090623     C*
143700090623     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
143800090623     C                   IF        SkSplitFLD(i) <> *blanks AND
143900090623     C                             SkSplitCSV(i) <> *blanks
144000090623     C*
144100090623     C                   exsr      CARVAL
144200090623     C*
144300090623     C                   ENDIF
144400090623     C                   EVAL      i = i + 1
144500090623     C                   ENDDO
144600090623     C*
144700090623     C***  ===>  Gestione campi "particolari" fuori ciclo
144800090623     C*
144900090623     C*
145000090623     C***  <===  -----------------------------------------
145100090623     C*
145200090623     C                   ENDIF
145300090623     C*
145400090623     C                   ENDSR
145500090623     C*----------------------------------------------------*
145600090623
145700090623
145800090623     C*----------------------------------------------------*
145900090623     C*  ROUTINE DI VALORIZZAZIONE CAMPI
146000090623     C*----------------------------------------------------*
146100090623     C     CARVAL        BEGSR
146200090623     C*
146300090623     C*** ATB
146400090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
146500090623     C                   EVAL      VABATB = SkSplitCSV(i)
146600090623     C                   ENDIF
146700090623     C*** RSD
146800090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
146900090623     C                   EVAL      VABRSD = SkSplitCSV(i)
147000090623     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
147100090623     C     '@':'A'       XLATE     VABRSD        VABRSD
147200090623     C* ==
147300090623     C                   ENDIF
147400090623     C*** RD2
147500090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
147600090623     C                   EVAL      VABRD2 = SkSplitCSV(i)
147700090623     C                   ENDIF
147800090623     C*** IND
147900090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
148000090623     C                   EVAL      VABIND = SkSplitCSV(i)
148100090623     C                   ENDIF
148200090623     C*** LOD
148300090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
148400090623     C                   EVAL      VABLOD = SkSplitCSV(i)
148500090623     C                   ENDIF
148600090623     C*** PRD
148700090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
148800090623     C                   EVAL      VABPRD = SkSplitCSV(i)
148900090623     C                   ENDIF
149000090623     C*** NZD
149100090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
149200090623     C                   EVAL      VABNZD = SkSplitCSV(i)
149300090623     C                   ENDIF
149400090623     C*** RMA
149500090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
149600090623     C                   EVAL      VABRMA = SkSplitCSV(i)
149700090623     C                   ENDIF
149800090623     C*** NOT
149900090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
150000090623     C                   EVAL      VABNOT = SkSplitCSV(i)
150100090623     C                   ENDIF
150200090623     C*** NT2
150300090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
150400090623     C                   EVAL      VABNT2 = SkSplitCSV(i)
150500090623     C                   ENDIF
150600090623     C*** VCA
150700090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
150800090623     C                   EVAL      VABVCA = SkSplitCSV(i)
150900090623     C                   ENDIF
151000090623     C*** VAS
151100090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
151200090623     C                   EVAL      VABVAS = SkSplitCSV(i)
151300090623     C                   ENDIF
151400090623     C*** FFD
151500090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
151600090623     C                   EVAL      VABFFD = SkSplitCSV(i)
151700090623     C                   ENDIF
151800090623     C*** GC1
151900090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
152000090623     C                   EVAL      VABGC1 = SkSplitCSV(i)
152100090623     C                   ENDIF
152200090623     C*** GC2
152300090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
152400090623     C                   EVAL      VABGC2 = SkSplitCSV(i)
152500090623     C                   ENDIF
152600090623     C*** TSP
152700090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
152800090623     C                   EVAL      VABTSP = SkSplitCSV(i)
152900090623     C                   ENDIF
153000090623     C*** CBO
153100090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
153200090623     C                   EVAL      VABCBO = SkSplitCSV(i)
153300090623     C                   ENDIF
153400090623     C*** NAS
153500090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
153600090623     C                   EVAL      VABNAS = SkSplitCSV(i)
153700090623     C                   ENDIF
153800090623     C*** TIC
153900090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
154000090623     C                   EVAL      VABTIC = SkSplitCSV(i)
154100090623     C                   ENDIF
154200090623     C*** GCA
154300090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
154400090623     C                   EVAL      VABGCA = SkSplitCSV(i)
154500090623     C                   ENDIF
154600090623     C*** XCO
154700090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
154800090623     C                   EVAL      VABXCO = SkSplitCSV(i)
154900090623     C                   ENDIF
155000090623     C*** CTM
155100090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
155200090623     C                   EVAL      VABCTM = SkSplitCSV(i)
155300090623     C                   ENDIF
155400090623     C*** TCR
155500090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
155600090623     C                   EVAL      VABTCR = SkSplitCSV(i)
155700090623     C                   ENDIF
155800090623     C*** CTS
155900090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
156000090623     C                   EVAL      VABCTS = SkSplitCSV(i)
156100090623     C                   ENDIF
156200090623     C*** FTM
156300090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
156400090623     C                   EVAL      VABFTM = SkSplitCSV(i)
156500090623     C                   ENDIF
156600090623     C*** VAD
156700090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
156800090623     C                   EVAL      VABVAD = SkSplitCSV(i)
156900090623     C                   ENDIF
157000090623     C*** GMA
157100090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
157200090623     C                   EVAL      VABGMA = SkSplitCSV(i)
157300090623     C                   ENDIF
157400090623     C*** GGA
157500090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
157600090623     C                   EVAL      VABGGA = SkSplitCSV(i)
157700090623     C                   ENDIF
157800090623     C*** GVA
157900090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
158000090623     C                   EVAL      VABGVA = SkSplitCSV(i)
158100090623     C                   ENDIF
158200090623     C*** TC1
158300090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
158400090623     C                   EVAL      VABTC1 = SkSplitCSV(i)
158500090623     C                   ENDIF
158600090623     C*** TC2
158700090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
158800090623     C                   EVAL      VABTC2 = SkSplitCSV(i)
158900090623     C                   ENDIF
159000090623     C*** SCL
159100090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
159200090623     C                   EVAL      VABSCL = SkSplitCSV(i)
159300090623     C                   ENDIF
159400090623     C*** RMO
159500090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
159600090623     C                   EVAL      VABRMO = SkSplitCSV(i)
159700090623     C                   ENDIF
159800090623     C*** NMO
159900090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
160000090623     C                   EVAL      VABNMO = SkSplitCSV(i)
160100090623     C                   ENDIF
160200090623     C*
160300090623     C* Reperisco quindi i campi numerici...
160400090623     C*** NSP
160500090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
160600090623     C                   EVAL      PiStr=SkSplitCSV(i)
160700090623     C                   EXSR      CHKNUM
160800090623     C                   IF        PiInt=*on
160900090623     C                   Z-ADD     PiVal         VABNSP
161000090623     C                   ELSE
161100090623     C                   SETON                                        31
161200090623     C                   EVAL      VABNSP = *zeros
161300090623     C                   EVAL      vinmsg = %trimr(vinmsg)
161400090623     C                             + ' ' + 'VABNSP'
161500090623     C                   ENDIF
161600090623     C                   ENDIF
161700090623     C*** RMN
161800090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
161900090623     C                   EVAL      PiStr=SkSplitCSV(i)
162000090623     C                   EXSR      CHKNUM
162100090623     C                   IF        PiInt=*on
162200090623     C                   Z-ADD     PiVal         VABRMN
162300090623     C                   ELSE
162400090623     C                   SETON                                        32
162500090623     C                   EVAL      VABRMN = *zeros
162600090623     C                   EVAL      vinmsg = %trimr(vinmsg)
162700090623     C                             + ' ' + 'VABRMN'
162800090623     C                   ENDIF
162900090623     C                   ENDIF
163000090623     C*** CAD
163100090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
163200090623     C                   EVAL      PiStr=SkSplitCSV(i)
163300090623     C                   EXSR      CHKNUM
163400090623     C                   IF        PiInt=*on
163500090623     C                   Z-ADD     PiVal         Num5_0
163600090623     C                   MOVEL(p)  Num5_0        VABCAD
163700090623     C                   ELSE
163800090623     C                   EVAL      VABCAD = SkSplitCSV(i)
163900090623     C                   SETON                                        32
164000090623     C                   EVAL      VABCAD = *zeros
164100090623     C                   EVAL      vinmsg = %trimr(vinmsg)
164200090623     C                             + ' ' + 'VABCAD'
164300090623     C                   ENDIF
164400090623     C                   ENDIF
164500090623     C*** NCL
164600090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
164700090623     C                   EVAL      PiStr=SkSplitCSV(i)
164800090623     C                   EXSR      CHKNUM
164900090623     C                   IF        PiInt=*on
165000090623     C                   Z-ADD     PiVal         VABNCL
165100090623     C                   ELSE
165200090623     C                   SETON                                        32
165300090623     C                   EVAL      VABNCL = *zeros
165400090623     C                   EVAL      vinmsg = %trimr(vinmsg)
165500090623     C                             + ' ' + 'VABNCL'
165600090623     C                   ENDIF
165700090623     C                   ENDIF
165800090623     C*** PKB
165900090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
166000090623     C                   EVAL      PiStr=SkSplitCSV(i)
166100090623     C                   EXSR      CHKNUM
166200090623     C                   IF        PiNum=*on
166300090623     C                   Z-ADD     PiVal         VABPKB
166400090623     C                   ELSE
166500090623     C                   SETON                                        32
166600090623     C                   EVAL      VABPKB = *zeros
166700090623     C                   EVAL      vinmsg = %trimr(vinmsg)
166800090623     C                             + ' ' + 'VABPKB'
166900090623     C                   ENDIF
167000090623     C                   ENDIF
167100090623     C*** CAS
167200090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
167300090623     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
167400090623     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
167500090623     C                             %trim(SkSplitCSV(i)) <> '0'      AND
167600090623     C                             %trim(SkSplitCSV(i)) <> '0,00'
167700090623     C                   EVAL      wFlgCAS = '1'
167800090623     C                   ENDIF
167900090623     C                   EVAL      PiStr=SkSplitCSV(i)
168000090623     C                   EXSR      CHKNUM
168100090623     C                   IF        PiNum=*on
168200090623     C                   Z-ADD     PiVal         VABCAS
168300090623     C                   ELSE
168400090623     C                   SETON                                        32
168500090623     C                   EVAL      VABCAS = *zeros
168600090623     C                   EVAL      vinmsg = %trimr(vinmsg)
168700090623     C                             + ' ' + 'VABCAS'
168800090623     C                   ENDIF
168900090623     C                   ENDIF
169000090623     C*** CCM
169100090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
169200090623     C                   EVAL      PiStr=SkSplitCSV(i)
169300090623     C                   EXSR      CHKNUM
169400090623     C                   IF        PiInt=*on      AND
169500090623     C                             PiVal<=9999999 AND
169600090623     C                             PiVal>*zeros
169700090623     C                   Z-ADD     PiVal         VABCCM
169800090623     C                   ELSE
169900090623     C                   SETON                                        32
170000090623     C                   EVAL      VABCCM = *zeros
170100090623     C                   EVAL      vinmsg = %trimr(vinmsg)
170200090623     C                             + ' ' + 'VABCCM'
170300090623     C                   ENDIF
170400090623     C                   ENDIF
170500090623     C*** LNP
170600090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
170700090623     C                   EVAL      PiStr=SkSplitCSV(i)
170800090623     C                   EXSR      CHKNUM
170900090623     C                   IF        PiInt=*on    AND
171000090623     C                             PiVal<=999   AND
171100090623     C                             PiVal>*zeros
171200090623     C                   Z-ADD     PiVal         VABLNP
171300090623     C                   ELSE
171400090623     C                   SETON                                        32
171500090623     C                   EVAL      VABLNP = *zeros
171600090623     C                   EVAL      vinmsg = %trimr(vinmsg)
171700090623     C                             + ' ' + 'VABLNP'
171800090623     C                   ENDIF
171900090623     C                   ENDIF
172000090623     C*** NRS
172100090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
172200090623     C                   EVAL      PiStr=SkSplitCSV(i)
172300090623     C                   EXSR      CHKNUM
172400090623     C                   IF        PiInt=*on AND
172500090623     C                             PiVal<=99
172600090623     C                   Z-ADD     PiVal         VABNRS
172700090623     C                   ELSE
172800090623     C                   SETON                                        32
172900090623     C                   EVAL      vinmsg = %trimr(vinmsg)
173000090623     C                             + ' ' + 'VABNRS'
173100090623     C                   ENDIF
173200090623     C                   ENDIF
173300090623     C*** LNA
173400090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
173500090623     C                   EVAL      PiStr=SkSplitCSV(i)
173600090623     C                   EXSR      CHKNUM
173700090623     C                   IF        PiInt=*on    AND
173800090623     C                             PiVal<=999   AND
173900090623     C                             PiVal>*zeros
174000090623     C                   Z-ADD     PiVal         VABLNA
174100090623     C                   ELSE
174200090623     C                   SETON                                        32
174300090623     C                   EVAL      VABLNA = *zeros
174400090623     C                   EVAL      vinmsg = %trimr(vinmsg)
174500090623     C                             + ' ' + 'VABLNA'
174600090623     C                   ENDIF
174700090623     C                   ENDIF
174800090623     C*** CTR
174900090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
175000090623     C                   EVAL      PiStr=SkSplitCSV(i)
175100090623     C                   EXSR      CHKNUM
175200090623     C                   IF        PiInt=*on  AND
175300090623     C                             PiVal<=999
175400090623     C                   Z-ADD     PiVal         VABCTR
175500090623     C                   ELSE
175600090623     C                   SETON                                        32
175700090623     C                   EVAL      vinmsg = %trimr(vinmsg)
175800090623     C                             + ' ' + 'VABCTR'
175900090623     C                   ENDIF
176000090623     C                   ENDIF
176100090623     C*** IAS
176200090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
176300090623     C                   EVAL      PiStr=SkSplitCSV(i)
176400090623     C                   EXSR      CHKNUM
176500090623     C                   IF        PiNum=*on
176600090623     C                   Z-ADD     PiVal         VABIAS
176700090623     C                   ELSE
176800090623     C                   SETON                                        32
176900090623     C                   EVAL      vinmsg = %trimr(vinmsg)
177000090623     C                             + ' ' + 'VABIAS'
177100090623     C                   ENDIF
177200090623     C                   ENDIF
177300090623     C*** VLB
177400090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
177500090623     C                   EVAL      PiStr=SkSplitCSV(i)
177600090623     C                   EXSR      CHKNUM
177700090623     C                   IF        PiNum=*on
177800090623     C                   Z-ADD     PiVal         VABVLB
177900090623     C                   ELSE
178000090623     C                   SETON                                        32
178100090623     C                   EVAL      vinmsg = %trimr(vinmsg)
178200090623     C                             + ' ' + 'VABVLB'
178300090623     C                   ENDIF
178400090623     C                   ENDIF
178500090623     C*** QFT
178600090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
178700090623     C                   EVAL      PiStr=SkSplitCSV(i)
178800090623     C                   EXSR      CHKNUM
178900090623     C                   IF        PiNum=*on
179000090623     C                   Z-ADD     PiVal         VABQFT
179100090623     C                   ELSE
179200090623     C                   SETON                                        32
179300090623     C                   EVAL      vinmsg = %trimr(vinmsg)
179400090623     C                             + ' ' + 'VABQFT'
179500090623     C                   ENDIF
179600090623     C                   ENDIF
179700090623     C*** NCD
179800090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
179900090623     C                   EVAL      PiStr=SkSplitCSV(i)
180000090623     C                   EXSR      CHKNUM
180100090623     C                   IF        PiInt=*on      AND
180200090623     C                             PiVal<=9999999
180300090623     C                   Z-ADD     PiVal         VABNCD
180400090623     C                   ELSE
180500090623     C                   SETON                                        32
180600090623     C                   EVAL      vinmsg = %trimr(vinmsg)
180700090623     C                             + ' ' + 'VABNCD'
180800090623     C                   ENDIF
180900090623     C                   ENDIF
181000090623     C*** NCA
181100090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
181200090623     C                   EVAL      PiStr=SkSplitCSV(i)
181300090623     C                   EXSR      CHKNUM
181400090623     C                   IF        PiInt=*on      AND
181500090623     C                             PiVal<=9999999
181600090623     C                   Z-ADD     PiVal         VABNCA
181700090623     C                   ELSE
181800090623     C                   SETON                                        32
181900090623     C                   EVAL      vinmsg = %trimr(vinmsg)
182000090623     C                             + ' ' + 'VABNCA'
182100090623     C                   ENDIF
182200090623     C                   ENDIF
182300090623     C*** ZNC
182400090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
182500090623     C                   EVAL      PiStr=SkSplitCSV(i)
182600090623     C                   EXSR      CHKNUM
182700090623     C                   IF        PiInt=*on AND
182800090623     C                             PiVal<=99
182900090623     C                   Z-ADD     PiVal         VABZNC
183000090623     C                   ELSE
183100090623     C                   SETON                                        32
183200090623     C                   EVAL      vinmsg = %trimr(vinmsg)
183300090623     C                             + ' ' + 'VABZNC'
183400090623     C                   ENDIF
183500090623     C                   ENDIF
183600090623     C*** DCR
183700090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
183800090623     C                   EVAL      PiStr=SkSplitCSV(i)
183900090623     C                   EXSR      CHKNUM
184000090623     C                   IF        PiInt=*on       AND
184100090623     C                             PiVal<=99999999
184200090623     C                   Z-ADD     PiVal         VABDCR
184300090623     C                   ELSE
184400090623     C                   SETON                                        32
184500090623     C                   EVAL      vinmsg = %trimr(vinmsg)
184600090623     C                             + ' ' + 'VABDCR'
184700090623     C                   ENDIF
184800090623     C                   ENDIF
184900090623     C*** HCR
185000090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
185100090623     C                   EVAL      PiStr=SkSplitCSV(i)
185200090623     C                   EXSR      CHKNUM
185300090623     C                   IF        PiInt=*on   AND
185400090623     C                             PiVal<=9999
185500090623     C                   Z-ADD     PiVal         VABHCR
185600090623     C                   ELSE
185700090623     C                   SETON                                        32
185800090623     C                   EVAL      vinmsg = %trimr(vinmsg)
185900090623     C                             + ' ' + 'VABHCR'
186000090623     C                   ENDIF
186100090623     C                   ENDIF
186200090623     C*** VMD
186300090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
186400090623     C                   EVAL      PiStr=SkSplitCSV(i)
186500090623     C                   EXSR      CHKNUM
186600090623     C                   IF        PiNum=*on
186700090623     C                   Z-ADD     PiVal         VABVMD
186800090623     C                   ELSE
186900090623     C                   SETON                                        32
187000090623     C                   EVAL      vinmsg = %trimr(vinmsg)
187100090623     C                             + ' ' + 'VABVMD'
187200090623     C                   ENDIF
187300090623     C                   ENDIF
187400090623     C*** ANT
187500090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
187600090623     C                   EVAL      PiStr=SkSplitCSV(i)
187700090623     C                   EXSR      CHKNUM
187800090623     C                   IF        PiInt=*on        AND
187900090623     C                             PiVal<=999999999
188000090623     C                   Z-ADD     PiVal         VABANT
188100090623     C                   ELSE
188200090623     C                   SETON                                        32
188300090623     C                   EVAL      vinmsg = %trimr(vinmsg)
188400090623     C                             + ' ' + 'VABANT'
188500090623     C                   ENDIF
188600090623     C                   ENDIF
188700090623     C*** CMO
188800090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
188900090623     C                   EVAL      PiStr=SkSplitCSV(i)
189000090623     C                   EXSR      CHKNUM
189100090623     C                   IF        PiInt=*on
189200090623     C                   Z-ADD     PiVal         Num5_0
189300090623     C                   MOVEL(p)  Num5_0        VABCMO
189400090623     C                   ELSE
189500090623     C                   SETON                                        32
189600090623     C                   EVAL      VABCMO = *zeros
189700090623     C                   EVAL      vinmsg = %trimr(vinmsg)
189800090623     C                             + ' ' + 'VABCMO'
189900090623     C                   ENDIF
190000090623     C                   ENDIF
190100090623     C*
190200090623     C* Reperisco i campi del FIVAT
190300090623     C*** VATNOT
190400090623     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
190500090623     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
190600090623     C                   ENDIF
190700090623     C*** VATTRC
190800090623     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
190900090623     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
191000090623     C                   ENDIF
191100090623     C*** VATNOT_A
191200090623     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
191300090623     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
191400090623     C                   ENDIF
191500090623     C*** VATNOT_B
191600090623     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
191700090623     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
191800090623     C                   ENDIF
191900090623     C*** VATNOT_E
192000090623     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
192100090623     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
192200090623     C                   ENDIF
192300090623     C*
192400090623     C***  ===>  Gestione campi "particolari" dentro ciclo
192500090623     C*
192600090623     C*** W_VABNSP
192700090623     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNSP'
192800090623     C                   EVAL      PiStr=SkSplitCSV(i)
192900090623     C                   EXSR      CHKNUM
193000090623     C                   IF        PiInt=*on
193100090623     C                   Z-ADD     PiVal         VABNSP
193200090623     C                   Z-ADD     PiVal         VABRMN
193300090623     C                   ELSE
193400090623     C                   SETON                                        31
193500090623     C                   EVAL      VABNSP = *zeros
193600090623     C                   EVAL      VABRMN = *zeros
193700090623     C                   EVAL      vinmsg = %trimr(vinmsg)
193800090623     C                             + ' ' + 'VABNSP VABRMN'
193900090623     C                   ENDIF
194000090623     C                   ENDIF
194100090623     C*** W_VABIND_1
194200090623     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABIND_1'
194300090623     C                   EVAL      VABIND = %trim(VABIND) + ' ' +
194400090623     C                                      SkSplitCSV(i)
194500090623     C                   ENDIF
194600090623     C*** W_VABIND_2
194700090623     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABIND_2'
194800090623     C                   EVAL      VABIND = %trim(VABIND) + ' ' +
194900090623     C                                      SkSplitCSV(i)
195000090623     C                   ENDIF
195100090623     C*** W_VABNZD
195200090623     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNZD'
195300090623     C                   IF        SkSplitCSV(i) <> 'IT'
195400090623     C                   EVAL      VABNZD = SkSplitCSV(i)
195500090623     C                   ENDIF
195600090623     C                   ENDIF
195700090904     C*** W_VABRMA_1
195800090623     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMA_1'
195900090623     C                   EVAL      VABRMA = %trim(VABRMA) + ' ' +
196000090623     C                                      SkSplitCSV(i)
196100090623     C                   ENDIF
196200090904     C*** W_VABRMA_2
196300090623     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMA_2'
196400090623     C                   EVAL      VABRMA = %trim(VABRMA) + ' ' +
196500090623     C                                      SkSplitCSV(i)
196600090623     C                   ENDIF
196700090623     C*** W_VABNOT_1
196800090623     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT_1'
196900090623     C                   MOVEL     *blanks       wNOTE            70
197000090623     C                   EVAL      wNOTE  = %trim(wNOTE) + ' ' +
197100090623     C                                      SkSplitCSV(i)
197200090623     C                   ENDIF
197300090623     C*** W_VABNOT_2
197400090623     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT_2'
197500090623     C                   EVAL      wNOTE  = %trim(wNOTE) + ' ' +
197600090623     C                                      SkSplitCSV(i)
197700090623     C                   ENDIF
197800090623     C*** W_VABNOT_3
197900090623     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT_3'
198000090623     C                   EVAL      wNOTE  = %trim(wNOTE) + ' ' +
198100090623     C                                      SkSplitCSV(i)
198200090623     C                   ENDIF
198300090623     C*** W_VABNOT_4
198400090623     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT_4'
198500090623     C                   EVAL      wNOTE  = %trim(wNOTE) + ' ' +
198600090623     C                                      SkSplitCSV(i)
198700090623     C                   ENDIF
198800090623     C*** W_VABNOT_5
198900090623     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT_5'
199000090623     C                   EVAL      wNOTE  = %trim(wNOTE) + ' ' +
199100090623     C                                      SkSplitCSV(i)
199200090623     C                   ENDIF
199300090904     C*
199400090904     C* Sommo peso e volume solamente a rottura d "chi sono"
199500090904     C*
199600090904     C* => se "chi sono" campo 7
199700090904     C                   IF        SkSplitCSV(7) <> *blanks
199800090904     C*** W_VABPKB
199900090904     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABPKB'
200000090909     C                   IF        SkSplitCSV(7) <> sVATNOT_E
200100090904     C                   EVAL      PiStr='0'+SkSplitCSV(i)
200200090904     C                   EXSR      CHKNUM
200300090904     C                   IF        PiNum=*on
200400090904     C                   ADD       PiVal         VABPKB
200500090904     C                   ELSE
200600090904     C                   SETON                                        32
200700090904     C                   EVAL      vinmsg = %trimr(vinmsg)
200800090904     C                             + ' ' + 'VABPKB'
200900090904     C                   ENDIF
201000090904     C                   ENDIF
201100090904     C                   ENDIF
201200090904     C*** W_VABVLB
201300090904     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABVLB'
201400090909     C                   IF        SkSplitCSV(7) <> sVATNOT_E
201500090904     C                   EVAL      PiStr='0'+SkSplitCSV(i)
201600090904     C                   EXSR      CHKNUM
201700090904     C                   IF        PiNum=*on
201800090904     C                   ADD       PiVal         VABVLB
201900090904     C                   ELSE
202000090904     C                   SETON                                        32
202100090904     C                   EVAL      vinmsg = %trimr(vinmsg)
202200090904     C                             + ' ' + 'VABVLB'
202300090904     C                   ENDIF
202400090904     C                   ENDIF
202500090904     C                   ENDIF
202600090904     C*
202700090904     C                   ELSE
202800090904     C*
202900090904     C* => se "chi sono" campo 26
203000090904     C                   IF        SkSplitCSV(26) <> *blanks
203100090904     C*** W_VABPKB
203200090904     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABPKB'
203300090904     C                   IF        '00'+SkSplitCSV(26) <> sVATNOT_E
203400090904     C                   EVAL      PiStr='0'+SkSplitCSV(i)
203500090904     C                   EXSR      CHKNUM
203600090904     C                   IF        PiNum=*on
203700090904     C                   ADD       PiVal         VABPKB
203800090904     C                   ELSE
203900090904     C                   SETON                                        32
204000090904     C                   EVAL      vinmsg = %trimr(vinmsg)
204100090904     C                             + ' ' + 'VABPKB'
204200090904     C                   ENDIF
204300090904     C                   ENDIF
204400090904     C                   ENDIF
204500090904     C*** W_VABVLB
204600090904     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABVLB'
204700090904     C                   IF        '00'+SkSplitCSV(26) <> sVATNOT_E
204800090904     C                   EVAL      PiStr='0'+SkSplitCSV(i)
204900090904     C                   EXSR      CHKNUM
205000090904     C                   IF        PiNum=*on
205100090904     C                   ADD       PiVal         VABVLB
205200090904     C                   ELSE
205300090904     C                   SETON                                        32
205400090904     C                   EVAL      vinmsg = %trimr(vinmsg)
205500090904     C                             + ' ' + 'VABVLB'
205600090904     C                   ENDIF
205700090904     C                   ENDIF
205800090904     C                   ENDIF
205900090904     C*
206000090904     C                   ENDIF
206100090904     C                   ENDIF
206200090904     C*
206300090904     C*** W_VATNOT_E_1
206400090904     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_E_1'
206500090904     C                   EVAL      wVATNOT_E_1 = '00'+%trim(SkSplitCSV(i))
206600090703     C                   ENDIF
206700090904     C*** W_VATNOT_E_2
206800090904     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_E_2'
206900090909     C                   EVAL      wVATNOT_E_2 = %trim(SkSplitCSV(i))
207000090904     C                   ENDIF
207100090623     C*** W_VAXDTT_1
207200090623     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAXDTT_1'
207300090623     C                   EVAL      wVAXDTT_1 = SkSplitCSV(i)
207400090623     C                   ENDIF
207500090623     C*** W_VAXDTT_2
207600090623     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAXDTT_2'
207700090623     C                   EVAL      wVAXDTT_2 = SkSplitCSV(i)
207800090623     C                   ENDIF
207900090623     C*** W_VAXDTT_3
208000090624     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAXDTT_3'
208100090624     C                   EVAL      wVAXDTT_3 = SkSplitCSV(i)
208200090624     C                   ENDIF
208300090624     C*** W_VAX_NUMBP
208400090624     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAX_NUMBP'
208500090624     C                   EVAL      wVAX_NUMBP = SkSplitCSV(i)
208600090624     C                   ENDIF
208700090624     C*** W_VAX_NUMORD
208800090624     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAX_NUMORD'
208900090624     C                   EVAL      wVAX_NUMORD = SkSplitCSV(i)
209000090624     C                   ENDIF
209100090624     C*** W_VAX_CODCLI
209200090624     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAX_CODCLI'
209300090624     C                   EVAL      wVAX_CODCLI = SkSplitCSV(i)
209400090624     C                   ENDIF
209500090623     C*
209600090623     C***  <===  -----------------------------------------
209700090908     C*
209800090908     C                   IF        %trim(wVATNOT_E_2) <> *blanks
209900090908     C                   EVAL      wVATNOT_E = wVATNOT_E_2
210000090908     C                   ELSE
210100090908     C                   EVAL      wVATNOT_E = wVATNOT_E_1
210200090908     C                   ENDIF
210300090623     C*
210400090623     C                   ENDSR
210500090623     C***
210600090623
210700010601
210800020204
210900020204     C*----------------------------------------------------*
211000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
211100020204     C*----------------------------------------------------*
211200020204     C     CHKIMPDIV     BEGSR
211300020204     C*
211400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
211500020204     C                   Z-ADD     *zeros        wrkDec            9 9
211600020204     C*
211700020204     C* Come prima cosa effettuo considerazioni sulla divisa
211800020204     C                   IF        vabIAS > *zeros
211900020204     C                   IF        vabVAS <> 'EUR'
212000020204     C                   EVAL      vabVAS =  'ITL'
212100020204     C                   ENDIF
212200020204     C                   ENDIF
212300020204     C*
212400020204     C                   IF        vabCAS > *zeros
212500020204     C                   IF        vabVCA <> 'EUR'
212600020204     C                   EVAL      vabVCA =  'ITL'
212700020204     C                   ENDIF
212800020204     C                   ENDIF
212900020204     C*
213000020204     C                   IF        vabVMD > *zeros
213100020204     C                   IF        vabVAD <> 'EUR'
213200020204     C                   EVAL      vabVAD =  'ITL'
213300020204     C                   ENDIF
213400020204     C                   ENDIF
213500020204     C*
213600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
213700020204     C                   Z-ADD     vabIAS        wrkDec
213800020204     C                   IF        wrkDec > *zeros
213900020204     C                   IF        vabVAS = 'ITL'
214000020204     C                   EVAL      vabIAS = *zeros
214100020204     C                   ENDIF
214200020204     C                   ENDIF
214300020204     C*
214400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
214500020204     C                   Z-ADD     vabCAS        wrkDec
214600020204     C                   IF        wrkDec > *zeros
214700020204     C                   IF        vabVCA = 'ITL'
214800020204     C                   EVAL      vabCAS = *zeros
214900020204     C                   ENDIF
215000020204     C                   ENDIF
215100020204     C*
215200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
215300020204     C                   Z-ADD     vabVMD        wrkDec
215400020204     C                   IF        wrkDec > *zeros
215500020204     C                   IF        vabVAD = 'ITL'
215600020204     C                   EVAL      vabVMD = *zeros
215700020204     C                   ENDIF
215800020204     C                   ENDIF
215900020204     C*
216000020204     C                   ENDSR
216100020204     C***
216200080617
216300080617
216400080617
216500080617     C*----------------------------------------------------*
216600080617     C*  CONTROLLO NUMERICITA' CAMPI
216700080617     C*----------------------------------------------------*
216800080617     C     CHKNUM        BEGSR
216900080617     C*
217000080617     C                   IF        PiDecChr = *blanks
217100080617     C                   EVAL      PiDecChr = CharNUM
217200080617     C                   ENDIF
217300080617     C*
217400080617     C                   callp     UBISNUM_Check(PiStr
217500080617     C                                          :PiDecChr
217600080617     C                                          :PiVal
217700080617     C                                          :PiNum
217800080617     C                                          :PiInt)
217900080617     C*
218000080617     C                   ENDSR
218100080617     C***
218200010330
218300010601
218400010601
218500010601
218600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
218700010601     C     repfil        BEGSR
218800010601     C*
218900010601     C                   if        invfil = *zeros and
219000010601     C                             depfil > *zeros and
219100010629     C                             (vinflg = *blanks or
219200010629     C                              vinflg = *zeros)
219300010601     C
219400010601     C                   eval      invfil = depfil
219500010601     C                   endif
219600010601     C*
219700010601     C                   if        depfil <> invfil and
219800010601     C                             invfil > *zeros
219900010601     C                   eval      flgMulti = '1'
220000010601     C                   if        vinflg = *blanks
220100010601     C                   add       1             cntNonEl
220200010601     C                   endif
220300010601     C                   endif
220400010601     C*
220500010601     C                   if        vinflg = '2'
220600010601     C                   eval      flgStato = '2'
220700010601     C                   endif
220800010601     C*
220900010601     C                   ENDSR
221000010601     C***
221100010601
221200010601
221300010601
221400010330
221500040119      /TITLE Invio dei dati al punto operativo.
221600040119     C     invio         BEGSR
221700090623     C*
221800090623     C* 1° invio FIVAX
221900090623     C                   reset                   dscmz
222000090623     C                   move      vlrpoi        cmzdst
222100090623     C                   eval      cmzfld = 'FIVAXWWR'
222200090623     C                   eval      cmzmbd = vlrhdl
222300090623     C                   eval      %subst(cmzmbd:1:1) = 'M'
222400090623     C***                if        prmfir = *blanks
222500090623     C                   eval      cmzfla = 'FIVAX00F'
222600090623     C                   eval      cmzmba = 'FIVAX00F'
222700090623     C***                else
222800090623     C***                eval      cmzfla = prmfir
222900090623     C***                eval      cmzmba = prmfir
223000090623     C***                endif
223100090623     C                   eval      cmznrr = *zeros
223200090623     C                   move      §ctrokvx      cmznrr
223300090623     C                   eval      cmzlba = vlrfl1
223400090623     C                   call(e)   'TIS711C'
223500090623     C                   parm                    dscmz
223600090623     C                   parm      *blanks       esito
223700090623     C                   if        %error
223800090623     C                             or cmzerr = '1'
223900090623     C                             or esito  = '1'
224000090623     C                   eval      wrkesito = '3'
224100090623     C                   else
224200040119     C*
224300090623     C* 2° invio FIVAT
224400040119     C                   reset                   dscmz
224500040119     C                   move      vlrpoi        cmzdst
224600040119     C                   eval      cmzfld = 'FIVATWWR'
224700040119     C                   eval      cmzmbd = vlrhdl
224800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
224900040119     C***                if        prmfir = *blanks
225000040119     C                   eval      cmzfla = 'FIVAT00F'
225100040119     C                   eval      cmzmba = 'FIVAT00F'
225200040119     C***                else
225300040119     C***                eval      cmzfla = prmfir
225400040119     C***                eval      cmzmba = prmfir
225500040119     C***                endif
225600040119     C                   eval      cmznrr = *zeros
225700040119     C                   move      §ctrokvt      cmznrr
225800040119     C                   eval      cmzlba = vlrfl1
225900040119     C                   call(e)   'TIS711C'
226000040119     C                   parm                    dscmz
226100040119     C                   parm      *blanks       esito
226200040119     C                   if        %error
226300040119     C                             or cmzerr = '1'
226400040119     C                             or esito  = '1'
226500040119     C                   eval      wrkesito = '3'
226600040119     C                   else
226700040119     C*
226800090623     C* 3° invio FIVAB
226900040119     C                   reset                   dscmz
227000040119     C                   move      vlrpoi        cmzdst
227100040119     C                   eval      cmzfld = vlrfou
227200040119     C                   eval      cmzmbd = vlrhdl
227300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
227400040119     C***                if        prmfir = *blanks
227500040119     C                   eval      cmzfla = 'FIVAB00F'
227600040119     C                   eval      cmzmba = 'FIVAB00F'
227700040119     C***                else
227800040119     C***                eval      cmzfla = prmfir
227900040119     C***                eval      cmzmba = prmfir
228000040119     C***                endif
228100040119     C                   eval      cmznrr = *zeros
228200040119     C                   move      §ctrokvb      cmznrr
228300040119     C                   eval      cmzlba = vlrfl1
228400040119     C                   call(e)   'TIS711C'
228500040119     C                   parm                    dscmz
228600040119     C                   parm      *blanks       esito
228700040119     C                   if        %error
228800040119     C                             or cmzerr = '1'
228900040119     C                             or esito  = '1'
229000040119     C                   eval      wrkesito = '3'
229100040119     C                   endif
229200040119     C                   endif
229300090623     C                   endif
229400040119     C*
229500040119     C                   ENDSR
229600040119     C***
229700010601
229800010601
229900010601
230000010601
230100010601      /TITLE Invio dei dati al punto operativo.
230200010601     C     opeini        BEGSR
230300010601     C*
230400010601     C* Inizializzo flag e contatori operativi
230500010601     C                   movel     '0'           flgGiro           1
230600010601     C                   movel     '0'           flgMulti          1
230700010601     C                   movel     '1'           flgStato          1
230800010615     C                   movel     '0'           flgOk             1
230900010601     C                   z-add     *zeros        cntNonEl         10 0
231000010601     C                   z-add     *zeros        depfil            3 0
231100010601     C                   z-add     *zeros        invfil            3 0
231200010601     C*
231300010601     C                   ENDSR
231400010601     C***
231500070326
231600070326
231700070326
231800070326
231900080916     C     *pssr         BEGSR
232000070329     C*
232100080916     C                   if        %open(tivin00r)
232200080916     C                   close     tivin00r
232300080916     C                   endif
232400080916     C                   if        %open(fivabwwr)
232500080916     C                   close     fivabwwr
232600080916     C                   endif
232700080916     C                   if        %open(fivatwwr)
232800080916     C                   close     fivatwwr
232900080916     C                   endif
233000090623     C                   if        %open(fivaxwwr)
233100090623     C                   close     fivaxwwr
233200090623     C                   endif
233300070326     C*
233400070326     C* Effettuo la chiamata al CLLE preposto
233500090623     C                   call(e)   'TITVVTXC'
233600080916     C                   parm                    parccm
233700080916     C                   parm                    parmbr
233800080916     C                   parm      '2'           paropz
233900070326     C*
234000080916     C                   eval      wrkesito = '2'
234100070404     C*
234200080916     C                   seton                                        LR
234300070326     C*
234400080916     C                   ENDSR     '*CANCL'
234500070326     C***
234600070326
234700070326
234800010330
234900010330
235000000613     C     *inzsr        BEGSR
235100990910     C*
235200990910     C     *entry        plist
235300990920     C                   parm                    tivlrds
235400990921     C                   parm      wrkesito      esito
235500000724     C                   parm                    prmlit
235600000710     C                   parm                    prmfir
235700010330     C*
235800010330     C* CALCOLA LA DATA CORRENTE
235900010330     C                   time                    wn14             14 0
236000010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
236100010330     C                   z-add     wn8           g08dat
236200010330     C                   z-add     *zeros        g08inv
236300010330     C                   movel     '0'           g08err
236400010330     C                   call      'XSRDA8'
236500010330     C                   parm                    wlbda8
236600010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
236700000613     C*
236800000613     C                   ENDSR
236900000613     C***
237000990908
237100090623     Otitv1q7p  E            riepilogo         2
237200990915     O                                              'Upload via Internet'
237300990915     O                                           +1 'Traduzione TIVIN00R -'
237400021025     O                                           +1 'FIVABWWR'
237500990915     O                                           +1 'Testate bolle'
237600990915     O          E            riepilogo   2
237700990915     O                       wrkdata
237800990915     O                       wrkora              +1
237900990915     O                       procname            +1
238000990915     O          E            riepilogo   2
238100990915     O                                              'Cliente..................:'
238200010605     O                       VABCCM        z     +1
238300990915     O          E            riepilogo   2
238400990920     O                                              'Riferimento Strategi.....:'
238500990920     O                       vlrhdl              +1
238600990915     O          E            riepilogo   2
238700990915     O                                              'Giusti...................:'
238800040119     O                       §CTROKVB      2   +  1
238900990915     O          E            riepilogo   2
239000990915     O                                              'Sbagliati e corretti.....:'
239100971022     O                       §CTRMO        2   +  1
239200990915     O          E            riepilogo   2
239300990915     O                                              'Sbagliati e scartati.....:'
239400971022     O                       §CTRNO        2   +  1
239500000613
239600090623     Otitv1q7ps E            testdett          2
239700000613     O                                              'Upload via Internet'
239800000613     O                                           +1 'Traduzione TIVIN00R -'
239900021025     O                                           +1 'FIVABWWR'
240000000613     O                                           +1 'Testate bolle'
240100000616     O          E            testdett    3
240200000613     O                                           +2 'N° rec'
240300000613     O                                           +3 'Anteprima contenuto'
240400000616     O          E            rigadett    2
240500000613     O                       rrnum               +2
240600000621     O                       recko               +3
240700000616     O          E            findett     2
240800000613     O                       wrkdata
240900000613     O                       wrkora              +1
241000000613     O                       procname            +1
241100000616     O          E            findett     2
241200000613     O                                              'Cliente..................:'
241300010605     O                       VABCCM        z     +1
241400000616     O          E            findett     2
241500000613     O                                              'Riferimento Strategi.....:'
241600000613     O                       vlrhdl              +1
241700000616     O          E            findett     2
241800000613     O                                              'Giusti...................:'
241900040119     O                       §CTROKVB      2   +  1
242000000616     O          E            findett     2
242100000613     O                                              'Sbagliati e corretti.....:'
242200000613     O                       §CTRMO        2   +  1
242300000616     O          E            findett     2
242400000613     O                                              'Sbagliati e scartati.....:'
242500000613     O                       §CTRNO        2   +  1
242600000613** CMD - COMANDI CL
242700090623OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1Q7P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
242800090623OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1Q7PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
242900090623DLTOVR FILE(TITV1Q7P TITV1Q7PS) LVL(*)
243000010606
243100000613
243200090623** ENTEXP - INTESTAZIONE COLONNE PERSONALIZZATA TIPO RECORD "ENTEXP"
243300090904X;X;VABRMA;X;W_VABNSP;W_VAX_NUMBP;X;X;X;X;X;X;X;X;VABRSD;VABRD2;
243400090623W_VABIND_1;W_VABIND_2;VABLOD;VABCAD;X;W_VABNZD;X;X;
243500090623W_VABNOT_1;W_VABNOT_2;W_VABNOT_3;W_VABNOT_4;W_VABNOT_5;VABPRD;X;X;X;
243600090624X;X;W_VAX_NUMORD;W_VAX_CODCLI;X;X;X;X;X;X;X;X;X
243700090623
243800090623** LIGEXP - INTESTAZIONE COLONNE PERSONALIZZATA TIPO RECORD "LIGEXP"
243900090904X;X;X;X;W_VABPKB;X;W_VATNOT_E_2;X;X;X;X;X;W_VAXDTT_1;X;X;W_VAXDTT_2;X;
244000090904W_VAXDTT_3;X;X;X;X;X;X;X;W_VATNOT_E_1;X;X;X;X;X;X;X;X;X;X;
244100090623X;X;X;X;X;X;X;
244200090623
244300090623
244400090624** PRTCONST - COSTANTI IN STAMPA
244500090624CODICE CLIENTE         NUM.ORDINE        NUM.BP
244600090624_______________        __________        __________
244700090703   ARTICOLO       DESCRIZIONE                                Q.TA'
244800090624   __________     ___________________________________        __________
244900090624____________________________________________________________________________
