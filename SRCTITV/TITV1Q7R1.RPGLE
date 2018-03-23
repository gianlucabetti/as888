000100090911      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR/EDIVAXWR.
000200080617     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600090911     FEDIVABwr  O    E             DISK    usropn
000700090911     FEDIVATwr  O    E             DISK    usropn
000800090911     FEDIVAXwr  O    E             DISK    usropn
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
005000090911     D wNomeFile       s             30    INZ(*blanks)
005100090911     D wCMR            s             35    INZ(*blanks)
005200030715     D*------------------
005300030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005400030715     D*------------------
005500090624     D SkSplitFLD      S             15    DIM(100)
005600030715     D SkSplitCSV      S            256    DIM(100)
005700030715     D CharCSV         S              1
005800030715     D CharTXT         S              1
005900030715     D CharNUM         S              1
006000030715     D posDa           S              3  0 INZ(*zeros)
006100030715     D posA            S              3  0 INZ(*zeros)
006200030715     D i               s              3  0 INZ(1)
006300030715     D wGiro           s              1  0 INZ(*zeros)
006400080125     D wVATNOT_A       s                   LIKE(VATNOT)
006500080125     D wVATNOT_B       s                   LIKE(VATNOT)
006600090904     D wVATNOT_E       s                   LIKE(VATNOT)
006700090904     D wVATNOT_E_1     s                   LIKE(VATNOT)
006800090904     D wVATNOT_E_2     s                   LIKE(VATNOT)
006900030822     D*------------------
007000030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
007100030822     D*------------------
007200030822     D posDaDft        S              3  0 INZ(*zeros)
007300030822     D posADft         S              3  0 INZ(*zeros)
007400030822     D j               s              3  0 INZ(1)
007500030822     D wGiroDft        s              1  0 INZ(*zeros)
007600010330     D*------------------
007700010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
007800010330     D*------------------
007900010330     D WLBDA8          DS                  INZ
008000010330     D  G08DAT                 1      8  0
008100010330     D  G08INV                 9     16  0
008200010330     D  G08ERR                17     17
008300010330     D  G08TGI                18     22  0
008400031201     D*------------------
008500031201     D* Costanti
008600031201     D*------------------
008700031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008800031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008900080617     D*------------------
009000080617
009100080617     D*------------------
009200080617     D* LINKING A DEFINIZIONI ESTERNE
009300080617     D*------------------
009400080617     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009500090220     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009600010330
009700010330
009800010330
009900990915     C                   time                    wrkdata
010000990915     C                   time                    wrkora
010100000913     C                   reset                   rrnum
010200990921     C                   reset                   esito
010300081014     C                   reset                   wrkesito
010400010601     C*
010500010601     C                   exsr      opeini
010600010605     C                   exsr      rwvab
010700081014     C*
010800081014     C                   exsr      endela
010900010601     C*
011000010601     C                   seton                                        lr
011100010601
011200010601
011300010601
011400010601     C*--------------------------------------------------------
011500010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011600010601     C*--------------------------------------------------------
011700010601     C     PREELA        BEGSR
011800010601     C*
011900010601     C* SE OCCORRE SPEDIRE IN FILIALE
012000010601     C                   if        invfil <> *zeros and
012100010601     C                             flgGiro = '0'
012200010601     C*
012300010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012400010601     C                   eval      flgGiro = '1'
012500000724     C*
012600000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
012700000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012800010601     C     invfil        chain     azorg01l
012900000724     C                   if        %found
013000000724     C                   movel(p)  CMD(1)        depcmd
013100020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
013200000724     C*
013300000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
013400011113     C                   Z-ADD     150           LENGH            15 5
013500000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013600000724     C                   PARM                    depcmd
013700000724     C                   PARM                    LENGH
013800000724     C*
013900000724     C                   endif
014000000616     C*
014100000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
014200000616     C                   movel(p)  CMD(2)        depcmd
014300000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
014400000616     C*
014500000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
014600011113     C                   Z-ADD     150           LENGH            15 5
014700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014800000616     C                   PARM                    depcmd
014900000616     C                   PARM                    LENGH
015000000613     C*
015100090623     C                   if        not %open(titv1q7ps)
015200090623     C                   open      titv1q7ps
015300070329     C                   except    testdett
015400000613     C                   endif
015500010601     C*
015600010601     C                   endif
015700010601     C*
015800010601     C                   ENDSR
015900010601     C***
016000010601
016100010601
016200010601
016300010601
016400010601     C*--------------------------------------------------------
016500010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
016600010601     C*--------------------------------------------------------
016700010601     C     ENDELA        BEGSR
016800081014     C*
016900081014     C                   if        %open(tivin00r)
017000081014     C                   close     tivin00r
017100081014     C                   endif
017200090911     C                   if        %open(edivabwr)
017300090911     C                   close     edivabwr
017400081014     C                   endif
017500090911     C                   if        %open(edivatwr)
017600090911     C                   close     edivatwr
017700081014     C                   endif
017800090911     C                   if        %open(edivaxwr)
017900090911     C                   close     edivaxwr
018000090623     C                   endif
018100040119     C*
018200040119     C* Effettuo la chiamata al CLLE preposto
018300090911     C                   call(e)   'TITVEVTXC'
018400040510     C                   parm                    parccm
018500040119     C                   parm                    parmbr
018600040119     C                   parm      '2'           paropz
018700050201     C*
018800050201     C* Effettuo lancio TISI95 solo x chiusura
018900050201     C                   CLEAR                   TISI95DS
019000050201     C                   EVAL      I95TLA = 'C'
019100050201     C                   CALL      'TISI95R'
019200050201     C                   PARM                    TISI95DS
019300000613     C*
019400090623     C                   if        %open(titv1q7ps)
019500000616     C                   except    findett
019600090623     C                   close     titv1q7ps
019700000613     C                   endif
019800000616     C*
019900000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
020000011113     C                   Z-ADD     150           LENGH            15 5
020100010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
020200000616     C                   PARM                    CMD(3)
020300000616     C                   PARM                    LENGH
020400000616     C*
020500010601     C                   ENDSR
020600010601     C***
020700010601
020800010601
020900010601
021000000613
021100000613     C*--------------------------------------------------------
021200000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
021300000613     C*--------------------------------------------------------
021400000621     C     STPR          BEGSR
021500000613     C*
021600090623     C                   if        not %open(titv1q7p)
021700090623     C   50              open      titv1q7p
021800990915     C                   endif
021900990915     C*
022000030715     C   50              except    riepilogo
022100990915     C*
022200090623     C                   if        %open(titv1q7p)
022300090623     C   50              close     titv1q7p
022400990914     C                   endif
022500000613     C*
022600000613     C                   ENDSR
022700000613     C***
022800000613
022900010601
023000010601
023100010330     C*--------------------------------------------------------
023200090911     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
023300010330     C*--------------------------------------------------------
023400010605     C     RWVAB         BEGSR
023500010330     C*
023600010330     C                   if        not %open(tivin00r)
023700010330     C                   open      tivin00r
023800010330     C                   endif
023900090911     C                   if        not %open(edivabwr)
024000090911     C                   open      edivabwr
024100010330     C                   endif
024200040119     C*
024300090911     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR/EDIVAXWR
024400090623     C                   exsr      prevatvax
024500040119     C*
024600040119     C                   if        chkcall = '0'
024700040119     C*
024800090911     C                   if        not %open(edivatwr)
024900090911     C                   open      edivatwr
025000040119     C                   endif
025100090623     C*
025200090911     C                   if        not %open(edivaxwr)
025300090911     C                   open      edivaxwr
025400090623     C                   endif
025500010330     C*
025600040119     C                   clear                   §CTROKVB          7 0
025700040119     C                   clear                   §CTROKVT          7 0
025800090623     C                   clear                   §CTROKVX          7 0
025900040119     C                   clear                   §CTRMO            7 0
026000040119     C                   clear                   §CTRNO            7 0
026100090623     C*
026200090623     C* Inizializzazioni
026300090623     C                   exsr      inzvar
026400090623     C                   z-add     *zeros        wNewBolla         7 0
026500010330     C*
026600030822     C                   do        *HIVAL
026700010330     C*
026800030822     C                   read      tivin00r                               70
026900010618     C*
027000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
027100010618     C                   if        vinflg = '1'
027200010618     C                   eval      flgOk = '1'
027300010618     C                   endif
027400040510     C*
027500040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
027600040510     C                   z-add     *zeros        wLenVINDTA        4 0
027700040510     C                   z-add     *zeros        wFlgVINDTA        1 0
027800040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
027900040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
028000040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
028100040510     C                   z-add     1             wFlgVINDTA
028200040510     C                   leave
028300040510     C                   endif
028400040510     C                   enddo
028500090420     C*
028600090420     C                   add       1             rrnum
028700010618     C*
028800040510     C                   if        vindta > *blanks AND
028900090420     C                             wFlgVINDTA = 1
029000010330     C*
029100010601     C                   if        *in70 = *off and
029200010330     C                             (vinflg = *blanks
029300010330     C                              or vinflg = '0'
029400010330     C                              or vinflg = '2')
029500010330     C*
029600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
029700010711     C                   if        vinflg = *blanks or vinflg = '0'
029800010711     C                   clear                   vinmsg
029900010711     C                   endif
030000010601     C*
030100090421     C* Aggiungo un byte ad '*' all'inizio d ogni record
030200090421     C                   if        %subst(%trim(vindta):1:1) = CharCSV
030300090421     C                   eval      vindta  = '*' + %trim(vindta)
030400090421     C                   endif
030500010601     C*
030600010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
030700010605     C                   eval      depfil = VABLNP
030800010601     C                   exsr      repfil
030900010601     C                   if        depfil = invfil
031000021025     C                   if        vlrpoi = 999
031100030822     C                   move(P)   invfil        VABFGS
031200021025     C                   else
031300030822     C                   move(P)   vlrpoi        VABFGS
031400021025     C                   endif
031500090914     C*
031600090914     C                   move      VABFGS        currFGS           3 0
031700090914     C*
031800090914     C* Elaborazione record corrente
031900090914     C                   exsr      elabCurrRec
032000010601     C*
032100010601     C                   exsr      PREELA
032200010604     C*
032300010604     C                   if        *in31 = *off and
032400010604     C                             *in32 = *off
032500010604     C                   eval      vinflg = '1'
032600010604     C                   else
032700010604     C                   eval      recko = vindta
032800010604     C                   except    rigadett
032900010604     C                   eval      vinflg = '2'
033000010604     C                   endif
033100010604     C                   endif
033200010601     C*
033300010604     C                   endif
033400010604     C*
033500010330     C                   else
033600010330     C                   eval      vinflg = '1'
033700010330     C                   endif
033800010601     C*
033900010601     C  N70              update    tivin000
034000010330     C*
034100030822     C  N70              enddo
034200090623     C*
034300090623     C* Scarico la testata rimasta "in canna"
034400090624     C  N31              exsr      wrivaxFoot
034500090623     C  N31              exsr      wriVAB
034600040119     C*
034700040119     C                   endif                                                  (endif - chkcall)
034800010601     C*
034900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
035000010601     C                   if        cntNonEl = *zeros or
035100010601     C                             flgMulti = '0'
035200010330     C* Se non ci sono record con errori ...
035300010601     C                   if        §ctrno = 0 and
035400010604     C                             §ctrmo = 0 and
035500010601     C                             flgStato <> '2'
035600010330     C* ... restituisco esito OK.
035700010330     C                   eval      wrkesito = '0'
035800010330     C                   else
035900040119     C                   if        §ctrokvb > 0
036000010330     C                   eval      wrkesito = '1'
036100010330     C                   else
036200010615     C                   if        flgOk = '0'
036300010615     C                   eval      wrkesito = '2'
036400010615     C                   else
036500010615     C                   eval      wrkesito = '6'
036600010615     C                   endif
036700010330     C                   endif
036800010330     C                   endif
036900010601     C                   else
037000010601     C                   eval      wrkesito = '9'
037100010601     C                   endif
037200010330     C*
037300010330     C                   if        %open(tivin00r)
037400010330     C                   close     tivin00r
037500010330     C                   endif
037600090911     C                   if        %open(edivabwr)
037700090911     C                   close     edivabwr
037800010330     C                   endif
037900090911     C                   if        %open(edivatwr)
038000090911     C                   close     edivatwr
038100040512     C                   endif
038200090911     C                   if        %open(edivaxwr)
038300090911     C                   close     edivaxwr
038400090623     C                   endif
038500010601     C*
038600010601     C                   if        vlrpoi <> 999
038700010601     C                   eval      invfil = vlrpoi
038800010601     C                   endif
038900010330     C*
039000040119     C                   if        §ctrokvb > 0
039100010601     C                             and invfil > *zeros
039200010330     C                   exsr      invio
039300010330     C                   endif
039400010601     C*
039500010618     C                   if        flgGiro = '1'
039600081014     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
039700010618     C                   endif
039800010330     C*
039900010330     C                   ENDSR
040000010330     C***
040100090623
040200090623
040300090623
040400090623     C*----------------------------------------------------*
040500090623     C*  INIZIALIZZAZIONI CAMPI DI WRK
040600090623     C*----------------------------------------------------*
040700090623     C     INZWRK        BEGSR
040800090623     C*
040900090623     C                   eval      i = 1
041000090623     C                   eval      posDa = *zeros
041100090623     C                   eval      posA  = *zeros
041200090623     C*
041300090623     C                   ENDSR
041400090623
041500090623
041600090623
041700090623     C*----------------------------------------------------*
041800090623     C*  ELABORAZIONE RECORD CORRENTE
041900090623     C*----------------------------------------------------*
042000090623     C     ELABCURRREC   BEGSR
042100090623     C*
042200090623     C                   select
042300090623     C*
042400090623     C* Tipo record "TESTATA" - ENTEXP
042500090623     C                   when      %subst(vindta:1:6) = 'ENTEXP'
042600090623     C                   if        wNewBolla > *zeros
042700090624     C  N31              exsr      wrivaxFoot
042800090623     C  N31              exsr      wriVAB
042900090623     C                   exsr      inzvar
043000090623     C                   endif
043100090624     C                   eval      wNewBolla = 1
043200090623     C                   eval      wGiro = *zeros
043300090623     C                   clear                   SkSplitFLD
043400090624     C                   exsr      inzwrk
043500090623     C                   exsr      impvab
043600090623     C                   exsr      inzwrk
043700090623     C                   exsr      impvab
043800090624     C  N31              exsr      wrivaxHead
043900090623     C*
044000090623     C* Tipo record "DETTAGLIO" - LIGEXP
044100090623     C                   when      %subst(vindta:1:6) = 'LIGEXP'
044200090623     C                   eval      wGiro = *zeros
044300090623     C                   clear                   SkSplitFLD
044400090623     C                   exsr      inzwrk
044500090623     C                   exsr      impvatvax
044600090623     C                   exsr      inzwrk
044700090623     C                   exsr      impvatvax
044800090624     C*
044900090624     C  N31              exsr      wrivat
045000090624     C  N31              exsr      wrivaxBody
045100090623     C*
045200090623     C*
045300090623     C                   endsl
045400090623     C*
045500090623     C                   ENDSR
045600040119
045700040119
045800040119
045900040119     C*----------------------------------------------------*
046000040119     C*  SCARICAMENTO BUFFER RECORDS VAB
046100040119     C*----------------------------------------------------*
046200040510     C     WRIVAB        BEGSR
046300090623     C*
046400090623     C  N31              add       1             §CTROKVB
046500090623     C   32              add       1             §CTRMO
046600090623     C   31              add       1             §CTRNO
046700040119     C*
046800060331     C* Gestisco l'eventuale rottura x numero spedizione
046900070823     C                   if        VABNSP <> depspe
047000070301     C                   if        wVATNOT_E <> *blanks
047100080125     C                   if        VABCTM = *blanks
047200080125     C                   eval      VABCTM = '7Q'
047300080125     C                   endif
047400070301     C                   endif
047500090911     C*
047600090911     C* VALORIZZO CAMPI RELATIVI AL "CMR"
047700090911     C                   eval      VABCMR = wCMR
047800090911     C                   eval      VABDCM = DATCOR
047900090911     C                   eval      VABDTS = DATCOR
048000090911     C                   eval      VABHMS = ORACOR
048100090911     C                   eval      VABCNT = 1
048200090911     C****
048300090911     C                   write     edivab00                                     => scarico il VAB
048400060331     C                   endif
048500090623     C*
048600090623     C  N31              eval      depspe = vabNSP
048700040119     C*
048800040119     C                   ENDSR
048900040119
049000040119
049100040119
049200040119     C*----------------------------------------------------*
049300040119     C*  SCARICAMENTO BUFFER RECORDS VAT
049400040119     C*----------------------------------------------------*
049500040510     C     WRIVAT        BEGSR
049600060512     C*
049700060512     C* Inizializzo i campi chiave
049800090914     C                   eval      VATFGS = currFGS
049900060512     C                   eval      VATAAS = VABAAS
050000060512     C                   eval      VATCCM = VABCCM
050100070823     C                   eval      VATNRS = VABNRS
050200060512     C                   eval      VATNSP = VABNSP
050300060512     C                   eval      VATLNP = VABLNP
050400090914     C                   eval      VATCMR = wCMR
050500090914     C                   eval      VATCNT = 1
050600060331     C*
050700060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
050800060331     C                   if        wVATNOT_A <> *blanks
050900060331     C                   eval      VATNOT = wVATNOT_A
051000060331     C                   eval      VATTRC = 'A'
051100090911     C                   write     EDIVAT00
051200060331     C                   add       1             §CTROKVT
051300060331     C                   endif
051400060331     C*
051500060331     C                   if        wVATNOT_B <> *blanks
051600060331     C                   eval      VATNOT = wVATNOT_B
051700060331     C                   eval      VATTRC = 'B'
051800090911     C                   write     EDIVAT00
051900060331     C                   add       1             §CTROKVT
052000060331     C                   endif
052100090623     C*
052200090623     C* Considero il VAT solo a rottura del "chi sono"
052300090623     C                   if        wVATNOT_E <> *blanks AND
052400090623     C                             wVATNOT_E <> sVATNOT_E
052500060331     C                   eval      VATNOT = wVATNOT_E
052600060331     C                   eval      VATTRC = 'E'
052700090911     C                   write     EDIVAT00
052800080716     C                   add       1             §CTROKVT
052900090623     C                   eval      sVATNOT_E = wVATNOT_E
053000090624     C                   add       1             VABNCL
053100080625     C                   endif
053200040119     C*
053300040119     C                   ENDSR
053400090624
053500090624
053600090624
053700090624     C*----------------------------------------------------*
053800090624     C*  SCARICAMENTO BUFFER RECORDS VAX - TESTATA
053900090624     C*----------------------------------------------------*
054000090624     C     WRIVAXHEAD    BEGSR
054100090624     C*
054200090624     C* Inizializzo i campi chiave
054300090914     C                   eval      VAXFGS = currFGS
054400090624     C                   eval      VAXAAS = VABAAS
054500090624     C                   eval      VAXCCM = VABCCM
054600090624     C                   eval      VAXNRS = VABNRS
054700090624     C                   eval      VAXNSP = VABNSP
054800090624     C                   eval      VAXLNP = VABLNP
054900090914     C                   eval      VAXCMR = wCMR
055000090914     C                   eval      VAXCNT = 1
055100090703     C*
055200090703     C                   if        VAXTMD = *blanks
055300090703     C                   eval      VAXTMD = 'STD1'
055400090703     C                   endif
055500090624     C*
055600090624     C* Lascio 1 righe vuote prima della stampa della testata
055700090624     C                   eval      VAXPRR = VAXPRR + 1
055800090624     C                   eval      vaxdtt = *blanks
055900090911     C                   write     EDIVAX00
056000090624     C                   add       1             §CTROKVX
056100090624     C*
056200090624     C* Quindi compongo la testata d stampa
056300090624     C                   eval      VAXPRR = VAXPRR + 1
056400090624     C                   eval      VAXDTT = prtconst(1)
056500090703     C                   eval      VAXTRK = 'T1'
056600090624     C                   if        vaxdtt <> *blanks
056700090911     C                   write     EDIVAX00
056800090624     C                   add       1             §CTROKVX
056900090624     C                   endif
057000090624     C*
057100090624     C                   eval      VAXPRR = VAXPRR + 1
057200090624     C                   eval      VAXDTT = prtconst(2)
057300090703     C                   eval      VAXTRK = 'T1'
057400090624     C                   if        vaxdtt <> *blanks
057500090911     C                   write     EDIVAX00
057600090624     C                   add       1             §CTROKVX
057700090624     C                   endif
057800090624     C*
057900090624     C                   eval      VAXPRR = VAXPRR + 1
058000090624     C                   eval      VAXDTT = wVAX_CODCLI + '        ' +
058100090624     C                                      wVAX_NUMORD + '        ' +
058200090624     C                                      wVAX_NUMBP
058300090703     C                   eval      VAXTRK = 'T1'
058400090624     C                   if        vaxdtt <> *blanks
058500090911     C                   write     EDIVAX00
058600090624     C                   add       1             §CTROKVX
058700090624     C                   endif
058800090624     C*
058900090624     C* Lascio 2 righe vuote dopo la stampa della testata
059000090624     C                   eval      VAXPRR = VAXPRR + 1
059100090624     C                   eval      vaxdtt = *blanks
059200090911     C                   write     EDIVAX00
059300090624     C                   add       1             §CTROKVX
059400090624     C                   eval      VAXPRR = VAXPRR + 1
059500090911     C                   write     EDIVAX00
059600090624     C                   add       1             §CTROKVX
059700090624     C*
059800090624     C* Quindi compongo l'intestazione del body
059900090624     C                   eval      VAXPRR = VAXPRR + 1
060000090703     C                   eval      VAXDTT = prtconst(3)
060100090703     C                   eval      VAXTRK = 'C1'
060200090624     C                   if        vaxdtt <> *blanks
060300090911     C                   write     EDIVAX00
060400090624     C                   add       1             §CTROKVX
060500090624     C                   endif
060600090624     C*
060700090624     C                   eval      VAXPRR = VAXPRR + 1
060800090703     C                   eval      VAXDTT = prtconst(4)
060900090703     C                   eval      VAXTRK = 'C1'
061000090624     C                   if        vaxdtt <> *blanks
061100090911     C                   write     EDIVAX00
061200090624     C                   add       1             §CTROKVX
061300090624     C                   endif
061400090624     C*
061500090624     C* Lascio 1 righe vuote prima della stampa del body
061600090624     C                   eval      VAXPRR = VAXPRR + 1
061700090624     C                   eval      vaxdtt = *blanks
061800090911     C                   write     EDIVAX00
061900090624     C                   add       1             §CTROKVX
062000090624     C*
062100090624     C                   ENDSR
062200090623
062300090623
062400090623
062500090623     C*----------------------------------------------------*
062600090624     C*  SCARICAMENTO BUFFER RECORDS VAX - CORPO
062700090623     C*----------------------------------------------------*
062800090624     C     WRIVAXBODY    BEGSR
062900090623     C*
063000090623     C* Inizializzo i campi chiave
063100090914     C                   eval      VAXFGS = currFGS
063200090623     C                   eval      VAXAAS = VABAAS
063300090623     C                   eval      VAXCCM = VABCCM
063400090623     C                   eval      VAXNRS = VABNRS
063500090623     C                   eval      VAXNSP = VABNSP
063600090623     C                   eval      VAXLNP = VABLNP
063700090914     C                   eval      VAXCMR = wCMR
063800090914     C                   eval      VAXCNT = 1
063900090623     C*
064000090623     C                   eval      VAXTRK = 'C1'
064100090703     C*
064200090703     C                   if        VAXTMD = *blanks
064300090703     C                   eval      VAXTMD = 'STD1'
064400090703     C                   endif
064500090623     C*
064600090624     C* Allineo a destra il numero di scatole
064700090624     C                   evalr     wVAXDTT_3 = %trim(wVAXDTT_3)
064800090624     C*
064900090624     C                   eval      VAXPRR = VAXPRR + 1
065000090624     C                   eval      VAXDTT = '   ' + wVAXDTT_1 + '     ' +
065100090624     C                                      wVAXDTT_2 + '        ' + WVAXDTT_3
065200090623     C*
065300090623     C                   if        vaxdtt <> *blanks
065400090911     C                   write     EDIVAX00
065500090623     C                   add       1             §CTROKVX
065600090623     C                   endif
065700090623     C*
065800090623     C                   ENDSR
065900090624
066000090624
066100090624
066200090624     C*----------------------------------------------------*
066300090624     C*  SCARICAMENTO BUFFER RECORDS VAX - PIEDE
066400090624     C*----------------------------------------------------*
066500090624     C     WRIVAXFOOT    BEGSR
066600090624     C*
066700090624     C* Inizializzo i campi chiave
066800090914     C                   eval      VAXFGS = currFGS
066900090624     C                   eval      VAXAAS = VABAAS
067000090624     C                   eval      VAXCCM = VABCCM
067100090624     C                   eval      VAXNRS = VABNRS
067200090624     C                   eval      VAXNSP = VABNSP
067300090624     C                   eval      VAXLNP = VABLNP
067400090914     C                   eval      VAXCMR = wCMR
067500090914     C                   eval      VAXCNT = 1
067600090624     C*
067700090624     C                   eval      VAXTRK = 'T2'
067800090703     C*
067900090703     C                   if        VAXTMD = *blanks
068000090703     C                   eval      VAXTMD = 'STD1'
068100090703     C                   endif
068200090624     C*
068300090624     C* Lascio 1 righe vuote prima della stampa del piede
068400090624     C                   eval      VAXPRR = VAXPRR + 1
068500090624     C                   eval      vaxdtt = *blanks
068600090911     C                   write     EDIVAX00
068700090624     C                   add       1             §CTROKVX
068800090624     C*
068900090624     C* Quindi riga di separazione body/foot
069000090624     C                   eval      VAXPRR = VAXPRR + 1
069100090624     C                   eval      VAXDTT = prtconst(5)
069200090624     C                   if        vaxdtt <> *blanks
069300090911     C                   write     EDIVAX00
069400090624     C                   add       1             §CTROKVX
069500090624     C                   endif
069600090624     C*
069700090624     C* Lascio 1 righe vuote prima della stampa del piede
069800090624     C                   eval      VAXPRR = VAXPRR + 1
069900090624     C                   eval      vaxdtt = *blanks
070000090911     C                   write     EDIVAX00
070100090624     C                   add       1             §CTROKVX
070200090624     C*
070300090624     C* Quindi componfo il piede d stampa
070400090624     C                   eval      VAXPRR = VAXPRR + 1
070500090624     C                   eval      VAXDTT = 'CAUSALE: Vendita'                 +
070600090624     C                                      '               '                  +
070700090624     C                                      'CARTONI: ' + %char(vabNCL)        +
070800090624     C                                      '               '                  +
070900090624     C                                      'PESO (Kg.): ' + %char(vabPKB)
071000090624     C*
071100090624     C                   if        vaxdtt <> *blanks
071200090911     C                   write     EDIVAX00
071300090624     C                   add       1             §CTROKVX
071400090624     C                   endif
071500090624     C*
071600090624     C* Quindi riga di chiusura finale
071700090624     C                   eval      VAXPRR = VAXPRR + 1
071800090624     C                   eval      VAXDTT = prtconst(5)
071900090624     C                   if        vaxdtt <> *blanks
072000090911     C                   write     EDIVAX00
072100090624     C                   add       1             §CTROKVX
072200090624     C                   endif
072300090624     C*
072400090624     C                   ENDSR
072500040119
072600040119
072700040119
072800040119     C*----------------------------------------------------*
072900090911     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR/EDIVAXWR
073000040119     C*----------------------------------------------------*
073100090623     C     PREVATVAX     BEGSR
073200040119     C*
073300090911     C* Compongo il nome del membro da dare al EDIVATWR/EDIVAXWR
073400040119     C                   eval      parmbr = vlrhdl
073500040119     C                   movel     'M'           parmbr
073600040510     C                   eval      parccm = vlrksc
073700040119     C                   eval      paropz = '1'
073800040119     C* Effettuo la chiamata al CLLE preposto
073900090911     C                   call(e)   'TITVEVTXC'
074000040119     C                   parm                    parccm
074100040119     C                   parm                    parmbr
074200040119     C                   parm                    paropz
074300040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
074400040119     C                   if        %error
074500040119     C                   movel     '1'           chkcall
074600040119     C                   else
074700040119     C                   movel     '0'           chkcall
074800040119     C                   endif
074900040119     C*
075000040119     C                   ENDSR
075100010601
075200010601
075300010601
075400010330     C*----------------------------------------------------*
075500030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
075600010330     C*----------------------------------------------------*
075700040119     C     INZVAR        BEGSR
075800010330     C*
075900030822     C* Inizializzo variabili di wrk
076000090916     C                   Z-ADD     *zeros        Num5_0            5 0
076100090916     C                   MOVEL     *blanks       wNOTE            70
076200060331     C                   MOVEL     *blanks       wVATNOT_A
076300060331     C                   MOVEL     *blanks       wVATNOT_B
076400090904     C                   MOVEL     *blanks       wVATNOT_E
076500090904     C                   MOVEL     *blanks       wVATNOT_E_1
076600090904     C                   MOVEL     *blanks       wVATNOT_E_2
076700090623     C*
076800090624     C                   MOVEL     *blanks       sVATNOT_E        35
076900090623     C                   MOVEL     *blanks       wVAXDTT_1        10
077000090623     C                   MOVEL     *blanks       wVAXDTT_2        35
077100090623     C                   MOVEL     *blanks       wVAXDTT_3        10
077200090624     C                   MOVEL     *blanks       wVAX_NUMBP       10
077300090624     C                   MOVEL     *blanks       wVAX_NUMORD      10
077400090624     C                   MOVEL     *blanks       wVAX_CODCLI      15
077500090220     C*
077600080617     C                   EVAL      wFlgCAS = *blanks
077700030822     C*
077800030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
077900090911     C                   CLEAR                   EDIVAB00
078000090911     C                   CLEAR                   EDIVAT00
078100090911     C                   CLEAR                   EDIVAX00
078200030822     C                   CLEAR                   SkSplitCSV
078300090623     C                   EXSR      inzwrk
078400030822     C*
078500030822     C* Reimposto i valori di default
078600030822     C                   EXSR      DEFCAM
078700030822     C*
078800010330     C                   ENDSR
078900010330     C*----------------------------------------------------*
079000030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
079100010330     C*----------------------------------------------------*
079200010330     C     DEFCAM        BEGSR
079300030822     C*
079400030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
079500030715     C* e delimitatore testo.
079600030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
079700030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
079800030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
079900030822     C*
080000030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
080100030715     C                   IF        %subst(vlrppt:1:1) = 'S'
080200030715     C                   SETON                                        50
080300030715     C                   ELSE
080400030715     C                   SETOFF                                       50
080500030715     C                   ENDIF
080600090421     C*
080700090421     C* D default SEMPRE porto franco / SEMPRE lnp = p.o. invio / SEMPRE nsp = rrnum
080800090421     C                   EVAL      VABCBO = '1'
080900090421     C                   EVAL      VABLNP = vlrpoi
081000090421     C                   EVAL      VABNSP = rrnum
081100030822     C*
081200030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
081300030822     C                   EVAL      posDaDft = 1
081400030822     C                   EVAL      posADft  = 0
081500030822     C                   EVAL      wGiroDft = 0
081600030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
081700030822     C                             posDaDft > 0
081800030822     C*
081900030822     C* Gestisco il 1° giro
082000030822     C                   IF        wGiroDft = 0
082100030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
082200030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
082300030822     C* Incremento il contatore dei "giri"
082400030822     C                   EVAL      wGiroDft = 1
082500030822     C                   ELSE
082600030822     C                   EVAL      posDaDft = posADft
082700030822     C                   ENDIF
082800030822     C* Eseguo lo scan x trovare la fine del campo corrente
082900030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
083000030822     C*
083100030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
083200030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
083300030822     C                   IF        posDaDft > 0 AND
083400030822     C                             posADft  > 0
083500030822     C* NCL
083600030822     C                   IF        %subst(
083700030822     C                             %subst(vlrppt:posDaDft+1:
083800030822     C                             posADft-posDaDft-1):1:3)
083900030822     C                             = 'NCL'
084000030822     C                   EVAL      PiStr=%trim(%subst(
084100030822     C                             %subst(vlrppt:posDaDft+1:
084200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084300030822     C                   EXSR      CHKNUM
084400030822     C                   IF        PiInt=*on
084500030822     C                   Z-ADD     PiVal         VABNCL
084600030822     C                   ENDIF
084700030822     C                   ENDIF
084800030822     C* CCM
084900030822     C                   IF        %subst(
085000030822     C                             %subst(vlrppt:posDaDft+1:
085100030822     C                             posADft-posDaDft-1):1:3)
085200030822     C                             = 'CCM'
085300030822     C                   EVAL      PiStr=%trim(%subst(
085400030822     C                             %subst(vlrppt:posDaDft+1:
085500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085600030822     C                   EXSR      CHKNUM
085700030822     C                   IF        PiInt=*on
085800030822     C                   Z-ADD     PiVal         VABCCM
085900030822     C                   ENDIF
086000030822     C                   ENDIF
086100030822     C* LNP
086200030822     C                   IF        %subst(
086300030822     C                             %subst(vlrppt:posDaDft+1:
086400030822     C                             posADft-posDaDft-1):1:3)
086500030822     C                             = 'LNP'
086600030822     C                   EVAL      PiStr=%trim(%subst(
086700030822     C                             %subst(vlrppt:posDaDft+1:
086800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086900030822     C                   EXSR      CHKNUM
087000030822     C                   IF        PiInt=*on
087100030822     C                   Z-ADD     PiVal         VABLNP
087200030822     C                   ENDIF
087300030822     C                   ENDIF
087400030822     C* NRS
087500030822     C                   IF        %subst(
087600030822     C                             %subst(vlrppt:posDaDft+1:
087700030822     C                             posADft-posDaDft-1):1:3)
087800030822     C                             = 'NRS'
087900030822     C                   EVAL      PiStr=%trim(%subst(
088000030822     C                             %subst(vlrppt:posDaDft+1:
088100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
088200030822     C                   EXSR      CHKNUM
088300030822     C                   IF        PiInt=*on
088400030822     C                   Z-ADD     PiVal         VABNRS
088500030822     C                   ENDIF
088600030822     C                   ENDIF
088700030822     C* CTR
088800030822     C                   IF        %subst(
088900030822     C                             %subst(vlrppt:posDaDft+1:
089000030822     C                             posADft-posDaDft-1):1:3)
089100030822     C                             = 'CTR'
089200030822     C                   EVAL      PiStr=%trim(%subst(
089300030822     C                             %subst(vlrppt:posDaDft+1:
089400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
089500030822     C                   EXSR      CHKNUM
089600030822     C                   IF        PiInt=*on
089700030822     C                   Z-ADD     PiVal         VABCTR
089800030822     C                   ENDIF
089900030822     C                   ENDIF
090000030822     C* PKB
090100030822     C                   IF        %subst(
090200030822     C                             %subst(vlrppt:posDaDft+1:
090300030822     C                             posADft-posDaDft-1):1:3)
090400030822     C                             = 'PKB'
090500030822     C                   EVAL      PiStr=%trim(%subst(
090600030822     C                             %subst(vlrppt:posDaDft+1:
090700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
090800030822     C                   EXSR      CHKNUM
090900030822     C                   IF        PiNum=*on
091000030822     C                   Z-ADD     PiVal         VABPKB
091100030822     C                   ENDIF
091200030822     C                   ENDIF
091300030822     C* VLB
091400030822     C                   IF        %subst(
091500030822     C                             %subst(vlrppt:posDaDft+1:
091600030822     C                             posADft-posDaDft-1):1:3)
091700030822     C                             = 'VLB'
091800030822     C                   EVAL      PiStr=%trim(%subst(
091900030822     C                             %subst(vlrppt:posDaDft+1:
092000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
092100030822     C                   EXSR      CHKNUM
092200030822     C                   IF        PiNum=*on
092300030822     C                   Z-ADD     PiVal         VABVLB
092400030822     C                   ENDIF
092500030822     C                   ENDIF
092600030822     C* QFT
092700030822     C                   IF        %subst(
092800030822     C                             %subst(vlrppt:posDaDft+1:
092900030822     C                             posADft-posDaDft-1):1:3)
093000030822     C                             = 'QFT'
093100030822     C                   EVAL      PiStr=%trim(%subst(
093200030822     C                             %subst(vlrppt:posDaDft+1:
093300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
093400030822     C                   EXSR      CHKNUM
093500030822     C                   IF        PiNum=*on
093600030822     C                   Z-ADD     PiVal         VABQFT
093700030822     C                   ENDIF
093800030822     C                   ENDIF
093900030822     C* CBO
094000030822     C                   IF        %subst(
094100030822     C                             %subst(vlrppt:posDaDft+1:
094200030822     C                             posADft-posDaDft-1):1:3)
094300030822     C                             = 'CBO'
094400030822     C                   EVAL      VABCBO=%trim(%subst(
094500030822     C                             %subst(vlrppt:posDaDft+1:
094600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
094700030822     C                   ENDIF
094800030822     C* TSP
094900030822     C                   IF        %subst(
095000030822     C                             %subst(vlrppt:posDaDft+1:
095100030822     C                             posADft-posDaDft-1):1:3)
095200030822     C                             = 'TSP'
095300030822     C                   EVAL      VABTSP=%trim(%subst(
095400030822     C                             %subst(vlrppt:posDaDft+1:
095500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
095600030822     C                   ENDIF
095700030822     C* VAS
095800030822     C                   IF        %subst(
095900030822     C                             %subst(vlrppt:posDaDft+1:
096000030822     C                             posADft-posDaDft-1):1:3)
096100030822     C                             = 'VAS'
096200030822     C                   EVAL      VABVAS=%trim(%subst(
096300030822     C                             %subst(vlrppt:posDaDft+1:
096400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
096500030822     C                   ENDIF
096600030822     C* VCA
096700030822     C                   IF        %subst(
096800030822     C                             %subst(vlrppt:posDaDft+1:
096900030822     C                             posADft-posDaDft-1):1:3)
097000030822     C                             = 'VCA'
097100030822     C                   EVAL      VABVCA=%trim(%subst(
097200030822     C                             %subst(vlrppt:posDaDft+1:
097300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
097400030822     C                   ENDIF
097500070925     C* TIC
097600070925     C                   IF        %subst(
097700070925     C                             %subst(vlrppt:posDaDft+1:
097800070925     C                             posADft-posDaDft-1):1:3)
097900070925     C                             = 'TIC'
098000070925     C                   EVAL      VABTIC=%trim(%subst(
098100070925     C                             %subst(vlrppt:posDaDft+1:
098200070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
098300070925     C                   ENDIF
098400030822     C* GCA
098500030822     C                   IF        %subst(
098600030822     C                             %subst(vlrppt:posDaDft+1:
098700030822     C                             posADft-posDaDft-1):1:3)
098800030822     C                             = 'GCA'
098900030822     C                   EVAL      VABGCA=%trim(%subst(
099000030822     C                             %subst(vlrppt:posDaDft+1:
099100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
099200030822     C                   ENDIF
099300030822     C* CTM
099400030822     C                   IF        %subst(
099500030822     C                             %subst(vlrppt:posDaDft+1:
099600030822     C                             posADft-posDaDft-1):1:3)
099700030822     C                             = 'CTM'
099800030822     C                   EVAL      VABCTM=%trim(%subst(
099900030822     C                             %subst(vlrppt:posDaDft+1:
100000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
100100030822     C                   ENDIF
100200030822     C* FFD
100300030822     C                   IF        %subst(
100400030822     C                             %subst(vlrppt:posDaDft+1:
100500030822     C                             posADft-posDaDft-1):1:3)
100600030822     C                             = 'FFD'
100700030822     C                   EVAL      VABFFD=%trim(%subst(
100800030822     C                             %subst(vlrppt:posDaDft+1:
100900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
101000030822     C                   ENDIF
101100030822     C* VAD
101200030822     C                   IF        %subst(
101300030822     C                             %subst(vlrppt:posDaDft+1:
101400030822     C                             posADft-posDaDft-1):1:3)
101500030822     C                             = 'VAD'
101600030822     C                   EVAL      VABVAD=%trim(%subst(
101700030822     C                             %subst(vlrppt:posDaDft+1:
101800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
101900030822     C                   ENDIF
102000030822     C* GMA
102100030822     C                   IF        %subst(
102200030822     C                             %subst(vlrppt:posDaDft+1:
102300030822     C                             posADft-posDaDft-1):1:3)
102400030822     C                             = 'GMA'
102500030822     C                   EVAL      VABGMA=%trim(%subst(
102600030822     C                             %subst(vlrppt:posDaDft+1:
102700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
102800030822     C                   ENDIF
102900030822     C* GGA
103000030822     C                   IF        %subst(
103100030822     C                             %subst(vlrppt:posDaDft+1:
103200030822     C                             posADft-posDaDft-1):1:3)
103300030822     C                             = 'GGA'
103400030822     C                   EVAL      VABGGA=%trim(%subst(
103500030822     C                             %subst(vlrppt:posDaDft+1:
103600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
103700030822     C                   ENDIF
103800030822     C* GVA
103900030822     C                   IF        %subst(
104000030822     C                             %subst(vlrppt:posDaDft+1:
104100030822     C                             posADft-posDaDft-1):1:3)
104200030822     C                             = 'GVA'
104300030822     C                   EVAL      VABGVA=%trim(%subst(
104400030822     C                             %subst(vlrppt:posDaDft+1:
104500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
104600030822     C                   ENDIF
104700030822     C* TC1
104800030822     C                   IF        %subst(
104900030822     C                             %subst(vlrppt:posDaDft+1:
105000030822     C                             posADft-posDaDft-1):1:3)
105100030822     C                             = 'TC1'
105200030822     C                   EVAL      VABTC1=%trim(%subst(
105300030822     C                             %subst(vlrppt:posDaDft+1:
105400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
105500030822     C                   ENDIF
105600030822     C* TC2
105700030822     C                   IF        %subst(
105800030822     C                             %subst(vlrppt:posDaDft+1:
105900030822     C                             posADft-posDaDft-1):1:3)
106000030822     C                             = 'TC2'
106100030822     C                   EVAL      VABTC2=%trim(%subst(
106200030822     C                             %subst(vlrppt:posDaDft+1:
106300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
106400030822     C                   ENDIF
106500040714     C* VATTRC
106600040714     C                   IF        %subst(
106700040714     C                             %subst(vlrppt:posDaDft+1:
106800040714     C                             posADft-posDaDft-1):1:3)
106900040714     C                             = 'TRC'
107000040714     C                   EVAL      VATTRC=%trim(%subst(
107100040714     C                             %subst(vlrppt:posDaDft+1:
107200040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
107300040714     C                   ENDIF
107400090623     C* VAXTMD
107500090623     C                   IF        %subst(
107600090623     C                             %subst(vlrppt:posDaDft+1:
107700090623     C                             posADft-posDaDft-1):1:3)
107800090623     C                             = 'TMD'
107900090623     C                   EVAL      VAXTMD=%trim(%subst(
108000090623     C                             %subst(vlrppt:posDaDft+1:
108100090623     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
108200090623     C                   ENDIF
108300030822     C* ...
108400030822     C                   ENDIF
108500030822     C                   ENDDO
108600020204     C*
108700010330     C                   ENDSR
108800010607     C*----------------------------------------------------*
108900090911     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
109000010607     C*----------------------------------------------------*
109100090623     C     IMPVAB        BEGSR
109200010607     C*
109300010607     C                   SETOFF                                       3132
109400030515     C*
109500010607     C                   MOVEL     datcor        VABAAS
109600010607     C                   MOVE      datcor        VABMGS
109700010607     C*
109800030715     C**********
109900030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
110000030715     C**********
110100030715     C                   IF        wGiro = *zeros
110200080716     C*
110300080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
110400080716     C                   EVAL      depvinDTA = vinDTA
110500040113     C*
110600040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
110700090623     C                   IF        %trim(entexp(1))+%trim(entexp(2))+
110800090623     C                             %trim(entexp(3))+%trim(entexp(4))+
110900090623     C                             %trim(entexp(5))<>*blanks
111000090623     C                   EVAL      vindta = %trim(entexp(1))+%trim(entexp(2))+
111100090623     C                                      %trim(entexp(3))+%trim(entexp(4))+
111200090623     C                                      %trim(entexp(5))
111300070823     C     ';':CharCSV   XLATE     vindta        vindta
111400040510     C                   ENDIF
111500080626     C*
111600031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
111700031201     C     minu:maiu     XLATE     vindta        vindta
111800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
111900030715     C                   DOW       posDa <= %len(%trim(vindta))
112000030715     C*
112100030715     C* Gestisco il 1° campo
112200030715     C                   IF        i = 1
112300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
112400030715     C                   EVAL      posDa = 1
112500030715     C* Eseguo lo scan x trovare la fine del primo campo
112600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
112700030715     C* A questo punto "estraggo" il campo corrente
112800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
112900030715     C                                                    (posA-posDa))
113000030715     C* X i campi successivi al 1°
113100030715     C                   ELSE
113200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
113300030715     C                   EVAL      posDa = posA + 1
113400030715     C* Eseguo lo scan x trovare la fine del campo corrente
113500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
113600030715     C* Gestisco l'ultimo campo
113700030715     C                   IF        posA = *zeros
113800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
113900030715     C* A questo punto "estraggo" il campo corrente
114000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
114100030715     C                                                    (posA-posDa))
114200030715     C                   LEAVE
114300030715     C                   ELSE
114400030715     C* A questo punto "estraggo" il campo corrente
114500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
114600030715     C                                                    (posA-posDa))
114700030715     C                   ENDIF
114800030715     C                   ENDIF
114900030715     C* Incremento il contatore d campo
115000030715     C                   EVAL      i = i +1
115100030715     C                   ENDDO
115200030715     C*
115300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
115400030715     C* delimitazione testo
115500030715     C                   EVAL      i = 1
115600030715     C                   DOW       i < %elem(SkSplitFLD)
115700030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
115800030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
115900030715     C                   EVAL      i = i + 1
116000030715     C                   ENDDO
116100030715     C                   EVAL      wGiro = 1
116200080716     C* Ripristino la 1° riga originale
116300080716     C                   EVAL      vinDTA = depvinDTA
116400030715     C                   ELSE
116500030715     C                   EVAL      wGiro = 2
116600031204     C**********
116700031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
116800031204     C* d separatore campo
116900031204     C**********
117000031204     C                   EVAL      vindta = %trim(vindta)
117100031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
117200031204     C                   EVAL      vindta = %subst(vindta:2)
117300031204     C                   ENDDO
117400031204     C*
117500031204     C                   Z-ADD     *zeros        lunghInput        4 0
117600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
117700031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
117800031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
117900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
118000031204     C                   ENDDO
118100030715     C**********
118200030715     C* Effettuo lo split del campo dati d input
118300030715     C**********
118400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
118500030715     C                   DOW       posDa <= %len(%trim(vindta))
118600030715     C*
118700030715     C* Gestisco il 1° campo
118800030715     C                   IF        i = 1
118900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
119000030715     C                   EVAL      posDa = 1
119100030715     C* Eseguo lo scan x trovare la fine del primo campo
119200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
119300030715     C* A questo punto "estraggo" il campo corrente
119400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
119500030715     C                                                    (posA-posDa))
119600030715     C* X i campi successivi al 1°
119700030715     C                   ELSE
119800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
119900030715     C                   EVAL      posDa = posA + 1
120000031204     C* Verifico che nn vi sia il campo nn valorizzato
120100031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
120200031204     C* Se campo nn valorizzato skippo al prossimo
120300031204     C                   EVAL      SkSplitCSV(i) = *blanks
120400031204     C                   EVAL      posA  = posA + 1
120500031204     C                   ELSE
120600030715     C* Eseguo lo scan x trovare la fine del campo corrente
120700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
120800030715     C* Gestisco l'ultimo campo
120900030715     C                   IF        posA = *zeros
121000030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
121100030715     C* A questo punto "estraggo" il campo corrente
121200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
121300030715     C                                                    (posA-posDa))
121400030715     C                   LEAVE
121500030715     C                   ELSE
121600030715     C* A questo punto "estraggo" il campo corrente
121700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
121800030715     C                                                    (posA-posDa))
121900030715     C                   ENDIF
122000031204     C                   ENDIF
122100030715     C                   ENDIF
122200030715     C* Incremento il contatore d campo
122300030715     C                   EVAL      i = i +1
122400030715     C                   ENDDO
122500030715     C*
122600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
122700030715     C* delimitazione testo
122800030715     C                   EVAL      i = 1
122900030715     C                   DOW       i < %elem(SkSplitCSV)
123000030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
123100030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
123200030715     C                   EVAL      i = i + 1
123300030715     C                   ENDDO
123400030715     C*
123500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
123600030715     C                   EVAL      i = 1
123700030715     C                   DOW       i < %elem(SkSplitFLD)
123800030715     C*
123900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
124000030715     C                   IF        SkSplitFLD(i) <> *blanks AND
124100090623     C                             SkSplitCSV(i) <> *blanks
124200090623     C*
124300090623     C                   exsr      CARVAL
124400040510     C*
124500030715     C                   ENDIF
124600030715     C                   EVAL      i = i + 1
124700030715     C                   ENDDO
124800070925     C*
124900071029     C***  ===>  Gestione campi "particolari" fuori ciclo
125000080125     C*
125100090623     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
125200090623     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
125300091007     C*
125400091007     C                   IF        wNOTE <> *blanks
125500091007     C                   EVAL      VABNMO = *all'*'
125600091007     C                   ENDIF
125700070925     C*
125800071029     C***  <===  -----------------------------------------
125900070925     C*
126000070925     C* Considerazioni finali su CBO/CAS
126100080125     C                   IF        wFlgCAS = '1'
126200070925     C                   IF        VABCBO = '1'
126300070925     C                   EVAL      VABCBO = '4'
126400070925     C                   ENDIF
126500070925     C                   IF        VABCBO = '2'
126600070925     C                   EVAL      VABCBO = '6'
126700070925     C                   ENDIF
126800070925     C                   ENDIF
126900070925     C***  <===  ----------------------------
127000040510     C*
127100040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
127200040510     C                   IF        VABNSP = *zeros
127300040510     C                   SETON                                        31
127400040510     C                   ENDIF
127500040715     C*
127600040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
127700040715     C                   IF        VABLNP = *zeros
127800040715     C                   SETON                                        31
127900040715     C                   ENDIF
128000040510     C*
128100040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
128200040510     C                   IF        VABRMN = *zeros
128300040510     C                   Z-ADD     VABNSP        VABRMN
128400040510     C                   ENDIF
128500040510     C*
128600040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
128700040510     C                   IF        VABRMA = *blanks
128800040714     C                   MOVEL     VABRMN        VABRMA
128900040510     C                   ENDIF
129000040128     C*
129100040510     C* Se provincia nn valorizzata la reperisco
129200040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
129300040510     C                   IF        VABPRD = *blanks
129400040128     C                   CLEAR                   TISI95DS
129500040128     C                   EVAL      I95TCN = '3'
129600040128     C                   Z-ADD     datcor        I95DAT
129700040128     C                   EVAL      I95NAR = VABNZD
129800040128     C                   EVAL      I95CAP = VABCAD
129900040128     C                   EVAL      I95LOC = VABLOD
130000040128     C                   CALL      'TISI95R'
130100040128     C                   PARM                    TISI95DS
130200040128     C                   EVAL      VABPRD = O95PRV
130300040510     C                   ENDIF
130400020204     C*
130500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
130600020204     C                   EXSR      CHKIMPDIV
130700030715     C                   ENDIF
130800020204     C*
130900010607     C                   ENDSR
131000010607     C*----------------------------------------------------*
131100090623
131200090623
131300090623
131400090623     C*----------------------------------------------------*
131500090911     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT/EDIVAX)
131600090623     C*----------------------------------------------------*
131700090623     C     IMPVATVAX     BEGSR
131800090623     C*
131900090623     C**********
132000090623     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
132100090623     C**********
132200090623     C                   IF        wGiro = *zeros
132300090623     C*
132400090623     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
132500090623     C                   EVAL      depvinDTA = vinDTA
132600090623     C*
132700090623     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
132800090623     C                   IF        %trim(ligexp(1))+%trim(ligexp(2))+
132900090623     C                             %trim(ligexp(3))+%trim(ligexp(4))+
133000090623     C                             %trim(ligexp(5))<>*blanks
133100090623     C                   EVAL      vindta = %trim(ligexp(1))+%trim(ligexp(2))+
133200090623     C                                      %trim(ligexp(3))+%trim(ligexp(4))+
133300090623     C                                      %trim(ligexp(5))
133400090623     C     ';':CharCSV   XLATE     vindta        vindta
133500090623     C                   ENDIF
133600090623     C*
133700090623     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
133800090623     C     minu:maiu     XLATE     vindta        vindta
133900090623     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
134000090623     C                   DOW       posDa <= %len(%trim(vindta))
134100090623     C*
134200090623     C* Gestisco il 1° campo
134300090623     C                   IF        i = 1
134400090623     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
134500090623     C                   EVAL      posDa = 1
134600090623     C* Eseguo lo scan x trovare la fine del primo campo
134700090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
134800090623     C* A questo punto "estraggo" il campo corrente
134900090623     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
135000090623     C                                                    (posA-posDa))
135100090623     C* X i campi successivi al 1°
135200090623     C                   ELSE
135300090623     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
135400090623     C                   EVAL      posDa = posA + 1
135500090623     C* Eseguo lo scan x trovare la fine del campo corrente
135600090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
135700090623     C* Gestisco l'ultimo campo
135800090623     C                   IF        posA = *zeros
135900090623     C                   EVAL      posA = %len(%trim(vindta)) + 1
136000090623     C* A questo punto "estraggo" il campo corrente
136100090623     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
136200090623     C                                                    (posA-posDa))
136300090623     C                   LEAVE
136400090623     C                   ELSE
136500090623     C* A questo punto "estraggo" il campo corrente
136600090623     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
136700090623     C                                                    (posA-posDa))
136800090623     C                   ENDIF
136900090623     C                   ENDIF
137000090623     C* Incremento il contatore d campo
137100090623     C                   EVAL      i = i +1
137200090623     C                   ENDDO
137300090623     C*
137400090623     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
137500090623     C* delimitazione testo
137600090623     C                   EVAL      i = 1
137700090623     C                   DOW       i < %elem(SkSplitFLD)
137800090623     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
137900090623     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
138000090623     C                   EVAL      i = i + 1
138100090623     C                   ENDDO
138200090623     C                   EVAL      wGiro = 1
138300090623     C* Ripristino la 1° riga originale
138400090623     C                   EVAL      vinDTA = depvinDTA
138500090623     C                   ELSE
138600090623     C                   EVAL      wGiro = 2
138700090623     C**********
138800090623     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
138900090623     C* d separatore campo
139000090623     C**********
139100090623     C                   EVAL      vindta = %trim(vindta)
139200090623     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
139300090623     C                   EVAL      vindta = %subst(vindta:2)
139400090623     C                   ENDDO
139500090623     C*
139600090623     C                   Z-ADD     *zeros        lunghInput        4 0
139700090623     C                   EVAL      lunghInput = %len(%trim(vindta))
139800090623     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
139900090623     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
140000090623     C                   EVAL      lunghInput = %len(%trim(vindta))
140100090623     C                   ENDDO
140200090623     C**********
140300090623     C* Effettuo lo split del campo dati d input
140400090623     C**********
140500090623     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
140600090623     C                   DOW       posDa <= %len(%trim(vindta))
140700090623     C*
140800090623     C* Gestisco il 1° campo
140900090623     C                   IF        i = 1
141000090623     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
141100090623     C                   EVAL      posDa = 1
141200090623     C* Eseguo lo scan x trovare la fine del primo campo
141300090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
141400090623     C* A questo punto "estraggo" il campo corrente
141500090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
141600090623     C                                                    (posA-posDa))
141700090623     C* X i campi successivi al 1°
141800090623     C                   ELSE
141900090623     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
142000090623     C                   EVAL      posDa = posA + 1
142100090623     C* Verifico che nn vi sia il campo nn valorizzato
142200090623     C                   IF        %subst(vindta:posDa:1) = CharCSV
142300090623     C* Se campo nn valorizzato skippo al prossimo
142400090623     C                   EVAL      SkSplitCSV(i) = *blanks
142500090623     C                   EVAL      posA  = posA + 1
142600090623     C                   ELSE
142700090623     C* Eseguo lo scan x trovare la fine del campo corrente
142800090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
142900090623     C* Gestisco l'ultimo campo
143000090623     C                   IF        posA = *zeros
143100090623     C                   EVAL      posA = %len(%trim(vindta)) + 1
143200090623     C* A questo punto "estraggo" il campo corrente
143300090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
143400090623     C                                                    (posA-posDa))
143500090623     C                   LEAVE
143600090623     C                   ELSE
143700090623     C* A questo punto "estraggo" il campo corrente
143800090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
143900090623     C                                                    (posA-posDa))
144000090623     C                   ENDIF
144100090623     C                   ENDIF
144200090623     C                   ENDIF
144300090623     C* Incremento il contatore d campo
144400090623     C                   EVAL      i = i +1
144500090623     C                   ENDDO
144600090623     C*
144700090623     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
144800090623     C* delimitazione testo
144900090623     C                   EVAL      i = 1
145000090623     C                   DOW       i < %elem(SkSplitCSV)
145100090623     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
145200090623     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
145300090623     C                   EVAL      i = i + 1
145400090623     C                   ENDDO
145500090623     C*
145600090623     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
145700090623     C                   EVAL      i = 1
145800090623     C                   DOW       i < %elem(SkSplitFLD)
145900090623     C*
146000090623     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
146100090623     C                   IF        SkSplitFLD(i) <> *blanks AND
146200090623     C                             SkSplitCSV(i) <> *blanks
146300090623     C*
146400090623     C                   exsr      CARVAL
146500090623     C*
146600090623     C                   ENDIF
146700090623     C                   EVAL      i = i + 1
146800090623     C                   ENDDO
146900090623     C*
147000090623     C***  ===>  Gestione campi "particolari" fuori ciclo
147100090623     C*
147200090623     C*
147300090623     C***  <===  -----------------------------------------
147400090623     C*
147500090623     C                   ENDIF
147600090623     C*
147700090623     C                   ENDSR
147800090623     C*----------------------------------------------------*
147900090623
148000090623
148100090623     C*----------------------------------------------------*
148200090623     C*  ROUTINE DI VALORIZZAZIONE CAMPI
148300090623     C*----------------------------------------------------*
148400090623     C     CARVAL        BEGSR
148500090623     C*
148600090623     C*** ATB
148700090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
148800090623     C                   EVAL      VABATB = SkSplitCSV(i)
148900090623     C                   ENDIF
149000090623     C*** RSD
149100090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
149200090623     C                   EVAL      VABRSD = SkSplitCSV(i)
149300090623     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
149400090623     C     '@':'A'       XLATE     VABRSD        VABRSD
149500090623     C* ==
149600090623     C                   ENDIF
149700090623     C*** RD2
149800090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
149900090623     C                   EVAL      VABRD2 = SkSplitCSV(i)
150000090623     C                   ENDIF
150100090623     C*** IND
150200090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
150300090623     C                   EVAL      VABIND = SkSplitCSV(i)
150400090623     C                   ENDIF
150500090623     C*** LOD
150600090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
150700090623     C                   EVAL      VABLOD = SkSplitCSV(i)
150800090623     C                   ENDIF
150900090623     C*** PRD
151000090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
151100090623     C                   EVAL      VABPRD = SkSplitCSV(i)
151200090623     C                   ENDIF
151300090623     C*** NZD
151400090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
151500090623     C                   EVAL      VABNZD = SkSplitCSV(i)
151600090623     C                   ENDIF
151700090623     C*** RMA
151800090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
151900090623     C                   EVAL      VABRMA = SkSplitCSV(i)
152000090623     C                   ENDIF
152100090623     C*** NOT
152200090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
152300090623     C                   EVAL      VABNOT = SkSplitCSV(i)
152400090623     C                   ENDIF
152500090623     C*** NT2
152600090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
152700090623     C                   EVAL      VABNT2 = SkSplitCSV(i)
152800090623     C                   ENDIF
152900090623     C*** VCA
153000090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
153100090623     C                   EVAL      VABVCA = SkSplitCSV(i)
153200090623     C                   ENDIF
153300090623     C*** VAS
153400090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
153500090623     C                   EVAL      VABVAS = SkSplitCSV(i)
153600090623     C                   ENDIF
153700090623     C*** FFD
153800090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
153900090623     C                   EVAL      VABFFD = SkSplitCSV(i)
154000090623     C                   ENDIF
154100090623     C*** GC1
154200090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
154300090623     C                   EVAL      VABGC1 = SkSplitCSV(i)
154400090623     C                   ENDIF
154500090623     C*** GC2
154600090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
154700090623     C                   EVAL      VABGC2 = SkSplitCSV(i)
154800090623     C                   ENDIF
154900090623     C*** TSP
155000090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
155100090623     C                   EVAL      VABTSP = SkSplitCSV(i)
155200090623     C                   ENDIF
155300090623     C*** CBO
155400090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
155500090623     C                   EVAL      VABCBO = SkSplitCSV(i)
155600090623     C                   ENDIF
155700090623     C*** NAS
155800090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
155900090623     C                   EVAL      VABNAS = SkSplitCSV(i)
156000090623     C                   ENDIF
156100090623     C*** TIC
156200090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
156300090623     C                   EVAL      VABTIC = SkSplitCSV(i)
156400090623     C                   ENDIF
156500090623     C*** GCA
156600090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
156700090623     C                   EVAL      VABGCA = SkSplitCSV(i)
156800090623     C                   ENDIF
156900090623     C*** XCO
157000090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
157100090623     C                   EVAL      VABXCO = SkSplitCSV(i)
157200090623     C                   ENDIF
157300090623     C*** CTM
157400090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
157500090623     C                   EVAL      VABCTM = SkSplitCSV(i)
157600090623     C                   ENDIF
157700090623     C*** TCR
157800090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
157900090623     C                   EVAL      VABTCR = SkSplitCSV(i)
158000090623     C                   ENDIF
158100090623     C*** CTS
158200090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
158300090623     C                   EVAL      VABCTS = SkSplitCSV(i)
158400090623     C                   ENDIF
158500090623     C*** FTM
158600090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
158700090623     C                   EVAL      VABFTM = SkSplitCSV(i)
158800090623     C                   ENDIF
158900090623     C*** VAD
159000090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
159100090623     C                   EVAL      VABVAD = SkSplitCSV(i)
159200090623     C                   ENDIF
159300090623     C*** GMA
159400090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
159500090623     C                   EVAL      VABGMA = SkSplitCSV(i)
159600090623     C                   ENDIF
159700090623     C*** GGA
159800090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
159900090623     C                   EVAL      VABGGA = SkSplitCSV(i)
160000090623     C                   ENDIF
160100090623     C*** GVA
160200090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
160300090623     C                   EVAL      VABGVA = SkSplitCSV(i)
160400090623     C                   ENDIF
160500090623     C*** TC1
160600090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
160700090623     C                   EVAL      VABTC1 = SkSplitCSV(i)
160800090623     C                   ENDIF
160900090623     C*** TC2
161000090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
161100090623     C                   EVAL      VABTC2 = SkSplitCSV(i)
161200090623     C                   ENDIF
161300090623     C*** SCL
161400090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
161500090623     C                   EVAL      VABSCL = SkSplitCSV(i)
161600090623     C                   ENDIF
161700090623     C*** RMO
161800090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
161900090623     C                   EVAL      VABRMO = SkSplitCSV(i)
162000090623     C                   ENDIF
162100090623     C*** NMO
162200090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
162300090623     C                   EVAL      VABNMO = SkSplitCSV(i)
162400090623     C                   ENDIF
162500090623     C*
162600090623     C* Reperisco quindi i campi numerici...
162700090623     C*** NSP
162800090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
162900090623     C                   EVAL      PiStr=SkSplitCSV(i)
163000090623     C                   EXSR      CHKNUM
163100090623     C                   IF        PiInt=*on
163200090623     C                   Z-ADD     PiVal         VABNSP
163300090623     C                   ELSE
163400090623     C                   SETON                                        31
163500090623     C                   EVAL      VABNSP = *zeros
163600090623     C                   EVAL      vinmsg = %trimr(vinmsg)
163700090623     C                             + ' ' + 'VABNSP'
163800090623     C                   ENDIF
163900090623     C                   ENDIF
164000090623     C*** RMN
164100090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
164200090623     C                   EVAL      PiStr=SkSplitCSV(i)
164300090623     C                   EXSR      CHKNUM
164400090623     C                   IF        PiInt=*on
164500090623     C                   Z-ADD     PiVal         VABRMN
164600090623     C                   ELSE
164700090623     C                   SETON                                        32
164800090623     C                   EVAL      VABRMN = *zeros
164900090623     C                   EVAL      vinmsg = %trimr(vinmsg)
165000090623     C                             + ' ' + 'VABRMN'
165100090623     C                   ENDIF
165200090623     C                   ENDIF
165300090623     C*** CAD
165400090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
165500090623     C                   EVAL      PiStr=SkSplitCSV(i)
165600090623     C                   EXSR      CHKNUM
165700090623     C                   IF        PiInt=*on
165800090623     C                   Z-ADD     PiVal         Num5_0
165900090623     C                   MOVEL(p)  Num5_0        VABCAD
166000090623     C                   ELSE
166100090623     C                   EVAL      VABCAD = SkSplitCSV(i)
166200090623     C                   SETON                                        32
166300090623     C                   EVAL      VABCAD = *zeros
166400090623     C                   EVAL      vinmsg = %trimr(vinmsg)
166500090623     C                             + ' ' + 'VABCAD'
166600090623     C                   ENDIF
166700090623     C                   ENDIF
166800090623     C*** NCL
166900090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
167000090623     C                   EVAL      PiStr=SkSplitCSV(i)
167100090623     C                   EXSR      CHKNUM
167200090623     C                   IF        PiInt=*on
167300090623     C                   Z-ADD     PiVal         VABNCL
167400090623     C                   ELSE
167500090623     C                   SETON                                        32
167600090623     C                   EVAL      VABNCL = *zeros
167700090623     C                   EVAL      vinmsg = %trimr(vinmsg)
167800090623     C                             + ' ' + 'VABNCL'
167900090623     C                   ENDIF
168000090623     C                   ENDIF
168100090623     C*** PKB
168200090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
168300090623     C                   EVAL      PiStr=SkSplitCSV(i)
168400090623     C                   EXSR      CHKNUM
168500090623     C                   IF        PiNum=*on
168600090623     C                   Z-ADD     PiVal         VABPKB
168700090623     C                   ELSE
168800090623     C                   SETON                                        32
168900090623     C                   EVAL      VABPKB = *zeros
169000090623     C                   EVAL      vinmsg = %trimr(vinmsg)
169100090623     C                             + ' ' + 'VABPKB'
169200090623     C                   ENDIF
169300090623     C                   ENDIF
169400090623     C*** CAS
169500090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
169600090623     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
169700090623     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
169800090623     C                             %trim(SkSplitCSV(i)) <> '0'      AND
169900090623     C                             %trim(SkSplitCSV(i)) <> '0,00'
170000090623     C                   EVAL      wFlgCAS = '1'
170100090623     C                   ENDIF
170200090623     C                   EVAL      PiStr=SkSplitCSV(i)
170300090623     C                   EXSR      CHKNUM
170400090623     C                   IF        PiNum=*on
170500090623     C                   Z-ADD     PiVal         VABCAS
170600090623     C                   ELSE
170700090623     C                   SETON                                        32
170800090623     C                   EVAL      VABCAS = *zeros
170900090623     C                   EVAL      vinmsg = %trimr(vinmsg)
171000090623     C                             + ' ' + 'VABCAS'
171100090623     C                   ENDIF
171200090623     C                   ENDIF
171300090623     C*** CCM
171400090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
171500090623     C                   EVAL      PiStr=SkSplitCSV(i)
171600090623     C                   EXSR      CHKNUM
171700090623     C                   IF        PiInt=*on      AND
171800090623     C                             PiVal<=9999999 AND
171900090623     C                             PiVal>*zeros
172000090623     C                   Z-ADD     PiVal         VABCCM
172100090623     C                   ELSE
172200090623     C                   SETON                                        32
172300090623     C                   EVAL      VABCCM = *zeros
172400090623     C                   EVAL      vinmsg = %trimr(vinmsg)
172500090623     C                             + ' ' + 'VABCCM'
172600090623     C                   ENDIF
172700090623     C                   ENDIF
172800090623     C*** LNP
172900090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
173000090623     C                   EVAL      PiStr=SkSplitCSV(i)
173100090623     C                   EXSR      CHKNUM
173200090623     C                   IF        PiInt=*on    AND
173300090623     C                             PiVal<=999   AND
173400090623     C                             PiVal>*zeros
173500090623     C                   Z-ADD     PiVal         VABLNP
173600090623     C                   ELSE
173700090623     C                   SETON                                        32
173800090623     C                   EVAL      VABLNP = *zeros
173900090623     C                   EVAL      vinmsg = %trimr(vinmsg)
174000090623     C                             + ' ' + 'VABLNP'
174100090623     C                   ENDIF
174200090623     C                   ENDIF
174300090623     C*** NRS
174400090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
174500090623     C                   EVAL      PiStr=SkSplitCSV(i)
174600090623     C                   EXSR      CHKNUM
174700090623     C                   IF        PiInt=*on AND
174800090623     C                             PiVal<=99
174900090623     C                   Z-ADD     PiVal         VABNRS
175000090623     C                   ELSE
175100090623     C                   SETON                                        32
175200090623     C                   EVAL      vinmsg = %trimr(vinmsg)
175300090623     C                             + ' ' + 'VABNRS'
175400090623     C                   ENDIF
175500090623     C                   ENDIF
175600090623     C*** LNA
175700090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
175800090623     C                   EVAL      PiStr=SkSplitCSV(i)
175900090623     C                   EXSR      CHKNUM
176000090623     C                   IF        PiInt=*on    AND
176100090623     C                             PiVal<=999   AND
176200090623     C                             PiVal>*zeros
176300090623     C                   Z-ADD     PiVal         VABLNA
176400090623     C                   ELSE
176500090623     C                   SETON                                        32
176600090623     C                   EVAL      VABLNA = *zeros
176700090623     C                   EVAL      vinmsg = %trimr(vinmsg)
176800090623     C                             + ' ' + 'VABLNA'
176900090623     C                   ENDIF
177000090623     C                   ENDIF
177100090623     C*** CTR
177200090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
177300090623     C                   EVAL      PiStr=SkSplitCSV(i)
177400090623     C                   EXSR      CHKNUM
177500090623     C                   IF        PiInt=*on  AND
177600090623     C                             PiVal<=999
177700090623     C                   Z-ADD     PiVal         VABCTR
177800090623     C                   ELSE
177900090623     C                   SETON                                        32
178000090623     C                   EVAL      vinmsg = %trimr(vinmsg)
178100090623     C                             + ' ' + 'VABCTR'
178200090623     C                   ENDIF
178300090623     C                   ENDIF
178400090623     C*** IAS
178500090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
178600090623     C                   EVAL      PiStr=SkSplitCSV(i)
178700090623     C                   EXSR      CHKNUM
178800090623     C                   IF        PiNum=*on
178900090623     C                   Z-ADD     PiVal         VABIAS
179000090623     C                   ELSE
179100090623     C                   SETON                                        32
179200090623     C                   EVAL      vinmsg = %trimr(vinmsg)
179300090623     C                             + ' ' + 'VABIAS'
179400090623     C                   ENDIF
179500090623     C                   ENDIF
179600090623     C*** VLB
179700090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
179800090623     C                   EVAL      PiStr=SkSplitCSV(i)
179900090623     C                   EXSR      CHKNUM
180000090623     C                   IF        PiNum=*on
180100090623     C                   Z-ADD     PiVal         VABVLB
180200090623     C                   ELSE
180300090623     C                   SETON                                        32
180400090623     C                   EVAL      vinmsg = %trimr(vinmsg)
180500090623     C                             + ' ' + 'VABVLB'
180600090623     C                   ENDIF
180700090623     C                   ENDIF
180800090623     C*** QFT
180900090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
181000090623     C                   EVAL      PiStr=SkSplitCSV(i)
181100090623     C                   EXSR      CHKNUM
181200090623     C                   IF        PiNum=*on
181300090623     C                   Z-ADD     PiVal         VABQFT
181400090623     C                   ELSE
181500090623     C                   SETON                                        32
181600090623     C                   EVAL      vinmsg = %trimr(vinmsg)
181700090623     C                             + ' ' + 'VABQFT'
181800090623     C                   ENDIF
181900090623     C                   ENDIF
182000090623     C*** NCD
182100090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
182200090623     C                   EVAL      PiStr=SkSplitCSV(i)
182300090623     C                   EXSR      CHKNUM
182400090623     C                   IF        PiInt=*on      AND
182500090623     C                             PiVal<=9999999
182600090623     C                   Z-ADD     PiVal         VABNCD
182700090623     C                   ELSE
182800090623     C                   SETON                                        32
182900090623     C                   EVAL      vinmsg = %trimr(vinmsg)
183000090623     C                             + ' ' + 'VABNCD'
183100090623     C                   ENDIF
183200090623     C                   ENDIF
183300090623     C*** NCA
183400090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
183500090623     C                   EVAL      PiStr=SkSplitCSV(i)
183600090623     C                   EXSR      CHKNUM
183700090623     C                   IF        PiInt=*on      AND
183800090623     C                             PiVal<=9999999
183900090623     C                   Z-ADD     PiVal         VABNCA
184000090623     C                   ELSE
184100090623     C                   SETON                                        32
184200090623     C                   EVAL      vinmsg = %trimr(vinmsg)
184300090623     C                             + ' ' + 'VABNCA'
184400090623     C                   ENDIF
184500090623     C                   ENDIF
184600090623     C*** ZNC
184700090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
184800090623     C                   EVAL      PiStr=SkSplitCSV(i)
184900090623     C                   EXSR      CHKNUM
185000090623     C                   IF        PiInt=*on AND
185100090623     C                             PiVal<=99
185200090623     C                   Z-ADD     PiVal         VABZNC
185300090623     C                   ELSE
185400090623     C                   SETON                                        32
185500090623     C                   EVAL      vinmsg = %trimr(vinmsg)
185600090623     C                             + ' ' + 'VABZNC'
185700090623     C                   ENDIF
185800090623     C                   ENDIF
185900090623     C*** DCR
186000090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
186100090623     C                   EVAL      PiStr=SkSplitCSV(i)
186200090623     C                   EXSR      CHKNUM
186300090623     C                   IF        PiInt=*on       AND
186400090623     C                             PiVal<=99999999
186500090623     C                   Z-ADD     PiVal         VABDCR
186600090623     C                   ELSE
186700090623     C                   SETON                                        32
186800090623     C                   EVAL      vinmsg = %trimr(vinmsg)
186900090623     C                             + ' ' + 'VABDCR'
187000090623     C                   ENDIF
187100090623     C                   ENDIF
187200090623     C*** HCR
187300090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
187400090623     C                   EVAL      PiStr=SkSplitCSV(i)
187500090623     C                   EXSR      CHKNUM
187600090623     C                   IF        PiInt=*on   AND
187700090623     C                             PiVal<=9999
187800090623     C                   Z-ADD     PiVal         VABHCR
187900090623     C                   ELSE
188000090623     C                   SETON                                        32
188100090623     C                   EVAL      vinmsg = %trimr(vinmsg)
188200090623     C                             + ' ' + 'VABHCR'
188300090623     C                   ENDIF
188400090623     C                   ENDIF
188500090623     C*** VMD
188600090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
188700090623     C                   EVAL      PiStr=SkSplitCSV(i)
188800090623     C                   EXSR      CHKNUM
188900090623     C                   IF        PiNum=*on
189000090623     C                   Z-ADD     PiVal         VABVMD
189100090623     C                   ELSE
189200090623     C                   SETON                                        32
189300090623     C                   EVAL      vinmsg = %trimr(vinmsg)
189400090623     C                             + ' ' + 'VABVMD'
189500090623     C                   ENDIF
189600090623     C                   ENDIF
189700090623     C*** ANT
189800090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
189900090623     C                   EVAL      PiStr=SkSplitCSV(i)
190000090623     C                   EXSR      CHKNUM
190100090623     C                   IF        PiInt=*on        AND
190200090623     C                             PiVal<=999999999
190300090623     C                   Z-ADD     PiVal         VABANT
190400090623     C                   ELSE
190500090623     C                   SETON                                        32
190600090623     C                   EVAL      vinmsg = %trimr(vinmsg)
190700090623     C                             + ' ' + 'VABANT'
190800090623     C                   ENDIF
190900090623     C                   ENDIF
191000090623     C*** CMO
191100090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
191200090623     C                   EVAL      PiStr=SkSplitCSV(i)
191300090623     C                   EXSR      CHKNUM
191400090623     C                   IF        PiInt=*on
191500090623     C                   Z-ADD     PiVal         Num5_0
191600090623     C                   MOVEL(p)  Num5_0        VABCMO
191700090623     C                   ELSE
191800090623     C                   SETON                                        32
191900090623     C                   EVAL      VABCMO = *zeros
192000090623     C                   EVAL      vinmsg = %trimr(vinmsg)
192100090623     C                             + ' ' + 'VABCMO'
192200090623     C                   ENDIF
192300090623     C                   ENDIF
192400090623     C*
192500090911     C* Reperisco i campi del EDIVAT
192600090623     C*** VATNOT
192700090623     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
192800090623     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
192900090623     C                   ENDIF
193000090623     C*** VATTRC
193100090623     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
193200090623     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
193300090623     C                   ENDIF
193400090623     C*** VATNOT_A
193500090623     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
193600090623     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
193700090623     C                   ENDIF
193800090623     C*** VATNOT_B
193900090623     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
194000090623     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
194100090623     C                   ENDIF
194200090623     C*** VATNOT_E
194300090623     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
194400090623     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
194500090623     C                   ENDIF
194600090623     C*
194700090623     C***  ===>  Gestione campi "particolari" dentro ciclo
194800090623     C*
194900090623     C*** W_VABNSP
195000090623     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNSP'
195100090623     C                   EVAL      PiStr=SkSplitCSV(i)
195200090623     C                   EXSR      CHKNUM
195300090623     C                   IF        PiInt=*on
195400090623     C                   Z-ADD     PiVal         VABNSP
195500090623     C                   Z-ADD     PiVal         VABRMN
195600090623     C                   ELSE
195700090623     C                   SETON                                        31
195800090623     C                   EVAL      VABNSP = *zeros
195900090623     C                   EVAL      VABRMN = *zeros
196000090623     C                   EVAL      vinmsg = %trimr(vinmsg)
196100090623     C                             + ' ' + 'VABNSP VABRMN'
196200090623     C                   ENDIF
196300090623     C                   ENDIF
196400090623     C*** W_VABIND_1
196500090623     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABIND_1'
196600090623     C                   EVAL      VABIND = %trim(VABIND) + ' ' +
196700090623     C                                      SkSplitCSV(i)
196800090623     C                   ENDIF
196900090623     C*** W_VABIND_2
197000090623     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABIND_2'
197100090623     C                   EVAL      VABIND = %trim(VABIND) + ' ' +
197200090623     C                                      SkSplitCSV(i)
197300090623     C                   ENDIF
197400090623     C*** W_VABNZD
197500090623     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNZD'
197600090623     C                   IF        SkSplitCSV(i) <> 'IT'
197700090623     C                   EVAL      VABNZD = SkSplitCSV(i)
197800090623     C                   ENDIF
197900090623     C                   ENDIF
198000090623     C*** W_VABNOT_1
198100090623     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT_1'
198200090623     C                   EVAL      wNOTE  = %trim(wNOTE) + ' ' +
198300090623     C                                      SkSplitCSV(i)
198400090623     C                   ENDIF
198500090623     C*** W_VABNOT_2
198600090623     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT_2'
198700090623     C                   EVAL      wNOTE  = %trim(wNOTE) + ' ' +
198800090623     C                                      SkSplitCSV(i)
198900090623     C                   ENDIF
199000090623     C*** W_VABNOT_3
199100090623     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT_3'
199200090623     C                   EVAL      wNOTE  = %trim(wNOTE) + ' ' +
199300090623     C                                      SkSplitCSV(i)
199400090623     C                   ENDIF
199500090623     C*** W_VABNOT_4
199600090623     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT_4'
199700090623     C                   EVAL      wNOTE  = %trim(wNOTE) + ' ' +
199800090623     C                                      SkSplitCSV(i)
199900090623     C                   ENDIF
200000090623     C*** W_VABNOT_5
200100090623     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT_5'
200200090623     C                   EVAL      wNOTE  = %trim(wNOTE) + ' ' +
200300090623     C                                      SkSplitCSV(i)
200400090623     C                   ENDIF
200500090904     C*
200600090904     C* Sommo peso e volume solamente a rottura d "chi sono"
200700090904     C*
200800090904     C* => se "chi sono" campo 7
200900090904     C                   IF        SkSplitCSV(7) <> *blanks
201000090904     C*** W_VABPKB
201100090904     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABPKB'
201200090909     C                   IF        SkSplitCSV(7) <> sVATNOT_E
201300090904     C                   EVAL      PiStr='0'+SkSplitCSV(i)
201400090904     C                   EXSR      CHKNUM
201500090904     C                   IF        PiNum=*on
201600090904     C                   ADD       PiVal         VABPKB
201700090904     C                   ELSE
201800090904     C                   SETON                                        32
201900090904     C                   EVAL      vinmsg = %trimr(vinmsg)
202000090904     C                             + ' ' + 'VABPKB'
202100090904     C                   ENDIF
202200090904     C                   ENDIF
202300090904     C                   ENDIF
202400090904     C*** W_VABVLB
202500090904     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABVLB'
202600090909     C                   IF        SkSplitCSV(7) <> sVATNOT_E
202700090904     C                   EVAL      PiStr='0'+SkSplitCSV(i)
202800090904     C                   EXSR      CHKNUM
202900090904     C                   IF        PiNum=*on
203000090904     C                   ADD       PiVal         VABVLB
203100090904     C                   ELSE
203200090904     C                   SETON                                        32
203300090904     C                   EVAL      vinmsg = %trimr(vinmsg)
203400090904     C                             + ' ' + 'VABVLB'
203500090904     C                   ENDIF
203600090904     C                   ENDIF
203700090904     C                   ENDIF
203800090904     C*
203900090904     C                   ELSE
204000090904     C*
204100090904     C* => se "chi sono" campo 26
204200090904     C                   IF        SkSplitCSV(26) <> *blanks
204300090904     C*** W_VABPKB
204400090904     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABPKB'
204500090904     C                   IF        '00'+SkSplitCSV(26) <> sVATNOT_E
204600090904     C                   EVAL      PiStr='0'+SkSplitCSV(i)
204700090904     C                   EXSR      CHKNUM
204800090904     C                   IF        PiNum=*on
204900090904     C                   ADD       PiVal         VABPKB
205000090904     C                   ELSE
205100090904     C                   SETON                                        32
205200090904     C                   EVAL      vinmsg = %trimr(vinmsg)
205300090904     C                             + ' ' + 'VABPKB'
205400090904     C                   ENDIF
205500090904     C                   ENDIF
205600090904     C                   ENDIF
205700090904     C*** W_VABVLB
205800090904     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABVLB'
205900090904     C                   IF        '00'+SkSplitCSV(26) <> sVATNOT_E
206000090904     C                   EVAL      PiStr='0'+SkSplitCSV(i)
206100090904     C                   EXSR      CHKNUM
206200090904     C                   IF        PiNum=*on
206300090904     C                   ADD       PiVal         VABVLB
206400090904     C                   ELSE
206500090904     C                   SETON                                        32
206600090904     C                   EVAL      vinmsg = %trimr(vinmsg)
206700090904     C                             + ' ' + 'VABVLB'
206800090904     C                   ENDIF
206900090904     C                   ENDIF
207000090904     C                   ENDIF
207100090904     C*
207200090904     C                   ENDIF
207300090904     C                   ENDIF
207400090904     C*
207500090904     C*** W_VATNOT_E_1
207600090904     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_E_1'
207700090904     C                   EVAL      wVATNOT_E_1 = '00'+%trim(SkSplitCSV(i))
207800090703     C                   ENDIF
207900090904     C*** W_VATNOT_E_2
208000090904     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_E_2'
208100090909     C                   EVAL      wVATNOT_E_2 = %trim(SkSplitCSV(i))
208200090904     C                   ENDIF
208300090623     C*** W_VAXDTT_1
208400090623     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAXDTT_1'
208500090623     C                   EVAL      wVAXDTT_1 = SkSplitCSV(i)
208600090623     C                   ENDIF
208700090623     C*** W_VAXDTT_2
208800090623     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAXDTT_2'
208900090623     C                   EVAL      wVAXDTT_2 = SkSplitCSV(i)
209000090623     C                   ENDIF
209100090623     C*** W_VAXDTT_3
209200090624     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAXDTT_3'
209300090624     C                   EVAL      wVAXDTT_3 = SkSplitCSV(i)
209400090624     C                   ENDIF
209500090624     C*** W_VAX_NUMBP
209600090624     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAX_NUMBP'
209700090624     C                   EVAL      wVAX_NUMBP = SkSplitCSV(i)
209800090925     C                   EVAL      VABRMA = SkSplitCSV(i)
209900090624     C                   ENDIF
210000090624     C*** W_VAX_NUMORD
210100090624     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAX_NUMORD'
210200090624     C                   EVAL      wVAX_NUMORD = SkSplitCSV(i)
210300090624     C                   ENDIF
210400090624     C*** W_VAX_CODCLI
210500090624     C                   IF        %trim(SkSplitFLD(i)) = 'W_VAX_CODCLI'
210600090624     C                   EVAL      wVAX_CODCLI = SkSplitCSV(i)
210700090624     C                   ENDIF
210800090623     C*
210900090623     C***  <===  -----------------------------------------
211000090908     C*
211100090908     C                   IF        %trim(wVATNOT_E_2) <> *blanks
211200090908     C                   EVAL      wVATNOT_E = wVATNOT_E_2
211300090908     C                   ELSE
211400090908     C                   EVAL      wVATNOT_E = wVATNOT_E_1
211500090908     C                   ENDIF
211600090623     C*
211700090623     C                   ENDSR
211800090623     C***
211900090623
212000010601
212100020204
212200020204     C*----------------------------------------------------*
212300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
212400020204     C*----------------------------------------------------*
212500020204     C     CHKIMPDIV     BEGSR
212600020204     C*
212700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
212800020204     C                   Z-ADD     *zeros        wrkDec            9 9
212900020204     C*
213000020204     C* Come prima cosa effettuo considerazioni sulla divisa
213100020204     C                   IF        vabIAS > *zeros
213200020204     C                   IF        vabVAS <> 'EUR'
213300020204     C                   EVAL      vabVAS =  'ITL'
213400020204     C                   ENDIF
213500020204     C                   ENDIF
213600020204     C*
213700020204     C                   IF        vabCAS > *zeros
213800020204     C                   IF        vabVCA <> 'EUR'
213900020204     C                   EVAL      vabVCA =  'ITL'
214000020204     C                   ENDIF
214100020204     C                   ENDIF
214200020204     C*
214300020204     C                   IF        vabVMD > *zeros
214400020204     C                   IF        vabVAD <> 'EUR'
214500020204     C                   EVAL      vabVAD =  'ITL'
214600020204     C                   ENDIF
214700020204     C                   ENDIF
214800020204     C*
214900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
215000020204     C                   Z-ADD     vabIAS        wrkDec
215100020204     C                   IF        wrkDec > *zeros
215200020204     C                   IF        vabVAS = 'ITL'
215300020204     C                   EVAL      vabIAS = *zeros
215400020204     C                   ENDIF
215500020204     C                   ENDIF
215600020204     C*
215700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
215800020204     C                   Z-ADD     vabCAS        wrkDec
215900020204     C                   IF        wrkDec > *zeros
216000020204     C                   IF        vabVCA = 'ITL'
216100020204     C                   EVAL      vabCAS = *zeros
216200020204     C                   ENDIF
216300020204     C                   ENDIF
216400020204     C*
216500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
216600020204     C                   Z-ADD     vabVMD        wrkDec
216700020204     C                   IF        wrkDec > *zeros
216800020204     C                   IF        vabVAD = 'ITL'
216900020204     C                   EVAL      vabVMD = *zeros
217000020204     C                   ENDIF
217100020204     C                   ENDIF
217200020204     C*
217300020204     C                   ENDSR
217400020204     C***
217500080617
217600080617
217700080617
217800080617     C*----------------------------------------------------*
217900080617     C*  CONTROLLO NUMERICITA' CAMPI
218000080617     C*----------------------------------------------------*
218100080617     C     CHKNUM        BEGSR
218200080617     C*
218300080617     C                   IF        PiDecChr = *blanks
218400080617     C                   EVAL      PiDecChr = CharNUM
218500080617     C                   ENDIF
218600080617     C*
218700080617     C                   callp     UBISNUM_Check(PiStr
218800080617     C                                          :PiDecChr
218900080617     C                                          :PiVal
219000080617     C                                          :PiNum
219100080617     C                                          :PiInt)
219200080617     C*
219300080617     C                   ENDSR
219400080617     C***
219500010330
219600010601
219700010601
219800010601
219900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
220000010601     C     repfil        BEGSR
220100010601     C*
220200010601     C                   if        invfil = *zeros and
220300010601     C                             depfil > *zeros and
220400010629     C                             (vinflg = *blanks or
220500010629     C                              vinflg = *zeros)
220600010601     C
220700010601     C                   eval      invfil = depfil
220800010601     C                   endif
220900010601     C*
221000010601     C                   if        depfil <> invfil and
221100010601     C                             invfil > *zeros
221200010601     C                   eval      flgMulti = '1'
221300010601     C                   if        vinflg = *blanks
221400010601     C                   add       1             cntNonEl
221500010601     C                   endif
221600010601     C                   endif
221700010601     C*
221800010601     C                   if        vinflg = '2'
221900010601     C                   eval      flgStato = '2'
222000010601     C                   endif
222100010601     C*
222200010601     C                   ENDSR
222300010601     C***
222400010601
222500010601
222600010601
222700010330
222800040119      /TITLE Invio dei dati al punto operativo.
222900040119     C     invio         BEGSR
223000090623     C*
223100090911     C* 1° invio EDIVAX
223200090623     C                   reset                   dscmz
223300090623     C                   move      vlrpoi        cmzdst
223400090911     C                   eval      cmzfld = 'EDIVAXWR'
223500090623     C                   eval      cmzmbd = vlrhdl
223600090623     C                   eval      %subst(cmzmbd:1:1) = 'M'
223700090623     C***                if        prmfir = *blanks
223800090911     C                   eval      cmzfla = 'EDIVAX0F'
223900090911     C                   eval      cmzmba = 'EDIVAX0F'
224000090623     C***                else
224100090623     C***                eval      cmzfla = prmfir
224200090623     C***                eval      cmzmba = prmfir
224300090623     C***                endif
224400090623     C                   eval      cmznrr = *zeros
224500090623     C                   move      §ctrokvx      cmznrr
224600090623     C                   eval      cmzlba = vlrfl1
224700090623     C                   call(e)   'TIS711C'
224800090623     C                   parm                    dscmz
224900090623     C                   parm      *blanks       esito
225000090623     C                   if        %error
225100090623     C                             or cmzerr = '1'
225200090623     C                             or esito  = '1'
225300090623     C                   eval      wrkesito = '3'
225400090623     C                   else
225500040119     C*
225600090911     C* 2° invio EDIVAT
225700040119     C                   reset                   dscmz
225800040119     C                   move      vlrpoi        cmzdst
225900090911     C                   eval      cmzfld = 'EDIVATWR'
226000040119     C                   eval      cmzmbd = vlrhdl
226100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
226200040119     C***                if        prmfir = *blanks
226300090911     C                   eval      cmzfla = 'EDIVAT0F'
226400090911     C                   eval      cmzmba = 'EDIVAT0F'
226500040119     C***                else
226600040119     C***                eval      cmzfla = prmfir
226700040119     C***                eval      cmzmba = prmfir
226800040119     C***                endif
226900040119     C                   eval      cmznrr = *zeros
227000040119     C                   move      §ctrokvt      cmznrr
227100040119     C                   eval      cmzlba = vlrfl1
227200040119     C                   call(e)   'TIS711C'
227300040119     C                   parm                    dscmz
227400040119     C                   parm      *blanks       esito
227500040119     C                   if        %error
227600040119     C                             or cmzerr = '1'
227700040119     C                             or esito  = '1'
227800040119     C                   eval      wrkesito = '3'
227900040119     C                   else
228000040119     C*
228100090911     C* 3° invio EDIVAB
228200040119     C                   reset                   dscmz
228300040119     C                   move      vlrpoi        cmzdst
228400040119     C                   eval      cmzfld = vlrfou
228500040119     C                   eval      cmzmbd = vlrhdl
228600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
228700040119     C***                if        prmfir = *blanks
228800090911     C                   eval      cmzfla = 'EDIVAB0F'
228900090911     C                   eval      cmzmba = 'EDIVAB0F'
229000040119     C***                else
229100040119     C***                eval      cmzfla = prmfir
229200040119     C***                eval      cmzmba = prmfir
229300040119     C***                endif
229400040119     C                   eval      cmznrr = *zeros
229500040119     C                   move      §ctrokvb      cmznrr
229600040119     C                   eval      cmzlba = vlrfl1
229700040119     C                   call(e)   'TIS711C'
229800040119     C                   parm                    dscmz
229900040119     C                   parm      *blanks       esito
230000040119     C                   if        %error
230100040119     C                             or cmzerr = '1'
230200040119     C                             or esito  = '1'
230300040119     C                   eval      wrkesito = '3'
230400040119     C                   endif
230500040119     C                   endif
230600090623     C                   endif
230700040119     C*
230800040119     C                   ENDSR
230900040119     C***
231000010601
231100010601
231200010601
231300010601
231400010601      /TITLE Invio dei dati al punto operativo.
231500010601     C     opeini        BEGSR
231600010601     C*
231700010601     C* Inizializzo flag e contatori operativi
231800010601     C                   movel     '0'           flgGiro           1
231900010601     C                   movel     '0'           flgMulti          1
232000010601     C                   movel     '1'           flgStato          1
232100010615     C                   movel     '0'           flgOk             1
232200010601     C                   z-add     *zeros        cntNonEl         10 0
232300010601     C                   z-add     *zeros        depfil            3 0
232400010601     C                   z-add     *zeros        invfil            3 0
232500010601     C*
232600010601     C                   ENDSR
232700010601     C***
232800070326
232900070326
233000070326
233100070326
233200080916     C     *pssr         BEGSR
233300070329     C*
233400080916     C                   if        %open(tivin00r)
233500080916     C                   close     tivin00r
233600080916     C                   endif
233700090911     C                   if        %open(edivabwr)
233800090911     C                   close     edivabwr
233900080916     C                   endif
234000090911     C                   if        %open(edivatwr)
234100090911     C                   close     edivatwr
234200080916     C                   endif
234300090911     C                   if        %open(edivaxwr)
234400090911     C                   close     edivaxwr
234500090623     C                   endif
234600070326     C*
234700070326     C* Effettuo la chiamata al CLLE preposto
234800090911     C                   call(e)   'TITVEVTXC'
234900080916     C                   parm                    parccm
235000080916     C                   parm                    parmbr
235100080916     C                   parm      '2'           paropz
235200070326     C*
235300080916     C                   eval      wrkesito = '2'
235400070404     C*
235500080916     C                   seton                                        LR
235600070326     C*
235700080916     C                   ENDSR     '*CANCL'
235800070326     C***
235900070326
236000070326
236100010330
236200010330
236300000613     C     *inzsr        BEGSR
236400990910     C*
236500990910     C     *entry        plist
236600990920     C                   parm                    tivlrds
236700990921     C                   parm      wrkesito      esito
236800000724     C                   parm                    prmlit
236900000710     C                   parm                    prmfir
237000090911     C*
237100090911     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
237200090911     C                   move(p)   vlrMSG        wNomeFile
237300090911     C*
237400090911     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
237500090911     C* del codice CMR dal nome del file importato.
237600090911     C                   if        wNomeFile <> *blanks AND
237700090911     C                             vlrppt<>*blanks
237800090911     C                   eval      wCMR = %subst(wNomeFile:7:6)
237900090911     C                   else
238000090911     C                   eval      wCMR = *all'*'
238100090911     C                   endif
238200010330     C*
238300010330     C* CALCOLA LA DATA CORRENTE
238400010330     C                   time                    wn14             14 0
238500010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
238600090911     C                   movel     wn14          oracor            6 0          *ORA
238700010330     C                   z-add     wn8           g08dat
238800010330     C                   z-add     *zeros        g08inv
238900010330     C                   movel     '0'           g08err
239000010330     C                   call      'XSRDA8'
239100010330     C                   parm                    wlbda8
239200010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
239300000613     C*
239400000613     C                   ENDSR
239500000613     C***
239600990908
239700090623     Otitv1q7p  E            riepilogo         2
239800990915     O                                              'Upload via Internet'
239900990915     O                                           +1 'Traduzione TIVIN00R -'
240000090911     O                                           +1 'EDIVABWR'
240100990915     O                                           +1 'Testate bolle'
240200990915     O          E            riepilogo   2
240300990915     O                       wrkdata
240400990915     O                       wrkora              +1
240500990915     O                       procname            +1
240600990915     O          E            riepilogo   2
240700990915     O                                              'Cliente..................:'
240800010605     O                       VABCCM        z     +1
240900990915     O          E            riepilogo   2
241000990920     O                                              'Riferimento Strategi.....:'
241100990920     O                       vlrhdl              +1
241200990915     O          E            riepilogo   2
241300990915     O                                              'Giusti...................:'
241400040119     O                       §CTROKVB      2   +  1
241500990915     O          E            riepilogo   2
241600990915     O                                              'Sbagliati e corretti.....:'
241700971022     O                       §CTRMO        2   +  1
241800990915     O          E            riepilogo   2
241900990915     O                                              'Sbagliati e scartati.....:'
242000971022     O                       §CTRNO        2   +  1
242100000613
242200090623     Otitv1q7ps E            testdett          2
242300000613     O                                              'Upload via Internet'
242400000613     O                                           +1 'Traduzione TIVIN00R -'
242500090911     O                                           +1 'EDIVABWR'
242600000613     O                                           +1 'Testate bolle'
242700000616     O          E            testdett    3
242800000613     O                                           +2 'N° rec'
242900000613     O                                           +3 'Anteprima contenuto'
243000000616     O          E            rigadett    2
243100000613     O                       rrnum               +2
243200000621     O                       recko               +3
243300000616     O          E            findett     2
243400000613     O                       wrkdata
243500000613     O                       wrkora              +1
243600000613     O                       procname            +1
243700000616     O          E            findett     2
243800000613     O                                              'Cliente..................:'
243900010605     O                       VABCCM        z     +1
244000000616     O          E            findett     2
244100000613     O                                              'Riferimento Strategi.....:'
244200000613     O                       vlrhdl              +1
244300000616     O          E            findett     2
244400000613     O                                              'Giusti...................:'
244500040119     O                       §CTROKVB      2   +  1
244600000616     O          E            findett     2
244700000613     O                                              'Sbagliati e corretti.....:'
244800000613     O                       §CTRMO        2   +  1
244900000616     O          E            findett     2
245000000613     O                                              'Sbagliati e scartati.....:'
245100000613     O                       §CTRNO        2   +  1
245200000613** CMD - COMANDI CL
245300090623OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1Q7P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
245400090623OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1Q7PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
245500090623DLTOVR FILE(TITV1Q7P TITV1Q7PS) LVL(*)
245600010606
245700000613
245800090623** ENTEXP - INTESTAZIONE COLONNE PERSONALIZZATA TIPO RECORD "ENTEXP"
245900090925X;X;X;X;W_VABNSP;W_VAX_NUMBP;X;X;X;X;X;X;X;X;VABRSD;VABRD2;
246000090623W_VABIND_1;W_VABIND_2;VABLOD;VABCAD;X;W_VABNZD;X;X;
246100090623W_VABNOT_1;W_VABNOT_2;W_VABNOT_3;W_VABNOT_4;W_VABNOT_5;VABPRD;X;X;X;
246200090624X;X;W_VAX_NUMORD;W_VAX_CODCLI;X;X;X;X;X;X;X;X;X
246300090623
246400090623** LIGEXP - INTESTAZIONE COLONNE PERSONALIZZATA TIPO RECORD "LIGEXP"
246500090904X;X;X;X;W_VABPKB;X;W_VATNOT_E_2;X;X;X;X;X;W_VAXDTT_1;X;X;W_VAXDTT_2;X;
246600090904W_VAXDTT_3;X;X;X;X;X;X;X;W_VATNOT_E_1;X;X;X;X;X;X;X;X;X;X;
246700090623X;X;X;X;X;X;X;
246800090623
246900090623
247000090624** PRTCONST - COSTANTI IN STAMPA
247100090624CODICE CLIENTE         NUM.ORDINE        NUM.BP
247200090624_______________        __________        __________
247300090703   ARTICOLO       DESCRIZIONE                                Q.TA'
247400090624   __________     ___________________________________        __________
247500090624____________________________________________________________________________
