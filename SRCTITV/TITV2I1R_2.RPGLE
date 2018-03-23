000100101005      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR
000200080617     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600090911     FEDIVABwr  O    E             DISK    usropn
000700090911     FEDIVATwr  O    E             DISK    usropn
000800090623     Ftitv1q7p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000080916     F                                     infsr(*pssr)
001100090623     Ftitv1q7ps O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300080916     F                                     infsr(*pssr)
001400000313     D*
001500000613     D*------------
001600000613     D* COMANDI
001700000613     D*------------
001800011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900040113     D*------------
002000110923     D prefix          S             10    inz
002100110923     D header          S                   like(vindta) inz
002200110923     D parcel          S                   like(vindta) inz
002300010330     D*----------------------------------------------------
002400010330     D* DICHIARAZIOINE VARIABILI DI WRK
002500010330     D*----------------------------------------------------
002600010330     D dscmz         e ds                  inz
002700010330     D psds           sds
002800010330     D  procname         *PROC
002900010330     D tivlrds       e ds                  extname(tivlr00f)
003000040128     D tisi95ds      e ds
003100010330     D esito           s              1
003200010330     D prmlit          s             10
003300010330     D prmfir          s             10
003400010330     D wrkesito        s                   like(esito)
003500010330     D wrkdata         s               d
003600010330     D wrkora          s               t
003700010330     D rrnum           s              6  0 INZ(*zeros)
003800010330     D recko           s            150    INZ(*blanks)
003900011113     D depcmd          s            150    INZ(*blanks)
004000040119     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004100040510     D parccm          s              8    INZ(*blanks)
004200040119     D parmbr          s             10    INZ(*blanks)
004300040119     D paropz          s              1    INZ(*blanks)
004400040119     D chkcall         s              1    INZ(*blanks)
004500080125     D wFlgCAS         s              1    INZ(*blanks)
004600080716     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
004700090911     D wCMR            s             35    INZ(*blanks)
004800030715     D*------------------
004900030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005000030715     D*------------------
005100111118     D SkSplitFLD      S             32    DIM(200)
005200111118     D SkSplitCSV      S            256    DIM(200)
005300030715     D CharCSV         S              1
005400030715     D CharTXT         S              1
005500030715     D CharNUM         S              1
005600030715     D posDa           S              3  0 INZ(*zeros)
005700030715     D posA            S              3  0 INZ(*zeros)
005800030715     D i               s              3  0 INZ(1)
005900030715     D wGiro           s              1  0 INZ(*zeros)
006000080125     D wVATNOT_A       s                   LIKE(VATNOT)
006100080125     D wVATNOT_B       s                   LIKE(VATNOT)
006200090904     D wVATNOT_E       s                   LIKE(VATNOT)
006300111117     D wVATNOT_E_1     s                   LIKE(VATNOT)
006400111117     D wVATNOT_E_2     s                   LIKE(VATNOT)
006500111118     D wVATNOT_E_3     s                   LIKE(VATNOT)
006600111118     D wVATNOT_E_4     s                   LIKE(VATNOT)
006700030822     D*------------------
006800030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006900030822     D*------------------
007000030822     D posDaDft        S              3  0 INZ(*zeros)
007100030822     D posADft         S              3  0 INZ(*zeros)
007200030822     D j               s              3  0 INZ(1)
007300030822     D wGiroDft        s              1  0 INZ(*zeros)
007400031201     D*------------------
007500031201     D* Costanti
007600031201     D*------------------
007700031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007800031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007900080617     D*------------------
008000080617
008100080617     D*------------------
008200080617     D* LINKING A DEFINIZIONI ESTERNE
008300080617     D*------------------
008400080617     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008500090220     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
008600010330
008700010330
008800010330
008900990915     C                   time                    wrkdata
009000990915     C                   time                    wrkora
009100000913     C                   reset                   rrnum
009200990921     C                   reset                   esito
009300081014     C                   reset                   wrkesito
009400010601     C*
009500010601     C                   exsr      opeini
009600010605     C                   exsr      rwvab
009700081014     C*
009800081014     C                   exsr      endela
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
014100090623     C                   if        not %open(titv1q7ps)
014200090623     C                   open      titv1q7ps
014300070329     C                   except    testdett
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
015700010601     C     ENDELA        BEGSR
015800081014     C*
015900081014     C                   if        %open(tivin00r)
016000081014     C                   close     tivin00r
016100081014     C                   endif
016200090911     C                   if        %open(edivabwr)
016300090911     C                   close     edivabwr
016400081014     C                   endif
016500090911     C                   if        %open(edivatwr)
016600090911     C                   close     edivatwr
016700081014     C                   endif
016800040119     C*
016900040119     C* Effettuo la chiamata al CLLE preposto
017000101005     C                   call(e)   'TITVEVTC'
017100040510     C                   parm                    parccm
017200040119     C                   parm                    parmbr
017300040119     C                   parm      '2'           paropz
017400050201     C*
017500050201     C* Effettuo lancio TISI95 solo x chiusura
017600050201     C                   CLEAR                   TISI95DS
017700050201     C                   EVAL      I95TLA = 'C'
017800050201     C                   CALL      'TISI95R'
017900050201     C                   PARM                    TISI95DS
018000000613     C*
018100090623     C                   if        %open(titv1q7ps)
018200000616     C                   except    findett
018300090623     C                   close     titv1q7ps
018400000613     C                   endif
018500000616     C*
018600000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
018700011113     C                   Z-ADD     150           LENGH            15 5
018800010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
018900000616     C                   PARM                    CMD(3)
019000000616     C                   PARM                    LENGH
019100000616     C*
019200010601     C                   ENDSR
019300010601     C***
019400010601
019500010601
019600010601
019700000613
019800000613     C*--------------------------------------------------------
019900000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
020000000613     C*--------------------------------------------------------
020100000621     C     STPR          BEGSR
020200000613     C*
020300090623     C                   if        not %open(titv1q7p)
020400090623     C   50              open      titv1q7p
020500990915     C                   endif
020600990915     C*
020700030715     C   50              except    riepilogo
020800990915     C*
020900090623     C                   if        %open(titv1q7p)
021000090623     C   50              close     titv1q7p
021100990914     C                   endif
021200000613     C*
021300000613     C                   ENDSR
021400000613     C***
021500000613
021600010601
021700010601
021800010330     C*--------------------------------------------------------
021900090911     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
022000010330     C*--------------------------------------------------------
022100010605     C     RWVAB         BEGSR
022200010330     C*
022300010330     C                   if        not %open(tivin00r)
022400010330     C                   open      tivin00r
022500010330     C                   endif
022600090911     C                   if        not %open(edivabwr)
022700090911     C                   open      edivabwr
022800010330     C                   endif
022900040119     C*
023000101005     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
023100101005     C                   exsr      prevat
023200040119     C*
023300040119     C                   if        chkcall = '0'
023400040119     C*
023500090911     C                   if        not %open(edivatwr)
023600090911     C                   open      edivatwr
023700040119     C                   endif
023800090623     C*
023900040119     C                   clear                   §CTROKVB          7 0
024000040119     C                   clear                   §CTROKVT          7 0
024100040119     C                   clear                   §CTRMO            7 0
024200040119     C                   clear                   §CTRNO            7 0
024300090623     C*
024400090623     C* Inizializzazioni
024500090623     C                   exsr      inzvar
024600090623     C                   z-add     *zeros        wNewBolla         7 0
024700010330     C*
024800030822     C                   do        *HIVAL
024900010330     C*
025000030822     C                   read      tivin00r                               70
025100010618     C*
025200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
025300010618     C                   if        vinflg = '1'
025400010618     C                   eval      flgOk = '1'
025500010618     C                   endif
025600040510     C*
025700040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
025800040510     C                   z-add     *zeros        wLenVINDTA        4 0
025900040510     C                   z-add     *zeros        wFlgVINDTA        1 0
026000040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
026100040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
026200040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
026300040510     C                   z-add     1             wFlgVINDTA
026400040510     C                   leave
026500040510     C                   endif
026600040510     C                   enddo
026700110923     C*
026800110923     C* Ignoro i record che iniziano x '#'
026900110923     C                   if        %subst(vindta:1:1)  ='#'                  AND
027000110923     C                             %subst(vindta:1:18)<>'#DEF;MPSEXP:HEADER' AND
027100110923     C                             %subst(vindta:1:18)<>'#DEF;MPSEXP:PARCEL'
027200111118     C                   z-add     0             wFlgVINDTA
027300111118     C                   else
027400110923     C*
027500110923     C* Setto i column headers "variabili"
027600111118     C                   if        %subst(vindta:1:18) = '#DEF;MPSEXP:HEADER'
027700110923     C                   eval      header = %trim(%subst(vindta:13))
027800110923     C                   endif
027900111118     C                   if        %subst(vindta:1:18) = '#DEF;MPSEXP:PARCEL'
028000110923     C                   eval      parcel = %trim(%subst(vindta:13))
028100110923     C                   endif
028200111118     C                   endif
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
030000111117     C***                if        %subst(%trim(vindta):1:1) = CharCSV
030100111117     C***                eval      vindta  = '*' + %trim(vindta)
030200111117     C***                endif
030300010601     C*
030400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
030500010605     C                   eval      depfil = VABLNP
030600010601     C                   exsr      repfil
030700010601     C                   if        depfil = invfil
030800021025     C                   if        vlrpoi = 999
030900030822     C                   move(P)   invfil        VABFGS
031000021025     C                   else
031100030822     C                   move(P)   vlrpoi        VABFGS
031200021025     C                   endif
031300090914     C*
031400090914     C                   move      VABFGS        currFGS           3 0
031500090914     C*
031600090914     C* Elaborazione record corrente
031700090914     C                   exsr      elabCurrRec
031800010601     C*
031900010601     C                   exsr      PREELA
032000010604     C*
032100010604     C                   if        *in31 = *off and
032200010604     C                             *in32 = *off
032300010604     C                   eval      vinflg = '1'
032400010604     C                   else
032500010604     C                   eval      recko = vindta
032600010604     C                   except    rigadett
032700010604     C                   eval      vinflg = '2'
032800010604     C                   endif
032900010604     C                   endif
033000010601     C*
033100010604     C                   endif
033200010604     C*
033300010330     C                   else
033400010330     C                   eval      vinflg = '1'
033500010330     C                   endif
033600010601     C*
033700010601     C  N70              update    tivin000
033800010330     C*
033900030822     C  N70              enddo
034000090623     C*
034100090623     C* Scarico la testata rimasta "in canna"
034200090623     C  N31              exsr      wriVAB
034300040119     C*
034400040119     C                   endif                                                  (endif - chkcall)
034500010601     C*
034600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
034700010601     C                   if        cntNonEl = *zeros or
034800010601     C                             flgMulti = '0'
034900010330     C* Se non ci sono record con errori ...
035000010601     C                   if        §ctrno = 0 and
035100010604     C                             §ctrmo = 0 and
035200010601     C                             flgStato <> '2'
035300010330     C* ... restituisco esito OK.
035400010330     C                   eval      wrkesito = '0'
035500010330     C                   else
035600040119     C                   if        §ctrokvb > 0
035700010330     C                   eval      wrkesito = '1'
035800010330     C                   else
035900010615     C                   if        flgOk = '0'
036000010615     C                   eval      wrkesito = '2'
036100010615     C                   else
036200010615     C                   eval      wrkesito = '6'
036300010615     C                   endif
036400010330     C                   endif
036500010330     C                   endif
036600010601     C                   else
036700010601     C                   eval      wrkesito = '9'
036800010601     C                   endif
036900010330     C*
037000010330     C                   if        %open(tivin00r)
037100010330     C                   close     tivin00r
037200010330     C                   endif
037300090911     C                   if        %open(edivabwr)
037400090911     C                   close     edivabwr
037500010330     C                   endif
037600090911     C                   if        %open(edivatwr)
037700090911     C                   close     edivatwr
037800040512     C                   endif
037900010601     C*
038000010601     C                   if        vlrpoi <> 999
038100010601     C                   eval      invfil = vlrpoi
038200010601     C                   endif
038300010330     C*
038400040119     C                   if        §ctrokvb > 0
038500010601     C                             and invfil > *zeros
038600010330     C                   exsr      invio
038700010330     C                   endif
038800010601     C*
038900010618     C                   if        flgGiro = '1'
039000081014     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
039100010618     C                   endif
039200010330     C*
039300010330     C                   ENDSR
039400010330     C***
039500090623
039600090623
039700090623
039800090623     C*----------------------------------------------------*
039900090623     C*  INIZIALIZZAZIONI CAMPI DI WRK
040000090623     C*----------------------------------------------------*
040100090623     C     INZWRK        BEGSR
040200090623     C*
040300090623     C                   eval      i = 1
040400090623     C                   eval      posDa = *zeros
040500090623     C                   eval      posA  = *zeros
040600090623     C*
040700090623     C                   ENDSR
040800090623
040900090623
041000090623
041100090623     C*----------------------------------------------------*
041200090623     C*  ELABORAZIONE RECORD CORRENTE
041300090623     C*----------------------------------------------------*
041400090623     C     ELABCURRREC   BEGSR
041500090623     C*
041600090623     C                   select
041700090623     C*
041800110923     C* Tipo record "TESTATA" - HEADER
041900111118     C                   when      %subst(vindta:1:6) = 'HEADER'
042000090623     C                   if        wNewBolla > *zeros
042100090623     C  N31              exsr      wriVAB
042200090623     C                   exsr      inzvar
042300090623     C                   endif
042400090624     C                   eval      wNewBolla = 1
042500090623     C                   eval      wGiro = *zeros
042600110923     C                   clear                   SkSplitFLD
042700110923     C                   eval      prefix = 'HDR_'
042800090624     C                   exsr      inzwrk
042900090623     C                   exsr      impvab
043000090623     C                   exsr      inzwrk
043100090623     C                   exsr      impvab
043200090623     C*
043300110923     C* Tipo record "DETTAGLIO" - PARCEL
043400110923     C                   when      %subst(vindta:1:6) = 'PARCEL'
043500090623     C                   eval      wGiro = *zeros
043600110923     C                   clear                   SkSplitFLD
043700110923     C                   eval      prefix = 'PCL_'
043800090623     C                   exsr      inzwrk
043900101005     C                   exsr      impvat
044000090623     C                   exsr      inzwrk
044100101005     C                   exsr      impvat
044200090624     C*
044300090624     C  N31              exsr      wrivat
044400090623     C*
044500090623     C*
044600090623     C                   endsl
044700090623     C*
044800090623     C                   ENDSR
044900040119
045000040119
045100040119
045200040119     C*----------------------------------------------------*
045300040119     C*  SCARICAMENTO BUFFER RECORDS VAB
045400040119     C*----------------------------------------------------*
045500040510     C     WRIVAB        BEGSR
045600090623     C*
045700090623     C  N31              add       1             §CTROKVB
045800090623     C   32              add       1             §CTRMO
045900090623     C   31              add       1             §CTRNO
046000040119     C*
046100060331     C* Gestisco l'eventuale rottura x numero spedizione
046200070823     C                   if        VABNSP <> depspe
046300070301     C                   if        wVATNOT_E <> *blanks
046400080125     C                   if        VABCTM = *blanks
046500080125     C                   eval      VABCTM = '7Q'
046600080125     C                   endif
046700070301     C                   endif
046800090911     C*
046900090911     C* VALORIZZO CAMPI RELATIVI AL "CMR"
047000090911     C                   eval      VABCMR = wCMR
047100090911     C                   eval      VABDCM = DATCOR
047200090911     C                   eval      VABDTS = DATCOR
047300090911     C                   eval      VABHMS = ORACOR
047400090911     C                   eval      VABCNT = 1
047500090911     C****
047600090911     C                   write     edivab00                                     => scarico il VAB
047700060331     C                   endif
047800090623     C*
047900090623     C  N31              eval      depspe = vabNSP
048000040119     C*
048100040119     C                   ENDSR
048200040119
048300040119
048400040119
048500040119     C*----------------------------------------------------*
048600040119     C*  SCARICAMENTO BUFFER RECORDS VAT
048700040119     C*----------------------------------------------------*
048800040510     C     WRIVAT        BEGSR
048900060512     C*
049000060512     C* Inizializzo i campi chiave
049100090914     C                   eval      VATFGS = currFGS
049200060512     C                   eval      VATAAS = VABAAS
049300060512     C                   eval      VATCCM = VABCCM
049400070823     C                   eval      VATNRS = VABNRS
049500060512     C                   eval      VATNSP = VABNSP
049600060512     C                   eval      VATLNP = VABLNP
049700111117     C                   eval      VATCMR = wCMR
049800090914     C                   eval      VATCNT = 1
049900060331     C*
050000060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
050100060331     C                   if        wVATNOT_A <> *blanks
050200060331     C                   eval      VATNOT = wVATNOT_A
050300060331     C                   eval      VATTRC = 'A'
050400090911     C                   write     EDIVAT00
050500060331     C                   add       1             §CTROKVT
050600060331     C                   endif
050700060331     C*
050800060331     C                   if        wVATNOT_B <> *blanks
050900060331     C                   eval      VATNOT = wVATNOT_B
051000060331     C                   eval      VATTRC = 'B'
051100090911     C                   write     EDIVAT00
051200060331     C                   add       1             §CTROKVT
051300060331     C                   endif
051400090623     C*
051500111118 xxx C* Considero il VAT solo a rottura del "chi sono"
051600111118     C***                if        wVATNOT_E <> *blanks AND
051700111118     C***                          wVATNOT_E <> sVATNOT_E
051800111118     C***                eval      VATNOT = wVATNOT_E
051900111118     C***                eval      VATTRC = 'E'
052000111118     C***                write     EDIVAT00
052100111118     C***                add       1             §CTROKVT
052200111118     C***                eval      sVATNOT_E = wVATNOT_E
052300111118     C***                add       1             VABNCL
052400111118     C***                endif
052500040119     C*
052600040119     C                   ENDSR
052700040119
052800040119
052900040119
053000040119     C*----------------------------------------------------*
053100101005     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
053200040119     C*----------------------------------------------------*
053300101005     C     PREVAT        BEGSR
053400040119     C*
053500101005     C* Compongo il nome del membro da dare al EDIVATWR
053600040119     C                   eval      parmbr = vlrhdl
053700040119     C                   movel     'M'           parmbr
053800040510     C                   eval      parccm = vlrksc
053900040119     C                   eval      paropz = '1'
054000040119     C* Effettuo la chiamata al CLLE preposto
054100101005     C                   call(e)   'TITVEVTC'
054200040119     C                   parm                    parccm
054300040119     C                   parm                    parmbr
054400040119     C                   parm                    paropz
054500040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
054600040119     C                   if        %error
054700040119     C                   movel     '1'           chkcall
054800040119     C                   else
054900040119     C                   movel     '0'           chkcall
055000040119     C                   endif
055100040119     C*
055200040119     C                   ENDSR
055300010601
055400010601
055500010601
055600010330     C*----------------------------------------------------*
055700030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
055800010330     C*----------------------------------------------------*
055900040119     C     INZVAR        BEGSR
056000010330     C*
056100030822     C* Inizializzo variabili di wrk
056200090916     C                   Z-ADD     *zeros        Num5_0            5 0
056300090916     C                   MOVEL     *blanks       wNOTE            70
056400060331     C                   MOVEL     *blanks       wVATNOT_A
056500060331     C                   MOVEL     *blanks       wVATNOT_B
056600090904     C                   MOVEL     *blanks       wVATNOT_E
056700111117     C                   MOVEL     *blanks       wVATNOT_E_1
056800111117     C                   MOVEL     *blanks       wVATNOT_E_2
056900111118     C                   MOVEL     *blanks       wVATNOT_E_3
057000111118     C                   MOVEL     *blanks       wVATNOT_E_4
057100090623     C*
057200111118 xxx C***                MOVEL     *blanks       sVATNOT_E        35
057300090220     C*
057400080617     C                   EVAL      wFlgCAS = *blanks
057500030822     C*
057600030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
057700090911     C                   CLEAR                   EDIVAB00
057800090911     C                   CLEAR                   EDIVAT00
057900030822     C                   CLEAR                   SkSplitCSV
058000090623     C                   EXSR      inzwrk
058100030822     C*
058200030822     C* Reimposto i valori di default
058300030822     C                   EXSR      DEFCAM
058400030822     C*
058500010330     C                   ENDSR
058600010330     C*----------------------------------------------------*
058700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
058800010330     C*----------------------------------------------------*
058900010330     C     DEFCAM        BEGSR
059000030822     C*
059100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
059200030715     C* e delimitatore testo.
059300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
059400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
059500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
059600030822     C*
059700030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
059800030715     C                   IF        %subst(vlrppt:1:1) = 'S'
059900030715     C                   SETON                                        50
060000030715     C                   ELSE
060100030715     C                   SETOFF                                       50
060200030715     C                   ENDIF
060300090421     C*
060400090421     C* D default SEMPRE porto franco / SEMPRE lnp = p.o. invio / SEMPRE nsp = rrnum
060500090421     C                   EVAL      VABCBO = '1'
060600090421     C                   EVAL      VABLNP = vlrpoi
060700090421     C                   EVAL      VABNSP = rrnum
060800030822     C*
060900030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
061000030822     C                   EVAL      posDaDft = 1
061100030822     C                   EVAL      posADft  = 0
061200030822     C                   EVAL      wGiroDft = 0
061300030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
061400030822     C                             posDaDft > 0
061500030822     C*
061600030822     C* Gestisco il 1° giro
061700030822     C                   IF        wGiroDft = 0
061800030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
061900030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
062000030822     C* Incremento il contatore dei "giri"
062100030822     C                   EVAL      wGiroDft = 1
062200030822     C                   ELSE
062300030822     C                   EVAL      posDaDft = posADft
062400030822     C                   ENDIF
062500030822     C* Eseguo lo scan x trovare la fine del campo corrente
062600030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
062700030822     C*
062800030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
062900030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
063000030822     C                   IF        posDaDft > 0 AND
063100030822     C                             posADft  > 0
063200030822     C* NCL
063300030822     C                   IF        %subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):1:3)
063600030822     C                             = 'NCL'
063700030822     C                   EVAL      PiStr=%trim(%subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064000030822     C                   EXSR      CHKNUM
064100030822     C                   IF        PiInt=*on
064200030822     C                   Z-ADD     PiVal         VABNCL
064300030822     C                   ENDIF
064400030822     C                   ENDIF
064500030822     C* CCM
064600030822     C                   IF        %subst(
064700030822     C                             %subst(vlrppt:posDaDft+1:
064800030822     C                             posADft-posDaDft-1):1:3)
064900030822     C                             = 'CCM'
065000030822     C                   EVAL      PiStr=%trim(%subst(
065100030822     C                             %subst(vlrppt:posDaDft+1:
065200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065300030822     C                   EXSR      CHKNUM
065400030822     C                   IF        PiInt=*on
065500030822     C                   Z-ADD     PiVal         VABCCM
065600030822     C                   ENDIF
065700030822     C                   ENDIF
065800030822     C* LNP
065900030822     C                   IF        %subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):1:3)
066200030822     C                             = 'LNP'
066300030822     C                   EVAL      PiStr=%trim(%subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066600030822     C                   EXSR      CHKNUM
066700030822     C                   IF        PiInt=*on
066800030822     C                   Z-ADD     PiVal         VABLNP
066900030822     C                   ENDIF
067000030822     C                   ENDIF
067100030822     C* NRS
067200030822     C                   IF        %subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):1:3)
067500030822     C                             = 'NRS'
067600030822     C                   EVAL      PiStr=%trim(%subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067900030822     C                   EXSR      CHKNUM
068000030822     C                   IF        PiInt=*on
068100030822     C                   Z-ADD     PiVal         VABNRS
068200030822     C                   ENDIF
068300030822     C                   ENDIF
068400030822     C* CTR
068500030822     C                   IF        %subst(
068600030822     C                             %subst(vlrppt:posDaDft+1:
068700030822     C                             posADft-posDaDft-1):1:3)
068800030822     C                             = 'CTR'
068900030822     C                   EVAL      PiStr=%trim(%subst(
069000030822     C                             %subst(vlrppt:posDaDft+1:
069100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069200030822     C                   EXSR      CHKNUM
069300030822     C                   IF        PiInt=*on
069400030822     C                   Z-ADD     PiVal         VABCTR
069500030822     C                   ENDIF
069600030822     C                   ENDIF
069700030822     C* PKB
069800030822     C                   IF        %subst(
069900030822     C                             %subst(vlrppt:posDaDft+1:
070000030822     C                             posADft-posDaDft-1):1:3)
070100030822     C                             = 'PKB'
070200030822     C                   EVAL      PiStr=%trim(%subst(
070300030822     C                             %subst(vlrppt:posDaDft+1:
070400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070500030822     C                   EXSR      CHKNUM
070600030822     C                   IF        PiNum=*on
070700030822     C                   Z-ADD     PiVal         VABPKB
070800030822     C                   ENDIF
070900030822     C                   ENDIF
071000030822     C* VLB
071100030822     C                   IF        %subst(
071200030822     C                             %subst(vlrppt:posDaDft+1:
071300030822     C                             posADft-posDaDft-1):1:3)
071400030822     C                             = 'VLB'
071500030822     C                   EVAL      PiStr=%trim(%subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071800030822     C                   EXSR      CHKNUM
071900030822     C                   IF        PiNum=*on
072000030822     C                   Z-ADD     PiVal         VABVLB
072100030822     C                   ENDIF
072200030822     C                   ENDIF
072300030822     C* QFT
072400030822     C                   IF        %subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):1:3)
072700030822     C                             = 'QFT'
072800030822     C                   EVAL      PiStr=%trim(%subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073100030822     C                   EXSR      CHKNUM
073200030822     C                   IF        PiNum=*on
073300030822     C                   Z-ADD     PiVal         VABQFT
073400030822     C                   ENDIF
073500030822     C                   ENDIF
073600030822     C* CBO
073700030822     C                   IF        %subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):1:3)
074000030822     C                             = 'CBO'
074100030822     C                   EVAL      VABCBO=%trim(%subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074400030822     C                   ENDIF
074500030822     C* TSP
074600030822     C                   IF        %subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):1:3)
074900030822     C                             = 'TSP'
075000030822     C                   EVAL      VABTSP=%trim(%subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075300030822     C                   ENDIF
075400030822     C* VAS
075500030822     C                   IF        %subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):1:3)
075800030822     C                             = 'VAS'
075900030822     C                   EVAL      VABVAS=%trim(%subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076200030822     C                   ENDIF
076300030822     C* VCA
076400030822     C                   IF        %subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):1:3)
076700030822     C                             = 'VCA'
076800030822     C                   EVAL      VABVCA=%trim(%subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077100030822     C                   ENDIF
077200070925     C* TIC
077300070925     C                   IF        %subst(
077400070925     C                             %subst(vlrppt:posDaDft+1:
077500070925     C                             posADft-posDaDft-1):1:3)
077600070925     C                             = 'TIC'
077700070925     C                   EVAL      VABTIC=%trim(%subst(
077800070925     C                             %subst(vlrppt:posDaDft+1:
077900070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078000070925     C                   ENDIF
078100030822     C* GCA
078200030822     C                   IF        %subst(
078300030822     C                             %subst(vlrppt:posDaDft+1:
078400030822     C                             posADft-posDaDft-1):1:3)
078500030822     C                             = 'GCA'
078600030822     C                   EVAL      VABGCA=%trim(%subst(
078700030822     C                             %subst(vlrppt:posDaDft+1:
078800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078900030822     C                   ENDIF
079000030822     C* CTM
079100030822     C                   IF        %subst(
079200030822     C                             %subst(vlrppt:posDaDft+1:
079300030822     C                             posADft-posDaDft-1):1:3)
079400030822     C                             = 'CTM'
079500030822     C                   EVAL      VABCTM=%trim(%subst(
079600030822     C                             %subst(vlrppt:posDaDft+1:
079700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079800030822     C                   ENDIF
079900030822     C* FFD
080000030822     C                   IF        %subst(
080100030822     C                             %subst(vlrppt:posDaDft+1:
080200030822     C                             posADft-posDaDft-1):1:3)
080300030822     C                             = 'FFD'
080400030822     C                   EVAL      VABFFD=%trim(%subst(
080500030822     C                             %subst(vlrppt:posDaDft+1:
080600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080700030822     C                   ENDIF
080800030822     C* VAD
080900030822     C                   IF        %subst(
081000030822     C                             %subst(vlrppt:posDaDft+1:
081100030822     C                             posADft-posDaDft-1):1:3)
081200030822     C                             = 'VAD'
081300030822     C                   EVAL      VABVAD=%trim(%subst(
081400030822     C                             %subst(vlrppt:posDaDft+1:
081500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081600030822     C                   ENDIF
081700030822     C* GMA
081800030822     C                   IF        %subst(
081900030822     C                             %subst(vlrppt:posDaDft+1:
082000030822     C                             posADft-posDaDft-1):1:3)
082100030822     C                             = 'GMA'
082200030822     C                   EVAL      VABGMA=%trim(%subst(
082300030822     C                             %subst(vlrppt:posDaDft+1:
082400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082500030822     C                   ENDIF
082600030822     C* GGA
082700030822     C                   IF        %subst(
082800030822     C                             %subst(vlrppt:posDaDft+1:
082900030822     C                             posADft-posDaDft-1):1:3)
083000030822     C                             = 'GGA'
083100030822     C                   EVAL      VABGGA=%trim(%subst(
083200030822     C                             %subst(vlrppt:posDaDft+1:
083300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083400030822     C                   ENDIF
083500030822     C* GVA
083600030822     C                   IF        %subst(
083700030822     C                             %subst(vlrppt:posDaDft+1:
083800030822     C                             posADft-posDaDft-1):1:3)
083900030822     C                             = 'GVA'
084000030822     C                   EVAL      VABGVA=%trim(%subst(
084100030822     C                             %subst(vlrppt:posDaDft+1:
084200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084300030822     C                   ENDIF
084400030822     C* TC1
084500030822     C                   IF        %subst(
084600030822     C                             %subst(vlrppt:posDaDft+1:
084700030822     C                             posADft-posDaDft-1):1:3)
084800030822     C                             = 'TC1'
084900030822     C                   EVAL      VABTC1=%trim(%subst(
085000030822     C                             %subst(vlrppt:posDaDft+1:
085100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085200030822     C                   ENDIF
085300030822     C* TC2
085400030822     C                   IF        %subst(
085500030822     C                             %subst(vlrppt:posDaDft+1:
085600030822     C                             posADft-posDaDft-1):1:3)
085700030822     C                             = 'TC2'
085800030822     C                   EVAL      VABTC2=%trim(%subst(
085900030822     C                             %subst(vlrppt:posDaDft+1:
086000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086100030822     C                   ENDIF
086200040714     C* VATTRC
086300040714     C                   IF        %subst(
086400040714     C                             %subst(vlrppt:posDaDft+1:
086500040714     C                             posADft-posDaDft-1):1:3)
086600040714     C                             = 'TRC'
086700040714     C                   EVAL      VATTRC=%trim(%subst(
086800040714     C                             %subst(vlrppt:posDaDft+1:
086900040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
087000040714     C                   ENDIF
087100030822     C* ...
087200030822     C                   ENDIF
087300030822     C                   ENDDO
087400020204     C*
087500010330     C                   ENDSR
087600010607     C*----------------------------------------------------*
087700090911     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
087800010607     C*----------------------------------------------------*
087900090623     C     IMPVAB        BEGSR
088000010607     C*
088100010607     C                   SETOFF                                       3132
088200030515     C*
088300010607     C                   MOVEL     datcor        VABAAS
088400010607     C                   MOVE      datcor        VABMGS
088500010607     C*
088600030715     C**********
088700030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
088800030715     C**********
088900030715     C                   IF        wGiro = *zeros
089000080716     C*
089100080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
089200080716     C                   EVAL      depvinDTA = vinDTA
089300040113     C*
089400040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
089500110923     C                   IF        header <> *blanks
089600110923     C                   EVAL      vindta = header
089700070823     C     ';':CharCSV   XLATE     vindta        vindta
089800040510     C                   ENDIF
089900080626     C*
090000031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
090100031201     C     minu:maiu     XLATE     vindta        vindta
090200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
090300030715     C                   DOW       posDa <= %len(%trim(vindta))
090400030715     C*
090500030715     C* Gestisco il 1° campo
090600030715     C                   IF        i = 1
090700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
090800030715     C                   EVAL      posDa = 1
090900030715     C* Eseguo lo scan x trovare la fine del primo campo
091000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091100030715     C* A questo punto "estraggo" il campo corrente
091200110923     C                   EVAL      SkSplitFLD(i) = %trim(prefix) +
091300110923     C                                         %subst(vindta:posDa:(posA-posDa))
091400030715     C* X i campi successivi al 1°
091500030715     C                   ELSE
091600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
091700030715     C                   EVAL      posDa = posA + 1
091800030715     C* Eseguo lo scan x trovare la fine del campo corrente
091900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092000030715     C* Gestisco l'ultimo campo
092100030715     C                   IF        posA = *zeros
092200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
092300030715     C* A questo punto "estraggo" il campo corrente
092400110923     C                   EVAL      SkSplitFLD(i) = %trim(prefix) +
092500110923     C                                         %subst(vindta:posDa:(posA-posDa))
092600030715     C                   LEAVE
092700030715     C                   ELSE
092800030715     C* A questo punto "estraggo" il campo corrente
092900110923     C                   EVAL      SkSplitFLD(i) = %trim(prefix) +
093000110923     C                                         %subst(vindta:posDa:(posA-posDa))
093100030715     C                   ENDIF
093200030715     C                   ENDIF
093300030715     C* Incremento il contatore d campo
093400030715     C                   EVAL      i = i +1
093500030715     C                   ENDDO
093600030715     C*
093700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
093800030715     C* delimitazione testo
093900030715     C                   EVAL      i = 1
094000030715     C                   DOW       i < %elem(SkSplitFLD)
094100030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
094200030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
094300030715     C                   EVAL      i = i + 1
094400030715     C                   ENDDO
094500030715     C                   EVAL      wGiro = 1
094600080716     C* Ripristino la 1° riga originale
094700080716     C                   EVAL      vinDTA = depvinDTA
094800030715     C                   ELSE
094900030715     C                   EVAL      wGiro = 2
095000031204     C**********
095100031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
095200031204     C* d separatore campo
095300031204     C**********
095400031204     C                   EVAL      vindta = %trim(vindta)
095500031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
095600031204     C                   EVAL      vindta = %subst(vindta:2)
095700031204     C                   ENDDO
095800031204     C*
095900031204     C                   Z-ADD     *zeros        lunghInput        4 0
096000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
096100031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
096200031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
096300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
096400031204     C                   ENDDO
096500030715     C**********
096600030715     C* Effettuo lo split del campo dati d input
096700030715     C**********
096800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
096900030715     C                   DOW       posDa <= %len(%trim(vindta))
097000030715     C*
097100030715     C* Gestisco il 1° campo
097200030715     C                   IF        i = 1
097300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
097400030715     C                   EVAL      posDa = 1
097500030715     C* Eseguo lo scan x trovare la fine del primo campo
097600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
097700030715     C* A questo punto "estraggo" il campo corrente
097800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
097900030715     C                                                    (posA-posDa))
098000030715     C* X i campi successivi al 1°
098100030715     C                   ELSE
098200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
098300030715     C                   EVAL      posDa = posA + 1
098400031204     C* Verifico che nn vi sia il campo nn valorizzato
098500031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
098600031204     C* Se campo nn valorizzato skippo al prossimo
098700031204     C                   EVAL      SkSplitCSV(i) = *blanks
098800031204     C                   EVAL      posA  = posA + 1
098900031204     C                   ELSE
099000030715     C* Eseguo lo scan x trovare la fine del campo corrente
099100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
099200030715     C* Gestisco l'ultimo campo
099300030715     C                   IF        posA = *zeros
099400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
099500030715     C* A questo punto "estraggo" il campo corrente
099600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
099700030715     C                                                    (posA-posDa))
099800030715     C                   LEAVE
099900030715     C                   ELSE
100000030715     C* A questo punto "estraggo" il campo corrente
100100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
100200030715     C                                                    (posA-posDa))
100300030715     C                   ENDIF
100400031204     C                   ENDIF
100500030715     C                   ENDIF
100600030715     C* Incremento il contatore d campo
100700030715     C                   EVAL      i = i +1
100800030715     C                   ENDDO
100900030715     C*
101000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
101100030715     C* delimitazione testo
101200030715     C                   EVAL      i = 1
101300030715     C                   DOW       i < %elem(SkSplitCSV)
101400030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
101500030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
101600030715     C                   EVAL      i = i + 1
101700030715     C                   ENDDO
101800030715     C*
101900030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
102000030715     C                   EVAL      i = 1
102100030715     C                   DOW       i < %elem(SkSplitFLD)
102200030715     C*
102300030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
102400030715     C                   IF        SkSplitFLD(i) <> *blanks AND
102500090623     C                             SkSplitCSV(i) <> *blanks
102600090623     C*
102700090623     C                   exsr      CARVAL
102800040510     C*
102900030715     C                   ENDIF
103000030715     C                   EVAL      i = i + 1
103100030715     C                   ENDDO
103200070925     C*
103300071029     C***  ===>  Gestione campi "particolari" fuori ciclo
103400080125     C*
103500111117     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
103600111117     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
103700070925     C*
103800071029     C***  <===  -----------------------------------------
103900070925     C*
104000070925     C* Considerazioni finali su CBO/CAS
104100080125     C                   IF        wFlgCAS = '1'
104200070925     C                   IF        VABCBO = '1'
104300070925     C                   EVAL      VABCBO = '4'
104400070925     C                   ENDIF
104500070925     C                   IF        VABCBO = '2'
104600070925     C                   EVAL      VABCBO = '6'
104700070925     C                   ENDIF
104800070925     C                   ENDIF
104900070925     C***  <===  ----------------------------
105000040510     C*
105100040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
105200040510     C                   IF        VABNSP = *zeros
105300040510     C                   SETON                                        31
105400040510     C                   ENDIF
105500040715     C*
105600040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
105700040715     C                   IF        VABLNP = *zeros
105800040715     C                   SETON                                        31
105900040715     C                   ENDIF
106000040510     C*
106100040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
106200040510     C                   IF        VABRMN = *zeros
106300040510     C                   Z-ADD     VABNSP        VABRMN
106400040510     C                   ENDIF
106500040510     C*
106600040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
106700040510     C                   IF        VABRMA = *blanks
106800040714     C                   MOVEL     VABRMN        VABRMA
106900040510     C                   ENDIF
107000040128     C*
107100040510     C* Se provincia nn valorizzata la reperisco
107200040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
107300040510     C                   IF        VABPRD = *blanks
107400040128     C                   CLEAR                   TISI95DS
107500040128     C                   EVAL      I95TCN = '3'
107600040128     C                   Z-ADD     datcor        I95DAT
107700040128     C                   EVAL      I95NAR = VABNZD
107800040128     C                   EVAL      I95CAP = VABCAD
107900040128     C                   EVAL      I95LOC = VABLOD
108000040128     C                   CALL      'TISI95R'
108100040128     C                   PARM                    TISI95DS
108200040128     C                   EVAL      VABPRD = O95PRV
108300040510     C                   ENDIF
108400020204     C*
108500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
108600020204     C                   EXSR      CHKIMPDIV
108700030715     C                   ENDIF
108800020204     C*
108900010607     C                   ENDSR
109000010607     C*----------------------------------------------------*
109100090623
109200090623
109300090623
109400090623     C*----------------------------------------------------*
109500101005     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
109600090623     C*----------------------------------------------------*
109700101005     C     IMPVAT        BEGSR
109800090623     C*
109900090623     C**********
110000090623     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
110100090623     C**********
110200090623     C                   IF        wGiro = *zeros
110300090623     C*
110400090623     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
110500090623     C                   EVAL      depvinDTA = vinDTA
110600090623     C*
110700090623     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
110800110923     C                   IF        parcel <> *blanks
110900110923     C                   EVAL      vindta = parcel
111000090623     C     ';':CharCSV   XLATE     vindta        vindta
111100090623     C                   ENDIF
111200090623     C*
111300090623     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
111400090623     C     minu:maiu     XLATE     vindta        vindta
111500090623     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
111600090623     C                   DOW       posDa <= %len(%trim(vindta))
111700090623     C*
111800090623     C* Gestisco il 1° campo
111900090623     C                   IF        i = 1
112000090623     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
112100090623     C                   EVAL      posDa = 1
112200090623     C* Eseguo lo scan x trovare la fine del primo campo
112300090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
112400090623     C* A questo punto "estraggo" il campo corrente
112500110923     C                   EVAL      SkSplitFLD(i) = %trim(prefix) +
112600110923     C                                         %subst(vindta:posDa:(posA-posDa))
112700090623     C* X i campi successivi al 1°
112800090623     C                   ELSE
112900090623     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
113000090623     C                   EVAL      posDa = posA + 1
113100090623     C* Eseguo lo scan x trovare la fine del campo corrente
113200090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
113300090623     C* Gestisco l'ultimo campo
113400090623     C                   IF        posA = *zeros
113500090623     C                   EVAL      posA = %len(%trim(vindta)) + 1
113600090623     C* A questo punto "estraggo" il campo corrente
113700110923     C                   EVAL      SkSplitFLD(i) = %trim(prefix) +
113800110923     C                                         %subst(vindta:posDa:(posA-posDa))
113900090623     C                   LEAVE
114000090623     C                   ELSE
114100090623     C* A questo punto "estraggo" il campo corrente
114200110923     C                   EVAL      SkSplitFLD(i) = %trim(prefix) +
114300110923     C                                         %subst(vindta:posDa:(posA-posDa))
114400090623     C                   ENDIF
114500090623     C                   ENDIF
114600090623     C* Incremento il contatore d campo
114700090623     C                   EVAL      i = i +1
114800090623     C                   ENDDO
114900090623     C*
115000090623     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
115100090623     C* delimitazione testo
115200090623     C                   EVAL      i = 1
115300090623     C                   DOW       i < %elem(SkSplitFLD)
115400090623     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
115500090623     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
115600090623     C                   EVAL      i = i + 1
115700090623     C                   ENDDO
115800090623     C                   EVAL      wGiro = 1
115900090623     C* Ripristino la 1° riga originale
116000090623     C                   EVAL      vinDTA = depvinDTA
116100090623     C                   ELSE
116200090623     C                   EVAL      wGiro = 2
116300090623     C**********
116400090623     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
116500090623     C* d separatore campo
116600090623     C**********
116700090623     C                   EVAL      vindta = %trim(vindta)
116800090623     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
116900090623     C                   EVAL      vindta = %subst(vindta:2)
117000090623     C                   ENDDO
117100090623     C*
117200090623     C                   Z-ADD     *zeros        lunghInput        4 0
117300090623     C                   EVAL      lunghInput = %len(%trim(vindta))
117400090623     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
117500090623     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
117600090623     C                   EVAL      lunghInput = %len(%trim(vindta))
117700090623     C                   ENDDO
117800090623     C**********
117900090623     C* Effettuo lo split del campo dati d input
118000090623     C**********
118100090623     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
118200090623     C                   DOW       posDa <= %len(%trim(vindta))
118300090623     C*
118400090623     C* Gestisco il 1° campo
118500090623     C                   IF        i = 1
118600090623     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
118700090623     C                   EVAL      posDa = 1
118800090623     C* Eseguo lo scan x trovare la fine del primo campo
118900090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
119000090623     C* A questo punto "estraggo" il campo corrente
119100090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
119200090623     C                                                    (posA-posDa))
119300090623     C* X i campi successivi al 1°
119400090623     C                   ELSE
119500090623     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
119600090623     C                   EVAL      posDa = posA + 1
119700090623     C* Verifico che nn vi sia il campo nn valorizzato
119800090623     C                   IF        %subst(vindta:posDa:1) = CharCSV
119900090623     C* Se campo nn valorizzato skippo al prossimo
120000090623     C                   EVAL      SkSplitCSV(i) = *blanks
120100090623     C                   EVAL      posA  = posA + 1
120200090623     C                   ELSE
120300090623     C* Eseguo lo scan x trovare la fine del campo corrente
120400090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
120500090623     C* Gestisco l'ultimo campo
120600090623     C                   IF        posA = *zeros
120700090623     C                   EVAL      posA = %len(%trim(vindta)) + 1
120800090623     C* A questo punto "estraggo" il campo corrente
120900090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
121000090623     C                                                    (posA-posDa))
121100090623     C                   LEAVE
121200090623     C                   ELSE
121300090623     C* A questo punto "estraggo" il campo corrente
121400090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
121500090623     C                                                    (posA-posDa))
121600090623     C                   ENDIF
121700090623     C                   ENDIF
121800090623     C                   ENDIF
121900090623     C* Incremento il contatore d campo
122000090623     C                   EVAL      i = i +1
122100090623     C                   ENDDO
122200090623     C*
122300090623     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
122400090623     C* delimitazione testo
122500090623     C                   EVAL      i = 1
122600090623     C                   DOW       i < %elem(SkSplitCSV)
122700090623     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
122800090623     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
122900090623     C                   EVAL      i = i + 1
123000090623     C                   ENDDO
123100090623     C*
123200090623     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
123300090623     C                   EVAL      i = 1
123400090623     C                   DOW       i < %elem(SkSplitFLD)
123500090623     C*
123600090623     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
123700090623     C                   IF        SkSplitFLD(i) <> *blanks AND
123800090623     C                             SkSplitCSV(i) <> *blanks
123900090623     C*
124000090623     C                   exsr      CARVAL
124100090623     C*
124200090623     C                   ENDIF
124300090623     C                   EVAL      i = i + 1
124400090623     C                   ENDDO
124500090623     C*
124600090623     C***  ===>  Gestione campi "particolari" fuori ciclo
124700090623     C*
124800090623     C*
124900090623     C***  <===  -----------------------------------------
125000090623     C*
125100090623     C                   ENDIF
125200090623     C*
125300090623     C                   ENDSR
125400090623     C*----------------------------------------------------*
125500090623
125600090623
125700090623     C*----------------------------------------------------*
125800090623     C*  ROUTINE DI VALORIZZAZIONE CAMPI
125900090623     C*----------------------------------------------------*
126000090623     C     CARVAL        BEGSR
126100090623     C*
126200090623     C*** ATB
126300090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
126400090623     C                   EVAL      VABATB = SkSplitCSV(i)
126500090623     C                   ENDIF
126600090623     C*** RSD
126700111117     C                   IF        %trim(SkSplitFLD(i)) = 'HDR_RNAME1'
126800090623     C                   EVAL      VABRSD = SkSplitCSV(i)
126900090623     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
127000090623     C     '@':'A'       XLATE     VABRSD        VABRSD
127100090623     C* ==
127200090623     C                   ENDIF
127300090623     C*** RD2
127400111117     C                   IF        %trim(SkSplitFLD(i)) = 'HDR_RNAME2'
127500090623     C                   EVAL      VABRD2 = SkSplitCSV(i)
127600090623     C                   ENDIF
127700111117     C*** IND 1
127800111117     C                   IF        %trim(SkSplitFLD(i)) = 'HDR_RSTREET'
127900090623     C                   EVAL      VABIND = SkSplitCSV(i)
128000090623     C                   ENDIF
128100111117     C*** IND 2
128200111117     C                   IF        %trim(SkSplitFLD(i)) = 'HDR_RHOUSENO'
128300111118     C                   EVAL      VABIND = %trim(VABIND) + ' ' + SkSplitCSV(i)
128400111117     C                   ENDIF
128500111117     C*** CAD
128600111117     C                   IF        %trim(SkSplitFLD(i)) = 'HDR_RPOSTAL'
128700111117     C                   EVAL      VABCAD = SkSplitCSV(i)
128800111117     C                   ENDIF
128900090623     C*** LOD
129000111117     C                   IF        %trim(SkSplitFLD(i)) = 'HDR_RCITY'
129100090623     C                   EVAL      VABLOD = SkSplitCSV(i)
129200090623     C                   ENDIF
129300111117     C*** PRD
129400090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
129500090623     C                   EVAL      VABPRD = SkSplitCSV(i)
129600090623     C                   ENDIF
129700090623     C*** NZD
129800090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
129900090623     C                   EVAL      VABNZD = SkSplitCSV(i)
130000090623     C                   ENDIF
130100090623     C*** RMA
130200111117     C                   IF        %trim(SkSplitFLD(i)) = 'HDR_MPSCREF1'
130300111117     C                   EVAL      VABRMA = %subst(SkSplitCSV(i):4:14)
130400090623     C                   ENDIF
130500111117     C*** NOT
130600111117     C                   IF        %trim(SkSplitFLD(i)) = 'HDR_SCOMMENT'
130700111117     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' + SkSplitCSV(i)
130800090623     C                   ENDIF
130900111117     C*** NT2
131000111117     C                   IF        %trim(SkSplitFLD(i)) = 'HDR_RCOMMENT'
131100111117     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' + SkSplitCSV(i)
131200090623     C                   ENDIF
131300090623     C*** VCA
131400090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
131500090623     C                   EVAL      VABVCA = SkSplitCSV(i)
131600090623     C                   ENDIF
131700090623     C*** VAS
131800090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
131900090623     C                   EVAL      VABVAS = SkSplitCSV(i)
132000090623     C                   ENDIF
132100090623     C*** FFD
132200090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
132300090623     C                   EVAL      VABFFD = SkSplitCSV(i)
132400090623     C                   ENDIF
132500090623     C*** GC1
132600090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
132700090623     C                   EVAL      VABGC1 = SkSplitCSV(i)
132800090623     C                   ENDIF
132900090623     C*** GC2
133000090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
133100090623     C                   EVAL      VABGC2 = SkSplitCSV(i)
133200090623     C                   ENDIF
133300090623     C*** TSP
133400090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
133500090623     C                   EVAL      VABTSP = SkSplitCSV(i)
133600090623     C                   ENDIF
133700090623     C*** CBO
133800090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
133900090623     C                   EVAL      VABCBO = SkSplitCSV(i)
134000090623     C                   ENDIF
134100090623     C*** NAS
134200090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
134300090623     C                   EVAL      VABNAS = SkSplitCSV(i)
134400090623     C                   ENDIF
134500090623     C*** TIC
134600090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
134700090623     C                   EVAL      VABTIC = SkSplitCSV(i)
134800090623     C                   ENDIF
134900090623     C*** GCA
135000090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
135100090623     C                   EVAL      VABGCA = SkSplitCSV(i)
135200090623     C                   ENDIF
135300090623     C*** XCO
135400090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
135500090623     C                   EVAL      VABXCO = SkSplitCSV(i)
135600090623     C                   ENDIF
135700090623     C*** CTM
135800090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
135900090623     C                   EVAL      VABCTM = SkSplitCSV(i)
136000090623     C                   ENDIF
136100090623     C*** TCR
136200090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
136300090623     C                   EVAL      VABTCR = SkSplitCSV(i)
136400090623     C                   ENDIF
136500090623     C*** CTS
136600090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
136700090623     C                   EVAL      VABCTS = SkSplitCSV(i)
136800090623     C                   ENDIF
136900090623     C*** FTM
137000090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
137100090623     C                   EVAL      VABFTM = SkSplitCSV(i)
137200090623     C                   ENDIF
137300090623     C*** VAD
137400090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
137500090623     C                   EVAL      VABVAD = SkSplitCSV(i)
137600090623     C                   ENDIF
137700090623     C*** GMA
137800090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
137900090623     C                   EVAL      VABGMA = SkSplitCSV(i)
138000090623     C                   ENDIF
138100090623     C*** GGA
138200090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
138300090623     C                   EVAL      VABGGA = SkSplitCSV(i)
138400090623     C                   ENDIF
138500090623     C*** GVA
138600090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
138700090623     C                   EVAL      VABGVA = SkSplitCSV(i)
138800090623     C                   ENDIF
138900090623     C*** TC1
139000090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
139100090623     C                   EVAL      VABTC1 = SkSplitCSV(i)
139200090623     C                   ENDIF
139300090623     C*** TC2
139400090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
139500090623     C                   EVAL      VABTC2 = SkSplitCSV(i)
139600090623     C                   ENDIF
139700090623     C*** SCL
139800090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
139900090623     C                   EVAL      VABSCL = SkSplitCSV(i)
140000090623     C                   ENDIF
140100090623     C*** RMO
140200090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
140300090623     C                   EVAL      VABRMO = SkSplitCSV(i)
140400090623     C                   ENDIF
140500090623     C*** NMO
140600090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
140700090623     C                   EVAL      VABNMO = SkSplitCSV(i)
140800090623     C                   ENDIF
140900090623     C*
141000090623     C* Reperisco quindi i campi numerici...
141100111117     C*** NSP
141200111117     C                   IF        %trim(SkSplitFLD(i)) = 'HDR_MPSID'
141300111117     C                   EVAL      PiStr=%subst(SkSplitCSV(i):4:14)
141400090623     C                   EXSR      CHKNUM
141500090623     C                   IF        PiInt=*on
141600090623     C                   Z-ADD     PiVal         VABNSP
141700090623     C                   ELSE
141800090623     C                   SETON                                        31
141900090623     C                   EVAL      VABNSP = *zeros
142000090623     C                   EVAL      vinmsg = %trimr(vinmsg)
142100090623     C                             + ' ' + 'VABNSP'
142200090623     C                   ENDIF
142300090623     C                   ENDIF
142400090623     C*** RMN
142500111117     C                   IF        %trim(SkSplitFLD(i)) = 'HDR_MPSID'
142600111117     C                   EVAL      PiStr=%subst(SkSplitCSV(i):4:14)
142700090623     C                   EXSR      CHKNUM
142800090623     C                   IF        PiInt=*on
142900090623     C                   Z-ADD     PiVal         VABRMN
143000090623     C                   ELSE
143100090623     C                   SETON                                        32
143200090623     C                   EVAL      VABRMN = *zeros
143300090623     C                   EVAL      vinmsg = %trimr(vinmsg)
143400090623     C                             + ' ' + 'VABRMN'
143500090623     C                   ENDIF
143600090623     C                   ENDIF
143700090623     C*** CAD
143800090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
143900090623     C                   EVAL      PiStr=SkSplitCSV(i)
144000090623     C                   EXSR      CHKNUM
144100090623     C                   IF        PiInt=*on
144200090623     C                   Z-ADD     PiVal         Num5_0
144300090623     C                   MOVEL(p)  Num5_0        VABCAD
144400090623     C                   ELSE
144500090623     C                   EVAL      VABCAD = SkSplitCSV(i)
144600090623     C                   SETON                                        32
144700090623     C                   EVAL      VABCAD = *zeros
144800090623     C                   EVAL      vinmsg = %trimr(vinmsg)
144900090623     C                             + ' ' + 'VABCAD'
145000090623     C                   ENDIF
145100090623     C                   ENDIF
145200090623     C*** NCL
145300111117     C                   IF        %trim(SkSplitFLD(i)) = 'HDR_MPSCOUNT'
145400090623     C                   EVAL      PiStr=SkSplitCSV(i)
145500090623     C                   EXSR      CHKNUM
145600090623     C                   IF        PiInt=*on
145700090623     C                   Z-ADD     PiVal         VABNCL
145800090623     C                   ELSE
145900090623     C                   SETON                                        32
146000090623     C                   EVAL      VABNCL = *zeros
146100090623     C                   EVAL      vinmsg = %trimr(vinmsg)
146200090623     C                             + ' ' + 'VABNCL'
146300090623     C                   ENDIF
146400090623     C                   ENDIF
146500090623     C*** PKB
146600111117     C                   IF        %trim(SkSplitFLD(i)) = 'HDR_MPSWEIGHT'
146700090623     C                   EVAL      PiStr=SkSplitCSV(i)
146800090623     C                   EXSR      CHKNUM
146900090623     C                   IF        PiNum=*on
147000090623     C                   Z-ADD     PiVal         VABPKB
147100090623     C                   ELSE
147200090623     C                   SETON                                        32
147300090623     C                   EVAL      VABPKB = *zeros
147400090623     C                   EVAL      vinmsg = %trimr(vinmsg)
147500090623     C                             + ' ' + 'VABPKB'
147600090623     C                   ENDIF
147700090623     C                   ENDIF
147800090623     C*** CAS
147900090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
148000090623     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
148100090623     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
148200090623     C                             %trim(SkSplitCSV(i)) <> '0'      AND
148300090623     C                             %trim(SkSplitCSV(i)) <> '0,00'
148400090623     C                   EVAL      wFlgCAS = '1'
148500090623     C                   ENDIF
148600090623     C                   EVAL      PiStr=SkSplitCSV(i)
148700090623     C                   EXSR      CHKNUM
148800090623     C                   IF        PiNum=*on
148900090623     C                   Z-ADD     PiVal         VABCAS
149000090623     C                   ELSE
149100090623     C                   SETON                                        32
149200090623     C                   EVAL      VABCAS = *zeros
149300090623     C                   EVAL      vinmsg = %trimr(vinmsg)
149400090623     C                             + ' ' + 'VABCAS'
149500090623     C                   ENDIF
149600090623     C                   ENDIF
149700090623     C*** CCM
149800090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
149900090623     C                   EVAL      PiStr=SkSplitCSV(i)
150000090623     C                   EXSR      CHKNUM
150100090623     C                   IF        PiInt=*on      AND
150200090623     C                             PiVal<=9999999 AND
150300090623     C                             PiVal>*zeros
150400090623     C                   Z-ADD     PiVal         VABCCM
150500090623     C                   ELSE
150600090623     C                   SETON                                        32
150700090623     C                   EVAL      VABCCM = *zeros
150800090623     C                   EVAL      vinmsg = %trimr(vinmsg)
150900090623     C                             + ' ' + 'VABCCM'
151000090623     C                   ENDIF
151100090623     C                   ENDIF
151200090623     C*** LNP
151300090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
151400090623     C                   EVAL      PiStr=SkSplitCSV(i)
151500090623     C                   EXSR      CHKNUM
151600090623     C                   IF        PiInt=*on    AND
151700090623     C                             PiVal<=999   AND
151800090623     C                             PiVal>*zeros
151900090623     C                   Z-ADD     PiVal         VABLNP
152000090623     C                   ELSE
152100090623     C                   SETON                                        32
152200090623     C                   EVAL      VABLNP = *zeros
152300090623     C                   EVAL      vinmsg = %trimr(vinmsg)
152400090623     C                             + ' ' + 'VABLNP'
152500090623     C                   ENDIF
152600090623     C                   ENDIF
152700090623     C*** NRS
152800090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
152900090623     C                   EVAL      PiStr=SkSplitCSV(i)
153000090623     C                   EXSR      CHKNUM
153100090623     C                   IF        PiInt=*on AND
153200090623     C                             PiVal<=99
153300090623     C                   Z-ADD     PiVal         VABNRS
153400090623     C                   ELSE
153500090623     C                   SETON                                        32
153600090623     C                   EVAL      vinmsg = %trimr(vinmsg)
153700090623     C                             + ' ' + 'VABNRS'
153800090623     C                   ENDIF
153900090623     C                   ENDIF
154000090623     C*** LNA
154100090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
154200090623     C                   EVAL      PiStr=SkSplitCSV(i)
154300090623     C                   EXSR      CHKNUM
154400090623     C                   IF        PiInt=*on    AND
154500090623     C                             PiVal<=999   AND
154600090623     C                             PiVal>*zeros
154700090623     C                   Z-ADD     PiVal         VABLNA
154800090623     C                   ELSE
154900090623     C                   SETON                                        32
155000090623     C                   EVAL      VABLNA = *zeros
155100090623     C                   EVAL      vinmsg = %trimr(vinmsg)
155200090623     C                             + ' ' + 'VABLNA'
155300090623     C                   ENDIF
155400090623     C                   ENDIF
155500090623     C*** CTR
155600090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
155700090623     C                   EVAL      PiStr=SkSplitCSV(i)
155800090623     C                   EXSR      CHKNUM
155900090623     C                   IF        PiInt=*on  AND
156000090623     C                             PiVal<=999
156100090623     C                   Z-ADD     PiVal         VABCTR
156200090623     C                   ELSE
156300090623     C                   SETON                                        32
156400090623     C                   EVAL      vinmsg = %trimr(vinmsg)
156500090623     C                             + ' ' + 'VABCTR'
156600090623     C                   ENDIF
156700090623     C                   ENDIF
156800090623     C*** IAS
156900090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
157000090623     C                   EVAL      PiStr=SkSplitCSV(i)
157100090623     C                   EXSR      CHKNUM
157200090623     C                   IF        PiNum=*on
157300090623     C                   Z-ADD     PiVal         VABIAS
157400090623     C                   ELSE
157500090623     C                   SETON                                        32
157600090623     C                   EVAL      vinmsg = %trimr(vinmsg)
157700090623     C                             + ' ' + 'VABIAS'
157800090623     C                   ENDIF
157900090623     C                   ENDIF
158000090623     C*** VLB
158100111117     C                   IF        %trim(SkSplitFLD(i)) = 'HDR_MPSVOLUME'
158200090623     C                   EVAL      PiStr=SkSplitCSV(i)
158300090623     C                   EXSR      CHKNUM
158400090623     C                   IF        PiNum=*on
158500111117     C                   EVAL      PiVal = PiVal / 1000000                      * da cm3 a m3
158600090623     C                   Z-ADD     PiVal         VABVLB
158700090623     C                   ELSE
158800090623     C                   SETON                                        32
158900090623     C                   EVAL      vinmsg = %trimr(vinmsg)
159000090623     C                             + ' ' + 'VABVLB'
159100090623     C                   ENDIF
159200090623     C                   ENDIF
159300090623     C*** QFT
159400090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
159500090623     C                   EVAL      PiStr=SkSplitCSV(i)
159600090623     C                   EXSR      CHKNUM
159700090623     C                   IF        PiNum=*on
159800090623     C                   Z-ADD     PiVal         VABQFT
159900090623     C                   ELSE
160000090623     C                   SETON                                        32
160100090623     C                   EVAL      vinmsg = %trimr(vinmsg)
160200090623     C                             + ' ' + 'VABQFT'
160300090623     C                   ENDIF
160400090623     C                   ENDIF
160500090623     C*** NCD
160600090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
160700090623     C                   EVAL      PiStr=SkSplitCSV(i)
160800090623     C                   EXSR      CHKNUM
160900090623     C                   IF        PiInt=*on      AND
161000090623     C                             PiVal<=9999999
161100090623     C                   Z-ADD     PiVal         VABNCD
161200090623     C                   ELSE
161300090623     C                   SETON                                        32
161400090623     C                   EVAL      vinmsg = %trimr(vinmsg)
161500090623     C                             + ' ' + 'VABNCD'
161600090623     C                   ENDIF
161700090623     C                   ENDIF
161800090623     C*** NCA
161900090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
162000090623     C                   EVAL      PiStr=SkSplitCSV(i)
162100090623     C                   EXSR      CHKNUM
162200090623     C                   IF        PiInt=*on      AND
162300090623     C                             PiVal<=9999999
162400090623     C                   Z-ADD     PiVal         VABNCA
162500090623     C                   ELSE
162600090623     C                   SETON                                        32
162700090623     C                   EVAL      vinmsg = %trimr(vinmsg)
162800090623     C                             + ' ' + 'VABNCA'
162900090623     C                   ENDIF
163000090623     C                   ENDIF
163100090623     C*** ZNC
163200090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
163300090623     C                   EVAL      PiStr=SkSplitCSV(i)
163400090623     C                   EXSR      CHKNUM
163500090623     C                   IF        PiInt=*on AND
163600090623     C                             PiVal<=99
163700090623     C                   Z-ADD     PiVal         VABZNC
163800090623     C                   ELSE
163900090623     C                   SETON                                        32
164000090623     C                   EVAL      vinmsg = %trimr(vinmsg)
164100090623     C                             + ' ' + 'VABZNC'
164200090623     C                   ENDIF
164300090623     C                   ENDIF
164400090623     C*** DCR
164500090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
164600090623     C                   EVAL      PiStr=SkSplitCSV(i)
164700090623     C                   EXSR      CHKNUM
164800090623     C                   IF        PiInt=*on       AND
164900090623     C                             PiVal<=99999999
165000090623     C                   Z-ADD     PiVal         VABDCR
165100090623     C                   ELSE
165200090623     C                   SETON                                        32
165300090623     C                   EVAL      vinmsg = %trimr(vinmsg)
165400090623     C                             + ' ' + 'VABDCR'
165500090623     C                   ENDIF
165600090623     C                   ENDIF
165700090623     C*** HCR
165800090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
165900090623     C                   EVAL      PiStr=SkSplitCSV(i)
166000090623     C                   EXSR      CHKNUM
166100090623     C                   IF        PiInt=*on   AND
166200090623     C                             PiVal<=9999
166300090623     C                   Z-ADD     PiVal         VABHCR
166400090623     C                   ELSE
166500090623     C                   SETON                                        32
166600090623     C                   EVAL      vinmsg = %trimr(vinmsg)
166700090623     C                             + ' ' + 'VABHCR'
166800090623     C                   ENDIF
166900090623     C                   ENDIF
167000090623     C*** VMD
167100090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
167200090623     C                   EVAL      PiStr=SkSplitCSV(i)
167300090623     C                   EXSR      CHKNUM
167400090623     C                   IF        PiNum=*on
167500090623     C                   Z-ADD     PiVal         VABVMD
167600090623     C                   ELSE
167700090623     C                   SETON                                        32
167800090623     C                   EVAL      vinmsg = %trimr(vinmsg)
167900090623     C                             + ' ' + 'VABVMD'
168000090623     C                   ENDIF
168100090623     C                   ENDIF
168200090623     C*** ANT
168300090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
168400090623     C                   EVAL      PiStr=SkSplitCSV(i)
168500090623     C                   EXSR      CHKNUM
168600090623     C                   IF        PiInt=*on        AND
168700090623     C                             PiVal<=999999999
168800090623     C                   Z-ADD     PiVal         VABANT
168900090623     C                   ELSE
169000090623     C                   SETON                                        32
169100090623     C                   EVAL      vinmsg = %trimr(vinmsg)
169200090623     C                             + ' ' + 'VABANT'
169300090623     C                   ENDIF
169400090623     C                   ENDIF
169500090623     C*** CMO
169600090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
169700090623     C                   EVAL      PiStr=SkSplitCSV(i)
169800090623     C                   EXSR      CHKNUM
169900090623     C                   IF        PiInt=*on
170000090623     C                   Z-ADD     PiVal         Num5_0
170100090623     C                   MOVEL(p)  Num5_0        VABCMO
170200090623     C                   ELSE
170300090623     C                   SETON                                        32
170400090623     C                   EVAL      VABCMO = *zeros
170500090623     C                   EVAL      vinmsg = %trimr(vinmsg)
170600090623     C                             + ' ' + 'VABCMO'
170700090623     C                   ENDIF
170800090623     C                   ENDIF
170900090623     C*
171000090911     C* Reperisco i campi del EDIVAT
171100090623     C*** VATNOT
171200090623     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
171300090623     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
171400090623     C                   ENDIF
171500090623     C*** VATTRC
171600090623     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
171700090623     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
171800090623     C                   ENDIF
171900090623     C*** VATNOT_A
172000111117     C                   IF        %trim(SkSplitFLD(i)) = 'HDR_RCONTACT'
172100090623     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
172200090623     C                   ENDIF
172300090623     C*** VATNOT_B
172400111117     C                   IF        %trim(SkSplitFLD(i)) = 'HDR_RPHONE'
172500090623     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
172600090623     C                   ENDIF
172700111117     C*** VATNOT_E_1
172800111117     C                   IF        %trim(SkSplitFLD(i)) = 'PCL_CREF1'
172900111117     C                   EVAL      wVATNOT_E_1 = '%0'+%subst(SkSplitCSV(i):8:6)
173000090623     C                   ENDIF
173100111117     C*** VATNOT_E_2
173200111117     C                   IF        %trim(SkSplitFLD(i)) = 'PCL_PARCELNO'
173300111117     C                   EVAL      wVATNOT_E_2 = %subst(SkSplitCSV(i):7:14)
173400111117     C                   ENDIF
173500111118     C*** VATNOT_E_3
173600111118     C                   IF        %trim(SkSplitFLD(i)) = 'PCL_SERVICE'
173700111118     C                   EVAL      wVATNOT_E_3 = SkSplitCSV(i)
173800111118     C                   ENDIF
173900111118     C*** VATNOT_E_4
174000111118     C                   IF        %trim(SkSplitFLD(i)) = 'PCL_RCOUNTRYN'
174100111118     C                   EVAL      wVATNOT_E_4 = SkSplitCSV(i)
174200111118     C                   ENDIF
174300090623     C*
174400090623     C***  ===>  Gestione campi "particolari" dentro ciclo
174500090623     C*
174600090623     C*
174700090623     C***  <===  -----------------------------------------
174800090908     C*
174900111118     C                   EVAL      wVATNOT_E = %trim(wVATNOT_E_1) +
175000111118     C                                         %trim(wVATNOT_E_2) +
175100111118     C                                         %trim(wVATNOT_E_3) +
175200111118     C                                         %trim(wVATNOT_E_4)
175300090623     C*
175400090623     C                   ENDSR
175500090623     C***
175600090623
175700010601
175800020204
175900020204     C*----------------------------------------------------*
176000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
176100020204     C*----------------------------------------------------*
176200020204     C     CHKIMPDIV     BEGSR
176300020204     C*
176400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
176500020204     C                   Z-ADD     *zeros        wrkDec            9 9
176600020204     C*
176700020204     C* Come prima cosa effettuo considerazioni sulla divisa
176800020204     C                   IF        vabIAS > *zeros
176900020204     C                   IF        vabVAS <> 'EUR'
177000020204     C                   EVAL      vabVAS =  'ITL'
177100020204     C                   ENDIF
177200020204     C                   ENDIF
177300020204     C*
177400020204     C                   IF        vabCAS > *zeros
177500020204     C                   IF        vabVCA <> 'EUR'
177600020204     C                   EVAL      vabVCA =  'ITL'
177700020204     C                   ENDIF
177800020204     C                   ENDIF
177900020204     C*
178000020204     C                   IF        vabVMD > *zeros
178100020204     C                   IF        vabVAD <> 'EUR'
178200020204     C                   EVAL      vabVAD =  'ITL'
178300020204     C                   ENDIF
178400020204     C                   ENDIF
178500020204     C*
178600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
178700020204     C                   Z-ADD     vabIAS        wrkDec
178800020204     C                   IF        wrkDec > *zeros
178900020204     C                   IF        vabVAS = 'ITL'
179000020204     C                   EVAL      vabIAS = *zeros
179100020204     C                   ENDIF
179200020204     C                   ENDIF
179300020204     C*
179400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
179500020204     C                   Z-ADD     vabCAS        wrkDec
179600020204     C                   IF        wrkDec > *zeros
179700020204     C                   IF        vabVCA = 'ITL'
179800020204     C                   EVAL      vabCAS = *zeros
179900020204     C                   ENDIF
180000020204     C                   ENDIF
180100020204     C*
180200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
180300020204     C                   Z-ADD     vabVMD        wrkDec
180400020204     C                   IF        wrkDec > *zeros
180500020204     C                   IF        vabVAD = 'ITL'
180600020204     C                   EVAL      vabVMD = *zeros
180700020204     C                   ENDIF
180800020204     C                   ENDIF
180900020204     C*
181000020204     C                   ENDSR
181100020204     C***
181200080617
181300080617
181400080617
181500080617     C*----------------------------------------------------*
181600080617     C*  CONTROLLO NUMERICITA' CAMPI
181700080617     C*----------------------------------------------------*
181800080617     C     CHKNUM        BEGSR
181900080617     C*
182000080617     C                   IF        PiDecChr = *blanks
182100080617     C                   EVAL      PiDecChr = CharNUM
182200080617     C                   ENDIF
182300080617     C*
182400080617     C                   callp     UBISNUM_Check(PiStr
182500080617     C                                          :PiDecChr
182600080617     C                                          :PiVal
182700080617     C                                          :PiNum
182800080617     C                                          :PiInt)
182900080617     C*
183000080617     C                   ENDSR
183100080617     C***
183200010330
183300010601
183400010601
183500010601
183600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
183700010601     C     repfil        BEGSR
183800010601     C*
183900010601     C                   if        invfil = *zeros and
184000010601     C                             depfil > *zeros and
184100010629     C                             (vinflg = *blanks or
184200010629     C                              vinflg = *zeros)
184300010601     C
184400010601     C                   eval      invfil = depfil
184500010601     C                   endif
184600010601     C*
184700010601     C                   if        depfil <> invfil and
184800010601     C                             invfil > *zeros
184900010601     C                   eval      flgMulti = '1'
185000010601     C                   if        vinflg = *blanks
185100010601     C                   add       1             cntNonEl
185200010601     C                   endif
185300010601     C                   endif
185400010601     C*
185500010601     C                   if        vinflg = '2'
185600010601     C                   eval      flgStato = '2'
185700010601     C                   endif
185800010601     C*
185900010601     C                   ENDSR
186000010601     C***
186100010601
186200010601
186300010601
186400010330
186500040119      /TITLE Invio dei dati al punto operativo.
186600040119     C     invio         BEGSR
186700040119     C*
186800101005     C* 1° invio EDIVAT
186900040119     C                   reset                   dscmz
187000040119     C                   move      vlrpoi        cmzdst
187100090911     C                   eval      cmzfld = 'EDIVATWR'
187200040119     C                   eval      cmzmbd = vlrhdl
187300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
187400040119     C***                if        prmfir = *blanks
187500090911     C                   eval      cmzfla = 'EDIVAT0F'
187600090911     C                   eval      cmzmba = 'EDIVAT0F'
187700040119     C***                else
187800040119     C***                eval      cmzfla = prmfir
187900040119     C***                eval      cmzmba = prmfir
188000040119     C***                endif
188100040119     C                   eval      cmznrr = *zeros
188200040119     C                   move      §ctrokvt      cmznrr
188300040119     C                   eval      cmzlba = vlrfl1
188400040119     C                   call(e)   'TIS711C'
188500040119     C                   parm                    dscmz
188600040119     C                   parm      *blanks       esito
188700040119     C                   if        %error
188800040119     C                             or cmzerr = '1'
188900040119     C                             or esito  = '1'
189000040119     C                   eval      wrkesito = '3'
189100040119     C                   else
189200040119     C*
189300101005     C* 2° invio EDIVAB
189400040119     C                   reset                   dscmz
189500040119     C                   move      vlrpoi        cmzdst
189600040119     C                   eval      cmzfld = vlrfou
189700040119     C                   eval      cmzmbd = vlrhdl
189800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
189900040119     C***                if        prmfir = *blanks
190000090911     C                   eval      cmzfla = 'EDIVAB0F'
190100090911     C                   eval      cmzmba = 'EDIVAB0F'
190200040119     C***                else
190300040119     C***                eval      cmzfla = prmfir
190400040119     C***                eval      cmzmba = prmfir
190500040119     C***                endif
190600040119     C                   eval      cmznrr = *zeros
190700040119     C                   move      §ctrokvb      cmznrr
190800040119     C                   eval      cmzlba = vlrfl1
190900040119     C                   call(e)   'TIS711C'
191000040119     C                   parm                    dscmz
191100040119     C                   parm      *blanks       esito
191200040119     C                   if        %error
191300040119     C                             or cmzerr = '1'
191400040119     C                             or esito  = '1'
191500040119     C                   eval      wrkesito = '3'
191600040119     C                   endif
191700040119     C                   endif
191800040119     C*
191900040119     C                   ENDSR
192000040119     C***
192100010601
192200010601
192300010601
192400010601
192500010601      /TITLE Invio dei dati al punto operativo.
192600010601     C     opeini        BEGSR
192700010601     C*
192800010601     C* Inizializzo flag e contatori operativi
192900010601     C                   movel     '0'           flgGiro           1
193000010601     C                   movel     '0'           flgMulti          1
193100010601     C                   movel     '1'           flgStato          1
193200010615     C                   movel     '0'           flgOk             1
193300010601     C                   z-add     *zeros        cntNonEl         10 0
193400010601     C                   z-add     *zeros        depfil            3 0
193500010601     C                   z-add     *zeros        invfil            3 0
193600010601     C*
193700010601     C                   ENDSR
193800010601     C***
193900070326
194000070326
194100070326
194200070326
194300080916     C     *pssr         BEGSR
194400070329     C*
194500080916     C                   if        %open(tivin00r)
194600080916     C                   close     tivin00r
194700080916     C                   endif
194800090911     C                   if        %open(edivabwr)
194900090911     C                   close     edivabwr
195000080916     C                   endif
195100090911     C                   if        %open(edivatwr)
195200090911     C                   close     edivatwr
195300080916     C                   endif
195400070326     C*
195500070326     C* Effettuo la chiamata al CLLE preposto
195600101005     C                   call(e)   'TITVEVTC'
195700080916     C                   parm                    parccm
195800080916     C                   parm                    parmbr
195900080916     C                   parm      '2'           paropz
196000070326     C*
196100080916     C                   eval      wrkesito = '2'
196200070404     C*
196300080916     C                   seton                                        LR
196400070326     C*
196500080916     C                   ENDSR     '*CANCL'
196600070326     C***
196700070326
196800070326
196900010330
197000010330
197100000613     C     *inzsr        BEGSR
197200990910     C*
197300990910     C     *entry        plist
197400990920     C                   parm                    tivlrds
197500990921     C                   parm      wrkesito      esito
197600000724     C                   parm                    prmlit
197700000710     C                   parm                    prmfir
197800010330     C*
197900010330     C* CALCOLA LA DATA CORRENTE
198000010330     C                   time                    wn14             14 0
198100090911     C                   movel     wn14          oracor            6 0          *ORA
198200110923     C                   z-add     *zeros        datcor            8 0
198300110923     C                   eval      datcor = %dec(%date() : *ISO)
198400111118     C*
198500111118     C* Forzo un numero CMR
198600111118     C                   eval      wCMR = 'DPD NL'+%char(datcor)+%char(oracor)
198700000613     C*
198800000613     C                   ENDSR
198900000613     C***
199000990908
199100090623     Otitv1q7p  E            riepilogo         2
199200990915     O                                              'Upload via Internet'
199300990915     O                                           +1 'Traduzione TIVIN00R -'
199400090911     O                                           +1 'EDIVABWR'
199500990915     O                                           +1 'Testate bolle'
199600990915     O          E            riepilogo   2
199700990915     O                       wrkdata
199800990915     O                       wrkora              +1
199900990915     O                       procname            +1
200000990915     O          E            riepilogo   2
200100990915     O                                              'Cliente..................:'
200200010605     O                       VABCCM        z     +1
200300990915     O          E            riepilogo   2
200400990920     O                                              'Riferimento Strategi.....:'
200500990920     O                       vlrhdl              +1
200600990915     O          E            riepilogo   2
200700990915     O                                              'Giusti...................:'
200800040119     O                       §CTROKVB      2   +  1
200900990915     O          E            riepilogo   2
201000990915     O                                              'Sbagliati e corretti.....:'
201100971022     O                       §CTRMO        2   +  1
201200990915     O          E            riepilogo   2
201300990915     O                                              'Sbagliati e scartati.....:'
201400971022     O                       §CTRNO        2   +  1
201500000613
201600090623     Otitv1q7ps E            testdett          2
201700000613     O                                              'Upload via Internet'
201800000613     O                                           +1 'Traduzione TIVIN00R -'
201900090911     O                                           +1 'EDIVABWR'
202000000613     O                                           +1 'Testate bolle'
202100000616     O          E            testdett    3
202200000613     O                                           +2 'N° rec'
202300000613     O                                           +3 'Anteprima contenuto'
202400000616     O          E            rigadett    2
202500000613     O                       rrnum               +2
202600000621     O                       recko               +3
202700000616     O          E            findett     2
202800000613     O                       wrkdata
202900000613     O                       wrkora              +1
203000000613     O                       procname            +1
203100000616     O          E            findett     2
203200000613     O                                              'Cliente..................:'
203300010605     O                       VABCCM        z     +1
203400000616     O          E            findett     2
203500000613     O                                              'Riferimento Strategi.....:'
203600000613     O                       vlrhdl              +1
203700000616     O          E            findett     2
203800000613     O                                              'Giusti...................:'
203900040119     O                       §CTROKVB      2   +  1
204000000616     O          E            findett     2
204100000613     O                                              'Sbagliati e corretti.....:'
204200000613     O                       §CTRMO        2   +  1
204300000616     O          E            findett     2
204400000613     O                                              'Sbagliati e scartati.....:'
204500000613     O                       §CTRNO        2   +  1
204600000613** CMD - COMANDI CL
204700090623OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1Q7P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
204800090623OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1Q7PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
204900090623DLTOVR FILE(TITV1Q7P TITV1Q7PS) LVL(*)
205000010606
205100000613
