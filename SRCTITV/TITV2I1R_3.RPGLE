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
005100111118     D SkSplitFLDH     S             32    DIM(200)
005200111118     D SkSplitFLDP     S             32    DIM(200)
005300111118     D SkSplitCSV      S            256    DIM(200)
005400030715     D CharCSV         S              1
005500030715     D CharTXT         S              1
005600030715     D CharNUM         S              1
005700030715     D posDa           S              3  0 INZ(*zeros)
005800030715     D posA            S              3  0 INZ(*zeros)
005900030715     D i               s              3  0 INZ(1)
006000030715     D wGiro           s              1  0 INZ(*zeros)
006100080125     D wVATNOT_A       s                   LIKE(VATNOT)
006200080125     D wVATNOT_B       s                   LIKE(VATNOT)
006300090904     D wVATNOT_E       s                   LIKE(VATNOT)
006400111117     D wVATNOT_E_1     s                   LIKE(VATNOT)
006500111117     D wVATNOT_E_2     s                   LIKE(VATNOT)
006600111118     D wVATNOT_E_3     s                   LIKE(VATNOT)
006700111118     D wVATNOT_E_4     s                   LIKE(VATNOT)
006800030822     D*------------------
006900030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
007000030822     D*------------------
007100030822     D posDaDft        S              3  0 INZ(*zeros)
007200030822     D posADft         S              3  0 INZ(*zeros)
007300030822     D j               s              3  0 INZ(1)
007400030822     D wGiroDft        s              1  0 INZ(*zeros)
007500031201     D*------------------
007600031201     D* Costanti
007700031201     D*------------------
007800031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007900031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008000080617     D*------------------
008100080617
008200080617     D*------------------
008300080617     D* LINKING A DEFINIZIONI ESTERNE
008400080617     D*------------------
008500080617     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008600090220     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
008700010330
008800010330
008900010330
009000990915     C                   time                    wrkdata
009100990915     C                   time                    wrkora
009200000913     C                   reset                   rrnum
009300990921     C                   reset                   esito
009400081014     C                   reset                   wrkesito
009500010601     C*
009600010601     C                   exsr      opeini
009700010605     C                   exsr      rwvab
009800081014     C*
009900081014     C                   exsr      endela
010000010601     C*
010100010601     C                   seton                                        lr
010200010601
010300010601
010400010601
010500010601     C*--------------------------------------------------------
010600010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010700010601     C*--------------------------------------------------------
010800010601     C     PREELA        BEGSR
010900010601     C*
011000010601     C* SE OCCORRE SPEDIRE IN FILIALE
011100010601     C                   if        invfil <> *zeros and
011200010601     C                             flgGiro = '0'
011300010601     C*
011400010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011500010601     C                   eval      flgGiro = '1'
011600000724     C*
011700000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
011800000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011900010601     C     invfil        chain     azorg01l
012000000724     C                   if        %found
012100000724     C                   movel(p)  CMD(1)        depcmd
012200020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
012300000724     C*
012400000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
012500011113     C                   Z-ADD     150           LENGH            15 5
012600000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012700000724     C                   PARM                    depcmd
012800000724     C                   PARM                    LENGH
012900000724     C*
013000000724     C                   endif
013100000616     C*
013200000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
013300000616     C                   movel(p)  CMD(2)        depcmd
013400000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
013500000616     C*
013600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
013700011113     C                   Z-ADD     150           LENGH            15 5
013800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013900000616     C                   PARM                    depcmd
014000000616     C                   PARM                    LENGH
014100000613     C*
014200090623     C                   if        not %open(titv1q7ps)
014300090623     C                   open      titv1q7ps
014400070329     C                   except    testdett
014500000613     C                   endif
014600010601     C*
014700010601     C                   endif
014800010601     C*
014900010601     C                   ENDSR
015000010601     C***
015100010601
015200010601
015300010601
015400010601
015500010601     C*--------------------------------------------------------
015600010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
015700010601     C*--------------------------------------------------------
015800010601     C     ENDELA        BEGSR
015900081014     C*
016000081014     C                   if        %open(tivin00r)
016100081014     C                   close     tivin00r
016200081014     C                   endif
016300090911     C                   if        %open(edivabwr)
016400090911     C                   close     edivabwr
016500081014     C                   endif
016600090911     C                   if        %open(edivatwr)
016700090911     C                   close     edivatwr
016800081014     C                   endif
016900040119     C*
017000040119     C* Effettuo la chiamata al CLLE preposto
017100101005     C                   call(e)   'TITVEVTC'
017200040510     C                   parm                    parccm
017300040119     C                   parm                    parmbr
017400040119     C                   parm      '2'           paropz
017500050201     C*
017600050201     C* Effettuo lancio TISI95 solo x chiusura
017700050201     C                   CLEAR                   TISI95DS
017800050201     C                   EVAL      I95TLA = 'C'
017900050201     C                   CALL      'TISI95R'
018000050201     C                   PARM                    TISI95DS
018100000613     C*
018200090623     C                   if        %open(titv1q7ps)
018300000616     C                   except    findett
018400090623     C                   close     titv1q7ps
018500000613     C                   endif
018600000616     C*
018700000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
018800011113     C                   Z-ADD     150           LENGH            15 5
018900010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
019000000616     C                   PARM                    CMD(3)
019100000616     C                   PARM                    LENGH
019200000616     C*
019300010601     C                   ENDSR
019400010601     C***
019500010601
019600010601
019700010601
019800000613
019900000613     C*--------------------------------------------------------
020000000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
020100000613     C*--------------------------------------------------------
020200000621     C     STPR          BEGSR
020300000613     C*
020400090623     C                   if        not %open(titv1q7p)
020500090623     C   50              open      titv1q7p
020600990915     C                   endif
020700990915     C*
020800030715     C   50              except    riepilogo
020900990915     C*
021000090623     C                   if        %open(titv1q7p)
021100090623     C   50              close     titv1q7p
021200990914     C                   endif
021300000613     C*
021400000613     C                   ENDSR
021500000613     C***
021600000613
021700010601
021800010601
021900010330     C*--------------------------------------------------------
022000090911     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
022100010330     C*--------------------------------------------------------
022200010605     C     RWVAB         BEGSR
022300010330     C*
022400010330     C                   if        not %open(tivin00r)
022500010330     C                   open      tivin00r
022600010330     C                   endif
022700090911     C                   if        not %open(edivabwr)
022800090911     C                   open      edivabwr
022900010330     C                   endif
023000040119     C*
023100101005     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
023200101005     C                   exsr      prevat
023300040119     C*
023400040119     C                   if        chkcall = '0'
023500040119     C*
023600090911     C                   if        not %open(edivatwr)
023700090911     C                   open      edivatwr
023800040119     C                   endif
023900090623     C*
024000040119     C                   clear                   §CTROKVB          7 0
024100040119     C                   clear                   §CTROKVT          7 0
024200040119     C                   clear                   §CTRMO            7 0
024300040119     C                   clear                   §CTRNO            7 0
024400090623     C*
024500090623     C* Inizializzazioni
024600090623     C                   exsr      inzvar
024700090623     C                   z-add     *zeros        wNewBolla         7 0
024800010330     C*
024900030822     C                   do        *HIVAL
025000010330     C*
025100030822     C                   read      tivin00r                               70
025200010618     C*
025300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
025400010618     C                   if        vinflg = '1'
025500010618     C                   eval      flgOk = '1'
025600010618     C                   endif
025700040510     C*
025800040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
025900040510     C                   z-add     *zeros        wLenVINDTA        4 0
026000040510     C                   z-add     *zeros        wFlgVINDTA        1 0
026100040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
026200040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
026300040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
026400040510     C                   z-add     1             wFlgVINDTA
026500040510     C                   leave
026600040510     C                   endif
026700040510     C                   enddo
026800110923     C*
026900110923     C* Ignoro i record che iniziano x '#'
027000110923     C                   if        %subst(vindta:1:1)  ='#'                  AND
027100110923     C                             %subst(vindta:1:18)<>'#DEF;MPSEXP:HEADER' AND
027200110923     C                             %subst(vindta:1:18)<>'#DEF;MPSEXP:PARCEL'
027300111118     C                   z-add     0             wFlgVINDTA
027400111118     C                   else
027500110923     C*
027600110923     C* Setto i column headers "variabili"
027700111118     C                   if        %subst(vindta:1:18) = '#DEF;MPSEXP:HEADER'
027800110923     C                   eval      header = %trim(%subst(vindta:13))
027900110923     C                   endif
028000111118     C                   if        %subst(vindta:1:18) = '#DEF;MPSEXP:PARCEL'
028100110923     C                   eval      parcel = %trim(%subst(vindta:13))
028200110923     C                   endif
028300111118     C                   endif
028400090420     C*
028500090420     C                   add       1             rrnum
028600010618     C*
028700040510     C                   if        vindta > *blanks AND
028800090420     C                             wFlgVINDTA = 1
028900010330     C*
029000010601     C                   if        *in70 = *off and
029100010330     C                             (vinflg = *blanks
029200010330     C                              or vinflg = '0'
029300010330     C                              or vinflg = '2')
029400010330     C*
029500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
029600010711     C                   if        vinflg = *blanks or vinflg = '0'
029700010711     C                   clear                   vinmsg
029800010711     C                   endif
029900010601     C*
030000090421     C* Aggiungo un byte ad '*' all'inizio d ogni record
030100111117     C***                if        %subst(%trim(vindta):1:1) = CharCSV
030200111117     C***                eval      vindta  = '*' + %trim(vindta)
030300111117     C***                endif
030400010601     C*
030500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
030600010605     C                   eval      depfil = VABLNP
030700010601     C                   exsr      repfil
030800010601     C                   if        depfil = invfil
030900021025     C                   if        vlrpoi = 999
031000030822     C                   move(P)   invfil        VABFGS
031100021025     C                   else
031200030822     C                   move(P)   vlrpoi        VABFGS
031300021025     C                   endif
031400090914     C*
031500090914     C                   move      VABFGS        currFGS           3 0
031600090914     C*
031700090914     C* Elaborazione record corrente
031800090914     C                   exsr      elabCurrRec
031900010601     C*
032000010601     C                   exsr      PREELA
032100010604     C*
032200010604     C                   if        *in31 = *off and
032300010604     C                             *in32 = *off
032400010604     C                   eval      vinflg = '1'
032500010604     C                   else
032600010604     C                   eval      recko = vindta
032700010604     C                   except    rigadett
032800010604     C                   eval      vinflg = '2'
032900010604     C                   endif
033000010604     C                   endif
033100010601     C*
033200010604     C                   endif
033300010604     C*
033400010330     C                   else
033500010330     C                   eval      vinflg = '1'
033600010330     C                   endif
033700010601     C*
033800010601     C  N70              update    tivin000
033900010330     C*
034000030822     C  N70              enddo
034100090623     C*
034200090623     C* Scarico la testata rimasta "in canna"
034300090623     C  N31              exsr      wriVAB
034400040119     C*
034500040119     C                   endif                                                  (endif - chkcall)
034600010601     C*
034700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
034800010601     C                   if        cntNonEl = *zeros or
034900010601     C                             flgMulti = '0'
035000010330     C* Se non ci sono record con errori ...
035100010601     C                   if        §ctrno = 0 and
035200010604     C                             §ctrmo = 0 and
035300010601     C                             flgStato <> '2'
035400010330     C* ... restituisco esito OK.
035500010330     C                   eval      wrkesito = '0'
035600010330     C                   else
035700040119     C                   if        §ctrokvb > 0
035800010330     C                   eval      wrkesito = '1'
035900010330     C                   else
036000010615     C                   if        flgOk = '0'
036100010615     C                   eval      wrkesito = '2'
036200010615     C                   else
036300010615     C                   eval      wrkesito = '6'
036400010615     C                   endif
036500010330     C                   endif
036600010330     C                   endif
036700010601     C                   else
036800010601     C                   eval      wrkesito = '9'
036900010601     C                   endif
037000010330     C*
037100010330     C                   if        %open(tivin00r)
037200010330     C                   close     tivin00r
037300010330     C                   endif
037400090911     C                   if        %open(edivabwr)
037500090911     C                   close     edivabwr
037600010330     C                   endif
037700090911     C                   if        %open(edivatwr)
037800090911     C                   close     edivatwr
037900040512     C                   endif
038000010601     C*
038100010601     C                   if        vlrpoi <> 999
038200010601     C                   eval      invfil = vlrpoi
038300010601     C                   endif
038400010330     C*
038500040119     C                   if        §ctrokvb > 0
038600010601     C                             and invfil > *zeros
038700010330     C                   exsr      invio
038800010330     C                   endif
038900010601     C*
039000010618     C                   if        flgGiro = '1'
039100081014     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
039200010618     C                   endif
039300010330     C*
039400010330     C                   ENDSR
039500010330     C***
039600090623
039700090623
039800090623
039900090623     C*----------------------------------------------------*
040000090623     C*  INIZIALIZZAZIONI CAMPI DI WRK
040100090623     C*----------------------------------------------------*
040200090623     C     INZWRK        BEGSR
040300090623     C*
040400090623     C                   eval      i = 1
040500090623     C                   eval      posDa = *zeros
040600090623     C                   eval      posA  = *zeros
040700090623     C*
040800090623     C                   ENDSR
040900090623
041000090623
041100090623
041200090623     C*----------------------------------------------------*
041300090623     C*  ELABORAZIONE RECORD CORRENTE
041400090623     C*----------------------------------------------------*
041500090623     C     ELABCURRREC   BEGSR
041600090623     C*
041700090623     C                   select
041800090623     C*
041900110923     C* Tipo record "TESTATA" - HEADER
042000111118     C                   when      %subst(vindta:1:6) = 'HEADER'
042100090623     C                   if        wNewBolla > *zeros
042200090623     C  N31              exsr      wriVAB
042300090623     C                   exsr      inzvar
042400090623     C                   endif
042500090624     C                   eval      wNewBolla = 1
042600090623     C                   eval      wGiro = *zeros
042700111118     C                   clear                   SkSplitFLDH
042800110923     C                   eval      prefix = 'HDR_'
042900090624     C                   exsr      inzwrk
043000090623     C                   exsr      impvab
043100090623     C                   exsr      inzwrk
043200090623     C                   exsr      impvab
043300090623     C*
043400110923     C* Tipo record "DETTAGLIO" - PARCEL
043500110923     C                   when      %subst(vindta:1:6) = 'PARCEL'
043600090623     C                   eval      wGiro = *zeros
043700111118     C                   clear                   SkSplitFLDP
043800110923     C                   eval      prefix = 'PCL_'
043900090623     C                   exsr      inzwrk
044000101005     C                   exsr      impvat
044100090623     C                   exsr      inzwrk
044200101005     C                   exsr      impvat
044300090624     C*
044400090624     C  N31              exsr      wrivat
044500090623     C*
044600090623     C*
044700090623     C                   endsl
044800090623     C*
044900090623     C                   ENDSR
045000040119
045100040119
045200040119
045300040119     C*----------------------------------------------------*
045400040119     C*  SCARICAMENTO BUFFER RECORDS VAB
045500040119     C*----------------------------------------------------*
045600040510     C     WRIVAB        BEGSR
045700090623     C*
045800090623     C  N31              add       1             §CTROKVB
045900090623     C   32              add       1             §CTRMO
046000090623     C   31              add       1             §CTRNO
046100040119     C*
046200060331     C* Gestisco l'eventuale rottura x numero spedizione
046300070823     C                   if        VABNSP <> depspe
046400070301     C                   if        wVATNOT_E <> *blanks
046500080125     C                   if        VABCTM = *blanks
046600080125     C                   eval      VABCTM = '7Q'
046700080125     C                   endif
046800070301     C                   endif
046900090911     C*
047000090911     C* VALORIZZO CAMPI RELATIVI AL "CMR"
047100090911     C                   eval      VABCMR = wCMR
047200090911     C                   eval      VABDCM = DATCOR
047300090911     C                   eval      VABDTS = DATCOR
047400090911     C                   eval      VABHMS = ORACOR
047500090911     C                   eval      VABCNT = 1
047600090911     C****
047700090911     C                   write     edivab00                                     => scarico il VAB
047800060331     C                   endif
047900090623     C*
048000090623     C  N31              eval      depspe = vabNSP
048100040119     C*
048200040119     C                   ENDSR
048300040119
048400040119
048500040119
048600040119     C*----------------------------------------------------*
048700040119     C*  SCARICAMENTO BUFFER RECORDS VAT
048800040119     C*----------------------------------------------------*
048900040510     C     WRIVAT        BEGSR
049000060512     C*
049100060512     C* Inizializzo i campi chiave
049200090914     C                   eval      VATFGS = currFGS
049300060512     C                   eval      VATAAS = VABAAS
049400060512     C                   eval      VATCCM = VABCCM
049500070823     C                   eval      VATNRS = VABNRS
049600060512     C                   eval      VATNSP = VABNSP
049700060512     C                   eval      VATLNP = VABLNP
049800111117     C                   eval      VATCMR = wCMR
049900090914     C                   eval      VATCNT = 1
050000060331     C*
050100060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
050200060331     C                   if        wVATNOT_A <> *blanks
050300060331     C                   eval      VATNOT = wVATNOT_A
050400060331     C                   eval      VATTRC = 'A'
050500090911     C                   write     EDIVAT00
050600060331     C                   add       1             §CTROKVT
050700060331     C                   endif
050800060331     C*
050900060331     C                   if        wVATNOT_B <> *blanks
051000060331     C                   eval      VATNOT = wVATNOT_B
051100060331     C                   eval      VATTRC = 'B'
051200090911     C                   write     EDIVAT00
051300060331     C                   add       1             §CTROKVT
051400060331     C                   endif
051500111118     C*
051600111118     C                   if        wVATNOT_E <> *blanks
051700111118     C                   eval      VATNOT = wVATNOT_E
051800111118     C                   eval      VATTRC = 'E'
051900111118     C                   write     EDIVAT00
052000111118     C                   add       1             §CTROKVT
052100111118     C                   endif
052200090623     C*
052300111118 xxx C* Considero il VAT solo a rottura del "chi sono"
052400111118     C***                if        wVATNOT_E <> *blanks AND
052500111118     C***                          wVATNOT_E <> sVATNOT_E
052600111118     C***                eval      VATNOT = wVATNOT_E
052700111118     C***                eval      VATTRC = 'E'
052800111118     C***                write     EDIVAT00
052900111118     C***                add       1             §CTROKVT
053000111118     C***                eval      sVATNOT_E = wVATNOT_E
053100111118     C***                add       1             VABNCL
053200111118     C***                endif
053300040119     C*
053400040119     C                   ENDSR
053500040119
053600040119
053700040119
053800040119     C*----------------------------------------------------*
053900101005     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
054000040119     C*----------------------------------------------------*
054100101005     C     PREVAT        BEGSR
054200040119     C*
054300101005     C* Compongo il nome del membro da dare al EDIVATWR
054400040119     C                   eval      parmbr = vlrhdl
054500040119     C                   movel     'M'           parmbr
054600040510     C                   eval      parccm = vlrksc
054700040119     C                   eval      paropz = '1'
054800040119     C* Effettuo la chiamata al CLLE preposto
054900101005     C                   call(e)   'TITVEVTC'
055000040119     C                   parm                    parccm
055100040119     C                   parm                    parmbr
055200040119     C                   parm                    paropz
055300040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
055400040119     C                   if        %error
055500040119     C                   movel     '1'           chkcall
055600040119     C                   else
055700040119     C                   movel     '0'           chkcall
055800040119     C                   endif
055900040119     C*
056000040119     C                   ENDSR
056100010601
056200010601
056300010601
056400010330     C*----------------------------------------------------*
056500030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
056600010330     C*----------------------------------------------------*
056700040119     C     INZVAR        BEGSR
056800010330     C*
056900030822     C* Inizializzo variabili di wrk
057000090916     C                   Z-ADD     *zeros        Num5_0            5 0
057100090916     C                   MOVEL     *blanks       wNOTE            70
057200060331     C                   MOVEL     *blanks       wVATNOT_A
057300060331     C                   MOVEL     *blanks       wVATNOT_B
057400090904     C                   MOVEL     *blanks       wVATNOT_E
057500111117     C                   MOVEL     *blanks       wVATNOT_E_1
057600111117     C                   MOVEL     *blanks       wVATNOT_E_2
057700111118     C                   MOVEL     *blanks       wVATNOT_E_3
057800111118     C                   MOVEL     *blanks       wVATNOT_E_4
057900090623     C*
058000111118 xxx C***                MOVEL     *blanks       sVATNOT_E        35
058100090220     C*
058200080617     C                   EVAL      wFlgCAS = *blanks
058300030822     C*
058400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
058500090911     C                   CLEAR                   EDIVAB00
058600090911     C                   CLEAR                   EDIVAT00
058700030822     C                   CLEAR                   SkSplitCSV
058800090623     C                   EXSR      inzwrk
058900030822     C*
059000030822     C* Reimposto i valori di default
059100030822     C                   EXSR      DEFCAM
059200030822     C*
059300010330     C                   ENDSR
059400010330     C*----------------------------------------------------*
059500030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
059600010330     C*----------------------------------------------------*
059700010330     C     DEFCAM        BEGSR
059800030822     C*
059900030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
060000030715     C* e delimitatore testo.
060100030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
060200030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
060300030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
060400030822     C*
060500030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
060600030715     C                   IF        %subst(vlrppt:1:1) = 'S'
060700030715     C                   SETON                                        50
060800030715     C                   ELSE
060900030715     C                   SETOFF                                       50
061000030715     C                   ENDIF
061100090421     C*
061200090421     C* D default SEMPRE porto franco / SEMPRE lnp = p.o. invio / SEMPRE nsp = rrnum
061300090421     C                   EVAL      VABCBO = '1'
061400090421     C                   EVAL      VABLNP = vlrpoi
061500090421     C                   EVAL      VABNSP = rrnum
061600030822     C*
061700030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
061800030822     C                   EVAL      posDaDft = 1
061900030822     C                   EVAL      posADft  = 0
062000030822     C                   EVAL      wGiroDft = 0
062100030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
062200030822     C                             posDaDft > 0
062300030822     C*
062400030822     C* Gestisco il 1° giro
062500030822     C                   IF        wGiroDft = 0
062600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
062700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
062800030822     C* Incremento il contatore dei "giri"
062900030822     C                   EVAL      wGiroDft = 1
063000030822     C                   ELSE
063100030822     C                   EVAL      posDaDft = posADft
063200030822     C                   ENDIF
063300030822     C* Eseguo lo scan x trovare la fine del campo corrente
063400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
063500030822     C*
063600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
063700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
063800030822     C                   IF        posDaDft > 0 AND
063900030822     C                             posADft  > 0
064000030822     C* NCL
064100030822     C                   IF        %subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):1:3)
064400030822     C                             = 'NCL'
064500030822     C                   EVAL      PiStr=%trim(%subst(
064600030822     C                             %subst(vlrppt:posDaDft+1:
064700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064800030822     C                   EXSR      CHKNUM
064900030822     C                   IF        PiInt=*on
065000030822     C                   Z-ADD     PiVal         VABNCL
065100030822     C                   ENDIF
065200030822     C                   ENDIF
065300030822     C* CCM
065400030822     C                   IF        %subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):1:3)
065700030822     C                             = 'CCM'
065800030822     C                   EVAL      PiStr=%trim(%subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066100030822     C                   EXSR      CHKNUM
066200030822     C                   IF        PiInt=*on
066300030822     C                   Z-ADD     PiVal         VABCCM
066400030822     C                   ENDIF
066500030822     C                   ENDIF
066600030822     C* LNP
066700030822     C                   IF        %subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):1:3)
067000030822     C                             = 'LNP'
067100030822     C                   EVAL      PiStr=%trim(%subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067400030822     C                   EXSR      CHKNUM
067500030822     C                   IF        PiInt=*on
067600030822     C                   Z-ADD     PiVal         VABLNP
067700030822     C                   ENDIF
067800030822     C                   ENDIF
067900030822     C* NRS
068000030822     C                   IF        %subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):1:3)
068300030822     C                             = 'NRS'
068400030822     C                   EVAL      PiStr=%trim(%subst(
068500030822     C                             %subst(vlrppt:posDaDft+1:
068600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068700030822     C                   EXSR      CHKNUM
068800030822     C                   IF        PiInt=*on
068900030822     C                   Z-ADD     PiVal         VABNRS
069000030822     C                   ENDIF
069100030822     C                   ENDIF
069200030822     C* CTR
069300030822     C                   IF        %subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):1:3)
069600030822     C                             = 'CTR'
069700030822     C                   EVAL      PiStr=%trim(%subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070000030822     C                   EXSR      CHKNUM
070100030822     C                   IF        PiInt=*on
070200030822     C                   Z-ADD     PiVal         VABCTR
070300030822     C                   ENDIF
070400030822     C                   ENDIF
070500030822     C* PKB
070600030822     C                   IF        %subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):1:3)
070900030822     C                             = 'PKB'
071000030822     C                   EVAL      PiStr=%trim(%subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071300030822     C                   EXSR      CHKNUM
071400030822     C                   IF        PiNum=*on
071500030822     C                   Z-ADD     PiVal         VABPKB
071600030822     C                   ENDIF
071700030822     C                   ENDIF
071800030822     C* VLB
071900030822     C                   IF        %subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):1:3)
072200030822     C                             = 'VLB'
072300030822     C                   EVAL      PiStr=%trim(%subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600030822     C                   EXSR      CHKNUM
072700030822     C                   IF        PiNum=*on
072800030822     C                   Z-ADD     PiVal         VABVLB
072900030822     C                   ENDIF
073000030822     C                   ENDIF
073100030822     C* QFT
073200030822     C                   IF        %subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):1:3)
073500030822     C                             = 'QFT'
073600030822     C                   EVAL      PiStr=%trim(%subst(
073700030822     C                             %subst(vlrppt:posDaDft+1:
073800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073900030822     C                   EXSR      CHKNUM
074000030822     C                   IF        PiNum=*on
074100030822     C                   Z-ADD     PiVal         VABQFT
074200030822     C                   ENDIF
074300030822     C                   ENDIF
074400030822     C* CBO
074500030822     C                   IF        %subst(
074600030822     C                             %subst(vlrppt:posDaDft+1:
074700030822     C                             posADft-posDaDft-1):1:3)
074800030822     C                             = 'CBO'
074900030822     C                   EVAL      VABCBO=%trim(%subst(
075000030822     C                             %subst(vlrppt:posDaDft+1:
075100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075200030822     C                   ENDIF
075300030822     C* TSP
075400030822     C                   IF        %subst(
075500030822     C                             %subst(vlrppt:posDaDft+1:
075600030822     C                             posADft-posDaDft-1):1:3)
075700030822     C                             = 'TSP'
075800030822     C                   EVAL      VABTSP=%trim(%subst(
075900030822     C                             %subst(vlrppt:posDaDft+1:
076000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076100030822     C                   ENDIF
076200030822     C* VAS
076300030822     C                   IF        %subst(
076400030822     C                             %subst(vlrppt:posDaDft+1:
076500030822     C                             posADft-posDaDft-1):1:3)
076600030822     C                             = 'VAS'
076700030822     C                   EVAL      VABVAS=%trim(%subst(
076800030822     C                             %subst(vlrppt:posDaDft+1:
076900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077000030822     C                   ENDIF
077100030822     C* VCA
077200030822     C                   IF        %subst(
077300030822     C                             %subst(vlrppt:posDaDft+1:
077400030822     C                             posADft-posDaDft-1):1:3)
077500030822     C                             = 'VCA'
077600030822     C                   EVAL      VABVCA=%trim(%subst(
077700030822     C                             %subst(vlrppt:posDaDft+1:
077800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077900030822     C                   ENDIF
078000070925     C* TIC
078100070925     C                   IF        %subst(
078200070925     C                             %subst(vlrppt:posDaDft+1:
078300070925     C                             posADft-posDaDft-1):1:3)
078400070925     C                             = 'TIC'
078500070925     C                   EVAL      VABTIC=%trim(%subst(
078600070925     C                             %subst(vlrppt:posDaDft+1:
078700070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078800070925     C                   ENDIF
078900030822     C* GCA
079000030822     C                   IF        %subst(
079100030822     C                             %subst(vlrppt:posDaDft+1:
079200030822     C                             posADft-posDaDft-1):1:3)
079300030822     C                             = 'GCA'
079400030822     C                   EVAL      VABGCA=%trim(%subst(
079500030822     C                             %subst(vlrppt:posDaDft+1:
079600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079700030822     C                   ENDIF
079800030822     C* CTM
079900030822     C                   IF        %subst(
080000030822     C                             %subst(vlrppt:posDaDft+1:
080100030822     C                             posADft-posDaDft-1):1:3)
080200030822     C                             = 'CTM'
080300030822     C                   EVAL      VABCTM=%trim(%subst(
080400030822     C                             %subst(vlrppt:posDaDft+1:
080500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080600030822     C                   ENDIF
080700030822     C* FFD
080800030822     C                   IF        %subst(
080900030822     C                             %subst(vlrppt:posDaDft+1:
081000030822     C                             posADft-posDaDft-1):1:3)
081100030822     C                             = 'FFD'
081200030822     C                   EVAL      VABFFD=%trim(%subst(
081300030822     C                             %subst(vlrppt:posDaDft+1:
081400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081500030822     C                   ENDIF
081600030822     C* VAD
081700030822     C                   IF        %subst(
081800030822     C                             %subst(vlrppt:posDaDft+1:
081900030822     C                             posADft-posDaDft-1):1:3)
082000030822     C                             = 'VAD'
082100030822     C                   EVAL      VABVAD=%trim(%subst(
082200030822     C                             %subst(vlrppt:posDaDft+1:
082300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082400030822     C                   ENDIF
082500030822     C* GMA
082600030822     C                   IF        %subst(
082700030822     C                             %subst(vlrppt:posDaDft+1:
082800030822     C                             posADft-posDaDft-1):1:3)
082900030822     C                             = 'GMA'
083000030822     C                   EVAL      VABGMA=%trim(%subst(
083100030822     C                             %subst(vlrppt:posDaDft+1:
083200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083300030822     C                   ENDIF
083400030822     C* GGA
083500030822     C                   IF        %subst(
083600030822     C                             %subst(vlrppt:posDaDft+1:
083700030822     C                             posADft-posDaDft-1):1:3)
083800030822     C                             = 'GGA'
083900030822     C                   EVAL      VABGGA=%trim(%subst(
084000030822     C                             %subst(vlrppt:posDaDft+1:
084100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084200030822     C                   ENDIF
084300030822     C* GVA
084400030822     C                   IF        %subst(
084500030822     C                             %subst(vlrppt:posDaDft+1:
084600030822     C                             posADft-posDaDft-1):1:3)
084700030822     C                             = 'GVA'
084800030822     C                   EVAL      VABGVA=%trim(%subst(
084900030822     C                             %subst(vlrppt:posDaDft+1:
085000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085100030822     C                   ENDIF
085200030822     C* TC1
085300030822     C                   IF        %subst(
085400030822     C                             %subst(vlrppt:posDaDft+1:
085500030822     C                             posADft-posDaDft-1):1:3)
085600030822     C                             = 'TC1'
085700030822     C                   EVAL      VABTC1=%trim(%subst(
085800030822     C                             %subst(vlrppt:posDaDft+1:
085900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086000030822     C                   ENDIF
086100030822     C* TC2
086200030822     C                   IF        %subst(
086300030822     C                             %subst(vlrppt:posDaDft+1:
086400030822     C                             posADft-posDaDft-1):1:3)
086500030822     C                             = 'TC2'
086600030822     C                   EVAL      VABTC2=%trim(%subst(
086700030822     C                             %subst(vlrppt:posDaDft+1:
086800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086900030822     C                   ENDIF
087000040714     C* VATTRC
087100040714     C                   IF        %subst(
087200040714     C                             %subst(vlrppt:posDaDft+1:
087300040714     C                             posADft-posDaDft-1):1:3)
087400040714     C                             = 'TRC'
087500040714     C                   EVAL      VATTRC=%trim(%subst(
087600040714     C                             %subst(vlrppt:posDaDft+1:
087700040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
087800040714     C                   ENDIF
087900030822     C* ...
088000030822     C                   ENDIF
088100030822     C                   ENDDO
088200020204     C*
088300010330     C                   ENDSR
088400010607     C*----------------------------------------------------*
088500090911     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
088600010607     C*----------------------------------------------------*
088700090623     C     IMPVAB        BEGSR
088800010607     C*
088900010607     C                   SETOFF                                       3132
089000030515     C*
089100010607     C                   MOVEL     datcor        VABAAS
089200010607     C                   MOVE      datcor        VABMGS
089300010607     C*
089400030715     C**********
089500030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
089600030715     C**********
089700030715     C                   IF        wGiro = *zeros
089800080716     C*
089900080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
090000080716     C                   EVAL      depvinDTA = vinDTA
090100040113     C*
090200040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
090300110923     C                   IF        header <> *blanks
090400110923     C                   EVAL      vindta = header
090500070823     C     ';':CharCSV   XLATE     vindta        vindta
090600040510     C                   ENDIF
090700080626     C*
090800031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
090900031201     C     minu:maiu     XLATE     vindta        vindta
091000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
091100030715     C                   DOW       posDa <= %len(%trim(vindta))
091200030715     C*
091300030715     C* Gestisco il 1° campo
091400030715     C                   IF        i = 1
091500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
091600030715     C                   EVAL      posDa = 1
091700030715     C* Eseguo lo scan x trovare la fine del primo campo
091800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091900030715     C* A questo punto "estraggo" il campo corrente
092000111118     C                   EVAL      SkSplitFLDH(i) = %trim(prefix) +
092100110923     C                                         %subst(vindta:posDa:(posA-posDa))
092200030715     C* X i campi successivi al 1°
092300030715     C                   ELSE
092400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
092500030715     C                   EVAL      posDa = posA + 1
092600030715     C* Eseguo lo scan x trovare la fine del campo corrente
092700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092800030715     C* Gestisco l'ultimo campo
092900030715     C                   IF        posA = *zeros
093000030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
093100030715     C* A questo punto "estraggo" il campo corrente
093200111118     C                   EVAL      SkSplitFLDH(i) = %trim(prefix) +
093300110923     C                                         %subst(vindta:posDa:(posA-posDa))
093400030715     C                   LEAVE
093500030715     C                   ELSE
093600030715     C* A questo punto "estraggo" il campo corrente
093700111118     C                   EVAL      SkSplitFLDH(i) = %trim(prefix) +
093800110923     C                                         %subst(vindta:posDa:(posA-posDa))
093900030715     C                   ENDIF
094000030715     C                   ENDIF
094100030715     C* Incremento il contatore d campo
094200030715     C                   EVAL      i = i +1
094300030715     C                   ENDDO
094400030715     C*
094500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
094600030715     C* delimitazione testo
094700030715     C                   EVAL      i = 1
094800111118     C                   DOW       i < %elem(SkSplitFLDH)
094900111118     C     CharTXT:' '   XLATE     SkSplitFLDH(i)SkSplitFLDH(i)
095000111118     C                   EVAL      SkSplitFLDH(i) = %trim(SkSplitFLDH(i))
095100030715     C                   EVAL      i = i + 1
095200030715     C                   ENDDO
095300030715     C                   EVAL      wGiro = 1
095400080716     C* Ripristino la 1° riga originale
095500080716     C                   EVAL      vinDTA = depvinDTA
095600030715     C                   ELSE
095700030715     C                   EVAL      wGiro = 2
095800031204     C**********
095900031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
096000031204     C* d separatore campo
096100031204     C**********
096200031204     C                   EVAL      vindta = %trim(vindta)
096300031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
096400031204     C                   EVAL      vindta = %subst(vindta:2)
096500031204     C                   ENDDO
096600031204     C*
096700031204     C                   Z-ADD     *zeros        lunghInput        4 0
096800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
096900031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
097000031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
097100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
097200031204     C                   ENDDO
097300030715     C**********
097400030715     C* Effettuo lo split del campo dati d input
097500030715     C**********
097600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
097700030715     C                   DOW       posDa <= %len(%trim(vindta))
097800030715     C*
097900030715     C* Gestisco il 1° campo
098000030715     C                   IF        i = 1
098100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
098200030715     C                   EVAL      posDa = 1
098300030715     C* Eseguo lo scan x trovare la fine del primo campo
098400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
098500030715     C* A questo punto "estraggo" il campo corrente
098600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
098700030715     C                                                    (posA-posDa))
098800030715     C* X i campi successivi al 1°
098900030715     C                   ELSE
099000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
099100030715     C                   EVAL      posDa = posA + 1
099200031204     C* Verifico che nn vi sia il campo nn valorizzato
099300031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
099400031204     C* Se campo nn valorizzato skippo al prossimo
099500031204     C                   EVAL      SkSplitCSV(i) = *blanks
099600031204     C                   EVAL      posA  = posA + 1
099700031204     C                   ELSE
099800030715     C* Eseguo lo scan x trovare la fine del campo corrente
099900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
100000030715     C* Gestisco l'ultimo campo
100100030715     C                   IF        posA = *zeros
100200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
100300030715     C* A questo punto "estraggo" il campo corrente
100400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
100500030715     C                                                    (posA-posDa))
100600030715     C                   LEAVE
100700030715     C                   ELSE
100800030715     C* A questo punto "estraggo" il campo corrente
100900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
101000030715     C                                                    (posA-posDa))
101100030715     C                   ENDIF
101200031204     C                   ENDIF
101300030715     C                   ENDIF
101400030715     C* Incremento il contatore d campo
101500030715     C                   EVAL      i = i +1
101600030715     C                   ENDDO
101700030715     C*
101800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
101900030715     C* delimitazione testo
102000030715     C                   EVAL      i = 1
102100030715     C                   DOW       i < %elem(SkSplitCSV)
102200030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
102300030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
102400030715     C                   EVAL      i = i + 1
102500030715     C                   ENDDO
102600030715     C*
102700030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
102800030715     C                   EVAL      i = 1
102900111118     C                   DOW       i < %elem(SkSplitFLDH)
103000030715     C*
103100030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
103200111118     C                   IF        SkSplitFLDH(i) <> *blanks AND
103300090623     C                             SkSplitCSV(i) <> *blanks
103400090623     C*
103500111118     C                   exsr      CARVALH
103600040510     C*
103700030715     C                   ENDIF
103800030715     C                   EVAL      i = i + 1
103900030715     C                   ENDDO
104000070925     C*
104100071029     C***  ===>  Gestione campi "particolari" fuori ciclo
104200080125     C*
104300111117     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
104400111117     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
104500070925     C*
104600071029     C***  <===  -----------------------------------------
104700070925     C*
104800070925     C* Considerazioni finali su CBO/CAS
104900080125     C                   IF        wFlgCAS = '1'
105000070925     C                   IF        VABCBO = '1'
105100070925     C                   EVAL      VABCBO = '4'
105200070925     C                   ENDIF
105300070925     C                   IF        VABCBO = '2'
105400070925     C                   EVAL      VABCBO = '6'
105500070925     C                   ENDIF
105600070925     C                   ENDIF
105700070925     C***  <===  ----------------------------
105800040510     C*
105900040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
106000040510     C                   IF        VABNSP = *zeros
106100040510     C                   SETON                                        31
106200040510     C                   ENDIF
106300040715     C*
106400040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
106500040715     C                   IF        VABLNP = *zeros
106600040715     C                   SETON                                        31
106700040715     C                   ENDIF
106800040510     C*
106900040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
107000040510     C                   IF        VABRMN = *zeros
107100040510     C                   Z-ADD     VABNSP        VABRMN
107200040510     C                   ENDIF
107300040510     C*
107400040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
107500040510     C                   IF        VABRMA = *blanks
107600040714     C                   MOVEL     VABRMN        VABRMA
107700040510     C                   ENDIF
107800040128     C*
107900040510     C* Se provincia nn valorizzata la reperisco
108000040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
108100040510     C                   IF        VABPRD = *blanks
108200040128     C                   CLEAR                   TISI95DS
108300040128     C                   EVAL      I95TCN = '3'
108400040128     C                   Z-ADD     datcor        I95DAT
108500040128     C                   EVAL      I95NAR = VABNZD
108600040128     C                   EVAL      I95CAP = VABCAD
108700040128     C                   EVAL      I95LOC = VABLOD
108800040128     C                   CALL      'TISI95R'
108900040128     C                   PARM                    TISI95DS
109000040128     C                   EVAL      VABPRD = O95PRV
109100040510     C                   ENDIF
109200020204     C*
109300020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
109400020204     C                   EXSR      CHKIMPDIV
109500030715     C                   ENDIF
109600020204     C*
109700010607     C                   ENDSR
109800010607     C*----------------------------------------------------*
109900090623
110000090623
110100090623
110200090623     C*----------------------------------------------------*
110300101005     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
110400090623     C*----------------------------------------------------*
110500101005     C     IMPVAT        BEGSR
110600090623     C*
110700090623     C**********
110800090623     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
110900090623     C**********
111000090623     C                   IF        wGiro = *zeros
111100090623     C*
111200090623     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
111300090623     C                   EVAL      depvinDTA = vinDTA
111400090623     C*
111500090623     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
111600110923     C                   IF        parcel <> *blanks
111700110923     C                   EVAL      vindta = parcel
111800090623     C     ';':CharCSV   XLATE     vindta        vindta
111900090623     C                   ENDIF
112000090623     C*
112100090623     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
112200090623     C     minu:maiu     XLATE     vindta        vindta
112300090623     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
112400090623     C                   DOW       posDa <= %len(%trim(vindta))
112500090623     C*
112600090623     C* Gestisco il 1° campo
112700090623     C                   IF        i = 1
112800090623     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
112900090623     C                   EVAL      posDa = 1
113000090623     C* Eseguo lo scan x trovare la fine del primo campo
113100090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
113200090623     C* A questo punto "estraggo" il campo corrente
113300111118     C                   EVAL      SkSplitFLDP(i) = %trim(prefix) +
113400110923     C                                         %subst(vindta:posDa:(posA-posDa))
113500090623     C* X i campi successivi al 1°
113600090623     C                   ELSE
113700090623     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
113800090623     C                   EVAL      posDa = posA + 1
113900090623     C* Eseguo lo scan x trovare la fine del campo corrente
114000090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
114100090623     C* Gestisco l'ultimo campo
114200090623     C                   IF        posA = *zeros
114300090623     C                   EVAL      posA = %len(%trim(vindta)) + 1
114400090623     C* A questo punto "estraggo" il campo corrente
114500111118     C                   EVAL      SkSplitFLDP(i) = %trim(prefix) +
114600110923     C                                         %subst(vindta:posDa:(posA-posDa))
114700090623     C                   LEAVE
114800090623     C                   ELSE
114900090623     C* A questo punto "estraggo" il campo corrente
115000111118     C                   EVAL      SkSplitFLDP(i) = %trim(prefix) +
115100110923     C                                         %subst(vindta:posDa:(posA-posDa))
115200090623     C                   ENDIF
115300090623     C                   ENDIF
115400090623     C* Incremento il contatore d campo
115500090623     C                   EVAL      i = i +1
115600090623     C                   ENDDO
115700090623     C*
115800090623     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
115900090623     C* delimitazione testo
116000090623     C                   EVAL      i = 1
116100111118     C                   DOW       i < %elem(SkSplitFLDP)
116200111118     C     CharTXT:' '   XLATE     SkSplitFLDP(i)SkSplitFLDP(i)
116300111118     C                   EVAL      SkSplitFLDP(i) = %trim(SkSplitFLDP(i))
116400090623     C                   EVAL      i = i + 1
116500090623     C                   ENDDO
116600090623     C                   EVAL      wGiro = 1
116700090623     C* Ripristino la 1° riga originale
116800090623     C                   EVAL      vinDTA = depvinDTA
116900090623     C                   ELSE
117000090623     C                   EVAL      wGiro = 2
117100090623     C**********
117200090623     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
117300090623     C* d separatore campo
117400090623     C**********
117500090623     C                   EVAL      vindta = %trim(vindta)
117600090623     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
117700090623     C                   EVAL      vindta = %subst(vindta:2)
117800090623     C                   ENDDO
117900090623     C*
118000090623     C                   Z-ADD     *zeros        lunghInput        4 0
118100090623     C                   EVAL      lunghInput = %len(%trim(vindta))
118200090623     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
118300090623     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
118400090623     C                   EVAL      lunghInput = %len(%trim(vindta))
118500090623     C                   ENDDO
118600090623     C**********
118700090623     C* Effettuo lo split del campo dati d input
118800090623     C**********
118900090623     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
119000090623     C                   DOW       posDa <= %len(%trim(vindta))
119100090623     C*
119200090623     C* Gestisco il 1° campo
119300090623     C                   IF        i = 1
119400090623     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
119500090623     C                   EVAL      posDa = 1
119600090623     C* Eseguo lo scan x trovare la fine del primo campo
119700090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
119800090623     C* A questo punto "estraggo" il campo corrente
119900090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
120000090623     C                                                    (posA-posDa))
120100090623     C* X i campi successivi al 1°
120200090623     C                   ELSE
120300090623     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
120400090623     C                   EVAL      posDa = posA + 1
120500090623     C* Verifico che nn vi sia il campo nn valorizzato
120600090623     C                   IF        %subst(vindta:posDa:1) = CharCSV
120700090623     C* Se campo nn valorizzato skippo al prossimo
120800090623     C                   EVAL      SkSplitCSV(i) = *blanks
120900090623     C                   EVAL      posA  = posA + 1
121000090623     C                   ELSE
121100090623     C* Eseguo lo scan x trovare la fine del campo corrente
121200090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
121300090623     C* Gestisco l'ultimo campo
121400090623     C                   IF        posA = *zeros
121500090623     C                   EVAL      posA = %len(%trim(vindta)) + 1
121600090623     C* A questo punto "estraggo" il campo corrente
121700090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
121800090623     C                                                    (posA-posDa))
121900090623     C                   LEAVE
122000090623     C                   ELSE
122100090623     C* A questo punto "estraggo" il campo corrente
122200090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
122300090623     C                                                    (posA-posDa))
122400090623     C                   ENDIF
122500090623     C                   ENDIF
122600090623     C                   ENDIF
122700090623     C* Incremento il contatore d campo
122800090623     C                   EVAL      i = i +1
122900090623     C                   ENDDO
123000090623     C*
123100090623     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
123200090623     C* delimitazione testo
123300090623     C                   EVAL      i = 1
123400090623     C                   DOW       i < %elem(SkSplitCSV)
123500090623     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
123600090623     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
123700090623     C                   EVAL      i = i + 1
123800090623     C                   ENDDO
123900090623     C*
124000090623     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
124100090623     C                   EVAL      i = 1
124200111118     C                   DOW       i < %elem(SkSplitFLDP)
124300090623     C*
124400090623     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
124500111118     C                   IF        SkSplitFLDP(i) <> *blanks AND
124600090623     C                             SkSplitCSV(i) <> *blanks
124700090623     C*
124800111118     C                   exsr      CARVALP
124900090623     C*
125000090623     C                   ENDIF
125100090623     C                   EVAL      i = i + 1
125200090623     C                   ENDDO
125300090623     C*
125400090623     C***  ===>  Gestione campi "particolari" fuori ciclo
125500111118     C*
125600111118     C                   EVAL      wVATNOT_E = %trim(wVATNOT_E_1) +
125700111118     C                                         %trim(wVATNOT_E_2) +
125800111118     C                                         %trim(wVATNOT_E_3) +
125900111118     C                                         %trim(wVATNOT_E_4)
126000090623     C*
126100090623     C***  <===  -----------------------------------------
126200090623     C*
126300090623     C                   ENDIF
126400090623     C*
126500090623     C                   ENDSR
126600090623     C*----------------------------------------------------*
126700090623
126800090623
126900090623     C*----------------------------------------------------*
127000090623     C*  ROUTINE DI VALORIZZAZIONE CAMPI
127100090623     C*----------------------------------------------------*
127200111118     C     CARVALH       BEGSR
127300090623     C*
127400090623     C*** ATB
127500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABATB'
127600090623     C                   EVAL      VABATB = SkSplitCSV(i)
127700090623     C                   ENDIF
127800090623     C*** RSD
127900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RNAME1'
128000090623     C                   EVAL      VABRSD = SkSplitCSV(i)
128100090623     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
128200090623     C     '@':'A'       XLATE     VABRSD        VABRSD
128300090623     C* ==
128400090623     C                   ENDIF
128500090623     C*** RD2
128600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RNAME2'
128700090623     C                   EVAL      VABRD2 = SkSplitCSV(i)
128800090623     C                   ENDIF
128900111117     C*** IND 1
129000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RSTREET'
129100090623     C                   EVAL      VABIND = SkSplitCSV(i)
129200090623     C                   ENDIF
129300111117     C*** IND 2
129400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RHOUSENO'
129500111118     C                   EVAL      VABIND = %trim(VABIND) + ' ' + SkSplitCSV(i)
129600111117     C                   ENDIF
129700111117     C*** CAD
129800111216     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RPOSTAL'
129900111117     C                   EVAL      VABCAD = SkSplitCSV(i)
130000111216     C                   MOVEL     *blanks       wAlfa7            7
130100111216     C                   MOVEL     *zeros        wAlfa7z           7
130200111216     C                   EVALR     wAlfa7 = wAlfa7z + %trim(VABCAD)
130300111216     C                   EVAL      wVATNOT_E_1 = '%'+wAlfa7
130400111117     C                   ENDIF
130500090623     C*** LOD
130600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCITY'
130700090623     C                   EVAL      VABLOD = SkSplitCSV(i)
130800090623     C                   ENDIF
130900111117     C*** PRD
131000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABPRD'
131100090623     C                   EVAL      VABPRD = SkSplitCSV(i)
131200090623     C                   ENDIF
131300090623     C*** NZD
131400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNZD'
131500090623     C                   EVAL      VABNZD = SkSplitCSV(i)
131600090623     C                   ENDIF
131700090623     C*** RMA
131800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSCREF1'
131900111117     C                   EVAL      VABRMA = %subst(SkSplitCSV(i):4:14)
132000090623     C                   ENDIF
132100111117     C*** NOT
132200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_SCOMMENT'
132300111117     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' + SkSplitCSV(i)
132400090623     C                   ENDIF
132500111117     C*** NT2
132600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCOMMENT'
132700111117     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' + SkSplitCSV(i)
132800090623     C                   ENDIF
132900090623     C*** VCA
133000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABVCA'
133100090623     C                   EVAL      VABVCA = SkSplitCSV(i)
133200090623     C                   ENDIF
133300090623     C*** VAS
133400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABVAS'
133500090623     C                   EVAL      VABVAS = SkSplitCSV(i)
133600090623     C                   ENDIF
133700090623     C*** FFD
133800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABFFD'
133900090623     C                   EVAL      VABFFD = SkSplitCSV(i)
134000090623     C                   ENDIF
134100090623     C*** GC1
134200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGC1'
134300090623     C                   EVAL      VABGC1 = SkSplitCSV(i)
134400090623     C                   ENDIF
134500090623     C*** GC2
134600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGC2'
134700090623     C                   EVAL      VABGC2 = SkSplitCSV(i)
134800090623     C                   ENDIF
134900090623     C*** TSP
135000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTSP'
135100090623     C                   EVAL      VABTSP = SkSplitCSV(i)
135200090623     C                   ENDIF
135300090623     C*** CBO
135400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCBO'
135500090623     C                   EVAL      VABCBO = SkSplitCSV(i)
135600090623     C                   ENDIF
135700090623     C*** NAS
135800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNAS'
135900090623     C                   EVAL      VABNAS = SkSplitCSV(i)
136000090623     C                   ENDIF
136100090623     C*** TIC
136200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTIC'
136300090623     C                   EVAL      VABTIC = SkSplitCSV(i)
136400090623     C                   ENDIF
136500090623     C*** GCA
136600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGCA'
136700090623     C                   EVAL      VABGCA = SkSplitCSV(i)
136800090623     C                   ENDIF
136900090623     C*** XCO
137000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABXCO'
137100090623     C                   EVAL      VABXCO = SkSplitCSV(i)
137200090623     C                   ENDIF
137300090623     C*** CTM
137400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCTM'
137500090623     C                   EVAL      VABCTM = SkSplitCSV(i)
137600090623     C                   ENDIF
137700090623     C*** TCR
137800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTCR'
137900090623     C                   EVAL      VABTCR = SkSplitCSV(i)
138000090623     C                   ENDIF
138100090623     C*** CTS
138200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCTS'
138300090623     C                   EVAL      VABCTS = SkSplitCSV(i)
138400090623     C                   ENDIF
138500090623     C*** FTM
138600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABFTM'
138700090623     C                   EVAL      VABFTM = SkSplitCSV(i)
138800090623     C                   ENDIF
138900090623     C*** VAD
139000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABVAD'
139100090623     C                   EVAL      VABVAD = SkSplitCSV(i)
139200090623     C                   ENDIF
139300090623     C*** GMA
139400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGMA'
139500090623     C                   EVAL      VABGMA = SkSplitCSV(i)
139600090623     C                   ENDIF
139700090623     C*** GGA
139800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGGA'
139900090623     C                   EVAL      VABGGA = SkSplitCSV(i)
140000090623     C                   ENDIF
140100090623     C*** GVA
140200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGVA'
140300090623     C                   EVAL      VABGVA = SkSplitCSV(i)
140400090623     C                   ENDIF
140500090623     C*** TC1
140600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTC1'
140700090623     C                   EVAL      VABTC1 = SkSplitCSV(i)
140800090623     C                   ENDIF
140900090623     C*** TC2
141000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTC2'
141100090623     C                   EVAL      VABTC2 = SkSplitCSV(i)
141200090623     C                   ENDIF
141300090623     C*** SCL
141400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABSCL'
141500090623     C                   EVAL      VABSCL = SkSplitCSV(i)
141600090623     C                   ENDIF
141700090623     C*** RMO
141800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABRMO'
141900090623     C                   EVAL      VABRMO = SkSplitCSV(i)
142000090623     C                   ENDIF
142100090623     C*** NMO
142200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNMO'
142300090623     C                   EVAL      VABNMO = SkSplitCSV(i)
142400090623     C                   ENDIF
142500090623     C*
142600090623     C* Reperisco quindi i campi numerici...
142700111117     C*** NSP
142800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSID'
142900111117     C                   EVAL      PiStr=%subst(SkSplitCSV(i):4:14)
143000090623     C                   EXSR      CHKNUM
143100090623     C                   IF        PiInt=*on
143200090623     C                   Z-ADD     PiVal         VABNSP
143300090623     C                   ELSE
143400090623     C                   SETON                                        31
143500090623     C                   EVAL      VABNSP = *zeros
143600090623     C                   EVAL      vinmsg = %trimr(vinmsg)
143700090623     C                             + ' ' + 'VABNSP'
143800090623     C                   ENDIF
143900090623     C                   ENDIF
144000090623     C*** RMN
144100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSID'
144200111117     C                   EVAL      PiStr=%subst(SkSplitCSV(i):4:14)
144300090623     C                   EXSR      CHKNUM
144400090623     C                   IF        PiInt=*on
144500090623     C                   Z-ADD     PiVal         VABRMN
144600090623     C                   ELSE
144700090623     C                   SETON                                        32
144800090623     C                   EVAL      VABRMN = *zeros
144900090623     C                   EVAL      vinmsg = %trimr(vinmsg)
145000090623     C                             + ' ' + 'VABRMN'
145100090623     C                   ENDIF
145200090623     C                   ENDIF
145300090623     C*** CAD
145400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCAD'
145500090623     C                   EVAL      PiStr=SkSplitCSV(i)
145600090623     C                   EXSR      CHKNUM
145700090623     C                   IF        PiInt=*on
145800090623     C                   Z-ADD     PiVal         Num5_0
145900090623     C                   MOVEL(p)  Num5_0        VABCAD
146000090623     C                   ELSE
146100090623     C                   EVAL      VABCAD = SkSplitCSV(i)
146200090623     C                   SETON                                        32
146300090623     C                   EVAL      VABCAD = *zeros
146400090623     C                   EVAL      vinmsg = %trimr(vinmsg)
146500090623     C                             + ' ' + 'VABCAD'
146600090623     C                   ENDIF
146700090623     C                   ENDIF
146800090623     C*** NCL
146900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSCOUNT'
147000090623     C                   EVAL      PiStr=SkSplitCSV(i)
147100090623     C                   EXSR      CHKNUM
147200090623     C                   IF        PiInt=*on
147300090623     C                   Z-ADD     PiVal         VABNCL
147400090623     C                   ELSE
147500090623     C                   SETON                                        32
147600090623     C                   EVAL      VABNCL = *zeros
147700090623     C                   EVAL      vinmsg = %trimr(vinmsg)
147800090623     C                             + ' ' + 'VABNCL'
147900090623     C                   ENDIF
148000090623     C                   ENDIF
148100090623     C*** PKB
148200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSWEIGHT'
148300090623     C                   EVAL      PiStr=SkSplitCSV(i)
148400090623     C                   EXSR      CHKNUM
148500090623     C                   IF        PiNum=*on
148600111118     C                   EVAL      PiVal = PiVal / 1000                         * da gr. a kg.
148700090623     C                   Z-ADD     PiVal         VABPKB
148800090623     C                   ELSE
148900090623     C                   SETON                                        32
149000090623     C                   EVAL      VABPKB = *zeros
149100090623     C                   EVAL      vinmsg = %trimr(vinmsg)
149200090623     C                             + ' ' + 'VABPKB'
149300090623     C                   ENDIF
149400090623     C                   ENDIF
149500090623     C*** CAS
149600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCAS'
149700090623     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
149800090623     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
149900090623     C                             %trim(SkSplitCSV(i)) <> '0'      AND
150000090623     C                             %trim(SkSplitCSV(i)) <> '0,00'
150100090623     C                   EVAL      wFlgCAS = '1'
150200090623     C                   ENDIF
150300090623     C                   EVAL      PiStr=SkSplitCSV(i)
150400090623     C                   EXSR      CHKNUM
150500090623     C                   IF        PiNum=*on
150600090623     C                   Z-ADD     PiVal         VABCAS
150700090623     C                   ELSE
150800090623     C                   SETON                                        32
150900090623     C                   EVAL      VABCAS = *zeros
151000090623     C                   EVAL      vinmsg = %trimr(vinmsg)
151100090623     C                             + ' ' + 'VABCAS'
151200090623     C                   ENDIF
151300090623     C                   ENDIF
151400090623     C*** CCM
151500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCCM'
151600090623     C                   EVAL      PiStr=SkSplitCSV(i)
151700090623     C                   EXSR      CHKNUM
151800090623     C                   IF        PiInt=*on      AND
151900090623     C                             PiVal<=9999999 AND
152000090623     C                             PiVal>*zeros
152100090623     C                   Z-ADD     PiVal         VABCCM
152200090623     C                   ELSE
152300090623     C                   SETON                                        32
152400090623     C                   EVAL      VABCCM = *zeros
152500090623     C                   EVAL      vinmsg = %trimr(vinmsg)
152600090623     C                             + ' ' + 'VABCCM'
152700090623     C                   ENDIF
152800090623     C                   ENDIF
152900090623     C*** LNP
153000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABLNP'
153100090623     C                   EVAL      PiStr=SkSplitCSV(i)
153200090623     C                   EXSR      CHKNUM
153300090623     C                   IF        PiInt=*on    AND
153400090623     C                             PiVal<=999   AND
153500090623     C                             PiVal>*zeros
153600090623     C                   Z-ADD     PiVal         VABLNP
153700090623     C                   ELSE
153800090623     C                   SETON                                        32
153900090623     C                   EVAL      VABLNP = *zeros
154000090623     C                   EVAL      vinmsg = %trimr(vinmsg)
154100090623     C                             + ' ' + 'VABLNP'
154200090623     C                   ENDIF
154300090623     C                   ENDIF
154400090623     C*** NRS
154500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNRS'
154600090623     C                   EVAL      PiStr=SkSplitCSV(i)
154700090623     C                   EXSR      CHKNUM
154800090623     C                   IF        PiInt=*on AND
154900090623     C                             PiVal<=99
155000090623     C                   Z-ADD     PiVal         VABNRS
155100090623     C                   ELSE
155200090623     C                   SETON                                        32
155300090623     C                   EVAL      vinmsg = %trimr(vinmsg)
155400090623     C                             + ' ' + 'VABNRS'
155500090623     C                   ENDIF
155600090623     C                   ENDIF
155700090623     C*** LNA
155800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABLNA'
155900090623     C                   EVAL      PiStr=SkSplitCSV(i)
156000090623     C                   EXSR      CHKNUM
156100090623     C                   IF        PiInt=*on    AND
156200090623     C                             PiVal<=999   AND
156300090623     C                             PiVal>*zeros
156400090623     C                   Z-ADD     PiVal         VABLNA
156500090623     C                   ELSE
156600090623     C                   SETON                                        32
156700090623     C                   EVAL      VABLNA = *zeros
156800090623     C                   EVAL      vinmsg = %trimr(vinmsg)
156900090623     C                             + ' ' + 'VABLNA'
157000090623     C                   ENDIF
157100090623     C                   ENDIF
157200090623     C*** CTR
157300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCTR'
157400090623     C                   EVAL      PiStr=SkSplitCSV(i)
157500090623     C                   EXSR      CHKNUM
157600090623     C                   IF        PiInt=*on  AND
157700090623     C                             PiVal<=999
157800090623     C                   Z-ADD     PiVal         VABCTR
157900090623     C                   ELSE
158000090623     C                   SETON                                        32
158100090623     C                   EVAL      vinmsg = %trimr(vinmsg)
158200090623     C                             + ' ' + 'VABCTR'
158300090623     C                   ENDIF
158400090623     C                   ENDIF
158500090623     C*** IAS
158600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABIAS'
158700090623     C                   EVAL      PiStr=SkSplitCSV(i)
158800090623     C                   EXSR      CHKNUM
158900090623     C                   IF        PiNum=*on
159000090623     C                   Z-ADD     PiVal         VABIAS
159100090623     C                   ELSE
159200090623     C                   SETON                                        32
159300090623     C                   EVAL      vinmsg = %trimr(vinmsg)
159400090623     C                             + ' ' + 'VABIAS'
159500090623     C                   ENDIF
159600090623     C                   ENDIF
159700090623     C*** VLB
159800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSVOLUME'
159900090623     C                   EVAL      PiStr=SkSplitCSV(i)
160000090623     C                   EXSR      CHKNUM
160100090623     C                   IF        PiNum=*on
160200111117     C                   EVAL      PiVal = PiVal / 1000000                      * da cm3 a m3
160300090623     C                   Z-ADD     PiVal         VABVLB
160400090623     C                   ELSE
160500090623     C                   SETON                                        32
160600090623     C                   EVAL      vinmsg = %trimr(vinmsg)
160700090623     C                             + ' ' + 'VABVLB'
160800090623     C                   ENDIF
160900090623     C                   ENDIF
161000090623     C*** QFT
161100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABQFT'
161200090623     C                   EVAL      PiStr=SkSplitCSV(i)
161300090623     C                   EXSR      CHKNUM
161400090623     C                   IF        PiNum=*on
161500090623     C                   Z-ADD     PiVal         VABQFT
161600090623     C                   ELSE
161700090623     C                   SETON                                        32
161800090623     C                   EVAL      vinmsg = %trimr(vinmsg)
161900090623     C                             + ' ' + 'VABQFT'
162000090623     C                   ENDIF
162100090623     C                   ENDIF
162200090623     C*** NCD
162300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNCD'
162400090623     C                   EVAL      PiStr=SkSplitCSV(i)
162500090623     C                   EXSR      CHKNUM
162600090623     C                   IF        PiInt=*on      AND
162700090623     C                             PiVal<=9999999
162800090623     C                   Z-ADD     PiVal         VABNCD
162900090623     C                   ELSE
163000090623     C                   SETON                                        32
163100090623     C                   EVAL      vinmsg = %trimr(vinmsg)
163200090623     C                             + ' ' + 'VABNCD'
163300090623     C                   ENDIF
163400090623     C                   ENDIF
163500090623     C*** NCA
163600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNCA'
163700090623     C                   EVAL      PiStr=SkSplitCSV(i)
163800090623     C                   EXSR      CHKNUM
163900090623     C                   IF        PiInt=*on      AND
164000090623     C                             PiVal<=9999999
164100090623     C                   Z-ADD     PiVal         VABNCA
164200090623     C                   ELSE
164300090623     C                   SETON                                        32
164400090623     C                   EVAL      vinmsg = %trimr(vinmsg)
164500090623     C                             + ' ' + 'VABNCA'
164600090623     C                   ENDIF
164700090623     C                   ENDIF
164800090623     C*** ZNC
164900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABZNC'
165000090623     C                   EVAL      PiStr=SkSplitCSV(i)
165100090623     C                   EXSR      CHKNUM
165200090623     C                   IF        PiInt=*on AND
165300090623     C                             PiVal<=99
165400090623     C                   Z-ADD     PiVal         VABZNC
165500090623     C                   ELSE
165600090623     C                   SETON                                        32
165700090623     C                   EVAL      vinmsg = %trimr(vinmsg)
165800090623     C                             + ' ' + 'VABZNC'
165900090623     C                   ENDIF
166000090623     C                   ENDIF
166100090623     C*** DCR
166200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABDCR'
166300090623     C                   EVAL      PiStr=SkSplitCSV(i)
166400090623     C                   EXSR      CHKNUM
166500090623     C                   IF        PiInt=*on       AND
166600090623     C                             PiVal<=99999999
166700090623     C                   Z-ADD     PiVal         VABDCR
166800090623     C                   ELSE
166900090623     C                   SETON                                        32
167000090623     C                   EVAL      vinmsg = %trimr(vinmsg)
167100090623     C                             + ' ' + 'VABDCR'
167200090623     C                   ENDIF
167300090623     C                   ENDIF
167400090623     C*** HCR
167500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABHCR'
167600090623     C                   EVAL      PiStr=SkSplitCSV(i)
167700090623     C                   EXSR      CHKNUM
167800090623     C                   IF        PiInt=*on   AND
167900090623     C                             PiVal<=9999
168000090623     C                   Z-ADD     PiVal         VABHCR
168100090623     C                   ELSE
168200090623     C                   SETON                                        32
168300090623     C                   EVAL      vinmsg = %trimr(vinmsg)
168400090623     C                             + ' ' + 'VABHCR'
168500090623     C                   ENDIF
168600090623     C                   ENDIF
168700090623     C*** VMD
168800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABVMD'
168900090623     C                   EVAL      PiStr=SkSplitCSV(i)
169000090623     C                   EXSR      CHKNUM
169100090623     C                   IF        PiNum=*on
169200090623     C                   Z-ADD     PiVal         VABVMD
169300090623     C                   ELSE
169400090623     C                   SETON                                        32
169500090623     C                   EVAL      vinmsg = %trimr(vinmsg)
169600090623     C                             + ' ' + 'VABVMD'
169700090623     C                   ENDIF
169800090623     C                   ENDIF
169900090623     C*** ANT
170000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABANT'
170100090623     C                   EVAL      PiStr=SkSplitCSV(i)
170200090623     C                   EXSR      CHKNUM
170300090623     C                   IF        PiInt=*on        AND
170400090623     C                             PiVal<=999999999
170500090623     C                   Z-ADD     PiVal         VABANT
170600090623     C                   ELSE
170700090623     C                   SETON                                        32
170800090623     C                   EVAL      vinmsg = %trimr(vinmsg)
170900090623     C                             + ' ' + 'VABANT'
171000090623     C                   ENDIF
171100090623     C                   ENDIF
171200090623     C*** CMO
171300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCMO'
171400090623     C                   EVAL      PiStr=SkSplitCSV(i)
171500090623     C                   EXSR      CHKNUM
171600090623     C                   IF        PiInt=*on
171700090623     C                   Z-ADD     PiVal         Num5_0
171800090623     C                   MOVEL(p)  Num5_0        VABCMO
171900090623     C                   ELSE
172000090623     C                   SETON                                        32
172100090623     C                   EVAL      VABCMO = *zeros
172200090623     C                   EVAL      vinmsg = %trimr(vinmsg)
172300090623     C                             + ' ' + 'VABCMO'
172400090623     C                   ENDIF
172500090623     C                   ENDIF
172600090623     C*
172700090911     C* Reperisco i campi del EDIVAT
172800090623     C*** VATNOT
172900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VATNOT'
173000090623     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
173100090623     C                   ENDIF
173200090623     C*** VATTRC
173300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VATTRC'
173400090623     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
173500090623     C                   ENDIF
173600090623     C*** VATNOT_A
173700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCONTACT'
173800090623     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
173900090623     C                   ENDIF
174000090623     C*** VATNOT_B
174100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RPHONE'
174200090623     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
174300090623     C                   ENDIF
174400111118     C*** VATNOT_E_4
174500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCOUNTRYN'
174600111118     C                   EVAL      wVATNOT_E_4 = SkSplitCSV(i)
174700111118     C                   ENDIF
174800090623     C*
174900090623     C***  ===>  Gestione campi "particolari" dentro ciclo
175000090623     C*
175100090623     C*
175200090623     C***  <===  -----------------------------------------
175300090908     C*
175400090623     C*
175500090623     C                   ENDSR
175600090623     C***
175700090623
175800111118
175900111118
176000111118     C*----------------------------------------------------*
176100111118     C*  ROUTINE DI VALORIZZAZIONE CAMPI
176200111118     C*----------------------------------------------------*
176300111118     C     CARVALP       BEGSR
176400111118     C*
176500111118     C* Reperisco i campi del EDIVAT
176600111118     C*** VATNOT
176700111118     C                   IF        %trim(SkSplitFLDP(i)) = 'VATNOT'
176800111118     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
176900111118     C                   ENDIF
177000111118     C*** VATTRC
177100111118     C                   IF        %trim(SkSplitFLDP(i)) = 'VATTRC'
177200111118     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
177300111118     C                   ENDIF
177400111118     C*** VATNOT_A
177500111118     C                   IF        %trim(SkSplitFLDP(i)) = 'HDR_RCONTACT'
177600111118     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
177700111118     C                   ENDIF
177800111118     C*** VATNOT_B
177900111118     C                   IF        %trim(SkSplitFLDP(i)) = 'HDR_RPHONE'
178000111118     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
178100111118     C                   ENDIF
178200111216     C*** VATNOT_E_1
178300111216     C***                IF        %trim(SkSplitFLDP(i)) = 'PCL_CREF1'
178400111216     C***                EVAL      wVATNOT_E_1 = '%'+%subst(SkSplitCSV(i):7:4)
178500111216     C***                ENDIF
178600111118     C*** VATNOT_E_2
178700111118     C                   IF        %trim(SkSplitFLDP(i)) = 'PCL_PARCELNO'
178800111215     C                   EVAL      wVATNOT_E_2 = SkSplitCSV(i)
178900111118     C                   ENDIF
179000111118     C*** VATNOT_E_3
179100111118     C                   IF        %trim(SkSplitFLDP(i)) = 'PCL_SERVICE'
179200111118     C                   EVAL      wVATNOT_E_3 = SkSplitCSV(i)
179300111118     C                   ENDIF
179400111118     C*** VATNOT_E_4
179500111118     C                   IF        %trim(SkSplitFLDP(i)) = 'PCL_RCOUNTRYN'
179600111118     C                   EVAL      wVATNOT_E_4 = SkSplitCSV(i)
179700111118     C                   ENDIF
179800111118     C*
179900111118     C***  ===>  Gestione campi "particolari" dentro ciclo
180000111118     C*
180100111118     C*
180200111118     C***  <===  -----------------------------------------
180300111118     C*
180400111118     C*
180500111118     C                   ENDSR
180600111118     C***
180700010601
180800020204
180900020204     C*----------------------------------------------------*
181000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
181100020204     C*----------------------------------------------------*
181200020204     C     CHKIMPDIV     BEGSR
181300020204     C*
181400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
181500020204     C                   Z-ADD     *zeros        wrkDec            9 9
181600020204     C*
181700020204     C* Come prima cosa effettuo considerazioni sulla divisa
181800020204     C                   IF        vabIAS > *zeros
181900020204     C                   IF        vabVAS <> 'EUR'
182000020204     C                   EVAL      vabVAS =  'ITL'
182100020204     C                   ENDIF
182200020204     C                   ENDIF
182300020204     C*
182400020204     C                   IF        vabCAS > *zeros
182500020204     C                   IF        vabVCA <> 'EUR'
182600020204     C                   EVAL      vabVCA =  'ITL'
182700020204     C                   ENDIF
182800020204     C                   ENDIF
182900020204     C*
183000020204     C                   IF        vabVMD > *zeros
183100020204     C                   IF        vabVAD <> 'EUR'
183200020204     C                   EVAL      vabVAD =  'ITL'
183300020204     C                   ENDIF
183400020204     C                   ENDIF
183500020204     C*
183600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
183700020204     C                   Z-ADD     vabIAS        wrkDec
183800020204     C                   IF        wrkDec > *zeros
183900020204     C                   IF        vabVAS = 'ITL'
184000020204     C                   EVAL      vabIAS = *zeros
184100020204     C                   ENDIF
184200020204     C                   ENDIF
184300020204     C*
184400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
184500020204     C                   Z-ADD     vabCAS        wrkDec
184600020204     C                   IF        wrkDec > *zeros
184700020204     C                   IF        vabVCA = 'ITL'
184800020204     C                   EVAL      vabCAS = *zeros
184900020204     C                   ENDIF
185000020204     C                   ENDIF
185100020204     C*
185200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
185300020204     C                   Z-ADD     vabVMD        wrkDec
185400020204     C                   IF        wrkDec > *zeros
185500020204     C                   IF        vabVAD = 'ITL'
185600020204     C                   EVAL      vabVMD = *zeros
185700020204     C                   ENDIF
185800020204     C                   ENDIF
185900020204     C*
186000020204     C                   ENDSR
186100020204     C***
186200080617
186300080617
186400080617
186500080617     C*----------------------------------------------------*
186600080617     C*  CONTROLLO NUMERICITA' CAMPI
186700080617     C*----------------------------------------------------*
186800080617     C     CHKNUM        BEGSR
186900080617     C*
187000080617     C                   IF        PiDecChr = *blanks
187100080617     C                   EVAL      PiDecChr = CharNUM
187200080617     C                   ENDIF
187300080617     C*
187400080617     C                   callp     UBISNUM_Check(PiStr
187500080617     C                                          :PiDecChr
187600080617     C                                          :PiVal
187700080617     C                                          :PiNum
187800080617     C                                          :PiInt)
187900080617     C*
188000080617     C                   ENDSR
188100080617     C***
188200010330
188300010601
188400010601
188500010601
188600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
188700010601     C     repfil        BEGSR
188800010601     C*
188900010601     C                   if        invfil = *zeros and
189000010601     C                             depfil > *zeros and
189100010629     C                             (vinflg = *blanks or
189200010629     C                              vinflg = *zeros)
189300010601     C
189400010601     C                   eval      invfil = depfil
189500010601     C                   endif
189600010601     C*
189700010601     C                   if        depfil <> invfil and
189800010601     C                             invfil > *zeros
189900010601     C                   eval      flgMulti = '1'
190000010601     C                   if        vinflg = *blanks
190100010601     C                   add       1             cntNonEl
190200010601     C                   endif
190300010601     C                   endif
190400010601     C*
190500010601     C                   if        vinflg = '2'
190600010601     C                   eval      flgStato = '2'
190700010601     C                   endif
190800010601     C*
190900010601     C                   ENDSR
191000010601     C***
191100010601
191200010601
191300010601
191400010330
191500040119      /TITLE Invio dei dati al punto operativo.
191600040119     C     invio         BEGSR
191700040119     C*
191800101005     C* 1° invio EDIVAT
191900040119     C                   reset                   dscmz
192000040119     C                   move      vlrpoi        cmzdst
192100090911     C                   eval      cmzfld = 'EDIVATWR'
192200040119     C                   eval      cmzmbd = vlrhdl
192300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
192400040119     C***                if        prmfir = *blanks
192500090911     C                   eval      cmzfla = 'EDIVAT0F'
192600090911     C                   eval      cmzmba = 'EDIVAT0F'
192700040119     C***                else
192800040119     C***                eval      cmzfla = prmfir
192900040119     C***                eval      cmzmba = prmfir
193000040119     C***                endif
193100040119     C                   eval      cmznrr = *zeros
193200040119     C                   move      §ctrokvt      cmznrr
193300040119     C                   eval      cmzlba = vlrfl1
193400040119     C                   call(e)   'TIS711C'
193500040119     C                   parm                    dscmz
193600040119     C                   parm      *blanks       esito
193700040119     C                   if        %error
193800040119     C                             or cmzerr = '1'
193900040119     C                             or esito  = '1'
194000040119     C                   eval      wrkesito = '3'
194100040119     C                   else
194200040119     C*
194300101005     C* 2° invio EDIVAB
194400040119     C                   reset                   dscmz
194500040119     C                   move      vlrpoi        cmzdst
194600040119     C                   eval      cmzfld = vlrfou
194700040119     C                   eval      cmzmbd = vlrhdl
194800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
194900040119     C***                if        prmfir = *blanks
195000090911     C                   eval      cmzfla = 'EDIVAB0F'
195100090911     C                   eval      cmzmba = 'EDIVAB0F'
195200040119     C***                else
195300040119     C***                eval      cmzfla = prmfir
195400040119     C***                eval      cmzmba = prmfir
195500040119     C***                endif
195600040119     C                   eval      cmznrr = *zeros
195700040119     C                   move      §ctrokvb      cmznrr
195800040119     C                   eval      cmzlba = vlrfl1
195900040119     C                   call(e)   'TIS711C'
196000040119     C                   parm                    dscmz
196100040119     C                   parm      *blanks       esito
196200040119     C                   if        %error
196300040119     C                             or cmzerr = '1'
196400040119     C                             or esito  = '1'
196500040119     C                   eval      wrkesito = '3'
196600040119     C                   endif
196700040119     C                   endif
196800040119     C*
196900040119     C                   ENDSR
197000040119     C***
197100010601
197200010601
197300010601
197400010601
197500010601      /TITLE Invio dei dati al punto operativo.
197600010601     C     opeini        BEGSR
197700010601     C*
197800010601     C* Inizializzo flag e contatori operativi
197900010601     C                   movel     '0'           flgGiro           1
198000010601     C                   movel     '0'           flgMulti          1
198100010601     C                   movel     '1'           flgStato          1
198200010615     C                   movel     '0'           flgOk             1
198300010601     C                   z-add     *zeros        cntNonEl         10 0
198400010601     C                   z-add     *zeros        depfil            3 0
198500010601     C                   z-add     *zeros        invfil            3 0
198600010601     C*
198700010601     C                   ENDSR
198800010601     C***
198900070326
199000070326
199100070326
199200070326
199300080916     C     *pssr         BEGSR
199400070329     C*
199500080916     C                   if        %open(tivin00r)
199600080916     C                   close     tivin00r
199700080916     C                   endif
199800090911     C                   if        %open(edivabwr)
199900090911     C                   close     edivabwr
200000080916     C                   endif
200100090911     C                   if        %open(edivatwr)
200200090911     C                   close     edivatwr
200300080916     C                   endif
200400070326     C*
200500070326     C* Effettuo la chiamata al CLLE preposto
200600101005     C                   call(e)   'TITVEVTC'
200700080916     C                   parm                    parccm
200800080916     C                   parm                    parmbr
200900080916     C                   parm      '2'           paropz
201000070326     C*
201100080916     C                   eval      wrkesito = '2'
201200070404     C*
201300080916     C                   seton                                        LR
201400070326     C*
201500080916     C                   ENDSR     '*CANCL'
201600070326     C***
201700070326
201800070326
201900010330
202000010330
202100000613     C     *inzsr        BEGSR
202200990910     C*
202300990910     C     *entry        plist
202400990920     C                   parm                    tivlrds
202500990921     C                   parm      wrkesito      esito
202600000724     C                   parm                    prmlit
202700000710     C                   parm                    prmfir
202800010330     C*
202900010330     C* CALCOLA LA DATA CORRENTE
203000010330     C                   time                    wn14             14 0
203100090911     C                   movel     wn14          oracor            6 0          *ORA
203200110923     C                   z-add     *zeros        datcor            8 0
203300110923     C                   eval      datcor = %dec(%date() : *ISO)
203400111118     C*
203500111118     C* Forzo un numero CMR
203600111118     C                   eval      wCMR = 'DPD NL '+%char(datcor)+%char(oracor)
203700000613     C*
203800000613     C                   ENDSR
203900000613     C***
204000990908
204100090623     Otitv1q7p  E            riepilogo         2
204200990915     O                                              'Upload via Internet'
204300990915     O                                           +1 'Traduzione TIVIN00R -'
204400090911     O                                           +1 'EDIVABWR'
204500990915     O                                           +1 'Testate bolle'
204600990915     O          E            riepilogo   2
204700990915     O                       wrkdata
204800990915     O                       wrkora              +1
204900990915     O                       procname            +1
205000990915     O          E            riepilogo   2
205100990915     O                                              'Cliente..................:'
205200010605     O                       VABCCM        z     +1
205300990915     O          E            riepilogo   2
205400990920     O                                              'Riferimento Strategi.....:'
205500990920     O                       vlrhdl              +1
205600990915     O          E            riepilogo   2
205700990915     O                                              'Giusti...................:'
205800040119     O                       §CTROKVB      2   +  1
205900990915     O          E            riepilogo   2
206000990915     O                                              'Sbagliati e corretti.....:'
206100971022     O                       §CTRMO        2   +  1
206200990915     O          E            riepilogo   2
206300990915     O                                              'Sbagliati e scartati.....:'
206400971022     O                       §CTRNO        2   +  1
206500000613
206600090623     Otitv1q7ps E            testdett          2
206700000613     O                                              'Upload via Internet'
206800000613     O                                           +1 'Traduzione TIVIN00R -'
206900090911     O                                           +1 'EDIVABWR'
207000000613     O                                           +1 'Testate bolle'
207100000616     O          E            testdett    3
207200000613     O                                           +2 'N° rec'
207300000613     O                                           +3 'Anteprima contenuto'
207400000616     O          E            rigadett    2
207500000613     O                       rrnum               +2
207600000621     O                       recko               +3
207700000616     O          E            findett     2
207800000613     O                       wrkdata
207900000613     O                       wrkora              +1
208000000613     O                       procname            +1
208100000616     O          E            findett     2
208200000613     O                                              'Cliente..................:'
208300010605     O                       VABCCM        z     +1
208400000616     O          E            findett     2
208500000613     O                                              'Riferimento Strategi.....:'
208600000613     O                       vlrhdl              +1
208700000616     O          E            findett     2
208800000613     O                                              'Giusti...................:'
208900040119     O                       §CTROKVB      2   +  1
209000000616     O          E            findett     2
209100000613     O                                              'Sbagliati e corretti.....:'
209200000613     O                       §CTRMO        2   +  1
209300000616     O          E            findett     2
209400000613     O                                              'Sbagliati e scartati.....:'
209500000613     O                       §CTRNO        2   +  1
209600000613** CMD - COMANDI CL
209700090623OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1Q7P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
209800090623OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1Q7PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
209900090623DLTOVR FILE(TITV1Q7P TITV1Q7PS) LVL(*)
210000010606
210100000613
