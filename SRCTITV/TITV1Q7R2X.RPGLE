000100101004      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR
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
002000040113     D* SCHIEDE A PROGRAMMA
002100040113     D*------------
002200090623     D entexp          S            100    DIM(5) CTDATA PERRCD(1)
002300090623     D ligexp          S            100    DIM(5) CTDATA PERRCD(1)
002400090624     D prtconst        S            100    DIM(5) CTDATA PERRCD(1)
002500010330     D*----------------------------------------------------
002600010330     D* DICHIARAZIOINE VARIABILI DI WRK
002700010330     D*----------------------------------------------------
002800010330     D dscmz         e ds                  inz
002900010330     D psds           sds
003000010330     D  procname         *PROC
003100010330     D tivlrds       e ds                  extname(tivlr00f)
003200040128     D tisi95ds      e ds
003300010330     D esito           s              1
003400010330     D prmlit          s             10
003500010330     D prmfir          s             10
003600010330     D wrkesito        s                   like(esito)
003700010330     D wrkdata         s               d
003800010330     D wrkora          s               t
003900010330     D rrnum           s              6  0 INZ(*zeros)
004000010330     D recko           s            150    INZ(*blanks)
004100011113     D depcmd          s            150    INZ(*blanks)
004200040119     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004300040510     D parccm          s              8    INZ(*blanks)
004400040119     D parmbr          s             10    INZ(*blanks)
004500040119     D paropz          s              1    INZ(*blanks)
004600040119     D chkcall         s              1    INZ(*blanks)
004700080125     D wFlgCAS         s              1    INZ(*blanks)
004800080716     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
004900090911     D wNomeFile       s             30    INZ(*blanks)
005000090911     D wCMR            s             35    INZ(*blanks)
005100030715     D*------------------
005200030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005300030715     D*------------------
005400090624     D SkSplitFLD      S             15    DIM(100)
005500030715     D SkSplitCSV      S            256    DIM(100)
005600030715     D CharCSV         S              1
005700030715     D CharTXT         S              1
005800030715     D CharNUM         S              1
005900030715     D posDa           S              3  0 INZ(*zeros)
006000030715     D posA            S              3  0 INZ(*zeros)
006100030715     D i               s              3  0 INZ(1)
006200030715     D wGiro           s              1  0 INZ(*zeros)
006300080125     D wVATNOT_A       s                   LIKE(VATNOT)
006400080125     D wVATNOT_B       s                   LIKE(VATNOT)
006500090904     D wVATNOT_E       s                   LIKE(VATNOT)
006600090904     D wVATNOT_E_1     s                   LIKE(VATNOT)
006700090904     D wVATNOT_E_2     s                   LIKE(VATNOT)
006800030822     D*------------------
006900030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
007000030822     D*------------------
007100030822     D posDaDft        S              3  0 INZ(*zeros)
007200030822     D posADft         S              3  0 INZ(*zeros)
007300030822     D j               s              3  0 INZ(1)
007400030822     D wGiroDft        s              1  0 INZ(*zeros)
007500010330     D*------------------
007600010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
007700010330     D*------------------
007800010330     D WLBDA8          DS                  INZ
007900010330     D  G08DAT                 1      8  0
008000010330     D  G08INV                 9     16  0
008100010330     D  G08ERR                17     17
008200010330     D  G08TGI                18     22  0
008300031201     D*------------------
008400031201     D* Costanti
008500031201     D*------------------
008600031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008700031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008800080617     D*------------------
008900080617
009000080617     D*------------------
009100080617     D* LINKING A DEFINIZIONI ESTERNE
009200080617     D*------------------
009300080617     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009400090220     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009500010330
009600010330
009700010330
009800990915     C                   time                    wrkdata
009900990915     C                   time                    wrkora
010000000913     C                   reset                   rrnum
010100990921     C                   reset                   esito
010200081014     C                   reset                   wrkesito
010300010601     C*
010400010601     C                   exsr      opeini
010500010605     C                   exsr      rwvab
010600081014     C*
010700081014     C                   exsr      endela
010800010601     C*
010900010601     C                   seton                                        lr
011000010601
011100010601
011200010601
011300010601     C*--------------------------------------------------------
011400010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011500010601     C*--------------------------------------------------------
011600010601     C     PREELA        BEGSR
011700010601     C*
011800010601     C* SE OCCORRE SPEDIRE IN FILIALE
011900010601     C                   if        invfil <> *zeros and
012000010601     C                             flgGiro = '0'
012100010601     C*
012200010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012300010601     C                   eval      flgGiro = '1'
012400000724     C*
012500000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
012600000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012700010601     C     invfil        chain     azorg01l
012800000724     C                   if        %found
012900000724     C                   movel(p)  CMD(1)        depcmd
013000020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
013100000724     C*
013200000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
013300011113     C                   Z-ADD     150           LENGH            15 5
013400000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013500000724     C                   PARM                    depcmd
013600000724     C                   PARM                    LENGH
013700000724     C*
013800000724     C                   endif
013900000616     C*
014000000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
014100000616     C                   movel(p)  CMD(2)        depcmd
014200000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
014300000616     C*
014400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
014500011113     C                   Z-ADD     150           LENGH            15 5
014600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014700000616     C                   PARM                    depcmd
014800000616     C                   PARM                    LENGH
014900000613     C*
015000090623     C                   if        not %open(titv1q7ps)
015100090623     C                   open      titv1q7ps
015200070329     C                   except    testdett
015300000613     C                   endif
015400010601     C*
015500010601     C                   endif
015600010601     C*
015700010601     C                   ENDSR
015800010601     C***
015900010601
016000010601
016100010601
016200010601
016300010601     C*--------------------------------------------------------
016400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
016500010601     C*--------------------------------------------------------
016600010601     C     ENDELA        BEGSR
016700081014     C*
016800081014     C                   if        %open(tivin00r)
016900081014     C                   close     tivin00r
017000081014     C                   endif
017100090911     C                   if        %open(edivabwr)
017200090911     C                   close     edivabwr
017300081014     C                   endif
017400090911     C                   if        %open(edivatwr)
017500090911     C                   close     edivatwr
017600081014     C                   endif
017700040119     C*
017800040119     C* Effettuo la chiamata al CLLE preposto
017900101004     C                   call(e)   'TITVEVTC'
018000040510     C                   parm                    parccm
018100040119     C                   parm                    parmbr
018200040119     C                   parm      '2'           paropz
018300050201     C*
018400050201     C* Effettuo lancio TISI95 solo x chiusura
018500050201     C                   CLEAR                   TISI95DS
018600050201     C                   EVAL      I95TLA = 'C'
018700050201     C                   CALL      'TISI95R'
018800050201     C                   PARM                    TISI95DS
018900000613     C*
019000090623     C                   if        %open(titv1q7ps)
019100000616     C                   except    findett
019200090623     C                   close     titv1q7ps
019300000613     C                   endif
019400000616     C*
019500000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
019600011113     C                   Z-ADD     150           LENGH            15 5
019700010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
019800000616     C                   PARM                    CMD(3)
019900000616     C                   PARM                    LENGH
020000000616     C*
020100010601     C                   ENDSR
020200010601     C***
020300010601
020400010601
020500010601
020600000613
020700000613     C*--------------------------------------------------------
020800000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
020900000613     C*--------------------------------------------------------
021000000621     C     STPR          BEGSR
021100000613     C*
021200090623     C                   if        not %open(titv1q7p)
021300090623     C   50              open      titv1q7p
021400990915     C                   endif
021500990915     C*
021600030715     C   50              except    riepilogo
021700990915     C*
021800090623     C                   if        %open(titv1q7p)
021900090623     C   50              close     titv1q7p
022000990914     C                   endif
022100000613     C*
022200000613     C                   ENDSR
022300000613     C***
022400000613
022500010601
022600010601
022700010330     C*--------------------------------------------------------
022800090911     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
022900010330     C*--------------------------------------------------------
023000010605     C     RWVAB         BEGSR
023100010330     C*
023200010330     C                   if        not %open(tivin00r)
023300010330     C                   open      tivin00r
023400010330     C                   endif
023500090911     C                   if        not %open(edivabwr)
023600090911     C                   open      edivabwr
023700010330     C                   endif
023800040119     C*
023900101004     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
024000101004     C                   exsr      prevat
024100040119     C*
024200040119     C                   if        chkcall = '0'
024300040119     C*
024400090911     C                   if        not %open(edivatwr)
024500090911     C                   open      edivatwr
024600040119     C                   endif
024700010330     C*
024800040119     C                   clear                   §CTROKVB          7 0
024900040119     C                   clear                   §CTROKVT          7 0
025000090623     C                   clear                   §CTROKVX          7 0
025100040119     C                   clear                   §CTRMO            7 0
025200040119     C                   clear                   §CTRNO            7 0
025300090623     C*
025400090623     C* Inizializzazioni
025500090623     C                   exsr      inzvar
025600090623     C                   z-add     *zeros        wNewBolla         7 0
025700010330     C*
025800030822     C                   do        *HIVAL
025900010330     C*
026000030822     C                   read      tivin00r                               70
026100010618     C*
026200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
026300010618     C                   if        vinflg = '1'
026400010618     C                   eval      flgOk = '1'
026500010618     C                   endif
026600040510     C*
026700040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
026800040510     C                   z-add     *zeros        wLenVINDTA        4 0
026900040510     C                   z-add     *zeros        wFlgVINDTA        1 0
027000040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
027100040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
027200040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
027300040510     C                   z-add     1             wFlgVINDTA
027400040510     C                   leave
027500040510     C                   endif
027600040510     C                   enddo
027700090420     C*
027800090420     C                   add       1             rrnum
027900010618     C*
028000040510     C                   if        vindta > *blanks AND
028100090420     C                             wFlgVINDTA = 1
028200010330     C*
028300010601     C                   if        *in70 = *off and
028400010330     C                             (vinflg = *blanks
028500010330     C                              or vinflg = '0'
028600010330     C                              or vinflg = '2')
028700010330     C*
028800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
028900010711     C                   if        vinflg = *blanks or vinflg = '0'
029000010711     C                   clear                   vinmsg
029100010711     C                   endif
029200010601     C*
029300090421     C* Aggiungo un byte ad '*' all'inizio d ogni record
029400090421     C                   if        %subst(%trim(vindta):1:1) = CharCSV
029500090421     C                   eval      vindta  = '*' + %trim(vindta)
029600090421     C                   endif
029700010601     C*
029800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
029900010605     C                   eval      depfil = VABLNP
030000010601     C                   exsr      repfil
030100010601     C                   if        depfil = invfil
030200021025     C                   if        vlrpoi = 999
030300030822     C                   move(P)   invfil        VABFGS
030400021025     C                   else
030500030822     C                   move(P)   vlrpoi        VABFGS
030600021025     C                   endif
030700090914     C*
030800090914     C                   move      VABFGS        currFGS           3 0
030900090914     C*
031000090914     C* Elaborazione record corrente
031100090914     C                   exsr      elabCurrRec
031200010601     C*
031300010601     C                   exsr      PREELA
031400010604     C*
031500010604     C                   if        *in31 = *off and
031600010604     C                             *in32 = *off
031700010604     C                   eval      vinflg = '1'
031800010604     C                   else
031900010604     C                   eval      recko = vindta
032000010604     C                   except    rigadett
032100010604     C                   eval      vinflg = '2'
032200010604     C                   endif
032300010604     C                   endif
032400010601     C*
032500010604     C                   endif
032600010604     C*
032700010330     C                   else
032800010330     C                   eval      vinflg = '1'
032900010330     C                   endif
033000010601     C*
033100010601     C  N70              update    tivin000
033200010330     C*
033300030822     C  N70              enddo
033400090623     C*
033500090623     C* Scarico la testata rimasta "in canna"
033600090623     C  N31              exsr      wriVAB
033700040119     C*
033800040119     C                   endif                                                  (endif - chkcall)
033900010601     C*
034000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
034100010601     C                   if        cntNonEl = *zeros or
034200010601     C                             flgMulti = '0'
034300010330     C* Se non ci sono record con errori ...
034400010601     C                   if        §ctrno = 0 and
034500010604     C                             §ctrmo = 0 and
034600010601     C                             flgStato <> '2'
034700010330     C* ... restituisco esito OK.
034800010330     C                   eval      wrkesito = '0'
034900010330     C                   else
035000040119     C                   if        §ctrokvb > 0
035100010330     C                   eval      wrkesito = '1'
035200010330     C                   else
035300010615     C                   if        flgOk = '0'
035400010615     C                   eval      wrkesito = '2'
035500010615     C                   else
035600010615     C                   eval      wrkesito = '6'
035700010615     C                   endif
035800010330     C                   endif
035900010330     C                   endif
036000010601     C                   else
036100010601     C                   eval      wrkesito = '9'
036200010601     C                   endif
036300010330     C*
036400010330     C                   if        %open(tivin00r)
036500010330     C                   close     tivin00r
036600010330     C                   endif
036700090911     C                   if        %open(edivabwr)
036800090911     C                   close     edivabwr
036900010330     C                   endif
037000090911     C                   if        %open(edivatwr)
037100090911     C                   close     edivatwr
037200040512     C                   endif
037300010601     C*
037400010601     C                   if        vlrpoi <> 999
037500010601     C                   eval      invfil = vlrpoi
037600010601     C                   endif
037700010330     C*
037800040119     C                   if        §ctrokvb > 0
037900010601     C                             and invfil > *zeros
038000010330     C                   exsr      invio
038100010330     C                   endif
038200010601     C*
038300010618     C                   if        flgGiro = '1'
038400081014     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
038500010618     C                   endif
038600010330     C*
038700010330     C                   ENDSR
038800010330     C***
038900090623
039000090623
039100090623
039200090623     C*----------------------------------------------------*
039300090623     C*  INIZIALIZZAZIONI CAMPI DI WRK
039400090623     C*----------------------------------------------------*
039500090623     C     INZWRK        BEGSR
039600090623     C*
039700090623     C                   eval      i = 1
039800090623     C                   eval      posDa = *zeros
039900090623     C                   eval      posA  = *zeros
040000090623     C*
040100090623     C                   ENDSR
040200090623
040300090623
040400090623
040500090623     C*----------------------------------------------------*
040600090623     C*  ELABORAZIONE RECORD CORRENTE
040700090623     C*----------------------------------------------------*
040800090623     C     ELABCURRREC   BEGSR
040900090623     C*
041000090623     C                   select
041100090623     C*
041200090623     C* Tipo record "TESTATA" - ENTEXP
041300090623     C                   when      %subst(vindta:1:6) = 'ENTEXP'
041400090623     C                   if        wNewBolla > *zeros
041500090623     C  N31              exsr      wriVAB
041600090623     C                   exsr      inzvar
041700090623     C                   endif
041800090624     C                   eval      wNewBolla = 1
041900090623     C                   eval      wGiro = *zeros
042000090623     C                   clear                   SkSplitFLD
042100090624     C                   exsr      inzwrk
042200090623     C                   exsr      impvab
042300090623     C                   exsr      inzwrk
042400090623     C                   exsr      impvab
042500090623     C*
042600090623     C* Tipo record "DETTAGLIO" - LIGEXP
042700090623     C                   when      %subst(vindta:1:6) = 'LIGEXP'
042800090623     C                   eval      wGiro = *zeros
042900090623     C                   clear                   SkSplitFLD
043000090623     C                   exsr      inzwrk
043100101004     C                   exsr      impvat
043200090624     C*
043300090624     C  N31              exsr      wrivat
043400090623     C*
043500090623     C*
043600090623     C                   endsl
043700090623     C*
043800090623     C                   ENDSR
043900040119
044000040119
044100040119
044200040119     C*----------------------------------------------------*
044300040119     C*  SCARICAMENTO BUFFER RECORDS VAB
044400040119     C*----------------------------------------------------*
044500040510     C     WRIVAB        BEGSR
044600090623     C*
044700090623     C  N31              add       1             §CTROKVB
044800090623     C   32              add       1             §CTRMO
044900090623     C   31              add       1             §CTRNO
045000040119     C*
045100060331     C* Gestisco l'eventuale rottura x numero spedizione
045200070823     C                   if        VABNSP <> depspe
045300070301     C                   if        wVATNOT_E <> *blanks
045400080125     C                   if        VABCTM = *blanks
045500080125     C                   eval      VABCTM = '7Q'
045600080125     C                   endif
045700070301     C                   endif
045800090911     C*
045900090911     C* VALORIZZO CAMPI RELATIVI AL "CMR"
046000090911     C                   eval      VABCMR = wCMR
046100090911     C                   eval      VABDCM = DATCOR
046200090911     C                   eval      VABDTS = DATCOR
046300090911     C                   eval      VABHMS = ORACOR
046400090911     C                   eval      VABCNT = 1
046500090911     C****
046600090911     C                   write     edivab00                                     => scarico il VAB
046700060331     C                   endif
046800090623     C*
046900090623     C  N31              eval      depspe = vabNSP
047000040119     C*
047100040119     C                   ENDSR
047200040119
047300040119
047400040119
047500040119     C*----------------------------------------------------*
047600040119     C*  SCARICAMENTO BUFFER RECORDS VAT
047700040119     C*----------------------------------------------------*
047800040510     C     WRIVAT        BEGSR
047900060512     C*
048000060512     C* Inizializzo i campi chiave
048100090914     C                   eval      VATFGS = currFGS
048200060512     C                   eval      VATAAS = VABAAS
048300060512     C                   eval      VATCCM = VABCCM
048400070823     C                   eval      VATNRS = VABNRS
048500060512     C                   eval      VATNSP = VABNSP
048600060512     C                   eval      VATLNP = VABLNP
048700090914     C                   eval      VATCMR = wCMR
048800090914     C                   eval      VATCNT = 1
048900060331     C*
049000060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
049100060331     C                   if        wVATNOT_A <> *blanks
049200060331     C                   eval      VATNOT = wVATNOT_A
049300060331     C                   eval      VATTRC = 'A'
049400090911     C                   write     EDIVAT00
049500060331     C                   add       1             §CTROKVT
049600060331     C                   endif
049700060331     C*
049800060331     C                   if        wVATNOT_B <> *blanks
049900060331     C                   eval      VATNOT = wVATNOT_B
050000060331     C                   eval      VATTRC = 'B'
050100090911     C                   write     EDIVAT00
050200060331     C                   add       1             §CTROKVT
050300060331     C                   endif
050400090623     C*
050500090623     C* Considero il VAT solo a rottura del "chi sono"
050600090623     C                   if        wVATNOT_E <> *blanks AND
050700090623     C                             wVATNOT_E <> sVATNOT_E
050800060331     C                   eval      VATNOT = wVATNOT_E
050900060331     C                   eval      VATTRC = 'E'
051000090911     C                   write     EDIVAT00
051100080716     C                   add       1             §CTROKVT
051200090623     C                   eval      sVATNOT_E = wVATNOT_E
051300090624     C                   add       1             VABNCL
051400080625     C                   endif
051500040119     C*
051600040119     C                   ENDSR
051700090624
051800040119
051900040119
052000040119
052100040119     C*----------------------------------------------------*
052200101004     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
052300040119     C*----------------------------------------------------*
052400101004     C     PREVAT        BEGSR
052500040119     C*
052600101004     C* Compongo il nome del membro da dare al EDIVATWR
052700040119     C                   eval      parmbr = vlrhdl
052800040119     C                   movel     'M'           parmbr
052900040510     C                   eval      parccm = vlrksc
053000040119     C                   eval      paropz = '1'
053100040119     C* Effettuo la chiamata al CLLE preposto
053200101004     C                   call(e)   'TITVEVTC'
053300040119     C                   parm                    parccm
053400040119     C                   parm                    parmbr
053500040119     C                   parm                    paropz
053600040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
053700040119     C                   if        %error
053800040119     C                   movel     '1'           chkcall
053900040119     C                   else
054000040119     C                   movel     '0'           chkcall
054100040119     C                   endif
054200040119     C*
054300040119     C                   ENDSR
054400010601
054500010601
054600010601
054700010330     C*----------------------------------------------------*
054800030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
054900010330     C*----------------------------------------------------*
055000040119     C     INZVAR        BEGSR
055100010330     C*
055200030822     C* Inizializzo variabili di wrk
055300090916     C                   Z-ADD     *zeros        Num5_0            5 0
055400090916     C                   MOVEL     *blanks       wNOTE            70
055500060331     C                   MOVEL     *blanks       wVATNOT_A
055600060331     C                   MOVEL     *blanks       wVATNOT_B
055700090904     C                   MOVEL     *blanks       wVATNOT_E
055800090904     C                   MOVEL     *blanks       wVATNOT_E_1
055900090904     C                   MOVEL     *blanks       wVATNOT_E_2
056000090623     C*
056100090624     C                   MOVEL     *blanks       sVATNOT_E        35
056200090220     C*
056300080617     C                   EVAL      wFlgCAS = *blanks
056400030822     C*
056500030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
056600090911     C                   CLEAR                   EDIVAB00
056700090911     C                   CLEAR                   EDIVAT00
056800030822     C                   CLEAR                   SkSplitCSV
056900090623     C                   EXSR      inzwrk
057000030822     C*
057100030822     C* Reimposto i valori di default
057200030822     C                   EXSR      DEFCAM
057300030822     C*
057400010330     C                   ENDSR
057500010330     C*----------------------------------------------------*
057600030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
057700010330     C*----------------------------------------------------*
057800010330     C     DEFCAM        BEGSR
057900030822     C*
058000030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
058100030715     C* e delimitatore testo.
058200030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
058300030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
058400030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
058500030822     C*
058600030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
058700030715     C                   IF        %subst(vlrppt:1:1) = 'S'
058800030715     C                   SETON                                        50
058900030715     C                   ELSE
059000030715     C                   SETOFF                                       50
059100030715     C                   ENDIF
059200090421     C*
059300090421     C* D default SEMPRE porto franco / SEMPRE lnp = p.o. invio / SEMPRE nsp = rrnum
059400090421     C                   EVAL      VABCBO = '1'
059500090421     C                   EVAL      VABLNP = vlrpoi
059600090421     C                   EVAL      VABNSP = rrnum
059700030822     C*
059800030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
059900030822     C                   EVAL      posDaDft = 1
060000030822     C                   EVAL      posADft  = 0
060100030822     C                   EVAL      wGiroDft = 0
060200030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
060300030822     C                             posDaDft > 0
060400030822     C*
060500030822     C* Gestisco il 1° giro
060600030822     C                   IF        wGiroDft = 0
060700030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
060800030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
060900030822     C* Incremento il contatore dei "giri"
061000030822     C                   EVAL      wGiroDft = 1
061100030822     C                   ELSE
061200030822     C                   EVAL      posDaDft = posADft
061300030822     C                   ENDIF
061400030822     C* Eseguo lo scan x trovare la fine del campo corrente
061500030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
061600030822     C*
061700030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
061800030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
061900030822     C                   IF        posDaDft > 0 AND
062000030822     C                             posADft  > 0
062100030822     C* NCL
062200030822     C                   IF        %subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):1:3)
062500030822     C                             = 'NCL'
062600030822     C                   EVAL      PiStr=%trim(%subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062900030822     C                   EXSR      CHKNUM
063000030822     C                   IF        PiInt=*on
063100030822     C                   Z-ADD     PiVal         VABNCL
063200030822     C                   ENDIF
063300030822     C                   ENDIF
063400030822     C* CCM
063500030822     C                   IF        %subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):1:3)
063800030822     C                             = 'CCM'
063900030822     C                   EVAL      PiStr=%trim(%subst(
064000030822     C                             %subst(vlrppt:posDaDft+1:
064100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064200030822     C                   EXSR      CHKNUM
064300030822     C                   IF        PiInt=*on
064400030822     C                   Z-ADD     PiVal         VABCCM
064500030822     C                   ENDIF
064600030822     C                   ENDIF
064700030822     C* LNP
064800030822     C                   IF        %subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):1:3)
065100030822     C                             = 'LNP'
065200030822     C                   EVAL      PiStr=%trim(%subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065500030822     C                   EXSR      CHKNUM
065600030822     C                   IF        PiInt=*on
065700030822     C                   Z-ADD     PiVal         VABLNP
065800030822     C                   ENDIF
065900030822     C                   ENDIF
066000030822     C* NRS
066100030822     C                   IF        %subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):1:3)
066400030822     C                             = 'NRS'
066500030822     C                   EVAL      PiStr=%trim(%subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066800030822     C                   EXSR      CHKNUM
066900030822     C                   IF        PiInt=*on
067000030822     C                   Z-ADD     PiVal         VABNRS
067100030822     C                   ENDIF
067200030822     C                   ENDIF
067300030822     C* CTR
067400030822     C                   IF        %subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):1:3)
067700030822     C                             = 'CTR'
067800030822     C                   EVAL      PiStr=%trim(%subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068100030822     C                   EXSR      CHKNUM
068200030822     C                   IF        PiInt=*on
068300030822     C                   Z-ADD     PiVal         VABCTR
068400030822     C                   ENDIF
068500030822     C                   ENDIF
068600030822     C* PKB
068700030822     C                   IF        %subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):1:3)
069000030822     C                             = 'PKB'
069100030822     C                   EVAL      PiStr=%trim(%subst(
069200030822     C                             %subst(vlrppt:posDaDft+1:
069300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069400030822     C                   EXSR      CHKNUM
069500030822     C                   IF        PiNum=*on
069600030822     C                   Z-ADD     PiVal         VABPKB
069700030822     C                   ENDIF
069800030822     C                   ENDIF
069900030822     C* VLB
070000030822     C                   IF        %subst(
070100030822     C                             %subst(vlrppt:posDaDft+1:
070200030822     C                             posADft-posDaDft-1):1:3)
070300030822     C                             = 'VLB'
070400030822     C                   EVAL      PiStr=%trim(%subst(
070500030822     C                             %subst(vlrppt:posDaDft+1:
070600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070700030822     C                   EXSR      CHKNUM
070800030822     C                   IF        PiNum=*on
070900030822     C                   Z-ADD     PiVal         VABVLB
071000030822     C                   ENDIF
071100030822     C                   ENDIF
071200030822     C* QFT
071300030822     C                   IF        %subst(
071400030822     C                             %subst(vlrppt:posDaDft+1:
071500030822     C                             posADft-posDaDft-1):1:3)
071600030822     C                             = 'QFT'
071700030822     C                   EVAL      PiStr=%trim(%subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072000030822     C                   EXSR      CHKNUM
072100030822     C                   IF        PiNum=*on
072200030822     C                   Z-ADD     PiVal         VABQFT
072300030822     C                   ENDIF
072400030822     C                   ENDIF
072500030822     C* CBO
072600030822     C                   IF        %subst(
072700030822     C                             %subst(vlrppt:posDaDft+1:
072800030822     C                             posADft-posDaDft-1):1:3)
072900030822     C                             = 'CBO'
073000030822     C                   EVAL      VABCBO=%trim(%subst(
073100030822     C                             %subst(vlrppt:posDaDft+1:
073200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073300030822     C                   ENDIF
073400030822     C* TSP
073500030822     C                   IF        %subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):1:3)
073800030822     C                             = 'TSP'
073900030822     C                   EVAL      VABTSP=%trim(%subst(
074000030822     C                             %subst(vlrppt:posDaDft+1:
074100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074200030822     C                   ENDIF
074300030822     C* VAS
074400030822     C                   IF        %subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):1:3)
074700030822     C                             = 'VAS'
074800030822     C                   EVAL      VABVAS=%trim(%subst(
074900030822     C                             %subst(vlrppt:posDaDft+1:
075000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075100030822     C                   ENDIF
075200030822     C* VCA
075300030822     C                   IF        %subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):1:3)
075600030822     C                             = 'VCA'
075700030822     C                   EVAL      VABVCA=%trim(%subst(
075800030822     C                             %subst(vlrppt:posDaDft+1:
075900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076000030822     C                   ENDIF
076100070925     C* TIC
076200070925     C                   IF        %subst(
076300070925     C                             %subst(vlrppt:posDaDft+1:
076400070925     C                             posADft-posDaDft-1):1:3)
076500070925     C                             = 'TIC'
076600070925     C                   EVAL      VABTIC=%trim(%subst(
076700070925     C                             %subst(vlrppt:posDaDft+1:
076800070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076900070925     C                   ENDIF
077000030822     C* GCA
077100030822     C                   IF        %subst(
077200030822     C                             %subst(vlrppt:posDaDft+1:
077300030822     C                             posADft-posDaDft-1):1:3)
077400030822     C                             = 'GCA'
077500030822     C                   EVAL      VABGCA=%trim(%subst(
077600030822     C                             %subst(vlrppt:posDaDft+1:
077700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077800030822     C                   ENDIF
077900030822     C* CTM
078000030822     C                   IF        %subst(
078100030822     C                             %subst(vlrppt:posDaDft+1:
078200030822     C                             posADft-posDaDft-1):1:3)
078300030822     C                             = 'CTM'
078400030822     C                   EVAL      VABCTM=%trim(%subst(
078500030822     C                             %subst(vlrppt:posDaDft+1:
078600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078700030822     C                   ENDIF
078800030822     C* FFD
078900030822     C                   IF        %subst(
079000030822     C                             %subst(vlrppt:posDaDft+1:
079100030822     C                             posADft-posDaDft-1):1:3)
079200030822     C                             = 'FFD'
079300030822     C                   EVAL      VABFFD=%trim(%subst(
079400030822     C                             %subst(vlrppt:posDaDft+1:
079500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079600030822     C                   ENDIF
079700030822     C* VAD
079800030822     C                   IF        %subst(
079900030822     C                             %subst(vlrppt:posDaDft+1:
080000030822     C                             posADft-posDaDft-1):1:3)
080100030822     C                             = 'VAD'
080200030822     C                   EVAL      VABVAD=%trim(%subst(
080300030822     C                             %subst(vlrppt:posDaDft+1:
080400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080500030822     C                   ENDIF
080600030822     C* GMA
080700030822     C                   IF        %subst(
080800030822     C                             %subst(vlrppt:posDaDft+1:
080900030822     C                             posADft-posDaDft-1):1:3)
081000030822     C                             = 'GMA'
081100030822     C                   EVAL      VABGMA=%trim(%subst(
081200030822     C                             %subst(vlrppt:posDaDft+1:
081300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081400030822     C                   ENDIF
081500030822     C* GGA
081600030822     C                   IF        %subst(
081700030822     C                             %subst(vlrppt:posDaDft+1:
081800030822     C                             posADft-posDaDft-1):1:3)
081900030822     C                             = 'GGA'
082000030822     C                   EVAL      VABGGA=%trim(%subst(
082100030822     C                             %subst(vlrppt:posDaDft+1:
082200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082300030822     C                   ENDIF
082400030822     C* GVA
082500030822     C                   IF        %subst(
082600030822     C                             %subst(vlrppt:posDaDft+1:
082700030822     C                             posADft-posDaDft-1):1:3)
082800030822     C                             = 'GVA'
082900030822     C                   EVAL      VABGVA=%trim(%subst(
083000030822     C                             %subst(vlrppt:posDaDft+1:
083100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083200030822     C                   ENDIF
083300030822     C* TC1
083400030822     C                   IF        %subst(
083500030822     C                             %subst(vlrppt:posDaDft+1:
083600030822     C                             posADft-posDaDft-1):1:3)
083700030822     C                             = 'TC1'
083800030822     C                   EVAL      VABTC1=%trim(%subst(
083900030822     C                             %subst(vlrppt:posDaDft+1:
084000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084100030822     C                   ENDIF
084200030822     C* TC2
084300030822     C                   IF        %subst(
084400030822     C                             %subst(vlrppt:posDaDft+1:
084500030822     C                             posADft-posDaDft-1):1:3)
084600030822     C                             = 'TC2'
084700030822     C                   EVAL      VABTC2=%trim(%subst(
084800030822     C                             %subst(vlrppt:posDaDft+1:
084900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085000030822     C                   ENDIF
085100040714     C* VATTRC
085200040714     C                   IF        %subst(
085300040714     C                             %subst(vlrppt:posDaDft+1:
085400040714     C                             posADft-posDaDft-1):1:3)
085500040714     C                             = 'TRC'
085600040714     C                   EVAL      VATTRC=%trim(%subst(
085700040714     C                             %subst(vlrppt:posDaDft+1:
085800040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085900040714     C                   ENDIF
086000030822     C* ...
086100030822     C                   ENDIF
086200030822     C                   ENDDO
086300020204     C*
086400010330     C                   ENDSR
086500010607     C*----------------------------------------------------*
086600090911     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
086700010607     C*----------------------------------------------------*
086800090623     C     IMPVAB        BEGSR
086900010607     C*
087000010607     C                   SETOFF                                       3132
087100030515     C*
087200010607     C                   MOVEL     datcor        VABAAS
087300010607     C                   MOVE      datcor        VABMGS
087400010607     C*
087500030715     C**********
087600030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
087700030715     C**********
087800030715     C                   IF        wGiro = *zeros
087900080716     C*
088000080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
088100080716     C                   EVAL      depvinDTA = vinDTA
088200040113     C*
088300040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
088400090623     C                   IF        %trim(entexp(1))+%trim(entexp(2))+
088500090623     C                             %trim(entexp(3))+%trim(entexp(4))+
088600090623     C                             %trim(entexp(5))<>*blanks
088700090623     C                   EVAL      vindta = %trim(entexp(1))+%trim(entexp(2))+
088800090623     C                                      %trim(entexp(3))+%trim(entexp(4))+
088900090623     C                                      %trim(entexp(5))
089000070823     C     ';':CharCSV   XLATE     vindta        vindta
089100040510     C                   ENDIF
089200080626     C*
089300031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
089400031201     C     minu:maiu     XLATE     vindta        vindta
089500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
089600030715     C                   DOW       posDa <= %len(%trim(vindta))
089700030715     C*
089800030715     C* Gestisco il 1° campo
089900030715     C                   IF        i = 1
090000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
090100030715     C                   EVAL      posDa = 1
090200030715     C* Eseguo lo scan x trovare la fine del primo campo
090300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090400030715     C* A questo punto "estraggo" il campo corrente
090500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
090600030715     C                                                    (posA-posDa))
090700030715     C* X i campi successivi al 1°
090800030715     C                   ELSE
090900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
091000030715     C                   EVAL      posDa = posA + 1
091100030715     C* Eseguo lo scan x trovare la fine del campo corrente
091200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091300030715     C* Gestisco l'ultimo campo
091400030715     C                   IF        posA = *zeros
091500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
091600030715     C* A questo punto "estraggo" il campo corrente
091700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
091800030715     C                                                    (posA-posDa))
091900030715     C                   LEAVE
092000030715     C                   ELSE
092100030715     C* A questo punto "estraggo" il campo corrente
092200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
092300030715     C                                                    (posA-posDa))
092400030715     C                   ENDIF
092500030715     C                   ENDIF
092600030715     C* Incremento il contatore d campo
092700030715     C                   EVAL      i = i +1
092800030715     C                   ENDDO
092900030715     C*
093000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
093100030715     C* delimitazione testo
093200030715     C                   EVAL      i = 1
093300030715     C                   DOW       i < %elem(SkSplitFLD)
093400030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
093500030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
093600030715     C                   EVAL      i = i + 1
093700030715     C                   ENDDO
093800030715     C                   EVAL      wGiro = 1
093900080716     C* Ripristino la 1° riga originale
094000080716     C                   EVAL      vinDTA = depvinDTA
094100030715     C                   ELSE
094200030715     C                   EVAL      wGiro = 2
094300031204     C**********
094400031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
094500031204     C* d separatore campo
094600031204     C**********
094700031204     C                   EVAL      vindta = %trim(vindta)
094800031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
094900031204     C                   EVAL      vindta = %subst(vindta:2)
095000031204     C                   ENDDO
095100031204     C*
095200031204     C                   Z-ADD     *zeros        lunghInput        4 0
095300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
095400031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
095500031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
095600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
095700031204     C                   ENDDO
095800030715     C**********
095900030715     C* Effettuo lo split del campo dati d input
096000030715     C**********
096100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
096200030715     C                   DOW       posDa <= %len(%trim(vindta))
096300030715     C*
096400030715     C* Gestisco il 1° campo
096500030715     C                   IF        i = 1
096600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
096700030715     C                   EVAL      posDa = 1
096800030715     C* Eseguo lo scan x trovare la fine del primo campo
096900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
097000030715     C* A questo punto "estraggo" il campo corrente
097100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
097200030715     C                                                    (posA-posDa))
097300030715     C* X i campi successivi al 1°
097400030715     C                   ELSE
097500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
097600030715     C                   EVAL      posDa = posA + 1
097700031204     C* Verifico che nn vi sia il campo nn valorizzato
097800031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
097900031204     C* Se campo nn valorizzato skippo al prossimo
098000031204     C                   EVAL      SkSplitCSV(i) = *blanks
098100031204     C                   EVAL      posA  = posA + 1
098200031204     C                   ELSE
098300030715     C* Eseguo lo scan x trovare la fine del campo corrente
098400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
098500030715     C* Gestisco l'ultimo campo
098600030715     C                   IF        posA = *zeros
098700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
098800030715     C* A questo punto "estraggo" il campo corrente
098900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
099000030715     C                                                    (posA-posDa))
099100030715     C                   LEAVE
099200030715     C                   ELSE
099300030715     C* A questo punto "estraggo" il campo corrente
099400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
099500030715     C                                                    (posA-posDa))
099600030715     C                   ENDIF
099700031204     C                   ENDIF
099800030715     C                   ENDIF
099900030715     C* Incremento il contatore d campo
100000030715     C                   EVAL      i = i +1
100100030715     C                   ENDDO
100200030715     C*
100300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
100400030715     C* delimitazione testo
100500030715     C                   EVAL      i = 1
100600030715     C                   DOW       i < %elem(SkSplitCSV)
100700030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
100800030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
100900030715     C                   EVAL      i = i + 1
101000030715     C                   ENDDO
101100030715     C*
101200030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
101300030715     C                   EVAL      i = 1
101400030715     C                   DOW       i < %elem(SkSplitFLD)
101500030715     C*
101600030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
101700030715     C                   IF        SkSplitFLD(i) <> *blanks AND
101800090623     C                             SkSplitCSV(i) <> *blanks
101900090623     C*
102000090623     C                   exsr      CARVAL
102100040510     C*
102200030715     C                   ENDIF
102300030715     C                   EVAL      i = i + 1
102400030715     C                   ENDDO
102500070925     C*
102600071029     C***  ===>  Gestione campi "particolari" fuori ciclo
102700080125     C*
102800090623     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
102900090623     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
103000091007     C*
103100091007     C                   IF        wNOTE <> *blanks
103200091007     C                   EVAL      VABNMO = *all'*'
103300091007     C                   ENDIF
103400070925     C*
103500071029     C***  <===  -----------------------------------------
103600070925     C*
103700070925     C* Considerazioni finali su CBO/CAS
103800080125     C                   IF        wFlgCAS = '1'
103900070925     C                   IF        VABCBO = '1'
104000070925     C                   EVAL      VABCBO = '4'
104100070925     C                   ENDIF
104200070925     C                   IF        VABCBO = '2'
104300070925     C                   EVAL      VABCBO = '6'
104400070925     C                   ENDIF
104500070925     C                   ENDIF
104600070925     C***  <===  ----------------------------
104700040510     C*
104800040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
104900040510     C                   IF        VABNSP = *zeros
105000040510     C                   SETON                                        31
105100040510     C                   ENDIF
105200040715     C*
105300040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
105400040715     C                   IF        VABLNP = *zeros
105500040715     C                   SETON                                        31
105600040715     C                   ENDIF
105700040510     C*
105800040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
105900040510     C                   IF        VABRMN = *zeros
106000040510     C                   Z-ADD     VABNSP        VABRMN
106100040510     C                   ENDIF
106200040510     C*
106300040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
106400040510     C                   IF        VABRMA = *blanks
106500040714     C                   MOVEL     VABRMN        VABRMA
106600040510     C                   ENDIF
106700040128     C*
106800040510     C* Se provincia nn valorizzata la reperisco
106900040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
107000040510     C                   IF        VABPRD = *blanks
107100040128     C                   CLEAR                   TISI95DS
107200040128     C                   EVAL      I95TCN = '3'
107300040128     C                   Z-ADD     datcor        I95DAT
107400040128     C                   EVAL      I95NAR = VABNZD
107500040128     C                   EVAL      I95CAP = VABCAD
107600040128     C                   EVAL      I95LOC = VABLOD
107700040128     C                   CALL      'TISI95R'
107800040128     C                   PARM                    TISI95DS
107900040128     C                   EVAL      VABPRD = O95PRV
108000040510     C                   ENDIF
108100020204     C*
108200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
108300020204     C                   EXSR      CHKIMPDIV
108400030715     C                   ENDIF
108500020204     C*
108600010607     C                   ENDSR
108700010607     C*----------------------------------------------------*
108800090623
108900090623
109000090623
109100090623     C*----------------------------------------------------*
109200101004     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
109300090623     C*----------------------------------------------------*
109400101004     C     IMPVAT        BEGSR
109500090623     C*
109600090623     C**********
109700090623     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
109800090623     C**********
109900090623     C                   IF        wGiro = *zeros
110000090623     C*
110100090623     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
110200090623     C                   EVAL      depvinDTA = vinDTA
110300090623     C*
110400090623     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
110500090623     C                   IF        %trim(ligexp(1))+%trim(ligexp(2))+
110600090623     C                             %trim(ligexp(3))+%trim(ligexp(4))+
110700090623     C                             %trim(ligexp(5))<>*blanks
110800090623     C                   EVAL      vindta = %trim(ligexp(1))+%trim(ligexp(2))+
110900090623     C                                      %trim(ligexp(3))+%trim(ligexp(4))+
111000090623     C                                      %trim(ligexp(5))
111100090623     C     ';':CharCSV   XLATE     vindta        vindta
111200090623     C                   ENDIF
111300090623     C*
111400090623     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
111500090623     C     minu:maiu     XLATE     vindta        vindta
111600090623     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
111700090623     C                   DOW       posDa <= %len(%trim(vindta))
111800090623     C*
111900090623     C* Gestisco il 1° campo
112000090623     C                   IF        i = 1
112100090623     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
112200090623     C                   EVAL      posDa = 1
112300090623     C* Eseguo lo scan x trovare la fine del primo campo
112400090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
112500090623     C* A questo punto "estraggo" il campo corrente
112600090623     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
112700090623     C                                                    (posA-posDa))
112800090623     C* X i campi successivi al 1°
112900090623     C                   ELSE
113000090623     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
113100090623     C                   EVAL      posDa = posA + 1
113200090623     C* Eseguo lo scan x trovare la fine del campo corrente
113300090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
113400090623     C* Gestisco l'ultimo campo
113500090623     C                   IF        posA = *zeros
113600090623     C                   EVAL      posA = %len(%trim(vindta)) + 1
113700090623     C* A questo punto "estraggo" il campo corrente
113800090623     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
113900090623     C                                                    (posA-posDa))
114000090623     C                   LEAVE
114100090623     C                   ELSE
114200090623     C* A questo punto "estraggo" il campo corrente
114300090623     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
114400090623     C                                                    (posA-posDa))
114500090623     C                   ENDIF
114600090623     C                   ENDIF
114700090623     C* Incremento il contatore d campo
114800090623     C                   EVAL      i = i +1
114900090623     C                   ENDDO
115000090623     C*
115100090623     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
115200090623     C* delimitazione testo
115300090623     C                   EVAL      i = 1
115400090623     C                   DOW       i < %elem(SkSplitFLD)
115500090623     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
115600090623     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
115700090623     C                   EVAL      i = i + 1
115800090623     C                   ENDDO
115900090623     C                   EVAL      wGiro = 1
116000090623     C* Ripristino la 1° riga originale
116100090623     C                   EVAL      vinDTA = depvinDTA
116200090623     C                   ELSE
116300090623     C                   EVAL      wGiro = 2
116400090623     C**********
116500090623     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
116600090623     C* d separatore campo
116700090623     C**********
116800090623     C                   EVAL      vindta = %trim(vindta)
116900090623     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
117000090623     C                   EVAL      vindta = %subst(vindta:2)
117100090623     C                   ENDDO
117200090623     C*
117300090623     C                   Z-ADD     *zeros        lunghInput        4 0
117400090623     C                   EVAL      lunghInput = %len(%trim(vindta))
117500090623     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
117600090623     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
117700090623     C                   EVAL      lunghInput = %len(%trim(vindta))
117800090623     C                   ENDDO
117900090623     C**********
118000090623     C* Effettuo lo split del campo dati d input
118100090623     C**********
118200090623     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
118300090623     C                   DOW       posDa <= %len(%trim(vindta))
118400090623     C*
118500090623     C* Gestisco il 1° campo
118600090623     C                   IF        i = 1
118700090623     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
118800090623     C                   EVAL      posDa = 1
118900090623     C* Eseguo lo scan x trovare la fine del primo campo
119000090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
119100090623     C* A questo punto "estraggo" il campo corrente
119200090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
119300090623     C                                                    (posA-posDa))
119400090623     C* X i campi successivi al 1°
119500090623     C                   ELSE
119600090623     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
119700090623     C                   EVAL      posDa = posA + 1
119800090623     C* Verifico che nn vi sia il campo nn valorizzato
119900090623     C                   IF        %subst(vindta:posDa:1) = CharCSV
120000090623     C* Se campo nn valorizzato skippo al prossimo
120100090623     C                   EVAL      SkSplitCSV(i) = *blanks
120200090623     C                   EVAL      posA  = posA + 1
120300090623     C                   ELSE
120400090623     C* Eseguo lo scan x trovare la fine del campo corrente
120500090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
120600090623     C* Gestisco l'ultimo campo
120700090623     C                   IF        posA = *zeros
120800090623     C                   EVAL      posA = %len(%trim(vindta)) + 1
120900090623     C* A questo punto "estraggo" il campo corrente
121000090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
121100090623     C                                                    (posA-posDa))
121200090623     C                   LEAVE
121300090623     C                   ELSE
121400090623     C* A questo punto "estraggo" il campo corrente
121500090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
121600090623     C                                                    (posA-posDa))
121700090623     C                   ENDIF
121800090623     C                   ENDIF
121900090623     C                   ENDIF
122000090623     C* Incremento il contatore d campo
122100090623     C                   EVAL      i = i +1
122200090623     C                   ENDDO
122300090623     C*
122400090623     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
122500090623     C* delimitazione testo
122600090623     C                   EVAL      i = 1
122700090623     C                   DOW       i < %elem(SkSplitCSV)
122800090623     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
122900090623     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
123000090623     C                   EVAL      i = i + 1
123100090623     C                   ENDDO
123200090623     C*
123300090623     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
123400090623     C                   EVAL      i = 1
123500090623     C                   DOW       i < %elem(SkSplitFLD)
123600090623     C*
123700090623     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
123800090623     C                   IF        SkSplitFLD(i) <> *blanks AND
123900090623     C                             SkSplitCSV(i) <> *blanks
124000090623     C*
124100090623     C                   exsr      CARVAL
124200090623     C*
124300090623     C                   ENDIF
124400090623     C                   EVAL      i = i + 1
124500090623     C                   ENDDO
124600090623     C*
124700090623     C***  ===>  Gestione campi "particolari" fuori ciclo
124800090623     C*
124900090623     C*
125000090623     C***  <===  -----------------------------------------
125100090623     C*
125200090623     C                   ENDIF
125300090623     C*
125400090623     C                   ENDSR
125500090623     C*----------------------------------------------------*
125600090623
125700090623
125800090623     C*----------------------------------------------------*
125900090623     C*  ROUTINE DI VALORIZZAZIONE CAMPI
126000090623     C*----------------------------------------------------*
126100090623     C     CARVAL        BEGSR
126200090623     C*
126300090623     C*** ATB
126400090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
126500090623     C                   EVAL      VABATB = SkSplitCSV(i)
126600090623     C                   ENDIF
126700090623     C*** RSD
126800090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
126900090623     C                   EVAL      VABRSD = SkSplitCSV(i)
127000090623     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
127100090623     C     '@':'A'       XLATE     VABRSD        VABRSD
127200090623     C* ==
127300090623     C                   ENDIF
127400090623     C*** RD2
127500090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
127600090623     C                   EVAL      VABRD2 = SkSplitCSV(i)
127700090623     C                   ENDIF
127800090623     C*** IND
127900090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
128000090623     C                   EVAL      VABIND = SkSplitCSV(i)
128100090623     C                   ENDIF
128200090623     C*** LOD
128300090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
128400090623     C                   EVAL      VABLOD = SkSplitCSV(i)
128500090623     C                   ENDIF
128600090623     C*** PRD
128700090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
128800090623     C                   EVAL      VABPRD = SkSplitCSV(i)
128900090623     C                   ENDIF
129000090623     C*** NZD
129100090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
129200090623     C                   EVAL      VABNZD = SkSplitCSV(i)
129300090623     C                   ENDIF
129400090623     C*** RMA
129500090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
129600090623     C                   EVAL      VABRMA = SkSplitCSV(i)
129700090623     C                   ENDIF
129800090623     C*** NOT
129900090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
130000090623     C                   EVAL      VABNOT = SkSplitCSV(i)
130100090623     C                   ENDIF
130200090623     C*** NT2
130300090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
130400090623     C                   EVAL      VABNT2 = SkSplitCSV(i)
130500090623     C                   ENDIF
130600090623     C*** VCA
130700090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
130800090623     C                   EVAL      VABVCA = SkSplitCSV(i)
130900090623     C                   ENDIF
131000090623     C*** VAS
131100090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
131200090623     C                   EVAL      VABVAS = SkSplitCSV(i)
131300090623     C                   ENDIF
131400090623     C*** FFD
131500090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
131600090623     C                   EVAL      VABFFD = SkSplitCSV(i)
131700090623     C                   ENDIF
131800090623     C*** GC1
131900090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
132000090623     C                   EVAL      VABGC1 = SkSplitCSV(i)
132100090623     C                   ENDIF
132200090623     C*** GC2
132300090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
132400090623     C                   EVAL      VABGC2 = SkSplitCSV(i)
132500090623     C                   ENDIF
132600090623     C*** TSP
132700090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
132800090623     C                   EVAL      VABTSP = SkSplitCSV(i)
132900090623     C                   ENDIF
133000090623     C*** CBO
133100090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
133200090623     C                   EVAL      VABCBO = SkSplitCSV(i)
133300090623     C                   ENDIF
133400090623     C*** NAS
133500090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
133600090623     C                   EVAL      VABNAS = SkSplitCSV(i)
133700090623     C                   ENDIF
133800090623     C*** TIC
133900090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
134000090623     C                   EVAL      VABTIC = SkSplitCSV(i)
134100090623     C                   ENDIF
134200090623     C*** GCA
134300090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
134400090623     C                   EVAL      VABGCA = SkSplitCSV(i)
134500090623     C                   ENDIF
134600090623     C*** XCO
134700090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
134800090623     C                   EVAL      VABXCO = SkSplitCSV(i)
134900090623     C                   ENDIF
135000090623     C*** CTM
135100090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
135200090623     C                   EVAL      VABCTM = SkSplitCSV(i)
135300090623     C                   ENDIF
135400090623     C*** TCR
135500090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
135600090623     C                   EVAL      VABTCR = SkSplitCSV(i)
135700090623     C                   ENDIF
135800090623     C*** CTS
135900090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
136000090623     C                   EVAL      VABCTS = SkSplitCSV(i)
136100090623     C                   ENDIF
136200090623     C*** FTM
136300090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
136400090623     C                   EVAL      VABFTM = SkSplitCSV(i)
136500090623     C                   ENDIF
136600090623     C*** VAD
136700090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
136800090623     C                   EVAL      VABVAD = SkSplitCSV(i)
136900090623     C                   ENDIF
137000090623     C*** GMA
137100090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
137200090623     C                   EVAL      VABGMA = SkSplitCSV(i)
137300090623     C                   ENDIF
137400090623     C*** GGA
137500090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
137600090623     C                   EVAL      VABGGA = SkSplitCSV(i)
137700090623     C                   ENDIF
137800090623     C*** GVA
137900090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
138000090623     C                   EVAL      VABGVA = SkSplitCSV(i)
138100090623     C                   ENDIF
138200090623     C*** TC1
138300090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
138400090623     C                   EVAL      VABTC1 = SkSplitCSV(i)
138500090623     C                   ENDIF
138600090623     C*** TC2
138700090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
138800090623     C                   EVAL      VABTC2 = SkSplitCSV(i)
138900090623     C                   ENDIF
139000090623     C*** SCL
139100090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
139200090623     C                   EVAL      VABSCL = SkSplitCSV(i)
139300090623     C                   ENDIF
139400090623     C*** RMO
139500090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
139600090623     C                   EVAL      VABRMO = SkSplitCSV(i)
139700090623     C                   ENDIF
139800090623     C*** NMO
139900090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
140000090623     C                   EVAL      VABNMO = SkSplitCSV(i)
140100090623     C                   ENDIF
140200090623     C*
140300090623     C* Reperisco quindi i campi numerici...
140400090623     C*** NSP
140500090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
140600090623     C                   EVAL      PiStr=SkSplitCSV(i)
140700090623     C                   EXSR      CHKNUM
140800090623     C                   IF        PiInt=*on
140900090623     C                   Z-ADD     PiVal         VABNSP
141000090623     C                   ELSE
141100090623     C                   SETON                                        31
141200090623     C                   EVAL      VABNSP = *zeros
141300090623     C                   EVAL      vinmsg = %trimr(vinmsg)
141400090623     C                             + ' ' + 'VABNSP'
141500090623     C                   ENDIF
141600090623     C                   ENDIF
141700090623     C*** RMN
141800090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
141900090623     C                   EVAL      PiStr=SkSplitCSV(i)
142000090623     C                   EXSR      CHKNUM
142100090623     C                   IF        PiInt=*on
142200090623     C                   Z-ADD     PiVal         VABRMN
142300090623     C                   ELSE
142400090623     C                   SETON                                        32
142500090623     C                   EVAL      VABRMN = *zeros
142600090623     C                   EVAL      vinmsg = %trimr(vinmsg)
142700090623     C                             + ' ' + 'VABRMN'
142800090623     C                   ENDIF
142900090623     C                   ENDIF
143000090623     C*** CAD
143100090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
143200090623     C                   EVAL      PiStr=SkSplitCSV(i)
143300090623     C                   EXSR      CHKNUM
143400090623     C                   IF        PiInt=*on
143500090623     C                   Z-ADD     PiVal         Num5_0
143600090623     C                   MOVEL(p)  Num5_0        VABCAD
143700090623     C                   ELSE
143800090623     C                   EVAL      VABCAD = SkSplitCSV(i)
143900090623     C                   SETON                                        32
144000090623     C                   EVAL      VABCAD = *zeros
144100090623     C                   EVAL      vinmsg = %trimr(vinmsg)
144200090623     C                             + ' ' + 'VABCAD'
144300090623     C                   ENDIF
144400090623     C                   ENDIF
144500090623     C*** NCL
144600090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
144700090623     C                   EVAL      PiStr=SkSplitCSV(i)
144800090623     C                   EXSR      CHKNUM
144900090623     C                   IF        PiInt=*on
145000090623     C                   Z-ADD     PiVal         VABNCL
145100090623     C                   ELSE
145200090623     C                   SETON                                        32
145300090623     C                   EVAL      VABNCL = *zeros
145400090623     C                   EVAL      vinmsg = %trimr(vinmsg)
145500090623     C                             + ' ' + 'VABNCL'
145600090623     C                   ENDIF
145700090623     C                   ENDIF
145800090623     C*** PKB
145900090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
146000090623     C                   EVAL      PiStr=SkSplitCSV(i)
146100090623     C                   EXSR      CHKNUM
146200090623     C                   IF        PiNum=*on
146300090623     C                   Z-ADD     PiVal         VABPKB
146400090623     C                   ELSE
146500090623     C                   SETON                                        32
146600090623     C                   EVAL      VABPKB = *zeros
146700090623     C                   EVAL      vinmsg = %trimr(vinmsg)
146800090623     C                             + ' ' + 'VABPKB'
146900090623     C                   ENDIF
147000090623     C                   ENDIF
147100090623     C*** CAS
147200090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
147300090623     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
147400090623     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
147500090623     C                             %trim(SkSplitCSV(i)) <> '0'      AND
147600090623     C                             %trim(SkSplitCSV(i)) <> '0,00'
147700090623     C                   EVAL      wFlgCAS = '1'
147800090623     C                   ENDIF
147900090623     C                   EVAL      PiStr=SkSplitCSV(i)
148000090623     C                   EXSR      CHKNUM
148100090623     C                   IF        PiNum=*on
148200090623     C                   Z-ADD     PiVal         VABCAS
148300090623     C                   ELSE
148400090623     C                   SETON                                        32
148500090623     C                   EVAL      VABCAS = *zeros
148600090623     C                   EVAL      vinmsg = %trimr(vinmsg)
148700090623     C                             + ' ' + 'VABCAS'
148800090623     C                   ENDIF
148900090623     C                   ENDIF
149000090623     C*** CCM
149100090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
149200090623     C                   EVAL      PiStr=SkSplitCSV(i)
149300090623     C                   EXSR      CHKNUM
149400090623     C                   IF        PiInt=*on      AND
149500090623     C                             PiVal<=9999999 AND
149600090623     C                             PiVal>*zeros
149700090623     C                   Z-ADD     PiVal         VABCCM
149800090623     C                   ELSE
149900090623     C                   SETON                                        32
150000090623     C                   EVAL      VABCCM = *zeros
150100090623     C                   EVAL      vinmsg = %trimr(vinmsg)
150200090623     C                             + ' ' + 'VABCCM'
150300090623     C                   ENDIF
150400090623     C                   ENDIF
150500090623     C*** LNP
150600090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
150700090623     C                   EVAL      PiStr=SkSplitCSV(i)
150800090623     C                   EXSR      CHKNUM
150900090623     C                   IF        PiInt=*on    AND
151000090623     C                             PiVal<=999   AND
151100090623     C                             PiVal>*zeros
151200090623     C                   Z-ADD     PiVal         VABLNP
151300090623     C                   ELSE
151400090623     C                   SETON                                        32
151500090623     C                   EVAL      VABLNP = *zeros
151600090623     C                   EVAL      vinmsg = %trimr(vinmsg)
151700090623     C                             + ' ' + 'VABLNP'
151800090623     C                   ENDIF
151900090623     C                   ENDIF
152000090623     C*** NRS
152100090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
152200090623     C                   EVAL      PiStr=SkSplitCSV(i)
152300090623     C                   EXSR      CHKNUM
152400090623     C                   IF        PiInt=*on AND
152500090623     C                             PiVal<=99
152600090623     C                   Z-ADD     PiVal         VABNRS
152700090623     C                   ELSE
152800090623     C                   SETON                                        32
152900090623     C                   EVAL      vinmsg = %trimr(vinmsg)
153000090623     C                             + ' ' + 'VABNRS'
153100090623     C                   ENDIF
153200090623     C                   ENDIF
153300090623     C*** LNA
153400090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
153500090623     C                   EVAL      PiStr=SkSplitCSV(i)
153600090623     C                   EXSR      CHKNUM
153700090623     C                   IF        PiInt=*on    AND
153800090623     C                             PiVal<=999   AND
153900090623     C                             PiVal>*zeros
154000090623     C                   Z-ADD     PiVal         VABLNA
154100090623     C                   ELSE
154200090623     C                   SETON                                        32
154300090623     C                   EVAL      VABLNA = *zeros
154400090623     C                   EVAL      vinmsg = %trimr(vinmsg)
154500090623     C                             + ' ' + 'VABLNA'
154600090623     C                   ENDIF
154700090623     C                   ENDIF
154800090623     C*** CTR
154900090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
155000090623     C                   EVAL      PiStr=SkSplitCSV(i)
155100090623     C                   EXSR      CHKNUM
155200090623     C                   IF        PiInt=*on  AND
155300090623     C                             PiVal<=999
155400090623     C                   Z-ADD     PiVal         VABCTR
155500090623     C                   ELSE
155600090623     C                   SETON                                        32
155700090623     C                   EVAL      vinmsg = %trimr(vinmsg)
155800090623     C                             + ' ' + 'VABCTR'
155900090623     C                   ENDIF
156000090623     C                   ENDIF
156100090623     C*** IAS
156200090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
156300090623     C                   EVAL      PiStr=SkSplitCSV(i)
156400090623     C                   EXSR      CHKNUM
156500090623     C                   IF        PiNum=*on
156600090623     C                   Z-ADD     PiVal         VABIAS
156700090623     C                   ELSE
156800090623     C                   SETON                                        32
156900090623     C                   EVAL      vinmsg = %trimr(vinmsg)
157000090623     C                             + ' ' + 'VABIAS'
157100090623     C                   ENDIF
157200090623     C                   ENDIF
157300090623     C*** VLB
157400090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
157500090623     C                   EVAL      PiStr=SkSplitCSV(i)
157600090623     C                   EXSR      CHKNUM
157700090623     C                   IF        PiNum=*on
157800090623     C                   Z-ADD     PiVal         VABVLB
157900090623     C                   ELSE
158000090623     C                   SETON                                        32
158100090623     C                   EVAL      vinmsg = %trimr(vinmsg)
158200090623     C                             + ' ' + 'VABVLB'
158300090623     C                   ENDIF
158400090623     C                   ENDIF
158500090623     C*** QFT
158600090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
158700090623     C                   EVAL      PiStr=SkSplitCSV(i)
158800090623     C                   EXSR      CHKNUM
158900090623     C                   IF        PiNum=*on
159000090623     C                   Z-ADD     PiVal         VABQFT
159100090623     C                   ELSE
159200090623     C                   SETON                                        32
159300090623     C                   EVAL      vinmsg = %trimr(vinmsg)
159400090623     C                             + ' ' + 'VABQFT'
159500090623     C                   ENDIF
159600090623     C                   ENDIF
159700090623     C*** NCD
159800090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
159900090623     C                   EVAL      PiStr=SkSplitCSV(i)
160000090623     C                   EXSR      CHKNUM
160100090623     C                   IF        PiInt=*on      AND
160200090623     C                             PiVal<=9999999
160300090623     C                   Z-ADD     PiVal         VABNCD
160400090623     C                   ELSE
160500090623     C                   SETON                                        32
160600090623     C                   EVAL      vinmsg = %trimr(vinmsg)
160700090623     C                             + ' ' + 'VABNCD'
160800090623     C                   ENDIF
160900090623     C                   ENDIF
161000090623     C*** NCA
161100090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
161200090623     C                   EVAL      PiStr=SkSplitCSV(i)
161300090623     C                   EXSR      CHKNUM
161400090623     C                   IF        PiInt=*on      AND
161500090623     C                             PiVal<=9999999
161600090623     C                   Z-ADD     PiVal         VABNCA
161700090623     C                   ELSE
161800090623     C                   SETON                                        32
161900090623     C                   EVAL      vinmsg = %trimr(vinmsg)
162000090623     C                             + ' ' + 'VABNCA'
162100090623     C                   ENDIF
162200090623     C                   ENDIF
162300090623     C*** ZNC
162400090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
162500090623     C                   EVAL      PiStr=SkSplitCSV(i)
162600090623     C                   EXSR      CHKNUM
162700090623     C                   IF        PiInt=*on AND
162800090623     C                             PiVal<=99
162900090623     C                   Z-ADD     PiVal         VABZNC
163000090623     C                   ELSE
163100090623     C                   SETON                                        32
163200090623     C                   EVAL      vinmsg = %trimr(vinmsg)
163300090623     C                             + ' ' + 'VABZNC'
163400090623     C                   ENDIF
163500090623     C                   ENDIF
163600090623     C*** DCR
163700090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
163800090623     C                   EVAL      PiStr=SkSplitCSV(i)
163900090623     C                   EXSR      CHKNUM
164000090623     C                   IF        PiInt=*on       AND
164100090623     C                             PiVal<=99999999
164200090623     C                   Z-ADD     PiVal         VABDCR
164300090623     C                   ELSE
164400090623     C                   SETON                                        32
164500090623     C                   EVAL      vinmsg = %trimr(vinmsg)
164600090623     C                             + ' ' + 'VABDCR'
164700090623     C                   ENDIF
164800090623     C                   ENDIF
164900090623     C*** HCR
165000090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
165100090623     C                   EVAL      PiStr=SkSplitCSV(i)
165200090623     C                   EXSR      CHKNUM
165300090623     C                   IF        PiInt=*on   AND
165400090623     C                             PiVal<=9999
165500090623     C                   Z-ADD     PiVal         VABHCR
165600090623     C                   ELSE
165700090623     C                   SETON                                        32
165800090623     C                   EVAL      vinmsg = %trimr(vinmsg)
165900090623     C                             + ' ' + 'VABHCR'
166000090623     C                   ENDIF
166100090623     C                   ENDIF
166200090623     C*** VMD
166300090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
166400090623     C                   EVAL      PiStr=SkSplitCSV(i)
166500090623     C                   EXSR      CHKNUM
166600090623     C                   IF        PiNum=*on
166700090623     C                   Z-ADD     PiVal         VABVMD
166800090623     C                   ELSE
166900090623     C                   SETON                                        32
167000090623     C                   EVAL      vinmsg = %trimr(vinmsg)
167100090623     C                             + ' ' + 'VABVMD'
167200090623     C                   ENDIF
167300090623     C                   ENDIF
167400090623     C*** ANT
167500090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
167600090623     C                   EVAL      PiStr=SkSplitCSV(i)
167700090623     C                   EXSR      CHKNUM
167800090623     C                   IF        PiInt=*on        AND
167900090623     C                             PiVal<=999999999
168000090623     C                   Z-ADD     PiVal         VABANT
168100090623     C                   ELSE
168200090623     C                   SETON                                        32
168300090623     C                   EVAL      vinmsg = %trimr(vinmsg)
168400090623     C                             + ' ' + 'VABANT'
168500090623     C                   ENDIF
168600090623     C                   ENDIF
168700090623     C*** CMO
168800090623     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
168900090623     C                   EVAL      PiStr=SkSplitCSV(i)
169000090623     C                   EXSR      CHKNUM
169100090623     C                   IF        PiInt=*on
169200090623     C                   Z-ADD     PiVal         Num5_0
169300090623     C                   MOVEL(p)  Num5_0        VABCMO
169400090623     C                   ELSE
169500090623     C                   SETON                                        32
169600090623     C                   EVAL      VABCMO = *zeros
169700090623     C                   EVAL      vinmsg = %trimr(vinmsg)
169800090623     C                             + ' ' + 'VABCMO'
169900090623     C                   ENDIF
170000090623     C                   ENDIF
170100090623     C*
170200090911     C* Reperisco i campi del EDIVAT
170300090623     C*** VATNOT
170400090623     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
170500090623     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
170600090623     C                   ENDIF
170700090623     C*** VATTRC
170800090623     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
170900090623     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
171000090623     C                   ENDIF
171100090623     C*** VATNOT_A
171200090623     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
171300090623     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
171400090623     C                   ENDIF
171500090623     C*** VATNOT_B
171600090623     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
171700090623     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
171800090623     C                   ENDIF
171900090623     C*** VATNOT_E
172000090623     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
172100090623     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
172200090623     C                   ENDIF
172300090623     C*
172400090623     C***  ===>  Gestione campi "particolari" dentro ciclo
172500090623     C*
172600090623     C*** W_VABNSP
172700090623     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNSP'
172800090623     C                   EVAL      PiStr=SkSplitCSV(i)
172900090623     C                   EXSR      CHKNUM
173000090623     C                   IF        PiInt=*on
173100090623     C                   Z-ADD     PiVal         VABNSP
173200090623     C                   Z-ADD     PiVal         VABRMN
173300090623     C                   ELSE
173400090623     C                   SETON                                        31
173500090623     C                   EVAL      VABNSP = *zeros
173600090623     C                   EVAL      VABRMN = *zeros
173700090623     C                   EVAL      vinmsg = %trimr(vinmsg)
173800090623     C                             + ' ' + 'VABNSP VABRMN'
173900090623     C                   ENDIF
174000090623     C                   ENDIF
174100090623     C*** W_VABIND_1
174200090623     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABIND_1'
174300090623     C                   EVAL      VABIND = %trim(VABIND) + ' ' +
174400090623     C                                      SkSplitCSV(i)
174500090623     C                   ENDIF
174600090623     C*** W_VABIND_2
174700090623     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABIND_2'
174800090623     C                   EVAL      VABIND = %trim(VABIND) + ' ' +
174900090623     C                                      SkSplitCSV(i)
175000090623     C                   ENDIF
175100090623     C*** W_VABNZD
175200090623     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNZD'
175300090623     C                   IF        SkSplitCSV(i) <> 'IT'
175400090623     C                   EVAL      VABNZD = SkSplitCSV(i)
175500090623     C                   ENDIF
175600090623     C                   ENDIF
175700090623     C*** W_VABNOT_1
175800090623     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT_1'
175900090623     C                   EVAL      wNOTE  = %trim(wNOTE) + ' ' +
176000090623     C                                      SkSplitCSV(i)
176100090623     C                   ENDIF
176200090623     C*** W_VABNOT_2
176300090623     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT_2'
176400090623     C                   EVAL      wNOTE  = %trim(wNOTE) + ' ' +
176500090623     C                                      SkSplitCSV(i)
176600090623     C                   ENDIF
176700090623     C*** W_VABNOT_3
176800090623     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT_3'
176900090623     C                   EVAL      wNOTE  = %trim(wNOTE) + ' ' +
177000090623     C                                      SkSplitCSV(i)
177100090623     C                   ENDIF
177200090623     C*** W_VABNOT_4
177300090623     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT_4'
177400090623     C                   EVAL      wNOTE  = %trim(wNOTE) + ' ' +
177500090623     C                                      SkSplitCSV(i)
177600090623     C                   ENDIF
177700090623     C*** W_VABNOT_5
177800090623     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT_5'
177900090623     C                   EVAL      wNOTE  = %trim(wNOTE) + ' ' +
178000090623     C                                      SkSplitCSV(i)
178100090623     C                   ENDIF
178200090904     C*
178300090904     C* Sommo peso e volume solamente a rottura d "chi sono"
178400090904     C*
178500090904     C* => se "chi sono" campo 7
178600090904     C                   IF        SkSplitCSV(7) <> *blanks
178700090904     C*** W_VABPKB
178800090904     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABPKB'
178900090909     C                   IF        SkSplitCSV(7) <> sVATNOT_E
179000090904     C                   EVAL      PiStr='0'+SkSplitCSV(i)
179100090904     C                   EXSR      CHKNUM
179200090904     C                   IF        PiNum=*on
179300090904     C                   ADD       PiVal         VABPKB
179400090904     C                   ELSE
179500090904     C                   SETON                                        32
179600090904     C                   EVAL      vinmsg = %trimr(vinmsg)
179700090904     C                             + ' ' + 'VABPKB'
179800090904     C                   ENDIF
179900090904     C                   ENDIF
180000090904     C                   ENDIF
180100090904     C*** W_VABVLB
180200090904     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABVLB'
180300090909     C                   IF        SkSplitCSV(7) <> sVATNOT_E
180400090904     C                   EVAL      PiStr='0'+SkSplitCSV(i)
180500090904     C                   EXSR      CHKNUM
180600090904     C                   IF        PiNum=*on
180700090904     C                   ADD       PiVal         VABVLB
180800090904     C                   ELSE
180900090904     C                   SETON                                        32
181000090904     C                   EVAL      vinmsg = %trimr(vinmsg)
181100090904     C                             + ' ' + 'VABVLB'
181200090904     C                   ENDIF
181300090904     C                   ENDIF
181400090904     C                   ENDIF
181500090904     C*
181600090904     C                   ELSE
181700090904     C*
181800090904     C* => se "chi sono" campo 26
181900090904     C                   IF        SkSplitCSV(26) <> *blanks
182000090904     C*** W_VABPKB
182100090904     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABPKB'
182200090904     C                   IF        '00'+SkSplitCSV(26) <> sVATNOT_E
182300090904     C                   EVAL      PiStr='0'+SkSplitCSV(i)
182400090904     C                   EXSR      CHKNUM
182500090904     C                   IF        PiNum=*on
182600090904     C                   ADD       PiVal         VABPKB
182700090904     C                   ELSE
182800090904     C                   SETON                                        32
182900090904     C                   EVAL      vinmsg = %trimr(vinmsg)
183000090904     C                             + ' ' + 'VABPKB'
183100090904     C                   ENDIF
183200090904     C                   ENDIF
183300090904     C                   ENDIF
183400090904     C*** W_VABVLB
183500090904     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABVLB'
183600090904     C                   IF        '00'+SkSplitCSV(26) <> sVATNOT_E
183700090904     C                   EVAL      PiStr='0'+SkSplitCSV(i)
183800090904     C                   EXSR      CHKNUM
183900090904     C                   IF        PiNum=*on
184000090904     C                   ADD       PiVal         VABVLB
184100090904     C                   ELSE
184200090904     C                   SETON                                        32
184300090904     C                   EVAL      vinmsg = %trimr(vinmsg)
184400090904     C                             + ' ' + 'VABVLB'
184500090904     C                   ENDIF
184600090904     C                   ENDIF
184700090904     C                   ENDIF
184800090904     C*
184900090904     C                   ENDIF
185000090904     C                   ENDIF
185100090904     C*
185200090904     C*** W_VATNOT_E_1
185300090904     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_E_1'
185400090904     C                   EVAL      wVATNOT_E_1 = '00'+%trim(SkSplitCSV(i))
185500090703     C                   ENDIF
185600090904     C*** W_VATNOT_E_2
185700090904     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_E_2'
185800090909     C                   EVAL      wVATNOT_E_2 = %trim(SkSplitCSV(i))
185900090904     C                   ENDIF
186000090623     C*
186100090623     C***  <===  -----------------------------------------
186200090908     C*
186300090908     C                   IF        %trim(wVATNOT_E_2) <> *blanks
186400090908     C                   EVAL      wVATNOT_E = wVATNOT_E_2
186500090908     C                   ELSE
186600090908     C                   EVAL      wVATNOT_E = wVATNOT_E_1
186700090908     C                   ENDIF
186800090623     C*
186900090623     C                   ENDSR
187000090623     C***
187100090623
187200010601
187300020204
187400020204     C*----------------------------------------------------*
187500020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
187600020204     C*----------------------------------------------------*
187700020204     C     CHKIMPDIV     BEGSR
187800020204     C*
187900020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
188000020204     C                   Z-ADD     *zeros        wrkDec            9 9
188100020204     C*
188200020204     C* Come prima cosa effettuo considerazioni sulla divisa
188300020204     C                   IF        vabIAS > *zeros
188400020204     C                   IF        vabVAS <> 'EUR'
188500020204     C                   EVAL      vabVAS =  'ITL'
188600020204     C                   ENDIF
188700020204     C                   ENDIF
188800020204     C*
188900020204     C                   IF        vabCAS > *zeros
189000020204     C                   IF        vabVCA <> 'EUR'
189100020204     C                   EVAL      vabVCA =  'ITL'
189200020204     C                   ENDIF
189300020204     C                   ENDIF
189400020204     C*
189500020204     C                   IF        vabVMD > *zeros
189600020204     C                   IF        vabVAD <> 'EUR'
189700020204     C                   EVAL      vabVAD =  'ITL'
189800020204     C                   ENDIF
189900020204     C                   ENDIF
190000020204     C*
190100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
190200020204     C                   Z-ADD     vabIAS        wrkDec
190300020204     C                   IF        wrkDec > *zeros
190400020204     C                   IF        vabVAS = 'ITL'
190500020204     C                   EVAL      vabIAS = *zeros
190600020204     C                   ENDIF
190700020204     C                   ENDIF
190800020204     C*
190900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
191000020204     C                   Z-ADD     vabCAS        wrkDec
191100020204     C                   IF        wrkDec > *zeros
191200020204     C                   IF        vabVCA = 'ITL'
191300020204     C                   EVAL      vabCAS = *zeros
191400020204     C                   ENDIF
191500020204     C                   ENDIF
191600020204     C*
191700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
191800020204     C                   Z-ADD     vabVMD        wrkDec
191900020204     C                   IF        wrkDec > *zeros
192000020204     C                   IF        vabVAD = 'ITL'
192100020204     C                   EVAL      vabVMD = *zeros
192200020204     C                   ENDIF
192300020204     C                   ENDIF
192400020204     C*
192500020204     C                   ENDSR
192600020204     C***
192700080617
192800080617
192900080617
193000080617     C*----------------------------------------------------*
193100080617     C*  CONTROLLO NUMERICITA' CAMPI
193200080617     C*----------------------------------------------------*
193300080617     C     CHKNUM        BEGSR
193400080617     C*
193500080617     C                   IF        PiDecChr = *blanks
193600080617     C                   EVAL      PiDecChr = CharNUM
193700080617     C                   ENDIF
193800080617     C*
193900080617     C                   callp     UBISNUM_Check(PiStr
194000080617     C                                          :PiDecChr
194100080617     C                                          :PiVal
194200080617     C                                          :PiNum
194300080617     C                                          :PiInt)
194400080617     C*
194500080617     C                   ENDSR
194600080617     C***
194700010330
194800010601
194900010601
195000010601
195100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
195200010601     C     repfil        BEGSR
195300010601     C*
195400010601     C                   if        invfil = *zeros and
195500010601     C                             depfil > *zeros and
195600010629     C                             (vinflg = *blanks or
195700010629     C                              vinflg = *zeros)
195800010601     C
195900010601     C                   eval      invfil = depfil
196000010601     C                   endif
196100010601     C*
196200010601     C                   if        depfil <> invfil and
196300010601     C                             invfil > *zeros
196400010601     C                   eval      flgMulti = '1'
196500010601     C                   if        vinflg = *blanks
196600010601     C                   add       1             cntNonEl
196700010601     C                   endif
196800010601     C                   endif
196900010601     C*
197000010601     C                   if        vinflg = '2'
197100010601     C                   eval      flgStato = '2'
197200010601     C                   endif
197300010601     C*
197400010601     C                   ENDSR
197500010601     C***
197600010601
197700010601
197800010601
197900010330
198000040119      /TITLE Invio dei dati al punto operativo.
198100040119     C     invio         BEGSR
198200040119     C*
198300101004     C* 1° invio EDIVAT
198400040119     C                   reset                   dscmz
198500040119     C                   move      vlrpoi        cmzdst
198600090911     C                   eval      cmzfld = 'EDIVATWR'
198700040119     C                   eval      cmzmbd = vlrhdl
198800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
198900040119     C***                if        prmfir = *blanks
199000090911     C                   eval      cmzfla = 'EDIVAT0F'
199100090911     C                   eval      cmzmba = 'EDIVAT0F'
199200040119     C***                else
199300040119     C***                eval      cmzfla = prmfir
199400040119     C***                eval      cmzmba = prmfir
199500040119     C***                endif
199600040119     C                   eval      cmznrr = *zeros
199700040119     C                   move      §ctrokvt      cmznrr
199800040119     C                   eval      cmzlba = vlrfl1
199900040119     C                   call(e)   'TIS711C'
200000040119     C                   parm                    dscmz
200100040119     C                   parm      *blanks       esito
200200040119     C                   if        %error
200300040119     C                             or cmzerr = '1'
200400040119     C                             or esito  = '1'
200500040119     C                   eval      wrkesito = '3'
200600040119     C                   else
200700040119     C*
200800101004     C* 2° invio EDIVAB
200900040119     C                   reset                   dscmz
201000040119     C                   move      vlrpoi        cmzdst
201100040119     C                   eval      cmzfld = vlrfou
201200040119     C                   eval      cmzmbd = vlrhdl
201300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
201400040119     C***                if        prmfir = *blanks
201500090911     C                   eval      cmzfla = 'EDIVAB0F'
201600090911     C                   eval      cmzmba = 'EDIVAB0F'
201700040119     C***                else
201800040119     C***                eval      cmzfla = prmfir
201900040119     C***                eval      cmzmba = prmfir
202000040119     C***                endif
202100040119     C                   eval      cmznrr = *zeros
202200040119     C                   move      §ctrokvb      cmznrr
202300040119     C                   eval      cmzlba = vlrfl1
202400040119     C                   call(e)   'TIS711C'
202500040119     C                   parm                    dscmz
202600040119     C                   parm      *blanks       esito
202700040119     C                   if        %error
202800040119     C                             or cmzerr = '1'
202900040119     C                             or esito  = '1'
203000040119     C                   eval      wrkesito = '3'
203100040119     C                   endif
203200040119     C                   endif
203300040119     C*
203400040119     C                   ENDSR
203500040119     C***
203600010601
203700010601
203800010601
203900010601
204000010601      /TITLE Invio dei dati al punto operativo.
204100010601     C     opeini        BEGSR
204200010601     C*
204300010601     C* Inizializzo flag e contatori operativi
204400010601     C                   movel     '0'           flgGiro           1
204500010601     C                   movel     '0'           flgMulti          1
204600010601     C                   movel     '1'           flgStato          1
204700010615     C                   movel     '0'           flgOk             1
204800010601     C                   z-add     *zeros        cntNonEl         10 0
204900010601     C                   z-add     *zeros        depfil            3 0
205000010601     C                   z-add     *zeros        invfil            3 0
205100010601     C*
205200010601     C                   ENDSR
205300010601     C***
205400070326
205500070326
205600070326
205700070326
205800080916     C     *pssr         BEGSR
205900070329     C*
206000080916     C                   if        %open(tivin00r)
206100080916     C                   close     tivin00r
206200080916     C                   endif
206300090911     C                   if        %open(edivabwr)
206400090911     C                   close     edivabwr
206500080916     C                   endif
206600090911     C                   if        %open(edivatwr)
206700090911     C                   close     edivatwr
206800080916     C                   endif
206900070326     C*
207000070326     C* Effettuo la chiamata al CLLE preposto
207100101004     C                   call(e)   'TITVEVTC'
207200080916     C                   parm                    parccm
207300080916     C                   parm                    parmbr
207400080916     C                   parm      '2'           paropz
207500070326     C*
207600080916     C                   eval      wrkesito = '2'
207700070404     C*
207800080916     C                   seton                                        LR
207900070326     C*
208000080916     C                   ENDSR     '*CANCL'
208100070326     C***
208200070326
208300070326
208400010330
208500010330
208600000613     C     *inzsr        BEGSR
208700990910     C*
208800990910     C     *entry        plist
208900990920     C                   parm                    tivlrds
209000990921     C                   parm      wrkesito      esito
209100000724     C                   parm                    prmlit
209200000710     C                   parm                    prmfir
209300090911     C*
209400090911     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
209500090911     C                   move(p)   vlrMSG        wNomeFile
209600090911     C*
209700090911     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
209800090911     C* del codice CMR dal nome del file importato.
209900090911     C                   if        wNomeFile <> *blanks AND
210000090911     C                             vlrppt<>*blanks
210100090911     C                   eval      wCMR = %subst(wNomeFile:7:6)
210200090911     C                   else
210300090911     C                   eval      wCMR = *all'*'
210400090911     C                   endif
210500010330     C*
210600010330     C* CALCOLA LA DATA CORRENTE
210700010330     C                   time                    wn14             14 0
210800010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
210900090911     C                   movel     wn14          oracor            6 0          *ORA
211000010330     C                   z-add     wn8           g08dat
211100010330     C                   z-add     *zeros        g08inv
211200010330     C                   movel     '0'           g08err
211300010330     C                   call      'XSRDA8'
211400010330     C                   parm                    wlbda8
211500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
211600000613     C*
211700000613     C                   ENDSR
211800000613     C***
211900990908
212000090623     Otitv1q7p  E            riepilogo         2
212100990915     O                                              'Upload via Internet'
212200990915     O                                           +1 'Traduzione TIVIN00R -'
212300090911     O                                           +1 'EDIVABWR'
212400990915     O                                           +1 'Testate bolle'
212500990915     O          E            riepilogo   2
212600990915     O                       wrkdata
212700990915     O                       wrkora              +1
212800990915     O                       procname            +1
212900990915     O          E            riepilogo   2
213000990915     O                                              'Cliente..................:'
213100010605     O                       VABCCM        z     +1
213200990915     O          E            riepilogo   2
213300990920     O                                              'Riferimento Strategi.....:'
213400990920     O                       vlrhdl              +1
213500990915     O          E            riepilogo   2
213600990915     O                                              'Giusti...................:'
213700040119     O                       §CTROKVB      2   +  1
213800990915     O          E            riepilogo   2
213900990915     O                                              'Sbagliati e corretti.....:'
214000971022     O                       §CTRMO        2   +  1
214100990915     O          E            riepilogo   2
214200990915     O                                              'Sbagliati e scartati.....:'
214300971022     O                       §CTRNO        2   +  1
214400000613
214500090623     Otitv1q7ps E            testdett          2
214600000613     O                                              'Upload via Internet'
214700000613     O                                           +1 'Traduzione TIVIN00R -'
214800090911     O                                           +1 'EDIVABWR'
214900000613     O                                           +1 'Testate bolle'
215000000616     O          E            testdett    3
215100000613     O                                           +2 'N° rec'
215200000613     O                                           +3 'Anteprima contenuto'
215300000616     O          E            rigadett    2
215400000613     O                       rrnum               +2
215500000621     O                       recko               +3
215600000616     O          E            findett     2
215700000613     O                       wrkdata
215800000613     O                       wrkora              +1
215900000613     O                       procname            +1
216000000616     O          E            findett     2
216100000613     O                                              'Cliente..................:'
216200010605     O                       VABCCM        z     +1
216300000616     O          E            findett     2
216400000613     O                                              'Riferimento Strategi.....:'
216500000613     O                       vlrhdl              +1
216600000616     O          E            findett     2
216700000613     O                                              'Giusti...................:'
216800040119     O                       §CTROKVB      2   +  1
216900000616     O          E            findett     2
217000000613     O                                              'Sbagliati e corretti.....:'
217100000613     O                       §CTRMO        2   +  1
217200000616     O          E            findett     2
217300000613     O                                              'Sbagliati e scartati.....:'
217400000613     O                       §CTRNO        2   +  1
217500000613** CMD - COMANDI CL
217600090623OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1Q7P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
217700090623OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1Q7PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
217800090623DLTOVR FILE(TITV1Q7P TITV1Q7PS) LVL(*)
217900010606
218000000613
218100090623** ENTEXP - INTESTAZIONE COLONNE PERSONALIZZATA TIPO RECORD "ENTEXP"
218200101004X;X;X;X;W_VABNSP;X;X;X;X;X;X;X;X;X;VABRSD;VABRD2;
218300090623W_VABIND_1;W_VABIND_2;VABLOD;VABCAD;X;W_VABNZD;X;X;
218400090623W_VABNOT_1;W_VABNOT_2;W_VABNOT_3;W_VABNOT_4;W_VABNOT_5;VABPRD;X;X;X;
218500101004X;X;X;X;X;X;X;X;X;X;X;X;X
218600090623
218700090623** LIGEXP - INTESTAZIONE COLONNE PERSONALIZZATA TIPO RECORD "LIGEXP"
218800101004X;X;X;X;W_VABPKB;X;W_VATNOT_E_2;X;X;X;X;X;X;X;X;X;X;
218900101004X;X;X;X;X;X;X;X;W_VATNOT_E_1;X;X;X;X;X;X;X;X;X;X;
219000090623X;X;X;X;X;X;X;
219100090623
219200090623
219300090624** PRTCONST - COSTANTI IN STAMPA
219400090624CODICE CLIENTE         NUM.ORDINE        NUM.BP
219500090624_______________        __________        __________
219600090703   ARTICOLO       DESCRIZIONE                                Q.TA'
219700090624   __________     ___________________________________        __________
219800090624____________________________________________________________________________
