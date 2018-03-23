000100120306      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200100318     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600120306     FEDIVABwr  O    E             DISK    usropn prefix(ok_)
000700120306     FEDIVATwr  O    E             DISK    usropn
000800100720     Ftitv1y7p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070329     F                                     infsr(*pssr)
001100100720     Ftitv1y7ps O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070329     F                                     infsr(*pssr)
001400000313     D*
001500000613     D*------------
001600000613     D* COMANDI
001700000613     D*------------
001800011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900040113     D*------------
002000040113     D* SCHIEDE A PROGRAMMA
002100040113     D*------------
002200080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
002300010330     D*----------------------------------------------------
002400010330     D* DICHIARAZIOINE VARIABILI DI WRK
002500010330     D*----------------------------------------------------
002600010330     D dscmz         e ds                  inz
002700010330     D psds           sds
002800010330     D  procname         *PROC
002900010330     D tivlrds       e ds                  extname(tivlr00f)
003000040128     D tisi95ds      e ds
003100120306     D edivabs       e ds                  extname(edivab0f)
003200120306     D edivabs_sav   e ds                  extname(edivab0f) prefix(sav_)
003300120306     D edivabs_ok    e ds                  extname(edivab0f) prefix(ok_)
003400010330     D esito           s              1
003500010330     D prmlit          s             10
003600010330     D prmfir          s             10
003700010330     D wrkesito        s                   like(esito)
003800010330     D wrkdata         s               d
003900010330     D wrkora          s               t
004000010330     D rrnum           s              6  0 INZ(*zeros)
004100010330     D recko           s            150    INZ(*blanks)
004200011113     D depcmd          s            150    INZ(*blanks)
004300040510     D parccm          s              8    INZ(*blanks)
004400040119     D parmbr          s             10    INZ(*blanks)
004500040119     D paropz          s              1    INZ(*blanks)
004600040119     D chkcall         s              1    INZ(*blanks)
004700080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004800080418     D wFlgCAS         s              1    INZ(*blanks)
004900100326     D*------------------
005000100326     D* DS REPERIMENTO NUMERATORE
005100100326     D*------------------
005200100326     D trul33ds      e ds                  inz
005300100326     D*------------------
005400100326     D* DS ARCHITETTURA
005500100326     D*------------------
005600100326     D kpjba         e ds                  inz
005700100326
005800030715     D*------------------
005900030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
006000030715     D*------------------
006100030715     D SkSplitFLD      S             10    DIM(100)
006200030715     D SkSplitCSV      S            256    DIM(100)
006300030715     D CharCSV         S              1
006400030715     D CharTXT         S              1
006500030715     D CharNUM         S              1
006600100318     D CharSOS         S              1
006700030715     D posDa           S              3  0 INZ(*zeros)
006800030715     D posA            S              3  0 INZ(*zeros)
006900030715     D i               s              3  0 INZ(1)
007000030715     D wGiro           s              1  0 INZ(*zeros)
007100070823     D wVATNOT_A       s                   LIKE(VATNOT)
007200060331     D wVATNOT_B       s                   LIKE(VATNOT)
007300060331     D wVATNOT_E       s                   LIKE(VATNOT)
007400030822     D*------------------
007500030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
007600030822     D*------------------
007700030822     D posDaDft        S              3  0 INZ(*zeros)
007800030822     D posADft         S              3  0 INZ(*zeros)
007900030822     D j               s              3  0 INZ(1)
008000030822     D wGiroDft        s              1  0 INZ(*zeros)
008100080418     D*------------------
008200080418     D* Costanti
008300080418     D*------------------
008400080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008500080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008600100318
008700100318     D*------------------
008800100318     D* LINKING A DEFINIZIONI ESTERNE
008900100318     D*------------------
009000100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009100100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009200100318
009300010330
009400010330
009500010330
009600990915     C                   time                    wrkdata
009700990915     C                   time                    wrkora
009800000913     C                   reset                   rrnum
009900990921     C                   reset                   esito
010000990921     C                   reset                   wrkesito
010100010601     C*
010200010601     C                   exsr      opeini
010300010605     C                   exsr      rwvab
010400010601     C*
010500010601     C                   seton                                        lr
010600010601
010700010601
010800010601
010900010601     C*--------------------------------------------------------
011000010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011100010601     C*--------------------------------------------------------
011200010601     C     PREELA        BEGSR
011300010601     C*
011400010601     C* SE OCCORRE SPEDIRE IN FILIALE
011500010601     C                   if        invfil <> *zeros and
011600010601     C                             flgGiro = '0'
011700010601     C*
011800010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011900010601     C                   eval      flgGiro = '1'
012000000724     C*
012100000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
012200000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012300010601     C     invfil        chain     azorg01l
012400000724     C                   if        %found
012500000724     C                   movel(p)  CMD(1)        depcmd
012600020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
012700000724     C*
012800000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
012900011113     C                   Z-ADD     150           LENGH            15 5
013000000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013100000724     C                   PARM                    depcmd
013200000724     C                   PARM                    LENGH
013300000724     C*
013400000724     C                   endif
013500000616     C*
013600000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
013700000616     C                   movel(p)  CMD(2)        depcmd
013800000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
013900000616     C*
014000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
014100011113     C                   Z-ADD     150           LENGH            15 5
014200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014300000616     C                   PARM                    depcmd
014400000616     C                   PARM                    LENGH
014500000613     C*
014600100720     C                   if        not %open(titv1y7ps)
014700100720     C                   open      titv1y7ps
014800070329     C                   except    testdett
014900000613     C                   endif
015000010601     C*
015100010601     C                   endif
015200010601     C*
015300010601     C                   ENDSR
015400010601     C***
015500010601
015600010601
015700010601
015800010601
015900010601     C*--------------------------------------------------------
016000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
016100010601     C*--------------------------------------------------------
016200010601     C     ENDELA        BEGSR
016300990915     C*
016400000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
016500040119     C*
016600040119     C* Effettuo la chiamata al CLLE preposto
016700120306     C                   call(e)   'TITVEVTC'
016800040510     C                   parm                    parccm
016900040119     C                   parm                    parmbr
017000040119     C                   parm      '2'           paropz
017100050201     C*
017200050201     C* Effettuo lancio TISI95 solo x chiusura
017300050201     C                   CLEAR                   TISI95DS
017400050201     C                   EVAL      I95TLA = 'C'
017500050201     C                   CALL      'TISI95R'
017600050201     C                   PARM                    TISI95DS
017700000613     C*
017800100720     C                   if        %open(titv1y7ps)
017900000616     C                   except    findett
018000100720     C                   close     titv1y7ps
018100000613     C                   endif
018200000616     C*
018300000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
018400011113     C                   Z-ADD     150           LENGH            15 5
018500010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
018600000616     C                   PARM                    CMD(3)
018700000616     C                   PARM                    LENGH
018800000616     C*
018900010601     C                   ENDSR
019000010601     C***
019100010601
019200010601
019300010601
019400000613
019500000613     C*--------------------------------------------------------
019600000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
019700000613     C*--------------------------------------------------------
019800000621     C     STPR          BEGSR
019900000613     C*
020000100720     C                   if        not %open(titv1y7p)
020100100720     C   50              open      titv1y7p
020200990915     C                   endif
020300990915     C*
020400030715     C   50              except    riepilogo
020500990915     C*
020600100720     C                   if        %open(titv1y7p)
020700100720     C   50              close     titv1y7p
020800990914     C                   endif
020900000613     C*
021000000613     C                   ENDSR
021100000613     C***
021200000613
021300010601
021400010601
021500010330     C*--------------------------------------------------------
021600120306     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
021700010330     C*--------------------------------------------------------
021800010605     C     RWVAB         BEGSR
021900010330     C*
022000010330     C                   if        not %open(tivin00r)
022100010330     C                   open      tivin00r
022200010330     C                   endif
022300120306     C                   if        not %open(edivabwr)
022400120306     C                   open      edivabwr
022500010330     C                   endif
022600040119     C*
022700120306     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
022800040119     C                   exsr      prevat
022900040119     C*
023000040119     C                   if        chkcall = '0'
023100040119     C*
023200120306     C                   if        not %open(edivatwr)
023300120306     C                   open      edivatwr
023400040119     C                   endif
023500010330     C*
023600040119     C                   clear                   §CTROKVB          7 0
023700040119     C                   clear                   §CTROKVT          7 0
023800040119     C                   clear                   §CTRMO            7 0
023900040119     C                   clear                   §CTRNO            7 0
024000070823     C*
024100070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
024200070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
024300100318     C***                exsr      inzvar
024400100318     C***                exsr      impvab
024500010330     C*
024600030822     C                   do        *HIVAL
024700010330     C*
024800030822     C                   read      tivin00r                               70
024900120306     C*
025000120306 xxx C                   seton                                        65
025100120306     C*
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
026800010618     C*
026900040510     C                   if        vindta > *blanks AND
027000040510     C                             wFlgVINDTA = 1
027100010330     C                   add       1             rrnum
027200010330     C*
027300010601     C                   if        *in70 = *off and
027400010330     C                             (vinflg = *blanks
027500010330     C                              or vinflg = '0'
027600010330     C                              or vinflg = '2')
027700010330     C*
027800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
027900010711     C                   if        vinflg = *blanks or vinflg = '0'
028000010711     C                   clear                   vinmsg
028100010711     C                   endif
028200010601     C*
028300070925     C* Aggiungo un byte a blanks all'inizio d ogni record
028400070925     C                   eval      vindta  = ' ' + %trim(vindta)
028500070925     C*
028600080527     C                   exsr      inzvar
028700100318     C                   exsr      normCSV
028800010605     C                   exsr      impvab
028900010601     C*
029000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
029100010605     C                   eval      depfil = VABLNP
029200010601     C                   exsr      repfil
029300010601     C                   if        depfil = invfil
029400021025     C                   if        vlrpoi = 999
029500030822     C                   move(P)   invfil        VABFGS
029600021025     C                   else
029700030822     C                   move(P)   vlrpoi        VABFGS
029800021025     C                   endif
029900010601     C*
030000010601     C                   exsr      PREELA
030100010601     C*
030200010604     C* Ebbene...
030300010604     C*
030400040119     C  N31              add       1             §CTROKVB
030500040119     C   32              add       1             §CTRMO
030600040119     C   31              add       1             §CTRNO
030700080418     C*
030800030822     C                   if        wGiro = 2
030900080513     C* Scrivo sempre il VAT
031000080513     C  N31              exsr      wrivat
031100080527     C                   exsr      chkvab
031200080513     C                   endif
031300010604     C*
031400010604     C                   if        *in31 = *off and
031500010604     C                             *in32 = *off
031600010604     C                   eval      vinflg = '1'
031700010604     C                   else
031800010604     C                   eval      recko = vindta
031900010604     C                   except    rigadett
032000010604     C                   eval      vinflg = '2'
032100010604     C                   endif
032200010604     C                   endif
032300010601     C*
032400010604     C                   endif
032500010604     C*
032600010330     C                   else
032700010330     C                   eval      vinflg = '1'
032800010330     C                   endif
032900010601     C*
033000010601     C  N70              update    tivin000
033100010330     C*
033200030822     C  N70              enddo
033300040119     C*
033400080527     C* Verifico dati rimasti "in cannna"...
033500080527     C                   seton                                        65
033600080527     C                   exsr      chkvab
033700080527     C*
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
036700120306     C                   if        %open(edivabwr)
036800120306     C                   close     edivabwr
036900010330     C                   endif
037000120306     C                   if        %open(edivatwr)
037100120306     C                   close     edivatwr
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
038400010601     C                   exsr      endela
038500010618     C                   endif
038600010330     C*
038700010330     C                   ENDSR
038800010330     C***
038900080527
039000080527
039100080527
039200080527     C*----------------------------------------------------*
039300080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
039400080527     C*----------------------------------------------------*
039500100413     C     CHKVAB        BEGSR
039600080527     C*
039700080527     C* Scrivo il VAB solo a rottura d spedizione
039800080527     C                   if        depspe = *zeros
039900120306     C  N31              eval      edivabs_sav = edivabs
040000080527     C                   eval      depspe = vabNSP
040100080527     C                   else
040200080527     C                   if        depspe = vabNSP AND *in65 = *off
040300100413     C   51              add       VABNCL        sav_VABNCL
040400100413     C   51              add       VABPKB        sav_VABPKB
040500100413     C   51              add       VABVLB        sav_VABVLB
040600100413     C  N51              z-add     VABNCL        sav_VABNCL
040700100413     C  N51              z-add     VABPKB        sav_VABPKB
040800100413     C  N51              z-add     VABVLB        sav_VABVLB
040900080527     C                   else
041000120306     C  N31              eval      edivabs_ok = edivabs_sav
041100080527     C  N31              exsr      wrivab
041200120306     C  N31              eval      edivabs_sav = edivabs
041300080527     C                   eval      depspe = vabNSP
041400080527     C                   endif
041500080527     C                   endif
041600080527     C*
041700080527     C                   ENDSR
041800040119
041900040119
042000040119
042100040119     C*----------------------------------------------------*
042200040119     C*  SCARICAMENTO BUFFER RECORDS VAB
042300040119     C*----------------------------------------------------*
042400040510     C     WRIVAB        BEGSR
042500080513     C*
042600080513     C* Gestisco l'eventuale rottura x numero spedizione
042700080527     C                   if        VABNSP <> depspe or *in65 = *on
042800070301     C                   if        wVATNOT_E <> *blanks
042900070301     C                   eval      VABCTM = '7Q'
043000070301     C                   endif
043100120306     C*
043200120306     C* Imposto i dati del CMR
043300120306     C                   eval      ok_VABCMR = 'LOHMANN'
043400120306     C                   eval      ok_VABDCM = datcor
043500120306     C                   eval      ok_VABCNT = 1
043600120306     C*
043700120306     C                   write     edivab00
043800080513     C                   endif
043900040119     C*
044000040119     C                   ENDSR
044100040119
044200040119
044300040119
044400040119     C*----------------------------------------------------*
044500040119     C*  SCARICAMENTO BUFFER RECORDS VAT
044600040119     C*----------------------------------------------------*
044700040510     C     WRIVAT        BEGSR
044800060512     C*
044900060512     C* Inizializzo i campi chiave
045000060512     C                   eval      VATFGS = VABFGS
045100060512     C                   eval      VATAAS = VABAAS
045200060512     C                   eval      VATCCM = VABCCM
045300070823     C                   eval      VATNRS = VABNRS
045400060512     C                   eval      VATNSP = VABNSP
045500060512     C                   eval      VATLNP = VABLNP
045600040714     C*
045700040714     C* Solo se VATNOT valorizzato scarico il buffer
045800040714     C                   if        VATNOT <> *blanks
045900040119     C*
046000040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
046100040510     C                   if        VATTRC = *blanks
046200040510     C                   eval      VATTRC = 'E'
046300040510     C                   endif
046400120306     C*
046500120306     C* Imposto i dati del CMR
046600120306     C                   eval      VATCMR = 'LOHMANN'
046700120306     C                   eval      VATCNT = 1
046800120306     C*
046900120306     C                   write     EDIVAT00
047000040119     C                   add       1             §CTROKVT
047100040714     C*
047200040714     C                   endif
047300060331     C*
047400060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
047500060331     C                   if        wVATNOT_A <> *blanks
047600060331     C                   eval      VATNOT = wVATNOT_A
047700060331     C                   eval      VATTRC = 'A'
047800120306     C*
047900120306     C* Imposto i dati del CMR
048000120306     C                   eval      VATCMR = 'LOHMANN'
048100120306     C                   eval      VATCNT = 1
048200120306     C*
048300120306     C                   write     EDIVAT00
048400060331     C                   add       1             §CTROKVT
048500060331     C                   endif
048600060331     C*
048700060331     C                   if        wVATNOT_B <> *blanks
048800060331     C                   eval      VATNOT = wVATNOT_B
048900060331     C                   eval      VATTRC = 'B'
049000120306     C*
049100120306     C* Imposto i dati del CMR
049200120306     C                   eval      VATCMR = 'LOHMANN'
049300120306     C                   eval      VATCNT = 1
049400120306     C*
049500120306     C                   write     EDIVAT00
049600060331     C                   add       1             §CTROKVT
049700060331     C                   endif
049800060331     C*
049900060331     C                   if        wVATNOT_E <> *blanks
050000060331     C                   eval      VATNOT = wVATNOT_E
050100060331     C                   eval      VATTRC = 'E'
050200120306     C*
050300120306     C* Imposto i dati del CMR
050400120306     C                   eval      VATCMR = 'LOHMANN'
050500120306     C                   eval      VATCNT = 1
050600120306     C*
050700120306     C                   write     EDIVAT00
050800060331     C                   add       1             §CTROKVT
050900060331     C                   endif
051000040119     C*
051100040119     C                   ENDSR
051200040119
051300040119
051400040119
051500040119     C*----------------------------------------------------*
051600120306     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
051700040119     C*----------------------------------------------------*
051800040119     C     PREVAT        BEGSR
051900040119     C*
052000120306     C* Compongo il nome del membro da dare al EDIVATWR
052100040119     C                   eval      parmbr = vlrhdl
052200040119     C                   movel     'M'           parmbr
052300040510     C                   eval      parccm = vlrksc
052400040119     C                   eval      paropz = '1'
052500040119     C* Effettuo la chiamata al CLLE preposto
052600120306     C                   call(e)   'TITVEVTC'
052700040119     C                   parm                    parccm
052800040119     C                   parm                    parmbr
052900040119     C                   parm                    paropz
053000040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
053100040119     C                   if        %error
053200040119     C                   movel     '1'           chkcall
053300040119     C                   else
053400040119     C                   movel     '0'           chkcall
053500040119     C                   endif
053600040119     C*
053700040119     C                   ENDSR
053800010601
053900010601
054000010601
054100010330     C*----------------------------------------------------*
054200030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
054300010330     C*----------------------------------------------------*
054400040119     C     INZVAR        BEGSR
054500080527     C*
054600080527     C                   setoff                                       65
054700010330     C*
054800030822     C* Inizializzo variabili di wrk
054900020204     C                   Z-ADD     *zeros        Num5_0            5 0
055000060331     C                   MOVEL     *blanks       wVATNOT_A
055100060331     C                   MOVEL     *blanks       wVATNOT_B
055200060331     C                   MOVEL     *blanks       wVATNOT_E
055300100824     C                   MOVEL     *blanks       wNOTE            80
055400030822     C*
055500030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
055600030822     C                   CLEAR                   SkSplitCSV
055700030822     C                   EVAL      i = 1
055800030822     C                   EVAL      posDa = *zeros
055900030822     C                   EVAL      posA  = *zeros
056000080527     C*
056100120306     C                   CLEAR                   EDIVABS
056200120306     C                   CLEAR                   EDIVAT00
056300030822     C*
056400030822     C* Reimposto i valori di default
056500030822     C                   EXSR      DEFCAM
056600030822     C*
056700010330     C                   ENDSR
056800010330     C*----------------------------------------------------*
056900030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
057000010330     C*----------------------------------------------------*
057100010330     C     DEFCAM        BEGSR
057200030822     C*
057300030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
057400030715     C* e delimitatore testo.
057500030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
057600030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
057700030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
057800100318     C*
057900100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
058000100318     C                   EVAL      CharSOS = CharNUM
058100030822     C*
058200030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
058300100413     C                   SETOFF                                       5051
058400100413     C                   SELECT
058500100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
058600030715     C                   SETON                                        50
058700100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
058800100413     C                   SETON                                        51
058900100413     C                   ENDSL
059000030822     C*
059100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
059200030822     C                   EVAL      posDaDft = 1
059300030822     C                   EVAL      posADft  = 0
059400030822     C                   EVAL      wGiroDft = 0
059500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
059600030822     C                             posDaDft > 0
059700030822     C*
059800030822     C* Gestisco il 1° giro
059900030822     C                   IF        wGiroDft = 0
060000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
060100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
060200030822     C* Incremento il contatore dei "giri"
060300030822     C                   EVAL      wGiroDft = 1
060400030822     C                   ELSE
060500030822     C                   EVAL      posDaDft = posADft
060600030822     C                   ENDIF
060700030822     C* Eseguo lo scan x trovare la fine del campo corrente
060800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
060900030822     C*
061000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
061100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
061200030822     C                   IF        posDaDft > 0 AND
061300030822     C                             posADft  > 0
061400030822     C* NCL
061500030822     C                   IF        %subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):1:3)
061800030822     C                             = 'NCL'
061900030822     C                   EVAL      PiStr=%trim(%subst(
062000030822     C                             %subst(vlrppt:posDaDft+1:
062100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062200030822     C                   EXSR      CHKNUM
062300030822     C                   IF        PiInt=*on
062400030822     C                   Z-ADD     PiVal         VABNCL
062500030822     C                   ENDIF
062600030822     C                   ENDIF
062700030822     C* CCM
062800030822     C                   IF        %subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):1:3)
063100030822     C                             = 'CCM'
063200030822     C                   EVAL      PiStr=%trim(%subst(
063300030822     C                             %subst(vlrppt:posDaDft+1:
063400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063500030822     C                   EXSR      CHKNUM
063600030822     C                   IF        PiInt=*on
063700030822     C                   Z-ADD     PiVal         VABCCM
063800030822     C                   ENDIF
063900030822     C                   ENDIF
064000030822     C* LNP
064100030822     C                   IF        %subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):1:3)
064400030822     C                             = 'LNP'
064500030822     C                   EVAL      PiStr=%trim(%subst(
064600030822     C                             %subst(vlrppt:posDaDft+1:
064700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064800030822     C                   EXSR      CHKNUM
064900030822     C                   IF        PiInt=*on
065000030822     C                   Z-ADD     PiVal         VABLNP
065100030822     C                   ENDIF
065200030822     C                   ENDIF
065300030822     C* NRS
065400030822     C                   IF        %subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):1:3)
065700030822     C                             = 'NRS'
065800030822     C                   EVAL      PiStr=%trim(%subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066100030822     C                   EXSR      CHKNUM
066200030822     C                   IF        PiInt=*on
066300030822     C                   Z-ADD     PiVal         VABNRS
066400030822     C                   ENDIF
066500030822     C                   ENDIF
066600030822     C* CTR
066700030822     C                   IF        %subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):1:3)
067000030822     C                             = 'CTR'
067100030822     C                   EVAL      PiStr=%trim(%subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067400030822     C                   EXSR      CHKNUM
067500030822     C                   IF        PiInt=*on
067600030822     C                   Z-ADD     PiVal         VABCTR
067700030822     C                   ENDIF
067800030822     C                   ENDIF
067900030822     C* PKB
068000030822     C                   IF        %subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):1:3)
068300030822     C                             = 'PKB'
068400030822     C                   EVAL      PiStr=%trim(%subst(
068500030822     C                             %subst(vlrppt:posDaDft+1:
068600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068700030822     C                   EXSR      CHKNUM
068800030822     C                   IF        PiNum=*on
068900030822     C                   Z-ADD     PiVal         VABPKB
069000030822     C                   ENDIF
069100030822     C                   ENDIF
069200030822     C* VLB
069300030822     C                   IF        %subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):1:3)
069600030822     C                             = 'VLB'
069700030822     C                   EVAL      PiStr=%trim(%subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070000030822     C                   EXSR      CHKNUM
070100030822     C                   IF        PiNum=*on
070200030822     C                   Z-ADD     PiVal         VABVLB
070300030822     C                   ENDIF
070400030822     C                   ENDIF
070500030822     C* QFT
070600030822     C                   IF        %subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):1:3)
070900030822     C                             = 'QFT'
071000030822     C                   EVAL      PiStr=%trim(%subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071300030822     C                   EXSR      CHKNUM
071400030822     C                   IF        PiNum=*on
071500030822     C                   Z-ADD     PiVal         VABQFT
071600030822     C                   ENDIF
071700030822     C                   ENDIF
071800030822     C* CBO
071900030822     C                   IF        %subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):1:3)
072200030822     C                             = 'CBO'
072300030822     C                   EVAL      VABCBO=%trim(%subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600030822     C                   ENDIF
072700030822     C* TSP
072800030822     C                   IF        %subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):1:3)
073100030822     C                             = 'TSP'
073200030822     C                   EVAL      VABTSP=%trim(%subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073500030822     C                   ENDIF
073600030822     C* VAS
073700030822     C                   IF        %subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):1:3)
074000030822     C                             = 'VAS'
074100030822     C                   EVAL      VABVAS=%trim(%subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074400030822     C                   ENDIF
074500030822     C* VCA
074600030822     C                   IF        %subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):1:3)
074900030822     C                             = 'VCA'
075000030822     C                   EVAL      VABVCA=%trim(%subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075300030822     C                   ENDIF
075400070925     C* TIC
075500070925     C                   IF        %subst(
075600070925     C                             %subst(vlrppt:posDaDft+1:
075700070925     C                             posADft-posDaDft-1):1:3)
075800070925     C                             = 'TIC'
075900070925     C                   EVAL      VABTIC=%trim(%subst(
076000070925     C                             %subst(vlrppt:posDaDft+1:
076100070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076200070925     C                   ENDIF
076300030822     C* GCA
076400030822     C                   IF        %subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):1:3)
076700030822     C                             = 'GCA'
076800030822     C                   EVAL      VABGCA=%trim(%subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077100030822     C                   ENDIF
077200030822     C* CTM
077300030822     C                   IF        %subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):1:3)
077600030822     C                             = 'CTM'
077700030822     C                   EVAL      VABCTM=%trim(%subst(
077800030822     C                             %subst(vlrppt:posDaDft+1:
077900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078000030822     C                   ENDIF
078100030822     C* FFD
078200030822     C                   IF        %subst(
078300030822     C                             %subst(vlrppt:posDaDft+1:
078400030822     C                             posADft-posDaDft-1):1:3)
078500030822     C                             = 'FFD'
078600030822     C                   EVAL      VABFFD=%trim(%subst(
078700030822     C                             %subst(vlrppt:posDaDft+1:
078800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078900030822     C                   ENDIF
079000030822     C* VAD
079100030822     C                   IF        %subst(
079200030822     C                             %subst(vlrppt:posDaDft+1:
079300030822     C                             posADft-posDaDft-1):1:3)
079400030822     C                             = 'VAD'
079500030822     C                   EVAL      VABVAD=%trim(%subst(
079600030822     C                             %subst(vlrppt:posDaDft+1:
079700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079800030822     C                   ENDIF
079900030822     C* GMA
080000030822     C                   IF        %subst(
080100030822     C                             %subst(vlrppt:posDaDft+1:
080200030822     C                             posADft-posDaDft-1):1:3)
080300030822     C                             = 'GMA'
080400030822     C                   EVAL      VABGMA=%trim(%subst(
080500030822     C                             %subst(vlrppt:posDaDft+1:
080600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080700030822     C                   ENDIF
080800030822     C* GGA
080900030822     C                   IF        %subst(
081000030822     C                             %subst(vlrppt:posDaDft+1:
081100030822     C                             posADft-posDaDft-1):1:3)
081200030822     C                             = 'GGA'
081300030822     C                   EVAL      VABGGA=%trim(%subst(
081400030822     C                             %subst(vlrppt:posDaDft+1:
081500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081600030822     C                   ENDIF
081700030822     C* GVA
081800030822     C                   IF        %subst(
081900030822     C                             %subst(vlrppt:posDaDft+1:
082000030822     C                             posADft-posDaDft-1):1:3)
082100030822     C                             = 'GVA'
082200030822     C                   EVAL      VABGVA=%trim(%subst(
082300030822     C                             %subst(vlrppt:posDaDft+1:
082400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082500030822     C                   ENDIF
082600030822     C* TC1
082700030822     C                   IF        %subst(
082800030822     C                             %subst(vlrppt:posDaDft+1:
082900030822     C                             posADft-posDaDft-1):1:3)
083000030822     C                             = 'TC1'
083100030822     C                   EVAL      VABTC1=%trim(%subst(
083200030822     C                             %subst(vlrppt:posDaDft+1:
083300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083400030822     C                   ENDIF
083500030822     C* TC2
083600030822     C                   IF        %subst(
083700030822     C                             %subst(vlrppt:posDaDft+1:
083800030822     C                             posADft-posDaDft-1):1:3)
083900030822     C                             = 'TC2'
084000030822     C                   EVAL      VABTC2=%trim(%subst(
084100030822     C                             %subst(vlrppt:posDaDft+1:
084200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084300030822     C                   ENDIF
084400040714     C* VATTRC
084500040714     C                   IF        %subst(
084600040714     C                             %subst(vlrppt:posDaDft+1:
084700040714     C                             posADft-posDaDft-1):1:3)
084800040714     C                             = 'TRC'
084900040714     C                   EVAL      VATTRC=%trim(%subst(
085000040714     C                             %subst(vlrppt:posDaDft+1:
085100040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085200040714     C                   ENDIF
085300030822     C* ...
085400030822     C                   ENDIF
085500030822     C                   ENDDO
085600020204     C*
085700010330     C                   ENDSR
085800010607     C*----------------------------------------------------*
085900120306     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
086000010607     C*----------------------------------------------------*
086100010607     C     IMPVAB        BEGSR
086200010607     C*
086300010607     C                   SETOFF                                       3132
086400030515     C*
086500010607     C                   MOVEL     datcor        VABAAS
086600010607     C                   MOVE      datcor        VABMGS
086700010607     C*
086800030715     C**********
086900030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
087000030715     C**********
087100030715     C                   IF        wGiro = *zeros
087200040113     C*
087300040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
087400040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
087500080429     C                             %trim(intcol(3))+%trim(intcol(4))+
087600080429     C                             %trim(intcol(5))<>*blanks
087700040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
087800080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
087900080429     C                                      %trim(intcol(5))
088000070823     C     ';':CharCSV   XLATE     vindta        vindta
088100040510     C                   ENDIF
088200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
088300031201     C     minu:maiu     XLATE     vindta        vindta
088400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088500030715     C                   DOW       posDa <= %len(%trim(vindta))
088600030715     C*
088700030715     C* Gestisco il 1° campo
088800030715     C                   IF        i = 1
088900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
089000030715     C                   EVAL      posDa = 1
089100030715     C* Eseguo lo scan x trovare la fine del primo campo
089200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089300030715     C* A questo punto "estraggo" il campo corrente
089400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089500030715     C                                                    (posA-posDa))
089600030715     C* X i campi successivi al 1°
089700030715     C                   ELSE
089800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
089900030715     C                   EVAL      posDa = posA + 1
090000030715     C* Eseguo lo scan x trovare la fine del campo corrente
090100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090200030715     C* Gestisco l'ultimo campo
090300030715     C                   IF        posA = *zeros
090400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090500030715     C* A questo punto "estraggo" il campo corrente
090600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
090700030715     C                                                    (posA-posDa))
090800030715     C                   LEAVE
090900030715     C                   ELSE
091000030715     C* A questo punto "estraggo" il campo corrente
091100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
091200030715     C                                                    (posA-posDa))
091300030715     C                   ENDIF
091400030715     C                   ENDIF
091500030715     C* Incremento il contatore d campo
091600030715     C                   EVAL      i = i +1
091700030715     C                   ENDDO
091800030715     C*
091900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092000030715     C* delimitazione testo
092100030715     C                   EVAL      i = 1
092200030715     C                   DOW       i < %elem(SkSplitFLD)
092300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
092400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
092500030715     C                   EVAL      i = i + 1
092600030715     C                   ENDDO
092700030715     C                   EVAL      wGiro = 1
092800030715     C                   ELSE
092900030715     C                   EVAL      wGiro = 2
093000031204     C**********
093100031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
093200031204     C* d separatore campo
093300031204     C**********
093400031204     C                   EVAL      vindta = %trim(vindta)
093500031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
093600031204     C                   EVAL      vindta = %subst(vindta:2)
093700031204     C                   ENDDO
093800031204     C*
093900031204     C                   Z-ADD     *zeros        lunghInput        4 0
094000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
094100031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
094200031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
094300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
094400031204     C                   ENDDO
094500030715     C**********
094600030715     C* Effettuo lo split del campo dati d input
094700030715     C**********
094800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
094900030715     C                   DOW       posDa <= %len(%trim(vindta))
095000030715     C*
095100030715     C* Gestisco il 1° campo
095200030715     C                   IF        i = 1
095300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
095400030715     C                   EVAL      posDa = 1
095500030715     C* Eseguo lo scan x trovare la fine del primo campo
095600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
095700030715     C* A questo punto "estraggo" il campo corrente
095800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095900030715     C                                                    (posA-posDa))
096000030715     C* X i campi successivi al 1°
096100030715     C                   ELSE
096200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
096300030715     C                   EVAL      posDa = posA + 1
096400031204     C* Verifico che nn vi sia il campo nn valorizzato
096500031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
096600031204     C* Se campo nn valorizzato skippo al prossimo
096700031204     C                   EVAL      SkSplitCSV(i) = *blanks
096800031204     C                   EVAL      posA  = posA + 1
096900031204     C                   ELSE
097000030715     C* Eseguo lo scan x trovare la fine del campo corrente
097100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
097200030715     C* Gestisco l'ultimo campo
097300030715     C                   IF        posA = *zeros
097400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
097500030715     C* A questo punto "estraggo" il campo corrente
097600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
097700030715     C                                                    (posA-posDa))
097800030715     C                   LEAVE
097900030715     C                   ELSE
098000030715     C* A questo punto "estraggo" il campo corrente
098100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
098200030715     C                                                    (posA-posDa))
098300030715     C                   ENDIF
098400031204     C                   ENDIF
098500030715     C                   ENDIF
098600030715     C* Incremento il contatore d campo
098700030715     C                   EVAL      i = i +1
098800030715     C                   ENDDO
098900030715     C*
099000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
099100030715     C* delimitazione testo
099200030715     C                   EVAL      i = 1
099300030715     C                   DOW       i < %elem(SkSplitCSV)
099400030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
099500030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
099600030715     C                   EVAL      i = i + 1
099700030715     C                   ENDDO
099800100720     C*
099900100720     C* Stacco un progressivo x numero spedizione
100000100720 xxx C***                EXSR      repNSP
100100100326     C*
100200030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
100300030715     C                   EVAL      i = 1
100400030715     C                   DOW       i < %elem(SkSplitFLD)
100500030715     C*
100600030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
100700030715     C                   IF        SkSplitFLD(i) <> *blanks AND
100800030715     C                             SkSplitCSV(i) <> *blanks
100900030721     C*** ATB
101000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
101100030721     C                   EVAL      VABATB = SkSplitCSV(i)
101200030721     C                   ENDIF
101300030715     C*** RSD
101400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
101500030715     C                   EVAL      VABRSD = SkSplitCSV(i)
101600030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
101700030715     C     '@':'A'       XLATE     VABRSD        VABRSD
101800030715     C* ==
101900030715     C                   ENDIF
102000030721     C*** RD2
102100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
102200030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
102300030721     C                   ENDIF
102400030715     C*** IND
102500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
102600030715     C                   EVAL      VABIND = SkSplitCSV(i)
102700030715     C                   ENDIF
102800030715     C*** LOD
102900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
103000030715     C                   EVAL      VABLOD = SkSplitCSV(i)
103100030715     C                   ENDIF
103200030715     C*** PRD
103300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
103400030715     C                   EVAL      VABPRD = SkSplitCSV(i)
103500030715     C                   ENDIF
103600030721     C*** NZD
103700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
103800030721     C                   EVAL      VABNZD = SkSplitCSV(i)
103900030721     C                   ENDIF
104000030715     C*** RMA
104100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
104200030715     C                   EVAL      VABRMA = SkSplitCSV(i)
104300030715     C                   ENDIF
104400030715     C*** NOT
104500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
104600030715     C                   EVAL      VABNOT = SkSplitCSV(i)
104700030715     C                   ENDIF
104800030715     C*** NT2
104900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
105000030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
105100030715     C                   ENDIF
105200030715     C*** VCA
105300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
105400030715     C                   EVAL      VABVCA = SkSplitCSV(i)
105500030715     C                   ENDIF
105600030715     C*** VAS
105700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
105800030715     C                   EVAL      VABVAS = SkSplitCSV(i)
105900030715     C                   ENDIF
106000030715     C*** FFD
106100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
106200030715     C                   EVAL      VABFFD = SkSplitCSV(i)
106300030715     C                   ENDIF
106400030721     C*** GC1
106500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
106600030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
106700030721     C                   ENDIF
106800030721     C*** GC2
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
107000030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
107100030721     C                   ENDIF
107200030721     C*** TSP
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
107400030721     C                   EVAL      VABTSP = SkSplitCSV(i)
107500030721     C                   ENDIF
107600040510     C*** CBO
107700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
107800040510     C                   EVAL      VABCBO = SkSplitCSV(i)
107900040510     C                   ENDIF
108000030721     C*** NAS
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
108200030721     C                   EVAL      VABNAS = SkSplitCSV(i)
108300030721     C                   ENDIF
108400030721     C*** TIC
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
108600030721     C                   EVAL      VABTIC = SkSplitCSV(i)
108700030721     C                   ENDIF
108800030721     C*** GCA
108900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
109000030721     C                   EVAL      VABGCA = SkSplitCSV(i)
109100030721     C                   ENDIF
109200030721     C*** XCO
109300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
109400030721     C                   EVAL      VABXCO = SkSplitCSV(i)
109500030721     C                   ENDIF
109600030721     C*** CTM
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
109800030721     C                   EVAL      VABCTM = SkSplitCSV(i)
109900030721     C                   ENDIF
110000030721     C*** TCR
110100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
110200030721     C                   EVAL      VABTCR = SkSplitCSV(i)
110300030721     C                   ENDIF
110400030721     C*** CTS
110500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
110600030721     C                   EVAL      VABCTS = SkSplitCSV(i)
110700030721     C                   ENDIF
110800030721     C*** FTM
110900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
111000030721     C                   EVAL      VABFTM = SkSplitCSV(i)
111100030721     C                   ENDIF
111200030721     C*** VAD
111300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
111400030721     C                   EVAL      VABVAD = SkSplitCSV(i)
111500030721     C                   ENDIF
111600030721     C*** GMA
111700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
111800030721     C                   EVAL      VABGMA = SkSplitCSV(i)
111900030721     C                   ENDIF
112000030721     C*** GGA
112100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
112200030721     C                   EVAL      VABGGA = SkSplitCSV(i)
112300030721     C                   ENDIF
112400030721     C*** GVA
112500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
112600030721     C                   EVAL      VABGVA = SkSplitCSV(i)
112700030721     C                   ENDIF
112800030721     C*** TC1
112900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
113000030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
113100030721     C                   ENDIF
113200030721     C*** TC2
113300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
113400030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
113500030721     C                   ENDIF
113600030721     C*** SCL
113700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
113800030721     C                   EVAL      VABSCL = SkSplitCSV(i)
113900030721     C                   ENDIF
114000030721     C*** RMO
114100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
114200030721     C                   EVAL      VABRMO = SkSplitCSV(i)
114300030721     C                   ENDIF
114400030721     C*** NMO
114500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
114600030721     C                   EVAL      VABNMO = SkSplitCSV(i)
114700030721     C                   ENDIF
114800030721     C*
114900030721     C* Reperisco quindi i campi numerici...
115000040510     C*** NSP
115100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
115200040510     C                   EVAL      PiStr=SkSplitCSV(i)
115300040510     C                   EXSR      CHKNUM
115400040510     C                   IF        PiInt=*on
115500040510     C                   Z-ADD     PiVal         VABNSP
115600040510     C                   ELSE
115700040510     C                   SETON                                        31
115800040510     C                   EVAL      VABNSP = *zeros
115900040510     C                   EVAL      vinmsg = %trimr(vinmsg)
116000040510     C                             + ' ' + 'VABNSP'
116100040510     C                   ENDIF
116200040510     C                   ENDIF
116300071106     C*** RMN
116400071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
116500071106     C                   EVAL      PiStr=SkSplitCSV(i)
116600071106     C                   EXSR      CHKNUM
116700071106     C                   IF        PiInt=*on
116800071106     C                   Z-ADD     PiVal         VABRMN
116900071106     C                   ELSE
117000071106     C                   SETON                                        32
117100071106     C                   EVAL      VABRMN = *zeros
117200071106     C                   EVAL      vinmsg = %trimr(vinmsg)
117300071106     C                             + ' ' + 'VABRMN'
117400071106     C                   ENDIF
117500071106     C                   ENDIF
117600030721     C*** CAD
117700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
117800030721     C                   EVAL      PiStr=SkSplitCSV(i)
117900030721     C                   EXSR      CHKNUM
118000030721     C                   IF        PiInt=*on
118100030721     C                   Z-ADD     PiVal         Num5_0
118200030721     C                   MOVEL(p)  Num5_0        VABCAD
118300030721     C                   ELSE
118400030721     C                   SETON                                        32
118500030721     C                   EVAL      VABCAD = *zeros
118600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118700030721     C                             + ' ' + 'VABCAD'
118800030721     C                   ENDIF
118900030721     C                   ENDIF
119000030721     C*** NCL
119100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
119200030721     C                   EVAL      PiStr=SkSplitCSV(i)
119300030721     C                   EXSR      CHKNUM
119400030721     C                   IF        PiInt=*on
119500030721     C                   Z-ADD     PiVal         VABNCL
119600030721     C                   ELSE
119700030721     C                   SETON                                        32
119800030721     C                   EVAL      VABNCL = *zeros
119900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120000040122     C                             + ' ' + 'VABNCL'
120100030721     C                   ENDIF
120200030721     C                   ENDIF
120300030721     C*** PKB
120400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
120500030721     C                   EVAL      PiStr=SkSplitCSV(i)
120600030721     C                   EXSR      CHKNUM
120700030721     C                   IF        PiNum=*on
120800030721     C                   Z-ADD     PiVal         VABPKB
120900030721     C                   ELSE
121000030721     C                   SETON                                        32
121100030721     C                   EVAL      VABPKB = *zeros
121200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121300030721     C                             + ' ' + 'VABPKB'
121400030721     C                   ENDIF
121500030721     C                   ENDIF
121600030721     C*** CAS
121700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
121800030721     C                   EVAL      PiStr=SkSplitCSV(i)
121900030721     C                   EXSR      CHKNUM
122000030721     C                   IF        PiNum=*on
122100030721     C                   Z-ADD     PiVal         VABCAS
122200030721     C                   ELSE
122300030721     C                   SETON                                        32
122400030721     C                   EVAL      VABCAS = *zeros
122500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122600030721     C                             + ' ' + 'VABCAS'
122700030721     C                   ENDIF
122800030721     C                   ENDIF
122900030721     C*** CCM
123000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
123100030721     C                   EVAL      PiStr=SkSplitCSV(i)
123200030721     C                   EXSR      CHKNUM
123300030721     C                   IF        PiInt=*on      AND
123400030721     C                             PiVal<=9999999 AND
123500030721     C                             PiVal>*zeros
123600030721     C                   Z-ADD     PiVal         VABCCM
123700030721     C                   ELSE
123800030721     C                   SETON                                        32
123900030721     C                   EVAL      VABCCM = *zeros
124000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124100040510     C                             + ' ' + 'VABCCM'
124200030721     C                   ENDIF
124300030721     C                   ENDIF
124400030721     C*** LNP
124500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
124600030721     C                   EVAL      PiStr=SkSplitCSV(i)
124700030721     C                   EXSR      CHKNUM
124800030721     C                   IF        PiInt=*on    AND
124900030721     C                             PiVal<=999   AND
125000030721     C                             PiVal>*zeros
125100030721     C                   Z-ADD     PiVal         VABLNP
125200030721     C                   ELSE
125300040715     C                   SETON                                        32
125400030721     C                   EVAL      VABLNP = *zeros
125500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125600040510     C                             + ' ' + 'VABLNP'
125700030721     C                   ENDIF
125800030721     C                   ENDIF
125900030721     C*** NRS
126000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
126100030721     C                   EVAL      PiStr=SkSplitCSV(i)
126200030721     C                   EXSR      CHKNUM
126300030721     C                   IF        PiInt=*on AND
126400030721     C                             PiVal<=99
126500030721     C                   Z-ADD     PiVal         VABNRS
126600030721     C                   ELSE
126700030721     C                   SETON                                        32
126800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126900030721     C                             + ' ' + 'VABNRS'
127000030721     C                   ENDIF
127100030721     C                   ENDIF
127200030721     C*** LNA
127300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
127400030721     C                   EVAL      PiStr=SkSplitCSV(i)
127500030721     C                   EXSR      CHKNUM
127600030721     C                   IF        PiInt=*on    AND
127700030721     C                             PiVal<=999   AND
127800030721     C                             PiVal>*zeros
127900030721     C                   Z-ADD     PiVal         VABLNA
128000030721     C                   ELSE
128100030721     C                   SETON                                        32
128200030721     C                   EVAL      VABLNA = *zeros
128300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128400030721     C                             + ' ' + 'VABLNA'
128500030721     C                   ENDIF
128600030721     C                   ENDIF
128700030721     C*** CTR
128800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
128900030721     C                   EVAL      PiStr=SkSplitCSV(i)
129000030721     C                   EXSR      CHKNUM
129100030721     C                   IF        PiInt=*on  AND
129200030721     C                             PiVal<=999
129300030721     C                   Z-ADD     PiVal         VABCTR
129400030721     C                   ELSE
129500030721     C                   SETON                                        32
129600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129700030721     C                             + ' ' + 'VABCTR'
129800030721     C                   ENDIF
129900030721     C                   ENDIF
130000030721     C*** IAS
130100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
130200030721     C                   EVAL      PiStr=SkSplitCSV(i)
130300030721     C                   EXSR      CHKNUM
130400030721     C                   IF        PiNum=*on
130500030721     C                   Z-ADD     PiVal         VABIAS
130600030721     C                   ELSE
130700030721     C                   SETON                                        32
130800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130900030721     C                             + ' ' + 'VABIAS'
131000030721     C                   ENDIF
131100030721     C                   ENDIF
131200030721     C*** VLB
131300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
131400030721     C                   EVAL      PiStr=SkSplitCSV(i)
131500030721     C                   EXSR      CHKNUM
131600030721     C                   IF        PiNum=*on
131700030721     C                   Z-ADD     PiVal         VABVLB
131800030721     C                   ELSE
131900030721     C                   SETON                                        32
132000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132100030721     C                             + ' ' + 'VABVLB'
132200030721     C                   ENDIF
132300030721     C                   ENDIF
132400030721     C*** QFT
132500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
132600030721     C                   EVAL      PiStr=SkSplitCSV(i)
132700030721     C                   EXSR      CHKNUM
132800030721     C                   IF        PiNum=*on
132900030721     C                   Z-ADD     PiVal         VABQFT
133000030721     C                   ELSE
133100030721     C                   SETON                                        32
133200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133300030721     C                             + ' ' + 'VABQFT'
133400030721     C                   ENDIF
133500030721     C                   ENDIF
133600030721     C*** NCD
133700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
133800030721     C                   EVAL      PiStr=SkSplitCSV(i)
133900030721     C                   EXSR      CHKNUM
134000030721     C                   IF        PiInt=*on      AND
134100030721     C                             PiVal<=9999999
134200030721     C                   Z-ADD     PiVal         VABNCD
134300030721     C                   ELSE
134400030721     C                   SETON                                        32
134500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134600030721     C                             + ' ' + 'VABNCD'
134700030721     C                   ENDIF
134800030721     C                   ENDIF
134900030721     C*** NCA
135000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
135100030721     C                   EVAL      PiStr=SkSplitCSV(i)
135200030721     C                   EXSR      CHKNUM
135300030721     C                   IF        PiInt=*on      AND
135400030721     C                             PiVal<=9999999
135500030721     C                   Z-ADD     PiVal         VABNCA
135600030721     C                   ELSE
135700030721     C                   SETON                                        32
135800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135900030721     C                             + ' ' + 'VABNCA'
136000030721     C                   ENDIF
136100030721     C                   ENDIF
136200030721     C*** ZNC
136300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
136400030721     C                   EVAL      PiStr=SkSplitCSV(i)
136500030721     C                   EXSR      CHKNUM
136600030721     C                   IF        PiInt=*on AND
136700030721     C                             PiVal<=99
136800030721     C                   Z-ADD     PiVal         VABZNC
136900030721     C                   ELSE
137000030721     C                   SETON                                        32
137100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137200030721     C                             + ' ' + 'VABZNC'
137300030721     C                   ENDIF
137400030721     C                   ENDIF
137500030721     C*** DCR
137600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
137700030721     C                   EVAL      PiStr=SkSplitCSV(i)
137800030721     C                   EXSR      CHKNUM
137900030721     C                   IF        PiInt=*on       AND
138000030721     C                             PiVal<=99999999
138100030721     C                   Z-ADD     PiVal         VABDCR
138200030721     C                   ELSE
138300030721     C                   SETON                                        32
138400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138500030721     C                             + ' ' + 'VABDCR'
138600030721     C                   ENDIF
138700030721     C                   ENDIF
138800030721     C*** HCR
138900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
139000030721     C                   EVAL      PiStr=SkSplitCSV(i)
139100030721     C                   EXSR      CHKNUM
139200030721     C                   IF        PiInt=*on   AND
139300030721     C                             PiVal<=9999
139400030721     C                   Z-ADD     PiVal         VABHCR
139500030721     C                   ELSE
139600030721     C                   SETON                                        32
139700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139800030721     C                             + ' ' + 'VABHCR'
139900030721     C                   ENDIF
140000030721     C                   ENDIF
140100030721     C*** VMD
140200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
140300030721     C                   EVAL      PiStr=SkSplitCSV(i)
140400030721     C                   EXSR      CHKNUM
140500030721     C                   IF        PiNum=*on
140600030721     C                   Z-ADD     PiVal         VABVMD
140700030721     C                   ELSE
140800030721     C                   SETON                                        32
140900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141000030721     C                             + ' ' + 'VABVMD'
141100030721     C                   ENDIF
141200030721     C                   ENDIF
141300030721     C*** ANT
141400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
141500030721     C                   EVAL      PiStr=SkSplitCSV(i)
141600030721     C                   EXSR      CHKNUM
141700030721     C                   IF        PiInt=*on        AND
141800030721     C                             PiVal<=999999999
141900030721     C                   Z-ADD     PiVal         VABANT
142000030721     C                   ELSE
142100030721     C                   SETON                                        32
142200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142300030721     C                             + ' ' + 'VABANT'
142400030721     C                   ENDIF
142500030721     C                   ENDIF
142600030721     C*** CMO
142700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
142800030721     C                   EVAL      PiStr=SkSplitCSV(i)
142900030721     C                   EXSR      CHKNUM
143000030721     C                   IF        PiInt=*on
143100030721     C                   Z-ADD     PiVal         Num5_0
143200030721     C                   MOVEL(p)  Num5_0        VABCMO
143300030721     C                   ELSE
143400030721     C                   SETON                                        32
143500030721     C                   EVAL      VABCMO = *zeros
143600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
143700030721     C                             + ' ' + 'VABCMO'
143800030721     C                   ENDIF
143900030721     C                   ENDIF
144000040510     C*
144100120306     C* Reperisco i campi del EDIVAT
144200040510     C*** VATNOT
144300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
144400040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
144500040510     C                   ENDIF
144600040510     C*** VATTRC
144700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
144800040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
144900040510     C                   ENDIF
145000060331     C*** VATNOT_A
145100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
145200060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
145300060331     C                   ENDIF
145400060331     C*** VATNOT_B
145500060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
145600060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
145700060331     C                   ENDIF
145800060331     C*** VATNOT_E
145900070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
146000060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
146100060331     C                   ENDIF
146200070823     C*
146300071029     C***  ===>  Gestione campi "particolari" dentro ciclo
146400100720     C*** RMA / NSP
146500100720     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMA'
146600100419     C                   EVAL      VABRMA = %trim(SkSplitCSV(i))
146700100413     C                   EVAL      PiStr=SkSplitCSV(i)
146800100413     C                   EXSR      CHKNUM
146900100413     C                   IF        PiInt=*on
147000100419     C                   Z-ADD     PiVal         VABNSP
147100100413     C                   ELSE
147200100413     C                   SETON                                        32
147300100413     C                   EVAL      vinmsg = %trimr(vinmsg)
147400100720     C                             + ' ' + 'VABNSP'
147500100413     C                   ENDIF
147600100326     C                   ENDIF
147700100720     C*** VLB
147800100720     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABVLB'
147900100720     C                   EVAL      PiStr=SkSplitCSV(i)
148000100720     C                   EXSR      CHKNUM
148100100720     C                   IF        PiNum=*on
148200100720     C                   EVAL      PiVal = PiVal / 1000000                      * da cm3 a m3
148300100720     C                   Z-ADD     PiVal         VABVLB
148400100720     C                   ELSE
148500100720     C                   SETON                                        32
148600100720     C                   EVAL      vinmsg = %trimr(vinmsg)
148700100720     C                             + ' ' + 'VABVLB'
148800100720     C                   ENDIF
148900100720     C                   ENDIF
149000100419     C*** CBO
149100100419     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCBO'
149200100419     C                   SELECT
149300100720     C                   WHEN      %trim(SkSplitCSV(i)) = 'X'
149400100720     C                   EVAL      VABCBO = '4'
149500120306     C                   MOVE(P)   *all'9'       VABCAS
149600120306     C                   MOVEL     'EUR'         VABVCA
149700100419     C                   ENDSL
149800100413     C                   ENDIF
149900100720     C*** NOT / NT2
150000100824     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT_1'
150100100824     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' +
150200100824     C                                     SkSplitCSV(i)
150300100720     C                   ENDIF
150400100824     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT_2'
150500100824     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' +
150600100824     C                                     SkSplitCSV(i)
150700100824     C                   ENDIF
150800070925     C*
150900071029     C***  <===  -----------------------------------------
151000040510     C*
151100030715     C                   ENDIF
151200030715     C                   EVAL      i = i + 1
151300030715     C                   ENDDO
151400070925     C*
151500071029     C***  ===>  Gestione campi "particolari" fuori ciclo
151600100824     C*** NOT / NT2
151700100824     C                   EVAL      wNOTE = %trim(wNOTE)
151800100824     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
151900100824     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
152000070925     C*
152100071029     C***  <===  -----------------------------------------
152200070925     C*
152300070925     C* Considerazioni finali su CBO/CAS
152400100318     C                   IF        wFlgCAS = '1'    OR
152500100318     C                             VABCAS  > *zeros
152600070925     C                   IF        VABCBO = '1'
152700070925     C                   EVAL      VABCBO = '4'
152800070925     C                   ENDIF
152900070925     C                   IF        VABCBO = '2'
153000070925     C                   EVAL      VABCBO = '6'
153100070925     C                   ENDIF
153200070925     C                   ENDIF
153300070925     C***  <===  ----------------------------
153400040510     C*
153500040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
153600040510     C                   IF        VABNSP = *zeros
153700040510     C                   SETON                                        31
153800040510     C                   ENDIF
153900040715     C*
154000040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
154100040715     C                   IF        VABLNP = *zeros
154200040715     C                   SETON                                        31
154300040715     C                   ENDIF
154400040510     C*
154500040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
154600040510     C                   IF        VABRMN = *zeros
154700040510     C                   Z-ADD     VABNSP        VABRMN
154800040510     C                   ENDIF
154900040510     C*
155000040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
155100040510     C                   IF        VABRMA = *blanks
155200040714     C                   MOVEL     VABRMN        VABRMA
155300040510     C                   ENDIF
155400040128     C*
155500040510     C* Se provincia nn valorizzata la reperisco
155600040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
155700040510     C                   IF        VABPRD = *blanks
155800040128     C                   CLEAR                   TISI95DS
155900040128     C                   EVAL      I95TCN = '3'
156000040128     C                   Z-ADD     datcor        I95DAT
156100040128     C                   EVAL      I95NAR = VABNZD
156200040128     C                   EVAL      I95CAP = VABCAD
156300040128     C                   EVAL      I95LOC = VABLOD
156400040128     C                   CALL      'TISI95R'
156500040128     C                   PARM                    TISI95DS
156600040128     C                   EVAL      VABPRD = O95PRV
156700040510     C                   ENDIF
156800020204     C*
156900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
157000020204     C                   EXSR      CHKIMPDIV
157100030715     C                   ENDIF
157200020204     C*
157300010607     C                   ENDSR
157400010607     C*----------------------------------------------------*
157500010601
157600020204
157700020204     C*----------------------------------------------------*
157800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
157900020204     C*----------------------------------------------------*
158000020204     C     CHKIMPDIV     BEGSR
158100020204     C*
158200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
158300020204     C                   Z-ADD     *zeros        wrkDec            9 9
158400020204     C*
158500020204     C* Come prima cosa effettuo considerazioni sulla divisa
158600020204     C                   IF        vabIAS > *zeros
158700020204     C                   IF        vabVAS <> 'EUR'
158800020204     C                   EVAL      vabVAS =  'ITL'
158900020204     C                   ENDIF
159000020204     C                   ENDIF
159100020204     C*
159200020204     C                   IF        vabCAS > *zeros
159300020204     C                   IF        vabVCA <> 'EUR'
159400020204     C                   EVAL      vabVCA =  'ITL'
159500020204     C                   ENDIF
159600020204     C                   ENDIF
159700020204     C*
159800020204     C                   IF        vabVMD > *zeros
159900020204     C                   IF        vabVAD <> 'EUR'
160000020204     C                   EVAL      vabVAD =  'ITL'
160100020204     C                   ENDIF
160200020204     C                   ENDIF
160300020204     C*
160400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
160500020204     C                   Z-ADD     vabIAS        wrkDec
160600020204     C                   IF        wrkDec > *zeros
160700020204     C                   IF        vabVAS = 'ITL'
160800020204     C                   EVAL      vabIAS = *zeros
160900020204     C                   ENDIF
161000020204     C                   ENDIF
161100020204     C*
161200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
161300020204     C                   Z-ADD     vabCAS        wrkDec
161400020204     C                   IF        wrkDec > *zeros
161500020204     C                   IF        vabVCA = 'ITL'
161600020204     C                   EVAL      vabCAS = *zeros
161700020204     C                   ENDIF
161800020204     C                   ENDIF
161900020204     C*
162000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
162100020204     C                   Z-ADD     vabVMD        wrkDec
162200020204     C                   IF        wrkDec > *zeros
162300020204     C                   IF        vabVAD = 'ITL'
162400020204     C                   EVAL      vabVMD = *zeros
162500020204     C                   ENDIF
162600020204     C                   ENDIF
162700020204     C*
162800020204     C                   ENDSR
162900020204     C***
163000010330
163100100318
163200100318
163300100318     C*----------------------------------------------------*
163400100318     C*  CONTROLLO NUMERICITA' CAMPI
163500100318     C*----------------------------------------------------*
163600100318     C     CHKNUM        BEGSR
163700100318     C*
163800100318     C                   IF        PiDecChr = *blanks
163900100318     C                   EVAL      PiDecChr = CharNUM
164000100318     C                   ENDIF
164100100318     C*
164200100318     C                   callp     UBISNUM_Check(PiStr
164300100318     C                                          :PiDecChr
164400100318     C                                          :PiVal
164500100318     C                                          :PiNum
164600100318     C                                          :PiInt)
164700100318     C*
164800100318     C                   ENDSR
164900100318     C***
165000010601
165100010601
165200010601
165300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
165400010601     C     repfil        BEGSR
165500010601     C*
165600010601     C                   if        invfil = *zeros and
165700010601     C                             depfil > *zeros and
165800010629     C                             (vinflg = *blanks or
165900010629     C                              vinflg = *zeros)
166000010601     C
166100010601     C                   eval      invfil = depfil
166200010601     C                   endif
166300010601     C*
166400010601     C                   if        depfil <> invfil and
166500010601     C                             invfil > *zeros
166600010601     C                   eval      flgMulti = '1'
166700010601     C                   if        vinflg = *blanks
166800010601     C                   add       1             cntNonEl
166900010601     C                   endif
167000010601     C                   endif
167100010601     C*
167200010601     C                   if        vinflg = '2'
167300010601     C                   eval      flgStato = '2'
167400010601     C                   endif
167500010601     C*
167600010601     C                   ENDSR
167700010601     C***
167800010601
167900010601
168000010601
168100010330
168200040119      /TITLE Invio dei dati al punto operativo.
168300040119     C     invio         BEGSR
168400040119     C*
168500120306     C* 1° invio EDIVAT
168600040119     C                   reset                   dscmz
168700040119     C                   move      vlrpoi        cmzdst
168800120306     C                   eval      cmzfld = 'EDIVATWR'
168900040119     C                   eval      cmzmbd = vlrhdl
169000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
169100040119     C***                if        prmfir = *blanks
169200120306     C                   eval      cmzfla = 'EDIVAT0F'
169300120306     C                   eval      cmzmba = 'EDIVAT0F'
169400040119     C***                else
169500040119     C***                eval      cmzfla = prmfir
169600040119     C***                eval      cmzmba = prmfir
169700040119     C***                endif
169800040119     C                   eval      cmznrr = *zeros
169900040119     C                   move      §ctrokvt      cmznrr
170000040119     C                   eval      cmzlba = vlrfl1
170100040119     C                   call(e)   'TIS711C'
170200040119     C                   parm                    dscmz
170300040119     C                   parm      *blanks       esito
170400040119     C                   if        %error
170500040119     C                             or cmzerr = '1'
170600040119     C                             or esito  = '1'
170700040119     C                   eval      wrkesito = '3'
170800040119     C                   else
170900040119     C*
171000120306     C* 2° invio EDIVAB
171100040119     C                   reset                   dscmz
171200040119     C                   move      vlrpoi        cmzdst
171300040119     C                   eval      cmzfld = vlrfou
171400040119     C                   eval      cmzmbd = vlrhdl
171500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
171600040119     C***                if        prmfir = *blanks
171700120306     C                   eval      cmzfla = 'EDIVAB0F'
171800120306     C                   eval      cmzmba = 'EDIVAB0F'
171900040119     C***                else
172000040119     C***                eval      cmzfla = prmfir
172100040119     C***                eval      cmzmba = prmfir
172200040119     C***                endif
172300040119     C                   eval      cmznrr = *zeros
172400040119     C                   move      §ctrokvb      cmznrr
172500040119     C                   eval      cmzlba = vlrfl1
172600040119     C                   call(e)   'TIS711C'
172700040119     C                   parm                    dscmz
172800040119     C                   parm      *blanks       esito
172900040119     C                   if        %error
173000040119     C                             or cmzerr = '1'
173100040119     C                             or esito  = '1'
173200040119     C                   eval      wrkesito = '3'
173300040119     C                   endif
173400040119     C                   endif
173500040119     C*
173600040119     C                   ENDSR
173700040119     C***
173800010601
173900010601
174000010601
174100010601
174200010601      /TITLE Invio dei dati al punto operativo.
174300010601     C     opeini        BEGSR
174400010601     C*
174500010601     C* Inizializzo flag e contatori operativi
174600010601     C                   movel     '0'           flgGiro           1
174700010601     C                   movel     '0'           flgMulti          1
174800010601     C                   movel     '1'           flgStato          1
174900010615     C                   movel     '0'           flgOk             1
175000010601     C                   z-add     *zeros        cntNonEl         10 0
175100010601     C                   z-add     *zeros        depfil            3 0
175200010601     C                   z-add     *zeros        invfil            3 0
175300010601     C*
175400010601     C                   ENDSR
175500010601     C***
175600070326
175700070326
175800070326
175900070326
176000070326     C     *pssr         BEGSR
176100070329     C*
176200070329     C                   if        %open(tivin00r)
176300070329     C                   close     tivin00r
176400070329     C                   endif
176500120306     C                   if        %open(edivabwr)
176600120306     C                   close     edivabwr
176700070329     C                   endif
176800120306     C                   if        %open(edivatwr)
176900120306     C                   close     edivatwr
177000070329     C                   endif
177100070326     C*
177200070326     C* Effettuo la chiamata al CLLE preposto
177300120306     C                   call(e)   'TITVEVTC'
177400070326     C                   parm                    parccm
177500070326     C                   parm                    parmbr
177600070326     C                   parm      '2'           paropz
177700070326     C*
177800070326     C                   eval      wrkesito = '2'
177900070404     C*
178000070404     C                   seton                                        LR
178100070326     C*
178200070326     C                   ENDSR     '*CANCL'
178300070326     C***
178400100318
178500100318
178600100318
178700100318     C     normCSV       BEGSR
178800100318     C*
178900100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
179000100318     C* separatori campi
179100100318     C                   if        CharTXT <> *blanks
179200100318     C                   z-add     *zeros        wLenVINDTA        4 0
179300100318     C                   z-add     *zeros        wFlgVINDTA        1 0
179400100318     C                   setoff                                       80
179500100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
179600100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
179700100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
179800100318     C                   if        not *in80
179900100318     C                   seton                                        80
180000100318     C                   else
180100100318     C                   setoff                                       80
180200100318     C                   endif
180300100318     C                   endif
180400100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
180500100318     C                             and *in80 = *on
180600100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
180700100318     C                   endif
180800100318     C                   enddo
180900100318     C                   endif
181000100318     C*
181100100318     C                   ENDSR
181200100318     C***
181300070326
181400100326
181500100326
181600100326     C*----------------------------------------------------*
181700100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
181800100326     C*----------------------------------------------------*
181900100326     C     REPNSP        BEGSR
182000100326     C*
182100100326     C* NSP => Stacco un numeratore da AZNUM
182200100326     C                   clear                   TRUL33DS
182300100326     C                   eval      I33OPE = *zeros
182400100326     C                   eval      I33CNU = 302
182500100326     C                   eval      I33NUM = 1
182600100326     C                   movel     TRUL33DS      KPJBU
182700100326     C                   call      'TRUL33R'
182800100326     C                   parm                    KPJBA
182900100326     C                   movel     KPJBU         TRUL33DS
183000100326     C                   if        O33ERR = *zeros
183100100326     C                   z-add     O33NRF        VABNSP
183200100326     C                   z-add     O33NRF        VATNSP
183300100326     C                   else
183400100326     C                   SETON                                        31
183500100326     C                   EVAL      vinmsg = %trimr(vinmsg)
183600100326     C                             + ' ' + 'VABNSP VATNSP'
183700100326     C                   endif
183800100326     C*
183900100326     C                   ENDSR
184000010330
184100100326
184200010330
184300000613     C     *inzsr        BEGSR
184400990910     C*
184500990910     C     *entry        plist
184600990920     C                   parm                    tivlrds
184700990921     C                   parm      wrkesito      esito
184800000724     C                   parm                    prmlit
184900000710     C                   parm                    prmfir
185000010330     C*
185100010330     C* CALCOLA LA DATA CORRENTE
185200100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
185300100318     C                   eval      datcor = %dec(%date() : *ISO)
185400000613     C*
185500000613     C                   ENDSR
185600000613     C***
185700990908
185800100720     Otitv1y7p  E            riepilogo         2
185900990915     O                                              'Upload via Internet'
186000990915     O                                           +1 'Traduzione TIVIN00R -'
186100120306     O                                           +1 'EDIVABWR'
186200990915     O                                           +1 'Testate bolle'
186300990915     O          E            riepilogo   2
186400990915     O                       wrkdata
186500990915     O                       wrkora              +1
186600990915     O                       procname            +1
186700990915     O          E            riepilogo   2
186800990915     O                                              'Cliente..................:'
186900010605     O                       VABCCM        z     +1
187000990915     O          E            riepilogo   2
187100990920     O                                              'Riferimento Strategi.....:'
187200990920     O                       vlrhdl              +1
187300990915     O          E            riepilogo   2
187400990915     O                                              'Giusti...................:'
187500040119     O                       §CTROKVB      2   +  1
187600990915     O          E            riepilogo   2
187700990915     O                                              'Sbagliati e corretti.....:'
187800971022     O                       §CTRMO        2   +  1
187900990915     O          E            riepilogo   2
188000990915     O                                              'Sbagliati e scartati.....:'
188100971022     O                       §CTRNO        2   +  1
188200000613
188300100720     Otitv1y7ps E            testdett          2
188400000613     O                                              'Upload via Internet'
188500000613     O                                           +1 'Traduzione TIVIN00R -'
188600120306     O                                           +1 'EDIVABWR'
188700000613     O                                           +1 'Testate bolle'
188800000616     O          E            testdett    3
188900000613     O                                           +2 'N° rec'
189000000613     O                                           +3 'Anteprima contenuto'
189100000616     O          E            rigadett    2
189200000613     O                       rrnum               +2
189300000621     O                       recko               +3
189400000616     O          E            findett     2
189500000613     O                       wrkdata
189600000613     O                       wrkora              +1
189700000613     O                       procname            +1
189800000616     O          E            findett     2
189900000613     O                                              'Cliente..................:'
190000010605     O                       VABCCM        z     +1
190100000616     O          E            findett     2
190200000613     O                                              'Riferimento Strategi.....:'
190300000613     O                       vlrhdl              +1
190400000616     O          E            findett     2
190500000613     O                                              'Giusti...................:'
190600040119     O                       §CTROKVB      2   +  1
190700000616     O          E            findett     2
190800000613     O                                              'Sbagliati e corretti.....:'
190900000613     O                       §CTRMO        2   +  1
191000000616     O          E            findett     2
191100000613     O                                              'Sbagliati e scartati.....:'
191200000613     O                       §CTRNO        2   +  1
191300000613** CMD - COMANDI CL
191400100720OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1Y7P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
191500100720OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1Y7PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
191600100720DLTOVR FILE(TITV1Y7P TITV1Y7PS) LVL(*)
191700010606
191800000613
191900040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
192000100921VABRMN;VABNCL;VABPKB;W_VABVLB;W_VABRMA;VABRSD;VABIND;VABLOD;VABCAD;
192100100921VATNOT_A;VATNOT_B;FIL001;W_VABCBO;VABPRD;W_VABNOT_1;W_VABNOT_2;
192200100921VABDCR;
192300100318
192400100318
