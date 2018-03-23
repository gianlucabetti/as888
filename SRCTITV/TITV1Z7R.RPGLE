000100100826      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200100318     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600100826     FEDIVABwr  O    E             DISK    usropn prefix(ok_)
000700100826     FEDIVATwr  O    E             DISK    usropn
000800100720     Ftitv1y8p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070329     F                                     infsr(*pssr)
001100100720     Ftitv1y8ps O    f  198        PRINTER usropn
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
003100100826     D edivabds      e ds                  extname(edivab0f)
003200100826     D edivabds_sav  e ds                  extname(edivab0f) prefix(sav_)
003300100826     D edivabds_ok   e ds                  extname(edivab0f) prefix(ok_)
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
014600100720     C                   if        not %open(titv1y8ps)
014700100720     C                   open      titv1y8ps
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
016700100826     C                   call(e)   'TITVEVTC'
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
017800100720     C                   if        %open(titv1y8ps)
017900000616     C                   except    findett
018000100720     C                   close     titv1y8ps
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
020000100720     C                   if        not %open(titv1y8p)
020100100720     C   50              open      titv1y8p
020200990915     C                   endif
020300990915     C*
020400030715     C   50              except    riepilogo
020500990915     C*
020600100720     C                   if        %open(titv1y8p)
020700100720     C   50              close     titv1y8p
020800990914     C                   endif
020900000613     C*
021000000613     C                   ENDSR
021100000613     C***
021200000613
021300010601
021400010601
021500010330     C*--------------------------------------------------------
021600100826     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
021700010330     C*--------------------------------------------------------
021800010605     C     RWVAB         BEGSR
021900010330     C*
022000010330     C                   if        not %open(tivin00r)
022100010330     C                   open      tivin00r
022200010330     C                   endif
022300100826     C                   if        not %open(edivabwr)
022400100826     C                   open      edivabwr
022500010330     C                   endif
022600040119     C*
022700100826     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
022800040119     C                   exsr      prevat
022900040119     C*
023000040119     C                   if        chkcall = '0'
023100040119     C*
023200100826     C                   if        not %open(edivatwr)
023300100826     C                   open      edivatwr
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
028000070925     C* Aggiungo un byte a blanks all'inizio d ogni record
028100070925     C                   eval      vindta  = ' ' + %trim(vindta)
028200070925     C*
028300080527     C                   exsr      inzvar
028400100318     C                   exsr      normCSV
028500010605     C                   exsr      impvab
028600010601     C*
028700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
028800010605     C                   eval      depfil = VABLNP
028900010601     C                   exsr      repfil
029000010601     C                   if        depfil = invfil
029100021025     C                   if        vlrpoi = 999
029200030822     C                   move(P)   invfil        VABFGS
029300021025     C                   else
029400030822     C                   move(P)   vlrpoi        VABFGS
029500021025     C                   endif
029600010601     C*
029700010601     C                   exsr      PREELA
029800010601     C*
029900010604     C* Ebbene...
030000010604     C*
030100040119     C  N31              add       1             §CTROKVB
030200040119     C   32              add       1             §CTRMO
030300040119     C   31              add       1             §CTRNO
030400080418     C*
030500030822     C                   if        wGiro = 2
030600080513     C* Scrivo sempre il VAT
030700080513     C  N31              exsr      wrivat
030800080527     C                   exsr      chkvab
030900080513     C                   endif
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
033100080527     C* Verifico dati rimasti "in cannna"...
033200080527     C                   seton                                        65
033300080527     C                   exsr      chkvab
033400080527     C*
033500040119     C                   endif                                                  (endif - chkcall)
033600010601     C*
033700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
033800010601     C                   if        cntNonEl = *zeros or
033900010601     C                             flgMulti = '0'
034000010330     C* Se non ci sono record con errori ...
034100010601     C                   if        §ctrno = 0 and
034200010604     C                             §ctrmo = 0 and
034300010601     C                             flgStato <> '2'
034400010330     C* ... restituisco esito OK.
034500010330     C                   eval      wrkesito = '0'
034600010330     C                   else
034700040119     C                   if        §ctrokvb > 0
034800010330     C                   eval      wrkesito = '1'
034900010330     C                   else
035000010615     C                   if        flgOk = '0'
035100010615     C                   eval      wrkesito = '2'
035200010615     C                   else
035300010615     C                   eval      wrkesito = '6'
035400010615     C                   endif
035500010330     C                   endif
035600010330     C                   endif
035700010601     C                   else
035800010601     C                   eval      wrkesito = '9'
035900010601     C                   endif
036000010330     C*
036100010330     C                   if        %open(tivin00r)
036200010330     C                   close     tivin00r
036300010330     C                   endif
036400100826     C                   if        %open(edivabwr)
036500100826     C                   close     edivabwr
036600010330     C                   endif
036700100826     C                   if        %open(edivatwr)
036800100826     C                   close     edivatwr
036900040512     C                   endif
037000010601     C*
037100010601     C                   if        vlrpoi <> 999
037200010601     C                   eval      invfil = vlrpoi
037300010601     C                   endif
037400010330     C*
037500040119     C                   if        §ctrokvb > 0
037600010601     C                             and invfil > *zeros
037700010330     C                   exsr      invio
037800010330     C                   endif
037900010601     C*
038000010618     C                   if        flgGiro = '1'
038100010601     C                   exsr      endela
038200010618     C                   endif
038300010330     C*
038400010330     C                   ENDSR
038500010330     C***
038600080527
038700080527
038800080527
038900080527     C*----------------------------------------------------*
039000080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
039100080527     C*----------------------------------------------------*
039200100413     C     CHKVAB        BEGSR
039300080527     C*
039400080527     C* Scrivo il VAB solo a rottura d spedizione
039500080527     C                   if        depspe = *zeros
039600100826     C  N31              eval      edivabds_sav = edivabds
039700080527     C                   eval      depspe = vabNSP
039800080527     C                   else
039900080527     C                   if        depspe = vabNSP AND *in65 = *off
040000100413     C   51              add       VABNCL        sav_VABNCL
040100100413     C   51              add       VABPKB        sav_VABPKB
040200100413     C   51              add       VABVLB        sav_VABVLB
040300100720     C   52              z-add     VABNCL        sav_VABNCL
040400100720     C   52              z-add     VABPKB        sav_VABPKB
040500100720     C   52              z-add     VABVLB        sav_VABVLB
040600100720     C   53              add       VABNCL        sav_VABNCL
040700100720     C   53              z-add     VABPKB        sav_VABPKB
040800100720     C   53              z-add     VABVLB        sav_VABVLB
040900080527     C                   else
041000100826     C  N31              eval      edivabds_ok = edivabds_sav
041100080527     C  N31              exsr      wrivab
041200100826     C  N31              eval      edivabds_sav = edivabds
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
042500100826     C*
042600080513     C*
042700080513     C* Gestisco l'eventuale rottura x numero spedizione
042800080527     C                   if        VABNSP <> depspe or *in65 = *on
042900070301     C                   if        wVATNOT_E <> *blanks
043000070301     C                   eval      VABCTM = '7Q'
043100070301     C                   endif
043200100826     C                   write     edivab00
043300080513     C                   endif
043400040119     C*
043500040119     C                   ENDSR
043600040119
043700040119
043800040119
043900040119     C*----------------------------------------------------*
044000040119     C*  SCARICAMENTO BUFFER RECORDS VAT
044100040119     C*----------------------------------------------------*
044200040510     C     WRIVAT        BEGSR
044300060512     C*
044400060512     C* Inizializzo i campi chiave
044500060512     C                   eval      VATFGS = VABFGS
044600060512     C                   eval      VATAAS = VABAAS
044700060512     C                   eval      VATCCM = VABCCM
044800070823     C                   eval      VATNRS = VABNRS
044900060512     C                   eval      VATNSP = VABNSP
045000060512     C                   eval      VATLNP = VABLNP
045100100826     C*
045200100826     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
045300110913     C                   EVAL      VATCMR = 'FNVA'
045400100826     C                   EVAL      VATCNT = 1
045500040714     C*
045600040714     C* Solo se VATNOT valorizzato scarico il buffer
045700040714     C                   if        VATNOT <> *blanks
045800040119     C*
045900040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
046000040510     C                   if        VATTRC = *blanks
046100040510     C                   eval      VATTRC = 'E'
046200040510     C                   endif
046300100826     C                   write     EDIVAT00
046400040119     C                   add       1             §CTROKVT
046500040714     C*
046600040714     C                   endif
046700060331     C*
046800060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
046900060331     C                   if        wVATNOT_A <> *blanks
047000060331     C                   eval      VATNOT = wVATNOT_A
047100060331     C                   eval      VATTRC = 'A'
047200100826     C                   write     EDIVAT00
047300060331     C                   add       1             §CTROKVT
047400060331     C                   endif
047500060331     C*
047600060331     C                   if        wVATNOT_B <> *blanks
047700060331     C                   eval      VATNOT = wVATNOT_B
047800060331     C                   eval      VATTRC = 'B'
047900100826     C                   write     EDIVAT00
048000060331     C                   add       1             §CTROKVT
048100060331     C                   endif
048200060331     C*
048300060331     C                   if        wVATNOT_E <> *blanks
048400060331     C                   eval      VATNOT = wVATNOT_E
048500060331     C                   eval      VATTRC = 'E'
048600100826     C                   write     EDIVAT00
048700060331     C                   add       1             §CTROKVT
048800060331     C                   endif
048900040119     C*
049000040119     C                   ENDSR
049100040119
049200040119
049300040119
049400040119     C*----------------------------------------------------*
049500100826     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
049600040119     C*----------------------------------------------------*
049700040119     C     PREVAT        BEGSR
049800040119     C*
049900100826     C* Compongo il nome del membro da dare al EDIVATWR
050000040119     C                   eval      parmbr = vlrhdl
050100040119     C                   movel     'M'           parmbr
050200040510     C                   eval      parccm = vlrksc
050300040119     C                   eval      paropz = '1'
050400040119     C* Effettuo la chiamata al CLLE preposto
050500100826     C                   call(e)   'TITVEVTC'
050600040119     C                   parm                    parccm
050700040119     C                   parm                    parmbr
050800040119     C                   parm                    paropz
050900040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
051000040119     C                   if        %error
051100040119     C                   movel     '1'           chkcall
051200040119     C                   else
051300040119     C                   movel     '0'           chkcall
051400040119     C                   endif
051500040119     C*
051600040119     C                   ENDSR
051700010601
051800010601
051900010601
052000010330     C*----------------------------------------------------*
052100030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
052200010330     C*----------------------------------------------------*
052300040119     C     INZVAR        BEGSR
052400080527     C*
052500080527     C                   setoff                                       65
052600010330     C*
052700030822     C* Inizializzo variabili di wrk
052800020204     C                   Z-ADD     *zeros        Num5_0            5 0
052900060331     C                   MOVEL     *blanks       wVATNOT_A
053000060331     C                   MOVEL     *blanks       wVATNOT_B
053100060331     C                   MOVEL     *blanks       wVATNOT_E
053200030822     C*
053300030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
053400030822     C                   CLEAR                   SkSplitCSV
053500030822     C                   EVAL      i = 1
053600030822     C                   EVAL      posDa = *zeros
053700030822     C                   EVAL      posA  = *zeros
053800080527     C*
053900100826     C                   CLEAR                   EDIVABDS
054000100826     C                   CLEAR                   EDIVAT00
054100030822     C*
054200030822     C* Reimposto i valori di default
054300030822     C                   EXSR      DEFCAM
054400030822     C*
054500010330     C                   ENDSR
054600010330     C*----------------------------------------------------*
054700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
054800010330     C*----------------------------------------------------*
054900010330     C     DEFCAM        BEGSR
055000030822     C*
055100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
055200030715     C* e delimitatore testo.
055300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
055400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
055500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
055600100318     C*
055700100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
055800100318     C                   EVAL      CharSOS = CharNUM
055900030822     C*
056000030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
056100100720     C                   SETOFF                                       505152
056200100720     C                   SETOFF                                       53
056300100413     C                   SELECT
056400100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
056500100720     C                   SETON                                        50
056600100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
056700100413     C                   SETON                                        51
056800100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
056900100720     C                   SETON                                        52
057000100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
057100100720     C                   SETON                                        53
057200100413     C                   ENDSL
057300030822     C*
057400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
057500030822     C                   EVAL      posDaDft = 1
057600030822     C                   EVAL      posADft  = 0
057700030822     C                   EVAL      wGiroDft = 0
057800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
057900030822     C                             posDaDft > 0
058000030822     C*
058100030822     C* Gestisco il 1° giro
058200030822     C                   IF        wGiroDft = 0
058300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
058400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
058500030822     C* Incremento il contatore dei "giri"
058600030822     C                   EVAL      wGiroDft = 1
058700030822     C                   ELSE
058800030822     C                   EVAL      posDaDft = posADft
058900030822     C                   ENDIF
059000030822     C* Eseguo lo scan x trovare la fine del campo corrente
059100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
059200030822     C*
059300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
059400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
059500030822     C                   IF        posDaDft > 0 AND
059600030822     C                             posADft  > 0
059700030822     C* NCL
059800030822     C                   IF        %subst(
059900030822     C                             %subst(vlrppt:posDaDft+1:
060000030822     C                             posADft-posDaDft-1):1:3)
060100030822     C                             = 'NCL'
060200030822     C                   EVAL      PiStr=%trim(%subst(
060300030822     C                             %subst(vlrppt:posDaDft+1:
060400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060500030822     C                   EXSR      CHKNUM
060600030822     C                   IF        PiInt=*on
060700030822     C                   Z-ADD     PiVal         VABNCL
060800030822     C                   ENDIF
060900030822     C                   ENDIF
061000030822     C* CCM
061100030822     C                   IF        %subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):1:3)
061400030822     C                             = 'CCM'
061500030822     C                   EVAL      PiStr=%trim(%subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061800030822     C                   EXSR      CHKNUM
061900030822     C                   IF        PiInt=*on
062000030822     C                   Z-ADD     PiVal         VABCCM
062100030822     C                   ENDIF
062200030822     C                   ENDIF
062300030822     C* LNP
062400030822     C                   IF        %subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):1:3)
062700030822     C                             = 'LNP'
062800030822     C                   EVAL      PiStr=%trim(%subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063100030822     C                   EXSR      CHKNUM
063200030822     C                   IF        PiInt=*on
063300030822     C                   Z-ADD     PiVal         VABLNP
063400030822     C                   ENDIF
063500030822     C                   ENDIF
063600030822     C* NRS
063700030822     C                   IF        %subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):1:3)
064000030822     C                             = 'NRS'
064100030822     C                   EVAL      PiStr=%trim(%subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064400030822     C                   EXSR      CHKNUM
064500030822     C                   IF        PiInt=*on
064600030822     C                   Z-ADD     PiVal         VABNRS
064700030822     C                   ENDIF
064800030822     C                   ENDIF
064900030822     C* CTR
065000030822     C                   IF        %subst(
065100030822     C                             %subst(vlrppt:posDaDft+1:
065200030822     C                             posADft-posDaDft-1):1:3)
065300030822     C                             = 'CTR'
065400030822     C                   EVAL      PiStr=%trim(%subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065700030822     C                   EXSR      CHKNUM
065800030822     C                   IF        PiInt=*on
065900030822     C                   Z-ADD     PiVal         VABCTR
066000030822     C                   ENDIF
066100030822     C                   ENDIF
066200030822     C* PKB
066300030822     C                   IF        %subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):1:3)
066600030822     C                             = 'PKB'
066700030822     C                   EVAL      PiStr=%trim(%subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067000030822     C                   EXSR      CHKNUM
067100030822     C                   IF        PiNum=*on
067200030822     C                   Z-ADD     PiVal         VABPKB
067300030822     C                   ENDIF
067400030822     C                   ENDIF
067500030822     C* VLB
067600030822     C                   IF        %subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):1:3)
067900030822     C                             = 'VLB'
068000030822     C                   EVAL      PiStr=%trim(%subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068300030822     C                   EXSR      CHKNUM
068400030822     C                   IF        PiNum=*on
068500030822     C                   Z-ADD     PiVal         VABVLB
068600030822     C                   ENDIF
068700030822     C                   ENDIF
068800030822     C* QFT
068900030822     C                   IF        %subst(
069000030822     C                             %subst(vlrppt:posDaDft+1:
069100030822     C                             posADft-posDaDft-1):1:3)
069200030822     C                             = 'QFT'
069300030822     C                   EVAL      PiStr=%trim(%subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069600030822     C                   EXSR      CHKNUM
069700030822     C                   IF        PiNum=*on
069800030822     C                   Z-ADD     PiVal         VABQFT
069900030822     C                   ENDIF
070000030822     C                   ENDIF
070100030822     C* CBO
070200030822     C                   IF        %subst(
070300030822     C                             %subst(vlrppt:posDaDft+1:
070400030822     C                             posADft-posDaDft-1):1:3)
070500030822     C                             = 'CBO'
070600030822     C                   EVAL      VABCBO=%trim(%subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070900030822     C                   ENDIF
071000030822     C* TSP
071100030822     C                   IF        %subst(
071200030822     C                             %subst(vlrppt:posDaDft+1:
071300030822     C                             posADft-posDaDft-1):1:3)
071400030822     C                             = 'TSP'
071500030822     C                   EVAL      VABTSP=%trim(%subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071800030822     C                   ENDIF
071900030822     C* VAS
072000030822     C                   IF        %subst(
072100030822     C                             %subst(vlrppt:posDaDft+1:
072200030822     C                             posADft-posDaDft-1):1:3)
072300030822     C                             = 'VAS'
072400030822     C                   EVAL      VABVAS=%trim(%subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072700030822     C                   ENDIF
072800030822     C* VCA
072900030822     C                   IF        %subst(
073000030822     C                             %subst(vlrppt:posDaDft+1:
073100030822     C                             posADft-posDaDft-1):1:3)
073200030822     C                             = 'VCA'
073300030822     C                   EVAL      VABVCA=%trim(%subst(
073400030822     C                             %subst(vlrppt:posDaDft+1:
073500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073600030822     C                   ENDIF
073700070925     C* TIC
073800070925     C                   IF        %subst(
073900070925     C                             %subst(vlrppt:posDaDft+1:
074000070925     C                             posADft-posDaDft-1):1:3)
074100070925     C                             = 'TIC'
074200070925     C                   EVAL      VABTIC=%trim(%subst(
074300070925     C                             %subst(vlrppt:posDaDft+1:
074400070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074500070925     C                   ENDIF
074600030822     C* GCA
074700030822     C                   IF        %subst(
074800030822     C                             %subst(vlrppt:posDaDft+1:
074900030822     C                             posADft-posDaDft-1):1:3)
075000030822     C                             = 'GCA'
075100030822     C                   EVAL      VABGCA=%trim(%subst(
075200030822     C                             %subst(vlrppt:posDaDft+1:
075300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075400030822     C                   ENDIF
075500030822     C* CTM
075600030822     C                   IF        %subst(
075700030822     C                             %subst(vlrppt:posDaDft+1:
075800030822     C                             posADft-posDaDft-1):1:3)
075900030822     C                             = 'CTM'
076000030822     C                   EVAL      VABCTM=%trim(%subst(
076100030822     C                             %subst(vlrppt:posDaDft+1:
076200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076300030822     C                   ENDIF
076400030822     C* FFD
076500030822     C                   IF        %subst(
076600030822     C                             %subst(vlrppt:posDaDft+1:
076700030822     C                             posADft-posDaDft-1):1:3)
076800030822     C                             = 'FFD'
076900030822     C                   EVAL      VABFFD=%trim(%subst(
077000030822     C                             %subst(vlrppt:posDaDft+1:
077100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077200030822     C                   ENDIF
077300030822     C* VAD
077400030822     C                   IF        %subst(
077500030822     C                             %subst(vlrppt:posDaDft+1:
077600030822     C                             posADft-posDaDft-1):1:3)
077700030822     C                             = 'VAD'
077800030822     C                   EVAL      VABVAD=%trim(%subst(
077900030822     C                             %subst(vlrppt:posDaDft+1:
078000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078100030822     C                   ENDIF
078200030822     C* GMA
078300030822     C                   IF        %subst(
078400030822     C                             %subst(vlrppt:posDaDft+1:
078500030822     C                             posADft-posDaDft-1):1:3)
078600030822     C                             = 'GMA'
078700030822     C                   EVAL      VABGMA=%trim(%subst(
078800030822     C                             %subst(vlrppt:posDaDft+1:
078900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079000030822     C                   ENDIF
079100030822     C* GGA
079200030822     C                   IF        %subst(
079300030822     C                             %subst(vlrppt:posDaDft+1:
079400030822     C                             posADft-posDaDft-1):1:3)
079500030822     C                             = 'GGA'
079600030822     C                   EVAL      VABGGA=%trim(%subst(
079700030822     C                             %subst(vlrppt:posDaDft+1:
079800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079900030822     C                   ENDIF
080000030822     C* GVA
080100030822     C                   IF        %subst(
080200030822     C                             %subst(vlrppt:posDaDft+1:
080300030822     C                             posADft-posDaDft-1):1:3)
080400030822     C                             = 'GVA'
080500030822     C                   EVAL      VABGVA=%trim(%subst(
080600030822     C                             %subst(vlrppt:posDaDft+1:
080700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080800030822     C                   ENDIF
080900030822     C* TC1
081000030822     C                   IF        %subst(
081100030822     C                             %subst(vlrppt:posDaDft+1:
081200030822     C                             posADft-posDaDft-1):1:3)
081300030822     C                             = 'TC1'
081400030822     C                   EVAL      VABTC1=%trim(%subst(
081500030822     C                             %subst(vlrppt:posDaDft+1:
081600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081700030822     C                   ENDIF
081800030822     C* TC2
081900030822     C                   IF        %subst(
082000030822     C                             %subst(vlrppt:posDaDft+1:
082100030822     C                             posADft-posDaDft-1):1:3)
082200030822     C                             = 'TC2'
082300030822     C                   EVAL      VABTC2=%trim(%subst(
082400030822     C                             %subst(vlrppt:posDaDft+1:
082500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082600030822     C                   ENDIF
082700040714     C* VATTRC
082800040714     C                   IF        %subst(
082900040714     C                             %subst(vlrppt:posDaDft+1:
083000040714     C                             posADft-posDaDft-1):1:3)
083100040714     C                             = 'TRC'
083200040714     C                   EVAL      VATTRC=%trim(%subst(
083300040714     C                             %subst(vlrppt:posDaDft+1:
083400040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083500040714     C                   ENDIF
083600030822     C* ...
083700030822     C                   ENDIF
083800030822     C                   ENDDO
083900020204     C*
084000010330     C                   ENDSR
084100010607     C*----------------------------------------------------*
084200100826     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
084300010607     C*----------------------------------------------------*
084400010607     C     IMPVAB        BEGSR
084500010607     C*
084600010607     C                   SETOFF                                       3132
084700030515     C*
084800010607     C                   MOVEL     datcor        VABAAS
084900010607     C                   MOVE      datcor        VABMGS
085000010607     C*
085100030715     C**********
085200030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
085300030715     C**********
085400030715     C                   IF        wGiro = *zeros
085500040113     C*
085600040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
085700040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
085800080429     C                             %trim(intcol(3))+%trim(intcol(4))+
085900080429     C                             %trim(intcol(5))<>*blanks
086000040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
086100080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
086200080429     C                                      %trim(intcol(5))
086300070823     C     ';':CharCSV   XLATE     vindta        vindta
086400040510     C                   ENDIF
086500031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
086600031201     C     minu:maiu     XLATE     vindta        vindta
086700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
086800030715     C                   DOW       posDa <= %len(%trim(vindta))
086900030715     C*
087000030715     C* Gestisco il 1° campo
087100030715     C                   IF        i = 1
087200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
087300030715     C                   EVAL      posDa = 1
087400030715     C* Eseguo lo scan x trovare la fine del primo campo
087500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087600030715     C* A questo punto "estraggo" il campo corrente
087700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087800030715     C                                                    (posA-posDa))
087900030715     C* X i campi successivi al 1°
088000030715     C                   ELSE
088100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
088200030715     C                   EVAL      posDa = posA + 1
088300030715     C* Eseguo lo scan x trovare la fine del campo corrente
088400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088500030715     C* Gestisco l'ultimo campo
088600030715     C                   IF        posA = *zeros
088700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
088800030715     C* A questo punto "estraggo" il campo corrente
088900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089000030715     C                                                    (posA-posDa))
089100030715     C                   LEAVE
089200030715     C                   ELSE
089300030715     C* A questo punto "estraggo" il campo corrente
089400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089500030715     C                                                    (posA-posDa))
089600030715     C                   ENDIF
089700030715     C                   ENDIF
089800030715     C* Incremento il contatore d campo
089900030715     C                   EVAL      i = i +1
090000030715     C                   ENDDO
090100030715     C*
090200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
090300030715     C* delimitazione testo
090400030715     C                   EVAL      i = 1
090500030715     C                   DOW       i < %elem(SkSplitFLD)
090600030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
090700030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
090800030715     C                   EVAL      i = i + 1
090900030715     C                   ENDDO
091000030715     C                   EVAL      wGiro = 1
091100030715     C                   ELSE
091200030715     C                   EVAL      wGiro = 2
091300031204     C**********
091400031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
091500031204     C* d separatore campo
091600031204     C**********
091700031204     C                   EVAL      vindta = %trim(vindta)
091800031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
091900031204     C                   EVAL      vindta = %subst(vindta:2)
092000031204     C                   ENDDO
092100031204     C*
092200031204     C                   Z-ADD     *zeros        lunghInput        4 0
092300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
092400031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
092500031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
092600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
092700031204     C                   ENDDO
092800030715     C**********
092900030715     C* Effettuo lo split del campo dati d input
093000030715     C**********
093100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
093200030715     C                   DOW       posDa <= %len(%trim(vindta))
093300030715     C*
093400030715     C* Gestisco il 1° campo
093500030715     C                   IF        i = 1
093600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
093700030715     C                   EVAL      posDa = 1
093800030715     C* Eseguo lo scan x trovare la fine del primo campo
093900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094000030715     C* A questo punto "estraggo" il campo corrente
094100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094200030715     C                                                    (posA-posDa))
094300030715     C* X i campi successivi al 1°
094400030715     C                   ELSE
094500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
094600030715     C                   EVAL      posDa = posA + 1
094700031204     C* Verifico che nn vi sia il campo nn valorizzato
094800031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
094900031204     C* Se campo nn valorizzato skippo al prossimo
095000031204     C                   EVAL      SkSplitCSV(i) = *blanks
095100031204     C                   EVAL      posA  = posA + 1
095200031204     C                   ELSE
095300030715     C* Eseguo lo scan x trovare la fine del campo corrente
095400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
095500030715     C* Gestisco l'ultimo campo
095600030715     C                   IF        posA = *zeros
095700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
095800030715     C* A questo punto "estraggo" il campo corrente
095900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096000030715     C                                                    (posA-posDa))
096100030715     C                   LEAVE
096200030715     C                   ELSE
096300030715     C* A questo punto "estraggo" il campo corrente
096400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096500030715     C                                                    (posA-posDa))
096600030715     C                   ENDIF
096700031204     C                   ENDIF
096800030715     C                   ENDIF
096900030715     C* Incremento il contatore d campo
097000030715     C                   EVAL      i = i +1
097100030715     C                   ENDDO
097200030715     C*
097300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
097400030715     C* delimitazione testo
097500030715     C                   EVAL      i = 1
097600030715     C                   DOW       i < %elem(SkSplitCSV)
097700030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
097800030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
097900030715     C                   EVAL      i = i + 1
098000030715     C                   ENDDO
098100100720     C*
098200100720     C* Stacco un progressivo x numero spedizione
098300100720 xxx C***                EXSR      repNSP
098400100326     C*
098500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
098600030715     C                   EVAL      i = 1
098700030715     C                   DOW       i < %elem(SkSplitFLD)
098800030715     C*
098900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
099000030715     C                   IF        SkSplitFLD(i) <> *blanks AND
099100030715     C                             SkSplitCSV(i) <> *blanks
099200030721     C*** ATB
099300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
099400030721     C                   EVAL      VABATB = SkSplitCSV(i)
099500030721     C                   ENDIF
099600030715     C*** RSD
099700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
099800030715     C                   EVAL      VABRSD = SkSplitCSV(i)
099900030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
100000030715     C     '@':'A'       XLATE     VABRSD        VABRSD
100100030715     C* ==
100200030715     C                   ENDIF
100300030721     C*** RD2
100400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
100500030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
100600030721     C                   ENDIF
100700030715     C*** IND
100800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
100900030715     C                   EVAL      VABIND = SkSplitCSV(i)
101000030715     C                   ENDIF
101100030715     C*** LOD
101200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
101300030715     C                   EVAL      VABLOD = SkSplitCSV(i)
101400030715     C                   ENDIF
101500030715     C*** PRD
101600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
101700030715     C                   EVAL      VABPRD = SkSplitCSV(i)
101800030715     C                   ENDIF
101900030721     C*** NZD
102000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
102100030721     C                   EVAL      VABNZD = SkSplitCSV(i)
102200030721     C                   ENDIF
102300030715     C*** RMA
102400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
102500030715     C                   EVAL      VABRMA = SkSplitCSV(i)
102600030715     C                   ENDIF
102700030715     C*** NOT
102800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
102900030715     C                   EVAL      VABNOT = SkSplitCSV(i)
103000030715     C                   ENDIF
103100030715     C*** NT2
103200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
103300030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
103400030715     C                   ENDIF
103500030715     C*** VCA
103600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
103700030715     C                   EVAL      VABVCA = SkSplitCSV(i)
103800030715     C                   ENDIF
103900030715     C*** VAS
104000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
104100030715     C                   EVAL      VABVAS = SkSplitCSV(i)
104200030715     C                   ENDIF
104300030715     C*** FFD
104400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
104500030715     C                   EVAL      VABFFD = SkSplitCSV(i)
104600030715     C                   ENDIF
104700030721     C*** GC1
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
104900030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
105000030721     C                   ENDIF
105100030721     C*** GC2
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
105300030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
105400030721     C                   ENDIF
105500030721     C*** TSP
105600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
105700030721     C                   EVAL      VABTSP = SkSplitCSV(i)
105800030721     C                   ENDIF
105900040510     C*** CBO
106000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
106100040510     C                   EVAL      VABCBO = SkSplitCSV(i)
106200040510     C                   ENDIF
106300030721     C*** NAS
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
106500030721     C                   EVAL      VABNAS = SkSplitCSV(i)
106600030721     C                   ENDIF
106700030721     C*** TIC
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
106900030721     C                   EVAL      VABTIC = SkSplitCSV(i)
107000030721     C                   ENDIF
107100030721     C*** GCA
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
107300030721     C                   EVAL      VABGCA = SkSplitCSV(i)
107400030721     C                   ENDIF
107500030721     C*** XCO
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
107700030721     C                   EVAL      VABXCO = SkSplitCSV(i)
107800030721     C                   ENDIF
107900030721     C*** CTM
108000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
108100030721     C                   EVAL      VABCTM = SkSplitCSV(i)
108200030721     C                   ENDIF
108300030721     C*** TCR
108400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
108500030721     C                   EVAL      VABTCR = SkSplitCSV(i)
108600030721     C                   ENDIF
108700030721     C*** CTS
108800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
108900030721     C                   EVAL      VABCTS = SkSplitCSV(i)
109000030721     C                   ENDIF
109100030721     C*** FTM
109200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
109300030721     C                   EVAL      VABFTM = SkSplitCSV(i)
109400030721     C                   ENDIF
109500030721     C*** VAD
109600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
109700030721     C                   EVAL      VABVAD = SkSplitCSV(i)
109800030721     C                   ENDIF
109900030721     C*** GMA
110000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
110100030721     C                   EVAL      VABGMA = SkSplitCSV(i)
110200030721     C                   ENDIF
110300030721     C*** GGA
110400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
110500030721     C                   EVAL      VABGGA = SkSplitCSV(i)
110600030721     C                   ENDIF
110700030721     C*** GVA
110800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
110900030721     C                   EVAL      VABGVA = SkSplitCSV(i)
111000030721     C                   ENDIF
111100030721     C*** TC1
111200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
111300030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
111400030721     C                   ENDIF
111500030721     C*** TC2
111600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
111700030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
111800030721     C                   ENDIF
111900030721     C*** SCL
112000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
112100030721     C                   EVAL      VABSCL = SkSplitCSV(i)
112200030721     C                   ENDIF
112300030721     C*** RMO
112400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
112500030721     C                   EVAL      VABRMO = SkSplitCSV(i)
112600030721     C                   ENDIF
112700030721     C*** NMO
112800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
112900030721     C                   EVAL      VABNMO = SkSplitCSV(i)
113000030721     C                   ENDIF
113100030721     C*
113200030721     C* Reperisco quindi i campi numerici...
113300040510     C*** NSP
113400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
113500040510     C                   EVAL      PiStr=SkSplitCSV(i)
113600040510     C                   EXSR      CHKNUM
113700040510     C                   IF        PiInt=*on
113800040510     C                   Z-ADD     PiVal         VABNSP
113900040510     C                   ELSE
114000040510     C                   SETON                                        31
114100040510     C                   EVAL      VABNSP = *zeros
114200040510     C                   EVAL      vinmsg = %trimr(vinmsg)
114300040510     C                             + ' ' + 'VABNSP'
114400040510     C                   ENDIF
114500040510     C                   ENDIF
114600071106     C*** RMN
114700071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
114800071106     C                   EVAL      PiStr=SkSplitCSV(i)
114900071106     C                   EXSR      CHKNUM
115000071106     C                   IF        PiInt=*on
115100071106     C                   Z-ADD     PiVal         VABRMN
115200071106     C                   ELSE
115300071106     C                   SETON                                        32
115400071106     C                   EVAL      VABRMN = *zeros
115500071106     C                   EVAL      vinmsg = %trimr(vinmsg)
115600071106     C                             + ' ' + 'VABRMN'
115700071106     C                   ENDIF
115800071106     C                   ENDIF
115900030721     C*** CAD
116000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
116100030721     C                   EVAL      PiStr=SkSplitCSV(i)
116200030721     C                   EXSR      CHKNUM
116300030721     C                   IF        PiInt=*on
116400030721     C                   Z-ADD     PiVal         Num5_0
116500030721     C                   MOVEL(p)  Num5_0        VABCAD
116600030721     C                   ELSE
116700030721     C                   SETON                                        32
116800030721     C                   EVAL      VABCAD = *zeros
116900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117000030721     C                             + ' ' + 'VABCAD'
117100030721     C                   ENDIF
117200030721     C                   ENDIF
117300030721     C*** NCL
117400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
117500030721     C                   EVAL      PiStr=SkSplitCSV(i)
117600030721     C                   EXSR      CHKNUM
117700030721     C                   IF        PiInt=*on
117800030721     C                   Z-ADD     PiVal         VABNCL
117900030721     C                   ELSE
118000030721     C                   SETON                                        32
118100030721     C                   EVAL      VABNCL = *zeros
118200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118300040122     C                             + ' ' + 'VABNCL'
118400030721     C                   ENDIF
118500030721     C                   ENDIF
118600030721     C*** PKB
118700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
118800030721     C                   EVAL      PiStr=SkSplitCSV(i)
118900030721     C                   EXSR      CHKNUM
119000030721     C                   IF        PiNum=*on
119100030721     C                   Z-ADD     PiVal         VABPKB
119200030721     C                   ELSE
119300030721     C                   SETON                                        32
119400030721     C                   EVAL      VABPKB = *zeros
119500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119600030721     C                             + ' ' + 'VABPKB'
119700030721     C                   ENDIF
119800030721     C                   ENDIF
119900030721     C*** CAS
120000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
120100030721     C                   EVAL      PiStr=SkSplitCSV(i)
120200030721     C                   EXSR      CHKNUM
120300030721     C                   IF        PiNum=*on
120400030721     C                   Z-ADD     PiVal         VABCAS
120500030721     C                   ELSE
120600030721     C                   SETON                                        32
120700030721     C                   EVAL      VABCAS = *zeros
120800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120900030721     C                             + ' ' + 'VABCAS'
121000030721     C                   ENDIF
121100030721     C                   ENDIF
121200030721     C*** CCM
121300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
121400030721     C                   EVAL      PiStr=SkSplitCSV(i)
121500030721     C                   EXSR      CHKNUM
121600030721     C                   IF        PiInt=*on      AND
121700030721     C                             PiVal<=9999999 AND
121800030721     C                             PiVal>*zeros
121900030721     C                   Z-ADD     PiVal         VABCCM
122000030721     C                   ELSE
122100030721     C                   SETON                                        32
122200030721     C                   EVAL      VABCCM = *zeros
122300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122400040510     C                             + ' ' + 'VABCCM'
122500030721     C                   ENDIF
122600030721     C                   ENDIF
122700030721     C*** LNP
122800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
122900030721     C                   EVAL      PiStr=SkSplitCSV(i)
123000030721     C                   EXSR      CHKNUM
123100030721     C                   IF        PiInt=*on    AND
123200030721     C                             PiVal<=999   AND
123300030721     C                             PiVal>*zeros
123400030721     C                   Z-ADD     PiVal         VABLNP
123500030721     C                   ELSE
123600040715     C                   SETON                                        32
123700030721     C                   EVAL      VABLNP = *zeros
123800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123900040510     C                             + ' ' + 'VABLNP'
124000030721     C                   ENDIF
124100030721     C                   ENDIF
124200030721     C*** NRS
124300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
124400030721     C                   EVAL      PiStr=SkSplitCSV(i)
124500030721     C                   EXSR      CHKNUM
124600030721     C                   IF        PiInt=*on AND
124700030721     C                             PiVal<=99
124800030721     C                   Z-ADD     PiVal         VABNRS
124900030721     C                   ELSE
125000030721     C                   SETON                                        32
125100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125200030721     C                             + ' ' + 'VABNRS'
125300030721     C                   ENDIF
125400030721     C                   ENDIF
125500030721     C*** LNA
125600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
125700030721     C                   EVAL      PiStr=SkSplitCSV(i)
125800030721     C                   EXSR      CHKNUM
125900030721     C                   IF        PiInt=*on    AND
126000030721     C                             PiVal<=999   AND
126100030721     C                             PiVal>*zeros
126200030721     C                   Z-ADD     PiVal         VABLNA
126300030721     C                   ELSE
126400030721     C                   SETON                                        32
126500030721     C                   EVAL      VABLNA = *zeros
126600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126700030721     C                             + ' ' + 'VABLNA'
126800030721     C                   ENDIF
126900030721     C                   ENDIF
127000030721     C*** CTR
127100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
127200030721     C                   EVAL      PiStr=SkSplitCSV(i)
127300030721     C                   EXSR      CHKNUM
127400030721     C                   IF        PiInt=*on  AND
127500030721     C                             PiVal<=999
127600030721     C                   Z-ADD     PiVal         VABCTR
127700030721     C                   ELSE
127800030721     C                   SETON                                        32
127900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128000030721     C                             + ' ' + 'VABCTR'
128100030721     C                   ENDIF
128200030721     C                   ENDIF
128300030721     C*** IAS
128400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
128500030721     C                   EVAL      PiStr=SkSplitCSV(i)
128600030721     C                   EXSR      CHKNUM
128700030721     C                   IF        PiNum=*on
128800030721     C                   Z-ADD     PiVal         VABIAS
128900030721     C                   ELSE
129000030721     C                   SETON                                        32
129100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129200030721     C                             + ' ' + 'VABIAS'
129300030721     C                   ENDIF
129400030721     C                   ENDIF
129500030721     C*** VLB
129600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
129700030721     C                   EVAL      PiStr=SkSplitCSV(i)
129800030721     C                   EXSR      CHKNUM
129900030721     C                   IF        PiNum=*on
130000030721     C                   Z-ADD     PiVal         VABVLB
130100030721     C                   ELSE
130200030721     C                   SETON                                        32
130300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130400030721     C                             + ' ' + 'VABVLB'
130500030721     C                   ENDIF
130600030721     C                   ENDIF
130700030721     C*** QFT
130800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
130900030721     C                   EVAL      PiStr=SkSplitCSV(i)
131000030721     C                   EXSR      CHKNUM
131100030721     C                   IF        PiNum=*on
131200030721     C                   Z-ADD     PiVal         VABQFT
131300030721     C                   ELSE
131400030721     C                   SETON                                        32
131500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131600030721     C                             + ' ' + 'VABQFT'
131700030721     C                   ENDIF
131800030721     C                   ENDIF
131900030721     C*** NCD
132000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
132100030721     C                   EVAL      PiStr=SkSplitCSV(i)
132200030721     C                   EXSR      CHKNUM
132300030721     C                   IF        PiInt=*on      AND
132400030721     C                             PiVal<=9999999
132500030721     C                   Z-ADD     PiVal         VABNCD
132600030721     C                   ELSE
132700030721     C                   SETON                                        32
132800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132900030721     C                             + ' ' + 'VABNCD'
133000030721     C                   ENDIF
133100030721     C                   ENDIF
133200030721     C*** NCA
133300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
133400030721     C                   EVAL      PiStr=SkSplitCSV(i)
133500030721     C                   EXSR      CHKNUM
133600030721     C                   IF        PiInt=*on      AND
133700030721     C                             PiVal<=9999999
133800030721     C                   Z-ADD     PiVal         VABNCA
133900030721     C                   ELSE
134000030721     C                   SETON                                        32
134100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134200030721     C                             + ' ' + 'VABNCA'
134300030721     C                   ENDIF
134400030721     C                   ENDIF
134500030721     C*** ZNC
134600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
134700030721     C                   EVAL      PiStr=SkSplitCSV(i)
134800030721     C                   EXSR      CHKNUM
134900030721     C                   IF        PiInt=*on AND
135000030721     C                             PiVal<=99
135100030721     C                   Z-ADD     PiVal         VABZNC
135200030721     C                   ELSE
135300030721     C                   SETON                                        32
135400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135500030721     C                             + ' ' + 'VABZNC'
135600030721     C                   ENDIF
135700030721     C                   ENDIF
135800030721     C*** DCR
135900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
136000030721     C                   EVAL      PiStr=SkSplitCSV(i)
136100030721     C                   EXSR      CHKNUM
136200030721     C                   IF        PiInt=*on       AND
136300030721     C                             PiVal<=99999999
136400030721     C                   Z-ADD     PiVal         VABDCR
136500030721     C                   ELSE
136600030721     C                   SETON                                        32
136700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136800030721     C                             + ' ' + 'VABDCR'
136900030721     C                   ENDIF
137000030721     C                   ENDIF
137100030721     C*** HCR
137200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
137300030721     C                   EVAL      PiStr=SkSplitCSV(i)
137400030721     C                   EXSR      CHKNUM
137500030721     C                   IF        PiInt=*on   AND
137600030721     C                             PiVal<=9999
137700030721     C                   Z-ADD     PiVal         VABHCR
137800030721     C                   ELSE
137900030721     C                   SETON                                        32
138000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138100030721     C                             + ' ' + 'VABHCR'
138200030721     C                   ENDIF
138300030721     C                   ENDIF
138400030721     C*** VMD
138500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
138600030721     C                   EVAL      PiStr=SkSplitCSV(i)
138700030721     C                   EXSR      CHKNUM
138800030721     C                   IF        PiNum=*on
138900030721     C                   Z-ADD     PiVal         VABVMD
139000030721     C                   ELSE
139100030721     C                   SETON                                        32
139200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139300030721     C                             + ' ' + 'VABVMD'
139400030721     C                   ENDIF
139500030721     C                   ENDIF
139600030721     C*** ANT
139700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
139800030721     C                   EVAL      PiStr=SkSplitCSV(i)
139900030721     C                   EXSR      CHKNUM
140000030721     C                   IF        PiInt=*on        AND
140100030721     C                             PiVal<=999999999
140200030721     C                   Z-ADD     PiVal         VABANT
140300030721     C                   ELSE
140400030721     C                   SETON                                        32
140500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140600030721     C                             + ' ' + 'VABANT'
140700030721     C                   ENDIF
140800030721     C                   ENDIF
140900030721     C*** CMO
141000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
141100030721     C                   EVAL      PiStr=SkSplitCSV(i)
141200030721     C                   EXSR      CHKNUM
141300030721     C                   IF        PiInt=*on
141400030721     C                   Z-ADD     PiVal         Num5_0
141500030721     C                   MOVEL(p)  Num5_0        VABCMO
141600030721     C                   ELSE
141700030721     C                   SETON                                        32
141800030721     C                   EVAL      VABCMO = *zeros
141900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142000030721     C                             + ' ' + 'VABCMO'
142100030721     C                   ENDIF
142200030721     C                   ENDIF
142300040510     C*
142400100826     C* Reperisco i campi del EDIVAT
142500040510     C*** VATNOT
142600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
142700040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
142800040510     C                   ENDIF
142900040510     C*** VATTRC
143000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
143100040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
143200040510     C                   ENDIF
143300060331     C*** VATNOT_A
143400060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
143500060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
143600060331     C                   ENDIF
143700060331     C*** VATNOT_B
143800060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
143900060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
144000060331     C                   ENDIF
144100060331     C*** VATNOT_E
144200070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
144300060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
144400060331     C                   ENDIF
144500070823     C*
144600071029     C***  ===>  Gestione campi "particolari" dentro ciclo
144700100826     C*** RMA
144800100826     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMA'
144900100826     C                   EVAL      VABRMA = %subst(SkSplitCSV(i):1:4)  +
145000100826     C                                      %subst(SkSplitCSV(i):6:4)  +
145100100826     C                                      %subst(SkSplitCSV(i):11:1) +
145200100826     C                                      %subst(SkSplitCSV(i):13)
145300100826     C                   ENDIF
145400100826     C*** NOT_E
145500100826     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_E'
145600100903     C                   EVAL      wVATNOT_E = '000000' +
145700100903     C                                         %subst(SkSplitCSV(i):5:7)
145800100826     C                   ENDIF
145900100720     C*** RMN / NSP
146000100720     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
146100100826     C                   EVAL      PiStr=%subst(SkSplitCSV(i):1:4) +
146200100826     C                                   %subst(SkSplitCSV(i):6)
146300100413     C                   EXSR      CHKNUM
146400100413     C                   IF        PiInt=*on
146500100419     C                   Z-ADD     PiVal         VABNSP
146600100720     C                   Z-ADD     PiVal         VABRMN
146700100413     C                   ELSE
146800100720     C                   Z-ADD     *zeros        VABRMN
146900100413     C                   SETON                                        32
147000100413     C                   EVAL      vinmsg = %trimr(vinmsg)
147100100720     C                             + ' ' + 'VABNSP VABRMN'
147200100413     C                   ENDIF
147300100326     C                   ENDIF
147400070925     C*
147500071029     C***  <===  -----------------------------------------
147600040510     C*
147700030715     C                   ENDIF
147800030715     C                   EVAL      i = i + 1
147900030715     C                   ENDDO
148000070925     C*
148100071029     C***  ===>  Gestione campi "particolari" fuori ciclo
148200070925     C*
148300071029     C***  <===  -----------------------------------------
148400100826     C*
148500100826     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
148600110913     C                   EVAL      VABCMR = 'FNVA'
148700100826     C                   EVAL      VABDCM = DATCOR
148800100826     C                   EVAL      VABDTS = DATCOR
148900100826     C                   EVAL      VABHMS = ORACOR
149000100826     C                   EVAL      VABCNT = 1
149100070925     C*
149200070925     C* Considerazioni finali su CBO/CAS
149300100318     C                   IF        wFlgCAS = '1'    OR
149400100318     C                             VABCAS  > *zeros
149500070925     C                   IF        VABCBO = '1'
149600070925     C                   EVAL      VABCBO = '4'
149700070925     C                   ENDIF
149800070925     C                   IF        VABCBO = '2'
149900070925     C                   EVAL      VABCBO = '6'
150000070925     C                   ENDIF
150100070925     C                   ENDIF
150200070925     C***  <===  ----------------------------
150300040510     C*
150400040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
150500040510     C                   IF        VABNSP = *zeros
150600040510     C                   SETON                                        31
150700040510     C                   ENDIF
150800040715     C*
150900040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
151000040715     C                   IF        VABLNP = *zeros
151100040715     C                   SETON                                        31
151200040715     C                   ENDIF
151300040510     C*
151400040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
151500040510     C                   IF        VABRMN = *zeros
151600040510     C                   Z-ADD     VABNSP        VABRMN
151700040510     C                   ENDIF
151800040510     C*
151900040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
152000040510     C                   IF        VABRMA = *blanks
152100040714     C                   MOVEL     VABRMN        VABRMA
152200040510     C                   ENDIF
152300040128     C*
152400040510     C* Se provincia nn valorizzata la reperisco
152500040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
152600040510     C                   IF        VABPRD = *blanks
152700040128     C                   CLEAR                   TISI95DS
152800040128     C                   EVAL      I95TCN = '3'
152900040128     C                   Z-ADD     datcor        I95DAT
153000040128     C                   EVAL      I95NAR = VABNZD
153100040128     C                   EVAL      I95CAP = VABCAD
153200040128     C                   EVAL      I95LOC = VABLOD
153300040128     C                   CALL      'TISI95R'
153400040128     C                   PARM                    TISI95DS
153500040128     C                   EVAL      VABPRD = O95PRV
153600040510     C                   ENDIF
153700020204     C*
153800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
153900020204     C                   EXSR      CHKIMPDIV
154000030715     C                   ENDIF
154100020204     C*
154200010607     C                   ENDSR
154300010607     C*----------------------------------------------------*
154400010601
154500020204
154600020204     C*----------------------------------------------------*
154700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
154800020204     C*----------------------------------------------------*
154900020204     C     CHKIMPDIV     BEGSR
155000020204     C*
155100020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
155200020204     C                   Z-ADD     *zeros        wrkDec            9 9
155300020204     C*
155400020204     C* Come prima cosa effettuo considerazioni sulla divisa
155500020204     C                   IF        vabIAS > *zeros
155600020204     C                   IF        vabVAS <> 'EUR'
155700020204     C                   EVAL      vabVAS =  'ITL'
155800020204     C                   ENDIF
155900020204     C                   ENDIF
156000020204     C*
156100020204     C                   IF        vabCAS > *zeros
156200020204     C                   IF        vabVCA <> 'EUR'
156300020204     C                   EVAL      vabVCA =  'ITL'
156400020204     C                   ENDIF
156500020204     C                   ENDIF
156600020204     C*
156700020204     C                   IF        vabVMD > *zeros
156800020204     C                   IF        vabVAD <> 'EUR'
156900020204     C                   EVAL      vabVAD =  'ITL'
157000020204     C                   ENDIF
157100020204     C                   ENDIF
157200020204     C*
157300020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
157400020204     C                   Z-ADD     vabIAS        wrkDec
157500020204     C                   IF        wrkDec > *zeros
157600020204     C                   IF        vabVAS = 'ITL'
157700020204     C                   EVAL      vabIAS = *zeros
157800020204     C                   ENDIF
157900020204     C                   ENDIF
158000020204     C*
158100020204     C* Stabilisco se il contrasegno ha decimali valorizzati
158200020204     C                   Z-ADD     vabCAS        wrkDec
158300020204     C                   IF        wrkDec > *zeros
158400020204     C                   IF        vabVCA = 'ITL'
158500020204     C                   EVAL      vabCAS = *zeros
158600020204     C                   ENDIF
158700020204     C                   ENDIF
158800020204     C*
158900020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
159000020204     C                   Z-ADD     vabVMD        wrkDec
159100020204     C                   IF        wrkDec > *zeros
159200020204     C                   IF        vabVAD = 'ITL'
159300020204     C                   EVAL      vabVMD = *zeros
159400020204     C                   ENDIF
159500020204     C                   ENDIF
159600020204     C*
159700020204     C                   ENDSR
159800020204     C***
159900010330
160000100318
160100100318
160200100318     C*----------------------------------------------------*
160300100318     C*  CONTROLLO NUMERICITA' CAMPI
160400100318     C*----------------------------------------------------*
160500100318     C     CHKNUM        BEGSR
160600100318     C*
160700100318     C                   IF        PiDecChr = *blanks
160800100318     C                   EVAL      PiDecChr = CharNUM
160900100318     C                   ENDIF
161000100318     C*
161100100318     C                   callp     UBISNUM_Check(PiStr
161200100318     C                                          :PiDecChr
161300100318     C                                          :PiVal
161400100318     C                                          :PiNum
161500100318     C                                          :PiInt)
161600100318     C*
161700100318     C                   ENDSR
161800100318     C***
161900010601
162000010601
162100010601
162200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
162300010601     C     repfil        BEGSR
162400010601     C*
162500010601     C                   if        invfil = *zeros and
162600010601     C                             depfil > *zeros and
162700010629     C                             (vinflg = *blanks or
162800010629     C                              vinflg = *zeros)
162900010601     C
163000010601     C                   eval      invfil = depfil
163100010601     C                   endif
163200010601     C*
163300010601     C                   if        depfil <> invfil and
163400010601     C                             invfil > *zeros
163500010601     C                   eval      flgMulti = '1'
163600010601     C                   if        vinflg = *blanks
163700010601     C                   add       1             cntNonEl
163800010601     C                   endif
163900010601     C                   endif
164000010601     C*
164100010601     C                   if        vinflg = '2'
164200010601     C                   eval      flgStato = '2'
164300010601     C                   endif
164400010601     C*
164500010601     C                   ENDSR
164600010601     C***
164700010601
164800010601
164900010601
165000010330
165100040119      /TITLE Invio dei dati al punto operativo.
165200040119     C     invio         BEGSR
165300040119     C*
165400100826     C* 1° invio EDIVAT
165500040119     C                   reset                   dscmz
165600040119     C                   move      vlrpoi        cmzdst
165700100826     C                   eval      cmzfld = 'EDIVATWR'
165800040119     C                   eval      cmzmbd = vlrhdl
165900040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
166000040119     C***                if        prmfir = *blanks
166100100826     C                   eval      cmzfla = 'EDIVAT0F'
166200100826     C                   eval      cmzmba = 'EDIVAT0F'
166300040119     C***                else
166400040119     C***                eval      cmzfla = prmfir
166500040119     C***                eval      cmzmba = prmfir
166600040119     C***                endif
166700040119     C                   eval      cmznrr = *zeros
166800040119     C                   move      §ctrokvt      cmznrr
166900040119     C                   eval      cmzlba = vlrfl1
167000040119     C                   call(e)   'TIS711C'
167100040119     C                   parm                    dscmz
167200040119     C                   parm      *blanks       esito
167300040119     C                   if        %error
167400040119     C                             or cmzerr = '1'
167500040119     C                             or esito  = '1'
167600040119     C                   eval      wrkesito = '3'
167700040119     C                   else
167800040119     C*
167900100826     C* 2° invio EDIVAB
168000040119     C                   reset                   dscmz
168100040119     C                   move      vlrpoi        cmzdst
168200040119     C                   eval      cmzfld = vlrfou
168300040119     C                   eval      cmzmbd = vlrhdl
168400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
168500040119     C***                if        prmfir = *blanks
168600100826     C                   eval      cmzfla = 'EDIVAB0F'
168700100826     C                   eval      cmzmba = 'EDIVAB0F'
168800040119     C***                else
168900040119     C***                eval      cmzfla = prmfir
169000040119     C***                eval      cmzmba = prmfir
169100040119     C***                endif
169200040119     C                   eval      cmznrr = *zeros
169300040119     C                   move      §ctrokvb      cmznrr
169400040119     C                   eval      cmzlba = vlrfl1
169500040119     C                   call(e)   'TIS711C'
169600040119     C                   parm                    dscmz
169700040119     C                   parm      *blanks       esito
169800040119     C                   if        %error
169900040119     C                             or cmzerr = '1'
170000040119     C                             or esito  = '1'
170100040119     C                   eval      wrkesito = '3'
170200040119     C                   endif
170300040119     C                   endif
170400040119     C*
170500040119     C                   ENDSR
170600040119     C***
170700010601
170800010601
170900010601
171000010601
171100010601      /TITLE Invio dei dati al punto operativo.
171200010601     C     opeini        BEGSR
171300010601     C*
171400010601     C* Inizializzo flag e contatori operativi
171500010601     C                   movel     '0'           flgGiro           1
171600010601     C                   movel     '0'           flgMulti          1
171700010601     C                   movel     '1'           flgStato          1
171800010615     C                   movel     '0'           flgOk             1
171900010601     C                   z-add     *zeros        cntNonEl         10 0
172000010601     C                   z-add     *zeros        depfil            3 0
172100010601     C                   z-add     *zeros        invfil            3 0
172200010601     C*
172300010601     C                   ENDSR
172400010601     C***
172500070326
172600070326
172700070326
172800070326
172900070326     C     *pssr         BEGSR
173000070329     C*
173100070329     C                   if        %open(tivin00r)
173200070329     C                   close     tivin00r
173300070329     C                   endif
173400100826     C                   if        %open(edivabwr)
173500100826     C                   close     edivabwr
173600070329     C                   endif
173700100826     C                   if        %open(edivatwr)
173800100826     C                   close     edivatwr
173900070329     C                   endif
174000070326     C*
174100070326     C* Effettuo la chiamata al CLLE preposto
174200100826     C                   call(e)   'TITVEVTC'
174300070326     C                   parm                    parccm
174400070326     C                   parm                    parmbr
174500070326     C                   parm      '2'           paropz
174600070326     C*
174700070326     C                   eval      wrkesito = '2'
174800070404     C*
174900070404     C                   seton                                        LR
175000070326     C*
175100070326     C                   ENDSR     '*CANCL'
175200070326     C***
175300100318
175400100318
175500100318
175600100318     C     normCSV       BEGSR
175700100318     C*
175800100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
175900100318     C* separatori campi
176000100318     C                   if        CharTXT <> *blanks
176100100318     C                   z-add     *zeros        wLenVINDTA        4 0
176200100318     C                   z-add     *zeros        wFlgVINDTA        1 0
176300100318     C                   setoff                                       80
176400100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
176500100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
176600100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
176700100318     C                   if        not *in80
176800100318     C                   seton                                        80
176900100318     C                   else
177000100318     C                   setoff                                       80
177100100318     C                   endif
177200100318     C                   endif
177300100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
177400100318     C                             and *in80 = *on
177500100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
177600100318     C                   endif
177700100318     C                   enddo
177800100318     C                   endif
177900100318     C*
178000100318     C                   ENDSR
178100100318     C***
178200070326
178300100326
178400100326
178500100326     C*----------------------------------------------------*
178600100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
178700100326     C*----------------------------------------------------*
178800100326     C     REPNSP        BEGSR
178900100326     C*
179000100326     C* NSP => Stacco un numeratore da AZNUM
179100100326     C                   clear                   TRUL33DS
179200100326     C                   eval      I33OPE = *zeros
179300100326     C                   eval      I33CNU = 302
179400100326     C                   eval      I33NUM = 1
179500100326     C                   movel     TRUL33DS      KPJBU
179600100326     C                   call      'TRUL33R'
179700100326     C                   parm                    KPJBA
179800100326     C                   movel     KPJBU         TRUL33DS
179900100326     C                   if        O33ERR = *zeros
180000100326     C                   z-add     O33NRF        VABNSP
180100100326     C                   z-add     O33NRF        VATNSP
180200100326     C                   else
180300100326     C                   SETON                                        31
180400100326     C                   EVAL      vinmsg = %trimr(vinmsg)
180500100326     C                             + ' ' + 'VABNSP VATNSP'
180600100326     C                   endif
180700100326     C*
180800100326     C                   ENDSR
180900010330
181000100326
181100010330
181200000613     C     *inzsr        BEGSR
181300990910     C*
181400990910     C     *entry        plist
181500990920     C                   parm                    tivlrds
181600990921     C                   parm      wrkesito      esito
181700000724     C                   parm                    prmlit
181800000710     C                   parm                    prmfir
181900100826     C*
182000100826     C* CALCOLA LA DATA CORRENTE
182100100826     C                   time                    wn14             14 0
182200100826     C                   movel     wn14          oracor            6 0          *ORA
182300100826     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
182400100826     C                   eval      datcor = %dec(%date() : *ISO)
182500000613     C*
182600000613     C                   ENDSR
182700000613     C***
182800990908
182900100720     Otitv1y8p  E            riepilogo         2
183000990915     O                                              'Upload via Internet'
183100990915     O                                           +1 'Traduzione TIVIN00R -'
183200100826     O                                           +1 'EDIVABWR'
183300990915     O                                           +1 'Testate bolle'
183400990915     O          E            riepilogo   2
183500990915     O                       wrkdata
183600990915     O                       wrkora              +1
183700990915     O                       procname            +1
183800990915     O          E            riepilogo   2
183900990915     O                                              'Cliente..................:'
184000010605     O                       VABCCM        z     +1
184100990915     O          E            riepilogo   2
184200990920     O                                              'Riferimento Strategi.....:'
184300990920     O                       vlrhdl              +1
184400990915     O          E            riepilogo   2
184500990915     O                                              'Giusti...................:'
184600040119     O                       §CTROKVB      2   +  1
184700990915     O          E            riepilogo   2
184800990915     O                                              'Sbagliati e corretti.....:'
184900971022     O                       §CTRMO        2   +  1
185000990915     O          E            riepilogo   2
185100990915     O                                              'Sbagliati e scartati.....:'
185200971022     O                       §CTRNO        2   +  1
185300000613
185400100720     Otitv1y8ps E            testdett          2
185500000613     O                                              'Upload via Internet'
185600000613     O                                           +1 'Traduzione TIVIN00R -'
185700100826     O                                           +1 'EDIVABWR'
185800000613     O                                           +1 'Testate bolle'
185900000616     O          E            testdett    3
186000000613     O                                           +2 'N° rec'
186100000613     O                                           +3 'Anteprima contenuto'
186200000616     O          E            rigadett    2
186300000613     O                       rrnum               +2
186400000621     O                       recko               +3
186500000616     O          E            findett     2
186600000613     O                       wrkdata
186700000613     O                       wrkora              +1
186800000613     O                       procname            +1
186900000616     O          E            findett     2
187000000613     O                                              'Cliente..................:'
187100010605     O                       VABCCM        z     +1
187200000616     O          E            findett     2
187300000613     O                                              'Riferimento Strategi.....:'
187400000613     O                       vlrhdl              +1
187500000616     O          E            findett     2
187600000613     O                                              'Giusti...................:'
187700040119     O                       §CTROKVB      2   +  1
187800000616     O          E            findett     2
187900000613     O                                              'Sbagliati e corretti.....:'
188000000613     O                       §CTRMO        2   +  1
188100000616     O          E            findett     2
188200000613     O                                              'Sbagliati e scartati.....:'
188300000613     O                       §CTRNO        2   +  1
188400000613** CMD - COMANDI CL
188500100720OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1Y8P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
188600100720OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1Y8PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
188700100720DLTOVR FILE(TITV1Y8P TITV1Y8PS) LVL(*)
188800010606
188900000613
189000040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
189100100826FIL001;W_VABRMA;FIL002;W_VATNOT_E;VABPKB;FIL003;VABVLB;FIL004;W_VABRMN;
189200100826FIL005;FIL006;FIL007;FIL008;FIL009;VABRSD;VABRD2;VABIND;VABCAD;VABLOD;
189300100826FIL010;FIL011;VATNOT_B;
189400100318
189500100318
