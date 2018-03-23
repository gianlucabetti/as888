000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200100318     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
000700040119     FFIVATwwr  O    E             DISK    usropn
000800110627     Ftitv2g4p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070329     F                                     infsr(*pssr)
001100110627     Ftitv2g4ps O    f  198        PRINTER usropn
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
003100100413     D fivabds       e ds                  extname(fivab00f)
003200100413     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
003300100413     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
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
007400100831     D wVABRSD         s                   LIKE(VABRSD)
007500100831     D wVABIND         s                   LIKE(VABIND)
007600100831     D wVABCAD         s                   LIKE(VABCAD)
007700100831     D wVABLOD         s                   LIKE(VABLOD)
007800100831     D wVABPRD         s                   LIKE(VABPRD)
007900030822     D*------------------
008000030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
008100030822     D*------------------
008200030822     D posDaDft        S              3  0 INZ(*zeros)
008300030822     D posADft         S              3  0 INZ(*zeros)
008400030822     D j               s              3  0 INZ(1)
008500030822     D wGiroDft        s              1  0 INZ(*zeros)
008600080418     D*------------------
008700080418     D* Costanti
008800080418     D*------------------
008900080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009000080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009100100318
009200100318     D*------------------
009300100318     D* LINKING A DEFINIZIONI ESTERNE
009400100318     D*------------------
009500100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009600100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009700100318
009800100909     D*------------------
009900100909     D* DS CALCOLO CHECK-DIGITS
010000100909     D*------------------
010100100909     D TRUL28DSX     e ds                  inz
010200010330
010300010330
010400010330
010500990915     C                   time                    wrkdata
010600990915     C                   time                    wrkora
010700000913     C                   reset                   rrnum
010800990921     C                   reset                   esito
010900990921     C                   reset                   wrkesito
011000010601     C*
011100010601     C                   exsr      opeini
011200010605     C                   exsr      rwvab
011300010601     C*
011400010601     C                   seton                                        lr
011500010601
011600010601
011700010601
011800010601     C*--------------------------------------------------------
011900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
012000010601     C*--------------------------------------------------------
012100010601     C     PREELA        BEGSR
012200010601     C*
012300010601     C* SE OCCORRE SPEDIRE IN FILIALE
012400010601     C                   if        invfil <> *zeros and
012500010601     C                             flgGiro = '0'
012600010601     C*
012700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012800010601     C                   eval      flgGiro = '1'
012900000724     C*
013000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
013100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
013200010601     C     invfil        chain     azorg01l
013300000724     C                   if        %found
013400000724     C                   movel(p)  CMD(1)        depcmd
013500020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
013600000724     C*
013700000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
013800011113     C                   Z-ADD     150           LENGH            15 5
013900000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014000000724     C                   PARM                    depcmd
014100000724     C                   PARM                    LENGH
014200000724     C*
014300000724     C                   endif
014400000616     C*
014500000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
014600000616     C                   movel(p)  CMD(2)        depcmd
014700000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
014800000616     C*
014900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
015000011113     C                   Z-ADD     150           LENGH            15 5
015100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
015200000616     C                   PARM                    depcmd
015300000616     C                   PARM                    LENGH
015400000613     C*
015500110627     C                   if        not %open(titv2g4ps)
015600110627     C                   open      titv2g4ps
015700070329     C                   except    testdett
015800000613     C                   endif
015900010601     C*
016000010601     C                   endif
016100010601     C*
016200010601     C                   ENDSR
016300010601     C***
016400010601
016500010601
016600010601
016700010601
016800010601     C*--------------------------------------------------------
016900010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
017000010601     C*--------------------------------------------------------
017100010601     C     ENDELA        BEGSR
017200990915     C*
017300000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
017400040119     C*
017500040119     C* Effettuo la chiamata al CLLE preposto
017600040119     C                   call(e)   'TITVVTC'
017700040510     C                   parm                    parccm
017800040119     C                   parm                    parmbr
017900040119     C                   parm      '2'           paropz
018000050201     C*
018100050201     C* Effettuo lancio TISI95 solo x chiusura
018200050201     C                   CLEAR                   TISI95DS
018300050201     C                   EVAL      I95TLA = 'C'
018400050201     C                   CALL      'TISI95R'
018500050201     C                   PARM                    TISI95DS
018600000613     C*
018700110627     C                   if        %open(titv2g4ps)
018800000616     C                   except    findett
018900110627     C                   close     titv2g4ps
019000000613     C                   endif
019100000616     C*
019200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
019300011113     C                   Z-ADD     150           LENGH            15 5
019400010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
019500000616     C                   PARM                    CMD(3)
019600000616     C                   PARM                    LENGH
019700000616     C*
019800010601     C                   ENDSR
019900010601     C***
020000010601
020100010601
020200010601
020300000613
020400000613     C*--------------------------------------------------------
020500000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
020600000613     C*--------------------------------------------------------
020700000621     C     STPR          BEGSR
020800000613     C*
020900110627     C                   if        not %open(titv2g4p)
021000110627     C   50              open      titv2g4p
021100990915     C                   endif
021200990915     C*
021300030715     C   50              except    riepilogo
021400990915     C*
021500110627     C                   if        %open(titv2g4p)
021600110627     C   50              close     titv2g4p
021700990914     C                   endif
021800000613     C*
021900000613     C                   ENDSR
022000000613     C***
022100000613
022200010601
022300010601
022400010330     C*--------------------------------------------------------
022500021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
022600010330     C*--------------------------------------------------------
022700010605     C     RWVAB         BEGSR
022800010330     C*
022900010330     C                   if        not %open(tivin00r)
023000010330     C                   open      tivin00r
023100010330     C                   endif
023200021025     C                   if        not %open(fivabwwr)
023300021025     C                   open      fivabwwr
023400010330     C                   endif
023500040119     C*
023600040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
023700040119     C                   exsr      prevat
023800040119     C*
023900040119     C                   if        chkcall = '0'
024000040119     C*
024100040119     C                   if        not %open(fivatwwr)
024200040119     C                   open      fivatwwr
024300040119     C                   endif
024400010330     C*
024500040119     C                   clear                   §CTROKVB          7 0
024600040119     C                   clear                   §CTROKVT          7 0
024700040119     C                   clear                   §CTRMO            7 0
024800040119     C                   clear                   §CTRNO            7 0
024900070823     C*
025000070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
025100070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
025200110415     C                   exsr      inzvar
025300110415     C                   exsr      impvab
025400110131     C*
025500110131     C* Effettuo una prima lettura fuori ciclo x ignorare la 1° riga
025600110315 xxx C***                read      tivin00r                               70
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
027700010618     C*
027800040510     C                   if        vindta > *blanks AND
027900040510     C                             wFlgVINDTA = 1
028000010330     C                   add       1             rrnum
028100010330     C*
028200010601     C                   if        *in70 = *off and
028300010330     C                             (vinflg = *blanks
028400010330     C                              or vinflg = '0'
028500010330     C                              or vinflg = '2')
028600010330     C*
028700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
028800010711     C                   if        vinflg = *blanks or vinflg = '0'
028900010711     C                   clear                   vinmsg
029000010711     C                   endif
029100010601     C*
029200070925     C* Aggiungo un byte a blanks all'inizio d ogni record
029300070925     C                   eval      vindta  = ' ' + %trim(vindta)
029400070925     C*
029500080527     C                   exsr      inzvar
029600100318     C                   exsr      normCSV
029700010605     C                   exsr      impvab
029800010601     C*
029900010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
030000010605     C                   eval      depfil = VABLNP
030100010601     C                   exsr      repfil
030200010601     C                   if        depfil = invfil
030300021025     C                   if        vlrpoi = 999
030400030822     C                   move(P)   invfil        VABFGS
030500021025     C                   else
030600030822     C                   move(P)   vlrpoi        VABFGS
030700021025     C                   endif
030800010601     C*
030900010601     C                   exsr      PREELA
031000010601     C*
031100010604     C* Ebbene...
031200010604     C*
031300040119     C  N31              add       1             §CTROKVB
031400040119     C   32              add       1             §CTRMO
031500040119     C   31              add       1             §CTRNO
031600080418     C*
031700030822     C                   if        wGiro = 2
031800080513     C* Scrivo sempre il VAT
031900080513     C  N31              exsr      wrivat
032000110201     C                   exsr      chkvab
032100080513     C                   endif
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
034200040119     C*
034300080527     C* Verifico dati rimasti "in cannna"...
034400080527     C                   seton                                        65
034500110201     C                   exsr      chkvab
034600080527     C*
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
037600021025     C                   if        %open(fivabwwr)
037700021025     C                   close     fivabwwr
037800010330     C                   endif
037900040512     C                   if        %open(fivatwwr)
038000040512     C                   close     fivatwwr
038100040512     C                   endif
038200010601     C*
038300010601     C                   if        vlrpoi <> 999
038400010601     C                   eval      invfil = vlrpoi
038500010601     C                   endif
038600010330     C*
038700040119     C                   if        §ctrokvb > 0
038800010601     C                             and invfil > *zeros
038900010330     C                   exsr      invio
039000010330     C                   endif
039100010601     C*
039200010618     C                   if        flgGiro = '1'
039300010601     C                   exsr      endela
039400010618     C                   endif
039500010330     C*
039600010330     C                   ENDSR
039700010330     C***
039800080527
039900080527
040000080527
040100080527     C*----------------------------------------------------*
040200080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
040300080527     C*----------------------------------------------------*
040400100413     C     CHKVAB        BEGSR
040500110201     C*
040600110201     C* Se richiesti ignorare colli / peso / volume
040700110315 xxx C***                if        *in35
040800110315 xxx C***                z-add     *zeros        VABNCL
040900110315 xxx C***                z-add     *zeros        VABPKB
041000110315 xxx C***                z-add     *zeros        VABVLB
041100110315 xxx C***                endif
041200080527     C*
041300080527     C* Scrivo il VAB solo a rottura d spedizione
041400080527     C                   if        depspe = *zeros
041500080527     C  N31              eval      fivabds_sav = fivabds
041600080527     C                   eval      depspe = vabNSP
041700080527     C                   else
041800080527     C                   if        depspe = vabNSP AND *in65 = *off
041900100413     C   51              add       VABNCL        sav_VABNCL
042000100413     C   51              add       VABPKB        sav_VABPKB
042100100413     C   51              add       VABVLB        sav_VABVLB
042200100720     C   52              z-add     VABNCL        sav_VABNCL
042300100720     C   52              z-add     VABPKB        sav_VABPKB
042400100720     C   52              z-add     VABVLB        sav_VABVLB
042500100720     C   53              add       VABNCL        sav_VABNCL
042600100720     C   53              z-add     VABPKB        sav_VABPKB
042700100720     C   53              z-add     VABVLB        sav_VABVLB
042800080527     C                   else
042900080527     C  N31              eval      fivabds_ok = fivabds_sav
043000080527     C  N31              exsr      wrivab
043100080527     C  N31              eval      fivabds_sav = fivabds
043200080527     C                   eval      depspe = vabNSP
043300080527     C                   endif
043400080527     C                   endif
043500080527     C*
043600080527     C                   ENDSR
043700040119
043800040119
043900040119
044000040119     C*----------------------------------------------------*
044100040119     C*  SCARICAMENTO BUFFER RECORDS VAB
044200040119     C*----------------------------------------------------*
044300040510     C     WRIVAB        BEGSR
044400080513     C*
044500080513     C* Gestisco l'eventuale rottura x numero spedizione
044600080527     C                   if        VABNSP <> depspe or *in65 = *on
044700070301     C                   if        wVATNOT_E <> *blanks
044800070301     C                   eval      VABCTM = '7Q'
044900070301     C                   endif
045000100413     C                   write     fivab000
045100080513     C                   endif
045200040119     C*
045300040119     C                   ENDSR
045400040119
045500040119
045600040119
045700040119     C*----------------------------------------------------*
045800040119     C*  SCARICAMENTO BUFFER RECORDS VAT
045900040119     C*----------------------------------------------------*
046000040510     C     WRIVAT        BEGSR
046100060512     C*
046200060512     C* Inizializzo i campi chiave
046300060512     C                   eval      VATFGS = VABFGS
046400060512     C                   eval      VATAAS = VABAAS
046500060512     C                   eval      VATCCM = VABCCM
046600070823     C                   eval      VATNRS = VABNRS
046700060512     C                   eval      VATNSP = VABNSP
046800060512     C                   eval      VATLNP = VABLNP
046900040119     C*
047000040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
047100040510     C                   if        VATTRC = *blanks
047200040510     C                   eval      VATTRC = 'E'
047300040510     C                   endif
047400110318     C                   if        VATNOT <> *blanks
047500040510     C                   write     FIVAT000
047600040119     C                   add       1             §CTROKVT
047700110318     C                   endif
047800060331     C*
047900060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
048000060331     C                   if        wVATNOT_A <> *blanks
048100060331     C                   eval      VATNOT = wVATNOT_A
048200060331     C                   eval      VATTRC = 'A'
048300060331     C                   write     FIVAT000
048400060331     C                   add       1             §CTROKVT
048500060331     C                   endif
048600060331     C*
048700060331     C                   if        wVATNOT_B <> *blanks
048800060331     C                   eval      VATNOT = wVATNOT_B
048900060331     C                   eval      VATTRC = 'B'
049000060331     C                   write     FIVAT000
049100060331     C                   add       1             §CTROKVT
049200060331     C                   endif
049300060331     C*
049400060331     C                   if        wVATNOT_E <> *blanks
049500110201     C                   eval      VATNOT = wVATNOT_E
049600060331     C                   eval      VATTRC = 'E'
049700110201     C                   write     FIVAT000
049800110201     C                   add       1             §CTROKVT
049900110201     C                   endif
050000040119     C*
050100040119     C                   ENDSR
050200040119
050300040119
050400040119
050500040119     C*----------------------------------------------------*
050600040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
050700040119     C*----------------------------------------------------*
050800040119     C     PREVAT        BEGSR
050900040119     C*
051000040119     C* Compongo il nome del membro da dare al FIVATWWR
051100040119     C                   eval      parmbr = vlrhdl
051200040119     C                   movel     'M'           parmbr
051300040510     C                   eval      parccm = vlrksc
051400040119     C                   eval      paropz = '1'
051500040119     C* Effettuo la chiamata al CLLE preposto
051600040119     C                   call(e)   'TITVVTC'
051700040119     C                   parm                    parccm
051800040119     C                   parm                    parmbr
051900040119     C                   parm                    paropz
052000040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
052100040119     C                   if        %error
052200040119     C                   movel     '1'           chkcall
052300040119     C                   else
052400040119     C                   movel     '0'           chkcall
052500040119     C                   endif
052600040119     C*
052700040119     C                   ENDSR
052800010601
052900010601
053000010601
053100010330     C*----------------------------------------------------*
053200030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
053300010330     C*----------------------------------------------------*
053400040119     C     INZVAR        BEGSR
053500080527     C*
053600080527     C                   setoff                                       65
053700010330     C*
053800030822     C* Inizializzo variabili di wrk
053900020204     C                   Z-ADD     *zeros        Num5_0            5 0
054000101018     C                   MOVEL     *blanks       wFlgCAS
054100060331     C                   MOVEL     *blanks       wVATNOT_A
054200060331     C                   MOVEL     *blanks       wVATNOT_B
054300060331     C                   MOVEL     *blanks       wVATNOT_E
054400100831     C                   MOVEL     *blanks       wVABRSD
054500100831     C                   MOVEL     *blanks       wVABIND
054600100831     C                   MOVEL     *blanks       wVABCAD
054700100831     C                   MOVEL     *blanks       wVABLOD
054800100831     C                   MOVEL     *blanks       wVABPRD
054900030822     C*
055000030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
055100030822     C                   CLEAR                   SkSplitCSV
055200030822     C                   EVAL      i = 1
055300030822     C                   EVAL      posDa = *zeros
055400030822     C                   EVAL      posA  = *zeros
055500080527     C*
055600080527     C                   CLEAR                   FIVABDS
055700080527     C                   CLEAR                   FIVAT000
055800030822     C*
055900030822     C* Reimposto i valori di default
056000030822     C                   EXSR      DEFCAM
056100030822     C*
056200010330     C                   ENDSR
056300010330     C*----------------------------------------------------*
056400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
056500010330     C*----------------------------------------------------*
056600010330     C     DEFCAM        BEGSR
056700030822     C*
056800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
056900030715     C* e delimitatore testo.
057000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
057100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
057200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
057300100318     C*
057400100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
057500100318     C                   EVAL      CharSOS = CharNUM
057600030822     C*
057700030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
057800100720     C                   SETOFF                                       505152
057900100720     C                   SETOFF                                       53
058000100413     C                   SELECT
058100100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
058200100720     C                   SETON                                        50
058300100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
058400100413     C                   SETON                                        51
058500100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
058600100720     C                   SETON                                        52
058700100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
058800100720     C                   SETON                                        53
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
085900021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
086000010607     C*----------------------------------------------------*
086100010607     C     IMPVAB        BEGSR
086200010607     C*
086300010607     C                   SETOFF                                       3132
086400110315 xxx C***                SETON                                        35
086500030515     C*
086600010607     C                   MOVEL     datcor        VABAAS
086700010607     C                   MOVE      datcor        VABMGS
086800010607     C*
086900030715     C**********
087000030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
087100030715     C**********
087200030715     C                   IF        wGiro = *zeros
087300040113     C*
087400040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
087500040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
087600080429     C                             %trim(intcol(3))+%trim(intcol(4))+
087700080429     C                             %trim(intcol(5))<>*blanks
087800040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
087900080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
088000080429     C                                      %trim(intcol(5))
088100070823     C     ';':CharCSV   XLATE     vindta        vindta
088200040510     C                   ENDIF
088300031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
088400031201     C     minu:maiu     XLATE     vindta        vindta
088500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088600030715     C                   DOW       posDa <= %len(%trim(vindta))
088700030715     C*
088800030715     C* Gestisco il 1° campo
088900030715     C                   IF        i = 1
089000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
089100030715     C                   EVAL      posDa = 1
089200030715     C* Eseguo lo scan x trovare la fine del primo campo
089300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089400030715     C* A questo punto "estraggo" il campo corrente
089500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089600030715     C                                                    (posA-posDa))
089700030715     C* X i campi successivi al 1°
089800030715     C                   ELSE
089900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
090000030715     C                   EVAL      posDa = posA + 1
090100030715     C* Eseguo lo scan x trovare la fine del campo corrente
090200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090300030715     C* Gestisco l'ultimo campo
090400030715     C                   IF        posA = *zeros
090500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090600030715     C* A questo punto "estraggo" il campo corrente
090700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
090800030715     C                                                    (posA-posDa))
090900030715     C                   LEAVE
091000030715     C                   ELSE
091100030715     C* A questo punto "estraggo" il campo corrente
091200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
091300030715     C                                                    (posA-posDa))
091400030715     C                   ENDIF
091500030715     C                   ENDIF
091600030715     C* Incremento il contatore d campo
091700030715     C                   EVAL      i = i +1
091800030715     C                   ENDDO
091900030715     C*
092000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092100030715     C* delimitazione testo
092200030715     C                   EVAL      i = 1
092300030715     C                   DOW       i < %elem(SkSplitFLD)
092400030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
092500030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
092600030715     C                   EVAL      i = i + 1
092700030715     C                   ENDDO
092800030715     C                   EVAL      wGiro = 1
092900030715     C                   ELSE
093000030715     C                   EVAL      wGiro = 2
093100031204     C**********
093200031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
093300031204     C* d separatore campo
093400031204     C**********
093500031204     C                   EVAL      vindta = %trim(vindta)
093600031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
093700031204     C                   EVAL      vindta = %subst(vindta:2)
093800031204     C                   ENDDO
093900031204     C*
094000031204     C                   Z-ADD     *zeros        lunghInput        4 0
094100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
094200031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
094300031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
094400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
094500031204     C                   ENDDO
094600030715     C**********
094700030715     C* Effettuo lo split del campo dati d input
094800030715     C**********
094900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
095000030715     C                   DOW       posDa <= %len(%trim(vindta))
095100030715     C*
095200030715     C* Gestisco il 1° campo
095300030715     C                   IF        i = 1
095400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
095500030715     C                   EVAL      posDa = 1
095600030715     C* Eseguo lo scan x trovare la fine del primo campo
095700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
095800030715     C* A questo punto "estraggo" il campo corrente
095900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096000030715     C                                                    (posA-posDa))
096100030715     C* X i campi successivi al 1°
096200030715     C                   ELSE
096300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
096400030715     C                   EVAL      posDa = posA + 1
096500031204     C* Verifico che nn vi sia il campo nn valorizzato
096600031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
096700031204     C* Se campo nn valorizzato skippo al prossimo
096800031204     C                   EVAL      SkSplitCSV(i) = *blanks
096900031204     C                   EVAL      posA  = posA + 1
097000031204     C                   ELSE
097100030715     C* Eseguo lo scan x trovare la fine del campo corrente
097200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
097300030715     C* Gestisco l'ultimo campo
097400030715     C                   IF        posA = *zeros
097500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
097600030715     C* A questo punto "estraggo" il campo corrente
097700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
097800030715     C                                                    (posA-posDa))
097900030715     C                   LEAVE
098000030715     C                   ELSE
098100030715     C* A questo punto "estraggo" il campo corrente
098200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
098300030715     C                                                    (posA-posDa))
098400030715     C                   ENDIF
098500031204     C                   ENDIF
098600030715     C                   ENDIF
098700030715     C* Incremento il contatore d campo
098800030715     C                   EVAL      i = i +1
098900030715     C                   ENDDO
099000030715     C*
099100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
099200030715     C* delimitazione testo
099300030715     C                   EVAL      i = 1
099400030715     C                   DOW       i < %elem(SkSplitCSV)
099500030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
099600030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
099700030715     C                   EVAL      i = i + 1
099800030715     C                   ENDDO
099900100720     C*
100000100720     C* Stacco un progressivo x numero spedizione
100100110421 xxx C                   EXSR      repNSP
100200100326     C*
100300030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
100400030715     C                   EVAL      i = 1
100500030715     C                   DOW       i < %elem(SkSplitFLD)
100600030715     C*
100700030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
100800030715     C                   IF        SkSplitFLD(i) <> *blanks AND
100900030715     C                             SkSplitCSV(i) <> *blanks
101000030721     C*** ATB
101100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
101200030721     C                   EVAL      VABATB = SkSplitCSV(i)
101300030721     C                   ENDIF
101400030715     C*** RSD
101500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
101600030715     C                   EVAL      VABRSD = SkSplitCSV(i)
101700030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
101800030715     C     '@':'A'       XLATE     VABRSD        VABRSD
101900030715     C* ==
102000030715     C                   ENDIF
102100030721     C*** RD2
102200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
102300030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
102400030721     C                   ENDIF
102500030715     C*** IND
102600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
102700030715     C                   EVAL      VABIND = SkSplitCSV(i)
102800030715     C                   ENDIF
102900030715     C*** LOD
103000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
103100030715     C                   EVAL      VABLOD = SkSplitCSV(i)
103200030715     C                   ENDIF
103300030715     C*** PRD
103400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
103500030715     C                   EVAL      VABPRD = SkSplitCSV(i)
103600030715     C                   ENDIF
103700030721     C*** NZD
103800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
103900030721     C                   EVAL      VABNZD = SkSplitCSV(i)
104000030721     C                   ENDIF
104100030715     C*** RMA
104200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
104300030715     C                   EVAL      VABRMA = SkSplitCSV(i)
104400030715     C                   ENDIF
104500030715     C*** NOT
104600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
104700030715     C                   EVAL      VABNOT = SkSplitCSV(i)
104800030715     C                   ENDIF
104900030715     C*** NT2
105000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
105100030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
105200030715     C                   ENDIF
105300030715     C*** VCA
105400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
105500030715     C                   EVAL      VABVCA = SkSplitCSV(i)
105600030715     C                   ENDIF
105700030715     C*** VAS
105800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
105900030715     C                   EVAL      VABVAS = SkSplitCSV(i)
106000030715     C                   ENDIF
106100030715     C*** FFD
106200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
106300030715     C                   EVAL      VABFFD = SkSplitCSV(i)
106400030715     C                   ENDIF
106500030721     C*** GC1
106600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
106700030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
106800030721     C                   ENDIF
106900030721     C*** GC2
107000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
107100030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
107200030721     C                   ENDIF
107300030721     C*** TSP
107400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
107500030721     C                   EVAL      VABTSP = SkSplitCSV(i)
107600030721     C                   ENDIF
107700040510     C*** CBO
107800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
107900040510     C                   EVAL      VABCBO = SkSplitCSV(i)
108000040510     C                   ENDIF
108100030721     C*** NAS
108200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
108300030721     C                   EVAL      VABNAS = SkSplitCSV(i)
108400030721     C                   ENDIF
108500030721     C*** TIC
108600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
108700030721     C                   EVAL      VABTIC = SkSplitCSV(i)
108800030721     C                   ENDIF
108900030721     C*** GCA
109000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
109100030721     C                   EVAL      VABGCA = SkSplitCSV(i)
109200030721     C                   ENDIF
109300030721     C*** XCO
109400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
109500030721     C                   EVAL      VABXCO = SkSplitCSV(i)
109600030721     C                   ENDIF
109700030721     C*** CTM
109800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
109900030721     C                   EVAL      VABCTM = SkSplitCSV(i)
110000030721     C                   ENDIF
110100030721     C*** TCR
110200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
110300030721     C                   EVAL      VABTCR = SkSplitCSV(i)
110400030721     C                   ENDIF
110500030721     C*** CTS
110600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
110700030721     C                   EVAL      VABCTS = SkSplitCSV(i)
110800030721     C                   ENDIF
110900030721     C*** FTM
111000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
111100030721     C                   EVAL      VABFTM = SkSplitCSV(i)
111200030721     C                   ENDIF
111300030721     C*** VAD
111400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
111500030721     C                   EVAL      VABVAD = SkSplitCSV(i)
111600030721     C                   ENDIF
111700030721     C*** GMA
111800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
111900030721     C                   EVAL      VABGMA = SkSplitCSV(i)
112000030721     C                   ENDIF
112100030721     C*** GGA
112200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
112300030721     C                   EVAL      VABGGA = SkSplitCSV(i)
112400030721     C                   ENDIF
112500030721     C*** GVA
112600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
112700030721     C                   EVAL      VABGVA = SkSplitCSV(i)
112800030721     C                   ENDIF
112900030721     C*** TC1
113000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
113100030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
113200030721     C                   ENDIF
113300030721     C*** TC2
113400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
113500030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
113600030721     C                   ENDIF
113700030721     C*** SCL
113800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
113900030721     C                   EVAL      VABSCL = SkSplitCSV(i)
114000030721     C                   ENDIF
114100030721     C*** RMO
114200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
114300030721     C                   EVAL      VABRMO = SkSplitCSV(i)
114400030721     C                   ENDIF
114500030721     C*** NMO
114600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
114700030721     C                   EVAL      VABNMO = SkSplitCSV(i)
114800030721     C                   ENDIF
114900030721     C*
115000030721     C* Reperisco quindi i campi numerici...
115100040510     C*** NSP
115200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
115300040510     C                   EVAL      PiStr=SkSplitCSV(i)
115400040510     C                   EXSR      CHKNUM
115500040510     C                   IF        PiInt=*on
115600040510     C                   Z-ADD     PiVal         VABNSP
115700040510     C                   ELSE
115800040510     C                   SETON                                        31
115900040510     C                   EVAL      VABNSP = *zeros
116000040510     C                   EVAL      vinmsg = %trimr(vinmsg)
116100040510     C                             + ' ' + 'VABNSP'
116200040510     C                   ENDIF
116300040510     C                   ENDIF
116400071106     C*** RMN
116500071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
116600071106     C                   EVAL      PiStr=SkSplitCSV(i)
116700071106     C                   EXSR      CHKNUM
116800071106     C                   IF        PiInt=*on
116900071106     C                   Z-ADD     PiVal         VABRMN
117000071106     C                   ELSE
117100071106     C                   SETON                                        32
117200071106     C                   EVAL      VABRMN = *zeros
117300071106     C                   EVAL      vinmsg = %trimr(vinmsg)
117400071106     C                             + ' ' + 'VABRMN'
117500071106     C                   ENDIF
117600071106     C                   ENDIF
117700030721     C*** CAD
117800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
117900030721     C                   EVAL      PiStr=SkSplitCSV(i)
118000030721     C                   EXSR      CHKNUM
118100030721     C                   IF        PiInt=*on
118200030721     C                   Z-ADD     PiVal         Num5_0
118300030721     C                   MOVEL(p)  Num5_0        VABCAD
118400030721     C                   ELSE
118500030721     C                   SETON                                        32
118600030721     C                   EVAL      VABCAD = *zeros
118700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118800030721     C                             + ' ' + 'VABCAD'
118900030721     C                   ENDIF
119000030721     C                   ENDIF
119100030721     C*** NCL
119200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
119300030721     C                   EVAL      PiStr=SkSplitCSV(i)
119400030721     C                   EXSR      CHKNUM
119500030721     C                   IF        PiInt=*on
119600030721     C                   Z-ADD     PiVal         VABNCL
119700030721     C                   ELSE
119800030721     C                   SETON                                        32
119900030721     C                   EVAL      VABNCL = *zeros
120000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120100040122     C                             + ' ' + 'VABNCL'
120200030721     C                   ENDIF
120300030721     C                   ENDIF
120400030721     C*** PKB
120500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
120600030721     C                   EVAL      PiStr=SkSplitCSV(i)
120700030721     C                   EXSR      CHKNUM
120800030721     C                   IF        PiNum=*on
120900030721     C                   Z-ADD     PiVal         VABPKB
121000030721     C                   ELSE
121100030721     C                   SETON                                        32
121200030721     C                   EVAL      VABPKB = *zeros
121300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121400030721     C                             + ' ' + 'VABPKB'
121500030721     C                   ENDIF
121600030721     C                   ENDIF
121700030721     C*** CAS
121800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
121900110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
122000110506     C                             %trim(SkSplitCSV(i)) <> *zeros
122100101018     C                   EVAL      wFlgCAS = '1'
122200101018     C                   ENDIF
122300030721     C                   EVAL      PiStr=SkSplitCSV(i)
122400030721     C                   EXSR      CHKNUM
122500030721     C                   IF        PiNum=*on
122600030721     C                   Z-ADD     PiVal         VABCAS
122700030721     C                   ELSE
122800030721     C                   SETON                                        32
122900030721     C                   EVAL      VABCAS = *zeros
123000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123100030721     C                             + ' ' + 'VABCAS'
123200030721     C                   ENDIF
123300030721     C                   ENDIF
123400030721     C*** CCM
123500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
123600030721     C                   EVAL      PiStr=SkSplitCSV(i)
123700030721     C                   EXSR      CHKNUM
123800030721     C                   IF        PiInt=*on      AND
123900030721     C                             PiVal<=9999999 AND
124000030721     C                             PiVal>*zeros
124100030721     C                   Z-ADD     PiVal         VABCCM
124200030721     C                   ELSE
124300030721     C                   SETON                                        32
124400030721     C                   EVAL      VABCCM = *zeros
124500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124600040510     C                             + ' ' + 'VABCCM'
124700030721     C                   ENDIF
124800030721     C                   ENDIF
124900030721     C*** LNP
125000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
125100030721     C                   EVAL      PiStr=SkSplitCSV(i)
125200030721     C                   EXSR      CHKNUM
125300030721     C                   IF        PiInt=*on    AND
125400030721     C                             PiVal<=999   AND
125500030721     C                             PiVal>*zeros
125600030721     C                   Z-ADD     PiVal         VABLNP
125700030721     C                   ELSE
125800040715     C                   SETON                                        32
125900030721     C                   EVAL      VABLNP = *zeros
126000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126100040510     C                             + ' ' + 'VABLNP'
126200030721     C                   ENDIF
126300030721     C                   ENDIF
126400030721     C*** NRS
126500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
126600030721     C                   EVAL      PiStr=SkSplitCSV(i)
126700030721     C                   EXSR      CHKNUM
126800030721     C                   IF        PiInt=*on AND
126900030721     C                             PiVal<=99
127000030721     C                   Z-ADD     PiVal         VABNRS
127100030721     C                   ELSE
127200030721     C                   SETON                                        32
127300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127400030721     C                             + ' ' + 'VABNRS'
127500030721     C                   ENDIF
127600030721     C                   ENDIF
127700030721     C*** LNA
127800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
127900030721     C                   EVAL      PiStr=SkSplitCSV(i)
128000030721     C                   EXSR      CHKNUM
128100030721     C                   IF        PiInt=*on    AND
128200030721     C                             PiVal<=999   AND
128300030721     C                             PiVal>*zeros
128400030721     C                   Z-ADD     PiVal         VABLNA
128500030721     C                   ELSE
128600030721     C                   SETON                                        32
128700030721     C                   EVAL      VABLNA = *zeros
128800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128900030721     C                             + ' ' + 'VABLNA'
129000030721     C                   ENDIF
129100030721     C                   ENDIF
129200030721     C*** CTR
129300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
129400030721     C                   EVAL      PiStr=SkSplitCSV(i)
129500030721     C                   EXSR      CHKNUM
129600030721     C                   IF        PiInt=*on  AND
129700030721     C                             PiVal<=999
129800030721     C                   Z-ADD     PiVal         VABCTR
129900030721     C                   ELSE
130000030721     C                   SETON                                        32
130100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130200030721     C                             + ' ' + 'VABCTR'
130300030721     C                   ENDIF
130400030721     C                   ENDIF
130500030721     C*** IAS
130600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
130700030721     C                   EVAL      PiStr=SkSplitCSV(i)
130800030721     C                   EXSR      CHKNUM
130900030721     C                   IF        PiNum=*on
131000030721     C                   Z-ADD     PiVal         VABIAS
131100030721     C                   ELSE
131200030721     C                   SETON                                        32
131300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131400030721     C                             + ' ' + 'VABIAS'
131500030721     C                   ENDIF
131600030721     C                   ENDIF
131700030721     C*** VLB
131800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
131900030721     C                   EVAL      PiStr=SkSplitCSV(i)
132000030721     C                   EXSR      CHKNUM
132100030721     C                   IF        PiNum=*on
132200030721     C                   Z-ADD     PiVal         VABVLB
132300030721     C                   ELSE
132400030721     C                   SETON                                        32
132500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132600030721     C                             + ' ' + 'VABVLB'
132700030721     C                   ENDIF
132800030721     C                   ENDIF
132900030721     C*** QFT
133000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
133100030721     C                   EVAL      PiStr=SkSplitCSV(i)
133200030721     C                   EXSR      CHKNUM
133300030721     C                   IF        PiNum=*on
133400030721     C                   Z-ADD     PiVal         VABQFT
133500030721     C                   ELSE
133600030721     C                   SETON                                        32
133700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133800030721     C                             + ' ' + 'VABQFT'
133900030721     C                   ENDIF
134000030721     C                   ENDIF
134100030721     C*** NCD
134200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
134300030721     C                   EVAL      PiStr=SkSplitCSV(i)
134400030721     C                   EXSR      CHKNUM
134500030721     C                   IF        PiInt=*on      AND
134600030721     C                             PiVal<=9999999
134700030721     C                   Z-ADD     PiVal         VABNCD
134800030721     C                   ELSE
134900030721     C                   SETON                                        32
135000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135100030721     C                             + ' ' + 'VABNCD'
135200030721     C                   ENDIF
135300030721     C                   ENDIF
135400030721     C*** NCA
135500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
135600030721     C                   EVAL      PiStr=SkSplitCSV(i)
135700030721     C                   EXSR      CHKNUM
135800030721     C                   IF        PiInt=*on      AND
135900030721     C                             PiVal<=9999999
136000030721     C                   Z-ADD     PiVal         VABNCA
136100030721     C                   ELSE
136200030721     C                   SETON                                        32
136300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136400030721     C                             + ' ' + 'VABNCA'
136500030721     C                   ENDIF
136600030721     C                   ENDIF
136700030721     C*** ZNC
136800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
136900030721     C                   EVAL      PiStr=SkSplitCSV(i)
137000030721     C                   EXSR      CHKNUM
137100030721     C                   IF        PiInt=*on AND
137200030721     C                             PiVal<=99
137300030721     C                   Z-ADD     PiVal         VABZNC
137400030721     C                   ELSE
137500030721     C                   SETON                                        32
137600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137700030721     C                             + ' ' + 'VABZNC'
137800030721     C                   ENDIF
137900030721     C                   ENDIF
138000030721     C*** DCR
138100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
138200030721     C                   EVAL      PiStr=SkSplitCSV(i)
138300030721     C                   EXSR      CHKNUM
138400030721     C                   IF        PiInt=*on       AND
138500030721     C                             PiVal<=99999999
138600030721     C                   Z-ADD     PiVal         VABDCR
138700030721     C                   ELSE
138800030721     C                   SETON                                        32
138900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139000030721     C                             + ' ' + 'VABDCR'
139100030721     C                   ENDIF
139200030721     C                   ENDIF
139300030721     C*** HCR
139400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
139500030721     C                   EVAL      PiStr=SkSplitCSV(i)
139600030721     C                   EXSR      CHKNUM
139700030721     C                   IF        PiInt=*on   AND
139800030721     C                             PiVal<=9999
139900030721     C                   Z-ADD     PiVal         VABHCR
140000030721     C                   ELSE
140100030721     C                   SETON                                        32
140200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140300030721     C                             + ' ' + 'VABHCR'
140400030721     C                   ENDIF
140500030721     C                   ENDIF
140600030721     C*** VMD
140700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
140800030721     C                   EVAL      PiStr=SkSplitCSV(i)
140900030721     C                   EXSR      CHKNUM
141000030721     C                   IF        PiNum=*on
141100030721     C                   Z-ADD     PiVal         VABVMD
141200030721     C                   ELSE
141300030721     C                   SETON                                        32
141400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141500030721     C                             + ' ' + 'VABVMD'
141600030721     C                   ENDIF
141700030721     C                   ENDIF
141800030721     C*** ANT
141900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
142000030721     C                   EVAL      PiStr=SkSplitCSV(i)
142100030721     C                   EXSR      CHKNUM
142200030721     C                   IF        PiInt=*on        AND
142300030721     C                             PiVal<=999999999
142400030721     C                   Z-ADD     PiVal         VABANT
142500030721     C                   ELSE
142600030721     C                   SETON                                        32
142700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142800030721     C                             + ' ' + 'VABANT'
142900030721     C                   ENDIF
143000030721     C                   ENDIF
143100030721     C*** CMO
143200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
143300030721     C                   EVAL      PiStr=SkSplitCSV(i)
143400030721     C                   EXSR      CHKNUM
143500030721     C                   IF        PiInt=*on
143600030721     C                   Z-ADD     PiVal         Num5_0
143700030721     C                   MOVEL(p)  Num5_0        VABCMO
143800030721     C                   ELSE
143900030721     C                   SETON                                        32
144000030721     C                   EVAL      VABCMO = *zeros
144100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
144200030721     C                             + ' ' + 'VABCMO'
144300030721     C                   ENDIF
144400030721     C                   ENDIF
144500040510     C*
144600040510     C* Reperisco i campi del FIVAT
144700040510     C*** VATNOT
144800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
144900040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
145000040510     C                   ENDIF
145100040510     C*** VATTRC
145200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
145300040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
145400040510     C                   ENDIF
145500060331     C*** VATNOT_A
145600060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
145700060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
145800060331     C                   ENDIF
145900060331     C*** VATNOT_B
146000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
146100060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
146200060331     C                   ENDIF
146300060331     C*** VATNOT_E
146400070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
146500060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
146600060331     C                   ENDIF
146700070823     C*
146800100831     C***  ===>  Gestione campi "particolari" dentro ciclo
146900110421     C*
147000110627     C*** CBO
147100110627     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCBO'
147200110627     C                   SELECT
147300110627     C                   WHEN      SkSplitCSV(i) = 'F'
147400110627     C                   EVAL      VABCBO = '1'
147500110627     C                   WHEN      SkSplitCSV(i) = 'A'
147600110627     C                   EVAL      VABCBO = '2'
147700110627     C                   ENDSL
147800110415     C                   ENDIF
147900070925     C*
148000071029     C***  <===  -----------------------------------------
148100040510     C*
148200030715     C                   ENDIF
148300030715     C                   EVAL      i = i + 1
148400030715     C                   ENDDO
148500070925     C*
148600071029     C***  ===>  Gestione campi "particolari" fuori ciclo
148700100831     C*
148800100831     C                   IF        wVABRSD <> *blanks
148900100831     C                   EVAL      VABRSD = wVABRSD
149000100831     C                   ENDIF
149100100831     C*
149200100831     C                   IF        wVABIND <> *blanks
149300100831     C                   EVAL      VABIND = wVABIND
149400100831     C                   ENDIF
149500100831     C*
149600100831     C                   IF        wVABCAD <> *blanks
149700100831     C                   EVAL      VABCAD = wVABCAD
149800100831     C                   ENDIF
149900100831     C*
150000100831     C                   IF        wVABLOD <> *blanks
150100100831     C                   EVAL      VABLOD = wVABLOD
150200100831     C                   ENDIF
150300100831     C*
150400100831     C                   IF        wVABPRD <> *blanks
150500100831     C                   EVAL      VABPRD = wVABPRD
150600100831     C                   ENDIF
150700070925     C*
150800071029     C***  <===  -----------------------------------------
150900070925     C*
151000070925     C* Considerazioni finali su CBO/CAS
151100100318     C                   IF        wFlgCAS = '1'    OR
151200100318     C                             VABCAS  > *zeros
151300070925     C                   IF        VABCBO = '1'
151400070925     C                   EVAL      VABCBO = '4'
151500070925     C                   ENDIF
151600070925     C                   IF        VABCBO = '2'
151700070925     C                   EVAL      VABCBO = '6'
151800070925     C                   ENDIF
151900070925     C                   ENDIF
152000070925     C***  <===  ----------------------------
152100040510     C*
152200040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
152300040510     C                   IF        VABNSP = *zeros
152400040510     C                   SETON                                        31
152500040510     C                   ENDIF
152600040715     C*
152700040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
152800040715     C                   IF        VABLNP = *zeros
152900040715     C                   SETON                                        31
153000040715     C                   ENDIF
153100040510     C*
153200040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
153300040510     C                   IF        VABRMN = *zeros
153400040510     C                   Z-ADD     VABNSP        VABRMN
153500040510     C                   ENDIF
153600040510     C*
153700040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
153800040510     C                   IF        VABRMA = *blanks
153900040714     C                   MOVEL     VABRMN        VABRMA
154000040510     C                   ENDIF
154100040128     C*
154200040510     C* Se provincia nn valorizzata la reperisco
154300040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
154400040510     C                   IF        VABPRD = *blanks
154500040128     C                   CLEAR                   TISI95DS
154600040128     C                   EVAL      I95TCN = '3'
154700040128     C                   Z-ADD     datcor        I95DAT
154800040128     C                   EVAL      I95NAR = VABNZD
154900040128     C                   EVAL      I95CAP = VABCAD
155000040128     C                   EVAL      I95LOC = VABLOD
155100040128     C                   CALL      'TISI95R'
155200040128     C                   PARM                    TISI95DS
155300040128     C                   EVAL      VABPRD = O95PRV
155400040510     C                   ENDIF
155500020204     C*
155600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
155700020204     C                   EXSR      CHKIMPDIV
155800030715     C                   ENDIF
155900020204     C*
156000010607     C                   ENDSR
156100010607     C*----------------------------------------------------*
156200010601
156300020204
156400020204     C*----------------------------------------------------*
156500020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
156600020204     C*----------------------------------------------------*
156700020204     C     CHKIMPDIV     BEGSR
156800020204     C*
156900020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
157000020204     C                   Z-ADD     *zeros        wrkDec            9 9
157100020204     C*
157200020204     C* Come prima cosa effettuo considerazioni sulla divisa
157300020204     C                   IF        vabIAS > *zeros
157400020204     C                   IF        vabVAS <> 'EUR'
157500020204     C                   EVAL      vabVAS =  'ITL'
157600020204     C                   ENDIF
157700020204     C                   ENDIF
157800020204     C*
157900020204     C                   IF        vabCAS > *zeros
158000020204     C                   IF        vabVCA <> 'EUR'
158100020204     C                   EVAL      vabVCA =  'ITL'
158200020204     C                   ENDIF
158300020204     C                   ENDIF
158400020204     C*
158500020204     C                   IF        vabVMD > *zeros
158600020204     C                   IF        vabVAD <> 'EUR'
158700020204     C                   EVAL      vabVAD =  'ITL'
158800020204     C                   ENDIF
158900020204     C                   ENDIF
159000020204     C*
159100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
159200020204     C                   Z-ADD     vabIAS        wrkDec
159300020204     C                   IF        wrkDec > *zeros
159400020204     C                   IF        vabVAS = 'ITL'
159500020204     C                   EVAL      vabIAS = *zeros
159600020204     C                   ENDIF
159700020204     C                   ENDIF
159800020204     C*
159900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
160000020204     C                   Z-ADD     vabCAS        wrkDec
160100020204     C                   IF        wrkDec > *zeros
160200020204     C                   IF        vabVCA = 'ITL'
160300020204     C                   EVAL      vabCAS = *zeros
160400020204     C                   ENDIF
160500020204     C                   ENDIF
160600020204     C*
160700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
160800020204     C                   Z-ADD     vabVMD        wrkDec
160900020204     C                   IF        wrkDec > *zeros
161000020204     C                   IF        vabVAD = 'ITL'
161100020204     C                   EVAL      vabVMD = *zeros
161200020204     C                   ENDIF
161300020204     C                   ENDIF
161400020204     C*
161500020204     C                   ENDSR
161600020204     C***
161700010330
161800100318
161900100318
162000100318     C*----------------------------------------------------*
162100100318     C*  CONTROLLO NUMERICITA' CAMPI
162200100318     C*----------------------------------------------------*
162300100318     C     CHKNUM        BEGSR
162400100318     C*
162500100318     C                   IF        PiDecChr = *blanks
162600100318     C                   EVAL      PiDecChr = CharNUM
162700100318     C                   ENDIF
162800100318     C*
162900100318     C                   callp     UBISNUM_Check(PiStr
163000100318     C                                          :PiDecChr
163100100318     C                                          :PiVal
163200100318     C                                          :PiNum
163300100318     C                                          :PiInt)
163400100318     C*
163500100318     C                   ENDSR
163600100318     C***
163700010601
163800010601
163900010601
164000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
164100010601     C     repfil        BEGSR
164200010601     C*
164300010601     C                   if        invfil = *zeros and
164400010601     C                             depfil > *zeros and
164500010629     C                             (vinflg = *blanks or
164600010629     C                              vinflg = *zeros)
164700010601     C
164800010601     C                   eval      invfil = depfil
164900010601     C                   endif
165000010601     C*
165100010601     C                   if        depfil <> invfil and
165200010601     C                             invfil > *zeros
165300010601     C                   eval      flgMulti = '1'
165400010601     C                   if        vinflg = *blanks
165500010601     C                   add       1             cntNonEl
165600010601     C                   endif
165700010601     C                   endif
165800010601     C*
165900010601     C                   if        vinflg = '2'
166000010601     C                   eval      flgStato = '2'
166100010601     C                   endif
166200010601     C*
166300010601     C                   ENDSR
166400010601     C***
166500010601
166600010601
166700010601
166800010330
166900040119      /TITLE Invio dei dati al punto operativo.
167000040119     C     invio         BEGSR
167100040119     C*
167200040119     C* 1° invio FIVAT
167300040119     C                   reset                   dscmz
167400040119     C                   move      vlrpoi        cmzdst
167500040119     C                   eval      cmzfld = 'FIVATWWR'
167600040119     C                   eval      cmzmbd = vlrhdl
167700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
167800040119     C***                if        prmfir = *blanks
167900040119     C                   eval      cmzfla = 'FIVAT00F'
168000040119     C                   eval      cmzmba = 'FIVAT00F'
168100040119     C***                else
168200040119     C***                eval      cmzfla = prmfir
168300040119     C***                eval      cmzmba = prmfir
168400040119     C***                endif
168500040119     C                   eval      cmznrr = *zeros
168600040119     C                   move      §ctrokvt      cmznrr
168700040119     C                   eval      cmzlba = vlrfl1
168800040119     C                   call(e)   'TIS711C'
168900040119     C                   parm                    dscmz
169000040119     C                   parm      *blanks       esito
169100040119     C                   if        %error
169200040119     C                             or cmzerr = '1'
169300040119     C                             or esito  = '1'
169400040119     C                   eval      wrkesito = '3'
169500040119     C                   else
169600040119     C*
169700040119     C* 2° invio FIVAB
169800040119     C                   reset                   dscmz
169900040119     C                   move      vlrpoi        cmzdst
170000040119     C                   eval      cmzfld = vlrfou
170100040119     C                   eval      cmzmbd = vlrhdl
170200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
170300040119     C***                if        prmfir = *blanks
170400040119     C                   eval      cmzfla = 'FIVAB00F'
170500040119     C                   eval      cmzmba = 'FIVAB00F'
170600040119     C***                else
170700040119     C***                eval      cmzfla = prmfir
170800040119     C***                eval      cmzmba = prmfir
170900040119     C***                endif
171000040119     C                   eval      cmznrr = *zeros
171100040119     C                   move      §ctrokvb      cmznrr
171200040119     C                   eval      cmzlba = vlrfl1
171300040119     C                   call(e)   'TIS711C'
171400040119     C                   parm                    dscmz
171500040119     C                   parm      *blanks       esito
171600040119     C                   if        %error
171700040119     C                             or cmzerr = '1'
171800040119     C                             or esito  = '1'
171900040119     C                   eval      wrkesito = '3'
172000040119     C                   endif
172100040119     C                   endif
172200040119     C*
172300040119     C                   ENDSR
172400040119     C***
172500010601
172600010601
172700010601
172800010601
172900010601      /TITLE Invio dei dati al punto operativo.
173000010601     C     opeini        BEGSR
173100010601     C*
173200010601     C* Inizializzo flag e contatori operativi
173300010601     C                   movel     '0'           flgGiro           1
173400010601     C                   movel     '0'           flgMulti          1
173500010601     C                   movel     '1'           flgStato          1
173600010615     C                   movel     '0'           flgOk             1
173700010601     C                   z-add     *zeros        cntNonEl         10 0
173800010601     C                   z-add     *zeros        depfil            3 0
173900010601     C                   z-add     *zeros        invfil            3 0
174000010601     C*
174100010601     C                   ENDSR
174200010601     C***
174300070326
174400070326
174500070326
174600070326
174700070326     C     *pssr         BEGSR
174800070329     C*
174900070329     C                   if        %open(tivin00r)
175000070329     C                   close     tivin00r
175100070329     C                   endif
175200070329     C                   if        %open(fivabwwr)
175300070329     C                   close     fivabwwr
175400070329     C                   endif
175500070329     C                   if        %open(fivatwwr)
175600070329     C                   close     fivatwwr
175700070329     C                   endif
175800070326     C*
175900070326     C* Effettuo la chiamata al CLLE preposto
176000070326     C                   call(e)   'TITVVTC'
176100070326     C                   parm                    parccm
176200070326     C                   parm                    parmbr
176300070326     C                   parm      '2'           paropz
176400070326     C*
176500070326     C                   eval      wrkesito = '2'
176600070404     C*
176700070404     C                   seton                                        LR
176800070326     C*
176900070326     C                   ENDSR     '*CANCL'
177000070326     C***
177100100318
177200100318
177300100318
177400100318     C     normCSV       BEGSR
177500100318     C*
177600100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
177700100318     C* separatori campi
177800100318     C                   if        CharTXT <> *blanks
177900100318     C                   z-add     *zeros        wLenVINDTA        4 0
178000100318     C                   z-add     *zeros        wFlgVINDTA        1 0
178100100318     C                   setoff                                       80
178200100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
178300100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
178400100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
178500100318     C                   if        not *in80
178600100318     C                   seton                                        80
178700100318     C                   else
178800100318     C                   setoff                                       80
178900100318     C                   endif
179000100318     C                   endif
179100100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
179200100318     C                             and *in80 = *on
179300100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
179400100318     C                   endif
179500100318     C                   enddo
179600100318     C                   endif
179700100318     C*
179800100318     C                   ENDSR
179900100318     C***
180000070326
180100100326
180200100326
180300100326     C*----------------------------------------------------*
180400100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
180500100326     C*----------------------------------------------------*
180600100326     C     REPNSP        BEGSR
180700100326     C*
180800100326     C* NSP => Stacco un numeratore da AZNUM
180900100326     C                   clear                   TRUL33DS
181000100326     C                   eval      I33OPE = *zeros
181100100326     C                   eval      I33CNU = 302
181200100326     C                   eval      I33NUM = 1
181300100326     C                   movel     TRUL33DS      KPJBU
181400100326     C                   call      'TRUL33R'
181500100326     C                   parm                    KPJBA
181600100326     C                   movel     KPJBU         TRUL33DS
181700100326     C                   if        O33ERR = *zeros
181800100326     C                   z-add     O33NRF        VABNSP
181900100326     C                   z-add     O33NRF        VATNSP
182000100326     C                   else
182100100326     C                   SETON                                        31
182200100326     C                   EVAL      vinmsg = %trimr(vinmsg)
182300100326     C                             + ' ' + 'VABNSP VATNSP'
182400100326     C                   endif
182500100326     C*
182600100326     C                   ENDSR
182700010330
182800100326
182900010330
183000000613     C     *inzsr        BEGSR
183100990910     C*
183200990910     C     *entry        plist
183300990920     C                   parm                    tivlrds
183400990921     C                   parm      wrkesito      esito
183500000724     C                   parm                    prmlit
183600000710     C                   parm                    prmfir
183700010330     C*
183800010330     C* CALCOLA LA DATA CORRENTE
183900100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
184000100318     C                   eval      datcor = %dec(%date() : *ISO)
184100000613     C*
184200000613     C                   ENDSR
184300000613     C***
184400990908
184500110627     Otitv2g4p  E            riepilogo         2
184600990915     O                                              'Upload via Internet'
184700990915     O                                           +1 'Traduzione TIVIN00R -'
184800021025     O                                           +1 'FIVABWWR'
184900990915     O                                           +1 'Testate bolle'
185000990915     O          E            riepilogo   2
185100990915     O                       wrkdata
185200990915     O                       wrkora              +1
185300990915     O                       procname            +1
185400990915     O          E            riepilogo   2
185500990915     O                                              'Cliente..................:'
185600010605     O                       VABCCM        z     +1
185700990915     O          E            riepilogo   2
185800990920     O                                              'Riferimento Strategi.....:'
185900990920     O                       vlrhdl              +1
186000990915     O          E            riepilogo   2
186100990915     O                                              'Giusti...................:'
186200040119     O                       §CTROKVB      2   +  1
186300990915     O          E            riepilogo   2
186400990915     O                                              'Sbagliati e corretti.....:'
186500971022     O                       §CTRMO        2   +  1
186600990915     O          E            riepilogo   2
186700990915     O                                              'Sbagliati e scartati.....:'
186800971022     O                       §CTRNO        2   +  1
186900000613
187000110627     Otitv2g4ps E            testdett          2
187100000613     O                                              'Upload via Internet'
187200000613     O                                           +1 'Traduzione TIVIN00R -'
187300021025     O                                           +1 'FIVABWWR'
187400000613     O                                           +1 'Testate bolle'
187500000616     O          E            testdett    3
187600000613     O                                           +2 'N° rec'
187700000613     O                                           +3 'Anteprima contenuto'
187800000616     O          E            rigadett    2
187900000613     O                       rrnum               +2
188000000621     O                       recko               +3
188100000616     O          E            findett     2
188200000613     O                       wrkdata
188300000613     O                       wrkora              +1
188400000613     O                       procname            +1
188500000616     O          E            findett     2
188600000613     O                                              'Cliente..................:'
188700010605     O                       VABCCM        z     +1
188800000616     O          E            findett     2
188900000613     O                                              'Riferimento Strategi.....:'
189000000613     O                       vlrhdl              +1
189100000616     O          E            findett     2
189200000613     O                                              'Giusti...................:'
189300040119     O                       §CTROKVB      2   +  1
189400000616     O          E            findett     2
189500000613     O                                              'Sbagliati e corretti.....:'
189600000613     O                       §CTRMO        2   +  1
189700000616     O          E            findett     2
189800000613     O                                              'Sbagliati e scartati.....:'
189900000613     O                       §CTRNO        2   +  1
190000000613** CMD - COMANDI CL
190100110627OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV2G4P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
190200110627OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV2G4PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
190300110627DLTOVR FILE(TITV2G4P TITV2G4PS) LVL(*)
190400010606
190500000613
190600040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
190700110707VABRSD;VABIND;VABLOD;VABCAD;VABPRD;VABRMN;FILXXX;VABNCL;FILXXX;VABPKB;
190800110707FILXXX;VABNOT;W_VABCBO;FILXXX;FILXXX;FILXXX;
190900110315
191000110315
191100100318
