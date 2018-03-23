000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200100318     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
000700040119     FFIVATwwr  O    E             DISK    usropn
000800110406     Ftitv2d9p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070329     F                                     infsr(*pssr)
001100110406     Ftitv2d9ps O    f  198        PRINTER usropn
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
015500110406     C                   if        not %open(titv2d9ps)
015600110406     C                   open      titv2d9ps
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
018700110406     C                   if        %open(titv2d9ps)
018800000616     C                   except    findett
018900110406     C                   close     titv2d9ps
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
020900110406     C                   if        not %open(titv2d9p)
021000110406     C   50              open      titv2d9p
021100990915     C                   endif
021200990915     C*
021300030715     C   50              except    riepilogo
021400990915     C*
021500110406     C                   if        %open(titv2d9p)
021600110406     C   50              close     titv2d9p
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
025200110406     C                   exsr      inzvar
025300110406     C                   exsr      impvab
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
054900110406     C                   MOVEL     *blanks       wVABLOD_1        35
055000110406     C                   MOVEL     *blanks       wVABLOD_2        35
055100030822     C*
055200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
055300030822     C                   CLEAR                   SkSplitCSV
055400030822     C                   EVAL      i = 1
055500030822     C                   EVAL      posDa = *zeros
055600030822     C                   EVAL      posA  = *zeros
055700080527     C*
055800080527     C                   CLEAR                   FIVABDS
055900080527     C                   CLEAR                   FIVAT000
056000030822     C*
056100030822     C* Reimposto i valori di default
056200030822     C                   EXSR      DEFCAM
056300030822     C*
056400010330     C                   ENDSR
056500010330     C*----------------------------------------------------*
056600030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
056700010330     C*----------------------------------------------------*
056800010330     C     DEFCAM        BEGSR
056900030822     C*
057000030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
057100030715     C* e delimitatore testo.
057200030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
057300030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
057400030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
057500100318     C*
057600100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
057700100318     C                   EVAL      CharSOS = CharNUM
057800030822     C*
057900030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
058000100720     C                   SETOFF                                       505152
058100100720     C                   SETOFF                                       53
058200100413     C                   SELECT
058300100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
058400100720     C                   SETON                                        50
058500100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
058600100413     C                   SETON                                        51
058700100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
058800100720     C                   SETON                                        52
058900100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
059000100720     C                   SETON                                        53
059100100413     C                   ENDSL
059200030822     C*
059300030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
059400030822     C                   EVAL      posDaDft = 1
059500030822     C                   EVAL      posADft  = 0
059600030822     C                   EVAL      wGiroDft = 0
059700030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
059800030822     C                             posDaDft > 0
059900030822     C*
060000030822     C* Gestisco il 1° giro
060100030822     C                   IF        wGiroDft = 0
060200030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
060300030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
060400030822     C* Incremento il contatore dei "giri"
060500030822     C                   EVAL      wGiroDft = 1
060600030822     C                   ELSE
060700030822     C                   EVAL      posDaDft = posADft
060800030822     C                   ENDIF
060900030822     C* Eseguo lo scan x trovare la fine del campo corrente
061000030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
061100030822     C*
061200030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
061300030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
061400030822     C                   IF        posDaDft > 0 AND
061500030822     C                             posADft  > 0
061600030822     C* NCL
061700030822     C                   IF        %subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):1:3)
062000030822     C                             = 'NCL'
062100030822     C                   EVAL      PiStr=%trim(%subst(
062200030822     C                             %subst(vlrppt:posDaDft+1:
062300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062400030822     C                   EXSR      CHKNUM
062500030822     C                   IF        PiInt=*on
062600030822     C                   Z-ADD     PiVal         VABNCL
062700030822     C                   ENDIF
062800030822     C                   ENDIF
062900030822     C* CCM
063000030822     C                   IF        %subst(
063100030822     C                             %subst(vlrppt:posDaDft+1:
063200030822     C                             posADft-posDaDft-1):1:3)
063300030822     C                             = 'CCM'
063400030822     C                   EVAL      PiStr=%trim(%subst(
063500030822     C                             %subst(vlrppt:posDaDft+1:
063600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063700030822     C                   EXSR      CHKNUM
063800030822     C                   IF        PiInt=*on
063900030822     C                   Z-ADD     PiVal         VABCCM
064000030822     C                   ENDIF
064100030822     C                   ENDIF
064200030822     C* LNP
064300030822     C                   IF        %subst(
064400030822     C                             %subst(vlrppt:posDaDft+1:
064500030822     C                             posADft-posDaDft-1):1:3)
064600030822     C                             = 'LNP'
064700030822     C                   EVAL      PiStr=%trim(%subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065000030822     C                   EXSR      CHKNUM
065100030822     C                   IF        PiInt=*on
065200030822     C                   Z-ADD     PiVal         VABLNP
065300030822     C                   ENDIF
065400030822     C                   ENDIF
065500030822     C* NRS
065600030822     C                   IF        %subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):1:3)
065900030822     C                             = 'NRS'
066000030822     C                   EVAL      PiStr=%trim(%subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066300030822     C                   EXSR      CHKNUM
066400030822     C                   IF        PiInt=*on
066500030822     C                   Z-ADD     PiVal         VABNRS
066600030822     C                   ENDIF
066700030822     C                   ENDIF
066800030822     C* CTR
066900030822     C                   IF        %subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):1:3)
067200030822     C                             = 'CTR'
067300030822     C                   EVAL      PiStr=%trim(%subst(
067400030822     C                             %subst(vlrppt:posDaDft+1:
067500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067600030822     C                   EXSR      CHKNUM
067700030822     C                   IF        PiInt=*on
067800030822     C                   Z-ADD     PiVal         VABCTR
067900030822     C                   ENDIF
068000030822     C                   ENDIF
068100030822     C* PKB
068200030822     C                   IF        %subst(
068300030822     C                             %subst(vlrppt:posDaDft+1:
068400030822     C                             posADft-posDaDft-1):1:3)
068500030822     C                             = 'PKB'
068600030822     C                   EVAL      PiStr=%trim(%subst(
068700030822     C                             %subst(vlrppt:posDaDft+1:
068800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068900030822     C                   EXSR      CHKNUM
069000030822     C                   IF        PiNum=*on
069100030822     C                   Z-ADD     PiVal         VABPKB
069200030822     C                   ENDIF
069300030822     C                   ENDIF
069400030822     C* VLB
069500030822     C                   IF        %subst(
069600030822     C                             %subst(vlrppt:posDaDft+1:
069700030822     C                             posADft-posDaDft-1):1:3)
069800030822     C                             = 'VLB'
069900030822     C                   EVAL      PiStr=%trim(%subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070200030822     C                   EXSR      CHKNUM
070300030822     C                   IF        PiNum=*on
070400030822     C                   Z-ADD     PiVal         VABVLB
070500030822     C                   ENDIF
070600030822     C                   ENDIF
070700030822     C* QFT
070800030822     C                   IF        %subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):1:3)
071100030822     C                             = 'QFT'
071200030822     C                   EVAL      PiStr=%trim(%subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071500030822     C                   EXSR      CHKNUM
071600030822     C                   IF        PiNum=*on
071700030822     C                   Z-ADD     PiVal         VABQFT
071800030822     C                   ENDIF
071900030822     C                   ENDIF
072000030822     C* CBO
072100030822     C                   IF        %subst(
072200030822     C                             %subst(vlrppt:posDaDft+1:
072300030822     C                             posADft-posDaDft-1):1:3)
072400030822     C                             = 'CBO'
072500030822     C                   EVAL      VABCBO=%trim(%subst(
072600030822     C                             %subst(vlrppt:posDaDft+1:
072700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072800030822     C                   ENDIF
072900030822     C* TSP
073000030822     C                   IF        %subst(
073100030822     C                             %subst(vlrppt:posDaDft+1:
073200030822     C                             posADft-posDaDft-1):1:3)
073300030822     C                             = 'TSP'
073400030822     C                   EVAL      VABTSP=%trim(%subst(
073500030822     C                             %subst(vlrppt:posDaDft+1:
073600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073700030822     C                   ENDIF
073800030822     C* VAS
073900030822     C                   IF        %subst(
074000030822     C                             %subst(vlrppt:posDaDft+1:
074100030822     C                             posADft-posDaDft-1):1:3)
074200030822     C                             = 'VAS'
074300030822     C                   EVAL      VABVAS=%trim(%subst(
074400030822     C                             %subst(vlrppt:posDaDft+1:
074500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074600030822     C                   ENDIF
074700030822     C* VCA
074800030822     C                   IF        %subst(
074900030822     C                             %subst(vlrppt:posDaDft+1:
075000030822     C                             posADft-posDaDft-1):1:3)
075100030822     C                             = 'VCA'
075200030822     C                   EVAL      VABVCA=%trim(%subst(
075300030822     C                             %subst(vlrppt:posDaDft+1:
075400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075500030822     C                   ENDIF
075600070925     C* TIC
075700070925     C                   IF        %subst(
075800070925     C                             %subst(vlrppt:posDaDft+1:
075900070925     C                             posADft-posDaDft-1):1:3)
076000070925     C                             = 'TIC'
076100070925     C                   EVAL      VABTIC=%trim(%subst(
076200070925     C                             %subst(vlrppt:posDaDft+1:
076300070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076400070925     C                   ENDIF
076500030822     C* GCA
076600030822     C                   IF        %subst(
076700030822     C                             %subst(vlrppt:posDaDft+1:
076800030822     C                             posADft-posDaDft-1):1:3)
076900030822     C                             = 'GCA'
077000030822     C                   EVAL      VABGCA=%trim(%subst(
077100030822     C                             %subst(vlrppt:posDaDft+1:
077200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077300030822     C                   ENDIF
077400030822     C* CTM
077500030822     C                   IF        %subst(
077600030822     C                             %subst(vlrppt:posDaDft+1:
077700030822     C                             posADft-posDaDft-1):1:3)
077800030822     C                             = 'CTM'
077900030822     C                   EVAL      VABCTM=%trim(%subst(
078000030822     C                             %subst(vlrppt:posDaDft+1:
078100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078200030822     C                   ENDIF
078300030822     C* FFD
078400030822     C                   IF        %subst(
078500030822     C                             %subst(vlrppt:posDaDft+1:
078600030822     C                             posADft-posDaDft-1):1:3)
078700030822     C                             = 'FFD'
078800030822     C                   EVAL      VABFFD=%trim(%subst(
078900030822     C                             %subst(vlrppt:posDaDft+1:
079000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079100030822     C                   ENDIF
079200030822     C* VAD
079300030822     C                   IF        %subst(
079400030822     C                             %subst(vlrppt:posDaDft+1:
079500030822     C                             posADft-posDaDft-1):1:3)
079600030822     C                             = 'VAD'
079700030822     C                   EVAL      VABVAD=%trim(%subst(
079800030822     C                             %subst(vlrppt:posDaDft+1:
079900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080000030822     C                   ENDIF
080100030822     C* GMA
080200030822     C                   IF        %subst(
080300030822     C                             %subst(vlrppt:posDaDft+1:
080400030822     C                             posADft-posDaDft-1):1:3)
080500030822     C                             = 'GMA'
080600030822     C                   EVAL      VABGMA=%trim(%subst(
080700030822     C                             %subst(vlrppt:posDaDft+1:
080800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080900030822     C                   ENDIF
081000030822     C* GGA
081100030822     C                   IF        %subst(
081200030822     C                             %subst(vlrppt:posDaDft+1:
081300030822     C                             posADft-posDaDft-1):1:3)
081400030822     C                             = 'GGA'
081500030822     C                   EVAL      VABGGA=%trim(%subst(
081600030822     C                             %subst(vlrppt:posDaDft+1:
081700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081800030822     C                   ENDIF
081900030822     C* GVA
082000030822     C                   IF        %subst(
082100030822     C                             %subst(vlrppt:posDaDft+1:
082200030822     C                             posADft-posDaDft-1):1:3)
082300030822     C                             = 'GVA'
082400030822     C                   EVAL      VABGVA=%trim(%subst(
082500030822     C                             %subst(vlrppt:posDaDft+1:
082600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082700030822     C                   ENDIF
082800030822     C* TC1
082900030822     C                   IF        %subst(
083000030822     C                             %subst(vlrppt:posDaDft+1:
083100030822     C                             posADft-posDaDft-1):1:3)
083200030822     C                             = 'TC1'
083300030822     C                   EVAL      VABTC1=%trim(%subst(
083400030822     C                             %subst(vlrppt:posDaDft+1:
083500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083600030822     C                   ENDIF
083700030822     C* TC2
083800030822     C                   IF        %subst(
083900030822     C                             %subst(vlrppt:posDaDft+1:
084000030822     C                             posADft-posDaDft-1):1:3)
084100030822     C                             = 'TC2'
084200030822     C                   EVAL      VABTC2=%trim(%subst(
084300030822     C                             %subst(vlrppt:posDaDft+1:
084400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084500030822     C                   ENDIF
084600040714     C* VATTRC
084700040714     C                   IF        %subst(
084800040714     C                             %subst(vlrppt:posDaDft+1:
084900040714     C                             posADft-posDaDft-1):1:3)
085000040714     C                             = 'TRC'
085100040714     C                   EVAL      VATTRC=%trim(%subst(
085200040714     C                             %subst(vlrppt:posDaDft+1:
085300040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085400040714     C                   ENDIF
085500030822     C* ...
085600030822     C                   ENDIF
085700030822     C                   ENDDO
085800020204     C*
085900010330     C                   ENDSR
086000010607     C*----------------------------------------------------*
086100021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
086200010607     C*----------------------------------------------------*
086300010607     C     IMPVAB        BEGSR
086400010607     C*
086500010607     C                   SETOFF                                       3132
086600110315 xxx C***                SETON                                        35
086700030515     C*
086800010607     C                   MOVEL     datcor        VABAAS
086900010607     C                   MOVE      datcor        VABMGS
087000010607     C*
087100030715     C**********
087200030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
087300030715     C**********
087400030715     C                   IF        wGiro = *zeros
087500040113     C*
087600040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
087700040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
087800080429     C                             %trim(intcol(3))+%trim(intcol(4))+
087900080429     C                             %trim(intcol(5))<>*blanks
088000040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
088100080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
088200080429     C                                      %trim(intcol(5))
088300070823     C     ';':CharCSV   XLATE     vindta        vindta
088400040510     C                   ENDIF
088500031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
088600031201     C     minu:maiu     XLATE     vindta        vindta
088700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088800030715     C                   DOW       posDa <= %len(%trim(vindta))
088900030715     C*
089000030715     C* Gestisco il 1° campo
089100030715     C                   IF        i = 1
089200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
089300030715     C                   EVAL      posDa = 1
089400030715     C* Eseguo lo scan x trovare la fine del primo campo
089500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089600030715     C* A questo punto "estraggo" il campo corrente
089700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089800030715     C                                                    (posA-posDa))
089900030715     C* X i campi successivi al 1°
090000030715     C                   ELSE
090100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
090200030715     C                   EVAL      posDa = posA + 1
090300030715     C* Eseguo lo scan x trovare la fine del campo corrente
090400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090500030715     C* Gestisco l'ultimo campo
090600030715     C                   IF        posA = *zeros
090700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090800030715     C* A questo punto "estraggo" il campo corrente
090900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
091000030715     C                                                    (posA-posDa))
091100030715     C                   LEAVE
091200030715     C                   ELSE
091300030715     C* A questo punto "estraggo" il campo corrente
091400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
091500030715     C                                                    (posA-posDa))
091600030715     C                   ENDIF
091700030715     C                   ENDIF
091800030715     C* Incremento il contatore d campo
091900030715     C                   EVAL      i = i +1
092000030715     C                   ENDDO
092100030715     C*
092200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092300030715     C* delimitazione testo
092400030715     C                   EVAL      i = 1
092500030715     C                   DOW       i < %elem(SkSplitFLD)
092600030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
092700030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
092800030715     C                   EVAL      i = i + 1
092900030715     C                   ENDDO
093000030715     C                   EVAL      wGiro = 1
093100030715     C                   ELSE
093200030715     C                   EVAL      wGiro = 2
093300031204     C**********
093400031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
093500031204     C* d separatore campo
093600031204     C**********
093700031204     C                   EVAL      vindta = %trim(vindta)
093800031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
093900031204     C                   EVAL      vindta = %subst(vindta:2)
094000031204     C                   ENDDO
094100031204     C*
094200031204     C                   Z-ADD     *zeros        lunghInput        4 0
094300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
094400031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
094500031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
094600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
094700031204     C                   ENDDO
094800030715     C**********
094900030715     C* Effettuo lo split del campo dati d input
095000030715     C**********
095100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
095200030715     C                   DOW       posDa <= %len(%trim(vindta))
095300030715     C*
095400030715     C* Gestisco il 1° campo
095500030715     C                   IF        i = 1
095600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
095700030715     C                   EVAL      posDa = 1
095800030715     C* Eseguo lo scan x trovare la fine del primo campo
095900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
096000030715     C* A questo punto "estraggo" il campo corrente
096100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096200030715     C                                                    (posA-posDa))
096300030715     C* X i campi successivi al 1°
096400030715     C                   ELSE
096500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
096600030715     C                   EVAL      posDa = posA + 1
096700031204     C* Verifico che nn vi sia il campo nn valorizzato
096800031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
096900031204     C* Se campo nn valorizzato skippo al prossimo
097000031204     C                   EVAL      SkSplitCSV(i) = *blanks
097100031204     C                   EVAL      posA  = posA + 1
097200031204     C                   ELSE
097300030715     C* Eseguo lo scan x trovare la fine del campo corrente
097400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
097500030715     C* Gestisco l'ultimo campo
097600030715     C                   IF        posA = *zeros
097700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
097800030715     C* A questo punto "estraggo" il campo corrente
097900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
098000030715     C                                                    (posA-posDa))
098100030715     C                   LEAVE
098200030715     C                   ELSE
098300030715     C* A questo punto "estraggo" il campo corrente
098400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
098500030715     C                                                    (posA-posDa))
098600030715     C                   ENDIF
098700031204     C                   ENDIF
098800030715     C                   ENDIF
098900030715     C* Incremento il contatore d campo
099000030715     C                   EVAL      i = i +1
099100030715     C                   ENDDO
099200030715     C*
099300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
099400030715     C* delimitazione testo
099500030715     C                   EVAL      i = 1
099600030715     C                   DOW       i < %elem(SkSplitCSV)
099700030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
099800030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
099900030715     C                   EVAL      i = i + 1
100000030715     C                   ENDDO
100100100720     C*
100200100720     C* Stacco un progressivo x numero spedizione
100300100720 xxx C***                EXSR      repNSP
100400100326     C*
100500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
100600030715     C                   EVAL      i = 1
100700030715     C                   DOW       i < %elem(SkSplitFLD)
100800030715     C*
100900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
101000030715     C                   IF        SkSplitFLD(i) <> *blanks AND
101100030715     C                             SkSplitCSV(i) <> *blanks
101200030721     C*** ATB
101300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
101400030721     C                   EVAL      VABATB = SkSplitCSV(i)
101500030721     C                   ENDIF
101600030715     C*** RSD
101700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
101800030715     C                   EVAL      VABRSD = SkSplitCSV(i)
101900030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
102000030715     C     '@':'A'       XLATE     VABRSD        VABRSD
102100030715     C* ==
102200030715     C                   ENDIF
102300030721     C*** RD2
102400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
102500030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
102600030721     C                   ENDIF
102700030715     C*** IND
102800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
102900030715     C                   EVAL      VABIND = SkSplitCSV(i)
103000030715     C                   ENDIF
103100030715     C*** LOD
103200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
103300030715     C                   EVAL      VABLOD = SkSplitCSV(i)
103400030715     C                   ENDIF
103500030715     C*** PRD
103600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
103700030715     C                   EVAL      VABPRD = SkSplitCSV(i)
103800030715     C                   ENDIF
103900030721     C*** NZD
104000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
104100030721     C                   EVAL      VABNZD = SkSplitCSV(i)
104200030721     C                   ENDIF
104300030715     C*** RMA
104400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
104500030715     C                   EVAL      VABRMA = SkSplitCSV(i)
104600030715     C                   ENDIF
104700030715     C*** NOT
104800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
104900030715     C                   EVAL      VABNOT = SkSplitCSV(i)
105000030715     C                   ENDIF
105100030715     C*** NT2
105200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
105300030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
105400030715     C                   ENDIF
105500030715     C*** VCA
105600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
105700030715     C                   EVAL      VABVCA = SkSplitCSV(i)
105800030715     C                   ENDIF
105900030715     C*** VAS
106000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
106100030715     C                   EVAL      VABVAS = SkSplitCSV(i)
106200030715     C                   ENDIF
106300030715     C*** FFD
106400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
106500030715     C                   EVAL      VABFFD = SkSplitCSV(i)
106600030715     C                   ENDIF
106700030721     C*** GC1
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
106900030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
107000030721     C                   ENDIF
107100030721     C*** GC2
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
107300030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
107400030721     C                   ENDIF
107500030721     C*** TSP
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
107700030721     C                   EVAL      VABTSP = SkSplitCSV(i)
107800030721     C                   ENDIF
107900040510     C*** CBO
108000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
108100040510     C                   EVAL      VABCBO = SkSplitCSV(i)
108200040510     C                   ENDIF
108300030721     C*** NAS
108400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
108500030721     C                   EVAL      VABNAS = SkSplitCSV(i)
108600030721     C                   ENDIF
108700030721     C*** TIC
108800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
108900030721     C                   EVAL      VABTIC = SkSplitCSV(i)
109000030721     C                   ENDIF
109100030721     C*** GCA
109200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
109300030721     C                   EVAL      VABGCA = SkSplitCSV(i)
109400030721     C                   ENDIF
109500030721     C*** XCO
109600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
109700030721     C                   EVAL      VABXCO = SkSplitCSV(i)
109800030721     C                   ENDIF
109900030721     C*** CTM
110000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
110100030721     C                   EVAL      VABCTM = SkSplitCSV(i)
110200030721     C                   ENDIF
110300030721     C*** TCR
110400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
110500030721     C                   EVAL      VABTCR = SkSplitCSV(i)
110600030721     C                   ENDIF
110700030721     C*** CTS
110800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
110900030721     C                   EVAL      VABCTS = SkSplitCSV(i)
111000030721     C                   ENDIF
111100030721     C*** FTM
111200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
111300030721     C                   EVAL      VABFTM = SkSplitCSV(i)
111400030721     C                   ENDIF
111500030721     C*** VAD
111600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
111700030721     C                   EVAL      VABVAD = SkSplitCSV(i)
111800030721     C                   ENDIF
111900030721     C*** GMA
112000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
112100030721     C                   EVAL      VABGMA = SkSplitCSV(i)
112200030721     C                   ENDIF
112300030721     C*** GGA
112400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
112500030721     C                   EVAL      VABGGA = SkSplitCSV(i)
112600030721     C                   ENDIF
112700030721     C*** GVA
112800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
112900030721     C                   EVAL      VABGVA = SkSplitCSV(i)
113000030721     C                   ENDIF
113100030721     C*** TC1
113200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
113300030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
113400030721     C                   ENDIF
113500030721     C*** TC2
113600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
113700030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
113800030721     C                   ENDIF
113900030721     C*** SCL
114000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
114100030721     C                   EVAL      VABSCL = SkSplitCSV(i)
114200030721     C                   ENDIF
114300030721     C*** RMO
114400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
114500030721     C                   EVAL      VABRMO = SkSplitCSV(i)
114600030721     C                   ENDIF
114700030721     C*** NMO
114800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
114900030721     C                   EVAL      VABNMO = SkSplitCSV(i)
115000030721     C                   ENDIF
115100030721     C*
115200030721     C* Reperisco quindi i campi numerici...
115300040510     C*** NSP
115400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
115500040510     C                   EVAL      PiStr=SkSplitCSV(i)
115600040510     C                   EXSR      CHKNUM
115700040510     C                   IF        PiInt=*on
115800040510     C                   Z-ADD     PiVal         VABNSP
115900040510     C                   ELSE
116000040510     C                   SETON                                        31
116100040510     C                   EVAL      VABNSP = *zeros
116200040510     C                   EVAL      vinmsg = %trimr(vinmsg)
116300040510     C                             + ' ' + 'VABNSP'
116400040510     C                   ENDIF
116500040510     C                   ENDIF
116600071106     C*** RMN
116700071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
116800071106     C                   EVAL      PiStr=SkSplitCSV(i)
116900071106     C                   EXSR      CHKNUM
117000071106     C                   IF        PiInt=*on
117100071106     C                   Z-ADD     PiVal         VABRMN
117200071106     C                   ELSE
117300071106     C                   SETON                                        32
117400071106     C                   EVAL      VABRMN = *zeros
117500071106     C                   EVAL      vinmsg = %trimr(vinmsg)
117600071106     C                             + ' ' + 'VABRMN'
117700071106     C                   ENDIF
117800071106     C                   ENDIF
117900030721     C*** CAD
118000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
118100030721     C                   EVAL      PiStr=SkSplitCSV(i)
118200030721     C                   EXSR      CHKNUM
118300030721     C                   IF        PiInt=*on
118400030721     C                   Z-ADD     PiVal         Num5_0
118500030721     C                   MOVEL(p)  Num5_0        VABCAD
118600030721     C                   ELSE
118700030721     C                   SETON                                        32
118800030721     C                   EVAL      VABCAD = *zeros
118900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119000030721     C                             + ' ' + 'VABCAD'
119100030721     C                   ENDIF
119200030721     C                   ENDIF
119300030721     C*** NCL
119400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
119500030721     C                   EVAL      PiStr=SkSplitCSV(i)
119600030721     C                   EXSR      CHKNUM
119700030721     C                   IF        PiInt=*on
119800030721     C                   Z-ADD     PiVal         VABNCL
119900030721     C                   ELSE
120000030721     C                   SETON                                        32
120100030721     C                   EVAL      VABNCL = *zeros
120200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120300040122     C                             + ' ' + 'VABNCL'
120400030721     C                   ENDIF
120500030721     C                   ENDIF
120600030721     C*** PKB
120700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
120800030721     C                   EVAL      PiStr=SkSplitCSV(i)
120900030721     C                   EXSR      CHKNUM
121000030721     C                   IF        PiNum=*on
121100030721     C                   Z-ADD     PiVal         VABPKB
121200030721     C                   ELSE
121300030721     C                   SETON                                        32
121400030721     C                   EVAL      VABPKB = *zeros
121500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121600030721     C                             + ' ' + 'VABPKB'
121700030721     C                   ENDIF
121800030721     C                   ENDIF
121900030721     C*** CAS
122000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
122100101018     C                   IF        SkSplitCSV(i) <> *blanks AND
122200101018     C                             SkSplitCSV(i) <> *zeros
122300101018     C                   EVAL      wFlgCAS = '1'
122400101018     C                   ENDIF
122500030721     C                   EVAL      PiStr=SkSplitCSV(i)
122600030721     C                   EXSR      CHKNUM
122700030721     C                   IF        PiNum=*on
122800030721     C                   Z-ADD     PiVal         VABCAS
122900030721     C                   ELSE
123000030721     C                   SETON                                        32
123100030721     C                   EVAL      VABCAS = *zeros
123200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123300030721     C                             + ' ' + 'VABCAS'
123400030721     C                   ENDIF
123500030721     C                   ENDIF
123600030721     C*** CCM
123700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
123800030721     C                   EVAL      PiStr=SkSplitCSV(i)
123900030721     C                   EXSR      CHKNUM
124000030721     C                   IF        PiInt=*on      AND
124100030721     C                             PiVal<=9999999 AND
124200030721     C                             PiVal>*zeros
124300030721     C                   Z-ADD     PiVal         VABCCM
124400030721     C                   ELSE
124500030721     C                   SETON                                        32
124600030721     C                   EVAL      VABCCM = *zeros
124700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124800040510     C                             + ' ' + 'VABCCM'
124900030721     C                   ENDIF
125000030721     C                   ENDIF
125100030721     C*** LNP
125200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
125300030721     C                   EVAL      PiStr=SkSplitCSV(i)
125400030721     C                   EXSR      CHKNUM
125500030721     C                   IF        PiInt=*on    AND
125600030721     C                             PiVal<=999   AND
125700030721     C                             PiVal>*zeros
125800030721     C                   Z-ADD     PiVal         VABLNP
125900030721     C                   ELSE
126000040715     C                   SETON                                        32
126100030721     C                   EVAL      VABLNP = *zeros
126200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126300040510     C                             + ' ' + 'VABLNP'
126400030721     C                   ENDIF
126500030721     C                   ENDIF
126600030721     C*** NRS
126700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
126800030721     C                   EVAL      PiStr=SkSplitCSV(i)
126900030721     C                   EXSR      CHKNUM
127000030721     C                   IF        PiInt=*on AND
127100030721     C                             PiVal<=99
127200030721     C                   Z-ADD     PiVal         VABNRS
127300030721     C                   ELSE
127400030721     C                   SETON                                        32
127500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127600030721     C                             + ' ' + 'VABNRS'
127700030721     C                   ENDIF
127800030721     C                   ENDIF
127900030721     C*** LNA
128000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
128100030721     C                   EVAL      PiStr=SkSplitCSV(i)
128200030721     C                   EXSR      CHKNUM
128300030721     C                   IF        PiInt=*on    AND
128400030721     C                             PiVal<=999   AND
128500030721     C                             PiVal>*zeros
128600030721     C                   Z-ADD     PiVal         VABLNA
128700030721     C                   ELSE
128800030721     C                   SETON                                        32
128900030721     C                   EVAL      VABLNA = *zeros
129000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129100030721     C                             + ' ' + 'VABLNA'
129200030721     C                   ENDIF
129300030721     C                   ENDIF
129400030721     C*** CTR
129500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
129600030721     C                   EVAL      PiStr=SkSplitCSV(i)
129700030721     C                   EXSR      CHKNUM
129800030721     C                   IF        PiInt=*on  AND
129900030721     C                             PiVal<=999
130000030721     C                   Z-ADD     PiVal         VABCTR
130100030721     C                   ELSE
130200030721     C                   SETON                                        32
130300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130400030721     C                             + ' ' + 'VABCTR'
130500030721     C                   ENDIF
130600030721     C                   ENDIF
130700030721     C*** IAS
130800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
130900030721     C                   EVAL      PiStr=SkSplitCSV(i)
131000030721     C                   EXSR      CHKNUM
131100030721     C                   IF        PiNum=*on
131200030721     C                   Z-ADD     PiVal         VABIAS
131300030721     C                   ELSE
131400030721     C                   SETON                                        32
131500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131600030721     C                             + ' ' + 'VABIAS'
131700030721     C                   ENDIF
131800030721     C                   ENDIF
131900030721     C*** VLB
132000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
132100030721     C                   EVAL      PiStr=SkSplitCSV(i)
132200030721     C                   EXSR      CHKNUM
132300030721     C                   IF        PiNum=*on
132400030721     C                   Z-ADD     PiVal         VABVLB
132500030721     C                   ELSE
132600030721     C                   SETON                                        32
132700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132800030721     C                             + ' ' + 'VABVLB'
132900030721     C                   ENDIF
133000030721     C                   ENDIF
133100030721     C*** QFT
133200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
133300030721     C                   EVAL      PiStr=SkSplitCSV(i)
133400030721     C                   EXSR      CHKNUM
133500030721     C                   IF        PiNum=*on
133600030721     C                   Z-ADD     PiVal         VABQFT
133700030721     C                   ELSE
133800030721     C                   SETON                                        32
133900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134000030721     C                             + ' ' + 'VABQFT'
134100030721     C                   ENDIF
134200030721     C                   ENDIF
134300030721     C*** NCD
134400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
134500030721     C                   EVAL      PiStr=SkSplitCSV(i)
134600030721     C                   EXSR      CHKNUM
134700030721     C                   IF        PiInt=*on      AND
134800030721     C                             PiVal<=9999999
134900030721     C                   Z-ADD     PiVal         VABNCD
135000030721     C                   ELSE
135100030721     C                   SETON                                        32
135200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135300030721     C                             + ' ' + 'VABNCD'
135400030721     C                   ENDIF
135500030721     C                   ENDIF
135600030721     C*** NCA
135700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
135800030721     C                   EVAL      PiStr=SkSplitCSV(i)
135900030721     C                   EXSR      CHKNUM
136000030721     C                   IF        PiInt=*on      AND
136100030721     C                             PiVal<=9999999
136200030721     C                   Z-ADD     PiVal         VABNCA
136300030721     C                   ELSE
136400030721     C                   SETON                                        32
136500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136600030721     C                             + ' ' + 'VABNCA'
136700030721     C                   ENDIF
136800030721     C                   ENDIF
136900030721     C*** ZNC
137000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
137100030721     C                   EVAL      PiStr=SkSplitCSV(i)
137200030721     C                   EXSR      CHKNUM
137300030721     C                   IF        PiInt=*on AND
137400030721     C                             PiVal<=99
137500030721     C                   Z-ADD     PiVal         VABZNC
137600030721     C                   ELSE
137700030721     C                   SETON                                        32
137800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137900030721     C                             + ' ' + 'VABZNC'
138000030721     C                   ENDIF
138100030721     C                   ENDIF
138200030721     C*** DCR
138300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
138400030721     C                   EVAL      PiStr=SkSplitCSV(i)
138500030721     C                   EXSR      CHKNUM
138600030721     C                   IF        PiInt=*on       AND
138700030721     C                             PiVal<=99999999
138800030721     C                   Z-ADD     PiVal         VABDCR
138900030721     C                   ELSE
139000030721     C                   SETON                                        32
139100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139200030721     C                             + ' ' + 'VABDCR'
139300030721     C                   ENDIF
139400030721     C                   ENDIF
139500030721     C*** HCR
139600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
139700030721     C                   EVAL      PiStr=SkSplitCSV(i)
139800030721     C                   EXSR      CHKNUM
139900030721     C                   IF        PiInt=*on   AND
140000030721     C                             PiVal<=9999
140100030721     C                   Z-ADD     PiVal         VABHCR
140200030721     C                   ELSE
140300030721     C                   SETON                                        32
140400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140500030721     C                             + ' ' + 'VABHCR'
140600030721     C                   ENDIF
140700030721     C                   ENDIF
140800030721     C*** VMD
140900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
141000030721     C                   EVAL      PiStr=SkSplitCSV(i)
141100030721     C                   EXSR      CHKNUM
141200030721     C                   IF        PiNum=*on
141300030721     C                   Z-ADD     PiVal         VABVMD
141400030721     C                   ELSE
141500030721     C                   SETON                                        32
141600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141700030721     C                             + ' ' + 'VABVMD'
141800030721     C                   ENDIF
141900030721     C                   ENDIF
142000030721     C*** ANT
142100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
142200030721     C                   EVAL      PiStr=SkSplitCSV(i)
142300030721     C                   EXSR      CHKNUM
142400030721     C                   IF        PiInt=*on        AND
142500030721     C                             PiVal<=999999999
142600030721     C                   Z-ADD     PiVal         VABANT
142700030721     C                   ELSE
142800030721     C                   SETON                                        32
142900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
143000030721     C                             + ' ' + 'VABANT'
143100030721     C                   ENDIF
143200030721     C                   ENDIF
143300030721     C*** CMO
143400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
143500030721     C                   EVAL      PiStr=SkSplitCSV(i)
143600030721     C                   EXSR      CHKNUM
143700030721     C                   IF        PiInt=*on
143800030721     C                   Z-ADD     PiVal         Num5_0
143900030721     C                   MOVEL(p)  Num5_0        VABCMO
144000030721     C                   ELSE
144100030721     C                   SETON                                        32
144200030721     C                   EVAL      VABCMO = *zeros
144300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
144400030721     C                             + ' ' + 'VABCMO'
144500030721     C                   ENDIF
144600030721     C                   ENDIF
144700040510     C*
144800040510     C* Reperisco i campi del FIVAT
144900040510     C*** VATNOT
145000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
145100040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
145200040510     C                   ENDIF
145300040510     C*** VATTRC
145400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
145500040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
145600040510     C                   ENDIF
145700060331     C*** VATNOT_A
145800060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
145900060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
146000060331     C                   ENDIF
146100060331     C*** VATNOT_B
146200060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
146300060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
146400060331     C                   ENDIF
146500060331     C*** VATNOT_E
146600070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
146700060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
146800060331     C                   ENDIF
146900070823     C*
147000100831     C***  ===>  Gestione campi "particolari" dentro ciclo
147100110406     C*** NSP / RMN
147200101021     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNSP'
147300110318     C                   EVAL      PiStr=%trim(SkSplitCSV(i))
147400101021     C                   EXSR      CHKNUM
147500101021     C                   IF        PiInt=*on
147600101021     C                   Z-ADD     PiVal         VABNSP
147700101021     C                   Z-ADD     PiVal         VABRMN
147800101021     C                   ELSE
147900101021     C                   SETON                                        31
148000101021     C                   EVAL      VABNSP = *zeros
148100101021     C                   EVAL      VABRMN = *zeros
148200101021     C                   EVAL      vinmsg = %trimr(vinmsg)
148300110406     C                             + ' ' + 'VABNSP VABRMN'
148400101021     C                   ENDIF
148500101021     C                   ENDIF
148600110406     C*** PRD
148700110406     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABPRD'
148800110406     C                   IF        %trim(SkSplitCSV(i)) = 'ITALY'
148900110406     C                   EVAL      VABPRD = *blanks
149000110406     C                   ELSE
149100110406     C                   EVAL      VABPRD = %trim(SkSplitCSV(i))
149200110406     C                   ENDIF
149300110406     C                   ENDIF
149400110406     C*** LOD_1
149500110406     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABLOD_1'
149600110406     C                   EVAL      wVABLOD_1 = %trim(SkSplitCSV(i))
149700110406     C                   ENDIF
149800110406     C*** LOD_2
149900110406     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABLOD_2'
150000110406     C                   EVAL      wVABLOD_2 = %trim(SkSplitCSV(i))
150100110406     C                   ENDIF
150200070925     C*
150300071029     C***  <===  -----------------------------------------
150400040510     C*
150500030715     C                   ENDIF
150600030715     C                   EVAL      i = i + 1
150700030715     C                   ENDDO
150800070925     C*
150900071029     C***  ===>  Gestione campi "particolari" fuori ciclo
151000100831     C*
151100100831     C                   IF        wVABRSD <> *blanks
151200100831     C                   EVAL      VABRSD = wVABRSD
151300100831     C                   ENDIF
151400100831     C*
151500100831     C                   IF        wVABIND <> *blanks
151600100831     C                   EVAL      VABIND = wVABIND
151700100831     C                   ENDIF
151800100831     C*
151900100831     C                   IF        wVABCAD <> *blanks
152000100831     C                   EVAL      VABCAD = wVABCAD
152100100831     C                   ENDIF
152200100831     C*
152300100831     C                   IF        wVABLOD <> *blanks
152400100831     C                   EVAL      VABLOD = wVABLOD
152500100831     C                   ENDIF
152600100831     C*
152700100831     C                   IF        wVABPRD <> *blanks
152800100831     C                   EVAL      VABPRD = wVABPRD
152900100831     C                   ENDIF
153000110406     C*
153100110406     C                   IF        wVABLOD_1 <> *blanks
153200110406     C                   EVAL      VABLOD = wVABLOD_1
153300110406     C                   ELSE
153400110406     C                   EVAL      VABLOD = wVABLOD_2
153500110406     C                   ENDIF
153600070925     C*
153700071029     C***  <===  -----------------------------------------
153800070925     C*
153900070925     C* Considerazioni finali su CBO/CAS
154000100318     C                   IF        wFlgCAS = '1'    OR
154100100318     C                             VABCAS  > *zeros
154200070925     C                   IF        VABCBO = '1'
154300070925     C                   EVAL      VABCBO = '4'
154400070925     C                   ENDIF
154500070925     C                   IF        VABCBO = '2'
154600070925     C                   EVAL      VABCBO = '6'
154700070925     C                   ENDIF
154800070925     C                   ENDIF
154900070925     C***  <===  ----------------------------
155000040510     C*
155100040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
155200040510     C                   IF        VABNSP = *zeros
155300040510     C                   SETON                                        31
155400040510     C                   ENDIF
155500040715     C*
155600040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
155700040715     C                   IF        VABLNP = *zeros
155800040715     C                   SETON                                        31
155900040715     C                   ENDIF
156000040510     C*
156100040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
156200040510     C                   IF        VABRMN = *zeros
156300040510     C                   Z-ADD     VABNSP        VABRMN
156400040510     C                   ENDIF
156500040510     C*
156600040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
156700040510     C                   IF        VABRMA = *blanks
156800040714     C                   MOVEL     VABRMN        VABRMA
156900040510     C                   ENDIF
157000040128     C*
157100040510     C* Se provincia nn valorizzata la reperisco
157200040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
157300040510     C                   IF        VABPRD = *blanks
157400040128     C                   CLEAR                   TISI95DS
157500040128     C                   EVAL      I95TCN = '3'
157600040128     C                   Z-ADD     datcor        I95DAT
157700040128     C                   EVAL      I95NAR = VABNZD
157800040128     C                   EVAL      I95CAP = VABCAD
157900040128     C                   EVAL      I95LOC = VABLOD
158000040128     C                   CALL      'TISI95R'
158100040128     C                   PARM                    TISI95DS
158200040128     C                   EVAL      VABPRD = O95PRV
158300040510     C                   ENDIF
158400020204     C*
158500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
158600020204     C                   EXSR      CHKIMPDIV
158700030715     C                   ENDIF
158800020204     C*
158900010607     C                   ENDSR
159000010607     C*----------------------------------------------------*
159100010601
159200020204
159300020204     C*----------------------------------------------------*
159400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
159500020204     C*----------------------------------------------------*
159600020204     C     CHKIMPDIV     BEGSR
159700020204     C*
159800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
159900020204     C                   Z-ADD     *zeros        wrkDec            9 9
160000020204     C*
160100020204     C* Come prima cosa effettuo considerazioni sulla divisa
160200020204     C                   IF        vabIAS > *zeros
160300020204     C                   IF        vabVAS <> 'EUR'
160400020204     C                   EVAL      vabVAS =  'ITL'
160500020204     C                   ENDIF
160600020204     C                   ENDIF
160700020204     C*
160800020204     C                   IF        vabCAS > *zeros
160900020204     C                   IF        vabVCA <> 'EUR'
161000020204     C                   EVAL      vabVCA =  'ITL'
161100020204     C                   ENDIF
161200020204     C                   ENDIF
161300020204     C*
161400020204     C                   IF        vabVMD > *zeros
161500020204     C                   IF        vabVAD <> 'EUR'
161600020204     C                   EVAL      vabVAD =  'ITL'
161700020204     C                   ENDIF
161800020204     C                   ENDIF
161900020204     C*
162000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
162100020204     C                   Z-ADD     vabIAS        wrkDec
162200020204     C                   IF        wrkDec > *zeros
162300020204     C                   IF        vabVAS = 'ITL'
162400020204     C                   EVAL      vabIAS = *zeros
162500020204     C                   ENDIF
162600020204     C                   ENDIF
162700020204     C*
162800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
162900020204     C                   Z-ADD     vabCAS        wrkDec
163000020204     C                   IF        wrkDec > *zeros
163100020204     C                   IF        vabVCA = 'ITL'
163200020204     C                   EVAL      vabCAS = *zeros
163300020204     C                   ENDIF
163400020204     C                   ENDIF
163500020204     C*
163600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
163700020204     C                   Z-ADD     vabVMD        wrkDec
163800020204     C                   IF        wrkDec > *zeros
163900020204     C                   IF        vabVAD = 'ITL'
164000020204     C                   EVAL      vabVMD = *zeros
164100020204     C                   ENDIF
164200020204     C                   ENDIF
164300020204     C*
164400020204     C                   ENDSR
164500020204     C***
164600010330
164700100318
164800100318
164900100318     C*----------------------------------------------------*
165000100318     C*  CONTROLLO NUMERICITA' CAMPI
165100100318     C*----------------------------------------------------*
165200100318     C     CHKNUM        BEGSR
165300100318     C*
165400100318     C                   IF        PiDecChr = *blanks
165500100318     C                   EVAL      PiDecChr = CharNUM
165600100318     C                   ENDIF
165700100318     C*
165800100318     C                   callp     UBISNUM_Check(PiStr
165900100318     C                                          :PiDecChr
166000100318     C                                          :PiVal
166100100318     C                                          :PiNum
166200100318     C                                          :PiInt)
166300100318     C*
166400100318     C                   ENDSR
166500100318     C***
166600010601
166700010601
166800010601
166900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
167000010601     C     repfil        BEGSR
167100010601     C*
167200010601     C                   if        invfil = *zeros and
167300010601     C                             depfil > *zeros and
167400010629     C                             (vinflg = *blanks or
167500010629     C                              vinflg = *zeros)
167600010601     C
167700010601     C                   eval      invfil = depfil
167800010601     C                   endif
167900010601     C*
168000010601     C                   if        depfil <> invfil and
168100010601     C                             invfil > *zeros
168200010601     C                   eval      flgMulti = '1'
168300010601     C                   if        vinflg = *blanks
168400010601     C                   add       1             cntNonEl
168500010601     C                   endif
168600010601     C                   endif
168700010601     C*
168800010601     C                   if        vinflg = '2'
168900010601     C                   eval      flgStato = '2'
169000010601     C                   endif
169100010601     C*
169200010601     C                   ENDSR
169300010601     C***
169400010601
169500010601
169600010601
169700010330
169800040119      /TITLE Invio dei dati al punto operativo.
169900040119     C     invio         BEGSR
170000040119     C*
170100040119     C* 1° invio FIVAT
170200040119     C                   reset                   dscmz
170300040119     C                   move      vlrpoi        cmzdst
170400040119     C                   eval      cmzfld = 'FIVATWWR'
170500040119     C                   eval      cmzmbd = vlrhdl
170600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
170700040119     C***                if        prmfir = *blanks
170800040119     C                   eval      cmzfla = 'FIVAT00F'
170900040119     C                   eval      cmzmba = 'FIVAT00F'
171000040119     C***                else
171100040119     C***                eval      cmzfla = prmfir
171200040119     C***                eval      cmzmba = prmfir
171300040119     C***                endif
171400040119     C                   eval      cmznrr = *zeros
171500040119     C                   move      §ctrokvt      cmznrr
171600040119     C                   eval      cmzlba = vlrfl1
171700040119     C                   call(e)   'TIS711C'
171800040119     C                   parm                    dscmz
171900040119     C                   parm      *blanks       esito
172000040119     C                   if        %error
172100040119     C                             or cmzerr = '1'
172200040119     C                             or esito  = '1'
172300040119     C                   eval      wrkesito = '3'
172400040119     C                   else
172500040119     C*
172600040119     C* 2° invio FIVAB
172700040119     C                   reset                   dscmz
172800040119     C                   move      vlrpoi        cmzdst
172900040119     C                   eval      cmzfld = vlrfou
173000040119     C                   eval      cmzmbd = vlrhdl
173100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
173200040119     C***                if        prmfir = *blanks
173300040119     C                   eval      cmzfla = 'FIVAB00F'
173400040119     C                   eval      cmzmba = 'FIVAB00F'
173500040119     C***                else
173600040119     C***                eval      cmzfla = prmfir
173700040119     C***                eval      cmzmba = prmfir
173800040119     C***                endif
173900040119     C                   eval      cmznrr = *zeros
174000040119     C                   move      §ctrokvb      cmznrr
174100040119     C                   eval      cmzlba = vlrfl1
174200040119     C                   call(e)   'TIS711C'
174300040119     C                   parm                    dscmz
174400040119     C                   parm      *blanks       esito
174500040119     C                   if        %error
174600040119     C                             or cmzerr = '1'
174700040119     C                             or esito  = '1'
174800040119     C                   eval      wrkesito = '3'
174900040119     C                   endif
175000040119     C                   endif
175100040119     C*
175200040119     C                   ENDSR
175300040119     C***
175400010601
175500010601
175600010601
175700010601
175800010601      /TITLE Invio dei dati al punto operativo.
175900010601     C     opeini        BEGSR
176000010601     C*
176100010601     C* Inizializzo flag e contatori operativi
176200010601     C                   movel     '0'           flgGiro           1
176300010601     C                   movel     '0'           flgMulti          1
176400010601     C                   movel     '1'           flgStato          1
176500010615     C                   movel     '0'           flgOk             1
176600010601     C                   z-add     *zeros        cntNonEl         10 0
176700010601     C                   z-add     *zeros        depfil            3 0
176800010601     C                   z-add     *zeros        invfil            3 0
176900010601     C*
177000010601     C                   ENDSR
177100010601     C***
177200070326
177300070326
177400070326
177500070326
177600070326     C     *pssr         BEGSR
177700070329     C*
177800070329     C                   if        %open(tivin00r)
177900070329     C                   close     tivin00r
178000070329     C                   endif
178100070329     C                   if        %open(fivabwwr)
178200070329     C                   close     fivabwwr
178300070329     C                   endif
178400070329     C                   if        %open(fivatwwr)
178500070329     C                   close     fivatwwr
178600070329     C                   endif
178700070326     C*
178800070326     C* Effettuo la chiamata al CLLE preposto
178900070326     C                   call(e)   'TITVVTC'
179000070326     C                   parm                    parccm
179100070326     C                   parm                    parmbr
179200070326     C                   parm      '2'           paropz
179300070326     C*
179400070326     C                   eval      wrkesito = '2'
179500070404     C*
179600070404     C                   seton                                        LR
179700070326     C*
179800070326     C                   ENDSR     '*CANCL'
179900070326     C***
180000100318
180100100318
180200100318
180300100318     C     normCSV       BEGSR
180400100318     C*
180500100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
180600100318     C* separatori campi
180700100318     C                   if        CharTXT <> *blanks
180800100318     C                   z-add     *zeros        wLenVINDTA        4 0
180900100318     C                   z-add     *zeros        wFlgVINDTA        1 0
181000100318     C                   setoff                                       80
181100100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
181200100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
181300100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
181400100318     C                   if        not *in80
181500100318     C                   seton                                        80
181600100318     C                   else
181700100318     C                   setoff                                       80
181800100318     C                   endif
181900100318     C                   endif
182000100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
182100100318     C                             and *in80 = *on
182200100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
182300100318     C                   endif
182400100318     C                   enddo
182500100318     C                   endif
182600100318     C*
182700100318     C                   ENDSR
182800100318     C***
182900070326
183000100326
183100100326
183200100326     C*----------------------------------------------------*
183300100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
183400100326     C*----------------------------------------------------*
183500100326     C     REPNSP        BEGSR
183600100326     C*
183700100326     C* NSP => Stacco un numeratore da AZNUM
183800100326     C                   clear                   TRUL33DS
183900100326     C                   eval      I33OPE = *zeros
184000100326     C                   eval      I33CNU = 302
184100100326     C                   eval      I33NUM = 1
184200100326     C                   movel     TRUL33DS      KPJBU
184300100326     C                   call      'TRUL33R'
184400100326     C                   parm                    KPJBA
184500100326     C                   movel     KPJBU         TRUL33DS
184600100326     C                   if        O33ERR = *zeros
184700100326     C                   z-add     O33NRF        VABNSP
184800100326     C                   z-add     O33NRF        VATNSP
184900100326     C                   else
185000100326     C                   SETON                                        31
185100100326     C                   EVAL      vinmsg = %trimr(vinmsg)
185200100326     C                             + ' ' + 'VABNSP VATNSP'
185300100326     C                   endif
185400100326     C*
185500100326     C                   ENDSR
185600010330
185700100326
185800010330
185900000613     C     *inzsr        BEGSR
186000990910     C*
186100990910     C     *entry        plist
186200990920     C                   parm                    tivlrds
186300990921     C                   parm      wrkesito      esito
186400000724     C                   parm                    prmlit
186500000710     C                   parm                    prmfir
186600010330     C*
186700010330     C* CALCOLA LA DATA CORRENTE
186800100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
186900100318     C                   eval      datcor = %dec(%date() : *ISO)
187000000613     C*
187100000613     C                   ENDSR
187200000613     C***
187300990908
187400110406     Otitv2d9p  E            riepilogo         2
187500990915     O                                              'Upload via Internet'
187600990915     O                                           +1 'Traduzione TIVIN00R -'
187700021025     O                                           +1 'FIVABWWR'
187800990915     O                                           +1 'Testate bolle'
187900990915     O          E            riepilogo   2
188000990915     O                       wrkdata
188100990915     O                       wrkora              +1
188200990915     O                       procname            +1
188300990915     O          E            riepilogo   2
188400990915     O                                              'Cliente..................:'
188500010605     O                       VABCCM        z     +1
188600990915     O          E            riepilogo   2
188700990920     O                                              'Riferimento Strategi.....:'
188800990920     O                       vlrhdl              +1
188900990915     O          E            riepilogo   2
189000990915     O                                              'Giusti...................:'
189100040119     O                       §CTROKVB      2   +  1
189200990915     O          E            riepilogo   2
189300990915     O                                              'Sbagliati e corretti.....:'
189400971022     O                       §CTRMO        2   +  1
189500990915     O          E            riepilogo   2
189600990915     O                                              'Sbagliati e scartati.....:'
189700971022     O                       §CTRNO        2   +  1
189800000613
189900110406     Otitv2d9ps E            testdett          2
190000000613     O                                              'Upload via Internet'
190100000613     O                                           +1 'Traduzione TIVIN00R -'
190200021025     O                                           +1 'FIVABWWR'
190300000613     O                                           +1 'Testate bolle'
190400000616     O          E            testdett    3
190500000613     O                                           +2 'N° rec'
190600000613     O                                           +3 'Anteprima contenuto'
190700000616     O          E            rigadett    2
190800000613     O                       rrnum               +2
190900000621     O                       recko               +3
191000000616     O          E            findett     2
191100000613     O                       wrkdata
191200000613     O                       wrkora              +1
191300000613     O                       procname            +1
191400000616     O          E            findett     2
191500000613     O                                              'Cliente..................:'
191600010605     O                       VABCCM        z     +1
191700000616     O          E            findett     2
191800000613     O                                              'Riferimento Strategi.....:'
191900000613     O                       vlrhdl              +1
192000000616     O          E            findett     2
192100000613     O                                              'Giusti...................:'
192200040119     O                       §CTROKVB      2   +  1
192300000616     O          E            findett     2
192400000613     O                                              'Sbagliati e corretti.....:'
192500000613     O                       §CTRMO        2   +  1
192600000616     O          E            findett     2
192700000613     O                                              'Sbagliati e scartati.....:'
192800000613     O                       §CTRNO        2   +  1
192900000613** CMD - COMANDI CL
193000110406OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV2D9P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
193100110406OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV2D9PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
193200110406DLTOVR FILE(TITV2D9P TITV2D9PS) LVL(*)
193300010606
193400000613
193500040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
193600110406W_VABNSP;VABRMA;FIL001;VABRSD;VABIND;W_VABLOD_1;W_VABLOD_2;W_VABPRD;VABCAD;
193700110406VATNOT_B;VABPKB;VABNCL;VATNOT_E;
193800110315
193900110315
194000100318
