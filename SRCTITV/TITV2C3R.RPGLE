000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200100318     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
000700040119     FFIVATwwr  O    E             DISK    usropn
000800110131     Ftitv2c3p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070329     F                                     infsr(*pssr)
001100110131     Ftitv2c3ps O    f  198        PRINTER usropn
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
015500110131     C                   if        not %open(titv2c3ps)
015600110131     C                   open      titv2c3ps
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
018700110131     C                   if        %open(titv2c3ps)
018800000616     C                   except    findett
018900110131     C                   close     titv2c3ps
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
020900110131     C                   if        not %open(titv2c3p)
021000110131     C   50              open      titv2c3p
021100990915     C                   endif
021200990915     C*
021300030715     C   50              except    riepilogo
021400990915     C*
021500110131     C                   if        %open(titv2c3p)
021600110131     C   50              close     titv2c3p
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
025200100318     C***                exsr      inzvar
025300100318     C***                exsr      impvab
025400110131     C*
025500110131     C* Effettuo una prima lettura fuori ciclo x ignorare la 1° riga
025600110131     C                   read      tivin00r                               70
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
040700110201     C                   if        *in35
040800110201     C                   z-add     *zeros        VABNCL
040900110201     C                   z-add     *zeros        VABPKB
041000110201     C                   z-add     *zeros        VABVLB
041100110201     C                   endif
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
046900040714     C*
047000040714     C* Solo se VATNOT valorizzato scarico il buffer
047100040714     C                   if        VATNOT <> *blanks
047200040119     C*
047300040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
047400040510     C                   if        VATTRC = *blanks
047500040510     C                   eval      VATTRC = 'E'
047600040510     C                   endif
047700040510     C                   write     FIVAT000
047800040119     C                   add       1             §CTROKVT
047900040714     C*
048000040714     C                   endif
048100060331     C*
048200060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
048300060331     C                   if        wVATNOT_A <> *blanks
048400060331     C                   eval      VATNOT = wVATNOT_A
048500060331     C                   eval      VATTRC = 'A'
048600060331     C                   write     FIVAT000
048700060331     C                   add       1             §CTROKVT
048800060331     C                   endif
048900060331     C*
049000060331     C                   if        wVATNOT_B <> *blanks
049100060331     C                   eval      VATNOT = wVATNOT_B
049200060331     C                   eval      VATTRC = 'B'
049300060331     C                   write     FIVAT000
049400060331     C                   add       1             §CTROKVT
049500060331     C                   endif
049600060331     C*
049700060331     C                   if        wVATNOT_E <> *blanks
049800110201     C                   eval      VATNOT = wVATNOT_E
049900060331     C                   eval      VATTRC = 'E'
050000110201     C                   write     FIVAT000
050100110201     C                   add       1             §CTROKVT
050200110201     C                   endif
050300040119     C*
050400040119     C                   ENDSR
050500040119
050600040119
050700040119
050800040119     C*----------------------------------------------------*
050900040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
051000040119     C*----------------------------------------------------*
051100040119     C     PREVAT        BEGSR
051200040119     C*
051300040119     C* Compongo il nome del membro da dare al FIVATWWR
051400040119     C                   eval      parmbr = vlrhdl
051500040119     C                   movel     'M'           parmbr
051600040510     C                   eval      parccm = vlrksc
051700040119     C                   eval      paropz = '1'
051800040119     C* Effettuo la chiamata al CLLE preposto
051900040119     C                   call(e)   'TITVVTC'
052000040119     C                   parm                    parccm
052100040119     C                   parm                    parmbr
052200040119     C                   parm                    paropz
052300040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
052400040119     C                   if        %error
052500040119     C                   movel     '1'           chkcall
052600040119     C                   else
052700040119     C                   movel     '0'           chkcall
052800040119     C                   endif
052900040119     C*
053000040119     C                   ENDSR
053100010601
053200010601
053300010601
053400010330     C*----------------------------------------------------*
053500030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
053600010330     C*----------------------------------------------------*
053700040119     C     INZVAR        BEGSR
053800080527     C*
053900080527     C                   setoff                                       65
054000010330     C*
054100030822     C* Inizializzo variabili di wrk
054200020204     C                   Z-ADD     *zeros        Num5_0            5 0
054300101018     C                   MOVEL     *blanks       wFlgCAS
054400060331     C                   MOVEL     *blanks       wVATNOT_A
054500060331     C                   MOVEL     *blanks       wVATNOT_B
054600060331     C                   MOVEL     *blanks       wVATNOT_E
054700100831     C                   MOVEL     *blanks       wVABRSD
054800100831     C                   MOVEL     *blanks       wVABIND
054900100831     C                   MOVEL     *blanks       wVABCAD
055000100831     C                   MOVEL     *blanks       wVABLOD
055100100831     C                   MOVEL     *blanks       wVABPRD
055200030822     C*
055300030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
055400030822     C                   CLEAR                   SkSplitCSV
055500030822     C                   EVAL      i = 1
055600030822     C                   EVAL      posDa = *zeros
055700030822     C                   EVAL      posA  = *zeros
055800080527     C*
055900080527     C                   CLEAR                   FIVABDS
056000080527     C                   CLEAR                   FIVAT000
056100030822     C*
056200030822     C* Reimposto i valori di default
056300030822     C                   EXSR      DEFCAM
056400030822     C*
056500010330     C                   ENDSR
056600010330     C*----------------------------------------------------*
056700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
056800010330     C*----------------------------------------------------*
056900010330     C     DEFCAM        BEGSR
057000030822     C*
057100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
057200030715     C* e delimitatore testo.
057300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
057400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
057500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
057600100318     C*
057700100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
057800100318     C                   EVAL      CharSOS = CharNUM
057900030822     C*
058000030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
058100100720     C                   SETOFF                                       505152
058200100720     C                   SETOFF                                       53
058300100413     C                   SELECT
058400100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
058500100720     C                   SETON                                        50
058600100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
058700100413     C                   SETON                                        51
058800100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
058900100720     C                   SETON                                        52
059000100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
059100100720     C                   SETON                                        53
059200100413     C                   ENDSL
059300030822     C*
059400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
059500030822     C                   EVAL      posDaDft = 1
059600030822     C                   EVAL      posADft  = 0
059700030822     C                   EVAL      wGiroDft = 0
059800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
059900030822     C                             posDaDft > 0
060000030822     C*
060100030822     C* Gestisco il 1° giro
060200030822     C                   IF        wGiroDft = 0
060300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
060400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
060500030822     C* Incremento il contatore dei "giri"
060600030822     C                   EVAL      wGiroDft = 1
060700030822     C                   ELSE
060800030822     C                   EVAL      posDaDft = posADft
060900030822     C                   ENDIF
061000030822     C* Eseguo lo scan x trovare la fine del campo corrente
061100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
061200030822     C*
061300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
061400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
061500030822     C                   IF        posDaDft > 0 AND
061600030822     C                             posADft  > 0
061700030822     C* NCL
061800030822     C                   IF        %subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):1:3)
062100030822     C                             = 'NCL'
062200030822     C                   EVAL      PiStr=%trim(%subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062500030822     C                   EXSR      CHKNUM
062600030822     C                   IF        PiInt=*on
062700030822     C                   Z-ADD     PiVal         VABNCL
062800030822     C                   ENDIF
062900030822     C                   ENDIF
063000030822     C* CCM
063100030822     C                   IF        %subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):1:3)
063400030822     C                             = 'CCM'
063500030822     C                   EVAL      PiStr=%trim(%subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063800030822     C                   EXSR      CHKNUM
063900030822     C                   IF        PiInt=*on
064000030822     C                   Z-ADD     PiVal         VABCCM
064100030822     C                   ENDIF
064200030822     C                   ENDIF
064300030822     C* LNP
064400030822     C                   IF        %subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):1:3)
064700030822     C                             = 'LNP'
064800030822     C                   EVAL      PiStr=%trim(%subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065100030822     C                   EXSR      CHKNUM
065200030822     C                   IF        PiInt=*on
065300030822     C                   Z-ADD     PiVal         VABLNP
065400030822     C                   ENDIF
065500030822     C                   ENDIF
065600030822     C* NRS
065700030822     C                   IF        %subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):1:3)
066000030822     C                             = 'NRS'
066100030822     C                   EVAL      PiStr=%trim(%subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066400030822     C                   EXSR      CHKNUM
066500030822     C                   IF        PiInt=*on
066600030822     C                   Z-ADD     PiVal         VABNRS
066700030822     C                   ENDIF
066800030822     C                   ENDIF
066900030822     C* CTR
067000030822     C                   IF        %subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):1:3)
067300030822     C                             = 'CTR'
067400030822     C                   EVAL      PiStr=%trim(%subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067700030822     C                   EXSR      CHKNUM
067800030822     C                   IF        PiInt=*on
067900030822     C                   Z-ADD     PiVal         VABCTR
068000030822     C                   ENDIF
068100030822     C                   ENDIF
068200030822     C* PKB
068300030822     C                   IF        %subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):1:3)
068600030822     C                             = 'PKB'
068700030822     C                   EVAL      PiStr=%trim(%subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069000030822     C                   EXSR      CHKNUM
069100030822     C                   IF        PiNum=*on
069200030822     C                   Z-ADD     PiVal         VABPKB
069300030822     C                   ENDIF
069400030822     C                   ENDIF
069500030822     C* VLB
069600030822     C                   IF        %subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):1:3)
069900030822     C                             = 'VLB'
070000030822     C                   EVAL      PiStr=%trim(%subst(
070100030822     C                             %subst(vlrppt:posDaDft+1:
070200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070300030822     C                   EXSR      CHKNUM
070400030822     C                   IF        PiNum=*on
070500030822     C                   Z-ADD     PiVal         VABVLB
070600030822     C                   ENDIF
070700030822     C                   ENDIF
070800030822     C* QFT
070900030822     C                   IF        %subst(
071000030822     C                             %subst(vlrppt:posDaDft+1:
071100030822     C                             posADft-posDaDft-1):1:3)
071200030822     C                             = 'QFT'
071300030822     C                   EVAL      PiStr=%trim(%subst(
071400030822     C                             %subst(vlrppt:posDaDft+1:
071500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071600030822     C                   EXSR      CHKNUM
071700030822     C                   IF        PiNum=*on
071800030822     C                   Z-ADD     PiVal         VABQFT
071900030822     C                   ENDIF
072000030822     C                   ENDIF
072100030822     C* CBO
072200030822     C                   IF        %subst(
072300030822     C                             %subst(vlrppt:posDaDft+1:
072400030822     C                             posADft-posDaDft-1):1:3)
072500030822     C                             = 'CBO'
072600030822     C                   EVAL      VABCBO=%trim(%subst(
072700030822     C                             %subst(vlrppt:posDaDft+1:
072800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072900030822     C                   ENDIF
073000030822     C* TSP
073100030822     C                   IF        %subst(
073200030822     C                             %subst(vlrppt:posDaDft+1:
073300030822     C                             posADft-posDaDft-1):1:3)
073400030822     C                             = 'TSP'
073500030822     C                   EVAL      VABTSP=%trim(%subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073800030822     C                   ENDIF
073900030822     C* VAS
074000030822     C                   IF        %subst(
074100030822     C                             %subst(vlrppt:posDaDft+1:
074200030822     C                             posADft-posDaDft-1):1:3)
074300030822     C                             = 'VAS'
074400030822     C                   EVAL      VABVAS=%trim(%subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074700030822     C                   ENDIF
074800030822     C* VCA
074900030822     C                   IF        %subst(
075000030822     C                             %subst(vlrppt:posDaDft+1:
075100030822     C                             posADft-posDaDft-1):1:3)
075200030822     C                             = 'VCA'
075300030822     C                   EVAL      VABVCA=%trim(%subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075600030822     C                   ENDIF
075700070925     C* TIC
075800070925     C                   IF        %subst(
075900070925     C                             %subst(vlrppt:posDaDft+1:
076000070925     C                             posADft-posDaDft-1):1:3)
076100070925     C                             = 'TIC'
076200070925     C                   EVAL      VABTIC=%trim(%subst(
076300070925     C                             %subst(vlrppt:posDaDft+1:
076400070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076500070925     C                   ENDIF
076600030822     C* GCA
076700030822     C                   IF        %subst(
076800030822     C                             %subst(vlrppt:posDaDft+1:
076900030822     C                             posADft-posDaDft-1):1:3)
077000030822     C                             = 'GCA'
077100030822     C                   EVAL      VABGCA=%trim(%subst(
077200030822     C                             %subst(vlrppt:posDaDft+1:
077300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077400030822     C                   ENDIF
077500030822     C* CTM
077600030822     C                   IF        %subst(
077700030822     C                             %subst(vlrppt:posDaDft+1:
077800030822     C                             posADft-posDaDft-1):1:3)
077900030822     C                             = 'CTM'
078000030822     C                   EVAL      VABCTM=%trim(%subst(
078100030822     C                             %subst(vlrppt:posDaDft+1:
078200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078300030822     C                   ENDIF
078400030822     C* FFD
078500030822     C                   IF        %subst(
078600030822     C                             %subst(vlrppt:posDaDft+1:
078700030822     C                             posADft-posDaDft-1):1:3)
078800030822     C                             = 'FFD'
078900030822     C                   EVAL      VABFFD=%trim(%subst(
079000030822     C                             %subst(vlrppt:posDaDft+1:
079100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079200030822     C                   ENDIF
079300030822     C* VAD
079400030822     C                   IF        %subst(
079500030822     C                             %subst(vlrppt:posDaDft+1:
079600030822     C                             posADft-posDaDft-1):1:3)
079700030822     C                             = 'VAD'
079800030822     C                   EVAL      VABVAD=%trim(%subst(
079900030822     C                             %subst(vlrppt:posDaDft+1:
080000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080100030822     C                   ENDIF
080200030822     C* GMA
080300030822     C                   IF        %subst(
080400030822     C                             %subst(vlrppt:posDaDft+1:
080500030822     C                             posADft-posDaDft-1):1:3)
080600030822     C                             = 'GMA'
080700030822     C                   EVAL      VABGMA=%trim(%subst(
080800030822     C                             %subst(vlrppt:posDaDft+1:
080900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081000030822     C                   ENDIF
081100030822     C* GGA
081200030822     C                   IF        %subst(
081300030822     C                             %subst(vlrppt:posDaDft+1:
081400030822     C                             posADft-posDaDft-1):1:3)
081500030822     C                             = 'GGA'
081600030822     C                   EVAL      VABGGA=%trim(%subst(
081700030822     C                             %subst(vlrppt:posDaDft+1:
081800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081900030822     C                   ENDIF
082000030822     C* GVA
082100030822     C                   IF        %subst(
082200030822     C                             %subst(vlrppt:posDaDft+1:
082300030822     C                             posADft-posDaDft-1):1:3)
082400030822     C                             = 'GVA'
082500030822     C                   EVAL      VABGVA=%trim(%subst(
082600030822     C                             %subst(vlrppt:posDaDft+1:
082700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082800030822     C                   ENDIF
082900030822     C* TC1
083000030822     C                   IF        %subst(
083100030822     C                             %subst(vlrppt:posDaDft+1:
083200030822     C                             posADft-posDaDft-1):1:3)
083300030822     C                             = 'TC1'
083400030822     C                   EVAL      VABTC1=%trim(%subst(
083500030822     C                             %subst(vlrppt:posDaDft+1:
083600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083700030822     C                   ENDIF
083800030822     C* TC2
083900030822     C                   IF        %subst(
084000030822     C                             %subst(vlrppt:posDaDft+1:
084100030822     C                             posADft-posDaDft-1):1:3)
084200030822     C                             = 'TC2'
084300030822     C                   EVAL      VABTC2=%trim(%subst(
084400030822     C                             %subst(vlrppt:posDaDft+1:
084500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084600030822     C                   ENDIF
084700040714     C* VATTRC
084800040714     C                   IF        %subst(
084900040714     C                             %subst(vlrppt:posDaDft+1:
085000040714     C                             posADft-posDaDft-1):1:3)
085100040714     C                             = 'TRC'
085200040714     C                   EVAL      VATTRC=%trim(%subst(
085300040714     C                             %subst(vlrppt:posDaDft+1:
085400040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085500040714     C                   ENDIF
085600030822     C* ...
085700030822     C                   ENDIF
085800030822     C                   ENDDO
085900020204     C*
086000010330     C                   ENDSR
086100010607     C*----------------------------------------------------*
086200021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
086300010607     C*----------------------------------------------------*
086400010607     C     IMPVAB        BEGSR
086500010607     C*
086600010607     C                   SETOFF                                       3132
086700110201     C                   SETON                                        35
086800030515     C*
086900010607     C                   MOVEL     datcor        VABAAS
087000010607     C                   MOVE      datcor        VABMGS
087100010607     C*
087200030715     C**********
087300030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
087400030715     C**********
087500030715     C                   IF        wGiro = *zeros
087600040113     C*
087700040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
087800040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
087900080429     C                             %trim(intcol(3))+%trim(intcol(4))+
088000080429     C                             %trim(intcol(5))<>*blanks
088100040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
088200080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
088300080429     C                                      %trim(intcol(5))
088400070823     C     ';':CharCSV   XLATE     vindta        vindta
088500040510     C                   ENDIF
088600031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
088700031201     C     minu:maiu     XLATE     vindta        vindta
088800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088900030715     C                   DOW       posDa <= %len(%trim(vindta))
089000030715     C*
089100030715     C* Gestisco il 1° campo
089200030715     C                   IF        i = 1
089300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
089400030715     C                   EVAL      posDa = 1
089500030715     C* Eseguo lo scan x trovare la fine del primo campo
089600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089700030715     C* A questo punto "estraggo" il campo corrente
089800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089900030715     C                                                    (posA-posDa))
090000030715     C* X i campi successivi al 1°
090100030715     C                   ELSE
090200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
090300030715     C                   EVAL      posDa = posA + 1
090400030715     C* Eseguo lo scan x trovare la fine del campo corrente
090500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090600030715     C* Gestisco l'ultimo campo
090700030715     C                   IF        posA = *zeros
090800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090900030715     C* A questo punto "estraggo" il campo corrente
091000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
091100030715     C                                                    (posA-posDa))
091200030715     C                   LEAVE
091300030715     C                   ELSE
091400030715     C* A questo punto "estraggo" il campo corrente
091500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
091600030715     C                                                    (posA-posDa))
091700030715     C                   ENDIF
091800030715     C                   ENDIF
091900030715     C* Incremento il contatore d campo
092000030715     C                   EVAL      i = i +1
092100030715     C                   ENDDO
092200030715     C*
092300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092400030715     C* delimitazione testo
092500030715     C                   EVAL      i = 1
092600030715     C                   DOW       i < %elem(SkSplitFLD)
092700030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
092800030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
092900030715     C                   EVAL      i = i + 1
093000030715     C                   ENDDO
093100030715     C                   EVAL      wGiro = 1
093200030715     C                   ELSE
093300030715     C                   EVAL      wGiro = 2
093400031204     C**********
093500031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
093600031204     C* d separatore campo
093700031204     C**********
093800031204     C                   EVAL      vindta = %trim(vindta)
093900031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
094000031204     C                   EVAL      vindta = %subst(vindta:2)
094100031204     C                   ENDDO
094200031204     C*
094300031204     C                   Z-ADD     *zeros        lunghInput        4 0
094400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
094500031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
094600031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
094700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
094800031204     C                   ENDDO
094900030715     C**********
095000030715     C* Effettuo lo split del campo dati d input
095100030715     C**********
095200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
095300030715     C                   DOW       posDa <= %len(%trim(vindta))
095400030715     C*
095500030715     C* Gestisco il 1° campo
095600030715     C                   IF        i = 1
095700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
095800030715     C                   EVAL      posDa = 1
095900030715     C* Eseguo lo scan x trovare la fine del primo campo
096000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
096100030715     C* A questo punto "estraggo" il campo corrente
096200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096300030715     C                                                    (posA-posDa))
096400030715     C* X i campi successivi al 1°
096500030715     C                   ELSE
096600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
096700030715     C                   EVAL      posDa = posA + 1
096800031204     C* Verifico che nn vi sia il campo nn valorizzato
096900031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
097000031204     C* Se campo nn valorizzato skippo al prossimo
097100031204     C                   EVAL      SkSplitCSV(i) = *blanks
097200031204     C                   EVAL      posA  = posA + 1
097300031204     C                   ELSE
097400030715     C* Eseguo lo scan x trovare la fine del campo corrente
097500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
097600030715     C* Gestisco l'ultimo campo
097700030715     C                   IF        posA = *zeros
097800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
097900030715     C* A questo punto "estraggo" il campo corrente
098000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
098100030715     C                                                    (posA-posDa))
098200030715     C                   LEAVE
098300030715     C                   ELSE
098400030715     C* A questo punto "estraggo" il campo corrente
098500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
098600030715     C                                                    (posA-posDa))
098700030715     C                   ENDIF
098800031204     C                   ENDIF
098900030715     C                   ENDIF
099000030715     C* Incremento il contatore d campo
099100030715     C                   EVAL      i = i +1
099200030715     C                   ENDDO
099300030715     C*
099400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
099500030715     C* delimitazione testo
099600030715     C                   EVAL      i = 1
099700030715     C                   DOW       i < %elem(SkSplitCSV)
099800030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
099900030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
100000030715     C                   EVAL      i = i + 1
100100030715     C                   ENDDO
100200100720     C*
100300100720     C* Stacco un progressivo x numero spedizione
100400100720 xxx C***                EXSR      repNSP
100500100326     C*
100600030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
100700030715     C                   EVAL      i = 1
100800030715     C                   DOW       i < %elem(SkSplitFLD)
100900030715     C*
101000030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
101100030715     C                   IF        SkSplitFLD(i) <> *blanks AND
101200030715     C                             SkSplitCSV(i) <> *blanks
101300030721     C*** ATB
101400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
101500030721     C                   EVAL      VABATB = SkSplitCSV(i)
101600030721     C                   ENDIF
101700030715     C*** RSD
101800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
101900030715     C                   EVAL      VABRSD = SkSplitCSV(i)
102000030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
102100030715     C     '@':'A'       XLATE     VABRSD        VABRSD
102200030715     C* ==
102300030715     C                   ENDIF
102400030721     C*** RD2
102500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
102600030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
102700030721     C                   ENDIF
102800030715     C*** IND
102900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
103000030715     C                   EVAL      VABIND = SkSplitCSV(i)
103100030715     C                   ENDIF
103200030715     C*** LOD
103300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
103400030715     C                   EVAL      VABLOD = SkSplitCSV(i)
103500030715     C                   ENDIF
103600030715     C*** PRD
103700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
103800030715     C                   EVAL      VABPRD = SkSplitCSV(i)
103900030715     C                   ENDIF
104000030721     C*** NZD
104100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
104200030721     C                   EVAL      VABNZD = SkSplitCSV(i)
104300030721     C                   ENDIF
104400030715     C*** RMA
104500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
104600030715     C                   EVAL      VABRMA = SkSplitCSV(i)
104700030715     C                   ENDIF
104800030715     C*** NOT
104900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
105000030715     C                   EVAL      VABNOT = SkSplitCSV(i)
105100030715     C                   ENDIF
105200030715     C*** NT2
105300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
105400030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
105500030715     C                   ENDIF
105600030715     C*** VCA
105700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
105800030715     C                   EVAL      VABVCA = SkSplitCSV(i)
105900030715     C                   ENDIF
106000030715     C*** VAS
106100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
106200030715     C                   EVAL      VABVAS = SkSplitCSV(i)
106300030715     C                   ENDIF
106400030715     C*** FFD
106500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
106600030715     C                   EVAL      VABFFD = SkSplitCSV(i)
106700030715     C                   ENDIF
106800030721     C*** GC1
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
107000030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
107100030721     C                   ENDIF
107200030721     C*** GC2
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
107400030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
107500030721     C                   ENDIF
107600030721     C*** TSP
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
107800030721     C                   EVAL      VABTSP = SkSplitCSV(i)
107900030721     C                   ENDIF
108000040510     C*** CBO
108100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
108200040510     C                   EVAL      VABCBO = SkSplitCSV(i)
108300040510     C                   ENDIF
108400030721     C*** NAS
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
108600030721     C                   EVAL      VABNAS = SkSplitCSV(i)
108700030721     C                   ENDIF
108800030721     C*** TIC
108900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
109000030721     C                   EVAL      VABTIC = SkSplitCSV(i)
109100030721     C                   ENDIF
109200030721     C*** GCA
109300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
109400030721     C                   EVAL      VABGCA = SkSplitCSV(i)
109500030721     C                   ENDIF
109600030721     C*** XCO
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
109800030721     C                   EVAL      VABXCO = SkSplitCSV(i)
109900030721     C                   ENDIF
110000030721     C*** CTM
110100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
110200030721     C                   EVAL      VABCTM = SkSplitCSV(i)
110300030721     C                   ENDIF
110400030721     C*** TCR
110500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
110600030721     C                   EVAL      VABTCR = SkSplitCSV(i)
110700030721     C                   ENDIF
110800030721     C*** CTS
110900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
111000030721     C                   EVAL      VABCTS = SkSplitCSV(i)
111100030721     C                   ENDIF
111200030721     C*** FTM
111300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
111400030721     C                   EVAL      VABFTM = SkSplitCSV(i)
111500030721     C                   ENDIF
111600030721     C*** VAD
111700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
111800030721     C                   EVAL      VABVAD = SkSplitCSV(i)
111900030721     C                   ENDIF
112000030721     C*** GMA
112100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
112200030721     C                   EVAL      VABGMA = SkSplitCSV(i)
112300030721     C                   ENDIF
112400030721     C*** GGA
112500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
112600030721     C                   EVAL      VABGGA = SkSplitCSV(i)
112700030721     C                   ENDIF
112800030721     C*** GVA
112900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
113000030721     C                   EVAL      VABGVA = SkSplitCSV(i)
113100030721     C                   ENDIF
113200030721     C*** TC1
113300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
113400030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
113500030721     C                   ENDIF
113600030721     C*** TC2
113700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
113800030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
113900030721     C                   ENDIF
114000030721     C*** SCL
114100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
114200030721     C                   EVAL      VABSCL = SkSplitCSV(i)
114300030721     C                   ENDIF
114400030721     C*** RMO
114500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
114600030721     C                   EVAL      VABRMO = SkSplitCSV(i)
114700030721     C                   ENDIF
114800030721     C*** NMO
114900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
115000030721     C                   EVAL      VABNMO = SkSplitCSV(i)
115100030721     C                   ENDIF
115200030721     C*
115300030721     C* Reperisco quindi i campi numerici...
115400040510     C*** NSP
115500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
115600040510     C                   EVAL      PiStr=SkSplitCSV(i)
115700040510     C                   EXSR      CHKNUM
115800040510     C                   IF        PiInt=*on
115900040510     C                   Z-ADD     PiVal         VABNSP
116000040510     C                   ELSE
116100040510     C                   SETON                                        31
116200040510     C                   EVAL      VABNSP = *zeros
116300040510     C                   EVAL      vinmsg = %trimr(vinmsg)
116400040510     C                             + ' ' + 'VABNSP'
116500040510     C                   ENDIF
116600040510     C                   ENDIF
116700071106     C*** RMN
116800071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
116900071106     C                   EVAL      PiStr=SkSplitCSV(i)
117000071106     C                   EXSR      CHKNUM
117100071106     C                   IF        PiInt=*on
117200071106     C                   Z-ADD     PiVal         VABRMN
117300071106     C                   ELSE
117400071106     C                   SETON                                        32
117500071106     C                   EVAL      VABRMN = *zeros
117600071106     C                   EVAL      vinmsg = %trimr(vinmsg)
117700071106     C                             + ' ' + 'VABRMN'
117800071106     C                   ENDIF
117900071106     C                   ENDIF
118000030721     C*** CAD
118100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
118200030721     C                   EVAL      PiStr=SkSplitCSV(i)
118300030721     C                   EXSR      CHKNUM
118400030721     C                   IF        PiInt=*on
118500030721     C                   Z-ADD     PiVal         Num5_0
118600030721     C                   MOVEL(p)  Num5_0        VABCAD
118700030721     C                   ELSE
118800030721     C                   SETON                                        32
118900030721     C                   EVAL      VABCAD = *zeros
119000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119100030721     C                             + ' ' + 'VABCAD'
119200030721     C                   ENDIF
119300030721     C                   ENDIF
119400030721     C*** NCL
119500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
119600030721     C                   EVAL      PiStr=SkSplitCSV(i)
119700030721     C                   EXSR      CHKNUM
119800030721     C                   IF        PiInt=*on
119900030721     C                   Z-ADD     PiVal         VABNCL
120000030721     C                   ELSE
120100030721     C                   SETON                                        32
120200030721     C                   EVAL      VABNCL = *zeros
120300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120400040122     C                             + ' ' + 'VABNCL'
120500030721     C                   ENDIF
120600030721     C                   ENDIF
120700030721     C*** PKB
120800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
120900030721     C                   EVAL      PiStr=SkSplitCSV(i)
121000030721     C                   EXSR      CHKNUM
121100030721     C                   IF        PiNum=*on
121200030721     C                   Z-ADD     PiVal         VABPKB
121300030721     C                   ELSE
121400030721     C                   SETON                                        32
121500030721     C                   EVAL      VABPKB = *zeros
121600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121700030721     C                             + ' ' + 'VABPKB'
121800030721     C                   ENDIF
121900030721     C                   ENDIF
122000030721     C*** CAS
122100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
122200101018     C                   IF        SkSplitCSV(i) <> *blanks AND
122300101018     C                             SkSplitCSV(i) <> *zeros
122400101018     C                   EVAL      wFlgCAS = '1'
122500101018     C                   ENDIF
122600030721     C                   EVAL      PiStr=SkSplitCSV(i)
122700030721     C                   EXSR      CHKNUM
122800030721     C                   IF        PiNum=*on
122900030721     C                   Z-ADD     PiVal         VABCAS
123000030721     C                   ELSE
123100030721     C                   SETON                                        32
123200030721     C                   EVAL      VABCAS = *zeros
123300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123400030721     C                             + ' ' + 'VABCAS'
123500030721     C                   ENDIF
123600030721     C                   ENDIF
123700030721     C*** CCM
123800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
123900030721     C                   EVAL      PiStr=SkSplitCSV(i)
124000030721     C                   EXSR      CHKNUM
124100030721     C                   IF        PiInt=*on      AND
124200030721     C                             PiVal<=9999999 AND
124300030721     C                             PiVal>*zeros
124400030721     C                   Z-ADD     PiVal         VABCCM
124500030721     C                   ELSE
124600030721     C                   SETON                                        32
124700030721     C                   EVAL      VABCCM = *zeros
124800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124900040510     C                             + ' ' + 'VABCCM'
125000030721     C                   ENDIF
125100030721     C                   ENDIF
125200030721     C*** LNP
125300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
125400030721     C                   EVAL      PiStr=SkSplitCSV(i)
125500030721     C                   EXSR      CHKNUM
125600030721     C                   IF        PiInt=*on    AND
125700030721     C                             PiVal<=999   AND
125800030721     C                             PiVal>*zeros
125900030721     C                   Z-ADD     PiVal         VABLNP
126000030721     C                   ELSE
126100040715     C                   SETON                                        32
126200030721     C                   EVAL      VABLNP = *zeros
126300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126400040510     C                             + ' ' + 'VABLNP'
126500030721     C                   ENDIF
126600030721     C                   ENDIF
126700030721     C*** NRS
126800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
126900030721     C                   EVAL      PiStr=SkSplitCSV(i)
127000030721     C                   EXSR      CHKNUM
127100030721     C                   IF        PiInt=*on AND
127200030721     C                             PiVal<=99
127300030721     C                   Z-ADD     PiVal         VABNRS
127400030721     C                   ELSE
127500030721     C                   SETON                                        32
127600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127700030721     C                             + ' ' + 'VABNRS'
127800030721     C                   ENDIF
127900030721     C                   ENDIF
128000030721     C*** LNA
128100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
128200030721     C                   EVAL      PiStr=SkSplitCSV(i)
128300030721     C                   EXSR      CHKNUM
128400030721     C                   IF        PiInt=*on    AND
128500030721     C                             PiVal<=999   AND
128600030721     C                             PiVal>*zeros
128700030721     C                   Z-ADD     PiVal         VABLNA
128800030721     C                   ELSE
128900030721     C                   SETON                                        32
129000030721     C                   EVAL      VABLNA = *zeros
129100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129200030721     C                             + ' ' + 'VABLNA'
129300030721     C                   ENDIF
129400030721     C                   ENDIF
129500030721     C*** CTR
129600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
129700030721     C                   EVAL      PiStr=SkSplitCSV(i)
129800030721     C                   EXSR      CHKNUM
129900030721     C                   IF        PiInt=*on  AND
130000030721     C                             PiVal<=999
130100030721     C                   Z-ADD     PiVal         VABCTR
130200030721     C                   ELSE
130300030721     C                   SETON                                        32
130400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130500030721     C                             + ' ' + 'VABCTR'
130600030721     C                   ENDIF
130700030721     C                   ENDIF
130800030721     C*** IAS
130900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
131000030721     C                   EVAL      PiStr=SkSplitCSV(i)
131100030721     C                   EXSR      CHKNUM
131200030721     C                   IF        PiNum=*on
131300030721     C                   Z-ADD     PiVal         VABIAS
131400030721     C                   ELSE
131500030721     C                   SETON                                        32
131600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131700030721     C                             + ' ' + 'VABIAS'
131800030721     C                   ENDIF
131900030721     C                   ENDIF
132000030721     C*** VLB
132100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
132200030721     C                   EVAL      PiStr=SkSplitCSV(i)
132300030721     C                   EXSR      CHKNUM
132400030721     C                   IF        PiNum=*on
132500030721     C                   Z-ADD     PiVal         VABVLB
132600030721     C                   ELSE
132700030721     C                   SETON                                        32
132800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132900030721     C                             + ' ' + 'VABVLB'
133000030721     C                   ENDIF
133100030721     C                   ENDIF
133200030721     C*** QFT
133300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
133400030721     C                   EVAL      PiStr=SkSplitCSV(i)
133500030721     C                   EXSR      CHKNUM
133600030721     C                   IF        PiNum=*on
133700030721     C                   Z-ADD     PiVal         VABQFT
133800030721     C                   ELSE
133900030721     C                   SETON                                        32
134000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134100030721     C                             + ' ' + 'VABQFT'
134200030721     C                   ENDIF
134300030721     C                   ENDIF
134400030721     C*** NCD
134500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
134600030721     C                   EVAL      PiStr=SkSplitCSV(i)
134700030721     C                   EXSR      CHKNUM
134800030721     C                   IF        PiInt=*on      AND
134900030721     C                             PiVal<=9999999
135000030721     C                   Z-ADD     PiVal         VABNCD
135100030721     C                   ELSE
135200030721     C                   SETON                                        32
135300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135400030721     C                             + ' ' + 'VABNCD'
135500030721     C                   ENDIF
135600030721     C                   ENDIF
135700030721     C*** NCA
135800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
135900030721     C                   EVAL      PiStr=SkSplitCSV(i)
136000030721     C                   EXSR      CHKNUM
136100030721     C                   IF        PiInt=*on      AND
136200030721     C                             PiVal<=9999999
136300030721     C                   Z-ADD     PiVal         VABNCA
136400030721     C                   ELSE
136500030721     C                   SETON                                        32
136600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136700030721     C                             + ' ' + 'VABNCA'
136800030721     C                   ENDIF
136900030721     C                   ENDIF
137000030721     C*** ZNC
137100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
137200030721     C                   EVAL      PiStr=SkSplitCSV(i)
137300030721     C                   EXSR      CHKNUM
137400030721     C                   IF        PiInt=*on AND
137500030721     C                             PiVal<=99
137600030721     C                   Z-ADD     PiVal         VABZNC
137700030721     C                   ELSE
137800030721     C                   SETON                                        32
137900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138000030721     C                             + ' ' + 'VABZNC'
138100030721     C                   ENDIF
138200030721     C                   ENDIF
138300030721     C*** DCR
138400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
138500030721     C                   EVAL      PiStr=SkSplitCSV(i)
138600030721     C                   EXSR      CHKNUM
138700030721     C                   IF        PiInt=*on       AND
138800030721     C                             PiVal<=99999999
138900030721     C                   Z-ADD     PiVal         VABDCR
139000030721     C                   ELSE
139100030721     C                   SETON                                        32
139200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139300030721     C                             + ' ' + 'VABDCR'
139400030721     C                   ENDIF
139500030721     C                   ENDIF
139600030721     C*** HCR
139700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
139800030721     C                   EVAL      PiStr=SkSplitCSV(i)
139900030721     C                   EXSR      CHKNUM
140000030721     C                   IF        PiInt=*on   AND
140100030721     C                             PiVal<=9999
140200030721     C                   Z-ADD     PiVal         VABHCR
140300030721     C                   ELSE
140400030721     C                   SETON                                        32
140500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140600030721     C                             + ' ' + 'VABHCR'
140700030721     C                   ENDIF
140800030721     C                   ENDIF
140900030721     C*** VMD
141000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
141100030721     C                   EVAL      PiStr=SkSplitCSV(i)
141200030721     C                   EXSR      CHKNUM
141300030721     C                   IF        PiNum=*on
141400030721     C                   Z-ADD     PiVal         VABVMD
141500030721     C                   ELSE
141600030721     C                   SETON                                        32
141700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141800030721     C                             + ' ' + 'VABVMD'
141900030721     C                   ENDIF
142000030721     C                   ENDIF
142100030721     C*** ANT
142200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
142300030721     C                   EVAL      PiStr=SkSplitCSV(i)
142400030721     C                   EXSR      CHKNUM
142500030721     C                   IF        PiInt=*on        AND
142600030721     C                             PiVal<=999999999
142700030721     C                   Z-ADD     PiVal         VABANT
142800030721     C                   ELSE
142900030721     C                   SETON                                        32
143000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
143100030721     C                             + ' ' + 'VABANT'
143200030721     C                   ENDIF
143300030721     C                   ENDIF
143400030721     C*** CMO
143500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
143600030721     C                   EVAL      PiStr=SkSplitCSV(i)
143700030721     C                   EXSR      CHKNUM
143800030721     C                   IF        PiInt=*on
143900030721     C                   Z-ADD     PiVal         Num5_0
144000030721     C                   MOVEL(p)  Num5_0        VABCMO
144100030721     C                   ELSE
144200030721     C                   SETON                                        32
144300030721     C                   EVAL      VABCMO = *zeros
144400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
144500030721     C                             + ' ' + 'VABCMO'
144600030721     C                   ENDIF
144700030721     C                   ENDIF
144800040510     C*
144900040510     C* Reperisco i campi del FIVAT
145000040510     C*** VATNOT
145100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
145200040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
145300040510     C                   ENDIF
145400040510     C*** VATTRC
145500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
145600040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
145700040510     C                   ENDIF
145800060331     C*** VATNOT_A
145900060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
146000060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
146100060331     C                   ENDIF
146200060331     C*** VATNOT_B
146300060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
146400060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
146500060331     C                   ENDIF
146600060331     C*** VATNOT_E
146700070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
146800060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
146900060331     C                   ENDIF
147000070823     C*
147100100831     C***  ===>  Gestione campi "particolari" dentro ciclo
147200110201     C*** VATNOT_E
147300110201     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_E'
147400110201     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
147500110201     C                   IF        wVATNOT_E <> *blanks
147600110201     C                   SETOFF                                       35
147700110201     C                   ENDIF
147800110201     C                   ENDIF
147900110131     C*** CAD / PRD
148000110131     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCAD'
148100110131     C                   EVAL      VABCAD = %subst(SkSplitCSV(i):1:5)
148200110131     C                   EVAL      VABPRD = %subst(SkSplitCSV(i):7:2)
148300100831     C                   ENDIF
148400110523     C*** TSP / TC1
148500110131     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABTSP'
148600110131     C                   SELECT
148700110523     C                   WHEN      SkSplitCSV(i) = 'PRIORITY'  OR
148800110523     C                             SkSplitCSV(i) = 'E'
148900110131     C                   EVAL      VABTSP = 'E'
149000110523     C                   WHEN      SkSplitCSV(i) = 'ORE 10.30' OR
149100110523     C                             SkSplitCSV(i) = 'H'
149200110131     C                   EVAL      VABTSP = 'H'
149300110523     C                   WHEN      SkSplitCSV(i) = 'A'
149400110523     C                   EVAL      VABTC1 = 'A'
149500110131     C                   ENDSL
149600110131     C                   ENDIF
149700110131     C*** PKB
149800110131     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABPKB'
149900110131     C                   EVAL      PiStr=SkSplitCSV(i)
150000110131     C                   EXSR      CHKNUM
150100110131     C                   IF        PiNum=*on
150200110131     C                   EVAL      PiVal = PiVal / 1000                         * da gr a kg
150300110131     C                   Z-ADD     PiVal         VABPKB
150400110131     C                   ELSE
150500110131     C                   SETON                                        32
150600110131     C                   EVAL      VABPKB = *zeros
150700110131     C                   EVAL      vinmsg = %trimr(vinmsg)
150800110131     C                             + ' ' + 'VABPKB'
150900110131     C                   ENDIF
151000110131     C                   ENDIF
151100110131     C*** VLB
151200110131     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABVLB'
151300110131     C                   EVAL      PiStr=SkSplitCSV(i)
151400110131     C                   EXSR      CHKNUM
151500110131     C                   IF        PiNum=*on
151600110131     C                   EVAL      PiVal = PiVal / 1000000                      * da cm3 a m3
151700110131     C                   Z-ADD     PiVal         VABVLB
151800110131     C                   ELSE
151900110131     C                   SETON                                        32
152000110131     C                   EVAL      VABVLB = *zeros
152100110131     C                   EVAL      vinmsg = %trimr(vinmsg)
152200110131     C                             + ' ' + 'VABVLB'
152300110131     C                   ENDIF
152400110131     C                   ENDIF
152500110131     C*** NSP / RMN
152600101021     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNSP'
152700110131     C                   EVAL      PiStr=%subst(SkSplitCSV(i):2)
152800101021     C                   EXSR      CHKNUM
152900101021     C                   IF        PiInt=*on
153000101021     C                   Z-ADD     PiVal         VABNSP
153100101021     C                   Z-ADD     PiVal         VABRMN
153200101021     C                   ELSE
153300101021     C                   SETON                                        31
153400101021     C                   EVAL      VABNSP = *zeros
153500101021     C                   EVAL      VABRMN = *zeros
153600101021     C                   EVAL      vinmsg = %trimr(vinmsg)
153700101021     C                             + ' ' + 'VABNSP VABRMN'
153800101021     C                   ENDIF
153900101021     C                   ENDIF
154000070925     C*
154100071029     C***  <===  -----------------------------------------
154200040510     C*
154300030715     C                   ENDIF
154400030715     C                   EVAL      i = i + 1
154500030715     C                   ENDDO
154600070925     C*
154700071029     C***  ===>  Gestione campi "particolari" fuori ciclo
154800100831     C*
154900100831     C                   IF        wVABRSD <> *blanks
155000100831     C                   EVAL      VABRSD = wVABRSD
155100100831     C                   ENDIF
155200100831     C*
155300100831     C                   IF        wVABIND <> *blanks
155400100831     C                   EVAL      VABIND = wVABIND
155500100831     C                   ENDIF
155600100831     C*
155700100831     C                   IF        wVABCAD <> *blanks
155800100831     C                   EVAL      VABCAD = wVABCAD
155900100831     C                   ENDIF
156000100831     C*
156100100831     C                   IF        wVABLOD <> *blanks
156200100831     C                   EVAL      VABLOD = wVABLOD
156300100831     C                   ENDIF
156400100831     C*
156500100831     C                   IF        wVABPRD <> *blanks
156600100831     C                   EVAL      VABPRD = wVABPRD
156700100831     C                   ENDIF
156800070925     C*
156900071029     C***  <===  -----------------------------------------
157000070925     C*
157100070925     C* Considerazioni finali su CBO/CAS
157200100318     C                   IF        wFlgCAS = '1'    OR
157300100318     C                             VABCAS  > *zeros
157400070925     C                   IF        VABCBO = '1'
157500070925     C                   EVAL      VABCBO = '4'
157600070925     C                   ENDIF
157700070925     C                   IF        VABCBO = '2'
157800070925     C                   EVAL      VABCBO = '6'
157900070925     C                   ENDIF
158000070925     C                   ENDIF
158100070925     C***  <===  ----------------------------
158200040510     C*
158300040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
158400040510     C                   IF        VABNSP = *zeros
158500040510     C                   SETON                                        31
158600040510     C                   ENDIF
158700040715     C*
158800040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
158900040715     C                   IF        VABLNP = *zeros
159000040715     C                   SETON                                        31
159100040715     C                   ENDIF
159200040510     C*
159300040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
159400040510     C                   IF        VABRMN = *zeros
159500040510     C                   Z-ADD     VABNSP        VABRMN
159600040510     C                   ENDIF
159700040510     C*
159800040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
159900040510     C                   IF        VABRMA = *blanks
160000040714     C                   MOVEL     VABRMN        VABRMA
160100040510     C                   ENDIF
160200040128     C*
160300040510     C* Se provincia nn valorizzata la reperisco
160400040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
160500040510     C                   IF        VABPRD = *blanks
160600040128     C                   CLEAR                   TISI95DS
160700040128     C                   EVAL      I95TCN = '3'
160800040128     C                   Z-ADD     datcor        I95DAT
160900040128     C                   EVAL      I95NAR = VABNZD
161000040128     C                   EVAL      I95CAP = VABCAD
161100040128     C                   EVAL      I95LOC = VABLOD
161200040128     C                   CALL      'TISI95R'
161300040128     C                   PARM                    TISI95DS
161400040128     C                   EVAL      VABPRD = O95PRV
161500040510     C                   ENDIF
161600020204     C*
161700020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
161800020204     C                   EXSR      CHKIMPDIV
161900030715     C                   ENDIF
162000020204     C*
162100010607     C                   ENDSR
162200010607     C*----------------------------------------------------*
162300010601
162400020204
162500020204     C*----------------------------------------------------*
162600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
162700020204     C*----------------------------------------------------*
162800020204     C     CHKIMPDIV     BEGSR
162900020204     C*
163000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
163100020204     C                   Z-ADD     *zeros        wrkDec            9 9
163200020204     C*
163300020204     C* Come prima cosa effettuo considerazioni sulla divisa
163400020204     C                   IF        vabIAS > *zeros
163500020204     C                   IF        vabVAS <> 'EUR'
163600020204     C                   EVAL      vabVAS =  'ITL'
163700020204     C                   ENDIF
163800020204     C                   ENDIF
163900020204     C*
164000020204     C                   IF        vabCAS > *zeros
164100020204     C                   IF        vabVCA <> 'EUR'
164200020204     C                   EVAL      vabVCA =  'ITL'
164300020204     C                   ENDIF
164400020204     C                   ENDIF
164500020204     C*
164600020204     C                   IF        vabVMD > *zeros
164700020204     C                   IF        vabVAD <> 'EUR'
164800020204     C                   EVAL      vabVAD =  'ITL'
164900020204     C                   ENDIF
165000020204     C                   ENDIF
165100020204     C*
165200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
165300020204     C                   Z-ADD     vabIAS        wrkDec
165400020204     C                   IF        wrkDec > *zeros
165500020204     C                   IF        vabVAS = 'ITL'
165600020204     C                   EVAL      vabIAS = *zeros
165700020204     C                   ENDIF
165800020204     C                   ENDIF
165900020204     C*
166000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
166100020204     C                   Z-ADD     vabCAS        wrkDec
166200020204     C                   IF        wrkDec > *zeros
166300020204     C                   IF        vabVCA = 'ITL'
166400020204     C                   EVAL      vabCAS = *zeros
166500020204     C                   ENDIF
166600020204     C                   ENDIF
166700020204     C*
166800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
166900020204     C                   Z-ADD     vabVMD        wrkDec
167000020204     C                   IF        wrkDec > *zeros
167100020204     C                   IF        vabVAD = 'ITL'
167200020204     C                   EVAL      vabVMD = *zeros
167300020204     C                   ENDIF
167400020204     C                   ENDIF
167500020204     C*
167600020204     C                   ENDSR
167700020204     C***
167800010330
167900100318
168000100318
168100100318     C*----------------------------------------------------*
168200100318     C*  CONTROLLO NUMERICITA' CAMPI
168300100318     C*----------------------------------------------------*
168400100318     C     CHKNUM        BEGSR
168500100318     C*
168600100318     C                   IF        PiDecChr = *blanks
168700100318     C                   EVAL      PiDecChr = CharNUM
168800100318     C                   ENDIF
168900100318     C*
169000100318     C                   callp     UBISNUM_Check(PiStr
169100100318     C                                          :PiDecChr
169200100318     C                                          :PiVal
169300100318     C                                          :PiNum
169400100318     C                                          :PiInt)
169500100318     C*
169600100318     C                   ENDSR
169700100318     C***
169800010601
169900010601
170000010601
170100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
170200010601     C     repfil        BEGSR
170300010601     C*
170400010601     C                   if        invfil = *zeros and
170500010601     C                             depfil > *zeros and
170600010629     C                             (vinflg = *blanks or
170700010629     C                              vinflg = *zeros)
170800010601     C
170900010601     C                   eval      invfil = depfil
171000010601     C                   endif
171100010601     C*
171200010601     C                   if        depfil <> invfil and
171300010601     C                             invfil > *zeros
171400010601     C                   eval      flgMulti = '1'
171500010601     C                   if        vinflg = *blanks
171600010601     C                   add       1             cntNonEl
171700010601     C                   endif
171800010601     C                   endif
171900010601     C*
172000010601     C                   if        vinflg = '2'
172100010601     C                   eval      flgStato = '2'
172200010601     C                   endif
172300010601     C*
172400010601     C                   ENDSR
172500010601     C***
172600010601
172700010601
172800010601
172900010330
173000040119      /TITLE Invio dei dati al punto operativo.
173100040119     C     invio         BEGSR
173200040119     C*
173300040119     C* 1° invio FIVAT
173400040119     C                   reset                   dscmz
173500040119     C                   move      vlrpoi        cmzdst
173600040119     C                   eval      cmzfld = 'FIVATWWR'
173700040119     C                   eval      cmzmbd = vlrhdl
173800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
173900040119     C***                if        prmfir = *blanks
174000040119     C                   eval      cmzfla = 'FIVAT00F'
174100040119     C                   eval      cmzmba = 'FIVAT00F'
174200040119     C***                else
174300040119     C***                eval      cmzfla = prmfir
174400040119     C***                eval      cmzmba = prmfir
174500040119     C***                endif
174600040119     C                   eval      cmznrr = *zeros
174700040119     C                   move      §ctrokvt      cmznrr
174800040119     C                   eval      cmzlba = vlrfl1
174900040119     C                   call(e)   'TIS711C'
175000040119     C                   parm                    dscmz
175100040119     C                   parm      *blanks       esito
175200040119     C                   if        %error
175300040119     C                             or cmzerr = '1'
175400040119     C                             or esito  = '1'
175500040119     C                   eval      wrkesito = '3'
175600040119     C                   else
175700040119     C*
175800040119     C* 2° invio FIVAB
175900040119     C                   reset                   dscmz
176000040119     C                   move      vlrpoi        cmzdst
176100040119     C                   eval      cmzfld = vlrfou
176200040119     C                   eval      cmzmbd = vlrhdl
176300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
176400040119     C***                if        prmfir = *blanks
176500040119     C                   eval      cmzfla = 'FIVAB00F'
176600040119     C                   eval      cmzmba = 'FIVAB00F'
176700040119     C***                else
176800040119     C***                eval      cmzfla = prmfir
176900040119     C***                eval      cmzmba = prmfir
177000040119     C***                endif
177100040119     C                   eval      cmznrr = *zeros
177200040119     C                   move      §ctrokvb      cmznrr
177300040119     C                   eval      cmzlba = vlrfl1
177400040119     C                   call(e)   'TIS711C'
177500040119     C                   parm                    dscmz
177600040119     C                   parm      *blanks       esito
177700040119     C                   if        %error
177800040119     C                             or cmzerr = '1'
177900040119     C                             or esito  = '1'
178000040119     C                   eval      wrkesito = '3'
178100040119     C                   endif
178200040119     C                   endif
178300040119     C*
178400040119     C                   ENDSR
178500040119     C***
178600010601
178700010601
178800010601
178900010601
179000010601      /TITLE Invio dei dati al punto operativo.
179100010601     C     opeini        BEGSR
179200010601     C*
179300010601     C* Inizializzo flag e contatori operativi
179400010601     C                   movel     '0'           flgGiro           1
179500010601     C                   movel     '0'           flgMulti          1
179600010601     C                   movel     '1'           flgStato          1
179700010615     C                   movel     '0'           flgOk             1
179800010601     C                   z-add     *zeros        cntNonEl         10 0
179900010601     C                   z-add     *zeros        depfil            3 0
180000010601     C                   z-add     *zeros        invfil            3 0
180100010601     C*
180200010601     C                   ENDSR
180300010601     C***
180400070326
180500070326
180600070326
180700070326
180800070326     C     *pssr         BEGSR
180900070329     C*
181000070329     C                   if        %open(tivin00r)
181100070329     C                   close     tivin00r
181200070329     C                   endif
181300070329     C                   if        %open(fivabwwr)
181400070329     C                   close     fivabwwr
181500070329     C                   endif
181600070329     C                   if        %open(fivatwwr)
181700070329     C                   close     fivatwwr
181800070329     C                   endif
181900070326     C*
182000070326     C* Effettuo la chiamata al CLLE preposto
182100070326     C                   call(e)   'TITVVTC'
182200070326     C                   parm                    parccm
182300070326     C                   parm                    parmbr
182400070326     C                   parm      '2'           paropz
182500070326     C*
182600070326     C                   eval      wrkesito = '2'
182700070404     C*
182800070404     C                   seton                                        LR
182900070326     C*
183000070326     C                   ENDSR     '*CANCL'
183100070326     C***
183200100318
183300100318
183400100318
183500100318     C     normCSV       BEGSR
183600100318     C*
183700100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
183800100318     C* separatori campi
183900100318     C                   if        CharTXT <> *blanks
184000100318     C                   z-add     *zeros        wLenVINDTA        4 0
184100100318     C                   z-add     *zeros        wFlgVINDTA        1 0
184200100318     C                   setoff                                       80
184300100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
184400100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
184500100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
184600100318     C                   if        not *in80
184700100318     C                   seton                                        80
184800100318     C                   else
184900100318     C                   setoff                                       80
185000100318     C                   endif
185100100318     C                   endif
185200100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
185300100318     C                             and *in80 = *on
185400100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
185500100318     C                   endif
185600100318     C                   enddo
185700100318     C                   endif
185800100318     C*
185900100318     C                   ENDSR
186000100318     C***
186100070326
186200100326
186300100326
186400100326     C*----------------------------------------------------*
186500100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
186600100326     C*----------------------------------------------------*
186700100326     C     REPNSP        BEGSR
186800100326     C*
186900100326     C* NSP => Stacco un numeratore da AZNUM
187000100326     C                   clear                   TRUL33DS
187100100326     C                   eval      I33OPE = *zeros
187200100326     C                   eval      I33CNU = 302
187300100326     C                   eval      I33NUM = 1
187400100326     C                   movel     TRUL33DS      KPJBU
187500100326     C                   call      'TRUL33R'
187600100326     C                   parm                    KPJBA
187700100326     C                   movel     KPJBU         TRUL33DS
187800100326     C                   if        O33ERR = *zeros
187900100326     C                   z-add     O33NRF        VABNSP
188000100326     C                   z-add     O33NRF        VATNSP
188100100326     C                   else
188200100326     C                   SETON                                        31
188300100326     C                   EVAL      vinmsg = %trimr(vinmsg)
188400100326     C                             + ' ' + 'VABNSP VATNSP'
188500100326     C                   endif
188600100326     C*
188700100326     C                   ENDSR
188800010330
188900100326
189000010330
189100000613     C     *inzsr        BEGSR
189200990910     C*
189300990910     C     *entry        plist
189400990920     C                   parm                    tivlrds
189500990921     C                   parm      wrkesito      esito
189600000724     C                   parm                    prmlit
189700000710     C                   parm                    prmfir
189800010330     C*
189900010330     C* CALCOLA LA DATA CORRENTE
190000100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
190100100318     C                   eval      datcor = %dec(%date() : *ISO)
190200000613     C*
190300000613     C                   ENDSR
190400000613     C***
190500990908
190600110131     Otitv2c3p  E            riepilogo         2
190700990915     O                                              'Upload via Internet'
190800990915     O                                           +1 'Traduzione TIVIN00R -'
190900021025     O                                           +1 'FIVABWWR'
191000990915     O                                           +1 'Testate bolle'
191100990915     O          E            riepilogo   2
191200990915     O                       wrkdata
191300990915     O                       wrkora              +1
191400990915     O                       procname            +1
191500990915     O          E            riepilogo   2
191600990915     O                                              'Cliente..................:'
191700010605     O                       VABCCM        z     +1
191800990915     O          E            riepilogo   2
191900990920     O                                              'Riferimento Strategi.....:'
192000990920     O                       vlrhdl              +1
192100990915     O          E            riepilogo   2
192200990915     O                                              'Giusti...................:'
192300040119     O                       §CTROKVB      2   +  1
192400990915     O          E            riepilogo   2
192500990915     O                                              'Sbagliati e corretti.....:'
192600971022     O                       §CTRMO        2   +  1
192700990915     O          E            riepilogo   2
192800990915     O                                              'Sbagliati e scartati.....:'
192900971022     O                       §CTRNO        2   +  1
193000000613
193100110131     Otitv2c3ps E            testdett          2
193200000613     O                                              'Upload via Internet'
193300000613     O                                           +1 'Traduzione TIVIN00R -'
193400021025     O                                           +1 'FIVABWWR'
193500000613     O                                           +1 'Testate bolle'
193600000616     O          E            testdett    3
193700000613     O                                           +2 'N° rec'
193800000613     O                                           +3 'Anteprima contenuto'
193900000616     O          E            rigadett    2
194000000613     O                       rrnum               +2
194100000621     O                       recko               +3
194200000616     O          E            findett     2
194300000613     O                       wrkdata
194400000613     O                       wrkora              +1
194500000613     O                       procname            +1
194600000616     O          E            findett     2
194700000613     O                                              'Cliente..................:'
194800010605     O                       VABCCM        z     +1
194900000616     O          E            findett     2
195000000613     O                                              'Riferimento Strategi.....:'
195100000613     O                       vlrhdl              +1
195200000616     O          E            findett     2
195300000613     O                                              'Giusti...................:'
195400040119     O                       §CTROKVB      2   +  1
195500000616     O          E            findett     2
195600000613     O                                              'Sbagliati e corretti.....:'
195700000613     O                       §CTRMO        2   +  1
195800000616     O          E            findett     2
195900000613     O                                              'Sbagliati e scartati.....:'
196000000613     O                       §CTRNO        2   +  1
196100000613** CMD - COMANDI CL
196200110131OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV2C3P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
196300110131OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV2C3PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
196400110131DLTOVR FILE(TITV2C3P TITV2C3PS) LVL(*)
196500010606
196600000613
196700040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
196800110714FIL001;W_VATNOT_E;FIL002;VABRMO;FIL003;FIL004;;FIL005;W_VABNSP;FIL006;VABRMA;
196900110201VABNCL;W_VABPKB;W_VABVLB;FIL010;VABRSD;VABIND;VABRD2;W_VABCAD;VABLOD;FIL008;
197000110201VATNOT_B;W_VABTSP;FIL009;VABNOT
197100101018
197200100318
