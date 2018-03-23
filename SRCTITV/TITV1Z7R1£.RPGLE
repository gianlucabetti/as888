000100110926      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200100318     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600110926     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
000700110926     FFIVATwwr  O    E             DISK    usropn
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
003100110926     D fivab0ds      e ds                  extname(fivab00f)
003200110926     D fivab0ds_sav  e ds                  extname(fivab00f) prefix(sav_)
003300110926     D fivab0ds_ok   e ds                  extname(fivab00f) prefix(ok_)
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
016700110927     C                   call(e)   'TITVVTC'
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
021600110926     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
021700010330     C*--------------------------------------------------------
021800010605     C     RWVAB         BEGSR
021900010330     C*
022000010330     C                   if        not %open(tivin00r)
022100010330     C                   open      tivin00r
022200010330     C                   endif
022300110926     C                   if        not %open(fivabwwr)
022400110926     C                   open      fivabwwr
022500010330     C                   endif
022600040119     C*
022700110926     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
022800040119     C                   exsr      prevat
022900040119     C*
023000040119     C                   if        chkcall = '0'
023100040119     C*
023200110926     C                   if        not %open(fivatwwr)
023300110926     C                   open      fivatwwr
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
036400110926     C                   if        %open(fivabwwr)
036500110926     C                   close     fivabwwr
036600010330     C                   endif
036700110926     C                   if        %open(fivatwwr)
036800110926     C                   close     fivatwwr
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
039600110926     C  N31              eval      fivab0ds_sav = fivab0ds
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
041000110926     C  N31              eval      fivab0ds_ok = fivab0ds_sav
041100080527     C  N31              exsr      wrivab
041200110926     C  N31              eval      fivab0ds_sav = fivab0ds
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
043000110928     C                   eval      VABCTM = '7T'
043100070301     C                   endif
043200110926     C                   write     fivab000
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
045100040714     C*
045200040714     C* Solo se VATNOT valorizzato scarico il buffer
045300040714     C                   if        VATNOT <> *blanks
045400040119     C*
045500040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
045600040510     C                   if        VATTRC = *blanks
045700040510     C                   eval      VATTRC = 'E'
045800040510     C                   endif
045900110926     C                   write     FIVAT000
046000040119     C                   add       1             §CTROKVT
046100040714     C*
046200040714     C                   endif
046300060331     C*
046400060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
046500060331     C                   if        wVATNOT_A <> *blanks
046600060331     C                   eval      VATNOT = wVATNOT_A
046700060331     C                   eval      VATTRC = 'A'
046800110926     C                   write     FIVAT000
046900060331     C                   add       1             §CTROKVT
047000060331     C                   endif
047100060331     C*
047200060331     C                   if        wVATNOT_B <> *blanks
047300060331     C                   eval      VATNOT = wVATNOT_B
047400060331     C                   eval      VATTRC = 'B'
047500110926     C                   write     FIVAT000
047600060331     C                   add       1             §CTROKVT
047700060331     C                   endif
047800060331     C*
047900060331     C                   if        wVATNOT_E <> *blanks
048000060331     C                   eval      VATNOT = wVATNOT_E
048100060331     C                   eval      VATTRC = 'E'
048200110926     C                   write     FIVAT000
048300060331     C                   add       1             §CTROKVT
048400060331     C                   endif
048500040119     C*
048600040119     C                   ENDSR
048700040119
048800040119
048900040119
049000040119     C*----------------------------------------------------*
049100110926     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
049200040119     C*----------------------------------------------------*
049300040119     C     PREVAT        BEGSR
049400040119     C*
049500110926     C* Compongo il nome del membro da dare al FIVATWWR
049600040119     C                   eval      parmbr = vlrhdl
049700040119     C                   movel     'M'           parmbr
049800040510     C                   eval      parccm = vlrksc
049900040119     C                   eval      paropz = '1'
050000040119     C* Effettuo la chiamata al CLLE preposto
050100110927     C                   call(e)   'TITVVTC'
050200040119     C                   parm                    parccm
050300040119     C                   parm                    parmbr
050400040119     C                   parm                    paropz
050500040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
050600040119     C                   if        %error
050700040119     C                   movel     '1'           chkcall
050800040119     C                   else
050900040119     C                   movel     '0'           chkcall
051000040119     C                   endif
051100040119     C*
051200040119     C                   ENDSR
051300010601
051400010601
051500010601
051600010330     C*----------------------------------------------------*
051700030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
051800010330     C*----------------------------------------------------*
051900040119     C     INZVAR        BEGSR
052000080527     C*
052100080527     C                   setoff                                       65
052200010330     C*
052300030822     C* Inizializzo variabili di wrk
052400020204     C                   Z-ADD     *zeros        Num5_0            5 0
052500060331     C                   MOVEL     *blanks       wVATNOT_A
052600060331     C                   MOVEL     *blanks       wVATNOT_B
052700060331     C                   MOVEL     *blanks       wVATNOT_E
052800030822     C*
052900030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
053000030822     C                   CLEAR                   SkSplitCSV
053100030822     C                   EVAL      i = 1
053200030822     C                   EVAL      posDa = *zeros
053300030822     C                   EVAL      posA  = *zeros
053400080527     C*
053500110926     C                   CLEAR                   FIVAB0DS
053600110926     C                   CLEAR                   FIVAT000
053700030822     C*
053800030822     C* Reimposto i valori di default
053900030822     C                   EXSR      DEFCAM
054000030822     C*
054100010330     C                   ENDSR
054200010330     C*----------------------------------------------------*
054300030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
054400010330     C*----------------------------------------------------*
054500010330     C     DEFCAM        BEGSR
054600030822     C*
054700030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
054800030715     C* e delimitatore testo.
054900030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
055000030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
055100030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
055200100318     C*
055300100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
055400100318     C                   EVAL      CharSOS = CharNUM
055500030822     C*
055600030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
055700100720     C                   SETOFF                                       505152
055800100720     C                   SETOFF                                       53
055900100413     C                   SELECT
056000100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
056100100720     C                   SETON                                        50
056200100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
056300100413     C                   SETON                                        51
056400100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
056500100720     C                   SETON                                        52
056600100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
056700100720     C                   SETON                                        53
056800100413     C                   ENDSL
056900030822     C*
057000030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
057100030822     C                   EVAL      posDaDft = 1
057200030822     C                   EVAL      posADft  = 0
057300030822     C                   EVAL      wGiroDft = 0
057400030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
057500030822     C                             posDaDft > 0
057600030822     C*
057700030822     C* Gestisco il 1° giro
057800030822     C                   IF        wGiroDft = 0
057900030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
058000030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
058100030822     C* Incremento il contatore dei "giri"
058200030822     C                   EVAL      wGiroDft = 1
058300030822     C                   ELSE
058400030822     C                   EVAL      posDaDft = posADft
058500030822     C                   ENDIF
058600030822     C* Eseguo lo scan x trovare la fine del campo corrente
058700030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
058800030822     C*
058900030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
059000030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
059100030822     C                   IF        posDaDft > 0 AND
059200030822     C                             posADft  > 0
059300030822     C* NCL
059400030822     C                   IF        %subst(
059500030822     C                             %subst(vlrppt:posDaDft+1:
059600030822     C                             posADft-posDaDft-1):1:3)
059700030822     C                             = 'NCL'
059800030822     C                   EVAL      PiStr=%trim(%subst(
059900030822     C                             %subst(vlrppt:posDaDft+1:
060000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060100030822     C                   EXSR      CHKNUM
060200030822     C                   IF        PiInt=*on
060300030822     C                   Z-ADD     PiVal         VABNCL
060400030822     C                   ENDIF
060500030822     C                   ENDIF
060600030822     C* CCM
060700030822     C                   IF        %subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):1:3)
061000030822     C                             = 'CCM'
061100030822     C                   EVAL      PiStr=%trim(%subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061400030822     C                   EXSR      CHKNUM
061500030822     C                   IF        PiInt=*on
061600030822     C                   Z-ADD     PiVal         VABCCM
061700030822     C                   ENDIF
061800030822     C                   ENDIF
061900030822     C* LNP
062000030822     C                   IF        %subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):1:3)
062300030822     C                             = 'LNP'
062400030822     C                   EVAL      PiStr=%trim(%subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062700030822     C                   EXSR      CHKNUM
062800030822     C                   IF        PiInt=*on
062900030822     C                   Z-ADD     PiVal         VABLNP
063000030822     C                   ENDIF
063100030822     C                   ENDIF
063200030822     C* NRS
063300030822     C                   IF        %subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):1:3)
063600030822     C                             = 'NRS'
063700030822     C                   EVAL      PiStr=%trim(%subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064000030822     C                   EXSR      CHKNUM
064100030822     C                   IF        PiInt=*on
064200030822     C                   Z-ADD     PiVal         VABNRS
064300030822     C                   ENDIF
064400030822     C                   ENDIF
064500030822     C* CTR
064600030822     C                   IF        %subst(
064700030822     C                             %subst(vlrppt:posDaDft+1:
064800030822     C                             posADft-posDaDft-1):1:3)
064900030822     C                             = 'CTR'
065000030822     C                   EVAL      PiStr=%trim(%subst(
065100030822     C                             %subst(vlrppt:posDaDft+1:
065200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065300030822     C                   EXSR      CHKNUM
065400030822     C                   IF        PiInt=*on
065500030822     C                   Z-ADD     PiVal         VABCTR
065600030822     C                   ENDIF
065700030822     C                   ENDIF
065800030822     C* PKB
065900030822     C                   IF        %subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):1:3)
066200030822     C                             = 'PKB'
066300030822     C                   EVAL      PiStr=%trim(%subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066600030822     C                   EXSR      CHKNUM
066700030822     C                   IF        PiNum=*on
066800030822     C                   Z-ADD     PiVal         VABPKB
066900030822     C                   ENDIF
067000030822     C                   ENDIF
067100030822     C* VLB
067200030822     C                   IF        %subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):1:3)
067500030822     C                             = 'VLB'
067600030822     C                   EVAL      PiStr=%trim(%subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067900030822     C                   EXSR      CHKNUM
068000030822     C                   IF        PiNum=*on
068100030822     C                   Z-ADD     PiVal         VABVLB
068200030822     C                   ENDIF
068300030822     C                   ENDIF
068400030822     C* QFT
068500030822     C                   IF        %subst(
068600030822     C                             %subst(vlrppt:posDaDft+1:
068700030822     C                             posADft-posDaDft-1):1:3)
068800030822     C                             = 'QFT'
068900030822     C                   EVAL      PiStr=%trim(%subst(
069000030822     C                             %subst(vlrppt:posDaDft+1:
069100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069200030822     C                   EXSR      CHKNUM
069300030822     C                   IF        PiNum=*on
069400030822     C                   Z-ADD     PiVal         VABQFT
069500030822     C                   ENDIF
069600030822     C                   ENDIF
069700030822     C* CBO
069800030822     C                   IF        %subst(
069900030822     C                             %subst(vlrppt:posDaDft+1:
070000030822     C                             posADft-posDaDft-1):1:3)
070100030822     C                             = 'CBO'
070200030822     C                   EVAL      VABCBO=%trim(%subst(
070300030822     C                             %subst(vlrppt:posDaDft+1:
070400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070500030822     C                   ENDIF
070600030822     C* TSP
070700030822     C                   IF        %subst(
070800030822     C                             %subst(vlrppt:posDaDft+1:
070900030822     C                             posADft-posDaDft-1):1:3)
071000030822     C                             = 'TSP'
071100030822     C                   EVAL      VABTSP=%trim(%subst(
071200030822     C                             %subst(vlrppt:posDaDft+1:
071300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071400030822     C                   ENDIF
071500030822     C* VAS
071600030822     C                   IF        %subst(
071700030822     C                             %subst(vlrppt:posDaDft+1:
071800030822     C                             posADft-posDaDft-1):1:3)
071900030822     C                             = 'VAS'
072000030822     C                   EVAL      VABVAS=%trim(%subst(
072100030822     C                             %subst(vlrppt:posDaDft+1:
072200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072300030822     C                   ENDIF
072400030822     C* VCA
072500030822     C                   IF        %subst(
072600030822     C                             %subst(vlrppt:posDaDft+1:
072700030822     C                             posADft-posDaDft-1):1:3)
072800030822     C                             = 'VCA'
072900030822     C                   EVAL      VABVCA=%trim(%subst(
073000030822     C                             %subst(vlrppt:posDaDft+1:
073100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073200030822     C                   ENDIF
073300070925     C* TIC
073400070925     C                   IF        %subst(
073500070925     C                             %subst(vlrppt:posDaDft+1:
073600070925     C                             posADft-posDaDft-1):1:3)
073700070925     C                             = 'TIC'
073800070925     C                   EVAL      VABTIC=%trim(%subst(
073900070925     C                             %subst(vlrppt:posDaDft+1:
074000070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074100070925     C                   ENDIF
074200030822     C* GCA
074300030822     C                   IF        %subst(
074400030822     C                             %subst(vlrppt:posDaDft+1:
074500030822     C                             posADft-posDaDft-1):1:3)
074600030822     C                             = 'GCA'
074700030822     C                   EVAL      VABGCA=%trim(%subst(
074800030822     C                             %subst(vlrppt:posDaDft+1:
074900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075000030822     C                   ENDIF
075100030822     C* CTM
075200030822     C                   IF        %subst(
075300030822     C                             %subst(vlrppt:posDaDft+1:
075400030822     C                             posADft-posDaDft-1):1:3)
075500030822     C                             = 'CTM'
075600030822     C                   EVAL      VABCTM=%trim(%subst(
075700030822     C                             %subst(vlrppt:posDaDft+1:
075800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075900030822     C                   ENDIF
076000030822     C* FFD
076100030822     C                   IF        %subst(
076200030822     C                             %subst(vlrppt:posDaDft+1:
076300030822     C                             posADft-posDaDft-1):1:3)
076400030822     C                             = 'FFD'
076500030822     C                   EVAL      VABFFD=%trim(%subst(
076600030822     C                             %subst(vlrppt:posDaDft+1:
076700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076800030822     C                   ENDIF
076900030822     C* VAD
077000030822     C                   IF        %subst(
077100030822     C                             %subst(vlrppt:posDaDft+1:
077200030822     C                             posADft-posDaDft-1):1:3)
077300030822     C                             = 'VAD'
077400030822     C                   EVAL      VABVAD=%trim(%subst(
077500030822     C                             %subst(vlrppt:posDaDft+1:
077600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077700030822     C                   ENDIF
077800030822     C* GMA
077900030822     C                   IF        %subst(
078000030822     C                             %subst(vlrppt:posDaDft+1:
078100030822     C                             posADft-posDaDft-1):1:3)
078200030822     C                             = 'GMA'
078300030822     C                   EVAL      VABGMA=%trim(%subst(
078400030822     C                             %subst(vlrppt:posDaDft+1:
078500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078600030822     C                   ENDIF
078700030822     C* GGA
078800030822     C                   IF        %subst(
078900030822     C                             %subst(vlrppt:posDaDft+1:
079000030822     C                             posADft-posDaDft-1):1:3)
079100030822     C                             = 'GGA'
079200030822     C                   EVAL      VABGGA=%trim(%subst(
079300030822     C                             %subst(vlrppt:posDaDft+1:
079400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079500030822     C                   ENDIF
079600030822     C* GVA
079700030822     C                   IF        %subst(
079800030822     C                             %subst(vlrppt:posDaDft+1:
079900030822     C                             posADft-posDaDft-1):1:3)
080000030822     C                             = 'GVA'
080100030822     C                   EVAL      VABGVA=%trim(%subst(
080200030822     C                             %subst(vlrppt:posDaDft+1:
080300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080400030822     C                   ENDIF
080500030822     C* TC1
080600030822     C                   IF        %subst(
080700030822     C                             %subst(vlrppt:posDaDft+1:
080800030822     C                             posADft-posDaDft-1):1:3)
080900030822     C                             = 'TC1'
081000030822     C                   EVAL      VABTC1=%trim(%subst(
081100030822     C                             %subst(vlrppt:posDaDft+1:
081200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081300030822     C                   ENDIF
081400030822     C* TC2
081500030822     C                   IF        %subst(
081600030822     C                             %subst(vlrppt:posDaDft+1:
081700030822     C                             posADft-posDaDft-1):1:3)
081800030822     C                             = 'TC2'
081900030822     C                   EVAL      VABTC2=%trim(%subst(
082000030822     C                             %subst(vlrppt:posDaDft+1:
082100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082200030822     C                   ENDIF
082300040714     C* VATTRC
082400040714     C                   IF        %subst(
082500040714     C                             %subst(vlrppt:posDaDft+1:
082600040714     C                             posADft-posDaDft-1):1:3)
082700040714     C                             = 'TRC'
082800040714     C                   EVAL      VATTRC=%trim(%subst(
082900040714     C                             %subst(vlrppt:posDaDft+1:
083000040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083100040714     C                   ENDIF
083200030822     C* ...
083300030822     C                   ENDIF
083400030822     C                   ENDDO
083500020204     C*
083600010330     C                   ENDSR
083700010607     C*----------------------------------------------------*
083800110926     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
083900010607     C*----------------------------------------------------*
084000010607     C     IMPVAB        BEGSR
084100010607     C*
084200010607     C                   SETOFF                                       3132
084300030515     C*
084400010607     C                   MOVEL     datcor        VABAAS
084500010607     C                   MOVE      datcor        VABMGS
084600010607     C*
084700030715     C**********
084800030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
084900030715     C**********
085000030715     C                   IF        wGiro = *zeros
085100040113     C*
085200040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
085300040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
085400080429     C                             %trim(intcol(3))+%trim(intcol(4))+
085500080429     C                             %trim(intcol(5))<>*blanks
085600040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
085700080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
085800080429     C                                      %trim(intcol(5))
085900070823     C     ';':CharCSV   XLATE     vindta        vindta
086000040510     C                   ENDIF
086100031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
086200031201     C     minu:maiu     XLATE     vindta        vindta
086300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
086400030715     C                   DOW       posDa <= %len(%trim(vindta))
086500030715     C*
086600030715     C* Gestisco il 1° campo
086700030715     C                   IF        i = 1
086800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
086900030715     C                   EVAL      posDa = 1
087000030715     C* Eseguo lo scan x trovare la fine del primo campo
087100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087200030715     C* A questo punto "estraggo" il campo corrente
087300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087400030715     C                                                    (posA-posDa))
087500030715     C* X i campi successivi al 1°
087600030715     C                   ELSE
087700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
087800030715     C                   EVAL      posDa = posA + 1
087900030715     C* Eseguo lo scan x trovare la fine del campo corrente
088000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088100030715     C* Gestisco l'ultimo campo
088200030715     C                   IF        posA = *zeros
088300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
088400030715     C* A questo punto "estraggo" il campo corrente
088500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088600030715     C                                                    (posA-posDa))
088700030715     C                   LEAVE
088800030715     C                   ELSE
088900030715     C* A questo punto "estraggo" il campo corrente
089000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089100030715     C                                                    (posA-posDa))
089200030715     C                   ENDIF
089300030715     C                   ENDIF
089400030715     C* Incremento il contatore d campo
089500030715     C                   EVAL      i = i +1
089600030715     C                   ENDDO
089700030715     C*
089800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
089900030715     C* delimitazione testo
090000030715     C                   EVAL      i = 1
090100030715     C                   DOW       i < %elem(SkSplitFLD)
090200030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
090300030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
090400030715     C                   EVAL      i = i + 1
090500030715     C                   ENDDO
090600030715     C                   EVAL      wGiro = 1
090700030715     C                   ELSE
090800030715     C                   EVAL      wGiro = 2
090900031204     C**********
091000031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
091100031204     C* d separatore campo
091200031204     C**********
091300031204     C                   EVAL      vindta = %trim(vindta)
091400031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
091500031204     C                   EVAL      vindta = %subst(vindta:2)
091600031204     C                   ENDDO
091700031204     C*
091800031204     C                   Z-ADD     *zeros        lunghInput        4 0
091900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
092000031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
092100031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
092200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
092300031204     C                   ENDDO
092400030715     C**********
092500030715     C* Effettuo lo split del campo dati d input
092600030715     C**********
092700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
092800030715     C                   DOW       posDa <= %len(%trim(vindta))
092900030715     C*
093000030715     C* Gestisco il 1° campo
093100030715     C                   IF        i = 1
093200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
093300030715     C                   EVAL      posDa = 1
093400030715     C* Eseguo lo scan x trovare la fine del primo campo
093500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
093600030715     C* A questo punto "estraggo" il campo corrente
093700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093800030715     C                                                    (posA-posDa))
093900030715     C* X i campi successivi al 1°
094000030715     C                   ELSE
094100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
094200030715     C                   EVAL      posDa = posA + 1
094300031204     C* Verifico che nn vi sia il campo nn valorizzato
094400031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
094500031204     C* Se campo nn valorizzato skippo al prossimo
094600031204     C                   EVAL      SkSplitCSV(i) = *blanks
094700031204     C                   EVAL      posA  = posA + 1
094800031204     C                   ELSE
094900030715     C* Eseguo lo scan x trovare la fine del campo corrente
095000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
095100030715     C* Gestisco l'ultimo campo
095200030715     C                   IF        posA = *zeros
095300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
095400030715     C* A questo punto "estraggo" il campo corrente
095500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095600030715     C                                                    (posA-posDa))
095700030715     C                   LEAVE
095800030715     C                   ELSE
095900030715     C* A questo punto "estraggo" il campo corrente
096000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096100030715     C                                                    (posA-posDa))
096200030715     C                   ENDIF
096300031204     C                   ENDIF
096400030715     C                   ENDIF
096500030715     C* Incremento il contatore d campo
096600030715     C                   EVAL      i = i +1
096700030715     C                   ENDDO
096800030715     C*
096900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
097000030715     C* delimitazione testo
097100030715     C                   EVAL      i = 1
097200030715     C                   DOW       i < %elem(SkSplitCSV)
097300030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
097400030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
097500030715     C                   EVAL      i = i + 1
097600030715     C                   ENDDO
097700100720     C*
097800100720     C* Stacco un progressivo x numero spedizione
097900100720 xxx C***                EXSR      repNSP
098000100326     C*
098100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
098200030715     C                   EVAL      i = 1
098300030715     C                   DOW       i < %elem(SkSplitFLD)
098400030715     C*
098500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
098600030715     C                   IF        SkSplitFLD(i) <> *blanks AND
098700030715     C                             SkSplitCSV(i) <> *blanks
098800030721     C*** ATB
098900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
099000030721     C                   EVAL      VABATB = SkSplitCSV(i)
099100030721     C                   ENDIF
099200030715     C*** RSD
099300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
099400030715     C                   EVAL      VABRSD = SkSplitCSV(i)
099500030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
099600030715     C     '@':'A'       XLATE     VABRSD        VABRSD
099700030715     C* ==
099800030715     C                   ENDIF
099900030721     C*** RD2
100000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
100100030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
100200030721     C                   ENDIF
100300030715     C*** IND
100400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
100500030715     C                   EVAL      VABIND = SkSplitCSV(i)
100600030715     C                   ENDIF
100700030715     C*** LOD
100800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
100900030715     C                   EVAL      VABLOD = SkSplitCSV(i)
101000030715     C                   ENDIF
101100030715     C*** PRD
101200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
101300030715     C                   EVAL      VABPRD = SkSplitCSV(i)
101400030715     C                   ENDIF
101500030721     C*** NZD
101600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
101700030721     C                   EVAL      VABNZD = SkSplitCSV(i)
101800030721     C                   ENDIF
101900030715     C*** RMA
102000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
102100030715     C                   EVAL      VABRMA = SkSplitCSV(i)
102200030715     C                   ENDIF
102300030715     C*** NOT
102400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
102500030715     C                   EVAL      VABNOT = SkSplitCSV(i)
102600030715     C                   ENDIF
102700030715     C*** NT2
102800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
102900030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
103000030715     C                   ENDIF
103100030715     C*** VCA
103200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
103300030715     C                   EVAL      VABVCA = SkSplitCSV(i)
103400030715     C                   ENDIF
103500030715     C*** VAS
103600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
103700030715     C                   EVAL      VABVAS = SkSplitCSV(i)
103800030715     C                   ENDIF
103900030715     C*** FFD
104000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
104100030715     C                   EVAL      VABFFD = SkSplitCSV(i)
104200030715     C                   ENDIF
104300030721     C*** GC1
104400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
104500030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
104600030721     C                   ENDIF
104700030721     C*** GC2
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
104900030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
105000030721     C                   ENDIF
105100030721     C*** TSP
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
105300030721     C                   EVAL      VABTSP = SkSplitCSV(i)
105400030721     C                   ENDIF
105500040510     C*** CBO
105600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
105700040510     C                   EVAL      VABCBO = SkSplitCSV(i)
105800040510     C                   ENDIF
105900030721     C*** NAS
106000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
106100030721     C                   EVAL      VABNAS = SkSplitCSV(i)
106200030721     C                   ENDIF
106300030721     C*** TIC
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
106500030721     C                   EVAL      VABTIC = SkSplitCSV(i)
106600030721     C                   ENDIF
106700030721     C*** GCA
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
106900030721     C                   EVAL      VABGCA = SkSplitCSV(i)
107000030721     C                   ENDIF
107100030721     C*** XCO
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
107300030721     C                   EVAL      VABXCO = SkSplitCSV(i)
107400030721     C                   ENDIF
107500030721     C*** CTM
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
107700030721     C                   EVAL      VABCTM = SkSplitCSV(i)
107800030721     C                   ENDIF
107900030721     C*** TCR
108000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
108100030721     C                   EVAL      VABTCR = SkSplitCSV(i)
108200030721     C                   ENDIF
108300030721     C*** CTS
108400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
108500030721     C                   EVAL      VABCTS = SkSplitCSV(i)
108600030721     C                   ENDIF
108700030721     C*** FTM
108800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
108900030721     C                   EVAL      VABFTM = SkSplitCSV(i)
109000030721     C                   ENDIF
109100030721     C*** VAD
109200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
109300030721     C                   EVAL      VABVAD = SkSplitCSV(i)
109400030721     C                   ENDIF
109500030721     C*** GMA
109600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
109700030721     C                   EVAL      VABGMA = SkSplitCSV(i)
109800030721     C                   ENDIF
109900030721     C*** GGA
110000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
110100030721     C                   EVAL      VABGGA = SkSplitCSV(i)
110200030721     C                   ENDIF
110300030721     C*** GVA
110400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
110500030721     C                   EVAL      VABGVA = SkSplitCSV(i)
110600030721     C                   ENDIF
110700030721     C*** TC1
110800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
110900030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
111000030721     C                   ENDIF
111100030721     C*** TC2
111200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
111300030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
111400030721     C                   ENDIF
111500030721     C*** SCL
111600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
111700030721     C                   EVAL      VABSCL = SkSplitCSV(i)
111800030721     C                   ENDIF
111900030721     C*** RMO
112000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
112100030721     C                   EVAL      VABRMO = SkSplitCSV(i)
112200030721     C                   ENDIF
112300030721     C*** NMO
112400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
112500030721     C                   EVAL      VABNMO = SkSplitCSV(i)
112600030721     C                   ENDIF
112700030721     C*
112800030721     C* Reperisco quindi i campi numerici...
112900040510     C*** NSP
113000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
113100040510     C                   EVAL      PiStr=SkSplitCSV(i)
113200040510     C                   EXSR      CHKNUM
113300040510     C                   IF        PiInt=*on
113400040510     C                   Z-ADD     PiVal         VABNSP
113500040510     C                   ELSE
113600040510     C                   SETON                                        31
113700040510     C                   EVAL      VABNSP = *zeros
113800040510     C                   EVAL      vinmsg = %trimr(vinmsg)
113900040510     C                             + ' ' + 'VABNSP'
114000040510     C                   ENDIF
114100040510     C                   ENDIF
114200071106     C*** RMN
114300071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
114400071106     C                   EVAL      PiStr=SkSplitCSV(i)
114500071106     C                   EXSR      CHKNUM
114600071106     C                   IF        PiInt=*on
114700071106     C                   Z-ADD     PiVal         VABRMN
114800071106     C                   ELSE
114900071106     C                   SETON                                        32
115000071106     C                   EVAL      VABRMN = *zeros
115100071106     C                   EVAL      vinmsg = %trimr(vinmsg)
115200071106     C                             + ' ' + 'VABRMN'
115300071106     C                   ENDIF
115400071106     C                   ENDIF
115500030721     C*** CAD
115600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
115700030721     C                   EVAL      PiStr=SkSplitCSV(i)
115800030721     C                   EXSR      CHKNUM
115900030721     C                   IF        PiInt=*on
116000030721     C                   Z-ADD     PiVal         Num5_0
116100030721     C                   MOVEL(p)  Num5_0        VABCAD
116200030721     C                   ELSE
116300030721     C                   SETON                                        32
116400030721     C                   EVAL      VABCAD = *zeros
116500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116600030721     C                             + ' ' + 'VABCAD'
116700030721     C                   ENDIF
116800030721     C                   ENDIF
116900030721     C*** NCL
117000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
117100030721     C                   EVAL      PiStr=SkSplitCSV(i)
117200030721     C                   EXSR      CHKNUM
117300030721     C                   IF        PiInt=*on
117400030721     C                   Z-ADD     PiVal         VABNCL
117500030721     C                   ELSE
117600030721     C                   SETON                                        32
117700030721     C                   EVAL      VABNCL = *zeros
117800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117900040122     C                             + ' ' + 'VABNCL'
118000030721     C                   ENDIF
118100030721     C                   ENDIF
118200030721     C*** PKB
118300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
118400030721     C                   EVAL      PiStr=SkSplitCSV(i)
118500030721     C                   EXSR      CHKNUM
118600030721     C                   IF        PiNum=*on
118700030721     C                   Z-ADD     PiVal         VABPKB
118800030721     C                   ELSE
118900030721     C                   SETON                                        32
119000030721     C                   EVAL      VABPKB = *zeros
119100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119200030721     C                             + ' ' + 'VABPKB'
119300030721     C                   ENDIF
119400030721     C                   ENDIF
119500030721     C*** CAS
119600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
119700030721     C                   EVAL      PiStr=SkSplitCSV(i)
119800030721     C                   EXSR      CHKNUM
119900030721     C                   IF        PiNum=*on
120000030721     C                   Z-ADD     PiVal         VABCAS
120100030721     C                   ELSE
120200030721     C                   SETON                                        32
120300030721     C                   EVAL      VABCAS = *zeros
120400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120500030721     C                             + ' ' + 'VABCAS'
120600030721     C                   ENDIF
120700030721     C                   ENDIF
120800030721     C*** CCM
120900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
121000030721     C                   EVAL      PiStr=SkSplitCSV(i)
121100030721     C                   EXSR      CHKNUM
121200030721     C                   IF        PiInt=*on      AND
121300030721     C                             PiVal<=9999999 AND
121400030721     C                             PiVal>*zeros
121500030721     C                   Z-ADD     PiVal         VABCCM
121600030721     C                   ELSE
121700030721     C                   SETON                                        32
121800030721     C                   EVAL      VABCCM = *zeros
121900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122000040510     C                             + ' ' + 'VABCCM'
122100030721     C                   ENDIF
122200030721     C                   ENDIF
122300030721     C*** LNP
122400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
122500030721     C                   EVAL      PiStr=SkSplitCSV(i)
122600030721     C                   EXSR      CHKNUM
122700030721     C                   IF        PiInt=*on    AND
122800030721     C                             PiVal<=999   AND
122900030721     C                             PiVal>*zeros
123000030721     C                   Z-ADD     PiVal         VABLNP
123100030721     C                   ELSE
123200040715     C                   SETON                                        32
123300030721     C                   EVAL      VABLNP = *zeros
123400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123500040510     C                             + ' ' + 'VABLNP'
123600030721     C                   ENDIF
123700030721     C                   ENDIF
123800030721     C*** NRS
123900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
124000030721     C                   EVAL      PiStr=SkSplitCSV(i)
124100030721     C                   EXSR      CHKNUM
124200030721     C                   IF        PiInt=*on AND
124300030721     C                             PiVal<=99
124400030721     C                   Z-ADD     PiVal         VABNRS
124500030721     C                   ELSE
124600030721     C                   SETON                                        32
124700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124800030721     C                             + ' ' + 'VABNRS'
124900030721     C                   ENDIF
125000030721     C                   ENDIF
125100030721     C*** LNA
125200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
125300030721     C                   EVAL      PiStr=SkSplitCSV(i)
125400030721     C                   EXSR      CHKNUM
125500030721     C                   IF        PiInt=*on    AND
125600030721     C                             PiVal<=999   AND
125700030721     C                             PiVal>*zeros
125800030721     C                   Z-ADD     PiVal         VABLNA
125900030721     C                   ELSE
126000030721     C                   SETON                                        32
126100030721     C                   EVAL      VABLNA = *zeros
126200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126300030721     C                             + ' ' + 'VABLNA'
126400030721     C                   ENDIF
126500030721     C                   ENDIF
126600030721     C*** CTR
126700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
126800030721     C                   EVAL      PiStr=SkSplitCSV(i)
126900030721     C                   EXSR      CHKNUM
127000030721     C                   IF        PiInt=*on  AND
127100030721     C                             PiVal<=999
127200030721     C                   Z-ADD     PiVal         VABCTR
127300030721     C                   ELSE
127400030721     C                   SETON                                        32
127500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127600030721     C                             + ' ' + 'VABCTR'
127700030721     C                   ENDIF
127800030721     C                   ENDIF
127900030721     C*** IAS
128000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
128100030721     C                   EVAL      PiStr=SkSplitCSV(i)
128200030721     C                   EXSR      CHKNUM
128300030721     C                   IF        PiNum=*on
128400030721     C                   Z-ADD     PiVal         VABIAS
128500030721     C                   ELSE
128600030721     C                   SETON                                        32
128700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128800030721     C                             + ' ' + 'VABIAS'
128900030721     C                   ENDIF
129000030721     C                   ENDIF
129100030721     C*** VLB
129200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
129300030721     C                   EVAL      PiStr=SkSplitCSV(i)
129400030721     C                   EXSR      CHKNUM
129500030721     C                   IF        PiNum=*on
129600030721     C                   Z-ADD     PiVal         VABVLB
129700030721     C                   ELSE
129800030721     C                   SETON                                        32
129900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130000030721     C                             + ' ' + 'VABVLB'
130100030721     C                   ENDIF
130200030721     C                   ENDIF
130300030721     C*** QFT
130400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
130500030721     C                   EVAL      PiStr=SkSplitCSV(i)
130600030721     C                   EXSR      CHKNUM
130700030721     C                   IF        PiNum=*on
130800030721     C                   Z-ADD     PiVal         VABQFT
130900030721     C                   ELSE
131000030721     C                   SETON                                        32
131100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131200030721     C                             + ' ' + 'VABQFT'
131300030721     C                   ENDIF
131400030721     C                   ENDIF
131500030721     C*** NCD
131600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
131700030721     C                   EVAL      PiStr=SkSplitCSV(i)
131800030721     C                   EXSR      CHKNUM
131900030721     C                   IF        PiInt=*on      AND
132000030721     C                             PiVal<=9999999
132100030721     C                   Z-ADD     PiVal         VABNCD
132200030721     C                   ELSE
132300030721     C                   SETON                                        32
132400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132500030721     C                             + ' ' + 'VABNCD'
132600030721     C                   ENDIF
132700030721     C                   ENDIF
132800030721     C*** NCA
132900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
133000030721     C                   EVAL      PiStr=SkSplitCSV(i)
133100030721     C                   EXSR      CHKNUM
133200030721     C                   IF        PiInt=*on      AND
133300030721     C                             PiVal<=9999999
133400030721     C                   Z-ADD     PiVal         VABNCA
133500030721     C                   ELSE
133600030721     C                   SETON                                        32
133700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133800030721     C                             + ' ' + 'VABNCA'
133900030721     C                   ENDIF
134000030721     C                   ENDIF
134100030721     C*** ZNC
134200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
134300030721     C                   EVAL      PiStr=SkSplitCSV(i)
134400030721     C                   EXSR      CHKNUM
134500030721     C                   IF        PiInt=*on AND
134600030721     C                             PiVal<=99
134700030721     C                   Z-ADD     PiVal         VABZNC
134800030721     C                   ELSE
134900030721     C                   SETON                                        32
135000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135100030721     C                             + ' ' + 'VABZNC'
135200030721     C                   ENDIF
135300030721     C                   ENDIF
135400030721     C*** DCR
135500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
135600030721     C                   EVAL      PiStr=SkSplitCSV(i)
135700030721     C                   EXSR      CHKNUM
135800030721     C                   IF        PiInt=*on       AND
135900030721     C                             PiVal<=99999999
136000030721     C                   Z-ADD     PiVal         VABDCR
136100030721     C                   ELSE
136200030721     C                   SETON                                        32
136300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136400030721     C                             + ' ' + 'VABDCR'
136500030721     C                   ENDIF
136600030721     C                   ENDIF
136700030721     C*** HCR
136800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
136900030721     C                   EVAL      PiStr=SkSplitCSV(i)
137000030721     C                   EXSR      CHKNUM
137100030721     C                   IF        PiInt=*on   AND
137200030721     C                             PiVal<=9999
137300030721     C                   Z-ADD     PiVal         VABHCR
137400030721     C                   ELSE
137500030721     C                   SETON                                        32
137600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137700030721     C                             + ' ' + 'VABHCR'
137800030721     C                   ENDIF
137900030721     C                   ENDIF
138000030721     C*** VMD
138100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
138200030721     C                   EVAL      PiStr=SkSplitCSV(i)
138300030721     C                   EXSR      CHKNUM
138400030721     C                   IF        PiNum=*on
138500030721     C                   Z-ADD     PiVal         VABVMD
138600030721     C                   ELSE
138700030721     C                   SETON                                        32
138800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138900030721     C                             + ' ' + 'VABVMD'
139000030721     C                   ENDIF
139100030721     C                   ENDIF
139200030721     C*** ANT
139300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
139400030721     C                   EVAL      PiStr=SkSplitCSV(i)
139500030721     C                   EXSR      CHKNUM
139600030721     C                   IF        PiInt=*on        AND
139700030721     C                             PiVal<=999999999
139800030721     C                   Z-ADD     PiVal         VABANT
139900030721     C                   ELSE
140000030721     C                   SETON                                        32
140100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140200030721     C                             + ' ' + 'VABANT'
140300030721     C                   ENDIF
140400030721     C                   ENDIF
140500030721     C*** CMO
140600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
140700030721     C                   EVAL      PiStr=SkSplitCSV(i)
140800030721     C                   EXSR      CHKNUM
140900030721     C                   IF        PiInt=*on
141000030721     C                   Z-ADD     PiVal         Num5_0
141100030721     C                   MOVEL(p)  Num5_0        VABCMO
141200030721     C                   ELSE
141300030721     C                   SETON                                        32
141400030721     C                   EVAL      VABCMO = *zeros
141500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141600030721     C                             + ' ' + 'VABCMO'
141700030721     C                   ENDIF
141800030721     C                   ENDIF
141900040510     C*
142000110926     C* Reperisco i campi del FIVAT
142100040510     C*** VATNOT
142200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
142300040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
142400040510     C                   ENDIF
142500040510     C*** VATTRC
142600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
142700040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
142800040510     C                   ENDIF
142900060331     C*** VATNOT_A
143000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
143100060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
143200060331     C                   ENDIF
143300060331     C*** VATNOT_B
143400060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
143500060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
143600060331     C                   ENDIF
143700060331     C*** VATNOT_E
143800070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
143900060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
144000060331     C                   ENDIF
144100070823     C*
144200071029     C***  ===>  Gestione campi "particolari" dentro ciclo
144300100826     C*** RMA
144400100826     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMA'
144500100826     C                   EVAL      VABRMA = %subst(SkSplitCSV(i):1:4)  +
144600100826     C                                      %subst(SkSplitCSV(i):6:4)  +
144700100826     C                                      %subst(SkSplitCSV(i):11:1) +
144800100826     C                                      %subst(SkSplitCSV(i):13)
144900100826     C                   ENDIF
145000100826     C*** NOT_E
145100100826     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_E'
145200100903     C                   EVAL      wVATNOT_E = '000000' +
145300100903     C                                         %subst(SkSplitCSV(i):5:7)
145400100826     C                   ENDIF
145500100720     C*** RMN / NSP
145600100720     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
145700100826     C                   EVAL      PiStr=%subst(SkSplitCSV(i):1:4) +
145800100826     C                                   %subst(SkSplitCSV(i):6)
145900100413     C                   EXSR      CHKNUM
146000100413     C                   IF        PiInt=*on
146100100419     C                   Z-ADD     PiVal         VABNSP
146200100720     C                   Z-ADD     PiVal         VABRMN
146300100413     C                   ELSE
146400100720     C                   Z-ADD     *zeros        VABRMN
146500100413     C                   SETON                                        32
146600100413     C                   EVAL      vinmsg = %trimr(vinmsg)
146700100720     C                             + ' ' + 'VABNSP VABRMN'
146800100413     C                   ENDIF
146900100326     C                   ENDIF
147000070925     C*
147100071029     C***  <===  -----------------------------------------
147200040510     C*
147300030715     C                   ENDIF
147400030715     C                   EVAL      i = i + 1
147500030715     C                   ENDDO
147600070925     C*
147700071029     C***  ===>  Gestione campi "particolari" fuori ciclo
147800070925     C*
147900071029     C***  <===  -----------------------------------------
148000100826     C*
148100070925     C*
148200070925     C* Considerazioni finali su CBO/CAS
148300100318     C                   IF        wFlgCAS = '1'    OR
148400100318     C                             VABCAS  > *zeros
148500070925     C                   IF        VABCBO = '1'
148600070925     C                   EVAL      VABCBO = '4'
148700070925     C                   ENDIF
148800070925     C                   IF        VABCBO = '2'
148900070925     C                   EVAL      VABCBO = '6'
149000070925     C                   ENDIF
149100070925     C                   ENDIF
149200070925     C***  <===  ----------------------------
149300040510     C*
149400040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
149500040510     C                   IF        VABNSP = *zeros
149600040510     C                   SETON                                        31
149700040510     C                   ENDIF
149800040715     C*
149900040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
150000040715     C                   IF        VABLNP = *zeros
150100040715     C                   SETON                                        31
150200040715     C                   ENDIF
150300040510     C*
150400040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
150500040510     C                   IF        VABRMN = *zeros
150600040510     C                   Z-ADD     VABNSP        VABRMN
150700040510     C                   ENDIF
150800040510     C*
150900040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
151000040510     C                   IF        VABRMA = *blanks
151100040714     C                   MOVEL     VABRMN        VABRMA
151200040510     C                   ENDIF
151300040128     C*
151400040510     C* Se provincia nn valorizzata la reperisco
151500040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
151600040510     C                   IF        VABPRD = *blanks
151700040128     C                   CLEAR                   TISI95DS
151800040128     C                   EVAL      I95TCN = '3'
151900040128     C                   Z-ADD     datcor        I95DAT
152000040128     C                   EVAL      I95NAR = VABNZD
152100040128     C                   EVAL      I95CAP = VABCAD
152200040128     C                   EVAL      I95LOC = VABLOD
152300040128     C                   CALL      'TISI95R'
152400040128     C                   PARM                    TISI95DS
152500040128     C                   EVAL      VABPRD = O95PRV
152600040510     C                   ENDIF
152700020204     C*
152800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
152900020204     C                   EXSR      CHKIMPDIV
153000030715     C                   ENDIF
153100020204     C*
153200010607     C                   ENDSR
153300010607     C*----------------------------------------------------*
153400010601
153500020204
153600020204     C*----------------------------------------------------*
153700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
153800020204     C*----------------------------------------------------*
153900020204     C     CHKIMPDIV     BEGSR
154000020204     C*
154100020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
154200020204     C                   Z-ADD     *zeros        wrkDec            9 9
154300020204     C*
154400020204     C* Come prima cosa effettuo considerazioni sulla divisa
154500020204     C                   IF        vabIAS > *zeros
154600020204     C                   IF        vabVAS <> 'EUR'
154700020204     C                   EVAL      vabVAS =  'ITL'
154800020204     C                   ENDIF
154900020204     C                   ENDIF
155000020204     C*
155100020204     C                   IF        vabCAS > *zeros
155200020204     C                   IF        vabVCA <> 'EUR'
155300020204     C                   EVAL      vabVCA =  'ITL'
155400020204     C                   ENDIF
155500020204     C                   ENDIF
155600020204     C*
155700020204     C                   IF        vabVMD > *zeros
155800020204     C                   IF        vabVAD <> 'EUR'
155900020204     C                   EVAL      vabVAD =  'ITL'
156000020204     C                   ENDIF
156100020204     C                   ENDIF
156200020204     C*
156300020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
156400020204     C                   Z-ADD     vabIAS        wrkDec
156500020204     C                   IF        wrkDec > *zeros
156600020204     C                   IF        vabVAS = 'ITL'
156700020204     C                   EVAL      vabIAS = *zeros
156800020204     C                   ENDIF
156900020204     C                   ENDIF
157000020204     C*
157100020204     C* Stabilisco se il contrasegno ha decimali valorizzati
157200020204     C                   Z-ADD     vabCAS        wrkDec
157300020204     C                   IF        wrkDec > *zeros
157400020204     C                   IF        vabVCA = 'ITL'
157500020204     C                   EVAL      vabCAS = *zeros
157600020204     C                   ENDIF
157700020204     C                   ENDIF
157800020204     C*
157900020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
158000020204     C                   Z-ADD     vabVMD        wrkDec
158100020204     C                   IF        wrkDec > *zeros
158200020204     C                   IF        vabVAD = 'ITL'
158300020204     C                   EVAL      vabVMD = *zeros
158400020204     C                   ENDIF
158500020204     C                   ENDIF
158600020204     C*
158700020204     C                   ENDSR
158800020204     C***
158900010330
159000100318
159100100318
159200100318     C*----------------------------------------------------*
159300100318     C*  CONTROLLO NUMERICITA' CAMPI
159400100318     C*----------------------------------------------------*
159500100318     C     CHKNUM        BEGSR
159600100318     C*
159700100318     C                   IF        PiDecChr = *blanks
159800100318     C                   EVAL      PiDecChr = CharNUM
159900100318     C                   ENDIF
160000100318     C*
160100100318     C                   callp     UBISNUM_Check(PiStr
160200100318     C                                          :PiDecChr
160300100318     C                                          :PiVal
160400100318     C                                          :PiNum
160500100318     C                                          :PiInt)
160600100318     C*
160700100318     C                   ENDSR
160800100318     C***
160900010601
161000010601
161100010601
161200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
161300010601     C     repfil        BEGSR
161400010601     C*
161500010601     C                   if        invfil = *zeros and
161600010601     C                             depfil > *zeros and
161700010629     C                             (vinflg = *blanks or
161800010629     C                              vinflg = *zeros)
161900010601     C
162000010601     C                   eval      invfil = depfil
162100010601     C                   endif
162200010601     C*
162300010601     C                   if        depfil <> invfil and
162400010601     C                             invfil > *zeros
162500010601     C                   eval      flgMulti = '1'
162600010601     C                   if        vinflg = *blanks
162700010601     C                   add       1             cntNonEl
162800010601     C                   endif
162900010601     C                   endif
163000010601     C*
163100010601     C                   if        vinflg = '2'
163200010601     C                   eval      flgStato = '2'
163300010601     C                   endif
163400010601     C*
163500010601     C                   ENDSR
163600010601     C***
163700010601
163800010601
163900010601
164000010330
164100040119      /TITLE Invio dei dati al punto operativo.
164200040119     C     invio         BEGSR
164300040119     C*
164400110926     C* 1° invio FIVAT
164500040119     C                   reset                   dscmz
164600040119     C                   move      vlrpoi        cmzdst
164700110926     C                   eval      cmzfld = 'FIVATWWR'
164800040119     C                   eval      cmzmbd = vlrhdl
164900040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
165000040119     C***                if        prmfir = *blanks
165100110926     C                   eval      cmzfla = 'FIVAT00F'
165200110926     C                   eval      cmzmba = 'FIVAT00F'
165300040119     C***                else
165400040119     C***                eval      cmzfla = prmfir
165500040119     C***                eval      cmzmba = prmfir
165600040119     C***                endif
165700040119     C                   eval      cmznrr = *zeros
165800040119     C                   move      §ctrokvt      cmznrr
165900040119     C                   eval      cmzlba = vlrfl1
166000040119     C                   call(e)   'TIS711C'
166100040119     C                   parm                    dscmz
166200040119     C                   parm      *blanks       esito
166300040119     C                   if        %error
166400040119     C                             or cmzerr = '1'
166500040119     C                             or esito  = '1'
166600040119     C                   eval      wrkesito = '3'
166700040119     C                   else
166800040119     C*
166900110926     C* 2° invio FIVAB
167000040119     C                   reset                   dscmz
167100040119     C                   move      vlrpoi        cmzdst
167200040119     C                   eval      cmzfld = vlrfou
167300040119     C                   eval      cmzmbd = vlrhdl
167400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
167500040119     C***                if        prmfir = *blanks
167600110926     C                   eval      cmzfla = 'FIVAB00F'
167700110926     C                   eval      cmzmba = 'FIVAB00F'
167800040119     C***                else
167900040119     C***                eval      cmzfla = prmfir
168000040119     C***                eval      cmzmba = prmfir
168100040119     C***                endif
168200040119     C                   eval      cmznrr = *zeros
168300040119     C                   move      §ctrokvb      cmznrr
168400040119     C                   eval      cmzlba = vlrfl1
168500040119     C                   call(e)   'TIS711C'
168600040119     C                   parm                    dscmz
168700040119     C                   parm      *blanks       esito
168800040119     C                   if        %error
168900040119     C                             or cmzerr = '1'
169000040119     C                             or esito  = '1'
169100040119     C                   eval      wrkesito = '3'
169200040119     C                   endif
169300040119     C                   endif
169400040119     C*
169500040119     C                   ENDSR
169600040119     C***
169700010601
169800010601
169900010601
170000010601
170100010601      /TITLE Invio dei dati al punto operativo.
170200010601     C     opeini        BEGSR
170300010601     C*
170400010601     C* Inizializzo flag e contatori operativi
170500010601     C                   movel     '0'           flgGiro           1
170600010601     C                   movel     '0'           flgMulti          1
170700010601     C                   movel     '1'           flgStato          1
170800010615     C                   movel     '0'           flgOk             1
170900010601     C                   z-add     *zeros        cntNonEl         10 0
171000010601     C                   z-add     *zeros        depfil            3 0
171100010601     C                   z-add     *zeros        invfil            3 0
171200010601     C*
171300010601     C                   ENDSR
171400010601     C***
171500070326
171600070326
171700070326
171800070326
171900070326     C     *pssr         BEGSR
172000070329     C*
172100070329     C                   if        %open(tivin00r)
172200070329     C                   close     tivin00r
172300070329     C                   endif
172400110926     C                   if        %open(fivabwwr)
172500110926     C                   close     fivabwwr
172600070329     C                   endif
172700110926     C                   if        %open(fivatwwr)
172800110926     C                   close     fivatwwr
172900070329     C                   endif
173000070326     C*
173100070326     C* Effettuo la chiamata al CLLE preposto
173200110927     C                   call(e)   'TITVVTC'
173300070326     C                   parm                    parccm
173400070326     C                   parm                    parmbr
173500070326     C                   parm      '2'           paropz
173600070326     C*
173700070326     C                   eval      wrkesito = '2'
173800070404     C*
173900070404     C                   seton                                        LR
174000070326     C*
174100070326     C                   ENDSR     '*CANCL'
174200070326     C***
174300100318
174400100318
174500100318
174600100318     C     normCSV       BEGSR
174700100318     C*
174800100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
174900100318     C* separatori campi
175000100318     C                   if        CharTXT <> *blanks
175100100318     C                   z-add     *zeros        wLenVINDTA        4 0
175200100318     C                   z-add     *zeros        wFlgVINDTA        1 0
175300100318     C                   setoff                                       80
175400100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
175500100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
175600100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
175700100318     C                   if        not *in80
175800100318     C                   seton                                        80
175900100318     C                   else
176000100318     C                   setoff                                       80
176100100318     C                   endif
176200100318     C                   endif
176300100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
176400100318     C                             and *in80 = *on
176500100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
176600100318     C                   endif
176700100318     C                   enddo
176800100318     C                   endif
176900100318     C*
177000100318     C                   ENDSR
177100100318     C***
177200070326
177300100326
177400100326
177500100326     C*----------------------------------------------------*
177600100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
177700100326     C*----------------------------------------------------*
177800100326     C     REPNSP        BEGSR
177900100326     C*
178000100326     C* NSP => Stacco un numeratore da AZNUM
178100100326     C                   clear                   TRUL33DS
178200100326     C                   eval      I33OPE = *zeros
178300100326     C                   eval      I33CNU = 302
178400100326     C                   eval      I33NUM = 1
178500100326     C                   movel     TRUL33DS      KPJBU
178600100326     C                   call      'TRUL33R'
178700100326     C                   parm                    KPJBA
178800100326     C                   movel     KPJBU         TRUL33DS
178900100326     C                   if        O33ERR = *zeros
179000100326     C                   z-add     O33NRF        VABNSP
179100100326     C                   z-add     O33NRF        VATNSP
179200100326     C                   else
179300100326     C                   SETON                                        31
179400100326     C                   EVAL      vinmsg = %trimr(vinmsg)
179500100326     C                             + ' ' + 'VABNSP VATNSP'
179600100326     C                   endif
179700100326     C*
179800100326     C                   ENDSR
179900010330
180000100326
180100010330
180200000613     C     *inzsr        BEGSR
180300990910     C*
180400990910     C     *entry        plist
180500990920     C                   parm                    tivlrds
180600990921     C                   parm      wrkesito      esito
180700000724     C                   parm                    prmlit
180800000710     C                   parm                    prmfir
180900100826     C*
181000100826     C* CALCOLA LA DATA CORRENTE
181100100826     C                   time                    wn14             14 0
181200100826     C                   movel     wn14          oracor            6 0          *ORA
181300100826     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
181400100826     C                   eval      datcor = %dec(%date() : *ISO)
181500000613     C*
181600000613     C                   ENDSR
181700000613     C***
181800990908
181900100720     Otitv1y8p  E            riepilogo         2
182000990915     O                                              'Upload via Internet'
182100990915     O                                           +1 'Traduzione TIVIN00R -'
182200110926     O                                           +1 'FIVABWWR'
182300990915     O                                           +1 'Testate bolle'
182400990915     O          E            riepilogo   2
182500990915     O                       wrkdata
182600990915     O                       wrkora              +1
182700990915     O                       procname            +1
182800990915     O          E            riepilogo   2
182900990915     O                                              'Cliente..................:'
183000010605     O                       VABCCM        z     +1
183100990915     O          E            riepilogo   2
183200990920     O                                              'Riferimento Strategi.....:'
183300990920     O                       vlrhdl              +1
183400990915     O          E            riepilogo   2
183500990915     O                                              'Giusti...................:'
183600040119     O                       §CTROKVB      2   +  1
183700990915     O          E            riepilogo   2
183800990915     O                                              'Sbagliati e corretti.....:'
183900971022     O                       §CTRMO        2   +  1
184000990915     O          E            riepilogo   2
184100990915     O                                              'Sbagliati e scartati.....:'
184200971022     O                       §CTRNO        2   +  1
184300000613
184400100720     Otitv1y8ps E            testdett          2
184500000613     O                                              'Upload via Internet'
184600000613     O                                           +1 'Traduzione TIVIN00R -'
184700110926     O                                           +1 'FIVABWWR'
184800000613     O                                           +1 'Testate bolle'
184900000616     O          E            testdett    3
185000000613     O                                           +2 'N° rec'
185100000613     O                                           +3 'Anteprima contenuto'
185200000616     O          E            rigadett    2
185300000613     O                       rrnum               +2
185400000621     O                       recko               +3
185500000616     O          E            findett     2
185600000613     O                       wrkdata
185700000613     O                       wrkora              +1
185800000613     O                       procname            +1
185900000616     O          E            findett     2
186000000613     O                                              'Cliente..................:'
186100010605     O                       VABCCM        z     +1
186200000616     O          E            findett     2
186300000613     O                                              'Riferimento Strategi.....:'
186400000613     O                       vlrhdl              +1
186500000616     O          E            findett     2
186600000613     O                                              'Giusti...................:'
186700040119     O                       §CTROKVB      2   +  1
186800000616     O          E            findett     2
186900000613     O                                              'Sbagliati e corretti.....:'
187000000613     O                       §CTRMO        2   +  1
187100000616     O          E            findett     2
187200000613     O                                              'Sbagliati e scartati.....:'
187300000613     O                       §CTRNO        2   +  1
187400000613** CMD - COMANDI CL
187500100720OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1Y8P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
187600100720OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1Y8PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
187700100720DLTOVR FILE(TITV1Y8P TITV1Y8PS) LVL(*)
187800010606
187900000613
188000040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
188100100826FIL001;W_VABRMA;FIL002;W_VATNOT_E;VABPKB;FIL003;VABVLB;FIL004;W_VABRMN;
188200100826FIL005;FIL006;FIL007;FIL008;FIL009;VABRSD;VABRD2;VABIND;VABCAD;VABLOD;
188300100826FIL010;FIL011;VATNOT_B;
188400100318
188500100318
