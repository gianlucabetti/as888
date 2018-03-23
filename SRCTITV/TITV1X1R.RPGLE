000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200100318     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
000700040119     FFIVATwwr  O    E             DISK    usropn
000800100419     Ftitv1x1p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070329     F                                     infsr(*pssr)
001100100419     Ftitv1x1ps O    f  198        PRINTER usropn
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
014600100419     C                   if        not %open(titv1x1ps)
014700100419     C                   open      titv1x1ps
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
016700040119     C                   call(e)   'TITVVTC'
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
017800100419     C                   if        %open(titv1x1ps)
017900000616     C                   except    findett
018000100419     C                   close     titv1x1ps
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
020000100419     C                   if        not %open(titv1x1p)
020100100419     C   50              open      titv1x1p
020200990915     C                   endif
020300990915     C*
020400030715     C   50              except    riepilogo
020500990915     C*
020600100419     C                   if        %open(titv1x1p)
020700100419     C   50              close     titv1x1p
020800990914     C                   endif
020900000613     C*
021000000613     C                   ENDSR
021100000613     C***
021200000613
021300010601
021400010601
021500010330     C*--------------------------------------------------------
021600021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
021700010330     C*--------------------------------------------------------
021800010605     C     RWVAB         BEGSR
021900010330     C*
022000010330     C                   if        not %open(tivin00r)
022100010330     C                   open      tivin00r
022200010330     C                   endif
022300021025     C                   if        not %open(fivabwwr)
022400021025     C                   open      fivabwwr
022500010330     C                   endif
022600040119     C*
022700040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
022800040119     C                   exsr      prevat
022900040119     C*
023000040119     C                   if        chkcall = '0'
023100040119     C*
023200040119     C                   if        not %open(fivatwwr)
023300040119     C                   open      fivatwwr
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
036400021025     C                   if        %open(fivabwwr)
036500021025     C                   close     fivabwwr
036600010330     C                   endif
036700040512     C                   if        %open(fivatwwr)
036800040512     C                   close     fivatwwr
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
039600080527     C  N31              eval      fivabds_sav = fivabds
039700080527     C                   eval      depspe = vabNSP
039800080527     C                   else
039900080527     C                   if        depspe = vabNSP AND *in65 = *off
040000100413     C   51              add       VABNCL        sav_VABNCL
040100100413     C   51              add       VABPKB        sav_VABPKB
040200100413     C   51              add       VABVLB        sav_VABVLB
040300100413     C  N51              z-add     VABNCL        sav_VABNCL
040400100413     C  N51              z-add     VABPKB        sav_VABPKB
040500100413     C  N51              z-add     VABVLB        sav_VABVLB
040600080527     C                   else
040700080527     C  N31              eval      fivabds_ok = fivabds_sav
040800080527     C  N31              exsr      wrivab
040900080527     C  N31              eval      fivabds_sav = fivabds
041000080527     C                   eval      depspe = vabNSP
041100080527     C                   endif
041200080527     C                   endif
041300080527     C*
041400080527     C                   ENDSR
041500040119
041600040119
041700040119
041800040119     C*----------------------------------------------------*
041900040119     C*  SCARICAMENTO BUFFER RECORDS VAB
042000040119     C*----------------------------------------------------*
042100040510     C     WRIVAB        BEGSR
042200080513     C*
042300080513     C* Gestisco l'eventuale rottura x numero spedizione
042400080527     C                   if        VABNSP <> depspe or *in65 = *on
042500070301     C                   if        wVATNOT_E <> *blanks
042600070301     C                   eval      VABCTM = '7Q'
042700070301     C                   endif
042800100413     C                   write     fivab000
042900080513     C                   endif
043000040119     C*
043100040119     C                   ENDSR
043200040119
043300040119
043400040119
043500040119     C*----------------------------------------------------*
043600040119     C*  SCARICAMENTO BUFFER RECORDS VAT
043700040119     C*----------------------------------------------------*
043800040510     C     WRIVAT        BEGSR
043900060512     C*
044000060512     C* Inizializzo i campi chiave
044100060512     C                   eval      VATFGS = VABFGS
044200060512     C                   eval      VATAAS = VABAAS
044300060512     C                   eval      VATCCM = VABCCM
044400070823     C                   eval      VATNRS = VABNRS
044500060512     C                   eval      VATNSP = VABNSP
044600060512     C                   eval      VATLNP = VABLNP
044700040714     C*
044800040714     C* Solo se VATNOT valorizzato scarico il buffer
044900040714     C                   if        VATNOT <> *blanks
045000040119     C*
045100040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
045200040510     C                   if        VATTRC = *blanks
045300040510     C                   eval      VATTRC = 'E'
045400040510     C                   endif
045500040510     C                   write     FIVAT000
045600040119     C                   add       1             §CTROKVT
045700040714     C*
045800040714     C                   endif
045900060331     C*
046000060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
046100060331     C                   if        wVATNOT_A <> *blanks
046200060331     C                   eval      VATNOT = wVATNOT_A
046300060331     C                   eval      VATTRC = 'A'
046400060331     C                   write     FIVAT000
046500060331     C                   add       1             §CTROKVT
046600060331     C                   endif
046700060331     C*
046800060331     C                   if        wVATNOT_B <> *blanks
046900060331     C                   eval      VATNOT = wVATNOT_B
047000060331     C                   eval      VATTRC = 'B'
047100060331     C                   write     FIVAT000
047200060331     C                   add       1             §CTROKVT
047300060331     C                   endif
047400060331     C*
047500060331     C                   if        wVATNOT_E <> *blanks
047600060331     C                   eval      VATNOT = wVATNOT_E
047700060331     C                   eval      VATTRC = 'E'
047800060331     C                   write     FIVAT000
047900060331     C                   add       1             §CTROKVT
048000060331     C                   endif
048100040119     C*
048200040119     C                   ENDSR
048300040119
048400040119
048500040119
048600040119     C*----------------------------------------------------*
048700040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
048800040119     C*----------------------------------------------------*
048900040119     C     PREVAT        BEGSR
049000040119     C*
049100040119     C* Compongo il nome del membro da dare al FIVATWWR
049200040119     C                   eval      parmbr = vlrhdl
049300040119     C                   movel     'M'           parmbr
049400040510     C                   eval      parccm = vlrksc
049500040119     C                   eval      paropz = '1'
049600040119     C* Effettuo la chiamata al CLLE preposto
049700040119     C                   call(e)   'TITVVTC'
049800040119     C                   parm                    parccm
049900040119     C                   parm                    parmbr
050000040119     C                   parm                    paropz
050100040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
050200040119     C                   if        %error
050300040119     C                   movel     '1'           chkcall
050400040119     C                   else
050500040119     C                   movel     '0'           chkcall
050600040119     C                   endif
050700040119     C*
050800040119     C                   ENDSR
050900010601
051000010601
051100010601
051200010330     C*----------------------------------------------------*
051300030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
051400010330     C*----------------------------------------------------*
051500040119     C     INZVAR        BEGSR
051600080527     C*
051700080527     C                   setoff                                       65
051800010330     C*
051900030822     C* Inizializzo variabili di wrk
052000020204     C                   Z-ADD     *zeros        Num5_0            5 0
052100060331     C                   MOVEL     *blanks       wVATNOT_A
052200060331     C                   MOVEL     *blanks       wVATNOT_B
052300060331     C                   MOVEL     *blanks       wVATNOT_E
052400030822     C*
052500030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
052600030822     C                   CLEAR                   SkSplitCSV
052700030822     C                   EVAL      i = 1
052800030822     C                   EVAL      posDa = *zeros
052900030822     C                   EVAL      posA  = *zeros
053000080527     C*
053100080527     C                   CLEAR                   FIVABDS
053200080527     C                   CLEAR                   FIVAT000
053300030822     C*
053400030822     C* Reimposto i valori di default
053500030822     C                   EXSR      DEFCAM
053600030822     C*
053700010330     C                   ENDSR
053800010330     C*----------------------------------------------------*
053900030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
054000010330     C*----------------------------------------------------*
054100010330     C     DEFCAM        BEGSR
054200030822     C*
054300030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
054400030715     C* e delimitatore testo.
054500030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
054600030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
054700030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
054800100318     C*
054900100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
055000100318     C                   EVAL      CharSOS = CharNUM
055100030822     C*
055200030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
055300100413     C                   SETOFF                                       5051
055400100413     C                   SELECT
055500100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
055600030715     C                   SETON                                        50
055700100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
055800100413     C                   SETON                                        51
055900100413     C                   ENDSL
056000030822     C*
056100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
056200030822     C                   EVAL      posDaDft = 1
056300030822     C                   EVAL      posADft  = 0
056400030822     C                   EVAL      wGiroDft = 0
056500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
056600030822     C                             posDaDft > 0
056700030822     C*
056800030822     C* Gestisco il 1° giro
056900030822     C                   IF        wGiroDft = 0
057000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
057100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
057200030822     C* Incremento il contatore dei "giri"
057300030822     C                   EVAL      wGiroDft = 1
057400030822     C                   ELSE
057500030822     C                   EVAL      posDaDft = posADft
057600030822     C                   ENDIF
057700030822     C* Eseguo lo scan x trovare la fine del campo corrente
057800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
057900030822     C*
058000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
058100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
058200030822     C                   IF        posDaDft > 0 AND
058300030822     C                             posADft  > 0
058400030822     C* NCL
058500030822     C                   IF        %subst(
058600030822     C                             %subst(vlrppt:posDaDft+1:
058700030822     C                             posADft-posDaDft-1):1:3)
058800030822     C                             = 'NCL'
058900030822     C                   EVAL      PiStr=%trim(%subst(
059000030822     C                             %subst(vlrppt:posDaDft+1:
059100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059200030822     C                   EXSR      CHKNUM
059300030822     C                   IF        PiInt=*on
059400030822     C                   Z-ADD     PiVal         VABNCL
059500030822     C                   ENDIF
059600030822     C                   ENDIF
059700030822     C* CCM
059800030822     C                   IF        %subst(
059900030822     C                             %subst(vlrppt:posDaDft+1:
060000030822     C                             posADft-posDaDft-1):1:3)
060100030822     C                             = 'CCM'
060200030822     C                   EVAL      PiStr=%trim(%subst(
060300030822     C                             %subst(vlrppt:posDaDft+1:
060400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060500030822     C                   EXSR      CHKNUM
060600030822     C                   IF        PiInt=*on
060700030822     C                   Z-ADD     PiVal         VABCCM
060800030822     C                   ENDIF
060900030822     C                   ENDIF
061000030822     C* LNP
061100030822     C                   IF        %subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):1:3)
061400030822     C                             = 'LNP'
061500030822     C                   EVAL      PiStr=%trim(%subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061800030822     C                   EXSR      CHKNUM
061900030822     C                   IF        PiInt=*on
062000030822     C                   Z-ADD     PiVal         VABLNP
062100030822     C                   ENDIF
062200030822     C                   ENDIF
062300030822     C* NRS
062400030822     C                   IF        %subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):1:3)
062700030822     C                             = 'NRS'
062800030822     C                   EVAL      PiStr=%trim(%subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063100030822     C                   EXSR      CHKNUM
063200030822     C                   IF        PiInt=*on
063300030822     C                   Z-ADD     PiVal         VABNRS
063400030822     C                   ENDIF
063500030822     C                   ENDIF
063600030822     C* CTR
063700030822     C                   IF        %subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):1:3)
064000030822     C                             = 'CTR'
064100030822     C                   EVAL      PiStr=%trim(%subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064400030822     C                   EXSR      CHKNUM
064500030822     C                   IF        PiInt=*on
064600030822     C                   Z-ADD     PiVal         VABCTR
064700030822     C                   ENDIF
064800030822     C                   ENDIF
064900030822     C* PKB
065000030822     C                   IF        %subst(
065100030822     C                             %subst(vlrppt:posDaDft+1:
065200030822     C                             posADft-posDaDft-1):1:3)
065300030822     C                             = 'PKB'
065400030822     C                   EVAL      PiStr=%trim(%subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065700030822     C                   EXSR      CHKNUM
065800030822     C                   IF        PiNum=*on
065900030822     C                   Z-ADD     PiVal         VABPKB
066000030822     C                   ENDIF
066100030822     C                   ENDIF
066200030822     C* VLB
066300030822     C                   IF        %subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):1:3)
066600030822     C                             = 'VLB'
066700030822     C                   EVAL      PiStr=%trim(%subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067000030822     C                   EXSR      CHKNUM
067100030822     C                   IF        PiNum=*on
067200030822     C                   Z-ADD     PiVal         VABVLB
067300030822     C                   ENDIF
067400030822     C                   ENDIF
067500030822     C* QFT
067600030822     C                   IF        %subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):1:3)
067900030822     C                             = 'QFT'
068000030822     C                   EVAL      PiStr=%trim(%subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068300030822     C                   EXSR      CHKNUM
068400030822     C                   IF        PiNum=*on
068500030822     C                   Z-ADD     PiVal         VABQFT
068600030822     C                   ENDIF
068700030822     C                   ENDIF
068800030822     C* CBO
068900030822     C                   IF        %subst(
069000030822     C                             %subst(vlrppt:posDaDft+1:
069100030822     C                             posADft-posDaDft-1):1:3)
069200030822     C                             = 'CBO'
069300030822     C                   EVAL      VABCBO=%trim(%subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069600030822     C                   ENDIF
069700030822     C* TSP
069800030822     C                   IF        %subst(
069900030822     C                             %subst(vlrppt:posDaDft+1:
070000030822     C                             posADft-posDaDft-1):1:3)
070100030822     C                             = 'TSP'
070200030822     C                   EVAL      VABTSP=%trim(%subst(
070300030822     C                             %subst(vlrppt:posDaDft+1:
070400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070500030822     C                   ENDIF
070600030822     C* VAS
070700030822     C                   IF        %subst(
070800030822     C                             %subst(vlrppt:posDaDft+1:
070900030822     C                             posADft-posDaDft-1):1:3)
071000030822     C                             = 'VAS'
071100030822     C                   EVAL      VABVAS=%trim(%subst(
071200030822     C                             %subst(vlrppt:posDaDft+1:
071300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071400030822     C                   ENDIF
071500030822     C* VCA
071600030822     C                   IF        %subst(
071700030822     C                             %subst(vlrppt:posDaDft+1:
071800030822     C                             posADft-posDaDft-1):1:3)
071900030822     C                             = 'VCA'
072000030822     C                   EVAL      VABVCA=%trim(%subst(
072100030822     C                             %subst(vlrppt:posDaDft+1:
072200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072300030822     C                   ENDIF
072400070925     C* TIC
072500070925     C                   IF        %subst(
072600070925     C                             %subst(vlrppt:posDaDft+1:
072700070925     C                             posADft-posDaDft-1):1:3)
072800070925     C                             = 'TIC'
072900070925     C                   EVAL      VABTIC=%trim(%subst(
073000070925     C                             %subst(vlrppt:posDaDft+1:
073100070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073200070925     C                   ENDIF
073300030822     C* GCA
073400030822     C                   IF        %subst(
073500030822     C                             %subst(vlrppt:posDaDft+1:
073600030822     C                             posADft-posDaDft-1):1:3)
073700030822     C                             = 'GCA'
073800030822     C                   EVAL      VABGCA=%trim(%subst(
073900030822     C                             %subst(vlrppt:posDaDft+1:
074000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074100030822     C                   ENDIF
074200030822     C* CTM
074300030822     C                   IF        %subst(
074400030822     C                             %subst(vlrppt:posDaDft+1:
074500030822     C                             posADft-posDaDft-1):1:3)
074600030822     C                             = 'CTM'
074700030822     C                   EVAL      VABCTM=%trim(%subst(
074800030822     C                             %subst(vlrppt:posDaDft+1:
074900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075000030822     C                   ENDIF
075100030822     C* FFD
075200030822     C                   IF        %subst(
075300030822     C                             %subst(vlrppt:posDaDft+1:
075400030822     C                             posADft-posDaDft-1):1:3)
075500030822     C                             = 'FFD'
075600030822     C                   EVAL      VABFFD=%trim(%subst(
075700030822     C                             %subst(vlrppt:posDaDft+1:
075800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075900030822     C                   ENDIF
076000030822     C* VAD
076100030822     C                   IF        %subst(
076200030822     C                             %subst(vlrppt:posDaDft+1:
076300030822     C                             posADft-posDaDft-1):1:3)
076400030822     C                             = 'VAD'
076500030822     C                   EVAL      VABVAD=%trim(%subst(
076600030822     C                             %subst(vlrppt:posDaDft+1:
076700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076800030822     C                   ENDIF
076900030822     C* GMA
077000030822     C                   IF        %subst(
077100030822     C                             %subst(vlrppt:posDaDft+1:
077200030822     C                             posADft-posDaDft-1):1:3)
077300030822     C                             = 'GMA'
077400030822     C                   EVAL      VABGMA=%trim(%subst(
077500030822     C                             %subst(vlrppt:posDaDft+1:
077600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077700030822     C                   ENDIF
077800030822     C* GGA
077900030822     C                   IF        %subst(
078000030822     C                             %subst(vlrppt:posDaDft+1:
078100030822     C                             posADft-posDaDft-1):1:3)
078200030822     C                             = 'GGA'
078300030822     C                   EVAL      VABGGA=%trim(%subst(
078400030822     C                             %subst(vlrppt:posDaDft+1:
078500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078600030822     C                   ENDIF
078700030822     C* GVA
078800030822     C                   IF        %subst(
078900030822     C                             %subst(vlrppt:posDaDft+1:
079000030822     C                             posADft-posDaDft-1):1:3)
079100030822     C                             = 'GVA'
079200030822     C                   EVAL      VABGVA=%trim(%subst(
079300030822     C                             %subst(vlrppt:posDaDft+1:
079400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079500030822     C                   ENDIF
079600030822     C* TC1
079700030822     C                   IF        %subst(
079800030822     C                             %subst(vlrppt:posDaDft+1:
079900030822     C                             posADft-posDaDft-1):1:3)
080000030822     C                             = 'TC1'
080100030822     C                   EVAL      VABTC1=%trim(%subst(
080200030822     C                             %subst(vlrppt:posDaDft+1:
080300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080400030822     C                   ENDIF
080500030822     C* TC2
080600030822     C                   IF        %subst(
080700030822     C                             %subst(vlrppt:posDaDft+1:
080800030822     C                             posADft-posDaDft-1):1:3)
080900030822     C                             = 'TC2'
081000030822     C                   EVAL      VABTC2=%trim(%subst(
081100030822     C                             %subst(vlrppt:posDaDft+1:
081200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081300030822     C                   ENDIF
081400040714     C* VATTRC
081500040714     C                   IF        %subst(
081600040714     C                             %subst(vlrppt:posDaDft+1:
081700040714     C                             posADft-posDaDft-1):1:3)
081800040714     C                             = 'TRC'
081900040714     C                   EVAL      VATTRC=%trim(%subst(
082000040714     C                             %subst(vlrppt:posDaDft+1:
082100040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082200040714     C                   ENDIF
082300030822     C* ...
082400030822     C                   ENDIF
082500030822     C                   ENDDO
082600020204     C*
082700010330     C                   ENDSR
082800010607     C*----------------------------------------------------*
082900021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
083000010607     C*----------------------------------------------------*
083100010607     C     IMPVAB        BEGSR
083200010607     C*
083300010607     C                   SETOFF                                       3132
083400030515     C*
083500010607     C                   MOVEL     datcor        VABAAS
083600010607     C                   MOVE      datcor        VABMGS
083700010607     C*
083800030715     C**********
083900030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
084000030715     C**********
084100030715     C                   IF        wGiro = *zeros
084200040113     C*
084300040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
084400040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
084500080429     C                             %trim(intcol(3))+%trim(intcol(4))+
084600080429     C                             %trim(intcol(5))<>*blanks
084700040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
084800080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
084900080429     C                                      %trim(intcol(5))
085000070823     C     ';':CharCSV   XLATE     vindta        vindta
085100040510     C                   ENDIF
085200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
085300031201     C     minu:maiu     XLATE     vindta        vindta
085400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
085500030715     C                   DOW       posDa <= %len(%trim(vindta))
085600030715     C*
085700030715     C* Gestisco il 1° campo
085800030715     C                   IF        i = 1
085900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
086000030715     C                   EVAL      posDa = 1
086100030715     C* Eseguo lo scan x trovare la fine del primo campo
086200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086300030715     C* A questo punto "estraggo" il campo corrente
086400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086500030715     C                                                    (posA-posDa))
086600030715     C* X i campi successivi al 1°
086700030715     C                   ELSE
086800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
086900030715     C                   EVAL      posDa = posA + 1
087000030715     C* Eseguo lo scan x trovare la fine del campo corrente
087100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087200030715     C* Gestisco l'ultimo campo
087300030715     C                   IF        posA = *zeros
087400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
087500030715     C* A questo punto "estraggo" il campo corrente
087600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087700030715     C                                                    (posA-posDa))
087800030715     C                   LEAVE
087900030715     C                   ELSE
088000030715     C* A questo punto "estraggo" il campo corrente
088100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088200030715     C                                                    (posA-posDa))
088300030715     C                   ENDIF
088400030715     C                   ENDIF
088500030715     C* Incremento il contatore d campo
088600030715     C                   EVAL      i = i +1
088700030715     C                   ENDDO
088800030715     C*
088900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
089000030715     C* delimitazione testo
089100030715     C                   EVAL      i = 1
089200030715     C                   DOW       i < %elem(SkSplitFLD)
089300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
089400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
089500030715     C                   EVAL      i = i + 1
089600030715     C                   ENDDO
089700030715     C                   EVAL      wGiro = 1
089800030715     C                   ELSE
089900030715     C                   EVAL      wGiro = 2
090000031204     C**********
090100031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
090200031204     C* d separatore campo
090300031204     C**********
090400031204     C                   EVAL      vindta = %trim(vindta)
090500031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
090600031204     C                   EVAL      vindta = %subst(vindta:2)
090700031204     C                   ENDDO
090800031204     C*
090900031204     C                   Z-ADD     *zeros        lunghInput        4 0
091000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
091100031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
091200031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
091300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
091400031204     C                   ENDDO
091500030715     C**********
091600030715     C* Effettuo lo split del campo dati d input
091700030715     C**********
091800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
091900030715     C                   DOW       posDa <= %len(%trim(vindta))
092000030715     C*
092100030715     C* Gestisco il 1° campo
092200030715     C                   IF        i = 1
092300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
092400030715     C                   EVAL      posDa = 1
092500030715     C* Eseguo lo scan x trovare la fine del primo campo
092600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092700030715     C* A questo punto "estraggo" il campo corrente
092800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092900030715     C                                                    (posA-posDa))
093000030715     C* X i campi successivi al 1°
093100030715     C                   ELSE
093200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
093300030715     C                   EVAL      posDa = posA + 1
093400031204     C* Verifico che nn vi sia il campo nn valorizzato
093500031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
093600031204     C* Se campo nn valorizzato skippo al prossimo
093700031204     C                   EVAL      SkSplitCSV(i) = *blanks
093800031204     C                   EVAL      posA  = posA + 1
093900031204     C                   ELSE
094000030715     C* Eseguo lo scan x trovare la fine del campo corrente
094100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094200030715     C* Gestisco l'ultimo campo
094300030715     C                   IF        posA = *zeros
094400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
094500030715     C* A questo punto "estraggo" il campo corrente
094600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094700030715     C                                                    (posA-posDa))
094800030715     C                   LEAVE
094900030715     C                   ELSE
095000030715     C* A questo punto "estraggo" il campo corrente
095100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095200030715     C                                                    (posA-posDa))
095300030715     C                   ENDIF
095400031204     C                   ENDIF
095500030715     C                   ENDIF
095600030715     C* Incremento il contatore d campo
095700030715     C                   EVAL      i = i +1
095800030715     C                   ENDDO
095900030715     C*
096000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
096100030715     C* delimitazione testo
096200030715     C                   EVAL      i = 1
096300030715     C                   DOW       i < %elem(SkSplitCSV)
096400030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
096500030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
096600030715     C                   EVAL      i = i + 1
096700030715     C                   ENDDO
096800100326     C*
096900100326     C* Stacco un progressivo x numero spedizione
097000100326     C                   EXSR      repNSP
097100100326     C*
097200030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
097300030715     C                   EVAL      i = 1
097400030715     C                   DOW       i < %elem(SkSplitFLD)
097500030715     C*
097600030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
097700030715     C                   IF        SkSplitFLD(i) <> *blanks AND
097800030715     C                             SkSplitCSV(i) <> *blanks
097900030721     C*** ATB
098000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
098100030721     C                   EVAL      VABATB = SkSplitCSV(i)
098200030721     C                   ENDIF
098300030715     C*** RSD
098400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
098500030715     C                   EVAL      VABRSD = SkSplitCSV(i)
098600030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
098700030715     C     '@':'A'       XLATE     VABRSD        VABRSD
098800030715     C* ==
098900030715     C                   ENDIF
099000030721     C*** RD2
099100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
099200030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
099300030721     C                   ENDIF
099400030715     C*** IND
099500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
099600030715     C                   EVAL      VABIND = SkSplitCSV(i)
099700030715     C                   ENDIF
099800030715     C*** LOD
099900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
100000030715     C                   EVAL      VABLOD = SkSplitCSV(i)
100100030715     C                   ENDIF
100200030715     C*** PRD
100300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
100400030715     C                   EVAL      VABPRD = SkSplitCSV(i)
100500030715     C                   ENDIF
100600030721     C*** NZD
100700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
100800030721     C                   EVAL      VABNZD = SkSplitCSV(i)
100900030721     C                   ENDIF
101000030715     C*** RMA
101100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
101200030715     C                   EVAL      VABRMA = SkSplitCSV(i)
101300030715     C                   ENDIF
101400030715     C*** NOT
101500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
101600030715     C                   EVAL      VABNOT = SkSplitCSV(i)
101700030715     C                   ENDIF
101800030715     C*** NT2
101900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
102000030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
102100030715     C                   ENDIF
102200030715     C*** VCA
102300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
102400030715     C                   EVAL      VABVCA = SkSplitCSV(i)
102500030715     C                   ENDIF
102600030715     C*** VAS
102700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
102800030715     C                   EVAL      VABVAS = SkSplitCSV(i)
102900030715     C                   ENDIF
103000030715     C*** FFD
103100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
103200030715     C                   EVAL      VABFFD = SkSplitCSV(i)
103300030715     C                   ENDIF
103400030721     C*** GC1
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
103600030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
103700030721     C                   ENDIF
103800030721     C*** GC2
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
104000030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
104100030721     C                   ENDIF
104200030721     C*** TSP
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
104400030721     C                   EVAL      VABTSP = SkSplitCSV(i)
104500030721     C                   ENDIF
104600040510     C*** CBO
104700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
104800040510     C                   EVAL      VABCBO = SkSplitCSV(i)
104900040510     C                   ENDIF
105000030721     C*** NAS
105100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
105200030721     C                   EVAL      VABNAS = SkSplitCSV(i)
105300030721     C                   ENDIF
105400030721     C*** TIC
105500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
105600030721     C                   EVAL      VABTIC = SkSplitCSV(i)
105700030721     C                   ENDIF
105800030721     C*** GCA
105900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
106000030721     C                   EVAL      VABGCA = SkSplitCSV(i)
106100030721     C                   ENDIF
106200030721     C*** XCO
106300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
106400030721     C                   EVAL      VABXCO = SkSplitCSV(i)
106500030721     C                   ENDIF
106600030721     C*** CTM
106700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
106800030721     C                   EVAL      VABCTM = SkSplitCSV(i)
106900030721     C                   ENDIF
107000030721     C*** TCR
107100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
107200030721     C                   EVAL      VABTCR = SkSplitCSV(i)
107300030721     C                   ENDIF
107400030721     C*** CTS
107500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
107600030721     C                   EVAL      VABCTS = SkSplitCSV(i)
107700030721     C                   ENDIF
107800030721     C*** FTM
107900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
108000030721     C                   EVAL      VABFTM = SkSplitCSV(i)
108100030721     C                   ENDIF
108200030721     C*** VAD
108300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
108400030721     C                   EVAL      VABVAD = SkSplitCSV(i)
108500030721     C                   ENDIF
108600030721     C*** GMA
108700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
108800030721     C                   EVAL      VABGMA = SkSplitCSV(i)
108900030721     C                   ENDIF
109000030721     C*** GGA
109100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
109200030721     C                   EVAL      VABGGA = SkSplitCSV(i)
109300030721     C                   ENDIF
109400030721     C*** GVA
109500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
109600030721     C                   EVAL      VABGVA = SkSplitCSV(i)
109700030721     C                   ENDIF
109800030721     C*** TC1
109900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
110000030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
110100030721     C                   ENDIF
110200030721     C*** TC2
110300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
110400030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
110500030721     C                   ENDIF
110600030721     C*** SCL
110700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
110800030721     C                   EVAL      VABSCL = SkSplitCSV(i)
110900030721     C                   ENDIF
111000030721     C*** RMO
111100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
111200030721     C                   EVAL      VABRMO = SkSplitCSV(i)
111300030721     C                   ENDIF
111400030721     C*** NMO
111500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
111600030721     C                   EVAL      VABNMO = SkSplitCSV(i)
111700030721     C                   ENDIF
111800030721     C*
111900030721     C* Reperisco quindi i campi numerici...
112000040510     C*** NSP
112100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
112200040510     C                   EVAL      PiStr=SkSplitCSV(i)
112300040510     C                   EXSR      CHKNUM
112400040510     C                   IF        PiInt=*on
112500040510     C                   Z-ADD     PiVal         VABNSP
112600040510     C                   ELSE
112700040510     C                   SETON                                        31
112800040510     C                   EVAL      VABNSP = *zeros
112900040510     C                   EVAL      vinmsg = %trimr(vinmsg)
113000040510     C                             + ' ' + 'VABNSP'
113100040510     C                   ENDIF
113200040510     C                   ENDIF
113300071106     C*** RMN
113400071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
113500071106     C                   EVAL      PiStr=SkSplitCSV(i)
113600071106     C                   EXSR      CHKNUM
113700071106     C                   IF        PiInt=*on
113800071106     C                   Z-ADD     PiVal         VABRMN
113900071106     C                   ELSE
114000071106     C                   SETON                                        32
114100071106     C                   EVAL      VABRMN = *zeros
114200071106     C                   EVAL      vinmsg = %trimr(vinmsg)
114300071106     C                             + ' ' + 'VABRMN'
114400071106     C                   ENDIF
114500071106     C                   ENDIF
114600030721     C*** CAD
114700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
114800030721     C                   EVAL      PiStr=SkSplitCSV(i)
114900030721     C                   EXSR      CHKNUM
115000030721     C                   IF        PiInt=*on
115100030721     C                   Z-ADD     PiVal         Num5_0
115200030721     C                   MOVEL(p)  Num5_0        VABCAD
115300030721     C                   ELSE
115400030721     C                   SETON                                        32
115500030721     C                   EVAL      VABCAD = *zeros
115600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115700030721     C                             + ' ' + 'VABCAD'
115800030721     C                   ENDIF
115900030721     C                   ENDIF
116000030721     C*** NCL
116100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
116200030721     C                   EVAL      PiStr=SkSplitCSV(i)
116300030721     C                   EXSR      CHKNUM
116400030721     C                   IF        PiInt=*on
116500030721     C                   Z-ADD     PiVal         VABNCL
116600030721     C                   ELSE
116700030721     C                   SETON                                        32
116800030721     C                   EVAL      VABNCL = *zeros
116900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117000040122     C                             + ' ' + 'VABNCL'
117100030721     C                   ENDIF
117200030721     C                   ENDIF
117300030721     C*** PKB
117400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
117500030721     C                   EVAL      PiStr=SkSplitCSV(i)
117600030721     C                   EXSR      CHKNUM
117700030721     C                   IF        PiNum=*on
117800030721     C                   Z-ADD     PiVal         VABPKB
117900030721     C                   ELSE
118000030721     C                   SETON                                        32
118100030721     C                   EVAL      VABPKB = *zeros
118200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118300030721     C                             + ' ' + 'VABPKB'
118400030721     C                   ENDIF
118500030721     C                   ENDIF
118600030721     C*** CAS
118700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
118800030721     C                   EVAL      PiStr=SkSplitCSV(i)
118900030721     C                   EXSR      CHKNUM
119000030721     C                   IF        PiNum=*on
119100030721     C                   Z-ADD     PiVal         VABCAS
119200030721     C                   ELSE
119300030721     C                   SETON                                        32
119400030721     C                   EVAL      VABCAS = *zeros
119500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119600030721     C                             + ' ' + 'VABCAS'
119700030721     C                   ENDIF
119800030721     C                   ENDIF
119900030721     C*** CCM
120000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
120100030721     C                   EVAL      PiStr=SkSplitCSV(i)
120200030721     C                   EXSR      CHKNUM
120300030721     C                   IF        PiInt=*on      AND
120400030721     C                             PiVal<=9999999 AND
120500030721     C                             PiVal>*zeros
120600030721     C                   Z-ADD     PiVal         VABCCM
120700030721     C                   ELSE
120800030721     C                   SETON                                        32
120900030721     C                   EVAL      VABCCM = *zeros
121000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121100040510     C                             + ' ' + 'VABCCM'
121200030721     C                   ENDIF
121300030721     C                   ENDIF
121400030721     C*** LNP
121500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
121600030721     C                   EVAL      PiStr=SkSplitCSV(i)
121700030721     C                   EXSR      CHKNUM
121800030721     C                   IF        PiInt=*on    AND
121900030721     C                             PiVal<=999   AND
122000030721     C                             PiVal>*zeros
122100030721     C                   Z-ADD     PiVal         VABLNP
122200030721     C                   ELSE
122300040715     C                   SETON                                        32
122400030721     C                   EVAL      VABLNP = *zeros
122500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122600040510     C                             + ' ' + 'VABLNP'
122700030721     C                   ENDIF
122800030721     C                   ENDIF
122900030721     C*** NRS
123000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
123100030721     C                   EVAL      PiStr=SkSplitCSV(i)
123200030721     C                   EXSR      CHKNUM
123300030721     C                   IF        PiInt=*on AND
123400030721     C                             PiVal<=99
123500030721     C                   Z-ADD     PiVal         VABNRS
123600030721     C                   ELSE
123700030721     C                   SETON                                        32
123800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123900030721     C                             + ' ' + 'VABNRS'
124000030721     C                   ENDIF
124100030721     C                   ENDIF
124200030721     C*** LNA
124300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
124400030721     C                   EVAL      PiStr=SkSplitCSV(i)
124500030721     C                   EXSR      CHKNUM
124600030721     C                   IF        PiInt=*on    AND
124700030721     C                             PiVal<=999   AND
124800030721     C                             PiVal>*zeros
124900030721     C                   Z-ADD     PiVal         VABLNA
125000030721     C                   ELSE
125100030721     C                   SETON                                        32
125200030721     C                   EVAL      VABLNA = *zeros
125300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125400030721     C                             + ' ' + 'VABLNA'
125500030721     C                   ENDIF
125600030721     C                   ENDIF
125700030721     C*** CTR
125800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
125900030721     C                   EVAL      PiStr=SkSplitCSV(i)
126000030721     C                   EXSR      CHKNUM
126100030721     C                   IF        PiInt=*on  AND
126200030721     C                             PiVal<=999
126300030721     C                   Z-ADD     PiVal         VABCTR
126400030721     C                   ELSE
126500030721     C                   SETON                                        32
126600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126700030721     C                             + ' ' + 'VABCTR'
126800030721     C                   ENDIF
126900030721     C                   ENDIF
127000030721     C*** IAS
127100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
127200030721     C                   EVAL      PiStr=SkSplitCSV(i)
127300030721     C                   EXSR      CHKNUM
127400030721     C                   IF        PiNum=*on
127500030721     C                   Z-ADD     PiVal         VABIAS
127600030721     C                   ELSE
127700030721     C                   SETON                                        32
127800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127900030721     C                             + ' ' + 'VABIAS'
128000030721     C                   ENDIF
128100030721     C                   ENDIF
128200030721     C*** VLB
128300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
128400030721     C                   EVAL      PiStr=SkSplitCSV(i)
128500030721     C                   EXSR      CHKNUM
128600030721     C                   IF        PiNum=*on
128700030721     C                   Z-ADD     PiVal         VABVLB
128800030721     C                   ELSE
128900030721     C                   SETON                                        32
129000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129100030721     C                             + ' ' + 'VABVLB'
129200030721     C                   ENDIF
129300030721     C                   ENDIF
129400030721     C*** QFT
129500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
129600030721     C                   EVAL      PiStr=SkSplitCSV(i)
129700030721     C                   EXSR      CHKNUM
129800030721     C                   IF        PiNum=*on
129900030721     C                   Z-ADD     PiVal         VABQFT
130000030721     C                   ELSE
130100030721     C                   SETON                                        32
130200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130300030721     C                             + ' ' + 'VABQFT'
130400030721     C                   ENDIF
130500030721     C                   ENDIF
130600030721     C*** NCD
130700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
130800030721     C                   EVAL      PiStr=SkSplitCSV(i)
130900030721     C                   EXSR      CHKNUM
131000030721     C                   IF        PiInt=*on      AND
131100030721     C                             PiVal<=9999999
131200030721     C                   Z-ADD     PiVal         VABNCD
131300030721     C                   ELSE
131400030721     C                   SETON                                        32
131500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131600030721     C                             + ' ' + 'VABNCD'
131700030721     C                   ENDIF
131800030721     C                   ENDIF
131900030721     C*** NCA
132000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
132100030721     C                   EVAL      PiStr=SkSplitCSV(i)
132200030721     C                   EXSR      CHKNUM
132300030721     C                   IF        PiInt=*on      AND
132400030721     C                             PiVal<=9999999
132500030721     C                   Z-ADD     PiVal         VABNCA
132600030721     C                   ELSE
132700030721     C                   SETON                                        32
132800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132900030721     C                             + ' ' + 'VABNCA'
133000030721     C                   ENDIF
133100030721     C                   ENDIF
133200030721     C*** ZNC
133300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
133400030721     C                   EVAL      PiStr=SkSplitCSV(i)
133500030721     C                   EXSR      CHKNUM
133600030721     C                   IF        PiInt=*on AND
133700030721     C                             PiVal<=99
133800030721     C                   Z-ADD     PiVal         VABZNC
133900030721     C                   ELSE
134000030721     C                   SETON                                        32
134100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134200030721     C                             + ' ' + 'VABZNC'
134300030721     C                   ENDIF
134400030721     C                   ENDIF
134500030721     C*** DCR
134600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
134700030721     C                   EVAL      PiStr=SkSplitCSV(i)
134800030721     C                   EXSR      CHKNUM
134900030721     C                   IF        PiInt=*on       AND
135000030721     C                             PiVal<=99999999
135100030721     C                   Z-ADD     PiVal         VABDCR
135200030721     C                   ELSE
135300030721     C                   SETON                                        32
135400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135500030721     C                             + ' ' + 'VABDCR'
135600030721     C                   ENDIF
135700030721     C                   ENDIF
135800030721     C*** HCR
135900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
136000030721     C                   EVAL      PiStr=SkSplitCSV(i)
136100030721     C                   EXSR      CHKNUM
136200030721     C                   IF        PiInt=*on   AND
136300030721     C                             PiVal<=9999
136400030721     C                   Z-ADD     PiVal         VABHCR
136500030721     C                   ELSE
136600030721     C                   SETON                                        32
136700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136800030721     C                             + ' ' + 'VABHCR'
136900030721     C                   ENDIF
137000030721     C                   ENDIF
137100030721     C*** VMD
137200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
137300030721     C                   EVAL      PiStr=SkSplitCSV(i)
137400030721     C                   EXSR      CHKNUM
137500030721     C                   IF        PiNum=*on
137600030721     C                   Z-ADD     PiVal         VABVMD
137700030721     C                   ELSE
137800030721     C                   SETON                                        32
137900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138000030721     C                             + ' ' + 'VABVMD'
138100030721     C                   ENDIF
138200030721     C                   ENDIF
138300030721     C*** ANT
138400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
138500030721     C                   EVAL      PiStr=SkSplitCSV(i)
138600030721     C                   EXSR      CHKNUM
138700030721     C                   IF        PiInt=*on        AND
138800030721     C                             PiVal<=999999999
138900030721     C                   Z-ADD     PiVal         VABANT
139000030721     C                   ELSE
139100030721     C                   SETON                                        32
139200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139300030721     C                             + ' ' + 'VABANT'
139400030721     C                   ENDIF
139500030721     C                   ENDIF
139600030721     C*** CMO
139700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
139800030721     C                   EVAL      PiStr=SkSplitCSV(i)
139900030721     C                   EXSR      CHKNUM
140000030721     C                   IF        PiInt=*on
140100030721     C                   Z-ADD     PiVal         Num5_0
140200030721     C                   MOVEL(p)  Num5_0        VABCMO
140300030721     C                   ELSE
140400030721     C                   SETON                                        32
140500030721     C                   EVAL      VABCMO = *zeros
140600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140700030721     C                             + ' ' + 'VABCMO'
140800030721     C                   ENDIF
140900030721     C                   ENDIF
141000040510     C*
141100040510     C* Reperisco i campi del FIVAT
141200040510     C*** VATNOT
141300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
141400040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
141500040510     C                   ENDIF
141600040510     C*** VATTRC
141700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
141800040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
141900040510     C                   ENDIF
142000060331     C*** VATNOT_A
142100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
142200060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
142300060331     C                   ENDIF
142400060331     C*** VATNOT_B
142500060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
142600060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
142700060331     C                   ENDIF
142800060331     C*** VATNOT_E
142900070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
143000060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
143100060331     C                   ENDIF
143200070823     C*
143300071029     C***  ===>  Gestione campi "particolari" dentro ciclo
143400100419     C*** NSP/RMN/RMA
143500100413     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
143600100419     C                   EVAL      VABRMA = %trim(SkSplitCSV(i))
143700100413     C                   EVAL      PiStr=SkSplitCSV(i)
143800100413     C                   EXSR      CHKNUM
143900100413     C                   IF        PiInt=*on
144000100419     C                   Z-ADD     PiVal         VABNSP
144100100419     C                   Z-ADD     PiVal         VABRMN
144200100413     C                   ELSE
144300100413     C                   SETON                                        32
144400100413     C                   EVAL      VABRMN = *zeros
144500100413     C                   EVAL      vinmsg = %trimr(vinmsg)
144600100419     C                             + ' ' + 'VABNSP VABRMN'
144700100413     C                   ENDIF
144800100326     C                   ENDIF
144900100419     C*** CBO
145000100419     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCBO'
145100100419     C                   SELECT
145200100419     C                   WHEN      %trim(SkSplitCSV(i)) = 'DDP'
145300100419     C                   EVAL      VABCBO = '1'
145400100419     C                   WHEN      %trim(SkSplitCSV(i)) = 'EXW'
145500100419     C                   EVAL      VABCBO = '2'
145600100419     C                   ENDSL
145700100413     C                   ENDIF
145800100419     C*** DCR
145900100419     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABDCR'
146000100419     C                   IF        %trim(SkSplitCSV(i)) <> *blanks
146100100419     C                   EVAL      VABDCR = %dec(
146200100419     C                                      %subst(SkSplitCSV(i):7:4) +
146300100419     C                                      %subst(SkSplitCSV(i):4:2) +
146400100419     C                                      %subst(SkSplitCSV(i):1:2)
146500100419     C                                      :8:0)
146600100419     C                   ENDIF
146700100419     C                   ENDIF
146800100419     C*** TSP
146900100419     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABTSP'
147000100419     C                   SELECT
147100100419     C                   WHEN      %trim(SkSplitCSV(i)) = '22'
147200100419     C                   EVAL      VABTSP = 'C'
147300100419     C                   WHEN      %trim(SkSplitCSV(i)) = '23'
147400100419     C                   EVAL      VABTSP = 'H'
147500100419     C                   ENDSL
147600100419     C                   ENDIF
147700070925     C*
147800071029     C***  <===  -----------------------------------------
147900040510     C*
148000030715     C                   ENDIF
148100030715     C                   EVAL      i = i + 1
148200030715     C                   ENDDO
148300070925     C*
148400071029     C***  ===>  Gestione campi "particolari" fuori ciclo
148500070925     C*
148600071029     C***  <===  -----------------------------------------
148700070925     C*
148800070925     C* Considerazioni finali su CBO/CAS
148900100318     C                   IF        wFlgCAS = '1'    OR
149000100318     C                             VABCAS  > *zeros
149100070925     C                   IF        VABCBO = '1'
149200070925     C                   EVAL      VABCBO = '4'
149300070925     C                   ENDIF
149400070925     C                   IF        VABCBO = '2'
149500070925     C                   EVAL      VABCBO = '6'
149600070925     C                   ENDIF
149700070925     C                   ENDIF
149800070925     C***  <===  ----------------------------
149900040510     C*
150000040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
150100040510     C                   IF        VABNSP = *zeros
150200040510     C                   SETON                                        31
150300040510     C                   ENDIF
150400040715     C*
150500040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
150600040715     C                   IF        VABLNP = *zeros
150700040715     C                   SETON                                        31
150800040715     C                   ENDIF
150900040510     C*
151000040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
151100040510     C                   IF        VABRMN = *zeros
151200040510     C                   Z-ADD     VABNSP        VABRMN
151300040510     C                   ENDIF
151400040510     C*
151500040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
151600040510     C                   IF        VABRMA = *blanks
151700040714     C                   MOVEL     VABRMN        VABRMA
151800040510     C                   ENDIF
151900040128     C*
152000040510     C* Se provincia nn valorizzata la reperisco
152100040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
152200040510     C                   IF        VABPRD = *blanks
152300040128     C                   CLEAR                   TISI95DS
152400040128     C                   EVAL      I95TCN = '3'
152500040128     C                   Z-ADD     datcor        I95DAT
152600040128     C                   EVAL      I95NAR = VABNZD
152700040128     C                   EVAL      I95CAP = VABCAD
152800040128     C                   EVAL      I95LOC = VABLOD
152900040128     C                   CALL      'TISI95R'
153000040128     C                   PARM                    TISI95DS
153100040128     C                   EVAL      VABPRD = O95PRV
153200040510     C                   ENDIF
153300020204     C*
153400020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
153500020204     C                   EXSR      CHKIMPDIV
153600030715     C                   ENDIF
153700020204     C*
153800010607     C                   ENDSR
153900010607     C*----------------------------------------------------*
154000010601
154100020204
154200020204     C*----------------------------------------------------*
154300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
154400020204     C*----------------------------------------------------*
154500020204     C     CHKIMPDIV     BEGSR
154600020204     C*
154700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
154800020204     C                   Z-ADD     *zeros        wrkDec            9 9
154900020204     C*
155000020204     C* Come prima cosa effettuo considerazioni sulla divisa
155100020204     C                   IF        vabIAS > *zeros
155200020204     C                   IF        vabVAS <> 'EUR'
155300020204     C                   EVAL      vabVAS =  'ITL'
155400020204     C                   ENDIF
155500020204     C                   ENDIF
155600020204     C*
155700020204     C                   IF        vabCAS > *zeros
155800020204     C                   IF        vabVCA <> 'EUR'
155900020204     C                   EVAL      vabVCA =  'ITL'
156000020204     C                   ENDIF
156100020204     C                   ENDIF
156200020204     C*
156300020204     C                   IF        vabVMD > *zeros
156400020204     C                   IF        vabVAD <> 'EUR'
156500020204     C                   EVAL      vabVAD =  'ITL'
156600020204     C                   ENDIF
156700020204     C                   ENDIF
156800020204     C*
156900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
157000020204     C                   Z-ADD     vabIAS        wrkDec
157100020204     C                   IF        wrkDec > *zeros
157200020204     C                   IF        vabVAS = 'ITL'
157300020204     C                   EVAL      vabIAS = *zeros
157400020204     C                   ENDIF
157500020204     C                   ENDIF
157600020204     C*
157700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
157800020204     C                   Z-ADD     vabCAS        wrkDec
157900020204     C                   IF        wrkDec > *zeros
158000020204     C                   IF        vabVCA = 'ITL'
158100020204     C                   EVAL      vabCAS = *zeros
158200020204     C                   ENDIF
158300020204     C                   ENDIF
158400020204     C*
158500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
158600020204     C                   Z-ADD     vabVMD        wrkDec
158700020204     C                   IF        wrkDec > *zeros
158800020204     C                   IF        vabVAD = 'ITL'
158900020204     C                   EVAL      vabVMD = *zeros
159000020204     C                   ENDIF
159100020204     C                   ENDIF
159200020204     C*
159300020204     C                   ENDSR
159400020204     C***
159500010330
159600100318
159700100318
159800100318     C*----------------------------------------------------*
159900100318     C*  CONTROLLO NUMERICITA' CAMPI
160000100318     C*----------------------------------------------------*
160100100318     C     CHKNUM        BEGSR
160200100318     C*
160300100318     C                   IF        PiDecChr = *blanks
160400100318     C                   EVAL      PiDecChr = CharNUM
160500100318     C                   ENDIF
160600100318     C*
160700100318     C                   callp     UBISNUM_Check(PiStr
160800100318     C                                          :PiDecChr
160900100318     C                                          :PiVal
161000100318     C                                          :PiNum
161100100318     C                                          :PiInt)
161200100318     C*
161300100318     C                   ENDSR
161400100318     C***
161500010601
161600010601
161700010601
161800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
161900010601     C     repfil        BEGSR
162000010601     C*
162100010601     C                   if        invfil = *zeros and
162200010601     C                             depfil > *zeros and
162300010629     C                             (vinflg = *blanks or
162400010629     C                              vinflg = *zeros)
162500010601     C
162600010601     C                   eval      invfil = depfil
162700010601     C                   endif
162800010601     C*
162900010601     C                   if        depfil <> invfil and
163000010601     C                             invfil > *zeros
163100010601     C                   eval      flgMulti = '1'
163200010601     C                   if        vinflg = *blanks
163300010601     C                   add       1             cntNonEl
163400010601     C                   endif
163500010601     C                   endif
163600010601     C*
163700010601     C                   if        vinflg = '2'
163800010601     C                   eval      flgStato = '2'
163900010601     C                   endif
164000010601     C*
164100010601     C                   ENDSR
164200010601     C***
164300010601
164400010601
164500010601
164600010330
164700040119      /TITLE Invio dei dati al punto operativo.
164800040119     C     invio         BEGSR
164900040119     C*
165000040119     C* 1° invio FIVAT
165100040119     C                   reset                   dscmz
165200040119     C                   move      vlrpoi        cmzdst
165300040119     C                   eval      cmzfld = 'FIVATWWR'
165400040119     C                   eval      cmzmbd = vlrhdl
165500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
165600040119     C***                if        prmfir = *blanks
165700040119     C                   eval      cmzfla = 'FIVAT00F'
165800040119     C                   eval      cmzmba = 'FIVAT00F'
165900040119     C***                else
166000040119     C***                eval      cmzfla = prmfir
166100040119     C***                eval      cmzmba = prmfir
166200040119     C***                endif
166300040119     C                   eval      cmznrr = *zeros
166400040119     C                   move      §ctrokvt      cmznrr
166500040119     C                   eval      cmzlba = vlrfl1
166600040119     C                   call(e)   'TIS711C'
166700040119     C                   parm                    dscmz
166800040119     C                   parm      *blanks       esito
166900040119     C                   if        %error
167000040119     C                             or cmzerr = '1'
167100040119     C                             or esito  = '1'
167200040119     C                   eval      wrkesito = '3'
167300040119     C                   else
167400040119     C*
167500040119     C* 2° invio FIVAB
167600040119     C                   reset                   dscmz
167700040119     C                   move      vlrpoi        cmzdst
167800040119     C                   eval      cmzfld = vlrfou
167900040119     C                   eval      cmzmbd = vlrhdl
168000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
168100040119     C***                if        prmfir = *blanks
168200040119     C                   eval      cmzfla = 'FIVAB00F'
168300040119     C                   eval      cmzmba = 'FIVAB00F'
168400040119     C***                else
168500040119     C***                eval      cmzfla = prmfir
168600040119     C***                eval      cmzmba = prmfir
168700040119     C***                endif
168800040119     C                   eval      cmznrr = *zeros
168900040119     C                   move      §ctrokvb      cmznrr
169000040119     C                   eval      cmzlba = vlrfl1
169100040119     C                   call(e)   'TIS711C'
169200040119     C                   parm                    dscmz
169300040119     C                   parm      *blanks       esito
169400040119     C                   if        %error
169500040119     C                             or cmzerr = '1'
169600040119     C                             or esito  = '1'
169700040119     C                   eval      wrkesito = '3'
169800040119     C                   endif
169900040119     C                   endif
170000040119     C*
170100040119     C                   ENDSR
170200040119     C***
170300010601
170400010601
170500010601
170600010601
170700010601      /TITLE Invio dei dati al punto operativo.
170800010601     C     opeini        BEGSR
170900010601     C*
171000010601     C* Inizializzo flag e contatori operativi
171100010601     C                   movel     '0'           flgGiro           1
171200010601     C                   movel     '0'           flgMulti          1
171300010601     C                   movel     '1'           flgStato          1
171400010615     C                   movel     '0'           flgOk             1
171500010601     C                   z-add     *zeros        cntNonEl         10 0
171600010601     C                   z-add     *zeros        depfil            3 0
171700010601     C                   z-add     *zeros        invfil            3 0
171800010601     C*
171900010601     C                   ENDSR
172000010601     C***
172100070326
172200070326
172300070326
172400070326
172500070326     C     *pssr         BEGSR
172600070329     C*
172700070329     C                   if        %open(tivin00r)
172800070329     C                   close     tivin00r
172900070329     C                   endif
173000070329     C                   if        %open(fivabwwr)
173100070329     C                   close     fivabwwr
173200070329     C                   endif
173300070329     C                   if        %open(fivatwwr)
173400070329     C                   close     fivatwwr
173500070329     C                   endif
173600070326     C*
173700070326     C* Effettuo la chiamata al CLLE preposto
173800070326     C                   call(e)   'TITVVTC'
173900070326     C                   parm                    parccm
174000070326     C                   parm                    parmbr
174100070326     C                   parm      '2'           paropz
174200070326     C*
174300070326     C                   eval      wrkesito = '2'
174400070404     C*
174500070404     C                   seton                                        LR
174600070326     C*
174700070326     C                   ENDSR     '*CANCL'
174800070326     C***
174900100318
175000100318
175100100318
175200100318     C     normCSV       BEGSR
175300100318     C*
175400100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
175500100318     C* separatori campi
175600100318     C                   if        CharTXT <> *blanks
175700100318     C                   z-add     *zeros        wLenVINDTA        4 0
175800100318     C                   z-add     *zeros        wFlgVINDTA        1 0
175900100318     C                   setoff                                       80
176000100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
176100100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
176200100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
176300100318     C                   if        not *in80
176400100318     C                   seton                                        80
176500100318     C                   else
176600100318     C                   setoff                                       80
176700100318     C                   endif
176800100318     C                   endif
176900100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
177000100318     C                             and *in80 = *on
177100100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
177200100318     C                   endif
177300100318     C                   enddo
177400100318     C                   endif
177500100318     C*
177600100318     C                   ENDSR
177700100318     C***
177800070326
177900100326
178000100326
178100100326     C*----------------------------------------------------*
178200100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
178300100326     C*----------------------------------------------------*
178400100326     C     REPNSP        BEGSR
178500100326     C*
178600100326     C* NSP => Stacco un numeratore da AZNUM
178700100326     C                   clear                   TRUL33DS
178800100326     C                   eval      I33OPE = *zeros
178900100326     C                   eval      I33CNU = 302
179000100326     C                   eval      I33NUM = 1
179100100326     C                   movel     TRUL33DS      KPJBU
179200100326     C                   call      'TRUL33R'
179300100326     C                   parm                    KPJBA
179400100326     C                   movel     KPJBU         TRUL33DS
179500100326     C                   if        O33ERR = *zeros
179600100326     C                   z-add     O33NRF        VABNSP
179700100326     C                   z-add     O33NRF        VATNSP
179800100326     C                   else
179900100326     C                   SETON                                        31
180000100326     C                   EVAL      vinmsg = %trimr(vinmsg)
180100100326     C                             + ' ' + 'VABNSP VATNSP'
180200100326     C                   endif
180300100326     C*
180400100326     C                   ENDSR
180500010330
180600100326
180700010330
180800000613     C     *inzsr        BEGSR
180900990910     C*
181000990910     C     *entry        plist
181100990920     C                   parm                    tivlrds
181200990921     C                   parm      wrkesito      esito
181300000724     C                   parm                    prmlit
181400000710     C                   parm                    prmfir
181500010330     C*
181600010330     C* CALCOLA LA DATA CORRENTE
181700100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
181800100318     C                   eval      datcor = %dec(%date() : *ISO)
181900000613     C*
182000000613     C                   ENDSR
182100000613     C***
182200990908
182300100419     Otitv1x1p  E            riepilogo         2
182400990915     O                                              'Upload via Internet'
182500990915     O                                           +1 'Traduzione TIVIN00R -'
182600021025     O                                           +1 'FIVABWWR'
182700990915     O                                           +1 'Testate bolle'
182800990915     O          E            riepilogo   2
182900990915     O                       wrkdata
183000990915     O                       wrkora              +1
183100990915     O                       procname            +1
183200990915     O          E            riepilogo   2
183300990915     O                                              'Cliente..................:'
183400010605     O                       VABCCM        z     +1
183500990915     O          E            riepilogo   2
183600990920     O                                              'Riferimento Strategi.....:'
183700990920     O                       vlrhdl              +1
183800990915     O          E            riepilogo   2
183900990915     O                                              'Giusti...................:'
184000040119     O                       §CTROKVB      2   +  1
184100990915     O          E            riepilogo   2
184200990915     O                                              'Sbagliati e corretti.....:'
184300971022     O                       §CTRMO        2   +  1
184400990915     O          E            riepilogo   2
184500990915     O                                              'Sbagliati e scartati.....:'
184600971022     O                       §CTRNO        2   +  1
184700000613
184800100419     Otitv1x1ps E            testdett          2
184900000613     O                                              'Upload via Internet'
185000000613     O                                           +1 'Traduzione TIVIN00R -'
185100021025     O                                           +1 'FIVABWWR'
185200000613     O                                           +1 'Testate bolle'
185300000616     O          E            testdett    3
185400000613     O                                           +2 'N° rec'
185500000613     O                                           +3 'Anteprima contenuto'
185600000616     O          E            rigadett    2
185700000613     O                       rrnum               +2
185800000621     O                       recko               +3
185900000616     O          E            findett     2
186000000613     O                       wrkdata
186100000613     O                       wrkora              +1
186200000613     O                       procname            +1
186300000616     O          E            findett     2
186400000613     O                                              'Cliente..................:'
186500010605     O                       VABCCM        z     +1
186600000616     O          E            findett     2
186700000613     O                                              'Riferimento Strategi.....:'
186800000613     O                       vlrhdl              +1
186900000616     O          E            findett     2
187000000613     O                                              'Giusti...................:'
187100040119     O                       §CTROKVB      2   +  1
187200000616     O          E            findett     2
187300000613     O                                              'Sbagliati e corretti.....:'
187400000613     O                       §CTRMO        2   +  1
187500000616     O          E            findett     2
187600000613     O                                              'Sbagliati e scartati.....:'
187700000613     O                       §CTRNO        2   +  1
187800000613** CMD - COMANDI CL
187900100419OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1X1P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
188000100419OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1X1PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
188100100419DLTOVR FILE(TITV1X1P TITV1X1PS) LVL(*)
188200010606
188300000613
188400040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
188500100419VABRSD;VABIND;VABLOD;VABCAD;VABPRD;W_VABRMN;FIL001;VABNCL;VABPKB;VABVLB;
188600100419W_VABCBO;FIL002;W_VABDCR;FIL003;W_VABTSP
188700100318
188800100318
188900100318
