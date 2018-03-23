000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200100318     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
000700040119     FFIVATwwr  O    E             DISK    usropn
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
052400100824     C                   MOVEL     *blanks       wNOTE            80
052500030822     C*
052600030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
052700030822     C                   CLEAR                   SkSplitCSV
052800030822     C                   EVAL      i = 1
052900030822     C                   EVAL      posDa = *zeros
053000030822     C                   EVAL      posA  = *zeros
053100080527     C*
053200080527     C                   CLEAR                   FIVABDS
053300080527     C                   CLEAR                   FIVAT000
053400030822     C*
053500030822     C* Reimposto i valori di default
053600030822     C                   EXSR      DEFCAM
053700030822     C*
053800010330     C                   ENDSR
053900010330     C*----------------------------------------------------*
054000030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
054100010330     C*----------------------------------------------------*
054200010330     C     DEFCAM        BEGSR
054300030822     C*
054400030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
054500030715     C* e delimitatore testo.
054600030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
054700030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
054800030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
054900100318     C*
055000100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
055100100318     C                   EVAL      CharSOS = CharNUM
055200030822     C*
055300030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
055400100413     C                   SETOFF                                       5051
055500100413     C                   SELECT
055600100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
055700030715     C                   SETON                                        50
055800100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
055900100413     C                   SETON                                        51
056000100413     C                   ENDSL
056100030822     C*
056200030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
056300030822     C                   EVAL      posDaDft = 1
056400030822     C                   EVAL      posADft  = 0
056500030822     C                   EVAL      wGiroDft = 0
056600030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
056700030822     C                             posDaDft > 0
056800030822     C*
056900030822     C* Gestisco il 1° giro
057000030822     C                   IF        wGiroDft = 0
057100030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
057200030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
057300030822     C* Incremento il contatore dei "giri"
057400030822     C                   EVAL      wGiroDft = 1
057500030822     C                   ELSE
057600030822     C                   EVAL      posDaDft = posADft
057700030822     C                   ENDIF
057800030822     C* Eseguo lo scan x trovare la fine del campo corrente
057900030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
058000030822     C*
058100030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
058200030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
058300030822     C                   IF        posDaDft > 0 AND
058400030822     C                             posADft  > 0
058500030822     C* NCL
058600030822     C                   IF        %subst(
058700030822     C                             %subst(vlrppt:posDaDft+1:
058800030822     C                             posADft-posDaDft-1):1:3)
058900030822     C                             = 'NCL'
059000030822     C                   EVAL      PiStr=%trim(%subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059300030822     C                   EXSR      CHKNUM
059400030822     C                   IF        PiInt=*on
059500030822     C                   Z-ADD     PiVal         VABNCL
059600030822     C                   ENDIF
059700030822     C                   ENDIF
059800030822     C* CCM
059900030822     C                   IF        %subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):1:3)
060200030822     C                             = 'CCM'
060300030822     C                   EVAL      PiStr=%trim(%subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060600030822     C                   EXSR      CHKNUM
060700030822     C                   IF        PiInt=*on
060800030822     C                   Z-ADD     PiVal         VABCCM
060900030822     C                   ENDIF
061000030822     C                   ENDIF
061100030822     C* LNP
061200030822     C                   IF        %subst(
061300030822     C                             %subst(vlrppt:posDaDft+1:
061400030822     C                             posADft-posDaDft-1):1:3)
061500030822     C                             = 'LNP'
061600030822     C                   EVAL      PiStr=%trim(%subst(
061700030822     C                             %subst(vlrppt:posDaDft+1:
061800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061900030822     C                   EXSR      CHKNUM
062000030822     C                   IF        PiInt=*on
062100030822     C                   Z-ADD     PiVal         VABLNP
062200030822     C                   ENDIF
062300030822     C                   ENDIF
062400030822     C* NRS
062500030822     C                   IF        %subst(
062600030822     C                             %subst(vlrppt:posDaDft+1:
062700030822     C                             posADft-posDaDft-1):1:3)
062800030822     C                             = 'NRS'
062900030822     C                   EVAL      PiStr=%trim(%subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063200030822     C                   EXSR      CHKNUM
063300030822     C                   IF        PiInt=*on
063400030822     C                   Z-ADD     PiVal         VABNRS
063500030822     C                   ENDIF
063600030822     C                   ENDIF
063700030822     C* CTR
063800030822     C                   IF        %subst(
063900030822     C                             %subst(vlrppt:posDaDft+1:
064000030822     C                             posADft-posDaDft-1):1:3)
064100030822     C                             = 'CTR'
064200030822     C                   EVAL      PiStr=%trim(%subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064500030822     C                   EXSR      CHKNUM
064600030822     C                   IF        PiInt=*on
064700030822     C                   Z-ADD     PiVal         VABCTR
064800030822     C                   ENDIF
064900030822     C                   ENDIF
065000030822     C* PKB
065100030822     C                   IF        %subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):1:3)
065400030822     C                             = 'PKB'
065500030822     C                   EVAL      PiStr=%trim(%subst(
065600030822     C                             %subst(vlrppt:posDaDft+1:
065700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065800030822     C                   EXSR      CHKNUM
065900030822     C                   IF        PiNum=*on
066000030822     C                   Z-ADD     PiVal         VABPKB
066100030822     C                   ENDIF
066200030822     C                   ENDIF
066300030822     C* VLB
066400030822     C                   IF        %subst(
066500030822     C                             %subst(vlrppt:posDaDft+1:
066600030822     C                             posADft-posDaDft-1):1:3)
066700030822     C                             = 'VLB'
066800030822     C                   EVAL      PiStr=%trim(%subst(
066900030822     C                             %subst(vlrppt:posDaDft+1:
067000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067100030822     C                   EXSR      CHKNUM
067200030822     C                   IF        PiNum=*on
067300030822     C                   Z-ADD     PiVal         VABVLB
067400030822     C                   ENDIF
067500030822     C                   ENDIF
067600030822     C* QFT
067700030822     C                   IF        %subst(
067800030822     C                             %subst(vlrppt:posDaDft+1:
067900030822     C                             posADft-posDaDft-1):1:3)
068000030822     C                             = 'QFT'
068100030822     C                   EVAL      PiStr=%trim(%subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068400030822     C                   EXSR      CHKNUM
068500030822     C                   IF        PiNum=*on
068600030822     C                   Z-ADD     PiVal         VABQFT
068700030822     C                   ENDIF
068800030822     C                   ENDIF
068900030822     C* CBO
069000030822     C                   IF        %subst(
069100030822     C                             %subst(vlrppt:posDaDft+1:
069200030822     C                             posADft-posDaDft-1):1:3)
069300030822     C                             = 'CBO'
069400030822     C                   EVAL      VABCBO=%trim(%subst(
069500030822     C                             %subst(vlrppt:posDaDft+1:
069600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069700030822     C                   ENDIF
069800030822     C* TSP
069900030822     C                   IF        %subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):1:3)
070200030822     C                             = 'TSP'
070300030822     C                   EVAL      VABTSP=%trim(%subst(
070400030822     C                             %subst(vlrppt:posDaDft+1:
070500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070600030822     C                   ENDIF
070700030822     C* VAS
070800030822     C                   IF        %subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):1:3)
071100030822     C                             = 'VAS'
071200030822     C                   EVAL      VABVAS=%trim(%subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071500030822     C                   ENDIF
071600030822     C* VCA
071700030822     C                   IF        %subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):1:3)
072000030822     C                             = 'VCA'
072100030822     C                   EVAL      VABVCA=%trim(%subst(
072200030822     C                             %subst(vlrppt:posDaDft+1:
072300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072400030822     C                   ENDIF
072500070925     C* TIC
072600070925     C                   IF        %subst(
072700070925     C                             %subst(vlrppt:posDaDft+1:
072800070925     C                             posADft-posDaDft-1):1:3)
072900070925     C                             = 'TIC'
073000070925     C                   EVAL      VABTIC=%trim(%subst(
073100070925     C                             %subst(vlrppt:posDaDft+1:
073200070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073300070925     C                   ENDIF
073400030822     C* GCA
073500030822     C                   IF        %subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):1:3)
073800030822     C                             = 'GCA'
073900030822     C                   EVAL      VABGCA=%trim(%subst(
074000030822     C                             %subst(vlrppt:posDaDft+1:
074100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074200030822     C                   ENDIF
074300030822     C* CTM
074400030822     C                   IF        %subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):1:3)
074700030822     C                             = 'CTM'
074800030822     C                   EVAL      VABCTM=%trim(%subst(
074900030822     C                             %subst(vlrppt:posDaDft+1:
075000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075100030822     C                   ENDIF
075200030822     C* FFD
075300030822     C                   IF        %subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):1:3)
075600030822     C                             = 'FFD'
075700030822     C                   EVAL      VABFFD=%trim(%subst(
075800030822     C                             %subst(vlrppt:posDaDft+1:
075900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076000030822     C                   ENDIF
076100030822     C* VAD
076200030822     C                   IF        %subst(
076300030822     C                             %subst(vlrppt:posDaDft+1:
076400030822     C                             posADft-posDaDft-1):1:3)
076500030822     C                             = 'VAD'
076600030822     C                   EVAL      VABVAD=%trim(%subst(
076700030822     C                             %subst(vlrppt:posDaDft+1:
076800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076900030822     C                   ENDIF
077000030822     C* GMA
077100030822     C                   IF        %subst(
077200030822     C                             %subst(vlrppt:posDaDft+1:
077300030822     C                             posADft-posDaDft-1):1:3)
077400030822     C                             = 'GMA'
077500030822     C                   EVAL      VABGMA=%trim(%subst(
077600030822     C                             %subst(vlrppt:posDaDft+1:
077700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077800030822     C                   ENDIF
077900030822     C* GGA
078000030822     C                   IF        %subst(
078100030822     C                             %subst(vlrppt:posDaDft+1:
078200030822     C                             posADft-posDaDft-1):1:3)
078300030822     C                             = 'GGA'
078400030822     C                   EVAL      VABGGA=%trim(%subst(
078500030822     C                             %subst(vlrppt:posDaDft+1:
078600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078700030822     C                   ENDIF
078800030822     C* GVA
078900030822     C                   IF        %subst(
079000030822     C                             %subst(vlrppt:posDaDft+1:
079100030822     C                             posADft-posDaDft-1):1:3)
079200030822     C                             = 'GVA'
079300030822     C                   EVAL      VABGVA=%trim(%subst(
079400030822     C                             %subst(vlrppt:posDaDft+1:
079500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079600030822     C                   ENDIF
079700030822     C* TC1
079800030822     C                   IF        %subst(
079900030822     C                             %subst(vlrppt:posDaDft+1:
080000030822     C                             posADft-posDaDft-1):1:3)
080100030822     C                             = 'TC1'
080200030822     C                   EVAL      VABTC1=%trim(%subst(
080300030822     C                             %subst(vlrppt:posDaDft+1:
080400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080500030822     C                   ENDIF
080600030822     C* TC2
080700030822     C                   IF        %subst(
080800030822     C                             %subst(vlrppt:posDaDft+1:
080900030822     C                             posADft-posDaDft-1):1:3)
081000030822     C                             = 'TC2'
081100030822     C                   EVAL      VABTC2=%trim(%subst(
081200030822     C                             %subst(vlrppt:posDaDft+1:
081300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081400030822     C                   ENDIF
081500040714     C* VATTRC
081600040714     C                   IF        %subst(
081700040714     C                             %subst(vlrppt:posDaDft+1:
081800040714     C                             posADft-posDaDft-1):1:3)
081900040714     C                             = 'TRC'
082000040714     C                   EVAL      VATTRC=%trim(%subst(
082100040714     C                             %subst(vlrppt:posDaDft+1:
082200040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082300040714     C                   ENDIF
082400030822     C* ...
082500030822     C                   ENDIF
082600030822     C                   ENDDO
082700020204     C*
082800010330     C                   ENDSR
082900010607     C*----------------------------------------------------*
083000021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
083100010607     C*----------------------------------------------------*
083200010607     C     IMPVAB        BEGSR
083300010607     C*
083400010607     C                   SETOFF                                       3132
083500030515     C*
083600010607     C                   MOVEL     datcor        VABAAS
083700010607     C                   MOVE      datcor        VABMGS
083800010607     C*
083900030715     C**********
084000030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
084100030715     C**********
084200030715     C                   IF        wGiro = *zeros
084300040113     C*
084400040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
084500040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
084600080429     C                             %trim(intcol(3))+%trim(intcol(4))+
084700080429     C                             %trim(intcol(5))<>*blanks
084800040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
084900080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
085000080429     C                                      %trim(intcol(5))
085100070823     C     ';':CharCSV   XLATE     vindta        vindta
085200040510     C                   ENDIF
085300031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
085400031201     C     minu:maiu     XLATE     vindta        vindta
085500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
085600030715     C                   DOW       posDa <= %len(%trim(vindta))
085700030715     C*
085800030715     C* Gestisco il 1° campo
085900030715     C                   IF        i = 1
086000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
086100030715     C                   EVAL      posDa = 1
086200030715     C* Eseguo lo scan x trovare la fine del primo campo
086300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086400030715     C* A questo punto "estraggo" il campo corrente
086500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086600030715     C                                                    (posA-posDa))
086700030715     C* X i campi successivi al 1°
086800030715     C                   ELSE
086900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
087000030715     C                   EVAL      posDa = posA + 1
087100030715     C* Eseguo lo scan x trovare la fine del campo corrente
087200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087300030715     C* Gestisco l'ultimo campo
087400030715     C                   IF        posA = *zeros
087500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
087600030715     C* A questo punto "estraggo" il campo corrente
087700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087800030715     C                                                    (posA-posDa))
087900030715     C                   LEAVE
088000030715     C                   ELSE
088100030715     C* A questo punto "estraggo" il campo corrente
088200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088300030715     C                                                    (posA-posDa))
088400030715     C                   ENDIF
088500030715     C                   ENDIF
088600030715     C* Incremento il contatore d campo
088700030715     C                   EVAL      i = i +1
088800030715     C                   ENDDO
088900030715     C*
089000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
089100030715     C* delimitazione testo
089200030715     C                   EVAL      i = 1
089300030715     C                   DOW       i < %elem(SkSplitFLD)
089400030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
089500030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
089600030715     C                   EVAL      i = i + 1
089700030715     C                   ENDDO
089800030715     C                   EVAL      wGiro = 1
089900030715     C                   ELSE
090000030715     C                   EVAL      wGiro = 2
090100031204     C**********
090200031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
090300031204     C* d separatore campo
090400031204     C**********
090500031204     C                   EVAL      vindta = %trim(vindta)
090600031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
090700031204     C                   EVAL      vindta = %subst(vindta:2)
090800031204     C                   ENDDO
090900031204     C*
091000031204     C                   Z-ADD     *zeros        lunghInput        4 0
091100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
091200031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
091300031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
091400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
091500031204     C                   ENDDO
091600030715     C**********
091700030715     C* Effettuo lo split del campo dati d input
091800030715     C**********
091900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
092000030715     C                   DOW       posDa <= %len(%trim(vindta))
092100030715     C*
092200030715     C* Gestisco il 1° campo
092300030715     C                   IF        i = 1
092400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
092500030715     C                   EVAL      posDa = 1
092600030715     C* Eseguo lo scan x trovare la fine del primo campo
092700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092800030715     C* A questo punto "estraggo" il campo corrente
092900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093000030715     C                                                    (posA-posDa))
093100030715     C* X i campi successivi al 1°
093200030715     C                   ELSE
093300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
093400030715     C                   EVAL      posDa = posA + 1
093500031204     C* Verifico che nn vi sia il campo nn valorizzato
093600031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
093700031204     C* Se campo nn valorizzato skippo al prossimo
093800031204     C                   EVAL      SkSplitCSV(i) = *blanks
093900031204     C                   EVAL      posA  = posA + 1
094000031204     C                   ELSE
094100030715     C* Eseguo lo scan x trovare la fine del campo corrente
094200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094300030715     C* Gestisco l'ultimo campo
094400030715     C                   IF        posA = *zeros
094500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
094600030715     C* A questo punto "estraggo" il campo corrente
094700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094800030715     C                                                    (posA-posDa))
094900030715     C                   LEAVE
095000030715     C                   ELSE
095100030715     C* A questo punto "estraggo" il campo corrente
095200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095300030715     C                                                    (posA-posDa))
095400030715     C                   ENDIF
095500031204     C                   ENDIF
095600030715     C                   ENDIF
095700030715     C* Incremento il contatore d campo
095800030715     C                   EVAL      i = i +1
095900030715     C                   ENDDO
096000030715     C*
096100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
096200030715     C* delimitazione testo
096300030715     C                   EVAL      i = 1
096400030715     C                   DOW       i < %elem(SkSplitCSV)
096500030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
096600030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
096700030715     C                   EVAL      i = i + 1
096800030715     C                   ENDDO
096900100720     C*
097000100720     C* Stacco un progressivo x numero spedizione
097100100720 xxx C***                EXSR      repNSP
097200100326     C*
097300030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
097400030715     C                   EVAL      i = 1
097500030715     C                   DOW       i < %elem(SkSplitFLD)
097600030715     C*
097700030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
097800030715     C                   IF        SkSplitFLD(i) <> *blanks AND
097900030715     C                             SkSplitCSV(i) <> *blanks
098000030721     C*** ATB
098100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
098200030721     C                   EVAL      VABATB = SkSplitCSV(i)
098300030721     C                   ENDIF
098400030715     C*** RSD
098500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
098600030715     C                   EVAL      VABRSD = SkSplitCSV(i)
098700030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
098800030715     C     '@':'A'       XLATE     VABRSD        VABRSD
098900030715     C* ==
099000030715     C                   ENDIF
099100030721     C*** RD2
099200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
099300030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
099400030721     C                   ENDIF
099500030715     C*** IND
099600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
099700030715     C                   EVAL      VABIND = SkSplitCSV(i)
099800030715     C                   ENDIF
099900030715     C*** LOD
100000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
100100030715     C                   EVAL      VABLOD = SkSplitCSV(i)
100200030715     C                   ENDIF
100300030715     C*** PRD
100400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
100500030715     C                   EVAL      VABPRD = SkSplitCSV(i)
100600030715     C                   ENDIF
100700030721     C*** NZD
100800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
100900030721     C                   EVAL      VABNZD = SkSplitCSV(i)
101000030721     C                   ENDIF
101100030715     C*** RMA
101200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
101300030715     C                   EVAL      VABRMA = SkSplitCSV(i)
101400030715     C                   ENDIF
101500030715     C*** NOT
101600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
101700030715     C                   EVAL      VABNOT = SkSplitCSV(i)
101800030715     C                   ENDIF
101900030715     C*** NT2
102000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
102100030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
102200030715     C                   ENDIF
102300030715     C*** VCA
102400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
102500030715     C                   EVAL      VABVCA = SkSplitCSV(i)
102600030715     C                   ENDIF
102700030715     C*** VAS
102800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
102900030715     C                   EVAL      VABVAS = SkSplitCSV(i)
103000030715     C                   ENDIF
103100030715     C*** FFD
103200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
103300030715     C                   EVAL      VABFFD = SkSplitCSV(i)
103400030715     C                   ENDIF
103500030721     C*** GC1
103600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
103700030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
103800030721     C                   ENDIF
103900030721     C*** GC2
104000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
104100030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
104200030721     C                   ENDIF
104300030721     C*** TSP
104400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
104500030721     C                   EVAL      VABTSP = SkSplitCSV(i)
104600030721     C                   ENDIF
104700040510     C*** CBO
104800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
104900040510     C                   EVAL      VABCBO = SkSplitCSV(i)
105000040510     C                   ENDIF
105100030721     C*** NAS
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
105300030721     C                   EVAL      VABNAS = SkSplitCSV(i)
105400030721     C                   ENDIF
105500030721     C*** TIC
105600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
105700030721     C                   EVAL      VABTIC = SkSplitCSV(i)
105800030721     C                   ENDIF
105900030721     C*** GCA
106000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
106100030721     C                   EVAL      VABGCA = SkSplitCSV(i)
106200030721     C                   ENDIF
106300030721     C*** XCO
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
106500030721     C                   EVAL      VABXCO = SkSplitCSV(i)
106600030721     C                   ENDIF
106700030721     C*** CTM
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
106900030721     C                   EVAL      VABCTM = SkSplitCSV(i)
107000030721     C                   ENDIF
107100030721     C*** TCR
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
107300030721     C                   EVAL      VABTCR = SkSplitCSV(i)
107400030721     C                   ENDIF
107500030721     C*** CTS
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
107700030721     C                   EVAL      VABCTS = SkSplitCSV(i)
107800030721     C                   ENDIF
107900030721     C*** FTM
108000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
108100030721     C                   EVAL      VABFTM = SkSplitCSV(i)
108200030721     C                   ENDIF
108300030721     C*** VAD
108400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
108500030721     C                   EVAL      VABVAD = SkSplitCSV(i)
108600030721     C                   ENDIF
108700030721     C*** GMA
108800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
108900030721     C                   EVAL      VABGMA = SkSplitCSV(i)
109000030721     C                   ENDIF
109100030721     C*** GGA
109200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
109300030721     C                   EVAL      VABGGA = SkSplitCSV(i)
109400030721     C                   ENDIF
109500030721     C*** GVA
109600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
109700030721     C                   EVAL      VABGVA = SkSplitCSV(i)
109800030721     C                   ENDIF
109900030721     C*** TC1
110000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
110100030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
110200030721     C                   ENDIF
110300030721     C*** TC2
110400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
110500030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
110600030721     C                   ENDIF
110700030721     C*** SCL
110800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
110900030721     C                   EVAL      VABSCL = SkSplitCSV(i)
111000030721     C                   ENDIF
111100030721     C*** RMO
111200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
111300030721     C                   EVAL      VABRMO = SkSplitCSV(i)
111400030721     C                   ENDIF
111500030721     C*** NMO
111600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
111700030721     C                   EVAL      VABNMO = SkSplitCSV(i)
111800030721     C                   ENDIF
111900030721     C*
112000030721     C* Reperisco quindi i campi numerici...
112100040510     C*** NSP
112200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
112300040510     C                   EVAL      PiStr=SkSplitCSV(i)
112400040510     C                   EXSR      CHKNUM
112500040510     C                   IF        PiInt=*on
112600040510     C                   Z-ADD     PiVal         VABNSP
112700040510     C                   ELSE
112800040510     C                   SETON                                        31
112900040510     C                   EVAL      VABNSP = *zeros
113000040510     C                   EVAL      vinmsg = %trimr(vinmsg)
113100040510     C                             + ' ' + 'VABNSP'
113200040510     C                   ENDIF
113300040510     C                   ENDIF
113400071106     C*** RMN
113500071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
113600071106     C                   EVAL      PiStr=SkSplitCSV(i)
113700071106     C                   EXSR      CHKNUM
113800071106     C                   IF        PiInt=*on
113900071106     C                   Z-ADD     PiVal         VABRMN
114000071106     C                   ELSE
114100071106     C                   SETON                                        32
114200071106     C                   EVAL      VABRMN = *zeros
114300071106     C                   EVAL      vinmsg = %trimr(vinmsg)
114400071106     C                             + ' ' + 'VABRMN'
114500071106     C                   ENDIF
114600071106     C                   ENDIF
114700030721     C*** CAD
114800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
114900030721     C                   EVAL      PiStr=SkSplitCSV(i)
115000030721     C                   EXSR      CHKNUM
115100030721     C                   IF        PiInt=*on
115200030721     C                   Z-ADD     PiVal         Num5_0
115300030721     C                   MOVEL(p)  Num5_0        VABCAD
115400030721     C                   ELSE
115500030721     C                   SETON                                        32
115600030721     C                   EVAL      VABCAD = *zeros
115700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115800030721     C                             + ' ' + 'VABCAD'
115900030721     C                   ENDIF
116000030721     C                   ENDIF
116100030721     C*** NCL
116200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
116300030721     C                   EVAL      PiStr=SkSplitCSV(i)
116400030721     C                   EXSR      CHKNUM
116500030721     C                   IF        PiInt=*on
116600030721     C                   Z-ADD     PiVal         VABNCL
116700030721     C                   ELSE
116800030721     C                   SETON                                        32
116900030721     C                   EVAL      VABNCL = *zeros
117000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117100040122     C                             + ' ' + 'VABNCL'
117200030721     C                   ENDIF
117300030721     C                   ENDIF
117400030721     C*** PKB
117500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
117600030721     C                   EVAL      PiStr=SkSplitCSV(i)
117700030721     C                   EXSR      CHKNUM
117800030721     C                   IF        PiNum=*on
117900030721     C                   Z-ADD     PiVal         VABPKB
118000030721     C                   ELSE
118100030721     C                   SETON                                        32
118200030721     C                   EVAL      VABPKB = *zeros
118300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118400030721     C                             + ' ' + 'VABPKB'
118500030721     C                   ENDIF
118600030721     C                   ENDIF
118700030721     C*** CAS
118800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
118900030721     C                   EVAL      PiStr=SkSplitCSV(i)
119000030721     C                   EXSR      CHKNUM
119100030721     C                   IF        PiNum=*on
119200030721     C                   Z-ADD     PiVal         VABCAS
119300030721     C                   ELSE
119400030721     C                   SETON                                        32
119500030721     C                   EVAL      VABCAS = *zeros
119600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119700030721     C                             + ' ' + 'VABCAS'
119800030721     C                   ENDIF
119900030721     C                   ENDIF
120000030721     C*** CCM
120100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
120200030721     C                   EVAL      PiStr=SkSplitCSV(i)
120300030721     C                   EXSR      CHKNUM
120400030721     C                   IF        PiInt=*on      AND
120500030721     C                             PiVal<=9999999 AND
120600030721     C                             PiVal>*zeros
120700030721     C                   Z-ADD     PiVal         VABCCM
120800030721     C                   ELSE
120900030721     C                   SETON                                        32
121000030721     C                   EVAL      VABCCM = *zeros
121100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121200040510     C                             + ' ' + 'VABCCM'
121300030721     C                   ENDIF
121400030721     C                   ENDIF
121500030721     C*** LNP
121600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
121700030721     C                   EVAL      PiStr=SkSplitCSV(i)
121800030721     C                   EXSR      CHKNUM
121900030721     C                   IF        PiInt=*on    AND
122000030721     C                             PiVal<=999   AND
122100030721     C                             PiVal>*zeros
122200030721     C                   Z-ADD     PiVal         VABLNP
122300030721     C                   ELSE
122400040715     C                   SETON                                        32
122500030721     C                   EVAL      VABLNP = *zeros
122600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122700040510     C                             + ' ' + 'VABLNP'
122800030721     C                   ENDIF
122900030721     C                   ENDIF
123000030721     C*** NRS
123100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
123200030721     C                   EVAL      PiStr=SkSplitCSV(i)
123300030721     C                   EXSR      CHKNUM
123400030721     C                   IF        PiInt=*on AND
123500030721     C                             PiVal<=99
123600030721     C                   Z-ADD     PiVal         VABNRS
123700030721     C                   ELSE
123800030721     C                   SETON                                        32
123900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124000030721     C                             + ' ' + 'VABNRS'
124100030721     C                   ENDIF
124200030721     C                   ENDIF
124300030721     C*** LNA
124400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
124500030721     C                   EVAL      PiStr=SkSplitCSV(i)
124600030721     C                   EXSR      CHKNUM
124700030721     C                   IF        PiInt=*on    AND
124800030721     C                             PiVal<=999   AND
124900030721     C                             PiVal>*zeros
125000030721     C                   Z-ADD     PiVal         VABLNA
125100030721     C                   ELSE
125200030721     C                   SETON                                        32
125300030721     C                   EVAL      VABLNA = *zeros
125400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125500030721     C                             + ' ' + 'VABLNA'
125600030721     C                   ENDIF
125700030721     C                   ENDIF
125800030721     C*** CTR
125900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
126000030721     C                   EVAL      PiStr=SkSplitCSV(i)
126100030721     C                   EXSR      CHKNUM
126200030721     C                   IF        PiInt=*on  AND
126300030721     C                             PiVal<=999
126400030721     C                   Z-ADD     PiVal         VABCTR
126500030721     C                   ELSE
126600030721     C                   SETON                                        32
126700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126800030721     C                             + ' ' + 'VABCTR'
126900030721     C                   ENDIF
127000030721     C                   ENDIF
127100030721     C*** IAS
127200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
127300030721     C                   EVAL      PiStr=SkSplitCSV(i)
127400030721     C                   EXSR      CHKNUM
127500030721     C                   IF        PiNum=*on
127600030721     C                   Z-ADD     PiVal         VABIAS
127700030721     C                   ELSE
127800030721     C                   SETON                                        32
127900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128000030721     C                             + ' ' + 'VABIAS'
128100030721     C                   ENDIF
128200030721     C                   ENDIF
128300030721     C*** VLB
128400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
128500030721     C                   EVAL      PiStr=SkSplitCSV(i)
128600030721     C                   EXSR      CHKNUM
128700030721     C                   IF        PiNum=*on
128800030721     C                   Z-ADD     PiVal         VABVLB
128900030721     C                   ELSE
129000030721     C                   SETON                                        32
129100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129200030721     C                             + ' ' + 'VABVLB'
129300030721     C                   ENDIF
129400030721     C                   ENDIF
129500030721     C*** QFT
129600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
129700030721     C                   EVAL      PiStr=SkSplitCSV(i)
129800030721     C                   EXSR      CHKNUM
129900030721     C                   IF        PiNum=*on
130000030721     C                   Z-ADD     PiVal         VABQFT
130100030721     C                   ELSE
130200030721     C                   SETON                                        32
130300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130400030721     C                             + ' ' + 'VABQFT'
130500030721     C                   ENDIF
130600030721     C                   ENDIF
130700030721     C*** NCD
130800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
130900030721     C                   EVAL      PiStr=SkSplitCSV(i)
131000030721     C                   EXSR      CHKNUM
131100030721     C                   IF        PiInt=*on      AND
131200030721     C                             PiVal<=9999999
131300030721     C                   Z-ADD     PiVal         VABNCD
131400030721     C                   ELSE
131500030721     C                   SETON                                        32
131600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131700030721     C                             + ' ' + 'VABNCD'
131800030721     C                   ENDIF
131900030721     C                   ENDIF
132000030721     C*** NCA
132100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
132200030721     C                   EVAL      PiStr=SkSplitCSV(i)
132300030721     C                   EXSR      CHKNUM
132400030721     C                   IF        PiInt=*on      AND
132500030721     C                             PiVal<=9999999
132600030721     C                   Z-ADD     PiVal         VABNCA
132700030721     C                   ELSE
132800030721     C                   SETON                                        32
132900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133000030721     C                             + ' ' + 'VABNCA'
133100030721     C                   ENDIF
133200030721     C                   ENDIF
133300030721     C*** ZNC
133400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
133500030721     C                   EVAL      PiStr=SkSplitCSV(i)
133600030721     C                   EXSR      CHKNUM
133700030721     C                   IF        PiInt=*on AND
133800030721     C                             PiVal<=99
133900030721     C                   Z-ADD     PiVal         VABZNC
134000030721     C                   ELSE
134100030721     C                   SETON                                        32
134200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134300030721     C                             + ' ' + 'VABZNC'
134400030721     C                   ENDIF
134500030721     C                   ENDIF
134600030721     C*** DCR
134700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
134800030721     C                   EVAL      PiStr=SkSplitCSV(i)
134900030721     C                   EXSR      CHKNUM
135000030721     C                   IF        PiInt=*on       AND
135100030721     C                             PiVal<=99999999
135200030721     C                   Z-ADD     PiVal         VABDCR
135300030721     C                   ELSE
135400030721     C                   SETON                                        32
135500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135600030721     C                             + ' ' + 'VABDCR'
135700030721     C                   ENDIF
135800030721     C                   ENDIF
135900030721     C*** HCR
136000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
136100030721     C                   EVAL      PiStr=SkSplitCSV(i)
136200030721     C                   EXSR      CHKNUM
136300030721     C                   IF        PiInt=*on   AND
136400030721     C                             PiVal<=9999
136500030721     C                   Z-ADD     PiVal         VABHCR
136600030721     C                   ELSE
136700030721     C                   SETON                                        32
136800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136900030721     C                             + ' ' + 'VABHCR'
137000030721     C                   ENDIF
137100030721     C                   ENDIF
137200030721     C*** VMD
137300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
137400030721     C                   EVAL      PiStr=SkSplitCSV(i)
137500030721     C                   EXSR      CHKNUM
137600030721     C                   IF        PiNum=*on
137700030721     C                   Z-ADD     PiVal         VABVMD
137800030721     C                   ELSE
137900030721     C                   SETON                                        32
138000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138100030721     C                             + ' ' + 'VABVMD'
138200030721     C                   ENDIF
138300030721     C                   ENDIF
138400030721     C*** ANT
138500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
138600030721     C                   EVAL      PiStr=SkSplitCSV(i)
138700030721     C                   EXSR      CHKNUM
138800030721     C                   IF        PiInt=*on        AND
138900030721     C                             PiVal<=999999999
139000030721     C                   Z-ADD     PiVal         VABANT
139100030721     C                   ELSE
139200030721     C                   SETON                                        32
139300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139400030721     C                             + ' ' + 'VABANT'
139500030721     C                   ENDIF
139600030721     C                   ENDIF
139700030721     C*** CMO
139800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
139900030721     C                   EVAL      PiStr=SkSplitCSV(i)
140000030721     C                   EXSR      CHKNUM
140100030721     C                   IF        PiInt=*on
140200030721     C                   Z-ADD     PiVal         Num5_0
140300030721     C                   MOVEL(p)  Num5_0        VABCMO
140400030721     C                   ELSE
140500030721     C                   SETON                                        32
140600030721     C                   EVAL      VABCMO = *zeros
140700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140800030721     C                             + ' ' + 'VABCMO'
140900030721     C                   ENDIF
141000030721     C                   ENDIF
141100040510     C*
141200040510     C* Reperisco i campi del FIVAT
141300040510     C*** VATNOT
141400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
141500040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
141600040510     C                   ENDIF
141700040510     C*** VATTRC
141800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
141900040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
142000040510     C                   ENDIF
142100060331     C*** VATNOT_A
142200060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
142300060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
142400060331     C                   ENDIF
142500060331     C*** VATNOT_B
142600060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
142700060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
142800060331     C                   ENDIF
142900060331     C*** VATNOT_E
143000070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
143100060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
143200060331     C                   ENDIF
143300070823     C*
143400071029     C***  ===>  Gestione campi "particolari" dentro ciclo
143500100720     C*** RMA / NSP
143600100720     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMA'
143700100419     C                   EVAL      VABRMA = %trim(SkSplitCSV(i))
143800100413     C                   EVAL      PiStr=SkSplitCSV(i)
143900100413     C                   EXSR      CHKNUM
144000100413     C                   IF        PiInt=*on
144100100419     C                   Z-ADD     PiVal         VABNSP
144200100413     C                   ELSE
144300100413     C                   SETON                                        32
144400100413     C                   EVAL      vinmsg = %trimr(vinmsg)
144500100720     C                             + ' ' + 'VABNSP'
144600100413     C                   ENDIF
144700100326     C                   ENDIF
144800100720     C*** VLB
144900100720     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABVLB'
145000100720     C                   EVAL      PiStr=SkSplitCSV(i)
145100100720     C                   EXSR      CHKNUM
145200100720     C                   IF        PiNum=*on
145300100720     C                   EVAL      PiVal = PiVal / 1000000                      * da cm3 a m3
145400100720     C                   Z-ADD     PiVal         VABVLB
145500100720     C                   ELSE
145600100720     C                   SETON                                        32
145700100720     C                   EVAL      vinmsg = %trimr(vinmsg)
145800100720     C                             + ' ' + 'VABVLB'
145900100720     C                   ENDIF
146000100720     C                   ENDIF
146100100419     C*** CBO
146200100419     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCBO'
146300100419     C                   SELECT
146400100720     C                   WHEN      %trim(SkSplitCSV(i)) = 'X'
146500100720     C                   EVAL      VABCBO = '4'
146600100419     C                   ENDSL
146700100413     C                   ENDIF
146800100720     C*** NOT / NT2
146900100824     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT_1'
147000100824     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' +
147100100824     C                                     SkSplitCSV(i)
147200100720     C                   ENDIF
147300100824     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT_2'
147400100824     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' +
147500100824     C                                     SkSplitCSV(i)
147600100824     C                   ENDIF
147700070925     C*
147800071029     C***  <===  -----------------------------------------
147900040510     C*
148000030715     C                   ENDIF
148100030715     C                   EVAL      i = i + 1
148200030715     C                   ENDDO
148300070925     C*
148400071029     C***  ===>  Gestione campi "particolari" fuori ciclo
148500100824     C*** NOT / NT2
148600100824     C                   EVAL      wNOTE = %trim(wNOTE)
148700100824     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
148800100824     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
148900070925     C*
149000071029     C***  <===  -----------------------------------------
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
165400040119     C* 1° invio FIVAT
165500040119     C                   reset                   dscmz
165600040119     C                   move      vlrpoi        cmzdst
165700040119     C                   eval      cmzfld = 'FIVATWWR'
165800040119     C                   eval      cmzmbd = vlrhdl
165900040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
166000040119     C***                if        prmfir = *blanks
166100040119     C                   eval      cmzfla = 'FIVAT00F'
166200040119     C                   eval      cmzmba = 'FIVAT00F'
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
167900040119     C* 2° invio FIVAB
168000040119     C                   reset                   dscmz
168100040119     C                   move      vlrpoi        cmzdst
168200040119     C                   eval      cmzfld = vlrfou
168300040119     C                   eval      cmzmbd = vlrhdl
168400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
168500040119     C***                if        prmfir = *blanks
168600040119     C                   eval      cmzfla = 'FIVAB00F'
168700040119     C                   eval      cmzmba = 'FIVAB00F'
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
173400070329     C                   if        %open(fivabwwr)
173500070329     C                   close     fivabwwr
173600070329     C                   endif
173700070329     C                   if        %open(fivatwwr)
173800070329     C                   close     fivatwwr
173900070329     C                   endif
174000070326     C*
174100070326     C* Effettuo la chiamata al CLLE preposto
174200070326     C                   call(e)   'TITVVTC'
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
181900010330     C*
182000010330     C* CALCOLA LA DATA CORRENTE
182100100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
182200100318     C                   eval      datcor = %dec(%date() : *ISO)
182300000613     C*
182400000613     C                   ENDSR
182500000613     C***
182600990908
182700100720     Otitv1y7p  E            riepilogo         2
182800990915     O                                              'Upload via Internet'
182900990915     O                                           +1 'Traduzione TIVIN00R -'
183000021025     O                                           +1 'FIVABWWR'
183100990915     O                                           +1 'Testate bolle'
183200990915     O          E            riepilogo   2
183300990915     O                       wrkdata
183400990915     O                       wrkora              +1
183500990915     O                       procname            +1
183600990915     O          E            riepilogo   2
183700990915     O                                              'Cliente..................:'
183800010605     O                       VABCCM        z     +1
183900990915     O          E            riepilogo   2
184000990920     O                                              'Riferimento Strategi.....:'
184100990920     O                       vlrhdl              +1
184200990915     O          E            riepilogo   2
184300990915     O                                              'Giusti...................:'
184400040119     O                       §CTROKVB      2   +  1
184500990915     O          E            riepilogo   2
184600990915     O                                              'Sbagliati e corretti.....:'
184700971022     O                       §CTRMO        2   +  1
184800990915     O          E            riepilogo   2
184900990915     O                                              'Sbagliati e scartati.....:'
185000971022     O                       §CTRNO        2   +  1
185100000613
185200100720     Otitv1y7ps E            testdett          2
185300000613     O                                              'Upload via Internet'
185400000613     O                                           +1 'Traduzione TIVIN00R -'
185500021025     O                                           +1 'FIVABWWR'
185600000613     O                                           +1 'Testate bolle'
185700000616     O          E            testdett    3
185800000613     O                                           +2 'N° rec'
185900000613     O                                           +3 'Anteprima contenuto'
186000000616     O          E            rigadett    2
186100000613     O                       rrnum               +2
186200000621     O                       recko               +3
186300000616     O          E            findett     2
186400000613     O                       wrkdata
186500000613     O                       wrkora              +1
186600000613     O                       procname            +1
186700000616     O          E            findett     2
186800000613     O                                              'Cliente..................:'
186900010605     O                       VABCCM        z     +1
187000000616     O          E            findett     2
187100000613     O                                              'Riferimento Strategi.....:'
187200000613     O                       vlrhdl              +1
187300000616     O          E            findett     2
187400000613     O                                              'Giusti...................:'
187500040119     O                       §CTROKVB      2   +  1
187600000616     O          E            findett     2
187700000613     O                                              'Sbagliati e corretti.....:'
187800000613     O                       §CTRMO        2   +  1
187900000616     O          E            findett     2
188000000613     O                                              'Sbagliati e scartati.....:'
188100000613     O                       §CTRNO        2   +  1
188200000613** CMD - COMANDI CL
188300100720OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1Y7P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
188400100720OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1Y7PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
188500100720DLTOVR FILE(TITV1Y7P TITV1Y7PS) LVL(*)
188600010606
188700000613
188800040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
188900100921VABRMN;VABNCL;VABPKB;W_VABVLB;W_VABRMA;VABRSD;VABIND;VABLOD;VABCAD;
189000100921VATNOT_A;VATNOT_B;FIL001;W_VABCBO;VABPRD;W_VABNOT_1;W_VABNOT_2;
189100100921VABDCR;
189200100318
189300100318
