000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130114     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
000700040119     FFIVATwwr  O    E             DISK    usropn
000800101018     Ftitv2a8p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070329     F                                     infsr(*pssr)
001100101018     Ftitv2a8ps O    f  198        PRINTER usropn
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
015500101018     C                   if        not %open(titv2a8ps)
015600101018     C                   open      titv2a8ps
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
018700101018     C                   if        %open(titv2a8ps)
018800000616     C                   except    findett
018900101018     C                   close     titv2a8ps
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
020900101018     C                   if        not %open(titv2a8p)
021000101018     C   50              open      titv2a8p
021100990915     C                   endif
021200990915     C*
021300030715     C   50              except    riepilogo
021400990915     C*
021500101018     C                   if        %open(titv2a8p)
021600101018     C   50              close     titv2a8p
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
025400010330     C*
025500030822     C                   do        *HIVAL
025600010330     C*
025700030822     C                   read      tivin00r                               70
025800010618     C*
025900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
026000010618     C                   if        vinflg = '1'
026100010618     C                   eval      flgOk = '1'
026200010618     C                   endif
026300040510     C*
026400040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
026500040510     C                   z-add     *zeros        wLenVINDTA        4 0
026600040510     C                   z-add     *zeros        wFlgVINDTA        1 0
026700040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
026800040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
026900040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
027000040510     C                   z-add     1             wFlgVINDTA
027100040510     C                   leave
027200040510     C                   endif
027300040510     C                   enddo
027400010618     C*
027500040510     C                   if        vindta > *blanks AND
027600040510     C                             wFlgVINDTA = 1
027700010330     C                   add       1             rrnum
027800010330     C*
027900010601     C                   if        *in70 = *off and
028000010330     C                             (vinflg = *blanks
028100010330     C                              or vinflg = '0'
028200010330     C                              or vinflg = '2')
028300010330     C*
028400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
028500010711     C                   if        vinflg = *blanks or vinflg = '0'
028600010711     C                   clear                   vinmsg
028700010711     C                   endif
028800010601     C*
028900070925     C* Aggiungo un byte a blanks all'inizio d ogni record
029000070925     C                   eval      vindta  = ' ' + %trim(vindta)
029100070925     C*
029200080527     C                   exsr      inzvar
029300100318     C                   exsr      normCSV
029400010605     C                   exsr      impvab
029500010601     C*
029600010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
029700010605     C                   eval      depfil = VABLNP
029800010601     C                   exsr      repfil
029900010601     C                   if        depfil = invfil
030000021025     C                   if        vlrpoi = 999
030100030822     C                   move(P)   invfil        VABFGS
030200021025     C                   else
030300030822     C                   move(P)   vlrpoi        VABFGS
030400021025     C                   endif
030500010601     C*
030600010601     C                   exsr      PREELA
030700010601     C*
030800010604     C* Ebbene...
030900010604     C*
031000040119     C  N31              add       1             §CTROKVB
031100040119     C   32              add       1             §CTRMO
031200040119     C   31              add       1             §CTRNO
031300080418     C*
031400030822     C                   if        wGiro = 2
031500080513     C* Scrivo sempre il VAT
031600080513     C  N31              exsr      wrivat
031700080527     C                   exsr      chkvab
031800080513     C                   endif
031900010604     C*
032000010604     C                   if        *in31 = *off and
032100010604     C                             *in32 = *off
032200010604     C                   eval      vinflg = '1'
032300010604     C                   else
032400010604     C                   eval      recko = vindta
032500010604     C                   except    rigadett
032600010604     C                   eval      vinflg = '2'
032700010604     C                   endif
032800010604     C                   endif
032900010601     C*
033000010604     C                   endif
033100010604     C*
033200010330     C                   else
033300010330     C                   eval      vinflg = '1'
033400010330     C                   endif
033500010601     C*
033600010601     C  N70              update    tivin000
033700010330     C*
033800030822     C  N70              enddo
033900040119     C*
034000080527     C* Verifico dati rimasti "in cannna"...
034100080527     C                   seton                                        65
034200080527     C                   exsr      chkvab
034300080527     C*
034400040119     C                   endif                                                  (endif - chkcall)
034500010601     C*
034600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
034700010601     C                   if        cntNonEl = *zeros or
034800010601     C                             flgMulti = '0'
034900010330     C* Se non ci sono record con errori ...
035000010601     C                   if        §ctrno = 0 and
035100010604     C                             §ctrmo = 0 and
035200010601     C                             flgStato <> '2'
035300010330     C* ... restituisco esito OK.
035400010330     C                   eval      wrkesito = '0'
035500010330     C                   else
035600040119     C                   if        §ctrokvb > 0
035700010330     C                   eval      wrkesito = '1'
035800010330     C                   else
035900010615     C                   if        flgOk = '0'
036000010615     C                   eval      wrkesito = '2'
036100010615     C                   else
036200010615     C                   eval      wrkesito = '6'
036300010615     C                   endif
036400010330     C                   endif
036500010330     C                   endif
036600010601     C                   else
036700010601     C                   eval      wrkesito = '9'
036800010601     C                   endif
036900010330     C*
037000010330     C                   if        %open(tivin00r)
037100010330     C                   close     tivin00r
037200010330     C                   endif
037300021025     C                   if        %open(fivabwwr)
037400021025     C                   close     fivabwwr
037500010330     C                   endif
037600040512     C                   if        %open(fivatwwr)
037700040512     C                   close     fivatwwr
037800040512     C                   endif
037900010601     C*
038000010601     C                   if        vlrpoi <> 999
038100010601     C                   eval      invfil = vlrpoi
038200010601     C                   endif
038300010330     C*
038400040119     C                   if        §ctrokvb > 0
038500010601     C                             and invfil > *zeros
038600010330     C                   exsr      invio
038700010330     C                   endif
038800010601     C*
038900010618     C                   if        flgGiro = '1'
039000010601     C                   exsr      endela
039100010618     C                   endif
039200010330     C*
039300010330     C                   ENDSR
039400010330     C***
039500080527
039600080527
039700080527
039800080527     C*----------------------------------------------------*
039900080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
040000080527     C*----------------------------------------------------*
040100100413     C     CHKVAB        BEGSR
040200080527     C*
040300080527     C* Scrivo il VAB solo a rottura d spedizione
040400080527     C                   if        depspe = *zeros
040500080527     C  N31              eval      fivabds_sav = fivabds
040600080527     C                   eval      depspe = vabNSP
040700080527     C                   else
040800080527     C                   if        depspe = vabNSP AND *in65 = *off
040900100413     C   51              add       VABNCL        sav_VABNCL
041000100413     C   51              add       VABPKB        sav_VABPKB
041100100413     C   51              add       VABVLB        sav_VABVLB
041200100720     C   52              z-add     VABNCL        sav_VABNCL
041300100720     C   52              z-add     VABPKB        sav_VABPKB
041400100720     C   52              z-add     VABVLB        sav_VABVLB
041500100720     C   53              add       VABNCL        sav_VABNCL
041600100720     C   53              z-add     VABPKB        sav_VABPKB
041700100720     C   53              z-add     VABVLB        sav_VABVLB
041800080527     C                   else
041900080527     C  N31              eval      fivabds_ok = fivabds_sav
042000080527     C  N31              exsr      wrivab
042100080527     C  N31              eval      fivabds_sav = fivabds
042200080527     C                   eval      depspe = vabNSP
042300080527     C                   endif
042400080527     C                   endif
042500080527     C*
042600080527     C                   ENDSR
042700040119
042800040119
042900040119
043000040119     C*----------------------------------------------------*
043100040119     C*  SCARICAMENTO BUFFER RECORDS VAB
043200040119     C*----------------------------------------------------*
043300040510     C     WRIVAB        BEGSR
043400080513     C*
043500080513     C* Gestisco l'eventuale rottura x numero spedizione
043600080527     C                   if        VABNSP <> depspe or *in65 = *on
043700070301     C                   if        wVATNOT_E <> *blanks
043800070301     C                   eval      VABCTM = '7Q'
043900070301     C                   endif
044000100413     C                   write     fivab000
044100080513     C                   endif
044200040119     C*
044300040119     C                   ENDSR
044400040119
044500040119
044600040119
044700040119     C*----------------------------------------------------*
044800040119     C*  SCARICAMENTO BUFFER RECORDS VAT
044900040119     C*----------------------------------------------------*
045000040510     C     WRIVAT        BEGSR
045100060512     C*
045200060512     C* Inizializzo i campi chiave
045300060512     C                   eval      VATFGS = VABFGS
045400060512     C                   eval      VATAAS = VABAAS
045500060512     C                   eval      VATCCM = VABCCM
045600070823     C                   eval      VATNRS = VABNRS
045700060512     C                   eval      VATNSP = VABNSP
045800060512     C                   eval      VATLNP = VABLNP
045900040714     C*
046000040714     C* Solo se VATNOT valorizzato scarico il buffer
046100040714     C                   if        VATNOT <> *blanks
046200040119     C*
046300040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
046400040510     C                   if        VATTRC = *blanks
046500040510     C                   eval      VATTRC = 'E'
046600040510     C                   endif
046700040510     C                   write     FIVAT000
046800040119     C                   add       1             §CTROKVT
046900040714     C*
047000040714     C                   endif
047100060331     C*
047200060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
047300060331     C                   if        wVATNOT_A <> *blanks
047400060331     C                   eval      VATNOT = wVATNOT_A
047500060331     C                   eval      VATTRC = 'A'
047600060331     C                   write     FIVAT000
047700060331     C                   add       1             §CTROKVT
047800060331     C                   endif
047900060331     C*
048000060331     C                   if        wVATNOT_B <> *blanks
048100060331     C                   eval      VATNOT = wVATNOT_B
048200060331     C                   eval      VATTRC = 'B'
048300060331     C                   write     FIVAT000
048400060331     C                   add       1             §CTROKVT
048500060331     C                   endif
048600060331     C*
048700060331     C                   if        wVATNOT_E <> *blanks
048800060331     C                   eval      VATTRC = 'E'
048900100901     C*
049000100901     C* X ogni collo indicato in testata
049100100909     C                   clear                   TRUL28DSX
049200100909     C                   z-add     *zeros        wWhoIs           11 0
049300100901     C                   z-add     1             wCollo            5 0
049400100909     C                   eval      wWhoIs=%dec(%trim(%subst(wVATNOT_E:3:11))
049500100909     C                                    :11:0)
049600100901     C                   dow       wCollo <= VABNCL
049700100901     C*
049800100901     C* Valorizzo il buffer di scrittura del FIVAT
049900100909     C                   eval      I284STD = 'E10'
050000100909     C                   eval      I284COD = %trim(%editc(wWhoIs:'X'))
050100100909     C                   call      'TRUL28R4'
050200100909     C                   parm                    TRUL28DSX
050300100909     C* Se tutto ok scrivo il "chi sono" corrente
050400100909     C                   if        O284err = *blanks
050500100901     C                   eval      VATNOT = %subst(wVATNOT_E:1:2) +
050600100909     C                                      %trim(O284COD)
050700100901     C                   write     FIVAT000
050800100901     C                   add       1             §CTROKVT
050900100909     C                   endif
051000100901     C*
051100100901     C                   eval      wWhoIs = wWhoIs + 1
051200100901     C                   add       1             wCollo
051300100901     C                   enddo
051400100901     C*
051500060331     C                   endif
051600040119     C*
051700040119     C                   ENDSR
051800040119
051900040119
052000040119
052100040119     C*----------------------------------------------------*
052200040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
052300040119     C*----------------------------------------------------*
052400040119     C     PREVAT        BEGSR
052500040119     C*
052600040119     C* Compongo il nome del membro da dare al FIVATWWR
052700040119     C                   eval      parmbr = vlrhdl
052800040119     C                   movel     'M'           parmbr
052900040510     C                   eval      parccm = vlrksc
053000040119     C                   eval      paropz = '1'
053100040119     C* Effettuo la chiamata al CLLE preposto
053200040119     C                   call(e)   'TITVVTC'
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
055100080527     C*
055200080527     C                   setoff                                       65
055300010330     C*
055400030822     C* Inizializzo variabili di wrk
055500020204     C                   Z-ADD     *zeros        Num5_0            5 0
055600101018     C                   MOVEL     *blanks       wFlgCAS
055700060331     C                   MOVEL     *blanks       wVATNOT_A
055800060331     C                   MOVEL     *blanks       wVATNOT_B
055900060331     C                   MOVEL     *blanks       wVATNOT_E
056000100831     C                   MOVEL     *blanks       wVABRSD
056100100831     C                   MOVEL     *blanks       wVABIND
056200100831     C                   MOVEL     *blanks       wVABCAD
056300100831     C                   MOVEL     *blanks       wVABLOD
056400100831     C                   MOVEL     *blanks       wVABPRD
056500030822     C*
056600030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
056700030822     C                   CLEAR                   SkSplitCSV
056800030822     C                   EVAL      i = 1
056900030822     C                   EVAL      posDa = *zeros
057000030822     C                   EVAL      posA  = *zeros
057100080527     C*
057200080527     C                   CLEAR                   FIVABDS
057300080527     C                   CLEAR                   FIVAT000
057400030822     C*
057500030822     C* Reimposto i valori di default
057600030822     C                   EXSR      DEFCAM
057700030822     C*
057800010330     C                   ENDSR
057900010330     C*----------------------------------------------------*
058000030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
058100010330     C*----------------------------------------------------*
058200010330     C     DEFCAM        BEGSR
058300030822     C*
058400030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
058500030715     C* e delimitatore testo.
058600030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
058700030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
058800030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
058900100318     C*
059000100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
059100100318     C                   EVAL      CharSOS = CharNUM
059200030822     C*
059300030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
059400100720     C                   SETOFF                                       505152
059500100720     C                   SETOFF                                       53
059600100413     C                   SELECT
059700100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
059800100720     C                   SETON                                        50
059900100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
060000100413     C                   SETON                                        51
060100100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
060200100720     C                   SETON                                        52
060300100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
060400100720     C                   SETON                                        53
060500100413     C                   ENDSL
060600030822     C*
060700030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
060800030822     C                   EVAL      posDaDft = 1
060900030822     C                   EVAL      posADft  = 0
061000030822     C                   EVAL      wGiroDft = 0
061100030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
061200030822     C                             posDaDft > 0
061300030822     C*
061400030822     C* Gestisco il 1° giro
061500030822     C                   IF        wGiroDft = 0
061600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
061700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
061800030822     C* Incremento il contatore dei "giri"
061900030822     C                   EVAL      wGiroDft = 1
062000030822     C                   ELSE
062100030822     C                   EVAL      posDaDft = posADft
062200030822     C                   ENDIF
062300030822     C* Eseguo lo scan x trovare la fine del campo corrente
062400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
062500030822     C*
062600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
062700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
062800030822     C                   IF        posDaDft > 0 AND
062900030822     C                             posADft  > 0
063000030822     C* NCL
063100030822     C                   IF        %subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):1:3)
063400030822     C                             = 'NCL'
063500030822     C                   EVAL      PiStr=%trim(%subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063800030822     C                   EXSR      CHKNUM
063900030822     C                   IF        PiInt=*on
064000030822     C                   Z-ADD     PiVal         VABNCL
064100030822     C                   ENDIF
064200030822     C                   ENDIF
064300030822     C* CCM
064400030822     C                   IF        %subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):1:3)
064700030822     C                             = 'CCM'
064800030822     C                   EVAL      PiStr=%trim(%subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065100030822     C                   EXSR      CHKNUM
065200030822     C                   IF        PiInt=*on
065300030822     C                   Z-ADD     PiVal         VABCCM
065400030822     C                   ENDIF
065500030822     C                   ENDIF
065600030822     C* LNP
065700030822     C                   IF        %subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):1:3)
066000030822     C                             = 'LNP'
066100030822     C                   EVAL      PiStr=%trim(%subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066400030822     C                   EXSR      CHKNUM
066500030822     C                   IF        PiInt=*on
066600030822     C                   Z-ADD     PiVal         VABLNP
066700030822     C                   ENDIF
066800030822     C                   ENDIF
066900030822     C* NRS
067000030822     C                   IF        %subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):1:3)
067300030822     C                             = 'NRS'
067400030822     C                   EVAL      PiStr=%trim(%subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067700030822     C                   EXSR      CHKNUM
067800030822     C                   IF        PiInt=*on
067900030822     C                   Z-ADD     PiVal         VABNRS
068000030822     C                   ENDIF
068100030822     C                   ENDIF
068200030822     C* CTR
068300030822     C                   IF        %subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):1:3)
068600030822     C                             = 'CTR'
068700030822     C                   EVAL      PiStr=%trim(%subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069000030822     C                   EXSR      CHKNUM
069100030822     C                   IF        PiInt=*on
069200030822     C                   Z-ADD     PiVal         VABCTR
069300030822     C                   ENDIF
069400030822     C                   ENDIF
069500030822     C* PKB
069600030822     C                   IF        %subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):1:3)
069900030822     C                             = 'PKB'
070000030822     C                   EVAL      PiStr=%trim(%subst(
070100030822     C                             %subst(vlrppt:posDaDft+1:
070200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070300030822     C                   EXSR      CHKNUM
070400030822     C                   IF        PiNum=*on
070500030822     C                   Z-ADD     PiVal         VABPKB
070600030822     C                   ENDIF
070700030822     C                   ENDIF
070800030822     C* VLB
070900030822     C                   IF        %subst(
071000030822     C                             %subst(vlrppt:posDaDft+1:
071100030822     C                             posADft-posDaDft-1):1:3)
071200030822     C                             = 'VLB'
071300030822     C                   EVAL      PiStr=%trim(%subst(
071400030822     C                             %subst(vlrppt:posDaDft+1:
071500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071600030822     C                   EXSR      CHKNUM
071700030822     C                   IF        PiNum=*on
071800030822     C                   Z-ADD     PiVal         VABVLB
071900030822     C                   ENDIF
072000030822     C                   ENDIF
072100030822     C* QFT
072200030822     C                   IF        %subst(
072300030822     C                             %subst(vlrppt:posDaDft+1:
072400030822     C                             posADft-posDaDft-1):1:3)
072500030822     C                             = 'QFT'
072600030822     C                   EVAL      PiStr=%trim(%subst(
072700030822     C                             %subst(vlrppt:posDaDft+1:
072800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072900030822     C                   EXSR      CHKNUM
073000030822     C                   IF        PiNum=*on
073100030822     C                   Z-ADD     PiVal         VABQFT
073200030822     C                   ENDIF
073300030822     C                   ENDIF
073400030822     C* CBO
073500030822     C                   IF        %subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):1:3)
073800030822     C                             = 'CBO'
073900030822     C                   EVAL      VABCBO=%trim(%subst(
074000030822     C                             %subst(vlrppt:posDaDft+1:
074100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074200030822     C                   ENDIF
074300030822     C* TSP
074400030822     C                   IF        %subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):1:3)
074700030822     C                             = 'TSP'
074800030822     C                   EVAL      VABTSP=%trim(%subst(
074900030822     C                             %subst(vlrppt:posDaDft+1:
075000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075100030822     C                   ENDIF
075200030822     C* VAS
075300030822     C                   IF        %subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):1:3)
075600030822     C                             = 'VAS'
075700030822     C                   EVAL      VABVAS=%trim(%subst(
075800030822     C                             %subst(vlrppt:posDaDft+1:
075900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076000030822     C                   ENDIF
076100030822     C* VCA
076200030822     C                   IF        %subst(
076300030822     C                             %subst(vlrppt:posDaDft+1:
076400030822     C                             posADft-posDaDft-1):1:3)
076500030822     C                             = 'VCA'
076600030822     C                   EVAL      VABVCA=%trim(%subst(
076700030822     C                             %subst(vlrppt:posDaDft+1:
076800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076900030822     C                   ENDIF
077000070925     C* TIC
077100070925     C                   IF        %subst(
077200070925     C                             %subst(vlrppt:posDaDft+1:
077300070925     C                             posADft-posDaDft-1):1:3)
077400070925     C                             = 'TIC'
077500070925     C                   EVAL      VABTIC=%trim(%subst(
077600070925     C                             %subst(vlrppt:posDaDft+1:
077700070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077800070925     C                   ENDIF
077900030822     C* GCA
078000030822     C                   IF        %subst(
078100030822     C                             %subst(vlrppt:posDaDft+1:
078200030822     C                             posADft-posDaDft-1):1:3)
078300030822     C                             = 'GCA'
078400030822     C                   EVAL      VABGCA=%trim(%subst(
078500030822     C                             %subst(vlrppt:posDaDft+1:
078600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078700030822     C                   ENDIF
078800030822     C* CTM
078900030822     C                   IF        %subst(
079000030822     C                             %subst(vlrppt:posDaDft+1:
079100030822     C                             posADft-posDaDft-1):1:3)
079200030822     C                             = 'CTM'
079300030822     C                   EVAL      VABCTM=%trim(%subst(
079400030822     C                             %subst(vlrppt:posDaDft+1:
079500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079600030822     C                   ENDIF
079700030822     C* FFD
079800030822     C                   IF        %subst(
079900030822     C                             %subst(vlrppt:posDaDft+1:
080000030822     C                             posADft-posDaDft-1):1:3)
080100030822     C                             = 'FFD'
080200030822     C                   EVAL      VABFFD=%trim(%subst(
080300030822     C                             %subst(vlrppt:posDaDft+1:
080400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080500030822     C                   ENDIF
080600030822     C* VAD
080700030822     C                   IF        %subst(
080800030822     C                             %subst(vlrppt:posDaDft+1:
080900030822     C                             posADft-posDaDft-1):1:3)
081000030822     C                             = 'VAD'
081100030822     C                   EVAL      VABVAD=%trim(%subst(
081200030822     C                             %subst(vlrppt:posDaDft+1:
081300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081400030822     C                   ENDIF
081500030822     C* GMA
081600030822     C                   IF        %subst(
081700030822     C                             %subst(vlrppt:posDaDft+1:
081800030822     C                             posADft-posDaDft-1):1:3)
081900030822     C                             = 'GMA'
082000030822     C                   EVAL      VABGMA=%trim(%subst(
082100030822     C                             %subst(vlrppt:posDaDft+1:
082200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082300030822     C                   ENDIF
082400030822     C* GGA
082500030822     C                   IF        %subst(
082600030822     C                             %subst(vlrppt:posDaDft+1:
082700030822     C                             posADft-posDaDft-1):1:3)
082800030822     C                             = 'GGA'
082900030822     C                   EVAL      VABGGA=%trim(%subst(
083000030822     C                             %subst(vlrppt:posDaDft+1:
083100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083200030822     C                   ENDIF
083300030822     C* GVA
083400030822     C                   IF        %subst(
083500030822     C                             %subst(vlrppt:posDaDft+1:
083600030822     C                             posADft-posDaDft-1):1:3)
083700030822     C                             = 'GVA'
083800030822     C                   EVAL      VABGVA=%trim(%subst(
083900030822     C                             %subst(vlrppt:posDaDft+1:
084000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084100030822     C                   ENDIF
084200030822     C* TC1
084300030822     C                   IF        %subst(
084400030822     C                             %subst(vlrppt:posDaDft+1:
084500030822     C                             posADft-posDaDft-1):1:3)
084600030822     C                             = 'TC1'
084700030822     C                   EVAL      VABTC1=%trim(%subst(
084800030822     C                             %subst(vlrppt:posDaDft+1:
084900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085000030822     C                   ENDIF
085100030822     C* TC2
085200030822     C                   IF        %subst(
085300030822     C                             %subst(vlrppt:posDaDft+1:
085400030822     C                             posADft-posDaDft-1):1:3)
085500030822     C                             = 'TC2'
085600030822     C                   EVAL      VABTC2=%trim(%subst(
085700030822     C                             %subst(vlrppt:posDaDft+1:
085800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085900030822     C                   ENDIF
086000040714     C* VATTRC
086100040714     C                   IF        %subst(
086200040714     C                             %subst(vlrppt:posDaDft+1:
086300040714     C                             posADft-posDaDft-1):1:3)
086400040714     C                             = 'TRC'
086500040714     C                   EVAL      VATTRC=%trim(%subst(
086600040714     C                             %subst(vlrppt:posDaDft+1:
086700040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086800040714     C                   ENDIF
086900030822     C* ...
087000030822     C                   ENDIF
087100030822     C                   ENDDO
087200020204     C*
087300010330     C                   ENDSR
087400010607     C*----------------------------------------------------*
087500021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
087600010607     C*----------------------------------------------------*
087700010607     C     IMPVAB        BEGSR
087800010607     C*
087900010607     C                   SETOFF                                       3132
088000030515     C*
088100010607     C                   MOVEL     datcor        VABAAS
088200010607     C                   MOVE      datcor        VABMGS
088300010607     C*
088400030715     C**********
088500030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
088600030715     C**********
088700030715     C                   IF        wGiro = *zeros
088800040113     C*
088900040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
089000040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
089100080429     C                             %trim(intcol(3))+%trim(intcol(4))+
089200080429     C                             %trim(intcol(5))<>*blanks
089300040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
089400080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
089500080429     C                                      %trim(intcol(5))
089600070823     C     ';':CharCSV   XLATE     vindta        vindta
089700040510     C                   ENDIF
089800031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
089900031201     C     minu:maiu     XLATE     vindta        vindta
090000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
090100030715     C                   DOW       posDa <= %len(%trim(vindta))
090200030715     C*
090300030715     C* Gestisco il 1° campo
090400030715     C                   IF        i = 1
090500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
090600030715     C                   EVAL      posDa = 1
090700030715     C* Eseguo lo scan x trovare la fine del primo campo
090800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090900030715     C* A questo punto "estraggo" il campo corrente
091000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
091100030715     C                                                    (posA-posDa))
091200030715     C* X i campi successivi al 1°
091300030715     C                   ELSE
091400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
091500030715     C                   EVAL      posDa = posA + 1
091600030715     C* Eseguo lo scan x trovare la fine del campo corrente
091700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091800030715     C* Gestisco l'ultimo campo
091900030715     C                   IF        posA = *zeros
092000030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
092100030715     C* A questo punto "estraggo" il campo corrente
092200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
092300030715     C                                                    (posA-posDa))
092400030715     C                   LEAVE
092500030715     C                   ELSE
092600030715     C* A questo punto "estraggo" il campo corrente
092700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
092800030715     C                                                    (posA-posDa))
092900030715     C                   ENDIF
093000030715     C                   ENDIF
093100030715     C* Incremento il contatore d campo
093200030715     C                   EVAL      i = i +1
093300030715     C                   ENDDO
093400030715     C*
093500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
093600030715     C* delimitazione testo
093700030715     C                   EVAL      i = 1
093800030715     C                   DOW       i < %elem(SkSplitFLD)
093900030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
094000030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
094100030715     C                   EVAL      i = i + 1
094200030715     C                   ENDDO
094300030715     C                   EVAL      wGiro = 1
094400030715     C                   ELSE
094500030715     C                   EVAL      wGiro = 2
094600031204     C**********
094700031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
094800031204     C* d separatore campo
094900031204     C**********
095000031204     C                   EVAL      vindta = %trim(vindta)
095100031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
095200031204     C                   EVAL      vindta = %subst(vindta:2)
095300031204     C                   ENDDO
095400031204     C*
095500031204     C                   Z-ADD     *zeros        lunghInput        4 0
095600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
095700031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
095800031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
095900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
096000031204     C                   ENDDO
096100030715     C**********
096200030715     C* Effettuo lo split del campo dati d input
096300030715     C**********
096400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
096500030715     C                   DOW       posDa <= %len(%trim(vindta))
096600030715     C*
096700030715     C* Gestisco il 1° campo
096800030715     C                   IF        i = 1
096900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
097000030715     C                   EVAL      posDa = 1
097100030715     C* Eseguo lo scan x trovare la fine del primo campo
097200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
097300030715     C* A questo punto "estraggo" il campo corrente
097400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
097500030715     C                                                    (posA-posDa))
097600030715     C* X i campi successivi al 1°
097700030715     C                   ELSE
097800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
097900030715     C                   EVAL      posDa = posA + 1
098000031204     C* Verifico che nn vi sia il campo nn valorizzato
098100031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
098200031204     C* Se campo nn valorizzato skippo al prossimo
098300031204     C                   EVAL      SkSplitCSV(i) = *blanks
098400031204     C                   EVAL      posA  = posA + 1
098500031204     C                   ELSE
098600030715     C* Eseguo lo scan x trovare la fine del campo corrente
098700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
098800030715     C* Gestisco l'ultimo campo
098900030715     C                   IF        posA = *zeros
099000030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
099100030715     C* A questo punto "estraggo" il campo corrente
099200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
099300030715     C                                                    (posA-posDa))
099400030715     C                   LEAVE
099500030715     C                   ELSE
099600030715     C* A questo punto "estraggo" il campo corrente
099700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
099800030715     C                                                    (posA-posDa))
099900030715     C                   ENDIF
100000031204     C                   ENDIF
100100030715     C                   ENDIF
100200030715     C* Incremento il contatore d campo
100300030715     C                   EVAL      i = i +1
100400030715     C                   ENDDO
100500030715     C*
100600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
100700030715     C* delimitazione testo
100800030715     C                   EVAL      i = 1
100900030715     C                   DOW       i < %elem(SkSplitCSV)
101000030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
101100030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
101200030715     C                   EVAL      i = i + 1
101300030715     C                   ENDDO
101400100720     C*
101500100720     C* Stacco un progressivo x numero spedizione
101600100720 xxx C***                EXSR      repNSP
101700100326     C*
101800030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
101900030715     C                   EVAL      i = 1
102000030715     C                   DOW       i < %elem(SkSplitFLD)
102100030715     C*
102200030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
102300030715     C                   IF        SkSplitFLD(i) <> *blanks AND
102400030715     C                             SkSplitCSV(i) <> *blanks
102500030721     C*** ATB
102600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
102700030721     C                   EVAL      VABATB = SkSplitCSV(i)
102800030721     C                   ENDIF
102900030715     C*** RSD
103000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
103100030715     C                   EVAL      VABRSD = SkSplitCSV(i)
103200030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
103300030715     C     '@':'A'       XLATE     VABRSD        VABRSD
103400030715     C* ==
103500030715     C                   ENDIF
103600030721     C*** RD2
103700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
103800030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
103900030721     C                   ENDIF
104000030715     C*** IND
104100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
104200030715     C                   EVAL      VABIND = SkSplitCSV(i)
104300030715     C                   ENDIF
104400030715     C*** LOD
104500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
104600030715     C                   EVAL      VABLOD = SkSplitCSV(i)
104700030715     C                   ENDIF
104800030715     C*** PRD
104900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
105000030715     C                   EVAL      VABPRD = SkSplitCSV(i)
105100030715     C                   ENDIF
105200030721     C*** NZD
105300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
105400030721     C                   EVAL      VABNZD = SkSplitCSV(i)
105500030721     C                   ENDIF
105600030715     C*** RMA
105700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
105800030715     C                   EVAL      VABRMA = SkSplitCSV(i)
105900030715     C                   ENDIF
106000030715     C*** NOT
106100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
106200030715     C                   EVAL      VABNOT = SkSplitCSV(i)
106300030715     C                   ENDIF
106400030715     C*** NT2
106500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
106600030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
106700030715     C                   ENDIF
106800030715     C*** VCA
106900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
107000030715     C                   EVAL      VABVCA = SkSplitCSV(i)
107100030715     C                   ENDIF
107200030715     C*** VAS
107300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
107400030715     C                   EVAL      VABVAS = SkSplitCSV(i)
107500030715     C                   ENDIF
107600030715     C*** FFD
107700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
107800030715     C                   EVAL      VABFFD = SkSplitCSV(i)
107900030715     C                   ENDIF
108000030721     C*** GC1
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
108200030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
108300030721     C                   ENDIF
108400030721     C*** GC2
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
108600030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
108700030721     C                   ENDIF
108800030721     C*** TSP
108900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
109000030721     C                   EVAL      VABTSP = SkSplitCSV(i)
109100030721     C                   ENDIF
109200040510     C*** CBO
109300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
109400040510     C                   EVAL      VABCBO = SkSplitCSV(i)
109500040510     C                   ENDIF
109600030721     C*** NAS
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
109800030721     C                   EVAL      VABNAS = SkSplitCSV(i)
109900030721     C                   ENDIF
110000030721     C*** TIC
110100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
110200030721     C                   EVAL      VABTIC = SkSplitCSV(i)
110300030721     C                   ENDIF
110400030721     C*** GCA
110500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
110600030721     C                   EVAL      VABGCA = SkSplitCSV(i)
110700030721     C                   ENDIF
110800030721     C*** XCO
110900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
111000030721     C                   EVAL      VABXCO = SkSplitCSV(i)
111100030721     C                   ENDIF
111200030721     C*** CTM
111300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
111400030721     C                   EVAL      VABCTM = SkSplitCSV(i)
111500030721     C                   ENDIF
111600030721     C*** TCR
111700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
111800030721     C                   EVAL      VABTCR = SkSplitCSV(i)
111900030721     C                   ENDIF
112000030721     C*** CTS
112100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
112200030721     C                   EVAL      VABCTS = SkSplitCSV(i)
112300030721     C                   ENDIF
112400030721     C*** FTM
112500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
112600030721     C                   EVAL      VABFTM = SkSplitCSV(i)
112700030721     C                   ENDIF
112800030721     C*** VAD
112900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
113000030721     C                   EVAL      VABVAD = SkSplitCSV(i)
113100030721     C                   ENDIF
113200030721     C*** GMA
113300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
113400030721     C                   EVAL      VABGMA = SkSplitCSV(i)
113500030721     C                   ENDIF
113600030721     C*** GGA
113700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
113800030721     C                   EVAL      VABGGA = SkSplitCSV(i)
113900030721     C                   ENDIF
114000030721     C*** GVA
114100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
114200030721     C                   EVAL      VABGVA = SkSplitCSV(i)
114300030721     C                   ENDIF
114400030721     C*** TC1
114500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
114600030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
114700030721     C                   ENDIF
114800030721     C*** TC2
114900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
115000030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
115100030721     C                   ENDIF
115200030721     C*** SCL
115300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
115400030721     C                   EVAL      VABSCL = SkSplitCSV(i)
115500030721     C                   ENDIF
115600030721     C*** RMO
115700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
115800030721     C                   EVAL      VABRMO = SkSplitCSV(i)
115900030721     C                   ENDIF
116000030721     C*** NMO
116100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
116200030721     C                   EVAL      VABNMO = SkSplitCSV(i)
116300030721     C                   ENDIF
116400030721     C*
116500030721     C* Reperisco quindi i campi numerici...
116600040510     C*** NSP
116700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
116800040510     C                   EVAL      PiStr=SkSplitCSV(i)
116900040510     C                   EXSR      CHKNUM
117000040510     C                   IF        PiInt=*on
117100040510     C                   Z-ADD     PiVal         VABNSP
117200040510     C                   ELSE
117300040510     C                   SETON                                        31
117400040510     C                   EVAL      VABNSP = *zeros
117500040510     C                   EVAL      vinmsg = %trimr(vinmsg)
117600040510     C                             + ' ' + 'VABNSP'
117700040510     C                   ENDIF
117800040510     C                   ENDIF
117900071106     C*** RMN
118000071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
118100071106     C                   EVAL      PiStr=SkSplitCSV(i)
118200071106     C                   EXSR      CHKNUM
118300071106     C                   IF        PiInt=*on
118400071106     C                   Z-ADD     PiVal         VABRMN
118500071106     C                   ELSE
118600071106     C                   SETON                                        32
118700071106     C                   EVAL      VABRMN = *zeros
118800071106     C                   EVAL      vinmsg = %trimr(vinmsg)
118900071106     C                             + ' ' + 'VABRMN'
119000071106     C                   ENDIF
119100071106     C                   ENDIF
119200030721     C*** CAD
119300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
119400030721     C                   EVAL      PiStr=SkSplitCSV(i)
119500030721     C                   EXSR      CHKNUM
119600030721     C                   IF        PiInt=*on
119700030721     C                   Z-ADD     PiVal         Num5_0
119800030721     C                   MOVEL(p)  Num5_0        VABCAD
119900030721     C                   ELSE
120000030721     C                   SETON                                        32
120100030721     C                   EVAL      VABCAD = *zeros
120200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120300030721     C                             + ' ' + 'VABCAD'
120400030721     C                   ENDIF
120500030721     C                   ENDIF
120600030721     C*** NCL
120700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
120800030721     C                   EVAL      PiStr=SkSplitCSV(i)
120900030721     C                   EXSR      CHKNUM
121000030721     C                   IF        PiInt=*on
121100030721     C                   Z-ADD     PiVal         VABNCL
121200030721     C                   ELSE
121300030721     C                   SETON                                        32
121400030721     C                   EVAL      VABNCL = *zeros
121500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121600040122     C                             + ' ' + 'VABNCL'
121700030721     C                   ENDIF
121800030721     C                   ENDIF
121900030721     C*** PKB
122000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
122100030721     C                   EVAL      PiStr=SkSplitCSV(i)
122200030721     C                   EXSR      CHKNUM
122300030721     C                   IF        PiNum=*on
122400030721     C                   Z-ADD     PiVal         VABPKB
122500030721     C                   ELSE
122600030721     C                   SETON                                        32
122700030721     C                   EVAL      VABPKB = *zeros
122800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122900030721     C                             + ' ' + 'VABPKB'
123000030721     C                   ENDIF
123100030721     C                   ENDIF
123200030721     C*** CAS
123300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
123400101018     C                   IF        SkSplitCSV(i) <> *blanks AND
123500101018     C                             SkSplitCSV(i) <> *zeros
123600101018     C                   EVAL      wFlgCAS = '1'
123700101018     C                   ENDIF
123800030721     C                   EVAL      PiStr=SkSplitCSV(i)
123900030721     C                   EXSR      CHKNUM
124000030721     C                   IF        PiNum=*on
124100030721     C                   Z-ADD     PiVal         VABCAS
124200030721     C                   ELSE
124300030721     C                   SETON                                        32
124400030721     C                   EVAL      VABCAS = *zeros
124500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124600030721     C                             + ' ' + 'VABCAS'
124700030721     C                   ENDIF
124800030721     C                   ENDIF
124900030721     C*** CCM
125000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
125100030721     C                   EVAL      PiStr=SkSplitCSV(i)
125200030721     C                   EXSR      CHKNUM
125300030721     C                   IF        PiInt=*on      AND
125400030721     C                             PiVal<=9999999 AND
125500030721     C                             PiVal>*zeros
125600030721     C                   Z-ADD     PiVal         VABCCM
125700030721     C                   ELSE
125800030721     C                   SETON                                        32
125900030721     C                   EVAL      VABCCM = *zeros
126000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126100040510     C                             + ' ' + 'VABCCM'
126200030721     C                   ENDIF
126300030721     C                   ENDIF
126400030721     C*** LNP
126500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
126600030721     C                   EVAL      PiStr=SkSplitCSV(i)
126700030721     C                   EXSR      CHKNUM
126800030721     C                   IF        PiInt=*on    AND
126900030721     C                             PiVal<=999   AND
127000030721     C                             PiVal>*zeros
127100030721     C                   Z-ADD     PiVal         VABLNP
127200030721     C                   ELSE
127300040715     C                   SETON                                        32
127400030721     C                   EVAL      VABLNP = *zeros
127500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127600040510     C                             + ' ' + 'VABLNP'
127700030721     C                   ENDIF
127800030721     C                   ENDIF
127900030721     C*** NRS
128000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
128100030721     C                   EVAL      PiStr=SkSplitCSV(i)
128200030721     C                   EXSR      CHKNUM
128300030721     C                   IF        PiInt=*on AND
128400030721     C                             PiVal<=99
128500030721     C                   Z-ADD     PiVal         VABNRS
128600030721     C                   ELSE
128700030721     C                   SETON                                        32
128800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128900030721     C                             + ' ' + 'VABNRS'
129000030721     C                   ENDIF
129100030721     C                   ENDIF
129200030721     C*** LNA
129300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
129400030721     C                   EVAL      PiStr=SkSplitCSV(i)
129500030721     C                   EXSR      CHKNUM
129600030721     C                   IF        PiInt=*on    AND
129700030721     C                             PiVal<=999   AND
129800030721     C                             PiVal>*zeros
129900030721     C                   Z-ADD     PiVal         VABLNA
130000030721     C                   ELSE
130100030721     C                   SETON                                        32
130200030721     C                   EVAL      VABLNA = *zeros
130300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130400030721     C                             + ' ' + 'VABLNA'
130500030721     C                   ENDIF
130600030721     C                   ENDIF
130700030721     C*** CTR
130800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
130900030721     C                   EVAL      PiStr=SkSplitCSV(i)
131000030721     C                   EXSR      CHKNUM
131100030721     C                   IF        PiInt=*on  AND
131200030721     C                             PiVal<=999
131300030721     C                   Z-ADD     PiVal         VABCTR
131400030721     C                   ELSE
131500030721     C                   SETON                                        32
131600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131700030721     C                             + ' ' + 'VABCTR'
131800030721     C                   ENDIF
131900030721     C                   ENDIF
132000030721     C*** IAS
132100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
132200030721     C                   EVAL      PiStr=SkSplitCSV(i)
132300030721     C                   EXSR      CHKNUM
132400030721     C                   IF        PiNum=*on
132500030721     C                   Z-ADD     PiVal         VABIAS
132600030721     C                   ELSE
132700030721     C                   SETON                                        32
132800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132900030721     C                             + ' ' + 'VABIAS'
133000030721     C                   ENDIF
133100030721     C                   ENDIF
133200030721     C*** VLB
133300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
133400030721     C                   EVAL      PiStr=SkSplitCSV(i)
133500030721     C                   EXSR      CHKNUM
133600030721     C                   IF        PiNum=*on
133700030721     C                   Z-ADD     PiVal         VABVLB
133800030721     C                   ELSE
133900030721     C                   SETON                                        32
134000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134100030721     C                             + ' ' + 'VABVLB'
134200030721     C                   ENDIF
134300030721     C                   ENDIF
134400030721     C*** QFT
134500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
134600030721     C                   EVAL      PiStr=SkSplitCSV(i)
134700030721     C                   EXSR      CHKNUM
134800030721     C                   IF        PiNum=*on
134900030721     C                   Z-ADD     PiVal         VABQFT
135000030721     C                   ELSE
135100030721     C                   SETON                                        32
135200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135300030721     C                             + ' ' + 'VABQFT'
135400030721     C                   ENDIF
135500030721     C                   ENDIF
135600030721     C*** NCD
135700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
135800030721     C                   EVAL      PiStr=SkSplitCSV(i)
135900030721     C                   EXSR      CHKNUM
136000030721     C                   IF        PiInt=*on      AND
136100030721     C                             PiVal<=9999999
136200030721     C                   Z-ADD     PiVal         VABNCD
136300030721     C                   ELSE
136400030721     C                   SETON                                        32
136500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136600030721     C                             + ' ' + 'VABNCD'
136700030721     C                   ENDIF
136800030721     C                   ENDIF
136900030721     C*** NCA
137000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
137100030721     C                   EVAL      PiStr=SkSplitCSV(i)
137200030721     C                   EXSR      CHKNUM
137300030721     C                   IF        PiInt=*on      AND
137400030721     C                             PiVal<=9999999
137500030721     C                   Z-ADD     PiVal         VABNCA
137600030721     C                   ELSE
137700030721     C                   SETON                                        32
137800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137900030721     C                             + ' ' + 'VABNCA'
138000030721     C                   ENDIF
138100030721     C                   ENDIF
138200030721     C*** ZNC
138300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
138400030721     C                   EVAL      PiStr=SkSplitCSV(i)
138500030721     C                   EXSR      CHKNUM
138600030721     C                   IF        PiInt=*on AND
138700030721     C                             PiVal<=99
138800030721     C                   Z-ADD     PiVal         VABZNC
138900030721     C                   ELSE
139000030721     C                   SETON                                        32
139100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139200030721     C                             + ' ' + 'VABZNC'
139300030721     C                   ENDIF
139400030721     C                   ENDIF
139500030721     C*** DCR
139600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
139700030721     C                   EVAL      PiStr=SkSplitCSV(i)
139800030721     C                   EXSR      CHKNUM
139900030721     C                   IF        PiInt=*on       AND
140000030721     C                             PiVal<=99999999
140100030721     C                   Z-ADD     PiVal         VABDCR
140200030721     C                   ELSE
140300030721     C                   SETON                                        32
140400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140500030721     C                             + ' ' + 'VABDCR'
140600030721     C                   ENDIF
140700030721     C                   ENDIF
140800030721     C*** HCR
140900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
141000030721     C                   EVAL      PiStr=SkSplitCSV(i)
141100030721     C                   EXSR      CHKNUM
141200030721     C                   IF        PiInt=*on   AND
141300030721     C                             PiVal<=9999
141400030721     C                   Z-ADD     PiVal         VABHCR
141500030721     C                   ELSE
141600030721     C                   SETON                                        32
141700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141800030721     C                             + ' ' + 'VABHCR'
141900030721     C                   ENDIF
142000030721     C                   ENDIF
142100030721     C*** VMD
142200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
142300030721     C                   EVAL      PiStr=SkSplitCSV(i)
142400030721     C                   EXSR      CHKNUM
142500030721     C                   IF        PiNum=*on
142600030721     C                   Z-ADD     PiVal         VABVMD
142700030721     C                   ELSE
142800030721     C                   SETON                                        32
142900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
143000030721     C                             + ' ' + 'VABVMD'
143100030721     C                   ENDIF
143200030721     C                   ENDIF
143300030721     C*** ANT
143400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
143500030721     C                   EVAL      PiStr=SkSplitCSV(i)
143600030721     C                   EXSR      CHKNUM
143700030721     C                   IF        PiInt=*on        AND
143800030721     C                             PiVal<=999999999
143900030721     C                   Z-ADD     PiVal         VABANT
144000030721     C                   ELSE
144100030721     C                   SETON                                        32
144200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
144300030721     C                             + ' ' + 'VABANT'
144400030721     C                   ENDIF
144500030721     C                   ENDIF
144600030721     C*** CMO
144700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
144800030721     C                   EVAL      PiStr=SkSplitCSV(i)
144900030721     C                   EXSR      CHKNUM
145000030721     C                   IF        PiInt=*on
145100030721     C                   Z-ADD     PiVal         Num5_0
145200030721     C                   MOVEL(p)  Num5_0        VABCMO
145300030721     C                   ELSE
145400030721     C                   SETON                                        32
145500030721     C                   EVAL      VABCMO = *zeros
145600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
145700030721     C                             + ' ' + 'VABCMO'
145800030721     C                   ENDIF
145900030721     C                   ENDIF
146000040510     C*
146100040510     C* Reperisco i campi del FIVAT
146200040510     C*** VATNOT
146300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
146400040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
146500040510     C                   ENDIF
146600040510     C*** VATTRC
146700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
146800040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
146900040510     C                   ENDIF
147000060331     C*** VATNOT_A
147100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
147200060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
147300060331     C                   ENDIF
147400060331     C*** VATNOT_B
147500060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
147600060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
147700060331     C                   ENDIF
147800060331     C*** VATNOT_E
147900070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
148000060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
148100060331     C                   ENDIF
148200070823     C*
148300100831     C***  ===>  Gestione campi "particolari" dentro ciclo
148400100831     C*** RSD
148500100831     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD'
148600101018     C                   MOVEL     *blanks       wRSD             70
148700101018     C                   EVAL      wRSD = %trim(SkSplitCSV(i))
148800101018     C                   EVAL      VABRSD = %subst(wRSD:1:35)
148900101018     C                   EVAL      VABRD2 = %subst(wRSD:36:35)
149000100831     C                   ENDIF
149100101021     C*** NSP / RMN
149200101021     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNSP'
149300101021     C                   EVAL      PiStr=SkSplitCSV(i)
149400101021     C                   EXSR      CHKNUM
149500101021     C                   IF        PiInt=*on
149600101021     C                   Z-ADD     PiVal         VABNSP
149700101021     C                   Z-ADD     PiVal         VABRMN
149800101021     C                   ELSE
149900101021     C                   SETON                                        31
150000101021     C                   EVAL      VABNSP = *zeros
150100101021     C                   EVAL      VABRMN = *zeros
150200101021     C                   EVAL      vinmsg = %trimr(vinmsg)
150300101021     C                             + ' ' + 'VABNSP VABRMN'
150400101021     C                   ENDIF
150500101021     C                   ENDIF
150600070925     C*
150700071029     C***  <===  -----------------------------------------
150800040510     C*
150900030715     C                   ENDIF
151000030715     C                   EVAL      i = i + 1
151100030715     C                   ENDDO
151200070925     C*
151300071029     C***  ===>  Gestione campi "particolari" fuori ciclo
151400100831     C*
151500100831     C                   IF        wVABRSD <> *blanks
151600100831     C                   EVAL      VABRSD = wVABRSD
151700100831     C                   ENDIF
151800100831     C*
151900100831     C                   IF        wVABIND <> *blanks
152000100831     C                   EVAL      VABIND = wVABIND
152100100831     C                   ENDIF
152200100831     C*
152300100831     C                   IF        wVABCAD <> *blanks
152400100831     C                   EVAL      VABCAD = wVABCAD
152500100831     C                   ENDIF
152600100831     C*
152700100831     C                   IF        wVABLOD <> *blanks
152800100831     C                   EVAL      VABLOD = wVABLOD
152900100831     C                   ENDIF
153000100831     C*
153100100831     C                   IF        wVABPRD <> *blanks
153200100831     C                   EVAL      VABPRD = wVABPRD
153300100831     C                   ENDIF
153400070925     C*
153500071029     C***  <===  -----------------------------------------
153600070925     C*
153700070925     C* Considerazioni finali su CBO/CAS
153800100318     C                   IF        wFlgCAS = '1'    OR
153900100318     C                             VABCAS  > *zeros
154000070925     C                   IF        VABCBO = '1'
154100070925     C                   EVAL      VABCBO = '4'
154200070925     C                   ENDIF
154300070925     C                   IF        VABCBO = '2'
154400070925     C                   EVAL      VABCBO = '6'
154500070925     C                   ENDIF
154600070925     C                   ENDIF
154700070925     C***  <===  ----------------------------
154800040510     C*
154900040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
155000040510     C                   IF        VABNSP = *zeros
155100040510     C                   SETON                                        31
155200040510     C                   ENDIF
155300040715     C*
155400040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
155500040715     C                   IF        VABLNP = *zeros
155600040715     C                   SETON                                        31
155700040715     C                   ENDIF
155800040510     C*
155900040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
156000040510     C                   IF        VABRMN = *zeros
156100040510     C                   Z-ADD     VABNSP        VABRMN
156200040510     C                   ENDIF
156300040510     C*
156400040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
156500040510     C                   IF        VABRMA = *blanks
156600040714     C                   MOVEL     VABRMN        VABRMA
156700040510     C                   ENDIF
156800040128     C*
156900040510     C* Se provincia nn valorizzata la reperisco
157000040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
157100040510     C                   IF        VABPRD = *blanks
157200040128     C                   CLEAR                   TISI95DS
157300040128     C                   EVAL      I95TCN = '3'
157400040128     C                   Z-ADD     datcor        I95DAT
157500040128     C                   EVAL      I95NAR = VABNZD
157600040128     C                   EVAL      I95CAP = VABCAD
157700040128     C                   EVAL      I95LOC = VABLOD
157800040128     C                   CALL      'TISI95R'
157900040128     C                   PARM                    TISI95DS
158000040128     C                   EVAL      VABPRD = O95PRV
158100040510     C                   ENDIF
158200020204     C*
158300020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
158400020204     C                   EXSR      CHKIMPDIV
158500030715     C                   ENDIF
158600020204     C*
158700010607     C                   ENDSR
158800010607     C*----------------------------------------------------*
158900010601
159000020204
159100020204     C*----------------------------------------------------*
159200020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
159300020204     C*----------------------------------------------------*
159400020204     C     CHKIMPDIV     BEGSR
159500020204     C*
159600020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
159700020204     C                   Z-ADD     *zeros        wrkDec            9 9
159800020204     C*
159900020204     C* Come prima cosa effettuo considerazioni sulla divisa
160000020204     C                   IF        vabIAS > *zeros
160100020204     C                   IF        vabVAS <> 'EUR'
160200020204     C                   EVAL      vabVAS =  'ITL'
160300020204     C                   ENDIF
160400020204     C                   ENDIF
160500020204     C*
160600020204     C                   IF        vabCAS > *zeros
160700020204     C                   IF        vabVCA <> 'EUR'
160800020204     C                   EVAL      vabVCA =  'ITL'
160900020204     C                   ENDIF
161000020204     C                   ENDIF
161100020204     C*
161200020204     C                   IF        vabVMD > *zeros
161300020204     C                   IF        vabVAD <> 'EUR'
161400020204     C                   EVAL      vabVAD =  'ITL'
161500020204     C                   ENDIF
161600020204     C                   ENDIF
161700020204     C*
161800020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
161900020204     C                   Z-ADD     vabIAS        wrkDec
162000020204     C                   IF        wrkDec > *zeros
162100020204     C                   IF        vabVAS = 'ITL'
162200020204     C                   EVAL      vabIAS = *zeros
162300020204     C                   ENDIF
162400020204     C                   ENDIF
162500020204     C*
162600020204     C* Stabilisco se il contrasegno ha decimali valorizzati
162700020204     C                   Z-ADD     vabCAS        wrkDec
162800020204     C                   IF        wrkDec > *zeros
162900020204     C                   IF        vabVCA = 'ITL'
163000020204     C                   EVAL      vabCAS = *zeros
163100020204     C                   ENDIF
163200020204     C                   ENDIF
163300020204     C*
163400020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
163500020204     C                   Z-ADD     vabVMD        wrkDec
163600020204     C                   IF        wrkDec > *zeros
163700020204     C                   IF        vabVAD = 'ITL'
163800020204     C                   EVAL      vabVMD = *zeros
163900020204     C                   ENDIF
164000020204     C                   ENDIF
164100020204     C*
164200020204     C                   ENDSR
164300020204     C***
164400010330
164500100318
164600100318
164700100318     C*----------------------------------------------------*
164800100318     C*  CONTROLLO NUMERICITA' CAMPI
164900100318     C*----------------------------------------------------*
165000100318     C     CHKNUM        BEGSR
165100100318     C*
165200100318     C                   IF        PiDecChr = *blanks
165300100318     C                   EVAL      PiDecChr = CharNUM
165400100318     C                   ENDIF
165500100318     C*
165600100318     C                   callp     UBISNUM_Check(PiStr
165700100318     C                                          :PiDecChr
165800100318     C                                          :PiVal
165900100318     C                                          :PiNum
166000100318     C                                          :PiInt)
166100100318     C*
166200100318     C                   ENDSR
166300100318     C***
166400010601
166500010601
166600010601
166700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
166800010601     C     repfil        BEGSR
166900010601     C*
167000010601     C                   if        invfil = *zeros and
167100010601     C                             depfil > *zeros and
167200010629     C                             (vinflg = *blanks or
167300010629     C                              vinflg = *zeros)
167400010601     C
167500010601     C                   eval      invfil = depfil
167600010601     C                   endif
167700010601     C*
167800010601     C                   if        depfil <> invfil and
167900010601     C                             invfil > *zeros
168000010601     C                   eval      flgMulti = '1'
168100010601     C                   if        vinflg = *blanks
168200010601     C                   add       1             cntNonEl
168300010601     C                   endif
168400010601     C                   endif
168500010601     C*
168600010601     C                   if        vinflg = '2'
168700010601     C                   eval      flgStato = '2'
168800010601     C                   endif
168900010601     C*
169000010601     C                   ENDSR
169100010601     C***
169200010601
169300010601
169400010601
169500010330
169600040119      /TITLE Invio dei dati al punto operativo.
169700040119     C     invio         BEGSR
169800040119     C*
169900040119     C* 1° invio FIVAT
170000040119     C                   reset                   dscmz
170100040119     C                   move      vlrpoi        cmzdst
170200040119     C                   eval      cmzfld = 'FIVATWWR'
170300040119     C                   eval      cmzmbd = vlrhdl
170400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
170500040119     C***                if        prmfir = *blanks
170600040119     C                   eval      cmzfla = 'FIVAT00F'
170700040119     C                   eval      cmzmba = 'FIVAT00F'
170800040119     C***                else
170900040119     C***                eval      cmzfla = prmfir
171000040119     C***                eval      cmzmba = prmfir
171100040119     C***                endif
171200040119     C                   eval      cmznrr = *zeros
171300040119     C                   move      §ctrokvt      cmznrr
171400040119     C                   eval      cmzlba = vlrfl1
171500040119     C                   call(e)   'TIS711C'
171600040119     C                   parm                    dscmz
171700040119     C                   parm      *blanks       esito
171800040119     C                   if        %error
171900040119     C                             or cmzerr = '1'
172000040119     C                             or esito  = '1'
172100040119     C                   eval      wrkesito = '3'
172200040119     C                   else
172300040119     C*
172400040119     C* 2° invio FIVAB
172500040119     C                   reset                   dscmz
172600040119     C                   move      vlrpoi        cmzdst
172700040119     C                   eval      cmzfld = vlrfou
172800040119     C                   eval      cmzmbd = vlrhdl
172900040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
173000040119     C***                if        prmfir = *blanks
173100040119     C                   eval      cmzfla = 'FIVAB00F'
173200040119     C                   eval      cmzmba = 'FIVAB00F'
173300040119     C***                else
173400040119     C***                eval      cmzfla = prmfir
173500040119     C***                eval      cmzmba = prmfir
173600040119     C***                endif
173700040119     C                   eval      cmznrr = *zeros
173800040119     C                   move      §ctrokvb      cmznrr
173900040119     C                   eval      cmzlba = vlrfl1
174000040119     C                   call(e)   'TIS711C'
174100040119     C                   parm                    dscmz
174200040119     C                   parm      *blanks       esito
174300040119     C                   if        %error
174400040119     C                             or cmzerr = '1'
174500040119     C                             or esito  = '1'
174600040119     C                   eval      wrkesito = '3'
174700040119     C                   endif
174800040119     C                   endif
174900040119     C*
175000040119     C                   ENDSR
175100040119     C***
175200010601
175300010601
175400010601
175500010601
175600010601      /TITLE Invio dei dati al punto operativo.
175700010601     C     opeini        BEGSR
175800010601     C*
175900010601     C* Inizializzo flag e contatori operativi
176000010601     C                   movel     '0'           flgGiro           1
176100010601     C                   movel     '0'           flgMulti          1
176200010601     C                   movel     '1'           flgStato          1
176300010615     C                   movel     '0'           flgOk             1
176400010601     C                   z-add     *zeros        cntNonEl         10 0
176500010601     C                   z-add     *zeros        depfil            3 0
176600010601     C                   z-add     *zeros        invfil            3 0
176700010601     C*
176800010601     C                   ENDSR
176900010601     C***
177000070326
177100070326
177200070326
177300070326
177400070326     C     *pssr         BEGSR
177500070329     C*
177600070329     C                   if        %open(tivin00r)
177700070329     C                   close     tivin00r
177800070329     C                   endif
177900070329     C                   if        %open(fivabwwr)
178000070329     C                   close     fivabwwr
178100070329     C                   endif
178200070329     C                   if        %open(fivatwwr)
178300070329     C                   close     fivatwwr
178400070329     C                   endif
178500070326     C*
178600070326     C* Effettuo la chiamata al CLLE preposto
178700070326     C                   call(e)   'TITVVTC'
178800070326     C                   parm                    parccm
178900070326     C                   parm                    parmbr
179000070326     C                   parm      '2'           paropz
179100070326     C*
179200070326     C                   eval      wrkesito = '2'
179300070404     C*
179400070404     C                   seton                                        LR
179500070326     C*
179600070326     C                   ENDSR     '*CANCL'
179700070326     C***
179800100318
179900100318
180000100318
180100100318     C     normCSV       BEGSR
180200100318     C*
180300100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
180400100318     C* separatori campi
180500100318     C                   if        CharTXT <> *blanks
180600100318     C                   z-add     *zeros        wLenVINDTA        4 0
180700100318     C                   z-add     *zeros        wFlgVINDTA        1 0
180800100318     C                   setoff                                       80
180900100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
181000100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
181100100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
181200100318     C                   if        not *in80
181300100318     C                   seton                                        80
181400100318     C                   else
181500100318     C                   setoff                                       80
181600100318     C                   endif
181700100318     C                   endif
181800100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
181900100318     C                             and *in80 = *on
182000100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
182100100318     C                   endif
182200100318     C                   enddo
182300100318     C                   endif
182400100318     C*
182500100318     C                   ENDSR
182600100318     C***
182700070326
182800100326
182900100326
183000100326     C*----------------------------------------------------*
183100100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
183200100326     C*----------------------------------------------------*
183300100326     C     REPNSP        BEGSR
183400100326     C*
183500100326     C* NSP => Stacco un numeratore da AZNUM
183600100326     C                   clear                   TRUL33DS
183700100326     C                   eval      I33OPE = *zeros
183800100326     C                   eval      I33CNU = 302
183900100326     C                   eval      I33NUM = 1
184000100326     C                   movel     TRUL33DS      KPJBU
184100100326     C                   call      'TRUL33R'
184200100326     C                   parm                    KPJBA
184300100326     C                   movel     KPJBU         TRUL33DS
184400100326     C                   if        O33ERR = *zeros
184500100326     C                   z-add     O33NRF        VABNSP
184600100326     C                   z-add     O33NRF        VATNSP
184700100326     C                   else
184800100326     C                   SETON                                        31
184900100326     C                   EVAL      vinmsg = %trimr(vinmsg)
185000100326     C                             + ' ' + 'VABNSP VATNSP'
185100100326     C                   endif
185200100326     C*
185300100326     C                   ENDSR
185400010330
185500100326
185600010330
185700000613     C     *inzsr        BEGSR
185800990910     C*
185900990910     C     *entry        plist
186000990920     C                   parm                    tivlrds
186100990921     C                   parm      wrkesito      esito
186200000724     C                   parm                    prmlit
186300000710     C                   parm                    prmfir
186400010330     C*
186500010330     C* CALCOLA LA DATA CORRENTE
186600100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
186700100318     C                   eval      datcor = %dec(%date() : *ISO)
186800000613     C*
186900000613     C                   ENDSR
187000000613     C***
187100990908
187200101018     Otitv2a8p  E            riepilogo         2
187300990915     O                                              'Upload via Internet'
187400990915     O                                           +1 'Traduzione TIVIN00R -'
187500021025     O                                           +1 'FIVABWWR'
187600990915     O                                           +1 'Testate bolle'
187700990915     O          E            riepilogo   2
187800990915     O                       wrkdata
187900990915     O                       wrkora              +1
188000990915     O                       procname            +1
188100990915     O          E            riepilogo   2
188200990915     O                                              'Cliente..................:'
188300010605     O                       VABCCM        z     +1
188400990915     O          E            riepilogo   2
188500990920     O                                              'Riferimento Strategi.....:'
188600990920     O                       vlrhdl              +1
188700990915     O          E            riepilogo   2
188800990915     O                                              'Giusti...................:'
188900040119     O                       §CTROKVB      2   +  1
189000990915     O          E            riepilogo   2
189100990915     O                                              'Sbagliati e corretti.....:'
189200971022     O                       §CTRMO        2   +  1
189300990915     O          E            riepilogo   2
189400990915     O                                              'Sbagliati e scartati.....:'
189500971022     O                       §CTRNO        2   +  1
189600000613
189700101018     Otitv2a8ps E            testdett          2
189800000613     O                                              'Upload via Internet'
189900000613     O                                           +1 'Traduzione TIVIN00R -'
190000021025     O                                           +1 'FIVABWWR'
190100000613     O                                           +1 'Testate bolle'
190200000616     O          E            testdett    3
190300000613     O                                           +2 'N° rec'
190400000613     O                                           +3 'Anteprima contenuto'
190500000616     O          E            rigadett    2
190600000613     O                       rrnum               +2
190700000621     O                       recko               +3
190800000616     O          E            findett     2
190900000613     O                       wrkdata
191000000613     O                       wrkora              +1
191100000613     O                       procname            +1
191200000616     O          E            findett     2
191300000613     O                                              'Cliente..................:'
191400010605     O                       VABCCM        z     +1
191500000616     O          E            findett     2
191600000613     O                                              'Riferimento Strategi.....:'
191700000613     O                       vlrhdl              +1
191800000616     O          E            findett     2
191900000613     O                                              'Giusti...................:'
192000040119     O                       §CTROKVB      2   +  1
192100000616     O          E            findett     2
192200000613     O                                              'Sbagliati e corretti.....:'
192300000613     O                       §CTRMO        2   +  1
192400000616     O          E            findett     2
192500000613     O                                              'Sbagliati e scartati.....:'
192600000613     O                       §CTRNO        2   +  1
192700000613** CMD - COMANDI CL
192800101018OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV2A8P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
192900101018OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV2A8PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
193000101018DLTOVR FILE(TITV2A8P TITV2A8PS) LVL(*)
193100010606
193200000613
193300040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
193400101021W_VABRSD;VABIND;VABLOD;VABPRD;VABCAD;W_VABNSP;VABNT2;FIL001;VATNOT_B;
193500101021FIL002;FIL003;FIL004;VABRMA;VABNCL;VABPKB;VABNAS;VABNOT;FIL005;
193600101021VABCAS;FIL006;FIL007;
193700101018
193800100318
