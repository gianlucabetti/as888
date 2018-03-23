000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200100318     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
000700040119     FFIVATwwr  O    E             DISK    usropn
000800110623     Ftitv2g3p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070329     F                                     infsr(*pssr)
001100110623     Ftitv2g3ps O    f  198        PRINTER usropn
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
005700110623     D*------------------
005800100326
005900030715     D*------------------
006000030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
006100030715     D*------------------
006200030715     D SkSplitFLD      S             10    DIM(100)
006300030715     D SkSplitCSV      S            256    DIM(100)
006400030715     D CharCSV         S              1
006500030715     D CharTXT         S              1
006600030715     D CharNUM         S              1
006700100318     D CharSOS         S              1
006800030715     D posDa           S              3  0 INZ(*zeros)
006900030715     D posA            S              3  0 INZ(*zeros)
007000030715     D i               s              3  0 INZ(1)
007100030715     D wGiro           s              1  0 INZ(*zeros)
007200070823     D wVATNOT_A       s                   LIKE(VATNOT)
007300060331     D wVATNOT_B       s                   LIKE(VATNOT)
007400060331     D wVATNOT_E       s                   LIKE(VATNOT)
007500100831     D wVABRSD         s                   LIKE(VABRSD)
007600100831     D wVABIND         s                   LIKE(VABIND)
007700100831     D wVABCAD         s                   LIKE(VABCAD)
007800100831     D wVABLOD         s                   LIKE(VABLOD)
007900100831     D wVABPRD         s                   LIKE(VABPRD)
008000030822     D*------------------
008100030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
008200030822     D*------------------
008300030822     D posDaDft        S              3  0 INZ(*zeros)
008400030822     D posADft         S              3  0 INZ(*zeros)
008500030822     D j               s              3  0 INZ(1)
008600030822     D wGiroDft        s              1  0 INZ(*zeros)
008700080418     D*------------------
008800080418     D* Costanti
008900080418     D*------------------
009000080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009100080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009200100318
009300100318     D*------------------
009400100318     D* LINKING A DEFINIZIONI ESTERNE
009500100318     D*------------------
009600100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009700100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009800100318
009900100909     D*------------------
010000100909     D* DS CALCOLO CHECK-DIGITS
010100100909     D*------------------
010200100909     D TRUL28DSX     e ds                  inz
010300010330
010400010330
010500010330
010600990915     C                   time                    wrkdata
010700990915     C                   time                    wrkora
010800000913     C                   reset                   rrnum
010900990921     C                   reset                   esito
011000990921     C                   reset                   wrkesito
011100010601     C*
011200010601     C                   exsr      opeini
011300010605     C                   exsr      rwvab
011400010601     C*
011500010601     C                   seton                                        lr
011600010601
011700010601
011800010601
011900010601     C*--------------------------------------------------------
012000010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
012100010601     C*--------------------------------------------------------
012200010601     C     PREELA        BEGSR
012300010601     C*
012400010601     C* SE OCCORRE SPEDIRE IN FILIALE
012500010601     C                   if        invfil <> *zeros and
012600010601     C                             flgGiro = '0'
012700010601     C*
012800010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012900010601     C                   eval      flgGiro = '1'
013000000724     C*
013100000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
013200000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
013300010601     C     invfil        chain     azorg01l
013400000724     C                   if        %found
013500000724     C                   movel(p)  CMD(1)        depcmd
013600020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
013700000724     C*
013800000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
013900011113     C                   Z-ADD     150           LENGH            15 5
014000000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014100000724     C                   PARM                    depcmd
014200000724     C                   PARM                    LENGH
014300000724     C*
014400000724     C                   endif
014500000616     C*
014600000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
014700000616     C                   movel(p)  CMD(2)        depcmd
014800000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
014900000616     C*
015000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
015100011113     C                   Z-ADD     150           LENGH            15 5
015200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
015300000616     C                   PARM                    depcmd
015400000616     C                   PARM                    LENGH
015500000613     C*
015600110623     C                   if        not %open(titv2g3ps)
015700110623     C                   open      titv2g3ps
015800070329     C                   except    testdett
015900000613     C                   endif
016000010601     C*
016100010601     C                   endif
016200010601     C*
016300010601     C                   ENDSR
016400010601     C***
016500010601
016600010601
016700010601
016800010601
016900010601     C*--------------------------------------------------------
017000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
017100010601     C*--------------------------------------------------------
017200010601     C     ENDELA        BEGSR
017300990915     C*
017400000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
017500040119     C*
017600040119     C* Effettuo la chiamata al CLLE preposto
017700040119     C                   call(e)   'TITVVTC'
017800040510     C                   parm                    parccm
017900040119     C                   parm                    parmbr
018000040119     C                   parm      '2'           paropz
018100050201     C*
018200050201     C* Effettuo lancio TISI95 solo x chiusura
018300050201     C                   CLEAR                   TISI95DS
018400050201     C                   EVAL      I95TLA = 'C'
018500050201     C                   CALL      'TISI95R'
018600050201     C                   PARM                    TISI95DS
018700000613     C*
018800110623     C                   if        %open(titv2g3ps)
018900000616     C                   except    findett
019000110623     C                   close     titv2g3ps
019100000613     C                   endif
019200000616     C*
019300000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
019400011113     C                   Z-ADD     150           LENGH            15 5
019500010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
019600000616     C                   PARM                    CMD(3)
019700000616     C                   PARM                    LENGH
019800000616     C*
019900010601     C                   ENDSR
020000010601     C***
020100010601
020200010601
020300010601
020400000613
020500000613     C*--------------------------------------------------------
020600000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
020700000613     C*--------------------------------------------------------
020800000621     C     STPR          BEGSR
020900000613     C*
021000110623     C                   if        not %open(titv2g3p)
021100110623     C   50              open      titv2g3p
021200990915     C                   endif
021300990915     C*
021400030715     C   50              except    riepilogo
021500990915     C*
021600110623     C                   if        %open(titv2g3p)
021700110623     C   50              close     titv2g3p
021800990914     C                   endif
021900000613     C*
022000000613     C                   ENDSR
022100000613     C***
022200000613
022300010601
022400010601
022500010330     C*--------------------------------------------------------
022600021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
022700010330     C*--------------------------------------------------------
022800010605     C     RWVAB         BEGSR
022900010330     C*
023000010330     C                   if        not %open(tivin00r)
023100010330     C                   open      tivin00r
023200010330     C                   endif
023300021025     C                   if        not %open(fivabwwr)
023400021025     C                   open      fivabwwr
023500010330     C                   endif
023600040119     C*
023700040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
023800040119     C                   exsr      prevat
023900040119     C*
024000040119     C                   if        chkcall = '0'
024100040119     C*
024200040119     C                   if        not %open(fivatwwr)
024300040119     C                   open      fivatwwr
024400040119     C                   endif
024500010330     C*
024600040119     C                   clear                   §CTROKVB          7 0
024700040119     C                   clear                   §CTROKVT          7 0
024800040119     C                   clear                   §CTRMO            7 0
024900040119     C                   clear                   §CTRNO            7 0
025000070823     C*
025100070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
025200070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
025300110623     C***                exsr      inzvar
025400110623     C***                exsr      impvab
025500110131     C*
025600110131     C* Effettuo una prima lettura fuori ciclo x ignorare la 1° riga
025700110315 xxx C***                read      tivin00r                               70
025800010330     C*
025900030822     C                   do        *HIVAL
026000010330     C*
026100030822     C                   read      tivin00r                               70
026200010618     C*
026300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
026400010618     C                   if        vinflg = '1'
026500010618     C                   eval      flgOk = '1'
026600010618     C                   endif
026700040510     C*
026800040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
026900040510     C                   z-add     *zeros        wLenVINDTA        4 0
027000040510     C                   z-add     *zeros        wFlgVINDTA        1 0
027100040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
027200040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
027300040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
027400040510     C                   z-add     1             wFlgVINDTA
027500040510     C                   leave
027600040510     C                   endif
027700040510     C                   enddo
027800010618     C*
027900040510     C                   if        vindta > *blanks AND
028000040510     C                             wFlgVINDTA = 1
028100010330     C                   add       1             rrnum
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
029300070925     C* Aggiungo un byte a blanks all'inizio d ogni record
029400070925     C                   eval      vindta  = ' ' + %trim(vindta)
029500070925     C*
029600080527     C                   exsr      inzvar
029700100318     C                   exsr      normCSV
029800010605     C                   exsr      impvab
029900010601     C*
030000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
030100010605     C                   eval      depfil = VABLNP
030200010601     C                   exsr      repfil
030300010601     C                   if        depfil = invfil
030400021025     C                   if        vlrpoi = 999
030500030822     C                   move(P)   invfil        VABFGS
030600021025     C                   else
030700030822     C                   move(P)   vlrpoi        VABFGS
030800021025     C                   endif
030900010601     C*
031000010601     C                   exsr      PREELA
031100010601     C*
031200010604     C* Ebbene...
031300010604     C*
031400040119     C  N31              add       1             §CTROKVB
031500040119     C   32              add       1             §CTRMO
031600040119     C   31              add       1             §CTRNO
031700080418     C*
031800030822     C                   if        wGiro = 2
031900080513     C* Scrivo sempre il VAT
032000080513     C  N31              exsr      wrivat
032100110201     C                   exsr      chkvab
032200080513     C                   endif
032300010604     C*
032400010604     C                   if        *in31 = *off and
032500010604     C                             *in32 = *off
032600010604     C                   eval      vinflg = '1'
032700010604     C                   else
032800010604     C                   eval      recko = vindta
032900010604     C                   except    rigadett
033000010604     C                   eval      vinflg = '2'
033100010604     C                   endif
033200010604     C                   endif
033300010601     C*
033400010604     C                   endif
033500010604     C*
033600010330     C                   else
033700010330     C                   eval      vinflg = '1'
033800010330     C                   endif
033900010601     C*
034000010601     C  N70              update    tivin000
034100010330     C*
034200030822     C  N70              enddo
034300040119     C*
034400080527     C* Verifico dati rimasti "in cannna"...
034500080527     C                   seton                                        65
034600110201     C                   exsr      chkvab
034700080527     C*
034800040119     C                   endif                                                  (endif - chkcall)
034900010601     C*
035000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
035100010601     C                   if        cntNonEl = *zeros or
035200010601     C                             flgMulti = '0'
035300010330     C* Se non ci sono record con errori ...
035400010601     C                   if        §ctrno = 0 and
035500010604     C                             §ctrmo = 0 and
035600010601     C                             flgStato <> '2'
035700010330     C* ... restituisco esito OK.
035800010330     C                   eval      wrkesito = '0'
035900010330     C                   else
036000040119     C                   if        §ctrokvb > 0
036100010330     C                   eval      wrkesito = '1'
036200010330     C                   else
036300010615     C                   if        flgOk = '0'
036400010615     C                   eval      wrkesito = '2'
036500010615     C                   else
036600010615     C                   eval      wrkesito = '6'
036700010615     C                   endif
036800010330     C                   endif
036900010330     C                   endif
037000010601     C                   else
037100010601     C                   eval      wrkesito = '9'
037200010601     C                   endif
037300010330     C*
037400010330     C                   if        %open(tivin00r)
037500010330     C                   close     tivin00r
037600010330     C                   endif
037700021025     C                   if        %open(fivabwwr)
037800021025     C                   close     fivabwwr
037900010330     C                   endif
038000040512     C                   if        %open(fivatwwr)
038100040512     C                   close     fivatwwr
038200040512     C                   endif
038300010601     C*
038400010601     C                   if        vlrpoi <> 999
038500010601     C                   eval      invfil = vlrpoi
038600010601     C                   endif
038700010330     C*
038800040119     C                   if        §ctrokvb > 0
038900010601     C                             and invfil > *zeros
039000010330     C                   exsr      invio
039100010330     C                   endif
039200010601     C*
039300010618     C                   if        flgGiro = '1'
039400010601     C                   exsr      endela
039500010618     C                   endif
039600010330     C*
039700010330     C                   ENDSR
039800010330     C***
039900080527
040000080527
040100080527
040200080527     C*----------------------------------------------------*
040300080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
040400080527     C*----------------------------------------------------*
040500100413     C     CHKVAB        BEGSR
040600110201     C*
040700110201     C* Se richiesti ignorare colli / peso / volume
040800110315 xxx C***                if        *in35
040900110315 xxx C***                z-add     *zeros        VABNCL
041000110315 xxx C***                z-add     *zeros        VABPKB
041100110315 xxx C***                z-add     *zeros        VABVLB
041200110315 xxx C***                endif
041300080527     C*
041400080527     C* Scrivo il VAB solo a rottura d spedizione
041500080527     C                   if        depspe = *zeros
041600080527     C  N31              eval      fivabds_sav = fivabds
041700080527     C                   eval      depspe = vabNSP
041800080527     C                   else
041900080527     C                   if        depspe = vabNSP AND *in65 = *off
042000100413     C   51              add       VABNCL        sav_VABNCL
042100100413     C   51              add       VABPKB        sav_VABPKB
042200100413     C   51              add       VABVLB        sav_VABVLB
042300100720     C   52              z-add     VABNCL        sav_VABNCL
042400100720     C   52              z-add     VABPKB        sav_VABPKB
042500100720     C   52              z-add     VABVLB        sav_VABVLB
042600100720     C   53              add       VABNCL        sav_VABNCL
042700100720     C   53              z-add     VABPKB        sav_VABPKB
042800100720     C   53              z-add     VABVLB        sav_VABVLB
042900080527     C                   else
043000080527     C  N31              eval      fivabds_ok = fivabds_sav
043100080527     C  N31              exsr      wrivab
043200080527     C  N31              eval      fivabds_sav = fivabds
043300080527     C                   eval      depspe = vabNSP
043400080527     C                   endif
043500080527     C                   endif
043600080527     C*
043700080527     C                   ENDSR
043800040119
043900040119
044000040119
044100040119     C*----------------------------------------------------*
044200040119     C*  SCARICAMENTO BUFFER RECORDS VAB
044300040119     C*----------------------------------------------------*
044400040510     C     WRIVAB        BEGSR
044500080513     C*
044600080513     C* Gestisco l'eventuale rottura x numero spedizione
044700080527     C                   if        VABNSP <> depspe or *in65 = *on
044800070301     C                   if        wVATNOT_E <> *blanks
044900070301     C                   eval      VABCTM = '7Q'
045000070301     C                   endif
045100100413     C                   write     fivab000
045200080513     C                   endif
045300040119     C*
045400040119     C                   ENDSR
045500040119
045600040119
045700040119
045800040119     C*----------------------------------------------------*
045900040119     C*  SCARICAMENTO BUFFER RECORDS VAT
046000040119     C*----------------------------------------------------*
046100040510     C     WRIVAT        BEGSR
046200060512     C*
046300060512     C* Inizializzo i campi chiave
046400060512     C                   eval      VATFGS = VABFGS
046500060512     C                   eval      VATAAS = VABAAS
046600060512     C                   eval      VATCCM = VABCCM
046700070823     C                   eval      VATNRS = VABNRS
046800060512     C                   eval      VATNSP = VABNSP
046900060512     C                   eval      VATLNP = VABLNP
047000040119     C*
047100040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
047200040510     C                   if        VATTRC = *blanks
047300040510     C                   eval      VATTRC = 'E'
047400040510     C                   endif
047500110318     C                   if        VATNOT <> *blanks
047600040510     C                   write     FIVAT000
047700040119     C                   add       1             §CTROKVT
047800110318     C                   endif
047900060331     C*
048000060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
048100060331     C                   if        wVATNOT_A <> *blanks
048200060331     C                   eval      VATNOT = wVATNOT_A
048300060331     C                   eval      VATTRC = 'A'
048400060331     C                   write     FIVAT000
048500060331     C                   add       1             §CTROKVT
048600060331     C                   endif
048700060331     C*
048800060331     C                   if        wVATNOT_B <> *blanks
048900060331     C                   eval      VATNOT = wVATNOT_B
049000060331     C                   eval      VATTRC = 'B'
049100060331     C                   write     FIVAT000
049200060331     C                   add       1             §CTROKVT
049300060331     C                   endif
049400060331     C*
049500060331     C                   if        wVATNOT_E <> *blanks
049600110623     C***                eval      VATNOT = wVATNOT_E
049700110623     C***                eval      VATTRC = 'E'
049800110623     C***                write     FIVAT000
049900110623     C***                add       1             §CTROKVT
050000110623     C*
050100110623     C* Sviluppati i "CHI SONO" in relazione al numero colli
050200110623     C*
050300110623     C* Inizializzo progressivo collo su totale colli della spedizione
050400110623     C                   Z-ADD     1             wNumColloDA       5 0
050500110623     C                   MOVEL     *blanks       wNumColloCor      5
050600110623     C                   MOVEL     *blanks       wNumWhoAre        7
050700110623     C*
050800110623     C                   DOW       wNumColloDA <= VABNCL
050900110623     C                   MOVEL(P)  wNumColloDA   wNumColloCor
051000110623     C                   MOVEL(P)  VABNSP        wNumWhoAre
051100110623     C                   EVAL      VATNOT = wNumWhoAre + wNumColloCor
051200110623     C*
051300110623     C* Calcolo in check-digits EAN13
051400110623     C                   clear                   TRUL28DSX
051500110623     C                   eval      I284STD = 'E13'
051600110623     C                   eval      I284COD = VATNOT
051700110623     C                   call      'TRUL28R4'
051800110623     C                   parm                    TRUL28DSX
051900110623     C*
052000110623     C* Se ok => valorizzo "CHI SONO", altrimenti nn effettuo nemmeno la scrittura
052100110623     C                   if        O284err = *blanks
052200110623     C                   eval      VATNOT = %trim(O284COD)
052300110623     C                   eval      VATTRC = 'E'
052400110623     C                   write     FIVAT000
052500110623     C                   add       1             §CTROKVT
052600110623     C                   endif
052700110623     C*
052800110623     C                   EVAL      wNumColloDA = wNumColloDA + 1
052900110623     C                   ENDDO
053000040119     C*
053100110623     C                   endif
053200110623     C*
053300040119     C                   ENDSR
053400040119
053500040119
053600040119
053700040119     C*----------------------------------------------------*
053800040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
053900040119     C*----------------------------------------------------*
054000040119     C     PREVAT        BEGSR
054100040119     C*
054200040119     C* Compongo il nome del membro da dare al FIVATWWR
054300040119     C                   eval      parmbr = vlrhdl
054400040119     C                   movel     'M'           parmbr
054500040510     C                   eval      parccm = vlrksc
054600040119     C                   eval      paropz = '1'
054700040119     C* Effettuo la chiamata al CLLE preposto
054800040119     C                   call(e)   'TITVVTC'
054900040119     C                   parm                    parccm
055000040119     C                   parm                    parmbr
055100040119     C                   parm                    paropz
055200040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
055300040119     C                   if        %error
055400040119     C                   movel     '1'           chkcall
055500040119     C                   else
055600040119     C                   movel     '0'           chkcall
055700040119     C                   endif
055800040119     C*
055900040119     C                   ENDSR
056000010601
056100010601
056200010601
056300010330     C*----------------------------------------------------*
056400030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
056500010330     C*----------------------------------------------------*
056600040119     C     INZVAR        BEGSR
056700080527     C*
056800080527     C                   setoff                                       65
056900010330     C*
057000030822     C* Inizializzo variabili di wrk
057100020204     C                   Z-ADD     *zeros        Num5_0            5 0
057200101018     C                   MOVEL     *blanks       wFlgCAS
057300060331     C                   MOVEL     *blanks       wVATNOT_A
057400060331     C                   MOVEL     *blanks       wVATNOT_B
057500060331     C                   MOVEL     *blanks       wVATNOT_E
057600100831     C                   MOVEL     *blanks       wVABRSD
057700100831     C                   MOVEL     *blanks       wVABIND
057800100831     C                   MOVEL     *blanks       wVABCAD
057900100831     C                   MOVEL     *blanks       wVABLOD
058000100831     C                   MOVEL     *blanks       wVABPRD
058100030822     C*
058200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
058300030822     C                   CLEAR                   SkSplitCSV
058400030822     C                   EVAL      i = 1
058500030822     C                   EVAL      posDa = *zeros
058600030822     C                   EVAL      posA  = *zeros
058700080527     C*
058800080527     C                   CLEAR                   FIVABDS
058900080527     C                   CLEAR                   FIVAT000
059000030822     C*
059100030822     C* Reimposto i valori di default
059200030822     C                   EXSR      DEFCAM
059300030822     C*
059400010330     C                   ENDSR
059500010330     C*----------------------------------------------------*
059600030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
059700010330     C*----------------------------------------------------*
059800010330     C     DEFCAM        BEGSR
059900030822     C*
060000030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
060100030715     C* e delimitatore testo.
060200030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
060300030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
060400030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
060500100318     C*
060600100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
060700100318     C                   EVAL      CharSOS = CharNUM
060800030822     C*
060900030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
061000100720     C                   SETOFF                                       505152
061100100720     C                   SETOFF                                       53
061200100413     C                   SELECT
061300100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
061400100720     C                   SETON                                        50
061500100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
061600100413     C                   SETON                                        51
061700100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
061800100720     C                   SETON                                        52
061900100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
062000100720     C                   SETON                                        53
062100100413     C                   ENDSL
062200030822     C*
062300030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
062400030822     C                   EVAL      posDaDft = 1
062500030822     C                   EVAL      posADft  = 0
062600030822     C                   EVAL      wGiroDft = 0
062700030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
062800030822     C                             posDaDft > 0
062900030822     C*
063000030822     C* Gestisco il 1° giro
063100030822     C                   IF        wGiroDft = 0
063200030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
063300030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
063400030822     C* Incremento il contatore dei "giri"
063500030822     C                   EVAL      wGiroDft = 1
063600030822     C                   ELSE
063700030822     C                   EVAL      posDaDft = posADft
063800030822     C                   ENDIF
063900030822     C* Eseguo lo scan x trovare la fine del campo corrente
064000030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
064100030822     C*
064200030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
064300030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
064400030822     C                   IF        posDaDft > 0 AND
064500030822     C                             posADft  > 0
064600030822     C* NCL
064700030822     C                   IF        %subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):1:3)
065000030822     C                             = 'NCL'
065100030822     C                   EVAL      PiStr=%trim(%subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065400030822     C                   EXSR      CHKNUM
065500030822     C                   IF        PiInt=*on
065600030822     C                   Z-ADD     PiVal         VABNCL
065700030822     C                   ENDIF
065800030822     C                   ENDIF
065900030822     C* CCM
066000030822     C                   IF        %subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):1:3)
066300030822     C                             = 'CCM'
066400030822     C                   EVAL      PiStr=%trim(%subst(
066500030822     C                             %subst(vlrppt:posDaDft+1:
066600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066700030822     C                   EXSR      CHKNUM
066800030822     C                   IF        PiInt=*on
066900030822     C                   Z-ADD     PiVal         VABCCM
067000030822     C                   ENDIF
067100030822     C                   ENDIF
067200030822     C* LNP
067300030822     C                   IF        %subst(
067400030822     C                             %subst(vlrppt:posDaDft+1:
067500030822     C                             posADft-posDaDft-1):1:3)
067600030822     C                             = 'LNP'
067700030822     C                   EVAL      PiStr=%trim(%subst(
067800030822     C                             %subst(vlrppt:posDaDft+1:
067900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068000030822     C                   EXSR      CHKNUM
068100030822     C                   IF        PiInt=*on
068200030822     C                   Z-ADD     PiVal         VABLNP
068300030822     C                   ENDIF
068400030822     C                   ENDIF
068500030822     C* NRS
068600030822     C                   IF        %subst(
068700030822     C                             %subst(vlrppt:posDaDft+1:
068800030822     C                             posADft-posDaDft-1):1:3)
068900030822     C                             = 'NRS'
069000030822     C                   EVAL      PiStr=%trim(%subst(
069100030822     C                             %subst(vlrppt:posDaDft+1:
069200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069300030822     C                   EXSR      CHKNUM
069400030822     C                   IF        PiInt=*on
069500030822     C                   Z-ADD     PiVal         VABNRS
069600030822     C                   ENDIF
069700030822     C                   ENDIF
069800030822     C* CTR
069900030822     C                   IF        %subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):1:3)
070200030822     C                             = 'CTR'
070300030822     C                   EVAL      PiStr=%trim(%subst(
070400030822     C                             %subst(vlrppt:posDaDft+1:
070500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070600030822     C                   EXSR      CHKNUM
070700030822     C                   IF        PiInt=*on
070800030822     C                   Z-ADD     PiVal         VABCTR
070900030822     C                   ENDIF
071000030822     C                   ENDIF
071100030822     C* PKB
071200030822     C                   IF        %subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):1:3)
071500030822     C                             = 'PKB'
071600030822     C                   EVAL      PiStr=%trim(%subst(
071700030822     C                             %subst(vlrppt:posDaDft+1:
071800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071900030822     C                   EXSR      CHKNUM
072000030822     C                   IF        PiNum=*on
072100030822     C                   Z-ADD     PiVal         VABPKB
072200030822     C                   ENDIF
072300030822     C                   ENDIF
072400030822     C* VLB
072500030822     C                   IF        %subst(
072600030822     C                             %subst(vlrppt:posDaDft+1:
072700030822     C                             posADft-posDaDft-1):1:3)
072800030822     C                             = 'VLB'
072900030822     C                   EVAL      PiStr=%trim(%subst(
073000030822     C                             %subst(vlrppt:posDaDft+1:
073100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073200030822     C                   EXSR      CHKNUM
073300030822     C                   IF        PiNum=*on
073400030822     C                   Z-ADD     PiVal         VABVLB
073500030822     C                   ENDIF
073600030822     C                   ENDIF
073700030822     C* QFT
073800030822     C                   IF        %subst(
073900030822     C                             %subst(vlrppt:posDaDft+1:
074000030822     C                             posADft-posDaDft-1):1:3)
074100030822     C                             = 'QFT'
074200030822     C                   EVAL      PiStr=%trim(%subst(
074300030822     C                             %subst(vlrppt:posDaDft+1:
074400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074500030822     C                   EXSR      CHKNUM
074600030822     C                   IF        PiNum=*on
074700030822     C                   Z-ADD     PiVal         VABQFT
074800030822     C                   ENDIF
074900030822     C                   ENDIF
075000030822     C* CBO
075100030822     C                   IF        %subst(
075200030822     C                             %subst(vlrppt:posDaDft+1:
075300030822     C                             posADft-posDaDft-1):1:3)
075400030822     C                             = 'CBO'
075500030822     C                   EVAL      VABCBO=%trim(%subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075800030822     C                   ENDIF
075900030822     C* TSP
076000030822     C                   IF        %subst(
076100030822     C                             %subst(vlrppt:posDaDft+1:
076200030822     C                             posADft-posDaDft-1):1:3)
076300030822     C                             = 'TSP'
076400030822     C                   EVAL      VABTSP=%trim(%subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076700030822     C                   ENDIF
076800030822     C* VAS
076900030822     C                   IF        %subst(
077000030822     C                             %subst(vlrppt:posDaDft+1:
077100030822     C                             posADft-posDaDft-1):1:3)
077200030822     C                             = 'VAS'
077300030822     C                   EVAL      VABVAS=%trim(%subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077600030822     C                   ENDIF
077700030822     C* VCA
077800030822     C                   IF        %subst(
077900030822     C                             %subst(vlrppt:posDaDft+1:
078000030822     C                             posADft-posDaDft-1):1:3)
078100030822     C                             = 'VCA'
078200030822     C                   EVAL      VABVCA=%trim(%subst(
078300030822     C                             %subst(vlrppt:posDaDft+1:
078400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078500030822     C                   ENDIF
078600070925     C* TIC
078700070925     C                   IF        %subst(
078800070925     C                             %subst(vlrppt:posDaDft+1:
078900070925     C                             posADft-posDaDft-1):1:3)
079000070925     C                             = 'TIC'
079100070925     C                   EVAL      VABTIC=%trim(%subst(
079200070925     C                             %subst(vlrppt:posDaDft+1:
079300070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079400070925     C                   ENDIF
079500030822     C* GCA
079600030822     C                   IF        %subst(
079700030822     C                             %subst(vlrppt:posDaDft+1:
079800030822     C                             posADft-posDaDft-1):1:3)
079900030822     C                             = 'GCA'
080000030822     C                   EVAL      VABGCA=%trim(%subst(
080100030822     C                             %subst(vlrppt:posDaDft+1:
080200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080300030822     C                   ENDIF
080400030822     C* CTM
080500030822     C                   IF        %subst(
080600030822     C                             %subst(vlrppt:posDaDft+1:
080700030822     C                             posADft-posDaDft-1):1:3)
080800030822     C                             = 'CTM'
080900030822     C                   EVAL      VABCTM=%trim(%subst(
081000030822     C                             %subst(vlrppt:posDaDft+1:
081100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081200030822     C                   ENDIF
081300030822     C* FFD
081400030822     C                   IF        %subst(
081500030822     C                             %subst(vlrppt:posDaDft+1:
081600030822     C                             posADft-posDaDft-1):1:3)
081700030822     C                             = 'FFD'
081800030822     C                   EVAL      VABFFD=%trim(%subst(
081900030822     C                             %subst(vlrppt:posDaDft+1:
082000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082100030822     C                   ENDIF
082200030822     C* VAD
082300030822     C                   IF        %subst(
082400030822     C                             %subst(vlrppt:posDaDft+1:
082500030822     C                             posADft-posDaDft-1):1:3)
082600030822     C                             = 'VAD'
082700030822     C                   EVAL      VABVAD=%trim(%subst(
082800030822     C                             %subst(vlrppt:posDaDft+1:
082900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083000030822     C                   ENDIF
083100030822     C* GMA
083200030822     C                   IF        %subst(
083300030822     C                             %subst(vlrppt:posDaDft+1:
083400030822     C                             posADft-posDaDft-1):1:3)
083500030822     C                             = 'GMA'
083600030822     C                   EVAL      VABGMA=%trim(%subst(
083700030822     C                             %subst(vlrppt:posDaDft+1:
083800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083900030822     C                   ENDIF
084000030822     C* GGA
084100030822     C                   IF        %subst(
084200030822     C                             %subst(vlrppt:posDaDft+1:
084300030822     C                             posADft-posDaDft-1):1:3)
084400030822     C                             = 'GGA'
084500030822     C                   EVAL      VABGGA=%trim(%subst(
084600030822     C                             %subst(vlrppt:posDaDft+1:
084700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084800030822     C                   ENDIF
084900030822     C* GVA
085000030822     C                   IF        %subst(
085100030822     C                             %subst(vlrppt:posDaDft+1:
085200030822     C                             posADft-posDaDft-1):1:3)
085300030822     C                             = 'GVA'
085400030822     C                   EVAL      VABGVA=%trim(%subst(
085500030822     C                             %subst(vlrppt:posDaDft+1:
085600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085700030822     C                   ENDIF
085800030822     C* TC1
085900030822     C                   IF        %subst(
086000030822     C                             %subst(vlrppt:posDaDft+1:
086100030822     C                             posADft-posDaDft-1):1:3)
086200030822     C                             = 'TC1'
086300030822     C                   EVAL      VABTC1=%trim(%subst(
086400030822     C                             %subst(vlrppt:posDaDft+1:
086500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086600030822     C                   ENDIF
086700030822     C* TC2
086800030822     C                   IF        %subst(
086900030822     C                             %subst(vlrppt:posDaDft+1:
087000030822     C                             posADft-posDaDft-1):1:3)
087100030822     C                             = 'TC2'
087200030822     C                   EVAL      VABTC2=%trim(%subst(
087300030822     C                             %subst(vlrppt:posDaDft+1:
087400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
087500030822     C                   ENDIF
087600040714     C* VATTRC
087700040714     C                   IF        %subst(
087800040714     C                             %subst(vlrppt:posDaDft+1:
087900040714     C                             posADft-posDaDft-1):1:3)
088000040714     C                             = 'TRC'
088100040714     C                   EVAL      VATTRC=%trim(%subst(
088200040714     C                             %subst(vlrppt:posDaDft+1:
088300040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
088400040714     C                   ENDIF
088500030822     C* ...
088600030822     C                   ENDIF
088700030822     C                   ENDDO
088800020204     C*
088900010330     C                   ENDSR
089000010607     C*----------------------------------------------------*
089100021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
089200010607     C*----------------------------------------------------*
089300010607     C     IMPVAB        BEGSR
089400010607     C*
089500010607     C                   SETOFF                                       3132
089600110315 xxx C***                SETON                                        35
089700030515     C*
089800010607     C                   MOVEL     datcor        VABAAS
089900010607     C                   MOVE      datcor        VABMGS
090000010607     C*
090100030715     C**********
090200030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
090300030715     C**********
090400030715     C                   IF        wGiro = *zeros
090500040113     C*
090600040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
090700040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
090800080429     C                             %trim(intcol(3))+%trim(intcol(4))+
090900080429     C                             %trim(intcol(5))<>*blanks
091000040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
091100080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
091200080429     C                                      %trim(intcol(5))
091300070823     C     ';':CharCSV   XLATE     vindta        vindta
091400040510     C                   ENDIF
091500031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
091600031201     C     minu:maiu     XLATE     vindta        vindta
091700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
091800030715     C                   DOW       posDa <= %len(%trim(vindta))
091900030715     C*
092000030715     C* Gestisco il 1° campo
092100030715     C                   IF        i = 1
092200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
092300030715     C                   EVAL      posDa = 1
092400030715     C* Eseguo lo scan x trovare la fine del primo campo
092500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092600030715     C* A questo punto "estraggo" il campo corrente
092700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
092800030715     C                                                    (posA-posDa))
092900030715     C* X i campi successivi al 1°
093000030715     C                   ELSE
093100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
093200030715     C                   EVAL      posDa = posA + 1
093300030715     C* Eseguo lo scan x trovare la fine del campo corrente
093400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
093500030715     C* Gestisco l'ultimo campo
093600030715     C                   IF        posA = *zeros
093700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
093800030715     C* A questo punto "estraggo" il campo corrente
093900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
094000030715     C                                                    (posA-posDa))
094100030715     C                   LEAVE
094200030715     C                   ELSE
094300030715     C* A questo punto "estraggo" il campo corrente
094400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
094500030715     C                                                    (posA-posDa))
094600030715     C                   ENDIF
094700030715     C                   ENDIF
094800030715     C* Incremento il contatore d campo
094900030715     C                   EVAL      i = i +1
095000030715     C                   ENDDO
095100030715     C*
095200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
095300030715     C* delimitazione testo
095400030715     C                   EVAL      i = 1
095500030715     C                   DOW       i < %elem(SkSplitFLD)
095600030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
095700030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
095800030715     C                   EVAL      i = i + 1
095900030715     C                   ENDDO
096000030715     C                   EVAL      wGiro = 1
096100030715     C                   ELSE
096200030715     C                   EVAL      wGiro = 2
096300031204     C**********
096400031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
096500031204     C* d separatore campo
096600031204     C**********
096700031204     C                   EVAL      vindta = %trim(vindta)
096800031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
096900031204     C                   EVAL      vindta = %subst(vindta:2)
097000031204     C                   ENDDO
097100031204     C*
097200031204     C                   Z-ADD     *zeros        lunghInput        4 0
097300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
097400031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
097500031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
097600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
097700031204     C                   ENDDO
097800030715     C**********
097900030715     C* Effettuo lo split del campo dati d input
098000030715     C**********
098100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
098200030715     C                   DOW       posDa <= %len(%trim(vindta))
098300030715     C*
098400030715     C* Gestisco il 1° campo
098500030715     C                   IF        i = 1
098600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
098700030715     C                   EVAL      posDa = 1
098800030715     C* Eseguo lo scan x trovare la fine del primo campo
098900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
099000030715     C* A questo punto "estraggo" il campo corrente
099100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
099200030715     C                                                    (posA-posDa))
099300030715     C* X i campi successivi al 1°
099400030715     C                   ELSE
099500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
099600030715     C                   EVAL      posDa = posA + 1
099700031204     C* Verifico che nn vi sia il campo nn valorizzato
099800031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
099900031204     C* Se campo nn valorizzato skippo al prossimo
100000031204     C                   EVAL      SkSplitCSV(i) = *blanks
100100031204     C                   EVAL      posA  = posA + 1
100200031204     C                   ELSE
100300030715     C* Eseguo lo scan x trovare la fine del campo corrente
100400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
100500030715     C* Gestisco l'ultimo campo
100600030715     C                   IF        posA = *zeros
100700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
100800030715     C* A questo punto "estraggo" il campo corrente
100900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
101000030715     C                                                    (posA-posDa))
101100030715     C                   LEAVE
101200030715     C                   ELSE
101300030715     C* A questo punto "estraggo" il campo corrente
101400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
101500030715     C                                                    (posA-posDa))
101600030715     C                   ENDIF
101700031204     C                   ENDIF
101800030715     C                   ENDIF
101900030715     C* Incremento il contatore d campo
102000030715     C                   EVAL      i = i +1
102100030715     C                   ENDDO
102200030715     C*
102300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
102400030715     C* delimitazione testo
102500030715     C                   EVAL      i = 1
102600030715     C                   DOW       i < %elem(SkSplitCSV)
102700030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
102800030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
102900030715     C                   EVAL      i = i + 1
103000030715     C                   ENDDO
103100100720     C*
103200100720     C* Stacco un progressivo x numero spedizione
103300110421 xxx C                   EXSR      repNSP
103400100326     C*
103500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
103600030715     C                   EVAL      i = 1
103700030715     C                   DOW       i < %elem(SkSplitFLD)
103800030715     C*
103900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
104000030715     C                   IF        SkSplitFLD(i) <> *blanks AND
104100030715     C                             SkSplitCSV(i) <> *blanks
104200030721     C*** ATB
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
104400030721     C                   EVAL      VABATB = SkSplitCSV(i)
104500030721     C                   ENDIF
104600030715     C*** RSD
104700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
104800030715     C                   EVAL      VABRSD = SkSplitCSV(i)
104900030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
105000030715     C     '@':'A'       XLATE     VABRSD        VABRSD
105100030715     C* ==
105200030715     C                   ENDIF
105300030721     C*** RD2
105400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
105500030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
105600030721     C                   ENDIF
105700030715     C*** IND
105800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
105900030715     C                   EVAL      VABIND = SkSplitCSV(i)
106000030715     C                   ENDIF
106100030715     C*** LOD
106200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
106300030715     C                   EVAL      VABLOD = SkSplitCSV(i)
106400030715     C                   ENDIF
106500030715     C*** PRD
106600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
106700030715     C                   EVAL      VABPRD = SkSplitCSV(i)
106800030715     C                   ENDIF
106900030721     C*** NZD
107000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
107100030721     C                   EVAL      VABNZD = SkSplitCSV(i)
107200030721     C                   ENDIF
107300030715     C*** RMA
107400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
107500030715     C                   EVAL      VABRMA = SkSplitCSV(i)
107600030715     C                   ENDIF
107700030715     C*** NOT
107800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
107900030715     C                   EVAL      VABNOT = SkSplitCSV(i)
108000030715     C                   ENDIF
108100030715     C*** NT2
108200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
108300030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
108400030715     C                   ENDIF
108500030715     C*** VCA
108600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
108700030715     C                   EVAL      VABVCA = SkSplitCSV(i)
108800030715     C                   ENDIF
108900030715     C*** VAS
109000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
109100030715     C                   EVAL      VABVAS = SkSplitCSV(i)
109200030715     C                   ENDIF
109300030715     C*** FFD
109400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
109500030715     C                   EVAL      VABFFD = SkSplitCSV(i)
109600030715     C                   ENDIF
109700030721     C*** GC1
109800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
109900030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
110000030721     C                   ENDIF
110100030721     C*** GC2
110200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
110300030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
110400030721     C                   ENDIF
110500030721     C*** TSP
110600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
110700030721     C                   EVAL      VABTSP = SkSplitCSV(i)
110800030721     C                   ENDIF
110900040510     C*** CBO
111000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
111100040510     C                   EVAL      VABCBO = SkSplitCSV(i)
111200040510     C                   ENDIF
111300030721     C*** NAS
111400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
111500030721     C                   EVAL      VABNAS = SkSplitCSV(i)
111600030721     C                   ENDIF
111700030721     C*** TIC
111800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
111900030721     C                   EVAL      VABTIC = SkSplitCSV(i)
112000030721     C                   ENDIF
112100030721     C*** GCA
112200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
112300030721     C                   EVAL      VABGCA = SkSplitCSV(i)
112400030721     C                   ENDIF
112500030721     C*** XCO
112600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
112700030721     C                   EVAL      VABXCO = SkSplitCSV(i)
112800030721     C                   ENDIF
112900030721     C*** CTM
113000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
113100030721     C                   EVAL      VABCTM = SkSplitCSV(i)
113200030721     C                   ENDIF
113300030721     C*** TCR
113400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
113500030721     C                   EVAL      VABTCR = SkSplitCSV(i)
113600030721     C                   ENDIF
113700030721     C*** CTS
113800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
113900030721     C                   EVAL      VABCTS = SkSplitCSV(i)
114000030721     C                   ENDIF
114100030721     C*** FTM
114200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
114300030721     C                   EVAL      VABFTM = SkSplitCSV(i)
114400030721     C                   ENDIF
114500030721     C*** VAD
114600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
114700030721     C                   EVAL      VABVAD = SkSplitCSV(i)
114800030721     C                   ENDIF
114900030721     C*** GMA
115000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
115100030721     C                   EVAL      VABGMA = SkSplitCSV(i)
115200030721     C                   ENDIF
115300030721     C*** GGA
115400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
115500030721     C                   EVAL      VABGGA = SkSplitCSV(i)
115600030721     C                   ENDIF
115700030721     C*** GVA
115800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
115900030721     C                   EVAL      VABGVA = SkSplitCSV(i)
116000030721     C                   ENDIF
116100030721     C*** TC1
116200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
116300030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
116400030721     C                   ENDIF
116500030721     C*** TC2
116600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
116700030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
116800030721     C                   ENDIF
116900030721     C*** SCL
117000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
117100030721     C                   EVAL      VABSCL = SkSplitCSV(i)
117200030721     C                   ENDIF
117300030721     C*** RMO
117400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
117500030721     C                   EVAL      VABRMO = SkSplitCSV(i)
117600030721     C                   ENDIF
117700030721     C*** NMO
117800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
117900030721     C                   EVAL      VABNMO = SkSplitCSV(i)
118000030721     C                   ENDIF
118100030721     C*
118200030721     C* Reperisco quindi i campi numerici...
118300040510     C*** NSP
118400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
118500040510     C                   EVAL      PiStr=SkSplitCSV(i)
118600040510     C                   EXSR      CHKNUM
118700040510     C                   IF        PiInt=*on
118800040510     C                   Z-ADD     PiVal         VABNSP
118900040510     C                   ELSE
119000040510     C                   SETON                                        31
119100040510     C                   EVAL      VABNSP = *zeros
119200040510     C                   EVAL      vinmsg = %trimr(vinmsg)
119300040510     C                             + ' ' + 'VABNSP'
119400040510     C                   ENDIF
119500040510     C                   ENDIF
119600071106     C*** RMN
119700071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
119800071106     C                   EVAL      PiStr=SkSplitCSV(i)
119900071106     C                   EXSR      CHKNUM
120000071106     C                   IF        PiInt=*on
120100071106     C                   Z-ADD     PiVal         VABRMN
120200071106     C                   ELSE
120300071106     C                   SETON                                        32
120400071106     C                   EVAL      VABRMN = *zeros
120500071106     C                   EVAL      vinmsg = %trimr(vinmsg)
120600071106     C                             + ' ' + 'VABRMN'
120700071106     C                   ENDIF
120800071106     C                   ENDIF
120900030721     C*** CAD
121000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
121100030721     C                   EVAL      PiStr=SkSplitCSV(i)
121200030721     C                   EXSR      CHKNUM
121300030721     C                   IF        PiInt=*on
121400030721     C                   Z-ADD     PiVal         Num5_0
121500030721     C                   MOVEL(p)  Num5_0        VABCAD
121600030721     C                   ELSE
121700030721     C                   SETON                                        32
121800030721     C                   EVAL      VABCAD = *zeros
121900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122000030721     C                             + ' ' + 'VABCAD'
122100030721     C                   ENDIF
122200030721     C                   ENDIF
122300030721     C*** NCL
122400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
122500030721     C                   EVAL      PiStr=SkSplitCSV(i)
122600030721     C                   EXSR      CHKNUM
122700030721     C                   IF        PiInt=*on
122800030721     C                   Z-ADD     PiVal         VABNCL
122900030721     C                   ELSE
123000030721     C                   SETON                                        32
123100030721     C                   EVAL      VABNCL = *zeros
123200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123300040122     C                             + ' ' + 'VABNCL'
123400030721     C                   ENDIF
123500030721     C                   ENDIF
123600030721     C*** PKB
123700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
123800030721     C                   EVAL      PiStr=SkSplitCSV(i)
123900030721     C                   EXSR      CHKNUM
124000030721     C                   IF        PiNum=*on
124100030721     C                   Z-ADD     PiVal         VABPKB
124200030721     C                   ELSE
124300030721     C                   SETON                                        32
124400030721     C                   EVAL      VABPKB = *zeros
124500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124600030721     C                             + ' ' + 'VABPKB'
124700030721     C                   ENDIF
124800030721     C                   ENDIF
124900030721     C*** CAS
125000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
125100110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
125200110506     C                             %trim(SkSplitCSV(i)) <> *zeros
125300101018     C                   EVAL      wFlgCAS = '1'
125400101018     C                   ENDIF
125500030721     C                   EVAL      PiStr=SkSplitCSV(i)
125600030721     C                   EXSR      CHKNUM
125700030721     C                   IF        PiNum=*on
125800030721     C                   Z-ADD     PiVal         VABCAS
125900030721     C                   ELSE
126000030721     C                   SETON                                        32
126100030721     C                   EVAL      VABCAS = *zeros
126200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126300030721     C                             + ' ' + 'VABCAS'
126400030721     C                   ENDIF
126500030721     C                   ENDIF
126600030721     C*** CCM
126700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
126800030721     C                   EVAL      PiStr=SkSplitCSV(i)
126900030721     C                   EXSR      CHKNUM
127000030721     C                   IF        PiInt=*on      AND
127100030721     C                             PiVal<=9999999 AND
127200030721     C                             PiVal>*zeros
127300030721     C                   Z-ADD     PiVal         VABCCM
127400030721     C                   ELSE
127500030721     C                   SETON                                        32
127600030721     C                   EVAL      VABCCM = *zeros
127700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127800040510     C                             + ' ' + 'VABCCM'
127900030721     C                   ENDIF
128000030721     C                   ENDIF
128100030721     C*** LNP
128200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
128300030721     C                   EVAL      PiStr=SkSplitCSV(i)
128400030721     C                   EXSR      CHKNUM
128500030721     C                   IF        PiInt=*on    AND
128600030721     C                             PiVal<=999   AND
128700030721     C                             PiVal>*zeros
128800030721     C                   Z-ADD     PiVal         VABLNP
128900030721     C                   ELSE
129000040715     C                   SETON                                        32
129100030721     C                   EVAL      VABLNP = *zeros
129200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129300040510     C                             + ' ' + 'VABLNP'
129400030721     C                   ENDIF
129500030721     C                   ENDIF
129600030721     C*** NRS
129700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
129800030721     C                   EVAL      PiStr=SkSplitCSV(i)
129900030721     C                   EXSR      CHKNUM
130000030721     C                   IF        PiInt=*on AND
130100030721     C                             PiVal<=99
130200030721     C                   Z-ADD     PiVal         VABNRS
130300030721     C                   ELSE
130400030721     C                   SETON                                        32
130500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130600030721     C                             + ' ' + 'VABNRS'
130700030721     C                   ENDIF
130800030721     C                   ENDIF
130900030721     C*** LNA
131000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
131100030721     C                   EVAL      PiStr=SkSplitCSV(i)
131200030721     C                   EXSR      CHKNUM
131300030721     C                   IF        PiInt=*on    AND
131400030721     C                             PiVal<=999   AND
131500030721     C                             PiVal>*zeros
131600030721     C                   Z-ADD     PiVal         VABLNA
131700030721     C                   ELSE
131800030721     C                   SETON                                        32
131900030721     C                   EVAL      VABLNA = *zeros
132000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132100030721     C                             + ' ' + 'VABLNA'
132200030721     C                   ENDIF
132300030721     C                   ENDIF
132400030721     C*** CTR
132500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
132600030721     C                   EVAL      PiStr=SkSplitCSV(i)
132700030721     C                   EXSR      CHKNUM
132800030721     C                   IF        PiInt=*on  AND
132900030721     C                             PiVal<=999
133000030721     C                   Z-ADD     PiVal         VABCTR
133100030721     C                   ELSE
133200030721     C                   SETON                                        32
133300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133400030721     C                             + ' ' + 'VABCTR'
133500030721     C                   ENDIF
133600030721     C                   ENDIF
133700030721     C*** IAS
133800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
133900030721     C                   EVAL      PiStr=SkSplitCSV(i)
134000030721     C                   EXSR      CHKNUM
134100030721     C                   IF        PiNum=*on
134200030721     C                   Z-ADD     PiVal         VABIAS
134300030721     C                   ELSE
134400030721     C                   SETON                                        32
134500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134600030721     C                             + ' ' + 'VABIAS'
134700030721     C                   ENDIF
134800030721     C                   ENDIF
134900030721     C*** VLB
135000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
135100030721     C                   EVAL      PiStr=SkSplitCSV(i)
135200030721     C                   EXSR      CHKNUM
135300030721     C                   IF        PiNum=*on
135400030721     C                   Z-ADD     PiVal         VABVLB
135500030721     C                   ELSE
135600030721     C                   SETON                                        32
135700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135800030721     C                             + ' ' + 'VABVLB'
135900030721     C                   ENDIF
136000030721     C                   ENDIF
136100030721     C*** QFT
136200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
136300030721     C                   EVAL      PiStr=SkSplitCSV(i)
136400030721     C                   EXSR      CHKNUM
136500030721     C                   IF        PiNum=*on
136600030721     C                   Z-ADD     PiVal         VABQFT
136700030721     C                   ELSE
136800030721     C                   SETON                                        32
136900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137000030721     C                             + ' ' + 'VABQFT'
137100030721     C                   ENDIF
137200030721     C                   ENDIF
137300030721     C*** NCD
137400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
137500030721     C                   EVAL      PiStr=SkSplitCSV(i)
137600030721     C                   EXSR      CHKNUM
137700030721     C                   IF        PiInt=*on      AND
137800030721     C                             PiVal<=9999999
137900030721     C                   Z-ADD     PiVal         VABNCD
138000030721     C                   ELSE
138100030721     C                   SETON                                        32
138200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138300030721     C                             + ' ' + 'VABNCD'
138400030721     C                   ENDIF
138500030721     C                   ENDIF
138600030721     C*** NCA
138700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
138800030721     C                   EVAL      PiStr=SkSplitCSV(i)
138900030721     C                   EXSR      CHKNUM
139000030721     C                   IF        PiInt=*on      AND
139100030721     C                             PiVal<=9999999
139200030721     C                   Z-ADD     PiVal         VABNCA
139300030721     C                   ELSE
139400030721     C                   SETON                                        32
139500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139600030721     C                             + ' ' + 'VABNCA'
139700030721     C                   ENDIF
139800030721     C                   ENDIF
139900030721     C*** ZNC
140000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
140100030721     C                   EVAL      PiStr=SkSplitCSV(i)
140200030721     C                   EXSR      CHKNUM
140300030721     C                   IF        PiInt=*on AND
140400030721     C                             PiVal<=99
140500030721     C                   Z-ADD     PiVal         VABZNC
140600030721     C                   ELSE
140700030721     C                   SETON                                        32
140800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140900030721     C                             + ' ' + 'VABZNC'
141000030721     C                   ENDIF
141100030721     C                   ENDIF
141200030721     C*** DCR
141300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
141400030721     C                   EVAL      PiStr=SkSplitCSV(i)
141500030721     C                   EXSR      CHKNUM
141600030721     C                   IF        PiInt=*on       AND
141700030721     C                             PiVal<=99999999
141800030721     C                   Z-ADD     PiVal         VABDCR
141900030721     C                   ELSE
142000030721     C                   SETON                                        32
142100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142200030721     C                             + ' ' + 'VABDCR'
142300030721     C                   ENDIF
142400030721     C                   ENDIF
142500030721     C*** HCR
142600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
142700030721     C                   EVAL      PiStr=SkSplitCSV(i)
142800030721     C                   EXSR      CHKNUM
142900030721     C                   IF        PiInt=*on   AND
143000030721     C                             PiVal<=9999
143100030721     C                   Z-ADD     PiVal         VABHCR
143200030721     C                   ELSE
143300030721     C                   SETON                                        32
143400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
143500030721     C                             + ' ' + 'VABHCR'
143600030721     C                   ENDIF
143700030721     C                   ENDIF
143800030721     C*** VMD
143900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
144000030721     C                   EVAL      PiStr=SkSplitCSV(i)
144100030721     C                   EXSR      CHKNUM
144200030721     C                   IF        PiNum=*on
144300030721     C                   Z-ADD     PiVal         VABVMD
144400030721     C                   ELSE
144500030721     C                   SETON                                        32
144600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
144700030721     C                             + ' ' + 'VABVMD'
144800030721     C                   ENDIF
144900030721     C                   ENDIF
145000030721     C*** ANT
145100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
145200030721     C                   EVAL      PiStr=SkSplitCSV(i)
145300030721     C                   EXSR      CHKNUM
145400030721     C                   IF        PiInt=*on        AND
145500030721     C                             PiVal<=999999999
145600030721     C                   Z-ADD     PiVal         VABANT
145700030721     C                   ELSE
145800030721     C                   SETON                                        32
145900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
146000030721     C                             + ' ' + 'VABANT'
146100030721     C                   ENDIF
146200030721     C                   ENDIF
146300030721     C*** CMO
146400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
146500030721     C                   EVAL      PiStr=SkSplitCSV(i)
146600030721     C                   EXSR      CHKNUM
146700030721     C                   IF        PiInt=*on
146800030721     C                   Z-ADD     PiVal         Num5_0
146900030721     C                   MOVEL(p)  Num5_0        VABCMO
147000030721     C                   ELSE
147100030721     C                   SETON                                        32
147200030721     C                   EVAL      VABCMO = *zeros
147300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
147400030721     C                             + ' ' + 'VABCMO'
147500030721     C                   ENDIF
147600030721     C                   ENDIF
147700040510     C*
147800040510     C* Reperisco i campi del FIVAT
147900040510     C*** VATNOT
148000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
148100040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
148200040510     C                   ENDIF
148300040510     C*** VATTRC
148400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
148500040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
148600040510     C                   ENDIF
148700060331     C*** VATNOT_A
148800060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
148900060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
149000060331     C                   ENDIF
149100060331     C*** VATNOT_B
149200060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
149300060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
149400060331     C                   ENDIF
149500060331     C*** VATNOT_E
149600070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
149700060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
149800060331     C                   ENDIF
149900070823     C*
150000100831     C***  ===>  Gestione campi "particolari" dentro ciclo
150100110421     C*
150200110623     C*** NSP
150300110623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
150400110623     C                   EVAL      PiStr=SkSplitCSV(i)
150500110623     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
150600110623     C                   EXSR      CHKNUM
150700110623     C                   IF        PiInt=*on
150800110623     C                   Z-ADD     PiVal         VABNSP
150900110623     C                   ELSE
151000110623     C                   SETON                                        31
151100110623     C                   EVAL      VABNSP = *zeros
151200110623     C                   EVAL      vinmsg = %trimr(vinmsg)
151300110623     C                             + ' ' + 'VABNSP VATNOT_E'
151400110623     C                   ENDIF
151500110623     C                   ENDIF
151600070925     C*
151700071029     C***  <===  -----------------------------------------
151800040510     C*
151900030715     C                   ENDIF
152000030715     C                   EVAL      i = i + 1
152100030715     C                   ENDDO
152200070925     C*
152300071029     C***  ===>  Gestione campi "particolari" fuori ciclo
152400100831     C*
152500100831     C                   IF        wVABRSD <> *blanks
152600100831     C                   EVAL      VABRSD = wVABRSD
152700100831     C                   ENDIF
152800100831     C*
152900100831     C                   IF        wVABIND <> *blanks
153000100831     C                   EVAL      VABIND = wVABIND
153100100831     C                   ENDIF
153200100831     C*
153300100831     C                   IF        wVABCAD <> *blanks
153400100831     C                   EVAL      VABCAD = wVABCAD
153500100831     C                   ENDIF
153600100831     C*
153700100831     C                   IF        wVABLOD <> *blanks
153800100831     C                   EVAL      VABLOD = wVABLOD
153900100831     C                   ENDIF
154000100831     C*
154100100831     C                   IF        wVABPRD <> *blanks
154200100831     C                   EVAL      VABPRD = wVABPRD
154300100831     C                   ENDIF
154400070925     C*
154500071029     C***  <===  -----------------------------------------
154600070925     C*
154700070925     C* Considerazioni finali su CBO/CAS
154800100318     C                   IF        wFlgCAS = '1'    OR
154900100318     C                             VABCAS  > *zeros
155000070925     C                   IF        VABCBO = '1'
155100070925     C                   EVAL      VABCBO = '4'
155200070925     C                   ENDIF
155300070925     C                   IF        VABCBO = '2'
155400070925     C                   EVAL      VABCBO = '6'
155500070925     C                   ENDIF
155600070925     C                   ENDIF
155700070925     C***  <===  ----------------------------
155800040510     C*
155900040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
156000040510     C                   IF        VABNSP = *zeros
156100040510     C                   SETON                                        31
156200040510     C                   ENDIF
156300040715     C*
156400040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
156500040715     C                   IF        VABLNP = *zeros
156600040715     C                   SETON                                        31
156700040715     C                   ENDIF
156800040510     C*
156900040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
157000040510     C                   IF        VABRMN = *zeros
157100040510     C                   Z-ADD     VABNSP        VABRMN
157200040510     C                   ENDIF
157300040510     C*
157400040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
157500040510     C                   IF        VABRMA = *blanks
157600040714     C                   MOVEL     VABRMN        VABRMA
157700040510     C                   ENDIF
157800040128     C*
157900040510     C* Se provincia nn valorizzata la reperisco
158000040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
158100040510     C                   IF        VABPRD = *blanks
158200040128     C                   CLEAR                   TISI95DS
158300040128     C                   EVAL      I95TCN = '3'
158400040128     C                   Z-ADD     datcor        I95DAT
158500040128     C                   EVAL      I95NAR = VABNZD
158600040128     C                   EVAL      I95CAP = VABCAD
158700040128     C                   EVAL      I95LOC = VABLOD
158800040128     C                   CALL      'TISI95R'
158900040128     C                   PARM                    TISI95DS
159000040128     C                   EVAL      VABPRD = O95PRV
159100040510     C                   ENDIF
159200020204     C*
159300020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
159400020204     C                   EXSR      CHKIMPDIV
159500030715     C                   ENDIF
159600020204     C*
159700010607     C                   ENDSR
159800010607     C*----------------------------------------------------*
159900010601
160000020204
160100020204     C*----------------------------------------------------*
160200020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
160300020204     C*----------------------------------------------------*
160400020204     C     CHKIMPDIV     BEGSR
160500020204     C*
160600020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
160700020204     C                   Z-ADD     *zeros        wrkDec            9 9
160800020204     C*
160900020204     C* Come prima cosa effettuo considerazioni sulla divisa
161000020204     C                   IF        vabIAS > *zeros
161100020204     C                   IF        vabVAS <> 'EUR'
161200020204     C                   EVAL      vabVAS =  'ITL'
161300020204     C                   ENDIF
161400020204     C                   ENDIF
161500020204     C*
161600020204     C                   IF        vabCAS > *zeros
161700020204     C                   IF        vabVCA <> 'EUR'
161800020204     C                   EVAL      vabVCA =  'ITL'
161900020204     C                   ENDIF
162000020204     C                   ENDIF
162100020204     C*
162200020204     C                   IF        vabVMD > *zeros
162300020204     C                   IF        vabVAD <> 'EUR'
162400020204     C                   EVAL      vabVAD =  'ITL'
162500020204     C                   ENDIF
162600020204     C                   ENDIF
162700020204     C*
162800020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
162900020204     C                   Z-ADD     vabIAS        wrkDec
163000020204     C                   IF        wrkDec > *zeros
163100020204     C                   IF        vabVAS = 'ITL'
163200020204     C                   EVAL      vabIAS = *zeros
163300020204     C                   ENDIF
163400020204     C                   ENDIF
163500020204     C*
163600020204     C* Stabilisco se il contrasegno ha decimali valorizzati
163700020204     C                   Z-ADD     vabCAS        wrkDec
163800020204     C                   IF        wrkDec > *zeros
163900020204     C                   IF        vabVCA = 'ITL'
164000020204     C                   EVAL      vabCAS = *zeros
164100020204     C                   ENDIF
164200020204     C                   ENDIF
164300020204     C*
164400020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
164500020204     C                   Z-ADD     vabVMD        wrkDec
164600020204     C                   IF        wrkDec > *zeros
164700020204     C                   IF        vabVAD = 'ITL'
164800020204     C                   EVAL      vabVMD = *zeros
164900020204     C                   ENDIF
165000020204     C                   ENDIF
165100020204     C*
165200020204     C                   ENDSR
165300020204     C***
165400010330
165500100318
165600100318
165700100318     C*----------------------------------------------------*
165800100318     C*  CONTROLLO NUMERICITA' CAMPI
165900100318     C*----------------------------------------------------*
166000100318     C     CHKNUM        BEGSR
166100100318     C*
166200100318     C                   IF        PiDecChr = *blanks
166300100318     C                   EVAL      PiDecChr = CharNUM
166400100318     C                   ENDIF
166500100318     C*
166600100318     C                   callp     UBISNUM_Check(PiStr
166700100318     C                                          :PiDecChr
166800100318     C                                          :PiVal
166900100318     C                                          :PiNum
167000100318     C                                          :PiInt)
167100100318     C*
167200100318     C                   ENDSR
167300100318     C***
167400010601
167500010601
167600010601
167700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
167800010601     C     repfil        BEGSR
167900010601     C*
168000010601     C                   if        invfil = *zeros and
168100010601     C                             depfil > *zeros and
168200010629     C                             (vinflg = *blanks or
168300010629     C                              vinflg = *zeros)
168400010601     C
168500010601     C                   eval      invfil = depfil
168600010601     C                   endif
168700010601     C*
168800010601     C                   if        depfil <> invfil and
168900010601     C                             invfil > *zeros
169000010601     C                   eval      flgMulti = '1'
169100010601     C                   if        vinflg = *blanks
169200010601     C                   add       1             cntNonEl
169300010601     C                   endif
169400010601     C                   endif
169500010601     C*
169600010601     C                   if        vinflg = '2'
169700010601     C                   eval      flgStato = '2'
169800010601     C                   endif
169900010601     C*
170000010601     C                   ENDSR
170100010601     C***
170200010601
170300010601
170400010601
170500010330
170600040119      /TITLE Invio dei dati al punto operativo.
170700040119     C     invio         BEGSR
170800040119     C*
170900040119     C* 1° invio FIVAT
171000040119     C                   reset                   dscmz
171100040119     C                   move      vlrpoi        cmzdst
171200040119     C                   eval      cmzfld = 'FIVATWWR'
171300040119     C                   eval      cmzmbd = vlrhdl
171400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
171500040119     C***                if        prmfir = *blanks
171600040119     C                   eval      cmzfla = 'FIVAT00F'
171700040119     C                   eval      cmzmba = 'FIVAT00F'
171800040119     C***                else
171900040119     C***                eval      cmzfla = prmfir
172000040119     C***                eval      cmzmba = prmfir
172100040119     C***                endif
172200040119     C                   eval      cmznrr = *zeros
172300040119     C                   move      §ctrokvt      cmznrr
172400040119     C                   eval      cmzlba = vlrfl1
172500040119     C                   call(e)   'TIS711C'
172600040119     C                   parm                    dscmz
172700040119     C                   parm      *blanks       esito
172800040119     C                   if        %error
172900040119     C                             or cmzerr = '1'
173000040119     C                             or esito  = '1'
173100040119     C                   eval      wrkesito = '3'
173200040119     C                   else
173300040119     C*
173400040119     C* 2° invio FIVAB
173500040119     C                   reset                   dscmz
173600040119     C                   move      vlrpoi        cmzdst
173700040119     C                   eval      cmzfld = vlrfou
173800040119     C                   eval      cmzmbd = vlrhdl
173900040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
174000040119     C***                if        prmfir = *blanks
174100040119     C                   eval      cmzfla = 'FIVAB00F'
174200040119     C                   eval      cmzmba = 'FIVAB00F'
174300040119     C***                else
174400040119     C***                eval      cmzfla = prmfir
174500040119     C***                eval      cmzmba = prmfir
174600040119     C***                endif
174700040119     C                   eval      cmznrr = *zeros
174800040119     C                   move      §ctrokvb      cmznrr
174900040119     C                   eval      cmzlba = vlrfl1
175000040119     C                   call(e)   'TIS711C'
175100040119     C                   parm                    dscmz
175200040119     C                   parm      *blanks       esito
175300040119     C                   if        %error
175400040119     C                             or cmzerr = '1'
175500040119     C                             or esito  = '1'
175600040119     C                   eval      wrkesito = '3'
175700040119     C                   endif
175800040119     C                   endif
175900040119     C*
176000040119     C                   ENDSR
176100040119     C***
176200010601
176300010601
176400010601
176500010601
176600010601      /TITLE Invio dei dati al punto operativo.
176700010601     C     opeini        BEGSR
176800010601     C*
176900010601     C* Inizializzo flag e contatori operativi
177000010601     C                   movel     '0'           flgGiro           1
177100010601     C                   movel     '0'           flgMulti          1
177200010601     C                   movel     '1'           flgStato          1
177300010615     C                   movel     '0'           flgOk             1
177400010601     C                   z-add     *zeros        cntNonEl         10 0
177500010601     C                   z-add     *zeros        depfil            3 0
177600010601     C                   z-add     *zeros        invfil            3 0
177700010601     C*
177800010601     C                   ENDSR
177900010601     C***
178000070326
178100070326
178200070326
178300070326
178400070326     C     *pssr         BEGSR
178500070329     C*
178600070329     C                   if        %open(tivin00r)
178700070329     C                   close     tivin00r
178800070329     C                   endif
178900070329     C                   if        %open(fivabwwr)
179000070329     C                   close     fivabwwr
179100070329     C                   endif
179200070329     C                   if        %open(fivatwwr)
179300070329     C                   close     fivatwwr
179400070329     C                   endif
179500070326     C*
179600070326     C* Effettuo la chiamata al CLLE preposto
179700070326     C                   call(e)   'TITVVTC'
179800070326     C                   parm                    parccm
179900070326     C                   parm                    parmbr
180000070326     C                   parm      '2'           paropz
180100070326     C*
180200070326     C                   eval      wrkesito = '2'
180300070404     C*
180400070404     C                   seton                                        LR
180500070326     C*
180600070326     C                   ENDSR     '*CANCL'
180700070326     C***
180800100318
180900100318
181000100318
181100100318     C     normCSV       BEGSR
181200100318     C*
181300100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
181400100318     C* separatori campi
181500100318     C                   if        CharTXT <> *blanks
181600100318     C                   z-add     *zeros        wLenVINDTA        4 0
181700100318     C                   z-add     *zeros        wFlgVINDTA        1 0
181800100318     C                   setoff                                       80
181900100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
182000100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
182100100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
182200100318     C                   if        not *in80
182300100318     C                   seton                                        80
182400100318     C                   else
182500100318     C                   setoff                                       80
182600100318     C                   endif
182700100318     C                   endif
182800100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
182900100318     C                             and *in80 = *on
183000100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
183100100318     C                   endif
183200100318     C                   enddo
183300100318     C                   endif
183400100318     C*
183500100318     C                   ENDSR
183600100318     C***
183700070326
183800100326
183900100326
184000100326     C*----------------------------------------------------*
184100100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
184200100326     C*----------------------------------------------------*
184300100326     C     REPNSP        BEGSR
184400100326     C*
184500100326     C* NSP => Stacco un numeratore da AZNUM
184600100326     C                   clear                   TRUL33DS
184700100326     C                   eval      I33OPE = *zeros
184800100326     C                   eval      I33CNU = 302
184900100326     C                   eval      I33NUM = 1
185000100326     C                   movel     TRUL33DS      KPJBU
185100100326     C                   call      'TRUL33R'
185200100326     C                   parm                    KPJBA
185300100326     C                   movel     KPJBU         TRUL33DS
185400100326     C                   if        O33ERR = *zeros
185500100326     C                   z-add     O33NRF        VABNSP
185600100326     C                   z-add     O33NRF        VATNSP
185700100326     C                   else
185800100326     C                   SETON                                        31
185900100326     C                   EVAL      vinmsg = %trimr(vinmsg)
186000100326     C                             + ' ' + 'VABNSP VATNSP'
186100100326     C                   endif
186200100326     C*
186300100326     C                   ENDSR
186400010330
186500100326
186600010330
186700000613     C     *inzsr        BEGSR
186800990910     C*
186900990910     C     *entry        plist
187000990920     C                   parm                    tivlrds
187100990921     C                   parm      wrkesito      esito
187200000724     C                   parm                    prmlit
187300000710     C                   parm                    prmfir
187400010330     C*
187500010330     C* CALCOLA LA DATA CORRENTE
187600100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
187700100318     C                   eval      datcor = %dec(%date() : *ISO)
187800000613     C*
187900000613     C                   ENDSR
188000000613     C***
188100990908
188200110623     Otitv2g3p  E            riepilogo         2
188300990915     O                                              'Upload via Internet'
188400990915     O                                           +1 'Traduzione TIVIN00R -'
188500021025     O                                           +1 'FIVABWWR'
188600990915     O                                           +1 'Testate bolle'
188700990915     O          E            riepilogo   2
188800990915     O                       wrkdata
188900990915     O                       wrkora              +1
189000990915     O                       procname            +1
189100990915     O          E            riepilogo   2
189200990915     O                                              'Cliente..................:'
189300010605     O                       VABCCM        z     +1
189400990915     O          E            riepilogo   2
189500990920     O                                              'Riferimento Strategi.....:'
189600990920     O                       vlrhdl              +1
189700990915     O          E            riepilogo   2
189800990915     O                                              'Giusti...................:'
189900040119     O                       §CTROKVB      2   +  1
190000990915     O          E            riepilogo   2
190100990915     O                                              'Sbagliati e corretti.....:'
190200971022     O                       §CTRMO        2   +  1
190300990915     O          E            riepilogo   2
190400990915     O                                              'Sbagliati e scartati.....:'
190500971022     O                       §CTRNO        2   +  1
190600000613
190700110623     Otitv2g3ps E            testdett          2
190800000613     O                                              'Upload via Internet'
190900000613     O                                           +1 'Traduzione TIVIN00R -'
191000021025     O                                           +1 'FIVABWWR'
191100000613     O                                           +1 'Testate bolle'
191200000616     O          E            testdett    3
191300000613     O                                           +2 'N° rec'
191400000613     O                                           +3 'Anteprima contenuto'
191500000616     O          E            rigadett    2
191600000613     O                       rrnum               +2
191700000621     O                       recko               +3
191800000616     O          E            findett     2
191900000613     O                       wrkdata
192000000613     O                       wrkora              +1
192100000613     O                       procname            +1
192200000616     O          E            findett     2
192300000613     O                                              'Cliente..................:'
192400010605     O                       VABCCM        z     +1
192500000616     O          E            findett     2
192600000613     O                                              'Riferimento Strategi.....:'
192700000613     O                       vlrhdl              +1
192800000616     O          E            findett     2
192900000613     O                                              'Giusti...................:'
193000040119     O                       §CTROKVB      2   +  1
193100000616     O          E            findett     2
193200000613     O                                              'Sbagliati e corretti.....:'
193300000613     O                       §CTRMO        2   +  1
193400000616     O          E            findett     2
193500000613     O                                              'Sbagliati e scartati.....:'
193600000613     O                       §CTRNO        2   +  1
193700000613** CMD - COMANDI CL
193800110623OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV2G3P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
193900110623OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV2G3PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
194000110623DLTOVR FILE(TITV2G3P TITV2G3PS) LVL(*)
194100010606
194200000613
194300040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
194400110623
194500110623
194600110315
194700110315
194800100318
