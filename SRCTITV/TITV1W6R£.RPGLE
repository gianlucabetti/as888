000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200100318     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400100705     Ftabel00f  if   e           k disk
000500000724     Fazorg01l  if   e           k disk
000600990910     Ftivin00r  uF   E             DISK    usropn
000700100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
000800040119     FFIVATwwr  O    E             DISK    usropn
000900100413     Ftitv1w6p  O    f  132        PRINTER usropn
001000000621     F                                     oflind(*inoa)
001100070329     F                                     infsr(*pssr)
001200100413     Ftitv1w6ps O    f  198        PRINTER usropn
001300000621     F                                     oflind(*inob)
001400070329     F                                     infsr(*pssr)
001500000313     D*
001600000613     D*------------
001700000613     D* COMANDI
001800000613     D*------------
001900011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
002000040113     D*------------
002100040113     D* SCHIEDE A PROGRAMMA
002200040113     D*------------
002300080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
002400010330     D*----------------------------------------------------
002500010330     D* DICHIARAZIOINE VARIABILI DI WRK
002600010330     D*----------------------------------------------------
002700010330     D dscmz         e ds                  inz
002800010330     D psds           sds
002900010330     D  procname         *PROC
003000010330     D tivlrds       e ds                  extname(tivlr00f)
003100040128     D tisi95ds      e ds
003200100705     D ds15          e ds
003300100413     D fivabds       e ds                  extname(fivab00f)
003400100413     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
003500100413     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
003600010330     D esito           s              1
003700010330     D prmlit          s             10
003800010330     D prmfir          s             10
003900010330     D wrkesito        s                   like(esito)
004000010330     D wrkdata         s               d
004100010330     D wrkora          s               t
004200010330     D rrnum           s              6  0 INZ(*zeros)
004300010330     D recko           s            150    INZ(*blanks)
004400011113     D depcmd          s            150    INZ(*blanks)
004500040510     D parccm          s              8    INZ(*blanks)
004600040119     D parmbr          s             10    INZ(*blanks)
004700040119     D paropz          s              1    INZ(*blanks)
004800040119     D chkcall         s              1    INZ(*blanks)
004900080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
005000100705
005100100705     D*------------
005200100705     D* GESTIONE NAZIONI ESTERE
005300100705     D*------------
005400100705     D wNAZ            S              3    INZ
005500100705     D jNAZ            S              5  0 INZ
005600100705     D skNAZISO        S              3    DIM(1000)
005700100705     D skNAZBAR        S              3    DIM(1000)
005800100705
005900100705
006000100326     D*------------------
006100100326     D* DS REPERIMENTO NUMERATORE
006200100326     D*------------------
006300100326     D trul33ds      e ds                  inz
006400100326     D*------------------
006500100326     D* DS ARCHITETTURA
006600100326     D*------------------
006700100326     D kpjba         e ds                  inz
006800100326
006900030715     D*------------------
007000030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
007100030715     D*------------------
007200030715     D SkSplitFLD      S             10    DIM(100)
007300030715     D SkSplitCSV      S            256    DIM(100)
007400030715     D CharCSV         S              1
007500030715     D CharTXT         S              1
007600030715     D CharNUM         S              1
007700100318     D CharSOS         S              1
007800030715     D posDa           S              3  0 INZ(*zeros)
007900030715     D posA            S              3  0 INZ(*zeros)
008000030715     D i               s              3  0 INZ(1)
008100030715     D wGiro           s              1  0 INZ(*zeros)
008200070823     D wVATNOT_A       s                   LIKE(VATNOT)
008300060331     D wVATNOT_B       s                   LIKE(VATNOT)
008400060331     D wVATNOT_E       s                   LIKE(VATNOT)
008500030822     D*------------------
008600030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
008700030822     D*------------------
008800030822     D posDaDft        S              3  0 INZ(*zeros)
008900030822     D posADft         S              3  0 INZ(*zeros)
009000030822     D j               s              3  0 INZ(1)
009100030822     D wGiroDft        s              1  0 INZ(*zeros)
009200080418     D*------------------
009300080418     D* Costanti
009400080418     D*------------------
009500080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009600080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009700100318
009800100318     D*------------------
009900100318     D* LINKING A DEFINIZIONI ESTERNE
010000100318     D*------------------
010100100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
010200100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
010300100318
010400010330
010500010330
010600100705     C*
010700100705     C* Carico le tabelle
010800100705     C                   exsr      cartab
010900100705     C*
011000990915     C                   time                    wrkdata
011100990915     C                   time                    wrkora
011200000913     C                   reset                   rrnum
011300990921     C                   reset                   esito
011400990921     C                   reset                   wrkesito
011500010601     C*
011600010601     C                   exsr      opeini
011700010605     C                   exsr      rwvab
011800010601     C*
011900010601     C                   seton                                        lr
012000010601
012100010601
012200010601
012300010601     C*--------------------------------------------------------
012400010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
012500010601     C*--------------------------------------------------------
012600010601     C     PREELA        BEGSR
012700010601     C*
012800010601     C* SE OCCORRE SPEDIRE IN FILIALE
012900010601     C                   if        invfil <> *zeros and
013000010601     C                             flgGiro = '0'
013100010601     C*
013200010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
013300010601     C                   eval      flgGiro = '1'
013400000724     C*
013500000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
013600000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
013700010601     C     invfil        chain     azorg01l
013800000724     C                   if        %found
013900000724     C                   movel(p)  CMD(1)        depcmd
014000020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
014100000724     C*
014200000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
014300011113     C                   Z-ADD     150           LENGH            15 5
014400000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014500000724     C                   PARM                    depcmd
014600000724     C                   PARM                    LENGH
014700000724     C*
014800000724     C                   endif
014900000616     C*
015000000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
015100000616     C                   movel(p)  CMD(2)        depcmd
015200000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
015300000616     C*
015400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
015500011113     C                   Z-ADD     150           LENGH            15 5
015600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
015700000616     C                   PARM                    depcmd
015800000616     C                   PARM                    LENGH
015900000613     C*
016000100413     C                   if        not %open(titv1w6ps)
016100100413     C                   open      titv1w6ps
016200070329     C                   except    testdett
016300000613     C                   endif
016400010601     C*
016500010601     C                   endif
016600010601     C*
016700010601     C                   ENDSR
016800010601     C***
016900010601
017000010601
017100010601
017200010601
017300010601     C*--------------------------------------------------------
017400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
017500010601     C*--------------------------------------------------------
017600010601     C     ENDELA        BEGSR
017700990915     C*
017800000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
017900040119     C*
018000040119     C* Effettuo la chiamata al CLLE preposto
018100040119     C                   call(e)   'TITVVTC'
018200040510     C                   parm                    parccm
018300040119     C                   parm                    parmbr
018400040119     C                   parm      '2'           paropz
018500050201     C*
018600050201     C* Effettuo lancio TISI95 solo x chiusura
018700050201     C                   CLEAR                   TISI95DS
018800050201     C                   EVAL      I95TLA = 'C'
018900050201     C                   CALL      'TISI95R'
019000050201     C                   PARM                    TISI95DS
019100000613     C*
019200100413     C                   if        %open(titv1w6ps)
019300000616     C                   except    findett
019400100413     C                   close     titv1w6ps
019500000613     C                   endif
019600000616     C*
019700000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
019800011113     C                   Z-ADD     150           LENGH            15 5
019900010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
020000000616     C                   PARM                    CMD(3)
020100000616     C                   PARM                    LENGH
020200000616     C*
020300010601     C                   ENDSR
020400010601     C***
020500010601
020600010601
020700010601
020800000613
020900000613     C*--------------------------------------------------------
021000000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
021100000613     C*--------------------------------------------------------
021200000621     C     STPR          BEGSR
021300000613     C*
021400100413     C                   if        not %open(titv1w6p)
021500100413     C   50              open      titv1w6p
021600990915     C                   endif
021700990915     C*
021800030715     C   50              except    riepilogo
021900990915     C*
022000100413     C                   if        %open(titv1w6p)
022100100413     C   50              close     titv1w6p
022200990914     C                   endif
022300000613     C*
022400000613     C                   ENDSR
022500000613     C***
022600000613
022700010601
022800010601
022900010330     C*--------------------------------------------------------
023000021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
023100010330     C*--------------------------------------------------------
023200010605     C     RWVAB         BEGSR
023300010330     C*
023400010330     C                   if        not %open(tivin00r)
023500010330     C                   open      tivin00r
023600010330     C                   endif
023700021025     C                   if        not %open(fivabwwr)
023800021025     C                   open      fivabwwr
023900010330     C                   endif
024000040119     C*
024100040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
024200040119     C                   exsr      prevat
024300040119     C*
024400040119     C                   if        chkcall = '0'
024500040119     C*
024600040119     C                   if        not %open(fivatwwr)
024700040119     C                   open      fivatwwr
024800040119     C                   endif
024900010330     C*
025000040119     C                   clear                   §CTROKVB          7 0
025100040119     C                   clear                   §CTROKVT          7 0
025200040119     C                   clear                   §CTRMO            7 0
025300040119     C                   clear                   §CTRNO            7 0
025400070823     C*
025500070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
025600070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
025700100318     C***                exsr      inzvar
025800100318     C***                exsr      impvab
025900010330     C*
026000030822     C                   do        *HIVAL
026100010330     C*
026200030822     C                   read      tivin00r                               70
026300010618     C*
026400010618     C* Dopo ogni lettura verifico se ci sono stati record OK
026500010618     C                   if        vinflg = '1'
026600010618     C                   eval      flgOk = '1'
026700010618     C                   endif
026800040510     C*
026900040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
027000040510     C                   z-add     *zeros        wLenVINDTA        4 0
027100040510     C                   z-add     *zeros        wFlgVINDTA        1 0
027200040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
027300040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
027400040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
027500040510     C                   z-add     1             wFlgVINDTA
027600040510     C                   leave
027700040510     C                   endif
027800040510     C                   enddo
027900010618     C*
028000040510     C                   if        vindta > *blanks AND
028100040510     C                             wFlgVINDTA = 1
028200010330     C                   add       1             rrnum
028300010330     C*
028400010601     C                   if        *in70 = *off and
028500010330     C                             (vinflg = *blanks
028600010330     C                              or vinflg = '0'
028700010330     C                              or vinflg = '2')
028800010330     C*
028900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
029000010711     C                   if        vinflg = *blanks or vinflg = '0'
029100010711     C                   clear                   vinmsg
029200010711     C                   endif
029300010601     C*
029400070925     C* Aggiungo un byte a blanks all'inizio d ogni record
029500070925     C                   eval      vindta  = ' ' + %trim(vindta)
029600070925     C*
029700080527     C                   exsr      inzvar
029800100318     C                   exsr      normCSV
029900010605     C                   exsr      impvab
030000010601     C*
030100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
030200010605     C                   eval      depfil = VABLNP
030300010601     C                   exsr      repfil
030400010601     C                   if        depfil = invfil
030500021025     C                   if        vlrpoi = 999
030600030822     C                   move(P)   invfil        VABFGS
030700021025     C                   else
030800030822     C                   move(P)   vlrpoi        VABFGS
030900021025     C                   endif
031000010601     C*
031100010601     C                   exsr      PREELA
031200010601     C*
031300010604     C* Ebbene...
031400010604     C*
031500040119     C  N31              add       1             §CTROKVB
031600040119     C   32              add       1             §CTRMO
031700040119     C   31              add       1             §CTRNO
031800080418     C*
031900030822     C                   if        wGiro = 2
032000080513     C* Scrivo sempre il VAT
032100080513     C  N31              exsr      wrivat
032200080527     C                   exsr      chkvab
032300080513     C                   endif
032400010604     C*
032500010604     C                   if        *in31 = *off and
032600010604     C                             *in32 = *off
032700010604     C                   eval      vinflg = '1'
032800010604     C                   else
032900010604     C                   eval      recko = vindta
033000010604     C                   except    rigadett
033100010604     C                   eval      vinflg = '2'
033200010604     C                   endif
033300010604     C                   endif
033400010601     C*
033500010604     C                   endif
033600010604     C*
033700010330     C                   else
033800010330     C                   eval      vinflg = '1'
033900010330     C                   endif
034000010601     C*
034100010601     C  N70              update    tivin000
034200010330     C*
034300030822     C  N70              enddo
034400040119     C*
034500080527     C* Verifico dati rimasti "in cannna"...
034600080527     C                   seton                                        65
034700080527     C                   exsr      chkvab
034800080527     C*
034900040119     C                   endif                                                  (endif - chkcall)
035000010601     C*
035100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
035200010601     C                   if        cntNonEl = *zeros or
035300010601     C                             flgMulti = '0'
035400010330     C* Se non ci sono record con errori ...
035500010601     C                   if        §ctrno = 0 and
035600010604     C                             §ctrmo = 0 and
035700010601     C                             flgStato <> '2'
035800010330     C* ... restituisco esito OK.
035900010330     C                   eval      wrkesito = '0'
036000010330     C                   else
036100040119     C                   if        §ctrokvb > 0
036200010330     C                   eval      wrkesito = '1'
036300010330     C                   else
036400010615     C                   if        flgOk = '0'
036500010615     C                   eval      wrkesito = '2'
036600010615     C                   else
036700010615     C                   eval      wrkesito = '6'
036800010615     C                   endif
036900010330     C                   endif
037000010330     C                   endif
037100010601     C                   else
037200010601     C                   eval      wrkesito = '9'
037300010601     C                   endif
037400010330     C*
037500010330     C                   if        %open(tivin00r)
037600010330     C                   close     tivin00r
037700010330     C                   endif
037800021025     C                   if        %open(fivabwwr)
037900021025     C                   close     fivabwwr
038000010330     C                   endif
038100040512     C                   if        %open(fivatwwr)
038200040512     C                   close     fivatwwr
038300040512     C                   endif
038400010601     C*
038500010601     C                   if        vlrpoi <> 999
038600010601     C                   eval      invfil = vlrpoi
038700010601     C                   endif
038800010330     C*
038900040119     C                   if        §ctrokvb > 0
039000010601     C                             and invfil > *zeros
039100010330     C                   exsr      invio
039200010330     C                   endif
039300010601     C*
039400010618     C                   if        flgGiro = '1'
039500010601     C                   exsr      endela
039600010618     C                   endif
039700010330     C*
039800010330     C                   ENDSR
039900010330     C***
040000080527
040100080527
040200080527
040300080527     C*----------------------------------------------------*
040400080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
040500080527     C*----------------------------------------------------*
040600100413     C     CHKVAB        BEGSR
040700080527     C*
040800080527     C* Scrivo il VAB solo a rottura d spedizione
040900080527     C                   if        depspe = *zeros
041000080527     C  N31              eval      fivabds_sav = fivabds
041100080527     C                   eval      depspe = vabNSP
041200080527     C                   else
041300080527     C                   if        depspe = vabNSP AND *in65 = *off
041400100413     C   51              add       VABNCL        sav_VABNCL
041500100413     C   51              add       VABPKB        sav_VABPKB
041600100413     C   51              add       VABVLB        sav_VABVLB
041700100413     C  N51              z-add     VABNCL        sav_VABNCL
041800100413     C  N51              z-add     VABPKB        sav_VABPKB
041900100413     C  N51              z-add     VABVLB        sav_VABVLB
042000080527     C                   else
042100080527     C  N31              eval      fivabds_ok = fivabds_sav
042200080527     C  N31              exsr      wrivab
042300080527     C  N31              eval      fivabds_sav = fivabds
042400080527     C                   eval      depspe = vabNSP
042500080527     C                   endif
042600080527     C                   endif
042700080527     C*
042800080527     C                   ENDSR
042900040119
043000040119
043100040119
043200040119     C*----------------------------------------------------*
043300040119     C*  SCARICAMENTO BUFFER RECORDS VAB
043400040119     C*----------------------------------------------------*
043500040510     C     WRIVAB        BEGSR
043600080513     C*
043700080513     C* Gestisco l'eventuale rottura x numero spedizione
043800080527     C                   if        VABNSP <> depspe or *in65 = *on
043900070301     C                   if        wVATNOT_E <> *blanks
044000070301     C                   eval      VABCTM = '7Q'
044100070301     C                   endif
044200100413     C                   write     fivab000
044300080513     C                   endif
044400040119     C*
044500040119     C                   ENDSR
044600040119
044700040119
044800040119
044900040119     C*----------------------------------------------------*
045000040119     C*  SCARICAMENTO BUFFER RECORDS VAT
045100040119     C*----------------------------------------------------*
045200040510     C     WRIVAT        BEGSR
045300060512     C*
045400060512     C* Inizializzo i campi chiave
045500060512     C                   eval      VATFGS = VABFGS
045600060512     C                   eval      VATAAS = VABAAS
045700060512     C                   eval      VATCCM = VABCCM
045800070823     C                   eval      VATNRS = VABNRS
045900060512     C                   eval      VATNSP = VABNSP
046000060512     C                   eval      VATLNP = VABLNP
046100040714     C*
046200040714     C* Solo se VATNOT valorizzato scarico il buffer
046300040714     C                   if        VATNOT <> *blanks
046400040119     C*
046500040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
046600040510     C                   if        VATTRC = *blanks
046700040510     C                   eval      VATTRC = 'E'
046800040510     C                   endif
046900040510     C                   write     FIVAT000
047000040119     C                   add       1             §CTROKVT
047100040714     C*
047200040714     C                   endif
047300060331     C*
047400060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
047500060331     C                   if        wVATNOT_A <> *blanks
047600060331     C                   eval      VATNOT = wVATNOT_A
047700060331     C                   eval      VATTRC = 'A'
047800060331     C                   write     FIVAT000
047900060331     C                   add       1             §CTROKVT
048000060331     C                   endif
048100060331     C*
048200060331     C                   if        wVATNOT_B <> *blanks
048300060331     C                   eval      VATNOT = wVATNOT_B
048400060331     C                   eval      VATTRC = 'B'
048500060331     C                   write     FIVAT000
048600060331     C                   add       1             §CTROKVT
048700060331     C                   endif
048800060331     C*
048900060331     C                   if        wVATNOT_E <> *blanks
049000060331     C                   eval      VATNOT = wVATNOT_E
049100060331     C                   eval      VATTRC = 'E'
049200060331     C                   write     FIVAT000
049300060331     C                   add       1             §CTROKVT
049400060331     C                   endif
049500040119     C*
049600040119     C                   ENDSR
049700040119
049800040119
049900040119
050000040119     C*----------------------------------------------------*
050100040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
050200040119     C*----------------------------------------------------*
050300040119     C     PREVAT        BEGSR
050400040119     C*
050500040119     C* Compongo il nome del membro da dare al FIVATWWR
050600040119     C                   eval      parmbr = vlrhdl
050700040119     C                   movel     'M'           parmbr
050800040510     C                   eval      parccm = vlrksc
050900040119     C                   eval      paropz = '1'
051000040119     C* Effettuo la chiamata al CLLE preposto
051100040119     C                   call(e)   'TITVVTC'
051200040119     C                   parm                    parccm
051300040119     C                   parm                    parmbr
051400040119     C                   parm                    paropz
051500040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
051600040119     C                   if        %error
051700040119     C                   movel     '1'           chkcall
051800040119     C                   else
051900040119     C                   movel     '0'           chkcall
052000040119     C                   endif
052100040119     C*
052200040119     C                   ENDSR
052300010601
052400010601
052500010601
052600010330     C*----------------------------------------------------*
052700030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
052800010330     C*----------------------------------------------------*
052900040119     C     INZVAR        BEGSR
053000080527     C*
053100080527     C                   setoff                                       65
053200010330     C*
053300030822     C* Inizializzo variabili di wrk
053400020204     C                   Z-ADD     *zeros        Num5_0            5 0
053500100705     C                   MOVEL     *blanks       wFlgCAS           1
053600060331     C                   MOVEL     *blanks       wVATNOT_A
053700060331     C                   MOVEL     *blanks       wVATNOT_B
053800060331     C                   MOVEL     *blanks       wVATNOT_E
053900030822     C*
054000030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
054100030822     C                   CLEAR                   SkSplitCSV
054200030822     C                   EVAL      i = 1
054300030822     C                   EVAL      posDa = *zeros
054400030822     C                   EVAL      posA  = *zeros
054500080527     C*
054600080527     C                   CLEAR                   FIVABDS
054700080527     C                   CLEAR                   FIVAT000
054800030822     C*
054900030822     C* Reimposto i valori di default
055000030822     C                   EXSR      DEFCAM
055100030822     C*
055200010330     C                   ENDSR
055300010330     C*----------------------------------------------------*
055400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
055500010330     C*----------------------------------------------------*
055600010330     C     DEFCAM        BEGSR
055700030822     C*
055800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
055900030715     C* e delimitatore testo.
056000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
056100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
056200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
056300100318     C*
056400100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
056500100318     C                   EVAL      CharSOS = CharNUM
056600030822     C*
056700030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
056800100413     C                   SETOFF                                       5051
056900100413     C                   SELECT
057000100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
057100030715     C                   SETON                                        50
057200100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
057300100413     C                   SETON                                        51
057400100413     C                   ENDSL
057500030822     C*
057600030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
057700030822     C                   EVAL      posDaDft = 1
057800030822     C                   EVAL      posADft  = 0
057900030822     C                   EVAL      wGiroDft = 0
058000030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
058100030822     C                             posDaDft > 0
058200030822     C*
058300030822     C* Gestisco il 1° giro
058400030822     C                   IF        wGiroDft = 0
058500030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
058600030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
058700030822     C* Incremento il contatore dei "giri"
058800030822     C                   EVAL      wGiroDft = 1
058900030822     C                   ELSE
059000030822     C                   EVAL      posDaDft = posADft
059100030822     C                   ENDIF
059200030822     C* Eseguo lo scan x trovare la fine del campo corrente
059300030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
059400030822     C*
059500030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
059600030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
059700030822     C                   IF        posDaDft > 0 AND
059800030822     C                             posADft  > 0
059900030822     C* NCL
060000030822     C                   IF        %subst(
060100030822     C                             %subst(vlrppt:posDaDft+1:
060200030822     C                             posADft-posDaDft-1):1:3)
060300030822     C                             = 'NCL'
060400030822     C                   EVAL      PiStr=%trim(%subst(
060500030822     C                             %subst(vlrppt:posDaDft+1:
060600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060700030822     C                   EXSR      CHKNUM
060800030822     C                   IF        PiInt=*on
060900030822     C                   Z-ADD     PiVal         VABNCL
061000030822     C                   ENDIF
061100030822     C                   ENDIF
061200030822     C* CCM
061300030822     C                   IF        %subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):1:3)
061600030822     C                             = 'CCM'
061700030822     C                   EVAL      PiStr=%trim(%subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062000030822     C                   EXSR      CHKNUM
062100030822     C                   IF        PiInt=*on
062200030822     C                   Z-ADD     PiVal         VABCCM
062300030822     C                   ENDIF
062400030822     C                   ENDIF
062500030822     C* LNP
062600030822     C                   IF        %subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):1:3)
062900030822     C                             = 'LNP'
063000030822     C                   EVAL      PiStr=%trim(%subst(
063100030822     C                             %subst(vlrppt:posDaDft+1:
063200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063300030822     C                   EXSR      CHKNUM
063400030822     C                   IF        PiInt=*on
063500030822     C                   Z-ADD     PiVal         VABLNP
063600030822     C                   ENDIF
063700030822     C                   ENDIF
063800030822     C* NRS
063900030822     C                   IF        %subst(
064000030822     C                             %subst(vlrppt:posDaDft+1:
064100030822     C                             posADft-posDaDft-1):1:3)
064200030822     C                             = 'NRS'
064300030822     C                   EVAL      PiStr=%trim(%subst(
064400030822     C                             %subst(vlrppt:posDaDft+1:
064500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064600030822     C                   EXSR      CHKNUM
064700030822     C                   IF        PiInt=*on
064800030822     C                   Z-ADD     PiVal         VABNRS
064900030822     C                   ENDIF
065000030822     C                   ENDIF
065100030822     C* CTR
065200030822     C                   IF        %subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):1:3)
065500030822     C                             = 'CTR'
065600030822     C                   EVAL      PiStr=%trim(%subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065900030822     C                   EXSR      CHKNUM
066000030822     C                   IF        PiInt=*on
066100030822     C                   Z-ADD     PiVal         VABCTR
066200030822     C                   ENDIF
066300030822     C                   ENDIF
066400030822     C* PKB
066500030822     C                   IF        %subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):1:3)
066800030822     C                             = 'PKB'
066900030822     C                   EVAL      PiStr=%trim(%subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067200030822     C                   EXSR      CHKNUM
067300030822     C                   IF        PiNum=*on
067400030822     C                   Z-ADD     PiVal         VABPKB
067500030822     C                   ENDIF
067600030822     C                   ENDIF
067700030822     C* VLB
067800030822     C                   IF        %subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):1:3)
068100030822     C                             = 'VLB'
068200030822     C                   EVAL      PiStr=%trim(%subst(
068300030822     C                             %subst(vlrppt:posDaDft+1:
068400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068500030822     C                   EXSR      CHKNUM
068600030822     C                   IF        PiNum=*on
068700030822     C                   Z-ADD     PiVal         VABVLB
068800030822     C                   ENDIF
068900030822     C                   ENDIF
069000030822     C* QFT
069100030822     C                   IF        %subst(
069200030822     C                             %subst(vlrppt:posDaDft+1:
069300030822     C                             posADft-posDaDft-1):1:3)
069400030822     C                             = 'QFT'
069500030822     C                   EVAL      PiStr=%trim(%subst(
069600030822     C                             %subst(vlrppt:posDaDft+1:
069700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069800030822     C                   EXSR      CHKNUM
069900030822     C                   IF        PiNum=*on
070000030822     C                   Z-ADD     PiVal         VABQFT
070100030822     C                   ENDIF
070200030822     C                   ENDIF
070300030822     C* CBO
070400030822     C                   IF        %subst(
070500030822     C                             %subst(vlrppt:posDaDft+1:
070600030822     C                             posADft-posDaDft-1):1:3)
070700030822     C                             = 'CBO'
070800030822     C                   EVAL      VABCBO=%trim(%subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071100030822     C                   ENDIF
071200030822     C* TSP
071300030822     C                   IF        %subst(
071400030822     C                             %subst(vlrppt:posDaDft+1:
071500030822     C                             posADft-posDaDft-1):1:3)
071600030822     C                             = 'TSP'
071700030822     C                   EVAL      VABTSP=%trim(%subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072000030822     C                   ENDIF
072100030822     C* VAS
072200030822     C                   IF        %subst(
072300030822     C                             %subst(vlrppt:posDaDft+1:
072400030822     C                             posADft-posDaDft-1):1:3)
072500030822     C                             = 'VAS'
072600030822     C                   EVAL      VABVAS=%trim(%subst(
072700030822     C                             %subst(vlrppt:posDaDft+1:
072800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072900030822     C                   ENDIF
073000030822     C* VCA
073100030822     C                   IF        %subst(
073200030822     C                             %subst(vlrppt:posDaDft+1:
073300030822     C                             posADft-posDaDft-1):1:3)
073400030822     C                             = 'VCA'
073500030822     C                   EVAL      VABVCA=%trim(%subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073800030822     C                   ENDIF
073900070925     C* TIC
074000070925     C                   IF        %subst(
074100070925     C                             %subst(vlrppt:posDaDft+1:
074200070925     C                             posADft-posDaDft-1):1:3)
074300070925     C                             = 'TIC'
074400070925     C                   EVAL      VABTIC=%trim(%subst(
074500070925     C                             %subst(vlrppt:posDaDft+1:
074600070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074700070925     C                   ENDIF
074800030822     C* GCA
074900030822     C                   IF        %subst(
075000030822     C                             %subst(vlrppt:posDaDft+1:
075100030822     C                             posADft-posDaDft-1):1:3)
075200030822     C                             = 'GCA'
075300030822     C                   EVAL      VABGCA=%trim(%subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075600030822     C                   ENDIF
075700030822     C* CTM
075800030822     C                   IF        %subst(
075900030822     C                             %subst(vlrppt:posDaDft+1:
076000030822     C                             posADft-posDaDft-1):1:3)
076100030822     C                             = 'CTM'
076200030822     C                   EVAL      VABCTM=%trim(%subst(
076300030822     C                             %subst(vlrppt:posDaDft+1:
076400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076500030822     C                   ENDIF
076600030822     C* FFD
076700030822     C                   IF        %subst(
076800030822     C                             %subst(vlrppt:posDaDft+1:
076900030822     C                             posADft-posDaDft-1):1:3)
077000030822     C                             = 'FFD'
077100030822     C                   EVAL      VABFFD=%trim(%subst(
077200030822     C                             %subst(vlrppt:posDaDft+1:
077300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077400030822     C                   ENDIF
077500030822     C* VAD
077600030822     C                   IF        %subst(
077700030822     C                             %subst(vlrppt:posDaDft+1:
077800030822     C                             posADft-posDaDft-1):1:3)
077900030822     C                             = 'VAD'
078000030822     C                   EVAL      VABVAD=%trim(%subst(
078100030822     C                             %subst(vlrppt:posDaDft+1:
078200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078300030822     C                   ENDIF
078400030822     C* GMA
078500030822     C                   IF        %subst(
078600030822     C                             %subst(vlrppt:posDaDft+1:
078700030822     C                             posADft-posDaDft-1):1:3)
078800030822     C                             = 'GMA'
078900030822     C                   EVAL      VABGMA=%trim(%subst(
079000030822     C                             %subst(vlrppt:posDaDft+1:
079100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079200030822     C                   ENDIF
079300030822     C* GGA
079400030822     C                   IF        %subst(
079500030822     C                             %subst(vlrppt:posDaDft+1:
079600030822     C                             posADft-posDaDft-1):1:3)
079700030822     C                             = 'GGA'
079800030822     C                   EVAL      VABGGA=%trim(%subst(
079900030822     C                             %subst(vlrppt:posDaDft+1:
080000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080100030822     C                   ENDIF
080200030822     C* GVA
080300030822     C                   IF        %subst(
080400030822     C                             %subst(vlrppt:posDaDft+1:
080500030822     C                             posADft-posDaDft-1):1:3)
080600030822     C                             = 'GVA'
080700030822     C                   EVAL      VABGVA=%trim(%subst(
080800030822     C                             %subst(vlrppt:posDaDft+1:
080900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081000030822     C                   ENDIF
081100030822     C* TC1
081200030822     C                   IF        %subst(
081300030822     C                             %subst(vlrppt:posDaDft+1:
081400030822     C                             posADft-posDaDft-1):1:3)
081500030822     C                             = 'TC1'
081600030822     C                   EVAL      VABTC1=%trim(%subst(
081700030822     C                             %subst(vlrppt:posDaDft+1:
081800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081900030822     C                   ENDIF
082000030822     C* TC2
082100030822     C                   IF        %subst(
082200030822     C                             %subst(vlrppt:posDaDft+1:
082300030822     C                             posADft-posDaDft-1):1:3)
082400030822     C                             = 'TC2'
082500030822     C                   EVAL      VABTC2=%trim(%subst(
082600030822     C                             %subst(vlrppt:posDaDft+1:
082700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082800030822     C                   ENDIF
082900040714     C* VATTRC
083000040714     C                   IF        %subst(
083100040714     C                             %subst(vlrppt:posDaDft+1:
083200040714     C                             posADft-posDaDft-1):1:3)
083300040714     C                             = 'TRC'
083400040714     C                   EVAL      VATTRC=%trim(%subst(
083500040714     C                             %subst(vlrppt:posDaDft+1:
083600040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083700040714     C                   ENDIF
083800030822     C* ...
083900030822     C                   ENDIF
084000030822     C                   ENDDO
084100020204     C*
084200010330     C                   ENDSR
084300010607     C*----------------------------------------------------*
084400021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
084500010607     C*----------------------------------------------------*
084600010607     C     IMPVAB        BEGSR
084700010607     C*
084800010607     C                   SETOFF                                       3132
084900030515     C*
085000010607     C                   MOVEL     datcor        VABAAS
085100010607     C                   MOVE      datcor        VABMGS
085200010607     C*
085300030715     C**********
085400030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
085500030715     C**********
085600030715     C                   IF        wGiro = *zeros
085700040113     C*
085800040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
085900040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
086000080429     C                             %trim(intcol(3))+%trim(intcol(4))+
086100080429     C                             %trim(intcol(5))<>*blanks
086200040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
086300080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
086400080429     C                                      %trim(intcol(5))
086500070823     C     ';':CharCSV   XLATE     vindta        vindta
086600040510     C                   ENDIF
086700031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
086800031201     C     minu:maiu     XLATE     vindta        vindta
086900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
087000030715     C                   DOW       posDa <= %len(%trim(vindta))
087100030715     C*
087200030715     C* Gestisco il 1° campo
087300030715     C                   IF        i = 1
087400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
087500030715     C                   EVAL      posDa = 1
087600030715     C* Eseguo lo scan x trovare la fine del primo campo
087700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087800030715     C* A questo punto "estraggo" il campo corrente
087900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088000030715     C                                                    (posA-posDa))
088100030715     C* X i campi successivi al 1°
088200030715     C                   ELSE
088300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
088400030715     C                   EVAL      posDa = posA + 1
088500030715     C* Eseguo lo scan x trovare la fine del campo corrente
088600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088700030715     C* Gestisco l'ultimo campo
088800030715     C                   IF        posA = *zeros
088900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
089000030715     C* A questo punto "estraggo" il campo corrente
089100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089200030715     C                                                    (posA-posDa))
089300030715     C                   LEAVE
089400030715     C                   ELSE
089500030715     C* A questo punto "estraggo" il campo corrente
089600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089700030715     C                                                    (posA-posDa))
089800030715     C                   ENDIF
089900030715     C                   ENDIF
090000030715     C* Incremento il contatore d campo
090100030715     C                   EVAL      i = i +1
090200030715     C                   ENDDO
090300030715     C*
090400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
090500030715     C* delimitazione testo
090600030715     C                   EVAL      i = 1
090700030715     C                   DOW       i < %elem(SkSplitFLD)
090800030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
090900030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
091000030715     C                   EVAL      i = i + 1
091100030715     C                   ENDDO
091200030715     C                   EVAL      wGiro = 1
091300030715     C                   ELSE
091400030715     C                   EVAL      wGiro = 2
091500031204     C**********
091600031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
091700031204     C* d separatore campo
091800031204     C**********
091900031204     C                   EVAL      vindta = %trim(vindta)
092000031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
092100031204     C                   EVAL      vindta = %subst(vindta:2)
092200031204     C                   ENDDO
092300031204     C*
092400031204     C                   Z-ADD     *zeros        lunghInput        4 0
092500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
092600031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
092700031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
092800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
092900031204     C                   ENDDO
093000030715     C**********
093100030715     C* Effettuo lo split del campo dati d input
093200030715     C**********
093300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
093400030715     C                   DOW       posDa <= %len(%trim(vindta))
093500030715     C*
093600030715     C* Gestisco il 1° campo
093700030715     C                   IF        i = 1
093800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
093900030715     C                   EVAL      posDa = 1
094000030715     C* Eseguo lo scan x trovare la fine del primo campo
094100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094200030715     C* A questo punto "estraggo" il campo corrente
094300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094400030715     C                                                    (posA-posDa))
094500030715     C* X i campi successivi al 1°
094600030715     C                   ELSE
094700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
094800030715     C                   EVAL      posDa = posA + 1
094900031204     C* Verifico che nn vi sia il campo nn valorizzato
095000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
095100031204     C* Se campo nn valorizzato skippo al prossimo
095200031204     C                   EVAL      SkSplitCSV(i) = *blanks
095300031204     C                   EVAL      posA  = posA + 1
095400031204     C                   ELSE
095500030715     C* Eseguo lo scan x trovare la fine del campo corrente
095600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
095700030715     C* Gestisco l'ultimo campo
095800030715     C                   IF        posA = *zeros
095900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
096000030715     C* A questo punto "estraggo" il campo corrente
096100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096200030715     C                                                    (posA-posDa))
096300030715     C                   LEAVE
096400030715     C                   ELSE
096500030715     C* A questo punto "estraggo" il campo corrente
096600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096700030715     C                                                    (posA-posDa))
096800030715     C                   ENDIF
096900031204     C                   ENDIF
097000030715     C                   ENDIF
097100030715     C* Incremento il contatore d campo
097200030715     C                   EVAL      i = i +1
097300030715     C                   ENDDO
097400030715     C*
097500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
097600030715     C* delimitazione testo
097700030715     C                   EVAL      i = 1
097800030715     C                   DOW       i < %elem(SkSplitCSV)
097900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
098000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
098100030715     C                   EVAL      i = i + 1
098200030715     C                   ENDDO
098300100326     C*
098400100326     C* Stacco un progressivo x numero spedizione
098500100705     C***                EXSR      repNSP
098600100326     C*
098700030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
098800030715     C                   EVAL      i = 1
098900030715     C                   DOW       i < %elem(SkSplitFLD)
099000030715     C*
099100030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
099200030715     C                   IF        SkSplitFLD(i) <> *blanks AND
099300030715     C                             SkSplitCSV(i) <> *blanks
099400030721     C*** ATB
099500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
099600030721     C                   EVAL      VABATB = SkSplitCSV(i)
099700030721     C                   ENDIF
099800030715     C*** RSD
099900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
100000030715     C                   EVAL      VABRSD = SkSplitCSV(i)
100100030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
100200030715     C     '@':'A'       XLATE     VABRSD        VABRSD
100300030715     C* ==
100400030715     C                   ENDIF
100500030721     C*** RD2
100600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
100700030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
100800030721     C                   ENDIF
100900030715     C*** IND
101000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
101100030715     C                   EVAL      VABIND = SkSplitCSV(i)
101200030715     C                   ENDIF
101300030715     C*** LOD
101400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
101500030715     C                   EVAL      VABLOD = SkSplitCSV(i)
101600030715     C                   ENDIF
101700030715     C*** PRD
101800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
101900030715     C                   EVAL      VABPRD = SkSplitCSV(i)
102000030715     C                   ENDIF
102100030721     C*** NZD
102200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
102300030721     C                   EVAL      VABNZD = SkSplitCSV(i)
102400030721     C                   ENDIF
102500030715     C*** RMA
102600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
102700030715     C                   EVAL      VABRMA = SkSplitCSV(i)
102800030715     C                   ENDIF
102900030715     C*** NOT
103000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
103100030715     C                   EVAL      VABNOT = SkSplitCSV(i)
103200030715     C                   ENDIF
103300030715     C*** NT2
103400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
103500030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
103600030715     C                   ENDIF
103700030715     C*** VCA
103800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
103900030715     C                   EVAL      VABVCA = SkSplitCSV(i)
104000030715     C                   ENDIF
104100030715     C*** VAS
104200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
104300030715     C                   EVAL      VABVAS = SkSplitCSV(i)
104400030715     C                   ENDIF
104500030715     C*** FFD
104600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
104700030715     C                   EVAL      VABFFD = SkSplitCSV(i)
104800030715     C                   ENDIF
104900030721     C*** GC1
105000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
105100030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
105200030721     C                   ENDIF
105300030721     C*** GC2
105400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
105500030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
105600030721     C                   ENDIF
105700030721     C*** TSP
105800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
105900030721     C                   EVAL      VABTSP = SkSplitCSV(i)
106000030721     C                   ENDIF
106100040510     C*** CBO
106200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
106300040510     C                   EVAL      VABCBO = SkSplitCSV(i)
106400040510     C                   ENDIF
106500030721     C*** NAS
106600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
106700030721     C                   EVAL      VABNAS = SkSplitCSV(i)
106800030721     C                   ENDIF
106900030721     C*** TIC
107000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
107100030721     C                   EVAL      VABTIC = SkSplitCSV(i)
107200030721     C                   ENDIF
107300030721     C*** GCA
107400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
107500030721     C                   EVAL      VABGCA = SkSplitCSV(i)
107600030721     C                   ENDIF
107700030721     C*** XCO
107800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
107900030721     C                   EVAL      VABXCO = SkSplitCSV(i)
108000030721     C                   ENDIF
108100030721     C*** CTM
108200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
108300030721     C                   EVAL      VABCTM = SkSplitCSV(i)
108400030721     C                   ENDIF
108500030721     C*** TCR
108600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
108700030721     C                   EVAL      VABTCR = SkSplitCSV(i)
108800030721     C                   ENDIF
108900030721     C*** CTS
109000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
109100030721     C                   EVAL      VABCTS = SkSplitCSV(i)
109200030721     C                   ENDIF
109300030721     C*** FTM
109400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
109500030721     C                   EVAL      VABFTM = SkSplitCSV(i)
109600030721     C                   ENDIF
109700030721     C*** VAD
109800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
109900030721     C                   EVAL      VABVAD = SkSplitCSV(i)
110000030721     C                   ENDIF
110100030721     C*** GMA
110200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
110300030721     C                   EVAL      VABGMA = SkSplitCSV(i)
110400030721     C                   ENDIF
110500030721     C*** GGA
110600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
110700030721     C                   EVAL      VABGGA = SkSplitCSV(i)
110800030721     C                   ENDIF
110900030721     C*** GVA
111000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
111100030721     C                   EVAL      VABGVA = SkSplitCSV(i)
111200030721     C                   ENDIF
111300030721     C*** TC1
111400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
111500030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
111600030721     C                   ENDIF
111700030721     C*** TC2
111800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
111900030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
112000030721     C                   ENDIF
112100030721     C*** SCL
112200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
112300030721     C                   EVAL      VABSCL = SkSplitCSV(i)
112400030721     C                   ENDIF
112500030721     C*** RMO
112600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
112700030721     C                   EVAL      VABRMO = SkSplitCSV(i)
112800030721     C                   ENDIF
112900030721     C*** NMO
113000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
113100030721     C                   EVAL      VABNMO = SkSplitCSV(i)
113200030721     C                   ENDIF
113300030721     C*
113400030721     C* Reperisco quindi i campi numerici...
113500040510     C*** NSP
113600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
113700040510     C                   EVAL      PiStr=SkSplitCSV(i)
113800040510     C                   EXSR      CHKNUM
113900040510     C                   IF        PiInt=*on
114000040510     C                   Z-ADD     PiVal         VABNSP
114100040510     C                   ELSE
114200040510     C                   SETON                                        31
114300040510     C                   EVAL      VABNSP = *zeros
114400040510     C                   EVAL      vinmsg = %trimr(vinmsg)
114500040510     C                             + ' ' + 'VABNSP'
114600040510     C                   ENDIF
114700040510     C                   ENDIF
114800071106     C*** RMN
114900071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
115000071106     C                   EVAL      PiStr=SkSplitCSV(i)
115100071106     C                   EXSR      CHKNUM
115200071106     C                   IF        PiInt=*on
115300071106     C                   Z-ADD     PiVal         VABRMN
115400071106     C                   ELSE
115500071106     C                   SETON                                        32
115600071106     C                   EVAL      VABRMN = *zeros
115700071106     C                   EVAL      vinmsg = %trimr(vinmsg)
115800071106     C                             + ' ' + 'VABRMN'
115900071106     C                   ENDIF
116000071106     C                   ENDIF
116100030721     C*** CAD
116200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
116300030721     C                   EVAL      PiStr=SkSplitCSV(i)
116400030721     C                   EXSR      CHKNUM
116500030721     C                   IF        PiInt=*on
116600030721     C                   Z-ADD     PiVal         Num5_0
116700030721     C                   MOVEL(p)  Num5_0        VABCAD
116800030721     C                   ELSE
116900030721     C                   SETON                                        32
117000030721     C                   EVAL      VABCAD = *zeros
117100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117200030721     C                             + ' ' + 'VABCAD'
117300030721     C                   ENDIF
117400030721     C                   ENDIF
117500030721     C*** NCL
117600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
117700030721     C                   EVAL      PiStr=SkSplitCSV(i)
117800030721     C                   EXSR      CHKNUM
117900030721     C                   IF        PiInt=*on
118000030721     C                   Z-ADD     PiVal         VABNCL
118100030721     C                   ELSE
118200030721     C                   SETON                                        32
118300030721     C                   EVAL      VABNCL = *zeros
118400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118500040122     C                             + ' ' + 'VABNCL'
118600030721     C                   ENDIF
118700030721     C                   ENDIF
118800030721     C*** PKB
118900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
119000030721     C                   EVAL      PiStr=SkSplitCSV(i)
119100030721     C                   EXSR      CHKNUM
119200030721     C                   IF        PiNum=*on
119300030721     C                   Z-ADD     PiVal         VABPKB
119400030721     C                   ELSE
119500030721     C                   SETON                                        32
119600030721     C                   EVAL      VABPKB = *zeros
119700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119800030721     C                             + ' ' + 'VABPKB'
119900030721     C                   ENDIF
120000030721     C                   ENDIF
120100030721     C*** CAS
120200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
120300030721     C                   EVAL      PiStr=SkSplitCSV(i)
120400100705     C                   IF        PiStr<>*blanks
120500100705     C                   EVAL      wFlgCAS = '1'
120600100705     C                   EVAL      VABVCA='EUR'
120700100705     C                   ENDIF
120800030721     C                   EXSR      CHKNUM
120900030721     C                   IF        PiNum=*on
121000030721     C                   Z-ADD     PiVal         VABCAS
121100030721     C                   ELSE
121200030721     C                   SETON                                        32
121300030721     C                   EVAL      VABCAS = *zeros
121400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121500030721     C                             + ' ' + 'VABCAS'
121600030721     C                   ENDIF
121700030721     C                   ENDIF
121800030721     C*** CCM
121900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
122000030721     C                   EVAL      PiStr=SkSplitCSV(i)
122100030721     C                   EXSR      CHKNUM
122200030721     C                   IF        PiInt=*on      AND
122300030721     C                             PiVal<=9999999 AND
122400030721     C                             PiVal>*zeros
122500030721     C                   Z-ADD     PiVal         VABCCM
122600030721     C                   ELSE
122700030721     C                   SETON                                        32
122800030721     C                   EVAL      VABCCM = *zeros
122900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123000040510     C                             + ' ' + 'VABCCM'
123100030721     C                   ENDIF
123200030721     C                   ENDIF
123300030721     C*** LNP
123400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
123500030721     C                   EVAL      PiStr=SkSplitCSV(i)
123600030721     C                   EXSR      CHKNUM
123700030721     C                   IF        PiInt=*on    AND
123800030721     C                             PiVal<=999   AND
123900030721     C                             PiVal>*zeros
124000030721     C                   Z-ADD     PiVal         VABLNP
124100030721     C                   ELSE
124200040715     C                   SETON                                        32
124300030721     C                   EVAL      VABLNP = *zeros
124400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124500040510     C                             + ' ' + 'VABLNP'
124600030721     C                   ENDIF
124700030721     C                   ENDIF
124800030721     C*** NRS
124900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
125000030721     C                   EVAL      PiStr=SkSplitCSV(i)
125100030721     C                   EXSR      CHKNUM
125200030721     C                   IF        PiInt=*on AND
125300030721     C                             PiVal<=99
125400030721     C                   Z-ADD     PiVal         VABNRS
125500030721     C                   ELSE
125600030721     C                   SETON                                        32
125700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125800030721     C                             + ' ' + 'VABNRS'
125900030721     C                   ENDIF
126000030721     C                   ENDIF
126100030721     C*** LNA
126200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
126300030721     C                   EVAL      PiStr=SkSplitCSV(i)
126400030721     C                   EXSR      CHKNUM
126500030721     C                   IF        PiInt=*on    AND
126600030721     C                             PiVal<=999   AND
126700030721     C                             PiVal>*zeros
126800030721     C                   Z-ADD     PiVal         VABLNA
126900030721     C                   ELSE
127000030721     C                   SETON                                        32
127100030721     C                   EVAL      VABLNA = *zeros
127200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127300030721     C                             + ' ' + 'VABLNA'
127400030721     C                   ENDIF
127500030721     C                   ENDIF
127600030721     C*** CTR
127700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
127800030721     C                   EVAL      PiStr=SkSplitCSV(i)
127900030721     C                   EXSR      CHKNUM
128000030721     C                   IF        PiInt=*on  AND
128100030721     C                             PiVal<=999
128200030721     C                   Z-ADD     PiVal         VABCTR
128300030721     C                   ELSE
128400030721     C                   SETON                                        32
128500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128600030721     C                             + ' ' + 'VABCTR'
128700030721     C                   ENDIF
128800030721     C                   ENDIF
128900030721     C*** IAS
129000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
129100030721     C                   EVAL      PiStr=SkSplitCSV(i)
129200030721     C                   EXSR      CHKNUM
129300030721     C                   IF        PiNum=*on
129400030721     C                   Z-ADD     PiVal         VABIAS
129500030721     C                   ELSE
129600030721     C                   SETON                                        32
129700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129800030721     C                             + ' ' + 'VABIAS'
129900030721     C                   ENDIF
130000030721     C                   ENDIF
130100030721     C*** VLB
130200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
130300030721     C                   EVAL      PiStr=SkSplitCSV(i)
130400030721     C                   EXSR      CHKNUM
130500030721     C                   IF        PiNum=*on
130600030721     C                   Z-ADD     PiVal         VABVLB
130700030721     C                   ELSE
130800030721     C                   SETON                                        32
130900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131000030721     C                             + ' ' + 'VABVLB'
131100030721     C                   ENDIF
131200030721     C                   ENDIF
131300030721     C*** QFT
131400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
131500030721     C                   EVAL      PiStr=SkSplitCSV(i)
131600030721     C                   EXSR      CHKNUM
131700030721     C                   IF        PiNum=*on
131800030721     C                   Z-ADD     PiVal         VABQFT
131900030721     C                   ELSE
132000030721     C                   SETON                                        32
132100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132200030721     C                             + ' ' + 'VABQFT'
132300030721     C                   ENDIF
132400030721     C                   ENDIF
132500030721     C*** NCD
132600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
132700030721     C                   EVAL      PiStr=SkSplitCSV(i)
132800030721     C                   EXSR      CHKNUM
132900030721     C                   IF        PiInt=*on      AND
133000030721     C                             PiVal<=9999999
133100030721     C                   Z-ADD     PiVal         VABNCD
133200030721     C                   ELSE
133300030721     C                   SETON                                        32
133400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133500030721     C                             + ' ' + 'VABNCD'
133600030721     C                   ENDIF
133700030721     C                   ENDIF
133800030721     C*** NCA
133900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
134000030721     C                   EVAL      PiStr=SkSplitCSV(i)
134100030721     C                   EXSR      CHKNUM
134200030721     C                   IF        PiInt=*on      AND
134300030721     C                             PiVal<=9999999
134400030721     C                   Z-ADD     PiVal         VABNCA
134500030721     C                   ELSE
134600030721     C                   SETON                                        32
134700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134800030721     C                             + ' ' + 'VABNCA'
134900030721     C                   ENDIF
135000030721     C                   ENDIF
135100030721     C*** ZNC
135200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
135300030721     C                   EVAL      PiStr=SkSplitCSV(i)
135400030721     C                   EXSR      CHKNUM
135500030721     C                   IF        PiInt=*on AND
135600030721     C                             PiVal<=99
135700030721     C                   Z-ADD     PiVal         VABZNC
135800030721     C                   ELSE
135900030721     C                   SETON                                        32
136000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136100030721     C                             + ' ' + 'VABZNC'
136200030721     C                   ENDIF
136300030721     C                   ENDIF
136400030721     C*** DCR
136500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
136600030721     C                   EVAL      PiStr=SkSplitCSV(i)
136700030721     C                   EXSR      CHKNUM
136800030721     C                   IF        PiInt=*on       AND
136900030721     C                             PiVal<=99999999
137000030721     C                   Z-ADD     PiVal         VABDCR
137100030721     C                   ELSE
137200030721     C                   SETON                                        32
137300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137400030721     C                             + ' ' + 'VABDCR'
137500030721     C                   ENDIF
137600030721     C                   ENDIF
137700030721     C*** HCR
137800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
137900030721     C                   EVAL      PiStr=SkSplitCSV(i)
138000030721     C                   EXSR      CHKNUM
138100030721     C                   IF        PiInt=*on   AND
138200030721     C                             PiVal<=9999
138300030721     C                   Z-ADD     PiVal         VABHCR
138400030721     C                   ELSE
138500030721     C                   SETON                                        32
138600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138700030721     C                             + ' ' + 'VABHCR'
138800030721     C                   ENDIF
138900030721     C                   ENDIF
139000030721     C*** VMD
139100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
139200030721     C                   EVAL      PiStr=SkSplitCSV(i)
139300030721     C                   EXSR      CHKNUM
139400030721     C                   IF        PiNum=*on
139500030721     C                   Z-ADD     PiVal         VABVMD
139600030721     C                   ELSE
139700030721     C                   SETON                                        32
139800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139900030721     C                             + ' ' + 'VABVMD'
140000030721     C                   ENDIF
140100030721     C                   ENDIF
140200030721     C*** ANT
140300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
140400030721     C                   EVAL      PiStr=SkSplitCSV(i)
140500030721     C                   EXSR      CHKNUM
140600030721     C                   IF        PiInt=*on        AND
140700030721     C                             PiVal<=999999999
140800030721     C                   Z-ADD     PiVal         VABANT
140900030721     C                   ELSE
141000030721     C                   SETON                                        32
141100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141200030721     C                             + ' ' + 'VABANT'
141300030721     C                   ENDIF
141400030721     C                   ENDIF
141500030721     C*** CMO
141600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
141700030721     C                   EVAL      PiStr=SkSplitCSV(i)
141800030721     C                   EXSR      CHKNUM
141900030721     C                   IF        PiInt=*on
142000030721     C                   Z-ADD     PiVal         Num5_0
142100030721     C                   MOVEL(p)  Num5_0        VABCMO
142200030721     C                   ELSE
142300030721     C                   SETON                                        32
142400030721     C                   EVAL      VABCMO = *zeros
142500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142600030721     C                             + ' ' + 'VABCMO'
142700030721     C                   ENDIF
142800030721     C                   ENDIF
142900040510     C*
143000040510     C* Reperisco i campi del FIVAT
143100040510     C*** VATNOT
143200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
143300040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
143400040510     C                   ENDIF
143500040510     C*** VATTRC
143600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
143700040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
143800040510     C                   ENDIF
143900060331     C*** VATNOT_A
144000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
144100060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
144200060331     C                   ENDIF
144300060331     C*** VATNOT_B
144400060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
144500060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
144600060331     C                   ENDIF
144700060331     C*** VATNOT_E
144800070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
144900060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
145000060331     C                   ENDIF
145100070823     C*
145200071029     C***  ===>  Gestione campi "particolari" dentro ciclo
145300100413     C*** NZD/CTM/CTR
145400100413     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNZD'
145500100413     C                   EVAL      VABNZD=SkSplitCSV(i)
145600100705     C*
145700100705     C* ...passo dal codice ISO a quello "Bartolini"
145800100705     C                   Z-ADD     1             jNAZ
145900100705     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         55
146000100705     C                   IF        %found
146100100705     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
146200100705     C                   ENDIF
146300100705     C*
146400100413     C                   IF        VABNZD<> *blanks
146500100413     C                   EVAL      VABCTM='7R'
146600100413     C                   EVAL      VABCTR=352
146700100413     C                   ELSE
146800100413     C                   EVAL      VABCTM='7Q'
146900120918     C***                EVAL      VABCTR=000
147000120918     C                   EVAL      VABCTR=001
147100100413     C                   ENDIF
147200100326     C                   ENDIF
147300110802     C*** NSP / RMN / RMA
147400100705     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
147500110802     C                   EVAL      VABRMA=SkSplitCSV(i)
147600100705     C                   EVAL      PiStr=SkSplitCSV(i)
147700100705     C                   EXSR      CHKNUM
147800100705     C                   IF        PiInt=*on
147900100705     C                   Z-ADD     PiVal         VABRMN
148000100705     C                   Z-ADD     PiVal         VABNSP
148100100705     C                   ELSE
148200100705     C                   SETON                                        31
148300100705     C                   EVAL      VABRMN = *zeros
148400100705     C                   EVAL      vinmsg = %trimr(vinmsg)
148500110802     C                             + ' ' + 'VABRMN VABNSP VATNSP'
148600100705     C                   ENDIF
148700100705     C                   ENDIF
148800070925     C*
148900071029     C***  <===  -----------------------------------------
149000040510     C*
149100030715     C                   ENDIF
149200030715     C                   EVAL      i = i + 1
149300030715     C                   ENDDO
149400070925     C*
149500071029     C***  ===>  Gestione campi "particolari" fuori ciclo
149600070925     C*
149700071029     C***  <===  -----------------------------------------
149800070925     C*
149900070925     C* Considerazioni finali su CBO/CAS
150000100318     C                   IF        wFlgCAS = '1'    OR
150100100318     C                             VABCAS  > *zeros
150200070925     C                   IF        VABCBO = '1'
150300070925     C                   EVAL      VABCBO = '4'
150400070925     C                   ENDIF
150500070925     C                   IF        VABCBO = '2'
150600070925     C                   EVAL      VABCBO = '6'
150700070925     C                   ENDIF
150800070925     C                   ENDIF
150900070925     C***  <===  ----------------------------
151000040510     C*
151100040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
151200040510     C                   IF        VABNSP = *zeros
151300040510     C                   SETON                                        31
151400040510     C                   ENDIF
151500040715     C*
151600040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
151700040715     C                   IF        VABLNP = *zeros
151800040715     C                   SETON                                        31
151900040715     C                   ENDIF
152000040510     C*
152100040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
152200040510     C                   IF        VABRMN = *zeros
152300040510     C                   Z-ADD     VABNSP        VABRMN
152400040510     C                   ENDIF
152500040510     C*
152600040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
152700040510     C                   IF        VABRMA = *blanks
152800040714     C                   MOVEL     VABRMN        VABRMA
152900040510     C                   ENDIF
153000040128     C*
153100040510     C* Se provincia nn valorizzata la reperisco
153200040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
153300040510     C                   IF        VABPRD = *blanks
153400040128     C                   CLEAR                   TISI95DS
153500040128     C                   EVAL      I95TCN = '3'
153600040128     C                   Z-ADD     datcor        I95DAT
153700040128     C                   EVAL      I95NAR = VABNZD
153800040128     C                   EVAL      I95CAP = VABCAD
153900040128     C                   EVAL      I95LOC = VABLOD
154000040128     C                   CALL      'TISI95R'
154100040128     C                   PARM                    TISI95DS
154200040128     C                   EVAL      VABPRD = O95PRV
154300040510     C                   ENDIF
154400020204     C*
154500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
154600020204     C                   EXSR      CHKIMPDIV
154700030715     C                   ENDIF
154800020204     C*
154900010607     C                   ENDSR
155000010607     C*----------------------------------------------------*
155100010601
155200020204
155300020204     C*----------------------------------------------------*
155400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
155500020204     C*----------------------------------------------------*
155600020204     C     CHKIMPDIV     BEGSR
155700020204     C*
155800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
155900020204     C                   Z-ADD     *zeros        wrkDec            9 9
156000020204     C*
156100020204     C* Come prima cosa effettuo considerazioni sulla divisa
156200020204     C                   IF        vabIAS > *zeros
156300020204     C                   IF        vabVAS <> 'EUR'
156400020204     C                   EVAL      vabVAS =  'ITL'
156500020204     C                   ENDIF
156600020204     C                   ENDIF
156700020204     C*
156800020204     C                   IF        vabCAS > *zeros
156900020204     C                   IF        vabVCA <> 'EUR'
157000020204     C                   EVAL      vabVCA =  'ITL'
157100020204     C                   ENDIF
157200020204     C                   ENDIF
157300020204     C*
157400020204     C                   IF        vabVMD > *zeros
157500020204     C                   IF        vabVAD <> 'EUR'
157600020204     C                   EVAL      vabVAD =  'ITL'
157700020204     C                   ENDIF
157800020204     C                   ENDIF
157900020204     C*
158000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
158100020204     C                   Z-ADD     vabIAS        wrkDec
158200020204     C                   IF        wrkDec > *zeros
158300020204     C                   IF        vabVAS = 'ITL'
158400020204     C                   EVAL      vabIAS = *zeros
158500020204     C                   ENDIF
158600020204     C                   ENDIF
158700020204     C*
158800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
158900020204     C                   Z-ADD     vabCAS        wrkDec
159000020204     C                   IF        wrkDec > *zeros
159100020204     C                   IF        vabVCA = 'ITL'
159200020204     C                   EVAL      vabCAS = *zeros
159300020204     C                   ENDIF
159400020204     C                   ENDIF
159500020204     C*
159600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
159700020204     C                   Z-ADD     vabVMD        wrkDec
159800020204     C                   IF        wrkDec > *zeros
159900020204     C                   IF        vabVAD = 'ITL'
160000020204     C                   EVAL      vabVMD = *zeros
160100020204     C                   ENDIF
160200020204     C                   ENDIF
160300020204     C*
160400020204     C                   ENDSR
160500020204     C***
160600010330
160700100318
160800100318
160900100318     C*----------------------------------------------------*
161000100318     C*  CONTROLLO NUMERICITA' CAMPI
161100100318     C*----------------------------------------------------*
161200100318     C     CHKNUM        BEGSR
161300100318     C*
161400100318     C                   IF        PiDecChr = *blanks
161500100318     C                   EVAL      PiDecChr = CharNUM
161600100318     C                   ENDIF
161700100318     C*
161800100318     C                   callp     UBISNUM_Check(PiStr
161900100318     C                                          :PiDecChr
162000100318     C                                          :PiVal
162100100318     C                                          :PiNum
162200100318     C                                          :PiInt)
162300100318     C*
162400100318     C                   ENDSR
162500100318     C***
162600010601
162700010601
162800010601
162900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
163000010601     C     repfil        BEGSR
163100010601     C*
163200010601     C                   if        invfil = *zeros and
163300010601     C                             depfil > *zeros and
163400010629     C                             (vinflg = *blanks or
163500010629     C                              vinflg = *zeros)
163600010601     C
163700010601     C                   eval      invfil = depfil
163800010601     C                   endif
163900010601     C*
164000010601     C                   if        depfil <> invfil and
164100010601     C                             invfil > *zeros
164200010601     C                   eval      flgMulti = '1'
164300010601     C                   if        vinflg = *blanks
164400010601     C                   add       1             cntNonEl
164500010601     C                   endif
164600010601     C                   endif
164700010601     C*
164800010601     C                   if        vinflg = '2'
164900010601     C                   eval      flgStato = '2'
165000010601     C                   endif
165100010601     C*
165200010601     C                   ENDSR
165300010601     C***
165400010601
165500010601
165600010601
165700010330
165800040119      /TITLE Invio dei dati al punto operativo.
165900040119     C     invio         BEGSR
166000040119     C*
166100040119     C* 1° invio FIVAT
166200040119     C                   reset                   dscmz
166300040119     C                   move      vlrpoi        cmzdst
166400040119     C                   eval      cmzfld = 'FIVATWWR'
166500040119     C                   eval      cmzmbd = vlrhdl
166600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
166700040119     C***                if        prmfir = *blanks
166800040119     C                   eval      cmzfla = 'FIVAT00F'
166900040119     C                   eval      cmzmba = 'FIVAT00F'
167000040119     C***                else
167100040119     C***                eval      cmzfla = prmfir
167200040119     C***                eval      cmzmba = prmfir
167300040119     C***                endif
167400040119     C                   eval      cmznrr = *zeros
167500040119     C                   move      §ctrokvt      cmznrr
167600040119     C                   eval      cmzlba = vlrfl1
167700040119     C                   call(e)   'TIS711C'
167800040119     C                   parm                    dscmz
167900040119     C                   parm      *blanks       esito
168000040119     C                   if        %error
168100040119     C                             or cmzerr = '1'
168200040119     C                             or esito  = '1'
168300040119     C                   eval      wrkesito = '3'
168400040119     C                   else
168500040119     C*
168600040119     C* 2° invio FIVAB
168700040119     C                   reset                   dscmz
168800040119     C                   move      vlrpoi        cmzdst
168900040119     C                   eval      cmzfld = vlrfou
169000040119     C                   eval      cmzmbd = vlrhdl
169100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
169200040119     C***                if        prmfir = *blanks
169300040119     C                   eval      cmzfla = 'FIVAB00F'
169400040119     C                   eval      cmzmba = 'FIVAB00F'
169500040119     C***                else
169600040119     C***                eval      cmzfla = prmfir
169700040119     C***                eval      cmzmba = prmfir
169800040119     C***                endif
169900040119     C                   eval      cmznrr = *zeros
170000040119     C                   move      §ctrokvb      cmznrr
170100040119     C                   eval      cmzlba = vlrfl1
170200040119     C                   call(e)   'TIS711C'
170300040119     C                   parm                    dscmz
170400040119     C                   parm      *blanks       esito
170500040119     C                   if        %error
170600040119     C                             or cmzerr = '1'
170700040119     C                             or esito  = '1'
170800040119     C                   eval      wrkesito = '3'
170900040119     C                   endif
171000040119     C                   endif
171100040119     C*
171200040119     C                   ENDSR
171300040119     C***
171400010601
171500010601
171600010601
171700010601
171800010601      /TITLE Invio dei dati al punto operativo.
171900010601     C     opeini        BEGSR
172000010601     C*
172100010601     C* Inizializzo flag e contatori operativi
172200010601     C                   movel     '0'           flgGiro           1
172300010601     C                   movel     '0'           flgMulti          1
172400010601     C                   movel     '1'           flgStato          1
172500010615     C                   movel     '0'           flgOk             1
172600010601     C                   z-add     *zeros        cntNonEl         10 0
172700010601     C                   z-add     *zeros        depfil            3 0
172800010601     C                   z-add     *zeros        invfil            3 0
172900010601     C*
173000010601     C                   ENDSR
173100010601     C***
173200070326
173300070326
173400070326
173500070326
173600070326     C     *pssr         BEGSR
173700070329     C*
173800070329     C                   if        %open(tivin00r)
173900070329     C                   close     tivin00r
174000070329     C                   endif
174100070329     C                   if        %open(fivabwwr)
174200070329     C                   close     fivabwwr
174300070329     C                   endif
174400070329     C                   if        %open(fivatwwr)
174500070329     C                   close     fivatwwr
174600070329     C                   endif
174700070326     C*
174800070326     C* Effettuo la chiamata al CLLE preposto
174900070326     C                   call(e)   'TITVVTC'
175000070326     C                   parm                    parccm
175100070326     C                   parm                    parmbr
175200070326     C                   parm      '2'           paropz
175300070326     C*
175400070326     C                   eval      wrkesito = '2'
175500070404     C*
175600070404     C                   seton                                        LR
175700070326     C*
175800070326     C                   ENDSR     '*CANCL'
175900070326     C***
176000100318
176100100318
176200100318
176300100318     C     normCSV       BEGSR
176400100318     C*
176500100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
176600100318     C* separatori campi
176700100318     C                   if        CharTXT <> *blanks
176800100318     C                   z-add     *zeros        wLenVINDTA        4 0
176900100318     C                   z-add     *zeros        wFlgVINDTA        1 0
177000100318     C                   setoff                                       80
177100100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
177200100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
177300100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
177400100318     C                   if        not *in80
177500100318     C                   seton                                        80
177600100318     C                   else
177700100318     C                   setoff                                       80
177800100318     C                   endif
177900100318     C                   endif
178000100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
178100100318     C                             and *in80 = *on
178200100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
178300100318     C                   endif
178400100318     C                   enddo
178500100318     C                   endif
178600100318     C*
178700100318     C                   ENDSR
178800100318     C***
178900070326
179000100326
179100100326
179200100326     C*----------------------------------------------------*
179300100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
179400100326     C*----------------------------------------------------*
179500100326     C     REPNSP        BEGSR
179600100326     C*
179700100326     C* NSP => Stacco un numeratore da AZNUM
179800100326     C                   clear                   TRUL33DS
179900100326     C                   eval      I33OPE = *zeros
180000100326     C                   eval      I33CNU = 302
180100100326     C                   eval      I33NUM = 1
180200100326     C                   movel     TRUL33DS      KPJBU
180300100326     C                   call      'TRUL33R'
180400100326     C                   parm                    KPJBA
180500100326     C                   movel     KPJBU         TRUL33DS
180600100326     C                   if        O33ERR = *zeros
180700100326     C                   z-add     O33NRF        VABNSP
180800100326     C                   z-add     O33NRF        VATNSP
180900100326     C                   else
181000100326     C                   SETON                                        31
181100100326     C                   EVAL      vinmsg = %trimr(vinmsg)
181200100326     C                             + ' ' + 'VABNSP VATNSP'
181300100326     C                   endif
181400100326     C*
181500100326     C                   ENDSR
181600010330
181700100705
181800100705
181900100705     C*--------------------------------------------------------
182000100705     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
182100100705     C*--------------------------------------------------------
182200100705     C     CARTAB        BEGSR
182300100705     C*
182400100705     C* TABELLA '15' - NAZIONI
182500100705     C                   clear                   skNAZISO
182600100705     C                   clear                   skNAZBAR
182700100705     C                   eval      tblKUT = 1
182800100705     C                   eval      tblCOD = '15'
182900100705     C     KEYtabP       setll     tabel00f
183000100705     C     KEYtabP       reade     tabel00f
183100100705     C                   dow       not %eof(tabel00f)
183200100705     C                   if        tblFLG = *blanks
183300100705     C                   movel(p)  tblUNI        ds15
183400100705     C                   add       1             jNAZ
183500100705     C                   eval      skNAZBAR(jNAZ) = tblKEY
183600100705     C                   eval      skNAZISO(jNAZ) = §15COD
183700100705     C                   endif
183800100705     C     KEYtabP       reade     tabel00f
183900100705     C                   enddo
184000100705     C*
184100100705     C                   ENDSR
184200100705     C***
184300100705
184400100326
184500010330
184600000613     C     *inzsr        BEGSR
184700990910     C*
184800990910     C     *entry        plist
184900990920     C                   parm                    tivlrds
185000990921     C                   parm      wrkesito      esito
185100000724     C                   parm                    prmlit
185200000710     C                   parm                    prmfir
185300100705     C*
185400100705     C* Chiave su TABEL00F - parziale
185500100705     C     KEYtabP       KLIST
185600100705     C                   KFLD                    tblKUT
185700100705     C                   KFLD                    tblCOD
185800010330     C*
185900010330     C* CALCOLA LA DATA CORRENTE
186000100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
186100100318     C                   eval      datcor = %dec(%date() : *ISO)
186200000613     C*
186300000613     C                   ENDSR
186400000613     C***
186500990908
186600100413     Otitv1w6p  E            riepilogo         2
186700990915     O                                              'Upload via Internet'
186800990915     O                                           +1 'Traduzione TIVIN00R -'
186900021025     O                                           +1 'FIVABWWR'
187000990915     O                                           +1 'Testate bolle'
187100990915     O          E            riepilogo   2
187200990915     O                       wrkdata
187300990915     O                       wrkora              +1
187400990915     O                       procname            +1
187500990915     O          E            riepilogo   2
187600990915     O                                              'Cliente..................:'
187700010605     O                       VABCCM        z     +1
187800990915     O          E            riepilogo   2
187900990920     O                                              'Riferimento Strategi.....:'
188000990920     O                       vlrhdl              +1
188100990915     O          E            riepilogo   2
188200990915     O                                              'Giusti...................:'
188300040119     O                       §CTROKVB      2   +  1
188400990915     O          E            riepilogo   2
188500990915     O                                              'Sbagliati e corretti.....:'
188600971022     O                       §CTRMO        2   +  1
188700990915     O          E            riepilogo   2
188800990915     O                                              'Sbagliati e scartati.....:'
188900971022     O                       §CTRNO        2   +  1
189000000613
189100100413     Otitv1w6ps E            testdett          2
189200000613     O                                              'Upload via Internet'
189300000613     O                                           +1 'Traduzione TIVIN00R -'
189400021025     O                                           +1 'FIVABWWR'
189500000613     O                                           +1 'Testate bolle'
189600000616     O          E            testdett    3
189700000613     O                                           +2 'N° rec'
189800000613     O                                           +3 'Anteprima contenuto'
189900000616     O          E            rigadett    2
190000000613     O                       rrnum               +2
190100000621     O                       recko               +3
190200000616     O          E            findett     2
190300000613     O                       wrkdata
190400000613     O                       wrkora              +1
190500000613     O                       procname            +1
190600000616     O          E            findett     2
190700000613     O                                              'Cliente..................:'
190800010605     O                       VABCCM        z     +1
190900000616     O          E            findett     2
191000000613     O                                              'Riferimento Strategi.....:'
191100000613     O                       vlrhdl              +1
191200000616     O          E            findett     2
191300000613     O                                              'Giusti...................:'
191400040119     O                       §CTROKVB      2   +  1
191500000616     O          E            findett     2
191600000613     O                                              'Sbagliati e corretti.....:'
191700000613     O                       §CTRMO        2   +  1
191800000616     O          E            findett     2
191900000613     O                                              'Sbagliati e scartati.....:'
192000000613     O                       §CTRNO        2   +  1
192100000613** CMD - COMANDI CL
192200100413OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1W6P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
192300100413OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1W6PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
192400100413DLTOVR FILE(TITV1W6P TITV1W6PS) LVL(*)
192500010606
192600000613
192700040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
192800100705W_VABRMN;VATNOT_E;VABRSD;VABRD2;VABIND;VABNOT;VABCAD;VABLOD;VABPRD;
192900110804W_VABNZD;VATNOT_B;VABNCL;VABPKB;VABCAS;VABCBO;VABFFD;VABNT2;VABIAS;
193000100318
193100100318
193200100318
