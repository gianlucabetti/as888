000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600080215     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
000700040119     FFIVATwwr  O    E             DISK    usropn
000800080215     Ftitv1d9p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000080219     F                                     infsr(*pssr)
001100080215     Ftitv1d9ps O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300080219     F                                     infsr(*pssr)
001400000313     D*
001500000613     D*------------
001600000613     D* COMANDI
001700000613     D*------------
001800011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900040113     D*------------
002000040113     D* SCHIEDE A PROGRAMMA
002100040113     D*------------
002200080215     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
002300010330     D*----------------------------------------------------
002400010330     D* DICHIARAZIOINE VARIABILI DI WRK
002500010330     D*----------------------------------------------------
002600010330     D dscmz         e ds                  inz
002700010330     D psds           sds
002800010330     D  procname         *PROC
002900010330     D tivlrds       e ds                  extname(tivlr00f)
003000040128     D tisi95ds      e ds
003100080215     D fivabds       e ds                  extname(fivabwwr) inz
003200080215     D fivabds_ok    e ds                  extname(fivabwwr) prefix(ok_) inz
003300010330     D esito           s              1
003400010330     D prmlit          s             10
003500010330     D prmfir          s             10
003600010330     D wrkesito        s                   like(esito)
003700010330     D wrkdata         s               d
003800010330     D wrkora          s               t
003900010330     D rrnum           s              6  0 INZ(*zeros)
004000010330     D recko           s            150    INZ(*blanks)
004100011113     D depcmd          s            150    INZ(*blanks)
004200080215     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004300040510     D parccm          s              8    INZ(*blanks)
004400040119     D parmbr          s             10    INZ(*blanks)
004500040119     D paropz          s              1    INZ(*blanks)
004600040119     D chkcall         s              1    INZ(*blanks)
004700080125     D wFlgCAS         s              1    INZ(*blanks)
004800080215     D wLargheCM       s             30  9 inz
004900080215     D wAltezzCM       s             30  9 inz
005000080215     D wProfonCM       s             30  9 inz
005100080215     D wVolumeCM3      s             30  9 inz
005200080215     D wVolumeM3       s             30  9 inz
005300080215     D wPesoKG         s             30  9 inz
005400030715     D*------------------
005500030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005600030715     D*------------------
005700030715     D SkSplitFLD      S             10    DIM(100)
005800030715     D SkSplitCSV      S            256    DIM(100)
005900030715     D CharCSV         S              1
006000030715     D CharTXT         S              1
006100030715     D CharNUM         S              1
006200030715     D posDa           S              3  0 INZ(*zeros)
006300030715     D posA            S              3  0 INZ(*zeros)
006400030715     D i               s              3  0 INZ(1)
006500030715     D wGiro           s              1  0 INZ(*zeros)
006600080125     D wVATNOT_A       s                   LIKE(VATNOT)
006700080125     D wVATNOT_B       s                   LIKE(VATNOT)
006800080125     D wVATNOT_E       s                   LIKE(VATNOT)
006900090107     D wVATNOT_E1      s                   LIKE(VATNOT)
007000090107     D wVATNOT_E2      s                   LIKE(VATNOT)
007100090122     D wVATNOT_E3      s                   LIKE(VATNOT)
007200030822     D*------------------
007300030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
007400030822     D*------------------
007500030822     D posDaDft        S              3  0 INZ(*zeros)
007600030822     D posADft         S              3  0 INZ(*zeros)
007700030822     D j               s              3  0 INZ(1)
007800030822     D wGiroDft        s              1  0 INZ(*zeros)
007900010330     D*------------------
008000010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
008100010330     D*------------------
008200010330     D WLBDA8          DS                  INZ
008300010330     D  G08DAT                 1      8  0
008400010330     D  G08INV                 9     16  0
008500010330     D  G08ERR                17     17
008600010330     D  G08TGI                18     22  0
008700031201     D*------------------
008800031201     D* Costanti
008900031201     D*------------------
009000031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009100031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009200010330
009300010330
009400010330
009500990915     C                   time                    wrkdata
009600990915     C                   time                    wrkora
009700000913     C                   reset                   rrnum
009800990921     C                   reset                   esito
009900990921     C                   reset                   wrkesito
010000010601     C*
010100010601     C                   exsr      opeini
010200010605     C                   exsr      rwvab
010300010601     C*
010400010601     C                   seton                                        lr
010500010601
010600010601
010700010601
010800010601     C*--------------------------------------------------------
010900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011000010601     C*--------------------------------------------------------
011100010601     C     PREELA        BEGSR
011200010601     C*
011300010601     C* SE OCCORRE SPEDIRE IN FILIALE
011400010601     C                   if        invfil <> *zeros and
011500010601     C                             flgGiro = '0'
011600010601     C*
011700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011800010601     C                   eval      flgGiro = '1'
011900000724     C*
012000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
012100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012200010601     C     invfil        chain     azorg01l
012300000724     C                   if        %found
012400000724     C                   movel(p)  CMD(1)        depcmd
012500020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
012600000724     C*
012700000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
012800011113     C                   Z-ADD     150           LENGH            15 5
012900000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013000000724     C                   PARM                    depcmd
013100000724     C                   PARM                    LENGH
013200000724     C*
013300000724     C                   endif
013400000616     C*
013500000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
013600000616     C                   movel(p)  CMD(2)        depcmd
013700000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
013800000616     C*
013900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
014000011113     C                   Z-ADD     150           LENGH            15 5
014100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014200000616     C                   PARM                    depcmd
014300000616     C                   PARM                    LENGH
014400000613     C*
014500080215     C                   if        not %open(titv1d9ps)
014600080215     C                   open      titv1d9ps
014700070329     C                   except    testdett
014800000613     C                   endif
014900010601     C*
015000010601     C                   endif
015100010601     C*
015200010601     C                   ENDSR
015300010601     C***
015400010601
015500010601
015600010601
015700010601
015800010601     C*--------------------------------------------------------
015900010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
016000010601     C*--------------------------------------------------------
016100010601     C     ENDELA        BEGSR
016200990915     C*
016300000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
016400040119     C*
016500040119     C* Effettuo la chiamata al CLLE preposto
016600040119     C                   call(e)   'TITVVTC'
016700040510     C                   parm                    parccm
016800040119     C                   parm                    parmbr
016900040119     C                   parm      '2'           paropz
017000050201     C*
017100050201     C* Effettuo lancio TISI95 solo x chiusura
017200050201     C                   CLEAR                   TISI95DS
017300050201     C                   EVAL      I95TLA = 'C'
017400050201     C                   CALL      'TISI95R'
017500050201     C                   PARM                    TISI95DS
017600000613     C*
017700080215     C                   if        %open(titv1d9ps)
017800000616     C                   except    findett
017900080215     C                   close     titv1d9ps
018000000613     C                   endif
018100000616     C*
018200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
018300011113     C                   Z-ADD     150           LENGH            15 5
018400010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
018500000616     C                   PARM                    CMD(3)
018600000616     C                   PARM                    LENGH
018700000616     C*
018800010601     C                   ENDSR
018900010601     C***
019000010601
019100010601
019200010601
019300000613
019400000613     C*--------------------------------------------------------
019500000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
019600000613     C*--------------------------------------------------------
019700000621     C     STPR          BEGSR
019800000613     C*
019900080215     C                   if        not %open(titv1d9p)
020000080215     C   50              open      titv1d9p
020100990915     C                   endif
020200990915     C*
020300030715     C   50              except    riepilogo
020400990915     C*
020500080215     C                   if        %open(titv1d9p)
020600080215     C   50              close     titv1d9p
020700990914     C                   endif
020800000613     C*
020900000613     C                   ENDSR
021000000613     C***
021100000613
021200010601
021300010601
021400010330     C*--------------------------------------------------------
021500021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
021600010330     C*--------------------------------------------------------
021700010605     C     RWVAB         BEGSR
021800010330     C*
021900010330     C                   if        not %open(tivin00r)
022000010330     C                   open      tivin00r
022100010330     C                   endif
022200021025     C                   if        not %open(fivabwwr)
022300021025     C                   open      fivabwwr
022400010330     C                   endif
022500040119     C*
022600040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
022700040119     C                   exsr      prevat
022800040119     C*
022900040119     C                   if        chkcall = '0'
023000040119     C*
023100040119     C                   if        not %open(fivatwwr)
023200040119     C                   open      fivatwwr
023300040119     C                   endif
023400010330     C*
023500040119     C                   clear                   §CTROKVB          7 0
023600040119     C                   clear                   §CTROKVT          7 0
023700040119     C                   clear                   §CTRMO            7 0
023800040119     C                   clear                   §CTRNO            7 0
023900070823     C*
024000070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
024100070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
024200080215     C                   exsr      inzvar
024300080215     C                   exsr      impvab
024400010330     C*
024500030822     C                   do        *HIVAL
024600010330     C*
024700030822     C                   read      tivin00r                               70
024800010618     C*
024900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
025000010618     C                   if        vinflg = '1'
025100010618     C                   eval      flgOk = '1'
025200010618     C                   endif
025300040510     C*
025400040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
025500040510     C                   z-add     *zeros        wLenVINDTA        4 0
025600040510     C                   z-add     *zeros        wFlgVINDTA        1 0
025700040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
025800040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
025900040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
026000040510     C                   z-add     1             wFlgVINDTA
026100040510     C                   leave
026200040510     C                   endif
026300040510     C                   enddo
026400010618     C*
026500040510     C                   if        vindta > *blanks AND
026600040510     C                             wFlgVINDTA = 1
026700010330     C                   add       1             rrnum
026800010330     C*
026900010601     C                   if        *in70 = *off and
027000010330     C                             (vinflg = *blanks
027100010330     C                              or vinflg = '0'
027200010330     C                              or vinflg = '2')
027300010330     C*
027400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
027500010711     C                   if        vinflg = *blanks or vinflg = '0'
027600010711     C                   clear                   vinmsg
027700010711     C                   endif
027800010601     C*
027900070925     C* Aggiungo un byte a blanks all'inizio d ogni record
028000070925     C                   eval      vindta  = ' ' + %trim(vindta)
028100070925     C*
028200080219     C                   exsr      inzvar
028300010605     C                   exsr      impvab
028400010601     C*
028500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
028600010605     C                   eval      depfil = VABLNP
028700010601     C                   exsr      repfil
028800010601     C                   if        depfil = invfil
028900021025     C                   if        vlrpoi = 999
029000030822     C                   move(P)   invfil        VABFGS
029100021025     C                   else
029200030822     C                   move(P)   vlrpoi        VABFGS
029300021025     C                   endif
029400010601     C*
029500010601     C                   exsr      PREELA
029600010601     C*
029700010604     C* Ebbene...
029800010604     C*
029900040119     C  N31              add       1             §CTROKVB
030000040119     C   32              add       1             §CTRMO
030100040119     C   31              add       1             §CTRNO
030200030822     C                   if        wGiro = 2
030300040510     C* Scrivo sempre il VAT
030400040510     C  N31              exsr      wrivat
030500040510     C* Scrivo il VAB solo a rottura d spedizione
030600080219     C                   if        depspe = *zeros
030700080215     C  N31              eval      depspe = vabNSP
030800080215     C                   endif
030900080215     C                   if        depspe <> *zeros AND
031000080215     C                             depspe <> vabNSP
031100040119     C  N31              exsr      wrivab
031200080220     C***                EVAL      VABPKB = *zeros
031300080220     C***                EVAL      VABVLB = *zeros
031400080219     C                   EVAL      depspe = vabNSP
031500040119     C                   endif
031600040119     C                   endif
031700080220     C***                EVAL      VABPKB = VABPKB + wPesoKG
031800080220     C***                EVAL      VABVLB = VABVLB + wVolumeM3
031900080219     C                   eval      fivabds_ok = fivabds
032000010604     C*
032100010604     C                   if        *in31 = *off and
032200010604     C                             *in32 = *off
032300010604     C                   eval      vinflg = '1'
032400010604     C                   else
032500010604     C                   eval      recko = vindta
032600010604     C                   except    rigadett
032700010604     C                   eval      vinflg = '2'
032800010604     C                   endif
032900010604     C                   endif
033000010601     C*
033100010604     C                   endif
033200010604     C*
033300010330     C                   else
033400010330     C                   eval      vinflg = '1'
033500010330     C                   endif
033600010601     C*
033700010601     C  N70              update    tivin000
033800010330     C*
033900030822     C  N70              enddo
034000040119     C*
034100080215     C* Scarico l'ultima bolla rimasta in canna
034200080215     C  N31              exsr      wrivab
034300080215     C*
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
039500040119
039600040119
039700040119
039800040119     C*----------------------------------------------------*
039900040119     C*  SCARICAMENTO BUFFER RECORDS VAB
040000040119     C*----------------------------------------------------*
040100040510     C     WRIVAB        BEGSR
040200080220     C*
040300080220     C* Forzatura richiesta da RF
040400080220     C                   eval      VABPKB = 1 * VABNCL
040500080220     C                   eval      VABVLB = *zeros
040600040119     C*
040700060331     C* Gestisco l'eventuale rottura x numero spedizione
040800070301     C                   if        wVATNOT_E <> *blanks
040900080125     C                   if        VABCTM = *blanks
041000080125     C                   eval      VABCTM = '7Q'
041100080125     C                   endif
041200070301     C                   endif
041300080215     C*
041400040119     C                   write     fivab000                                     => scarico il VAB
041500040119     C*
041600040119     C                   ENDSR
041700040119
041800040119
041900040119
042000040119     C*----------------------------------------------------*
042100040119     C*  SCARICAMENTO BUFFER RECORDS VAT
042200040119     C*----------------------------------------------------*
042300040510     C     WRIVAT        BEGSR
042400060512     C*
042500060512     C* Inizializzo i campi chiave
042600060512     C                   eval      VATFGS = VABFGS
042700060512     C                   eval      VATAAS = VABAAS
042800060512     C                   eval      VATCCM = VABCCM
042900070823     C                   eval      VATNRS = VABNRS
043000060512     C                   eval      VATNSP = VABNSP
043100060512     C                   eval      VATLNP = VABLNP
043200040714     C*
043300040714     C* Solo se VATNOT valorizzato scarico il buffer
043400040714     C                   if        VATNOT <> *blanks
043500040119     C*
043600040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
043700040510     C                   if        VATTRC = *blanks
043800040510     C                   eval      VATTRC = 'E'
043900040510     C                   endif
044000040510     C                   write     FIVAT000
044100040119     C                   add       1             §CTROKVT
044200040714     C*
044300040714     C                   endif
044400060331     C*
044500060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
044600060331     C                   if        wVATNOT_A <> *blanks
044700060331     C                   eval      VATNOT = wVATNOT_A
044800060331     C                   eval      VATTRC = 'A'
044900060331     C                   write     FIVAT000
045000060331     C                   add       1             §CTROKVT
045100060331     C                   endif
045200060331     C*
045300060331     C                   if        wVATNOT_B <> *blanks
045400060331     C                   eval      VATNOT = wVATNOT_B
045500060331     C                   eval      VATTRC = 'B'
045600060331     C                   write     FIVAT000
045700060331     C                   add       1             §CTROKVT
045800060331     C                   endif
045900060331     C*
046000060331     C                   if        wVATNOT_E <> *blanks
046100060331     C                   eval      VATNOT = wVATNOT_E
046200060331     C                   eval      VATTRC = 'E'
046300060331     C                   write     FIVAT000
046400060331     C                   add       1             §CTROKVT
046500060331     C                   endif
046600040119     C*
046700040119     C                   ENDSR
046800040119
046900040119
047000040119
047100040119     C*----------------------------------------------------*
047200040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
047300040119     C*----------------------------------------------------*
047400040119     C     PREVAT        BEGSR
047500040119     C*
047600040119     C* Compongo il nome del membro da dare al FIVATWWR
047700040119     C                   eval      parmbr = vlrhdl
047800040119     C                   movel     'M'           parmbr
047900040510     C                   eval      parccm = vlrksc
048000040119     C                   eval      paropz = '1'
048100040119     C* Effettuo la chiamata al CLLE preposto
048200040119     C                   call(e)   'TITVVTC'
048300040119     C                   parm                    parccm
048400040119     C                   parm                    parmbr
048500040119     C                   parm                    paropz
048600040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048700040119     C                   if        %error
048800040119     C                   movel     '1'           chkcall
048900040119     C                   else
049000040119     C                   movel     '0'           chkcall
049100040119     C                   endif
049200040119     C*
049300040119     C                   ENDSR
049400010601
049500010601
049600010601
049700010330     C*----------------------------------------------------*
049800030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
049900010330     C*----------------------------------------------------*
050000040119     C     INZVAR        BEGSR
050100010330     C*
050200030822     C* Inizializzo variabili di wrk
050300020204     C                   Z-ADD     *zeros        Num5_0            5 0
050400060331     C                   MOVEL     *blanks       wVATNOT_A
050500060331     C                   MOVEL     *blanks       wVATNOT_B
050600060331     C                   MOVEL     *blanks       wVATNOT_E
050700090109     C                   MOVEL     *blanks       wVATNOT_E1
050800090109     C                   MOVEL     *blanks       wVATNOT_E2
050900090122     C                   MOVEL     *blanks       wVATNOT_E3
051000030822     C*
051100030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
051200080215     C                   IF        depspe =  *zeros OR
051300080215     C                             depspe <> VABNSP
051400030822     C                   CLEAR                   FIVAB000
051500080215     C                   CLEAR                   FIVABDS
051600080215     C                   CLEAR                   wLargheCM
051700080215     C                   CLEAR                   wAltezzCM
051800080215     C                   CLEAR                   wProfonCM
051900080215     C                   CLEAR                   wVolumeCM3
052000080215     C                   CLEAR                   wVolumeM3
052100080215     C                   CLEAR                   wPesoKG
052200080215     C                   ENDIF
052300040119     C                   CLEAR                   FIVAT000
052400030822     C                   CLEAR                   SkSplitCSV
052500030822     C                   EVAL      i = 1
052600030822     C                   EVAL      posDa = *zeros
052700030822     C                   EVAL      posA  = *zeros
052800030822     C*
052900030822     C* Reimposto i valori di default
053000030822     C                   EXSR      DEFCAM
053100030822     C*
053200010330     C                   ENDSR
053300010330     C*----------------------------------------------------*
053400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
053500010330     C*----------------------------------------------------*
053600010330     C     DEFCAM        BEGSR
053700030822     C*
053800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
053900030715     C* e delimitatore testo.
054000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
054100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
054200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
054300030822     C*
054400030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
054500030715     C                   IF        %subst(vlrppt:1:1) = 'S'
054600030715     C                   SETON                                        50
054700030715     C                   ELSE
054800030715     C                   SETOFF                                       50
054900030715     C                   ENDIF
055000030822     C*
055100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
055200030822     C                   EVAL      posDaDft = 1
055300030822     C                   EVAL      posADft  = 0
055400030822     C                   EVAL      wGiroDft = 0
055500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
055600030822     C                             posDaDft > 0
055700030822     C*
055800030822     C* Gestisco il 1° giro
055900030822     C                   IF        wGiroDft = 0
056000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
056100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
056200030822     C* Incremento il contatore dei "giri"
056300030822     C                   EVAL      wGiroDft = 1
056400030822     C                   ELSE
056500030822     C                   EVAL      posDaDft = posADft
056600030822     C                   ENDIF
056700030822     C* Eseguo lo scan x trovare la fine del campo corrente
056800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
056900030822     C*
057000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
057100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
057200030822     C                   IF        posDaDft > 0 AND
057300030822     C                             posADft  > 0
057400030822     C* NCL
057500030822     C                   IF        %subst(
057600030822     C                             %subst(vlrppt:posDaDft+1:
057700030822     C                             posADft-posDaDft-1):1:3)
057800030822     C                             = 'NCL'
057900030822     C                   EVAL      PiStr=%trim(%subst(
058000030822     C                             %subst(vlrppt:posDaDft+1:
058100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058200030822     C                   EXSR      CHKNUM
058300030822     C                   IF        PiInt=*on
058400030822     C                   Z-ADD     PiVal         VABNCL
058500030822     C                   ENDIF
058600030822     C                   ENDIF
058700030822     C* CCM
058800030822     C                   IF        %subst(
058900030822     C                             %subst(vlrppt:posDaDft+1:
059000030822     C                             posADft-posDaDft-1):1:3)
059100030822     C                             = 'CCM'
059200030822     C                   EVAL      PiStr=%trim(%subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059500030822     C                   EXSR      CHKNUM
059600030822     C                   IF        PiInt=*on
059700030822     C                   Z-ADD     PiVal         VABCCM
059800030822     C                   ENDIF
059900030822     C                   ENDIF
060000030822     C* LNP
060100030822     C                   IF        %subst(
060200030822     C                             %subst(vlrppt:posDaDft+1:
060300030822     C                             posADft-posDaDft-1):1:3)
060400030822     C                             = 'LNP'
060500030822     C                   EVAL      PiStr=%trim(%subst(
060600030822     C                             %subst(vlrppt:posDaDft+1:
060700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060800030822     C                   EXSR      CHKNUM
060900030822     C                   IF        PiInt=*on
061000030822     C                   Z-ADD     PiVal         VABLNP
061100030822     C                   ENDIF
061200030822     C                   ENDIF
061300030822     C* NRS
061400030822     C                   IF        %subst(
061500030822     C                             %subst(vlrppt:posDaDft+1:
061600030822     C                             posADft-posDaDft-1):1:3)
061700030822     C                             = 'NRS'
061800030822     C                   EVAL      PiStr=%trim(%subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062100030822     C                   EXSR      CHKNUM
062200030822     C                   IF        PiInt=*on
062300030822     C                   Z-ADD     PiVal         VABNRS
062400030822     C                   ENDIF
062500030822     C                   ENDIF
062600030822     C* CTR
062700030822     C                   IF        %subst(
062800030822     C                             %subst(vlrppt:posDaDft+1:
062900030822     C                             posADft-posDaDft-1):1:3)
063000030822     C                             = 'CTR'
063100030822     C                   EVAL      PiStr=%trim(%subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063400030822     C                   EXSR      CHKNUM
063500030822     C                   IF        PiInt=*on
063600030822     C                   Z-ADD     PiVal         VABCTR
063700030822     C                   ENDIF
063800030822     C                   ENDIF
063900030822     C* PKB
064000030822     C                   IF        %subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):1:3)
064300030822     C                             = 'PKB'
064400030822     C                   EVAL      PiStr=%trim(%subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064700030822     C                   EXSR      CHKNUM
064800030822     C                   IF        PiNum=*on
064900030822     C                   Z-ADD     PiVal         VABPKB
065000030822     C                   ENDIF
065100030822     C                   ENDIF
065200030822     C* VLB
065300030822     C                   IF        %subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):1:3)
065600030822     C                             = 'VLB'
065700030822     C                   EVAL      PiStr=%trim(%subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066000030822     C                   EXSR      CHKNUM
066100030822     C                   IF        PiNum=*on
066200030822     C                   Z-ADD     PiVal         VABVLB
066300030822     C                   ENDIF
066400030822     C                   ENDIF
066500030822     C* QFT
066600030822     C                   IF        %subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):1:3)
066900030822     C                             = 'QFT'
067000030822     C                   EVAL      PiStr=%trim(%subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067300030822     C                   EXSR      CHKNUM
067400030822     C                   IF        PiNum=*on
067500030822     C                   Z-ADD     PiVal         VABQFT
067600030822     C                   ENDIF
067700030822     C                   ENDIF
067800030822     C* CBO
067900030822     C                   IF        %subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):1:3)
068200030822     C                             = 'CBO'
068300030822     C                   EVAL      VABCBO=%trim(%subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068600030822     C                   ENDIF
068700030822     C* TSP
068800030822     C                   IF        %subst(
068900030822     C                             %subst(vlrppt:posDaDft+1:
069000030822     C                             posADft-posDaDft-1):1:3)
069100030822     C                             = 'TSP'
069200030822     C                   EVAL      VABTSP=%trim(%subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069500030822     C                   ENDIF
069600030822     C* VAS
069700030822     C                   IF        %subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):1:3)
070000030822     C                             = 'VAS'
070100030822     C                   EVAL      VABVAS=%trim(%subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070400030822     C                   ENDIF
070500030822     C* VCA
070600030822     C                   IF        %subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):1:3)
070900030822     C                             = 'VCA'
071000030822     C                   EVAL      VABVCA=%trim(%subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071300030822     C                   ENDIF
071400070925     C* TIC
071500070925     C                   IF        %subst(
071600070925     C                             %subst(vlrppt:posDaDft+1:
071700070925     C                             posADft-posDaDft-1):1:3)
071800070925     C                             = 'TIC'
071900070925     C                   EVAL      VABTIC=%trim(%subst(
072000070925     C                             %subst(vlrppt:posDaDft+1:
072100070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072200070925     C                   ENDIF
072300030822     C* GCA
072400030822     C                   IF        %subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):1:3)
072700030822     C                             = 'GCA'
072800030822     C                   EVAL      VABGCA=%trim(%subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073100030822     C                   ENDIF
073200030822     C* CTM
073300030822     C                   IF        %subst(
073400030822     C                             %subst(vlrppt:posDaDft+1:
073500030822     C                             posADft-posDaDft-1):1:3)
073600030822     C                             = 'CTM'
073700030822     C                   EVAL      VABCTM=%trim(%subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074000030822     C                   ENDIF
074100030822     C* FFD
074200030822     C                   IF        %subst(
074300030822     C                             %subst(vlrppt:posDaDft+1:
074400030822     C                             posADft-posDaDft-1):1:3)
074500030822     C                             = 'FFD'
074600030822     C                   EVAL      VABFFD=%trim(%subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074900030822     C                   ENDIF
075000030822     C* VAD
075100030822     C                   IF        %subst(
075200030822     C                             %subst(vlrppt:posDaDft+1:
075300030822     C                             posADft-posDaDft-1):1:3)
075400030822     C                             = 'VAD'
075500030822     C                   EVAL      VABVAD=%trim(%subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075800030822     C                   ENDIF
075900030822     C* GMA
076000030822     C                   IF        %subst(
076100030822     C                             %subst(vlrppt:posDaDft+1:
076200030822     C                             posADft-posDaDft-1):1:3)
076300030822     C                             = 'GMA'
076400030822     C                   EVAL      VABGMA=%trim(%subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076700030822     C                   ENDIF
076800030822     C* GGA
076900030822     C                   IF        %subst(
077000030822     C                             %subst(vlrppt:posDaDft+1:
077100030822     C                             posADft-posDaDft-1):1:3)
077200030822     C                             = 'GGA'
077300030822     C                   EVAL      VABGGA=%trim(%subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077600030822     C                   ENDIF
077700030822     C* GVA
077800030822     C                   IF        %subst(
077900030822     C                             %subst(vlrppt:posDaDft+1:
078000030822     C                             posADft-posDaDft-1):1:3)
078100030822     C                             = 'GVA'
078200030822     C                   EVAL      VABGVA=%trim(%subst(
078300030822     C                             %subst(vlrppt:posDaDft+1:
078400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078500030822     C                   ENDIF
078600030822     C* TC1
078700030822     C                   IF        %subst(
078800030822     C                             %subst(vlrppt:posDaDft+1:
078900030822     C                             posADft-posDaDft-1):1:3)
079000030822     C                             = 'TC1'
079100030822     C                   EVAL      VABTC1=%trim(%subst(
079200030822     C                             %subst(vlrppt:posDaDft+1:
079300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079400030822     C                   ENDIF
079500030822     C* TC2
079600030822     C                   IF        %subst(
079700030822     C                             %subst(vlrppt:posDaDft+1:
079800030822     C                             posADft-posDaDft-1):1:3)
079900030822     C                             = 'TC2'
080000030822     C                   EVAL      VABTC2=%trim(%subst(
080100030822     C                             %subst(vlrppt:posDaDft+1:
080200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080300030822     C                   ENDIF
080400040714     C* VATTRC
080500040714     C                   IF        %subst(
080600040714     C                             %subst(vlrppt:posDaDft+1:
080700040714     C                             posADft-posDaDft-1):1:3)
080800040714     C                             = 'TRC'
080900040714     C                   EVAL      VATTRC=%trim(%subst(
081000040714     C                             %subst(vlrppt:posDaDft+1:
081100040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081200040714     C                   ENDIF
081300030822     C* ...
081400030822     C                   ENDIF
081500030822     C                   ENDDO
081600020204     C*
081700010330     C                   ENDSR
081800010607     C*----------------------------------------------------*
081900021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
082000010607     C*----------------------------------------------------*
082100010607     C     IMPVAB        BEGSR
082200010607     C*
082300010607     C                   SETOFF                                       3132
082400030515     C*
082500010607     C                   MOVEL     datcor        VABAAS
082600010607     C                   MOVE      datcor        VABMGS
082700010607     C*
082800030715     C**********
082900030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
083000030715     C**********
083100030715     C                   IF        wGiro = *zeros
083200040113     C*
083300080215     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
083400040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
083500080215     C                             %trim(intcol(3))+%trim(intcol(4))+
083600080215     C                             %trim(intcol(5))<>*blanks
083700040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
083800080215     C                                      %trim(intcol(3))+%trim(intcol(4))+
083900080215     C                                      %trim(intcol(5))
084000070823     C     ';':CharCSV   XLATE     vindta        vindta
084100040510     C                   ENDIF
084200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
084300031201     C     minu:maiu     XLATE     vindta        vindta
084400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
084500030715     C                   DOW       posDa <= %len(%trim(vindta))
084600030715     C*
084700030715     C* Gestisco il 1° campo
084800030715     C                   IF        i = 1
084900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
085000030715     C                   EVAL      posDa = 1
085100030715     C* Eseguo lo scan x trovare la fine del primo campo
085200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085300030715     C* A questo punto "estraggo" il campo corrente
085400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085500030715     C                                                    (posA-posDa))
085600030715     C* X i campi successivi al 1°
085700030715     C                   ELSE
085800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
085900030715     C                   EVAL      posDa = posA + 1
086000030715     C* Eseguo lo scan x trovare la fine del campo corrente
086100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086200030715     C* Gestisco l'ultimo campo
086300030715     C                   IF        posA = *zeros
086400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
086500030715     C* A questo punto "estraggo" il campo corrente
086600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086700030715     C                                                    (posA-posDa))
086800030715     C                   LEAVE
086900030715     C                   ELSE
087000030715     C* A questo punto "estraggo" il campo corrente
087100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087200030715     C                                                    (posA-posDa))
087300030715     C                   ENDIF
087400030715     C                   ENDIF
087500030715     C* Incremento il contatore d campo
087600030715     C                   EVAL      i = i +1
087700030715     C                   ENDDO
087800030715     C*
087900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
088000030715     C* delimitazione testo
088100030715     C                   EVAL      i = 1
088200030715     C                   DOW       i < %elem(SkSplitFLD)
088300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
088400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
088500030715     C                   EVAL      i = i + 1
088600030715     C                   ENDDO
088700030715     C                   EVAL      wGiro = 1
088800030715     C                   ELSE
088900030715     C                   EVAL      wGiro = 2
089000031204     C**********
089100031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
089200031204     C* d separatore campo
089300031204     C**********
089400031204     C                   EVAL      vindta = %trim(vindta)
089500031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
089600031204     C                   EVAL      vindta = %subst(vindta:2)
089700031204     C                   ENDDO
089800031204     C*
089900031204     C                   Z-ADD     *zeros        lunghInput        4 0
090000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
090100031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
090200031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
090300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
090400031204     C                   ENDDO
090500030715     C**********
090600030715     C* Effettuo lo split del campo dati d input
090700030715     C**********
090800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
090900030715     C                   DOW       posDa <= %len(%trim(vindta))
091000030715     C*
091100030715     C* Gestisco il 1° campo
091200030715     C                   IF        i = 1
091300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
091400030715     C                   EVAL      posDa = 1
091500030715     C* Eseguo lo scan x trovare la fine del primo campo
091600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091700030715     C* A questo punto "estraggo" il campo corrente
091800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091900030715     C                                                    (posA-posDa))
092000030715     C* X i campi successivi al 1°
092100030715     C                   ELSE
092200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
092300030715     C                   EVAL      posDa = posA + 1
092400031204     C* Verifico che nn vi sia il campo nn valorizzato
092500031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
092600031204     C* Se campo nn valorizzato skippo al prossimo
092700031204     C                   EVAL      SkSplitCSV(i) = *blanks
092800031204     C                   EVAL      posA  = posA + 1
092900031204     C                   ELSE
093000030715     C* Eseguo lo scan x trovare la fine del campo corrente
093100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
093200030715     C* Gestisco l'ultimo campo
093300030715     C                   IF        posA = *zeros
093400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
093500030715     C* A questo punto "estraggo" il campo corrente
093600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093700030715     C                                                    (posA-posDa))
093800030715     C                   LEAVE
093900030715     C                   ELSE
094000030715     C* A questo punto "estraggo" il campo corrente
094100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094200030715     C                                                    (posA-posDa))
094300030715     C                   ENDIF
094400031204     C                   ENDIF
094500030715     C                   ENDIF
094600030715     C* Incremento il contatore d campo
094700030715     C                   EVAL      i = i +1
094800030715     C                   ENDDO
094900030715     C*
095000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
095100030715     C* delimitazione testo
095200030715     C                   EVAL      i = 1
095300030715     C                   DOW       i < %elem(SkSplitCSV)
095400030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
095500030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
095600030715     C                   EVAL      i = i + 1
095700030715     C                   ENDDO
095800030715     C*
095900030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
096000030715     C                   EVAL      i = 1
096100030715     C                   DOW       i < %elem(SkSplitFLD)
096200030715     C*
096300030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
096400030715     C                   IF        SkSplitFLD(i) <> *blanks AND
096500030715     C                             SkSplitCSV(i) <> *blanks
096600030721     C*** ATB
096700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
096800030721     C                   EVAL      VABATB = SkSplitCSV(i)
096900030721     C                   ENDIF
097000030715     C*** RSD
097100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
097200030715     C                   EVAL      VABRSD = SkSplitCSV(i)
097300030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
097400030715     C     '@':'A'       XLATE     VABRSD        VABRSD
097500030715     C* ==
097600030715     C                   ENDIF
097700030721     C*** RD2
097800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
097900030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
098000030721     C                   ENDIF
098100030715     C*** IND
098200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
098300030715     C                   EVAL      VABIND = SkSplitCSV(i)
098400030715     C                   ENDIF
098500030715     C*** LOD
098600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
098700030715     C                   EVAL      VABLOD = SkSplitCSV(i)
098800030715     C                   ENDIF
098900030715     C*** PRD
099000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
099100030715     C                   EVAL      VABPRD = SkSplitCSV(i)
099200030715     C                   ENDIF
099300030721     C*** NZD
099400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
099500030721     C                   EVAL      VABNZD = SkSplitCSV(i)
099600030721     C                   ENDIF
099700030715     C*** RMA
099800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
099900030715     C                   EVAL      VABRMA = SkSplitCSV(i)
100000030715     C                   ENDIF
100100030715     C*** NOT
100200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
100300030715     C                   EVAL      VABNOT = SkSplitCSV(i)
100400030715     C                   ENDIF
100500030715     C*** NT2
100600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
100700030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
100800030715     C                   ENDIF
100900030715     C*** VCA
101000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
101100030715     C                   EVAL      VABVCA = SkSplitCSV(i)
101200030715     C                   ENDIF
101300030715     C*** VAS
101400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
101500030715     C                   EVAL      VABVAS = SkSplitCSV(i)
101600030715     C                   ENDIF
101700030715     C*** FFD
101800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
101900030715     C                   EVAL      VABFFD = SkSplitCSV(i)
102000030715     C                   ENDIF
102100030721     C*** GC1
102200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
102300030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
102400030721     C                   ENDIF
102500030721     C*** GC2
102600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
102700030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
102800030721     C                   ENDIF
102900030721     C*** TSP
103000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
103100030721     C                   EVAL      VABTSP = SkSplitCSV(i)
103200030721     C                   ENDIF
103300040510     C*** CBO
103400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
103500040510     C                   EVAL      VABCBO = SkSplitCSV(i)
103600040510     C                   ENDIF
103700030721     C*** NAS
103800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
103900030721     C                   EVAL      VABNAS = SkSplitCSV(i)
104000030721     C                   ENDIF
104100030721     C*** TIC
104200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
104300030721     C                   EVAL      VABTIC = SkSplitCSV(i)
104400030721     C                   ENDIF
104500030721     C*** GCA
104600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
104700030721     C                   EVAL      VABGCA = SkSplitCSV(i)
104800030721     C                   ENDIF
104900030721     C*** XCO
105000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
105100030721     C                   EVAL      VABXCO = SkSplitCSV(i)
105200030721     C                   ENDIF
105300030721     C*** CTM
105400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
105500030721     C                   EVAL      VABCTM = SkSplitCSV(i)
105600030721     C                   ENDIF
105700030721     C*** TCR
105800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
105900030721     C                   EVAL      VABTCR = SkSplitCSV(i)
106000030721     C                   ENDIF
106100030721     C*** CTS
106200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
106300030721     C                   EVAL      VABCTS = SkSplitCSV(i)
106400030721     C                   ENDIF
106500030721     C*** FTM
106600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
106700030721     C                   EVAL      VABFTM = SkSplitCSV(i)
106800030721     C                   ENDIF
106900030721     C*** VAD
107000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
107100030721     C                   EVAL      VABVAD = SkSplitCSV(i)
107200030721     C                   ENDIF
107300030721     C*** GMA
107400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
107500030721     C                   EVAL      VABGMA = SkSplitCSV(i)
107600030721     C                   ENDIF
107700030721     C*** GGA
107800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
107900030721     C                   EVAL      VABGGA = SkSplitCSV(i)
108000030721     C                   ENDIF
108100030721     C*** GVA
108200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
108300030721     C                   EVAL      VABGVA = SkSplitCSV(i)
108400030721     C                   ENDIF
108500030721     C*** TC1
108600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
108700030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
108800030721     C                   ENDIF
108900030721     C*** TC2
109000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
109100030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
109200030721     C                   ENDIF
109300030721     C*** SCL
109400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
109500030721     C                   EVAL      VABSCL = SkSplitCSV(i)
109600030721     C                   ENDIF
109700030721     C*** RMO
109800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
109900030721     C                   EVAL      VABRMO = SkSplitCSV(i)
110000030721     C                   ENDIF
110100030721     C*** NMO
110200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
110300030721     C                   EVAL      VABNMO = SkSplitCSV(i)
110400030721     C                   ENDIF
110500030721     C*
110600030721     C* Reperisco quindi i campi numerici...
110700040510     C*** NSP
110800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
110900040510     C                   EVAL      PiStr=SkSplitCSV(i)
111000040510     C                   EXSR      CHKNUM
111100040510     C                   IF        PiInt=*on
111200040510     C                   Z-ADD     PiVal         VABNSP
111300040510     C                   ELSE
111400040510     C                   SETON                                        31
111500040510     C                   EVAL      VABNSP = *zeros
111600040510     C                   EVAL      vinmsg = %trimr(vinmsg)
111700040510     C                             + ' ' + 'VABNSP'
111800040510     C                   ENDIF
111900040510     C                   ENDIF
112000071106     C*** RMN
112100071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
112200071106     C                   EVAL      PiStr=SkSplitCSV(i)
112300071106     C                   EXSR      CHKNUM
112400071106     C                   IF        PiInt=*on
112500071106     C                   Z-ADD     PiVal         VABRMN
112600071106     C                   ELSE
112700071106     C                   SETON                                        32
112800071106     C                   EVAL      VABRMN = *zeros
112900071106     C                   EVAL      vinmsg = %trimr(vinmsg)
113000071106     C                             + ' ' + 'VABRMN'
113100071106     C                   ENDIF
113200071106     C                   ENDIF
113300030721     C*** CAD
113400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
113500030721     C                   EVAL      PiStr=SkSplitCSV(i)
113600030721     C                   EXSR      CHKNUM
113700030721     C                   IF        PiInt=*on
113800030721     C                   Z-ADD     PiVal         Num5_0
113900030721     C                   MOVEL(p)  Num5_0        VABCAD
114000030721     C                   ELSE
114100030721     C                   SETON                                        32
114200030721     C                   EVAL      VABCAD = *zeros
114300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114400030721     C                             + ' ' + 'VABCAD'
114500030721     C                   ENDIF
114600030721     C                   ENDIF
114700030721     C*** NCL
114800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
114900030721     C                   EVAL      PiStr=SkSplitCSV(i)
115000030721     C                   EXSR      CHKNUM
115100030721     C                   IF        PiInt=*on
115200030721     C                   Z-ADD     PiVal         VABNCL
115300030721     C                   ELSE
115400030721     C                   SETON                                        32
115500030721     C                   EVAL      VABNCL = *zeros
115600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115700040122     C                             + ' ' + 'VABNCL'
115800030721     C                   ENDIF
115900030721     C                   ENDIF
116000030721     C*** PKB
116100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
116200030721     C                   EVAL      PiStr=SkSplitCSV(i)
116300030721     C                   EXSR      CHKNUM
116400030721     C                   IF        PiNum=*on
116500030721     C                   Z-ADD     PiVal         VABPKB
116600030721     C                   ELSE
116700030721     C                   SETON                                        32
116800030721     C                   EVAL      VABPKB = *zeros
116900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117000030721     C                             + ' ' + 'VABPKB'
117100030721     C                   ENDIF
117200030721     C                   ENDIF
117300030721     C*** CAS
117400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
117500080125     C                   IF        SkSplitCSV(i) <> *blanks
117600080125     C                   EVAL      wFlgCAS = '1'
117700080125     C                   ENDIF
117800030721     C                   EVAL      PiStr=SkSplitCSV(i)
117900030721     C                   EXSR      CHKNUM
118000030721     C                   IF        PiNum=*on
118100030721     C                   Z-ADD     PiVal         VABCAS
118200030721     C                   ELSE
118300030721     C                   SETON                                        32
118400030721     C                   EVAL      VABCAS = *zeros
118500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118600030721     C                             + ' ' + 'VABCAS'
118700030721     C                   ENDIF
118800030721     C                   ENDIF
118900030721     C*** CCM
119000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
119100030721     C                   EVAL      PiStr=SkSplitCSV(i)
119200030721     C                   EXSR      CHKNUM
119300030721     C                   IF        PiInt=*on      AND
119400030721     C                             PiVal<=9999999 AND
119500030721     C                             PiVal>*zeros
119600030721     C                   Z-ADD     PiVal         VABCCM
119700030721     C                   ELSE
119800030721     C                   SETON                                        32
119900030721     C                   EVAL      VABCCM = *zeros
120000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120100040510     C                             + ' ' + 'VABCCM'
120200030721     C                   ENDIF
120300030721     C                   ENDIF
120400030721     C*** LNP
120500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
120600030721     C                   EVAL      PiStr=SkSplitCSV(i)
120700030721     C                   EXSR      CHKNUM
120800030721     C                   IF        PiInt=*on    AND
120900030721     C                             PiVal<=999   AND
121000030721     C                             PiVal>*zeros
121100030721     C                   Z-ADD     PiVal         VABLNP
121200030721     C                   ELSE
121300040715     C                   SETON                                        32
121400030721     C                   EVAL      VABLNP = *zeros
121500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121600040510     C                             + ' ' + 'VABLNP'
121700030721     C                   ENDIF
121800030721     C                   ENDIF
121900030721     C*** NRS
122000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
122100030721     C                   EVAL      PiStr=SkSplitCSV(i)
122200030721     C                   EXSR      CHKNUM
122300030721     C                   IF        PiInt=*on AND
122400030721     C                             PiVal<=99
122500030721     C                   Z-ADD     PiVal         VABNRS
122600030721     C                   ELSE
122700030721     C                   SETON                                        32
122800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122900030721     C                             + ' ' + 'VABNRS'
123000030721     C                   ENDIF
123100030721     C                   ENDIF
123200030721     C*** LNA
123300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
123400030721     C                   EVAL      PiStr=SkSplitCSV(i)
123500030721     C                   EXSR      CHKNUM
123600030721     C                   IF        PiInt=*on    AND
123700030721     C                             PiVal<=999   AND
123800030721     C                             PiVal>*zeros
123900030721     C                   Z-ADD     PiVal         VABLNA
124000030721     C                   ELSE
124100030721     C                   SETON                                        32
124200030721     C                   EVAL      VABLNA = *zeros
124300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124400030721     C                             + ' ' + 'VABLNA'
124500030721     C                   ENDIF
124600030721     C                   ENDIF
124700030721     C*** CTR
124800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
124900030721     C                   EVAL      PiStr=SkSplitCSV(i)
125000030721     C                   EXSR      CHKNUM
125100030721     C                   IF        PiInt=*on  AND
125200030721     C                             PiVal<=999
125300030721     C                   Z-ADD     PiVal         VABCTR
125400030721     C                   ELSE
125500030721     C                   SETON                                        32
125600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125700030721     C                             + ' ' + 'VABCTR'
125800030721     C                   ENDIF
125900030721     C                   ENDIF
126000030721     C*** IAS
126100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
126200030721     C                   EVAL      PiStr=SkSplitCSV(i)
126300030721     C                   EXSR      CHKNUM
126400030721     C                   IF        PiNum=*on
126500030721     C                   Z-ADD     PiVal         VABIAS
126600030721     C                   ELSE
126700030721     C                   SETON                                        32
126800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126900030721     C                             + ' ' + 'VABIAS'
127000030721     C                   ENDIF
127100030721     C                   ENDIF
127200030721     C*** VLB
127300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
127400030721     C                   EVAL      PiStr=SkSplitCSV(i)
127500030721     C                   EXSR      CHKNUM
127600030721     C                   IF        PiNum=*on
127700030721     C                   Z-ADD     PiVal         VABVLB
127800030721     C                   ELSE
127900030721     C                   SETON                                        32
128000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128100030721     C                             + ' ' + 'VABVLB'
128200030721     C                   ENDIF
128300030721     C                   ENDIF
128400030721     C*** QFT
128500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
128600030721     C                   EVAL      PiStr=SkSplitCSV(i)
128700030721     C                   EXSR      CHKNUM
128800030721     C                   IF        PiNum=*on
128900030721     C                   Z-ADD     PiVal         VABQFT
129000030721     C                   ELSE
129100030721     C                   SETON                                        32
129200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129300030721     C                             + ' ' + 'VABQFT'
129400030721     C                   ENDIF
129500030721     C                   ENDIF
129600030721     C*** NCD
129700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
129800030721     C                   EVAL      PiStr=SkSplitCSV(i)
129900030721     C                   EXSR      CHKNUM
130000030721     C                   IF        PiInt=*on      AND
130100030721     C                             PiVal<=9999999
130200030721     C                   Z-ADD     PiVal         VABNCD
130300030721     C                   ELSE
130400030721     C                   SETON                                        32
130500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130600030721     C                             + ' ' + 'VABNCD'
130700030721     C                   ENDIF
130800030721     C                   ENDIF
130900030721     C*** NCA
131000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
131100030721     C                   EVAL      PiStr=SkSplitCSV(i)
131200030721     C                   EXSR      CHKNUM
131300030721     C                   IF        PiInt=*on      AND
131400030721     C                             PiVal<=9999999
131500030721     C                   Z-ADD     PiVal         VABNCA
131600030721     C                   ELSE
131700030721     C                   SETON                                        32
131800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131900030721     C                             + ' ' + 'VABNCA'
132000030721     C                   ENDIF
132100030721     C                   ENDIF
132200030721     C*** ZNC
132300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
132400030721     C                   EVAL      PiStr=SkSplitCSV(i)
132500030721     C                   EXSR      CHKNUM
132600030721     C                   IF        PiInt=*on AND
132700030721     C                             PiVal<=99
132800030721     C                   Z-ADD     PiVal         VABZNC
132900030721     C                   ELSE
133000030721     C                   SETON                                        32
133100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133200030721     C                             + ' ' + 'VABZNC'
133300030721     C                   ENDIF
133400030721     C                   ENDIF
133500030721     C*** DCR
133600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
133700030721     C                   EVAL      PiStr=SkSplitCSV(i)
133800030721     C                   EXSR      CHKNUM
133900030721     C                   IF        PiInt=*on       AND
134000030721     C                             PiVal<=99999999
134100030721     C                   Z-ADD     PiVal         VABDCR
134200030721     C                   ELSE
134300030721     C                   SETON                                        32
134400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134500030721     C                             + ' ' + 'VABDCR'
134600030721     C                   ENDIF
134700030721     C                   ENDIF
134800030721     C*** HCR
134900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
135000030721     C                   EVAL      PiStr=SkSplitCSV(i)
135100030721     C                   EXSR      CHKNUM
135200030721     C                   IF        PiInt=*on   AND
135300030721     C                             PiVal<=9999
135400030721     C                   Z-ADD     PiVal         VABHCR
135500030721     C                   ELSE
135600030721     C                   SETON                                        32
135700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135800030721     C                             + ' ' + 'VABHCR'
135900030721     C                   ENDIF
136000030721     C                   ENDIF
136100030721     C*** VMD
136200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
136300030721     C                   EVAL      PiStr=SkSplitCSV(i)
136400030721     C                   EXSR      CHKNUM
136500030721     C                   IF        PiNum=*on
136600030721     C                   Z-ADD     PiVal         VABVMD
136700030721     C                   ELSE
136800030721     C                   SETON                                        32
136900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137000030721     C                             + ' ' + 'VABVMD'
137100030721     C                   ENDIF
137200030721     C                   ENDIF
137300030721     C*** ANT
137400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
137500030721     C                   EVAL      PiStr=SkSplitCSV(i)
137600030721     C                   EXSR      CHKNUM
137700030721     C                   IF        PiInt=*on        AND
137800030721     C                             PiVal<=999999999
137900030721     C                   Z-ADD     PiVal         VABANT
138000030721     C                   ELSE
138100030721     C                   SETON                                        32
138200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138300030721     C                             + ' ' + 'VABANT'
138400030721     C                   ENDIF
138500030721     C                   ENDIF
138600030721     C*** CMO
138700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
138800030721     C                   EVAL      PiStr=SkSplitCSV(i)
138900030721     C                   EXSR      CHKNUM
139000030721     C                   IF        PiInt=*on
139100030721     C                   Z-ADD     PiVal         Num5_0
139200030721     C                   MOVEL(p)  Num5_0        VABCMO
139300030721     C                   ELSE
139400030721     C                   SETON                                        32
139500030721     C                   EVAL      VABCMO = *zeros
139600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139700030721     C                             + ' ' + 'VABCMO'
139800030721     C                   ENDIF
139900030721     C                   ENDIF
140000040510     C*
140100040510     C* Reperisco i campi del FIVAT
140200040510     C*** VATNOT
140300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
140400040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
140500040510     C                   ENDIF
140600040510     C*** VATTRC
140700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
140800040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
140900040510     C                   ENDIF
141000060331     C*** VATNOT_A
141100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
141200060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
141300060331     C                   ENDIF
141400060331     C*** VATNOT_B
141500060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
141600060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
141700071106     C                   EVAL      VABNOT    = %trim(SkSplitCSV(i))
141800060331     C                   ENDIF
141900060331     C*** VATNOT_E
142000070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
142100060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
142200060331     C                   ENDIF
142300070823     C*
142400071029     C***  ===>  Gestione campi "particolari" dentro ciclo
142500071204     C*
142600091021     C*** NSP/RMN/RMA
142700080215     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
142800091021     C                   EVAL      VABRMA=%subst(SkSplitCSV(i):2)
142900091021     C                   EVAL      PiStr=%subst(SkSplitCSV(i):2)
143000071204     C                   EXSR      CHKNUM
143100071204     C                   IF        PiInt=*on
143200071204     C                   Z-ADD     PiVal         VABNSP
143300071204     C                   Z-ADD     PiVal         VATNSP
143400071204     C                   Z-ADD     PiVal         VABRMN
143500071204     C                   ELSE
143600071204     C                   SETON                                        32
143700071204     C                   EVAL      VABNSP = *zeros
143800071204     C                   EVAL      VATNSP = *zeros
143900071204     C                   EVAL      VABRMN = *zeros
144000071204     C                   EVAL      vinmsg = %trimr(vinmsg)
144100091021     C                             + ' ' + 'VABNSP VATNSP VABRMN VABRMA'
144200071204     C                   ENDIF
144300071204     C                   ENDIF
144400071204     C*** PKB
144500080215     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABPKB'
144600071204     C                   EVAL      PiStr=SkSplitCSV(i)
144700071204     C                   EXSR      CHKNUM
144800071204     C                   IF        PiNum=*on
144900080215     C                   Z-ADD     PiVal         wPesoKG
145000071204     C                   ELSE
145100071204     C                   SETON                                        32
145200071204     C                   EVAL      VABPKB = *zeros
145300071204     C                   EVAL      vinmsg = %trimr(vinmsg)
145400080215     C                             + ' ' + 'wPesoKG'
145500071204     C                   ENDIF
145600071204     C                   ENDIF
145700080215     C*** W_ALT
145800080215     C                   IF        %trim(SkSplitFLD(i)) = 'W_ALT'
145900080215     C                   EVAL      PiStr=SkSplitCSV(i)
146000080215     C                   EXSR      CHKNUM
146100080215     C                   IF        PiNum=*on
146200080215     C                   Z-ADD(H)  PiVal         wAltezzCM
146300080215     C                   ELSE
146400080215     C                   SETON                                        32
146500080215     C                   EVAL      VABVLB = *zeros
146600080215     C                   EVAL      vinmsg = %trimr(vinmsg)
146700080215     C                             + ' ' + 'wAltezzCM'
146800080215     C                   ENDIF
146900080215     C                   ENDIF
147000080215     C*** W_LARG
147100080215     C                   IF        %trim(SkSplitFLD(i)) = 'W_LARG'
147200080215     C                   EVAL      PiStr=SkSplitCSV(i)
147300080215     C                   EXSR      CHKNUM
147400080215     C                   IF        PiNum=*on
147500080215     C                   Z-ADD(H)  PiVal         wLargheCM
147600080215     C                   ELSE
147700080215     C                   SETON                                        32
147800080215     C                   EVAL      VABVLB = *zeros
147900080215     C                   EVAL      vinmsg = %trimr(vinmsg)
148000080215     C                             + ' ' + 'wLargheCM'
148100080215     C                   ENDIF
148200080215     C                   ENDIF
148300080215     C*** W_PROF
148400080215     C                   IF        %trim(SkSplitFLD(i)) = 'W_PROF'
148500080215     C                   EVAL      PiStr=SkSplitCSV(i)
148600080215     C                   EXSR      CHKNUM
148700080215     C                   IF        PiNum=*on
148800080215     C                   Z-ADD(H)  PiVal         wProfonCM
148900080215     C                   ELSE
149000080215     C                   SETON                                        32
149100080215     C                   EVAL      VABVLB = *zeros
149200080215     C                   EVAL      vinmsg = %trimr(vinmsg)
149300080215     C                             + ' ' + 'wProfonCM'
149400080215     C                   ENDIF
149500080215     C                   ENDIF
149600080226     C*** W_NOT
149700080226     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT'
149800080226     C                   EVAL      VABNOT = 'FAX: ' + SkSplitCSV(i)
149900080226     C                   ENDIF
150000090122     C*** VATNOT_E3
150100090122     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOTE3'
150200090122     C                   EVAL      wVATNOT_E3 = %trim(SkSplitCSV(i))
150300090122     C                   ENDIF
150400090107     C*** VATNOT_E2
150500090107     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOTE2'
150600090122     C                   EVAL      wVATNOT_E2 = 'O'+%trim(SkSplitCSV(i))
150700090107     C                   ENDIF
150800090107     C*** VATNOT_E1
150900090107     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOTE1'
151000090122     C                   EVAL      wVATNOT_E1 = 'M'+%trim(SkSplitCSV(i))
151100090107     C                   ENDIF
151200070925     C*
151300071029     C***  <===  -----------------------------------------
151400040510     C*
151500030715     C                   ENDIF
151600030715     C                   EVAL      i = i + 1
151700030715     C                   ENDDO
151800070925     C*
151900071029     C***  ===>  Gestione campi "particolari" fuori ciclo
152000080220     C*
152100080220     C* Verifica codice nazione destinatario
152200080220     C                   IF        VABNZD = 'IT' OR
152300080220     C                             VABNZD = 'ITA'
152400080220     C                   EVAL      VABNZD = *blanks
152500080220     C                   ENDIF
152600080125     C*
152700080215     C* Porto il volume da cm3 a m3
152800080215     C                   EVAL      wVolumeCM3=wAltezzCM*wLargheCM*wProfonCM
152900080215     C                   EVAL      wVolumeM3 =wVolumeCM3/1000000
153000090107     C*
153100090107     C* Verifico il "chi sono" da considerare
153200090107     C                   IF        wVATNOT_E1 <> *blanks
153300090122     C                   EVAL      wVATNOT_E  = wVATNOT_E1
153400090107     C                   ELSE
153500090122     C                   IF        wVATNOT_E2 <> *blanks
153600090107     C                   EVAL      wVATNOT_E  = wVATNOT_E2
153700090122     C                   ELSE
153800090122     C                   EVAL      wVATNOT_E  = wVATNOT_E3
153900090122     C                   ENDIF
154000090107     C                   ENDIF
154100070925     C*
154200071029     C***  <===  -----------------------------------------
154300040510     C*
154400040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
154500040510     C                   IF        VABNSP = *zeros
154600040510     C                   SETON                                        31
154700040510     C                   ENDIF
154800040715     C*
154900040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
155000040715     C                   IF        VABLNP = *zeros
155100040715     C                   SETON                                        31
155200040715     C                   ENDIF
155300040510     C*
155400040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
155500040510     C                   IF        VABRMN = *zeros
155600040510     C                   Z-ADD     VABNSP        VABRMN
155700040510     C                   ENDIF
155800040510     C*
155900040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
156000040510     C                   IF        VABRMA = *blanks
156100040714     C                   MOVEL     VABRMN        VABRMA
156200040510     C                   ENDIF
156300040128     C*
156400040510     C* Se provincia nn valorizzata la reperisco
156500040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
156600040510     C                   IF        VABPRD = *blanks
156700040128     C                   CLEAR                   TISI95DS
156800040128     C                   EVAL      I95TCN = '3'
156900040128     C                   Z-ADD     datcor        I95DAT
157000040128     C                   EVAL      I95NAR = VABNZD
157100040128     C                   EVAL      I95CAP = VABCAD
157200040128     C                   EVAL      I95LOC = VABLOD
157300040128     C                   CALL      'TISI95R'
157400040128     C                   PARM                    TISI95DS
157500040128     C                   EVAL      VABPRD = O95PRV
157600040510     C                   ENDIF
157700040113     C*
157800040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
157900080215     C                   IF        VABCAS  > *zeros OR
158000080215     C                             wFlgCAS = '1'
158100040119     C                   EVAL      VABVCA = 'EUR'
158200040119     C                   IF        VABCBO = '1'
158300040113     C                   EVAL      VABCBO = '4'
158400040119     C                   ENDIF
158500040119     C                   IF        VABCBO = '2'
158600040119     C                   EVAL      VABCBO = '6'
158700040119     C                   ENDIF
158800040113     C                   ENDIF
158900020204     C*
159000020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
159100020204     C                   EXSR      CHKIMPDIV
159200030715     C                   ENDIF
159300020204     C*
159400010607     C                   ENDSR
159500010607     C*----------------------------------------------------*
159600010601
159700020204
159800020204     C*----------------------------------------------------*
159900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
160000020204     C*----------------------------------------------------*
160100020204     C     CHKIMPDIV     BEGSR
160200020204     C*
160300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
160400020204     C                   Z-ADD     *zeros        wrkDec            9 9
160500020204     C*
160600020204     C* Come prima cosa effettuo considerazioni sulla divisa
160700020204     C                   IF        vabIAS > *zeros
160800020204     C                   IF        vabVAS <> 'EUR'
160900020204     C                   EVAL      vabVAS =  'ITL'
161000020204     C                   ENDIF
161100020204     C                   ENDIF
161200020204     C*
161300020204     C                   IF        vabCAS > *zeros
161400020204     C                   IF        vabVCA <> 'EUR'
161500020204     C                   EVAL      vabVCA =  'ITL'
161600020204     C                   ENDIF
161700020204     C                   ENDIF
161800020204     C*
161900020204     C                   IF        vabVMD > *zeros
162000020204     C                   IF        vabVAD <> 'EUR'
162100020204     C                   EVAL      vabVAD =  'ITL'
162200020204     C                   ENDIF
162300020204     C                   ENDIF
162400020204     C*
162500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
162600020204     C                   Z-ADD     vabIAS        wrkDec
162700020204     C                   IF        wrkDec > *zeros
162800020204     C                   IF        vabVAS = 'ITL'
162900020204     C                   EVAL      vabIAS = *zeros
163000020204     C                   ENDIF
163100020204     C                   ENDIF
163200020204     C*
163300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
163400020204     C                   Z-ADD     vabCAS        wrkDec
163500020204     C                   IF        wrkDec > *zeros
163600020204     C                   IF        vabVCA = 'ITL'
163700020204     C                   EVAL      vabCAS = *zeros
163800020204     C                   ENDIF
163900020204     C                   ENDIF
164000020204     C*
164100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
164200020204     C                   Z-ADD     vabVMD        wrkDec
164300020204     C                   IF        wrkDec > *zeros
164400020204     C                   IF        vabVAD = 'ITL'
164500020204     C                   EVAL      vabVMD = *zeros
164600020204     C                   ENDIF
164700020204     C                   ENDIF
164800020204     C*
164900020204     C                   ENDSR
165000020204     C***
165100020204
165200010330
165300010330
165400010330     C*----------------------------------------------------*
165500010330     C*  CONTROLLO NUMERICITA' CAMPI
165600010330     C*----------------------------------------------------*
165700010330     C     CHKNUM        BEGSR
165800010330     C*
165900010606     C                   IF        PiDecChr = *blanks
166000030715     C                   EVAL      PiDecChr = CharNUM
166100010606     C                   ENDIF
166200010606     C*
166300010606     C                   CALL(e)   'ISNUMERIC'
166400010330     C                   PARM                    PiStr            30
166500010606     C                   PARM                    PiDecChr          1
166600010330     C                   PARM      *ZEROS        PiVal            30 9
166700010330     C                   PARM      '0'           PiInt             1
166800010330     C                   PARM      '0'           PiNum             1
166900010330     C                   IF        %error
167000010606     C                   EVAL      PiNum=*off
167100010330     C                   ENDIF
167200010330     C*
167300010330     C                   ENDSR
167400010330     C***
167500010330
167600010601
167700010601
167800010601
167900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
168000010601     C     repfil        BEGSR
168100010601     C*
168200010601     C                   if        invfil = *zeros and
168300010601     C                             depfil > *zeros and
168400010629     C                             (vinflg = *blanks or
168500010629     C                              vinflg = *zeros)
168600010601     C
168700010601     C                   eval      invfil = depfil
168800010601     C                   endif
168900010601     C*
169000010601     C                   if        depfil <> invfil and
169100010601     C                             invfil > *zeros
169200010601     C                   eval      flgMulti = '1'
169300010601     C                   if        vinflg = *blanks
169400010601     C                   add       1             cntNonEl
169500010601     C                   endif
169600010601     C                   endif
169700010601     C*
169800010601     C                   if        vinflg = '2'
169900010601     C                   eval      flgStato = '2'
170000010601     C                   endif
170100010601     C*
170200010601     C                   ENDSR
170300010601     C***
170400010601
170500010601
170600010601
170700010330
170800040119      /TITLE Invio dei dati al punto operativo.
170900040119     C     invio         BEGSR
171000040119     C*
171100040119     C* 1° invio FIVAT
171200040119     C                   reset                   dscmz
171300040119     C                   move      vlrpoi        cmzdst
171400040119     C                   eval      cmzfld = 'FIVATWWR'
171500040119     C                   eval      cmzmbd = vlrhdl
171600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
171700040119     C***                if        prmfir = *blanks
171800040119     C                   eval      cmzfla = 'FIVAT00F'
171900040119     C                   eval      cmzmba = 'FIVAT00F'
172000040119     C***                else
172100040119     C***                eval      cmzfla = prmfir
172200040119     C***                eval      cmzmba = prmfir
172300040119     C***                endif
172400040119     C                   eval      cmznrr = *zeros
172500040119     C                   move      §ctrokvt      cmznrr
172600040119     C                   eval      cmzlba = vlrfl1
172700040119     C                   call(e)   'TIS711C'
172800040119     C                   parm                    dscmz
172900040119     C                   parm      *blanks       esito
173000040119     C                   if        %error
173100040119     C                             or cmzerr = '1'
173200040119     C                             or esito  = '1'
173300040119     C                   eval      wrkesito = '3'
173400040119     C                   else
173500040119     C*
173600040119     C* 2° invio FIVAB
173700040119     C                   reset                   dscmz
173800040119     C                   move      vlrpoi        cmzdst
173900040119     C                   eval      cmzfld = vlrfou
174000040119     C                   eval      cmzmbd = vlrhdl
174100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
174200040119     C***                if        prmfir = *blanks
174300040119     C                   eval      cmzfla = 'FIVAB00F'
174400040119     C                   eval      cmzmba = 'FIVAB00F'
174500040119     C***                else
174600040119     C***                eval      cmzfla = prmfir
174700040119     C***                eval      cmzmba = prmfir
174800040119     C***                endif
174900040119     C                   eval      cmznrr = *zeros
175000040119     C                   move      §ctrokvb      cmznrr
175100040119     C                   eval      cmzlba = vlrfl1
175200040119     C                   call(e)   'TIS711C'
175300040119     C                   parm                    dscmz
175400040119     C                   parm      *blanks       esito
175500040119     C                   if        %error
175600040119     C                             or cmzerr = '1'
175700040119     C                             or esito  = '1'
175800040119     C                   eval      wrkesito = '3'
175900040119     C                   endif
176000040119     C                   endif
176100040119     C*
176200040119     C                   ENDSR
176300040119     C***
176400010601
176500010601
176600010601
176700010601
176800010601      /TITLE Invio dei dati al punto operativo.
176900010601     C     opeini        BEGSR
177000010601     C*
177100010601     C* Inizializzo flag e contatori operativi
177200010601     C                   movel     '0'           flgGiro           1
177300010601     C                   movel     '0'           flgMulti          1
177400010601     C                   movel     '1'           flgStato          1
177500010615     C                   movel     '0'           flgOk             1
177600010601     C                   z-add     *zeros        cntNonEl         10 0
177700010601     C                   z-add     *zeros        depfil            3 0
177800010601     C                   z-add     *zeros        invfil            3 0
177900010601     C*
178000010601     C                   ENDSR
178100010601     C***
178200070326
178300070326
178400070326
178500070326
178600080219     C     *pssr         BEGSR
178700070329     C*
178800080219     C                   if        %open(tivin00r)
178900080219     C                   close     tivin00r
179000080219     C                   endif
179100080219     C                   if        %open(fivabwwr)
179200080219     C                   close     fivabwwr
179300080219     C                   endif
179400080219     C                   if        %open(fivatwwr)
179500080219     C                   close     fivatwwr
179600080219     C                   endif
179700070326     C*
179800070326     C* Effettuo la chiamata al CLLE preposto
179900080219     C                   call(e)   'TITVVTC'
180000080219     C                   parm                    parccm
180100080219     C                   parm                    parmbr
180200080219     C                   parm      '2'           paropz
180300080219     C*
180400080219     C                   eval      wrkesito = '2'
180500080219     C*
180600080219     C                   seton                                        LR
180700070326     C*
180800080219     C                   ENDSR     '*CANCL'
180900070326     C***
181000070326
181100070326
181200010330
181300010330
181400000613     C     *inzsr        BEGSR
181500990910     C*
181600990910     C     *entry        plist
181700990920     C                   parm                    tivlrds
181800990921     C                   parm      wrkesito      esito
181900000724     C                   parm                    prmlit
182000000710     C                   parm                    prmfir
182100010330     C*
182200010330     C* CALCOLA LA DATA CORRENTE
182300010330     C                   time                    wn14             14 0
182400010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
182500010330     C                   z-add     wn8           g08dat
182600010330     C                   z-add     *zeros        g08inv
182700010330     C                   movel     '0'           g08err
182800010330     C                   call      'XSRDA8'
182900010330     C                   parm                    wlbda8
183000010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
183100000613     C*
183200000613     C                   ENDSR
183300000613     C***
183400990908
183500080215     Otitv1d9p  E            riepilogo         2
183600990915     O                                              'Upload via Internet'
183700990915     O                                           +1 'Traduzione TIVIN00R -'
183800021025     O                                           +1 'FIVABWWR'
183900990915     O                                           +1 'Testate bolle'
184000990915     O          E            riepilogo   2
184100990915     O                       wrkdata
184200990915     O                       wrkora              +1
184300990915     O                       procname            +1
184400990915     O          E            riepilogo   2
184500990915     O                                              'Cliente..................:'
184600010605     O                       VABCCM        z     +1
184700990915     O          E            riepilogo   2
184800990920     O                                              'Riferimento Strategi.....:'
184900990920     O                       vlrhdl              +1
185000990915     O          E            riepilogo   2
185100990915     O                                              'Giusti...................:'
185200040119     O                       §CTROKVB      2   +  1
185300990915     O          E            riepilogo   2
185400990915     O                                              'Sbagliati e corretti.....:'
185500971022     O                       §CTRMO        2   +  1
185600990915     O          E            riepilogo   2
185700990915     O                                              'Sbagliati e scartati.....:'
185800971022     O                       §CTRNO        2   +  1
185900000613
186000080215     Otitv1d9ps E            testdett          2
186100000613     O                                              'Upload via Internet'
186200000613     O                                           +1 'Traduzione TIVIN00R -'
186300021025     O                                           +1 'FIVABWWR'
186400000613     O                                           +1 'Testate bolle'
186500000616     O          E            testdett    3
186600000613     O                                           +2 'N° rec'
186700000613     O                                           +3 'Anteprima contenuto'
186800000616     O          E            rigadett    2
186900000613     O                       rrnum               +2
187000000621     O                       recko               +3
187100000616     O          E            findett     2
187200000613     O                       wrkdata
187300000613     O                       wrkora              +1
187400000613     O                       procname            +1
187500000616     O          E            findett     2
187600000613     O                                              'Cliente..................:'
187700010605     O                       VABCCM        z     +1
187800000616     O          E            findett     2
187900000613     O                                              'Riferimento Strategi.....:'
188000000613     O                       vlrhdl              +1
188100000616     O          E            findett     2
188200000613     O                                              'Giusti...................:'
188300040119     O                       §CTROKVB      2   +  1
188400000616     O          E            findett     2
188500000613     O                                              'Sbagliati e corretti.....:'
188600000613     O                       §CTRMO        2   +  1
188700000616     O          E            findett     2
188800000613     O                                              'Sbagliati e scartati.....:'
188900000613     O                       §CTRNO        2   +  1
189000000613** CMD - COMANDI CL
189100080215OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1D9P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
189200080215OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITV1D9PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
189300080215DLTOVR FILE(TITV1D9P TITV1D9PS) LVL(*)
189400010606
189500000613
189600040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
189700091021FIL01;W_VABRMN;FIL02;FIL03;VABNCL;FIL04;FIL05;FIL06;FIL07;FIL08;
189800080215W_VABPKB;FIL08;W_ALT;W_LARG;W_PROF;FIL09;FIL10;FIL11;FIL12;
189900080215VABRSD;VABIND;VABLOD;VABCAD;VABPRD;VABNZD;FIL13;
190000080306FIL14;FIL15;FIL16;FIL17;FIL18;FIL19;FIL01;FIL20;FIL21;VATNOT_B;
190100090122W_VABNOT;VABGC1;W_VATNOTE3;W_VATNOTE2;W_VATNOTE1
